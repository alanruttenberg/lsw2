(in-package :cl-user)

;; Graphdb 9, as compared to graphdb that I used originally, has a different api.
;; In the original graphdb there was a SYSTEM repository and the api was via sparql queries
;; In Graphdb 9 it's a rest API that's used.

(defclass graphdb9-instance (graphdb-instance)
  ((server-file-directory :accessor server-file-directory :initarg :server-file-directory))
  (:documentation "GraphDB version 9 (http://graphdb.ontotext.com)"))

(defmethod initialize-instance ((g graphdb9-instance) &key)
  (call-next-method)
  (let ((ql::*quickload-verbose* nil))
    ;; Jonathan is a JSON encoder/decoder
    ;; See https://github.com/Rudolph-Miller/jonathan
    (asdf::load-system "jonathan")))

;; these functions so that we can be lazy about loading Jonathan, which brought in a pile of
;; other code.
(defun json-parse (string)
  (funcall (intern "PARSE" 'jonathan.decode) string))

(defun json-encode (&rest args)
  (apply (intern "TO-JSON" 'jonathan.encode) args))

(defmethod repository-class  ((g graphdb9-instance))
  'graphdb9-repository)

;; endpoint is a misnomer. This is a repository. It has a query-endpoint and an update-endpoint, which are endpoints
(defclass graphdb9-repository (graphdb-sparql-endpoint)
  ()
  (:documentation "A repository within a graphdb instance"))

(defmethod repository-named ((instance graphdb9-instance) name &optional (errorp t))
  (endpoint-named instance name errorp))

(defmethod get-repository-parameters ((repo graphdb9-repository))
  (let ((res (json-api-call-get repo "/rest/repositories/")))
    (assert (eq (car res) :|params|) () "Unexpected format for repository parameters")
    (append 
     (loop for (field value) on (second res) by #'cddr
               for justvalue = (getf value :|value|)
               collect (list (de-camelcase field) justvalue))
     (loop for (field value) on (cddr res) by #'cddr
               collect (list (de-camelcase field) value)))))
(defmethod json-api-call-post ((repo graphdb9-repository) path json &key (result-type :json))
  (let ((result 
          (get-url (format nil "~a~a~a" (repository-root (instance repo)) path (repo-id repo))
                   :verb "POST" :post json)))
    (ecase result-type
      (:json (json-parse result))
      (:text result))))

(defmethod json-api-call-get ((repo graphdb9-repository) path &optional (result-type :json))
  (let ((result (get-url (format nil "~a~a~a" (repository-root (instance repo)) path (repo-id repo)) :force-refetch t :persist nil :dont-cache t)))
    (ecase result-type
      (:json (json-parse result))
      (:text result))))

(defmethod load-server-file ((repo graphdb9-repository) file)
  (let ((answer 
          (json-api-call-post repo "/rest/data/import/server/" (json-encode `(("fileNames" ,file)) :from :alist) :result-type :text)))
    (assert (#"matches" answer "File .* sent for import.") () "Load of ~a failed with '~a'" file answer)
    t))

(defmethod clear-repository ((repo graphdb9-repository))
  (json-parse (get-url (format nil "~a/repositories/~a/statements" (repository-root (instance repo)) (repo-id repo))
                       :verb "DELETE" :force-refetch t :persist nil :dont-cache t)))

(defmethod sparql-query ((repo graphdb9-repository) query &rest keys &key &allow-other-keys )
  (declare (ignorable query-options geturl-options command format trace))
  (apply 'sparql query :endpoint (query-endpoint repo) keys))

(defmethod sparql-update ((repo graphdb9-repository) query &rest keys &key query-options geturl-options (command :select) format (trace nil) &allow-other-keys)
  (declare (ignorable query-options geturl-options command format trace))
  (apply 'sparql-endpoint-query (update-endpoint repo) query  keys))

(defmethod total-triples ((repo graphdb9-repository))
  (parse-integer (caar (sparql-query repo '(:select ((:count (*) as ?c)) () (?s ?p ?o))))))

(defmethod get-repository-prefixes ((repo graphdb9-repository))
  (all-matches (get-url  (uri-full (namespaces-endpoint repo)) :force-refetch t :persist nil :dont-cache t) "(?m)(\\S*),(\\S+)" 1 2))

;; set-repository-prefixes inherited from graphdb (original class, on which this class is based)

(defmethod delete-repository-prefixes ((repo graphdb9-repository) &optional prefixes)
  (loop for prefix in prefixes
        do (cl-user::get-url (format nil "~a/~a" (uri-full (cl-user::namespaces-endpoint repo)) prefix)
                             :verb "DELETE" :force-refetch t :ignore-errors t)))

;; ****************************************************************
(eval-when (:execute :load-toplevel)
  (register-namespace "ontotext-geosparql:" "http://www.ontotext.com/plugins/geosparql#")
  (register-namespace "geosparql:" "http://www.opengis.net/ont/geosparql#" t)
  (register-namespace "simple-feature:" "http://www.opengis.net/ont/sf#")
  (register-namespace "sys:" "http://www.ontotext.com/owlim/system#"))

(defmethod geosparql-configuration ((repo graphdb9-repository))
  (sparql-query repo '(:select (*) (:distinct t)
                       (:_b !ontotext-geosparql:currentPrefixTree ?tree)
                       (:_b !ontotext-geosparql:currentPrecision ?precision))))

(defmethod enable-geosparql ((repo graphdb9-repository) &key (enable t))
  (sparql-update repo `(:update (:insert (:_b !ontotext-geosparql:enabled ,(if enable "true" "false"))))))

(defmethod disable-geosparql ((repo graphdb9-repository))
  (enable-geosparql repo nil))


(defmethod reindex-geosparql ((repo graphdb9-repository))
  (sparql-update repo '(:update (:insert (:_b1 !ontotext-geosparql:forceReindex :_b2)))))


(defmethod update-geosparql-configuation ((repo graphdb9-repository) &key (ignore-indexing-errors nil ignore-supplied-p)
                                                                       (prefix-tree '(:quad 25) prefix-tree-supplied-p))
  (if ignore-supplied-p
      (sparql-update repo `(:update (:insert (:_b1 !ontotext-geosparql:ignoreErrors ,(if ignore-indexing-errors "true" "false"))))))
  (if prefix-tree-supplied-p
      (sparql-update repo `(:update (:insert
                                     (:_b1 !ontotext-geosparql:prefixTree ,(format nil "~a" (car prefix-tree)))
                                     (:_b1 !ontotext-geosparql:precision ,(format nil "~a" (second prefix-tree))))) :trace t)))


(defmethod re-infer ((repo graphdb9-repository))
  (sparql-update repo '(:update (:insert ( :_b !<http://www.ontotext.com/owlim/system#reinfer> :_b )))))

;; ****************************************************************
;; Rule sets

(defmethod get-rulesets ((repo graphdb9-repository))
  (sparql-query repo '(:select (?state ?ruleset) () (?state !sys:listRulesets ?ruleset))))

;; doesn't work in my version of free.  (explore-rulesets *first* "owl2-rl-optimized") -> nil. Maybe for custom ones
(defmethod explore-ruleset ((repo graphdb9-repository) ruleset)
  (sparql-query repo `(:select (?content) () (?content !sys:exploreRuleset ,ruleset))))

(defmethod add-ruleset ((repo graphdb9-repository) ruleset )
  (sparql-update repo `(:update (:insert (:_b !sys:addRuleset ,ruleset)))))

(defmethod change-ruleset ((repo graphdb9-repository) ruleset)
  (sparql-update repo `(:update (:insert (:_b !sys:defaultRuleset ,ruleset)))))

(defmethod remove-ruleset ((repo graphdb9-repository) ruleset)
  (sparql-update repo `(:update (:insert (:_b !sys:removeRuleset ,ruleset)))))
  
(defvar *graphdb9-ruleset-directory* "/Applications/GraphDB Free.app/Contents/Java/configs/rules/")

(defvar *graphdb9-builtin-rulesets*
  '((:empty "No reasoning, i.e., GraphDB operates as a plain RDF store."
     )
    (:rdfs "Supports the standard model-theoretic RDFS semantics. This includes support for subClassOf and related type inference, as well as subPropertyOf."
     "builtin_RdfsRules.pie" "builtin_RdfsRules-optimized.pie" )
    (:rdfs-plus "Extended version of RDFS with the support also symmetric, inverse and transitive properties, via the OWL vocabulary: owl:SymmetricProperty, owl:inverseOf and owl:TransitiveProperty."
     "builtin_rdfsPlus.pie" "builtin_rdfsPlus-optimized.pie")
    (:owl-horst "OWL dialect close to OWL-Horst - essentially pD*"
     "builtin_Rules-horst-optimized.pie")
    (:owl-max "RDFS and that part of OWL Lite that can be captured in rules (deriving functional and inverse functional properties, all-different, subclass by union/enumeration; min/max cardinality constraints, etc.)."
     "builtin_Rules.pie" "builtin_Rules-optimized.pie"
     )
    (:owl2-ql "The OWL2-QL profile - a fragment of OWL2 Full designed so that sound and complete query answering is LOGSPACE with respect to the size of the data. This OWL2 profile is based on DL-LiteR, a variant of DL-Lite that does not require the unique name assumption."
     "builtin_owl2-ql.pie" "builtin_owl2-ql-optimized.pie" )
    (:owl2-rl "The OWL2-RL profile - an expressive fragment of OWL2 Full that is amenable for implementation on rule engines.")
    "builtin_owl2-rl.pie" "builtin_owl2-rl-optimized.pie")
  )

#|
(setq *default-graphdb* (make-instance 'graphdb9-instance
                                      :root "http://127.0.0.1:7200/"
                                      :name :local-graphdb
                                      :server-file-directory *test-import-directory*))
(setq repo (repository-named *default-graphdb* "first"))
(get-repository-parameters repo)
(clear-repository repo)
(update-geosparql-configuation e :ignore-indexing-errors repo)
(update-geosparql-configuation e :prefix-tree '("quad" 11))
|#
