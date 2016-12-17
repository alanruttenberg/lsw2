(in-package :cl-user)

(defclass sparql-repository-set ()
  ((root :initarg :root :accessor repository-root)
   (repositories :accessor repositories :initarg :repositories :initform (make-hash-table :test 'equal))))

(defclass openrdf-sesame-instance (sparql-repository-set)
  ((system-endpoint :accessor system-endpoint :initarg system-endpoint))
  )

(defmethod initialize-instance ((instance openrdf-sesame-instance) &rest initargs)
  (call-next-method))

(defmethod system-endpoint :around ((instance openrdf-sesame-instance))
  (or (and (slot-boundp instance 'system-endpoint) (call-next-method))
      (progn (get-repositories instance)
	     (setf (system-endpoint instance) (gethash "SYSTEM" (repositories instance))))))

(defmethod endpoint-named ((instance openrdf-sesame-instance) name)
  (gethash name (repositories instance)))
  
(defclass graphdb-instance (openrdf-sesame-instance)
  )

(defclass sparql-endpoint ()
  ())

(defclass sesame-sparql-endpoint ()
  ((query-endpoint :accessor query-endpoint :initarg :query-endpoint)
   (update-endpoint :accessor update-endpoint :initarg :update-endpoint)
   (repo-id :accessor repo-id :initarg :repo-id)
   (readable :accessor readable :initarg :readable)
   (writeable :accessor writeable :initarg :writeable)
   (title :accessor title :initarg :title)
   (instance :accessor instance :initarg :instance)))

(defmethod print-object ((e sesame-sparql-endpoint) stream)
  (print-unreadable-object (e stream :type t)
    (unless (not (slot-boundp e 'repo-id))
      (format stream "~a at ~a" (repo-id e) (query-endpoint e)))))

(defmethod namespaces-endpoint ((endpoint sesame-sparql-endpoint))
  (make-uri (concatenate 'string (uri-full (query-endpoint endpoint)) "/namespaces")))

(defclass graphdb-sparql-endpoint (sesame-sparql-endpoint)
  )

(defmethod endpoint-class  ((g openrdf-sesame-instance))
  'sesame-sparql-endpoint)

(defmethod endpoint-class  ((g graphdb-instance))
  'graphdb-sparql-endpoint)
  
(setq *graphdb* (make-instance 'graphdb-instance :root "http://127.0.0.1:8080/graphdb-workbench-ee/"))

; (register-namespace "graphdb:" (format nil "~arepositories/" *graphdb-root*) t)
(register-namespace "sesamerep:" "http://www.openrdf.org/config/repository#")
(register-namespace "owlim:" "http://www.ontotext.com/trree/owlim#")
(register-namespace "sailrepository:" "http://www.openrdf.org/config/repository/sail#")
(register-namespace "sesamesail:" "http://www.openrdf.org/config/sail#")

(defmethod get-repositories ((instance openrdf-sesame-instance))
  (with-input-from-string (s (get-url (format nil "~arepositories" (repository-root instance)) :accept  "text/csv" :force-refetch t))
    (read-line s)
    (loop with endpoint-class = (endpoint-class instance)
	  for line =  (read-line s nil :eof)
	  until (eq line :eof)
	  for (uri id title readable writeable) = (split-at-char (#"replaceAll" line "\\r" "") #\,)
	  for repo = (or (gethash id (repositories instance))
			 (setf (gethash id  (repositories instance))
			       (make-instance endpoint-class
					      :instance instance
					      :repo-id id
					      :title title
					      :readable (equal readable "true")
					      :writeable (equal writeable "true")
					      :query-endpoint (make-uri uri)
					      :update-endpoint (and (equal writeable "true")
								    (make-uri (concatenate 'string  uri "/statements"))))))
	  collect repo)))
  

(defmethod get-timeout ((endpoint graphdb-sparql-endpoint))
  "get the current query timeout, in seconds"
  (let ((endpoint-id (repo-id  endpoint))
	(system-repo (system-endpoint (instance endpoint))))
    (let ((timeout
	    (caar
	     (sparql `(:select (?timeout) () 
			(?ctx a !sesamerep:RepositoryContext)
			(:graph ?ctx
				(:_a !sesamerep:repositoryID ,endpoint-id)
				(:_a  !rdf:type !sesamerep:Repository)
				(?place !owlim:query-timeout ?timeout)))
		     :endpoint (query-endpoint system-repo)))))
     (when timeout     
       (parse-integer timeout)))))

(defmethod get-endpoint-parameters ((repo graphdb-sparql-endpoint))
  (let ((endpoint-id (repo-id repo))
	(system-repo (system-endpoint (instance repo))))
    (sparql `(:select (?graph ?graphtype ?type ?rep ?delegate ?param ?value) ()
	       (?graph !rdf:type ?graphtype)
	       (:graph ?graph
		       (?rep !sesamerep:repositoryID ,endpoint-id)
		       (?rep !sesamerep:repositoryImpl ?delegate)
		       (?delegate !sesamerep:delegate ?impl)
		       (?impl !sesamerep:repositoryType ?type)
		       (:optional
			(?impl !sailrepository:sailImpl ?sail)
			(?sail ?param ?value)))
	    )
	       :endpoint (query-endpoint system-repo))))

(defmethod get-endpoint-parameters ((repo graphdb-sparql-endpoint))
  (let ((endpoint-id (repo-id repo))
	(system-repo (system-endpoint (instance repo))))
    (sparql `(:select (?graph ?graphtype ?s ?p ?o) ()
	       (?graph !rdf:type ?graphtype)
	       (:graph ?graph (?s ?p ?o)))
	    ;; 	       (?rep !sesamerep:repositoryID ,endpoint-id)
	    ;; 	       (?rep !sesamerep:repositoryImpl ?delegate)
	    ;; 	       (?delegate !sesamerep:delegate ?impl)
	    ;; 	       (?impl !sesamerep:repositoryType ?type)
	    ;; 	       (:optional
	    ;; 		(?impl !sailrepository:sailImpl ?sail)
	    ;; 		(?sail ?param ?value)))
	    ;; )
	    :endpoint (query-endpoint system-repo))))

;; graphdb_ee_change_parameter <- function(repo,parameter,value)
;;   { sparqlupdate(
;;     "PREFIX sys:  <http://www.openrdf.org/config/repository#>",
;;     "PREFIX sail: <http://www.openrdf.org/config/repository/sail#>",
;;     "PREFIX onto: <http://www.ontotext.com/trree/owlim#>",
;;     "DELETE { GRAPH ?g {?sail ?param ?old_value } }",
;;     "INSERT { GRAPH ?g {?sail ?param ?new_value } }",
;;     "WHERE {",
;;     "  GRAPH ?g { ?rep sys:repositoryID ?id . }",
;;     "  GRAPH ?g { ?rep sys:repositoryImpl ?delegate . }",
;;     "  GRAPH ?g { ?delegate sys:repositoryType ?type . }",
;;     "  GRAPH ?g { ?delegate sys:delegate ?impl . }",
;;     "  GRAPH ?g { ?impl sail:sailImpl ?sail . }",
;;     "  GRAPH ?g { ?sail ?param ?old_value . }",
;;     "  FILTER( ?id = \"",repo,"\" ) .",
;;     "  FILTER( ?param = ",param," ) .",
;;     "  BIND( ",value," AS ?new_value ) .",
;;     "}",
;;     endpoint=gsub("(.*/)(.*)","\\1SYSTEM/statements",current_endpoint,perl=TRUE)
;;     )
;;   }

(defmethod get-repository-prefixes ((repo sesame-sparql-endpoint))
  (extract-sparql-results (get-url (uri-full (namespaces-endpoint repo)) :force-refetch t)))
  
(defmethod set-repository-prefixes ((repo sesame-sparql-endpoint) prefixes)
  (loop for (prefix replacement) in prefixes
	with namespace-uri = (uri-full (namespaces-endpoint repo))
	do
	   (get-url (format nil "~a/~a" namespace-uri (#"replaceFirst" prefix ":$" ""))
		    :post replacement :verb "PUT")))

;(create-repository "LSW")

(defmethod create-repository ((instance graphdb-instance) name
			      &key
				(label (format nil "A triple store named: ~a" name))
				(context-index "false")
				(predicate-memory 80)
				(full-text-memory 0)
				(full-text "never")
				(transaction-mode "fast")
				(transaction-isolation "true")
				(tuple-index-memory 7686)
				(enable-predicate-list "true")
				(in-memory-literal-properties "false")
				(full-text-literals-only "true")
				(cache-memory (+ tuple-index-memory predicate-memory full-text-memory 1))
				(entity-index-size 500000)
				(system (update-endpoint (system-endpoint instance)))
			      &allow-other-keys)
  (flet ((m (n) (format nil "~am" n)))
    (let ((context (make-uri (format nil "urn:lsw:graph:~a" name))))
      (sparql-endpoint-query system `(:update
			(:insert
			 (:graph ,context
				 (:_repo !rdf:type !sesamerep:Repository)
				 (:_repo !sesamerep:repositoryID ,name)
				 (:_repo !rdfs:label ,label)
				 (:_repo !sesamerep:repositoryImpl :_impl)
				 (:_impl !sesamerep:repositoryType "owlim:MonitorRepository")
				 (:_impl !sailrepository:sailImpl :_sail)
				 (:_sail !sesamesail:sailType "owlimClusterWorker:Sail" )

				 (:_sail !owlim:base-URL "urn:shouldnt:be:here:" )
				 (:_sail !owlim:defaultNS "")
				 (:_sail !owlim:entity-index-size ,entity-index-size)
				 (:_sail !owlim:entity-id-size "32")
				 (:_sail !owlim:imports "")
				 (:_sail !owlim:repository-type "file-repository")
				 (:_sail !owlim:ruleset "owl-horst-optimized" )
				 (:_sail !owlim:storage-folder "storage")
				 (:_sail !owlim:cache-memory ,cache-memory)

				 (:_sail !owlim:query-timeout "0")
				 (:_sail !owlim:query-limit-results "0")

				 (:_sail !owlim:enablePredicateList ,enable-predicate-list)
				 (:_sail !owlim:read-only "false")
				 (:_sail !owlim:in-memory-literal-properties ,in-memory-literal-properties)
				 (:_sail !owlim:enable-literal-index "true")

				 (:_sail !owlim:check-for-inconsistencies "false")
					;(:_sail !owlim:disable-sameAs "false")

					;(:_sail !owlim:enable-context-index ,context-index)

					;(:_sail !owlim:throw-QueryEvaluationException-on-timeout "false")

					;(:_sail !owlim:nonInterpretablePredicates "http://www.w3.org/2000/01/rdf-schema#label;http://www.w3.org/1999/02/22-rdf-syntax-ns#type;http://www.ontotext.com/owlim/ces#gazetteerConfig;http://www.ontotext.com/owlim/ces#metadataConfig")

				 (:_sail !owlim:tuple-index-memory ,(m tuple-index-memory))
				 (:_sail !owlim:predicate-memory ,(m predicate-memory))
				 (:_sail !owlim:fts-memory ,(m full-text-memory))
				 (:_sail !owlim:ftsIndexPolicy ,full-text)
				 (:_sail !owlim:ftsLiteralsOnly ,full-text-literals-only)
				 (:_sail !owlim:transaction-mode ,transaction-mode)
				 (:_sail !owlim:transaction-isolation ,transaction-isolation)))
			(:insert 
			 (,context !rdf:type !sesamerep:RepositoryContext)))
	       :command :update :trace t))))
#|
:_sail owlim:entity-index-size "500000" ;
:_sail owlim:cache-memory "7686m" ;
:_sail owlim:tuple-index-memory "7686m" ;
:_sail owlim:enable-context-index "false" ;
:_sail owlim:enablePredicateList "false" ;
:_sail owlim:predicate-memory "0m" ;
:_sail owlim:fts-memory "0" ;
:_sail owlim:ftsIndexPolicy "never" ;
:_sail owlim:ftsLiteralsOnly "true" ;
:_sail owlim:in-memory-literal-properties "false" ;
:_sail owlim:transaction-mode "fast" ;
:_sail owlim:transaction-isolation "false" ;


## How can I script (automate) the creation of remote repositories?
## This can be achieved on the command line using a repository configuration file (usually in Turtle format) and curl. The following steps must be followed:

## Create a repository configuration file containing the repository ID and its configuration parameters, e.g. filename config.ttl containing:
## @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>.
## @prefix rep: <http://www.openrdf.org/config/repository#>.
## @prefix sr: <http://www.openrdf.org/config/repository/sail#>.
## @prefix sail: <http://www.openrdf.org/config/sail#>.
## @prefix owlim: <http://www.ontotext.com/trree/owlim#>.

## [] a rep:Repository ;
##    rep:repositoryID "my_repo_id" ;
##    rdfs:label "Description of my repository" ;
##    rep:repositoryImpl [
##      rep:repositoryType "openrdf:SailRepository" ;
##      sr:sailImpl [
##        owlim:ruleset "owl-horst-optimized" ;
##        sail:sailType "owlim:Sail" ;
##        owlim:base-URL "http://example.org/owlim#" ;
##        owlim:repository-type "file-repository" ;
##       ]
##    ].
## Update the Sesame SYSTEM repository with this configuration by issuing the following on the command line (all in one line) replacing the filename (config.ttl), URL to the remote server's SYSTEM directory (http://localhost:8080/openrdf-sesame/repositories/SYSTEM) and a unique context in which to put the repository configuration (http://example.com#g1):
## curl -X POST -H "Content-Type:application/x-turtle"
##     -T config.ttl
##     http://localhost:8080/openrdf-sesame/repositories/SYSTEM/rdf-graphs/service?graph=http://example.com#g1
## Finally, update the SYSTEM repository with a single statement to indicate that the unique context is an instance of sys:RepositoryContext:
## curl -X POST -H "Content-Type:application/x-turtle"
##     -d "<http://example.com#g1> a <http://www.openrdf.org/config/repository#RepositoryContext>."
##     http://localhost:8080/openrdf-sesame/repositories/SYSTEM/statements
|#
#|
  
[] a rep:Repository ;
   rep:repositoryID "PROTEST" ;
   rdfs:label "Pro Test" ;
   rep:repositoryImpl [
      rep:repositoryType "openrdf:SailRepository" ;
      sr:sailImpl [
        sail:sailType "owlim:Sail" ;
        owlim:ruleset "empty" ;
        owlim:repository-type "file-repository" ;
        owlim:base-URL "http://purl.obolibrary.org/obo/" ;
        owlim:imports "" ;
        owlim:defaultNS "" ;

        owlim:entity-index-size "500000" ;
        owlim:cache-memory "7686m" ;
        owlim:tuple-index-memory "7686m" ;
        owlim:enable-context-index "false" ;
        owlim:enablePredicateList "false" ;
        owlim:predicate-memory "0m" ;
        owlim:fts-memory "0" ;
        owlim:ftsIndexPolicy "never" ;
        owlim:ftsLiteralsOnly "true" ;
        owlim:in-memory-literal-properties "false" ;
        owlim:transaction-mode "fast" ;
        owlim:transaction-isolation "false" ;
        ]
	|#
