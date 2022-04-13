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
    (ql:quickload "jonathan")))

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
  (let ((result (get-url (format nil "~a~a~a" (repository-root (instance repo)) path (repo-id repo)))))
    (ecase result-type
      (:json (json-parse result))
      (:text result))))

(defmethod load-file ((repo graphdb9-repository) file)
  (let ((answer 
          (json-api-call-post repo "/rest/data/import/server/" (json-encode `(("fileNames" ,file)) :from :alist) :result-type :text)))
    (assert (#"matches" answer "File .* sent for import.") () "Load of ~a failed with '~a'" file answer)
    t))

(defmethod clear-repository ((repo graphdb9-repository))
  (json-parse (get-url (format nil "~a/repositories/~a/statements" (repository-root (instance repo)) (repo-id repo))
                       :verb "DELETE")))

(defmethod sparql-query ((repo graphdb9-repository) query &rest keys &key query-options geturl-options (command :select) format (trace nil) &allow-other-keys)
  (declare (ignorable query-options geturl-options command format trace))
  (apply 'sparql-endpoint-query (query-endpoint repo) query  keys))

(defmethod sparql-update ((repo graphdb9-repository) query &rest keys &key query-options geturl-options (command :select) format (trace nil) &allow-other-keys)
  (declare (ignorable query-options geturl-options command format trace))
  (apply 'sparql-endpoint-query (update-endpoint repo) query  keys))

;; ****************************************************************
(eval-when (:execute :load-toplevel)
  (register-namespace "ontotext-geosparql:" "http://www.ontotext.com/plugins/geosparql#")
  (register-namespace "geosparql:" "http://www.opengis.net/ont/geosparql#" t)
  (register-namespace "simple-feature:" "http://www.opengis.net/ont/sf#"))

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
