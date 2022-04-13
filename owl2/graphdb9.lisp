(in-package :cl-user)

;; Graphdb 9, as compared to graphdb that I used originally, has a different api.
;; In the original graphdb there was a SYSTEM repository and the api was via sparql queries
;; In Graphdb 9 it's a rest API that's used.

(defclass graphdb9-instance (graphdb-instance)
  ()
  (:documentation "GraphDB version 9 (http://graphdb.ontotext.com)"))

(defmethod make-instance ((g graphdb9-instance) &key)
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
  (apply (intern "ENCODE" 'jonathan.decode) args))

(defmethod repository-class  ((g graphdb9-instance))
  'graphdb9-repository)

;; endpoint is a misnomer. This is a repository. It has a query-endpoint and an update-endpoint, which are endpoints
(defclass graphdb9-repository (graphdb-sparql-endpoint)
  ()
  (:documentation "A repository within a graphdb instance"))

(defmethod repository-named ((instance graphdb9-instance) name &optional (errorp t))
  (endpoint-named instance name errorp))

(defmethod get-repository-parameters ((repo graphdb9-repository))
  (json-api-call-get repo "/rest/repositories/"))

(defmethod json-api-call-post ((repo graphdb9-repository) path json)
  (json-parse (get-url (format nil "~a~a~a" (repository-root (instance repo)) path (repo-id repo))
                                   :verb "POST" :post json)))

(defmethod json-api-call-get ((repo graphdb9-repository) path)
  (json-parse (get-url (format nil "~a~a~a" (repository-root (instance repo)) path (repo-id repo)))))

(defmethod load-file ((repo graphdb9-repository) file)
  (json-api-call-post repo "/rest/data/import/server/" (json-encode `(("fileNames" ,file)) :from :alist)))

(defmethod clear-repository ((repo graphdb9-repository))
  (json-parse (get-url (format nil "~a/repositories/~a/statements" (repository-root (instance repo)) (repo-id repo))
                       :verb "DELETE")))

;(setq *default-graphdb* (make-instance 'graphdb9-instance :root "http://127.0.0.1:7200/" :name :local-graphdb))
;(setq e (repository-named *default-graphdb* "first"))
;(get-repository-parameters e)
;(clear-repository e)




