(in-package :cl-user)

(defclass sparql-repository-set ()
  ((root :initarg :root :accessor repository-root)
   (repositories :accessor repositories :initarg :repositories :initform (make-hash-table :test 'equal))
   (name :accessor name :initarg :name))
  (:documentation "Some system that manages multiple sparql endpoints")
)

(defclass openrdf-sesame-instance (sparql-repository-set)
  ((system-endpoint :accessor system-endpoint :initarg system-endpoint))
  (:documentation "An OpenRDF Sesame repository set. There is a distinguished SYSTEM repo for configuring other repositories in the set")
  ) 

(defmethod system-endpoint :around ((instance openrdf-sesame-instance))
  "Lazy initialization of system endpoint the first time its asked for"
  (or (and (slot-boundp instance 'system-endpoint) (call-next-method))
      (progn (get-repositories instance)
	     (setf (system-endpoint instance) (gethash "SYSTEM" (repositories instance))))))

(defmethod endpoint-named ((instance openrdf-sesame-instance) name)
  "Allways refresh the list of repositories as they might be added from outside"
  (get-repositories instance)
  (gethash name (repositories instance)))
  
(defclass graphdb-instance (openrdf-sesame-instance)
  ()
  (:documentation "GraphDB (http://graphdb.ontotext.com) is based on Sesame"))

(defclass sparql-endpoint ()
  ((query-endpoint :accessor query-endpoint :initarg :query-endpoint)
   (update-endpoint :accessor update-endpoint :initarg :update-endpoint))
  (:documentation "A sparql endpoint migtht actually consist of a number of different services, in particular query and update"))

(defclass sesame-sparql-endpoint ()
  ((repo-id :accessor repo-id :initarg :repo-id)
   (readable :accessor readable :initarg :readable)
   (writeable :accessor writeable :initarg :writeable)
   (title :accessor title :initarg :title)
   (instance :accessor instance :initarg :instance))
  (:documentation "A Sesame SPARQL endpoint has an id, title, as well as read/write flags"))

(defmethod print-object ((e sesame-sparql-endpoint) stream)
  (print-unreadable-object (e stream :type t)
    (unless (not (slot-boundp e 'repo-id))
      (format stream "~a@~a" (repo-id e) (name (instance e))))))

(defmethod namespaces-endpoint ((endpoint sesame-sparql-endpoint))
  "The namespaces endpoint of a Sesame endpoint is a service location for accessing prefixes known to the endpoint" 
  (make-uri (concatenate 'string (uri-full (query-endpoint endpoint)) "/namespaces")))

(defmethod repositories-endpoint ((instance openrdf-sesame-instance))
  "The repositories endpoint of a Sesame endpoint is a service location for accessing information about the repositories in the set" 
  (make-uri (concatenate 'string (uri-full (root instance)) "/namespaces")))

(defmethod repositories-endpoint ((endpoint sesame-sparql-endpoint))
  (repositories-endpoint (instance endpoint)))

(defclass graphdb-sparql-endpoint (sesame-sparql-endpoint)
  ()
  (:documentation "A SPARQL endpoint within a graphdb instance"))

(defgeneric endpoint-class (instance) (:documentation "The class of SPARQL endpoint associated with endpoints in an instance"))

(defmethod endpoint-class  ((g openrdf-sesame-instance))
  'sesame-sparql-endpoint)

(defmethod endpoint-class  ((g graphdb-instance))
  'graphdb-sparql-endpoint)
  
(defvar *default-graphdb* (make-instance 'graphdb-instance :root "http://127.0.0.1:8080/graphdb-workbench-ee/" :name :local-graphdb))

(register-namespace "sesamerep:" "http://www.openrdf.org/config/repository#") ;; called rep: sometimes
(register-namespace "owlim:" "http://www.ontotext.com/trree/owlim#") 
(register-namespace "sailrepository:" "http://www.openrdf.org/config/repository/sail#") ;; called sr: sometimes
(register-namespace "sesamesail:" "http://www.openrdf.org/config/sail#") ;; called sail: sometimes

(defmethod get-repositories ((instance openrdf-sesame-instance))
  "Get the list of repositories (sparql endpoint objects) for a sesame instance"
  (with-input-from-string (s (get-url (repositories-endpoint instance) :accept  "text/csv" :force-refetch t))
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
	(system (query-endpoint (system-endpoint (instance repo)))))
    (sparql-endpoint-query
     system
     `(:select (?param ?value) ()
	(?graph !rdf:type ?graphtype)
	(:graph ?graph
		(?rep !sesamerep:repositoryID ,endpoint-id)
		(?rep (/ !sesamerep:repositoryImpl (* !sesamerep:delegate) !sailrepository:sailImpl) ?sail)
		(?sail !sesamesail:sailType ?type)
		(?sail ?param ?value))))))

(defmethod get-endpoint-parameter ((repo graphdb-sparql-endpoint) param)
  (let ((endpoint-id (repo-id repo))
	(system (query-endpoint (system-endpoint (instance repo)))))
    (caar (sparql-endpoint-query
     system
     `(:select (?value) ()
	(?graph !rdf:type ?graphtype)
	(:graph ?graph
		(?rep !sesamerep:repositoryID ,endpoint-id)
		(?rep (/ !sesamerep:repositoryImpl (* !sesamerep:delegate) !sailrepository:sailImpl) ?sail)
		(?sail !sesamesail:sailType ?type)
		(?sail ,param ?value)))))))

(defmethod set-endpoint-parameter ((repo graphdb-sparql-endpoint) param value)
  (let ((endpoint-id (repo-id repo))
	(system (system-endpoint (instance repo))))
    (destructuring-bind (oldvalue graph)
	(car (sparql-endpoint-query
	      (query-endpoint system)
	      `(:select (?oldvalue ?graph) ()
		 (:graph ?graph
			 (?rep !sesamerep:repositoryID ,endpoint-id)
			 (?rep (/ !sesamerep:repositoryImpl (* !sesamerep:delegate) !sailrepository:sailImpl) ?sail)
			 (?sail !sesamesail:sailType ?type)
			 (:optional (?sail ,param ?oldvalue))))))
    (sparql-endpoint-query
     (update-endpoint system)
     `(:update  
       ((:with ,graph) ,@(if oldvalue `((:delete (?sail ,param ,oldvalue)))) (:insert (?sail ,param ,value))
	(?rep !sesamerep:repositoryID ,endpoint-id)
	(?rep (/ !sesamerep:repositoryImpl (* !sesamerep:delegate) !sailrepository:sailImpl) ?sail)
	(?sail !sesamesail:sailType ?type)
	))
     )
      oldvalue)))
    
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
				   (:with :_impl
					  (!sesamerep:repositoryType "owlim:ReplicationClusterWorker")
					  (!sesamerep:delegate :_delegate) )
				   (:with :_delegate
					 (!sesamerep:repositoryType "owlim:MonitorRepository")
					 (!sailrepository:sailImpl :_sail))
				   (:with :_sail
					  (!sesamesail:sailType "owlimClusterWorker:Sail" )
					  (!owlim:base-URL "urn:shouldnt:be:here:" )
					  (!owlim:defaultNS "")
					  (!owlim:entity-index-size ,(prin1-to-string entity-index-size))
					  (!owlim:entity-id-size "32")
					  (!owlim:imports "")
					  (!owlim:repository-type "file-repository")
					  (!owlim:ruleset "owl-horst-optimized" )
					  (!owlim:storage-folder "storage")
					  (!owlim:cache-memory ,(m cache-memory))

					  (!owlim:query-timeout "0")
					  (!owlim:query-limit-results "0")

					  (!owlim:enablePredicateList ,enable-predicate-list)
					  (!owlim:read-only "false")
					  (!owlim:in-memory-literal-properties ,in-memory-literal-properties)
					  (!owlim:enable-literal-index "true")

					  (!owlim:check-for-inconsistencies "false")
					;(!owlim:disable-sameAs "false")

					;(!owlim:enable-context-index ,context-index)

					;(!owlim:throw-QueryEvaluationException-on-timeout "false")

					;(!owlim:nonInterpretablePredicates "http://www.w3.org/2000/01/rdf-schema#label;http://www.w3.org/1999/02/22-rdf-syntax-ns#type;http://www.ontotext.com/owlim/ces#gazetteerConfig;http://www.ontotext.com/owlim/ces#metadataConfig")

					  (!owlim:tuple-index-memory ,(m tuple-index-memory))
					  (!owlim:predicate-memory ,(m predicate-memory))
					  (!owlim:fts-memory ,(m full-text-memory))
					  (!owlim:ftsIndexPolicy ,full-text)
					  (!owlim:ftsLiteralsOnly ,full-text-literals-only)
					  (!owlim:transaction-mode ,transaction-mode)
					  (!owlim:transaction-isolation ,transaction-isolation))))
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
