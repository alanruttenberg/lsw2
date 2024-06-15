(in-package :cl-user)

;; http://jena.sourceforge.net/ontology/index.html
;; http://jena.sourceforge.net/how-to/model-factory.html

;; https://www.w3.org/TR/sparql11-query/

#|A lispy syntax for SPARQL

(:select (<var-or-expression>+) (<options>)
         (<clause>*)
         )
options:
  :distinct t <options>*
  :count t <options>*
  :limit <integer> <options>*
  :order-by (<var>+) <options>*
  :group-by (<var>+) <options>*
  :from <IRI> <options>*

var:
  ?<symbol>
  
var-or-expression:
   <var> | <expression>

var-or-node:
   <var> | <bnode>

bnode:
  :_<symbol>
  
symbol:
  matches regex [A-Za-z][A-Za-z0-9_]*

path:
   <IRI>
   | (* <path>) 
   | (+ <path>)
   | (? <path)
   | (/ <path>+)
   | (^ <path>)
   | (not <path>)
   | (or <path> <path>)
   | ({ <path> <number> [<number>|t])

clause:
  (<var-or-node> [<var-or-node> | <path>] [<var-or-node> | <literal>]
  | (:filter <filter-expression>) 
  | (:filter-not-exists (<clause>+)) 
  | (:filter-exists (<clause>+))
  | (:optional (<clause>+))
  | (:union (<clause>+) (<clause>+))
  | (:minus (<clause>+))
  | (:verbatim <string>)
  | (:graph <IRI> (<clause>+))
  | (:bind <var> :as <expression>)
  | (<var-or-node> [!rdfs:subClassOf | !rdf:type] <class-expression>)

class-expression:
  <IRI>
  | ([:and|:that] <class-expression>)
  | (:or <class-expression>)
  | (:not <class-expression>)
  | (:some <IRI> <class-expression>)
  | (:all <IRI> <class-expression>)
  | (:min <integer> <class-expression>)
  | (:max <integer> <class-expression>)
  | (:exactly <integer> <class-expression>)
  | (:value <IRI> [<IRI> | <literal>]

|#
(defvar *include-reasoning-prefix*)
(defvar *sparql-using-pellet* nil)
(defvar *sparql-namespace-uses* nil)
(defvar *sparql-allow-trace* t)
(defvar *sparql-always-trace* nil)

(defun sparql-update-load (endpoint folder-iri files)
  (sparql-endpoint-query endpoint
			 (format nil "~{load <~a>;~%~}" (mapcar (lambda(e) (concatenate 'string folder-iri e))  files)) :command "request"))

(defun sparql-endpoint-query (url query &key query-options geturl-options (command :select) format (trace nil) &allow-other-keys)
  (when (consp query) 
    (setq command (car query))
    (setq query (sparql-stringify query)))
  (when trace (princ query))
  (let* ((response
	   (apply 'get-url (if (uri-p url) (uri-full url) url)
		  :post (append `((,(cond ((member command '(:select :describe :ask :construct)) "query")
					  ((member command '(:update)) "update")
					  (t command)) ,query)
				  ,(unless (eq command :update)
				     (unless (eq command :update) (list "format" (or format "application/sparql-results+xml"))))
				  ,@(if (eq command :select)
					'(("should-sponge" "soft"))))
				query-options)
		  (append geturl-options (if (eq command :construct) 
					     `(:accept ,(or format "application/rdf+xml"))
					     (if (eq command :update)
						 nil
						 `(:accept ,(or format "application/sparql-results+xml"))) )
			  (list :dont-cache t :force-refetch t)
			  ))))
    (if format
        response
        (extract-sparql-results response url))))
	 
(defun extract-sparql-results (xml &optional (endpoint ""))
  (if (equal xml "")
      nil
      (let* ((parsed 
	       (if (consp xml)
		   xml
		   (xmls:parse xml)))
	     (results (find-elements-with-tag parsed "result"))
	     (variables (mapcar (lambda(e) (attribute-named e "name")) (find-elements-with-tag parsed "variable"))))
	(loop for result in results
	      collect
	      (loop for binding in (find-elements-with-tag result "binding" )
		    for name = (attribute-named binding "name")
		    collect
		    (cons name
			  (cond ((equal (caar (third binding)) "uri")
				 (make-uri (third (third binding))))
				((equal (caar (third binding)) "bnode")
				 (if (eql 0 (search "nodeID://" (third (third binding)))) 
				     (make-uri (third (third binding))) ;; hack for virtuoso, since we can then use them in queries as is.
				     (make-uri (format nil "~a~a" *blankprefix* (#"replaceAll" (format nil "~a~a" (uri-full endpoint) (third (third binding))) "://" "_")))))
				((member (caar (third binding)) '("literal" "string") :test 'equal)
				 (third (third binding)))
				(t (read-from-string (third (third binding)))))) into bound
		    finally (return (loop for variable in variables collect (cdr (assoc variable bound :test 'equal)))))))))

(defvar *default-reasoner* :pellet)

(defvar *endpoint-abbreviations* nil)
;; http://www-128.ibm.com/developerworks/xml/library/j-sparql/

#|

The arguments to the sparql function were added to over the years and need to be reorganized. 
Let's document their current behavior

query: Either a lisp-form sparql query, format documented below, or a string.

By default returns a list of lists of binding values, in the order they are in the query.

kb: The OWL ontology object (either lisp or java object)

use-reasoner: What's doing the reasoning for the query. Possible values
  :none/nil - Use this if you want to query just over the stated values 
  :pellet - Default, uses pellet reasoner, which is the only one (at the time written) that can 
     do SPARQL queries with reasoning. Pellet SPARQL works as an extension to Jena, so a jena model 
     is created from the original kb. 
  :sparqldl - Synonymous with :pellet 
  A URL or endpoint keyword. See endpoint variable below.
  :jena - I think a synonym for :none. Haven't used this in a while 
  
endpoint: Either a URL that is a SPARQL endpoint that the query is sent to, or a keyword
  which is used as a lookup on the alist *endpoint-abbreviations*
endpoint-options: passed on to sparql-endpoint-query. See doc there.
geturl-options: passed on to get-url, which sparql-endpoint-query uses. See get-url (non?) documentation.
command: If the query is lisp form no need to pass this. If textual needs to be set to one of 
  :select :describe :ask or :construct.  So that I don't have to bother parsing the text query to figure it out. 
  Needs to be known because there are different options for :select et all vs :update

Tracing is controlled also by the variables 
  *sparql-always-trace*: Globally defaults to trace
  *sparql-allow-trace*: Globally inhibits tracing

trace: t/nil. Whether to print query/query results, usually for debugging. 
trace-show-query: Whether the query should be shown in a trace, default t.
values: Whether the sparql function should return the results of the query or not. In a repl
  sometimes you only want to see the trace.

flatten: Concatenate all the bindings lists. Typical use is when you only have one binding, to return
  a list of results instead of a list of length one binding lists

syntax: :sparql or :terp. Default :sparql. If terp, see sparql-twerpish-class? for now until 
  better documentation.

labels-for: If the query is lisp form, transform the query so that the given bindings return, 
  instead of their IRI, their label. See transform-for-labels

|#


(defun sparql (query &rest all &key (kb (and (boundp '*default-kb*) *default-kb*)) (use-reasoner :pellet) (flatten nil) (trace nil) (trace-show-query trace) endpoint-options geturl-options (values t) (endpoint nil) (chunk-size nil) (syntax :sparql) labels-for explicit-literals stringify &allow-other-keys &aux (command :select) count)
  (when stringify (return-from sparql
                    (if (stringp query) query
                        (sparql-stringify query use-reasoner :labels-for labels-for))))
  (when chunk-size (return-from sparql (apply 'sparql-by-chunk query all)))
  (setq use-reasoner (or endpoint use-reasoner))
  (setq count (and (consp query)
		   (eq (car query) :select)
		   (getf (third query) :count)
		   (member use-reasoner '(:jena :none :pellet :sparqldl))))
  (when (listp query) 
    (setq command (car query))
    (setq query (sparql-stringify query use-reasoner :labels-for labels-for)))
  (setq use-reasoner (or (second (assoc use-reasoner *endpoint-abbreviations*)) use-reasoner))
  (if (stringp use-reasoner) (setq use-reasoner (make-uri use-reasoner)))
  (let ((do-trace (or *sparql-always-trace* (and trace  *sparql-allow-trace*))))
    (if (and do-trace (or *sparql-always-trace* trace-show-query))
        (format *trace-output* "Query: ~a~%~a~%~%Results:~%" (if (stringp trace) trace "")  query)
        (if do-trace
            (format *trace-output* "Query: ~a~%~%Results:~%" (if (stringp trace) trace ""))))
    (if (uri-p use-reasoner)
	(let ((bindings (sparql-endpoint-query use-reasoner query :query-options endpoint-options :geturl-options geturl-options :command command)))
	  (when do-trace
	    (loop for one in bindings
	       do (format t "~{~s~^	~}~%" one))
	    (terpri t))
	  (if flatten (loop for b in bindings append b) (if values bindings (values))))

	(let* (	;; Query query = QueryFactory.create(queryString);
	       (jquery  (#"create" 'QueryFactory query (if (eq syntax :terp)
							  (#"getInstance" 'TerpSyntax)
							  (#"lookup" 'jena.query.Syntax "SPARQL"))))
	       ;; Execute the query and obtain results
	       ;; QueryExecution qe = QueryExecutionFactory.create(query, model);
	       (qe (progn (cond ((or (member use-reasoner '(:sparqldl :pellet t)))
			         (unless (v3kb-pellet-jena-model kb)
			           (instantiate-reasoner kb :pellet-sparql nil)
			           (unless (v3kb-pellet-jena-model kb)
			             (setf (v3kb-pellet-jena-model kb) 
				           (let ((graph (new 'org.mindswap.pellet.jena.PelletReasoner)))
				             (#"createInfModel" 'com.hp.hpl.jena.rdf.model.ModelFactory
							        (#"bind" graph (#"getKB" (v3kb-reasoner kb))))))))
			         (#"prepare" (v3kb-pellet-jena-model kb))
			         (#"create" 'SparqlDLExecutionFactory jquery (v3kb-pellet-jena-model kb)))
			        ((or (eq use-reasoner :none) (eq use-reasoner nil))
			         (#"create" 'QueryExecutionFactory jquery
				            (if (java-object-p kb) kb (jena-model kb))))
			        ((or (eq use-reasoner :jena))
			         (if (java-object-p kb)
			             (#"create" 'QueryExecutionFactory jquery kb)
			             (progn
				       (unless (v3kb-pellet-jena-model kb)
				         (instantiate-reasoner kb :pellet-sparql nil))
				       (#"create" 'QueryExecutionFactory jquery (v3kb-pellet-jena-model kb)))))
			        ((eq use-reasoner :owl) (error "Not supported yet")
			         (#"create" 'QueryExecutionFactory jquery 
				            (#"createInfModel" 'modelfactory 
							       (#"getOWLReasoner" 'ReasonerRegistry)
							       ;; FIXME kb-jena-reasoner not defined
							       (#"getModel" (kb-jena-reasoner kb)))))
			        (t (error "SPARQL isn't supported with reasoner ~a. It is only supported, currently, when using reasoners :pellet, :pellet-sparql, :none" use-reasoner)))))
	       ;; ResultSet results = qe.execSelect();
	       (vars (set-to-list (#"getResultVars" jquery))))
	  (unwind-protect
	       (with-constant-signature ((getv "get") (next "next") (has-next "hasNext") (get-uri "getURI"))
		 (flet ((get-vars (bindingset)
			  (let ((bindings
				 (loop for var in vars 
				    for jval = (getv bindingset var)
				    for val = (if (null jval) 
						  nil
						  (if 
						   (#"isResource" jval)
						   (make-uri (or (get-uri jval)
								 (format nil "~a~a" *blankprefix* (#"toString" jval))
								 ))
                                                   (if explicit-literals ;; don't convert to lisp values
                                                       (progn
                                                         `(:literal
                                                           ,(#"getLexicalForm" jval)
                                                           ,(make-uri (#"getDatatypeURI" jval))))
						       (let ((val (#"getValue" jval)))
						         (if (java-object-p val) ;; handle a wierd case found in EquipmentOntology
							     (if (and (jss::jtypep  val 'BaseDatatype$TypedValue)
								      (equal (get-java-field val "datatypeURI") "http://www.w3.org/2000/01/rdf-schema#Literal"))
							         (get-java-field val "lexicalValue")
                                                                 (progn `(:literal ,(get-java-field val "lexicalValue") ,(make-uri (get-java-field val "datatypeURI")))
                                        ;(error "Don't know what this sparql value is: ~a" val)
                                                                        ))
							     val)))))
				    collect val)))
			    (when do-trace
			      (format t "~{~s~^	~}~%" bindings))
			    bindings)))
					;		 (when (and (eq use-reasoner :pellet) query-uses-blank-nodes)
					;		   (set-java-field 'PelletOptions "TREAT_ALL_VARS_DISTINGUISHED" nil))
		   ;; (if (member use-reasoner '(:pellet :jena))
;; 		       (when (kb-kb kb)
;; 			 (#"realize" (kb-kb kb))))
					; work around pellet bug
		   (let ((results (if (eq use-reasoner :pellet)
				      (#"execSelect" qe) ; (#"execQuery" (v3kb-jena-reasoner kb) jquery)

				      (#"execSelect" qe))))

		     (if count (return-from sparql (loop while (has-next results) do (next results) sum 1)))
		     (if values 
			 (if flatten 
			     (loop while (has-next results) 
				append
				(get-vars (next results)))
			     (loop while (has-next results) 
				collect 
				(get-vars (next results))))
			 (loop while (has-next results) 
			      do (get-vars (next results)) finally (return (values))
			    )
			 ))))

	    ;; Important - free up resources used running the query
	    ;; qe.close();
	    (#"close" qe)
	    (if do-trace (terpri))
	    )))))

(defun sparql-by-chunk (query &rest all &key chunk-size &allow-other-keys)
  (assert (and (consp query) (eq (car query) :select))
	  (query chunk-size)
	  "Only can do chunked queries for sparql selects using sexp syntax for now")
  (assert (and (not (find :limit (third query))) (not (find :offset (third query)))) (query)
	  "Can't specify chunk size and limit or offset in query too")
  (remf all :chunk-size)
  (loop 
     for offset from 0 by chunk-size
     for results = (apply 'sparql `(,(first query) ,(second query) (:limit ,chunk-size :offset ,offset ,@(third query))
			     ,@(cdddr query))
		   all)
     while results
     append results))

(defun adding-sparql-prefixes (fn)
  (let* ((*sparql-namespace-uses* nil)
	 (query (funcall fn))
	 (*nslookup* (mapcar 'reverse *namespace-replacements*))
	 (prefix (with-output-to-string (p)
		     (loop for ns in *sparql-namespace-uses* 
			   do (format p "PREFIX ~a <~a>~%" ns (second (assoc ns *nslookup* :test 'equal)))))))
      (concatenate 'string (string #\linefeed) prefix query)))

(defun sparql-stringify (form &optional reasoner &rest ignore &key labels-for)
  (declare (ignore ignore))
  (let ((*sparql-using-pellet* (eq reasoner :pellet))
	(*blankcounter* 0))
    (setq form (eval-uri-reader-macro form))
    (if labels-for (setq form (transform-for-labels form labels-for)))
    (let ((query (adding-sparql-prefixes 
		  (lambda()
		    (cond ((eq (car form) :select)
			   (destructuring-bind (vars (&key  limit distinct from count offset order-by group-by) &rest clauses) (cdr form)
			     (with-output-to-string (s) 
			       (let ((*print-case*  :downcase))
				 (format s "SELECT ~a~a~{~a~^ ~}~a~a~%WHERE { "
					 (if (and count (not (member reasoner '(:jena :none :pellet :sparqldl)))) "COUNT(" "")
					 (if distinct "DISTINCT " "")
					 vars 
					 (if (and count (not (member reasoner '(:jena :none :pellet :sparqldl)))) ")" "")
					 (if from (format nil "~{ FROM <~a> ~^~%~}" (mapcar 'uri-full (if (atom from) (list from) from))) "")
					 )
				 (loop for clause in clauses
				       do (emit-sparql-clause clause s))
				 (format s "} ~a~a~a"
					 (if group-by (format nil "~%GROUP BY ~{~a~^ ~} " group-by) "")
					 (if order-by (format nil "~%ORDER BY ~{~a~^ ~} " order-by) "")
					 (if limit (format nil "LIMIT ~a " limit) "")
					 (if offset (format nil "OFFSET ~a " offset) "")
					 )))))
			  ((eq (car form) :update)
			   (with-output-to-string (s)
			     (if (intersection (second form) '(:with :using :silent :into))
				 (progn (write-string (sparql-stringify-update-clause (rest form)) s) (terpri s))
				 (loop for el in (rest form) do (write-string (sparql-stringify-update-clause el) s) (write-string ";" s) (terpri s)))))
			  ((eq (car form) :construct)
			   (sparql-stringify-construct-clause form)))))))
      ;; magic?
      (if (search "reasoning:" query)
	  (format nil "PREFIX reasoning: <http://www.mindswap.org/2005/sparql/reasoning#>~%~a" query)
	  query))
    ))

(defun sparql-stringify-update-clause (form &aux qualifiers)
  "Syntax of SPARQL Update:

  (:update <clause> OR <clauses>)
  <clauses> = (<clause>)+
  <clause> = 
   (:delete <literal-triples>)
   OR (:insert <literal-triples>)
   OR [([:with <graph-uri>] [:using <graph>])] (:insert <triples>) <triples>
   OR [([:with <graph>] [:using <graph>])](:delete <triples>) <triples>
   OR [([:with <graph>] [:using <graph>])](:delete <triples>) (:insert <triples>) <triples>
   OR [(:silent <bool>)] (:create <graph>)
   OR [(:silent <bool>)] (:copy <graph-or-default> <graph-or-default>)
   OR [(:silent <bool>)] (:move <graph-or-default> <graph-or-default>)
   OR [(:silent <bool>)] (:add <graph-or-default> <graph-or-default>)
   OR [(:silent <bool>)] (:drop <graph-any>)
   OR [(:silent <bool>)] (:clear <graph-any>)
  <graph> = URI
  <graph-or-default> = <uri> OR :default
  <graph-any> = <uri> OR :default OR :any OR :named
  <bool> = t OR nil
  <literal-triple> = (<uri> <uri> <uri>)+
  <uri-or-var> = <uri> OR ?...
  <uri> are LSW URIs, <triple> is LSW SPARQL triple, which can also be twerpish, or use ':a' instead of !rdf:type.
  Note: <> =  for productions, [] for optional, 'OR' for alternatives, '+' one or more, 
  ... = symbol constituents
  anything else is literal, including '('
" 
   (flet ((graph-or-default (el)
	   (cond ((uri-p el) (maybe-sparql-format-uri el))
		 ((eq el :default) "DEFAULT")
		 (t (error "Expected graph URI or 'DEFAULT' in ~a" form))))
	 (graph-spec (el)
	   (cond ((uri-p el) (maybe-sparql-format-uri el))
		 ((member el '(:all :default :named)) (string el))
		 (t (error "Expected graph URI or 'DEFAULT','NAME', or 'ALL' in ~a" form)))))
       (with-output-to-string (s)
	 (when (and (consp (car form)) (intersection (car form) '(:with :using :silent :into)))
	   (setq qualifiers (pop form)))
	 (setq form (if (= (length form) 1) (car form) form))
	 (flet ((clauses (forms)
		  (write-char #\{ s)
		  (loop for clause in forms
			do (emit-sparql-clause clause s))
		  (write-char #\} s))
		(error-if-remaining-qualifiers ()
		  (and qualifiers (warn "Unused qualifiers: ~a" qualifiers)))
		(graph-spec (spec)
		  (if (uri-p spec) (concatenate 'string "<" (uri-full spec) ">") (string spec))))
	     (cond ((member (car  form) '(:delete :insert))
		    (format s  "~a DATA " (string (car form)))
		    (if (and (= (length (rest form)) 1)
			     (consp (car (rest form)))
			     (eq (caar (rest form)) :graph))
			(progn (format s " { GRAPH ~a " (maybe-sparql-format-uri (second (car (rest form)))))
			       (clauses (cddr (car (rest form))))
			       (format s " }" ))
			(clauses (rest form))))
		   ((and (consp (car form))
			 (member (caar form) '(:insert :delete)))
		    (let ((with (getf qualifiers :with)))
		      (remf qualifiers :with)
		      (when with
			(format s "WITH ~a " (maybe-sparql-format-uri with))))
		    (let ((head (pop form)))
		      (write-string (string (car head)) s)
		      (clauses (rest head))
		      (if (and (eq (car head) :delete)
			       (consp (car form))
			       (member (caar form) '(:insert)))
			  (let ((head (pop form)))
			    (write-string (string (car head)) s)
			    (clauses (rest head)))))
		    (let ((using (getf qualifiers :using)))
		      (remf qualifiers :using)
		      (when using
			(loop for u in (if (atom using) (list using) using) do (format s " USING ~a~%" u)))
		      (write-string " WHERE " s)
		      (clauses form)))
		   ((member (car form) '(:copy :move :add))
		    (let ((silent (getf qualifiers :silent)))
		      (remf qualifiers :silent)
		      (format s "~a ~a~a ~a" (string (car form)) (if silent "SILENT " "") (graph-or-default (second form)) (graph-or-default (third form)))))
		   ((eq (car form) :create)
		    (let ((silent (getf qualifiers :silent)))
		      (remf qualifiers :silent)
		      (format s "CREATE ~a~a" (if silent "SILENT " "") (maybe-sparql-format-uri (second form)))))
		   ((eq (car form) :load)
		    (let ((into (getf qualifiers :into))
			  (silent (getf qualifiers :silent)))
		      (remf qualifiers :into) (remf qualifiers :silent)
		      (format s "LOAD ~a~a~a~a" (if silent "SILENT " "") (maybe-sparql-format-uri (second form)) (if into " INTO " "") (if into (graph-spec into) ""))))
		   ((member (car form) '(:drop :clear))
		    (let ((silent (getf qualifiers :silent)))
		      (remf qualifiers :silent)
		      (format s "~a ~a~a" (string (car form)) (if silent "SILENT " "") (graph-spec (second form)))))
		   (t (error "don't know how to generate sparql update: ~a" form)))
	   (error-if-remaining-qualifiers)))))

(defun sparql-stringify-construct-clause (form)
  "(:construct (<triples>) <triples>)"
  (with-output-to-string (s)
    (format s "CONSTRUCT {")
    (loop for clause in (second form)
	  do (emit-sparql-clause clause s))
    (format s "}~% WHERE~% {")
    (loop for clause in (cddr form)
	  do (emit-sparql-clause clause s))
    (format s "}~%")))

(defparameter *sparql-update-examples*
  '((:update (:with !g) (:delete (?s ?p ?o)) (:insert (?s ?p ?o)) (?a :a !s))
    (:update 
     ((:with !g) (:delete (?s ?p ?o)) (:insert (?s ?p ?o)) (?a :a !s))
     ((:using !g) (:delete (?s ?p ?o)) (:insert (?s ?p ?o)) (?a :a !s))
     ((:delete (?s ?p ?o)) (:insert (?s ?p ?o)) (?a :a !s))
     (:insert (:graph !af (!a :a !g))))
    (:update (:insert (:graph !af (!a :a !g))))
    (:update ((:silent t) (:clear !g)) (:load !f))
    (:update (:load !f) (:load !g) ((:into :default) (:load !h)))
    (:update ((:with !w :using (!u !v)) (:delete (?s ?p ?o)) (:insert (?s ?p ?o)) (?a :a !s)))
    (:update (:clear :named) (:drop :default) (:copy !a :default) (:clear :all))))

(defun run-sparql-update-examples ()
  (loop for ex in *sparql-update-examples*
	for exr = (eval-uri-reader-macro ex)
	do (pprint exr) (princ (sparql-stringify exr))))

(defun emit-blank-node (name stream)
  (if (eq name '[])
      (format stream "[]")
      (let ((name (string-downcase (subseq (string name) 1))))
	(if (equal name "")
	    (emit-blank-node '[] stream)
	    ;(write-string (concatenate 'string "_:" name) nil) ;; side effect writes to standard out when stream nil
            (format stream "~a" (concatenate 'string "_:" name))
            ))))

;; Need to add the rest of these. As of now there's just "*"
;; uri	A URI or a prefixed name. A path of length one.
;; ^elt	Inverse path (object to subject).
;; (elt)	A group path elt, brackets control precedence.
;; elt1 / elt2	A sequence path of elt1, followed by elt2
;; elt1 ^ elt2	Shorthand for elt1 / ^elt2, that is elt1 followed by the inverse of elt2.
;; elt1 | elt2	A alternative path of elt1, or elt2 (all possibilities are tried).
;; elt*	A path of zero or more occurrences of elt.
;; elt+	A path of one or more occurrences of elt.
;; elt?	A path of zero or one elt.
;; elt{n,m}	A path between n and m occurrences of elt.
;; elt{n}	Exactly n occurrences of elt. A fixed length path.
;; elt{n,}	n or more occurrences of elt.
;; elt{,n}	Between 0 and n occurrences of elt.

(defun maybe-sparql-format-uri (el)
  (cond ((eq el :a)
	 "a")
	((eq el '[])
	 (emit-blank-node '[] nil))
	((and (keywordp el)
	      (char= (char (string el) 0) #\_))
	 (emit-blank-node el nil))
	((equal el "")
	 "\"\"")
	((uri-p el)
	 (let ((star nil))
		    ;; handle * pattern by first taking it off, formatting the URI and then adding it back. ;
	   (when (#"matches" (uri-full el) ".*\\*")
	     (setq el (make-uri (subseq (uri-full el) 0 (- (length (uri-full el)) 1))))
	     (setq star t))
	   (let ((almost 
		   (multiple-value-bind (string ns) (maybe-abbreviate-namespace (uri-full el) :sparql)
		     (if ns
			 (progn 
			   (pushnew ns *sparql-namespace-uses* :test 'equal)
			   string)
			 (if (search "urn:blank:" string)
			     (concatenate 'string "_:b" (subseq string 10) )
			     (if (and *allow-unknown-namespaces* (#"matches" (uri-full el) ".*:"))
                                 ;; gross hack. Allow unknown namespace passthrough so I can add temporary prefixes
                                 (uri-full el)
                                 (format nil "<~a>" (uri-full el))))))))
	     (if star (concatenate 'string almost "*") almost))))
	((and (stringp el) (char= (char el 0) #\<)
	      (char= (char el (1- (length el))) #\>))
	 el)
        ((typep el 'double-float)
         (substitute #\e #\d (format nil "~a" el)))
	((and (consp el) (eq (car el) :literal))
	 (let ((value (second el))
	       (datatype (third el)))
           (when (typep el 'double-float)
               (setq el (substitute #\e #\d (format nil "~a" el))))
	   (format nil "\"~a\"~a~a" value (if datatype "^^" "") (if datatype (maybe-sparql-format-uri datatype) ""))))
	((consp  el)
	 (mapcar #'maybe-sparql-format-uri el))
	(t
;	 (let ((transformed (maybe-unabbreviate-namespace el)))
;	   (if (eq el transformed)
	       (cond ((stringp el)
		      (if nil;(search "urn:blank:" transformed :test 'char-equal)
			  (concatenate 'string "_:b" (subseq transformed 0 9) )
			  (format nil "~s" el)))
		     ((and (integerp el) (minusp el))
		      (format nil "\"~A\"^^<http://www.w3.org/2001/XMLSchema#integer>" el))
		     (t el))
	       ;(format nil "<~a>" transformed)
	       )))
;;))

(defun emit-sparql-clause (clause s)
  (cond ((eq (car clause) :verbatim)
         (format s "~&~a~%" (second clause)))
        ((eq (car clause) :optional)
	 (format s "~%OPTIONAL { ")
	 (loop for sub in (cdr clause) do (funcall 'emit-sparql-clause sub s))
	 (format s "}."))
	((eq (car clause) :union)
	 (loop for (sub more) on (cdr clause) do
	   (format s " { ")
	   (mapcar (lambda(c) (emit-sparql-clause c s)) sub)
	   (write-string "}" s)
	   (when more (write-string " UNION " s)))
	 (write-string "." s))
        ((eq (car clause) :graph)
	 (format s " Graph ~a {" (if (uri-p (second clause))
                                     (format nil "<~a>" (uri-full (second clause)))
                                     (second clause)))
	 (loop for sub in (cddr clause) do
           (emit-sparql-clause sub s))
	 (write-string "}" s))
	((eq (car clause) :filter)
	 (format s "~%FILTER ")
	 (emit-sparql-filter (second clause) s))
        ((member (car clause) '(:filter-not-exists :filter-exists))
	 (format s "~%FILTER ~aEXISTS {" (if (eq (car clause) :filter-exists) "" "NOT "))
         (map nil (lambda(x) (emit-sparql-clause x s)) (second clause))
         (if (third clause)
	      (emit-sparql-filter (third clause) s))
         (format s "}"))
	((eq (car clause) :with)
	 ;; expect either an atom then pairs or a pair then atoms
	 (if (atom (second clause))
	     (loop for (pred obj) in (cddr clause) do (emit-sparql-clause (list (second clause) pred obj) s ))
	     (loop for obj in (cddr clause) do (emit-sparql-clause (list (car (second clause)) (second (second clause)) obj) s))))
	((eq (car clause) :bind)
	 (format s "~%BIND(")
                                        ;	 (assert (equalp (string (third clause)) "AS") () "BIND missing AS")
         (assert (find :as clause :test 'equalp))
         (if (atom (second clause))
             (format s "~a" (sparql-path (second clause) t))
	     (emit-sparql-filter (subseq clause 1 (- (length clause) 2)) s))
	 (format s " AS ~a) " (car (last clause))))
	((eq (car clause) :graph)
	 (format s "graph ~a {" (maybe-sparql-format-uri (second clause)))
	 (loop for sub in (cddr clause) do
	   (emit-sparql-clause  sub s))
	 (format s "~%}")(values))
	((member (car clause) '(:minus))
	 (format s "~%~a {"
		 (string (car clause)))
	 (loop for sub in (cdr clause) do
	   (emit-sparql-clause  sub s))
	 (format s "~%}.")(values))
	((and (member (second clause) `(,!rdf:type ,!rdfs:subClassOf))
	      (sparql-twerpish-class? (third clause)))
	 (translate-sparql-twerp-object clause s))
	((sparql-twerpish-class? clause)
	 (translate-sparql-twerp clause s))
	((and (= (length clause) 3) (consp (second clause))) ; path
	 (format s "~%~a ~a ~a . " (maybe-sparql-format-uri (car clause)) (sparql-path (second clause) t) (maybe-sparql-format-uri (third clause))))
	(t (apply 'format s "~%~a ~a ~a . " (mapcar #'maybe-sparql-format-uri clause)))))

(defun sparql-path (form &optional top)
  "SPARQL path syntax
elt is either URI or expression
!elt -> (not elt)   - negated property set
elt/elt/... - (/ &rest elt ) - group path 
elt|elt|... - (or &rest elt) - property set
^elt -> (^ elt) - inverse path
elt{n} -> ({ elt n) - n occurrences
elt{n,} -> ({ elt n t) - at least n occurrences
elt{,n} -> ({ elt 0 n) - at most n occurrences 
elt{n,m} -> ({ elt n m) - between n and m occurrences 
elt+ -> (+ elt) - one or more occurences
elt* -> (* elt) - zero or more occurences
elt? -> (? elt) - zero or one occurences

e.g. (sparql '(:select (?element) () 
                 (:_list (/ (* !rdf:rest) !rdf:first) ?element)))

See: https://www.w3.org/2009/sparql/docs/property-paths/Overview.xml
"
  (if (consp form)
      (let ((head  (car form))
	    (tail (cdr form)))
	(ecase head
	  (or (format nil (if top "~{~a~^|~}" "(~{~a~^|~})") (mapcar 'sparql-path tail)))
	  (/ (format nil (if top "~{~a~^/~}" "(~{~a~^/~})") (mapcar 'sparql-path tail)))
	  ({ (cond ((= (length tail) 2)
		    (format nil "~a{~a}" (sparql-path (car tail)) (second tail)))
		   ((and (= (length tail) 3) (eq (second tail) t))
		    (format nil "~a{~a,}" (sparql-path (car tail)) (second tail)))
		   ((and (= (length tail) 3)) 
		    (apply 'format nil "~a{~a,~a}" (sparql-path (car tail)) (cdr tail)))
		   ((t (error "")))))
	  ((* + ?)
	   (assert (not (cdr tail)) () "")
	   (format nil "~a~a" (sparql-path (car tail)) head)
	   )
	  ((^ not)
	   (assert (not (cdr tail)) () "")
	   (format nil "~a~a" (if (eq head 'not) "!" head) (sparql-path (car tail)))
	   )))
      (if (uri-p form)
	  (maybe-sparql-format-uri form)
	  (error "Error in sparql path"))))

(defun sparql-twerpish-class? (clause)
  (and (consp clause)
       (memq (car clause) '(:and :or :some :all :min :max :exactly :only :value :that :not))))

(defparameter *sparql-function-names*
  '((is-canonical "reasoning:isCanonical")
    (isiri "isIRI")
    (isliteral "isLiteral")
    (isnumeric "isLiteral")
    (isblank "isBlank")
    (bound "bound")
    (uri "URI")
    (concat "CONCAT")
    (strafter "STRAFTER")
    (if "IF")
    (coalesce "COALESCE")
    (exists "EXISTS")
    (not-exists "NOT EXISTS")
    (sameterm "sameTerm")
    (datatype "DATATYPE")
    (str "STR")
    (regex "REGEX")))
 
(defun emit-sparql-filter (expression s)
  (let ((*print-case* :downcase))
    (cond ((and (consp expression)
                (assoc (car expression) '((and "&&")(or "||") (equal "=") (<= "<=") (>= ">=") (< "<") (> ">"))))
	   (write-char #\( s)
	   (loop for rest on (cdr expression) do 
	     (emit-sparql-filter (car rest) s)
	     (when (cdr rest) 
	       (format s " ~a " (second (assoc (car expression) '((and "&&")(or "||") (equal "=") (< "<") (> ">") (<= "<=") (>= ">=")))))))
	   (write-char #\) s))
	  ((and (consp expression) (eq (car expression) 'not))
	   (write-string "(!(" s)
	   (loop for arg in (cdr expression) do (emit-sparql-filter arg s))
	   (write-string "))" s))
          ((typep expression 'double-float)
           (write-string (substitute #\e #\d (format nil "~a" expression)) s))
	  ((and (consp expression) (eq (car expression) :literal))
	   (let ((value (second expression))
	         (datatype (third expression)))
             (when (typep value 'double-float)
               (write-string (substitute #\e #\d (format nil "~a" value)) s))
	     (format s "~a~a~a~a~a"
                     (if (stringp value) "" "\"")
                     value
                     (if (stringp value) "" "\"")
                     (if datatype "^^" "") (if datatype (maybe-sparql-format-uri datatype) ""))))
	  ((and (keywordp expression)
		(char= (char (string expression) 0) #\_))
	   (emit-blank-node expression s))
	  ((uri-p expression)
	   (format s " <~a> " (uri-full expression)))
	  ;; MT tentative addition -- try to get FILTER clause in right format (still requires Jena)
 	  ((or (stringp expression)
	       (numberp expression))
 	   (format s "~s" expression))
	  (t
	   (if (atom expression)
	       (if (stringp expression)
		   (format s "~s" expression)
		   (princ (string-downcase (string expression)) s))
	       (progn
		 (format s "~a(" (or (second (assoc (car expression) *sparql-function-names*))
				     (if (uri-p (car expression))
                                         (maybe-sparql-format-uri (car expression))
                                         (car expression)
                                         )))
		 (loop for rest on (cdr expression) do 
		   (emit-sparql-filter (car rest) s)
		   (when (cdr rest) 
		     (write-char #\, s)))
		 (write-char #\) s)))))))

;; Take a query in lisp form and a list of variables that you want labels for.
;;
;; Suppose the variable is ?var. Rewrite all places (except select) changing
;; that variable to ?var_inst. Then add (?var_inst !rdfs:label ?var)
;;
;; e.g. (transform-for-labels '(:select (?a ?b) ()  (?a !ex:r ?b)) '(?a))
;; -> '(:select (?a ?b) nil (?a_inst !rdfs:label ?a) (?a_inst !ex:r ?b))
;;
;; We need to add the label triple beside other bindings of the unlabeled.
;; At each level we have bgp + maybe :optional, :minus, :union, :filter[-exists|-not-exists] forms
;; DOES NOT HANDLE VERBATIM, SO DONT USE VERBATIM WITH LABEL REPLACEMENT
;; If the triples at any level contains one of the ?vars, we can add the label there
;; and not bother recursing 
;; Otherwise recurse on the special forms 
(defun transform-for-labels (query labels)
  (labels (;; given ?var creates triple (?var_ !rdfs:label ?var)
           (label-triple (el)
             (let ((new-var 
                     (intern (format nil "~a_" (string el)) (symbol-package el))))
               `(,new-var ,!rdfs:label ,el)))
           ;; Given the body of a sparql query, transform it by rewriting ?var as ?var_ then
           ;; add a label triple. pattern is all the forms in the body
           (transform-1 (pattern labels)
             ;; We're going to use nsubst to replace the ?var with ?var_ so make a copy first
             (setq pattern (mapcar 'copy-list pattern)) 
             ;; Triples will be forms in the body that are triple patterns, or BIND statements
             ;; Triples will be augmented by label-triples 
             ;; The others are :union, :optional, :minus, and :filters and they will be
             ;; transformed by a recursive call. Critically, if a label-triple has been added
             ;; before the call, it won't need to be added in a recursive call.
             ;; The result will be that the label-triple will be added at the highest level
             ;; that there are triples that constrain it. Other wise we can have:
             ;; * (:optional (?a_ !p ?b)) (?a_ !rdfs:label ?a)). If the optional doesn't result in a binding
             ;; we get (?a_ !rdfs:label ?a) without further constraint, triggering a cross produce
             ;; as any of the other bindings could be paired with every possible label of something else.
             ;; So instead of *, we have (:optional (?a_ !p ?b) (?a_ !rdfs:label ?a)) - the label-triple
             ;; is inside the optional, so it won't have a label binding unless the optional block is satisfied
             ;; triples will be the elements we aren't recursing into.
             (let ((triples (remove-if (lambda(el) (and (keywordp (car el)) (not (eq el :bind))))
                                       pattern)))
               ;; Extras will be the label-triples to add at this level
               (let* ((extras
                        (loop for label in labels
                              if (tree-find label triples)
                              collect (label-triple label))))
                 ;; Once we've determined which ?vars are going to be replaced with ?var_
                 ;; we remove ?var from the labels so we don't also add the label-triple in another place.
                 (setq labels (set-difference labels (mapcar 'third extras)))
                 ;; Here's the recursive call,which is done for any form that has a nested pattern.
                 ;; Again, not sure I have to do this in all cases, but better safe than sorry
                 (let ((rewritten 
                         (loop for el in pattern
                               ;; These forms have one pattern as argument
                               collect (cond ((member (car el) '(:optional :minus :filter-exists :filter-not-exists))
                                              `(:optional ,@(transform-1
                                                             (cdr el) 
                                                             labels)))
                                             ;; Union has two patterns as arguments
                                             ((eq (car el) :union)
                                              `(:union ,(transform-1 (second el) labels)
                                                       ,(transform-1 (third el) labels)))
                                             ;; Anything else we don't recurse on
                                             (t el)) into rewritten
                               finally 
                                  ;; Now we do the replacement of ?var with ?var_ in the initial form
                                  ;; We can reuse extras to get the necessary replacements
                                  ;; We use nsubst since it will poke the replacement where we want it
                                  ;; without having to do another walk 
                                  (loop for (instance-var nil label-var) in extras
                                        do (setq rewritten (nsubst instance-var label-var rewritten)))
                                  ;; The result is the rewritten pattern, followed by the label-triples
                                  (return (append rewritten extras)))))
                   rewritten
                   )))))
    ;; Top level is (:select <vars> <options> &rest pattern). :select, <vars>, and <options> are to be
    ;; left alone. We only transform the body pattern
    ;; The intersection with vars is because if we're not selecting the variable then there's no
    ;; point computing the label, despite what the called has said.
    (destructuring-bind (cmd vars options . pattern) query
        `(,cmd ,vars ,options ,@(transform-1 pattern (intersection labels vars))))))
    


;; (sparql-endpoint-query "http://localhost:8080/openrdf-sesame/repositories/reactome43"  
;; 				"prefix r: <http://purl.obolibrary.org/obo/reactome/record/>
;; prefix rt: <http://purl.obolibrary.org/obo/reactome/record/>
;; prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
;; prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
;; prefix xsd: <http://www.w3.org/2001/XMLSchema#>
;; prefix owl: <http://www.w3.org/2002/07/owl#>
;; construct {?s ?p ?o} where {?s ?p ?o} limit 100" :command :construct :format "text/turtle")
