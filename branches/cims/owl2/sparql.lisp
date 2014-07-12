(in-package :cl-user)

;; http://jena.sourceforge.net/ontology/index.html
;; http://jena.sourceforge.net/how-to/model-factory.html

;; INSERT and other mutating commands:
;; http://jena.hpl.hp.com/~afs/SPARQL-Update.html


(defvar *include-reasoning-prefix*)
(defvar *sparql-using-pellet* nil)
(defvar *sparql-namespace-uses* nil)
(defvar *sparql-allow-trace* t)
(defvar *sparql-always-trace* nil)


(defun sparql-update-load (endpoint folder-iri files)
  (sparql-endpoint-query endpoint
			 (format nil "~{load <~a>;~%~}" (mapcar (lambda(e) (concatenate 'string folder-iri e))  files)) :command "request"))

(defun sparql-endpoint-query (url query &key query-options geturl-options (command :select) format)
  (let* ((parsed (xmls::parse
		  (apply 'get-url (if (uri-p url) (uri-full url) url)
			 :post (append `((,(cond ((member command '(:select :describe :ask :construct)) "query")
						 ((member command '(:update)) "update")
						 (t command)) ,query)
					 ,(unless (eq command :update)
						  (or format (unless (eq command :update) "application/sparql-results+xml")))
					 ,@(if (eq command :select)
					       '(("should-sponge" "soft"))))
				       query-options)
			 (append geturl-options (if (eq command :construct) 
						    `(:accept ,(or format "application/rdf+xml"))
						    (if (eq command :update)
							nil
							'(:accept "application/sparql-results+xml")) )
				 (list :dont-cache t :force-refetch t)
				 ))))
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
			       (make-uri (format nil "~a~a" *blankprefix* (#"replaceAll" (format nil "~a~a" (uri-full url) (third (third binding))) "://" "_")))))
			  ((member (caar (third binding)) '("literal" "string") :test 'equal)
			   (third (third binding)))
			  (t (read-from-string (third (third binding)))))) into bound
	      finally (return (loop for variable in variables collect (cdr (assoc variable bound :test 'equal))))
	      ))))

(defvar *default-reasoner* :pellet)

(defvar *endpoint-abbreviations* nil)
;; http://www-128.ibm.com/developerworks/xml/library/j-sparql/

(defun sparql (query &rest all &key (kb (and (boundp '*default-kb*) *default-kb*)) (use-reasoner :pellet) (flatten nil) (trace nil) (trace-show-query trace) endpoint-options geturl-options (values t) (endpoint nil) (chunk-size nil) (syntax :sparql) &allow-other-keys &aux (command :select) count)
  (when chunk-size (return-from sparql (apply 'sparql-by-chunk query all)))
  (setq use-reasoner (or endpoint use-reasoner))
  (setq count (and (consp query)
		   (eq (car query) :select)
		   (getf (third query) :count)
		   (member use-reasoner '(:jena :none :pellet :sparqldl))))
  (when (typep kb 'owl-ontology)
    (setq kb (kb kb))) 
  (when (listp query) 
    (setq command (car query))
    (setq query (sparql-stringify query use-reasoner)))
  (setq use-reasoner (or (second (assoc use-reasoner *endpoint-abbreviations*)) use-reasoner))
  (if (stringp use-reasoner) (setq use-reasoner (make-uri use-reasoner)))
  (let ((do-trace (or *sparql-always-trace* (and trace  *sparql-allow-trace*))))
    (if (and do-trace trace-show-query)
      (format t "Query: ~a~%~a~%Results:~%" (or trace "Tracing all")  query)
      (if do-trace
	  (format t "Query: ~a~%Results:~%" (or trace "Tracing all"))))
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
	       (qe (cond ((or (member use-reasoner '(:sparqldl :pellet t)))
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
							(#"getModel" (kb-jena-reasoner kb)))))
			 (t (error "wtf: use-reasoner: ~a"use-reasoner))))
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
						   (#"getValue" jval)))
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

(defun sparql-stringify (form &optional reasoner &rest ignore)
  (declare (ignore ignore))
  (let ((*sparql-using-pellet* (eq reasoner :pellet))
	(*sparql-namespace-uses* nil)
	(*include-reasoning-prefix* nil)
	query)
    (setq form (eval-uri-reader-macro form))
    (setq query
	  ;; DELETE and INSERT can take WHERE clauses, not supported here yet
    (cond ((eq (car form) :insert)
	   (destructuring-bind ((&key from) &rest clauses)  (cdr form)
	     (declare (ignore clauses))
	     (with-output-to-string (s)
	       (format s "INSERT ~A { "
		     (if from (format nil "INTO GRAPH <~A>" (uri-full from)) ""))
	       (loop for clause in (cddr form)
		     do (emit-sparql-clause clause s))
	       (format s " }"))))
	  ;; ((eq (car form) :delete)
	  ;;  (destructuring-bind ((&key from) &rest clauses) (cdr form)
	  ;;    (declare (ignore clauses))
	  ;;    (with-output-to-string (s)
	  ;;      (format s "DELETE ~A { "
	  ;; 	     (if from (format nil "FROM GRAPH <~A>" (uri-full from)) ""))
	  ;;      (loop for clause in (cddr form)
	  ;; 	     do (emit-sparql-clause clause s))
	  ;;      (format s " }"))))

	  ((member (car form) '(:select :delete)) ;; change for sparql 1.1 
	   (destructuring-bind (vars (&key limit distinct from count offset order-by) &rest clauses) (cdr form)
		     (with-output-to-string (s) 
		       (let ((*print-case*  :downcase))
			 (format s (cond ((eq (car form) :select)
					  "SELECT ~a~a~{~a~^ ~}~a~a~%WHERE { ")
					 ((eq (car form) :delete)
					  "DELETE ~a~a{~{~a~^ ~}}~a~a~%WHERE { ") ;; change for sparql 1.1 - in a delete the bindings position is instead like a construct
					 (t (error "don't know how to do sparql command ~a" (car form))))
				 (if (and count (not (member reasoner '(:jena :none :pellet :sparqldl)))) "COUNT(" "")
				 (if distinct "DISTINCT " "")
				 vars 
				 (if (and count (not (member reasoner '(:jena :none :pellet :sparqldl)))) ")" "")
				 (if from (format nil "~{ FROM <~a> ~^~%~}" (mapcar 'uri-full (if (atom from) (list from) from))) "")
				 )
			 (loop for clause in clauses
			       do (emit-sparql-clause clause s))
			 (format s "} ~a~a~a"
				 (if order-by (format nil "~%ORDER BY ~{~a~^ ~} " order-by) "")
				 (if limit (format nil "LIMIT ~a " limit) "")
				 (if offset (format nil "OFFSET ~a " offset) "")
				 )))))
	  (t (error "Can't handle ~A command yet" (car form)))))
    ;; add prefixes
    (let* ((*nslookup* (mapcar 'reverse *namespace-replacements*))
	   (prefix (with-output-to-string (p)
		     (loop for ns in *sparql-namespace-uses* 
			   do (format p "PREFIX ~a <~a>~%" ns (second (assoc ns *nslookup* :test 'equal)))))))
      (setq query (concatenate 'string prefix query ))
      ;; magic?
      (if (search "reasoning:" query)
	  (format nil "PREFIX reasoning: <http://www.mindswap.org/2005/sparql/reasoning#>~%~a" query)
	  query))
    ))


(defun emit-blank-node (name stream)
  (if (eq name '[])
      (format stream "[]")
      (let ((name (subseq (string name) 1)))
	(if (equal name "")
	    (emit-blank-node '[] stream)
	    (concatenate 'string "_:" name)))))

(defun emit-sparql-clause (clause s)
  (labels ((maybe-format-uri (el)
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
		  (multiple-value-bind (string ns) (maybe-abbreviate-namespace (uri-full el) :sparql)
		    (if ns
			(progn 
			  (pushnew ns *sparql-namespace-uses* :test 'equal)
			  string)
			(if (search "urn:blank:" string)
			    (concatenate 'string "_:b" (subseq string 10) )
			    (format nil "<~a>" (uri-full el))))))
		 ((and (stringp el) (char= (char el 0) #\<)
		       (char= (char el (1- (length el))) #\>))
		  el)
		 ((consp  el)
		  (mapcar #'maybe-format-uri el))
		 (t
		  (let ((transformed (maybe-unabbreviate-namespace el)))
		    (if (eq el transformed)
			  (cond ((stringp el)
				 (if (search "urn:blank:" transformed :test 'char-equal)
				     (concatenate 'string "_:b" (subseq transformed 0 9) )
				     (format nil "~s" el)))
				((and (integerp el) (minusp el))
				 (format nil "\"~A\"^^<http://www.w3.org/2001/XMLSchema#integer>" el))
				(t el))
			(format nil "<~a>" transformed)))))))
    (cond ((eq (car clause) :optional)
	   (format s "~%OPTIONAL { ")
	   (loop for sub in (cdr clause) do (funcall 'emit-sparql-clause sub s))
	   (format s "}."))
	  ((eq (car clause) :union)
	   (loop for (sub more) on (cdr clause) do
		(format s "~% { ")
		(mapcar (lambda(c) (emit-sparql-clause c s)) sub)
		(write-string "}" s)
		(when more (write-string " UNION " s)))
	   (write-string "." s))
	  ((eq (car clause) :filter)
	   (format s "~%FILTER ")
	   (emit-sparql-filter (second clause) s))
	  ((eq (car clause) :graph)
	   (format s "graph ~a {" (maybe-format-uri (second clause)))
	   (loop for sub in (cddr clause) do
		(emit-sparql-clause  sub s))
	   (format s "~%}")(values))
	  (t (apply 'format s "~%~a ~a ~a . " (mapcar #'maybe-format-uri clause))))))

(defparameter *sparql-function-names*
  '((is-canonical "reasoning:isCanonical")
    (isiri "isIRI")
    (isliteral "isLiteral")
    (isblank "isBlank")
    (bound "bound")
    ))
 
(defun emit-sparql-filter (expression s)
  (let ((*print-case* :downcase))
    (cond ((and (consp expression)
 (assoc (car expression) '((and "&&")(or "||") (equal "=") (< "<") (> ">"))))
	   (write-char #\( s)
	   (loop for rest on (cdr expression) do 
		(emit-sparql-filter (car rest) s)
		(when (cdr rest) 
		  (format s " ~a " (second (assoc (car expression) '((and "&&")(or "||") (equal "=") (< "<") (> ">")))))))
	   (write-char #\) s))
	  ((and (consp expression) (eq (car expression) 'not))
	   (write-string "(!(" s)
	   (loop for arg in (cdr expression) do (emit-sparql-filter arg s))
	   (write-string "))" s))
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
				     (car expression)))
		 (loop for rest on (cdr expression) do 
		      (emit-sparql-filter (car rest) s)
		      (when (cdr rest) 
			(write-char #\, s)))
		 (write-char #\) s)))))))






;; (sparql-endpoint-query "http://localhost:8080/openrdf-sesame/repositories/reactome43"  
;; 				"prefix r: <http://purl.obolibrary.org/obo/reactome/record/>
;; prefix rt: <http://purl.obolibrary.org/obo/reactome/record/>
;; prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
;; prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
;; prefix xsd: <http://www.w3.org/2001/XMLSchema#>
;; prefix owl: <http://www.w3.org/2002/07/owl#>
;; construct {?s ?p ?o} where {?s ?p ?o} limit 100" :command :construct :format "text/turtle")
