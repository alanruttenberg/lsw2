(in-package :cl-user)

;; http://jena.sourceforge.net/ontology/index.html
;; http://jena.sourceforge.net/how-to/model-factory.html

;; https://www.w3.org/TR/sparql11-query/

(defvar *include-reasoning-prefix*)
(defvar *sparql-using-pellet* nil)
(defvar *sparql-namespace-uses* nil)
(defvar *sparql-allow-trace* t)
(defvar *sparql-always-trace* nil)

(defun sparql-update-load (endpoint folder-iri files)
  (sparql-endpoint-query endpoint
			 (format nil "~{load <~a>;~%~}" (mapcar (lambda(e) (concatenate 'string folder-iri e))  files)) :command "request"))

(defun sparql-endpoint-query (url query &key query-options geturl-options (command :select) format (trace nil))
  (when (consp query) 
    (setq command (car query))
    (setq query (sparql-stringify query)))
  (when trace (princ query))
  (let* ((xml
	   (apply 'get-url (if (uri-p url) (uri-full url) url)
		  :post (append `((,(cond ((member command '(:select :describe :ask :construct)) "query")
					  ((member command '(:update)) "update")
					  (t command)) ,query)
				  ,(unless (eq command :update)
				     (or format (unless (eq command :update) (list "format" "application/sparql-results+xml"))))
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
    (extract-sparql-results xml url)))
	 
(defun extract-sparql-results (xml &optional (endpoint ""))
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
		finally (return (loop for variable in variables collect (cdr (assoc variable bound :test 'equal))))))))

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
  (when (listp query) 
    (setq command (car query))
    (setq query (sparql-stringify query use-reasoner)))
  (setq use-reasoner (or (second (assoc use-reasoner *endpoint-abbreviations*)) use-reasoner))
  (if (stringp use-reasoner) (setq use-reasoner (make-uri use-reasoner)))
  (let ((do-trace (or *sparql-always-trace* (and trace  *sparql-allow-trace*))))
    (if (and do-trace (or *sparql-always-trace* trace-show-query))
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

(defun adding-sparql-prefixes (fn)
  (let* ((*sparql-namespace-uses* nil)
	 (query (funcall fn))
	 (*nslookup* (mapcar 'reverse *namespace-replacements*))
	 (prefix (with-output-to-string (p)
		     (loop for ns in *sparql-namespace-uses* 
			   do (format p "PREFIX ~a <~a>~%" ns (second (assoc ns *nslookup* :test 'equal)))))))
      (concatenate 'string (string #\linefeed) prefix query)))

(defun sparql-stringify (form &optional reasoner &rest ignore)
  (declare (ignore ignore))
  (let ((*sparql-using-pellet* (eq reasoner :pellet))
	(*blankcounter* 0))
    (setq form (eval-uri-reader-macro form))
    (let ((query (adding-sparql-prefixes 
		  (lambda()
		    (cond ((eq (car form) :select)
			   (destructuring-bind (vars (&key limit distinct from count offset order-by) &rest clauses) (cdr form)
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
			(format s "WITH ~a " with)))
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
      (let ((name (subseq (string name) 1)))
	(if (equal name "")
	    (emit-blank-node '[] stream)
	    (concatenate 'string "_:" name)))))

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
			     (format nil "<~a>" (uri-full el)))))))
	     (if star (concatenate 'string almost "*") almost))))
	((and (stringp el) (char= (char el 0) #\<)
	      (char= (char el (1- (length el))) #\>))
	 el)
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
	((eq (car clause) :bind)
	 (format s "~%BIND(")
	 (assert (equalp (string (third clause)) "AS") () "BIND missing AS")
	 (emit-sparql-filter (second clause) s)
	 (format s " AS ~a) " (fourth clause)))
	((eq (car clause) :graph)
	 (format s "graph ~a {" (maybe-sparql-format-uri (second clause)))
	 (loop for sub in (cddr clause) do
	   (emit-sparql-clause  sub s))
	 (format s "~%}")(values))
	((member (car clause) '(:minus :exists :not-exists))
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
	  (error ""))))

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
