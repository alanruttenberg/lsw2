(in-package :cl-user)

;; This file translates an ontology in rdf/xml into the owl2 lisp syntax 

;; The strategy is to first translate to functional syntax, and then use a custom
;; readtable to parse that. Some post processing to put rearrange things a bit
;; puts it into either  (with- ..) form or as an ontology form ready to pass to t-jena.
;;
;; (pprint-owl-lisp-syntax  "http://www.biopax.org/release/biopax-level2.owl" :biopax)
;; will print out a form which can be evaluated to and create an instance of owl-ontology bound to 
;; the variable biopax-level-2. URIs with labels will be printed as !'my label'@biopax.


(defparameter *as-readtable* (copy-readtable))

(defparameter *owl-keyword-terms* (make-hash-table))
(defvar *debug-owl-parse* nil)

;; make case sensitive
(setf (readtable-case *as-readtable*) :preserve)

;; reuse the uri reader macro. It understands what to do when the first character is 
;; a < rather than a !
(set-macro-character  #\< 'read-uri nil  *as-readtable*)

;; gensyms for colons, and equals. 
(defparameter *colon* (make-symbol "colon"))
(defparameter *equals* (make-symbol "="))

;; These come in as separate tokens. We reassemble later
(defun return-colon (stream char)
  (declare (ignore stream char))
  *colon*)

(defun return-equal (stream char)
  (declare (ignore stream char))
  *equals*)

(defun return-carets (stream char)
  (declare (ignore stream char))
  (assert (char= (peek-char t stream ) #\^) () "Expected ^^ but got ^~a" (peek-char t stream))
  (read-char stream )
  :^^)

(set-macro-character #\: 'return-colon nil *as-readtable*)
(set-macro-character #\= 'return-equal nil *as-readtable*)
(set-macro-character #\^ 'return-carets nil *as-readtable*)


;; Tokenize by calling read using our custom readtable until we run out of "forms".
;; All symbols are interned as keywords.
(defun read-and-tokenize-functional-syntax (stream &optional (eof-marker :eof))
  (let ((*readtable* *as-readtable*)
	(*package* (load-time-value (find-package :keyword))))
    (loop for form = (read stream nil eof-marker)
	 until (eq form :eof)
	 collect form)))

;; do the work. 
(defun owl-to-lisp-syntax (ontology &optional (bare? nil))
  (let ((as (to-owl-syntax ontology :functional)))
;    (print as)
    (with-input-from-string (s (regex-replace-all "_value" as "_ value"))
      (multiple-value-bind (axioms ontology-iri version-iri namespaces)
	  (parse-functional-syntax (read-and-tokenize-functional-syntax s))
;	(print-db ontology-iri version-iri namespaces)
	(let ((base (second (assoc nil namespaces))))
	  (when (eql (search "##" base :from-end t) (- (length base) 2)) ;; fix a sometimes bug
	    (setq base (subseq base (1- (length base)))))
	  (let ((ontnamevar (intern "ONT")))
	    (if bare?
		(maybe-reorder-assertions
		 `(ontology ,@(and ontology-iri (list ontology-iri))
			    ,@(and version-iri (list version-iri))
			    ,@axioms))
		(values `(with-ontology ,ontnamevar
			     (:collecting t :base ,(and base (#"replaceFirst" base "#*$" "")) ;; shouldn't be necessary 
					  :ontology-iri ,ontology-iri
					  :version-iri ,version-iri
					  :about ,(make-uri (v3kb-name ontology))
					  ))
			axioms
			ontnamevar
			base))))))))
 
;; since : are separate tokens, we need to reassemble the qnames,
;; which are 3 tokens, into a single uri. That's what this function
;; does. Also have to handle the bare "http:" in the namespace
;; definitions (must these always be http?)

(defun collapse-qnames (tokenized namespaces)
  (let ((*namespace-replacements* 
	 (append (mapcar 'reverse namespaces) *namespace-replacements*)))
    (labels ((doit (tok)
	     (cond ((atom tok) tok)
		   ((consp tok) 
		    (loop 
		       while tok
		       for (this colon? that) = tok
		       collect (cond ((eq colon? *colon*)
				      (setq tok (cdddr tok))
				      (let ((*print-case* :upcase))
					(make-uri (format nil "~a~a"
							  (if (eq :|http| this) 
							      "http:"
							      (second  (assoc (princ-to-string this) namespaces
									      :test 'equal)))
							  (princ-to-string that)))))
				     ((consp this)
				      (setq tok (cdr tok))
				      (doit this))
				     (t (setq tok (cdr tok)) this)))))))
      (doit tokenized))))

;; Read the namespace declarations, then call parse-ontology, after
;; converting qnames to uri's using these namespaces.

;; Prefix(iao:=<http://purl.obolibrary.org/obo/iao/>)
;; ...
;; Ontology(<http://purl.obolibrary.org/obo/iao/dev/iao-main.owl>
;;  Import(<http://protege.stanford.edu/plugins/owl/dc/protege-dc.owl>)
;;  ...
;;  Annotation(rdfs:seeAlso <http://code.google.com/p/information-artifact-ontology/>)
;;  Annotation(owl:versionInfo \"$Revision$\"@en)
;;  ...

(defun parse-functional-syntax (tokenized)
  (setq tokenized (eval-uri-reader-macro tokenized))
  (loop for top = (car tokenized)
     with namespaces
     while (eq top :|Prefix|)
     do  
     (multiple-value-bind (namespace rest) (parse-namespace (cdr tokenized))
       (setq tokenized rest)
       (push namespace namespaces))
     finally 
     (unless (eq top :|Ontology|)
       (error "Expecting Ontology statement but got : ~a" top))
       	(let* ((base (second (assoc nil namespaces)))
	      (*default-uri-base* base))
	  (return (parse-ontology (eval-uri-reader-macro (collapse-qnames (cdr tokenized) namespaces)) namespaces)))))

;; Namespaces come in as 4 tokens, abbreviate *colon* '=' uri
(defun parse-namespace (tokenized)
  (let ((args (if (eq (caar tokenized) *colon*)
		  (cons nil (car tokenized))
		  (car tokenized))))
    (assert (and (eq (second args) *colon*)
		 (eq (third args) *equals*)
		 (uri-p (fourth args)))
	    ()
	    "Malformed namespace declaration: ~a" args)
    (values (list (and (car args) (symbol-name (car args))) (uri-full (fourth args)))
	    (cdr tokenized))))

(defun parse-ontology (tokenized namespaces)
  (let* ((tokens (car tokenized))
	 (forms (setq @ (rearrange-functional-syntax-parens-for-lisp tokens)))
	 (ontology-iri (pop forms))
	 (version-iri (and (uri-p (car forms)) (pop forms))))
    (values forms ontology-iri version-iri namespaces)))


;; Change infix to prefix by assuming that keywords (other than the
;; owl keywords like partial etc) are function calls, so the next form
;; are the arguments. Literals other than xsd:string and xsd:int are
;; translated to (literal value-as-string type-uri).

(defun rearrange-functional-syntax-parens-for-lisp (tokenized)
  (and *debug-owl-parse*
       (format t "Enter rearrange-functional-syntax-parens-for-lisp with: ~a~%" tokenized))
  (let ((xsd-string (make-uri "http://www.w3.org/2001/XMLSchema#string"))
	(xsd-int  (make-uri "http://www.w3.org/2001/XMLSchema#int"))
	(xsd-float  (make-uri "http://www.w3.org/2001/XMLSchema#float")))
    (if (atom tokenized) 
	tokenized
	(loop for (f args) = tokenized  
	   while tokenized
	   do (and *debug-owl-parse* (format t "At loop start f: ~a args: ~a~%" f args))
	   collect
	     (cond ( ;; translate owl keywords like "partial" to appropriate lisp keywords
		    (and (symbolp f) (gethash (symbol-name f) *owl-keyword-terms*))
		    (prog1
		      (gethash (symbol-name f) *owl-keyword-terms*)
		    (and *debug-owl-parse* (format t "Keyword or uri: ~a~%" f))
		    (setf tokenized (cdr tokenized))))
		 (;; leave uris alone
		  (uri-p f)
		  (prog1 f (setf tokenized (cdr tokenized))))
		 (;; anonymous nodes are sometimes translated to this, e.g. 
		  ;; pellet-svn/trunk/test_data/owl-test/DatatypeProperty/consistent001.rdf
		  ;; leave them as is for now. Don't know what to do with them yet, though
		  (eq f :_)
		  (prog1
		      f
		    (setf tokenized (cdr tokenized))))
		 ( ;; if we're not a symbol, then we're a literal, or we should be left alone
		  (not (symbolp f))
		  (if (eq args :^^)
		      (prog1 
			(cond ((eq (third tokenized) xsd-string)
				    f)
				   ((eq (third tokenized) xsd-int)
				    (parse-integer f))
				   ((eq (third tokenized) xsd-float)
				    (read-from-string f))
				   (t
				    `(literal ,f ,(third tokenized))))
			(setf tokenized (cdddr tokenized)))
		      (if (and (stringp f)
			       (keywordp (second tokenized))
			       (char= #\@ (char (string (second tokenized)) 0)))
			  (prog1 (format nil "~a~a" f (second tokenized))
			    (setf tokenized (cddr tokenized)))
			  (prog1 f (setf tokenized (cdr tokenized))))))
		 (t ;; we're a function, change to prefix after recursively processing args
		  (prog1 
		      (let ((temp (rearrange-functional-syntax-parens-for-lisp args)))
			(cons (or (or (second (gethash (symbol-name f) *owl2-vocabulary-forms*))
				      (first (gethash (symbol-name f) *owl2-vocabulary-forms*)))
				  (progn
				    (warn "Don't know what function '~a' is in '~a'~%" f `(,f ,args))
				    f))
			      ;; make rdfs comments prettier by moving them to the front of the expression (after the name)
			      (if (member (symbol-name f) '("Class" "Individual" "DatatypeProperty" "ObjectProperty") :test 'equal)
				  (maybe-move-rdfs-comment temp)
				  temp)))
		      (and *debug-owl-parse* (format t "Function head: ~a, args: ~a, after: ~a~%" f args temp))
		      (setf tokenized (cddr tokenized)))))))))

;; print out a nice lispy version of an ontology 
(defun pprint-owl-lisp-syntax (ontology-location label-source-key)
  (let ((*print-case* :downcase)
	(*print-right-margin* 150))
    (let ((ontology (load-ontology ontology-location)))
      (make-instance 'label-source :key label-source-key :sources (list ontology))
      (let ((*print-uri-with-labels-from* (list label-source-key)))
	(multiple-value-bind  (header definitions ontvar) (owl-to-lisp-syntax ontology)
	    (let ((header-string (with-output-to-string (s) (pprint header s))))
	      (write-string (subseq header-string 0 (1- (length header-string))))
	      (format t "~%  ((asq~%~{    ~s~%~}  ))~%  ~a)~%" definitions ontvar)
	      (decache-uri-abbreviated)
	      (values)))))))


;; pull out rdfs:comments, and put them (as bare strings) after the class/individual/property name
(defun maybe-move-rdfs-comment (args)
  (let ((comment (find-if (lambda(el)(and (consp el) 
					  (eq (first el) 'annotation)
					  (eq (second el) !rdfs:comment)))
			  args)))
    (if comment
	(list* (car args) (third comment) (remove comment (rest args)))
	args)))
