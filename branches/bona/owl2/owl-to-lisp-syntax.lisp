(in-package :cl-user)

;; This file translates an ontology in rdf/xml into the owl lisp syntax defined 
;; in lisp-syntax.lisp. 

;; The strategy is to first translate to abstract syntax, and then use a custom
;; readtable to parse that. Some post processing to put rearrange things a bit
;; puts it into a (define-ontolog ..) form.
;;
;; (pprint-owl-lisp-syntax  "http://www.biopax.org/release/biopax-level2.owl" 'biopax-level-2)
;; will print out a form which can be evaluated to and create an instance of owl-ontology bound to 
;; the variable biopax-level-2
;;

(defparameter *as-readtable* (copy-readtable))

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
(defun as-tokenize (stream &optional (eof-marker :eof))
  (let ((*readtable* *as-readtable*)
	(*package* (load-time-value (find-package :keyword))))
    (loop for form = (read stream nil eof-marker)
	 until (eq form :eof)
	 collect form)))

;; do the work. 
(defun owl-to-lisp-syntax (ontology)
  (let ((as (to-owl-syntax ontology :functional)))
    (with-input-from-string (s (regex-replace-all "_value" as "_ value"))
      (multiple-value-bind (definitions ontology-iri version-iri ontology-annotations namespaces)
	  (as-parse (as-tokenize s))
	(let ((base (second (assoc "a" namespaces :test 'equal))))
	  ;; I think these are actually errors, but let's fix them
					;(setq base (#"replaceFirst" base "#*$" ""))
	  (when (eql (search "##" base :from-end t) (- (length base) 2))
	    (setq base (subseq base (1- (length base)))))
	  (let ((ontnamevar (make-symbol "ONT")))
	    (values `(with-ontology ,ontnamevar
			 (:base ,(and base (#"replaceFirst" base "#*$" "")) :ontology-iri ,ontology-iri :version-iri ,version-iri :ontology-properties ',ontology-annotations)
			 (,@definitions)
		       ,ontnamevar)
		    base)))))))
 
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

(defun as-parse (tokenized)
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
     (return (parse-ontology (eval-uri-reader-macro (collapse-qnames (cdr tokenized) namespaces)) namespaces))))

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
    (values (list (symbol-name (car args)) (uri-full (fourth args)))
	    (cdr tokenized))))

(defun parse-ontology (tokenized namespaces)
  (let* ((tokens (car tokenized))
	 (ontology-iri (pop tokens))
	 (version-iri (if (uri-p (car tokens)) (pop tokens)))
	 (forms (as-functionize tokens))
	 (ontology-annotations (loop while (and (consp (car forms)) (eq (car forms) 'annotation)) collect (pop forms))))
    (print-db ontology-iri version-iri (subseq forms 0 5) ontology-annotations)
    (values forms ontology-iri version-iri ontology-annotations namespaces)))


;; Change infix to prefix by assuming that keywords (other than the
;; owl keywords like partial etc) are function calls, so the next form
;; are the arguments. Literals other than xsd:string and xsd:int are
;; translated to (literal value-as-string type-uri).

(defun as-functionize (tokenized)
  (and *debug-owl-parse*
       (format t "Enter as-functionize with: ~a~%" tokenized))
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
		      (let ((temp (as-functionize args)))
			(cons (or (gethash (symbol-name f) *owl-string-to-function*)
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
(defun pprint-owl-lisp-syntax (ontology-location name)
  (let ((*print-case* :downcase)
	(*print-right-margin* 150))
    (multiple-value-bind (ont base) (owl-to-lisp-syntax (maybe-url-filename ontology-location) name)
      (let ((*namespace-replacements* (cons (list (or base *default-uri-base*) "") *namespace-replacements*)))
	(pprint ont)
	(decache-uri-abbreviated)
	))))
    
;; pull out rdfs:comments, and put them (as bare strings) after the class/individual/property name
(defun maybe-move-rdfs-comment (args)
  (let ((comment (find-if (lambda(el)(and (consp el) 
					  (eq (first el) 'annotation)
					  (eq (second el) !rdfs:comment)))
			  args)))
    (if comment
	(list* (car args) (third comment) (remove comment (rest args)))
	args)))
