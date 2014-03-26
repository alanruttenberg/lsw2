(in-package :cl-user)

;; This file translates an ontology in owl 2 functional syntax into a version suitable for 
;; being the input to the rdf mapping tool. It's adapted from the lsw owl rdf/xml parser that used the owlapi.
;; This is, for the most part, a hack for a parser, but well, it works well enough.

(defparameter *fs-functions*
  (loop for s in 
       '(ontology declaration objectinverseof
	 dataintersectionof dataunionof datacomplementof dataoneof
	 objectintersectionof objectunionof objectcomplementof objectoneof
	 objectsomevaluesfrom objectallvaluesfrom objecthasvalue objecthasself
	 objectmincardinality objectmaxcardinality objectexactcardinality
	 datasomevaluesfrom dataallvaluesfrom datahasvalue datamincardinality
	 datamaxcardinality dataexactcardinality subclassof equivalentclasses
	 disjointclasses disjointunion subobjectpropertyof
	 equivalentobjectproperties disjointobjectproperties
	 objectpropertydomain objectpropertyrange inverseobjectproperties
	 functionalobjectproperty inversefunctionalobjectproperty
	 reflexiveobjectproperty irreflexiveobjectproperty
	 symmetricobjectproperty asymmetricobjectproperty
	 transitiveobjectproperty subdatapropertyof equivalentdataproperties
	 disjointdataproperties datapropertydomain datapropertyrange
	 functionaldataproperty datatypedefinition sameindividual
	 differentindividuals classassertion objectpropertyassertion
	 negativeobjectpropertyassertion datapropertyassertion
	 negativedatapropertyassertion annotationassertion
	 subannotationpropertyof annotationpropertydomain
	 annotationpropertyrange datatyperestriction haskey annotation
	 datatype class objectproperty dataproperty annotationproperty namedindividual
	 imports objectpropertychain
	  )
       with table = (make-hash-table :test 'equalp)
       do (setf (gethash (symbol-name s) table) s)
       finally (return table)
       ))

(defparameter *owl2-fs-spec-namespaces*
  `(("owl" ,(uri-full !owl:))
    ("xsd" ,(uri-full !xsd:))
    ("rdf" ,(uri-full !rdf:))
    ("rdfs" ,(uri-full !rdfs:))))

(defparameter *fs-readtable* (copy-readtable))

(defvar *debug-owl-parse* nil)

;; make case sensitive
(setf (readtable-case *fs-readtable*) :preserve)

;; reuse the uri reader macro. It understands what to do when the first character is 
;; a < rather than a !
(set-macro-character  #\< 'read-uri nil  *fs-readtable*)

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

(set-macro-character #\: 'return-colon nil *fs-readtable*)
(set-macro-character #\= 'return-equal nil *fs-readtable*)
(set-macro-character #\^ 'return-carets nil *fs-readtable*)


;; Tokenize by calling read using our custom readtable until we run out of "forms".
;; All symbols are interned as keywords.
(defun fs-tokenize (stream &optional (eof-marker :eof))
  (if (stringp stream) (setq stream (make-string-input-stream stream)))
  (let ((*readtable* *fs-readtable*)
	(*package* (load-time-value (find-package :keyword))))
    (loop for form = (read stream nil eof-marker)
	 until (eq form :eof)
	 collect form)))

;; do the work. 
(defun parse-functional-syntax (string)
  (with-input-from-string (s string)
    (multiple-value-bind (ontology name namespaces)
	(fs-parse (fs-tokenize s))
      (if (and (consp (second ontology)) (not (eq (car (second ontology)) :blank)))
	  (setq ontology `(ontology (:blank :ontology) ,@(cdr ontology))))
      (values ontology namespaces)
      )))
 
;; since : are separate tokens, we need to reassemble the qnames,
;; which are 3 tokens, into a single uri. That's what this function
;; does. Also have to handle the bare "http:" in the namespace
;; definitions (must these always be http?)

(defun fs-collapse-qnames (tokenized namespaces)
  (labels ((doit (tok)
;	     (format t "Looking at: ~a ~a ~a~%~%" (first tok) (second tok) (third tok))
	     (cond ((atom tok) tok)
		   ((consp tok) 
		    (loop 
		       while tok
		       for (this that) = tok
		       collect (cond ((eq this *colon*)
				      (let ((default (second (assoc "" namespaces :test 'equalp))))
					(assert default () (format t "No default prefix for ~a" tok))
					(prog1
					    (make-uri (format nil "~a~a" default (second tok)))
					  (setq tok (cddr tok)))))
				     ((and (eq that *colon*) (keywordp this))
				      (let ((prefix (or (second (assoc (string this) namespaces :test 'equalp))
							(let ((found (second (assoc (string this) *owl2-fs-spec-namespaces* :test 'equalp))))
							    (when found
							      (format *debug-io* "Warning: Missing prefix ~a in ~a:~a~%" (string this)  (string this) (third tok))
							      found))
							(and (equal (string this) "_")
							     :blank))
							))
					(assert prefix () (format t "No prefix for ~a:" this))
					(prog1
					    (if (eq prefix :blank)
						`(:blank ,(third tok))
						(make-uri (format nil "~a~a" prefix (third tok))))
					  (setq tok (cdddr tok)))))
				     ((consp this)
				      (setq tok (cdr tok))
				      (doit this))
				     (t (setq tok (cdr tok))
					this)))))))
    (doit tokenized)))

;; bind around call to parsing to capture parsed prefixes
(defvar *parsed-prefixes*)

;; Read the namespace declarations, then call parse-ontology, after
;; converting qnames to uri's using these namespaces.

(defun fs-parse (tokenized)
  (setq tokenized (eval-uri-reader-macro tokenized))
  (loop for top = (car tokenized)
     with namespaces
     while (eq top :|Prefix|)
     do  
     (multiple-value-bind (namespace rest) (fs-parse-namespace (cdr tokenized))
       (setq tokenized rest)
       (push namespace namespaces))
     finally 
       (unless (eq top :|Ontology|)
	 (error "Expecting Ontology statement but got : ~a" top))
       (when (boundp '*parsed-prefixes*)
	   (setq *parsed-prefixes* (append namespaces *parsed-prefixes*)))
       (return (parse-fs-ontology (eval-uri-reader-macro (fs-collapse-qnames tokenized namespaces)) namespaces))))

(defun fs-parse-namespace (tokenized)
  (let ((args (car tokenized)))
    (assert (or (and
		 (eq (second args) *colon*)
		 (eq (third args) *equals*)
		 (uri-p (fourth args)))
		(and
		 (eq (first args) *colon*)
		 (eq (second args) *equals*)
		 (uri-p (third args))))
	    ()
	    "Malformed namespace declaration: ~a" args)
    (values
     (if (eq (first args) *colon*)
	 (list "" (uri-full (third args)))
	 (list (symbol-name (first args)) (uri-full (fourth args))))
     (cdr tokenized))))

(defun parse-fs-ontology (tokenized namespaces)
  (values (car (fs-functionize tokenized)) (second tokenized) namespaces))

;; Change infix to prefix by assuming that keywords (other than the
;; owl keywords like partial etc) are function calls, so the next form
;; are the arguments. Literals other than xsd:string and xsd:int are
;; translated to (literal value-as-string type-uri).

(defun fs-functionize (tokenized)
  (and *debug-owl-parse*
       (format t "Enter fs-functionize with: ~a~%" tokenized))
  (if (atom tokenized) 
      tokenized
      (loop for (f args) = tokenized  
	 while tokenized
	 do (and *debug-owl-parse* (format t "At loop start f: ~a args: ~a~%" f args))
	 collect
	 (cond
	   ( ;; leave uris alone
	    (uri-p f)
	    (prog1 f (setf tokenized (cdr tokenized))))
	   ( ;; if we're not a symbol, then we're a literal, or we should be left alone
	    (not (symbolp f))
	    (if (eq (second tokenized) :^^)
		(prog1
		    `(:literal ,f ,(third tokenized))
		  (setf tokenized (cdddr tokenized)))
		(progn
		  (setf tokenized (cdr tokenized))
		  f)))
	   ((gethash  (symbol-name f) *fs-functions*)
	    (setf tokenized (cddr tokenized))
	    (cons (gethash (string-downcase (symbol-name f)) *fs-functions*)
		  (fs-functionize args)))
	   ((null f) (setq tokenized (cdr tokenized)) f)
	   (t (format t "wtf: ~a~%" f) (setq tokenized (cdr tokenized)) f)))))

