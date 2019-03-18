(in-package :logic)

(defclass latex-logic-generator (logic-generator) 
  ((formula-format :accessor formula-format :initarg :formula-format :initform "~a:~%~a")
   (insert-line-breaks :accessor insert-line-breaks :initarg :insert-line-breaks :initform nil)
   (prettify-names :accessor prettify-names :initform t :initarg :prettify-names)
   (with-names :accessor with-names :initform t :initarg :with-names)
   (write-descriptions :accessor write-descriptions :initform nil :initarg :write-descriptions)
   (numbered :accessor numbered :initform nil :initarg :numbered)))

(defmethod normalize-names ((g latex-logic-generator) e)
  (cond ((and (symbolp e) (char= (char (string e) 0) #\?))
	 (camelCase (subseq (string e) 1) nil))
	((symbolp e) (camelCase (string e) nil))
	((and (stringp e) (or (find #\( e :test 'char=) (find #\= e :test 'char=) (find #\\ e :test 'char=))) e) ;; already done
	((stringp e) (camelCase e nil))
	((uri-p e) (camelCase (if (and (boundp '*default-kb*) *default-kb*)
						    (uri-label e)
						    (#"replaceAll" (uri-full e) ".*/" "")) nil) )
	((atom e) e)
	(t (mapcar (lambda(e) (normalize-names g e)) e))))

(defmethod latex-expression ((g latex-logic-generator) expression)
  (if (null expression) (break))
  (if (consp expression)
      (format nil "~a(~{~a~^{,}~})" (normalize-names g (car expression)) (normalize-names g (cdr expression)))
      (normalize-names g expression))
  )

(defmethod maybe-line-break ((g latex-logic-generator))
  (if (insert-line-breaks g) #\newline ""))
  
(defmethod latex-quantifier-vars ((g latex-logic-generator) vars)
  (normalize-names g vars))

(defmethod logical-relation ((g latex-logic-generator) head &rest args)
  (format nil "\\text{~a}(~{~a~^{,}~}\\,)" (normalize-names g head )
	  (mapcar (lambda(e) (let ((sym (normalize-names g e)))
			       (if (logic-var-p e) 
				   (format nil "\\text{{\\it ~a}}" sym)
				   (format nil "\\text{~a}" sym))))
		  args)))
(defmethod logical-forall ((g latex-logic-generator) vars expressions)
  (format nil "\\forall\\, ~{~a~^{,}~}\\, ~{~a~}"  (latex-quantifier-vars g vars)
	  (mapcar (lambda(e)(latex-expression g e)) expressions)))

(defmethod logical-exists ((g latex-logic-generator) vars expressions)
  (format nil "\\exists\\, ~{~a~^{,}~}\\, ~{~a~}"  (latex-quantifier-vars g vars)
	  (mapcar (lambda(e)(latex-expression g e)) expressions)))

(defmethod logical-implies ((g latex-logic-generator) antecedent consequent)
  (format nil "~a ~a\\rightarrow ~a" (latex-expression g antecedent) (maybe-line-break g) (latex-expression g consequent)))

(defmethod logical-and ((g latex-logic-generator) expressions)
  (let ((format-string (if (eql (maybe-line-break g) #\newline)
			   "~{~a ~^~%\\land ~}"
			   "~{~a ~^\\land ~}")))
    (format nil format-string  (mapcar (lambda(e) (latex-expression g e)) expressions))))

(defmethod logical-distinct ((g latex-logic-generator) &rest names)
  (let ((expanded (make-instance 'axiom :sexp (call-next-method) :dont-validate t)))
    (eval `(let ((*logic-generator* ,g))
	     (eval (axiom-generation-form ,expanded))))))


(defmethod logical-or ((g latex-logic-generator) expressions) 
  (format nil "~{~a ~^\\lor ~}"  (mapcar (lambda(e) (latex-expression g e)) expressions)))

(defmethod logical-iff ((g latex-logic-generator) antecedent consequent)
  (format nil "~a ~a\\leftrightarrow ~a" (latex-expression g antecedent) (maybe-line-break g) (latex-expression g consequent)))

(defmethod logical-not ((g latex-logic-generator) expression)
  (format nil "\\neg ~a" (latex-expression g expression)))

(defmethod logical-= ((g latex-logic-generator) a b)
  (format nil "~a = ~a" (latex-expression g a) (latex-expression g b)))

(defmethod logical-holds ((g latex-logic-generator) &rest args) 
  (latex-expression g `(holds ,@args)))

(defmethod logical-fact ((g latex-logic-generator) fact)
  (latex-expression g fact))

(defmethod logical-parens ((g latex-logic-generator) expression)
  (format nil "~a(~a)" (maybe-line-break g) (latex-expression g expression)))

;; ugh this got ugly handling bug  (:not (:exists (?a ?b) ..)) should -> (:not (:exists (?a) (:not (:exists (?b) ...

(defmethod make-explicit-parentheses ((g latex-logic-generator) e &optional parent propagate-negation)
  (cond ((symbolp e) e)
	((not (keywordp (car e))) e)
	;; ((and (member (car e) '(:forall :exists))
	;;       (> (length (second e)) 1)
	;;       (make-explicit-parentheses g `(,(car e) (,(car (second e)))
	;; 				     ,(if propagate-negation
	;; 					  `(:not (,(car e) ,(rest (second e)) ,@(cddr e)))
	;; 					  `(,(car e) ,(rest (second e)) ,@(cddr e))))
	;; 				 (car e) propagate-negation)
	;; 				 ))
	((member (car e) '(:forall :exists))
	 (if (and (keywordp (car (third e)))  (not (eq (car e) parent)))
	     `(,(car e) ,(second e) (:parens ,(make-explicit-parentheses g  (third e) parent nil)))
	     `(,(car e) ,(second e) ,(make-explicit-parentheses g  (third e) parent nil))))
	((keywordp (car e))
	 (let ((form `(,(car e)
		       ,@(mapcar (lambda(e2) 
				   (if (and (consp e2) (keywordp (car e2)))
				       (if (or #|(eq (car e) (car e2))|# (lower-precedence-p (car e) (car e2)))
					   (make-explicit-parentheses g e2 (if (eq (car e) :not) parent :not)  (eq (car e) :not))
					   `(:parens ,(make-explicit-parentheses g e2 (if (eq (car e) :not) parent :not) (or propagate-negation (eq (car e) :not)))))
				       (make-explicit-parentheses g e2 (if (eq (car e) :not) parent :not)  (or propagate-negation (eq (car e) :not)))))
				 (rest e)))))
	   (if (and (not (eq :not (car e))) (member parent '(:forall :exists))) `(:parens ,form) form)))
	(t e)))

;; https://en.wikipedia.org/wiki/Logical_connective#Order_of_precedence
(defun lower-precedence-p (a b)
  (> (position a '(:distinct :fact :forall :exists :not :and :or :implies :iff :=))
     (position b '(:distinct :fact :forall :exists :not :and :or  :implies :iff :=))))


(defun quote-for-latex (string)
  (#"replaceAll" string "([&_])" "\\\\$1"))

(defmethod render-axiom ((g latex-logic-generator) (a axiom))
  (let ((*logic-generator* g))
    (let ((name (string (axiom-name a))))
      (if (prettify-names g)
	  (progn
	    (setq name  (string-downcase (#"replaceAll" (string (axiom-name a)) "-" " ")))
	    (setf (char name 0) (char-upcase (char name 0))))
	  (setq name (format nil ":~a" (string-downcase name))))
      (concatenate 'string
		   (if (and (write-descriptions g) (stringp (axiom-description a)) (not (equal (axiom-description a) "")))
		       (quote-for-latex (axiom-description a))
		       "")

      (if (with-names g)
	  (format nil (formula-format g) name
	      (eval (rewrite-to-axiom-generation-form (make-explicit-parentheses g (axiom-sexp a)))))
	  (format nil (formula-format g) 
	      (eval (rewrite-to-axiom-generation-form (make-explicit-parentheses g (axiom-sexp a))))))))))

(defmethod render-axioms ((generator latex-logic-generator) axs)
  (let ((count 0))
    (if (stringp axs)
	axs
	(format nil "~{~a~^~%~}" 
		(mapcar (lambda(e)
			  (concatenate 'string
				       (if (numbered generator) (format nil "~a. " (incf count))
					   "")
				       (render-axiom generator e))) axs))
		)))


;; even using breqn there is trouble with long parenthesized expressions
;; one strategy is that if there are more than 3 existentials, put them on a separate line with \\ and remove the surrounding parentheses.
;; might try the "." instead of parentheses for quantifiers

(defun axiom-sexp-length (sexp)
  (cond ((atom sexp) (length (string sexp)))
	((member (car sexp) '(:forall :exists :and :or :not :implies :iff))
	 (+ 2 (apply '+ (mapcar 'axiom-sexp-length (rest sexp)))))
	(t (+ (axiom-sexp-length (car sexp)) (apply '+ (mapcar 'axiom-sexp-length (rest sexp)))))))
	 
  
