(in-package :logic)




(defclass latex-logic-generator (logic-generator) ())

(defmethod normalize-names ((g latex-logic-generator) e)
  (cond ((and (symbolp e) (char= (char (string e) 0) #\?))
	 (cl-user::camelCase (subseq (string e) 1) nil))
	((symbolp e) (cl-user::camelCase (string e) nil))
	((and (stringp e) (or (find #\( e :test 'char=) (find #\= e :test 'char=) (find #\\ e :test 'char=))) e) ;; already done
	((stringp e) (cl-user::camelCase e nil))
	((cl-user::uri-p e) (cl-user::camelCase (if (and (boundp 'cl-user::*default-kb*) cl-user::*default-kb*)
						    (cl-user::uri-label e)
						    (#"replaceAll" (cl-user::uri-full e) ".*/" "")) nil) )
	((atom e) e)
	(t (mapcar (lambda(e) (normalize-names g e)) e))))

(defmethod latex-expression ((g latex-logic-generator) expression)
  (if (null expression) (break))
  (if (consp expression)
      (format nil "~a(~{~a~^,~})" (normalize-names g (car expression)) (normalize-names g (cdr expression)))
      (normalize-names g expression))
  )

(defmethod latex-quantifier-vars ((g latex-logic-generator) vars)
  (normalize-names g vars))

(defmethod logical-forall ((g latex-logic-generator) vars expressions)
  (format nil "~{\\forall ~a\\, ~} ~{~a~}"  (latex-quantifier-vars g vars)
	  (mapcar (lambda(e)(latex-expression g e)) expressions)))

(defmethod logical-exists ((g latex-logic-generator) vars expressions)
  (format nil "~{\\exists ~a\\, ~} ~{~a~}"  (latex-quantifier-vars g vars)
	  (mapcar (lambda(e)(latex-expression g e)) expressions)))

(defmethod logical-implies ((g latex-logic-generator) antecedent consequent)
  (format nil "~a \\rightarrow ~a" (latex-expression g antecedent) (latex-expression g consequent)))

(defmethod logical-and ((g latex-logic-generator) expressions) 
 (format nil "~{~a ~^\\land ~}"  (mapcar (lambda(e) (latex-expression g e)) expressions)))

(defmethod logical-or ((g latex-logic-generator) expressions) 
  (format nil "~{~a ~^\\lor ~}"  (mapcar (lambda(e) (latex-expression g e)) expressions)))

(defmethod logical-iff ((g latex-logic-generator) antecedent consequent)
  (format nil "~a \\leftrightarrow ~a" (latex-expression g antecedent) (latex-expression g consequent)))

(defmethod logical-not ((g latex-logic-generator) expression)
  (format nil "\\neg ~a" (latex-expression g expression)))

(defmethod logical-= ((g latex-logic-generator) a b)
  (format nil "~a = ~a" (latex-expression g a) (latex-expression g b)))

(defmethod logical-holds ((g latex-logic-generator) &rest args) 
  (latex-expression g `(holds ,@args)))

(defmethod logical-fact ((g latex-logic-generator) fact)
  (latex-expression g fact))

(defmethod logical-parens ((g latex-logic-generator) expression)
  (format nil "(~a)" (latex-expression g expression)))

(defmethod make-explicit-parentheses ((g latex-logic-generator) e)
  (cond ((symbolp e) e)
	((not (keywordp (car e))) e)
	((and (member (car e) '(:forall :exists))
	      (> (length (second e)) 1)
	      (make-explicit-parentheses g `(,(car e) (,(car (second e))) (,(car e) ,(rest (second e)) ,@(cddr e))))))
	((member (car e) '(:forall :exists))
	 (if (and (keywordp (car (third e)))  (not (eq (car (third e)) (car e))))
	     `(,(car e) ,(second e) (:parens ,(make-explicit-parentheses g  (third e))))
	     `(,(car e) ,(second e) ,(make-explicit-parentheses g  (third e)))))
	((keywordp (car e))
	 `(,(car e)
	   ,@(mapcar (lambda(e2) 
		       (if (and (consp e2) (keywordp (car e2)))
			   (if (or #|(eq (car e) (car e2))|# (lower-precedence-p (car e) (car e2)))
					(make-explicit-parentheses g e2)
					`(:parens ,(make-explicit-parentheses g e2)))
				    (make-explicit-parentheses g e2)))
		     (rest e))))
	(t e)))

;; https://en.wikipedia.org/wiki/Logical_connective#Order_of_precedence
(defun lower-precedence-p (a b)
  (> (position a '(:forall :exists:not :and :or :implies :iff :=))
     (position b '(:forall :exists :not :and :or  :implies :iff :=))))


(defmethod render-axiom ((g latex-logic-generator) (a axiom))
  (let ((*logic-generator* g))
    (eval (rewrite-to-axiom-generation-form (make-explicit-parentheses g (axiom-sexp a))))))
