(in-package :logic)

(defclass prover9-logic-generator (logic-generator)
  ((variable-prefix :accessor variable-prefix :initarg :name-prefix :initform "")))

(defmethod normalize-names ((g prover9-logic-generator) e)
  (cond ((and (symbolp e) (char= (char (string e) 0) #\?))
	 (camelcase-uparrow (subseq (string e) 1)))
	((symbolp e) (concatenate 'string (variable-prefix g) (camelcase-uparrow (string e))))
	((and (stringp e) (find #\( e :test 'char=)) e) ;; already done
	((stringp e) (concatenate 'string (variable-prefix g) (camelcase-uparrow e)))
	((uri-p e) (camelcase-uparrow (if (and (boundp '*default-kb*) *default-kb*)
				  (uri-label e)
				  (#"replaceAll" (uri-full e) ".*/" ""))) )
	((atom e) e)
	(t (mapcar (lambda(e) (normalize-names g e)) e))))

(defmethod maybe-render-function-expression ((g prover9-logic-generator) expression)
  (cond ((atom expression) expression)
	(t (format nil "~a(~{~a~^,~})" (car expression)
		   (mapcar (lambda(e) 
			     (maybe-render-function-expression g e ))
			   (cdr expression))))))
  
(defmethod prover-expression ((g prover9-logic-generator) expression)
  (if (consp expression)
      (format nil "~a(~{~a~^,~})" (normalize-names g (car expression)) 
	      (mapcar (lambda(e) (maybe-render-function-expression g e)) (normalize-names g (cdr expression))))
      (normalize-names g expression))
  )

(defmethod prover-quantifier-vars ((g prover9-logic-generator) vars)
  (normalize-names g vars))

(defmethod logical-forall ((g prover9-logic-generator) vars expressions) 
  (format nil "~{~a ~} (~{~a~})"  (mapcar (lambda(e) (format nil "all ~a" e)) (prover-quantifier-vars g vars))
	  (mapcar (lambda(e)(prover-expression g e)) expressions)))

(defmethod logical-exists ((g prover9-logic-generator) vars expressions)
  (format nil "~{~a ~} (~{~a~})"  (mapcar (lambda(e) (format nil "exists ~a" e)) (prover-quantifier-vars g vars))
	  (mapcar (lambda(e)(prover-expression g e)) expressions)))

(defmethod logical-implies ((g prover9-logic-generator) antecedent consequent)
  (format nil "(~a) -> (~a)" (prover-expression g antecedent) (prover-expression g consequent)))

(defmethod logical-and ((g prover9-logic-generator) expressions) 
 (format nil "(~{(~a)~^ & ~})"  (mapcar (lambda(e) (prover-expression g e)) expressions)))

(defmethod logical-or ((g prover9-logic-generator) expressions) 
  (format nil "(~{(~a)~^ | ~})"  (mapcar (lambda(e) (prover-expression g e)) expressions)))

(defmethod logical-iff ((g prover9-logic-generator) antecedent consequent)
  (format nil "(~a) <-> (~a)" (prover-expression g antecedent) (prover-expression g consequent)))

(defmethod logical-not ((g prover9-logic-generator) expression)
  (format nil "-(~a)" (prover-expression g expression)))

(defmethod logical-= ((g prover9-logic-generator) a b)
  (format nil "(~a) = (~a)" (prover-expression g a) (prover-expression g b)))

(defmethod logical-holds ((g prover9-logic-generator) &rest args) 
  (prover-expression g `(holds ,@args)))

(defmethod logical-fact ((g prover9-logic-generator) fact)
  (prover-expression g fact))

(defmethod logical-distinct ((g prover9-logic-generator) &rest args)
  (logical-and g
	 (loop for (a . rest) on args
	       append
	       (loop for b in rest
		     collect (logical-not g (logical-= g a b))))))

(defmethod render-axiom ((g prover9-logic-generator) (a axiom))
  (let ((ax (call-next-method)))
    (concatenate 'string
		 (format nil "~{% ~a~%~}" (and (axiom-description a) (jss::split-at-char (axiom-description a) #\newline)))
		 ax
		 (if (axiom-name a)
		     (format nil " # label(\"~a\") .~%" (axiom-name a))
		     (format nil ".~%")))))


(defmethod render-axioms ((generator prover9-logic-generator) axs)
  (if (stringp axs)
      axs
      (apply 'concatenate 'string (call-next-method))))


