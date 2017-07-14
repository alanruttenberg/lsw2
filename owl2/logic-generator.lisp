(defpackage :logic (:use cl))

(in-package :logic)
(export '(l-forall l-exists l-and l-or l-iff l-equal l-implies
	  logical-forall logical-exists logical-and logical-or logical-iff
	  logical-equal logical-implies pred-property pred-class *use-holds*
	  logical-holds logic-generator logical-class logical-relation)
	:logic)

;(loop for s in '(l-forall l-exists l-and l-or l-iff l-equal l-implies logical-forall logical-exists logical-and logical-or logical-iff logical-equal logical-implies pred-property pred-class pred-property pred-class *use-holds* logic-generator logical-holds) do (shadowing-import s 'cl-user))
	      
(defclass logic-generator () ())

(defgeneric logical-forall ((g logic-generator) vars expressions))
(defgeneric logical-exists ((g logic-generator) vars expressions))
(defgeneric logical-implies ((g logic-generator) antecedent consequent))
(defgeneric logical-and ((g logic-generator) expressions))
(defgeneric logical-or ((g logic-generator) expressions))
(defgeneric logical-iff ((g logic-generator) antecedent consequent))
(defgeneric logical-not ((g logic-generator) expression))
(defgeneric logical-equal ((g logic-generator) a b))
(defgeneric logical-holds ((g logic-generator) &rest args))
(defgeneric logical-class ((g logic-generator) class el))
(defgeneric logical-relation ((g logic-generator) head &rest args))

(defvar *use-holds* nil)

(defmethod logical-relation ((g logic-generator) head &rest args)
    (if *use-holds*
	(apply 'logical-holds g head args)
	`(,head ,@args)))

(defmethod logical-class ((g logic-generator) class el)
  (if *use-holds*
      (logical-holds g class el)
      `(,class ,el)))

(defclass debug-logic-generator (logic-generator) ())

(defmethod logical-forall ((g debug-logic-generator) vars expressions) `(:forall ,vars ,@expressions))
(defmethod logical-exists ((g debug-logic-generator) vars expressions) `(:exists ,vars ,@expressions))
(defmethod logical-implies ((g debug-logic-generator) antecedent consequent) `(:implies ,antecedent ,consequent))
(defmethod logical-and ((g debug-logic-generator) expressions) `(:and ,@expressions))
(defmethod logical-or ((g debug-logic-generator) expressions) `(:or ,@expressions))
(defmethod logical-iff ((g debug-logic-generator) antecedent consequent) `(:iff ,antecedent ,consequent))
(defmethod logical-not ((g debug-logic-generator) expression) `(:not ,expression))
(defmethod logical-equal ((g debug-logic-generator) a b) `(:= ,a ,b))
(defmethod logical-holds ((g debug-logic-generator) &rest args) `(:holds ,@args))

(defvar *logic-generator* (make-instance 'debug-logic-generator))

(defparameter *logic-symbols* "PQRSUVWXYZABCDEFGHIJKLMNOT")

(defmacro with-logic-vars ((vars n &optional (from *logic-symbols* from-supplied-p)) &body body)
  `(let ((,vars (loop for i below  ,n
		      for base = (elt ,(if from-supplied-p 'from '*logic-symbols*) i)
		      collect (intern (concatenate 'string "?" (string base))
				      (if (symbolp base) (symbol-package base) *package*))
		      )))
     (let ((*logic-symbols* (if ,from-supplied-p *logic-symbols* (subseq *logic-symbols* ,n))))
       ,@body)))

(defmacro with-logic-var (var &body body)
  (let ((vars (gensym)))
    `(with-logic-vars (,vars 1)
       (let ((,var (car ,vars)))
	 ,@body))))

(defun pred-property (head &rest args)
  (apply 'logical-relation *logic-generator* head args))

(defun pred-class (class arg)
  (apply 'logical-class *logic-generator*  class arg))

(defun l-forall (vars &rest expressions)
  (logical-forall *logic-generator* vars expressions))

(defun l-exists (vars &rest expressions)
  (logical-exists *logic-generator* vars expressions))

(defun l-implies (antecedent consequent)
  (logical-implies *logic-generator* antecedent consequent))

(defun l-and (&rest expressions)
  (logical-and *logic-generator* expressions))

(defun l-or (&rest expressions)
  (logical-or *logic-generator* expressions))

(defun l-iff (antecedent consequent)
  (logical-iff *logic-generator* antecedent consequent))

(defun l-not (expression)
  (logical-not *logic-generator* expression))

(defun l-equal (a b)
  (logical-equals *logic-generator* a b))
