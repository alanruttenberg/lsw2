(in-package :logic)

(defvar *snark-symbols* (make-hash-table))

;; make it so snark is only lazy-loaded if you make one of these.

(defclass snark-logic-generator (logic-generator) ())

(defmethod initialize-instance  ((i snark-logic-generator) &rest initargs)
  (require 'snark)
  (loop for term in '("FORALL" "EXISTS" "IMPLIES" "AND" "OR" "IFF" "NOT" "=")
	do (setf (gethash (intern term 'keyword) *snark-symbols*) (intern term 'snark-user)))
  (call-next-method))

(defun ss (sym)
  (gethash sym *snark-symbols*))
    
(defmethod logical-forall ((g snark-logic-generator) vars expressions) `(,(ss :forall) ,vars ,@expressions))
(defmethod logical-exists ((g snark-logic-generator) vars expressions) `(,(ss :exists) ,vars ,@expressions))
(defmethod logical-implies ((g snark-logic-generator) antecedent consequent) `(,(ss :implies) ,antecedent ,consequent))
(defmethod logical-and ((g snark-logic-generator) expressions) `(,(ss :and) ,@expressions))
(defmethod logical-or ((g snark-logic-generator) expressions) `(,(ss :or) ,@expressions))
(defmethod logical-iff ((g snark-logic-generator) antecedent consequent) `(,(ss :iff) ,antecedent ,consequent))
(defmethod logical-not ((g snark-logic-generator) expression) `(,(ss :not) ,expression))
(defmethod logical-equals ((g snark-logic-generator) a b) `(,(ss :=) ,a ,b))
(defmethod logical-holds ((g snark-logic-generator) &rest args) `(holds ,@args))
