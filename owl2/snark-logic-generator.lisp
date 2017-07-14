(in-package :snark-user)
(use-package :logic)

(defclass snark-logic-generator (logic-generator) ())

(defmethod logical-forall ((g snark-logic-generator) vars expressions) `(forall ,vars ,@expressions))
(defmethod logical-exists ((g snark-logic-generator) vars expressions) `(exists ,vars ,@expressions))
(defmethod logical-implies ((g snark-logic-generator) antecedent consequent) `(implies ,antecedent ,consequent))
(defmethod logical-and ((g snark-logic-generator) expressions) `(and ,@expressions))
(defmethod logical-or ((g snark-logic-generator) expressions) `(or ,@expressions))
(defmethod logical-iff ((g snark-logic-generator) antecedent consequent) `(iff ,antecedent ,consequent))
(defmethod logical-not ((g snark-logic-generator) expression) `(not ,expression))
(defmethod logical-equals ((g snark-logic-generator) a b) `(= ,a ,b))
(defmethod logical-holds ((g snark-logic-generator) &rest args) `(holds ,@args))
