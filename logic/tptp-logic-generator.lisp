(in-package :logic)

(defclass tptp-logic-generator (logic-generator) ())

(defmethod render-axiom ((generator tptp-logic-generator) expression)
  (let* ((prover9 (prepare-prover9-input (list expression) nil))
	 (translated (run-program-string->string
		      (prover-binary "ladr_to_tptp")
		      '("-q")
		      prover9)))
    translated ))

(defmethod with-names ((g tptp-logic-generator))
  nil)

(defmethod render-axioms ((generator tptp-logic-generator) axs)
  (if (stringp axs)
      axs
      (let ((format-string (if (with-names generator)  "~{~a~^~%~%~}"  "~{~a~^~%~}")))
	(format nil format-string (mapcar (lambda(e) (render-axiom generator e)) axs))
	)))




