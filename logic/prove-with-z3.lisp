(in-package :logic)

;; brew install z3

(defvar *z3-executable* "/usr/local/bin/z3")

(defun does-z3-output-say-sat (output)
  (if (jss::all-matches output "\\bsat\\b") t nil))
  
(defun z3-prove (assumptions goals &optional (timeout 10))
  (does-z3-output-say-sat 
   (run-program-string->string
    *z3-executable* 
    (list  "-in" (format nil "-T:~a" timeout))
    (render-axioms (make-instance 'z3-logic-generator) (append assumptions (mapcar (lambda(e) `(not ,e)) goals))))))

(defun z3-find-model (assumptions goals &optional (timeout 10))
  (run-program-string->string 
   *z3-executable* 
   (list  "-in" (format nil "-T:~a" timeout))
   (concatenate 'string
		(render-axioms (make-instance 'z3-logic-generator)
			       (append 
				(apply 'logic::collect-axioms-from-spec assumptions)
				(mapcar (lambda(e) `(not ,e)) (apply 'logic::collect-axioms-from-spec goals))))
		(format nil "(check-sat)~%")
		(format nil "(get-model)~%")
		)))

(defun z3-check-satisfiability (axioms &optional (timeout 10))
  (does-z3-output-say-sat
   (run-program-string->string
    *z3-executable* 
    (list  "-in" (format nil "-T:~a" timeout))
    (concatenate 'string
		 (render-axioms (make-instance 'z3-logic-generator) (apply 'logic::collect-axioms-from-spec axioms))
		 (format nil "(check-sat)~%")
		 ))
   "\\bsat\\b"))
