(in-package :logic)

(defvar *expected-proofs* (make-hash-table))

(defclass expected-proof ()
  ((assumptions :accessor assumptions :initarg :assumptions)
   (goal :accessor goal :initarg :goal)
   (options :accessor options :initarg :options)
   (expected-result :accessor expected-result  :initarg :expected-result)
   (timeout :accessor timeout :initarg :timeout)
   (with :accessor with :initarg :with)))

(defmethod initialize-instance ((e expected-proof) &rest rest &key &allow-other-keys )
  (call-next-method)
  (loop for key in '(:with :timeout :goal :expected-result :assumptions)
		    do (remf rest key) (remf rest key))
  (setf (options e) rest))

(defmacro def-expect-satisfiable (name (&rest options &key (with 'z3-check-satisfiability) (timeout 10))  &body assumptions)
  `;(let ((options ',keys))
    ; (when (not (getf options :with)) (setq options `(:with ,',with ,@options)))
     ;(when (not (getf options :timeout)) (setq options `(:timeout ,'],timeout ,@options)))
  (setf (gethash ,(intern (string name) :keyword) *expected-proofs*)
	(apply 'make-instance 'expected-proof :goal nil :assumptions ',assumptions :expected-result :sat
	       :with ',with :timeout ,timeout ',options)))

(defmacro def-expect-unsatisfiable (name (&rest options &key (with 'z3-check-satisfiability) (timeout 10)) &body assumptions)
  `(setf (gethash ,(intern (string name) :keyword) *expected-proofs*)
	 (apply 'make-instance 'expected-proof :goal nil :assumptions ',assumptions :expected-result :unsat
					      :with ',with :timeout ,timeout ',options)))


(defmacro def-expect-provable (name (&rest options &key (with 'z3-prove) (timeout 10)) goal &body assumptions)
  `(setf (gethash ,(intern (string name) :keyword) *expected-proofs*)
	 (apply 'make-instance 'expected-proof :goal ',goal :assumptions ',assumptions :expected-result :proved
					      :with ',with :timeout ,timeout ',options)))

(defun run-proof (name)
  (let ((expected-proof (gethash name *expected-proofs*)))
    (assert expected-proof (name) "Didn't find expected proof ~a" name)
    (progn
      (let ((form `(,(with expected-proof)
		    (append ,@(mapcar (lambda(e) (if (symbolp e) `(quote ,(list e)) e)) (assumptions expected-proof)))
		    ,@(if (goal expected-proof) (list `(quote ,(list (goal expected-proof)))) nil)
		    :timeout ,(timeout expected-proof)
		    ,@(options expected-proof))))
	(format t "~%~a~a~%" (ecase (expected-result expected-proof)
			     (:sat "Checking satisfiability in ")
			     (:unsat "Checking unsatisfiability in ")
			     (:proved "Trying proof in "))
		name)
	(let ((*print-pretty* t))
	  (format t "~s" form))
	(let ((result (eval form)))
	  (if (eq result (expected-result expected-proof))
	      (format t "~%Success!! (result was ~s)~%" result)
	      (format t "~%Failed!! Expected ~s got ~s.~%" (expected-result expected-proof) result))
	  (eq result (expected-result expected-proof)))))))


