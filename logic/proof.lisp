(in-package :logic)

(defvar *expected-proofs* (make-hash-table))

(defclass expected-proof ()
  ((assumptions :accessor assumptions :initarg :assumptions)
   (goal :accessor goal :initarg :goal)
   (options :accessor options :initarg :options)
   (expected-result :accessor expected-result  :initarg :expected-result)
   (timeout :accessor timeout :initarg :timeout)
   (with :accessor with :initarg :with)
   (name :accessor name :initarg :name)))

(defmethod initialize-instance ((e expected-proof) &rest rest &key &allow-other-keys )
  (call-next-method)
  (loop for key in '(:with :timeout :goal :expected-result :assumptions :name)
		    do (remf rest key) (remf rest key))
  (setf (options e) rest))

;; ****************************************************************
;; Top level forms for defining things we want proved/checked

(defmacro def-expect-satisfiable (name (&rest options &key (with 'z3-check-satisfiability) (timeout 10)) &body assumptions)
  (let ((name (intern (string name) :keyword)))
    `(setf (gethash ,name *expected-proofs*)
	   (apply 'make-instance 'expected-proof 
		  :goal nil
		  :assumptions ',assumptions
		  :expected-result :sat
		  :with ',with
		  :timeout ,timeout
		  :name ,name
		  ',options ))))

(defmacro def-expect-unsatisfiable (name (&rest options &key (with 'z3-check-satisfiability) (timeout 10)) &body assumptions)
  (let ((name (intern (string name) :keyword)))
    `(setf (gethash ,name *expected-proofs*)
	   (apply 'make-instance 'expected-proof
		  :goal nil
		  :assumptions ',assumptions
		  :expected-result :unsat
		  :with ',with
		  :timeout ,timeout
		  :name ,name
		  ',options))))

(defmacro def-expect-provable (name (&rest options &key (with 'z3-prove) (timeout 10)) goal &body assumptions)
  (let ((name (intern (string name) :keyword)))
    `(setf (gethash ,name *expected-proofs*)
	   (apply 'make-instance 'expected-proof
		  :goal ',goal
		  :assumptions ',assumptions
		  :expected-result :proved
		  :name ,name
		  :with ',with
		  :timeout ,timeout
		  ',options))))

;; ****************************************************************
;; return a list of specs suitable for collect-axioms-from-spec. (def-expect-provable & friends take forms that evaluate to specs)
(defmethod proof-assumption-specs ((expected-proof expected-proof))
  (mapcan (lambda(e) (copy-list (if (symbolp e) (list e) (eval e)))) (assumptions expected-proof)))

(defmethod proof-goal-specs ((expected-proof expected-proof))
  (and (goal expected-proof) (list (goal expected-proof))))

;; ****************************************************************
;; returns a form that can be evaluated to run the proof
(defmethod proof-form ((expected-proof expected-proof))
  `(,(with expected-proof)
    (append ,@(mapcar (lambda(e) (if (symbolp e) `(quote ,(list e)) e)) (assumptions expected-proof)))
    ,@(if (goal expected-proof) (list `(quote ,(list (goal expected-proof)))) nil)
    :timeout ,(timeout expected-proof)
    ,@(options expected-proof)))

(defmethod proof-form ((expected-proof symbol))
  (proof-form (gethash expected-proof *expected-proofs*)))

;; ****************************************************************
;; formats the assumptions and negated goal (if present) in the logical syntax for the prover named by argument which
(defmethod render-proof ((which symbol) (expected-proof expected-proof))
  (render which (mapcan (lambda(e) (copy-list (if (symbolp e) (list e) (eval e)))) (assumptions expected-proof))
	  (and (goal expected-proof) (list (goal expected-proof)))
	  ))

(defmethod render-proof ((which symbol) (expected-proof symbol))
  (render-proof which (gethash expected-proof *expected-proofs*)))  

;; ****************************************************************
;; e.g. (prove-with :binary-parthood-domain-range :vampire)
(defmethod prove-with ((expected-proof expected-proof) which &key timeout (with-goals t)) 
  (let ((proof-function (if (keywordp which)
			    (ecase which
			      (:prover9 'prover9-proof)
			      (:z3 'z3-prove)
			      (:vampire 'vampire-prove)
			      (:mace4 'mace4-find-model))
			    which)))
    (if with-goals
	(funcall proof-function (proof-assumption-specs expected-proof) (proof-goal-specs expected-proof) :timeout (or timeout (timeout expected-proof)))
	(funcall proof-function (proof-assumption-specs expected-proof) :timeout (or timeout (timeout expected-proof))))))

(defmethod prove-with ((expected-proof symbol) which &key timeout (with-goals t))
  (prove-with (gethash expected-proof *expected-proofs*)
	      (ecase which
		(:prover9 'prover9-proof)
		(:z3 'z3-prove)
		(:vampire 'vampire-prove)
		(:mace4 'mace4-find-model))
	      :timeout  timeout :with-goals with-goals))
	
;; ****************************************************************
;; given a name of an expected proof runs it and prints diagnostics
(defun run-proof (name)
  "Run a proof by name"
  (let ((expected-proof (gethash name *expected-proofs*)))
    (assert expected-proof (name) "Didn't find expected proof ~a" name)
    (progn
      (let ((form (proof-form expected-proof)))
	(format t "~%~a~a~%" (ecase (expected-result expected-proof)
			     (:sat "Checking satisfiability in ")
			     (:unsat "Checking unsatisfiability in ")
			     (:proved "Trying proof in "))
		name)
	(let ((*print-pretty* t))
	  (format t "~a" form))
	(let ((result (eval form)))
	  (if (eq result (expected-result expected-proof))
	      (format t "~%Success!! (result was ~s)~%" result)
	      (format t "~%Failed!! Expected ~s got ~s.~%" (expected-result expected-proof) result))
	  (eq result (expected-result expected-proof)))))))

;; ****************************************************************
;; Runs all the proofs with prover9 eor mace4, just cause I was curious.

;; Today, all prover9 attempts worked except :binary-parthood-domain-range which timed out. None of the attempts to use
;; mace4 to determine satisfiablity succeeded.

(defun check-with-prover9-mace4()
  "Run through all the proofs and try them with prover9 (:unsat :proved) or mace4 (:sat)"
  (maphash (lambda(k v) 
	   (when (member (expected-result v) '(:proved :unsat))
	     (format t "~a (:~a) ~a~%" k (expected-result v) (prove-with k 'prover9-prove))))
	 *expected-proofs*)
  (maphash (lambda(k v) 
	     (when (member (expected-result v) '(:sat))
	       (format t "~a (:~a) ~a~%" k (expected-result v) (prove-with k 'mace4-find-model :with-goals nil))))
	   *expected-proofs*))
