(in-package :logic)

(defvar *expected-proofs* (make-hash-table))
(defvar *proof-notifications* nil)

(defclass expected-proof ()
  ((assumptions :accessor assumptions :initarg :assumptions) ; spec 
   (goal :accessor goal :initarg :goal) ; formula or axiom name
   (options :accessor options :initarg :options) ; options passed to prove function
   (expected-result :accessor expected-result  :initarg :expected-result)  ; :sat :unsat :proved :not-entailed
   (timeout :accessor timeout :initarg :timeout :initform 10) ; in seconds 
   (with :accessor with :initarg :with) ; prove function 
   (name :accessor name :initarg :name :initform "unnamed") ; label for this expected proof
   (counterexample :accessor counterexample :initarg :counterexample :initform nil) ; generate if necessary WIP
   (prover-input :accessor prover-input  :initform nil) ; as rendered text input to prover
   (prover-output :accessor prover-output :initform nil) ; text output of prover
   (prover-model :accessor prover-model  :initform nil) ; if the proof is instead a model build attempt, then the model
   (prover-unsat-explanation :accessor prover-unsat-explanation  :initform nil) ; list of axiom names / formulas - z3 specific proof support
   (proof-support :accessor proof-support) ;; list of axiom names / formulas
   (result :accessor result :initform nil) ;; proof answer: :proved :sat :unsat :timeout ...
   (proof-worker :accessor proof-worker) ;; thread, if running
   ))

(defmethod initialize-instance ((e expected-proof) &rest rest &key &allow-other-keys )
  (call-next-method)
  (assert (member (expected-result e) '(:sat :unsat :proved :not-entailed)) ((expected-result e))
	  "Don't know expected-proof type: ~a" (expected-result e))
  (loop for key in '(:with :timeout :goal :expected-result :assumptions :name :counterexample :excluding :dry-run)
		    do (remf rest key) (remf rest key))
  (setf (options e) rest))

;; ****************************************************************
;; Top level forms for defining things we want proved/checked

(defmacro def-expect-satisfiable (name (&rest options &key (with 'z3-check-satisfiability) (timeout 10) &allow-other-keys) &body assumptions)
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

(defmacro def-expect-unsatisfiable (name (&rest options &key (with 'z3-check-satisfiability) (timeout 10)  &allow-other-keys) &body assumptions)
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

(defmacro def-expect-provable (name (&rest options &key (with 'z3-prove) (timeout 10)  &allow-other-keys) goal &body assumptions)
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

(defmacro def-expect-not-entailed (name (&rest options &key counterexample (with 'z3-check-satisfiability) (timeout 10) &allow-other-keys) goal &body assumptions)
  (let ((name (intern (string name) :keyword)))
    `(setf (gethash ,name *expected-proofs*)
	   (apply 'make-instance 'expected-proof
		  :goal ',goal
		  :assumptions ',assumptions
		  :expected-result :not-entailed
		  :counterexample ,counterexample
		  :name ,name
		  :with ',with
		  :timeout ,timeout
		  ',options))))

;; ****************************************************************
;; return a list of specs suitable for collect-axioms-from-spec. (def-expect-provable & friends take forms that evaluate to specs)
(defmethod proof-assumption-specs ((expected-proof expected-proof))
   (mapcan (lambda(e) (copy-list (if (symbolp e) (list e) (eval e)))) (assumptions expected-proof)))

(defmethod proof-goal-specs ((expected-proof expected-proof))
  (if (goal expected-proof)
      (if (formula-sexp-p (goal expected-proof))
	  (list (goal expected-proof))
	  (if (consp  (goal expected-proof))
	      (goal expected-proof)
	      (list (goal expected-proof))))))

(defmethod proof-counterexample-specs ((expected-proof expected-proof))
  (if (counterexample expected-proof)
      (if (formula-sexp-p (counterexample expected-proof))
	  (list (counterexample expected-proof))
	  (if (consp  (counterexample expected-proof))
	      (counterexample expected-proof)
	      (list (counterexample expected-proof))))))

;; ****************************************************************
;; returns a form that can be evaluated to run the proof

;; To show non-entailment we offer two ways.
;; Either directly try to show that the negation of the goal in combination is satisfiable (therefore not true in all models)
;; Or accept a manually constructed counterexample, verify that's incompatible with the goal, and then show that the
;; counterexample with assumptions are satisfiable

(defmethod proof-form ((expected-proof expected-proof))
  (flet ((assumptions-form (assumptions)
	   (list* 'append (mapcar (lambda(e) (if (or (symbolp e) (and (consp e) (eq (car e) :negate))) `(quote ,(list e)) e)) assumptions))))
    (if (eq (expected-result expected-proof) :not-entailed)
	(if (counterexample expected-proof)
	    (let ((counterexample-sat-form
		    `(,(with expected-proof)
		      ,(assumptions-form (list (counterexample expected-proof)))
		      :timeout ,(timeout expected-proof)
		      :expected-proof ,expected-proof
		      ,@(options expected-proof)))
		  (sat-form
		    `(,(with expected-proof)
		      ,(assumptions-form (list* (counterexample expected-proof) (assumptions expected-proof)))
		      :timeout ,(timeout expected-proof)
		      :expected-proof ,expected-proof
		      ,@(options expected-proof)))
		  (unsat-form
		    `(,(with expected-proof)
		      ,(assumptions-form (list (goal expected-proof) (counterexample expected-proof)))
		      :timeout ,(timeout expected-proof)
		      :expected-proof ,expected-proof
		      ,@(options expected-proof))))
	      `(case ,counterexample-sat-form
		 (:unsat :counterexample-not-sat)
		 (:timeout :timeout)
		 (:sat 
		  (case ,unsat-form
		    (:sat :not-counterexample)
		    (:timeout :timeout)
		    (:unsat
		     (case ,sat-form
		       (:unsat :counterexample-contradicts-assumptions)
		       (:timeout :timeout)
		       (:sat :not-entailed)
		       (otherwise :failure)))
		    (otherwise :failure)))
		 (otherwise :failure)))
	    `(let ((result
		     (,(with expected-proof)
		      ,(assumptions-form (list* `(:negate ,(goal expected-proof)) (assumptions expected-proof)))
		      :timeout ,(timeout expected-proof)
		      :expected-proof ,expected-proof
		      ,@(options expected-proof))))
	       (case result
		 (:timeout :timeout)
		 (:sat :not-entailed)
		 (t result))))
	 `(,(with expected-proof)
	  ,(assumptions-form (assumptions expected-proof))
	  ,@(if (goal expected-proof) (list `(quote ,(list (goal expected-proof)))) nil)
	  :timeout ,(timeout expected-proof)
	  :expected-proof ,expected-proof
	  ,@(options expected-proof)))
    ))

(defmethod proof-form ((expected-proof symbol))
  (assert (gethash expected-proof *expected-proofs*) (expected-proof) "Can't find proof named ~s" expected-proof)
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
	

(defmacro maybe-with-color (color effect format-string &rest args)
  (if (and (find-package 'cl-ansi-text) (not (and (eq color :black) (eq effect :normal))))
      `(,(intern "WITH-COLOR" 'cl-ansi-text) (,color :effect ,effect)
	(format t ,format-string ,@args))
      `(format t ,format-string ,@args)))
      
  
;; ****************************************************************
;; given a name of an expected proof runs it and prints diagnostics

(defun run-proof (name &key print-axiom-names print-formulas print-executed-form timeout queue-notify excluding dry-run print-axiom-count &aux (headline "") (level 3))
  "Run a proof by name"
  (flet ((doit ()
	   (let ((expected-proof (if (typep name 'expected-proof) name (gethash name *expected-proofs*))))
	     (if (typep name 'expected-proof) (setq name (name name)))
	     (assert expected-proof (name) "Didn't find expected proof ~a" name)
	     (when timeout (setf (timeout expected-proof) timeout))
	     (progn
	       (let ((form (proof-form expected-proof)))
		 (format t "~&~a~a..." (ecase (expected-result expected-proof)
					 (:sat "Checking satisfiability in ")
					 (:unsat "Checking unsatisfiability in ")
					 (:proved "Trying proof in ")
					 (:not-entailed "Trying to test non-entailment in "))
			 name)
		 (let ((*print-pretty* t))
		   (when (or print-axiom-names print-formulas print-axiom-count)
		     (format t "~&With:~%")
		     (when (counterexample expected-proof)
		       (maybe-with-color :red :normal "~%counterexample + goal~%")
		       (loop for f in (collect-axioms-from-spec (list (counterexample expected-proof)))
			     do
				(if print-axiom-names (maybe-with-color (if print-formulas :blue :black) :normal  "~a~%" (axiom-name f)))
				(if print-formulas (format t "~a~%" (axiom-sexp f))))
		       (maybe-with-color :red :normal "~%counterexample + assumptions~%"))
		     (let ((the-axioms (append (collect-axioms-from-spec (append (proof-assumption-specs expected-proof) (mapcar (lambda(e) `(:exclude ,e)) excluding)))
					       (or (collect-axioms-from-spec (proof-counterexample-specs expected-proof))
						   (collect-axioms-from-spec (proof-goal-specs expected-proof))))))
		       (when print-axiom-count
			 (format t "~a axioms~%" (length the-axioms))))
		     (loop for f in (collect-axioms-from-spec (append (proof-assumption-specs expected-proof) (mapcar (lambda(e) `(:exclude ,e)) excluding)))
			   do
			      (if print-axiom-names (maybe-with-color (if print-formulas :blue :black) :normal  "~a~%" (axiom-name f)))
			      (if print-formulas (format t "~a~%" (axiom-sexp f))))
		     (let ((last (car (or (collect-axioms-from-spec (proof-counterexample-specs expected-proof)) (collect-axioms-from-spec (proof-goal-specs expected-proof))))))
		       (when last
			 (when print-axiom-names
			   (maybe-with-color (if print-formulas :blue :black) :normal "~a~%"
					     (if (counterexample expected-proof)
						 (if (and (consp last) (not print-formulas)) (axiom-sexp last) (axiom-name last))
						 `(:negated ,(if (and (consp last) (not print-formulas)) (axiom-sexp last) (axiom-name last))))))
			 (when print-formulas
			   (if (counterexample expected-proof)
			       (format t "~a~%" (axiom-sexp last))
			       (format t "~a~%" `(:not ,(axiom-sexp last))))))
		     ))
		   (when print-executed-form 
		     (format t "~&~s~%" form)))
		 (unless dry-run
		   (let ((start  (get-internal-real-time))
			 (result (eval form)))
		     (let ((end (get-internal-real-time)))
		       (if (eq result (expected-result expected-proof))
			   (progn (setq headline "Success!" level 5)
				  (format t "Success!! (result was ~s) (~a seconds)~%" result  (/ (floor (- end start) 100) 10.0)))
			   (progn (setq headline "Failed!" level 1)
				  (format t "Failed!! Expected ~s got ~s. (~a seconds)~%" (expected-result expected-proof) result (floor (/ (- end start) 100) 10.0)))))
		     (eq result (expected-result expected-proof)))))))))
    (if (and queue-notify (not dry-run))
	(threads::make-thread  (lambda() 
				 (let ((*standard-output* (make-string-output-stream)))
				   (doit)
				   (let ((output (get-output-stream-string *standard-output*)))
				     (push (list (get-universal-time) name (format nil "Proof run: ~a" headline) output) *proof-notifications*)
				     (cl-user::prowl-notify (format nil "Proof run: ~a" headline) output :priority level))))
			       :name (if (or (stringp name) (symbolp name)) name (name name)))
	(doit))
    (if (typep name 'expected-proof)  name
    (gethash name *expected-proofs*))))
					   

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


;; ****************************************************************

(defun who-bad (base candidates timeout )
  (assert (eq :sat (z3-check-satisfiability base :timeout timeout) ) ()
	  "Oops - base wasn't even satisfied")
  (catch :found
    (labels ((doit (base candidates timeout vetted) 
	       (loop for candidate in candidates
		     for start = (get-internal-real-time)
		     for result = (z3-check-satisfiability (list* candidate (append vetted base)) :timeout timeout)
		     for end = (get-internal-real-time)
		     do (format t "checked ~a, result:~s in ~a seconds" (axiom-name candidate) result (/ (floor (- end start) 100) 10.0))
		     if  (eq result :timeout)
		       do (throw :found `(:good ,vetted :bad ,candidate)))
	       (doit base (cdr candidates) timeout (car candidates))))
      (doit base candidates timeout nil))))








    
;; ****************************************************************

(defun proof-function-from-key (key)
  (ecase key
    (:prover9 'prover9-prove*)
    (:z3 'z3-prove)
    (:vampire 'vampire-prove)))

(defvar *default-theorem-prover* :vampire)
(defvar *default-theorem-prover-timeout* 30)

(defun prove (assumptions goals &rest args &key (timeout *default-theorem-prover-timeout*)
					     (with *default-theorem-prover*) label)
  (let ((reasoner-options (remf :timeout (remf :with  (remf :label args)))))
    (let ((expected-proof  
	    (make-instance 'logic::expected-proof  :name label 
						 :assumptions (list (list 'quote assumptions))
						   :timeout timeout
						   :options reasoner-options
						   :goal goals
						   :expected-result :proved
						   :with (proof-function-from-key with))))
      (setq @ expected-proof)
      (run-proof expected-proof))))
