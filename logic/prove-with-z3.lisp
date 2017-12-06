(in-package :logic)

;; brew install z3

(defvar *z3-executable* "/usr/local/bin/z3")
(defvar *last-z3-output* nil)
(defvar *last-z3-input* nil)

(defun does-z3-output-say-sat (output)
  (if (jss::all-matches output "\\bsat\\b") t nil))

(defun does-z3-output-say-unsat (output)
  (if (jss::all-matches output "\\bunsat\\b") t nil))

(defun does-z3-output-say-timeout (output)
  (if (jss::all-matches output "timeout\\b") t nil))

(defun z3-output-errors (output)
  (jss::all-matches output "(?s)\\(error .*" 0))

(defun run-z3 (input timeout)
  (setq *last-z3-output* (run-program-string->string
   *z3-executable* 
   (list  "-in" (format nil "-T:~a" timeout))
   (setq *last-z3-input* input)
   )))

(defun z3-syntax-check (assumptions &optional goals (errorp t))
  (let ((answer 
	  (if (stringp assumptions)
	      (run-z3 assumptions 10)
	      (run-z3
	       (z3-render (collect-axioms-from-spec assumptions)
			  (mapcar (lambda(e) (negate-axiom e)) (collect-axioms-from-spec goals)) nil)
	       10))))
    (when (search "error" answer)
         (princ (if (stringp assumptions) assumptions
		    (z3-render assumptions
			       goals))))
    (if (z3-output-errors answer)
	(if errorp
	    (error "Z3 syntax error:~a~%"  (z3-output-errors answer))
	    (z3-output-errors answer))
	t)))
  
(defun z3-render (assumptions &optional goals commands)
  (apply 'concatenate 'string
	 (if (stringp assumptions) assumptions (render :z3 assumptions goals))
	 (mapcar (lambda(e) (format nil "~a" e)) commands)))
  
(defun z3-prove (assumptions goals &key (timeout 30) (return-contradiction nil) expected-proof)
  (z3-syntax-check assumptions goals)
  (let* ((input (z3-render assumptions goals '("(check-sat)")))
	 (answer 
	  (setq *last-z3-output* 
	  (run-z3 (setq *last-z3-input* input) timeout)
	  )))
    (when expected-proof
      (setf  (prover-output expected-proof) (setq *last-z3-output* answer))
      (setf (prover-input expected-proof) input))
    (let ((result 
	    (or (z3-output-errors answer)
		(if (does-z3-output-say-timeout answer)
		    :timeout
		    (if (does-z3-output-say-unsat answer)
			:proved
			(if (does-z3-output-say-sat answer)
			    :disproved))))))
      (if (and (eq result :disproved) return-contradiction)
	  (let ((model (z3-find-model (append assumptions goals) :timeout timeout :expected-proof expected-proof)))
	    (when expected-proof
	      (setf (prover-model expected-proof) nil)
	      (setf (prover-unsat-explanation expected-proof) model))
	    (values result  model))
	  (if expected-proof
	      (setf (result expected-proof) result)
	      result)))))


(defun z3-find-model (assumptions &key (timeout 10) expected-proof)
  (z3-syntax-check assumptions nil)
  (let* ((input	(z3-render assumptions nil (list "(check-sat)" "(get-model)")))
	 (model (run-z3 input timeout)))
    (when expected-proof
      (setf (prover-model expected-proof) model)
      (setf (prover-input expected-proof) input)
      (unless (prover-output expected-proof)
	(setf (prover-output expected-proof) model)))
    model))

(defun z3-get-unsat-core (assumptions &key (timeout 10) expected-proof)
  (z3-syntax-check assumptions nil)
  (let* ((input  (concatenate 'string "(set-option :produce-unsat-cores true)" (z3-render assumptions nil (list "(check-sat)""(get-unsat-core)"))))
	 (answer (run-z3 input timeout)))
    (when expected-proof
      (setf (prover-unsat-explanation expected-proof) answer)
      (setf (prover-input expected-proof) answer)
      (setf (prover-output expected-proof) answer))
    answer))

(defun z3-check-satisfiability (assumptions &key (timeout 10) expected-proof)
  (let* ((input (concatenate 'string (z3-render assumptions) "(check-sat)" (string #\newline))))
    (when expected-proof
      (setf (prover-input expected-proof) input))
    (z3-syntax-check input)
    (let ((answer
	    (run-program-string->string
	     *z3-executable* 
	     (list  "-in" (format nil "-T:~a" timeout))
	     input)))
      (when expected-proof
	(setf (prover-output expected-proof) answer))
      (setq *last-z3-output* answer)
      (let ((result (if (does-z3-output-say-timeout answer)
			:timeout
			(if (does-z3-output-say-unsat answer)
			    :unsat
			    (if (does-z3-output-say-sat answer)
				:sat
				answer)))))
	(when expected-proof
	  (setf (result expected-proof) result))
	result))))
