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
  (run-program-string->string
   *z3-executable* 
   (list  "-in" (format nil "-T:~a" timeout))
   input
   ))

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
  
(defun z3-prove (assumptions goals &key (timeout 30) (return-contradiction nil) )
  (z3-syntax-check assumptions goals)
  (let ((answer 
	  (setq *last-z3-output* 
	  (run-z3 (setq *last-z3-input* (z3-render assumptions goals '("(check-sat)"))) timeout)
	  )))
    (or (z3-output-errors answer)
	(if (does-z3-output-say-timeout answer)
	    :timeout
	    (if (does-z3-output-say-unsat answer)
		:proved
		(if (does-z3-output-say-sat answer)
		    (if return-contradiction
			(values :disproved
			    (z3-find-model (append assumptions goals) nil timeout)
			    )
			:disproved)))))))


(defun z3-find-model (assumptions &key (timeout 10))
  (z3-syntax-check assumptions nil)
  (run-z3
   (z3-render assumptions nil (list "(check-sat)" "(get-model)"))
   timeout))

(defun z3-get-unsat-core (assumptions &key (timeout 10))
  (z3-syntax-check assumptions nil)
  (run-z3
   (concatenate 'string "(set-option :produce-unsat-cores true)" (z3-render assumptions nil (list "(check-sat)""(get-unsat-core)")))
   timeout))

(defun z3-check-satisfiability (assumptions &key (timeout 10))
  (let* ((input (z3-render assumptions)))
    (z3-syntax-check input)
    (let ((answer
	    (setq *last-z3-output* (run-program-string->string
				    *z3-executable* 
				    (list  "-in" (format nil "-T:~a" timeout))
				    (setq *last-z3-input*
					  (concatenate 'string input
						       "(check-sat)"
						       (string #\newline)))))))
      (if (does-z3-output-say-timeout answer)
	  :timeout
	  (if (does-z3-output-say-unsat answer)
	      :unsat
	      (if (does-z3-output-say-sat answer)
		  :sat
		  answer))))))
