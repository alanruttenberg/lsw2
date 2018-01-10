(in-package :cl-user)

(defvar *after-slime-starts-hook* nil)

;; end a line with & and it is done in the backgroud
;; %<n> gets the result of job n
;; %% lists jobs

(defun maybe-repl-job-command (args)
  (if (> (length (car args)) 1)
      (if (char= (char (car args)  0) #\%)
	  (let ((next (read-from-string (car args) nil nil :start 1 )))
	    (setf (car args)
		  (if (numberp next)
		      (format nil "(cl-user::% ~a)" next)
		      (format nil "(cl-user::jobs)" next))))
	  (if (#"matches" (car args) ".*&\\s*$")
	      (setf (car args)
		    (format nil "(cl-user::& ~a)"
			    (#"replaceAll" (car args) "(?s)&\\s*$" "")))))))

(defun install-repl-job-command ()
  (eval `(advise ,(intern "REPL-EVAL" 'swank-repl) (progn (maybe-repl-job-command args) (:do-it)) :when :around)))

(pushnew 'install-repl-job-command *after-slime-starts-hook*)

;(unadvise swank::repl-eval)
;(untrace)
