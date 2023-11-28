(in-package :cl-user)

(defvar *after-slime-starts-hook* nil)

;; end a line with & and it is done in the backgroud
;; %<n> gets the result of job n
;; %% lists jobs
;; %! flushes jobs

;; UGLY dispatch. FIXME
(defun maybe-repl-job-command (args)
  (if (> (length (car args)) 1)
      (if (char= (char (car args)  0) #\%)
	  (let ((showcommand (char= (char (car args)  1) #\?)))
	    (let ((next (let ((*readtable* system::+standard-readtable+))
			  (read-from-string (car args) nil nil :start (if showcommand 2 1)))))
	      (setf (car args)
		    (cond ((numberp next)
			   (format nil "(cl-user::% ~a ~a)" next showcommand))
			  ((eq (char (string next) 0) #\%) (format nil "(cl-user::jobs)" next))
			  ((equal (string next) "!!")
			   (format nil "(cl-user::kill-all-running-processes)" next))
			  ((char= (char (car args) 1) #\?)
			   (format nil "(cl-user::%&)" next))
			  ((equal (string next) "!") 
			   (format nil "(progn (cl-user::flush-jobs)(cl-user::jobs))" next))
			  ((and (char= (char (string next) 0) #\!)
				(digit-char-p (char (string next) 1)))
			   (let ((pnum (read-from-string (subseq (string next) 1))))
			     (format nil "(cl-user::kill-processes-for-job ~a)" pnum)
			     ))
			  (t (format nil "\"Don't know what ~a means\"" (subseq (car args) 0 (- (length (car args)) 1))))))))
	  (if (#"matches" (car args) "(?s).*&\\s*$")
	      (setf (car args)
		    (format nil "(cl-user::& ~a)"
			    (#"replaceAll" (car args) "(?s)&\\s*$" "")))))))

(defun install-repl-job-command ()
  (eval `(sys::advise ,(intern "REPL-EVAL" 'swank-repl) (progn (maybe-repl-job-command sys::args) (:do-it)) :when :around)))


(defun after-swank-is-loaded (hook)
  (if (find-package 'swank)
      (funcall hook)
      (pushnew hook *after-slime-starts-hook*)))

(after-swank-is-loaded 'install-repl-job-command)

;(unadvise swank::repl-eval)
;(untrace)
