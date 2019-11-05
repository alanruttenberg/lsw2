(in-package :cl-user)

;; Note: Originally I read error first, and it would sometimes hang (on one vm but not another???)  So I switched it to
;; reading output first, and just in case, peek-char the error stream before doing a read-line

;; e.g. (run-program-string->string "/bin/ls" '("-l") (namestring (truename "~/"))) -> string with directory listing

(defvar *running-processes* nil)
  
(defun run-program-string->string (executable args &optional input &key wd)
  (let ((process (sys::run-program executable args :directory wd :input (if input :stream))))
    (push process *running-processes*)
    (when (boundp '*active-job-slot*)
      (push (list process executable args) (unix-processes (aref *jobs* *active-job-slot*))))
    (unwind-protect 
	 (progn
	   (when input
	     (write-string input (sys::process-input process))
	     (ignore-errors (close (sys::process-input process))))
	   (let ((output (with-output-to-string (s)
			   (loop for line = (read-line (sys::process-output process) nil :eof)
				 until (eq line :eof)
				 do (write-line line s) ))))
	     (let ((error 

		     (catch 'caught
		       (handler-bind ((sys::stream-error #'(lambda (c) (throw 'caught c))))
			 (unless (member (peek-char nil (sys::process-error process) nil :eof) '(:eof #\Null) :test 'eql)
			   (with-output-to-string (s)
			     (loop for line = (read-line (sys::process-error process) nil :eof)
				   until (eq line :eof)
				   do (write-line line s))))))))
	       (values output error))))
      (setq *running-processes* (remove process *running-processes*)))))



