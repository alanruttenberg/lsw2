(in-package :logic)

;; expects vampire o
(defvar *running-in-vagrant* t)
(defvar *vampire-box-name* "vampirebox")
(defvar *vampire-executable* "/vagrant/linux/vampire")
(defvar *vampire-shared-directory-local* (namestring (truename "~/repos/bfo-theory/tools/vampire/")))
(defvar *vampire-shared-directory-remote* (namestring "/vagrant/"))
(defvar *checked-vampire-box-present* nil)
(defvar *checked-vampire-box-running* nil)
(defvar *vampire-box-id* "5878d63")
(defclass vampire-logic-generator (z3-logic-generator))

(defun run-vampire (input timeout)
  (when *running-in-vagrant* 
    (ensure-vampire-box-running)
    (let ((tmpdir (merge-pathnames (make-pathname :directory '(:relative "tmp")) *vampire-shared-directory-local*)))
      (ensure-directories-exist tmpdir)
      (let ((file (uiop/stream::get-temporary-file :directory tmpdir)))
	(with-open-file (f file :direction :output)
	  (write-string input f))
	(run-program-string->string
	 "vagrant" 
	 (list  "ssh" *vampire-box-id* "-c"
		(format nil "~a --input_syntax smtlib2 --time_limit ~a --memory_limit 4096  --mode vampire  ~a"
			*vampire-executable* timeout (merge-pathnames (make-pathname :name (pathname-name file)
										     :type (pathname-type file))
								      (merge-pathnames "tmp/" *vampire-shared-directory-remote*))))
	 ""
	 )
	))))

(defun ensure-vampire-box-running () t)

(defun vampire-render (assumptions &optional goals)
  (render :z3 assumptions goals))

(defun vampire-prove (assumptions goals &key (timeout 30) )
  (assert (eq (z3-syntax-check assumptions goals) t) (assumptions goals) "smtlib2 syntax error")
  (let ((answer 
	  (setq *last-vampire-output* 
		(run-vampire (setq *last-z3-input* (z3-render assumptions goals '("(check-sat)"))) timeout))
	  ))
    (if (jss::all-matches answer "Termination reason: Refutation")
	:proved
	(if (jss::all-matches answer "Termination reason: Time limit")
	    :timeout
	    nil))))
