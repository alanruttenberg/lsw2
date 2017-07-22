(in-package :logic)

;; expects vampire o
(defvar *running-in-vagrant* t)
(defvar *vampire-box-name* "vampirebox")
(defvar *vampire-executable* "/vagrant/linux/vampire")

(defvar *vampire-shared-directory-remote* (namestring "/vagrant/"))
(defvar *checked-vampire-box-present* nil)
(defvar *checked-vampire-box-running* nil)
(defvar *vampire-box-id* (cl-user::get-vagrant-box-id "vampirebox"))
(defvar *vampire-shared-directory-local* (make-directory (cl-user::get-vagrant-wd "vampirebox")))

(defclass vampire-logic-generator (z3-logic-generator) ())

(defun run-vampire (input timeout &optional (mode :vampire) (switches nil))
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
		(format nil "~a --input_syntax smtlib2 --time_limit ~a --memory_limit 4096  --mode ~a ~{--~a ~a ~} ~a "
			*vampire-executable* timeout (string-downcase (string mode)) switches
			(merge-pathnames (make-pathname :name (pathname-name file)
										     :type (pathname-type file))
								      (merge-pathnames "tmp/" *vampire-shared-directory-remote*))))
	 ""
	 )
	))))

(defun ensure-vampire-box-running () t)

(defun vampire-render (assumptions &optional goals)
  (render :z3 assumptions goals))

(defun vampire-prove (assumptions goals &key (timeout 30) (mode :vampire) (switches nil))
  (assert (eq (z3-syntax-check assumptions goals) t) (assumptions goals) "smtlib2 syntax error")
  (let ((answer 
	  (setq *last-vampire-output* 
		(run-vampire (setq *last-z3-input* (z3-render assumptions goals '("(check-sat)")))  timeout mode switches))
	  ))
    (if (and (jss::all-matches answer "Termination reason: Refutation")
	     (not (jss::all-matches answer "Termination reason: Refutation not")))
	:proved
	(if (or (jss::all-matches answer "Termination reason: Time limit")
		(and (jss::all-matches answer "Proof not found in time")
		     (jss::all-matches answer "SZS status GaveUp")))
	    :timeout
	    nil))))