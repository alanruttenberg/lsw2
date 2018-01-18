(in-package :logic)

;; expects vampire o
(defvar *running-in-vagrant* t)
(defvar *vampire-box-name* "vampirebox")
(defvar *vampire-executable* "/vagrant/linux/vampire")

(defvar *vampire-shared-directory-remote* (namestring "/vagrant/"))
(defvar *checked-vampire-box-present* nil)
(defvar *checked-vampire-box-running* nil)
(defvar *vampire-box-id* (and *running-in-vagrant* (get-vagrant-box-id *vampire-box-name*)))
(defvar *vampire-shared-directory-local* (and *running-in-vagrant* (make-pathname :directory (get-vagrant-box-wd *vampire-box-name*))))
(defvar *last-vampire-output* )
(defvar *last-vampire-input*)

(defclass vampire-logic-generator (z3-logic-generator) ())

(defun run-vampire (input timeout &optional (mode :vampire) (switches nil))
  (if *running-in-vagrant* 
      (progn
	(ensure-vampire-box-running)
	(let ((tmpdir (merge-pathnames (make-pathname :directory '(:relative "tmp")) *vampire-shared-directory-local*)))
	  (ensure-directories-exist tmpdir)
	  (let ((file (uiop/stream::get-temporary-file :directory tmpdir)))
	    (with-open-file (f file :direction :output)
			    (write-string input f))
	    (multiple-value-bind (output error-output)
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
	      (assert (or (not error-output) ) () "Vampire failed: ~a" error-output)
	    ))))
    (let ((file (uiop/stream::get-temporary-file :directory "/tmp")))
      (with-open-file (f file :direction :output)
		      (write-string input f))
      (run-program-string->string  *vampire-executable*
				   `("--input_syntax" "smtlib2"
				     "--time_limit" ,(prin1-to-string timeout)
				     "--memory_limit" "4096"
				     "--mode" ,(string-downcase (string mode))
				     ,(namestring file)
				     ,@switches)
				   ""))))

(defun ensure-vampire-box-running (&optional force)
  (when force
    (setq *vampire-box-id* (get-vagrant-box-id *vampire-box-name*)))
  (or (if force nil *checked-vampire-box-running*)
      (unless (equalp (get-vagrant-box-status *vampire-box-id*) "running")
	(vagrant-box-up  *vampire-box-id*)
	(unless (equalp (get-vagrant-box-status *vampire-box-id*) "running")
	  (error "Couldn't bring up vagrant box ~a" *vampire-box-id*)))
      (setq *checked-vampire-box-running* t)))

(defun vampire-render (assumptions &optional goals commands)
  (apply 'concatenate 'string
	 (render :vampire assumptions goals)
	 (mapcar (lambda(e) (format nil "~a" e)) commands)))

(defun vampire-prove (assumptions goals &key (timeout 30) (mode :vampire) (switches nil) expected-proof &allow-other-keys)
  (assert (eq (z3-syntax-check assumptions goals) t) (assumptions goals) "smtlib2 syntax error")
  (let* ((input  (vampire-render assumptions goals '("(check-sat)")))
	 (answer 
	   (setq *last-vampire-output* 
		 (run-vampire (setq *last-vampire-input* input)  timeout mode switches))
	   ))
    (let ((result 
	    (if (and (jss::all-matches answer "Termination reason: Refutation")
		     (not (jss::all-matches answer "Termination reason: Refutation not" 0)))
		:proved
		(if (or (jss::all-matches answer "Termination reason: Time limit" 0)
			(and (jss::all-matches answer "Proof not found in time" 0)
			     (jss::all-matches answer "SZS status GaveUp" 0)))
		    :timeout
		    nil))))
      (when expected-proof
	(setf (prover-input expected-proof) input)
	(setf (prover-output expected-proof) answer)
	(setf (result expected-proof) result))
      result)))
    
(defun vampire-check-unsatisfiable (assumptions &rest keys &key expected-proof &allow-other-keys)
  (let ((result (apply 'vampire-prove assumptions nil keys)))
    (let ((new-result
	    (case result
	      (:proved :unsat)
	      (otherwise result))))
      (when expected-proof
	(setf (result expected-proof) new-result))
      new-result)))
