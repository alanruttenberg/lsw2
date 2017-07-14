(in-package :logic)

(flet ((prover-binary (name)
	 (asdf::system-relative-pathname
	  "logic"
	  (make-pathname :directory
			 (list :relative
			       (string-downcase (string (uiop/os:operating-system)))
			       "prover9"))
	  :name name)))

  (defvar *prover9-executable* (prover-binary "prover9"))
  (defvar *mace4-executable* (prover-binary "mace4"))
  (defvar *interpformat-executable* (prover-binary "interpformat")))


(defun mace-or-prover9 (which assumptions goals &key (timeout 10) (interpformat :cooked))
  (maybe-remind-to-install)
  (let ((generator (make-instance 'prover9-logic-generator)))
    (let ((assumptions
	    (if (stringp assumptions) assumptions
		(format nil "~{   ~a.~%~}" (mapcar (lambda(e) (if (stringp e) e (generate-from-sexp generator e))) assumptions))))
	  (goals
	    (if (stringp goals) goals
		(format nil "~{   ~a.~%~}" (mapcar (lambda(e) (if (stringp e) e (generate-from-sexp generator e))) goals)))))
      (let ((process (sys::run-program (ecase which
					 (:mace4 *mace4-executable*)
					 (:prover9 *prover9-executable*))
				       `(,@(if (eq which :mace4) '("-c") nil) "-t" ,(prin1-to-string timeout)))))
	(format (sys::process-input process) "formulas(sos).~%~aend_of_list.~%formulas(goals).~%~a~%end_of_list.~%"
		assumptions
		goals)
	(close (sys::process-input process))
	(let ((output 
		(with-output-to-string (s)
		  (loop for line = (read-line (sys::process-output process) nil :eof)
			until (eq line :eof)
			do (write-line line s))
		  (close (sys::process-output process)))))
	  (let ((error (caar (jss::all-matches output "%%ERROR:(.*)"  1))))
	    (when error
	      (let ((what (caar (jss::all-matches output "(?s)%%START ERROR%%(.*)%%END ERROR%%" 1))))
		(error "~a error: ~a in: ~a" (string-downcase (string which)) error what))))
	  (values (if (ecase which
			(:mace4 (search "interpretation" output))
			(:prover9 (search "THEOREM PROVED" output)))
		      t)
		  (ecase which
		    (:mace4 (if interpformat (reformat-interpretation output interpformat) output))
		    (:prover9 output))
		  ))))))

(defun maybe-remind-to-install ()
  (when (or (not (probe-file *prover9-executable*))
	    (not (probe-file *mace4-executable*))
	    (not (probe-file *interpformat-executable*)))
    (error (format nil (concatenate 'string
				    "One of the prover executables is not present.~%"
				    "They are expected to be in the os-specific prover9 directory of the logic asdf system:~%  ~a.~%"
				    "Binaries can be found at http://www.cs.unm.edu/~~~~mccune/prover9/gui/v05.html (in the app bundle for Mac OS).~%")
		   (namestring (asdf::system-relative-pathname "logic" (make-pathname :directory
										      (list :relative
											    (string-downcase (string (uiop/os:operating-system))) "prover9"))))))))


(defun reformat-interpretation (mace4-output kind)
  (assert (member kind '(:standard :standard2 :portable :tabular :raw :cooked :xml)))
      (let ((process (sys::run-program *interpformat-executable* (list (string-downcase (string kind))))))
	(write-string mace4-output (sys::process-input process) )
	(close (sys::process-input process))
	(let ((output 
		(with-output-to-string (s)
		  (loop for line = (read-line (sys::process-output process) nil :eof)
			until (eq line :eof)
			do (write-line line s))
		  (close (sys::process-output process)))))
	  output)))

(defun prover9-prove (assumptions goals  &key (timeout 10))
  (mace-or-prover9  :prover9 assumptions goals :timeout timeout))

(defun mace4-find-model (assumptions goals  &key (timeout 10) (format :cooked))
  (mace-or-prover9 :mace4 assumptions goals :timeout timeout :interpformat format))

