(in-package :logic)

(defvar *debug* nil)

(flet ((prover-binary (name)
	 (asdf::system-relative-pathname
	  "logic"
	  (make-pathname :directory
			 (list :relative
			       (string-downcase (string (uiop/os:operating-system)))
			       "prover9")
			 :name name))))

  (defvar *prover9-executable* (prover-binary "prover9"))
  (defvar *mace4-executable* (prover-binary "mace4"))
  (defvar *interpformat-executable* (prover-binary "interpformat")))


(defun mace-or-prover9 (which assumptions goals &key (timeout 10) (interpformat :baked) (show-translated-axioms nil)
						  domain-min-size domain-max-size)
  (assert (numberp timeout) (timeout) "Timeout should be a number of seconds") 
  (maybe-remind-to-install)
  (let ((generator (make-instance 'prover9-logic-generator)))
    (let ((assumptions
	    (if (stringp assumptions) 
		assumptions
		(format nil "~{   ~a~%~}" (mapcar (lambda(e) 
						    (if (stringp e) e
							(if (typep e 'axiom)
							    (logic::render generator e)
							    (concatenate 'string (generate-from-sexp generator e) ".")
							    )))
						  assumptions))))
	  (goals
	    (if (stringp goals) goals
		(format nil "~{   ~a~%~}" (mapcar (lambda(e) 
						    (if (stringp e) e
							(if (typep e 'axiom)
							    (logic::render generator e)
							    (concatenate 'string (generate-from-sexp generator e) ".")
							    ))) goals))))
	  (settings '("set(prolog_style_variables)")))
      (when (eq which :mace4)
	(when domain-max-size (push (format nil "assign(end_size, ~a)" domain-max-size) settings))
	(when domain-min-size (push (format nil "assign(start_size, ~a)" domain-min-size) settings)))
      (when (or show-translated-axioms *debug*)
	(format t "Prover Assumptions:~% ~a~%" assumptions)
	(format t "Prover Goal:~%~a~%" goals))
      (let ((process (sys::run-program (ecase which
					 (:mace4 *mace4-executable*)
					 (:prover9 *prover9-executable*))
				       `(,@(if (eq which :mace4) '("-c") nil) "-t" ,(prin1-to-string timeout))))
	    (input 
	      (format nil "~{~a.~%~}formulas(sos).~%~aend_of_list.~%formulas(goals).~%~a~%end_of_list.~%"
		      settings
		      assumptions
		      goals)))
	(write-string input (sys::process-input process))
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
		(setq @ (cons input output))
		(inspect @)
		(error "~a error: ~a in: ~a" (string-downcase (string which)) error what))))
	  (values (if (ecase which
			(:mace4 (search "interpretation" output))
			(:prover9 (search "THEOREM PROVED" output)))
		      t)
		  (let ((output
			  (multiple-value-list
			   (ecase which
			     (:mace4
			      (cook-mace4-output output interpformat))
			     (:prover9 output)))))
		    (when *debug* (princ (car output)))
		    output)
		  ))))))

(defun cook-mace4-output (output format)
  (if (or (eq format :cooked) (eq format :baked))
      (let ((it (reformat-interpretation output :cooked)))
	(if (eq format :baked)
	    (let ((reduced (#"replaceAll"  (#"replaceAll" it "\\s(-|f\\d+|c\\d+).*" "") "\\n{2,}" (string #\newline))))
	      (let ((matched (reverse (jss::all-matches reduced "\\n([A-Za-z]+) = (\\d+)\\." 1 2))))
		(setq reduced (#"replaceAll" reduced "\\n([A-Za-z]+) = (\\d+)\\." ""))
		  (loop for (name number) in matched
		    do (setq reduced (#"replaceAll" reduced (concatenate 'string "([^0-9])(" number ")([^0-9])") 
						    (concatenate 'string "$1" name "$3")))))
	      reduced)
	    it))
      (reformat-interpretation output format)))

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

(defun prover9-prove (assumptions goals  &key (timeout 10) (show-translated-axioms nil))
  (mace-or-prover9  :prover9 assumptions goals :timeout timeout :show-translated-axioms show-translated-axioms))

(defun mace4-find-model (assumptions goals  &key (timeout 10) (format :baked) (show-translated-axioms nil) domain-max-size domain-min-size)
  (mace-or-prover9 :mace4 assumptions goals :timeout timeout :interpformat format :show-translated-axioms show-translated-axioms
		   :domain-min-size domain-min-size :domain-max-size domain-max-size))

