(in-package :logic)

(defvar *debug* nil)

;; versions of these that compile on osx 10.5.5 at
;; https://github.com/alanruttenberg/ladr
;; https://github.com/alanruttenberg/iprover

(defun prover-binary (name)
  (if (probe-file "/usr/bin/prover9")
      (format nil "/usr/bin/" name)
      (asdf::system-relative-pathname
       "logic"
       (make-pathname :directory
		      (list :relative
			    (string-downcase (string (uiop/os:operating-system)))
			    "prover9")
		      :name name))))


;; allow assumptions to be the fully rendered assumptions + negated goal.
(defun prepare-prover9-input (assumptions goals &key (generator (make-instance 'prover9-logic-generator)) settings hints show-translated-axioms)
  (let ((assumptions (render-axioms generator (collect-axioms-from-spec assumptions)))
	(goals (render-axioms generator (collect-axioms-from-spec goals)))
	(settings (append settings '("set(prolog_style_variables)"))))
    (when (or show-translated-axioms *debug*)
      (format t "Prover Assumptions:~% ~a~%" assumptions)
      (format t "Prover Goal:~%~a~%" goals))
    (let ((base (format nil "~{~a.~%~}formulas(sos).~%~aend_of_list.~%formulas(goals).~%~a~%end_of_list.~%"
			settings assumptions (or goals ""))))
      (if hints
	  (concatenate 'string base hints)
	  base))))


(defun mace-or-prover9 (which assumptions goals &key (timeout 10) (interpformat :baked) (show-translated-axioms nil)
						  domain-min-size domain-max-size generate-hints hints
						  &aux settings)

  (assert (numberp timeout) (timeout) "Timeout should be a number of seconds") 
  (maybe-remind-to-install)

  (when (eq which :mace4)
    (when domain-max-size (push (format nil "assign(end_size, ~a)" domain-max-size) settings))
    (when domain-min-size (push (format nil "assign(start_size, ~a)" domain-min-size) settings)))
  (let* ((input (prepare-prover9-input assumptions goals :settings settings :show-translated-axioms show-translated-axioms :hints hints))
	(output
	  (run-program-string->string
	   (ecase which (:mace4 (prover-binary "mace4")) (:prover9 (prover-binary "prover9")))
	   `(,@(if (eq which :mace4) '("-c") nil) "-t" ,(prin1-to-string timeout))
	   input
	   )))
      (let ((error (caar (jss::all-matches output "%%ERROR:(.*)"  1))))
	(when error
	  (let ((what (caar (jss::all-matches output "%%START ERROR%%(.*)%%END ERROR%%" 1))))
	    (setq @ (cons input output))
	    (inspect @)
	    (error "~a error: ~a in: ~a" (string-downcase (string which)) error what))))
    (values (ecase which
	      (:mace4 (if (search "interpretation" output) :sat))
	      (:prover9 (if (search "THEOREM PROVED" output)
			    :proved
			    (if (search "exit (max_seconds)" output)
				:timeout
				(if (search "SEARCH FAILED" output)
				    :failed
				    nil)))))
	      (let ((output
		      (multiple-value-list
		       (ecase which
			 (:mace4
			  (cook-mace4-output output interpformat))
			 (:prover9 output)))))
		(when *debug* (princ (car output)))
		output)
	      )))

(defun proof-to-hints (assumptions goals &optional (timeout 10))
  (let ((proof (run-program-string->string (prover-binary "prover9") `("-t" ,(princ-to-string timeout))
					   (prepare-prover9-input assumptions goals))))
    (run-program-string->string (prover-binary "prooftrans") '("hints") proof)))

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
  (when (or (not (probe-file (prover-binary "prover9")))
	    (not (probe-file (prover-binary "mace4")))
	    (not (probe-file (prover-binary "interpformat"))))
    (error (format nil (concatenate 'string
				    "One of the prover executables is not present.~%"
				    "They are expected to be in the os-specific prover9 directory of the logic asdf system:~%  ~a.~%"
				    "Source that compiles on OSX;; https://github.com/alanruttenberg/ladr and https://github.com/alanruttenberg/iprover"
				    "Some binaries from prover9 can be found at http://www.cs.unm.edu/~~~~mccune/prover9/gui/v05.html (in the app bundle for Mac OS).~%")
		   (namestring (asdf::system-relative-pathname "logic" (make-pathname :directory
										      (list :relative
											    (string-downcase (string (uiop/os:operating-system))) "prover9"))))))))


(defun reformat-interpretation (mace4-output kind)
  (assert (member kind '(:standard :standard2 :portable :tabular :raw :cooked :xml)))
      (let ((process (sys::run-program (prover-binary "interpformat") (list (string-downcase (string kind))))))
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
	
(defun prover9-check-unsatisfiable (assumptions   &rest keys)
  (let ((result (apply 'prover9-prove assumptions nil keys)))
    (case result
      (:proved :unsat)
      (otherwise result))))

(defun mace4-find-model (assumptions &key (timeout 10) (format :baked) (show-translated-axioms nil) domain-max-size domain-min-size)
  (mace-or-prover9 :mace4 assumptions nil :timeout timeout :interpformat format :show-translated-axioms show-translated-axioms
		   :domain-min-size domain-min-size :domain-max-size domain-max-size))

