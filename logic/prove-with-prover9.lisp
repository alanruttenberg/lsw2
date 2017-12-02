(in-package :logic)

(defvar *debug* nil)
(defvar *last-mace4-model* nil)
(defvar *last-prover9-input* nil)
(defvar *last-prover9-output* nil)
  
;; versions of these that compile on osx 10.5.5 at
;; https://github.com/alanruttenberg/ladr
;; https://github.com/alanruttenberg/iprover

(defun prover-binary (name)
  (if (probe-file "/usr/bin/prover9")
      (format nil "/usr/bin/~a" name)
      (system-relative-pathname
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
						  max-memory domain-min-size domain-max-size max-time-per-domain-size generate-hints hints
						  &aux settings)

  (assert (numberp timeout) (timeout) "Timeout should be a number of seconds") 
  (maybe-remind-to-install)

  (when (eq which :mace4)
    (when domain-max-size (push (format nil "assign(end_size, ~a)" domain-max-size) settings))
    (when domain-min-size (push (format nil "assign(start_size, ~a)" domain-min-size) settings))
    (when max-memory (push (format nil "assign(max_megs, ~a)" max-memory) settings))
    (when max-time-per-domain-size (push (format nil "assign(max_seconds_per, ~a)" max-time-per-domain-size) settings)))
  (let* ((input (prepare-prover9-input assumptions goals :settings settings :show-translated-axioms show-translated-axioms :hints hints))
	(output
	  (run-program-string->string
	   (ecase which (:mace4 (prover-binary "mace4")) (:prover9 (prover-binary "prover9")))
	   `(,@(if (eq which :mace4) '("-c") nil) "-t" ,(prin1-to-string timeout))
	   input
	   )))
    (setq *last-prover9-input* input *last-prover9-output* output)
      (let ((error (caar (all-matches output "%%ERROR:(.*)"  1))))
	(when error
	  (let ((what (caar (all-matches output "%%START ERROR%%(.*)%%END ERROR%%" 1))))
	    (setq @ (cons input output))
	    (inspect @)
	    (error "~a error: ~a in: ~a" (string-downcase (string which)) error what))))
    (values (ecase which
	      (:mace4 (if (search "interpretation" output)
			  :sat
			  (if (search "exit (max_sec_no)" output)
			      :timeout
			      (if (search "exit (exhausted)" output)
				  :exhausted
				  :error))))
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

;; (defun cook-mace4-output (output format)
;;   (if (or (eq format :cooked) (eq format :baked))
;;       (let ((it (reformat-interpretation output :cooked)))
;; 	(if (eq format :baked)
;; 	    ;; get rid of the skolem functions and negatives (things that don't hold)
;; 	    (let ((reduced (#"replaceAll"  (#"replaceAll" it "\\s(-|f\\d+|c\\d+).*" "") "\\n{2,}" (string #\newline))))
;; 	      ;; suck up the names of the universals and other constants
;; 	      (let ((matched (reverse (all-matches reduced "\\n([A-Za-z0-9]+) = (\\d+)\\." 1 2))))
;; 		(print matched)
;; 		(setq reduced (#"replaceAll" reduced "\\n([A-Za-z]+) = (\\d+)\\." ""))
;; 		;; replace the numbers for universals and constants with their name
;; 		(loop for (name number) in matched
;; 		      do (setq reduced (#"replaceAll" reduced (concatenate 'string "([^0-9])(" number ")([^0-9])") 
;; 						      (concatenate 'string "$1" name "$3"))))
;; 		reduced))
;; 	    it))
;;       (reformat-interpretation output format)))

(defun cook-mace4-output (output format)
  (if (or (eq format :cooked) (eq format :baked))
      (let ((it (reformat-interpretation output :cooked)))
	(if (eq format :baked)
	    ;; get rid of the skolem functions and negatives (things that don't hold)
	    (let ((matched (reverse (all-matches it "\\n([A-Za-z0-9]+) = (\\d+)\\." 1 2))))
	      (let ((reduced (#"replaceAll"  (#"replaceAll" it "\\s(-|f\\d+|c\\d+).*" "") "\\n{2,}" (string #\newline))))
		;; suck up the names of the universals and other constants
		(setq reduced (#"replaceAll" (#"replaceAll" reduced "\\n([A-Za-z]+) = (\\d+)\\." "")  "(?m)^%.*\\n" "" ))
		;; There may be other instances - skolems. Let's find and rename them
		(let* ((mentioned-numbers 
			 (remove-duplicates 
			  (mapcar 'parse-integer 
				  (mapcar 'car (all-matches reduced "[^A-Za-z](\\d+)" 1)))))
		       (unaccounted-for-numbers 
			 (sort (set-difference mentioned-numbers (mapcar 'parse-integer (mapcar 'second matched))) '<))
		       (already (count-if (lambda(e) (#"matches" (car e) "^c\\d+")) matched)))
		  ;; augment matched list with the new assignments
		  (setq matched (append (loop for number in unaccounted-for-numbers
					      for count from (1+ already)
					      for name = (format nil "c~a" count)
					      collect (list name (prin1-to-string number)))
					matched ))
		  )
		;; replace the numbers for universals and constants with their name
		(replace-all reduced "(?s)(\\d+)" (lambda(num) (car (find num matched :test 'equalp :key 'second))) 1)
		))
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
		   (namestring (system-relative-pathname "logic" (make-pathname :directory
										      (list :relative
											    (string-downcase (string (uiop/os:operating-system))) "prover9"))))))))


(defun reformat-interpretation (mace4-output kind)
  (assert (member kind '(:standard :standard2 :portable :tabular :raw :cooked :xml)))
      (let ((process (run-program (prover-binary "interpformat") (list (string-downcase (string kind))))))
	(write-string mace4-output (process-input process) )
	(close (process-input process))
	(let ((output 
		(with-output-to-string (s)
		  (loop for line = (read-line (process-output process) nil :eof)
			until (eq line :eof)
			do (write-line line s))
		  (close (process-output process)))))
	  output)))

(defun prover9-prove (assumptions goals  &key (timeout 10) (show-translated-axioms nil))
  (mace-or-prover9  :prover9 assumptions goals :timeout timeout :show-translated-axioms show-translated-axioms))
	
(defun prover9-check-unsatisfiable (assumptions   &rest keys)
  (let ((result (apply 'prover9-prove assumptions nil keys)))
    (case result
      (:proved :unsat)
      (otherwise result))))

(defun mace4-check-satisfiability (assumptions &rest keys)
  "General version seeing whether mace can find a model"
  (let ((result (apply 'mace4-find-model assumptions keys)))
    (case result
      (:sat :sat)
      (otherwise result))))

(defun mace4-check-satisfiability-alt (assumptions &rest keys)
  "Version that starts with a minimum domain size (12) and only allows 1 second per domain. This tends to work for a bunch of cases"
  (multiple-value-bind (result model)
      (apply 'mace4-find-model assumptions :max-time-per-domain-size 1 :domain-min-size 12 keys)
    (values
     (case result
       (:sat :sat)
       (otherwise result))
     (if (eq result :sat)
	 model)
     )))

(defun mace4-find-model (assumptions &key (timeout 10) (format :baked) (show-translated-axioms nil) domain-max-size domain-min-size max-memory
				       max-time-per-domain-size)
  (multiple-value-bind(result model)
      (mace-or-prover9 :mace4 assumptions nil :timeout timeout :interpformat format :show-translated-axioms show-translated-axioms
								   :domain-min-size domain-min-size :domain-max-size domain-max-size :max-memory max-memory
					      :max-time-per-domain-size max-time-per-domain-size)
    (values result (setq *last-mace4-model* model))))


