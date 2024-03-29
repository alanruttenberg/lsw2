(in-package :logic)

;; TODO somewhere: For a proof, the proof support should be satisfiable
(defvar *debug* nil)
(defvar *last-mace4-model* nil)
(defvar *last-prover9-input* nil)
(defvar *last-prover9-output* nil)
  
;; versions of these that compile on osx 10.5.5 at
;; https://github.com/alanruttenberg/ladr
;; https://github.com/alanruttenberg/iprover

(defvar cl-user::*ladr-binaries* 
  (or (ignore-errors 
       (let ((found (#"replaceAll" 
                     (uiop::run-program "which clausetester"
                                        :output :string
                                        :ignore-error-status t)
                     "(?s)(.*)/.*" "$1/")))
         (and found (not (equalp found ""))))
      "~/repos/ladr/bin/")))


;(setq *ladr-binaries* "~/repos/ladr/bin/")
(defun prover-binary (name)
  (namestring (truename (if cl-user::*ladr-binaries*
      (format nil "~a~a" cl-user::*ladr-binaries* name)
      (if (probe-file "/usr/bin/prover9")
	  (format nil "/usr/bin/~a" name)
	  (system-relative-pathname
	   "logic"
	   (make-pathname :directory
			  (list :relative
				(string-downcase (string (uiop/os:operating-system)))
				"prover9")
			  :name name)))))))


;; allow assumptions to be the fully rendered assumptions + negated goal.
(defun prepare-prover9-input (assumptions goals &key (generator (make-instance 'prover9-logic-generator)) settings hints show-translated-axioms)
  (let ((assumptions (render-axioms generator (collect-axioms-from-spec assumptions)))
	(goals (render-axioms generator (collect-axioms-from-spec goals)))
	(settings (append settings '("set(prolog_style_variables)"))))
    (when (or show-translated-axioms *debug*)
      (format t "Prover Assumptions:~% ~a~%" assumptions)
      (format t "Prover Goal:~%~a~%" goals))
    (let ((base (format nil "~{~a.~%~}formulas(assumptions).~%~aend_of_list.~%formulas(goals).~%~a~%end_of_list.~%"
			settings assumptions (or goals ""))))
      (if hints
	  (concatenate 'string base hints)
	  base))))


(defun mace-or-prover9 (which assumptions goals &key (timeout 10) (interpformat :baked) (show-translated-axioms nil)
						  max-memory domain-min-size domain-max-size max-time-per-domain-size  hints
						  expected-proof term-ordering max-weight cac-redundancy skolems-last
						  return-proof return-proof-support (include-output nil) self-label-ground 
			&aux settings)

  (assert (numberp timeout) (timeout) "Timeout should be a number of seconds") 
  (maybe-remind-to-install)

  (push (format nil "assign(max_seconds,~a)" timeout) settings)
  ;; autodenials - not confident about this setting, but the effect was in certain proofs that (horn nets) the total
  ;; proof was in parts, and so the code that got the proof support was confused.
  (push "clear(auto_denials)" settings)
  (when (eq which :mace4)
    (when max-weight (push (format nil "assign(max_weight, ~a)" max-weight) settings))
    (when domain-max-size (push (format nil "assign(end_size, ~a)" domain-max-size) settings))
    (when domain-min-size (push (format nil "assign(start_size, ~a)" domain-min-size) settings))
    (when max-memory (push (format nil "assign(max_megs, ~a)" max-memory) settings))
    (when max-time-per-domain-size (push (format nil "assign(max_seconds_per, ~a)" max-time-per-domain-size) settings))
    (when skolems-last (push (format nil "~a(skolems_last)~%" (string-downcase (string skolems-last))) settings))
    )
  (when (eq which :prover9)
    (when cac-redundancy (push (format nil "~a(cac_redundancy)~%" (string-downcase (string cac-redundancy))) settings))
    (when term-ordering (push (format nil "assign(order, ~a)" term-ordering) settings)))
  (let* ((input (prepare-prover9-input assumptions goals :settings settings :show-translated-axioms show-translated-axioms :hints hints
							 :generator (make-instance 'prover9-logic-generator :self-label-ground self-label-ground)))
	 (output
	   (run-program-string->string
	    (ecase which (:mace4 (prover-binary "mace4")) (:prover9 (prover-binary "prover9")))
	    `(,@(if (eq which :mace4) '("-c") nil) "-t" ,(prin1-to-string timeout))
	    input
	    )))
    (setq *last-prover9-input* input *last-prover9-output* output)
    (when expected-proof
      (setf (prover-input expected-proof) input)
      (setf (prover-output expected-proof) output))
    (let ((error (caar (all-matches output "%%ERROR:(.*)"  1))))
      (when error
	(let ((what (caar (all-matches output "%%START ERROR%%(.*)%%END ERROR%%" 1))))
	  ;; FIXME. This is a problem with inspect being called from a thread. Should be fixed in inspect
	  ;; (let #+swank ((swank::*buffer-package* *package*) (swank::*buffer-readtable* *readtable*)) #-swank nil
	  ;;   (inspect (cons input output)))
	  (error "~a error: ~a in: ~a" (string-downcase (string which)) error what))))
    (let ((reason (caar (all-matches output "(?s)Process \\d+ exit \\((.*?)\\)" 1))))
      (flet ((maybe-exceeded-resource-limit ()
	       (if  (or (equal reason "max_sec_no") (equal reason "max_seconds"))
		    :timeout
		    (if  (or (equal reason "max_megs") (equal reason "max_megs_no"))
			 :out-of-memory
			 (intern (string-upcase (string reason)) 'keyword)))))
	(let ((status (ecase which
			(:mace4 (if (search "interpretation" output)
				    :sat
				    (maybe-exceeded-resource-limit)))
			(:prover9 (if  (search "THEOREM PROVED" output)
				       :proved
				       (if (search "SEARCH FAILED" output)
					   :failed
					   (maybe-exceeded-resource-limit)))))))
	  (values status
		  (if return-proof
		      (prover9-output-proof-section output)
		      (if return-proof-support
			  (get-proof-support output)
			  (if include-output
			    (let ((output
				    (ecase which
				      (:mace4
				       (let ((model (make-instance 'mace4-model :raw-form output)))
					 (if (search "interpretation(" output)
					     (cook-mace4-output output interpformat model)
					     (values))))
				      (:prover9 output))))
			    (when *debug* (princ (car output)))
			      output)
                            (values)))))
	  )))))

(defun proof-to-hints (assumptions goals &optional (timeout 10))
  (let ((proof (run-program-string->string (prover-binary "prover9") `("-t" ,(princ-to-string timeout))
					   (prepare-prover9-input assumptions goals))))
    (run-program-string->string (prover-binary "prooftrans") '("hints") proof)))

(defvar *model-equality-display* :=)

(defun handle-equated-names (matched model)
  (let ((table (make-hash-table :test 'equalp)))
    (map nil (lambda(el) (push (intern (string-upcase  (first el)) 'keyword) (gethash (second el) table))) matched)
    (setf (equivalences model) (alexandria::hash-table-values table))
    (loop for constant being the hash-keys of table
	    using (hash-value names)
	  collect (list (ecase *model-equality-display*
			  (:= (format nil "~{~a~^=~}" names))
			  (:single (format nil  "~a" (car names))))
			constant))))
    

;; FIXME: predicate names with numbers in them are not getting parsed correctly.
;; ERROR: (mace4-find-model '(:and (p1 x1) (p2 x1)))
(defun cook-mace4-output (output format model)
  (if (or (eq format :cooked) (eq format :baked))
      (let ((it (reformat-interpretation output :cooked)))
	(setf (cooked-output model) it)
	(if (eq format :baked)
	    (progn 
	      ;; for reasons that escape me, sometimes true/false is indicated by a prefix of "-" for false, and an absence of it for true.
	      ;; other times (and I can't tell why in one case than another, other than when I generate interpretations for model checking)
	      ;; T/F is indicated by after the prop "= 0/1" 
	      ;; if written in the latter form, we fix that here:
	      (setq it (replace-all it "(?m)^\\s*(.*?\\(.*?\\)) = (\\d+).$"
				    (lambda(form value)
				      (format nil "~a ~a" (if (equal value "0") "-" " ") form))
				    1 2))
	      ;; get rid of the skolem functions and negatives (things that don't hold)
	      (let ((matched (reverse (all-matches it "\\n([A-Za-z0-9_]+) = (\\d+)\\." 1 2)))
		    (domain-size (caar (all-matches it "% Interpretation of size (\\d+)" 1))))
		(setq matched (handle-equated-names matched model))
		(let ((reduced (#"replaceAll"  (#"replaceAll" it "\\s(-|f\\d+|c\\d+).*" "") "\\n{2,}" (string #\newline))))
		  ;; suck up the names of the universals and other constants
		  (setq reduced (#"replaceAll" (#"replaceAll" reduced "\\n([_A-Za-z0-9]+) = (\\d+)\\." "")  "(?m)^%.*\\n" "" ))
		  ;; There may be other instances - skolems. Let's find and rename them
		  (let* ((mentioned-numbers 
			   (remove-duplicates 
			    (mapcar 'parse-integer 
				    (mapcar 'car (all-matches reduced "[^A-Za-z](\\d+)" 1)))))
		       
			 (unaccounted-for-numbers 
			   (sort (set-difference mentioned-numbers (mapcar 'parse-integer (mapcar 'second matched))) '<))
			 (already (count-if (lambda(e) (#"matches" (car e) "^c\\d+")) matched)))
                    (:print-db unaccounted-for-numbers)
		    ;; augment matched list with skolems
		    (setq matched (append (loop for number in unaccounted-for-numbers 
						for count from (1+ already)
						for name = (format nil "_sk~a" count)
						collect (list name (prin1-to-string number)))
					  matched ))
		    )
		  ;; replace the numbers for universals and constants with their name
		  (let* ((final (#"replaceAll" (replace-all reduced "(?s)(\\d+)" (lambda(num) (or (car (find num matched :test 'equalp :key 'second)) num)) 1)  "(?m)^\\s*$" ""))
			 (sexp (baked-to-sexp final)))
		    (setf (tuples model) sexp))
		  model
		  )))))
      (reformat-interpretation output format)))

(defun baked-to-sexp (macebaked &aux out)
  (setq out (#"replaceAll" macebaked "(?s)[,.]" " "))
  (setq out (replace-all out "([a-z])([A-Z])" (lambda(before after) (format nil "~a-~a" before after)) 1 2))
  (tree-replace (lambda(e)
		  (if (symbolp e) (intern (string e) 'keyword) e))
		(read-from-string (concatenate 'string "("  (#"replaceAll" out "^*(\\S+?)\\(" "($1 ") ")"))))

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
      (let ((process (run-program (prover-binary "interpformat") (list (string-downcase (string kind))) :wait nil)))
	(write-string mace4-output (process-input process) )
	(close (process-input process))
	(let ((output 
		(with-output-to-string (s)
		  (loop for line = (read-line (process-output process) nil :eof)
			until (eq line :eof)
			do (write-line line s))
		  (close (process-output process)))))
	  output)))

(defun prover9-prove (assumptions goals  &rest keys &key (timeout 10) (show-translated-axioms nil) &allow-other-keys)
  (when (and goals (atom goals)) (setq goals (list goals)))
  (apply 'mace-or-prover9  :prover9 assumptions goals :timeout timeout :show-translated-axioms show-translated-axioms keys))

(defun prover9-prove* (&rest args)
  (prog1 (apply 'prover9-prove args)))
	
(defun prover9-check-unsatisfiable (assumptions &rest keys  &key expected-proof &allow-other-keys)
  (let ((result (apply 'prover9-prove assumptions nil keys)))
    (let ((new-result 
	    (case result
	      (:proved :unsat)
	      (otherwise result))))
      (when expected-proof
	(setf (result expected-proof) new-result))
      new-result)))

(defun mace4-model-p (e)
  (or (consp e) (typep e 'mace4-model)))

(defun mace4-check-satisfiability (assumptions &rest keys &key expected-proof &allow-other-keys)
  "General version seeing whether mace can find a model"
  (let ((result (apply 'mace4-find-model assumptions  keys)))
    (when expected-proof
      (setf (result expected-proof) (if (mace4-model-p result) :sat result)))
    (if (mace4-model-p result) :sat result)))

(defun prover9-check-true (axiom &rest keys)
  (let ((result (apply 'prover9-check-unsatisfiable (negate-axiom axiom) keys)))
    (if (eq result :unsat)
	:proved
	(if (eq result :sat)
	    :disproved
	    result))))

(defun mace4-check-satisfiability-alt (assumptions &rest keys &key expected-proof &allow-other-keys)
  "Version that starts with a minimum domain size (12) and only allows 1 second per domain. This tends to work for a bunch of cases"
  (multiple-value-bind (result model)
      (apply 'mace4-find-model assumptions  :max-time-per-domain-size 1  keys)
    (when expected-proof
      (setf (result expected-proof) (if (mace4-model-p result) :sat result)))
    (if (mace4-model-p result) :sat result)))

(defun disallow-constants-with-leading-underscore (assumptions)
  (tree-walk assumptions
             (lambda(x) (if (and (symbolp x) (char= (char (string x) 0) #\_))
                            (error "models can't have constants that start with an underscore: ~:W" assumptions)))))

(defun mace4-find-model (assumptions &rest keys &key (timeout 10) (format :baked) expected-proof (pprint nil) &allow-other-keys)
  (remf keys :pprint)
  (disallow-constants-with-leading-underscore assumptions)
  (multiple-value-bind (result model)
      (apply 'mace-or-prover9 :mace4 assumptions nil :timeout timeout :interpformat format keys)
    (when expected-proof
      (setf (prover-model expected-proof) model))
    (if (eq result :sat)
	(progn
	  (when pprint (pprint-model model))
	  (setq *last-mace4-model* model))
	result)))

(defun prover9-output-proof-section (&optional (output *last-prover9-output*))
  (and (search "THEOREM PROVED" output)
       (caar (all-matches output "(?sm)={30,30} PROOF =+$(.*?)={30,30} end of proof =+$" 1))))

(defun get-prover9-proof-support ()
  (get-proof-support))

(defun get-proof-support (&optional (prover9-output *last-prover9-output*))
  (if (consp prover9-output)
      (setq prover9-output (car prover9-output)))
  (if (search "THEOREM PROVED" prover9-output)
      (let* ((proof-section (prover9-output-proof-section prover9-output))
	     (without-goal-deny (#"replaceAll" proof-section "(?m)^(.*((label\\(goal)|(\\[deny)).*?)$" ""))
	     (labels (mapcar 'car (all-matches without-goal-deny "# label\\(\"([^\"]+?)\"" 1))))
	(remove-duplicates (mapcar 'keywordify (mapcar 'string-upcase labels))))
      (values nil :wasnt-a-proof)))
    
