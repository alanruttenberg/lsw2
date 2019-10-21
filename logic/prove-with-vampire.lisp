(in-package :logic)

(defvar *running-in-vagrant* nil)
(defvar *vampire-box-name* "vampirebox")

;; On OSX
;; cp vampire.rb /usr/local/Homebrew/Library/Taps/homebrew/homebrew-core/Formula/
;; brew install vampire

(defvar *vampire-executable* "vampire_rel")

(defvar *vampire-shared-directory-remote* (namestring "/vagrant/"))
(defvar *checked-vampire-box-present* nil)
(defvar *checked-vampire-box-running* nil)
(defvar *vampire-box-id* (and *running-in-vagrant* (get-vagrant-box-id *vampire-box-name*)))
(defvar *vampire-shared-directory-local* (and *running-in-vagrant* (make-pathname :directory (get-vagrant-box-wd *vampire-box-name*))))
(defvar *last-vampire-output* )
(defvar *last-vampire-input*)
(defvar *last-vampire-axiom-map*)

(defclass vampire-logic-generator (z3-logic-generator)
  ((axiom-map :accessor axiom-map)))

(defmethod with-names ((g vampire-logic-generator)) nil)

(defmethod render-axioms ((g vampire-logic-generator) (a list))
  (setf *last-vampire-axiom-map* (setf (axiom-map g) (coerce a 'vector)))
  (values (call-next-method) g))

(defun get-vampire-proof-support ()
  (loop for (num) in (butlast (all-matches *last-vampire-output* "(?m)(\\d+)\\.\\s+(\\S+).*\\[input\\]" 1 2))
	for ax = (svref *last-vampire-axiom-map* (1- (parse-integer num)))
	if (typep ax 'axiom)
	  collect (keywordify (axiom-name ax))
	else collect ax))

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
	      output
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
      (unless (is-vagrant-box-running *vampire-box-id*)
	(vagrant-box-up  *vampire-box-id*)
	(unless (equalp (get-vagrant-box-status *vampire-box-id*) "running")
	  (error "Couldn't bring up vagrant box ~a" *vampire-box-id*)))
      (setq *checked-vampire-box-running* t)))

;; TODO modify each axiom with an extra clause (:= <label> <label>)
;; Then look for lines with [input] and extract to recover the axiom.
;; Will require making a copy of the axiom
(defun vampire-render (assumptions &optional goals commands)
  (apply 'concatenate 'string
	 (render :vampire assumptions goals)
	 (mapcar (lambda(e) (format nil "~a" e)) commands)))

(defun vampire-prove (assumptions goals &key (timeout 30) (mode :vampire) (switches nil) expected-proof &allow-other-keys)
  (if (and goals (atom goals)) (setq goals (list goals)))
  (assert (eq (z3-syntax-check assumptions goals) t) (assumptions goals) "smtlib2 syntax error")
  (let* ((input  (vampire-render assumptions goals '("(check-sat)")))
	 (answer 
	   (setq *last-vampire-output* 
		 (run-vampire (setq *last-vampire-input* input)  timeout mode switches))
	   ))
    (let ((result
	    (cond ((or (search "Termination reason: Time limit" answer)
		       (search "Proof not found in time" answer))
		   :timeout)
		  ((search "Refutation found." answer)
		   :proved)
		  (t :failed))))
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

;; Should be able to parse this to get proof support from vampire
;; 210. (temporalPartOf(X0,X1) => occurrentPartOf(X0,X1)) [input]
;; 233. ! [X0 : $int,X1 : $int] : (occurrentPartOf(X0,X1) => (? [X3 : $int] : instanceOf(X0,processBoundary,X3) => ? [X2 : $int] : (instanceOf(X1,process,X2) | instanceOf(X1,processBoundary,X2)))) [input]
;; 234. ! [X0 : $int,X1 : $int] : (occurrentPartOf(X0,X1) => (? [X3 : $int] : instanceOf(X0,process,X3) => ? [X2 : $int] : instanceOf(X1,process,X2))) [input]
;; 279. ~! [X0 : $int,X1 : $int] : (temporalPartOf(X0,X1) => (? [X3 : $int] : (instanceOf(X0,process,X3) | instanceOf(X0,processBoundary,X3)) => ? [X2 : $int] : (instanceOf(X1,process,X2) | instanceOf(X1,processBoundary,X2)))) [input]
