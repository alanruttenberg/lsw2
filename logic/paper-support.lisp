(in-package :logic)

;; Support for logic/proofs in papers written in org mode
;; To use this, first write the source in org mode.

;; org mode source for a proof looks like this:

;; First line is an include. The directory name defaults to proof-org/
;; but can be changed in define-paper-proof The name should match the
;; name given in define-paper-proof.
;; Immediately after we expect a lisp form that describes the proofs inside a source block
;; :exports none makes the form not show up in the paper, which is what we want.

;;;; #+INCLUDE: "i/consistent-base.org" 
;;;; #+BEGIN_SRC lisp :exports none
;;;; (define-paper-proof consistent-base
;;;;   :from :ropaper
;;;;   :kind :sat
;;;;   :with :z3
;;;;   :assumptions (:o1 :o2 :o3 :o4 :TDR :tt :ta :be1 :be2 :BT :BIW :BIB :BW :BIW)
;;;;   :theorem-text "The theory comprising $assumptions is consistent"
;;;;   :proof-text "There is a consistent model."
;;;; )     
;;;; #_END_SRC

;; Other forms are recognized as well: define-logic-axioms, define-model, paper-defaults

;; (paper-defaults &rest key-values) lets you default one or more of the initialization arguments in define-paper-proof
;; For instance, (paper-defaults :from :ropaper) lets you skip that in the define-paper-proof form.

;; (define-model name &body tuples) sets the variable name to the list of tuples.

;; Note that the local variables section needs:
;; # eval: (setq org-babel-default-header-args:lisp '((:package . "LOGIC") (:exports . "none") (:results . "silent")))
;; which defaults that no code is run during export of the org file,
;; that the package evaluated in is 'logic, and that results of
;; interactive evaluation don't put results into document

;; TODO: Write a function that creates a templated paper.org 

;; In the theorem text for $assumptions is substituted the names of the
;; formula, as references. References are assumed to be named "f:" then the
;; formula name.

;; (do-paper-proofs org-file)

;; Collects the proof specification 
;; Runs the proof, erroring if there's a problem
;; Writes the supplemental files
;; Writes the text that will be incorporated for the #INCLUDE:
;; You can do the same interactively within the source block using C-c C-c


(defvar *paper-proofs* (make-hash-table))
(defvar *proofs-noninteractive* nil)
  
(defun collect-paper-proofs (org-file)
  (setq *paper-proofs* (make-hash-table))
  (with-open-file (f org-file)
    (loop for line = (read-line f nil :eof)
	  until (eq line :eof)
	  when (#"matches" line "#\\+BEGIN_SRC lisp.*")
	    do (let ((*package* (find-package 'logic))
		     (*proofs-noninteractive* t))
		 (let ((form (read f)))
		   (when (and (consp form) (member (car form) '(define-paper-proof def-logic-axiom define-model paper-defaults)))
		     (eval form)))))))



(defclass paper-proof ()
  ((name :accessor name :initform nil :initarg :name)
   (from :accessor from :initform nil :initarg :from)
   (kind :accessor kind :initform nil :initarg :kind)
   (assumptions :accessor assumptions :initform nil :initarg :assumptions)
   (goal :accessor goal :initarg :goal :initform nil)
   (model :accessor model :initarg :model :initform nil) ; model will be specified by a symbol whose value is the model
   (reasoner :accessor reasoner :initform nil :initarg :reasoner)
   (theorem-text :accessor theorem-text :initform nil :initarg :theorem-text)
   (proof-text :accessor proof-text :initform nil :initarg :proof-text)
   (org-include-directory :initarg :org-include-directory :accessor org-include-directory :initform nil)
   (org-proof-directory :initarg :org-proof-directory :accessor org-proof-directory :initform nil)
   (org-include-name :accessor org-include-name :initarg :org-include-name :initform nil)
   (cache-directory :accessor cache-directory :initform nil :initarg :cache-directory)
   (succeeded :accessor succeeded :initarg :succeeded :initform nil)
   (invocation :accessor invocation :initarg :invocation :initform nil)
   ))

(defmethod initialize-instance ((p paper-proof) &rest initargs)
  (apply #'call-next-method p (print-db (append initargs *org-paper-defaults*)))
  ;; Default name of file to be included, directory where those are kept, and directory for proof supplemental files
  (unless (org-include-name p)
    (setf (org-include-name p) (string-downcase (string (name p)))))
  (unless (org-include-directory p)
    (setf (org-include-directory p) (asdf/system:system-relative-pathname (from p)  "i/")))
  (unless (org-proof-directory p)
    (setf (org-proof-directory p) (asdf/system:system-relative-pathname (from p) "proofs-supplemental/")))
  ;; default the reasoner if not supplied
  (unless (reasoner p)
    (setf (reasoner p) 
	  (ecase (kind p)
	    (:check-sat :z3)
	    (:check-unsat :z3)
	    (:check-model :clausetester)
	    (:prove :prover9)))))

(defvar *org-paper-defaults* nil)

(defmacro paper-defaults (&body body)
  `(setq *org-paper-defaults* ',body))

(defmacro define-paper-proof (name &rest initargs) ; from kind assumptions theorem-text proof-text goal reasoner model)
  `(progn
     (let ((proof
	     (setf (gethash ,(keywordify name) *paper-proofs*)
		   (apply 'make-instance 'paper-proof ',initargs))))
       (unless *proofs-noninteractive*
	 (maybe-cache-proof-for-paper proof)
	 (write-paper-files proof))
       proof)
     ))

(defmethod formula-name-to-reference ((p paper-proof) name)
  (format nil "\\\\ref*{f:~a}" (string-upcase (string name))))
    
(defmethod do-proof-substitutions ((p paper-proof) text)
  (setq text (#"replaceAll" text "[$]assumptions" (format nil "~{~a~^, ~}" 
							  (mapcar (lambda(e) (formula-name-to-reference p e))
								  (assumptions p)))))
  (#"replaceAll" text "[$]goal" (formula-name-to-reference p (goal p)))) 

(defmethod org-to-include-path ((p paper-proof))
  (merge-pathnames (make-pathname :name (org-include-name p) :type "org") (org-include-directory p)))

(defmethod write-org-file ((proof paper-proof))
  (ensure-directories-exist (org-to-include-path proof))
  (with-open-file (f (org-to-include-path proof) :direction :output :if-does-not-exist :create :if-exists :supersede)
    (proof-org-mode-text proof f)
    ))

(defmethod proof-org-mode-text ((proof paper-proof) stream)
  (let ((raw 
	  (with-output-to-string (s)
	    (format s "#+begin_theorem~%~a~%#+end_theorem~%" (do-proof-substitutions proof (theorem-text proof)))
	    (format s "#+begin_proof~%~a (proof by ~a; supplemental files ~a.* ~a) ~%#+end_proof"
		    (do-proof-substitutions proof (proof-text proof))
		    (string-downcase (string (reasoner proof)))
		    (name proof)
		    (if (not (succeeded proof))
			" FAILED! " ""))
	    )))
    (if (succeeded proof)
	(write-string raw stream)
	(format stream "#+BEGIN_LaTeX~%{\\color{red}~%#+END_LaTeX~%~a~&#+BEGIN_LaTeX~%}~%#+END_LaTeX~%" raw))))

(defun md5 (string)
  (format nil "~(~{~2,'0X~}~)"
	(map 'list #'identity (md5::md5sum-string string))))

(defmethod get-formulas ((p paper-proof) formulas)
  (mapcar 'axiom-sexp (collect-axioms-from-spec `((,(from p) (:or ,@formulas))) nil)))

(defmethod get-formula ((p paper-proof) formula)
  (axiom-sexp (car (get-formulas p (list formula)))))

;; The file names are: input, output, interpretation, model, result, and form
;; The file extensions are either
;;   the reasoner name for input, output, interpretation
;;   "cl" for model
;;   "lisp" for invocation
;;   "txt" for result of the invocation

(defmethod write-results ((p paper-proof) &key input output model interpretation result form)
  (ensure-directories-exist (cache-directory p))
  (labels ((the-path (name reasoner)
	     (format nil "~a~a.~a" (cache-directory p) name (string-downcase (string reasoner ))))
	   (write-it (filename reasoner string)
	     (when (keywordp string)
	       (setq string (prin1-to-string string)))
	     (with-open-file (f (the-path filename reasoner) :direction :output :if-exists :supersede :if-does-not-exist :create)
	       (write-string string f))))
    (if input (write-it "input" (if (eq (reasoner p) :clausetester) :prover9 (reasoner p))  input))
    (if output (write-it  "output" (reasoner p) output))
    (if interpretation (write-it "interpretation" (reasoner p) interpretation))
    (if model  (write-it  "model" "cl"  (with-output-to-string (s) (loop for tuple in model do (princ tuple s) (terpri)))))
    (if result (if model (write-it  "result" "txt" :sat) (write-it  "result" "txt" result)))
    (when form
      (write-it "invocation" "lisp" (with-output-to-string (s) (pprint form s)))
      (setf (invocation p) form)
      )
    (write-it "passfail" "lisp" (string (succeeded p)))
    ))
	
(defmethod must-be-reasoner ((p paper-proof) &rest possible)
  (assert (member (reasoner p) possible) ()
	  "incompatible reasoner for ~a" (name p)))
  
;; check if we've got a cached result.
;; the cache key is a string comprised of all the formulas, the kind of check, the reasoner, and model if relevant.
;; We don't use the names as the formulas may change.

(defmethod is-cached ((p paper-proof))
  (let ((*package* (find-package 'logic)))
    (let ((elements `(,@(if (goal p) (list `(:goal ,(get-formula p (goal p)))))
		      (:assumptions ,(get-formulas p (assumptions p)))
		      (:kind ,(kind p))
		      (:reasoner ,(reasoner p))
		      ,@(if (model p) (list `(:model ,(if (symbolp (model p)) (symbol-value (model p)) (model p))))))
		    ))
      (let ((md5 (md5 (with-output-to-string (s)
			(loop for (nil what) in elements
			      do (princ what s))
			))))
	(setf (cache-directory p) (format nil "~a/~a/" (asdf/system::system-relative-pathname (from p) "proofcache") md5 ))
	(and (probe-file (cache-directory p))
	     (probe-file (merge-pathnames "passfail.lisp"  (cache-directory p)))
	     (with-open-file (f (merge-pathnames "passfail.lisp"  (cache-directory p)))
	       (setf (invocation p) (with-open-file (f (merge-pathnames "invocation.lisp"  (cache-directory p))) (read f)))
	       (setf (succeeded p) (read f))))))))

	  
(defmethod failed-message ((p paper-proof) result)
  (ecase (kind p)
    (:check-unsat (format nil "~a was expected to be unsatisfiable but we got ~s instead." (name p) result))
    (:check-sat (format nil "~a was expected to be satisfiable but we got ~s instead." (name p) result))
    (:prove  (format nil "~a was expected to be proved but wasn't. Got ~s." (name p) result))
    (:check-model (format nil "in ~a the model was not satisfying. Got ~s." (name p) result))))

(defmethod maybe-cache-proof-for-paper ((p paper-proof))
  (flet ((keyed (names)
	   `((,(from p) (:or ,@names))))
	 (expect (result what)
	   (if (if (functionp what)
		   (not (funcall what result))
		   (not (eq result what)))
	       (warn (failed-message p result))
	       (setf (succeeded p) t))))
    (if (is-cached p)
	(format *debug-io* "Getting cached results for ~a~%" (name p))
	(progn
	  (format *debug-io* "Recomputing ~a ~a~%" (kind p) (name p))
	  (ecase (kind p)
	    (:check-sat
	     (must-be-reasoner p :check-sat :z3)
	     (let* ((form `(z3-find-model ',(keyed (assumptions p))))
		    (result (eval form)))
	       (expect result (lambda(e) (typep e 'z3-model)))
	       (write-results p :input *last-z3-input* :output *last-z3-output* :result result :form form
			      :model (tuples result))
	       ))
	    (:check-unsat 
	     (must-be-reasoner p :check-unsat :z3 :prover9 :vampire)
	     (let* ((form (ecase (reasoner p)
			    (:z3 `(z3-check-satisfiability ',(keyed (assumptions p))))
			    (:prover9 `(prover9-check-unsatisfiable ',(keyed (assumptions p))))
			    (:vampire `(vampire-check-unsatisfiable ',(keyed (assumptions p))))))
		    (result (eval form)))
	       (expect  result :unsat)
	       (ecase (reasoner p)
		 (:z3 (write-results p :input *last-z3-input* :output *last-z3-output* :result result :form form))
		 (:prover9 (write-results p :input *last-prover9-input* :output *last-prover9-output* :result result :form form))
		 (:vampire (write-results p :input *last-vampire-input* :output *last-vampire-output* :result result :form form)))
	       ))
	    (:prove
	     (must-be-reasoner p :prove :z3 :prover9 :vampire)
	     (let* ((form (ecase (reasoner p)
			    (:z3 `(z3-prove ',(keyed (assumptions p)) ',(keyed (list (goal p)))))
			    (:prover9 `(prover9-prove ',(keyed (assumptions p)) ',(keyed (list (goal p)))))
			    (:vampire `(vampire-prove ',(keyed (assumptions p)) ',(keyed (list (goal p)))))))
		    (result (eval form)))
	       (expect result :proved)
	       (ecase (reasoner p)
		 (:z3 (write-results p :input *last-z3-input* :output *last-z3-output* :result result  :form form))
		 (:prover9 (write-results p :input *last-prover9-input* :output *last-prover9-output* :result result :form form))
		 (:vampire (write-results p :input *last-vampire-input* :output *last-vampire-output* :result result :form form)))
	       ))
	    (:check-model
	     (must-be-reasoner p :check-model :clausetester)
	     (let* ((form `(clausetester-check-model ',(model p) ',(keyed (assumptions p))))
		    (result (eval form)))
	       (expect result :satisfying-model)
	       (write-results p :interpretation (first *last-clausetester-input*)
				:input (second *last-clausetester-input*)
				:output *last-clausetester-output*
				:model (model p)
				:form form))
	     )
	    )))))

(defmethod write-paper-files ((p paper-proof))
  (write-org-file p)
  (flet ((cache-file-named (name)
	   (car (directory (merge-pathnames (make-pathname :name name :type :wild) (cache-directory p)))))) 
    (flet ((copy-one (which &key type (suffix ""))
	     (let* ((file (cache-file-named which)))
	       (when file
		 (let ((new-name (merge-pathnames (make-pathname :name (concatenate 'string (string-downcase (string (name p))) suffix)
						:type (or type (pathname-type file)))
						  (org-proof-directory p))))
		   (ensure-directories-exist new-name)
		   (uiop/stream:copy-file (cache-file-named which) new-name)
		   (delete-file (format nil "~a.bak" (namestring new-name)))
		   ) ))))
      (copy-one "input")
      (copy-one "output" :type "out")
      (copy-one "model" :suffix "-model")
      (copy-one "interpretation" :type "interp")
      )))
  
(defmacro define-model (name &body tuples)
  `(setq ,name ',tuples))

(defun do-paper-proofs (org-file)
  (collect-paper-proofs org-file)
  (maphash (lambda(k v)
	     (declare (ignore k))
	     (maybe-cache-proof-for-paper v)
	     (write-paper-files v))
	   *paper-proofs*))

(defun flush-proof-cache (sys)
  (map nil (lambda(e) (uiop/filesystem:delete-directory-tree e :validate t))
       (directory (merge-pathnames "*" (asdf/system::system-relative-pathname sys "proofcache/")))))
  
#|
(setq cb (define-paper-proof consistent-base
	   :from :ropaper
	   :kind :check-sat
	   :assumptions (:o1 :o2 :o3 :o4 :TDR :tt :ta :be1 :be2 :BT :BIW :BIB :BW )
	   :theorem-text "The theory comprising $assumptions is consistent"
	   :proof-text "There is a consistent model."
	   ))
(maybe-cache-proof-for-paper cb)
(cache-directory cb)
(write-org-file cb)
(failed-message cb :bad)
(write-paper-files cb)
(do-paper-proofs "~/repos/writing/temporal/mungall-critique-test-paper-support.org")

;; A proof is uniqely determined by the assumption formulas, the goal formula (if applicable), the reasoner, and the kind of proof.

;; We will have a local cache directory (so it can be archived with the paper)
;; The time-consuming thing is the proofs - other work can be redone if necessary. That means we
;; need to cache the input and output files for the reasoner
;; Within the cache directory there will be a directory to hold the various artifacts.
;; Directory will be named with hash of determining information.
;; Fields in the object, and whether they are part of identifing information:

;; name - not essential
;; from - not essential - used to look up formulas
;; kind - essential sat, unsat, checkmodel, prove 
;; assumptions - essential
;; goal - essential
;; reasoner - essential 
;; theorem-text - not essential. It's cheap to regenerate
;; proof-text - not essential. It's cheap to regenerate
;; ;; directories don't need to be cached.
;; org-include-directory 
;; org-proof-directory
;; org-include-name


;; Algorithm:
;; When checking if we already have it:
;; 1. Gather list of assumptions and goals.
;; 2. sort them?
;; 3. compute hash/directory
;; 4. do the reasoning
;; 5. save input and output files
;; 6. save the invocation 
;; 7. Save time and other bits

|#
