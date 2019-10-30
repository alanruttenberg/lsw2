(in-package :logic)

;; Support for logic/proofs in papers written in org mode
;; To use this, first write the source in org mode.
;; Generated files are put in one of three places relative to org paper file
;; i/formulas - tex to be included for formulas
;; i/proofs - tex to be included for proofs
;; supplemental/ - files intended to be distributed with the paper as supplemental material
;; supplemental/formulas.cl - the formulas in the paper, in CLIF format
;; supplemental/proofs - the supplemental files for proofs - native inputs and outputs
;; proof-cache/ - cache files so that proofs don't have to be re-evaluated unless they have changed

;; org mode source for a proof looks like this:

;; First line is an optional name. With name the results block can be put anywhere. Recommended.
;; Immediately after we expect a lisp form that describes the proofs inside a source block
;; :exports none makes the form not show up in the paper, which is what we want.

;;;; #+NAME: proof-consistent-base
;;;; #+BEGIN_SRC lisp 
;;;; (add-proof-here consistent-base
;;;;   :formula-key :ropaper
;;;;   :kind :sat
;;;;   :reasoner :z3
;;;;   :assumptions (:o1 :o2 :o3 :o4 :TDR :tt :ta :be1 :be2 :BT :BIW :BIB :BW :BIW)
;;;;   :theorem-text "The theory comprising $assumptions is consistent"
;;;;   :proof-text "There is a consistent model."
;;;; )     
;;;; #+END_SRC

;; When you execute the block, a results block is added
;; To execute a block you can do C-c C-c in the body of the lisp source, or
;; "M-x org-babel-execute-buffer" to execute all source blocks

;;;; #+RESULTS: proof-consistent-base
;;;; #+BEGIN_LaTeX
;;;; \input{i/proofs/consistent-base}
;;;; #+END_LaTeX

;; org mode source for a formula looks like this:

;; First line is an optional name. With name the results block can be put anywhere. Recommended.
;; Immediately after we expect a series of forms denoting formulas.
;; The formula can either be a name followed by an LSW formula followed by keywords,
;; as in symmetric-f below. Or it can be the name of a formula in the lisp environment,
;; followed by keywords, in which case the actual formula is retrieved with get-axiom
;; Keywords:
;;  The paper key (:howtopaper below) can be used to give a formula identifiers in the typeset version.
;;  This is useful if there might be different labels for different papers, all in the same lisp source.
;; Alternatively :label can be given

;; #+NAME: symmetric-f
;; #+BEGIN_SRC lisp
;; (add-formulas-here
;;    (symmetric-f
;;       (:forall (?x ?y) (:implies (my-relation ?x ?y) (my-relation ?y ?x)))
;;        :howtopaper :symf)
;;    (:exists-at-domain-range :label :edf))
;; #+END_SRC 

;; When you execute the block a results block is added.

;; #+BEGIN_LaTeX
;; \insertFormulaPath{./i/formulas/symmetric-f.tex}\vspace*{-\baselineskip}\vspace*{-\baselineskip}
;; \insertFormulaPath{./i/formulas/exists-at-domain-range.tex}\vspace*{-\baselineskip}
;; #+END_LaTeX

;; The function (write-formulas-clif) will write all formulas to supplemental directory formulas.cl

;; How implemented:
;; Org mode is set to not evaluate blocks on export, but to export results, and for results to be
;; interpreted as latex.
;; You add the results manually with C-c C-c in the source block, or do the whole buffer with M-x org-babel-execute-buffer
;; The function that is called generates some latex and returns it as a string, which is put in the result

;; For add-proof here and add-formulas-here one or more auxilliary tex files are generated and put in the "i/"
;; directory relative to the org source file. These are only generated when explicitly evaluating the source
;; block, so that the text of the paper can be edited without having lisp running.

;; Other forms are recognized as well: add-formulas-here, define-model, paper-defaults

;; (paper-defaults &rest key-values) lets you default one or more of the initialization arguments in add-proof-here
;; You need at least something like (paper-defaults :formula-key :ropaper). 

;; (define-model name &body tuples) sets the variable name to the list of tuples.

;; Note that the local variables section needs:

;; # eval: (setq org-babel-default-header-args:lisp '((:package . "LOGIC") (:exports . "results") (:results . "replace value latex")))
;; # eval: (setq org-export-babel-evaluate nil)

;; which defaults that no code is run during export of the org file,
;; that the package evaluated in is 'logic, and that results of
;; interactive evaluation are put into the document as latex

;; TODO: Write a function that creates a templated paper.org 

;; Formulas: The formula-key is used to look up what the formula should be labeled in the paper.
;; Make sure that add-formulas-here has a key/value like :ropaper :ot 
;; References to formulas are assumed to be named "f:" then the formula name.
;; When the block is executed the latex is generated and saved in ./i/formulas/

;; Proofs: Assumptions and goals are a list of the formula-keys (e.g. (:ot) above)
;; In the theorem text for $assumptions is substituted the formula-keys of the
;; formula, as references. Proof associated files are cached in "./proof-cache"

;; (do-paper-proofs org-file)

;; Collects the proof specification 
;; Runs the proof, erroring if there's a problem
;; Writes the supplemental files
;; Writes the text that will be incorporated for the #INCLUDE:

(defvar *paper-proofs* (make-hash-table))
(defvar *proofs-noninteractive* nil)
(defvar *org-paper-supplemental-directory* "supplemental")
(defvar *org-paper-includes-directory* "i")
(defvar *org-paper-cache-directory* "proof-cache")
  
;; emacs helper functions. Useful when functions here are called in
;; response to being evaluated as source blocks in org mode
;; top buffer is the one that you are editing when evaluating the source block

(defun eval-in-emacs (form)
  (funcall (intern "EVAL-IN-EMACS" 'swank) form))

(defun emacs-top-buffer-contents ()
  (eval-in-emacs
   '(save-excursion 
     (set-buffer (car (buffer-list (selected-frame)))) 
     (buffer-substring-no-properties
      (point-min) (point-max)))))

(defun emacs-top-buffer-name ()
  (eval-in-emacs '(buffer-name (car (buffer-list (selected-frame))))))

(defun collect-paper-proofs (org-file)
  (setq *paper-proofs* (make-hash-table))
  (with-open-file (f org-file)
    (loop for line = (read-line f nil :eof)
	  until (eq line :eof)
	  when (#"matches" line "#\\+BEGIN_SRC lisp.*")
	    do (let ((*package* (find-package 'logic))
		     (*proofs-noninteractive* t))
		 (loop while (not (char= (peek-char t f) #\#))
		       do 
			  (let ((form (read f)))
			    (when (and (consp form) (member (car form) '(add-proof-here def-logic-axiom define-model paper-defaults)))
			      (eval form))))))))

;; org-file-path-or-string is a string then understand as file name
;; if a list then car is org mode source as string
;; read through and pick up pairs of names and formulas
;; formula can either be a formula-sexp or a formula name
(defun paper-formulas-and-names (org-file-path-or-string &aux key)
  (let ((paper-formulas nil))
    (flet ((doit (stream)
	     (loop for line = (read-line stream nil :eof)
		   until (eq line :eof)
		   when (#"matches" line "#\\+BEGIN_SRC lisp.*")
		     do (let ((*package* (find-package 'logic))
			      (*proofs-noninteractive* t))
			  (loop while (not (char= (peek-char t stream) #\#))
				do 
				   (let ((form (read stream)))
				     (when (and (consp form) (eq (car form) 'paper-defaults)
						(setq key (or (second (member :formula-key form)) key))
						))
				     (when (and (consp form) (member (car form) '(add-formulas-here)))
				       (setq paper-formulas (append  paper-formulas
								     (let ((*org-paper-defaults* (list :formula-key key)))
								       (mapcar 'get-add-formulas-formula
									       (cdr form)))))
				       )))))))
      (if (consp org-file-path-or-string)
	  (with-input-from-string (s (car org-file-path-or-string))
	    (doit s))
	  (with-open-file (f org-file-path-or-string)
	    (doit f)))
      paper-formulas)))

;; if org-file is passed, then the formulas are read from that file and saved
;; to i/formulas.cl
;; Otherwise assume the top buffer is our org file and ask emacs for the current string contents.
(defun write-formulas-clif (&optional org-file)
  (with-open-file (f (ensure-directories-exist
		      (merge-pathnames (make-pathname :directory `(:relative ,*org-paper-supplemental-directory*)
						      :name "formulas"
						      :type "cl")
				       *default-pathname-defaults*)) 
		     :direction :output
		     :if-does-not-exist :create
		     :if-exists :supersede)
    (let ((name-formulas (paper-formulas-and-names
			  (or org-file
			      (list (emacs-top-buffer-contents))))))
      (loop for (label formula) in name-formulas
	    do (format f "(cl:comment 'label:~a')~%" label)
	       (write-string (render :clif (axiom-sexp formula)) f)
	       (terpri f)(terpri f)))))


;; (:foo) -> if paper key
;;            then spec is '((key foo)) and label is key.
;;           otherwise names a formula in which case
;;             :label <label> 
;;             <paperkey> <label>
;; (foo (:forall ...) &rest keys ->
;;           if paper key then if keys has 
;;              <paperkey> <label> 
;;             :label <label> 
;;             otherwise foo 
;;
;; Return either (<label> spec) or (<label> <formula>)

(defun get-add-formulas-formula (e)
  (flet ((explicit-label ()
	   (second (member :label e)))
	 (keyed-name ()
	   (second (member (getf *org-paper-defaults* :formula-key) e))))
    (cond ((keywordp (car e))
	   (let ((spec (if (getf *org-paper-defaults* :formula-key)
			   (list (getf *org-paper-defaults* :formula-key) (car e))))
		 (name (or (explicit-label) (keyed-name) (car e))))
	     (let ((matching-formulas (collect-axioms-from-spec (list spec))))
	       (assert (= (length matching-formulas) 1) ()
		       "Formula key ~a is ambiguous" spec)
	       (list name (keywordify (axiom-name (car matching-formulas)))))))
	  (t (list (or (explicit-label) (keyed-name) (car e))
		    (second e))))))

(defclass paper-proof ()
  ((name :accessor name :initform nil :initarg :name)
   (formula-key :accessor formula-key :initform nil :initarg :formula-key)
   (kind :accessor kind :initform nil :initarg :kind)
   (assumptions :accessor assumptions :initform nil :initarg :assumptions)
   (goal :accessor goal :initarg :goal :initform nil)
   (model :accessor model :initarg :model :initform nil) ; model will be specified by a symbol whose value is the model
   (reasoner :accessor reasoner :initform nil :initarg :reasoner)
   (theorem-text :accessor theorem-text :initform nil :initarg :theorem-text)
   (proof-text :accessor proof-text :initform nil :initarg :proof-text)
   (org-directory :accessor org-directory) 
   (cache-directory :accessor cache-directory :initform nil :initarg :cache-directory)
   (succeeded :accessor succeeded :initarg :succeeded :initform nil)
   (invocation :accessor invocation :initarg :invocation :initform nil)
   (reasoner-arguments :accessor reasoner-arguments :initform nil :initarg :reasoner-arguments)
   ))

(defmethod initialize-instance ((p paper-proof) &rest initargs)
  (apply #'call-next-method p  (append initargs *org-paper-defaults*))
  ;; Default name of file to be included, directory where those are kept, and directory for proof supplemental files
  (setf (org-directory p) *default-pathname-defaults*)
  ;; default the reasoner if not supplied
  (unless (reasoner p)
    (setf (reasoner p) 
	  (ecase (kind p)
	    (:check-sat :z3)
	    (:check-unsat :z3)
	    (:check-model :clausetester)
	    (:prove :prover9)))))

(defun paper-directory-relative (path)
  (merge-pathnames path *default-pathname-defaults*))

(defvar *org-paper-defaults* nil)

(defmacro paper-defaults (&body body)
  `(progn (setq *org-paper-defaults* ',(mapcar 'eval body))
	  (setf (getf *org-paper-defaults* :buffer-name) (emacs-top-buffer-name))
	  (setf (getf *org-paper-defaults* :path-defaults) *default-pathname-defaults*)))

(defvar *org-lisp-requirements-loaded* nil)

(defun ensure-paper-lisp-requirements-loaded ()
  (loop for (key value) on *org-paper-defaults* by #'cddr
	when (and (eq key :load-system)
		  (not (member value *org-lisp-requirements-loaded* :test 'equalp)))
	  do (asdf::oos 'asdf::load-op value)
	     (push value *org-lisp-requirements-loaded*)
	when (and (eq key :load-file)
		  (not (member (truename value) *org-lisp-requirements-loaded* :test 'equalp)))
	  do (load value)
	     (push (truename value) *org-lisp-requirements-loaded*)))
	     
(defmacro add-proof-here (name &rest initargs) ; from kind assumptions theorem-text proof-text goal reasoner model)
  `(progn
     (ensure-paper-lisp-requirements-loaded)
     (let ((proof
	     (setf (gethash ,(keywordify name) *paper-proofs*)
		   (apply 'make-instance 'paper-proof :name ',name ',initargs))))
       (unless *proofs-noninteractive*
	 (maybe-cache-proof-for-paper proof)
	 (write-paper-files proof))
       (format nil "\\input{i/proofs/~a}~%" ,(string-downcase (string name)))
     )))

(defmacro add-formulas-here (&rest formula-descs)
  (let ((names (gensym)))
;    (assert (getf *org-paper-defaults* :formula-key) () "Need to default :formula-key so we know where to get the label formula-key")
    `(progn (ensure-paper-lisp-requirements-loaded)
	    (let ((,names nil))
	      ,@(loop for formula-desc in formula-descs
		      for name = (car formula-desc)
		      for formula-here = (and (consp (second formula-desc)) (second formula-desc))
		      for properties = (if formula-here (cddr formula-desc) (cdr formula-desc))
		      for label = (or (getf properties (getf *org-paper-defaults* :formula-key))
				      (getf properties :label)
				      (and (getf *org-paper-defaults* :formula-key) name)
				      (and (get-axiom name nil)
					   (or 
					    (second (assoc (getf *org-paper-defaults* :formula-key) (axiom-plist (get-axiom name))))
					    (second (assoc :label (axiom-plist (get-axiom name))))
					    )))
		      when formula-here 
			collect
		      `(def-logic-axiom ,@formula-desc)
		      collect `(push (list ',name ',label) ,names))
	      ;;	      \insertFormulaPathTagged{./binary/example-mem-t}{MEMT}
	      (with-output-to-string (s)
		(loop for rnames on (reverse ,names)
		      for (name short) = (car rnames)
		      for skip = (if (null (cdr rnames))
				     "\\vspace*{-\\baselineskip}"
				     "\\vspace*{-\\baselineskip}\\vspace*{-\\baselineskip}")
		      if short
			do (format s "\\insertFormulaPathTagged{./i/formulas/~a.tex}{~a}~a~%" name (string-upcase (string short)) skip)
		      else
			do
			   (format s "\\insertFormulaPath{./i/formulas/~a.tex}~a~%" name skip)
		      do
		      (write-formula-tex (list 
					  (if (getf *org-paper-defaults* :formula-key)
					      (get-axiom-by-key (getf *org-paper-defaults* :formula-key) name)
					      (keywordify name)))
					 (merge-pathnames (make-pathname :directory `(:relative ,*org-paper-includes-directory* "formulas")
									 :type "tex")
							  *default-pathname-defaults*)
					    name)))
	      ))))

(defmethod formula-name-to-reference ((p paper-proof) name)
  (format nil "\\\\ref*{f:~a}" (string-upcase (string name))))
    
(defmethod do-proof-substitutions ((p paper-proof) text)
  (setq text (#"replaceAll" text "[$]assumptions" (format nil "~{~a~^, ~}" 
							  (mapcar (lambda(e) (formula-name-to-reference p e))
								  (assumptions p)))))
  (#"replaceAll" text "[$]goal" (formula-name-to-reference p (goal p)))) 

(defmethod write-tex-file ((proof paper-proof))
  (let ((path 
	  (merge-pathnames (make-pathname :name (string-downcase (string (name proof)))
					  :type "tex" :directory '(:relative "i" "proofs"))
			   (org-directory proof))))
    (ensure-directories-exist path)
    (with-open-file (f path :direction :output :if-does-not-exist :create :if-exists :supersede)
      (write-proof-tex proof f)
      )))

(defmethod write-proof-tex ((proof paper-proof) stream)
  (let ((raw 
	  (with-output-to-string (s)
	    (format s "\\begin{theorem}~%\\label{~a}~%~a~%\\end{theorem}~%" 
		    (name proof)
		    (do-proof-substitutions proof (theorem-text proof)))
	    (format s "\\begin{proof}~%~a (proof by ~a; supplemental files ~a.* ~a) ~%\\end{proof}"
		    (do-proof-substitutions proof (proof-text proof))
		    (string-downcase (string (reasoner proof)))
		    (name proof)
		    (if (not (succeeded proof))
			" FAILED! " ""))
	    )))
    (if (succeeded proof)
	(write-string raw stream)
	(format stream "{\\color{red}~%~a~&}~%" raw))))

(defun md5 (string)
  (format nil "~(~{~2,'0X~}~)"
	(map 'list #'identity (md5::md5sum-string string))))

(defmethod get-formulas ((p paper-proof) formulas)
  (mapcar 'axiom-sexp 
	  (if (formula-key p)
	      (collect-axioms-from-spec `((,(formula-key p) (:or ,@formulas))))
	      (collect-axioms-from-spec `(,@formulas)))))

(defmethod get-formula ((p paper-proof) formula)
  (axiom-sexp (car (get-formulas p (list formula)))))

;; The file names are: input, output, interpretation, model, result, and form
;; The file extensions are either
;;   the reasoner name for input, output, interpretation
;;   "cl" for model
;;   "lisp" for invocation
;;   "txt" for result of the invocation

(defmethod write-proof-results ((p paper-proof) &key input output model interpretation result form)
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
	(setf (cache-directory p) (format nil "~a/~a/" "proofcache" md5 ))
	(ensure-directories-exist (cache-directory p))
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
	   (if (formula-key p)
	       `((,(formula-key p) (:or ,@names)))
	       names))
	 (expect (result what)
	   (if (if (functionp what)
		   (not (funcall what result))
		   (not (eq result what)))
	       (warn (failed-message p result))
	       (setf (succeeded p) t))))
    (let ((reasoner-args (reasoner-arguments p)))
      (if (is-cached p)
	  (format *debug-io* "Getting cached results for ~a~%" (name p))
	  (progn
	    (format *debug-io* "Recomputing ~a ~a~%" (kind p) (name p))
	    (ecase (kind p)
	      (:check-sat
	       (must-be-reasoner p :check-sat :z3)
	       (let* ((form `(z3-find-model ',(keyed (assumptions p)) ,@reasoner-args))
		      (result (eval form)))
		 (expect result (lambda(e) (typep e 'z3-model)))
		 (write-proof-results p :input *last-z3-input* :output *last-z3-output* :result result :form form
					:model (tuples result))
		 ))
	      (:check-unsat 
	       (must-be-reasoner p :check-unsat :z3 :prover9 :vampire)
	       (let* ((form (ecase (reasoner p)
			      (:z3 `(z3-check-satisfiability ',(keyed (assumptions p)) ,@reasoner-args))
			      (:prover9 `(prover9-check-unsatisfiable ',(keyed (assumptions p))))
			      (:vampire `(vampire-check-unsatisfiable ',(keyed (assumptions p))))))
		      (result (eval form)))
		 (expect  result :unsat)
		 (ecase (reasoner p)
		   (:z3 (write-proof-results p :input *last-z3-input* :output *last-z3-output* :result result :form form))
		   (:prover9 (write-proof-results p :input *last-prover9-input* :output *last-prover9-output* :result result :form form))
		   (:vampire (write-proof-results p :input *last-vampire-input* :output *last-vampire-output* :result result :form form)))
		 ))
	      (:prove
	       (must-be-reasoner p :prove :z3 :prover9 :vampire)
	       (let* ((form (ecase (reasoner p)
			      (:z3 `(z3-prove ',(keyed (assumptions p)) ',(keyed (list (goal p))) ,@reasoner-args))
			      (:prover9 `(prover9-prove ',(keyed (assumptions p)) ',(keyed (list (goal p))) ,@reasoner-args))
			      (:vampire `(vampire-prove ',(keyed (assumptions p)) ',(keyed (list (goal p))) ,@reasoner-args))))
		      (result (eval form)))
		 (expect result :proved)
		 (ecase (reasoner p)
		   (:z3 (write-proof-results p :input *last-z3-input* :output *last-z3-output* :result result  :form form))
		   (:prover9 (write-proof-results p :input *last-prover9-input* :output *last-prover9-output* :result result :form form))
		   (:vampire (write-proof-results p :input *last-vampire-input* :output *last-vampire-output* :result result :form form)))
		 ))
	      (:check-model
	       (must-be-reasoner p :check-model :clausetester)
	       (let* ((form `(clausetester-check-model ,(model p) ',(keyed (assumptions p)) ,@reasoner-args))
		      (result (eval form)))
		 (expect result :satisfying-model)
		 (write-proof-results p :interpretation (first *last-clausetester-input*)
					:input (second *last-clausetester-input*)
					:output *last-clausetester-output*
					:model (symbol-value (model p))
					:form form))
	       )
	      ))))))

(defmethod write-paper-files ((p paper-proof))
  (write-tex-file p)
  (flet ((cache-file-named (name)
	   (car (directory (merge-pathnames (make-pathname :name name :type :wild) (cache-directory p)))))) 
    (flet ((copy-one (which &key type (suffix ""))
	     (let* ((file (cache-file-named which)))
	       (when file
		 (let ((new-name (merge-pathnames (make-pathname :name (concatenate 'string (string-downcase (string (name p))) suffix)
								 :type (or type (pathname-type file))
								 :directory `(:relative ,*org-paper-supplemental-directory* "proofs"))
						  (org-directory p))))
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

;; No global for the paper.
;; (defun flush-proof-cache (sys)
;;   '(map nil (lambda(e) (uiop/filesystem:delete-directory-tree e :validate t))
;;        (directory (merge-pathnames "*" (paper-directory-relative "proofcache/")))))

(defun write-formula-tex (spec dest-dir &optional name)
  (ensure-directories-exist dest-dir)
  (let ((g (make-instance 'logic::latex-logic-generator
			  :formula-format "~a"
			  :insert-line-breaks t
			  :with-names nil
			  :prettify-names nil
			  :numbered nil 
			  :write-descriptions nil)))
      (let ((formulas (collect-axioms-from-spec spec)))
	(loop for formula in formulas
	      for path = (merge-pathnames (format nil "~a.tex" (string-downcase (or name (string (axiom-name formula))))) dest-dir)
	      for tex-string = (render-axiom g formula)
	      do
		 (with-open-file (f path :direction :output :if-exists :supersede)
		   (format f "~a~%" tex-string))))))
  
#|
(setq cb (add-proof-here consistent-base
	   :formula-key :ropaper
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
;; formula-key - not essential - used to look up formulas
;; kind - essential sat, unsat, checkmodel, prove 
;; assumptions - essential
;; goal - essential
;; reasoner - essential 
;; theorem-text - not essential. It's cheap to regenerate
;; proof-text - not essential. It's cheap to regenerate
;; ;; directories don't need to be cached.


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
