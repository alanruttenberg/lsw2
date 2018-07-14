(in-package :logic)

;; 1. Takes a spec for theory
;; 2. Computes rules if necessary.
;; 3. Expands model using rules.
;; 4. Checks all formulas against the expanded model
;; 5. Collect any failing formulas other than <expect-failing-formulas>
;; 6. Collect any <expect-propositions> that aren't in the expanded model
;; 7. Collect any <unexpected-propositions> that *are* in the model
;; 8. Returns either t if pass, otherwise a plist with keys
;; :failing-formulas - any formula collected in 5
;; :missing-propositions - any proposition collected in 6
;; :found-propositions - any proposition collected in 7


(defvar *expanded-models* (make-hash-table :weakness :value :test #'eql))
(defvar *expanded-model-counter* 0)
(defvar *last-checked-seed* nil)
(defvar *last-checked-spec* nil)
(defvar *last-expanded-model* nil)
;; If everything ok, return t
;; Otherwise return information about failures

(defun check-theory-with-model-seed (theory model &key expect-failing-formulas expect-propositions unexpected-propositions print-debug? expect-unsat label saveas)
  (let ((rules (rules-for-spec theory)))
    (setq *last-checked-seed* model)
    (setq *last-checked-spec* theory)
    (let* ((expanded (setq *last-expanded-model* (expand-seed model rules)))
	   (inverses (multiple-value-list (inverses-from-spec theory)))
	   (theory-formula-names (mapcar 'axiom-name (collect-axioms-from-spec theory)))
	   (failed (mapcar 'keywordify (second (multiple-value-list (evaluate-formulas theory expanded :binary-inverses (car inverses) :ternary-inverses (second inverses))))))
	   (non-theory-expected-failures (loop for f in 
						     (set-difference expect-failing-formulas theory-formula-names :test 'equalp)
					       when (not (evaluate-formula (axiom-sexp f) expanded)) collect f))
	   (reportable-failed-formulas (set-difference (union failed non-theory-expected-failures) expect-failing-formulas))
	   (reportable-expected-propositions (set-difference expect-propositions expanded :test 'equalp))
	   (reportable-unexpected-propositions (intersection unexpected-propositions expanded :test 'equalp))
	   (reportable-should-have-failed-formulas (set-difference expect-failing-formulas (union failed non-theory-expected-failures)
								   :test 'string-equal))
	   )
      (or (and expect-unsat (check-unsat theory model))
	  (if (or reportable-failed-formulas reportable-expected-propositions reportable-unexpected-propositions reportable-should-have-failed-formulas)
	      (if (and print-debug? reportable-failed-formulas)
		  (progn 
		    (terpri)
		    (princ "(" )
		    (map nil (lambda(e) (print `(why-failed? ,e)))
			 reportable-failed-formulas)
		    (print `(:found-propositions ,@reportable-unexpected-propositions))
		    (print `(:missing-propositions ,@reportable-expected-propositions))
		    (princ ")" )
		    (values))
		  `(:failing-formulas ,reportable-failed-formulas
		    :should-have-failed-formulas ,reportable-should-have-failed-formulas
		    :unexpected-propositions ,reportable-unexpected-propositions
		    :missing-propositions ,reportable-expected-propositions
		    ,@(if expect-unsat '(:unsat nil ))
		    ))
	      t)))))

(defun why-failed? (axiom &optional (seed *last-checked-seed*) (spec *last-checked-spec*))
  (pprint (evaluate-formula (axiom-sexp axiom)
			    (expand-seed seed (rules-for-spec spec))
			    :trace t :return-annotated t)))

;(def-logic-axiom my-test (:forall (?a) (:implies (x ?a) (y ?a))) "for testing" :kind :test-testing)

'(check-theory-with-model-seed
 '(:my-test)
 '((x a) (y c))
 :expect-failing-formulas nil
 :expect-propositions '((y a))
 :unexpected-propositions '((y c)))


 
;(with-open-file (f "/tmp/test" :direction :output)
					;      (write-string (ladr-write-positive-ground-model (expand-seed (base-model) (rules-for-spec time-testing-spec)) time-testing-spec 'string) f))
;(render :prover9 time-testing-spec)

(defvar *theory-checks* (make-hash-table))

(defclass theory-check-with-seed ()
  ((name :accessor name :initarg :name :initform nil)
   (spec :accessor spec :initarg :spec :initform nil)
   (seed :accessor seed :initarg :seed :initform nil)
   (expect-failing-formulas :accessor expect-failing-formulas :initarg :expect-failing-formulas :initform nil)
   (expect-propositions :accessor expect-propositions :initarg :expect-propositions :initform nil)
   (unexpected-propositions :accessor unexpected-propositions :initarg :unexpected-propositions :initform nil)
   (expect-unsat :accessor expect-unsat :initarg :expect-unsat :initform nil)
   (result :accessor check-result :initarg :result :initform nil)
   ))

(defmacro def-check-theory-with-model-seed (name spec seed &key expect-failing-formulas expect-propositions unexpected-propositions expect-unsat figure &allow-other-keys)
  `(let ((it (make-instance 'theory-check-with-seed
		  :name ',name
		  :spec ,spec
		  :seed ,seed
		  :expect-failing-formulas ,expect-failing-formulas
		  :expect-propositions ,expect-propositions
		  :unexpected-propositions ,unexpected-propositions
		  :expect-unsat ,expect-unsat)))
     (setf (gethash ',name *theory-checks*) it)
     (when ,figure (print (cons ',name ,figure)))))
		  
(defmethod run-check ((c theory-check-with-seed))
  (setf (check-result c)
	(check-theory-with-model-seed (spec c) (seed c)
				      :expect-failing-formulas (expect-failing-formulas c)
				      :unexpected-propositions  (unexpected-propositions c)
				      :expect-propositions (expect-propositions c)
				      :expect-unsat (expect-unsat c))))

;; given a proposition, a spec, and a seed, use the theory to prove the proposition, and if proved, return the proof support

(defun explain-inference (prop &optional (spec *last-checked-spec*) (seed *last-checked-seed*))
  (let ((axs (collect-axioms-from-spec spec))
	(facts (mapcar (lambda(e) `(:fact ,e)) seed)))
    (and (prover9-prove (append axs facts) `(:fact ,prop) :timeout 120)
	 (let ((support (get-proof-support)))
	   (loop for sup in support
		 for formula = (axiom-sexp sup)
		 if (eq (car formula) :fact) collect (second formula)
		   else collect sup)))))

(defun check-unsat (&optional (spec *last-checked-spec*) (seed *last-checked-seed*))
  (let ((axs (collect-axioms-from-spec spec))
	(facts (mapcar (lambda(e) `(:fact ,e)) seed)))
    (eq :unsat (prover9-check-unsatisfiable  (append axs facts)))))
