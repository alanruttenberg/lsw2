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

;; If everything ok, return t
;; Otherwise return information about failures

(defun check-theory-with-model-seed (theory model &key expect-failing-formulas expect-propositions unexpected-propositions print-debug?)
  (let ((rules (rules-for-spec theory)))
    (let* ((expanded (expand-seed model rules))
	   (failed (mapcar 'keywordify (second (multiple-value-list (evaluate-formulas theory expanded)))))
	   (reportable-failed-formulas (set-difference failed expect-failing-formulas))
	   (reportable-expected-propositions (set-difference expect-propositions expanded :test 'equalp))
	   (reportable-unexpected-propositions (intersection unexpected-propositions expanded :test 'equalp)))
      (setf (gethash (incf *expanded-model-counter*) *expanded-models*) expanded)
      (if (or reportable-failed-formulas reportable-expected-propositions reportable-unexpected-propositions)
	  (if (and print-debug? reportable-failed-formulas)
	      (progn 
		(terpri)
		(princ "(" )
		(map nil (lambda(e) (print `(why-failed? ,e :model *expanded-model-counter*)))
		     reportable-failed-formulas)
		(print `(:found-propositions ,@reportable-unexpected-propositions))
		(print `(:missing-propositions ,@reportable-expected-propositions))
		(princ ")" )
		(values))
	      `(:failing-formulas ,reportable-failed-formulas
		:unexpected-propositions ,reportable-unexpected-propositions
		:missing-propositions ,reportable-expected-propositions))
	  t))))

(defun why-failed? (axiom seed spec)
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


 




