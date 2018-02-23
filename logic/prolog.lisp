(in-package :logic)

;; helper functions for using paiprolog

;; Load prolog with a set of rules and a set of facts.
;; We take care of two idiosyncracies that interfere with our use.

;; 1) For every ground term we add the fact (domain-element term). This is so that we can guard negation, since the not
;;    predicate fail-if doens't generate bindings. Instead we can use, e.g.,
;;    (and (domain-predicate ?x) (fail-if (f?x)))
;;    to get the right (closed-world) answer.
;; 2) For every predicate that is used in some registered formula (*axioms* table) but which is not used in the set of
;;    facts, we compile a predicate with no clauses, in case the predicate is used in some query.


;; initialize prolog state with a set of rules and facts.
(defun load-prolog (rules facts)
  (clear-prolog-db)
  (loop for prop in facts
	do (prolog-tell prop))
  (let ((universe (remove-duplicates (apply 'append (mapcar 'cdr facts)))))
    (dolist (atom universe)
      (prolog-tell `(domain-element ,atom))))
  (loop for (nil (nil (nil . clauses) head)) in rules ; (label (:implies (:and <clauses>) head))
	do (prolog-tell head clauses))
  (compile-prolog-predicates-for-unused-relations facts)
  )

;; Paiprolog compiles predicates that have been used by facts or rules it's been told. However sometimes we query using
;; a predicate that isn't in any told statement. We expect such a clause to fail, however instead we get an undefined
;; function error.  To fix this, for a set of facts, for any given formula (default all *axioms*) if any predicate in
;; the formula isn't used in the facts, compile a prolog predicate function for it.

(defun compile-prolog-predicates-for-unused-relations (facts &optional (formulas (mapcar 'axiom-sexp (alexandria::hash-table-values *axioms*))))
  (let ((predicates-in-db (formula-elements `(:and ,@facts)))) ;; compute this because paiprolog doesn't keep a list of predicate/arity 
    (let ((predicates-in-formulas (formula-elements `(:and ,@formulas))))
      (loop for (predicate arity)
	      in (set-difference predicates-in-formulas predicates-in-db :test 'equalp)
	    when (not (fboundp (paiprolog::make-predicate predicate arity)))
	      do (let ((extensions:*suppress-compiler-warnings* t))
		   (paiprolog::compile-predicate predicate arity nil))))))

;; reset to empty the prolog db
(defun clear-prolog-db ()
  (paiprolog::clear-db))

;; Helper for <- 
(defun prolog-tell (head &optional clauses)
  (flet ((fix-keywords (form)
	   (tree-replace (lambda (e) (if (keywordp e) (intern (string e)) e)) form)))
    (eval `(paiprolog::<- ,(fix-keywords head) ,@(fix-keywords clauses)))))

