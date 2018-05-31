(in-package :logic)

(defvar *count* 0)

;; Rename quantified-over variable so that each is distinct
(defun normalize-qvars (form &optional (already (make-hash-table)))
  (if (and (consp form)
	   (member (car form) '(:forall :exists)))
      (loop for var in (second form)
	    with body = (third form)
	    for new = (if (gethash var already)
			  (intern (concatenate 'string (string var) "-" (prin1-to-string (gethash var already))))
			  var)
	    do (incf (gethash new already 0))
	       (when (not (eq new var))
		 (setq body (subst new var body))) 
	    collect new into vars
	    finally (return `(,(car form) ,vars 
			      ,(normalize-qvars body already))))
      (if (and (consp form) (keywordp (car form)))
	  `(,(car form) ,@(mapcar (lambda(e)(normalize-qvars e already)) (rest form)))
	  form)))

;; replace all facts anywhere in the formula with an expression that checks whether the fact is in the kb.  Assumes
;; variables will be bound at the point of evaluation.  The format of the KB is a equalp hash table of vectors for each
;; fact.  Here a vector us stuffed with the predicate and either the ground terms or the bound (at this point) variable
;; and then a check is done to see whether the fact is in the kb. Note that we need to be careful to avoid keeping the
;; vector around before checking the KB so that it doesn't get written over by some other evaluation.

;; A form is determined to be a fact (other than in the case of :forall-enumerated) when the first element of the
;; list is a non-keyword symbol.

;; In the case of a :forall-enumerated the bindings satisfy this criterion - that looks like
;; (:forall-enumerated (?x ?y) '((john mary) (sam jane)...)
;; So we watch out for that and don't touch those bdingins.

;; Tracing, if called for (trace? = t), is implemented in a vector with an element for each variable.  Each time a
;; proposition is about to be checked the appropriate places in the trace vector for the variables used are set to the
;; value at that time. The value therefore reflect the values of the quantified variables, the last time they were used.
;;
;; Returns three values:
;; 1) A form suitable to be evaluated in lisp

;; 2) A list of the vectors needed - one for each arity of a predicate evaluated. Vector are returned as a list of names
;; vec-<n> where n is the length of the vector. These will be allocated at the top of the 

;; 3) A list of variables that were in this form, for tracing purposes, in the order their values appear in the trace vector.

;; Maybe TBD: The tracing might be better, perhaps recording the last form evaluated rather than the variables. We might
;; also null the variables at the end of a loop.

(defun convert-to-check (formula trace?)
  (let ((needed-vecs nil)
	(tracevars nil))
    (labels ((varpos (var)
	     (or (position var tracevars)
		 (progn (setq tracevars (append tracevars (list var)))
			(1- (length tracevars)))))
	   (replace (form)
	     (tree-replace (lambda(form)
			     (if (and (consp form)
				      (eq (car form) :forall-enumerated))
				 `(:forall-enumerated ,(second form)
						      ,(replace (third form)))
				 (if (and (consp form)
					  (not (keywordp (car form)))
					  (not (logic-var-p (car form))))
				     (let ((vec (intern (format nil "VEC-~a" (length form)))))
				       (pushnew (length form) needed-vecs)
				       `(progn
					  ,@(loop for el in form
						  for i from 0
						  for is-var = (logic-var-p el)
						  if (and is-var trace?)
						    collect `(setf (svref trace ,(varpos el)) ,el) 
						  if is-var
						    collect `(setf (svref ,vec ,i) ,el)
						  else
						    collect `(setf (svref ,vec ,i) ',el))
					  (if *abort* (abort))
					  (incf *count*)
					  (gethash ,vec kb)))
				     form)))
			   form)))
      (values
       (replace (normalize-qvars formula))
       needed-vecs
       tracevars))))

(defvar *abort* nil)

;; Generates the code to handle the logic primitives. Makes some effort to be efficient except in the case of the
;; looping for forall-enumerated. The effort to make quick code is for the worse case, where you have to iterate over
;; all cases, for example: (:exists (?a ?b ?c ?d ?e) (p ?a ?b ?c ?d ?e)) where p is never true, in which case we have to
;; do <kb size> ^ 5 in this case. kb size of 1000 yields a billion evaluations, which adds up.

;; Cases:
;; :forall - loop through all values of the variable, immediately returning nil if the body fails, otherwise returning t
;;   if we make it all the way through.
;; :exists - loop through all values of the variable, immediately returning t if the body succeeds, otherwise returning
;;   nil if we make it all the way through the loop.
;; :forall-enumerated - successively bind the variables to the pre-computed values, acting same as :forall 
;; :and - successively check each of the conjuncts immediately returning nil if any fails, otherwise t
;; :or - successively check each of the disjuncts immediately returning t if any succeeds, otherwise nil
;; :implies - evaluates antecedent, return t immediately if it fails, otherwise returns value of consequent.
;; :iff - evaluates each side returning t if they both evaluate to the same value
;; :fact - Shouldn't really happen, but just in case, evaluates the second element of the form
;; :not - evaluates and returns nil if t, t if nil

;; example 	 
;; (let ((*print-right-margin* 120))
;;   (pprint
;;    (convert-logic 
;;     (convert-to-check
;;      '(:forall (?x ?t)
;; 	 (:not (:and (instance-of ?x continuant ?t) (:= ?x ?t))))
;;      nil))))
;; ->

(defun convert-logic (formula &optional tracevars)
  (if (atom formula)
      formula
      (case (car formula)
	(:forall
	    (let ((vars (second formula))
		  (body (third formula)))
	      (if (= (length vars) 1)
		  (let ((label (gensym)))
		    `(let ((success t))
		       (tagbody 
			  (dolist (,(car vars) universe)
			    (unless ,(convert-logic body tracevars)
			      (setq success nil)
			      (go ,label))
			    ,@(if tracevars `((setf (svref trace ,(position (car vars) tracevars)) nil)) nil))
			  ,label)
			success))
		  (convert-logic
		   `(:forall (,(car vars))
		      (:forall (,@(cdr vars))
			,body))
		   tracevars))))
	(:forall-enumerated
	 (let ((vars (car (second formula)))
	       (bindings (cdr (second formula)))
	       (body (third formula)))
	   `(loop for ,vars in ',bindings
		 always ,(convert-logic body tracevars))))
	(:exists
	    (let ((vars (second formula))
		  (body (third formula)))
	      (if (= (length vars) 1)
		  (let ((label (gensym)))
		    `(let ((success nil))
		       (tagbody 
			  (dolist (,(car vars) universe)
			    (when ,(convert-logic body tracevars)
			      (setq success t)
			      (go ,label))
			    ,@(if tracevars `((setf (svref trace ,(position (car vars) tracevars)) nil)) nil)
			    )
			  ,label)
		       success))
		  (convert-logic
		   `(:exists (,(car vars))
		      (:exists (,@(cdr vars))
			,body)) tracevars))))
	(:and
	 (let ((label (gensym)))
	   `(let ((success t))
	      (tagbody
		 ,@(loop for conjunct in (cdr formula)
			 collect `(unless ,(convert-logic conjunct tracevars)
				    (setq success nil)
				    (go ,label)))
		 ,label)
	     success)))
	(:or 
	 (let ((label (gensym)))
	   `(let ((success nil))
	      (tagbody
		 ,@(loop for conjunct in (cdr formula)
			 collect `(when ,(convert-logic conjunct tracevars)
				    (setq success t)
				    (go ,label)))
		 ,label)
	      success)))
	(:implies
	    (let ((ant (gensym))
		  (cons (gensym)))
	      `(let* ((,ant ,(convert-logic (second formula)))
		      (,cons (if ,ant ,(convert-logic (third formula) tracevars))))
		 (or (not ,ant) ,cons))))
	(:iff 
	    (let ((ant (gensym))
		  (cons (gensym)))
	      `(let ((,ant ,(convert-logic (second formula) tracevars))
		     (,cons ,(convert-logic (third formula) tracevars)))
		 (eq ,ant ,cons))))
	(:fact 
	 (convert-logic (second formula) tracevars))
	(:distinct 
	 t)
	(:=
	 (let ((a (second formula))
	       (b (third formula)))
	   `(eq ,(if (logic-var-p a) a `(quote ,a))
		,(if (logic-var-p b) b `(quote ,b)))))
	(:not 
	    `(not ,(convert-logic (second formula) tracevars)))
	(otherwise formula))))

;; Optimization of the top loop (in many cases)
;; ****************************************************************
;; In deeply nested axioms the time to check was getting onerous. Case: 70 individuals led to worst case 1.6 billion
;; checks for 5 nested quantified variables. This code speeds it up by making the observation that in the common case of
;; an axiom starting with (forall (...) (implies ( .. ) ...., if the antecedent is false, the consequent is true, which
;; means we really only need to test the cases where the antedent is true

;; In many cases the antecedent is only true for relatively few bindings. We solve for those bindings by transforming
;; the antecedent prolog query and only iterate over successful bindings.

;; Example
;; (:forall (?o1 ?o2 ?t1 ?t2)
;;   (:implies (:and (:exists (?t) (:or (instance-of ?o1 process ?t)(instance-of ?o1 process-boundary ?t)))
;; 		  (:exists (?t) (instance-of ?o2 process ?t)))
;;       ...))

;; The antecedent is transformed into the following prolog query:
;; (and (or (instance-of ?o1 process ?t) (instance-of ?o1 process-boundary ?t))
;;      (instance-of ?o2 process ?t-1))

;; The result of the query finds 10 binding sets.
;; This compared to a worst-case (expt 70 5) ~ 1.6 billion

;; Many checks are not this complicated, but the worst case can currently take a few minutes - even though I parallelize
;; the checks, the time is dominated by the slowest check. This is only an optimization if the time for computing the
;; bindings is less than the time it would take to iterate. Currently I do it in all cases, but it might be worth only
;; doing it in cases where there is sufficient nesting / sufficiently large kb.

;; For the set I've been working on, this optimization does seem to have been worth the effort with about 14x
;; speedup. The relevance computation will do this check with each of the ~1200 assertions in the model removed one at a
;; time. With this speedup the computation will take 10 hours instead of 6 days. CPU utilization isn't 100% yet, despite
;; parallelization, so it could be less than 10 hours if we're lucky.

;; bfo> (check-expanded-model)&
;; %2 finished in 32.66 seconds (check-expanded-model)

;; bfo> (check-expanded-model)&
;; %2 finished in 441.56 seconds (check-expanded-model)

;; This checks for the optimizable pattern - a top level forall, over an implication, where the implication doesn't
;; itself contain a forall.
;; TBD: Extend to bi-implication, taking the union of the computed bindings.

(defun implies-pattern-conservative (form &optional vars)
  (if (and (consp form) (eq (car form) :forall))
      (implies-pattern-conservative (third form) (union vars (second form)))
      (if (and (consp form)
	       (eq (car form) :implies)
	       (not (tree-find :forall (second form)))
	       )
	  (values vars (second form) (third form))
	  nil)))

;; Not needed?
(defun just-the-props (form)
  (if (consp form)
      (case (car form)
	((:and :or :implies :iff :not) (mapcan 'just-the-props (rest form)))
	((:forall :exists) (mapcan 'just-the-props (cddr form)))
	(otherwise (list form)))))

;; not needed?
(defun kb-used (kb predicate position negate?)
  (loop for pred in kb
	with them
	when (eq (car pred) predicate)
	  do (pushnew (nth position pred) them)
	finally
	   (return
	     (if negate?
	       (set-difference (remove-duplicates (apply 'append (mapcar 'cdr kb))) them)
	       them))))

;;not needed?
(defun forall-scope (kb var form negate)
  (if (consp form)
      (case (car form)
	((:and :or :iff :implies) (apply 'append (mapcar (lambda(e) (forall-scope kb var e negate)) (cdr form))))
	((:not) (forall-scope kb var (second form) (not negate)))
	((:forall :exists :=) nil)
	(otherwise  (if (position var form)
			(kb-used kb (car form) (position var form) negate)
			(remove-duplicates (apply 'append (mapcar 'cdr kb))))))))

;; how deep is the deepest quantifier nesting
(defun quantified-depth (axiom)
  (case (car axiom)
    ((:forall :exists) (+ (length (second axiom)) (quantified-depth (third axiom ))))
    ((:and :or not :implies :iff) (apply 'max (mapcar 'quantified-depth (rest axiom))))
    (otherwise 0)))

;; Return all the variables in form 
(defun variables-in (form)
  (let ((them nil))
    (if (atom form)
	(when (and (symbolp form) (char= (char (string form) 0) #\?)) (push form them))
	(tree-walk form (lambda(e) (when (and (symbolp e) (char= (char (string e) 0) #\?)) (pushnew e them)))))
    them))

(defun rewrite-to-iterate-over-pattern-bindings (vars implication)
  ;; expect implies pattern has only :and :or :not :exists :=
  ;; get the implies pattern.
  ;; change enclosed existentials to anonymous variables
  ;; strip existentials
  ;; Figure out which quantified variables are actually in the implies-pattern - if not in, then it's a regular forall.
  ;; in any clause, push negation to bottom and add domain-element _ for _ any variable in the negation.
  ;; query
  (let* ((standardized (normalize-qvars implication))
	 (vars-in-implication (variables-in (second standardized)))
	 (to-solve (intersection vars-in-implication vars))
	 (to-iterate (set-difference vars to-solve)) 
	 (query (rewrite-as-prolog-phase-2 (move-negation-and-simplify (rewrite-as-prolog-phase-1 (second standardized)))))
	 (bindings (eval `(paiprolog::prolog-collect ,to-solve ,query))))
    `(:forall-enumerated (,(if (= (length to-solve) 1) (car to-solve) to-solve) ,@bindings)
       (,@(if to-iterate (list :forall to-iterate) (list :and))
        (:implies ,(second implication) ,(third implication))))))

;; First phase of the rewrite to prolog
;; Translate our keywords to prolog builtins or things which will be translated away in phase-2
;; Remove existentials (the whole query is an existential)
(defun rewrite-as-prolog-phase-1 (form)
  (tree-replace (lambda(el)
		  (if (and (consp el)
			   (eq (car el) :exists))
		      (rewrite-as-prolog-phase-1 (third el))
		      el))
		(tree-replace (lambda(el)
				(case el
				  (:and 'and)
				  (:or 'or)
				  (:= '==)
				  (:iff 'iff)
				  (:implies 'if)
				  (:not 'not)
				  (otherwise
				   (assert (or (eq el :exists) (not (keywordp el))) (el) "Didn't expect a keyword here - how did ~a get through?" el)
				   el)))
			      form)))

;; Phase 2 of the rewrite to prolog. Here we have to take some care. Once done we will have a query that only uses the
;; 'and and 'or and 'fail-if prolog built-ins.
;;
;; In conjunctions, reorder the conjuncts pushing the negations to the bottom. The later the negation is, the more of a
;; chance some other conjunct wil bind it's variables.
;; e.g. (and (not (f ?x)) (g ?x) (j ?x)) -> (and (g ?x) (j ?x) (not (f ?x)))
;; If the negation was first the above would immediately fail. Coming at the end it acts as a filter.
;;
;; For negated clauses inside a conjunction, since fail-if/negation doesn't bind, we need to be sure all
;; bindings are available.  We look at the previous clauses to see what variables are already bound, and the, for each
;; variable left add a (domain-element ?x) conjunct - ensuring the variable will be bound. I didn't find a prolog
;; builtin that was true for any argument to any predicate, so in preparation of the database we assert it for every
;; element of the universe.
;; e.g. (and (g ?x) (j ?y)  (not (f ?x ?y ?z)))
;;  In the above, ?z isn't yet bound, so this is written to:
;;  (and (g ?x) (j ?y)  (domain-element ?z) (fail-if (f ?x ?y ?z)))

;; For disjunctions and singular negations we need to bind every variable in a negation since it may be the only clause
;; that succeeeds.  e.g. (or (not (f ?x ?y)) (g ?x)) -> (or (and (domain-element ?x) (domain-element ?y) (fail-if (f ?x
;; ?y))) (g ?x)) This could, in the worse case, be very bad e.g (not (f ?a) (g ?b) (h ?c) (i ?d)) could effectively
;; iterate over (kb size ^ 4 ) potential bindings.
;; In practice this hasn't happened - the query has other elements that constrain the bindings by the time we get to the negation.

;; if and iff are reduced to and/or before being further rewritten.

;; There is a problem with predicates that aren't yet in the KB - paiprolog hasn't compiled a function for them and so
;; instead of just failing it gives an unknown function error. Handled now earlier in the process when loading the
;; prolog db.

(defun rewrite-as-prolog-phase-2 (form &optional already-bound)
  (flet ((guarded (to-protect form)
	   (and form
		(append (loop for v in to-protect collect `(domain-element ,v))
			(list (list* (if (eq (car form) 'not) 'fail-if (car form)) (cdr form)))))))
    (cond ((and (consp form) (eq (car form) 'and))
	   (let* ((bound-here (remove-if (lambda(e) (member e '(not and or))) (rest form) :key 'car))
		  (positives (mapcar (lambda(e) (rewrite-as-prolog-phase-2 e (variables-in bound-here))) (remove 'not (rest form) :key 'car)))
		  (negative (mapcar (lambda(e) `(paiprolog::fail-if ,(second e)))
				    (remove 'not (rest form)  :test-not 'eq :key 'car)))
		  (free-in-negative (set-difference (set-difference (variables-in negative) (variables-in bound-here)) already-bound)))
	     `(and ,@positives ,@(guarded free-in-negative (car negative)) ,@(cdr negative))))
	  ((and (consp form) (eq (car form) 'or))
	   `(or 
	     ,@(loop for form in (rest form)
		      if (eq (car form) 'not)
			    collect `(and ,@(guarded (set-difference (variables-in form) already-bound) form))
		     else collect (rewrite-as-prolog-phase-2 form already-bound))))
	  ((and (consp form) (eq (car form) 'not))
	   `(and ,@(guarded (variables-in (second form)) form)))
	  (t form))))

;; Move negation inwards using !(a & b) -> (!a | !b), !( a | b) -> !a & !b
;; collapse nested ands, nested ors
;; Translate if, iff to their and/or versions
(defun move-negation-and-simplify (form)
  (if (and (consp form) (consp (second form)))
      (destructuring-bind (outer . ((inner . args) . clauses)) form
	(cond ((and (member outer '(:not not)) (member inner '(:and and)))
	       (move-negation-and-simplify `(or ,@(mapcar (lambda(e) (move-negation-and-simplify`(not ,e))) args)))
	       )
	      ((and (member outer '(:not not)) (member inner '(:or or)))
	       (move-negation-and-simplify `(and ,@(mapcar (lambda(e) (move-negation-and-simplify`(not ,e))) args)))
	       )
	      ((and (member outer '(:not not)) (member inner '(:not not)))
	       (move-negation-and-simplify(car args)))
	      ((and (member outer '(:and and)))
	       `(and ,@(loop for conjunct in (cons `(,inner ,@args) clauses)
			     if (member (car (move-negation-and-simplify conjunct)) '(:and and))
			       append (mapcar 'move-negation-and-simplify(cdr conjunct))
			     else collect (move-negation-and-simplify conjunct))))
	      ((and (member outer '(:or or)))
	       `(or ,@(loop for disjunct in (cons `(,inner ,@args) clauses)
			    when (member (car (move-negation-and-simplify disjunct)) '(:or or))
			      append (mapcar 'move-negation-and-simplify(cdr disjunct))
			    else collect disjunct)))
	      ((member outer '(if :implies))
	       (move-negation-and-simplify `(or ,(move-negation-and-simplify (second form)) ,(move-negation-and-simplify `(not ,(third form))))))
	      ((member outer '(iff :iff))
	       (move-negation-and-simplify `(and (or ,(move-negation-and-simplify (second form)) (not ,(move-negation-and-simplify (third form)))) (or ,(move-negation-and-simplify (third form)) (not ,(move-negation-and-simplify (second form)))))))
	      (t form)))
      form))

;;(implies-pattern '(:forall (?x ?y ?z) (:implies (:and (foo ?z) (bar ?y)) (bif ?z))))
;;(paiprolog::prolog-collect (?answer) (setof ?a (and (instance-of ?a ?x ?t) (fail-if (instance-of ?a spatial-region ?t))) ?answer))

;; (implies-pattern-conservative '(:forall (?c ?r ?t) ;; failure c=m r=space t=1dt-2
;; 		   (:implies
;; 		       (:and  (instance-of ?c independent-continuant ?t)
;; 			      (:not (instance-of ?c spatial-region ?t))
;; 			      (instance-of ?r spatial-region ?t)
;; 			      (instance-of ?t temporal-region ?t)
;; 			      )
;; 		       (win ?x))))

;; -> (:and (instance-of ?c independent-continuant ?t) (:not (instance-of ?c spatial-region ?t)) (instance-of ?r spatial-region ?t) (instance-of ?t temporal-region ?t))
;; (paiprolog::prolog-collect (?c ?t ?t) 
;; 				       (instance-of ?c independent-continuant ?t) 
;; 				       (fail-if (instance-of ?c spatial-region ?t))
;; 				       (instance-of ?r spatial-region ?t)
;; 				       (instance-of ?t temporal-region ?t)) 

;; (:forall (?a) (:exists (?b) ...))

	  
;; ;; issue - axioms that don't 
;; (loop for ax in (rewrite-inverses (collect-axioms-from-spec *everything-theory*) :copy-names? t :binary-inverses *inverse-binary-relation-pairs* :ternary-inverses *inverse-ternary-relation-pairs*)
;; 	   do (multiple-value-bind (vars ant cons) 
;; 		  (logic::implies-pattern-conservative  (axiom-sexp ax))
;; 		(print-db (axiom-name ax))
;; 		(when (and vars ant)
;; 		  (logic::implies-pattern-bindings vars `(:implies ,ant ,cons)))))

(defun rewrite-inverses (spec &key copy-names? binary-inverses ternary-inverses)
  "Rewrites the formulas in spec to rewrite inverse relationships to use only 1. Inverses are specified as pairs in arguments to :binary-inverses :ternary-inverses. With copy-names? t returns a list of axioms with the same names as the originals. Without just returns the sexps"
  (let ((axioms (collect-axioms-from-spec spec)))
    (loop for ax in axioms
	  for rewritten-sexp in
	      (mapcar (lambda(e) (if (keywordp (car e)) e `(:fact ,e)))
		      (render-axioms (make-instance 'logic-generator 
						    :binary-inverses binary-inverses
						    :ternary-inverses ternary-inverses)
				     (collect-axioms-from-spec spec)))
	  collect
	  (if copy-names?
	      (make-instance 'axiom :sexp rewritten-sexp :name (axiom-name ax))
	      rewritten-sexp))))

;; Walk down a form in which bindings have been substituted, evaluating each subform
;; Facts are translated to either (:true <fact>) or (:false <fact>)
;; Expressions (forms starting with a keyword) are translated to
;; style=:nested same as other forms
;; style=:right-of-keyword ((<keyword> :true) ... ) or ((<keyword> :false) ... )
;; style=:left-of-keyword ((:true <keyword>) ... ) or ((:false <keyword>) ... )
;; style=:hyphen (<keyword>-true ...) or  (<keyword>-false ...)
(defun annotate-lossage (form model &optional (style :hyphen))
  (flet ((annotate-one (form status)
	   (ecase style
	     (:nested (list status form))
	     (:hyphen (if (keywordp (car form)) (list* (intern (concatenate 'string (string (car form)) "-" (string status)) 'keyword) (cdr form))))
	     (:left-of-keyword (if (keywordp (car form)) (list* (list status (car form)) (cdr form)) (list status form)))
	     (:right-of-keyword (if (keywordp (car form)) (list* (list (car form) status) (cdr form)) (list status form))))))
    (if (some 'logic-var-p form)
	form
	(let ((status (if (evaluate-bound form model) :true :false)))
	  (cond ((member (car form) '(:forall :exists))
		 (if (some 'logic-var-p (second form))
		     (annotate-one form status)
		     (annotate-one (list (car form) (second form) (annotate-lossage (third form) model)) status)))
		((member (car form) '(:and :or :iff :implies :not))
		 (annotate-one (list* (car form) (mapcar (lambda(e) (annotate-lossage e model)) (cdr form))) status))
		(t (list status form)))))))

;; take a formula in which a set of bindings has been substituted and evaluate it.
;; This consists of evaluating after removing constants where a quantifying variable of a forall or exists 
(defun evaluate-bound (form model)
  (labels ((thin-quantifiers (form)
	     (if (consp form)
		 (if (member (car form) '(:forall :exists))
		     (let ((qvars (remove-if-not 'logic-var-p (second form))))
		       (if (null qvars)
			   (thin-quantifiers (third form))
			   `(,(car form) ,qvars
			     ,(thin-quantifiers (third form)))))
		     (if (member (car form) '(:and :or :iff :implies :not))
			 `(,(car form) ,@(mapcar #'thin-quantifiers (cdr form)))
			 form))
		 form)))
    (evaluate-formula (thin-quantifiers form) model)))

(defun evaluate-formula (formula interpretation &key trace show-form return-annotated (implies-optimize nil))
  "Take a logical formula and test against an interpretation given as the list of positive propositions. 
Works by transforming the formula into a function, compiling it, and running it. The list macros are used 
to avoid consing, which can land up be quite a lot"
  (when (keywordp formula) (setq formula (axiom-sexp formula)))
  (let ((paiprolog::*trail*  (make-array 200 :fill-pointer 0 :adjustable t)))
    (when (and implies-optimize (or (not (numberp implies-optimize)) (>= (quantified-depth formula) implies-optimize)))
      (multiple-value-bind (vars ant cons) 
	  (logic::implies-pattern-conservative  formula)
	(when (and vars ant)
	  (setq formula (rewrite-to-iterate-over-pattern-bindings vars `(:implies ,ant ,cons))))))
    (multiple-value-bind (converted needed-vecs tracevars) (convert-to-check formula trace)
      (let ((trace-array (and trace (make-array (length tracevars)))))
	(setq @ trace-array)
	(let ((fun `(lambda (universe kb &optional trace)
		      (declare (optimize (speed 3) (safety 0)))
		      (declare (ignorable universe kb trace))
		      (let ,(loop for size in needed-vecs
				  collect
				  (list (intern (format nil "VEC-~a" size))
					`(make-array ,size)))
			,(convert-logic converted tracevars))))
	      (kb (make-hash-table :test 'equalp))
	      (universe (remove-duplicates (apply 'append (mapcar 'cdr interpretation)))))
	  (setq *count* 0)
	  (setq *abort* nil)
	  (dolist (prop interpretation) (setf (gethash (coerce prop 'simple-vector) kb) t))
	  (if show-form
	      (let ((*print-right-margin* 200)) (pprint (ext:macroexpand-all fun)))
	      (let* ((compiled (compile nil fun))
		     (evaluation (funcall compiled universe kb trace-array)))
		(if (and trace (not evaluation))
		    (let* ((trace-bindings (remove nil (map 'list 'cons tracevars trace-array) :key 'cdr))
			   (annotated (annotate-lossage (if trace-bindings
							    (paiprolog::subst-bindings trace-bindings (normalize-qvars formula))
							    formula)
							interpretation)))
		      (unless return-annotated 
			(pprint annotated))
		      (if return-annotated
			  (values annotated trace-bindings)
			  evaluation))
		    (values evaluation
			    (and trace (map 'list 'cons tracevars trace-array) )
			    *count*)))))))))

(defun test ()
  (loop for int in '(((f a) (g a)) ((g a)) ((f a)) ())
      for answer in '(t t nil t)
	always (eq (logic::evaluate-formula '(:forall (?x) (:implies (f ?x) (g ?x))) int) answer)))

(defvar *default-forall-implies-optimization-level* 4
  "Use it when quantifiers are nested at least this deep. nil to not use the optimization")
    

(defun evaluate-formulas (spec model &key binary-inverses ternary-inverses (debug nil debug-supplied-p) (implies-optimize *default-forall-implies-optimization-level*))
  "Take spec and interpreation as list of positive propositions, with optional pairs of inverse relations to rewrite, and 
check each of them, as would ladr's clausetester. Return either :satisfying-model or :failed. In the latter case the 
second value is the list of formulas that failed"
  (let ((rewritten (rewrite-inverses spec
				     :binary-inverses binary-inverses
				     :ternary-inverses ternary-inverses
				     :copy-names? t)))
    (progv (if debug-supplied-p `(*debug-eval-formula*)) (if debug-supplied-p (list debug))
	(let ((results 
		(lparallel::pmapcan 
		 (lambda(e) (let ((paiprolog::*trail* paiprolog::*trail* ))
			      (if (evaluate-formula
				   (car (rewrite-inverses (list (axiom-sexp e))
							  :binary-inverses binary-inverses
							  :ternary-inverses ternary-inverses))
				   model
				   :implies-optimize implies-optimize)
				  nil
				  (list (axiom-name e)))))
		 (coerce rewritten 'vector))))
	  (if results
	      (values :failed results)
	      :satisfying-model)))))


(defun annotate-lossage-html (formula model)
  (let ((lines (cl-user::split-at-char 
		(let ((*print-right-margin* 60))
		  (with-output-to-string (s) (pprint (evaluate-formula formula (expanded-model) :trace t :return-annotated t ) s)))  #\newline)))
	
    (loop for line in lines
	  for line2 = (#"replaceAll" line "(:((forall)|(exists)|(implies)|(and)|(or)|(not)|(iff)|(=)))" "<b>$1</b>")
	  for ((pre sep tval after)) = (jss::all-matches line2 "(.*?)([-:]){0,1}((true)|(false))(.*)" 1 2 3 6)
	  do 
	     (cond ((null tval)
		    (format t "<div class=\"label\"></div><div class=\"value\">~a</div><br>~%" (#"replaceAll" line " " "&nbsp;")))
		   ((equal sep ":")
		    (format t "<div class=\"label\">~a</div><div class=\"value\">~a~a</div><br>~%" 
			    tval
			    (#"replaceAll" (subseq "          " 0  (length tval)) " " "&nbsp;")
			    (#"replaceAll" (subseq after 0 (1- (length after))) " " "&nbsp;")))
		   (t (format t "<div class=\"label\">~a</div><div class=\"value\">~a~a</div><br>~%" tval (#"replaceAll" pre " " "&nbsp;") (#"replaceAll" after " " "&nbsp;" )))))))
