(in-package :logic)

;; tools to print using traditional fol syntax, used by latex and fol-text generators
;; functions:
;;   rewrite-form-adding-parentheses-as-necessary to add parentheses
;;   token-list to return a serialized list of tokens

(defclass fol-logic-generator (logic-generator) ())

;; helper to recognize :not=
(defmacro equals-inside-not (expression)
  `(and (consp ,expression)
       (eq (car ,expression) :not)
       (consp (second ,expression))
       (eq (car (second ,expression)) :=)))

;; An expression is singular if it doesn't have to be parenthesized in a conjunction.
(defmethod expression-singular ((g fol-logic-generator) exp)
  (if (not (formula-sexp-p exp)) ;; it's a relation
      exp 
      (cond ((eq (car exp) :parens) t) ;; it's singular by virtue of already being parenthesized
	    ((eq (car exp) :not)  ;; If the inside of negation is singular, the negation is as well
	     (expression-singular g (second exp)))
	    ((member (car exp) '(:and :or :implies :iff)) 
		     nil ) ;; these never are singular - syntactically they have more than one element
	    ((member (car exp) '(:forall :exists)) ;; singular if what's in their scope is singular
		     (expression-singular g (third exp)))
	    ((member (car exp) '(:= :not=))  ;; = binds tightly, so an equality is singular
	     t)
	    (t (error "what did I forget?"))))) ;; hopefully nothing
  
;; parenthesize an expression that isn't singular. Return the expression if singular
(defmethod parenthesize-unless-singular ((g fol-logic-generator) exp)
  (if (expression-singular g exp)
      exp
      `(:parens ,exp)))

;; rewrite-form-adding-parentheses-as-necessary
;; ____________________________________________

;; Takes as input a formula sexp. At this point there are no explicit
;; parentheses. The parentheses in the sexp are not what we typeset - there
;; are too many of them Instead we have to figure out were parenthesis
;; should be. To mark them, we transform the expression to (:parens expression).
;; Parens are typeset by l-parens above.

;; The rules of how to typeset FOL are given by Herbert Enderton, A
;; Mathematical Introduction to Logic, page 78. Those rules specify how
;; operators bind (tightly usually) and show, for expressions that might be
;; ambiguous about grouping(lhs), which parenthesized version holds (rhs)

;; In our case, the formula sexp is a very parenthesized rhs for those
;; rules, corresponding to the rhs. We do this by categorizing expressions by
;; their operator and deciding whether the unparenthesized version of the expression 
;; means what our formula means. If not, we add parentheses as appropriate.

;; In doing so we consider whether a form is 'singular' - a form that would not need to 
;; parenthesized in a conjunction, vs one that would have to be parenthesized. Manifestly
;; nonsingular expressions are conditionals, conjunctions, disjunctions, since there are 
;; always more than one element and those elements might need to be parenthesized.

;; We do one transformation outside of adding parentheses. When we see (:not (:= x y))
;; we translate it to (:not= x y), which typesets as the notequal sign.

;; Here is the relevant bit from Edgerton
;; (thanks to https://math.stackexchange.com/questions/1150746/what-is-the-operator-precedence-for-quantifiers)

;; The recursive definition of formula for FOL is (having defined term) more or less this :

;;   (i) 𝑡1=𝑡2 and 𝑃𝑛(𝑡1,…,𝑡𝑛) are atomic formulas, where 𝑡1,…,𝑡𝑛 are terms and 𝑃𝑛 is a 𝑛-ary predicate symbol;
;;  (ii) if 𝜑,𝜓 are formulas, then ¬𝜑,𝜑∧𝜓,𝜑∨𝜓,𝜑→𝜓 are formulas;
;; (iii) if 𝜑 is a formula, then ((∀𝑥)𝜑),((∃𝑥)𝜑) are formulas.

;; Then we can introduce abbreviations for readibility. see Herbert Enderton, A Mathematical Introduction to Logic, page 78 

;; For parentheses we will omit mention of just as many as we possibly can. Toward that end we adopt the following conventions:
;; 1. Outermost parentheses may be dropped. For example, ∀𝑥α→β is (∀𝑥α→β).

;; 2. ¬,∀, and ∃ apply to as little as possible. For example,
;;     ¬α∧β is ((¬α)∧β), and not ¬(α∧β)
;;     ∀𝑥α→β is (∀𝑥α→β), and not ∀𝑥(α→β)
;;     ∃𝑥α∧β is (∃𝑥α∧β), and not ∃𝑥(α∧β)

;; In such cases we might even add gratuitous parentheses, as in (∃𝑥α)∧β.

;; 3. ∧ and ∨ apply to as little as possible, subject to item 2. For example, ¬α∧β→γ is ((¬α)∧β)→γ

;; 4. When one connective is used repeatedly, the expression is grouped to the right. For example, α→β→γ is α→(β→γ)

		     
(defmethod rewrite-form-adding-parentheses-as-necessary ((g fol-logic-generator) expression)
  (setq expression (simplify-and-or expression))
  (labels ((rewrite (expression)
	     (if (not (formula-sexp-p expression)) ;; leave relationships alone 
		 expression
		 (let ((op (car expression)))
		   (cond 
		     ;; rewrite "(:not (:= a b)" -> "(:not= a b)" 
		     ((equals-inside-not expression)
		      `(:not= ,@(cdr (second expression))))

		     ;; not applies to as little as possible, so unless it's a relational expression, parenthesize	
		     ((eq op :not)
		      ;; We always rewrite the inner form before checking whether singular 
		      (let ((inner (rewrite (second expression))))
			`(:not ,(parenthesize-unless-singular g inner))
			))
		 
		     ;; :exists and :forall  apply to as little as possible, like :not 
		     ;; So if the body isn't singular it needs to be parenthesized 
		     ((member op '(:forall :exists))
		      (let ((inner (rewrite (third expression))))
			(if (expression-singular g inner)
			    `(,(car expression) ,(second expression) ,inner)
			    `(,(car expression) ,(second expression) (:parens ,inner)))))

		     ;; For :implies and :iff, if the consequent is another implication then wrap it in
		     ;; "gratuitous" parens, even though according to rule 4 it doesn't need it.
		     ;; Hard for me to read, otherwise
		     ((member op '(:implies :iff))
		      (let ((raw
			      `(,(car expression)
				,(rewrite (second expression))
				,(rewrite (third expression)))))
			(if (member (car (third expression)) '(:iff :implies))
			    raw
			    `(:parens, raw))))

		     ;; in a conjunctiion, parenthesize anything not singular (definition of singular) 
		     ((member op '(:and :or))
		      `(,(car expression)
			,@(mapcar
			   (lambda(e)
			     (parenthesize-unless-singular g (rewrite e)))
			   (cdr expression))))
	  
		     ;; equality binds tightest, so always singular 
		     ((member (car expression) '(:= :not=))
		      expression)

		     ;; :fact wraps a relationship when it would otherwise be toplevel. 
		     ((eq (car expression) :fact)
		      (second expression))

		     ((eq (car expression) :parens)
		      expression)

		     ((eq (car expression) :distinct)
		      expression)
	  
		     (t (error "Shouldn't be here - missed a case in rewrite-form-adding-parentheses-as-necessary")))))))
    (rewrite expression)))


;; Creates a linearized formula - the tokens respond to who the formula would be laid out literally.
;; e.g. (:and (f ?x) (g ?x)) becomes: f { x } ∧ g { x }
;; The logic tokens are the standard symbols. ∀ ∃ ¬ = ≠ ∨ ∧ → ↔ 
;; There are 4 extra types of tokens.
;; {} parentheses around function call arguments
;; [] grouping parentheses
;; _ after the last of quantified variable - makes it easier to space a paren specially
;; , (comma) between args in a function and between lists of quantified over variables 
;; This was written to support writing FOL as text but might be useful in a rewrite of the logic generator.

;; STILL NEEDED: pretty-print for longer formula 

(defmethod token-list ((g fol-logic-generator) (a list))
  (let ((them nil))
    (labels ((emit (&rest args)
	       (loop for arg in args
		     if (char= (char (string arg) 0) #\?)
		       do (push (intern (subseq (string arg) 1)) them)
		     else do (push arg them)))
	     (emit-join (args sep &optional (each #'emit))
	       (loop for el on args
		     do (funcall each (car el))
		     until (null (cdr el))
		     do (emit sep)))
	     (emit-w-comma (args)
	       (emit-join args '|,|))
	     (emit-w-middle (pair middle &optional (each #'emit))
	       (funcall each (first pair))
	       (emit middle)
	       (funcall each (second pair)))
	     (emit-quantified (quantifier vars-body)
	       (emit quantifier)
	       (emit-w-comma (first vars-body))
	       (emit '_)
	       (inner (second vars-body)))
	     (inner (form)
	       (if (not (formula-sexp-p form))
		   (progn 
		     (emit (car form) '{) (emit-w-comma (cdr form)) (emit '}))
		   (let ((op (car form)))
		     (ecase op
		       (:forall (emit-quantified '∀ (cdr form)))
		       (:exists (emit-quantified '∃ (cdr form)))
		       (:and (emit-join (cdr form) '∧ #'inner))
		       (:or (emit-join (cdr form) '∨ #'inner))
		       (:parens (emit '[) (inner (second form)) (emit ']))
		       (:implies (emit-w-middle (cdr form) '→ #'inner))
		       (:iff (emit-w-middle  (cdr form) '↔ #'inner))
		       (:not (emit '¬) (inner (second form)))
		       (:= (emit-w-middle (cdr form) '=))
		       (:not= (emit-w-middle (cdr form) '≠))
		       (:distinct (progn (emit 'distinct '{) (emit-w-comma (cdr form)) (emit '})))
			   )))))
      (inner (axiom-sexp a))
      (reverse them))))

