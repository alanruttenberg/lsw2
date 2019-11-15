(in-package :logic)

(defclass latex-logic-generator (logic-generator) 
  ((formula-format :accessor formula-format :initarg :formula-format :initform "~a:~%~a")
   (insert-line-breaks :accessor insert-line-breaks :initarg :insert-line-breaks :initform nil)
   (prettify-names :accessor prettify-names :initform t :initarg :prettify-names)
   (with-names :accessor with-names :initform t :initarg :with-names)
   (write-descriptions :accessor write-descriptions :initform nil :initarg :write-descriptions)
   (numbered :accessor numbered :initform nil :initarg :numbered)
   ;; \left( and \right) are versions of parens that tex can use in some cases.
   ;; notably they don't work in autobreak, but do in breqn
   (use-left-right :accessor use-left-right :initform nil :initarg :use-left-right )
   ;; Have grouping parentheses bold so easier to visually group
   (bold-grouping-parentheses :accessor bold-grouping-parentheses :initform t :initarg :bold-grouping-parentheses )
   ))

;; Change symbols with dashes into camelcase. A but ugly because we're getting a mix of
;; symbols and already generated latex

(defmethod normalize-names ((g latex-logic-generator) e)
  (cond ((and (symbolp e) (char= (char (string e) 0) #\?))
	 (camelCase (subseq (string e) 1) nil))
	((symbolp e) (camelCase (string e) nil))
	((and (stringp e) (or (find #\( e :test 'char=) (find #\= e :test 'char=) (find #\\ e :test 'char=))) e) ;; already done
	((stringp e) (camelCase e nil))
	((uri-p e) (camelCase (if (and (boundp '*default-kb*) *default-kb*)
						    (uri-label e)
						    (#"replaceAll" (uri-full e) ".*/" "")) nil) )
	((atom e) e)
	(t (mapcar (lambda(e) (normalize-names g e)) e))))

(defmethod latex-expression ((g latex-logic-generator) expression)
  (if (null expression) (break "latex null expression"))
  (if (consp expression)
      (format nil "~a(~{~a~^{,}~})" (normalize-names g (car expression)) (normalize-names g (cdr expression)))
      (normalize-names g expression))
  )

;; Controls whether there should be a linefeed after various operators. In autobreak
;; the linefeeds become hints about where it's ok to break a line
;; return #\newline or "" as the result will be used in the format string

(defmethod break-after-operator? ((g latex-logic-generator) operator)
  (assert (member operator '(:iff :implies :and :or)) () "Breaks after '~s'?" operator)
  ;; for now always as autobreak does best with this
  (if (and (member operator '(:iff :implies)) (insert-line-breaks g))
      #\newline
      ""))

(defmethod break-before-operator? ((g latex-logic-generator) operator)
  (assert (member operator '(:iff :implies :and :or)) () "Breaks after '~s'?" operator)
  ;; for now always as autobreak does best with this
  (if (and (member operator '(:iff :implies :and :or)) (insert-line-breaks g))
      #\newline
      ""
      ))

(defmethod latex-quantifier-vars ((g latex-logic-generator) vars)
  (normalize-names g vars))

(defmethod logical-relation ((g latex-logic-generator) head &rest args)
  (format nil "\\text{~a}(~{~a~^{,}~}\\,)" (normalize-names g head )
	  (mapcar (lambda(e) (let ((sym (normalize-names g e)))
			       (if (logic-var-p e) 
				   (format nil "\\text{{\\it ~a}}" sym)
				   (format nil "\\text{~a}" sym))))
		  args)))
(defmethod logical-forall ((g latex-logic-generator) vars expressions)
  (format nil "\\forall\\, ~{~a~^{,}~}\\, ~{~a~}"  (latex-quantifier-vars g vars)
	  (mapcar (lambda(e)(latex-expression g e)) expressions)))

(defmethod logical-exists ((g latex-logic-generator) vars expressions)
  (format nil "\\exists\\, ~{~a~^{,}~}\\, ~{~a~}"  (latex-quantifier-vars g vars)
	  (mapcar (lambda(e)(latex-expression g e)) expressions)))

(defmethod logical-implies ((g latex-logic-generator) antecedent consequent)
  (format nil "~a ~a\\rightarrow~a ~a" (latex-expression g antecedent)
	  (break-before-operator? g :implies)
	  (break-after-operator? g :implies)
	  (latex-expression g consequent)))

(defmethod logical-and ((g latex-logic-generator) expressions)
  (let ((format-string (if (eql (break-after-operator? g :and) #\newline)
			   "~{~a ~^~%\\land ~%~}"
			   "~{~a ~^\\land ~^~%~}")))
    (format nil format-string  (mapcar (lambda(e) (latex-expression g e)) expressions))))

(defmethod logical-distinct ((g latex-logic-generator) &rest names)
  (let ((expanded (make-instance 'axiom :sexp (call-next-method) :dont-validate t)))
    (eval `(let ((*logic-generator* ,g))
	     (eval (axiom-generation-form ,expanded))))))

(defmethod logical-or ((g latex-logic-generator) expressions) 
  (format nil
	  (format nil "~~{~~a ~~^~a\\lor ~a~~}"
		  (break-before-operator? g :or)
		  (break-afer-operator? g :or))
	  (if (eql (break-before-operator? g :or) #\newline)
	      "~{~a ~^~%\\lor ~}"
	      "~{~a ~^\\lor ~}")
	  (mapcar (lambda(e) (latex-expression g e)) expressions)))

(defmethod logical-iff ((g latex-logic-generator) antecedent consequent)
  (format nil "~a ~a\\leftrightarrow~a ~a"
	  (latex-expression g antecedent)
	  (break-before-operator? g :iff)
	  (break-after-operator? g :iff)
	  (latex-expression g consequent)))

(defmethod logical-not ((g latex-logic-generator) expression)
  (format nil "\\neg ~a" (latex-expression g expression)))

(defmethod logical-= ((g latex-logic-generator) a b)
  (format nil "~a = ~a" (latex-expression g a) (latex-expression g b)))

(defmethod logical-neq ((g latex-logic-generator) a b)
  (format nil "~a \\neq ~a" (latex-expression g a) (latex-expression g b)))

(defmethod logical-holds ((g latex-logic-generator) &rest args) 
  (latex-expression g `(holds ,@args)))

(defmethod logical-fact ((g latex-logic-generator) fact)
  (latex-expression g fact))

;; Parentheses are either for relationships or functions f(x,y).. or for
;; grouping This function generates the grouping parentheses.

;; The switch use-left-right controls whether we use \left( and \right) for
;; the parens. Using this doesn't work when using autobreak. It at least isn't
;; rejected by breqn, but I'm not sure it's helpful. Default off.

;; The switch bold-grouping-parentheses controls whether we want the
;; grouping parentheses to be bold to help contrast them with the parens
;; used in relationships so it's easier to visually group. Default on.

(defmethod logical-parens ((g latex-logic-generator) expression)
  (format nil 
	  (if (use-left-right g)
	      "\\left(~a\\right)"
	      (if (bold-grouping-parentheses g)
		  "\\text{\\textbf{(}}~a\\text{\\textbf{)}}"
		  "(~a)"
		  ))
	  (latex-expression g expression)))

;; helper to recognize :not=
(defun equals-inside-not (expression)
  (and (consp expression)
       (eq (car expression) :not)
       (consp (second expression))
       (eq (car (second expression)) :=)))

;; An expression is singular if it doesn't have to be parenthesized in a conjunction.
(defmethod expression-singular ((g latex-logic-generator) exp)
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
(defmethod parenthesize-unless-singular ((g latex-logic-generator) exp)
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

(defmethod rewrite-form-adding-parentheses-as-necessary ((g logic-generator) expression)
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
	   (let ((inner (rewrite-form-adding-parentheses-as-necessary g (second expression))))
	     `(:not ,(parenthesize-unless-singular g inner))
	     ))
		 
	  ;; :exists and :forall  apply to as little as possible, like :not 
	  ;; So if the body isn't singular it needs to be parenthesized 
	  ((member op '(:forall :exists))
	   (let ((inner (rewrite-form-adding-parentheses-as-necessary g (third expression))))
	     (if (expression-singular g inner)
		 `(,(car expression) ,(second expression) ,inner)
		 `(,(car expression) ,(second expression) (:parens ,inner)))))

	  ;; :implies and :iff have parentheses around each side that isn't singular 
	  ((member op '(:implies :iff))
	   `(,(car expression)
	     ,(parenthesize-unless-singular g (rewrite-form-adding-parentheses-as-necessary g (second expression)))
	     ,(parenthesize-unless-singular g (rewrite-form-adding-parentheses-as-necessary g (third expression)))))

	  ;; in a conjunctiion, parenthesize anything not singular (definition of singular) 
	  ((member op '(:and :or))
	   `(,(car expression)
	     ,@(mapcar
		(lambda(e)
		  (parenthesize-unless-singular g (rewrite-form-adding-parentheses-as-necessary g e)))
		(cdr expression))))
	  
	  ;; equality binds tightest, so always singular 
	  ((member (car expression) '(:= :not=))
	   expression)

	  ;; :fact wraps a relationship when it would otherwise be toplevel. 
 	  ((eq (car expression) :fact)
	   (second expression))
	  
	  (t (error "Shouldn't be here - missed a case in rewrite-form-adding-parentheses-as-necessary"))))))

(defun quote-for-latex (string)
  (#"replaceAll" string "([&_])" "\\\\$1"))

(defmethod render-axiom ((g latex-logic-generator) (a axiom))
  (let ((*logic-generator* g))
    (let ((name (string (axiom-name a))))
      (if (prettify-names g)
	  (progn
	    (setq name  (string-downcase (#"replaceAll" (string (axiom-name a)) "-" " ")))
	    (setf (char name 0) (char-upcase (char name 0))))
	  (setq name (format nil ":~a" (string-downcase name))))
      (flet ((nodashes (string)
	       (and string (#"replaceAll" string "-" " "))))
	(concatenate 'string
		   (if (and (write-descriptions g) (stringp (axiom-description a)) (not (equal (axiom-description a) "")))
		       (quote-for-latex (nodashes (axiom-description a)))
		       "")
		   (if (eq (with-names g) :description)
		       (or (second (assoc  :latex-alternative (axiom-plist a)))
			   (format nil (formula-format g) (nodashes (axiom-description a))
				   (eval (rewrite-to-axiom-generation-form (rewrite-form-adding-parentheses-as-necessary g (axiom-sexp a))))))
		       (if (with-names g)
			   (or (second (assoc  :latex-alternative (axiom-plist a)))
			       (format nil (formula-format g) name
				       (eval (rewrite-to-axiom-generation-form (rewrite-form-adding-parentheses-as-necessary g (axiom-sexp a))))))
			   (or (second (assoc  :latex-alternative (axiom-plist a)))
			       (format nil (formula-format g) 

				       (eval (rewrite-to-axiom-generation-form (rewrite-form-adding-parentheses-as-necessary g (axiom-sexp a)))))))))))))

(defmethod render-axioms ((generator latex-logic-generator) axs)
  (let ((count 0))
    (if (stringp axs)
	axs
	(format nil "~{~a~^~%~}" 
		(mapcar (lambda(e)
			  (concatenate 'string
				       (if (numbered generator) (format nil "~a. " (incf count))
					   "")
				       (render-axiom generator e))) axs))
		)))


;; even using breqn there is trouble with long parenthesized expressions
;; one strategy is that if there are more than 3 existentials, put them on a separate line with \\ and remove the surrounding parentheses.
;; might try the "." instead of parentheses for quantifiers

(defun axiom-sexp-length (sexp)
  (cond ((atom sexp) (length (string sexp)))
	((member (car sexp) '(:forall :exists :and :or :not :implies :iff))
	 (+ 2 (apply '+ (mapcar 'axiom-sexp-length (rest sexp)))))
	(t (+ (axiom-sexp-length (car sexp)) (apply '+ (mapcar 'axiom-sexp-length (rest sexp)))))))
	 
  
