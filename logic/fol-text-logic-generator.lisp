(in-package :logic)

(defclass fol-text-logic-generator () 
  ((use-camel :accessor use-camel :initform t :initarg :use-camel)
   (no-spaces :accessor no-spaces :initform nil :initarg :no-spaces)
   (right-margin :accessor right-margin :initform 75 :initarg :right-margin)
   (pprint-dispatch-table :accessor pprint-dispatch-table
			  :initform (copy-pprint-dispatch)
			  :initarg :pprint-dispatch-table)))

(defmethod initialize-instance ((g fol-text-logic-generator) &rest args &key &allow-other-keys)
  (declare (ignore args))
  (call-next-method)
  (loop for (type function priority) in
	'(((cons (member :and :or)) pprint-and-or 1)
	  ((cons (member :exists :forall)) pprint-quantified 1)
	  ((cons (member :implies :iff)) pprint-implication 1)
	  ((cons (member := :not=)) pprint-= 1)
	  ((cons (member :not)) pprint-not 1)
	  (cons pprint-function 0)
	  ((cons (member :fact)) pprint-fact 1)
	  (symbol pprint-symbol 0))
	do 
	   (let ((function function) (priority priority) (type type))
	     (set-pprint-dispatch type
				  (lambda(stream expression)
				    (funcall function g stream expression))
				  priority
				  (pprint-dispatch-table g)
				  ))))

function priority *fol-text-pprint-dispatch*))  

;; constants for the unicode characters for the logic symbols

(defconstant s-forall "∀")
(defconstant s-exists "∃")
(defconstant s-and "∧")
(defconstant s-or "∨")
(defconstant s-implies "→")
(defconstant s-iff "↔")
(defconstant s-not "¬")
(defconstant s-= "=")
(defconstant s-not= "≠")


;; The rules of how to typeset FOL are given by Herbert Enderton, A
;; Mathematical Introduction to Logic, page 78. Those rules specify how
;; operators bind (tightly usually) and show, for expressions that might be
;; ambiguous about grouping(lhs), which parenthesized version holds (rhs)

;; In our case, the formula sexp is a very parenthesized rhs for those
;; rules, corresponding to the rhs. We use the pretty printer interface
;; adding parentheses when necessary, and in one case when not neccessary.

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

(defmethod pprint-and-or ((g fol-text-logic-generator) stream sexp)
  (pprint-logical-block (stream sexp)
    (let ((sep (if (eq (car sexp) :and) s-and s-or)))
      (pprint-pop)
      (loop
	(let ((next (pprint-pop)))
					;	  (pprint-indent :block -2 stream)
	  (pprint-w-paren-if-not-singular g stream next))
	(pprint-exit-if-list-exhausted) 
	(pprint-newline :fill stream)
					;	  (pprint-indent :block 0 stream)
	(write-char #\space stream)
	(write-string sep stream)
	(write-char #\space stream)
	))))

(defmethod pprint-implication ((g fol-text-logic-generator) stream sexp)
  (pprint-logical-block (stream sexp)
    (let ((sep (if (eq (car sexp) :implies) s-implies s-iff)))
      (pprint-pop)
      (let ((ant (pprint-pop))
	    (cons (pprint-pop)))
	(pprint-logical-block (stream ant)
	  (pprint-indent :block -3 stream)
	  (write ant :stream stream)
	  (pprint-newline :linear stream)) ;; does better than :fill
	(write-char #\space stream)
	(write-string sep stream)
	(write-char #\space stream)
	;; For :implies and :iff, if the consequent is another implication then wrap it in
	;; "gratuitous" parens, even though according to rule 4 it doesn't need it.
	;; Hard for me to read, otherwise
	(if (member (car (third sexp)) '(:iff :implies))
	    (pprint-logical-block (stream cons :prefix "(" :suffix ")")
	      (write cons :stream stream))
	    (write cons :stream stream))
	))))

(defmethod pprint-function ((g fol-text-logic-generator) stream sexp)
  (pprint-logical-block (stream sexp)
      (write (pprint-pop) :stream stream)
      (pprint-logical-block (stream (cdr sexp) :prefix "(" :suffix ")")
      (loop (write (pprint-pop) :stream stream )
	    (pprint-exit-if-list-exhausted) 
	    (write-char #\, stream))
      )))

(defmethod pprint-not ((g fol-text-logic-generator) stream sexp)
  (if (eq (car (second sexp)) :=)
      (write `(:not= ,@(cdr (second sexp))) :stream stream)
      (pprint-logical-block (stream sexp)
	(write-string s-not stream)
	(pprint-w-paren-if-not-singular g stream (second sexp)))))

(defmethod pprint-= ((g fol-text-logic-generator) stream sexp)
  (pprint-logical-block (stream sexp)
    (write (second sexp) :stream stream)
    (if (eq (car sexp) ':=)
	(write-string s-= stream)
	(write-string s-not= stream))
    (write (third sexp) :stream stream)
    ))

;; An expression is singular if it doesn't have to be parenthesized in a conjunction.
(defmethod expression-singular ((g fol-text-logic-generator) exp)
  (if (not (formula-sexp-p exp)) ;; it's a relation
      exp 
      (cond ((eq (car exp) :parens) t) ;; it's singular by virtue of already being parenthesized
	    ((eq (car exp) :not)  ;; If the inside of negation is singular, the negation is as well
	     t);(singular (second exp)))
	    ((member (car exp) '(:and :or :implies :iff)) 
		     nil ) ;; these never are singular - syntactically they have more than one element
	    ((member (car exp) '(:forall :exists)) ;; singular if what's in their scope is singular
		     (expression-singular g (third exp)))
	    ((member (car exp) '(:= :not=))  ;; = binds tightly, so an equality is singular
	     t)
	    (t (error "what did I forget?")))))

(defmethod pprint-w-paren-if-not-singular ((g fol-text-logic-generator) stream form &optional indent)
  (if (expression-singular g form)
	(pprint-logical-block (stream form)
	  (when indent (pprint-indent :block indent stream)) 
	  (write form :stream stream))
	(pprint-logical-block (stream form :prefix "(" :suffix ")")
	  (when indent (pprint-indent :block indent stream))
	  (write form :stream stream))))

(defmethod pprint-quantified ((g fol-text-logic-generator) stream sexp)
  (pprint-logical-block (stream sexp)
    (if (eq (pprint-pop) :forall)
	(write-string s-forall stream)
	(write-string s-exists stream))
    (pprint-logical-block (stream (second sexp))
      (loop (write (pprint-pop) :stream stream )
	    (pprint-exit-if-list-exhausted) 
	    (write-char #\, stream)))
    (write-char #\space stream)
    (pprint-w-paren-if-not-singular g stream (third sexp))
    ))

(defmethod pprint-fact ((g fol-text-logic-generator) stream sexp)
  (pprint-function g stream (second sexp)))

(defmethod render-axiom ((g fol-text-logic-generator) (a axiom))
  (let* ((*print-pprint-dispatch* (pprint-dispatch-table g))
	 (*print-right-margin* (right-margin g))
	 (*print-pretty* t))
    (with-output-to-string (s)
      (pprint-logical-block (nil (axiom-sexp a))
	(write (simplify-and-or (eval (rewrite-to-axiom-generation-form (axiom-sexp a)))) :stream s)))))

;; pretty-print-formula - print indented as (UTF-8) text 
(defun ppf (sexp &key (right-margin 70) (stream t))
  (let* ((g (make-instance 'fol-text-logic-generator :right-margin right-margin)))
    (format stream "~a" (render-axiom g 
			(if (keywordp sexp)
			    (get-axiom sexp)
			    (make-instance 'axiom :sexp sexp :dont-validate t)))
	    stream)))
	 
;; ****************************************************************

(defclass latex-logic-generator-2 (logic-generator)
  ((text-generator :accessor text-generator :initform nil :initarg :text-generator)
   (centered :accessor centered :initform t :initarg :centered)))
  

(defmethod initialize-instance ((g latex-logic-generator-2) &rest args &key &allow-other-keys)
  (call-next-method)
  (setf (text-generator g) (apply 'make-instance 'fol-text-logic-generator args)))

;; foltext is pretty-printed text of formula
;; This collects the different numbers of spaces indented for all the lines
;; which will be turned into tab stops
(defmethod tab-positions-for-foltext ((g latex-logic-generator-2) foltext)
  (sort (remove-duplicates
	 (loop for line in (split-at-char foltext #\newline)
	       for space = (position-if-not 'sys::whitespacep line)
	       collect space)  :test 'eql) '<))

;; insert the tab positions "\="  (latex tabbing environment)
;; Put the tab stop on the first line that it is long enough. Sometimes
;; the first line is short but tab stop has to be past the end of that line.
;; Next line is probably longer so put it there.
(defmethod insert-tab-positions ((g latex-logic-generator-2) foltext)
  (let ((positions (tab-positions-for-foltext g foltext)))
    (with-output-to-string (s)
      (loop for char across foltext
	    for count from 0
	    ;; next line if extends past first
	    if (char= char #\newline)
	      do (setq count 0)  
	    when (member (+ count 1) (cdr positions) :test 'eql)
	      do (write-string "\\=" s) (pop positions) ;; remove position because we've handled it
	    do (write-char char s)))))

;; Put as many tabs "\>" as needed to indent to the right position.
(defmethod insert-leading-tabs ((g latex-logic-generator-2) foltext)
  (let ((stops (tab-positions-for-foltext g foltext)))
    (with-output-to-string (s)
      (loop for line in (split-at-char (insert-tab-positions g foltext) #\newline)
	    for space = (position-if-not 'whitespacep line)
	    do (unless (zerop space)
		 (loop repeat (position space stops) do (write-string "\\>" s)))
	       (format s "~a\\\\~%" line s)
	    ))))

;; Simple transform of foltext to latex. Mostly change the symbols to the latex math symbols
;; Also change "prime" variables to print using "'".
;; Don't replace the = with math = because the tab stops are already in place.
;; Maybe fix that.
;; This isn't the greatest - e.g. not easy to style variables. But indenting properly is more important
(defmethod foltext-to-latex ((g latex-logic-generator-2) foltext)
  (setq foltext (#"replaceAll" foltext s-forall "\\$\\\\forall\\$\\\\;"))
  (setq foltext (#"replaceAll" foltext s-exists "\\$\\\\exists\\$"))
  (setq foltext (#"replaceAll" foltext s-and "\\$\\\\land\\$"))
  (setq foltext (#"replaceAll" foltext s-or "\\$\\\\lor\\$"))
  (setq foltext (#"replaceAll" foltext s-implies "\\$\\\\rightarrow\\$"))
  (setq foltext (#"replaceAll" foltext s-iff "\\$\\\\leftrightarrow\\$"))
  (setq foltext (#"replaceAll" foltext s-not "\\$\\\\neg\\$"))
  ;;      (setq foltext (#"replaceAll" foltext s-= "\\$=\\$"))
  (setq foltext (#"replaceAll" foltext "([a-z])(prime)\\b" "$1\\\\textprime"))
  (setq foltext (#"replaceAll" foltext s-not= "\\$\\\\neq\\$")))


(defmethod formula-in-tabbed-latex-environment ((g latex-logic-generator-2) latex )
  (let ((tabbed (format nil "\\begin{tabbing}~a~%\\end{tabbing}~%"
			latex)))
    (if (centered g) 
	(format nil "\\begin{center}\\parbox{0cm}{~a}\\end{center}" tabbed)
	tabbed)))

(defmethod render-axiom ((g latex-logic-generator-2) (a axiom))
  (formula-in-tabbed-latex-environment g
   (foltext-to-latex g
    (insert-leading-tabs g (render-axiom (text-generator g) a)))))

;; pretty-print latex
(defun ppl (sexp &key (right-margin 70) (centered t))
  (render-axiom
   (make-instance 'latex-logic-generator-2 :right-margin right-margin :centered centered)
   (make-instance 'axiom :sexp sexp :dont-validate t)))

(defun dump-bfo-fol-pp ()
  (with-open-file (f  "~/Desktop/debug.txt" :if-does-not-exist :create :if-exists :supersede :direction :output)
    (let ((*standard-output* f))
      (loop with counts 
	    for ax in (collect-axioms-from-spec bfo::*everything-theory*) 
	    for sexp = (axiom-sexp ax)
	    do  (terpri) (princ "----------------------------------------------------------------")
		(pps (axiom-name ax))
		(terpri)
		(ppf sexp 100)
		(terpri)(terpri)
		(ppf sexp 70)
		(terpri)
	    ))))

(defun dump-bfo-fol-latex (&optional (margin 80))
  (with-open-file (f  "~/Desktop/debug.tex" :if-does-not-exist :create :if-exists :supersede :direction :output)
    (let ((*standard-output* f))
      (format f "\\documentclass{article}~%\\usepackage{amsmath}~%\\usepackage{flexisym}~%\\begin{document}\\setlength{\\parindent}{0pt}
~%")

      (loop for ax in (collect-axioms-from-spec bfo::*everything-theory*) 
	    if (null (ignore-errors (ppl (axiom-sexp ax) :right-margin margin)))
	      do (warn "error in ~a" (axiom-name ax))
	    else	    do (ignore-errors 
				(format t "~%\\textbf{:~a}" (axiom-name ax))
				(format t "~%\\message{~a}" (axiom-name ax))
				(princ (ppl (axiom-sexp ax) :right-margin margin) f)
				(format f "~%")))
      (format f "\\end{document}~%"))))


