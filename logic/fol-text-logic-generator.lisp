(in-package :logic)

(defconstant s-forall "∀")
(defconstant s-exists "∃")
(defconstant s-and "∧")
(defconstant s-or "∨")
(defconstant s-implies "→")
(defconstant s-iff "↔")
(defconstant s-not "¬")
(defconstant s-= "=")
(defconstant s-not= "≠")

(defclass fol-text-logic-generator (fol-logic-generator) 
  ((use-camel :accessor use-camel :initform t :initarg :use-camel)
   (no-spaces :accessor no-spaces :initform nil :initarg :no-spaces)))

;; renders as a string, based on fol-logic-generator tokens

(defmethod render-axiom ((g fol-text-logic-generator) (a axiom))
  (with-output-to-string (s)
    (loop for el in (token-list g (rewrite-form-adding-parentheses-as-necessary g (axiom-sexp a)))
	  do (case el
	       ((∀ ∃) (format s "~a" el))
	       (([ {) (write-char #\( s))
	       ((] }) (write-char #\) s))
	       (_ (unless (no-spaces g) (write-char #\space s)))
	       ((∧ ∨ → ↔) (if (no-spaces g) (format s "~a" el s) (format s " ~a " el)))
	       (t (format s "~a" (if (use-camel g)
				     (cl-user::camelcase (string el))
				     (string-downcase (string el))
				     )))))))

(defmethod render-axiom ((g fol-text-logic-generator) (a symbol))
  (render-axiom (axiom-sexp a)))

(defmethod render-axioms ((g fol-text-logic-generator) axs)
  (format t "~{~a~^~%~}" (mapcar (lambda(e) (render-axiom g e)) axs)))

(defun test ()
  (let ((g (make-instance 'fol-text-logic-generator :use-camel nil)))
    (flet ((no-white (s) (#"replaceAll" s "\\s" "")))
      (values
       (loop for (case expected string) in (testcases g)
	     for pass = (and (equalp (token-list g case) expected)
			     (equalp (no-white (render-axiom g case))
				     (no-white string)))
	     unless pass do (print-db case expected (token-list g case)
				      (no-white (render-axiom g case))
				      (no-white string) 
				      )
	       always pass)
       (render-axiom g (caar (last (testcases g))))))))

(defmethod testcases ((g fol-text-logic-generator))
  '(
    ((:and (f x) (g x))
     (f { x } ∧ g { x })
     "f(x) ∧ g(x)")
    ((:forall (?i ?start ?end)
       (:parens
	(:implies
	    (:parens
	     (:and (instance-of ?i temporal-interval ?i)
		   (has-first-instant ?i ?start) (has-last-instant ?i ?end)))
	    (:not
		(:exists (?gap ?gap-start ?gap-end)
		  (:parens
		   (:and (:not (instance-of ?gap temporal-instant ?gap))
			 (has-first-instant ?gap ?gap-start)
			 (has-last-instant ?gap ?gap-end)
			 (:parens
			  (:or (precedes ?gap-end ?end)
			       (:parens
				(:and (temporal-part-of ?end ?i)
				      (:= ?gap-end ?end)))))
			 (:parens
			  (:or (precedes ?start ?gap-start)
			       (:parens
				(:and (temporal-part-of ?start ?i)
				      (:= ?gap-start ?start)))))
			 (:not (temporal-part-of ?gap ?i))))))))) 
     (∀ i |,| start |,| end _ [ [ instance-of { i
      |,| temporal-interval |,| i } ∧ has-first-instant { i |,| start } ∧ has-last-instant { i |,| end } ] → ¬ ∃ gap
      |,| gap-start |,| gap-end _ [ ¬ instance-of { gap |,| temporal-instant |,| gap } ∧ has-first-instant { gap |,|
      gap-start } ∧ has-last-instant { gap |,| gap-end } ∧ [ precedes { gap-end |,| end } ∨ [ temporal-part-of { end
      |,| i } ∧ gap-end = end ] ] ∧ [ precedes { start |,| gap-start } ∨ [ temporal-part-of { start |,| i } ∧
      gap-start = start ] ] ∧ ¬ temporal-part-of { gap |,| i } ] ])
     "∀i,start,end ((instance-of(i,temporal-interval,i) ∧ has-first-instant(i,start) ∧ has-last-instant(i,end)) → ¬∃gap,gap-start,gap-end (¬instance-of(gap,temporal-instant,gap) ∧ has-first-instant(gap,gap-start) ∧ has-last-instant(gap,gap-end) ∧ (precedes(gap-end,end) ∨ (temporal-part-of(end,i) ∧ gap-end=end)) ∧ (precedes(start,gap-start) ∨ (temporal-part-of(start,i) ∧ gap-start=start)) ∧ ¬temporal-part-of(gap,i)))")))



#|
∀ i,start,end ((instance-of(i temporal-interval i)  
                ∧ has-first-instant(i start) ∧ has-last-instant (i end)) 
       →  ¬∃ gap,gap-start,gap-end
            (¬instance-of(gap temporal-instant gap) ∧ has-first-instant(gap gap-start) 
             ∧ has-last-instant(gap gap-end) 
             ∧ (precedes(gap-end end) ∨ (temporal-part-of(end i) ∧ gap-end = end)) 
             ∧ (precedes(start gap-start) ∨ (temporal-part-of(start i) ∧ gap-start=start)) 
             ∧ ¬temporal-part-of(gap i)))




((:distinct . 1) (:not . 1) (:and . 10) (:forall . 299) (:fact . 36))
inside the foralls
((:iff . 35) (:or . 1) (:implies . 263) (nil . 48))

(loop with counts 
	   for ax in (collect-axioms-from-spec *everything-theory*) 
	   for head = (car (axiom-sexp ax))
	   do
	      (if (assoc head counts)
		(incf (cdr (assoc head counts)))
		(push (cons head 1) counts))
      finally (return counts))

(loop with counts 
	   for ax in (collect-axioms-from-spec *everything-theory*) 
	   for head = (and (eq (car (axiom-sexp ax)) :forall)
			   (car (third (axiom-sexp ax))))
	   do
	      (when (eq head :or) (print ax))
	      (if (assoc head counts)
		(incf (cdr (assoc head counts)))
		(push (cons head 1) counts))
      finally (return counts))

:sdc-concretizes-means-bearer-generically-depends

∀ g,b,sdc (∃ t instanceOf(g,genericallyDependentContinuant,t )
∧ ∃ t instanceOf(sdc,specificallyDependentContinuant,t )
∧ ∃ t instanceOf(b,independentContinuant,t ) → ∀ t (concretizes(sdc,g,t ) (4)
∧ inheresIn(sdc,b )
→ genericallyDependsOn(g,b,t )))


∀ g,b,sdc (∃ t instanceOf(g,genericallyDependentContinuant,t )
         ∧ ∃ t instanceOf(sdc,specificallyDependentContinuant,t )
         ∧ ∃ t instanceOf(b,independentContinuant,t ) 
    → ∀ t (concretizes(sdc,g,t ) ∧ inheresIn(sdc,b ) → genericallyDependsOn(g,b,t )))

I'm willing to break at
 between top implication 
 first level conjunction


above:
1. decide that we should split at top implication 
2. Is ant too long? yes
   is it a conjunction?
   split to conjuncts as fit 
3. Is cons too long
no send line is -> and cons
yes 

rules for split-at -> <-> (second line a little before first line)
, A V as many on a line as possible, then overlaps aligning to first conjunct


(setq a (loop with counts 
	   for ax in (collect-axioms-from-spec bfo::*everything-theory*) 
	   with g = (make-instance 'logic::fol-text-logic-generator)
		   with table = (make-hash-table :test 'equalp)
	   for head = (and (eq (car (axiom-sexp ax)) :forall)
			   (car (third (axiom-sexp ax))))
	      with op =  :iff
	      for ant = (and (eq head op)  (render-axiom g  (make-instance 'logic::axiom :sexp  (second (third (axiom-sexp ax))) :dont-validate t)))
	      for cons = (and (eq head op)  (render-axiom g  (make-instance 'logic::axiom :sexp  (third (third (axiom-sexp ax))) :dont-validate t)))
	      when (eq head op) 
		do (push (list ant cons (axiom-name ax)) (gethash (cons (length ant) (length cons)) table))
		   finally (return table)))


∀ o1,o2 (∃ t1,t2 ((occupiesTemporalRegion(o1,t1 ) ∨ temporallyProjectsOnto(o1,t1 ) ∨ t1 = o1)
∧ (occupiesTemporalRegion(o2,t2 ) ∨ temporallyProjectsOnto(o2,t2 ) ∨ t2 = o2) ∧ precedes(t1,t2 ))
↔ precedes(o1,o2 ))

∃t1,t2 ((occupiesTemporalRegion(o1,t1) ∨ temporallyProjectsOnto(o1,t1) ∨ t1=o1)
      ∧ (occupiesTemporalRegion(o2,t2) ∨ temporallyProjectsOnto(o2,t2) ∨ t2=o2)
      ∧ precedes(t1,t2))
  ↔ precedes(o1,o2)

\newlength{\mylength}
\settowidth{\mylength}{$\forall\, o1{,}o2\, \text{\textbf{(}}\exists\, t1{,}t2$}
\newlength{\mylengtha}
\settowidth{\mylengtha}{$\forall\, o1{,}o2\,$}

\hbox{$  \forall\, o1{,}o2\, \text{\textbf{(}}\exists\, t1{,}t2\, \text{\textbf{(}}\text{\textbf{(}}\text{occupiesTemporalRegion}(\text{{\it o1}}{,}\text{{\it t1}}\,) \lor \text{temporallyProjectsOnto}(\text{{\it o1}}{,}\text{{\it t1}}\,) \lor t1 = o1 \text{\textbf{)}} $}
\hbox{\hspace{\mylength}$ \land \text{\textbf{(}}\text{occupiesTemporalRegion}(\text{{\it o2}}{,}\text{{\it t2}}\,) \lor \text{temporallyProjectsOnto}(\text{{\it o2}}{,}\text{{\it t2}}\,)\lor t2 = o2 \text{\textbf{)}}$} 
\hbox{\hspace{\mylength}$\land \text{precedes}(\text{{\it t1}}{,}\text{{\it t2}}\,) \text{\textbf{)}} $}
\hbox{\hspace{\mylengtha}$\leftrightarrow \text{precedes}(\text{{\it o1}}{,}\text{{\it o2}}\,)\text{\textbf{)}}$}
|#

(defvar *formula-right-margin* 75)

(defun formula-width (sexp)
  (length (render-axiom (make-instance 'fol-text-logic-generator)  (make-instance 'logic::axiom :sexp sexp :dont-validate t))))
		      
(defun find-implication-position (sexp rendered)
  (position #\→ rendered))

#|(loop with counts 
	   for ax in (collect-axioms-from-spec *everything-theory*) 
	   for sexp = (axiom-sexp ax)
      for string = (logic::split-formula sexp)
      when string do (print string))
|#

(defun split-formula (sexp)
  (cond ((< (formula-width sexp) *formula-right-margin*) nil)
	((and (eq (car sexp) :forall)
	      (eq (car (third sexp)) :implies))
	 (let ((a (formula-width (second (third sexp))))
	       (b (formula-width (third (third sexp)))))
	   (if (and (< a *formula-right-margin*)
		    (< b *formula-right-margin*))
	       (let* ((rendered (render-axiom (make-instance 'fol-text-logic-generator) sexp))
		      (implication-position (find-implication-position sexp rendered))
		      (part-1 (subseq rendered 0 (1- implication-position)))
		      (part-2 (subseq rendered (1- implication-position)))
		      (first-paren (position #\( rendered)))
		 (format nil "~a~%~a~a~%" 
			 part-1
			 (subseq "                " 0 (1- first-paren)) part-2))
	       (progn (pprint sexp)
		      (list a b)))))))
		
(defun split-group (already-indent limit conjuncts)
       (loop with lines 
	     with line
	     for c in conjuncts
	     for length = (logic::formula-width c)
	     with right = already-indent
	     do 
		(if (null line)
		    (progn
		      (setq line (list c))
		      (incf right length))
		    (if (> (+ right length) limit)
			(progn 
			  (push line lines)
			  (setq line (list c))
			  (setq right (+ already-indent length)))
			(progn 
			  (setq line (append line (list c)))
			  (incf right length))))
	     finally 
		(progn (when line (push line lines))
		       (return (reverse lines)))))



;; implications get split if whole is too long
;; con/dis-junctions are packed as many per line as possible
;; = not, not= never touched
;; quantifiers stick to their body


(defun pprint-and-or (stream sexp)
  (pprint-logical-block (stream sexp)
    (let ((sep (if (eq (car sexp) :and) s-and s-or)))
      (pprint-pop)
      (loop
	(let ((next (pprint-pop)))
					;	  (pprint-indent :block -2 stream)
	  (pprint-w-paren-if-not-singular stream next))
	(pprint-exit-if-list-exhausted) 
	(pprint-newline :fill stream)
					;	  (pprint-indent :block 0 stream)
	(write-char #\space stream)
	(write-string sep stream)
	(write-char #\space stream)
	))))

(defun pprint-implication (stream sexp)
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
	(if (member (car (third sexp)) '(:iff :implies))
	    (pprint-logical-block (stream cons :prefix "(" :suffix ")")
	      (write cons :stream stream))
	    (write cons :stream stream))
	))))

(defun pprint-function (stream sexp)
  (pprint-logical-block (stream sexp)
      (write (pprint-pop) :stream stream)
      (pprint-logical-block (stream (cdr sexp) :prefix "(" :suffix ")")
      (loop (write (pprint-pop) :stream stream )
	    (pprint-exit-if-list-exhausted) 
	    (write-char #\, stream))
      )))

(defun pprint-not (stream sexp)
  (pprint-logical-block (stream sexp)
    (write-string s-not stream)
    (pprint-w-paren-if-not-singular stream (second sexp))))

(defun pprint-= (stream sexp)
  (pprint-logical-block (stream sexp)
    (write (second sexp) :stream stream)
    (if (eq (car sexp) ':=)
	(write-string s-= stream)
	(write-string s-not= stream))
    (write (third sexp) :stream stream)
    ))

(defun pprint-w-paren-if-not-singular (stream form &optional indent)
  (if (singular form)
	(pprint-logical-block (stream form)
	  (when indent (pprint-indent :block indent stream)) 
	  (write form :stream stream))
	(pprint-logical-block (stream form :prefix "(" :suffix ")")
	  (when indent (pprint-indent :block indent stream))
	  (write form :stream stream))))

(defun pprint-quantified (stream sexp)
  (pprint-logical-block (stream sexp)
    (if (eq (pprint-pop) :forall)
	(write-string s-forall stream)
	(write-string s-exists stream))
    (pprint-logical-block (stream (second sexp))
      (loop (write (pprint-pop) :stream stream )
	    (pprint-exit-if-list-exhausted) 
	    (write-char #\, stream)))
    (write-char #\space stream)
    (pprint-w-paren-if-not-singular stream (third sexp))
    ))

(defun pprint-fact (stream sexp)
  (pprint-function stream (second sexp)))
	
(defparameter *fol-text-pprint-dispatch* (copy-pprint-dispatch))

(set-pprint-dispatch '(cons (member :and :or)) 
  'pprint-and-or 1 *fol-text-pprint-dispatch*)
(set-pprint-dispatch '(cons (member :exists :forall)) 
  'pprint-quantified 1 *fol-text-pprint-dispatch*)
(set-pprint-dispatch '(cons (member :implies :iff)) 
		     'pprint-implication 1 *fol-text-pprint-dispatch*)
(set-pprint-dispatch '(cons (member := :not=)) 
		     'pprint-= 1 *fol-text-pprint-dispatch*)
(set-pprint-dispatch '(cons (member :not)) 
		     'pprint-not 1 *fol-text-pprint-dispatch*)
(set-pprint-dispatch 'cons 
		     'pprint-function 0 *fol-text-pprint-dispatch*)
(set-pprint-dispatch '(cons (member :fact))
		     'pprint-fact 1 *fol-text-pprint-dispatch*)

(set-pprint-dispatch 'symbol
		     #'(lambda (s id)
			 (let ((name (string-downcase (string id))))
			   (if (char= (char name 0) #\?)
			       (setq name (subseq name 1)))
			   (write-string (cl-user::camelcase name) s)))
		     0 *fol-text-pprint-dispatch*)

(defun ppf (sexp &key (right-margin 70) (stream t))
  (let ((*print-pprint-dispatch* *fol-text-pprint-dispatch*)
	(*print-right-margin* right-margin)
	(*print-pretty* t))
    (pprint-logical-block (stream (axiom-sexp sexp))
      (write (simplify-and-or (axiom-sexp sexp)) :stream stream))))

(defun singular (exp)
  (if (not (formula-sexp-p exp)) ;; it's a relation
      exp 
      (cond ((eq (car exp) :parens) t) ;; it's singular by virtue of already being parenthesized
	    ((eq (car exp) :not)  ;; If the inside of negation is singular, the negation is as well
	     t);(singular (second exp)))
	    ((member (car exp) '(:and :or :implies :iff)) 
		     nil ) ;; these never are singular - syntactically they have more than one element
	    ((member (car exp) '(:forall :exists)) ;; singular if what's in their scope is singular
		     (singular (third exp)))
	    ((member (car exp) '(:= :not=))  ;; = binds tightly, so an equality is singular
	     t)
	    (t (error "what did I forget?")))))

;; (let ((*print-pretty* t) (*print-right-margin* 20))
;;   (pprint-logical-block (t nil) 
;;     (pprint-logical-block (t nil) (write 'uuuuuuuuuuuuuuuutttttttttttttttt :stream t))
;;     (pprint-indent :block 5 t)
;;     (pprint-newline :fill t)
;;     (pprint-logical-block (t nil)  (write 'vvvvvvvvvvvvvvvvuuuuuuuuuuuuuuuu :stream t))))

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

(defun dump-bfo-fol-latex ()
  (with-open-file (f  "~/Desktop/debug.tex" :if-does-not-exist :create :if-exists :supersede :direction :output)
    (let ((*standard-output* f))
      (format f "\\documentclass{article}~%\\usepackage{amsmath}~%\\usepackage{flexisym}~%\\begin{document}\\setlength{\\parindent}{0pt}
~%")

      (loop with counts 
	    for ax in (collect-axioms-from-spec bfo::*everything-theory*) 
	    if (null (ignore-errors (ppl (axiom-sexp ax) 80)))
	     do (warn "error in ~a" (axiom-name ax))
	    else	    do (ignore-errors 
		(format t "~%\\textbf{:~a}" (axiom-name ax))
		(format t "~%\\message{~a}" (axiom-name ax))

		(format f "\\begin{center}\\parbox{0cm}{\\begin{tabbing}~%")
	       (princ (ppl (axiom-sexp ax) 80) f)
	       (format f "\\end{tabbing}}\\end{center}")
	       (format f "~%")))
      (format f "\\end{document}~%"))))


"∀g,b,sdc (∃t instanceOf(g,genericallyDependentContinuant,t)
           ∧ ∃t instanceOf(sdc,specificallyDependentContinuant,t)
           ∧ ∃t instanceOf(b,independentContinuant,t)
        → ∀t (concretizes(sdc,g,t) ∧ inheresIn(sdc,b)
            → genericallyDependsOn(g,b,t)))"

(defun tab-positions-for-foltext (foltext)
  (sort (remove-duplicates
   (loop for line in (cl-user::split-at-char foltext #\newline)
	for space = (position-if-not 'sys::whitespacep line)
	collect space)  :test 'eql) '<))

(defun insert-tab-positions (foltext)
  (let ((positions (tab-positions-for-foltext foltext)))
    (let ((s (with-output-to-string (s)
      (loop for char across foltext
	    for count from 0
	    ;; next line if extends past first
	    if (char= char #\newline)
	      do (setq count 0)  
	    when (member (+ count 1) (cdr positions) :test 'eql)
	      do (write-string "\\=" s) (pop positions) ;; remove position because we've handled it
	    do (write-char char s)))))
      (setq s (#"replaceAll" s s-forall "\\$\\\\forall\\$"))
      (setq s (#"replaceAll" s s-exists "\\$\\\\exists\\$"))
      (setq s (#"replaceAll" s s-and "\\$\\\\land\\$"))
      (setq s (#"replaceAll" s s-or "\\$\\\\lor\\$"))
      (setq s (#"replaceAll" s s-implies "\\$\\\\rightarrow\\$"))
      (setq s (#"replaceAll" s s-iff "\\$\\\\leftrightarrow\\$"))
      (setq s (#"replaceAll" s s-not "\\$\\\\neg\\$"))
;      (setq s (#"replaceAll" s s-= "\\$=\\$"))
      (setq s (#"replaceAll" s "([a-z])(prime)\\b" "$1\\\\textprime"))
      (setq s (#"replaceAll" s s-not= "\\$\\\\neq\\$"))
      )))

(defun insert-leading-tabs (foltext)
  (let ((stops (tab-positions-for-foltext foltext)))
    (with-output-to-string (s)
      (loop for line in (cl-user::split-at-char (insert-tab-positions foltext) #\newline)
	    for space = (position-if-not 'sys::whitespacep line)
	    do (unless (zerop space)
		 (loop repeat (position space stops) do (write-string "\\>" s)))
	       (format s "~a\\\\~%" line s)
	    ))))

(defun ppl (sexp &optional margin)
  (insert-leading-tabs (with-output-to-string (s)   (ppf sexp :right-margin margin :stream s))))
  






