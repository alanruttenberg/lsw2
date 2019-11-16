(in-package :logic)

(defclass fol-text-logic-generator (fol-logic-generator) 
  ((use-camel :accessor use-camel :initform t :initarg :use-camel)))

;; renders as a string, based on fol-logic-generator tokens

(defmethod render-axiom ((g fol-text-logic-generator) (a axiom))
  (with-output-to-string (s)
    (loop for el in (token-list g (rewrite-form-adding-parentheses-as-necessary g (axiom-sexp a)))
	  do (case el
	       ((∀ ∃) (format s "~a" el))
	       (([ {) (write-char #\( s))
	       ((] }) (write-char #\) s))
	       (_ (write-char #\space s))
	       ((∧ ∨ → ↔) (format s " ~a " el))
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



|#
