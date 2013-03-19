;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: rewrite-code.lisp
;;; The contents of this file are subject to the Mozilla Public License
;;; Version 1.1 (the "License"); you may not use this file except in
;;; compliance with the License. You may obtain a copy of the License at
;;; http://www.mozilla.org/MPL/
;;;
;;; Software distributed under the License is distributed on an "AS IS"
;;; basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
;;; License for the specific language governing rights and limitations
;;; under the License.
;;;
;;; The Original Code is SNARK.
;;; The Initial Developer of the Original Code is SRI International.
;;; Portions created by the Initial Developer are Copyright (C) 1981-2012.
;;; All Rights Reserved.
;;;
;;; Contributor(s): Mark E. Stickel <stickel@ai.sri.com>.

(in-package :snark)

(defun equality-rewriter (atom subst)
  ;; (= t t) -> true
  ;; (= t s) -> false if t and s are headed by different constructors
  ;; (= (f t1 ... tn) (f s1 ... sn)) -> (and (= t1 s1) ... (= tn sn)) if f is injective
  ;; (= t s) -> false if t and s have disjoint sorts
  ;; also try equality-rewrite-code functions for (= (f ...) (f ...))
  ;; none otherwise
  (mvlet ((*=* (head atom))
          ((list x y) (args atom)))
    (or (dereference2
         x y subst
         :if-variable*variable (cond
                                ((eq x y)
                                 true))
         :if-constant*constant (cond
                                ((eql x y)
                                 true))
         :if-compound*compound (cond
                                ((equal-p x y subst)
                                 true)
                                (t
                                 (let ((fn1 (head x)) (fn2 (head y)))
                                   (cond
                                    ((eq fn1 fn2)
                                     (cond
                                      ((dolist (fun (function-equality-rewrite-code fn1) nil)
                                         (let ((v (funcall fun atom subst)))
                                           (unless (eq none v)
                                             (return v)))))
                                      ((function-associative fn1)
                                       nil)
                                      ((and (function-constructor fn1) (function-commutative fn1))
                                       (let ((xargs (args x))
                                             (yargs (args y)))
                                         (if (length= xargs yargs)
                                             (conjoin (let ((x1 (first xargs)) (x2 (second xargs))
                                                            (y1 (first yargs)) (y2 (second yargs)))
                                                        (disjoin (conjoin (make-equality x1 y1) (make-equality x2 y2) subst)
                                                                 (conjoin (make-equality x1 y2) (make-equality x2 y1) subst)
                                                                 subst))
                                                      (conjoin* (mapcar #'make-equality (rrest xargs) (rrest yargs)) subst)
                                                      subst)
                                             false)))
                                      ((function-injective fn1)
                                       (let ((xargs (args x))
                                             (yargs (args y)))
                                         (if (length= xargs yargs)
                                             (conjoin* (mapcar #'make-equality xargs yargs) subst)	;may result in nonclause
                                             false))))))))))
        (let ((xconstant nil) (xcompound nil) (xconstructor nil) xsort
              (yconstant nil) (ycompound nil) (yconstructor nil) ysort)
          (dereference
           x nil
           :if-constant (setf xconstant t xconstructor (constant-constructor x))
           :if-compound (setf xcompound t xconstructor (function-constructor (head x))))
          (dereference
           y nil
           :if-constant (setf yconstant t yconstructor (constant-constructor y))
           :if-compound (setf ycompound t yconstructor (function-constructor (head y))))
          (cond
           ((or (and xconstructor yconstructor (implies (and xcompound ycompound) (neq (head x) (head y))))
                (sort-disjoint?
                 (setf xsort (if xcompound (compound-sort x subst) (if xconstant (constant-sort x) (variable-sort x))))
                 (setf ysort (if ycompound (compound-sort y subst) (if yconstant (constant-sort y) (variable-sort y)))))
                (and (not (same-sort? xsort ysort))
                     (or (and xconstructor (not (subsort? xsort ysort)) (not (same-sort? xsort (sort-intersection xsort ysort))))
                         (and yconstructor (not (subsort? ysort xsort)) (not (same-sort? ysort (sort-intersection xsort ysort))))))
                (and xconstructor
                     xcompound
                     (cond
                      (yconstant (constant-occurs-below-constructor-p y x subst))
                      (ycompound (compound-occurs-below-constructor-p y x subst))
                      (t         (variable-occurs-below-constructor-p y x subst))))
                (and yconstructor
                     ycompound
                     (cond
                      (xconstant (constant-occurs-below-constructor-p x y subst))
                      (xcompound (compound-occurs-below-constructor-p x y subst))
                      (t         (variable-occurs-below-constructor-p x y subst)))))
            false)))
        none)))

(defun make-characteristic-atom-rewriter (pred sort)
  (setf sort (the-sort sort))
  (lambda (atom subst)
    (let ((term (arg1 atom)) s)
      (or (dereference
           term subst
           :if-variable (progn (setf s (variable-sort term)) nil)
           :if-constant (cond
                         ((funcall pred term)
                          true)
                         ((constant-constructor term)
                          false)
                         (t
                          (progn (setf s (constant-sort term)) nil)))
           :if-compound-cons (cond
                              ((funcall pred term)	;for pred being listp or consp
                               true)
                              (t
                               false))
           :if-compound-appl (cond
                              ((funcall pred term)	;for pred being bagp
                               true)
                              ((function-constructor (head term))
                               false)
                              (t
                               (progn (setf s (compound-sort term subst)) nil))))
          (cond
;;         ((subsort? s sort)
;;          true)
           ((sort-disjoint? s sort)
            false))
          none))))

(defun reflexivity-rewriter (atom subst)
  ;; example: this is called when trying to rewrite (rel a b) after
  ;; doing (declare-relation 'rel 2 :rewrite-code 'reflexivity-rewriter)
  ;; (rel a b) -> true after unifying a and b
  ;; returns new value (true) or none (no rewriting done)
  (let ((args (args atom)))
    (if (equal-p (first args) (second args) subst) true none)))

(defun irreflexivity-rewriter (atom subst)
  ;; example: this is called when trying to rewrite (rel a b) after
  ;; doing (declare-relation 'rel 2 :rewrite-code 'irreflexivity-rewriter)
  ;; (rel a b) -> false after unifying a and b
  ;; returns new value (false) or none (no rewriting done)
  (let ((args (args atom)))
    (if (equal-p (first args) (second args) subst) false none)))

(defun associative-identity-rewriter (term subst)
  ;; remove identities from argument list
  ;; eliminate head when less than two arguments
  (let* ((head (head term))
         (identity (function-identity head)))
    (unless (eq none identity)
      (labels
        ((simp (args)
           (if (null args)
               nil
               (let* ((y (rest args))
                      (y* (simp y))
                      (x (first args)))
                 (if (dereference x subst :if-constant (eql identity x))
                     y*
                     (if (eq y y*) args (cons x y*)))))))
        (let* ((args (flatargs term))
               (args* (simp args)))
          (cond
           ((null args*)
            identity)
           ((null (rest args*))
            (first args*))
           ((neq args args*)
            (make-compound* head args*))
           (t
            none)))))))

(defun associative-identity-paramodulater (cc term subst0 &optional (collapse (test-option44?)))
  (let* ((head (head term))
         (identity (function-identity head)))
    (unless (eq none identity)
      (labels
        ((param (args subst l)
           (if (null args)
               (unless (eq subst0 subst)
                 (funcall cc (make-a1-compound* head identity (reverse l)) subst))
               (let ((x (first args)))
                 (dereference
                  x subst
                  :if-variable (unless (member x l)
                                 (prog->
                                   (unify x identity subst ->* subst)
                                   (param (rest args) subst l))))
                 (cond
                  ((eql identity x)
                   (param (rest args) subst l))
                  ((implies collapse (null l))
                   (param (rest args) subst (cons x l))))))))
        (param (flatargs term subst0) subst0 nil)))))

(defun nonvariable-rewriter (atom subst)
  (let ((x (arg1 atom)))
    (dereference
     x subst
     :if-variable none
     :if-constant true
     :if-compound true)))

(defun the-term-rewriter (term subst)
  ;; (the sort value) -> value, if value's sort is a subsort of sort
  (let* ((args (args term))
         (arg1 (first args))
         (arg2 (second args)))
    (if (dereference
         arg1 subst
         :if-constant (and (sort-name? arg1) (subsort? (term-sort arg2 subst) (the-sort arg1))))
        arg2
        none)))

(defun not-wff-rewriter (wff subst)
  (declare (ignore subst))
  (let ((arg (arg1 wff)))
    (cond
     ((eq true arg)
      false)
     ((eq false arg)
      true)
     (t
      none))))

(defun and-wff-rewriter (wff subst)
  (let ((wff* (conjoin* (args wff) subst)))
    (if (equal-p wff wff* subst) none wff*)))

(defun or-wff-rewriter (wff subst)
  (let ((wff* (disjoin* (args wff) subst)))
    (if (equal-p wff wff* subst) none wff*)))

(defun implies-wff-rewriter (wff subst)
  (let ((args (args wff)))
    (implies-wff-rewriter1 (first args) (second args) subst)))

(defun implied-by-wff-rewriter (wff subst)
  (let ((args (args wff)))
    (implies-wff-rewriter1 (second args) (first args) subst)))

(defun implies-wff-rewriter1 (x y subst)
    (or (dereference2
	  x y subst
	  :if-variable*variable (cond
				  ((eq x y)
				   true))
	  :if-variable*constant (cond
				  ((eq true y)
				   true)
				  ((eq false y)
				   (negate x subst)))
	  :if-constant*variable (cond
				  ((eq true x)
				   y)
				  ((eq false x)
				   true))
	  :if-constant*constant (cond
				  ((eql x y)
				   true)
				  ((eq true x)
				   y)
				  ((eq false x)
				   true)
				  ((eq true y)
				   true)
				  ((eq false y)
				   (negate x subst)))
	  :if-variable*compound (cond
				  ((and (negation-p y) (eq x (arg1 y)))
				   false))
	  :if-compound*variable (cond
				  ((and (negation-p x) (eq (arg1 x) y))
				   false))
	  :if-constant*compound (cond
				  ((eq true x)
				   y)
				  ((eq false x)
				   true)
				  ((and (negation-p y) (eql x (arg1 y)))
				   false))
	  :if-compound*constant (cond
				  ((eq true y)
				   true)
				  ((eq false y)
				   (negate x subst))
				  ((and (negation-p x) (eql (arg1 x) y))
				   false))
	  :if-compound*compound (cond
				  ((equal-p x y subst)
				   true)
				  ((and (negation-p x) (equal-p (arg1 x) y subst))
				   false)
				  ((and (negation-p y) (equal-p x (arg1 y) subst))
				   false)))
	none))

(defun distributive-law1-p (lhs rhs &optional subst)
  ;; checks if LHS=RHS is of form X*(Y+Z)=(X*Y)+(X*Z) for variables X,Y,Z and distinct function symbols *,+
  (let (fn1 fn2 vars sort)
    (and (dereference
	   lhs subst
	   :if-compound (progn (setf fn1 (head lhs)) t))
	 (dereference
	   rhs subst
	   :if-compound (neq (setf fn2 (head rhs)) fn1))
	 (= (length (setf vars (variables rhs subst (variables lhs subst)))) 3)
	 (same-sort? (setf sort (variable-sort (first vars))) (variable-sort (second vars)))
	 (same-sort? sort (variable-sort (third vars)))
	 (let ((x (make-variable sort))
	       (y (make-variable sort))
	       (z (make-variable sort)))
	   (variant-p (cons (make-compound fn1 x (make-compound fn2 y z))
			    (make-compound fn2 (make-compound fn1 x y) (make-compound fn1 x z)))
		      (cons lhs rhs)
		      subst)))))

(defun cancel1 (eq fn identity terms1 terms2 subst)
  (prog->
    (count-arguments fn terms2 subst (count-arguments fn terms1 subst) -1 -> terms-and-counts cancel)
    (cond
      ((null cancel)
       none)
      (t
       (quote nil -> args1)
       (quote nil -> args2)
       (progn
	 (dolist terms-and-counts ->* v)
	 (tc-count v -> count)
	 (cond
	   ((> count 0)
	    (setf args1 (consn (tc-term v) args1 count)))
	   ((< count 0)
	    (setf args2 (consn (tc-term v) args2 (- count))))))
       (if (or (and (null args1) args2 (null (cdr args2)) (eql identity (car args2)))
	       (and (null args2) args1 (null (cdr args1)) (eql identity (car args1))))	;don't simplify x+0=x
	   none
	   (make-compound eq
			     (make-a1-compound* fn identity args1)
			     (make-a1-compound* fn identity args2)))))))

(defun make-cancel (eq fn identity)
  (lambda (equality subst)
    (prog->
      (args equality -> args)
      (first args -> x)
      (second args -> y)
      (cond
       ((dereference x subst :if-compound (eq fn (head x)))
        (cancel1 eq fn identity (args x) (list y) subst))
       ((dereference y subst :if-compound (eq fn (head y)))
        (cancel1 eq fn identity (list x) (args y) subst))
       (t
        none)))))

(defun declare-cancellation-law (equality-relation-symbol function-symbol identity-symbol)
  (let ((eq (input-relation-symbol equality-relation-symbol 2))
	(fn (input-function-symbol function-symbol 2))
	(id (input-constant-symbol identity-symbol)))
    (declare-relation equality-relation-symbol 2 :locked nil :rewrite-code (make-cancel eq fn id))))

(defun distributivity-rewriter (term subst op2)
  ;; distributes (head term) over op2 (e.g., * over + in (* (+ a b) c))
  ;; flattens argument lists of both operators
  (let* ((head (head term))
         (args (argument-list-a1 head (args term) subst)))
    (cond
     ((member-if #'(lambda (arg) (dereference arg subst :if-compound-appl (eq op2 (heada arg)))) args)
      (labels
        ((distribute (args)
           (if (null args)
               (list nil)
               (let ((l (distribute (rest args)))
                     (arg (first args)))
                 (if (dereference arg subst :if-compound-appl (eq op2 (heada arg)))
                     (prog->
                       (mapcan (argument-list-a1 op2 (args arg) subst) ->* x)
                       (mapcar l ->* y)
                       (cons x y))
                     (prog->
                       (mapcar l ->* y)
                       (cons arg y)))))))
        (make-compound* op2 (mapcar #'(lambda (x) (make-compound* head x)) (distribute args)))))
     (t
      none))))

(defun declare-distributive-law (fn1 fn2)
  (let ((fn1 (input-function-symbol fn1 2))	;sum
	(fn2 (input-function-symbol fn2 2)))	;product
    (declare-function 
     fn2 (function-arity fn2)
     :rewrite-code (lambda (term subst) (distributivity-rewriter term subst fn1)))))

;;; rewrite-code.lisp EOF
