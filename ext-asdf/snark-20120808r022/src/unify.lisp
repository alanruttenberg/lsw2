;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: unify.lisp
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
;;; Portions created by the Initial Developer are Copyright (C) 1981-2010.
;;; All Rights Reserved.
;;;
;;; Contributor(s): Mark E. Stickel <stickel@ai.sri.com>.

(in-package :snark)

(declaim (special *subsuming*))

(defvar *unify-special* t)

(defstruct special-unification-problem
  algorithms
  term1
  term2)

(defun unify (cc term1 term2 &optional subst)
  (macrolet
    ((unify-variable*constant (u v)
       `(if (and (not (variable-frozen-p ,u))
		 (constant-sort-p ,v (variable-sort ,u)))
	    (setf subst (bind-variable-to-term ,u ,v subst))
	    (return-from unify)))
     (unify-variable*compound (u v)
       `(if (and (not (variable-frozen-p ,u))
		 (if (embedding-variable-p ,u)
		     (not (embedding-variable-occurs-p (args ,v) subst))
		     (not (variable-occurs-p ,u (args ,v) subst)))
		 (let ((s (variable-sort ,u)))
                   (or (top-sort? s)
                       (subsort? (compound-sort ,v subst) s))))
	    (setf subst (bind-variable-to-term ,u ,v subst))
	    (return-from unify))))
    (prog ((args1 nil) (args2 nil) (moreterms1 nil) (moreterms2 nil) oterm1 oterm2
	   (special-unification-problems nil) algrthm temp1 temp2
           (tracing (trace-unify?)))
       (when tracing
         (let ((cc1 cc))
           (setf cc (lambda (subst)
                      (format t "~2%RESULT = ~A" subst)
                      (funcall cc1 subst)))))
       loop
       (when tracing
         (format t "~2%TERM1 = ~A" term1)
         (format t ";  ARGS1 = ~A" args1)
         (format t ";  MORETERMS1 = ~A" moreterms1)
         (format t "~1%TERM2 = ~A" term2)
         (format t ";  ARGS2 = ~A" args2)
         (format t ";  MORETERMS2 = ~A" moreterms2)
	 (format t "~1%SPECIAL = ~A"
                 (mapcar (lambda (x)
                           (make-compound
                            *=*
                            (special-unification-problem-term1 x)
                            (special-unification-problem-term2 x)))
                         special-unification-problems))
         (format t "~1%SUBST = ~A" subst))
	  (cond
	    ((eql term1 term2)
	     )
	    (t
	     (dereference2
	       term1 term2 subst
	       :if-variable*variable (cond
				       ((eq term1 term2)
					)
				       ((and (embedding-variable-p term1) (embedding-variable-p term2))
					(return-from unify))
				       ((variable-frozen-p term1)
					(if (and (not (variable-frozen-p term2))
						 (subsort? (variable-sort term1) (variable-sort term2)))
					    (setf subst (bind-variable-to-term term2 term1 subst))
					    (return-from unify)))
				       ((variable-frozen-p term2)
					(if (subsort? (variable-sort term2) (variable-sort term1))
					    (setf subst (bind-variable-to-term term1 term2 subst))
					    (return-from unify)))
				       (t
					(when (prefer-to-bind-p term2 term1)
					  (psetq term1 term2 term2 term1))
					(let ((sterm1 (variable-sort term1))
                                              (sterm2 (variable-sort term2)))
                                          (cond
                                           ((subsort? sterm2 sterm1)
                                            (setf subst (bind-variable-to-term term1 term2 subst)))
                                           ((subsort? sterm1 sterm2)
                                            (setf subst (bind-variable-to-term term2 term1 subst)))
                                           (t
                                            (let ((sz (sort-intersection sterm1 sterm2)))
                                              (if (null sz)
                                                  (return-from unify)
                                                  (let ((z (make-variable sz)))
                                                    (setf subst (bind-variable-to-term term2 z (bind-variable-to-term term1 z subst)))))))))))
	       :if-compound*compound (unless (eq term1 term2)
				       (cond
                                         ((neq (setf temp1 (head term1)) (head term2))
                                          (return-from unify))
                                         ((eq *cons* temp1)
                                          (unless (eq (setf temp1 (cdr term1)) (setf temp2 (cdr term2)))
                                            (push temp1 moreterms1)
					    (push temp2 moreterms2))
                                          (setf term1 (car term1) term2 (car term2))
                                          (go loop))
					 (t
                                          (setf oterm1 term1 oterm2 term2)
					  (setf term1 (argsa term1) term2 (argsa term2) algrthm (function-unify-code temp1))
				          (cond
					   ((not algrthm)
					    (cond
					      ((or args1 args2)
					       (push term1 moreterms1)
					       (push term2 moreterms2))
					      (t
					       (setf args1 term1)
					       (setf args2 term2))))
					   ((or (null *unify-special*)	;might-unify-p ignores some special-unification problems
					        (and (consp *unify-special*)
						     (not (subsetp algrthm *unify-special*))))
					    )
					   ((or args1 args2 moreterms1 special-unification-problems)
					    (push (make-special-unification-problem :algorithms algrthm :term1 oterm1 :term2 oterm2)
						  special-unification-problems))
					   (t
					    (dolist (fun algrthm)
					      (funcall fun cc oterm1 oterm2 subst))
					    (return-from unify))))))
	       :if-constant*constant (unless (eql term1 term2)
				       (return-from unify))
	       :if-variable*compound (unify-variable*compound term1 term2)
	       :if-compound*variable (unify-variable*compound term2 term1)
	       :if-variable*constant (unify-variable*constant term1 term2)
	       :if-constant*variable (unify-variable*constant term2 term1)
	       :if-compound*constant (return-from unify)
	       :if-constant*compound (return-from unify))))
	  ;; term1 and term2 have been unified
	  (cond
	    (args1
	     (cond
	       (args2
		(setf term1 (pop args1))
		(setf term2 (pop args2))
		(go loop))
	       (t
		(return-from unify))))
	    (args2
	     (return-from unify))
	    (moreterms1
	     (setf term1 (pop moreterms1))
	     (setf term2 (pop moreterms2))
	     (go loop))
	    (special-unification-problems
	     (unify-special cc special-unification-problems subst))
	    (t
	     (funcall cc subst))))))

(defun unify-p (x y &optional subst)
  (prog->
    (unify x y subst ->* subst)
    (declare (ignore subst))
    (return-from unify-p t))
  nil)

(defun might-unify-p (x y &optional subst)
  ;; returns nil if x and y are definitely not unifiable
  ;; used by unify-bag to identify nonunifiable arguments
  (let ((*unify-special* '(unify-commute)))
    (unify-p x y subst)))

(defun unifiers (x y &optional subst)
  (let ((unifiers nil) unifiers-last)
    (prog->
      (unify x y subst ->* subst)
      (collect subst unifiers))
    unifiers))

(defun unify-special (cc special-unification-problems subst)
  (prog->
    (first special-unification-problems -> x)
    (rest special-unification-problems -> l)
    (cond
      ((null l)
       (dolist (special-unification-problem-algorithms x) ->* fun)
       (funcall fun (special-unification-problem-term1 x) (special-unification-problem-term2 x) subst ->* subst)
       (funcall cc subst))
      (t
       (dolist (special-unification-problem-algorithms x) ->* fun)
       (funcall fun (special-unification-problem-term1 x) (special-unification-problem-term2 x) subst ->* subst)
       (unify-special cc l subst)))))

(defun commutative-unify (cc x y subst)
  (let* ((terms1 (args x))
         (terms2 (args y))
         (x1 (first terms1)) (l1 (rest terms1)) (y1 (first l1)) (z1 (rest l1))
	 (x2 (first terms2)) (l2 (rest terms2)) (y2 (first l2)) (z2 (rest l2)))
    ;; terms1 = (x1 . l1) = (x1 y1 . z1)
    ;; terms2 = (x2 . l2) = (x2 y2 . z2)
    (cond
      ((equal-p x1 x2 subst)
       (unify cc l1 l2 subst))
      ((equal-p x1 y2 subst)
       (unify cc l1 (cons x2 z2) subst))
      ((equal-p y1 x2 subst)
       (unify cc (cons x1 z1) l2 subst))
      ((equal-p y1 y2 subst)
       (unify cc (cons x1 z1) (cons x2 z2) subst))
      (t
       (unify cc terms1 terms2 subst)
       (unless (or (equal-p x1 y1 subst)
		   (equal-p x2 y2 subst))
	 (unify cc terms1 (list* y2 x2 z2) subst))))))

(defun dont-unify (cc x y subst)
  ;; can use this to prevent resolution of list-to-atom formulas, for example
  (cond
   (*subsuming*
    (unify cc (args x) (args y) subst))
   ((equal-p x y subst)
    (funcall cc subst))))

;;; unify.lisp EOF
