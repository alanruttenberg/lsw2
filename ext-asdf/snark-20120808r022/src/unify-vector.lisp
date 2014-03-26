;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: unify-vector.lisp
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

;;; unify-vector implements incomplete associative unification
;;; complete associative unification is infinitary

(defun first-and-rest-of-vector (terms subst fn identity)
  (cond
    ((null terms)
     (values none nil))
    (t
     (let ((term (first terms)))
       (dereference
	 term subst
	 :if-compound (when (eq fn (head term))
			(return-from first-and-rest-of-vector
			  (first-and-rest-of-vector (append (args term) (rest terms)) subst fn identity)))
	 :if-constant (when (eql identity term)
			(return-from first-and-rest-of-vector
			  (first-and-rest-of-vector (rest terms) subst fn identity))))
       (values term (rest terms))))))

(defun unify-identity-with-vector (cc terms subst fn identity)
  (let ((vars nil) term)
    (loop
      (setf (values term terms) (first-and-rest-of-vector terms subst fn identity))
      (cond
	((eq none term)
         (dolist (var vars)
           (setf subst (bind-variable-to-term var identity subst)))
	 (funcall cc subst)
         (return))
	((and (unfrozen-variable-p term)
	      (constant-sort-p identity (variable-sort term)))
	 (pushnew term vars))
	(t
	 (return))))))

(defun unify-variable-with-vector (cc var arg args subst fn identity max)
  ;; case where var matches arg plus one or more terms from args
  (when (and (implies max (<= 2 max))
             (subsort? (function-sort fn) (variable-sort var)))
    (let ((l nil)
          (count 0))
      (loop
        (cond
         ((or (eq none arg)
              (not (implies max (>= max count)))
              (variable-occurs-p var arg subst))
          (return))
         (t
          (setf l (append l (list arg)))
          (when (<= 2 (incf count))
            (funcall cc (bind-variable-to-term var (make-compound* fn l) subst) args))
          (setf (values arg args) (first-and-rest-of-vector args subst fn identity))))))))

(defun unify-variable-with-vector-max (args args2 subst fn identity)
  (and (frozen-p args subst)
       (- (+ 1 (argument-count-a1 fn args subst identity))
          (argument-count-a1 fn args2 subst identity t))))

(defun associative-unify (cc x y subst)
  (unify-vector cc (args x) (args y) subst (head x)))

(defun unify-vector (cc args1 args2 subst fn &optional (identity (function-identity2 fn)))
  ;; terminating, incomplete associative unification--no variable splitting
  (prog->
    (first-and-rest-of-vector args1 subst fn identity -> firstargs1 restargs1)
    (first-and-rest-of-vector args2 subst fn identity -> firstargs2 restargs2)
    (cond
      ((eql firstargs1 firstargs2)
       (if (eq none firstargs1)
	   (funcall cc subst)
	   (unify-vector cc restargs1 restargs2 subst fn identity)))
      ((eq none firstargs1)
       (unless (eq none identity)
	 (unify-identity-with-vector cc args2 subst fn identity)))
      ((eq none firstargs2)
       (unless (eq none identity)
	 (unify-identity-with-vector cc args1 subst fn identity)))
      ((and (null restargs1) (null restargs2))
       (unify cc firstargs1 firstargs2 subst))
      (t
       (when (unfrozen-variable-p firstargs1)
	 (unless (eq none identity)
	   (when (constant-sort-p identity (variable-sort firstargs1))
	     (unify-vector cc restargs1 args2 (bind-variable-to-term firstargs1 identity subst) fn identity)))
	 (when restargs2
	   (unify-variable-with-vector
            firstargs1 firstargs2 restargs2 subst fn identity
            (unify-variable-with-vector-max restargs2 restargs1 subst fn identity)
            ->* subst restargs2)
	   (unify-vector cc restargs1 restargs2 subst fn identity)))
       (when (unfrozen-variable-p firstargs2)
	 (unless (eq none identity)
	   (when (constant-sort-p identity (variable-sort firstargs2))
	     (unify-vector cc args1 restargs2 (bind-variable-to-term firstargs2 identity subst) fn identity)))
	 (when restargs1
	   (unify-variable-with-vector
            firstargs2 firstargs1 restargs1 subst fn identity
            (unify-variable-with-vector-max restargs1 restargs2 subst fn identity)
            ->* subst restargs1)
	   (unify-vector cc restargs1 restargs2 subst fn identity)))
       (unless (and (or (null restargs1) (null restargs2)) (eq none identity))
	 (if (and (compound-appl-p firstargs1)
		  (compound-appl-p firstargs2)
		  (eq (heada firstargs1) (heada firstargs2))
		  (or (special-unify-p firstargs1 subst)
		      (special-unify-p firstargs2 subst)))
	     (prog->
	       (unify-vector restargs1 restargs2 subst fn ->* subst)
	       (unify cc firstargs1 firstargs2 subst))
	     (prog->
	       (unify firstargs1 firstargs2 subst ->* subst)
	       (unify-vector cc restargs1 restargs2 subst fn identity))))))))

;;; unify-vector.lisp EOF
