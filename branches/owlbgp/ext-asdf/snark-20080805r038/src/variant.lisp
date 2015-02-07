;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: variant.lisp
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
;;; Portions created by the Initial Developer are Copyright (C) 1981-2006.
;;; All Rights Reserved.
;;;
;;; Contributor(s): Mark E. Stickel <stickel@ai.sri.com>.

(in-package :snark)

(defvar *extended-variant* nil)

(defun variant (cc x y &optional subst matches)
  (macrolet
    ((variant1 (x y)
       `(let ((v (assoc ,x matches)))
	  (cond
	    ((null v)
	     (when (null (rassoc ,y matches))
	       (setf matches (acons ,x ,y matches))))
	    ((eq (cdr v) ,y)
	     t)))))
    (match-term
      x y subst
      :if-constant*constant (cond
			      (*extended-variant*
			       (when (and (same-sort? (constant-sort x) (constant-sort y))
					  (variant1 x y))
				 (funcall cc matches)))
			      ((eql x y)
			       (funcall cc matches)))
      :if-compound*compound (let ((xhead (head x)) (yhead (head y)))
			      (cond
				((and *extended-variant*
				      (not (function-logical-symbol-p xhead))
				      (not (function-logical-symbol-p yhead))
				      (not (eq *cons* xhead))
				      (not (eq *cons* yhead))
                                      (not (equality-relation-symbol-p xhead))
                                      (not (equality-relation-symbol-p yhead)))
				 (when (variant1 xhead yhead)
				   (variantl cc (argsa x) (argsa y) subst matches)))
                                ((neq xhead yhead)
                                 )
                                ((eq *cons* xhead)
                                 (prog->
                                   (variant (car x) (car y) subst matches ->* matches)
                                   (variant cc (cdr x) (cdr y) subst matches)))
				(t
				 (let ((funs (function-variant-code xhead)))
				   (if funs
				       (dolist (fun funs)
					 (funcall fun cc (argsa x) (argsa y) subst matches xhead))
				       (variantl cc (argsa x) (argsa y) subst matches))))))
      :if-variable*variable (when (and (same-sort? (variable-sort x) (variable-sort y))
				       (variant1 x y))
			      (funcall cc matches)))))

(defun variantl (cc x y subst matches)
  (cond
    ((null x)
     (when (null y)
       (funcall cc matches)))
    ((rest x)
     (when (rest y)
       (prog->
	 (variantl (rest x) (rest y) subst matches ->* matches)
	 (variant cc (first x) (first y) subst matches))))
    ((null (rest y))
     (variant cc (first x) (first y) subst matches))))

(defun variant-p (x y &optional subst)
  (prog->
    (variant x y subst ->* matches)
    (return-from variant-p (or matches t)))
  nil)

(defun variant-bag (cc terms1 terms2 subst matches fn)
  (let ((counts1 (count-arguments fn terms1 subst))
	(counts2 (count-arguments fn terms2 subst)))
    (cond
      ((null counts1)
       (when (null counts2)
	 (funcall cc subst)))
      ((null counts2)
       )
      ((null (cdr counts1))
       (when (null (cdr counts2))
	 (variant cc (tc-term (car counts1)) (tc-term (car counts2)) subst matches)))
      ((null (cdr counts2))
       )
      ((and (length= (cddr counts1) (cddr counts2))
	    (submultisetp (let (w)
			    (dolist (tc counts1)
			      (push (tc-count tc) w))
			    w)
			  (let (w)
			    (dolist (tc counts2)
			      (push (tc-count tc) w))
			    w)))
       (variant-bag* cc counts1 counts2 subst matches)))))

(defun variant-bag* (cc counts1 counts2 subst matches)
  (let ((count1 (car counts1)))
    (dolist (count2 counts2)
      (when (eql (tc-count count1) (tc-count count2))
	(cond
	  ((null (cdr counts1))
	   (variant cc (tc-term count1) (tc-term count2) subst matches))
	  (t
	   (prog->
	     (variant (tc-term count1) (tc-term count2) subst matches ->* matches)
	     (variant-bag* cc (cdr counts1) (remove count2 counts2) subst matches))))))))

(defun variant-commute (cc terms1 terms2 subst matches fn)
  ;; It is assumed that commutative functions that are not assocative
  ;; have at least two arguments only the first two of which commute.
  (declare (ignore fn))
  (variantl cc terms1 terms2 subst matches)
  (variantl cc terms1 (list* (second terms2) (first terms2) (cddr terms2)) subst matches))

(defun variant-vector (cc terms1 terms2 subst matches fn)
  (and (or *extended-variant* (similar-argument-list-ac1-p fn terms1 terms2 subst))
       (variantl cc
                 (argument-list-a1 fn terms1 subst)
                 (argument-list-a1 fn terms2 subst)
                 subst
                 matches)))

;;; variant.lisp EOF
