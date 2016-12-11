;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: subsume-bag.lisp
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

;;; notes:
;;; should check sort compatibility of variable and (fn ...) earlier
;;; incomplete identity handling
;;;  variables in terms1 can be bound to identity
;;;  count-arguments, recount-arguments don't eliminate identity
;;; using recount-arguments is somewhat inefficient
;;;  it recompares terms in terms2
;;;  it could check whether terms in terms1 are frozen
;;; use solve-sum instead of solve-sum-solutions?

(defun subsume-bag (cc terms1 terms2 subst fn)
  ;; assume variables of terms2 are already frozen
  ;; eliminate terms in common, find multiplicities
  (subsume-bag0 cc (count-arguments fn terms2 subst (count-arguments fn terms1 subst) -1) subst fn))

(defun subsume-bag0 (cc terms-and-counts subst fn)
  ;; ensure length constraint is satisfiable
  (let ((len1 0) (len2 0) (vars nil) (varc 0))
    (dolist (tc terms-and-counts)
      (let ((c (tc-count tc)))
	(cond
	  ((plusp c)
	   (if (unfrozen-variable-p (tc-term tc))
	       (progn
		 (push c vars)
		 (incf varc c))
	       (incf len1 c)))
	  ((minusp c)
	   (decf len2 c)))))
    (cond
      ((null vars)
       (when (eql len1 len2)
	 (if (eql 0 len1)
	     (funcall cc subst)
	     (subsume-bag1 cc terms-and-counts subst fn))))
      ((if (and (eq none (function-identity2 fn)) (not (function-identity-p1 fn)))
	   (and (<= (+ len1 varc) len2) (solve-sum-p (- len2 len1 varc) vars))
	   (and (<= len1 len2) (solve-sum-p (- len2 len1) vars)))
       (if (eql 0 len1)
	   (subsume-bag2 cc terms-and-counts subst fn)
	   (subsume-bag1 cc terms-and-counts subst fn))))))

(defun subsume-bag1 (cc terms-and-counts subst fn)
  ;; eliminate highest multiplicity nonvariable term in terms1
  ;; by matching it with terms in terms2
  (prog->
    (maxtc1 terms-and-counts subst -> tc1)
;;  (cl:assert tc1)
    (unless (eq 'quit tc1)			;unmatched frozen term in terms1
      (dolist terms-and-counts ->* tc2)
      (when (<= (tc-count tc1) (- (tc-count tc2)))
	(unify (tc-term tc1) (tc-term tc2) subst ->* subst)
	(subsume-bag0 cc (recount-arguments fn terms-and-counts subst) subst fn)))))

(defun subsume-bag2 (cc terms-and-counts subst fn)
  ;; only variables left in terms1
  ;; generate equations to apportion terms in terms2 to variables
  (let ((vars nil) (terms nil) (coefs nil) (boundss nil) (sums nil))
    (dolist (tc terms-and-counts)
      (let ((c (tc-count tc)))
	(when (plusp c)
	  (push (tc-term tc) vars)
	  (push c coefs))))
    (dolist (tc terms-and-counts)
      (let ((c (tc-count tc)))
	(when (minusp c)
	  (setf c (- c))
	  (let* ((term (tc-term tc))
		 (bounds (compute-bounds c coefs vars term subst fn)))
	    (when (and bounds (loop for b in bounds always (eql 0 b)))
	      (return-from subsume-bag2))	;can't match term
	    (push term terms)
	    (push bounds boundss)
	    (push c sums)))))
    (subsume-bag3 cc vars terms coefs boundss sums subst fn)))

(defun subsume-bag3 (cc vars terms coefs boundss sums subst fn)
  ;; solve equations that apportion all occurrences of each term among variables
  (subsume-bag4
    cc
    vars
    (consn nil nil (length vars))
    terms
    (loop for bounds in boundss
	  as sum in sums
	  collect (or (solve-sum-solutions sum coefs bounds)
		      (return-from subsume-bag3)))
    subst
    fn))

(defun subsume-bag4 (cc vars vals terms solss subst fn)
  ;; generate substitutions from equation solutions
  (cond
    ((null terms)
     (let ((identity (function-identity2 fn))
           (bup (function-identity-p1 fn))	;retain fn in bindings for bag-union terms
           (fn-sort (associative-function-sort fn)))
       (unless (and (eq none identity) (not bup) (member nil vals))
         (do ((vars vars (rest vars))
              (vals vals (rest vals)))
             ((null vars)
              (funcall cc subst))
           (let ((var (first vars))
                 (val (first vals)))
             (cond
              ((and (null val) (not bup))
               (if (constant-sort-p identity (variable-sort var))
                   (setf subst (bind-variable-to-term var identity subst))
                   (return)))
              ((and (null (rest val)) (not bup))
               ;; already checked sort compatibility in compute-bounds
               (setf subst (bind-variable-to-term var (first val) subst)))
              (t
               ;; it would be more efficient to check sort compatibility earlier
               (if (subsort? fn-sort (variable-sort var))
                   (setf subst (bind-variable-to-term var (make-compound* fn val) subst))
                   (return)))))))))
    (t
     (let ((term (pop terms)))
       (dolist (sol (pop solss))
	 (subsume-bag4
	   cc
	   vars
	   (mapcar (lambda (val)
                     (let ((k (pop sol)))
                       (if (or (null k) (eql 0 k))
                           val
                           (consn term val k))))
           vals)
	   terms
	   solss
	   subst
	   fn))))))

(defun maxtc1 (terms-and-counts subst)
  ;; find term-and-count for nonvariable term with maximum positive count
  (let ((maxtc1 nil))
    (dolist (tc terms-and-counts)
      (let ((c (tc-count tc)))
	(when (plusp c)
	  (let ((term (tc-term tc)))
	    (cond
	      ((unfrozen-variable-p term)
	       )
	      ((frozen-p term subst)
	       (return-from maxtc1 'quit))
	      ((or (null maxtc1) (> c (tc-count maxtc1)))
	       (setf maxtc1 tc)))))))
    maxtc1))

(defun compute-bounds (sum coefs vars term subst fn)
  ;; set bound of zero for variables of too high multiplicity or that occur in term
  (prog->
    (mapcar coefs vars ->* coef var)
    (cond
     ((or (> coef sum) (variable-occurs-p var term subst))
      0)
     ((function-boolean-valued-p fn)
      nil)
     (t
      (variable-sort var -> sort)
      (cond
       ((top-sort? sort)
        nil)
       ((not (subsort? (term-sort term subst) sort))
        0)
       ((not (subsort? (associative-function-sort fn) sort))
        1)
       (t
        nil))))))
  
;;; subsume-bag.lisp EOF
