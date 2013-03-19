;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: knuth-bendix-ordering.lisp
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

(defmacro variable-kbo-weight (variable)
  (declare (ignore variable))
  `(kbo-variable-weight?))

(defun kbo-minimum-weight-of-term (term subst)
  (dereference 
    term subst
    :if-variable (variable-kbo-weight term)
    :if-constant (constant-kbo-weight term)
    :if-compound (let ((head (head term))
		       (args (args term))
		       n)
		   (let ((w (function-kbo-weight head)))
		     (cond
		       ((consp w)
			(setf n (car w))
			(loop for arg in args
			      as weight in (cdr w)
			      do (incf n (* (kbo-minimum-weight-of-term arg subst) weight))))
		       (t
			(setf n w)
			(when (function-associative head)
			  (setf n (* (- (max 2 (length args)) 1) n)))	;DO SOMETHING DIFFERENT FOR ZERO OR ONE ARGS
			(loop for arg in args
			      do (incf n (kbo-minimum-weight-of-term arg subst))))))
		   n)))

(defun count-variables (term subst &optional (inc 1) counts)
  (dereference
   term subst
   :if-variable (let ((v (assoc/eq term counts)))
                  (cond
                   ((null v)
                    (acons term inc counts))
                   (t
                    (incf (cdr v) inc)
                    counts)))
   :if-constant counts
   :if-compound (cond
                 ((ground-p term subst)
                  counts)
                 (t
                  (dolist (arg (args term) counts)
		    (setf counts (count-variables arg subst inc counts)))))))

(defun kbo-count-variables (term subst &optional (inc 1) counts)
  (dereference
   term subst
   :if-variable (let ((v (assoc/eq term counts)))
                  (cond
                   ((null v)
                    (acons term inc counts))
                   (t
                    (incf (cdr v) inc)
                    counts)))
   :if-constant counts
   :if-compound (cond
                 ((ground-p term subst)
                  counts)
                 (t
                  (let ((w (function-kbo-weight (head term))))
		    (cond
		     ((consp w)
		      (loop for arg in (args term)
			    as weight in (cdr w)
			    do (setf counts (kbo-count-variables arg subst (* weight inc) counts))
			    finally (return counts)))
		     (t
		      (dolist (arg (args term) counts)
			(setf counts (kbo-count-variables arg subst inc counts))))))))))

(defmacro kbo-compare-variable*compound (x y subst <)
  `(cond
     ((variable-occurs-p ,x ,y ,subst)
      ',<)
     (t
      '?)))

(defmacro kbo-compare-constant*compound (x y subst < >)
  `(let ((m (constant-kbo-weight ,x))
	 (n (kbo-minimum-weight-of-term ,y ,subst)))
     (cond
       ((< m n)
	',<)
       ((> m n)
	(cond
	  ((ground-p ,y ,subst)
	   ',>)
	  (t
	   '?)))
       (t
        (ecase (symbol-ordering-compare ,x (head ,y))
          (<
           ',<)
          (>
           (cond
            ((ground-p ,y ,subst)
             ',>)
            (t
             '?)))
          (?
           '?))))))

(defun kbo-compare-terms (x y subst)
  ;; knuth-bendix ordering is controlled by operator weights (>= 0)
  ;; plus an ordering on operator symbols
  ;; each nullary operator must have positive weight
  ;; each unary operator must have positive weight, with the possible exception of the highest indexed operator
  ;; total on ground terms
  ;; if x > y then xs > ys for substitution s
  ;; x > y if y is a subterm of x
  ;; (compare requirements for strong simplification ordering, CADE-8 pp. 142-143)
  (match-term
    x y subst
    :if-variable*constant '?
    :if-constant*variable '?
    :if-variable*compound (kbo-compare-variable*compound x y subst <)
    :if-compound*variable (kbo-compare-variable*compound y x subst >)
    :if-constant*compound (kbo-compare-constant*compound x y subst < >)
    :if-compound*constant (kbo-compare-constant*compound y x subst > <)
    :if-variable*variable (cond
                           ((eq x y)
                            '=)
                           (t
                            '?))
    :if-constant*constant (cond
                           ((eql x y)
                            '=)
                           (t
                            (let ((m (constant-kbo-weight x))
                                  (n (constant-kbo-weight y)))
                              (cond
                               ((> m n)
                                '>)
                               ((< m n)
                                '<)
                               (t
                                (symbol-ordering-compare x y))))))
    :if-compound*compound (cond
                           ((equal-p x y subst)
                            '=)
                           (t
                            (check-variable-counts
                             x y subst
                             (let ((m (kbo-minimum-weight-of-term x subst))
				   (n (kbo-minimum-weight-of-term y subst)))
                               (cond
			        ((> m n)
			         '>)
			        ((< m n)
			         '<)
                                (t
                                 (ecase (symbol-ordering-compare (head x) (head y))
                                   (=
                                    (case (function-arity (head x))
                                      (1
                                       (kbo-compare-terms (arg1 x) (arg1 y) subst))
                                      (otherwise
                                       (ecase (or (function-ordering-status (head x)) (kbo-status?))
                                         ((:multiset :ac)
                                          ;; (unimplemented)
                                          '?)
                                         (:right-to-left
                                          (do ((xargs (reverse (args x)) (rest xargs))
                                               (yargs (reverse (args y)) (rest yargs)))
                                              ((null xargs)	;assumes fixed arity
                                               '=)
                                            (cond
                                             ((equal-p (first xargs) (first yargs) subst)
                                              )
                                             (t
                                              (return (kbo-compare-terms (first xargs) (first yargs) subst))))))
                                         (:left-to-right
                                          (do ((xargs (args x) (rest xargs))
                                               (yargs (args y) (rest yargs)))
                                              ((null xargs)	;assumes fixed arity
                                               '=)
                                            (cond
                                             ((equal-p (first xargs) (first yargs) subst)
                                              )
                                             (t
                                              (return (kbo-compare-terms (first xargs) (first yargs) subst))))))))))
                                   (<
                                    '<)
                                   (>
                                    '>)
                                   (?
                                    '?))))))))))

(defun check-variable-counts (x y subst dir)
  (case dir
    (>
     (dolist (vc (kbo-count-variables y subst -1 (kbo-count-variables x subst)) dir)
       (unless (<= 0 (cdr vc))
         (return '?))))
    (<
     (dolist (vc (kbo-count-variables x subst -1 (kbo-count-variables y subst)) dir)
       (unless (<= 0 (cdr vc))
         (return '?))))
    (otherwise
     dir)))

;;; special handling for associative functions with zero or one arguments????
;;; incompatible with commutative unification?

;;; knuth-bendix-ordering.lisp EOF
