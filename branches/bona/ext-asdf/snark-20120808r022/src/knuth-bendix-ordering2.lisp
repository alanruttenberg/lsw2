;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: knuth-bendix-ordering2.lisp
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
;;; Portions created by the Initial Developer are Copyright (C) 1981-2011.
;;; All Rights Reserved.
;;;
;;; Contributor(s): Mark E. Stickel <stickel@ai.sri.com>.

(in-package :snark)

;;; this implementation is inspired by
;;; Bernd L\"{o}chner's "Things to Know When Implementing KBO" in JAR (2006)
;;;
;;; extensions:
;;; status to allow not just left-to-right lexical ordering
;;; weight multipliers (must be >= 1) for arguments of ordinary fixed arity functions for linear polynomial ordering
;;;   (declare-function 'commutator 2 :kbo-weight '(5 3 3)) etc. in overbeek1e example
;;; flattening of argument lists for associative functions
;;; argument lists are greater in ordering than their prefixes
;;;
;;; should use integer or rational weights (not floats) for exact arithmetic
;;;
;;; re :multiset status
;;; even if (f 2) exceeds (f 1 1), it cannot exceed (f 1 1 ... 1) for arbitrary number of 1s

(definline variable-kbo-weight (var)
  (let ((w (kbo-variable-weight?)))
    (if (numberp w) w (funcall w var))))

(defun kbo-evaluate-term (term subst mult weight vars)
  (dereference
   term subst
   :if-variable (values (+ weight (* mult (variable-kbo-weight term))) (acons+ term mult vars))
   :if-constant (values (+ weight (* mult (constant-kbo-weight term))) vars)
   :if-compound (let* ((head (head term))
                       (args (args term))
                       (w (function-kbo-weight head))
                       (ws (if (consp w) (rest w) nil))
                       (w (if (consp w) (first w) w)))
                  (cond
                   ((function-associative head)
                    (setf weight (+ weight (* mult w (max 1 (- (length args) 1))))))
                   (t
                    (setf weight (+ weight (* mult w)))))
                  (kbo-evaluate-terms args subst mult weight vars ws))))

(defun kbo-evaluate-terms (terms subst mult weight vars ws)
  (dolist (term terms)
    (setf (values weight vars) (kbo-evaluate-term term subst (if (null ws) mult (* mult (pop ws))) weight vars)))
  (values weight vars))

(defun kbo-compare-terms (x y &optional subst testval (mult 1))
  (dereference2
   x y subst
   :if-variable*variable (if (eq x y)
                             (values '= 0 nil)
                             (values '? (* mult (- (variable-kbo-weight x) (variable-kbo-weight y))) (acons+ x mult (acons+ y (- mult) nil))))
   :if-constant*constant (if (eql x y)
                             (values '= 0 nil)
                             (let ((weight (* mult (- (constant-kbo-weight x) (constant-kbo-weight y)))))
                               (values
                                (cond
                                 ((> weight 0) '>)
                                 ((< weight 0) '<)
                                 (t (symbol-ordering-compare x y)))
                                weight
                                nil)))
   :if-variable*constant (values '? (* mult (- (variable-kbo-weight x) (constant-kbo-weight y))) (acons+ x mult nil))
   :if-constant*variable (values '? (* mult (- (constant-kbo-weight x) (variable-kbo-weight y))) (acons+ y (- mult) nil))
   :if-variable*compound (mvlet (((values weight vars) (kbo-evaluate-term y subst (- mult) (* mult (variable-kbo-weight x)) (acons+ x mult nil))))
                           (values (if (alist-notany-plusp vars) '< '?) weight vars))
   :if-compound*variable (mvlet (((values weight vars) (kbo-evaluate-term x subst mult (* mult (- (variable-kbo-weight y))) (acons+ y (- mult) nil))))
                           (values (if (alist-notany-minusp vars) '> '?) weight vars))
   :if-constant*compound (mvlet (((values weight vars) (kbo-evaluate-term y subst (- mult) (* mult (constant-kbo-weight x)) nil)))
                           (values
                            (cond
                             ((> weight 0) (if (alist-notany-minusp vars) '> '?))
                             ((< weight 0) '<)
                             (t (ecase (symbol-ordering-compare x (head y))
                                  (> (if (alist-notany-minusp vars) '> '?))
                                  (< '<)
                                  (? '?))))
                            weight
                            vars))
   :if-compound*constant (mvlet (((values weight vars) (kbo-evaluate-term x subst mult (* mult (- (constant-kbo-weight y))) nil)))
                           (values
                            (cond
                             ((> weight 0) '>)
                             ((< weight 0) (if (alist-notany-plusp vars) '< '?))
                             (t (ecase (symbol-ordering-compare (head x) y)
                                  (> '>)
                                  (< (if (alist-notany-plusp vars) '< '?))
                                  (? '?))))
                            weight
                            vars))
   :if-compound*compound (cond
                          ((eq x y)
                           (values '= 0 nil))
                          (t
                           (let ((head (head x)))
                             (cond
                              ((not (eq head (head y)))
                               (mvlet* (((values weight vars) (kbo-evaluate-term x subst mult 0 nil))
                                        ((values weight vars) (kbo-evaluate-term y subst (- mult) weight vars)))
                                 (values
                                  (cond
                                   ((> weight 0) (if (alist-notany-minusp vars) '> '?))
                                   ((< weight 0) (if (alist-notany-plusp vars) '< '?))
                                   (t (ecase (symbol-ordering-compare head (head y))
                                        (> (if (alist-notany-minusp vars) '> '?))
                                        (< (if (alist-notany-plusp vars) '< '?))
                                        (? '?))))
                                  weight
                                  vars)))
                              (t
                               (let* ((xargs (args x))
                                      (yargs (args y))
                                      (status (function-kbo-status head))
                                      (w (function-kbo-weight head))
                                      (ws (if (consp w) (rest w) nil))
                                      (w (if (consp w) (first w) w))
                                      (weight 0)
                                      (vars nil)
                                      com)
                                 (cond
                                  ((function-associative head)
                                   (setf xargs (flatten-args head xargs subst))
                                   (setf yargs (flatten-args head yargs subst))))
                                 (ecase status
                                   ((:left-to-right :right-to-left)
                                    (let ((xargs (if (eq :right-to-left status) (reverse xargs) xargs))
                                          (yargs (if (eq :right-to-left status) (reverse yargs) yargs))
                                          (ws (if (null ws) nil (if (eq :right-to-left status) (reverse ws) ws))))
                                      (loop
                                        (cond
                                         ((or (null xargs) (null yargs))
                                          (cond
                                           (xargs
                                            (setf com '>)
                                            (setf (values weight vars) (kbo-evaluate-terms xargs subst mult weight vars ws)))
                                           (yargs
                                            (setf com '<)
                                            (setf (values weight vars) (kbo-evaluate-terms yargs subst (- mult) weight vars ws)))
                                           (t
                                            (setf com '=)))
                                          (return))
                                         ((not (eq '= (setf (values com weight vars) (kbo-compare-terms (first xargs) (first yargs) subst nil (if (null ws) mult (* mult (pop ws)))))))
                                          (setf (values weight vars) (kbo-evaluate-terms (rest xargs) subst mult weight vars ws))
                                          (setf (values weight vars) (kbo-evaluate-terms (rest yargs) subst (- mult) weight vars ws))
                                          (return))
                                         (t
                                          (setf xargs (rest xargs))
                                          (setf yargs (rest yargs)))))))
                                   ((:commutative :multiset)
                                    (cond
                                     ((and (eq :commutative status) (or (rrest xargs) (rrest yargs)))
                                      (setf (values com weight vars)
                                        (kbo-compare-terms (make-compound* *a-function-with-left-to-right-ordering-status*
                                                                           (make-compound *a-function-with-multiset-ordering-status* (first xargs) (second xargs))
                                                                           (rrest xargs))
                                                           (make-compound* *a-function-with-left-to-right-ordering-status*
                                                                           (make-compound *a-function-with-multiset-ordering-status* (first yargs) (second yargs))
                                                                           (rrest yargs))
                                                           subst
                                                           testval
                                                           mult)))
                                     (t
                                      (unless (eq '= (setf com (compare-term-multisets #'kbo-compare-terms xargs yargs subst nil)))
                                        (setf (values weight vars) (kbo-evaluate-terms xargs subst mult weight vars ws))
                                        (setf (values weight vars) (kbo-evaluate-terms yargs subst (- mult) weight vars ws))))))
                                   ((:ac :none)
                                    ;; (unimplemented)
                                    (cond
                                     ((equal-p x y subst)
                                      (setf com '=))
                                     (t
                                      (setf com '?)
                                      (setf (values weight vars) (kbo-evaluate-terms xargs subst mult weight vars ws))
                                      (setf (values weight vars) (kbo-evaluate-terms yargs subst (- mult) weight vars ws))))))
                                 (cond
                                  ((function-associative head)
                                   (setf weight (+ weight (* mult w (- (max 1 (- (length xargs) 1)) (max 1 (- (length yargs) 1))))))))
                                 (values
                                  (cond
                                   ((eq '= com) '=)
                                   ((> weight 0) (if (alist-notany-minusp vars) '> '?))
                                   ((< weight 0) (if (alist-notany-plusp vars) '< '?))
                                   ((eq '> com) (if (alist-notany-minusp vars) '> '?))
                                   ((eq '< com) (if (alist-notany-plusp vars) '< '?))
                                   (t '?))
                                  weight
                                  vars)))))))))

;;; knuth-bendix-ordering2.lisp EOF
