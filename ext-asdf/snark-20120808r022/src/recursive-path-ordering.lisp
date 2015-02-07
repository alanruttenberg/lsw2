;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: recursive-path-ordering.lisp
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

(defvar *rpo-cache*)
(defvar *rpo-cache-numbering*)
(defvar *ac-rpo-cache*)

(defun rpo-compare-terms-top (x y &optional subst testval)
  (let ((*rpo-cache* nil)
        (*rpo-cache-numbering* nil)
        (*ac-rpo-cache* nil))
    (rpo-compare-terms x y subst testval)))

(defun rpo-cache-lookup (x y)
  (and *rpo-cache*
       (let ((x# (funcall *rpo-cache-numbering* :lookup x))
             (y# (funcall *rpo-cache-numbering* :lookup y)))
         (sparef *rpo-cache* x# y#))))

(defun rpo-cache-store (x y com)
  (when com
    (unless *rpo-cache*
      (setf *rpo-cache* (make-sparse-vector))
      (setf *rpo-cache-numbering* (make-numbering)))
    (let ((x# (funcall *rpo-cache-numbering* :lookup x))
          (y# (funcall *rpo-cache-numbering* :lookup y)))
      (setf (sparef *rpo-cache* x# y#) com))))

(definline rpo-compare-variable*compound (x y subst testval)
  (and (or (null testval) (eq '< testval)) (if (variable-occurs-p x y subst) '< '?)))

(definline rpo-compare-compound*variable (x y subst testval)
  (and (or (null testval) (eq '> testval)) (if (variable-occurs-p y x subst) '> '?)))

(defun rpo-compare-terms (x y &optional subst testval)
  (cond
   ((eql x y)
    '=)
   (t
    (dereference2
     x y subst
     :if-variable*variable (if (eq x y) '= '?)                             
     :if-variable*constant '?
     :if-constant*variable '?
     :if-variable*compound (rpo-compare-variable*compound x y subst testval)
     :if-compound*variable (rpo-compare-compound*variable x y subst testval)
     :if-constant*constant (symbol-ordering-compare x y)
     :if-compound*constant (and (neq '= testval) (rpo-compare-compound*constant x y subst testval))
     :if-constant*compound (and (neq '= testval) (rpo-compare-constant*compound x y subst testval))
     :if-compound*compound (rpo-compare-compounds x y subst testval)))))

(defun rpo-compare-compound*constant (compound constant subst testval)
  ;; for a constant to be bigger than a compound,
  ;; constant must be bigger than every constant/function symbol in compound
  ;; and compound must be ground
  ;;
  ;; for a constant to be less than a compound,
  ;; constant must be smaller than or identical to some constant/function symbol in compound
  (let ((can-be-< t))
    (labels
      ((compare-with-term (term)
         (dereference
          term subst
          :if-variable (if (eq '< testval) (return-from rpo-compare-compound*constant nil) (setf can-be-< nil))
          :if-constant (ecase (symbol-ordering-compare term constant)
                         ((> =)
                          (return-from rpo-compare-compound*constant '>))
                         (?
                          (if (eq '< testval) (return-from rpo-compare-compound*constant nil) (setf can-be-< nil)))
                         (<
                          ))
          :if-compound (progn
                         (ecase (symbol-ordering-compare (head term) constant)
                           (>
                            (return-from rpo-compare-compound*constant '>))
                           (?
                            (if (eq '< testval) (return-from rpo-compare-compound*constant nil) (setf can-be-< nil)))
                           (<
                            ))
                         (dolist (arg (args term))
                           (compare-with-term arg))))))
      (let ((head (head compound)))
        (cond
         ((function-boolean-valued-p head)
          (return-from rpo-compare-compound*constant
            (if (constant-boolean-valued-p constant)
		(if (ordering-functions>constants?) '> (symbol-ordering-compare head constant))	;no subterm comparisons
                '>)))					;atom > term
         ((constant-boolean-valued-p constant)
          (return-from rpo-compare-compound*constant '<))			;term < atom
         ((ordering-functions>constants?)
          '>)
         (t
          (ecase (symbol-ordering-compare head constant)
            (>
             (return-from rpo-compare-compound*constant '>))
            (?
             (if (eq '< testval) (return-from rpo-compare-compound*constant nil) (setf can-be-< nil)))
            (<
             ))
          (dolist (arg (args compound))
            (compare-with-term arg))
          (if can-be-< '< '?)))))))

(defun rpo-compare-constant*compound (constant compound subst testval)
  (opposite-order (rpo-compare-compound*constant compound constant subst (opposite-order testval))))

(defun rpo-compare-compounds (x y subst testval)
  (cond
   ((eq x y)
    '=)
   ((test-option19?)
    (rpo-compare-compounds0 x y subst testval))
   (t
    (ecase testval
      (>
       (and (implies (test-option20?) (no-new-variable-occurs-p y subst (variables x subst)))
            (rpo-compare-compounds0 x y subst '>)))
      (<
       (and (implies (test-option20?) (no-new-variable-occurs-p x subst (variables y subst)))
            (rpo-compare-compounds0 x y subst '<)))
      (=
       (let ((xvars (variables x subst))
             (yvars (variables y subst)))
         (and (length= xvars yvars)
              (dolist (v xvars t)
                (unless (member v yvars :test #'eq)
                  (return nil)))
              (rpo-compare-compounds0 x y subst '=))))
      ((nil)
       (let ((xvars (variables x subst))
             (yvars (variables y subst)))
         (dolist (v xvars)
           (unless (member v yvars :test #'eq)
             (setf testval '>)
             (return)))
         (dolist (v yvars)
           (unless (member v xvars :test #'eq)
             (cond
              ((null testval)
               (setf testval '<)
               (return))
              (t
               (return-from rpo-compare-compounds '?))))))
       (let ((v (rpo-compare-compounds0 x y subst testval)))
         (if (or (null testval) (eq testval v)) v '?)))))))

(defun rpo-compare-compounds0 (x y subst testval)
  (let ((fn (head x)))
    (ecase (symbol-ordering-compare fn (head y))
      (=
       (case (function-arity fn)
         (1
          (rpo-compare-terms (arg1 x) (arg1 y) subst testval))
         (otherwise
          (let ((status (function-rpo-status fn)))
            (ecase status
              (:left-to-right
               (rpo-compare-lists x y (args x) (args y) subst testval))
              (:right-to-left
               (rpo-compare-lists x y (reverse (args x)) (reverse (args y)) subst testval))
              ((:commutative :multiset)
               (let ((xargs (args x))
                     (yargs (args y)))
                 (cond
                  ((and (eq :commutative status) (or (rrest xargs) (rrest yargs)))
                   (rpo-compare-terms (make-compound* *a-function-with-left-to-right-ordering-status*
                                                      (make-compound *a-function-with-multiset-ordering-status* (first xargs) (second xargs))
                                                      (rrest xargs))
                                      (make-compound* *a-function-with-left-to-right-ordering-status*
                                                      (make-compound *a-function-with-multiset-ordering-status* (first yargs) (second yargs))
                                                      (rrest yargs))
                                      subst
                                      testval))
                  (t
                   (compare-term-multisets #'rpo-compare-terms xargs yargs subst testval)))))
              (:ac
               (with-clock-on ordering-ac
                 (ac-rpo-compare-compounds fn (flatargs x subst) (flatargs y subst) subst)))
              ((:none)
               ;; (unimplemented)
               (cond
                ((equal-p x y subst)
                 '=)
                (t
                 '?))))))))
      (>
       (and (neq '= testval) (rpo-compare-compounds> x (flatargs y subst) subst testval)))
      (<
       (and (neq '= testval) (rpo-compare-compounds< (flatargs x subst) y subst testval)))
      (?
       (and (neq '= testval) (rpo-compare-compounds? x y (flatargs x subst) (flatargs y subst) subst testval))))))

(defun rpo-compare-lists (x y xargs yargs subst testval)
  (let (xarg yarg)
    (loop
      (cond
       ((null xargs)
        (return (if (null yargs) '= '<)))
       ((null yargs)
        (return '>))
       ((eql (setf xarg (pop xargs)) (setf yarg (pop yargs)))
        )
       (t
        (ecase (rpo-compare-terms xarg yarg subst nil)
          (>
           (return (and (neq '= testval) (rpo-compare-compounds> x yargs subst testval))))
          (<
           (return (and (neq '= testval) (rpo-compare-compounds< xargs y subst testval))))
          (?
           (return (and (neq '= testval) (rpo-compare-compounds? x y xargs yargs subst testval))))
          (=
           )))))))

(defun rpo-compare-compounds> (x yargs subst testval)
  (if (or (null yargs) (function-boolean-valued-p (head x)))
      '>
      (let ((can-be-> t))
        (dolist (yarg yargs (if can-be-> '> '?))
          (ecase (rpo-compare-terms x yarg subst nil)
            (?
             (if (eq '> testval) (return nil) (setf can-be-> nil)))
            ((< =)
             (return '<))
            (>
             ))))))

(defun rpo-compare-compounds< (xargs y subst testval)
  (if (or (null xargs) (function-boolean-valued-p (head y)))
      '<
      (let ((can-be-< t))
        (dolist (xarg xargs (if can-be-< '< '?))
          (ecase (rpo-compare-terms xarg y subst nil)
            (?
             (if (eq '< testval) (return nil) (setf can-be-< nil)))
            ((> =)
             (return '>))
            (<
             ))))))

(defun rpo-compare-compounds? (x y xargs yargs subst testval)
  (cond
   ((and (or (null testval) (eq '> testval)) (thereis-rpo-equal-or-greaterp xargs y subst))
    '>)
   ((and (or (null testval) (eq '< testval)) (thereis-rpo-equal-or-greaterp yargs x subst))
    '<)
   ((null testval)
    '?)))

(defun thereis-rpo-equal-or-greaterp (args term subst)
  (and (not (function-boolean-valued-p (head term)))
       (dolist (arg args nil)
         (dereference
          arg subst
          :if-constant (when (eq '< (rpo-compare-compound*constant term arg subst '<))
                         (return t))
          :if-compound (case (rpo-compare-compounds arg term subst '>)
                         ((> =)		;= should be returned if they're equal even if testval is >
                          (return t)))))))

(defun rpo-compare-alists (alist1 alist2 subst testval)
  ;; this should be specialized for better performance
  (labels
    ((rpo-alist-args (alist)
       (dereference
        alist subst
        :if-variable (list alist)
        :if-constant nil
        :if-compound (lcons (first alist)
                            (rpo-alist-args (rest alist))
                            alist))))
    (compare-term-multisets #'rpo-compare-terms (rpo-alist-args alist1) (rpo-alist-args alist2) subst testval)))

;;; recursive-path-ordering.lisp EOF
