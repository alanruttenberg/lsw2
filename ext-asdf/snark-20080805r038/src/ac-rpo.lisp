;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: ac-rpo.lisp
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

;;; recursive-path-ordering extensions for Rubio's "A fully syntactic AC-RPO"

(defun ac-rpo-compare-compounds (fn xargs yargs subst)
  (or (ac-rpo-cache-lookup fn xargs yargs)
      (ac-rpo-cache-store fn xargs yargs (ac-rpo-compare-compounds* fn xargs yargs subst))))

(defun ac-rpo-compare-compounds* (fn xargs yargs subst)
  (let ((com1 nil) (com2 nil) (com3 nil) (com4 nil)
        (always-> t) (always-< t) 
        big-head-of-x no-small-head-of-x
        big-head-of-y no-small-head-of-y)
    (when (and (eq '= (setf com1 (compare-argument-counts xargs yargs subst)))
               (eq '= (rpo-compare-multisets xargs yargs subst '=)))
      (return-from ac-rpo-compare-compounds* '=))
    (dolist (yargs1 (emb-no-big fn yargs subst))
      (case (ac-rpo-compare-compounds fn xargs yargs1 subst)
        (?
         (setf always-> nil))
        ((< =)
         (return-from ac-rpo-compare-compounds* '<))))
    (when always->
      (multiple-value-setq (big-head-of-x no-small-head-of-x)
        (big-head-and-no-small-head fn xargs subst))
      (multiple-value-setq (big-head-of-y no-small-head-of-y)
        (big-head-and-no-small-head fn yargs subst))
      (when (and (case (setf com4 (compare-no-small-heads
                                   fn no-small-head-of-x no-small-head-of-y subst nil))
                   ((> =)
                    t))
                 (or (eq '> com1)
                     (eq '> (setf com2 (rpo-compare-multisets big-head-of-x big-head-of-y subst nil)))
                     (case com1
                       ((>= =)
                        (cond
                         ((and (eq big-head-of-y yargs) (eq '> com2))
                          t)
                         ((and (eq big-head-of-x xargs) (neq '> com2))
                          nil)
                         ((and (eq big-head-of-x xargs) (eq big-head-of-y yargs))
                          (eq '> com2))
                         (t
                          (eq '> (setf com3 (rpo-compare-multisets xargs yargs subst nil)))))))))
        (return-from ac-rpo-compare-compounds* '>)))
    (dolist (xargs1 (emb-no-big fn xargs subst))
      (case (ac-rpo-compare-compounds fn xargs1 yargs subst)
        (?
         (setf always-< nil))
        ((> =)
         (return-from ac-rpo-compare-compounds* '>))))
    (when always-<
      (unless always->
        (multiple-value-setq (big-head-of-x no-small-head-of-x)
          (big-head-and-no-small-head fn xargs subst))
        (multiple-value-setq (big-head-of-y no-small-head-of-y)
          (big-head-and-no-small-head fn yargs subst)))
      (when (and (case (or com4 (compare-no-small-heads
                                 fn no-small-head-of-x no-small-head-of-y subst nil))
                   ((< =)
                    t))
                 (or (eq '< com1)
                     (eq '< (or com2 (setf com2 (rpo-compare-multisets big-head-of-x big-head-of-y subst nil))))
                     (case com1
                       ((<= =)
                        (cond
                         ((and (eq big-head-of-x xargs) (eq '< com2))
                          t)
                         ((and (eq big-head-of-y yargs) (neq '< com2))
                          nil)
                         ((and (eq big-head-of-x xargs) (eq big-head-of-y yargs))
                          (eq '< com2))
                         (t
                          (eq '< (or com3 (rpo-compare-multisets xargs yargs subst '<)))))))))
        (return-from ac-rpo-compare-compounds* '<)))
    '?))

(defun emb-no-big (fn args subst)
  ;; defn 12
  (let ((revargs nil) (result nil) result-last)
    (dotails (args args)
      (let ((argi (first args)))
        (when (dereference argi subst :if-compound (neq '> (symbol-ordering-compare (head argi) fn)))
          (dolist (argij (args argi))
            (collect (revappend
                      revargs
                      (dereference
                       argij subst
                       :if-variable (cons argij (rest args))
                       :if-constant (cons argij (rest args))
                       :if-compound (if (eq fn (head argij))
                                        (append (flatargs argij subst) (rest args))
                                        (cons argij (rest args)))))
              result)))
        (push argi revargs)))
    result))

(defun big-head-and-no-small-head (fn args subst)
  ;; defn 2: big-head is multiset of arguments for which (> (top arg) fn)
  ;; defn 7: no-small-head is multiset of arguments for which (not (< (top arg) fn))
  (labels
    ((big-head-and-no-small-head* (args)
       (if (null args)
           (values nil nil)
           (let* ((l (rest args))
                  (arg (first args))
                  (com (dereference
                        arg subst
                        :if-variable '?
                        :if-constant (symbol-ordering-compare arg fn)
                        :if-compound (symbol-ordering-compare (head arg) fn))))
             (mvlet (((:values big-head no-small-head) (big-head-and-no-small-head* l)))
               (values (if (eq '> com)
                           (if (eq big-head l) args (cons arg big-head))
                           big-head)
                       (if (neq '< com)
                           (if (eq no-small-head l) args (cons arg no-small-head))
                           no-small-head)))))))
    (big-head-and-no-small-head* args)))

(defun compare-no-small-heads (fn no-small-head-of-x no-small-head-of-y subst testval)
  ;; defn 11 comparison function adds the following
  ;; conditions to the usual comparison
  ;;  (> compound compound') : (or (> (head compound) fn) (>= (head compound) (head compound'))
  ;;  (> constant compound)  : (or (> constant fn) (> constant (head compound)))
  ;;  (> compound constant)  : (or (> (head compound) fn) (> (head compound) constant))
  ;;  (> compound variable)  : (> (head compound) fn)
  (symbol-ordering-compare-term-multisets
   no-small-head-of-x no-small-head-of-y subst testval
   (lambda (x y subst testval)
     (ecase testval
       (=
        (rpo-compare-compounds x y subst testval))
       (>
        (and (or (eq '> (symbol-ordering-compare (head x) fn))
                 (case (symbol-ordering-compare (head x) (head y))
                   ((> =)
                    t)))
             (rpo-compare-compounds x y subst testval)))
       (<
        (and (or (eq '> (symbol-ordering-compare (head y) fn))
                 (case (symbol-ordering-compare (head y) (head x))
                   ((> =)
                    t)))
             (rpo-compare-compounds x y subst testval)))
       ((nil)
        (ecase (rpo-compare-compounds x y subst testval)
          (>
           (if (or (eq '> (symbol-ordering-compare (head x) fn))
                   (case (symbol-ordering-compare (head x) (head y))
                     ((> =)
                      t)))
               '>
               '?))
          (<
           (if (or (eq '> (symbol-ordering-compare (head y) fn))
                   (case (symbol-ordering-compare (head y) (head x))
                     ((> =)
                      t)))
               '<
               '?))
          (?
           '?)))))
   (lambda (compound constant subst testval)
     (ecase testval
       (>
        (and (or (eq '> (symbol-ordering-compare (head compound) fn))
                 (eq '> (symbol-ordering-compare (head compound) constant)))
             (symbol-ordering-compare-compound*constant compound constant subst testval)))
       (<
        (and (or (eq '> (symbol-ordering-compare constant fn))
                 (eq '> (symbol-ordering-compare constant (head compound))))
             (symbol-ordering-compare-compound*constant compound constant subst testval)))
       ((nil)
        (ecase (symbol-ordering-compare-compound*constant compound constant subst testval)
          (>
           (if (or (eq '> (symbol-ordering-compare (head compound) fn))
                   (eq '> (symbol-ordering-compare (head compound) constant)))
               '>
               '?))
          (<
           (if (or (eq '> (symbol-ordering-compare constant fn))
                   (eq '> (symbol-ordering-compare constant (head compound))))
               '<
               '?))
          (?
           '?)))))
   (lambda (compound variable subst)
     (if (eq '> (symbol-ordering-compare (head compound) fn))
         (symbol-ordering-compare-compound*variable compound variable subst)
         '?))))

(defun compare-argument-counts (xargs yargs subst)
  ;; xargs.subst and yargs.subst are already flattened argument lists
  ;; of the same associative function
  ;; this is the AC-RPO comparison of #(x) and #(y) that returns
  ;; =, >, <, >=, =<, or ?
  (let ((variable-counts nil) (variable-count 0) (nonvariable-count 0))
    (labels
      ((count-arguments (args inc)
         (declare (fixnum inc))
         (let (v)
           (dolist (term args)
             (dereference
              term subst
              :if-variable (cond
                            ((null variable-counts)
                             (setf variable-counts (cons (make-tc term inc) nil)))
                            ((setf v (assoc/eq term variable-counts))
                             (incf (tc-count v) inc))
                            (t
                             (push (make-tc term inc) variable-counts)))
              :if-constant (incf nonvariable-count inc)
              :if-compound (incf nonvariable-count inc))))))
      (count-arguments xargs 1)
      (count-arguments yargs -1)
      (dolist (v variable-counts)
        (let ((c (tc-count v)))
          (cond
           ((plusp c)
            (if (minusp variable-count)
                (return-from compare-argument-counts '?)
                (incf variable-count c)))
           ((minusp c)
            (if (plusp variable-count)
                (return-from compare-argument-counts '?)
                (incf variable-count c))))))
      (cond
       ((plusp variable-count)
        (cond
         ((minusp nonvariable-count)
          (let ((d (+ variable-count nonvariable-count)))
            (cond
             ((eql 0 d)
              '>=)
             ((plusp d)
              '>)
             (t
              '?))))
         (t
          '>)))
       ((minusp variable-count)
        (cond
         ((plusp nonvariable-count)
          (let ((d (+ variable-count nonvariable-count)))
            (cond
             ((eql 0 d)
              '=<)
             ((minusp d)
              '<)
             (t
              '?))))
         (t
          '<)))
       ((eql 0 nonvariable-count)
        '=)
       (t
        (if (plusp nonvariable-count) '> '<))))))

(defun ac-rpo-cache-lookup (fn xargs yargs)
  (dolist (x *ac-rpo-cache* nil)
    (when (and (eq fn (first x))
               (eql-list xargs (first (setf x (rest x))))
               (eql-list yargs (first (setf x (rest x)))))
      (return (first (rest x))))))

(defun ac-rpo-cache-store (fn xargs yargs com)
  (push (list fn xargs yargs com) *ac-rpo-cache*)
  com)

(defun eql-list (l1 l2)
  (loop
    (cond
     ((null l1)
      (return (null l2)))
     ((null l2)
      (return nil))
     ((neql (pop l1) (pop l2))
      (return nil)))))

;;; ac-rpo.lisp EOF
