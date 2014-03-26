;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: sorts-functions.lisp
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

;;; an argument-sort-alist (asa) is an alist of argument-ids and argument-sorts like
;;;   ((2 . arg2-sort) (1 . arg1-sort) (t . default-arg-sort))

(defun asa-arg-sort (asa argid)
  ;; find in asa the sort restriction for argument argid
  ;; argid is an argument number or a key in the case of alist/plist functions/relations
  (dolist (p asa (top-sort))
    (let ((key (car p)))
      (when (or (eql argid key) (eq t key))
        (return (cdr p))))))

(defun input-argument-sort-alist (function l)
  ;; input-argument-sort-alist inputs argument sort restrictions of the form
  ;;   ((2 arg2-sort) (1 arg1-sort) (t default-arg-sort))
  ;; that are recognized by can-be-argument-sort-alist-p1
  ;;
  ;; it also converts old-style declarations of the form
  ;;   (arg1-sort arg2-sort)
  ;; that are recognized by can-be-argument-sort-alist-p2
  (cond
   ((null l)
    nil)
   ((can-be-argument-sort-alist-p1 function l)
    (mapcar (lambda (p) (cons (first p) (the-sort (second p)))) l))
   ((can-be-argument-sort-alist-p2 function l)
    (let ((i 0)) (mapcar (lambda (s) (cons (incf i) (the-sort s))) l)))
   (t
    (with-standard-io-syntax2
      (error "The sort of the argument list of ~A ~S cannot be ~S."	;not very informative
             (function-kind function) (function-name function) l)))))

(defun can-be-argument-sort-alist-p1 (function l)
  (and (consp l)
       (let* ((arity (function-arity function))
              (can-be-key-p (cond
                             ((naturalp arity)
                              (lambda (x) (and (integerp x) (<= 1 x arity))))
                             (t
                              (ecase arity
                                (:any #'naturalp))))))
         (dotails (l l t)
           (let ((p (first l)))
             (unless (and (consp p)
                          (if (eq t (first p))
                              (null (rest l))
                              (funcall can-be-key-p (first p)))
                          (consp (rest p))
                          (null (rrest p))
                          (the-sort (second p)))
               (return nil)))))))

(defun can-be-argument-sort-alist-p2 (function l)
  (and (consp l)
       (let ((arity (function-arity function)))
         (and (or (naturalp arity) (eq :any arity))
              (every (lambda (s)
                       (the-sort s))
                     l)))))

;;; sorts-functions.lisp EOF
