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
;;; Portions created by the Initial Developer are Copyright (C) 1981-2007.
;;; All Rights Reserved.
;;;
;;; Contributor(s): Mark E. Stickel <stickel@ai.sri.com>.

(in-package :snark)

;;; a function-sort-declaration is a single sort declaration for a function or relation
;;; fsd-result-sort returns the sort of the result (top-sort in the case of relations)
;;; fsd-argument-sort-alist returns an alist of argument-ids and argument-sorts like
;;;   ((2 . arg2-sort) (1 . arg1-sort) (t . default-arg-sort)) and
;;;
;;; the function-sort-declaration-slot slot will contain a list of zero, one, or more fsds
;;;
;;; nil will be interpreted as a function-sort-declaration with result and arguments
;;; all being instances of top-sort

(defstruct (function-sort-declaration
            (:print-function print-fsd3)
            (:copier nil))
  (result-sort (top-sort) :read-only t)
  (argument-sort-alist nil :read-only t))

(definline fsd-result-sort (x)
  (if (null x) (top-sort) (function-sort-declaration-result-sort x)))

(definline fsd-argument-sort-alist (x)
  (if (null x) nil (function-sort-declaration-argument-sort-alist x)))

(definline fsd-subsort-p (fsd sort)
  (or (top-sort? sort) (subsort? (fsd-result-sort fsd) sort)))

(defun fsd-arg-sort (fsd argid)
  ;; find in (fsd-argument-sort-alist fsd) the sort restriction for argument argid
  ;; argid is an argument number or a key in the case of alist/plist functions/relations
  (dolist (p (fsd-argument-sort-alist fsd) (top-sort))
    (let ((key (car p)))
      (when (or (eql argid key) (eq t key))
        (return (cdr p))))))

(defun print-fsd3 (fsd &optional (stream *standard-output*) depth)
  (declare (ignore depth))
  (print-unreadable-object (fsd stream :type t :identity nil)
    (let ((sort (fsd-result-sort fsd)))
      (unless (top-sort? sort)
        (princ ":RESULT " stream)
        (prin1 (sort-name sort) stream)
        (princ " " stream)))
    (let ((arg-sort-alist (fsd-argument-sort-alist fsd)))
      (when arg-sort-alist
        (princ ":ARGS " stream)
        (prin1 (mapcar (lambda (p) (list (car p) (sort-name (cdr p)))) arg-sort-alist) stream)))))

;;; same-fsd-p and sub-fsd-p check whether two function sort declarations
;;; are the same or one is an subcase of the other
;;; limitation: little more than argument-sort-alists with all the same keys
;;; can be recognized as possibly being the same or a subcase

(defun same-fsd-p (fsd1 fsd2)
  (and (same-sort? (fsd-result-sort fsd1) (fsd-result-sort fsd2))
       (same-sort? (fsd-arg-sort fsd1 t) (fsd-arg-sort fsd2 t))
       (every (lambda (p) (or (eq t (car p)) (same-sort? (cdr p) (fsd-arg-sort fsd2 (car p)))))
              (fsd-argument-sort-alist fsd1))
       (every (lambda (p) (or (eq t (car p)) (same-sort? (fsd-arg-sort fsd1 (car p)) (cdr p))))
              (fsd-argument-sort-alist fsd2))))

(defun sub-fsd-p (fsd1 fsd2)
  (and (subsort? (fsd-result-sort fsd1) (fsd-result-sort fsd2))
       (subsort? (fsd-arg-sort fsd1 t) (fsd-arg-sort fsd2 t))
       (every (lambda (p) (or (eq t (car p)) (subsort? (cdr p) (fsd-arg-sort fsd2 (car p)))))
              (fsd-argument-sort-alist fsd1))
       (every (lambda (p) (or (eq t (car p)) (subsort? (fsd-arg-sort fsd1 (car p)) (cdr p))))
              (fsd-argument-sort-alist fsd2))))

(defun super-fsd-p (fsd1 fsd2)
  (sub-fsd-p fsd2 fsd1))

(defun fsd-intersection (fsd1 fsd2)
  (let (result-sort default-sort (arg-sort-alist nil))
    (cond
     ((null fsd1)
      fsd2)
     ((null fsd2)
      fsd1)
     ((null (setf default-sort (sort-intersection (fsd-arg-sort fsd1 t) (fsd-arg-sort fsd2 t))))
      none)
     ((dolist (p (fsd-argument-sort-alist fsd1))
        (let ((key (car p)))
          (unless (eq t key)
            (let ((sort (sort-intersection (cdr p) (fsd-arg-sort fsd2 key))))
              (cond
               ((null sort)
                (return t))
               (t
                (push (cons key sort) arg-sort-alist)))))))
      none)
     ((dolist (p (fsd-argument-sort-alist fsd2))
        (let ((key (car p)))
          (unless (or (eq t key) (assoc key arg-sort-alist))
            (let ((sort (sort-intersection (cdr p) (fsd-arg-sort fsd1 key))))
              (cond
               ((null sort)
                (return t))
               (t
                (push (cons key sort) arg-sort-alist)))))))
      none)
     ;; error to have compatible arguments and incompatible results
     ((null (setf result-sort (sort-intersection (fsd-result-sort fsd1) (fsd-result-sort fsd2))))
      :error)
     (t
      (unless (top-sort? default-sort)
        (push (cons t default-sort) arg-sort-alist))
      (make-function-sort-declaration
       :result-sort result-sort
       :argument-sort-alist (nreverse arg-sort-alist))))))

;;; input-argument-sort-alist inputs argument sort restrictions of the form
;;;   ((2 arg2-sort) (1 arg1-sort) (t default-arg-sort)) and
;;; recognized by can-be-argument-sort-alist-p1
;;;
;;; it also converts old-style declarations of the form
;;;   (arg1-sort arg2-sort)
;;; recognized by can-be-argument-sort-alist-p2 to
;;;   ((1 arg1-sort) (2 arg2-sort))

(defun input-argument-sort-alist (function l)
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
                          (null (rest (rest p)))
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
