;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: equal.lisp
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

;;; EQ suffices to compare function, relation, and variable symbols
;;; EQL suffices to compare constant symbols
;;;  string constants must be term-hashed to be EQ

(defun equal-p (x y &optional subst)
  (or (eql x y)
      (dereference
       x subst
       :if-variable (dereference y subst :if-variable (eq x y))
       :if-constant (dereference y subst :if-constant (eql x y))
       :if-compound-cons (dereference
                          y subst
                          :if-compound-cons (and (equal-p (carc x) (carc y) subst)
                                                 (equal-p (cdrc x) (cdrc y) subst)))
       :if-compound-appl (dereference
                          y subst
                          :if-compound-appl
                          (or (eq x y)
                              (let ((head (heada x)))
                                (cond
                                 ((neq head (heada y))
                                  nil)
                                 (t
                                  (dolist (fun (function-equal-code head) (equal-p (argsa x) (argsa y) subst))
                                    (let ((v (funcall fun x y subst)))
                                      (unless (eq none v)
                                        (return v))))))))))))

(defun ac-equal-p (x y subst)
  (let ((fn (head x))
        (terms1 (args x))
        (terms2 (args y)))
    (and (similar-argument-list-ac1-p fn terms1 terms2 subst)
         (progn
           (setf terms2 (cons nil (copy-list (argument-list-a1 fn terms2 subst))))
           (loop for term1 in (argument-list-a1 fn terms1 subst)
                 always (loop for y1 = terms2 then y2
                              for y2 on (cdr terms2)
                              thereis (if (equal-p term1 (car y2) subst)
                                          (rplacd y1 (cdr y2))	;non-nil
                                          nil)))))))

(defun commutative-equal-p (x y subst)
  (mvlet (((list* x y z) (args x))
          ((list* u v w) (args y)))
    (and (or (eq z w) (equal-p z w subst))
         (cond
          ((equal-p x u subst)
           (equal-p y v subst))
          ((equal-p x v subst)
           (equal-p y u subst))
          (t
           nil)))))

(defun associative-equal-p (x y subst)
  (let ((fn (head x))
        (terms1 (args x))
        (terms2 (args y)))
    (and (eql (argument-count-a1 fn terms1 subst)
              (argument-count-a1 fn terms2 subst))
         (let (x y)
           (loop
             (cond
              ((null terms1)
               (return (null terms2)))
              ((null terms2)
               (return nil))
              (t
               (setf (values x terms1) (first-and-rest-of-vector terms1 subst fn none))
               (setf (values y terms2) (first-and-rest-of-vector terms2 subst fn none))
               (unless (equal-p x y subst)
                 (return nil)))))))))

(defun member-p (item list &optional subst)
  (or (member item list)
      (dotails (l list nil)
        (when (equal-p item (first l) subst)
          (return l)))))

(defun assoc-p (item alist &optional subst)
  (or (assoc item alist)
      (dolist (pair alist nil)
        (when (equal-p item (car pair) subst)
          (return pair)))))

(defun literal-member-p (atom polarity list)
  (or (dolist (x list nil)
        (when (and (eq atom (first x)) (eq polarity (second x)))
          (return x)))
      (dolist (x list nil)
        (when (and (eq polarity (second x)) (equal-p atom (first x)))
          (return x)))))

;;; equal.lisp EOF
