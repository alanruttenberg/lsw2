;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: alists.lisp
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

;; alists are assumed to be well formed:
;; lists of dotted pairs ending with nil
;; car of each dotted pair is a distinct constant

(defun equal-alist-p (alist1 alist2 subst)
  (and
   (do ((p1 alist1 (rest p1))
        (p2 alist2 (rest p2)))
       (nil)
     (dereference
      p1 subst
      :if-variable (return (dereference p2 subst :if-variable (eq p1 p2)))	;allow variable at end
      :if-constant (return (dereference p2 subst :if-constant t))		;assume p1=p2=nil
      :if-compound-cons (unless (dereference p2 subst :if-compound-cons t)
                          (return nil))))
   (do ((p1 alist1 (rest p1)))
       (nil)
     (dereference
      p1 subst
      :if-variable (return t)
      :if-constant (return t)
      :if-compound-cons (unless (do ((p2 alist2 (rest p2)))
                                    (nil)
                                  (dereference
                                   p2 subst
                                   :if-variable (return nil)
                                   :if-constant (return nil)
                                   :if-compound-cons (when (eql (car (first p1)) (car (first p2)))
                                                       (return (equal-p (cdr (first p1)) (cdr (first p2)) subst)))))
                          (return nil))))))

(defun conjoin-alists (alist1 alist2)
  (let ((result nil) result-last)
    (dolist (x alist1)
      (let ((x1 (car x)))
        (dolist (y alist2 (collect x result))
          (when (eql x1 (car y))
            (collect (cons x1 (conjoin (cdr x) (cdr y))) result)
            (return)))))
    (dolist (y alist2)
      (let ((y1 (car y)))
        (dolist (x alist1 (collect y result))
          (when (eql y1 (car x))
            (return)))))
    result))

(defun conjoin-alist1 (key value alist)
  (labels
    ((conjoin-alist1 (alist)
       (cond
        ((null alist)
         (values nil nil))
        (t
         (let ((p (first alist)))
           (cond
            ((eql key (car p))
             (let ((p* (lcons (car p) (conjoin value (cdr p)) p)))
               (values (if (eq p p*) alist (cons p* (rest alist))) t)))
            (t
             (let ((v (rest alist)))
               (multiple-value-bind (v* found) (conjoin-alist1 v)
                 (values (if (eq v v*) alist (cons p v*)) found))))))))))
    (multiple-value-bind (alist* found) (conjoin-alist1 alist)
      (if found alist* (cons (cons key value) alist*)))))

(defun disjoin-alists (alist1 alist2)
  (let ((result nil) result-last)
    (dolist (x alist1)
      (let ((x1 (car x)))
        (dolist (y alist2 (collect x result))
          (when (eql x1 (car y))
            (collect (cons x1 (disjoin (cdr x) (cdr y))) result)
            (return)))))
    (dolist (y alist2)
      (let ((y1 (car y)))
        (dolist (x alist1 (collect y result))
          (when (eql y1 (car x))
            (return)))))
    result))

(defun disjoin-alist1 (key value alist)
  (labels
    ((disjoin-alist1 (alist)
       (cond
        ((null alist)
         (values nil nil))
        (t
         (let ((p (first alist)))
           (cond
            ((eql key (car p))
             (let ((p* (lcons (car p) (disjoin value (cdr p)) p)))
               (values (if (eq p p*) alist (cons p* (rest alist))) t)))
            (t
             (let ((v (rest alist)))
               (multiple-value-bind (v* found) (disjoin-alist1 v)
                 (values (if (eq v v*) alist (cons p v*)) found))))))))))
    (multiple-value-bind (alist* found) (disjoin-alist1 alist)
      (if found alist* (cons (cons key value) alist*)))))

;;; alists.lisp EOF
