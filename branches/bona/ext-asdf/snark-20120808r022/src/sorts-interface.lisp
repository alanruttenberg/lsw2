;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: sorts-interface.lisp
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

;;; this file implements SNARK's sort system based on snark-features
;;; interfacing to a different sort system in SNARK should be possible by replacing this file

(defvar *top-sort*)

(definline top-sort-name ()
  'top-sort)

(defun top-sort-name? (x)
  (or (eq 'top-sort x) (eq :top-sort x) (eq t x) (eq 'true x) (eq true x)))

(defun initialize-sort-theory ()
  (setf *top-sort* (declare-feature (top-sort-name)))
  nil)

(defun print-sort-theory ()
  (print-feature-tree :node (top-sort)))

(definline top-sort ()
  *top-sort*)

(definline same-sort? (x y)
  (eq x y))

(definline top-sort? (x)
  (same-sort? (top-sort) x))

(defun subsort0 (x y)
  (with-clock-on sortal-reasoning
    (feature-subsumes? y x)))

(definline subsort? (x y)
  ;; returns true for both identical sorts and strict subsorts
  (or (same-sort? x y)
      (top-sort? y)
      (if (top-sort? x) nil (subsort0 x y))))

(definline subsort1? (x y)
;;(cl:assert (not (top-sort? y)))
  (or (same-sort? x y)
      (if (top-sort? x) nil (subsort0 x y))))

(defun sort-intersection0 (x y)
  ;; returns canonical intersection of x and y, nil if x and y are incompatible
  (with-clock-on sortal-reasoning
    (feature-union x y)))

(definline sort-intersection (x y)
  (cond
   ((or (same-sort? x y) (top-sort? x))
    y)
   ((top-sort? y)
    x)
   (t
    (sort-intersection0 x y))))

(definline sort-disjoint? (x y)
  (null (sort-intersection x y)))

(defun sort? (x)
  (and (or (feature? x) (snark-feature::feature-combo? x))
       (feature-subsumes? (top-sort) x)))

(defun sort-name (sort)
  (let ((sort-name (snark-feature::feature-sym sort)))
    (cl:assert (not (null sort-name)) () "There is no sort named ~S." sort)
    sort-name))

(defun sort-name? (x &optional action)
  ;; returns actual sort if x is a sort-name, nil otherwise
  (or (and (top-sort-name? x) (top-sort))
      (let ((v (find-symbol-table-entry x :sort)))
        (and (neq none v) v))
      (and action (funcall action "There is no sort named ~S." x))))

(defun sort-name-expression? (x &optional action)
  ;; allows conjunction of sort names too
  (cond
   ((atom x)
    (sort-name? x action))
   ((eq 'and (first x))
    (every #'(lambda (x) (sort-name-expression? x action)) (rest x)))
   (t
    (and action (funcall action "~S is not a sort expression." x)))))

(defun fix-sort-name-expression (x)
  (cond
   ((atom x)
    (sort-name? x 'error))
   ((eq 'and (first x))
    (cons 'and (mapcar #'fix-sort-name-expression (rest x))))))

(defun the-sort (sort-expr &optional (action 'error))
  (or (sort-name? sort-expr)
      (let ((x (the-feature (fix-sort-name-expression sort-expr) nil 'error)))
        (and x (feature-subsumes? (top-sort) x) x))	;make sure the feature is specifically a sort
      (and action (funcall action "~S has not been declared as a sort." sort-expr))))

;;; user operations for defining a sort theory:
;;;   declare-sort
;;;   declare-subsort
;;;   declare-sorts-incompatible
;;;
;;; sorts can be declared only once
;;; sorts must be declared before they are used
;;; sort incompatibilities must be declared before incompatible sorts are used

(defun declare-sort1 (sort-name sort)
  (can-be-sort-name sort-name 'error)
  (find-or-create-symbol-table-entry sort-name :sort nil sort)
  (let ((sort-name* (intern (symbol-name sort-name) :snark-user)))
    (unless (eq sort-name sort-name*)
      ;; put the sort name into snark-user package so that sort-from-variable-name can find it
      (find-or-create-symbol-table-entry sort-name* :sort nil sort)))
  (when (test-option30?)
    (declare-the-sort-function-symbol sort-name sort))
  sort)

(defun declare-sort (sort-name &key iff subsorts-incompatible alias)
  (cl:assert (not (and iff subsorts-incompatible)))
  (let ((sort (sort-name? sort-name)))
    (cond
     (sort
      (when (or iff subsorts-incompatible (null alias))
        (warn "Ignoring sort declaration; ~S has already been declared." sort-name)))
     (t
      (setf sort (declare-sort1
                  sort-name
                  (cond
                   (iff
                    (with-clock-on sortal-reasoning
                      (declare-feature sort-name :iff (the-sort iff))))
                   (t
                    (with-clock-on sortal-reasoning 
                      (declare-feature sort-name :parent (the-sort (declare-root-sort?)) :children-incompatible subsorts-incompatible))))))))
    (when alias
      (create-aliases-for-symbol sort alias))
    sort))

(defun declare-subsort (sort-name supersort-expr &key subsorts-incompatible alias)
  (let ((sort (sort-name? sort-name)))
    (cond
     (sort
      (when (or subsorts-incompatible (null alias))
        (warn "Ignoring sort declaration; ~S has already been declared." sort-name)))
     (t
      (setf sort (declare-sort1
                  sort-name
                  (with-clock-on sortal-reasoning
                    (declare-feature sort-name :implies (the-sort supersort-expr) :children-incompatible subsorts-incompatible))))))
    (when alias
      (create-aliases-for-symbol sort alias))
    sort))

(defun declare-sorts-incompatible (sort-name1 sort-name2 &rest more-sort-names)
  (with-clock-on sortal-reasoning
    (apply 'declare-features-incompatible sort-name1 sort-name2 more-sort-names)))

;;; sorts-interface.lisp EOF
