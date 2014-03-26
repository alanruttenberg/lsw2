;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: row-contexts.lisp
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
;;; Portions created by the Initial Developer are Copyright (C) 1981-2008.
;;; All Rights Reserved.
;;;
;;; Contributor(s): Mark E. Stickel <stickel@ai.sri.com>.

(in-package :snark)

;;; assertions ordinarily go into root context
;;; assumptions and negated conjectures go into (current-row-context)
;;; inferred rows go into the maximum of the contexts of the rows they're inferred from

;;; add assert-context-type snark option?
;;; add assume-context-type snark option?

(defvar *root-row-context*)
(defvar *current-row-context*)

(defmacro root-row-context ()
  `*root-row-context*)

(defmacro current-row-context ()
  `*current-row-context*)

(defun initialize-row-contexts ()
  (setf (root-row-context) (make-feature :name '#:root-row-context :children-incompatible t))
  (setf (current-row-context) (make-feature :parent (root-row-context) :children-incompatible t))
  nil)

(definline context-parent (c)
  (feature-parent c))

(definline context-live? (c)
  (feature-live? c))

(defun print-row-context-tree ()
  (print-feature-tree :node (root-row-context)))

(defun the-row-context (context &optional action)
  (cond
   ((or (eq :root context) (eql 1 context))
    (root-row-context))
   ((eq :current context)
    (current-row-context))
   (t
    (the-feature context action))))	;should verify that it's really a row-context, not just a feature

(defun make-row-context (&key name parent (children-incompatible t))
  (make-feature :name name
                :children-incompatible children-incompatible
                :parent (if parent (the-row-context parent 'error) (current-row-context))))

(defun delete-row-context (context)
  (when (setf context (the-row-context context 'warn))
    (cond
     ((eq (root-row-context) context)
      (warn "Cannot delete root row context ~A." context))
     (t
      (when (eq (current-row-context) context)
        (let ((parent (context-parent context)))
          (setf (current-row-context) parent)
          (warn "Deleting current row context; now in parent row context ~A." parent)))
      (delete-feature context)
      (delete-rows :test (lambda (row) (not (row-context-live? row))))
      t))))

(defun in-row-context (context)
  (setf context (the-row-context context 'error))
  (setf (current-row-context) context))

(defun push-row-context (&key name (children-incompatible t))
  (setf (current-row-context) (make-row-context :name name :children-incompatible children-incompatible)))

(defun pop-row-context ()
  (let* ((context (current-row-context))
         (parent (context-parent context)))
    (cond
     ((null parent)
      (warn "Cannot delete root row context ~A." context))
     (t
      (setf (current-row-context) parent)
      (delete-row-context context)
      parent))))

(defun new-row-context (&key name (children-incompatible t))
  (pop-row-context)
  (push-row-context :name name :children-incompatible children-incompatible))

;;; when partitions are used
;;; row-context is represented as list of elements of the form
;;; (partition-id . row-context)

(defun the-row-context2 (context partitions)
  ;; (use-partitions?) is either nil (partitions are not being used)
  ;; or a list of partition ids
  (setf context (the-row-context context 'error))
  (let ((all-partitions (use-partitions?)))
    (cond
      (all-partitions
       (mapcar (lambda (part)
                 (if (member part all-partitions)
                     (cons part context)
                     (error "~A is not a partition." part)))
               (mklist partitions)))
      (t
       context))))

(defun row-context-live? (row)
  (let ((context (row-context row)))
    (cond
     ((use-partitions?)
      (mapcan (lambda (pcd)
	        (let* ((part (car pcd))
                       (cd (cdr pcd))
		       (cd* (context-live? cd)))
		  (when cd*
		    (list (if (eq cd cd*) pcd (cons part cd*))))))
	      context))
     (t
      (context-live? context)))))

(defun context-intersection-p (x y)
  (cond
    ((use-partitions?)
     (mapcan (lambda (pcd)
	       (let* ((part (car pcd))
		      (cd (cdr pcd))
		      (cd* (feature-union (cdr (assoc part x)) cd)))
		 (when cd*
		   (list (if (eq cd cd*) pcd (cons part cd*))))))
	     y))
    (t
     (feature-union x y))))

(defun context-subsumes? (x y)
  (cond
   ((use-partitions?)
    (let ((w (mapcan (lambda (pcd)
                       (let* ((part (car pcd))
                              (cd (cdr pcd))
                              (v (cdr (assoc part x))))
                         (cond
                          ((null v)
                           (list pcd))
                          (t
                           (let ((cd* (feature-subsumes? v cd)))
                             (cond
                              ((null cd*)
                               (list pcd))
                              ((eq t cd*)
                               nil)
                              (t
                               (list (cons part cd*)))))))))
                     y)))
      (cond
       ((null w)					;x always includes y
        t)
       ((equal x w)					;x never includes y
        nil)
       (t						;x partly includes y
        w)))) 
   (t
    (feature-subsumes? x y))))

;;; *rewriting-row-context* is rebound around the code for rewriting to
;;; restrict what rewrites are available and thus prevent application of
;;; a rewrite to a row in a lower context

(defvar *rewriting-row-context* nil)

;;; row-contexts.lisp EOF
