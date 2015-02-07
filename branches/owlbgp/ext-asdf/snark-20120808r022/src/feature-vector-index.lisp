;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: feature-vector-index.lisp
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
;;; Portions created by the Initial Developer are Copyright (C) 1981-2012.
;;; All Rights Reserved.
;;;
;;; Contributor(s): Mark E. Stickel <stickel@ai.sri.com>.

(in-package :snark)

(defvar *feature-vector-row-index*)
(defvar *feature-vector-term-index*)

(defstruct (feature-vector-index
            (:include trie)
            (:constructor make-feature-vector-index0)
            (:copier nil))
  (entry-counter (make-counter) :read-only t)
  (retrieve-generalization-calls 0 :type integer)	;forward subsumption
  (retrieve-generalization-count 0 :type integer)
  (retrieve-instance-calls 0 :type integer)		;backward subsumption
  (retrieve-instance-count 0 :type integer))

(defun make-feature-vector-row-index ()
  (setf *feature-vector-row-index* (make-feature-vector-index0)))

(defun make-feature-vector-term-index ()
  (setf *feature-vector-term-index* (make-feature-vector-index0)))

(defun feature-vector-index-entry-number (entry)
  (cond
   ((row-p entry)
    (row-number entry))
   (t
    (tme-number entry))))

(defun feature-vector-index-entry-keys (entry)
  (cond
   ((row-p entry)
    (clause-feature-vector (row-wff entry)))
   (t
    (atom-or-term-feature-vector (index-entry-term entry)))))

(defun feature-vector-index-insert (entry index)
  (let* ((entry# (feature-vector-index-entry-number entry))
         (keys (feature-vector-index-entry-keys entry))
         (entries (trieref index keys)))
    (cond
     ((null entries)
      (setf (sparef (setf (trieref index keys) (make-sparse-vector)) entry#) entry)
      (increment-counter (feature-vector-index-entry-counter index)))
     (t
      (let ((c (sparse-vector-count entries)))
        (setf (sparef entries entry#) entry)
        (let ((c* (sparse-vector-count entries)))
          (when (< c c*)
            (increment-counter (feature-vector-index-entry-counter index)))))))
    nil))

(defun feature-vector-index-delete (entry index)
  (let* ((entry# (feature-vector-index-entry-number entry))
         (keys (feature-vector-index-entry-keys entry))
         (entries (trieref index keys)))
    (unless (null entries)
      (let ((c (sparse-vector-count entries)))
        (setf (sparef entries entry#) nil)
        (let ((c* (sparse-vector-count entries)))
          (when (> c c*)
            (decrement-counter (feature-vector-index-entry-counter index))
            (when (= 0 c*)
              (setf (trieref index keys) nil))))))
    nil))

(defun map-feature-vector-row-index-forward-subsumption-candidates (function row)
  (prog->
    (identity *feature-vector-row-index* -> index)
    (incf (feature-vector-index-retrieve-generalization-calls index))
    (map-fv-trie<= index (clause-feature-vector (row-wff row)) ->* entries)
    (incf (feature-vector-index-retrieve-generalization-count index) (sparse-vector-count entries))
    (map-sparse-vector function entries)))

(defun map-feature-vector-row-index-backward-subsumption-candidates (function row)
  (prog->
    (identity *feature-vector-row-index* -> index)
    (incf (feature-vector-index-retrieve-instance-calls index))
    (map-fv-trie>= index (clause-feature-vector (row-wff row)) ->* entries)
    (incf (feature-vector-index-retrieve-instance-count index) (sparse-vector-count entries))
    (map-sparse-vector function entries)))

(defun map-feature-vector-term-index-generalizations (function term &optional subst)
  (prog->
    (dereference term subst :if-variable none :if-constant term :if-compound (head term) -> head)
    (identity *feature-vector-term-index* -> index)
    (incf (feature-vector-index-retrieve-generalization-calls index))
    (map-fv-trie<= index (atom-or-term-feature-vector term subst) ->* entries)
    (map-sparse-vector entries ->* entry)
    (index-entry-term entry -> term2)
    (dereference term2 nil :if-variable head :if-constant term2 :if-compound (head term2) -> head2)
    (when (eql head head2)
      (incf (feature-vector-index-retrieve-generalization-count index))
      (funcall function entry))))

(defun map-feature-vector-term-index-instances (function term &optional subst)
  (prog->
    (dereference term subst :if-variable none :if-constant term :if-compound (head term) -> head)
    (identity *feature-vector-term-index* -> index)
    (incf (feature-vector-index-retrieve-instance-calls index))
    (map-fv-trie>= index (atom-or-term-feature-vector term subst) ->* entries)
    (map-sparse-vector entries ->* entry)
    (index-entry-term entry -> term2)
    (dereference term2 nil :if-variable none :if-constant term2 :if-compound (head term2) -> head2)
    (when (or (eq none head) (eql head head2))
      (incf (feature-vector-index-retrieve-instance-count index))
      (funcall function entry))))

(defun print-feature-vector-index1 (index format1 format2 format3 format4)
  (let ((entries-count 0))
    (prog->
      (map-trie index ->* entries)
      (setf entries-count (+ entries-count (sparse-vector-count entries))))
    (mvlet (((:values current peak added deleted) (counter-values (feature-vector-index-entry-counter index))))
      (format t format1 current peak added deleted))
    (mvlet (((:values current peak added deleted) (counter-values (feature-vector-index-node-counter index))))
      (format t format2 current peak added deleted))
    (unless (eql 0 (feature-vector-index-retrieve-generalization-calls index))
      (format t format3 (feature-vector-index-retrieve-generalization-count index) (feature-vector-index-retrieve-generalization-calls index)))
    (unless (eql 0 (feature-vector-index-retrieve-instance-calls index))
      (format t format4 (feature-vector-index-retrieve-instance-count index) (feature-vector-index-retrieve-instance-calls index)))))
  
(defun print-feature-vector-row-index ()
  (print-feature-vector-index1
   *feature-vector-row-index*
   "~%; Feature-vector-row-index has ~:D entr~:@P (~:D at peak, ~:D added, ~:D deleted)."
   "~%; Feature-vector-row-index has ~:D node~:P (~:D at peak, ~:D added, ~:D deleted)."
   "~%;  Retrieved ~:D possibly forward subsuming row~:P in ~:D call~:P."
   "~%;  Retrieved ~:D possibly backward subsumed row~:P in ~:D call~:P."))

(defun print-feature-vector-term-index ()
  (print-feature-vector-index1
   *feature-vector-term-index*
   "~%; Feature-vector-term-index has ~:D entr~:@P (~:D at peak, ~:D added, ~:D deleted)."
   "~%; Feature-vector-term-index has ~:D node~:P (~:D at peak, ~:D added, ~:D deleted)."
   "~%;  Retrieved ~:D possibly generalization term~:P in ~:D call~:P."
   "~%;  Retrieved ~:D possibly instance term~:P in ~:D call~:P."))

;;; feature-vector-index.lisp EOF
