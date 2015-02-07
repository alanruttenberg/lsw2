;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: posets.lisp
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
;;; Portions created by the Initial Developer are Copyright (C) 1981-2005.
;;; All Rights Reserved.
;;;
;;; Contributor(s): Mark E. Stickel <stickel@ai.sri.com>.

(in-package :snark)

;;; notes:
;;; integers are used as elements so that sparse-arrays can be used

(defun make-poset (&rest args)
  (declare (ignore args))
  (make-sparse-matrix :boolean t))

(definline poset-greaterp (poset x y)
  (and (not (eql x y))
       (sparef poset x y)))

(definline poset-lessp (poset x y)
  (and (not (eql x y))
       (sparef poset y x)))

(defun poset-equivalent (poset x y)
  (declare (ignorable poset))
  (or (eql x y)
      (unimplemented)))

(defun declare-poset-greaterp (poset x y)
  (add-edge-transitively poset x y))

(defun declare-poset-lessp (poset x y)
  (add-edge-transitively poset y x))

(defun poset-superiors (poset element)
  (setf (sparse-matrix-column poset element) t))

(defun poset-inferiors (poset element)
  (setf (sparse-matrix-row poset element) t))

(defun add-edge-transitively (graph vertex1 vertex2)
  (let ((l1 (list vertex1))
        (l2 (list vertex2)))
    (let ((col (sparse-matrix-column graph vertex1)))
      (when col (map-sparse-vector (lambda (vertex) (push vertex l1)) col)))
    (let ((row (sparse-matrix-row graph vertex2)))
      (when row (map-sparse-vector (lambda (vertex) (push vertex l2)) row)))
    (dolist (v1 l1)
      (dolist (v2 l2)
        (cond
         ((eql v1 v2)
          (error "Trying to define node ~A > node ~A in ordering relation." v1 v2))
         (t
          (setf (sparef graph v1 v2) t)))))))

;;; posets.lisp EOF
