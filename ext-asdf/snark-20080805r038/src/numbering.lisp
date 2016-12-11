;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark-numbering -*-
;;; File: numbering.lisp
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

(in-package :snark-numbering)

(defvar *nonce* 0)
(declaim (type integer *nonce*))
(defvar *standard-eql-numbering*)
(defvar *standard-equal-numbering*)

(definline nonce ()
  ;; each call returns a new positive value in ascending order
  (incf *nonce*))

(defun initialize-numberings ()
  (setf *nonce* 0)
  (setf *standard-eql-numbering* (make-numbering :test #'eql))
  (setf *standard-equal-numbering* (make-numbering :test #'equal))
  nil)

(defun make-numbering (&key (test #'eql) (inverse t))
  ;; make-numbering returns a function f such that
  ;; (f :lookup object) returns a unique number for object, adding one if necessary
  ;; (f :lookup? object) returns the number for object or nil if there isn't one
  ;; (f :delete object) deletes an object from the numbering
  ;; (f :inverse number) returns an object by its number
  ;; (f :map fn) applies binary function fn to each object and its number
  (let ((table (make-hash-table :test test)))
    (if inverse
        (let ((invtable (make-sparse-vector :default-value '%absent%)))
          (lambda (action arg)
            (ecase action
              (:lookup
               (or (gethash arg table)
                   (let ((number (nonce)))
                     (setf (sparef invtable number) arg (gethash arg table) number))))
              (:lookup?
               (gethash arg table))
              (:inverse
               (let ((object (sparef invtable arg)))
                 (if (eq '%absent% object) (error "No object numbered ~D." arg) object)))
              (:delete
               (let ((number (gethash arg table)))
                 (when number
                   (setf (sparef invtable number) '%absent%)
                   (remhash arg table)
                   number)))
              (:map
               (map-sparse-vector-with-indexes arg invtable)))))
        (lambda (action arg)
          (ecase action
            (:lookup
             (or (gethash arg table)
                 (let ((number (nonce)))
                   (setf (gethash arg table) number))))
            (:lookup?
             (gethash arg table))
            (:delete
             (let ((number (gethash arg table)))
               (when number
                 (remhash arg table)
                 number))))))))

#+ignore
(eval-when (:load-toplevel :execute)
  (initialize-numberings))

;;; numbering.lisp EOF
