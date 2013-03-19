;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: variables.lisp
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

(defconstant $number-of-variable-blocks 1000)
(defconstant $number-of-variables-per-block 6000)
(defconstant $number-of-variables-in-blocks (* $number-of-variable-blocks $number-of-variables-per-block))

(defvar *variables*)				;tables to translate (sort number) pairs to variables
(defvar *next-variable-number* 0)		;next number to use for new unique variable
(declaim (type integer *next-variable-number*))

(defstruct (variable
            (:constructor make-variable0 (sort number))
            (:copier nil)
            (:print-function print-variable))
  number
  sort)

(defun initialize-variables ()
  (setf *variables* (list (make-sparse-vector) (make-hash-table :test #'equal)))
  (setf *next-variable-number* $number-of-variables-in-blocks)
  nil)

(defun make-variable (&optional (sort (top-sort)) number)
  ;; if number is specified, return canonical variable for that sort and number
  ;; if number is not specified, create a new unique variable with that sort
  ;;
  ;; variable identity must be testable by EQ
  ;; this variable representation must also be understood by dereference
  ;;
  ;; don't create last variable in a block; when incrementing variable numbers,
  ;; the following variable would be in the next block creating confusion
  (cond
   (number
    (let ((vars (if (top-sort? sort)
                    (first *variables*)
                    (let ((v (second *variables*)))
                      (or (gethash sort v) (setf (gethash sort v) (make-sparse-vector)))))))
      (or (sparef vars number)
          (progn
            (cl:assert (<= 0 number))
            (cl:assert (< number $number-of-variables-in-blocks))
            (cl:assert (/= 0 (mod (+ number 1) $number-of-variables-per-block)))
            (setf (sparef vars number) (make-variable0 sort number))))))
   (t
    (setf *next-variable-number* (+ (setf number *next-variable-number*) 1))
    (make-variable0 sort number))))


(defun variable-block (n)
  (declare (fixnum n))
  (cl:assert (< 0 n $number-of-variable-blocks))
  (* $number-of-variables-per-block n))

(defun variable-block-0-p (varnum)
  (declare (fixnum varnum))
  (> $number-of-variables-per-block varnum))

;;; variables.lisp EOF
