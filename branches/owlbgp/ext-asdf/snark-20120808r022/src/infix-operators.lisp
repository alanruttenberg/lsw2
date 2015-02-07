;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark-infix-reader -*-
;;; File: infix-operators.lisp
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

(in-package :snark-infix-reader)

(defvar *infix-operators* nil)
(defvar *prefix-operators* nil)
(defvar *postfix-operators* nil)

(defparameter infix-types '(:xfx :xfy :yfx :yfy))
(defparameter prefix-types '(:fx :fy))
(defparameter postfix-types '(:xf :yf))

(defstruct (operator
            (:copier nil))
  (input-string nil :read-only t)
  (type nil :read-only t)
  (precedence nil :read-only t)
  (output-symbol nil :read-only t))

(definline infix-operator-p (op)
  (and (operator-p op) (member (operator-type op) infix-types)))

(definline prefix-operator-p (op)
  (and (operator-p op) (member (operator-type op) prefix-types)))

(definline postfix-operator-p (op)
  (and *postfix-operators* (operator-p op) (member (operator-type op) postfix-types)))

(defun initialize-operator-syntax ()
  (setf *infix-operators* nil)
  (setf *prefix-operators* nil)
  (setf *postfix-operators* nil))

(definline operator-lookup0 (input-string list)
  (dolist (op list nil)
    (when (string= input-string (operator-input-string op))
      (return op))))

(definline infix-operator-lookup (input-string)
  (operator-lookup0 input-string *infix-operators*))

(definline prefix-operator-lookup (input-string)
  (operator-lookup0 input-string *prefix-operators*))

(definline postfix-operator-lookup (input-string)
  (operator-lookup0 input-string *postfix-operators*))

(defun update-operator-syntax (input-string op listname)
  (let ((l (remove input-string (symbol-value listname) :key #'operator-input-string :test #'string=)))
    (setf (symbol-value listname) (if op (cons op l) l))))

(defun declare-operator-syntax (input-string type &optional (precedence nil precedence-supplied) (output-symbol input-string))
  ;; (declare-operator-syntax "<=>" :xfy 505) declares <=> as a type xfy operator with precedence 505
  ;; (declare-operator-syntax "<=>" :xfy nil) undeclares <=> as a type xfy operator
  ;; (declare-operator-syntax "<=>" nil) undeclares <=> as any kind of operator
  (if (null type)
      (cl:assert (null precedence))
      (progn
        (cl:assert (or (member type infix-types) (member type prefix-types) (member type postfix-types)))
        (cl:assert precedence-supplied)
        (cl:assert (implies precedence (integerp precedence)))))
  (unless (stringp input-string)
    (setf input-string (string input-string)))
  (unless (implies (and type precedence) (symbolp output-symbol))
    (setf output-symbol (intern (string output-symbol))))
  (let ((op (and type precedence (make-operator :input-string input-string :type type :precedence precedence :output-symbol output-symbol))))
    (cond
     ((member type infix-types)
      (update-operator-syntax input-string op '*infix-operators*))
     ((member type prefix-types)
      (update-operator-syntax input-string op '*prefix-operators*))
     ((member type postfix-types)
      (update-operator-syntax input-string op '*postfix-operators*))
     (t
      (update-operator-syntax input-string op '*infix-operators*)
      (update-operator-syntax input-string op '*prefix-operators*)
      (update-operator-syntax input-string op '*postfix-operators*)))
    op))

(definline reduce-before? (op1 op2)
  (let ((p1 (operator-precedence op1))
        (p2 (operator-precedence op2)))
    (or (< p1 p2)
        (and (eql p1 p2)
             (member (operator-type op2) '(:yfx :yfy :yf))
             (member (operator-type op1) '(:xfx :yfx :fx))))))

;;; infix-operators.lisp EOF
