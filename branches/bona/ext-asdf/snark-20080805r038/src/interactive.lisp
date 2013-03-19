;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: interactive.lisp
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

(in-package :snark)

;;; notes:
;;; clean up *interactive? *row2* interface to resolve.lisp code
;;; (paramodulate wff) isn't implemented
;;; add rewrite operation

(declaim
  (special
    *negative-hyperresolution*
    *interactive?
    *row2*))

(defvar it nil)				;set to row name or number of last result

(defvar *last-row-number-before-interactive-operation*)

(defun row-to-designator-string (row)		;needed by Amphion?
  (prin1-to-string (row-name-or-number row)))

(defun before-interactive-operation ()
  (let-options ((use-closure-when-satisfiable t)
		(print-rows-when-derived nil)
		(print-summary-when-finished nil))
    (closure :only-unnumbered-rows t))
  (setf *last-row-number-before-interactive-operation*
        (let ((last-row (last-row)))
          (if last-row
              (row-number last-row)
              0))))

(defun after-interactive-operation (op)
  (let-options ((use-closure-when-satisfiable t)
		(print-rows-when-derived t)
		(print-summary-when-finished nil))
    (closure :only-unnumbered-rows t))
  (prog->
    (map-rows :reverse t ->* row)
    (cond
     ((>= *last-row-number-before-interactive-operation* (row-number row))
      (return-from after-interactive-operation (setf it nil)))
     ((implies op (let ((reason (row-reason row)))
                    (and (consp reason) (eq op (first reason)))))
      (return-from after-interactive-operation (setf it (row-name-or-number row))))))
  (setf it nil))

(defmacro wrap-interactive-operation (op &rest forms)
  `(progn
     (before-interactive-operation)
     ,@forms
     (after-interactive-operation ',op)))

(defun mark-as-given (wff)
  ;; mark wff as given
  (store-given-row (row wff 'error)))

(defun give (wff)
  ;; perform all selected inference operations
  ;; between wff and previously given wffs
  (wrap-interactive-operation
    nil
    (let-options ((print-rows-when-given nil))
      (giver (row wff 'error)))))

(defun factor (wff)
  (wrap-interactive-operation
    factor
    (with-clock-on factoring
      (factorer (row wff 'error)))))

(defun resolve (wff1 &optional wff2)
  (wrap-interactive-operation
    resolve
    (let ((*interactive? t)
	  (*row2* (and wff2 (row wff2 'error))))
      (with-clock-on resolution
	(resolver (row wff1 'error))))))

(defun hyperresolve (nucleus &rest electrons)
  (wrap-interactive-operation
    hyperresolve
    (cl:assert (not (row-positive-p (row nucleus 'error))))		;make sure it's a nucleus
    (let ((*interactive? t)
	  (*row2* (mapcar (lambda (e) (row e 'error)) electrons))
          (*negative-hyperresolution* nil))
      (with-clock-on resolution
	(hyperresolver (row nucleus 'error))))))

(defun negative-hyperresolve (nucleus &rest electrons)
  (wrap-interactive-operation
    negative-hyperresolve
    (cl:assert (not (row-negative-p (row nucleus 'error))))		;make sure it's a nucleus
    (let ((*interactive? t)
	  (*row2* (mapcar (lambda (e) (row e 'error)) electrons))
          (*negative-hyperresolution* t))
      (with-clock-on resolution
	(hyperresolver (row nucleus 'error))))))

(defun ur-resolve (nucleus &rest electrons)
  (wrap-interactive-operation
    ur-resolve
    (cl:assert (row-clause-p (row nucleus 'error)))			;make sure it's a nucleus
    (let ((*interactive? t)
	  (*row2* (mapcar (lambda (e) (row e 'error)) electrons)))
      (with-clock-on resolution
        (ur-resolver1 (row nucleus 'error))))))

(defun paramodulate (wff &optional wff-with-equality)
  (if wff-with-equality
      (paramodulate-by wff-with-equality wff)
      (error "(PARAMODULATE wff) is unimplemented; use (PARAMODULATE wff wff-with-equality).")))

(defun paramodulate-by (wff-with-equality &optional wff)
  (wrap-interactive-operation
    paramodulate
    (let ((*interactive? t)
	  (*row2* (and wff (row wff 'error))))
      (with-clock-on paramodulation
	(paramodulater-from (row wff-with-equality 'error))))))

;;; interactive.lisp EOF
