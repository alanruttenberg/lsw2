;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: dp-refute.lisp
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

(declaim (special map-atoms-first *subsuming* *frozen-variables*))

(defstruct (context
	     (:constructor make-context (formula &optional assignment substitution))
	     (:print-function print-context))
  formula
  (substitution nil)
  (assignment nil))

(defun make-context2 (formula assignment substitution)
  (make-context
   (simplify-formula formula assignment substitution)	;should be incremental for efficiency
   assignment
   substitution))

(defun dp-refute-p (formula)
  (prog->
    (dp-refute (make-context formula) ->* substitution)
    (return-from dp-refute-p (or substitution t))))

(defun dp-refute (cc context)
  (when (trace-dp-refute?)
    (dp-refute-trace context))
  (cond
    ((eq true (context-formula context))
     )						;don't do anything if formula is not falsifiable (return failed context?)
    ((eq false (context-formula context))
     (funcall cc (context-substitution context)))	;succeeded
    (t
     (prog->
       (refute-methods context ->* x)
       (ecase (first x)

	 (instantiate				;extend substitution
	   (second x -> substitution)
;;         (cl:assert (and (neq (context-substitution context) substitution)
;;			   (tailp (context-substitution context) substitution)))
	   (dp-refute
	     (make-context2
	       (context-formula context)
	       (context-assignment context)
	       substitution)
	     ->* substitution)
	   (funcall cc substitution))

	 (split
	   (second x -> atom)
	   (third x -> value)			;refute atom-value branch first
	   (if (eq true value) false true -> not-value)
	   (when (trace-dp-refute?)
	     (dp-refute-trace context atom value))
	   (dp-refute
	     (make-context2
	       (context-formula context)
	       (cons (list atom value) (context-assignment context))
	       (context-substitution context))
	     ->* substitution)
	   (when (trace-dp-refute?)
	     (dp-refute-trace context atom not-value))
	   (dp-refute
	     (make-context2
	       (context-formula context)
	       (cons (list atom not-value) (context-assignment context))
	       substitution)
	     ->* substitution)
	   (funcall cc substitution))

	 (close-branch-and-refute-other-branch
	   (second x -> atom)
	   (third x -> value)
	   (fourth x -> substitution)
	   (if (eq true value) false true -> not-value)
;;	   (cl:assert (and (neq (context-substitution context) substitution)
;;			   (tailp (context-substitution context) substitution)))
	   (dp-refute
	     (make-context2
	       (context-formula context)
	       (cons (list atom not-value) (context-assignment context))
	       substitution)
	     ->* substitution)
	   (funcall cc substitution))))))
  nil)

(defun dp-refute-trace (context &optional atom value)
  (terpri)
  (dolist (x (context-assignment context))
    (declare (ignorable x))
    (princ "  "))
  (cond
    ((null atom)
     (princ "REFUTE: ")
     (print-context context))
    (t
     (princ "  ")
     (prin1 atom)
     (princ " <- ")
     (prin1 value))))

;;; simple versions of choose-atom, refute-methods, and simplify-formula
;;; that are suitable for SNARK are given
;;; STeP will require much more sophisticated versions

(defun choose-atom (cc context)
  ;; pick any atom not already assigned a value
  ;; better heuristic selection is called for
  (prog->
    (context-substitution context -> substitution)
    (identity map-atoms-first -> maf)
    (quote t -> map-atoms-first)
    (map-atoms-in-wff (context-formula context) ->* atom polarity)
    (declare (ignore polarity))
    (identity maf -> map-atoms-first)
    (unless (member atom (context-assignment context) :key #'car :test (lambda (x y) (equal-p x y substitution)))
      (funcall cc atom)
      ;; quit after finding first one
      ;; STeP may require additional choices, if falsifiability depends on order in which branches are explored
      (return-from choose-atom atom))))

(defun refute-methods (cc context)
  ;; pick an atom to assign
  ;; attempt to refute it by unification with a complementary assignment
  ;; there will be more ways to refute atoms when theories are interpreted
  (let ((assignment (context-assignment context))
	(substitution (context-substitution context)))
    (prog->
      (choose-atom context ->* atom)
      (quote nil -> empty-substitution-works)
      (prog-> 
	(dolist assignment ->* x)
	(first x -> atom2)
	(second x -> value2)
	(if (eq true value2) false true -> value)
	(unify atom atom2 substitution ->* substitution2)
	(when (eq substitution2 substitution)
	  (setf empty-substitution-works t))
	(funcall cc `(close-branch-and-refute-other-branch ,atom ,value ,substitution2)))
      (unless empty-substitution-works
	(funcall cc `(split ,atom ,true))))))

(defun simplify-formula (formula assignment substitution)
  (prog-> 
    (map-atoms-in-wff-and-compose-result formula ->* atom polarity)
    (declare (ignore polarity))
    (or (second (assoc-p atom assignment substitution))
        (instantiate atom substitution))))

(defun print-context (context &optional (stream *standard-output*) depth)
  (declare (ignore depth))
  (format stream "#<context formula: ")
  (prin1 (context-formula context) stream)
  (format stream "; assignment: ")
  (prin1 (context-assignment context) stream)
  (format stream "; substitution: ")
  (prin1 (context-substitution context) stream)
  (format stream ">")
  context)

(defun dp-subsume* (cc wff1 wff2 subst neg)
  (cond
    ((if neg
	 (or (eq false wff2) (eq true wff1))
	 (or (eq true wff2) (eq false wff1)))
     (funcall cc subst))
    ((if neg
	 (or (eq true wff2) (eq false wff1))
	 (or (eq false wff2) (eq true wff1)))
     )
    (t
     (prog->
       (if neg
	   (maximum-and-minimum-clause-lengths-neg wff1 subst)
	   (maximum-and-minimum-clause-lengths wff1 subst)
	   -> max1 min1)
       (declare (ignore min1))
       (if neg
	   (maximum-and-minimum-clause-lengths-neg wff2 subst)
	   (maximum-and-minimum-clause-lengths wff2 subst)
	   -> max2 min2)
       (declare (ignore max2))
       (when (> max1 min2)
	 (return-from dp-subsume*)))
     (dp-refute
      cc
      (make-context2
       (if neg (conjoin wff2 (negate wff1)) (conjoin (negate wff2) wff1))
       nil
       subst)))))

(defun dp-subsume-constraint-alists* (cc constraint-alist1 constraint-alist2 subst)
  (cond
   ((null constraint-alist1)
    (funcall cc subst))
   (t
    (prog->
      (first constraint-alist1 -> x)
      (dp-subsume* (cdr x) (or (cdr (assoc (car x) constraint-alist2)) false) subst nil ->* subst)
      (dp-subsume-constraint-alists* (rest constraint-alist1) constraint-alist2 subst ->* subst)
      (funcall cc subst))))
  nil)

(defun dp-subsume (cc wff1 wff2 subst neg)
  (prog->
    (identity *subsuming* -> sb)
    (quote t -> *subsuming*)
    (identity *frozen-variables* -> fv)		;save list of frozen variables
    (variables wff2 subst fv -> *frozen-variables*)	;add wff2's variables to frozen variables
    (dp-subsume* wff1 wff2 subst neg ->* subst)
    (identity sb -> *subsuming*)
    (identity fv -> *frozen-variables*)		;restore list of frozen variables
    (funcall cc subst)))

(defun dp-subsume+ (row1 row2)
  (prog->
    (row-wff row1 -> wff1)
    (row-wff row2 -> wff2)
    (row-constraints row1 -> constraint-alist1)
    (row-constraints row2 -> constraint-alist2)
    (row-answer row1 -> answer1)
    (row-answer row2 -> answer2)

    (row-variables row2 *frozen-variables* -> *frozen-variables*)

    (dp-subsume* wff1 wff2 nil nil ->* subst)
    (dp-subsume-constraint-alists* constraint-alist1 constraint-alist2 subst ->* subst)
    (dp-subsume* answer1 answer2 subst nil ->* subst)
    (declare (ignore subst))
    (return-from dp-subsume+ t)))

;;; dp-refute.lisp EOF
