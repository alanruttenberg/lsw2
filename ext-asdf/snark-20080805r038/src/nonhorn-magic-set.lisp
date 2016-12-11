;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: nonhorn-magic-set.lisp
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
;;; Portions created by the Initial Developer are Copyright (C) 1981-2007.
;;; All Rights Reserved.
;;;
;;; Contributor(s): Mark E. Stickel <stickel@ai.sri.com>.

(in-package :snark)

(defun make-magic-goal-atom (atom)
  (flet ((magic-goal-name (name)
           (intern (format nil "~A_~A" 'goal name) :snark-user)))
    (dereference
     atom nil
     :if-constant (let ((v (constant-magic atom)))
                    (if (or (null v) (eq 'goal v))
                        true
                        (if (eq t v)
                            (setf (constant-magic atom)
                                  (declare-proposition
                                   (magic-goal-name atom)
                                   :magic 'goal))
                            v)))
     :if-compound (let* ((head (head atom))
                         (v (function-magic head)))
                    (if (or (null v) (eq 'goal v))
                        true
                        (make-compound* (if (eq t v)
                                            (setf (function-magic head)
                                                  (declare-relation
                                                   (magic-goal-name (function-name head))
                                                   (function-arity head)
                                                   :commutative (function-commutative head)
                                                   :magic 'goal))
                                            v)
                                        (args atom)))))))

(defun magic-transform-clause (cc clause &key (transform-negative-clauses t) (transform-positive-units nil))
  ;; {d} yields
  ;;   {d}              if transform-positive-units is false
  ;;   or
  ;;   {~goal_d, d}     if transform-positive-units is true
  ;; {d, e, f} yields
  ;;   {~goal_d, ~goal_e, ~goal_f, d, e, f}
  ;; {~a} yields
  ;;   {goal_a}         if transform-negative-clauses is true
  ;;   and
  ;;   {~a}
  ;; {~a, ~b, ~c} yields
  ;;   {goal_a}         if transform-negative-clauses is true
  ;;   and
  ;;   {~a, goal_b}     if transform-negative-clauses is true
  ;;   and
  ;;   {~a, ~b, goal_c} if transform-negative-clauses is true
  ;;   and
  ;;   {~a, ~b, ~c}
  ;; {~a, ~b, ~c, d, e, f} yields
  ;;   {~goal_d, ~goal_e, ~goal_f, goal_a}
  ;;   and
  ;;   {~goal_d, ~goal_e, ~goal_f, ~a, goal_b}
  ;;   and
  ;;   {~goal_d, ~goal_e, ~goal_f, ~a, ~b, goal_c}
  ;;   and
  ;;   {~goal_d, ~goal_e, ~goal_f, ~a, ~b, ~c, d, e, f}
  (let ((posatoms nil) posatoms-last
        (negatoms nil) negatoms-last)
    (prog->
      (map-atoms-in-clause clause ->* atom polarity)
      (if (eq :pos polarity) (collect atom posatoms) (collect atom negatoms)))
    (cl:assert (not (and (null posatoms) (null negatoms))))
    (let ((l nil) l-last)
      (dolist (atom posatoms)
        (collect (negate (make-magic-goal-atom atom)) l))
      (dolist (atom negatoms)
        (unless (and (null posatoms) (not transform-negative-clauses))
          (funcall cc (disjoin* (append l (list (make-magic-goal-atom atom))))))
        (collect (negate atom) l))
      (cond
       ((and (null negatoms) (null (rest posatoms)) (not transform-positive-units))
        (funcall cc (first posatoms)))
       (t
        (funcall cc (disjoin* (append l posatoms)))))))
  nil)

(defun magic-transform-wff (wff &key (transform-negative-clauses t) (transform-positive-units nil))
  ;; for use only if wff is a clause or conjunction of clauses
  ;; magic-transform-wff is idempotent
  (if (or (eq true wff) (eq false wff))
      wff
      (let ((clauses nil) clauses-last)
        (prog->
          (map-conjuncts wff ->* clause)
          (magic-transform-clause
           clause
           :transform-negative-clauses transform-negative-clauses
           :transform-positive-units transform-positive-units
           ->* clause)
          (collect clause clauses))
        (conjoin* clauses))))

(defun proposition-magic-goal-p (prop)
  (eq 'goal (constant-magic prop)))

(defun relation-magic-goal-p (rel)
  (eq 'goal (function-magic rel)))

(defun magic-goal-atom-p (atom)
  (dereference
   atom nil
   :if-constant (proposition-magic-goal-p atom)
   :if-compound (relation-magic-goal-p (head atom))))

(defun magic-goal-occurs-p (wff)
  (prog->
    (map-atoms-in-wff wff ->* atom polarity)
    (when (and (eq :pos polarity) (magic-goal-atom-p atom))
      (return-from prog-> t))))

;;; nonhorn-magic-set.lisp EOF
