;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: constraints.lisp
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
;;; Portions created by the Initial Developer are Copyright (C) 1981-2010.
;;; All Rights Reserved.
;;;
;;; Contributor(s): Mark E. Stickel <stickel@ai.sri.com>.

(in-package :snark)

(declaim (special *processing-row*))

(defgeneric checkpoint-theory (theory)
  ;; create checkpoint
  (:method (theory)
    (error "No checkpoint method for theory ~S." theory)))

(defgeneric uncheckpoint-theory (theory)
  ;; eliminate checkpoint, keeping changes since then
  (:method (theory)
    (error "No uncheckpoint method for theory ~S." theory)))

(defgeneric restore-theory (theory)
  ;; undo changes since checkpoint, keeping checkpoint
  (:method (theory)
    (error "No restore method for theory ~S." theory)))

(defgeneric theory-closure (theory)
  ;; returns non-NIL value if theory is inconsistent
  (:method (theory)
    (error "No closure method for theory ~S." theory)))

(defgeneric theory-assert (atom theory)
  (:method (atom theory)
    (declare (ignorable atom))
    (error "No assert method for theory ~S." theory)))

(defgeneric theory-deny (atom theory)
  (:method (atom theory)
    (declare (ignorable atom))
    (error "No deny method for theory ~S." theory)))

(defgeneric theory-simplify (wff theory)
  ;; wff is conjunction of literals in negated context
  (:method (wff theory)
    (let ((row *processing-row*))
      (cond
       ((or (eq true wff) (eq false wff))
        wff)
       ((and row
             (eq false (row-wff row))
             (not (row-nonassertion-p row))
             (eq theory (row-unit-constraint row))
             (ground-p wff))
        (mvlet (((:values atom polarity) (literal-p wff :neg)))
          (if (eq :pos polarity)
              (theory-assert2 atom theory)
              (theory-deny2 atom theory)))
        false)
       (t
        (checkpoint-theory theory)
        (let ((wff* (prog->
                      (map-atoms-in-wff-and-compose-result wff :neg ->* atom polarity)
                      (cond
                       ((if (eq :pos polarity)
                            (theory-falsep atom theory)
                            (theory-truep atom theory))
;;                      (when row
;;                        (pushnew theory (row-rewrites-used row)))
                        (if (eq :pos polarity) false true))
                       ((progn
                          (if (eq :pos polarity)
                              (theory-deny atom theory)
                              (theory-assert atom theory))
                          (theory-closure theory))
                        (restore-theory theory)
                        (uncheckpoint-theory theory)
                        (return-from theory-simplify false))
                       (t
                        atom)))))
          (restore-theory theory)
          (uncheckpoint-theory theory)
          wff*)))))
  (:method (wff (theory (eql 'assumption)))
    (let ((row-wff (row-wff *processing-row*)))
      (cond
       ((and (clause-p row-wff) (clause-p wff nil nil t))
        (prog->
          (map-atoms-in-wff-and-compose-result wff ->* atom polarity)
          (or (prog->
                (map-atoms-in-wff row-wff ->* atom2 polarity2)
                (when (and (neq polarity polarity2) (equal-p atom atom2))
                  (return-from prog-> (if (eq :pos polarity) true false))))
              atom)))
       (t
        wff)))))
           
(defgeneric theory-rewrite (wff theory)
  (:method (wff theory)
    (declare (ignorable theory))
    (rewriter wff nil))
  (:method (wff (theory (eql 'assumption)))
    wff))

(defun theory-assert2 (atom theory)
  (checkpoint-theory theory)
  (theory-assert atom theory)
  (when (theory-closure theory)					;inconsistent?
    (cerror "Continue without asserting it."
            "Asserting ~A leads to a contradiction."
            atom)
    (restore-theory theory))
  (uncheckpoint-theory theory))

(defun theory-deny2 (atom theory)
  (checkpoint-theory theory)
  (theory-deny atom theory)
  (when (theory-closure theory)					;inconsistent?
    (cerror "Continue without denying it."
            "Denying ~A leads to a contradiction."
            atom)
    (restore-theory theory))
  (uncheckpoint-theory theory))

(defun theory-truep (atom theory)
  (let (inconsistent)
    (checkpoint-theory theory)
    (theory-deny atom theory)
    (setf inconsistent (theory-closure theory))
    (restore-theory theory)
    (uncheckpoint-theory theory)
    inconsistent))

(defun theory-falsep (atom theory)
  (let (inconsistent)
    (checkpoint-theory theory)
    (theory-assert atom theory)
    (setf inconsistent (theory-closure theory))
    (restore-theory theory)
    (uncheckpoint-theory theory)
    inconsistent))

(defun simplify-constraint-alist (alist)
  (and alist
       (let* ((x (first alist))
              (x* (lcons (car x) (theory-simplify (cdr x) (car x)) x)))
         (cond
          ((eq true (cdr x*))
           (simplify-constraint-alist (rest alist)))
          (t
           (lcons x* (simplify-constraint-alist (rest alist)) alist))))))

(defun rewrite-constraint-alist (alist)
  (and alist
       (let* ((x (first alist))
              (x* (lcons (car x) (theory-rewrite (cdr x) (car x)) x)))
         (cond
          ((eq true (cdr x*))
           (rewrite-constraint-alist (rest alist)))
          (t
           (lcons x* (rewrite-constraint-alist (rest alist)) alist))))))

(defun assumptive-constraint-theory-p (theory)
  ;; assumptive constraint theories can simply be assumed
  ;; they don't require row coverage
  (eq 'assumption theory))

(defun row-constrained-p (row)
  (dolist (x (row-constraints row) nil)
    (unless (eq true (cdr x))
      (return t))))

(defun row-constrained-p2 (row)
  (dolist (x (row-constraints row) nil)
    (unless (or (eq true (cdr x))
                (assumptive-constraint-theory-p (car x)))
      (return t))))

(defun row-unit-constraint (row)
  (let ((v nil))
    (dolist (x (row-constraints row))
      (cond
       ((eq true (cdr x))
        )
       (v
        (setf v nil)
        (return))
       ((assumptive-constraint-theory-p (car x))
        (return))
       (t
        (setf v x))))
    (when v
      (mvlet* (((:list* theory wff) v)
               ((:values atom polarity) (literal-p wff :neg)))
        (when atom
          (values theory atom polarity))))))

(defun row-constraint-coverage (rows)
  ;; returns t if row-constraint coverage is complete
  ;; by doing matings search over constraint wffs
  ;; but with NO INSTANTIATION
  ;; cf. Bjorner, Stickel, Uribe CADE-14 paper
  (let ((theories nil) (new-rows nil) new-rows-last)
    (dolist (row rows)
      (dolist (x (row-constraints row))
        (mvlet (((:list* theory wff) x))
          (cl:assert (neq false wff))
          (unless (or (eq true wff)
                      (member theory theories)
                      (assumptive-constraint-theory-p theory)
                      (theory-closure theory))
            (checkpoint-theory theory)
            (push theory theories)))))
    (dolist (row rows)
      (mvlet (((:values theory atom polarity) (row-unit-constraint row)))
        (cond
         ((and theory (member theory theories))
          (if (eq :pos polarity)
              (theory-assert atom theory)
              (theory-deny atom theory)))
         (t
          (collect row new-rows)))))
    (prog1
      (dolist (theory theories t)
        (unless (theory-closure theory)
          (return (row-constraint-coverage* new-rows theories))))
      (dolist (theory theories)
        (restore-theory theory)
        (uncheckpoint-theory theory)))))

(defun row-constraint-coverage* (rows theories)
  (and rows
       (dolist (x (row-constraints (first rows)) t)	;return t if all paths closed
         (mvlet (((:list* theory wff) x))	;constraint wff is conjunction of literals
           (unless (or (eq true wff)
                       (not (member theory theories))
                       (theory-closure theory))
             (prog->
               (map-atoms-in-wff wff :neg ->* atom polarity)
               (cond
                ((prog2
                  (checkpoint-theory theory)
                  (progn
                    (if (eq :pos polarity)			;trial value
                        (theory-assert atom theory)
                        (theory-deny atom theory))
                    (or (theory-closure theory)		;inconsistent now?
                        (row-constraint-coverage* (rest rows) theories)))	;all paths closed?
                  (restore-theory theory)
                  (uncheckpoint-theory theory))
                 #+ignore
                 (if (eq :pos polarity)			;assert negation and continue
                     (theory-deny atom theory)
                     (theory-assert atom theory)))
                (t
                 (return-from row-constraint-coverage* nil)))))))))	;return nil if unclosed path

(defun assumption-test1 ()
  ;; answer 1 with assumption (b 1)
  ;; answer 2 with assumption (a 2)
  ;; answer ?x with assumption (and (a ?x) (b ?x))
  (initialize)
  (use-resolution)
  (use-subsumption-by-false)
  (assert '(a 1))
  (assert '(b 2))
  (assert '(a ?x) :constraints '((assumption (a ?x))))
  (assert '(b ?x) :constraints '((assumption (b ?x))))
  (prove '(and (a ?x) (b ?x)) :answer '(values ?x)))

(defun assumption-test2 ()
  (initialize)
  (use-resolution)
  (assert '(implies (bird ?x) (flies ?x)) :constraints '((assumption (normal-wrt-flies ?x))))
  (assert '(bird tweety))
  (prove '(flies tweety)))

;;; constraints.lisp EOF
