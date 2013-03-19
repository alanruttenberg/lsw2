;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: subsume-clause.lisp
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

(defun clause-subsumes-p (clause1 clause2)
  ;; does clause1 subsume clause2?
  (clause-subsumes-p1
   (atoms-in-clause2 clause1)
   (atoms-in-clause2 clause2)
   (variables clause2 nil *frozen-variables*)))

(defun clause-subsumes-p1 (l1 l2 frozen-variables)
  (prog->
    (clause-subsumes1 l1 l2 frozen-variables ->* subst)
    (declare (ignore subst))
    (return-from prog-> t)))

(defun clause-subsumes1 (cc l1 l2 frozen-variables)
  ;; returns nil
  (cond
   ((null l1)					;clause1 is the empty clause
    (funcall cc nil)
    nil)
   ((null l2)					;clause2 is the empty clause
    nil)
   (t
    (with-clock-on clause-clause-subsumption
      (clause-subsumes2 cc l1 l2 frozen-variables)))))

(defun clause-subsumes2 (cc l1 l2 frozen-variables)
  ;; returns nil
  (cond
   ((null (rest l1))				;clause1 is a unit clause
    (prog->
      (quote t -> *subsuming*)
      (identity frozen-variables -> *frozen-variables*)
      (first l1 -> lit1)
      (first lit1 -> atom1)
      (second lit1 -> polarity1)
      (dolist l2 ->* lit2)
      (when (eq polarity1 (second lit2))
        (unify atom1 (first lit2) nil ->* subst)
        (funcall cc subst))))
   (t
    ;; new DPLL-based approach 2004-10
    (prog->
      (make-subsumption-test-dp-clause-set l1 l2 frozen-variables -> clause-set subst0)
      (case clause-set
        (:unsatisfiable
         nil)
        (:empty-set-of-clauses
         (funcall cc subst0)
         nil)
        (otherwise
         (when (trace-dpll-subsumption?)
           (format t "~2%Does    ~S" (atoms-to-clause2 l1))
           (format t "~1%subsume ~S" (atoms-to-clause2 l2))
           (when subst0
             (format t "~%Matching substitution must include ")
             (print-substitution subst0))
           (when (eq :clauses (trace-dpll-subsumption?))
             (format t "~%Matching substitution must satisfy")
             (dp-clauses 'print clause-set)))
         (dp-satisfiable-p
          clause-set
          :find-all-models -1
          :model-test-function (lambda (model)
                                 (let ((subst subst0))
                                   (dolist (atom model)
                                     (when (and (consp atom) (eq 'bind (first atom)))
                                       (setf subst (add-binding-to-substitution (second atom) subst))))
                                   (when (trace-dpll-subsumption?)
                                     (format t "~&Found matching substitution ")
                                     (print-substitution subst))
                                   (funcall cc subst)
                                   t))
          :more-units-function (and (use-lookahead-in-dpll-for-subsumption?) #'lookahead-true)
          :pure-literal-check nil
          :print-warnings (trace-dpll-subsumption?)
          :print-summary (trace-dpll-subsumption?)
          :trace nil
          :trace-choices nil)
         nil))))))

(defun make-subsumption-test-dp-clause-set (l1 l2 frozen-variables)
  (prog->
    (make-subsumption-test-clauses l1 l2 frozen-variables -> clauses subst)
    (cond
     ((eq :unsatisfiable clauses)
      :unsatisfiable)
     ((null clauses)
      (values :empty-set-of-clauses subst))
     (t
      (values (make-subsumption-test-dp-clause-set1 clauses subst) subst)))))

(defun reorder-atoms2 (l1 l2)
  ;; reorder l1 to increase likelihood that determinate matches appear first
  ;; count number of occurrences of polarity-relation pairs in l2
  ;; (count '=' doubly because it is symmetric and often matches twice)
  ;; reorder l1 in ascending order of count in l2
  (let ((counts nil))
    ;; count polarity-relation pairs in l2
    (prog->
      (dolist l2 ->* x)
      (second x -> polarity)
      (first x -> atom)
      (if (compound-p atom) (head atom) atom -> head)
      (dolist counts (push (list* head polarity (if (eq *=* head) 2 1)) counts) ->* y)
      (when (and (eq head (first y)) (eq polarity (second y)))
        (incf (cddr y) (if (eq *=* head) 2 1))
        (return)))
    (when (prog->				;only annotate (and sort) if counts are not uniform
            (cddr (first counts) -> n)
            (dolist (rest counts) nil ->* y)
            (when (not (eql n (cddr y)))
              (return t)))
      ;; annotate l1 with counts in l2
      (let ((l1* (prog->
                   (mapcar l1 ->* x)
                   (second x -> polarity)
                   (first x -> atom)
                   (if (compound-p atom) (head atom) atom -> head)
                   (dolist counts (return-from reorder-atoms2 :unsatisfiable) ->* y)
                   (when (and (eq head (first y)) (eq polarity (second y)))
                     (return (cons (cddr y) x))))))
        (when (prog->				;only sort if counts in l1 are not uniform
                (first (first l1*) -> n)
                (dolist (rest l1*) nil ->* x)
                (when (not (eql n (first x)))
                  (return t)))
          (setf l1* (stable-sort l1* #'< :key #'car))
          ;; remove annotation
          (prog->
            (dotails l1* ->* l)
            (setf (first l) (cdr (first l))))
          (setf l1 l1*))))
    l1))

(defun refine-substs (clauses subst)
  ;; eliminate matches in clauses that are incompatible with subst
  ;; return :unsatisfiable if a clause becomes empty after eliminating all its matches
  ;; trim away bindings that are already in subst
  (dotails (l clauses)
    (let* ((shortened nil)
           (clause (delete-if (lambda (x)
                                (let* ((subst1 (cdr x))
                                       (subst1* (substitution-diff2 subst1 subst)))
                                  (cond
                                   ((eq none subst1*)		;incompatible with subst
                                    (setf shortened t))		;delete it
                                   (t
                                    (unless (eq subst subst1*)
                                      (setf (cdr x) subst1*))	;subst1 duplicated bindings in subst
                                    nil))))
                              (first l))))
      (when shortened
        (if (null clause)
            (return-from refine-substs :unsatisfiable)
            (setf (first l) clause)))))
  (values clauses subst))

(defun make-subsumption-test-clauses (l1 l2 *frozen-variables*)
  ;; reorder l1 to increase likelihood that determinate matches appear first
  (setf l1 (reorder-atoms2 l1 l2))
  (when (eq :unsatisfiable l1)
    (return-from make-subsumption-test-clauses :unsatisfiable))
  (let ((clauses nil)
        (subst nil)
        (*subsuming* t))
    (prog->
      (quote nil -> subst1)
      (quote 0 -> i)
      (dolist l1 ->* lit1)
      (incf i)
      (first lit1 -> atom1)
      (second lit1 -> polarity1)
      (quote nil -> clause)			;list of possible matches for atom1 in l2
      (prog->
        (quote 0 -> j)
        (dolist l2 ->* lit2)
        (incf j)
        (first lit2 -> atom2)
        (second lit2 -> polarity2)
        (when (eq polarity1 polarity2)
          (quote 0 -> k)
          (block unify
            (unify atom1 atom2 subst ->* subst*)
            (incf k)
            (cond
             ((eq subst subst*)			;atom1 matches atom2 with no (further) instantiation
              (setf clause none)		;no clause or further search for atom1 matches is needed
              (return-from prog->))
             (t
              (setf subst1 subst*)		;save subst* in case this is the only match for atom1
              (push (cons (list 'match i j k)
                          (substitution-diff subst* subst))
                    clause)))			;clause is list of (match-atom . subst) pairs for later processing
            (when (and (test-option36?) (<= (test-option36?) k))
              (return-from unify)))))
      (cond
       ((null clause)				;there is no match for atom1, quit
        (return-from make-subsumption-test-clauses :unsatisfiable))
       ((neq none clause)
        (if (null (rest clause))		;if there is only one match for atom1
            (setf subst subst1)			;force other matches to extend it
            (push clause clauses)))))
    (if (and subst clauses) (refine-substs clauses subst) (values clauses subst))))

(defun make-subsumption-test-dp-clause-set1 (clauses subst)
  (let ((clause-set (make-dp-clause-set))
        (empty :empty-set-of-clauses)
        (dp-binding-atoms nil))
    (labels
      ((dp-binding-atom (binding &optional tv)
         ;; wrapper around dp-atom-named to ensure that there are no two binding atoms
         ;; for same variable whose values are equal-p
         ;; dp-binding-atoms is nested alists for mapping var -> val -> binding-atom
         (let* ((var (binding-var binding))
                (val (binding-value binding))
                (v (assoc var dp-binding-atoms :test #'eq))
                (v1 (if v (rest v) (progn (push (setf v (cons var nil)) dp-binding-atoms) nil))))
           (let ((v2 (and v1 (assoc-p val v1))))
             (if (null v2)
                 (let ((atom (or tv (snark-dpll::dp-atom-named (list 'bind binding) clause-set :if-does-not-exist :create))))
                   (setf (rest v) (cons (cons val atom) v1))
                   atom)
                 (cdr v2))))))
      (dobindings (binding subst)
        (dp-binding-atom binding true))
      (prog->
        (dolist clauses ->* clause)
        (cl:assert clause)			;no empty clauses
        (prog->
          (dotails clause ->* l)
          (cdr (first l) -> subst)
          (snark-dpll::dp-atom-named (car (first l)) clause-set :if-does-not-exist :create -> match-atom)
          (setf (first l) match-atom)		;replace (match-atom . subst) by dp-match-atom in clause
          (quote nil -> binding-atoms)
          (dobindings (binding subst)
            (prog->
              (dp-binding-atom binding -> atom)
              (unless (eq true atom)
                (push atom binding-atoms))))
          (cond
           ((null binding-atoms)
            (setf clause none)			;atom is aleady matched, ignore this clause
            (return-from prog->))
           (t
            ;; add clauses for (iff match (and binding1 ... bindingn))
            (setf empty nil)
            (dp-insert (cons match-atom (and binding-atoms (mapcar (lambda (x) (list 'not x)) binding-atoms))) clause-set :print-warnings :safe)
            (list (list 'not match-atom) -> match-lit-list)
            (dolist (atom binding-atoms)
              (dp-insert (cons atom match-lit-list) clause-set :print-warnings :safe)))))
        ;; add (or (match m) ... (match n)) clause for all the ways one literal can match
        (unless (eq none clause)
          (dp-insert clause clause-set :print-warnings :safe)))
      (when empty
        (return-from make-subsumption-test-dp-clause-set1 empty))
      ;; add clauses for unsatisfiability of var=val1, var=val2 bindings
      (prog->
        (dolist dp-binding-atoms ->* v)		;v=(var ((val_1 . dp-binding-atom_1)) ... (val_n . dp-binding-atom_n))
        (dotails (cdr v) ->* v1)
        (first v1 -> p1)			;p1=(val_i . dp-binding-atom_i)
        (cdr p1 -> atom_i)
        (if (eq true atom_i) nil (list (list 'not atom_i)) -> lit_i-list)
        (dolist (rest v1) ->* p2)		;p2=(val_j . dp-binding-atom_j)
        (cdr p2 -> atom_j)
        (cond
         ((neq true atom_j)
          (list 'not atom_j -> lit_j)
          (dp-insert (cons lit_j lit_i-list) clause-set :print-warnings :safe))
         (lit_i-list
          (dp-insert lit_i-list clause-set :print-warnings :safe))
         (t
          (return-from make-subsumption-test-dp-clause-set1 :unsatisfiable))))	;never happens (requires subst to be inconsistent)
      clause-set)))

(defun condenser (clause)
  ;; new approach 2004-10
  ;; enumerate matching substitutions of clause (renumbered) to itself
  ;; there is at least one but we search for one that matches all literals
  ;; in clause to a subset of its literals
  ;; remove any literals in the clause that are left over after the match
  ;;
  ;; for example, when condensing (or (p ?x) (p a)),
  ;; (or (p ?x') (p a)) subsumes (or (p ?x) (p a)) with {x' -> a}
  ;; but (p ?x) does not occur in (or (p ?x') (p a)).{x' -> a}
  ;; so (p ?x) can be removed to yield (p a) by condensing
  ;;
  ;; efficiency issue: how often will there be too many matching substitutions of clause to itself?
  ;;
  ;; should be improved by dynamically adding dp-clauses to force models to extend condensing one
  ;; also could stop early if condensed to unit or ground clause
  (let ((l2 (atoms-in-clause2 clause))
        (condensed nil))
    (cond
     ((null (rest l2))			;no condensing of unit clauses
      clause)
     (t
      (let ((vars (variables l2)))
        (cond
         ((null vars)				;no condensing of ground clauses
          clause)
         (t
          (prog->
            (renumber-new l2 -> l1)
            (clause-subsumes2 l1 l2 vars ->* subst)	;does l2 subsume itself?
            (identity condensed -> new-condensed)
            (block mapc
              (mapc l1 l2 ->* y1 x)
              (cond
               ((and				;is x unmatched by l1.subst?
                 (not (equal-p (first x) (first y1) subst))	;try this likely match first
                 (not (member x l1 :test (lambda (x y)	;then the others
                                           (and (and (neq y1 y))
                                                (eq (second x) (second y))
                                                (equal-p (first x) (first y) subst))))))
                (unless (and condensed (member x condensed :test #'eq))
                  (push x new-condensed)))
               ((and condensed (member x condensed :test #'eq))
                (setf new-condensed nil)
                (return-from mapc))))
            (when (and new-condensed (neq condensed new-condensed))
              (setf condensed new-condensed)
              (when (trace-dpll-subsumption?)
                (format t "~%Can remove ~A by condensing" (atoms-to-clause2 condensed)))))
          (if condensed
              (atoms-to-clause2 (delete-if (lambda (x) (member x condensed :test #'eq)) l2))
              clause))))))))

;;; subsume-clause.lisp EOF
