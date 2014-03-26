;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: resolve-code.lisp
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

(defun reflexivity-satisfier (cc atom subst)
  ;; example: this is called when trying to resolve away (not (rel a b)) after
  ;; doing (declare-relation 'rel 2 :satisfy-code 'reflexivity-satisfier)
  ;; (rel a b) -> true after unifying a and b
  (mvlet (((list a b) (args atom)))
    (unify cc a b subst)))			;call cc with resulting substitutions

(defun irreflexivity-falsifier (cc atom subst)
  (reflexivity-satisfier cc atom subst))

(defun constructor-reflexivity-satisfier (cc atom subst)
  (mvlet (((list a b) (args atom)))
    (when (or (constructor-term-p a subst) (constructor-term-p b subst))
      (unify cc a b subst))))

(defun constructor-irreflexivity-falsifier (cc atom subst)
  (constructor-reflexivity-satisfier cc atom subst))

(defun variables-reflexivity-satisfier (cc atom subst)
  (mvlet (((list a b) (args atom)))
    (when (and (dereference a subst :if-variable t) (dereference b subst :if-variable t))
      (unify cc a b subst))))

(defun variables-irreflexivity-falsifier (cc atom subst)
  (variables-reflexivity-satisfier cc atom subst))

(defun variable-satisfier (cc atom subst)
  (let ((x (arg1 atom)))
    (dereference
     x subst
     :if-variable (funcall cc subst))))

(defun nonvariable-satisfier (cc atom subst)
  (let ((x (arg1 atom)))
    (dereference
     x subst
     :if-constant (funcall cc subst)
     :if-compound (funcall cc subst))))

(defun resolve-code-example1 (&optional (case 1))
  (let ((mother-table (print '((alice betty)
                               (alice barbara)
                               (betty carol)
                               (betty claudia)))))
    (flet ((mother-satisfier (cc atom subst)
             ;; the two definitions below are equivalent
             #+ignore
             (let ((args (args atom)))
               (mapc (lambda (pair) (unify cc args pair subst))
                     mother-table))
             (prog->
               (args atom -> args)
               (mapc mother-table ->* pair)
               (unify args pair subst ->* subst2)
               (funcall cc subst2))))
      (initialize)
      (print-options-when-starting nil)
      (print-rows-when-derived nil)
      (print-summary-when-finished nil)
      (case case
        (1
         (use-resolution t))
        (2
         (use-hyperresolution t))
        (3
         (use-negative-hyperresolution t)))
      (declare-relation 'mother 2 :satisfy-code #'mother-satisfier)
      (prove '(mother betty ?x) :answer '(values ?x) :name 'who-is-bettys-child?)
      (loop
        (when (eq :agenda-empty (closure))
          (return)))
      (mapcar (lambda (x) (arg1 x)) (answers)))))

(defun resolve-code-example2 (&optional (case 1))
  ;; silly example to illustrate satisfy/falsify code with residue
  ;; suppose (* a b c) means a*b=c
  ;; then use satisfy code with residue for the following resolution operations
  ;; (not (* ?x a b)) -> (not (= a b)) with {?x <- 1}
  ;; (not (* a ?x b)) -> (not (= a b)) with {?x <- 1}
  (initialize)
  (declare-constant 1)
  (declare-relation '* 3 :satisfy-code 'resolve-code-example2-satisfier)
  (case case
    (1
     (use-resolution t)
     (prove '(and (not (p ?x ?y ?z)) (* ?x a b) (* c ?y d) (* e ?z ?z))))	;nucleus
    (2
     (use-hyperresolution t)
     (prove '(and (not (p ?x ?y ?z)) (* ?x a b) (* c ?y d) (* e ?z ?z))))	;nucleus
    (3
     (use-negative-hyperresolution t)
     (prove '(* ?x a b)))							;electron
    (4
     (use-ur-resolution t)
     (prove '(and (not (p ?x ?y ?z)) (* ?x a b) (* c ?y d) (* e ?z ?z))))	;nucleus
    ))

(defun resolve-code-example2-satisfier (cc atom subst)
  (prog->
    (args atom -> args)
    (unify 1 (first args) subst ->* subst)
    (funcall cc subst (make-compound *not* (make-compound *=* (second args) (third args)))))
  (prog->
    (args atom -> args)
    (unify 1 (second args) subst ->* subst)
    (funcall cc subst (make-compound *not* (make-compound *=* (first args) (third args))))))

(define-plist-slot-accessor function :resolve-code-satisfy-code)
(define-plist-slot-accessor function :resolve-code-falsify-code)

(defun resolve-code-resolver1 (cc wff subst)
  ;; resolve-code takes wff and substitution as input,
  ;; calls continuation with new substitution and optional new wff (residue) as result
  ;;
  ;; this particular sample resolve-code uses functions, written in the style
  ;; of function-satisfy-code and function-falsify-code, but stored as
  ;; function-resolve-code-satisfy-code and function-resolve-code-falsify-code
  ;; to simultaneously satisfy/falsify literals in a clause in all possible ways
  (when (clause-p wff)
    (mvlet (((values negatoms posatoms) (atoms-in-clause3 wff)))
      (labels
        ((resolver (negatoms posatoms subst residue)
           (cond
            (negatoms
             (let ((atom (pop negatoms)))
               (dereference
                atom subst
                :if-compound-appl
                (prog->
                  ;; for every way of satisfying this atom by code,
                  ;; try to satisfy/falsify the remaining atoms by code
                  (dolist (function-resolve-code-satisfy-code (head atom)) ->* fun)
                  (funcall fun atom subst ->* subst res)
                  (resolver negatoms posatoms subst (if (and residue res)
                                                        (disjoin residue res)
                                                        (or residue res)))))
               ;; also try to satisfy/falsify remaining atoms leaving this atom in residue
               (resolver negatoms posatoms subst (if residue
                                                     (disjoin residue (negate atom))
                                                     (negate atom)))))
            (posatoms
             (let ((atom (pop posatoms)))
               (dereference
                atom subst
                :if-compound-appl
                (prog->
                  ;; for every way of falsifying this atom by code,
                  ;; try to satisfy/falsify the remaining atoms by code
                  (dolist (function-resolve-code-falsify-code (head atom)) ->* fun)
                  (funcall fun atom subst ->* subst res)
                  (resolver negatoms posatoms subst (if (and residue res)
                                                        (disjoin residue res)
                                                        (or residue res)))))
               ;; also try to satisfy/falsify remaining atoms leaving this atom in residue
               (resolver negatoms posatoms subst (if residue
                                                     (disjoin residue atom)
                                                     atom))))
            (t
             (funcall cc subst residue)))))
        (resolver negatoms posatoms subst nil)))))

(defun resolve-code-example3 ()
  ;; silly example to illustrate resolve-code for whole formulas
  ;; gives same result as resolve-code-example2, but in single rather than multiple steps
  (initialize)
  (declare-relation '* 3)
  (setf (function-resolve-code-satisfy-code (input-relation-symbol '* 3))
        '(resolve-code-example2-satisfier))
  (use-resolve-code 'resolve-code-resolver1)
  (prove '(and (not (p ?x ?y ?z)) (* ?x a b) (* c ?y d) (* e ?z ?z))))

;;; resolve-code.lisp EOF
