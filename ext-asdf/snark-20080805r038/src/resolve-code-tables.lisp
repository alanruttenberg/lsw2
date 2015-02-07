;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: resolve-code-tables.lisp
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

(defun table-satisfier (cc atom subst)
  ;; enables procedural attachment of a table to a relation
  (let* ((args (args atom))
         (pattern (table-lookup-pattern args subst)))
    (cond
     ((eq none pattern)
      )						;inapplicable
     (t
      (prog->
        (predicate-to-table (function-name (head atom)) -> table mapper exporters)
        (funcall mapper table exporters pattern subst ->* subst)
        (funcall cc subst))))))

(defun table-rewriter (atom subst)
  ;; assume completeness of table to return false
  (let* ((args (args atom))
         (pattern (table-lookup-pattern args subst)))
    (cond
     ((eq none pattern)
      none)						;inapplicable
     ((ground-p pattern)
      (prog-> 
        (predicate-to-table (function-name (head atom)) -> table mapper exporters)
        (funcall mapper table exporters pattern nil ->* subst)
        (declare (ignore subst))
        (return-from table-rewriter true))		;true if in table
      (dolist (x pattern)
        (unless (constant-constructor x)
          (return-from table-rewriter none)))		;don't rewrite if args aren't constructors
      false)						;false if not in table
     (t
      (dolist (x pattern)
        (unless (or (variable-p x) (constant-constructor x))
          (return-from table-rewriter none)))		;don't rewrite if args aren't constructors
      (prog->
        (predicate-to-table (function-name (head atom)) -> table mapper exporters)
        (quote nil -> *frozen-variables*)
        (funcall mapper table exporters pattern nil ->* subst)
        (declare (ignore subst))
        (return-from table-rewriter none))		;don't rewrite if an instance exists
      false))))						;false if there are no instances

(defun table-lookup-pattern (args subst)
  (mapcar
   (lambda (arg)
     (dereference
      arg subst
      :if-compound (return-from table-lookup-pattern none)	;inapplicable
      :if-variable arg
      :if-constant arg))
   args))

(defun simple-table-mapper (cc table exporters pattern subst)
  ;; this mapper function just does linear search of the table
  (let ((revvars nil))
    (dolist (x pattern)
      (when (variable-p x)
        (push x revvars)))
    (dolist (row table)
      (do ((r row (rest r))
           (p pattern (rest p)))
          ((or (null r) (null p))
           (when (and (null r) (null p))
             (do ((r row (rest r))
                  (p pattern (rest p))
                  (e exporters (rest e))
                  (revvals nil))
                 ((null r)
                  (unify cc revvars revvals subst))
               (when (variable-p (first p))
                 (push (if (first e)
                           (funcall (first e) (first r))
                           (declare-constant (first r)))
                       revvals)))))
        (unless (or (equal (first r) (first p)) (variable-p (first p)))
          (return))))
    nil))

(defun predicate-to-table (p)
  (relation-to-table p))

(defun relation-to-table (p)
  ;; return table for relation p (could be filename or some other way to refer to a file),
  ;; a mapper function (finds tuples in the table that match the pattern),
  ;; and an export function for each column
  (case p
    ;; supervises example
    ;; (in package SNARK-USER so it's largely invisible except for running the example)
    (snark-user::supervises
     (values '(("perrault" "lowrance")
               ("lowrance" "stickel")
               ("lowrance" "waldinger"))
             'simple-table-mapper
             (consn (lambda (x) (declare-constant x :sort 'person)) nil 2)))
    ))

(defun test-table-resolver (&optional (test 1))
  (initialize)
  (use-resolution)
  (declare-sort 'person)
  (declare-relation
   'snark-user::supervises 2
   :satisfy-code 'table-satisfier
   :rewrite-code 'table-rewriter)
  (declare-constant "lowrance" :sort 'person)
  (declare-constant "stickel"  :sort 'person)
  (declare-constant 'stickel   :sort 'person)
  (ecase test
    (1
     (prove '(snark-user::supervises "lowrance" "stickel")))
    (2
     (prove '(snark-user::supervises "lowrance" ?person) :answer '(values ?person)))
    (3
     (prove '(snark-user::supervises ?person "stickel") :answer '(values ?person)))
    (4
     (prove '(snark-user::supervises ?person1 ?person2) :answer '(values ?person1 ?person2)))
    (5
     (prove '(not (snark-user::supervises "stickel" "perrault"))))
    (6
     (prove '(not (snark-user::supervises "stickel" ?person)) :answer '(values ?person)))
    (7
     ;; should fail (stickel isn't constructor)
     (prove '(not (snark-user::supervises stickel "perrault"))))
    (8
     ;; should fail (stickel isn't constructor)
     (prove '(not (snark-user::supervises stickel ?person))))
    )
  (loop
    (when (eq :agenda-empty (closure))
      (return)))
  (print-rows))

;;; resolve-code-tables.lisp EOF
