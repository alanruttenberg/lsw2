;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: common-lisp-user -*-
;;; File: dpll-system.lisp
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

(in-package :common-lisp-user)

(defpackage :snark-dpll
  (:use :common-lisp :snark-lisp)
  (:export
   #:dp-prover #:dp-version
   #:dp-tracing #:dp-tracing-state #:dp-tracing-models #:dp-tracing-choices
   #:dp-satisfiable-p #:dp-satisfiable-file-p #:make-dp-clause-set
   #:dp-insert #:dp-insert-sorted #:dp-insert-wff #:dp-insert-file
   #:dp-count #:dp-clauses #:dp-output-clauses-to-file #:wff-clauses
   #:dp-horn-clause-set-p
   #:checkpoint-dp-clause-set #:restore-dp-clause-set #:uncheckpoint-dp-clause-set
   #:choose-an-atom-of-a-shortest-clause
   #:choose-an-atom-of-a-shortest-clause-randomly
   #:choose-an-atom-of-a-shortest-clause-with-most-occurrences
   #:choose-an-atom-of-a-shortest-clause-with-most-occurrences-randomly
   #:choose-an-atom-of-a-shortest-positive-clause
   #:choose-an-atom-of-a-shortest-positive-clause-randomly
   #:choose-an-atom-of-a-shortest-positive-clause-with-most-occurrences
   #:choose-an-atom-of-a-shortest-positive-clause-with-most-occurrences-randomly
   #:lookahead-true #:lookahead-false
   #:lookahead-true-false #:lookahead-false-true
   ))

(loads "davis-putnam3")

;;; dpll-system.lisp EOF
