;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark-user -*-
;;; File: tptp-symbols.lisp
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

(in-package :snark-user)

;;; defines TPTP arithmetic relations and functions in terms of SNARK ones
;;;
;;; TPTP assumes polymorphic relations and functions over disjoint integer, rational, and real domains
;;;
;;; SNARK integers are a subtype of rationals and rationals are a subtype of reals
;;;
;;; reals are represented as rationals (e.g., 0.5 -> 1/2)

(defun declare-tptp-sort (sort-name)
  (declare-subsort sort-name 'tptp-nonnumber :subsorts-incompatible t))

(defun declare-tptp-symbols1 (&key new-name)
  (declare-sort '|$int|  :iff 'integer)
  (declare-sort '|$rat|  :iff 'rational)
  (declare-sort '|$real| :iff 'real)
  ;; instead of
  ;; (declare-subsort '|$i| :top-sort-a :subsorts-incompatible t),
  ;; declare TPTP sorts so that TPTP distinct_objects can be sorted not just as strings
  (declare-subsort 'tptp-nonnumber 'top-sort :subsorts-incompatible t)
  (declare-sorts-incompatible 'tptp-nonnumber 'number)
  (declare-tptp-sort '|$i|)
  
  (labels
    ((declare-tptp-symbol (fn x)
       (mvlet (((list tptp-name name arity) x))
         (funcall fn name arity (if new-name :new-name :alias) tptp-name))))
    
    (mapc #'(lambda (x) (declare-tptp-symbol 'declare-relation x))
          '((|$less|          $$less 2)
            (|$lesseq|        $$lesseq 2)
            (|$greater|       $$greater 2)
            (|$greatereq|     $$greatereq 2)

            #+ignore
            (|$evaleq|        $$eq 2)
            
            (|$is_int|        $$integerp 1)
            (|$is_rat|        $$rationalp 1)
            (|$is_real|       $$realp 1)
            ))
    
    (mapc #'(lambda (x) (declare-tptp-symbol 'declare-function x))
          '((|$uminus|         $$uminus 1)
            (|$sum|            $$sum 2)
            (|$difference|     $$difference 2)
            (|$product|        $$product 2)
            (|$quotient|       $$quotient 2)
            (|$quotient_e|     $$quotient_e 2)
            (|$quotient_f|     $$quotient_f 2)
            (|$quotient_t|     $$quotient_t 2)
            (|$remainder_e|    $$remainder_e 2)
            (|$remainder_f|    $$remainder_f 2)
            (|$remainder_t|    $$remainder_t 2)
            (|$floor|          $$floor 1)
            (|$truncate|       $$truncate 1)
            (|$to_int|         $$floor 1)
            ))

    (snark::declare-arithmetic-function '|$to_rat|  1 :sort 'rational :rewrite-code 'to_rat-term-rewriter)
    (snark::declare-arithmetic-function '|$to_real| 1 :sort 'real     :rewrite-code 'to_real-term-rewriter)
    nil))

(defun declare-tptp-symbols2 (&optional type)
  (declare (ignore type))
  nil)

(defun to_rat-term-rewriter (term subst)
  (let ((x (first (args term))))
    (dereference x subst)
    (if (rationalp x) x (if (subsort? (term-sort x subst) (the-sort 'rational)) x none))))

(defun to_real-term-rewriter (term subst)
  (let ((x (first (args term))))
    (dereference x subst)
    (if (realp x) x (if (subsort? (term-sort x subst) (the-sort 'real)) x none))))

;;; tptp-symbols.lisp EOF
