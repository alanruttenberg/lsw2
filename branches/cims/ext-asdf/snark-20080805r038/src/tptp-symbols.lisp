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
;;; Portions created by the Initial Developer are Copyright (C) 1981-2010.
;;; All Rights Reserved.
;;;
;;; Contributor(s): Mark E. Stickel <stickel@ai.sri.com>.

(in-package :snark-user)

;;; defines TPTP arithmetic relations and functions in terms of SNARK ones
;;;
;;; TPTP assumes polymorphic relations and functions over disjoint integer, rational, and real domains
;;;
;;; SNARK does not allow polymorphic relations and functions
;;; and integers are a subtype of rationals and rationals are a subtype of reals
;;;
;;; reals are represented as rationals (e.g., 0.5 -> 1/2)
;;;
;;; SNARK provides typed versions of functions like $$SUM_INTEGER
;;; and untyped versions like $$SUM, whose result type is number
;;;
;;; TPTP $sum cannot be replaced SNARK $$SUM if the term must be of type integer/rational/real
;;;
;;; declare-tptp-symbols1 declares strongly typed functions names
;;; $less_integer, $sum_integer, etc.
;;;
;;; declare-tptp-symbols2 declares TPTP names
;;; $less, $sum, etc. to be equivalent to
;;; $less_integer, $sum_integer, etc. (depending on the type argument)
;;; if no type is specified,
;;; TPTP $sum is replaced by SNARK $$SUM, despite the type issue

(defun declare-tptp-symbols1 (&key (new-name t))
  (declare-sort '|$int|  :iff 'integer)
  (declare-sort '|$rat|  :iff 'rational)
  (declare-sort '|$real| :iff 'real)
  (declare-subsort '|$i| :top-sort-a :subsorts-incompatible t)
  
  (labels
    ((declare-tptp-symbol (fn x)
       (let ((tptp-name (first x)) (name (second x)) (arity (third x)))
         (funcall fn name arity (if new-name :new-name :alias) tptp-name))))
    
    (mapc #'(lambda (x) (declare-tptp-symbol 'declare-relation x))
          '((|$less_int|      $$less_integer 2)        (|$less_rat|      $$less_rational 2)        (|$less_real|      $$less_real 2)
            (|$lesseq_int|    $$lesseq_integer 2)      (|$lesseq_rat|    $$lesseq_rational 2)      (|$lesseq_real|    $$lesseq_real 2)
            (|$greater_int|   $$greater_integer 2)     (|$greater_rat|   $$greater_rational 2)     (|$greater_real|   $$greater_real 2)
            (|$greatereq_int| $$greatereq_integer 2)   (|$greatereq_rat| $$greatereq_rational 2)   (|$greatereq_real| $$greatereq_real 2)
            
            (|$evaleq_int|    $$eq_integer 2)          (|$evaleq_rat|    $$eq_rational 2)          (|$evaleq_real|    $$eq_real 2)
            
            (|$is_int|        $$integerp 1)
            (|$is_rat|        $$rationalp 1)
            (|$is_real|       $$realp 1)
            ))
    
    (mapc #'(lambda (x) (declare-tptp-symbol 'declare-function x))
          '((|$uminus_int|     $$uminus_integer 1)       (|$uminus_rat|     $$uminus_rational 1)       (|$uminus_real|     $$uminus_real 1)
            (|$sum_int|        $$sum_integer 2)          (|$sum_rat|        $$sum_rational 2)          (|$sum_real|        $$sum_real 2)
            (|$difference_int| $$difference_integer 2)   (|$difference_rat| $$difference_rational 2)   (|$difference_real| $$difference_real 2)
            (|$product_int|    $$product_integer 2)      (|$product_rat|    $$product_rational 2)      (|$product_real|    $$product_real 2)

            (|$to_int|         $$floor 1)
            ))

    (snark::declare-arithmetic-function '|$to_rat|  1 #'(lambda (term subst) (snark::arithmetic-term-rewriter1 term subst #'rationalp #'identity)) :sort 'rational)
    (snark::declare-arithmetic-function '|$to_real| 1 #'(lambda (term subst) (snark::arithmetic-term-rewriter1 term subst #'realp #'identity)) :sort 'real)
    nil))

(defun declare-tptp-symbols2 (&optional type)
  (ecase type
    ((nil)
     (declare-relation '$$less                2 :alias '|$less|)
     (declare-relation '$$lesseq              2 :alias '|$lesseq|)
     (declare-relation '$$greater             2 :alias '|$greater|)
     (declare-relation '$$greatereq           2 :alias '|$greatereq|)
     (declare-relation '$$eq                  2 :alias '|$evaleq|)
     (declare-function '$$uminus              1 :alias '|$uminus|)
     (declare-function '$$sum                 2 :alias '|$sum|)
     (declare-function '$$difference          2 :alias '|$difference|)
     (declare-function '$$product             2 :alias '|$product|)
     )
    (integer
     (declare-relation '$$less_integer        2 :alias '|$less|)
     (declare-relation '$$lesseq_integer      2 :alias '|$lesseq|)
     (declare-relation '$$greater_integer     2 :alias '|$greater|)
     (declare-relation '$$greatereq_integer   2 :alias '|$greatereq|)
     (declare-relation '$$eq_integer          2 :alias '|$evaleq|)
     (declare-function '$$uminus_integer      1 :alias '|$uminus|)
     (declare-function '$$sum_integer         2 :alias '|$sum|)
     (declare-function '$$difference_integer  2 :alias '|$difference|)
     (declare-function '$$product_integer     2 :alias '|$product|)
     )
    (rational
     (declare-relation '$$less_rational       2 :alias '|$less|)
     (declare-relation '$$lesseq_rational     2 :alias '|$lesseq|)
     (declare-relation '$$greater_rational    2 :alias '|$greater|)
     (declare-relation '$$greatereq_rational  2 :alias '|$greatereq|)
     (declare-relation '$$eq_rational         2 :alias '|$evaleq|)
     (declare-function '$$uminus_rational     1 :alias '|$uminus|)
     (declare-function '$$sum_rational        2 :alias '|$sum|)
     (declare-function '$$difference_rational 2 :alias '|$difference|)
     (declare-function '$$product_rational    2 :alias '|$product|)
     )
    (real
     (declare-relation '$$less_real           2 :alias '|$less|)
     (declare-relation '$$lesseq_real         2 :alias '|$lesseq|)
     (declare-relation '$$greater_real        2 :alias '|$greater|)
     (declare-relation '$$greatereq_real      2 :alias '|$greatereq|)
     (declare-relation '$$eq_real             2 :alias '|$evaleq|)
     (declare-function '$$uminus_real         1 :alias '|$uminus|)
     (declare-function '$$sum_real            2 :alias '|$sum|)
     (declare-function '$$difference_real     2 :alias '|$difference|)
     (declare-function '$$product_real        2 :alias '|$product|)
     ))
  nil)

;;; tptp-symbols.lisp EOF
