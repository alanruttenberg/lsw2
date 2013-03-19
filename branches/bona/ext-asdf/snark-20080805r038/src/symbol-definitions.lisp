;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: symbol-definitions.lisp
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

(declaim (special *skolem-function-alist*))

(defvar *all-both-polarity*)

(eval-when (:load-toplevel :execute)
  (setf *all-both-polarity* (cons (constantly :both) nil))
  (rplacd *all-both-polarity* *all-both-polarity*)
  nil)

(defun initialize-symbol-table ()
  (setf *skolem-function-alist* nil)
  (make-symbol-table))

(defun initialize-symbol-table2 ()
  (declare-proposition 'true :locked t)
  (declare-proposition 'false :locked t)
  ;; SNARK code assumes that propositions and constants with the same name have different
  ;; internal representations so that different properties can be specified for them
  ;; this includes the case for true and false, which are treated specially
  (cl:assert (neq true 'true))
  (cl:assert (neq false 'false))
  (setf *not*
        (declare-logical-symbol
         'not
         :make-compound*-function #'negate*
         :input-code #'input-negation
         :polarity-map (list #'opposite-polarity)))
  (setf *and*
        (declare-logical-symbol
         'and
         :make-compound*-function #'conjoin*
         :input-code #'input-conjunction
         :associative (use-ac-connectives?)
         :commutative (use-ac-connectives?)
         :rewrite-code (if (use-ac-connectives?) '(and-wff-rewriter) nil)))
  (setf *or*
        (declare-logical-symbol
         'or
         :make-compound*-function #'disjoin*
         :input-code #'input-disjunction
         :associative (use-ac-connectives?)
         :commutative (use-ac-connectives?)
         :rewrite-code (if (use-ac-connectives?) '(or-wff-rewriter) nil)))
  (setf *implies*
        (declare-logical-symbol
         'implies
         :make-compound*-function #'make-implication*
         :input-code #'input-implication
         :polarity-map (list #'opposite-polarity)
         :rewrite-code '(implies-wff-rewriter)))
  (setf *implied-by*
        (declare-logical-symbol
         'implied-by
         :make-compound*-function #'make-reverse-implication*
         :input-code #'input-reverse-implication
         :polarity-map (list #'identity #'opposite-polarity)
         :rewrite-code '(implied-by-wff-rewriter)))
  (setf *iff*
        (declare-logical-symbol
         'iff
         :make-compound*-function #'make-equivalence*
         :input-code #'input-equivalence
         :polarity-map *all-both-polarity*
         :associative (use-ac-connectives?)
         :commutative (use-ac-connectives?)
         :alias '<=>))
  (setf *xor*
        (declare-logical-symbol
         'xor
         :make-compound*-function #'make-exclusive-or*
         :input-code #'input-exclusive-or
         :polarity-map *all-both-polarity*
         :associative (use-ac-connectives?)
         :commutative (use-ac-connectives?)))
  (setf *if*
        (declare-logical-symbol
         'if
         :make-compound*-function #'make-conditional*
         :input-code #'input-conditional
         :polarity-map (list (constantly :both))))
  (setf *answer-if*
        (declare-logical-symbol
         'answer-if
         :make-compound*-function #'make-conditional-answer*
         :input-code #'input-conditional-answer
         :polarity-map (list (constantly :both))))
  (setf *forall* (declare-logical-symbol 'forall :input-code #'input-quantification :to-lisp-code #'quant-compound-to-lisp))
  (setf *exists* (declare-logical-symbol 'exists :input-code #'input-quantification :to-lisp-code #'quant-compound-to-lisp))
  (setf *=* (declare-relation1 '= 2 :input-code #'input-equality :rewrite-code '(equality-rewriter) :satisfy-code '(reflexivity-satisfier) :commutative t))
  (declare-logical-symbol '=>        :macro t :input-code #'input-kif-forward-implication)
  (declare-logical-symbol '<=        :macro t :input-code #'input-kif-backward-implication)
  (declare-logical-symbol 'nand      :macro t :input-code #'input-nand)
  (declare-logical-symbol 'nor       :macro t :input-code #'input-nor)
  (declare-relation1 '/= 2 :macro t :input-code #'input-disequality)
  (setf (function-boolean-valued-p *=*) '=)
  (setf (function-logical-symbol-dual *and*) *or*)
  (setf (function-logical-symbol-dual *or*) *and*)
  (setf (function-logical-symbol-dual *forall*) *exists*)
  (setf (function-logical-symbol-dual *exists*) *forall*)

  (setf *a-function-with-left-to-right-ordering-status* (declare-function '$$_internal1 :any :ordering-status :left-to-right))
  (setf *a-function-with-multiset-ordering-status* (declare-function '$$_internal2 :any :ordering-status :multiset))

  (declare-function1 '$$quote :any :macro t :input-code #'input-quoted-constant)
  (setf *eq* (declare-relation2 '$$eq 2 :rewrite-code 'equality-rewriter :satisfy-code 'constructor-reflexivity-satisfier :alias '$$evaleq))
  (declare-code-for-lists2)
  (declare-code-for-strings)
  (declare-code-for-numbers2)
  (declare-code-for-bags2)
  (declare-code-for-dates)

  #+ignore
  (declare-relation2 'nonvariable 1 :rewrite-code 'nonvariable-rewriter :satisfy-code 'nonvariable-satisfier)
  #+ignore
  (declare-function 'the 2 :rewrite-code 'the-term-rewriter)
  nil)

(defun initialize-sort-theory2 ()
  (declare-subsort :top-sort-a t :subsorts-incompatible t)
  (declare-subsort 'string :top-sort-a)
  (declare-subsort 'list   :top-sort-a :subsorts-incompatible t)
  (declare-subsort 'bag    :top-sort-a :subsorts-incompatible t)
  (declare-subsort 'number :top-sort-a)

  (declare-subsort 'cons 'list)
  (declare-subsort 'null 'list)
  (declare-subsort 'nonempty-bag 'bag)
  (declare-subsort 'empty-bag 'bag)

  (declare-sort    'complex :iff 'number)
  (declare-subsort 'real 'complex)
  (declare-subsort 'rational 'real :subsorts-incompatible t)
  (declare-subsort 'integer 'rational)
  
  (declare-subsort 'nonnegative 'real)
  (declare-subsort 'nonpositive 'real)
  (declare-subsort 'nonzero 'number)
  (declare-sorts-incompatible 'nonnegative 'nonpositive 'nonzero)

  (declare-sort 'positive :iff '(and nonnegative nonzero))
  (declare-sort 'negative :iff '(and nonpositive nonzero))
  (declare-sort 'zero     :iff '(and nonnegative nonpositive integer))

  ;; includes sort names used by declare-number
  (dolist (sign '(positive negative nonnegative nonzero))
    (dolist (type '(number real rational integer))
      (when (if (eq 'nonzero sign) t (neq 'number type))
        (declare-sort (intern (format nil "~A-~A" sign type) :snark) :iff `(and ,sign ,type)))))

  (declare-sort 'natural :iff 'nonnegative-integer)
  nil)

(defun declare-string (x)
  (cl:assert (stringp x))
  ;; canonicalize strings so that (implies (string= str1 str2) (eq (declare-string str1) (declare-string str2)))
  (let* ((strings *string-table*)
         (v (gethash x strings)))
    (if v
        (setf x v)
        (setf (gethash x strings) x)))
  (get-constant-info x nil)
  (declare-constant-sort x 'string))

(defun declare-number (x)
  (cl:assert (numberp x))
  (cond
   ((floatp x)
    (setf x (rationalize x)))
   ((and (complexp x) (float (realpart x)))
    (setf x (complex (rationalize (realpart x)) (rationalize (imagpart x))))))
  (get-constant-info x nil)
  (when (top-sort? (constant-sort x))
    (declare-constant-sort x (number-sort-name x)))
  x)

(defun number-sort-name (x)   
  (etypecase x
    (integer
     (if (< 0 x) 'positive-integer (if (> 0 x) 'negative-integer 'zero)))
    (ratio
     (if (< 0 x) 'positive-rational 'negative-rational))
    (complex
     'nonzero)))

;;; symbol-definitions.lisp EOF
