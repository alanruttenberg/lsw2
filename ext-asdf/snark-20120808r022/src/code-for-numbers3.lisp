;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: code-for-numbers3.lisp
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

(in-package :snark)

;;; SNARK can evaluate arithmetic expressions as if by table lookup
;;; for procedurally attached relations and functions
;;;
;;; most of what SNARK "knows" about numbers is limited by this notion of table lookup;
;;; few if any more general properties are known
;;; like (= (+ x 0) x), (= (* x 0) 0), (exists (x) (< x 0)),
;;; associativity and commutativity of + and *, etc.
;;;
;;; this is intended to provide simple arithmetic calculation and not much if any symbolic algebra
;;;
;;; SNARK numbers are represented by Lisp rational numbers (integers or ratios)
;;; and complex numbers with rational real and imaginary parts
;;;
;;; floating-point numbers are replaced by rationals when input
;;;
;;; SNARK number type hierarchy: number = complex > real > rational > integer
;;;
;;; arithmetic relations are encoded in terms of $less
;;; using lexicographic ordering of complex numbers
;;; that also enables additive cancellation law
;;; and multiplicative cancellation law for multiplication by nonzero reals

(defvar *sum*)
(defvar *product*)
(defvar *less*)
(defvar *reciprocal*)

(defun rnumberp (x)
  ;; test for SNARK number, no floats
  (or (rationalp x) (and (complexp x) (rationalp (realpart x)) (rationalp (imagpart x)))))

(defun nonzero-rnumberp (x)
  (and (rnumberp x) (neql 0 x)))

(defun nonzero-rationalp (x)
  (and (rationalp x) (neql 0 x)))

(defun less? (x y)
  ;; extend < to total lexicographic ordering of complex numbers so that
  ;; a < b  or  a = b  or  a > b
  ;; a < b  iff  a+c < b+c
  ;; a < b  iff  a*c < b*c  (real c>0)
  ;; a < b  iff  a*c > b*c  (real c<0)
  (or (< (realpart x) (realpart y))
      (and (= (realpart x) (realpart y))
           (< (imagpart x) (imagpart y)))))

(defun lesseq? (x y)
  (or (= x y) (less? x y)))

(defun greater? (x y)
  (less? y x))

(defun greatereq? (x y)
  (lesseq? y x))

(defun euclidean-quotient (number &optional (divisor 1))
  (mvlet (((values quotient remainder) (truncate number divisor)))
    (if (minusp remainder)
        (if (plusp divisor)
            (values (- quotient 1) (+ remainder divisor))
            (values (+ quotient 1) (- remainder divisor)))
        (values quotient remainder))))

(defun euclidean-remainder (number &optional (divisor 1))
  ;; 0 <= remainder < abs(divisor)
  (nth-value 1 (euclidean-quotient number divisor)))

(defun ceiling-remainder (number &optional (divisor 1))
  (nth-value 1 (ceiling number divisor)))

(defun round-remainder (number &optional (divisor 1))
  (nth-value 1 (round number divisor)))

(defun declare-arithmetic-characteristic-relation (name pred sort &rest options)
  (apply 'declare-characteristic-relation name pred sort :constraint-theory 'arithmetic options))

(defun declare-arithmetic-relation (name arity &rest options)
  (apply 'declare-relation2 name arity
         :constraint-theory 'arithmetic
         `(,@options :sort ((t number)))))

(defun declare-arithmetic-function (name arity &rest options &key (sort 'number) &allow-other-keys)
  (apply 'declare-function2 name arity
         :constraint-theory 'arithmetic
         (if (consp sort) options `(:sort (,sort (t number)) ,@options))))

(defun declare-code-for-numbers ()
  (declare-constant 0)
  (declare-constant 1)
  (declare-constant -1)

  (declare-arithmetic-characteristic-relation '$$numberp   #'rnumberp  'number)
  (declare-arithmetic-characteristic-relation '$$complexp  #'rnumberp  'complex)	;all Lisp numbers are SNARK complex numbers
  (declare-arithmetic-characteristic-relation '$$realp     #'rationalp 'real)		;no floats though
  (declare-arithmetic-characteristic-relation '$$rationalp #'rationalp 'rational)
  (declare-arithmetic-characteristic-relation '$$integerp  #'integerp  'integer)
  (declare-arithmetic-characteristic-relation '$$naturalp  #'naturalp  'natural)

  (declare-arithmetic-inequality-relations)

  (setf *sum* (declare-arithmetic-function '$$sum 2
                                           :associative t
                                           :commutative t
                                           :sort 'number :sort-code 'arithmetic-term-sort-computer1
                                           :rewrite-code (list #'(lambda (x s) (arithmetic-term-rewriter3 x s #'+ 0 none))
                                                               'sum-term-rewriter1)
                                           :arithmetic-relation-rewrite-code 'sum-rel-number-atom-rewriter))
  
  (setf *product* (declare-arithmetic-function '$$product 2
                                               :associative t
                                               :commutative t
                                               :sort 'number :sort-code 'arithmetic-term-sort-computer1
                                               :rewrite-code (list #'(lambda (x s) (arithmetic-term-rewriter3 x s #'* 1 0))
                                                                   #'(lambda (x s) (distributivity-rewriter x s *sum*)))
                                               :arithmetic-relation-rewrite-code 'product-rel-number-atom-rewriter))
  
  (declare-arithmetic-function '$$uminus     1 :sort 'number :sort-code 'arithmetic-term-sort-computer1 :rewrite-code 'uminus-term-rewriter)
  (declare-arithmetic-function '$$difference 2 :sort 'number :sort-code 'arithmetic-term-sort-computer1 :rewrite-code #'(lambda (x s) (arithmetic-term-rewriter5 x s *sum* '$$uminus)))

  (declare-arithmetic-function '$$floor      1 :sort 'integer :rewrite-code #'(lambda (x s) (arithmetic-term-rewriter4 x s #'floor)))
  (declare-arithmetic-function '$$ceiling    1 :sort 'integer :rewrite-code #'(lambda (x s) (arithmetic-term-rewriter4 x s #'ceiling)))
  (declare-arithmetic-function '$$truncate   1 :sort 'integer :rewrite-code #'(lambda (x s) (arithmetic-term-rewriter4 x s #'truncate)))
  (declare-arithmetic-function '$$round      1 :sort 'integer :rewrite-code #'(lambda (x s) (arithmetic-term-rewriter4 x s #'round)))

  ;; partial, guard against division by zero
  (declare-arithmetic-function '$$quotient_f 2 :sort 'integer                                          :rewrite-code #'(lambda (x s) (arithmetic-term-rewriter2 x s #'rationalp #'floor)))
  (declare-arithmetic-function '$$quotient_c 2 :sort 'integer                                          :rewrite-code #'(lambda (x s) (arithmetic-term-rewriter2 x s #'rationalp #'ceiling)))
  (declare-arithmetic-function '$$quotient_t 2 :sort 'integer                                          :rewrite-code #'(lambda (x s) (arithmetic-term-rewriter2 x s #'rationalp #'truncate)))
  (declare-arithmetic-function '$$quotient_r 2 :sort 'integer                                          :rewrite-code #'(lambda (x s) (arithmetic-term-rewriter2 x s #'rationalp #'round)))
  (declare-arithmetic-function '$$quotient_e 2 :sort 'integer                                          :rewrite-code #'(lambda (x s) (arithmetic-term-rewriter2 x s #'rationalp #'euclidean-quotient)))
  (declare-arithmetic-function '$$remainder_f 2 :sort 'real :sort-code 'arithmetic-term-sort-computer3 :rewrite-code #'(lambda (x s) (arithmetic-term-rewriter2 x s #'rationalp #'mod)))
  (declare-arithmetic-function '$$remainder_c 2 :sort 'real :sort-code 'arithmetic-term-sort-computer3 :rewrite-code #'(lambda (x s) (arithmetic-term-rewriter2 x s #'rationalp #'ceiling-remainder)))
  (declare-arithmetic-function '$$remainder_t 2 :sort 'real :sort-code 'arithmetic-term-sort-computer3 :rewrite-code #'(lambda (x s) (arithmetic-term-rewriter2 x s #'rationalp #'rem)))
  (declare-arithmetic-function '$$remainder_r 2 :sort 'real :sort-code 'arithmetic-term-sort-computer3 :rewrite-code #'(lambda (x s) (arithmetic-term-rewriter2 x s #'rationalp #'round-remainder)))
  (declare-arithmetic-function '$$remainder_e 2 :sort 'real :sort-code 'arithmetic-term-sort-computer3 :rewrite-code #'(lambda (x s) (arithmetic-term-rewriter2 x s #'rationalp #'euclidean-remainder)))

  ;; partial, guard against division by zero
  (setf *reciprocal* (declare-arithmetic-function '$$reciprocal 1
                                                  :sort 'number :sort-code 'arithmetic-term-sort-computer2
                                                  :rewrite-code #'(lambda (x s) (arithmetic-term-rewriter2 x s #'rnumberp #'/))
                                                  :arithmetic-relation-rewrite-code 'reciprocal-rel-number-atom-rewriter))
  (declare-arithmetic-function '$$quotient  2 :sort 'number :sort-code 'arithmetic-term-sort-computer2 :rewrite-code #'(lambda (x s) (arithmetic-term-rewriter5 x s *product* '$$reciprocal)))

  ;; abs of complex numbers might be irrational
  (declare-arithmetic-function '$$abs       1 :sort 'real :sort-code 'arithmetic-term-sort-computer3 :rewrite-code #'(lambda (x s) (arithmetic-term-rewriter1 x s #'rationalp #'abs)))

  (declare-arithmetic-function '$$realpart  1 :sort 'real :sort-code 'arithmetic-term-sort-computer3 :rewrite-code #'(lambda (x s) (arithmetic-term-rewriter1 x s #'rnumberp #'realpart)))
  (declare-arithmetic-function '$$imagpart  1 :sort 'real :sort-code 'arithmetic-term-sort-computer3 :rewrite-code #'(lambda (x s) (arithmetic-term-rewriter1 x s #'rnumberp #'imagpart)))
  nil)

(defun declare-arithmetic-inequality-relations ()
  (setf *less* (declare-arithmetic-relation '$$$less 2
                                            :rewrite-code (list 'irreflexivity-rewriter
                                                                #'(lambda (x s) (arithmetic-atom-rewriter1 x s #'rnumberp #'less?))
                                                                'arithmetic-relation-rewriter
                                                                'term-rel-term-to-0-rel-difference-atom-rewriter)
                                            :falsify-code 'irreflexivity-falsifier))
  (declare-arithmetic-relation '$$$greater   2 :rewrite-code #'(lambda (x s) (arithmetic-atom-rewriter4 x s '$$$less t nil)))
  (declare-arithmetic-relation '$$$lesseq    2 :rewrite-code #'(lambda (x s) (arithmetic-atom-rewriter4 x s '$$$less t t)))
  (declare-arithmetic-relation '$$$greatereq 2 :rewrite-code #'(lambda (x s) (arithmetic-atom-rewriter4 x s '$$$less nil t)))
  (let ((inputter
         (let ((done nil))
           (function
            (lambda (head args polarity)
              (declare (ignorable head args polarity))
              (unless done
                (setf done t)
                (assert '(forall (x) (not ($$less x x)))    :name :$$less-is-irreflexive)
                (assert '(forall (x) (not ($$greater x x))) :name :$$greater-is-irreflexive)
                (assert '(forall (x) ($$lesseq x x))        :name :$$lesseq-is-reflexive)
                (assert '(forall (x) ($$greatereq x x))     :name :$$greatereq-is-reflexive)
                (assert '(forall ((x number) (y number)) (implied-by ($$less x y)                 ($$$less x y)))  :name :solve-$$less-by-$$$less)
                (assert '(forall ((x number) (y number)) (implied-by ($$greater x y)              ($$$less y x)))  :name :solve-$$greater-by-$$$less)
                (assert '(forall ((x number) (y number)) (implied-by ($$lesseq x y)          (not ($$$less y x)))) :name :solve-$$lesseq-by-$$$less)
                (assert '(forall ((x number) (y number)) (implied-by ($$greatereq x y)       (not ($$$less x y)))) :name :solve-$$greatereq-by-$$$less)
                (assert '(forall ((x number) (y number)) (implied-by (not ($$less x y))      (not ($$$less x y)))) :name :solve-~$$less-by-$$$less)
                (assert '(forall ((x number) (y number)) (implied-by (not ($$greater x y))   (not ($$$less y x)))) :name :solve-~$$greater-by-$$$less)
                (assert '(forall ((x number) (y number)) (implied-by (not ($$lesseq x y))         ($$$less y x)))  :name :solve-~$$lesseq-by-$$$less)
                (assert '(forall ((x number) (y number)) (implied-by (not ($$greatereq x y))      ($$$less x y)))  :name :solve-~$$greatereq-by-$$$less))
              none)))))
    (declare-relation '$$less      2 :input-code inputter :rewrite-code #'(lambda (x s) (arithmetic-atom-rewriter1 x s #'rnumberp #'less?)))
    (declare-relation '$$greater   2 :input-code inputter :rewrite-code #'(lambda (x s) (arithmetic-atom-rewriter1 x s #'rnumberp #'greater?)))
    (declare-relation '$$lesseq    2 :input-code inputter :rewrite-code #'(lambda (x s) (arithmetic-atom-rewriter1 x s #'rnumberp #'lesseq?)))
    (declare-relation '$$greatereq 2 :input-code inputter :rewrite-code #'(lambda (x s) (arithmetic-atom-rewriter1 x s #'rnumberp #'greatereq?))))
  nil)

(defun arithmetic-term-sort-computer0 (term subst sort-names default-sort-name)
  ;; when sort-names is '(integer rational real) and default-sort-name is number
  ;; if all arguments are integers then integer
  ;; elif all arguments are rationals then rational
  ;; elif all arguments are reals then real
  ;; else number
  (let ((top-arg-sort (the-sort (pop sort-names))))
    (dolist (arg (args term) top-arg-sort)
      (let ((arg-sort (term-sort arg subst)))
        (when (or (top-sort? arg-sort)
                  (loop
                    (cond
                     ((subsort? arg-sort top-arg-sort)
                      (return nil))
                     ((null sort-names)
                      (return t))
                     (t
                      (setf top-arg-sort (the-sort (pop sort-names)))))))
          (return (the-sort default-sort-name)))))))

(defun arithmetic-term-sort-computer1 (term subst)
  (arithmetic-term-sort-computer0 term subst '(integer rational real) 'number))

(defun arithmetic-term-sort-computer2 (term subst)
  (arithmetic-term-sort-computer0 term subst '(rational real) 'number))

(defun arithmetic-term-sort-computer3 (term subst)
  (arithmetic-term-sort-computer0 term subst '(integer rational) 'real))

(defun arithmetic-expr-args (x subst pred)
  ;; return dereferenced arguments of x if all satisfy pred; otherwise, return none
  (prog->
    (split-if (args x) subst ->* arg)
    (or (funcall pred arg) (return-from arithmetic-expr-args none))))

(defun arithmetic-atom-rewriter1 (atom subst pred operator)
  (let ((args (arithmetic-expr-args atom subst pred)))
    (if (eq none args) none (if (apply operator args) true false))))

(defun arithmetic-atom-rewriter4 (atom subst newhead reverse negate)
  ;; a<=b -> ~(b<a)
  ;; a>b -> b<a
  ;; a>=b -> ~(a<b)
  (declare (ignorable subst))
  (let* ((args (args atom))
         (atom* (make-compound* (input-relation-symbol newhead (length args)) (if reverse (reverse args) args))))
    (if negate (negate atom*) atom*)))

(defun arithmetic-term-rewriter1 (term subst pred operator)
  (let ((args (arithmetic-expr-args term subst pred)))
    (if (eq none args) none (declare-constant (apply operator args)))))

(defun arithmetic-term-rewriter2 (term subst pred operator)
  ;; like arithmetic-term-rewriter1 but last argument must be nonzero
  (let ((args (arithmetic-expr-args term subst pred)))
    (if (or (eq none args) (eql 0 (first (last args)))) none (declare-constant (apply operator args)))))

(defun arithmetic-term-rewriter3 (term subst operator identity absorber)
  ;; combines numerical arguments in sum and product terms
  (let* ((head (head term))
         (args (args term))
         (args* (argument-list-a1 head args subst identity)))
    (cond
     ((null args*)
      identity)
     ((null (rest args*))
      (first args*))
     (t
      (mvlet (((values nums nonnums) (split-if #'rnumberp args* subst)))
        (cond
         ((null nums)
          (if (eq args args*) none (make-compound* head args*)))
         (t
          (let ((num (if (null (rest nums)) (first nums) (declare-constant (apply operator nums)))))
            (cond
             ((eql absorber num)
              num)
             ((eql identity num)
              (make-a1-compound* head identity nonnums))
             ((and (eq args args*) (null (rest nums)) (let ((arg1 (first args))) (dereference arg1 subst :if-constant (eql num arg1))))
              none)
             (t
              (make-a1-compound* head identity num nonnums)))))))))))

(defun arithmetic-term-rewriter4 (term subst operator)
  ;; for floor, ceiling, truncate, and round
  (let ((arg (first (args term))))
    (cond
     ((dereference arg subst :if-constant (realp arg))
      (declare-constant (funcall operator arg)))
     ((subsort? (term-sort arg subst) (the-sort 'integer))
      arg)
     (t
      none))))

(defun arithmetic-term-rewriter5 (term subst op2 op1)
  ;; ($$difference a b) -> ($$sum a ($$uminus b))
  ;; ($$quotient a b) -> ($$product a ($$reciprocal b))
  (declare (ignorable subst))
  (mvlet (((list a b) (args term)))
    (make-compound (input-function-symbol op2 2) a (make-compound (input-function-symbol op1 1) b))))

(defun decompose-product-term (term subst)
  (if (dereference term subst :if-compound-appl t)
      (let ((head (heada term)))
        (if (eq *product* head)
            (mvlet* ((args (args term))
                     ((values nums nonnums) (split-if #'rnumberp (argument-list-a1 head args subst) subst)))
              (if (and nonnums nums (null (rest nums)) (not (eql 0 (first nums))))
                  (values (make-a1-compound* head 1 nonnums) (first nums))
                  (values term 1)))
            (values term 1)))
      (values term 1)))

(defun sum-term-rewriter1 (term subst)
  ;; collect equal arguments into products
  ;; ($$sum a a b a) -> ($$sum ($$product 3 a) b)
  ;; ($$sum ($$product 2 a b) ($$product b 3 a)) -> ($$product 5 a b))
  (let ((rewritten nil))
    (labels
      ((combine-terms (terms)
         (cond
          ((null (rest terms))
           terms)
          (t
           (mvlet (((values term1 mult1) (decompose-product-term (first terms) subst)))
             ;; combine terms in (rest terms) then find a match for term1 if there is one
             (mvlet* ((mult2 nil)
                      ((values matches nonmatches) (prog->
                                                     (split-if (combine-terms (rest terms)) subst ->* term2)
                                                     (unless mult2
                                                       (unless (rnumberp term2)	;don't combine numbers
                                                         (mvlet (((values term2 mult) (decompose-product-term term2 subst)))
                                                           (when (equal-p term1 term2 subst)
                                                             (setf mult2 mult))))))))
               (declare (ignorable matches))
               (cond
                (mult2
                 (setf rewritten t)
                 (let ((mult (declare-constant (+ mult1 mult2))))
                   (cond
                    ((eql 0 mult)
                     nonmatches)
                    ((eql 1 mult)
                     (cons term1 nonmatches))
                    ((dereference term1 subst :if-compound-appl (eq *product* (heada term1)))
                     (cons (make-compound* *product* mult (args term1)) nonmatches))
                    (t
                     (cons (make-compound *product* mult term1) nonmatches)))))
                ((eq (rest terms) nonmatches)
                 terms)
                (t
                 (cons (first terms) nonmatches)))))))))
      (let* ((head (head term))
             (args (argument-list-a1 head (args term) subst))
             (args* (combine-terms args)))
        (if rewritten (make-a1-compound* head 0 args*) none)))))

(defun uminus-term-rewriter (term subst)
  ;; ($$uminus a) -> ($$product -1 a)
  (declare (ignorable subst))
  (make-compound *product* -1 (first (args term))))

(defun arithmetic-relation-rewriter (atom subst)
  (mvlet (((list a b) (args atom)))
    (or (dereference2
         a b subst
         :if-constant*compound (and (rnumberp a)
                                    (let ((fn (head b)))
                                      (dolist (fun (function-arithmetic-relation-rewrite-code fn) nil)
                                        (let ((v (funcall fun atom subst)))
                                          (unless (eq none v)
                                            (pushnew (function-code-name fn) *rewrites-used*)
                                            (return v))))))
         :if-compound*constant (and (rnumberp b)
                                    (let ((fn (head a)))
                                      (dolist (fun (function-arithmetic-relation-rewrite-code fn) nil)
                                        (let ((v (funcall fun atom subst)))
                                          (unless (eq none v)
                                            (pushnew (function-code-name fn) *rewrites-used*)
                                            (return v)))))))
        none)))

(defun term-rel-term-to-0-rel-difference-atom-rewriter (atom subst)
  (mvlet ((rel (head atom))
          ((list a b) (args atom)))
    (cl:assert (eq *less* rel))
    (cond
     ((dereference2
       a b subst
       :if-variable*compound (variable-occurs-p a b subst)
       :if-compound*variable (variable-occurs-p b a subst)
       :if-constant*compound (and (not (rnumberp a)) (constant-occurs-p a b subst))
       :if-compound*constant (and (not (rnumberp b)) (constant-occurs-p b a subst))
       :if-compound*compound t)
      (pushnew (function-code-name *product*) *rewrites-used*)
      (pushnew (function-code-name *sum*) *rewrites-used*)
      (make-compound rel 0 (make-compound *sum* b (make-compound *product* -1 a))))
     (t
      none))))

(defun sum-rel-number-atom-rewriter (atom subst)
  ;; (eq (sum 2 c) 6) -> (eq c 4) and (less 6 (sum 2 c)) -> (less 4 c) etc.
  (mvlet ((rel (head atom))
          ((list a b) (args atom)))
    (cl:assert (or (eq *less* rel) (eq *=* rel)))
    (or (dereference
         a subst
         :if-constant (and (rnumberp a)
                           (dereference
                            b subst
                            :if-compound (and (eq *sum* (head b))
                                              (let* ((args (args b)) (arg1 (first args)))
                                                (and (rnumberp arg1)
                                                     (make-compound (head atom) (declare-constant (- a arg1)) (make-a1-compound* *sum* 0 (rest args))))))))
         :if-compound (and (eq *sum* (head a))
                           (dereference
                            b subst
                            :if-constant (and (rnumberp b)
                                              (let* ((args (args a)) (arg1 (first args)))
                                                (and (rnumberp arg1)
                                                     (make-compound (head atom) (make-a1-compound* *sum* 0 (rest args)) (declare-constant (- b arg1)))))))))
        none)))

(defun product-rel-number-atom-rewriter (atom subst)
  ;; like sum-rel-number-atom-rewriter, but don't divide by zero, and reverse arguments when dividing by negative number
  (mvlet ((rel (head atom))
          ((list a b) (args atom)))
    (cl:assert (or (eq *less* rel) (eq *=* rel)))
    (or (dereference
         a subst
         :if-constant (and (rnumberp a)
                           (dereference
                            b subst
                            :if-compound (and (eq *product* (head b))
                                              (let* ((args (args b)) (arg1 (first args)))
                                                (and (if (eq *less* rel) (nonzero-rationalp arg1) (nonzero-rnumberp arg1))
                                                     (if (and (eq *less* rel) (minusp arg1))
                                                         (make-compound (head atom) (make-a1-compound* *product* 0 (rest args)) (declare-constant (/ a arg1)))
                                                         (make-compound (head atom) (declare-constant (/ a arg1)) (make-a1-compound* *product* 0 (rest args)))))))))
         :if-compound (and (eq *product* (head a))
                           (dereference
                            b subst
                            :if-constant (and (rnumberp b)
                                              (let* ((args (args a)) (arg1 (first args)))
                                                (and (if (eq *less* rel) (nonzero-rationalp arg1) (nonzero-rnumberp arg1))
                                                     (if (and (eq *less* rel) (minusp arg1))
                                                         (make-compound (head atom) (declare-constant (/ b arg1)) (make-a1-compound* *product* 0 (rest args)))
                                                         (make-compound (head atom) (make-a1-compound* *product* 0 (rest args)) (declare-constant (/ b arg1))))))))))
        none)))

(defun reciprocal-rel-number-atom-rewriter (atom subst)
  (mvlet ((rel (head atom))
          ((list a b) (args atom)))
    (cl:assert (or (eq *less* rel) (eq *=* rel)))
    (cond
     ((eq *less* rel)
      none)
     (t
      (or (dereference
           a subst
           :if-constant (and (nonzero-rnumberp a)
                             (dereference
                              b subst
                              :if-compound (and (eq *reciprocal* (head b))
                                                (make-compound (head atom) (declare-constant (/ a)) (arg1 b)))))
           :if-compound (and (eq *reciprocal* (head a))
                             (dereference
                              b subst
                              :if-constant (and (nonzero-rnumberp b)
                                                (make-compound (head atom) (arg1 a) (declare-constant (/ b)))))))
          none)))))

(defmethod checkpoint-theory ((theory (eql 'arithmetic)))
  nil)

(defmethod uncheckpoint-theory ((theory (eql 'arithmetic)))
  nil)

(defmethod restore-theory ((theory (eql 'arithmetic)))
  nil)

(defmethod theory-closure ((theory (eql 'arithmetic)))
  nil)

(defmethod theory-assert (atom (theory (eql 'arithmetic)))
  (declare (ignorable atom))
  nil)

(defmethod theory-deny (atom (theory (eql 'arithmetic)))
  (declare (ignorable atom))
  nil)

(defmethod theory-simplify (wff (theory (eql 'arithmetic)))
  wff)

;;; code-for-numbers3.lisp EOF
