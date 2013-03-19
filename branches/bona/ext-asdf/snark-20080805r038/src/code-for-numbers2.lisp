;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: code-for-numbers2.lisp
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

;;; SNARK can evaluate arithmetic expressions as if by table lookup
;;; for procedurally attached relations and functions
;;;
;;; what SNARK "knows" about numbers is limited by this notion of table lookup;
;;; few if any more general properties are known
;;; like (= (+ x 0) x), (= (* x 0) 0), (exists (x) (< x 0)),
;;; associativity and commutativity of + and *, etc.
;;;
;;; this is intended to provide simple arithmetic calculation and not much if any symbolic algebra
;;;
;;; SNARK numbers are represented by Lisp rational numbers (integers or ratios)
;;; and Lisp complex numbers with rational number real and imaginary parts
;;;
;;; floating-point numbers are replaced by rationals when input
;;;
;;; SNARK number type hierarchy: number = complex > real > rational > integer & ratio

(defun declare-code-for-numbers2 ()
  (declare-arithmetic-characteristic-relation '$$numberp   #'numberp   'number)
  (declare-arithmetic-characteristic-relation '$$complexp  #'numberp   'complex)	;all Lisp numbers are SNARK complex numbers
  (declare-arithmetic-characteristic-relation '$$realp     #'realp     'real)		;no floats though
  (declare-arithmetic-characteristic-relation '$$rationalp #'rationalp 'rational)
  (declare-arithmetic-characteristic-relation '$$integerp  #'integerp  'integer)
  (declare-arithmetic-characteristic-relation '$$naturalp  #'naturalp  'natural)
  
  (declare-arithmetic-relations2 '$$eq          2 'equality-rewriter :satisfy-code 'constructor-reflexivity-satisfier)
  (declare-arithmetic-relations1 '$$lesseq      2 (list 'reflexivity-rewriter   #'(lambda (atom subst) (arithmetic-atom-rewriter1 atom subst #'realp #'<=))) :satisfy-code 'reflexivity-satisfier)
  (declare-arithmetic-relations1 '$$greatereq   2 (list 'reflexivity-rewriter   #'(lambda (atom subst) (arithmetic-atom-rewriter1 atom subst #'realp #'>=))) :satisfy-code 'reflexivity-satisfier)
  (declare-arithmetic-relations1 '$$less        2 (list 'irreflexivity-rewriter #'(lambda (atom subst) (arithmetic-atom-rewriter1 atom subst #'realp #'<)))  :falsify-code 'irreflexivity-falsifier)
  (declare-arithmetic-relations1 '$$greater     2 (list 'irreflexivity-rewriter #'(lambda (atom subst) (arithmetic-atom-rewriter1 atom subst #'realp #'>)))  :falsify-code 'irreflexivity-falsifier)

  (declare-arithmetic-functions1 '$$sum      :any #'(lambda (term subst) (arithmetic-term-rewriter3 term subst #'numberp #'+ 0 none))
                                 :arithmetic-equality-rewrite-code 'sum-equals-number-atom-rewriter)

  (declare-arithmetic-functions1 '$$product  :any #'(lambda (term subst) (arithmetic-term-rewriter3 term subst #'numberp #'* 1 0))
                                 :arithmetic-equality-rewrite-code 'product-equals-number-atom-rewriter)

  (declare-arithmetic-functions1 '$$difference  2 #'(lambda (term subst) (arithmetic-term-rewriter1 term subst #'numberp #'-))
                                 :arithmetic-equality-rewrite-code 'difference-equals-number-atom-rewriter)

  (declare-arithmetic-functions1 '$$uminus      1 #'(lambda (term subst) (arithmetic-term-rewriter1 term subst #'numberp #'-))
                                 :arithmetic-equality-rewrite-code 'uminus-equals-number-atom-rewriter)

  (declare-arithmetic-functions2 '$$quotient    2 #'(lambda (term subst) (arithmetic-term-rewriter4 term subst #'numberp #'/))		;partial, guard against division by zero, not for integers
                                 :arithmetic-equality-rewrite-code 'quotient-equals-number-atom-rewriter)

  (declare-arithmetic-functions2 '$$reciprocal  1 #'(lambda (term subst) (arithmetic-term-rewriter4 term subst #'numberp #'/))		;partial, guard against division by zero, not for integers
                                 :arithmetic-equality-rewrite-code 'reciprocal-equals-number-atom-rewriter)

  (declare-arithmetic-functions3 '$$abs         1 #'(lambda (term subst) (arithmetic-term-rewriter1 term subst #'realp   #'abs)))	;abs of complex numbers might be irrational
  (declare-arithmetic-functions4 '$$realpart    1 #'(lambda (term subst) (arithmetic-term-rewriter1 term subst #'numberp #'realpart)))
  (declare-arithmetic-functions4 '$$imagpart    1 #'(lambda (term subst) (arithmetic-term-rewriter1 term subst #'numberp #'imagpart)))
  (declare-arithmetic-functions7 '$$floor       1 #'(lambda (term subst) (arithmetic-term-rewriter1 term subst #'realp   #'floor)))
  (declare-arithmetic-functions7 '$$ceiling     1 #'(lambda (term subst) (arithmetic-term-rewriter1 term subst #'realp   #'ceiling)))
  (declare-arithmetic-functions7 '$$truncate    1 #'(lambda (term subst) (arithmetic-term-rewriter1 term subst #'realp   #'truncate)))
  (declare-arithmetic-functions7 '$$round       1 #'(lambda (term subst) (arithmetic-term-rewriter1 term subst #'realp   #'round)))
  (let-options ((print-symbol-table-warnings nil))
    (declare-arithmetic-functions8 '$$floor     2 #'(lambda (term subst) (arithmetic-term-rewriter4 term subst #'realp   #'floor)))	;partial, guard against division by zero
    (declare-arithmetic-functions8 '$$ceiling   2 #'(lambda (term subst) (arithmetic-term-rewriter4 term subst #'realp   #'ceiling)))	;partial, guard against division by zero
    (declare-arithmetic-functions8 '$$truncate  2 #'(lambda (term subst) (arithmetic-term-rewriter4 term subst #'realp   #'truncate)))	;partial, guard against division by zero
    (declare-arithmetic-functions8 '$$round     2 #'(lambda (term subst) (arithmetic-term-rewriter4 term subst #'realp   #'round)))	;partial, guard against division by zero
    )
  (declare-arithmetic-functions9 '$$modulus     2 #'(lambda (term subst) (arithmetic-term-rewriter4 term subst #'realp   #'mod)))	;partial, guard against division by zero
  (declare-arithmetic-functions9 '$$remainder   2 #'(lambda (term subst) (arithmetic-term-rewriter4 term subst #'realp   #'rem)))	;partial, guard against division by zero
  )

(defun declare-arithmetic-relations1 (name arity rewrite-code &rest options)
  (apply 'declare-arithmetic-relations name arity rewrite-code
         '((nil nil) (real ((t real))) (rational ((t rational))) (integer ((t integer))))
         options))

(defun declare-arithmetic-relations2 (name arity rewrite-code &rest options)
  ;; don't declare name/2, only declare name_integer/2 etc. (to define $$eq_integer etc. but not $$eq that is defined elsewhere)
  (apply 'declare-arithmetic-relations name arity rewrite-code
         '((complex ((t complex))) (real ((t real))) (rational ((t rational))) (integer ((t integer))))
         options))

(defun declare-arithmetic-functions1 (name arity rewrite-code &rest options)
  (apply 'declare-arithmetic-functions name arity rewrite-code
         '((nil number) (complex (complex (t complex))) (real (real (t real))) (rational (rational (t rational))) (integer (integer (t integer))))
         options))

(defun declare-arithmetic-functions2 (name arity rewrite-code &rest options)
  ;; no integer version of quotient and reciprocal because result might not be an integer
  ;; use [quotient|reciprocal]_rational or [floor|ceiling|truncate|round]_integer depending on whether rational or integer result is required
  (apply 'declare-arithmetic-functions name arity rewrite-code
         '((nil number) (complex (complex (t complex))) (real (real (t real))) (rational (rational (t rational))))
         options))

(defun declare-arithmetic-functions3 (name arity rewrite-code &rest options)
  (apply 'declare-arithmetic-functions name arity rewrite-code
         '((nil real) (complex (real (t complex))) (real (real (t real))) (rational (rational (t rational))) (integer (integer (t integer))))
         options))

(defun declare-arithmetic-functions4 (name arity rewrite-code &rest options)
  (apply 'declare-arithmetic-functions name arity rewrite-code
         '((nil real) (complex (real (t complex))))
         options))

(defun declare-arithmetic-functions7 (name arity rewrite-code &rest options)
  (apply 'declare-arithmetic-functions name arity rewrite-code
         '((nil integer) (real (integer (t real))) (rational (integer (t rational))) (integer (integer (t integer))))
         options))

(defun declare-arithmetic-functions8 (name arity rewrite-code &rest options)
  (apply 'declare-arithmetic-functions name arity rewrite-code
         '((nil integer) (real (integer (t real))) (rational (integer (t rational))) (integer (integer (t integer))))
         options))

(defun declare-arithmetic-functions9 (name arity rewrite-code &rest options)
  (apply 'declare-arithmetic-functions name arity rewrite-code
         '((nil real) (real (real (t real))) (rational (rational (t rational))) (integer (integer (t integer))))
         options))

(defun declare-arithmetic-characteristic-relation (name pred sort &rest options)
  (apply 'declare-characteristic-relation name pred sort :constraint-theory 'arithmetic options))

(defun declare-arithmetic-relation (name arity rewrite-code &rest options)
  (apply 'declare-relation2 name arity :rewrite-code rewrite-code :constraint-theory 'arithmetic options))

(defun declare-arithmetic-function (name arity rewrite-code &rest options)
  (apply 'declare-function2 name arity :rewrite-code rewrite-code :constraint-theory 'arithmetic options))

(defun declare-arithmetic-relations (name arity rewrite-code suffixes-and-sorts &rest options)
  (dolist (ss suffixes-and-sorts)
    (let ((suffix (first ss))
          (sort (second ss)))
      (apply 'declare-arithmetic-relation (make-snark-symbol1 name suffix) arity rewrite-code :sort sort options))))

(defun declare-arithmetic-functions (name arity rewrite-code suffixes-and-sorts &rest options)
  (dolist (ss suffixes-and-sorts)
    (let ((suffix (first ss))
          (sort (second ss)))
      (apply 'declare-arithmetic-function (make-snark-symbol1 name suffix) arity rewrite-code :sort sort options))))

(defun make-snark-symbol1 (name type)
  (if type (intern (concatenate 'string (string name) "_" (string type)) :snark) name))

(defun arithmetic-atom-rewriter1 (atom subst pred fun)
  (let ((args nil))
    (dolist (arg (args atom) (if (apply fun (nreverse args)) true false))
      (if (dereference arg subst :if-constant (funcall pred arg))
          (push arg args)
          (return none)))))

(defun arithmetic-term-rewriter1 (term subst pred fun)
  ;; if all arguments satisfy pred return result of apply fun to them
  (let ((args nil))
    (dolist (arg (args term) (declare-constant (apply fun (nreverse args))))
      (if (dereference arg subst :if-constant (funcall pred arg))
          (push arg args)
          (return none)))))

(defun arithmetic-term-rewriter3 (term subst pred fun id kill)
  ;; this works whether sum and product are declared as 2-ary or as any-ary functions
  (let ((args (args term)))
    (cond
     ((null args)
      (declare-constant id))
     ((null (rest args))
      (first args))
     (t
      (let ((nums 0)
            (val nil))
        (declare (fixnum nums))
        (dolist (arg args)
          (when (dereference arg subst :if-constant (funcall pred arg))
            (if (= 0 nums)
                (setf val arg nums 1)
                (setf val (funcall fun val arg) nums (+ 1 nums)))))
        (cond
         ((= 0 nums)
          none)
         ((and (neq none kill) (= kill val))
          (declare-constant val))
         ((and (neq none id) (= id val))
          (let ((args2 nil))
            (dolist (arg args)
              (unless (dereference arg subst :if-constant (funcall pred arg))
                (push arg args2)))
            (cond
             ((null args2)
              (declare-constant val))
             ((null (rest args2))
              (first args2))
             (t
              (make-compound* (head term) (nreverse args2))))))
         ((and (= 1 nums) (let ((arg1 (first args))) (dereference arg1 subst :if-constant (eql val arg1))))
          none)
         (t
          (let ((args2 nil))
            (dolist (arg args)
              (unless (dereference arg subst :if-constant (funcall pred arg))
                (push arg args2)))
            (cond
             ((null args2)
              (declare-constant val))
             (t
              (make-compound* (head term) (declare-constant val) (nreverse args2))))))))))))

(defun arithmetic-term-rewriter4 (term subst pred fun)
  ;; like arithmetic-term-rewriter1 but last argument must be nonzero
  (let ((args nil))
    (dolist (arg (args term) (if (zerop (first args)) none (declare-constant (apply fun (nreverse args)))))
      (if (dereference arg subst :if-constant (funcall pred arg))
          (push arg args)
          (return none)))))

(defun uminus-equals-number-atom-rewriter (atom subst)
  (mvlet (((:list a b) (args atom)))
    (dereference a subst)
    (dereference b subst)
    (cond
     ((numberp a)
      (make-compound (head atom) (declare-constant (- a)) (first (args b))))
     ((numberp b)
      (make-compound (head atom) (first (args a)) (declare-constant (- b))))
     (t
      none))))

(defun reciprocal-equals-number-atom-rewriter (atom subst)
  (mvlet (((:list a b) (args atom)))
    (dereference a subst)
    (dereference b subst)
    (cond
     ((and (numberp a) (/= 0 a))
      (make-compound (head atom) (declare-constant (/ a)) (first (args b))))
     ((and (numberp b) (/= 0 b))
      (make-compound (head atom) (first (args a)) (declare-constant (/ b))))
     (t
      none))))

(defun difference-equals-number-atom-rewriter (atom subst)
  (mvlet (((:list a b) (args atom)))
    (dereference a subst)
    (dereference b subst)
    (cond
     ((numberp a)
      (mvlet (((:list u v) (args b)))	;(= a (- u v))
        (cond
         ((dereference v subst :if-constant (numberp v))
          (make-compound (head atom) (declare-constant (+ v a)) u))
         ((dereference u subst :if-constant (numberp u))
          (make-compound (head atom) (declare-constant (- u a)) v))
         (t
          none))))
     ((numberp b)
      (mvlet (((:list u v) (args a)))	;(= (- u v) b)
        (cond
         ((dereference v subst :if-constant (numberp v))
          (make-compound (head atom) u (declare-constant (+ v b))))
         ((dereference u subst :if-constant (numberp u))
          (make-compound (head atom) v (declare-constant (- u b))))
         (t
          none))))
     (t
      none))))

(defun quotient-equals-number-atom-rewriter (atom subst)
  (mvlet (((:list a b) (args atom)))
    (dereference a subst)
    (dereference b subst)
    (cond
     ((numberp a)
      (mvlet (((:list u v) (args b)))	;(= a (/ u v))
        (cond
         ((dereference v subst :if-constant (and (numberp v) (/= 0 v)))
          (make-compound (head atom) (declare-constant (* v a)) u))
         ((and (/= 0 a) (dereference u subst :if-constant (and (numberp u) (/= 0 u))))
          (make-compound (head atom) (declare-constant (/ u a)) v))
         (t
          none))))
     ((numberp b)
      (mvlet (((:list u v) (args a)))	;(= (/ u v) b)
        (cond
         ((dereference v subst :if-constant (and (numberp v) (/= 0 v)))
          (make-compound (head atom) u (declare-constant (* v b))))
         ((and (/= 0 b) (dereference u subst :if-constant (and (numberp u) (/= 0 u))))
          (make-compound (head atom) v (declare-constant (/ u b))))
         (t
          none))))
     (t
      none))))

(defun sum-equals-number-atom-rewriter (atom subst)
  (mvlet (((:list a b) (args atom)))
    (dereference a subst)
    (dereference b subst)
    (cond
     ((numberp a)
      (let* ((args (args b))
             (arg1 (first args)))
        (cond
         ((dereference arg1 subst :if-constant (numberp arg1))
          (make-compound
           (head atom)
           (declare-constant (- a arg1))
           (cond
            ((null (rest args))
             (declare-constant 0))
            ((null (rest (rest args)))
             (second args))
            (t
             (make-compound* (head b) (rest args))))))
         (t
          none))))
     ((numberp b)
      (let* ((args (args a))
             (arg1 (first args)))
        (cond
         ((dereference arg1 subst :if-constant (numberp arg1))
          (make-compound
           (head atom)
           (cond
            ((null (rest args))
             (declare-constant 0))
            ((null (rest (rest args)))
             (second args))
            (t
             (make-compound* (head a) (rest args))))
           (declare-constant (- b arg1))))
         (t
          none))))
     (t
      none))))

(defun product-equals-number-atom-rewriter (atom subst)
  (mvlet (((:list a b) (args atom)))
    (dereference a subst)
    (dereference b subst)
    (cond
     ((numberp a)
      (let* ((args (args b))
             (arg1 (first args)))
        (cond
         ((dereference arg1 subst :if-constant (and (numberp arg1) (/= 0 arg1)))
          (make-compound
           (head atom)
           (declare-constant (/ a arg1))
           (cond
            ((null (rest args))
             (declare-constant 1))
            ((null (rest (rest args)))
             (second args))
            (t
             (make-compound* (head b) (rest args))))))
         (t
          none))))
     ((numberp b)
      (let* ((args (args a))
             (arg1 (first args)))
        (cond
         ((dereference arg1 subst :if-constant (and (numberp arg1) (/= 0 arg1)))
          (make-compound
           (head atom)
           (cond
            ((null (rest args))
             (declare-constant 1))
            ((null (rest (rest args)))
             (second args))
            (t
             (make-compound* (head a) (rest args))))
           (declare-constant (/ b arg1))))
         (t
          none))))
     (t
      none))))

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

;;; code-for-numbers2.lisp EOF
