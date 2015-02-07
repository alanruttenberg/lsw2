;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: code-for-bags2.lisp
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

;;; ($$bag a b c)  -> ($$bag-cons a ($$bag-cons b ($$bag-cons c $$empty-bag)))
;;; ($$bag* a b c) -> ($$bag-cons a ($$bag-cons b c))

(defun declare-code-for-bags2 ()
  (declare-function1 '$$bag :any :macro t :input-code 'input-bag-term2)
  (declare-function1 '$$bag* :any :macro t :input-code 'input-bag*-term2)
  (setf *bag-cons* (declare-function1 '$$bag-cons 2
                                      :sort 'nonempty-bag
                                      :weight-code 'bag-weight
                                      :constructor t
                                      :injective nil
                                      :equality-rewrite-code 'rewrite-equal-bag-cons-atom
                                      :equal-code 'equal-bag-cons-p
                                      :variant-code 'variant-bag-cons
                                      :unify-code 'unify-bag-cons
                                      :to-lisp-code 'bag-cons-term-to-lisp
                                      :index-type :bag-cons))
  (declare-constant '$$empty-bag :locked t :sort 'empty-bag :constructor t)

  (declare-ordering-greaterp '$$bag-cons '$$empty-bag)

  (declare-characteristic-relation '$$bagp           #'bagp           'bag)
  (declare-characteristic-relation '$$empty-bag-p    #'empty-bag-p    'empty-bag)
  (declare-characteristic-relation '$$nonempty-bag-p #'nonempty-bag-p 'nonempty-bag)

  (declare-function1 '$$bag-to-list 1 :rewrite-code 'bag-to-list-term-rewriter :sort 'list)
  (declare-function1 '$$list-to-bag 1 :rewrite-code 'list-to-bag-term-rewriter :sort 'bag)
  )

(definline empty-bag-p (x)
  (eq '$$empty-bag x))

(defun nonempty-bag-p (x)
  (and (compound-appl-p x) (eq *bag-cons* (heada x))))

(defun bagp (x)
  (or (empty-bag-p x) (nonempty-bag-p x)))

(defun input-bag-term2 (head args polarity)
  (declare (ignore head))
  (labels
    ((ibt (args)
       (if (endp args)
           '$$empty-bag
           (list '$$bag-cons (first args) (ibt (rest args))))))
    (input-term1 (ibt args) polarity)))

(defun input-bag*-term2 (head args polarity)
  (require-n-or-more-arguments head args polarity 1)
  (labels
    ((ibt (args)
       (if (endp (rest args))
           (first args)
           (list '$$bag-cons (first args) (ibt (rest args))))))
    (input-term1 (ibt args) polarity)))

(defun bag-cons-term-to-lisp (head args subst)
  (declare (ignore head))
  (let* ((y (term-to-lisp (second args) subst))
         (x (term-to-lisp (first args) subst))
         (bag (function-name (find-symbol-table-entry '$$bag :function :any)))
         (bag* (function-name (find-symbol-table-entry '$$bag* :function :any))))
    (cond
     ((empty-bag-p y)
      (list bag x))
     ((and (consp y) (eq bag (first y)))
      (list* bag x (rest y)))
     ((and (consp y) (eq bag* (first y)))
      (list* bag* x (rest y)))
     (t
      (list bag* x y)))))

(defun equal-bag-cons-p (terms1 terms2 subst fn)
  ;; test if tails and lengths of bag-cons chains are equal
  (prog->
    (fn-chain-tail fn (second terms1) subst 1 -> tail1 len1)
    (fn-chain-tail fn (second terms2) subst 1 -> tail2 len2)
    (and (= len1 len2)
         (equal-p tail1 tail2 subst)
         (dolist (tc (bag-cons-terms-and-counts terms1 terms2 subst fn) t)
           (unless (= 0 (tc-count tc))
             (return nil))))))

(defun variant-bag-cons (cc terms1 terms2 subst matches fn)
  (prog->
    (fn-chain-tail fn (second terms1) subst 1 -> tail1 len1)
    (fn-chain-tail fn (second terms2) subst 1 -> tail2 len2)
    (when (= len1 len2)
      (variant tail1 tail2 subst matches ->* matches)
      (cond
       ((= 1 len1)
        (variant cc (first terms1) (first terms2) subst matches))
       (t
        (cons (first terms1) (fn-chain-items fn (second terms1) nil) -> l1)
        (cons (first terms2) (fn-chain-items fn (second terms2) nil) -> l2)
        (variant-bag cc l1 l2 subst matches nil))))))

(defun unify-bag-cons (cc terms1 terms2 subst fn)
  (prog->
    (fn-chain-tail fn (second terms1) subst 1 -> tail1 len1)
    (fn-chain-tail fn (second terms2) subst 1 -> tail2 len2)
    (unfrozen-variable-p tail1 -> var1)
    (unfrozen-variable-p tail2 -> var2)
    (cond
     ((and var1 var2)
      (if (eq tail1 tail2)
          (unify-bag-cons0 cc (bag-cons-terms-and-counts terms1 terms2 subst fn) subst)
          (let ((v (make-variable (the-sort 'bag))))
            (unify-bag-cons2 cc (bag-cons-terms-and-counts terms1 terms2 subst fn) subst fn tail1 tail2 v v))))
     (var1
      (when (<= len1 len2)
        (unify-bag-cons1 cc (bag-cons-terms-and-counts terms1 terms2 subst fn) subst fn tail1 tail2)))
     (var2
      (when (>= len1 len2)
        (unify-bag-cons1 cc (bag-cons-terms-and-counts terms2 terms1 subst fn) subst fn tail2 tail1)))
     (t
      (when (= len1 len2)
        (unify tail1 tail2 subst ->* subst)
        (unify-bag-cons0 cc (bag-cons-terms-and-counts terms1 terms2 subst fn) subst))))))

(defun bag-cons-terms-and-counts (terms1 terms2 subst fn)
  (let ((l1 (cons (first terms1) (fn-chain-items fn (second terms1) subst)))
        (l2 (cons (first terms2) (fn-chain-items fn (second terms2) subst))))
    (count-arguments nil l2 subst (count-arguments nil l1 subst) -1)))

(defun unify-bag-cons0 (cc terms-and-counts subst)
  (when (prog->
          (dolist terms-and-counts t ->* tc1)	;return t when there are no positive counts
          (when (< 0 (tc-count tc1))		;find a term with a positive count
            (prog->
              (dolist terms-and-counts ->* tc2)
              (when (> 0 (tc-count tc2))	;unify it with a term with a negative count
                (unify (tc-term tc1) (tc-term tc2) subst ->* subst)
                (unify-bag-cons0 cc (recount-arguments nil terms-and-counts subst) subst)))
            (return nil)))
    (funcall cc subst)))

(defun unify-bag-cons1 (cc terms-and-counts subst fn var1 tail2)
  (when (prog->
          (dolist terms-and-counts t ->* tc1)	;return t when there are no positive counts
          (when (< 0 (tc-count tc1))		;find a term with a positive count
            (prog->
              (dolist terms-and-counts ->* tc2)
              (when (> 0 (tc-count tc2))	;unify it with a term with a negative count
                (unify (tc-term tc1) (tc-term tc2) subst ->* subst)
                (unify-bag-cons1 cc (recount-arguments nil terms-and-counts subst) subst fn var1 tail2)))
            (return nil)))
    ;; construct value for var1 of unmatched terms (with negative counts)
    (prog->
      (dolist terms-and-counts ->* tc2)
      (when (> 0 (tc-count tc2))
        (dotimes (- (tc-count tc2)) ->* i)
        (setf tail2 (make-compound fn (tc-term tc2) tail2))))
    (unify cc var1 tail2 subst)))

(defun unify-bag-cons2 (cc terms-and-counts subst fn var1 var2 tail1 tail2)
  (when (prog->
          (dolist terms-and-counts t ->* tc1)	;return t when there are no positive counts
          (when (< 0 (tc-count tc1))		;find a term with a positive count
            (prog->
              (dolist terms-and-counts ->* tc2)
              (when (> 0 (tc-count tc2))	;unify it with a term with a negative count
                (unify (tc-term tc1) (tc-term tc2) subst ->* subst)
                (unify-bag-cons2 cc (recount-arguments nil terms-and-counts subst) subst fn var1 var2 tail1 tail2)))
            (prog->
              (dotimes (tc-count tc1) ->* i)
              (setf tail1 (make-compound fn (tc-term tc1) tail1)))
            (unify-bag-cons2 cc (recount-arguments nil (remove tc1 terms-and-counts) subst) subst fn var1 var2 tail1 tail2)
            (return nil)))
    ;; construct value for var1 of unmatched terms (with negative counts)
    (prog->
      (dolist terms-and-counts ->* tc2)
      (when (> 0 (tc-count tc2))
        (dotimes (- (tc-count tc2)) ->* i)
        (setf tail2 (make-compound fn (tc-term tc2) tail2))))
    (unify cc (cons var1 var2) (cons tail2 tail1) subst)))

(defun rewrite-equal-bag-cons-atom (atom subst)
  ;; rewrite (= (bag-cons ...) (bag-cons ...))
  (prog->
    (args atom -> args)
    (first args -> arg1)
    (second args -> arg2)
    (dereference arg1 subst)
    (dereference arg2 subst)
    (head arg1 -> fn)
;;  (cl:assert (eq fn (head arg2)))
    (fn-chain-tail fn arg1 subst -> tail1 len1)
    (fn-chain-tail fn arg2 subst -> tail2 len2)
    (when (equal-p tail1 tail2 subst)
      (if (= len1 len2)
          (setf tail1 (setf tail2 '$$empty-bag))
          (return-from rewrite-equal-bag-cons-atom false)))
    (bag-cons-terms-and-counts (args arg1) (args arg2) subst fn -> terms-and-counts cancel)
    (cond
     (cancel
      (let ((arg1* tail1)
            (arg2* tail2)
            (c1 0)
            (c2 0))
        (dolist (tc terms-and-counts)
          (cond
           ((< 0 (tc-count tc))
            (setf c1 (+ 1 c1))
            (dotimes (i (tc-count tc))
              (declare (ignorable i))
              (setf arg1* (make-compound fn (tc-term tc) arg1*))))
           ((> 0 (tc-count tc))
            (setf c2 (+ 1 c2))
            (dotimes (i (- (tc-count tc)))
              (declare (ignorable i))
              (setf arg2* (make-compound fn (tc-term tc) arg2*))))))
        ;; c1 and c2 are number of distinct elements in arg1* and arg2*
        (cond
         ((and (= 0 c1) (= 0 c2))
          (if (and (empty-bag-p tail1) (empty-bag-p tail2))
              true
              (make-compound *=* arg1* arg2*)))
         ((and (= 1 c1) (= 1 c2))
          (if (and (empty-bag-p tail1) (empty-bag-p tail2))
              (make-compound *=* (first (args arg1*)) (first (args arg2*)))
              (make-compound *=* arg1* arg2*)))
         (t
          (make-compound *=* arg1* arg2*)))))
     (t
      none))))

(defun bag-to-list (x subst &optional bag-to-list-symbol)
  (labels
    ((make-bag-to-list-term (x)
       (make-compound (or bag-to-list-symbol (find-symbol-table-entry '$$bag-to-list :function 1)) x))
     (convert (x)
       (dereference
        x subst
        :if-constant (if (empty-bag-p x) nil (make-bag-to-list-term x))
        :if-variable (make-bag-to-list-term x)
        :if-compound (if (eq *bag-cons* (head x))
                         (let ((args (args x)))
                           (cons (first args) (convert (second args))))
                         (make-bag-to-list-term x)))))
    (convert x)))

(defun list-to-bag (x subst &optional list-to-bag-symbol)
  (labels
    ((make-list-to-bag-term (x)
       (make-compound (or list-to-bag-symbol (find-symbol-table-entry '$$list-to-bag :function 1)) x))
     (convert (x)
       (dereference
        x subst
        :if-constant (if (null x) '$$empty-bag (make-list-to-bag-term x))
        :if-variable (make-list-to-bag-term x)
        :if-compound-cons (make-compound *bag-cons* (carc x) (convert (cdrc x)))
        :if-compound-appl (make-list-to-bag-term x))))
    (convert x)))

(defun bag-to-list-term-rewriter (term subst)
  ;; (assert-rewrite '(= ($$bag-to-list $$empty-bag) nil))
  ;; (assert-rewrite '(= ($$bag-to-list ($$bag-cons ?x ?y)) ($$cons ?x ($$bag-to-list ?y))))
  (let ((x (first (args term))))
    (dereference
     x subst
     :if-constant (if (empty-bag-p x) nil none)
     :if-variable none
     :if-compound (if (eq *bag-cons* (head x)) (bag-to-list x subst (head term)) none))))

(defun list-to-bag-term-rewriter (term subst)
  ;; (assert-rewrite '(= ($$list-to-bag nil) $$empty-bag))
  ;; (assert-rewrite '(= ($$list-to-bag ($$cons ?x ?y)) ($$bag-cons ?x ($$list-to-bag ?y))))
  (let ((x (first (args term))))
    (dereference
     x subst
     :if-constant (if (null x) '$$empty-bag none)
     :if-variable none
     :if-compound (if (eq *cons* (head x)) (list-to-bag x subst (head term)) none))))

(defun bag-weight (term &optional subst)
  (cond
   ((bag-weight-factorial?)
    (let ((n 0)
          (n! 1)
          (w 0))
      (loop
        (cond
         ((dereference term subst :if-compound-appl (eq *bag-cons* (heada term)))
          (setf n (+ 1 n))
          (setf n! (* n n!))
          (let ((args (argsa term)))
            (setf w (+ (weight (first args) subst) w))
            (setf term (second args))))
         (t
          (return (+ (* n (function-weight *bag-cons*))
                     (* n! w)
                     (weight term subst))))))))
   (t
    none)))

;;; code-for-bags2.lisp EOF
