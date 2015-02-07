;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: jepd-relations.lisp
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

;;; reasoning facilities for jointly-exhaustive and pairwise-disjoint sets of binary relations
;;; including
;;;  spatial regions (RCC8)
;;;  time intervals (Allen)
;;;  time points
;;; that use composition tables to derive consequences and determine local consistency

;;; for theories implemented here, the main functions are
;;;  declare-rcc8-relations
;;;  declare-time-relations
;;; these declare the appropriate relation symbols
;;; (determined by the values of rcc8-jepd-relation-names, rcc8-more-relation-names, etc.)
;;; and declare procedural attachments for composing and intersecting disjunctions of
;;; jepd binary relations

;;; in the following encodings,
;;;  a primitive relation allowed to be true is signified by the constant 1
;;;  a primitive relation required to be false is signified by a variable
;;; encoding "no" by variables this way makes factoring and subsumption do the right thing

;;; for example, here is the encoding of time interval-interval relations
;;; they are all translated to positive occurrences of time-ii-relation
;;;   0 (before a b)            ($$time-ii a b (list 1 ? ? ? ? ? ? ? ? ? ? ? ?))
;;;   1 (during a b)            ($$time-ii a b (list ? 1 ? ? ? ? ? ? ? ? ? ? ?))
;;;   2 (overlaps a b)          ($$time-ii a b (list ? ? 1 ? ? ? ? ? ? ? ? ? ?))
;;;   3 (meets a b)             ($$time-ii a b (list ? ? ? 1 ? ? ? ? ? ? ? ? ?))
;;;   4 (starts a b)            ($$time-ii a b (list ? ? ? ? 1 ? ? ? ? ? ? ? ?))
;;;   5 (finishes a b)          ($$time-ii a b (list ? ? ? ? ? 1 ? ? ? ? ? ? ?))
;;;   6 (equal a b)             ($$time-ii a b (list ? ? ? ? ? ? 1 ? ? ? ? ? ?))
;;;   7 (finished-by a b)       ($$time-ii a b (list ? ? ? ? ? ? ? 1 ? ? ? ? ?))
;;;   8 (started-by a b)        ($$time-ii a b (list ? ? ? ? ? ? ? ? 1 ? ? ? ?))
;;;   9 (met-by a b)            ($$time-ii a b (list ? ? ? ? ? ? ? ? ? 1 ? ? ?))
;;;  10 (overlapped-by a b)     ($$time-ii a b (list ? ? ? ? ? ? ? ? ? ? 1 ? ?))
;;;  11 (contains a b)          ($$time-ii a b (list ? ? ? ? ? ? ? ? ? ? ? 1 ?))
;;;  12 (after a b)             ($$time-ii a b (list ? ? ? ? ? ? ? ? ? ? ? ? 1))
;;;     (disjoint a b)          ($$time-ii a b (list 1 ? ? 1 ? ? ? ? ? 1 ? ? 1))
;;;     (not (before a b))      ($$time-ii a b (list ? 1 1 1 1 1 1 1 1 1 1 1 1))
;;;     (not (during a b))      ($$time-ii a b (list 1 ? 1 1 1 1 1 1 1 1 1 1 1))
;;;     etc.

;;; these SNARK options can be used to specify the sort and relation names to be used
;;; by setting them BEFORE executing (declare-rcc8-relations) or (declare-time-relations)

(declare-snark-option rcc8-region-sort-name 'region 'region)
(declare-snark-option time-interval-sort-name 'time-interval 'time-interval)
(declare-snark-option time-point-sort-name 'time-point 'time-point)

(defparameter rcc8-jepd-relation-names
  '($$rcc8-tpp					;0   tangential proper part - inverse of 7
    $$rcc8-ntpp					;1   nontangential proper part - inverse of 6
    $$rcc8-dc					;2   disconnected - self inverse
    $$rcc8-ec					;3   externally connected - self inverse
    $$rcc8-po					;4   partially overlaps - self inverse
    $$rcc8-eq					;5   equality - self inverse
    $$rcc8-ntppi				;6   nontangential proper part inverse
    $$rcc8-tppi))				;7   tangential proper part inverse

(defparameter rcc8-more-relation-names		;composite relations and aliases
  '($$rcc8-dr (2 3)				;    discrete (complement of overlaps)
    $$rcc8-pp (0 1)				;    proper part
    $$rcc8-p (0 1 5)				;    part
    $$rcc8-ppi (6 7)				;    proper part inverse
    $$rcc8-pi (5 6 7)				;    part inverse
    $$rcc8-o (0 1 4 5 6 7)			;    overlaps (complement of discrete)
    $$rcc8-c (0 1 3 4 5 6 7)			;    connected (complement of disconnected)
    $$rcc8-tp (0 5)				;    tangential part
    $$rcc8-tpi (5 7)				;    tangential part inverse
    
    ;; rcc8-not-tpp etc. are unnecessary for input
    ;; since (not (rcc8-tpp ...)) etc. can be written instead
    ;; they are used to improve output using only positive literals
    $$rcc8-not-tpp (1 2 3 4 5 6 7)
    $$rcc8-not-ntpp (0 2 3 4 5 6 7)
    $$rcc8-not-ec (0 1 2 4 5 6 7)
    $$rcc8-not-po (0 1 2 3 5 6 7)
    $$rcc8-not-eq (0 1 2 3 4 6 7)
    $$rcc8-not-ntppi (0 1 2 3 4 5 7)
    $$rcc8-not-tppi (0 1 2 3 4 5 6)
    $$rcc8-not-pp (2 3 4 5 6 7)
    $$rcc8-not-p (2 3 4 6 7)
    $$rcc8-not-ppi (0 1 2 3 4 5)
    $$rcc8-not-pi (0 1 2 3 4)
    $$rcc8-not-tp (1 2 3 4 6 7)
    $$rcc8-not-tpi (0 1 2 3 4 6)
    ))

(defparameter time-ii-jepd-relation-names
  '($$time-ii-before				;0 - inverse of 12
    $$time-ii-during				;1 - inverse of 11
    $$time-ii-overlaps				;2 - inverse of 10
    $$time-ii-meets				;3 - inverse of 9
    $$time-ii-starts				;4 - inverse of 8
    $$time-ii-finishes				;5 - inverse of 7
    $$time-ii-equal				;6 - self inverse
    $$time-ii-finished-by			;7
    $$time-ii-started-by			;8
    $$time-ii-met-by				;9
    $$time-ii-overlapped-by			;10
    $$time-ii-contains				;11
    $$time-ii-after))				;12

(defparameter time-ii-more-relation-names	;composite relations and aliases
  '($$time-ii-starts-before (0 2 3 7 11)
    $$time-ii-starts-equal (4 6 8)
    $$time-ii-starts-after (1 5 9 10 12)
    $$time-ii-finishes-before (0 1 2 3 4)
    $$time-ii-finishes-equal (5 6 7)
    $$time-ii-finishes-after (8 9 10 11 12)
    $$time-ii-subsumes (6 7 8 11)
    $$time-ii-subsumed-by (1 4 5 6)
    $$time-ii-disjoint (0 3 9 12)
    $$time-ii-intersects (1 2 4 5 6 7 8 10 11)	;complement of disjoint
    
    ;; time-ii-not-before etc. are unnecessary for input
    ;; since (not (before ...)) etc. can be written instead
    ;; they are used to improve output using only positive literals
    $$time-ii-not-before (1 2 3 4 5 6 7 8 9 10 11 12)
    $$time-ii-not-during (0 2 3 4 5 6 7 8 9 10 11 12)
    $$time-ii-not-overlaps (0 1 3 4 5 6 7 8 9 10 11 12)
    $$time-ii-not-meets (0 1 2 4 5 6 7 8 9 10 11 12)
    $$time-ii-not-starts (0 1 2 3 5 6 7 8 9 10 11 12)
    $$time-ii-not-finishes (0 1 2 3 4 6 7 8 9 10 11 12)
    $$time-ii-not-equal (0 1 2 3 4 5 7 8 9 10 11 12)
    $$time-ii-not-finished-by (0 1 2 3 4 5 6 8 9 10 11 12)
    $$time-ii-not-started-by (0 1 2 3 4 5 6 7 9 10 11 12)
    $$time-ii-not-met-by (0 1 2 3 4 5 6 7 8 10 11 12)
    $$time-ii-not-overlapped-by (0 1 2 3 4 5 6 7 8 9 11 12)
    $$time-ii-not-contains (0 1 2 3 4 5 6 7 8 9 10 12)
    $$time-ii-not-after (0 1 2 3 4 5 6 7 8 9 10 11)
    $$time-ii-not-starts-before (1 4 5 6 8 9 10 12)
    $$time-ii-not-starts-equal (0 1 2 3 5 7 9 10 11 12)
    $$time-ii-not-starts-after (0 2 3 4 6 7 8 11)
    $$time-ii-not-finishes-before (5 6 7 8 9 10 11 12)
    $$time-ii-not-finishes-equal (0 1 2 3 4 8 9 10 11 12)
    $$time-ii-not-finishes-after (0 1 2 3 4 5 7 7)
    $$time-ii-not-subsumes (0 1 2 3 4 5 9 10 12)
    $$time-ii-not-subsumed-by (0 2 3 7 8 9 10 11 12)
    
    $$time-ii-contained-by (1)			;alias of time-ii-during
    ))

(defparameter time-pp-jepd-relation-names
  '($$time-pp-before				;0 - inverse of 2
    $$time-pp-equal				;1 - self inverse
    $$time-pp-after))				;2

(defparameter time-pp-more-relation-names	;composite relations and aliases
  '($$time-pp-not-before (1 2)
    $$time-pp-not-equal (0 2)
    $$time-pp-not-after (0 1)
    ))

(defparameter time-pi-jepd-relation-names
  '($$time-pi-before				;0
    $$time-pi-starts				;1
    $$time-pi-during				;2
    $$time-pi-finishes				;3
    $$time-pi-after))				;4

(defparameter time-pi-more-relation-names	;composite relations and aliases
  '($$time-pi-disjoint (0 4)
    $$time-pi-intersects (1 2 3)		;complement of disjoint
    $$time-pi-not-before (1 2 3 4)
    $$time-pi-not-starts (0 2 3 4)
    $$time-pi-not-during (0 1 3 4)
    $$time-pi-not-finishes (0 1 2 4)
    $$time-pi-not-after (0 1 2 3)
    $$time-pi-contained-by (2)			;alias of time-pi-during
    ))

;;; interval-point relations are converted to point-interval relations

(defparameter time-ip-jepd-relation-names
  '($$time-ip-after				;0
    $$time-ip-started-by			;1
    $$time-ip-contains				;2
    $$time-ip-finished-by			;3
    $$time-ip-before))				;4

(defparameter time-ip-more-relation-names	;composite relations and aliases
  '($$time-ip-disjoint (0 4)
    $$time-ip-intersects (1 2 3)		;complement of disjoint
    $$time-ip-not-after (1 2 3 4)
    $$time-ip-not-started-by (0 2 3 4)
    $$time-ip-not-contains (0 1 3 4)
    $$time-ip-not-finished-by (0 1 2 4)
    $$time-ip-not-before (0 1 2 3)
    ))

(defun jepd-relation-input-function (head args polarity rel reverse n i)
  (cond
   ((eq :both polarity)
    (throw 'needs-strict-polarity nil))
   (t
    (require-n-arguments head args polarity 2)
    (let ((atom `(,rel ,@(if reverse (reverse args) args) ($$list ,@(1-or-?s n i polarity)))))
      (input-wff1 (if (eq :pos polarity) atom `(not ,atom)) polarity)))))

(defun 1-or-?s (n i &optional (polarity :pos))
  (let ((l nil) l-last)
    (dotimes (k n)
      (collect (if (if (consp i) (member k i) (eql i k))
                   (if (eq :pos polarity) 1 (make-variable))
                   (if (eq :pos polarity) (make-variable) 1))
        l))
    l))

(defun 1s-count (x &optional subst)
  (dereference
   x subst
   :if-variable 0
   :if-constant 0
   :if-compound-appl 0
   :if-compound-cons (let ((x1 (carc x)))
                       (if (dereference x1 subst :if-constant (eql 1 x1))
                           (+ (1s-count (cdrc x)) 1)
                           (1s-count (cdrc x))))))

(defun 1-indexes (x &optional subst (n 0))
  (dereference
   x subst
   :if-variable nil
   :if-constant nil
   :if-compound-appl nil
   :if-compound-cons (let ((x1 (carc x)))
                       (if (dereference x1 subst :if-constant (eql 1 x1))
                           (cons n (1-indexes (cdrc x) subst (+ n 1)))
                           (1-indexes (cdrc x) subst (+ n 1))))))

(defun jepd-relation-composition-rewriter (atom subst fun)
  (let* ((args (args atom))
         (l1 (pop args))
         (l2 (pop args))
         (x (pop args))
         (y (pop args))
         (z (first args)))
    (cond
     ((or (equal-p x y subst)			;don't compose (r1 a a) and (r2 a b)
          (equal-p y z subst)			;don't compose (r1 a b) and (r2 b b)
          (and (test-option17?)
               (equal-p x z subst)))		;don't compose (r1 a b) and (r2 b a)
      true)
     ((and (dereference l1 subst :if-compound-cons t)
           (dereference l2 subst :if-compound-cons t))
      (funcall fun l1 l2 x y z subst))		;get result using theory's composition table
     (t
      none))))					;useless consequences of the axioms?

(defun jepd-relation-composition-rewriter1 (atom subst rel table &optional (n (first (array-dimensions table))))
  (jepd-relation-composition-rewriter
   atom
   subst
   (lambda (l1 l2 x y z subst)
     (declare (ignore y))
     (let ((result (make-array n :initial-element nil))
           (i 0))
       (dolist (v l1)
         (when (dereference v subst :if-constant t)
           (let ((j 0))
             (dolist (v l2)
               (when (dereference v subst :if-constant t)
                 (dolist (v (aref table i j))
                   (setf (svref result v) t)))
               (incf j))))
         (incf i))
       (cond
        ((every #'identity result)
         true)
        (t
         (make-compound
          rel
          x
          z
          (let ((l nil) l-last)
            (dotimes (i n)
              (collect (if (svref result i) 1 (make-and-freeze-variable)) l))
            l))))))))

(defun reversem (l m &optional (n (length l)))
  (nconc (nreverse (subseq l (- n m) n))
         (subseq l m (- n m))
         (nreverse (subseq l 0 m))))

(defun xx-intersection (l1 l2 subst)
  ;; fresh variables returned
  (dereference l1 subst)
  (dereference l2 subst)
  (if (null l1)
      nil
      (cons (or (let ((x (first l1))) (dereference x subst :if-variable (make-and-freeze-variable)))
                (let ((x (first l2))) (dereference x subst :if-variable (make-and-freeze-variable)))
                1)
            (xx-intersection (rest l1) (rest l2) subst))))

(defun jepd-relation-intersection-rewriter1 (rel atom subst invert)
  (let* ((args (args atom))
         (l1 (pop args))
         (l2 (pop args)))
    (cond
     ((and (dereference l1 subst :if-compound-cons t)
           (dereference l2 subst :if-compound-cons t))
      (let ((l (xx-intersection l1 l2 subst)))
        (cond
         ((not (member 1 l))
          false)
         ((and invert (test-option17?))
          (make-compound rel (second args) (first args) (reversem l invert)))
         (t
          (make-compound rel (first args) (second args) l)))))
     ((and (dereference l1 subst :if-variable t)
           (dereference l2 subst :if-variable t)
           (eq l1 l2))
      true)				;useless consequences of the axioms?
     (t
      none))))

(defun jepd-relation-atom-weight (x &optional subst)
  (let ((args (args x)))
    (+ (weight (pop args) subst)
       (weight (pop args) subst)
       (1s-count (first args) subst)
       (function-weight (head x)))))

(defun declare-jepd-relation (relname sort names more-names invert)
  (let ((use-special-unification (and invert (not (test-option17?)))))
    (declare-relation1
     relname 3
     :rewrite-code 'jepd-relation-atom-rewriter
     :sort sort
     :equal-code (and use-special-unification
                      (lambda (x y subst)
                        (equal-jepd-relation-atom-args-p (args x) (args y) subst invert)))
     :variant-code (and use-special-unification
                        (lambda (cc x y subst matches)
                          (variant-jepd-relation-atom-args cc (args x) (args y) subst matches invert)))
     :unify-code (and use-special-unification
                      (lambda (cc x y subst)
                        (unify-jepd-relation-atom-args cc (args x) (args y) subst invert)))
     :index-type (and use-special-unification :jepd)
     :ordering-status (if use-special-unification :commutative :left-to-right)
     :to-lisp-code #'(lambda (head args subst) (jepd-atom-to-lisp head args subst names more-names))
     :weight-code 'jepd-relation-atom-weight)))

(defun declare-jepd-relation-input (relname names more-names n reverse)
  (let ((i 0))
    (dolist (name names)
      (declare-relation1
       name :any
       :macro t
       :input-code (let ((i i))
                     (lambda (head args polarity)
                       (jepd-relation-input-function head args polarity relname reverse n i))))
      (incf i)))
  (do ((l more-names (cddr l)))
      ((endp l)
       )
    (declare-relation1
     (first l) :any
     :macro t
     :input-code (let ((i (second l)))
                   (lambda (head args polarity)
                     (jepd-relation-input-function head args polarity relname reverse n i))))))

(defun declare-equality-jepd-relation (relname sort n equality)
  (when equality
    (cl:assert (same-sort? (first sort) (second sort)))
    (assert `(forall ((?x :sort ,(first sort)))
                     (,relname ?x ?x ($$list ,@(1-or-?s n equality))))
            :name (intern (to-string relname :-equality) :keyword)
            :supported nil)))

(defun declare-jepd-relation-intersection (relname rel sort invert)
  (let ((intersection (intern (to-string relname :-intersection) :snark)))
    (declare-relation1
     intersection 4
     :rewrite-code (list
                    (lambda (atom subst)
                      (jepd-relation-intersection-rewriter1 rel atom subst invert))))
    (assert `(forall ((?x :sort ,(first sort))
                      (?y :sort ,(second sort))
                      ?l1
                      ?l2)
                     (implies (and (,relname ?x ?y ?l1) (,relname ?x ?y ?l2))
                              (,intersection ?l1 ?l2 ?x ?y)))
            :name (intern (symbol-name intersection) :keyword)
            :supported nil)))

(defun declare-jepd-relations (relname sort composition invert equality names more-names)
  ;; three operations may be necessary:
  ;;  intersection: (r1 a b) & (r2 a b) -> (r1&r2 a b)
  ;;  inverse: (r1 a b) -> (r1' b a)              
  ;;  composition: (r1 a b) & (r2 b c) -> (r3 a c)
  ;;
  ;; if inverse is necessary, it is incorporated into the intersection operation:
  ;;  intersection: (r1 a b) & (r2 a b) -> (r1&r2 b a)
  ;; so that only composition and (possibly inverting) intersection are used
  (let ((n (length names))
        (rel (declare-jepd-relation relname sort names more-names invert)))
    (declare-jepd-relation-input relname names more-names n nil)
    (declare-equality-jepd-relation relname sort n equality)
    (declare-jepd-relation-intersection relname rel sort invert)
    (let ((table composition)
          (composition (intern (to-string relname :-composition) :snark)))
      (declare-relation1
       composition 5
       :rewrite-code (list
                      (lambda (atom subst)
                        (jepd-relation-composition-rewriter1 atom subst rel table))))
      (assert `(forall ((?x :sort ,(first sort))
                        (?y :sort ,(second sort))	;sorts should be the same
                        (?z :sort ,(second sort))
                        ?l1
                        ?l2)
                       (implies (and (,relname ?x ?y ?l1) (,relname ?y ?z ?l2))
                                (,composition ?l1 ?l2 ?x ?y ?z)))
              :name (intern (symbol-name composition) :keyword)
              :supported nil))))

(defun jepd-relation-code (x alist)
  (let ((v (assoc x alist)))
    (cl:assert v)
    (cdr v)))

(defun make-composition-table (tab ocode &optional (icode1 ocode) (icode2 ocode))
  (let* ((nrows (length icode1))
         (ncols (length icode2))
         (table (make-array (list nrows ncols) :initial-element nil)))
    (dolist (x tab)
      (let ((i (jepd-relation-code (first x) icode1))
            (j (jepd-relation-code (second x) icode2)))
        (cl:assert (null (aref table i j)))
        (setf (aref table i j) (mapcar (lambda (x) (jepd-relation-code x ocode)) (cddr x)))))
    (dotimes (i nrows)
      (dotimes (j ncols)
        (cl:assert (not (null (aref table i j))))))
    table))

(defvar *rcc8-composition-table* nil)
(defvar *time-iii-composition-table* nil)
(defvar *time-ipi-composition-table* nil)
(defvar *time-pii-composition-table* nil)
(defvar *time-pip-composition-table* nil)
(defvar *time-ppi-composition-table* nil)
(defvar *time-ppp-composition-table* nil)

(defun firsta (x)
  (if (consp x) (first x) x))

(defun resta (x)
  (if (consp x) (rest x) nil))

(defun declare-rcc8-relations ()
  ;; this function should not be done more than once after (initialize)
  (let ((region-sort (rcc8-region-sort-name?)))
    (unless (sort-name? region-sort)
      (let ((l (resta region-sort)))
        (apply 'declare-sort (setf region-sort (firsta region-sort)) l)))
    (declare-jepd-relations
     '$$rcc8
     (list region-sort region-sort)
     (or *rcc8-composition-table*
         (setf *rcc8-composition-table* (make-composition-table
                                         $rcc8-composition-table
                                         $rcc8-relation-code)))
     2
     (jepd-relation-code 'eq $rcc8-relation-code)
     rcc8-jepd-relation-names
     rcc8-more-relation-names)))

(defun declare-time-relations (&key intervals points dates)
  ;; this function should not be done more than once after (initialize)
  (unless (or intervals points)
    (setf intervals t points t))
  (when dates
    (setf points t))
  (let ((interval-sort (time-interval-sort-name?))
        (point-sort (time-point-sort-name?)))
    (when intervals
      (unless (sort-name? interval-sort)
        (let ((l (resta interval-sort)))
          (apply 'declare-sort (setf interval-sort (firsta interval-sort)) l)))
      (declare-jepd-relations
       '$$time-ii
       (list interval-sort interval-sort)
       (or *time-iii-composition-table*
           (setf *time-iii-composition-table* (make-composition-table
                                               $time-iii-composition-table
                                               $time-ii-relation-code)))
       6
       (jepd-relation-code '= $time-ii-relation-code)
       time-ii-jepd-relation-names
       time-ii-more-relation-names))
    (when points
      (unless (sort-name? point-sort)
        (let ((l (resta point-sort)))
          (apply 'declare-sort (setf point-sort (firsta point-sort)) l)))
      (declare-jepd-relations
       '$$time-pp
       (list point-sort point-sort)
       (or *time-ppp-composition-table*
           (setf *time-ppp-composition-table* (make-composition-table
                                               $time-ppp-composition-table
                                               $time-pp-relation-code)))
       1
       (jepd-relation-code 'p=p $time-pp-relation-code)
       time-pp-jepd-relation-names
       time-pp-more-relation-names))
    (when (and intervals points)
      (unless (or (top-sort-name? interval-sort) (top-sort-name? point-sort))
        (declare-sorts-incompatible interval-sort point-sort))
      (let* ((relname '$$time-pi)
             (sort (list point-sort interval-sort))
             (names time-pi-jepd-relation-names)
             (more-names time-pi-more-relation-names)
             (n (length names))
             (rel (declare-jepd-relation relname sort names more-names nil)))
        (declare-jepd-relation-input relname names more-names n nil)
        ;; convert interval-point relations to point-interval relations
        (setf names time-ip-jepd-relation-names)
        (cl:assert (eql n (length names)))
        (declare-jepd-relation-input relname names time-ip-more-relation-names n t)
        (declare-jepd-relation-intersection relname rel sort nil)
        ;;; PI * II -> PI composition
        (let ((composition (intern (to-string relname :-ii-composition) :snark)))
          (declare-relation1
           composition 5
           :rewrite-code (let ((table (or *time-pii-composition-table*
                                          (setf *time-pii-composition-table* (make-composition-table
                                                                              $time-pii-composition-table
                                                                              $time-pi-relation-code
                                                                              $time-pi-relation-code
                                                                              $time-ii-relation-code))))
                               (n (length $time-pi-relation-code)))
                           (list
                            (lambda (atom subst)
                              (jepd-relation-composition-rewriter1 atom subst rel table n)))))
          (assert `(forall ((?x :sort ,point-sort)
                            (?y :sort ,interval-sort)
                            (?z :sort ,interval-sort)
                            ?l1
                            ?l2)
                           (implies (and (,relname ?x ?y ?l1) ($$time-ii ?y ?z ?l2))
                                    (,composition ?l1 ?l2 ?x ?y ?z)))
                  :name (intern (symbol-name composition) :keyword)
                  :supported nil))
        ;;; PP * PI -> PI composition
        (let ((composition (intern (to-string relname :-pp-composition) :snark)))
          (declare-relation1
           composition 5
           :rewrite-code (let ((table (or *time-ppi-composition-table*
                                          (setf *time-ppi-composition-table* (make-composition-table
                                                                              $time-ppi-composition-table
                                                                              $time-pi-relation-code
                                                                              $time-pp-relation-code
                                                                              $time-pi-relation-code))))
                               (n (length $time-pi-relation-code)))
                           (list
                            (lambda (atom subst)
                              (jepd-relation-composition-rewriter1 atom subst rel table n)))))
          (assert `(forall ((?x :sort ,point-sort)
                            (?y :sort ,point-sort)
                            (?z :sort ,interval-sort)
                            ?l1
                            ?l2)
                           (implies (and ($$time-pp ?x ?y ?l1) (,relname ?y ?z ?l2))
                                    (,composition ?l1 ?l2 ?x ?y ?z)))
                  :name (intern (symbol-name composition) :keyword)
                  :supported nil))
        ;;; PI * IP -> PP composition
        (let ((composition (intern (to-string relname :-pi-composition) :snark)))
          (declare-relation1
           composition 5
           :rewrite-code (let ((rel (input-relation-symbol '$$time-pp 3))
                               (table (or *time-pip-composition-table*
                                          (setf *time-pip-composition-table* (make-composition-table
                                                                              $time-pip-composition-table
                                                                              $time-pp-relation-code
                                                                              $time-pi-relation-code
                                                                              $time-ip-relation-code))))
                               (n (length $time-pp-relation-code)))
                           (list
                            (lambda (atom subst)
                              (jepd-relation-composition-rewriter1 atom subst rel table n)))))
          (assert `(forall ((?x :sort ,point-sort)
                            (?y :sort ,interval-sort)
                            (?z :sort ,point-sort)
                            ?l1
                            ?l2)
                           (implies (and (,relname ?x ?y ?l1) (,relname ?z ?y ?l2))
                                    (,composition ?l1 ?l2 ?x ?y ?z)))
                  :name (intern (symbol-name composition) :keyword)
                  :supported nil))
        ;;; IP * PI -> II composition
        (let ((composition (intern (to-string relname :-pi-composition2) :snark)))
          (declare-relation1
           composition 5
           :rewrite-code (let ((rel (input-relation-symbol '$$time-ii 3))
                               (table (or *time-ipi-composition-table*
                                          (setf *time-ipi-composition-table* (make-composition-table
                                                                              $time-ipi-composition-table
                                                                              $time-ii-relation-code
                                                                              $time-ip-relation-code
                                                                              $time-pi-relation-code))))
                               (n (length $time-ii-relation-code)))
                           (list
                            (lambda (atom subst)
                              (jepd-relation-composition-rewriter1 atom subst rel table n)))))
          (assert `(forall ((?x :sort ,interval-sort)
                            (?y :sort ,point-sort)
                            (?z :sort ,interval-sort)
                            ?l1
                            ?l2)
                           (implies (and (,relname ?y ?x ?l1) (,relname ?y ?z ?l2))
                                    (,composition ?l1 ?l2 ?x ?y ?z)))
                  :name (intern (symbol-name composition) :keyword)
                  :supported nil))))
    (when dates
      (declare-date-functions :intervals intervals :points points))
    nil))

(defun jepd-atom-to-lisp (head args subst &optional names more-names)
  (let* ((arg1 (term-to-lisp (pop args) subst))
         (arg2 (term-to-lisp (pop args) subst))
         (arg3 (first args))
         (rels (and names (1-indexes arg3 subst))))
    (cond
     ((null rels)
      (list (function-name head) arg1 arg2 (term-to-lisp arg3 subst)))
     ((null (rest rels))
      (list (function-name (input-relation-symbol (nth (first rels) names) 2)) arg1 arg2))
     ((do ((l more-names (cddr l)))
          ((null l)
           nil)
        (when (equal rels (second l))
          (return (list (function-name (input-relation-symbol (first l) 2)) arg1 arg2)))))
     (t
      (let ((l nil) l-last)
        (dolist (rel rels)
          (collect (list (function-name (input-relation-symbol (nth rel names) 2)) arg1 arg2) l))
        (cons 'or-jepd l))))))

(defun equal-jepd-relation-atom-args-p (args1 args2 subst invert)
  ;; lists of possible relations in third argument are compared by variant-p instead of equal-p
  ;; after inversion; all the variables in a list of possible relations are required to be unique,
  ;; so their exact identity is unimportant
  (let ((x1 (pop args1)) (y1 (pop args1)) (rels1 (first args1))
        (x2 (pop args2)) (y2 (pop args2)) (rels2 (first args2)))
    (or (and (equal-p x1 x2 subst)
             (equal-p y1 y2 subst)
             (equal-p rels1 rels2 subst))
        (and (dereference rels1 subst :if-compound-cons t)
             (dereference rels2 subst :if-compound-cons t)
             (and (equal-p x1 y2 subst)
                  (equal-p y1 x2 subst)
                  (variant-p rels1 (reversem rels2 invert) subst))))))

(defun variant-jepd-relation-atom-args (cc args1 args2 subst matches invert)
  (let ((x1 (pop args1)) (y1 (pop args1)) (rels1 (first args1))
        (x2 (pop args2)) (y2 (pop args2)) (rels2 (first args2)))
    (prog->
      (variant x1 x2 subst matches ->* matches)
      (variant y1 y2 subst matches ->* matches)
      (variant rels1 rels2 subst matches ->* matches)
      (funcall cc matches))
    (when (and (dereference rels1 subst :if-compound-cons t)
               (dereference rels2 subst :if-compound-cons t))
      (prog->
        (quote nil -> rels2*)
        (variant x1 y2 subst matches ->* matches)
        (variant y1 x2 subst matches ->* matches)
        (variant rels1 (or rels2* (setf rels2* (reversem rels2 invert))) subst matches ->* matches)
        (funcall cc matches)))))

(defun unify-jepd-relation-atom-args (cc args1 args2 subst invert)
  (let ((x1 (pop args1)) (y1 (pop args1)) (rels1 (first args1))
        (x2 (pop args2)) (y2 (pop args2)) (rels2 (first args2)))
    (prog->
      (unify x1 x2 subst ->* subst)
      (unify y1 y2 subst ->* subst)
      (unify rels1 rels2 subst ->* subst)
      (funcall cc subst))
    (cond
     ((dereference rels2 subst :if-compound-cons t)
      (prog->
        (quote nil -> rels2*)
        (unify x1 y2 subst ->* subst)
        (unify y1 x2 subst ->* subst)
        (unify rels1 (or rels2* (setf rels2* (reversem rels2 invert))) subst ->* subst)
        (funcall cc subst)))
     ((dereference rels1 subst :if-compound-cons t)
      (prog->
        (quote nil -> rels1*)
        (unify y1 x2 subst ->* subst)
        (unify x1 y2 subst ->* subst)
        (unify (or rels1* (setf rels1* (reversem rels1 invert))) rels2 subst ->* subst)
        (funcall cc subst))))))

(defun jepd-relation-atom-rewriter (atom subst)
  ;; replace by true
  ;; atoms like (time-pp-relation a b (list 1 1 1))
  ;; that can be produced by factoring
  (let ((v (third (args atom))))
    (if (dereference
         v subst
         :if-compound-cons (dolist (x v t)
                             (dereference x subst :if-variable (return nil))))
        true
        none)))

;;; jepd-relations.lisp
