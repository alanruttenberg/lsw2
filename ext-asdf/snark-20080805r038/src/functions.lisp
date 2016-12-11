;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: functions.lisp
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

(declaim (special *subsuming*))

(defvar *name*)

(defstruct (function-symbol
            (:constructor make-function-symbol0 (name arity))
            (:copier nil)
            (:print-function print-function-symbol)
            (:conc-name :function-))
  (name nil)
  (arity nil :read-only t)
  (number nil)
  (hash-code (make-atom-hash-code) :read-only t)
  (boolean-valued-p nil)
  (constructor nil)
  (injective nil)
  (magic t)					;nil means don't make magic-set goal for this relation
  (allowed-in-answer t)
  (kbo-weight 1)
  (weight 1)
  (constraint-theory nil)
  (sort-declarations nil)
  (logical-symbol-p nil)
  (logical-symbol-dual nil)
  (polarity-map nil) 				;list of unary functions to compute polarity of arguments
  (ordering-status nil)				;:left-to-right, :right-to-left, or :multiset comparison of argument lists
  (make-compound*-function nil)
  (input-code nil)
  (weight-code nil)
  (satisfy-code nil)				;LISP functions for making atoms headed by this relation true
  (falsify-code nil)				;LISP functions for making atoms headed by this relation false
  (paramodulate-code nil)			;LISP functions for paramodulating terms headed by this function
  (rewrite-code nil)				;LISP functions for rewriting terms headed by this function
  (equality-rewrite-code nil)			;LISP functions for rewriting equality of two terms headed by this function
  (arithmetic-equality-rewrite-code nil)	;LISP functions for rewriting equality of a number and an arithmetic term
  (equal-code nil)
  (variant-code nil)
  (unify-code nil)
  (associative nil)
  (commutative nil)
;;(idempotent nil)				;unifiable terms may have different heads
;;(inverse nil)					;unifiable terms may have different heads
  (identity none)				;unifiable terms may have different heads (none means no identity)
  (index-type nil)
  (rewritable-p nil) 				;if nil, no rewrite rule exists with this symbol as lhs head
  #+ignore (canonical-variants (make-sparse-vector))	;for instance-graphs
  #+ignore (instance-graph				;for instance-graphs
            (make-instance-graph
             :name (concatenate 'string "for " (string *name*))))
  #+ignore (term-memory-entries (make-sparse-vector))	;for instance-graphs
  (plist nil))					;property list for more properties

(define-plist-slot-accessor function :locked)
(define-plist-slot-accessor function :documentation)
(define-plist-slot-accessor function :author)
(define-plist-slot-accessor function :source)
(define-plist-slot-accessor function :code-name0)
(define-plist-slot-accessor function :macro)
(define-plist-slot-accessor function :complement)	;complement of the symbol P is the symbol ~P
(define-plist-slot-accessor function :skolem-p)
(define-plist-slot-accessor function :created-p)
(define-plist-slot-accessor function :to-lisp-code)
(define-plist-slot-accessor function :rewrites)
(define-plist-slot-accessor function :identity-p1)	;unify (bag-union x y) (bag-union a) should yield x->(bag-union a), y->(bag-union) etc.
(define-plist-slot-accessor function :injective-supplied)
(define-plist-slot-accessor function :do-not-resolve)
(define-plist-slot-accessor function :do-not-factor)
(define-plist-slot-accessor function :do-not-paramodulate)

(defun make-function-symbol (name arity)
  (let* ((*name* name)
         (fn (make-function-symbol0 name arity)))
    (setf (function-number fn) (funcall *standard-eql-numbering* :lookup fn))
    fn))

(defun function-kind (fn)
  (cond
   ((function-logical-symbol-p fn)
    :logical-symbol)
   ((function-boolean-valued-p fn)
    :relation)
   (t
    :function)))

(defun function-sort-declaration (fn)
  (let ((fsds (function-sort-declarations fn)))
    (unless (null (rest fsds))
      (cerror "Use the first one."
              "~S has more than one sort declaration."
              (function-name fn)))
    (first fsds)))

(defun function-identity2 (fn)
  (if *subsuming* none (function-identity fn)))

(defun function-name-lessp (x y)
  (string< x y))

#+ignore
(defun right-identity-e-term-rewriter (term subst)
  ;; function-rewrite-code example
  ;; (fn x e) -> x
  (mvlet (((:list x y) (args term)))
    (if (equal-p y 'e subst) x none)))		;return value or none

#+ignore
(defun right-identity-e-term-paramodulater (cc term subst)
  ;; function-paramodulate-code example
  ;; (fn x y) -> x after unifying y with e
  (prog->
    (args term -> (:list x y))
    (unify y 'e subst ->* subst)
    (funcall cc x subst)))		;call cc with value and substitution

(defmacro set-function-code (code)
  (let ((code-supplied (intern (format nil "~A-~A" code :supplied) :snark))
        (function-code (intern (format nil "~A-~A" :function code) :snark)))
    `(when ,code-supplied
       (setf (,function-code symbol)
             (if (listp ,code)
                 (remove-duplicates ,code :from-end t)				;replace
                 (cons ,code (remove ,code (,function-code symbol))))))))	;add

(defun declare-function-symbol0 (symbol
                                 &key
                                 new-name
                                 alias
                                 sort
                                 (locked nil locked-supplied)
                                 (documentation nil documentation-supplied)
                                 (author nil author-supplied)
                                 (source nil source-supplied)
                                 (macro nil macro-supplied)
                                 (weight nil weight-supplied)
                                 (allowed-in-answer nil allowed-in-answer-supplied)
                                 (ordering-status nil ordering-status-supplied)
                                 (constructor nil constructor-supplied)
                                 (injective nil injective-supplied)
                                 (skolem-p nil skolem-p-supplied)
                                 (created-p nil created-p-supplied)
                                 (kbo-weight nil kbo-weight-supplied)
                                 (complement nil complement-supplied)
                                 (magic t magic-supplied)
                                 (constraint-theory nil constraint-theory-supplied)
                                 (polarity-map nil polarity-map-supplied)
                                 (make-compound*-function nil make-compound*-function-supplied)
                                 (input-code nil input-code-supplied)
                                 (to-lisp-code nil to-lisp-code-supplied)
                                 (weight-code nil weight-code-supplied)
                                 (rewrite-code nil rewrite-code-supplied)
                                 (equality-rewrite-code nil equality-rewrite-code-supplied)
                                 (arithmetic-equality-rewrite-code nil arithmetic-equality-rewrite-code-supplied)
                                 (equal-code nil equal-code-supplied)
                                 (variant-code nil variant-code-supplied)
                                 (unify-code nil unify-code-supplied)
                                 (paramodulate-code nil paramodulate-code-supplied)
                                 (satisfy-code nil satisfy-code-supplied)
                                 (falsify-code nil falsify-code-supplied)
                                 (associative nil associative-supplied)
                                 (commutative nil commutative-supplied)
                                 (identity nil identity-supplied)
                                 (index-type nil index-type-supplied)
                                 (identity-p1 nil identity-p1-supplied)
                                 (infix nil infix-supplied)
                                 )
  (cl:assert (implies satisfy-code-supplied (eq :relation (function-kind symbol))))
  (cl:assert (implies falsify-code-supplied (eq :relation (function-kind symbol))))
  (cl:assert (implies constructor-supplied (eq :function (function-kind symbol))))
  (cl:assert (implies skolem-p-supplied (eq :function (function-kind symbol))))
  (cl:assert (implies complement-supplied (eq :relation (function-kind symbol))))
  (cl:assert (implies magic-supplied (eq :relation (function-kind symbol))))
  (cl:assert (implies polarity-map-supplied (eq :logical-symbol (function-kind symbol))))
  (cl:assert (implies constraint-theory-supplied (or (eq :function (function-kind symbol)) (eq :relation (function-kind symbol)))))
  (cl:assert (implies associative-supplied (or (eq :function (function-kind symbol)) (eq :logical-symbol (function-kind symbol)))))
  (cl:assert (implies identity-supplied (or (eq :function (function-kind symbol)) (eq :logical-symbol (function-kind symbol)))))
  ;; doesn't do anything if no keywords are supplied
  (when new-name
    (rename-function-symbol symbol new-name))
  (when alias
    (create-aliases-for-symbol symbol alias))
  (when sort
    (declare-function-sort symbol sort))
  (set-slot-if-supplied function locked)
  (set-slot-if-supplied function documentation)
  (set-slot-if-supplied function author)
  (set-slot-if-supplied function source)
  (set-slot-if-supplied function macro)
  (set-slot-if-supplied function weight)
  (set-slot-if-supplied function allowed-in-answer)
  (set-slot-if-supplied function ordering-status)
  (set-slot-if-supplied function constructor)
  (cond
   (injective-supplied
    (setf (function-injective symbol) injective)
    (setf (function-injective-supplied symbol) t))
   ((and constructor (not (function-injective-supplied symbol)))
    (setf (function-injective symbol) t)))	;declare constructors to be injective unless explicitly declared otherwise
  (set-slot-if-supplied function skolem-p)
  (set-slot-if-supplied function created-p)
  (set-slot-if-supplied function kbo-weight)
  (set-slot-if-supplied function complement)
  (set-slot-if-supplied function magic)
  (set-slot-if-supplied function constraint-theory)
  (set-slot-if-supplied function polarity-map)
  (set-slot-if-supplied function make-compound*-function)
  (set-function-code input-code)		;first non-none result of function call is returned
  (set-function-code to-lisp-code)		;first non-none result of function call is returned
  (set-function-code weight-code)		;first non-none result of function call is returned
  (set-function-code rewrite-code)		;first non-none result of function call is returned
  (set-function-code equality-rewrite-code)	;first non-none result of function call is returned
  (set-function-code arithmetic-equality-rewrite-code)	;first non-none result of function call is returned
  (when associative-supplied
    (when associative				;can't undeclare it
      (declare-function-associative symbol)))
  (when commutative-supplied
    (when commutative				;can't undeclare it
      (declare-function-commutative symbol)))
  (set-function-code equal-code)		;first non-none result of function call is returned
  (set-function-code variant-code)		;all functions called with continuation
  (set-function-code unify-code)		;all functions called with continuation
  (set-function-code paramodulate-code)		;all functions called with continuation
  (set-function-code satisfy-code)		;all functions called with continuation
  (set-function-code falsify-code)		;all functions called with continuation
  (set-slot-if-supplied function identity)
  (set-slot-if-supplied function index-type)
  (set-slot-if-supplied function identity-p1)	;special for bag-union
  (when infix-supplied
    (declare-operator-syntax (string (function-name symbol))
                             (first infix)	;one of :xfx, :xfy, :yfx, :yfy, :fx, :fy, :xf, :yf
                             (second infix)	;numerical precedence
                             (function-name symbol)))
  symbol)

(defun declare-function-symbol1 (symbol keys-and-values changeable)
  (cond
   ((null keys-and-values)
    symbol)
   (t
    (apply 'declare-function-symbol0
           symbol
           (cond
            ((function-locked symbol)
             (changeable-keys-and-values symbol keys-and-values changeable))
            (t
             keys-and-values))))))

(defun declare-function (name arity &rest keys-and-values)
  (declare (dynamic-extent keys-and-values))
  (declare-function-symbol1 (input-function-symbol name arity) keys-and-values (changeable-properties-of-locked-function?)))

(defun declare-relation (name arity &rest keys-and-values)
  (declare (dynamic-extent keys-and-values))
  (declare-function-symbol1 (input-relation-symbol name arity) keys-and-values (changeable-properties-of-locked-relation?)))

(defun declare-logical-symbol (name &rest keys-and-values)
  (declare-function-symbol1 (input-logical-symbol name t) `(,@keys-and-values :locked t) (changeable-properties-of-locked-logical-symbol?)))

(defun declare-function-associative (function &optional verbose)
  (when verbose
    (with-standard-io-syntax2
      (if (function-commutative function)
          (format t "~%; Declaring ~A to be associative-commutative." (function-name function))
          (format t "~%; Declaring ~A to be associative." (function-name function)))))
  (setf (function-associative function) t)
;;(setf (function-input-code function) (cons (lambda (h a p) (require-n-or-more-arguments h a p 2)) (function-input-code function)))
  (cond
    ((function-commutative function)
     (declare-function-symbol0
      function
      :ordering-status :ac
      :equal-code (cons 'equal-bag-p (remove 'equal-commute-p (function-equal-code function)))
      :variant-code (cons 'variant-bag (remove 'variant-commute (function-variant-code function)))
      :unify-code (cons 'unify-bag (remove 'unify-commute (function-unify-code function)))
      :index-type nil))
    (t
     (declare-function-symbol0
      function
      :ordering-status :ac
      :equal-code 'equal-vector-p
      :variant-code 'variant-vector
      :unify-code 'unify-vector
      :index-type nil)))
  (check-associative-function-sort function)
  nil)

(defun declare-function-commutative (function &optional verbose)
  (when verbose
    (with-standard-io-syntax2
      (if (function-associative function)
          (format t "~%; Declaring ~A to be associative-commutative." (function-name function))
          (format t "~%; Declaring ~A to be commutative." (function-name function)))))
  (setf (function-commutative function) t)
  (cond
    ((function-associative function)
     (declare-function-symbol0
      function
      :ordering-status :ac
      :equal-code (cons 'equal-bag-p (remove 'equal-vector-p (function-equal-code function)))
      :variant-code (cons 'variant-bag (remove 'variant-vector (function-variant-code function)))
      :unify-code (cons 'unify-bag (remove 'unify-vector (function-unify-code function)))
      :index-type nil))
    (t
     (declare-function-symbol0
      function
      :ordering-status :multiset
      :equal-code 'equal-commute-p
      :variant-code 'variant-commute
      :unify-code 'unify-commute
      :index-type :commute)))
  nil)

(defun function-code-name (symbol)
  (or (function-code-name0 symbol)
      (setf (function-code-name0 symbol) (intern (format nil "~A-~A" :code-for (function-name symbol)) :keyword))))

(defun function-resolve-code (fn v)
  (cond
   ((or (eq true v) (eq :neg v))
    (function-satisfy-code fn))
   (t
    (cl:assert (or (eq false v) (eq :pos v)))
    (function-falsify-code fn))))

(defun declare-function1 (name arity &rest options)
  (apply 'declare-function name arity
         `(,@options
           :locked t)))

(defun declare-function2 (name arity &rest options)
  (apply 'declare-function name arity
         `(,@options
           :locked t)))

(defun declare-relation1 (name arity &rest options)
  (apply 'declare-relation name arity
         `(:sort nil		;ignore sort declarations
           ,@options
           :locked t
           :magic nil)))

(defun declare-relation2 (name arity &rest options)
  (apply 'declare-relation name arity
         `(,@options
           :locked t
           :magic nil)))

(defun declare-characteristic-relation (name pred sort &rest options)
  (apply 'declare-relation2 name 1
         `(,@options
           :rewrite-code ,(make-characteristic-atom-rewriter pred sort))))

;;; functions.lisp EOF
