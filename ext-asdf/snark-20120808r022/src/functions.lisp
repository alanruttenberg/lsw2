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
;;; Portions created by the Initial Developer are Copyright (C) 1981-2012.
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
  (sort (top-sort))
  (argument-sort-alist nil)
  (logical-symbol-p nil)
  (logical-symbol-dual nil)
  (polarity-map nil) 				;list of unary functions to compute polarity of arguments
  (ordering-status nil)				;:left-to-right, :right-to-left, :multiset, or :ac comparison of argument lists
  (make-compound*-function nil)
  (input-code nil)
  (weight-code nil)
  (satisfy-code nil)				;Lisp functions for making atoms headed by this relation true
  (falsify-code nil)				;Lisp functions for making atoms headed by this relation false
  (paramodulate-code nil)			;Lisp functions for paramodulating terms headed by this function
  (rewrite-code nil)				;Lisp functions for rewriting terms headed by this function
  (equality-rewrite-code nil)			;Lisp functions for rewriting equality of two terms headed by this function
  (arithmetic-relation-rewrite-code nil)	;Lisp functions for rewriting equality of a number and an arithmetic term
  (sort-code nil)				;Lisp functions for computing sort of a term
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
             :name (to-string "for " *name*)))
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
(define-plist-slot-accessor function :injective-supplied)
(define-plist-slot-accessor function :do-not-resolve)
(define-plist-slot-accessor function :do-not-factor)
(define-plist-slot-accessor function :do-not-paramodulate)
(define-plist-slot-accessor function :keep-head)	;keep (fn) and (fn arg) instead of identity and arg respectively

(definline function-rpo-status (fn)
  (or (function-ordering-status fn) (rpo-status?)))

(definline function-kbo-status (fn)
  (or (function-ordering-status fn) (kbo-status?)))

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

(defun function-has-arity-p (fn arity)
  (let ((a (function-arity fn)))
    (or (eql arity a) (eq :any a) (function-associative fn))))

(defun function-identity2 (fn)
  (if (and *subsuming* (not (test-option45?))) none (function-identity fn)))

(defun function-name-lessp (x y)
  (string< x y))

(defun function-name-arity-lessp (fn1 fn2)
  (let ((name1 (function-name fn1))
        (name2 (function-name fn2)))
    (and (string<= name1 name2)
         (implies (string= name1 name2)
                  (let ((arity1 (function-arity fn1)))
                    (and (numberp arity1)
                         (let ((arity2 (function-arity fn2)))
                           (and (numberp arity2) (< arity1 arity2)))))))))

#+ignore
(defun right-identity-e-term-rewriter (term subst)
  ;; function-rewrite-code example
  ;; (fn x e) -> x
  (mvlet (((list x y) (args term)))
    (if (equal-p y 'e subst) x none)))		;return value or none

#+ignore
(defun right-identity-e-term-paramodulater (cc term subst)
  ;; function-paramodulate-code example
  ;; (fn x y) -> x after unifying y with e
  (prog->
    (args term -> (list x y))
    (unify y 'e subst ->* subst)
    (funcall cc x subst)))		;call cc with value and substitution

(defmacro set-function-code (code)
  (let ((code-supplied (intern (to-string code :-supplied) :snark))
        (function-code (intern (to-string :function- code) :snark)))
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
                                 locked
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
                                 (arithmetic-relation-rewrite-code nil arithmetic-relation-rewrite-code-supplied)
                                 (sort-code nil sort-code-supplied)
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
                                 (infix nil infix-supplied)
                                 (do-not-resolve nil do-not-resolve-supplied)
                                 (do-not-factor nil do-not-factor-supplied)
                                 (do-not-paramodulate nil do-not-paramodulate-supplied)
                                 (keep-head nil keep-head-supplied)
                                 )
  (cl:assert (implies satisfy-code-supplied (eq :relation (function-kind symbol))))
  (cl:assert (implies falsify-code-supplied (eq :relation (function-kind symbol))))
  (cl:assert (implies constructor-supplied (eq :function (function-kind symbol))))
  (cl:assert (implies skolem-p-supplied (eq :function (function-kind symbol))))
  (cl:assert (implies complement-supplied (eq :relation (function-kind symbol))))
  (cl:assert (implies magic-supplied (eq :relation (function-kind symbol))))
  (cl:assert (implies polarity-map-supplied (eq :logical-symbol (function-kind symbol))))
  (cl:assert (implies constraint-theory-supplied (or (eq :function (function-kind symbol)) (eq :relation (function-kind symbol)))))
  (cl:assert (implies associative-supplied (and (member (function-kind symbol) '(:function :logical-symbol))
                                                (member (function-arity symbol) '(2 :any)))))
  (cl:assert (implies identity-supplied (member (function-kind symbol) '(:function :logical-symbol))))
  (cl:assert (implies (and kbo-weight-supplied (consp kbo-weight)) (eql (function-arity symbol) (length (rest kbo-weight)))))
  ;; doesn't do anything if no keywords are supplied
  (when new-name
    (rename-function-symbol symbol new-name))
  (when alias
    (create-aliases-for-symbol symbol alias))
  (when sort
    (declare-function-sort symbol sort))
  (when locked
    (setf (function-locked symbol) locked))		;once locked, stays locked
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
    (setf (function-injective symbol) t)))		;declare constructors to be injective unless explicitly declared otherwise
  (set-slot-if-supplied function skolem-p)
  (set-slot-if-supplied function created-p)
  (set-slot-if-supplied function kbo-weight)
  (set-slot-if-supplied function complement)
  (set-slot-if-supplied function magic)
  (set-slot-if-supplied function constraint-theory)
  (set-slot-if-supplied function polarity-map)
  (set-slot-if-supplied function make-compound*-function)
  (set-function-code input-code)			;first non-none result of function call is returned
  (set-function-code to-lisp-code)			;first non-none result of function call is returned
  (set-function-code weight-code)			;first non-none result of function call is returned
  (set-function-code rewrite-code)			;first non-none result of function call is returned
  (set-function-code equality-rewrite-code)		;first non-none result of function call is returned
  (set-function-code arithmetic-relation-rewrite-code)	;first non-none result of function call is returned
  (set-function-code sort-code)				;first non-none result of function call is returned
  (when associative-supplied
    (when associative					;can't undeclare it
      (declare-function-associative symbol)))
  (when commutative-supplied
    (when commutative					;can't undeclare it
      (declare-function-commutative symbol)))
  (set-function-code equal-code)			;first non-none result of function call is returned
  (set-function-code variant-code)			;all functions called with continuation
  (set-function-code unify-code)			;all functions called with continuation
  (set-function-code paramodulate-code)			;all functions called with continuation
  (set-function-code satisfy-code)			;all functions called with continuation
  (set-function-code falsify-code)			;all functions called with continuation
  (when identity-supplied
    (unless (eq none identity)
      (cond
       ((equal '(function) identity)			;e.g., use (bag-union) as identity for bag-union function
        (setf identity (make-compound symbol)))
       (t
        (setf identity (declare-constant identity))))
    (setf (function-identity symbol) identity)))
  (set-slot-if-supplied function index-type)
  (set-slot-if-supplied function do-not-resolve)
  (set-slot-if-supplied function do-not-factor)
  (set-slot-if-supplied function do-not-paramodulate)
  (set-slot-if-supplied function keep-head)
  (when (and (function-constructor symbol) (or (function-associative symbol) (function-commutative symbol)))
    (setf (function-injective symbol) nil))
  (when (and (neq none (function-identity symbol)) (function-associative symbol))
    (let ((rewrite-code-supplied t)
          (paramodulate-code-supplied t)
          (rewrite-code 'associative-identity-rewriter)
          (paramodulate-code 'associative-identity-paramodulater))
      (set-function-code rewrite-code)
      (set-function-code paramodulate-code)))
  (cl:assert (implies (consp (function-kbo-weight symbol))
                      (and (member (function-kbo-status symbol) '(:left-to-right :right-to-left))
                           (not (function-associative symbol)))))
  (when infix-supplied
    (declare-operator-syntax (string (function-name symbol))
                             (first infix)	;one of :xfx, :xfy, :yfx, :yfy, :fx, :fy, :xf, :yf
                             (second infix)	;numerical precedence
                             (function-name symbol)))
  symbol)

(defun declare-function-symbol1 (symbol keys-and-values)
  (cond
   ((null keys-and-values)
    symbol)
   (t
    (apply 'declare-function-symbol0
           symbol
           (cond
            ((and (function-locked symbol) (eq none (getf keys-and-values :locked none)))
             (changeable-keys-and-values
              symbol
              keys-and-values
              (if (function-logical-symbol-p symbol) '(:alias) (changeable-properties-of-locked-function?))))
            (t
             keys-and-values))))))

(defun declare-function (name arity &rest keys-and-values)
  (declare (dynamic-extent keys-and-values))
  (declare-function-symbol1 (input-function-symbol name arity) keys-and-values))

(defun declare-relation (name arity &rest keys-and-values)
  (declare (dynamic-extent keys-and-values))
  (declare-function-symbol1 (input-relation-symbol name arity) keys-and-values))

(defun declare-logical-symbol (name &rest keys-and-values)
  (declare-function-symbol1 (input-logical-symbol name t) `(,@keys-and-values :locked t)))

(defun declare-function-associative (function)
  (setf (function-associative function) t)
;;(setf (function-input-code function) (cons (lambda (h a p) (require-n-or-more-arguments h a p 2)) (function-input-code function)))
  (cond
    ((function-commutative function)
     (declare-function-symbol0
      function
      :ordering-status :ac
      :equal-code (cons 'ac-equal-p (remove 'commutative-equal-p (function-equal-code function)))
      :variant-code (cons 'variant-bag (remove 'variant-commute (function-variant-code function)))
      :unify-code (cons 'ac-unify (remove 'commutative-unify (function-unify-code function)))
      :index-type nil))
    (t
     (declare-function-symbol0
      function
;;    :ordering-status :ac
      :equal-code 'associative-equal-p
      :variant-code 'variant-vector
      :unify-code 'associative-unify
      :index-type nil)))
;;(check-associative-function-sort function)
  nil)

(defun declare-function-commutative (function)
  (setf (function-commutative function) t)
  (cond
    ((function-associative function)
     (declare-function-symbol0
      function
      :ordering-status :ac
      :equal-code (cons 'ac-equal-p (remove 'associative-equal-p (function-equal-code function)))
      :variant-code (cons 'variant-bag (remove 'variant-vector (function-variant-code function)))
      :unify-code (cons 'ac-unify (remove 'associative-unify (function-unify-code function)))
      :index-type nil))
    (t
     (declare-function-symbol0
      function
      :ordering-status :commutative
      :equal-code 'commutative-equal-p
      :variant-code 'variant-commute
      :unify-code 'commutative-unify
      :index-type :commute)))
  nil)

(defun function-code-name (symbol)
  (or (function-code-name0 symbol)
      (setf (function-code-name0 symbol) (intern (to-string :code-for- (function-name symbol)) :keyword))))

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
           ;; :unify-code (dont-unify)	;omitted in 20120808r008
           :do-not-paramodulate t
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
           :do-not-resolve t
           :do-not-factor t
           :locked t
           :magic nil)))

(defun declare-characteristic-relation (name pred sort &rest options)
  (apply 'declare-relation2 name 1
         `(,@options
           :rewrite-code ,(make-characteristic-atom-rewriter pred sort))))

;;; functions.lisp EOF
