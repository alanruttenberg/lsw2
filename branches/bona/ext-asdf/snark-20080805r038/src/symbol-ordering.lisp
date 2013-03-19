;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: symbol-ordering.lisp
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

(declaim
  (special
    *symbols-in-symbol-table*
    ))

;;; use-default-ordering = nil       no default ordering
;;; use-default-ordering = t         high arity > low arity, same arity alphabetically later > earlier
;;; use-default-ordering = :reverse  high arity > low arity, same arity alphabetically earlier > later
;;; use-default-ordering = :arity    high arity > low arity

(defvar ordering-is-total nil)			;can be set if all symbols have been totally ordered by ordering declarations

(defvar *symbol-ordering*)

(defun initialize-symbol-ordering ()
  (setf *symbol-ordering* (make-poset)))

(defun default-symbol-ordering-compare (symbol1 symbol2)
  (cond
   ((and (test-option23?)
         (if (function-symbol-p symbol1) (function-skolem-p symbol1) (constant-skolem-p symbol1))
         (not (if (function-symbol-p symbol2) (function-skolem-p symbol2) (constant-skolem-p symbol2))))
    '>)
   ((and (test-option23?)
         (not (if (function-symbol-p symbol1) (function-skolem-p symbol1) (constant-skolem-p symbol1)))
         (if (function-symbol-p symbol2) (function-skolem-p symbol2) (constant-skolem-p symbol2)))
    '<)
   ((function-symbol-p symbol1)
    (cond
     ((not (function-symbol-p symbol2))
      '>)
     ((and (equality-relation-symbol-p symbol1) (not (equality-relation-symbol-p symbol2)))
      '<)
     ((and (not (equality-relation-symbol-p symbol1)) (equality-relation-symbol-p symbol2))
      '>)
     (t
      (let ((arity1 (if (function-associative symbol1) 2 (function-arity symbol1)))
            (arity2 (if (function-associative symbol2) 2 (function-arity symbol2))))
        (cond
         ((eql arity1 arity2)
          (cond
           ((and (function-skolem-p symbol1) (not (function-skolem-p symbol2)))
            '>)
           ((and (not (function-skolem-p symbol1)) (function-skolem-p symbol2))
            '<)
           ((eq :arity (use-default-ordering?))
            '?)
           (t
            (default-symbol-ordering-compare1 (function-name symbol1) (function-name symbol2)))))
         ((or (not (numberp arity1))
              (not (numberp arity2)))
          '?)
         ((and (1-ary-functions>2-ary-functions-in-default-ordering?) (= 1 arity1) (= 2 arity2) (not (function-boolean-valued-p symbol1)) (not (function-boolean-valued-p symbol2)))
          '>)
         ((and (1-ary-functions>2-ary-functions-in-default-ordering?) (= 2 arity1) (= 1 arity2) (not (function-boolean-valued-p symbol1)) (not (function-boolean-valued-p symbol2)))
          '<)
         (t
          (if (> arity1 arity2) '> '<)))))))
   ((function-symbol-p symbol2)
    '<)
   ((symbolp symbol1)				;symbols > strings > numbers
    (if (symbolp symbol2)
        (cond
         ((and (constant-skolem-p symbol1) (not (constant-skolem-p symbol2)))
          '>)
         ((and (not (constant-skolem-p symbol1)) (constant-skolem-p symbol2))
          '<)
         ((eq :arity (use-default-ordering?))
          '?)
         (t
          (default-symbol-ordering-compare1 symbol1 symbol2)))
        '>))
   ((symbolp symbol2)
    '<)
   ((stringp symbol1)
    (if (stringp symbol2) (if (string> symbol1 symbol2) '> '<) '>))
   ((stringp symbol2)
    '<)
   (t
    (if (> symbol1 symbol2) '> '<))))

(defun default-symbol-ordering-compare1 (symbol1 symbol2)
  (if (if (eq :reverse (use-default-ordering?))
          (string< (symbol-name symbol1) (symbol-name symbol2))
          (string> (symbol-name symbol1) (symbol-name symbol2)))
      '>
      '<))

(defun declare-ordering-greaterp2 (x y)
  (cond
   ((or (not (iff (symbol-boolean-valued-p x) (symbol-boolean-valued-p y)))
        (and (ordering-functions>constants?) (not (function-symbol-p x)) (function-symbol-p y)))
    (warn "Ignoring ordering declaration ~A > ~A." x y))
   ((not (and (ordering-functions>constants?) (function-symbol-p x) (not (function-symbol-p y))))
    (declare-poset-greaterp *symbol-ordering* (symbol-number x) (symbol-number y)))))

(definline symbol-ordering-compare (symbol1 symbol2)
  (cond
   ((eql symbol1 symbol2)
    '=)
   (t
    (symbol-ordering-compare1 symbol1 symbol2))))

(defun symbol-ordering-compare1 (symbol1 symbol2)
  (let ((n1 (symbol-number symbol1))
        (n2 (symbol-number symbol2)))
    (cond
     ((poset-greaterp *symbol-ordering* n1 n2)
      '>)
     ((poset-greaterp *symbol-ordering* n2 n1)
      '<)
     (t
      (let ((ordering-fun (use-default-ordering?)))
        (cond
         (ordering-fun
          (cl:assert (iff (symbol-boolean-valued-p symbol1) (symbol-boolean-valued-p symbol2)))
          (let ((com (funcall (if (or (eq t ordering-fun)
                                      (eq :arity ordering-fun)
                                      (eq :reverse ordering-fun))
                                  #'default-symbol-ordering-compare
                                  ordering-fun)
                              symbol1
                              symbol2)))
            (ecase com
              (>
               (declare-ordering-greaterp2 symbol1 symbol2))
              (<
               (declare-ordering-greaterp2 symbol2 symbol1))
              (?
               ))
            com))
         (t
          '?)))))))

(defun opposite-order (x)
  (case x
    (>
     '<)
    (<
     '>)
    (otherwise
     x)))

(defun symbol-to-name (x)
  (cond
   ((function-symbol-p x)
    (function-name x))
   (t
    (constant-name x))))

(defun print-symbol-ordering (&optional (symbol-or-symbols none))
  (let ((symbols (cond
                  ((eq none symbol-or-symbols)
                   none)
                  ((consp symbol-or-symbols)
                   symbol-or-symbols)
                  (t
                   (list symbol-or-symbols))))
        (l nil))
    (prog->
      (map-sparse-vector-with-indexes (sparse-matrix-rows *symbol-ordering*) ->* row x#)
      (symbol-numbered x# -> x)
      (map-sparse-vector row ->* y#)
      (symbol-numbered y# -> y)
      (when (implies (neq none symbols)
                     (member (symbol-to-name x) symbols))
        (or (assoc x l) (first (push (list x nil nil) l)) -> v)
        (push y (third v)))
      (when (implies (neq none symbols)
                     (member (symbol-to-name y) symbols))
        (or (assoc y l) (first (push (list y nil nil) l)) -> v)
        (push x (second v))))
    (mapc (lambda (v)
            (setf (first v) (symbol-to-name (first v)))
            (when (second v)
              (setf (second v) (sort (mapcar 'symbol-to-name (second v)) 'constant-name-lessp)))
            (when (third v)
              (setf (third v) (sort (mapcar 'symbol-to-name (third v)) 'constant-name-lessp))))
          l)
    (setf l (sort l 'constant-name-lessp :key #'first))
    (terpri-comment)
    (prin1 `(ordering-functions>constants? ,(ordering-functions>constants?)))
    (dolist (v l)
      (terpri-comment)
      (prin1 (cons 'declare-ordering-greaterp
                   (append (and (second v) (list (kwote (second v))))
                           (list (kwote (first v)))
                           (and (third v) (list (kwote (third v))))))))))
   
(defun symbol-ordering-compare-compound*constant (compound constant subst testval)
  ;; for a constant to be bigger than a compound,
  ;; constant must be bigger than every constant/function symbol in compound
  ;; and compound must be ground
  ;;
  ;; for a constant to be less than a compound,
  ;; constant must be smaller than or identical to some constant/function symbol in compound
  (let ((can-be-< t))
    (labels
      ((compare-with-term (term)
         (dereference
          term subst
          :if-variable (if (eq '< testval) (return-from symbol-ordering-compare-compound*constant nil) (setf can-be-< nil))
          :if-constant (ecase (symbol-ordering-compare term constant)
                         ((> =)
                          (return-from symbol-ordering-compare-compound*constant '>))
                         (?
                          (if (eq '< testval) (return-from symbol-ordering-compare-compound*constant nil) (setf can-be-< nil)))
                         (<
                          ))
          :if-compound (progn
                         (ecase (symbol-ordering-compare (head term) constant)
                           (>
                            (return-from symbol-ordering-compare-compound*constant '>))
                           (?
                            (if (eq '< testval) (return-from symbol-ordering-compare-compound*constant nil) (setf can-be-< nil)))
                           (<
                            ))
                         (dolist (arg (args term))
                           (compare-with-term arg))))))
      (let ((head (head compound)))
        (cond
         ((function-boolean-valued-p head)
          (return-from symbol-ordering-compare-compound*constant
            (if (constant-boolean-valued-p constant)
		(if (ordering-functions>constants?) '> (symbol-ordering-compare head constant))	;no subterm comparisons
                '>)))					;atom > term
         ((constant-boolean-valued-p constant)
          (return-from symbol-ordering-compare-compound*constant '<))			;term < atom
         ((ordering-functions>constants?)
          '>)
         (t
          (ecase (symbol-ordering-compare head constant)
            (>
             (return-from symbol-ordering-compare-compound*constant '>))
            (?
             (if (eq '< testval) (return-from symbol-ordering-compare-compound*constant nil) (setf can-be-< nil)))
            (<
             ))
          (dolist (arg (args compound))
            (compare-with-term arg))
          (if can-be-< '< '?)))))))

(defun symbol-ordering-compare-constant*compound (constant compound subst testval)
  (opposite-order
   (symbol-ordering-compare-compound*constant
    compound constant subst (opposite-order testval))))

(defun symbol-ordering-compare-compound*variable (y x subst)
;;(if (variable-occurs-p x y subst) '> '?)
  (if (dereference
       y #+ignore subst nil			;known to be compound, already dereferenced
       :if-compound-cons (if (null subst)
                             (or (variable-occurs-p1 x (carc y))
                                 (variable-occurs-p1 x (cdrc y)))
                             (or (variable-occurs-p2 x (carc y) subst)
                                 (variable-occurs-p2 x (cdrc y) subst)))
       :if-compound-appl (if (null subst)
                             (variable-occurs-p1l x (argsa y))
                             (variable-occurs-p2l x (argsa y) subst)))
      '>
      '?))

(defun symbol-ordering-compare-variable*compound (x y subst)
;;(if (variable-occurs-p x y subst) '< '?)
  (if (dereference
       y #+ignore subst nil			;known to be compound, already dereferenced
       :if-compound-cons (if (null subst)
                             (or (variable-occurs-p1 x (carc y))
                                 (variable-occurs-p1 x (cdrc y)))
                             (or (variable-occurs-p2 x (carc y) subst)
                                 (variable-occurs-p2 x (cdrc y) subst)))
       :if-compound-appl (if (null subst)
                             (variable-occurs-p1l x (argsa y))
                             (variable-occurs-p2l x (argsa y) subst)))
      '<
      '?))

(defun symbol-ordering-compare-term-multisets (xargs yargs subst testval
            &optional
            (compound*compound-compare-fn #'rpo-compare-compounds)
            (compound*constant-compare-fn #'symbol-ordering-compare-compound*constant)
            (compound*variable-compare-fn #'symbol-ordering-compare-compound*variable))
  (let ((variable-counts nil) (constant-counts nil) (compound-counts nil)
        (xargs-compound-exists nil) (yargs-compound-exists nil)
        (xargs-remain nil) (yargs-remain nil) term)
    ;; destructively updates lists of
    ;; variable and count pairs,
    ;; constant and count pairs, and
    ;; compound and count paris
    ;; term and count pair is represented as (term . count)
    (let (v)				;count variables and constants in xargs
      (dolist (term xargs)
        (dereference
         term subst
         :if-compound (setf xargs-compound-exists t)
         :if-variable (cond
                       ((null variable-counts)
                        (setf variable-counts (cons (make-tc term 1) nil)))
                       ((setf v (assoc/eq term variable-counts))
                        (incf (tc-count v)))
                       (t
                        (push (make-tc term 1) variable-counts)))
         :if-constant (cond
                       ((null constant-counts)
                        (setf constant-counts (cons (make-tc term 1) nil)))
                       ((setf v (assoc term constant-counts))
                        (incf (tc-count v)))
                       (t
                        (push (make-tc term 1) constant-counts))))))
    
    (let (v)				;count variables and constants in yargs
      (dolist (term yargs)
        (dereference
         term subst
         :if-compound (setf yargs-compound-exists t)
         :if-variable (cond
                       ((null variable-counts)
                        (if (eq '= testval)
                            (return-from symbol-ordering-compare-term-multisets nil)
                            (setf variable-counts (cons (make-tc term -1) nil))))
                       ((setf v (assoc/eq term variable-counts))
                        (if (and (eq '= testval) (eql 0 (tc-count v)))
                            (return-from symbol-ordering-compare-term-multisets nil)
                            (decf (tc-count v))))
                       (t
                        (if (eq '= testval)
                            (return-from symbol-ordering-compare-term-multisets nil)
                            (push (make-tc term -1) variable-counts))))
         :if-constant (cond
                       ((null constant-counts)
                        (if (eq '= testval)
                            (return-from symbol-ordering-compare-term-multisets nil)
                            (setf constant-counts (cons (make-tc term -1) nil))))
                       ((setf v (assoc term constant-counts))
                        (if (and (eq '= testval) (eql 0 (tc-count v)))
                            (return-from symbol-ordering-compare-term-multisets nil)
                            (decf (tc-count v))))
                       (t
                        (if (eq '= testval)
                            (return-from symbol-ordering-compare-term-multisets nil)
                            (push (make-tc term -1) constant-counts)))))))
    
    (when (eq '= testval)
      (dolist (v constant-counts)
        (unless (eql 0 (tc-count v))
          (return-from symbol-ordering-compare-term-multisets nil)))
      (dolist (v variable-counts)
        (unless (eql 0 (tc-count v))
          (return-from symbol-ordering-compare-term-multisets nil)))
      (cond
       ((not xargs-compound-exists)
        (return-from symbol-ordering-compare-term-multisets (if yargs-compound-exists nil '=)))
       ((not yargs-compound-exists)
        (return-from symbol-ordering-compare-term-multisets nil))))
    
    (when xargs-compound-exists
      (let (v)			;count compounds in xargs
        (dolist (term xargs)
          (dereference
           term subst
           :if-compound (cond
                         ((null compound-counts)
                          (setf compound-counts (cons (make-tc term 1) nil)))
                         ((setf v (or (assoc/eq term compound-counts)
                                      (assoc term compound-counts
                                             :test (lambda (x y)
                                                     (eq '= (funcall compound*compound-compare-fn x y subst '=))))))
                          (incf (tc-count v)))
                         (t
                          (push (make-tc term 1) compound-counts)))))))
    
    (when yargs-compound-exists
      (let (v)			;count compounds in yargs
        (dolist (term yargs)
          (dereference
           term subst
           :if-compound (cond
                         ((null compound-counts)
                          (if (eq '= testval)
                              (return-from symbol-ordering-compare-term-multisets nil)
                              (setf compound-counts (cons (make-tc term -1) nil))))
                         ((setf v (or (assoc/eq term compound-counts)
                                      (assoc term compound-counts
                                             :test (lambda (x y)
                                                     (eq '= (funcall compound*compound-compare-fn x y subst '=))))))
                          (if (and (eq '= testval) (eql 0 (tc-count v)))
                              (return-from symbol-ordering-compare-term-multisets nil)
                              (decf (tc-count v))))
                         (t
                          (if (eq '= testval)
                              (return-from symbol-ordering-compare-term-multisets nil)
                              (push (make-tc term -1) compound-counts))))))))
    
    (when (eq '= testval)
      (dolist (v compound-counts)
        (unless (eql 0 (tc-count v))
          (return-from symbol-ordering-compare-term-multisets nil)))
      (return-from symbol-ordering-compare-term-multisets '=))
    
    (dolist (x variable-counts)
      (when (plusp (tc-count x))
        (setf term (tc-term x))
        (or (dolist (y compound-counts nil)
              (when (minusp (tc-count y))
                (when (eq '> (funcall compound*variable-compare-fn (tc-term y) term subst))
                  (setf (tc-count x) 0)
                  (return t))))
            (cond				;uneliminated xarg variable
             ((and testval (neq '> testval))
              (return-from symbol-ordering-compare-term-multisets nil))
             (t
              (setf xargs-remain t))))))
    
    (dolist (y variable-counts)
      (when (minusp (tc-count y))
        (setf term (tc-term y))
        (or (dolist (x compound-counts nil)
              (when (plusp (tc-count x))
                (when (eq '> (funcall compound*variable-compare-fn (tc-term x) term subst))
                  (setf (tc-count y) 0)
                  (return t))))
            (cond				;uneliminated yarg variable
             ((and testval (neq '< testval))
              (return-from symbol-ordering-compare-term-multisets nil))
             (xargs-remain
              (return-from symbol-ordering-compare-term-multisets '?))
             (t
              (setf yargs-remain t))))))
    
    (dolist (x constant-counts)
      (when (plusp (tc-count x))
        (setf term (tc-term x))
        (dolist (y constant-counts nil)
          (when (minusp (tc-count y))
            (ecase (symbol-ordering-compare term (tc-term y))
              (<
               (setf (tc-count x) 0)
               (return t))
              (>
               (setf (tc-count y) 0))
              (?
               ))))))
    
    (dolist (x constant-counts)
      (when (plusp (tc-count x))
        (setf term (tc-term x))
        (or (dolist (y compound-counts nil)
              (when (minusp (tc-count y))
                (ecase (funcall compound*constant-compare-fn (tc-term y) term subst nil)
                  (>
                   (setf (tc-count x) 0)
                   (return t))
                  (<
                   (setf (tc-count y) 0))
                  (?
                   ))))
            (cond					;uneliminated xarg constant
             ((and testval (neq '> testval))
              (return-from symbol-ordering-compare-term-multisets nil))
             (yargs-remain
              (return-from symbol-ordering-compare-term-multisets '?))
             (t
              (setf xargs-remain t))))))
    
    (dolist (y constant-counts)
      (when (minusp (tc-count y))
        (setf term (tc-term y))
        (or (dolist (x compound-counts nil)
              (when (plusp (tc-count x))
                (ecase (funcall compound*constant-compare-fn (tc-term x) term subst nil)
                  (>
                   (setf (tc-count y) 0)
                   (return t))
                  (<
                   (setf (tc-count x) 0))
                  (?
                   ))))
            (cond					;uneliminated yarg constant
             ((and testval (neq '< testval))
              (return-from symbol-ordering-compare-term-multisets nil))
             (xargs-remain
              (return-from symbol-ordering-compare-term-multisets '?))
             (t
              (setf yargs-remain t))))))
    
    (dolist (x compound-counts)
      (when (plusp (tc-count x))
        (setf term (tc-term x))
        (or (dolist (y compound-counts nil)
              (when (minusp (tc-count y))
                (ecase (funcall compound*compound-compare-fn term (tc-term y) subst nil)
                  (<
                   (setf (tc-count x) 0)
                   (return t))
                  (>
                   (setf (tc-count y) 0))
                  (?
                   ))))
            (cond					;uneliminated xarg compound
             ((and testval (neq '> testval))
              (return-from symbol-ordering-compare-term-multisets nil))
             (yargs-remain
              (return-from symbol-ordering-compare-term-multisets '?))
             (t
              (setf xargs-remain t))))))
    
    (cond
     (yargs-remain
      '<)
     (t
      (dolist (y compound-counts)
        (when (minusp (tc-count y))
          (cond						;uneliminated yarg compound
           (xargs-remain
            (return-from symbol-ordering-compare-term-multisets '?))
           (t
            (return-from symbol-ordering-compare-term-multisets '<)))))
      (if xargs-remain '> '=)))))

(defun declare-ordering-greaterp (x y &rest others)
  ;; user function for declaring that x > y in ordering precedence relation
  ;; x and y can be a symbol or lists of symbols
  ;; if x and y are lists of symbols, then every symbol in x is declared greater than every symbol in y
  (dotails (l (mapcar (lambda (x)
                        (if (consp x) (mapcar #'input-symbol x) (list (input-symbol x))))
		      (list* x y others)))
    (unless (null (rest l))
      (dolist (x (first l))
	(dolist (y (second l))
	  (declare-ordering-greaterp2 x y))))))

(defun rpo-add-created-function-symbol (fn)
  (prog->
    (map-symbol-table ->* name type# symbol)
    (declare (ignore name))
    (decode-symbol-table-type type# -> kind arity)
    (declare (ignore arity))
    (cond
     ((or (eq :variable kind) (eq :sort kind))
      )
     ((eq symbol fn)
      )
     ((symbol-boolean-valued-p symbol)
      )
     ((if (function-symbol-p fn)
          (and (function-symbol-p symbol)
               (function-created-p symbol)
               (> (function-arity fn) (function-arity symbol)))
          (and (not (function-symbol-p symbol))
               (constant-created-p symbol)))
      (declare-ordering-greaterp2 fn symbol))
     (t
      (declare-ordering-greaterp2 symbol fn)))))

;;; symbol-ordering.lisp EOF
