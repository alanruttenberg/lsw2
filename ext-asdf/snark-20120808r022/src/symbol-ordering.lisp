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
;;; Portions created by the Initial Developer are Copyright (C) 1981-2012.
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
         (not (if (function-symbol-p symbol2) (function-skolem-p symbol2) (constant-skolem-p symbol2)))
         (not (and (ordering-functions>constants?) (not (function-symbol-p symbol1)) (function-symbol-p symbol2))))
    '>)
   ((and (test-option23?)
         (not (if (function-symbol-p symbol1) (function-skolem-p symbol1) (constant-skolem-p symbol1)))
         (if (function-symbol-p symbol2) (function-skolem-p symbol2) (constant-skolem-p symbol2))
         (not (and (ordering-functions>constants?) (function-symbol-p symbol1) (not (function-symbol-p symbol2)))))
    '<)
   ((function-symbol-p symbol1)
    (cond
     ((not (function-symbol-p symbol2))
      '>)
     ((and (equality-relation-symbol-p symbol1) (not (equality-relation-symbol-p symbol2)))
      '<)
     ((and (equality-relation-symbol-p symbol2) (not (equality-relation-symbol-p symbol1)))
      '>)
     ((and (function-skolem-p symbol1) (not (function-skolem-p symbol2)))
      '>)
     ((and (function-skolem-p symbol2) (not (function-skolem-p symbol1)))
      '<)
     ((and (function-constructor symbol1) (not (function-constructor symbol2)))
      '<)
     ((and (function-constructor symbol2) (not (function-constructor symbol1)))
      '>)
     ((and (eq 'arithmetic (function-constraint-theory symbol1)) (not (eq 'arithmetic (function-constraint-theory symbol2))))
      '<)
     ((and (eq 'arithmetic (function-constraint-theory symbol2)) (not (eq 'arithmetic (function-constraint-theory symbol1))))
      '>)
     (t
      (let ((arity1 (if (function-associative symbol1) 2 (function-arity symbol1)))
            (arity2 (if (function-associative symbol2) 2 (function-arity symbol2))))
        (cond
         ((eql arity1 arity2)
          (cond
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
         ((and (constant-skolem-p symbol2) (not (constant-skolem-p symbol1)))
          '<)
         ((and (constant-constructor symbol1) (not (constant-constructor symbol2)))
          '<)
         ((and (constant-constructor symbol2) (not (constant-constructor symbol1)))
          '>)
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
    (if (greater? symbol1 symbol2) '> '<))))

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
    (map-symbol-table ->* name kind symbol)
    (declare (ignore name))
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
