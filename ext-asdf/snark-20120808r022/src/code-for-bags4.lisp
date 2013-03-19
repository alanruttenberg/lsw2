;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: code-for-bags4.lisp
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

(defvar *singleton-bag*)
(defvar *bag-union*)

;;; $$bag and $$bag* terms are translated into a standardized internal representation for bags
;;; that has $$$bag-union as the top function symbol
;;; ($$bag)        -> ($$bag-union)
;;; ($$bag a)      -> ($$bag-union ($$singleton-bag a))
;;; ($$bag a b)    -> ($$bag-union ($$singleton-bag a) ($$singleton-bag b))
;;; ($$bag a b c)  -> ($$bag-union ($$singleton-bag a) ($$singleton-bag b) ($$singleton-bag c))
;;; ($$bag* a)     -> ($$bag-union a)
;;; ($$bag* a b)   -> ($$bag-union ($$singleton-bag a) b)
;;; ($$bag* a b c) -> ($$bag-union ($$singleton-bag a) ($$singleton-bag b) c)

;;; variables and terms that represent bags should always be enclosed in bag-union, bag, or bag* symbols
;;; (bag-union a ?x) and a are not recognized as unifiable because they have different head symbols
;;; (bag-union a ?x) and (bag-union a) can be unified

(defun declare-code-for-bags ()
  (declare-subsort 'bag :top-sort-a)
  (declare-characteristic-relation '$$bagp #'bagp 'bag)
  (declare-function1 '$$bag :any :macro t :input-code 'input-bag-term)
  (declare-function1 '$$bag* :any :macro t :input-code 'input-bag*-term)
  (setf *singleton-bag*					;should only be used as argument of bag-union
        (declare-function1 '$$singleton-bag 1		;unexported symbol that shouldn't be visible to user
                           :sort 'bag
                           :constructor t))
  (setf *bag-union*
        (declare-function1 '$$bag-union 2
                           :sort '(bag (t bag))
                           :associative t
                           :commutative t
                           :identity '(function)	;use (bag-union) as identity
                           :keep-head t
                           :to-lisp-code 'bag-union-term-to-lisp))
  (declare-ordering-greaterp '$$bag-union '$$singleton-bag)
  (declare-function1 '$$bag-to-list 1 :sort 'list :rewrite-code #'(lambda (x s) (bag-to-list (arg1 x) s)))
  (declare-function1 '$$list-to-bag 1 :sort 'bag  :rewrite-code #'(lambda (x s) (list-to-bag (arg1 x) s)))
  nil)

(defun bagp (x &optional subst)
  (dereference x subst :if-compound-appl (eq *bag-union* (heada x))))

(defun input-bag-term (head args polarity)
  (declare (ignore head))
  (input-term1 `($$bag-union ,@(mapcar #'(lambda (arg) `($$singleton-bag ,arg)) args)) polarity))

(defun input-bag*-term (head args polarity)
  (require-n-or-more-arguments head args polarity 1)
  (input-term1 `($$bag-union ,@(mapcar #'(lambda (arg) `($$singleton-bag ,arg)) (butlast args)) ,(first (last args))) polarity))

(defun bag-union-term-to-lisp (head args subst)
  (mvlet* (((:values u v) (split-if #'(lambda (x) (dereference x subst :if-compound-appl (eq *singleton-bag* (heada x))))
                                    (argument-list-a1 head args subst)))
           (u (mapcar #'(lambda (x) (dereference x subst) (term-to-lisp (arg1a x) subst)) u))
           (v (mapcar #'(lambda (x) (term-to-lisp x subst)) v)))
    (cond
     ((null v)
      `(,(current-function-name '$$bag :any) ,@u))
     ((null u)
      `(,(function-name *bag-union*) ,@v))
     (t
      `(,(function-name *bag-union*) (,(current-function-name '$$bag :any) ,@u) ,@v)))))

(defun bag-to-list (bag &optional subst)
  (dereference
   bag subst
   :if-variable none
   :if-constant none
   :if-compound-cons none
   :if-compound-appl (cond
                      ((eq *bag-union* (heada bag))
                       (mapcar #'(lambda (x)
                                   (if (dereference x subst :if-compound-appl (eq *singleton-bag* (heada x)))
                                       (first (argsa x))
                                       (return-from bag-to-list none)))
                               (argument-list-a1 *bag-union* (argsa bag) subst)))
                      (t
                       none))))

(defun list-to-bag (list &optional subst)
  (dereference
   list subst
   :if-variable none
   :if-compound-appl none
   :if-constant (if (null list) (make-compound *bag-union*) none)
   :if-compound-cons (let ((sbags nil))
                       (loop
                         (push (make-compound *singleton-bag* (pop list)) sbags)
                         (dereference
                          list subst
                          :if-variable (return none)
                          :if-compound-appl (return none)
                          :if-constant (return (if (null list) (make-compound* *bag-union* (reverse sbags)) none)))))))

;;; code-for-bags4.lisp EOF
