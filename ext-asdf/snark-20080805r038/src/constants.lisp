;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: constants.lisp
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

;;; Lisp symbols, strings, numbers, and characters are used directly as SNARK constants
;;; but SNARK needs to associate information with them
;;; it is stored in constant-info structures found in *constant-info-table* hash-array

(defstruct constant-info
  (number nil)
  (hash-code (make-atom-hash-code) :read-only t)
  (boolean-valued-p0 nil)		;overloaded to be input name of the proposition
  (constructor0 nil)
  (magic t)				;nil means don't make magic-set goal for this proposition
  (allowed-in-answer t)
  (kbo-weight 1)
  (weight 1)
  (constraint-theory nil)
  (sort (top-sort))
  (plist nil))				;property list for more properties

(defvar *constant-info-table*)

(definline get-constant-info0 (const)
  (gethash const *constant-info-table*))

(definline get-constant-info (const &optional (action 'error))
  (or (get-constant-info0 const)
      (init-constant-info const action)))

(defun init-constant-info (const action)
  (when action
    (can-be-constant-name const action))
  (let ((info (make-constant-info)))
    (setf (constant-info-number info) (funcall *standard-eql-numbering* :lookup const))
    (setf (gethash const *constant-info-table*) info)))

(defmacro define-constant-slot-accessor (name &key read-only)
  (let ((constant-slot (intern (format nil "~A-~A" :constant name) :snark))
        (constant-info-slot (intern (format nil "~A-~A" 'constant-info name) :snark)))
    `(progn
       (#-(or allegro lispworks) definline #+(or allegro lispworks) defun ,constant-slot (const)
         (,constant-info-slot (get-constant-info const)))
       ,@(unless read-only
           (list
            `(defun (setf ,constant-slot) (value const)
               (setf (,constant-info-slot (get-constant-info const)) value)))))))

(define-constant-slot-accessor number :read-only t)
(define-constant-slot-accessor hash-code :read-only t)
(define-constant-slot-accessor boolean-valued-p0)
(define-constant-slot-accessor constructor0)
(define-constant-slot-accessor magic)
(define-constant-slot-accessor allowed-in-answer)
(define-constant-slot-accessor kbo-weight)
(define-constant-slot-accessor weight)
(define-constant-slot-accessor constraint-theory)
(define-constant-slot-accessor sort)
(define-constant-slot-accessor plist)

(define-plist-slot-accessor constant :locked0)
(define-plist-slot-accessor constant :documentation)
(define-plist-slot-accessor constant :author)
(define-plist-slot-accessor constant :source)
(define-plist-slot-accessor constant :complement)	;complement of the symbol P is the symbol ~P
(define-plist-slot-accessor constant :skolem-p)
(define-plist-slot-accessor constant :created-p)
(define-plist-slot-accessor constant :do-not-resolve)

(definline builtin-constant-p (x)
  (or (numberp x) (stringp x)))

(definline constant-builtin-p (const)
  ;; equivalent to but faster than builtin-constant-p for known constants (can-be-constant-name is true)
  (not (symbolp const)))

(definline constant-boolean-valued-p (const)
  (and (not (constant-builtin-p const)) (constant-boolean-valued-p0 const)))

(definline constant-constructor (const)
  (or (constant-builtin-p const) (constant-constructor0 const)))

(definline constant-locked (const)
  (or (constant-builtin-p const) (constant-locked0 const)))

(definline constant-name (const)
  (or (constant-boolean-valued-p const) const))

(defun constant-name-lessp (x y)
  (cond
    ((complexp x)
     (if (complexp y) (or (< (realpart x) (realpart y)) (and (= (realpart x) (realpart y)) (< (imagpart x) (imagpart y)))) t))
    ((complexp y)
     nil)
    ((realp x)
     (if (realp y) (< x y) t))
    ((realp y)
     nil)
    ((stringp x)
     (if (stringp y) (string< x y) t))
    ((stringp y)
     nil)
    (t
     (string< x y))))

(defun initialize-constants ()
  (setf *constant-info-table* (make-hash-table))
  nil)

(defun map-constants (&optional function)
  (let ((result nil) result-last)
    (prog->
      (maphash *constant-info-table* ->* k v)
      (declare (ignore v))
      (if function (funcall function k) (collect k result)))
    result))

(defmacro set-slot-if-supplied (type slot)
  (let ((slot-supplied (intern (format nil "~A-~A" slot :supplied) :snark))
        (type-slot (intern (format nil "~A-~A" type slot) :snark)))
    `(when ,slot-supplied
       (setf (,type-slot symbol) ,slot))))

(defun declare-constant-symbol0 (symbol
                                 &key
                                 alias
                                 sort
                                 ((:locked locked0) nil locked0-supplied)
                                 (documentation nil documentation-supplied)
                                 (author nil author-supplied)
                                 (source nil source-supplied)
                                 (complement nil complement-supplied)
                                 (magic t magic-supplied)
                                 (skolem-p nil skolem-p-supplied)
                                 (created-p nil created-p-supplied)
                                 ((:constructor constructor0) nil constructor0-supplied)
                                 (allowed-in-answer nil allowed-in-answer-supplied)
                                 (kbo-weight nil kbo-weight-supplied)
                                 (weight nil weight-supplied)
                                 )
  ;; doesn't do anything if no keywords are supplied
  (when constructor0-supplied
    (cl:assert (implies (constant-builtin-p symbol) constructor0) () "Builtin constant ~S cannot be a nonconstructor." symbol))
  (when alias
    (create-aliases-for-symbol symbol alias))
  (when sort
    (declare-constant-sort symbol sort))
  (set-slot-if-supplied constant locked0)
  (set-slot-if-supplied constant documentation)
  (set-slot-if-supplied constant author)
  (set-slot-if-supplied constant source)
  (set-slot-if-supplied constant complement)
  (set-slot-if-supplied constant magic)
  (set-slot-if-supplied constant skolem-p)
  (set-slot-if-supplied constant created-p)
  (set-slot-if-supplied constant constructor0)
  (set-slot-if-supplied constant allowed-in-answer)
  (set-slot-if-supplied constant kbo-weight)
  (set-slot-if-supplied constant weight)
  symbol)

(defun changeable-keys-and-values0 (keys-and-values changeable)
  (let ((keys-and-values1 nil) keys-and-values1-last
        (keys-and-values2 nil) keys-and-values2-last)
    (loop
      (cond
       ((endp keys-and-values)
        (return (values keys-and-values1 keys-and-values2)))
       ((and (eq :locked (first keys-and-values)) (null (second keys-and-values)))
        (return (values (nconc keys-and-values1 keys-and-values2 (cddr keys-and-values)) nil)))
       ((member (first keys-and-values) changeable)
        (collect (pop keys-and-values) keys-and-values1)
        (collect (pop keys-and-values) keys-and-values1))
       (t
        (collect (pop keys-and-values) keys-and-values2)
        (collect (pop keys-and-values) keys-and-values2))))))

(defun changeable-keys-and-values (symbol keys-and-values changeable)
  (let (keys-and-values2)
    (multiple-value-setq (keys-and-values keys-and-values2) (changeable-keys-and-values0 keys-and-values changeable))
    (when keys-and-values2
      (warn "Ignoring redeclaration of locked symbol ~S with arguments~{ ~S~}." symbol keys-and-values2))
    keys-and-values))

(defun declare-constant-symbol1 (symbol keys-and-values changeable)
  (cond
   ((null keys-and-values)
    symbol)
   (t
    (apply 'declare-constant-symbol0
           symbol
           (cond
            ((constant-locked symbol)
             (changeable-keys-and-values symbol keys-and-values changeable))
            (t
             keys-and-values))))))

(defun declare-constant (name &rest keys-and-values)
  (declare (dynamic-extent keys-and-values))
  (declare-constant-symbol1 (input-constant-symbol name) keys-and-values (changeable-properties-of-locked-constant?)))

(defun declare-proposition (name &rest keys-and-values)
  (declare (dynamic-extent keys-and-values))
  (declare-constant-symbol1 (input-proposition-symbol name) keys-and-values (changeable-properties-of-locked-proposition?)))

;;; constants.lisp EOF
