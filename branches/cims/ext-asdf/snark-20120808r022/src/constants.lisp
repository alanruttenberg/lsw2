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
;;; Portions created by the Initial Developer are Copyright (C) 1981-2012.
;;; All Rights Reserved.
;;;
;;; Contributor(s): Mark E. Stickel <stickel@ai.sri.com>.

(in-package :snark)

;;; Lisp symbols, strings, numbers, and characters are used directly as SNARK constants
;;; but SNARK needs to associate information with them
;;; it is stored in constant-info structures found in *constant-info-table* hash-array
;;; or *number-info-table* or *string-info-table* in the case of numbers and strings
;;; that only require sort information be stored

(defstruct constant-info
  (hash-code0 (make-atom-hash-code) :read-only t)
  (boolean-valued-p0 nil)		;overloaded to be input name of the proposition
  (constructor0 nil)
  (magic t)				;nil means don't make magic-set goal for this proposition
  (allowed-in-answer0 t)
  (kbo-weight0 1)
  (weight0 1)
  (sort0 (top-sort))
  (plist nil))				;property list for more properties

(definline constant-number (const)
  (funcall *standard-eql-numbering* :lookup const))

(defvar *constant-info-table*)

(defmacro constant-info0 (const)
  `(gethash ,const *constant-info-table*))

(definline constant-info (const &optional (action 'error))
  (or (constant-info0 const)
      (init-constant-info const action)))

(defun init-constant-info (const action)
  (when action
    (can-be-constant-name const action))
  (constant-number const)		;initialize it at first occurrence
  (let ((info (make-constant-info)))
    (setf (constant-info0 const) info)))

(defmacro define-constant-slot-accessor (name &key read-only)
  (let ((constant-slot (intern (to-string :constant- name) :snark))
        (constant-info-slot (intern (to-string :constant-info- name) :snark)))
    `(progn
       (#-(or allegro lispworks) definline #+(or allegro lispworks) defun ,constant-slot (const)
         (,constant-info-slot (constant-info const)))
       ,@(unless read-only
           (list
            `(defun (setf ,constant-slot) (value const)
               (setf (,constant-info-slot (constant-info const)) value)))))))

(define-constant-slot-accessor hash-code0 :read-only t)
(define-constant-slot-accessor boolean-valued-p0)
(define-constant-slot-accessor constructor0)
(define-constant-slot-accessor magic)
(define-constant-slot-accessor allowed-in-answer0)
(define-constant-slot-accessor kbo-weight0)
(define-constant-slot-accessor weight0)
(define-constant-slot-accessor sort0)
(define-constant-slot-accessor plist)

(define-plist-slot-accessor constant :locked0)
(define-plist-slot-accessor constant :documentation)
(define-plist-slot-accessor constant :author)
(define-plist-slot-accessor constant :source)
(define-plist-slot-accessor constant :complement)	;complement of the symbol P is the symbol ~P
(define-plist-slot-accessor constant :skolem-p)
(define-plist-slot-accessor constant :created-p)
(define-plist-slot-accessor constant :do-not-resolve)

(defvar *number-info-table*)		;number -> (sort)
(defvar *string-info-table*)		;string -> (sort canonical-string)

(defstruct (number-info
            (:type list)
            (:copier nil))
  sort)

(defstruct (string-info
            (:type list)
            (:copier nil))
  sort
  (canonical nil :read-only t))

(defmacro number-info (number)
  `(gethash ,number *number-info-table*))

(defmacro string-info (string)
  `(gethash ,string *string-info-table*))

(defun number-canonical (x)
  (cl:assert (numberp x))
  (cond
   ((floatp x)
    (rationalize x))
   ((and (complexp x) (float (realpart x)))
    (complex (rationalize (realpart x)) (rationalize (imagpart x))))
   (t
    x)))

(defun declare-number (x)
  (setf x (number-canonical x))
  (or (number-info x)
      (progn
        (constant-number x)		;initialize it at first occurrence
        (setf (number-info x) (make-number-info :sort (the-sort (number-sort-name x))))))
  x)

(defun declare-string (x)
  (cl:assert (stringp x))
  ;; canonicalize strings so that (implies (string= str1 str2) (eq (declare-string str1) (declare-string str2)))
  (string-info-canonical
   (or (string-info x)
       (progn
         (constant-number x)		;initialize it at first occurrence
         (setf (string-info x) (make-string-info :sort (the-sort (declare-string-sort?)) :canonical x))))))

(definline builtin-constant-p (x)
  (or (numberp x) (stringp x)))

(definline constant-builtin-p (const)
  ;; equivalent to but faster than builtin-constant-p for known constants (can-be-constant-name is true)
  (not (symbolp const)))

(defun constant-hash-code (const)
  (if (constant-builtin-p const) (+ 2 (mod (constant-number const) 1022)) (constant-hash-code0 const)))

(definline constant-boolean-valued-p (const)
  (if (constant-builtin-p const) nil (constant-boolean-valued-p0 const)))

(definline constant-constructor (const)
  (if (constant-builtin-p const) t (constant-constructor0 const)))

(definline constant-allowed-in-answer (const)
  (if (constant-builtin-p const) t (constant-allowed-in-answer0 const)))

(definline constant-kbo-weight (const)
  (if (constant-builtin-p const)
      (let ((v (kbo-builtin-constant-weight?)))
        (if (numberp v) v (funcall v const)))
      (constant-kbo-weight0 const)))

(definline constant-weight (const)
  (if (constant-builtin-p const)
      (let ((v (builtin-constant-weight?)))
        (if (numberp v) v (funcall v const)))
      (constant-weight0 const)))

(defun constant-sort (const)
  (cond
   ((numberp const)
    (number-info-sort (number-info const)))
   ((stringp const)
    (string-info-sort (string-info const)))
   (t
    (constant-sort0 const))))

(defun (setf constant-sort) (value const)
  (cond
   ((numberp const)
    (setf (number-info-sort (number-info const)) value))
   ((stringp const)
    (setf (string-info-sort (string-info const)) value))
   (t
    (setf (constant-sort0 const) value))))

(definline constant-locked (const)
  (if (constant-builtin-p const) t (constant-locked0 const)))

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
  (setf *number-info-table* (make-hash-table))
  (setf *string-info-table* (make-hash-table :test #'equal))
  nil)

(defmacro set-slot-if-supplied (type slot)
  (let ((slot-supplied (intern (to-string slot :-supplied) :snark))
        (type-slot (intern (to-string type "-" slot) :snark)))
    `(when ,slot-supplied
       (setf (,type-slot symbol) ,slot))))

(defun declare-constant-symbol0 (symbol
                                 &key
                                 alias
                                 ((:sort sort0) nil)
                                 ((:locked locked0) nil)
                                 (documentation nil documentation-supplied)
                                 (author nil author-supplied)
                                 (source nil source-supplied)
                                 (complement nil complement-supplied)
                                 (magic t magic-supplied)
                                 (skolem-p nil skolem-p-supplied)
                                 (created-p nil created-p-supplied)
                                 ((:constructor constructor0) nil constructor0-supplied)
                                 ((:allowed-in-answer allowed-in-answer0) nil allowed-in-answer0-supplied)
                                 ((:kbo-weight kbo-weight0) nil kbo-weight0-supplied)
                                 ((:weight weight0) nil weight0-supplied)
                                 (do-not-resolve nil do-not-resolve-supplied)
                                 )
  ;; doesn't do anything if no keywords are supplied
  (when constructor0-supplied
    (cl:assert (implies (constant-builtin-p symbol) constructor0) () "Builtin constant ~S cannot be a nonconstructor." symbol))
  (when alias
    (create-aliases-for-symbol symbol alias))
  (when sort0
    (declare-constant-sort symbol sort0))
  (when locked0
    (setf (constant-locked0 symbol) locked0))		;stays locked
  (set-slot-if-supplied constant documentation)
  (set-slot-if-supplied constant author)
  (set-slot-if-supplied constant source)
  (set-slot-if-supplied constant complement)
  (set-slot-if-supplied constant magic)
  (set-slot-if-supplied constant skolem-p)
  (set-slot-if-supplied constant created-p)
  (set-slot-if-supplied constant constructor0)
  (set-slot-if-supplied constant allowed-in-answer0)
  (set-slot-if-supplied constant kbo-weight0)
  (set-slot-if-supplied constant weight0)
  (set-slot-if-supplied constant do-not-resolve)
  symbol)

(defun changeable-keys-and-values0 (keys-and-values changeable)
  (let ((keys-and-values1 nil) keys-and-values1-last
        (keys-and-values2 nil) keys-and-values2-last)
    (loop
      (cond
       ((endp keys-and-values)
        (return (values keys-and-values1 keys-and-values2)))
       ((member (first keys-and-values) changeable)
        (collect (pop keys-and-values) keys-and-values1)
        (collect (pop keys-and-values) keys-and-values1))
       (t
        (collect (pop keys-and-values) keys-and-values2)
        (collect (pop keys-and-values) keys-and-values2))))))

(defun changeable-keys-and-values (symbol keys-and-values changeable)
  (let (keys-and-values2)
    (setf (values keys-and-values keys-and-values2) (changeable-keys-and-values0 keys-and-values changeable))
    (when keys-and-values2
      (warn "Ignoring declaration of locked symbol ~S with arguments~{ ~S~}." symbol keys-and-values2))
    keys-and-values))

(defun declare-constant-symbol1 (symbol keys-and-values)
  (cond
   ((null keys-and-values)
    symbol)
   (t
    (apply 'declare-constant-symbol0
           symbol
           (cond
            ((and (constant-locked symbol) (eq none (getf keys-and-values :locked none)))
             (changeable-keys-and-values
              symbol
              keys-and-values
              (if (constant-builtin-p symbol) '(:alias :sort) (changeable-properties-of-locked-constant?))))
            (t
             keys-and-values))))))

(defun declare-constant (name &rest keys-and-values)
  (declare (dynamic-extent keys-and-values))
  (declare-constant-symbol1 (input-constant-symbol name) keys-and-values))

(defun declare-proposition (name &rest keys-and-values)
  (declare (dynamic-extent keys-and-values))
  (declare-constant-symbol1 (input-proposition-symbol name) keys-and-values))

;;; constants.lisp EOF
