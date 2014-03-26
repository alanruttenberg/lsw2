;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: symbol-table.lisp
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

(defvar *symbol-table*)
(defvar *string-table*)				;canonicalized strings

(declaim (special *input-wff*))

;;; identical names in different packages yield different symbols
;;; logical symbols, equality relation, etc., are in SNARK package

;;; a symbol-table where
;;;  each row index is a name (encoded as an integer)
;;;  each column index is a type (encoded as an integer)
;;;  the value at row*column is the internal symbol

;;; sparse-matrix representation instead of hash-table ensures mapping order is fixed
;;; however, map-constants for numbers, etc. does use a hash-table and mapping order is not fixed

(defun make-symbol-table ()
  (setf *symbol-table* (make-sparse-matrix :rows t :default-value none))
  (setf *string-table* (make-hash-table :test #'equal))
  nil)

(defun create-symbol-table-entry (name# type# symbol)
  (setf (sparef *symbol-table* name# type#) symbol))

(defun find-symbol-table-entries* (name#)
  (sparse-matrix-row *symbol-table* name#))

(defun find-symbol-table-entries (name)
  (let ((name# (funcall *standard-eql-numbering* :lookup? name)))
    (if name# (find-symbol-table-entries* name#) nil)))

(defun find-symbol-table-entry* (name# type# &optional delete)
  (let ((entries (find-symbol-table-entries* name#)))
    (cond
     ((null entries)
      none)
     (t
      (let ((symbol (sparef entries type#)))
        (cond
         ((neq none symbol)
          (when delete
            (setf (sparef entries type#) none))
          symbol)
         (t
          (prog->
            (map-sparse-vector-with-indexes entries ->* symbol2 type2#)
            (when (symbol-table-type-match type# type2# symbol2)
              (when delete
                (setf (sparef entries type2#) none))
              (return-from find-symbol-table-entry* symbol2)))
          none)))))))

(defun find-symbol-table-entry (name kind &optional arity delete)
  (let ((name# (funcall *standard-eql-numbering* :lookup? name)))
    (if name# (find-symbol-table-entry* name# (encode-symbol-table-type kind arity) delete) none)))

(defun find-or-create-symbol-table-entry (name kind &optional arity (sym none))
  (let* ((name# (funcall *standard-eql-numbering* :lookup name))
         (type# (encode-symbol-table-type kind arity))
         (symbol (find-symbol-table-entry* name# type#)))
    (cond
     ((neq none symbol)
      (when (and (neq none sym) (neql sym symbol))
        (with-standard-io-syntax2
          (error "~S cannot be used as ~A name or alias of ~S; it is a ~A name or alias of ~S." name kind sym kind symbol)))
      symbol)
     (t
      (cond
       ((neq none sym)
        (setf symbol sym))
       ((eq :variable kind)
        (setf symbol (make-variable none)))		;declare-variable replaces none by proper sort
       ((eq :sort kind)
        (setf symbol name))
       ((eq :constant kind)
        (setf symbol name)
        (get-constant-info symbol nil))
       ((eq :proposition kind)
        ;; use value of lisp defconstants true and false to represent true and false truth values
        (setf symbol
              (cond
               ((eq 'true name)
                true)
               ((eq 'false name)
                false)
               (t
                (make-symbol (symbol-name name)))))
        (get-constant-info symbol nil)
        (setf (constant-boolean-valued-p0 symbol) name))
       ((or (eq :function kind) (eq :relation kind))
        (setf symbol (make-function-symbol name arity))
        (when (eq :relation kind)
          (setf (function-boolean-valued-p symbol) t)))
       ((eq :logical-symbol kind)
        (setf symbol (make-function-symbol name :any))
        (setf (function-boolean-valued-p symbol) t)
        (setf (function-logical-symbol-p symbol) name)))
      (prog->
        (map-sparse-vector-with-indexes (find-symbol-table-entries* name#) ->* symbol2 type2#)
        (declare (ignore symbol2))
        (decode-symbol-table-type type2# -> kind2 arity2)
        (cond
         ((or (and (eq kind kind2)
                   (naturalp arity2)			;function or relation already declared with fixed arity
                   (not (naturalp arity))		;now with special (e.g., :any) arity
                   (ecase arity (:any t)))
              (and (eq :relation kind)
                   (eq :logical-symbol kind2)))
          (with-standard-io-syntax2
            (error "~S cannot be used as a ~@[~A-ary ~]~A; it is a ~@[~A-ary ~]~A." name arity kind arity2 kind2)))
         ((and (print-symbol-table-warnings?)
               (or (eq :all (print-symbol-table-warnings?))
                   (and (or (eq :function kind) (eq :relation kind))
                        (or (eq :function kind2) (eq :relation kind2) (eq :logical-symbol kind2)))
                   (and (eq :constant kind) (eq :variable kind2))
                   (and (eq :variable kind) (eq :constant kind2))))
          (with-standard-io-syntax2
            (warn "~S is being used as a ~@[~A-ary ~]~A~@[ in ~S~]; it is also a ~@[~A-ary ~]~A." name arity kind *input-wff* arity2 kind2)))))
      (create-symbol-table-entry name# type# symbol)
      (values symbol t)))))

(defun create-aliases-for-symbol (symbol aliases)
  ;; only for functions and constants
  (setf aliases (if (listp aliases) (reverse aliases) (list aliases)))
  (let* ((functionp (function-symbol-p symbol))
         (booleanp (if functionp
                       (function-boolean-valued-p symbol)
                       (constant-boolean-valued-p symbol)))
         (kind (if functionp
                   (if booleanp :relation :function)
                   (if booleanp :proposition :constant)))
         (name (if functionp
                   (function-name symbol)
                   (constant-name symbol)))
         (arity (if functionp (function-arity symbol) nil)))
    (dolist (alias aliases)
      (unless (eql name alias)
        (if functionp
            (if booleanp
                (can-be-relation-name alias 'error)
                (can-be-function-name alias 'error))
            (if booleanp
                (can-be-proposition-name alias 'error)
                (can-be-constant-alias alias 'error)))
        (find-or-create-symbol-table-entry alias kind arity symbol)))))

(defun rename-function-symbol (symbol new-name)
  (let ((name (function-name symbol)))
    (unless (eql name new-name)
      (let ((booleanp (function-boolean-valued-p symbol)))
        (if booleanp
            (can-be-relation-name new-name 'error)
            (can-be-function-name new-name 'error))
        (let ((kind (if booleanp :relation :function))
              (arity (function-arity symbol)))
          (find-or-create-symbol-table-entry new-name kind arity symbol)
          (setf (function-name symbol) new-name)
          (setf (function-code-name0 symbol) nil))))))

(defun input-symbol (name &key macro)
  ;; return SNARK symbol whose name is name
  ;; primary usage is for term ordering declarations
  ;; special handling for true and false
  ;;   accept as input the internal symbols for true and false
  ;;   if name is 'true or 'false, return the constant true or false if there is one; otherwise return the proposition
  (cond
   ((numberp name)
    (declare-number name))
   ((stringp name)
    (declare-string name))
   ((or (eq true name) (eq false name))
    name)						;already in internal format
   (t
    (can-be-constant-or-function-name name 'error)
    (let ((found nil))
      (prog->
        (map-sparse-vector-with-indexes (find-symbol-table-entries name) ->* symbol type#)
        (decode-symbol-table-type type# -> kind arity)
        (declare (ignore arity))
        (cond
         ((or (eq :sort kind) (eq :variable kind))
          )
         ((and (not macro) (function-symbol-p symbol) (function-macro symbol))
          )
         (found
          (cond
           ((and (or (eq 'true name) (eq 'false name)) (eq :proposition kind) (eq :constant (first found)))
            )
           ((and (or (eq 'true name) (eq 'false name)) (eq :constant kind) (eq :proposition (first found)))
            (setf found (cons kind symbol)))
           (t
            (error "There is more than one entry for ~S in symbol table." name))))
         (t
          (setf found (cons kind symbol)))))
      (cond
       ((null found)
        (error "Couldn't find ~S in symbol table." name))
       (t
        (cdr found)))))))

(defun input-constant-symbol (name)
  (let ((quoted (and (consp name) (eq '$$quote (first name)) (rest name) (null (rest (rest name))))))
    (when quoted
      (setf name (second name)))
    (cond
     ((numberp name)
      (declare-number name))
     ((stringp name)
      (declare-string name))
     (t
      (unless (and quoted (atom name))
        (can-be-constant-name name 'error))
      (find-or-create-symbol-table-entry name :constant)))))

(defun input-proposition-symbol (name)
  (cond
   ((or (eq true name) (eq false name))			;allow internal true and false values in input
    name)						;they are already in internal format
   (t
    (can-be-proposition-name name 'error)
    (find-or-create-symbol-table-entry name :proposition))))

(defun input-function-symbol1 (symbol new)
  (cond
   (new
    (values symbol new))
   (t
    symbol)))

(defun input-function-symbol (name arity)
  ;; find or create a function symbol with the right name and arity
  (prog-> 
    (can-be-function-name name 'error)
    (find-or-create-symbol-table-entry name :function arity -> symbol new)
    (input-function-symbol1 symbol new)))

(defun input-relation-symbol (name arity)
  ;; find or create a relation symbol with the right name and arity
  (prog->
    (can-be-relation-name name 'error)
    (find-or-create-symbol-table-entry name :relation arity -> symbol new)
    (input-function-symbol1 symbol new)))

(defun input-logical-symbol (name &optional create-if-does-not-exist)
  (cond
   (create-if-does-not-exist
    (prog->
      (can-be-logical-symbol-name name 'error)
      (find-or-create-symbol-table-entry name :logical-symbol -> symbol new)
      (input-function-symbol1 symbol new)))
   (t
    (find-symbol-table-entry name :logical-symbol))))

(defun expr-arity (x)
  ;; used by input-wff etc. to count arguments of nonatomic expression
  (list-p (rest x)))

(defun input-head-function-symbol (term)
  (input-function-symbol (first term) (expr-arity term)))

(defun input-head-relation-symbol (wff)
  (input-relation-symbol (first wff) (expr-arity wff)))

(defun input-variable (name)
  (can-be-variable-name name 'error)
  (find-or-create-symbol-table-entry name :variable))

(defun encode-symbol-table-type (kind &optional arity)
  (ecase kind
    (:variable       -1)
    (:sort           -2)
    (:logical-symbol -3)
    (:constant       99)
    (:proposition   -99)
    ((:function :relation)
     (case arity
       (:any      (if (eq :function kind) 98 -98))
       (otherwise (cl:assert (naturalp arity)) (if (eq :function kind) (+ 100 arity) (- -100 arity)))))))

(defun decode-symbol-table-type (type#)
  (case type#
    ( -1 :variable)
    ( -2 :sort)
    ( -3 :logical-symbol)
    ( 99 :constant)
    (-99 :proposition)
    ( 98 (values :function :any))
    (-98 (values :relation :any))
    (t
     (cl:assert (and (integerp type#) (<= 100 (abs type#))) () "~S is not an encoded symbol-table-type." type#)
     (if (< 0 type#)
         (values :function (- type# 100))
         (values :relation (- -100 type#))))))

(defun symbol-table-type-match (type# type2# symbol2)
  ;; type# is type of new symbol, type2# is type of exisiting symbol2
  ;; can existing symbol represent the new symbol?
  (or (= type# type2#)
      (multiple-value-bind (kind arity) (decode-symbol-table-type type#)
        (and (naturalp arity)			;function or relation with specific arity
             (multiple-value-bind (kind2 arity2) (decode-symbol-table-type type2#)
               (and (eq kind kind2)
                    (or (and (not (naturalp arity2))
                             (ecase arity2 ((:any) t)))
                        (function-associative symbol2))))))))

(defun map-symbol-table (cc &key logical-symbols variables numbers strings)
  (prog->
    (map-sparse-vector-with-indexes (sparse-matrix-rows *symbol-table*) ->* row name#)
    (funcall *standard-eql-numbering* :inverse name# -> name)
    (map-sparse-vector-with-indexes row ->* symbol type#)
    (decode-symbol-table-type type# -> kind arity)
    (declare (ignore arity))
    (when (case kind
            (:variable variables)
            (:logical-symbol logical-symbols)
            (:proposition (implies (not logical-symbols) (not (or (eq true symbol) (eq false symbol)))))
            (otherwise t))
      (funcall cc name kind symbol)))
  ;; numbers and strings aren't in *symbol-table*
  (when (or numbers strings)
    (prog->
      (map-constants ->* symbol)
      (when (or (and numbers (numberp symbol))
                (and strings (stringp symbol)))
        (funcall cc symbol :constant symbol)))))

(defun symbol-table-entries (name)
  (cond
   ((builtin-constant-p name)
    (if (get-constant-info0 name)
        (list (list name :constant))
        nil))
   (t
    (let ((result nil) result-last)
      (prog->
        (map-sparse-vector-with-indexes (find-symbol-table-entries name) ->* symbol type#)
        (decode-symbol-table-type type# -> kind arity)
        (collect (if arity (list symbol kind arity) (list symbol kind)) result))
      result))))

(defun symbol-table-constant? (name)
  (delete-if (lambda (x) (not (eq :constant (second x)))) (symbol-table-entries name)))

(defun symbol-table-function? (name)
  (delete-if (lambda (x) (not (eq :function (second x)))) (symbol-table-entries name)))

(defun symbol-table-relation? (name)
  (delete-if (lambda (x) (not (eq :relation (second x)))) (symbol-table-entries name)))

(defun symbol-aliases (symbol)
  ;; slow
  (let ((aliases nil))
    (prog->
      (symbol-to-name symbol -> name)
      (map-symbol-table :logical-symbols t :variables nil ->* name2 kind2 symbol2)
      (declare (ignore kind2))
      (when (eql symbol symbol2)
        (unless (eql name name2)
          (push name2 aliases))))
    (sort aliases #'string< :key #'symbol-name)))

(defun print-symbol-table (&key logical-symbols variables numbers strings)
  (with-standard-io-syntax2
    (labels
      ((print-aliases (symbol)
         (let ((aliases (symbol-aliases symbol)))
           (when aliases
             (format t "~35T (alias ~S~{, ~S~})" (first aliases) (rest aliases)))))
       (print-symbols1 (list kind)
         (when list
           (let ((len (length list)))
             (format t "~%~D ~A~P:" len kind len))
           (dolist (symbol (sort list #'function-name-lessp :key #'function-name))
             (format t "~%    ~S~26T" symbol)
             (let ((arity (function-arity symbol)))
               (unless (member arity '(:any))
                 (format t " ~A-ary" arity)))
             (when (function-macro symbol)
               (format t " macro"))
             (print-aliases symbol)
;;           (when (rest (function-sort-declarations symbol))
;;             (format t " with multiple sorts"))
             )))
       (print-symbols2 (list kind orderfn)
         (when list
           (let ((len (length list)))
             (format t "~%~D ~A~P:" len kind len))
           (dolist (symbol (sort list orderfn))
             (cond
              ((or (eq :constant kind) (eq :proposition kind))
               (format t "~%    ~S" (constant-name symbol))
               (print-aliases symbol))
              (t
               (format t "~%    ~S" symbol)))))))
      (let ((list-of-variables nil)
	    (list-of-sorts nil)
	    (list-of-constants nil)
	    (list-of-propositions nil)
	    (list-of-functions nil)
	    (list-of-relations nil)
	    (list-of-logical-symbols nil)
	    (ambiguous nil))
        (prog->
          (identity none -> previous-name)
          (map-symbol-table :logical-symbols logical-symbols :variables variables :numbers numbers :strings strings ->* name kind symbol)
          (cond
           ((neql previous-name name)
            (setf previous-name name))
           ((or (null ambiguous) (neql name (first ambiguous)))
            (push name ambiguous)))
          (ecase kind
            (:variable
             (push name list-of-variables))
            (:sort
             (when (eq name (sort-name symbol))
               (push name list-of-sorts)))
            (:constant
             (when (eql name (constant-name symbol))
               (push symbol list-of-constants)))
            (:proposition
             (when (eq name (constant-name symbol))
               (push symbol list-of-propositions)))
            (:function
             (when (eq name (function-name symbol))
               (push symbol list-of-functions)))
            (:relation
             (when (eq name (function-name symbol))
               (push symbol list-of-relations)))
            (:logical-symbol
             (when (eq name (function-name symbol))
               (push symbol list-of-logical-symbols)))))
        (print-symbols1 list-of-logical-symbols :logical-symbol)
        (print-symbols2 list-of-variables :variable #'string<)
        (print-symbols2 list-of-sorts :sort #'string<)
        (print-symbols2 list-of-propositions :proposition #'constant-name-lessp)
        (print-symbols2 list-of-constants :constant #'constant-name-lessp)
        (print-symbols1 list-of-functions :function)
        (print-symbols1 list-of-relations :relation)
        (when ambiguous
          (format t "~%~D symbol~:P with multiple meanings:" (length ambiguous))
          (dolist (symbol (sort ambiguous #'string<))
            (format t "~%    ~S" symbol)))
        nil))))

(defun symbol-boolean-valued-p (x)
  (if (function-symbol-p x)
      (function-boolean-valued-p x)
      (constant-boolean-valued-p x)))

(defun symbol-number (x)
  (if (function-symbol-p x)
      (function-number x)
      (constant-number x)))

(definline symbol-numbered (n)
  (funcall *standard-eql-numbering* :inverse n))

;;; symbol-table.lisp EOF
