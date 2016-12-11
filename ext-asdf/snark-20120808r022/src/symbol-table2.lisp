;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: symbol-table2.lisp
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

(declaim (special *input-wff*))

;;; identical names in different packages yield different symbols
;;; logical symbols, equality relation, etc., are in SNARK package
;;;
;;; builtin constants (numbers and strings) are not stored in the symbol table

(defun make-symbol-table ()
  (setf *symbol-table* (make-hash-table))
  nil)

(defmacro symbol-table-entries (name)
  `(gethash ,name *symbol-table*))

(defun create-symbol-table-entry (name symbol)
  (pushnew symbol (symbol-table-entries name))
  symbol)

(defun find-symbol-table-entry (name kind &optional arity)
;;(cl:assert (implies (eq :logical-symbol kind) (eq :any arity)))
  (dolist (symbol (symbol-table-entries name) none)
    (when (symbol-table-kind-match symbol kind arity)
      (return symbol))))

(defun find-or-create-symbol-table-entry (name kind &optional arity (sym none))
;;(cl:assert (implies (eq :logical-symbol kind) (eq :any arity)))
  (let ((symbol (find-symbol-table-entry name kind arity)))
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
       (t
        (ecase kind
          (:variable
           (setf symbol (make-variable none)))		;declare-variable replaces none by proper sort
          (:constant
           (setf symbol name)
           (constant-info symbol nil))
          (:proposition
           (setf symbol
                 (cond
                  ((eq 'true name)			;use value of lisp defconstants true and false to represent truth values
                   true)
                  ((eq 'false name)
                   false)
                  (t
                   (make-symbol (symbol-name name)))))
           (constant-info symbol nil)
           (setf (constant-boolean-valued-p0 symbol) name))
          (:function
           (setf symbol (make-function-symbol name arity)))
          (:relation
           (setf symbol (make-function-symbol name arity))
           (setf (function-boolean-valued-p symbol) t))
          (:logical-symbol
           (setf symbol (make-function-symbol name :any))
           (setf (function-boolean-valued-p symbol) t)
           (setf (function-logical-symbol-p symbol) name)))))
      (prog->
        (dolist (symbol-table-entries name) ->* symbol2)
        (symbol-kind symbol2 -> kind2 arity2)
        (cond
         ((or (and (eq kind kind2)
                   (naturalp arity2)			;function or relation already declared with fixed arity
                   (not (naturalp arity))		;now with special (e.g., :any) arity
                   (ecase arity (:any t)))
              (and (eq :relation kind) (eq :logical-symbol kind2))
              (and (eq :logical-symbol kind) (eq :relation kind2)))
          (with-standard-io-syntax2
            (error "~S cannot be used as a ~@[~A-ary ~]~A; it is a ~@[~A-ary ~]~A."
                   name (if (eq :logical-symbol kind) nil arity) kind  (if (eq :logical-symbol kind2) nil arity2) kind2)))
         ((and (print-symbol-table-warnings?)
               (or (eq :all (print-symbol-table-warnings?))
                   (and (or (eq :function kind) (eq :relation kind) (eq :logical-symbol kind))
                        (or (eq :function kind2) (eq :relation kind2) (eq :logical-symbol kind2)))
                   (and (eq :constant kind) (eq :variable kind2))
                   (and (eq :variable kind) (eq :constant kind2))))
          (with-standard-io-syntax2
            (warn "~S is being used as a ~@[~A-ary ~]~A~@[ in ~S~]; it is also a ~@[~A-ary ~]~A."
                  name (if (eq :logical-symbol kind) nil arity) kind  *input-wff* (if (eq :logical-symbol kind2) nil arity2) kind2)))))
      (create-symbol-table-entry name symbol)
      (values symbol t)))))

(defun create-aliases-for-symbol (symbol aliases)
  (mvlet (((values kind arity) (symbol-kind symbol)))
    (dolist (alias (mklist aliases))
      (ecase kind
        (:function (can-be-function-name alias 'error))
        (:relation (can-be-relation-name alias 'error))
        (:constant (can-be-constant-alias alias 'error))
        (:proposition (can-be-proposition-name alias 'error))
        (:logical-symbol (can-be-logical-symbol-name alias 'error))
        (:sort (can-be-sort-name alias 'error)))
      (find-or-create-symbol-table-entry alias kind arity symbol))))

(defun rename-function-symbol (symbol new-name)
  (create-aliases-for-symbol symbol new-name)
  (setf (function-name symbol) new-name)
  (setf (function-code-name0 symbol) nil))

(defun symbol-kind (x)
  (cond
   ((function-symbol-p x)
    (values (function-kind x) (function-arity x)))
   ((variable-p x)
    :variable)
   ((sort? x)
    :sort)
   ((constant-boolean-valued-p x)
    :proposition)
   (t
    :constant)))

(defun symbol-table-kind-match (symbol2 kind arity)
  ;; can existing symbol2 be used as a kind/arity symbol
  (mvlet (((values kind2 arity2) (symbol-kind symbol2)))
    (and (eq kind kind2)
         (or (eql arity arity2)
             (case arity2
               (:any
                (or (eq :any arity) (naturalp arity)))
               (2
                (and (function-associative symbol2) (or (eq :any arity) (naturalp arity))))
               (otherwise
                nil))))))

(defun symbol-table-constant? (name)
  (remove-if-not #'(lambda (x) (eq :constant (symbol-kind x))) (symbol-table-entries name)))

(defun symbol-table-function? (name)
  (remove-if-not #'(lambda (x) (eq :function (symbol-kind x))) (symbol-table-entries name)))

(defun symbol-table-relation? (name)
  (remove-if-not #'(lambda (x) (eq :relation (symbol-kind x))) (symbol-table-entries name)))

(defun map-symbol-table (cc &key logical-symbols variables)
  (prog->
    (maphash *symbol-table* ->* name entries)
    (dolist entries ->* symbol)
    (symbol-kind symbol -> kind)
    (when (case kind
            (:variable variables)
            (:logical-symbol logical-symbols)
            (:proposition (implies (not logical-symbols) (not (or (eq true symbol) (eq false symbol)))))
            (otherwise t))
      (funcall cc name kind symbol))))

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

(defun print-symbol-table (&key logical-symbols variables)
  (with-standard-io-syntax2
    (labels
      ((print-aliases (symbol)
         (let ((aliases (symbol-aliases symbol)))
           (when aliases
             (format t "~35T (alias ~S~{, ~S~})" (first aliases) (rest aliases)))))
       (print-symbols1 (list kind)
         (when list
           (let ((len (length list)))
             (format t "~%~D ~(~A~)~P:" len kind len))
           (dolist (symbol (sort list #'function-name-arity-lessp))
             (format t "~%    ~S~26T" symbol)
             (let ((arity (function-arity symbol)))
               (unless (member arity '(:any))
                 (format t " ~A-ary" arity)))
             (when (function-macro symbol)
               (format t " macro"))
             (print-aliases symbol))))
       (print-symbols2 (list kind orderfn)
         (when list
           (let ((len (length list)))
             (format t "~%~D ~(~A~)~P:" len kind len))
           (dolist (symbol (sort list orderfn))
             (cond
              ((or (eq :constant kind) (eq :proposition kind))
               (format t "~%    ~S" (constant-name symbol))
               (print-aliases symbol))
              ((eq :sort kind)
               (format t "~%    ~S" (sort-name symbol))
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
          (map-symbol-table :logical-symbols logical-symbols :variables variables ->* name kind symbol)
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
               (push symbol list-of-sorts)))
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
        (print-symbols2 list-of-sorts :sort #'(lambda (x y) (string< (sort-name x) (sort-name y))))
        (print-symbols2 list-of-propositions :proposition #'constant-name-lessp)
        (print-symbols2 list-of-constants :constant #'constant-name-lessp)
        (print-symbols1 list-of-functions :function)
        (print-symbols1 list-of-relations :relation)
        (when ambiguous
          (format t "~%~D symbol~:P with multiple meanings:" (length ambiguous))
          (dolist (symbol (sort ambiguous #'string<))
            (format t "~%    ~S" symbol)))
        nil))))

(defun symbol-to-name (x)
  (cond
   ((function-symbol-p x)
    (function-name x))
   ((sort? x)
    (sort-name x))
   (t
    (constant-name x))))

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

(defun the-function-symbol (name arity &optional kind)
  (let ((symbol (find-symbol-table-entry name (or kind :function) arity)))
    (cl:assert (neq none symbol))
    symbol))

(defun current-function-name (name arity &optional kind)
  (function-name (the-function-symbol name arity (or kind :function))))

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
   ((or (eq true name) (eq false name) (function-symbol-p name))
    name)						;already in internal format
   (t
    (can-be-constant-or-function-name name 'error)
    (let ((found nil))
      (prog->
        (dolist (symbol-table-entries name) ->* symbol)
        (symbol-kind symbol -> kind)
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
  (let ((quoted (and (consp name) (eq '$$quote (first name)) (rest name) (null (rrest name)))))
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

(defun input-function-symbol (name arity &optional rel)
  ;; find or create a function (or relation) symbol with the given name and arity
  (cond
   ((function-symbol-p name)
    ;; generalize by allowing name to be a function (or relation) symbol of correct arity
    (cl:assert (and (function-has-arity-p name arity) (iff (function-boolean-valued-p name) rel)))
    name)
   (t
    (can-be-function-name name 'error)
    (find-or-create-symbol-table-entry name (if rel :relation :function) arity))))

(defun input-relation-symbol (name arity)
  ;; find or create a relation symbol with the given name and arity
  (input-function-symbol name arity t))

(defun input-logical-symbol (name &optional create-if-does-not-exist)
  (cond
   (create-if-does-not-exist
    (can-be-logical-symbol-name name 'error)
    (find-or-create-symbol-table-entry name :logical-symbol :any))
   (t
    (find-symbol-table-entry name :logical-symbol :any))))

(defun expr-arity (x)
  ;; used by input-wff etc. to count arguments of nonatomic expression
  (list-p (rest x)))

(defun input-head-function-symbol (term)
  (input-function-symbol (first term) (expr-arity term)))

(defun input-head-relation-symbol (wff)
  (input-relation-symbol (first wff) (expr-arity wff)))

;;; symbol-table2.lisp EOF
