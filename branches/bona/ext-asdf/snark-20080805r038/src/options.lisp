;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: options.lisp
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

(declaim (special *snark-globals* *agenda-of-rows-to-give* *agenda-of-rows-to-process*))

(defvar *snark-options* nil)

(defmacro declare-snark-option (name &optional (default-value nil) (invisible-value :always-print))
  ;; example:
  ;; (declare-snark-option USE-FOO t)
  ;; yields the functions USE-FOO, DEFAULT-USE-FOO, USE-FOO?
  ;;
  ;; (USE-FOO value)  sets the value of the USE-FOO option
  ;; (USE-FOO)        sets the value of the USE-FOO option to T
  ;;
  ;; (DEFAULT-USE-FOO value)  sets the default value of the USE-FOO option
  ;; (DEFAULT-USE-FOO)        sets the default value of the USE-FOO option to T
  ;;
  ;; (USE-FOO?)          returns the value of the USE-FOO option
  ;; (DEFAULT-USE-FOO?)  returns the default value of the USE-FOO option
  ;;
  ;; (initialize) will initialize options to their default values
  ;;
  ;; DEFAULT-USE-FOO should be used BEFORE initialize to establish a
  ;; default value for foo for all future runs; USE-FOO should be used
  ;; AFTER initialize to change the value of foo for an individual run
  ;;
  ;; (print-options) will print the value of each SNARK option
  ;; whose value differs from its invisible value (:always-print
  ;; or :never-print can be specified instead of an invisible value)
  (cl:assert (or (symbolp name) (stringp name)))
  (setf name (intern (string name) :snark))
  (let ((snark-option-variable-name (intern (format nil "*%~A%*" name) :snark))
        (default-snark-option-variable-name (intern (format nil "*%~A-~A%*" :default name) :snark))
        (invisible-snark-option-variable-name (intern (format nil "*%~A-~A%*" :invisible name) :snark))
        (snark-option-access-function-name (intern (format nil "~A?" name) :snark))
        (default-snark-option-function-name (intern (format nil "~A-~A" :default name) :snark))
        (default-snark-option-access-function-name (intern (format nil "~A-~A?" :default name) :snark)))
  `(progn
     (unless (member ',name *snark-options*)
       (setf *snark-options* (nconc *snark-options* (list ',name)))
       (nconc *snark-globals*
              (list ',snark-option-variable-name))
       (nconc *snark-nonsave-globals*
              (list ',default-snark-option-variable-name
                    ',invisible-snark-option-variable-name)))

     (eval-when (:compile-toplevel :load-toplevel :execute)
       (export '(,default-snark-option-access-function-name
		 ,default-snark-option-function-name
		 ,snark-option-access-function-name
		 ,name)
	       :snark))
     
     (defparameter ,default-snark-option-variable-name ,default-value)
     
     (defparameter ,invisible-snark-option-variable-name ,invisible-value)
     
     (defvar ,snark-option-variable-name ,default-snark-option-variable-name)
     
     (defun ,default-snark-option-access-function-name ()
       ,default-snark-option-variable-name)

     (defun ,default-snark-option-function-name (&optional (value t))
       (setf ,default-snark-option-variable-name value))	;affects only future runs
     
     (definline ,snark-option-access-function-name ()
       ,snark-option-variable-name)
     
     (defgeneric ,name (&optional value)
       (:method (&optional (value t))
         (setf ,snark-option-variable-name value))))))

(declare-snark-option variable-symbol-prefixes '(#\?) :never-print)	;use first for output, any for input

(declare-snark-option use-resolution nil)
(declare-snark-option use-hyperresolution nil)
(declare-snark-option use-negative-hyperresolution nil)
(declare-snark-option use-ur-resolution nil)
(declare-snark-option use-ur-pttp nil)
(declare-snark-option use-paramodulation nil)
(declare-snark-option use-factoring nil)
(declare-snark-option use-equality-factoring nil)
(declare-snark-option use-condensing t)
(declare-snark-option use-resolve-code nil)			;list of resolve-code functions

(declare-snark-option use-unit-restriction nil)
(declare-snark-option use-input-restriction nil)
(declare-snark-option use-literal-ordering-with-resolution nil)
(declare-snark-option use-literal-ordering-with-hyperresolution nil)
(declare-snark-option use-literal-ordering-with-negative-hyperresolution nil)
(declare-snark-option use-literal-ordering-with-ur-resolution nil)
(declare-snark-option use-literal-ordering-with-paramodulation nil)

(declare-snark-option use-subsumption t)			;nil, :forward, t
(declare-snark-option use-subsumption-by-false :false)		;nil, :false, :forward, t
(declare-snark-option use-lookahead-in-dpll-for-subsumption t t)
(declare-snark-option use-simplification-by-units t)		;nil, :forward, t
(declare-snark-option use-simplification-by-equalities t)	;nil, :forward, t
(declare-snark-option use-term-ordering :rpo)	;nil, :manual, :kbo, :rpo, or a function
(declare-snark-option use-term-ordering-cache nil nil)
(declare-snark-option use-default-ordering t)			;nil, :arity, :reverse, t
(declare-snark-option 1-ary-functions>2-ary-functions-in-default-ordering nil)
(declare-snark-option ordering-functions>constants nil)		;t for speed, only if functions > constants always
(declare-snark-option rpo-status :multiset)			;default status
(declare-snark-option kbo-status :left-to-right)		;default status
(declare-snark-option kbo-variable-weight 1)			;constant-weight >= this > 0

(declare-snark-option use-indefinite-answers nil)		;nil, :disjunctive, :conditional (UNIMPLEMENTED)
(declare-snark-option use-conditional-answer-creation nil)
(declare-snark-option use-constructive-answer-restriction nil :never-print)	;no longer necessary (use constant-allowed-in-answer and function-allowed-in-answer)
(declare-snark-option use-answers-during-subsumption t :never-print)		;no longer necessary (always enabled)
(declare-snark-option use-constraint-solver-in-subsumption nil)
(declare-snark-option allow-skolem-symbols-in-answers t)
(declare-snark-option rewrite-answers nil)
(declare-snark-option rewrite-constraints t :never-print)	;nop
(declare-snark-option use-constraint-purification nil)		;nil, t, 1, 2
(declare-snark-option use-embedded-rewrites t t)
(declare-snark-option use-function-creation nil)
(declare-snark-option use-replacement-resolution-with-x=x nil)
(declare-snark-option use-paramodulation-only-into-units nil)
(declare-snark-option use-paramodulation-only-from-units nil)
(declare-snark-option use-single-replacement-paramodulation nil)

(declare-snark-option use-partitions nil nil)			;nil or list of partition ids
(declare-snark-option partition-communication-table nil :never-print)

(declare-snark-option declare-sort-root :top-sort-a)

(declare-snark-option assert-reason 'assertion :never-print)		;assertion, assumption
(declare-snark-option assert-context :root)			;:root, :current

(declare-snark-option assert-supported t)			;nil, t  :uninherited
(declare-snark-option assume-supported t)			;nil, t, :uninherited
(declare-snark-option prove-supported t)			;nil, t, :uninherited
(declare-snark-option assert-sequential nil)			;nil, t, :uninherited
(declare-snark-option assume-sequential nil)			;nil, t, :uninherited
(declare-snark-option prove-sequential nil)			;nil, t, :uninherited

(declare-snark-option prove-closure t :never-print)

(declare-snark-option number-of-given-rows-limit nil)
(declare-snark-option number-of-rows-limit nil)
(declare-snark-option agenda-length-before-simplification-limit 10000)
(declare-snark-option agenda-length-limit 3000)
(declare-snark-option run-time-limit nil)
(declare-snark-option row-argument-count-limit nil nil)
(declare-snark-option row-weight-limit nil)
(declare-snark-option row-weight-before-simplification-limit nil)
(declare-snark-option level-pref-for-giving nil)
(declare-snark-option variable-weight 1)
(declare-snark-option bag-weight-factorial nil nil)

(declare-snark-option agenda-ordering-function 'row-weight+depth)
(declare-snark-option pruning-tests '(row-weight-limit-exceeded))
(declare-snark-option pruning-tests-before-simplification '(row-weight-before-simplification-limit-exceeded))

(declare-snark-option use-clausification t)
(declare-snark-option use-equality-elimination nil)		;nil, t, or :unconstrained
(declare-snark-option use-magic-transformation nil)
(declare-snark-option use-ac-connectives t)
(declare-snark-option use-purity-test nil)
(declare-snark-option use-relevance-test nil)
(declare-snark-option use-assertion-analysis t t)

(declare-snark-option use-unify-bag-constant-abstraction nil nil)
(declare-snark-option use-associative-unification nil nil)
(declare-snark-option use-dp-subsumption nil nil)
(declare-snark-option unify-bag-basis-size-limit 1000 1000)

(declare-snark-option use-term-memory-deletion t t)
(declare-snark-option use-rewrite-cache nil nil)	;size or nil

(declare-snark-option variable-sort-marker #\# :never-print)

(declare-snark-option use-variable-name-sorts t :never-print)
(declare-snark-option use-well-sorting nil :never-print)		;nil, t, or :terms
(declare-snark-option use-extended-implications 'warn :never-print)	;nil, t, or warn
(declare-snark-option use-extended-quantifiers 'warn :never-print)	;nil, t, or warn
(declare-snark-option use-sort-relativization nil :never-print)
(declare-snark-option use-quantifier-preservation nil :never-print)

(declare-snark-option input-floats-as-ratios t :never-print)	;nop (always input floats as ratios)

(declare-snark-option use-closure-when-satisfiable t :never-print)

(declare-snark-option listen-for-commands nil :never-print)

(declare-snark-option variable-to-lisp-code nil :never-print)

(declare-snark-option print-rows-when-given nil :never-print)
(declare-snark-option print-rows-when-derived t :never-print)
(declare-snark-option print-rows-when-processed nil :never-print)
(declare-snark-option print-final-rows t :never-print)
(declare-snark-option print-unorientable-rows t :never-print)
(declare-snark-option print-pure-rows nil :never-print)
(declare-snark-option print-irrelevant-rows nil :never-print)
(declare-snark-option print-rewrite-orientation nil :never-print)	;1998-07-29

(declare-snark-option print-rows-test nil :never-print)

;;; the following options control how a row is printed
(declare-snark-option print-rows-shortened nil :never-print)
(declare-snark-option print-rows-prettily t :never-print)
(declare-snark-option print-row-wffs-prettily t :never-print)
(declare-snark-option print-row-answers t :never-print)
(declare-snark-option print-row-constraints t :never-print)
(declare-snark-option print-row-reasons t :never-print)
(declare-snark-option print-row-goals t :never-print)
(declare-snark-option print-row-partitions t :never-print)
(declare-snark-option print-row-length-limit nil :never-print)
(declare-snark-option print-given-row-lines-printing 2 :never-print)
(declare-snark-option print-given-row-lines-signalling 1 :never-print)

;;; the following options control what is printed when closure finishes
(declare-snark-option print-summary-when-finished t :never-print)
(declare-snark-option print-clocks-when-finished t :never-print)
(declare-snark-option print-term-memory-when-finished t :never-print)
(declare-snark-option print-agenda-when-finished t :never-print)

(declare-snark-option print-options-when-starting t :never-print)
(declare-snark-option print-assertion-analysis-notes t :never-print)
(declare-snark-option print-symbol-table-warnings t :never-print)

;;; the following options are for debugging
(declare-snark-option print-time-used nil :never-print)
(declare-snark-option trace-unify nil :never-print)
(declare-snark-option meter-unify-bag nil :never-print)		;nil, t, or number of seconds
(declare-snark-option trace-unify-bag-basis nil :never-print)
(declare-snark-option trace-unify-bag-bindings nil :never-print)
(declare-snark-option trace-dp-refute nil :never-print)
(declare-snark-option trace-rewrite nil :never-print)
(declare-snark-option trace-optimize-sparse-vector-expression nil :never-print)
(declare-snark-option trace-dpll-subsumption nil :never-print)	;nil, :summary, :clauses

(declare-snark-option changeable-properties-of-locked-constant '(:alias :allowed-in-answer :kbo-weight :weight) :never-print)
(declare-snark-option changeable-properties-of-locked-proposition '(:alias :allowed-in-answer :kbo-weight :weight) :never-print)
(declare-snark-option changeable-properties-of-locked-function '(:alias :new-name :allowed-in-answer :kbo-weight :weight :weight-code) :never-print)
(declare-snark-option changeable-properties-of-locked-relation '(:alias :new-name :allowed-in-answer :kbo-weight :weight :weight-code) :never-print)
(declare-snark-option changeable-properties-of-locked-logical-symbol '(:alias) :never-print)

(declare-snark-option test-option2 nil nil)	;simplification-ordering-compare-equality-arguments
(declare-snark-option test-option3 nil nil)	;paramodulater for waldinger
(declare-snark-option test-option6 nil nil)	;clausify
(declare-snark-option test-option8 nil nil)	;unify-bag
(declare-snark-option test-option9 nil nil)	;rewriting during hyperresolution
(declare-snark-option test-option10 nil nil)	;instance-graph - insert in tm-store
(declare-snark-option test-option11 nil nil)	;instance-graph - insert in retrieve-g/i-terms
(declare-snark-option test-option12 nil nil)	;instance-graph - insert in retrieve-u-terms
(declare-snark-option test-option13 nil nil)	;instance-graph - insert uses bottom-up search
(declare-snark-option test-option14 nil nil)	;sparse-vector-expressions for indexing
(declare-snark-option test-option17 nil nil)	;revert to nonspecial unification for jepd relation atoms
(declare-snark-option test-option18 nil nil)	;instance-graph - insert uses might-unify-p
(declare-snark-option test-option19 nil nil)	;revert to earlier rpo
(declare-snark-option test-option20 nil nil)	;rpo
(declare-snark-option test-option21 nil nil)	;maximum-intersection-size in optimize-sparse-vector-expression
(declare-snark-option test-option23 nil nil)	;make skolem symbols bigger than nonskolems in default symbol ordering
(declare-snark-option test-option29 nil nil)	;magic-transform-positive-units
(declare-snark-option test-option30 nil nil)	;declare sort coercion functions like the-bird, the-integer
(declare-snark-option test-option36 nil nil)	;nil or cutoff for number of unifiers for incomplete subsumption test
(declare-snark-option test-option37 nil nil)	;nop (always use extended any-ary sum and product functions)
(declare-snark-option test-option38 nil nil)	;turn off term hashing
(declare-snark-option test-option39 nil nil)	;compare-multisets
(declare-snark-option test-option40 nil nil)	;rpo-compare-multisets
(declare-snark-option test-option41 nil nil)	;resolve with $$eq in constraints
(declare-snark-option test-option42 nil nil)	;rewrite ($$less a b) to (not ($$lesseq b a)) and ($$lesseq a b) to (not ($$less b a))
(declare-snark-option test-option43 nil nil)	;don't use do-not-resolve atoms for rewriting
(declare-snark-option test-option44 nil nil)	;associative-identity-paramodulater generates only collapsed terms
(declare-snark-option test-option45 nil nil)	;function-identity2 returns identity when subsuming as well as unifying
(declare-snark-option test-option46 nil nil)
(declare-snark-option test-option47 nil nil)
(declare-snark-option test-option48 nil nil)
(declare-snark-option test-option49 nil nil)
(declare-snark-option test-option50 nil nil)

(defvar options-have-been-critiqued)

(defun initialize-options ()
  (setf options-have-been-critiqued nil)
  (dolist (name *snark-options*)
    (setf (symbol-value (intern (format nil "*%~A%*" name) :snark))
          (symbol-value (intern (format nil "*%~A-~A%*" :default name) :snark)))))

(defun finalize-options ()
  (dolist (name *snark-options*)
    (funcall name (symbol-value (intern (format nil "*%~A%*" name) :snark)))))

(defun assert-snark-option-spec-p (x)
  ;; ttp: 5/23/00 18:15 Use ~s instead of ~S so that the package is displayed.
  (cl:assert (if (symbolp x)
                 (member x *snark-options*)
                 (and (listp x)
                      (eql 2 (length x))
                      (member (first x) *snark-options*)))
             ()
             "~S is not a SNARK option."
             x))

(defun assert-snark-option-specs-p (options)
  (dolist (x options)
    (assert-snark-option-spec-p x)))

(defun set-options (options)
  ;; ttp: 5/23/00 18:15 Do funcall instead of setting the symbol value
  ;; of the underlying variable so that "before" methods are invoked,
  ;; e.g. for use-code-for-lists.
  (assert-snark-option-specs-p options)
  (setf options-have-been-critiqued nil)
  (dolist (x options)
    (if (symbolp x)
        (funcall x t)
        (funcall (first x) (second x)))))

(defmacro let-options (options &body forms)
  (assert-snark-option-specs-p options)
  (list*
   'let
   (mapcar
    (lambda (x)
      (let (name value)
        (if (symbolp x)
            (setf name x value t)
            (setf name (first x) value (second x)))
        (list (intern (concatenate 'string "*%" (symbol-name name) "%*") :snark)
              value)))
    options)
   forms))

#+(and mcl (not openmcl))
(progn
  (pushnew '(let-options  . 1) ccl:*fred-special-indent-alist* :test #'equal)
  nil)

(defun print-options (&optional all)
  (with-standard-io-syntax2
    (format t "~&; The current SNARK option values are")
    (dolist (name *snark-options*)
      (let ((value
	     (symbol-value
	      (intern (concatenate 'string "*%" (symbol-name name) "%*") :snark)))
            (default-value
              (symbol-value
               (intern (concatenate 'string "*%" (symbol-name :default-) (symbol-name name) "%*") :snark)))
	    (invisible-value
	     (symbol-value 
	      (intern (concatenate 'string "*%" (symbol-name :invisible-) (symbol-name name) "%*") :snark))))
        (when (or all
                  (and (neq :never-print invisible-value)
                       (or (eq :always-print invisible-value)
                           (neq value invisible-value))))
          (if (neql value default-value)
              (format t "~%;    (~A ~S)" name value)
              (format t "~%;       (~A ~S)" name value)))))
    (format t "~%")
    nil))

(defmethod agenda-length-limit :before (&optional (value t))
  (limit-agenda-length *agenda-of-rows-to-give* value))

(defmethod agenda-length-before-simplification-limit :before (&optional (value t))
  (limit-agenda-length *agenda-of-rows-to-process* value))

(defmethod use-resolve-code :around (&optional (value nil))
  (call-next-method
   (if (listp value)
       (remove-duplicates value :from-end t)			;replace
       (cons value (remove value (use-resolve-code?))))))	;add

(defmethod use-term-ordering :around (&optional (value nil))
  (call-next-method
   (case value
     (:recursive-path :rpo)
     (:knuth-bendix :kbo)
     (otherwise value))))

;;; options.lisp EOF
