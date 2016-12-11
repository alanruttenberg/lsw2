;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: main.lisp
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
    ordering-is-total
    *printing-deleted-messages*
    *agenda*
    ))

(defvar options-print-mode t)

(defvar *snark-is-running* nil)
(defvar *agenda-of-false-rows-to-process*)
(defvar *agenda-of-new-embeddings-to-process*)
(defvar *agenda-of-input-rows-to-give*)
(defvar *agenda-of-input-rows-to-process*)
(defvar *agenda-of-backward-simplifiable-rows-to-process*)
(defvar *agenda-of-rows-to-process*)
(defvar *agenda-of-rows-to-give*)

(defvar *proof*)

(defvar *false-rows*)
(defvar *constraint-rows*)
(defvar *hint-rows*)

(defvar *manual-ordering-results*)

(defvar critique-options t)

(defvar *break-snark?* nil)			;ttp

(defvar *interactive? nil			;ttp
  "When T, an inference rule has been called interactively, so typically
   it is not supposed to form all consequences; just with the one in *ROW2*.")

(defvar *row2* nil				;ttp
  "the second argument to an interactive inference rule.")

(defvar *propositional-abstraction-of-input-wffs*)

(defvar *negative-hyperresolution*)

(defvar *find-else-substitution* nil)

(defvar *processing-row* nil)

(defvar *hints-subsumed*)

(declaim
  (special
    rewrite-strategy
    clause-subsumption
    subsumption-mark
    *rewrites-used*
    ))

(defvar recursive-unstore nil)

(defun critique-options ()
  (unless options-have-been-critiqued
    (when (print-options-when-starting?)
      (print-options))
    (unless (or (use-resolution?)
                (use-hyperresolution?)
                (use-negative-hyperresolution?)
                (use-ur-resolution?)
                (use-paramodulation?)
                (use-ur-pttp?)
                (use-resolve-code?))
      (warn "Neither resolution nor paramodulation are specified."))
    (setf options-have-been-critiqued t))
  nil)

(defvar *number-of-given-rows* 0)
(defvar *number-of-backward-eliminated-rows* 0)
(defvar *number-of-agenda-full-deleted-rows* 0)
(declaim (type integer *number-of-given-rows* *number-of-backward-eliminated-rows*)
         (type integer *number-of-agenda-full-deleted-rows*))

(defun clear-statistics ()
  (setf *row-count* 0)
  (setf *number-of-rows* 0)
  (setf *number-of-given-rows* 0)
  (setf *number-of-backward-eliminated-rows* 0)
  (setf *number-of-agenda-full-deleted-rows* 0)
  nil)

(defun print-summary (&key (clocks t) (term-memory t) (agenda t))
  (format t "~%; Summary of computation:")
  (let ((total-number-of-rows *row-count*))
    (format t "~%; ~9D formulas have been input or derived (from ~D formulas)." total-number-of-rows *number-of-given-rows*)
    (when (< 0 total-number-of-rows)
      (format t "~%; ~9D (~2D%) were retained." *number-of-rows* (percentage *number-of-rows* total-number-of-rows))
      (when (< 0 *number-of-rows*)
	(let ((number-of-still-kept-wffs (rowset-size *rows*))
	      (number-of-reduced-wffs (- *number-of-backward-eliminated-rows* *number-of-agenda-full-deleted-rows*)))
	  (format t "  Of these,")
          (unless (eql 0 number-of-reduced-wffs)
	    (format t "~%; ~12D (~2D%) were simplified or subsumed later," number-of-reduced-wffs (percentage number-of-reduced-wffs *number-of-rows*)))
          (unless (eql 0 *number-of-agenda-full-deleted-rows*)
	    (format t "~%; ~12D (~2D%) were deleted later because the agenda was full," *number-of-agenda-full-deleted-rows* (percentage *number-of-agenda-full-deleted-rows* *number-of-rows*)))
	  (format t "~%; ~12D (~2D%) are still being kept." number-of-still-kept-wffs (percentage number-of-still-kept-wffs *number-of-rows*))))))
  (when clocks
    (format t "~%; ")
    (print-clocks))
  (when term-memory
    (format t "~%; ")
    (print-term-memory))
  (when agenda
    (format t "~%; ")
    (print-agenda))
  nil)

(defun print-rewrites (&key ancestry (test (print-rows-test?)))
  (let ((rowset (make-rowset nil)))
    (prog->
      (retrieve-all-entries #'tme-rewrites ->* e rewrites)
      (declare (ignore e))
      (dolist rewrites ->* rewrite)
      (unless (or (null (rewrite-row rewrite))
		  (null (rewrite-condition rewrite)))
	(rowset-insert (rewrite-row rewrite) rowset)))
    (let ((*rows* rowset))
      (print-rows :ancestry ancestry :test test))))

(defvar rewrites-initialized)

(defparameter initialization-functions
  (list 'clear-statistics
	'initialize-features
        'initialize-row-contexts
        'initialize-term-hash
        'initialize-simplification-ordering-compare-equality-arguments-hash-table
        'initialize-sort-theory
        'initialize-symbol-ordering
        'initialize-symbol-table
        'initialize-sort-theory2
        'initialize-symbol-table2
        'initialize-propositional-abstraction-of-input-wffs
        'initialize-assertion-analysis
        'clear-rewrite-cache
        'finalize-options
        ))

(defun initialize (&key (verbose t))
  (cond
    (*snark-is-running*
     (error "SNARK is already running."))
    (t
     (initialize-clocks)
     (when verbose
       (format t "~&; Running SNARK from ~A in ~A ~A on ~A at "
               cl-user::*snark-system-pathname*
               (lisp-implementation-type)
               (lisp-implementation-version)
               (machine-instance))
       (print-current-time)
       (format t "~%")
       (force-output))
;;   (setf *random-state* (make-random-state t))
     (initialize-numberings)
     (initialize-options)
     (initialize-operator-syntax)
     (nocomment)
     (initialize-rows2)
     (initialize-constants)
     (initialize-variables)
     (setf *number-of-new-symbols* 0)
     (setf *new-symbol-prefix* (newsym-prefix))
     (setf *new-symbol-table* (make-hash-table))

     (setf clause-subsumption t)
     (setf subsumption-mark 0)

     (setf *manual-ordering-results* nil)
;;   (dolist (modality modalatomsigns) (intensional (car modality)))
;;   (intensional 'answer)				; ???

     (make-term-memory :indexing-method :path)
     (initialize-agenda)
     (setf rewrites-initialized nil)
;;   (store-boolean-ring-rewrites)
     (setf ordering-is-total nil)
     (setf *proof* nil)
     (dolist (fn initialization-functions)
       (funcall fn))
     nil)))

(defun initialize-rows2 ()
  (initialize-rows)
  (setf *false-rows* (make-rowset))
  (setf *constraint-rows* (make-rowset))
  (setf *hint-rows* (make-rowset))
  nil)

(defmacro with-input-functions-disabled (symbols &body body)
  (let ((symbol-temps (mapcar (lambda (x) (declare (ignore x)) (gensym)) symbols))
	(value-temps1 (mapcar (lambda (x) (declare (ignore x)) (gensym)) symbols))
        (value-temps2 (mapcar (lambda (x) (declare (ignore x)) (gensym)) symbols)))
    `(let ,(mapcar (lambda (symbol symbol-temp) `(,symbol-temp ,symbol)) symbols symbol-temps)
       (let (,@(mapcan (lambda (symbol-temp value-temp1 value-temp2)
                           (declare (ignorable value-temp2))
			   (list `(,value-temp1 (function-input-code ,symbol-temp))
;;                               `(,value-temp2 (function-logical-symbol-p ,symbol-temp))
                                 ))
		     symbol-temps value-temps1 value-temps2))
	 (unwind-protect
	     (progn
	       ,@(mapcan (lambda (symbol-temp)
			     (list `(setf (function-input-code ,symbol-temp) nil)
;;                                 `(setf (function-logical-symbol-p ,symbol-temp) nil)
                                   ))
			 symbol-temps)
	       ,@body)
	   ,@(mapcan (lambda (symbol-temp value-temp1 value-temp2)
                         (declare (ignorable value-temp2))
			 (list `(setf (function-input-code ,symbol-temp) ,value-temp1)
;;                             `(setf (function-logical-symbol-p ,symbol-temp) ,value-temp2)
                               ))
		     symbol-temps value-temps1 value-temps2))))))

(defun initialize-agenda ()
  (setf *agenda*
	(list
	  (setf *agenda-of-false-rows-to-process* 
		(make-agenda :name "false rows to process"
			     :same-item-p #'same-agenda-item-p))
	  (setf *agenda-of-new-embeddings-to-process*
		(make-agenda :name "new embeddings to process"
			     :same-item-p #'same-agenda-item-p))
	  (setf *agenda-of-input-rows-to-process*
		(make-agenda :name "input rows to process"
			     :same-item-p #'same-agenda-item-p))
	  (setf *agenda-of-backward-simplifiable-rows-to-process*
		(make-agenda :name "backward simplifiable rows to process"
			     :same-item-p #'same-agenda-item-p))
	  (setf *agenda-of-rows-to-process*
		(make-agenda :name "rows to process"
			     :length-limit (agenda-length-before-simplification-limit?)
			     :same-item-p #'same-agenda-item-p))
          (setf *agenda-of-input-rows-to-give*
                (make-agenda :name "input rows to give"
                             :same-item-p #'same-agenda-item-p))
	  (setf *agenda-of-rows-to-give*
		(make-agenda :name "rows to give"
			     :length-limit (agenda-length-limit?)
			     :length-limit-deletion-action #'unstore-agenda-item
			     :same-item-p #'same-agenda-item-p)))))

(defun initialize-rewrites ()
  (prog->
    (map-symbol-table ->* name arity-code symbol)
    (declare (ignore name arity-code))
    (when (function-symbol-p symbol)
      (dolist (rewrite (function-rewrites symbol))
	(assert-rewrite rewrite)))))

(defun store-boolean-ring-rewrites ()
  (declare-logical-symbol '%rewrite)
  (dolist (rewrite '((%rewrite (or ?x ?y) (xor (and ?x ?y) ?x ?y))	;translate OR
		     (%rewrite (implies ?x ?y) (xor (and ?x ?y) ?x true))	;translate IMPLIES
                     (%rewrite (implied-by ?y ?x) (xor (and ?x ?y) ?x true))
		     (%rewrite (iff ?x ?y) (xor ?x ?y true))	;translate IFF
		     (%rewrite (not ?x) (xor ?x true))
;;		     (%rewrite (xor ?x false) ?x)
;;		     (%rewrite (xor ?x ?x) false)
;;		     (%rewrite (xor ?y ?x ?x) ?y)			;embedding of above
;;		     (%rewrite (and ?x true) ?x)
;;		     (%rewrite (and ?x false) false)
;;		     (%rewrite (and ?x ?x) ?x)
;;		     (%rewrite (and ?y ?x ?x) (and ?x ?y))		;embedding of above
		     (%rewrite (and ?x (xor ?y ?z)) (xor (and ?x ?y) (and ?x ?z)))
		     ))
    (store-rewrite
     (renumber
      (with-input-functions-disabled
        (*and* *or* *not* *implies* *implied-by* *iff* *xor* *if*)
        (let ((*input-proposition-variables* t))
          (input-wff rewrite))))
     '>)))

(defun renumber-row (row)
  (let ((rsubst nil))
    (let ((wff (row-wff row)))
      (multiple-value-setq (wff rsubst) (renumber wff nil rsubst))
      (setf (row-wff row) wff))
    (let ((constraint-alist (row-constraints row)))
      (when constraint-alist
	(multiple-value-setq (constraint-alist rsubst) (renumber constraint-alist nil rsubst))
	(setf (row-constraints row) constraint-alist)))
    (let ((answer (row-answer row)))
      (unless (eq false answer)
	(multiple-value-setq (answer rsubst) (renumber answer nil rsubst))
	(setf (row-answer row) answer)))
    rsubst))

(defvar *embedding-variables* nil)		;list of embedding variables

(defun embedding-variable-p (x)
  (let ((l *embedding-variables*))
    (and l (member x l :test #'eq))))

(defvar *assert-rewrite-polarity* nil)

(defun assert-rewrite-check (wff)
  (declare (ignore wff))
;;(cl:assert (member (instantiating-direction (arg1 wff) (arg2 wff) nil) '(> <>)))
  )

(defun assert-rewrite (wff &key name (reason (assert-reason?)) (input t) (partitions (use-partitions?)) (conditional nil))
  (cl:assert (symbolp name))
  (macrolet 
    ((make-row1 (wff)
       `(make-row :wff ,wff
                  :number (incf *number-of-rows*)
                  :name name
                  :context context
                  :reason reason
                  :input-wff input-wff)))
  (prog->
    (the-row-context2 (ecase reason (assertion (assert-context?)) (assumption :current)) partitions -> context)
    (if conditional '>? '> -> dir)
    (if input (input-wff wff) (values wff nil (term-to-lisp wff)) -> wff dp-alist input-wff)
    (declare (ignore dp-alist))
    (cond
     ((or (equality-p wff) (and (equivalence-p wff) (atom-p (arg1 wff))))
      (renumber wff -> wff rsubst)
      (declare (ignore rsubst))
      (assert-rewrite-check wff)
      (store-rewrite wff dir (make-row1 wff)))
     ((literal-p wff)
      (literal-p wff -> atom polarity)
      (renumber atom -> atom rsubst)
      (declare (ignore rsubst))
      (store-rewrite2 atom (if (eq :pos polarity) true false) (make-row1 wff) nil))
     ((and (implication-p wff)
           (atom-p (arg1 wff)))
      (prog->
        (make-compound *iff* (arg1 wff) (arg2 wff) -> wff1)
        (renumber wff1 -> wff1 rsubst)
        (declare (ignore rsubst))
        (quote :pos -> *assert-rewrite-polarity*)
        (assert-rewrite-check wff1)
        (store-rewrite wff1 dir (make-row1 wff))))
     ((and (implication-p wff)
           (negation-p (arg1 wff))
           (atom-p (arg1 (arg1 wff))))
      (prog->
        (make-compound *iff* (arg1 (arg1 wff)) (negate (arg2 wff)) -> wff1)
        (renumber wff1 -> wff1 rsubst)
        (declare (ignore rsubst))
        (quote :neg -> *assert-rewrite-polarity*)
        (assert-rewrite-check wff1)
        (store-rewrite wff1 dir (make-row1 wff))))
     ((and (reverse-implication-p wff)
           (atom-p (arg1 wff)))
      (prog->
        (make-compound *iff* (arg1 wff) (arg2 wff) -> wff1)
        (renumber wff1 -> wff1 rsubst)
        (declare (ignore rsubst))
        (quote :neg -> *assert-rewrite-polarity*)
        (assert-rewrite-check wff1)
        (store-rewrite wff1 dir (make-row1 wff))))
     ((and (reverse-implication-p wff)
           (negation-p (arg1 wff))
           (atom-p (arg1 (arg1 wff))))
      (prog->
        (make-compound *iff* (arg1 (arg1 wff)) (negate (arg2 wff)) -> wff1)
        (renumber wff1 -> wff1 rsubst)
        (declare (ignore rsubst))
        (quote :pos -> *assert-rewrite-polarity*)
        (assert-rewrite-check wff1)
        (store-rewrite wff1 dir (make-row1 wff))))
     ((and (conjunction-p wff)
           (implication-p (arg1 wff))
           (implication-p (arg2 wff))
           (equal-p (arg1 (arg1 wff)) (arg2 (arg2 wff)))
           (atom-p (arg1 (arg1 wff))))
      (prog->
        (make-compound *iff* (arg1 (arg1 wff)) (arg2 (arg1 wff)) -> wff1)
        (renumber wff1 -> wff1 rsubst)
        (declare (ignore rsubst))
        (quote :pos -> *assert-rewrite-polarity*)
        (assert-rewrite-check wff1)
        (store-rewrite wff1 dir (make-row1 (arg1 wff))))
      (prog->
        (make-compound *iff* (arg2 (arg2 wff)) (arg1 (arg2 wff)) -> wff2)
        (renumber wff2 -> wff2 rsubst)
        (declare (ignore rsubst))
        (quote :neg -> *assert-rewrite-polarity*)
        (assert-rewrite-check wff2)
        (store-rewrite wff2 dir (make-row1 (arg2 wff)))))	;same name?
     ((and (conjunction-p wff)
           (implication-p (arg1 wff))
           (reverse-implication-p (arg2 wff))
           (equal-p (arg1 (arg1 wff)) (arg1 (arg2 wff)))
           (atom-p (arg1 (arg1 wff))))
      (prog->
        (make-compound *iff* (arg1 (arg1 wff)) (arg2 (arg1 wff)) -> wff1)
        (renumber wff1 -> wff1 rsubst)
        (declare (ignore rsubst))
        (quote :pos -> *assert-rewrite-polarity*)
        (assert-rewrite-check wff1)
        (store-rewrite wff1 dir (make-row1 (arg1 wff))))
      (prog->
        (make-compound *iff* (arg1 (arg2 wff)) (arg2 (arg2 wff)) -> wff2)
        (renumber wff2 -> wff2 rsubst)
        (declare (ignore rsubst))
        (quote :neg -> *assert-rewrite-polarity*)
        (assert-rewrite-check wff2)
        (store-rewrite wff2 dir (make-row1 (arg2 wff)))))	;same name?
     (t
      (error "Improper form for assert-rewrite."))))
  nil))

(defmacro assertion (wff &rest keys-and-values)
  (cond
   ((getf keys-and-values :ignore)
    nil)
   (t
    `(assertionfun ',wff ',keys-and-values))))			;don't evaluate wff or options

(defun assertionfun (wff keys-and-values)
  (apply 'assert wff keys-and-values))

(defun assert (wff
	       &key
	       name
	       conc-name
	       (answer false)
               constraints			;2-lists of theory name and wff
               (reason (assert-reason?))
               context
	       (partitions (use-partitions?))
               (supported nil supported-supplied)
               (sequential nil sequential-supplied)
	       documentation
               author				;for KIF
               source				;for KIF
               (input-wff none)
               (magic (use-magic-transformation?))
               closure)
  (with-clock-on assert
    (when name
      (unless (can-be-row-name name 'warn)
        (setf name nil)))
    (when (eq 'conjecture reason)
      (setf wff `(not ,wff))
      (setf reason 'negated_conjecture))
    (cl:assert (member reason '(assertion assumption negated_conjecture hint)))
    (unless supported-supplied
      (setf supported (ecase reason
                        (assertion (assert-supported?))
                        (assumption (assume-supported?))
                        (negated_conjecture (prove-supported?))
                        (hint nil))))
    (cl:assert (member supported '(nil t :uninherited)))
    (unless sequential-supplied
      (setf sequential (ecase reason
                         (assertion (assert-sequential?))
                         (assumption (assume-sequential?))
                         (negated_conjecture (prove-sequential?))
                         (hint nil))))
    (cl:assert (member sequential '(nil t :uninherited)))
    (unless context
      (setf context (ecase reason
                      (assertion (assert-context?))
                      ((assumption negated_conjecture hint) :current))))
    (when (eq :current context)
      (setf context (current-row-context)))
    (let ((n 0))
      (prog->
        (not (use-well-sorting?) -> *%check-for-well-sorted-atom%*)
        (input-wff wff :clausify (use-clausification?) -> wff dp-alist input-wff1 input-wff-subst)
        (declare (ignore dp-alist))
        (when *find-else-substitution*
          (setf wff (instantiate wff *find-else-substitution*)))
        (mapcar (lambda (x) (cons (first x) (input-wff (second x) :*input-wff-substitution* input-wff-subst))) constraints -> constraint-alist)
        (when (eq 'from-wff answer)
          (cond
           ((and (consp input-wff1) (eq 'forall (first input-wff1)))
            (setf answer (cons 'values (mapcar (lambda (x) (if (consp x) (first x) x)) (second input-wff1)))))
           ((and (consp input-wff1) (eq 'not (first input-wff1)) (consp (second input-wff1)) (eq 'exists (first (second input-wff1))))
            (setf answer (cons 'values (mapcar (lambda (x) (if (consp x) (first x) x)) (second (second input-wff1))))))
           (t
            (setf answer false))))
        (input-wff answer :*input-wff-substitution* input-wff-subst -> answer)
        (if (use-equality-elimination?) (equality-eliminate-wff wff) wff -> wff)
        (if (and magic (not (eq 'hint reason))) (magic-transform-wff wff :transform-negative-clauses supported :transform-positive-units (test-option29?)) wff -> wff)
        (well-sort-wffs (list* wff answer (mapcar #'cdr constraint-alist)) ->* subst)
        (incf n)
        (map-conjuncts wff ->* wff)
        (catch 'fail
          (let* ((wff (fail-when-true (instantiate wff subst)))
                 (row (make-row :wff wff
                                :constraints (fail-when-constraint-false (instantiate constraint-alist subst))
                                :answer (if (and magic (magic-goal-occurs-p wff))
                                            false
                                            (fail-when-disallowed (instantiate answer subst)))
                                :context (the-row-context2 context partitions)
                                :reason reason
                                :supported supported
                                :sequential sequential
                                :conc-name (and conc-name (if (stringp conc-name) conc-name (funcall conc-name wff)))
                                :documentation documentation
                                :author author
                                :source source
                                :input-wff (if (neq none input-wff) input-wff input-wff1)
                                :name name)))
	    (when (use-constraint-purification?)
	      (setf row (constraint-purify-row row)))
            (when (use-assertion-analysis?)
              (assertion-analysis row))
            (record-new-input-wff row))))
      (unless (eql 1 n)
        (with-standard-io-syntax2
          (warn "Input wff ~A has ~D well-sorted instances." wff n)))))
  (when closure
    (closure)))

(defun assume (wff &rest keys-and-values)
  (apply #'assert wff (append keys-and-values (list :reason 'assumption))))

(defun prove (wff &rest keys-and-values)
  (apply #'assert wff (append keys-and-values (list :reason 'conjecture :closure (prove-closure?)))))

(defun new-prove (wff &rest keys-and-values)
  (new-row-context)
  (apply #'prove wff keys-and-values))

(defun hint (wff &rest keys-and-values)
  (apply #'assert wff (append keys-and-values (list :reason 'hint))))

(defun fail ()
  (throw 'fail nil))

(defun fail-when-nil (x)
  (if (null x)
      (throw 'fail nil)
      x))

(defun fail-when-true (x)
  (if (eq true x)
      (throw 'fail nil)
      x))

(defun fail-when-false (x)
  (if (eq false x)
      (throw 'fail nil)
      x))

(defun fail-when-constraint-false (constraint-alist)
  (dolist (x constraint-alist constraint-alist)
    (when (eq false (cdr x))
      (throw 'fail nil))))

(defun fail-when-disallowed (answer)
  (if (answer-disallowed-p answer)
      (throw 'fail nil)
      answer))

(defvar *check-for-disallowed-answer* nil)

(defun answer-disallowed-p (answer)
  (if (and (rewrite-answers?) (not *check-for-disallowed-answer*))
      nil
      (disallowed-symbol-occurs-in-answer-p answer nil)))

(defun make-demodulant (row1 row2 wff2* context1 context2)
  (cond
   ((eq true wff2*)
    :tautology)
   (t
    (prog->
      (context-intersection-p context1 context2 ->nonnil context)
      (make-row :wff (instantiate wff2* 1)
	        :constraints (instantiate (row-constraints row2) 1)
	        :answer (instantiate (row-answer row2) 1)
	        :supported (row-supported row2)
                :sequential (row-sequential row2)
                :context context
	        :reason `(rewrite ,row2 ,row1))))))

(defun make-answer2 (row1 row2 subst cond swap)
  (let ((answer1 (instantiate (row-answer row1) 1 subst))
	(answer2 (instantiate (row-answer row2) 2 subst)))
    (fail-when-disallowed
      (cond
	((eq false answer1)
	 answer2)
	((eq false answer2)
	 answer1)
	((equal-p answer1 answer2)
	 answer1)
	((use-conditional-answer-creation?)
	 (if swap
	     (make-conditional-answer (instantiate cond subst) answer2 answer1 nil)
	     (make-conditional-answer (instantiate cond subst) answer1 answer2 nil)))
	(t
	 (disjoin answer1 answer2))))))

(defmacro make-resolvent-part (rown atomn atomn* truthvaluen n subst)
  (let ((wffn (gensym))
	(atom (gensym))
	(polarity (gensym))
	(atom* (gensym)))
    `(prog->
       (row-wff ,rown -> ,wffn)
       (cond
	 ((eq ,wffn ,atomn)
	  ,truthvaluen)
	 (t
	  (map-atoms-in-wff-and-compose-result ,wffn ->* ,atom ,polarity)
	  (declare (ignore ,polarity))
	  (cond
	    ((eq ,atom ,atomn)
	     ,truthvaluen)
	    (t
	     (instantiate ,atom ,n ,subst -> ,atom*)
	     (cond
	       ((equal-p ,atom* ,atomn* subst)
		,truthvaluen)
	       (t
		,atom*)))))))))

(defun make-resolvent1 (row1 atom1 truthvalue1 row2 atom2 truthvalue2 subst context1 context2)
  (prog->
    (context-intersection-p context1 context2 ->nonnil context)
    (instantiate atom1 1 -> atom1*)
    (instantiate atom2 2 -> atom2*)
    (disjoin
     (make-resolvent-part row1 atom1 atom1* truthvalue1 1 subst)
     (make-resolvent-part row2 atom2 atom2* truthvalue2 2 subst)
     -> wff)
    (cond
     ((eq true wff)
      :tautology)
     (t
      (make-row :wff wff
                :constraints (conjoin-alists
                              (instantiate (row-constraints row1) 1 subst)
                              (instantiate (row-constraints row2) 2 subst))
                :answer (make-answer2 row1 row2 subst atom1* (eq false truthvalue1))
                :supported (or (row-supported-inheritably row1) (row-supported-inheritably row2))
                :sequential (or (row-sequential-inheritably row1) (row-sequential-inheritably row2))
                :context context
                :reason (if (eq true truthvalue1) `(resolve ,row1 ,row2) `(resolve ,row2 ,row1)))))))

(defun make-resolvent (row1 atom1 atom1* truthvalue1 row2 atom2 atom2* truthvalue2 subst
                       context1 context2)
  (let ((made nil))
    (prog->
      (context-intersection-p context1 context2 ->nonnil context)
      (catch 'fail
        (record-new-derived-row
         (make-row :wff (fail-when-true
                         (if (eq true truthvalue1)
                             (disjoin
                              (make-resolvent-part row2 atom2 atom2* truthvalue2 2 subst)
                              (make-resolvent-part row1 atom1 atom1* truthvalue1 1 subst))
                             (disjoin
                              (make-resolvent-part row1 atom1 atom1* truthvalue1 1 subst)
                              (make-resolvent-part row2 atom2 atom2* truthvalue2 2 subst))))
                   :constraints (fail-when-constraint-false
				      (conjoin-alists
                                       (instantiate (row-constraints row1) 1 subst)
                                       (instantiate (row-constraints row2) 2 subst)))
                   :answer (make-answer2 row1 row2 subst atom1* (eq false truthvalue1))
                   :supported (or (row-supported-inheritably row1) (row-supported-inheritably row2))
                   :sequential (or (row-sequential-inheritably row1) (row-sequential-inheritably row2))
                   :context context
                   :reason (if (eq true truthvalue1) `(resolve ,row1 ,row2) `(resolve ,row2 ,row1))))
        (setf made t)))
    made))

(defun make-resolventa (row1 atom1 atom1* truthvalue1 subst context1 &optional residue)
  (prog->
   (catch 'fail
     (record-new-derived-row
      (make-row :wff (fail-when-true
                      (let ((wff (make-resolvent-part row1 atom1 atom1* truthvalue1 1 subst)))
                        (if residue (disjoin (instantiate residue subst) wff) wff)))
		:constraints (fail-when-constraint-false (instantiate (row-constraints row1) 1 subst))
		:answer (fail-when-disallowed (instantiate (row-answer row1) 1 subst))
		:supported (row-supported row1)
		:sequential (row-sequential row1)
                :context context1
		:reason `(resolve ,row1 ,(function-code-name (head atom1*))))))))

(defun make-resolventb (row1 residue subst context1)
  (prog->
   (catch 'fail
     (record-new-derived-row
      (make-row :wff (fail-when-true
                      (instantiate residue subst))
                :constraints (fail-when-constraint-false (instantiate (row-constraints row1) 1 subst))
                :answer (fail-when-disallowed (instantiate (row-answer row1) 1 subst))
                :supported (row-supported row1)
                :sequential (row-sequential row1)
                :context context1
                :reason `(resolve ,row1 :resolve-code))))))

(defun make-hyperresolvent-nucleus-part (nucleus subst)
  (prog->
    (hyperresolution-nucleus-polarity -> nucleus-polarity)
    (if (eq :pos nucleus-polarity) false true -> truthvalue)
    (map-atoms-in-wff-and-compose-result (row-wff nucleus) ->* atom polarity)
    (cond
     ((eq nucleus-polarity polarity)
      truthvalue)
     (t
      (instantiate atom 1 subst)))))

(defvar *resolve-functions-used* nil)

(defun make-hyperresolvent (nucleus electrons residues subst)
  (prog->
    (row-context-live? nucleus ->nonnil context)
    (catch 'fail
      (let ((k (+ (length electrons) 1))
	    (wff (fail-when-true (make-hyperresolvent-nucleus-part nucleus subst)))
	    (constraint-alist (fail-when-constraint-false (instantiate (row-constraints nucleus) 1 subst)))
	    (answer (fail-when-disallowed (instantiate (row-answer nucleus) 1 subst)))
	    (supported (row-supported-inheritably nucleus))
            (sequential (row-sequential-inheritably nucleus))
	    parents)
	(dolist (residue residues)
          (setf wff (fail-when-true (disjoin (instantiate residue subst) wff))))
        (dolist (x electrons)
          (mvlet (((:list electron+ atom atom*) x))
	    (setf wff (fail-when-true
			(disjoin
			  (make-resolvent-part electron+ atom atom* (if *negative-hyperresolution* true false) k subst)
			  wff)))
	    (when (row-constraints electron+)
	      (setf constraint-alist (fail-when-constraint-false
				 (conjoin-alists
				   (instantiate (row-constraints electron+) k subst)
				   constraint-alist))))
	    (unless (eq false (row-answer electron+))
	      (setf answer (cond
			     ((eq false answer)
			      (fail-when-disallowed (instantiate (row-answer electron+) k subst)))
			     ((not (use-conditional-answer-creation?))
			      (disjoin
				(fail-when-disallowed (instantiate (row-answer electron+) k subst))
				answer))
			     (*negative-hyperresolution*
			      (make-conditional-answer
				(fail-when-disallowed (instantiate atom* k subst))
				(fail-when-disallowed (instantiate (row-answer electron+) k subst))
				answer
				nil))
			     (t
			      (make-conditional-answer
				(fail-when-disallowed (instantiate atom* k subst))
				answer
				(fail-when-disallowed (instantiate (row-answer electron+) k subst))
				nil)))))
            (setf context (fail-when-nil (context-intersection-p
                                          context (row-context-live? electron+))))
	    (unless supported
	      (setf supported (row-supported-inheritably electron+)))
            (unless sequential
              (setf sequential (row-sequential-inheritably electron+)))
	    (push electron+ parents)
	    (decf k)))
	(push nucleus parents)
	(record-new-derived-row
	  (make-row :wff wff
		    :constraints constraint-alist
		    :answer answer
		    :supported supported
                    :sequential sequential
                    :context context
		    :reason (if *negative-hyperresolution*
				`(negative-hyperresolve ,@parents ,@*resolve-functions-used*)
				`(hyperresolve ,@parents ,@*resolve-functions-used*))))))))

(defun make-ur-resolvent (nucleus electrons target-atom target-polarity subst)
  (prog->
    (row-context-live? nucleus ->nonnil context)
    (catch 'fail
      (let ((k (+ (length electrons) 1))
            (constraint-alist (fail-when-constraint-false (instantiate (row-constraints nucleus) 1 subst)))
            (answer (fail-when-disallowed (instantiate (row-answer nucleus) 1 subst)))
            (supported (row-supported-inheritably nucleus))
            (sequential (row-sequential-inheritably nucleus))
            parents)
        (dolist (electron electrons)
          (when (row-constraints electron)
            (setf constraint-alist (fail-when-constraint-false
                                    (conjoin-alists
                                     (instantiate (row-constraints electron) k subst)
                                     constraint-alist))))
          (unless (eq false (row-answer electron))
            (setf answer (cond
                          ((eq false answer)
                           (fail-when-disallowed (instantiate (row-answer electron) k subst)))
                          ((not (use-conditional-answer-creation?))
                           (disjoin
                            (fail-when-disallowed (instantiate (row-answer electron) k subst))
                            answer))
                          (t
                           (make-conditional-answer
                            (fail-when-disallowed (instantiate (row-wff electron) k subst))
                            answer
                            (fail-when-disallowed (instantiate (row-answer electron) k subst))
                            nil)))))
          (setf context (fail-when-nil (context-intersection-p
                                        context (row-context-live? electron))))
          (unless supported
            (setf supported (row-supported-inheritably electron)))
          (unless sequential
            (setf sequential (row-sequential-inheritably electron)))
          (push electron parents)
          (decf k))
        (push nucleus parents)
        (record-new-derived-row
         (make-row :wff (if target-atom
                            (if (eq :pos target-polarity)
                                (instantiate target-atom subst)
                                (make-compound *not* (instantiate target-atom subst)))
                            false)
                   :constraints constraint-alist
                   :answer answer
                   :supported supported
                   :sequential sequential
                   :context context
                   :reason `(ur-resolve ,@parents ,@*resolve-functions-used*)))))))

(defun make-paramodulant-form (cc value1* term2* wff2* subst)
  (cond
    ((not (term-subsort-p value1* term2* subst))
     )
    ((use-single-replacement-paramodulation?)
     (substitute-once cc value1* term2* wff2* subst))
    (t
     (funcall cc (substitute value1* term2* wff2* subst)))))

(defun make-paramodulant (row1 equality1 value1* row2 term2* subst context1 context2)
  (prog->
    (context-intersection-p context1 context2 ->nonnil context)
    (catch 'fail
      (fail-when-constraint-false
       (conjoin-alists
        (instantiate (row-constraints row2) 2 subst)
        (instantiate (row-constraints row1) 1 subst))
       -> constraint)
      (instantiate equality1 1 subst -> equality1*)
      (make-answer2 row1 row2 subst equality1* t -> answer)
      (or (row-supported-inheritably row1) (row-supported-inheritably row2) -> supported)
      (or (row-sequential-inheritably row1) (row-sequential-inheritably row2) -> sequential)
      (list 'paramodulate row2 row1 -> reason)
      (make-resolvent-part row1 equality1 equality1* false 1 subst -> w1)
      (instantiate value1* subst -> value1*)
      (instantiate (row-wff row2) 2 subst -> wff2*)
      (make-paramodulant-form value1* term2* wff2* subst ->* w2)
      (catch 'fail
        (record-new-derived-row
         (make-row :wff (fail-when-true (disjoin w1 w2))
                   :constraints constraint
                   :answer answer
                   :supported supported
                   :sequential sequential
                   :context context
                   :reason reason))))))

(defun make-paramodulanta (value1* row2 term2* subst context2)
  (prog->
    (catch 'fail
      (fail-when-constraint-false (instantiate (row-constraints row2) 2 subst) -> constraint)
      (fail-when-disallowed (instantiate (row-answer row2) 2 subst) -> answer)
      (row-supported-inheritably row2 -> supported)
      (row-sequential-inheritably row2 -> sequential)
      (list 'paramodulate row2 (function-code-name (head term2*)) -> reason)
      (make-paramodulant-form
       (instantiate value1* subst) term2* (instantiate (row-wff row2) 2 subst) subst ->* w2)
      (catch 'fail
        (record-new-derived-row
         (make-row :wff (fail-when-true w2)
                   :constraints constraint
                   :answer answer
                   :supported supported
                   :sequential sequential
                   :context context2
                   :reason reason))))))

(defun canonicalize-wff (wff)
  (prog->
    (map-atoms-in-wff-and-compose-result wff ->* atom polarity)
    (unless (variable-p atom)			;shouldn't be variable atom
      (setf atom (hash-term atom))
      (map-terms-in-atom atom nil polarity ->* term polarity)
      (declare (ignore polarity))
      (unless (variable-p term)
	(tm-store term)))
    atom))

(defun index-terms-in-atom-of-derived-wff (atom polarity row)
  (setf atom (hash-term atom))
  (prog->
    (map-terms-in-atom atom nil polarity ->* term polarity)
    (declare (ignore polarity))
    (dereference
     term nil
     :if-constant (unless (constant-constructor term)	;suppress reduction, paramodulation
                    (tm-store term)
                    (insert-into-rows-containing-term row term))
     :if-compound (progn
                    (tm-store term)
                    (insert-into-rows-containing-term row term))))
  atom)

(defun dont-make-embedding-p (a b)
  (declare (ignore b))
  ;; don't make embedding if ac lhs has a single-occurrence top-level variable
  (let ((head (head a)))
    (and
     (function-associative head)
     (function-commutative head)
     (let ((terms-and-counts (count-arguments head (args a) nil)))
       (loop for tc1 in terms-and-counts
             thereis (and
                      (eql 1 (tc-count tc1))
                      (variable-p (tc-term tc1))
                      (same-sort? (associative-function-argument-sort head) (variable-sort (tc-term tc1)))
                      (loop for tc2 in terms-and-counts
                            never (and (neq tc1 tc2) (variable-occurs-p (tc-term tc1) (tc-term tc2) nil)))))))))

(defun embedding-types (pattern value)
  (let ((head (head pattern)))
    (when (function-associative head)
      (unless (dont-make-embedding-p pattern value)
	(cond
	  ((function-commutative head)
	   :l)
	  (t
	   :l&r))))))

(defun store-rewrite2 (pattern value row conditional)
  (cond
    ((variable-p pattern)
     nil)
    (t
     (clear-rewrite-cache)
     (prog->
       (make-rewrite row
		     pattern
		     value
		     (if conditional 'simplification-ordering-greaterp t)
		     (symbol-count pattern)
		     (new-variables value nil (variables pattern))
                     *assert-rewrite-polarity*
		     -> rewrite)
       (setf pattern (hash-term pattern))
       (tm-store pattern)
       (when (compound-p pattern)
	 (setf (function-rewritable-p (head pattern)) t)
	 (setf (rewrite-embeddings rewrite) (embedding-types pattern value)))
       (push rewrite (rewrites pattern))
       (when row
	 (push rewrite (row-rewrites row))))
     t)))

(defun store-rewrite (equality-or-equivalence &optional dir row)
  (let ((args (args equality-or-equivalence)) stored)
    (unless dir
      (setf dir (simplification-ordering-compare-equality-arguments equality-or-equivalence nil t)))
    (when (and (or (eq '> dir) (eq '>? dir) (eq '<>? dir))
	       (store-rewrite2 (first args) (second args) row (neq '> dir)))
      (setf stored t))
    (when (and (or (eq dir '<) (eq dir '<?) (eq dir '<>?))
	       (store-rewrite2 (second args) (first args) row (neq '< dir)))
      (setf stored t))
    (cond
      (stored
       )
      ((member dir '(> >? < <? <>?))
       (warn "Cannot use equality or equivalence ~A as rewrite." equality-or-equivalence))
      (t
       (when (print-unorientable-rows?)
	 (print-unorientable-wff equality-or-equivalence))))))

(defun maybe-store-atom-rewrite (atom truth-value row)
  (when (use-simplification-by-units?)
    (store-rewrite (make-compound *iff* atom truth-value) '> row)))

(defun store-given-row (row)
  (unless (row-given-p row)
    (prog->
      (map-atoms-in-wff (row-wff row) ->* atom polarity)
      (when (and (eq :pos polarity) (equality-p atom))
	(args atom -> args)
	(first args -> arg1)
	(second args -> arg2)
        (unless (equal-p arg1 arg2)
	  (simplification-ordering-compare-equality-arguments atom nil -> dir)
	  (unless (eq '< dir)
	    (store-given-row-equality row arg1 arg2))
	  (unless (eq '> dir)
	    (store-given-row-equality row arg2 arg1)))))
    (setf (row-status row) :given))
  row)

(defun store-given-row-equality (row pattern value)
  (unless (variable-p pattern)
    (prog->
      (setf pattern (hash-term pattern))
      (tm-store pattern)
      (pushnew (cons row value)
	       (rows-containing-paramodulatable-equality pattern)
	       :test (lambda (x y) (and (eq (car x) (car y)) (eq (cdr x) (cdr y)))))
      )))

(defun store-derived-wff (row)
  ;; indexes atomic formulas of row so they can be retrieved for subsumption
  ;; indexes terms of row so they can be retrieved for demodulation
  ;; make rewrite from row if possible
  (let* ((wff (row-wff row))
         (answer (row-answer row))
	 (hint (row-hint-p row))
	 (potential-rewrite (and (not hint) (row-bare-unit-p row) (not (row-embedding-p row)))))
    (setf wff (map-atoms-in-wff-and-compose-result
               (lambda (atom polarity)
                 (unless hint
                   (setf atom (index-terms-in-atom-of-derived-wff atom polarity row)))
                 (prog->
                   (setf atom (hash-term atom))
                   (tm-store atom)
                   (unless (eq :neg polarity)
                     (insert-into-rows-containing-atom-positively row atom))
                   (unless (eq :pos polarity)
                     (insert-into-rows-containing-atom-negatively row atom))
                   (insert-into-rows-containing-term row atom)
                   (when potential-rewrite
                     (cond
                      ((and (use-simplification-by-equalities?) (eq :pos polarity) (equality-p atom))
                       (let ((args (args atom)))
                         (ecase (simplification-ordering-compare-equality-arguments atom nil t row)
                           (<
                            (store-rewrite atom '< row))
                           (>
                            (store-rewrite atom '> row))
                           (=
                            (unless (and (not (variable-p (first args)))
                                         (equal-p (first args) (second args)))
                              (maybe-store-atom-rewrite atom true row)))
                           (?
                            (case (instantiating-direction (first args) (second args) nil)
                              (>
                               (store-rewrite atom '>? row))
                              (<
                               (store-rewrite atom '<? row))
                              (<>
                               (if (variant-p (first args) (instantiate (second args) 1))
                                   (store-rewrite atom '>? row)
                                   (store-rewrite atom '<>? row))))
                            (maybe-store-atom-rewrite atom true row)))))
                      (t
                       (maybe-store-atom-rewrite atom (if (eq :pos polarity) true false) row))))
                   atom))
		wff))
    (unless (or (eq false answer) (variable-p answer))
      (setf answer (canonicalize-wff answer)))
    (setf (row-wff row) wff)
    (setf (row-answer row) answer)
    (dolist (parent (row-parents row))
      (rowset-insert row (or (row-children parent)
                             (setf (row-children parent) (make-rowset)))))))

(defun recursively-unstore-wff (row msg stop-predicate)
  (unless (funcall stop-predicate row)
    (prog->
      (map-rows :rowset (row-children row) :reverse t ->* child)
      (recursively-unstore-wff child "Deleted descendant" stop-predicate))
    (unstore-wff row msg)))

(defun unstore-wff (row msg)
  (unless (row-deleted-p row)
    (delete-row-from-agenda row)
    (when (row-number row)
      (rowsets-delete row))
    (let ((rewrites (row-rewrites row)))
      (when rewrites
	(clear-rewrite-cache)
	(dolist (rewrite rewrites)
	  (setf (rewrite-condition rewrite) nil)
	  (let ((e (the-term-memory-entry (rewrite-pattern rewrite))))
	    (setf (tme-rewrites e) (delete rewrite (tme-rewrites e) :count 1))))
	(setf (row-rewrites row) nil)))
    (prog->
      (map-terms-in-term (row-wff row) ->* term polarity)
      (declare (ignore polarity))
      (unless (variable-p term)
	(some-term-memory-entry term -> e)
	(when e
	  (let ((l (tme-rows-containing-paramodulatable-equality e)))
	    (when l
	      (setf (tme-rows-containing-paramodulatable-equality e) (delete row l :key #'car))))
	  (when (use-term-memory-deletion?)
	    (when (tme-useless-p e)
	      (tm-remove-entry e))))))		;reinstated deletion 1997-08-16
    (setf (row-status row) :deleted)
    (when (row-number row)
      (incf *number-of-backward-eliminated-rows*)
      (when (print-rows-when-derived?)
	(print-deleted-wff row msg))
      (prog->
        (map-rows :rowset (row-children row) :reverse t ->* child)
        (when (row-embedding-p child)
          (unstore-wff child "Deleted embedding")))
      (rowsets-delete-column (row-children row))
      (setf (row-children row) nil))))

(defun delete-row (name-or-number)
  (prog->
    (quote 0 -> *number-of-backward-eliminated-rows*)
    (quote nil -> *printing-deleted-messages*)
    (row name-or-number 'warn ->nonnil row)
    (unstore-wff row "Deleted")))

(defun delete-rows (&rest map-rows-options)
  (prog->
    (quote 0 -> *number-of-backward-eliminated-rows*)
    (quote nil -> *printing-deleted-messages*)
    (apply 'map-rows map-rows-options ->* row)
    (unstore-wff row "Deleted")))

(defun constraint-purify-row (row)
  (prog->
    (row-wff row -> wff)
    (constraint-purify-wff wff -> wff* constraint-alist-additions)
    (unless (eq wff wff*)
      (conjoin-alists (row-constraints row) constraint-alist-additions -> constraints*)
      (fail-when-constraint-false constraints*)
      (setf row (maybe-new-row row))
      (setf (row-wff row) wff*)
      (setf (row-constraints row) constraints*)
      (setf (row-reason row) `(purify ,(row-reason row)))))
  row)

(defun make-split (row wff answer polarity)
  (let* ((constraint-alist (row-constraints row))
	 (suppress-answer (let ((vars (variables answer)))
			    (and vars
				 (dolist (var vars t)
				   (when (or (variable-occurs-p var wff nil)
					     (variable-occurs-p var constraint-alist nil))
				     (return nil)))))))
    (make-row :wff (if (eq :pos polarity) wff (make-compound *not* wff))
	      :constraints constraint-alist
	      :answer (if suppress-answer false answer)
	      :supported (row-supported row)
	      :sequential (row-sequential row)
              :context (row-context row)
	      :reason (row-reason row)
              :conc-name (or (row-conc-name row)
                             (let ((name (row-name row)))
                               (and name (format nil "~A-" name))))
              :documentation (row-documentation row)
              :author (row-author row)
              :source (row-source row)
              :input-wff (row-input-wff row))))

(defun factorer (row)
  (when (row-hint-p row)
    (return-from factorer nil))
  (prog->
    (row-context-live? row ->nonnil context)
    (dopairs (atoms-in-wff2 (row-wff row) nil :pos 1) ->* x y)
    (when (and (or (eq (second x) (second y)) (eq :both (second x)) (eq :both (second y)))
               (implies (row-sequential row)
                        (or (atom-satisfies-sequential-restriction-p (first x) (row-wff row))
                            (atom-satisfies-sequential-restriction-p (first y) (row-wff row)))))
      (unify (first x) (first y) ->* subst)
      (catch 'fail
        (record-new-derived-row
         (make-row :wff (fail-when-true (instantiate (row-wff row) 1 subst))
                   :constraints (fail-when-constraint-false (instantiate (row-constraints row) 1 subst))
                   :answer (fail-when-disallowed (instantiate (row-answer row) 1 subst))
                   :supported (row-supported row)
                   :sequential (row-sequential row)
                   :context context
                   :reason `(factor ,row)))))))

(defun resolve-with-x=x (row)
  (when (row-hint-p row)
    (return-from resolve-with-x=x nil))
  (prog->
    (row-context-live? row ->nonnil context)
    (when (row-supported row)
      (map-atoms-in-wff (row-wff row) ->* atom polarity)
      (when (and (eq :neg polarity) (equality-p atom))
        (args atom -> args)
        (when (or (variable-p (first args)) (variable-p (second args)))
          (instantiate atom 1 -> atom*)
          (args atom* -> args*)
          (unify (first args*) (second args*) ->* subst)
          (when (make-resolventa row atom atom* true subst context)
            (return-from resolve-with-x=x t))))))
  nil)

(defun resolver (row1)
  (when (row-hint-p row1)
    (return-from resolver nil))
  (prog->
    (row-context-live? row1 ->nonnil context1)
    (use-literal-ordering-with-resolution? -> orderfun)
    (selected-atoms-in-row row1 orderfun -> selected-atoms-in-row1)
    (flet ((resolver1 (atom1 truthvalue1 truthvalue2 polarity1 polarity2)
             (prog->
               (quote nil -> atom1*)
               ;; apply resolve-code procedural attachments:
               (when (row-supported row1)
                 (dolist (and (compound-p atom1) (function-resolve-code (head atom1) truthvalue1)) ->* fun)
                 (funcall fun (setq-once atom1* (instantiate atom1 1)) nil ->* subst &optional residue)
                 (when (selected-atom-p atom1 polarity1 selected-atoms-in-row1 orderfun subst 1 atom1*)
                   (make-resolventa row1 atom1 atom1* truthvalue1 subst context1 residue)))
               ;; resolve row1 with other rows:
               (retrieve-unifiable-entries
                atom1
                nil
                (if (eq false truthvalue2)
                    #'tme-rows-containing-atom-positively
                    #'tme-rows-containing-atom-negatively)
                ->* atom2-entry row2s)
               (tme-term atom2-entry -> atom2)
               (quote nil -> atom2*)
               (map-rows :rowset row2s :reverse t ->* row2)
               (row-context-live? row2 ->nonnil context2)
               (selected-atoms-in-row row2 orderfun -> selected-atoms-in-row2)
               (when (and (if *interactive?
                              (implies *row2* (eq row2 *row2*))
                              (row-given-p row2))
                          (not (row-hint-p row2))
                          (or (and (row-unit-p row1) (row-unit-p row2))
                              (meets-binary-restrictions-p row1 row2))
                          (selected-atom-p atom2 polarity2 selected-atoms-in-row2 orderfun))
                 (setq-once atom1* (instantiate atom1 1))
                 (setq-once atom2* (instantiate atom2 2))
                 (unify atom1* atom2* nil ->* subst)
                 (when (and (selected-atom-p atom1 polarity1 selected-atoms-in-row1 orderfun subst 1 atom1*)
                            (selected-atom-p atom2 polarity2 selected-atoms-in-row2 orderfun subst 2 atom2*))
                   (make-resolvent row1 atom1 atom1* truthvalue1 
                                   row2 atom2 atom2* truthvalue2
                                   subst context1 context2))))))
      (prog->
        (dolist selected-atoms-in-row1 ->* x)
        (values-list x -> atom1 polarity1)
        (unless (eq :neg polarity1)
          (resolver1 atom1 false true :pos :neg))
        (unless (eq :pos polarity1)
          (resolver1 atom1 true false :neg :pos))))))

(defun code-resolver (row1)
  (when (row-hint-p row1)
    (return-from code-resolver nil))
  (prog->
    (when (row-supported row1)
      (row-context-live? row1 ->nonnil context1)
      (instantiate (row-wff row1) 1 -> wff1)
      (dolist (use-resolve-code?) ->* fun)
      (funcall fun wff1 nil ->* subst &optional wff1*)
      (make-resolventb row1 (or wff1* false) subst context1))))

(definline hyperresolution-electron-polarity ()
  ;; every atom in an electron has this polarity
  (if *negative-hyperresolution* :neg :pos))

(definline hyperresolution-nucleus-polarity ()
  ;; some atom in a nucleus has this polarity
  (if *negative-hyperresolution* :pos :neg))

(definline row-hyperresolution-electron-p (row)
  (if *negative-hyperresolution* (row-negative-p row) (row-positive-p row)))

(definline hyperresolution-orderfun ()
  (if *negative-hyperresolution*
      (use-literal-ordering-with-negative-hyperresolution?)
      (use-literal-ordering-with-hyperresolution?)))

(defun hyperresolver (row)
  (when (row-hint-p row)
    (return-from hyperresolver nil))
  (prog->
    (cond
      ((row-hyperresolution-electron-p row)
       (hyperresolution-orderfun -> orderfun)
       (dolist (selected-atoms-in-row row orderfun) ->* x)	;row is electron
       (values-list x -> atom2 polarity2)
       (if (eq :pos polarity2) false true -> truthvalue2)
       (prog->					;use procedural attachment as unit nucleus
         (row-context-live? row ->nonnil context)
         (when (row-supported row)
           (quote nil -> atom2*)
           (dolist (and (compound-p atom2) (function-resolve-code (head atom2) polarity2)) ->* fun)
           (funcall fun (setq-once atom2* (instantiate atom2 1)) nil ->* subst &optional residue)
           (selected-atoms-in-row row orderfun -> selected-atoms-in-row)
           (when (selected-atom-p atom2 polarity2 selected-atoms-in-row orderfun subst 1 atom2*)
             (make-resolventa row atom2 atom2* truthvalue2 subst context residue))))
       (prog->
       (quote nil -> atom2*)
       (retrieve-unifiable-entries
	 atom2
	 nil
	 (if *negative-hyperresolution*
             #'tme-rows-containing-atom-positively
             #'tme-rows-containing-atom-negatively)
	 ->* atom1-entry row1s)
       (tme-term atom1-entry -> atom1)
       (quote nil -> atom1*)
       (map-rows :rowset row1s :reverse t ->* row1)
       (when (and (or *interactive? (row-given-p row1))
                  (not (row-hint-p row1)))
	 (setq-once atom1* (instantiate atom1 1))
	 (setq-once atom2* (instantiate atom2 2))
	 (unify atom1* atom2* nil ->* subst)
	 (hyperresolver1 row1 atom1 row atom2 atom2* subst))))
      (t					;row is nucleus
       (let ((atoms nil) (atoms* nil))
	 (prog->
	  (map-atoms-in-wff (row-wff row) ->* atom polarity)
	  (when (and (eq (hyperresolution-nucleus-polarity) polarity)
		     (not (member atom atoms)))	;equal-p => eq for canonical terms
	    (push atom atoms)
	    (push (instantiate atom 1) atoms*)))
	 (when atoms*
	   (hyperresolver2 row nil (nreverse atoms*) 2 nil nil)))))))

(defun hyperresolver1 (nucleus atom1 electron atom2 atom2* subst)
  (let ((atoms nil) (atoms* nil))
    (prog->
      (map-atoms-in-wff (row-wff nucleus) ->* atom polarity)
      (when (and (neq atom atom1)		;equal-p => eq for canonical terms
		 (eq (hyperresolution-nucleus-polarity) polarity)
		 (not (member atom atoms)))	;equal-p => eq for canonical terms
	(push atom atoms)
	(push (instantiate atom 1) atoms*)))	;no dereferencing needed
    (hyperresolver2 nucleus (list (list electron atom2 atom2*)) (nreverse atoms*) 3 nil subst)))

(defun hyperresolver2 (nucleus electrons atoms* n residues subst)
  (declare (type fixnum n))
  (prog->
    (hyperresolution-orderfun -> orderfun)
    (cond
     ((null atoms*)
      (when (and (implies *interactive? (submultisetp *row2* (mapcar #'first electrons)))
		 (or (row-supported nucleus)
		     (some (lambda (x) (row-supported (first x))) electrons))
		 (selected-atoms-in-hyperresolution-electrons-p electrons subst))
	(make-hyperresolvent nucleus electrons residues subst)))
     (t
      (first atoms* -> atom*)
      (when (test-option9?)
        (let ((atom** (rewriter atom* subst)))
          ;; should record what rewrites are used
          (when (neq none atom*)
            (cond
             ((eq true atom**)
              (return-from hyperresolver2
                (unless *negative-hyperresolution*
                  (hyperresolver2 nucleus electrons (rest atoms*) n residues subst))))
             ((eq false atom**)
              (return-from hyperresolver2
                (when *negative-hyperresolution*
                  (hyperresolver2 nucleus electrons (rest atoms*) n residues subst))))
             (t
              (setf atom* atom**))))))
      (prog->
        (dolist (and (compound-p atom*)
                     (function-resolve-code (head atom*) (if *negative-hyperresolution* false true)))
          ->* fun)
        (funcall fun atom* subst ->* subst &optional residue)
        (cons (function-code-name (head atom*)) *resolve-functions-used* -> *resolve-functions-used*)
        (hyperresolver2 nucleus electrons (rest atoms*) n (cons-unless-nil residue residues) subst))
      (retrieve-unifiable-entries
       atom*
       subst
       (if *negative-hyperresolution* #'tme-rows-containing-atom-negatively #'tme-rows-containing-atom-positively)
       ->* atomn-entry rowns)
      (tme-term atomn-entry -> atomn)
      (quote nil -> atomn*)
      (map-rows :rowset rowns :reverse t ->* rown)
      (selected-atoms-in-row rown orderfun -> selected-atoms-in-rown)
      (when (and (or *interactive? (row-given-p rown))
                 (not (row-hint-p rown))
		 (row-hyperresolution-electron-p rown))
	(when (selected-atom-p
               atomn
               (hyperresolution-electron-polarity)
               selected-atoms-in-rown
               orderfun)
	  (unify (first atoms*) (setq-once atomn* (instantiate atomn n)) subst ->* subst)
	  (hyperresolver2 nucleus (cons (list rown atomn atomn*) electrons) (rest atoms*) (+ n 1) residues subst)))))))

(defun ur-resolver (row)
  (when (row-clause-p row)				;nucleus
    (ur-resolver1 row))
  (when (row-unit-p row)				;electron
    (prog->
      (map-atoms-in-wff (row-wff row) ->* atom2 polarity2)
      (setf atom2 (instantiate atom2 2))
      (retrieve-unifiable-entries
       atom2
       nil
       (if (eq :pos polarity2) #'tme-rows-containing-atom-negatively #'tme-rows-containing-atom-positively)
       ->* atom1-entry row1s)
      (tme-term atom1-entry -> atom1)
      (quote nil -> atom1*)
      (map-rows :rowset row1s :reverse t ->* row1)	;nucleus
      (when (and (or *interactive? (row-given-p row1))
                 (row-clause-p row1)
                 (not (row-hint-p row1))
                 (not (row-unit-p row1)))
        (setq-once atom1* (instantiate atom1 1))
        (unify atom1* atom2 ->* subst)
        (ur-resolve1 row1 (list row) nil nil subst (atoms-in-clause2 (row-wff row1) atom1) 3))))
  nil)

(defun ur-resolver1 (nucleus)
  (when (row-hint-p nucleus)
    (return-from ur-resolver1 nil))
  (ur-resolve1 nucleus nil nil nil nil (atoms-in-clause2 (row-wff nucleus)) 2))

(defun ur-resolve1 (nucleus electrons target-atom target-polarity subst l k)
  (declare (type fixnum k))
  (cond
   ((null l)
    (when (and (implies *interactive? (submultisetp *row2* electrons))
               (or (row-supported nucleus)
                   (some #'row-supported electrons))
               (implies (and target-atom
                             (use-literal-ordering-with-ur-resolution?)
                             (clause-p (row-wff nucleus)))
                        (literal-is-not-dominating-in-clause-p
                         (use-literal-ordering-with-ur-resolution?)
                         target-atom
                         target-polarity
                         (instantiate (row-wff nucleus) 1)
                         subst)))
      (make-ur-resolvent nucleus electrons target-atom target-polarity subst)))
   (t
    (let ((atom1 (instantiate (first (first l)) 1))
          (polarity1 (second (first l))))
      (when (null target-atom)
        (ur-resolve1 nucleus electrons atom1 polarity1 subst (rest l) k))
      (when (eq target-polarity polarity1)
        (prog->
          (unify target-atom atom1 subst ->* subst)
          (ur-resolve1 nucleus electrons target-atom target-polarity subst (rest l) k)))
      (prog->
        (dolist (and (compound-p atom1) (function-resolve-code (heada atom1) polarity1)) ->* fun)
        (funcall fun atom1 subst ->* subst &optional residue)
        (unless residue
          (cons (function-code-name (head atom1)) *resolve-functions-used* -> *resolve-functions-used*)
          (ur-resolve1 nucleus electrons target-atom target-polarity subst (rest l) k)))
      (prog->
        (retrieve-unifiable-entries
         atom1
         subst
         (if (eq :pos polarity1) #'tme-rows-containing-atom-negatively #'tme-rows-containing-atom-positively)
         ->* atomk-entry rowks)
        (tme-term atomk-entry -> atomk)
        (quote nil -> atomk*)
        (map-rows :rowset rowks :reverse t ->* rowk)
        (when (and (or *interactive? (row-given-p rowk))
                   (not (row-hint-p rowk))
                   (row-unit-p rowk))
          (setq-once atomk* (instantiate atomk k))
          (unify atom1 atomk* subst ->* subst)
          (ur-resolve1 nucleus (cons rowk electrons) target-atom target-polarity subst (rest l) (+ k 1))))))))

(defun backward-demodulate-by (row1)
  (when (row-hint-p row1)
    (return-from backward-demodulate-by nil))
  (loop for rewrite in (row-rewrites row1)
	as pattern = (rewrite-pattern rewrite)
	as value = (rewrite-value rewrite)
	as pattern-symbol-count = (rewrite-pattern-symbol-count rewrite)
	as cond = (rewrite-condition rewrite)
	as embeddings = (rewrite-embeddings rewrite)
	when (if (or (eq true value) (eq false value))
		 (and (use-simplification-by-units?)
		      (neq :forward (use-simplification-by-units?)))
		 (and (use-simplification-by-equalities?)
		      (neq :forward (use-simplification-by-equalities?))))
        do (prog->
             (row-context-live? row1 ->nonnil context1)
             (instantiate pattern 1 -> pattern*)
             (instantiate value 1 -> value*)
             (retrieve-instance-entries pattern* nil ->* e-entry)
             (tme-term e-entry -> e)
             (let ((row2s (tme-rows-containing-term e-entry)) e*)	;paramodulatable term?
               (unless (rowset-empty? row2s)
                 (when (block it
                         (prog->
                           (rewrite-patterns-and-values
                            pattern* value* pattern-symbol-count embeddings (symbol-count e) ->* pattern** value**)
                           (subsume pattern** e nil ->* subst)
                           (when (and (or (eq cond t) (funcall cond pattern* value* subst))
                                      (term-subsort-p value** pattern** subst))
                             (setf e* (instantiate value** subst))
                             (return-from it t)))
                         nil)
                   (prog->
                     (map-rows :rowset row2s :reverse t ->* row2)
                     (row-context-live? row2 ->nonnil context2)
                     (unless (or (eq row1 row2)
                                 (row-embedding-p row2)
                                 (row-deleted-p row2)
                                 (not (eq t (context-subsumes? context1 context2))))
                       (cond
                        ((row-hint-p row2)
                         (when (or (eq true value) (eq false value))
                           (pushnew row2 *hints-subsumed*))
                         nil)
                        ((or (eq true value) (eq false value))
                         (let ((result (make-resolvent1 row1 pattern (if (eq true value) false true)
                                                        row2 e value nil context1 context2)))
                           (when result
                             (unless (eq :tautology result)
                               (setf (row-reason result) `(rewrite ,row2 ,row1)))
                             result)))
                        (t
                         (make-demodulant row1 row2 (substitute e* e (row-wff row2)) context1 context2))
                        ->nonnil demodulant)
                       (if recursive-unstore
                           (recursively-unstore-wff row2 "Simplified" (lambda (x) (eq row1 x)))
                           (unstore-wff row2 "Simplified"))
                       (unless (eq :tautology demodulant)
                         (record-backward-simplifiable-wff demodulant)))))))))
  (setf *printing-deleted-messages* nil)
  (prog->
    (identity *hint-rows* -> hints)
    (unless (rowset-empty? hints)
      (row-wff row1 -> wff1)
      (when (equality-p wff1)
        (row-context-live? row1 ->nonnil context1)
        (identity nil -> wff1*)
        (map-rows :rowset hints ->* row2)
        (row-context-live? row2 ->nonnil context2)
        (unless (or (row-deleted-p row2)
                    (not (eq t (context-subsumes? context1 context2))))
          (setq-once wff1* (renumber-new wff1))
          (when (subsumes-p wff1* (row-wff row2))
            (pushnew row2 *hints-subsumed*))))))
  nil)

(defun paramodulater-from (row1)
  (when (row-hint-p row1)
    (return-from paramodulater-from nil))
  (prog->
    (use-literal-ordering-with-paramodulation? -> orderfun)
    (row-wff row1 -> wff1)
    (when (and (implies (and orderfun
                             (not (test-option3?))
                             (not (row-sequential row1))	;don't restrict to equality wff if sequential snark-20061213b
                             (clause-p wff1))
                        (positive-equality-wff-p wff1))
	       (implies (use-paramodulation-only-from-units?) (equality-p wff1)))
      (map-atoms-in-wff wff1 ->* atom1 polarity1)
      (when (and (neq polarity1 :neg)
		 (equality-p atom1)
                 (if (row-sequential row1)
                     (atom-satisfies-sequential-restriction-p atom1 wff1)
                     (implies orderfun (literal-satisfies-ordering-restriction-p
                                        orderfun atom1 :pos wff1))))
	(args atom1 -> args)
	(first args -> a)
	(second args -> b)
	(unless (eq a b)			;equal-p => eq for canonical terms
	  (simplification-ordering-compare-equality-arguments atom1 nil -> dir)
	  (setf a (instantiate a 1))
	  (setf b (instantiate b 1))
	  (unless (or (variable-p a) (eq '< dir))
	    (paramodulater-from1 row1 atom1 a b dir))
	  (unless (or (variable-p b) (eq '> dir))
	    (paramodulater-from1 row1 atom1 b a dir)))))))

(defun paramodulater-from1 (row1 equality1 pattern1* value1* dir)
  ;; row1 has the equality
  (declare (ignore dir))
  (prog->
    (row-context-live? row1 ->nonnil context1)
    (and (row-embedding-p row1) (embedding-variables row1 1) -> embedding-variables1)
    (retrieve-unifiable-entries pattern1* nil ->* term2-entry)
    (tme-term term2-entry -> term2)
    (unless (variable-p term2)
      (rows-containing-paramodulatable-term term2 -> row2s)
      (when row2s
        (setf row2s (impose-binary-restrictions row1 (if (and *interactive? *row2*)
                                                         (remove-if-not (lambda (x) (eq *row2* x)) row2s)
                                                         row2s)))
        (when row2s
          (instantiate term2 2 -> term2*)
          (and embedding-variables1		;unify-bag only cares if both terms are embeddings
               (loop for row2 in row2s
                     always (and (row-embedding-p row2)
                                 (or (equal-p term2 (first (args (row-wff row2))) nil)
                                     (equal-p term2 (second (args (row-wff row2))) nil))))
               (embedding-variables (car row2s) 2)
               -> embedding-variables2)
          (and embedding-variables2 (append embedding-variables1 embedding-variables2) -> *embedding-variables*)
          (when (allowable-embedding-superposition (row-embedding-p row1) (row-embedding-p (car row2s)))
            (unify pattern1* term2* nil ->* subst)
            (unless (or (equal-p pattern1* value1* subst)
;;                      (and (neq dir '>)
;;                           (neq dir '<)
;;                           (eq '< (simplification-ordering-compare-terms pattern1* value1* subst '<)))
                        )
              (dolist row2s ->* row2)
              (row-context-live? row2 ->nonnil context2)
              (make-paramodulant row1 equality1 value1* row2 term2* subst context1 context2))))))))

(defun paramodulater-to (row2)
  (when (row-hint-p row2)
    (return-from paramodulater-to nil))
  (prog-> 
    (quote nil -> done)
    (use-literal-ordering-with-paramodulation? -> orderfun)
    (row-wff row2 -> wff2)
    (implies (and orderfun
                  (not (test-option3?))
                  (clause-p wff2))
             (positive-equality-wff-p wff2)
             -> paramodulate-to-equalities)
    (dolist (selected-atoms-in-row row2 orderfun) ->* x)
    (values-list x -> atom2 polarity2)
    (cond
      ((and (eq :pos polarity2) (equality-p atom2))
       (when paramodulate-to-equalities
	 (args atom2 -> args)
	 (first args -> a)
	 (second args -> b)
	 (simplification-ordering-compare-equality-arguments atom2 nil -> dir)
	 (unless (eq '< dir)
	   (map-terms-in-term a nil polarity2 ->* term2 polarity)
	   (declare (ignore polarity))
	   (unless (or (eq term2 a) (variable-p term2) (member term2 done))
	     (paramodulater-to1 row2 term2 (instantiate term2 2) dir)
	     (push term2 done)))
	 (unless (eq '> dir)
	   (map-terms-in-term b nil polarity2 ->* term2 polarity)
	   (declare (ignore polarity))
	   (unless (or (eq term2 b) (variable-p term2) (member term2 done))
	     (paramodulater-to1 row2 term2 (instantiate term2 2) dir)
	     (push term2 done)))))
      (t
       (map-terms-in-atom atom2 nil :pos ->* term2 polarity)
       (declare (ignore polarity))
       (unless (or (variable-p term2) (member term2 done))
	 (paramodulater-to1 row2 term2 (instantiate term2 2) nil)
	 (push term2 done))))))

(defun paramodulater-to1 (row2 term2 term2* dir)
  (declare (ignore dir))
  (prog->
    (row-context-live? row2 ->nonnil context2)
    (when (row-supported row2)
      (dolist (and (compound-p term2*) (function-paramodulate-code (head term2*))) ->* fun)
      (funcall fun term2* nil ->* value1* subst)
      (make-paramodulanta value1* row2 term2* subst context2))
    (and (row-embedding-p row2)
         (or (equal-p term2 (first (args (row-wff row2))) nil)
             (equal-p term2 (second (args (row-wff row2))) nil))
         (embedding-variables row2 2) -> embedding-variables2)
    (retrieve-unifiable-entries term2* nil #'tme-rows-containing-paramodulatable-equality ->* pattern1-entry ws)
    (tme-term pattern1-entry -> pattern1)
    (instantiate pattern1 1 -> pattern1*)
    (dolist ws ->* w)
    (car w -> row1)
    (row-context-live? row1 ->nonnil context1)
    (when (and (not (row-hint-p row1)) (meets-binary-restrictions-p row2 row1))
      (cdr w -> value1)
      (unless (eq pattern1 value1)		;equal-p => eq for canonical terms
        (make-compound *=* pattern1 value1 -> equality1)
        (when (if (row-sequential row1)
                  (atom-satisfies-sequential-restriction-p equality1 (row-wff row1))
                  (let ((orderfun (use-literal-ordering-with-paramodulation?)))
                    (implies orderfun (literal-satisfies-ordering-restriction-p
                                       orderfun equality1 :pos (row-wff row1)))))
          (instantiate value1 1 -> value1*)
          (and embedding-variables2		;unify-bag only cares if both terms are embeddings
               (row-embedding-p row1)
               (embedding-variables row1 1)
               -> embedding-variables1)
          (and embedding-variables1 (append embedding-variables1 embedding-variables2) -> *embedding-variables*)
          (when (allowable-embedding-superposition (row-embedding-p row1) (row-embedding-p row2))
            (unify pattern1* term2* nil ->* subst)
            (unless (or (equal-p pattern1* value1* subst)
;;                      (and (neq dir '>)
;;                           (neq dir '<)
;;                           (eq '< (simplification-ordering-compare-terms pattern1* value1* subst '<)))
		        )
              (unless (eql (row-number row1) (row-number row2))
                ;;don't duplicate work (DO THIS IN IMPOSE-BINARY-RESTRICTIONS INSTEAD)
                (make-paramodulant row1 equality1 value1* row2 term2* subst context1 context2)))))))))

(defun paramodulation-allowable-p (term row)
  (prog->
    (row-wff row -> wff)
    (map-atoms-in-wff wff ->* atom polarity)
    (identity nil -> atom-not-selected)
    (cond
     ((and (eq :pos polarity) (equality-p atom))
      (args atom -> args)
      (simplification-ordering-compare-equality-arguments atom nil -> dir)
      (unless (eq '< dir)
        (when (if (row-embedding-p row) (equal-p term (first args) nil) (occurs-p term (first args) nil))
          (if (if (row-sequential row)
                  (atom-satisfies-sequential-restriction-p atom wff)
                  (let ((orderfun (use-literal-ordering-with-paramodulation?)))
                    (implies orderfun (literal-satisfies-ordering-restriction-p orderfun atom polarity wff))))
              (return-from paramodulation-allowable-p t)
              (setf atom-not-selected t))))
      (unless atom-not-selected
        (unless (eq '> dir)
          (when (if (row-embedding-p row) (equal-p term (second args) nil) (occurs-p term (second args) nil))
            (when (if (row-sequential row)
                      (atom-satisfies-sequential-restriction-p atom wff)
                      (let ((orderfun (use-literal-ordering-with-paramodulation?)))
                        (implies orderfun (literal-satisfies-ordering-restriction-p orderfun atom polarity wff))))
              (return-from paramodulation-allowable-p t))))))
     ((occurs-p term atom nil)
      (when (if (row-sequential row)
                (atom-satisfies-sequential-restriction-p atom wff)
                (let ((orderfun (use-literal-ordering-with-paramodulation?)))
                  (implies orderfun (literal-satisfies-ordering-restriction-p orderfun atom polarity wff))))
        (return-from paramodulation-allowable-p t)))))
  nil)

(defun rows-containing-paramodulatable-term (term)
  (rows :rowset (rows-containing-term term)
        :reverse t
        :test (lambda (row)
                (and (or *interactive? (row-given-p row))
                     (implies (use-paramodulation-only-into-units?) (row-unit-p row))
                     (paramodulation-allowable-p term row)))))

(defun make-embeddings (cc row)
  (unless (row-embedding-p row)
    (let ((wff (row-wff row)))
      (when (equality-p wff)
        (mvlet* (((:list a b) (args wff))
	         (a-associative (and (compound-appl-p a)
				     (function-associative (head a))
				     (function-unify-code (head a))))
	         (b-associative (and (compound-appl-p b)
				     (function-associative (head b))
				     (function-unify-code (head b)))))
          (with-clock-on embedding
	    (when (or a-associative b-associative)
	      (let ((dir (simplification-ordering-compare-terms a b)))
	        (cond
                 ((eq '> dir)
                  (when a-associative
                    (make-embeddings1 cc row a b)))
                 ((eq '< dir)
                  (when b-associative
                    (make-embeddings1 cc row b a)))
                 ((and (compound-appl-p a) (compound-appl-p b) (eq (head a) (head b)))
                  (make-embeddings1 cc row a b))
                 (t
                  (when a-associative
                    (make-embeddings1 cc row a b))
                  (when b-associative
                    (make-embeddings1 cc row b a))))))))))))

(defun make-embeddings1 (cc row a b)
  (let* ((head (head a))
	 (args (args a))
         (sort (associative-function-argument-sort head))
	 (newvar2 (make-variable sort))
	 (temp (append args (list newvar2))))
    (cond
     ((function-commutative head)
      (let ((a* (make-compound* head temp))
            (b* (make-compound head b newvar2)))		;might not be flattened
        (unless (subsumes-p (renumber (cons a b)) (cons a* b*))
          (funcall cc (make-embedding row a* b* t)))))
     (t
      (let ((newvar1 (make-variable sort))
            (abs (list (renumber (cons a b)))))
        (let ((a* (make-compound* head (cons newvar1 args)))
              (b* (make-compound head newvar1 b)))		;might not be flattened
          (unless (dolist (ab abs)
                    (when (subsumes-p ab (cons a* b*))
                      (return t)))
            (push (renumber (cons a* b*)) abs)
            (funcall cc (make-embedding row a* b* :l))))
        (let ((a* (make-compound* head temp))
              (b* (make-compound head b newvar2)))		;might not be flattened
          (unless (dolist (ab abs)
                    (when (subsumes-p ab (cons a* b*))
                      (return t)))
            (push (renumber (cons a* b*)) abs)
            (funcall cc (make-embedding row a* b* :r))))
        (let ((a* (make-compound* head (cons newvar1 temp)))
              (b* (make-compound head newvar1 b newvar2)))	;might not be flattened
          (unless (dolist (ab abs)
                    (when (subsumes-p ab (cons a* b*))
                      (return t)))
            (funcall cc (make-embedding row a* b* :l&r)))))))))

(defun make-embedding (row a1 b1 type)
  (make-row :wff (make-equality a1 b1 nil)
	    :constraints (row-constraints row)
	    :answer (row-answer row)
	    :supported (row-supported row)
            :sequential (row-sequential row)
            :context (row-context row)
	    :reason (if (eq t type) `(embed ,row) `(embed ,row ,type))))

(defun embedding-variables (embedding+ n)
  ;; may not return all embedding-variables because the embedding
  ;; (= (f a ?x) (f b ?x)) might be stored as (= (f a ?x) (f ?x b)) if f is AC
  (mvlet ((vars nil)
          ((:list arg1 arg2) (args (row-wff embedding+))))
    (when (and (compound-appl-p arg1)
               (compound-appl-p arg2)
               (eq (heada arg1) (heada arg2)))
      (let ((type (row-embedding-p embedding+)))
        (when (or (eq :l&r type) (eq :r type) (eq t type))
          (let ((x (first (last (argsa arg1))))
                (y (first (last (argsa arg2)))))
            (when (and (eq x y) (variable-p x))
              (push (instantiate x n) vars))))
        (when (or (eq :l&r type) (eq :l type))
          (let ((x (first (argsa arg1)))
                (y (first (argsa arg2))))
            (when (and (eq x y) (variable-p x))
              (push (instantiate x n) vars))))))
    vars))

(defun allowable-embedding-superposition (type1 type2)
  (or (null type1)
      (null type2)
      (and (eq t type1) (eq t type2))
      (and (eq :l type1) (eq :r type2))
      (and (eq :r type1) (eq :l type2))))

(defun meets-binary-restrictions-p (row1 row2)
  (and (or (row-supported row1) (row-supported row2))
       (implies (use-unit-restriction?) (or (row-unit-p row1) (row-unit-p row2)))
       (implies (use-input-restriction?) (or (row-input-p row1) (row-input-p row2)))))

(defun impose-binary-restrictions (row1 l &key (key #'identity))
  (remove-if-not (lambda (x) (meets-binary-restrictions-p row1 (funcall key x))) l))

(defun process-new-row-msg (control-string &rest args)
  (when (print-rows-when-processed?)
    (with-clock-on printing
      (format t "~%; ")
      (apply #'format t control-string args))))

(defun maybe-new-row (row)
  (cond
   ((symbolp (row-reason row))
    (let ((row* (make-row :wff (row-wff row)
                          :constraints (row-constraints row)
                          :answer (row-answer row)
                          :reason row
                          :context (row-context row)
                          :supported (row-supported row)
                          :sequential (row-sequential row))))
      (setf (row-wff row) (flatten-term (row-wff row) nil))
      (renumber-row row)
      (set-row-number row (incf *number-of-rows*))
      (incf *number-of-backward-eliminated-rows*)
      row*))
   (t
    row)))

(defun process-new-row (row agenda-value agenda)
  (with-clock-on process-new-row
    (let ((*processing-row* row)
          (wff (row-wff row))
	  (*rewriting-row-context* (row-context-live? row)))
      (unless *rewriting-row-context*
        (return-from process-new-row nil))
      (when (print-rows-when-processed?)
        (print-processed-row row))
      (when (eq true wff)
        (process-new-row-msg "Row wff is true.")
        (return-from process-new-row nil))
      (when (and (eq agenda *agenda-of-rows-to-process*)
	         (loop for parent in (row-parents row)
		       thereis (row-deleted-p parent)))
        (process-new-row-msg "Row parent is deleted.")
        (return-from process-new-row nil))
      (when (and (use-clausification?) (not (clause-p wff)))
        (process-new-row-msg "Row wff will be and-split.")
        #+ignore (progn (terpri) (print-term wff))
        (clausify wff (lambda (clause) (insert-row-into-agenda (make-split row clause (row-answer row) :pos) agenda-value *agenda-of-rows-to-process* t)))
        (return-from process-new-row nil))
      (dolist (fun (pruning-tests-before-simplification?))
        (when (funcall fun row)
	  (process-new-row-msg "Row is unacceptable before simplification.")
	  (return-from process-new-row nil)))
      (let ((answer (row-answer row))
	    constraint-alist
	    (and-split-this nil))
        (when (and (or (use-simplification-by-units?) (use-simplification-by-equalities?)) (not (row-hint-p row)))
          (let ((*rewrites-used* (row-rewrites-used row)))
	    (unless (row-embedding-p row)
	      (let ((wff* (with-clock-on forward-simplification (rewriter wff nil))))
                (unless (eq wff wff*)
                  (when (eq true wff*)
	            (process-new-row-msg "Simplified row wff is true.")
	            (return-from process-new-row nil))
                  (when *rewrites-used*
                    (setf row (maybe-new-row row))
	            (setf (row-rewrites-used row) *rewrites-used*))
	          (setf (row-wff row) (setf wff wff*))))
	      (when (rewrite-answers?)
	        (let ((answer* (with-clock-on forward-simplification (rewriter answer nil))))
                  (unless (eq answer answer*)
                    (when *rewrites-used*
                      (setf row (maybe-new-row row))
                      (setf (row-rewrites-used row) *rewrites-used*))
	            (setf (row-answer row) (setf answer answer*))))))
            (when (rewrite-constraints?)
              ;; inefficient to always rewrite constraints
              ;; can't rewrite constraints already in global data structures
              (let* ((constraints (row-constraints row))
                     (constraints* (with-clock-on forward-simplification (rewrite-constraint-alist constraints))))
                (unless (eq constraints constraints*)
                  (when *rewrites-used*
                    (setf row (maybe-new-row row))
                    (setf (row-rewrites-used row) *rewrites-used*))
                  (setf (row-constraints row) constraints*))))))
        (let ((*check-for-disallowed-answer* t))
          (when (answer-disallowed-p answer)
	    (process-new-row-msg "Row answer contains disallowed symbol.")
	    (return-from process-new-row nil)))
        (setf constraint-alist (row-constraints row))
        (when constraint-alist
	  (with-clock-off constraint-simplification
	    (setf (row-constraints row) (setf constraint-alist (simplify-constraint-alist constraint-alist)))))
        (dolist (x constraint-alist)
          (when (eq false (cdr x))
            (process-new-row-msg "Row constraint is false.")
            (return-from process-new-row nil)))
        (when (and (use-function-creation?) (equality-p wff))
	  (let* ((args (args wff))
	         (vars1 (variables (first args)))
	         (vars2 (variables (second args))))
            ;;	(when (and (set-difference vars1 vars2)
            ;;		   (set-difference vars2 vars1))
            ;;	  (let* ((vars (intersection vars1 vars2))
            ;;		 (fn (declare-function (newsym) (length vars)))
            ;;		 (val (make-compound* fn vars)))
	    (when (and vars1 vars2 (null (intersection vars1 vars2)))	;create only constants
	      (let* ((vars nil)
		     (fn (declare-constant (newsym)))
		     (val fn))
	        (if vars
		    (setf (function-created-p fn) t)
		    (setf (constant-created-p fn) t))
	        (when (eq :rpo (use-term-ordering?))
		  (rpo-add-created-function-symbol fn))
	        (setf (row-wff row) (setf wff (conjoin
					       (make-equality (first args) val)
					       (make-equality (second args) val))))
	        (setf and-split-this t)))))
        (when (or and-split-this (and (use-clausification?) (not (clause-p wff))))
          (process-new-row-msg "Row wff will be and-split.")
          #+ignore (progn (terpri) (print-term wff))
	  (clausify wff (lambda (clause) (insert-row-into-agenda (make-split row clause answer :pos) agenda-value *agenda-of-rows-to-process* t)))	
          (return-from process-new-row nil))
        (when (and (use-condensing?) (not (row-hint-p row)) (row-bare-p row) (not (literal-p wff)) (clause-p wff))
          (with-clock-on condensing
            (let ((wff* (condenser wff)))
              (unless (eq wff wff*)
                (setf row (maybe-new-row row))
                (setf (row-wff row) (setf wff wff*))
                (setf (row-reason row) (list 'condense (row-reason row)))))))
        (unless (or (not (use-subsumption?))
		    (and (use-simplification-by-units?) (row-bare-unit-p row))
                    (row-hint-p row)
		    (row-embedding-p row))
          (let ((subsuming-row (forward-subsumed row)))
	    (when subsuming-row
	      (process-new-row-msg "Row is forward subsumed by row ~(~A~)." (row-name-or-number subsuming-row))
	      (return-from process-new-row nil))))
        (dolist (fun (pruning-tests?))
	  (when (funcall fun row)
	    (process-new-row-msg "Row is unaccepable.")
	    (return-from process-new-row nil)))
        (when (and (use-embedded-rewrites?) (not (row-hint-p row)))
	  (make-embeddings #'record-new-embedding row))
        (prog->
	  (setf (row-wff row) (setf wff (flatten-term (row-wff row) nil)))
	  (renumber-row row)
          (set-row-number row (+ *number-of-rows* 1))
	  (when (prog1 (record-new-row-to-give row) (setf *printing-deleted-messages* nil))
	    (incf *number-of-rows*)
	    (when (print-rows-when-derived?)
	      (print-derived-row row))
            (let ((*hints-subsumed* nil))
	      (unless (or (not (use-subsumption?))
		          (eq :forward (use-subsumption?))
		          (and (use-simplification-by-units?)
			       (neq :forward (use-simplification-by-units?))
			       (row-bare-unit-p row))
		          (row-embedding-p row)
                          (row-hint-p row))
	        (backward-subsumption
                 (lambda (subsumed-row)
                   (if recursive-unstore
                       (recursively-unstore-wff subsumed-row "Subsumed" (lambda (x) (eq row x)))
                       (unstore-wff subsumed-row "Subsumed")))
                 (make-row0 :wff wff	;NOT RENUMBERED
                            :constraints constraint-alist
                            :answer answer
                            :context (row-context row)
                            :reason (row-reason row)))
                (setf *printing-deleted-messages* nil))
              (rowset-insert row *rows*)
	      (when (eq false wff)
                (if (row-constrained-p2 row)
                    (rowset-insert row *constraint-rows*)
                    (rowset-insert row *false-rows*)))
              (when (and (row-hint-p row) (equality-p wff))
                (rowset-insert row *hint-rows*))
	      (store-derived-wff row)
	      (unless (or (row-hint-p row) (row-embedding-p row))
	        (with-clock-on backward-simplification
	          (backward-demodulate-by row)))
              (when *hints-subsumed*
                (setf (row-hints-subsumed row) *hints-subsumed*)
                (record-new-row-to-give-again row)))))
        nil))))

(defun row-pref (row)
  (cond
   ((row-hints-subsumed row)
    0)
   (t
    (funcall (agenda-ordering-function?) row))))

(defun agenda-item-row (form)
  (ecase (car form)
    (giver
      (second form))
    (process-new-row
      (second form))))

(defun agenda-item-val (form)
  (ecase (car form)
    (giver
     (third form))
    (process-new-row
     (third form))))

(defun same-agenda-item-p (form1 form2)
  (let ((row1 (agenda-item-row form1))
	(row2 (agenda-item-row form2)))
    (and (iff (row-number row1) (row-number row2))
	 (implies (not (use-subsumption-by-false?)) (neq false (row-wff row1)))	;keep other proofs
	 (equal-p (row-wff row1) (row-wff row2))
	 (equal-alist-p (row-constraints row1) (row-constraints row2) nil)
	 (equal-p (row-answer row1) (row-answer row2))
	 ;; something for case
         (equal (row-context row1) (row-context row2))
         (iff (row-hint-p row1) (row-hint-p row2))
	 )))

(defun unstore-agenda-item (form)
  (ecase (first form)
    (giver
     (let ((row (second form)))
       (setf (row-agenda-entries row) (delete form (row-agenda-entries row)))	;don't double delete it from agenda
       (unstore-wff row "Deleted because agenda full"))
     (incf *number-of-agenda-full-deleted-rows*))))

(defun insert-row-into-agenda (row val agenda &optional at-front)
  (let ((v (if (row-number row)
               `(giver ,row ,val ,agenda)
               `(process-new-row ,row ,val ,agenda))))
    (push v (row-agenda-entries row))
    (agenda-insert v val agenda at-front)))

(defun delete-row-from-agenda (row &optional test)
  (let ((undeleted-agenda-entries nil) undeleted-agenda-entries-last)
    (dolist (x (row-agenda-entries row))
      (ecase (first x)
        ((giver process-new-row)
         (if (implies test (funcall test x))
             (agenda-delete x (third x) (fourth x))
             (collect x undeleted-agenda-entries)))))
      (setf (row-agenda-entries row) undeleted-agenda-entries)))

(defun pop-form-from-agenda ()
  (let ((form (pop-agenda *agenda*)))
    (dolist (x (rest form))
      (when (row-p x)
        (setf (row-agenda-entries x) (delete form (row-agenda-entries x)))))
    form))    

(defun record-new-embedding (row)
  (insert-row-into-agenda row 0 *agenda-of-new-embeddings-to-process*))

(defun record-new-input-wff (row)
  (insert-row-into-agenda row 0 *agenda-of-input-rows-to-process*))

(defun record-backward-simplifiable-wff (row)
  (cond
   ((eq false (row-wff row))
    (insert-row-into-agenda row 0 *agenda-of-false-rows-to-process*))
   (t
    (insert-row-into-agenda row 0 *agenda-of-backward-simplifiable-rows-to-process* t))))

(defun record-new-derived-row (row)
  (cond
   ((eq false (row-wff row))
    (insert-row-into-agenda row 0 *agenda-of-false-rows-to-process*))
   (t
    (mvlet (((:values row-pref at-front) (row-pref row)))
      (insert-row-into-agenda row row-pref *agenda-of-rows-to-process* at-front)))))

(defun record-new-row-to-give (row)
  (cond
   ((eq false (row-wff row))
    (insert-row-into-agenda row 0 *agenda-of-false-rows-to-process*))
   (t
    (mvlet (((:values row-pref at-front) (row-pref row)))
      (cond
       ((row-input-p row)
        (insert-row-into-agenda row row-pref *agenda-of-input-rows-to-give* at-front))
       ((let ((p (level-pref-for-giving?)))
          (and p (<= (row-level row) p)))
        (insert-row-into-agenda row (cons 3 row-pref) *agenda-of-rows-to-give* at-front))
       (t
        (insert-row-into-agenda row (cons 4 row-pref) *agenda-of-rows-to-give* at-front)))))))

(defun record-new-row-to-give-again (row)
  ;; when the value of row-pref changes because the row subsumes a hint,
  ;; use this to delete the row from the agenda and reinsert it with its higher priority
  (when (row-agenda-entries row)
    (delete-row-from-agenda row (lambda (x) (eq 'giver (first x))))
    (record-new-row-to-give row)))

(defun giver (given-row &optional agenda-value agenda)
  (declare (ignore agenda-value agenda))
  (unless (row-context-live? given-row)
    (return-from giver nil))
  (incf *number-of-given-rows*)
  (print-given-row given-row)
  (when (use-replacement-resolution-with-x=x?)
    (let ((*check-for-disallowed-answer* t))
      (when (resolve-with-x=x given-row)
        (return-from giver nil))))
  (store-given-row given-row)
  (when (row-hint-p given-row)
    (return-from giver nil))
  (when (eq false (row-wff given-row))
    (cond
     ((not (row-constrained-p2 given-row))
      (setf *proof* given-row)
      (when (print-final-rows?)
        (print-final-row given-row))
      (return-from giver t))
     (t
      (give-constraint-row given-row)
      (return-from giver nil))))
  (let ((use-factoring? (use-factoring?)))
    (when (and use-factoring?
               (not (literal-p (row-wff given-row)))
               (implies (eq :pos use-factoring?) (row-positive-p given-row))
               (implies (eq :neg use-factoring?) (row-negative-p given-row)))
      (with-clock-on factoring
        (factorer given-row))))
  (when (use-resolution?)
    (with-clock-on resolution
      (resolver given-row)))
  (when (use-hyperresolution?)
    (with-clock-on resolution
      (let ((*negative-hyperresolution* nil))
        (hyperresolver given-row))))
  (when (use-negative-hyperresolution?)
    (with-clock-on resolution
      (let ((*negative-hyperresolution* t))
        (hyperresolver given-row))))
  (when (use-ur-resolution?)
    (with-clock-on resolution
      (ur-resolver given-row)))
#+ignore
  (when (use-ur-pttp?)
    (with-clock-on resolution
      (ur-pttp given-row)))
  (when (use-paramodulation?)
    (with-clock-on paramodulation
      (paramodulater-from given-row)
      (unless (row-embedding-p given-row)
        (paramodulater-to given-row))))
  (when (use-resolve-code?)
    (with-clock-on resolution
      (code-resolver given-row)))
  nil)

(defun give-constraint-row (given-row)
  ;; given-row is of of the form 'constraints -> false'
  (when (and (row-from-conjecture-p given-row)	;assumed consistent otherwise
             (row-constraint-coverage (rows :rowset *constraint-rows* :reverse t)))
    (record-new-derived-row
     (make-row :wff false
               :answer (let ((n 0))
                         (disjoin*
                          (rows :collect (lambda (x) (instantiate (row-answer x) (incf n)))
                                :rowset *constraint-rows*
                                :reverse t)))
;;?            :supported (row-supported row)
;;?            :sequential (row-sequential row)
               :context (row-context given-row)
               :reason `(combine ,@(rows :rowset *constraint-rows* :reverse t))))
    (rowset-delete given-row *constraint-rows*)))

(defun break-snark ()				;ttp
  (setf *break-snark?* t))

(defun initialize-propositional-abstraction-of-input-wffs ()
  (let ((clause-set (make-dp-clause-set)))
    (dp-insert (list (list (function-name *=*) (function-arity *=*))) clause-set)
    (setf *propositional-abstraction-of-input-wffs* clause-set)))

(defun check-propositional-abstraction-of-input-wffs ()
  ;; clause-set should be checkpointed so that
  ;; assumptions and conjectures can be removed, e.g., by new-row-context
  (with-clock-on satisfiability-testing
    (let ((clause-set *propositional-abstraction-of-input-wffs*))
      (prog->
        (mapnconc-agenda *agenda-of-input-rows-to-process* ->* x)
        (second x -> row)
        (row-wff row -> wff)
        (quote t -> *propositional-abstraction-term-to-lisp*)
        (term-to-lisp wff -> wff*)
        (cond
         ((eq 'false wff*)
          (return-from check-propositional-abstraction-of-input-wffs nil))
         ((neq 'true wff*)
          (dp-insert-wff wff* clause-set :print-warnings nil)))
        nil)
;;    (dp-clauses 'print clause-set)
      (dp-satisfiable-p clause-set
                        :find-all-models 1
                        :print-summary nil
                        :print-warnings nil
                        :trace nil
                        :trace-choices nil
                        :branch-limit 10000000))))

(defun closure-init ()
  (setf *proof* nil)
  (when (use-assertion-analysis?)
    (complete-assertion-analysis))
  (when critique-options
    (with-clock-on printing
      (critique-options)))
  (unless rewrites-initialized
    (initialize-rewrites)
    (setf rewrites-initialized t))
  (unless (use-closure-when-satisfiable?)
    (let ((v (check-propositional-abstraction-of-input-wffs)))
      (when v
        (with-clock-on printing
          (warn "Propositional abstraction of input is satisfiable with model ~S." (first v)))
        (return-from closure-init :satisfiable))))
  nil)

(defun closure (&key
		(number-of-given-rows-limit (number-of-given-rows-limit?))
		(number-of-rows-limit (number-of-rows-limit?))
		(run-time-limit (run-time-limit?))
		(only-unnumbered-rows nil)
		(listen-for-commands (listen-for-commands?)))
  (unwind-protect
    (progn
      (setf *snark-is-running* t)
      (let ((v (closure-init)))
        (when v
          (return-from closure v)))
      (when number-of-given-rows-limit
        (incf number-of-given-rows-limit *number-of-given-rows*))
      (when number-of-rows-limit
        (incf number-of-rows-limit *number-of-rows*))
      (when run-time-limit
        (incf run-time-limit (total-run-time)))
      #+lcl5.0
      (when listen-for-commands
        (clear-input))
      (loop
        (when (and number-of-given-rows-limit (<= number-of-given-rows-limit *number-of-given-rows*))
          (return :number-of-given-rows-limit))
        (when (and number-of-rows-limit (<= number-of-rows-limit *number-of-rows*))
          (return :number-of-rows-limit))
        (when (and run-time-limit (<= run-time-limit (total-run-time)))
          (return :run-time-limit))
        (when *break-snark?*
          (clear-input)
          (setf *break-snark?* nil)
          (break "Break in closure at user request."))
        (when listen-for-commands
          (case (read-char-no-hang *terminal-io* nil nil)
            ((nil)
             )
            ((#\Q #\q)
             (return :user-quit))
            ((#\B #\b)
             (with-clock-on halted
               (clear-input)
               (break "Break in closure at user request.")))
            (otherwise
             (with-clock-on halted
               (clear-input)
               (when (yes-or-no-p "Stop now? ")
                 (return :user-quit))))))
        (when (and only-unnumbered-rows
                   (let ((v (agenda-first *agenda*)))
                     (and v (row-number (second v)))))
          (return :only-unnumbered-rows))
        (prog->
          (pop-form-from-agenda -> form)
          (cond
           ((null form)
            (return :agenda-empty))
           ((apply (car form) (cdr form))
            (return :proof-found))))))
    (setf *snark-is-running* nil)
    (when (print-summary-when-finished?)
      (terpri)
      (print-summary
       :clocks (print-clocks-when-finished?)
       :term-memory (print-term-memory-when-finished?)
       :agenda (print-agenda-when-finished?)))
    (nocomment)))

(defun proof ()
  ;; final row of the proof found in the most recent call on closure
  ;; nil if no proof was found in the most recent call on closure
  *proof*)

(defun proofs ()
  ;; final rows of all proofs
  (rows :rowset *false-rows*))

(defun answer (&optional term-to-lisp)
  (and *proof* (if term-to-lisp (term-to-lisp (row-answer *proof*)) (row-answer *proof*))))

(defun answers (&optional term-to-lisp)
  (rows :rowset *false-rows* :collect (lambda (*proof*) (answer term-to-lisp))))

(defun make-snark-system (&optional compile)
  (cl-user::make-snark-system compile))

#+cmu
(defun save-snark-system (&key (name "snark-cmucl.core"))
  (format t "~2%SNARK can be started by '~A -core ~A'" cl-user::*command-line-utility-name* name)
  (format t "~2%")
  (force-output)
  (extensions:save-lisp name))

#+sbcl
(defun save-snark-system (&key (name "snark-sbcl.core") executable)
  (cond
   (executable
    (format t "~2%SNARK can be started by '~A'" name)
    (format t "~%followed by (in-package :snark-user)")
    (format t "~2%")
    (force-output)
    (sb-ext:save-lisp-and-die name :executable t))
   (t
    (format t "~2%SNARK can be started by '~A --core ~A'" (first cl-user::*posix-argv*) name)
    (format t "~%followed by (in-package :snark-user)")
    (format t "~2%")
    (force-output)
    (sb-ext:save-lisp-and-die name))))

#+(and ccl (not mcl))
(defun save-snark-system (&key (name "snark-ccl.image"))
  (format t "~2%SNARK can be started by '~A --image-name ~A'" (first (ccl::command-line-arguments)) name)
  (format t "~%followed by (in-package :snark-user)")
  (format t "~2%")
  (force-output)
  (ccl:save-application name))

#+allegro
(defun save-snark-system (&key (name "snark-acl.dxl"))
  (format t "~2%SNARK can be started by '~A -I ~A'" (sys:command-line-argument 0) name)
  (format t "~%followed by (in-package :snark-user)")
  (format t "~2%")
  (force-output)
  (cl-user::dumplisp :name name)
  (quit))

#+clisp
(defun save-snark-system (&key (name "snark-lispinit.mem"))
  (format t "~2%SNARK can be started by '~A -M ~A'" "clisp" name)
  (format t "~2%")
  (force-output)
  (ext:saveinitmem name)
  (quit))

;;; wffs are stored with variables in block 0
;;;  these are used directly for demodulation and subsumption
;;; given wff is renumbered to have variables in block 1
;;; additional inference operation inputs are renumbered to have variables in block 2, 3, ...
;;; result of inference operation will have variables in blocks 1, 2, 3, ... (but not 0)
;;; and possibly "temporary" variables as well

;;; main.lisp EOF
