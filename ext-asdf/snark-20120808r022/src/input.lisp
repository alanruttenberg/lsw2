;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: input.lisp
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

(defvar *skolem-function-alist* nil)
(defvar *input-wff* nil)
(defvar *input-wff-substitution*)		;alist of (variable-name . variable) or (variable-name . skolem-term) pairs
(defvar *input-wff-substitution2*)
(defvar *input-wff-new-antecedents*)
(defvar *input-wff-modal-prefix*)
(defvar *input-proposition-variables* nil)	;for cnf and boolean ring rewrites

(defun keyword-argument-list-p (x)
  (or (null x)
      (and (consp x)
	   (keywordp (first x))
	   (consp (rest x))
	   (keyword-argument-list-p (rrest x)))))

(defun can-be-name1 (x &optional ?ok)
  (and (symbolp x)
       (not (null x))
       (neq none x)
       (neq true x)
       (neq false x)
       (let ((s (symbol-name x)))
         (and (<= 1 (length s))
              (if ?ok t (not (variable-symbol-prefixed-p s)))))))

(defun can-be-free-variable-name (x &optional action)
  ;; a free variable in an input formula is represented
  ;; by a symbol that starts with a variable-symbol-prefix
  (or (and (can-be-name1 x t)
           (variable-symbol-prefixed-p x))
      (and action (funcall action "~S cannot be the name of a free variable." x))))

(defun can-be-variable-name (x &optional action)
  ;; a bound variable is represented like a free variable, or by an ordinary symbol
  (or (can-be-name1 x t)
      (and action (funcall action "~S cannot be the name of a variable." x))))

(defun can-be-constant-name (x &optional action)
  (or (can-be-name1 x)
      (null x)
      (builtin-constant-p x)
      (and (symbolp x) (= 0 (length (symbol-name x))))
      (and action (funcall action "~S cannot be the name of a constant." x))))

(defun can-be-constant-alias (x &optional action)
  (or (can-be-name1 x)
      (and (symbolp x) (= 0 (length (symbol-name x))))
      (and action (funcall action "~S cannot be the alias of a constant." x))))

(defun can-be-proposition-name (x &optional action)
  (or (or (eq true x)					;allow internal true and false values in input
          (eq false x)			
          (can-be-name1 x))
      (and action (funcall action "~S cannot be the name of a proposition." x))))
                    
(defun can-be-function-name (x &optional action)
  (or (can-be-name1 x)
      (and action (funcall action "~S cannot be the name of a function." x))))

(defun can-be-relation-name (x &optional action)
  (or (and (can-be-name1 x)
           (neq '$$quote x))
      (and action (funcall action "~S cannot be the name of a relation." x))))

(defun can-be-logical-symbol-name (x &optional action)
  (or (can-be-name1 x)
      (and action (funcall action "~S cannot be the name of a logical symbol." x))))

(defun can-be-sort-name (x &optional action)
  ;; disallow names with "&" to avoid confusion with SNARK created sorts
  ;; disallow names with variable-sort-marker that is used to mark sorts in variable names
  (or (top-sort-name? x)
      (and (can-be-name1 x)
           (not (eq 'and x))
           (not (eq 'or x))
           (not (eq 'not x))
           (let ((s (symbol-name x)))
             (and (not (find (variable-sort-marker?) s))
                  (or (null (symbol-package x)) (not (find #\& s))))))
      (and action (funcall action "~S cannot be the name of a sort." x))))

(defun can-be-row-name (x &optional action)
  (or (can-be-name1 x)
      (and action (funcall action "~S cannot be the name of a row." x))))

(defun can-be-constant-or-function-name (x &optional action)
  (or (can-be-constant-name x)
      (can-be-function-name x)
      (and action (funcall action "~S cannot be the name of a constant or function." x))))

(defun check-usable-head1 (head)
  ;; some operations cannot deal with function/relation symbols
  ;; with special input handling
  (when (function-input-code head)
    (with-standard-io-syntax2
      (error "~S cannot be used as a ~A here." (function-name head) (function-kind head))))
  head)

(defun cerror1 (datum &rest args)
  (apply #'cerror "Input it anyway, but this may result in additional errors." datum args))

(defun cerror2 (datum &rest args)
  (apply #'cerror "Ignore this sort declaration, but this may result in additional errors." datum args))

(defun variable-symbol-prefixed-p (x &optional (prefixes (variable-symbol-prefixes?)))
  ;; check whether symbol or string x begins with variable prefixes (like ?, _, @, or "...")
  ;; if so, return the number of characters in the prefix
  ;; otherwise return nil
  (let* ((s (string x))
         (len (length s))
         (pos 0))
    (loop
      (dolist (prefix prefixes (return-from variable-symbol-prefixed-p (and (/= 0 pos) pos)))
        (cond
         ((characterp prefix)
          (when (and (> len pos) (eql prefix (char s pos)))
            (setf pos (+ pos 1))
            (return)))
         (t
          (let* ((prefix (string prefix))
                 (plen (length prefix)))
            (when (and (>= len (+ pos plen)) (string= prefix s :start2 pos :end2 (+ pos plen)))
              (setf pos (+ pos plen))
              (return)))))))))

(defun unsortable-variable-name (name)
  ;; SNARK output uses ?, ?X, ?Y, ?Z, ?U, ?V, ?W, ?X1, ?Y1, ?Z1, ?U1, ?V1, ?W1, ...
  ;; as unsorted variables; to enable SNARK to faithfully input its own output,
  ;; don't allow these variables to be declared with a sort
  (let* ((s (symbol-name name))
         (v (variable-symbol-prefixed-p s (list (first (variable-symbol-prefixes?))))))
    (and v
         (let ((len (length s)))
           (or (eql len v)
               (and (member (char s v) '(#\X #\Y #\Z #\U #\V #\W #\x #\y #\z #\u #\v #\w))
                    (or (eql (+ 1 v) len)
                        (null (position-if-not #'digit-char-p s :start (+ 1 v))))))))))

(defun sort-from-variable-name (name)
  ;; ?name.sort is the preferred way to input sorted variables
  ;; ?sort<digit>* works too with deprecated use-variable-name-sorts option (but not for sort names that end in digits or sorts named x,y,z,u,v,w)
  (let* ((s (symbol-name name))
         (p (position (variable-sort-marker?) s :from-end t)))
    (cond
     (p						;interpret variable names that end with #sort like ?i2#integer
      (the-sort (intern (subseq s (+ p 1)) :snark-user)))
     ((use-variable-name-sorts?)		;old style try to interpret as a sort the substring between ?* at start and digit* at end
      (let ((m (or (variable-symbol-prefixed-p s) 0))
            (n (position-if-not #'digit-char-p s :from-end t)))
        (cond
         ((> m n)
          none)
         ((and (= m n) (< 0 m) (member (char s m) '(#\X #\Y #\Z #\U #\V #\W #\x #\y #\z #\u #\v #\w)))
          none)
         (t
          (mvlet (((values sym found) (find-symbol (subseq s m (+ n 1)) :snark-user)))
            (if found (find-symbol-table-entry sym :sort) none))))))
     (t
      none))))

(defun declare-variable (name &key (sort (top-sort-name) sort-supplied-p))
  ;; return same variable every time for same input free variable
  (can-be-variable-name name 'error)
  (setf sort (the-sort sort))
  (let* ((v (find-or-create-symbol-table-entry name :variable))
         (vsort (variable-sort v)))
    (when (eq none (variable-sort v))		;new variable
      (unless (eq none (setf vsort (sort-from-variable-name name)))
        (setf (variable-sort v) vsort)))
    (cond
     ((eq none vsort)
      (cl:assert (not (and (not (top-sort? sort)) (unsortable-variable-name name)))
                 ()
                 "Cannot declare ~A as variable of sort ~A; ~A is unsorted."
                 name (sort-name sort) name)
      (setf (variable-sort v) sort))
     (sort-supplied-p
      (cl:assert (same-sort? sort vsort) ()
                 "Cannot declare ~A as variable of sort ~A; ~A is of sort ~A."
                 name (sort-name sort) name (sort-name vsort))))
    v))

;;; Convert Lisp S-expression for formula into correct internal form for theorem prover
;;; Also eliminate quantifiers and modal operators

;;; after input-wff, *input-wff-substitution2* contains the substitutions for all
;;; bound variables in the wff; it will be misleading if bound variable names are
;;; repeated or if variable names occur unbound as constants

(defun input-wff (wff &key (polarity :pos) (clausify nil) (*input-wff-substitution* nil))
  (when (stringp wff)
    (setf wff (read-tptp-term wff :case (readtable-case *readtable*))))
  (let ((*input-wff* wff)
        (*input-wff-substitution2* nil)
        (*input-wff-new-antecedents* true)
	(*input-wff-modal-prefix* nil))
    (let ((usr (use-sort-relativization?)))
      (when usr
	(let ((l nil))
	  (dolist (x (input-variables-in-form wff nil nil))
            (when (variable-p (cdr x))
	      (let ((sort (variable-sort (cdr x))))
                (unless (top-sort? sort)
		  (push `(,(sort-name sort) ,(car x)) l)))))
	  (when l
	    (setf wff (list 'implies
			    (if (null (rest l))
				(first l)
				(cons 'and (nreverse l)))
			    wff))))))
    (let ((wff* (input-wff1 wff polarity)))
      (unless (eq true *input-wff-new-antecedents*)
        (setf wff* (make-implication *input-wff-new-antecedents* wff*)))
      (when clausify
        (setf wff* (clausify wff*)))
      (values wff* nil *input-wff* *input-wff-substitution2*))))

(defun input-wff1 (wff polarity)
  (when (stringp wff)
    (setf wff (read-tptp-term wff :case (readtable-case *readtable*))))
  (cond
   ((atom wff)
    (input-atom wff polarity))
   (t
    (let ((head (input-logical-symbol (first wff))))
      (if (neq none head)
          (dolist (fun (function-input-code head) (make-compound* head (input-wffs1 head (rest wff) polarity)))
            (let ((v (funcall fun head (rest wff) polarity)))
              (unless (eq none v)
                (return v))))
          (input-atom wff polarity))))))

(defun input-wffs1 (head args polarity)
  (input-wffs2 args polarity (function-polarity-map head)))

(defun input-wffs2 (wffs polarity polarity-map)
  (lcons (input-wff1 (first wffs) (map-polarity (first polarity-map) polarity))
	 (input-wffs2 (rest wffs) polarity (rest polarity-map))
	 wffs))

(defun input-quoted-constant (head args polarity)
  (require-n-arguments head args polarity 1)
  (input-constant-symbol (cons '$$quote args)))

(defun input-equality (head args polarity)
  ;; see related code in input-function-as-relation
  (require-n-arguments head args polarity 2)
  (let (fn)
    (cond
     ((and (consp (first args))
           (member 'input-function-as-relation
                   (function-input-code (setf fn (input-function-symbol (first (first args)) (length (rest (first args))))))))
      (input-atom `(,(function-name fn) ,@(rest (first args)) ,(second args)) polarity))
     ((and (consp (second args))
           (member 'input-function-as-relation
                   (function-input-code (setf fn (input-function-symbol (first (second args)) (length (rest (second args))))))))
      (input-atom `(,(function-name fn) ,@(rest (second args)) ,(first args)) polarity))
     (t
      (input-form* head args polarity)))))

(defun input-disequality (head args polarity)
  (declare (ignore head))
  (make-compound *not* (input-equality *=* args (opposite-polarity polarity))))

(defun input-negation (head args polarity)
  (if (and (test-option6?) (use-clausification?))
      (negate0 (input-wffs1 head args polarity))
      (negate* (input-wffs1 head args polarity))))

(defun input-conjunction (head args polarity)
  (conjoin* (input-wffs1 head args polarity)))

(defun input-disjunction (head args polarity)
  (disjoin* (input-wffs1 head args polarity)))

(defun input-implication (head args polarity)
  (if (eql 2 (length args))
      (make-implication* (input-wffs1 head args polarity))
      (input-kif-forward-implication head args polarity t)))

(defun input-reverse-implication (head args polarity)
  (if (eql 2 (length args))
      (make-reverse-implication* (input-wffs1 head args polarity))
      (input-kif-backward-implication head args polarity t)))

(defun input-kif-forward-implication (head args polarity &optional rep)
  (require-n-or-more-arguments head args polarity 1)
  (when rep
    (report-not-2-arguments-implication head args))
  (input-wff1
   (cond
    ((null (rest args))
     (first args))
    ((null (rrest args))
     `(implies ,(first args) ,(second args)))
    (t
     `(implies (and ,@(butlast args)) ,(first (last args)))))
   polarity))

(defun input-kif-backward-implication (head args polarity &optional rep)
  (require-n-or-more-arguments head args polarity 1)
  (when rep
    (report-not-2-arguments-implication head args))
  (input-wff1
   (cond
    ((null (rest args))
     (first args))
    ((null (rrest args))
     `(implied-by ,(first args) ,(second args)))
    (t
     `(implied-by ,(first args) (and ,@(rest args)))))
   polarity))

(defun input-nand (head args polarity)
  (declare (ignore head))
  (input-wff1 `(not (and ,@args)) polarity))

(defun input-nor (head args polarity)
  (declare (ignore head))
  (input-wff1 `(not (or ,@args)) polarity))

(defun input-lisp-list (head args polarity)
  (declare (ignore head))
  (input-terms args polarity))

(defun input-lisp-list* (head args polarity)
  (require-n-or-more-arguments head args polarity 1)
  (nconc (input-terms (butlast args) polarity) (input-term1 (first (last args)) polarity)))

(defun input-function-as-relation-result-sort2 (head args)
  (let* ((arity (+ (length args) 1))
         (rel (find-symbol-table-entry (function-name head) :relation arity)))
    (if (eq none rel)
        (top-sort)
        (asa-arg-sort (function-argument-sort-alist rel) arity))))

(defun input-function-as-relation-result-sort (head args)
  (let ((resultsort (sort-intersection
                     (function-sort head)
                     (input-function-as-relation-result-sort2 head args))))
    (cl:assert resultsort)
    resultsort))

(defun input-function-as-relation (head args polarity &optional (new-head-name (function-name head)))
  ;; see related code in input-equality
  (let* ((resultsort (input-function-as-relation-result-sort head args))
         (resultvar (if (top-sort? resultsort)
                        (make-symbol (to-string (first (variable-symbol-prefixes?)) (nonce)))
                        (make-symbol (to-string (first (variable-symbol-prefixes?)) resultsort (nonce)))))
         (antecedent (input-wff1 (cons new-head-name (append args (list resultvar))) :neg)))
    (setf *input-wff-new-antecedents* (conjoin *input-wff-new-antecedents* antecedent))
    (input-term1 resultvar polarity)))

(defun input-float-function-as-relation (head args polarity)
  (let* ((str (symbol-name (function-name head)))
         (len (length str)))
    (cl:assert (string-equal str "_float" :start1 (- len 6):end1 len))
    (input-function-as-relation head args polarity (intern (subseq str 0 (- len 6)) :snark))))

(defun input-relation-as-function (head args polarity)
  (input-atom (list '= (cons (function-name head) (butlast args)) (first (last args))) polarity))

(defun input-equivalence (head args polarity)
  (cond
    ((null args)
     true)
    ((null (rest args))
     (input-wff1 (first args) polarity))
    ((and (not (null (cddr args))) (eql 2 (function-arity head)))
     (input-equivalence head (list (first args) (cons (function-name head) (rest args))) polarity))
    ((eq :both polarity)
     (make-equivalence* (input-wffs1 head args polarity)))
    ((catch 'needs-strict-polarity
       (make-equivalence* (input-wffs1 head args polarity)))
     )
    (t
     (let ((x (first args))
	   (y (if (null (cddr args)) (second args) (cons (function-name head) (rest args)))))
       (input-wff1 (if (eq :neg polarity)
                       `(or (and ,x ,y) (and (not ,x) (not ,y)))
                       `(and (implies ,x ,y) (implied-by ,x ,y)))
                   polarity)))))

(defun input-exclusive-or (head args polarity)
  (cond
    ((null args)
     false)
    ((null (rest args))
     (input-wff1 (first args) polarity))
    ((and (not (null (cddr args))) (eql 2 (function-arity head)))
     (input-exclusive-or
       head (list (first args) (cons (function-name head) (rest args))) polarity))
    ((eq :both polarity)
     (make-exclusive-or* (input-wffs1 head args polarity)))
    ((catch 'needs-strict-polarity
       (make-exclusive-or* (input-wffs1 head args polarity)))
     )
    (t
     (let ((x (first args))
	   (y (if (null (cddr args)) (second args) (cons (function-name head) (rest args)))))
       (input-wff1 (if (eq :neg polarity)
                       `(or (and ,x (not ,y)) (and (not ,x) ,y))
                       `(and (or ,x ,y) (or (not ,x) (not ,y))))
                   polarity)))))

(defun input-conditional (head args polarity)
  (require-n-arguments head args polarity 3)
  (cond
    ((eq :both polarity)
     (make-conditional
      (input-wff1 (first args) :both)
      (input-wff1 (second args) polarity)
      (input-wff1 (third args) polarity)))
    ((catch 'needs-strict-polarity
       (make-conditional
        (input-wff1 (first args) :both)
        (input-wff1 (second args) polarity)
        (input-wff1 (third args) polarity)))
     )
    (t
     (input-wff1 (if (eq :neg polarity)
                     `(or (and ,(first args) ,(second args))
                          (and (not ,(first args)) ,(third args)))
                     `(and (implies ,(first args) ,(second args))
                           (implies (not ,(first args)) ,(third args))))
                 polarity))))

(defun input-conditional-answer (head args polarity)
  (require-n-arguments head args polarity 3)
  (make-conditional-answer
     (input-wff1 (first args) :both)
     (input-wff1 (second args) polarity)
     (input-wff1 (third args) polarity)))

(defun input-quantification (head args polarity)
  (cond
   ((eq :both polarity)
    (throw 'needs-strict-polarity nil))
   (t
    (unless (eql 2 (length args))
      ;; (forall (vars) form . forms) means (forall (vars) (implies (and . forms) form))
      ;; (exists (vars) form . forms) means (exists (vars) (and form . forms))
      (require-n-or-more-arguments head args polarity 2)
      (report-not-2-arguments-quantification head args)
      (setf args
            (list (first args)
                  (cond
                   ((eq *forall* head)
                    `(=> ,@(rest args)))
                   ((eq *exists* head)
                    `(and ,@(rest args)))))))
    (let ((var-specs (input-quantifier-variables (first args)))
          (form (second args))
          (substitution *input-wff-substitution*)
          *input-wff-substitution*)
      (cond
       ((or (and (eq :pos polarity) (eq *forall* head))
            (and (eq :neg polarity) (eq *exists* head)))
        ;; add (variable-name . variable) pairs to substitution
        (dolist (var-spec var-specs)
          (let ((var (first var-spec)))
            (push (cons var (make-variable-from-var-spec var-spec)) substitution)
            (push (car substitution) *input-wff-substitution2*)))
        (setf *input-wff-substitution* substitution))
       ((or (and (eq :pos polarity) (eq *exists* head))
            (and (eq :neg polarity) (eq *forall* head)))
        (let ((free-vars-in-form (input-variables-in-form form (mapcar #'first var-specs) substitution)))
          ;; add (variable-name . skolem-term) pairs to substitution
          (dolist (var-spec var-specs)
            (let ((var (first var-spec)))
              (push (cons var (if (use-quantifier-preservation?)
                                  (make-variable-from-var-spec var-spec)
                                  (create-skolem-term var-spec form free-vars-in-form polarity)))
                    substitution)
              (push (car substitution) *input-wff-substitution2*))))
        (setf *input-wff-substitution* substitution))
       (t
        (unimplemented)))
      (when (or (eq *forall* head)
                (eq *exists* head))
        (let ((usr (use-sort-relativization?))
              (l nil))
          (dolist (var-spec var-specs)
            (let ((sort (getf (rest var-spec) :sort)))
              (when (and (not (top-sort-name? sort))
                         (or usr (getf (rest var-spec) :sort-unknown)))
                (push `(,(sort-name sort) ,(first var-spec)) l))))
          (when l
            (setf form (list (if (eq *forall* head) 'implies 'and)
                             (if (null (rest l)) (first l) (cons 'and (nreverse l)))
                             form)))))
      (cond
       ((use-quantifier-preservation?)
        (make-compound
         head
         (input-terms (mapcar #'first var-specs) polarity)
         (input-wff1 form polarity)))
       (t
        (input-wff1 form polarity)))))))

(defun input-quantifier-variable (var-spec)
  ;; var-spec should be of form
  ;;  variable-name
  ;; or
  ;;  (variable-name . keyword-argument-list)
  ;; such as
  ;;  (variable-name :sort sort-name)
  ;; or
  ;;  (variable-name restriction-name . keyword-argument-list)
  ;; such as
  ;;  (variable-name restriction-name) - KIF
  ;; interpeted as
  ;;  (variable-name :sort restriction-name . keyword-argument-list)
  ;;
  ;; output is always of form
  ;;  (variable-name . keyword-argument-list)
  (cond
   ((atom var-spec)
    (setf var-spec (list var-spec)))
  ((and (evenp (length var-spec)) (top-sort-name? (second var-spec)))
    ;; ignore top-sort restriction iff :sort is specified
    (setf var-spec
          (if (getf (cddr var-spec) :sort)
              (list* (first var-spec) (cddr var-spec))
              (list* (first var-spec) :sort (second var-spec) (cddr var-spec)))))
   ((evenp (length var-spec))
    ;; restriction-name is interpreted as sort (possibly unknown)
    (cl:assert (equal (second var-spec) (getf (cddr var-spec) :sort (second var-spec))) ()
               "In quantification, ~S has both a restriction and a sort." var-spec)
    (setf var-spec
          (cond
           ((sort-name-expression? (second var-spec))
            (list* (first var-spec) :sort (second var-spec) (cddr var-spec)))
           (t
            (list* (first var-spec) :sort (second var-spec) :sort-unknown t (cddr var-spec)))))))
  (cl:assert (keyword-argument-list-p (rest var-spec)) ()
	     "In quantification, ~S is not a keyword argument list." (rest var-spec))
  (let ((var (first var-spec))
        (sort (getf (rest var-spec) :sort none))
        (sort-unknown (getf (rest var-spec) :sort-unknown)))
    (cl:assert (can-be-variable-name var) () "In quantification, ~S is not a variable name." var)
    (cond
     ((neq none sort)
      (cond
       (sort-unknown
        (declare-variable var))
       (t
        ;; sort must have been declared
        (the-sort sort)
        (declare-variable var)))
      (append var-spec
              '(:skolem-p t)
              `(:allowed-in-answer ,(allow-skolem-symbols-in-answers?))))
     (t
      (append var-spec 
              `(:sort ,(sort-name (variable-sort (declare-variable var))))
              '(:skolem-p t)
              `(:allowed-in-answer ,(allow-skolem-symbols-in-answers?)))))))

(defun make-variable-from-var-spec (var-spec)
  (if (getf (rest var-spec) :sort-unknown)
      (make-variable)
      (make-variable (the-sort (getf (rest var-spec) :sort)))))

(defun input-quantifier-variables (var-specs)
  ;; CycL requires single variable-name,
  ;; KIF 3.0 allows it,
  ;; KIF proposed ANSI standard disallows it
  (unless (listp var-specs)
    (setf var-specs (list var-specs)))
  (cl:assert (and (listp var-specs) (not (keywordp (second var-specs)))) ()
             "Quantifier requires a list of bound variables.")
  (setf var-specs (mapcar #'input-quantifier-variable var-specs))
  (setf var-specs (remove-duplicates
                   var-specs
                   :test (lambda (x y)
                           (when (eq (first x) (first y))
                             (funcall (if (equal (rest x) (rest y)) 'warn 'error)
                                      "In quantification, variable ~A is being rebound."
                                      (first x))
                             t))))
  (dolist (x var-specs)
    (when (assoc (first x) *input-wff-substitution*)
      (warn "In quantification, variable ~A is being rebound." (first x))))
  var-specs)

(defun input-variables-in-form (expr vars substitution &optional result)
  ;; excluding vars
  (cond
    ((atom expr)
     (let ((v nil))
       (cond
        ((member expr vars)
         result)
        ((setf v (assoc expr substitution))
         (cond
          ((variable-p (cdr v))
           (if (rassoc (cdr v) result) result (nconc result (list v))))
          ((compound-p (cdr v))
           (dolist (x (args (cdr v)))
             (unless (rassoc x result)
               (setf result (nconc result (list (cons (car (rassoc x substitution)) x))))))
           result)
          (t
           result)))
        ((can-be-free-variable-name expr)
         (setf v (declare-variable expr))
         (if (rassoc v result) result (nconc result (list (cons expr v)))))
        (t
         result))))
    ((eq 'quote (first expr))
     result)
    ((let ((v (input-logical-symbol (first expr))))
       (or (eq *forall* v) (eq *exists* v)))
     (dolist (var-spec (input-quantifier-variables (second expr)))
       (pushnew (first var-spec) vars))
     (input-variables-in-form
      (third expr)
      vars
      substitution
      result))
    (t
     (dolist (x (rest expr))
       (setf result (input-variables-in-form x vars substitution result)))
     result)))

(defun create-skolem-term (var-spec form free-vars-in-form polarity)
  (let ((sort (getf (rest var-spec) :sort))
        (sort-unknown (getf (rest var-spec) :sort-unknown))
	(newskfn (create-skolem-symbol var-spec form (mapcar #'car free-vars-in-form) polarity)))
    (setf var-spec (copy-list var-spec))
    (remf (rest var-spec) :sort)
    (remf (rest var-spec) :sort-unknown)
    (remf (rest var-spec) :conc-name)
    (cond
      ((null free-vars-in-form)
       (setf newskfn (apply #'declare-constant newskfn (rest var-spec)))
       (when (and (not (top-sort-name? sort)) (not sort-unknown))
	 (declare-constant-sort newskfn sort))
       newskfn)
      (t
       (setf newskfn (apply #'declare-function newskfn (length free-vars-in-form) (rest var-spec)))
       (when (and (not (top-sort-name? sort)) (not sort-unknown))
	 (declare-function-sort newskfn (cons sort (consn (top-sort-name) nil (length free-vars-in-form)))))
       (make-compound* newskfn (mapcar #'cdr free-vars-in-form))))))

(defun create-skolem-symbol (var-spec form free-vars-in-form polarity)
  ;; this code for generating skolem function names and world path function names
  ;; stores the generated name in an alist so that if the exact same wff is input
  ;; again, the same names will be generated
  ;; thus,
  ;; (assert '(forall (x) (exists (y) (p x y))))
  ;;  followed by
  ;; (assert '(forall (x) (exists (y) (p x y))))
  ;; will result in two occurrences of the same wff with the same skolem function
  ;;
  ;; this could be improved by checking for variants rather than equality so that
  ;; (assert '(forall (u) (exists (v) (p u v))))
  ;; would also produce the same wff with the same skolem function
  (let ((key (list var-spec form free-vars-in-form polarity)))
    (or (cdr (assoc key *skolem-function-alist* :test #'equal))
	(let* (conc-name
	       sort
	       (x (cond
                   ((setf conc-name (getf (rest var-spec) :conc-name))
                    (newsym2 conc-name))
                   ((and (not (getf (rest var-spec) :sort-unknown))
                         (not (top-sort-name? (setf sort (getf (rest var-spec) :sort)))))
                    (newsym :name :skolem :sort sort))
                   (t
                    (newsym :name :skolem)))))
;;	  (push (cons key x) *skolem-function-alist*)	;skolem symbol reuse disabled pending fix
	  x))))

;;; *new-symbol-prefix* is included in created (including skolem) constant and function symbol names
;;; to give them hopefully unambiguous internable names across SNARK runs
;;; to allow import and export of created symbols without conflict

(defvar *new-symbol-prefix*)			;set to "unique" value by (initialize)
(defvar *number-of-new-symbols*)		;set to 0 by (initialize)
(defvar *new-symbol-table*)			;set to hash table by (initialize)

(defun newsym-prefix ()
  (let ((alphabet (symbol-name :abcdefghijklmnopqrstuvwxyz))
        (n (get-internal-run-time))
        (l nil))
    (dotimes (i 4)
      (push (char alphabet (rem n 26)) l)
      (setf n (floor n 26)))
    (coerce l 'string)))

(defun newsym (&key (name :newsym) sort)
  (intern (if sort
              (to-string name *new-symbol-prefix* (incf *number-of-new-symbols*) (variable-sort-marker?) sort)
              (to-string name *new-symbol-prefix* (incf *number-of-new-symbols*)))
          :snark-user))

(defun newsym2 (conc-name)
  (let ((n (gethash conc-name *new-symbol-table* 0)))
    (cond
     ((= 0 n)
      (setf (gethash conc-name *new-symbol-table*) 1)
      conc-name)
     (t
      (setf (gethash conc-name *new-symbol-table*) (+ 1 n))
      (intern (to-string conc-name n) :snark-user)))))  

(defun input-form* (head terms polarity)
  (make-compound* head (input-terms terms polarity)))

(defun input-form (head terms polarity)
  (dolist (fun (function-input-code head) (input-form* head terms polarity))
    (let ((v (funcall fun head terms polarity)))
      (unless (eq none v)
        (return v)))))

(defun input-atom (atom polarity)
  (cond
    ((can-be-proposition-name atom)
     (cond
       ((cdr (assoc atom *input-wff-substitution*))
	(unimplemented))			;proposition variables
       (t
        (input-proposition-symbol atom))))
    ((and (consp atom) (can-be-function-name (first atom)))
     (check-for-well-sorted-atom
      (input-form (input-head-relation-symbol atom) (rest atom) polarity)))
    ((and *input-proposition-variables* (can-be-free-variable-name atom))
     (declare-variable atom))
    (t
     (error "Cannot understand ~S as an atomic formula." atom))))

(defun input-term (term &key (polarity :pos) (*input-wff-substitution* nil))
  (let ((*input-wff-new-antecedents* true)
        (*input-wff-modal-prefix* nil))
    (check-well-sorted (input-term1 term polarity))))

(defun input-term1 (term polarity)
  (cond
   ((variable-p term)
    term)
   ((cdr (assoc term *input-wff-substitution*))
    )
   ((atom term)
    (cond
     ((can-be-free-variable-name term)
      (declare-variable term))
     (t
      (input-constant-symbol term))))
   (t
    (can-be-function-name (first term) 'error)
    (input-form (input-head-function-symbol term) (rest term) polarity))))

(defun input-terms (terms polarity)
  (lcons (input-term1 (first terms) polarity)
	 (input-terms (rest terms) polarity)
	 terms))

(defun map-polarity (fun polarity)
  (if fun (funcall fun polarity) polarity))

(defun opposite-polarity (polarity)
  (ecase polarity
    (:pos
      :neg)
    (:neg
      :pos)
    (:both
      :both)))

(defun input-atom-with-keyword-arguments (head args polarity keywords)
  ;; (declare-relation 'person :any
  ;;   :sort '((1 string) (2 real) (3 string))
  ;;   :input-code (atom-with-keywords-inputter '(:name :age :sex)))
  ;; allows arguments of 3-ary person relation to be specified positionally, by keyword, or a combination
  ;; (person "john" 21 "male"),
  ;; (person "john" :age 21 :sex "male"),
  ;; (person "john" :sex "male" :age 21),
  ;; and (person :sex "male" :age 21 :name "john")
  ;; all yield (person "john" 21 "male")
  ;; argument list is scanned left-to-right, processed positionally until first keyword, then as keyword/value pairs
  ;; (keywords must be syntactically distinguishable from values for this to work properly)
  ;; missing arguments are replaced by existentially quantified variables
  (let ((arity (length keywords)))
    (cond
     ((and (length= arity args) (null (intersection keywords args)))
      none)
     (t
      (let ((args* (make-array (length keywords) :initial-element none)))
        (let ((l args)
              (processing-keyword-arguments nil)
              (i 0)
              pos)
          (loop
            (when (endp l)
              (return))
            (cond
             ((setf pos (position (first l) keywords))
              (cl:assert (eq none (svref args* pos)) () "~S argument given twice in ~S." (first l) (cons (function-name head) args))
              (cl:assert (not (endp (setf l (rest l)))) () "Too few arguments in ~S." (cons (function-name head) args))
              (setf processing-keyword-arguments t))
             (t
              (cl:assert (not processing-keyword-arguments) () "Expected ~S to be a keyword in ~S." (first l) (cons (function-name head) args)) 
              (cl:assert (< i arity) () "Too many arguments in ~S." (cons (function-name head) args))
              (setf pos i)
              (setf i (+ 1 i))))
            (setf (svref args* pos) (pop l))))
        (let ((vars nil))
          (dotimes (i arity)
            (when (eq none (svref args* i))
              (let ((var (gensym))
                    (sort (asa-arg-sort (function-argument-sort-alist head) (+ 1 i))))
                (setf (svref args* i) var)
                (push (if (top-sort? sort) var (list var :sort sort)) vars))))
          (let ((atom (cons (function-name head) (coerce args* 'list))))
            (input-wff1 (if vars (list 'exists (nreverse vars) atom) atom) polarity))))))))

(defun atom-with-keywords-inputter (keywords)
  #'(lambda (head args polarity) (input-atom-with-keyword-arguments head args polarity keywords)))

(defun clausify (wff &optional map-fun)
  ;; apply map-fun to each clause in the clause form of wff
  ;; if map-fun is NIL, return CNF of wff
  (let ((clauses nil) clauses-last)
    (labels
      ((clausify* (cc wff pos lits)
         (cond
          ((and pos (test-option6?) (clause-p wff t))
           (funcall cc (cons wff lits)))
          (t
	   (ecase (head-is-logical-symbol wff)
	     ((nil)
              (cond
               ((eq true wff)
                (unless pos
                  (funcall cc lits)))
               ((eq false wff)
                (when pos
                  (funcall cc lits)))
               (t
                (let ((-wff (make-compound *not* wff)))
                  (dolist (lit lits (funcall cc (cons (if pos wff -wff) lits)))
                    (cond
                     ((equal-p lit wff)
                      (when pos
                        (funcall cc lits))
                      (return))
                     ((equal-p lit -wff)
                      (unless pos
                        (funcall cc lits))
                      (return))))))))
	     (not
              (clausify* cc (first (args wff)) (not pos) lits))
	     (and
              (let ((args (args wff)))
	        (if pos
		    (if (and lits (some (lambda (arg) (member-p arg lits)) args))
		        (funcall cc lits)
		        (dolist (arg args)
		          (clausify* cc arg t lits)))
                    (let ((y (make-a1-compound* *and* true (rest args))))
		      (clausify* (lambda (l) (clausify* cc y nil l)) (first args) nil lits)))))
	     (or
              (let ((args (args wff)))
	        (if pos
                    (let ((y (make-a1-compound* *or* false (rest args))))
		      (clausify* (lambda (l) (clausify* cc y t l)) (first args) t lits))
		    (if (and lits (some (lambda (arg) (member-p (negate arg) lits)) args))
		        (funcall cc lits)
		        (dolist (arg args)
		          (clausify* cc arg nil lits))))))
	     (implies
              (let* ((args (args wff)) (x (first args)) (y (second args)))
	        (if pos
		    (clausify* (lambda (l) (clausify* cc y t l)) x nil lits)
		    (progn
		      (clausify* cc x t   lits)
		      (clausify* cc y nil lits)))))
	     (implied-by
              (let* ((args (args wff)) (x (first args)) (y (second args)))
	        (if pos
		    (clausify* (lambda (l) (clausify* cc y nil l)) x t lits)
		    (progn
		      (clausify* cc y t   lits)
		      (clausify* cc x nil lits)))))
	     (iff
              (let* ((args (args wff)) (x (first args)) (y (make-a1-compound* *iff* true (rest args))))
	        (if pos
		    (progn
		      (clausify* (lambda (l) (clausify* cc y t   l)) x nil lits)
		      (clausify* (lambda (l) (clausify* cc y nil l)) x t   lits))
		    (progn
		      (clausify* (lambda (l) (clausify* cc y nil l)) x nil lits)
		      (clausify* (lambda (l) (clausify* cc y t   l)) x t   lits)))))
	     (xor
              (let* ((args (args wff)) (x (first args)) (y (make-a1-compound* *xor* false (rest args))))
	        (if pos
		    (progn
		      (clausify* (lambda (l) (clausify* cc y nil l)) x nil lits)
		      (clausify* (lambda (l) (clausify* cc y t   l)) x t   lits))
		    (progn
		      (clausify* (lambda (l) (clausify* cc y t   l)) x nil lits)
		      (clausify* (lambda (l) (clausify* cc y nil l)) x t   lits)))))
             (if 
               (let* ((args (args wff)) (x (first args)) (y (second args)) (z (third args)))
                 (clausify* (lambda (l) (clausify* cc y pos l)) x nil lits)
                 (clausify* (lambda (l) (clausify* cc z pos l)) x t   lits))))))))
      (clausify* (lambda (lits)
                   (let ((clause (make-a1-compound* *or* false (reverse lits))))
                     (if map-fun (funcall map-fun clause) (collect clause clauses))))
                 wff t nil)
      (if map-fun nil (make-a1-compound* *and* true clauses)))))

(defun report-not-2-arguments-quantification (head args)
  (case (use-extended-quantifiers?)
    ((nil)
     (with-standard-io-syntax2
       (cerror "Convert it to a 2-ary quantification."
               "~S does not have exactly 2 arguments as ~A ~S wants."
               (cons (function-name head) args) (function-kind head) (function-name head))))
    (warn
     (with-standard-io-syntax2
       (warn "~S does not have exactly 2 arguments as ~A ~S wants.  It will be converted."
             (cons (function-name head) args) (function-kind head) (function-name head))))))

(defun report-not-2-arguments-implication (head args)
  (case (use-extended-implications?)
    ((nil)
     (with-standard-io-syntax2
       (cerror "Convert it to a 2-ary implication."
               "~S does not have exactly 2 arguments as ~A ~S wants."
               (cons (function-name head) args) (function-kind head) (function-name head))))
    (warn
     (with-standard-io-syntax2
       (warn "~S does not have exactly 2 arguments as ~A ~S wants.  It will be converted."
             (cons (function-name head) args) (function-kind head) (function-name head))))))

;;; the following functions can be used as in
;;; (declare-relation 'product :any :input-code (lambda (h a p) (require-n-arguments h a p 3)))
;;; so that that there is only one product relation symbol
;;; (not more than one of different arities as is usually allowed)
;;; and it always has three arguments
;;; (not arbitrarily many as is usual for :any arity relations)

(defun require-n-arguments (head args polarity n)
  ;; if no error, returns none to cause later input-function-code to be used
  (declare (ignore polarity))
  (unless (length= n args)
    (with-standard-io-syntax2
      (cerror1 "~S does not have exactly ~D argument~:P as ~A ~S requires."
               (cons (function-name head) args) n (function-kind head) (function-name head))))
  none)

(defun require-n-or-more-arguments (head args polarity n)
  ;; if no error, returns none to cause later input-function-code to be used
  (declare (ignore polarity))
  (unless (length<= n args)
    (with-standard-io-syntax2
      (cerror1 "~S does not have at least ~D argument~:P as ~A ~S requires."
               (cons (function-name head) args) n (function-kind head) (function-name head))))
  none)

;;; input.lisp EOF
