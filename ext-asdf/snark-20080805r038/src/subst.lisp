;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: subst.lisp
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

;;; a substitution is a list of bindings and an alist of variables and values
;;; substitutions can be manipulated as SNARK terms if this ever becomes useful

(defmacro make-binding (var value)
  `(cons ,var ,value))

(defmacro binding-var (binding)
  `(car ,binding))

(defmacro binding-value (binding)
  `(cdr ,binding))

(defmacro add-binding-to-substitution (binding subst)
  `(cons ,binding ,subst))

(defmacro dobindings ((binding subst &optional resultform) &body body)
  `(dolist (,binding ,subst ,resultform)
     ,@body))

(definline bind-variable-to-term (var term subst)
  (add-binding-to-substitution (make-binding var term) subst))

(defun lookup-variable-in-substitution (var subst)
  (let ((v (assoc var subst :test #'eq)))
    (if v (binding-value v) none)))

(defun lookup-value-in-substitution (value subst)
  (let ((v (rassoc value subst)))
    (if v (binding-var v) none)))

(defun lookup-value-in-substitution2 (value subst subst2)
  (let ((v (rassoc value subst :test (lambda (x y) (equal-p x y subst2)))))
    (if v (binding-var v) none)))

(defun substitution-equal-p (subst1 subst2)
  (and (length= subst1 subst2)
       (substitution-subset-p1 subst1 subst2)))

(defun substitution-subset-p (subst1 subst2)
  (and (length<= subst1 subst2)
       (substitution-subset-p1 subst1 subst2)))

(defun substitution-diff (subst1 subst2)
  (if subst2 (ldiff subst1 subst2) subst1))

(defun substitution-diff2 (subst1 subst2)
  (labels
    ((subst-diff (subst1)
       (if (null subst1)
           nil
           (let* ((b1 (first subst1))
                  (var (binding-var b1))
                  (val1 (binding-value b1))
                  (val2 (lookup-variable-in-substitution var subst2)))
             (cond
              ((eq none val2)				;var is unbound in subst2
               (let* ((l (rest subst1))
                      (l* (subst-diff l)))
                 (cond
                  ((eq none l*)
                   none)
                  ((eq l l*)
                   subst1)
                  (t
                   (cons b1 l*)))))
              ((equal-p val1 val2)			;var is bound equally in subst1 and subst2
               (subst-diff (rest subst1)))
              (t					;var is bound unequally in subst1 and subst2
               none))))))				;return none to signal incompatibility
    (if (null subst2)
        subst1
        (subst-diff subst1))))

(defun substitution-subset-p1 (subst1 subst2)
  (loop
    (if (null subst1)
        (return t)
        (let ((v (lookup-variable-in-substitution (binding-var (first subst1)) subst2)))
          (if (and (neq none v) (equal-p (binding-value (first subst1)) v))
              (setf subst1 (rest subst1))
              (return nil))))))

(defun remove-irrelevant-bindings (subst term)
  (cond
   ((null subst)
    nil)
   ((not (variable-occurs-p (binding-var (first subst)) term nil))
    (remove-irrelevant-bindings (rest subst) term))
   (t
    (let* ((l (rest subst))
           (l* (remove-irrelevant-bindings l term)))
      (if (eq l l*)
          subst
          (add-binding-to-substitution (first subst) l*))))))

(defun print-substitution (subst)
  (format t "{ ")
  (let ((first t))
    (dobindings (binding subst)
      (if first
          (setf first nil)
          (princ " , "))
      (format t "~S -> ~S" (binding-var binding) (binding-value binding))))
  (format t " }")
  subst)

(defun make-idempotent-substitution (subst)
  ;; create an idempotent substitution from subst
  ;; by instantiating the variable values
  (cond
    ((null subst)
     nil)
    ((null (rest subst))
     subst)
    (t
     (setf subst (copy-alist subst))
     (dolist (binding subst)
       (setf (binding-value binding) (instantiate (binding-value binding) subst)))
     subst)))

(defun variables (x &optional subst vars)
  "return a list of all the variables that occur in x"
  (dereference
   x subst
   :if-constant vars
   :if-compound-cons (variables (cdrc x) subst (variables (carc x) subst vars))
   :if-compound-appl (dolist (x1 (argsa x) vars)
                       (setf vars (variables x1 subst vars)))
   :if-variable (adjoin x vars)))

(defun nontheory-variables (x &optional subst theory vars)
  (dereference
   x subst
   :if-constant vars
   :if-compound-cons (nontheory-variables (cdrc x) subst theory (nontheory-variables (carc x) subst theory vars))
   :if-compound-appl (let ((head (heada x)))
                       (unless (function-constructor head)	;constructor symbols are transparent wrt theory
                         (setf theory (function-constraint-theory head)))
                       (dolist (x1 (argsa x) vars)
                         (setf vars (nontheory-variables x1 subst theory vars))))
   :if-variable (if (null theory) (adjoin x vars) vars)))	;only variables under nontheory symbols are returned

(defun variablesl (l &optional subst vars)
  (dolist (x l vars)
    (setf vars (variables x subst vars))))

(defun first-nonvariable-term (terms &optional subst)
  (dolist (term terms none)
    (dereference
     term subst
     :if-constant (return term)
     :if-compound (return term))))

(defun first-nonvariable-subterm (terms &optional subst)
  (dolist (term terms none)
    (dereference
     term subst
     :if-compound (let ((v (first-nonvariable-term (args term))))
                    (unless (eq none v)
                      (return v))))))

(defun variable-counts (x &optional subst counts)
  "return a list of all the variables that occur in x with their frequency, in dotted pairs"
  (dereference
   x subst
   :if-constant counts
   :if-compound-cons (variable-counts (cdrc x) subst (variable-counts (carc x) subst counts))
   :if-compound-appl (dolist (x1 (argsa x) counts)
                       (setf counts (variable-counts x1 subst counts)))
   :if-variable (let ((v (assoc/eq x counts)))
                  (if v (progn (incf (cdrc v)) counts) (cons (cons x 1) counts)))))

(defun new-variables (x &optional subst vars)
  "return a list of all the variables that occur in x but are not in vars"
  ;; ldiff could be done destructively
  (ldiff (variables x subst vars) vars))

(defun instantiate (x n &optional subst)
  "applies substitution to x, optionally first renumbering block-0 variables to block-n"
  (cond
   ((constant-p x)
    x)
   (t
    (when (or (consp n) (numberp subst))	;accept n and subst arguments in either order
      (psetq subst n n subst))
    (if (or (null n) (zerop n))
        (if (null subst)
            x					;nop
            (labels				;just substitute
              ((instantiate* (x)
                 (dereference
                  x subst
                  :if-variable x
                  :if-constant x
                  :if-compound-cons (lcons (instantiate* (car x)) (instantiate* (cdr x)) x)
                  :if-compound-appl (let* ((args (argsa x)) (args* (instantiatel args)))
                                      (if (eq args args*) x (make-compound* (heada x) args*)))))
               (instantiatel (l)
                 (lcons (instantiate* (first l)) (instantiatel (rest l)) l)))
              (instantiate* x)))
        (let ((incr (variable-block n)))
          (if (null subst)
              (labels				;just renumber
                ((instantiate* (x)
                   (dereference
                    x nil
                    :if-variable (let ((n (variable-number x)))
                                   (if (variable-block-0-p n)
                                       (make-variable (variable-sort x) (+ n incr))
                                       x))
                    :if-constant x
                    :if-compound-cons (lcons (instantiate* (car x)) (instantiate* (cdr x)) x)
                    :if-compound-appl (let* ((args (argsa x)) (args* (instantiatel args)))
                                        (if (eq args args*) x (make-compound* (heada x) args*)))))
                 (instantiatel (l)
                   (lcons (instantiate* (first l)) (instantiatel (rest l)) l)))
	        (instantiate* x))
              (labels				;renumber and substitute
                ((instantiate* (x)
                   (when (variable-p x)
                     (let ((n (variable-number x)))
                       (when (variable-block-0-p n)
	                 (setf x (make-variable (variable-sort x) (+ n incr))))))
                   (dereference
                    x subst
                    :if-variable x
                    :if-constant x
                    :if-compound-cons (lcons (instantiate* (car x)) (instantiate* (cdr x)) x)
                    :if-compound-appl (let* ((args (argsa x)) (args* (instantiatel args)))
                                        (if (eq args args*) x (make-compound* (heada x) args*)))))
                 (instantiatel (l)
                   (lcons (instantiate* (first l)) (instantiatel (rest l)) l)))
	        (instantiate* x))))))))

(defun renumber (x &optional subst rsubst)
  "applies substitution to x and renumbers variables (normally to block 0)"
  (dereference
   x subst
   :if-constant (values x rsubst)
   :if-compound-cons (values (let (u v)
                               (multiple-value-setq (u rsubst)
                                 (renumber (carc x) subst rsubst))
                               (multiple-value-setq (v rsubst)
                                 (renumber (cdrc x) subst rsubst))
                               (lcons u v x))
                             rsubst)
   :if-compound-appl (values (let* ((args (argsa x))
                                    (args* (let (dummy)
                                             (declare (ignorable dummy))
                                             (multiple-value-setq (dummy rsubst)
                                               (renumberl args subst rsubst)))))
                               (if (eq args args*)
                                   x
                                   (make-compound* (head x) args*)))
                             rsubst) 
   :if-variable (let ((v (lookup-variable-in-substitution x rsubst)))
                  (cond
                   ((neq none v)
                    (values v rsubst))
                   (t
                    (let ((var (renumberv x rsubst)))
;;                    (values var (bind-variable-to-term x var rsubst))	;maybe x=var
                      (values var (cons (cons x var) rsubst))))))))

(defun renumberl (l subst rsubst)
  (let (dummy)
    (declare (ignorable dummy))
    (values (lcons (multiple-value-setq (dummy rsubst)
		     (renumber (first l) subst rsubst))
		   (multiple-value-setq (dummy rsubst)
		     (renumberl (rest l) subst rsubst))
		   l)
	    rsubst)))

(defvar *renumber-first-number* 0)
(defvar *renumber-by-sort* nil)
(defvar *renumber-ignore-sort* nil)

(defun renumberv (var rsubst)
  (let ((sort (if *renumber-ignore-sort* (top-sort) (variable-sort var))))
    (if (null *renumber-first-number*)
        (make-variable sort)
        (loop
          (cond
           ((null rsubst)
            (return (make-variable sort *renumber-first-number*)))
           (t
            (let ((binding (first rsubst)))
              (when (implies *renumber-by-sort* (same-sort? sort (variable-sort (binding-value binding))))
                (return (make-variable sort (+ (variable-number (binding-value binding)) 1)))))
            (setf rsubst (rest rsubst))))))))

(defun renumber-new (x &optional subst rsubst)
  "applies substitution to x and renumbers variables to all new variables"
  (let ((*renumber-first-number* nil))
    (renumber x subst rsubst)))

(defun linearize (term &optional subst vars)
  (dereference
   term subst
   :if-constant (values term vars)
   :if-variable (let ((var (if (member term vars :test #'eq)
                               (make-variable (variable-sort term))
                               term)))
                  (values var (cons var vars)))
   :if-compound-cons (values
                      (lcons (mvlet (((:values v vars*) (linearize (carc term) subst vars)))
                               (setf vars vars*)
                               v)
                             (mvlet (((:values v vars*) (linearize (cdrc term) subst vars)))
                               (setf vars vars*)
                               v)
                             term)
                      vars)
   :if-compound-appl (values
                      (mvlet* ((args (argsa term))
                               ((:values args* vars*) (linearizel args subst vars)))
                        (setf vars vars*)
                        (if (eq args args*)
                            term
                            (make-compound* (heada term) args*)))
                      vars)))

(defun linearizel (terms subst vars)
  (values
   (lcons (mvlet (((:values v vars*) (linearize (first terms) subst vars)))
            (setf vars vars*)
            v)
          (mvlet (((:values v vars*) (linearizel (rest terms) subst vars)))
            (setf vars vars*)
            v)
          terms)
   vars))

(defun ground-p (x &optional subst)
  "return t if x is ground, nil otherwise"
  (dereference
   x subst
   :if-constant t
   :if-compound-cons (and (ground-p (carc x) subst) (ground-p (cdrc x) subst))
   :if-compound-appl (loop for x1 in (argsa x)
                           always (ground-p x1 subst))
   :if-variable nil))

(defun frozen-p (x subst)
  "return t if all variables of x are frozen, nil otherwise"
  (dereference
   x subst
   :if-constant t
   :if-compound-cons (and (frozen-p (carc x) subst) (frozen-p (cdrc x) subst))
   :if-compound-appl (loop for x1 in (argsa x)
                           always (frozen-p x1 subst))
   :if-variable (variable-frozen-p x)))

(defun constructor-term-p (x subst)
  ;; returns t if x is built entirely from constructors
  ;; treat nil as second argument of cons as a constructor even if not declared as such
  (dereference
   x subst
   :if-constant (constant-constructor x)
   :if-compound-cons (and (constructor-term-p (carc x) subst) (constructor-term-p (cdrc x) subst))
   :if-compound-appl (and (function-constructor (heada x))
                          (loop for x1 in (argsa x)
                                always (constructor-term-p x1 subst)))
   :if-variable nil))

(defun unsorted-p (x &optional subst)
  ;; check whether all symbols in x are unsorted
  ;; except $$cons and nil
  ;; and numbers and strings?
  (dereference
   x subst
   :if-variable (top-sort? (variable-sort x))
   :if-constant (or nil (top-sort? (constant-sort x)))
   :if-compound-cons (and (unsorted-p (carc x) subst) (unsorted-p (cdrc x) subst))
   :if-compound-appl (and (null (function-sort-declarations (heada x)))
                          (loop for x1 in (argsa x)
                                always (unsorted-p x1 subst)))))

(defun all-variables-p (terms &optional subst)
  (dolist (term terms t)
    (dereference
     term subst
     :if-constant (return nil)
     :if-compound (return nil))))

(defun occurs-p (x y &optional subst)
  "return t if x occurs in y, nil otherwise"
  (dereference
    x subst
    :if-constant (if (function-symbol-p x)
		     (function-occurs-p x y subst)
		     (constant-occurs-p x y subst))
    :if-compound (compound-occurs-p x y subst)
    :if-variable (variable-occurs-p x y subst)))

(defun function-occurs-p (x y subst)
  (dereference
    y subst
    :if-compound (or (eq x (head y))
		     (loop for y1 in (args y)
			   thereis (function-occurs-p x y1 subst)))))

(defun constant-occurs-p (x y subst)
  "return t if atom x occurs in y, nil otherwise"
  (dereference
    y subst
    :if-constant (eql x y)
    :if-compound (loop for y1 in (args y)
		       thereis (constant-occurs-p x y1 subst))))

(defun compound-occurs-p (x y subst)
  "return t if compound x occurs in y, nil otherwise"
  (dereference
    y subst
    :if-compound (or (equal-p x y subst)
		     (loop for y1 in (args y)
			   thereis (compound-occurs-p x y1 subst)))))

(defun no-new-variable-occurs-p (x subst vars)
  ;; returns t if every variable in x.subst is a member of vars, nil otherwise
  (labels ((no-new-variable (x)
             (dereference
              x subst
              :if-variable (member x vars :test #'eq)
              :if-constant t
              :if-compound-cons (and (no-new-variable (carc x)) (no-new-variable (cdrc x)))
              :if-compound-appl (dolist (x1 (argsa x) t)
                                  (unless (no-new-variable x1)
                                    (return nil))))))
    (not (null (no-new-variable x)))))

(defun constant-occurs-below-constructor-p (x y subst)
  (labels
    ((occ (y)
       (dereference
        y subst
        :if-constant (eql x y)
        :if-compound-cons (or (occ (carc y)) (occ (cdrc y)))
        :if-compound-appl (and (function-constructor (heada y))
                               (loop for y1 in (argsa y) thereis (occ y1))))))
    (dereference
     y subst
     :if-compound-cons (or (occ (carc y)) (occ (cdrc y)))
     :if-compound-appl (and (function-constructor (heada y))
                            (loop for y1 in (argsa y) thereis (occ y1))))))

(defun variable-occurs-below-constructor-p (x y subst)
  (labels
    ((occ (y)
       (dereference
        y subst
        :if-variable (eq x y)
        :if-compound-cons (or (occ (carc y)) (occ (cdrc y)))
        :if-compound-appl (and (function-constructor (heada y))
                               (loop for y1 in (args y) thereis (occ y1))))))
    (dereference
     y subst
     :if-compound-cons (or (occ (carc y)) (occ (cdrc y)))
     :if-compound-appl (and (function-constructor (heada y))
                            (loop for y1 in (argsa y) thereis (occ y1))))))

(defun compound-occurs-below-constructor-p (x y subst)
  (labels
    ((occ (y)
       (dereference
        y subst
        :if-compound-cons (or (if (consp x) (equal-p x y subst) nil)
                              (or (occ (carc y)) (occ (cdrc y))))
        :if-compound-appl (or (if (consp x) nil (equal-p x y subst))
                              (and (function-constructor (heada y))
                                   (loop for y1 in (argsa y) thereis (occ y1)))))))
    (dereference
     y subst
     :if-compound-cons (or (occ (carc y)) (occ (cdrc y)))
     :if-compound-appl (and (function-constructor (heada y))
                            (loop for y1 in (argsa y) thereis (occ y1))))))

(defmacro variable-occurs-p1-macro ()
  `(dereference
    y nil
    :if-compound-cons (or (variable-occurs-p1 x (carc y)) (variable-occurs-p1 x (cdrc y)))
    :if-compound-appl (dolist (y (argsa y) nil)
                        (when (variable-occurs-p1 x y)
                          (return t)))
    :if-variable (eq x y)))

(defmacro variable-occurs-p2-macro ()
  `(dereference
    y subst
    :if-compound-cons (or (variable-occurs-p2 x (carc y) subst) (variable-occurs-p2 x (cdrc y) subst))
    :if-compound-appl (dolist (y (argsa y) nil)
                        (when (variable-occurs-p2 x y subst)
                          (return t)))
    :if-variable (eq x y)))

(defun variable-occurs-p1l (x l)
  (dolist (y l nil)
    (when (variable-occurs-p1-macro)
      (return t))))

(defun variable-occurs-p2l (x l subst)
  (dolist (y l nil)
    (when (variable-occurs-p2-macro)
      (return t))))

(defun variable-occurs-p1 (x y)
  (variable-occurs-p1-macro))

(defun variable-occurs-p2 (x y subst)
  (variable-occurs-p2-macro))

(defun variable-occurs-p (x y subst)
  "return t if variable x occurs in y, nil otherwise"
  (if (null subst)
      (variable-occurs-p1-macro)
      (variable-occurs-p2-macro)))

(defun special-unify-p (x subst)
  (dereference
    x subst
    :if-compound (or (function-unify-code (head x))
                     (loop for x1 in (args x)
                           thereis (special-unify-p x1 subst)))))

(defun skolem-occurs-p (x subst)
  (dereference
    x subst
    :if-constant (constant-skolem-p x)
    :if-compound (or (function-skolem-p (head x))
		     (loop for x1 in (args x)
			   thereis (skolem-occurs-p x1 subst)))))

(defun disallowed-symbol-occurs-in-answer-p (x subst)
  (dereference
    x subst
    :if-constant (not (constant-allowed-in-answer x))
    :if-compound (or (not (function-allowed-in-answer (head x)))
		     (loop for x1 in (args x)
			   thereis (disallowed-symbol-occurs-in-answer-p x1 subst)))))

(defun embedding-variable-occurs-p (x subst)
  (dereference
    x subst
    :if-compound (loop for x1 in (args x)
		       thereis (embedding-variable-occurs-p x1 subst))
    :if-variable (embedding-variable-p x)))

;;; subst.lisp EOF
