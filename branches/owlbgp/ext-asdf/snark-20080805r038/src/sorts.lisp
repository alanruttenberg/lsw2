;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: sorts.lisp
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

(definline subsort-p (x y)
  ;; old function name
  (subsort? x y))

(defun sort-name? (x &optional action)
  (or (top-sort-name? x)
      (let ((v (find-symbol-table-entry x :sort)))
        (and (neq none v) v))
      (and action (and (can-be-sort-name x action) (funcall action "~S has not been declared as a sort." x)))))

(definline input-sort (name &optional (not-found-action 'error))
  ;; old function name
  (declare (ignore not-found-action))
  (the-sort name))

(defun declare-the-sort-function-symbol (name sort)
  (declare-function
   (intern (format nil "~A-~A" :the name) :snark-user) 1
   :sort name
   :rewrite-code (lambda (term subst)
                   (let ((x (arg1 term)))
                     (if (subsort? (term-sort x subst) sort) x none)))))

(defun declare-constant-sort (constant sort)
  "assigns a sort to a constant"
  (let* ((sort (the-sort sort))
         (old-sort (constant-sort constant))
         (new-sort (sort-intersection old-sort sort)))
    (cond
     ((same-sort? old-sort new-sort)
      )
     ((null new-sort)
      (error "Cannot declare ~A as constant of sort ~A; ~A is of incompatible sort ~A." constant sort constant old-sort))
     (t
      (setf (constant-sort constant) new-sort))))
  constant)

(defun maybe-declare-constant-sort (constant sort)
  (if (sort-name-expression? sort)
      (declare-constant-sort constant sort)
      constant))

(defun declare-function-sort (function sort-spec)
  (let ((fsd
         (cond
          ((function-boolean-valued-p function)
           (make-function-sort-declaration
            :argument-sort-alist (input-argument-sort-alist function sort-spec)))
          ((sort-name-expression? sort-spec nil)
           (make-function-sort-declaration
            :result-sort (the-sort sort-spec)))
          (t
           (make-function-sort-declaration
            :result-sort (the-sort (first sort-spec))
            :argument-sort-alist (input-argument-sort-alist function (rest sort-spec)))))))
    (case (function-arity function)
      (otherwise
       (declare-ordinary-function-sort function fsd)))))

(defun declare-ordinary-function-sort (function fsd)
  "assigns result and argument sorts to a function"
;; (unless argument-sorts
;;   (error "No argument sorts provided.~%Can't declare sorts for 0-ary function; use a constant instead."))
  (let ((fsds (function-sort-declarations function)))
    (cond
     ((null fsds)
      (setf (function-sort-declarations function) (list fsd)))
     ((function-boolean-valued-p function)
      ;; keep all sort-declarations that are not instances of another
      (unless (member fsd fsds :test #'sub-fsd-p)
        (setf (function-sort-declarations function) (cons fsd (remove fsd fsds :test #'super-fsd-p)))))
     (t
      (unless (member fsd fsds :test #'same-fsd-p)
        (cerror2 "Multiple sort declaration for function ~S is unimplemented."
                 (function-name function))
;;      (setf (function-sort-declarations function) (process-function-sorts (cons fsd fsds)))
        ))))
  (when (function-associative function)
    (check-associative-function-sort function)))

;;; ASSUME THAT SORT COERCION IS UNITARY
;;; not for functions with special unification algorithms

(defun coerce-list (l subst fsd)
  (do ((l l (rest l))
       (i 1 (+ i 1)))
      ((null l)
       subst)
    (when (eq none (setf subst (coerce-term (first l) subst (fsd-arg-sort fsd i))))
      (return none))))

(defun coerce-compound (x subst new-sort)
  ;; the current sort scheme ensures that only the first sort declaration
  ;; whose result sort is a subsort of new-sort must be tried
  (cl:assert (and new-sort (not (top-sort? new-sort))))
  (let* ((head (head x))
         (fsds (function-sort-declarations head)))
    (cond
     ((null fsds)
      none)
     ((null (rest fsds))
      (if (fsd-subsort-p (first fsds) new-sort) subst none))
     (t
      (let ((args (args x)))
        (case (function-arity head)
          (otherwise
           (dolist (fsd fsds)
             (when (fsd-subsort-p fsd new-sort)
               (return (coerce-list args subst fsd)))))))))))

(defun coerce-term (x subst new-sort)
  (cl:assert (and new-sort (not (top-sort? new-sort))))
  (dereference
   x subst
   :if-variable (let ((s (variable-sort x)))
                  (cond
                   ((subsort? s new-sort)
                    subst)
                   ((variable-frozen-p x)
                    none)
                   ((subsort? new-sort s)
                    (bind-variable-to-term x (make-variable new-sort) subst))
                   (t
                    (let ((s* (sort-intersection s new-sort)))
                      (if (null s*) none (bind-variable-to-term x (make-variable s*) subst))))))
   :if-compound (coerce-compound x subst new-sort)
   :if-constant (if (constant-sort-p x new-sort) subst none)))

(defvar *%check-for-well-sorted-atom%* t)

(defun check-for-well-sorted-atom (atom &optional subst)
  (when *%check-for-well-sorted-atom%*
    (assert-atom-is-well-sorted atom subst))
  atom)

(defun assert-atom-is-well-sorted (atom &optional subst)
  (or (well-sorted-p atom subst)
      (let ((*subst-for-printing* subst))
        (error "Atomic formula ~A is not well sorted." atom))))

(defun check-well-sorted (x &optional subst)
  (unless (well-sorted-p x subst)
    (let ((*subst-for-printing* subst))
      (error "~A is not well sorted." x)))
  x)

(defvar *%checking-well-sorted-p%* nil)

(defun well-sorted-p (x &optional subst (sort (top-sort)))
  ;; determines if expression is well sorted
  ;; it does this by doing well-sorting on the expression
  ;; with the restriction that no instantiation be done
  (prog->
    (quote t -> *%checking-well-sorted-p%*)
    (well-sort x subst sort ->* subst)
    (declare (ignore subst))
    (return-from prog-> t)))

(defun well-sorted-args-p (args subst fsd &optional (argcount 0))
  (prog->
    (quote t -> *%checking-well-sorted-p%*)
    (well-sort-args args subst fsd argcount ->* subst)
    (declare (ignore subst))
    (return-from prog-> t)))    

(defun term-sort (term &optional subst)
  ;; return sort of well-sorted term
  (dereference
   term subst
   :if-variable (variable-sort term)
   :if-constant (constant-sort term)
   :if-compound (compound-sort term subst)))

(defun compound-sort (term &optional subst)
  (let* ((head (head term))
         (fsds (function-sort-declarations head)))
    (cond
     ((null fsds)
      (top-sort))
     ((null (rest fsds))
      (fsd-result-sort (first fsds)))
     (t
      (case (function-arity head)
        (otherwise
         (let ((args (args term))
               (sort (top-sort)))
           (when (and (function-associative head) (cddr args))
             (setf args (list (first args) (make-compound* head (rest args)))))
           (dolist (fsd fsds)
             (let ((fsd-sort (fsd-result-sort fsd)))
               (when (and (not (subsort? sort fsd-sort))
                          (not (sort-disjoint? sort fsd-sort))
                          (well-sorted-args-p args subst fsd))
                 (setf sort (sort-intersection sort fsd-sort)))))
           sort)))))))

(defun well-sort (cc x &optional subst (sort (top-sort)))
  (dereference
   x subst
   :if-variable (cond
                 ((variable-sort-p x sort)
                  (funcall cc subst))
                 (*%checking-well-sorted-p%*
                  )
                 ((subsort? sort (variable-sort x))
                  (funcall cc (bind-variable-to-term x (make-variable sort) subst)))
                 (t
                  (let ((sort (sort-intersection sort (variable-sort x))))
                    (unless (null sort)
                      (funcall cc (bind-variable-to-term x (make-variable sort) subst))))))
   :if-constant (when (constant-sort-p x sort)
                  (funcall cc subst))
   :if-compound (let* ((head (head x))
                       (args (args x))
                       (fsds (function-sort-declarations head)))
                  (cond
                   ((null fsds)
                    (when (top-sort? sort)
                      (well-sort-args cc args subst nil)))
                   (t
                    (case (function-arity head)
                      (otherwise
                       (when (and (function-associative head) (cddr args))
                         (setf args (list (first args) (make-compound* head (rest args)))))
                       (dolist (fsd fsds)
                         (when (fsd-subsort-p fsd sort)
                           (well-sort-args cc args subst fsd)))))))))
  nil)

(defun well-sort-args (cc args subst fsd &optional (argcount 0))
  (dereference
   args subst
   :if-constant (funcall cc subst)
   :if-variable (funcall cc subst)
   :if-compound-appl (funcall cc subst)
   :if-compound-cons (prog->
                       (well-sort (carc args) subst (fsd-arg-sort fsd (incf argcount)) ->* subst)
                       (well-sort-args (cdrc args) subst fsd argcount ->* subst)
                       (funcall cc subst)))
  nil)

(defun well-sort-alist (cc alist subst fsd)
  (dereference
   alist subst
   :if-constant (funcall cc subst)	;nil at the end of the alist
   :if-variable (funcall cc subst)	;well-sorted except for future binding of this variable
   :if-compound-cons (prog->
                       (carc alist -> p)
                       (well-sort (cdr p) subst (fsd-arg-sort fsd (car p)) ->* subst)
                       (well-sort-alist (cdrc alist) subst fsd ->* subst)
                       (funcall cc subst)))
  nil)

(defun well-sort-atoms (cc atoms subst)
  (cond
   ((null atoms)
    (funcall cc subst))
   (t
    (prog->
      (well-sort (first atoms) subst ->* subst)
      (well-sort-atoms (rest atoms) subst ->* subst)
      (funcall cc subst)))))

(defun well-sort-atoms1 (cc atoms subst)
  (prog->
    (quote t -> first)
    (well-sort-which-atoms atoms subst -> atoms)
    (replace-skolem-terms-by-variables-in-atoms atoms subst -> atoms2 sksubst)
    (well-sort-atoms atoms2 subst ->* subst)
    (unless (fix-skolem-term-sorts sksubst first subst)
      (cerror "Use only first instance."
              "Input wff has more than well-sorted instance of existentially quantifed variable.")
      (return-from prog->))
    (setf first nil)
    (funcall cc subst)))

(defun well-sort-which-atoms (atoms &optional subst)
  (prog->
    (delete-if atoms ->* atom)
    (cond
     ((well-sorted-p atom subst)
      t)
     ((eq :terms (use-well-sorting?))
      (cond
       ((well-sorted-p (args atom) subst)
        (let ((*subst-for-printing* subst))
          (warn "Atomic formula ~A is not well sorted.~%Its arguments are well sorted, so will continue." atom))
        t)
       (t
        (let ((*subst-for-printing* subst))
          (warn "Atomic formula ~A is not well sorted.~%Will try to make its arguments well sorted and continue." atom))
        nil)))
     (t
      (let ((*subst-for-printing* subst))
        (warn "Atomic formula ~A is not well sorted." atom))
      nil))))

(defun well-sort-wff (cc wff &optional subst)
  (cond
   ((use-well-sorting?)
    (well-sort-atoms1 cc (atoms-in-wff wff subst) subst))
   (t
    (funcall cc subst))))

(defun well-sort-wffs (cc wffs &optional subst)
  (cond
   ((use-well-sorting?)
    (well-sort-atoms1 cc (atoms-in-wffs wffs subst) subst))
   (t
    (funcall cc subst))))

(defun replace-skolem-terms-by-variables-in-atoms (atoms &optional subst)
  ;; this garbage is for HPKB and is needed for
  ;; automatic well-sorting of unsorted wffs with existential quantifiers,
  ;; which shouldn't even be allowed
  ;; intended for freshly skolemized formulas; no skolem terms embedded in skolem terms
  (let ((sksubst nil))
    (values
     (prog->
       (mapcar atoms ->* atom)
       (map-terms-in-atom-and-compose-result atom subst ->* term polarity)
       (declare (ignore polarity))
       (dereference
        term subst
        :if-variable term
        :if-constant (if (constant-skolem-p term)
                         (let ((v (lookup-value-in-substitution term sksubst)))
                           (when (eq none v)
                             (setf v (make-variable (constant-sort term)))
                             (setf sksubst (bind-variable-to-term v term sksubst)))
                           v)
                         term)
        :if-compound (let ((fn (head term)))
                       (if (function-skolem-p fn)
                           (let ((v (lookup-value-in-substitution2 term sksubst subst)))
                             (when (eq none v)
                               (setf v (make-variable (fsd-result-sort (first (function-sort-declarations fn)))))
                               (setf sksubst (bind-variable-to-term v term sksubst)))
                             v)
                           term))))
     sksubst)))

(defun fix-skolem-term-sorts (sksubst first subst)
  (dobindings (binding sksubst t)
    (let ((sort (let ((var (binding-var binding)))
                  (dereference var subst)
                  (variable-sort var)))
          (val (binding-value binding)))
      (dereference
       val nil
       :if-constant (unless (same-sort? sort (constant-sort val))
                      (if first
                          (setf (constant-sort val) sort)
                          (return nil)))
       :if-compound (let* ((head (head val))
                           (fsd (first (function-sort-declarations head))))
                      (unless (same-sort? sort (fsd-result-sort fsd))
                        (if first
                            (setf (function-sort-declarations head)
                                  (list (make-function-sort-declaration
                                         :result-sort sort
                                         :argument-sort-alist (fsd-argument-sort-alist fsd))))
                            (return nil))))))))

(definline constant-sort-p (constant sort)
  (or (top-sort? sort)
      (subsort1? (constant-sort constant) sort)))

(definline variable-sort-p (variable sort)
  (or (top-sort? sort)
      (subsort1? (variable-sort variable) sort)))

(defun term-sort-p (term sort &optional subst)
  (or (top-sort? sort)
      (dereference
       term subst
       :if-constant (constant-sort-p term sort)
       :if-variable (variable-sort-p term sort)
       :if-compound (subsort1? (term-sort term subst) sort))))

(defun term-subsort-p (term1 term2 &optional subst)
  (or (dereference					;allows wffs for rewriting
       term2 subst
       :if-constant (constant-boolean-valued-p term2)
       :if-compound-appl (function-boolean-valued-p (heada term2))
       :if-variable (dereference
		     term1 subst
		     :if-constant (constant-boolean-valued-p term1)
		     :if-compound-appl (function-boolean-valued-p (head term1))))
      (term-sort-p term1 (term-sort term2 subst) subst)))

(defun sort-compatible-p (term1 term2 &optional subst)
  (let ((sort2 (term-sort term2 subst)))
    (or (top-sort? sort2) (not (sort-disjoint? (term-sort term1 subst) sort2)))))

(defun check-associative-function-sort (fn)
  (let ((fsds (function-sort-declarations fn)))
    (cond
     ((null fsds)
      (top-sort))
     ((rest fsds)
      (error "The associative function ~A can have only one sort declaration." fn))
     (t
      (let* ((fsd (first fsds))
             (sort (fsd-result-sort fsd))
             (argsort (fsd-arg-sort fsd t)))
        (cond
         ((and ;;(subsort? argsort sort)
               (same-sort? argsort sort)
               (every (lambda (p) (same-sort? argsort (cdr p)))
                      (fsd-argument-sort-alist fsd)))
          sort)
         (t
          ;; SNARK doesn't support sort declarations like
          ;; (positive (t real)) for associative functions
          ;; because (= (f ?real 0) ?real) cannot always be used as a rewrite
          ;; since the rhs sort is not a subsort of the lhs sort
          (setf (first fsds) (make-function-sort-declaration
                              :result-sort sort
                              :argument-sort-alist (list (cons t sort))))
          (warn "The associative function ~A is required to have arguments of sort ~A." fn sort)
          sort)))))))

(defun associative-function-sort (fn)
  (let ((fsds (function-sort-declarations fn)))
    (if (null fsds) (top-sort) (fsd-result-sort (first fsds)))))

(defun associative-function-argument-sort (fn)
  (let ((fsds (function-sort-declarations fn)))
    (if (null fsds) (top-sort) (fsd-arg-sort (first fsds) t))))

;;; sorts.lisp EOF
