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
;;; Portions created by the Initial Developer are Copyright (C) 1981-2012.
;;; All Rights Reserved.
;;;
;;; Contributor(s): Mark E. Stickel <stickel@ai.sri.com>.

(in-package :snark)

(defun declare-the-sort-function-symbol (name sort)
  (declare-function
   (intern (to-string :the- name) :snark-user) 1
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

(defun declare-function-sort (function sort-spec)
  (cond
   ((function-boolean-valued-p function)
    (setf (function-argument-sort-alist function) (input-argument-sort-alist function sort-spec)))
   ((sort-name-expression? sort-spec nil)
    (setf (function-sort function) (the-sort sort-spec)))
   (t
    (setf (function-sort function) (the-sort (first sort-spec)))
    (setf (function-argument-sort-alist function) (input-argument-sort-alist function (rest sort-spec)))))
  (when (function-associative function)
    (check-associative-function-sort function))
  nil)

(defvar *%check-for-well-sorted-atom%* t)

(defun check-for-well-sorted-atom (atom &optional subst)
  (when *%check-for-well-sorted-atom%*
    (assert-atom-is-well-sorted atom subst))
  atom)

(defun assert-atom-is-well-sorted (atom &optional subst)
  (or (well-sorted-p atom subst)
      (error "Atomic formula ~A is not well sorted." (term-to-lisp atom subst))))

(defun check-well-sorted (x &optional subst)
  (unless (well-sorted-p x subst)
    (error "~A is not well sorted." (term-to-lisp x subst)))
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
  (let ((head (head term)))
    (dolist (fun (function-sort-code head) (function-sort head))
      (let ((v (funcall fun term subst)))
        (unless (or (null v) (eq none v))
          (return v))))))

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
   :if-compound (prog->
                  (well-sort-args (args x) subst (function-argument-sort-alist (head x)) ->* subst)
                  (when (subsort? (term-sort x subst) sort)
                    (funcall cc subst))))
  nil)

(defun well-sort-args (cc args subst asa &optional (argcount 0))
  (dereference
   args subst
   :if-constant (funcall cc subst)
   :if-variable (funcall cc subst)
   :if-compound-appl (funcall cc subst)
   :if-compound-cons (prog->
                       (well-sort (carc args) subst (asa-arg-sort asa (incf argcount)) ->* subst)
                       (well-sort-args (cdrc args) subst asa argcount ->* subst)
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
        (warn "Atomic formula ~A is not well sorted.~%Its arguments are well sorted, so will continue." (term-to-lisp atom subst))
        t)
       (t
        (warn "Atomic formula ~A is not well sorted.~%Will try to make its arguments well sorted and continue." (term-to-lisp atom subst))
        nil)))
     (t
      (warn "Atomic formula ~A is not well sorted." (term-to-lisp atom subst))
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
                               (setf v (make-variable (function-sort fn)))
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
       :if-compound (let ((head (head val)))
                      (unless (same-sort? sort (function-sort head)))
                        (if first
                            (setf (function-sort head) sort)
                            (return nil)))))))


(definline constant-sort-p (constant sort)
  (or (top-sort? sort)
      (subsort1? (constant-sort constant) sort)))

(definline variable-sort-p (variable sort)
  (or (top-sort? sort)
      (subsort1? (variable-sort variable) sort)))

(defun term-sort-p (term sort &optional subst)
  (or (top-sort? sort)
      (subsort1? (term-sort term subst) sort)))

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
  ;; force sort specification to be of form (sort (t sort))
  (let ((sort (function-sort fn))
        (asa (function-argument-sort-alist fn)))
    (unless (and (eq t (car (first asa))) (same-sort? sort (cdr (first asa))))
      (setf (function-argument-sort-alist fn) (list (cons t sort)))
      (unless (and (same-sort? sort (asa-arg-sort asa 1)) (same-sort? sort (asa-arg-sort asa 2)))
        (warn "The associative function ~A is required to have arguments of sort ~A." fn sort)))
    sort))

;;; sorts.lisp EOF
