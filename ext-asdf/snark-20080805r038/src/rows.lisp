;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: rows.lisp
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

(defvar *rowsets*)
(defvar *rows*)
(defvar *row-count* 0)
(defvar *number-of-rows* 0)
(defvar *row-names*)
(declaim (type integer *row-count* *number-of-rows*))

(defun uninitialized (slot-name)
  (error "Value of row slot ~A was not supplied to make-row." slot-name))

(defstruct (row
	     (:constructor make-row0)
	     (:print-function print-row3))
  (number nil)
  (wff nil)
  (constraints nil)				;alist of theory names and wffs
  (answer false)
  (reason (uninitialized 'reason))
  (hints-subsumed nil)				;hint rows that are backward subsumed by this row
  (context (uninitialized 'context))		;row was added to/deleted from this pair of contexts
  (children nil)
  (rewrites nil)				;list of rewrites formed from this row
  (supported nil)
  (sequential nil)				;only leftmost literal usable
  (positive-or-negative none)
  (subsumption-mark nil)
  (subsumption-matches nil)
  (status nil)
  (agenda-entries nil)
  (level0 nil)					;computed and set by row-level function
  (wff-symbol-counts0 nil)
  (selections-alist nil)
  (plist nil))					;property list for more properties

(define-plist-slot-accessor row :documentation)
(define-plist-slot-accessor row :author)
(define-plist-slot-accessor row :source)
(define-plist-slot-accessor row :name)
(define-plist-slot-accessor row :conc-name)
(define-plist-slot-accessor row :input-wff)

(defun row-wff-symbol-counts (row)
  (or (row-wff-symbol-counts0 row)
      (setf (row-wff-symbol-counts0 row) (wff-symbol-counts (row-wff row)))))

(defun row-name-or-number (row)
  (or (row-name row) (row-number row)))

(defmacro make-row (&rest args)
  (let ((args0 nil) args0-last
        (plist nil) plist-last
        (v (gensym)))
    (do ((l args (cddr l)))
        ((endp l))
      (cond
       ((member (first l) '(:documentation :author :source :name :conc-name :input-wff))
        (collect `(let ((,v ,(second l))) (if ,v (list ,(first l) ,v) nil)) plist))
       (t
        (collect (first l) args0)
        (collect (second l) args0))))
    (when plist
      (collect :plist args0)
      (collect (if (rest plist) (cons 'nconc plist) (first plist)) args0))
    `(prog1
       (make-row0 ,@args0)
       (incf *row-count*))))

(defun initialize-rows ()
  ;; row structures can be stored in sets called rowsets
  ;; *rowsets* is a matrix that stores all of the rowsets
  ;; each row-index is (row-number row-defstruct)
  ;; each column is one of the rowsets
  ;; (the column-index is arbitrary because they are not accessed by number)
  ;; the value of each entry is the row-defstruct
  (setf *rowsets* (make-sparse-matrix))
  (setf *rows* (make-rowset))
  (setf *row-names* (make-hash-table))
  nil)

(defun row-given-p (row)
  (eq :given (row-status row)))

(defun row-deleted-p (row)
  (eq :deleted (row-status row)))

(defun row-hint-p (row)
  (eq 'hint (row-reason row)))

(defun row-input-p (row)
  (= 0 (row-level row)))

(defun row-nonassertion-p (x)
  (when (row-p x)
    (setf x (row-reason x)))
  (if (consp x)
      (some #'row-nonassertion-p (rest x))
      (member x '(assumption negated_conjecture))))

(defun row-from-conjecture-p (x)
  (when (row-p x)
    (setf x (row-reason x)))
  (if (consp x)
      (some #'row-from-conjecture-p (rest x))
      (member x '(negated_conjecture))))

(defun row-parents (row)
  (rows-in-reason (row-reason row)))

(defun row-parent (row)
  (let ((l (row-parents row)))
    (cl:assert (and l (null (rest l))))
    (first l)))

(defun row-embedding-p (row)
  (let ((reason (row-reason row)))
    (and (consp reason)
         (eq 'embed (first reason))
         (or (third reason) t))))

(defun row-rewrites-used (row)
  (let ((reason (row-reason row)))
    (cond
     ((and (consp reason) (eq 'rewrite (first reason)))
      (rrest reason))
     (t
      nil))))

(defun (setf row-rewrites-used) (value row)
  (let ((reason (row-reason row)))
    (cond
     ((and (consp reason) (eq 'rewrite (first reason)))
      (cl:assert (tailp (rrest reason) value))
      (setf (row-reason row) (list* 'rewrite (second reason) value)))
     (value
      (setf (row-reason row) (list* 'rewrite reason value))))
    value))

(defun row-level (row)
  (or (row-level0 row)
      (setf (row-level0 row)
	    (labels
	      ((row-level* (reason)
		 (ecase (if (consp reason) (first reason) reason)
		   ((resolve hyperresolve negative-hyperresolve ur-resolve ur-pttp paramodulate combine)
		    (+ 1 (loop for parent in (rest reason)
			       when (row-p parent)
			       maximize (row-level parent))))
		   ((rewrite factor condense embed case-split purify)
                    ;; ignore level of rewriters
                    (let ((parent (second reason)))
		      (if (row-p parent)
                          (row-level parent)
                          (row-level* parent))))
		   ((assertion assumption negated_conjecture hint)
		    0)
		   (and
		     (loop for reason in (rest reason)
                           minimize (row-level* reason))))))
	      (row-level* (row-reason row))))))

(defun row-clause-p (row)
  (clause-p (row-wff row)))

(defun row-unit-p (row)
  (literal-p (row-wff row)))

(defun row-bare-p (row)
  (and (eq false (row-answer row))
       (not (row-constrained-p row))
;;     (null (row-dp-alist row))
       ))

(defun row-bare-unit-p (row)
  (and (row-unit-p row)
       (row-bare-p row)))

(defun row-positive-p (row)
  (let ((v (row-positive-or-negative row)))
    (when (eq none v)
      (setf v (setf (row-positive-or-negative row) (wff-positive-or-negative (row-wff row)))))
    (eq :pos v)))

(defun row-negative-p (row)
  (let ((v (row-positive-or-negative row)))
    (when (eq none v)
      (setf v (setf (row-positive-or-negative row) (wff-positive-or-negative (row-wff row)))))
    (eq :neg v)))

(defun row-variables (row &optional vars)
  (setf vars (variables (row-wff row) nil vars))
  (setf vars (variables (row-constraints row) nil vars))
  (setf vars (variables (row-answer row) nil vars))
  vars)

(defun row-supported-inheritably (row)
  (let ((supported (row-supported row)))
    (and supported
	 (neq :uninherited supported))))

(defun row-sequential-inheritably (row)
  (let ((sequential (row-sequential row)))
    (and sequential
	 (neq :uninherited sequential))))

(defun make-rowset (&optional (rowsets *rowsets*))
  (if rowsets
      (let ((n (nonce)))
        (values (setf (sparse-matrix-column rowsets n) t) n))
      (make-sparse-vector)))

(defun rowset-size (rowset)
  (sparse-vector-count rowset))

(defun rowset-insert (row rowset)
  (let ((num (row-number row)))
    (and (not (sparef rowset num))
         (setf (sparef rowset num) row))))

(defun rowset-delete (row rowset)
  (when rowset
    (let ((num (row-number row)))
      (setf (sparef rowset num) nil))))

(defun rowsets-delete (row &optional (rowsets *rowsets*))
  ;; delete row from every rowset it is in
  (when rowsets
    (let ((num (row-number row)))
      (setf (sparse-matrix-row rowsets num) nil))))

(defun rowsets-delete-column (rowset)
  (when rowset
    (let ((type (snark-sparse-array::sparse-vector-type rowset)))
      (when (eq 'snark-sparse-array::column (first type))
        (setf (sparse-matrix-column (second type) (third type)) nil)))))

(defun rowset-empty? (rowset)
  (or (null rowset) (eql 0 (sparse-vector-count rowset))))

(defun map-rows-in-reason (fn x)
  (cond
   ((consp x)
    (map-rows-in-reason fn (car x))
    (map-rows-in-reason fn (cdr x)))
   ((row-p x)
    (funcall fn x)
    nil)))

(defun rows-in-reason (x &optional rows)
  (cond
   ((consp x)
    (rows-in-reason (cdr x) (rows-in-reason (car x) rows)))
   ((row-p x)
    (adjoin x rows))
   (t
    rows)))

(defun row-ancestry-rowset (rows)
  (let ((rowset (make-rowset nil)))
    (labels
      ((row-ancestry-rowset* (x)
         (when (and (row-p x) (rowset-insert x rowset))
           (dolist (x (rows-in-reason (row-rewrites-used x) (rows-in-reason (row-reason x))))
             (row-ancestry-rowset* x)))))
      (dolist (row rows)
        (row-ancestry-rowset* row))
      rowset)))

(defun row-ancestry (row)
  (let ((result nil) result-last)
    (prog->
     (map-sparse-vector (row-ancestry-rowset (list row)) ->* row)
     (collect row result))
    result))

(defun row (name-or-number &optional not-found-action)
  ;; Return the row named or numbered by the argument.
  ;; If error-p is true, it is an error if the row cannot be found;
  ;; otherwise, nil is returned if the row cannot be found.
  (cond
   ((row-p name-or-number)	;also allow a row itself as argument
    name-or-number)
   ((numberp name-or-number)
    (when (minusp name-or-number)
      (setf name-or-number (+ *number-of-rows* name-or-number 1)))
    (or (sparef *rows* name-or-number)
        (and not-found-action (funcall not-found-action "There is no row numbered ~D." name-or-number))))
   (t
    (let ((number (gethash name-or-number *row-names*)))
      (or (and number (sparef *rows* number))
          (and not-found-action (funcall not-found-action "There is no row named ~S." name-or-number)))))))

(defun mapnconc-rows (cc &key (rowset *rows*) min max reverse test)
  (when rowset
    (let ((result nil) result-last)
      (prog->
       (map-sparse-vector rowset :min min :max max :reverse reverse ->* row)
       (when (implies test (funcall test row))
         (cond
          ((null cc)
           (collect row result))
          (t
           (ncollect (funcall cc row) result)))))
      result)))

(defun map-rows (cc &key (rowset *rows*) min max reverse test)
  (when rowset
    (if (null test)
        (map-sparse-vector cc rowset :min min :max max :reverse reverse)
        (prog->
          (map-sparse-vector rowset :min min :max max :reverse reverse ->* row)
          (when (funcall test row)
            (funcall cc row))))))

(defun rows (&key (rowset *rows*) min max reverse test collect)
  (when rowset
    (let ((result nil) result-last)
      (prog->
       (map-sparse-vector rowset :min min :max max :reverse reverse ->* row)
       (when (implies test (funcall test row))
         (collect (if collect (funcall collect row) row) result)))
      result)))

(defun last-row ()
  (last-sparef *rows*))

(defun set-row-number (row number)
  (cl:assert (null (row-number row)))
  (setf (row-number row) number)
  (let (v)
    (cond
     ((setf v (row-name row))
      (setf (row-name row) nil)
      (name-row row v))
     ((setf v (row-conc-name row))
      (name-row row (intern (format nil "~A~D" v number)))))))

(defun name-row (row-id name)
  (when (can-be-row-name name 'warn)
    (let* ((row (if (row-p row-id) row-id (row row-id 'error)))
           (number (row-number row)))
      (cl:assert (integerp number))
      (let ((number2 (gethash name *row-names*)))
        (when (and number2 (neql number number2))
          (warn "Naming row ~D ~A, but row ~D is already named ~A.  Reusing the name." number name number2 name)))
      (let ((name2 (row-name row)))
        (when (and name2 (neql name name2))
          (warn "Naming row ~D ~A, but row ~D is already named ~A.  Renaming the row." number name number name2)))
      (setf (gethash name *row-names*) number)
      (setf (row-name row) name))))

(defun print-ancestry (row &key more-rows format)
  (prog->
    (map-rows :rowset (row-ancestry-rowset (cons row more-rows)) ->* row)
    (terpri)
    (when more-rows
      (princ (if (member row more-rows) "*" " ")))
    (print-row row :format format)))

(defun print-rows (&key (rowset *rows*) min max reverse (test (print-rows-test?)) ancestry format)
  (if ancestry
      (print-rows :rowset (row-ancestry-rowset (rows :rowset rowset :min min :max max :test test)) :reverse reverse :format format)
      (prog->
        (map-rows :rowset rowset :min min :max max :reverse reverse :test test ->* row)
        (terpri)
        (print-row row :format format))))

;;; rows.lisp EOF
