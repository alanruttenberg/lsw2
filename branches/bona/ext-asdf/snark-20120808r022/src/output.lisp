;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: output.lisp
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

(defmacro with-no-output (&body forms)
  ;; turn off SNARK printing options and redirect any remaining output to /dev/null
  ;; example usage:
  ;; (with-no-output
  ;;   (initialize)
  ;;   (assert ...)
  ;;   (prove ...))
  `(let-options ((default-print-rows-when-derived nil)
                 (default-print-rows-when-given nil)
                 (default-print-rows-when-processed nil)
                 (default-print-final-rows nil)
                 (default-print-unorientable-rows nil)
                 (default-print-pure-rows nil)
                 (default-print-irrelevant-rows nil)
                 (default-print-rewrite-orientation nil)
                 (default-print-summary-when-finished nil)
                 (default-print-clocks-when-finished nil)
                 (default-print-term-memory-when-finished nil)
                 (default-print-agenda-when-finished nil)
                 (default-print-rows-when-finished nil)
                 (default-print-options-when-starting nil)
                 (default-print-assertion-analysis-notes nil)
                 (default-print-symbol-table-warnings nil)
                 (print-rows-when-derived nil)
                 (print-rows-when-given nil)
                 (print-rows-when-processed nil)
                 (print-final-rows nil)
                 (print-unorientable-rows nil)
                 (print-pure-rows nil)
                 (print-irrelevant-rows nil)
                 (print-rewrite-orientation nil)
                 (print-summary-when-finished nil)
                 (print-clocks-when-finished nil)
                 (print-term-memory-when-finished nil)
                 (print-agenda-when-finished nil)
                 (print-rows-when-finished nil)
                 (print-options-when-starting nil)
                 (print-assertion-analysis-notes nil)
                 (print-symbol-table-warnings nil)
                 )
     #+mcl
     (progn ,@forms)
     #-mcl
     (with-open-file (*standard-output*
                      (make-pathname :directory '(:absolute "dev") :name "null")
                      :direction :output
                      :if-exists :append)
       (let ((*error-output* *standard-output*))
         ,@forms))))

(defun print-function-symbol (fn &optional (stream *standard-output*) depth)
  (declare (ignore depth))
  (write (function-name fn) :stream stream)
  fn)

(defun print-variable (x &optional (stream *standard-output*) depth)
  (declare (ignore depth))
  (let ((num (variable-number x))
        (sort (variable-sort x)))
    (princ (first (variable-symbol-prefixes?)) stream)
    (mvlet (((values i j) (floor num 6)))
      (princ (nth j '(x y z u v w)) stream)
      (unless (eql 0 i)
        (write i :stream stream :base 10 :radix nil)))
    (unless (top-sort? sort)
      (princ (variable-sort-marker?) stream)
      (princ (sort-name sort) stream))
    x))

(defun print-term3 (term &optional (stream *standard-output*) depth)
  (declare (ignore depth))
  (print-term term nil stream))

(defun print-term (term &optional subst (stream *standard-output*))
  ;; terms are printed by first converting them to lisp
  (with-standard-io-syntax2
    (write (term-to-lisp term subst) :stream stream))
  term)

(defun print-row-term (term &optional subst (stream *standard-output*))
  (let ((term term))
    (when (print-row-length-limit?)
      (dereference
       term subst
       :if-compound-appl (when (and (eq *or* (heada term)) (< (print-row-length-limit?) (length (argsa term))))
                           (setf term (make-compound* *or* (nconc (firstn (argsa term) (print-row-length-limit?)) '(---)))))))
    (let ((*print-pretty2* (and (print-rows-prettily?) (print-row-wffs-prettily?))))
      (print-term term subst stream)))
  term)

(defmethod print-given-row (row)
  (case (print-rows-when-given?)
    ((nil)
     (when (eq :signal (print-rows-when-derived?))
       (comment)
       (princ #\|)))
    (:signal
     (comment)
     (princ #\|))
    (otherwise
     (with-clock-on printing
       (when (print-time-used?)
	 (print-incremental-time-used))
       (dotimes (dummy (- (case (print-rows-when-derived?)
                            ((:signal nil)
                             (print-given-row-lines-signalling?))
                            (otherwise
                             (print-given-row-lines-printing?)))
                          1))
	 (declare (ignorable dummy))
	 (terpri))
       (terpri)
       (print-row row :string "Infer_from_row ")
       (princ " ")
       (force-output))))
  row)

(defmethod print-derived-row (row)
  (case (print-rows-when-derived?)
    ((nil)
     )
    (:signal
     (comment)
     (princ #\+))
    #+ignore
    (:fact
     (when (let ((wff (row-wff row)))
             (dereference wff nil :if-compound (eq fact-relation (head wff))))
       (with-clock-on printing
         (when (print-time-used?)
           (print-incremental-time-used))
         (terpri)
         (print-row row)
         (princ " "))))
    (otherwise
     (with-clock-on printing
       (when (print-time-used?)
	 (print-incremental-time-used))
       (terpri)
       (print-row row)
       (princ " "))))
  row)

(defun print-processed-row (row)
  (case (print-rows-when-processed?)
    ((nil :signal)
     )
    (otherwise
     (with-clock-on printing
       (when (print-time-used?)
	 (print-incremental-time-used))
       (terpri)
       (let-options ((use-to-lisp-code nil))
         (print-row row :string "Processing_row "))
       (princ " "))))
  row)

(defun print-pure-row (row)
  (case (print-pure-rows?)
    ((nil)
     )
    (otherwise
     (with-clock-on printing
       (when (print-time-used?)
         (print-incremental-time-used))
       (terpri)
       (print-row row :string "Pure_row ")
       (princ " "))))
  row)

(defvar *printing-deleted-messages* nil)

(defun print-deleted-wff (row msg)
  (case (print-rows-when-derived?)
    ((nil)
     )
    (:signal
     (comment)
     (princ (if (equal "deleted because agenda full" msg) #\d #\-)))
    #+ignore
    (:fact
     (when (let ((wff (row-wff row)))
             (dereference wff nil :if-compound (eq fact-relation (head wff))))
       (with-clock-on printing
         (terpri-comment)
         (format t "     ~A ~A" msg (row-name-or-number row)))))
    (otherwise
     (with-clock-on printing
       (cond
        ((equal *printing-deleted-messages* msg)
         (format t ",~A" (row-name-or-number row)))
        (t
         (terpri-comment)
         (format t "~A ~A" msg (row-name-or-number row))
         (setf *printing-deleted-messages* msg))))))
  row)

(defun print-unorientable-wff (equality-or-equivalence)
  (case (print-unorientable-rows?)
    ((nil :signal)
     )
    (otherwise
     (with-clock-on printing
       (warn "Could not orient ~A." equality-or-equivalence))))
  equality-or-equivalence)

(defvar *szs-filespec* nil)

(defvar *szs-conjecture* nil)

(defun print-szs-status (status &optional (nocomment nil) (filespec *szs-filespec*))
  (unless nocomment
    (terpri)
    (princ "#||")
    (terpri))
  (princ "% SZS status ")
  (princ (case status
           (:proof-found
            (if *szs-conjecture* "Theorem" "Unsatisfiable"))
           (:run-time-limit
            "Timeout")
           (:agenda-empty
            "GaveUp")
           (otherwise
            status)))
  (when filespec
    (princ " for ")
    (princ filespec))
  (unless nocomment
    (terpri)
    (princ "||#")
    (terpri)))

(defun print-szs-answers-short (answers)
  (let ((answers (mapcan (lambda (answer)
                           (and (compound-p answer) (eq 'values (function-name (head answer))) (list (args answer))))
                         answers)))
    (when answers
      (princ "% SZS answers short ")
      (print-term-in-tptp-format answers)
      (terpri)
      t)))

(defun print-final-row (row)
  (let ((p (print-final-rows?)))
    (cond
     ((null p)
      )
     ((eq :signal p)
      (comment)
      (princ #\.))
     (t
      (with-clock-on printing
        (unless (eq :tptp p)
          (terpri)
          (terpri)
          (princ "(Refutation")
          (print-ancestry row)
          (terpri)
          (princ ")"))
        (when (or (eq :tptp p) (eq :tptp-too p))
          (terpri)
          (terpri)
	  (princ "#||")
          (terpri)
          (print-szs-status :proof-found t)
	  (terpri)
          (print-szs-answers-short (list (row-answer row)))
          (princ "% SZS output start Refutation")
          (print-ancestry row :format :tptp)
          (terpri)
          (princ "% SZS output end Refutation")
	  (terpri)
	  (princ "||#")))))
    row))

(defun replace-rows-by-name-or-number (x)
  (cond
   ((consp x)
    (lcons (replace-rows-by-name-or-number (car x)) (replace-rows-by-name-or-number (cdr x)) x))
   ((row-p x)
    (row-name-or-number x))
   (t
    x)))

(defun print-row-reason (row)
  (with-standard-io-syntax2
    (prin1 (replace-rows-by-name-or-number (row-reason row))))
  nil)

(defun print-row3 (row *standard-output* depth)
  "this function is used in the defstruct for ROW to print rows."
  (declare (ignore depth))
  (let-options ((print-rows-shortened nil)
		(print-rows-prettily nil)
                (print-row-reasons nil)
		(print-row-answers nil)
		(print-row-constraints nil)
                (print-row-partitions nil))
    (print-row row)))

(defun print-row-length-limit1 (row)
  (let ((n1 (print-rows-shortened?)))
    (and n1
         (let* ((reason (row-reason row))
                (n2 (and (consp reason)
	                 (eq 'resolve (first reason))
	                 (row-p (third reason))
	                 (clause-p (row-wff (third reason)))
	                 (wff-length (row-wff (third reason))))))
           (if (numberp n1)
               (if n2 (min n1 n2) n1)
               n2)))))

(defun print-row (row &key (string "Row ") format ancestry reverse)
  (setf row (row row 'warn))
  (cond
   ((null row)
    )
   (ancestry
    (print-rows
     :rowset (let ((rowset (make-rowset))) (rowset-insert row rowset) rowset)
     :format format
     :ancestry ancestry
     :reverse reverse))
   (t
    (ecase format
      ((nil)
       (with-standard-io-syntax2
         (princ "(")
         (princ string)
         (prin1 (row-name-or-number row))
         (cond
          ((print-rows-prettily?)
           (terpri)
           (princ "   "))
          (t
           (princ " ")))
         (let-options ((print-row-length-limit (print-row-length-limit1 row)))
           (print-row-term
            (cond
             ((not (print-row-goals?))
              (prog->
                (map-atoms-in-wff-and-compose-result (row-wff row) ->* atom polarity)
                (declare (ignore polarity))
                (dereference
                 atom nil
                 :if-constant (if (proposition-magic-goal-p atom) true atom)
                 :if-compound (if (relation-magic-goal-p (head atom)) true atom))))
             (t
              (row-wff row)))))
         (when (print-row-reasons?)
           (cond
            ((print-rows-prettily?)
             (terpri)
             (princ "   "))
            (t
             (format t "~70T")))
           (print-row-reason row))
         (when (print-row-constraints?)
           (dolist (x (row-constraints row))
             (unless (eq true (cdr x))
               (terpri)
               (princ "   ")
               (princ (string-capitalize (car x)))
               (princ "-Constraint ")
               (print-row-term (negate (cdr x))))))
         (when (print-row-answers?)
           (let ((answer (row-answer row)))
             (unless (eq false answer)
               (terpri)
               (princ "   Answer ")
               (print-row-term answer))))
         (when (and (use-partitions?) (print-row-partitions?))
           (terpri)
           (princ "   Partitions ")
           (prin1 (mapcar #'car (row-context row))))
         (princ ")")))
      (:tptp
       (print-row-in-tptp-format row)))))
  row)

(defvar *propositional-abstraction-term-to-lisp* nil)

(defun term-to-lisp (term &optional subst)
  "Return a Lisp data structure for the given term."
  ;; returns (f a b c) for SNARK term f(a,b,c)
  ;; returns (list a b c) for SNARK term [a,b,c]
  ;;  use variable-p, variable-number, variable-sort
  ;;  sort information is invalid after SNARK is reinitialized
  (labels
    ((term-to-lisp (term)
       (dereference
	term subst
	:if-constant (let ((name (constant-name term)))
                       (cond
                        ((not (can-be-constant-name name))
                         (list '$$quote name))
                        (t
                         name)))
	:if-variable (dolist (fun (if (use-to-lisp-code?) (mklist (variable-to-lisp-code?)) nil) term)
                       (let ((v (funcall fun term)))
                         (unless (eq none v)
                           (return v))))
	:if-compound (let ((head (head term))
                           (args (args term)))
                       (cond
                        ((and *propositional-abstraction-term-to-lisp*
                              (not (function-logical-symbol-p head)))
                         (list (function-name head) (function-arity head)))
                        (t
                         (dolist (fun (if (use-to-lisp-code?) (function-to-lisp-code head) nil) (cons (function-name head) (args-to-lisp args)))
                           (let ((v (funcall fun head args subst)))
                             (unless (eq none v)
                               (return v)))))))))
     (args-to-lisp (args)
       (lcons (term-to-lisp (first args)) (args-to-lisp (rest args)) args)))
    (term-to-lisp term)))

(defun cons-term-to-lisp (head args subst)
  ;; converts
  ;;   (a)        to ($$list a)
  ;;   (a b)      to ($$list a b)
  ;;   (a . b)    to ($$cons a b)
  ;;   (a b . c)  to ($$list* a b c)
  ;; when used as to-lisp-code for cons
  (cl:assert (eq *cons* head))
  (let* ((y (term-to-lisp (second args) subst))
         (x (term-to-lisp (first args) subst)))
    (cond
     ((null y)
      (list (current-function-name '$$list :any) x))
     ((atom y)
      (list (function-name head) x y))
     (t
      (let ((v (first y)) list*)
        (cond
         ((eq v (current-function-name '$$list :any))
          (list* v x (rest y)))
         ((or (eq v (setf list* (current-function-name '$$list* :any)))
              (eq v (function-name head)))
          (list* list* x (rest y)))
         (t
          (list (function-name head) x y))))))))

(defun quant-compound-to-lisp (head args subst)
  (list (function-name head)
        (mapcar (lambda (var-spec)
                  (if (variable-p var-spec)
                      (term-to-lisp var-spec subst)
                      (mapcar #'(lambda (x) (term-to-lisp x subst)) var-spec)))
                (first args))
        (term-to-lisp (second args) subst)))

(defun row-sorts (row &optional sorts)
  (prog->
   (map-terms-in-wff (row-wff row) ->* term polarity)
   (declare (ignore polarity))
   (let ((sort (term-sort term)))
     (unless (top-sort? sort)
       (pushnew (term-sort term) sorts :test #'same-sort?))))
  sorts)

(defun derivation-sorts (row)
  (let ((sorts nil))
    (dolist (row (row-ancestry row))
      (setf sorts (row-sorts row sorts)))
    sorts))

(defun subsort-forms (sorts)
  (let ((result nil))
    (dotails (l sorts)
      (let ((sort1 (first l)))
	(dolist (sort2 (rest l))
	  (cond
	    ((subsort? sort1 sort2)
	     (push `(subsort ,(sort-name sort1) ,(sort-name sort2)) result))
	    ((subsort? sort2 sort1)
	     (push `(subsort ,(sort-name sort2) ,(sort-name sort1)) result))))))
    result))

(defun derivation-subsort-forms (row)
  (subsort-forms (derivation-sorts row)))

;;; output.lisp EOF
