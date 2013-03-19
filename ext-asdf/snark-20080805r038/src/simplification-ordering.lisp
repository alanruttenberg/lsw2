;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: simplification-ordering.lisp
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
;;; Portions created by the Initial Developer are Copyright (C) 1981-2006.
;;; All Rights Reserved.
;;;
;;; Contributor(s): Mark E. Stickel <stickel@ai.sri.com>.

(in-package :snark)

(declaim
  (special
    *manual-ordering-results*
    *negative-hyperresolution*))

(defun manual-ordering-compare-terms (x y subst)
  (setf x (renumber x subst))
  (setf y (renumber y subst))
  (let (v)
    (cond
      ((setf v (assoc (list x y) *manual-ordering-results* :test #'subsumed-p))
       (cdr v))
      ((setf v (assoc (list y x) *manual-ordering-results* :test #'subsumed-p))
       (opposite-order (cdr v)))
      (t
       (loop
	 (format t "~%You must answer the following simplification-ordering question:")
         (format t "~%~S~% is < or > or ? to~%~S" x y)
         (format t "~%Answer =")
	 (setf v (read))
	 (cond
	   ((member v '(< > ?))
	    (setf *manual-ordering-results* (acons (list x y) v *manual-ordering-results*))
	    (return v))
	   (t
	    (format t "~&You must answer < or > or ?."))))))))

(defun definition-p (x y)
  (and (compound-p x)
       (let ((args nil))
	 (and (not (function-occurs-p (head x) y nil))
	      (dolist (arg (args x) t)
		(cond
		  ((and (variable-p arg)
			(top-sort? (variable-sort arg))
			(not (member arg args :test #'eq)))
		   (push arg args))
		  (t
		   (return nil))))
	      (member (instantiating-direction1 args (variables y)) '(> <>))))))

(defun simplification-ordering-compare-terms0 (x y subst testval)
  (case (use-term-ordering?)
    (:rpo
     (rpo-compare-terms-top x y subst testval))
    (:kbo
     (kbo-compare-terms x y subst))
    ((nil :manual)
     (cond
      ((equal-p x y subst)
       '=)
      ((occurs-p x y subst)
       '<)
      ((occurs-p y x subst)
       '>)
      ((use-term-ordering?)
       (manual-ordering-compare-terms x y subst))
      (t
       '?)))
    (otherwise
     (funcall (use-term-ordering?) x y subst testval))))

(defun simplification-ordering-compare-terms1 (x y &optional subst testval warn row)
  (let ((dir (simplification-ordering-compare-terms0 x y subst testval)))
    (when warn
      (when (and (print-rewrite-orientation?)
                 (not (member (print-rows-when-derived?) '(nil :signal)))
                 (member dir '(< >))
                 row (row-number row))
        (with-clock-on printing
          (terpri-comment)
          (format t "Oriented ~A ~A "
                  (row-name-or-number row)
                  (cond
                   ((eq '> dir) "left-to-right")
                   ((eq '< dir) "right-to-left")))))
      (when (and (print-unorientable-rows?)
                 (not (member (print-rows-when-derived?) '(nil :signal)))
                 (not (member dir '(< > =))))
        (with-clock-on printing
          (terpri-comment)
          (cond
           ((and row (row-number row))
            (format t "Could not orient ~A " (row-name-or-number row)))
           (t
            (format t "Could not orient ~A=~A " x y))))))
    dir))

(defun simplification-ordering-compare-terms (x y &optional subst testval warn row)
  (with-clock-on ordering
    (simplification-ordering-compare-terms1 x y subst testval warn row)))

(defvar *simplification-ordering-compare-equality-arguments-hash-table*)

(defun initialize-simplification-ordering-compare-equality-arguments-hash-table ()
  (setf *simplification-ordering-compare-equality-arguments-hash-table*
        (if (test-option2?)
            (make-hash-table)
            nil)))

(defun simplification-ordering-compare-equality-arguments (equality subst &optional warn row)
  (if (test-option2?)
      (let* ((table *simplification-ordering-compare-equality-arguments-hash-table*)
             (v (gethash equality table)))
        (cond
         ((null v)
          (setf v (let ((args (args equality)))
                    (simplification-ordering-compare-terms
                     (first args) (second args) subst nil warn row)))
          (cl:assert v)
          (when (or (null subst) (eq '? v))
            (setf (gethash equality table) v))
          v)
         ((or (null subst) (neq '? v))
          v)
         (t
          (let ((args (args equality)))
            (simplification-ordering-compare-terms
             (first args) (second args) subst nil warn row)))))
      (let ((args (args equality)))
        (simplification-ordering-compare-terms
         (first args) (second args) subst nil warn row))))

(defun simplification-ordering-greaterp (x y subst)
  (eq '> (simplification-ordering-compare-terms x y subst '>)))

(defun instantiating-direction1 (xvars yvars)
  (let ((x-has-var-not-in-y (dolist (xv xvars)
			      (when (dolist (yv yvars t)
				      (when (eql xv yv)
					(return nil)))
				(return t))))
	(y-has-var-not-in-x (dolist (yv yvars)
			      (when (dolist (xv xvars t)
				      (when (eql xv yv)
					(return nil)))
				(return t)))))
    (cond
      (x-has-var-not-in-y
       (cond
	 (y-has-var-not-in-x
	  nil)
	 (t
	  '>)))
      (y-has-var-not-in-x
       '<)
      (t
       '<>))))

(defun instantiating-direction (x y subst)
  ;; returns <> x and y have the same variables
  ;; returns > if y's variables are proper subset of x's
  ;; returns < if x's variables are proper subset of y's
  ;; returns nil otherwise
  (with-clock-on ordering
    (instantiating-direction1 (variables x subst) (variables y subst))))

(defun literal-ordering-a (atom1 polarity1 atom2 polarity2 &optional subst testval)
  (declare (ignore polarity1 polarity2))
  (simplification-ordering-compare-terms atom1 atom2 subst testval))

(defun literal-ordering-p (atom1 polarity1 atom2 polarity2 &optional subst testval)
  ;; positive literals are ordered; no ordering between negative literals
  ;; negative literals are greater than positive literals
  (case polarity1
    (:pos
     (case polarity2
       (:pos
        (simplification-ordering-compare-terms atom1 atom2 subst testval))
       (:neg
        '<)
       (otherwise
        '?)))
    (:neg
     (case polarity2
       (:pos
        '>)
       (otherwise
        '?)))
    (otherwise
     '?)))

(defun literal-ordering-n (atom1 polarity1 atom2 polarity2 &optional subst testval)
  ;; negative literals are ordered; no ordering between positive literals
  ;; positive literals are greater than negative literals
  (case polarity1
    (:neg
     (case polarity2
       (:neg
        (simplification-ordering-compare-terms atom1 atom2 subst testval))
       (:pos
        '<)
       (otherwise
        '?)))
    (:pos
     (case polarity2
       (:neg
        '>)
       (otherwise
        '?)))
    (otherwise
     '?)))

(defun literal-is-not-dominated-in-clause-p (orderfun atom polarity clause subst)
  (prog->
    (map-atoms-in-clause clause ->* atom2 polarity2)
    (when (and (neq atom atom2)
               (eq '< (funcall orderfun atom polarity atom2 polarity2 subst '<)))
      (return-from literal-is-not-dominated-in-clause-p nil)))
  t)

(defun literal-is-not-dominating-in-clause-p (orderfun atom polarity clause subst)
  (prog->
    (map-atoms-in-clause clause ->* atom2 polarity2)
    (when (and (neq atom atom2)
               (eq '> (funcall orderfun atom polarity atom2 polarity2 subst '>)))
      (return-from literal-is-not-dominating-in-clause-p nil)))
  t)

(defun literal-satisfies-ordering-restriction-p (orderfun atom polarity wff &optional subst n)
  (implies (clause-p wff)
           (literal-is-not-dominated-in-clause-p
            orderfun
            (if (and subst n) (instantiate atom n) atom)
            polarity
            (if (and subst n) (instantiate wff n) wff)
            subst)))

(defun selected-atoms-in-row (row orderfun)
  ;; which atoms in row are selected by orderfun before considering instantiation
  (let* ((selections (row-selections-alist row))
         (v (assoc (or orderfun 'no-literal-ordering) selections)))
    (cond
     (v
      (cdr v))
     (t
      (let ((l nil))
        (cond
         ((row-sequential row)			;if sequential, select only the first atom
          (prog->
            (map-atoms-in-wff (row-wff row) ->* atom polarity)
            (setf l (list (list atom polarity)))
            (return-from prog->)))
         ((or (null orderfun)			;else if no orderfun or row is nonclausal,
              (not (clause-p (row-wff row))))	;select all of the atoms
          (setf l (atoms-in-wff2 (row-wff row))))
         (t					;else use orderfun on literals of clause and
          (prog->				;return eq subset of (selected-atoms-in-row row nil)
            (dolist (selected-atoms-in-row row nil) ->* x)
            (values-list x -> atom polarity)
            (cond
             ((null l)
              (setf l (list x)))
             ((dolist (y l t)			;select atom if it is not dominated by any atom2
                (mvlet (((:list atom2 polarity2) y))
                  (when (eq '> (funcall orderfun atom2 polarity2 atom polarity nil '>))
                    (return nil))))
              (setf l (nconc
                       (delete-if (lambda (y)	;deselect every atom2 that is dominated by atom
                                    (mvlet (((:list atom2 polarity2) y))
                                      (eq '< (funcall orderfun atom2 polarity2 atom polarity nil '<))))
                                  l)
                       (list x))))))))
        (setf (row-selections-alist row) (acons (or orderfun 'no-literal-ordering) l selections))
        l)))))

(defun selected-atom-in-row-p (atom polarity row orderfun &optional subst n atom*)
  (selected-atom-p atom polarity (selected-atoms-in-row row orderfun) orderfun subst n atom*))

(defun selected-atom-p (atom polarity selected-atoms orderfun &optional subst n atom*)
  ;; selected-atoms was computed by (selected-atoms-in-row row orderfun)
  ;; to list which atoms are selected before considering instantiation
  ;; both (p ?x ?y) and (p ?y ?x) might be in selected-atoms,
  ;; but only one might be acceptable to selected-atom-p when ?x and ?y are instantiated
  (let ((atom&polarity (literal-member-p atom polarity selected-atoms)))
    (and atom&polarity				;is (atom polarity) in selected-atoms?
         (implies (and orderfun subst)
                  (dolist (x selected-atoms t)	;is it still undominated after applying subst?
                    (unless (eq atom&polarity x)
                      (let ((atom2 (first x)) (polarity2 (second x)))
                        (when (eq '> (funcall orderfun
                                              (instantiate atom2 n)
                                              polarity2
                                              (setq-once atom* (instantiate atom n))
                                              polarity
                                              subst
                                              '>))
                          (return nil)))))))))

(defun selected-atoms-in-hyperresolution-electrons-p (electrons subst)
  (prog->
    (hyperresolution-orderfun -> orderfun)
    (hyperresolution-electron-polarity -> polarity)
    (+ (length electrons) 1 -> k)
    (dolist electrons t ->* x)
    (values-list x -> rowk atomk atomk*)
    (selected-atoms-in-row rowk orderfun -> selected-atoms-in-rowk)
    (unless (selected-atom-p atomk polarity selected-atoms-in-rowk orderfun subst k atomk*)
      (return nil))
    (decf k)))

(defmethod theory-simplify (wff (theory (eql 'ordering)))
  ;; no decision procedure:
  ;;   only tests conjuncts singly
  ;;   only treats variables as universally quantified
  (prog->
    (map-atoms-in-wff-and-compose-result wff :neg ->* atom polarity)
    (declare (ignore polarity))
    (args atom -> args)
    (ecase (function-name (head atom))
      (ordering>
       (ecase (simplification-ordering-compare-terms (first args) (second args)) (? atom) (> true) (< false) (= false)))
      (ordering>=
       (ecase (simplification-ordering-compare-terms (first args) (second args)) (? atom) (> true) (< false) (= true))))))

;;; simplification-ordering.lisp EOF
