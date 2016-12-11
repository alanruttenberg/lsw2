;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: subsume.lisp
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
;;; Portions created by the Initial Developer are Copyright (C) 1981-2009.
;;; All Rights Reserved.
;;;
;;; Contributor(s): Mark E. Stickel <stickel@ai.sri.com>.

(in-package :snark)

(declaim
  (special
    *false-rows*
    *constraint-rows*))

(defvar *subsuming* nil)

(defun make-and-freeze-variable (&optional sort number)
  (let ((v (make-variable sort number)))
    (push v *frozen-variables*)
    v))

(defun subsume (cc x y &optional subst)
  (prog->
    (identity *subsuming* -> sb)
    (quote t -> *subsuming*)
    (identity *frozen-variables* -> fv)		;save list of frozen variables
    (variables y subst fv -> *frozen-variables*)	;add y's variables to frozen variables
    (unify x y subst ->* subst)
    (identity sb -> *subsuming*)
    (identity fv -> *frozen-variables*)		;restore list of frozen variables
    (funcall cc subst)))

(defun subsumes-p (x y &optional subst)
  ;; x subsumes y?
  (subsumes-p1 x y (variables y subst *frozen-variables*) subst))

(defun subsumes-p1 (x y *frozen-variables* &optional subst)
  (let ((*subsuming* t))
    (unify-p x y subst)))

(defun subsumed-p (x y &optional subst)
  ;; x is subsumed by y?
  (subsumed-p1 x y (variables x subst *frozen-variables*) subst))

(defun subsumed-p1 (x y *frozen-variables* &optional subst)
  (let ((*subsuming* t))
    (unify-p y x subst)))

(defun subsumers (x y &optional subst)
  (subsumers1 x y (variables y subst *frozen-variables*) subst))

(defun subsumers1 (x y *frozen-variables* &optional subst)
  (let ((*subsuming* t))
    (unifiers x y subst)))

;;; use-subsumption = nil		don't use subsumption
;;; use-subsumption = :forward		use only forward subsumption
;;; use-subsumption = t			use forward and backward subsumption
;;;
;;; use-subsumption-by-false further specifies the behavior of use-subsumption in the case of
;;; "false rows" (those for which row-wff is false, kept in *false-rows* and *constraint-rows*)
;;;
;;; use-subsumption-by-false = nil	don't use subsumption
;;; use-subsumption-by-false = :false   use only forward subsumption on other false rows
;;; use-subsumption-by-false = :forward	use just forward subsumption generally
;;; use-subsumption-by-false = t	use forward and backward subsumption

;;; uses subsumption-mark, subsumption-matches fields of row
;;; uses containing-wffs property on term-memory records for literals

(defvar clause-subsumption t)

(defvar subsumption-mark)

(defun forward-subsumed (row)
  (prog->
    (forward-subsumption row ->* subsuming-row)
    (return-from forward-subsumed subsuming-row))
  nil)

(defun forward-subsumption (cc row)
  (when (row-hint-p row)
    (return-from forward-subsumption nil))		;no forward subsumption of hints
  (with-clock-on forward-subsumption
    (prog->
      (row-context-live? row ->nonnil row-context)
      (flet ((fsubsume (row2 test)
               (when (row-hint-p row2)
                 (return-from fsubsume nil))		;no forward subsumption by hints
               (prog->
                 (row-context-live? row2 ->nonnil row2-context)
		 (context-subsumes? row2-context row-context ->nonnil new-row-context)
                 (cond
                  ((eq t new-row-context)
                   (when (implies test (wff-subsumption nil row2 row))
                     (funcall cc row2)))
                  (t
                   (when (implies test (wff-subsumption nil row2 row))
                     (setf (row-context row) (setf row-context new-row-context))))))))
        (prog->
          (row-wff row -> wff)
          (when (let ((u (use-subsumption-by-false?))) (if (eq :false u) (eq false wff) u))
            (prog->
              (map-rows :rowset *false-rows* :reverse t ->* row2)
              (fsubsume row2 t))
            (prog->
              (map-rows :rowset *constraint-rows* :reverse t ->* row2)
              (fsubsume row2 t)))
          (cond
           ((eq false wff)
            )
           ((and clause-subsumption (or (clause-p wff) (setf clause-subsumption nil)))
            (forward-clause-subsumption row wff ->* row2)
            (fsubsume row2 nil))
           (t
            (forward-or-backward-wff-subsumption wff :pos :only nil (incf subsumption-mark) nil row ->* row2)
            (fsubsume row2 nil))))))))

(defun backward-subsumption (cc row)
  (when (row-hint-p row)
    (return-from backward-subsumption nil))		;no backward subsumption by hints
  (with-clock-on backward-subsumption
    (prog->
      (row-context-live? row ->nonnil row-context)
      (flet ((bsubsume (row2 test)
               (prog->
                 (row-context-live? row2 ->nonnil row2-context)
		 (context-subsumes? row-context row2-context ->nonnil new-row2-context)
                 (cond
                  ((eq t new-row2-context)
                   (when (implies test (wff-subsumption nil row row2))
                     (cond
                      ((row-hint-p row2)
                       (pushnew row2 *hints-subsumed*))	;row2 is a hint backward subsumed by row
                      (t
                       (funcall cc row2)))))
                  ((row-hint-p row2)
                   )
                  (t
                   (when (implies test (wff-subsumption nil row row2))
                     (setf (row-context row2) new-row2-context)))))))
        (prog->
          (row-wff row -> wff)
          (cond
           ((eq false wff)
            (when (let ((u (use-subsumption-by-false?))) (and u (neq :forward u) (neq :false u)))
              (map-rows :reverse t ->* row2)
              (bsubsume row2 t)))
           ((and clause-subsumption (or (clause-p wff) (setf clause-subsumption nil)))
            (backward-clause-subsumption row wff ->* row2)
            (bsubsume row2 nil))
           (t
            (forward-or-backward-wff-subsumption wff :pos :only nil (incf subsumption-mark) t row ->* row2)
            (bsubsume row2 nil))))))))

(defun forward-clause-subsumption (cc row2 clause)
  ;; applies fun to each stored clause that subsumes this clause
  ;; candidate subsuming clauses are the union of sets of clauses
  ;; that have generalizations of literals in this clause
  (do ((w (atoms-in-wff2 clause) (rest w))
       (candidate-subsuming-clauses nil)
       (mark (incf subsumption-mark))
       atom atompolarity)
      ((null w)
       (dolist (row candidate-subsuming-clauses)	;candidate subsuming clauses contains no duplicates
	 (when (and (let ((n 0))		;INEFFICIENT
		      (map-atoms-in-clause (lambda (atom polarity)
                                             (declare (ignore atom polarity))
                                             (incf n))
					   (row-wff row))
		      (length= n (row-subsumption-matches row)))
                    (if (use-dp-subsumption?)
                        (dp-subsume+ row row2)
		        (clause-subsumption row row2)))
	   (funcall cc row))))
    (setf atom (first (first w)))
    (setf atompolarity (second (first w)))
    (prog->
      (retrieve-generalization-entries
	atom
	nil
	(ecase atompolarity
	  (:pos
	    #'tme-rows-containing-atom-positively)
	  (:neg
	    #'tme-rows-containing-atom-negatively))
	->* atom2-entry rows)
      (tme-term atom2-entry -> atom2)
      (map-rows :rowset rows :reverse t ->* row)
      (cond
	((eql (row-subsumption-mark row) mark)
	 (let ((v (assoc atom2 (row-subsumption-matches row))))
	   (cond
	     (v
	      (push atom (cdr v)))
	     (t
	      (push (list atom2 atom) (row-subsumption-matches row))))))
	(t
	 (setf (row-subsumption-mark row) mark)
	 (setf (row-subsumption-matches row) (list (list atom2 atom)))
	 (push row candidate-subsuming-clauses))))))

(defun backward-clause-subsumption (cc row2 clause)
  ;; applies fun to each stored clause that this clause subsumes
  ;; candidate subsumed clauses are the intersection of sets of clauses
  ;; that have instances of literals in this clause
  (do ((w (atoms-in-wff2 clause) (rest w))
       (candidate-subsumed-clauses nil)		;might have duplicates
       (old-mark none new-mark)		;used to identify wffs involved in this subsumption operation
       (new-mark (incf subsumption-mark) (incf subsumption-mark))
       (first-atom t nil)
       atom atompolarity)
      ((null w)
       (dolist (row candidate-subsumed-clauses)	;candidate-subsumed-clauses may contain duplicates
	 (when (row-subsumption-mark row)
	   (setf (row-subsumption-mark row) nil)	;don't process duplicates
	   (when (if (use-dp-subsumption?)
                     (dp-subsume+ row2 row)
                     (clause-subsumption row2 row))
	     (funcall cc row)))))
    (setf atom (first (first w)))
    (setf atompolarity (second (first w)))
    (prog->
      (retrieve-instance-entries
	atom
	nil
	(ecase atompolarity
	  (:pos
	    #'tme-rows-containing-atom-positively)
	  (:neg
	    #'tme-rows-containing-atom-negatively))
	->* atom2-entry rows)
      (tme-term atom2-entry -> atom2)
      (map-rows :rowset rows :reverse t ->* row)
      (cond
	((eql (row-subsumption-mark row) old-mark)	;matches exist for all preceding literals, but not this one yet
	 (setf (row-subsumption-mark row) new-mark)
	 (push (list atom atom2) (row-subsumption-matches row))
	 (when (null (rest w))			;this is last atom to be matched
	   (push row candidate-subsumed-clauses)))
	((eql (row-subsumption-mark row) new-mark)	;matches exist for all preceding literals and this one
	 (push atom2 (cdr (assoc atom (row-subsumption-matches row))))
	 (when (null (rest w))			;this is last atom to be matched
	   (push row candidate-subsumed-clauses)))
	(first-atom
	 (setf (row-subsumption-mark row) new-mark)
	 (setf (row-subsumption-matches row) (list (list atom atom2)))
	 (when (null (rest w))			;this is last atom to be matched
	   (push row candidate-subsumed-clauses)))))))

(defun clause-subsumption (subsuming-row subsumed-row)
  (when (wff-symbol-counts-not-greaterp (row-wff-symbol-counts subsuming-row) (row-wff-symbol-counts subsumed-row))
    (catch 'subsumed
      (prog->
        (atoms-in-clause2 (row-wff subsuming-row) -> l1)
        (atoms-in-clause2 (row-wff subsumed-row) -> l2)
        (row-constraints subsuming-row -> subsuming-constraint-alist)
        (row-constraints subsumed-row  -> subsumed-constraint-alist)
        (row-answer subsuming-row -> subsuming-answer)
        (row-answer subsumed-row -> subsumed-answer)
        (quote t -> *subsuming*)
        (row-variables subsumed-row *frozen-variables* -> *frozen-variables*)
        (clause-subsumption1 l1 l2 subsuming-answer subsumed-answer ->* subst)
        (cond
         #+ignore
         ((use-constraint-solver-in-subsumption?)
          (when (eq false
                    (funcall (constraint-simplification-function?)
                             (conjoin subsuming-constraint (negate subsumed-constraint subst) subst)))
            (throw 'subsumed t)))
         (t
          (dp-subsume-constraint-alists* subsuming-constraint-alist subsumed-constraint-alist subst ->* subst)
          (declare (ignore subst))
          (throw 'subsumed t))))
      nil)))

(defun clause-subsumption1 (cc l1 l2 subsuming-answer subsumed-answer)
  (prog->
    (cond
     ((eq false subsuming-answer)
      (clause-subsumes1 l1 l2 *frozen-variables* ->* subst)
      (funcall cc subst))
     ((eq false subsumed-answer)
      )
     ((and (test-option37?) (clause-p subsuming-answer) (clause-p subsumed-answer))
      (atoms-in-clause2 subsuming-answer -> ans1)
      (atoms-in-clause2 subsumed-answer -> ans2)
      (cl:assert (disjoint-answer-relations-p l1 l2 ans1 ans2))
      (clause-subsumes1 (append ans1 l1) (append ans2 l2) *frozen-variables* ->* subst)
      (funcall cc subst))
     (t
      (clause-subsumes1 l1 l2 *frozen-variables* ->* subst)
      (subsume-answers subsuming-answer subsumed-answer subst ->* subst)
      (funcall cc subst)))))

(defun disjoint-answer-relations-p (l1 l2 ans1 ans2)
  (and (notany (lambda (x)
                 (or (member (head-or-term (car x)) l2 :key (lambda (y) (head-or-term (car y))))
                     (member (head-or-term (car x)) l1 :key (lambda (y) (head-or-term (car y))))))
               ans1)
       (notany (lambda (x)
                 (or (member (head-or-term (car x)) l2 :key (lambda (y) (head-or-term (car y))))
                     (member (head-or-term (car x)) l1 :key (lambda (y) (head-or-term (car y))))))
               ans2)))

(defun forward-or-backward-wff-subsumption (cc subwff polarity phase old-mark new-mark backward-p row)
  (dereference
    subwff nil
    :if-variable (error "Can't use variable wff in subsumption.")
    :if-constant (cond
		   ((or (eq true subwff) (eq false subwff))
		    (error "Can't use truth values in subsumption."))
		   (t
		    (forward-or-backward-atom-subsumption cc subwff polarity phase old-mark new-mark backward-p row)))
    :if-compound (let* ((head (head subwff))
			(kind (function-logical-symbol-p head))
			(args (args subwff)))
		   (when (and kind (null args))
		     (error "Can't use connectives with no arguments in subsumption."))
		   (ecase kind
		     (not
		       (forward-or-backward-wff-subsumption
			 cc (first args) (opposite-polarity polarity) phase old-mark new-mark backward-p row))
		     ((and or)
		      (cond
			((if backward-p (eq 'or kind) (eq 'and kind))
			 (do ((args args (rest args))
			      (first t nil)
			      (m old-mark)
			      n)
			     ((null (rest args))
			      (forward-or-backward-wff-subsumption
				cc (first args) polarity
				(ecase phase
				  (:only (if first :only :last))
				  (:first (if first :first :middle))
				  (:middle :middle)
				  (:last :last))
				m new-mark
				backward-p row))
			   (setf n (incf subsumption-mark))
			   (forward-or-backward-wff-subsumption
			     cc (first args) polarity
			     (ecase phase
			       (:only (if first :first :middle))
			       (:first (if first :first :middle))
			       (:middle :middle)
			       (:last :middle))
			     m n
			     backward-p row)
			   (setf m n)))
			(t
			 (do ((args args (rest args)))
			     ((null args))
			   (forward-or-backward-wff-subsumption
			     cc
			     (first args) polarity phase old-mark new-mark
			     backward-p row)))))
		     (implies
		       (forward-or-backward-wff-subsumption
			 cc
			 (make-compound *or*
					   (make-compound *not* (first args))
					   (second args))
			 polarity phase old-mark new-mark
			 backward-p row))
		     (implied-by
		       (forward-or-backward-wff-subsumption
			 cc
			 (make-compound *or*
					   (make-compound *not* (second args))
					   (first args))
			 polarity phase old-mark new-mark
			 backward-p row))
		     ((iff xor)			;should be more efficient
		      (cond
			((null (rest args))
			 (forward-or-backward-wff-subsumption
			   cc (first args) polarity phase old-mark new-mark backward-p row))
			(t
			 (let ((x (first args))
			       (y (if (null (cddr args)) (second args) (make-compound head (rest args)))))
			   (forward-or-backward-wff-subsumption
			     cc
			     (if (eq 'iff kind)
				 (make-compound *or*
						   (make-compound *and*
								     x
								     y)
						   (make-compound *and*
								     (make-compound *not* x)
								     (make-compound *not* y)))
				 (make-compound *or*
						   (make-compound *and*
								     x
								     (make-compound *not* y))
						   (make-compound *and*
								     (make-compound *not* x)
								     y)))
			     polarity phase old-mark new-mark
			     backward-p row)))))
		     (if			;should be more efficient
		       (forward-or-backward-wff-subsumption
			 cc
			 (make-compound *and*
					   (make-compound *or*
							     (make-compound *not* (first args))
							     (second args))
					   (make-compound *and*
							     (first args)
							     (third args)))
			 polarity phase old-mark new-mark
			 backward-p row))
		     ((nil)
		      (forward-or-backward-atom-subsumption
			cc subwff polarity phase old-mark new-mark backward-p row))))))

(defun forward-or-backward-atom-subsumption (cc atom polarity phase old-mark new-mark backward-p row)
  (funcall (if backward-p #'retrieve-instance-entries #'retrieve-generalization-entries)
	   (lambda (e row2s)
             (declare (ignore e))
             (prog->
               (map-rows :rowset row2s ->* row2)
               (ecase phase
                 (:only
                  (when (if backward-p
                            (if (use-dp-subsumption?)
                                (dp-subsume+ row row2)
                                (wff-subsumption nil row row2))
                            (if (use-dp-subsumption?)
                                (dp-subsume+ row2 row)
                                (wff-subsumption nil row2 row)))
                    (funcall cc row2)))
                 (:first
                  (setf (row-subsumption-mark row2) new-mark))
                 (:middle
                  (when (eql (row-subsumption-mark row2) old-mark)
                    (setf (row-subsumption-mark row2) new-mark)))
                 (:last
                  (when (eql (row-subsumption-mark row2) old-mark)
                    (when (if backward-p
                              (if (use-dp-subsumption?)
                                  (dp-subsume+ row row2)
                                  (wff-subsumption nil row row2))
                              (if (use-dp-subsumption?)
                                  (dp-subsume+ row2 row)
                                  (wff-subsumption nil row2 row)))
                      (funcall cc row2)))))))
	   atom
	   nil
	   (if (eq polarity :pos)
	       #'tme-rows-containing-atom-positively
	       #'tme-rows-containing-atom-negatively)))

(defun wff-subsumption (matches subsuming-row subsumed-row)
  (declare (ignore matches))
  (catch 'subsumed
    (prog->
      (row-wff subsuming-row -> subsuming-wff)
      (row-wff subsumed-row  -> subsumed-wff)
      (row-constraints subsuming-row -> subsuming-constraint-alist)
      (row-constraints subsumed-row  -> subsumed-constraint-alist)
      (row-answer subsuming-row -> subsuming-answer)
      (row-answer subsumed-row -> subsumed-answer)

      (quote t -> *subsuming*)
      (row-variables subsumed-row *frozen-variables* -> *frozen-variables*)

      (quote nil -> subst)
      (wff-subsumption* subsuming-wff subsumed-wff subst ->* subst)
      (subsume-answers subsuming-answer subsumed-answer subst ->* subst)
      (cond
       #+ignore
	((use-constraint-solver-in-subsumption?)
	 (when (eq false
		   (funcall (constraint-simplification-function?)
			    (conjoin subsuming-constraint (negate subsumed-constraint subst) subst)))
	   (throw 'subsumed t)))
	(t
	 (dp-subsume-constraint-alists* subsuming-constraint-alist subsumed-constraint-alist subst ->* subst)
;;       (wff-subsumption* subsuming-wff subsumed-wff subst ->* subst)
	 (declare (ignore subst))
	 (throw 'subsumed t))))))

(defun wff-subsumption* (cc subsuming-wff subsumed-wff subst)
  ;; assume variables of subsumed-wff are already frozen so that
  ;; unification really does subsumption
  (let (interpretations)
    ;; find every interpretation in which subsuming-wff is true and subsumed-wff is false
    #|
    (salsify t subsuming-wff nil
	     (lambda (interp1)
		 (salsify nil subsumed-wff interp1
			  (lambda (interp2)
			      (push (cons interp1 (ldiff interp2 interp1)) interpretations)))))
    |#
    (let (u v)
      (salsify t subsuming-wff nil (lambda (interp1) (push interp1 u)))
      (salsify nil subsumed-wff nil (lambda (interp2) (push interp2 v)))
      (dolist (interp1 u)
	(dolist (interp2 v)
	  (push (cons interp1 interp2) interpretations))))
    (let (w)
      (dolist (interp interpretations)
	(let ((n (nmatches interp subst)))
	  (when (eql 0 n)
	    (return-from wff-subsumption* nil))
	  (push (cons n interp) w)))
      (setf w (sort w #'< :key #'car))
      (setf interpretations nil)
      (dolist (x w)
	(push (cdr x) interpretations)))
    (wff-subsumption*1 cc interpretations subst)))

(defun wff-subsumption*1 (cc interpretations subst)
  (cond
   ((null interpretations)
    (funcall cc subst))
   (t
    (dolist (x (car (first interpretations)))
      (dolist (y (cdr (first interpretations)))
        (unless (eq (cdr x) (cdr y))
          (when (equal-p (car x) (car y) subst)
            (wff-subsumption*1 cc (rest interpretations) subst)
            (return-from wff-subsumption*1 nil)))))
    (dolist (x (car (first interpretations)))
      (dolist (y (cdr (first interpretations)))
        (unless (eq (cdr x) (cdr y))
          (prog->
            (unify (car x) (car y) subst ->* subst)
            (wff-subsumption*1 cc (rest interpretations) subst))))))))

(defun nmatches (interpretation subst)
  (let ((n 0))
    (dolist (x (car interpretation))
      (dolist (y (cdr interpretation))
        (unless (eq (cdr x) (cdr y))
	  (when (unify-p (car x) (car y) subst)
	    (incf n)))))
    n))

(defun subsume-answers (cc subsuming-answer subsumed-answer subst)
  (cond
   ((eq false subsuming-answer)
    (funcall cc subst))
   ((eq false subsumed-answer)
    )
   ((and (literal-p subsuming-answer) (literal-p subsumed-answer))
    (unify cc subsuming-answer subsumed-answer subst))
   ((and (clause-p subsuming-answer) (clause-p subsumed-answer))
    (prog->
      (instantiate subsuming-answer subst -> subsuming-answer)
      (atoms-in-clause2 subsuming-answer -> l1)
      (atoms-in-clause2 subsumed-answer -> l2)
      (clause-subsumes1 cc l1 l2 *frozen-variables*)))
   (t
    (wff-subsumption* cc subsuming-answer subsumed-answer subst))))

;;; wff-subsumption* allows wffs to subsume their own factors

;;; when subsuming one atom in an interpretation by
;;; another, make sure one is from the subsuming wff
;;; and the other is from the subsumed wff???
;;; split these lists to do M*N comparisons
;;; instead of (M+N)*(M+N)

;;; subsume.lisp EOF
