;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: connectives.lisp
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

;;; wff = well-formed formula
;;; atom = atomic fomula

(defun not-wff-error (x &optional subst)
  (error "~A is not a formula." (term-to-lisp x subst)))

(defun not-clause-error (x &optional subst)
  (error "~A is not a clause." (term-to-lisp x subst)))

(defun head-is-logical-symbol (wff)
  (dereference
    wff nil
    :if-constant      nil
    :if-variable      (not-wff-error wff)
    :if-compound-cons (not-wff-error wff)
    :if-compound-appl (function-logical-symbol-p (heada wff))))

(defun negation-p (wff)
  (eq 'not (head-is-logical-symbol wff)))

(defun conjunction-p (wff)
  (eq 'and (head-is-logical-symbol wff)))

(defun disjunction-p (wff)
  (eq 'or (head-is-logical-symbol wff)))

(defun implication-p (wff)
  (eq 'implies (head-is-logical-symbol wff)))

(defun reverse-implication-p (wff)
  (eq 'implied-by (head-is-logical-symbol wff)))

(defun equivalence-p (wff)
  (eq 'iff (head-is-logical-symbol wff)))

(defun exclusive-or-p (wff)
  (eq 'xor (head-is-logical-symbol wff)))

(defun conditional-p (wff)
  (eq 'if (head-is-logical-symbol wff)))

(defun universal-quantification-p (wff)
  (eq 'forall (head-is-logical-symbol wff)))

(defun existential-quantification-p (wff)
  (eq 'exists (head-is-logical-symbol wff)))

(defun atom-p (wff)
  (not (head-is-logical-symbol wff)))

(defun literal-p (wff &optional (polarity :pos) strict)
  ;; returns (values atom polarity)
  ;; only atomic formulas and negated atomic formulas are strict literals
  ;; nonstrict literals can have nested negations
  (let ((v (head-is-logical-symbol wff)))
    (cond
     ((null v)
      (values wff polarity))
     ((eq 'not v)
      (let ((wff1 (arg1a wff)))
        (if strict
            (and (atom-p wff1) (values wff1 (opposite-polarity polarity)))
            (literal-p wff1 (opposite-polarity polarity)))))
     (t
      nil))))

(defun clause-p (wff &optional no-true-false strict neg)
  ;; only atomic formulas, negated atomic formulas, their disjunctions, and (optionally) true and false are strict clauses
  ;; nonstrict clauses are implications etc. interpretable as single clauses
  (labels
    ((clause-p (wff neg)
       (case (head-is-logical-symbol wff)
         ((nil)
          (implies no-true-false (not (or (eq true wff) (eq false wff)))))
         (not
          (if strict
              (atom-p (arg1a wff))
              (clause-p (arg1a wff) (not neg))))
         (and
          (and (not strict)
               neg
               (dolist (arg (argsa wff) t)
                 (unless (clause-p arg t)
                   (return nil)))))
         (or
          (and (not neg)
               (if strict
                   (dolist (arg (argsa wff) t)
                     (unless (literal-p arg :pos t)
                       (return nil)))
                   (dolist (arg (argsa wff) t)
                     (unless (clause-p arg nil)
                       (return nil))))))
         (implies
          (and (not strict)
               (not neg)
               (let ((args (argsa wff)))
                 (and (clause-p (first args) t)
                      (clause-p (second args) nil)))))
         (implied-by
          (and (not strict)
               (not neg)
               (let ((args (argsa wff)))
                 (and (clause-p (first args) nil)
                      (clause-p (second args) t))))))))
    (clause-p wff neg)))

(defun equality-relation-symbol-p (fn)
  (eq '= (function-boolean-valued-p fn)))

(defun equality-p (wff)
  (dereference
    wff nil
    :if-constant      nil
    :if-variable      (not-wff-error wff)
    :if-compound-cons (not-wff-error wff)
    :if-compound-appl (equality-relation-symbol-p (head wff))))

(defun positive-equality-wff-p (wff)
  ;; nothing but strictly positive occurrences of equalities
  (prog->
    (map-atoms-in-wff wff ->* atom polarity)
      (unless (and (eq :pos polarity) (equality-p atom))
	(return-from positive-equality-wff-p nil)))
  t)

(declare-snark-option eliminate-negations nil nil)
(declare-snark-option flatten-connectives t t)	;e.g., replace (and a (and b c)) by (and a b c)
(declare-snark-option ex-join-negation t t)	;e.g., replace (equiv a false) by (not a)

(defun conjoin* (wffs &optional subst)
  (ao-join* wffs subst *and* true))

(defun disjoin* (wffs &optional subst)
  (ao-join* wffs subst *or* false))

(defun conjoin (wff1 wff2 &optional subst)
  (cond
    ((or (eq wff1 wff2) (eq true wff1) (eq false wff2))
     wff2)
    ((or (eq false wff1) (eq true wff2))
     wff1)
    (t
     (ao-join* (list wff1 wff2) subst *and* true))))

(defun disjoin (wff1 wff2 &optional subst)
  (cond
    ((or (eq wff1 wff2) (eq false wff1) (eq true wff2))
     wff2)
    ((or (eq true wff1) (eq false wff2))
     wff1)
    (t
     (ao-join* (list wff1 wff2) subst *or* false))))

(defun ao-join* (wffs subst connective identity)
  ;; create conjunction or disjunction of wffs
  ;; handle true, false, equal and complementary wffs
  (do ((not-identity (if (eq true identity) false true))
       (wffs* nil) wffs*-last
       (poswffs* nil)
       (negwffs* nil)
       wff)
      ((null wffs)
       (cond
        ((null wffs*)
         identity)
        ((null (rest wffs*))
         (first wffs*))
        ((flatten-connectives?)
         (make-compound* connective wffs*))
        (t
         (make-compound2 connective wffs*))))
    (setf wff (pop wffs))
    (when subst
      (setf wff (instantiate wff subst)))
    (cond
     ((and (compound-p wff) (eq connective (head wff)))
      (setf wffs (if wffs (append (argsa wff) wffs) (argsa wff))))
     (t
      (mvlet (((values wff neg) (not-not-eliminate wff)))
        (if neg
            (cond
             ((and poswffs* (hts-member-p neg poswffs*))
              (return not-identity))
             ((hts-adjoin-p neg (or negwffs* (setf negwffs* (make-hash-term-set))))
              (collect wff wffs*)))
            (cond
             ((eq identity wff)
              )
             ((eq not-identity wff)
              (return not-identity))
             ((and negwffs* (hts-member-p wff negwffs*))
              (return not-identity))
             ((hts-adjoin-p wff (or poswffs* (setf poswffs* (make-hash-term-set))))
              (collect wff wffs*)))))))))

(defun not-not-eliminate (wff)
  (let ((neg nil) -wff)
    (loop
      (dereference
       wff nil
       :if-variable (return-from not-not-eliminate
                      (if neg (values -wff wff) wff))
       :if-constant (return-from not-not-eliminate
                      (cond
                       ((eq true wff)
                        (if neg false true))
                       ((eq false wff)
                        (if neg true false))
                       (t
                        (if neg (values -wff wff) wff))))
       :if-compound (cond
                     ((eq *not* (head wff))
                      (if neg (setf neg nil) (setf neg t -wff wff))
                      (setf wff (arg1a wff)))
                     (t
                      (return-from not-not-eliminate
                        (if neg (values -wff wff) wff))))))))

(defun make-equivalence* (wffs &optional subst)
  (ex-join* wffs subst *iff* true))

(defun make-exclusive-or* (wffs &optional subst)
  (ex-join* wffs subst *xor* false))

(defun make-equivalence (wff1 wff2 &optional subst)
  (cond
    ((eq wff1 wff2)
     true)
    ((eq true wff1)
     wff2)
    ((eq true wff2)
     wff1)
    (t
     (make-equivalence* (list wff1 wff2) subst))))

(defun make-exclusive-or (wff1 wff2 &optional subst)
  (cond
    ((eq wff1 wff2)
     false)
    ((eq false wff1)
     wff2)
    ((eq false wff2)
     wff1)
    (t
     (make-exclusive-or* (list wff1 wff2) subst))))

(defun ex-join* (wffs subst connective identity)
  ;; create equivalence or exclusive-or of wffs
  ;; handle true, false, equal and complementary wffs
  (let ((not-identity (if (eq true identity) false true))
	n n1 n2 negate)
    (setf n (length (setf wffs (argument-list-a1 connective wffs subst identity))))
    (setf n1 (length (setf wffs (remove not-identity wffs))))
    (setf negate (oddp (- n n1)))
    (setf n n1)
    (do ((wffs* nil) wff)
	((null wffs)
	 (cond
	   ((null wffs*)
	    (if negate not-identity identity))
	   (t
	    (when negate
	      (setf wffs* (if (ex-join-negation?)
			      (cons (negate (first wffs*)) (rest wffs*))
			      (cons not-identity wffs*))))
	    (cond
	      ((null (rest wffs*))
	       (first wffs*))
	      ((flatten-connectives?)
	       (make-compound* connective (nreverse wffs*)))
	      (t
	       (make-compound2 connective (nreverse wffs*)))))))
      (setf wff (pop wffs))
      (setf n1 (length (setf wffs (remove wff wffs :test (lambda (x y) (equal-p x y subst))))))
      (setf n2 (length (setf wffs (remove wff wffs :test (lambda (x y) (complement-p x y subst))))))
      (psetq n1 (- n n1)			;count of wff in wffs
	     n2 (- n1 n2)			;count of ~wff in wffs
	     n n2)				;length of new value of wffs
      (cond
       ((evenp n1)
        (when (oddp n2)
          (push wff wffs*)
          (setf negate (not negate))	;was wrong (setf negate t); fixed 2011-05-13
          ))
       ((evenp n2)
        (push wff wffs*))
       (t
        (setf negate (not negate))	;was wrong (setf negate t); fixed 2011-05-13
        )))))

(defun negate0 (wffs &optional subst)
  (declare (ignore subst))
  (cl:assert (eql 1 (length wffs)))
  (make-compound* *not* wffs))

(defun negate* (wffs &optional subst)
  (cl:assert (eql 1 (length wffs)))
  (negate (first wffs) subst))

(defun make-implication* (wffs &optional subst)
  (cl:assert (eql 2 (length wffs)))
  (make-implication (first wffs) (second wffs) subst))

(defun make-reverse-implication* (wffs &optional subst)
  (cl:assert (eql 2 (length wffs)))
  (make-reverse-implication (first wffs) (second wffs) subst))

(defun make-conditional* (wffs &optional subst)
  (cl:assert (eql 3 (length wffs)))
  (make-conditional (first wffs) (second wffs) (third wffs) subst))

(defun make-conditional-answer* (wffs &optional subst)
  (cl:assert (eql 3 (length wffs)))
  (make-conditional-answer (first wffs) (second wffs) (third wffs) subst))

(defun negate (wff &optional subst)
  (dereference
    wff subst
    :if-constant (cond
		   ((eq true wff)
		    false)
		   ((eq false wff)
		    true)
                   ((eliminate-negations?)
                    (proposition-complementer wff))
		   (t
		    (make-compound *not* wff)))
    :if-variable (not-wff-error wff)
    :if-compound-cons (not-wff-error wff)
    :if-compound-appl (let ((head (heada wff)))
		        (ecase (function-logical-symbol-p head)
		          ((nil)			;atomic
		           (cond
			    ((eliminate-negations?)
			     (make-compound* (relation-complementer head) (argsa wff)))
			    (t
			     (make-compound *not* wff))))
                          (not
		           (arg1a wff))
		          (and
		           (disjoin* (mapcar (lambda (arg)
                                               (negate arg subst))
                                             (argsa wff))
                                     subst))
		          (or
		           (conjoin* (mapcar (lambda (arg)
                                               (negate arg subst))
                                             (argsa wff))
                                     subst))
		          ((implies implied-by iff xor)
		           (make-compound *not* wff))
		          (if
		            (let ((args (argsa wff)))
			      (make-compound head
					     (first args)
					     (negate (second args) subst)
					     (negate (third args) subst))))))))

(defun relation-complementer (fn)
  ;; if complement has special properties
  ;; such as associativity, rewrites, etc.,
  ;; these must be declared explicitly by the user
  (or (function-complement fn)
      (setf (function-complement fn)
            (declare-relation (complement-name (function-name fn)) (function-arity fn)))))

(defun proposition-complementer (const)
  (or (constant-complement const)
      (setf (constant-complement const)
            (declare-proposition (complement-name (constant-name const))))))

(defun complement-name (nm &optional noninterned)
  (let* ((s (symbol-name nm))
         (~s (if (eql #\~ (char s 0))
                 (subseq s 1)
                 (to-string "~" s))))
    (if noninterned
        (make-symbol ~s)
        (intern ~s (symbol-package nm)))))

(defun make-implication (wff1 wff2 &optional subst)
  (cond
    ((eq true wff1)
     wff2)
    ((eq true wff2)
     wff2)
    ((eq false wff1)
     true)
    ((eq false wff2)
     (negate wff1 subst))
    ((equal-p wff1 wff2 subst)
     true)
    ((complement-p wff1 wff2 subst)
     wff2)
    ((and (compound-p wff2) (eq *implies* (head wff2)))
     (let ((args2 (argsa wff2)))
       (make-implication (conjoin wff1 (first args2) subst) (second args2) subst)))
    ((eliminate-negations?)
     (disjoin (negate wff1 subst) wff2 subst))
    (t
     (make-compound *implies* wff1 wff2))))

(defun make-reverse-implication (wff2 wff1 &optional subst)
  (cond
    ((eq true wff1)
     wff2)
    ((eq true wff2)
     wff2)
    ((eq false wff1)
     true)
    ((eq false wff2)
     (negate wff1 subst))
    ((equal-p wff1 wff2 subst)
     true)
    ((complement-p wff1 wff2 subst)
     wff2)
    ((and (compound-p wff2) (eq *implied-by* (head wff2)))
     (let ((args2 (argsa wff2)))
       (make-reverse-implication (first args2) (conjoin (second args2) wff1 subst) subst)))
    ((eliminate-negations?)
     (disjoin wff2 (negate wff1 subst) subst))
    (t
     (make-compound *implied-by* wff2 wff1))))

(defun make-conditional (wff1 wff2 wff3 &optional subst)
  (cond
    ((eq true wff1)
     wff2)
    ((eq false wff1)
     wff3)
    ((negation-p wff1)
     (make-conditional (arg1 wff1) wff3 wff2 subst))
    (t
;;   (setf wff2 (substitute true wff1 wff2 subst))
;;   (setf wff3 (substitute false wff1 wff3 subst))
     (setf wff2 (prog->
		  (map-atoms-in-wff-and-compose-result wff2 ->* atom polarity)
		  (declare (ignore polarity))
		  (if (equal-p wff1 atom subst) true atom)))
     (setf wff3 (prog->
		  (map-atoms-in-wff-and-compose-result wff3 ->* atom polarity)
		  (declare (ignore polarity))
		  (if (equal-p wff1 atom subst) false atom)))
     (cond
       ((eq true wff2)
	(disjoin wff1 wff3 subst))
       ((eq false wff2)
	(conjoin (negate wff1 subst) wff3 subst))
       ((eq true wff3)
	(disjoin (negate wff1 subst) wff2 subst))
       ((eq false wff3)
	(conjoin wff1 wff2 subst))
       ((equal-p wff2 wff3 subst)
	wff2)
       ((eliminate-negations?)
	(disjoin
	  (conjoin wff1 wff2 subst)
	  (conjoin (negate wff1 subst) wff3 subst)
	  subst))
       (t
	(make-compound *if* wff1 wff2 wff3))))))

(defun make-conditional-answer (wff1 wff2 wff3 &optional subst)
  (cond
   ((eq true wff1)
    wff2)
   ((eq false wff1)
    wff3)
   ((negation-p wff1)
    (make-conditional-answer (arg1 wff1) wff3 wff2 subst))
   ((equal-p wff2 wff3 subst)
    wff2)
   (t
    (make-compound *answer-if* wff1 wff2 wff3))))    

(defun make-equality0 (term1 term2 &optional (relation *=*))
  (make-compound relation term1 term2))

(defun make-equality (term1 term2 &optional subst (relation *=*))
  (cond
    ((equal-p term1 term2 subst)
     true)
    (t
     (make-compound relation term1 term2))))

(defun complement-p (wff1 wff2 &optional subst)
  (let ((appl nil) (neg nil))
    (loop
      (dereference
	wff1 nil
	:if-constant (return)
	:if-variable (not-wff-error wff1)
        :if-compound-cons (not-wff-error wff1)
	:if-compound-appl (if (eq *not* (heada wff1))
			      (setf neg (not neg) wff1 (arg1a wff1))
			      (return (setf appl t)))))
    (loop
      (dereference
	wff2 nil
	:if-constant (return (and neg (eql wff1 wff2)))
	:if-variable (not-wff-error wff2)
        :if-compound-cons (not-wff-error wff2)
	:if-compound-appl (if (eq *not* (heada wff2))
			      (setf neg (not neg) wff2 (arg1a wff2))
			      (return (and appl neg (equal-p wff1 wff2 subst))))))))

(defun equal-or-complement-p (wff1 wff2 &optional subst)
  (let ((appl nil) (neg nil))
    (loop
      (dereference
	wff1 nil
        :if-constant (return)
        :if-variable (not-wff-error wff1)
        :if-compound-cons (not-wff-error wff1)
	:if-compound-appl (if (eq *not* (heada wff1))
			      (setf neg (not neg) wff1 (arg1a wff1))
			      (return (setf appl t)))))
    (loop
      (dereference
	wff2 nil
	:if-constant (return (and (eql wff1 wff2) (if neg :complement :equal)))
	:if-variable (not-wff-error wff2)
        :if-compound-cons (not-wff-error wff2)
	:if-compound-appl (if (eq *not* (heada wff2))
			      (setf neg (not neg) wff2 (arg1a wff2))
			      (return (and appl (equal-p wff1 wff2 subst) (if neg :complement :equal))))))))

;;; connectives.lisp EOF
