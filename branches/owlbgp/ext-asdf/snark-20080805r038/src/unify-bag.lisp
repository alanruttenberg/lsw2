;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: unify-bag.lisp
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
;;; Portions created by the Initial Developer are Copyright (C) 1981-2008.
;;; All Rights Reserved.
;;;
;;; Contributor(s): Mark E. Stickel <stickel@ai.sri.com>.

(in-package :snark)

(defun submultisetp (x y &key test key)
  (cond
    ((null x)
     t)
    ((null y)
     nil)
    (t
     (setf y (copy-list y))
     (dolist (x1 x t)
       (cond
	 ((if test
	      (funcall test x1 (car y))
	      (eql x1 (car y)))
	  (setf y (cdr y)))
	 (t
	  (do ((l1 y l2)
	       (l2 (cdr y) (cdr l2)))
	      ((null l2) (return-from submultisetp nil))
	    (when (if key
		      (if test
			  (funcall test (funcall key x1) (funcall key (car l2)))
			  (eql (funcall key x1) (funcall key (car l2))))
		      (if test
			  (funcall test x1 (car l2))
			  (eql x1 (car l2))))
	      (rplacd l1 (cdr l2))
	      (return nil)))))))))

(defun multiset-equal (x y &key test key)
  (and (length= x y)
       (submultisetp x y :test test :key key)))

;;; special variables used by bag unification algorithm
;;; and linear Diophantine equation basis algorithm

(defvar maxx)
(defvar maxy)

(defmacro check-unify-bag-basis-size ()
  `(when (< (unify-bag-basis-size-limit?) (incf unify-bag-basis-size))
     (warn "Unify-bag basis size limit exceeded.  No unifiers returned.")
     (throw 'unify-bag-basis-quit
       nil)))

(defmacro a-coef (i)
  `(svref a-coef-array ,i))

(defmacro b-coef (j)
  `(svref b-coef-array ,j))

(defmacro x-term (i)
  `(svref x-term-array ,i))

(defmacro y-term (j)
  `(svref y-term-array ,j))

(defmacro x-bind (i)
  `(svref x-bind-array ,i))

(defmacro y-bind (j)
  `(svref y-bind-array ,j))

(defmacro xx-unify-p (i k)
  ;; x-term.i,x-term.k unifiability (i<k)
  `(aref xx-and-yy-unify-array ,i ,k))

(defmacro yy-unify-p (j k)
  ;; y-term.j,x-term.k unifiability (j<k)
  `(aref xx-and-yy-unify-array ,k ,j))

(defmacro xy-unify-p (i j)
  `(aref simple-solution ,i ,j))

(defmacro x-term-ground-p (i)
  `(svref x-term-ground-array ,i))

(defmacro y-term-ground-p (j)
  `(svref y-term-ground-array ,j))

(defun print-unify-bag-basis (nxcoefs nycoefs a-coef-array b-coef-array simple-solution complex-solutions)
  (let ((count 0))
    (terpri-comment)
    (format t "Coefficients:")
    (terpri-comment)
    (dotimes (i nxcoefs) (format t "~4d" (a-coef i)))
    (format t "   ")
    (dotimes (j nycoefs) (format t "~4d" (b-coef j)))
    (terpri-comment)
    (format t "Simple solutions:")
    (dotimes (i nxcoefs)
      (dotimes (j nycoefs)
	(when (consp (aref simple-solution i j))
	  (incf count)
          (terpri-comment)
	  (dotimes (k nxcoefs) (format t "~4D" (if (eql k i) (car (aref simple-solution i j)) 0)))
	  (format t "   ")
	  (dotimes (k nycoefs) (format t "~4D" (if (eql k j) (cdr (aref simple-solution i j)) 0))))))
    (terpri-comment)
    (format t "Complex solutions:")
    (dolist (soln complex-solutions)
      (incf count)
      (terpri-comment)
      (dotimes (i nxcoefs) (format t "~4d" (svref (car soln) i)))
      (format t "   ")
      (dotimes (j nycoefs) (format t "~4d" (svref (cdr soln) j))))
    (terpri-comment)
    (format t "~D solutions in all." count)
    (terpri)))

(defun unify-bag-basis (nxcoefs nycoefs a-coef-array b-coef-array x-term-array y-term-array identity subst
			&optional all-x-term-ground all-y-term-ground)
  ;; The basis function computes the basis of solutions to homogeneous linear
  ;; diophantine equations.  It is based on an algorithm by Gerard Huet "An
  ;; algorithm to generate the basis of solutions to homogeneous linear
  ;; diophantine equations" appearing in Information Processing Letters 7, 3
  ;; (April 1978).  It is translated from the Pascal program included in
  ;; IRIA-LABORIA technical report #274 with the same title.  The Huet
  ;; algorithm has been modified to eliminate generation of basis vectors
  ;; which are inconsistent with the term arguments.  In particular, values
  ;; associated with nonvariable terms will be only 0 or 1, and no vectors
  ;; will assign values to two terms known not to be unifiable.  The purpose
  ;; is to find a basis for all nonnegative integer solutions to the vector
  ;; sum equation A*X = B*Y where A and B are vectors of positive integers.
  ;;
  ;; nxcoefs = length of a-coef-array = length of x-term-array
  ;; nycoefs = length of b-coef-array = length of y-term-array
  ;; a-coef-array = array for A, accessed by a-coef macro
  ;; b-coef-array = array for B, accessed by b-coef macro
  ;; x-term-array = array for terms corresponding to coefficients in A, accessed by x-term macro
  ;; y-term-array = array for terms corresponding to coefficients in B, accessed by y-term macro
  ;;
  ;; After basis completes execution, the following variables will
  ;; have the specified values:
  ;;   simple-solution[i,j]  (0<=i<nxcoefs, 0<=j<nycoefs) is
  ;;     dotted pair such that (Xi . Yj) with all other Xs and Ys = 0 is
  ;;     a 2-variable solution to the equation.
  ;;     simple-solution also encodes xy-unify-p information:
  ;;     nil indicates x-term.i and y-term.j are not unifiable;
  ;;     t indicates x-term.i and y-term.j are unifiable (but there is
  ;;     still no simple basis solution).
  ;;   complex-solutions is list of pairs (xsol . ysol) for which
  ;;     xsol[i] (0<=i<nxcoefs) is Xi and
  ;;     ysol[j] (0<=j<nycoefs) is Yj for a solution with > 2 variables.
  ;;
  ;; Performance should be best when:
  ;;   maxb <= maxa.
  ;;   a1 >= a2 >= ... >= am.
  ;;   b1 <= b2 <= ... <= bn.
  (let ((simple-solution (make-array (list nxcoefs nycoefs)))	;x-term.i,y-term.j unifiability
	(x-term-ground-array (and (not all-x-term-ground) (make-array nxcoefs))) (new-all-x-term-ground t)
	(y-term-ground-array (and (not all-y-term-ground) (make-array nycoefs))) (new-all-y-term-ground t)
	(maxa 0) (maxb 0) (suma 0) (sumb 0)
	(complex-solutions nil))
    ;; recompute all-x-term-ground and all-y-term-ground in case formerly nonground terms are now ground
    (loop for i below nxcoefs
	  as coef = (a-coef i)
	  do (incf suma coef)
	     (when (> coef maxa)
	       (setf maxa coef))
	     (unless all-x-term-ground
	       (let ((ground (frozen-p (x-term i) subst)))
		 (setf (x-term-ground-p i) ground)
		 (unless ground
		   (setf new-all-x-term-ground nil)))))
    (loop for j below nycoefs
	  as coef = (b-coef j)
	  do (incf sumb coef)
	     (when (> coef maxb)
	       (setf maxb coef))
	     (unless all-y-term-ground
	       (let ((ground (frozen-p (y-term j) subst)))
		 (setf (y-term-ground-p j) ground)
		 (unless ground
		   (setf new-all-y-term-ground nil)))))
    (setf all-x-term-ground new-all-x-term-ground)
    (setf all-y-term-ground new-all-y-term-ground)
    (when (cond
	    (all-x-term-ground
	     (or all-y-term-ground (and (eq none identity) (or (< suma sumb) (< maxa maxb)))))
	    (all-y-term-ground
	     (and (eq none identity) (or (> suma sumb) (> maxa maxb))))
	    (t
	     nil))
      (throw 'unify-bag-basis-quit nil))
    (dotimes (i nxcoefs)			;initialize xy-unify-p
      (let* ((x-term.i (x-term i))
	     (x-term.i-ground (or all-x-term-ground (x-term-ground-p i))))
	(dotimes (j nycoefs)
	  (let ((y-term.j (y-term j)))
	    (setf (xy-unify-p i j) (cond
				     ((and x-term.i-ground (or all-y-term-ground (y-term-ground-p j)))
				      nil)
				     ((and (use-unify-bag-constant-abstraction?)
					   (not (unfrozen-variable-p x-term.i))
					   (not (unfrozen-variable-p y-term.j)))
				      nil)
				     ((and (embedding-variable-p x-term.i)
					   (embedding-variable-p y-term.j))
				      nil)
				     (t
				      (unify-p x-term.i y-term.j subst))))))))
    (dotimes (i nxcoefs)
      (unless (and (neq none identity) (not (or all-x-term-ground (x-term-ground-p i))) (unify-p (x-term i) identity subst))
	(dotimes (j nycoefs (throw 'unify-bag-basis-quit nil))
	  (when (xy-unify-p i j)
	    (return nil)))))
    (dotimes (j nycoefs)
      (unless (and (neq none identity) (not (or all-y-term-ground (y-term-ground-p j))) (unify-p (y-term j) identity subst))
	(dotimes (i nxcoefs (throw 'unify-bag-basis-quit nil))
	  (when (xy-unify-p i j)
	    (return nil)))))
    (let ((xx-and-yy-unify-array (let ((ncoefs (if (>= nxcoefs nycoefs) nxcoefs nycoefs)))
				   (make-array (list ncoefs ncoefs))))
	  (unify-bag-basis-size 0))
      (unless all-x-term-ground
	(dotimes (i (- nxcoefs 1))		;initialize xx-unify-p
	  (do* ((x-term.i (x-term i))
		(x-term.i-ground (x-term-ground-p i))
		(k (+ i 1) (+ k 1)))
	       ((eql k nxcoefs))
	    (let ((x-term.k (x-term k)))
	      (setf (xx-unify-p i k) (cond
				       ((and x-term.i-ground (x-term-ground-p k))
					nil)
				       ((and (use-unify-bag-constant-abstraction?)
					     (not (unfrozen-variable-p x-term.i))
					     (not (unfrozen-variable-p x-term.k)))
					nil)
				       (t
					(unify-p x-term.i x-term.k subst))))))))
      (unless all-y-term-ground
	(dotimes (j (- nycoefs 1))		;initialize yy-unify-p
	  (do* ((y-term.j (y-term j))
		(y-term.j-ground (y-term-ground-p j))
		(k (+ j 1) (+ k 1)))
	       ((eql k nycoefs))
	    (let ((y-term.k (y-term k)))
	      (setf (yy-unify-p j k) (cond
				       ((and y-term.j-ground (y-term-ground-p k))
					nil)
				       ((and (use-unify-bag-constant-abstraction?)
					     (not (unfrozen-variable-p y-term.j))
					     (not (unfrozen-variable-p y-term.k)))
					nil)
				       (t
					(unify-p y-term.j y-term.k subst))))))))
      (setf x-term-ground-array nil)		;done with x-term-ground-array
      (setf y-term-ground-array nil)		;and y-term-ground-array now
      (dotimes (i nxcoefs)			;store 2 variable solutions in simple-solution
	(cond
	  ((unfrozen-variable-p (x-term i))
	   (dotimes (j nycoefs)
	     (when (xy-unify-p i j)
	       (cond
		 ((unfrozen-variable-p (y-term j))
		  (check-unify-bag-basis-size)
		  (let ((k (lcm (a-coef i) (b-coef j))))
		    (setf (aref simple-solution i j) (cons (truncate k (a-coef i))
							   (truncate k (b-coef j))))))
		 ((eql 0 (mod (b-coef j) (a-coef i)))
		  (check-unify-bag-basis-size)
		  (setf (aref simple-solution i j) (cons (truncate (b-coef j) (a-coef i)) 1)))))))
	  (t
	   (dotimes (j nycoefs)
	     (when (xy-unify-p i j)
	       (cond
		 ((unfrozen-variable-p (y-term j))
		  (cond
		    ((eql 0 (mod (a-coef i) (b-coef j)))
		     (check-unify-bag-basis-size)
		     (setf (aref simple-solution i j) (cons 1 (truncate (a-coef i) (b-coef j)))))))
		 ((eql (a-coef i) (b-coef j))
		  (check-unify-bag-basis-size)
		  #+openmcl	;workaround for openmcl-1.1-pre-070722
		  (setf (aref simple-solution i j) (cons 1 1))
		  #-openmcl
		  (setf (aref simple-solution i j) '(1 . 1)))))))))
      (cond
	((and (<= maxa 1) (<= maxb 1))		;no complex solutions if all coefficients <= 1
	 )
	(t
	 (let (initial-maxsum
	       (maxx (make-array nxcoefs))
	       (maxy (make-array nycoefs))
	       (xsol (make-array nxcoefs))
	       (ysol (make-array nycoefs))
	       complex-solutions-tail)
	   (cond
	     (all-x-term-ground
	      (setf initial-maxsum suma)
	      (dotimes (i nxcoefs)
		(setf (svref maxx i) 1))
	      (dotimes (j nycoefs)
		(setf (svref maxy j) (if (unfrozen-variable-p (y-term j)) maxa 1))))
	     (all-y-term-ground
	      (setf initial-maxsum sumb)
	      (dotimes (j nycoefs)
		(setf (svref maxy j) 1))
	      (dotimes (i nxcoefs)
		(setf (svref maxx i) (if (unfrozen-variable-p (x-term i)) maxb 1))))
	     (t
	      (setf initial-maxsum 0)
	      (dotimes (i nxcoefs)
		(setf (svref maxx i) (if (unfrozen-variable-p (x-term i)) maxb 1)))
	      (dotimes (j nycoefs)
		(incf initial-maxsum
		      (* (setf (svref maxy j) (if (unfrozen-variable-p (y-term j)) maxa 1))
			 (b-coef j))))))
	   (labels
	     ((xloop (i sum maxsum)
		(let ((i+1 (+ i 1)))
		  (setf (svref xsol i) 0)
		  (cond
		    ((< i+1 nxcoefs)
		     (xloop i+1 sum maxsum))
		    ((plusp sum)
		     (yloop 0 sum)))
		  (let ((maxval (svref maxx i)))
		    (when (plusp maxval)
		      (let ((a-coef.i (a-coef i)))
			(incf sum a-coef.i)
			(when (<= sum maxsum)
			  (do ((val 1 (+ val 1))
			       (maxx maxx)
			       (maxy maxy)
			       (newmaxx nil)
			       (newmaxy nil))
			      ((> val maxval))
			    (setf (svref xsol i) val)
			    (when (eql 1 val)
			      (do ((k (+ i 1) (+ k 1)))
				  ((eql k nxcoefs))
				(when (or all-x-term-ground (not (xx-unify-p i k)))
				  (unless newmaxx
				    (setf maxx (copy-seq maxx))
				    (setf newmaxx t))
				  (setf (svref maxx k) 0)))
			      (dotimes (j nycoefs)
				(let ((maxy.j (svref maxy j)))
				  (when (and (plusp maxy.j)
					     (not (xy-unify-p i j)))
				    (decf maxsum (* (b-coef j) maxy.j))
				    (unless newmaxy
				      (setf maxy (copy-seq maxy))
				      (setf newmaxy t))
				    (setf (svref maxy j) 0)))))
			    (dotimes (j nycoefs)
			      (let ((simple-solution.i.j (aref simple-solution i j)))
				(when (consp simple-solution.i.j)
				  (when (eql val (car simple-solution.i.j))
				    (let ((maxy.j (svref maxy j))
					  (n (cdr simple-solution.i.j)))
				      (when (>= maxy.j n)
					(let ((n-1 (- n 1)))
					  (decf maxsum (* (b-coef j) (- maxy.j n-1)))
					  (unless newmaxy
					    (setf maxy (copy-seq maxy))
					    (setf newmaxy t))
					  (setf (svref maxy j) n-1))))))))
			    (cond
			      ((< i+1 nxcoefs)
			       (xloop i+1 sum maxsum))
			      (t
			       (yloop 0 sum)))
			    (incf sum a-coef.i)
			    (when (> sum maxsum)
			      (return nil)))))))))

	      (yloop (j sum)
		(let ((b-coef.j (b-coef j))
		      (maxval (svref maxy j))
		      (j+1 (+ j 1)))
		  (cond
		    ((eql j+1 nycoefs)
		     (let ((val (truncate sum b-coef.j)))
		       (when (and (<= val maxval)
				  (eql (* b-coef.j val) sum))
			 (setf (svref ysol j) val)
			 (filter))))
		    (t
		     (do ((val 0 (+ val 1))
			  (maxy maxy)
			  (newmaxy nil))
			 ((> val maxval))
		       (setf (svref ysol j) val)
		       (when (eql val 1)
			 (do ((k (+ j 1) (+ k 1)))
			     ((eql k nycoefs))
			   (when (or all-y-term-ground (not (yy-unify-p j k)))
			     (unless newmaxy
			       (setf maxy (copy-seq maxy))
			       (setf newmaxy t))
			     (setf (svref maxy k) 0))))
		       (yloop j+1 sum)
		       (decf sum b-coef.j)
		       (when (minusp sum)
			 (return nil)))))))

	      (filter nil
		;; eliminate solutions with only two variables
		;; and solutions that that are greater than a previous solution and are thus composable
		;; store the solution if it passes the tests
;;              (format t "~%" ) (dotimes (i nxcoefs) (format t "~4d" (svref xsol i)))
;;              (format t "   ") (dotimes (j nycoefs) (format t "~4d" (svref ysol j)))
		(cond
		  ((and
		     (loop for i from (+ 1 (loop for k below nxcoefs when (plusp (svref xsol k)) return k)) below nxcoefs
			   never (plusp (svref xsol i)))	;returns t if xsol has only one nonzero value
		     (loop for j from (+ 1 (loop for k below nycoefs when (plusp (svref ysol k)) return k)) below nycoefs
			   never (plusp (svref ysol j))))	;returns t if ysol has only one nonzero value
		   )
		  ((loop for v in complex-solutions	;returns t if new solution is greater than previous one
			 thereis (and
				   (loop with xsol1 = (car v)
					 for i below nxcoefs
					 always (>= (svref xsol i) (svref xsol1 i)))
				   (loop with ysol1 = (cdr v)
					 for j below nycoefs
					 always (>= (svref ysol j) (svref ysol1 j)))))
		   )
		  (t
		   (check-unify-bag-basis-size)
		   (setf complex-solutions-tail
			 (if complex-solutions-tail
			     (setf (cdr complex-solutions-tail)
				   (cons (cons (copy-seq xsol)
					       (copy-seq ysol))
					 nil))
			     (setf complex-solutions
				   (cons (cons (copy-seq xsol)
					       (copy-seq ysol))
					 nil))))))))

	     (xloop 0 0 initial-maxsum)))))
      (when (trace-unify-bag-basis?)
	(print-unify-bag-basis nxcoefs nycoefs a-coef-array b-coef-array simple-solution complex-solutions))
      (values simple-solution complex-solutions))))

(declare-snark-option use-subsume-bag t t)

(defun unify-bag (cc terms1 terms2 subst fn)
  (cond
    ((and (use-subsume-bag?) (frozen-p terms2 subst))
     (subsume-bag cc terms1 terms2 subst fn))
    ((and (use-subsume-bag?) (frozen-p terms1 subst))
     (subsume-bag cc terms2 terms1 subst fn))
    ((meter-unify-bag?)
     (let ((start-time (get-internal-run-time)))
       (unwind-protect
	   (let-options ((meter-unify-bag nil))		;only meter top-level calls
	     (unify-bag* cc fn terms1 terms2 subst))
	 (let ((elapsed-time (/ (- (get-internal-run-time) start-time)
                                (float internal-time-units-per-second))))
	   (when (implies (numberp (meter-unify-bag?)) (<= (meter-unify-bag?) elapsed-time))
	     (format t "~2&~,3F seconds to unify-bag ~S and ~S."
                     elapsed-time
	             (flatten-term (make-compound* fn terms1) subst)
	             (flatten-term (make-compound* fn terms2) subst)))))))
    (t
     (unify-bag* cc fn terms1 terms2 subst))))

(defun unify-bag* (cc fn terms1 terms2 subst)
  (let ((identity (let ((id (function-identity2 fn)))
                    (cond
                     ((neq none id)
                      id)
                     ((function-identity-p1 fn)
                      (make-compound fn))
                     (t
                      none))))
	(nxcoefs 0) (nycoefs 0)
	(x-term-is-ground nil) (y-term-is-ground nil)
	(all-x-term-ground t) (all-y-term-ground t)
	firsta firstb firstx firsty
	(terms-and-counts (count-arguments fn terms2 subst (count-arguments fn terms1 subst) -1)))
    (loop for tc in terms-and-counts
	  as count = (tc-count tc)
	  when (plusp count)
	    do (incf nxcoefs)
	       (unless firsta
		 (setf firsta count)
		 (setf firstx (tc-term tc)))
	       (when (or (not x-term-is-ground) all-x-term-ground)
		 (if (frozen-p (tc-term tc) subst)
		     (setf x-term-is-ground t)
		     (setf all-x-term-ground nil)))
	  else
	  when (minusp count)
	    do (incf nycoefs)
	       (unless firstb
		 (setf firstb (- count))
		 (setf firsty (tc-term tc)))
	       (when (or (not y-term-is-ground) all-y-term-ground)
		 (if (frozen-p (tc-term tc) subst)
		     (setf y-term-is-ground t)
		     (setf all-y-term-ground nil))))
    (cond
      ((and (eql 0 nxcoefs) (eql 0 nycoefs))
       (funcall cc subst))
      ((or (eql 0 nxcoefs) (eql 0 nycoefs))
       (unless (eq none identity)
	 (unify-identity cc terms-and-counts subst identity)))
      ((and (eql 1 nxcoefs) (eql 1 nycoefs))	;unify-identity is an unimplemented possibility too
       (cond
	 ((eql firsta firstb)
	  (unify cc firstx firsty subst))
	 ((eql 0 (rem firstb firsta))
	  (when (unfrozen-variable-p firstx)
	    (unify cc firstx (make-compound* fn (consn firsty nil (/ firstb firsta))) subst)))
	 ((eql 0 (rem firsta firstb))
	  (when (unfrozen-variable-p firsty)
	    (unify cc (make-compound* fn (consn firstx nil (/ firsta firstb))) firsty subst)))
	 (t
	  (when (and (unfrozen-variable-p firstx) (unfrozen-variable-p firsty))
	    (let ((n (lcm firsta firstb))
		  (newvar (make-variable (associative-function-argument-sort fn))))
	      (prog->
		(unify firstx (make-compound* fn (consn newvar nil (/ n firsta))) subst ->* subst)
		(unify cc firsty (make-compound* fn (consn newvar nil (/ n firstb))) subst)))))))
      ((and (eql 1 nxcoefs) (eql 1 firsta))		;unify-identity is an unimplemented possibility too
       (when (unfrozen-variable-p firstx)
	 (unify cc firstx
		     (make-compound* fn (loop for tc in terms-and-counts
					      as count = (tc-count tc)
					      when (minusp count)
						nconc (consn (tc-term tc) nil (- count))))
		     subst
		    )))
      ((and (eql 1 nycoefs) (eql 1 firstb))		;unify-identity is an unimplemented possibility too
       (when (unfrozen-variable-p firsty)
	 (unify cc (make-compound* fn (loop for tc in terms-and-counts
					      as count = (tc-count tc)
					      when (plusp count)
						nconc (consn (tc-term tc) nil count)))
		     firsty
		     subst
		    )))
      ((use-unify-bag-constant-abstraction?)
       (cam-unify-bag0 cc fn nxcoefs nycoefs terms-and-counts identity subst all-x-term-ground all-y-term-ground))
      (all-y-term-ground
       (loop for tc in terms-and-counts
	     do (setf (tc-count tc) (- (tc-count tc))))
       (unify-bag0 cc fn nycoefs nxcoefs terms-and-counts identity subst all-y-term-ground all-x-term-ground))
      (t
       (unify-bag0 cc fn nxcoefs nycoefs terms-and-counts identity subst all-x-term-ground all-y-term-ground)))))

(defun sort-terms-and-counts (terms-and-counts subst)
  ;; compounds < constants & frozen variables < unfrozen variables
  (stable-sort terms-and-counts
               (lambda (tc1 tc2)
                 (let ((x (tc-term tc1)) (y (tc-term tc2)))
                   (dereference
                    x subst
                    :if-variable (dereference y subst :if-variable (and (variable-frozen-p x)
                                                                        (not (variable-frozen-p y))))
                    :if-constant (dereference y subst :if-variable (not (variable-frozen-p y)))
                    :if-compound (dereference y subst :if-variable t :if-constant t))))))

(defun unify-bag0 (cc fn nxcoefs nycoefs terms-and-counts identity subst all-x-term-ground all-y-term-ground)
  (let ((a-coef-array (make-array nxcoefs))
        (b-coef-array (make-array nycoefs))
        (x-term-array (make-array nxcoefs))
        (y-term-array (make-array nycoefs)))
    (loop for tc in (sort-terms-and-counts	;initialize a-coef-array, x-term-array
		      (loop for x in terms-and-counts when (plusp (tc-count x)) collect x)
		      subst)
	  as i from 0
	  do (setf (a-coef i) (tc-count tc))
	     (setf (x-term i) (tc-term tc)))
    (loop for tc in (sort-terms-and-counts	;initialize b-coef-array, y-term-array
		      (loop for x in terms-and-counts when (minusp (tc-count x)) collect x)
		      subst)
	  as j from 0
	  do (setf (b-coef j) (- (tc-count tc)))
	     (setf (y-term j) (tc-term tc)))
    (catch 'unify-bag-basis-quit
      (mvlet (((:values simple-solution complex-solutions)
	       (unify-bag-basis nxcoefs nycoefs a-coef-array b-coef-array x-term-array y-term-array identity subst
			        all-x-term-ground all-y-term-ground)))
	(dotimes (i nxcoefs) (setf (a-coef i) nil))	;reuse a-coef-array as x-bind-array
	(dotimes (j nycoefs) (setf (b-coef j) nil))	;reuse b-coef-array as y-bind-array
	(unify-bag1 cc fn nxcoefs nycoefs a-coef-array b-coef-array x-term-array y-term-array subst identity
		    simple-solution complex-solutions)))))

(defmacro nosol3x (s)
  `(and (null (x-bind i))			;x-term unmatched, but no later simple-solution applies
	(or (eq none identity)
	    (not (unfrozen-variable-p (x-term i))))
	(loop for j1 from ,s below nycoefs
	      as simple-solution.i.j1 = (aref simple-solution i j1)
	      never (and (consp simple-solution.i.j1)
			 (or (and (null (y-bind j1)) (eql 1 (cdr simple-solution.i.j1)))
			     (unfrozen-variable-p (y-term j1)))))))

(defmacro nosol3y (s)
  `(and (null (y-bind j))			;y-term unmatched, but no later simple-solution applies
	(or (eq none identity)
	    (not (unfrozen-variable-p (y-term j))))
	(loop for i1 from ,s below nxcoefs
	      as simple-solution.i1.j = (aref simple-solution i1 j)
	      never (and (consp simple-solution.i1.j)
			 (or (and (null (x-bind i1)) (eql 1 (car simple-solution.i1.j)))
			     (unfrozen-variable-p (x-term i1)))))))

(defmacro unify-bag2* (x subst)
  `(if ,x
       (unify-bag2 ,x ,subst)
       (unless (or (loop for i below nxcoefs thereis (nosol3x 0))
		   (loop for j below nycoefs thereis (nosol3y 0)))
	 (unify-bag3 0 0 ,subst))))

(defun unify-bag1 (cc fn
		   nxcoefs nycoefs
		   x-bind-array y-bind-array
		   x-term-array y-term-array subst
		   identity simple-solution complex-solutions)
  (labels
    ((unify-bag2 (complex-solns subst)
       (let ((xsol (caar complex-solns))
	     (ysol (cdar complex-solns)))
	 (cond
	   ((and				;check that this solution can be added in 
	      (loop for i below nxcoefs
		    as xsol.i = (svref xsol i)
		    never (and (neql 0 xsol.i)
			       (or (neql 1 xsol.i) (x-bind i))
			       (not (unfrozen-variable-p (x-term i)))))
	      (loop for j below nycoefs
		    as ysol.j = (svref ysol j)
		    never (and (neql 0 ysol.j)
			       (or (neql 1 ysol.j) (y-bind j))
			       (not (unfrozen-variable-p (y-term j))))))
            (when (test-option8?)
              (unless (and (neq none identity)		; AC1 UNIFICATION SUPPORT?
			   (loop for i below nxcoefs
			         never (and (plusp (svref xsol i))
                                            (not (unfrozen-variable-p (x-term i)))))
			   (loop for j below nycoefs
			         never (and (plusp (svref ysol j))
                                            (not (unfrozen-variable-p (y-term j))))))
	        (unify-bag2* (cdr complex-solns) subst)))
            (let ((newvar (or (dotimes (j nycoefs)
				(when (and (eql 1 (svref ysol j))
                                           (not (unfrozen-variable-p (y-term j))))
				  (return (y-term j))))
                              (dotimes (i nxcoefs)
				(when (and (eql 1 (svref xsol i))
                                           (not (unfrozen-variable-p (x-term i))))
				  (return (x-term i))))
                              (make-variable (associative-function-argument-sort fn)))))
              (dotimes (i nxcoefs)
                (let ((xsol.i (svref xsol i)))
                  (unless (eql 0 xsol.i)
                    (setf (x-bind i) (consn newvar (x-bind i) xsol.i)))))
              (dotimes (j nycoefs)
                (let ((ysol.j (svref ysol j)))
                  (unless (eql 0 ysol.j)
                    (setf (y-bind j) (consn newvar (y-bind j) ysol.j)))))
              (unify-bag2* (cdr complex-solns) subst))
            (dotimes (i nxcoefs)
              (let ((xsol.i (svref xsol i)))
                (unless (eql 0 xsol.i)
                  (setf (x-bind i) (nthcdr xsol.i (x-bind i))))))
            (dotimes (j nycoefs)
              (let ((ysol.j (svref ysol j)))
                (unless (eql 0 ysol.j)
                  (setf (y-bind j) (nthcdr ysol.j (y-bind j))))))
            (unless (test-option8?)
	      (unless (and (neq none identity)		; AC1 UNIFICATION SUPPORT?
			   (loop for i below nxcoefs
			         never (and (plusp (svref xsol i))
                                            (not (unfrozen-variable-p (x-term i)))))
			   (loop for j below nycoefs
			         never (and (plusp (svref ysol j))
                                            (not (unfrozen-variable-p (y-term j))))))
	        (unify-bag2* (cdr complex-solns) subst)))
	    )
	   (t
	    (unify-bag2* (cdr complex-solns) subst)))))

     (unify-bag3* (i j+1 subst)
       (if (eql j+1 nycoefs)
	   (let ((i+1 (+ i 1)))
	     (if (eql i+1 nxcoefs)
		 (progn
		   (when (trace-unify-bag-bindings?)
                     (terpri-comment)
		     (format t "Unify-bag will try to unify")
		     (print-bindings x-term-array x-bind-array nxcoefs)
		     (print-bindings y-term-array y-bind-array nycoefs)
		     (terpri))
		   (bind-xterm 0 subst))		;start unifying terms and bindings
		 (unify-bag3 i+1 0 subst)))
	   (unify-bag3 i j+1 subst)))

     (unify-bag3 (i j subst)
       (let ((simple-solution.i.j (aref simple-solution i j))
	     (j+1 (+ j 1)))
	 (cond
	   ((consp simple-solution.i.j)
	    (let ((m (car simple-solution.i.j))
		  (n (cdr simple-solution.i.j))
                  (x-term.i (x-term i))
                  (y-term.j (y-term j))
		  (x-bind.i (x-bind i))
		  (y-bind.j (y-bind j)))
	      (cond
		((and (or (and (null x-bind.i) (eql 1 m))
			  (unfrozen-variable-p x-term.i))
		      (or (and (null y-bind.j) (eql 1 n))
			  (unfrozen-variable-p y-term.j)))
		 (unless (and (neq none identity)		;AC1 UNIFICATION SUPPORT
			      (unfrozen-variable-p x-term.i)
			      (unfrozen-variable-p y-term.j))
                   (when (or x-bind.i y-bind.j)
		     (unless (or (nosol3x j+1) (nosol3y (+ i 1)))
		       (unify-bag3* i j+1 subst))))
		 (cond
		   ((and (null x-bind.i) (eql 1 m)
			 (null y-bind.j) (eql 1 n)
			 (not (unfrozen-variable-p x-term.i))
			 (not (unfrozen-variable-p y-term.j))
			 (not (special-unify-p x-term.i subst))
			 (not (special-unify-p y-term.j subst)))
		    (setf (x-bind i) (cons x-term.i nil))
		    (setf (y-bind j) (cons y-term.j nil))
		    (prog->
		      (unify x-term.i y-term.j subst ->* subst)
		      (unify-bag3* i j+1 subst)))
		   (t
		    (let ((newvar (cond
				    ((not (unfrozen-variable-p y-term.j))
				     y-term.j)
				    ((not (unfrozen-variable-p x-term.i))
				     x-term.i)
				    (t
				     (make-variable (associative-function-argument-sort fn))))))
		      (setf (x-bind i) (consn newvar x-bind.i m))
		      (setf (y-bind j) (consn newvar y-bind.j n))
		      (unify-bag3* i j+1 subst))))
		 (setf (x-bind i) x-bind.i)
		 (setf (y-bind j) y-bind.j)
		 (unless (and (neq none identity)		;AC1 UNIFICATION SUPPORT
			      (unfrozen-variable-p x-term.i)
			      (unfrozen-variable-p y-term.j))
		   (unless (or x-bind.i y-bind.j)
		     (unless (or (nosol3x j+1) (nosol3y (+ i 1)))
		       (unify-bag3* i j+1 subst)))))
		(t
		 (unless (or (nosol3x j+1) (nosol3y (+ i 1)))
		   (unify-bag3* i j+1 subst))))))
	   (t
	    (unify-bag3* i j+1 subst)))))

     (bind-xterm (i subst)
       (prog->
	 (x-term i -> x-term.i)
	 (x-bind i -> x-bind.i)
	 (+ i 1 -> i+1)
	 (cond
	   ((eql i+1 nxcoefs)			;unify x-term and x-bind, then do (bind-yterm 0)
	    (cond
	      ((null x-bind.i)
	       (unify x-term.i identity subst ->* subst)
	       (bind-yterm 0 subst))
	      ((null (cdr x-bind.i))
	       (cond
		 ((eq x-term.i (car x-bind.i))
		  (bind-yterm 0 subst))
		 (t
		  (unify x-term.i (car x-bind.i) subst ->* subst)
		  (bind-yterm 0 subst))))
	      (t
	       (unify x-term.i (make-compound* fn x-bind.i) subst ->* subst)
	       (bind-yterm 0 subst))))
	   (t					;unify x-term and x-bind, then do (bind-xterm i+1)
	    (cond
	      ((null x-bind.i)
	       (unify x-term.i identity subst ->* subst)
	       (bind-xterm i+1 subst))
	      ((null (cdr x-bind.i))
	       (cond
		 ((eq x-term.i (car x-bind.i))
		  (bind-xterm i+1 subst))
		 (t
		  (unify x-term.i (car x-bind.i) subst ->* subst)
		  (bind-xterm i+1 subst))))
	      (t
	       (unify x-term.i (make-compound* fn x-bind.i) subst ->* subst)
	       (bind-xterm i+1 subst)))))))
     
     (bind-yterm (j subst)
       (prog->
	 (y-term j -> y-term.j)
	 (y-bind j -> y-bind.j)
	 (+ j 1 -> j+1)
	 (cond
	   ((eql j+1 nycoefs)			;unify y-term and y-bind, then do (funcall function)
	    (cond
	      ((null y-bind.j)
	       (unify cc y-term.j identity subst))
	      ((null (cdr y-bind.j))
	       (cond
		 ((eq y-term.j (car y-bind.j))
		  (funcall cc subst))
		 (t
		  (unify cc y-term.j (car y-bind.j) subst))))
	      (t
	       (unify cc y-term.j (make-compound* fn y-bind.j) subst))))
	   (t					;unify y-term and y-bind, then do (bind-yterm j+1)
	    (cond
	      ((null y-bind.j)
	       (unify y-term.j identity subst ->* subst)
	       (bind-yterm j+1 subst))
	      ((null (cdr y-bind.j))
	       (cond
		 ((eq y-term.j (car y-bind.j))
		  (bind-yterm j+1 subst))
		 (t
		  (unify y-term.j (car y-bind.j) subst ->* subst)
		  (bind-yterm j+1 subst))))
	      (t
	       (unify y-term.j (make-compound* fn y-bind.j) subst ->* subst)
	       (bind-yterm j+1 subst)))))))

     (print-bindings (term bind ncoefs)
       (dotimes (i ncoefs)
         (format t "~% ~S & ~S" (svref term i) (make-a1-compound* fn identity (svref bind i))))))

    (unify-bag2* complex-solutions subst)))

(defun unify-identity (cc terms-and-counts subst identity)
  (let ((x (first terms-and-counts))
	(y (rest terms-and-counts)))
    (cond
      ((eql 0 (tc-count x))
       (cond
	 ((null y)
	  (funcall cc subst))
	 (t
	  (unify-identity cc y subst identity))))
      (t
       (cond
	 ((null y)
	  (unify cc (tc-term x) identity subst))
	 (t
	  (prog->
	    (unify (tc-term x) identity subst ->* subst)
	    (unify-identity cc y subst identity))))))))

(defun cam-unify-bag0 (cc fn nxcoefs nycoefs terms-and-counts identity subst all-x-term-ground all-y-term-ground)
  ;;; nxcoefs >= 1, nycoefs >= 1
  (labels
    ((cam-unify-bag1 (nxcoefs nycoefs terms-and-counts subst differing-terms)
      (do ((l1 terms-and-counts (cdr l1)))
	  ((null (cdr l1))
	   (let ((x-has-var nil) (x-has-nonvar all-x-term-ground)
		 (y-has-var nil) (y-has-nonvar all-y-term-ground))
	     (dolist (tc terms-and-counts)
	       (let ((count (tc-count tc)))
		 (cond
		   ((and (not all-x-term-ground) (plusp count))
		    (unless (and x-has-var x-has-nonvar)
		      (let ((term (tc-term tc)))
			(dereference
			  term subst
			  :if-variable (if (variable-frozen-p term)
					   (setf x-has-nonvar t)
					   (setf x-has-var t))
			  :if-constant (setf x-has-nonvar t)
			  :if-compound (setf x-has-nonvar t)))))
		   ((and (not all-y-term-ground) (minusp count))
		    (unless (and y-has-var y-has-nonvar)
		      (let ((term (tc-term tc)))
			(dereference
			  term subst
			  :if-variable (if (variable-frozen-p term)
					   (setf y-has-nonvar t)
					   (setf y-has-var t))
			  :if-constant (setf y-has-nonvar t)
			  :if-compound (setf y-has-nonvar t))))))))
	     (unless (or (and x-has-nonvar (not y-has-var))
			 (and y-has-nonvar (not x-has-var)))
	       (unify-bag0 cc fn nxcoefs nycoefs terms-and-counts identity subst all-x-term-ground all-y-term-ground))))
	(let ((m (tc-count (car l1))))
	  (unless (eql 0 m)
	    (let ((x (tc-term (car l1))))
	      (when (dereference x subst :if-compound t)
		(do ((l2 (cdr l1) (cdr l2)))
		    ((null l2))
		  (let ((n (tc-count (car l2))))
		    (unless (eql 0 n)
		      (let ((y (tc-term (car l2))))
			(when (dereference y subst :if-compound t)
			  (unless (or (neq (head x) (head y))
				      (and all-y-term-ground (minusp m) (minusp n))
				      (and all-x-term-ground (plusp m) (plusp n))
				      (dolist (z differing-terms)
					(cond
					  ((eq x (car z))
					   (when (equal-p y (cdr z) subst)
					     (return t)))
					  ((eq x (cdr z))
					   (when (equal-p y (car z) subst)
					     (return t)))
					  ((equal-p x (car z) subst)
					   (when (equal-p y (cdr z) subst)
					     (return t)))
					  ((equal-p x (cdr z) subst)
					   (when (equal-p y (car z) subst)
					     (return t))))))
			    (cam-unify-bag1 nxcoefs nycoefs terms-and-counts subst (cons (cons x y) differing-terms))
			    (prog->
			      (unify x y subst ->* subst)
			      (nreverse (recount-arguments fn terms-and-counts subst) -> terms-and-counts)
			      (quote 0 -> nxcoefs)
			      (quote 0 -> nycoefs)
			      (quote nil -> firsta)
			      (quote nil -> firstb)
			      (quote nil -> firstx)
			      (quote nil -> firsty)
			      (dolist (tc terms-and-counts)
				(let ((count (tc-count tc)))
				  (cond
				    ((plusp count)
				     (incf nxcoefs)
				     (unless firsta
				       (setf firsta count)
				       (setf firstx (tc-term tc))))
				    ((minusp count)
				     (incf nycoefs)
				     (unless firstb
				       (setf firstb count)
				       (setf firsty (tc-term tc)))))))
			      (cond
				((and (eql 0 nxcoefs) (eql 0 nycoefs))
				 (funcall cc subst))
				((or (eql 0 nxcoefs) (eql 0 nycoefs))
				 (unless (eq none identity)
				   (unify-identity cc terms-and-counts subst identity)))
				((and (eql 1 nxcoefs) (eql 1 nycoefs) (eql firsta (- firstb)))
				 (unify cc firstx firsty subst))
				(t
				 (cam-unify-bag1 nxcoefs nycoefs terms-and-counts subst differing-terms))))
			    (return-from cam-unify-bag1 nil))))))))))))))
    (cam-unify-bag1 nxcoefs nycoefs terms-and-counts subst nil)))

;;; unify-bag.lisp EOF
