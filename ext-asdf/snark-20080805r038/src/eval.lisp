;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: eval.lisp
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

(defvar *polarity*)

(defun fifo (row)
  (declare (ignore row))
  (values 0 nil))

(defun lifo (row)
  (declare (ignore row))
  (values 0 t))

(defun row-depth (row)
  (if (row-embedding-p row)
      (row-depth (row-parent row))
      (wff-depth (row-wff row))))

(defun row-size (row)
  (if (row-embedding-p row)
      (row-size (row-parent row))
      (wff-size (row-wff row))))

(defun row-weight (row)
  (if (row-embedding-p row)
      (row-weight (row-parent row))
      (wff-weight (row-wff row))))

(defun row-size+depth (row)
  (if (row-embedding-p row)
      (row-size+depth (row-parent row))
      (wff-size+depth (row-wff row))))

(defun row-weight+depth (row)
  (if (row-embedding-p row)
      (row-weight+depth (row-parent row))
      (wff-weight+depth (row-wff row))))

(defun row-size+depth+level (row)
  (if (row-embedding-p row)
      (row-size+depth+level (row-parent row))
      (+ (wff-size+depth (row-wff row)) (row-level row))))

(defun row-weight+depth+level (row)
  (if (row-embedding-p row)
      (row-weight+depth+level (row-parent row))
      (+ (wff-weight+depth (row-wff row)) (row-level row))))

(defun row-wff&answer-weight+depth (row)
  (if (row-embedding-p row)
      (row-wff&answer-weight+depth (row-parent row))
      (+ (wff-weight+depth (row-wff row)) (wff-weight+depth (row-answer row)))))

(defun row-neg (row)
  (if (row-embedding-p row)
      (row-neg (row-parent row))
      (wff-neg (row-wff row))))

(defun row-neg-size+depth (row)
  (if (row-embedding-p row)
      (row-neg-size+depth (row-parent row))
      (list (wff-neg (row-wff row)) (wff-size+depth (row-wff row)))))

(defun row-answer-weight (row)
  (weight (row-answer row)))

(defun wff-depth (wff &optional subst &key (polarity :pos))
  (prog->
    (wff-size* wff subst polarity ->* atom subst)
    (depth atom subst)))

(defun wff-size (wff &optional subst &key (polarity :pos))
  (prog->
    (wff-size* wff subst polarity ->* atom subst)
    (size atom subst)))

(defun wff-weight (wff &optional subst &key (polarity :pos))
  (prog->
    (wff-size* wff subst polarity ->* atom subst)
    (weight atom subst)))

(defun wff-size+depth (wff &optional subst &key (polarity :pos))
  (prog->
    (wff-size* wff subst polarity ->* atom subst)
    (+ (size atom subst) (depth atom subst))))

(defun wff-weight+depth (wff &optional subst &key (polarity :pos))
  (prog->
    (wff-size* wff subst polarity ->* atom subst)
    (+ (weight atom subst) (depth atom subst))))

(defun wff-length (wff &optional subst &key (polarity :pos))
  (prog->
    (wff-size* wff subst polarity ->* atom subst)
    (declare (ignore atom subst))
    1))

(defun wff-size* (atom-size-fun wff subst *polarity*)
  (dereference
    wff subst
    :if-variable (funcall atom-size-fun wff subst)
    :if-constant (cond
	          ((eq true wff)
		   (if (eq :pos *polarity*) 1000000 0))
	          ((eq false wff)
		   (if (eq :pos *polarity*) 0 1000000))
	          (t
		   (funcall atom-size-fun wff subst)))
    :if-compound (let* ((head (head wff))
		    (kind (function-logical-symbol-p head))
		    (args (args wff)))
	       (ecase kind
		 (not
		   (wff-size* atom-size-fun (first args) subst (opposite-polarity *polarity*)))
		 ((and or)
		  (if (if (eq 'and kind)
			  (eq :pos *polarity*)
			  (eq :neg *polarity*))
		      (let ((n 1000000))
			(dolist (arg args)
			  (let ((m (wff-size* atom-size-fun arg subst *polarity*)))
			    (when (< m n)
			      (setf n m))))
			n)
		      (let ((n 0))
			(dolist (arg args)
			  (incf n (wff-size* atom-size-fun arg subst *polarity*)))
			n)))
		 (implies
		   (if (eq :pos *polarity*)
		       (+ (wff-size* atom-size-fun (first args) subst :neg)
			  (wff-size* atom-size-fun (second args) subst :pos))
		       (min (wff-size* atom-size-fun (first args) subst :pos)
			    (wff-size* atom-size-fun (second args) subst :neg))))
		 (implied-by
		   (if (eq :pos *polarity*)
		       (+ (wff-size* atom-size-fun (second args) subst :neg)
			  (wff-size* atom-size-fun (first args) subst :pos))
		       (min (wff-size* atom-size-fun (second args) subst :pos)
			    (wff-size* atom-size-fun (first args) subst :neg))))
		 ((iff xor)
		  (let ((y (if (null (cddr args))
			       (second args)
			       (make-compound head (rest args)))))
		    (if (if (eq 'iff kind)
			    (eq :pos *polarity*)
			    (eq :neg *polarity*))
			(min (+ (wff-size* atom-size-fun (first args) subst :pos)
				(wff-size* atom-size-fun y subst :neg))
			     (+ (wff-size* atom-size-fun (first args) subst :neg)
				(wff-size* atom-size-fun y subst :pos)))
			(min (+ (wff-size* atom-size-fun (first args) subst :pos)
				(wff-size* atom-size-fun y subst :pos))
			     (+ (wff-size* atom-size-fun (first args) subst :neg)
				(wff-size* atom-size-fun y subst :neg))))))
		 ((if answer-if)
		   (if (eq :pos *polarity*)
		       (min (+ (wff-size* atom-size-fun (first args) subst :neg)
			       (wff-size* atom-size-fun (second args) subst :pos))
			    (+ (wff-size* atom-size-fun (first args) subst :pos)
			       (wff-size* atom-size-fun (third args) subst :pos)))
		       (min (+ (wff-size* atom-size-fun (first args) subst :neg)
			       (wff-size* atom-size-fun (second args) subst :neg))
			    (+ (wff-size* atom-size-fun (first args) subst :pos)
			       (wff-size* atom-size-fun (third args) subst :neg)))))
		 ((nil)				;atomic
		  (funcall atom-size-fun wff subst))))))

(defun wff-neg (wff &optional subst)
  (dereference 
    wff subst
    :if-constant 1
    :if-variable 1
    :if-compound (case (function-logical-symbol-p (head wff))
		   ((not implies implied-by iff xor if) 
		    0)
		   ((and or)
		    (dolist (arg (args wff) 1)
		      (when (eql 0 (wff-neg arg subst))
			(return 0))))
		   (otherwise
		     1))))

(defun row-argument-count-limit-exceeded (row)
  (prog->
    (row-argument-count-limit? ->nonnil lim)
    (quote nil -> arguments)
    (map-terms-in-wff (row-wff row) ->* term polarity)
    (declare (ignore polarity))
    (cond
     ((member-p term arguments)
      )
     ((eql 0 lim)
      (return-from prog-> t))
     (t
      (decf lim)
      (push term arguments)))))

(defun row-weight-limit-exceeded (row)
  (let ((lim (row-weight-limit?)))
    (and lim
         (not (row-input-p row))
         (not (row-embedding-p row))
         (< lim (row-weight row)))))

(defun row-weight-before-simplification-limit-exceeded (row)
  (let ((lim (row-weight-before-simplification-limit?)))
    (and lim
         (not (row-input-p row))
         (not (row-embedding-p row))
         (< lim (row-weight row)))))

(defun row-proof-length-limit-exceeded (row lim)
  (cond
   ((member (row-reason row) '(assertion assumption negated_conjecture))
    nil)
   (t
    (let ((lim-1 (- lim 1))
          (row-numbers (make-sparse-vector :boolean t)))
      (labels
        ((row-proof-length-limit-exceeded* (row)
           (unless (or (member (row-reason row) '(assertion assumption negated_conjecture))
                       (sparef row-numbers (row-number row)))
             (cond
              ((= lim-1 (sparse-vector-count row-numbers))
               (return-from row-proof-length-limit-exceeded t))
              (t
               (setf (sparef row-numbers (row-number row)) t)
               (map-rows-in-reason #'row-proof-length-limit-exceeded* (row-reason row)))))))
        (map-rows-in-reason #'row-proof-length-limit-exceeded* (row-reason row)))))))

(defun maximum-and-minimum-clause-lengths (wff subst)
  ;; return maximum and minimum lengths of clauses in cnf expansion of wff
  (dereference
    wff subst
    :if-variable (values 1 1)
    :if-constant (values 1 1)			;special case for true and false?
    :if-compound (let* ((head (head wff))
			(kind (function-logical-symbol-p head)))
		   (ecase kind
		     (not
		       (maximum-and-minimum-clause-lengths-neg (arg1 wff) subst))
		     (and
		       (let ((max 0) (min 1000000))
			 (prog->
			   (dolist (args wff) ->* arg)
			   (maximum-and-minimum-clause-lengths arg subst -> max1 min1)
			   (setf max (max max max1))
			   (setf min (min min min1)))
			 (values max min)))
		     (or
		       (let ((max 0) (min 0))
			 (prog->
			   (dolist (args wff) ->* arg)
			   (maximum-and-minimum-clause-lengths arg subst -> max1 min1)
			   (setf max (+ max max1))
			   (setf min (+ min min1)))
			 (values max min)))
		     (implies
		       (prog->
                         (args wff -> args)
			 (maximum-and-minimum-clause-lengths-neg (first args) subst -> max1 min1)
			 (maximum-and-minimum-clause-lengths (second args) subst -> max2 min2)
			 (values (+ max1 max2) (+ min1 min2))))
		     (implied-by
		       (prog->
                         (args wff -> args)
			 (maximum-and-minimum-clause-lengths-neg (second args) subst -> max1 min1)
			 (maximum-and-minimum-clause-lengths (first args) subst -> max2 min2)
			 (values (+ max1 max2) (+ min1 min2))))
		     ((iff xor if answer-if)
		      (unimplemented))
		     ((nil)
		      (values 1 1))))))

(defun maximum-and-minimum-clause-lengths-neg (wff subst)
  ;; return maximum and minimum lengths of clauses in cnf expansion of wff
  (dereference
    wff subst
    :if-variable (values 1 1)
    :if-constant (values 1 1)			;special case for true and false?
    :if-compound (let* ((head (head wff))
			(kind (function-logical-symbol-p head)))
		   (ecase kind
		     (not
		       (maximum-and-minimum-clause-lengths (arg1 wff) subst))
		     (and
		       (let ((max 0) (min 0))
			 (prog->
			   (dolist (args wff) ->* arg)
			   (maximum-and-minimum-clause-lengths-neg arg subst -> max1 min1)
			   (setf max (+ max max1))
			   (setf min (+ min min1)))
			 (values max min)))
		     (or
		       (let ((max 0) (min 1000000))
			 (prog->
			   (dolist (args wff) ->* arg)
			   (maximum-and-minimum-clause-lengths-neg arg subst -> max1 min1)
			   (setf max (max max max1))
			   (setf min (min min min1)))
			 (values max min)))
		     (implies
		       (prog->
                         (args wff -> args)
			 (maximum-and-minimum-clause-lengths (first args) subst -> max1 min1)
			 (maximum-and-minimum-clause-lengths-neg (second args) subst -> max2 min2)
			 (values (max max1 max2) (min min1 min2))))
		     (implied-by
		       (prog->
                         (args wff -> args)
			 (maximum-and-minimum-clause-lengths (second args) subst -> max1 min1)
			 (maximum-and-minimum-clause-lengths-neg (first args) subst -> max2 min2)
			 (values (max max1 max2) (min min1 min2))))
		     ((iff xor if answer-if)
		      (unimplemented))
		     ((nil)
		      (values 1 1))))))

;;; eval.lisp EOF
