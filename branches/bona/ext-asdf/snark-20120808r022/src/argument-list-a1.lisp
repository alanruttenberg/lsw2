;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: argument-list-a1.lisp
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

(defun argument-list-a1 (fn args &optional subst (identity none))
  ;; return list of arguments of associative function fn
  ;; return undereferenced args if no flattening or identity elimination
  (if (null args)
      nil
      (labels
        ((argument-list-a1* (args)
           (let* ((l (rest args))
                  (l* (if (null l) nil (argument-list-a1* l)))
                  (arg (first args))
                  (arg* arg))
             (cond
              ((dereference arg* subst :if-compound-appl (eq fn (heada arg*)))
               (let* ((v (argsa arg*))
                      (v* (if (null v) nil (argument-list-a1* v))))
                 (cond
                  ((null l*)
                   v*)
                  ((null v*)
                   l*)
                  (t
                   (append v* l*)))))
              ((eql identity arg*)
               l*)
              ((eq l l*)
               args)
              (t
               (cons arg l*))))))
        (argument-list-a1* args))))

(defun argument-count-a1 (fn args &optional subst (identity none) dont-count-variables)
  (let ((c 0))
    (dolist (arg args)
      (dereference
       arg subst
       :if-compound-appl (if (eq fn (heada arg))
                             (incf c (argument-count-a1 fn (argsa arg) subst identity dont-count-variables))
                             (incf c))
       :if-compound-cons (incf c)
       :if-constant (unless (eql identity arg)
                      (incf c))
       :if-variable (unless (and dont-count-variables
                                 (neq none identity)
                                 (not (variable-frozen-p arg)))
                      (incf c))))
    c))

(defun similar-argument-list-ac1-p (fn args1 args2 &optional subst (identity none))
  ;; same number of variable, list, constant, and application arguments
  ;; also same number of first constant and first function seen
  (let ((nvari 0) (nconst 0) (nappl 0)
	(const1 none) (head1 none) nconst1 nhead1)
    (labels
      ((similar-argument-list-ac1-p1 (arg)
	 (dereference
	   arg subst
	   :if-variable (incf nvari)
	   :if-constant (unless (eql identity arg)
			  (cond
			    ((eq const1 none)
			     (setf const1 arg)
			     (setf nconst1 1))
			    ((eql arg const1)
			     (incf nconst1))
			    (t
			     (incf nconst))))
	   :if-compound (let ((head (head arg)))
			  (if (eq fn head)
			      (dolist (x (args arg))
				(similar-argument-list-ac1-p1 x))
			      (cond
				((eq head1 none)
				 (setf head1 head)
				 (setf nhead1 1))
				((eq head head1)
				 (incf nhead1))
				(t
				 (incf nappl)))))))
       (similar-argument-list-ac1-p2 (arg)
	 (dereference
	   arg subst
	   :if-variable (if (eql 0 nvari)
			    (return-from similar-argument-list-ac1-p nil)
			    (decf nvari))
	   :if-constant (unless (eql identity arg)
			  (cond
			    ((eq none const1)
			     (return-from similar-argument-list-ac1-p nil))
			    ((eql arg const1)
			     (if (eql 0 nconst1)
				 (return-from similar-argument-list-ac1-p nil)
				 (decf nconst1)))
			    (t
			     (if (eql 0 nconst)
				 (return-from similar-argument-list-ac1-p nil)
				 (decf nconst)))))
	   :if-compound (let ((head (head arg)))
			  (if (eq fn head)
			      (dolist (x (args arg))
				(similar-argument-list-ac1-p2 x))
			      (cond
				((eq none head1)
				 (return-from similar-argument-list-ac1-p nil))
				((eq head head1)
				 (if (eql 0 nhead1)
				     (return-from similar-argument-list-ac1-p nil)
				     (decf nhead1)))
				(t
				 (if (eql 0 nappl)
				     (return-from similar-argument-list-ac1-p nil)
				     (decf nappl)))))))))
      (dolist (x args1)
	(similar-argument-list-ac1-p1 x))
      (dolist (x args2)
	(similar-argument-list-ac1-p2 x))
      (and (eql 0 nvari) (eql 0 nconst) (eql 0 nappl)))))

(defun flatargs (term &optional subst)
  (let ((fn (head term)))
    (if (function-associative fn)
	(argument-list-a1 fn (argsa term) subst)
	(args term))))

;;; argument-list-a1.lisp EOF
