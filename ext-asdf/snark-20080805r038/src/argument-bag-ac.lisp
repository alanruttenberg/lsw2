;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: argument-bag-ac.lisp
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

(defmacro inc-argument-count (compare-fun term counts inc not-found-form &optional cancel)
  (let ((count (gensym)) (v (gensym)))
    `(dolist (,v ,counts ,not-found-form)
       (let ((,count (tc-count ,v)))
	 (unless (eql 0 ,count)
	   (when ,(cond
		    ((member compare-fun '(equal-p))
		     `(,compare-fun ,term (tc-term ,v) subst))
		    (t
		     `(,compare-fun ,term (tc-term ,v))))
	     (setf (tc-count ,v) (+ ,count ,inc))
	     ,@(when cancel
		 `((unless ,cancel
		     (when (if (plusp ,count) (minusp ,inc) (plusp ,inc))
		       (setf ,cancel t)))))
	     (return)))))))

(defmacro count-argument (fn arg counts inc count-arguments-fun not-found-form &optional cancel)
  `(dereference
     ,arg subst
     :if-variable (inc-argument-count eq ,arg ,counts ,inc ,not-found-form ,cancel)
     :if-constant (inc-argument-count eql ,arg ,counts ,inc ,not-found-form ,cancel)
     :if-compound (cond
		    ((and ,fn (eq ,fn (head ,arg)))
		     ,(if cancel
			  `(if ,cancel
			       (setf ,counts (,count-arguments-fun ,fn (args ,arg) subst ,counts ,inc))
			       (multiple-value-setq (,counts ,cancel) (,count-arguments-fun ,fn (args ,arg) subst ,counts ,inc)))
			  `(setf ,counts (,count-arguments-fun ,fn (args ,arg) subst ,counts ,inc))))
		    (t
		     (inc-argument-count equal-p ,arg ,counts ,inc ,not-found-form ,cancel)))))

(defun count-arguments (fn args subst &optional counts (inc 1))
  ;; creates list of term and count pairs for argument list
  ;; term and count pair is represented as (term . count)
  ;; return 2nd value T if a cancellation occurs
  (let ((cancel nil))
    (dolist (arg args)
      (count-argument fn arg counts inc count-arguments (push (make-tc arg inc) counts) cancel))
    (if cancel
	(values counts t)
	counts)))

(defun recount-arguments (fn terms-and-counts subst)
  (let (new-terms-and-counts)
    (dolist (tc terms-and-counts)
      (let ((term (tc-term tc)) (count (tc-count tc)))
	(count-argument fn term new-terms-and-counts count count-arguments (push (make-tc term count) new-terms-and-counts))))
    new-terms-and-counts))

(defun term-size-difference (terms-and-counts subst &optional var0)
  (let ((n 0))
    (dolist (tc terms-and-counts)
      (let ((count (tc-count tc)))
	(unless (eql 0 count)
	  (let ((term (tc-term tc)))
	    (unless (and var0 (dereference term subst :if-variable (not (variable-frozen-p term))))
	      (incf n (* count (size term subst))))))))
    n))

;;; argument-bag-ac.lisp EOF
