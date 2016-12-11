;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: weight.lisp
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

(defun depth (x &optional subst head-if-associative)
  (dereference
   x subst
   :if-constant 0
   :if-variable 0
   :if-compound-cons (+ 1 (max (depth (carc x) subst) (depth (cdrc x) subst)))
   :if-compound-appl (let ((head (heada x)))
                       (cond
                        ((eq head head-if-associative)
                         (loop for x1 in (argsa x) maximize (depth x1 subst head)))
                        ((function-associative head)
                         (+ 1 (loop for x1 in (argsa x) maximize (depth x1 subst head))))
                        (t
                         (+ 1 (loop for x1 in (argsa x) maximize (depth x1 subst))))))))

(defun mindepth (x &optional subst head-if-associative)
  (dereference
   x subst
   :if-constant 0
   :if-variable 0
   :if-compound-cons (+ 1 (min (mindepth (carc x) subst) (mindepth (cdrc x) subst)))
   :if-compound-appl (let ((head (heada x)))
                       (cond
                        ((eq head head-if-associative)
                         (loop for x1 in (argsa x) minimize (mindepth x1 subst head)))
                        ((function-associative head)
                         (+ 1 (loop for x1 in (argsa x) minimize (mindepth x1 subst head))))
                        (t
                         (+ 1 (loop for x1 in (argsa x) minimize (mindepth x1 subst))))))))

(definline constantly-one (x)
  (declare (ignore x))
  1)

(definline constantly-nil (x)
  (declare (ignore x))
  nil)

(definline variable-weight1 (variable)
  (let ((w (variable-weight?)))
    (if (numberp w) w (funcall w variable))))

(defmacro weight-macro (weight-fn constant-weight-fn variable-weight-fn function-weight-fn function-weight-code-fn)
  `(dereference
    x subst
    :if-constant (,constant-weight-fn x)
    :if-variable (,variable-weight-fn x)
    :if-compound-cons (+ (,weight-fn (carc x) subst) (,weight-fn (cdrc x) subst) (,function-weight-fn *cons*))
    :if-compound-appl (let ((head (heada x)))
                        (dolist (fun (,function-weight-code-fn head)
                                     (cond
                                      ((function-associative head)	;do something different for zero or one args?
                                       (let ((args (argsa x)))
		                         (+ (loop for x1 in args sum (,weight-fn x1 subst))
			                    (* (,function-weight-fn head) (+ 1 (length (rrest args)))))))
                                      (t
                                       (+ (loop for x1 in (argsa x) sum (,weight-fn x1 subst))
                                          (,function-weight-fn head)))))
                          (let ((v (funcall fun x subst)))
                            (unless (or (null v) (eq none v))
                              (return v)))))))

(defun weight (x &optional subst)
  (weight-macro
   weight
   constant-weight
   variable-weight1
   function-weight
   function-weight-code))

(defun size (x &optional subst)
  (weight-macro
   size
   constantly-one
   constantly-one
   constantly-one
   constantly-nil))

(defun weigh-first-two-arguments (x &optional subst)
  (dereference
   x subst
   :if-compound-appl (let ((args (argsa x)))
                       (and (rest args)
                            (+ (weight (first args) subst)
                               (weight (second args) subst)
                               (function-weight (heada x)))))))

(defun maximum-argument-weight (args subst head-if-associative)
  (loop for arg in args
	maximize (if (and head-if-associative
                          (dereference
                           arg subst
                           :if-compound-appl (eq head-if-associative (heada arg))))
		     (maximum-argument-weight (argsa arg) subst head-if-associative)
		     (weight arg subst))))

(defun weightm (x &optional subst)
  (dereference
   x subst
   :if-constant (weight x)
   :if-variable (weight x)
   :if-compound-cons (+ (max (weight (carc x) subst) (weight (cdrc x) subst)) (function-weight *cons*))
   :if-compound-appl (let ((head (heada x)))
                       (+ (maximum-argument-weight (argsa x) subst (and (function-associative head) head))
                          (function-weight head)))))

(defstruct (symbol-count
            (:type list)
            (:constructor make-symbol-count ())
            (:copier nil))
  (total 0 :type fixnum)
  (alist nil))

(defun symbol-count (x &optional subst scount)
  ;; computes the total number of symbols in x and
  ;; an alist for counts of constants and functions in x
  ;; count 2 f's for f(x,y,z)=f(f(x,y),z)=f(x,f(y,z))
  (macrolet
    ((symbol-count1 (symbol count)
       `(let* ((count ,count)
               (alist (symbol-count-alist (or scount (setf scount (make-symbol-count)))))
	       (v (assoc ,symbol alist)))
	  (if v
              (incf (cdr v) count)
              (setf (symbol-count-alist scount) (acons ,symbol count alist)))
	  (incf (symbol-count-total scount) count))))
    (dereference
     x subst
     :if-constant (symbol-count1 x 1)
     :if-compound-cons (progn
                         (symbol-count1 *cons* 1)
                         (symbol-count (carc x) subst scount)
                         (symbol-count (cdrc x) subst scount))
     :if-compound-appl (let ((head (heada x))
                             (args (argsa x)))
                         (symbol-count1 head (if (function-associative head)
                                                 (+ 1 (length (rrest args)))
                                                 1))
                         (dolist (x1 args)
                           (symbol-count x1 subst scount)))
     :if-variable (incf (symbol-count-total scount)))
    scount))

(definline symbol-count-not-greaterp1 (scount1 scount2)
  (let ((alist2 (symbol-count-alist scount2)))
    (dolist (v1 (symbol-count-alist scount1) t)
      (let ((v2 (assoc (carc v1) alist2)))
        (when (or (null v2) (> (the fixnum (cdrc v1)) (the fixnum (cdrc v2))))
          (return nil))))))

(defun symbol-count-not-greaterp (scount1 scount2)
  (and (not (> (symbol-count-total scount1) (symbol-count-total scount2)))
       (symbol-count-not-greaterp1 scount1 scount2)))

(defun wff-symbol-counts (wff &optional subst)
  (let ((poscount nil)
        (negcount nil))
    (prog->
      (map-atoms-in-wff wff ->* atom polarity)
      (unless (eq :neg polarity)
        (setf poscount (symbol-count atom subst poscount)))
      (unless (eq :pos polarity)
        (setf negcount (symbol-count atom subst negcount))))
    (list poscount negcount)))

(defun wff-symbol-counts-not-greaterp (scounts1 scounts2)
  (let ((poscount1 (first scounts1))
        (negcount1 (second scounts1))
        poscount2
        negcount2)
    (and (implies poscount1 (and (setf poscount2 (first scounts2)) (not (> (symbol-count-total poscount1) (symbol-count-total poscount2)))))
         (implies negcount1 (and (setf negcount2 (second scounts2)) (not (> (symbol-count-total negcount1) (symbol-count-total negcount2)))))
         (implies poscount1 (symbol-count-not-greaterp1 poscount1 poscount2))
         (implies negcount1 (symbol-count-not-greaterp1 negcount1 negcount2)))))

;;; weight.lisp EOF
