;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark-lisp -*-
;;; File: progc.lisp
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

(in-package :snark-lisp)

(defparameter *prog->-function-second-forms*
	      '(funcall apply map map-into))

(defparameter *prog->-special-forms*
 '(
;; (pattern . forms)

   ((dolist list-form &rest l ->* var)
    (dolist (var list-form . l)
      (unnamed-prog-> . prog->-tail)))
   ((dotails list-form &rest l ->* var)
    (dotails (var list-form . l)
      (unnamed-prog-> . prog->-tail)))
   ((dopairs list-form &rest l ->* var1 var2)
    (dopairs (var1 var2 list-form . l)
      (unnamed-prog-> . prog->-tail)))
   ((dotimes count-form &rest l ->* var)
    (dotimes (var count-form . l)
      (unnamed-prog-> . prog->-tail)))
   ((identity form -> var)
    (let ((var form))
      (unnamed-prog-> . prog->-tail)))
   ))

(defun prog->*-function-second-form-p (fn)
  (member fn *prog->-function-second-forms*))

(defun prog->-special-form (fn)
  (assoc fn *prog->-special-forms* :key #'first))

(defun prog->-special-form-pattern (fn)
  (car (prog->-special-form fn)))

(defun prog->-special-form-args (fn)
  (rest (prog->-special-form-pattern fn)))

(defun prog->-special-form-result (fn)
  (cdr (prog->-special-form fn)))

(defun prog->-special-form-match-error (form)
  (error "~S doesn't match prog-> special form ~S."
	 form (prog->-special-form-pattern (first form))))

(defun prog->-no-variable-error (form)
  (error "No variable to assign value to in (prog-> ... ~S ...)."
	 form))

(defun prog->-too-many-variables-error (form)
  (error "More than one variable to assign value to in (prog-> ... ~S ...)." form))

(defun prog->-too-many->s-error (form)
  (error "More than one -> in (prog-> ... ~S ...)." form))

(defun prog->-unrecognized->-atom (atom form)
  (error "Unrecognized operation ~S in (prog-> ... ~S ...)." atom form))

(defun prog->-atom (x)
  (and (symbolp x)
       (<= 2 (length (string x)))
       (string= x "->" :end1 2)))

(defun prog->*-function-argument (forms args)
  (cond
    ((and (null (rest forms))
	  (consp (first forms))
	  (eq (caar forms) 'funcall)
	  (equal (cddar forms) args))
     (cadar forms))
    ((and (null (rest forms))
	  (consp (first forms))
	  (not (#-(or lucid (and mcl (not openmcl))) special-operator-p
;;		#-(or allegro lucid) special-form-p
;;		#+allegro cltl1:special-form-p
		#+(and mcl (not openmcl)) special-form-p
		#+lucid lisp:special-form-p
		(caar forms)))
	  (not (macro-function (caar forms)))
	  (equal (cdar forms) args))
     `(function ,(caar forms)))
    (t
     `(function (lambda ,args ,@forms)))))

(defun process-prog-> (forms)
  (cond
    ((null forms)
     nil)
    (t
     (let ((form (first forms)))
       (cond
	 ((not (consp form))
	  (cons form (process-prog-> (rest forms))))
	 (t
	  (let* ((args (rest form))
		 (x (member-if #'prog->-atom args)))
	    (cond
	      ((null x)
	       (cons (case (first form)		;forms with explicit or implicit progn also get prog-> processing
		       ((progn)
			(process-prog->-progn (rest form)))
		       ((block when unless let let* mvlet mvlet* catch)
			(list* (first form)
			       (second form)
			       (process-prog-> (cddr form))))
		       ((multiple-value-bind progv)
			(list* (first form)
			       (second form)
			       (third form)
			       (process-prog-> (cdddr form))))
		       ((cond)
			(cons (first form)
			      (mapcar (lambda (x)
                                        (cons (first x)
                                              (process-prog-> (rest x))))
				      (rest form))))
		       ((case ecase ccase typecase etypecase ctypecase)
			(list* (first form)
			       (second form)
			       (mapcar (lambda (x)
                                         (cons (first x)
                                               (process-prog-> (rest x))))
				       (cddr form))))
		       ((if)
			(cl:assert (<= 3 (length form) 4))
			(list (first form)
			      (second form)
			      (process-prog->-progn (list (third form)))
			      (process-prog->-progn (list (fourth form)))))
		       (otherwise
			 form))
		     (process-prog-> (rest forms))))
	      ((prog->-special-form (first form))
	       (do ((formals (prog->-special-form-args (first form)) (rest formals))
		    (args args (rest args))
		    (alist (acons 'prog->-tail (rest forms) nil)))
		   (nil)
		 (cond
		   ((and (endp formals) (endp args))
		    (return (sublis alist (prog->-special-form-result (first form)))))
		   ((endp formals)
		    (prog->-special-form-match-error form))
		   ((eq (first formals) '&rest)
		    (setf formals (rest formals))
		    (cond
		      ((or (endp args) (prog->-atom (first args)))
		       (setf args (cons nil args))
		       (setf alist (acons (first formals) nil alist)))
		      (t
		       (setf alist (acons (first formals)
					  (loop collect (first args)
						until (or (endp (rest args)) (prog->-atom (second args)))
						do (pop args))
					  alist)))))
		   ((endp args)
		    (prog->-special-form-match-error form))
		   ((prog->-atom (first formals))
		    (unless (string= (string (first formals)) (string (first args)))
		      (prog->-special-form-match-error form)))
		   (t
		    (setf alist (acons (first formals) (first args) alist))))))
	      ((member-if #'prog->-atom (rest x))
	       (prog->-too-many->s-error form))
	      (t
	       (let ((inputs (ldiff args x))
		     (outputs (rest x)))
		 (cond
		   ((string= (string (first x)) "->*")
                    (let ((funarg (prog->*-function-argument (process-prog-> (rest forms)) outputs)))
                      (cond
                       ((and (consp funarg)
                             (eq 'function (first funarg))
                             (consp (second funarg))
                             (eq 'lambda (first (second funarg))))
                        (let ((g (gensym)))
                          (list
                           `(flet ((,g ,@(rest (second funarg))))
                              (declare (dynamic-extent (function ,g)))
                              ,@(prog->*-call form inputs `(function ,g))))))
                       (t
                        (prog->*-call form inputs funarg)))))
		   ((null outputs)
		    (prog->-no-variable-error form))
		   ((string= (string (first x)) "->")
		    (cond
		      ((null (rest outputs))
                       (cond
                        ((and (consp (first outputs))
                              (member (first (first outputs)) '(values list list* :values :list :list*)))
                         (list `(mvlet ((,(first outputs) (,(first form) ,@inputs)))
                                  ,@(process-prog-> (rest forms)))))
                        (t
		         (list `(let ((,(first outputs) (,(first form) ,@inputs)))
				  ,@(process-prog-> (rest forms)))))))
		      (t
		       (list `(multiple-value-bind ,outputs
				  (,(first form) ,@inputs)
				,@(process-prog-> (rest forms)))))))
		   ((string= (string (first x)) (symbol-name :->nonnil))
		    (cond
		      ((null (rest outputs))
                       (cond
                        ((and (consp (first outputs))
                              (member (first (first outputs)) '(values list list* :values :list :list*)))
                         (list `(mvlet ((,(first outputs) (,(first form) ,@inputs)))
                                  (when ,(first outputs)
                                    ,@(process-prog-> (rest forms))))))
                        (t
		         (list `(let ((,(first outputs) (,(first form) ,@inputs)))
                                  (when ,(first outputs)
				    ,@(process-prog-> (rest forms))))))))
		      (t
		       (list `(multiple-value-bind ,outputs
				  (,(first form) ,@inputs)
                                (when ,(first outputs)
				  ,@(process-prog-> (rest forms))))))))
		   ((rest outputs)
		    (prog->-too-many-variables-error form))
		   ((string= (string (first x)) (symbol-name :->stack))
		    (list `(let ((,(first outputs) (,(first form) ,@inputs)))
                             (declare (dynamic-extent ,(first outputs)))
			     ,@(process-prog-> (rest forms)))))
		   ((string= (string (first x)) (symbol-name :->progv))
		    (list `(let ((!prog->temp1! (list (,(first form) ,@inputs)))
                                 (!prog->temp2! (list ,(first outputs))))
                             (declare (dynamic-extent !prog->temp1! !prog->temp2!))
                             (progv !prog->temp2! !prog->temp1! ,@(process-prog-> (rest forms))))))
		   (t
		    (prog->-unrecognized->-atom (first x) form)))))))))))))

(defun prog->*-call (form inputs funarg)
  (cond
   ((prog->*-function-second-form-p (first form))
    (list `(,(first form) ,(first inputs) ,funarg ,@(rest inputs))))
   (t
    (list `(,(first form) ,funarg ,@inputs)))))

(defun wrap-progn (forms &optional no-simplification)
  (cond
    ((and (null forms)
	  (not no-simplification))
     nil)
    ((and (null (rest forms))
	  (not no-simplification))
     (first forms))
    (t
     (cons 'progn forms))))

(defun wrap-block (name forms &optional no-simplification)
  (cond
    ((and (null forms)
	  (not no-simplification))
      nil)
    (t
     (list* 'block name forms))))

(defun process-prog->-progn (forms)
  (wrap-progn (process-prog-> forms)))

(defun process-prog->-block (forms)
  (wrap-block 'prog-> (process-prog-> forms)))

(defmacro unnamed-prog-> (&body forms)
  (process-prog->-progn forms))

(defmacro prog-> (&body forms)
  (process-prog->-block forms))

;;; progc.lisp EOF
