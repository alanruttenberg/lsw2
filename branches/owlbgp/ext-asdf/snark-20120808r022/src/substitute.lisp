;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: substitute.lisp
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

(defun substitute (new old x &optional subst)
  "substitute new for old in x"
  (dereference
   old subst
   :if-constant (if (function-symbol-p old)
		    (unimplemented)
		    (substitute-for-constant new old x subst))
   :if-compound (substitute-for-compound new old x subst)
   :if-variable (substitute-for-variable new old x subst)))

(defun substitutel (new old l &optional subst)
  (dereference
   old subst
   :if-constant (if (function-symbol-p old)
                    (unimplemented)
                    (substitute-for-constantl new old l subst))
   :if-compound (substitute-for-compoundl new old l subst)
   :if-variable (substitute-for-variablel new old l subst)))

(defun substitute-for-constant (new old x subst)
  "substitute new for constant old in x"
  ;; if old = nil, replace it in conses, but not at end of argument lists
  (dereference
   x subst
   :if-constant (if (eql old x) new x)
   :if-compound-cons (let* ((u (carc x)) (u* (substitute-for-constant new old u subst))
                            (v (cdrc x)) (v* (substitute-for-constant new old v subst)))
                       (if (and (eql u u*) (eql v v*)) x (cons u* v*)))
   :if-compound-appl (let* ((args (argsa x)) (args* (substitute-for-constantl new old args subst)))
                       (if (eq args args*) x (make-compound* (heada x) args*)))
   :if-variable x))

(defun substitute-for-compound (new old x subst)
  "substitute new for compound old in x"
  (dereference
   x subst
   :if-constant x
   :if-compound-cons (cond
		      ((equal-p old x subst)
		       new)
		      (t
		       (lcons (substitute-for-compound new old (car x) subst)
			      (substitute-for-compound new old (cdr x) subst)
			      x)))
   :if-compound-appl (cond
		      ((equal-p old x subst)
		       new)
		      (t
		       (let* ((args (argsa x)) (args* (substitute-for-compoundl new old args subst)))
		         (if (eq args args*) x (make-compound* (heada x) args*)))))
   :if-variable x))

(defun substitute-for-variable (new old x subst)
  "substitute new for variable old in x"
  (dereference
   x subst
   :if-constant x
   :if-compound-appl (let* ((args (argsa x)) (args* (substitute-for-variablel new old args subst)))
                       (if (eq args args*) x (make-compound* (heada x) args*)))
   :if-compound-cons (lcons (substitute-for-variable new old (carc x) subst)
			    (substitute-for-variable new old (cdrc x) subst)
			    x)
   :if-variable (if (eq old x) new x)))

(defun substitute-once (cc new old x &optional subst)
  (dereference
   old subst
   :if-constant (if (function-symbol-p old)
		    (unimplemented)
		    (substitute-for-constant-once cc new old x subst))
   :if-compound (substitute-for-compound-once cc new old x subst)
   :if-variable (substitute-for-variable-once cc new old x subst)))

(defun substitute-for-constant-once (cc new old x subst)
  ;; if old = nil, replace it in conses, but not at end of argument lists
  (dereference
   x subst
   :if-constant (when (eql old x)
		  (funcall cc new))
   :if-compound-cons (let ((u (carc x)) (v (cdrc x)))
                       (prog->
                         (substitute-for-constant-once new old u subst ->* u*)
                         (funcall cc (cons u* v)))
                       (prog->
                         (substitute-for-constant-once new old v subst ->* v*)
                         (funcall cc (cons u v*))))
   :if-compound-appl (prog->
                       (argsa x ->nonnil args)
                       (heada x -> head)
                       (substitute-for-constant-oncel new old args subst ->* args*)
                       (funcall cc (make-compound* head args*)))))

(defun substitute-for-compound-once (cc new old x subst)
  (dereference
   x subst
   :if-compound-cons (cond
		      ((equal-p old x subst)
		       (funcall cc new))
                      (t
		       (let ((u (carc x)) (v (cdrc x)))
		         (prog->
		           (substitute-for-compound-once new old u subst ->* u*)
		           (funcall cc (cons u* v)))
		         (prog->
		           (substitute-for-compound-once new old v subst ->* v*)
		           (funcall cc (cons u v*))))))
   :if-compound-appl (cond
		      ((equal-p old x subst)
		       (funcall cc new))
		      (t
		       (prog->
		         (argsa x ->nonnil args)
		         (heada x -> head)
                         (substitute-for-compound-oncel new old args subst ->* args*)
                         (funcall cc (make-compound* head args*)))))))

(defun substitute-for-variable-once (cc new old x subst)
  (dereference
   x subst
   :if-compound-cons (let ((u (carc x)) (v (cdrc x)))
		       (prog->
		         (substitute-for-variable-once new old u subst ->* u*)
		         (funcall cc (cons u* v)))
		       (prog->
		         (substitute-for-variable-once new old v subst ->* v*)
		         (funcall cc (cons u v*))))
   :if-compound-appl (prog->
                       (argsa x ->nonnil args)
                       (heada x -> head)
                       (substitute-for-variable-oncel new old args subst ->* args*)
                       (funcall cc (make-compound* head args*)))
   :if-variable (when (eq old x)
		  (funcall cc new))))

(defun substitute-for-constantl (new old l subst)
  (lcons (substitute-for-constant new old (first l) subst)
	 (substitute-for-constantl new old (rest l) subst)
	 l))

(defun substitute-for-compoundl (new old l subst)
  (lcons (substitute-for-compound new old (first l) subst)
	 (substitute-for-compoundl new old (rest l) subst)
	 l))

(defun substitute-for-variablel (new old l subst)
  (lcons (substitute-for-variable new old (first l) subst)
	 (substitute-for-variablel new old (rest l) subst)
	 l))

(defun substitute-for-constant-oncel (cc new old l subst)
  (let ((a (first l)) (d (rest l)))
    (prog->
     (substitute-for-constant-once new old a subst ->* a*)
     (funcall cc (cons a* d)))
    (when d
      (prog->
       (substitute-for-constant-oncel new old d subst ->* d*)
       (funcall cc (cons a d*))))))

(defun substitute-for-compound-oncel (cc new old l subst)
  (let ((a (first l)) (d (rest l)))
    (prog->
     (substitute-for-compound-once new old a subst ->* a*)
     (funcall cc (cons a* d)))
    (when d
      (prog->
       (substitute-for-compound-oncel new old d subst ->* d*)
       (funcall cc (cons a d*))))))

(defun substitute-for-variable-oncel (cc new old l subst)
  (let ((a (first l)) (d (rest l)))
    (prog->
     (substitute-for-variable-once new old a subst ->* a*)
     (funcall cc (cons a* d)))
    (when d
      (prog->
       (substitute-for-variable-oncel new old d subst ->* d*)
       (funcall cc (cons a d*))))))

;;; substitute.lisp EOF
