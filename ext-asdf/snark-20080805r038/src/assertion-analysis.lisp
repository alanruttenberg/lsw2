;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: assertion-analysis.lisp
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

;;; the main purpose of this code is to recognize axioms
;;; for commutativity, associativity, etc. so that the
;;; appropriate function or relation symbol declarations can be
;;; made when running TPTP problems, where stupid and inconvenient
;;; rules do not allow any problem-specific input other than the axioms
;;;
;;; in general, using assertion-analysis to automatically declare
;;; special properties of relations and functions is NOT encouraged

(in-package :snark)

(defvar *wff*)

(declaim (special *extended-variant*))

(defvar *assertion-analysis-patterns*)
(defvar *assertion-analysis-function-info*)
(defvar *assertion-analysis-relation-info*)

(defstruct aa-function
  function
  (left-identities nil)
  (right-identities nil)
  (left-inverses nil)
  (right-inverses nil)
  (commutative nil)
  (associative nil)
  (closure-relations nil))

(defstruct aa-relation
  relation
  (left-identities nil)
  (right-identities nil)
  (left-inverses nil)
  (right-inverses nil)
  (commutative nil)
  (assoc1-p nil)
  (assoc2-p nil)
  (functional-p nil)
  (closure-functions nil))

(defun aa-function (f)
  (let ((f# (funcall *standard-eql-numbering* :lookup f)))
    (or (sparef *assertion-analysis-function-info* f#)
        (progn
	  (cl:assert (function-symbol-p f))
	  (setf (sparef *assertion-analysis-function-info* f#)
	        (make-aa-function :function f))))))

(defun aa-relation (p)
  (let ((p# (funcall *standard-eql-numbering* :lookup p)))
    (or (sparef *assertion-analysis-relation-info* p#)
        (progn
	  (cl:assert (function-symbol-p p))
	  (setf (sparef *assertion-analysis-relation-info* p#)
	        (make-aa-relation :relation p))))))

(defun print-assertion-analysis-note (name)
  (with-standard-io-syntax2
    (format t "~%; Recognized ~A assertion ~S." name (renumber *wff*))))

(defun note-function-associative (f)
  (when (print-assertion-analysis-notes?)
    (print-assertion-analysis-note "associativity"))
  (setf (aa-function-associative (aa-function f)) t))

(defun note-function-commutative (f)
  (when (print-assertion-analysis-notes?)
    (print-assertion-analysis-note "commutativity"))
  (setf (aa-function-commutative (aa-function f)) t))

(defun note-function-left-identity (f e)
  (when (print-assertion-analysis-notes?)
    (print-assertion-analysis-note "left identity"))
  (pushnew e (aa-function-left-identities (aa-function f))))

(defun note-function-right-identity (f e)
  (when (print-assertion-analysis-notes?)
    (print-assertion-analysis-note "right identity"))
  (pushnew e (aa-function-right-identities (aa-function f))))

(defun note-function-left-inverse (f g e)
  (when (print-assertion-analysis-notes?)
    (print-assertion-analysis-note "possible left inverse"))
  (pushnew (list g e) (aa-function-left-inverses (aa-function f)) :test #'equal))

(defun note-function-right-inverse (f g e)
  (when (print-assertion-analysis-notes?)
    (print-assertion-analysis-note "possible right inverse"))
  (pushnew (list g e) (aa-function-right-inverses (aa-function f)) :test #'equal))

(defun note-relation-assoc1 (p)
  (when (print-assertion-analysis-notes?)
    (print-assertion-analysis-note "possible associativity"))
  (setf (aa-relation-assoc1-p (aa-relation p)) t))

(defun note-relation-assoc2 (p)
  (when (print-assertion-analysis-notes?)
    (print-assertion-analysis-note "possible associativity"))
  (setf (aa-relation-assoc2-p (aa-relation p)) t))

(defun note-relation-commutative (p)
  (when (print-assertion-analysis-notes?)
    (print-assertion-analysis-note "commutativity"))
  (setf (aa-relation-commutative (aa-relation p)) t))

(defun note-relation-left-identity (p e)
  (when (print-assertion-analysis-notes?)
    (print-assertion-analysis-note "possible left identity"))
  (pushnew e (aa-relation-left-identities (aa-relation p))))

(defun note-relation-right-identity (p e)
  (when (print-assertion-analysis-notes?)
    (print-assertion-analysis-note "possible right identity"))
  (pushnew e (aa-relation-right-identities (aa-relation p))))

(defun note-relation-left-inverse (p g e)
  (when (print-assertion-analysis-notes?)
    (print-assertion-analysis-note "possible left inverse"))
  (pushnew (list g e) (aa-relation-left-inverses (aa-relation p)) :test #'equal))

(defun note-relation-right-inverse (p g e)
  (when (print-assertion-analysis-notes?)
    (print-assertion-analysis-note "possible right inverse"))
  (pushnew (list g e) (aa-relation-right-inverses (aa-relation p)) :test #'equal))

(defun note-relation-functional (p)
  (when (print-assertion-analysis-notes?)
    (print-assertion-analysis-note "relation functionality"))
  (setf (aa-relation-functional-p (aa-relation p)) t))

(defun note-relation-closure (p f)
  (when (print-assertion-analysis-notes?)
    (print-assertion-analysis-note "relation function"))
  (pushnew f (aa-relation-closure-functions (aa-relation p)))
  (pushnew p (aa-function-closure-relations (aa-function f))))

(defun function-associativity-tests ()
  (let ((f (make-function-symbol (gensym) 2))
	(x (make-variable))
	(y (make-variable))
	(z (make-variable)))
    (list
      ;; (= (f (f x y) z) (f x (f y z)))
      (list (make-equality0 (make-compound f (make-compound f x y) z) (make-compound f x (make-compound f y z)))
	    (list 'note-function-associative f)))))

(defun function-commutativity-tests ()
  (let ((f (make-function-symbol (gensym) 2))
	(x (make-variable))
	(y (make-variable)))
    (list
      ;; (= (f x y) (f y x))
      (list (make-equality0 (make-compound f x y) (make-compound f y x))
	    (list 'note-function-commutative f)))))

(defun function-identity-tests ()
  (let ((f (make-function-symbol (gensym) 2))
	(e (gensym))
	(x (make-variable)))
    (list
      ;; (= (f e x) x)
      (list (make-equality0 (make-compound f e x) x)
	    (list 'note-function-left-identity f e))
      ;; (= (f x e) x)
      (list (make-equality0 (make-compound f x e) x)
	    (list 'note-function-right-identity f e)))))

(defun function-inverse-tests ()
  (let ((f (make-function-symbol (gensym) 2))
	(g (make-function-symbol (gensym) 1))
	(e (gensym))
	(x (make-variable)))
    (list
      ;; (= (f (g x) x) e)
      (list (make-equality0 (make-compound f (make-compound g x) x) e)
	    (list 'note-function-left-inverse f g e))
      ;; (= (f x (g x)) e)
      (list (make-equality0 (make-compound f x (make-compound g x)) e)
	    (list 'note-function-right-inverse f g e)))))

(defun relation-associativity-tests ()
  (let ((p (make-function-symbol (gensym) 3))
	(x (make-variable))
	(y (make-variable))
	(z (make-variable))
	(u (make-variable))
	(v (make-variable))
	(w (make-variable)))
    (let ((a (make-compound p x y u))
	  (b (make-compound p y z v))
	  (c (make-compound p u z w))
	  (d (make-compound p x v w)))
      (list
       ;; (or (not (p x y u)) (not (p y z v)) (not (p u z w)) (p x v w))
       (list (make-compound *or*
			    (make-compound *not* a)
			    (make-compound *not* b)
			    (make-compound *not* c)
			    d)
	     (list 'note-relation-assoc1 p))
       ;; (implies (and (p x y u) (p y z v) (p u z w)) (p x v w))
       (list (make-compound *implies*
			    (make-compound *and* a b c)
			    d)
	     (list 'note-relation-assoc1 p))
       ;; (or (not (p x y u)) (not (p y z v)) (not (p x v w)) (p u z w))
       (list (make-compound *or*
			    (make-compound *not* a)
			    (make-compound *not* b)
			    (make-compound *not* d)
			    c)
	     (list 'note-relation-assoc2 p))
       ;; (implies (and (p x y u) (p y z v) (p x v w)) (p u z w))
       (list (make-compound *implies*
			    (make-compound *and* a b d)
			    c)
	     (list 'note-relation-assoc2 p))))))

(defun relation-commutativity-tests ()
  (let ((p (make-function-symbol (gensym) 3))
	(x (make-variable))
	(y (make-variable))
	(z (make-variable)))
    (loop for a in (list (make-compound p x y) (make-compound p x y z))
	  as  b in (list (make-compound p y x) (make-compound p y x z))
	  nconc (list
		  ;; (or (not (p x y)) (p x y))  and  (or (not (p x y z)) (p y x z))
		  (list (make-compound *or* (make-compound *not* a) b)
			(list 'note-relation-commutative p))
		  ;; (implies (p x y) (p y x))   and  (implies (p x y z) (p y x z))
		  (list (make-compound *implies* a b)
			(list 'note-relation-commutative p))))))

(defun relation-identity-tests ()
  (let ((p (make-function-symbol (gensym) 3))
	(e (gensym))
	(x (make-variable)))
    (list
      ;; (p e x x)
      (list (make-compound p e x x)
	    (list 'note-relation-left-identity p e))
      ;; (p x e x)
      (list (make-compound p x e x)
	    (list 'note-relation-right-identity p e)))))

(defun relation-inverse-tests ()
  (let ((p (make-function-symbol (gensym) 3))
	(g (make-function-symbol (gensym) 1))
	(e (gensym))
	(x (make-variable)))
    (list
      ;; (p (g x) x e)
      (list (make-compound p (make-compound g x) x e)
	    (list 'note-relation-left-inverse p g e))
      ;; (p x (g x) e)
      (list (make-compound p x (make-compound g x) e)
	    (list 'note-relation-right-inverse p g e)))))

(defun relation-functionality-tests ()
  (let ((p (make-function-symbol (gensym) 3))
	(x (make-variable))
	(y (make-variable))
	(z1 (make-variable))
	(z2 (make-variable)))
    (let ((a (make-compound p x y z1))
	  (b (make-compound p x y z2))
	  (c (make-equality0 z1 z2)))
      (list
	;; (or (not (p x y z1)) (not (p x y z2)) (= z1 z2))
	(list
	  (make-compound *or*
			    (make-compound *not* a)
			    (make-compound *not* b)
			    c)
	  (list 'note-relation-functional p))
	;; (implies (and (p x y z1) (p x y z2)) (= z1 z2))
	(list
	  (make-compound *implies*
			    (make-compound *and* a b)
			    c)
	  (list 'note-relation-functional p))))))

(defun relation-closure-tests ()
  (let ((p (make-function-symbol (gensym) 3))
	(f (make-function-symbol (gensym) 2))
	(x (make-variable))
	(y (make-variable)))
    (list
      (list
	(make-compound p x y (make-compound f x y))
	(list 'note-relation-closure p f)))))

(defun initialize-assertion-analysis ()
  (setf *assertion-analysis-function-info* (make-sparse-vector))
  (setf *assertion-analysis-relation-info* (make-sparse-vector))
  (setf *assertion-analysis-patterns*
	(nconc (function-associativity-tests)
	       (function-commutativity-tests)
	       (function-identity-tests)
	       (function-inverse-tests)
	       (relation-associativity-tests)
	       (relation-commutativity-tests)
	       (relation-identity-tests)
	       (relation-inverse-tests)
	       (relation-functionality-tests)
	       (relation-closure-tests)
	       ))
  nil)

(defun assertion-analysis (row)
  (prog->
    (when (row-bare-p row)
      (row-wff row -> wff)
      (identity wff -> *wff*)
      (quote t -> *extended-variant*)
      (dolist *assertion-analysis-patterns* ->* x)
      (variant (first x) wff nil nil ->* varpairs)
      (sublis varpairs (second x) -> decl)
      (apply (first decl) (rest decl))
      (return-from assertion-analysis))))

(defun maybe-declare-function-associative (f)
  (unless (function-associative f)
    (when (or (use-associative-unification?) (function-commutative f))
      (declare-function-associative f t))))

(defun maybe-declare-function-commutative (f)
  (unless (function-commutative f)
    (declare-function-commutative f t)))

(defun aa-relation-associative (p)
  (if (or (aa-relation-commutative p)
	  (function-commutative (aa-relation-relation p)))
      (or (aa-relation-assoc1-p p) (aa-relation-assoc2-p p))
      (and (aa-relation-assoc1-p p) (aa-relation-assoc2-p p))))

(defun complete-assertion-analysis ()
  (prog->
    (map-sparse-vector *assertion-analysis-function-info* ->* f)
    (when (aa-function-commutative f)
      (maybe-declare-function-commutative (aa-function-function f)))
    (when (aa-function-associative f)
      (maybe-declare-function-associative (aa-function-function f))))
  (prog-> 
    (map-sparse-vector *assertion-analysis-relation-info* ->* p)
    (when (aa-relation-commutative p)
      (maybe-declare-function-commutative (aa-relation-relation p))
      (when (aa-relation-functional-p p)
	(dolist (f (aa-relation-closure-functions p))
	  (maybe-declare-function-commutative f))))
    (when (aa-relation-associative p)
      (when (aa-relation-functional-p p)
	(dolist (f (aa-relation-closure-functions p))
	  (maybe-declare-function-associative f))))))

;;; assertion-analysis.lisp EOF
