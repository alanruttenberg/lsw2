;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: term-hash.lisp
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

(defvar *atom-hash-code*)
(defvar *term-by-hash-array*)
(defvar *hash-term-uses-variable-numbers* t)
(defvar *hash-term-only-computes-code* nil)
(defvar *hash-term-not-found-action* :add)

(defun initialize-term-hash ()
  (setf *atom-hash-code* 0)
  (setf *term-by-hash-array* (make-sparse-vector))
  nil)

(defun make-atom-hash-code ()
  ;; return a hash-code in [2,1023]
  (if (<= (setf *atom-hash-code* (mod (+ (* 129 *atom-hash-code*) 1) 1024)) 1)
      (make-atom-hash-code)
      *atom-hash-code*))

(defun find-term-by-hash (x hash)
  (let* ((term-by-hash-array *term-by-hash-array*)
	 (terms (sparef term-by-hash-array hash)))
    (when terms
      (dolist (term terms)
	(when (eq term x)
	  (return-from find-term-by-hash term)))
      (dolist (term terms)
	(when (equal-p term x)
	  (return-from find-term-by-hash term))))
    (ecase *hash-term-not-found-action*
      (:add
	(setf (sparef term-by-hash-array hash) (cons x terms))
	x)
      (:throw
	(throw 'hash-term-not-found none))
      (:error
	(error "No hash-term for ~S." x)))))

(defun term-by-hash-array-terms (&optional delete-variants)
  (let ((terms nil) terms-last)
    (prog->
      (map-sparse-vector *term-by-hash-array* ->* l)
      (copy-list l -> l)
      (ncollect (if (and delete-variants (not *hash-term-uses-variable-numbers*))
                    (delete-duplicates l :test #'variant-p)
                    l)
                terms))
    (if (and delete-variants *hash-term-uses-variable-numbers*)
        (delete-duplicates terms :test #'variant-p)
        terms)))

(defmacro thvalues (hash x)
  `(if *hash-term-only-computes-code* ,hash (values ,hash ,x)))

(defun hash-term* (x subst)
  (dereference
   x subst
   :if-variable (thvalues (if *hash-term-uses-variable-numbers* (+ 1024 (variable-number x)) 0) x)
   :if-constant (thvalues (constant-hash-code x) x)
   :if-compound (mvlet (((:values hash x) (hash-compound x subst)))
                  (thvalues hash (if (eq *cons* (head x)) x (find-term-by-hash x hash))))))

(defun hash-term-code (x &optional subst)
  ;; just return the hash code without finding or creating canonical forms
  (let ((*hash-term-only-computes-code* t))
    (hash-term* x subst)))

(defun hash-term (x &optional subst)
  ;; find or create canonical form of x.subst
  ;; but doesn't store a canonical form for conses
  ;; (equal-p x (hash-term x))
  ;; (equal-p x y) => (eql (hash-term x) (hash-term y))
  (when (test-option38?)
    (return-from hash-term (instantiate x subst)))
  (mvlet (((:values hash x) (hash-term* x subst)))
    (values x hash)))

(defun some-hash-term (x &optional subst)
  ;; hash-term or none
  (let ((*hash-term-not-found-action* :throw))
    (catch 'hash-term-not-found
      (hash-term x subst))))

(defun the-hash-term (x &optional subst)
  ;; hash-term or error
  (let ((*hash-term-not-found-action* :error))
    (hash-term x subst)))

(defun hash-list (l subst multiplier)
  ;; (a b c ...) -> 2*hash(a) + 3*hash(b) + 4*hash(c) ...
  (cond
   ((null l)
    0)
   (t
    (mvlet* ((x (first l))
             ((:values xhash x*) (hash-term* x subst))
             (y (rest l)))
      (when multiplier
        (setf xhash (* multiplier xhash)))
      (if (null y)
          (thvalues xhash (if (eql x x*) l (cons x* nil)))
          (mvlet (((:values yhash y*) (hash-list y subst (and multiplier (+ multiplier 1)))))
            (thvalues (+ xhash yhash) (if (and (eq y y*) (eql x x*)) l (cons x* y*)))))))))

(defun hash-compound (compd &optional subst)
  ;; this uses a simpler term hashing function than before
  ;; it should be is easier to verify and maintain
  ;;
  ;; for (f t1 ... tn) it computes (+ (# f) (* 2 (# t1)) ... (* (+ n 1) (# tn)))
  ;; but uses 0 for (# f) if f is associative (since these symbols may disappear)
  ;; and uses 1 for multipliers if f is associative, commutative, etc.
  ;;
  ;; when *hash-term-uses-variable-numbers* is nil
  ;; it should be the case that (implies (subsumes-p t1 t2) (<= (# t1) (# t2)))
  (let ((head (head compd))
        (args (args compd)))
    (cond
     ((null args)
      (thvalues (function-hash-code head) compd))
     (t
      (ecase (function-index-type head)
        ((nil :hash-but-dont-index)
         (mvlet (((:values hash args*)
                  (hash-list args subst (and (not (function-associative head))
                                             (not (function-commutative head))
                                             2))))
           (incf hash (if (function-associative head)
                          (* (function-hash-code head) (+ 1 (length (rest (rest args)))))
                          (function-hash-code head)))
           (thvalues hash (if (eq args args*) compd (make-compound* head args*)))))
        (:commute
         (prog->
           (first args -> arg1)
           (hash-term* arg1 subst -> hash1 arg1*)
           (second args -> arg2)
           (hash-term* arg2 subst -> hash2 arg2*)
           (rest (rest args) -> args3)
           (hash-list args3 subst 4 -> hash3 args3*)
           (thvalues (+ (function-hash-code head) (* 2 hash1) (* 2 hash2) hash3)
                     (if (eq args3 args3*)
                         (if (eql arg2 arg2*)
                             (if (eql arg1 arg1*)
                                 compd
                                 (make-compound* head arg1* (rest args)))
                             (make-compound* head arg1* arg2* args3))
                         (make-compound* head arg1* arg2* args3*)))))
        (:jepd
         (prog->
           (first args -> arg1)
           (hash-term* arg1 subst -> hash1 arg1*)
           (second args -> arg2)
           (hash-term* arg2 subst -> hash2 arg2*)
           (third args -> arg3)
           (instantiate arg3 subst -> arg3*)
           (thvalues (+ (function-hash-code head) (* 2 hash1) (* 2 hash2))
                     (if (eq arg3 arg3*)
                         (if (eql arg2 arg2*)
                             (if (eql arg1 arg1*)
                                 compd
                                 (make-compound* head arg1* (rest args)))
                             (make-compound* head arg1* arg2* (rest (rest args))))
                         (make-compound head arg1* arg2* arg3*))))))))))

(defun print-term-hash (&key (details t) terms)
  (let ((a (and details (make-sparse-vector :default-value 0)))
        (nterms 0))
    (prog->
      (map-sparse-vector *term-by-hash-array* ->* l)
      (length l -> len)
      (incf nterms len)
      (when details
        (incf (sparef a len))))
    (cond
     (details
      (format t "~%; Term-hash-array has ~:D position~:P filled with ~:D term~:P in all."
              (sparse-vector-count *term-by-hash-array*) nterms)
      (prog->
        (map-sparse-vector-with-indexes a ->* n len)
        (format t "~%; Term-hash-array has ~:D position~:P filled with ~:D term~:P each." n len)))
     (t
      (format t "~%; Term-hash-array has ~:D term~:P in all." nterms))))
  (when terms
    (prog->
      (map-sparse-vector-with-indexes *term-by-hash-array* ->* l position)
      (when (implies (and (numberp terms) (< 1 terms)) (>= (length l) terms))
        (format t "~%; ~6D: ~S~{~%;         ~S~}" position (first l) (rest l))))))

(defvar *default-hash-term-set-count-down-to-hashing* 10)	;can insert this many before hashing

(defstruct (hash-term-set
            (:constructor make-hash-term-set (&optional substitution))
            (:conc-name :hts-))
  (terms nil)					;list or hash-table of terms
  (substitution nil :read-only t)
  (count-down-to-hashing *default-hash-term-set-count-down-to-hashing*))

(defun hts-member-p (term hts)
  (let* ((terms (hts-terms hts))
         (l (if (eql 0 (hts-count-down-to-hashing hts))
               (gethash (hash-term-code term) terms)
               terms)))
    (if (and l (member-p term l (hts-substitution hts))) t nil)))

(defun hts-adjoin-p (term hts)
  ;; if term is a already a member of hts, return NIL
  ;; otherwise add it and return true
  (let* ((terms (hts-terms hts))
         (c (hts-count-down-to-hashing hts))
         h
         (l (if (eql 0 c)
                (gethash (setf h (hash-term-code term)) terms)
                terms)))
    (cond
     ((and l (member-p term l (hts-substitution hts)))
      nil)
     ((eql 0 c)
      (setf (gethash h terms) (cons term l))
      t)
     ((eql 1 c)
      (setf (hts-terms hts) (setf terms (make-hash-table)))
      (setf (gethash (hash-term-code term) terms) (cons term nil))
      (dolist (term l)
        (push term (gethash (hash-term-code term) terms)))
      (setf (hts-count-down-to-hashing hts) 0)
      t)
     (t
      (setf (hts-terms hts) (cons term l))
      (setf (hts-count-down-to-hashing hts) (- c 1))
      t))))

;;; term-hash.lisp EOF
