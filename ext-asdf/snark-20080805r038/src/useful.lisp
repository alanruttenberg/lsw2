;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: snark -*-
;;; File: useful.lisp
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

#+lucid
(defmacro lambda (&rest args)
  `(function (lambda ,@args)))

(defmacro setq-once (var form)
  ;; return value of var if non-nil
  ;; otherwise set var to value of form and return it
  `(or ,var (setf ,var ,form) (error "setq-once value is nil.")))

(definline assoc/eq (item alist)
  #+lucid (assoc item alist)			;depending on the implementation,
  #-lucid (assoc item alist :test #'eq)		;specifying EQ can make assoc faster
  )

(defmacro unroll-assoc (n item a-list)
  (cond
    ((<= n 0)
     `(assoc ,item ,a-list))
    (t
     (let ((x (gensym)))
       `(let ((,x ,item))
	  ,(unroll-assoc1 n x a-list))))))

(defmacro unroll-assoc/eq (n item a-list)
  (cond
    ((<= n 0)
     `(assoc/eq ,item ,a-list))
    (t
     (let ((x (gensym)))
       `(let ((,x ,item))
	  ,(unroll-assoc/eq1 n x a-list))))))

(defun unroll-assoc1 (n item a-list)
  (let ((l (gensym)))
    `(let ((,l ,a-list))
       (and ,l
	    ,(cond
	       ((<= n 0)
		`(assoc ,item ,l))
	       (t
		(let ((p (gensym)))
		  `(let ((,p (car ,l)))
		     (if (eql ,item (car ,p))
			 ,p
			 ,(unroll-assoc1 (- n 1) item `(cdr ,l)))))))))))

(defun unroll-assoc/eq1 (n item a-list)
  (let ((l (gensym)))
    `(let ((,l ,a-list))
       (and ,l
	    ,(cond
	       ((<= n 0)
		`(assoc/eq ,item ,l))
	       (t
		(let ((p (gensym)))
		  `(let ((,p (car ,l)))
		     (if (eq ,item (car ,p))
			 ,p
			 ,(unroll-assoc/eq1 (- n 1) item `(cdr ,l)))))))))))

#+lucid
(defmacro declaim (&rest declaration-specifiers)
  (list* 'eval-when
	 '(compile load eval)
	 (mapcar (lambda (x) `(proclaim ',x)) declaration-specifiers)))

#+lucid
(defmacro constantly (object)
  (function (lambda (&rest args)
	      (declare (ignore args))
	      object)))

(defun list-p (x)
  ;; if x is a null terminated list, return its length
  ;; otherwise return nil
  (let ((n 0))
    (declare (type integer n))
    (loop
      (cond
	((null x)
	 (return n))
	((atom x)
	 (return nil))
	(t
	 (incf n)
	 (setf x (rest x)))))))

(defvar *outputting-comment* nil)

(definline comment* (output-stream)
  (princ "; " output-stream)
  (setf *outputting-comment* t)	;not stream specific bug
  nil)

(definline nocomment* (output-stream)
  (declare (ignore output-stream))
  (setf *outputting-comment* nil))

(defun comment (&optional (output-stream *standard-output*))
  (unless *outputting-comment*
    (comment* output-stream)))

(defun nocomment (&optional (output-stream *standard-output*))
  (declare (ignorable output-stream))
  (nocomment* output-stream))

(defun terpri (&optional (output-stream *standard-output*))
  (cl:terpri output-stream)
  (nocomment* output-stream))

(defun terpri-comment (&optional (output-stream *standard-output*))
  (cl:terpri output-stream)
  (comment* output-stream))

(defvar *terpri-indent* 0)
(declaim (type fixnum *terpri-indent*))

(defun terpri-comment-indent (&optional (output-stream *standard-output*))
  (cl:terpri output-stream)
  (comment* output-stream)
  (dotimes (dummy *terpri-indent*)
    (declare (ignorable dummy))
    (princ " " output-stream)))

(defun terpri-indent (&optional (output-stream *standard-output*))
  (cl:terpri output-stream)
  (nocomment* output-stream)
  (dotimes (dummy *terpri-indent*)
    (declare (ignorable dummy))
    (princ " " output-stream)))

(defun unimplemented (&optional (datum "Unimplemented functionality.") &rest args)
  (apply #'error datum args))

(defvar *hash-dollar-package* nil)
(defvar *hash-dollar-readtable* nil)

(defun hash-dollar-reader (stream subchar arg)
  ;; reads exp in #$exp into package (or *hash-dollar-package* *package*) with case preserved
  (declare (ignore subchar arg))
  (let ((*readtable* *hash-dollar-readtable*)
        (*package* (or *hash-dollar-package* *package*)))
    (read stream t nil t)))

(defun initialize-hash-dollar-reader ()
  (unless *hash-dollar-readtable*
    (setf *hash-dollar-readtable* (copy-readtable nil))
    (setf (readtable-case *hash-dollar-readtable*) :preserve)
    (set-dispatch-macro-character #\# #\$ 'hash-dollar-reader *hash-dollar-readtable*)
    (set-dispatch-macro-character #\# #\$ 'hash-dollar-reader)
    t))

(initialize-hash-dollar-reader)

(defstruct (hash-dollar
            (:constructor make-hash-dollar (symbol))
            (:print-function print-hash-dollar-symbol3)
            (:copier nil))
  (symbol nil :read-only t))

(defun print-hash-dollar-symbol3 (x stream depth)
  (declare (ignore depth))
  (let* ((symbol (hash-dollar-symbol x))
         (*readtable* *hash-dollar-readtable*)
         (*package* (or (symbol-package symbol) *package*)))
    (princ "#$" stream)
    (prin1 symbol stream)))

(defun hash-dollar-symbolize (x)
  (cond
   ((consp x)
    (cons (hash-dollar-symbolize (car x)) (hash-dollar-symbolize (cdr x))))
   ((and (symbolp x) (not (null x)) #+ignore (not (keywordp x)))
    (make-hash-dollar x))
   (t
    x)))

(defun hash-dollar-prin1 (object &optional (output-stream *standard-output*))
  (prin1 (hash-dollar-symbolize object) output-stream)
  object)

(defun hash-dollar-print (object &optional (output-stream *standard-output*))
  (prog2
   (terpri output-stream)
   (hash-dollar-prin1 object output-stream)
   (princ " " output-stream)))

;;; in MCL, (hash-dollar-print '|a"b|) erroneously prints #$a"b instead of #$|a"b|
;;; it appears that readtable-case = :preserve suppresses all escape character printing,
;;; not just those for case

;;; useful.lisp EOF
