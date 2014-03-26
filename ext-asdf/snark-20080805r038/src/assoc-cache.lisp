;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: assoc-cache.lisp
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

(defvar *%assoc-cache-special-item%* (list '#:empty))

(defun make-assoc-cache (size)
  (cl:assert (and (integerp size) (<= 2 size 100)))
  (acons *%assoc-cache-special-item%* size nil))

(defun assoc-cache-push (item value alist)
  (let ((v (first alist)))
    (cond
      ((eq *%assoc-cache-special-item%* (car v))
       (let ((n (cdr v)))
         (declare (fixnum n))
	 (cond
	   ((eql 1 n)
	    (setf (car v) item)
	    (setf (cdr v) value))
	   (t
	    (setf (cdr v) (the fixnum (- n 1)))
	    (setf (rest alist) (acons item value (rest alist)))
	    value))))
      (t
       (dotails (l (rest alist))
	 (setf (first l) (prog1 v (setf v (first l)))))
       (setf (first alist) v)
       (setf (car v) item)
       (setf (cdr v) value)))))

(defun assoc-cache-entries (alist)
  (if (eq *%assoc-cache-special-item%* (car (first alist)))
      (rest alist)
      alist))

;;; assoc-cache.lisp EOF
