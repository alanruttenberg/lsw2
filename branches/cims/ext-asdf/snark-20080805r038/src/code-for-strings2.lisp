;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: code-for-strings2.lisp
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

(defun declare-code-for-strings ()
  (declare-characteristic-relation '$$stringp #'stringp 'string)

  (declare-function1 '$$list-to-string 1 :rewrite-code 'list-to-string-term-rewriter :sort 'string)
  (declare-function1 '$$string-to-list 1 :rewrite-code 'string-to-list-term-rewriter :sort 'list)	;nil and $$cons must be of sort list for this to work
  nil)

(defun string-list-p (x &optional subst)
  (dereference
    x subst
    :if-constant (null x)
    :if-compound-cons (and (let ((a (carc x)))
                             (dereference a subst :if-constant (and (stringp a) (= 1 (length a)))))
                           (string-list-p (cdrc x) subst))))

(defun string-to-list (string)
  ;; (string-to-list "abc") -> (list "a" "b" "c")
  (map 'list (lambda (char) (declare-constant (string char))) string))

(defun list-to-string (list &optional subst)
  ;; (list-to-string (list "a" "b" "c")) -> "abc"
  ;; list is already dereferenced
  (cond
   ((null list)
    (declare-constant ""))
   (t
    (declare-constant (apply #'concatenate 'string (instantiate list subst))))))

(defun list-to-string-term-rewriter (term subst)
  (let ((x (first (args term))))
    (if (dereference x subst :if-constant (null x) :if-compound-cons (string-list-p x subst))
        (list-to-string x subst)
        none)))

(defun string-to-list-term-rewriter (term subst)
  (let ((x (first (args term))))
    (if (dereference x subst :if-constant (stringp x))
        (string-to-list x)
        none)))

;;; code-for-strings2.lisp EOF
