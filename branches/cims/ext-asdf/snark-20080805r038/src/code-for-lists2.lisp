;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: code-for-lists2.lisp
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

(defun declare-code-for-lists2 ()
  (declare-constant nil :constructor t :sort 'null)
  (setf *cons* (declare-function1 '$$cons 2 :constructor t :to-lisp-code 'cons-term-to-lisp :sort 'cons :ordering-status :left-to-right))

  (declare-ordering-greaterp '$$cons nil)

  (declare-function1 '$$list :any :macro t :input-code 'input-lisp-list)
  (declare-function1 '$$list* :any :macro t :input-code 'input-lisp-list*)

  (declare-characteristic-relation '$$listp #'listp 'list)
  (declare-characteristic-relation '$$consp #'consp 'cons)
  (declare-characteristic-relation '$$null  #'null  'null)
  )

;;; code-for-lists2.lisp EOF
