;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: common-lisp-user -*-
;;; File: deque-system.lisp
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
;;; Portions created by the Initial Developer are Copyright (C) 1981-2005.
;;; All Rights Reserved.
;;;
;;; Contributor(s): Mark E. Stickel <stickel@ai.sri.com>.

(in-package :common-lisp-user)

(defpackage :snark-deque
  (:use :common-lisp :snark-lisp)
  (:export
   #:make-deque
   #:deque?
   #:deque-empty?
   #:deque-push-first #:deque-pop-first #:deque-first
   #:deque-push-last #:deque-pop-last #:deque-last
   #:deque-length
   #:deque-delete
   #:deque-delete-if
   #:mapnconc-deque
;; #:deque-node-value
;; #:deque-next-node #:deque-prev-node
;; #:deque-insert-before-node #:deque-insert-after-node
;; #:deque-delete-node
   ))

(loads "deque")

;;; deque-system.lisp EOF
