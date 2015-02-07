;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: common-lisp-user -*-
;;; File: feature-system.lisp
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

(defpackage :snark-feature
  (:use :common-lisp :snark-lisp)
  (:export
   #:initialize-features
   #:make-feature #:declare-feature
   #:declare-features-incompatible
   #:feature? #:feature-parent
   #:the-feature
   #:delete-feature #:feature-live?
   #:feature-union #:feature-subsumes?
   #:print-feature-tree
   ))

(loads "feature")

;;; feature-system.lisp EOF
