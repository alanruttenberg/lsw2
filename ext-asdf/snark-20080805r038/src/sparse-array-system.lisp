;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: common-lisp-user -*-
;;; File: sparse-array-system.lisp
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

(in-package :common-lisp-user)

(defpackage :snark-sparse-array
  (:use :common-lisp :snark-lisp)
  (:export
   #:sparef
   #:sparse-vector #:make-sparse-vector #:sparse-vector-p
   #:sparse-vector-boolean #:sparse-vector-default-value
   #:sparse-vector-count
   #:map-sparse-vector #:map-sparse-vector-with-indexes #:map-sparse-vector-indexes-only
   #:with-sparse-vector-iterator
   #:first-sparef #:last-sparef #:pop-first-sparef #:pop-last-sparef
   #:copy-sparse-vector #:spacons
   #:sparse-matrix #:make-sparse-matrix #:sparse-matrix-p
   #:sparse-matrix-boolean #:sparse-matrix-default-value
   #:sparse-matrix-count
   #:sparse-matrix-row #:sparse-matrix-column #:sparse-matrix-rows #:sparse-matrix-columns
   #:map-sparse-matrix #:map-sparse-matrix-with-indexes #:map-sparse-matrix-indexes-only

   #:sparse-vector-expression-p
   #:map-sparse-vector-expression
   #:map-sparse-vector-expression-with-indexes
   #:map-sparse-vector-expression-indexes-only
   #:optimize-sparse-vector-expression
   #:uniond
   ))

(loads "sparse-vector5" "sparse-array" "sparse-vector-expression")

;;; sparse-array-system.lisp EOF
