;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: common-lisp-user -*-
;;; File: lisp-system.lisp
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

(in-package :common-lisp-user)

(defpackage :snark-lisp
  (:use :common-lisp)
  (:export

   ;; defined in mvlet.lisp
   #:mvlet #:mvlet*

   ;; defined in progc.lisp
   #:prog->
   #:*prog->-function-second-forms*
   #:*prog->-special-forms*

   ;; defined in lisp.lisp
   #:none
   #:true #:false
   #:definline
   #:neq #:neql #:nequal #:nequalp
   #:if-let #:when-let
   #:iff #:implies
   #:kwote #:unquote
   #:rrest #:rrrest #:rrrrest
   #:mklist #:firstn #:consn #:leafp
   #:naturalp #:ratiop
   #:carc #:cdrc #:caarcc #:cadrcc #:cdarcc #:cddrcc
   #:lcons
   #:cons-unless-nil #:push-unless-nil #:pushnew-unless-nil
   #:dotails #:dopairs
   #:choose
   #:integers-between #:ints
   #:length= #:length< #:length<= #:length> #:length>=
   #:acons+ #:alist-notany-plusp #:alist-notany-minusp
   #:cons-count
   #:find-or-make-package
   #:percentage
   #:print-current-time
   #:leap-year-p #:days-per-month #:month-number
   #:print-args
   #:define-plist-slot-accessor
   #:*print-pretty2*
   #:with-standard-io-syntax2
   #:quit

   ;; defined in collectors.lisp
   #:make-collector #:collector-value #:collect-item #:collect-list
   #:make-queue #:queue-empty-p #:enqueue #:dequeue
   #:collect #:ncollect

   ;; defined in map-file.lisp
   #:mapnconc-stream-forms #:mapnconc-stream-lines
   #:mapnconc-file-forms #:mapnconc-file-lines
   #:read-file #:read-file-lines #:read-file-to-string

   ;; defined in clocks.lisp
   #:initialize-clocks #:print-clocks
   #:with-clock-on #:with-clock-off
   #:total-run-time
   #:print-incremental-time-used

   ;; defined in counters.lisp
   #:make-counter
   #:increment-counter #:decrement-counter
   #:counter-value #:counter-values
   #:princf

   ;; defined in topological-sort.lisp
   #:topological-sort* #:topological-sort

   ;; undefined symbols used by snark
   #:implied-by #:xor #:nand #:nor
   #:forall #:exists
   #:$$cons #:$$list #:$$list*
   ))

(loads "mvlet" "progc" "lisp" "collectors" "map-file" "clocks" "counters" "topological-sort")

;;; lisp-system.lisp EOF
