;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: common-lisp-user -*-
;;; File: snark-pkg.lisp
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

;;; package definitions for snark system

(defpackage :snark
  (:use :common-lisp
	:snark-lisp
        :snark-deque
	:snark-sparse-array
        :snark-numbering
	:snark-agenda
	:snark-infix-reader
        :snark-feature
        :snark-dpll)
  (:import-from :common-lisp-user #:*compile-me*)
  (:shadow #:terpri)
  #-gcl (:shadow #:assert #:substitute #:variable #:row #:rows)
  (:export
   #:*hash-dollar-package* #:*hash-dollar-readtable* #:hash-dollar-prin1 #:hash-dollar-print
   #:*compile-me*
   #:profile #:sprofile
   #:can-be-constant-name
   #:can-be-free-variable-name
   #:declare-cancellation-law
   #:declare-snark-option
   #:derivation-subsort-forms
   #:function-logical-symbol-p
   #:function-symbol-p
   #:input-constant-symbol
   #:input-function-symbol
   #:input-relation-symbol
   #:input-proposition-symbol
   #:set-options
   #:make-snark-system
   #:map-rows
   #:matches-compound		;rewrite-compiler
   #:matches-constant		;rewrite-compiler
   #:print-agendas
   #:print-ancestry
   #:print-options
   #:print-rewrites
   #:print-row
   #:print-rows
   #:print-feature-tree
   #:print-row-term
   #:print-sort-theory
   #:print-summary
   #:print-symbol-ordering
   #:print-symbol-table
   #:print-term
   #:read-assertion-file
   #:refute-file
   #:do-tptp-problem #:do-tptp-problem0 #:do-tptp-problem1
   #:sort-name-p
   #:sortal
   #:temporal
   #:term-to-lisp
   #:var

   #:initialize #:assume #:prove #:hint #:closure #:proof #:proofs #:answer #:answers
   #:new-prove

   #:give #:factor #:resolve #:hyperresolve #:negative-hyperresolve
   #:paramodulate #:paramodulate-by #:ur-resolve #:rewrite #:condense
   #:row #:rows #:name-row #:last-row #:it #:mark-as-given
   #:delete-row #:delete-rows
   #:assert-rewrite

   #:make-row-context #:delete-row-context #:in-row-context
   #:push-row-context #:pop-row-context #:new-row-context
   #:current-row-context #:root-row-context

   #:dereference
   #:variable-p #:constant-p #:compound-p #:head #:args #:arg1 #:arg2
   #:make-compound #:make-compound*
   #:equal-p #:unify
   #:constant-sort #:variable-sort #:term-sort
   #:constant-name
   #:function-name #:function-arity
   #:row-name #:row-number #:row-name-or-number #:row-wff #:row-answer #:row-constraints
   #:row-constrained-p #:row-ancestry #:row-reason #:row-rewrites-used #:row-parents

   #:constant-documentation #:constant-author #:constant-source
   #:function-documentation #:function-author #:function-source
   #:sort-documentation #:sort-author #:sort-source
   #:row-documentation #:row-author #:row-source #:row-input-wff

   #:answer-if
   #:~ #:&
   #:=> #:<=>
   #:? #:?x #:?y #:?z #:?u #:?v #:?w #:_
   #:-- #:---

   #:symbol-table-entries #:symbol-table-constant? #:symbol-table-function? #:symbol-table-relation?

   #:read-infix-term
   #:initialize-operator-syntax #:declare-operator-syntax #:declare-tptp-operators

   #:assertion #:assumption #:conjecture #:negated_conjecture #:combine #:embed #:purify

   #:|cnf| #:|fof| #:|tff|	;for TPTP
   #:|axiom| #:|conjecture| #:|negated_conjecture| #:|assumption| #:|hypothesis|
   #:|question| #:|negated_question|
   #:|type|
   #:|$tType| #:|$i| #:|$o| #:|$int| #:|$rat| #:|$real|
   #:|$true| #:|$false|
   #:|file|
   #:|include|

   #:declare-constant #:declare-proposition
   #:declare-function #:declare-relation
   #:declare-variable

   #:declare-ordering-greaterp

   #:declare-sort #:declare-subsort #:declare-sorts-incompatible
   #:the-sort
   #:sort-name
   #:sort-intersection
   #:subsort? #:sort-disjoint?

   #:literal-ordering-a #:literal-ordering-n #:literal-ordering-p

   #:checkpoint-theory #:uncheckpoint-theory #:restore-theory
   #:suspend-snark #:resume-snark #:suspend-and-resume-snark

   #:fifo #:lifo
   #:row-depth #:row-size #:row-weight #:row-level
   #:row-size+depth #:row-weight+depth
   #:row-size+depth+level #:row-weight+depth+level
   #:row-weight-limit-exceeded #:row-weight-before-simplification-limit-exceeded
   #:row-wff&answer-weight+depth #:row-neg-size+depth

   #:in-language #:in-kb
   #:when-system
   #:has-author #:has-source #:my-source
   #:has-documentation #:has-name
   #:undefined

   #:declare-jepd-relations
   #:declare-rcc8-relations
   #:declare-time-relations
   #:region #:time-interval #:time-point
   #:$$date-point #:$$utime-point
   #:$$date-interval #:$$utime-interval

   #:$$rcc8-dc #:$$rcc8-ec #:$$rcc8-po #:$$rcc8-tpp #:$$rcc8-ntpp #:$$rcc8-tppi #:$$rcc8-ntppi #:$$rcc8-eq
   #:$$rcc8-dr #:$$rcc8-pp #:$$rcc8-p #:$$rcc8-ppi #:$$rcc8-pi #:$$rcc8-o #:$$rcc8-c
   #:$$rcc8-tp #:$$rcc8-tpi
   #:$$rcc8-not-tpp #:$$rcc8-not-ntpp #:$$rcc8-not-ec #:$$rcc8-not-po #:$$rcc8-not-eq #:$$rcc8-not-ntppi
   #:$$rcc8-not-tppi #:$$rcc8-not-pp #:$$rcc8-not-p #:$$rcc8-not-ppi #:$$rcc8-not-pi #:$$rcc8-not-tp #:$$rcc8-not-tpi

   ;; 3 primitive temporal point-point relations
   #:$$time-pp-before #:$$time-pp-equal #:$$time-pp-after

   ;; nonprimitive temporal point-point relations
   #:$$time-pp-not-before #:$$time-pp-not-equal #:$$time-pp-not-after

   ;; 13 primitive temporal interval-interval relations
   #:$$time-ii-before #:$$time-ii-during #:$$time-ii-overlaps #:$$time-ii-meets #:$$time-ii-starts
   #:$$time-ii-finishes #:$$time-ii-equal #:$$time-ii-finished-by #:$$time-ii-started-by
   #:$$time-ii-met-by #:$$time-ii-overlapped-by #:$$time-ii-contains #:$$time-ii-after
   #:$$time-ii-contained-by	;alias of time-ii-during

   ;; nonprimitive temporal interval-interval relations
   #:$$time-ii-starts-before #:$$time-ii-starts-equal #:$$time-ii-starts-after
   #:$$time-ii-finishes-before #:$$time-ii-finishes-equal #:$$time-ii-finishes-after
   #:$$time-ii-subsumes #:$$time-ii-subsumed-by
   #:$$time-ii-disjoint #:$$time-ii-intersects
   #:$$time-ii-not-before #:$$time-ii-not-during #:$$time-ii-not-overlaps #:$$time-ii-not-meets
   #:$$time-ii-not-starts #:$$time-ii-not-finishes #:$$time-ii-not-equal
   #:$$time-ii-not-finished-by #:$$time-ii-not-started-by
   #:$$time-ii-not-met-by #:$$time-ii-not-overlapped-by #:$$time-ii-not-contains #:$$time-ii-not-after
   #:$$time-ii-not-starts-before #:$$time-ii-not-starts-equal #:$$time-ii-not-starts-after
   #:$$time-ii-not-finishes-before #:$$time-ii-not-finishes-equal #:$$time-ii-not-finishes-after
   #:$$time-ii-not-subsumes #:$$time-ii-not-subsumed-by

   ;; 5 primitive temporal point-interval relations
   #:$$time-pi-before #:$$time-pi-starts #:$$time-pi-during #:$$time-pi-finishes #:$$time-pi-after
   #:$$time-ip-before #:$$time-ip-started-by #:$$time-ip-contains #:$$time-ip-finished-by #:$$time-ip-after
   #:$$time-pi-contained-by	;alias of time-pi-during

   ;; nonprimitive temporal point-interval relations
   #:$$time-pi-disjoint #:$$time-pi-intersects
   #:$$time-pi-not-before #:$$time-pi-not-starts #:$$time-pi-not-during #:$$time-pi-not-finishes #:$$time-pi-not-after
   #:$$time-ip-disjoint #:$$time-ip-intersects
   #:$$time-ip-not-after #:$$time-ip-not-started-by #:$$time-ip-not-contains #:$$time-ip-not-finished-by #:$$time-ip-not-before

   #:$$numberp #:$$realp #:$$rationalp #:$$integerp #:$$naturalp #:$$complexp

   ;; float support is going away; use reals or rationals instead; floats are automatically translated to ratios on input
   ;; integer quotient support is going away; use floor, ceiling, truncate, or round instead

   #:$$eq                   #:$$eq_integer          #:$$eq_rational          #:$$eq_real          #:$$eq_complex
   #:$$less               #:$$less_integer        #:$$less_rational        #:$$less_real
   #:$$lesseq           #:$$lesseq_integer      #:$$lesseq_rational      #:$$lesseq_real
   #:$$greater         #:$$greater_integer     #:$$greater_rational     #:$$greater_real
   #:$$greatereq     #:$$greatereq_integer   #:$$greatereq_rational   #:$$greatereq_real
   #:$$sum                 #:$$sum_integer         #:$$sum_rational         #:$$sum_real         #:$$sum_complex
   #:$$product         #:$$product_integer     #:$$product_rational     #:$$product_real     #:$$product_complex
   #:$$difference   #:$$difference_integer  #:$$difference_rational  #:$$difference_real  #:$$difference_complex
   #:$$uminus           #:$$uminus_integer      #:$$uminus_rational      #:$$uminus_real      #:$$uminus_complex
   #:$$quotient                               #:$$quotient_rational    #:$$quotient_real    #:$$quotient_complex
   #:$$reciprocal                           #:$$reciprocal_rational  #:$$reciprocal_real  #:$$reciprocal_complex
   #:$$abs                 #:$$abs_integer         #:$$abs_rational         #:$$abs_real         #:$$abs_complex
   #:$$realpart                                                                             #:$$realpart_complex
   #:$$imagpart                                                                             #:$$imagpart_complex
   #:$$floor             #:$$floor_integer       #:$$floor_rational       #:$$floor_real
   #:$$ceiling         #:$$ceiling_integer     #:$$ceiling_rational     #:$$ceiling_real
   #:$$truncate       #:$$truncate_integer    #:$$truncate_rational    #:$$truncate_real
   #:$$round             #:$$round_integer       #:$$round_rational       #:$$round_real
   #:$$modulus         #:$$modulus_integer     #:$$modulus_rational     #:$$modulus_real
   #:$$remainder     #:$$remainder_integer   #:$$remainder_rational   #:$$remainder_real

   #:$$evaleq

   #:$$quote

   #:$$cons #:$$list #:$$list*
   #:$$listp #:$$consp #:$$null
;; #:$$term-to-list #:$$list-to-term #:$$list-to-atom

   #:$$stringp #:$$string-to-list #:$$list-to-string

   #:$$bag #:$$bag*
   #:$$bag-cons #:$$empty-bag
   #:$$bagp #:$$nonempty-bag-p #:$$empty-bag-p
   #:$$bag-to-list #:$$list-to-bag

   #:bag #:nonempty-bag #:empty-bag

   #:positive       #:positive-real    #:positive-rational    #:positive-integer    #:positive-ratio
   #:negative       #:negative-real    #:negative-rational    #:negative-integer    #:negative-ratio
   #:nonnegative #:nonnegative-real #:nonnegative-rational #:nonnegative-integer #:nonnegative-ratio
   #:nonzero         #:nonzero-real     #:nonzero-rational     #:nonzero-integer    #:nonzero-number
   #:nonpositive
   #:zero
   #:natural

   #:the-string
   #:the-list #:the-cons #:the-null
   #:the-bag #:the-nonempty-bag #:the-empty-bag
   #:the-number #:the-real #:the-complex
   #:the-rational
   #:the-integer #:the-ratio

   #:positive #:the-positive
   #:negative #:the-negative
   #:nonpositive #:the-nonpositive
   #:nonnegative #:the-nonnegative
   #:nonzero #:the-nonzero
   #:zero #:the-zero

   #:positive-real #:negative-real #:nonnegative-real
   #:positive-rational #:negative-rational #:nonnegative-rational #:zero-rational
   #:positive-integer #:negative-integer #:nonnegative-integer #:zero-integer

   #:positive-integer #:the-positive-integer
   #:nonnegative-integer #:the-nonnegative-integer
   #:natural #:the-natural

   #:*tptp-environment-variable*
   #:*tptp-format*
   #:*tptp-input-directory*
   #:*tptp-input-directory-has-domain-subdirectories*
   #:*tptp-input-file-type*
   #:*tptp-output-directory*
   #:*tptp-output-directory-has-domain-subdirectories*
   #:*tptp-output-file-type*

   #:save-snark-system
   #:with-no-output
   ))

(defpackage :snark-user
  (:use :common-lisp
        :snark-lisp
        :snark-deque
        :snark-sparse-array
        :snark-dpll
        :snark)
  (:shadowing-import-from :snark #:assert))

;;; snark-pkg.lisp EOF
