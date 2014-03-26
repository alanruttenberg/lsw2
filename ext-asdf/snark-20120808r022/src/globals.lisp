;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: globals.lisp
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

(defvar *snark-globals*
  (nconc (mapcar 'first snark-lisp::*clocks*)
         (mapcar 'second snark-lisp::*clocks*)
	 '(
	   snark-lisp::*clocks*
	   snark-lisp::*excluded-clocks*
           snark-lisp::*first-real-time-value*
           snark-lisp::*first-run-time-value*
	   snark-lisp::*last-run-time-value*
           snark-lisp::*run-time-mark*
           snark-lisp::*total-seconds*
           snark-infix-reader::*infix-operators*
           snark-infix-reader::*prefix-operators*
           snark-infix-reader::*postfix-operators*
           *nonce*
	   *outputting-comment*
	   snark-lisp::*running-clocks*
           snark-feature::*feature-tree*
           *standard-eql-numbering*
           
           *cons*
           *singleton-bag*
           *bag-union*
	   *=*
	   *not*
	   *and*
	   *or*
	   *implies*
	   *implied-by*
	   *iff*
	   *xor*
	   *if*
	   *forall*
	   *exists*
	   *answer-if*
           *date-point*
           *utime-point*
           *date-interval*
           *utime-interval*

           *a-function-with-left-to-right-ordering-status*
           *a-function-with-multiset-ordering-status*
	   *agenda*
	   *agenda-of-backward-simplifiable-rows-to-process*
	   *agenda-of-false-rows-to-process*
           *agenda-of-input-rows-to-give*
	   *agenda-of-input-rows-to-process*
	   *agenda-of-new-embeddings-to-process*
           *agenda-of-rows-to-give*
	   *agenda-of-rows-to-process*
           *assert-rewrite-polarity*
	   *assertion-analysis-function-info*
	   *assertion-analysis-patterns*
	   *assertion-analysis-relation-info*
	   *atom-hash-code*
	   *conditional-answer-connective*
	   *constant-info-table*
           *constraint-rows*
           *current-row-context*
           *cycl-data*
           *cycl-read-action-table*
           *cycl-read-actionn-table*
           *date-interval-primitive-relations*
           *date-day-function*
           *date-hour-function*
           *date-minute-function*
           *date-month-function*
           *date-scenario-constant*
           *date-second-function*
           *date-year-function*
           *date-year-function2*
           *default-hash-term-set-count-down-to-hashing*
	   *dp-sort-intersections*
           *dr-universal-time-function-symbol*
	   *embedding-variables*
	   *extended-variant*
	   *false-rows*
           *feature-vector-row-index*
           *feature-vector-term-index*
	   *find-else-substitution*
           *finish-time-function-symbol*
           *form-author*
           *form-documentation*
           *form-name*
           *form-source*
	   *frozen-variables*
	   *gensym-variable-alist*
           *hint-rows*
           *hints-subsumed*
           *input-proposition-variables*
           *input-wff-substitution2*
           *input-wff-new-antecedents*
           *less*
	   *manual-ordering-results*
           *new-symbol-prefix*
           *new-symbol-table*
	   *next-variable-number*
           *nonce*
           *number-info-table*
           *number-of-new-symbols*
	   *path-index*
	   *pp-margin*
	   *pp?*
           *print-pretty2*
	   *processing-row*
           *product*
           *proof*
           *propositional-abstraction-of-input-wffs*
           *propositional-abstraction-term-to-lisp*
           *reciprocal*
           *renumber-by-sort*
           *renumber-first-number*
           *renumber-ignore-sort*
	   *rewrite-count-warning*
	   *rewrites-used*
           *root-row-context*
	   *row-count*
           *row-names*
           *rowsets*
	   *rows*
	   *skolem-function-alist*
	   *snark-is-running*
           *string-info-table*
           *subsuming*
           *sum*
           *symbol-ordering*
	   *symbol-table*
           *szs-conjecture*
           *szs-filespec*
	   *term-by-hash-array*
	   *term-memory*
	   *terpri-indent*
           *trie-index*
	   *unify-special*
	   *variables*
	   *world-path-function-alist*
	   clause-subsumption
	   critique-options
	   it
           *last-row-number-before-interactive-operation*
	   map-atoms-first
	   modal-input-wff
	   *number-of-agenda-full-deleted-rows*
	   *number-of-backward-eliminated-rows*
	   *number-of-given-rows*
	   *number-of-rows*
           *%checking-well-sorted-p%*
           *%check-for-well-sorted-atom%*
	   options-have-been-critiqued
	   options-print-mode
	   ordering-is-total
	   recursive-unstore
           *%rewrite-count%*
	   rewrite-strategy
	   rewrites-initialized
           *simplification-ordering-compare-equality-arguments-hash-table* 
	   subsumption-mark
           *top-sort*
           
           
	   ;LDPP'
           dp-tracing
           dp-tracing-choices
           dp-tracing-models
           dp-tracing-state
           *assignment-count*
           *default-atom-choice-function*
           *default-atom-cost-function*
           *default-branch-limit*
           *default-convert-to-clauses*
           *default-cost-bound*
           *default-cost-bound-function*
           *default-dependency-check*
           *default-dimacs-cnf-format*
           *default-find-all-models*
           *default-minimal-models-only*
           *default-minimal-models-suffice*
           *default-model-test-function*
           *default-more-units-function*
           *default-print-summary*
           *default-print-warnings*
           *default-pure-literal-check*
           *default-time-limit*
	   *default-subsumption*
           *dp-start-time*
           *subsumption-show-count*
           *verbose-lookahead*
           *verbose-lookahead-show-count*
           *verbose-subsumption*
	   )))

(defvar *snark-nonsave-globals*
	'(
	  *%assoc-cache-special-item%*
	  *prog->-function-second-forms*
	  *prog->-special-forms*

          $number-of-variable-blocks
          $number-of-variables-per-block
	  $number-of-variables-in-blocks

          $fv-features-per-symbol
          $fv-maximum-feature-value
          $fv-offset-neg-count
          $fv-offset-neg-max-depth
          $fv-offset-neg-min-depth
          $fv-offset-pos-count
          $fv-offset-pos-max-depth
          $fv-offset-pos-min-depth
          $fv-number-ground

	  *all-both-polarity*
	  *check-for-disallowed-answer*
          *hash-dollar-package*
          *hash-dollar-readtable*
	  *hash-term-not-found-action*
          *hash-term-only-computes-code*
          *hash-term-uses-variable-numbers*
          *input-wff*			;bound only by input-wff
	  *printing-deleted-messages*
          *redex-path*			;bound only by rewriter
	  *resolve-functions-used*
          *rewriting-row-context*	;bound only for rewriter
          *rpo-cache*			;bound only by rpo-compare-terms-top
          *rpo-cache-numbering*		;bound only by rpo-compare-terms-top
          *ac-rpo-cache*		;bound only by rpo-compare-terms-top
	  *snark-globals*
	  *snark-nonsave-globals*
	  *snark-options*
          *tptp-environment-variable*
          *tptp-format*
          *tptp-input-directory*
          *tptp-input-directory-has-domain-subdirectories*
          *tptp-input-file-type*
          *tptp-output-directory*
          *tptp-output-directory-has-domain-subdirectories*
          *tptp-output-file-type*

          rcc8-jepd-relation-names
          rcc8-more-relation-names
          time-ip-jepd-relation-names
          time-pp-jepd-relation-names
          time-ii-jepd-relation-names
          time-pi-jepd-relation-names
          time-ip-more-relation-names
          time-pp-more-relation-names
          time-ii-more-relation-names
          time-pi-more-relation-names

          $rcc8-composition-table *rcc8-composition-table*
          $time-iii-composition-table *time-iii-composition-table*
          $time-iip-composition-table
          $time-ipi-composition-table *time-ipi-composition-table*
          $time-ipp-composition-table
          $time-pii-composition-table *time-pii-composition-table*
          $time-pip-composition-table *time-pip-composition-table*
          $time-ppi-composition-table *time-ppi-composition-table*
          $time-ppp-composition-table *time-ppp-composition-table*
          $rcc8-relation-code
          $time-ii-relation-code
          $time-ip-relation-code
          $time-pi-relation-code
          $time-pp-relation-code

          dp-prover
          dp-version
	  false
	  float-internal-time-units-per-second
	  initialization-functions
          none
	  true
	  ))

;;; more than one copy of SNARK can be run alternately
;;; by using SUSPEND-SNARK and RESUME-SNARK
;;;
;;; SUSPEND-SNARK re-initializes SNARK so the run can be continued
;;; only after RESUME-SNARK; a suspended SNARK can only be resumed once
;;;
;;; SUSPEND-SNARK saves the values of SNARK's global variables;
;;; RESUME-SNARK restores them
;;;
;;; SUSPEND-AND-RESUME-SNARK suspends the current SNARK and resumes
;;; another without unnecessarily re-initializing

(defun suspend-snark* ()
  (let ((state (gensym)))
    (setf (symbol-value state)
	  (mapcar (lambda (var)
                    (cons var
                          (if (boundp var)
                              (symbol-value var)
                              '%unbound%)))
		  *snark-globals*))
    state))

(defun resume-snark (state)
  (let ((l (and (boundp state) (symbol-value state))))
    (cond
      ((consp l)
       (setf (symbol-value state) nil)
       (mapc (lambda (x)
               (if (eq '%unbound% (cdr x))
                   (makunbound (car x))
                   (setf (symbol-value (car x)) (cdr x))))
	     l))
      (t
       (error "Cannot resume SNARK from state ~S." state)))
    nil))

(defun suspend-snark ()
  (prog1
    (suspend-snark*)
    (initialize)))

(defun suspend-and-resume-snark (state)
  (prog1
    (suspend-snark*)
    (resume-snark state)))

(defun audit-snark-globals ()
  ;; used for suspend/resume to make sure all necessary values are saved;
  ;; prints names of symbols that might have been overlooked
  (dolist (package-name '(:snark-lisp :snark))
    (let ((package (find-package package-name)))
      (do-symbols (x package)
	(when (and (boundp x) (eq package (symbol-package x)))
	  (unless (or (member x *snark-globals*) (member x *snark-nonsave-globals*))
	    (print x)))))))

;;; globals.lisp EOF
