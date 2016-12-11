;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: constraint-purify.lisp
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

(defun constraint-purify-wff (wff)
  (let ((ucp (use-constraint-purification?)))
    ;; if ucp = 1, add equality constraints to wff instead of constraint-alist
    ;; if ucp = 2, add all constraints to wff instead of constraint-alist
    (setf ucp 2)				;separate constraints aren't fully supported yet
    (let ((vars (nontheory-variables wff))
          (constraint-alist-additions nil)
          (wff-additions true)
          (cache nil))
      (labels
        ((constraint-purify-atom (atom polarity)
           (dereference
            atom nil
            :if-constant atom
            :if-variable (not-wff-error atom)
            :if-compound-cons (not-wff-error atom)
            :if-compound-appl (let* ((head (heada atom))
                                     (args (argsa atom))
                                     (theory2 (function-constraint-theory head))
                                     (args* (constraint-purify-terms args theory2))
                                     (atom* (if (eq args args*) atom (make-compound* head args*))))
                                (if (or (null theory2) (eql 2 ucp))
                                    atom*
                                    (ecase polarity
                                      (:pos (add-constraint (negate atom*) theory2) false)
                                      (:neg (add-constraint atom* theory2) true))))))
         
         (constraint-purify-term (term theory1)
           (let ((theory2 nil) (dont-abstract nil))
             (dereference
              term nil
              :if-variable (setf dont-abstract (not (member term vars)))
              :if-constant (setf dont-abstract (constant-constructor term))
              :if-compound (let* ((head (head term))
                                  (args (args term))
                                  (args* (constraint-purify-terms
                                          args
                                          (if (setf dont-abstract (function-constructor head))
                                              theory1		;constructor symbols are transparent wrt theory
                                              (setf theory2 (function-constraint-theory head))))))
                             (unless (eq args args*)
                               (setf term (make-compound* head args*)))))
             (cond
              ((or dont-abstract (eq theory1 theory2))
               term)
              (theory1
               (variable-for term (or theory2 'equality)))
              (t
               (variable-for (variable-for term (or theory2 'equality)) 'equality)))))
         
         (constraint-purify-terms (terms theory)
           (lcons (constraint-purify-term (first terms) theory)
                  (constraint-purify-terms (rest terms) theory)
                  terms))
         
         (add-constraint (lit theory)
           (if (or (null theory) (eql 2 ucp) (and (eql 1 ucp) (eq 'equality theory)))
               (setf wff-additions (conjoin lit wff-additions))
               (setf constraint-alist-additions (conjoin-alist1 theory lit constraint-alist-additions))))
         
         (variable-for (term theory)
           ;; create a variable to use in place of term and store ($$eq var term) in theory constraint
           (or (cdr (assoc term cache :test #'equal-p))
               (let ((var (make-variable (term-sort term))))
                 (add-constraint (make-compound *eq* var term) theory)
                 (setf cache (acons term var cache))
                 var))))
        
        (values
         (disjoin
          (prog->
            (map-atoms-in-wff-and-compose-result wff ->* atom polarity)
            (constraint-purify-atom atom polarity))
          (negate wff-additions))
         constraint-alist-additions)))))

;;; constraint-purify.lisp EOF
