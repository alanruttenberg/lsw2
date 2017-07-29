(eval-when (:load-toplevel :execute :compile-toplevel)
  (loop for early in '("OWL-SEXP-TO-FOL" "URI-LABEL" "URI-FULL" "CAMELCASE")
		      do (intern early :cl-user)))
(defpackage :logic
  (:use cl)
  (:import-from :cl-user cl-user::owl-sexp-to-fol
		cl-user::uri-full cl-user::uri-p cl-user::uri-label cl-user::*default-kb*  
		cl-user::camelcase cl-user::replace-all
		cl-user::get-vagrant-box-id cl-user::get-vagrant-box-wd cl-user::vagrant-box-status cl-user::vagrant-box-up)
  (:export
   #:l-forall #:l-exists #:l-and #:l-or #:l-iff #:l-equal #:l-not
   #:l-implies #:pred-property #:pred-class #:*use-holds*
   #:logic-generator #:with-logic-var #:with-logic-vars #: #:
   #:def-logic-axiom #:axiom-sexp #:axiom-name #:axiom-description
   #:delete-axiom #:get-axiom #:prover9-prove #:mace4-find-model
   #:get-axioms
   #:prover9-logic-generator
   #:z3-logic-generator.
   #:latex-logic-generator.
   #:collect-axioms-from-spec
   #:z3-prove #:z3-find-model
   #:z3-check-satisfiability
   #:z3-syntax-check
   #:z3-render
   #:render
   #:render-axiom #:render-axioms
   #:vampire-prove
   #:vampire-render
   #:pprint-spec-axioms #:clear-axioms
   #:*last-z3-input* #:*last-z3-output*
   #:*last-vampire-input* #:*last-vampire-output*
   #:def-expect-satisfiable #:def-expect-unsatisfiable #:def-expect-provable #:def-expect-not-entailed 
   #:run-proof #:proof-form #:render-proof #:prove-with
   #:clif-logic-generator #:dol-logic-generator
   #:render-ontology
   #:vampire-check-unsatisfiable
   #:prover9-check-unsatisfiable
  ))
