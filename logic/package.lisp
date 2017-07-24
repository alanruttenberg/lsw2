(eval-when (:load-toplevel :execute :compile-toplevel)
  (intern "OWL-SEXP-TO-FOL" :cl-user))
(defpackage :logic
  (:use cl)
  (:import-from :cl-user cl-user::owl-sexp-to-fol)
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
   #:pprint-spec-axioms
   #:*last-z3-input* #:*last-z3-output*
   #:*last-vampire-input* #:*last-vampire-output*
   #:def-expect-satisfiable #:def-expect-unsatisfiable #:def-expect-provable #:def-expect-not-entailed 
   #:run-proof #:proof-form #:render-proof #:prove-with 
  ))
