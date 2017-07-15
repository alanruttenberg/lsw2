(defpackage :logic
  (:use cl)
  (:export
   #:l-forall #:l-exists #:l-and #:l-or #:l-iff #:l-equal
   #:l-implies #:pred-property #:pred-class #:*use-holds*
   #:logic-generator #:with-logic-var #:with-logic-vars #: #:
   #:def-logic-axiom #:axiom-sexp #:axiom-name #:axiom-description
   #:delete-axiom #:get-axiom #:prover9-prove #:mace4-find-model
   #:get-axioms
  ))
