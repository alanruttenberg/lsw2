;;;; -*- Mode: LISP -*-
;;;;

(in-package :asdf)

(defsystem :logic
  :name "LOGIC"
  :author "Alan Ruttenberg"
  :license "BSD"
  :components
  ((:file "logic-generator")
   (:file "snark-logic-generator")
   (:file "prover9-logic-generator")
   (:file "prover-with-prover9"))
  :depends-on ()
  :serial t)


