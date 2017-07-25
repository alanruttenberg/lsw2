;;;; -*- Mode: LISP -*-
;;;;

(in-package :asdf)

(defsystem :logic
  :name "LOGIC"
  :author "Alan Ruttenberg"
  :license "BSD"
  :components
  ((:module "package" :pathname ""
    :components
    ((:file "package")))
   (:module "main" :pathname ""
    :components
    ((:file "axiom")
     (:file "logic-generator"))
    :serial t
    :depends-on ("package"))
   (:module "provers" :pathname ""
    :components 
    ((:file "snark-logic-generator")
     (:file "prover9-logic-generator")
     (:file "z3-logic-generator")
     (:file "latex-logic-generator")
     (:file "prove-with-prover9")
     (:file "prove-with-z3")
     (:file "prove-with-vampire")
     (:file "proof"))
    :depends-on ("package" "main")))
  :depends-on (:cl-ansi-text)
  )


