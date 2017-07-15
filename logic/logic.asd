;;;; -*- Mode: LISP -*-
;;;;

(in-package :asdf)

(defsystem :logic
  :name "LOGIC"
  :author "Alan Ruttenberg"
  :license "BSD"
  :components
  ((:module "package"
	    :pathname ""
 	    :components
	    ((:file "package")))
   (:module "main"
	    :pathname ""
 	    :components
	    ((:file "logic-generator")
	     (:file "axiom")
	     (:file "snark-logic-generator")
	     (:file "prover9-logic-generator")
	     (:file "prove-with-prover9"))
	    :depends-on ("package")))
  :depends-on ()
  :serial t)


