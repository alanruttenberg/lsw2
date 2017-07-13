;;;; -*- Mode: LISP -*-
;;;;

(in-package :asdf)

(defsystem :logic
  :name "LOGIC"
  :author "Alan Ruttenberg"
  :license "BSD"
  :components
  ((:file "logic-generator")
   (:file "snark-logic-generator"))
  :depends-on ()
  :serial t)


