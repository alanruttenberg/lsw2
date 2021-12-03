;;;; -*- Mode: LISP -*-

(in-package :asdf)

(defsystem hets
  :name "HETS API via Docker"
  :author "Alan Ruttenberg" :version "2.0.0"
  :depends-on (logic)
  :components
  ((:org "hets")
   (:file "tptp-logic-generator.lisp")
   (:file "tptp-parser"))
  :depends-on ("cl-tptp-parser")
  :defsystem-depends-on  ("lilith"))


