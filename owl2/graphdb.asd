;;;; -*- Mode: LISP -*-
;;;;

(in-package :asdf)

;(load (merge-pathnames "winston-ai/winston-forward-chain.asd" *load-truename*))

(defsystem :graphdb
  :name "GRAPHDB"
  :author "Alan Ruttenberg"
  :license "BSD"
  :components
  ((:file "graphdb")
   (:file "graphdb9")))
