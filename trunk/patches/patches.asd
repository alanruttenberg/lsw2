;;;; -*- Mode: LISP -*-

(in-package :asdf)

(defsystem :patches
  :name "patches"
  :serial t
  :components
  ((:file "abcl-trace")
   (:file "abcl-src")
   (:file "ensure-directories-exist")
   (:file "jinterface-safe-implementation")
  ))

;;;; eof
