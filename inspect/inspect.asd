;;;; -*- Mode: LISP -*-

(in-package :asdf)

(defsystem :inspect
  :name "Java inspector for ABCL"
  :author "Mike Travers"
  :version "1"
  :licence ""
  :components
  ((:file "skijcomp")
   (:file "window")
   (:file "swing")
   (:file "thread")
   (:file "browse-url")
   (:file "oinspect")
   (:file "rdfinspect")
   (:file "grapher")
   )
  :depends-on
  (util))

;;;; eof
