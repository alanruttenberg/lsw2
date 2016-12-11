;;;; -*- Mode: LISP -*-

(in-package :asdf)

(defsystem :patches
  :name "patches"
  :serial t
  :components
<<<<<<< variant A
  (
;   (:file "abcl-trace")
;   (:file "abcl-src")
   (:file "ensure-directories-exist")
>>>>>>> variant B
  (
   (:file "abcl-function-doc")
####### Ancestor
  ((:file "abcl-trace")
;   (:file "abcl-src")
   (:file "ensure-directories-exist")
======= end
   (:file "jinterface-safe-implementation")
<<<<<<< variant A
   (:file "abcl-jss-fix")
   (:file "defun-interactive-record-arglist")
   (:file "jss-helper")
>>>>>>> variant B
   (:file "defun-interactive-record-arglist")
####### Ancestor
======= end
  ))

;;;; eof
