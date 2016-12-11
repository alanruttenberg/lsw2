(in-package :jss)

;; Unneeded.  Use (require :abcl-contrib)(require :abcl) stanzas instead.
(defun ensure-jss ()
  (set-dispatch-macro-character #\# #\" 'read-invoke))
