(in-package :jss)

(defun ensure-jss ()
  (set-dispatch-macro-character #\# #\" 'read-invoke))
