(setf *package* (find-package :snark-user))
(setf *print-pretty* nil)
#+openmcl (egc nil)
(funcall (intern (symbol-name :initialize) :snark))

