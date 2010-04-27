(in-package :cl-user)

(defvar *lsw-configuration* nil)

(defun config (&optional key)
  (if key
      (let ((value (getf *lsw-configuration* key :not-found)))
	(assert (not (eq value :not-found)) (value) "Didn't find lsw configuration ~a" key)
	value)
      *lsw-configuration*
      ))

(defun config-maybe (key &optional (if-not-found :not-found))
  (getf *lsw-configuration* key if-not-found))

(defun config-set (key value)
  (setf (getf *lsw-configuration* key) value))

(defun config-default (key value)
  (let ((existing-value (getf *lsw-configuration* key :not-found)))
    (if (eq existing-value :not-found)
	(config-set key value)
	existing-value)))