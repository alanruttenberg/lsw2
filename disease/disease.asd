;;;; -*- Mode: LISP -*-
;;;;

(in-package :asdf)

(setf (logical-pathname-translations "disease")
      `(
	("**;*.*" ,(make-pathname :directory (append (pathname-directory *load-pathname*) '(:wild-inferiors))
				    :name :wild
				    :type :wild))
	))

(defsystem :disease
  :name "Disease"
  :licence "BSD"
  :components
  ((:module api
	    :pathname ""
	    :components
	    ((:file "explore")))
   )
  :depends-on (jss cl-json graph-dag umls))

;;;; eof
