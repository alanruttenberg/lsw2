;;;; -*- Mode: LISP -*-
;;;;

(in-package :asdf)

(setf (logical-pathname-translations "umls")
      `(
	("**;*.*" ,(make-pathname :directory (append (pathname-directory *load-pathname*) '(:wild-inferiors))
				    :name :wild
				    :type :wild))
	))

(defsystem :umls
  :name "UMLS Rest API"
  :licence "BSD"
  :components
  ((:module macros
	    :pathname ""
	    :components
	    ((:file "define-api-function")))
   (:module files :pathname ""
	    :depends-on (macros)
	    :serial t
	    :components
	    ((:file "api")
	     (:file "helpers")
	     )))
  :depends-on (jss))

;;;; eof
