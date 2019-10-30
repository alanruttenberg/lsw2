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
  ((:module api
	    :pathname ""
	    :components
	    ((:file "define-api-function")
	     (:file "api"))
	    :serial t)
   (:module files :pathname ""
	    :components
	    ((:file "helper")
	     (:file "relations")
	     (:file "sources")
	     (:file "explore")
	     (:file "icd")
	     )))
  :depends-on (jss cl-json graph-dag))

;;;; eof
