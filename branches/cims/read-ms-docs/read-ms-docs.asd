;;;; -*- Mode: LISP -*-
;;;;

(in-package :asdf)

(setf (logical-pathname-translations "read-ms-docs")
      `(
	("**;*.*" ,(make-pathname :directory (append (pathname-directory *load-pathname*) '(:wild-inferiors))
				    :name :wild
				    :type :wild))
	))

(defsystem :read-ms-docs
    :name "Apache POI - the Java API for Microsoft Documents"
    :licence "BSD"
    :components
    ((:module jars 
	      :serial t
	      :pathname ""
	      :components
	      ((:jar-directory "apache-poi")))
     (:module files :pathname ""
	      :depends-on (jars)
	      :components
	      ((:file "spreadsheet")
	       (:file "blocks"))))
    :depends-on (jss))

;;;; eof
