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
	      :pathname ""
	      :components
	      ((:mvn "org.apache.poi/poi/3.6")
	       (:mvn "org.apache.poi/poi-scratchpad/3.6")
	       (:mvn "org.apache.poi/poi-ooxml/3.6")
	       (:mvn "org.apache.poi/poi-ooxml-schemas/3.6")))
     (:module files :pathname ""
	      :depends-on (jars)
	      :components
	      ((:file "spreadsheet")
	       (:file "blocks"))))
    :depends-on (jss))

;;;; eof
