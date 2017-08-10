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
  ((:mvn-module maven
    :dependencies 
    ("org.apache.poi/poi/3.15"
     "org.apache.poi/poi-scratchpad/3.15"
     "org.apache.poi/poi-ooxml/3.15"
     "org.apache.poi/poi-ooxml-schemas/3.15"
     ))
   (:module files :pathname ""
    :depends-on (maven)
    :components
    ((:file "spreadsheet")
     (:file "blocks"))))
  :depends-on (jss))

;;;; eof
