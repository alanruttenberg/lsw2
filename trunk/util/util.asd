;;;; -*- Mode: LISP -*-

(in-package :asdf)

(setf (logical-pathname-translations "util")
      `(("**;*.*" ,(make-pathname :directory (append (pathname-directory *load-pathname*)
						     '(:wild-inferiors))
				  :name :wild
				  :type :wild))))

(defsystem :util
  :name "misc utilities"
  :author "Alan Ruttenberg"
  :components
  ((:module macros
	    :pathname ""
	    :components ((:file "string") (:file "encapsulate"))) 
   (:module utils :pathname ""
	    :components
	    ((:jar-file "md5.jar")
	     (:file "config")
	     (:file "tests")
	     (:file "namespace")
	     (:file "uri")
	     (:file "geturl")
	     (:file "purl")
	     (:file "jss-condition")
	     (:file "xmls-helpers")
	     (:file "server")
	     (:file "mediawiki-bot")
	     (:file "backtrace-trim")
	     (:file "obo"))
	    :depends-on
	    (macros)))
  :depends-on (xptest xmls) )

;;;; eof
