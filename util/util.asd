;;;; -*- Mode: LISP -*-

(in-package :asdf)

(require 'asdf-mvn-module)

(setf (logical-pathname-translations "util")
      `(("**;*.*" ,(make-pathname :directory (append (pathname-directory *load-pathname*)
						     '(:wild-inferiors))
				  :name :wild
				  :type :wild))))

(in-package :cl-user)

;;; Should repackage as an lsw2 dependent system, as 'util' is overloaded
(asdf:defsystem util
  :name "LSW2 utilities"
  :author "Alan Ruttenberg" :version "2.0.0"
  :depends-on (xpath-lsw xmls xptest lparallel)
  :components
  ((:mvn-module maven
    :dependencies 
    ("commons-net/commons-net/LATEST"
     ))
   (:module macros
	    :pathname ""
	    :components ((:file "string")
			 (:file "encapsulate")
			 )) 
   (:module utils :pathname ""
	    :components
	    ((:jar-file "md5")
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
;	     (:file "backtrace-trim")
	     (:file "obo")
	     (:file "jdbc")
;	     (:file "sexp-sql")
	     (:file "format-as-table")
	     (:file "collections-misc")
	     (:file "print-java-object-by-class")
	     (:file "jargrep")
	     (:file "datetime")
	     (:file "applescript")
	     (:file "browse-url")
	     (:file "eval-with-timeout")
	     (:file "run-program")
	     (:file "send-prowl-notification")
	     (:file "vagrant")
	     (:file "exit-hook")
	     (:file "tree")
	     (:file "print-time-interval")
	     (:file "background")
	     (:file "slime-extras")
	     (:file "threads")))))


