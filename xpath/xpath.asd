;;;; -*- Mode: LISP -*-
;;;;

(in-package :asdf)

(setf (logical-pathname-translations "xpath")
      `(("**;*.*" ,(make-pathname :directory (append (pathname-directory *load-pathname*)
						     '(:wild-inferiors))
				  :name :wild
				  :type :wild))))

(in-package :asdf)

(defsystem :xpath
  :name ""
  :author "Alan Ruttenberg"
  :licence "BSD"
  :components
  ((:module jars
	    :pathname ""
	    :serial t
	    :components
	    ((:jar-directory "xalan-j_2_7_1")
	    ))
   (:module lisp
	    :pathname ""
	    :components
	    ((:file "xpath")
	     (:file "loop")
	     ))))

;;;; eof
 