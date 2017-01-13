;;;; -*- Mode: LISP -*-
;;;;

(in-package :asdf)

(setf (logical-pathname-translations "xpath")
      `(("**;*.*" ,(make-pathname :directory (append (pathname-directory *load-pathname*)
						     '(:wild-inferiors))
				  :name :wild
				  :type :wild))))

(in-package :asdf)

(defsystem :xpath-lsw
  :name ""
  :author "Alan Ruttenberg"
  :licence "BSD"
  :components
  ((:module jars
    :pathname ""
    :components
    ((:mvn "xalan/xalan/2.7.1")
     ))
   (:module lisp
    :pathname ""
    :serial t
    :components
    ((:file "xpath")
     (:file "loop")
     )
    :depends-on (jars))))

;;;; eof
 
