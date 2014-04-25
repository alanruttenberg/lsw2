;;;; -*- Mode: LISP -*-
;;;;

(in-package :asdf)

(setf (logical-pathname-translations "xpath")
      `(("**;*.*" ,(make-pathname :directory (append (pathname-directory *load-pathname*)
						     '(:wild-inferiors))
				  :name :wild
				  :type :wild))))

(in-package :asdf)

(defsystem :xpath-xalan
  :name ""
  :author "Alan Ruttenberg"
  :version "1.0.1"
  :licence "BSD"
  :components
  ((:module xalan-j_2_7_1
	    :components
	    ((:jar-file "serializer")
             (:jar-file "xalan")
             (:jar-file "xercesImpl")
             (:jar-file "xml-apis")
             (:jar-file "xsltc")))
   (:module lisp
	    :pathname ""
	    :components
	    ((:file "xpath")
	     (:file "loop")))))

;;;; eof
 
