;;;; -*- Mode: LISP -*-
;;;;

(in-package :asdf)

(setf (logical-pathname-translations "owl2")
      `(("**;*.*" ,(make-pathname :directory (append (pathname-directory *load-pathname*)
						     '(:wild-inferiors))
				  :name :wild
				  :type :wild
				  :defaults *load-pathname*))))

(in-package :asdf)

(defparameter cl-user::*factpp-jni-path*
  (namestring (merge-pathnames
	       (make-pathname :directory '(:relative "lib")
			      :name "libFaCTPlusPlusJNI"
			      :type "jnilib")
	       *load-pathname*)))

(defsystem :owl2
  :name "OWL"
  :author "Alan Ruttenberg"
  :license "BSD"
  :components
  ((:module lib
	    :components
	    ((:jar-file "org.semanticweb.HermiT.jar")
	     (:jar-file "owlapi-bin.jar")
	     (:jar-file "factplusplus-1.3.1.jar")
	     (:jar-directory "pellet")
	     (:jar-directory "prefuse")
	     ))
   (:module "basics"
	    :pathname ""
 	    :components
 	    ((:file "owlapi3")
	     (:file "debug"))
 	    :depends-on (lib))
   (:module matcher
	    :pathname ""
	    :components
	    ((:file "auxfns")
	     (:file "patmatch")))
   (:module translate
	    :pathname ""
	    :serial t
	    :components
	    ((:file "parse-mapping-spec")
	     (:file "generate-mapping")
	     (:file "the-mapping")
	     (:file "swrl-rdf-mapping")
	     (:file "sparql")
	     (:file "graph")
	     (:file "weaken")
	     (:file "parse-functional-syntax")
	     (:file "terminal-alternate-symbols"))
	    :depends-on (matcher basics)
	    ))
  :depends-on (util inspect xmls))

;;;; eof
