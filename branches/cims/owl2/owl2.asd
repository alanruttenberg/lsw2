;;;; -*- Mode: LISP -*-

;; FIXME!  Should be set before we get here...
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf)
  (require :abcl-contrib)
  (asdf:load-system :jss)
  (funcall (intern :ensure-compatibility :jss))
  (asdf:load-system :abcl-asdf))

(in-package :asdf)

(setf (logical-pathname-translations "owl2")
      `(("**;*.*" ,(make-pathname :directory (append (pathname-directory *load-pathname*)
						     '(:wild-inferiors))
				  :name :wild
				  :type :wild
				  :defaults *load-pathname*))))

(in-package :asdf)

;; taken from the Factpp manifest. I don't think the ppc entries are right, but I'll monkey it.
(defparameter *factpp-natives*
  '((("WindowsXP"  "Windows XP"  "WinXP"   "WindowsVista"  "Windows Vista"  "Windows2003"  "Windows 7")
     ("x86_64" "amd64")	  
     "64bit" "FaCTPlusPlusJNI.dll")
    (("Windows95"  "Windows 95" "Win95"  "Windows98"  "Windows 98"  "Win98"  "WindowsNT" "Windows NT"  "WinNT"  "WindowsCE"  "Winndows CE" "WinCE"  "WindowsXP"  "Windows XP" "WinXP" "WindowsVista" "Windows Vista" "Windows2003" "Windows 7")
     ("i386" "x86")
     "32bit" "FaCTPlusPlusJNI.dll")
    (("Linux")
     ("86_64" "amd64")
     "64bit" "libFaCTPlusPlusJNI.so")
    (("Linux")
     ("i386")
     "32bit" "libFaCTPlusPlusJNI.so")
    (("MacOSX"  "Mac OS X")
     ("ppc64" "x86_64")
     "64bit" "libFaCTPlusPlusJNI.jnilib")
    (("MacOSX" "Mac OS X")
     ("ppc" "i386")
     "32bit" "libFaCTPlusPlusJNI.jnilib")))

(defun factpp-native-dir (arch os)
  (third (find-if (lambda(e) (and (member arch (second e) :test 'equal)
				  (member os (first e) :test 'equal)))
		  *factpp-natives*)))

(defun factpp-library-name (arch os)
  (fourth (find-if (lambda(e) (and (member arch (second e) :test 'equal)
						  (member os (first e) :test 'equal)))
				  *factpp-natives*)))

#+nil			   
(defparameter cl-user::*factpp-jni-path*
  (let ((arch (#"getProperty" 'system "os.arch"))
	(os (#"getProperty" 'system "os.name")))
    (namestring (merge-pathnames
		 (make-pathname :directory `(:relative "lib" "factpp-native-1.5.0" ,(factpp-native-dir arch os))
				:name (pathname-name (factpp-library-name arch os))
				:type (pathname-type (factpp-library-name arch os))
				)
		 *load-pathname*))))

(defsystem :owl2
  :name "OWL"
  :author "Alan Ruttenberg"
  :license "BSD"
  :version "1.0.2"
  :components
  ((:module lib 
            :depends-on (lib/prefuse/lib
                         lib/pellet/lib
                         lib/pellet/lib/antlr
                         lib/pellet/lib/jena
                         lib/pellet/lib/jaxb)
	    :components
	    ((:jar-file "org.semanticweb.HermiT")
	     (:jar-file "owlapi-bin")
	     (:jar-file "factplusplus-1.5.0")
	     (:jar-file "elk-owlapi-041")
             #+nil
	     (:jar-directory "pellet")))
   (:module lib/pellet/lib
            :components
            ((:jar-file "aterm-java-1.6")
             (:jar-file "pellet-cli")
             (:jar-file "pellet-core")
             (:jar-file "pellet-datatypes")
             (:jar-file "pellet-dig")
             (:jar-file "pellet-el")
             (:jar-file "pellet-explanation")
             (:jar-file "pellet-jena")
             (:jar-file "pellet-modularity")
             (:jar-file "pellet-owlapi")
             (:jar-file "pellet-owlapiv3")
             (:jar-file "pellet-pellint")
             (:jar-file "pellet-query")
             (:jar-file "pellet-rules")
             #+nil
             (:jar-file "pellet-test")
             #+nil ;; Not including as we will be running LSW2 in a
                   ;; servlet context where (presumably) newer
                   ;; versions of these interfaces will be available
             (:jar-file "servlet")))
   (:module lib/pellet/lib/antlr
            :components
            ((:jar-file "antlr-runtime-3.2")))
   (:module lib/pellet/lib/jena
            :components
            ((:jar-file "arq-2.8.4")
             (:jar-file "icu4j-3.4.4")
             (:jar-file "iri-0.8")
             #+nil
             (:jar-file "jena-2.6.3-tests")
             (:jar-file "jena-2.6.3")
             #+nil
             (:jar-file "junit-4.5")
             #+nil ;; might be in use with servlet container
             (:jar-file "log4j-1.2.13")
             (:jar-file "lucene-core-2.3.1")
             #+nil ;; might be in use with servlet container
             (:jar-file "slf4j-api-1.5.8")
             #+nil ;; might be in use with servlet container
             (:jar-file "slf4j-log4j12-1.5.8")
             (:jar-file "stax-api-1.0.1")
             (:jar-file "wstx-asl-3.2.9")
             (:jar-file "xercesImpl-2.7.1")))
   (:module lib/pellet/lib/jaxb
            :components
            ((:jar-file "jaxb-api")))
   (:module lib/prefuse/lib
            :components
            ((:jar-file "prefuse")))
;;;; not including lib/pellet/lib/junit lib/pellet/lib/jetty
;;;; lib/pellet/lib/jgrapht as either they will potentially conflict
;;;; with being in Java Servlet container (e.g. jetty), or were not
;;;; deemed to be necessary (untested)
   (:module "basics"
	    :pathname ""
 	    :components
	    ((:file "owlapi3")
	     (:file "debug")
	     )
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
	     (:file "manchester-class-expression")
	     (:file "sparql")
	     (:file "graph")
	     (:file "weaken")
	     (:file "local")
	     (:file "module")
	     (:file "inferred-axioms")
	     (:file "parse-functional-syntax")
	     (:file "create-external-derived")
	     (:file "terminal-alternate-symbols")
	     (:file "jena")
	     (:file "label-source")
	     (:file "owl-to-lisp-syntax");
	     (:file "clean-subclass-tree")
	     (:file "violations")
	     (:file "to-owlapi-class-expression")
	     (:file "text-classtree")
	     )
	    :depends-on (matcher basics)
	    ))
  :depends-on (util inspect xmls))

;;;; eof
