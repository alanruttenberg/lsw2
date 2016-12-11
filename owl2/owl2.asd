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
		 (make-pathname :directory `(:relative "lib" "factpp-native-1.6.4" ,(factpp-native-dir arch os))
				:name (pathname-name (factpp-library-name arch os))
				:type (pathname-type (factpp-library-name arch os))
				)
		 *load-pathname*))))



(asdf:defsystem owl2
  :name "OWL" :author "Alan Ruttenberg"
  :license "BSD" :version "2.0.0"
  :defsystem-depends-on (abcl-asdf util)
  :depends-on (owl2/lib)
  :components ((:module "basics" :pathname "" :components
                        ((:file "owlapi")
                         (:file "debug")))
               (:module matcher :pathname "" :components
                        ((:file "auxfns")
                         (:file "patmatch")))
               (:module translate :pathname "" :serial t :components
                        ((:file "parse-mapping-spec")
                         (:file "generate-mapping")
                         (:file "the-mapping")
                         (:file "swrl-rdf-mapping")
                         (:file "manchester-class-expression")
                         (:file "sparql")
                         (:file "sparql-owlbgp")
                         (:file "sparql-twerpish")
                         (:file "graph")
                         (:file "axioms")
                         (:file "weaken")
                         (:file "materialize-restrictions-for-triplestore")
                         (:file "explanation")
                         (:file "local")
                         (:file "module")
                         (:file "inferred-axioms")
                         (:file "create-external-derived")
                         (:file "terminal-alternate-symbols")
                         (:file "jena")
                         (:file "label-source")
                         (:file "owl-to-lisp-syntax");
                         (:file "clean-subclass-tree")
                         (:file "violations")
                         (:file "to-owlapi-class-expression")
                         (:file "text-classtree")
                         (:file "dl-query")
                         (:file "hermit-debug")
                         (:file "domain-and-range")))))

(asdf:defsystem owl2/lib
  :description "Non-Lisp dependencies necessary for OWL to function."
  :components
  ((:module compatibility
            :description "A chance to load versions of artifacts first."
            :components ((:mvn "org.slf4j/slf4j-api/1.7.21")))
   (:module reasoner/pellet :components
            ((:mvn "net.sourceforge.owlapi/pellet-cli-ignazio1977/2.4.0-ignazio1977")))
   (:module reasoner/fact++ :pathname "lib" :components
            ((:jar-file "factplusplus-1.6.4-SNAPSHOT")))
   (:module reasoner/hermit :pathname "lib" :components
            ((:jar-file "org.semanticweb.hermit-1.3.8.413")))
   (:module owlapi-java :components
            ((:mvn "net.sourceforge.owlapi/owlapi-distribution/5.0.4")
             (:mvn "edu.stanford.protege/org.semanticweb.owl.owlapi/3.4.4")
             (:mvn "net.sourceforge.owlapi/owlexplanation/2.0.0")
             ;;; FIXME (:mvn "org.semanticweb.elk/elk-owlapi-standalone /0.4.3")))
             (:mvn "net.sourceforge.owlapi/owlapi-api/5.0.4")))
   (:module lib :pathname "lib"
            :depends-on (compatibility owlapi-java
                                       reasoner/pellet reasoner/fact++ reasoner/hermit))))

   


                         
