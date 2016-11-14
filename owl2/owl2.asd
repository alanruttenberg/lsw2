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
			   
(defparameter cl-user::*factpp-jni-path*
  (let ((arch (#"getProperty" 'system "os.arch"))
	(os (#"getProperty" 'system "os.name")))
    (namestring (merge-pathnames
		 (make-pathname :directory `(:relative "lib" "factpp-native-1.6.4" ,(factpp-native-dir arch os))
				:name (pathname-name (factpp-library-name arch os))
				:type (pathname-type (factpp-library-name arch os))
				)
		 *load-pathname*))))

(defsystem :owl2
  :name "OWL"
  :author "Alan Ruttenberg"
  :license "BSD"
  :components
  ((:module lib 
	    :components
	    (
	     (:jar-file "org.semanticweb.hermit-1.3.8.413")
	     (:jar-file "owlapi-distribution-4.2.6")  
	     (:jar-directory "owlapi-4.2.6-dependencies")
	     (:jar-file "factplusplus-1.6.4-SNAPSHOT")
;	     (:jar-file "elk-owlapi-041")
;	     (:jar-directory "jena")
;	     (:jar-file "uncommons-maths-1.2.2")
	     (:jar-directory "pelletcli/lib")
;;	     (:jar-file "OWL-BGP-0.1")
	     (:jar-file "elk-owlapi-standalone-0.5.0-SNAPSHOT-bin")
;;	     (:jar-directory "jfact")
	     (:jar-directory "explanation")
;	     (:jar-file "owlexplanation-1.1.1-SNAPSHOT")
	     (:jar-file "telemetry-1.0.0") ; should be in owlexplanation :(
;	     (:jar-file "owlapitools-atomicdecomposition-1.1.1") ; chainsaw
;	     (:jar-file "owlapitools-concurrentimpl-1.1.1") ;chainsaw
;	     (:jar-file "Chainsaw-1.0-SNAPSHOT")
;	     (:jar-file "jfact-1.2.3")
	     (:jar-directory "prefuse")
	     ))
   ;; only if we aren't loading pellet
   (:module jena
	    :pathname "lib/pelletcli/lib/"
	    :components
	     ((:jar-file "jena-arq-2.10.1")
	     (:jar-file "jena-core-2.10.1")
	     (:jar-file "jena-iri-0.9.6"))
	     )
   (:module "basics"
	    :pathname ""
 	    :components
	    ((:file "owlapi")
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
	     (:file "domain-and-range")
	     )
	    :depends-on (matcher basics)
	    ))
  :depends-on (util xmls))

;;;; eof
