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
  (pathname-name (fourth (find-if (lambda(e) (and (member arch (second e) :test 'equal)
						  (member os (first e) :test 'equal)))
				  *factpp-natives*))))
			   
(defparameter cl-user::*factpp-jni-path*
  (let ((arch (#"getProperty" 'system "os.arch"))
	(os (#"getProperty" 'system "os.name")))
    (namestring (merge-pathnames
		 (make-pathname :directory `(:relative "lib" "factpp-native-1.4.0" ,(factpp-native-dir arch os))
				:name (factpp-library-name arch os)
				:type "jnilib")
		 *load-pathname*))))

(defsystem :owl2
  :name "OWL"
  :author "Alan Ruttenberg"
  :license "BSD"
  :components
  ((:module lib
	    :components
	    ((:jar-file "org.semanticweb.HermiT.jar")
	     (:jar-file "owlapi-bin.jar")
	     (:jar-file "factplusplus-1.4.0.jar")
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
