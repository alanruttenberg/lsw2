(asdf:defsystem owl2libs-mvn
  :description "Non-Lisp dependencies necessary for OWL to function."
  :components
  ((:module compatibility
    :description "A chance to load versions of artifacts first."
    :components ((:mvn "org.slf4j/slf4j-api/1.7.21")))
   (:module reasoner/pellet :components
            ((:mvn "net.sourceforge.owlapi/pellet-cli-ignazio1977/2.4.0-ignazio1977")))
   (:module reasoner/elk :pathname "lib" :components
            ((:mvn "org.semanticweb.elk/elk-reasoner/0.4.3")))
   (:module reasoner/hermit :pathname "lib" :components
            ((:mvn "net.sourceforge.owlapi/org.semanticweb.hermit/1.3.8.413")))
   (:module reasoner/fact++ :pathname "lib" :components
            ((:bundle "uk.ac.manchester.cs.owl.factplusplus-1.6.5")))
   (:module owlapi-java :components
            ((:mvn "net.sourceforge.owlapi/owlapi-distribution/4.2.6")
             (:mvn "net.sourceforge.owlapi/owlexplanation/2.0.0")))
   (:module prefuse :pathname "lib" :components
	    ((:mvn "de.sciss/prefuse-core/1.0.1")
	     (:mvn "de.sciss/prefuse-demos/1.0.1")
	     (:jar-file "LSWTreeview-1.0.0")
	     (:jar-file "QuotedStringAnnotationVisitor-1.0.0")))
   (:module lib :pathname "lib"
    :depends-on (compatibility owlapi-java
			       reasoner/pellet reasoner/fact++ reasoner/hermit reasoner/elk))))

   


;; (in-package :cl-user)                         
;; ;; taken from the Factpp manifest. I don't think the ppc entries are right, but I'll monkey it.
;; (defparameter *factpp-natives*
;;   '((("WindowsXP"  "Windows XP"  "WinXP"   "WindowsVista"  "Windows Vista"  "Windows2003"  "Windows 7")
;;      ("x86_64" "amd64")	  
;;      "64bit" "FaCTPlusPlusJNI.dll")
;;     (("Windows95"  "Windows 95" "Win95"  "Windows98"  "Windows 98"  "Win98"  "WindowsNT" "Windows NT"  "WinNT"  "WindowsCE"  "Winndows CE" "WinCE"  "WindowsXP"  "Windows XP" "WinXP" "WindowsVista" "Windows Vista" "Windows2003" "Windows 7")
;;      ("i386" "x86")
;;      "32bit" "FaCTPlusPlusJNI.dll")
;;     (("Linux")
;;      ("86_64" "amd64")
;;      "64bit" "libFaCTPlusPlusJNI.so")
;;     (("Linux")
;;      ("i386")
;;      "32bit" "libFaCTPlusPlusJNI.so")
;;     (("MacOSX"  "Mac OS X")
;;      ("ppc64" "x86_64")
;;      "64bit" "libFaCTPlusPlusJNI.jnilib")
;;     (("MacOSX" "Mac OS X")
;;      ("ppc" "i386")
;;      "32bit" "libFaCTPlusPlusJNI.jnilib")))

;; (defun factpp-native-dir (arch os)
;;   (third (find-if (lambda(e) (and (member arch (second e) :test 'equal)
;; 				  (member os (first e) :test 'equal)))
;; 		  *factpp-natives*)))

;; (defun factpp-library-name (arch os)
;;   (fourth (find-if (lambda(e) (and (member arch (second e) :test 'equal)
;; 						  (member os (first e) :test 'equal)))
;; 				  *factpp-natives*)))
			   
;; (defparameter cl-user::*factpp-jni-path*
;;   (let ((arch (#"getProperty" 'system "os.arch"))
;; 	(os (#"getProperty" 'system "os.name")))
;;     (namestring (merge-pathnames
;; 		 (make-pathname :directory `(:relative "lib" "factpp-native-1.6.4" ,(factpp-native-dir arch os))
;; 				:name (pathname-name (factpp-library-name arch os))
;; 				:type (pathname-type (factpp-library-name arch os))
;; 				)
;; 		 *load-pathname*))))
