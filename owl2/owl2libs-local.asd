(asdf:defsystem owl2libs-local
  :description "Non-Lisp dependencies necessary for OWL to function."
  :components
  (
   ;; (:module compatibility
   ;;          :description "A chance to load versions of artifacts first."
   ;;          :components ((:mvn "org.slf4j/slf4j-api/1.7.21")))
   (:module reasoner/pellet :pathname "lib" :components
            ((:jar-directory "pelletcli/lib")))
   (:module reasoner/fact++ :pathname "lib" :components
            ((:jar-file "factplusplus-1.6.4-SNAPSHOT")))
   (:module reasoner/hermit :pathname "lib" :components
            ((:jar-file "org.semanticweb.hermit-1.3.8.413")))
   (:module reasoner/elk :pathname "lib" :components
            ((:jar-file "elk-owlapi-standalone-0.5.0-SNAPSHOT-bin")))
   (:module owlapi-java :pathname "lib" :components
            ((:jar-file "owlapi-distribution-4.2.6")
	     (:jar-directory "owlapi-4.2.6-dependencies")
	     (:jar-directory "explanation")
	     (:jar-file "telemetry-1.0.0")))  ; should be in owlexplanation :(
   (:module prefuse :pathname "lib" :components
	    ((:jar-directory "prefuse")))
   (:module jena
	    :pathname "lib/pelletcli/lib/"
	    :components
	     ((:jar-file "jena-arq-2.10.1")
	     (:jar-file "jena-core-2.10.1")
	     (:jar-file "jena-iri-0.9.6"))
	     )
   (:module lib :pathname "lib"
            :depends-on ( #|compatibility|# owlapi-java
                                       reasoner/pellet reasoner/fact++ reasoner/hermit reasoner/elk))))

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

