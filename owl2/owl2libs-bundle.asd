(defun open-osgi-for-lsw ()
  (loop for wedge in '("org.osgi.framework.system.packages.extra" "org.osgi.framework.bootdelegation")
	do (jss::add-to-comma-separated-osgi-config
	    wedge
	    '("com.google.common.collect" "com.google.common.collect.*"
             "gnu.trove" "gnu.trove.*" "javax.swing" "javax.swing.*"
	     "javax.xml.datatype" "javax.xml.datatype.*" "org.apache.log4j" "org.apache.log4j.*"
	     "org.semanticweb.owlapi.model" "org.semanticweb.owlapi.model.*"
	     "org.semanticweb.owlapi.reasoner" "org.semanticweb.owlapi.reasoner.*"
	     "org.semanticweb.owlapi.reasoner.impl" "org.semanticweb.owlapi.reasoner.impl.*"
	     "org.semanticweb.owlapi.reasoner.knowledgeexploration" "org.semanticweb.owlapi.reasoner.knowledgeexploration.*"
	     "org.semanticweb.owlapi.util" "org.semanticweb.owlapi.util.*"
	     "org.semanticweb.owlapi.vocab" "org.semanticweb.owlapi.vocab.*"))))

(defvar cl-user::*before-osgi-starting-hooks* nil)
(pushnew 'open-osgi-for-lsw cl-user::*before-osgi-starting-hooks*)

(asdf:defsystem owl2libs-bundle
  :description "Non-Lisp dependencies necessary for OWL to function."
  :components
  ((:module compatibility
    :description "A chance to load versions of artifacts first."
    :components ((:mvn "org.slf4j/slf4j-api/1.7.21")))
   (:module owlapi-java :components
            ((:mvn "net.sourceforge.owlapi/owlapi-distribution/4.2.6")
             (:mvn "net.sourceforge.owlapi/owlexplanation/2.0.0")))
   (:module reasoner/pellet :components
            ((:mvn "net.sourceforge.owlapi/pellet-cli-ignazio1977/2.4.0-ignazio1977")))
   (:module reasoner/elk :pathname "lib" :components
            ((:mvn "org.semanticweb.elk/elk-reasoner/0.4.3")))
   (:module reasoner/hermit :pathname "lib" :components
            ((:mvn "net.sourceforge.owlapi/org.semanticweb.hermit/1.3.8.413")))
   (:module reasoner/fact++ :pathname "lib" :components
	    ;; https://bitbucket.org/dtsarkov/factplusplus/downloads/uk.ac.manchester.cs.owl.factplusplus-P5.x-v1.6.5.jar
	    ;; Needs to be "de-fanged" by removing the Required-Bundles: header in the manifest file
	    ;; But at least it loads the natives by itself.
            ((:bundle "uk.ac.manchester.cs.owl.factplusplus-1.6.5")))
   (:module prefuse :pathname "lib" :components
	    ((:mvn "de.sciss/prefuse-core/1.0.1")
	     (:mvn "de.sciss/prefuse-demos/1.0.1")
	     (:jar-file "LSWTreeview-1.0.0")
	     (:jar-file "QuotedStringAnnotationVisitor-1.0.0")))
   (:module lib :pathname "lib"
    :depends-on (compatibility owlapi-java reasoner/pellet reasoner/fact++ reasoner/hermit reasoner/elk prefuse))))


