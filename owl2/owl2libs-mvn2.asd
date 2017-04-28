(defpackage :asdf/owl2libs (:use :asdf :cl))
(in-package :asdf/owl2libs)

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

(pushnew 'open-osgi-for-lsw cl-user::*before-osgi-starting-hooks*)

(asdf:defsystem owl2libs-mvn2
  :description "Non-Lisp dependencies necessary for OWL to function."
  :components
  ((:mvn-module maven
		:dependencies 
		("net.sourceforge.owlapi/pellet-cli-ignazio1977/2.4.0-ignazio1977"
		  "org.semanticweb.elk/elk-owlapi/0.4.3"
		  "net.sourceforge.owlapi/org.semanticweb.hermit/1.3.8.413"
		  "net.sourceforge.owlapi/owlapi-distribution/4.2.6"
		  "net.sourceforge.owlapi/owlexplanation/2.0.0"
		  "de.sciss/prefuse-core/1.0.1"
		  "de.sciss/prefuse-demos/1.0.1")
		:managed-dependencies
		("org.slf4j/slf4j-api/1.7.21"
		 "net.sourceforge.owlapi:owlapi-distribution:4.2.6")
		:exclusions
		("net.sourceforge.owlapi:owlapi-osgidistribution"
		 "edu.stanford.protege:org.protege.editor.owl"))
   (:module rest :pathname "lib" :components
            ((:bundle "uk.ac.manchester.cs.owl.factplusplus-1.6.5")
	     (:jar-file "LSWTreeview-1.0.0")
	     (:jar-file "QuotedStringAnnotationVisitor-1.0.0")))
   (:module lib :pathname "lib"
    :depends-on (maven rest)))
    :perform (load-op :after (o c)
		      (progn
			(#"configure" 'org.apache.log4j.BasicConfigurator (jss::new 'NullAppender))
			(print "configured log4j"))))

