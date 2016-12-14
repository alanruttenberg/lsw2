(asdf:defsystem owl2libs-mvn
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

   


                         
