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

   


                         
