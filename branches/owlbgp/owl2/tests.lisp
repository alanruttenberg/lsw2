
(defun test-anonymous ()
  (let* ((manager (#"createOWLOntologyManager" 'org.semanticweb.owlapi.apibinding.OWLManager))
	 (ont (#"createOntology" manager (to-iri !<http://purl.obolibary.org/obo/obi/test/anonymous.owl>)))
	 (factory (#"getOWLDataFactory" manager)))
    (#"addAxiom" manager ont
		 (#"getOWLAnnotationAssertionAxiom" factory (#"getOWLAnnotationProperty" factory (to-iri !rdfs:label))
						    (to-iri !obo:foony)
						    (#"getOWLAnonymousIndividual" factory)))
    (#"saveOntology" manager ont (new 'fileoutputstream (new 'file "/Users/alanr/Desktop/testout.owl")))
    (let ((manager (#"createOWLOntologyManager" 'org.semanticweb.owlapi.apibinding.OWLManager)))

      (#"saveOntology" manager
		       (#"loadOntology" manager (#"create" 'org.semanticweb.owlapi.model.IRI (new 'java.io.file "/Users/alanr/Desktop/testout.owl")))
		       (new 'fileoutputstream (new 'file "/Users/alanr/Desktop/testout2.owl")))
      )))