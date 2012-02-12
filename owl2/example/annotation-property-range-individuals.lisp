;;; sample code to look up, given an annotation property, and assuming
;;; that it has a range that is an OWL enumerated class, what the
;;; labels and individuals are in that enumeration.

(defparameter *test-ontology*
  (with-ontology test (:collecting t)
      ((asq 
       (declaration (class !choices))
       (declaration (named-individual !choice-1))
       (declaration (named-individual !choice-2))
       (declaration (named-individual !choice-3))
       (annotation-assertion !choices !rdfs:label "Choices")
       (annotation-assertion !rdfs:label !choice-1 "Choice 1")
       (annotation-assertion !rdfs:label !choice-2 "Choice 2")
       (annotation-assertion !rdfs:label !choice-3 "Choice 3")
       (equivalent-classes !choices (object-one-of !choice-1 !choice-2 !choice-3))
       (declaration (annotation-property !has-choice))
       (annotation-assertion !rdfs:label !has-choice "Has choice")
       (annotation-property-range !has-choice !choices)))
    test))

(defun choices-for-annotaton-property (property ontology manager &optional (factory (#"getOWLDataFactory" manager)))
  (let* ((axioms (#"getReferencingAxioms" (#"getOWLAnnotationProperty" factory (to-iri property)) (v3kb-ont ontology)))
	 (range 
	  (loop with iterator = (#"iterator" axioms)
	     while (#"hasNext" iterator)
	     for axiom = (#"next" iterator)
	     when (jinstance-of-p axiom (find-java-class "org.semanticweb.owlapi.model.OWLAnnotationPropertyRangeAxiom"))
	     return (when (#"getRange" axiom)
		      (#"getOWLClass" factory (#"getRange" axiom))))))
    (print range)
    (when range
      (loop with axs = (#"getReferencingAxioms" range (v3kb-ont ontology))
	 with iterator = (#"iterator" (progn (print (#"size" axs)) (print axs)))
	 while (#"hasNext" iterator)
	 for axiom = (print-db (#"next" iterator))
	 when (and (jinstance-of-p axiom (find-java-class "org.semanticweb.owlapi.model.OWLEquivalentClassesAxiom"))
		   (loop with iterator = (#"iterator" (#"getClassExpressions" axiom))
		      while (#"hasNext" iterator)
		      thereis (jinstance-of-p (#"next" iterator) (find-java-class "org.semanticweb.owlapi.model.OWLObjectOneOf"))))
	 return
	 (loop with iterator = (#"iterator" (#"getIndividualsInSignature" (setq @ (print axiom))))
	    while (#"hasNext" iterator)
	    for individual = (print (#"next" iterator))
	    collect
	    (let ((it  (#"next" (#"iterator" 
				 (#"getAnnotations" individual (v3kb-ont ontology) (#"getOWLAnnotationProperty" factory (to-iri !rdfs:label)))))))
	      (list (#"getLiteral" (#"getValue" it)) (#"toString" (#"getIRI" individual))))
	    )))))

#|
 (choices-for-annotaton-property !has-choice *test-ontology* (v3kb-manager *test-ontology*))
  -> (("Choice 3" "http://example.com/choice-3")
      ("Choice 2" "http://example.com/choice-2")
      ("Choice 1" "http://example.com/choice-1"))
|#