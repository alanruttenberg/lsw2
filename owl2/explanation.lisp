;; WIP see https://github.com/matthewhorridge/owlexplanation

(find-java-class 'org.semanticweb.owl.explanation.impl.blackbox.checker.InconsistentOntologyExplanationGeneratorFactory)

(defun explain-inconsistency (ontology)  
  (let* ((egf (new 'InconsistentOntologyExplanationGeneratorFactory (get-reasoner-factory ontology) (new 'Long "10000"))) ;; milliseconds
	 (eg (#"createExplanationGenerator" egf (v3kb-ont ontology))))
    (set-to-list (#"getExplanations" eg (subclassof-axiom !owl:Thing !owl:Nothing ontology) 2))))

(defun explain-unsatisfiable-class (ontology class)  
  (let* ((egf (#"createExplanationGeneratorFactory" 'ExplanationManager (get-reasoner-factory ontology)))
	 (eg (#"createExplanationGenerator" egf (v3kb-ont ontology))))
    (set-to-list (#"getExplanations" eg (subclassof-axiom class !owl:Nothing ontology) 2))))

(defun test-inconsistency-explanation ()
  (with-ontology ontology (:collecting t)
      ((asq (subclassof !a !owl:Nothing )
	    (subclassof !owl:Thing !a) ))
    (setf (v3kb-default-reasoner ontology) :hermit)
    (let* ((egf (new 'blackbox.checker.InconsistentOntologyExplanationGeneratorFactory (get-reasoner-factory ontology) (new 'Long "10000"))) ;; milliseconds
	   (eg (#"createExplanationGenerator" egf (v3kb-ont ontology))))
      (mapcar 'justified-entailments (iterable-to-list (#"getExplanations" eg (subclassof-axiom !owl:Thing !owl:Nothing ontology) 2))))))

;; todo express in manchester with labels
(defun justified-entailments (explanation)  
  (let* ((manager (#"createOWLOntologyManager" 'OWLManager))
	 (ontology (#"createOntology" manager (#"getAxioms" explanation)))
	 (df (#"getOWLDataFactory" manager))
	 (entailmentMarkerAnnotationProperty (#"getOWLAnnotationProperty" df (to-iri !<http://owl.cs.manchester.ac.uk/explanation/vocabulary#entailment>)))
	 (entailmentAnnotation (#"getOWLAnnotation"  df entailmentMarkerAnnotationProperty (#"getOWLLiteral" df +true+))))
    `(:entailment ,(#"toString" (#"getAxiomWithoutAnnotations" (#"getAnnotatedAxiom" (#"getEntailment" explanation) (#"singleton" 'Collections entailmentAnnotation) )))
       :support ,@(mapcar #"toString" (set-to-list (#"getAxioms" explanation))))))




	 
