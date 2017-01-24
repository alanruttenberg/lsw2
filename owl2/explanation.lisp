;; WIP see https://github.com/matthewhorridge/owlexplanation

(find-java-class 'org.semanticweb.owl.explanation.impl.blackbox.checker.InconsistentOntologyExplanationGeneratorFactory)

(defun explain-inconsistency (ontology)  
  (let* ((egf (new 'InconsistentOntologyExplanationGeneratorFactory (get-reasoner-factory ontology) (new 'Long "10000"))) ;; milliseconds
	 (eg (#"createExplanationGenerator" egf (v3kb-ont ontology))))
    (set-to-list (#"getExplanations" eg (subclassof-axiom !owl:Thing !owl:Nothing ontology) 2))))

(defun explain-unsatisfiable-class (ontology class)  
  (let* ((egf (#"createExplanationGeneratorFactory" 'api.ExplanationManager (get-reasoner-factory ontology)))
	 (eg (#"createExplanationGenerator" egf (v3kb-ont ontology))))
    (mapcar 'justified-entailments (set-to-list (#"getExplanations" eg (subclassof-axiom class !owl:Nothing ontology) 2)))))

(defun test-inconsistency-explanation ()
  (with-ontology ontology (:collecting t)
      ((asq (subclassof !a !owl:Nothing )
	    (subclassof !owl:Thing !a) ))
    (setf (v3kb-default-reasoner ontology) :hermit)
    (let* ((egf (new 'blackbox.checker.InconsistentOntologyExplanationGeneratorFactory (get-reasoner-factory ontology) (new 'Long "10000"))) ;; milliseconds
	   (eg (#"createExplanationGenerator" egf (v3kb-ont ontology))))
      (mapcar 'justified-entailments (iterable-to-list (#"getExplanations" eg (subclassof-axiom !owl:Thing !owl:Nothing ontology) 2))))))

(defparameter has-axiom-id-iri !<http://purl.obolibrary.org/obo/IAO_0010000>)

;; todo express in manchester with labels
(defun justified-entailments (explanation &optional (kb *default-kb*))
  
  (let* ((manager (if kb (v3kb-manager kb) (#"createOWLOntologyManager" 'OWLManager)))
	 (df (if kb (v3kb-datafactory kb) (#"getOWLDataFactory" manager)))
	 (entailmentMarkerAnnotationProperty (#"getOWLAnnotationProperty" df #"{explanation}.ENTAILMENT_MARKER_IRI"))
	 (entailmentAnnotation (#"getOWLAnnotation"  df entailmentMarkerAnnotationProperty (#"getOWLLiteral" df +true+)))
	 (axiom (#"getAxiomWithoutAnnotations"
		 (#"getAnnotatedAxiom"
		 (#"getEntailment" explanation) 
		 (#"singleton" 'Collections entailmentAnnotation))))
	 (entailment (axiom-to-lisp-syntax axiom))
	 (explanation-axioms  (setq @ (set-to-list (#"getAxioms" explanation))))
	 (support (mapcar 'axiom-to-lisp-syntax explanation-axioms)))
    (loop for axiom in explanation-axioms
	  (entity-annotations  has-axiom-id-iri 
    
    `(:entailment ,entailment
      :support ,support
      :signature ,(axioms-signature support))))

(defun axioms-signature (axioms)
  (let ((them nil)) 
    ;; hack tree-walk - always return nil for remove
    (tree-remove-if 
     (lambda(e) (when (and (atom e) (not (symbolp e))) (pushnew e them)) nil)
     axioms)
    them))





	 
