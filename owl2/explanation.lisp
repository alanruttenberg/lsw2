;; WIP see https://github.com/matthewhorridge/owlexplanation
(in-package :cl-user)

(defun explain-inconsistency (ontology &key (max-explanations 2)  (timeout 10000))
  (let ((*default-kb* ontology))
    (let* ((egf (new 'InconsistentOntologyExplanationGeneratorFactory (get-reasoner-factory ontology) (new 'Long (prin1-to-string timeout)))) ;; milliseconds
	   (eg (#"createExplanationGenerator" egf (v3kb-ont ontology))))
      (replace-with-labels (mapcar 'justified-entailments (j2list (#"getExplanations" eg (subclassof-axiom !owl:Thing !owl:Nothing ontology) max-explanations)))))))

(defun explain-unsatisfiable-class (ontology class &key (max-explanations 2)  (timeout 10000))
  (justify-axiom ontology (subclassof-axiom class !owl:Nothing ontology) :max-explanations max-explanations :timeout timeout))

(defun justify-axiom (ontology axiom &key (max-explanations 2)  (timeout 10000))
  (let* ((egf (#"createExplanationGeneratorFactory" 'api.ExplanationManager (get-reasoner-factory ontology))))
    (let ((eg (#"createExplanationGenerator" egf (v3kb-ont ontology))))
      ;; don't know why there isn't a constructor that takes timeout like with InconsistentOntologyExplanationGeneratorFactory
      (let ((checkerfactory #"{eg}.checkerFactory"))
	(setf #"{checkerfactory}.entailmentCheckTimeOutMS" (new 'Long (prin1-to-string timeout))))
      (mapcar 'justified-entailments (set-to-list (#"getExplanations" eg
								      (if (java-object-p axiom)
									  axiom
									  (to-owlapi-axiom axiom ontology))
								      max-explanations))))))

(defparameter has-axiom-id-iri !<http://purl.obolibrary.org/obo/IAO_0010000>)

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





	 
