;; WIP see https://github.com/matthewhorridge/owlexplanation

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
    (let* ((egf (new 'InconsistentOntologyExplanationGeneratorFactory (get-reasoner-factory ontology) (new 'Long "10000"))) ;; milliseconds
	   (eg (#"createExplanationGenerator" egf (v3kb-ont ontology))))
      (set-to-list (#"getExplanations" eg (subclassof-axiom !owl:Thing !owl:Nothing ontology) 2))))


