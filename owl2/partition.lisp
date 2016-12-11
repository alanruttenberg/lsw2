(defun split-deprecated (ont &optional new-ontology-name new-deprecated-name)
  "modify ont or create a new ontology in which the equivalentclasses axioms are turned into subclassof axioms"
  (let ((main-target (and new-ontology-name
			  (let* ((manager (#"createOWLOntologyManager" 'OWLManager))
				 (ontology (#"createOntology" manager)))
			    (make-v3kb :name new-ontology-name
				       :manager manager
				       :ont ontology
				       :datafactory (#"getOWLDataFactory" manager)
				       ))))
	(deprecated-target (and new-ontology-name
				(let* ((manager (#"createOWLOntologyManager" 'OWLManager))
				       (ontology (#"createOntology" manager)))
				  (make-v3kb :name new-deprecated-name
					     :manager manager
					     :ont ontology
					     :datafactory (#"getOWLDataFactory" manager)
					     )))))
    (unwind-protect
	 (each-axiom
	  ont
	  (lambda(axiom)
	      (axiom-typecase axiom
		(subclassof
		 (if (and (jinstance-of-p (#"getSubClass" axiom) (find-java-class 'OWLClass))
			  (or (jinstance-of-p (#"getSuperClass" axiom) (find-java-class 'OWLObjectMinCardinality))
			      (jinstance-of-p (#"getSuperClass" axiom) (find-java-class 'OWLObjectMaxCardinality))
			      (jinstance-of-p (#"getSuperClass" axiom) (find-java-class 'OWLObjectExactCardinality))))
		     (unless target
		       (remove-axiom axiom ont))))
		(otherwise 
		 (when target (add-axiom axiom target))))))
      (unwind-protect (and (v3kb-changes (or target ont))
			   (#"applyChanges"  (v3kb-manager (or target ont)) (v3kb-changes (or target ont))))
	(setf (v3kb-changes (or target ont)) nil)))
    (unwind-protect
	 (each-axiom
	  ont
	  (lambda(axiom)
	      (axiom-typecase axiom
		(subclassof
		 (if (and (jinstance-of-p (#"getSubClass" axiom) (find-java-class 'OWLClass))
			  (or (jinstance-of-p (#"getSuperClass" axiom) (find-java-class 'OWLObjectMinCardinality))
			      (jinstance-of-p (#"getSuperClass" axiom) (find-java-class 'OWLObjectMaxCardinality))
			      (jinstance-of-p (#"getSuperClass" axiom) (find-java-class 'OWLObjectExactCardinality))))
		     (unless target
		       (remove-axiom axiom ont))))
		(otherwise 
		 (when target (add-axiom axiom target))))))
      (unwind-protect (and (v3kb-changes (or target ont))
			   (#"applyChanges"  (v3kb-manager (or target ont)) (v3kb-changes (or target ont))))
	(setf (v3kb-changes (or target ont)) nil)))
    (or target ont)))


	    
