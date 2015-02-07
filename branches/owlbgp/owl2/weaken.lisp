(defun weaken-to-only-subclasses (ont &optional new-ontology-name &key (keep-class-assertions t))
  (let ((target (and new-ontology-name
		     (let* ((manager (#"createOWLOntologyManager" 'OWLManager))
			    (ontology (#"createOntology" manager)))
		       (make-v3kb :name new-ontology-name
				  :manager manager
				  :ont ontology
				  :datafactory (#"getOWLDataFactory" manager)
				  :weakened-from ont)))))
    (unwind-protect
	 (each-axiom
	  ont
	  (lambda(axiom)
	    (axiom-typecase axiom
	      (SubClassOf
	       (if (simple-subclassof-axiom? axiom)
		   (when target (add-axiom axiom target))
		   (when (not target) (remove-axiom axiom ont))))
	      ((Declaration AnnotationAssertion)
	       (when target (add-axiom axiom target)))
	      (ClassAssertion
	       (when keep-class-assertions
		 (if (jinstance-of-p (#"getClassExpression" axiom) (find-java-class 'OWLClass))
		   (when target (add-axiom axiom target))
		   (when (not target) (remove-axiom axiom ont)))))
	      (otherwise (when (not target) (remove-axiom axiom ont)))))
	  t)
      (unwind-protect (and (v3kb-changes (or target ont))
			   (#"applyChanges"  (v3kb-manager (or target ont)) (v3kb-changes (or target ont))))
	(setf (v3kb-changes (or target ont)) nil)))
    (or target ont)))

(defun weaken-remove-data-property-assertions (ont &optional new-ontology-name)
  (let ((target (and new-ontology-name
		     (let* ((manager (#"createOWLOntologyManager" 'OWLManager))
			    (ontology (#"createOntology" manager)))
		       (make-v3kb :name new-ontology-name
				  :manager manager
				  :ont ontology
				  :datafactory (#"getOWLDataFactory" manager)
				  :weakened-from ont)))))
    (unwind-protect
	 (each-axiom
	  ont
	  (lambda(axiom)
	    (axiom-typecase axiom
	      (DataPropertyAssertion
	       (format t ".")
	       (remove-axiom axiom ont))
	      (otherwise (print (#"toString" axiom)))
	      ))
	  t)
      (unwind-protect (and (v3kb-changes (or target ont))
			   (#"applyChanges"  (v3kb-manager (or target ont)) (v3kb-changes (or target ont))))
	(setf (v3kb-changes (or target ont)) nil)))
    (or target ont)))
