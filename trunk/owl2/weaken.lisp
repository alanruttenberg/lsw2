(defun simple-subclassof-axiom? (ax)
  (and (jinstance-of-p ax (find-java-class 'OWLSubClassOfAxiom ))
       (= (#"size" (#"getClassesInSignature" ax)) 2)
       (every (lambda(e) (jinstance-of-p e (find-java-class 'OWLClass))) (set-to-list (#"getClassExpressions" ax)))))

(defmacro axiom-typecase (axiom &body clauses)
  (let ((axiomv (make-symbol "AXIOM")))
    `(let ((,axiomv ,axiom))
	  (case (intern (#"getName" (#"getAxiomType" ,axiomv)) 'keyword)
	    ,@(loop for (types . body) in clauses
		   collect
		   (if (eq types 'otherwise)
		       (cons types body)
		       (list* (loop for type in (if (atom types) (list types) types)
				 collect
				   (find (string type)
					 (mapcar (lambda(e) (intern (#"getName" e) 'keyword))
						 (set-to-list (get-java-field 'org.semanticweb.owlapi.model.AxiomType "AXIOM_TYPES")))
					 :key  'string :test 'equalp))
			      body)))))))

(defun remove-axiom (axiom ont)
  (let ((changes (or (v3kb-changes ont) (setf (v3kb-changes ont) (new 'arraylist)))))
    (#"addAll" changes (#"removeAxiom" (v3kb-manager ont) (v3kb-ont ont) axiom))))

(defun add-axiom (axiom ont)
  (let ((changes (or (v3kb-changes ont) (setf (v3kb-changes ont) (new 'arraylist)))))
    (#"addAll" changes (#"addAxiom" (v3kb-manager ont) (v3kb-ont ont) axiom))))
    
(defun weaken-to-only-subclasses (ont &optional new-ontology-name)
  (let ((target (and new-ontology-name
		     (let* ((manager (#"createOWLOntologyManager" 'OWLManager))
			    (ontology (#"createOntology" manager)))
		       (make-v3kb :name new-ontology-name
				  :manager manager
				  :ont ontology
				  :datafactory (#"getOWLDataFactory" manager)
				  :weakened-from ont)))))
    (unwind-protect
	 (loop for ontology in (set-to-list (#"getImportsClosure" (v3kb-ont ont)))
	    do
	    (loop for axiom in (set-to-list (#"getAxioms" ontology))
	       do
	       (axiom-typecase axiom
		 (SubClassOf
		  (if (simple-subclassof-axiom? axiom)
		      (when target (add-axiom axiom target))
		      (when (not target) (remove-axiom axiom ont))))
		 ((Declaration AnnotationAssertion)
		  (when target (add-axiom axiom target)))
		 (ClassAssertion
		  (if (jinstance-of-p (#"getClassExpression" axiom) (find-java-class 'OWLClass))
		      (when target (add-axiom axiom target))
		      (when (not target) (remove-axiom axiom ont))))
		 (otherwise (when (not target) (remove-axiom axiom ont))))))
      (unwind-protect (and (v3kb-changes (or target ont))
			   (#"applyChanges"  (v3kb-manager (or target ont)) (v3kb-changes (or target ont))))
	(setf (v3kb-changes (or target ont)) nil)))
    (or target ont)))



