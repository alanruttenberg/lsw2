(defun simple-subclassof-axiom? (ax)
  (and (jinstance-of-p ax (find-java-class 'OWLSubClassOfAxiom ))
       (= (#"size" (#"getClassesInSignature" ax)) 2)
       (every (lambda(e) (jinstance-of-p e (find-java-class 'OWLClass))) (set-to-list (#"getClassExpressions" ax)))))

;; note bug https://sourceforge.net/tracker/index.php?func=detail&aid=2975093&group_id=90989&atid=595534
;; imports declarations not an axiom type and not found by get-axioms.

(defmacro axiom-typecase (axiom &body clauses)
  (let ((axiomv (make-symbol "AXIOM")))
    `(let ((,axiomv ,axiom))
	  (case (intern (#"getName" (#"getAxiomType" ,axiomv)) 'keyword)
	    ,@(loop for (types . body) in clauses
		   collect
		   (if (eq types 'otherwise)
		       (cons types body)
		       (list* (loop for type in (if (atom types) (list types) types)
				 for found = 
				   (find (string type)
					 (mapcar (lambda(e) (intern (#"getName" e) 'keyword))
						 (set-to-list (get-java-field 'org.semanticweb.owlapi.model.AxiomType "AXIOM_TYPES")))
					 :key  'string :test 'equalp)
				   unless found do (error "Didn't find axiom type ~a" type)
				   collect found)
			      body)))))))

(defun owl-declaration-type (declaration-axiom)
  (let ((class (jobject-class (#"getEntity" declaration-axiom))))
    (cond ((equal class (load-time-value (find-java-class "uk.ac.manchester.cs.owl.owlapi.OWLAnnotationPropertyImpl")))
	   :annotation-property)
	  ((equal class (load-time-value (find-java-class "uk.ac.manchester.cs.owl.owlapi.OWLObjectPropertyImpl")))
	   :object-property)
	  ((equal class (load-time-value (find-java-class "uk.ac.manchester.cs.owl.owlapi.OWLDataPropertyImpl")))
	   :data-property)
	  ((equal class (load-time-value (find-java-class "uk.ac.manchester.cs.owl.owlapi.OWLClassImpl")))
	   :class)
	  ((equal class (load-time-value (find-java-class "uk.ac.manchester.cs.owl.owlapi.OWLNamedIndividualImpl")))
	   :individual)
	  ((equal class (load-time-value (find-java-class "uk.ac.manchester.cs.owl.owlapi.OWLDatatypeImpl")))
	   :datatype)
	  (t (error "don't know what kind of declaration for ~a" (#"toString" declaration-axiom))))))

(defun remove-axiom (axiom ont)
  (let ((changes (or (v3kb-changes ont) (setf (v3kb-changes ont) (new 'arraylist)))))
    (#"addAll" changes (#"removeAxiom" (v3kb-manager ont) (v3kb-ont ont) axiom))))

(defun add-axiom (axiom ont)
  (let ((changes (or (v3kb-changes ont) (setf (v3kb-changes ont) (new 'arraylist)))))
    (#"addAll" changes (#"addAxiom" (v3kb-manager ont) (v3kb-ont ont) axiom))))

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