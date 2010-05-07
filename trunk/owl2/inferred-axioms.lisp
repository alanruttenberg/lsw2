; http://owlapi.svn.sourceforge.net/viewvc/owlapi/v3/trunk/examples/src/main/java/org/coode/owlapi/examples/Example11.java?view=markup

(defparameter *inferred-axiom-types*
  '((:class-assertios InferredClassAssertionAxiomGenerator)
    (:property-assertions InferredPropertyAssertionGenerator)
    
    (:disjoint-classes InferredDisjointClassesAxiomGenerator)
    (:equivalent-classes InferredEquivalentClassAxiomGenerator)
    (:subclasses InferredSubClassAxiomGenerator)

    (:equivalent-data-properties InferredEquivalentDataPropertiesAxiomGenerator)
    (:equivalent-object-properties InferredEquivalentObjectPropertyAxiomGenerator)
    (:inverse-properties InferredInverseObjectPropertiesAxiomGenerator)
    (:sub-data-properties InferredSubDataPropertyAxiomGenerator)
    (:sub-object-properties InferredSubObjectPropertyAxiomGenerator)
    
    (:data-property-characteristics InferredDataPropertyCharacteristicAxiomGenerator)
    (:object-property-characteristics InferredObjectPropertyCharacteristicAxiomGenerator)))

;; (ontology-with-inferred-axioms source-ont &key types)
;; where types is take from *inferred-axiom-types*
;; returns a java OWLOntologyInstance (pack this up later)
;; e.g 
;; (setq iao (load-ontology "http://purl.obolibrary.org/obo/iao.owl"))
;; (#"size" (#"getAxioms" (ontology-with-inferred-axioms iao :subclasses))) -> 159

(defun ontology-with-inferred-axioms (source-ont &key types)
  (let* ((manager (#"createOWLOntologyManager" 'org.semanticweb.owlapi.apibinding.OWLManager))
	 (inf-ont (#"createOntology" manager))
	 (generators (new 'arraylist)))
    (loop for type in types
	 for found = (second (assoc type *inferred-axiom-types*))
	 unless found do (error "don't know inferred axiom type ~a?")
	 do (#"add" generators (new found)))
    (unless (v3kb-reasoner source-ont) (instantiate-reasoner source-ont))
    (check-ontology source-ont)
    (let ((filler (new 'InferredOntologyGenerator (v3kb-reasoner source-ont) generators)))
      (#"fillOntology" filler manager inf-ont)
      inf-ont)))

(defun test-inferred-axioms-1 ()
  (with-ontology foo (:collecting t)
      ((asq (declaration (class !a))
	    (declaration (class !b))
	    (declaration (class !c))
	    (equivalent-classes !c (object-union-of !a !b))))
    (check-ontology foo :classify t :reasoner :factpp)
    (each-axiom (ontology-with-inferred-axioms foo :types '(:subclasses))
		(lambda(e) (print (#"toString" e))))))

(defun test-inferred-axioms-2 (&optional (reasoner :pellet))
  (with-ontology foo (:collecting t)
      ((asq (declaration (class !a))
	    (declaration (object-property !p1))
	    (declaration (object-property !p2))
	    (object-property-range !p1 !a)
	    (object-property-domain !p1 !a)
	    (object-property-range !p2 !a)
	    (object-property-domain !p2 !a)
	    (declaration (named-individual !a1))
	    (equivalent-classes !a (object-one-of !a1))
	    (object-property-assertion !p1 !a1 !a1)
	    (object-property-assertion !p2 !a1 !a1)
	    ))
    (check-ontology foo :classify t :reasoner reasoner)
    (princ (to-owl-syntax foo :functional))
    (each-axiom (ontology-with-inferred-axioms foo :types '(:equivalent-object-properties))
		(lambda(e) (print (#"toString" e))))
    ))

