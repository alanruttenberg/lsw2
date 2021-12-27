(in-package :cl-user)

;; http://owlapi.svn.sourceforge.net/viewvc/owlapi/v3/trunk/examples/src/main/java/org/coode/owlapi/examples/Example11.java?view=markup

(defparameter *inferred-axiom-types*
  '((:class-assertions InferredClassAssertionAxiomGenerator)
    (:property-assertions InferredPropertyAssertionGenerator)
    
    (:disjoint-classes owlapi.util.InferredDisjointClassesAxiomGenerator)
    (:equivalent-classes InferredEquivalentClassAxiomGenerator)
    (:subclasses InferredSubClassAxiomGenerator)

    (:equivalent-data-properties InferredEquivalentDataPropertiesAxiomGenerator)
    (:equivalent-object-properties InferredEquivalentObjectPropertyAxiomGenerator)
    (:inverse-properties InferredInverseObjectPropertiesAxiomGenerator)
    (:sub-data-properties InferredSubDataPropertyAxiomGenerator)
    (:sub-object-properties InferredSubObjectPropertyAxiomGenerator)
    
    (:data-property-characteristics InferredDataPropertyCharacteristicAxiomGenerator)
    (:object-property-characteristics InferredObjectPropertyCharacteristicAxiomGenerator)))

(defparameter *all-inferred-axiom-types* (mapcar 'car *inferred-axiom-types*))

;; (ontology-with-inferred-axioms source-ont &key types)
;; where types is take from *inferred-axiom-types*
;; returns a java OWLOntologyInstance (pack this up later)
;; e.g 
;; (setq iao (load-ontology "http://purl.obolibrary.org/obo/iao.owl"))
;; (#"size" (#"getAxioms" (ontology-with-inferred-axioms iao :subclasses))) -> 159

(defun add-inferred-axioms (source-ont &key to-ont (types *all-inferred-axiom-types*))
  (let* ((manager (unless to-ont (#"createOWLOntologyManager" 'org.semanticweb.owlapi.apibinding.OWLManager)))
	 (inf-ont (if to-ont (v3kb-ont to-ont) (#"createOntology" manager)))
	 (generators (new 'arraylist)))
    (loop for type in types
	  for found = (second (assoc type *inferred-axiom-types*))
	  unless found do (error "don't know inferred axiom type ~a?")
	    do (#"add" generators (new found)))
    (unless (v3kb-reasoner source-ont) (instantiate-reasoner source-ont))
    (check-ontology source-ont)
    (let ((filler (new 'InferredOntologyGenerator (v3kb-reasoner source-ont) generators)))
      (#"fillOntology" filler (v3kb-datafactory source-ont) inf-ont)
      inf-ont)))

(defun test-inferred-axioms-1 ()
  (with-ontology foo (:collecting t)
		 ((asq (declaration (class !a))
		       (declaration (class !b))
		       (declaration (class !c))
		       (equivalent-classes !c (object-union-of !a !b))))
    (check-ontology foo :classify t :reasoner :factpp)
    (each-axiom (add-inferred-axioms foo :types '((:subclasses)))
		(lambda(e) 
		  (print-db e
			    ;; workaround over-eager print-db-hook that calls eval-uri-reader-macro
			    (funcall 'make-uri (#"toString" (#"getIRI" (#"getSubClass" e))))
			    (#"toString" e))
		))
    ))

(defun write-ontology-with-inferred-axioms (ont ont-iri &optional file)
  (let* ((inf (add-inferred-axioms ont  :types '(:subclasses)))
	 (manager (#"getOWLOntologyManager" inf))
	 (it (make-v3kb :name ont-iri 
			:manager manager
			:ont  inf
			:datafactory  (#"getOWLDataFactory" manager)
			:default-reasoner :factpp
			:mapper nil)))
    (setf (v3kb-uri2entity it) (compute-uri2entity it))
    (with-ontology foo (:ontology-iri ont-iri)
		   ((each-axiom inf
				(lambda(e) 
				  (let ((sub (make-uri (#"toString" (#"getIRI" (#"getSubClass" e)))))
					(super (make-uri (#"toString" (#"getIRI" (#"getSuperClass" e))))))
				    (as `(subclass-of ,sub ,super))))))
      (to-owl-syntax foo :rdfxml file))))

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
    (each-axiom (add-inferred-axioms foo :types '(:equivalent-object-properties))
		(lambda(e) (print (#"toString" e))))
    ))

;; Returns a list of inferred axioms of the given types
(defun get-inferred-axioms (ont  &key (types *all-inferred-axiom-types*))
  (loop for type in types
	for found = (second (assoc type *inferred-axiom-types*))
	unless found do (error "don't know inferred axiom type ~a?")
	  append (set-to-list (#"createAxioms" (new found) (v3kb-datafactory ont) (v3kb-reasoner ont)))))

;; Returns a list of inferred axioms  of the given types as sexps
;; e.g. (get-inferred-axioms-as-sexps f :types '(:property-assertions ))
(defun get-inferred-axioms-as-sexps (ont &key (types *all-inferred-axiom-types*))
  (mapcar 'axiom-to-lisp-syntax (get-inferred-axioms ont :types types)))
