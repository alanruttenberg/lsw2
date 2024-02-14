(in-package :cl-user)


(defun filter-just-branch-of-ontology (ont root)
  (let* ((manager (#"createOWLOntologyManager" 'org.semanticweb.owlapi.apibinding.OWLManager)))
    (let ((keep (append (cons root (descendants root ont))
			(annotation-properties ont)
			(list !owl:Thing))))
      (flet ((signature (ax)
	       (let ((signature (get-axiom-set-signature (list (axiom-to-lisp-syntax ax)))))
		 signature)))
	(each-axiom ont 
	    (lambda(ax)
	      (when (set-difference (signature ax) keep)
		(#"removeAxiom" manager (v3kb-ont ont) ax))
	      ))))
    ont))

(defun filter-just-subclasses (ont )
  (let* ((manager (#"createOWLOntologyManager" 'org.semanticweb.owlapi.apibinding.OWLManager))
	 (new (make-v3kb :name "foo" :ont (#"createOntology" manager) :datafactory (#"getOWLDataFactory" manager))))
    (each-axiom ont 
	(lambda(ax)
	  (axiom-typecase ax
	    (:Declaration
	     (if (member (owl-declaration-type ax) '(:class :annotation-property))
		 (#"addAxiom"  manager (v3kb-ont new) ax)))
	    (:annotationassertion
	     (#"addAxiom" manager  (v3kb-ont new) ax))
	    (:subclassof
	     (if (and  (jtypep (#"getSubClass" ax) 'OWLClassImpl)
		       (jtypep (#"getSuperClass" ax) 'OWLClassImpl))
		 (#"addAxiom" manager (v3kb-ont new) ax)))
	    )))
;    (copy-annotations-between-ontologies ont new)
    (setf (v3kb-uri2entity new) (compute-uri2entity new))
    (setf (v3kb-manager new) manager)
    new))

(defun copy-annotations-between-ontologies (ont new)
  (let ((manager (#"createOWLOntologyManager" 'org.semanticweb.owlapi.apibinding.OWLManager)))
    (each-axiom ont 
	(lambda(ax)
	  (axiom-typecase ax
	    (:AnnotationAssertion
	     (#"addAxiom" manager (v3kb-ont new) ax))
	    (:Declaration
	     (if (eq (owl-declaration-type ax) :annotation-property)
		 (#"addAxiom"  manager (v3kb-ont new) ax))))))
    new))


(defun add-axioms-to-ontology (base axioms)
  (let ((manager (#"createOWLOntologyManager" 'org.semanticweb.owlapi.apibinding.OWLManager)))
    (with-ontology new ( )
      ((as axioms))			; and the facet axioms
      (each-axiom base (lambda(x) (#"addAxiom" manager (v3kb-ont new) x )))
      new)))

(defun axiom-within-signature? (terms axiom)
  (let ((signature  (mapcar #'make-uri (mapcar #"toString" (mapcar #"getIRI" (set-to-list (#"getSignature" axiom)))))))
    (not (set-difference signature terms))))

(defun get-axiom-set-signature (axioms)
  (let ((signature nil))
    (tree-walk axioms (lambda (el) (when (uri-p el) (pushnew el signature))))
    signature))

(defun merge-ontologies (options ontologies)
  (let ((it (eval `(with-ontology foo ,options () foo)))
	(manager (#"createOWLOntologyManager" 'org.semanticweb.owlapi.apibinding.OWLManager)))
    (loop for ontology in ontologies
	  do
	     (each-axiom ontology (lambda(x) (#"addAxiom" manager (v3kb-ont it) x))))
    (setf (v3kb-uri2entity it) (compute-uri2entity it))
    it))

(defun merge-ontology (ontology)
  "Take an ontology that imports others and combine all the axioms into one"
  (let ((ontology-iri (get-ontology-iri ontology))
        (version-iri (get-ontology-iri ontology)))
    (let ((new (with-ontology foo (:ontology-iri ontology-iri :version-iri version-iri) () foo))
          (manager (#"createOWLOntologyManager" 'org.semanticweb.owlapi.apibinding.OWLManager)))
      (each-axiom ontology
          (lambda(x)
            (#"addAxiom" manager (v3kb-ont new) x)) t)
      (setf (v3kb-uri2entity new) (compute-uri2entity new))
      new)))
	
	  

