(in-package :cl-user)

(defun filter-just-branch-of-ontology (ont root)
  (let ((manager (#"createOWLOntologyManager" 'org.semanticweb.owlapi.apibinding.OWLManager)))
    (let ((keep (append (cons root (descendants root ont))
			(annotation-properties ont))))
      (flet ((signature (ax)
	       (if (eq (intern (#"getName" (#"getAxiomType" ax)) 'keyword) :|AnnotationAssertion|)
		   (list (make-uri (#"toString" (#"getSubject" ax))) (make-uri (#"toString" (#"getIRI" (#"getProperty" ax)))))
		   (mapcar #'make-uri
			   (mapcar #"toString"
				   (mapcar #"getIRI"
					   (set-to-list (#"getSignature" ax))))))))
	(each-axiom ont 
	    (lambda(ax)
	      (if (set-difference (signature ax) keep)
		  (#"removeAxiom" manager (v3kb-ont ont) ax)))))))
  ont)

(defun filter-just-subclasses (ont )
  (let* ((manager (#"createOWLOntologyManager" 'org.semanticweb.owlapi.apibinding.OWLManager))
	 (new (make-v3kb :name "foo" :ont (#"createOntology" manager) :datafactory (#"getOWLDataFactory" manager))))
    (each-axiom ont 
	(lambda(ax)
	  (axiom-typecase ax
	    (:Declaration
	     (if (eq (owl-declaration-type ax) :class)
		 (#"addAxiom"  manager (v3kb-ont new) ax)))
	    (:subclassof
	     (if (and  (jtypep (#"getSubClass" ax) 'OWLClassImpl)
		       (jtypep (#"getSuperClass" ax) 'OWLClassImpl))
		 (#"addAxiom" manager (v3kb-ont new) ax)))
	    )))
    (copy-annotations-between-ontologies ont new)
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
