(in-package :cl-user)

;; Given an ontology and a list of URIs, return a module of the ontology that has given terms
(defun create-module-given-terms (ontology terms &key dest (module-type "STAR")
						(module-uri (#"get" (#"getOntologyIRI" (#"getOntologyID" (v3kb-ont ontology))))))
  (let* ((manager (#"createOWLOntologyManager" 'org.semanticweb.owlapi.apibinding.OWLManager))
	 (extractor (new 'SyntacticLocalityModuleExtractor manager (v3kb-ont ontology)   (get-java-field 'modularity.ModuleType module-type))))
    (let* ((owlapi-signature
	     (loop for el in terms
		   for entry = (gethash el (v3kb-uri2entity ontology))
		   when entry collect (caar entry)))
	  (sig (list-to-java-set owlapi-signature)))
      (let* ((extracted (#"extractAsOntology" extractor sig (to-iri module-uri)))
	     (changes (new 'arraylist))
	     (extracted-ont
	       (make-v3kb :name module-uri
			  :manager manager
			  :ont extracted
			  )))
    	(let ((entities (loop for entity in (append (set-to-list (#"getObjectPropertiesInSignature" extracted))
						    (set-to-list (#"getDataPropertiesInSignature" extracted))
						    (set-to-list (#"getAnnotationPropertiesInSignature" extracted))
						    (set-to-list (#"getClassesInSignature" extracted)))
			      with table = (make-hash-table :test 'equal)
			      do (setf (gethash (#"toString" (#"getIRI" entity)) table )
				       t)
			      finally (return table))))
	  (each-axiom ontology 
	      (lambda(ax)
		(axiom-typecase ax
		  (:AnnotationAssertion
		   (when (gethash (#"toString" (#"getSubject" ax)) entities)
		     (#"add" changes (#"addAxiom" manager extracted ax))))
		  (:Declaration
		   (if (eq (owl-declaration-type ax) :annotation-property)
		       (#"add" changes (#"addAxiom" manager extracted ax))))))
	    t)
      (when dest
	(write-rdfxml extracted dest))
      extracted-ont)))))
#|
test

(with-ontology foo ()
  ((asq (declaration (class !a))
	(declaration (class !b))
	(declaration (class !c))
	(declaration (object-property !p1))
	(declaration (object-property !p2))
	(subclassof !a (object-some-values-from  !p1 !b))
					;(subclassof !c (object-min-cardinality !p2 2))
	))
  (pprint (owl-to-lisp-syntax foo))
  (pprint (owl-to-lisp-syntax (create-module-for-signature foo (list !a !b !p1)))))

(with-ontology ont
               (:collecting t :base "urn:lsw:ontology:foo"
                :ontology-iri !<urn:lsw:ontology:foo> :version-iri
                nil :about !<FOO>)
               ((declaration (class !ex:a))
                (declaration (class !ex:b))
                (declaration (class !ex:c))
                (declaration (object-property !ex:p1))
                (declaration (object-property !ex:p2))
                (sub-class-of !ex:a
                 (object-some-values-from !ex:p1 !ex:b))))

(with-ontology ont
               (:collecting t :base "urn:lsw:ontology:foo"
                :ontology-iri !<urn:lsw:ontology:foo> :version-iri
                nil :about !<urn:lsw:ontology:foo>)
               ((declaration (class !ex:a))
                (declaration (class !ex:b))
                (declaration (object-property !ex:p1))
                (sub-class-of !ex:a
                 (object-some-values-from !ex:p1 !ex:b))))
|#
