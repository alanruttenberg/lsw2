;; http://owlapi.sourceforge.net/javadoc/uk/ac/manchester/cs/owlapi/modularity/SyntacticLocalityModuleExtractor.html

(defun terms-in-ontology (kb uri)
  (let (ont)
    (mapcar (lambda(e)
	      (when (equal (#"toString" (#"getOntologyIRI" (#"getOntologyID"  e))) (uri-full uri))
		(setq ont e)))
	    (set-to-list (#"getImportsClosure" (v3kb-ont kb))))
    ont))

(defun trimmed-imports-module (ontology module-uri &key dest (module-type "STAR"))
  (let* ((manager (#"createOWLOntologyManager" 'org.semanticweb.owlapi.apibinding.OWLManager))
	 (extractor (new 'SyntacticLocalityModuleExtractor manager (v3kb-ont ontology)   (get-java-field 'moduletype module-type)))
	 (sig (let ((it (#"getClassesInSignature" (v3kb-ont ontology))))
		(#"addAll" it (#"getObjectPropertiesInSignature" (v3kb-ont ontology)))
		(#"addAll" it (#"getDataPropertiesInSignature" (v3kb-ont ontology)))
		it))
	 (extracted (#"extractAsOntology" extractor sig (to-iri module-uri)))
	 (changes (new 'arraylist)))
    (let ((entities (loop for class in (append (set-to-list (#"getObjectPropertiesInSignature" extracted))
					       (set-to-list (#"getDataPropertiesInSignature" extracted))
					       (set-to-list (#"getClassesInSignature" extracted)))
		      with table = (make-hash-table :test 'equal)
		      do (setf (gethash (#"toString" (#"getIRI" class)) table ) t)
		      finally (return table))))
      (each-axiom ontology 
		  (lambda(ax)
		    (axiom-typecase ax
		     (:AnnotationAssertion
		      (when (gethash (#"toString" (#"getSubject" ax)) entities)
			(#"addAll" changes (#"addAxiom" manager extracted ax))))))
		  t)
      (#"applyChanges"  manager changes)
      (when dest
	(write-rdfxml extracted dest)))))

(defun test-make-ndpo-module ()
  ;(save-ontology-and-imports-locally "http://ccdb.ucsd.edu/NDPO/1.0/NDPO.owl" "/Users/alanr/Desktop/save/")
  (setq ndpo (load-ontology "/Users/alanr/Desktop/save/NDPO.owl"))
  (trimmed-imports-module ndpo !obo:ndpo/dev/module.owl :dest "/Users/alanr/Desktop/ndpo-module.owl"))
