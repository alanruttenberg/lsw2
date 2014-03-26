;; http://owlapi.sourceforge.net/javadoc/uk/ac/manchester/cs/owlapi/modularity/SyntacticLocalityModuleExtractor.html

(defun terms-in-ontology (kb uri)
  (let (ont)
    (mapcar (lambda(e)
	      (when (equal (#"toString" (#"getOntologyIRI" (#"getOntologyID"  e))) (uri-full uri))
		(setq ont e)))
	    (set-to-list (#"getImportsClosure" (v3kb-ont kb))))
    ont))

(defun trimmed-imports-module (ontology &key  (module-uri (#"getOntologyIRI" (#"getOntologyID" (v3kb-ont ontology))))
			       dest (module-type "STAR")
			       (include-object-properties t)
			       (include-data-properties t)
			       (include-annotation-properties t)
			       )
  (let* ((manager (#"createOWLOntologyManager" 'org.semanticweb.owlapi.apibinding.OWLManager))
	 (extractor (new 'SyntacticLocalityModuleExtractor manager (v3kb-ont ontology)   (get-java-field 'moduletype module-type)))
	 (sig (let ((it (#"getClassesInSignature" (v3kb-ont ontology))))
		(and include-object-properties (#"addAll" it (#"getObjectPropertiesInSignature" (v3kb-ont ontology))))
		(and include-data-properties (#"addAll" it (#"getDataPropertiesInSignature" (v3kb-ont ontology))))
		it))
	 (extracted (#"extractAsOntology" extractor sig (to-iri module-uri)))
	 (changes (new 'arraylist))
	 (extracted-ont
	  (make-v3kb :name module-uri
		     :manager manager
		     :ont extracted
		     :datafactory (#"getOWLDataFactory" manager)
		     :weakened-from ontology)))
    (let ((entities (loop for class in (append (and include-object-properties (set-to-list (#"getObjectPropertiesInSignature" extracted)))
					       (and include-data-properties (set-to-list (#"getDataPropertiesInSignature" extracted)))
					       (and include-annotation-properties (set-to-list (#"getAnnotationPropertiesInSignature" extracted)))
					       (set-to-list (#"getClassesInSignature" extracted)))
		       with table = (make-hash-table :test 'equal)
		       do (setf (gethash (#"toString" (#"getIRI" class)) table ) t)
		       finally (return table))))
      (each-axiom ontology 
		  (lambda(ax)
		    (axiom-typecase ax
		      (:AnnotationAssertion
		       (when (gethash (#"toString" (#"getSubject" ax)) entities)
			 (#"addAll" changes (#"addAxiom" manager extracted ax))))
		      (:Declaration
		       (if (eq (owl-declaration-type ax) :annotation-property)
			   (#"addAll" changes (#"addAxiom" manager extracted ax))))
		      ))
		  t)
      (#"applyChanges"  manager changes)
      (when dest
	(write-rdfxml extracted dest)))))

(defun test-make-ndpo-module ()
  ;(save-ontology-and-imports-locally "http://ccdb.ucsd.edu/NDPO/1.0/NDPO.owl" "/Users/alanr/Desktop/save/")
  (setq ndpo (load-ontology "/Users/alanr/Desktop/save/NDPO.owl"))
  (trimmed-imports-module ndpo :dest "/Users/alanr/Desktop/ndpo-module.owl"))


;; language:
;; + : include this term
;; - : exclude this term
;; +^ : include this until root or +v
;; -^ : exclude this until root or -v

;; this divides the tree into segments, trivialy each segment could be
;; a single class.
;; Q: How to get parents back. 
;;  brute force: For each term selected assert all parents as axioms
;;  optimize: If all a terms parent are in the set, just assert those.
;; Issue: There can be multiple paths to root. 

(defun extract-terms-by-annotation (slim kb)
  (let ((seen (make-hash-table)))
    (flet ((each-node (node parents)
	     (let ((children (children node kb)))
	       (if (annotated-top node)
		   (let ((*stop* (annotation-sense node)))
		     (dolist (s (children node kb))
		       (each-node s (list* node (blocker (annotation-sense node)) parents)))))
	       (cond ((annotated-bottom node)
		      (ecase (annotation-sense node)
			(:include (note-included node parents))
			(:exclude (note-excluded node parents))))
		     ((annotated-include (note-included node (list node))))
		     ((annotated-exclude (note-excluded node (list node)))))
	       (t (dolist (s (children node kb))
		    (each-node s (list* node  parents)))))))
      )))
		      
