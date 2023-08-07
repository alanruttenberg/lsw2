(in-package cl-user)

;; http://owlapi.sourceforge.net/javadoc/uk/ac/manchester/cs/owlapi/modularity/SyntacticLocalityModuleExtractor.html

;; Takes as input an ontology or the pathname of uri of an ontology,
;; loading if the latter.  Computes the signature, not including any
;; imports, then creates a module (an ontology) with that signature A
;; module is a reduction of an ontology such that any query involving a
;; term in the signature will have the same answer as if done against
;; the full ontology. All the annotations of terms in the module are
;; copied from the original ontology.

;; If dest is a pathname, writes the new ontology to dest.
;; If dest is t and the source of the ontology is the local file system, writes the
;; ontology module to the file with "-module" appended in the same directory

;; Module type can be "STAR", "TOP", and "BOTTOM". I've found STAR to be
;; most useful.

;; By default all classes, object properties and data properties are
;; included in the signature. use :include-data-properties or
;; :include-object-properties as nil to disable either.

;; If module-iri is past an iri then the module ontology iri is set to
;; that, otherwise its the same as the original.

;; By default, any terms that are brought into the module but not in the original
;; are labeled by their source (as heuristically determined based on their IRI)
;; Use :label-imported nil to override that behavior

(defun trimmed-imports-module (ontology &key (module-iri )
			                  dest (module-type "STAR")
			                  (include-object-properties t)
			                  (include-data-properties t)
                                          (label-imported t)
			                  )
  (when (stringp ontology)
    (setq ontology (load-ontology ontology)))
  (when (eq dest t)
    (let ((source (ignore-errors (probe-file (v3kb-source ontology)))))
      (if (and source (pathname-host source))
          (error "can't default dest because source isn't a pathname")
          (setq dest (merge-pathnames (make-pathname :name (format nil "~a-module" (pathname-name source))) source)))))
  (let* ((sig (let ((it (#"getClassesInSignature" (v3kb-ont ontology))))
		(and include-object-properties (#"addAll" it (#"getObjectPropertiesInSignature" (v3kb-ont ontology))))
		(and include-data-properties (#"addAll" it (#"getDataPropertiesInSignature" (v3kb-ont ontology))))
		it)))
    (let ((result (create-module-for-signature ontology sig :dest dest :module-iri module-iri :module-type module-type
                                               :label-imported label-imported)))
      (when dest
        (setf (v3kb-source result) dest))
      result)))


(defun create-module-for-signature (ontology signature &key module-iri dest (module-type "STAR") (label-imported t))
  ;; Default module-iri to same as original 
  (unless module-iri
    (setq module-iri (#"get" (#"getOntologyIRI" (#"getOntologyID" (v3kb-ont ontology))))))

  ;; Do the module extraction
  (let* ((manager (#"createOWLOntologyManager" 'org.semanticweb.owlapi.apibinding.OWLManager))
	 (extractor (new 'SyntacticLocalityModuleExtractor manager (v3kb-ont ontology)
                         (get-java-field 'modularity.ModuleType module-type)))
	 (extracted (#"extractAsOntology" extractor signature (to-iri module-iri)))
	 (changes (new 'arraylist))
	 (extracted-ont
	  (make-v3kb :name module-iri
		     :manager manager
		     :ont extracted
		     :datafactory (#"getOWLDataFactory" manager)
		     :weakened-from ontology)))

    ;; collect all entities in the extracted ontology so we can copy annotations for them
    (let ((entities (loop for el in (append  (set-to-list (#"getObjectPropertiesInSignature" extracted))
					     (set-to-list (#"getDataPropertiesInSignature" extracted))
					     (set-to-list (#"getAnnotationPropertiesInSignature" extracted))
					     (set-to-list (#"getClassesInSignature" extracted)))
		          with table = (make-hash-table :test 'equal)
		          do (setf (gethash (#"toString" (#"getIRI" el)) table ) t)
		          finally (return table))))
      ;; Copy over annotations from the source ontology
      (let ((sig-as-strings (and label-imported
                                 (loop with table = (make-hash-table :test 'equalp)
                                  for el in (j2list signature)
                                  do (setf (gethash (#"toString" (#"getIRI" el)) table) t)
                                  finally (return table)))))
        (each-axiom ontology 
	    (lambda(ax)
	      (axiom-typecase ax
		(:AnnotationAssertion
		 (when (gethash (#"toString" (#"getSubject" ax)) entities)
                   (if (and (equalp "rdfs:label"
                                    (#"toString" (#"getProperty" ax)))
                            label-imported
                            (not (gethash (#"toString" (#"getSubject" ax)) sig-as-strings)))
                       (progn 
                         (#"add" changes (#"addAxiom"
                                          manager extracted
                                          (to-owlapi-axiom
                                           `(annotation-assertion ,!rdfs:label
                                                                  ,(make-uri (#"toString" (#"getSubject" ax)))
                                                                  ,(append-ontology-parenthetical (#"toString"
                                                                                                   (or (ignore-errors (#"getLiteral" (#"getValue" ax)))
                                                                                                       (#"getValue" ax)))
                                                                                                  (#"toString" (#"getSubject" ax)))
                                                                  ) extracted-ont)))
                         (#"add" changes (#"removeAxiom" manager extracted ax)))
		       (#"add" changes (#"addAxiom" manager extracted ax)))))
		(:Declaration
		 (if (eq (owl-declaration-type ax) :annotation-property)
		     (#"add" changes (#"addAxiom" manager extracted ax))))
		))
	  t))
      ;; If dest provided, write the module there
      (when dest
	(write-rdfxml extracted-ont dest))
      ;; Return the module ontology object
      extracted-ont)))

;; get an ontology label from an IRI
;; First check OBO iris, XXX_dddddd and use XXX when found
;; Otherwise use the second to last component of the IRI, where the last component is either after # or /

(defun append-ontology-parenthetical  (value subject)
  (let ((ont (or (caar (all-matches subject "(.*/([^_]+)_.*)" 2))
                 (caar (all-matches subject "(.*/(.*)[/#][^/]*)" 2)))))
    (if (equal ont "CommonCoreOntologies") (setq ont "cco"))
    (#"replaceFirst" value "[\"]?([^@\"]+)[\"]?(@[a-zA-Z-]+)?" (format nil "$1 (~a)$2" (string-upcase ont)))))

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
		      
