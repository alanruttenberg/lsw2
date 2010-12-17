;; Using the labels in kb, reads input file, and puts an xml comment with
;; the propertly label before properties that use an OBI id, and an xml
;; comment with the label after the use of any other entity.

;; Comment's won't persis after editing the file and saving in protege,
;; so this is something to run on release, or when you need an annotated
;; copy to do some text editing of the OWL files.

;; e.g.

;;   <owl:Class rdf:about="http://purl.obolibrary.org/obo/OBI_0000219"><!-- cellular_feature_identification_objective -->
;;     <!-- definition --><OBI_0000291 xml:lang="en">Cellular_feature_identification_objective is a biological_feature_identification_objective role describing a study designed to examine or characterize a biological feature monitored at the cellular level, e.g. stage of cell cycle, stage of differentiation.</OBI_0000291>
;;     <!-- definition source --><OBI_0000279 xml:lang="en">source pending</OBI_0000279>
;;     <!-- definition editor --><OBI_0000274 xml:lang="en">Jennifer Fostel</OBI_0000274>
;;     <!-- example of usage --><OBI_0000287 xml:lang="en">example_of_usage to be added</OBI_0000287>
;;     <!-- preferred term --><OBI_0000288 xml:lang="en">cellular_feature_identification_objective</OBI_0000288>
;;     <rdfs:subClassOf>
;;       <owl:Class rdf:about="http://purl.obolibrary.org/obo/OBI_0000015"/><!-- biological_feature_identification_objective -->
;;     </rdfs:subClassOf>
;;     <!-- curation status --><OBI_0000281 rdf:resource="http://purl.obolibrary.org/obo/OBI_0000319"/><!-- metadata complete -->
;;     <rdfs:label xml:lang="en">cellular_feature_identification_objective</rdfs:label>
;;   </owl:Class>


(defun comment-ids-in-owl-file (in-path out-path)
  (let ((kb (load-ontology in-path)))
    (let ((labels (rdfs-labels kb)))
      (with-open-file (in in-path)
	(with-open-file (out out-path :direction :output :if-does-not-exist :create :if-exists :supersede)
	  (loop for line = (read-line in nil :eof)
	     until (eq line :eof)
	     for prop-replaced = (replace-all line  "<(OBI_\\d+)"
					      (lambda(id)
						(format nil "<!-- ~a --><~a" 
							(car
							(gethash
							 (make-uri (format nil "http://purl.obolibrary.org/obo/~a" id)
								   ) labels))
							id)) 1)
	     for entity-replaced = (replace-all prop-replaced  "(rdf:(about|resource)=\"http://purl.obolibrary.org/obo/(OBI_\\d+)\"/{0,1}>)"
						(lambda(whole id)
						  (format nil "~a<!-- ~a -->"  
				      
							  whole
							  (car (gethash
							   (make-uri (format nil "http://purl.obolibrary.org/obo/~a" id)
								     ) labels))))
						1 3)
	     
	     do
	     (write-string entity-replaced out)
	     (terpri out))))))))