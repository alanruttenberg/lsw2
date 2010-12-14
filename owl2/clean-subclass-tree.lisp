(defun necessary-subclass-assertions (kb)
  (let ((table (make-hash-table :test 'equalp)))
    (with-ontology subclass-only (:collecting t)
	((loop for (s o) in
	     (sparql '(:select (?s ?o)()
		       (?s !rdfs:subClassOf ?o)
		       (:filter (and (not (isblank ?s)) (not (isblank ?o)))))
		     :kb kb :use-reasoner :none)
	     do (as (list 'subclass-of s o))))
      (check-ontology subclass-only :classify t)
      (loop for class in (descendants !owl:Thing subclass-only)
	   do
	   (loop for parent in (parents class subclass-only)
		do (setf (gethash (list (uri-full class) (uri-full parent)) table) t)))

      table)))

(defun clean-subclasses (source dest)
  (let* ((kb (load-ontology source))
	 (out-model (create-empty-jena-model))
	 (necessary-subclass-assertions (necessary-subclass-assertions kb))
	 (in-model (#"createDefaultModel" 'modelfactory)))
    (#"read" in-model
	     (new 'bufferedinputstream
		  (#"getInputStream" (#"openConnection" (new 'java.net.url (format nil "file://~a" source)))))
	     "http://purl.obolibrary.org/obo/obi.owl")
    (loop with iterator = (#"listStatements" in-model)
       while (#"hasNext" iterator)
       for statement = (#"next" iterator)
       for subject = (#"getSubject" statement)
       for object = (#"getObject" statement)
       for predicate = (#"toString" (#"getURI" (#"getPredicate" statement)))
       ;; only include subclass relations that are needed - cleans up the display
       ;; of hierarchy for tools that don't otherwise do so
	 unless
	 (and 
	  (equal predicate "http://www.w3.org/2000/01/rdf-schema#subClassOf")
	  (not (#"isAnon" subject))
	  (not (#"isAnon" object))
	  (not (gethash (list (#"toString" (#"getURI" subject)) (#"toString" (#"getURI" object)))
			necessary-subclass-assertions)))
       ;; if we pass the gauntlet, add to the new model
       do (#"add" out-model statement))

    (write-jena-model out-model (namestring (translate-logical-pathname dest)))
    ))