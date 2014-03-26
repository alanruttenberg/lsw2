(defvar *no-new-obi-ids-below-this* 900)

(defun rewrite-instance-file (uri-rewrites in-file out-file ontology-url &rest prefixes)
  (let ((table (make-hash-table :test 'equal)))
    (with-open-file (f uri-rewrites)
      (loop for line = (read-line f nil :eof)
	 until (eq line :eof)
	 for (from to) = (car (all-matches line "(.*)=>(.*)$" 1 2))
	 do (setf (gethash from table) to)))
    (remhash ontology-url table)
    (apply 'rewrite-uris table in-file out-file ontology-url prefixes)))

(defun rewrite-uris (uri-rewrites in-file out-file ontology-url &key prefixes debug with-header)
  (let ((in-model (#"createDefaultModel" 'modelfactory))
	(out-model (#"createDefaultModel" 'modelfactory)))
    (#"setNsPrefix" out-model "owl" (uri-full !owl:))
    (#"setNsPrefix" out-model "xsd" (uri-full !xsd:))
    (#"setNsPrefix" out-model "rdfs" (uri-full !rdfs:))
    (#"setNsPrefix" out-model "rdf" (uri-full !rdf:))
    (#"setNsPrefix" out-model "roproposed" "http://purl.org/obo/owl/OBO_REL#")
    (#"setNsPrefix" out-model "protege" "http://protege.stanford.edu/plugins/owl/protege#")
    (#"setNsPrefix" out-model "obo" "http://purl.obolibrary.org/obo/")
    (#"setNsPrefix" out-model "oboInOwl" "http://www.geneontology.org/formats/oboInOwl#")
    (#"read" in-model
	     (new 'bufferedinputstream
		  (#"getInputStream" (#"openConnection" (new 'java.net.url (format nil "file://~a" (namestring (truename in-file)))))))
	     ontology-url)
    (loop for (abbrev expansion) in prefixes do
	 (#"setNsPrefix" in-model abbrev expansion))
    (loop with iterator = (#"listStatements" in-model)
       while (#"hasNext" iterator)
       for statement = (#"next" iterator)
       for subject = (#"getSubject" statement)
       for object = (#"getObject" statement)
       for predicate = (#"getPredicate" statement)
       for replace = nil
       for replacement = nil
       for count from 0
       do
       (when (and (#"isResource" subject) (not (#"isAnon" subject)))
	 (let ((replacement
		(gethash  (#"toString" (#"getURI" subject)) uri-rewrites)))
	   (when replacement
	     (when debug
	       (format t "subject replacement ~a => ~a~%" (#"toString" (#"getURI" subject)) replacement))
	     (setq subject (#"createResource" out-model replacement)
		   replace t
		   ))))
       (when (and (#"isResource" object) (not (#"isAnon" object)) (not (#"isLiteral" object)))
	 (let ((replacement (gethash (#"toString" (#"getURI" object)) uri-rewrites)))
	   (when replacement
	     (when debug (format t "object replacement ~a => ~a~%" (#"toString" (#"getURI" object)) replacement))
	     (setq object (#"createResource" out-model replacement)
		   replace t))))
       (let ((replacement (gethash (#"toString" (#"getURI" predicate)) uri-rewrites)))
	 (when replacement
	   (when (format t "predicate replacement ~a => ~a~%" (#"toString" (#"getURI" predicate)) replacement))
	   (setq predicate (#"createResource" out-model replacement)
		 replace t)))
       (if replace 
	   (add-jena-triple out-model subject
			    predicate
			    object)
	   (#"add" out-model statement)
	   )
	 (when (and (not replace) debug)
	   (format t "no replacement ~a ~%" count)))
    (if with-header
	;; http://jena.sourceforge.net/IO/iohowto.html#output
	(let ((writer (#"getWriter" out-model "RDF/XML-ABBREV")))
	  (#"setProperty" writer "xmlbase" ontology-url)
	  (#"setProperty" writer "relativeuris" "same-document")
	  (#"setProperty" writer "showXmlDeclaration" "true")
	  (let ((jfile (new 'java.io.file (namestring (translate-logical-pathname out-file)))))
	    (when (not (#"exists" jfile))
	      (#"createNewFile" jfile))
	    (#"write" writer out-model (new 'java.io.fileoutputstream jfile) "http://purl.obolibrary.org/obo/")))
	(progn
	  (let ((jfile (new 'java.io.file (namestring (translate-logical-pathname out-file)))))
	    (when (not (#"exists" jfile))
	      (#"createNewFile" jfile))
	    (#"write" out-model (new 'java.io.fileoutputstream jfile) "RDF/XML-ABBREV"  ontology-url))))))

(defun get-uri-rewrites (input &key examine
			 (min-id *no-new-obi-ids-below-this*)
			 (id-format "http://purl.obolibrary.org/obo/OBI_~7,'0d")
			 )
  (let ((count min-id)
	(need (make-hash-table :test 'equalp))
	(all (make-hash-table :test 'equalp))
	(map (make-hash-table :test 'equalp))
	(inputs (cons input examine)))
    (labels ((next ()
	       (let ((candidate (format nil id-format (incf count))))
		 (if (gethash candidate all)
		     (next)
		     candidate))))
      (loop for (file url) in inputs
	 do
	 (flet ((add (uri)
		  (setf (gethash uri all) t)
		  (unless (or (#"matches" uri "http://purl.obolibrary.org/obo/(OBI|VO|IAO|FLU|IDO|OGMS)_(\\d+)$")
			      (#"matches" uri "http://purl.org/obo/owl/[a-zA-Z0-9_]*#.*")
			      (#"matches" uri "http://(ontology.neuinfo.org|www.w3.org|www.geneontology.org|www.ifomis.org|protege.stanford.edu)/.*")
			      (#"matches" uri "http://purl.org/dc/.*")
			      (#"matches" uri ".*\\.owl")
			      (#"matches" uri ".*ro\\.owl#.*")
			      (#"matches" uri ".*ro_bfo_bridge.*")
			      (#"matches" uri ".*TMPIAO.*")
			      )
		    (setf (gethash uri need)  t))))
	   (let ((in-model (#"createDefaultModel" 'modelfactory)))
	     (#"read" in-model
		      (new 'bufferedinputstream
			   (#"getInputStream" (#"openConnection"
					       (new 'java.net.url
						    (format nil "file://~a" (namestring (truename file)))))))
		      url)
	     (loop with iterator = (#"listStatements" in-model)
		while (#"hasNext" iterator)
		for statement = (#"next" iterator)
		for subject = (#"getSubject" statement)
		for object = (#"getObject" statement)
		for predicate = (#"getPredicate" statement)
		for replace = nil
		for replacement = nil
		do
		  (unless (member (#"toString" (#"getURI" predicate))
				  '("http://www.w3.org/2002/07/owl#imports"
				    "http://purl.obolibrary.org/obo/IAO_0000412")
				  :test 'equal)
		    (when (and (#"isResource" subject) (not (#"isAnon" subject)))
		      (add (#"getURI" subject)))
		    (when (and (#"isResource" object) (not (#"isAnon" object)) (not (#"isLiteral" object)))
		      (add (#"getURI" object)))
		    (add (#"getURI" predicate)))
		))))
      (maphash (lambda(uri ignore)
		 (setf (gethash uri map) (next)))
	       need)
      map)))
	       
    
;(setq map (get-uri-rewrites '("~/obi/trunk/src/ontology/branches/obi.owl" "http://purl.obolibrary.org/obo/obi.owl") '("~/obi/trunk/src/ontology/branches/text-mining-experiment.owl" "http://purl.obolibrary.org/obo/obi/text-mining-experiment.owl")))
;(rewrite-uris map "/Users/alanr/obi/ontology/obi.owl" "/Users/alanr/obi/ontology/obi-new.owl" (uri-full !obo:obi.owl) :with-header t)
;(rewrite-uris map "/Users/alanr/obi/ontology/IEDB-use-case.owl" "/Users/alanr/obi/ontology/IEDB-use-case-new.owl" "http://purl.org/IEDB/IEDB.owl" :with-header t)
;(rewrite-uris map "/Users/alanr/obi/ontology/text-mining-experiment.owl" "/Users/alanr/obi/ontology/conferrred-quality-new.owl" "http://purl.org/IEDB/IEDB.owl" "http://purl.obolibrary.org/obo/obi/text-mining-experiment.owl" :with-header t)
		    
		    
		    
		    
		    
		    

