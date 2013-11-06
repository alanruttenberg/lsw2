;; e.g.

;; (save-ontology-and-imports-locally "http://purl.obolibrary.org/obo/obi.owl" "/Users/alanr/Desktop/save/")

;; Saves the ontology named at the top level of the directory, and the
;; rest of the imported ontologies in directories that mirror the url
;; path - host/dir/dir (ensuring there aren't collisions) Builds a
;; protege 4.1 friendly catalog-v0001.xml file so that protege knows
;; where all the files are.

;; Only subtlety is that sometimes the ontology imported doesn't have
;; the same URI as the one requested. In order to handle this, the
;; ontology manager is examined and a map from actual loaded ontology
;; to request ontology is built.  When saving the catalog we also
;; include an entry for the ontology *named* in the import.

;; Note that this calls out to the shell for wget to do the
;; fetching. Use :wget-command <path-to-wget> to specify where it
;; is. Unfortunately as we specify where to save the files, wget won't
;; do mirroring, so this will need to be replaced by code that does do
;; mirroring so that we can do a saner update when necessary.

(defun save-ontology-and-imports-locally (ontology directory &key dont-wget (wget-command (#"replaceFirst" (with-output-to-string (s) (run-shell-command "which wget" :output s)) "\\n" "")))
  (let ((top-uri (if (stringp ontology) ontology (v3kb-name ontology)))
	(ontology (setq @ (if (v3kb-p ontology) ontology (load-ontology ontology)))))
    (ensure-directories-exist directory)
    (let ((imported->import (make-hash-table :test 'equal)))
      (loop
	 with result-code
	 for (source ontology-iri ont) in (loaded-documents ontology)
	 for partial-path = (if (equal ontology-iri top-uri)
				(#"replaceAll" ontology-iri "^.*[#/]" "")
				(#"replaceAll" ontology-iri "^.*//" ""))
	 for fetch-cmd = (format nil "cd ~s ; ~a --output-document ~s   ~s" (namestring directory) wget-command partial-path source)
	 do (princ fetch-cmd)
	   (hashmap-to-hashtable
			     (get-java-field (#"getOWLOntologyManager" ont) "ontologyIDsByImportsDeclaration" t)
			     :invert? t
			     :keyfun (lambda(e) (#"toString" (#"getURI" e)))
			     :valfun (lambda(e) (#"toString" (#"getOntologyIRI" e)))
			     :table imported->import
			     :test 'equal)
	 (terpri)
	 (ensure-directories-exist (format nil "~a~a" directory partial-path))
	 (unless dont-wget
	   (setq result-code (run-shell-command fetch-cmd))
	   (when (not (zerop result-code))
	     (error "Wget returned an error: ~a" result-code)
	     (exit result-code)))
	 (sleep .01))
      (with-open-file (f (merge-pathnames "catalog-v001.xml" directory) :if-does-not-exist :create :direction :output :if-exists :supersede)
	(format f "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>~%")
	(format f "<catalog prefer=\"public\" xmlns=\"urn:oasis:names:tc:entity:xmlns:xml:catalog\">~%")
	(loop for (nil ontology-iri) in (loaded-documents ontology)
	   for partial-path = (if (equal ontology-iri top-uri)
				  (#"replaceAll" ontology-iri "^.*[#/]" "")
				  (#"replaceAll" ontology-iri "^.*//" ""))
	   do
	   (format f "  <uri name=~s uri =~s/>~%" ontology-iri partial-path)
	   (when (not (equal (gethash ontology-iri imported->import) ontology-iri))
	     (when (gethash ontology-iri imported->import)
	       (format f "  <uri name=~s uri =~s/>~%" (gethash ontology-iri imported->import) partial-path))))
	(format f "</catalog>~%")
	))))

(defun wget-executable ()
  (let ((it (#"replaceAll" (with-output-to-string (s) (run-shell-command "which wget" :output s))
		 "(.*?)\\s*$" "$1")))
    (assert (not (equal it "")) (it) "Couldn't find wget")
    it))

  ;; e.g.

;; (save-ontology-and-imports-locally "http://purl.obolibrary.org/obo/obi.owl" "/Users/alanr/Desktop/save/")

;; Saves the ontology named at the top level of the directory, and the
;; rest of the imported ontologies in directories that mirror the url
;; path - host/dir/dir (ensuring there aren't collisions) Builds a
;; protege 4.1 friendly catalog-v0001.xml file so that protege knows
;; where all the files are.

;; Only subtlety is that sometimes the ontology imported doesn't have
;; the same URI as the one requested. In order to handle this, the
;; ontology manager is examined and a map from actual loaded ontology
;; to request ontology is built.  When saving the catalog we also
;; include an entry for the ontology *named* in the import.

;; Note that this calls out to the shell for wget to do the
;; fetching. Use :wget-command <path-to-wget> to specify where it
;; is. Unfortunately as we specify where to save the files, wget won't
;; do mirroring, so this will need to be replaced by code that does do
;; mirroring so that we can do a saner update when necessary.



(defun uri-mapper-from-xml-catalog (catalog)
  (with-open-file (f catalog) 
    (loop
       with dir = (namestring (make-pathname :directory (pathname-directory catalog)))
       with mapper = (new 'OWLOntologyIRIMapperImpl)
       for el in (find-elements-with-tag (xmls::parse f) "uri")
       for uri = (coerce (attribute-named el "uri") 'simple-base-string)
       for name = (coerce (attribute-named el "name") 'simple-base-string)
       for physical-uri = (if (find #\: uri) uri (format nil "file://~a" (namestring (truename (format nil "~a~a" dir uri)))))
       when (and uri name)
       do 
       (#"addMapping" mapper (#"create" 'org.semanticweb.owlapi.model.IRI name) (#"create" 'org.semanticweb.owlapi.model.IRI physical-uri))
       finally (return mapper))))
	 
; (uri-mapper-from-xml-catalog "/Users/alanr/Desktop/save/catalog-v001.xml")

(defun uri-mapper-for-source (source)
  (cond ((and (uri-p source) (#"matches" (uri-full source) "^file:"))
	 (setq source (#"replaceAll" (uri-full source) "^file:/*" "/"))))
  (when (and (not (consp (pathname-host source)))
	     (probe-file source))
    (let ((catalog (merge-pathnames "catalog-v001.xml" source)))
      (if (probe-file catalog)
	  (uri-mapper-from-xml-catalog catalog)
	  (new 'AutoIRIMapper (new 'java.io.file (namestring (truename (make-pathname :directory (pathname-directory (truename source)))))) t)))))


