;; Rerun the ontofox script to update externals (todo)
;; Load an ontology, merge all axioms into one ontology. (done)
;; Run reasoner. Add inferred subclass axioms.  (done)
:; Do a little materialization.  (todo)
;; Copy the ontology annotations from the top ontology (not the imports)  (done)
;; Don't add certain duplicates, such as extra copies of definitions or annotation. Be aware of duplicate 
;;   axioms that might be pulled in by chains of imports and prefer the proximate one. (todo)
;; Add imported-from annotations to the imported ontologies (done)
;; Write the merged ontology to the release directory (done)
;; Copy our own imports and others' unversioned imports to the release directory (done)
;; Add versionIRIs and rewrite imports to used them (done)
;; Add a note saying how this was created (done)
;; Write out a catalog-v001.xml because protege can't find ontologies in front of its nose (todo)
;; Write out what will be needed to be added to the the PURL config (todo)
;; Write out some basic facts / release notes template (todo)

(defun new-empty-kb (ontology-iri &key reasoner)
  "Make an empty KB into which we'll copy the merged file and the inferences"
  (let* ((manager (#"createOWLOntologyManager" 'org.semanticweb.owlapi.apibinding.OWLManager))
	 (ont (#"createOntology" manager (to-iri (uri-full ontology-iri)))))
    (make-v3kb :name ontology-iri :manager manager :ont ont :datafactory (#"getOWLDataFactory" manager) :default-reasoner reasoner)))
  
(defun make-merged-candidate (source namespace &key (reasoner :factpp) (release-base (guess-release-base source)))
  (setq namespace (string-downcase namespace))
  (when (not (v3kb-p source))
    (setq source (load-ontology source)))
  (let ((destont (new-empty-kb (make-uri (format nil "http://purl.obolibrary.org/obo/~a.owl" namespace))))
	(inferred-axiom-ont (new-empty-kb (make-uri (format nil "http://purl.obolibrary.org/obo/~a-inferred.owl" namespace))))
	(dispositions (compute-imports-disposition source namespace))
	(imported-from !<http://purl.obolibrary.org/obo/IAO_0000412>))
    (instantiate-reasoner source reasoner nil (new 'SimpleConfiguration (new 'NullReasonerProgressMonitor)))
    (format *debug-io* "Checking consistency~%") 
    (check-ontology source :classify t :reasoner reasoner)
    (loop for annotation in (jss::j2list (#"getAnnotations" (v3kb-ont source)))
	  for prop = (make-uri (#"toString" (#"getIRI" (#"getProperty"  annotation))))
	  unless (eq prop !owl:versionIRI)
	    do (add-ontology-annotation  annotation destont))
    (loop for disp in dispositions
	  for versioniri = (getf disp :versioniri) 
	  do (add-ontology-annotation (list imported-from versioniri) destont))
    (add-version-iri destont (make-versioniri namespace))
    (add-ontology-annotation `(,!rdfs:comment "This version of the ontology is the merge of all its imports and has added axioms inferred by an OWL reasoner") destont)
    (loop for disp in dispositions
	  for ont = (getf disp :ontology)
	  do (each-axiom ont (lambda(ax) (add-axiom ax destont)) nil))
    (format *debug-io* "Merging~%") 
    (ontology-with-inferred-axioms source :to-ont inferred-axiom-ont)
    (format *debug-io* "Adding inferences~%") 
    (let ((dest (merge-pathnames (format nil "~a-merged.owl" namespace) (ensure-release-dir release-base)))
	  (uri-map (mapcar (lambda(d) (list (getf d :ontologyiri) (getf d :versioniri))) dispositions)))
      (to-owl-syntax destont :rdfxml dest)
      (loop for disp in dispositions
	    for copy = (getf disp :copy)
	    when copy
	      do (let ((dest (merge-pathnames (make-pathname :name (pathname-name copy) :type "owl") (ensure-release-dir release-base))))
		   (format *debug-io* "Copying ~a to ~a~%" copy dest)
		   ;; I would use copy-file but it doesn't know about redirects
		   ;; (uiop/stream:copy-file copy dest)
		   (sys::run-program "curl" (list "-L" (uri-full (make-uri (namestring copy))) "-o" (namestring dest)))
		   (add-version-iris-and-rewrite-imports dest uri-map))
		 (let ((to-be-deleted (directory (merge-pathnames (make-pathname :name :wild :type "bak") (ensure-release-dir release-base)))))
		   (loop for file in to-be-deleted
			 do (format *debug-io* "Deleting ~a~%" file)
			    (delete-file file)))
		 (values destont dest)))))

(defun guess-release-base (source)
  "Usual setup is 'releases' parallel to 'src' and ontology below
   'ontology'. Guess the release directory from our input, check if it is
   there and let us know where we stand"
  (let ((source-path (if (or (stringp source) (pathnamep source))
			 source
			 (caar (loaded-documents source)))))
    (let ((release-base (make-pathname :directory 
				       (append
					(remove-if
					 (lambda(e) (member e '("src" "ontology") :test'equal))
					 (pathname-directory source-path))
					'("releases")))))
      (if (probe-file release-base)
	  (format *debug-io* "Using release directory: ~a~%" release-base)
	  (cerror "Can't figure out release base (guessed ~a). Pass it to the function" release-base))
      release-base)))

(defun add-version-iris-and-rewrite-imports (ontology-path map)
  "Walk the loaded files. For our files copy to release directory and
  rewrite the imports and add versionIRIs. For other-than-our files
  that don't have versionIRIs we fetch their latest (? what about
  cached versions), save to release directory, and adjust their PURLs"
  (let ((manager (#"createOWLOntologyManager" 'org.semanticweb.owlapi.apibinding.OWLManager)))
    (let* ((ontology (#"loadOntologyFromOntologyDocument" manager (new 'filedocumentsource (new 'java.io.file  (namestring ontology-path)))))
	   (kb (make-v3kb :manager (#"getOWLOntologyManager" ontology)
			  :datafactory (#"getOWLDataFactory" (#"getOWLOntologyManager" ontology)) :ont ontology
			  :changes (new 'arraylist))))
      (loop for old-import in (jss::j2list (#"getImportsDeclarations" ontology))
	    for import-uri = (#"toString" (#"getIRI" old-import))
	    for replacement = (second (assoc (make-uri import-uri) map :test 'equal))
	    when replacement
	      do (remove-ontology-imports old-import kb)
		 (add-ontology-imports replacement kb))
      (let ((newversioniri (second (assoc (make-uri (#"toString" (#"get" (#"getOntologyIRI" (#"getOntologyID" ontology))))) map))))
	(when newversioniri
	  (let ((change (#"createOntologyChange"
			 (new 'SetOntologyIDData 
			      (new 'owlontologyid (#"get" (#"getOntologyIRI" (#"getOntologyID" ontology)))
				   (to-iri newversioniri))) ontology)))
	    (#"add" (v3kb-changes kb) change))))
      (apply-changes kb)
      (format *debug-io* "Rewriting imports for ~a~%" ontology-path)
      (to-owl-syntax kb :rdfxml ontology-path))))

(defun compute-imports-disposition (source namespace &optional (release-time (get-universal-time)))
  "For each of the imports decide if we are using a published
   versioned import or a version we serve. Return a data structure that
   will guide further work, including where to copy something from (or
   nil if we're not going to copy it) the ontologyiri the versioniri and
   where the import was loaded from"
  (let ((date (ontology-version-date release-time )))
    (loop for (loaded-from uri ont) in (loaded-documents source)
	  for id = (#"getOntologyID" ont)
	  for ontologyiri = (#"toString" (#"get" (#"getOntologyIRI" id)))
	  for versioniri = (ignore-errors (#"toString" (#"get" (#"getVersionIRI" id))))
	  do
	     (assert (equal ontologyiri uri) (uri ontologyiri)
		     "What's up - uri and ontologyiri don't match ~a - ~a" uri ontologyiri)
	  collect
	  (list* :ontologyiri (make-uri ontologyiri) :ontology ont 
		 (cond ((equal (third (pathname-directory ontologyiri)) namespace)
			;; This is one of ours, so make date relative purl <namespace>/<file.owl> -> <namespace>/<date>/<file.owl>
			(let ((versioned (namestring (merge-pathnames (make-pathname :directory `(:relative ,date)) ontologyiri))))
			  (list :copy loaded-from :ontologyiri ontologyiri :versioniri (make-uri versioned) )))
		       ((equal (pathname-name ontologyiri) namespace)
			;; This is the main file. <namespace.owl> -> <namespace>/<date><namespace.owl>
			(let ((versioned (namestring (merge-pathnames (make-pathname :directory `(:relative ,namespace ,date)) ontologyiri))))
			  (list :copy loaded-from :versioniri (make-uri versioned) )))
		       ;; This is an external import. Use its versionIRI
		       ;; Might neede to be careful - if locally cached it could be stale. Not the case for IAO
		       ((not versioniri)
			;(warn "Didn't get versionIRI for ~a so using copying and will use local version" ontologyiri)
			(list :copy ontologyiri :versioniri (make-local-version-uri ontologyiri namespace) ))
		       (t (list :copy nil :versioniri (make-uri versioniri))))))))

(defun make-local-version-uri (ontology-iri namespace)
  "We have http://auth/path/file.owl - We make:
  http://purl.obolibrary.org/obo/namespace/date/auth/path/file.owl (if
  path starts with /obo/ we don't include that. Ditto if auth is
  purl.obolibrary.org)."
  (let* ((auth (getf (pathname-host ontology-iri) :authority))
	 (path (pathname-directory ontology-iri))
	 (file (pathname-name ontology-iri))
	 (type (pathname-type ontology-iri))
	 (date (ontology-version-date)))
    (let ((path (if (equal "obo" (second path))
		    (cons (car path) (cddr path))
		    path))
	  (auth (if (equal auth "purl.obolibrary.org")
		    nil
		    `(:relative ,auth))))
      (make-uri (namestring (merge-pathnames (make-pathname :directory (rplaca path :relative))
		       (merge-pathnames (make-pathname :directory auth)
					(make-pathname
					 :directory `(:absolute  "obo" ,namespace ,date)
					 :name file
					 :type type
					 :host `(:scheme "http" :authority "purl.obolibrary.org"))
					)
		       ))))))
      
(defun ontology-version-date (&optional (when (get-universal-time)))
  "The date now, in the form YYYY-MM-DD"
  (multiple-value-bind (second minute hour date month year day)  (decode-universal-time when)
    (declare (ignore second minute hour  day))
    (format nil "~a-~2,'0D-~2,'0D" year month date)))

(defun make-versioniri (namespace)
  "The versioniri for the main artifact"
  (make-uri (format nil "http://purl.obolibrary.org/obo/~a/~a/~a.owl" namespace (ontology-version-date) namespace)))

(defun ensure-release-dir (release-base) 
  "Make sure that the dated release directory is present and if not create it"
  (let ((basename (namestring (translate-logical-pathname release-base))))
    (when (not (#"matches" basename ".*/$"))
      (setq basename (concatenate 'string basename "/")))
    (declare (ignore second minute hour  day))
    (ensure-directories-exist 
     (merge-pathnames (make-pathname :directory `(:relative ,(ontology-version-date)))
		      basename))))


				      
