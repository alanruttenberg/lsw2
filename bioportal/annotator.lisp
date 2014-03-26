;; this is the key associated with the account "lsw". You might want to get your own key,
;; by creating an account on bioportal.

(defvar *bioportal-key* "6551953c-31bb-4189-91b5-f0733a251c61")

(defun ncbo-annotate (text apikey &key (format "XML"))
  (get-url "http://rest.bioontology.org/obs/annotator" 
	   :post (append 
		  (list 
		   (list "apikey" apikey)
		   (list "textToAnnotate" text))
		  (when format (list (list "format" format))))))

(defun ncbo-get-signature (conceptid ontologyid &key (apikey *bioportal-key*) (format "XML"))
  (get-url (format nil "http://rest.bioontology.org/bioportal/concepts/~a?conceptid=~a&apikey=~a" ontologyid conceptid apikey)))

(defun test-ncbo-annotator ()
  (let ((sample "This protocol will evaluate patients with systemic lupus erythematosus (SLE) and their relatives")) 
   (xmls::parse (ncbo-annotate sample *bioportal-key* :format "XML"))))

(defun ensure-cached-ontology-by-ncbi-localid (id &key (cachedir "lsw:bioportal-cache;") force)
  (let ((fname (merge-pathnames "lsw:bioportal-cache;" (make-pathname :name id :type "owl"))))
    (ensure-directories-exist fname)
    (if (not (and (probe-file fname) (not force)))
	(progn
	  (format *debug-io* "Downloading ~a.owl from bioportal..." id)
	  (multiple-value-bind (result errorp) (ignore-errors
					       (car (multiple-value-list (get-url (format nil "http://rest.bioontology.org/bioportal/ontologies/download/~a?apikey=~a" id *bioportal-key*)))))
	    (if errorp
		(progn (warn (princ-to-string errorp)) (format *debug-io* "Failed~&"))
		(with-open-file (f  fname :direction :output :if-exists :supersede :if-does-not-exist :create :external-format :UTF-8)
		  (write-string result f)
		  (format *debug-io* "Done~&")
		  t))))
	t)))

(defun test-download-ontology ()
  (ensure-cached-ontology-by-ncbi-localid "39343" :force t))

(defvar *ontcache* (make-hash-table :test 'equal))

(defun test-superclasses ()
  (ensure-cached-ontology-by-ncbi-localid "39343")
  (let ((ont
	 (or
	  (gethash "39343.owl" *ontcache*)
	  (progn
	    (let* ((original (load-ontology "lsw:bioportal-cache;39343.owl")) ;; hehe, inconsistent. Unfortunately weakend doesn't like inconsistency either - unless new name?
		   (weakened (weaken-to-only-subclasses original (string (gensym)) :keep-class-assertions nil)))
	      (instantiate-reasoner weakened :factpp) ; cause it's the fastest
	      (setf (v3kb-weakened-from weakened) nil) ; free space of old ontology
	      (setf (gethash "39343.owl" *ontcache*) weakened)))))) ; might make sense to 

    (let* ((uri !<http://www.loria.fr/~coulet/ontology/sopharm/version2.1/chebi.owl#CHEBI_29234>)
	  (term (rdfs-label uri ont))
	  (superclasses (mapcar (lambda(e) (rdfs-label e ont))
				(ancestors uri ont))))
      (values (car term)  (mapcar 'car superclasses)))))


;(get-ontology-by-ncbi-local-id "47005" ) ;; whoops - not guaranteed to be OWL - this one is obo
;(map 'list (lambda(e) (third (find-element-with-tag e "localOntologyId")))  (find-elements-with-tag r "ontologyUsedBean"))