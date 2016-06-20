;; review https://www.nlm.nih.gov/research/umls/META3_current_semantic_types.html
;; and https://www.nlm.nih.gov/research/umls/META3_current_relations.html
;; and https://www.nlm.nih.gov/research/umls/sourcereleasedocs/index.html
;; Start with concept
;;   Show name, relations
;; For each atom
;;   Show atom name, source-term,
;; recurse over child parent relationship
;;   show child relation parent

(defun dump-cui (CUI)
  (let ((info (car (umls-concept-info cui)))
	(relations (umls-concept-relations cui))
	(definitions (umls-concept-definitions cui))
	(atoms (umls-concept-atoms cui)))
    (print "info")
    (pprint info)
    (print "relations")
    (pprint relations)
    
    (print "definitions")
    (pprint definitions)
    (print "atoms")
    (pprint atoms)))

(defun cui-from-url (url)
  (caar (all-matches url "CUI/(C\\d+)" 1)))

(defun aui-from-url (url)
  (caar (all-matches url "AUI/(A\\d+)" 1)))

(defun source-term-source-and-id-from-url (url)
  (car (all-matches url "/source/([^/]*)/([^/]*)" 1 2)))
  

(defun merge-sourced-strings (them)
  "Give a list of conses - string . source, return a list of lists one for each unique string with (cdr list) the list of sources. For concise display"
  (let ((table (make-hash-table :test 'equalp)))
    (loop for (string . source) in them
       do (pushnew source (gethash string table) :test 'equalp))
    (let ((result nil))
      (maphash (lambda (string sources)
		 (push (list* string sources) result))
	       table)
      result)))
  
(defun umls-field (thing field)
  (cdr (assoc field thing)))

(defmacro uget (&rest args) `(umls-field ,@args))

(defun atom-relations (atom)
  (when (assoc :relations atom)
    (let* ((subject-name (uget atom :name))
	   (subject-source (root-source atom))
	   (subject-id (uget atom :ui))
	   (relations (umls-atom-relations subject-id))
	   (atom-atom-relations (remove-if-not (lambda(rel) (equalp (uget rel :class-type) "AtomRelation")) relations))
	   (atom-concept-relations (set-difference relations atom-atom-relations)))
      (append
       (loop for rel in atom-atom-relations
	  for relation-source = (root-source rel)
	  for object-id = (aui-from-url (uget  rel :related-id))
	  for object-atom =  (car (umls-atom-info object-id))
	  for object-name =  (uget object-atom :name)
	  for object-source = (root-source object-atom)
	  for relation-name = (relation-label rel)
	  collect (list `(,subject-name ,subject-source)
			`(,relation-name ,relation-source)
			`(,object-name ,object-source)
			`(,subject-id ,object-id)))
       (loop for rel in atom-concept-relations
	  for relation-source = (root-source rel)
	  for object-id = (uget  rel :related-id) ;; Sheesh. For atoms this is a URL, but for concepts it's just the id.
	  for object-concept = (car (umls-concept-info object-id))
	  for object-name = (uget object-concept :name)
	  for object-source = (if (uget object-concept :root-source) (root-source object-concept) "UMLS")
	  for relation-name = (relation-label rel)
	  collect (list `(,subject-name ,subject-source)
			`(,relation-name ,relation-source)
			`(,object-name ,object-source)
			`(,subject-id ,object-id)))))))

(defun root-source (thing)
  (or (second (assoc (cdr (assoc :root-source thing)) *umls-sources* :test 'equalp))
      `(:wtf ,(cdr (assoc :root-source thing)))))

(defun relation-label-expanded (raw)
  (second (assoc raw *umls-relations* :test 'equal)) )

(defun relation-label (rel)
  (let ((first-try (uget rel :additional-relation-label)))
    (if (or (null first-try) (equal first-try ""))
	(relation-label-expanded (uget rel :relation-label)) 
	(cons first-try (uget rel :relation-label)))))  

			 
(defun describe-umls-concept (cui)
  (let* ((info (car (umls-concept-info cui)))
	 (name (cdr (assoc :name info)))
	 (definitions (and (assoc :definitions info)
			   (merge-sourced-strings
			    (loop for def in (umls-concept-definitions cui)
			       when (assoc :value def)
			       collect (cons (cdr (assoc :value def))
					     (or (second (assoc (cdr (assoc :root-source def)) *umls-sources* :test 'equalp))
						 `(:wtf ,(cdr (assoc :root-source def)))))))))
	 (relations (and (assoc :relations info)
			 (loop for rel in (umls-concept-relations cui)
			    for relatum = (let ((rinfo (car (umls-concept-info (cui-from-url (cdr (assoc :related-id rel))))))) (cons (cdr (assoc :name rinfo)) (cdr (assoc :ui rinfo))))
			    for rel-abbrev = (cdr (assoc :relation-label rel))
			    collect (cons (relation-label-expanded rel-abbrev) relatum))))
	 (atoms (and (assoc :atoms info)
		     (merge-sourced-strings
		      (loop for atom in (umls-concept-atoms cui)
			 for language = (cdr (assoc :language atom))
			 when (equalp language "ENG")
			 collect (append (list (cdr (assoc :name atom)))
					 (list (or (second (assoc (cdr (assoc :root-source atom)) *umls-sources* :test 'equalp))
					     `(:wtf ,(cdr (assoc :root-source atom)))))
					 (atom-relations atom))))))
	 (atom-relations (mapcan (lambda(atom) (atom-relations atom)) (umls-concept-atoms cui))))
    (pprint (list name definitions relations (remove-duplicates atoms :test 'equalp) atom-relations))))

(defun snomeds-from-cui (cui)
  (let ((atoms (umls-concept-atoms cui)))
    (mapcar (lambda(c)
	      (#"replaceFirst" c ".*?(\\d+)$" "$1"))
	    (remove-duplicates  (mapcan (lambda(atom) (list (cdr (assoc :code atom))))
					(remove-if-not (lambda(a) (equal (cdr (assoc :root-source a)) "SNOMEDCT_US")) atoms))
				:test 'equalp))))

(defun explore-snomed-superclasses (cui snomed)
  (let ((snomeds (snomeds-from-cui cui)))
    (and snomeds
	 (browse-parent-hierarchy (make-uri (concatenate 'string "http://snomed.info/id/" (car snomeds))) snomed))))
  
