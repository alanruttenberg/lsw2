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
			    for relatum = (let ((rinfo (car (umls-concept-info (cui-from-url (cdr (assoc :related-id rel))))))) (print-db rinfo)(cons (cdr (assoc :name rinfo)) (cdr (assoc :ui rinfo))))
			    for rel-abbrev = (cdr (assoc :relation-label rel))
			    collect (cons (second (assoc rel-abbrev *umls-relations* :test 'equal)) relatum))))
	 (atoms (and (assoc :atoms info)
		     (merge-sourced-strings
		     (loop for atom in (umls-concept-atoms cui)
			for language = (cdr (assoc :language atom))
			when (equalp language "ENG")
			collect (cons (cdr (assoc :name atom))
				      (or (second (assoc (cdr (assoc :root-source atom)) *umls-sources* :test 'equalp))
					  `(:wtf ,(cdr (assoc :root-source atom))))
				      ))))))
    (pprint (list name definitions relations (remove-duplicates atoms :test 'equalp)))))
