; https://www.nlm.nih.gov/research/umls/knowledge_sources/metathesaurus/release/abbreviations.html

(defun get-parent-hierarchy-for-concept (cui)
  "For the give CUI get the atoms, and from those get the parents, recursively.
   Cache results. The result would be parents up to several roots, one for 
   each difference source, apparently. For each node get the source, the id, 
   the terms, and the definitions."
  )

  
(defun cache-stuff ()
  (flet ((has-value (r c)
	   (and (assoc r c) (not (equalp (cdr (assoc r c)) "NONE")))))
    (with-open-file (f "/Second/Downloads/2016-05-28/disease_families.csv")
      (loop with diseases = (make-hash-table :test 'equalp)
	 for line = (read-line f nil :eof)
	 until (eq line :eof) 
	 for (familyId familyLabel numDescendants descendantId descendantLabel) = (split-at-char line #\,)
	 for concept = (umls-concept-info descendantId)
	 do
	   (princ descendantId) (princ #\C)
	   (when (has-value :relations concept) (umls-concept-relations descendantId) (princ #\R))
	   (when (has-value :definitions concept) (umls-concept-definitions descendantId) (princ #\D))
	   (when (has-value :atoms concept) (umls-concept-atoms descendantId) (princ #\A))
	   (terpri)))))


	    
