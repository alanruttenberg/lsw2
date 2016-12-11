; https://www.nlm.nih.gov/research/umls/knowledge_sources/metathesaurus/release/abbreviations.html

(defun get-parent-hierarchy-for-concept (cui)
  "For the give CUI get the atoms, and from those get the parents, recursively.
   Cache results. The result would be parents up to several roots, one for 
   each difference source, apparently. For each node get the source, the id, 
   the terms, and the definitions."
  )

  
(defun cache-concepts ()
  (flet ((has-value (r c)
	   (and (assoc r c) (not (equalp (cdr (assoc r c)) "NONE")))))
    (with-open-file (f "/Second/Downloads/2016-05-28/disease_families.csv")
      (loop for line = (read-line f nil :eof)
	 until (eq line :eof) 
	 for (familyId familyLabel numDescendants descendantId descendantLabel) = (split-at-char line #\,)
	 for concept = (umls-concept-info descendantId)
	 do
	   (princ descendantId) (princ #\C)
	   (when (has-value :relations concept) (umls-concept-relations descendantId) (princ #\R))
	   (when (has-value :definitions concept) (umls-concept-definitions descendantId) (princ #\D))
	   (when (has-value :atoms concept) (umls-concept-atoms descendantId) (princ #\A))
	   (terpri)))))

(defun ensure-result-structure ()
  (maphash (lambda(key results)
	     ;; the hash values are the (values) of the api call
	     (setq results (car results))
	     (when results
		  (every (lambda(result)
			   (every (lambda(pair)
				    (assert 
				     (or (member (car pair) 
						 '(:semantic-types ;; this can be a list
						   ;; and the below can be null
						   ;;:source-originated :suppressible :ancestors :descendants :obsolete
						   ))
					 (and (consp pair) (atom (car pair)) 
					      (or (and (cdr pair) (atom (cdr pair)))
						  (null (cdr pair)))))
				     (pair result results ) "Problem with entry ~a in pair ~a" key pair)
				    t)
				    result))
			   results)
			 ))
	   *umls-api-cache*))

(defun map-umls-cache-pairs (f)
  (maphash (lambda(k v)
	     (declare (ignore k))
	     (let ((results (car v))) ;; list of alists
	       (loop for result in results ;; each result an alist
		  do
		    (map nil 
			 (lambda (el)
			   (if (eq (car el) :semantic-types)
			       (map nil f (cdr el))
			       (funcall f el)))
			 result)))) ;; f gets called on a pair
	   *umls-api-cache*))

(defun map-umls-results (f)
  (maphash (lambda(k v)
	     (declare (ignore k))
	     (map nil f (car v)))
	   *umls-api-cache*))

(defun map-umls-source-terms (f)
  (map-umls-cache-pairs 
   (lambda(pair) (when (eq (car pair) :code) (funcall f (cdr pair))))))
   

(defun all-mentioned-uris ()
  (let ((table (make-hash-table :test 'equal)))
    (let ((re (#"compile" 'java.util.regex.pattern "^http.*")))
    (map-umls-cache-pairs
     (lambda(e) 
       (when (and (stringp (cdr e)) (#"matches" (#0"matcher" re (cdr e))))
	 (setf (gethash (cdr e) table) t)))) table)))

(defun is-url-mentioned (url)
  (let ((count 0))
    (map-umls-cache-pairs
     (lambda(e) 
       (when (equal url (cdr e))
	 (incf count))))
    count))

(defun key-valueset (key &aux (them nil))
    (map-umls-cache-pairs
     (lambda(e) 
       (when (eq key (car e))
	 (pushnew (cdr e) them :test 'equal))))
    them)

(defun cache-source-terms (&optional which)
  (flet ((has-value (r c)
	   (and (assoc r c) (not (equalp (cdr (assoc r c)) "NONE")))))
    (let ((table (make-hash-table :test 'equal)))
      (unless which (map-umls-source-terms (lambda(e) (setf (gethash e table) t))))
      (maphash (lambda(url v)
		 (declare (ignore v))
		   (destructuring-bind (source id) (car (all-matches url "/source/([^/]*)/([^/]*)" 1 2))
		     (let ((in-cache (umls-source-term-info source id :probe t)))
		       (let ((term (car (umls-source-term-info source id))))
			 (if in-cache
			     (princ ".")
			     (progn (princ source) (princ "/") (princ id) (princ "/") (princ #\S)))
			 (when (has-value :relations term) (umls-source-term-relations source id) (princ #\R))
			 (when (has-value :parents term) (umls-source-term-parents source id) (princ #\P))
			 (when (has-value :ancestors term) (umls-source-term-ancestors source id) (princ #\A))
			 (unless in-cache (terpri))))))
	       (or which table)))))



