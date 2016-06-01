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

(defun count-cuis ()
  (let ((table (make-hash-table :test 'equalp)))
    (with-open-file (f "/Second/Downloads/2016-05-28/disease_families.csv")
      (loop with diseases = (make-hash-table :test 'equalp)
	 for line = (read-line f nil :eof)
	 until (eq line :eof) 
	 for (familyId familyLabel numDescendants descendantId descendantLabel) = (split-at-char line #\,)
	 do (setf (gethash descendantId table) t)(setf (gethash familyid table) t)))
    table))

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

(defun cache-source-terms ()
  (flet ((has-value (r c)
	   (and (assoc r c) (not (equalp (cdr (assoc r c)) "NONE")))))
    (let ((table (make-hash-table :test 'equal)))
      (map-umls-source-terms (lambda(e) (setf (gethash e table) t)))
      (maphash (lambda(url v)
		 (declare (ignore v))
		 (setq @ url)
		 (destructuring-bind (source id) (car (all-matches url "/source/([^/]*)/([^/]*)" 1 2))
		   (let ((term (car (umls-source-term-info source id))))
		     (princ source) (princ "/") (princ id) (princ "/") (princ #\S)
		     (when (has-value :relations term) (umls-source-term-relations source id) (princ #\R))
		     (when (has-value :parents term) (umls-source-term-parents source id) (princ #\P))
		     (when (has-value :ancestors term) (umls-source-term-ancestors source id) (princ #\A))
		     (terpri))))
	       table))))

;(gethash '(umls-source-term-relations "MDRCZE" "10029004" NIL NIL NIL 100) *umls-api-cache*)

;(print *umls-api-cache*)
;(untrace)
;(pprint (umls-concept-atoms "C0311370"))

(defun hp (CUI)
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
#|
(hp "C0393639")

"info" 
((:CLASS-TYPE . "Concept") (:UI . "C0393639") (:SUPPRESSIBLE)
 (:DATE-ADDED . "01-01-1998") (:MAJOR-REVISION-DATE . "11-10-2015")
 (:STATUS . "R")
 (:SEMANTIC-TYPES
  ((:NAME . "Disease or Syndrome")
   (:URI
    . "https://uts-ws.nlm.nih.gov/rest/semantic-network/2016AA/TUI/T047")))
 (:ATOM-COUNT . 93) (:ATTRIBUTE-COUNT . 0) (:CV-MEMBER-COUNT . 0)
 (:ATOMS
  . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639/atoms")
 (:DEFINITIONS
  . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639/definitions")
 (:RELATIONS
  . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639/relations")
 (:DEFAULT-PREFERRED-ATOM
  . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639/atoms/preferred")
 (:RELATION-COUNT . 1) (:NAME . "Hashimoto's encephalitis"))
"relations" 
(((:CLASS-TYPE . "ConceptRelation") (:UI . "R15814805")
  (:SUPPRESSIBLE) (:SOURCE-UI . "NONE") (:OBSOLETE)
  (:SOURCE-ORIGINATED) (:ROOT-SOURCE . "MTH")
  (:RELATION-LABEL . "RO") (:ADDITIONAL-RELATION-LABEL . "")
  (:GROUP-ID . "NONE") (:ATTRIBUTE-COUNT . 0)
  (:RELATED-ID
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0521657")
  (:RELATED-ID-NAME . "Allergic encephalitis")))
"definitions" 
(((:CLASS-TYPE . "Definition") (:SOURCE-ORIGINATED)
  (:ROOT-SOURCE . "NCI")
  (:VALUE
   . "Inflammation of the brain secondary to an immune response triggered by the body itself."))
 ((:CLASS-TYPE . "Definition") (:SOURCE-ORIGINATED)
  (:ROOT-SOURCE . "NCI_NICHD")
  (:VALUE
   . "Inflammation of the brain secondary to an immune response triggered by the body itself.")))
"atoms" 
(((:Class-TYPE . "Atom") (:UI . "A26238737")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRCZE") (:TERM-TYPE . "LLT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRCZE/10014582")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A26238737/relations")
  (:DEFINITIONS . "NONE")
  (:NAME . "Alergická encefalitida (autoimunitní)")
  (:LANGUAGE . "CZE"))
 ((:CLASS-TYPE . "Atom") (:UI . "A25854075")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRHUN") (:TERM-TYPE . "LLT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRHUN/10014582")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A25854075/relations")
  (:DEFINITIONS . "NONE")
  (:NAME . "Allergiás encephalitis (autoimmun)") (:LANGUAGE . "HUN"))
 ((:CLASS-TYPE . "Atom") (:UI . "A25810637")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRHUN") (:TERM-TYPE . "LLT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRHUN/10072378")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A25810637/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Autoimmun encephalitis")
  (:LANGUAGE . "HUN"))
 ((:CLASS-TYPE . "Atom") (:UI . "A20293546")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRHUN") (:TERM-TYPE . "PT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRHUN/10072378")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A20293546/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Autoimmun encephalitis")
  (:LANGUAGE . "HUN"))
 ((:CLASS-TYPE . "Atom") (:UI . "A26059930")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRHUN") (:TERM-TYPE . "LLT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRHUN/10075691")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A26059930/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Autoimmun encephalopathia")
  (:LANGUAGE . "HUN"))
 ((:CLASS-TYPE . "Atom") (:UI . "A24690281")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRHUN") (:TERM-TYPE . "PT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRHUN/10075691")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A24690281/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Autoimmun encephalopathia")
  (:LANGUAGE . "HUN"))
 ((:CLASS-TYPE . "Atom") (:UI . "A25762793")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "NCI_NICHD") (:TERM-TYPE . "PT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/NCI_NICHD/C122414")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/NCI_NICHD/C122414")
  (:SOURCE-DESCRIPTOR . "NONE") (:ATTRIBUTES . "NONE")
  (:PARENTS . "NONE") (:ANCESTORS) (:CHILDREN . "NONE")
  (:DESCENDANTS) (:RELATIONS . "NONE") (:DEFINITIONS . "NONE")
  (:NAME . "Autoimmune Encephalitis") (:LANGUAGE . "ENG"))
 ((:CLASS-TYPE . "Atom") (:UI . "A25771844")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "NCI") (:TERM-TYPE . "PT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/NCI/C122414")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/NCI/C122414")
  (:SOURCE-DESCRIPTOR . "NONE") (:ATTRIBUTES . "NONE")
  (:PARENTS . "NONE") (:ANCESTORS) (:CHILDREN . "NONE")
  (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A25771844/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Autoimmune Encephalitis")
  (:LANGUAGE . "ENG"))
 ((:CLASS-TYPE . "Atom") (:UI . "A25918413")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRGER") (:TERM-TYPE . "LLT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRGER/10075691")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A25918413/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Autoimmune Enzephalopathie")
  (:LANGUAGE . "GER"))
 ((:CLASS-TYPE . "Atom") (:UI . "A24687682")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRGER") (:TERM-TYPE . "PT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRGER/10075691")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A24687682/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Autoimmune Enzephalopathie")
  (:LANGUAGE . "GER"))
 ((:CLASS-TYPE . "Atom") (:UI . "A18458318")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MSH") (:TERM-TYPE . "CE")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MSH/C535841")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MSH/M0530302")
  (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A18458318/attributes")
  (:PARENTS . "NONE") (:ANCESTORS) (:CHILDREN . "NONE")
  (:DESCENDANTS) (:RELATIONS . "NONE") (:DEFINITIONS . "NONE")
  (:NAME . "Autoimmune encephalitis") (:LANGUAGE . "ENG"))
 ((:CLASS-TYPE . "Atom") (:UI . "A3753518") (:SUPPRESSIBLE . "false")
  (:OBSOLETE . "false") (:ROOT-SOURCE . "SNOMEDCT_US")
  (:TERM-TYPE . "PT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/SNOMEDCT_US/95643007")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/SNOMEDCT_US/95643007")
  (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A3753518/attributes")
  (:PARENTS . "NONE") (:ANCESTORS) (:CHILDREN . "NONE")
  (:DESCENDANTS) (:RELATIONS . "NONE") (:DEFINITIONS . "NONE")
  (:NAME . "Autoimmune encephalitis") (:LANGUAGE . "ENG"))
 ((:CLASS-TYPE . "Atom") (:UI . "A3753507") (:SUPPRESSIBLE . "false")
  (:OBSOLETE . "false") (:ROOT-SOURCE . "SNOMEDCT_US")
  (:TERM-TYPE . "FN")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/SNOMEDCT_US/95643007")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/SNOMEDCT_US/95643007")
  (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A3753507/attributes")
  (:PARENTS . "NONE") (:ANCESTORS) (:CHILDREN . "NONE")
  (:DESCENDANTS) (:RELATIONS . "NONE") (:DEFINITIONS . "NONE")
  (:NAME . "Autoimmune encephalitis (disorder)") (:LANGUAGE . "ENG"))
 ((:CLASS-TYPE . "Atom") (:UI . "A0872563") (:SUPPRESSIBLE . "false")
  (:OBSOLETE . "false") (:ROOT-SOURCE . "SNMI") (:TERM-TYPE . "PT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/SNMI/DA-10550")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE")
  (:PARENTS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A0872563/parents")
  (:ANCESTORS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A0872563/ancestors")
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A0872563/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Autoimmune encephalitis, NOS")
  (:LANGUAGE . "ENG"))
 ((:CLASS-TYPE . "Atom") (:UI . "A1248240") (:SUPPRESSIBLE . "false")
  (:OBSOLETE . "false") (:ROOT-SOURCE . "RCD") (:TERM-TYPE . "PT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/RCD/X005E")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A1248240/attributes")
  (:PARENTS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A1248240/parents")
  (:ANCESTORS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A1248240/ancestors")
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A1248240/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Autoimmune encephalopathy")
  (:LANGUAGE . "ENG"))
 ((:CLASS-TYPE . "Atom") (:UI . "A3254267") (:SUPPRESSIBLE . "false")
  (:OBSOLETE . "false") (:ROOT-SOURCE . "SNOMEDCT_US")
  (:TERM-TYPE . "SY")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/SNOMEDCT_US/95643007")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/SNOMEDCT_US/95643007")
  (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A3254267/attributes")
  (:PARENTS . "NONE") (:ANCESTORS) (:CHILDREN . "NONE")
  (:DESCENDANTS) (:RELATIONS . "NONE") (:DEFINITIONS . "NONE")
  (:NAME . "Autoimmune encephalopathy") (:LANGUAGE . "ENG"))
 ((:CLASS-TYPE . "Atom") (:UI . "A25716509")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDR") (:TERM-TYPE . "LLT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDR/10075691")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A25716509/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Autoimmune encephalopathy")
  (:LANGUAGE . "ENG"))
 ((:CLASS-TYPE . "Atom") (:UI . "A24330241")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDR") (:TERM-TYPE . "PT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDR/10075691")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A24330241/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Autoimmune encephalopathy")
  (:LANGUAGE . "ENG"))
 ((:CLASS-TYPE . "Atom") (:UI . "A26244913")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRCZE") (:TERM-TYPE . "LLT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRCZE/10072378")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A26244913/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Autoimunitní encefalitida")
  (:LANGUAGE . "CZE"))
 ((:CLASS-TYPE . "Atom") (:UI . "A20222129")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRCZE") (:TERM-TYPE . "PT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRCZE/10072378")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A20222129/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Autoimunitní encefalitida")
  (:LANGUAGE . "CZE"))
 ((:CLASS-TYPE . "Atom") (:UI . "A26275334")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRCZE") (:TERM-TYPE . "LLT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRCZE/10075691")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A26275334/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Autoimunitní encefalopatie")
  (:LANGUAGE . "CZE"))
 ((:CLASS-TYPE . "Atom") (:UI . "A24652022")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRCZE") (:TERM-TYPE . "PT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRCZE/10075691")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A24652022/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Autoimunitní encefalopatie")
  (:LANGUAGE . "CZE"))
 ((:CLASS-TYPE . "Atom") (:UI . "A25995199")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRITA") (:TERM-TYPE . "LLT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRITA/10014582")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A25995199/relations")
  (:DEFINITIONS . "NONE")
  (:NAME . "Encefalite allergica (autoimmune)") (:LANGUAGE . "ITA"))
 ((:CLASS-TYPE . "Atom") (:UI . "A26320940")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRPOR") (:TERM-TYPE . "LLT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRPOR/10014582")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A26320940/relations")
  (:DEFINITIONS . "NONE")
  (:NAME . "Encefalite alérgica (auto-imune)") (:LANGUAGE . "POR"))
 ((:CLASS-TYPE . "Atom") (:UI . "A26357134")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRPOR") (:TERM-TYPE . "LLT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRPOR/10072378")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A26357134/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Encefalite auto-imune")
  (:LANGUAGE . "POR"))
 ((:CLASS-TYPE . "Atom") (:UI . "A20577012")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRPOR") (:TERM-TYPE . "PT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRPOR/10072378")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A20577012/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Encefalite auto-imune")
  (:LANGUAGE . "POR"))
 ((:CLASS-TYPE . "Atom") (:UI . "A25802350")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRITA") (:TERM-TYPE . "LLT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRITA/10072378")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A25802350/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Encefalite autoimmune")
  (:LANGUAGE . "ITA"))
 ((:CLASS-TYPE . "Atom") (:UI . "A20293855")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRITA") (:TERM-TYPE . "PT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRITA/10072378")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A20293855/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Encefalite autoimmune")
  (:LANGUAGE . "ITA"))
 ((:CLASS-TYPE . "Atom") (:UI . "A25787722")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRSPA") (:TERM-TYPE . "LLT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRSPA/10014582")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A25787722/relations")
  (:DEFINITIONS . "NONE")
  (:NAME . "Encefalitis alérgica (autoinmune)") (:LANGUAGE . "SPA"))
 ((:CLASS-TYPE . "Atom") (:UI . "A26092703")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRSPA") (:TERM-TYPE . "LLT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRSPA/10072378")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A26092703/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Encefalitis autoinmune")
  (:LANGUAGE . "SPA"))
 ((:CLASS-TYPE . "Atom") (:UI . "A20572272")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRSPA") (:TERM-TYPE . "PT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRSPA/10072378")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A20572272/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Encefalitis autoinmune")
  (:LANGUAGE . "SPA"))
 ((:CLASS-TYPE . "Atom") (:UI . "A26357575")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRPOR") (:TERM-TYPE . "LLT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRPOR/10075691")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A26357575/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Encefalopatia auto-imune")
  (:LANGUAGE . "POR"))
 ((:CLASS-TYPE . "Atom") (:UI . "A24647878")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRPOR") (:TERM-TYPE . "PT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRPOR/10075691")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A24647878/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Encefalopatia auto-imune")
  (:LANGUAGE . "POR"))
 ((:CLASS-TYPE . "Atom") (:UI . "A25852455")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRITA") (:TERM-TYPE . "LLT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRITA/10075691")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A25852455/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Encefalopatia autoimmune")
  (:LANGUAGE . "ITA"))
 ((:CLASS-TYPE . "Atom") (:UI . "A24398488")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRITA") (:TERM-TYPE . "PT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRITA/10075691")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A24398488/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Encefalopatia autoimmune")
  (:LANGUAGE . "ITA"))
 ((:CLASS-TYPE . "Atom") (:UI . "A26266253")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRPOR") (:TERM-TYPE . "LLT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRPOR/10069432")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A26266253/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Encefalopatia de Hashimoto")
  (:LANGUAGE . "POR"))
 ((:CLASS-TYPE . "Atom") (:UI . "A17279080")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRPOR") (:TERM-TYPE . "PT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRPOR/10069432")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A17279080/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Encefalopatia de Hashimoto")
  (:LANGUAGE . "POR"))
 ((:CLASS-TYPE . "Atom") (:UI . "A26150406")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRITA") (:TERM-TYPE . "LLT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRITA/10069432")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A26150406/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Encefalopatia di Hashimoto")
  (:LANGUAGE . "ITA"))
 ((:CLASS-TYPE . "Atom") (:UI . "A17280661")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRITA") (:TERM-TYPE . "PT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRITA/10069432")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A17280661/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Encefalopatia di Hashimoto")
  (:LANGUAGE . "ITA"))
 ((:CLASS-TYPE . "Atom") (:UI . "A26093099")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRSPA") (:TERM-TYPE . "LLT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRSPA/10075691")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A26093099/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Encefalopatía autoinmune")
  (:LANGUAGE . "SPA"))
 ((:CLASS-TYPE . "Atom") (:UI . "A24649592")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRSPA") (:TERM-TYPE . "PT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRSPA/10075691")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A24649592/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Encefalopatía autoinmune")
  (:LANGUAGE . "SPA"))
 ((:CLASS-TYPE . "Atom") (:UI . "A25843392")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRSPA") (:TERM-TYPE . "LLT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRSPA/10069432")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A25843392/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Encefalopatía de Hashimoto")
  (:LANGUAGE . "SPA"))
 ((:CLASS-TYPE . "Atom") (:UI . "A17277470")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRSPA") (:TERM-TYPE . "PT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRSPA/10069432")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A17277470/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Encefalopatía de Hashimoto")
  (:LANGUAGE . "SPA"))
 ((:CLASS-TYPE . "Atom") (:UI . "A25693040")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDR") (:TERM-TYPE . "LLT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDR/10014582")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A25693040/relations")
  (:DEFINITIONS . "NONE")
  (:NAME . "Encephalitis allergic (autoimmune)") (:LANGUAGE . "ENG"))
 ((:CLASS-TYPE . "Atom") (:UI . "A25699379")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDR") (:TERM-TYPE . "LLT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDR/10072378")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A25699379/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Encephalitis autoimmune")
  (:LANGUAGE . "ENG"))
 ((:CLASS-TYPE . "Atom") (:UI . "A20213247")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDR") (:TERM-TYPE . "PT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDR/10072378")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A20213247/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Encephalitis autoimmune")
  (:LANGUAGE . "ENG"))
 ((:CLASS-TYPE . "Atom") (:UI . "A25821168")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRFRE") (:TERM-TYPE . "LLT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRFRE/10014582")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A25821168/relations")
  (:DEFINITIONS . "NONE")
  (:NAME . "Encéphalite allergique (auto-immune)")
  (:LANGUAGE . "FRE"))
 ((:CLASS-TYPE . "Atom") (:UI . "A26175701")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRFRE") (:TERM-TYPE . "LLT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRFRE/10072378")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A26175701/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Encéphalite auto-immune")
  (:LANGUAGE . "FRE"))
 ((:CLASS-TYPE . "Atom") (:UI . "A24661406")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRFRE") (:TERM-TYPE . "PT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRFRE/10072378")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A24661406/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Encéphalite auto-immune")
  (:LANGUAGE . "FRE"))
 ((:CLASS-TYPE . "Atom") (:UI . "A25976847")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRFRE") (:TERM-TYPE . "LLT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRFRE/10075691")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A25976847/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Encéphalopathie auto-immune")
  (:LANGUAGE . "FRE"))
 ((:CLASS-TYPE . "Atom") (:UI . "A24661585")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRFRE") (:TERM-TYPE . "PT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRFRE/10075691")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A24661585/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Encéphalopathie auto-immune")
  (:LANGUAGE . "FRE"))
 ((:CLASS-TYPE . "Atom") (:UI . "A26175337")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRFRE") (:TERM-TYPE . "LLT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRFRE/10069432")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A26175337/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Encéphalopathie d'Hashimoto")
  (:LANGUAGE . "FRE"))
 ((:CLASS-TYPE . "Atom") (:UI . "A17279427")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRFRE") (:TERM-TYPE . "PT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRFRE/10069432")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A17279427/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Encéphalopathie d'Hashimoto")
  (:LANGUAGE . "FRE"))
 ((:CLASS-TYPE . "Atom") (:UI . "A25868577")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRGER") (:TERM-TYPE . "LLT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRGER/10072378")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A25868577/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Enzephalitis autoimmun")
  (:LANGUAGE . "GER"))
 ((:CLASS-TYPE . "Atom") (:UI . "A20291699")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRGER") (:TERM-TYPE . "PT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRGER/10072378")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A20291699/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Enzephalitis autoimmun")
  (:LANGUAGE . "GER"))
 ((:CLASS-TYPE . "Atom") (:UI . "A25862424")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRGER") (:TERM-TYPE . "LLT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRGER/10014582")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A25862424/relations")
  (:DEFINITIONS . "NONE")
  (:NAME . "Enzephalitis, allergisch (autoimmun)")
  (:LANGUAGE . "GER"))
 ((:CLASS-TYPE . "Atom") (:UI . "A26158689")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRHUN") (:TERM-TYPE . "LLT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRHUN/10069432")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A26158689/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Hashimoto encephalopathia")
  (:LANGUAGE . "HUN"))
 ((:CLASS-TYPE . "Atom") (:UI . "A19472054")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRHUN") (:TERM-TYPE . "PT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRHUN/10069432")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A19472054/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Hashimoto encephalopathia")
  (:LANGUAGE . "HUN"))
 ((:CLASS-TYPE . "Atom") (:UI . "A18458316")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MSH") (:TERM-TYPE . "NM")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MSH/C535841")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MSH/M0530302")
  (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A18458316/attributes")
  (:PARENTS . "NONE") (:ANCESTORS) (:CHILDREN . "NONE")
  (:DESCENDANTS) (:RELATIONS . "NONE") (:DEFINITIONS . "NONE")
  (:NAME . "Hashimoto's encephalitis") (:LANGUAGE . "ENG"))
 ((:CLASS-TYPE . "Atom") (:UI . "A17034120")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDR") (:TERM-TYPE . "PT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDR/10069432")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A17034120/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Hashimoto's encephalopathy")
  (:LANGUAGE . "ENG"))
 ((:CLASS-TYPE . "Atom") (:UI . "A18461418")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MSH") (:TERM-TYPE . "CE")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MSH/C535841")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MSH/M0530302")
  (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A18461418/attributes")
  (:PARENTS . "NONE") (:ANCESTORS) (:CHILDREN . "NONE")
  (:DESCENDANTS) (:RELATIONS . "NONE") (:DEFINITIONS . "NONE")
  (:NAME . "Hashimoto's encephalopathy") (:LANGUAGE . "ENG"))
 ((:CLASS-TYPE . "Atom") (:UI . "A25723879")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDR") (:TERM-TYPE . "LLT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDR/10069432")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A25723879/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Hashimoto's encephalopathy")
  (:LANGUAGE . "ENG"))
 ((:CLASS-TYPE . "Atom") (:UI . "A25868201")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRGER") (:TERM-TYPE . "LLT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRGER/10069432")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A25868201/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Hashimoto-Enzephalopathie")
  (:LANGUAGE . "GER"))
 ((:CLASS-TYPE . "Atom") (:UI . "A17281016")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRGER") (:TERM-TYPE . "PT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRGER/10069432")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A17281016/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Hashimoto-Enzephalopathie")
  (:LANGUAGE . "GER"))
 ((:CLASS-TYPE . "Atom") (:UI . "A25884872")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRDUT") (:TERM-TYPE . "LLT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRDUT/10069432")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A25884872/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Hashimoto-encefalopathie")
  (:LANGUAGE . "DUT"))
 ((:CLASS-TYPE . "Atom") (:UI . "A17281884")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRDUT") (:TERM-TYPE . "PT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRDUT/10069432")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A17281884/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Hashimoto-encefalopathie")
  (:LANGUAGE . "DUT"))
 ((:CLASS-TYPE . "Atom") (:UI . "A26244564")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRCZE") (:TERM-TYPE . "LLT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRCZE/10069432")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A26244564/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Hashimotova encefalopatie")
  (:LANGUAGE . "CZE"))
 ((:CLASS-TYPE . "Atom") (:UI . "A17282638")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRCZE") (:TERM-TYPE . "PT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRCZE/10069432")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A17282638/relations")
  (:DEFINITIONS . "NONE") (:NAME . "Hashimotova encefalopatie")
  (:LANGUAGE . "CZE"))
 ((:CLASS-TYPE . "Atom") (:UI . "A18458317")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MSH") (:TERM-TYPE . "CE")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MSH/C535841")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MSH/M0530302")
  (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A18458317/attributes")
  (:PARENTS . "NONE") (:ANCESTORS) (:CHILDREN . "NONE")
  (:DESCENDANTS) (:RELATIONS . "NONE") (:DEFINITIONS . "NONE")
  (:NAME
   . "Steroid-responsive encephalopathy associated with autoimmune thyroiditis")
  (:LANGUAGE . "ENG"))
 ((:CLASS-TYPE . "Atom") (:UI . "A26134711")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRDUT") (:TERM-TYPE . "LLT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRDUT/10075691")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A26134711/relations")
  (:DEFINITIONS . "NONE") (:NAME . "auto-immune encefalopathie")
  (:LANGUAGE . "DUT"))
 ((:CLASS-TYPE . "Atom") (:UI . "A24658029")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRDUT") (:TERM-TYPE . "PT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRDUT/10075691")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A24658029/relations")
  (:DEFINITIONS . "NONE") (:NAME . "auto-immune encefalopathie")
  (:LANGUAGE . "DUT"))
 ((:CLASS-TYPE . "Atom") (:UI . "A26034843")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRDUT") (:TERM-TYPE . "LLT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRDUT/10072378")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A26034843/relations")
  (:DEFINITIONS . "NONE") (:NAME . "encefalitis auto-immuun")
  (:LANGUAGE . "DUT"))
 ((:CLASS-TYPE . "Atom") (:UI . "A20233620")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRDUT") (:TERM-TYPE . "PT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRDUT/10072378")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A20233620/relations")
  (:DEFINITIONS . "NONE") (:NAME . "encefalitis auto-immuun")
  (:LANGUAGE . "DUT"))
 ((:CLASS-TYPE . "Atom") (:UI . "A17070362")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "SCTSPA") (:TERM-TYPE . "PT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/SCTSPA/95643007")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/SCTSPA/95643007")
  (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A17070362/attributes")
  (:PARENTS . "NONE") (:ANCESTORS) (:CHILDREN . "NONE")
  (:DESCENDANTS) (:RELATIONS . "NONE") (:DEFINITIONS . "NONE")
  (:NAME . "encefalitis autoinmunitaria") (:LANGUAGE . "SPA"))
 ((:CLASS-TYPE . "Atom") (:UI . "A17056199")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "SCTSPA") (:TERM-TYPE . "FN")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/SCTSPA/95643007")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/SCTSPA/95643007")
  (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A17056199/attributes")
  (:PARENTS . "NONE") (:ANCESTORS) (:CHILDREN . "NONE")
  (:DESCENDANTS) (:RELATIONS . "NONE") (:DEFINITIONS . "NONE")
  (:NAME . "encefalitis autoinmunitaria (trastorno)")
  (:LANGUAGE . "SPA"))
 ((:CLASS-TYPE . "Atom") (:UI . "A25978556")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRDUT") (:TERM-TYPE . "LLT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRDUT/10014582")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A25978556/relations")
  (:DEFINITIONS . "NONE")
  (:NAME . "encephalitis allergisch (auto-immuun)")
  (:LANGUAGE . "DUT"))
 ((:CLASS-TYPE . "Atom") (:UI . "A26217897")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRJPN") (:TERM-TYPE . "LLT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRJPN/10014582")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A26217897/relations")
  (:DEFINITIONS . "NONE") (:NAME . "アレルギー性脳炎（自己免疫性）")
  (:LANGUAGE . "JPN"))
 ((:CLASS-TYPE . "Atom") (:UI . "A26377820")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRJPN") (:TERM-TYPE . "LLT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRJPN/10069432")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A26377820/relations")
  (:DEFINITIONS . "NONE") (:NAME . "橋本脳症") (:LANGUAGE . "JPN"))
 ((:CLASS-TYPE . "Atom") (:UI . "A18422619")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRJPN") (:TERM-TYPE . "PT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRJPN/10069432")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A18422619/relations")
  (:DEFINITIONS . "NONE") (:NAME . "橋本脳症") (:LANGUAGE . "JPN"))
 ((:CLASS-TYPE . "Atom") (:UI . "A26408632")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRJPN") (:TERM-TYPE . "LLT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRJPN/10072378")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A26408632/relations")
  (:DEFINITIONS . "NONE") (:NAME . "自己免疫性脳炎") (:LANGUAGE . "JPN"))
 ((:CLASS-TYPE . "Atom") (:UI . "A20574688")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRJPN") (:TERM-TYPE . "PT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRJPN/10072378")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A20574688/relations")
  (:DEFINITIONS . "NONE") (:NAME . "自己免疫性脳炎") (:LANGUAGE . "JPN"))
 ((:CLASS-TYPE . "Atom") (:UI . "A26228689")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRJPN") (:TERM-TYPE . "LLT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRJPN/10075691")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A26228689/relations")
  (:DEFINITIONS . "NONE") (:NAME . "自己免疫性脳症") (:LANGUAGE . "JPN"))
 ((:CLASS-TYPE . "Atom") (:UI . "A24640834")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRJPN") (:TERM-TYPE . "PT")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRJPN/10075691")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A24640834/relations")
  (:DEFINITIONS . "NONE") (:NAME . "自己免疫性脳症") (:LANGUAGE . "JPN"))
 ((:CLASS-TYPE . "Atom") (:UI . "A26308227")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRJPN") (:TERM-TYPE . "LLTJKN")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRJPN/10014582")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A26308227/relations")
  (:DEFINITIONS . "NONE") (:NAME . "ｱﾚﾙｷﾞｰｾｲﾉｳｴﾝｼﾞｺﾒﾝｴｷｾｲ")
  (:LANGUAGE . "JPN"))
 ((:CLASS-TYPE . "Atom") (:UI . "A26438689")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRJPN") (:TERM-TYPE . "LLTJKN")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRJPN/10072378")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A26438689/relations")
  (:DEFINITIONS . "NONE") (:NAME . "ｼﾞｺﾒﾝｴｷｾｲﾉｳｴﾝ")
  (:LANGUAGE . "JPN"))
 ((:CLASS-TYPE . "Atom") (:UI . "A20575113")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRJPN") (:TERM-TYPE . "PTJKN")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRJPN/10072378")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A20575113/relations")
  (:DEFINITIONS . "NONE") (:NAME . "ｼﾞｺﾒﾝｴｷｾｲﾉｳｴﾝ")
  (:LANGUAGE . "JPN"))
 ((:CLASS-TYPE . "Atom") (:UI . "A26258495")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRJPN") (:TERM-TYPE . "LLTJKN")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRJPN/10075691")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A26258495/relations")
  (:DEFINITIONS . "NONE") (:NAME . "ｼﾞｺﾒﾝｴｷｾｲﾉｳｼｮｳ")
  (:LANGUAGE . "JPN"))
 ((:CLASS-TYPE . "Atom") (:UI . "A24642015")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRJPN") (:TERM-TYPE . "PTJKN")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRJPN/10075691")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A24642015/relations")
  (:DEFINITIONS . "NONE") (:NAME . "ｼﾞｺﾒﾝｴｷｾｲﾉｳｼｮｳ")
  (:LANGUAGE . "JPN"))
 ((:CLASS-TYPE . "Atom") (:UI . "A26287489")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRJPN") (:TERM-TYPE . "LLTJKN")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRJPN/10069432")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A26287489/relations")
  (:DEFINITIONS . "NONE") (:NAME . "ﾊｼﾓﾄﾉｳｼｮｳ") (:LANGUAGE . "JPN"))
 ((:CLASS-TYPE . "Atom") (:UI . "A18426084")
  (:SUPPRESSIBLE . "false") (:OBSOLETE . "false")
  (:ROOT-SOURCE . "MDRJPN") (:TERM-TYPE . "PTJKN")
  (:CODE
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/source/MDRJPN/10069432")
  (:CONCEPT
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/CUI/C0393639")
  (:SOURCE-CONCEPT . "NONE") (:SOURCE-DESCRIPTOR . "NONE")
  (:ATTRIBUTES . "NONE") (:PARENTS . "NONE") (:ANCESTORS)
  (:CHILDREN . "NONE") (:DESCENDANTS)
  (:RELATIONS
   . "https://uts-ws.nlm.nih.gov/rest/content/2016AA/AUI/A18426084/relations")
  (:DEFINITIONS . "NONE") (:NAME . "ﾊｼﾓﾄﾉｳｼｮｳ") (:LANGUAGE . "JPN")))
|#
