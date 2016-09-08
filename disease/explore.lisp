;; Good browser http://ontoserver.csiro.au/shrimp

(defun browse-snomed-parent-hierarchy (term &optional (ont *default-kb*) &key (parenthetical nil))
  (let ((snomed-uri (dwim-class term)))
    (let ((label (#"replaceAll" (#"replaceFirst" (label-from-uri ont snomed-uri) " \\(.*" "" ) " " "_")))
      (let ((trees (parents-tree snomed-uri ont)))
	(let ((spec (emit-dagre-d3-javascript trees ont (if parenthetical (if (eq parenthetical 'only) 'keep-only-parenthetical 'identity) 'remove-parenthetical)))
	      (data-path (temp-directory-path (concatenate 'string label ".js"))))
	  (print data-path)
	  (with-open-file (out data-path :direction :output :if-exists :supersede)
	    (write-string spec out))
	  (show-dag data-path))))))

(defun count-diseases-with-snomed-mappings ()
  (with-open-file (f (namestring (truename "disease:data;disease-families.csv")))
    (loop with count = 1
       for line = (read-line f nil :eof)
       until (eq line :eof) 
       for (nil nil nil descendantId nil) = (split-at-char line #\,)
       for atoms = (umls-concept-atoms descendantId)
       when (some #'identity (mapcar (lambda(el) (equal (cdr (assoc  :ROOT-SOURCE el)) "SNOMEDCT_US")) atoms))
       do (incf count)
       when (zerop (mod count 100))
       do (princ ".")
	 finally (return count))))


(defun uncached-source-terms-for-initial-concepts (&aux (todo (make-hash-table :test 'equalp)))
  (with-open-file (f (namestring (truename "disease:data;disease-families.csv")))
    (loop 
       for line = (read-line f nil :eof)
       until (eq line :eof) 
       for (nil nil nil descendantId nil) = (split-at-char line #\,)
       for atoms = (umls-concept-atoms descendantId)
       do
	 (loop for result in atoms
	    for code = (cdr (assoc :code result))
	    for (source id) = (source-term-source-and-id-from-url code)
	    unless (umls-source-term-info source id :probe t)
	    do (setf (gethash code todo) t))))
  todo)


(defun count-cuis ()
  (let ((table (make-hash-table :test 'equalp)))
    (with-open-file (f (namestring (truename "disease:data;disease-families.csv")))
      (loop with diseases = (make-hash-table :test 'equalp)
	 for line = (read-line f nil :eof)
	 until (eq line :eof) 
	 for (familyId familyLabel numDescendants descendantId descendantLabel) = (split-at-char line #\,)
	 do (setf (gethash descendantId table) t)(setf (gethash familyid table) t)))
    table))

(defun dwim-class (thing &optional (ont *default-kb*))
  (if (uri-p thing)
      thing
      (if (stringp thing)
	  (if (#"matches" thing "C\\d+")
	      (make-uri (concatenate 'string "http://snomed.info/id/" (car (snomeds-from-cui thing))))
	      (make-uri (#"toString" (#"getIRI" (parse-manchester-expression ont (if (find #\space thing) (concatenate 'string "'" thing "'") thing)))))))))
	  
(defun all-relevant-axioms (class &optional (ont *default-kb*))
  (remove-if 'null
  (append 
   (mapcar 'fix-unquoted-manchester-label (mapcar 'car (get-rendered-referencing-axioms (dwim-class class ont) :class ont t)))
   (loop for c in (ancestors (dwim-class class ont) ont)
      collect (mapcar 'fix-unquoted-manchester-label (mapcar 'car (get-rendered-referencing-axioms c :class ont t)))
	))))

(defun transform-axioms (rendered-axioms &optional (ont *default-kb*) &key (split-conjunctions t) (remove-noise-classes t) (remove-role-groups t) (remove-parenthetical nil))
 (sort (remove-duplicates
	(remove-if 'null 
		   (mapcan (lambda(el)
			     (funcall (if remove-noise-classes 'remove-noise-classes 'identity)
				      (funcall (if split-conjunctions 'split-conjunctions 'identity)
					       (let ((html el))
						 (funcall (if remove-role-groups 'remove-role-groups 'identity)
							  (manchester-axiom-to-sexp
							   (funcall (if remove-parenthetical 'remove-parenthetical 'identity)
								    (trim-base-term html))))))))
			   rendered-axioms)) 
	:test 'equal)  
	'string-lessp :key (lambda(el) (if (stringp el) el (car el)))))

(defun remove-role-groups (axiom)
  (loop for term in axiom
     if (and (consp term)
	     (or (equal (car term) "Role group")
		 (equal (car term) "Role group (attribute)")))
     if (consp (car (third term)))
     append (third term)
     else
     collect (third term)
     else collect term))

(defparameter *snomed-noise*
  (append
   '("Finding by site"
     "Disorder by body site"
     "Clinical finding"
     "Disease"
     "SNOMED CT Concept"
     "Body organ structure"
     "Body system structure"
     "Body region structure"
     "Anatomical or acquired body structure"
     "Structure of integumentary system"
     "Skin AND subcutaneous tissue structure"
     "Inflammation of specific body systems"
     "Inflammatory morphology")
;;(mapcar 'remove-parenthetical (labels-matching ".* by body site .*"))
  '("Radiotherapy by body site" "Surgical repair procedure by body site" "Connective tissue disorder by body site" "Melanoma in situ by body site" "Allergic disorder by body site affected" "Imaging by body site" "Diathermy procedure by body site" "Neoplasm by body site" "Manipulation procedure by body site" "Disorder by body site" "Stimulation procedure by body site" "Introduction of substance by body site")

;;(mapcar 'remove-parenthetical (labels-matching ".* by site .*"))
   '("Immune system procedure by site" "Diagnostic procedure by site" "Bacterial infection by site" "Dose form by site prepared for" "Finding of sensation by site" "Radiographic imaging procedure by site" "Infected foreign body by site" "Ultrasound studies by site" "Connective tissue by site" "Procedure on respiratory system structure by site" "Infected superficial injury, by site" "Traumatic injury by site" "Fungal infection by site" "Infected insect bite by site" "Finding by site" "Diagnostic procedure on respiratory system structure by site" "Contrast radiology study by site" "Procedure by site" "Viral infection by site" "Tumor invasion by site" "Nuclear medicine study by site" "Thermography by site" "Infection by site")))

(defun remove-noise-classes (axioms)
  (loop for axiom in axioms
;       for dummy = (print-db axiom)
       unless (null axiom)
     append
       (if (and axiom (member  (if (stringp axiom) axiom (third axiom))
		   *snomed-noise*
		   :test 'equalp))
	   nil
	   (if (maybe-exclude-and-or axiom)
	       nil
	       (list axiom)))))

(defun split-conjunctions (axiom)
  (if (and (consp axiom) (member 'AND axiom))
      (loop for (conjunct) on axiom by 'cddr
	 collect conjunct)
      (if (equal (length axiom) 1)
	  (list (car axiom))
	  (list axiom))))

(defun trim-base-term (html)
  (if (consp html)
      (if (> (length html) 1)
	  (break)
	  (trim-base-term (car html)))
      (#"replaceFirst" (#"replaceAll" (#"replaceAll" html "\\n" "") "<[^>]+>" "") "^.*?(SubClassOf|EquivalentTo) " "")))

'(defun trim-base-term (html)
  (print-db html)
  (let ((res (#"replaceFirst" html "^.*?(SubClassOf|EquivalentTo) " "")))
    (print-db res)
    res))

(defun remove-parenthetical (string)
  (#"replaceAll" string "\\s\\([^(]+?\\)" ""))

(defun keep-only-parenthetical (string)
  (if (#"matches" string "(?i).*disease.*")
      "disease"
      (#"replaceAll" string ".*\\s\\(([^(]+?)\\)" "$1")))

(defun sexp-back-to-manchester(sexp)
  (let ((*print-case* :downcase))
    (#"replaceAll" (prin1-to-string sexp) "\"" "'")))


;; ## FIXME Somewhere along the line labels without spaces didn't get quoted
(defun replace-all-protecting (string regex function protected which-protected &rest which)
  (let ((strings-protecting (mapcar 'car (all-matches string protected which-protected))))
    (let ((count -1))
      (let ((protected-string (replace-all string protected (lambda(e)  (format nil "&~a&" (incf count))) 1)))
	(let ((processed-string (apply 'replace-all protected-string regex function which)))
	  (replace-all processed-string "(&\\d+&)" (lambda(token) 
						    (let ((which (parse-integer (#"replaceAll" token "&" ""))))
								   (nth which strings-protecting)))
		       1))))))

(defun fix-unquoted-manchester-label (string)
  (replace-all-protecting
   string 
   "(\\w+)" 
   (lambda(s) 
     (if (and (upper-case-p (char s 0)) (not (member s '("EquivalentTo" "SubClassOf") :test 'equal)))
	 (concatenate 'string "'" s "'")
	 s))
   "('[^']+?')" 1 1))


(defun manchester-axiom-to-sexp (axiom)
  (if (null axiom) nil
  (read (make-string-input-stream  (concatenate 'string "(" (#"replaceAll"  axiom "'" "\"") ")")))))

;; ## FIXME

(defun organize-as-subclasses (sexps subject)
  (let* ((properties nil)
	 (pairs
	  (loop for subject in sexps
	     for subject-ce =  (if (consp subject)
				   (prog1
				       `(object-some-values-from ,(label-uri (car subject)) ,(label-uri (third subject)))
				     (pushnew (label-uri (car subject)) properties))
				   (label-uri subject))
	     append
	       (loop for object in sexps
		  for object-ce = (if (consp object) `(object-some-values-from ,(label-uri (car object)) ,(label-uri (third object))) (label-uri object))
		  when (and
			(or t (not (stringp object))
			    (member object *dont-exclude-classes* :test 'equal))
			(or t (not (stringp subject))
			    (member subject *dont-exclude-classes* :test 'equal))
			(is-subclass-of? subject-ce object-ce))
		  collect (list subject object subject-ce object-ce)))))
;    (print-db pairs)
    (let ((count 0))
      (flet ((fresh-uri ()
	       (make-uri (format nil "http://foo.com/~a" (incf count)))))
	(let ((sexp-to-name (make-hash-table :test 'equalp)))
	  (loop for (subject object sce oce) in pairs
	     do
	       (or (gethash subject sexp-to-name)
		   (setf (gethash subject sexp-to-name)
			 (list (name-for-expression subject) sce (fresh-uri))))
	       (or (gethash object sexp-to-name)
		   (setf (gethash object sexp-to-name)
			 (list (name-for-expression object) oce (fresh-uri)))))
	  (with-ontology reorg ()
	      ((loop for prop in properties
		  do
		    (as `(declaration (object-property ,prop)))
		    (as `(annotation-assertion !rdfs:label ,prop ,(uri-label prop))))
	       (maphash (lambda(exp label-ce-uri)
			  (declare (ignore exp))
			  (let ((label (car label-ce-uri))
				(uri (third label-ce-uri)))
			    (as `(declaration (class ,uri)))
			    (as `(annotation-assertion !rdfs:label ,uri ,(remove-parenthetical label)))
			    (as `(subclass-of ,subject ,uri))
			    (as `(annotation-assertion !rdfs:label ,subject ,(label-from-uri :snomed subject)))))
			sexp-to-name)
	       (as (loop for (subject object nil nil) in pairs
		      collect 
			`(subclass-of ,(third (gethash subject sexp-to-name)) ,(third (gethash object sexp-to-name))))))
	    (browse-subclass-tree "reorg" reorg  )))))))

  
(defparameter *relation-replacements*
  '(("Has definitional manifestation" "Manifestation")
    ("Finding site" "Site")
    ("Associated morphology" "Morphology")
    ("Pathological process" "Pathological")
    ("Causative agent" "Cause")
    ("Clinical course" "Course")))

(defparameter *and-or-override*
  (list "Sudden onset AND/OR short duration (qualifier value)" "Sudden onset AND/OR short duration"))

;; BUG: If we don't exclude a class, we need to remember to associate its axioms with it.
;; Otherwise, for eg. (browse-simplified-snomed-parent-hierarchy !'Systemic sclerosis (disorder)'@snomed)
;; Doesn't know that "Autoimmune disease" is equivalent to pathological process: autoimmune and shows them separately.
(defparameter *dont-exclude-classes* nil); '("Autoimmune disease (disorder)" "Autoimmune disease"))

(defun name-for-expression (sexp)
  (if (stringp sexp)
      sexp
      (let ((rel (or (second (assoc (remove-parenthetical (car sexp)) *relation-replacements* :test 'equal)) (remove-parenthetical (car sexp)))))
	(format nil "~a: ~a" rel (remove-parenthetical (third sexp))))))

(defun browse-simplified-snomed-parent-hierarchy (class)
  (organize-as-subclasses
   (transform-axioms  (all-relevant-axioms class)  *snomed* :remove-parenthetical nil)
   (dwim-class class)))

;; (defun most-specific-axioms (class)

(defun maybe-exclude-and-or (axiom)
  (if (stringp axiom)
       (and (not (member axiom *and-or-override* :test 'equalp))
	    (#"matches" axiom "(?i).*and/or.*"))
        (and (not (member (third axiom) *and-or-override* :test 'equalp))
	    (#"matches" (third axiom) "(?i).*and/or.*"))))
       
;  (read-from-string  (concatenate 'string "(" (substitute #\" #\' "'Vasculitis' and ('Role group' some (('Associated morphology' some 'Inflammation') and ('Finding site' some 'Systemic vascular structure')))") ")"))


(defun tree-count (thing tree &key (test #'eq))
  (cond ((null tree) 0)
	((atom tree)
	 (if (funcall test thing tree) 1 0))
	(t (apply '+ (mapcar (lambda(el) (tree-count thing el :test test)) tree)))))

(defun how-often-more-than-one-role-group (&optional (root !'Disease'@snomed) (ont *default-kb*))
  (loop with memo = (make-hash-table :test 'equal)
     for f in (descendants root ont) do (setq @ memo)
     sum
       (loop for ax in (all-relevant-axioms f ont)
	    for axfix = (if (consp ax) (car ax) ax)
	  unless (gethash axfix memo)
	  when (consp axfix) do (progn (print-db axfix (all-relevant-axioms f ont)) (break))
	  when (> (length (all-matches axfix "(Role group)" 1)) 2) sum 1 into multiple
	  do (setf (gethash axfix memo) (length (all-matches axfix "(Role group)" 1)))
	   finally (return (values multiple memo)))))

(defun top-by-count (table n)
  (subseq (sort (let ((them nil)) (maphash (lambda(k v) (push (list k v) them)) table) them) '> :key 'second) 0 n))
