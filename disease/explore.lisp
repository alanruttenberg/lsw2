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
	      (label-uri thing)))))

;	      (make-uri (#"toString" (#"getIRI" (parse-manchester-expression ont (if (find #\space thing) (concatenate 'string "'" thing "'") thing)))))))))
	  

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


       
;  (read-from-string  (concatenate 'string "(" (substitute #\" #\' "'Vasculitis' and ('Role group' some (('Associated morphology' some 'Inflammation') and ('Finding site' some 'Systemic vascular structure')))") ")"))




(defun top-by-count (table n)
  (subseq (sort (let ((them nil)) (maphash (lambda(k v) (push (list k v) them)) table) them) '> :key 'second) 0 n))
