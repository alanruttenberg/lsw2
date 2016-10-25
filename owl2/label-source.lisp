(defparameter *ohd-label-source* nil)

;; An easier way to choose to make a label source
;;  (make-instance 'label-source :key :obi :sources '("~/repos/obi/trunk/src/ontology/branches/obi.owl"))
;; This will make !'assay'@obi work
;; This remains compatible with making a custom label resolver by specializing make-uri-from-label-source with the first argument being an (eql :key)

(defclass label-source ()
  ((sources :initarg :sources :initform nil :accessor sources)
   (label2uri :initform (make-hash-table :test 'equalp) :accessor label2uri)
   (key :initarg :key :initform nil :accessor key)
   (key2instance :initarg :key2instance :accessor key2instance :allocation :class :initform nil)
   (uri2label :initarg :uri2label :initform nil :accessor uri2label)
   (ignore-obsolete :initarg :ignore-obsolete :initform t :accessor ignore-obsolete)
   ))

(defmethod initialize-instance ((ls label-source) &rest ignore)
  (call-next-method)
  (let ((table (label2uri ls)))
    (loop for el in (sources ls)
	   do
	   (let ((kb (if (v3kb-p el) el  (load-ontology (if (symbolp el) 
							    (format nil "http://purl.obolibrary.org/obo/~a.owl" (string-downcase el))
							     (namestring (truename el)))))))
	     (let ((labels (rdfs-labels kb)))
	       (setf (uri2label ls) (v3kb-uri2label kb))
	       (maphash (lambda(uri label) 
			  (let ((label (car label)))
			    (if (gethash label table) 
				(unless (eq (gethash label table)  uri)
				  (warn "Uri label ~a and ~a are both for ~a" (or (and (keywordp (gethash label table))(gethash label table))
										  (uri-full (gethash label table)) )
					(or (and (keywordp uri) uri) (uri-full uri)) label) 
				  (setf (gethash label table) :ambiguous))
				(setf (gethash label table) uri))))
			labels)))))
  (unless (slot-boundp ls 'key2instance) (setf (key2instance ls) nil))
  (setf (key2instance ls) (remove (key ls) (key2instance ls) :key 'car))
  (push (cons (key ls) ls) (key2instance ls)))

(defmethod label-from-uri ((source symbol) uri)
  (label-from-uri (cdr (assoc source (key2instance (mop:class-prototype (find-class 'label-source))))) uri))

(defmethod new-label-source ((source v3kb) &rest args)
  (let ((key (getf args :key))
	(proto (mop:class-prototype (find-class 'label-source))))
    (setf (key2instance proto) (remove key (key2instance proto) :key 'car))
    (push (cons key source) (key2instance proto))))

(defmethod new-label-source ((source symbol) &rest args)
  (apply 'make-instance 'label-source :key source args))

(defmethod label-from-uri ((source label-source) uri)
  (let ((label? (car (gethash uri (uri2label label-source)))))
      (when (string= label? "")
	(warn "empty label for ~a" uri))
      (unless (eq (gethash label? (label2uri label-source)) :ambiguous)
	label?)))

(defmethod label-from-uri ((source v3kb) uri)
  (entity-label uri source))

(defmethod make-uri-from-label-source ((source symbol) name &optional actual)
  (let ((instance (cdr (assoc source (key2instance (mop:class-prototype (find-class 'label-source)))))))
    (unless instance
      (error "don't know label source '~s'" () source))
    (make-uri-from-label-source instance name actual)))

(defmethod make-uri-from-label-source ((instance label-source) name &optional actual)
  (let ((table (label2uri instance)))
    (let ((found (gethash name table)))
      (when (eq found :ambiguous)
	(progn
	  (warn "Uri label ~a in ~a is ambiguous" name instance)
	  (setq found nil)))
      (if found
	  (progn
	    (when actual
	      (assert (eq found (make-uri nil actual)) (found actual)
		      "Uri label lookup for '~a' - ~a doesn't match specified actual '~a'"
		      name found actual))
	    found)
	  (if actual
	      (progn
		(warn "Uri label lookup for '~a' failed - using provided actual: '~a'" name actual)
		(make-uri nil actual))
	      (error "Couldn't determine which URI was meant by '~a' in ~a" name instance))))))

;; not the best method -should cache, but works.
(defmethod make-uri-from-label-source ((instance v3kb) name &optional actual)
  (make-uri (#"toString" (#"getIRI" (to-class-expression (if (find #\space name) (concatenate 'string "'" name "'") name) instance)))))

;; well, cache here
(defun-memo label-uri (label &optional (ont *default-kb*))
  (make-uri-from-label-source ont label))

(defun-memo uri-label (label &optional (ont *default-kb*))
  (label-from-uri ont label))

(defun compare-ontology-rdfs-labels (ont1 ont2)
  "Compares the rdfs:labels for the uris in two ontologies, and prints to the screen the uris (and the uri's label) in which the labels of each ontology do not match."
  (let ((ont1-labels nil)
	(ont2-labels nil)
	(diff-labels nil))

    ;; get the (uri rdfs:label) list of each ontology
    (setf ont1-labels 
	  (loop 
	     for k being the hash-keys in (rdfs-labels ont1) using (hash-value v) 
	     collect (format nil "~a ~a" k (car v))))
	     ;;collect (format nil "~a" (car v))))

    (setf ont2-labels 
	  (loop 
	     for k being the hash-keys in (rdfs-labels ont2) using (hash-value v)
	     collect (format nil "~a ~a" k (car v))))
	     ;;collect (format nil "~a" (car v))))


    ;; get the set difference of each
    (setf diff-labels (set-difference ont1-labels ont2-labels :test 'equalp))
    
    ;; return the diff labels (if any)
    diff-labels))
	
;; quick way to print some uris in label form
(defun print-uris-from (key &rest uris) 
  (let ((*print-uri-with-labels-from* (list key)))
    (loop for uri in uris do (terpri) (format t "~a ~a" (uri-full uri) uri)))
  (values))

(defun rdfs-label (uri &optional (kb *default-kb*))
  (or (gethash (if (stringp uri) (make-uri uri) uri) (rdfs-labels kb))
      (list (#"replaceAll" (if (stringp uri) uri (uri-full uri)) ".*[/#]" ""))))

(defun an-rdfs-label (uri &optional (kb *default-kb*))
  (or (car (gethash (if (stringp uri) (make-uri uri) uri) (rdfs-labels kb)))
      (#"replaceAll" (if (stringp uri) uri (uri-full uri)) ".*[/#]" "")))
  
  
(defun rdfs-labels (kb &optional (ignore-obsoletes t) force-refresh)
  (or (and (not force-refresh) (v3kb-uri2label kb))
      (flet ((get-labels (clauses)
	       (when ignore-obsoletes
		 (setq clauses (append  clauses (list `(:optional (?uri !owl:deprecated ?dep))))))
	       (or
		(and *classtree-preferred-language*
		     (sparql `(:select (?uri ?label) () ,@clauses
				       (:filter (and (equal (lang ?label) ,*classtree-preferred-language*)
						     (not (isblank ?uri))
						     (not (bound ?dep))))) ;; TODO filter out obsoletes
			     :kb kb :use-reasoner *annotation-query-reasoner*))
		(sparql `(:select (?uri ?label) ()
				  ,@clauses
				  (:filter (and (not (isblank ?uri)) (not (bound ?dep)))))
			:kb kb :use-reasoner *annotation-query-reasoner*))))
	(setf (v3kb-uri2label kb)
	      (loop with table = (make-hash-table)
		 for (uri label) in 
		 (or
		  (get-labels '((?uri !rdfs:label ?label)))
		  (get-labels '((?uri !foaf:name ?label)))
		  (get-labels '((?uri !swan:title ?label))))
		 for clean-label = (clean-label label t)
		 when clean-label do
		   (pushnew clean-label (gethash uri table ) :test 'equalp)
		 finally (return table))))))

(defvar *label-properties*
  (list !rdfs:label
	!foaf:name
	!swan:title
	!skos:prefLabel
	!skos:hiddenLabel 
	!skos:altLabel
	!snomed:Description.term.en-us.preferred
	!snomed:Description.term.en-us.synonym
	!oboinowl:hasExactSynonym
	!oboinowl:hasRelatedSynonym
	!oboinowl:hasBroaderSynonym
	!oboinowl:hasNarrowSynonym
	))

(defvar	*accession-properties*
  (list !oboinowl:id	  
	!oboinowl:hasAlternativeId
	!oboinowl:hasDbXref))

(defun labels-matching(regex &optional (ont *default-kb*))
  (let ((them nil)
	(re (#"compile" 'regex.Pattern regex)))
    (maphash (lambda(uri label)
	       (when (#"matches" (#"matcher" re (car label))) (push (car label) them)))
	     (rdfs-labels ont))
    them))

(defun to-labels (uris &optional (kb *default-kb*))
  (loop for uri in uris do (princ (car (rdfs-label uri kb))) (terpri)))




#|

#"hasLang
#"getLang

(defun entity-annotations (uri kb &optional prop)
  (loop for ont in (set-to-list (#"getImportsClosure" (v3kb-ont kb)))
     append
     (let ((annots (set-to-list (#"getAnnotations" 'EntitySearcher (#"getOWLNamedIndividual" (v3kb-datafactory kb) (to-iri uri)) ont))))
       (loop for annot in annots
	  for property = (#"toString" (#"getIRI" (#"getProperty" annot)))
	  for value = (#"getValue" annot)
	  for prop-uri = (make-uri property)
	  when (or (not prop) (eq prop prop-uri))
	  collect (list  prop-uri
			 (if (jclass-superclass-p (find-java-class "OWLLiteral") (jobject-class value))
			     (cond ((#"isRDFPlainLiteral" value) (#"getLiteral" value))
				   ((#"isBoolean" value) (let ((string (#"getLiteral" value)))
							   (if (equal string "true") :true :false)))
				   ((equal (#"toString" (#"getDatatype" value)) "xsd:string")
				    (#"getLiteral" value))
				   (t value))
			     value
			     ))))))

;;;
What do we want for a label api?

Fast
Including or not including imports
label precedence path, including fragments
scan labels
all labels vs one label
default kb
way to augment labels
fuzzy matching

fully label info

label
language
source
property
|#
