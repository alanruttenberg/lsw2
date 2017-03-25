(defparameter *ohd-label-source* nil)

;; An easier way to choose to make a label source
;;  (make-instance 'label-source :key :obi :sources '("~/repos/obi/trunk/src/ontology/branches/obi.owl"))
;; This will make !'assay'@obi work
;; This remains compatible with making a custom label resolver by specializing make-uri-from-label-source with the first argument being an (eql :key)

(defclass label-source ()
  ((sources :initarg :sources :initform nil :accessor sources)
   (label2uri :initform (make-hash-table :test 'equal) :accessor label2uri)
   (key :initarg :key :initform nil :accessor key)
   (key2instance :initarg :key2instance :accessor key2instance :allocation :class :initform nil)
   (uri2label :initarg :uri2label :initform nil :accessor uri2label)
   (ignore-obsolete :initarg :ignore-obsolete :initform t :accessor ignore-obsolete)
   (label-annotation-properties :initarg :label-annotation-properties :initform (list !rdfs:label !foaf:name !swan:title) :accessor label-annotation-properties) ; initform is for backwards compatibility
   ))

;; two uses
;; 1: UI - something to show. First one is fine.
;; 2: Reading !'....' need to be careful that label to uri is 1:1

(defclass careful-label-source (label-source))

(defmethod initialize-instance ((ls label-source) &rest ignore)
  (call-next-method)
  (let ((label2uri (make-hash-table :test 'equal)) ; string key
	(uri2label (make-hash-table :test 'eq)))   ; URIs are interned
    (loop for el in (sources ls)
       do
	 (let ((kb (if (v3kb-p el) 
		       el
		       (load-ontology 
			(if (symbolp el) 
			    (format nil "http://purl.obolibrary.org/obo/~a.owl" (string-downcase el))
			    (namestring (truename el)))))))
	   (each-entity-label
	    kb (label-annotation-properties ls)
	    (lambda(uri prop label)
	      (pushnew uri (gethash label label2uri))
	      (pushnew label (gethash uri uri2label) :test 'equal)
	      ))
	   (setf (label2uri ls) label2uri)
	   (setf (uri2label ls) uri2label)
	   (postprocess ls)))
    (register-self ls)))

;; for careful label sources what we care about are uri to label is 1:many as long as label to uri is 1:1 
(defmethod postprocess ((ls careful-label-source))
  (ensure-unique-label ls)
  )

;; for display label sources the order of the properties matter
(defmethod postprocess ((ls label-source))
  (let ((uri2label-reordered (make-hash-table :size (hash-table-size (uri2label ls)))))
    (maphash (lambda(uri labels)
	       (setf (gethash uri uri2label-reordered) (reverse labels)))
	     (uri2label ls))
    (setf (uri2label ls) uri2label-reordered)))

;; run transformation function on existing labels, and if the result would uniquely identify a URI then add it.
(defmethod augment-labels ((ls careful-label-source) transformation-function)
  (let ((label2uri (label2uri ls))
	(uri2label (uri2label ls)))
    (maphash (lambda(uri labels &aux changed)
	       (let ((potentials (mapcar transformation-function labels)))
		 (loop for potential in potentials
		    when (and (not (member potential labels :test 'equal))
			      (not (gethash potential label2uri)))
		    do
		      (setf (gethash potential label2uri) (list uri))
		      (nconc labels (list potential))
		      (setq changed t)))
	       (when changed
		 (let ((sorted (sort labels '< :key #'length)))
		   (setf (gethash uri uri2label) sorted)
		   )))
	     (uri2label ls))))
	     
(defmethod register-self ((ls label-source))
  (unless (slot-boundp ls 'key2instance) (setf (key2instance ls) nil))
  (setf (key2instance ls) (remove (key ls) (key2instance ls) :key 'car :test 'equal))
  (push (cons (key ls) ls) (key2instance ls)))

(defmethod ensure-unique-label ((ls careful-label-source))
  (let ((label2uri (label2uri ls)))
    (maphash (lambda (uri labels)
	       (declare (ignore uri))
	       (loop for label in labels
		  if (> (length (gethash label label2uri)) 1)
		  do
		    (error "A label is not unique: ~a maps to ~{~a~^, ~}" label (gethash label label2uri))))
	     (uri2label ls))
    ))

(defun all-label-sources () (key2instance (mop:class-prototype (find-class 'label-source))))
	     
;; meant to be fast. <5 seconds for a label across snomed
(defun each-entity-label (kb label-properties fn)
  (unless (consp label-properties) (setq label-properties (list label-properties)))
  (let ((props (mapcar (lambda(p) (#"getOWLAnnotationProperty" (v3kb-datafactory kb) (to-iri p))) label-properties)))
    (with-constant-signature ((iterator "iterator" t) (hasnext "hasNext") (next "next") (getvalue "getValue"))
      (maphash (lambda(uri entry)
		 (declare (optimize (speed 3) (safety 0)))
		 (loop for (entity nil eont) in entry
		    do   
		      (loop for prop in props for propuri in label-properties
			   do
			   (loop with iterator = (iterator (#0"getAnnotations" 'EntitySearcher entity eont prop))
			      while (hasNext iterator)
			      for item = (next iterator)
			      do (funcall fn uri propuri (#"getLiteral" (getvalue item)))))))
	       (v3kb-uri2entity kb)))))
  
(defmethod label-from-uri ((source symbol) uri)
  (label-from-uri (cdr (assoc source (key2instance (mop:class-prototype (find-class 'label-source))))) uri))

(defmethod new-label-source ((source v3kb) &rest args)
  (let ((key (getf args :key))
	(proto (mop:class-prototype (find-class 'label-source))))
    (setf (key2instance proto) (remove key (key2instance proto) :key 'car))
    (push (cons key source) (key2instance proto))))

(defmethod new-label-source ((source symbol) &rest args)
  (apply 'make-instance 'label-source :key source args))

(defmethod new-careful-label-source ((source symbol) &rest args)
  (apply 'make-instance 'careful-label-source :key source :label-annotation-properties (list !rdfs:label) args))

(defmethod label-from-uri ((source label-source) uri)
  (let ((label? (car (gethash uri (uri2label source)))))
    (when (string= label? "")
      (warn "empty label for ~a" uri))
    label?))

(defmethod label-from-uri ((source v3kb) uri)
  (entity-label uri source))

(defmethod make-uri-from-label-source ((source symbol) name &optional actual)
  (let ((instance (cdr (assoc source (key2instance (mop:class-prototype (find-class 'label-source)))))))
    (unless instance
      (error "don't know label source '~s'" () source))
    (make-uri-from-label-source instance name actual)))

(defmethod make-uri-from-label-source ((instance label-source) name &optional actual)
  (let ((table (label2uri instance)))
    (let ((found (car (gethash name table))))
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

;; Assumes that we are using quoted string short form provider. So we add quotes if we get a string with a space
(defmethod make-uri-from-label-source ((instance v3kb) name &optional actual)
  
  (setq name (#"replaceAll" name "'" "''")) ;; idiosyncracy of provider. They don't really need to quote it when it isn't the first or last of string, but hey.
  (let  ((found (#"getEntity" (short-form-provider *snomed*) (if (find #\space name) (concatenate 'string "'" name "'") name))))
    (assert found (name) "Didn't find term called ~a in ~a" name instance)
    (make-uri (#"toString" (#"getIRI" found)))))

;; well, cache here
(defun label-uri (label &optional (ont *default-kb*))
  (make-uri-from-label-source ont label))

;; have to more carefully memoize as the default defun-memo only looks at the first argument and uses eql
'(eval-when (:load-toplevel :execute)
  (memoize 'uri-label :key #'identity :test #'equalp))

(defun uri-label (label &optional (ont *default-kb*))
  (label-from-uri ont label))

'(eval-when (:load-toplevel :execute)
  (memoize 'uri-label :key #'identity :test #'equalp))

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
    (each-entity-label ont (list !rdfs:label) 
		       (lambda(uri prop label)
			      (when (#"matches" (#"matcher" re label)) (push label them)))
		       )
    them))

(defun labels/terms-matching(regex &key (annotation-property !rdfs:label) (ont *default-kb*))
  (let ((them nil)
	(re (#"compile" 'regex.Pattern regex)))
    (each-entity-label ont (list !rdfs:label) 
		       (lambda(uri prop label)
			      (when (#"matches" (#"matcher" re label)) (push (list uri label)  them)))
		       )
    them))

(defun to-labels (uris &optional (kb *default-kb*))
  (loop for uri in uris do (princ (car (rdfs-label uri kb))) (terpri)))

(defun replace-with-labels (sexp &optional (kb *default-kb*))
  (labels ((one (exp)
	     (cond ((atom exp)
		    (if (uri-p exp)
			(or (uri-label exp kb)
			    exp)
			exp))
		   ((consp exp)
		    (mapcar #'one exp))
		   (t exp))))
    (one (eval-uri-reader-macro sexp))))

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
