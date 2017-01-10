;; http://owlapi.svn.sourceforge.net/viewvc/owlapi/v3/trunk/examples/src/main/java/org/coode/owlapi/examples/Example8.java?view=markup

(defstruct (v3kb (:print-function print-v3kb-struct))
  name ;; uri (if loaded from URI) or symbol (if created with with-ontology)
  manager ;; instance of OWLOntologyManager
  ont ;; instance of OWLOntology
  reasoner ;; instance of OWLReasoner
  datafactory ;; instance of OWLDataFactory
  reasoner-factory 
  hermit-monitor ;; hermit specific performance instrumentation 
  short-form-provider ;; instance of short form provider mapping to labels
  manchester-parser ;; instance of manchester parse
  manchester-renderer ;; instance of manchester frame renderer
  manchester-renderer-writer ;; stringwriter
  pellet-jena-model ;; for use with sparql dl
  told-jena-model ;; for use with reasoner :none
  uri2label ;; entity to label cache
  uri2entity ;; uri2entity - 
  changes ;; to collect ontology changes before doing an update
  weakened-from ;; when creating a weakened kb, link to what it was weakened from
  default-reasoner ;; (:factpp :hermit :pellet) - can be overidden by an explicit call to check ontology or instantiate reasoner
  mapper 
  ;; for OWLBGP
  sparql-ontology-graph
  sparql-engine
  sparql-dataset
  )

;; http://docs.oracle.com/javase/tutorial/jaxp/limits/limits.html
;; the command line switch -DentityExpansionLimit doesn't seem to work any more.

(defvar *default-reasoner* :hermit)

(defvar *default-kb*)

(defvar *last-jena-model* nil) ; for the last with-ontology form

(defmethod jena-model ((o v3kb))
  (or (v3kb-told-jena-model o) 
      (setf (v3kb-told-jena-model o) (to-jena-model o))))

(defvar *default-rdfxml-writing-location* "~/Desktop/")



(defun print-v3kb-struct (kb stream depth)
  (print-unreadable-object (kb stream)
    (if (consp (v3kb-name kb))
	;; hack for faking out kb with just jena model
	(format stream "~a on ~a" (car (v3kb-name kb)) (second (v3kb-name kb)))
	(format stream "OWLAPIv3 KB on ~a" (v3kb-name kb)))))

(defun to-iri (thing)
  (cond ((and (stringp thing) (not (consp (pathname-host thing))) (probe-file thing))
	 (#"create" 'org.semanticweb.owlapi.model.IRI
		    (new 'java.io.file (namestring (truename
						    (merge-pathnames 
						     thing (format nil "~a/" (jstatic "getProperty" "java.lang.System" "user.dir"))))))))
	((stringp thing)
	 (#"create" 'org.semanticweb.owlapi.model.IRI (coerce thing 'simple-base-string))) ; yuck!!
	((uri-p thing)
	 (to-iri (uri-full thing)))
	((and (java-object-p thing)
	      (member (#"getClass" thing)
		      (load-time-value
		       (list
			(find-java-class 'java.net.uri)
			(find-java-class 'java.net.url)
			(find-java-class 'java.io.file)))
		      :test 'equal))
	 (#"create" 'org.semanticweb.owlapi.model.IRI thing))
	((jinstance-of-p thing (find-java-class "org.semanticweb.owlapi.model.IRI")) thing)
	(t (error "don't know how to coerce ~s to IRI" thing))))

(defun load-ontology (source &key name reasoner (silent-missing t))
;  (set-java-field 'OWLRDFConsumer "includeDublinCoreEvenThoughNotInSpec" nil)
;  (set-java-field 'ManchesterOWLSyntaxEditorParser "includeDublinCoreEvenThoughNotInSpec" nil)
  (if (uri-p source) (setq source (uri-full source)))
  (#"setProperty" 'system "jdk.xml.entityExpansionLimit" "100000000")
  (#"setProperty" 'system "entityExpansionLimit" "100000000") ; avoid low limit as we are not worried about security
  (#"setProperty" 'system "factpp.jni.path" *factpp-jni-path*)
  (let ((mapper nil)
	(uri nil))
    (setq source (if (java-object-p source) (#"toString" source) source))
    (when (or (stringp source) (uri-p source) )
      (setq uri source)
      (when (and (not (consp (pathname-host uri))) (probe-file uri))
	(let ((dir (make-pathname :directory (pathname-directory uri))))
	  (setq mapper (uri-mapper-for-source source))
	  ;(setq uri (format nil "file://~a" (truename uri)))
	  ;(list (length (set-to-list (#"getOntologyIRIs" mapper))) uri)
	  )))
    (let* ((manager (#"createOWLOntologyManager" 'org.semanticweb.owlapi.apibinding.OWLManager)))
;      (#"setSilentMissingImportsHandling" manager silent-missing)
      (and mapper (#"addIRIMapper" manager mapper))
      (let ((ont
	     (if uri
		 (#"loadOntologyFromOntologyDocument" manager (to-iri uri))
		 (#"loadOntologyFromOntologyDocument" manager (new 'java.io.ByteArrayInputStream
								   (if (consp source)
								       (if (keywordp (car source))
									   (#0"toString" (second source))
									   (let ((model (apply 't-jena (maybe-reorder-assertions source) nil)))
									     (let ((sw (new 'StringWriter)))
									       (setq *last-jena-model*  model)
									       (#"write" model sw "RDF/XML")
									       (#0"getBytes" (#0"toString" sw) "UTF-8"))))
								       (#0"toString" source))))
		 )))
	(let ((it (make-v3kb :name (or uri name) :manager manager :ont ont :datafactory (#"getOWLDataFactory" manager) :default-reasoner reasoner :mapper mapper)))
	  (setf (v3kb-uri2entity it) (compute-uri2entity it))
	  it
	)))))

(defun maybe-reorder-assertions (assertions &aux ontology-iri version-iri)
  "pull out two place annotations - ontology annotations, and import assertions, and put them at the start"
  (setq assertions (rest assertions)) ; pop 'ontology
  (and (and (null (car assertions)) (uri-p (second assertions)))
       (error "If you supply a version iri you need to supply an ontology iri"))
  (when (uri-p (car assertions))
    (setq ontology-iri (pop assertions))
    (when (uri-p (car assertions)) 
      (setq version-iri (pop assertions))))
  (destructuring-bind (imports ont-annotations rest)
      (loop for assertion in assertions
	 if (memq (car assertion) '(import imports))
	 collect assertion into imports
	 else if (memq (car assertion) '(annotation ontology-annotation))
	 collect assertion into annotations
	 else collect assertion into rest
	   finally (return (list imports annotations rest)))
    `(ontology ,@(and ontology-iri (list ontology-iri))
	       ,@(and version-iri (list version-iri))
	       ,@imports ,@ont-annotations ,@rest)))
    

; http://jena.apache.org/documentation/javadoc/jena/com/hp/hpl/jena/rdf/model/Model.html
; Predefined values for lang are "RDF/XML", "N-TRIPLE", "TURTLE" (or "TTL") and "N3". null represents the default language, "RDF/XML". "RDF/XML-ABBREV" is a synonym for "RDF/XML". 

(defun load-kb-jena (file &key (format "RDF/XML"))    
  (let ((url (if (search "//" file :test 'equalp) file (format nil "file://~a" (namestring (truename file)))))
	(in-model (#"createDefaultModel" 'hp.hpl.jena.rdf.model.ModelFactory)))
    (setq @ in-model)
    (#"read" in-model
	     (new 'bufferedinputstream
		  (#"getInputStream" (#"openConnection" (new 'java.net.url url))))
	     url format)
    in-model))

(defmacro collecting-axioms (&rest body)
  `(let ((as nil))
     (flet ((as (&rest axioms)
	      (loop for a in axioms
		 do (if (consp (car a))
			(dolist (aa a) (and aa (push aa as)))
			(and a
			     (push a as))))
	      ))
       (macrolet ((asq (&rest axioms)
		    `(as ',axioms)))
	 ,@body
	 (reverse as)))))

(defmacro def-metadata (prop ont &rest prop-val)
  `(setf (getf (get ',ont 'metadata) ',prop)
	 (append
	  (loop for (key val) on (getf (get ',ont 'metadata) ',prop) by #'cddr
	     when (not (getf ',prop-val key))
	     collect key collect val)
	  ',prop-val)))

(defmacro def-axiom (number ont axiom &optional doc)
  `(setf (getf (get ',ont 'axioms) ',number)
	 `(:number ,,number :documentation ,,doc :axioms ,',axiom)))

(defmacro with-label-vars-from (ont &body body)
  (let ((ontvar (make-symbol "ONT"))
	(classes (make-symbol "CLASSES")))
    `(let* ((,ontvar (load-ontology ,ont))
	   (,classes (mapcar (lambda(e) 
			       (list (intern (string-upcase (substitute #\- #\space (second e))))
				     (first e)))
			     (sparql '(:select (?class ?label) ()
				       (?class !rdf:type !owl:Class) 
				       (?class !rdfs:label ?label))
				     :kb ,ontvar :use-reasoner :none))))
       (progv (mapcar 'first ,classes) (mapcar 'second ,classes)
	 ,@body))))

(defmacro with-ontology (name (&key base ontology-properties about includes rules eval (collecting t) also-return-axioms only-return-axioms
				 ontology-iri version-iri) definitions &body body)
  (declare (ignore rules includes))
  (let ((axioms-var (make-symbol "AXIOMS"))
	(oiri (make-symbol "ONTOLOGY-IRI"))
	(viri (make-symbol "VERSION-IRI")))
    `(let* ((*default-uri-base* (or ,(cond ((stringp base) base) ((uri-p base) (uri-full base)))  *default-uri-base* ))
	    (,axioms-var nil)
	    (,oiri (or ,ontology-iri (make-uri (format nil "urn:lsw:ontology:~a" (string-downcase (string ',name))))))
	    (,viri ,version-iri))
       (when (stringp ,oiri) (setq ,oiri (make-uri ,oiri)))
       (when (stringp ,viri) (setq ,viri (make-uri ,viri)))
       (let ((,name 
	      (funcall (if ,only-return-axioms 'list 'load-ontology)
		       (append (list* 'ontology
			      ,@(if (or about oiri) (list (or about oiri)))
			      ,@(if viri (list viri))
			      ,ontology-properties)
		       ,(cond (eval definitions)
			      (collecting `(setq ,axioms-var (collecting-axioms ,@definitions
										(include-out-of-line-metadata ',name #'as)
										(include-out-of-line-axioms ',name #'as)
										)))
			      (t (list 'quote definitions)))
		       )
	       :name ',name
	       )))
	 (let ((*default-kb* ,name))
	   (declare (special *default-kb*))
;	   (declare (ignorable *default-kb* ))
	   (flet ((write-ontology (&optional (pathname (make-pathname  :name (string-downcase (string (v3kb-name *default-kb*))) :type "owl" :directory (pathname-directory "~/Desktop/"))))
		    (jena-serialize-to-file *last-jena-model* "RDF/XML-ABBREV" pathname)))
	     (values-list (append (multiple-value-list (progn ,@body)) (and ,also-return-axioms (list ,axioms-var))))))))))

(defun include-out-of-line-axioms (name as-fn))

(defun include-out-of-line-metadata (name as-fn )
  nil)
;; (let ((definition !obo:IAO_0000115)
;; 	(alternative-term !obo:IAO_0000118)
;; 	(example-of-usage !obo:IAO_0000112)
;; 	(editor-preferred-label !obo:IAO_0000111)
;; 	(editor-note  !obo:IAO_0000116)
;; 	(axiom-id !obo:IAO_0010000))
;;     (loop for (prop pairs) in (get name 'metadata)
;;        do 
;;        (loop for (annotprop value) in pairs
;; 	    (funcall as-fn
;; 		     `(annotation-assertion ,(prop-uri annotprop) ,(entity-uri prop) ,(value-form value)))

;; 	    ))))
 



;;; *classpath-manager* is unbound(if (#"getBaseLoader" *classpath-manager*)
;;; *classpath-manager* is unbound    (defmethod print-object ((obj (jclass "org.semanticweb.HermiT.Reasoner" (#"getBaseLoader" *classpath-manager*))) stream) 
;;; *classpath-manager* is unbound      (print-unreadable-object (obj stream :identity t)
;;; *classpath-manager* is unbound	(format stream "org.semanticweb.HermiT.Reasoner"))))

(defmethod print-object ((obj (jclass "java.lang.Class")) stream) 
  (print-unreadable-object (obj stream :identity t)
    (format stream "java class ~a" (jclass-name obj))))

(defun pellet-reasoner-config ()
  (let ((standard (new 'SimpleConfiguration))
	(progressMonitor (new 'owlapi.reasoner.ConsoleProgressMonitor)))
    (jss::set-java-field 'PelletOptions "USE_ANNOTATION_SUPPORT" +true+)
    (jss::set-java-field 'pelletoptions "TREAT_ALL_VARS_DISTINGUISHED" +false+)
    (new 'org.semanticweb.owlapi.reasoner.SimpleConfiguration progressMonitor
	 (#"getFreshEntityPolicy" standard)
	 (new 'long (#"toString" (#"getTimeOut" standard)))
	 (#"valueOf" 'individualNodeSetPolicy "BY_SAME_AS")
	 )))

(defun vanilla-reasoner-config ()
  (let ((standard (new 'SimpleConfiguration))
	(progressMonitor (new 'owlapi.reasoner.ConsoleProgressMonitor)))
    (let ((it (new 'org.semanticweb.owlapi.reasoner.SimpleConfiguration progressMonitor
		   (#"getFreshEntityPolicy" standard)
		   (new 'long "9223372036854775807")
		   (#"valueOf" 'individualNodeSetPolicy "BY_SAME_AS")
		   )))
;      (setq @ it)
;      (set-java-field it "ignoreUnsupportedDatatypes" +true+ t)
      it)))

(defun quiet-reasoner-config ()
  (let ((standard (new 'SimpleConfiguration (new 'NullReasonerProgressMonitor)))
	(progressMonitor (new 'owlapi.reasoner.ConsoleProgressMonitor)))
    (new 'org.semanticweb.owlapi.reasoner.SimpleConfiguration progressMonitor
	 (#"getFreshEntityPolicy" standard)
	 (new 'long "9223372036854775807")
	 (#"valueOf" 'individualNodeSetPolicy "BY_SAME_AS")
	 )))

(defun factpp-reasoner-config ()
  (vanilla-reasoner-config))

(defun jfact-reasoner-config ()
  (vanilla-reasoner-config))

(defun hermit-reasoner-config (&optional profile ont timeout)
  (if profile
      (let ((new (new 'org.semanticweb.HermiT.Configuration))
	    (monitor (new 'org.semanticweb.HermiT.monitor.CountingMonitor)))
	(jss::set-java-field new "monitor" monitor)
	(setf (v3kb-hermit-monitor ont) monitor)
	(and timeout (set-java-field new "individualTaskTimeout" (new 'long (prin1-to-string timeout))))
	new)
      (let ((it (new 'SimpleConfiguration (new 'owlapi.reasoner.ConsoleProgressMonitor) )))
	it)))

(defun elk-reasoner-config ()
  (vanilla-reasoner-config))

(defun chainsaw-reasoner-config ()
  (vanilla-reasoner-config))

(defun reset-reasoner (ont  &optional (reasoner *default-reasoner*) (profile nil))
  (setf (v3kb-reasoner ont) nil
	(v3kb-pellet-jena-model ont) nil)
  (instantiate-reasoner ont reasoner profile))

(defun get-reasoner-factory (ont)
  (#"setProperty" 'system "factpp.jni.path" *factpp-jni-path*)
  (or (v3kb-reasoner-factory ont)
      (setf (v3kb-reasoner-factory ont)
	    (ecase (or (v3kb-default-reasoner ont) *default-reasoner*)
	      (:hermit (new "org.semanticweb.HermiT.Reasoner$ReasonerFactory"))
	      ((:pellet :pellet-sparql) (new 'com.clarkparsia.pellet.owlapiv3.PelletReasonerFactory))
	      (:factpp   
	       (#"setProperty" 'system "factpp.jni.path" *factpp-jni-path*)
	       (new 'uk.ac.manchester.cs.factplusplus.owlapiv3.FaCTPlusPlusReasonerFactory))
	      (:elk  (new 'org.semanticweb.elk.owlapi.ElkReasonerFactory))
	      (:jfact (new 'uk.ac.manchester.cs.jfact.JFactFactory))
	      (:chainsaw (new 'uk.ac.manchester.cs.chainsaw.ChainsawReasonerFactory))
	      ))))
	     
    
(defun instantiate-reasoner (ont  &optional (reasoner *default-reasoner*) (profile nil) (config nil))
  (apply-changes ont)
  (#"setProperty" 'system "factpp.jni.path" *factpp-jni-path*)
  (unless (null reasoner)
    (unless (v3kb-reasoner ont)
      (setf (v3kb-default-reasoner ont) reasoner)
      (let* ((config (or config
			 (ecase reasoner 
			   (:hermit (hermit-reasoner-config profile ont))
			   ((:pellet :pellet-sparql) (pellet-reasoner-config))
			   (:factpp (factpp-reasoner-config))
			   (:elk (elk-reasoner-config))
			   (:jfact (jfact-reasoner-config))
			   (:chainsaw (chainsaw-reasoner-config )))))
	     (factory (ecase reasoner
			(:hermit (new "org.semanticweb.HermiT.Reasoner$ReasonerFactory"))
			((:pellet :pellet-sparql) (new 'com.clarkparsia.pellet.owlapiv3.PelletReasonerFactory))
			(:factpp (new 'uk.ac.manchester.cs.factplusplus.owlapiv3.FaCTPlusPlusReasonerFactory))
			(:elk  (new 'org.semanticweb.elk.owlapi.ElkReasonerFactory))
			(:jfact (new 'uk.ac.manchester.cs.jfact.JFactFactory))
			(:chainsaw (new 'uk.ac.manchester.cs.chainsaw.ChainsawReasonerFactory))
			))
	     (reasoner-instance
	      (if (eq reasoner :pellet-sparql)
		  (#"createNonBufferingReasoner" factory (v3kb-ont ont) config)
		  (#"createReasoner" factory (v3kb-ont ont) config))))
	(setf (v3kb-reasoner ont) reasoner-instance)
	(when (member reasoner '(:pellet :pellet-sparql))
	  (setf (v3kb-pellet-jena-model ont) 
		(let ((graph (new 'org.mindswap.pellet.jena.PelletReasoner)))
		  (#"createInfModel" 'com.hp.hpl.jena.rdf.model.ModelFactory
				     (#"bind" graph (#"getKB" reasoner-instance)))
		  )))
	t)
      )))

;; for later.
;;	setPhysicalURIForOntology(OWLOntology ontology, java.net.URI physicalURI) 
;;          Overrides the current physical URI for a given ontology.

(defun check-ontology (ont  &key classify reasoner (log "OFF") profile)
  (let ((reasoner (or reasoner (v3kb-default-reasoner ont) *default-reasoner*)))
    (instantiate-reasoner ont reasoner profile)
    (when (or (eq reasoner :pellet) (eq reasoner :pellet-sparql))
      (pellet-log-level (#"getKB" (v3kb-reasoner ont)) log))
    ;;  (if classify (#"prepareReasoner" (v3kb-reasoner ont)))
    (when classify
      (#"precomputeInferences" 
       (v3kb-reasoner ont)
       (jnew-array-from-array
	(find-java-class 'org.semanticweb.owlapi.reasoner.InferenceType)
	(make-array 1 :initial-contents (list (get-java-field 'inferencetype "CLASS_HIERARCHY")))))) 
    (prog1
	(#"isConsistent" (v3kb-reasoner ont))
      (when (or (eq reasoner :pellet) (eq reasoner :pellet-sparql) )
	(pellet-log-level (#"getKB" (v3kb-reasoner ont)) "OFF")
	))))

	 
;; (set-to-list (#"getViolations" (setq a2 (#"checkOntology" (setq a1 (new 'owl2dlprofile)) (v3kb-ont f)))))

;;   (with-ontology f (:collecting t) 
;; 	     ((asq (declaration (class !a)) (declaration (object-property !p)) (declaration (object-property !q))(subclassof  !a (object-has-self !p)) (transitiveobjectproperty !q)(disjointobjectproperties (annotation !rdfs:label "foo") !p !q)))
;; 	   (#"isAnnotated" (#"getAxiom" (print (car (set-to-list (#"getViolations" (setq a2 (#"checkOntology" (setq a1 (new 'owl2dlprofile)) (v3kb-ont f)))))))))
;; 	   ;(check-ontology f :classify t)
;; 	   )

;; ##FIXME## Need to handle case of mixed use, e.g. (object-intersection-of (some !p !v) (some !p (not !v)))
(defun to-class-expression (thing &optional (kb *default-kb*))
  (let ((it
	 (cond ((jclass-superclass-p (load-time-value (find-java-class 'org.semanticweb.owlapi.model.owlentity)) (jobject-class thing))
		thing)
	       ((jclass-superclass-p (load-time-value (find-java-class 'org.semanticweb.owlapi.model.OWLEntity.owlclassexpression)) (jobject-class thing))
		thing)
	       ((stringp thing)
		(parse-manchester-expression kb thing))
	       ((uri-p thing)
		(#"getOWLClass" (v3kb-datafactory kb) (to-iri thing)))
	       ((and (listp thing)
		     (member (car thing) '(and or :or not :not only :only some :some)))
		(to-class-expression (manchester-expression thing) kb))
	       ((consp thing)
		(to-owlapi-class-expression (eval-uri-reader-macro thing) (v3kb-datafactory kb)))
	       (t (error "don't know how to turn ~s into a class expression" thing))
	       )))
    it
    ))

(defun class-query (class kb fn &optional (flatten t) include-nothing filter)
  (instantiate-reasoner kb (or (v3kb-default-reasoner kb) *default-reasoner*) nil)
  (let ((expression (to-class-expression class kb))
	(reasoner (v3kb-reasoner kb)))
    (let ((nodes (funcall fn expression reasoner)))
      (loop for iri in (let ((them (jss::set-to-list (if flatten (#"getFlattened" nodes) nodes))))
			 (let ((res
				(if filter 
				    (remove-if-not (lambda(ce) (funcall filter ce reasoner)) them)
				    them)))
			   res))
	 for string = (and iri (#"toString" (#"getIRI" iri)))
	 for uri = (and iri (make-uri string))
	 unless (or (null iri) (and (eq uri !owl:Nothing) (not include-nothing))) collect (make-uri string)))))

(defun instance-query (instance kb fn &optional (flatten t) include-nothing filter)
  (instantiate-reasoner kb (or (v3kb-default-reasoner kb) *default-reasoner*) nil)
  (let ((expression (car (find :individual (gethash instance (v3kb-uri2entity kb)) :key 'second))))
    (when expression
      (let ((reasoner (v3kb-reasoner kb)))
	(let ((nodes (funcall fn expression reasoner)))
	  (loop for iri in (let ((them (jss::set-to-list (if flatten (#"getFlattened" nodes) nodes))))
			     (let ((res
				     (if filter 
					 (remove-if-not (lambda(ce) (funcall filter ce reasoner)) them)
					 them)))
			       res))
		for string = (and iri (#"toString" (#"getIRI" iri)))
		for uri = (and iri (make-uri string))
		unless (or (null iri) (and (eq uri !owl:Nothing) (not include-nothing))) collect (make-uri string)))))))

(defun property-query (property kb fn &optional (flatten t) include-top filter )
  (instantiate-reasoner kb (or (v3kb-default-reasoner kb) *default-reasoner*) nil)
  (let ((expression (car (find :object-property (gethash property (v3kb-uri2entity kb)) :key 'second))))
    (when expression
      (let ((reasoner (v3kb-reasoner kb)))
	(let ((nodes (funcall fn expression reasoner)))
	  (loop for iri in (let ((them (jss::set-to-list (if flatten (#"getFlattened" nodes) nodes))))
			     (let ((res
				     (if filter 
					 (remove-if-not (lambda(ce) (funcall filter ce reasoner)) them)
					 them)))
			       res))
		for string = (and iri (#"toString" (#"getIRI" iri)))
		for uri = (and iri (make-uri string))
		unless (or (null iri) (and (not include-top) (eq uri !owl:topObjectProperty))) collect uri))))))


(defun annotation-properties (kb)
  (mapcar 'make-uri (mapcar #"toString" (mapcar #"getIRI"  (set-to-list (#"getAnnotationPropertiesInSignature" (v3kb-ont o)))))))

(defun entity-annotations (uri  &optional (kb *default-kb*) prop)
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

(defun entity-annotation-value (uri kb prop)
  (second (car (entity-annotations uri kb prop))))

(defun entity-label (uri kb)
  (loop for ont in (set-to-list (#"getImportsClosure" (v3kb-ont kb)))
     append
     (let ((annots (set-to-list (#"getAnnotations" 'EntitySearcher (#"getOWLNamedIndividual" (v3kb-datafactory kb) (to-iri uri)) ont (#"getOWLAnnotationProperty" (v3kb-datafactory kb) (to-iri !rdfs:label))))))
       (loop for annot in annots
	  for value = (#"getValue" annot)
	  when value do (return-from entity-label
		       (if (jclass-superclass-p (find-java-class "OWLLiteral") (jobject-class value))
			   (cond ((#"isRDFPlainLiteral" value) (#"getLiteral" value))
				     (t value))
			   value
			   ))))))

(defun entity-annotations-2 (uri kb)
  (loop for ont in (set-to-list (#"getImportsClosure" (v3kb-ont kb)))
       append
       (let ((annots (set-to-list (#"getAnnotationAssertionAxioms" (v3kb-ont kb) (to-iri uri)))))
;	 (setq *all2* (append annots *all2*))
	 (loop for annot in annots
	    for property = (#"toString" (#"getIRI" (#"getProperty" annot)))
	    for value = (#"getValue" annot)
	    collect (list  (make-uri property)
			   (if (jclass-superclass-p (find-java-class "OWLLiteral") (jobject-class value))
			       (cond ((#"isRDFPlainLiteral" value) (#"getLiteral" value))
				     (t value))
			       value;(make-uri (#"toString" value))
			       ))))))

(defun loaded-documents (kb)
  (let ((manager (if (v3kb-p kb) (v3kb-manager kb) (#"getOWLOntologyManager" kb)))
	(ont (if (v3kb-p kb) (v3kb-ont kb)  kb)))
    (mapcar (lambda(e) (list (#"toString" (#"getOntologyDocumentIRI" manager e)) (#"toString" (if (#"isPresent" (#"getOntologyIRI" (#"getOntologyID" e))) (#"get" (#"getOntologyIRI" (#"getOntologyID" e))) "")) e))
	    (set-to-list (#"getImportsClosure" ont)))))

(defun unsatisfiable-classes (kb)
  (check-ontology kb)
  (loop for c in (set-to-list (#"getEntitiesMinusBottom" (#"getUnsatisfiableClasses" (v3kb-reasoner kb))))
     collect (make-uri (#"toString" (#"getIRI" c)))))

(defun unsatisfiable-properties (kb)
  (check-ontology kb)
  (loop for p in (remove-if #"isAnonymous" (set-to-list (#"getEquivalentObjectProperties" (v3kb-reasoner kb) (#"getOWLObjectProperty" (v3kb-datafactory kb) (to-iri !owl:bottomObjectProperty)))))
     for uri = (make-uri (#"toString" (#"getIRI" p)))
	unless (eq uri !owl:bottomObjectProperty) collect uri))

(defun get-ontology-iri (kb)
  (let ((ontology (if (v3kb-p kb) (v3kb-ont kb) kb)))
    (make-uri (#"toString" (#"get" (#"getOntologyIRI" (#"getOntologyID" ontology)))))))

(defun get-version-iri (kb)
  (let ((ontology (if (v3kb-p kb) (v3kb-ont kb) kb)))
    (make-uri (#"toString" (#"get" (#"getVersionIRI" (#"getOntologyID" ontology)))))))

(defun get-imports-declarations (kb)
  (mapcar (compose 'make-uri #"toString" #"getIRI")
	  (jss::j2list (#"getImportsDeclarations" (v3kb-ont kb)))))

(defun add-change (kb change)
  (unless (v3kb-changes kb)
    (setf (v3kb-changes kb) (new 'arraylist)))
  (#"add" (v3kb-changes kb) change))

(defun set-version-iri (kb iri)
  (let ((change (#"createOntologyChange"
		 (new 'SetOntologyIDData 
		      (new 'owlontologyid (to-iri (get-ontology-iri kb)) (to-iri iri)))
		 (v3kb-ont kb))))
    (add-change kb change)
    (apply-changes kb)))

(defun test-reasoners ()
  (let (o)
    (setq o (load-ontology "http://purl.obolibrary.org/obo/iao.owl"))
    (check-ontology o t :reasoner :hermit )
    (format t "Hermit says descendants of owl:Thing are ~a~%" (descendants !owl:Thing o)))
  (let (o)
    (setq o (load-ontology "http://purl.obolibrary.org/obo/iao.owl"))
    (check-ontology o t :reasoner :pellet )
    (format t "Pellet says descendants of owl:Thing are ~a~%" (descendants !owl:Thing o)))
  (let (o)
    (setq o (load-ontology "http://purl.obolibrary.org/obo/iao.owl"))
    (check-ontology o t :reasoner :factpp )
    (format t "Factplusplus says descendants of owl:Thing are ~a~%" (descendants !owl:Thing o))))

(defun short-form-provider (kb &key
			    (properties (list !rdfs:label))
			    (language-preferences '("en"))
			    (replace nil))
  (or (and (not replace) (v3kb-short-form-provider kb))
      (let* ((a-props (new 'java.util.arraylist)) 
	     (langs (new 'java.util.arraylist))
	     (langs-none (new 'java.util.arraylist))
	     (prop->lang (new 'hashmap))
	     (prop->lang-none (new 'hashmap))
	     (ontset (new 'owlapi.util.owlontologyimportsclosuresetprovider
			  (v3kb-manager kb) (v3kb-ont kb))))
	(dolist (lang language-preferences)
	  (#"add" langs lang))
	(dolist (prop properties)
	  (#"add" a-props (get-entity prop :annotation-property kb))
	  (#"put" prop->lang (get-entity prop :annotation-property kb) langs)
	  (#"put" prop->lang-none (get-entity prop :annotation-property kb) langs-none))
	(let ((any-lang-provider (new 'owlapi.util.annotationvalueshortformprovider a-props prop->lang-none ontset
				      (new 'simpleshortformprovider))))
	  (#"setLiteralRenderer" any-lang-provider (new 'quotedStringAnnotationVisitor))
	  ;(jss::set-java-field any-lang-provider "quoteShortFormsWithSpaces" +true+)
	  (let ((lang-specific-provider (new 'owlapi.util.annotationvalueshortformprovider a-props prop->lang ontset any-lang-provider)))
	    (#"setLiteralRenderer" lang-specific-provider (new 'quotedStringAnnotationVisitor))
	    ;; next isn't working yet
	    ;(jss::set-java-field lang-specific-provider "quoteShortFormsWithSpaces" +true+)
	    (setf (v3kb-short-form-provider kb) 
		  (new 'owlapi.util.BidirectionalShortFormProviderAdapter (#"getImportsClosure" (v3kb-ont kb))
		       lang-specific-provider)))))))



(defun manchester-parser (kb)
  (or (v3kb-manchester-parser kb)
      (setf (v3kb-manchester-parser kb)
	    (new 'ManchesterOWLSyntaxClassExpressionParser (v3kb-datafactory kb)
		 (new 'shortformentitychecker (short-form-provider kb))))))

(defun parse-manchester-expression (kb string)
  (or (#"getEntity" (short-form-provider kb)
		    (if (and (find #\space string) (not (char= (char string 0) #\')))
			(concatenate 'string "'" string "'")
			string))
      (#"parse" (manchester-parser kb) string)))

(defparameter *owlapi-syntax-renderers*
  '((:manchester org.semanticweb.owlapi.manchestersyntax.renderer.ManchesterOWLSyntaxRenderer)
    (:functional org.semanticweb.owlapi.functional.renderer.OWLFunctionalSyntaxRenderer )
    (:xml org.semanticweb.owlapi.owlxml.renderer.OWLXMLRenderer)
    (:latex org.semanticweb.owlapi.latex.renderer.LatexRenderer)
    (:rdfxml org.semanticweb.owlapi.rdf.rdfxml.renderer.RDFXMLRenderer)
    (:krss org.semanticweb.owlapi.krss2.renderer.KRSS2OWLSyntaxRenderer)
    (:turtle org.semanticweb.owlapi.rdf.turtle.renderer.TurtleRenderer)
    (:ntriples org.semanticweb.owlapi.formats.NTriplesDocumentFormat
    )))

(defun manchester-renderer (kb)
  (or (v3kb-manchester-renderer kb)
      (setf (v3kb-manchester-renderer kb)
	    (let ((it (new 'ManchesterOWLSyntaxOWLObjectRendererImpl)))
	      (#"setShortFormProvider" it (short-form-provider kb))
	      it))))

(defun compute-uri2entity (kb)
  ;; build table - uri -> triples entity, type, ontology
  (loop for ont in (set-to-list (#"getImportsClosure" (v3kb-ont kb)))
       with table = (make-hash-table)
     do
       (loop for (type entities) in (list (list :class (#"getClassesInSignature" ont))
					  (list :individual (#"getIndividualsInSignature" ont))
					  (list :object-property (#"getObjectPropertiesInSignature" ont))
					  (list :data-property (#"getDataPropertiesInSignature" ont))
					  (list :annotation-property (#"getAnnotationPropertiesInSignature" ont)))
	    do
	    (loop for entity in (set-to-list entities)
	       do (pushnew (list entity type ont) (gethash (make-uri (#"toString" (#"getIRI" entity))) table)
			   :test 'equalp)))
       finally (return table)))

;; might have used reserialized OWLAPI content but it screws up anonymous individuals
;; http://sourceforge.net/tracker/?func=detail&aid=2975911&group_id=90989&atid=595534

(defun to-jena-model (kb)
  (let ((model (#"createDefaultModel" 'hp.hpl.jena.rdf.model.ModelFactory)))
    (loop for (source) in (loaded-documents kb)
       do
	 (unless (search "Optional.of" source)
	   (if (or (search "inputstream" source)
	       (search "owlapi:" source)) ;; assume the only such one is the kb itself
	   (#"read" model (new 'stringreader (to-owl-syntax kb :rdfxml))
		    source)
	   (#"read" model source))))
    model))

(defun write-rdfxml (ont &optional path)
  (unless path
    (setq path (format nil "~a~a.owl" *default-rdfxml-writing-location*
		       (string-downcase (#"replaceAll"
					 (#"replaceAll" (string (v3kb-name ont)) ".*[/#]" "")
					 "\\..*?$" "")))))
  (setq path (namestring (translate-logical-pathname path)))
  (to-owl-syntax ont :rdfxml path))

(defun classtree-depth (kb &aux (maxdepth 0))
  (labels ((each-node (c depth)
	     (setq maxdepth (max maxdepth depth))
	     (dolist (cc (children c kb))
	       (unless (eq cc !owl:Nothing)
		 (each-node cc (1+ depth))))))
    (each-node !owl:Thing 0)
    maxdepth))

(defun get-referencing-axioms (entity type ont &optional direct-only)
  (mapcar 'manchester-expression-from-functionalish
	  (mapcar 'axiom-to-lisp-syntax 
		  (loop for (entity etype eont) in (gethash entity (v3kb-uri2entity ont))
		     when (eq etype type)
		     append (set-to-list (#"getAxioms" eont entity))
		     unless direct-only append (set-to-list (#"getReferencingAxioms" eont entity))))))

(defun get-rendered-referencing-axioms (entity type ont &optional direct-only)
  (loop for (entity etype eont) in (gethash entity (v3kb-uri2entity ont))
     with renderer = (manchester-renderer ont)
     when (eq etype type)
     append (mapcar (lambda(ax)
		      (list (#"render" renderer ax) ax))
		    (union 
		     (set-to-list (#"getAxioms" eont entity))
		     (unless direct-only (set-to-list (#"getReferencingAxioms" eont entity)))))))

(defun manchester-render-axiom (axiom)
  (let ((renderer (manchester-renderer ont)))
    (#"render" renderer axiom)))

(defun functional-render-axiom (axiom &optional (ont *default-kb*))
  (let* ((writer (new 'stringwriter))
	 (renderer (new 'FunctionalSyntaxObjectRenderer (v3kb-ont ont) writer)))
    (#"visit" renderer axiom)
    (#"toString" writer)))

(defun get-entity (uri type ont)
  (unless (v3kb-uri2entity ont)
    (setf (v3kb-uri2entity ont) (compute-uri2entity ont)))
  (loop for (entity etype) in (gethash uri (v3kb-uri2entity ont))
     when (eq etype type)
     do (return-from get-entity entity)))

  
(defun little-test-ont ()
  (with-ontology foo (:collecting t)
      ((asq
	(declaration (class !a))
	(declaration (class !b))
	(declaration (object-property !part))
	(annotation-assertion !rdfs:label !a "a")
	(annotation-assertion !rdfs:label !b "b")
	(annotation-assertion !rdfs:label !part "part")
	(subclass-of !a (object-some-values-from !part !b))
	(subclass-of !b !c)))
    foo))
	       
;; Call fn on each axiom in the ontology (include-imports-closure -> t to include the imports closure)



 '(sparql '(:select (?class ?label) () (?class !rdfs:subClassOf (!oborel:has_participant some !<http://purl.org/obo/owl/PRO#submitted_irf7pirf7p>))
	   (?class !rdfs:label ?label))
	 :kb kb :use-reasoner :sparqldl :syntax :terp)

;(find "getOWLObjectSomeValuesFrom" (#"getMethods" (jobject-class (v3kb-datafactory kb))) :key 'jmethod-name :test 'equal)
