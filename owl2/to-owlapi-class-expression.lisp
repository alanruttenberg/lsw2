;; Alan Ruttenberg
;; 2013-12-10
;;
;; parse an owl class expression from our lispy form

;; define enough syntax. Optional forms for each of the class
;; expressions and data range expressions. (* x ... ) means repeat.

(defparameter *class-expression-syntax*
  '((class-expression
     ("ObjectIntersectionOf" (* class-expression))
     ("ObjectUnionOf" (* class-expression))
     ("ObjectComplementOf" class-expression)
     ("ObjectOneOf" (* individual))
     ("ObjectSomeValuesFrom" object-property-expression class-expression)
     ("ObjectAllValuesFrom" object-property-expression class-expression)
     ("ObjectHasValue" object-property-expression individual)
     ("ObjectHasSelf" object-property-expression)
     ("ObjectMinCardinality" number object-property-expression class-expression )
     ("ObjectMaxCardinality" number object-property-expression class-expression )
     ("ObjectExactCardinality"  number object-property-expression class-expression )
     ("DataSomeValuesFrom" data-property data-range)
     ("DataAllValuesFrom" data-property data-range)
     ("DataHasValue" data-property literal)
     ("DataExactCardinality" number data-property data-range)
     ("DataMinCardinality" number data-property data-range)
     ("DataMaxCardinality" number data-property data-range)
     ("DataExactCardinality" number data-property data-range))
    (data-range 
     ("DataIntersectionOf" (* data-range))
     ("DataUnionOf" (* data-range))
     ("DataComplementOf" data-range)
     ("DataOneOf" (* literal))
     ("DatatypeRestriction" (* facet-restriction))) ;; fact-restriction = (facet literal)
    ))

;; parse a class expression. Recurse if necessary. We need to use a
;; data factory from the ontology object. The OWLAPI constructors are
;; named as the class expression heads with "getOWL" prepended.

(defvar *scratch-ontology* (with-ontology scratch ()() scratch))

(defun to-owlapi-class-expression (class-expression &optional (data-factory (v3kb-datafactory *scratch-ontology*)))
  (cond ((jclass-superclass-p (load-time-value (find-java-class 'org.semanticweb.owlapi.model.owlentity)) (jobject-class class-expression))
	 class-expression)
	((jclass-superclass-p (load-time-value (find-java-class 'org.semanticweb.owlapi.model.OWLEntity.owlclassexpression)) (jobject-class class-expression))
	 class-expression)
	(t
	 (let ((patterns (find 'class-expression *class-expression-syntax* :key 'car)))
	   (flet ((circular (list) (let ((list (copy-list list))) (setf (cdr (last list)) list))))
	     (if (atom class-expression)
		 (#"getOWLClass" data-factory (to-iri class-expression))
		 (destructuring-bind (expression-type . argtypes)
		     (find (third (gethash (car class-expression) *owl2-vocabulary-forms*))
			   (rest patterns)
			   :test 'equalp
			   :key 'car)

		   ;; this loop is complicated because
		   ;; a) There can be repeated arguments written as (* x)
		   ;; b) If there are repeated arguments they need to be
		   ;;    packaged up into a java set to pass to the constructor
		   (loop for arg in (rest class-expression)
		      with accumulate-set = nil
		      for argtype = (progn (when (consp (car argtypes))
					     (setq argtypes (circular (rest (car argtypes))))
					     (setq accumulate-set t))
					   (prog1 (car argtypes) (setq argtypes (cdr argtypes))))
		      for processed-arg = (ecase argtype
					    (literal (to-owlapi-literal arg data-factory))
					    (number (to-owlapi-number arg data-factory))
					    (individual (to-owlapi-individual arg data-factory))
					    (object-property-expression (to-owlapi-object-property-expression arg data-factory))
					    (class-expression (to-owlapi-class-expression arg data-factory))
					    (data-range (to-owlapi-data-range arg data-factory))
					    (data-property (#"getOWLDataProperty" data-factory (to-iri arg)))
					    (object-property (#"getOWLObjectProperty" data-factory (to-iri arg))))
		      if accumulate-set collect processed-arg into last-arg-set 
		      else collect processed-arg into leading-args
		      finally
			(return 
			  (if accumulate-set 
			      (apply (data-factory-constructor expression-type)
				     data-factory
				     (append leading-args (list (list-to-java-set last-arg-set))))
			      (apply (data-factory-constructor expression-type)
				     data-factory leading-args)))
			))))))))

(defun to-owlapi-object-property-expression (arg data-factory)
  (if (and (consp arg)
	   (eq (car (gethash (car arg)  *owl2-vocabulary-forms*))  'OBJECTINVERSEOF ))
      (#"getOWLObjectInverseOf" data-factory (#"getOWLObjectProperty" data-factory (to-iri (second arg))))
      (#"getOWLObjectProperty" data-factory (to-iri arg))))

(defun to-owlapi-data-property-expression (arg data-factory)
  (#"getOWLDataProperty" data-factory (to-iri arg)))

(defun to-owlapi-annotation-property (arg data-factory)
  (#"getOWLAnnotationProperty" data-factory (to-iri arg)))

(defun data-factory-constructor (arg)
  (let ((normalized (third (gethash arg *owl2-vocabulary-forms*))))
    (read-from-string (format nil "#\"getOWL~a\"" normalized))))

(defun to-owlapi-literal (arg data-factory)
  (cond ((and (stringp arg) (find #\@ arg :test 'char=))
	 (let ((last@ (position #\@ arg :test 'char= :from-end t)))
	   (#"getOWLLiteral" data-factory (subseq arg 0 last@) (subseq arg (1+ last@)))))
	((stringp arg)
	  (#"getOWLLiteral" data-factory arg))
	((and (consp arg) (eq (car arg) :literal))
	 (#"getOWLLiteral" data-factory (princ-to-string arg) (#"getOWLDatatype" data-factory (to-iri (third arg)))))
	((numberp arg)
	 (#"getOWLLiteral" data-factory arg))
	(t (error "don't understand literal ~s" arg))))

(defun to-owlapi-individual (arg data-factory)
  (#"getOWLNamedIndividual" data-factory (to-iri arg)))

(defun to-owlapi-number (arg data-factory)
  arg)

;; cut, paste, mod of to-owlapi-class-expression. I'm so embarassed.
;; This one slightly changes argument processing to be able to eat an
;; extra argument for facet restrictions.

(defun to-owlapi-data-range (data-range data-factory)
  (let ((patterns (find 'data-range *class-expression-syntax* :key 'car))
	(arguments nil))
    (flet ((circular (list) (let ((list (copy-list list))) (setf (cdr (last list)) list))))
      (if (atom data-range)
	  (#"getOWLDatatype" data-factory (to-iri data-range))
	  (destructuring-bind (expression-type . argtypes)
	      (find (third (gethash (car data-range) *owl2-vocabulary-forms*))
		    (rest patterns)
		    :test 'equalp
		    :key 'car)
	    ;; this loop is complicated because
	    ;; a) There can be repeated arguments written as (* x)
	    ;; b) If there are repeated arguments they need to be
	    ;;    packaged up into a java set to pass to the constructor
	    (progn
	      (setq arguments (rest data-range))
	      (loop with accumulate-set = nil
		    while arguments
		    for arg = (pop arguments)
		    for argtype = (progn
				    (when (consp (car argtypes))
				      (setq argtypes (circular (rest (car argtypes))))
				      (setq accumulate-set t))
				    (prog1 (car argtypes) (setq argtypes (cdr argtypes))))
		    for processed-arg = (ecase argtype
					  (literal (to-owlapi-literal arg data-factory))
					  (data-range (to-owlapi-data-range arg data-factory))
					  (facet-restriction (to-owlapi-facet-restriction arg (pop arguments) data-factory)))
		    if accumulate-set collect processed-arg into last-arg-set 
		      else collect processed-arg into leading-args
		    finally
		       (return 
			 (if accumulate-set 
			     (apply (data-factory-constructor expression-type)
				    data-factory
				    (append leading-args (list (list-to-java-set last-arg-set))))
			     (apply (data-factory-constructor expression-type)
				    data-factory leading-args)))
		    )))))))


(defun owlapi-axiom-constructor (axiom-type)
  (let ((normalized (third (gethash axiom-type *owl2-vocabulary-forms*))))
    (format nil "getOWL~aAxiom" normalized)))

(defun to-owlapi-axiom (axiom ont)
  (setq axiom (eval-uri-reader-macro axiom))
  (let ((axiom-type (car (gethash (car axiom) *owl2-vocabulary-forms*)))
	(df (v3kb-datafactory ont)))
    (assert axiom-type (axiom-type) "Don't know how to translate axiom type ~a" (car axiom))
    (case axiom-type
      ((subclassof disjointclasses equivalentclasses) 
       (apply 'jcall (owlapi-axiom-constructor axiom-type) df (mapcar (lambda(e)(to-class-expression e ont)) (cdr axiom))))
      ((TransitiveObjectProperty AsymmetricObjectProperty
			       SymmetricObjectProperty IrreflexiveObjectProperty
			       ReflexiveObjectProperty InverseFunctionalObjectProperty
			       FunctionalObjectProperty)
       (jcall (owlapi-axiom-constructor axiom-type) df (to-owlapi-object-property-expression (cdr axiom))))
      (FunctionalDataProperty
	(jcall (owlapi-axiom-constructor axiom-type) df (to-owlapi-data-property-expression (second axiom) df)))
      ((InverseObjectProperties DisjointObjectProperties EquivalentObjectProperties)
       (apply 'jcall (owlapi-axiom-constructor axiom-type) df (mapcar (lambda(e)(to-owlapi-object-property-expression e ont)) (cdr axiom))))
      ((SubDataPropertyOf EquivalentDataProperties DisjointDataProperties)
       (apply 'jcall (owlapi-axiom-constructor axiom-type) df (mapcar (lambda(e)(to-owlapi-data-property-expression e df)) (cdr axiom))))
      ((SubObjectPropertyOf EquivalentObjectProperties DisjointObjectProperties)
       (apply 'jcall (owlapi-axiom-constructor axiom-type) df (mapcar (lambda(e)(to-owlapi-object-property-expression e df)) (cdr axiom))))
      ((DataPropertyDomain  DataPropertyRange)
       (jcall (owlapi-axiom-constructor axiom-type) df 
	      (to-owlapi-data-property-expression (second axiom) df) 
	      (to-owlapi-data-range (third axiom) df)))
      ((ObjectPropertyDomain ObjectPropertyRange)
       (jcall (owlapi-axiom-constructor axiom-type) df 
	      (to-owlapi-object-property-expression (second axiom) df) 
	      (to-owlapi-class-expression (third axiom) df)))
      (t (error "don't know how to create owlapi for axiom ~a yet" axiom-type)))))

