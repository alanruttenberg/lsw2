(in-package :cl-user)
;; Call fn on each axiom in the ontology (include-imports-closure -> t to include the imports closure)

(defun each-axiom (ont fn &optional include-imports-closure)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((ont (if (v3kb-p ont) (v3kb-ont ont) ont))
	 (onts (if include-imports-closure
		   (set-to-list (#"getImportsClosure" ont))
		   (list ont))))
    (with-constant-signature ((iterator "iterator" t) (hasnext "hasNext") (next "next"))
      (loop for one in onts
	   do
	   (loop with iterator = (iterator (#"getAxioms" one))
	      while (hasNext iterator)
	      for item = (next iterator)
	      do (funcall fn  item))))))

(defun simple-subclassof-axiom? (ax)
  (and (jinstance-of-p ax (find-java-class 'OWLSubClassOfAxiom ))
       (= (#"size" (#"getClassesInSignature" ax)) 2)
       (every (lambda(e) (jinstance-of-p e (find-java-class 'OWLClass))) (set-to-list (#"getNestedClassExpressions" ax)))))

(defun make-subclass-axioms-from-equivalents (ax kb)
  "take an equivalentclasses expression in which there is a named class (i.e. not a GCI) and turn it into subclassof axioms for the named class"
  (let ((elements (set-to-list (#"getNestedClassExpressions" ax))))
    (let ((named (find-if (lambda(el) (jinstance-of-p el (find-java-class 'OWLClass))) elements)))
      (loop for el in elements
	   unless (eq named el)
	   collect (subclassof-axiom named el kb)))))

;; note bug https://sourceforge.net/tracker/index.php?func=detail&aid=2975093&group_id=90989&atid=595534
;; imports declarations not an axiom type and not found by get-axioms.

(defun simple-equivalentclasses-axiom? (ax)
  "check if an equivalentclasses expression has at least one named class (i.e. not a GCI)"
  (and (jinstance-of-p ax (find-java-class 'OWLEquivalentClassesAxiom ))
       (#"containsNamedEquivalentClass" ax)))

(defmacro axiom-typecase (axiom &body clauses)
  (let ((axiomv (make-symbol "AXIOM")))
    `(let ((,axiomv ,axiom))
	  (case (intern (#"getName" (#"getAxiomType" ,axiomv)) 'keyword)
	    ,@(loop for (types . body) in clauses
		   collect
		   (if (eq types 'otherwise)
		       (cons types body)
		       (list* (loop for type in (if (atom types) (list types) types)
				 for found = 
				   (find (string type)
					 (mapcar (lambda(e) (intern (#"getName" e) 'keyword))
						 (set-to-list (get-java-field 'org.semanticweb.owlapi.model.AxiomType "AXIOM_TYPES")))
					 :key  'string :test 'equalp)
				   unless found do (error "Didn't find axiom type ~a" type)
				   collect found)
			      body)))))))

(defun owl-declaration-type (declaration-axiom)
  (let ((class (jobject-class (#"getEntity" declaration-axiom))))
    (cond ((equal class (load-time-value (find-java-class "uk.ac.manchester.cs.owl.owlapi.OWLAnnotationPropertyImpl")))
	   :annotation-property)
	  ((equal class (load-time-value (find-java-class "uk.ac.manchester.cs.owl.owlapi.OWLObjectPropertyImpl")))
	   :object-property)
	  ((equal class (load-time-value (find-java-class "uk.ac.manchester.cs.owl.owlapi.OWLDataPropertyImpl")))
	   :data-property)
	  ((equal class (load-time-value (find-java-class "uk.ac.manchester.cs.owl.owlapi.OWLClassImpl")))
	   :class)
	  ((equal class (load-time-value (find-java-class "uk.ac.manchester.cs.owl.owlapi.OWLNamedIndividualImpl")))
	   :individual)
	  ((equal class (load-time-value (find-java-class "uk.ac.manchester.cs.owl.owlapi.OWLDatatypeImpl")))
	   :datatype)
	  (t (error "don't know what kind of declaration for ~a" (#"toString" declaration-axiom))))))

(defun remove-axiom (axiom kb)
  (let ((changes (or (v3kb-changes kb) (setf (v3kb-changes kb) (new 'arraylist)))))
    (#"add" changes (#"removeAxiom" (v3kb-manager kb) (v3kb-ont kb) axiom))))

(defun add-axiom (axiom kb)
  (let ((changes (or (v3kb-changes kb) (setf (v3kb-changes kb) (new 'arraylist)))))
    (#"add" changes (#"addAxiom" (v3kb-manager kb) (v3kb-ont kb) axiom))))

(defun add-axiom (axiom kb)
  (#"addAxiom" (v3kb-manager kb) (v3kb-ont kb) axiom))

(defun add-ontology-annotation (annotation kb)
  (let ((changes (or (v3kb-changes kb) (setf (v3kb-changes kb) (new 'arraylist)))))
    (when (consp annotation)
      (let ((annotation-object
	      (new 'OWLAnnotationImpl (#"getOWLAnnotationProperty"  (v3kb-datafactory kb) (to-iri (car annotation)))
		   (cond ((uri-p (second annotation))
			  (to-iri (second annotation)))
			 (t (#"getOWLLiteral" (v3kb-datafactory kb) (second annotation))))
		   (new 'java.util.hashset))))
	(setq annotation annotation-object)))
    (#"add" changes (new 'AddOntologyAnnotation (v3kb-ont kb) annotation))))

(defun add-ontology-imports (uri kb)
  (let ((changes (or (v3kb-changes kb) (setf (v3kb-changes kb) (new 'arraylist)))))
    (let ((import-object (new 'OWLImportsDeclarationImpl (to-iri uri))))
      (#"add" changes (new 'AddImport (v3kb-ont kb) import-object)))))

(defun remove-ontology-imports (uri kb)
  (let ((changes (or (v3kb-changes kb) (setf (v3kb-changes kb) (new 'arraylist)))))
    (let ((import-object (if (uri-p uri) (new 'OWLImportsDeclarationImpl (to-iri uri)) uri)))
      (#"add" changes (new 'RemoveImport (v3kb-ont kb) import-object)))))
  
(defun add-version-iri (kb iri)
  (add-ontology-annotation (list !owl:versionIRI iri) kb))

(defun apply-changes (ont)
  (unwind-protect (and (v3kb-changes ont)
		       (#"applyChanges"  (v3kb-manager ont) (v3kb-changes ont)))
    (setf (v3kb-changes ont) nil)))

(defun subclassof-axiom (subclass-expression superclass-expression kb)
  (#"getOWLSubClassOfAxiom"
   (v3kb-datafactory kb) 
   (to-class-expression subclass-expression kb)
   (to-class-expression superclass-expression kb)))

(defun count-descendants-with-axioms (term &optional (ont *default-kb*))
  (loop for desc in (descendants term)
     when (> (#"size" (#"getClassesInSignature" (#"next" (#"iterator" (#"getAxioms" (v3kb-ont ont) (to-class-expression desc)))))) 2)
     sum 1))

;; get each axiom in ont. Canonicalize blank nodes. canonicalize any url that isn't a reserved term. canonicalize literals
;; canonicalize blank nodes: find the different blank nodes in the expression. change them to blank1, blank2...
;; canonicalize urls: any uri that isn't in owl: rdf: rdfs: xsd: or sesame: gets changed to :term
;; canonicalize literal: any literal gets changed to: :literal
;; ignore annotations


(defun obo-axiom-element-normalizer (e)
  "Normalize an axiom by replacing some terms with constants, e.g. all non-foundry terms with the token :external. This normalizer gives a token for the obo ontology the term is from, with anything outside of obo becoming :external"
  (declare (optimize (speed 3) (safety 0)))
  (cond ((consp e) e)
	((uri-p e) 
	 (let ((obo-namespace (caar (all-matches (uri-full e) "http://purl.obolibrary.org/obo/([^#_]+)[#_].+" 1))))
	   (if obo-namespace 
	       (intern (string-upcase  obo-namespace) 'keyword)
	       :external)))
	((numberp e) '|#|)
	((eq e 'object-some-values-from) 'some)
	((eq e 'object-intersection-of) 'and)
	((eq e 'object-union-of) 'or)
	((eq e 'object-exact-cardinality) 'exactly)
	(t e)))

(defun simple-axiom-element-normalizer (e)
  "Normalize an axiom by replacing some terms with constants, e.g. all non-foundry terms with the token :external. This normalizer gives a token for the obo ontology the term is from, with anything outside of obo becoming :external"
  (declare (optimize (speed 3) (safety 0)))
  (cond ((consp e) e)
	((uri-p e)  :term)
	((numberp e) '|#|)
	((eq e 'object-some-values-from) 'some)
	((eq e 'object-intersection-of) 'and)
	((eq e 'object-union-of) 'or)
	((eq e 'object-exact-cardinality) 'exactly)
	(t e)))
  
(defun property-axiom-terms ()
  "owl terms related to properties"
  (let ((them nil))
    (maphash (lambda(k v)
	       (declare (ignore k))
	       (when (and (search "PROPERTY" (string (second v)))
			  (not (search "ANNOTATION" (string (second v)))))
		 (push (second v) them)))
	     *owl2-vocabulary-forms*)
    them))

(defvar *ax-sexp-cache*  (make-hash-table))

(defvar *shape-to-axiom*)

(defun axiom-shape(ax look-for &optional (normalizer 'obo-axiom-element-normalizer))
  "Returns either nil, if non-logical axiom, otherwise result of applying normalizer to each of the symbols/uris in the axiom"
  (let ((sexp (axiom-to-lisp-syntax ax)))
    (and (member (car sexp) look-for :test 'eq)
	 (let* ((fixed (tree-remove-if (lambda(el) (and (consp el) (member (car el) '(annotation)))) sexp))
	       (normed (and fixed (tree-replace normalizer fixed))))
	   (values normed sexp)))))
		   
(defun axiom-shapes (kb &key (which '(:class :property)) (normalizer 'obo-axiom-element-normalizer) &aux shapes)
  "goes through all axioms in ontology collecting distinct shapes"
  (let ((look-for (append (if (member :class which) '(sub-class-of equivalent-classes disjoint-classes))
			  (if (member :property which) (property-axiom-terms)))))
    (each-axiom kb
      (lambda(ax)
	;; filter out annotations early
	(unless (jinstance-of-p ax (load-time-value (find-java-class 'org.semanticweb.owlapi.model.OWLAnnotationAssertionAxiom)))
	  (pushnew (axiom-shape ax look-for normalizer) shapes :test 'equalp))))
    (sort (remove nil shapes) 'string< :key (compose 'string 'car)))) 

(defun examples-of-shape (kb shape &optional normalizer (howmany 5) )
  "Given a shape, give some examples of axioms that fit it"
  (if (and (boundp '*shape-to-axiom*) (gethash shape *shape-to-axiom*) (>= (length (gethash shape *shape-to-axiom*)) howmany))
      (map nil 'pprint (subseq (gethash shape *shape-to-axiom*) 0 howmany))
	  (let ((look-for (append '(sub-class-of equivalent-classes disjoint-classes) (property-axiom-terms))))
	    (let ((count 0))
	      (unless normalizer (setq normalizer 'obo-axiom-element-normalizer))
	      (each-axiom kb
		  (lambda(ax)
		    (multiple-value-bind (ax-shape expression) (axiom-shape ax look-for normalizer)
		      (when (boundp '*shape-to-axiom*)
			(push  expression (gethash ax-shape *shape-to-axiom*)))
		      (when (equalp ax-shape shape)
			(pprint (axiom-to-lisp-syntax ax))
			(when (>= count howmany)
			  (return-from examples-of-shape nil))
			(incf count)))))))))

  

(defun ppax (thing)
  "Pretty print one or more axioms. Thing a URI get all relevant axioms. Thing a list of lists, print each as axiom. Thing a list, print just that axiom. Thing a string - should parse but don't yet"
  (when (null thing)
    (print nil)
    (return-from ppax nil))
  (if (uri-p thing)
      ;; FIXME all-relevant-axioms not defined
      (setq thing (all-relevant-axioms thing)))
  (if (not (consp thing))
      (if (java-object-p thing)
	  (ppax (list (axiom-to-lisp-syntax thing)))
	  (ppax (list thing)))
      (loop for ax in thing
	    do
	       (pprint (replace-with-labels
			(cond
			  ((java-object-p ax) (axiom-to-lisp-syntax ax))
			  ((consp ax) ax)
			  ((uri-p ax) ax)
			  ((string ax) (error "haven't implemented parsing of string in arbitrary syntax: '~a'" ax))))))))
