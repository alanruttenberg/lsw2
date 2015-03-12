;; Call fn on each axiom in the ontology (include-imports-closure -> t to include the imports closure)

(defun each-axiom (ont fn &optional include-imports-closure)q
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
       (every (lambda(e) (jinstance-of-p e (find-java-class 'OWLClass))) (set-to-list (#"getClassExpressions" ax)))))

(defun make-subclass-axioms-from-equivalents (ax kb)
  "take an equivalentclasses expression in which there is a named class (i.e. not a GCI) and turn it into subclassof axioms for the named class"
  (let ((elements (set-to-list (#"getClassExpressions" ax))))
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
    (#"addAll" changes (#"removeAxiom" (v3kb-manager kb) (v3kb-ont kb) axiom))))

(defun add-axiom (axiom kb)
  (let ((changes (or (v3kb-changes kb) (setf (v3kb-changes kb) (new 'arraylist)))))
    (#"addAll" changes (#"addAxiom" (v3kb-manager kb) (v3kb-ont kb) axiom))))

(defun subclassof-axiom (subclass-expression superclass-expression kb)
  (#"getOWLSubClassOfAxiom"
   (v3kb-datafactory kb) 
   (to-class-expression subclass-expression kb)
   (to-class-expression superclass-expression kb)))
