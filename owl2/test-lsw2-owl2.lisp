(in-package :cl-user)

(with-ontology test-ontology ()
  ()
  (let ((expressions
	  '((subclassof !a !b)
	    (subobjectpropertyof !a !b)
	    (object-property-domain !a !b))))

    (prove:plan (length expressions))

    (loop for expression in expressions 
	  do
	     (eval `(prove:is-type (to-owlapi-axiom ',expression ,test-ontology) 'java-object)))))

(prove:finalize)

(prove:plan 2)


(prove:is 
 (with-ontology f () 
   ((asq (subclassof (annotation !this !one) !a !b) (= !b (not !a)))) 
   (check-ontology f)
   (explain-unsatisfiable-class f !a))
 (evurl '((:entailment (sub-class-of !ex:a !owl:Nothing) 
    :support ((sub-class-of !ex:a !ex:b)
	      (equivalent-classes !ex:b (object-complement-of !ex:a)))
	   :signature (!ex:b !ex:a)))))

(prove:is 
  (with-ontology ontology ()
      ((asq (subclassof !a !owl:Nothing )
	    (subclassof !owl:Thing !a) ))
      (explain-inconsistency ontology))
  (evurl
   '((:entailment (sub-class-of !owl:Thing !owl:Nothing)
      :support ((sub-class-of !owl:Thing !ex:a) (sub-class-of !ex:a !owl:Nothing))
      :signature (!owl:Nothing !ex:a !owl:Thing)))))

(prove:finalize)

(prove:plan 6)

(prove::is
 (with-ontology f () 
   ((asq  (declaration (object-property !c))
	  (declaration (object-property !d))
	  (equivalent-object-properties !c !d)))
   (instantiate-reasoner f :hermit :config (quiet-reasoner-config))
   (property-equivalents !c))
 (list  !c !d))

(prove::is
 (with-ontology f () 
   ((asq  (declaration (data-property !c))
	  (declaration (data-property !d))
	  (equivalent-data-properties !c !d)))
   (instantiate-reasoner f :hermit :config (quiet-reasoner-config))
   (property-equivalents !c))
 (list  !c !d))

(prove::is
 (with-ontology f () 
   ((asq (declaration (object-property !a))
	 (declaration (object-property !b))
	 (sub-object-property-of !a !b)))
   (instantiate-reasoner f :hermit :config (quiet-reasoner-config))
   (property-parents !a))
 (list !b))

(prove::is
 (with-ontology f () 
   ((asq (declaration (object-property !a))
	 (declaration (object-property !b))
	 (sub-object-property-of !a !b)))
   (instantiate-reasoner f :hermit :config (quiet-reasoner-config))
   (property-ancestors !a))
 (list !b))

(prove::is
 (with-ontology f () 
   ((asq (declaration (object-property !a))
	 (declaration (object-property !b))
	 (sub-object-property-of !a !b)))
   (instantiate-reasoner f :hermit :config (quiet-reasoner-config))
   (property-children !b))
 (list !a))

(prove::is
 (with-ontology f () 
   ((asq (declaration (object-property !a))
	 (declaration (object-property !b))
	 (sub-object-property-of !a !b)))
   (instantiate-reasoner f :hermit :config (quiet-reasoner-config))
   (property-descendants !b))
 (list !a))

(prove:finalize) 

(prove:plan 4)

(prove::is
 (all-owl2-term-alternatives 'class)
 '(class class "Class"))


(prove::is
 (all-owl2-term-alternatives 'sub-class-of)
 '(subclassof sub-class-of "SubClassOf" sub-class-of subclass-of < :<))

(prove::is
 (all-owl2-term-alternatives '<)
 '(subclassof sub-class-of "SubClassOf" sub-class-of subclass-of < :<))

(prove::is
 (all-owl2-term-alternatives "InverseOf")
 '(objectinverseof object-inverse-of "ObjectInverseOf" "InverseOf"))

(prove:finalize)


