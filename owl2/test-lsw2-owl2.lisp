(in-package :cl-user)

(with-ontology test-ontology ()
  ()
  (let ((expressions
	  '((subclassof !a !b)
	    (subobjectpropertyof !a !b)
	    (object-property-domain !a !b))))

    (prove:plan (length expressions) "Lispy axioms")

    (loop for expression in expressions 
	  do
	     (eval `(prove:is-type (to-owlapi-axiom ',expression ,test-ontology) 'java-object "construct axiom from sexp")))))

(prove:finalize)

(prove:plan 3 "Basic reasoner")

(prove.test::ok
 (with-ontology f () 
   ((asq (subclassof !a !b))) 
   (check-ontology f :classify t))
 "ontology can be classified")

(prove.test::ok
 (with-ontology f () 
   ((asq (declaration (class !a))
	 (declaration (class !b))
	 (disjoint-classes !a !b)
	 (declaration (named-individual !h))
	 (class-assertion !a !h)
	 (class-assertion !b !h)))
   (not (check-ontology f)))
 "inconsistent ontology recognized")

(prove.test::ok
 (with-ontology f () 
   ((asq (declaration (class !a))
	 (declaration (class !b))
	 (disjoint-classes !a !b)
	 (declaration (class !c))
	 (equivalent-classes !c (and !a !b))
	 ))
   (unsatisfiable-classes f))
 "unsatisfiable class recognized")

(prove:finalize)

(prove:plan 2 "Explanation")


(prove:is 
 (with-ontology f () 
   ((asq (subclassof (annotation !this !one) !a !b) (= !b (not !a)))) 
   (check-ontology f)
   (explain-unsatisfiable-class f !a))
 (evurl '((:entailment (sub-class-of !ex:a !owl:Nothing) 
    :support ((sub-class-of !ex:a !ex:b)
	      (equivalent-classes !ex:b (object-complement-of !ex:a)))
	   :signature (!ex:b !ex:a))))
 "explain unsatisfiable class")

(prove:is 
  (with-ontology ontology ()
      ((asq (subclassof !a !owl:Nothing )
	    (subclassof !owl:Thing !a) ))
      (explain-inconsistency ontology))
  (evurl
   '((:entailment (sub-class-of !owl:Thing !owl:Nothing)
      :support ((sub-class-of !owl:Thing !ex:a) (sub-class-of !ex:a !owl:Nothing))
      :signature (!owl:Nothing !ex:a !owl:Thing))))
  "Explain inconsistency")

(prove:finalize)

(prove:plan 6 "Property queries")

(prove::is
 (with-ontology f () 
   ((asq  (declaration (object-property !c))
	  (declaration (object-property !d))
	  (equivalent-object-properties !c !d)))
   (instantiate-reasoner f :hermit :config (quiet-reasoner-config))
   (property-equivalents !c))
 (list  !c !d)
 "property-equivalents (object-property)")

(prove::is
 (with-ontology f () 
   ((asq  (declaration (data-property !c))
	  (declaration (data-property !d))
	  (equivalent-data-properties !c !d)))
   (instantiate-reasoner f :hermit :config (quiet-reasoner-config))
   (property-equivalents !c))
 (list  !c !d)
 "property-equivalents (data-property)")

(prove::is
 (with-ontology f () 
   ((asq (declaration (object-property !a))
	 (declaration (object-property !b))
	 (sub-object-property-of !a !b)))
   (instantiate-reasoner f :hermit :config (quiet-reasoner-config))
   (property-parents !a))
 (list !b)
 "property-parents")

(prove::is
 (with-ontology f () 
   ((asq (declaration (object-property !a))
	 (declaration (object-property !b))
	 (sub-object-property-of !a !b)))
   (instantiate-reasoner f :hermit :config (quiet-reasoner-config))
   (property-ancestors !a))
 (list !b)
 "property-ancestors")

(prove::is
 (with-ontology f () 
   ((asq (declaration (object-property !a))
	 (declaration (object-property !b))
	 (sub-object-property-of !a !b)))
   (instantiate-reasoner f :hermit :config (quiet-reasoner-config))
   (property-children !b))
 (list !a)
 "property-children")

(prove::is
 (with-ontology f () 
   ((asq (declaration (object-property !a))
	 (declaration (object-property !b))
	 (sub-object-property-of !a !b)))
   (instantiate-reasoner f :hermit :config (quiet-reasoner-config))
   (property-descendants !b))
 (list !a)
 "property-descendants")

(prove:finalize) 

(prove:plan 4 "OWL syntax terminals")

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


