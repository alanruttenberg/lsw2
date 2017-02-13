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
   ((asq (< (annotation !this !one) !a !b) (= !b (not !a)))) 
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


