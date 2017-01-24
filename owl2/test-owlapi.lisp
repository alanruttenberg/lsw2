(in-package :cl-user)

(with-ontology test-ontology ()
  ()
  (let ((expressions
	  '((subclassof !a !b)
	    (subobjectpropertyof !a !b)
	    (object-property-domain !a !b)))
	(*read-time-uri* t))
	
    (prove:plan (length expressions))
    (loop for expression in expressions 
	  do
	     (eval `(prove:is-type (to-owlapi-axiom ',expression *test-ontology*) 'java-object)))))

(prove:finalize)

(prove:plan  1)

(prove:is 
 (with-ontology f () 
   ((asq (< (annotation !this !one) !a !b) (= !b (not !a)))) (check-ontology f)
   (explain-unsatisfiable-class f !a))
 '((:entailment (sub-class-of !ex:a !owl:Nothing) 
    :support ((sub-class-of !ex:a !ex:b)
	      (equivalent-classes !ex:b (object-complement-of !ex:a)))
    :signature (!ex:b !ex:a))))

(prove:finalize)

