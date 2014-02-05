(defun hidden-axioms ()
  '(equivalent-classes (object-some-values-from (object-inverse-Of !r) !b) (object-union-of !c !d)))

(defun example-1-hidden-axioms-inferred-domain ()
(with-ontology test (:collecting t)
	     ((asq (declaration (object-property !r))
		   (declaration (class !a))
		   (declaration (class !b))
		   (declaration (class !c))
		   (declaration (class !d))
		   (sub-class-of !b !a)
		   (object-property-domain !r !a)
		   (object-property-range !r !a))
	      (as (hidden-axioms)))
	   (children '(object-some-values-from (object-inverse-of !r) !b) test))


(defun example-2-narrowing-inferred-domain ()
  (with-ontology test (:collecting t)
      ((asq (declaration (object-property !r))
	    (declaration (object-property !r2))
	    (declaration (class !a))
	    (declaration (class !b))
	    (declaration (class !c))
	    (declaration (class !d))
	    (sub-class-of !b !a)
	    (object-property-domain !r !a)	; r a->thing
	    (object-property-range !r !owl:Thing) ; r a->thing
	    (object-property-domain !r2 !b)	  
	    (object-property-range !r2 (object-intersection-of !c !d)) ; r2 b->(c intersection d)
	    (sub-object-property-of !r !r2))) ; x r y => x r2 y
    (parents '(object-some-values-from (object-inverse-of !r) !Thing) test)))	 


