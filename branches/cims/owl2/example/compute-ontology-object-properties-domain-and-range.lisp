(defun example-2-narrowing-inferred-domain ()
  (with-ontology test (:collecting t)
      ((asq (declaration (object-property !r))
	    (declaration (class !a))
	    (declaration (class !b))
	    (declaration (class !c))
	    (declaration (class !d))
	    (sub-class-of !b !a)

	    (object-property-domain !r2 !b)	  

	    (object-property-range !r2 (object-intersection-of !c !d)) ; r2 b->(c intersection d)

	    ))

    (children '(object-some-values-from (object-inverse-of !r) !Thing) test)))

(defun get-classes-and-properties (ont)
  (let ((object-properties nil)
	(classes nil))
    (maphash (lambda (uri entries) 
	       (loop for entry in entries
		  do
		    (destructuring-bind (entity type onto) entry
		      (when (eq type :object-property)
			(pushnew uri object-properties))
		      (when (eq type :class)
			(pushnew uri classes)))))
	     (v3kb-uri2entity ont))
    (values classes object-properties)))


;; There are two ways to determine the domain and range of an object property.
;; The first is to get the asserted domain/range. This domain and range give the hardest bounds.
;; The other way is to use the OWLAPI method getObjectPropertyDomains and getObjectPropertyRanges or the method it uses:

;; pe - The property expression whose domains are to be retrieved.
;; direct - Specifies if the direct domains should be retrieved (true), or if all domains should be retrieved (false).
;; Returns:
;; Let N = getEquivalentClasses(ObjectSomeValuesFrom(pe owl:Thing)).
;; If direct is true: then if N is not empty then the return value is N, else the return value is the result of getSuperClasses(ObjectSomeValuesFrom(pe owl:Thing), true).
;; If direct is false: then the result of getSuperClasses(ObjectSomeValuesFrom(pe owl:Thing), false) together with N if N is non-empty.

;; pe - The property expression whose ranges are to be retrieved.
;; direct - Specifies if the direct ranges should be retrieved (true), or if all ranges should be retrieved (false).
;; Returns:
;; Let N = getEquivalentClasses(ObjectSomeValuesFrom(ObjectInverseOf(pe) owl:Thing)).
;; If direct is true: then if N is not empty then the return value is N, else the return value is the result of getSuperClasses(ObjectSomeValuesFrom(ObjectInverseOf(pe) owl:Thing), true).
;; If direct is false: then the result of getSuperClasses(ObjectSomeValuesFrom(ObjectInverseOf(pe) owl:Thing), false) together with N if N is non-empty.

;; Comments on the OWLAPI Methods 
;; getSuperClasses returns the direct superclasses. 
;; If there is more than one equivalent class then any is suitable and exact

;; The OWLAPI methods only return named classes, so if the domain of a
;; property is stated as a class expression and there is no named
;; class that is equivalent then the methods can only return an
;; approximation of the domain/range.

;; If superclasses are used, then it is, effectively, the union of
;; these classes that cover the domain or range, but may include
;; individuals which may not be the subject of the relation.

;; Therefore, if explicit axiom on domain/range are made, they should
;; be used, though they will need to be accmulateed from parents.
;; If not then we use the interferred domain/range.

;; The, iterating through the properties we try all classes as domain
;; finding range, all classes as range, finding domain.  This follows
;; the pattern for computing domain/range but substituting the probe
;; class for owl:Thing.  getEquivalentClasses(ObjectSomeValuesFrom(pe
;; <probeClass>)).


(defun image-of-property-for-class (property classes ont)
  (let ((biggest-range (parents `(object-some-values-from (object-inverse-of ,property) !Thing) ont)) ;image of Thing
	(biggest-domain (parents `(object-some-values-from ,property !Thing) ont)) ; inverse image of Thing
	(property-domain (domains property)) ; reasoner calculated
	(property-range (domains property))) ; reasoner calculated
    (loop for class in classes
       for domain = (parents `(object-some-values-from ,property ,class) ont)
       for domain-satisfiable = (satisfiable? `(object-intersection-of ,class ,@biggest-domain) ont)
       for range = (parents `(object-some-values-from (object-inverse-of ,property) ,class) ont)
       for range-satisfiable = (satisfiable? `(object-intersection-of ,@range) ont)
       when (and domain-satisfiable range-satisfiable)
       do (format t "It is possible that ~s ~s ~s.~%"
		  (car (rdfs-label class ont)) 
		  (car (rdfs-label property ont))
		  (car (mapcar (lambda(e) (rdfs-label e ont)) range )))
	 (break))))






