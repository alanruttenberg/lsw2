(in-package :logic)
(eval-when (:load-toplevel :execute)
  (prove:plan 14)
  (prove:is (logic::z3-check-true
             '(:owl (equivalent-classes (object-exact-cardinality 2 rel b) 
                     (object-intersection-of  
                      (object-min-cardinality 2 rel b)
                      (object-max-cardinality 2 rel b)))))
            :proved
            "Cardinality translations and relationships" )

  (prove:is (owl-sexp-to-fol '(property-assertion r a (:blank foo)))
            '(:exists (:?_foo) (property-assertion r a :?_foo))
            "blank node translation")

  (prove:is (owl-sexp-to-fol '(equivalent-classes a (object-min-cardinality 1 r b)))
            '(:forall (?p)
              (:iff (rdf-type a ?p)
                  (:exists (?q) (:and (r ?p ?q) 
                                      (rdf-type b ?q)))))
            "ObjectMinCardinality translation")

  (prove:is (z3-check-true '(:owl (equivalent-classes (object-some-values-from r b)  (object-min-cardinality 1 r b))))
            :proved
            "ObjectSomeValuesFrom same as ObjectMinCardinality 1")

  (prove:is (owl-sexp-to-fol '(object-property-domain r b1 ))
            '(:forall (?p ?q) (:implies (r ?p ?q) (rdf-type b1 ?p)))
            "ObjectPropertyDomain translation")

  (prove:is (owl-sexp-to-fol '(object-property-range r b1 ))
            '(:forall (?p ?q) (:implies (r ?q ?p) (rdf-type b1 ?p)))
            "ObjectPropertyRange translation")

  (prove:is (owl-sexp-to-fol '(equivalent-classes a
                               (object-all-values-from r b1 )))
            '(:forall (?p)
              (:iff (rdf-type a ?p)
                  (:forall (?q)
                    (:implies (r ?p ?q) (rdf-type b1 ?q)))))
            "AllValuesFrom translation")

  (prove:is (owl-sexp-to-fol '(equivalent-classes a
                               (object-union-of b c)))
            '(:forall (?p) (:iff (rdf-type a ?p) 
                               (:or (rdf-type b ?p) (rdf-type c ?p))))
            "ObjectUnionOf translation")

  (prove.test:is (owl-sexp-to-fol '(equivalent-classes a b))
                 '(:forall (?p) (:iff (rdf-type a ?p) (rdf-type b ?p)))
                 "EquivalentClasses translation")

  (prove.test:is (owl-sexp-to-fol '(equivalent-classes a
                                    (object-has-self r)))
                 '(:forall (?p) (:iff (rdf-type a ?p) (r ?p ?p)))
                 "ObjectHasSelf translation")

  (prove.test:is (owl-sexp-to-fol '(equivalent-classes a
                                    (object-has-value r v)))
                 '(:forall (?p) (:iff (rdf-type a ?p) (r ?p v)))
                 "ObjectHasValue translation")


  (prove.test:is (owl-sexp-to-fol '(equivalent-classes a
                                    (object-all-values-from r b)))
                 '(:forall (?p)
                   (:iff (rdf-type a ?p)
                       (:forall (?q)
                         (:implies (r ?p ?q) (rdf-type b ?q)))))
                 "AllValuesFrom translation")

  (prove.test:is (owl-sexp-to-fol '(equivalent-classes
                                    (object-some-values-from r a)
                                    (object-some-values-from r b)))
                 '(:forall (?p)
                   (:iff
                       (:exists (?q) (:and (r ?p ?q) (rdf-type a ?q)))
                       (:exists (?q) (:and (r ?p ?q) (rdf-type b ?q)))))
                 "Two ObjectSomevaluesFroms")

  (prove.test:is (owl-sexp-to-fol '(object-property-assertion r a b))
                 '(r a b)
                 "ObjectPropertyAssertion translation")

  (prove:finalize)

  (defparameter *skip-owl-tests* 
    '(("New-Feature-Keys-001" :data)
      ("New-Feature-Keys-003" :data)
      ("New-Feature-ObjectPropertyChain-BJP-003" :empty-consequent)
      ("TestCase:WebOnt-InverseFunctionalProperty-003" :not-dl)
      ("TestCase:WebOnt-InverseFunctionalProperty-004" :not-dl)
      ("TestCase:WebOnt-Nothing-002" :not-dl)
      ("TestCase:WebOnt-Ontology-004" :not-dl)
      ("TestCase:WebOnt-Restriction-006" :not-dl)
      ("TestCase:WebOnt-SymmetricProperty-001" :not-dl)
      ("TestCase:WebOnt-SymmetricProperty-003" :not-dl)
      ("TestCase:WebOnt-TransitiveProperty-001" :not-dl)
      ("TestCase:WebOnt-cardinality-006" :not-dl)
      ("TestCase:WebOnt-complementOf-001" :not-dl)
      ("TestCase:WebOnt-description-logic-661" :data)
      ("TestCase:WebOnt-description-logic-662" :data)
      ("TestCase:WebOnt-description-logic-663" :data)
      ("TestCase:WebOnt-description-logic-664" :data)
      ("TestCase:WebOnt-description-logic-665" :data)
      ("TestCase:WebOnt-description-logic-667" :data)
      ("TestCase:WebOnt-differentFrom-002" :not-dl)
      ("TestCase:WebOnt-disjointWith-002" :not-dl)
      ("TestCase:WebOnt-distinctMembers-001" :not-dl)
      ("TestCase:WebOnt-equivalentClass-007" :not-dl)
      ("TestCase:WebOnt-equivalentClass-008-Direct"  :empty-consequent)
      ("TestCase:WebOnt-equivalentProperty-005" :not-dl)
      ("TestCase:WebOnt-equivalentProperty-006" :not-dl)
      ("TestCase:WebOnt-extra-credit-002" :not-dl)
      ("TestCase:WebOnt-extra-credit-003" :not-dl)
      ("TestCase:WebOnt-extra-credit-004" :not-dl)
      ("TestCase:WebOnt-imports-001" :imports)
      ("TestCase:WebOnt-imports-003" :imports)
      ("TestCase:WebOnt-imports-010" :not-dl)
      ("TestCase:WebOnt-imports-011" :imports)
      ("TestCase:WebOnt-intersectionOf-001" :not-dl)
      ("TestCase:WebOnt-inverseOf-001" :not-dl)
      ("TestCase:WebOnt-miscellaneous-010" :imports)
      ("TestCase:WebOnt-miscellaneous-011" :imports)
      ("TestCase:WebOnt-oneOf-002" :not-dl)
      ))

  (defparameter *all-owl-tests*
    '("New-Feature-DisjointUnion-001"
      "New-Feature-Keys-001"
      "New-Feature-Keys-003"
      "New-Feature-ObjectPropertyChain-001"
      "New-Feature-ObjectPropertyChain-BJP-002"
      "New-Feature-ObjectPropertyChain-BJP-003"
      "New-Feature-ObjectQCR-001"
      "New-Feature-ObjectQCR-002"
      "New-Feature-ReflexiveProperty-001"
      "New-Feature-SelfRestriction-001"
      "New-Feature-SelfRestriction-002"

      "TestCase:WebOnt-InverseFunctionalProperty-003" 
      "TestCase:WebOnt-InverseFunctionalProperty-004"
      "TestCase:WebOnt-Nothing-002"
      "TestCase:WebOnt-Ontology-001"
      "TestCase:WebOnt-Ontology-004"
      "TestCase:WebOnt-Restriction-005-direct"
      "TestCase:WebOnt-Restriction-006"
      "TestCase:WebOnt-SymmetricProperty-001"
      "TestCase:WebOnt-SymmetricProperty-002"
      "TestCase:WebOnt-SymmetricProperty-003"
      "TestCase:WebOnt-TransitiveProperty-001"
      "TestCase:WebOnt-TransitiveProperty-002"
      "TestCase:WebOnt-allValuesFrom-001"
      "TestCase:WebOnt-cardinality-001"
      "TestCase:WebOnt-cardinality-002"
      "TestCase:WebOnt-cardinality-003"
      "TestCase:WebOnt-cardinality-004"
      "TestCase:WebOnt-cardinality-006"
      "TestCase:WebOnt-complementOf-001"
      "TestCase:WebOnt-description-logic-201"
      "TestCase:WebOnt-description-logic-202"
      "TestCase:WebOnt-description-logic-203"
      "TestCase:WebOnt-description-logic-204"
      "TestCase:WebOnt-description-logic-205"
      "TestCase:WebOnt-description-logic-206"
      "TestCase:WebOnt-description-logic-207"
      "TestCase:WebOnt-description-logic-208"
      "TestCase:WebOnt-description-logic-661"
      "TestCase:WebOnt-description-logic-662"
      "TestCase:WebOnt-description-logic-663"
      "TestCase:WebOnt-description-logic-664"
      "TestCase:WebOnt-description-logic-665"
      "TestCase:WebOnt-description-logic-667"
      "TestCase:WebOnt-description-logic-901"
      ;; cardinality 200                    "TestCase:WebOnt-description-logic-903" ;

      "TestCase:WebOnt-differentFrom-001"
      "TestCase:WebOnt-differentFrom-002"
      "TestCase:WebOnt-disjointWith-001"
      "TestCase:WebOnt-disjointWith-002"
      "TestCase:WebOnt-distinctMembers-001"
      "TestCase:WebOnt-equivalentClass-001"
      "TestCase:WebOnt-equivalentClass-002"
      "TestCase:WebOnt-equivalentClass-003"
      "TestCase:WebOnt-equivalentClass-004"
      "TestCase:WebOnt-equivalentClass-006"
      "TestCase:WebOnt-equivalentClass-007"
      "TestCase:WebOnt-equivalentClass-008-Direct"
      "TestCase:WebOnt-equivalentProperty-001"
      "TestCase:WebOnt-equivalentProperty-002"
      "TestCase:WebOnt-equivalentProperty-003"
      "TestCase:WebOnt-equivalentProperty-004"
      "TestCase:WebOnt-equivalentProperty-005"
      "TestCase:WebOnt-equivalentProperty-006"
      "TestCase:WebOnt-extra-credit-002"
      "TestCase:WebOnt-extra-credit-003"
      "TestCase:WebOnt-extra-credit-004"
      "TestCase:WebOnt-imports-001"
      "TestCase:WebOnt-imports-003"
      "TestCase:WebOnt-imports-010"
      "TestCase:WebOnt-imports-011"
      "TestCase:WebOnt-intersectionOf-001"
      "TestCase:WebOnt-inverseOf-001"
      "TestCase:WebOnt-miscellaneous-010"
      "TestCase:WebOnt-miscellaneous-011"
      "TestCase:WebOnt-miscellaneous-302-Direct"
      "TestCase:WebOnt-oneOf-002"
      ))

  (prove:plan 38)
  (loop for test in *all-owl-tests*
        for url = (format nil "http://owl.semanticweb.org/page/~a.html" test)
        unless (find test *skip-owl-tests* :key 'car :test 'equalp)
          do (prove:ok (check-positive-entailment-test-case url nil 'vampire-prove)
                       test))
  (prove:finalize)


  )
