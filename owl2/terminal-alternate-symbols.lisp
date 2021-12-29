(in-package :cl-user)
(defpackage :lsw2/owlterm (:use))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Manage alternative ways of writing OWL2 terms.
;;
;; API

(defun all-owl2-term-alternatives (term)
  "Return all possible writings of term. E.g. subclassof -> (subclassof sub-class-of \"SubClassOf\" subclass-of '<)"
  (or (gethash term *owl2-vocabulary-forms*)
      (error "~a isn't an OWL2 terminal symbol at the moment" term)))

(defun canonical-owl2-term (term)
  "For a given (potential synonym for) an OWL term, return the canonical term. By preference that's the one where changes in case are changed to dashes. E.g. (canonical-owl2-term '<) -> sub-class-of"
  (or (second (gethash term *owl2-vocabulary-forms*))
      (error "~a isn't an OWL2 terminal symbol at the moment" term)))

(defun rewrite-owl-canonical-functional (expression)
  (if (atom expression)
      (or (car (gethash expression *owl2-vocabulary-forms*))
	  (let ((here (and (symbolp expression) (find-symbol (string expression) (load-time-value *package*)))))
	    (and here (car (gethash here *owl2-vocabulary-forms*))))
	  expression)
      (mapcar 'rewrite-owl-canonical-functional expression)))
  
(defun owl-vocabulary-terms ()
  (let ((terms (remove-if 'keywordp (remove-if 'stringp (alexandria::hash-table-keys *owl2-vocabulary-forms*)))))
    (loop for entry in (alexandria::hash-table-values *owl2-vocabulary-forms*)
	  do (loop for el in entry
		   when (and (symbolp el) (not (keywordp el)))
		     do (pushnew el terms)))
    terms))

						  
	       
	 
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation
;; A hash table is made where a value is a list of alternatives.
;; A value has (in this order)
;;  - the lowercased string used in the spec
;;  - the lispish syntax symbol - dashes where the capital letters are
;;  - The string used in Functional Syntax
;;  - &rest more synonyms

(flet ((capitalize-by-hyphen (symbol)
	 (apply 'concatenate 'string 
		(mapcar 'string-capitalize 
			(split-at-char (string symbol) "-")))))
  (defparameter *owl2-vocabulary-forms*
    (let ((table (make-hash-table :test 'equalp)))
      (loop for (un worded . other-synonyms) in
	    '((annotation annotation)
	      (annotationassertion annotation-assertion)
	      (annotationproperty annotation-property)
	      (annotationpropertydomain annotation-property-domain)
	      (annotationpropertyrange annotation-property-range)
	      (asymmetricobjectproperty asymmetric-object-property)
	      (class class)
	      (classassertion class-assertion)
	      (dataallvaluesfrom data-all-values-from)
	      (datacomplementof data-complement-of)
	      (dataexactcardinality data-exact-cardinality)
	      (datahasvalue data-has-value)
	      (dataintersectionof data-intersection-of)
	      (datamaxcardinality data-max-cardinality)
	      (datamincardinality data-min-cardinality)
	      (dataoneof data-one-of)
	      (dataproperty data-property)
	      (datapropertyassertion data-property-assertion)
	      (datapropertydomain data-property-domain)
	      (datapropertyrange data-property-range)
	      (datasomevaluesfrom data-some-values-from)
	      (datatype datatype)
	      (datatypedefinition datatype-definition)
	      (datatyperestriction datatype-restriction)
	      (dataunionof data-union-of)
	      (declaration declaration)
	      (differentindividuals different-individuals)
	      (disjointclasses disjoint-classes)
	      (disjointdataproperties disjoint-data-properties)
	      (disjointobjectproperties disjoint-object-properties)
	      (disjointunion disjoint-union)
	      (equivalentclasses equivalent-classes)
	      (equivalentdataproperties equivalent-data-properties)
	      (equivalentobjectproperties equivalent-object-properties)
	      (functionaldataproperty functional-data-property)
	      (functionalobjectproperty functional-object-property)
	      (haskey has-key)
	      (import import imports)
	      (inversefunctionalobjectproperty inverse-functional-object-property)
	      (inverseobjectproperties inverse-object-properties)
	      (irreflexiveobjectproperty irreflexive-object-property)
	      (namedindividual named-individual)
	      (negativedatapropertyassertion negative-data-property-assertion)
	      (negativeobjectpropertyassertion negative-object-property-assertion)
	      (objectallvaluesfrom object-all-values-from)
	      (objectcomplementof object-complement-of)
	      (objectexactcardinality object-exact-cardinality)
	      (objecthasself object-has-self)
	      (objecthasvalue object-has-value)
	      (objectintersectionof object-intersection-of)
	      (objectinverseof object-inverse-of "InverseOf")
	      (objectmaxcardinality object-max-cardinality)
	      (objectmincardinality object-min-cardinality)
	      (objectoneof object-one-of)
	      (objectproperty object-property)
	      (objectpropertyassertion object-property-assertion)
	      (objectpropertychain object-property-chain)
	      (objectpropertydomain object-property-domain)
	      (objectpropertyrange object-property-range)
	      (objectsomevaluesfrom object-some-values-from)
	      (objectunionof object-union-of)
	      (ontology ontology)
	      (reflexiveobjectproperty reflexive-object-property)
	      (sameindividual same-individual)
	      (subannotationpropertyof sub-annotation-property-of)
	      (subclassof sub-class-of sub-class-of subclass-of)
	      (subdatapropertyof sub-data-property-of)
	      (subobjectpropertyof sub-object-property-of)
	      (symmetricobjectproperty symmetric-object-property)
	      (transitiveobjectproperty transitive-object-property))
	    for capitalized = (capitalize-by-hyphen worded)
	    for entry = (list* un worded capitalized other-synonyms)
	    do
	       (setf (gethash un table) entry)
	       (setf (gethash worded table) entry)
	       (setf (gethash capitalized table) entry)
	       (loop for s in other-synonyms do (setf (gethash s table) entry))
	    )
      (maphash (lambda(k v)
		 (when (and (symbolp k) (not (keywordp k)))
		   (import k 'lsw2/owlterm)
		   (export k 'lsw2/owlterm)))
	       table)
      table))
  "A value has (in this order)
  - the lowercased string used in the spec
  - the lispish syntax symbol - dashes where the capital letters are
  - The string used in Functional Syntax
  - &rest more synonyms
  An entry in the table is recorded with the key being each of the alternatives in the value")


(defparameter *owl2-manchesterish-function-syntax-terms*
 '((objectintersectionof and)
   (objectunionof or)
   (objectsomevaluesfrom some)
   (objectallvaluesfrom all)
   (objecthasvalue value)
   (equivalentclasses =)
   (subclassof <)
   (subobjectpropertyof p<)
   (object-property-chain o)
   (objectmincardinality min)
   (classassertion type)
   (objectmaxcardinality max)
   (objectexactcardinality exactly)
   (objectcomplementof not)
   (declaration is))
  "A list of ad-hoc abbreviations to make writing short ontologies by hand easier"
  )
   
(defun add-manchesterish-abbreviations ()
  "Write this:
 (with-ontology foo () 
   ((asq
     (= !probe (and !a (some !r !b)))
     (= !probe2 (and !a1 (some !r !b1)))
     (< !a1 !a)
     (< !b !b1)
     (type (and !probe2 (not !probe1)) !probeinstance))))

  Instead of this:

 (with-ontology ont ()
  ((sub-class-of !a1 !a)
   (sub-class-of !b !b1)
   (equivalent-classes !probe (object-intersection-of !a (object-some-values-from !r !b)))
   (equivalent-classes !probe2 (object-intersection-of !a1 (object-some-values-from !r !b1)))
   (class-assertion (object-intersection-of !probe2 (object-complement-of !probe1)) !probeinstance)))
"
  (loop for (base abbrev) in *owl2-manchesterish-function-syntax-terms*
	for entry = (gethash base *owl2-vocabulary-forms*)
	for keyword-abbrev = (intern (string abbrev) 'keyword)
	do
	   (nconc entry (list abbrev keyword-abbrev))
	   (setf (gethash abbrev *owl2-vocabulary-forms*) entry)
	   (setf (gethash keyword-abbrev *owl2-vocabulary-forms*) entry)))

(add-manchesterish-abbreviations)

