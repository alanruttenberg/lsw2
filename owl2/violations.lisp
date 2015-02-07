(defparameter *owl-profile-violations*
  '("CycleInDatatypeDefinition"
    "DatatypeIRIAlsoUsedAsClassIRI"
    "LastPropertyInChainNotInImposedRange"
    "LexicalNotInLexicalSpace"
    "OntologyIRINotAbsolute"
    "OntologyVersionIRINotAbsolute"
    "UseOfAnonymousIndividual"
    "UseOfBuiltInDatatypeInDatatypeDefinition"
    "UseOfDataOneOfWithMultipleLiterals"
    "UseOfDefinedDatatypeInDatatypeRestriction"
    "UseOfIllegalAxiom"
    "UseOfIllegalClassExpression"
    "UseOfIllegalDataRange"
    "UseOfIllegalFacetRestriction"
    "UseOfNonAbsoluteIRI"
    "UseOfNonAtomicClassExpression"
    "UseOfNonEquivalentClassExpression"
    "UseOfNonSimplePropertyInAsymmetricObjectPropertyAxiom"
    "UseOfNonSimplePropertyInCardinalityRestriction"
    "UseOfNonSimplePropertyInDisjointPropertiesAxiom"
    "UseOfNonSimplePropertyInFunctionalPropertyAxiom"
    "UseOfNonSimplePropertyInInverseFunctionalObjectPropertyAxiom"
    "UseOfNonSimplePropertyInIrreflexivePropertyAxiom"
    "UseOfNonSimplePropertyInObjectHasSelf"
    "UseOfNonSubClassExpression"
    "UseOfNonSuperClassExpression"
    "UseOfObjectOneOfWithMultipleIndividuals"
    "UseOfObjectPropertyInverse"
    "UseOfPropertyInChainCausesCycle"
    "UseOfReservedVocabularyForAnnotationPropertyIRI"
    "UseOfReservedVocabularyForClassIRI"
    "UseOfReservedVocabularyForDataPropertyIRI"
    "UseOfReservedVocabularyForIndividualIRI"
    "UseOfReservedVocabularyForObjectPropertyIRI"
    "UseOfReservedVocabularyForOntologyIRI"
    "UseOfReservedVocabularyForVersionIRI"
    "UseOfTopDataPropertyAsSubPropertyInSubPropertyAxiom"
    "UseOfUndeclaredAnnotationProperty"
    "UseOfUndeclaredClass"
    "UseOfUndeclaredDataProperty"
    "UseOfUndeclaredDatatype"
    "UseOfUndeclaredObjectProperty"
    "UseOfUnknownDatatype"))

(defparameter *owl-profile-violations-preventing-reasoning*
  '("CycleInDatatypeDefinition"
    "DatatypeIRIAlsoUsedAsClassIRI"
    "LastPropertyInChainNotInImposedRange"
    "UseOfNonSimplePropertyInAsymmetricObjectPropertyAxiom"
    "UseOfNonSimplePropertyInCardinalityRestriction"
    "UseOfNonSimplePropertyInDisjointPropertiesAxiom"
    "UseOfNonSimplePropertyInFunctionalPropertyAxiom"
    "UseOfNonSimplePropertyInInverseFunctionalObjectPropertyAxiom"
    "UseOfNonSimplePropertyInIrreflexivePropertyAxiom"
    "UseOfNonSimplePropertyInObjectHasSelf"
    "UseOfPropertyInChainCausesCycle"
    "UseOfTopDataPropertyAsSubPropertyInSubPropertyAxiom"
    ))

(defun check-profile (ont &key (profile 'dl))
  (let* ((profiler (new (intern (format nil "OWL2~aPROFILE" profile))))
	 (report (#"checkOntology" profiler (v3kb-ont ont)))
	 (violations (set-to-list (#"getViolations" report)))
	 (axioms (mapcar #"getAxiom"  violations)))
    (values
    violations
    (loop for v in violations 
	 for vname = (jclass-name (jobject-class v))
	 for bareclass = (subseq vname (print (length "org.semanticweb.owlapi.profiles.")))
	 thereis (member bareclass *owl-profile-violations-preventing-reasoning* :test 'equalp)))))

(defun replace-uris-with-labels-in-report (ont report)
  (replace-all report "((?s)<(.*?)>)" (lambda(e) (format nil "'~a'" (car (rdfs-label (make-uri (subseq e 1 (- (length e) 1 ))) b)))) 1))