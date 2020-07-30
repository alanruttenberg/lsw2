(in-package :cl-user)

(defun short-form-provider (kb &key
				 (properties (list !rdfs:label))
				 (language-preferences '("en"))
				 (replace nil)
				 (quoted-when-has-space t))
  (or (and (not replace) (v3kb-short-form-provider kb))
      (setf (v3kb-short-form-provider kb) 
	    (make-short-form-provider
	     kb
	     :properties properties :language-preferences language-preferences
	     :quoted-when-has-space quoted-when-has-space ))))


(defun make-short-form-provider (kb &key
				      (properties (list !rdfs:label))
				      (language-preferences '("en"))
				      (quoted-when-has-space t))
  (let* ((a-props (new 'java.util.arraylist)) 
	 (langs (new 'java.util.arraylist))
	 (langs-none (new 'java.util.arraylist))
	 (prop->lang (new 'util.hashmap))
	 (prop->lang-none (new 'util.hashmap))
	 (ontset (new 'owlapi.util.owlontologyimportsclosuresetprovider
		      (v3kb-manager kb) (v3kb-ont kb))))
    (dolist (lang language-preferences)
      (#"add" langs lang))
    (dolist (prop properties)
      (#"add" a-props (get-entity prop :annotation-property kb))
      (#"put" prop->lang (get-entity prop :annotation-property kb) langs)
      (#"put" prop->lang-none (get-entity prop :annotation-property kb) langs-none))
    (let ((any-lang-provider (new 'owlapi.util.annotationvalueshortformprovider a-props prop->lang-none ontset
				  (new 'simpleshortformprovider))))
      (when quoted-when-has-space
	(#"setLiteralRenderer" any-lang-provider (new 'quotedStringAnnotationVisitor)))
      (let ((lang-specific-provider (new 'owlapi.util.annotationvalueshortformprovider a-props prop->lang ontset any-lang-provider)))
	(#"setLiteralRenderer" lang-specific-provider (new 'quotedStringAnnotationVisitor))
	(new 'owlapi.util.BidirectionalShortFormProviderAdapter (#"getImportsClosure" (v3kb-ont kb))
	     lang-specific-provider)))))


(defun camelCase (label &optional initialcap)
  (when (#"matches" label "'.*'$")
    (setq label (subseq label 1 (- (length label) 1))))
  (let* ((words (jss::all-matches label "([^_\\- ]+)" 1))
	 (humped (apply 'concatenate 'string (mapcar (lambda(word) (string-capitalize (car word))) words))))
    (unless initialcap
      (setf (char humped 0) (char-downcase (char humped 0))))
    humped))

(defun make-camel-case-short-form-provider (ontology)
  (let ((short-form-provider (short-form-provider ontology)))
    (flet ((camelCase (entity initialcap)
	     (camelCase (#"getShortForm" short-form-provider entity) initialcap)))
      (jinterface-safe-implementation
       "org.semanticweb.owlapi.util.ShortFormProvider"
       "dispose" (lambda())
       "getShortForm" (lambda(entity) (camelcase entity (not (jinstance-of-p entity "org.semanticweb.owlapi.model.OWLObjectProperty"))))))))
 
