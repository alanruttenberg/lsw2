(in-package :cl-user)

(defvar *use-qnames* t)

(defparameter *blankprefix* "urn:blank:")

(defparameter *namespace-replacements* 
  `(("http://xmlns.com/wordnet/1.6/" "wordnet:") ; used in foaf
    ("http://www.inoh.org/owl#" "inoh:")
    ("http://purl.org/obo/owl/FBbt#FBbt_" "fb:")
    ("http://www.ebi.ac.uk/experimentalfactors.owl#" "efo:")
    ("http://www.lsrn.org/lsrn/schema.owl#" "lsrn:")
    ("http://www.biomodels.net/MIRIAM/" "mir:")
    ("http://sharedname.net/sn/" "sn:")
    ("http://xref.biopax.org/xref/" "bpxref:")
    ("http://purl.org/dc/elements/1.1/" "dc:") 
    ("urn:lsid:ncbi.nlm.nih.gov.lsid.biopathways.org:" "ncbilsid:")
    ("http://www.w3.org/2003/01/geo/wgs84_pos#" "geo:") ; used in foaf
    ("http://www.w3.org/2000/10/swap/pim/contact#" "con:") ; used in foaf
    ("http://www.wikipedia.com/wiki/" "wikipedia:")
    ("http://purl.org/dc/terms/" "dcterms:")
    ("http://www.biopax.org/release/biopax-level2.owl#" "bp2:")
    ("http://www.biopax.org/release/biopax-level1.owl#" "bp1:")
    ("http://xmlns.com/foaf/0.1/" "foaf:")
    ("http://www.w3.org/2003/11/swrl#" "swrl:")
    ("http://www.w3.org/2003/11/swrlb#" "swrlb:")
    ("http://www.w3.org/2006/gen/ont#" "gen:")
    ("http://www.w3.org/2000/10/swap/pim/doc#" "pim:")
;    ("http://www.geneontology.org/owl/#" "go:")
    ("http://purl.org/obo/owl/GO#GO_" "go:")
    ("http://purl.org/obo/owl/" "oboont:")
    ("http://www.biopax.org/prototype#" "proto:")
    ("http://www.reactome.org/biopax#" "reactome:")
    ("http://www.biopax.org/xref/cas/#" "cas:")
    ("http://www.biopax.org/xref/kegg/#" "kegg:")
    ("http://karma.med.harvard.edu/wiki/Debugging_the_bug/bug.owl#" "bug:")
    ("http://ecocyc.org/compound/#" "ecocyc:")
;    ("http://ecocyc.org/compounds/#" "ecomol:")
;    ("http://ecocyc.org/reactions/#" "ecoreact:")
    ("http://gcrg.ucsd.edu/iJR904/#" "palsson:")
    ("http://www.loa-cnr.it/ontologies/DOLCE-Lite#" "dolce:")
    ("http://www.w3.org/1999/02/22-rdf-syntax-ns#" "rdf:")
    ("http://www.ifomis.org/acgt/1.0#" "acgt:")
    ("http://mumble.net/#" "internal:")
    ("http://www.w3.org/2002/07/owl#" "owl:")
    ("http://www.w3.org/2007/OWL/testOntology#" "owl2t:")
    ("http://www.w3.org/2000/01/rdf-schema#" "rdfs:")
    ("http://example.com/" "ex:")
    ("http://example.org/" "exo:")
    ("http://www.w3.org/2001/XMLSchema#" "xsd:")
    ("http://www.w3.org/2004/02/skos/core#" "skos:")
    ("http://www.w3.org/2004/02/skos/extensions#" "skosx:")
    ("http://www.co-ode.org/ontologies/meta/2006/05/15/meta.owl" "meta:")
    ("http://semweb.med.yale.edu/NeuroWeb/owl/senselab#" "senselab:")
    ("http://semweb.med.yale.edu/NeuroWeb/owl/cocodat#" "cocodat:")
    ("http://neuroweb.med.yale.edu/senselab/neuron_ontology.owl#" "neurondb:")
    ("http://neuroscientific.net/ont/biopax-level2_neuro_extension.owl#" "neuro:")
    ("http://neuroscientific.net/ont/kidb#" "kidb:")
    ("http://purl.obolibrary.org/obo/" "obo:")
    ("http://purl.obolibrary.org/obo/" "obi:")
    ("http://www.ifomis.org/bfo/1.1/span#" "span:")
    ("http://www.ifomis.org/bfo/1.1/ro#" "ro:")
    ("http://www.ifomis.org/bfo/1.1/snap#" "snap:")
    ("http://www.ifomis.org/bfo/1.1#" "bfo:")
    ("http://www.ifomis.org/obo/ro/1.0/light#" "bforo:")
    ("http://protege.stanford.edu/plugins/owl/protege#" "protegeowl:")
    ("http://www.loa-cnr.it/ontologies/IRE/IRE.owl#" "ire:")
    ("http://www.loa-cnr.it/ontologies/ExtendedDnS.owl#" "edns:")
    ("http://www.loa-cnr.it/ontologies/DOLCE-Lite.owl#" "dolce:")
    ("http://www.loa-cnr.it/ontologies/OD/OntologyDesign.owl#" "od:")
    ("http://www.loa-cnr.it/ontologies/InformationObjects.owl#" "infob:")
    ;; ("http://sw.neurocommons.org/2007/annotations#" "sc:")
;    ("http://www.geneontology.org/formats/oboInOwl#" "obo:")
    ("http://www.geneontology.org/formats/oboInOwl#" "oboinowl:")
    ("http://www.obofoundry.org/ro/ro.owl#" "oborel:")
    ("http://purl.org/obo/owl/PATO#PATO_" "pato:")
    ("http://purl.org/obo/owl/CL#CL_" "cl:")
    ("http://purl.obolibrary.org/obo/PRO_" "pro:")
    ("http://purl.org/obo/owl/UO#UO_" "unit:")
    ("http://purl.org/obo/owl/SO#SO_" "so:")
    ("http://purl.org/obo/owl/CL#CL_" "cell:")
    ("http://purl.org/obo/owl/CARO#CARO_" "caro:")
    ("http://purl.org/obo/owl/CHEBI#CHEBI_" "chebi:")
    ("http://purl.org/obo/owl/ECO#ECO_" "evidence:")
    ("http://www.lsrn.org/lsrn/schema.owl#" "lsrnschema:")
    ("http://www.ifomis.org/biotop/1.0#" "biotop:")
    ("http://swan.mindinformatics.org/ontology/1.0/20070313/collections.owl#" "collections:")
    ("http://swan.mindinformatics.org/ontology/1.0/20070410/core.owl#" "swan:")
    ("http://purl.org/science/owl/sciencecommons/" "scdef:")
    ("http://purl.org/science/owl/thesaurus/" "scto:")
    ("http://purl.org/science/locusthesaurus/" "sclt:")
    ("http://purl.org/science/" "science:")
    ("http://purl.org/commons/record/" "record:")
    ("http://purl.org/commons/record/mesh/" "mesh:")
    ("http://usefulinc.com/ns/doap#" "doap:")
    ("http://ccdb.ucsd.edu/SAO/1.1#" "ccdb:")
    ("http://purl.org/nbirn/birnlex/ontology/annotation/BIRNLex_annotation_properties.owl#" "birn-annot:")
    ("http://purl.org/nbirn/birnlex/ontology/annotation/OBO_annotation_properties.owl#" "obo-annot:")
    ("http://purl.org/nbirn/birnlex/ontology/BIRNLex-OrganismalTaxonomy.owl#" "birn-org:")
    ("http://purl.org/obo/owl/NCBITaxon#NCBITaxon_" "taxon:")
    ("http://www.myexperiment.org/rdf/ontology#" "myexp:")
    ("http://purl.org/obo/owl/OBO_REL#" "roproposed:")
    (,*blankprefix* "blank:")
    ))


(defparameter *namespace-regexes* (make-hash-table :test #'equal))

(defparameter *qnameable-pattern-according-to-spec* nil)
(defparameter *absowl-qnameable-pattern* nil)

(defun maybe-abbreviate-namespace (s &optional for-external-parsing)
  (declare (optimize (speed 3) (safety 0)))
  (unless *use-qnames* (return-from maybe-abbreviate-namespace s))
  (unless *absowl-qnameable-pattern* 
    (setq *absowl-qnameable-pattern*  (#"compile" 'util.regex.pattern "^[a-zA-Z_][a-zA-Z_0-9]*$")))
  (unless *qnameable-pattern-according-to-spec*
    (setq *qnameable-pattern-according-to-spec*
	   (#"compile" 'util.regex.pattern "^[a-zA-Z_][a-zA-Z_.0-9-]*$")))
  (with-constant-signature ((matches "matches") 
			    (matcher "matcher" t)
			    (substring "substring" t)
			    (concat "concat"))
    (and (stringp s)
	 (loop for entry in *namespace-replacements*
	    for (prefix replacement ) = entry
	    for url-pattern = (gethash prefix *namespace-regexes*)
	    unless url-pattern
	    do (setf (gethash prefix *namespace-regexes*)
		     (setq url-pattern 
			   (#"compile" 'util.regex.pattern (format nil "~a.*" prefix))))
	    when (matches (matcher url-pattern s))
	    do (return-from maybe-abbreviate-namespace 
		 (let ((name-part (substring s (length prefix))))
		   (cond ((and (eq for-external-parsing :absowl)
			       (matches (matcher *absowl-qnameable-pattern* name-part)))
			  (values (concat replacement name-part) replacement))
			 ((and (not (eq for-external-parsing :absowl))
			       (matches (matcher *qnameable-pattern-according-to-spec* name-part)))
			  (values (concat replacement name-part) replacement))
			 (for-external-parsing s)
			 (t (values (concat replacement name-part) replacement)))))))
    s))


(defun unabbreviate-namespace (s)
  (let ((unabbreviated (maybe-unabbreviate-namespace s)))
    (when (eq s unabbreviated)
      (error "Don't know the namespace in ~a" s))
    unabbreviated))

(defun maybe-unabbreviate-namespace (s)
  (declare (optimize (speed 3) (safety 0)))
  (with-constant-signature ((matches "matches") 
			    (matcher "matcher" t)
			    (substring "substring" t)
			    (concat "concat"))
    (and (stringp s)
	 (loop for entry in *namespace-replacements*
	    for (prefix replacement) = entry
	    for pattern = (gethash replacement *namespace-regexes*)
	    unless pattern 
	    do (setf (gethash replacement *namespace-regexes*)
		     (setq pattern 
			   (#"compile" 'util.regex.pattern  (format nil "(?i)~a.*" replacement))))
	    when (matches (matcher pattern s))
	    do (return-from maybe-unabbreviate-namespace 
		 (concat prefix
		  (substring s (length replacement)))))))
    s)

; *namespace-replacements* has the form (("http://long/form/" "short:") ...)

(defun register-namespace (abbreviation expanded &optional replace)
  (let ((existing (find-if (lambda(el) (equal (second el) abbreviation)) *namespace-replacements*)))
      (if existing
	  (when (not (equal expanded (car existing)))
	    (if replace
		(progn
		  (setq *namespace-replacements* 
			(remove-if (lambda(el) (equal (second el) abbreviation)) *namespace-replacements*))
		  (push (list expanded abbreviation) *namespace-replacements*))
		(error "~a is already an abbreviation for ~a"
		       abbreviation
		       (car existing))))
	  (push (list expanded abbreviation) *namespace-replacements*))))

;; not ready
(defmacro with-default-namespace (base &body body)
  `(with-default-namespace-1 ,base (lambda() ,@body)))

(defun with-default-namespace-1 (base continue)
  (let ((*default-uri-base* base)
	(*namespace-replacements* (cons (list base ":") *namespace-replacements*)))
    (multiple-value-prog1
	(funcall continue)
      (decache-uri-abbreviated))))


