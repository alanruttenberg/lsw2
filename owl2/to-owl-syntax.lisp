;; New implementation of to-owl-syntax taking advantage of latest OWLAPI

(defparameter *owlapi-io-classes*
  '((:manchester ManchesterSyntaxStorerFactory ManchesterSyntaxDocumentFormat)
    (:functional FunctionalSyntaxStorerFactory FunctionalSyntaxDocumentFormat)
    (:xml OWLXMLStorerFactory OWLXMLDocumentFormat)
    (:latex LatexStorerFactory LatexDocumentFormat)
    (:rdfxml RDFXMLStorerFactory RDFXMLDocumentFormat)
    (:krss KRSSSyntaxStorerFactory KRSSDocumentFormat)
    (:turtle TurtleStorerFactory TurtleDocumentFormat)
    (:ntriples RioNTriplesStorerFactory NTriplesDocumentFormat)
    (:obo OBOFormatStorerFactory OBODocumentFormat) 
    (:dl DLSyntaxStorerFactory DLSyntaxDocumentFormat)
    (:html-dl DLSyntaxHTMLStorerFactory DLSyntaxHTMLDocumentFormat)
    (:json RioJsonStorerFactory RDFJSONDocumentFormat)
    (:jsonld RioJsonLDStorerFactory RDFJSONLDDocumentFormat)
    (:n3 RioN3StorerFactory N3DocumentFormat)
    (:nquads RioNQuadsStorerFactory NQuadsDocumentFormat)
    (:trig RioTrigStorerFactory TrigDocumentFormat)
    (:trix RioTrixStorerFactory TrixDocumentFormat )
    ))

(defun to-owl-syntax (ont syntax &optional dest)
  "Serialize an ontology in one of a variety of formats, most commonly
   have syntax arg be :rdfxml, :owlxml, :turtle. Full list in
   *owlapi-io-classes*.

   ont is either a v3kb ont or a java OWLOntology instance

   dest is either nil, in which case a string is returned, or is the
   path to where it should get written. If the file type given is .gz,
   .zip, or .xz then the file will be accordingly compressed"
  (if (v3kb-p ont) (apply-changes ont))
  (let ((ont (if (v3kb-p ont) (v3kb-ont ont) ont))
	(storer (#"createStorer" (new (second (assoc syntax *owlapi-io-classes*)))))
	(target (if (null dest) 
		    (new 'stringdocumenttarget)
		    (if (eq t dest)
			(new 'SystemOutDocumentTarget)
			(let ((file (new 'java.io.file (namestring (translate-logical-pathname dest))))
			      (type (pathname-type dest)))
			  (cond ((equal type "gz") (new 'GZipFileDocumentTarget file))
				((equal type "zip") (new 'ZipDocumentTarget file))
				((equal type "xz") (new 'XZFileDocumentTarget file))
				(t (new 'filedocumenttarget file)))))))
	(document-format (new (third (assoc syntax *owlapi-io-classes*)))))
    (#"storeOntology" storer ont target document-format)
    (if (null dest) 
	(#"toString" target)
	(if (eq t dest) 
	    (values)
	    (truename dest)))))



;; Formats   
;; owlapi.formats.BinaryRDFDocumentFormat
;; owlapi.formats.DLSyntaxDocumentFormat
;; owlapi.formats.DLSyntaxHTMLDocumentFormat
;; owlapi.formats.FunctionalSyntaxDocumentFormat
;; owlapi.formats.KRSS2DocumentFormat
;; owlapi.formats.KRSSDocumentFormat
;; owlapi.formats.LabelFunctionalDocumentFormat
;; owlapi.formats.LatexAxiomsListDocumentFormat
;; owlapi.formats.LatexDocumentFormat
;; owlapi.formats.ManchesterSyntaxDocumentFormat
;; owlapi.formats.N3DocumentFormat
;; owlapi.formats.NQuadsDocumentFormat
;; owlapi.formats.NTriplesDocumentFormat
;; owlapi.formats.OBODocumentFormat
;; owlapi.formats.OWLXMLDocumentFormat
;; owlapi.formats.RDFaDocumentFormat
;; owlapi.formats.RDFDocumentFormat
;; owlapi.formats.RDFJsonDocumentFormat
;; owlapi.formats.RDFJsonLDDocumentFormat
;; owlapi.formats.RDFXMLDocumentFormat
;; owlapi.formats.RioRDFDocumentFormat
;; owlapi.formats.RioRDFNonPrefixDocumentFormat
;; owlapi.formats.RioRDFPrefixDocumentFormat
;; owlapi.formats.RioRDFXMLDocumentFormat
;; owlapi.formats.RioTurtleDocumentFormat
;; owlapi.formats.TrigDocumentFormat
;; owlapi.formats.TrixDocumentFormat
;; owlapi.formats.TurtleDocumentFormat

;; Storers (seems these have to be paired with compatable format)

;; org.semanticweb.owlapi.dlsyntax.renderer.DLSyntaxHTMLStorerFactory
;; org.semanticweb.owlapi.dlsyntax.renderer.DLSyntaxStorerFactory
;; org.semanticweb.owlapi.formats.RioRDFStorerFactory
;; org.semanticweb.owlapi.functional.renderer.FunctionalSyntaxStorerFactory
;; org.semanticweb.owlapi.krss2.renderer.KRSS2OWLSyntaxStorerFactory
;; org.semanticweb.owlapi.krss2.renderer.KRSSSyntaxStorerFactory
;; org.semanticweb.owlapi.latex.renderer.LatexStorerFactory
;; org.semanticweb.owlapi.manchestersyntax.renderer.ManchesterSyntaxStorerFactory
;; org.semanticweb.owlapi.model.OWLStorerFactory
;; org.semanticweb.owlapi.oboformat.OBOFormatStorerFactory
;; org.semanticweb.owlapi.owlxml.renderer.OWLXMLStorerFactory
;; org.semanticweb.owlapi.rdf.rdfxml.renderer.RDFXMLStorerFactory
;; org.semanticweb.owlapi.rdf.turtle.renderer.TurtleStorerFactory
;; org.semanticweb.owlapi.rio.AbstractRioStorerFactory
;; org.semanticweb.owlapi.rio.RioBinaryRdfStorerFactory
;; org.semanticweb.owlapi.rio.RioJsonLDStorerFactory
;; org.semanticweb.owlapi.rio.RioJsonStorerFactory
;; org.semanticweb.owlapi.rio.RioN3StorerFactory
;; org.semanticweb.owlapi.rio.RioNQuadsStorerFactory
;; org.semanticweb.owlapi.rio.RioNTriplesStorerFactory
;; org.semanticweb.owlapi.rio.RioRDFXMLStorerFactory
;; org.semanticweb.owlapi.rio.RioTrigStorerFactory
;; org.semanticweb.owlapi.rio.RioTrixStorerFactory
;; org.semanticweb.owlapi.rio.RioTurtleStorerFactory
;; org.semanticweb.owlapi.util.OWLStorerFactoryImpl

;; Document target: Where its going to

;; org.semanticweb.owlapi.io.FileDocumentTarget
;; org.semanticweb.owlapi.io.GZipFileDocumentTarget
;; org.semanticweb.owlapi.io.GZipStreamDocumentTarget
;; org.semanticweb.owlapi.io.OWLOntologyDocumentTarget
;; org.semanticweb.owlapi.io.StreamDocumentTarget
;; org.semanticweb.owlapi.io.StringDocumentTarget
;; org.semanticweb.owlapi.io.SystemOutDocumentTarget
;; org.semanticweb.owlapi.io.WriterDocumentTarget
;; org.semanticweb.owlapi.io.XZFileDocumentTarget
;; org.semanticweb.owlapi.io.XZStreamDocumentTarget
;; org.semanticweb.owlapi.io.ZipDocumentTarget


