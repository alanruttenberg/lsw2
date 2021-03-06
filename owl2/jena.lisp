(in-package :cl-user)

(defun create-empty-jena-model ()
  (let ((model (#"createDefaultModel" 'modelfactory)))
    (#"setNsPrefix" model "owl" (uri-full !owl:))
    (#"setNsPrefix" model "xsd" (uri-full !xsd:))
    (#"setNsPrefix" model "rdfs" (uri-full !rdfs:))
    (#"setNsPrefix" model "rdf" (uri-full !rdf:))
    (#"setNsPrefix" model "obo" "http://purl.obolibrary.org/obo/")
    model))

(defun make-jena-kb (file-or-model)
  "Make a kb just good enough to sparql against. Don't expect any reasoning."
  (let ((kb (make-v3kb)))
    (let ((model (if (java-object-p file-or-model)
		     file-or-model
		     (#"loadModel" 'RDFDataMgr file-or-model))))
      (setf (v3kb-told-jena-model kb) model)
      (setf (v3kb-name kb) `(:jena ,file-or-model))
      kb)))

(defun read-jena-model (path)
  (#"loadModel" 'RDFDataMgr (namestring (truename path))))

(defun each-jena-statement (model fn)
  (let ((iterator (#"listStatements" model)))
    (loop while (#"hasNext" iterator)
	  do  (funcall fn (#"next" iterator)))))

(defun write-jena-model (model to &optional prefixes)
  (let ((jfile (new 'java.io.file (namestring (translate-logical-pathname to)))))
    (if (#"exists" jfile)
	(delete-file to))
    (#"createNewFile" jfile)
    (loop for (uri-string prefix-colon) in prefixes
	  for prefix = (#"replaceFirst" prefix-colon ":$" "")
	  do (#"setNsPrefix" model prefix uri-string))
    (let ((stream (new 'java.io.fileoutputstream jfile)))
      (#"write" stream (#"getBytes" (format nil "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%")))
      (#"write" model stream "RDF/XML-ABBREV"))))

(defun write-jena-model-turtle (model to &optional (prefixes *namespace-replacements*))
  (let ((w (new 'filewriter (namestring (translate-logical-pathname to)))))
    (loop for (uri-string prefix-colon) in prefixes
	  for prefix = (#"replaceFirst" prefix-colon ":$" "")
	  do (#"setNsPrefix" model prefix uri-string))
    (#"write" 'RDFDataMgr w model (get-java-field 'riot.rdfformat "TURTLE_BLOCKS"))))


(defun add-jena-triple (model s property value)
  (let ((subject 
	 (cond ((stringp s)
		(#"createResource" model s))
	       ((uri-p s)
		(#"createResource" model (uri-full s)))
	       ((java-object-p s) s)
	       (t (error "subject: ~a" s))))
	(property (cond ((stringp property)
			 (#"getProperty" model property))
			((uri-p property)
			 (#"getProperty" model (uri-full property)))
			((java-object-p property) property)
			(t (error "property: ~a" s))))
	(value (cond ((and (consp value)
			   (eq (car value) :literal))
		      (make-jena-literal model (second value) (uri-full (third value)))
		      )
		     ((and (stringp value)
			   (#"matches" value ".*@[a-zA-Z]{2,}[a-zA-Z-]*")
			   (make-jena-literal model value !rdf:text)))
		     ((uri-p value)
		      (#"createResource" model (uri-full value)))
		     ((integerp value)
		      (#"createTypedLiteral" model value))
		     ((floatp value)
		      (#"createTypedLiteral" model value))
		     ((java-object-p value) value)
		     (t value))))
    (#"add" model subject property value)
    ))

(defun make-jena-literal (model value type)
  (if (equal type !rdf:text)
      (apply #"createLiteral" model (car (all-matches value "(.*)@(.*)" 1 2)))
      (#"createTypedLiteral" model (if (stringp value) value (prin1-to-string value))
			     (new 'jena.datatypes.basedatatype (if (uri-p type) (uri-full type) type)))))

(defun fresh-jena-blank (model)
  (#"createResource" model))


#|
older
(defun add-jena-triple (model s property value)
  (let ((subject 
	 (cond ((stringp s)
		(#"createResource" model s))
	       ((uri-p s)
		(#"createResource" model (uri-full s)))
	       ((java-object-p s) s)
	       (t (error "subject: ~a" s))))
	(property (cond ((stringp property)
			 (#"getProperty" model property))
			((java-object-p s) s)
			((uri-p property)
			 (#"getProperty" model (uri-full property)))
			(t (error "property: ~a" s))))
	(value (cond ((and (consp value)
			   (eq (car value) :literal))
		      (make-jena-literal model (second value) (uri-full (third value)))
		      )
		     ((and (stringp value)
			   (#"matches" value ".*@[a-zA-Z]{2,}[a-zA-Z-]*")
			   (make-jena-literal model value !rdf:text)))
		     ((uri-p value)
		      (#"createResource" model (uri-full value)))
		     ((integerp value)
		      (#"createTypedLiteral" model value))
		     ((floatp value)
		      (#"createTypedLiteral" model value))
		     (t value))))
    (#"addProperty" subject property value)))

(defun make-jena-literal (model value type)
  (if (member type (load-time-value (list (uri-full !rdf:text) !rdf:text)) :test 'equal)
      (apply #"createLiteral" model (car (all-matches value "(.*)@(.*)" 1 2)))
      (#"createTypedLiteral" model (if (stringp value) value (prin1-to-string value)) (new 'jena.datatypes.basedatatype (if (uri-p type) (uri-full type) type)))))
|#
