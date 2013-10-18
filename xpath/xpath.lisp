(defpackage "XPATH"  (:use "COMMON-LISP") (:export "SET-NAMESPACE-URI" "QUERY" "ELEMENTS" "ATTRIBUTE-NAMED"))
(in-package :xpath)

(defun make-xpath-namespace-context ()
  (let ((wrapped (cl-user::new 'org.apache.xalan.extensions.ExtensionNamespaceContext))
	(interfaces (cl-user::jclass-all-interfaces 'org.apache.xalan.extensions.ExtensionNamespaceContext)))
    (jss::jdelegating-interface-implementation  
     (car interfaces)
     wrapped
     "getNamespaceURI"
     (lambda(prefix) (get-namespace-uri prefix))
     )))

(defparameter *xpath-namespace-context*
  (make-xpath-namespace-context))

(defvar *xpath* (let ((it (#"newXPath" (#"newInstance" 'javax.xml.xpath.XPathFactory))))
		  (#"setNamespaceContext" it *xpath-namespace-context*)
		  it))

(defvar *xpath-uri-namespace* (make-hash-table :test 'equal))

(defun get-namespace-uri (prefix)
  (gethash (#"toString" prefix) *xpath-uri-namespace*))

(defun set-namespace-uri (prefix uri)
  (setf (gethash prefix *xpath-uri-namespace*) uri))

(defparameter *cached-xpath-expressions* (make-hash-table :test 'equal))

(defun query (element path)
  (let ((expression (or (gethash path *cached-xpath-expressions*)
			(setf (gethash path *cached-xpath-expressions*) (#"compile" *xpath* path)))))
    (#"evaluate" expression element (cl-user::get-java-field 'XPathConstants "NODESET"))))

(defun elements (node &rest elements)
  (loop for element in elements
       for found = (#"item" (query node element) 0)
       collect (and found (#"getTextContent" found))))

(defun attribute-named (node name)
  (#"getAttribute" node name))

#|
(let ((builder  (#"newDocumentBuilder" (#"newInstance" 'DocumentBuilderFactory))))
  (#"parse" builder (new 'InputSource (namestring (truename "~/neuro/convert/pdb/examples/1K5N.xml")))))
  
Document document = builder.parse(new File("/widgets.xml"));

XPath xpath = XPathFactory.newInstance().newXPath();

(setq xp (new 'domxpath "//descendant::PDBx:chem_comp"))
#<JAVA-OBJECT org.jaxen.dom.DOMXPath {88089A}>
CL-USER> (#"size" (#"selectNodes" xp doc))
({public abstract java.util.List org.jaxen.XPath.selectNodes(java.lang.Object) throws org.jaxen.JaxenException}
 {//descendant::PDBx:chem_comp}
 ({[#document: null]}))
({public abstract java.util.List org.jaxen.XPath.selectNodes(java.lang.Object) throws org.jaxen.JaxenException}
 {//descendant::PDBx:chem_comp}
 ({[#document: null]}))
; Evaluation aborted
CL-USER> (#"addNamespace" xp "PDBx" "http://pdbml.pdb.org/schema/pdbx-v32.xsd")
NIL
CL-USER> (#"size" (#"selectNodes" xp doc))
0
CL-USER> 

;; this doesn't work, but it should, so I give up
(setq doc
 (let ((factory (#"newInstance" 'DocumentBuilderFactory)))
   (#"setNamespaceAware" factory t)
   (let ((builder  (#"newDocumentBuilder" factory)))
     (#"parse" builder (new 'InputSource (namestring (truename "~/neuro/convert/pdb/examples/1K5N.xml")))))))

(defvar *xpath-uri-namespace* (make-hash-table :test 'equal))
(defvar *xpath-namespace-uri* (make-hash-table :test 'equal))

(defun get-namespace-uri (prefix)
  (gethash (#"toString" prefix) *xpath-uri-namespace*))
(defun get-namespace-prefix (namespace)
  (gethash (#"toString" namespace) *xpath-namespace-uri*))

(jnew-runtime-class 
 "lisp.HashtableNamespaceContext4" "java.lang.Object" '("javax.xml.namespace.NamespaceContext")
 '((nil (lambda(item) item)))
 '(("getNamespaceURI" "java.lang.String" ("java.lang.String") 
   (lambda(prefix item) (get-namespace-uri prefix))
   "public")
   ("getPrefix" "java.lang.String" ("java.lang.String") 
    (lambda(namespace item) (get-namespace-prefix namespace))
    "public"))
 nil)

(defparameter *xpath-namespace-context*
  (jnew (jconstructor "lisp.HashtableNamespaceContext4")))

(defparameter *xpath-namespace-context*
  (make-xpath-namespace-context))

(defun xpath-test (&optional (path "./*/PDBx:citationCategory/PDBx:citation"))
  (let* ((xpath (#"newXPath" (#"newInstance" 'javax.xml.xpath.XPathFactory)))
	 (context *xpath-namespace-context*))
    (setf (gethash "http://pdbml.pdb.org/schema/pdbx-v32.xsd" *xpath-namespace-uri*) "PDBx")
    (setf (gethash "PDBx" *xpath-uri-namespace*) "http://pdbml.pdb.org/schema/pdbx-v32.xsd")
    (jcall (find "setNamespaceContext" (#"getMethods" (jobject-class xpath)) :test 'search :key #"toString") xpath context)
    (let* ((expression (#"compile" xpath path))
	   (nodes (#"evaluate" expression doc (get-java-field 'XPathConstants "NODESET"))))
      nodes)))

(setq xp (new 'domxpath "//PDBx:chem_comp"))
#<JAVA-OBJECT org.jaxen.dom.DOMXPath {E12A65}>
CL-USER> (#"addNamespace" xp "PDBx" "http://pdbml.pdb.org/schema/pdbx-v32.xsd")
NIL
CL-USER> (#"size" (#"selectNodes" xp doc))
0
|#