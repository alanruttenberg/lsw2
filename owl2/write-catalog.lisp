(in-package :cl-user)

;; Create a catalog-v001.xml for a directory by reading each ontology,
;; while not having imports followed, getting the ontology and version
;; IRI (if available), and escaping the file name appropriately. Works with
;; symbolic links, so one can, for example, link in the common core ontology
;; directory, run this, and have it properly use the local ontologies.
;; 
;; This is mostly to benefit Protege as I think the load-ontology does
;; this right although it will use this catalog if present.
;;
;; (write-owl-catalog <directory>) writes catalog-v001.xml in directory 

;; Create a subclass of ontology load configuration, defining the method
;; "isIgnoredImport" to return true all the
;; time. *ignore-imports-load-configuration* is an instance of that class

(defvar *ignore-imports-load-configuration-class*
   (java:jnew-runtime-class
   "IgnoreImportsOWLLoadedConfiguration"
   :methods (list
             (list "isIgnoredImport" :boolean '("org.semanticweb.owlapi.model.IRI")
                   (lambda (this iri) (declare (ignore this iri)) t)
                   )
             )
   :superclass "org.semanticweb.owlapi.model.OWLOntologyLoaderConfiguration"))

(defparameter *ignore-imports-load-configuration*
  (new *ignore-imports-load-configuration-class*))

;; Return a list of lists of IRI and local path
(defun directory-ontology-iris-and-paths (directory)
  (flet ((local-escaped-path (path)
           ;; trim off the directory name yielding local path
           (replace-all (subseq path (length directory))
                        ;; match anything other than alphanumeric, underscore, dot, or slash
                        "([^a-zA-Z0-9_./])"
                        ;; replace with %-encoded character
                        (lambda(x) (format nil "%~2,'0x" (char-code (char x 0))))
                        1)))
    ;; suppress logging because since we're not reading imports, some
    ;; axioms won't have IRIs typed correctly.
    (with-owl-logging-suppressed
      ;; ont will be the pathnames (as strings) of all the ontologies, recursively.
      (loop for ont-path
              in 
            ;; look just for ontology extensions. Only the ones I care about.
              (loop for extension in '("rdf" "ttl" "owl" "ofn")
                    for onts = (directory (merge-pathnames (make-pathname :directory '(:relative :wild-inferiors) :name :wild :type extension ) directory))
                    for path = (mapcar 'namestring onts)
                    append path)
              ;; load ontology but don't process imports
            for ontology = (load-ontology ont-path :configuration *ignore-imports-load-configuration*)
            for ontology-iri = (get-ontology-iri ontology)
            for version-iri = (get-version-iri ontology) 
            ;; collect a pair of the ontology iri (and version iri if
            ;; exists) and the path relative to directory
            collect (list ontology-iri (local-escaped-path ont-path))
            when version-iri
              collect (list version-iri (local-escaped-path ont-path))
            ))))

;; Write out catalog-v001.xml with mappings for each of the iri/path pairs.
(defun write-owl-catalog (directory)
  (let ((catalog-file (namestring (merge-pathnames  "catalog-v001.xml" directory))))
    ;; If there's an existing catalog, check before deleting it. We don't
    ;; want it around because it might be incorrect or malformed and
    ;; we aren't processing imports anyways
    (if (probe-file catalog-file)
        (when (y-or-n-p "Delete existing ~s" catalog-file)
          (delete-file catalog-file))))
  (let ((paths (directory-ontology-iris-and-paths (namestring directory))))
    (with-open-file (c (merge-pathnames  "catalog-v001.xml" directory)
		       :if-exists :supersede :direction :output)
      ;; write header
      (write-line "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>" c)
      (write-line "<catalog prefer=\"public\" xmlns=\"urn:oasis:names:tc:entity:xmlns:xml:catalog\">" c)
      ;; write mapping entry 
      (loop for (iri path) in paths
	    do (format c "    <uri id=\"User Entered Import Resolution\" name=\"~a\" uri=\"~a\"/>~%" (uri-full iri) path))
      (write-line "</catalog>" c))))

;; example
;; (write-owl-catalog "/Users/alanr/repos/cubrc-nga/From-CUBRC/Space-Ontologies-For-ShareFile/")



;(map nil 'print (ontologies "/Users/alanr/repos/cubrc-nga/From-CUBRC/Space-Ontologies-For-ShareFile/"))

;(load-ontology "/Users/alanr/repos/cubrc-nga/From-CUBRC/Space-Ontologies-For-ShareFile/SpacecraftOntology_[Contains_CUI].ttl" :configuration (new iiolc))



; (#"isIgnoredImport"  *ignore-imports-load-configuration* (to-iri "http://example.com/"))



;(map nil 'print (ontologies "/Users/alanr/repos/cubrc-nga/From-CUBRC/Space-Ontologies-For-ShareFile/") )
