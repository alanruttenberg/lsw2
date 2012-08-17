;; this is the key associated with the account "lsw". You might want to get your own key,
;; by creating an account on bioportal.

(defvar *bioportal-key* "6551953c-31bb-4189-91b5-f0733a251c61")

(defun ncbo-annotate (text apikey &key (format "XML"))
  (get-url "http://rest.bioontology.org/obs/annotator" 
	   :post (append 
		  (list 
		   (list "apikey" apikey)
		   (list "textToAnnotate" text))
		  (when format (list (list "format" format))))))



(defun test-ncbo-annotator ()
  (let ((sample "This protocol will evaluate patients with systemic lupus erythematosus (SLE) and their relatives")) 
   (xmls::parse (ncbo-annotate sample *bioportal-key* :format "XML"))))



(defun get-ontology-by-ncbi-local-id (id &key (cachedir "./cache"))
  (let ((fname (format nil "~A/~A.owl" cachedir id)))
    (unless (probe-file "lsw:ncbo-cache;") (
    
;(map 'list (lambda(e) (third (find-element-with-tag e "localOntologyId")))  (find-elements-with-tag r "ontologyUsedBean"))