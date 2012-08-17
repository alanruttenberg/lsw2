(defvar *bioportal-key* "")





(defun ncbo-annotate (text apikey &key format)
  (get-url "http://rest.bioontology.org/obs/annotator" 
	   :post (append 
		  (list 
		   (list "apikey" apikey)
		   (list "textToAnnotate" text))
		  (when format (list (list "format" format))))))

(setq sample "This protocol will evaluate patients with systemic lupus erythematosus (SLE) and their relatives")
(ncbo-annotate sample *bioportal-key*)
