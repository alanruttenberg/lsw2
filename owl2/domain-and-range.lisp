(defun domains (property ont &optional (direct +true+))
  (let ((reasoner (v3kb-reasoner ont))
	(expression (to-owlapi-object-property-expression property (v3kb-datafactory ont))))
    (mapcar 'make-uri (mapcar #"toString" (mapcar #"getURI"
    (mapcan 'set-to-list
	    (mapcar #"getEntities"
		    (set-to-list (#"getObjectPropertyDomains" reasoner expression direct)))))))))

(defun ranges (property ont &optional (direct +true+))
  (let ((reasoner (v3kb-reasoner ont))
	(expression (to-owlapi-object-property-expression property (v3kb-datafactory ont))))
    (mapcar 'make-uri
	    (mapcar #"toString"
		    (mapcar #"getURI"
			    (mapcan 'set-to-list
				    (mapcar #"getEntities"
					    (set-to-list
					     (#"getObjectPropertyRanges" reasoner expression direct)))))))))
