(defun domains (property  &optional (ont *default-kb*) (direct +true+))
  (let ((reasoner (v3kb-reasoner ont))
	(expression (to-owlapi-object-property-expression property (v3kb-datafactory ont))))
    (mapcar 'make-uri (mapcar #"toString" (mapcar #"getIRI"
						  (mapcan 'set-to-list
							  (mapcar #"getEntities"
								  (set-to-list (#"getObjectPropertyDomains" reasoner expression direct)))))))))

(defun ranges (property &optional (ont *default-kb*) (direct +true+))
  (let ((reasoner (v3kb-reasoner ont))
	(expression (to-owlapi-object-property-expression property (v3kb-datafactory ont))))
    (mapcar 'make-uri
	    (mapcar #"toString"
		    (mapcar #"getIRI"
			    (mapcan 'set-to-list
				    (mapcar #"getEntities"
					    (set-to-list
					     (#"getObjectPropertyRanges" reasoner expression direct)))))))))


;; Check if a property can relate instances of domain and range
;; if explicitish is t then accept if one of the named subclasses of the domain/range is a subclass of the target domain/range
;; if explicitish is nil then accept if intersection of target domain/range and prop domain/range is satisfiable
;; if ignore-owl-thing is t then domains/ranges that are owl:Thing are ignored under the assumption that
;;   the relation domain/range wasn't explicitly stated
(defun property-applicable-to-domain-range (prop domain range &key (kb *default-kb*) (ignore-owl-thing t) (explicitish t))
  (let ((domains (domains prop kb))
	(ranges (ranges prop kb)))
    (when ignore-owl-thing 
      (setq domains (remove !owl:Thing domains))
      (setq ranges  (remove !owl:Thing ranges)))
    (when (and domains ranges)
      (if explicitish
	  (let ((domain-descendants (descendants `(object-intersection-of ,@domains) kb)))
	    (and 
	     (some (lambda(e) (is-subclass-of? e domain kb)) domain-descendants)
	     (let ((range-descendants (descendants `(object-intersection-of ,@ranges) kb)))
	       (some (lambda(e) (is-subclass-of? e range kb)) range-descendants))))
      	  (and (satisfiable? `(object-intersection-of ,domain (object-intersection-of ,@domains)))
	       (satisfiable? `(object-intersection-of ,range (object-intersection-of ,@ranges))))))))

#|
e.g.
(let ((*default-kb* (load-ontology "~/repos/obo-relations/ro.owl" :reasoner :hermit)))
  ;; takes a long time to classify
  (loop for prop in (remove-if 'consp (property-descendants !owl:topObjectProperty))
	when (property-applicable-to-domain-range prop !'process'@ro !'process'@ro)
	  do (print (uri-label prop))))
|#	       
