(in-package :cl-user)

;; Allow lisp sparql queries to have clauses that are manchester expressions.
;; e.g `(:select (?foo) () (:some !ex:has-part (:and ?clone (:some !ex:has-part ?part))))

;; What we're doing:
;; Translate sparql variables into urns
;; Use t-collect on the ontology which we wrap the expression in
;; t-collect gives us back a list of triples
;; remove the last triple, which is the ontology declaration
;; translate the urns (and blanks) back so they are understood as sparql

(defvar *class-argument-predicates* `(,!rdf:type ,!rdfs:subClassOf))

(defun translate-sparql-twerp (clause stream)
  (manchester-to-bgp (manchester-expression clause) stream))

(defun translate-sparql-twerp-object (clause stream)
  (let ((manchester (manchester-expression (third clause))))
    (cond ((eq (second clause) !rdf:type)
	   (setq manchester `(class-assertion ,manchester ,(first clause))))
	  ((eq (second clause) !rdfs:subClassOf)
	   (setq manchester `(sub-class-of ,(first clause) ,manchester)))
	  (t (error "expected one of ~a" *class-argument-predicates*)))
    (manchester-to-bgp manchester stream)))

(defun manchester-to-bgp (manchester stream)
  (multiple-value-bind (transformed bindings) (substitute-uris-for-sparql-variables manchester)
    (let ((triples (t-collect `(ontology ,transformed) nil)))
      (assert (and (eq (second (car (last triples))) !rdf:type) (eq (third (car (last triples))) !owl:Ontology)) (triples) "Something's changed. Last triple should be ontology declaration!")
      (setq triples (butlast triples))
      (loop for triple in (unblank-uniri-vars triples)
	 do (emit-sparql-clause triple stream)))))

(defun substitute-uris-for-sparql-variables (clause)
  (labels ((sparql-variable-p (thing) (and (symbolp thing) (char= (char (string thing) 0) #\?)))
	   (doit (thing)
	      (cond ((sparql-variable-p thing)
		      (make-uri (format nil "urn:twerpish:~a" (string-downcase (subseq (string thing) 1)))))
		     ((atom thing)
		      thing)
		     (t (mapcar #'doit thing)))))
    (mapcar #'doit clause)))

(defun unblank-uniri-vars (thing)
  (labels ((sparql-variable (thing)
	     (let ((match (and (uri-p thing) (caar (all-matches (uri-full thing) "^urn:twerpish:(.*)" 1)))))
	       (and match
		    (intern (concatenate 'string "?" (string-upcase match))))))
	   (blank (thing)
	     (let ((match (and (uri-p thing) (caar (all-matches (uri-full thing) "^urn:blank:(.*)" 1)))))
	       (and match
		    (intern (concatenate 'string "_B" match) 'keyword))))
	   (doit (thing)
	     (if (uri-p thing)
		 (or (sparql-variable thing) (blank thing) thing)
		 (cond ((atom thing)
			thing)
		       (t (mapcar #'doit thing))))))
    (mapcar #'doit thing)))
