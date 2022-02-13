(in-package :cl-user)


;; Get a map from IRI to the preferred-label, where the preferred label is that
;; of the first property in the label-properties list, and if there are more
;; than one, the one with the specified language.
;; IRIs with no label do not appear in the table

(defun get-preferred-labels (ont &key (label-properties (list !rdfs:label)) (language "en"))
  ;; Fix label properties if passed as '(!rdfs:label ..)
  (setq label-properties (eval-uri-reader-macro label-properties)) 

  ;; query table will be map of IRI to all the label choices
  (let ((query-table (make-hash-table)))
    (loop for (iri label prop lang)
	    in
	    (sparql `(:select (?term ?label ?prop ?lang)
			 ()
		       ;; There will be one set of clauses for each property 
		       (:union 
			,@(loop for prop in label-properties
				collect
				`(;; in the results, ?prop is the property we are looking at
				  (:bind ,prop as ?prop)
				  (:union
				   ;; There's a label with a language tag for this property
				   ((?term ,prop ?label)
				    (:filter (equal (lang ?label) ,language))
				    (:bind ,language as ?lang))
				   ;; If there's no language tagged label the ?lang is "none"
				   ((?term ,prop ?label)
				    (:filter (not (lang ?label)))
				    (:bind "none" as ?lang))))))
		       ;; Check if deprecated
		       (:optional (?term !owl:deprecated ?dep))
		       ;; 
		       (:filter (and 
				 ;; No blank subjects
				 (not (isblank ?term))
				 ;; Don't include deprecated terms
				 (not (bound ?dep))))
		       )
		    :reasoner :none :kb ont )
	  do
	     (push  (list label prop lang) (gethash iri query-table)))

    ;; Ok now have to choose the preferred label
    (let ((results-table (make-hash-table)))
      (maphash (lambda(iri labels)
		 (let ((sorted (sort labels
				     ;; predicate sorts first by position of property in property list, then
				     ;; preferring language tags over not
				     (lambda(a b)
				       (destructuring-bind (prop1 lang1) (cdr a)
					 (destructuring-bind (prop2 lang2) (cdr b)
					   (or  (and (not (equal lang1 "none")) (equal lang2 "none")
						     (= (position prop1 label-properties) (position prop2 label-properties)))
					        (< (position prop1 label-properties) (position prop2 label-properties)))))))))
		   (setf (gethash iri results-table) (first (first sorted)))))
	       query-table)
      results-table)))

#|

Test

(with-ontology f ()
  ((asq (declaration (annotation-property !a))
	(declaration (annotation-property !b))
	(annotation-assertion !rdfs:label !a "rdfs-a-no-lang")
	(annotation-assertion !rdfs:label !a "rdfs-a-en@en")
	(annotation-assertion !skos:prefLabel !a "skos-a-no-lang")
	(annotation-assertion !skos:prefLabel !a "skos-a-en@en")

	(declaration (annotation-property !b))
	(annotation-assertion !rdfs:label !b "rdfs-b-no-lang")
	(annotation-assertion !rdfs:label !b "rdfs-b-en@en")
	(annotation-assertion !skos:prefLabel !b "skos-b-no-lang")
	(annotation-assertion !skos:prefLabel !b "skos-b-en@en")
	(annotation-assertion !skos:prefLabel !b "skos-b-fr@fr")
	))
  (let ((map (get-preferred-labels f :label-properties '(!rdfs:label !skos:prefLabel) )))
    (assert (equal (gethash !a map) "rdfs-a-en") () "Oops wrong answer")
    (assert (equal (gethash !b map) "rdfs-b-en") () "Oops wrong answer"))
  (let ((map (get-preferred-labels f :label-properties '(!skos:prefLabel !rdfs:label) :language "fr")))
    (setq @ map)
    (assert (equal (gethash !a map) "skos-a-no-lang") () "Oops wrong answer")
    (assert (equal (gethash !b map) "skos-b-fr") () "Oops wrong answer")))

|#


