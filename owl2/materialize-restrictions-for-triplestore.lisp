;; for each p property
;; foreach p some c alone or in conjunction
;; collect c and c's ancestors
;; collect if new intern(p some c) subclassOf intern(p some c') 


;; Assumption already classified with intersections unrolled to direct existential
;; o classified

;; We're essentially adding a shadow abbreviated network with node sharing.
;; For each property and parents and class and parents assert the superclass relation.

;; we're not replicating every single class as target of an
;; existential, instead providing only for superclasses of classes
;; that are otherwise mentioned in one. For that reason we should
;; actually be doing this on the union of all the files that we are
;; going to have in the triple store.

;; One Consequence of this is that there are now blank nodes that are
;; subclasses of blank nodes and most queries don't expect that. Need
;; to filter out blank nodes with !isBlank()

(defun make-jena-kb (file)
  ;; makes a kb just good enough to sparql against. Don't expect anything more.
  (let ((kb (make-v3kb)))
    (let ((model (#"loadModel" 'RDFDataMgr file)))
      (setf (v3kb-told-jena-model kb) model)
      (setf (v3kb-name kb) '(:jena "/Users/lori/Desktop/test.rdf"))
      kb)))

;; we use just jena since we don't need a reasoner for this
(defun materialize (o file)
  (let ((classes (make-hash-table :test 'equalp)) ;; actually simple existential restrictions 
	(subclass-relations (make-hash-table :test 'equalp)) ;; either pairs of such or a named class and one
	(seen (make-hash-table :test 'equalp))) ;; no use doing unnecessary work.
    (flet ((parents (c o) ;; parents of a named class
	     (sparql `(:select (?c) () (,c !rdfs:subClassOf ?c)) :use-reasoner :none :kb o :flatten t))
	   (property-ancestors (p o) ;; all superproperties of a property
	     (sparql `(:select (?p) () (,p !rdfs:subPropertyOf* ?p)) :use-reasoner :none :kb o :flatten t)))
      (labels ((intern-class (c)
		 (or (gethash c classes) (setf (gethash c classes ) c))) ;; save one existential
	       (intern-subclass-relation (sub super) ;; save a subclass relation 
		 (let ((r (list sub super)))
		   (or (gethash r subclass-relations)
		       (setf (gethash r subclass-relations) r))))
	       (assert-parents (c p &optional x)
		 ;; this is the business end. First call is a named class x subclass of a restriction p some c
		 ;; we're going to create all the supers of the existential, both by property and by class
		 ;; Do one level of class parents then recurse on that (no x for those)
		 (unless (gethash (list c p x)  seen)
		   (setf (gethash (list c p x)  seen) t)
		   (loop for p-now in (cons p (property-ancestors p o))
			 do
			    ;; assert the first link from the named class 
			    (and x (intern-subclass-relation x (list p-now c))) 
			    ;; then go up the named parents
			    (loop for c-anc in (remove-if (lambda(e) (and (uri-p e) (search "urn:blank:" (uri-full e) :test 'char-equal))) (parents c o))
				  do
				     ;; and add the more general existential and the subclass relation between child and general
				     (intern-class (list p-now c-anc))
				     (intern-subclass-relation (list p-now c) (list p-now c-anc))
				     ;; and recurse
				     (assert-parents c-anc p))))))
	;; The top level drives - existentials that are asserted as superclasses of named classes
	(loop for (p c x) in
			  (sparql '(:select (?p ?c ?x) ()
				    (?x !original !original)
				    (?x !rdfs:subClassOf ?r)
				    (?r !owl:onProperty ?p)
				    (?r !owl:someValuesFrom ?c)
				    (?c !original !original)
				    (:filter (and (not (isblank ?x)) (not (isblank ?c)) (not (isblank ?p)))))
				  :use-reasoner :none :kb o)
	      do
		 (assert-parents c p x))
	;; now that we've collected all the information, write it out.
	;; because we've interned classes and subclass relations we're
	;; sure to not write out redundant links. That's hard to
	;; accomplish with the owlim rul engine.
	(let ((*jena-model* (#"createDefaultModel" 'hp.hpl.jena.rdf.model.ModelFactory)))
	  ;; *jena-model* is dynamic scope. The calls to (triple)
	  ;; below add the triples to it.
	  ;; TBD - consider adding some provenance/debugging info in the form of labels
	  (maphash (lambda(k v)
		     (declare (ignore k))
		     (if (atom (first v))
			 ;; special case of named class as subclass 
			 (let ((super (second v))
			       (superrestriction (fresh-jena-blank *jena-model*)))
			   (triple (first v) !rdfs:subClassOf superrestriction)
			   (triple superrestriction !owl:onProperty (first super))
			   (triple superrestriction !owl:someValuesFrom (second super)))
			 ;; now the case of existential subclass of existential
			 (let ((sub (first v))
			       (super (second v))
			       (subrestriction (fresh-jena-blank *jena-model*))
			       (superrestriction (fresh-jena-blank *jena-model*)))
			   (triple subrestriction !rdfs:subClassOf superrestriction)
			   (triple subrestriction !owl:onProperty (first sub))
			   (triple subrestriction !owl:someValuesFrom (second sub))
			   (triple superrestriction !owl:onProperty (first super))
			   (triple superrestriction !owl:someValuesFrom (second super)))))
		   subclass-relations)
	  (let ((w (new 'filewriter file)))
	    (#"write" *jena-model* w "RDF/XML")
	    ))
	))))


(defun compare-reasoner-triplestore (query reasoned endpoint &key verbose)
  (let ((from-sparql (sparql `(:select (?other) (:distinct t)  (?other !rdfs:subClassOf ,query) (:filter (not (isBlank ?other)))) :use-reasoner endpoint :flatten t))
	(from-reasoner (descendants query reasoned))
	(from-jena (sparql `(:select (?other) (:distinct t)  (?other !rdfs:subClassOf ,query) (:filter (not (isBlank ?other)))) :use-reasoner :none :kb reasoned :flatten t)))
    (when verbose
      (format t "from sparql: ~a. from reasoner: ~a. no reasoning: ~a~%" (length from-sparql) (length from-reasoner) (length from-jena)) )
    (let ((only-in-sparql (set-difference from-sparql from-reasoner))
	  (only-in-reasoner (set-difference from-reasoner from-sparql)))
      (or (and (null only-in-reasoner) (null only-in-sparql)) 
	  (values nil (length from-reasoner) only-in-reasoner (length only-in-sparql) only-in-sparql)))))


;good: (compare-reasoner-triplestore '(:some !obo:RO_0002212 !obo:GO_0006310) go !<http://127.0.0.1:8080/graphdb-workbench-ee/repositories/GOTEST2>)
;bad:  part of some intracellular membrane-bounded organelle. Reasoner 934 others:0 because partonomy isn't done.
; (compare-reasoner-triplestore '(:some !obo:BFO_0000050 !obo:GO_0043231) go !<http://127.0.0.1:8080/graphdb-workbench-ee/repositories/GOTEST2> :verbose t)
; reasoner 3 others 0
; (compare-reasoner-triplestore '(:some !obo:BFO_0000051 !obo:GO_0043231) go !<http://127.0.0.1:8080/graphdb-workbench-ee/repositories/GOTEST2> :verbose t)

;; simple debug case
;; (with-ontology foo ( )
;; 	       ((asq (declaration (class !a))
;; 		     (declaration (class !b))
;; 		     (declaration (class !c))
;; 		     (declaration (class !d))
;; 		     (declaration (object-property !p1))
;; 		     (subclass-of !b !a)
;; 		     (subclass-of !c (object-some-values-from !p1 !b))
;; 		     (subclass-of !c (object-intersection-of !d (object-some-values-from !p1 !b)))
;; 		     (declaration (named-individual !x))
;; 		     (declaration (named-individual !y))
;; 		     (class-assertion !c !x)))
;;   (to-owl-syntax foo :turtle "/Volumes/trips/pro/owl/test.ttl"))

(defun relonomy(ont &rest rels)
  (let ((rel-table (make-hash-table))
	(declared (make-hash-table)))
    (unless rels (setq rels (sparql '(:select (?prop)  () 
				      (?prop a !owl:ObjectProperty))
				    :kb ont :flatten t :use-reasoner :none)))
    (format *debug-io* "Rels: ~{~a~^, ~}~%" rels)
    (format *debug-io* "Constructing relonomy...")
    (with-ontology relonomy () 
		     ((macrolet ((maybe-declare (term type &optional original)
				   `(unless (gethash ,term declared)
				      (setf (gethash ,term declared) t)
				      (as `(declaration (,,type ,,term)))
				      (if ,original 
					  (as `(annotation-assertion !original ,,term !original))))))
			(asq (imports !<file:///Users/alanr/repos/lsw2/go.owl>))
			(asq (declaration (annotation-property !original)))
			;; if we wanted to trim it would be here.
			;; depth first
			(loop for class in (sparql '(:select (?class) 
						     (:distinct t)
						     (?class !rdf:type !owl:Class)
						     (:filter (not (Isblank ?class))))
						   :kb ont :use-reasoner :none :flatten t )
			      for accession = (#"replaceFirst" (uri-full class) ".*/" "")
			      do
				 (loop for rel in rels
				       for relaccession = (#"replaceFirst" (uri-full class) ".*/" "")
				       for reluri = (make-uri (concatenate 'string "http://example.com/" relaccession "_" accession))
				       do
					  (maybe-declare rel 'object-property)
					  (maybe-declare class 'class t)
					  (as `(declaration (class ,reluri)))
					  (as `(sub-class-of ,reluri (object-some-values-from ,rel ,class)))))))
	(format *debug-io* "constructed relonomy. reasoning...")
	(check-ontology relonomy :classify t :reasoner :elk)
	(format *debug-io* "done.")
	relonomy)))


      ;; (sparql '(:select (?sub ?super) 
      ;; 		(:distinct t)
      ;; 		(?class1 !rdfs:subClassOf ?super) 
      ;; 		(:filter (and (not (isblank ?sub)) (not (isblank ?super)))))
      ;; 	      :kb ont :use-reasoner :none :flatten t )
      ;; relonomy)

;; relonomy will have all the superclass the the original would have and more.
;; We procede, then, by adding superclass assertions as in materialize
:; but: if there is a parts uri equivalent then use that class rather than the anonymous one.
;; at the end 

;; When materializing you need to add superclasses in the rel direction.
;; really we don't need any classes for rel that have no subs that are quoted in the original.
;; Unfortunately We don't know this beforehand
