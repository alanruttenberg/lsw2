(defvar *unblanked* nil)
(defvar *collect-triples*)
(defvar *triple-collector*)
(defvar *jena-model* nil)

(defun t (input)
  (if (and (consp input) ; handle (again) the case of no ontology uri. yes, I know this is ugly.
	   (eq (car input) 'ontology)
	   (consp (second input))
	   (not (eq (car (second input)) :blank))
	   )
      (t `(ontology ,(fresh-blank) ,@(cdr input)))
      (let ((*collect-triples* nil)
	    (*pending-translation* nil))
	(cond ((atom input)
	       (cond ((uri-p input) input)
		     ((java-object-p input) input)
		     ((stringp input) input)
		     (t (error "can't translate ~a" input))))
	      ((and (consp input) (eq (car input) :literal)) input)
	      (t
	       (setq *pending-translation* input)
	       (prog1
		   (if (can-and-is-annotated input)
		       (multiple-value-bind (annotations form) (separate-annotations input)
			 (rule-based-translator form *rules*
						:action (lambda(result then) (setq *pending-translation* nil) (funcall then result)))
			 (tann annotations form)
			 )
		       (rule-based-translator input *rules*
					      :action (lambda(result then) (setq *pending-translation* nil)  (funcall then result)))
		       )
		 (when *pending-translation*
		     (warn "Couldn't translate ~a" *pending-translation*)))
		 )))))

(defun annotation-strategy (form)
  (let ((head (car (gethash  (car form) *owl2-vocabulary-forms*))))
    (cond ((member head
		   '(subobjectpropertyof subdatapropertyof SubClassOf ObjectPropertyDomain DataPropertyDomain ObjectPropertyRange DataPropertyRange InverseObjectProperties InverseDataProperties
		     FunctionalObjectProperty FunctionalDataProperty InverseFunctionalObjectProperty ReflexiveObjectProperty IrreflexiveObjectProperty SymmetricObjectProperty AsymmetricObjectProperty TransitiveObjectProperty  ClassAssertion ObjectPropertyAssertion DataPropertyAssertion Declaration AnnotationAssertion haskey disjointunion))
	   :first-triple)
	  ((and (member head
			'(DisjointClasses DisjointDataProperties DisjointDataProperties DifferentIndividuals))
		(= (length (cdr form)) 2))
	   :first-triple)
	  ((member head
		   '(DisjointClasses DisjointDataProperties DisjointDataProperties DifferentIndividuals Negativedatapropertyassertion Negativeobjectpropertyassertion))
	   :first-triple-subject)
	  ;; be careful not to do nested t triples for these. 
	  ((member head '(EquivalentClasses EquivalentObjectProperties EquivalentDataProperties SameIndividual))
	   :all-triples)
	  (t (error "Oops! Missed some annotation case: ~a~%" form)))))

(defun can-and-is-annotated (input)
  (and (consp input)
       (consp (second input))
       (eq (car (second input)) 'annotation)
       ;(names-axiom (car input))
       ))

(defun names-axiom (term)
  (member term 
	  (load-time-value
	     (cons 'annotation
		   (remove-duplicates
		    (loop for (input output head) in *rdf-mapping-table*
		       when (and (consp input) (null head))
		       collect (caar input)))))))

(defun separate-annotations (form)
  (loop with rest = (cdr form)
     if (and (consp (car rest)) (eq (caar rest) 'annotation))
     collect (pop rest) into annotations
     else do (return (values annotations (cons (car form) rest)))))

(defvar *nesting-annotation* nil)

(defun tann (annotations form)
  (labels ((annotation-annotations-on (subject)
	     (loop for annotation in annotations
		for (p v) = (subseq annotation (- (length annotation) 2))
		do (t `(annotationassertion ,@(subseq annotation 1 (- (length annotation) 2)) ,p ,subject ,v))))
	   (reify-annotate (triple)
	     (let ((blank (fresh-blank)))
	       (triple blank !rdf:type (if (eq (car form) 'AnnotationAssertion) !owl:Annotation !owl:Axiom))
	       (triple blank !owl:annotatedSource (first triple))
	       (triple blank !owl:annotatedProperty (second triple))
	       (triple blank !owl:annotatedTarget (third triple))
	       (annotation-annotations-on blank)
	       )))
    (ecase (annotation-strategy form)
      (:first-triple (reify-annotate (car *collect-triples*)))
      (:first-triple-subject (annotation-annotations-on (caar *collect-triples*)))
      (:all-triples (map nil #'reify-annotate *collect-triples*))
      )))

(defun t-print (input)
  (t   (unblank-individuals (eval-uri-reader-macro input))))

(defun t-collect (input)
  (let ((*triple-collector* nil)
	(*blankcounter* 0))
    (reverse (t (unblank-individuals (eval-uri-reader-macro input))))
    *triple-collector*))


(defun t-jena (input &rest prefixes)
  (let ((*jena-model* (#"createDefaultModel" 'com.hp.hpl.jena.rdf.model.ModelFactory)))
    (#"setNsPrefix" *jena-model* "owl" (uri-full !owl:))
    (#"setNsPrefix" *jena-model* "xsd" (uri-full !xsd:))
    (#"setNsPrefix" *jena-model* "rdfs" (uri-full !rdfs:))
    (#"setNsPrefix" *jena-model* "rdf" (uri-full !rdf:))
    (loop for (abbrev expansion) in prefixes do
	 (#"setNsPrefix" *jena-model* abbrev expansion))
    (t (unblank-individuals (eval-uri-reader-macro input)))
    *jena-model*))

(defun t-jena-serialize (input format &rest prefixes)
  (let ((model (apply 't-jena input prefixes)))
    (let ((sw (new 'StringWriter)))
      (#"write" model sw format)
      (values (#"toString" sw) model))))

(defun t-owlapi (input name)
  (let ((model (apply 't-jena input nil)))
    (let ((sw (new 'StringWriter)))
      (#"write" model sw "RDF/XML")
      (load-ontology sw :name name))))

  
(defun t-rdf (input &rest prefixes)
  (apply 't-jena-serialize input "RDF/XML-ABBREV" prefixes))

(defun t-turtle (input &rest prefixes)
  (apply 't-jena-serialize input "TURTLE" prefixes))


;;****************************************************************
;; these methods need to be conditioalized for each type of t-*

(defun fresh-blank ()
  (cond (*jena-model*
	 (fresh-jena-blank *jena-model*))
	(t !blank:)))

(defun make-literal (value type)
  (if *jena-model*
      (if (equal type !rdf:text)
	  (apply make-jena-langed-literal (car (all-matches value "(.*)@(.*)" 1 2)))
	  (make-jena-literal *jena-model* value type))
      (format nil "\"~a\"^^~a" value (uri-abbreviated type))))

(defun triple (a b c)
  (when (boundp '*collect-triples*)
      (push (list a b c) *collect-triples*))
  (cond (*jena-model*
	 (add-jena-triple *jena-model* a b c))
	((boundp '*triple-collector* )
	 (push (list a b c) *triple-collector*))
	(t (format t "~a ~a ~a .~%" a b c)))
  t) ; return t so no fail

;;****************************************************************

(defun fresh-jena-blank (model)
  (#"createResource" model))

(defun make-jena-literal (model value type)
  (if (member type (load-time-value (list (uri-full !rdf:text) !rdf:text)) :test 'equal)
      (apply #"createLiteral" model (car (all-matches value "(.*)@(.*)" 1 2)))
      (#"createTypedLiteral" model (if (stringp value) value (prin1-to-string value)) (new 'jena.datatypes.basedatatype (if (uri-p type) (uri-full type) type)))))

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


(defvar *bindings* nil)

(defmacro ? (var)
  `(cdr (assoc ',var *bindings*)))

(defmacro defrdfm (name (&key pattern head case) &rest triples)
  (defrdfm-1 name pattern head triples))

(defun defrdfm-1 (name pattern head triples)
  (let ((classification (classify-subscripting pattern triples)))
    (cond ((member classification '(:subscript-free :named-variable :sequence :foreach))
					;(assert (every 'atom pattern) () "Funny arglist: ~a" pattern)
	   (multiple-value-bind (pattern head triples bindings)
	       (case classification
		 ((:subscript-free :named-variable :sequence)
		  (rewrite-compound pattern head triples))
		 (:foreach 
		  (rewrite-foreach pattern head triples))
		 (t (return-from defrdfm-1 nil)))
	     `(add-rule ',pattern
			(lambda(bindings)
			    (let ((*bindings* bindings))
			      (let ,bindings
				,@triples
				,@(when head (list head)))))))))))

(defun rewrite-compound (pattern head triples)
  ;; replace subscript with normal variables
  (labels ((unelipse (thing &optional patternp)
	     (cond ((atom thing) thing)
		   ((consp thing)
		    (loop for (from dotdot to) on thing with skip = 0
		       if (and (and (consp from) (eq (car from) :subscript))
			       (and (consp to) (eq (car to) :subscript))
			       (eq dotdot :elipsis))
		       collect (if patternp `(?+ ,(intern (format nil "?MULTIPLE-~a" (second from)))) (intern (format nil "?MULTIPLE-~a" (second from)))) and do (setq skip 3)
		       else unless (plusp (decf skip)); (or (eq from :elipsis) (and (consp from) (eq (car from) :subscript)))
		       collect (unelipse from patternp))))))
    ;; replace simple elipses with single arg
    (let ((pattern (unelipse (eval-uri-reader-macro pattern) t))
	  (triples (unelipse (eval-uri-reader-macro triples))))
      (labels ((unsub (thing)

		 (cond ((atom thing) thing)
		       ((and (consp thing) (eq (car thing) :subscript))
			(intern (format nil "~a-~a" (second thing) (third thing))))
		       (t (mapcar #'unsub thing)))))
	(let ((pattern (unsub pattern))
	      (triples (unsub triples))
	      blanks)
	  ;; replace blanks with bound values
	  (labels ((unblank (thing)
		     (cond ((atom thing) thing)
			   ((and (consp thing) (eq (car thing) :blank) )
			    (or (cdr (assoc (second thing) blanks))
				(progn
				  (push (cons (second thing) (gensym (format nil "B-~a-" (second thing)))) blanks)
				  (cdar blanks))))
			   (t (mapcar #'unblank thing)))))
	    (labels ((var-rewrite (thing)
		       (cond ((atom thing)
			      (if (and (symbolp thing) (char= (char (symbol-name thing) 0) #\?))
				  `(? ,thing)
				  thing))
			     (t (mapcar #'var-rewrite thing)))))
	      (labels ((rewrite-seq (thing)
			 (cond ((atom thing) thing)
			       (t 
				(if (eq (car thing) 'seq)
				    `(list* 'seq ,(second thing))
				    (mapcar #'rewrite-seq thing))))))
		(values (unblank pattern) (var-rewrite (unblank head)) (rewrite-seq (var-rewrite (unblank triples)))
			(loop for (nil . blankvar) in blanks collect (list blankvar '(fresh-blank))))))))))))


(defun unblank-individuals (form)
  (let ((blanks nil))
    (labels ((unblank (thing)
	       (cond ((atom thing) thing)
		     ((and (consp thing) (eq (car thing) :blank))
		      (or (cdr (assoc (second thing) blanks))
			  (progn
			    (push (cons (second thing) (fresh-blank)) blanks)
			    (cdar blanks))))
		     (t (mapcar #'unblank thing)))))
      (unblank form))))
    

#|
((TRIPLE (T (:SUBSCRIPT ?INDIVIDUAL 1))
         !owl:sameAs
         (T (:SUBSCRIPT ?INDIVIDUAL 2)))
 :ELIPSIS
 (TRIPLE (T (:SUBSCRIPT ?INDIVIDUAL ?N-1))
         !owl:sameAs
         (T (:SUBSCRIPT ?INDIVIDUAL ?N))))
|#

;; find the elipses, remove the second one, transform to (loop for (individual1 individual2) on (? args) by 'cdr 
(defun rewrite-foreach (pattern head triples)
  (let ((triples 
	 (loop with more = triples until (null more)
	    if (eq :elipsis (car more)) do (setq more (cddr more))
	    else collect (pop more))))
    (let* ((subscripted-variable (car (subscripted-variables triples)))
	   (subscripted-variable1 (intern (format nil "~a-~a" (subseq (string subscripted-variable) 1) 1)))
	   (subscripted-variable2 (intern (format nil "~a-~a" (subseq (string subscripted-variable) 1) 2)))
	   (subscripted-variables (list subscripted-variable1 subscripted-variable2))
	   (multiple (intern (format nil "?MULTIPLE-~a" (string subscripted-variable))))
	   (triples (append
		    (remove-if 'subscripted-variables triples)
		    (list `(loop for ,subscripted-variables on ,multiple by #'cdr
			      when ,(second subscripted-variables)
				do ,@(remove-if-not 'subscripted-variables triples))))))
      (labels ((unsub (thing)
		 (cond ((atom thing) thing)
		       ((and (consp thing) (eq (car thing) :subscript))
			(intern (format nil "~a-~a" (subseq (string (second thing)) 1) (third thing))))
		       (t (mapcar #'unsub thing)))))
	(rewrite-compound pattern head (unsub triples))))))
      


(defmacro defrdfm-override (&whole whole name (&key pattern head case) &rest triples)
  (let ((usual-call (cons 'defrdfm (cdr whole))))
    `(let ((exists (assoc ',(second whole) *overridden-rdfm*)))
       (if exists
	   (setf (second exists) ',usual-call)
	   (push (list ',(second whole) ',usual-call) *overridden-rdfm*)))))

(defrdfm-override haskey
    (:pattern (haskey ?ce (?multiple-?object-property-expression) (?multiple-?data-property-expression))
	       :case :subscript-free)
  (triple (t ?class-expression) !owl:hasKey (t `(seq ,@?multiple-?object-property-expression ,@?multiple-?data-property-expression))))


(defrdfm-override datatyperestriction 
    (:pattern (datatyperestriction ?datatype (?+ ?multiple-?facet-value-pairs))
     :head (:blank ?x) :case :subscript-free)
  (triple (:blank ?x) !rdf:type !rdfs:Datatype)
  (triple (:blank ?x) !owl:onDatatype (t ?datatype))
  (loop for (facet value) on ?multiple-?facet-value-pairs by #'cddr
       with blanks
       for blank = (fresh-blank)
       do (triple blank facet value) (push blank blanks)
       finally (triple (:blank ?x) !owl:withRestrictions (t `(seq ,@(reverse blanks))))))

;; fixme: no ontology uri allowed - fixed elsewhere
(defrdfm-override ontology
    (:pattern
     (ontology ?ontologyiri (?? ?versionuri)
	       (?+ ?multiple-annotations/axioms))
     :case :subscript-free)
  (triple ?ontologyiri
	  !rdf:type
	  !owl:Ontology)
  (if ?versionuri
      (if (uri-p ?versionuri)
	  (triple ?ontologyiri
		  !owl:versionIRI
		  ?versionuri)
	  (push ?versionuri ?multiple-annotations/axioms)))
  (loop while (eq (car (car ?multiple-annotations/axioms))
		  'imports)
     do (triple ?ontologyiri
		!owl:imports
		(second (pop ?multiple-annotations/axioms))))
  (loop while (eq (car (car ?multiple-annotations/axioms))
		  'annotation)
     do
     ;;	      (annotation (annotation ..) p v) -> (annotation-assertion (annotation ...) p o v)
     (let ((head (butlast (first ?multiple-annotations/axioms) 2))
	   (tail (last (first ?multiple-annotations/axioms) 2)))
       (pop ?multiple-annotations/axioms)
       (t `(annotationassertion ,@(cdr head) ,(first tail) ,?ontologyiri ,(second tail)))))
  (loop while ?multiple-annotations/axioms
     do (t (pop ?multiple-annotations/axioms))))

; the "true" was being parsed as ?true

(defrdfm-override objecthasself 
    (:pattern (objecthasself ?object-property-expression) :head (:blank ?x) :case :subscript-free)
  (triple (:blank ?x) !rdf:type !owl:Restriction)
  (triple (:blank ?x) !owl:onProperty (t ?object-property-expression))
  (triple (:blank ?x) !owl:hasSelf (make-literal "true" !xsd:boolean)))

;; (defun canonicalize-blanks (triples)
;;   ;; assign blank ids in a predictable way to a set of triples
;;   ;; cases:
;;   ;; No blanks - ignore
;;   ;; One blank - lexicographic order on sp or po as appropriate with sp winning in case of a tie
;;   ;; Two blanks - before one blank. Lexicograpic order on p. **** Bug - not unique. e.g. blank1 subclassof blank2, blank2 subclassof blank3
;;   (flet ((lexical (e)
;; 	   (cond ((uri-p e)
;; 		  (uri-full e))
;; 		 ((stringp e)
;; 		  e)
;; 		 ((and (consp e) (eq (car e) :literal))
;; 		  (format nil "\"~a\"^^~a" (second e) (third e)))
;; 		 (t (error "something's wrong - don't know how to lexicalize ~a" e)))))
;;     (let ((to-sort
;; 	   (loop for triple in triples
;; 	      for count = (count-if (lambda(e) (and (uri-p e) (ignore-errors (uri-blank-p e)))) triple)
;; 	      append
;; 	      (cond ((= count 0) nil)
;; 		    ((= count 2) nil; (list (cons (cons "" (uri-full (second triple))) triple)))
;; 		    ((= count 1) (list (cons (cons (uri-full (second triple))
;; 						   (if (ignore-errors (uri-blank-p (first triple)))
;; 						       (lexical (third triple))
;; 						       (uri-full (first triple)))) triple)))
;; 		    ((= count 3) (error "something's wrong - three blanks"))))))
;;       (sort to-sort
;; 	    (lambda (a b) (if (and (string-equal (car a) (car b))
;; 				   (string-equal (cdr a) (cdr b)))
;; 			      a
;; 			      (if (string-lessp (car a) (car b))
;; 				  a
;; 				  (if (string-equal (car a) (car b))
;; 				      (string-lessp (cdr a) (cdr b))))))
;; 	    :key 'car)
;; 	    )))

;; (defun test-iso ()
;;   (let ((ont (parse-functional-syntax "Prefix(:=<http://example.org/test#>)
;; Prefix( xsd: = <http://www.w3.org/2001/XMLSchema#> )

;; Ontology(<http://example.org/test>
;;   Declaration(NamedIndividual(:a))
;;   Declaration(Class(:A))
;;   Declaration(DataProperty(:dp))

;;   SubClassOf(:A DataExactCardinality(2 :dp xsd:boolean))

;;   ClassAssertion(:a :A)
;; )

;; ")))
;;     (multiple-value-bind (string model1) (t-rdf ont)
;;       (multiple-value-bind (string model2) (t-rdf ont)
;; 	(#"isIsomorphicWith" model1 model2)))))

;; (defun test-iso-reading (string)
;;   (let ((m1 (#"createDefaultModel" 'modelfactory)))
;;     (let ((sw (new 'StringReader string)))
;;       (#"read" m1 sw "http://example.org/"))
;;     (let ((m2 (#"createDefaultModel" 'modelfactory)))
;;       (let ((sw (new 'StringReader string)))
;; 	(#"read" m2 sw "http://example.org/")
;; 	(#"isIsomorphicWith" m1 m2)))))
