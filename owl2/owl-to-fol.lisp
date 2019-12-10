(in-package :logic)

;; References:
;; Using Vampire to Reason with OWL http://www.cs.man.ac.uk/~horrocks/Publications/download/2004/TRBH04a.pdf
;; Description Logic Handbook https://cdn.preterhuman.net/texts/science_and_technology/The%20Description%20Logic%20Handbook%20-%20Theory,%20Implementation%20and%20Applications%20(2003).pdf
;;  Chapter 2 Basic Description Logics 54-55
;;  Chapter 4 Relationships with other Formalisms, p 161-163
;; On the relative expressiveness of description logics and predicate logics https://core.ac.uk/download/pdf/82134362.pdf

(defvar *classinstancevar* )

;; If :rdf-type o-class-expression expands to (rdf-type c *classinstancevar*)
;; if :predicate expands to (c *classinstancevar*)

(defvar *owl-fol-class-handling* :rdf-type) 
  
(defun o-pred-property (head &rest args)
  (setq head (reduce-objectinversof head))
  (when (and (consp head) (eq (car head) 'cl-user::o-objectinverseof))
    (setq args (reverse args)))
  (apply 'pred-property head args))
    
(defun o-class-expression (expression)
  (if (atom expression)
      (ecase *owl-fol-class-handling*
	(:predicate (pred-class expression *classinstancevar*))
	(:rdf-type (o-pred-property 'rdf-type expression *classinstancevar*))) 
      (macroexpand expression)))


(defmacro o-subclassof (sub-expression super-expression)
  (with-logic-var *classinstancevar*
    (l-forall (list *classinstancevar*)
	      (l-implies (o-class-expression sub-expression) 
			 (o-class-expression super-expression)))))

(defmacro o-equivalentclasses (class-expression-1 class-expression-2 &rest more-class-expressions)
  (let ((equivalence
	  (with-logic-var *classinstancevar*
	    (l-forall (list *classinstancevar*)
		      (l-iff (o-class-expression class-expression-1)
			     (o-class-expression class-expression-2)
			     )))))
    (if more-class-expressions
	(l-and equivalence 
	       (macroexpand `(o-equivalent-classes ,class-expression-2 ,@more-class-expressions)))
	equivalence)))

(defmacro o-disjointclasses (&rest class-expressions)
  (apply 'l-and (loop for (c1 . rest) on class-expressions
		      append
		      (loop for c2 in rest
			    collect 
			    (with-logic-var *classinstancevar*
			      (l-forall (list *classinstancevar*)
					(l-not (l-and (o-class-expression c1)
						      (o-class-expression c2)))))))))
(defmacro o-disjointunion (class &rest class-expressions)
  (l-and (macroexpand `(o-equivalent-classes ,class (o-objectunionof ,@class-expressions)))
	 (macroexpand `(o-disjointclasses ,@class-expressions))))

(defmacro o-objectsomevaluesfrom (property-expression class-expression)
  (with-logic-var e
    (l-exists (list e) (l-and (o-pred-property property-expression *classinstancevar* e)
			      (let ((*classinstancevar* e))
				(o-class-expression class-expression))))))

(defmacro o-objectallvaluesfrom (property-expression class-expression)
  (with-logic-var e
    (l-forall (list e) (l-implies (o-pred-property property-expression *classinstancevar* e)
				  (let ((*classinstancevar* e))
				    (o-class-expression class-expression))))))

(defmacro o-objecthasself (property-expression)
  (o-pred-property property-expression *classinstancevar*  *classinstancevar*))

(defmacro o-objecthasvalue (property-expression individual)
  (o-pred-property property-expression *classinstancevar*  individual))

(defmacro o-functionalobjectproperty (property)
  (with-logic-var a
    (with-logic-var b
      (with-logic-var c
	(l-forall (list a b c)
		  (l-implies (l-and (o-pred-property property c a)
				    (o-pred-property property c b))
			     (l-= a b)))))))

(defmacro o-inversefunctionalobjectproperty (property)
  (with-logic-var a
    (with-logic-var b
      (with-logic-var c
	(l-forall (list a b c)
		  (l-implies (l-and (o-pred-property property a c)
				    (o-pred-property property b c))
			     (l-= a b)))))))

(defmacro o-transitiveobjectproperty (property)
  (with-logic-var a
    (with-logic-var b
      (with-logic-var c
	(l-forall (list a b c)
		  (l-implies (l-and (o-pred-property property a b)
				    (o-pred-property property b c))
			     (o-pred-property property a c)))))))

(defmacro o-reflexiveobjectproperty (property)
  (with-logic-var a
    (l-forall (list a)
	      (o-pred-property property a a))))

(defmacro o-irreflexiveobjectproperty (property)
  (with-logic-var a
    (l-forall (list a)
	      (l-not (o-pred-property property a a)))))

(defmacro o-symmetricobjectproperty (property)
  (with-logic-var a
    (with-logic-var b
      (l-forall (list a b)
		(l-iff (o-pred-property property a b) (o-pred-property property b a))))))

(defmacro o-asymmetricobjectproperty (property)
  (with-logic-var a
    (with-logic-var b
      (l-forall (list a b)
		(not (and (o-pred-property property a b) (o-pred-property property b a)))))))

(defmacro o-inverseobjectproperties (p1 p2)
  (with-logic-var a
    (with-logic-var b
      (l-forall (list a b)
		(l-iff (o-pred-property p1 a b) (o-pred-property p2 b a))))))

(defmacro o-equivalentobjectproperties (&rest properties)
  (with-logic-var a
    (with-logic-var b
      (l-forall (list a b)
		(apply 'l-and
		       (loop for (p1 p2) on properties while p2
			     collect (l-iff (o-pred-property p1 a b) (o-pred-property p2 a b))))))))

(defmacro o-objectpropertydomain (property class-expression)
  (with-logic-var *classinstancevar*
    (with-logic-var b
      (l-forall (list *classinstancevar* b)
		(l-implies (o-pred-property property *classinstancevar* b)
			   (o-class-expression class-expression))))))

(defmacro o-objectpropertyrange (property class-expression)
  (with-logic-var *classinstancevar*
    (with-logic-var b
      (l-forall (list *classinstancevar* b)
	       (l-implies (o-pred-property property b *classinstancevar*)
			(o-class-expression class-expression))))))

(defun reduce-objectinversof (prop)
  (cond ((atom prop) prop)
	((and (consp prop) (eq (car prop) 'o-objectinverseof) (atom (second prop))) prop)
	((and (consp prop) (not (eq (car prop) 'o-objectinverseof))) (error "malformed object property expression : ~a" prop))
	((and (consp prop) (consp (second prop)) (eq (car (second prop))  'o-objectinverseof))
	 (reduce-objectinversof (second (second prop))))
	(t (error "malformed object property expression : ~a" prop))))

(defun chain-fol-expression (target chain)
  (let ((target (reduce-objectinversof target)))
    (with-logic-vars (vars (1+ (length chain)))
      (l-forall vars (l-implies
		      (apply 'l-and (loop for (this next) on  vars 
					  for el in chain
					  if (and (consp el) 'o-objectinverseof)
					    collect (o-pred-property (second el) next this)
					  else collect (o-pred-property el this next)))
		      (if (consp target)
			  (funcall #'o-pred-property target (car (last vars)) (car vars))
			  (apply #'o-pred-property target (car vars) (last vars))))))))

(defmacro o-subobjectpropertyof (sub super)
  (if (and (consp sub) (eq (car sub) 'o-objectpropertychain))
      (chain-fol-expression super (cdr sub))
	(with-logic-var x
	  (with-logic-var y
	    (l-forall (list x y) (l-implies (o-pred-property sub x y)
					    (o-pred-property super x y)))))))

(defmacro o-disjointobjectproperties (&rest properties)
  (apply 'l-and (loop for (p1 . rest) on properties
		      append
		      (loop for p2 in rest
			    collect 
			    (with-logic-var x
			      (with-logic-var y
				(l-forall (list x y)
					  (l-not (l-and (o-pred-property p1 x y) (o-pred-property p2 x y))))))))))
						  
(defmacro o-objectoneof (&rest individuals)
  (apply 'l-or (loop for i in individuals collect (l-= i *classinstancevar*))))

(defmacro o-classassertion (class individual)
  (let ((*classinstancevar* individual))
    (o-class-expression class)))

(defmacro o-objectintersectionof  (&rest classes)
  (apply 'l-and (loop for c in classes collect (o-class-expression c))))

(defmacro o-objectunionof  (&rest classes)
  (apply 'l-or (loop for c in classes collect (o-class-expression c))))

(defmacro o-objectcomplementof  (class)
  (l-not (o-class-expression class)))

(defmacro o-objectpropertyassertion (prop x y)
  `(o-pred-property ,prop ,x ,y))

(defmacro o-negativeobjectpropertyassertion (prop x y)
  (l-not (o-objectpropertyassertion prop x y)))

(defmacro o-sameindividual (&rest individuals)
  (apply 'l-and
	 (loop for (a b) on individuals until (null b)
	       collect (l-= a b))))

(defmacro o-differentindividuals (&rest individuals)
  (apply 'l-and (loop for (a . rest) on individuals
		      append
		      (loop for b in rest 
			    collect (l-not (l-= a b))))))

(defmacro o-objectmincardinality (number property class)
  (with-logic-vars (is number)
    (apply 'l-and
	   (list*
	    (macroexpand `(o-differentindividuals ,@is))
	    (loop for i in is collect 
			      (l-and (let ((*classinstancevar* i)) (o-class-expression class))
				     (o-pred-property property *classinstancevar* i)) )))))

(defmacro o-objectexactcardinality (number property class)
  (with-logic-vars (is number)
    (apply 'l-and
	   (list*
	    (macroexpand `(o-differentindividuals ,@is))
	    (with-logic-var other
	     (l-forall (list other) 
		       (l-implies (o-pred-property property *classinstancevar* other)
				  (apply 'l-or
					 (loop for i in is collect (l-= i other)))))
	     )
	    (loop for i in is collect (l-and (let ((*classinstancevar* i)) (o-class-expression class)) (o-pred-property property *classinstancevar* i))))
	   )))

(defmacro o-objectmaxcardinality (number property class)
  (with-logic-vars (is number)
    (apply 'l-and
	   (list*
	    (with-logic-var other
	     (l-forall (list other) 
		       (l-implies (o-pred-property property *classinstancevar* other)
				  (apply 'l-or
					 (loop for i in is collect (l-= i other)))))
	     )
	    (loop for i in is collect (l-and (let ((*classinstancevar* i)) (o-class-expression class)) (o-pred-property property *classinstancevar* i))))
	   )))

;HasKey( CE ( OPE1 ... OPEm ) ( DPE1 ... DPEn ) )

;; This is too strong at the moment, as it
;; should only hold for a, b named individuals, but we aren't dealing
;; with declarations at the moment.
(defmacro o-haskey (class-expression object-properties data-properties)
  (with-logic-var a
    (with-logic-var b
      (with-logic-vars (vals (+ (length object-properties) (length data-properties)))
	(l-forall (list* a b vals)
		  (l-implies (apply 'l-and
				    (list* (pred-class class-expression a)
					   (pred-class class-expression b)
					   (loop for p in (append object-properties data-properties)
						 for v in vals
						collect (o-pred-property p a v)
						collect (o-pred-property p b v))))
			     (l-= a b)))))))

;; For now these are the same as their object equivalences. Todo: Ensure args are data  
(loop for head in '(dataallvaluesfrom 
		    dataallvaluesfrom 
		    datacomplementof 
		    dataexactcardinality 
		    datahasvalue 
		    dataintersectionof 
		    datamaxcardinality 
		    datamincardinality 
		    dataoneof 
		    datapropertyassertion 
		    datapropertydomain 
		    datapropertyrange 
		    datasomevaluesfrom 
		    dataunionof 
		    negativedatapropertyassertion
		    disjointdataproperties 
		    equivalentdataproperties 
		    functionaldataproperty )
      for d-name = (intern (concatenate 'string "O-" (string head)) (load-time-value *package*))
      for o-equivalent = (intern (concatenate 'string "O-" (#"replaceFirst" (string head) "DATA" "OBJECT" )) (load-time-value *package*))
      do (setf (macro-function d-name) (macro-function o-equivalent)))


(defun owl-sexp-to-fol (expression)
  (labels ((o-rewrite (expression)
	     (cond ((or (stringp expression) (symbolp expression))
		    (if (gethash (intern (string expression) 'cl-user) cl-user::*owl2-vocabulary-forms*) 
			(intern (concatenate 'string "O-" (string expression)) (load-time-value *package*))
			expression))
		   ((uri-p expression) expression)
		   ((numberp expression) expression)
		   (t (mapcar #'o-rewrite expression)))))
    (macroexpand-1 (o-rewrite (mapcar 'cl-user::rewrite-owl-canonical-functional expression)))))


#|
- haskey
- objectexactcardinality 
- differentindividuals
- objectmaxcardinality 
- objectmincardinality 
- sameindividual 
- asymmetricobjectproperty 
- classassertion 
- disjointunion 
- disjointclasses 
- disjointobjectproperties 
- equivalentclasses 
- equivalentobjectproperties 
- functionalobjectproperty 
- inversefunctionalobjectproperty 
- inverseobjectproperties 
- irreflexiveobjectproperty 
- negativeobjectpropertyassertion 
- objectallvaluesfrom 
- objectcomplementof 
- objecthasself 
- objecthasvalue 
- objectintersectionof 
- objectinverseof 
- objectoneof 
- objectpropertyassertion 
- objectpropertydomain 
- objectpropertyrange 
- objectsomevaluesfrom 
- objectunionof 
- reflexiveobjectproperty 
- subclassof 
- subobjectpropertyof 
- symmetricobjectproperty 
- transitiveobjectproperty 

annotationassertion 
annotationpropertydomain 
annotationpropertyrange 
declaration
subannotationpropertyof 
subdatapropertyof 

dataallvaluesfrom 
dataallvaluesfrom 
datacomplementof 
dataexactcardinality 
datahasvalue 
dataintersectionof 
datamaxcardinality 
datamincardinality 
dataoneof 
datapropertyassertion 
datapropertydomain 
datapropertyrange 
datasomevaluesfrom 
dataunionof 
negativedatapropertyassertion
disjointdataproperties 
equivalentdataproperties 
functionaldataproperty 

datatypedefinition 
datatyperestriction
|#


			



;; (macroexpand '(o-subclassof !a (o-objectsomevaluesfrom !p (o-objectsomevaluesfrom !r !b))))

;(subclass-of !c (object-all-values-from !p !b) )

;; (forall (?x) (implies (rdf-type !c ?x) (forall (?y) (implies (holds !p ?x ?y) (rdf-type !b ?y)) ))

;; (object-some-values-from !p !b) -> (exists (?y) (and (rdf-type !b ?y) (holds !p ?x ?y))), head: ?x

;; (forall (?head) (implies (!c ?head) expr))
