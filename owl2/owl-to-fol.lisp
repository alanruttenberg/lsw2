(defvar *classinstancevar* )

(defparameter *logic-symbols* "PQRSUVWXYZABCDEFGHIJKLMNOT")

(defmacro with-logic-vars ((vars n &optional (from *logic-symbols* from-supplied-p)) &body body)
  `(let ((,vars (loop for i below  ,n
		      for base = (elt ,(if from-supplied-p 'from '*logic-symbols*) i)
		      collect (intern (concatenate 'string "?" (string base))
				      (if (symbolp base) (symbol-package base) *package*))
		      )))
     (let ((*logic-symbols* (if ,from-supplied-p *logic-symbols* (subseq *logic-symbols* ,n))))
       ,@body)))

(defmacro with-logic-var (var &body body)
  (let ((vars (gensym)))
    `(with-logic-vars (,vars 1)
       (let ((,var (car ,vars)))
	 ,@body))))

(defvar *use-holds-for-properties* nil)
(defvar *use-rdf-type-for-class* nil)

(defun pred-property (head &rest args)
  (setq head (reduce-objectinversof head))
  (when (and (consp head) (eq (car head) 'o-objectinverseof))
    (setq args (reverse args))
    (setq head (second head)))
  (if *use-holds-for-properties*
      `(holds , head ,@args)
      `(,head ,@args)))

(defun pred-class (class arg)
  (if *use-rdf-type-for-class* 
      `(rdf-type ,class ,arg)
      (list class arg)))

(defun l-forall (vars &rest expressions)
  `(:forall ,vars ,@expressions))

(defun l-exists (vars &rest expressions)
  `(:exists ,vars ,@expressions))

(defun l-implies (antecedent consequent)
  `(:implies ,antecedent ,consequent))

(defun l-and (&rest expressions)
  `(:and ,@expressions))

(defun l-or (&rest expressions)
  `(:or ,@expressions))

(defun l-iff (antecedent consequent)
  `(:iff ,antecedent ,consequent))

(defun l-not (expression)
  `(:not ,expression))

(defun l-equal (a b)
  `(:= ,a ,b))

(defun o-class-expression (expression)
  (if (atom expression)
      (pred-class expression *classinstancevar*)
      (macroexpand expression)))


(defmacro o-subclassof (sub-expression super-expression)
  (with-logic-var *classinstancevar*
    (l-forall (list *classinstancevar*) (l-implies (o-class-expression sub-expression) (o-class-expression super-expression)))))

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
    (l-exists (list e) (l-and (pred-property property-expression *classinstancevar* e)
			      (let ((*classinstancevar* e))
				(o-class-expression class-expression))))))

(defmacro o-objectallvaluesfrom (property-expression class-expression)
  (with-logic-var e
    (l-forall (list e) (l-implies (pred-property property-expression *classinstancevar* e)
				  (let ((*classinstancevar* e))
				    (o-class-expression class-expression))))))

(defmacro o-objecthasself (property-expression)
  (pred-property property-expression *classinstancevar*  *classinstancevar*))

(defmacro o-objecthasvalue (property-expression individual)
  (pred-property property-expression *classinstancevar*  individual))

(defmacro o-functionalobjectproperty (property)
  (with-logic-var a
    (with-logic-var b
      (with-logic-var c
	(l-forall (list a b c)
		  (l-implies (l-and (pred-property property c a)
				    (pred-property property c b))
			     (l-equal a b)))))))

(defmacro o-inversefunctionalobjectproperty (property)
  (with-logic-var a
    (with-logic-var b
      (with-logic-var c
	(l-forall (list *classinstancevar* a b)
		  (l-implies (l-and (pred-property property a c)
				    (pred-property property b c))
			     (l-equal a b)))))))

(defmacro o-transitiveobjectproperty (property)
  (with-logic-var a
    (with-logic-var b
      (with-logic-var c
	(l-forall (list a b c)
		  (l-implies (l-and (pred-property property a b)
				    (pred-property property b c))
			     (pred-property property a c)))))))

(defmacro o-reflexiveobjectproperty (property)
  (with-logic-var a
    (l-forall (list a)
	      (pred-property property a a))))

(defmacro o-irreflexiveobjectproperty (property)
  (with-logic-var a
    (l-forall (list a)
	      (l-not (pred-property property a a)))))

(defmacro o-symmetricobjectproperty (property)
  (with-logic-var a
    (with-logic-var b
      (l-forall (list a b)
		(l-iff (pred-property property a b) (pred-property property b a))))))

(defmacro o-asymmetricobjectproperty (property)
  (with-logic-var a
    (with-logic-var b
      (l-forall (list a b)
		(not (and (pred-property property a b) (pred-property property b a)))))))

(defmacro o-inverseobjectproperties (p1 p2)
  (with-logic-var a
    (with-logic-var b
      (l-forall (list a b)
		(l-iff (pred-property p1 a b) (pred-property p2 b a))))))

(defmacro o-equivalentobjectproperties (&rest properties)
  (with-logic-var a
    (with-logic-var b
      (apply 'l-and
	     (loop for (p1 p2) on properties while p2
		   collect (l-iff (pred-property p1 a b) (pred-property p2 a b)))))))

(defmacro o-objectpropertydomain (property class-expression)
  (with-logic-var *classinstancevar*
    (with-logic-var b
      (l-forall (list *classinstancevar* b)
		(l-implies (pred-property property *classinstancevar* b)
			   (o-class-expression class-expression))))))

(defmacro o-objectpropertyrange (property class-expression)
  (with-logic-var *classinstancevar*
    (with-logic-var b
      (l-forall (*classinstancevar* b)
	       (implies (pred-property property b *classinstancevar*)
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
					    collect (pred-property (second el) next this)
					  else collect (pred-property el this next)))
		      (if (consp target)
			  (funcall #'pred-property target (car (last vars)) (car vars))
			  (apply #'pred-property target (car vars) (last vars))))))))

(defmacro o-subobjectpropertyof (sub super)
  (if (and (consp sub) (eq (car sub) 'o-objectpropertychain))
      (chain-fol-expression super (cdr sub))
	(with-logic-var x
	  (with-logic-var y
	    (l-forall (list x y) (l-implies (pred-property sub x y)
					    (pred-property super x y)))))))

(defmacro o-disjointobjectproperties (&rest properties)
  (apply 'l-and (loop for (p1 . rest) on properties
		      append
		      (loop for p2 in rest
			    collect 
			    (with-logic-var x
			      (with-logic-var y
				(l-forall (list x y)
					  (l-not (l-and (pred-property p1 x y) (pred-property p2 x y))))))))))
						  
(defmacro o-objectoneof (&rest individuals)
  (apply 'l-or (loop for i in individuals collect (l-equal i *classinstancevar*))))

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
  (pred-property prop x y))

(defmacro o-negativeobjectpropertyassertion (prop x y)
  (l-not (o-objectpropertyassertion prop x y)))

(defmacro o-sameindividual (&rest individuals)
  (apply 'l-and
	 (loop for (a b) on individuals until (null b)
	       collect (l-equal a b))))

(defmacro o-differentindividuals (&rest individuals)
  (apply 'l-and (loop for (a . rest) on individuals
		      append
		      (loop for b in rest 
			    collect (l-not (l-equal a b))))))

(defmacro o-objectmincardinality (property number)
  (with-logic-vars (is number)
    (apply 'l-and
	   (list*
	    (macroexpand `(o-differentindividuals ,@is))
	    (loop for i in is collect (pred-property property *classinstancevar* i))))))

(defmacro o-objectexactcardinality (property number)
  (with-logic-vars (is number)
    (apply 'l-and
	   (list*
	    (macroexpand `(o-differentindividuals ,@is))
	    (with-logic-var other
	     (l-forall (list other) 
		       (l-implies (pred-property property *classinstancevar* other)
				  (apply 'l-or
					 (loop for i in is collect (l-equal i other)))))
	     )
	    (loop for i in is collect (pred-property property *classinstancevar* i)))
	   )))

(defmacro o-objectmaxcardinality (property number)
  (with-logic-vars (is number)
    (apply 'l-and
	   (list*
	    (with-logic-var other
	     (l-forall (list other) 
		       (l-implies (pred-property property *classinstancevar* other)
				  (apply 'l-or
					 (loop for i in is collect (l-equal i other)))))
	     )
	    (loop for i in is collect (pred-property property *classinstancevar* i)))
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
						collect (pred-property p a v)
						collect (pred-property p b v))))
			     (l-equal a b)))))))

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
      for d-name = (intern(concatenate 'string "O-" (string head)))
      for o-equivalent = (intern(concatenate 'string "O-" (#"replaceFirst" (string head) "DATA" "OBJECT" )))
      do (setf (macro-function d-name) (macro-function o-equivalent)))


(defun owl-sexp-to-fol (expression)
  (labels ((o-rewrite (expression)
	     (if (atom expression)
		 (if (gethash expression *owl2-vocabulary-forms*) 
		     (intern (concatenate 'string "O-" (string expression)))
		     expression)
		 (mapcar #'o-rewrite expression))))
    (macroexpand (o-rewrite (mapcar 'rewrite-owl-canonical-functional expression)))))


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
