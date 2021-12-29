(defpackage lsw2/dlquery
  (:use cl)
  (:import-from :cl-user cl-user::class-query cl-user::instance-query cl-user::property-query cl-user::get-entity
		cl-user::make-uri cl-user::instantiate-reasoner 
		cl-user::to-class-expression cl-user::v3kb-reasoner cl-user::*default-kb* cl-user::to-iri cl-user::v3kb-datafactory
		cl-user::v3kb-uri2entity cl-user::get-inferred-axioms cl-user::get-owl-literal-value 
		;; used earlier
		cl-user::children cl-user::parents cl-user::instances
		cl-user::descendants 
		cl-user::direct-instances 
		cl-user::equivalents 
		cl-user::satisfiable? 
		cl-user::is-subclass-of? 
		cl-user::classtree-depth 
		)
  (:import-from :jss #:set-to-list)
  (:import-from :java java::jclass java::jinstance-of-p java::jequal)
  (:export  #:property-children  #:property-descendants 
	   #:property-parents #:ancestors #:property-ancestors  #:instance-types
	    #:property-equivalents  #:leaves #:same-individuals #:instance-properties
	   #:entailed?     #:equivalent-classes?))
(use-package :lsw2/dlquery :cl-user)

(in-package lsw2/dlquery)

(defun children (class &optional (kb *default-kb*))
  (class-query class kb (lambda(ce reasoner) (#"getSubClasses" reasoner ce t))))

(defun property-children (property &optional (kb *default-kb*))
  (property-query property kb (lambda(pe reasoner)  (#"getSubObjectProperties" reasoner pe t))))

(defun descendants (class &optional (kb *default-kb*))
  (class-query class kb (lambda(ce reasoner) (#"getSubClasses" reasoner ce nil))))

(defun property-descendants (property &optional (kb *default-kb*))
  (property-query property kb (lambda(pe reasoner) (#"getSubObjectProperties" reasoner pe nil))))

(defun parents (class &optional (kb *default-kb*))
  (class-query class kb (lambda(ce reasoner) (#"getSuperClasses" reasoner ce t))))

(defun property-parents (property &optional (kb *default-kb*))
  (property-query property kb (lambda(pe reasoner)  (#"getSuperObjectProperties" reasoner pe t))))

(defun ancestors (class &optional (kb *default-kb*))
  (class-query class kb (lambda(ce reasoner) (#"getSuperClasses" reasoner ce nil))))

(defun property-ancestors (property &optional (kb *default-kb*))
  (property-query property kb (lambda(pe reasoner)  (#"getSuperObjectProperties" reasoner pe nil))))

(defun instances (class &optional (kb *default-kb*))
  (class-query class kb (lambda(ce reasoner) (#"getInstances" reasoner ce nil))))

(defun instance-types (instance &optional (kb *default-kb*))
  (instance-query instance kb (lambda(ce reasoner) (#"getTypes" reasoner ce nil))))
  
(defun direct-instances (class &optional (kb *default-kb*))
  (class-query class kb (lambda(ce reasoner) (#"getInstances" reasoner ce t))))

(defun property-equivalents (property &optional (kb *default-kb*))
  (property-query property kb
		  (if (get-entity property :data-property kb)
		      (lambda(pe reasoner)
			(#"getEquivalentDataProperties" reasoner pe))
		      (lambda(pe reasoner)
			(#"getEquivalentObjectProperties" reasoner pe)))
		  nil))

(defun individual-properties (instance &optional (kb *default-kb*))
  (let ((ind (car (find :individual (gethash instance (v3kb-uri2entity kb)) :key 'second)))
	(dpa-class (jclass "org.semanticweb.owlapi.model.OWLDataPropertyAssertionAxiom")))
    (loop for assertion in (get-inferred-axioms kb :types '(:property-assertions))
	  when (jequal (#"getSubject" assertion) ind)
	    collect (list (make-uri (#"toString" (#"getIRI" (#"getProperty" assertion))))
			  (if (jinstance-of-p assertion dpa-class)
			      (get-owl-literal-value (#"getObject" assertion))
			      (make-uri (#"toString" (#"getIRI"  (#"getObject" assertion)))))))))

(defun equivalents (class &optional (kb *default-kb*))
  (class-query class kb (lambda(ce reasoner) (#"getEquivalentClasses" reasoner ce)) nil t))

(defun leaves (class &optional (kb *default-kb*))
  (class-query class kb (lambda(ce reasoner) (#"getSubClasses" reasoner ce nil)) t nil 
	       (lambda (ce reasoner)
		 (#"isBottomSingleton" (#"getSubClasses" reasoner ce t))
		 )))
	       
(defun same-individuals (individual &optional (kb *default-kb*))
  (loop for e in (set-to-list
		  (#"getSameIndividuals" (v3kb-reasoner kb)
		     (#"getOWLNamedIndividual" (v3kb-datafactory kb) (to-iri individual))))
       collecting (make-uri (#"toString" (#"getIRI" e)))))

;; FIXME to-axiom-expression is not defined
(defun entailed? (axiom-expression &optional (kb *default-kb*))
  (error "todo")
  (#"isEntailed" (cl-user::list-to-java-set (list (to-axiom-expression axiom-expression kb))) kb)) ;not yet implemented

(defun satisfiable? (class-expression &optional (kb *default-kb*))
  (instantiate-reasoner kb)
  (#"isSatisfiable" (v3kb-reasoner kb) (to-class-expression class-expression kb)))

(defun is-subclass-of? (sub super &optional (kb *default-kb*))
  "This is faster than using parents or ancestors as you don't have to classify the ontology in order to test it"
  (if (stringp super)
      (setq super (make-uri (#"toString" (#"getIRI" (to-class-expression super))))))
  (if (stringp sub)
      (setq sub (make-uri (#"toString" (#"getIRI" (to-class-expression sub))))))
  (not (satisfiable? `(and (not ,super) ,sub) kb)))

(defun equivalent-classes? (class-expression-1 class-expression-2 &optional (kb *default-kb*))
  (instantiate-reasoner kb)
  (let ((ce1 (to-class-expression class-expression-1 kb))
	(ce2 (to-class-expression class-expression-2 kb)))
    (not (#"isSatisfiable" (v3kb-reasoner kb)
			   (to-class-expression `(or (and ,ce1 (not ,ce2)) (and ,ce2 (not ,ce1))))
			   ))))

(defun classtree-depth (kb &aux (maxdepth 0))
  (labels ((each-node (c depth)
	     (setq maxdepth (max maxdepth depth))
	     (dolist (cc (lsw2/dlquery::children c kb))
	       (unless (eq cc !owl:Nothing)
		 (each-node cc (1+ depth))))))
    (each-node !owl:Thing 0)
    maxdepth))

