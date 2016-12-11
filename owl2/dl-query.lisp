(defun children (class &optional (kb *default-kb*))
  (class-query class kb (lambda(ce reasoner) (#"getSubClasses" reasoner ce t))))

(defun property-children (property &optional (kb *default-kb*))
  (property-query property kb (lambda(pe reasoner)  (#"getSubObjectProperties" reasoner pe t))))

(defun descendants (class &optional (kb *default-kb*))
  (class-query class kb (lambda(ce reasoner) (#"getSubClasses" reasoner ce nil))))

(defun property-descendants (property &optional (kb *default-kb*))
  (property-query property kb (lambda(pe reasoner)  (#"getSubObjectProperties" reasoner pe t))))

(defun parents (class &optional (kb *default-kb*))
  (class-query class kb (lambda(ce reasoner) (#"getSuperClasses" reasoner ce t))))

(defun property-parents (property &optional (kb *default-kb*))
  (property-query property kb (lambda(pe reasoner)  (#"getSuperObjectProperties" reasoner pe t))))p

(defun ancestors (class &optional (kb *default-kb*))
  (class-query class kb (lambda(ce reasoner) (#"getSuperClasses" reasoner ce nil))))

(defun property-ancestors (property &optional (kb *default-kb*))
  (property-query property kb (lambda(pe reasoner)  (#"getSuperObjectProperties" reasoner pe nil))))

(defun instances (class &optional (kb *default-kb*))
  (class-query class kb (lambda(ce reasoner) (#"getInstances" reasoner ce nil))))

(defun direct-instances (class &optional (kb *default-kb*))
  (class-query class kb (lambda(ce reasoner) (#"getInstances" reasoner ce t))))

(defun equivalents (class &optional (kb *default-kb*))
  (class-query class kb (lambda(ce reasoner) (#"getEquivalentClasses" reasoner ce)) nil t))

(defun leaves (class &optional (kb *default-kb*))
  (class-query class kb (lambda(ce reasoner) (#"getSubClasses" reasoner ce nil)) t nil 
	       (lambda (ce reasoner)
		 (#"isBottomSingleton" (#"getSubClasses" reasoner ce t))
		 )))
	       
(defun same-individuals (individual &optional (kb *default-kb*))
  (loop for e in (jss::set-to-list
		  (#"getSameIndividuals" (v3kb-reasoner kb)
		     (#"getOWLNamedIndividual" (v3kb-datafactory kb) (to-iri individual))))
       collecting (make-uri (#"toString" (#"getIRI" e)))))

(defun entailed? (axiom-expression &optional (kb *default-kb*))
  (error "todo")
  (#"isEntailed" (list-to-java-set (list (to-axiom-expression class-expression kb))) kb)) ;not yet implemented

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
