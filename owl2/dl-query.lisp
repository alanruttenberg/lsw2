(defun children (class &optional (kb *default-kb*))
  (class-query class kb (lambda(ce reasoner) (#"getSubClasses" reasoner ce t))))

(defun descendants (class &optional (kb *default-kb*))
  (class-query class kb (lambda(ce reasoner) (#"getSubClasses" reasoner ce nil))))

(defun parents (class &optional (kb *default-kb*))
  (class-query class kb (lambda(ce reasoner) (#"getSuperClasses" reasoner ce t))))

(defun ancestors (class &optional (kb *default-kb*))
  (class-query class kb (lambda(ce reasoner) (#"getSuperClasses" reasoner ce nil))))

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
