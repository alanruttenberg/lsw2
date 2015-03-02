(defun children (class kb)
  (class-query class kb (lambda(ce reasoner) (#"getSubClasses" reasoner ce t))))

(defun descendants (class kb)
  (class-query class kb (lambda(ce reasoner) (#"getSubClasses" reasoner ce nil))))

(defun parents (class kb)
  (class-query class kb (lambda(ce reasoner) (#"getSuperClasses" reasoner ce t))))

(defun ancestors (class kb)
  (class-query class kb (lambda(ce reasoner) (#"getSuperClasses" reasoner ce nil))))

(defun instances (class kb)
  (class-query class kb (lambda(ce reasoner) (#"getInstances" reasoner ce nil))))

(defun direct-instances (class kb)
  (class-query class kb (lambda(ce reasoner) (#"getInstances" reasoner ce t))))

(defun equivalents (class kb)
  (class-query class kb (lambda(ce reasoner) (#"getEquivalentClasses" reasoner ce)) nil t))


(defun same-individuals (individual kb)
  (loop for e in (jss::set-to-list
		  (#"getSameIndividuals" (v3kb-reasoner kb)
		     (#"getOWLNamedIndividual" (v3kb-datafactory kb) (to-iri individual))))
       collecting (make-uri (#"toString" (#"getIRI" e)))))

(defun entailed? (axiom-expression kb)
  (error "todo")
  (#"isEntailed" (list-to-java-set (list (to-axiom-expression class-expression kb))) kb)) ;not yet implemented

(defun satisfiable? (class-expression kb)
  (instantiate-reasoner kb)
  (#"isSatisfiable" (v3kb-reasoner kb) (to-class-expression class-expression kb)))
