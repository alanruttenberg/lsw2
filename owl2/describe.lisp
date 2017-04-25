(defmethod describe-object ((uri uri) stream)
  (if (and *default-kb*
	   (some (lambda(type) (get-entity uri type *default-kb*))
		 '(:class :object-property :data-property :individual)))
      (progn
	(loop for (prop value) in (entity-annotations uri)
	    do (format stream "~%~a: ~s" (or (uri-label prop ) prop) value))
	(map nil (lambda(e) (pprint e stream))
	     (append (when (get-entity uri :class *default-kb*)
		       (get-referencing-axioms uri :class *default-kb* t))
		     (when (get-entity uri :object-property *default-kb*)
		       (get-referencing-axioms uri :object-property *default-kb* t))
		     (when (get-entity uri :data-property *default-kb*)
		       (get-referencing-axioms uri :data-property *default-kb* t))
		     (when (get-entity uri :individual *default-kb*)
		       (get-referencing-axioms uri :individual *default-kb* t)))))
      (call-next-method)))

(defun swank-emacs-inspect (uri)
  (if (and *default-kb*
	   (some (lambda(type) (get-entity uri type *default-kb*))
		 '(:class :object-property :data-property :individual)))
      (let ((*print-uri-with-labels-from* (or *print-uri-with-labels-from* (list *default-kb*)))
	    (*print-uri-with-labels-show-source* nil))
	(append `((:label "URI: ") (:value ,uri ,(uri-full uri)) (:newline)
		  (:label "Ontology: ") (:value ,*default-kb*) (:newline))
		(loop for (prop value) in (entity-annotations uri)
		      collect `(:label ,(or (uri-label prop )
					    (uri-abbreviated prop)
					    (uri-full prop)))
		      collect ": "
		      collect `(:value ,value)
		      collect '(:newline))
		`((:label "Axioms") (:newline))
		(loop for type in '(:class :object-property :data-property :individual)
		      for axioms = (when (get-entity uri type *default-kb*)
				     (get-referencing-axioms uri :class *default-kb* t))
		      when axioms append
				  (list* `(:label ,(format nil "As ~a: " (string-downcase type)))
					 '(:newline)
					 (loop for ax in axioms
					       for pprinted = (with-output-to-string (s) 
								(let ((*print-lines* nil)
								      (*print-right-margin* 100))
								  (pprint ax s)))
					       collect `(:value ,ax ,(subseq pprinted 1)) collect '(:newline))))))
      (call-next-method)))

(defun define-owl-emacs-inspect ()
  (eval `(defmethod ,(intern "EMACS-INSPECT" "SWANK") ((uri uri))
	   (swank-emacs-inspect uri))))

(defvar cl-user::*after-swank-init-hook* nil)
(pushnew 'define-owl-emacs-inspect cl-user::*after-swank-init-hook*)



