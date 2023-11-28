(in-package :cl-user)

(defmethod describe-object ((uri uri) stream &aux direct)
  (if (and *default-kb*
	   (some (lambda(type) (get-entity uri type *default-kb*))
		 '(:class :object-property :data-property :individual)))
      (progn
	(loop for (prop value) in (entity-annotations uri)
	      do (format stream "~%~a: ~s" (or (uri-label prop ) prop) value))
	(map nil (lambda(e) (pprint e stream))
	     (setq direct (append (when (get-entity uri :class *default-kb*)
				    (get-referencing-axioms uri :class *default-kb* t))
				  (when (get-entity uri :object-property *default-kb*)
				    (get-referencing-axioms uri :object-property *default-kb* t))
				  (when (get-entity uri :data-property *default-kb*)
				    (get-referencing-axioms uri :data-property *default-kb* t))
				  (when (get-entity uri :individual *default-kb*)
				    (get-referencing-axioms uri :individual *default-kb* t)))))
	(format t "~%Referenced in other axioms:")
      
	(map nil (lambda(e) (pprint e stream))
	     (remove-if (lambda(e) (and (consp e) (eq (car e) 'declaration)))
			(set-difference (append (when (get-entity uri :class *default-kb*)
				       (get-referencing-axioms uri :class *default-kb* nil))
				     (when (get-entity uri :object-property *default-kb*)
				       (get-referencing-axioms uri :object-property *default-kb* nil))
				     (when (get-entity uri :data-property *default-kb*)
				       (get-referencing-axioms uri :data-property *default-kb* nil))
				     (when (get-entity uri :individual *default-kb*)
				       (get-referencing-axioms uri :individual *default-kb* nil)))
			     direct
			     :test 'equalp))))
      (call-next-method)))

;; Extension point:
(defvar *possible-uri-actions* nil)
;; each element is a list: (predicate  action label)
;; predicate is called on uri. If non-nil then add a :action element where action is called with inspected uri and label labels it.

(defun swank-emacs-inspect (uri &aux mentioned)
  (if (and (boundp '*default-kb*) *default-kb*
	   (some (lambda(type) (get-entity uri type *default-kb*))
		 '(:class :object-property :data-property :individual)))
      (let ((*print-uri-with-labels-from* (or (and (boundp '*print-uri-with-labels-from*) *print-uri-with-labels-from*)
					      (list *default-kb*)))
	    (*print-uri-with-labels-show-source* nil)
	    (*print-circle* nil)
	    (*print-case* :downcase))
	(append `((:label "URI: ") (:value ,uri ,(uri-full uri)) (:newline)
		  (:label "Ontology: ") (:value ,*default-kb*) (:newline))
		(loop for (predicate fn label) in *possible-uri-actions*
		      when (funcall predicate uri)
			append (let ((fn fn)(uri uri)) `((:action ,label ,(lambda() (funcall fn uri))) (:newline))))
		(loop for (prop value) in (entity-annotations uri)
		      collect `(:label ,(or (uri-label prop )
					    (uri-abbreviated prop)
					    (uri-full prop)))
		      collect ": "
		      collect `(:value ,value)
		      collect '(:newline))
		(flet ((mentioned(axioms)
			 (setq mentioned (append (axioms-signature axioms) mentioned))))
		  (flet ((collect-axioms (direct?)
			   (loop for type in '(:class :object-property :data-property :individual)
				 for axioms = (when (get-entity uri type *default-kb*)
						(get-referencing-axioms uri :class *default-kb* direct?))
				 collect (list type axioms))))
		    (flet ((axioms-display-lines (axioms type)
			     (when axioms
			       (list* `(:label ,(format nil "As ~a: " (string-downcase type)))
				      '(:newline)
				      (loop for ax in axioms
					    for pprinted = (with-output-to-string (s) 
							     (let ((*print-lines* nil)
								   (*print-right-margin* 100))
							       (pprint ax s)))
					    collect `(:value ,ax ,(subseq pprinted 1)) collect '(:newline))))))
		      (list* '(:label "Direct axioms") '(:newline)
			     (loop for (type axioms) in (collect-axioms t)
				   append (axioms-display-lines axioms type) into lines
				   append axioms into direct-axioms
				   do (mentioned axioms)
				   finally
				      (return (append lines
						      (list* '(:label "Mentioned in axioms") '(:newline)
							     (loop for (type axioms) in (collect-axioms nil)
								   for indirect = (set-difference axioms direct-axioms)
								   append (axioms-display-lines indirect type)
								   do (mentioned axioms)
								   )))))))))
		`((:label "Mentioned: ") (:newline)
		  ,@(loop for mention in (sort (remove uri (remove-duplicates mentioned)) 'string-lessp :key 'uri-label )
			  append `(" " (:value ,mention) (:newline))))
		))))

(defun define-owl-emacs-inspect ()
  (eval `(defmethod ,(intern "EMACS-INSPECT" "SWANK") ((uri uri))
	   (append 
	    `((:action "Access on web" ,(lambda() (browse-url uri))) (:newline)
	      (:action "GET to buffer" ,(lambda() 
					  (funcall (intern "ED-IN-EMACS" swank)
						   `(:string ,(get-url (uri-full uri) :dont-cache t :force-refetch t :persist nil)))))
	      (:newline))
	    (or (swank-emacs-inspect uri)
		(call-next-method))))))

(after-swank-is-loaded 'define-owl-emacs-inspect)



