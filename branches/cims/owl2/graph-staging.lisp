(defun html-for-class (entity kb)
    (let ((parents (format-parents (make-uri entity) kb))
	  (form (coalesce-disjoints (pretty-aterm-sexp
				     (mapcar 
				      'aterm-to-sexp
				      (setq axioms 
					    (set-to-list 
					     (#"getAxioms" (#"getTBox" (kb-kb kb)) aterm))))))))
      (with-output-to-string (s)
	(format s "<b>~a:  ~a</b>"
		*super-label*
		parents)
	(when *show-logical-in-tooltip*
	  (format s "<br><font color=\"#000080\">~a</font>"
		  (let ((*print-right-margin* (floor width 8)))
		    (if (consp form)
			(concatenate 'string (format-sexp-for-tooltip form)  "<br>")
			""
			))))
	(let* ((apv (annotation-property-values-or-labels entity kb)))
	  (loop for (p val) in apv
	     with need-hrule = t
	     when (and val (not (eq p !rdfs:comment)) (not (eq p !rdfs:label)))
	     unless (and (not (uri-p val)) (#"matches" val *flush-annotation-value-regexp*))
	     do 
	     (let ((p-label (p-label (uri-full p) (pretty-aterm-sexp (aterm-to-sexp (get-entity (uri-full p) kb))))))
	       (when (and need-hrule *want-hrule*) (format s "<hr>") (setq need-hrule nil))
	       (format s "<i>~a</i>: ~a<br>" 
		       p-label
		       (if (uri-p val)
			   (or (car (rdfs-label (uri-full p) kb))
			       (pretty-aterm-sexp val))
			   (if (stringp val)
			       val
			       val)))))
	  )
	(unless (or *inhibit-property-info-in-toolips* (equal entity (uri-full !owl:Nothing)))
	  (let nil			;((*current-labels* nil))
	    (multiple-value-bind (in-domain-of in-domain-of-subclass maybe-in-domain-of) (properties-that-can-have-as-subject (make-uri entity) kb)
	      (multiple-value-bind (in-range-of in-range-of-subclass maybe-in-range-of) (properties-that-can-have-as-object (make-uri entity) kb)
		(when (or in-domain-of in-range-of maybe-in-range-of maybe-in-domain-of in-domain-of-subclass in-range-of-subclass)
		  (when *want-hrule* (write-string "<hr>" s))
		  (when in-domain-of
		    (format s "<b>Subject of</b>: ~a<br>"  (join-with-char  (remove "ObsoleteProperty"  (mapcar 'pretty-aterm-sexp in-domain-of) :test 'equal) ", " )))
		  (when (or maybe-in-domain-of in-range-of-subclass)
		    (format s "<b>Possibly subject of</b>: ~a<br>"
			    (join-with-char (remove "ObsoleteProperty"
						    (append (mapcar (lambda(p) (format nil "<font color=\"#900000\">~a</font>" (pretty-aterm-sexp p))) maybe-in-domain-of)
							    (mapcar (lambda(p) (format nil "<font color=\"#505050\">~a</font>" (pretty-aterm-sexp p))) in-domain-of-subclass)) 
						    :test 'equal)
					    ", ")
			    ))
		  (when in-range-of
		    (format s "<b>Object of</b>: ~a<br>" (join-with-char (remove "ObsoleteProperty" (mapcar 'pretty-aterm-sexp in-range-of) :test 'equal) ", " )))
				 
		  (when (or maybe-in-range-of in-range-of-subclass)
		    (format s "<b>Possibly object of</b>: ~a<br>"
			    (join-with-char (remove "ObsoleteProperty"
						    (append (mapcar (lambda(p) (format nil "<font color=\"#900000\">~a</font>" (pretty-aterm-sexp p))) maybe-in-range-of)
							    (mapcar (lambda(p) (format nil "<font color=\"#505050\">~a</font>" (pretty-aterm-sexp p))) in-range-of-subclass)) 
						    :test 'equal)
					    ", ")
			    ))))))))))

(defun html-for-individual (entity kb)
  (with-output-to-string (s)
    (format s "a ~{~a~^, ~}<br>"
	    (or (remove "_TOP_"
			(mapcar 'pretty-aterm-sexp
				(mapcar 'aterm-to-sexp
					(apply 'append
					       (map 'list 'set-to-list
						    (set-to-list (#"getTypes" (kb-kb kb) aterm t))))))
			:test 'equal)
		(list "Thing")))
    (let* ((props (kb-properties kb))
	   (vs (mapcar (lambda(p) (mapcar 'aterm-to-sexp (set-to-list (#"getPropertyValues" (kb-kb kb) p aterm)))) props))
	   (apv (annotation-property-values-or-labels entity kb)))
      (loop for p in (append props (mapcar (lambda(pv) (get-entity (uri-full(first pv)) kb)) apv))
	 for vals in (append vs (mapcar 'list (mapcar 'second apv)))
	 with need-hrule = t
	 when vals 
	 do (let ((p (pretty-aterm-sexp (aterm-to-sexp p))))
	      (unless (and (listp p) (equal (caar p) :inv))
		(when (and need-hrule *want-hrule*) (format s "<hr>") (setq need-hrule nil))
		(write-string 
		 (format nil "<i>~a</i>: ~{~a~^, ~}<br>" 
			 p
			 (mapcar (lambda (v) (if (stringp v) 
						 v
						 (if (uri-p v)
						     (or (car (rdfs-label (uri-full v) kb))
							 (pretty-aterm-sexp v))
						     (pretty-aterm-sexp v))))
				 vals)
			 "&nbsp;&nbsp;&nbsp;&nbsp;") s)))
	 ))))