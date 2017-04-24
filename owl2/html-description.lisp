(defmethod html-describing-class-parents ((kb v3kb) entity)
  (let* ((asserted (stated-superclasses entity kb))
	 (inferred (set-difference (parents entity kb) asserted))
	 (mi? (> (length asserted) 1)))
    (if (or inferred asserted)
	(format nil "~{~a~^, ~}"
		(append
		 (mapcar (lambda(c)
			   (if mi?
			       (format nil "<font color=\"red\">~a</font>" (car (rdfs-label c kb)) )
			       (label-from-uri kb c)))
			 (remove-duplicates asserted :test 'equalp))
		 (mapcar (lambda(c) (format nil "<i>~a</i>" (label-from-uri kb c) inferred))
			 (remove-duplicates inferred :test 'equalp))
		 ))
	"<i><font color=\"red\">Thing</font></i>")))

(defmethod html-describing-class ((kb v3kb) entity &optional include-referencing)
  (let ((parents (html-describing-class-parents kb (make-uri entity)))
	(leading-entity-name-regex (format nil "^'{0,1}~a'{0,1}" (quote-for-regex (label-from-uri kb entity)))))
    (with-output-to-string (s)
      (unless (equal entity (uri-full !owl:Nothing))
	(format s "<b>~a:  ~a</b><br>" *super-label* parents))
      (loop for sentence in (manchester-logical-axioms-for-class entity kb include-referencing)
	   do (format s "&nbsp;&nbsp;<span class=\"logical\">~a</span><br>" (#"replaceFirst" sentence leading-entity-name-regex "")))
      (let* ((apv (annotation-property-values-for-description kb entity)))
	(loop for (p val) in apv
	   with need-hrule = t
	   when (and val (not (eq p !rdfs:comment)) (not (eq p !rdfs:label)))
	   unless (and (not (uri-p val)) (#"matches" val *flush-annotation-value-regexp*))
	   do 
	   (let ((p-label (or (label-from-uri kb p) (#"replaceAll" (uri-full p) ".*/" "" ))))
	     (when (and need-hrule *want-hrule*) (format s "<hr>") (setq need-hrule nil))
	     (format s "<i>~a</i>: ~a<br>" 
		     p-label
		     (if (uri-p val)
			 (or (label-from-uri kb p)
			     val)
			 val))))
	))))

(defmethod annotation-property-values-for-description ((kb v3kb) entity )
  (remove-if
   (lambda(el) (member (car el) `(,!rdfs:comment ,!oboinowl:hasDefinition))) ; these already in comment
   (entity-annotations entity kb)))
