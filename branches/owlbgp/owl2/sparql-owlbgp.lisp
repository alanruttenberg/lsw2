(defvar *register-terp-once* (progn (load-time-value (#"registerFactory" 'ARQTerpParser)) t))

(defun sparql-with-owlbgp (query  &key (kb (and (boundp '*default-kb*) *default-kb*))
				    (syntax :sparql)  )
  (let* ((ontology-graph 
	  (or (v3kb-sparql-ontology-graph kb) 
	      (setf (v3kb-sparql-ontology-graph kb)
		    (new 'OWLOntologyGraph (v3kb-ont kb) (get-reasoner-factory kb)))))
	 (sparql-engine (or (v3kb-sparql-engine kb)
			    (setf (v3kb-sparql-engine kb) 
				  (new 'OWLReasonerSPARQLEngine))))
	 (dataset (or (v3kb-sparql-dataset kb) (setf (v3kb-sparql-dataset kb) (new 'OWLOntologyDataSet ontology-graph +NULL+)))))
    (when (consp query) (setq query (sparql-stringify query)))
    (let ((query-object (#"create" 'QueryFactory query (if (eq syntax :terp) 
							   (#"getInstance" 'TerpSyntax)
							   (#"lookup" 'Syntax "SPARQL")))))
      (#"execQuery" sparql-engine query-object dataset))))


  
(defun process-sparql-results (jresult &key (trace nil) (flatten nil) (values t) (count nil) (trace-show-query trace) query)
  (let ((do-trace (or *sparql-always-trace* (and trace  *sparql-allow-trace*))))
    (if (and do-trace trace-show-query)
      (format t "Query: ~a~%~a~%Results:~%" (or trace "Tracing all")  query)
      (if do-trace
	  (format t "Query: ~a~%Results:~%" (or trace "Tracing all"))))
    (let ((projected-vars (set-to-list (#"getResultVars" jresult))))
      (with-constant-signature ((getv "get") (next "next") (has-next "hasNext") (get-uri "getURI"))
	(flet ((get-vars (bindingset)
		 (let ((bindings
			(loop for var in projected-vars
			   for jval = (getv bindingset var)
			   for val = (if (null jval) 
					 nil
					 (if 
					  (#"isResource" jval)
					  (make-uri (or (get-uri jval)
							(format nil "~a~a" *blankprefix* (#"toString" jval))
							))
					  (#"getValue" jval)))
			   collect val)))
		   (when trace
		     (format t "~{~s~^	~}~%" bindings))
		   bindings)))
	  (if count (return-from process-sparql-results (loop while (has-next jresult) do (next jresult) sum 1)))
	  (if values 
	      (if flatten 
		  (loop while (has-next jresult) 
		     append
		       (get-vars (next jresult)))
		  (loop while (has-next jresult) 
		     collect 
		       (get-vars (next jresult))))
	      (loop while (has-next jresult) 
		 do (get-vars (next jresult)) finally (return (values))
		   )
	      ))))))


