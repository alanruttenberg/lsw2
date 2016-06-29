;; looks correct
(defun parents-tree (x &optional (kb *default-kb*) (table (make-hash-table :test 'equalp)))
  (let ((child (list x (if (eq x !owl:Thing) "Thing" (entity-annotation-value x kb !rdfs:label))))
	(parents (mapcar
		  (lambda(c)
		    (if (eq c !owl:Thing)
			(list c "Thing")
			(list c (entity-annotation-value c kb !rdfs:label))))
		  (parents x kb))))
    (if parents
	(loop for parent in parents 
	   for pt = (or (gethash parent table) (setf (gethash parent table) (parents-tree (car parent) kb table)))
	   do  (setf (gethash pt table) (or (gethash pt table) pt))
	   collect (list* child  (gethash pt table))
	     )
	(list (list child ))
	)))

(defun ontology-subclass-dag (kb &key (include-referencing t) (start !owl:Thing))
  (let ((nodes-lookup (make-hash-table)) edges nodes)
    (loop for term in (cons start (descendants start kb))
	 for count from 1
       for node = (make-dag-term-node
		   :uri term
		   :label (if (eq term !owl:Thing)
			      "Thing"
			      (entity-annotation-value term kb !rdfs:label))
		   :parents (unless (eq term start) (parents term kb))
		   :tooltip (tree-tooltip kb term :include-referencing include-referencing)
		   :index count)
       do (setf (gethash term nodes-lookup) node))
    (maphash (lambda(term node)
	       (push node nodes)
	       (loop for parent in (dag-term-node-parents node)
		  do (push (make-dag-term-edge :from node :to (gethash parent nodes-lookup)) edges)))
	     nodes-lookup )
    (values nodes edges)))


(defun browse-subclass-tree (title &optional (ont *default-kb*) &key (orientation "RL") (start !owl:Thing))
  (multiple-value-bind (nodes edges) (ontology-subclass-dag ont :start start)
      (let ((spec (emit-dagre-d3-javascript-ne  nodes edges))
	    (data-path (temp-directory-path (concatenate 'string title ".js"))))
	(with-open-file (out data-path :direction :output :if-exists :supersede)
	  (write-string spec out))
	(show-dag data-path orientation))))

  
;; (parents-tree !<http://snomed.info/id/65172003> s)
