;; looks correct
(defun parents-tree (x &optional (kb *default-kb*) (table (make-hash-table :test 'equalp)))
  "Starting from a node in the ontology, build up the hierarchy to !owl:Thing. Each element is a pair of term and label. The tree is built up recursively from the bottom. 
(tree x) = (x (tree (parent_1 x)) (tree (parent_2 x)) ...)
!owl:Thing has no parents.
parent_i is the ith direct parent of x.
Because there is multiple inheritence, the portions of the parents tree can be duplicated. Elements (term label) are made unique by registering them in a hash table."
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
  (let ((nodes-lookup (make-hash-table))
	(edges nil)
	(nodes nil)
	(count 0))
    (flet ((maybe-new-node (term)
	     (or (gethash term nodes-lookup)
		 (let ((node (make-dag-term-node
			      :uri term
			      :label (if (eq term !owl:Thing)
					 "Thing"
					 (entity-annotation-value term kb !rdfs:label))
			      :parents (unless (eq term start) (parents term kb))
			      :tooltip 
			      (if (#"matches" (uri-full term) "http://snomed.info/.*")
				  (snomed-tree-tooltip *snomed* term)
				  "")
			      :index (incf count))))
		   (setf (gethash term nodes-lookup) node)))))
      (loop for term in (cons start (descendants start kb))
	 for node = (maybe-new-node term)
	 do (map nil #'maybe-new-node (dag-term-node-parents node)))
      (maphash (lambda(term node)
		 (push node nodes)
		 (loop for parent in (dag-term-node-parents node)
		      unless (null (gethash parent nodes-lookup))
		    do (push (make-dag-term-edge :from node :to (gethash parent nodes-lookup)) edges)))
	       nodes-lookup )
      (values nodes edges))))


(defun browse-subclass-tree (title &optional (ont *default-kb*) &key (orientation "BT") (start !owl:Thing))
  (multiple-value-bind (nodes edges) (ontology-subclass-dag ont :start start :include-referencing nil)
      (let ((spec (emit-dagre-d3-javascript-ne nodes edges))
	    (data-path (temp-directory-path (concatenate 'string title ".js"))))
	(with-open-file (out data-path :direction :output :if-exists :supersede)
	  (write-string spec out))
	(show-dag data-path orientation))))

  
;; (parents-tree !<http://snomed.info/id/65172003> s)
