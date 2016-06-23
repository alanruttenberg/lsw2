;; looks correct
(defun parents-tree (x &optional kb (table (make-hash-table :test 'equalp)))
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
;; (parents-tree !<http://snomed.info/id/65172003> s)
