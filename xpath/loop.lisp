(in-package :loop)

;; loop iteration path for iterating over nodeset return values from
;; java xpath results two versions: nodeset-nodes and
;; cloned-nodeset-nodes. In the latter case, the node is is cloned
;; before passing back, which at least for large files and small nodes
;; is enourmously faster for some reason.

;;(loop for el being the cloned-nodeset-nodes of (xpath:query pdbxml "//PDBx:struct_ref")
;;   collect  (xpath:elements el "PDBx:db_code" "PDBx:pdbx_db_accession" "PDBx:db_name"))

;;(loop for el being the nodeset-nodes of (xpath:query pdbxml "//PDBx:struct_ref")
;;   collect  (xpath:elements el "PDBx:db_code" "PDBx:pdbx_db_accession" "PDBx:db_name"))

(defun loop-xpath-nodeset-iteration-path (variable data-type prep-phrases &key clone-p)
  (cond ((and prep-phrases (cdr prep-phrases))
	 (loop-error "Too many prepositions!"))
        ((and prep-phrases (not (member (caar prep-phrases) '(:of))))
         (loop-error "Unknown preposition ~S." (caar prep-phrases))))
  (unless (symbolp variable)
    (loop-error "Destructuring is not valid for nodeset-node iteration."))
  (let ((nodes (gensym "LOOP-NODES-"))
	(node-index-var (gensym "LOOP-ITEM-"))
	(node-index-max (gensym "LOOP-ITEM-MAX-"))
	(variable (or variable (gensym "LOOP-NODESET-VAR-"))))
    (push `(let* ((,nodes ,(cadar prep-phrases)) (,node-index-var -1) (,node-index-max (#"getLength" ,nodes))))
	  *loop-wrappers*)
    (push `(ignorable ,(loop-when-it-var)) *loop-declarations*)
    `(((,variable nil ,data-type))
      ()
      ()
      ()
      (not (multiple-value-setq (,(loop-when-it-var)
				  ,variable)
	     (let ((in (< (incf ,node-index-var) ,node-index-max)))
	       (values in
		       ,(if clone-p
			    `(and in (#"cloneNode" (#"item" ,nodes ,node-index-var) t))
			    `(and in (#"item" ,nodes ,node-index-var))
			    )))))
      ())))

(add-loop-path '(nodeset-nodes) 'loop-xpath-nodeset-iteration-path *loop-ansi-universe*
	       :preposition-groups '((:of))
	       :inclusive-permitted nil
	       :user-data '(:clone-p nil))

(add-loop-path '(cloned-nodeset-nodes) 'loop-xpath-nodeset-iteration-path *loop-ansi-universe*
	       :preposition-groups '((:of))
	       :inclusive-permitted nil
	       :user-data '(:clone-p t))