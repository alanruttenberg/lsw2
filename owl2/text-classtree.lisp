;; Build the structure, print it - indented according to depth in tree

(defclass text-tree-node ()
  ((term :initarg :term :reader text-tree-node-term)
   (ontology :initarg :ontology :accessor text-tree-node-ontology)
   (children :initarg :children :accessor text-tree-node-children :initform nil)))

(defun create-text-tree-structure (ontology class &optional parent)
  (let ((node (make-instance 'text-tree-node :term class :ontology ontology)))
    (when parent 
      (pushnew node (text-tree-node-children parent) :key 'text-tree-node-term))
    (when (children class ontology)
      (loop for child in (children class ontology)
	 do (create-text-tree-structure ontology child node)))
    node))

(defmethod print-text-tree ((node text-tree-node) &optional (indent 0))
  (print-one-node node indent)
  (if (text-tree-node-children node)
      (loop for child in (text-tree-node-children node) do (print-text-tree child (+ indent 2)))))

(defmethod print-one-node  ((node text-tree-node) &optional (indent 0))
  (loop repeat indent do (write-char #\space))
  (write-string (car (rdfs-label (text-tree-node-term node) (text-tree-node-ontology node))))
  (terpri))

  
(defun test ()
  (with-ontology test (:collecting t)
      ((asq 
	(declaration (class !a))
	(declaration (class !b))
	(declaration (class !c))
	(declaration (class !d))
	(sub-class-of !b !a)
	(sub-class-of !c !b)
	(sub-class-of !d !b)
	))
    (print-text-tree (create-text-tree-structure test !a))))


    
  
