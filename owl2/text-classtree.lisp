;; Build the structure, print it - indented according to depth in tree

(defclass text-tree-node ()
  ((term :initarg :term :reader text-tree-node-term)
   (ontology :initarg :ontology :accessor text-tree-node-ontology)
   (children :initarg :children :accessor text-tree-node-children :initform nil)
   (indent-string :initarg :indent-string :accessor indent-string :initform (string #\space ))
   (indent-increment :initarg :indent-increment :accessor indent-increment :initform 2 )))

(defun create-text-tree-structure (ontology class &optional parent (indent-string (string #\space)) (indent-increment 1))
  (let ((node (make-instance 'text-tree-node :term class :ontology ontology :indent-string indent-string
			     :indent-increment indent-increment)))
    (when parent 
      (pushnew node (text-tree-node-children parent) :key 'text-tree-node-term))
    (when (children class ontology)
      (loop for child in (children class ontology)
	 do (create-text-tree-structure ontology child node indent-string)))
    node))

(defmethod print-text-tree ((node text-tree-node) &optional (indent 0))
  (print-one-node node indent)
  (if (text-tree-node-children node)
      (loop for child in (text-tree-node-children node) do (print-text-tree child (+ indent (indent-increment node))))))

(defmethod print-one-node  ((node text-tree-node) &optional (indent 0))
  (loop repeat indent do (write-string (indent-string node)))
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


    
  
