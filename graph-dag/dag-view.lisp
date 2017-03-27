(defparameter *dagre-template-url* (concatenate 'string
					  "file://"
					  (namestring (truename "graph-dag:dagre-d3;dagre3d-template.html"))))

(defstruct dag-term-node
  uri
  label
  level
  index
  tooltip
  parents)

(defstruct (dag-term-edge (:print-function print-edge))
  from
  to
  level
  )

(defun print-edge (edge stream x)
  (format stream "~a@~a -> ~a@~a" (dag-term-node-label (dag-term-edge-from edge)) (dag-term-node-level (dag-term-edge-from edge))
	  (dag-term-node-label (dag-term-edge-to edge)) (dag-term-node-level (dag-term-edge-to edge))))

(defun levelize (trees kb &optional (level 0) last (table (make-hash-table :test 'equal)) (nodes (make-array 100 :adjustable t :fill-pointer 0)) edges)
  (loop for tree in trees 
     do (let ((treetop
	       (or (gethash (caar tree) table)
		   (let* ((new (make-dag-term-node
				:uri (caar tree)
				:label (if (eq (caar tree) !owl:Thing)
					   "Thing"
					   (entity-annotation-value (caar tree) kb !rdfs:label))
				:parents (rest tree)
				:tooltip (snomed-tree-tooltip kb (caar tree) :include-referencing nil)
				)))
		     (setf (dag-term-node-index new) (vector-push-extend new nodes))
		     new))))
	  (when last
	    (pushnew (make-dag-term-edge :from last :to treetop) edges :test 'equalp))
	  (setf (gethash (caar tree) table) treetop)
	  (setf (dag-term-node-level treetop) level)
	  (multiple-value-setq (nodes edges) (levelize (rest tree) kb (1+ level) treetop table nodes edges))))
  (values nodes edges))
	 
(defun emit-javascript (trees kb &optional (stream t) (label-format-fn 'remove-parenthetical))
  (multiple-value-bind (nodes edges) (levelize trees kb)
    (with-output-to-string (s)
      (loop for node across nodes
	 for label = (funcall (or label-format-fn 'identity) (dag-term-node-label node))
	 for level = (dag-term-node-level node)
	 for id = (dag-term-node-index node)
	 do 
	   (format s "nodes.push({id:~a, label: ~s, level: ~a});~%" id label level)
	   )
      (loop for edge in edges
	 for from = (dag-term-node-index (dag-term-edge-from edge))
	 for to = (dag-term-node-index (dag-term-edge-to edge))
	 do
	   (format s "edges.push({from: ~a, to: ~a});~%" from to))
    )))

(defun emit-dagre-d3-javascript (trees kb &optional (label-format-fn 'remove-parenthetical) &key)
  (multiple-value-bind (nodes edges) (levelize trees kb)
    (with-output-to-string (s)
      (write-string "function initialize_data(g){" s )
      (loop for node across nodes
	 for label = (funcall (or label-format-fn 'identity) (dag-term-node-label node))
	 for id = (dag-term-node-index node)
	 for tooltip = (dag-term-node-tooltip node)
	 do 
	   (format s "g.setNode(~a,  { label: ~s , tip: ~s});~%"  id label tooltip)
	  
	   )
      (loop for edge in edges
	 for from = (dag-term-node-index (dag-term-edge-from edge))
	 for to = (dag-term-node-index (dag-term-edge-to edge))
	 do
	   (format s "g.setEdge(~a, ~a,  {lineInterpolate: 'basis'} );~%" from to))
      (write-string "} window.intialize_data=initialize_data;" s )
      )))

(defun emit-dagre-d3-javascript-ne (nodes edges &optional (label-format-fn 'remove-parenthetical) )
  (with-output-to-string (s)
      (write-string "function initialize_data(g){" s )
      (loop for node in nodes
;	 for dummy = 	   (print-db node)
	 for label =  (funcall (or label-format-fn 'identity) (dag-term-node-label node))
	 for id = (dag-term-node-index node)
	 for tooltip = "";(dag-term-node-tooltip node)
	 with props = nil
	 do
	   (format s "g.setNode(~a,  { label: ~s , tip: ~s ~{,~a:~s~} });~%"  id label tooltip (mapcar 'car props) (mapcar 'cdr props))
	  
	   )
      (loop for edge in edges
;	   for dummy = 	   (print-db edge)
	 for from = (dag-term-node-index (dag-term-edge-from edge))
	 for to = (dag-term-node-index (dag-term-edge-to edge))
	 do

	   (format s "g.setEdge(~a, ~a,  {lineInterpolate: 'basis'} );~%" from to))
      (write-string "} window.intialize_data=initialize_data;" s )
      ))

(defun show-dag (datajs &optional orientation)
  (BROWSE-URL (concatenate 'string *dagre-template-url* "?setup=file://" datajs (if orientation (concatenate 'string "&orientation=" orientation) ""))))

;{ style: "fill: #afa" });
  
#| This for dagre-3d
// Here we"re setting nodeclass, which is used by our custom drawNodes function
// below.
g.setNode(0,  { label: "TOP",       class: "type-TOP" });
g.setNode(1,  { label: "S",         class: "type-S" });
g.setNode(2,  { label: "NP",        class: "type-NP" });
g.setNode(3,  { label: "DT",        class: "type-DT" });
g.setNode(4,  { label: "This",      class: "type-TK" });
g.setNode(5,  { label: "VP",        class: "type-VP" });
g.setNode(6,  { label: "VBZ",       class: "type-VBZ" });
g.setNode(7,  { label: "is",        class: "type-TK" });
g.setNode(8,  { label: "NP",        class: "type-NP" });
g.setNode(9,  { label: "DT",        class: "type-DT" });
g.setNode(10, { label: "an",        class: "type-TK" });
g.setNode(11, { label: "NN",        class: "type-NN" });
g.setNode(12, { label: "example",   class: "type-TK" });
g.setNode(13, { label: ".",         class: "type-." });
g.setNode(14, { label: "sentence",  class: "type-TK" });

// Set up edges, no special attributes.
g.setEdge(3, 4);
g.setEdge(2, 3);
g.setEdge(1, 2);
g.setEdge(6, 7);
g.setEdge(5, 6);
g.setEdge(9, 10);
g.setEdge(8, 9);
g.setEdge(11,12);
g.setEdge(8, 11);
g.setEdge(5, 8);
g.setEdge(1, 5);
g.setEdge(13,14);
g.setEdge(1, 13);
g.setEdge(0, 1)

	       

Generate this for vis:

// randomly create some nodes and edges
            for (var i = 0; i < 15; i++) {
                nodes.push({id: i, label: String(i)});
            }
            edges.push({from: 0, to: 1});
            edges.push({from: 0, to: 6});
            edges.push({from: 0, to: 13});
            edges.push({from: 0, to: 11});
            edges.push({from: 1, to: 2});
            edges.push({from: 2, to: 3});
            edges.push({from: 2, to: 4});
            edges.push({from: 3, to: 5});
            edges.push({from: 1, to: 10});
            edges.push({from: 1, to: 7});
            edges.push({from: 2, to: 8});
            edges.push({from: 2, to: 9});
            edges.push({from: 3, to: 14});
            edges.push({from: 1, to: 12});
            nodes[0]["level"] = 0;
            nodes[1]["level"] = 1;
            nodes[2]["level"] = 3;
            nodes[3]["level"] = 4;
            nodes[4]["level"] = 4;
            nodes[5]["level"] = 5;
            nodes[6]["level"] = 1;
            nodes[7]["level"] = 2;
            nodes[8]["level"] = 4;
            nodes[9]["level"] = 4;
            nodes[10]["level"] = 2;
            nodes[11]["level"] = 1;
            nodes[12]["level"] = 2;
            nodes[13]["level"] = 1;
            nodes[14]["level"] = 5;

|#
