;; looks correct
(defun parents-tree (x &optional (ont snomed) (table (make-hash-table :test 'equalp)))
  (let ((child (list x (if (eq x !owl:Thing) "Thing" (entity-annotation-value x ont !rdfs:label))))
	(parents (mapcar
		  (lambda(c)
		    (if (eq c !owl:Thing)
			(list c "Thing")
			(list c (entity-annotation-value c ont !rdfs:label))))
		  (parents x ont))))
    (if parents
	(loop for parent in parents 
	   for pt = (or (gethash parent table) (setf (gethash parent table) (parents-tree (car parent) ont table)))
	   do  (setf (gethash pt table) (or (gethash pt table) pt))
	   collect (list* child  (gethash pt table))
	     )
	(list (list child ))
	)))
;; (parents-tree !<http://snomed.info/id/65172003> s)
       
(defstruct term-node
  uri
  label
  level
  index
  tooltip
  parents)

(defstruct (term-edge (:print-function print-edge))
  from
  to
  level
  )

(defun print-edge (edge stream x)
  (format stream "~a@~a -> ~a@~a" (term-node-label (term-edge-from edge)) (term-node-level (term-edge-from edge))
	  (term-node-label (term-edge-to edge)) (term-node-level (term-edge-to edge))))

(defun levelize (trees kb &optional (level 0) last (table (make-hash-table :test 'equal)) (nodes (make-array 100 :adjustable t :fill-pointer 0)) edges)
  (loop for tree in trees 
     do (let ((treetop
	       (or (gethash (caar tree) table)
		   (let* ((new (make-term-node
				:uri (caar tree)
				:label (if (eq (caar tree) !owl:Thing)
					   "Thing"
					   (entity-annotation-value (caar tree) kb !rdfs:label))
				:parents (rest tree)
				:tooltip (if nil;(eq (caar tree) !<http://snomed.info/id/64572001>)
					     "Not this time"
					     (tree-tooltip kb (caar tree) :include-referencing nil)))))
		     (setf (term-node-index new) (vector-push-extend new nodes))
		     new))))
	  (when last
	    (pushnew (make-term-edge :from last :to treetop) edges :test 'equalp))
	  (setf (gethash (caar tree) table) treetop)
	  (setf (term-node-level treetop) level)
	  (multiple-value-setq (nodes edges) (levelize (rest tree) kb (1+ level) treetop table nodes edges))))
  (values nodes edges))
	 
(defun emit-javascript (trees kb)
  (multiple-value-bind (nodes edges) (levelize trees kb)
    (with-output-to-string (s)
      (loop for node across nodes
	 for label = (#"replaceFirst" (term-node-label node) " \\(.*" "")
	 for level = (term-node-level node)
	 for id = (term-node-index node)
	 do 
	   (format s "nodes.push({id:~a, label: ~s, level: ~a});~%" id label level)
	   )
      (loop for edge in edges
	 for from = (term-node-index (term-edge-from edge))
	 for to = (term-node-index (term-edge-to edge))
	 do
	   (format s "edges.push({from: ~a, to: ~a});~%" from to)))))

(defun emit-dagre-d3-javascript (trees kb)
  (multiple-value-bind (nodes edges) (levelize trees kb)
    (with-output-to-string (s)
      (loop for node across nodes
	 for label = (#"replaceFirst" (term-node-label node) " \\(.*" "")
	 for level = (term-node-level node)
	 for id = (term-node-index node)
	   for tooltip = (term-node-tooltip node)
	 do 
	   (format s "g.setNode(~a,  { label: ~s , tip: ~s});~%"  id label tooltip)
	  
	   )
      (loop for edge in edges
	 for from = (term-node-index (term-edge-from edge))
	 for to = (term-node-index (term-edge-to edge))
	 do
	   (format s "g.setEdge(~a, ~a,  {lineInterpolate: 'basis'} );~%" from to)))))

(defun browse-parent-hierarchy (term ont)
  (let ((trees (parents-tree term ont)))
    (let ((spec (emit-dagre-d3-javascript trees ont))
	  (data-path (temp-directory-path "data.js")))
      (print data-path)
      (with-open-file (out data-path :direction :output :if-exists :supersede)
	(write-string "function initialize_data(g){" out )
	(write-string spec out)
	(write-string "}; window.initialize_data = initialize_data;" out))
      (browse-url (format nil "file:///Users/alanr/repos/lsw2git/graph-dag/dagre-d3/dagre3d-template.html?setup=file://~a" data-path) "safari"))))

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

g.nodes().forEach(function(v) {
  var node = g.node(v);
  // Round the corners of the nodes
  node.rx = node.ry = 5;
});

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
