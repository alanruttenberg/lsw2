(in-package :cl-user)

;;; Experiments with prefuse  

;;; TODO: edge labels, requires decorator, see here:
; https://sourceforge.net/forum/message.php?msg_id=3649406
;http://goosebumps4all.net/34all/bb/attachment.php?aid=219
;http://prefuse.org/gallery/treeview/TreeView.java

(defclass prefuse-graph (jframe)
  (display
   visualization
   (graph :initarg :graph :initform (make-graph t))
  (root :initarg :root :initform nil)
  (layout :initarg :layout)
  (layout-instance :initarg :layout-instance :accessor layout-instance)
  (continuous-layout :initarg :continuous-layout :initform t)
  (obj-node-assoc :initarg :obj-node-assoc :initform '())
  (controls :initarg :controls :initform '())
  (force-parameters :initarg :force-parameters :initform nil)
  (fit-to-window :initarg :fit-to-window :initform t)
  ))

(defmethod make-contents ((this prefuse-graph))
  (with-slots (display visualization graph root layout continuous-layout controls layout-instance force-parameters fit-to-window) this
    (let* ((nodes "graph.nodes")
	   (edges "graph.edges")
	   (tr (new 'LabelRenderer))
	   (draw (new 'ActionList))
	   fill animate
	   (g (make-random-graph 100 100))
	   (draw (new 'ActionList))
	   )
      (setq visualization (new 'prefuse.Visualization)
	    display (new 'Display visualization))
      (#"setSize" display 500 500)

      (#"setRoundedCorner" tr 8 8)
      (#"setRendererFactory" visualization (new 'DefaultRendererFactory tr))

      ;; set graph, set up listeners
      (#"removeGroup" visualization "graph")
      (#"addGraph" visualization "graph" graph) ;returns a VisualGraph

      (setq fill (new 'ColorAction nodes (visualitem "FILLCOLOR") (make-color 200 200 255)))
      (#"add" fill (visualitem "FIXED") (make-color 255 100 100))
      (#"add" fill (visualitem "HIGHLIGHT") (make-color 255 200 125))

      (#"add" draw fill)
      (#"add" draw (new 'ColorAction nodes (visualitem "STROKECOLOR") 0))
      (#"add" draw (new 'ColorAction nodes (visualitem "TEXTCOLOR") (make-color 0 0 0)))
      (#"add" draw (new 'ColorAction edges (visualitem "TEXTCOLOR") (make-color 0 0 0)))
      (#"add" draw (new 'ColorAction edges (visualitem "FILLCOLOR") (make-color 200 200 200)))
      (#"add" draw (new 'ColorAction edges (visualitem "STROKECOLOR") (make-color 200 200 100)))

      (setq animate (new 'ActionList (make-long  (or (and (numberp continuous-layout) (* continuous-layout 1000))
						     (get-java-field 'Activity "INFINITY")))))
					;      (#"setStepTime" animate (make-immediate-object 150 :long))

      (setq layout-instance (new (or layout 'ForceDirectedLayout) "graph"))
      (when force-parameters
	(loop for (force parameter value) in force-parameters
	   do (set-force-parameter this force parameter value)))
      (when (not fit-to-window) (#"setAutoScale" layout-instance nil))

      (let  ((repaint (new 'RepaintAction)))
					;(when root (#"setLayoutRoot" layout-instance root))
	(#"add" animate layout-instance)

	(#"add" animate fill)
	(#"add" animate repaint)

	(#"putAction" visualization "draw" draw)
	(#"putAction" visualization "layout" animate)
	(when continuous-layout
	  (#"runAfter" visualization "draw" "layout"))

	;; Controls - for now, the standard ones from GraphView
	(#"addControlListener" display (new 'FocusControl 1))
	(#"addControlListener" display (new 'DragControl))
	(#"addControlListener" display (new 'PanControl))
	(#"addControlListener" display (new 'ZoomControl))
	(#"addControlListener" display (new 'WheelZoomControl))
	(#"addControlListener" display (new 'ZoomToFitControl))
	(#"addControlListener" display (new 'NeighborHighlightControl))

	;; and for good measure
	(let ((listener (jinterface-safe-implementation ; there's something wrong with safe version
			 (find-java-class "prefuse.controls.Control")
			 "isEnabled"
			 #'(lambda (&rest whatever)
			     +true+ )
			 "itemClicked"
			 #'(lambda (node-item mouse-event)
			     (let ((node (#"getSourceTuple" node-item)))
			       (node-clicked this node mouse-event)))
			 "itemEntered"
			 #'(lambda(e event)
			     (#"run" repaint)
			     (#"run" draw))
			 "itemExited"
			 #'(lambda(e event)
			     (#"run" repaint)
			     (#"run" draw))
			 )))
	  (#"addControlListener" display listener))
	(if controls
	    (loop for control in controls do
		 (#"addControlListener" display control))
	    (#"addControlListener" display (new 'ToolTipControl "tooltip")))
	;; set things going!
	(#"run" visualization "draw")
	(unless continuous-layout
	  (#"run" layout-instance)
	  (#"addComponentListener" display
				   (jinterface-safe-implementation
				    (find-java-class 'java.awt.event.ComponentListener)
				    "componentResized"
				    #'(lambda(e)
					(#"run" layout-instance)
					(#"run" repaint)
					)
				    )
				   )
	  ))
      display
      )))

;;; default handler, override to be useful
(defmethod node-clicked ((this prefuse-graph) node mouse-event)
  (print `(click on ,node))
  )

(defmethod validate ((this prefuse-graph))
  (with-slots (visualization) this
    (#"run" visualization "draw")    ))

(defclass inspect-graph (prefuse-graph)
  ()
  (:default-initargs :name "Inspect Graph")
  )

(defmethod make-contents :after ((this inspect-graph))
  (setq *inspect-graph* this)
  (with-slots (graph) this
    ;; this isn't used, because ABCL cant pass arbitrary objects.
;    (#"addColumn" graph "lispobject" (jclass "java.lang.Object"))
    (#"addColumn" graph "tooltip" (jclass "java.lang.String"))))


(defmethod add-object ((this inspect-graph) object)
  (with-slots (graph obj-node-assoc) this
    (if (assoc object obj-node-assoc)
	(#"getNode" graph (cdr (assoc object obj-node-assoc)))  ;;; need aif
    (let ((node (#"addNode" graph)))
      (#"setString" node "label" (object-short-name this object))
      (#"setString" node "tooltip" (princ-to-string object))
;;; ABCL does not support this, dammit.
;      (#0"set" node "lispobject" object)
      ;; and nodes don't seem to preserve identity, so use their index
      (push (cons object (#"getRow" node)) obj-node-assoc)
      (validate this)
      node))))

(defparameter *label-string-limit* 50)

(defmethod object-short-name ((this inspect-graph) (object t))
  (let ((string (tostring this object)))
    (if (> (length string) *label-string-limit*)
	(concatenate 'string (subseq string 0 *label-string-limit*) "...")
	string)))

;;; +++ fold this into tostring
(defmethod object-short-name ((this inspect-graph) (object uri))
  (uri-name-for-node object))

(defmethod add-link ((this inspect-graph) from edge to)
  (with-slots (graph) this
  (let ((from-node (add-object this from))
	(to-node (add-object this to)))
    (#"addEdge" graph from-node to-node)
    ;; +++ figure out how to label!
    )))
    

(defmethod node-clicked ((this prefuse-graph) node mouse-event)
  ; (#0"get" node "lispobject")))
  (with-slots (obj-node-assoc) this
    (let ((object (car (rassoc (#"getRow" node) obj-node-assoc))))
      (if (functionp object)
	  (funcall object)
	  (oinspect object)))))

;;; +++ hook up to a button when/if there is a more elaborate UI
(defmethod clear ((this prefuse-graph)) 
  (with-slots (graph) this
    (#"clear" graph)
    (validate this)))


;;; UGLY!
(defun make-long (val)
  (new 'Long (princ-to-string val)))

(defun visualitem (name)
  (get-java-field 'VisualItem name))

(defun make-color (r g b)
  (#"rgb" 'ColorLib r g b))

(defun make-graph (&optional directed?)
  (let ((g (new 'prefuse.data.Graph directed?)))
    (#"addColumns" (#"getNodeTable" g)
		   (get-java-field 'prefuse.util.GraphLib "LABEL_SCHEMA"))
    ;		g.addColumn("rdftype", String.class);
    g))

(defun make-random-graph (n m)
  (let ((g (make-graph)))
    (dotimes (i n)
      (let ((node (#"addNode" g)))
	(#"setString" node "label" (princ-to-string i))))
    (dotimes (i m)
      (let ((from (#"getNode" g (random n)))
	    (to (#"getNode" g (random n))))
	(#"addEdge" g from to)))
    g))

    
;;; add a node and edge to a graph dynamically
(defun add-node (g label v from)
  (let ((node (#"addNode" g)))
    (#"setString" node "label" label)
    (if from
	(#"addEdge" g from node))
    (#"run" v "draw")
    node))


(defun kill-prefuse-threads ()
  (loop for thread in (all-threads)
	if (search "prefuse" (#"getName" thread))
	do (#"stop" thread)))

(defmethod force-parameters ((i prefuse-graph))
  (loop for force across 
       (#"getForces" (#"getForceSimulator" (layout-instance i)))
       append
       (loop for i below (length (#"getParameterNames" force))
	  for param in (map 'list #"toString" (#"getParameterNames" force))
	  collect (list (intern (string-upcase (#"replaceAll" (#"toString" force) "(.*\\.)(.+?)(@.*)" "$2")) 'keyword)
			(intern (String-upcase param) 'keyword) (#"getParameter" force i)))))

(defmethod get-force-parameter ((g prefuse-graph) force parameter &optional set)
  (loop for forcei across
       (#"getForces" (#"getForceSimulator" (layout-instance g)))
       for force-name = (intern (string-upcase (#"replaceAll" (#"toString" forcei) "(.*\\.)(.+?)(@.*)" "$2")) 'keyword)
       do
       (when (eq force-name force)
	 (loop for i below (length (#"getParameterNames" forcei))
	    for param in (map 'list #"toString" (#"getParameterNames" forcei))
	    for param-name = (intern (String-upcase param) 'keyword)
	    when (eq param-name parameter)
	    do
	      
	      (return-from get-force-parameter
		(prog1
		    (#"getParameter" forcei i)
		  (when set
		    (#"setParameter" forcei i set))))))))

(defmethod set-force-parameter ((g prefuse-graph) force parameter value)
  (get-force-parameter g force parameter value))
