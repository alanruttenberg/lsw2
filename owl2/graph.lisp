(defvar *treeview-depth* 3)
(defvar *want-hrule* t)
(defvar *uri-name-is-uri-hack* nil)
(defvar *inhibit-property-info-in-toolips* nil)
(defvar *toolip-font-size* 11)
(defvar *flush-annotation-value-regexp* "")
(defvar *classtree-preferred-language* nil)
(defvar *uri-name-html* t)
(defvar *annotation-query-reasoner* :none) 
(defvar *super-label* "super")
(defvar *show-logical-in-tooltip* t)

(defvar *built-in-labels* nil)

(defun built-in-labels ()
  (or *built-in-labels* 
      (progn
	(setq *built-in-labels* (make-hash-table))
	(setf (gethash !owl:AnnotationProperty *built-in-labels*) "Annotation Property")
	(setf (gethash !owl:DatatypeProperty *built-in-labels*) "Datatype Property")
	(setf (gethash !owl:ObjectProperty *built-in-labels*) "Object Property")
	*built-in-labels*)))

(defun uri-name-for-node-with-equivalents (uri kb labels &optional do)
  (or (gethash uri (built-in-labels))
      (if (or (search *blankprefix* (uri-full uri))
	      (search "bNode" (uri-full uri)))
	  (or (and labels (gethash uri labels))
	      (format nil "Blank~a" (#"replaceAll" (uri-full uri) "^.*[/#:](?=.)" "")))
	  (let ((equivalents (and do (equivalents uri kb))))
	    (if (and do (> (length equivalents) 1))
		(values 
		 (format nil "[~{~a~^, ~}]" 
			 (loop for uri in equivalents collect 
			      (or (and labels (car (gethash uri labels)))
				  (let ((string (maybe-abbreviate-namespace  (uri-full uri))))
				    (#"replaceAll" string "^.*[/#:](?=.)" "") ))))
		 equivalents)
		(or (and labels (car (gethash uri labels)))
		    (let ((string (maybe-abbreviate-namespace  (uri-full uri))))
		      (#"replaceAll" string "^.*[/#:](?=.)" "")
		      (#"replaceAll" string "^.*[/#:](?=.)" ""))))
	    )
	  )))

(defun clean-label (label &optional (ignore-if-xml nil))
  (let ((cleaned 
	 (and label
	      (#"replaceAll"
	      (#"replaceAll" 
	       (#"replaceAll"
		(#"replaceAll" (#"replaceFirst" (#0"replaceFirst" label "^\\s+" "") "\\s+$" "") "&" "&amp;")
		"<" "&lt;")
		">" "&gt;") "\"" "&quot;"))))
    (when cleaned
      (if (and (> (length cleaned) 35) (char= (char cleaned 0) #\&))
	  (if ignore-if-xml
	      nil
	      cleaned)
	  (if (equal cleaned "") nil cleaned)
	  ))))

(defun rdfs-label (uri kb)
  (or (gethash (if (stringp uri) (make-uri uri) uri) (rdfs-labels kb))
      (list (#"replaceAll" (if (stringp uri) uri (uri-full uri)) ".*[/#]" ""))))
  
  
(defun rdfs-labels (kb &optional (ignore-obsoletes t) force-refresh)
  (or (and (not force-refresh) (v3kb-uri2label kb))
      (flet ((get-labels (clauses)
	       (when ignore-obsoletes
		 (setq clauses (append  clauses (list `(:optional (?uri !owl:deprecated ?dep))))))
	       (or
		(and *classtree-preferred-language*
		     (sparql `(:select (?uri ?label) () ,@clauses
				       (:filter (and (equal (lang ?label) ,*classtree-preferred-language*)
						     (not (isblank ?uri))
						     (not (bound ?dep))))) ;; TODO filter out obsoletes
			     :kb kb :use-reasoner *annotation-query-reasoner*))
		(sparql `(:select (?uri ?label) ()
				  ,@clauses
				  (:filter (and (not (isblank ?uri)) (not (bound ?dep)))))
			:kb kb :use-reasoner *annotation-query-reasoner*))))
	(setf (v3kb-uri2label kb)
	      (loop with table = (make-hash-table)
		 for (uri label) in 
		 (or
		  (get-labels '((?uri !rdfs:label ?label)))
		  (get-labels '((?uri !foaf:name ?label)))
		  (get-labels '((?uri !swan:title ?label))))
		 for clean-label = (clean-label label t)
		 when clean-label do
		   (pushnew clean-label (gethash uri table ) :test 'equalp)
		 finally (return table))))))

(defvar *temp-directory* (pathname-directory (make-temp-file)))

(defun temp-directory-path (fname)
  (namestring (merge-pathnames fname (make-pathname :directory *temp-directory*))))

(defparameter *classtree-treeml-header* 
  "<tree> <declarations><attributeDecl name=\"name\" type=\"String\"/> <attributeDecl name=\"entity\" type=\"String\"/><attributeDecl name=\"tooltip\" type=\"String\"/></declarations>")

(defun asserted-instances (node kb)
  (if (stringp node) (setq node (make-uri node)))
  (sparql `(:select (?i) (:distinct t) (?i !rdf:type ,node)) :use-reasoner :none :kb kb :flatten t))

(defun asserted-subclasses (node kb)
  (if (stringp node) (setq node (make-uri node)))
  (if (equal node !owl:Thing)
      (sparql `(:select (?i) (:distinct t)
			(?i !rdf:type !owl:Class)
			(:optional (?i !rdfs:subClassOf ?j))
			(:filter (and (not (bound ?j)) (not (isblank ?i)))))
			:use-reasoner :none :kb kb :flatten t)
      (sparql `(:select (?i) (:distinct t) (?i !rdfs:subClassOf ,node)) :use-reasoner :none :kb kb :flatten t)))

(defmethod write-classtree-treeml ((kb v3kb) &key
				   root ;; top of the tree to display from
				   (fname (temp-directory-path "kb-treeml.xml")) ;; where to write to
				   include-instances ;; whether to include instances in the tree
				   merge-same ;; whether equivalentClasses / sameIndividual should be collapsed to a single node
				   dont-show ;; a list of nodes that we don't want to display
				   sort-function ;; if supplied, at each level subclasses are sorted top-bottom using this function
				   (use-labels t) ;; use labels vs. fragments
				   max-tree-size ;; don't include more then this many siblings in a branch
				   include-tooltips ;; whether to include tooltips in the saved file
				   (inferred t)) ;; whether subclasses/instances are inferred or asserted. Latter is faster but not necessarily correct
  (let ((unsatisfiable (if inferred (unsatisfiable-classes kb) nil))
	(equivalents-seen (make-hash-table :test 'equalp))
	(labels (and use-labels (rdfs-labels kb)))
	(treekb (if inferred kb (weaken-to-only-subclasses kb 'treekb))))
    (instantiate-reasoner treekb)

    ;; accessors for subclasses and instances of a node. Take care here if we are using inferred or uninferred
    (flet ((satisfiable-children (node)
	     (set-difference (children node treekb) unsatisfiable :test 'eq))
	   (direct-instances-1 (node)
	     (direct-instances node treekb))
	   (maybe-tooltip (kb node)
	     (if include-tooltips
		 (format nil "<attribute name=\"tooltip\" value=\"~a\"/>"
			 (encode-tooltip-for-xml-attribute (tree-tooltip kb node))) ""))
	   (node-name (kb node)
	     (multiple-value-bind (name equivs)
		 (uri-name-for-node-with-equivalents node kb labels (and inferred merge-same))
	       (loop for equiv in equivs do (setf (gethash equiv equivalents-seen) t))
	       name)))
      
      (with-open-file (f fname :direction :output :if-exists :supersede :if-does-not-exist :create)
	
	(write-string *classtree-treeml-header* f)

	;; will be calling this recursively as we descend down from root

	(labels ((subtree (node)
		   ;; unless we don't want to see this node, or we've already included it somewhere in the graph
		   (unless (or (gethash node equivalents-seen) (member node dont-show))

		     (let ((children (satisfiable-children node)))
		       (when sort-function (setq children (funcall sort-function children)))

		       ;; put unsatisfiable classes below !owl:Nothing
		       (when (and (equal node !owl:Thing) unsatisfiable)
			 (setq children (append children (list !owl:Nothing))))


		       (cond

			 ;; no children - subclasses or instances. So we are a leaf.
			 ((and (null children)  (or (not include-instances) (not (direct-instances-1 node))))
			  (format f "<leaf><attribute name=\"name\" value=\"~a\"/><attribute name=\"entity\" value=\"~a\"/>~a</leaf>~%" 
				  (node-name kb node)
				  (uri-full node)
				  (maybe-tooltip kb node)))

			 ;; No subclassses, but there are instances, so we are a branch.
			 ((and (null children) include-instances (direct-instances-1 node))
			  (format f "<branch><attribute name=\"name\" value=\"~a\"/><attribute name=\"entity\" value=\"~a\"/>~a~%" 
				  (node-name kb node)
				  (uri-full node)
				  (maybe-tooltip kb node))
			  
			  (loop for i in (direct-instances-1 node)
			     for count from 0
			     unless (and max-tree-size (> count max-tree-size))
			     do
			     (format f "<leaf><attribute name=\"name\" value=\"~a(i)\"/><attribute name=\"entity\" value=\"~a\"/>~a</leaf>~%" 
				     (uri-name-for-node-with-equivalents i  kb labels  (and inferred merge-same ))
				     (uri-full i)
				     (maybe-tooltip kb i)))
			  
			  (format f "</branch>~%"))


			 ;; subclasses, so we are a branch

			 (t
			  (format f "<branch><attribute name=\"name\" value=\"~a\"/><attribute name=\"entity\" value=\"~a\"/>~a~%"
				  (format nil "~a (~a)" (node-name kb node)
					  (length (satisfiable-children node)))
				  (uri-full node)
				  (maybe-tooltip kb node))

			  (loop for child in children
			     for count from 0
			     unless (and max-tree-size (> count max-tree-size))
			     do (subtree child))

			  (when include-instances
			    (loop for i in (direct-instances-1 node)
			       for count from 0
			       unless (and max-tree-size (> count max-tree-size))
			       do
			       (format f "<leaf><attribute name=\"name\" value=\"~a(i)\"/><attribute name=\"entity\" value=\"~a\"/>~a</leaf>~%" 
				       (uri-name-for-node-with-equivalents i  kb labels  (and inferred merge-same)) (uri-full i)
				       (maybe-tooltip kb i))))
			  
			  (format f "</branch>~%")))))))
	  (subtree (or root !owl:Thing)))
	(write-string  "</tree> " f)
	(values fname treekb)))))

(defun annotation-property-values-or-labels (entity &optional (kb *default-kb*))
  (remove-if
   (lambda(el) (member (car el) `(,!rdfs:comment ,!oboinowl:hasDefinition))) ; these already in comment
   (sparql `(:select (?p ?v) (:distinct t) 
		     (:union
		      ((?p :a !owl:AnnotationProperty)
		       (,(make-uri entity) ?p ?v)
		       (:filter (not (isblank ?v))))
		      ((?p :a !owl:AnnotationProperty)
		       (,(make-uri entity) ?p ?v1)
		       (?v1 !rdfs:label ?v)
		       (:filter (isblank ?v1)))
		      )) :kb kb 
			 :use-reasoner :none)))


(defun make-treeview (file name kb &optional (depth *treeview-depth*))
  (#"setDismissDelay" (#"sharedInstance" 'tooltipmanager) 300000) ;; make the tooltips last longer
  (#"setInitialDelay" (#"sharedInstance" 'tooltipmanager) 50)
  (let ((jpanel (#"demo" 'org.sc.prefuse.TreeView (format nil "file://~a" file) "name" "2" (format nil "~a" (max 3 (classtree-depth kb)))))
	(jframe (new 'JFrame (string-capitalize name))))
    (setq panel jpanel)
    (let ((visualization (#"getVisualization" (#"getComponent" jpanel 0) )))
      (setq vis visualization)
      (#"addControlListener" (#"getComponent" jpanel 0) (make-tooltip-control kb 'tree-tooltip))
      (#"setDefaultFont" (#"get" (#"getAction" visualization "filter") 1) (#"getFont" 'FontLib "Tahoma" 12 12) ) ;smaller font
      (#"run" visualization "filter")
      (#"run" visualization "treeLayout")
      (#"setContentPane" jframe jpanel)
      (#"pack" jframe)
      (#"setVisible" jframe t)
      #+darwin (run-shell-command "osascript -e 'tell application \"/usr/bin/java\"' -e \"activate\" -e \"end tell\"")
      ))
  kb)

(defmethod show-classtree ((url string) &rest stuff)
  (apply 'show-classtree (load-ontology url) stuff))

(defmethod show-classtree ((symbol symbol) &rest stuff)
  (apply 'show-classtree (load-ontology symbol) (append (standard-ontology-classtree-options symbol) stuff )))

(defmethod show-classtree ((kb v3kb) &rest stuff)
  (multiple-value-bind (fname treekb) (apply 'write-classtree-treeml kb stuff)
    (make-treeview fname (string (v3kb-name kb)) treekb (or (getf stuff :depth) *treeview-depth*))))

(defmethod show-propertytree ((symbol symbol) &rest stuff)
  (apply 'show-propertytree (load-ontology symbol) stuff))

(defmethod show-propertytree ((url string) &key root include-instances merge-same (include-annotation-properties t) (use-labels t) (dont-show t))
  (show-propertytree (load-ontology url) :root root :include-instances include-instances :merge-same merge-same
		     :include-annotation-properties include-annotation-properties))

(defmethod show-propertytree ((kb v3kb) &key root include-instances merge-same (include-annotation-properties t) (use-labels t) (dont-show nil))
  (make-treeview (write-propertytree-treeml kb :root root :include-instances include-instances :merge-same merge-same
					    :include-annotation-properties include-annotation-properties :dont-show dont-show) 
		 (string(v3kb-name kb)) kb))


;; http://sourceforge.net/forum/message.php?msg_id=3339297
;; RE: directed layered (hierarchical) layout 
;; 2005-09-14 15:10
;; There's no existing layout that will do this exactly. Some options include hacking VerticalTreeLayout to create a new layout that does what you want, or playing with the ForceDirectedLayout and adding a downstream Action instance that forces nodes to stay within horizontal "bands" depending on the depth level of the node. 
 
;; Hope that helps, 
;; -jeff 

(defun make-tooltip-control (kb tooltip-function &optional
			     (tooltip-ok? (lambda(item) (#"canGetString" item "entity")))
			     (tooltip-arg (lambda(item) (#"getString" item "entity"))))
  (let ((fields (jnew-array "java.lang.String" 1)))
    (jarray-set fields "tooltip"  0)
    (let ((wrapped (new 'tooltipcontrol fields))
	  (interfaces (jclass-all-interfaces 'tooltipcontrol)))
      (jss::jdelegating-interface-implementation  
       (car interfaces)
       wrapped
;;        "getToolTipLocation"
;;        (lambda(event)
;; 	 (new 'Point 15 0))
       "itemEntered" 
       (let ((kb kb))
	 (lambda (item event &aux display)
	   (unless (#"isShiftDown" event)
	     (handler-case 
		 (multiple-value-bind (value errorp)
		     (ignore-errors 
		       (setq display (#"getSource" event))
		       (when  (funcall tooltip-ok? item)
			 (funcall tooltip-function kb (funcall tooltip-arg item))))
		   (and display
			(if (and value (not errorp))
			    (#"setToolTipText" display value)
			    (if errorp
				(#"setToolTipText" display (format nil "Error ~a creating tooltip for ~a" errorp (#"getSourceTuple" item)))))))
	       (condition () nil)
	       ))))
       "itemExited"
       (lambda (item event &aux display)
	 (handler-case 
	     (multiple-value-bind (value errorp)
		 (ignore-errors 
		   (setq display (#"getSource" event)))
	       (and display
		    (if (not errorp)
			(unless (#"isShiftDown" event)
			  (#"setToolTipText" display +null+))
			)))
	   (condition () nil)
	   ))
       ))))

(defun rdfs-comment (entity kb)
  (let ((uri (make-uri entity)))
    (format nil "~{~a~^; ~}" 
	    (or
	     (or 
	      (and *classtree-preferred-language*
		   (sparql `(:select (?comment) () (,uri !oboinowl:hasDefinition ?def) (?def !rdfs:label ?comment) (:filter (and (equal (lang ?comment) ,*classtree-preferred-language*) (not (equal ?comment ""))))) :use-reasoner *annotation-query-reasoner* :kb kb :flatten t))
	      (sparql `(:select (?comment) () (,uri !oboinowl:hasDefinition ?def) (?def !rdfs:label ?comment) (:filter (not (equal ?comment "")))) :use-reasoner *annotation-query-reasoner* :kb kb :flatten t)
	      )
	     (or 
	      (and *classtree-preferred-language*
		   (sparql `(:select (?comment) () (:union ((,uri !obi:OBI_0000291 ?comment))
							   ((,uri !obi:IAO_0000115 ?comment)))
				     (:filter (and (equal (lang ?comment) ,*classtree-preferred-language*)
												  (not (equal ?comment "")))))
			   :kb kb :use-reasoner *annotation-query-reasoner* :flatten t))
	      (sparql `(:select (?comment) () (:union ((,uri !obi:OBI_0000291 ?comment))
							   ((,uri !obi:IAO_0000115 ?comment))) (:filter (not (equal ?comment "")))) :kb kb :use-reasoner *annotation-query-reasoner* :flatten t))
	     (or
	      (and *classtree-preferred-language*
		   (sparql `(:select (?comment) () (,uri !rdfs:comment ?comment) (:filter (and (equal (lang ?comment) ,*classtree-preferred-language*)
											       (not (equal ?comment "")))))
			   :kb kb :use-reasoner *annotation-query-reasoner* :flatten t))
	      (sparql `(:select (?comment) () (,uri !rdfs:comment ?comment) (:filter (not (equal ?comment "")))) :kb kb :use-reasoner *annotation-query-reasoner* :flatten t))
	     ))))

(defun tree-tooltip (kb entity)
  (rdfs-comment entity kb))

(defun encode-tooltip-for-xml-attribute (tooltip)
  (xml-encode-string-with-unicode (#"replaceAll" tooltip "\\n" "<br>")))

(defun uris-for-entity-html (entity kb)
  (format nil "~{~a<br>~}" 
	  (loop for e in (if (eq type :class) 
			     (or (equivalents (make-uri entity) kb) (list (make-uri entity)))
			     (if (member type '(:object-property :datatype-property))
				 (cons (make-uri entity) (same-properties (make-uri entity) kb))
				 (if (or (null type) (eq type :annotation-property))
				     (list (make-uri entity))
				     (cons (make-uri entity) (sames (make-uri entity) kb)))))
	     collect (format nil "~a~a" (if type (concatenate 'string (string-capitalize (string type)) ": ") "") (uri-full e)))
	  ))


(defun uris-for-entity-html (entity kb)
  (format nil "~a<br>" (html-quote (if (uri-p entity) (uri-full entity) entity))))

(defun html-for-class-parents (entity kb)
  (let* ((asserted (sparql `(:select (?parent) ()
				     (,entity !rdfs:subClassOf ?parent)
				     (:filter (not (isblank ?parent))))
			   :kb kb :use-reasoner :none :flatten t))
	 (inferred (set-difference (parents entity kb) asserted))
	 (mi? (> (length asserted) 1)))
    (if (or inferred asserted)
	(format nil "~{~a~^, ~}"
		 (append
		  (mapcar (lambda(c)
			    (if mi?
				(format nil "<font color=\"red\">~a</font>" (car (rdfs-label c kb)) )
				(car (rdfs-label c kb))))
			  (remove-duplicates asserted :test 'equalp))
		  (mapcar (lambda(c) (format nil "<i>~a</i>" (car (rdfs-label c kb)) inferred))
			  (remove-duplicates inferred :test 'equalp))
		  ))
	"<i><font color=\"red\">Thing</font></i>")))

(defun html-for-class (entity kb)
  (let ((parents (html-for-class-parents (make-uri entity) kb)))
    (with-output-to-string (s)
      (unless (equal entity (uri-full !owl:Nothing))
	(format s "<b>~a:  ~a</b><br>" *super-label* parents))
      (loop for sentence in (manchester-logical-axioms-for-class entity kb)
	   do (format s "&nbsp;&nbsp;<span class=\"logical\">~a</span><br>" sentence))
      (let* ((apv (annotation-property-values-or-labels entity kb)))
	(loop for (p val) in apv
	   with need-hrule = t
	   when (and val (not (eq p !rdfs:comment)) (not (eq p !rdfs:label)))
	   unless (and (not (uri-p val)) (#"matches" val *flush-annotation-value-regexp*))
	   do 
	   (let ((p-label (car (rdfs-label (uri-full p) kb))))
	     (when (and need-hrule *want-hrule*) (format s "<hr>") (setq need-hrule nil))
	     (format s "<i>~a</i>: ~a<br>" 
		     p-label
		     (if (uri-p val)
			 (or (car (rdfs-label (uri-full p) kb))
			     val)
			 val))))
	))))  

(defun html-quote (string)
  (and string
       (#"replaceAll" string "<" "&lt;")))

(defvar *workaround-applet-css* t)

(defun tree-tooltip-css ()
  (if *workaround-applet-css*
      ""
      "<head><style TYPE=\"text/css\">
.manchesterKeyword { color: #ee0088;}
.logical {color: #151B8D;}
</style></head>"))

(defun workaround-replace-styles (text)
  (if *workaround-applet-css*
      (progn
	(setq text (#"replaceAll" text "(?s)<span class=\"manchesterKeyword\">(.*?)</span>" "<font color=\"#ee0088\">$1</font>"))
	(#"replaceAll" text "(?s)<span class=\"logical\">(.*?)</span>" "<font color=\"#151b8d\">$1</font>"))
      tex))

(defun tree-tooltip (kb entity &key (width 600) (type :class))
  (setq @ (make-uri entity))
  (let* ((*default-kb* kb)
	 (comment (html-quote (rdfs-comment entity kb)))
	 (labels (rdfs-label entity kb)))
    (setq @@
	  (workaround-replace-styles
	   (format nil "<html>~a<div style=\"font-size:~a;width:~a;height=800\"><b>~a</b>~a<i>~a</i>~a</div></html>"
		   (tree-tooltip-css)
		   *toolip-font-size* width
		   (uris-for-entity-html entity kb)
		   (if labels (format nil "~{<b><i>~a</i></b>~^<br>~}" labels) "")
		   (if (equal entity (uri-full !owl:Nothing)) 
		       (format nil "<br>These classes are unsatisfiable!<br>~{~a~^<br>~}"
			       (mapcar (lambda(e) (format nil "~a (~a)" (car (rdfs-label e kb)) (uri-full e))) (unsatisfiable-classes kb)))
		       (if (and comment  (not (equal comment "")))
			   (format nil "<div style=\"margin-left:5px;margin-top:2px;margin-bottom:2px\">~a</div>" comment)
			   (if labels  "<br>" "")))
		   (cond ((eq type :class)
			  (html-for-class entity kb))
			 ((eq type :individual)
			  (html-for-individual entity kb)
			  )
			 ((member type '(:object-property :datatype-property))
			  (html-for-property entity kb))
			 (t "")))))))

	 
(defun @ ()
  (#"replaceAll" 
   (#"replaceAll"
    (#"replaceAll" 
     (#"replaceAll" (#"replaceAll"
		     (subseq @@ (+ 6 (length (tree-tooltip-css))))
		     "<(\/div|br|span|style|head)>" (load-time-value (concatenate 'string (string #\linefeed) (string #\linefeed)))) "<\\S+?>" "")
     "(<(div|font|span|style|head) .*?>)" "")
    "&\\S+?;" " ")
   "(?s)\\n(\\n)" "$1")
  )

(defun manchester-logical-axioms-for-class (class ont)
  (setq ont (or (v3kb-weakened-from ont) ont))
  (if (stringp class) (setq class (make-uri class)))
  (if (or (not (get-entity class :class ont)) (equal class !owl:Nothing))
      nil
      (remove-duplicates
       (let* ((class-shortform (quote-for-regex (#"getShortForm" (short-form-provider ont) (get-entity class :class ont))))
	      (this-axiom-header (format nil "~a <[^>]*?>SubClassOf<[^>]*?>\\s+" class-shortform))
	      (equivalent-classes-axiom (format nil "~a <[^>]*?>EquivalentTo.*" class-shortform)))
	 (loop for (sentence ax) in (get-rendered-referencing-axioms class :class ont)
	    unless (or (simple-subclassof-axiom? ax)
		       (jinstance-of-p ax (find-java-class 'OWLDeclarationAxiom )))
	    do (setq sentence (#"replaceAll" sentence "\\n" "<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"))
	    (if (#"matches" sentence equivalent-classes-axiom)
		(setq sentence (#"replaceFirst" sentence class-shortform ""))
		(setq sentence (#"replaceFirst" sentence "EquivalentTo" "EquivalentTo<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"))

		)
	    and collect
	    (string-trim " " 
			 (if (#"matches" sentence (format nil "~a.*" this-axiom-header))
			     (#"replaceFirst" sentence this-axiom-header "")
			     (#"replaceFirst" sentence "SubClassOf" "SubClassOf<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;")))))
       :test 'equal)))
	     
       
(defparameter *obo-noise-classes* 
  (list !oboinowl:DbXref !oboinowl:Definition !oboinowl:ObsoleteClass !oboinowl:ObsoleteProperty !oboinowl:Subset !oboinowl:Synonym !oboinowl:SynonymType !protegeowl:PAL-CONSTRAINT !protegeowl:DIRECTED-BINARY-RELATION !protegeowl:TO !protegeowl:FROM !protegeowl:SLOT-CONSTRAINTS !protegeowl:PAL-NAME !protegeowl:PAL-CONSTRAINTS !protegeowl:PAL-STATEMENT !protegeowl:PAL-RANGE !protegeowl:PAL-DESCRIPTION))

(defparameter *obi-noise-classes* 
  (append *obo-noise-classes* (list !obi:OBI_0000449 !obi:OBI_0000233 !obi:OBI_0000683 !obi:OBI_0600065 !snap:Object !snap:ObjectAggregate !snap:FiatObjectPart !snap:SpatialRegion !span:TemporalRegion !span:Process !span:ProcessBoundary !span:ProcessualContext !span:FiatProcessPart !span:ProcessAggregate !span:SpatiotemporalRegion !owl:Nothing !snap:ObjectBoundary !snap:Site )))
