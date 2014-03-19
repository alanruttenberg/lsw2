(in-package :cl-user)

;;; More OOPy version of inspector

(defvar *inspected*)
(defvar *inspector* nil)    ;;; try to keep just one around

;;; wrap this around reflective things that may throw a java exception
(defmacro report-errors (&body body)
  `(handler-case (progn ,@body)
    (error (condition) (format nil "<<Error: ~A>>"
			(princ-to-string condition)))))

(defun oinspect (obj &optional force-new (inspector-class 'inspector-window))
  (if (or (null *inspector*) force-new)
      (setq *inspector* (make-instance inspector-class :object obj))
      (set-object *inspector* obj)))

(defmethod odescribe (object)
  (loop for (field value) in (oinspect-data nil object)
	do (format t "~%~A:~12T~A" field value)))


(defclass inspector (jcomponent)
  ((object :initarg :object)
   header
   fields				;list of pairs (fieldname fieldvalue)
   table
   table-maker
   (history :initform nil)
   ))

(defclass inspector-window (inspector jframe)
  ())

(defmethod initialize-instance :after ((this inspector-window) &key (name "Inspector")) 
  (with-slots (jframe object) this
    (#"setTitle" jframe (tostring this object))))

;;; regular startup should go through here
(defmethod set-object ((this inspector) new-object) 
  (with-slots (object table-maker) this
    (setq object new-object)
    (funcall table-maker)
    (setq *inspected* object)
    ))

(defmethod set-object :after ((this inspector-window) new-object)
  (with-slots (jframe object) this
    (#"setTitle" jframe (tostring this object))
    (#"show" jframe)))  ; necessary for refresh


;;; returns a panel that becomes the contents of the window
(defmethod make-contents ((this inspector))
  (print this)
  (with-slots (object header fields table table-maker history) this
    (let (
  ;;; these vars are used by refresh; hence defined before value is set
	  (table-panel nil)		
	  (panel (new 'javax.swing.JPanel))
	  (layout (new 'java.awt.GridBagLayout))
	  (constraints (new 'java.awt.GridBagConstraints))
	  (remainder (get-java-field 'java.awt.GridBagConstraints "REMAINDER"))
	  )

      (#"setLayout" panel layout)
      (labels ((add-component (comp)
		 (#"setConstraints" layout comp constraints)
		 (#"add" panel comp))
	       (jump-selected ()
		 (multiple-value-bind (field row)
		     (selected-field this)
		   (oojump this field row t))))

	(loop for button in (make-inspector-buttons this)
	     do (add-component button))
	       
  
	;; this code is wrapped in a lambda so refresh can call it
	;; should be a method
	(setq table-maker
	      #'(lambda ()
		  (if table-panel
		      (let ((parent (#"getParent" table-panel)))
			(when parent
			  (#"remove" parent table-panel))))

		  (jss::set-java-field constraints "gridwidth" remainder)
		  (jss::set-java-field constraints "gridheight" remainder)
		  (jss::set-java-field constraints "weightx" 1.0)
		  (jss::set-java-field constraints "weighty" 1.0)
		  (jss::set-java-field constraints "gridx" 0)
		  (jss::set-java-field constraints "gridy" 1)
		  (jss::set-java-field constraints "fill" (get-java-field 'java.awt.GridBagConstraints "BOTH"))


		  (let ((data (oinspect-data this object)))
		    (setq header (car data))
		    (setq fields (cdr data))
		    (setq table (make-table (mapcar #'(lambda (row)
							(mapcar #'(lambda (field)
								    (tostring this field))
								row))
						    fields)
					    header)))
		  (#"addMouseListener" table
				       (jinterface-safe-implementation (jclass "java.awt.event.MouseListener")
								  "mousePressed"
								  #'(lambda (evt)
								      ;(print (selected-field this))
								      (if (= (#"getClickCount" evt) 2)
									  (jump-selected)))))

		  ;; This works, somewhat badly. The right way requires  writing Java code. See here:
		  ;; http://java.sun.com/docs/books/tutorial/uiswing/components/table.html#celltooltip
		  (#"addMouseMotionListener" table
				       (jinterface-safe-implementation (jclass "java.awt.event.MouseMotionListener")
								       "mouseMoved"
								       #'(lambda (evt)
									   (let* ((point (#"getPoint" evt))
										  (col (#"columnAtPoint" table point))
										  (row (#"rowAtPoint" table point))
										  (value (value-at this row col)))
									     (#"setToolTipText" table (html-wrap-string (tostring this value)))))))

		  (setq table-panel (make-table-panel table 350 200))
		  (add-component table-panel)))

	(funcall table-maker)

	

;;;	(inspect-set-top-object obj)
;;;    (inspect-add-to-history obj)


;;     (#"addWindowListener" window
;; 			  (new 'com.ibm.jikes.skij.misc.GenericCallback
;; 			       (lambda (evt)
;; 				 (if (= (#"getID" evt)
;; 					window-activate-event-id)
;; 				     (begin
;; 				      (setq inspect-top-window window)
;; 				      (inspect-set-top-object obj))))))


;;     (if inspect-top-window
;; 	(#"setLocation" window
;; 			(let ((newloc (new 'java.awt.Point (#"getLocation" inspect-top-window))))
;; 			  (#"translate" newloc 22 22)
;; 			  newloc)))

	panel))))

(defmethod make-inspector-buttons ((this inspector))
  (with-slots (table-maker history) this
    (labels ((jump-selected ()
		 (multiple-value-bind (field row)
		     (selected-field this)
		   (oojump this field row t)))
	       (inspect-selected-in-new-window ()
		 (multiple-value-bind (field row)
		     (selected-field this)
		   (oinspect field t))))

      (list
       	 (make-swing-button "Back" #'(lambda (evt)
				       (declare (ignore evt))
				       (if history
					   (oojump this (pop history) nil nil))))
	 (make-swing-button "Inspect" #'(lambda (evt)
					  (declare (ignore evt))
					  (jump-selected)))
	 (make-swing-button "Inspect in new" #'(lambda (evt)
					  (declare (ignore evt))
					  (inspect-selected-in-new-window)))
	 (make-swing-button "set @" #'(lambda (evt)
					  (declare (ignore evt))
					  (setq cl-user::@ (or (selected-field this)
							       (slot-value this 'object)))))
	 (make-swing-button "Refresh" #'(lambda (evt)
					  (declare (ignore evt))
					  (funcall table-maker)
					  ))))))


;;; HTMLize tooltips to wrap if necessary
(defun html-wrap-string (string &optional (wrap-length 60) (wrap-width 350))
  (if (< (length string) wrap-length)
      string
      (format nil "<html><div style=\"width:~a\">~a</div></html>" wrap-width string)))

(defvar *inspect-graph* nil)

;;; instrument to keep history, etc.
;;; +++ needs to be smarter about 3-column rdf, etc.
(defmethod oojump ((inspector inspector) fieldvalue row push)
;;;  (print `(jump fieldvalue ,fieldvalue row ,row))
  (with-slots (object history) inspector
    (let ((old-object object))
    (if push 
	(push object history))
    (set-object inspector fieldvalue)
    (if *inspect-graph*
	(add-link *inspect-graph* old-object (car row) object)
	))))


(defmethod selected-field ((inspector inspector))
  (with-slots (fields table) inspector
    (when fields
      (let* ((whichrow (#"getSelectedRow" table))
	     (whichcolumn (#"getSelectedColumn" table)))
	(when (and (>= whichrow 0) (>= whichcolumn 0))
	  (let* ((row (nth whichrow fields))
		 (column (nth whichcolumn row)))
	    (values column row)))))))


(defmethod value-at ((inspector inspector) row col)
  (with-slots (fields) inspector
  (when (and (>= row 0) (>= col 0))
    (let* ((rowvalues (nth row fields)))
       (nth col rowvalues)))))
      
(defmethod tostring :around ((inspector t) (value t))
  (report-errors
    (call-next-method)))

(defmethod tostring ((inspector t) (v t)) 
  (princ-to-string v))

(defparameter %%java-null (load-time-value +null+))

(defun %java-null (v) 
  (equal v %%java-null))

(defmethod tostring ((inspector t) (v java-object)) 
  (if (%java-null v)
      "null"
      (let ((classname (#"getName" (#"getClass" v))))
	(if (#"matches" classname "java\.lang\.*")
	    (#"toString" v)
	    (format nil "~a (~a)" (prin1-to-string v) (#"getName" (#"getClass" v)))))))

(defmethod oinspect-data ((inspector t) (object t))
;  (inspect-data object)
  (cond ((system:structure-object-p object)
	 (struct-inspect-data object))
	(t 
	 nil)))

;;; will fail on dotted pairs 
(defmethod oinspect-data ((inspector t) (list list))
  (let ((i -1))
    (cons '("Index" "Element")
	  (mapcar #'(lambda (elt)
		      (setq i (+ i 1))
		      (list i elt))
		  list))))

(defmethod oinspect-data ((inspector t) (list list))
  (if (cdr (last list))
      (setq list (append (butlast list) (list (car (last list)) (cdr (last list)))))
      list)
  (let ((i -1))
    (cons '("Element")
	  (mapcar #'(lambda (elt)
		      (list elt))
		  list))))

(defmethod oinspect-data ((inspector t) (vector vector))
  (let ((i -1))
    (cons '("Index" "Element")
	  (map-vector #'(lambda (elt)
			(setq i (+ i 1))
			(list i elt))
		      vector))))

(defmethod oinspect-data ((inspector t) (object standard-object))
  (let* ((class (find-class (type-of object)))
	 (slots (mop::compute-slots class)))
    (cons '("Slot" "Value")
	  (mapcar #'(lambda (slot)
		      (let ((slot-name (mop:slot-definition-name slot)))
			(list (string-downcase slot-name)
			      (if (slot-boundp object slot-name)
				  (slot-value object slot-name)
				  "<Unbound>"
				  ))))
		  slots
		  ))))

;;; Generated data for java objects
;;; +++ should be smarter about enumerations, and some other Java classes
(defmethod oinspect-data ((inspector t) (object java-object))
  (let ((class (jobject-class object)))
    (cond ((#"isArray" class)
	   (inspect-data-jarray object class))
	  ((member "toArray" (jcmn class) :test 'equal)
	   (inspect-data-jset object class))
	  ((member "keySet" (jcmn class) :test 'equal)
	   (inspect-data-map object class))
	  ((member "nextElement" (jcmn class) :test 'equal)
	   (inspect-data-vector object class))
	  (t 
	   (inspect-data-obj object class nil)))))

;      (when (equal class (jclass "java.lang.Class"))
;  	(let ((static-data (inspect-data-obj1 object object t)))
; 	  (mapc #'(lambda (entry)
;  		    (rplaca entry
; 			    (string-append "[static] " (car entry))))
; 		static-data)
; 	  (setq data (nconc static-data data))))



(defmethod oinspect-data ((inspector t) (object hash-table))
  (cons '("Key" "Value")
	(loop for key being the hash-key of object
	      collect (list key (gethash key object)))))



;;; generate inspect data for a java object
(defun inspect-data-obj (object class static?)
  (let ((data '()))
    (flet ((make-entry (name value)
	     (setq data (cons (list name value) data))))
      ;; fields
      (mapc #'(lambda (ivar)
		(if (not (xor static? (member-static? ivar)))
		    (make-entry (#"getName" ivar)
				(#0"get" ivar object))))
					;(invoke class 'getFields)
	    (all-fields class))
      ;; getter methods
      (for-vector #'(lambda (method)
;		      (print (#"getName" method))
		      (if (not (xor static? (member-static? method)))
			  (let ((name (#"getName" method)))
			    (if (and (>= (length name) 3)
				     (or (equal (subseq name 0 3) "get")
					 (equal (subseq name 0 2) "is"))
				     (zerop (length (#"getParameterTypes" method))))
				(make-entry
				 (string-append name "( )")
				 (report-errors 
				   (#0"invoke" method object no-args))
				 )))))
		  (#"getMethods" class))
      (setq data (sort data #'string-lessp :key #'car))
      (cons '("Member" "Value")
	    data))))

(defun inspect-data-jarray (array class)
  (cons '("Index" "Element")
	(loop for i from 0 to (1- (jarray-length array))
	      collect (list i (jarray-ref array i)))))

(defun inspect-data-jset (set class)
  (list* (list (#"getName" (#"getClass" set)))
	 (let ((array (#0"toArray" set)))
	   (loop for i from 0 to (1- (jarray-length array))
	      collect (list (jarray-ref array i))))))

(defun inspect-data-map (map class)
  (let ((keys (set-to-list (#"keySet" map))))
    (list* (list "key" "value")
	   (loop for key in keys
	      collect (list key (#"get" map key))))))

(defun inspect-data-vector (it class)
  (list* (list "value")
	 (mapcar 'list (vector-to-list it))))
  
(defparameter no-args (jnew-array "java.lang.Object" 0))

;;; new improved...
;;; +++ memoize
(defun all-fields (class) 
  (if t ; +++ java2?
      (let ((basefields (vector->list (#"getDeclaredFields" class)))
	    (superclass  (#"getSuperclass" class)))
	(map nil (lambda (f) (#"setAccessible" f t)) basefields)
	(if '(%%null? superclass)
	    basefields
	    (merge-fields basefields
			  (all-fields superclass))))
      (vector->list (#"getFields" class))))
    
(defun struct-inspect-data (struct)
  (let* ((type (type-of struct))
	 (def (get type 'system::structure-definition))
	 (slot-defs (svref def 13));+++ warning magic number
	 (result '()))		
    (dolist (sd slot-defs (cons '("Slot" "Value") (nreverse result)))
      (let ((slot-name (system::dsd-name sd))
	    (slot-value (funcall (system::dsd-reader sd) struct)))
	(push (list slot-name slot-value) result)))))
	

