(in-package :cl-user)

(defun make-swing-button (name action)
  (let ((b (new 'javax.swing.JButton name))
	(listener (jinterface-safe-implementation
		   (jclass "java.awt.event.ActionListener")
		   "actionPerformed"
		   #'(lambda (evt)
		       (funcall action evt)))))
    (#"addActionListener" b listener)
    b))

(defvar *empty-jarray* (load-time-value (jnew-array (find-java-class 'java.lang.object) 0 0)))
(defun make-table (data columns)
  (let ((data (if data (lists->jarray data) *empty-jarray*))
	(columns (if columns (list->jvector columns) *empty-jarray*)))
    (new 'javax.swing.JTable data columns)))


(defun make-table-panel (table width height)
  (let ((panel (new 'javax.swing.JPanel))
	(scroller (new 'javax.swing.JScrollPane table)))
    (#"setPreferredScrollableViewportSize" 
     table 
     (new 'java.awt.Dimension width height))
    (#"setLayout" panel (new 'java.awt.GridLayout 1 1))
    (#"add" panel scroller)
    panel))

(defun make-swing-window-for-panel (name panel)
  (let* ((w (new 'javax.swing.JFrame name))
	 (content-pane (#"getContentPane" w)))
    (#"add" content-pane panel)
    (#"pack" w)
    w))

;;; given a list of lists, make a 2D array. Assumes all inner lists have the same length
(defun lists->jarray (lists)
  (let* ((wid (length (car lists)))
	 (len (length lists))
	 (array (jnew-array "java.lang.Object" len wid))
	 (i 0))
    (mapc #'(lambda (lst)
	      (let ((j 0))
		(mapc #'(lambda (elt)
			  (setf (jarray-ref array i j) elt)
			  (setq j (+ 1 j)))
		      lst)
		(setq i (+ 1 i))))
	  lists)
    array))

(defun list->jvector (list &optional (type "java.lang.Object"))
  (let* ((wid (length list))
	 (array (jnew-array type wid))
	 (i 0))
    (mapc #'(lambda (elt)
	      (setf (jarray-ref array i) elt)
	      (setq i (+ 1 i)))
	  list)
    array))

(defun jvector->list (jvec)
  (loop for i from 0 to (1- (jarray-length jvec))
	collect (jarray-ref jvec i)))

;;; debug
(defun array-test ()
  (let ((array (jnew-array "java.lang.Object" 5 5 )))
    (jarray-set array 23 0 0)
    (jarray-set array "foo" 0 1)
    (jarray-set array (new 'java.awt.Frame) 0 2)
    (jarray-set array #(1 2 3) 0 3)
    (jarray-set array #() 0 4)
    ))
