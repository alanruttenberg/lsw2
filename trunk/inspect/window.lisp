(in-package :cl-user)

;;; Basic windowing

(defun make-window (name width height)
  (let ((w (new 'java.awt.Frame name)))
    (primp-window w width height nil)
    w))

(defun make-window-for-panel (title panel)
  (let ((w (new 'java.awt.Frame title)))
    (#"add" w panel)
    (#"pack" w)
    (#"show" w)
    (add-window-close-handler w nil)
    w))

(defun primp-window (w width height proc)
  (#"setSize" w width height)
  (#"setVisible" w t)
  (add-window-close-handler w proc)
  )

; proc is a thunk that performs any additional closing operations
(defun add-window-close-handler (window proc)
  (#"addWindowListener" 
   window
   (jinterface-safe-implementation 
    (jclass "java.awt.event.WindowListener")
    "windowClosing"
    #'(lambda (evt)
	(#"dispose" window)
	(if proc (funcall proc))))))

; action should be a procedure of one argument, which will be an AWT ActionEvent
(defun make-button (name action)
  (let ((b (new 'java.awt.Button name)))
    (#"addActionListener"
     b
     (jinterface-safe-implementation
      (jclass "java.awt.event.ActionListener")
      "actionPerformed"
      action))
    b))

;;; Some object-oriented classes for encapsulating java.awt.component classes
;;; (+++ didn't get very far with this)

(defclass jcomponent ()
  ((component :accessor jcomponent)))

(defclass jframe (jcomponent)
  (jframe 
   panel))

(defmethod initialize-instance :after ((this jframe) &key (name "Window") (height 200) (width 200))
  (with-slots (jframe panel) this
    (setq panel (make-contents this))
    (setq jframe (make-swing-window-for-panel name panel))
    (#"show" jframe)))
