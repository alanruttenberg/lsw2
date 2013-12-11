(require :abcl-contrib)
(require :jss)

(in-package :cl-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define a clojure-like "doto" macro. It cons on each call, but if we
;; cared we could add direct support to jss to avoid the consing.

(defmacro doto (object &body body)
  "have object be the implicit first argument to any jss calls in the body"
  (let ((object-var (make-symbol "DOTO-OBJECT")))
    `(let ((,object-var ,object))
       ,@(substitute-jss-calls 
	  body
	  (lambda(form)
	    (list* (first form) (second form) object-var `(cons ,(third form) ,(fourth form))
		   (cddddr form)))))))

(defun substitute-jss-calls (body fn)
  "Walk through the body (not very carefully, but it's unclear that is necessary)"
  (cond ((atom body) body)
	((and (consp body) (eq (car body) 'jss::invoke-restargs))
	 (funcall fn (mapcar (lambda(f)(substitute-jss-calls f fn)) body)))
	(t (mapcar (lambda(f)(substitute-jss-calls f fn)) body))))
	 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun demo-doto-celsius-converter ()
  (let ((frame (new 'JFrame "Celsius Converter"))
	(temp-text (new 'JTextField))
	(celsius-label (new 'JLabel "Celsius"))
	(convert-button (new 'JButton "Convert"))
	(fahrenheit-label (new 'JLabel "Fahrenheit")))
    (#"addActionListener" convert-button
			  (jmake-proxy "java.awt.event.ActionListener"
				       (lambda (obj method event) ;; used instead of hash for simplicity
					 (when (equal "actionPerformed" method)
					   (let ((c (read-from-string (#"getText" temp-text))))
					     (#"setText" fahrenheit-label 
							 (concatenate 'string (princ-to-string (+ 32 (* 1.8 c)))
								      " Fahrenheit")))))))
    (doto frame
      (#"setLayout"  (new 'awt.GridLayout 2 2 3 3))
      (#"add" temp-text)
      (#"add" celsius-label)
      (#"add" convert-button)
      (#"add" fahrenheit-label)
      (#"setSize" 300 80)
      (#"setVisible" +true+)
      (#"setDefaultCloseOperation" (get-java-field :JFrame "DISPOSE_ON_CLOSE")))))