(in-package :logic)

(defclass clif-logic-generator (logic-generator)
  ((double-quote-quote :accessor double-quote-quote   :initarg :double-quote-quote :initform t)
   (with-names :accessor with-names :initarg :with-names :initform t)
   (use-camelcase :accessor use-camelcase :initarg :use-camelcase :initform nil)))

(defmethod render-axiom ((g clif-logic-generator) axiom)
  (flet ((quote-string (e)
	   (if (double-quote-quote g)
	       (#"replaceAll" e "'" "''")
	       (#"replaceAll" e "'" "\\U+0027"))) ;; stupid commonlogic parser in hets doesn't recognize \'
	 (format-term (e)
	   (if (use-camelcase g)
	       (camelcase e)
	       (string-downcase (string e)))))
    (let ((form (tree-replace (lambda(e) 
				(when (eq e :implies) (setq e :if))
				(if (symbolp e)
				    (if (char= (char (string e) 0) #\?)
					(format-term (subseq (string e) 1))
					(format-term (string e)))
				    (if (eq (car e) :fact)
					(second e)
					e)))
			      (axiom-sexp axiom))))
      (let ((*print-pretty* t))
	    (if (typep axiom 'axiom)
		(format nil "(cl:comment 'label:~a')~%~{(cl:comment '~a')~%~}~a~%" 
			(quote-string (string-downcase (string (axiom-name axiom))) )
			(if (axiom-description axiom) (list (quote-string (axiom-description  axiom))))
			form
			)
		(format nil "~a" form)
		)))))
  
(defmethod render-axioms ((generator clif-logic-generator) axs)
  (if (stringp axs)
      axs
      (let ((format-string (if (with-names generator)  "~{~a~^~%~%~}"  "~{~a~^~%~}")))
	(format nil format-string (mapcar (lambda(e) (render-axiom generator e)) axs))
	)))



