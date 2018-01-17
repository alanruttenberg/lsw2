(in-package :logic)

(defclass clif-logic-generator (logic-generator)
  ((double-quote-quote :accessor double-quote-quote   :initarg :double-quote-quote :initform t)
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
    (let ((*print-pretty* t))
      (format nil "(cl:comment 'label:~a')~%~{(cl:comment '~a')~%~}~a" 
	      (quote-string (string-downcase (string (axiom-name axiom))) )
	      (if (axiom-description axiom) (list (quote-string (axiom-description  axiom))))
	      (format nil "~a~%"
		      (tree-replace (lambda(e) 
				      (when (eq e :implies) (setq e :if))
				      (if (and (symbolp e) (char= (char (string e) 0) #\?))
					  (format-term (subseq (string e) 1))
					  (if (symbolp e)
					      (format-term (string e))
					      e)))
				    (if (eq (car (axiom-sexp axiom)) :fact)
					(second (axiom-sexp axiom))
					(axiom-sexp axiom))))))))
  
(defmethod render-axioms ((generator clif-logic-generator) axs)
  (if (stringp axs)
      axs
      (format nil "~{~a~^~%~%~}" (mapcar (lambda(e) (render-axiom generator e)) axs))
      ))



