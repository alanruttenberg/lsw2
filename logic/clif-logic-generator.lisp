(in-package :logic)

(defclass clif-logic-generator (logic-generator)())

(defun tree-find (sym tree &key (test #'eq))
  (cond ((atom tree)
	 (funcall test sym tree))
	(t (some (lambda(el) (tree-find sym el)) tree))))

(defun tree-replace (replace-fn tree)
  "create new tree replacing each element with the result of calling replace-fn on it"
  (labels ((tr-internal (tree)
	   (cond ((atom tree) (funcall replace-fn tree))
		 (t (mapcar #'tr-internal (funcall replace-fn tree))))))
    (tr-internal tree)))

(defmethod render-axiom ((g clif-logic-generator) axiom)
  (flet ((quote-string (e)
	   (#"replaceAll" e "'" "\\U+0027"))) ;; stupid commonlogic parser in hets doesn't recognize \'
    (let ((*print-pretty* t))
      (format nil "(cl:comment '~a')~%~{(cl:comment '~a')~%~}~a" 
	      (quote-string (string-downcase (string (axiom-name axiom))) )
	      (if (axiom-description axiom) (list (quote-string (axiom-description  axiom))))
	      (format nil "~a~%"
		      (tree-replace (lambda(e) 
				      (when (eq e :implies) (setq e :if))
				      (if (and (symbolp e) (char= (char (string e) 0) #\?))
					  (cl-user::camelcase (subseq (string e) 1))
					  (if (symbolp e)
					      (cl-user::camelcase (string e))
					      e)))
				    (if (eq (car (axiom-sexp axiom)) :fact)
					(second (axiom-sexp axiom))
					(axiom-sexp axiom))))))))
  
(defmethod render-axioms ((generator clif-logic-generator) axs)
  (if (stringp axs)
      axs
      (format nil "~{~a~^~%~%~}" (mapcar (lambda(e) (render-axiom generator e)) axs))
      ))

render-ontology

