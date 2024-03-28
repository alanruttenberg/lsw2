(in-package :logic)

(defclass clif-logic-generator (logic-generator)
  ((double-quote-quote :accessor double-quote-quote   :initarg :double-quote-quote :initform t)
   (with-names :accessor with-names :initarg :with-names :initform t)
   (use-camelcase :accessor use-camelcase :initarg :use-camelcase :initform nil)))

;; extra syntax :dots -> ... (:dots x) -> ...x

(defmethod render-axiom ((g clif-logic-generator) axiom)
  (labels ((quote-string (e)
	   (if (double-quote-quote g)
	       (#"replaceAll" e "'" "''")
	       (#"replaceAll" e "'" "\\U+0027"))) ;; stupid commonlogic parser in hets doesn't recognize \'
	 (format-term (e)
	   (if (use-camelcase g)
	       (camelcase e)
	       (string-downcase (string e))))
	 (fix-tree (form) 
	   (tree-replace (lambda(e)
			   (cond ((eq e :implies) (setq e :if))
				 ((eq e :dots) (setq e :|...|))
				 ((and (consp e) (eq (car e) :dots))
				  (setq e (intern (format nil "...~a" (second e)) 'keyword)))
				 ((symbolp e)
				  (if (char= (char (string e) 0) #\?)
				      (format-term (subseq (string e) 1))
				      (format-term (string e))))
				 ((eq (car e) :fact)
				  (second e))
				 ((and (consp e) (member (car e) '(:exists :forall)))
				  (let ((body-forms (cddr e)))
				    (if (> (length body-forms) 1)
					`(,(car e) ,(mapcar #'fix-tree (second e)) (and ,@(mapcar #'fix-tree body-forms)))
					e)))
				 ((and (consp e) (eq (car e) :distinct))
				  `(and ,@(loop for (e1 . rest) on (cdr e)
						append (loop for e2 in rest
							     collect `(not (= ,e1 ,e2))))))
				 (t e)))
			 form)))
    (let ((form (fix-tree (axiom-sexp axiom))))
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



