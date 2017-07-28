(in-package :logic)

(defclass z3-logic-generator (logic-generator)
  ((with-declarations :accessor with-declarations :initarg :with-declarations :initform t )
   (with-names :accessor with-names :initarg :with-names :initform nil)))

(defmethod normalize-names ((g z3-logic-generator) e)
  (cond ((and (symbolp e) (char= (char (string e) 0) #\?))
	 (intern (subseq (string e) 1)))
	((keywordp e) e)
	((and (symbolp e) (find #\- (string e))  (intern (cl-user::camelCase (string e)))))
	((cl-user::uri-p e) (intern 
			     (cl-user::camelCase
			      (if (and (boundp 'cl-user::*default-kb*) cl-user::*default-kb*)
				  (cl-user::uri-label e)
				  (#"replaceAll" (cl-user::uri-full e) ".*/" "")))))
	((atom e) e)
	(t (mapcar (lambda(e) (normalize-names g e)) e))))

(defmethod z3-quantifier-vars ((g z3-logic-generator) vars)
  (normalize-names g vars))

(defmethod logical-forall ((g z3-logic-generator) vars expressions)
  `(forall ,(mapcar (lambda(e) `(,e |Int|)) (z3-quantifier-vars g vars)) ,@expressions))

(defmethod logical-exists ((g z3-logic-generator) vars expressions)
  `(exists ,(mapcar (lambda(e) `(,e |Int|)) (z3-quantifier-vars g vars)) ,@expressions))

(defmethod logical-implies ((g z3-logic-generator) antecedent consequent)
  `(=> ,antecedent ,consequent))

(defmethod logical-and ((g z3-logic-generator) expressions) 
  `(and ,@expressions))

(defmethod logical-or ((g z3-logic-generator) expressions) 
  `(or ,@expressions))

(defmethod logical-iff ((g z3-logic-generator) antecedent consequent)
  `(= ,antecedent  ,consequent))

(defmethod logical-not ((g z3-logic-generator) expression)
  `(not ,expression))

(defmethod logical-= ((g z3-logic-generator) a b)
  `(= ,a ,b))

(defmethod logical-holds ((g z3-logic-generator) &rest args) 
  `(holds ,@args))

(defmethod logical-fact ((g z3-logic-generator) fact)
   fact)

(defmethod logical-distinct ((g z3-logic-generator) &rest args)
  `(distinct ,@args))

(defmethod to-string  ((g z3-logic-generator) exp)
  (if (stringp exp) exp
      (let ((*print-case* nil))
	(format nil "~a~%"
		(cl-user::replace-all
		 (format nil "~a" exp) "\\b([A-Z-0-9]+)\\b"
		 (lambda(e)
		   (if (some 'lower-case-p e)
		       e
		       (string-downcase e)))
		 1)))))

(defmethod generate-declarations ((g z3-logic-generator) (a list))
  (let ((constants (remove-duplicates (mapcan (lambda(e) (constants (axiom-sexp e))) a)))
	(predicates (remove-duplicates (mapcan (lambda(e) (predicates (axiom-sexp e))) a) :test 'equalp)))
    (apply 'concatenate 'string
	   (append
	    (loop for c in constants
		  collect (to-string g `(declare-const ,(normalize-names g c) |Int|)))
	    (loop for p in predicates
		  collect (to-string g `(declare-fun ,(normalize-names g (car p))
						     ,(loop repeat (second p) collect '|Int|) |Bool|)))))))

  
(defmethod render-axiom ((g z3-logic-generator) (a axiom))
  (let ((bare (call-next-method)))
    (if (with-declarations g)
	(concatenate 'string 
		     (generate-declarations g (list a))
		     (to-string g `(assert ,(normalize-names g bare))))
	(if (with-names g)
	    (to-string g `(assert (|!| ,(normalize-names g bare) |:named| ,(string (axiom-name a)))))
	    (to-string g `(assert ,(normalize-names g bare)))))))

(defmethod render-axioms ((g z3-logic-generator) (a list))
  (apply 'concatenate 'string
	 (generate-declarations g a)
	 (mapcar 
	  (lambda(e) (to-string g e))
	  (let ((was  (with-declarations g)))
	    (unwind-protect (progn 
			      (setf (with-declarations g) nil)
			      (mapcar (lambda(e) (render-axiom g e)) a))
	      (setf (with-declarations g) was))))))
		    
;all x all y all t(continuantOverlapAt(x,y,t) <-> (exists z(continuantPartOfAt(z,x,t) & continuantPartOfAt(z,y,t)))).

