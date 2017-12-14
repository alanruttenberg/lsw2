(in-package :logic)

(defclass z3-logic-generator (logic-generator)
  ((with-declarations :accessor with-declarations :initarg :with-declarations :initform t )
   (with-names :accessor with-names :initarg :with-names :initform t)
   (domain-sort :accessor domain-sort :initarg :domain-sort :initform '|Int|)))

(defmethod normalize-names ((g z3-logic-generator) e)
  (cond ((and (symbolp e) (char= (char (string e) 0) #\?))
	 (intern (subseq (string e) 1)))
	((keywordp e) e)
	((and (symbolp e) (find #\- (string e))
	      (intern (camelCase (replace-all (string e) "(\\d+)" (lambda(e) (camelCase (format nil "~r" (read-from-string e)))) 1)))))
	((uri-p e) (intern 
			     (camelCase
			      (if (and (boundp '*default-kb*) *default-kb*)
				  (uri-label e)
				  (#"replaceAll" (uri-full e) ".*/" "")))))
	((atom e) e)
	(t (mapcar (lambda(e) (normalize-names g e)) e))))

(defmethod z3-quantifier-vars ((g z3-logic-generator) vars)
  (normalize-names g vars))

(defmethod logical-forall ((g z3-logic-generator) vars expressions)
  `(forall ,(mapcar (lambda(e) `(,e ,(domain-sort g))) (z3-quantifier-vars g vars)) ,@expressions))

(defmethod logical-exists ((g z3-logic-generator) vars expressions)
  `(exists ,(mapcar (lambda(e) `(,e ,(domain-sort g))) (z3-quantifier-vars g vars)) ,@expressions))

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
		(replace-all
		 (format nil "~a" exp) "\\b([A-Z-0-9]+)\\b"
		 (lambda(e)
		   (if (some 'lower-case-p e)
		       e
		       (string-downcase e)))
		 1)))))

(defmethod builtin-predicate ((g z3-logic-generator) pred)
  (member pred '(declare-datatypes + - < > * = <= >= ^)))
  
(defmethod generate-declarations ((g z3-logic-generator) (a list) &key (include-constants t) (include-predicates t))
  (let ((constants (remove-duplicates (mapcan (lambda(e) (constants g (axiom-sexp e))) a)))
	(predicates (remove-duplicates (mapcan (lambda(e) (predicates g (axiom-sexp e))) a) :test 'equalp)))
    (apply 'concatenate 'string
	   (append
	    (and include-constants
		 (loop for c in constants
		       collect (to-string g `(declare-const ,(normalize-names g c) ,(domain-sort g)))))
	    (and include-predicates
		 (loop for p in predicates
		       collect (to-string g
					  `(declare-fun ,(normalize-names g (car p))
							,(loop repeat (second p) collect (domain-sort g)) |Bool|))))))))

  
(defmethod render-axiom ((g z3-logic-generator) (a axiom))
  (let ((bare (call-next-method)))
    (if (with-declarations g)
	(concatenate 'string 
		     (generate-declarations g (list a))
		     (to-string g `(assert ,(normalize-names g bare))))
	(if (and (with-names g) (axiom-name a))
	    (to-string g `(assert (|!| ,(normalize-names g bare) |:named| ,(replace-all (string (axiom-name a)) "(\\d+)" (lambda(e) (camelCase (format nil ".~r" (read-from-string e)))) 1))))
	    (to-string g `(assert ,(normalize-names g bare)))))))

(defmethod render-axioms ((g z3-logic-generator) (a list))
  (apply 'concatenate 'string
	 (generate-declarations g a)
	 (mapcar 
	  (lambda(e) (to-string g e))
	  (let ((was (with-declarations g)))
	    (unwind-protect (progn 
			      (setf (with-declarations g) nil)
			      (mapcar (lambda(e) (render-axiom g e)) a))
	      (setf (with-declarations g) was))))))
		    
;all x all y all t(continuantOverlapAt(x,y,t) <-> (exists z(continuantPartOfAt(z,x,t) & continuantPartOfAt(z,y,t)))).

