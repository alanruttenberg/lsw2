(defpackage :logic (:use cl))
(in-package :logic)

;(loop for s in '(l-forall l-exists l-and l-or l-iff l-equal l-implies logical-forall logical-exists logical-and logical-or logical-iff logical-equal logical-implies pred-property pred-class pred-property pred-class *use-holds* logic-generator logical-holds) do (shadowing-import s 'cl-user))
	      
(defclass logic-generator () ())

(defgeneric logical-forall ((g logic-generator) vars expressions))
(defgeneric logical-exists ((g logic-generator) vars expressions))
(defgeneric logical-implies ((g logic-generator) antecedent consequent))
(defgeneric logical-and ((g logic-generator) expressions))
(defgeneric logical-or ((g logic-generator) expressions))
(defgeneric logical-iff ((g logic-generator) antecedent consequent))
(defgeneric logical-not ((g logic-generator) expression))
(defgeneric logical-= ((g logic-generator) a b))
(defgeneric logical-holds ((g logic-generator) &rest args))
(defgeneric logical-class ((g logic-generator) class el))
(defgeneric logical-relation ((g logic-generator) head &rest args))
(defgeneric logical-fact ((g logic-generator) fact))
(defgeneric logical-distinct ((g logic-generator) &rest els))

(defvar *use-holds* nil)

(defmethod logical-relation ((g logic-generator) head &rest args)
    (if *use-holds*
	(apply 'logical-holds g head args)
	`(,head ,@args)))

(defmethod logical-class ((g logic-generator) class el)
  (if *use-holds*
      (logical-holds g class el)
      `(,class ,el)))

(defmethod logical-forall ((g logic-generator) vars expressions) `(:forall ,vars ,@expressions))
(defmethod logical-exists ((g logic-generator) vars expressions) `(:exists ,vars ,@expressions))
(defmethod logical-implies ((g logic-generator) antecedent consequent) `(:implies ,antecedent ,consequent))
(defmethod logical-and ((g logic-generator) expressions) `(:and ,@expressions))
(defmethod logical-or ((g logic-generator) expressions) `(:or ,@expressions))
(defmethod logical-iff ((g logic-generator) antecedent consequent) `(:iff ,antecedent ,consequent))
(defmethod logical-not ((g logic-generator) expression) `(:not ,expression))
(defmethod logical-= ((g logic-generator) a b) `(:= ,a ,b))
(defmethod logical-holds ((g logic-generator) &rest args) `(:holds ,@args))
(defmethod logical-fact ((g logic-generator) fact) fact)
(defmethod logical-distinct ((g logic-generator) &rest els)
  (list* :and (loop for (a . rest) on els
		 append
		 (loop for b in rest
		       collect `(:not (:= ,a ,b))))))

(defvar *logic-generator* (make-instance 'logic-generator))

(defparameter *logic-symbols* "PQRSUVWXYZABCDEFGHIJKLMNOT")

(defmacro with-logic-vars ((vars n &optional (from *logic-symbols* from-supplied-p)) &body body)
  `(let ((,vars (loop for i below  ,n
		      for base = (elt ,(if from-supplied-p 'from '*logic-symbols*) i)
		      collect (intern (concatenate 'string "?" (string base))
				      (if (symbolp base) (symbol-package base) *package*))
		      )))
     (let ((*logic-symbols* (if ,from-supplied-p *logic-symbols* (subseq *logic-symbols* ,n))))
       ,@body)))

(defmacro with-logic-var (var &body body)
  (let ((vars (gensym)))
    `(with-logic-vars (,vars 1)
       (let ((,var (car ,vars)))
	 ,@body))))

(defun pred-property (head &rest args)
  (apply 'logical-relation *logic-generator* head args))

(defun pred-class (class arg)
  (logical-class *logic-generator*  class arg))

(defun l-forall (vars &rest expressions)
  (logical-forall *logic-generator* vars expressions))

(defun l-exists (vars &rest expressions)
  (logical-exists *logic-generator* vars expressions))

(defun l-implies (antecedent consequent)
  (logical-implies *logic-generator* antecedent consequent))

(defun l-and (&rest expressions)
  (logical-and *logic-generator* expressions))

(defun l-or (&rest expressions)
  (logical-or *logic-generator* expressions))

(defun l-iff (antecedent consequent)
  (logical-iff *logic-generator* antecedent consequent))

(defun l-not (expression)
  (logical-not *logic-generator* expression))

(defun l-= (a b)
  (logical-= *logic-generator* a b))

(defun l-fact (a)
  (logical-fact *logic-generator* a))

(defun l-distinct (&rest args)
  (apply 'logical-distinct *logic-generator* args))

(defun l-parens (expression)
  (logical-parens *logic-generator* expression))

(defun formula-sexp-p (it)
  (and (consp it) (member (car it) '(:implies :forall :exists :and :or :iff :not := :fact := :owl :expand))))

(defmethod builtin-predicate ((g logic-generator) pred)
  nil)
    
(defmethod predicates ((g logic-generator) (a axiom))
  (predicates g (axiom-sexp a)))

(defmethod constants ((g logic-generator) (a axiom))
  (constants g (axiom-sexp a)))

(defmethod predicates ((g logic-generator) (exp list))
  (let ((them nil)
	(exp (axiom-sexp exp))) ;; so macroexpansion happens
    (labels ((walk (form)
	       (unless (atom form)
		 (if (builtin-predicate g (car form))
		     (map nil #'walk (rest form))
		     (case (car form)
		       ((:forall :exists) (walk (third form)))
		       ((:implies :iff :and :or :not := :fact :distinct) (map nil #'walk (rest form)))
		       (otherwise 
			(pushnew (list (intern (string (car form))) (1- (length form)))  them :test 'equalp)
			(map nil #'walk (rest form))))))))
      (walk exp)
      them)))

(defmethod constants ((g logic-generator) (exp list))
  (let ((them nil)
	(exp (axiom-sexp exp))) ;; so macroexpansion happens
    (labels ((walk (form)
	       (if (and (symbolp form) (not (char= (char (string form) 0) #\?))) 
		   (pushnew (intern (string form)) them)
		   (unless (atom form)
		     (case (car form)
		       ((builtin-predicate g (car form)) (map nil #'walk (rest form)))
		       ((:forall :exists) (walk (third form)))
		       ((:implies :iff :and :or :not := :distinct) (map nil #'walk (rest form)))
		       (otherwise (map nil #'walk (rest form))))))))
      (walk exp)
      them)))

(defmethod render-axiom ((g logic-generator) (a axiom))
  (let ((*logic-generator* g))
    (eval (axiom-generation-form a))))

(defmethod render-axiom ((g logic-generator) (a list))
  (render-axiom g (make-instance 'axiom :sexp a)))

(defmethod render-axiom ((g logic-generator) (a string))
  a)

(defmethod render-axiom ((g symbol) (a string))
  (render-axiom (make-instance g) a))

(defmethod render-axioms ((g logic-generator b) axs)
  (if (stringp axs)
      axs
      (mapcar (lambda(e) (render-axiom g e)) axs)))

(defmethod render-axioms ((g symbol) axs)
  (render-axioms (make-instance g) axs))

(defun render (which assumptions &optional goals &key path at-beginning at-end)
  (let ((generator-class
	  (ecase which
	    (:z3 'z3-logic-generator)
	    (:prover9 'prover9-logic-generator)
	    (:vampire 'z3-logic-generator)
	    (:latex 'latex-logic-generator)
	    (:clif 'clif-logic-generator)
	    (:dol 'dol-logic-generator)
	    )))
    (flet ((doit ()
	     (let ((axioms (append (if (stringp assumptions) assumptions
					 (collect-axioms-from-spec assumptions))
				     (if (stringp goals) goals
					 (mapcar (lambda(e) (negate-axiom e)) (collect-axioms-from-spec goals))))))
	       (if (eq which :dol)
		   (render-ontology
		    (make-instance generator-class)
		    "Anonymous" axioms)
		   (concatenate
		    'string
		    (or (and at-beginning (format nil "~a~%" at-beginning)) "")
		    (if (eq which :vampire)
			(let ((g (make-instance generator-class :with-names nil)))
			  (render-axioms g axioms))
			(if (eq which :vampire)
			    (let ((g (make-instance generator-class :with-names t)))
			      (render-axioms g axioms))
			    (render-axioms generator-class axioms)))
		    (or (and at-end (format nil "~a~%" at-end))  ""))))))
      (if path
	(with-open-file (f path :direction :output :if-does-not-exist :create :if-exists :supersede)
	  (progn
	    (write-string (doit) f) (truename path)))
	(doit)))))
