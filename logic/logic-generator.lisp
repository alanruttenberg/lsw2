(in-package :logic)

;(loop for s in '(l-forall l-exists l-and l-or l-iff l-equal l-implies logical-forall logical-exists logical-and logical-or logical-iff logical-equal logical-implies pred-property pred-class pred-property pred-class *use-holds* logic-generator logical-holds) do (shadowing-import s 'cl-user))
	      
(defclass logic-generator () 
  ((binary-inverses :accessor binary-inverses :initarg :binary-inverses :initform nil)
   (ternary-inverses :accessor ternary-inverses :initarg :ternary-inverses :initform nil)
   (rewrite-inverses? :accessor rewrite-inverses? :initarg :rewrite-inverses? :initform t)
   (with-names :initarg :with-names)
   (mangled-labels :accessor mangled-labels :initarg :mangled-labels :initform nil)
   ))

(defgeneric logical-forall ((g logic-generator) vars expressions))
(defgeneric logical-exists ((g logic-generator) vars expressions))
(defgeneric logical-implies ((g logic-generator) antecedent consequent))
(defgeneric logical-and ((g logic-generator) expressions))
(defgeneric logical-or ((g logic-generator) expressions))
(defgeneric logical-iff ((g logic-generator) antecedent consequent))
(defgeneric logical-not ((g logic-generator) expression))
(defgeneric logical-= ((g logic-generator) a b))
(defgeneric logical-neq ((g logic-generator) a b))
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

(defmethod logical-relation :around ((g logic-generator) head &rest args)
  (let ((pair (and (rewrite-inverses? g)
		   (cond ((= (length args) 2)
			  (find head (binary-inverses g) :key 'second))
			 ((= (length args) 3)
			  (find head (ternary-inverses g) :key 'second))
			 ))))
    (if pair
	(apply #'call-next-method g (first pair) (second args) (first args) (cddr args))
	(call-next-method))))
		   
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
(defmethod logical-neq ((g logic-generator) a b) `(:not (:= ,a ,b)))
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
  (if (and (listp var) (second var))
      `(with-logic-var ,(car var)
         (with-logic-var ,(cdr var)
           ,@body))
      (progn
        (when (listp var) (setq var (car var)))
        (let ((vars (gensym)))
          `(with-logic-vars (,vars 1)
             (let ((,var (car ,vars)))
               ,@body))))))

(eval-when (:load-toplevel :execute)
  (xp::set-pprint-dispatch+
   '(cons (member with-logic-var)) 'xp::let-print
   '(0) *print-pprint-dispatch*))

(defun logic-var-p (thing)
  (and (symbolp thing) (char= (char (string thing) 0) #\?) (> (length (string thing)) 1)))

(defun pred-property (head &rest args)
  (apply 'logical-relation *logic-generator* head args))

(defun pred-class (class arg)
  (logical-class *logic-generator*  class arg))

(defvar *quantifier-scoped* nil)

(defun l-forall (vars &rest expressions)
  (assert (every 'logic-var-p vars) (vars)
	  "Anything quantified over needs to be a variable but got forall ~a" vars)
  (if (> (length expressions) 1)
      (l-forall vars (apply 'l-and expressions))
      (let ((*quantifier-scoped* (append vars *quantifier-scoped*)))
	(logical-forall *logic-generator* vars expressions))))

(defun l-relation (head &rest args)
  (apply 'logical-relation *logic-generator* head args))
    

(defun l-exists (vars &rest expressions)
  (assert (every 'logic-var-p vars) (vars)
	  "Anything quantified over needs to be a variable but got exists ~a" vars)
  (if (zerop (length vars))
      (apply 'l-and expressions)
      (if (> (length expressions) 1)
          (l-exists vars (apply 'l-and expressions))
          (let ((*quantifier-scoped* (append vars *quantifier-scoped*)))
            (logical-exists *logic-generator* vars expressions)))))

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

(defun l-neq (a b)
  (logical-neq *logic-generator* a b))

(defun l-fact (a)
  (logical-fact *logic-generator* a))

(defun l-distinct (&rest args)
  (apply 'logical-distinct *logic-generator* args))

(defun l-parens (expression)
  (logical-parens *logic-generator* expression))

(defun formula-sexp-p (it)
  (and (consp it) (find (car it) *operators* :key 'car)))

(defmethod builtin-predicate ((g logic-generator) pred)
  nil)
    
(defmethod render-axiom ((g logic-generator) (a axiom))
  (let ((*logic-generator* g))
    (eval (axiom-generation-form a))))

(defmethod render-axiom ((g logic-generator) (a list))
  (render-axiom g (make-instance 'axiom :sexp a)))

(defmethod render-axiom ((g logic-generator) (a string))
  a)

(defmethod render-axiom ((g symbol) (a string))
  (render-axiom (make-instance g) a))


(defmethod render-axioms :around (any axs)
  (let ((*print-case* :downcase))
    (call-next-method)))

(defmethod render-axioms ((g logic-generator b) axs)
  (if (stringp axs)
      axs
      (mapcar (lambda(e) (render-axiom g e)) axs)))

(defmethod render-axioms ((g symbol) axs)
  (render-axioms (make-instance g) axs))

(defun render (which assumptions &optional goals &key path at-beginning at-end (sorter 'identity) (with-names t) princ)
  (when (eq goals :princ)
    (setq goals nil princ t))
  (let ((generator-class
	  (ecase which
	    (:z3 'z3-logic-generator)
	    (:prover9 'prover9-logic-generator)
	    (:vampire 'vampire-logic-generator)
	    (:latex 'latex-logic-generator)
	    (:clif 'clif-logic-generator)
	    (:dol 'dol-logic-generator)
	    (:lsw 'logic-generator)
	    (:fol-text 'fol-text-logic-generator)
	    ))
	(*print-case* :downcase))
    (flet ((doit ()
	     (let ((axioms (funcall sorter (append (if (stringp assumptions) assumptions
						       (collect-axioms-from-spec assumptions))
						   (if (stringp goals) goals
						       (mapcar (lambda(e) (negate-axiom e)) (collect-axioms-from-spec goals))))))
		   generator)
	       (values
		(if (eq which :fol-text)
		     (render-axioms (make-instance generator-class) axioms )
		    (if (eq which :dol)
			(render-ontology
			 (setq generator (make-instance generator-class :with-names with-names))
			 "Anonymous" axioms)
			(if (eq which :lsw)
			    (with-output-to-string (s) (pprint (render-axioms (make-instance generator-class) axioms) s))
			    (concatenate
			     'string
			     (or (and at-beginning (format nil "~a~%" at-beginning)) "")
			     (if (eq which :vampire)
				 (let ((g (setq generator (make-instance generator-class   :with-names with-names))))
				   (values (render-axioms g axioms) g))
				 (if (eq which :z3)
				     (let ((g (setq generator (make-instance generator-class  :with-names with-names ))))
				       (render-axioms g axioms ))
				     (render-axioms (setq generator (make-instance generator-class  :with-names with-names )) axioms )))
			     (or (and at-end (format nil "~a~%" at-end))  "")))))
		generator))))
      (if path
	(with-open-file (f path :direction :output :if-does-not-exist :create :if-exists :supersede)
	  (progn
	    (write-string (doit) f) (truename path)))
	(if princ
	    (progn (princ (doit))
		   (values))
	    (doit)
	    )))))
