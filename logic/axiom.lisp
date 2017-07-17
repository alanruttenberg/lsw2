(in-package :logic)

(defvar *axioms* (make-hash-table))

(defclass axiom () 
  ((sexp :accessor axiom-sexp :initarg :sexp)
   (name :accessor axiom-name :initarg :name)
   (description :accessor axiom-description :initarg :description)
   (plist :accessor axiom-plist :initarg :plist)
   (generation-form :accessor axiom-generation-form)))

(defmacro def-logic-axiom (name sexp &optional description &rest key-values)
  `(progn
     (sys::record-source-information-for-type  ',name 'def-logic-axiom)
     (setf (gethash (intern (string ',name) 'keyword) *axioms*)
	   (make-instance 'axiom :sexp 
			  (if  (and (consp ',sexp) (keywordp (car ',sexp)))
			       ',sexp
			       (list :fact ',sexp))
			  :description ,description :name ',name
			  :plist ',(loop for (k v) on key-values by #'cddr collect (list k v))))))

(defmethod print-object ((a axiom) stream)
  (print-unreadable-object (a stream :type nil :identity nil)
    (format stream "axiom ~a: ~a" (axiom-name a) 
	    (if (and (consp (axiom-sexp a))
		     (eq  (car (axiom-sexp a)) :fact))
		(second (axiom-sexp a))
		(axiom-sexp a)))))

(defun get-axiom (name &optional (errorp t))
  (let ((found (gethash (intern (string name) 'keyword) *axioms*)))
    (if (and (not found) errorp)
	(error "Couldn't find axiom named ~s" name))
    found))

(defun get-axioms ( &rest key-values &key (errorp t) &allow-other-keys)
  (remf key-values :errorp)
  (let ((them nil))
    (maphash (lambda(name axiom)
	       (declare (ignore name))
	       (when
		   (loop for (k v) on key-values by #'cddr
			 always (find (list k v) (axiom-plist axiom) :test 'equalp))
		 (push axiom them)))
	     *axioms*)
    (if (and (not them) errorp)
	(error "Couldn't find axiom with keys ~s" key-values))
    them))

(defun delete-axiom (name)
  (remhash (intern (string name) 'keyword) *axioms*))

(defun collect-axioms-from-spec (&rest specs)
  "specs: list each element of which is either an axiom name, a list of key values, or a logic sexp. Output a list of axioms or unchanged logic sexps"
  (loop for spec in specs
	if (and (consp spec) (member (car spec) '(:forall :exists :and :or :iff :not := :fact)))
	  collect spec else
	if (atom spec) collect (get-axiom spec)
	  else append (apply 'logic::get-axioms spec)))


(defmethod predicates ((a axiom))
  (predicates (axiom-sexp a)))

(defmethod constants ((a axiom))
  (constants (axiom-sexp a)))

(defmethod predicates ((exp list))
  (let ((them nil))
    (labels ((walk (form)
	       (unless (atom form) 
		 (case (car form)
		   ((:forall :exists) (walk (third form)))
		   ((:implies :iff :and :or :not := :fact) (map nil #'walk (rest form)))
		   (otherwise 
		    (pushnew (list (car form) (1- (length form)))  them :test 'equalp)
		    (map nil #'walk (rest form)))))))
      (walk exp)
      them)))

(defmethod constants ((exp list))
  (let ((them nil))
    (labels ((walk (form)
	       (if (and (symbolp form) (not (char= (char (string form) 0) #\?))) 
		   (pushnew form them)
		   (unless (atom form)
		     (case (car form)
		       ((:forall :exists) (walk (third form)))
		       ((:implies :iff :and :or :not :=) (map nil #'walk (rest form)))
		       (otherwise (map nil #'walk (rest form))))))))
      (walk exp)
      them)))

(defmethod  axiom-generation-form ((a axiom))
  (if (slot-boundp a 'generation-form)
      (slot-value a 'generation-form)
      (let ((keys '((:implies l-implies) (:iff l-iff) (:and l-and) (:or l-or) (:forall l-forall) (:exists l-exists)
		    (:not l-not) (:= l-=) (:fact l-fact))))
	(labels ((rewrite (expression)
		   (cond ((and (consp expression) (member (car expression) keys :key 'car))
			  `(,(second (assoc (car expression) keys))
			    ,@(mapcar (lambda(e) 
			 		(if (and (consp e) (or (find (car e) keys :key 'car) (find (car e) keys :key 'second)))
			 		    e
			 		    `(quote ,e)))
			 	      (mapcar #'rewrite (cdr expression)))))
			 (t expression))
		   ))
	  (let ((form (axiom-sexp a)))
	    (setf (slot-value a 'generation-form)

	    (when (member (car form) keys :key 'car)
	      (rewrite form)
	      (rewrite `(:fact ,form)))))))))

(defmethod axiom-sexp ((a list))
  a)
