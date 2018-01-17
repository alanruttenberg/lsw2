(in-package :logic)

(defvar *axioms* (make-hash-table))

(defun clear-axioms ()
  (clrhash *axioms*))

(defclass axiom () 
  ((sexp :accessor axiom-sexp :initarg :sexp)
   (name :accessor axiom-name :initarg :name :initform nil)
   (description :accessor axiom-description :initarg :description :initform nil)
   (plist :accessor axiom-plist :initarg :plist :initform nil)
   (generation-form :accessor axiom-generation-form)
   (from :accessor axiom-from :initform nil :initarg :from)))

(defun keywordify (x) (intern (string x) :keyword))

(defmacro def-logic-axiom (name sexp &optional description &rest key-values)

    (when (keywordp description) (push description key-values) (setq description nil))
    `(progn
       (sys::record-source-information-for-type  ',name 'def-logic-axiom)
       (let* ((sexp-1 ',sexp)
	      (sexp (if  (and (consp sexp-1) (keywordp (car sexp-1)))
			 sexp-1
			 (list :fact sexp-1))))
	 (multiple-value-bind (predicates constants functions) (formula-elements sexp)
	   (setf (gethash (intern (string ',name) 'keyword) *axioms*)
		 (make-instance 'axiom :sexp sexp
				       :description ,description :name ',name
				       :plist (append
					       (loop for (pred) in predicates collect `(:relation ,(keywordify pred)))
					       (loop for function in functions collect `(:function ,(keywordify function)))
					       (loop for const in constants collect `(:constant ,(keywordify const)))
					       (loop for (k v) on ',key-values by #'cddr
						     collect (list k (if (symbolp k)
									 (keywordify v)
									 k))))))))))

(defmethod print-object ((a axiom) stream)
  (let ((*print-case* :downcase))
    (print-unreadable-object (a stream :type nil :identity nil)
      (format stream "axiom ~a: ~a" (axiom-name a) 
	      (if (and (consp (axiom-sexp a))
		       (eq  (car (axiom-sexp a)) :fact))
		  (second (axiom-sexp a))
		  (axiom-sexp a))))))

(defun get-axiom (name &optional (errorp t))
  (if (typep name 'axiom)
      name
      (let* ((key (intern (string name) 'keyword))
	     (found (or (gethash key  *axioms*) (gethash key *autonamed-axioms*))))
	(if (and (not found) errorp)
	    (error "Couldn't find axiom named ~s" name))
	found)))




(defun delete-axiom (name)
  (remhash (intern (string name) 'keyword) *axioms*))

;; Specifications of axioms - basically a way to have a databasish
;; query for axioms instead of having to name each one. A specification is a list.
;; Each element of the list is either
;;   a keyword, in which case it is considered the name of an axioms
;;   a list starting with a logic connective (:and, :iff ...) is considered an axiom sexp and used as it
;;   index lookup.
;;     The simplest index lookup is a series of key value
;;     pairs, and matches any axioms that have all those keys with those
;;     values.  e.g. (:kind :transtivity :kind mereology) will collect
;;     any axioms which have two :kind keys, one with each of
;;     :transtivity and :mereology
;;     An element of a lookup can also be one of a few forms
;;     if for a key you use (:if <key>) then it will match key as usual, when the axiom has that key, but also if there is no value for the key.
;;     If for a value you use (:not value) then it will match anything that doesn't have that value
;;     If for a value you use (:or <value1> <value2> ..) it will match if the key has any of those values.
;;     :not and :or can be nested
;;     There are two special values, which shouldn't be used for actual values
;;      :none matches if the key is not present
;;      :any-value matches if the key is present, regardless of value 
;;   (:exclude <element>) - remove any of these found in the above.

(defun get-axioms ( &rest key-values &key (errorp t) &allow-other-keys)
  (remf key-values :errorp)
  (let ((them nil))
    (maphash (lambda(name axiom)
	       (declare (ignore name))
	       (when
		   (loop for (k v) on key-values by #'cddr
			     always (check-key-spec k v (axiom-plist axiom)));(find (list k v) (axiom-plist axiom) :test 'equalp))
		 (push axiom them)))
	     *axioms*)
    (if (and (not them) errorp)
	(error "Couldn't find axiom with keys ~s" key-values))
    them))

;; see whether spec key value match the axiom keys.
;; 
(defun check-key-spec (key value keys)
  (labels ((check-value (spec-value value)
	     (cond ((eq spec-value :any-value)
		    t)
		   ((and (consp spec-value) (eq (car spec-value) :or))
		    (some (lambda(conjunct)
			     (some (lambda(e) (check-value conjunct (second e))) keys))
			   (rest spec-value)))
		   ((and (consp spec-value) (eq (car spec-value) :not))
		    (not (some (lambda(e) (check-value (second spec-value) (second e))) keys)))
		   ((and (consp spec-value) (eq (car spec-value) :and))
		    (every (lambda(conjunct)
			     (some (lambda(e) (check-value conjunct (second e))) keys))
			   (rest spec-value)))
		   ((consp spec-value)
		    (spec-error key value))
		   ((eq spec-value :any-value) t)
		   (t (eq spec-value value)))))
    (if (eq value :none)
	(not (assoc (if (consp key) (second key) key) keys))
	(let* ((if (and (consp key) (eq (car key) :if)))
	       (key-to-check (if if (second key) (if (atom key) key (spec-error key value)))))
	  (if key-to-check (assoc key-to-check keys))
	  (or (and if (not (assoc key-to-check keys)))
	      (if (or if (atom key))
		  (some
		   (lambda(kv)
		     (and (eq (first kv) key-to-check)
			  (check-value  value (second kv))))
		   keys)
		  (spec-error key value)))))))

(defun collect-axioms-from-spec (specs &optional (error-if-not-found t))
  "specs: either a single formuala or an axiom name or a list each element of which is either an axiom name, a list of key values, or a logic sexp. Output a list of axioms or unchanged logic sexps"
  (if (null specs) nil)
  (let* ((nots (remove-if-not (lambda(e) (and (consp e) (eq (car e) :exclude))) specs))
	 (specs (set-difference specs nots :test 'equalp)))
    (remove-duplicates 
     (set-difference
      (if (formula-sexp-p specs) 
	  (list specs)
	  (if (keywordp specs)
	      (list (get-axiom specs error-if-not-found))
	      (loop for spec in specs
		    if (formula-sexp-p spec)
		      collect spec
		    else if (atom spec)
			   collect (get-axiom spec error-if-not-found)
		    else if (eq (car spec) :negate)
			    collect (negate-axiom (get-axiom (second spec) error-if-not-found) )
		    else append (apply 'get-axioms (append spec (list :errorp error-if-not-found))))))
      (and nots (collect-axioms-from-spec (mapcar 'second nots) nil))
      :test 'equalp)
     :test 'equalp)))

(defun rewrite-to-axiom-generation-form (form)
  (let ((keys '((:distinct l-distinct) (:implies l-implies) (:iff l-iff) (:and l-and) (:or l-or) (:forall l-forall) (:exists l-exists)
		(:not l-not) (:= l-=) (:fact l-fact) (:owl l-owl) (:parens l-parens))))
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
      (if (member (car form) keys :key 'car)
	(rewrite form)
	(rewrite `(:fact ,form))))))


(defmethod  axiom-generation-form ((a axiom))
  (if (slot-boundp a 'generation-form)
      (slot-value a 'generation-form)
      (let ((form (axiom-sexp a)))
	(setf (slot-value a 'generation-form)
	      (rewrite-to-axiom-generation-form form)))))

(defmethod axiom-sexp :around ((a axiom))
  (axiom-sexp (axiom-sexp (slot-value a 'sexp))))

(defmethod axiom-sexp ((a list))
  (when (tree-find :expand a)
    (setq a (tree-replace
	     (lambda(e) (if (and (consp e) (eq (car e) :expand))
			    (macroexpand (second e))
			    e))
	     a)))
  (when (tree-find :axiom a)
    (setq a (tree-replace (lambda(e) (if (and (consp e) (eq (car e) :axiom))
					 (axiom-sexp (get-axiom (second e)))
					 e))
			  a)))
  (when (tree-find :owl a)
    (setq a (tree-replace (lambda(e) (if (and (consp e) (eq (car e) :owl))
					 (axiom-sexp (owl-sexp-to-fol (second e)))
					 e)) a)))
  a)
			       


(defmethod negate-axiom ((a list)) `(:not ,a))

(defmethod negate-axiom ((a axiom))
  (make-instance 'axiom 
		 :sexp `(:not ,(axiom-sexp a))
		 :name (intern (concatenate 'string "NEGATED-" (string (axiom-name a))) 'keyword)
		 :description (concatenate 'string "(negated) " (axiom-description a))
		 :from a))

(defmethod pprint-spec-axiom-names (spec &key  &allow-other-keys)
  (pprint-spec-axioms spec :only-name t))

(defmethod pprint-spec-axioms (spec &key only-name (plist nil) with-colons &allow-other-keys)
  (let ((*print-case* :downcase))
    (map nil (lambda(e)
	       (if (formula-sexp-p e)
		   (format t "unnamed")
		   (format t "~%** ~a: " (axiom-name e)))
	       (unless only-name
		 (when plist
	       (if (formula-sexp-p e)
		   nil
		   (loop for ((k v) . more) on (axiom-plist e) do (format t "~a:~a" k v) (when more (format t ", ")))))
	       (terpri)
	       (let ((*print-pretty* t))
		 (if with-colons
		 (format t "~%~s" (axiom-sexp e))
		 (format t "~%~a" (axiom-sexp e))))
	       (terpri)))
	 (logic::collect-axioms-from-spec (if (symbolp spec) (list spec) spec))
	 )))

(defparameter *autonamed-axioms* (make-hash-table :test 'equalp :weakness :key))
(defvar *axiom-counter* 0)

(defmethod axiom-name ((a list))
  (assert (formula-sexp-p a) (a) "Axioms should be objects of formula sexps: ~a" a)
  (or (call-next-method)
      (let ((name (substitute #\- #\space (format nil "formula-~r" (incf *axiom-counter*)))))
	(setf (gethash (intern (string-upcase name) :keyword) *autonamed-axioms*) a)
	(setf (slot-value a 'name)  name)
	name)))

(defmethod axiom-name :around ((a axiom))
  (or (call-next-method)
      (let ((name (substitute #\- #\space (format nil "formula-~r" (incf *axiom-counter*)))))
	(setf (gethash (intern (string-upcase name) :keyword) *autonamed-axioms*) (axiom-sexp a))
	(setf (slot-value a 'name)  name)
	name)))
	

;; return predicates, constants, function symbols in formula
(defun formula-elements (sexp)
  (let ((predicates nil)
	(constants nil)
	(variables nil)
	(functions nil)
	(exp (axiom-sexp sexp))) ;; so macroexpansion happens
    (labels ((uses-constant (sym) (pushnew sym constants))
	     (uses-predicate (sym args) (pushnew (list sym (length args)) predicates :test 'equalp))
	     (uses-function (sym) (pushnew sym functions))
	     (uses-variable (sym) (pushnew sym variables))
	     (walk-function (form)
	       (uses-function (car form))
	       (walk-terms (cdr form)))
	     (walk-terms (form)
	       (loop for el in form
		     do
			(cond ((and (symbolp el) 
				    (if (char= (char (string el) 0 ) #\?)
					(uses-variable el)
					(uses-constant el))))
			      ((atom el))
			      (t (walk-function el)))))
	     (walk (form)
	       (cond ((atom form)
		      (break "shouldn't be here")
		      (if (and (symbolp form) (char= (char (string form) 0 ) #\?))
			  nil
			  (uses-constant form)))
		     (t (case (car form)
			  ((:forall :exists) (walk (third form)))
			  ((:implies :iff :and :or :not  :fact) (map nil #'walk (rest form)))
			  ((:distinct :=) (walk-terms (rest form)))
			  (otherwise
			   (uses-predicate (car form) (Rest form))
			   (walk-terms (rest form))))))))
      (walk exp)
      (values predicates constants functions variables))))
