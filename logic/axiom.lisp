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

(defmacro def-logic-axiom (name sexp &optional description &rest key-values)
  `(progn
     (sys::record-source-information-for-type  ',name 'def-logic-axiom)
     (setf (gethash (intern (string ',name) 'keyword) *axioms*)
	   (make-instance 'axiom :sexp 
			  (if  (and (consp ',sexp) (keywordp (car ',sexp)))
			       ',sexp
			       (list :fact ',sexp))
			  :description ,description :name ',name
			  :plist ',(loop for (k v) on key-values by #'cddr collect (list k (if (symbolp k) (intern (string v) :keyword) k)))))))

(defmethod print-object ((a axiom) stream)
  (print-unreadable-object (a stream :type nil :identity nil)
    (format stream "axiom ~a: ~a" (axiom-name a) 
	    (if (and (consp (axiom-sexp a))
		     (eq  (car (axiom-sexp a)) :fact))
		(second (axiom-sexp a))
		(axiom-sexp a)))))

(defun get-axiom (name &optional (errorp t))
  (if (typep name 'axiom)
      name
      (let ((found (gethash (intern (string name) 'keyword) *axioms*)))
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

(defun collect-axioms-from-spec (specs)
  "specs: either a single formuala or an axiom name or a list each element of which is either an axiom name, a list of key values, or a logic sexp. Output a list of axioms or unchanged logic sexps"
  (if (null specs) nil)
  (let* ((nots (remove-if-not (lambda(e) (and (consp e) (eq (car e) :exclude))) specs))
	 (specs (set-difference specs nots :test 'equalp)))
    (remove-duplicates 
     (set-difference
      (if (formula-sexp-p specs) 
	  (list specs)
	  (if (keywordp specs)
	      (list (get-axiom specs))
	      (loop for spec in specs
		    if (formula-sexp-p spec)
		      collect spec
		    else if (atom spec)
			   collect (get-axiom spec)
		    else if (eq (car spec) :negate)
			    collect (negate-axiom (get-axiom (second spec)) )
		    else append (apply 'get-axioms spec))))
      (and nots (collect-axioms-from-spec (mapcar 'second nots)))
      :test 'equalp)
     :test 'equalp)))

(defmethod predicates ((a axiom))
  (predicates (axiom-sexp a)))

(defmethod constants ((a axiom))
  (constants (axiom-sexp a)))

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
  (let ((usual (call-next-method)))
    (if (tree-find :expand usual)
	(tree-replace (lambda(e) (if (and (consp e) (eq (car e) :expand)) (macroexpand (second e)) e)) usual)
	usual)))
			       
(defmethod axiom-sexp ((a list))
  (if (tree-find :expand a)
      (tree-replace (lambda(e) (if (and (consp e) (eq (car e) :expand)) (macroexpand (second e)) e)) a)
      a))

(defmethod negate-axiom ((a list)) `(:not ,a))

(defmethod negate-axiom ((a axiom))
  (make-instance 'axiom 
		 :sexp `(:not ,(axiom-sexp a))
		 :name (intern (concatenate 'string "NEGATED-" (string (axiom-name a))) 'keyword)
		 :description (concatenate 'string "(negated) " (axiom-description a))
		 :from a))
			
(defmethod pprint-spec-axioms (spec)
  (map nil (lambda(e)
		 (if (formula-sexp-p e)
		     (format t "unnamed")
		     (format t "~a: " (axiom-name e)))
	     (if (formula-sexp-p e)
		 nil
		 (loop for ((k v) . more) on (axiom-plist e) do (format t "~a:~a" k v) (when more (format t ", "))))
	     (terpri)
	     (let ((*print-pretty* t))
	       (format t "~a" (axiom-sexp e)))
	     (terpri))
       (logic::collect-axioms-from-spec spec)
       ))
