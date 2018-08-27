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
     (validate-formula-well-formed ',sexp ',name)
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
						     collect (list k (if (symbolp v)
									 (keywordify v)
									 v))))))))))

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

(defun get-axioms ( &rest key-values &key (errorp t) (from *axioms*)  &allow-other-keys)
  (remf key-values :errorp)
  (remf key-values :from)
  (let ((them nil))
    (if (hash-table-p from)
	(maphash (lambda(name axiom)
		   (declare (ignore name))
		   (when
		       (loop for (k v) on key-values by #'cddr
			     always (check-key-spec k v (axiom-plist axiom))) ;(find (list k v) (axiom-plist axiom) :test 'equalp))
		     (push axiom them)))
		 *axioms*)
	(loop for axiom in from
	      when
	      (loop for (k v) on key-values by #'cddr
		    always (check-key-spec k v (axiom-plist axiom)))
	      do (push axiom them)))
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
  "specs: either a single formula or an axiom name or a list each element of which is either an axiom name, a list of key values logic sexp, or a macro to be expanded. Output a list of axioms or unchanged logic sexps"
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
		    unless (null spec)
		      if (formula-sexp-p spec)
		      collect spec
		    else if (atom spec)
			   append (let ((res (get-axiom spec error-if-not-found)))
				     (if res (list res)))
		    else if (eq (car spec) :negate)
			    collect (negate-axiom (get-axiom (second spec) error-if-not-found) )
		    else if (keywordp (car spec))
			   append (apply 'get-axioms (append spec (list :errorp error-if-not-found)))
		    else collect `(:expand ,spec ))))
      (and nots (collect-axioms-from-spec (apply 'append (mapcar 'cdr nots)) nil))
      :test 'equalp)
     :test 'equalp)))

(defun collect-axiom-names-from-spec (specs &optional (error-if-not-found t))
  (loop for a in (collect-axioms-from-spec specs error-if-not-found)
	if (typep a 'axiom) collect (axiom-name a) else collect a))

(defun subjects-of (spec &optional (error-if-not-found t))
  (remove-duplicates (loop for ax in (collect-axioms-from-spec spec error-if-not-found)
	for subject = (and (typep ax 'axiom) (second (assoc :subject (axiom-plist ax))))
			   when subject collect subject)))

(defun plist-keyword-of (keyword spec)
  (remove-duplicates (loop for ax in (collect-axioms-from-spec spec)
	for value = (and (typep ax 'axiom) (second (assoc keyword (axiom-plist ax))))
			   when value collect value)))

(defun is-axiom-in-spec (axiom spec)
  (find axiom (mapcar 'axiom-name (collect-axioms-from-spec spec)) :test 'string-equal))

(defun spec-difference (spec1 spec2)
  (loop for el in  (set-difference (collect-axioms-from-spec spec1 ) (collect-axioms-from-spec spec2 ))
	if (typep el 'axiom) collect (axiom-name el) else collect el))

(defun rewrite-to-axiom-generation-form (form)
  (let ((keys '((:distinct l-distinct) (:implies l-implies) (:iff l-iff) (:and l-and) (:or l-or) (:forall l-forall) (:exists l-exists)
		(:not l-not) (:= l-=) (:fact l-fact) (:owl l-owl) (:parens l-parens))))
    (labels ((rewrite (expression)
	       (cond ((and (consp expression) (member (car expression) keys :key 'car))
		      `(,(second (assoc (car expression) keys))
			,@(mapcar (lambda(e) 
				    (if (and (consp e) (or (find (car e) keys :key 'car) (find (car e) keys :key 'second)))
					e
					(if (and (consp e) (not (logic-var-p (car e))))
					    `(l-relation ,@(mapcar (lambda(el) (list 'quote el))  e))
					    `(quote ,e))))
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
  (when (tree-find :axioms a)
    (setq a (tree-replace (lambda(e) (if (and (consp e) (eq (car e) :axioms))
					 `(:and ,@(mapcar 'axiom-sexp (mapcar 'get-axiom (rest e))))
					 e))
			  a)))
  (when (tree-find :owl a)
    (setq a (tree-replace (lambda(e) (if (and (consp e) (eq (car e) :owl))
					 (owl-sexp-to-fol (second e))
					 e)) a)))
  a)

(defmethod axiom-sexp ((s symbol))
  (axiom-sexp (get-axiom s)))

(defmethod negate-axiom ((a list)) `(:not ,a))

(defmethod negate-axiom ((a symbol)) `(:not ,(get-axiom a)))

(defmethod negate-axiom ((a axiom))
  (make-instance 'axiom 
		 :sexp `(:not ,(axiom-sexp a))




		 :name (intern (concatenate 'string "NEGATED-" (string (axiom-name a))) 'keyword)
		 :description (concatenate 'string "(negated) " (axiom-description a))
		 :from a))

(defmethod pprint-spec-axiom-names (spec &rest args &key  &allow-other-keys)
  (apply 'pprint-spec-axioms spec 
	 :only-name t args))

(defun pprint-spec-axioms (spec &key only-name (plist nil) with-colons (allow-missing t) &allow-other-keys)
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
	 (sort (logic::collect-axioms-from-spec (if (symbolp spec) (list spec) spec) (not allow-missing))
	       'string-lessp :key 'prin1-to-string)
	 )))

(defparameter *autonamed-axioms* (make-hash-table :test 'equalp :weakness :key))
(defvar *axiom-counter* 0)

(defmethod axiom-name ((a list))
  (assert (formula-sexp-p a) (a) "Axioms should be objects of formula sexps: ~a" a)
  (or (and (next-method-p) (call-next-method))
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
	
(defun spec-elements (spec)
  (formula-elements `(:and ,@(mapcar 'axiom-sexp (collect-axioms-from-spec spec)))))

;; return predicates, constants, function symbols in formula
(defun formula-elements (sexp)
  ;; expecting an sexp but accept a spec.
  ;; sexp might be missing :fact
  ;; spec can be '(:name ...) '((:kind ..))) ((:forall ..))
  ;; it's a spec if
  ;;  1) it starts with a keyword other than :forall ...
  ;;  2) the first element is an axiom
  ;;  3) The first element is a list
  (when (or (and (keywordp (car sexp))
		 (not (member (car sexp) '(:forall :exists :and :or :iff :implies := :not))))
	    (consp (car sexp))
	    (typep (car sexp) 'axiom))
    (setq sexp `(:and ,@(mapcar 'axiom-sexp (collect-axioms-from-spec sexp)))))
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
		      (break "shouldn't be here: ~a" form)
		      (if (and (symbolp form) (char= (char (string form) 0 ) #\?))
			  nil
			  (uses-constant form)))
		     (t (case (car form)
			  ((:forall :exists) (map nil #'walk (cddr form)))
			  ((:axiom) (walk (axiom-sexp (get-axiom (second form)))))
			  ((:implies :iff :and :or :not  :fact) 
			   (map nil #'walk (rest form)))
			  ((:distinct :=) (walk-terms (rest form)))
			  (otherwise
			   (uses-predicate (car form) (Rest form))
			   (walk-terms (rest form))))))))
      (walk exp)
      (values predicates constants functions variables))))

(defun definition-p (a)
  (cond ((formula-sexp-p a)
	 (when (eq (car a) :forall)
	   (when (eq (car (third a)) :iff)
	     (or (not (keywordp (car (second (third a)))))
		 (not (keywordp (car (third  (third a)))))))))
	((typep a 'axiom)
	 (definition-p (axiom-sexp a)))
	(t nil)))

;; flatten reduce nested :and or :or 
(defun simplify-and-or (form)
  (flet ((consolidate-and-or (e type)
	   (let ((result 
		   (if (and (consp e) (eq (car e) type))
		       (let ((queue (cdr e)))
			 (loop for next = (pop queue)
			       until (null next)
			       if (and (consp next)
				       (eq (car next) type))
				 do (setq queue (append queue (cdr next)))
			       else  collect (simplify-and-or next) into conjuncts
			       finally (return `(,type ,@conjuncts))))
		       e)))
	     result)))
    (tree-replace (lambda(e) (consolidate-and-or e :or))
		  (tree-replace (lambda(e) (consolidate-and-or e :and))
				form))))

(defun rename-variables (expression)
  (tree-replace (lambda(e) (if (and (symbolp e) (char= (char (string e) 0) #\?))
			       (intern (format nil "?VARIABLE-~a" (subseq (string e) 1)))
			       e))
		expression))

;; we have to rename variables so that we can compare incorrect formulas - ones which leave a variable out of scope
(defun equivalent-formulas (a b &key (with 'z3-prove))
  (or (equalp a b)
      (multiple-value-bind (errorp res)	
	  (ignore-errors (funcall with nil `(:iff ,(rename-variables a) ,(rename-variables b))))
	(or (and (typep errorp 'condition)
		 (values nil (apply 'format nil (slot-value errorp 'sys::format-control) (slot-value errorp 'sys::format-arguments))))
	    (eq errorp :proved))) 
      ))


;; yy should do this with a pattern lang
;; For non-binary relations expects the quantified variables that are swapped to be the LAST in the first :forall 
(defun does-formula-express-inverse (formula &aux vars )
  (let ((sexp (axiom-sexp formula)))

    (and
     (eq (car sexp) :forall)
     (setq vars (last (second sexp) 2))
     (eq (car (third sexp)) :iff)
     (let ((lhs (second (third sexp)))
	   (rhs (third (third sexp))))
       (and (equal (length lhs) (length rhs))
	    (equal (subseq lhs 1 3) vars)
	    (equal (subseq rhs 1 3) (reverse vars))
	    (equalp (cdddr lhs) (cdddr rhs))
	    (list (first lhs) (first rhs) (if (cdddr rhs) :ternary :binary))
	    )))))

(defun inverses-from-spec (spec)
  (let ((formulas (collect-axioms-from-spec spec)))
    (loop for formula in formulas
	  for (r inverse-r kind) = (does-formula-express-inverse formula)
	  when (eq kind :binary) collect (list r inverse-r) into binaries
	    when (eq kind :ternary) collect (list r inverse-r) into ternaries
	      finally (return (values binaries ternaries)))))
			 
(defmethod is-ground ((a axiom))
  (is-ground (axiom-sexp a)))

(defmethod is-ground ((a list))
  (or (eq (car a) :fact)
      (multiple-value-bind (predicates constants functions variables)
	  (formula-elements (axiom-sexp a))
	(null variables))))

(defun free-variables (formula)
  (let ((bound nil)
	(free nil))
    (declare (special bound))
    (labels ((free-variables (formula)
	       (let ((bound (if (boundp 'bound) bound nil)))
		 (declare (special bound))
		 (cond ((and (consp formula) (member (car formula) '(:forall :exists)))
			(setq bound (append (second formula) bound))
			(apply 'append (mapcar #'free-variables (cddr formula))))
		       ((logic-var-p formula)
			(unless (member formula bound)
			  (pushnew formula free)))
		       ((atom formula) nil)
		       (t (apply 'append (mapcar #'free-variables  formula)))))
    
	       ))
      (remove-duplicates (free-variables formula)))))

(defun check-builtins-keyword (formula &optional label)
  (tree-walk formula (lambda(e)
		       (when (and (atom e) (not (keywordp e)) (symbolp e))
			 (assert (not (member (string e)
					      '(and or implies forall exists not distinct)
					      :test 'equalp :key 'string)) ()
				 "Used ~s vs ~s in ~s (reserved keyword)"
				 e (intern e 'keyword) (or label formula))))))

(defun validate-formula-well-formed (formula &optional label)
  (when (symbolp formula) 
      (setq formula (axiom-sexp (get-axiom formula))))
  (when (typep formula 'axiom)
      (setq formula (axiom-sexp formula)))
  (let ((free (free-variables formula)))
    (assert (null free) (formula) "~{~a~^, ~} ~a free in ~a" free (if (> (length free) 1) "are" "is") (or label formula))
    (let ((predicates (formula-elements formula)))
      (loop for (p) in predicates
	    if (> (count p predicates :key 'car) 1)
	      do (error "~a is used with different arities in ~a" p (or label formula))))
    (check-builtins-keyword formula label)
    (check-builtins-args formula label)
    ))

(defun check-builtins-args (formula &optional label &aux explain)
  (tree-walk formula (lambda(e)
		       (when (consp e)
			 (assert 
			  (case (car e)
			    ((:and :or :distinct)
			     (setq explain ":and, :or, or :distinct empty")
			     (not (null (cdr e))))
			    ((:forall :exists)
			     (setq explain "quanitified variables need to start with '?'")
			     (and (consp (second e)) (every 'logic-var-p (second e))))
			    ((:implies :=) 
			     (setq explain ":if, :iff, and := should have 2 arguments")
			     (= (length (cdr e)) 2)
			     )
			    (:not 
				(setq explain ":not takes 1 argument")
			      (= (length (cdr e)) 1)
			      )
			    (otherwise t))
			  ()
			  "~a ~anot well formed: ~a"
			  e (if label (format nil "in ~a " label) "") explain)))))
