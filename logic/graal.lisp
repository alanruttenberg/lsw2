(in-package :logic)

(defclass graal-kb ()
  ((kb :accessor kb :initform nil)
   (dlgp-parser :accessor dlgp-parser :initform (new 'dlgpparser ""))
   (rules :accessor rules :initarg rules)
   (store :accessor store :initform (new 'DefaultInMemoryGraphStore))
   (mangled-labels :accessor mangled-labels :initform (make-hash-table :test 'equalp))
   (rule-names :accessor rule-names :initform (make-hash-table :test 'equalp))
   (with-inequality :accessor with-inequality :initarg :with-inequality :initform nil)
   (with-equality :accessor with-equality :initarg :with-equality :initform nil)))
   
;; Future enhancement: allow rules with equality or inequality assuming unique name assumption
;; Graal doesn't support this natively. It could if allowed one to assert the unique name assumption
;; 
;; We could, in theory handle this ourselves in the rule system by adding assertions (equal x x) for all elements of the
;; universe, and adding (different x y) for x,y in universe, and translating clauses that use equality or inequality
;; into clauses that use these pseudo-equality predicates

;; This gets called the first time the kb is needed, in order that the calls to add facts and add rules have been
;; done. It uses the KbBuilder since that seems to be the only documented way to set the Approach. Probably should 

(defmethod maybe-instantiate-kb ((kb graal-kb))
  (unless (kb kb)
    (let ((builder (new 'kbbuilder)))
      (#"setStore" builder (store kb))
      (#"setOntology" builder (rules kb))
;      (#"setApproach" builder #1"Approach.SATURATION_ONLY")
      (setf (kb kb) (#"build" builder)))))


;; Mangling

;; Our usual symbols come in but need to be mangled to fit into the syntax for dlgp.
;; Labels can only be  letters, numbers and underscore and space
;; Variables can only be letters, numbers and underscore but must start with a capital letter
;; Predicates and terms can either letters, numbers and underscore, or essentially anything surrounded by <>

;; For labels we change any illegal characters to _, and save the original in a hash table. If two labels differ by only
;; one illegal character in the same place you lose, sorry.

;; For variables we check that it's legal and complain otherwise

;; For predicates and terms we want to preserve the package the originals were in.
;; We print them using format ~s, then, if they are simple we leave them as is, otherwise
;; surround them with <>

;; Unmangle is the inverse, but noting that the <> aren't part of the identifier returned, and so don't need to be removed.

(defmethod dlgp-mangle-label ((kb graal-kb) label)
  (let ((it (#"replaceAll" (string label) "[^0-9,A-Z,a-z, ]" "_")))
    (setf (gethash it (mangled-labels kb)) label)
    it))

(defmethod dlgp-unmangle-label ((kb graal-kb) label)
  (gethash label (mangled-labels kb)))

(defmethod dlgp-mangle-symbol ((kb graal-kb) symbol)
  (let ((*print-case* :downcase))
    (if (char= (char (string symbol) 0) #\?)
	(let ((it (subseq (string symbol) 1)))
	  (assert (#"matches" it "[A-Za-z-_][A-Za-z0-9-_]*") () "Variable names can only start with a letter and be alphanumeric, but got ~a" symbol)
	  (setf (char it 0) (char-upcase (char it 0)))
	  (setq it (#"replaceAll" it "-" "_"))
	  (format nil "~a" it))
	(let ((*print-escape* t))
	  (let ((printed (format nil "~s" symbol)))
	    (if (#"matches" printed "^[A-Za-z][A-Za-z0-9_]*$")
		printed
		(format nil "<~a>" printed)))))))

;; takes either a prop or a list of props
(defmethod dlgp-mangle-form ((kb graal-kb) form)
  (if (consp (car form))
      (loop for prop in form collect (dlgp-mangle-form kb prop))
      (mapcar (lambda (e) (dlgp-mangle-symbol kb e)) form)))

(defmethod dlgp-unmangle-identifier ((kb graal-kb) string)
  (read-from-string  string))

;; Setting up the KB.
;; Graal supports incremental adding and removal of facts and rules IIRC but we don't bother.
;; The first time we need the kb we create it from the rules and facts.
;; If we set the rules or facts again the kb is removed and recreated.

;; See translate-rule-to-dlgp for format of rules
(defmethod set-rules ((kb graal-kb) rules)
  (setf (kb kb) nil)
  (let ((list (setf (rules kb) (new 'linkedlistruleset))))
    (loop for rule in rules
	  for dlgp = (if (stringp rule) rule (translate-rule-to-dlgp kb rule))
	  do (#"add" list (#"parseRule" 'dlgpparser dlgp)))
    list))

;; A fact is either a list of atoms or a list of a lable and a list of atoms
(defmethod set-facts ((kb graal-kb) facts)
  (setf (kb kb) nil)
  (let ((store (store kb)))
    (loop for fact in facts
	  do (if (consp (second fact))
		 (#"add" store (#"parseAtom" 'dlgpparser
					     (format nil "[~a] ~a(~{~a~^,~})."
						     (dlgp-mangle-label kb (car fact))
						     (dlgp-mangle-symbol kb (car (second fact)))
						     (dlgp-mangle-form (rest (second fact))))))
		 (#"add" store (#"parseAtom" 'dlgpparser
					     (format nil "~a(~{~a~^,~})."
						     (dlgp-mangle-symbol kb (car fact))
						     (dlgp-mangle-form kb (rest fact)))))))

    (when (with-equality kb)
      (let ((ground (reduce 'union (mapcar 'cdr facts))))
	(loop for g in ground
	      do (#"add" store (#"parseAtom" 'dlgpparser
					     (format nil "~a(~{~a~^,~})."
						     (dlgp-mangle-symbol kb 'same)
						     (dlgp-mangle-form kb (list g g))))))))
    (when (with-inequality kb)
      (let ((ground (reduce 'union (mapcar 'cdr facts))))
	(loop for (g1 . rest) on ground
	      do
		 (loop for g2 in rest
		       when (not (eq g1 g2))
			 do (#"add" store (#"parseAtom" 'dlgpparser
							(format nil "~a(~{~a~^,~})."
								(dlgp-mangle-symbol kb 'different)
								(dlgp-mangle-form kb (list g1 g2)))))))))))

;; rules are either:
;; A string, in which case it is expected that it is a DLGP format rule
;; (:implies (:and ...) consequent)
;; (:implies antecedent consequent)
;; (:implies antecedents consequent)
;; Any of the above can be optionally wrapped (<label> ...)
;; (antecedents conseqeuent)
;; (<label> antecedents conseqeuent)
;; Where an antecedent or consequent is a single proposional expression
;; Variables are assumed to be universally quantified

(defmethod translate-rule-to-dlgp ((kb graal-kb) rule &optional label)
  (when (and (atom (car rule)) (not (eq (car rule) :implies)))
    (setq label (first rule))
    (if (= (length (rest rule)) 1)
	(setq rule (second rule))
	(setq rule (rest rule))))
  (when (eq (car rule) :implies)
    (setq rule (cdr rule)))
  (let ((antecedents (cond ((eq (first (first rule)) :and)
			    (rest (first rule)))
			   ((keywordp (first (first rule)))
			    (error "antecedent can only be a conjunction: ~s" (first rule)))
			   ((atom (first (first rule)))
			    (list (first rule)))
			   (t (first rule))))
	(consequent (second rule)))
    (assert (not (some 'consp consequent)) (consequent) "Consequent isn't a propositional expression: ~s" consequent)
    (map nil (lambda(el) (assert (not (some 'consp el)) (antecedents) "Antecedent isn't a propositional expression: ~s" el)) antecedents)
    (let ((body (dlgp-mangle-form kb antecedents))
	  (head (dlgp-mangle-form kb consequent))
	  (label (and label (dlgp-mangle-label kb label))))
      (when label
	(let ((label-uniq (incf (gethash label (rule-names kb) 0))))
	  (if (> label-uniq 1)
	      (setq label (format nil "~a_~a" label label-uniq)))))
      (format nil "~a~a(~{~a~^,~}) :- ~{~a~^,~}."
	      (if label (format nil "[~a] " label) "") 
	      (car head) (rest head)
	      (mapcar (lambda(form) 
			(if (equal (car form) "=")
			    (format nil "~a = ~a " (second form) (third form))
			    (format nil "~a(~{~a~^,~})" (car form) (rest form)))) body)))))


;; Get all predicates used either in a rule or a fact
(defmethod all-predicates ((kb graal-kb))
  (maybe-instantiate-kb kb)
  ;; Need to get the ones in the KB as well as the ones in the rules, since you might call this before the database is
  ;; saturated.
  (let ((from-facts (j2list (#"getPredicates" (#"getFacts" (kb kb)))))
	(from-rules (loop for rule across (#"toArray" (rules kb))
			  append
			  (cl-user::set-to-list (#"getPredicates" (#"getHead" rule)))
			  append
			  (cl-user::set-to-list (#"getPredicates" (#"getBody" rule))))))
    (remove '=
	    (remove-duplicates
	     (mapcar (lambda(e) (list (dlgp-unmangle-identifier kb (#"toString" (#"getIdentifier" e))) (#"getArity" e) e))
		     (append from-facts from-rules))
	     :test (lambda(a b) (and (eq (car a) (car b)) (eq (second a) (second b)))))
	    :key 'car)))

;; Execute a query against the KB using the DLGP query syntax
;; Returns the values of the variables in the order they are given in the query
(defmethod dlgp-query ((kb graal-kb) query)
  (let ((q (#"parseQuery" 'dlgpparser query)))
    (let ((results (#"query" (kb kb) q)))
      (setq @ results)
      (break)
      (loop while (#"hasNext" results)
	    for res = (#"next" results)
	    for values = (mapcar (lambda(e) (dlgp-unmangle-identifier kb e))
				 (mapcar #"toString"
					 (mapcar #"getIdentifier"
						 (j2list (#"createImageOf" res (#"getTerms" res))))))
	    collect values
	    ))))

;; Two ways to get all the facts (including inferred).
;; 1) By calling saturate, and then getting them from the store
;; 2) By querying one predicate at a time.
;; Don't really need 2 anymore. Wrote it when saturate had a bug.
(defmethod get-all-facts ((kb graal-kb) &optional by-query)
  (if by-query
      (get-all-facts-by-query kb)
      (get-all-facts-from-store kb)))

;; Get all the facts by accumulating query results for one predicate at a time.
;; Should have same result as get-saturated
(defmethod get-all-facts-by-query ((kb graal-kb))
  (maybe-instantiate-kb kb)
  (let ((res 
	  (loop with vars = (mapcar 'string '(x y z w u v r s))
		for (pred arity ) in (all-predicates kb)
		for results =  (#"query" (kb kb) (#"parseQuery" 'dlgpparser (format nil "?(~{~a~^,~}) :- ~a(~{~a~^,~}).~%" (subseq vars 0 arity) (dlgp-mangle-symbol kb pred) (subseq vars 0 arity))))
		append 
		(loop while (#"hasNext" results)
		      for res = (#"next" results)
		      for values = (mapcar (lambda (e) (dlgp-unmangle-identifier kb e)) (mapcar #"toString" (mapcar #"getIdentifier" (j2list (#"createImageOf" res (#"getTerms" res))))))
		      collect (cons pred values)
		      ))))
    res))

(defmethod get-all-facts-from-store ((kb graal-kb))
  (maybe-instantiate-kb kb)
  (#"saturate" (kb kb)) 
  (loop for fact in (iterable-to-list (#"getFacts" (logic::kb kb)))
	collect (cons (dlgp-unmangle-identifier kb (#"toString" (#"getIdentifier"(#"getPredicate" fact))))
		      (mapcar (lambda(e) (dlgp-unmangle-identifier kb e))
			      (mapcar #"toString" (map 'list #"getIdentifier" (#"toArray" (#"getTerms" fact))))))))


(defun test-graal ()
  (let ((kb (make-instance 'graal-kb :with-equality t :with-inequality t)))
    (set-facts kb '((p x) (q x) (s y)))
    (set-rules kb '((((p ?x) (q ?x)) (r ?x))))
    (assert (member '(r x) (get-all-facts kb) :test 'equalp) () "simple graal test (saturate) failed")
    (assert (member '(r x) (get-all-facts kb t) :test 'equalp) () "simple graal test (query) failed")
    kb))

;; Returns a string with the dglp that if parsed would yield the kb
(defmethod kb-to-dglp ((kb graal-kb))
  (let* ((s (jss::new 'stringwriter))
	 (wr (jss::new 'dlgpwriter s)))
    (loop for fact in (iterable-to-list (#"getFacts" (kb kb)))
	  do (#"write" wr fact))
    (#"write" wr (get-java-field (kb kb) "ruleset" t))
    (#"toString" s)))

;; couple of better print methods

;; For predicates
(defmethod java::print-java-object-by-class ((class (eql :|fr.lirmm.graphik.graal.core.atomset.graph.PredicateVertex|)) obj stream)
    (print-unreadable-object (obj stream :type nil :identity nil)
      (format stream "predicate ~a/~a" (#"getIdentifier" obj) (#"getArity" obj))))

;; For bindings/"substitutions"
(defmethod java::print-java-object-by-class ((class (eql :|fr.lirmm.graphik.graal.core.HashMapSubstitution|)) obj stream)
  (print-unreadable-object (obj stream :type nil :identity nil)
    (let ((keys (mapcar #"getIdentifier" (j2list (#"getTerms" obj))))
	  (values (mapcar #"toString" (mapcar #"getIdentifier" (j2list (#"getValues" obj))))))
      (format stream "bindings ~{~a~^,~}" (loop for key in keys for value in values
	    collect (format nil "~a=~a" key (read-from-string (#"toString" value))))))))


