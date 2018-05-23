(in-package :logic)

(defclass graal-kb ()
  ((kb :accessor kb :initform nil)
   (dlgp-parser :accessor dlgp-parser :initform (new 'dlgpparser ""))
   (rules :accessor rules :initform (jss::new 'linkedlistruleset) :initarg rules)
   (store :accessor store :initform (jss::new 'DefaultInMemoryGraphStore))
   (graal-rules :accessor graal-rules :initform (new 'linkedlistruleset))
   (rule-names :accessor rule-names :initform (make-hash-table :test 'equalp))))
   
;; This gets called the first time the kb is needed, in order that the calls to add facts and
;; add rules have been done. It uses the KbBuilder since that seems to be the only documented way
;; to set the Approach.

(defmethod maybe-instantiate-kb ((kb graal-kb))
  (unless (kb kb)
    (let ((builder (new 'kbbuilder)))
      (#"setStore" builder (store kb))
      (#"setOntology" builder (rules kb))
      (#"setApproach" builder #1"Approach.SATURATION_ONLY")
      (setf (kb kb) (#"build" builder)))))

;; Our usual symbols come in but need to be mangled to fit into the syntax for dlgp,
;; which only allows letters, numbers and underscore (and spaces in labels)
;; We camelcase as usual but also prepend "n" if the term starts with a number
;; since that's not allowed in dlgp.
;; Rewrites symbols to strings
(defun dlgp-mangle (sexp)
  (cl-user::tree-replace (lambda(el) 
			   (cond ((and (symbolp el) (char= (char (string el) 0) #\?))
				  (let ((it (dlgp-mangle (intern (subseq (string el) 1)))))
				    (setf (char it 0) (char-upcase (char it 0)))
				    it))
				 ((symbolp el) 
				  (let ((it (substitute #\_ #\. (camelCase (#"replaceAll" (string el) "[^0-9,A-Z,a-z,=-]" ".")))))
				    (when (digit-char-p (char it 0)) (setq it (concatenate 'string "n" it)))
				    it))
				 (t el)))
			 sexp))

;; Invert the mangling (to the extent possible). 
(defun dlgp-unmangle (term)
  (when (#"matches" term "^n(\\d+\\S+)$")
    (setq term (subseq term 1)))
  (intern (de-camel-case term)))
				       
;; Parses and save the parsed rules, so they can be used when we create the KB.
(defmethod add-rules ((kb graal-kb) rules)
  (let ((list (rules kb)))
    (loop for rule in rules
	  for dlgp = (translate-to-dlgp kb rule)
	  do (#"add" list (#"parseRule" 'dlgpparser dlgp)))
    list))

;; Parses and save the facts, so they can be used when we create the KB.
;; The fact is either a list of atoms or a list of a lable and a list of atoms
(defmethod add-facts ((kb graal-kb) facts)
  (let ((store (store kb)))
    (loop for fact in facts
	  do (if (consp (second fact))
		 (#"add" store (#"parseAtom" 'dlgpparser
					     (format nil "[~a] ~a(~{~a~^,~})."
						     (dlgp-mangle (car fact))
						     (dlgp-mangle (car (second fact)))
						     (dlgp-mangle (rest (second fact))))))
		 (#"add" store (#"parseAtom" 'dlgpparser
					     (format nil "~a(~{~a~^,~})."
						     (dlgp-mangle (car fact))
						     (dlgp-mangle (rest fact)))))))))

;; Work around a bug in which saturate gets a null pointer exception if called before a query is done.

(defvar +fes-saturate+
  (load-time-value
   (let ((it (find "fesSaturate" (#"getDeclaredMethods" (jss::find-java-class 'graal.kb.DefaultKnowledgeBase)) :key #"getName" :test 'string-equal)))
     (#"setAccessible" it t)
     it)))

(defmethod get-saturated ((kb graal-kb))
  (maybe-instantiate-kb kb)
  (java::jcall +fes-saturate+ (kb kb))
;  (#"saturate" (kb kb)) ; crashes
  (loop for fact in (jss::iterable-to-list (#"getFacts" (logic::kb kb)))
	collect (cons (intern (dlgp-unmangle (#"getIdentifier"(#"getPredicate" fact))))
			      (mapcar 'dlgp-unmangle
				      (map 'list #"getIdentifier" (#"toArray" (#"getTerms" fact)))))))


;; rules are either:
;; (:implies (:and ...) consequent)
;; (:implies antecedent consequent)
;; (:implies antecedents consequent)
;; Any of the above can be optionally wrapped (<label> ...)
;; (antecedents conseqeuent)
;; (<label> antecedents conseqeuent)
;; Where an antecedent or consequent is a single proposional expression
;; Variables are assumed to be universally quantified

(defmethod translate-to-dlgp ((kb graal-kb) rule &optional label)
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
    (let ((body (dlgp-mangle antecedents))
	  (head (dlgp-mangle consequent))
	  (label (and label (dlgp-mangle label))))
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


;; Get all predicates 
(defmethod all-predicates ((kb graal-kb))
  (maybe-instantiate-kb kb)
  ;; Need to get the ones in the KB as well as the ones in the rules, since you might call this before the database is
  ;; saturated.
  (let ((from-facts (jss::j2list (#"getPredicates" (#"getFacts" (kb kb)))))
	(from-rules (loop for rule across (#"toArray" (rules kb))
			  append
			  (cl-user::set-to-list (#"getPredicates" (#"getHead" rule)))
			  append
			  (cl-user::set-to-list (#"getPredicates" (#"getBody" rule))))))
    (remove '=
	    (remove-duplicates
	     (mapcar (lambda(e) (list (dlgp-unmangle (#"getIdentifier" e)) (#"getArity" e) e))
		     (append from-facts from-rules))
	     :test (lambda(a b) (and (eq (car a) (car b)) (eq (second a) (second b)))))
	    :key 'car)))

;; Execute a query against the KB using the DLGP query syntax
;; Returns the values of the variables in the order they are given in the query
(defmethod dlgp-query ((kb graal-kb) query)
  (let ((q (#"parseQuery" 'dlgpparser query)))
    (let ((results (#"query" (kb kb) q)))
      (loop while (#"hasNext" results)
	    for res = (#"next" results)
	    for values = (mapcar 'dlgp-unmangle (mapcar #"getIdentifier" (jss::j2list (#"createImageOf" res (#"getTerms" res)))))
	    collect values
	    ))))

;; Get all the facts by accumulating query results for one predicate at a time.
;; Should have same result as get-saturated
(defmethod get-all-facts ((kb graal-kb))
  (maybe-instantiate-kb kb)
  (cl-user::with-constant-signature ((has-next "hasNext")
				     (next "next"))
    (let ((res 
	    (loop with vars = (mapcar 'string '(x y z w u v r s))
		  for (pred arity ) in (all-predicates kb)
		  for results =  (setq @@@@ (#"query" (kb kb) (#"parseQuery" 'dlgpparser (format nil "?(~{~a~^,~}) :- ~a(~{~a~^,~}).~%" (subseq vars 0 arity) (dlgp-mangle pred) (subseq vars 0 arity)))))
		  append 
		  (loop while (has-next results)
			for res = (next results)
			for values = (mapcar 'dlgp-unmangle (mapcar #"getIdentifier" (jss::j2list (#"createImageOf" res (#"getTerms" res)))))
			collect (cons pred values)
			))))
      res)))


(defmethod java::print-java-object-by-class ((class (eql :|fr.lirmm.graphik.graal.core.atomset.graph.PredicateVertex|)) obj stream)
    (print-unreadable-object (obj stream :type nil :identity nil)
      (format stream "predicate ~a/~a" (#"getIdentifier" obj) (#"getArity" obj))))

(defmethod java::print-java-object-by-class ((class (eql :|fr.lirmm.graphik.graal.core.HashMapSubstitution|)) obj stream)
  (print-unreadable-object (obj stream :type nil :identity nil)
    (let ((keys (mapcar #"getIdentifier" (jss::j2list (#"getTerms" obj))))
	  (values (mapcar #"getIdentifier" (jss::j2list (#"getValues" obj)))))
      (format stream "~a ~{~a~^,~}" (#"toString" obj) (loop for key in keys for value in values
	    collect (format nil "~a=~a" key (dlgp-unmangle value)))))))

