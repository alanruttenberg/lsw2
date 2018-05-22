(in-package :logic)

(defclass graal-kb ()
  ((kb :accessor kb)
   (dlgp-parser :accessor dlgp-parser :initform (new 'dlgpparser ""))
   (facts :accessor facts :initform nil :initarg facts)
   (rules :accessor rules :initform nil :initarg rules)
   (graal-rules :accessor graal-rules :initform (new 'linkedlistruleset))
   (rule-names :accessor rule-names :initform (make-hash-table :test 'equalp))))

   
(defmethod initialize-instance  ((kb graal-kb) &key  &allow-other-keys)
  (call-next-method)
  (setf (kb kb) (new 'defaultKnowledgeBase (dlgp-parser kb)))
  (set-java-field (kb kb) "approach" #1"Approach.SATURATION_ONLY" t)
  (setq @ kb))

;(setq dkb (new 'defaultKnowledgeBase (new 'DlgpParser "p(X) :- q(X). q(X) :- r(X). r(X) :- s(X).  s(a).")))
;(set-java-field dkb "approach" #1"Approach.SATURATION_ONLY")

;; public void testSaturate() throws ParseException, AtomSetException, KnowledgeBaseException {
;; 		KnowledgeBase kb = new DefaultKnowledgeBase(
;; 				new DlgpParser("p(X) :- q(X). q(X) :- r(X). r(X) :- s(X).  s(a)."));
;; 		kb.saturate();
;; 		Assert.assertTrue(kb.getFacts().contains(DlgpParser.parseAtom("r(a).")));
;; 		Assert.assertTrue(kb.getFacts().contains(DlgpParser.parseAtom("q(a).")));
;; 		Assert.assertTrue(kb.getFacts().contains(DlgpParser.parseAtom("p(a).")));
;; 		kb.close();
;; 	}


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

(defun dlgp-unmangle (term)
  (when (#"matches" term "^n(\\d+\\S+)$")
    (setq term (subseq term 1)))
  (intern (de-camel-case term)))
				       

(defmethod add-rules ((kb graal-kb) rules)
  (let ((list (jss::get-java-field (kb kb) "ruleset" t)))
    (loop for rule in rules
	  for dlgp = (translate-to-dlgp kb rule)
	  unless nil;(find #\= dlgp :test 'char=)
	    do (#"add" list (#"parseRule" 'dlgpparser dlgp)))
    list))

(defmethod add-facts ((kb graal-kb) facts)
  (let ((store (jss::get-java-field (kb kb) "store" t)))
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

(defmethod get-saturated ((kb graal-kb))
  (#"saturate" (kb kb))
  (loop for fact in (jss::iterable-to-list (#"getFacts" (logic::kb kb)))
	collect (cons (intern (de-camel-case (#"getIdentifier"(#"getPredicate" fact))))
		      (mapcar 'intern
			      (mapcar 'de-camel-case
				      (map 'list #"getIdentifier" (#"toArray" (#"getTerms" fact))))))))
					

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


(defmethod all-predicates ((kb graal-kb))
  (mapcar (lambda(e) (list (#"getIdentifier" e) (#"getArity" e) e))
	  (jss::j2list (#"getPredicates" (#"getFacts" kb)))))

(defmethod dlgp-query ((kb graal-kb) query)
  (let ((q (#"parseQuery" 'dlgpparser query)))
    (let ((results (#"query" (kb kb) q)))
      (cl-user::with-constant-signature ((has-next "hasNext")
				(next "next"))
	(loop :while (has-next results)
	      :collect (next results)))
      )))

(defmethod get-predicates ((kb graal-kb))
    (mapcar (lambda(e) (list (#"getIdentifier" e) (#"getArity" e) e)) 
	    (jss::j2list (#"getPredicates" (#"getFacts" (logic::kb kb))))))
      
(defmethod java::print-java-object-by-class ((class (eql :|fr.lirmm.graphik.graal.core.atomset.graph.PredicateVertex|)) obj stream)
    (print-unreadable-object (obj stream :type nil :identity nil)
      (format stream "predicate ~a/~a" (#"getIdentifier" obj) (#"getArity" obj))))

(defmethod java::print-java-object-by-class ((class (eql :|fr.lirmm.graphik.graal.core.HashMapSubstitution|)) obj stream)
  (print-unreadable-object (obj stream :type nil :identity nil)
    (let ((keys (mapcar #"getIdentifier" (jss::j2list (#"getTerms" obj))))
	  (values (mapcar #"getIdentifier" (jss::j2list (#"getValues" obj)))))
      (format stream "binding ~{~a~^,~}" (loop for key in keys for value in values
	    collect (format nil "~a=~a" key (dlgp-unmangle value)))))))

