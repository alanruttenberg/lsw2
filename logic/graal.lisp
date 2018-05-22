(in-package :logic)

(defclass graal-kb ()
  ((kb :accessor kb)
   (dlgp-parser :accessor dlgp-parser :initform (new 'dlgpparser ""))
   (facts :accessor facts :initform nil :initarg facts)
   (rules :accessor rules :initform nil :initarg rules)
   (graal-rules :accessor graal-rules :initform (new 'linkedlistruleset))))

   
(defmethod initialize-instance  ((kb graal-kb) &key  &allow-other-keys)
  (call-next-method)
  (setf (kb kb) (new 'defaultKnowledgeBase (dlgp-parser kb)))
  (set-java-field (kb kb) "approach" #1"Approach.SATURATE_ONLY"))

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
  (tree-replace (lambda(el) 
		  (cond ((and (symbolp el) (char= (char (string el) 0) #\?))
			 (string-upcase (subseq (string el) 1)))
			((symbolp el) (substitute #\_ #\. (camelCase (#"replaceAll" (string el) "[^0-9,A-Z,a-z,-]" "."))))
			(t el)))
		sexp))

(defmethod add-rules ((kb graal-kb) rules)
  (let ((list (jss::get-java-field (kb kb) "ruleset" t)))
    (loop for rule in rules
	  do (#"add" list (translate-to-dlgp rule)))
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
					

(defun translate-to-dlgp (rule)
  (destructuring-bind (name (implies (and . body) head)) (dlgp-mangle rule)
    (declare (ignore and implies))
    (format nil "[~a] ~a(~{~a~^,~}) :- ~{~a~^,~}."
	    name
	    (car head) (rest head)
	    (mapcar (lambda(form) (format nil "~a(~{~a~^,~})" (car form) (rest form))) body))))



