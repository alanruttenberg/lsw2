(in-package :logic)

;; A parser for prover9 formulas built on cl-yacc https://www.irif.fr/~jch/software/cl-yacc/
;;
;; So far this is enough to parse John Beverley's prover9 formulas
;; e.g. in https://github.com/johnbeve/Argument-Ontology/tree/master/Axioms
;;
;; all x all y all t(properContinuantPartOfAt(x,y,t) -> (exists z (all w(continuantPartOfAt(w,z,t) <-> (continuantPartOfAt(w,x,t) & continuantPartOfAt(w,y,t)))))).
;; ->
;; (:FORALL (?X)
;;  (:FORALL (?Y)
;;   (:FORALL (?T)
;;    (:IMPLIES (PROPER-CONTINUANT-PART-OF-AT ?X ?Y ?T)
;;     (:EXISTS (?Z)
;;      (:FORALL (?W)
;;       (:IFF (CONTINUANT-PART-OF-AT ?W ?Z ?T)
;;        (:AND (CONTINUANT-PART-OF-AT ?W ?X ?T)
;;         (CONTINUANT-PART-OF-AT ?W ?Y ?T)))))))))
;;
;; Today it parses one formula at a time.
;; TODO: parse a whole file
;; Note: Haven't tested with any prover9 past that. I'm not confident I understand the precedent rules for now.
;; Ignores case, which probably needs to be attended to.

(defvar *prover9-readtable* (let ((it (copy-readtable)))
			      (setf (readtable-case it) :preserve)
			      (let ((*readtable* it))
				(set-macro-character #\! nil))
			      it))

(defun preserve-comments (string)
  (let ((comments nil)
	(count 0))
    (let ((pass1 (replace-all string "(?m)((?s)%BEGIN(.*?)((%END)|\\Z))|(%(?!BEGIN)(.*$))"
			      (lambda(a b)
				(push (or a b) comments)
				(if a
				    (if (#"matches" a ".+?((?m)$)(?s).+$") ;; is it more than one line?
					(format nil ":COMMENTBLOCK ~a" (incf count))
					(format nil ":COMMENT ~a" (incf count)))
				    (format nil ":COMMENT ~a" (incf count)))
				)
				 2 6)))
      (values (#"replaceAll" pass1 "(?m)((^\\s*):COMMENT\\b)" "$2:COMMENTNEWLINE")
	      comments))))

;; replace "all","exists", "," , ".", "|" , "&" , "->" , "<->" , "-"  so they read as symbols
;; then read to get a token list

(defun tokenize-prover9-formula (string)
  (let ((*readtable* *prover9-readtable*)
	(*package* (find-package :logic)))
    (multiple-value-bind (string-no-comments comments) (preserve-comments string)
    (loop for (match replace)
	    in '(("\\|" " |\\\\|| ") ; | is or. do this first since past here | is used to bracket symbols for the lisp reader
		 ("((->)|&|(<->)|=|(!=)|-)" " $1 ") ; the major operators: 
		 ("([.,])" " |$1| ")
		 ("#\\s*((label)|(answer)|(action)|(bsub_hint))\\b\\s*\\((.*?)\\)" ":ATTRIBUTE :$1# \"$6\"")
		 ("\\(" " |(| ")
		 ("\\)" " |)| ")
		 )
	  do (setq string-no-comments (#"replaceAll" string-no-comments match replace)))
      (setq comments (reverse comments))
    (values (with-input-from-string (s string-no-comments) 
	      (loop for bit =  (read s nil :eof)
		    until (eq bit :eof)
		    if (member bit '(:comment :commentnewline :commentblock)) append `(,bit ,(elt comments (1- (read s))))
		      else collect bit))
	    string-no-comments))))

;; A common convention in the BFO world is that first letter is upper case for unary predicates and first letter
;; lowercase for nary predicates. We want to do our best to preserve case when parsing and rendering prover9 without
;; having to look at crazy case changes in lisp code. So we convert camelcase on the prover9 side into dash-separated
;; words on the lisp side. By default when rendering we write back out in camelcase but there is ambiguity as to whether
;; the first the original used initial cap camelcase or initial lower-case camel case.

;; When we parse camel-case, If there's a single word and it's all upper-case then prepend up-arrow in the symbol name.
;; this is the primary use case. The general screw to avoid is using single letter variables, upper case for class,
;; lowercase for instances.

;; Two hacks:
;; 1) If *prover9-uparrow-initialcap* then when a camelcase symbol with first letter is uppercase, also prepend the
;; uparrow.
;; 2) If *prover9-record-initialcap* then set :camelcase-initialcap prop to t on the (dash-version) symbol interned in
;; keyword package


;; Whether to do the upper-case thing for firstcap camelcase
(defvar *prover9-uparrow-initialcap* nil)
;; Whether to save the capitalization on the plist of a symbol 
(defvar *prover9-record-initialcap* nil)

;; Take a lisp symbol and convert to camel case, using the conventions
(defun de-camel-case (string)
  (let ((string (string string)))
    (if (#"matches" string "[A-Z]+[']*")
	(concatenate 'string (string (code-char 8593)) (string-upcase string))
	(if (and *prover9-uparrow-initialcap* (#"matches" string "[A-Z]+[A-Za-z]*[']*"))
	    (concatenate 'string (string (code-char 8593))				 
			 (string-upcase (#"replaceAll" string "([a-z])([A-Z])" "$1-$2")))
	    (let ((it (string-upcase (#"replaceAll" string "([a-z])([A-Z])" "$1-$2"))))
	      (setf (get (intern it :keyword) :camelcase-initialcap) t)
	      it)))))

;; Take a string that might have an uparrow at the start, or initialcap information cached, and 
(defun camelcase-uparrow (string)
  (setq string (string string))
  (if (= (char-code (char string 0)) 8593)  ;; we've got an uparrow
      (if (find #\- string :test 'char=)
	  (camelCase (subseq string 1) t) ;; it's a multi-word, render as camelcase with initialcap 
	  (subseq string 1))  ;; it's a single word - leave as is.
      ;; otherwise, camelcase it but pass the cached property as the switch between initialcap and not.
      (camelCase string (and *prover9-record-initialcap*
			     (get (intern string :keyword) :camelcase-initialcap)))))
					 

;; Variables in LSW logic have ? prepended.
;; symbols in general need to be rewritten from camel case
(defun variable (a) (intern (format nil "?~a" (de-camel-case a)) 'logic))
(defun predicate-name (a) (intern (de-camel-case a) 'logic))

(defun variable-ignore-comment (a comment)
  (variable a))

;; If we parse (:and (:and a b) c) reduce to (:and a b c). Same with :or
(defun maybe-collapse-and-or (head what)
  (if (and (consp what) (eq (car what) head))
      (cdr what)
      (list what)))

;; if we parse (:exists (?a) (:exists (?b) ...) then reduce to (:exists (?a ?b) ...)
(defun maybe-collapse-nested-quantifiers (head vars what)
  (if (and (consp what) (eq (car what) head))
      (list* (append vars (second what)) (cddr what))
      (list vars what)))

;; parser constructors
(defun predicate (a b c d)
  (declare (ignore b d))
  (list* (predicate-name a)
	 (if (atom c)
	     (list c)
	     c)))

(defun quantifier-all (a b c)
  (declare (ignore a))
  `(:forall ,@(maybe-collapse-nested-quantifiers :forall (list b) c)))

(defun quantifier-exists (a b c)
  (declare (ignore a))
  `(:exists ,@(maybe-collapse-nested-quantifiers :exists (list b) c)))

(defun conjunction (a b c) (declare (ignore b))
  (declare (ignore b))
  `(:and ,@(maybe-collapse-and-or :and a) ,@(maybe-collapse-and-or :and c)))

(defun disjunction (a b c) 
  (declare (ignore b))
  `(:or ,@(maybe-collapse-and-or :or a) ,@(maybe-collapse-and-or :or c)))

(defun implication (a b c)
  (declare (ignore b))
  `(:implies ,a ,c))

(defun bi-implication (a b c)
  (declare (ignore b))
  `(:iff ,a ,c))

(defun negation (a b)
  `(:not ,b))

(defun parenthesized (paren1 b paren2)
  (declare (ignore paren1 paren2 ))
  (list b))

(defun formula (a period-at-end)
  (declare (ignore period-at-end))
  (list `(:formula ,a)))

(defun formula-with-attributes (a attributes period-at-end)
  (declare (ignore period-at-end))
  (list `(:formula ,a ,@attributes)))

(defun an-attribute (i which value)
  `((:attribute ,(intern (string-upcase (string which)) :keyword) ,value)))

(defun equality (a b c)
  (if (eq b '=)
      `(:= ,a ,c)
      `(:not (:= ,a ,c))))

(defun predicate-arguments (a b c)
  (declare (ignore b))
  (append (if (consp a) a (list a)) (list c)))

(defun comment (token string)
  (list `(,token ,string)))

(defun expressions (first &optional second)
  (append first second))

(defun ignore-comment (comment string formula)
  (declare (ignore comment))
  formula)

;; Example:
;; f(a) -> ((a & b) -> c)
;; -> (:implies (f a) (:implies ((:and a b))  c))
;; the extra pares not harmful, but ugly and unecessary when already using sexps
;; This function removes them
(defun remove-redundant-parentheses (expr)
  (cond ((atom expr)
	 expr)
	((and (consp expr) (consp (car expr)) (null (cdr expr)))
	 (remove-redundant-parentheses (car expr)))
	(t (mapcar 'remove-redundant-parentheses expr))))

;; Only after we've parsed do we know which are the quantified over symbols.  The strategy is to consider all free
;; symbols variables, but then to go and change back the ones that are not mentioned in a scoped quantification to
;; ordinary symbols.

(defun fix-non-quantified-symbols (expr &optional quantified)
  (cond ((numberp expr) expr)
	((atom expr)
	 (if (member expr quantified)
	     expr
	     (if (char= (char (string expr) 0) #\?)
		 (intern (subseq (string expr) 1) 'logic)
		 expr)))
	((member (car expr)  '(:forall :exists))
	 ;; there's just one formula inside
	 (let ((vars (second expr)))
	   `(,(car expr) ,vars ,(fix-non-quantified-symbols (third expr) (append quantified vars)))))
	((symbolp (car expr))
	 (cons (car expr)
	       (mapcar (lambda(e) (fix-non-quantified-symbols e quantified))
		       (cdr expr))))
	(t (error "fix-non-quantified-symbols has a bug or you do: ~a" expr))))

;; this is the parser description for cl-yacc
;; There's a BNF at https://www.dwheeler.com/formal_methods/prover9-bnf.txt
;; Ours isn't complete yet wrt that.

(yacc::define-parser *prover9-parser*
  (:start-symbol expressions)
  (:terminals (id & |\|| -> <-> = |(| |)| |,| - |all| |exists| |.| = |!=| :commentnewline :comment :commentblock string number :attribute
									  :|label#| :|action#| :|answer#| :|bsub_hint_wt#|))
  (:precedence ((:nonassoc "#") (:left |all| |exists|) (:left -) (:left  & |\||)  (:right = |!=|) (:right -> <-> ) (:nonassoc |.| )))
  (expressions
   (expression #'expressions)
   (expressions expression #'expressions))
  (expression
   (formula |.| #'formula)
   (formula attributes |.| #'formula-with-attributes)
   comment)
  (attributes
   attribute
   (attributes attribute #'list))
  (attribute
   (:attribute attribute-name  string #'an-attribute))
  (attribute-name
   :|label#| :|answer#| :|action#| :|bsub_hint#|)
  (comment
   (:comment string #'comment)
   (:commentnewline string #'comment)
   (:commentblock string #'comment))
  (predicate 
   (id |(| args |)| #'predicate)
   )
  (allq 
   (|all| quantified formula #'quantifier-all))
  (existsq
   (|exists| quantified formula #'quantifier-exists))
  (id-maybe-comment
   (id #'variable)
   (id comment #'variable-ignore-comment)) ;; don't know how to handle comments in the middle of formula for now, so punt
  (formula
   allq
   existsq
   (formula & formula #'conjunction)
   (formula |\|| formula #'disjunction)
   (formula -> formula #'implication)
   (formula <-> formula #'bi-implication)
   (- formula #'negation)
   (id-maybe-comment = id-maybe-comment #'equality)
   (id-maybe-comment |!=| id-maybe-comment #'equality)
   (|(| formula |)| #'parenthesized)
   predicate
   id-maybe-comment
   )
  (quantified
   id-maybe-comment)
  (args 
   id-maybe-comment 
   (args |,| id-maybe-comment #'predicate-arguments)) 
  )

;; returns each element in the tokenized list with it's terminal or itself
(defun prover9-list-lexer (list)
       #'(lambda ()
           (let ((value (pop list)))
             (if (null value)
                 (values nil nil)
                 (let ((terminal
			 (cond ((member value '(& ||\| - = <-> -> |(| |)| |,| |all| |exists| |!=| :attribute :|label#| :|action#| :|answer#| :|bsub_hint_wt#| :commentnewline :comment :commentblock |.|)) value
				)
                              ((symbolp value) 'id)
			      ((stringp value) 'string)
			      ((numberp value) 'number)
                              (t (error "Unexpected value ~S" value)))))
                   (values terminal value))))))

(defun parse-prover9 (string)
  (let ((*package* (find-package :logic)))
    (mapcar 'fix-non-quantified-symbols (mapcar 'remove-redundant-parentheses (yacc::parse-with-lexer (prover9-list-lexer (tokenize-prover9-formula string)) *prover9-parser*)))))

(defun prover9-pprint-formula (string)
  (let ((*print-case* :downcase) 
	(*package* (find-package :logic)))
    (pprint (cadar (parse-prover9 string)))))

(defun prover9-to-lsw (path axiom-prefix)
  (let ((forms (let ((*package* (find-package :logic)))
		 (logic::parse-prover9 (uiop/stream:read-file-string path)))))
    (with-open-file (f (merge-pathnames (make-pathname :type "lisp") path) :if-does-not-exist :create :if-exists :supersede :direction :output)
      (format f "(in-package :logic)~%~%")
      (format f ";; translated from ~a~%~%" (namestring (truename path)))
      (loop with counter = 0
	    with theory = (intern (string axiom-prefix) :keyword)
	    for form in forms
	    for label = (third (find-if (lambda(e) (and (consp e) (eq (car e) :attribute) (eq (second  e) :label#) (third e))) form) )
	    do
	       (cond ((eq (car form) :formula)
		      (let ((*package* (find-package :logic))
			    (*print-case* :downcase))
			(terpri f)
			(pprint
			   `(def-logic-axiom ,(or label (intern (format nil "~a-~a" (string axiom-prefix) (incf counter)) :keyword))
					,(second form)
				      :theory ,theory)
			 f)))
		     ((eq (car form) :commentnewline)
		      (loop for line in (jss::split-at-char (second form) #\newline)
			    do (format f "~&;; ~a~%" line))))))))

