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

(defun setting (a period-at-end)
  (declare (ignore period-at-end))
  (list `(:setting ,a)))

(defun list-directive (a period-at-end)
  (if a
      `((:start-list ,a))
      '((:end-list))))

(defun end-list (period-at-end)
  (declare (ignore period-at-end))
  (list `(:end-list)))

(defun formula-kind-1 (fk open a close)
  (declare (ignore period-at-end))
  a)

(defun formula-kind-end (end)
  (declare (ignore period-at-end))
  nil)

(defun end-of-list (a period)
  '((:end-of-list)))

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

(defun value (&rest args)
  (car args))

(defun assign (&rest args)
  `(:assign ,(third args) ,(fifth args) ))

(defun set (&rest args)
  `(:assign ,(third args) t))

(defun unset (&rest args)
  `(:assign ,(third args) nil))

(defun conditional (if open id close dot1 settings end dot2)
  `(:if ,(keywordify (string-upcase id))
	,@settings))

	
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
									  :|label#| :|action#| :|answer#| :|bsub_hint_wt#| |set| |assign| |unset|
									  |if| |end_if| |formulas| |end_of_list|))
  ;; From left to right higher to lower. See prover9 table below
  (:precedence ((:nonassoc -) (:nonassoc = !=)  (:nonassoc |all| |exists|) (:right &) (:right |\||) (:nonassoc -> <-> <-)))
  (expressions
   (expression #'expressions)
   (expressions expression #'expressions))
  (expression
   (setting |.| #'setting)
   (list-kind |.| #'list-directive)
   (conditional )
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
  (setting-name
   (id #'variable))
  (setting-value
   (number #'value)
   (string #'value)
   )
  (list-kind
   set-list
   end-list)
  (set-list
   (|formulas| |(| id  |)|  #'formula-kind-1))
  (end-list
   (|end_of_list|  #'null))
  (conditional
   (|if| |(| id |)| |.| expressions |end_if| |.| #'conditional))
  (setting
   (set-flag )
   (unset-flag)
   (assign))
  (set-flag
   (|set| |(| setting-name |)| #'set))
  (unset-flag
   (|unset| |(| setting-name |)| #'unset))
  (assign
   (|assign| |(| setting-name |,| setting-value |)| #'assign))
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
			 (cond ((member value '(& ||\| - = <-> -> |(| |)| |,| |all| |exists| |!=| 
						:attribute :|label#| :|action#| :|answer#| :|bsub_hint_wt#| 
						:commentnewline :comment :commentblock |.|
						|set| |unset| |assign|
						|if| |end_if| |formulas| |end_of_list|)) value
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

(defun prover9-to-lsw (path axiom-prefix &key (parenthesize-with-prover9 t))
  (let ((forms (let ((*package* (find-package :logic)))
		 (if parenthesize-with-prover9
		     (logic::parse-prover9 (parenthesize-prover9-file path))
		     (logic::parse-prover9 (uiop/stream:read-file-string path))))))
    (with-open-file (f (merge-pathnames (make-pathname :type "lisp") path) :if-does-not-exist :create :if-exists :supersede :direction :output)
      (format f "(in-package :logic)~%~%")
      (format f ";; translated from ~a~%" (namestring (truename path)))
      (when parenthesize-with-prover9
	(format f ";; Sorry, comments were removed because we used prover9 to explicitly parenthesize~%"))
      (terpri f)
      (loop with counter = 0
	    with which-list = nil
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
			      :theory ,theory
			      ,@(if which-list `(:prover9-list ,(keywordify (string-upcase which-list))))
			      )
			   f)))
		     ((or (eq (car form) :setting))
		     (let ((*package* (find-package :logic))
			   (*print-case* :downcase))
		       (terpri f)
		       (format f ";; ~a" (second form))))
		     ((or (eq (car form) :if))
		     (let ((*package* (find-package :logic))
			   (*print-case* :downcase))
		       (terpri f)
		       (format f ";; ~s" form)))
		     ((eq (car form) :end-list)
		      (setq which-list nil))
		     ((eq (car form) :start-list)
		      (setq which-list (second form)))
		     ((eq (car form) :comment)
		      (format f " #|~a|# " (second form)))
		     ((eq (car form) :commentnewline)
		      (loop for line in (jss::split-at-char (second form) #\newline)
			    do (format f "~&;; ~a~%" line)))
		     (t (print form)))))))

(defun parenthesize-prover9-expression (formula)
  "take a single formula and run the prover9 parenthesization"
  (let ((input 
	  (with-output-to-string (s)
	    (loop for setting in 
		  '("set(echo_input)."
		    "set(quiet)."
		    "clear(print_initial_clauses)."
		    "clear(print_given)."
		    "clear(print_gen)."
		    "clear(print_kept)."
		    "clear(print_labeled)."
		    "clear(print_clause_properties)."
		    "clear(print_proofs)."
		    "clear(default_output)."
		    "assign(stats, none)."
		    "clear(clocks)."
		    "clear(bell)."
		    "formulas(usable)."
		    "end_of_list."
		    )
		  do (princ setting s))
	    (format s "formulas(assumptions).~%~a~%end_of_list." formula)
	    )))
    (let ((output (cl-user::run-program-string->string (logic::prover-binary "prover9")  '("-p" "-t 0") input)))
      (caar (jss::all-matches output "(?s).*=+ INPUT =+.*?formulas\\(assumptions\\)\\.\\s*(.*?)\\s*end_of_list.\\s*=+ end of input =+.*" 1)))))

(defun rename-variables (expression)
  (tree-replace (lambda(e) (if (and (symbolp e) (char= (char (string e) 0) #\?))
			       (intern (format nil "?VARIABLE-~a" (subseq (string e) 1)))
			       e))
		expression))

;; we have to rename variables so that we can compare incorrect formulas - ones which leave a variable out of scope

(defun equivalent-formulas (a b)
  (or (equalp a b)
      (multiple-value-bind (errorp res)	
	  (ignore-errors (prover9-prove nil `(:iff ,(rename-variables a) ,(rename-variables b))))
	(or (and (typep errorp 'condition)
		 (values nil (apply 'format nil (slot-value errorp 'sys::format-control) (slot-value errorp 'sys::format-arguments))))
	    (eq errorp :proved))) 
      ))

(defun compare-precedence-to-prover9 (expression)
  (let* ((*print-pretty* t)
	 (*print-case* :downcase)
	 (*print-escape* nil)
	 (*print-readably* nil)
	 (wo (second (first (logic::parse-prover9 expression))))
	 (w (second (first (logic::parse-prover9 (logic::parenthesize-prover9-expression expression))))))
    (princ wo) (terpri)
    (princ w) (terpri)
    (multiple-value-bind (res error)
	(equivalent-formulas w wo)
      (when error
	(print error))
      res)))
	
    (or (equalp wo w)
	(multiple-value-bind (errorp res)	
	    (ignore-errors (prover9-prove nil `(:iff ,(rename-variables w) ,(rename-variables wo))))
	  (print
	   (or (and (typep errorp 'condition)
		    (apply 'format nil (slot-value errorp 'sys::format-control) (slot-value errorp 'sys::format-arguments)))
	       (eq errorp :proved))) )
	)))

;; (compare-precedence-to-prover9 "(all a ImmaterialEntity(a) <-> IndependentContinuant(a) & -(exists b  exists t  (MaterialEntity(b) & continuantPartOfAt(b, a,t)))).")
;; (compare-precedence-to-prover9 "(all a ContinuantFiatBoundary(a)  <-> (exists b  ((ImmaterialEntity(a) &                      
;;                                 ZeroDimensionalSpatialRegion(b) |
;; 			                     OneDimensionalSpatialRegion(b) |
;; 				                 TwoDimensionalSpatialRegion(b)) &
;; 								 (all t  locatedInAt(a,b,t)) &
;; 								 -(exists c exists t SpatialRegion(c) & continuantPartOfAt(c,a,t))))).")
;; (compare-precedence-to-prover9 "(all a (RelationalQuality(a) <->  (exists b  exists c exists t (IndependentContinuant(b) &
;; 							 IndependentContinuant(c) &
;; 						     qualityOfAt(a,b,t) &
;; 							 qualityOfAt(a,c,t))))).")

(defun parenthesize-prover9-file (path)
  "take a file prover9 parenthesization, returning the result as a string"
  (let ((input 
	  (with-output-to-string (s)
	    (loop for setting in 
		  '("set(echo_input)."
		    "set(quiet)."
		    "clear(print_initial_clauses)."
		    "clear(print_given)."
		    "clear(print_gen)."
		    "clear(print_kept)."
		    "clear(print_labeled)."
		    "clear(print_clause_properties)."
		    "clear(print_proofs)."
		    "clear(default_output)."
		    "assign(stats, none)."
		    "clear(clocks)."
		    "clear(bell)."
		    "formulas(usable)."
		    "end_of_list."
		    )
		  do (princ setting s))
	    (with-open-file (f path :direction :input)
	      (loop for line = (read-line f nil :eof)
		    while (not (eq line :eof))
		    do
		       (write-string line s)
		       (terpri s))))))
    (let ((output (cl-user::run-program-string->string (logic::prover-binary "prover9")  '("-p" "-t 0") input)))
      (caar (jss::all-matches output "(?s).*=+ INPUT =+.*?formulas\\(usable\\).\\s*end_of_list.\\s*(.*?)=+ end of input =+.*" 1)))))


#|
Prover 9 precedence.

Type 	Example 	Standard Prefix 	Comment
infix 	a*(b*c) 	*(a,*(b,c)) 	like Prolog's xfx
infix_left 	a*b*c 	*(*(a,b),c) 	like Prolog's yfx
infix_right 	a*b*c 	*(a,*(b,c)) 	like Prolog's xfy
prefix 	--p 	-(-(p)) 	like Prolog's fy
prefix_paren 	-(-p) 	-(-(p)) 	like Prolog's fx
postfix 	a'' 	'('(a)) 	like Prolog's yf
postfix_paren	(a')' 	'('(a)) 	like Prolog's xf
ordinary 	*(a,b) 	*(a,b) 	takes away parsing properties 

Default values:

op(810, infix_right,  "#" ).  % for attaching attributes to clauses
	    
op(800, infix,      "<->" ).  % equivalence in formulas
op(800, infix,       "->" ).  % implication in formulas
op(800, infix,       "<-" ).  % backward implication in formulas
op(790, infix_right,  "|" ).  % disjunction in formulas or clauses
op(780, infix_right,  "&" ).  % conjunction in formulas

% Quantifiers (a special case) have precedence 750.
	    
op(700, infix,        "=" ).  % equal in atomic formulas
op(700, infix,       "!=" ).  % not equal in atomic formulas
op(700, infix,       "==" ).  
op(700, infix,        "<" ).
op(700, infix,       "<=" ).
op(700, infix,        ">" ).
op(700, infix,       ">=" ).
	    
op(500, infix,        "+" ).
op(500, infix,        "*" ).
op(500, infix,        "@" ).
op(500, infix,        "/" ).
op(500, infix,        "\" ).
op(500, infix,        "^" ).
op(500, infix,        "v" ).

op(350, prefix,       "-" ).  % logical negation in formulas or clauses
op(300, postfix,      "'" ).

Higher precedence means closer to the root of the object, and lower precedence means the the symbol binds more closely. For example, assume that the following declarations are in effect.

op(790, infix_right,  "|" ).  % disjunction in formulas or clauses
op(780, infix_right,  "&" ).  % conjunction in formulas


Then the string a & b | c is an abbreviation for (a & b) | c. 
|#
