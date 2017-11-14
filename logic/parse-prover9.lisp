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
			      it))


;; replace "all","exists", "," , ".", "|" , "&" , "->" , "<->" , "-"  so they read as symbols
;; then read to get a token list

(defun tokenize-prover9-formula (string)
  (let ((*readtable* *prover9-readtable*)
	(*package* (find-package :logic)))
    (loop for (match replace)
	    in '(("\\|" " |\\\\|| ") ; | is or. do this first since past here | is used to bracket symbols for the lisp reader
		 ("((->)|&|(<->)|=|(!=)|-)" " $1 ") ; the major operators: 
		 ("([.,])" " |$1| ")
		 ("\\(" " |(| ")
		 ("\\)" " |)| "))
	  do (setq string (#"replaceAll" string match replace)))
    (values (with-input-from-string (s string) 
	      (loop for bit =  (read s nil :eof)
		    until (eq bit :eof)
		    collect bit))
	    string)))

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
(defvar *prover9-record-initialcap* t)

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
(defun variable (a) (intern (format nil "?~a" (de-camel-case a)) 'cl-user))
(defun predicate-name (a) (intern (de-camel-case a) 'cl-user))

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
	     (list (variable c))
	     (mapcar 'variable c))))

(defun quantifier-all (a b c)
  (declare (ignore a))
  `(:forall ,@(maybe-collapse-nested-quantifiers :forall (list (variable b)) c)))

(defun quantifier-exists (a b c)
  (declare (ignore a))
  `(:exists ,@(maybe-collapse-nested-quantifiers :exists (list (variable b)) c)))

(defun conjunction (a b c) (declare (ignore b))
  (declare (ignore b))
  `(:and ,@(maybe-collapse-and-or :and a) ,@(maybe-collapse-and-or :and c)))

(defun disjunction (a b c) 
  (declare (ignore b))
  `(:or ,@(maybe-collapse-and-or a) ,@(maybe-collapse-and-or c)))

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

(defun expression (a period-at-end)
  (declare (ignore period-at-end))
  a)

(defun equality (a b c)
  (if (eq b '=)
      `(:= ,(variable a) ,(variable c))
      `(:not (:= ,(variable a) ,(variable c)))))

(defun predicate-arguments (a b c)
  (declare (ignore b))
  (append (if (consp a) a (list a)) (list c)))

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
  (cond ((atom expr)
	 (if (member expr quantified)
	     expr
	     (intern (subseq (string expr) 1) 'cl-user)))
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
  (:start-symbol expression-opt)
  (:terminals (id & |\|| -> <-> = |(| |)| |,| - |all| |exists| |.| = !=))
  (:precedence ((:left |all| |exists|) (:left -) (:left  & |\||)  (:right = !=) (:right -> <-> ) (:nonassoc |.| )))
  (expression-opt
   expression
   ())
  (predicate 
   (id |(| args |)| #'predicate)
   )
  (allq 
   (|all| quantified expression #'quantifier-all))
  (existsq
   (|exists| quantified expression #'quantifier-exists))
  (expression
   allq
   existsq
   (expression & expression #'conjunction)
   (expression |\|| expression #'disjunction)
   (expression -> expression #'implication)
   (expression <-> expression #'bi-implication)
   (- expression #'negation)
   (id = id #'equality)
   (id != id #'equality)
   (|(| expression |)| #'parenthesized)
   (expression |.| #'expression)                           
   predicate
   id
   )
  (quantified
   id)
  (args 
   id 
   (args |,| id #'predicate-arguments)) 
  )

;; returns each element in the tokenized list with it's terminal or itself
(defun prover9-list-lexer (list)
       #'(lambda ()
           (let ((value (pop list)))
             (if (null value)
                 (values nil nil)
                 (let ((terminal
                        (cond ((member value '(& ||\| - = <-> -> |(| |)| |,| |all| |exists| |.|)) value)
                              ((symbolp value) 'id)
                              (t (error "Unexpected value ~S" value)))))
                   (values terminal value))))))

(defun parse-prover9 (string)
  (remove-redundant-parentheses (yacc::parse-with-lexer (prover9-list-lexer (tokenize-prover9-formula string)) *prover9-parser*)))

