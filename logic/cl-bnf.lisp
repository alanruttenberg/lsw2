(in-package :clif)
(defpackage :clif (:use cl))
(import '(yacc::tstate yacc::transform-sequence))

(defparameter *clif-punctuation* '(#\~ #\! #\# #\$ #\% #\^ #\& #\* #\_ #\+ #\{ #\} #\| #\: #\< #\> #\? #\` #\- #\= #\[ #\] #\; #\, #\. #\/))

(defun starting-a-comment (stream)
  (when (eql (peek-char t stream nil :eof) #\/)
    (read-char stream)
    (if (or (eql (peek-char t stream nil :eof) #\/)
	    (eql (peek-char t stream nil :eof) #\*))
	t
	(progn
	  (unread-char #\/ stream)
	  nil))))

(defun skip-comment (stream)
  (cond ((eql (peek-char t stream nil :eof) #\/)
	 (read-line stream))
	((eql (peek-char t stream nil :eof) #\*)
	 (read-char stream)
	 (loop for char = (read-char stream) ;;  /*
	       until (or (and (eql char #\*)
			      (eql (peek-char nil stream nil :eof) #\/)
			      (read-char stream )
			      ))))))

(defun clif-escape (stringin)
  (let ((string (if (consp stringin) (second stringin) stringin)))
    (setq string (#"replaceAll" string "\\\\'" "'"))
    (setq string (#"replaceAll" string "\\\\\"" "\""))
    (setq string (cl-user::replace-all string "(\\\\[uU][A-Fa-f0-9]{4})"
		 (lambda(e)
		   (string (code-char (read-from-string (concatenate 'string "#x" (subseq e 2))))))
		 1))
    (if (consp stringin)
	`(,(car stringin) ,string)
	string))
  )
(defun clif-name-char-p (char)
  (and (characterp char)
       (or (alpha-char-p char)
	   (digit-char-p char)
	   (member char *clif-punctuation* :test 'eql))))

(defun read-clif-string (stream char)
  (clif-escape (coerce (butlast
		(loop for c = (read-char stream t)
		      collect c
		      until (eq char c)
		      when (eql char #\\)
			collect (read-char stream)))
	  'string)))

(defun read-clif-name (stream char)
  (let ((result 
	  (if (not (clif-name-char-p (peek-char nil stream nil :eof)))
	      (string char)
	      (if (starting-a-comment stream)
		  (progn (skip-comment stream)
			 (string char))
		  (coerce (cons char (if (clif-name-char-p (peek-char nil stream nil :eof))
					 (loop for next = (peek-char nil stream nil :eof)
					       until (or (eq next :eof)
							 (not (clif-name-char-p next)))
					       collect (read-char stream))
					 nil)) 'string)))))
    (if (or (#"matches" result "//.*")
	    (and (equal result "/")
		 (peek-char nil stream t )))
	(progn
	  (read-line stream)
	  (read stream))     
	(clif-escape result))))

(defun read-as-symbol (usual &optional peek with-peek)
  (lambda(stream char)
    (declare (ignore char))
    (if (and peek (eql (peek-char nil stream) peek))
	(progn (read-char stream) with-peek)
	usual)))

(defparameter *clif-readtable*
  (let ((table (copy-readtable)))
    (let ((*readtable* table))
      (set-syntax-from-char #\' #\")
      (set-syntax-from-char #\. #\a)
      (loop for char across "()="
	    do (set-macro-character char (read-as-symbol (intern char)) ))
      (set-macro-character #\' (lambda(stream char)
				 (let ((read (read-clif-string stream char)))
				   `(quotedstring ,read))))
      (set-macro-character #\" (lambda(stream char)
				 (let ((read (read-clif-string stream char)))
				   `(enclosedname ,read))))
      (loop for char in (append *clif-punctuation* 
      				  (loop for char from (char-code #\a) to (char-code #\z)
      					collect (code-char char))
      				  (loop for char from (char-code #\A) to (char-code #\Z)
      					collect (code-char char))
      				  (loop for char from (char-code #\0) to (char-code #\9)
      					collect (code-char char)))
      	      do (set-syntax-from-char char #\a)
      		 (set-macro-character char 'read-clif-name))
      (set-syntax-from-char #\\ #\a))
    table))

;; https://github.com/gruninger/colore/blob/79b28662d735ffab5c96492c8cdb016c1068770a/ontologies/psl_soo/soo.clif
(defparameter *cl-test-1* "(cl:text 

(cl-imports http://colore.oor.net/psl_actocc/actocc.clif)

(cl-comment 'x.th:def-same_grove (added 5-Jun-2009)')
(forall (o1 o2)
	(iff	(same_grove o1 o2)
			(exists (a s1 s2)
				(and	(occurrence_of o1 a)
						(occurrence_of o2 a)
						(root_occ s1 o1)
						(root_occ s2 o2)
						(or	(and	(initial s1)
									(initial s2))
							(exists (s4 a1 a2)
								(and	(= s1 (successor a1 s4))
										(= s2 (successor a2 s4)))))))))
)")

(defun read-clif-forms (stream)
  (let ((*readtable* *clif-readtable*))
      (loop for el = (read stream nil :eof)
	    until (eq el :eof)
	    collect el)))

(defun test (&key (string *cl-test-1*)  (start-symbol 'cltext) (trace *trace-lexer*) (rebuild-parser t) (pprint t) (case-insensitive t)
	       (permissive nil))
  (let ((parser (if rebuild-parser (setq @ (make-cl-parser :start-symbol start-symbol :permissive permissive)) clif-parser)))
    (let ((*trace-lexer* trace))
      (if (and (null string) (null start-symbol))
	  (progn (princ *cl-test-1*) (values))
	  (progn (when (null string)
		   (setq string *cl-test-1*))
		 (let* ((read (with-input-from-string (s (or string *cl-test-1*)) (read-clif-forms s)))
			(lexer (clif-lexer read :case-insensitive case-insensitive :permissive t)))
		   (let ((parsed (yacc::parse-with-lexer lexer parser)))
		     (if pprint (pprint parsed))
		     parsed
		     )))))))


;nonterminals
;(operator atomsent equation atom predicate numeral namecharsequence enclosedname interpretedname termseq boolsent quantsent boundlist  seqmark term commentsent name sentence quotedstring phrase open interpretablename close cltext module namedtext text)

(defvar *trace-lexer* nil)

(defun clif-lexer (list &key case-insensitive permissive)
  (prog1 (defun clif-lexer-1 ()
    (let ((value (pop  list)))
      (flet ((i (x)
	       (intern (if case-insensitive (string-upcase x) x))))
	(multiple-value-bind (cat v)
	    (cond 
	      ((member value '("=" "and" "or" "if" "iff" "not" "forall" "exists" "cl:imports" "cl:comment" "cl:text"
			       "cl:ttl" "cl:indiscourse" "cl:outdiscourse" "cl:prefix" "cl:restrict")
		       :test 'equalp)
	       (values (intern value) (intern (string-upcase (substitute #\- #\: value)) 'keyword)))
	      ((and permissive (member value '("cl-imports" "cl-comment" "cl-text"
					       "cl-ttl" "cl-indiscourse" "cl-outdiscourse" "cl-prefix" "cl-restrict")
				       :test 'equalp))
	       (values (intern (substitute #\: #\- value)) (intern (string-upcase value) 'keyword)))
	      ((member value '("cl-module" "cl-excludes" "cl-imports" "cl-text" ) :test 'equalp)
	       (values (intern (substitute #\: #\- value)) value))
	      ((eq value '|)|)
	       (values 'close value))
	      ((eq value '|(|)
	       (values 'open value))
	      ((and (stringp value) (eql 0 (search "..." value)))
	       (values 'seqmark `(:seqmark ,@(if (> (length value) 3) 
						 (list (i (subseq value 3)))))))
	      ((and (consp value) (eq (car value) 'quotedstring))
	       (values (car value) (second value)))
	      ((and (consp value) (eq (car value) 'enclosedname))
	       (values (car value) (i (second value))))
	      ((and (stringp value)
		    (plusp (length value))
		    (or (char= (char value 0) #\-)
			(digit-char-p (char value 0)))
		    (every 'digit-char-p (subseq value 1)))
	       (values 'numeral (parse-integer value)))
	      ((null value) nil)
	      (t (values 'namecharsequence (i value))))
	  (when *trace-lexer* (format t "~s ~s~%" cat v))
	  (values cat v)
	  )))) ))

(defun make-cl-parser(&key (start-symbol 'cltext) (permissive nil))
  (eval (eval
	 `(yacc::define-parser clif-parser
	    (:terminals (|=| |roleset:| |and| |or| |if| |iff| |not| |forall| |exists|
			     |cl:module| |cl:excludes| |cl:imports| |cl:comment| |cl:text| |cl:ttl| |cl:prefix|
			     open close seqmark quotedstring namecharsequence enclosedname numeral))
	    (:start-symbol ,start-symbol)

	    ;; termseq = { term  | seqmark } ;
	    (termseq
	     ((:* (:or term seqmark)) ))

	    ;; cseqmark = seqmark | ( open, 'cl:comment', quotedstring , seqmark , close ) ;
	    (cseqmark
	     seqmark
	     ( open |cl:comment| quotedstring seqmark close 'remove-parens))

	    ;; interpretedname =  numeral | quotedstring | ( open, 'cl:comment', quotedstring , (numeral | quotedstring) , close ) ;
	    (interpretedname
	     numeral
	     quotedstring
	     ;; unnecessary - handled by comment in production term 
	     ;; (open |cl:comment| quotedstring (:or numeral quotedstring) close )
	     )
	
	    ;; interpretablename =  namecharsequence | enclosedname | (open, 'cl:comment', quotedstring , interpretablename , close );
	    (interpretablename
	     namecharsequence
	     enclosedname
	     
	     ;; unnecessary - handled by comment in production term 
	     ;;(open |cl:comment| quotedstring interpretablename close)
	     )

	    ;; name = interpretedname | interpretablename ;
	    (name
	     interpretedname
	     interpretablename
	     ;;	     (open |cl:comment| quotedstring name close 'remove-parens)
	     )

	    ;; equation = open, '=', term, term, close ;
	    (equation
	     (open |=| term term close 'remove-parens))
  
  
	    ;; term = name | ( open, operator, termseq, close ) | ( open, 'cl:comment', quotedstring , term, close ) ;
	    (term
	     name
	     (open operator termseq close (lambda(o op seq c) (declare (ignore o c)) `(,op ,@seq)))
	     (open |cl:comment| quotedstring term close 'remove-parens)
	     )

	    ;; operator = term  ;
	    (operator
	     term)

	    ;; sentence = atomsent | boolsent | quantsent | commentsent ;
	    (sentence
	     atomsent
	     boolsent
	     quantsent
	     commentsent)
 
	    ;; atomsent = equation | atom ;
	    (atomsent
	     equation
	     simple_sentence)

	    ;; simple_sentence = ( open, predicate , termseq, close ) ;
	    (simple_sentence 
	     (open predicate termseq close (lambda(o predicate termseq c)
					     (declare (ignore c o))
					     (list* predicate termseq))))
 
	    ;; predicate =  term ;
	    (predicate
	     term)

	    ;; boolsent = ( open, ('and' | 'or') , { sentence }, close ) | ( open, ('if' | 'iff') , sentence , sentence, close ) | ( open, 'not' , sentence, close );
	    (boolsent
	     (open (:or |and|  |or|)  (:* sentence) close
		   (lambda(o op s c)
		     (declare (ignore c o))
		     `(,op ,@s)))
	     (open (:or |if| |iff|)  sentence  sentence close 'remove-parens)
	     (open |not|  sentence close 'remove-parens))

	    ;; quantsent = open, (:or |forall| | |exists|) , boundlist, sentence, close ;

	    (quantsent
	     (open (:or |forall| |exists|)  boundlist sentence close
		   (lambda(o q b s c)
		     (declare (ignore c o))
		      `(,q ,@b ,s))))
 
	    ;; boundlist = open, bvar, { bvar } , close ;
	    (boundlist
	     (open (:+ bvar)  close  'remove-parens))

	    ;; bvar = interpretablename |  cseqmark | ( open, (interpretablename | cseqmark), term, close ) ;
	    (bvar
	     interpretablename
	     cseqmark
	     (open (:or interpretablename cseqmark) term close 'remove-parens))

	    ;; commentsent = open, 'cl:comment', quotedstring , sentence , close ;
	    (commentsent
	     (open |cl:comment| quotedstring sentence close 'remove-parens))

	    ;;  titling = open, 'cl:ttl', interpretablename , text , close ;
	    (titling
	     (open |cl:ttl| interpretablename text close 'remove-parens)
	     ,@(if permissive '((open |cl:ttl| (:or quotedstring interpretablename) sentence close 'remove-parens)))
	     )

	    ;; indiscourse = open, 'cl:indiscourse', term, {term} , close ;
	    (indiscourse
	     (open |cl:indiscourse| (:+ term) close 'remove-parens))
  
	    ;; outdiscourse = open, 'cl:outdiscourse', term, {term} , close ;

	    (outdiscourse
	     (open |cl:outdiscourse| (:+ term) close 'remove-parens))
  
	    ;; discoursestatement = indiscourse | outdiscourse ;
  
	    (discoursestatement
	     indiscourse
	     outdiscourse)
  
	    ;; statement = titling | discoursestatement | ( open, 'cl:comment', quotedstring , statement , close ) ;
	    (statement
	     titling
	     discoursestatement
	     ( open |cl:comment| quotedstring  statement  close 'remove-parens))

	    ;; importation = open, 'cl:imports', interpretablename , close ;
	    (importation
	     (open |cl:imports| interpretablename close 'remove-parens))

	    ;; domainrestriction =  open, 'cl:restrict , term , text, close;
	    (domainrestriction
	     (open |cl:restrict| term text close 'remove-parens))
  
	    ;; textconstruction = open, 'cl:text', { sentence | statement | text },  close ;
	    (textconstruction
	     (open |cl:text| (:* (:or sentence statement text)) close
		   (lambda(o text ss c) (declare (ignore c o)) `(,text ,@ss)))
	     )
  
	    ;; prefixdeclaration = open, 'cl:prefix', (quotedstring - 'cl'), interpretablename, close ;
	    (prefixdeclaration
	     (open |cl:prefix| quotedstring interpretablename close 'remove-parens))
  
	    ;; commenttext = open, 'cl:comment', quotedstring, {prefixdeclaration}, cltext, close ;
	    (commenttext
	     (open |cl:comment| quotedstring (:* prefixdeclaration) cltext close
		   (lambda (o com q pxs text c)
		     (declare (ignore o c))
		     `(,com ,q ,@(if pxs pxs) ,@text))))
   
	    ;; text  = textconstruction | domainrestriction | importation |  commenttext ;
	    (text
	     textconstruction
	     domainrestriction
	     importation
	     commenttext)
  
	    ;; cltext = {text} ;
	    (cltext
	     ((:+ text) 'identity)
	     )		;&&&
	    ))))

(defun remove-parens (&rest args)
  (cdr (butlast args)))
  ;; module = open, 'cl:module' , interpretablename , [open, 'cl:excludes' , {name} , close ] , cltext, close;
;  (module
;   (open |cl:module|  interpretablename  (:? open |cl:excludes|  (:* name)  close )  cltext close))


(defun run-tests ()
  (let ((tests '(("(cl:comment 'ten' 10)" (:cl-comment "ten" 10) term)
		 ("(cl:comment 'ten' 'twenty')" (:cl-comment "ten" "twenty") term)
		 ("(cl:comment 'ten' twenty)" (:cl-comment "ten" twenty) term)
		 ("(forall (x) (f x))" (:forall (x) (f x)) quantsent)
		 ("(forall ((x class)) (f x))" (:forall ((x class)) (f x)) quantsent)
		 ("(or)" (:or) boolsent)
		 ("(and)" (:and) boolsent)
		 ("(and (f x))" (:and (f x)) boolsent)
		 ("(if (f x) (g x))" (:if (f x) (g x)) boolsent)
		 ("(iff (f x) (g x))" (:iff (f x) (g x)) boolsent)
		 ("(iff (exists (x) (f x)) (forall (x) (g x)))" (:iff (:exists (x) (f x)) (:forall (x) (g x))) boolsent)
		 ("(cl:comment 'a comment' (f x))" (:cl-comment "a comment" (f x)) term)
		 ("(cl:comment 'a comment' x)"  (:cl-comment "a comment" x) term)
		 ("(cl:comment 'a comment' 10)"  (:cl-comment "a comment" 10) term)
		 ("(cl:comment 'a comment' '10')"  (:cl-comment "a comment" "10") term)
		 ("(cl:comment 'a comment' \"10\")"  (:cl-comment "a comment" |10|) term)
		 ("(cl:comment 'ten' (cl:text (forall (x) (f x))))" (:cl-comment "ten" nil (:cl-text (:forall (x) (f x)))) commenttext)
		 ("(cl:comment 'ten' (cl:prefix 'dc' http://dublin) (cl:text (forall (x) (f x))))"
		  (:cl-comment "ten" ((:cl-prefix "dc" |HTTP://DUBLIN|)) (:cl-text (:forall (x) (f x)))) commenttext)
		 ("(cl:comment 'ten' (forall (x) (f x)))" (:cl-comment "ten" (:forall (x) (f x))) sentence)
		 ("(cl:text (cl:comment 'ten' (forall (x) (f x))))" ((:cl-text (:cl-comment "ten" (:forall (x) (f x))))) cltext)
		 ("(cl:text (cl:comment 'ten' (forall (x) (f x))) (forall (x) (f x)))"
		  ((:cl-text (:cl-comment "ten" (:forall (x) (f x))) (:forall (x) (f x)))) cltext)
		 )))
    (loop with last-start = nil
	  for (in out start) in tests
	  for (parsed errorp) = (multiple-value-list
				 (ignore-errors
				  (test :string in :start-symbol start :rebuild-parser (not (eq start last-start)) :pprint nil)))
	  do (setq last-start start)
	     (cond
	       ((equalp parsed out) (format t "~a -> ~s~%" in out))
	       (errorp (format t "~a failed to parse with error ~a~%" in errorp))
	       (t (format t "FAIL: ~a -> ~s instead of ~s~%" in parsed out))))))
	

#|
Errata

reservedtokens has 'cl:comment', but production uses 'cl-comment'

 6.1.1.3 A text construction contains a set, list or bag of sentences, statements and/or texts. A Common Logic text may be a sequence, a set or a bag of sentences, statements and/or texts; dialects may specify which is intended or leave this undefined. Re-orderings and repetitions of arguments of a text construction are semantically irrelevant. However, applications which transmit or re-publish Common Logic text shall preserve the structure of text constructions, since other applications are allowed to utilize the structure for other purposes, such as indexing. If a dialect imposes conditions on text constructions, then these conditions shall be preserved by conforming applications. A text construction may be empty.

But the CLIF production says.
 textconstruction = open, 'cl:text', { sentence | statement | text },  close ;


|#
