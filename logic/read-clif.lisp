(in-package :logic)

#| Maybe good enough clif reader.
Doesn't handle functions
unicode sequences not validated
Doesn't reconize numbers
Comments in the middle of forms not yet implemented

(read-clif-form stream eof-errop eof-value)
reads a clif form, ignoring comments.
Returns a parallel sexp but with everthing as strings rather than symbols.
single quoted literals are coded `(:literal <the string>)

(clif-form-to-lsw form)
Takes a form read by read-clif-form and translates to LSW
Tries to be studly about case - all lower case -> upper case, otherwise de-camelcase

1There will have to be mangling of names in order to pass stuff to the reasoner, if
names don't consist of standard characters. TBD
|#

(defun dl-demangle-style (form)
  (flet ((fix-uparrow (string) ;; de-camelcase artifact (when first letter is captitalized)
	   (if (char= (code-char 8593) (char string 0))
	       (subseq string 1)
	       string)))
    (if (and (= (length form) 2) ;; <letter><number> is his style for variables. If they aren't bound make them variables anyways
	     (alpha-char-p (char form 0))
	     (or (digit-char-p (char form 1)) (char-equal #\n (char form 1))))
	(intern (concatenate 'string "?" (string-upcase form)))
	(if (#"matches" form "[A-Za-z0-9]*") ;; if it is camelcase, decode
	    (intern (fix-uparrow (de-camel-case form)))
	    (if (#"matches" form "_[A-Za-z0-9]*_") ;; if it is camelcase with underscore prefix and suffix then remove underscore
		(intern (fix-uparrow (de-camel-case (#"replaceAll" form "_" "")))) ;; specific to David's style
		(intern form))))))

(defun no-mangling-style (form)
  (intern form))


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

(defun de-comment-formula (lsw-form)
  (cond ((atom lsw-form)
	 lsw-form)
	((consp lsw-form)
	 (if (eq (car lsw-form) :comment)
	     (de-comment-formula (third lsw-form))
	     (mapcar 'de-comment-formula lsw-form)
	     ))))

(defun maybe-comment-inner (form)
  (if (and (consp form)
	   (equalp (car form) "cl:comment")
	   (null (assert (eq (car (second form)) :literal) () "Literal needs to follow cl:comment, instead got ~a" (second form)))
	   (null (assert (= (length form) 3) () "Comment has to have 3 elements, got: ~s" form)))
      (let ((inner (third form)))
	(if (or (atom inner) (and (consp inner) (not (equalp (car inner) "cl:comment"))))
	  inner
	  (maybe-comment-inner inner)))
      form))

(defun maybe-rewrap-comment (initial rewritten-inner)
  (if (and (consp initial)
	   (equalp (car initial) "cl:comment"))
      `(:comment ,(second (second initial)) ,(maybe-rewrap-comment (third initial) rewritten-inner))
      rewritten-inner))

(defun clif-form-to-lsw (form &key (stylefn 'no-mangling-style) (keep-comments nil))
  (labels ((walk (form bindings)
;	     (print-db form bindings)
	     (let ((result 
		     (if (atom form)
			 (or (second (assoc form bindings :test 'equalp))
			     (funcall stylefn form)
			     )
			 (if (and (consp form) (eq (car form) :literal))
			     (second form)
					;			       (error "Can't handle common logic 'wild-west' expression as predicate : ~a" head))
			     (if (and (consp form) 
				      (equalp (car form) "cl:comment"))
				 `(:comment ,(walk (second form) bindings) ,(walk (third form) bindings))
				 (let ((name (car form)))
				   (cond ((member name '("FORALL" "EXISTS") :test 'equalp)
					  (let ((vars (mapcar 'maybe-comment-inner (second form))))
					    (assert (and (consp vars)
							 (every 'stringp vars))
						    () "Quantifier ~a expects a list of variables but got ~a" name (second form))
					    (let ((renamed (loop for maybe-comment-sym in (Second form)
								 for sym = (maybe-comment-inner maybe-comment-sym)
								 if (char= (char sym 0) #\?)
								   collect (list sym (intern (string-upcase (string sym))))
								 else collect (list sym 
										    (maybe-rewrap-comment maybe-comment-sym 
												      (intern (concatenate 'string "?" (string-upcase sym))))))))
					      `(:forall ,(mapcar 'second renamed)
						 ,@(mapcar (lambda(e) (walk e (append renamed bindings))) (cddr form))))))
					 ((member (maybe-comment-inner name) '("AND" "OR" "IFF" "IF" "NOT" "=") :test 'equalp)
					  (let ((maybe-wrapped-name (maybe-comment-inner name)))
					    (maybe-rewrap-comment maybe-wrapped-name 
							      `(,(maybe-rewrap-comment name  (if (equalp maybe-wrapped-name "IF") :implies (intern (string-upcase maybe-wrapped-name)  'keyword)))
								,@(mapcar (lambda(e) (walk e bindings)) (cdr form))))))
					 (t `( ,@(mapcar (lambda(e) (walk e bindings)) form))))))))))
	       result
	       ; (print-db result)
	       )))
    (if keep-comments
	(walk form nil)
	(de-comment-formula (walk form nil)))))

(defparameter *clif-punctuation* '(#\~ #\! #\# #\$ #\% #\^ #\& #\* #\_ #\+ #\{ #\} #\| #\: #\< #\> #\? #\` #\- #\= #\[ #\] #\; #\, #\. #\/))

(defun clif-name-char-p (char)
  (or (alpha-char-p char)
      (digit-char-p char)
      (member char *clif-punctuation* :test 'eql)))


	   
(defun read-clif-name (stream char)
  (let ((result 
	  (if (not (clif-name-char-p (peek-char nil stream)))
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

(defparameter *clif-readtable*
  (let ((table (copy-readtable)))
    (let ((*readtable* table))
      (set-syntax-from-char #\' #\")
      (set-macro-character #\" 'read-clif-string)
      (set-macro-character #\' (lambda(stream char)
				 (declare (ignore char))
				 `(:literal ,(funcall (get-macro-character #\")  stream #\'))))
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

(defun read-clif-string (stream char)
  (coerce (butlast
		(loop for c = (read-char stream t)
		      collect c
		      until (eq char c)
		      when (eql char #\\)
			collect (read-char stream)))
	  'string))

;; An enclosed name is simply a name which may contain characters which would break the lexicalization, such as “Mrs
;; Norah Jones” or “Girl(interrupted)”; like any other name, it may denote anything. The surrounding double-quote marks
;; are not considered part of the discourse name, which is defined to be the character string obtained by removing the
;; enclosing double-quote marks and replacing any internal occurrences of an innernamequote by a single double-quote
;; character. It is recommended to use the enclosed-name syntax when writing URIs, URI references and IRIs as names,
;; since these Web identifiers may contain characters which would otherwise break CLIF lexicalization: in particular,
;; Xpath-compliant URI references will often end in a closing parenthesis.

;; Innernamequote is used to indicate the presence of a double-quote character inside an enclosed name.
;;    nonascii = '\u' , hexa, hexa, hexa, hexa | '\U' , hexa, hexa, hexa, hexa, hexa, hexa ;
;;    innerstringquote = '\'' ;
;;    innernamequote = '\"' ;
;;    innerbackslash = ‘\\’
;;    numeral = digit , { digit } ;


;; Single quotes are delimiters for quoted strings; double quotes for enclosed names.

;; innerstringquote is used to indicate the presence of a single-quote character inside a quoted string. A quoted string
;; can contain any character, including whitespace; however, a single-quote character can occur inside a quoted string
;; only as part of an innerstringquote, i.e. when immediately preceded by a backslash character. The occurrence of a
;; single-quote character in the character stream of a quoted string marks the end of the quoted string lexical token
;; unless it is immediately preceded by a backslash character. Inside enclosed name strings, double quotes are treated
;; exactly similarly. Innernamequote is used to indicate the presence of a double-quote character inside an enclosed
;; name.

;;    nonascii = '\u' , hexa, hexa, hexa, hexa | '\U' , hexa, hexa, hexa, hexa, hexa, hexa ;
;;    innerstringquote = '\'' ;
;;    innernamequote = '\"' ;
;;    innerbackslash = ‘\\’
;;    numeral = digit , { digit } ;

;; A namecharsequence is a lexical token whitespace or parentheses, and may not start with a quote mark although they
;; may contain them. Numerals and sequence markers are not namecharsequences.

;; quotedstring = stringquote, { white | open | close | char | nonascii | namequote | innerstringquote | innerbackslash }, stringquote ;
;; enclosedname = namequote, { white | open | close | char | nonascii | stringquote | innernamequote }, namequote ;
;; reservedelement = '=' | 'and' | 'or' | 'iff' | 'if' | 'forall' | 'exists' | 'not' | 'roleset:' | ‘cl:text‘ | 'cl:imports' | 'cl:excludes' | 'cl:module' | 'cl:comment' ;
;;namecharsequence = ( char , { char | stringquote | namequote | backslash } ) - ( reservedelement | numeral | seqmark ) ;

;; Translation: If in double quotes, accept anything. 

(defun read-clif-form (stream &optional eof-errorp eof-value (non-form-errorp t))
  (when (stringp stream)
    (setq stream (make-string-input-stream stream)))
  (let ((peek1 (peek-char t stream nil :eof)))
    (if (and eof-errorp (eq peek1 eof-value))
	(return-from read-clif-form eof-value)
	(when (eql peek1 #\/)
	  (read-char stream )
	  (cond ((eql (peek-char nil stream eof-errorp eof-value) #\/) ;; // ignore rest of line
		 (progn
		   (read-line stream eof-errorp eof-value)
		   (return-from read-clif-form (read-clif-form stream eof-errorp eof-value))))
		((eql (peek-char nil stream eof-errorp eof-value) #\*)
		 (read-char stream)
		 (loop for char = (read-char stream eof-errorp eof-value ) ;;  /*
		       until (or (eq char eof-value)
				 (and (eql char #\*)
				      (eql (peek-char nil stream nil :eof) #\/)
				      (read-char stream )
				      )))
		 (if (and eof-errorp (equal (peek-char nil stream eof-errorp eof-value) eof-value))
		     (return-from read-clif-form eof-value)
		     (return-from read-clif-form (read-clif-form stream eof-errorp eof-value))))	   
		(t (unread-char #\/ stream)))))
       (let* ((*readtable* *clif-readtable*))
	 (let ((read (read stream eof-errorp eof-value)))
					;	   (push read @)
	   (if (eq read eof-value)
	       eof-value
	       (let ((runaway? (or (tree-find "*/" read :test 'equal)
				   (tree-find "/*" read :test 'equal)
				   (tree-find "//" read :test 'equal))))
		 (if runaway?
		     (error "Looks like there's a missing close parenthesis. Form starts:~%~a..."
			    (subseq (format nil "~a" read) 0 100))
		     (if (not (consp read))
			 (let ((message (format nil "Read something that wasn't a formula: '~a'. ~%Are comments properly delineated?~%Stopped parsing at file position ~a ~%" read
						(file-position stream))))
			   (if non-form-errorp
			       (error message)
			       (values nil message t)))
			 read
			 ))))))))
    
(defun clif-escape (stringin)
  (let ((string (if (consp stringin) (second stringin) stringin)))
    (setq string (#"replaceAll" string "\\\\'" "'"))
    (setq string (#"replaceAll" string "\\\\\"" "\""))
    (setq string (replace-all string "(\\\\[uU][A-Fa-f0-9]{4})"
		 (lambda(e)
		   (string (code-char (read-from-string (concatenate 'string "#x" (subseq e 2))))))
		 1))
    (if (consp stringin)
	`(,(car stringin) ,string)
	string))
  )



(defun test-read-clif ()
  (loop for (string result) in
	'(("..." "...")
	  ("a|b" "a|b")
	  ("'a|b'" (:literal "a|b"))
	  ("(cl:comment 'a|b')" ("cl:comment" (:literal "a|b"))))
	for read = (read-clif-form (make-string-input-stream string))
	do
;	   (print-db read result)
	   (assert (equalp read result) ()
		   "input: ~s, expected: ~s, got:~s" string result read)))


(defun test-comment-syntax ()
  (read-clif-form "(cl:comment 'a|b' (forall (x) (f x)))"))
  

(defvar *check-clif-last-args* nil)

;;(test-read-clif)

(defun check-clif (&optional path &rest args &key (print-clif nil print-clif-supplied) (print-lsw nil print-lsw-supplied))
  (let ((*default-pathname-defaults* (pathname "/local/")))
    (if (and (null path) *check-clif-last-args*)
	(if (or print-clif-supplied print-lsw-supplied)
	    (apply 'check-clif (car *check-clif-last-args*) args)
	    (apply 'check-clif *check-clif-last-args*))
	(if (not (probe-file (merge-pathnames path)))
	    (format t "Didn't find the file ~a" path)
	    (with-open-file (f (merge-pathnames path))
	      (setq *check-clif-last-args* (list* path args))
	      (loop for start = (file-position f)
		    with separator = (concatenate 'string "****************************************************************" (string #\newline))
		    for (clif clif-errorp abort) = (multiple-value-list (ignore-errors (read-clif-form f nil :eof nil)))
		    for (lsw lsw-errorp) = (and (not clif-errorp) (multiple-value-list (ignore-errors (validate-formula-well-formed (clif-form-to-lsw clif  :stylefn 'dl-demangle-style)))))
		    for end = (file-position f)
		    with count = 0
		    until (or abort (eq clif :eof))
		    if (or clif-errorp lsw-errorp)
		      do
			 (write-string separator)
			 (format t "Error in parsing form between file position ~a and ~a: ~%~a~%"
				 start end (or clif-errorp lsw-errorp))
			 (when (or clif-errorp lsw-errorp) (let ((*print-pretty* t)) (format t "~&~a~%" clif)))
			 (when (and lsw-errorp lsw)
			   (let ((*print-pretty* t)) (format t "~&~a~%" lsw)))
			 (write-string separator)
		    else
		      do (incf count)
			 (when (and (not clif-errorp) print-clif)
			   (let ((*print-pretty* t)) (format t "~&~a~%~%" clif)))
;			 (print-db lsw print-lsw lsw-errorp)
			 (when (and (not lsw-errorp) lsw print-lsw)
			   (let ((*print-pretty* t)) (format t "~&~s~%~%" lsw)))
		    finally
		       (when abort
			 (write-string separator)
			 (write-string clif-errorp)
			 (write-string separator))
		       (Format t  "Read ~a CLIF formulas without errors" count)))))))

#|
another check:

If you have an :implies you want it to be the case (generally) that any universally quantified variable used the
consequent is used in a relation with a variable that is in the antecedent.
(:iff a b) is same as (:and (:implies a b) (:implies b a))
e.g

(:forall (?a ?b)
  (:implies (f ?a) (g ?b)))

can be rewritten

(:forall (?a)
  (:implies (f ?a)
      (:forall (?b) (g ?b))))

Usually not what you want.


((let (walk (el)
      (lambda(el)
	(if (and (consp el) (eq (car el) :forall))
	    (push (second el) universally-quantified



(:forall (?a ?b)
  (:implies (f ?a) (:exist (?c) (:and (h ?c) (g ?c ?b))))

later
|#

#|
Explaining the initial intended use of this code, in conjunction with docker lsw:

I have written something that does some checks on a clif file. For now it will be a little clunky to use but I'll make
it easier once we know it works.

I've attached a lisp file that implements the check: read-clif.lisp
The current version is also at https://raw.githubusercontent.com/alanruttenberg/lsw2/owlapiv4/logic/read-clif.lisp

Put it in the directory you are in when you run docker.
I'll assume a clif file (call it f.clif) also is in that folder.

Start docker with: docker run -it --rm -v`pwd`:"/local" lsw2/lisp

The --rm cleans up the used docker container once you are done.
The "-v`pwd`:"/local" arranges for your current directory to be mapped to "/local" in the docker image.

Once lisp is running, do
(in-package :logic)
(load "/local/read-clif.lisp")

Now you can do the following
(check-clif "f.lisp")

It will print how many formulas it successfully reads, as well as any problems it finds. It won't find all problems, but
I can improve that over time.

If you want to have it print the clif that it reads, then instead do
(check-clif "RTA-RTP-RT-Axioms.clif" :print-clif t)

To see instead an LSW translation (I've tried to translate your symbols into idiomatic LSW)
(check-clif "RTA-RTP-RT-Axioms.clif" :print-lsw t)

You can print both with
(check-clif "RTA-RTP-RT-Axioms.clif" :print-lsw t :print-clif t)

If you call just (check-clif) then it will use the arguments you used last time you ran it within a docker run, to make
it easier to iterate as you work on a file.

Reminder: Comments have to be surrounded by "/*" and "*/", or on any line after "//" . With "//" all text until the end of the line is considered a comment.

To test whether error checking works, do some tests by modifying f.clif and then running check-clif on the modified version
Test 1: Remove the initial comment marker "/*"
Test 2: Add an extra close parenthesis
Test 3: Remove a closing parethesis

A note about CLIF vs LSW. In LSW all variables start with a "?". That makes it possible to flag free variables. In
common logic you can't tell the difference between a constant and a free variable. In order to do that (and other)
checks I'm considering something to be a variable in CLIF if it is a letter followed by a number or "n". You might
consider naming your variables to start with a "?" so I don't have to guess. Also, some of the reasoners are picky about
overloading symbols. Use a symbol for only one of relation, variable or constant.
