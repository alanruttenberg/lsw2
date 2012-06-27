(in-package :cl-user)

#|
URIs are interned. 

Syntax for reading uris:

Short form
!foo -> (make-uri-base-relative "foo") makes a uri relative to *default-uri-base*
!ex:foo -> uses *namespace-replacements* to expand uri
!"short form" to take advantage of lisp character quoting for the above(you need to quote ":" as "\:"

Long form
!<http://www.example.com/#foo> reads everything until the ">" You need to do the proper uri escaping. 

To construct a uri:

(make-uri full-name &optional abbreviation) e.g. (make-uri "http://example.com/foo") -> !ex:foo
(make-uri-base-relative "foo" "ex:") -> !ex:foo

To make a uri alias:
(def-uri-alias "material-entity" !<http://purl.obofoundry.org/obo/OBI_0000141>)
Which can then be used as !material-entity

|#

(defstruct (uri  (:print-function print-uri) (:constructor internal-make-uri (full &optional abbreviated blank-p)))
  full
  abbreviated
  blank-p)

(defparameter *interned-uris* (make-hash-table :test 'equal))

(defparameter *local-uri-aliases* nil)

(defparameter *default-uri-base* "http://example.com/")

(defmacro let-uri (bindings &body body)
  `(let ((*local-uri-aliases* (append (list ,@(loop for (name form) in bindings
						 collect `(cons ,name ,form)))  *local-uri-aliases*)))
     ,@body))

(defun make-uri-base-relative (string &optional (base *default-uri-base*))
  (let ((lastchar (char base (1- (length base)))))
    (if (eql lastchar #\:)
	(let ((concat (format nil "~a~a" base string)))
	  (make-uri  (unabbreviate-namespace concat) concat))
	(make-uri (format nil "~a~a" base string)))))

(defun get-uri-alias-or-make-uri-base-relative (string)
  (or (cdr (assoc string *local-uri-aliases* :test 'equal))
      (gethash string *interned-uris*)
      (make-uri-base-relative string)))

(defvar *blankcounter* 0)

(defun make-uri (string &optional abbreviation &rest format-args &aux blank?)
  (cond ((uri-p string)
	 string)
	((and (null string) (null abbreviation))
	 (internal-make-uri "bnode" "bnode")) ;; FIXME. How to return a bnode? GetURI returns null
	(t 
	 (when (equal abbreviation "blank:")
	   (setq abbreviation (format nil "blank:~a" (incf *blankcounter*)))
	   (setq blank? t))
	 (when (and abbreviation (char= (char abbreviation 0) #\_) (char= (char abbreviation 1) #\:))
	   (setq abbreviation (format nil "blank:~a" (subseq abbreviation 2))))
	 (if string
	     (when format-args
	       (setq string (apply 'format nil string format-args)))
	     (setq string (unabbreviate-namespace
			   (if format-args
			       (setq abbreviation (apply 'format nil abbreviation format-args))
			       abbreviation))))
	 (or (gethash string *interned-uris*)
	     (setf (gethash string *interned-uris*)
		   (internal-make-uri string abbreviation blank?))))))

(defun swrl-uri-p (x)
  (and (uri-p x)
       (#"matches" (uri-full x) "^urn:swrl#.*")))

(defun def-uri-alias (string uri)
  (setf (gethash string *interned-uris*) uri))

(defmethod make-load-form ((object uri) &optional environment)
  (declare (ignore environment))
  `(make-uri ,(uri-full object) ,(uri-abbreviated object)))

(defun print-uri (object stream depth)
  (let ((abbreviated (uri-abbreviated object))
	(full (uri-full object)))
    (if abbreviated
      (format stream "!~a" abbreviated)
      (let ((abbreviated (maybe-abbreviate-namespace full)))
	(if (eq abbreviated full)
	    (format stream "!<~a>" full)
	    (progn
	      (setf (uri-abbreviated object) abbreviated) 
	      (format stream "!~a" abbreviated)))))))

(defun decache-uri-abbreviated ()
  (maphash (lambda(s u) (setf (uri-abbreviated u) nil)) *interned-uris*))

(defun full-uri-string (uri)
  (uri-full uri))

(defun read-uri (stream char) 
  (declare (ignore char))
  ;; kludge to get around issue with function names starting with "!" in slime/swank-match.lisp
  (if (or (and (boundp '*compile-file-pathname*) 
	       (search "swank-match" (when *compile-file-pathname* (namestring *compile-file-pathname*)) :test #'char-equal))
	  (and (boundp '*load-pathname*) 
	       (search "swank-match" (when *load-pathname* (namestring *load-pathname*)) :test #'char-equal)))
      (progn 
	(unread-char #\! stream)
	(let ((*readtable* *saved-readtable*))
	  (read stream)))
      (progn
	(when (eql char #\<) (unread-char #\< stream))
	(let ((peek (peek-char nil stream nil :eof)))
	  (when (eql peek #\?) (return-from read-uri `(make-swrl-variable-uri ',(read stream))))
	  (let ((string
		 (cond ((eql peek #\")
			(read stream))
		       ((eql peek #\')
			(loop for char = (peek-char nil stream nil :eof)
			   while (not (or (eq char :eof)
					  (eql char #\@)))
			   for the-char-first-part = (read-char stream)
			   collect the-char-first-part into first-chars
			   finally (progn
				     (if (not (eql (read-char stream) #\@))
					 (error "Unterminated URI by name missing source")
					 (return
					   (loop for char = (peek-char nil stream nil :eof)
					      while (not (or (eq char :eof)
							     (system::whitespacep char)
							     (char= char #\))))
					      collect (read-char stream) into second-chars
					      finally (return  (coerce (append first-chars (list #\@) second-chars) 'string))))))))
		       ((eql peek #\<)
			(read-char stream)
			(return-from read-uri
			  `(make-uri 
			    ,(coerce 
			      (loop for char = (peek-char nil stream nil :eof)
				 while (not (or (eq char :eof)
						(eql char #\>)))
				 collect (read-char stream)
				 finally (when (not (eql (read-char stream) #\>)) 
					   (error "Unterminated URI: Missing >"))) 
			      'string))))
		       (t
			(coerce 
			 (loop for char = (peek-char nil stream nil :eof)
			    while (not (or (eq char :eof)
					   (system::whitespacep char)
					   (char= char #\))))
			    collect (read-char stream))
			 'string))
		       )))
	    (if (find #\' string)
		(let ((matched (car (all-matches string "'(.*?)'@([A-Za-z0-9-]+){0,1}(\\((.*)\\)){0,1}" 1 2 4))))
		  (if matched
		      (destructuring-bind (label source original) matched
			`(make-uri-from-label-source 
			  ,(intern (string-upcase source) 'keyword) ,label ,original))
		      (error "Malformed label uri string: ~a" string))) 
		(if (find #\: string)
		    `(make-uri nil ,string)
		    `(get-uri-alias-or-make-uri-base-relative ,string))))))))

(defun make-swrl-variable-uri (var)
  (if (and (consp var) (eq (car var ) 'quote)) (setq var (second var)))
  (make-uri (format nil "urn:swrl#~a" (subseq (string-downcase (string var)) 1))))

;; *saved-readtable* used in read-uri when we run into a !
(eval-when (:load-toplevel :execute :compile-toplevel)  
  (defparameter *saved-readtable* (copy-readtable nil)))
(set-macro-character #\! 'read-uri t)


(defun get-uri-alias (string)
  (gethash string *interned-uris*))

(defun eval-uri-reader-macro (form)
  "The reader macro doesn't evaluate anything at reader time. If you do '(!ex:foo) you get
 ((MAKE-URI NIL \"ex:foo\")). This function takes a form and evaluates just those make-uri
calls, so you can get what you probably wanted: (type-of (car (eval-uri-reader-macro '(!ex:foo)))) ->uri"
  (cond ((and (consp form) (member (car form) '(make-uri make-uri-base-relative get-uri-alias-or-make-uri-base-relative make-uri-from-label-source make-swrl-variable-uri)))
	 (apply (car form) (cdr form)))
	((consp form) (mapcar #'eval-uri-reader-macro form))
	(t form)))

(defparameter *uri-workaround-character-fixes* 
  (load-time-value
   (loop for fixme in '(#\& #\  #\( #\)  )
      collect (list (#"compile" '|java.util.regex.Pattern| (format nil "[~c]" fixme))
		    (format nil "%~2x" (char-code fixme)) fixme))))

(defun clean-uri (site path &optional (protocol "http" ) (fragment "") (query nil))
  (let ((null (load-time-value (make-immediate-object nil :ref))))
    (clean-string
     (#0"toString" (new 'java.net.uri protocol site path (or query null) (or fragment null))))))

(defun clean-string (string)
  (loop for (pattern replacement) in *uri-workaround-character-fixes*
       for new = 
       (#0"replaceAll" (#0"matcher" pattern string) replacement)
       then
       (#0"replaceAll" (#0"matcher" pattern new) replacement)
       finally (return  (#"toString" new)) ))

(eval-when (:load-toplevel :execute)
  (when (boundp '*print-db-hooks*)
    (pushnew 'eval-uri-reader-macro *print-db-hooks*)))

(defvar *default-uri-label-source* nil)

(defmethod make-uri-from-label-source ((source (eql :nil)) name actual) 
  (if *default-uri-label-source* 
      (make-uri-from-label-source *default-uri-label-source*   name actual)
      (error "No label source explicit and no default for ~a" name)))

;; needs jena in classpath
(defun invalid-uri? (uri)
  (let ((violations (#"violations" (#"create" (#"semanticWebImplementation" 'IRIFactory) (uri-full uri)) nil)))
    (loop while (#"hasNext" violations)
       collect (#"getLongMessage" (#"next" violations)))))
