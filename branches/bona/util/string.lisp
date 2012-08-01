(in-package :cl-user)

#+abcl
(defun split-at-regex (string regex)
  (declare (optimize (speed 3) (safety 0)))
  (with-constant-signature ((tostring "toString"))
    (loop for v across (#"split" string regex) collect (tostring v))))

#+abcl
(defun split-at-pattern (string pattern)
  (declare (optimize (speed 3) (safety 0)))
  (with-constant-signature ((tostring "toString"))
    (loop for v across (#"split" pattern string) collect (tostring v))))

(defparameter *regex-chars-needing-escape* 
  (concatenate 'string (string #\tab) ".[]()\\?*+{}^$&|"))

(defun quote-for-regex (string)
  (#"replaceAll" string "(\\.|\\[|\\]|\\(|\\)|\\\\|\\?|\\*|\\+|\\{|\\}|\\^|\\$|\\&|\\|)" "\\\\$1"))

#+abcl
(defun split-at-char (string char)
  (let ((regex (string char)))
    (when (simple-string-search regex *regex-chars-needing-escape*)
      (setq regex (system::concatenate-to-string (list "\\" regex))))
    (with-constant-signature ((tostring "toString"))
      (loop for v across (#"split" string regex) collect (tostring v)))))

#+abcl
(define-compiler-macro split-at-char
    (&whole form  string char)
  "Compile constant regex to pattern at compile time"
  (cond ((characterp char)
	 `(split-at-pattern
	   ,string
	   (load-time-value
	     (let ((regex (string ,char)))
	       (when (simple-string-search regex *regex-chars-needing-escape*)
		 (setq regex (system::concatenate-to-string (list "\\" regex))))
	       (#"compile" 'java.util.regex.Pattern regex)))))
        (t form)))

#+abcl
(defun regex-replace (regex string replacement)
  (#"replaceFirst" string regex replacement))

#+abcl
(defun regex-replace-all (regex string replacement)
  (#"replaceAll" string regex replacement))

#+abcl
(define-compiler-macro regex-replace-all
    (&whole form  regex string replacement)
  "Compile constant regex to pattern at compile time"
  (cond ((stringp regex)
	 `(#"replaceAll" (#0"matcher" 
			  (load-time-value     
			   (#0"compile" 'java.util.regex.Pattern ,regex)) ,string)
			 ,replacement))
        (t form)))

#+abcl
(defun scan (regex string)
  (if (java-object-p regex)
      (#"matches" (#0"matcher" regex string))
      (#"matches" string (format nil ".*~a.*" regex))))

#+abcl 
(defun compile-regex (regex)
  (#"compile" 'java.util.regex.pattern regex))

#+abcl
(defun compile-regex (regex)
  (#"compile" 'java.util.regex.pattern regex))

#+abcl
(defun all-matches (string regex &rest which)
  (declare (optimize (speed 3) (safety 0)))
  (and string
       (let ((matcher (#"matcher" 
		       (if (stringp regex)
			   (#"compile" 'java.util.regex.pattern regex)
			   regex)
		       string)))
	 (with-constant-signature ((mfind "find") (mgroup "group"))
	   (loop while (mfind matcher) 
	      collect (loop for g in which collect
			   (mgroup matcher g)))))))

#+abcl
(defmacro register-groups-bind (vars (regexp string) &body body)
  `(let ,vars
     (let ((matched (all-matches ,string ,regexp ,@(loop for i from 1 repeat (length vars) collect i))))
       (when matched
	 (destructuring-bind ,vars (car matched)
	   ,@body)))))
				

#+abcl
(define-compiler-macro all-matches
    (&whole form  string regex &rest which)
  "Compile constant regex to pattern at compile time"
  (cond ((stringp regex)
	 `(all-matches ,string (load-time-value (#"compile" 'java.util.regex.Pattern ,regex))
		       ,@which))
        (t form)))

#+abcl
(define-compiler-macro scan
    (&whole form regex string)
  "Compile constant regex to pattern at compile time"
  (cond ((stringp regex)
	 `(#"matches" (#0"matcher" (load-time-value (#0"compile" 'java.util.regex.Pattern (concatenate 'string ".*" ,regex ".*"))) ,string)))
	(t form)))

;;;   Copyright (C) 1994-2001 Digitool, Inc
;;;   This file is part of OpenMCL.  
;;;
;;;   OpenMCL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License , known as the LLGPL and distributed with OpenMCL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;   which is distributed with OpenMCL as the file "LGPL".  Where these
;;;   conflict, the preamble takes precedence.  
;;;
;;;   OpenMCL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html

(defmacro print-db (&rest forms &aux)
  `(multiple-value-prog1
       (let ((*print-case* :downcase))
     (progn ,@(print-db-aux forms))
     (terpri *trace-output*))))

(defvar *print-db-hooks* nil)

(defun print-db-aux (forms)
  (loop for hook in *print-db-hooks*
     do (setq forms (funcall hook forms)))
  (when forms
    (cond ((stringp (car forms))
	   `((print ',(car forms) *trace-output*)
	     ,@(print-db-aux (cdr forms))))
	  ((and (consp (car forms)) 
		(consp (caar forms)) 
		(eq (caaar forms) 'lambda)
		(eq (car (third (caar forms))) 'invoke-restargs))
	   `((format *trace-output* "~&~%(#~s ~a) " ,(second (third (caar forms))) ,(second (car forms)))
	     (prin1 ,(car forms) *trace-output* )
	       ,@(print-db-aux (cdr forms))))
	  ((null (cdr forms))
	   `((print ',(car forms) *trace-output*)
	     (let ((values (multiple-value-list ,(car forms))))
	       (prin1 (car values) *trace-output*)
	       (apply #'values values))))
	  (t `((print ',(car forms) *trace-output*)
	       (prin1 ,(car forms) *trace-output*)
	       ,@(print-db-aux (cdr forms)))))))

(defun replace-all (string regex function &rest which)
  (let ((matcher (#"matcher" (if (java-object-p regex) regex (#"compile" 'java.util.regex.pattern regex)) string))
	(sb (new 'stringbuffer)))
    (with-constant-signature ((append "appendReplacement")) ; workaround abcl bug
    (loop while (#"find" matcher) 
	 do
	 (append matcher sb (apply function  
					   (loop for g in which collect
						(#"group" matcher g))))))
    (#"appendTail" matcher sb)
    (#"toString" sb)))

(defun xml-encode-string-with-unicode (string)
  (declare (optimize (speed 3) (safety 0)))
  (let ((matcher (#"matcher" (load-time-value (#"compile" 'java.util.regex.pattern "([\"\\n<&<\\u0080-\\uFFFF])")) string))
	(sb (new 'stringbuffer)))
    (loop while (#"find" matcher) 
	 do
	 (#"appendReplacement" matcher sb (format nil "&#x~4,'0x;" (#"codePointAt" (#0"group" matcher 0) 0))))
    (#"appendTail" matcher sb)
    (#"toString" sb)))

;; (replace-all "one two three infinity" "(\\S+)" 
;; 	     (lambda(s) 
;; 	       (or (second (assoc (intern (string-upcase s) 'keyword)
;; 				  '((:one "1") (:two "2") (:three "3")))) s)) 1)

(defun print-tabbed (stream &rest items)
  (setq items (substitute "" nil items))
  (setq items (loop for item in items if (keywordp item) collect (string-downcase (String item)) else collect item))
  (princ (car items) stream)
  (dolist (item (cdr items))
    (write-char #\tab stream)
    (if (listp item)
	(format stream "~{~a~^|~}" item)
	(princ item stream)))
  (terpri stream))

(defmacro with-matchers (name-string-pairs &body body)
  `(let ,(loop for (name string) in name-string-pairs
	      collect `(,name (#"matcher" (#"compile" 'java.util.regex.pattern ,string) "")))
     ,@body))

(defun join-with-char (list char)
  (with-output-to-string (s)
    (loop for el on list
       do (princ (car el) s)
       (when (cdr el) (princ char s)))))

;;; was wrap-comment, but that was unused, so moved here and generalized.
#+abcl
(defun wrap-string (string &optional (separator (string #\Newline)))
  (let ((words nil) (lines nil) (so-far 0) (max-length 80))
	 (flet ((end-line () (push (format nil "~{~a~^ ~}" (reverse words)) lines)
			  (setq words nil so-far 0)))
	   (loop for word in (map 'list #"toString" (#"split" (#"replaceAll" string "\\\\\"" "\"") "\\s+"))
	      do
	      (cond ((> (+ so-far (length word)) max-length)
		     (end-line)
		     (push word words) (incf so-far (length word)))
		    (t (push word words) (incf so-far (length word)))))
	   (when words (end-line)))
	 (format nil (concatenate 'string "~{~a~^" separator "~}") (reverse lines))))
