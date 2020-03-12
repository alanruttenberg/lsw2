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
    (with-constant-signature ((split "split") (tostring "toString"))
      (loop for v across (split string regex) collect (tostring v)))))

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
  `(let ((*print-case* :downcase))
       (multiple-value-prog1
	 (progn ,@(print-db-aux forms))
	 (terpri *trace-output*))))

;; Define :print-db, which can be used anywhere without worrying about package!
(defmacro :print-db (&rest forms)
  `(print-db ,@forms))

(defvar *print-db-hooks* nil)

(defun print-db-aux (forms)
  (and (fboundp 'eval-uri-reader-macro)
       (setq forms (eval-uri-reader-macro forms)))
  (loop for hook in *print-db-hooks*
     do (setq forms (funcall hook forms)))
  (when forms
    (cond ((stringp (car forms))
	   `((pprint ',(car forms) *trace-output*) (princ " "*trace-output*)
	     ,@(print-db-aux (cdr forms))))
	  ((and (consp (car forms)) 
		(consp (caar forms)) 
		(eq (caaar forms) 'lambda)
		(eq (car (third (caar forms))) 'invoke-restargs))
	   `((format *trace-output* "~&~%(#~s ~a) " ,(second (third (caar forms))) ,(second (car forms)))
	     (prin1 ,(car forms) *trace-output* )
	       ,@(print-db-aux (cdr forms))))
	  ((null (cdr forms))
	   `((pprint ',(car forms) *trace-output*)(princ " " *trace-output*)
	     (let ((values (multiple-value-list ,(car forms))))
	       (prin1 (car values) *trace-output*)
	       (apply #'values values))))
	  (t `((pprint ',(car forms) *trace-output*) (princ " " *trace-output*)
	       (prin1 ,(car forms) *trace-output*)
	       ,@(print-db-aux (cdr forms)))))))

(defun replace-all (string regex function &rest which)
  (let ((matcher (#"matcher" (if (java-object-p regex) regex (#"compile" 'java.util.regex.pattern regex)) string))
	(sb (new 'stringbuffer)))
    (with-constant-signature ((append "appendReplacement")) 
      (loop for found = (#"find" matcher)
	    while found 
	    do
	       (#"appendReplacement" matcher sb (apply function  
						       (loop for g in which collect
									    (#"group" matcher g)))))
      )
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

;; adapted from swank::call/truncated-output-to-string
(defun truncating-print (object length  &key (downcase t))
  (let ((buffer (make-string (1+ length)))
	(fill-pointer 0))
    (block buffer-full
      (flet ((write-output (string)
	       (let* ((free (- length fill-pointer))
		      (count (min free (length string))))
		 (replace buffer string :start1 fill-pointer :end2 count)
		 (incf fill-pointer count)
		 (when (> (length string) free)
		   (replace buffer (string (code-char 8230)) :start1 fill-pointer)
		   (return-from buffer-full buffer)))))
	(let ((stream (ext:make-slime-output-stream #'write-output)))
	  (let ((*print-case* (if downcase :downcase *print-case*)))
	    (princ object stream)
	    (finish-output stream))
	  (subseq buffer 0 fill-pointer))))))

(defun number-aware-string-lessp (s1 s2 &key (start1 0) (start2 0)
                               (end1 (length s1)) (end2 (length s2)))
  (let ((lt t) (gt nil) (eq nil) (d1 0) (d2 0))
    (loop
     (when (and (= start1 end1) (= start2 end2)) (return eq))
     (when (= start1 end1) (return lt))
     (when (= start2 end2) (return gt))
     ;;
     (when 
         (and
          (> start1 d1) 
          (> start2 d2) 
          (digit-char-p (char s1 start1)) 
          (digit-char-p (char s2 start2)))
       (setq d1 (1+ start1)) (setq d2 (1+ start2))
       (loop 
        (when (or (= d1 end1) (not (digit-char-p (char s1 d1)))) (return))
        (incf d1))
       (loop 
        (when (or (= d2 end2) (not (digit-char-p (char s2 d2)))) (return))
        (incf d2)) 
       (when (> (- d1 start1) (- d2 start2)) (return gt))
       (when (< (- d1 start1) (- d2 start2)) (return lt)))
     ;;
     (when (char-lessp (char s1 start1) (char s2 start2)) (return lt))
     (when (char-greaterp (char s1 start1) (char s2 start2)) (return gt))
     (incf start1) (incf start2))))
      
;; Handle one key/clause and recurse for the rest
;; The clause is tree-searched for numeric keywords :1, :2, :3...
;; From those we determine which groups to ask all-matches for
;; Those groups are bound and the :1, :2... are replaces with the variable bound to.
;; I only look for single digit group numbers

(defun re-match-case-internal (string cases &key case-insensitive)
  (let ((matches (make-symbol "MATCHES")))
    (if (member (caar cases) '(t :otherwise))
	`(progn ,@(cdar cases))
	(let ((groups nil)
	      (body (cdar cases))
	      (re (caar cases)))
	  (when body
	    (tree-walk body (lambda(e) (when (and (symbolp e) (digit-char-p (char (string e) 0))) (pushnew e groups))))
	    (loop for g in (sort groups '< :key (lambda(e) (read-from-string (string e))))
		  for index from 0
		  for sym = (make-symbol (format nil "GROUP-~a" g))
		  do (setq body (subst sym g body))
		  collect `(,sym (nth ,index ,matches)) into bindings
		  collect (read-from-string (string g)) into indices
		  finally (return
			    ;; If no groups are specified then ask for the whole match (group 0)
			    `(let* ((,matches (car (all-matches ,string ,(if case-insensitive (concat "(?i)" re) re) ,@(or indices '(0))))))
			       (if ,matches
				   (let ,bindings
				     ,@body)
				   ,(re-match-case-internal string (cdr cases)))))))))))

;; A case like form, except the keys are regular expressions.
;; Clauses may use capture groups, calling them :1, :2 etc. Those keywords get replaced by the matched bit.
;; Can fall through to t or :otherwise if none
;; The keyplace is an atom in which case evaluated, or a list atom followed by key-value options
;; Current acceptable keys: :case-insensitive

;; Following the case documentation style: http://clhs.lisp.se/Body/m_case_.htm

;; re-case keyplace {clause}* => result*
;; keyplace::= atomic-keyplace| keyplace-with-options
;; atomic-keyplace::= string | symbol
;; keyplace-with-options::= (atomic-keyplace (key value)*)
;; clause::= normal-clause | otherwise-clause
;; normal-clause::= (regex form*) 
;; otherwise-clause::= ({otherwise | t} form*)

(defmacro re-match-case (string  &body cases)
  (if (consp string)
      (apply 're-match-case-internal (car string) cases (cdr string))
      (re-match-case-internal string cases nil)))
