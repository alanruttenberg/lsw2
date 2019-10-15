(in-package :logic)

(defun clif-form-to-lsw (form)
  (labels ((walk (form bindings)
	     (print-db form bindings)
	     (break)
	     (let ((result 
		     (if (atom form)
			 (or (second (assoc form bindings)) form)
			 (let ((head (car form)))
			   (if (consp head)
			       (error "Can't handle common logic 'wild-west' expression as predicate : ~a" head)
			       (let ((name (string head)))
				 (cond ((member name '("FORALL" "EXISTS") :test 'equalp)
					(let ((vars (second form)))
					  (assert (every 'symbolp vars) () "Quantifier ~a expects a list of variables but got ~a" (second form))
					  (let ((renamed (loop for sym in vars
							       if (char= (char (string sym) 0) #\?)
								 collect (list sym sym)
							       collect (list sym (intern (concatenate 'string "?" (symbol-name sym)) (symbol-package sym))))))
					    `(:forall ,(mapcar 'second renamed)
					       ,@(mapcar (lambda(e) (walk e (append renamed bindings))) (cddr form))))))
				       ((member name '("AND" "OR" "IFF" "IF" "NOT" "=") :test 'equalp)
					`(,(if (equal name "IF") :implies (intern name  'keyword))
					  ,@(mapcar (lambda(e) (walk e bindings)) (cdr form))))
				       (t `(,head ,@(mapcar (lambda(e) (walk e bindings)) (cdr form)))))))))))
	       (print-db result)
	       result)))
    (walk form nil)))
								    
			     

(defparameter *clif-punctuation* '(#\~ #\! #\# #\$ #\% #\^ #\& #\* #\_ #\+ #\{ #\} #\| #\: #\< #\> #\? #\` #\- #\= #\[ #\] #\; #\, #\. #\/))

(defun clif-name-char-p (char)
  (or (alpha-char-p char)
      (digit-char-p char)
      (member char *clif-punctuation* :test 'eql)))

(defparameter *clif-readtable*
  (let ((table (copy-readtable)))
    (let ((*readtable* table))
      (set-syntax-from-char #\' #\")
      (set-macro-character #\' (lambda(stream char) `(:literal ,(funcall (get-macro-character #\")  stream #\'))))
      (flet ((read-clif-name (stream char)
	       (coerce (cons char (loop for next = (peek-char nil stream nil :eof)
					until (or (eq next :eof)
						  (not (clif-name-char-p next)))
					collect (read-char stream))) 'string)))
	(loop for char in (append *clif-punctuation*

				  (loop for char from (char-code #\a) to (char-code #\z)
					collect (code-char char))
				  (loop for char from (char-code #\A) to (char-code #\Z)
					collect (code-char char))
				  (loop for char from (char-code #\0) to (char-code #\9)
					collect (code-char char)))
	      do (set-syntax-from-char char #\a)
		 (set-macro-character char #'read-clif-name))))
    table))

(defun read-clif-form (stream)
  (let ((*readtable* *clif-readtable*))
    (read stream)))

(defun test-read-clif ()
  (loop for (string result) in
	'(("..." "...")
	  ("a|b" "a|b")
	  ("'a|b'" (:literal "a|b"))
	  ("(cl:comment 'a|b')" ("cl:comment" (:literal "a|b"))))
	for read = (read-clif-form (make-string-input-stream string))
	do
	   (print-db read result)
	   (assert (equalp read result) ()
		   "input: ~s, expected: ~s, got:~s" string result read)))

;(test-read-clif)
