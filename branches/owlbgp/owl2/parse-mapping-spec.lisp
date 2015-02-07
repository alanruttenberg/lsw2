(defparameter *mapping-symbol-types* '(("OP" ?object-property) ("OPE" ?object-property-expression)
				  ("DP" ?data-property) ("DPE" ?data-property-expression)
				  ("AP" ?annotation-property) ("C" ?class)
				  ("CE" ?class-expression) ("DT" ?datatype)
				  ("DR" ?data-range) ("U" ?IRI)
				  ("F" ?constraining-facet) ("a" ?individual) ("x" ?individual) ("y" ?individual) 
				  ("*:a" ?named-individual) ("lt" ?literal) ("n" ?n) ("m" ?m)
				  ("as" ?annotation-source) ("av" ?annotation-value)))
(defun flatten-td (el)
  (labels ((tokenize (el)
	     (remove "" (split-at-regex el "(\\s+)|(?=[()])|(?<=[()])|(?=\\^+)|(?<=\\^+)") :test 'equal))
	   (one-el (el)
	     (cond ((and (consp el) (equal (caar el) "i"))
		    (mapcan #'one-el (cddr el)))
		   ((and (consp el) (equal (caar el) "br"))
		    (assert (null (cdddr el)) () "extra: ~a " el)
		    (list (third el)))
		   ((and (consp el) (equal (caar el) "span")))
		   ((and (consp el) (equal (caar el) "sub"))
		    (assert (null (cdddr el)) () "extra: ~a " el)
		    (list (format nil "[~a]" (third el))))
		   ((stringp el)
		    (list (#"replaceAll" el "nbsp;" ""))
		    )
		   (t (error "~a" `(:dontknow ,el)))))
	   ;; note presence of blank seq in datatyperestriction. Needs special handling. It's the only one.
	   (int-sub (v sub &optional (p 'keyword))
	     `(:subscript
	       ,(or (second (assoc v *mapping-symbol-types* :test 'equal))
		    (intern (string-upcase v)))
	       ,(if (#"matches" (subseq sub 1 (- (length sub) 1)) "\\d+")
		    (parse-integer (subseq sub 1 (- (length sub) 1)))
		    (intern (concatenate 'string "?" (string-upcase (subseq sub 1 (- (length sub) 1)))))))))
    (let ((rest (mapcan #'tokenize (remove nil (mapcan #'one-el (cddr el))))))
      (labels ((get-next ()
		 (cond ((equal (car rest) ",") (pop rest) (get-next))
		       ((and (second rest) (equal (second rest) "["))
			(prog1 `(:optional ,(third rest))
			  (pop rest)(pop rest) (pop rest) 
			  (pop rest)))
		       ((and (second rest) (char= (char (second rest) 0) #\[))
			(prog1
			    (int-sub (car rest) (second rest))
			  (pop rest) (pop rest)))
		       ((equal (first rest) "...") 
			(pop rest)
			:elipsis)
		       ((char= (char (car rest) 0) #\_)
			(prog1
			    `(:blank ,(intern (string-upcase (concatenate 'string "?"  (subseq (car rest) 2)))))
			  (pop rest)))
		       ((char= (char (car rest) 0) #\*)
			(prog1
			    `?named-individual
			  (pop rest)))
		       ((and (not (equal (first rest) ":"))
			     (find #\: (first rest) :test 'equal))
			(make-uri nil (pop rest)))
		       ((equal (car rest) ")")
			(prog1 ")" (pop rest)))
		       ((equal (car rest) "ne;")
			(prog1 '/= (pop rest)))
		       ((equal (car rest) "ge;")
			(prog1 '>= (pop rest)))
		       ((equal (car rest) ">")
			(prog1 '> (pop rest)))
		       ((equal (second rest) "^")
			`(make-literal ,(intern (concatenate 'string "?" (string-upcase (read-from-string (pop rest)))))
				   (make-uri nil ,(progn (pop rest) (pop rest) (pop rest)))))
		       ((equal (second rest) "(")
			(if (equal (third rest) "SEQ")
			    (list (prog1 (register-owl2-function (pop rest)) (pop rest))
				  (loop for next = (get-next) until (equal next ")")
				     collect next 
				       ))
			    (list* (prog1 (register-owl2-function (pop rest)) (pop rest))
				   (loop for next = (get-next) until (equal next ")")
				      collect next 
				      ))))
		       ((assoc (car rest) *mapping-symbol-types* :test 'equalp)
			(second (assoc (pop rest) *mapping-symbol-types* :test 'equalp)))
		       (t (intern (string-upcase (pop rest)))))))
	(eval-uri-reader-macro  (remove '|.| (remove "." (loop until (null rest)
	   with cond = nil
	     for next = (get-next)
	   do 
	     (when (equal next '|,|)
		(setq cond t))
	   finally (progn (when cond (format t "Conditional: ~a ~%" form)) (return form))
	   collect next into form) :test 'equal)))))
    ))

(defvar *owl2-functions* (make-hash-table))

(defun register-owl2-function (string)
  (let ((sym (intern (string-upcase string))))
    (if (eq sym 't)
	't
	(if (eq sym :tann)
	    'tann
	    (progn
	      (when (gethash sym *owl2-functions*)
		(assert (equal string (gethash sym *owl2-functions*)) () "Oops - collision ~a, ~a" string (gethash sym *owl2-functions*)))
	      (intern (string-upcase (setf (gethash sym *owl2-functions*) string)))
	      )))))
  
(defun mapping-table ()
  (flet ((triplify (els)
	   (loop with rest = els while rest
	      if (eq (car rest) :elipsis) collect (pop rest)
	      else collect (list 'triple (pop rest) (pop rest) (pop rest)))))
    (let ((table (second (find-elements-with-tag
			  (xmls:parse
			   (get-url "http://www.w3.org/2007/OWL/wiki/Mapping_to_RDF_Graphs"
				    :persist nil :force-refetch t))  "table"))))
      (loop for row in (rest (find-elements-with-tag table "tr"))
	 for flattened-row = (mapcar 'flatten-td (find-elements-with-tag row "td"))
	 do (setf (second flattened-row) (triplify (second flattened-row)))
	 collect flattened-row)
      )))

;(defparameter *rdf-mapping-table* (mapping-table))

(defvar *rdf-mapping-table* nil)

(defun check-elipses ()
  (labels ((compare2 (a b)
	     (print-db a b)
	     (cond ((and (atom a) (atom b))
		    (or (equal a b)
			(and (eql a 1) (member b '(:k :n :m)))))
		   ((and (consp a) (consp b))
		    (every #'compare2 a b))
		   (t nil)))
	   (check-parallel (form)
	     (cond ((atom form) t)
		   ((consp form)
		    (loop for (this next last peek1 peek2) on form
			 with last-type
		       do
			 (cond ((and (consp this) (eq (car this) :subscript) (eq last :elipsis))
				(format t "Compare: ~a To: ~a  in ~a => ~a ~%" this peek1 form (if (compare2 this peek1) "ok" "problem"))
				(format t "Compare: ~a To: ~a  in ~a => ~a ~%" next peek2 form (if (compare2 next peek2) "ok" "problem")))
			       ((and (consp this) (eq (car this) :subscript) (eq next :elipsis)
				     (not (and (consp peek1) (eq (car peek1) :subscript)))
				     (eq next :elipsis))
				(format t "Compare: ~a To: ~a  in ~a => ~a ~%" this last form (if (compare2 this last) "ok" "problem")))
			       (t (check-parallel this))))))))
    (loop for row in *rdf-mapping-table*
       for (syntax triples head) = row
       do
       (check-parallel syntax))))

(defun rewrite-elipses ()
  (labels ((rewrite (form)
	     (let ((result
		    (cond ((atom form) form)
			  ((consp form)
			   (loop for (this next last peek1 peek2) = form
			      while form
			      for result =
			      (cond ((and (consp this) (eq (car this) :subscript) (eq last :elipsis))
				     (setq form (nthcdr 5 form))
				     `(:sequence ,peek1 ,peek2))
				    ((and (consp this) (eq (car this) :subscript) (eq next :elipsis)
					  (not (and (consp peek1) (eq (car peek1) :subscript)))
					  (eq next :elipsis))
				     (setq form (nthcdr 3 form))
				     `(:sequence ,last))
				    (t (rewrite (pop form))))
				collect result)))))
	       (print-db form result)
	       result)))
    (loop for row in *rdf-mapping-table*
       for (syntax triples head) = row
       do
       (print-db syntax (rewrite syntax)))))

(defun print-for-sandro()
  (let ((*print-case* :downcase)
	(*print-readably* nil))
    (loop for (input output head) in (mapping-table)
       do
       (format t "~%Input: ~a" input)
       (format t "~%T(Input):~%~{~a~%~}" output)
       (and head (format t "Main Node: ~a" (or head "")))
       (terpri))))

(defun tree-find (sym tree)
  (cond ((atom tree)
	 (eq sym tree))
	(t (some (lambda(el) (tree-find sym el)) tree))))

(defun subscripted-variables (tree)
  (let ((them nil))
    (labels ((look (it)
	       (cond ((atom it) nil)
		     ((and (consp it) (eq (car it) :subscript))
		      (pushnew (second it) them))
		     (t (map nil #'look it)))))
      (look tree))
    them))

(defun classify-subscripting (input output)
  (let ((subscripted (subscripted-variables input)))
    (cond ((> (length (subscripted-variables input)) 1)
	   :multi-variable)
	  ((tree-find '?n-1 output)
	   :foreach)
	  ((tree-find :elipsis input)
	   :sequence)
	  ((> (length subscripted) 0)
	   :named-variable)
	  (t :subscript-free))))

'(loop for (input output head) in (mapping-table)
     do
     (print-db (classify-subscripting input output) input))

(defparameter *overridden-rdfm* nil)

(defun write-defs ()
  (loop for (input output head) in (mapping-table)
       for classified =  (classify-subscripting input output)
;       when (member classified '(:named-variable :subscript-free :sequence :foreach)) ; the ones we can handle for now
       do
       (let ((name (if (consp (car input)) (caar input) (car input)))
	     (pattern (car (if (consp (car input)) input (list input)))))
	 (let ((*print-case* :downcase))
	   (if (assoc name *overridden-rdfm*)
	       (progn
		 (format t ";; Not automatically translated - overridden by manual translation")
		 (pprint (second (assoc name *overridden-rdfm*)))
		 (terpri))
	       (format t "(defrdfm ~a ~%    ~s~%~{  ~s~^~%~})~%~%" name `(:pattern ,pattern ,@(if head (list :head (car head))) :case ,classified) output)
	   )))))
       

(defun multiple-case-heads ()
  (let ((table (make-hash-table :test 'equalp)))
    (loop for (input output head) in (mapping-table)
       when (consp (car input))
       do (push input (gethash (caar input) table)))
    (maphash (lambda(key value)
	       (when (> (length value) 1)
		 (format t "~a~%~{  ~a~%~}" key value)))
	     table)))

;; if you don't have a :head, then you can have annotations.