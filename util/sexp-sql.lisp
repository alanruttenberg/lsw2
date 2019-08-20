;; TODO
;; Syntax for "as" for columns - defining headers. Maybe don't need?
;; Syntax for "as" for tables then using the aliases. Needed for self-join where table needs to be referenced twice
;; handling expressions 

;; :column-initial-cap
;; :column-case-sensitive
;; :table-initial-cap
;; :table-case-sensitive
;; :db
;; :schema

(defvar *sql-defaults* nil)

(defun set-default-schema-options (schema &rest defaults)
  (setq *sql-defaults* (remove schema *sql-defaults* :key 'car))
  (push (list* schema defaults) *sql-defaults*))

(defvar *mentioned-columns*)


(render-sql
 '(:defaults :axium)
 '((txdiag chart-trx diag-code)
   (treatme2 (tm-auto auto) (pid "PId") deleted)
   (patient2 (p2-auto auto) (ptxpid "PtxPId") chart-id date tooth-state permanent-state)
   (fitem code f-f-item)
   (pitem (p-f-item f-item) patient ans-yes))
 '((f-f-item p-f-item)
   (p2-auto pid)
   (patient ptxpid)
   (chart-trx tm-auto))
 '(:select (:distinct t :order-by chart-id)
   (chart-id
    (:at-max-within tooth-state date chart-id)
    (:at-max-within permanent-state date chart-id))
   (:in diag-code 208602 863520 579834 674943 375160 785649 501493 713657 976264 630554 977376 976964 642230 977088 961043 181721 600061 316781)
   (:= ans-yes 1)
   (:= deleted 0)
   (:= diag-code "CMADEC1C2"))))

(defmacro format-sql-query (options bindings joins &body query)
  `(render-sql ',options ',(car query) ',joins ',bindings))

'(defun convert-bindings-tables (bindings options)
  (loop for binding in bindings
	for table = (car binding)
	for columns = (cdr binding)
	with explicit-schema = (getf options :schema)
	collect (cons (if (consp table) (car table) table)
		      (let ((*print-case* :downcase))
			(format nil "~a~s" 
				(if explicit-schema (format nil "~a." explicit-schema) "")
				(if (consp table)
				    (second table)
				    table))))))

(defun default-schema-options (options)
  (let ((defaults (getf options :defaults)))
    (if defaults
	(append (cdr (assoc defaults *sql-defaults*)) options)
	options)))
			       
;; Bindings is a list of tables with a list of columns used for each table
;; bindings:: '('<table> (<column> | <column+name>)+ ')'
;; column:: <symbol> 
;; column+name:: '(' <symbol> (<symbol> | <string>) ')'
;; table:: <symbol>
;;
;; Options control how symbols are rendered. Tables are rendered in lowercase.
;;
;; Columns that are symbols are understood to be camelecase, with first-cap determined by option :column-initial-cap
;; for column+name if a string then it is a literal otherwise converted as a symbol
;;
;; Creates an environment used subsequently
;; Each element of the environment is a list with first element the symbol
;; If a table then the second element is the string that will be rendered for it
;; If a column then the second element is the string prefixed column as it will be used followed by the table it is from (for joins)
;;
;; e.g.
;; (convert-bindings-columns '((txdiag chart-trx diag-code)
;; 			    (treatme2 (tm-auto auto) (pid "PId") deleted)) '(:defaults :axium))
;; ((TXDIAG "txdiag") 
;;  (CHART-TRX "txdiag.ChartTrx" TXDIAG)
;;  (DIAG-CODE "txdiag.DiagCode" TXDIAG)
;;  (TREATME2 "treatme2")
;;  (TM-AUTO "treatme2.Auto" TREATME2)
;;  (PID "treatme2.PId" TREATME2)
;;  (DELETED "treatme2.Deleted" TREATME2))
;;
;; Each bound symnbol must be unique

(defun convert-bindings-columns (bindings options)
  (setq options (default-schema-options options))
  (loop for binding in bindings
	for table-spec = (car binding)
	for (table qualified-table) = (parse-table-spec options table-spec)
	for columns = (cdr binding)
	collect (list table qualified-table)
	append
	(loop for column-spec in columns
	      for (column column-string) = (parse-column-spec options column-spec)
	      collect
	      (list (if (consp column) (car column) column)
		    (format nil "~a.~a" qualified-table column-string)
		    qualified-table))))

;; takes in a table spec and returns a table prefix 
(defun parse-table-spec (options spec)
;; (procedur ...
;; (foo.procedur 
;; ((procedur "PRocedur") ...
;; ((foo.procedur "PRocedur") ...
;; ((procedur "foo.PRocedur") ...
;; ((procedure-a :schema "Foo")
  (let ((raw-table
	  (if (consp spec)
	      (car spec)
	      spec))
	(raw-schema (or (and (consp spec)
			   (second (member :schema spec)))
		      (getf options :schema))))
    (when (find #\. (string raw-table))
      (let ((split (split-at-char (string raw-table) #\.)))
	(setq raw-schema (if (symbolp raw-table) (intern (car split) (symbol-package raw-table)) (car split)))
	(setq raw-table (if (symbolp raw-table) (intern (second split) (symbol-package raw-table)) (second split)))))
    (list raw-table
	  (concatenate 'string
		       (if raw-schema
			   (maybe-casify-name
			    raw-schema
			    (getf options :schema-case-sensitive)
			    (getf options :schema-initial-cap))
			   "")
		       (if raw-schema "." "")
		       (maybe-casify-name
			raw-table
			(getf options :table-case-sensitive)
			(getf options :table-initial-cap))
		       ))))


(defun parse-column-spec (options spec)
  ;; procedur 
  ;; (procedur "PRocedur") ...
  (assert (or (not (consp spec))
	      (and (consp spec) (cdr spec)))
	  () "Improper column spec: ~a" spec)
  (list (if (consp spec) (car spec) spec)
	(maybe-casify-name
	 (if (consp spec) (second spec) spec)
	 (getf options :column-case-sensitive)
	 (getf options :column-initial-cap))
	))

(defun maybe-casify-name (name case-sensitive initial-cap)	
 (let ((string 
	 (if (stringp name)
	     name
	     (if case-sensitive
		 (camelcase (string name) initial-cap)
		 (progn
		   (string-downcase (string name)))))))
   (if case-sensitive
       (format nil "~s" string)
       (if (stringp name)
	   name
	   (string-downcase (string name))))))

(defun render-sql (options  bindings form joins &optional stream)
  (setq form (macroexpand-all form))
  (let ((*mentioned-columns* nil))
    (setq stream (or stream (make-string-output-stream)))
    (ecase (car form)
      (:select (destructuring-bind (select-options selections &rest conditions) (cdr form)
		 (let ((env (append (if (getf select-options :bindings)
					(convert-bindings-columns (getf select-options :bindings) options) )
				    (convert-bindings-columns bindings options)))
		       (joins (or (getf select-options :joins) joins)))
		   (let ((select (render-sql-select select-options selections env nil))
			 (conditions (with-output-to-string (s) (render-sql-conditions select-options conditions joins env s)))
			 (explicit-join-string (with-output-to-string (s) (render-explicit-joins options joins env s)))
			 (groupby (if (getf select-options :group-by)
				      (format nil "~%group by ~{~a~^, ~}" 
					      (let ((columns (getf select-options :group-by)))
						(mapcar (lambda (e)
							  (render-sql-column-form options e env nil))
							(if (listp columns) columns (list columns)))))
				      ""))
			 (orderby (if (getf select-options :order-by)
				      (format nil "~%order by ~{~a~^, ~}" 
					      (let ((columns (getf select-options :order-by)))
						(mapcar (lambda (e)
							  (render-sql-column-form options e env nil))
							(if (listp columns) columns (list columns)))))
				      "")))

		     (let ((candidate-froms (selection-tables env *mentioned-columns*))
			   (explicit-join-tables (selection-tables env
								   (loop for (from nil type) in joins
									 when type collect from)))
			   )
		     (format stream "~a~%from ~{~a~^, ~}~a ~a~a~a~%"
			     select
			     (set-difference  candidate-froms explicit-join-tables :test 'equalp)
			     explicit-join-string
			     conditions
			     groupby
			     orderby
			     ))))))
			     
      )
    (get-output-stream-string stream)))

;; BUG:: if explicit join the table shouldn't me in the from clause

(defun render-sql-select (options selections env stream)
  (with-output-to-string (s)
    (write-string "select " s)
    (when (getf options :count)
      (write-string "count(") s)
    (when (getf options :distinct)
      (write-string "distinct(" s))
    (format s "~{~a~^,~%~}"
	    (loop for sel in selections
		  collect
		  (render-sql-column-form options sel  env )))
    (when (getf options :count)
      (write-string ")" s))
    (when (getf options :distinct)
      (write-string ")" s))
  ))

;; TODO make a proper environment (typed as column or table)
(defun env-tables (env)
  (loop for (nil string from) in env
	unless from
	  collect string))

(defun selection-tables (env selections)
  (loop for s in selections
	with them
	do (pushnew (env-column-table env s) them :test 'equalp)
	finally (return (reverse them))))

(defun env-column-string (env sym)
  (let ((string (second (find-if (lambda(e) (and (eq (car e) sym) (= (length e) 3))) env))))
    (assert string (string) "~a used as column but not defined" sym)
    string))

(defun env-table-string (env sym)
  (let ((string (second (find-if (lambda(e) (and (eq (car e) sym) (= (length e) 2))) env))))
    (assert string (string) "~a used as table but not defined" sym)
    string))

(defun env-column-table (env sym)
  (let ((table (third (find-if (lambda(e) (and (eq (car e) sym) (= (length e) 3))) env))));(third (assoc sym env))))
    (assert table (table) "~a used as column but no table found" sym)
    table))

(defun render-sql-conditions (options conditions joins env stream)
  (format stream " where ~% ~{~a~^ and~%~}"
	  (loop for condition in conditions
		collect
		(render-sql-condition options condition env)))
  (loop for join in joins
	for (column1 column2 type) = join
	when (null type)
	  do 
	     (format stream "~%and ~a=~a"
		     (env-column-string env column1)
		     (env-column-string env column2))))

;; no expressions yet

(defun render-sql-condition (options condition env)
  (ecase (car condition)
    ((:= :<> :> :< :>= :<= :!< :!>)
     (format nil "~a ~a ~a"
		(if (symbolp (second condition))
		    (progn (column-mention (second condition) 'conditions)
			   (env-column-string env (second condition)))
		    (render-sql-literal options (second condition)))
		(car condition)
		(if (symbolp (third condition))
		    (progn
		      (column-mention (third condition) 'conditions)
		      (env-column-string env (third condition)))
		    (render-sql-literal options (third condition)))))
    (:in  (column-mention (second condition) 'conditions)
     (format nil "~a in (~{~a~^, ~})" (env-column-string  env (second condition))
	     (if (and (consp  (cddr condition))
		      (consp (car (cddr condition)))
		      (eq :select (car (caddr condition))))
		 (list (eval `(format-sql-query ,options, nil nil ,(caddr condition))))
		 (mapcar (lambda(e) (render-sql-literal options e))
			 (cddr condition)))))
    (:or 
     (format nil "(~{(~a) ~^or ~})"
	     (loop for dis in (cdr condition) collect (render-sql-condition options dis env))))
    (:and
     (format nil "(~{(~a) ~^and ~})"
	     (loop for dis in (cdr condition) collect (render-sql-condition options dis env))))
    (:not (format nil "(not ~a)"
		  (render-sql-condition options (second condition) env)))
    (:starts-with 
     (format-starts-with options condition env))
    (:like 
     (format-like options condition env))
    (:like-insensitive 
     (format-like-insensitive options condition env))))

(defun ensure-ansi-date-format (string)
  (if (#"matches" string "\\d{4}-\\d{2}-\\d{2}")
      (destructuring-bind (month date)
	  (mapcar 'read-from-string (car (all-matches string "\\d{4}-(\\d{2})-(\\d{2})" 1 2)))
	(assert (and (<= 1 month 12)
		     (<= 1 date 31))
		(string month date)
		"~a isn't an ANSI date" string)
	string)
      (error "~a isn't an ANSI date" string)))

(defun format-in (options condition env)
  (column-mention (second condition) 'conditions)
  (format nil "~a in (~{~a~^, ~})" (env-column-string  env (second condition))
	  (if (and (consp  (cddr condition))
		   (consp (car (cddr condition)))
		   (eq :select (car (caddr condition))))
	      (list (eval `(format-sql-query ,options, nil nil ,(caddr condition))))
	      (mapcar (lambda(e) (render-sql-literal options e))
		      (cddr condition)))))

(defun format-like (options condition env)
  (format nil "(~{~a like '~a'~^ or ~})"
	  (loop with var = (env-column-string env (print-db (second condition)))
		for match in (cddr condition)
		collect var 
		collect match)))

(defun format-like-insensitive (options condition env)
  (format nil "(~{UPPER(~a) like '~a'~^ or ~})"
	  (loop with var = (env-column-string env (second condition))
		for match in (cddr condition)
		collect var 
		collect (string-upcase match))))

(defun format-starts-with (options condition env)
  (format-like options (list* (car condition) (second condition)
			     (mapcar (lambda(e) (concatenate 'string e "%")) (cddr condition)))
	       env))
(defun render-sql-literal (options lit)
  (if (consp lit)
	     (ecase (car lit)
	       (:date   (format nil "date '~a'" (second lit))))
      (format nil "'~a'" lit)))

;; Need syntax for "as"
(defun render-sql-column-form (options form env &optional (with-alias t))
  (cond ((and (consp form) (not (keywordp (car form))))
	 (column-mention (second form) 'column-form)
	 (if with-alias
	     (format nil "~a" (render-sql-column-form options (second form) env with-alias) (string-downcase (string (car form))))
	     (render-sql-column-form options (second form) env with-alias)))
	((symbolp form)
	 (column-mention form 'column-form)
	 (if with-alias
	     (format nil "~a" (or (second (assoc form env)) (string-downcase (string form))) (string-downcase (string form)))
	     (env-column-string env form)))
	;; disallow because we can't determine table
	;;((stringp form)
	;; (format nil "~a" form))
	((consp form)
	 ;; BUG: Need a way to specify colunm alias?
	 (ecase (car form)
	   ((:max :min :count :distinct) (format nil "~a(~a)" (string-downcase (car form)) (render-sql-column-form options (second form) env nil)))
	   (:at-max-within
	    (destructuring-bind (returned-column max-column group-column) (cdr form)
	      (format nil "first_value(~a) over (partition by ~a order by ~a desc)"
		      (render-sql-column-form options returned-column env nil)
		      (render-sql-column-form options group-column  env nil)
		      (render-sql-column-form options max-column  env nil))))))
	(t (error "don't know what ~a is" () form))))

(defun column-mention (column &optional tag)
  (if (boundp '*mentioned-columns*)
      (pushnew column *mentioned-columns*)
      (error "huh? ~a" column)))

;; The joins are a list of list of a pair of column symbols optionally followed by join type :inner, :outer, :left, :right
;; column symbol is unique and related to table, so tables are not specified.
;; If there is no join type :inner is assumed
;; Inner joins are rendered in the where clause. The rest are rendered as explicit joins

;; writes to stream a string to be used after 'from' tables
(defun render-explicit-joins (options joins env stream)
  (format stream "~%~{~a~%~}"
	  (loop for join in joins
		for (column1 column2 type) = join
		for column1-table-string = (third (assoc column1 env))
		unless (null type)
		  do (column-mention column2) and
		  collect (format nil "~a JOIN ~a on ~a=~a"
				  (string-upcase (string type))
				  column1-table-string
				  (second (assoc column1 env))
				  (second (assoc column2 env))))))

(defun maybe-add-schema (table-name options)
  (if (getf options :schema)
      (format nil "~a.~a" (getf options :schema) table-name)
      table-name))

;;
;; SQL is implemented as if a query was executed in the following order:

;; FROM clause
;; WHERE clause
;; GROUP BY clause
;; HAVING clause
;; SELECT clause
;; ORDER BY clause
;; For most relational database systems, this order explains which names (columns or aliases) are valid because they must have been introduced in a previous step.

;; So in Oracle and SQL Server, you cannot use a term in the GROUP BY clause that you define in the SELECT clause because the GROUP BY is executed before the SELECT clause.

