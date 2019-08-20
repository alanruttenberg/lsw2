(in-package :cl-user)

;; Copyright © 2013 Alan Ruttenberg and SUNY at Buffalo All Rights Reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:

;; 1. Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.

;; 2. Redistributions in binary form must reproduce the above
;; copyright notice, this list of conditions and the following
;; disclaimer in the documentation and/or other materials provided
;; with the distribution.

;; 3. The name of the author may not be used to endorse or promote
;; products derived from this software without specific prior written
;; permission.

;; THIS SOFTWARE IS PROVIDED BY Alan Ruttenberg "AS IS" AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(defvar *default-connection* nil)
(defvar *jdbc-trace* nil)

;; if requested in call to sql-query save the query results, indexed by the SQL+JDBC URL. We can get the JDBC URL from
;; the connection using (#"getURL")

(defvar *jdbc-query-cache* (make-hash-table :test 'equalp))

(defun with-jdbc-connection (fn jdbc-url)
  (let ((connection nil))
    (unwind-protect
	 (progn
	   (when (search "jdbc:sqlite" jdbc-url) 
	     (#"forName" 'Class "org.sqlite.JDBC"))
	   (if (equal (car *default-connection*) jdbc-url)
	       (progn (funcall fn (cdr *default-connection*)))
	       (let ((*default-connection* (cons jdbc-url (setq connection (#"getConnection" 'java.sql.DriverManager jdbc-url)))))
		 (funcall fn (cdr *default-connection*)))))
      (when (and connection (not (equal (car *default-connection*) jdbc-url)))
	(#"close" connection)))))

(defun set-default-jdbc-connection (jdbc-url)
  (setq *default-connection* (cons jdbc-url (#"getConnection" 'java.sql.DriverManager jdbc-url))))

(defgeneric with-jdbc-connection-named (keyword function)
  (:documentation "if a sql query is executed with a connection that is a keyword instead of a connection object, the sql is wrapped in a lambda and passed to this function, which calls the lambda with a single argument which is the connection. Clients of this functionality need to define eql methods on the first argument to implement the necessary setup of the connection"
  ))

(defun sql-query-render (query &optional with-linefeeds)
  (if (listp query)
      (format nil (if with-linefeeds "~{~a~^ ~%~}" "~{~a~^ ~}") 
	      (mapcar (lambda(e) (if (stringp e) e (apply 'format nil e))) query))
      query))
    
(defun check-if-known-database (connection)
  (assert   (or (equal "com.microsoft.sqlserver.jdbc.SQLServerConnection" (jclass-name (jobject-class connection)))
		(equal "oracle.jdbc.driver.T4CConnection" (jclass-name (jobject-class connection)))
		(equal "com.hxtt.sql.access.r" (jclass-name (jobject-class connection)))
		(equal "org.postgresql.jdbc4.Jdbc4Connection" (jclass-name (jobject-class connection)))
		(equal "org.sqlite.Conn" (jclass-name (jobject-class connection)))
		(equal "com.mysql.jdbc.JDBC4Connection" (jclass-name (jobject-class connection)))
		(equal "sybase.jdbc4.sqlanywhere.IConnection" (jclass-name (jobject-class connection))))
	    (connection) "Don't yet support sql-query for ~a" (jclass-name (jobject-class connection))))
   
(defun maybe-format-sql (query &optional format-args)
  (if (stringp query) (Setq query (list query)))
  (cond ((and (listp query)
	      (stringp (car query)))
	 (setq query (format nil "~{~a~^ ~}" 
			(mapcar (lambda(e) 
				  (if (stringp e) e (apply 'format nil e)))
				query)))
	 (when format-args (setq query (apply 'format nil query format-args)))
	 query)
	((listp query)
	 (eval query))))


(defun maybe-default-connection (connection)
  (assert (or connection *default-connection*) (connection) "Either connection or *default-connection* needs to be non-nil")
  (or connection (cdr *default-connection*)))


;; Do a sql query to connection. Result is a list of list, with each list one row of fields.
;; if with-headers is non-nil, then the values are instead ("fieldname" . value) instead of just value.
;; query can be a list of strings, in which case they are concatenated
;; if format-args is supplied then the query string used as a format string with the format-args
;; if print is non-nil then the rows are printed in a tabular form and there is no return value
;; otherwise two values are returned:
;;   1. the result of the query,
;;   2. the field names of the resultset

(defvar *last-sql-query* nil)
(defvar *last-sql-query-results* nil)
(defvar *always-cache-sql* nil)

(defun sql-query (query &rest args &key (connection (cdr *default-connection*)) with-headers print format-args trace (dwim-capitalized t) cache save-to)
  (if (keywordp connection) 
      (return-from sql-query 
	(with-jdbc-connection-named connection
	  (lambda(c) (apply 'sql-query query c args))))
      (setq connection (maybe-default-connection connection)))
  (setq query (maybe-format-sql query format-args))
  (when (or trace *jdbc-trace*) (print query))
  (check-if-known-database connection)
  (if (and dwim-capitalized (equal "oracle.jdbc.driver.T4CConnection" (jclass-name (jobject-class connection))))
      (setq query (oracle-quote-capitalized query)))
  ;; Now we have the query
  (setq *last-sql-query* query)

  ;; maybe return cached query, otherwise clear cache for this query
  ;; BUGS:
  ;;  - The print and with-headers args are not handled. If you change them the cache will be wrong
  ;;  - Don't distinguish no cache hit vs result nil
  (if (or cache *always-cache-sql*) 
      (let* ((cache-key (cons (#"getURL" connection) query))
	     (cache-value (gethash cache-key *jdbc-query-cache*)))
	(when (and cache cache-value)
	  (setq *last-sql-query-results* (car cache-value))
	  (return-from sql-query (values-list cache-value))))
      (remhash  (cons (#"getURL" connection) query) *jdbc-query-cache*))

  ;; otherswise execute the query
  (if save-to
      (with-open-file (f save-to :if-exists :supersede :if-does-not-exist :create :direction :output)
	(let ((*standard-output* f))
	  (print-or-return-query-results query connection t with-headers (or cache *always-cache-sql*))))
      (print-or-return-query-results query connection print with-headers (or cache *always-cache-sql*)))
  )

(defun print-or-return-query-results (query connection print with-headers cache &optional file)
  (let ((statement :s)
	(results :r))
    (unwind-protect 
	 (progn
	   (setq statement (#"createStatement" connection))
	   (setq results (#"executeQuery" statement query))
	   (let ((field-names
		   (loop 
		     for i from 1 to (#"getColumnCount" (#"getMetaData" results))
		     collect (#"getColumnName" (#"getMetaData" results) i))))
	     (when print 
	       (format t "~{~a~^	~}~%" field-names))
	     (loop
	       while (#"next" results) 
	       collect
	       (loop 
		 for field in field-names
		 for index from 1
		 if with-headers
		   collect (cons field (#"getString" results index)) into columns 
		 else collect (#"getString" results index) into columns
						
		 finally (progn
			   (when print
			     (format t "~{~a~^	~}~%"
				     (if with-headers
					 (mapcar 'cdr columns)
					 columns 
					 )
				     ))
			   (return columns)
			   ))
		 into rows
	       finally
		  (progn
		    (setq *last-sql-query-results* rows)
		    (when cache (setf (gethash (cons (#"getURL" connection) query) *jdbc-query-cache*) (list rows field-names)))
		    (return (if print
				nil
				(values rows field-names)))))))
      (and (not (eq results :r)) results (#"close" results))
      (and (not (eq statement :s)) statement (#"close" statement)))
    ))

;; Oracle is annoying. Table and column names are case insensitive unless surrounded by double quote.
;; This takes a string and adds those quotes, effectively making the queries case sensitive .
(defun oracle-quote-capitalized (string)
  (if (find #\" string)
      string
      (progn
	(setq string (#"replaceAll" string "([$])" "\\\\$1" ))
	(let* ((replacements nil)
	       (count 0)
	       (protected 
		 (replace-all string "('\\S+')" (lambda(quoted)
						  (let ((token (format nil "'@@~a@@'" (incf count))))
						    (push (list token quoted) replacements)
						    token))
			      1))
	       ;; fixed: A capitalized string can have more than one capital letter
	       (capitalized (#"replaceAll" protected "([A-Z]+[a-z_][A-Za-z_]*)" "\"$1\""))
	       (reconstituted (replace-all capitalized "('@@\\d+@@')" (lambda(token) (second (assoc token replacements :test 'equalp))) 1)))

;	  (print-db protected capitalized reconstituted)
	  reconstituted			;(#"replaceAll" reconstituted "([$])" "\\\\$1" ) ;; also quote $ signs
	  ))))

(defun sql-server-driver-properties ()
  (map 'list (lambda(e)
	       (cons (get-java-field e "name" t)
		     (get-java-field e "description" t)))
       (get-java-field 'SQLServerDriver "DRIVER_PROPERTIES"t)))

;;(add-to-classpath "/Volumes/Big/Downloads/2011/2011-01-13/sqljdbc_3.0/enu/sqljdbc4.jar")
;;(add-to-classpath "/Volumes/Big/Downloads/2013-04-12/sqljdbc_4.0/enu/sqljdbc4.jar")
;; sqljdbc4.jar is in lib/
;; not enough in java 7 - put sqljdbc4.jar in same dir as abcl.jar

(defun table-column-names  (table connection)
  (mapcar 'car (car (sql-query (list "select top 1 * from ~a" table) :connection connection :with-headers t))))

(defun table-rowcount (table connection)
  "Return the number or rows in a table"
  (caar (sql-query (list "select count(*) from ~a" table) :connection connection)))

(defun sql-server-columns-matching (connection &key (table-match "%")  schema (column-match "%"))
  "Retrieve table,schema,column for all tables. Constraint tables to match table-match and column to match column-match"
  (sql-query (list "SELECT t.name AS table_name, SCHEMA_NAME(schema_id) AS schema_name, c.name AS column_name FROM sys.tables AS t
INNER JOIN sys.columns c ON t.OBJECT_ID = c.OBJECT_ID
where t.name like '~a' and c.name like '~a'
ORDER BY schema_name, table_name;" table-match  column-match) :connection connection))


;; let's assume this works for all connections until we know otherwise
;; documentation http://docs.oracle.com/javase/7/docs/api/java/sql/DatabaseMetaData.html

;; return a list of pairs of table name and comment if available
(defun table-names (connection)
  (let ((arr (jnew-array "java.lang.String" 1)))
    (jarray-set arr "TABLE" 0)
    (loop with tables = 
	 (#"getTables" (#"getMetaData" connection) +null+ +null+ "%" arr)
       for next? = (#"next" tables)
       until (not next?)
       if (member (#"getString" tables 5) '(nil "") :test 'equal)
       collect (list (#"getString" tables 3))
       else collect (list (#"getString" tables 3) (#"getString" tables 5))
    )))

;; return a list of pairs of columns and comment if available
(defun table-columns (connection table)
  (loop with tables = 
       (#"getColumns" (#"getMetaData" connection) +null+ +null+ table "%")
     for next? = (#"next" tables)
     until (not next?)
     if (member (#"getString" tables 12) '(nil "") :test 'equal)
     collect (list (#"getString" tables 4) )
     else collect (list (#"getString" tables 4) (#"getString" tables 12) )
       ))

;; return a hash of table-name to list of column-names
(defun get-all-tables-and-columns (connection)
  (loop with tables = (make-hash-table :test 'equal)
     with meta = (#"getColumns" (#"getMetaData" connection) +null+ +null+ "%" "%")
     for next? = (#"next" meta)
     until (not next?)
     do (push  (list (#"getString" meta 4) (#"getString" meta 12)) (gethash  (#"getString" meta 3) tables))
     finally (return tables)))

;; return list of forms (source-table source-column target-table
;; target-column), where the source column is a foreign key to the
;; target-table and target column. Note this includes both foreign
;; keys in and out of the table passed as argument to the function
(defun table-references (connection table)
  (loop for (target) in (table-names connection) append
       (loop with meta = (#"getCrossReference" (#"getMetaData" connection) +null+ +null+ table +null+ +null+ target)
	  for next? = (#"next" meta)
	  until (not next?)
	  collect (list (#"getString" meta 7)  (#"getString" meta 8) (#"getString" meta 3)  (#"getString" meta 4) )
	    )
     append
       (loop with meta = (#"getCrossReference" (#"getMetaData" connection) +null+ +null+ target +null+ +null+ table)
	  for next? = (#"next" meta)
	  until (not next?)
	  collect (list (#"getString" meta 7)  (#"getString" meta 8) (#"getString" meta 3)  (#"getString" meta 4) )
	    )))

;; return a list of tables with foreign keys into table
(defun which-tables-reference (connection table)
  (let ((refs (table-references connection table)))
    (remove-duplicates (remove table (mapcar 'car refs) :test 'equal) :test 'equal)))

;; return a list of pairs of tables and commment where the table name matches regex
(defun table-names-matching (connection regex)
  (loop for (table comment) in (table-names connection) when (all-matches table regex 0) collect (list table comment)))

;; return a list of triples of tables, column, and comment on column where the column name matches regex
(defun column-names-matching (connection regex)
  (let ((them nil))
    (maphash 
     (lambda(table columns)
       (let ((matched (loop for (column comment) in columns when (all-matches column regex 0) collect (list table column comment))))
	 (when matched (setq them (append matched them)))))
     (get-all-tables-and-columns connection))
    them))

;; return a list of triples of tables, column, and comment on column where the comment matches regex
(defun columns-with-comment-matching (connection regex)
  (let ((them nil))
    (maphash 
     (lambda(table columns)
       (let ((matched (loop for (column comment) in columns when (all-matches comment regex 0) collect (list table column comment))))
	 (when matched (setq them (append matched them)))))
     (get-all-tables-and-columns connection))
    them))

(defun table-counts (connection tables &optional (threshold 1) &rest fields)
  (loop for table in tables 
     for count = (parse-integer (caar (sql-query (format nil "select count(*) from ~a" table) :connection connection)))
     if (and (plusp count)
	     (<= count threshold)
	     fields)
     collect (list table count (sql-query (format nil "select ~{~a~^,~} from ~a" fields table) :connection connection))
     else collect (list table count)))

(defun describe-table (connection table)
  (let ((table-and-comment (car (table-names-matching connection table))))
    (format t "~a:~%~a~%~{~a:  ~a~^~%~}" 
	    (first table-and-comment)
	    (second table-and-comment)
	    (loop for (column doc) in (table-columns connection table) 
	       collect column collect doc))))

;; http://dev.mysql.com/doc/refman/5.0/en/fulltext-search.html#function_match
