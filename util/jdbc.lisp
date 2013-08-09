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

(defun with-jdbc-connection (fn jdbc-url)
  (let ((connection nil))
    (unwind-protect
	 (progn
	   (funcall fn (#"getConnection" 'java.sql.DriverManager jdbc-url)))
      (and connection (#"close" connection))
      )))

(defgeneric with-jdbc-connection-named (keyword function)
  (:documentation "if a sql query is executed with a connection that is a keyword instead of a connection object, the sql is wrapped in a lambda and passed to this function, which calls the lambda with a single argument which is the connection. Clients of this functionality need to define eql methods on the first argument to implement the necessary setup of the connection"
  ))

;; Do a sql query to connection. Result is a list of list, with each list one row of fields.
;; if with-headers is non-nil, then the values are instead ("fieldname" . value) instead of just value.

(defun sql-query (query &optional (connection *default-connection*) &key with-headers print)
  (if (keywordp connection)
      (with-jdbc-connection-named connection
	(lambda(c) (sql-query query c :with-headers with-headers :print print)))
      (if (and (null connection) (not (null *default-connection*)))
	  (sql-query query *default-connection* :with-headers with-headers :print print)
	  (cond ((or (equal "com.microsoft.sqlserver.jdbc.SQLServerConnection" (jclass-name (jobject-class connection)))
		     (equal "oracle.jdbc.driver.T4CConnection" (jclass-name (jobject-class connection))))
		 (let (statement results)
		   (unwind-protect 
			(progn
			  (setq statement (#"createStatement" connection))
			  (setq results (#"executeQuery" statement (if (consp query) (apply 'format nil (car query) (cdr query)) query )))
			  (when print 
			    (format t "~{~a~^	~}~%" (loop for i from 1 to (#"getColumnCount" (#"getMetaData" results)) collect (#"getColumnName" (#"getMetaData" results) i))))
			  (loop while (#"next" results) 
			     with headers
			     collect (block columns (loop for column from 1 to (#"getColumnCount" (#"getMetaData" results))
						       when with-headers
						       do (unless headers (setq headers (make-array (#"getColumnCount" (#"getMetaData" results)))))
						       and collect (cons (or (svref headers (1- column))
									     (setf (svref headers (1- column))
										   (#"getColumnName" (#"getMetaData" results) column)))
									 (print (#"getString" results column)))
						       unless with-headers collect (#"getString" results column) into columns
						       finally (if print (format t "~{~s~^	~}~%" columns) (return-from columns columns))))
			     into rows
			     finally (if print nil (return-from nil rows))))
		     (and (boundp 'results) results (#"close" results))
		     (and (boundp 'statement) statement (#"close" statement)))
		   ))
		(t (error "Don't yet support sql-query for ~a" (jclass-name (jobject-class connection))))))))

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
  (mapcar 'car (car (sql-query (list "select top 1 * from ~a" table) connection :with-headers t))))

(defun sample-of-rows (table connection &optional howmany)
  (let ((primary-key (dbdesc-table-primary-key allscripts-dbdesc table)))
    (sql-query (list "select  * from ~a where ~a in (select top ~a ~a from ~a order by newid())" 
		     table primary-key (or howmany 5) primary-key table) connection :print t)))

(defun table-rowcount (table connection)
  "Return the number or rows in a table"
  (caar (sql-query (list "select count(*) from ~a" table) connection)))

(defun sql-server-columns-matching (connection &key (table-match "%")  schema (column-match "%"))
  "Retrieve table,schema,column for all tables. Constraint tables to match table-match and column to match column-match"
  (sql-query (list "SELECT t.name AS table_name, SCHEMA_NAME(schema_id) AS schema_name, c.name AS column_name FROM sys.tables AS t
INNER JOIN sys.columns c ON t.OBJECT_ID = c.OBJECT_ID
where t.name like '~a' and c.name like '~a'
ORDER BY schema_name, table_name;" table-match  column-match) connection))
