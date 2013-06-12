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

(defun with-jdbc-connection (fn jdbc-url)
  (let ((connection nil))
    (unwind-protect
	 (progn
	   (funcall fn (#"getConnection" 'java.sql.DriverManager jdbc-url)))
      (and connection (#"close" connection))
      )))

;; Do a sql query to connection. Result is a list of list, with each list one row of fields.
;; if with-headers is non-nil, then the values are instead ("fieldname" . value) instead of just value.

(defun sql-query (query connection &optional with-headers)
  (cond ((or (equal "com.microsoft.sqlserver.jdbc.SQLServerConnection" (jclass-name (jobject-class connection)))
	     (equal "oracle.jdbc.driver.T4CConnection" (jclass-name (jobject-class connection))))
	 (let (statement results)
	   (unwind-protect 
		(progn
		  (setq statement (#"createStatement" connection))
		  (setq results (#"executeQuery" statement query))
		  (loop while (#"next" results) 
		     with headers 
		     collect (loop for column from 1 to (#"getColumnCount" (#"getMetaData" results))
				when with-headers
				do (unless headers (setq headers (make-array (#"getColumnCount" (#"getMetaData" results)))))
				and collect (cons (or (svref headers (1- column))
						      (setf (svref headers (1- column))
							    (#"getColumnName" (#"getMetaData" results) column)))
					      (#"getString" results column))
				unless with-headers collect (#"getString" results column))))
	     (and (boundp 'results) results (#"close" results))
	     (and (boundp 'statement) statement (#"close" statement)))))
	(t (error "Don't yet support sql-query for ~a" (jclass-name (jobject-class connection))))))

(defun sql-server-driver-properties ()
  (map 'list (lambda(e)
	       (cons (get-java-field e "name" t)
		     (get-java-field e "description" t)))
       (get-java-field 'SQLServerDriver "DRIVER_PROPERTIES"t)))

;;(add-to-classpath "/Volumes/Big/Downloads/2011/2011-01-13/sqljdbc_3.0/enu/sqljdbc4.jar")
;;(add-to-classpath "/Volumes/Big/Downloads/2013-04-12/sqljdbc_4.0/enu/sqljdbc4.jar")
;; sqljdbc4.jar is in lib/
;; not enough in java 7 - put sqljdbc4.jar in same dir as abcl.jar

