(in-package :cl-user)

(require 'cl-json) ; must do (ql:quickload "cl-json") once to install

;;; YOU, yes YOU, need to set up the user and password
(defvar *umls-username*)
(defvar *umls-password*)

(defvar *umls-max-results-per-call* 100)
(defvar *last-umls-tgt* nil)
(defvar *last-umls-ticket* nil)
(defvar *umls-api-cache* (make-hash-table :test 'equalp))
(defvar *umls-api-cache-enabled* t)
(defvar *interned-umls-strings* (make-hash-table :test 'equal))
(defvar *current-umls-api-version* "current")

(defun get-umls-api-ticket-granting-ticket (&optional (username *umls-username*) (password *umls-password*))
  "This is like a session ticket. It is used to get a ticket, one of which is needed for each api call"
  (assert (and *umls-password* *umls-username*) (*umls-password* *umls-username*)
	  "*umls-username* and *umls-password* must first be set")
  (setq *last-umls-tgt* 
	(caar (all-matches 
	       (get-url "https://utslogin.nlm.nih.gov/cas/v1/tickets" 
			:post `(("username" ,username) ("password" ,password)))
	       "(TGT-[^\"]*)" 1))))

(defun get-umls-api-ticket (&optional (ticket-granting-ticket *last-umls-tgt*))
  "Gets a use-once ticket to be used when calling an api function"
  (catch 'catch-ticket-error 
    (handler-bind ((http-error-response 'handle-umls-ticket-error))
      (get-url (format nil "https://utslogin.nlm.nih.gov/cas/v1/tickets/~a" ticket-granting-ticket) 
	   :post `(("service" "http://umlsks.nlm.nih.gov"))))))


(defun handle-umls-ticket-error (condition)
  (let ((cleaned (caar (all-matches (slot-value condition 'message) "(?s)<h3>(.+)</h3>" 1))))
    (if (equal cleaned "TicketGrantingTicket could not be found.")
	(progn
	  (format *debug-io* "Getting new UMLS ticket granting ticket")
	  (get-umls-api-ticket-granting-ticket)))
    (throw 'catch-ticket-error (get-umls-api-ticket))))


(defun handle-umls-error-response (condition)
  (let ((cleaned (or (caar (all-matches (slot-value condition 'message) "(?s)<u></u>.*<u>(.+)</u>" 1))
		     (caar (all-matches (slot-value condition 'message) "(?s)<h3>(.+)</h3>" 1)))))
    (throw 'catch-umls-error (values nil cleaned (slot-value condition 'response-code)))))
 
(defun format-umls-api-function-documentation (doc parameters-doc extended-doc)
  (with-output-to-string (s)
    (format s "~a~%~%" doc)
    (if (not extended-doc)
	""
	(format s "~a~&~&" extended-doc))
    (if (not parameters-doc)
	""
	(progn
	  (format s "Parameters:~%~%")
	  (loop for parameter-desc in (split-at-char parameters-doc #\linefeed)
	     for (name required? description valid Default Note) = (split-at-char parameter-desc #\tab)
	     do (format s "~a(~a): ~a ~a~a~a~%~%"
			name 
			(if (equal required? "Y") "required" "optional")
			description
			(if (equal valid "n/a") "" (format nil "Valid values: ~a. " valid))
			(if (equal default "n/a") "" (format nil "Default: ~a. " default))
			(if (equal note "n/a") "" (format nil "Note: ~a. " note))))
	  s))))


(defun cache-umls-api-call (args values)
  (when *umls-api-cache-enabled*
    (setf (gethash (intern-umls-strings args :call) *umls-api-cache*) (intern-umls-strings values :results)))
  (values-list values))

(defun cached-umls-api-call (args)
  (and *umls-api-cache-enabled* (values-list (gethash args *umls-api-cache*))))

(defun is-umls-api-call-cached (args)
  (and *umls-api-cache-enabled* (gethash args *umls-api-cache*)))

(defun persist-umls-api-call-cache (&optional (path "~/Desktop/umls-api-cache.lisp"))
  (let ((c *umls-api-cache*))
    (ensure-directories-exist path)
    (if (probe-file path)
	(rename-file path (concatenate-string "backup-of-" (namestring (truename path)))))
    (with-open-file (f path :if-does-not-exist :create :if-exists :supersede :direction :output)
      (format f "(*umls-api-cache* :test ~a :size ~a :count ~a)~%" (hash-table-test c) (hash-table-size c) (hash-table-count c))
      (maphash (lambda(k v) (format f "~s~%~s~%;;;~%" k v)) *umls-api-cache*)
      )))

(defun restore-umls-api-call-cache (&optional (path "~/Desktop/umls-api-cache.lisp"))
  (with-open-file (f path :direction :input)
    (let ((spec (read f)))
      (destructuring-bind (token &key test size count) spec
	(print-db (setq @ token) test size count)
	(if (not (and (eq token '*umls-api-cache*) (member test '(eq equalp eql)) (integerp size) (integerp count)))
	    (error "not a umls api call cache. Spec is ~a" spec)
	    (let ((table (make-hash-table :test test :size size)))
	      (loop for count from 0
		 for key = (read f nil :eof)
		 for value = (read f nil :eof)
		 until (eq key :eof)
		 do
		   (intern-umls-strings key :call)
		   (intern-umls-strings value :results)
		   (setf (gethash key table) value)
		   (when (zerop (mod (incf count) 1000)) (princ ".")))
	      (assert (= (hash-table-count table) count) (table count) "Difference in number of entries saved and read")
	      (setq *umls-api-cache* table)))))))

(defun intern-umls-strings (thing type)
  (declare (ignore type))
  (flet ((intern-string (string)
	   (or (gethash string *interned-umls-strings*)
	       (setf (gethash string *interned-umls-strings*) string))))

    (tree-nsubst-if (lambda(e) (intern-string e)) 'stringp thing))) 
		    
    ;; (ecase type
    ;;   (:call (loop for arg in thing for pos from 0 if (stringp arg) do (setf (nth pos thing) (intern-string arg))))
    ;;   (:results 
    ;;    (flet ((maybe-intern-cdr (pair)
    ;; 		(if (stringp (cdr pair))
    ;; 		    (setf (cdr pair) (intern-string (cdr pair))))))
    ;; 	 (loop for result in (car thing) ;; each result an alist
    ;; 	       unless (eq (car result) :class-type)
    ;; 	    do
    ;; 	      (map nil 
    ;; 		   (lambda (el)
    ;; 		     (if (eq (car el) :semantic-types)
    ;; 			 (map nil #'maybe-intern-cdr (cdr el))
    ;; 			 (maybe-intern-cdr el)))
    ;; 		   result)))))
    ;; thing))


(defmacro define-umls-api-function (function-name path doc &optional extended-doc parameters-doc &key one-result-only use-http )
  "Defines a function to do a UMLS REST API call.  You need to call
get-umls-api-ticket-granting-ticket once every 8 hours, but otherwise
tickets (for authentication) are retrieved as needed.  Arguments are
cut/paste with optional editing from the documentation on and linked
from https://documentation.uts.nlm.nih.gov/rest/home.html

name: A name for the function

path: The path as listed https://documentation.uts.nlm.nih.gov/rest/home.html . 
Components in curly braces become arguments to the function

doc: The short doc string on https://documentation.uts.nlm.nih.gov/rest/home.html 

extended-doc: The first section from the documentation linked from the
home page

parameters-doc: The query parameters table from the documentaiton
linked from the home page, except for the 'ticket' parameter line,
as those are managed behind the scenes.  query parameters become
keyword arguments to the function.

One doesn't need (shouldn't use) the pagenumber and pagesize parameters.
They are a control on the number of results in a given call. The code 
will request *umls-max-results-per-call* per call and make multiple calls
to retrieve all results, if necessary.

The return values are left alone other than that they are parsed from
the json to sexp using cl-json and, for cases where there are multiple
results, the list of results is returned directly"
  (let ((parameter-names (mapcar 'first (all-matches path "[{]([^}]+)}" 1))))
    (setq parameter-names (remove "version" parameter-names :test 'equal))
    (let* ((args (mapcar 'intern (mapcar 'string-upcase parameter-names)))
	   (function-symbol (intern (string-upcase function-name)))
	   (query-parameters (and parameters-doc (mapcar (lambda(line) (car (split-at-char line #\tab))) (split-at-char parameters-doc #\linefeed))))
	   (query-parameter-syms (mapcar 'intern (mapcar 'string-upcase query-parameters))))
      (let ((doc (format-umls-api-function-documentation doc parameters-doc extended-doc)))
	(let ((method
	       `(defun ,function-symbol (,@args &key ,@query-parameter-syms probe ignore-errors &aux (path ,path))
		  ,doc
		  (unless *last-umls-tgt* (get-umls-api-ticket-granting-ticket))
		  ,@(when (member 'pagesize query-parameter-syms) 
			  `((setq pagesize *umls-max-results-per-call*)))
		  (let ((call-args (list ',function-symbol ,@args ,@query-parameter-syms)))
		    (if (is-umls-api-call-cached call-args)
			(cached-umls-api-call call-args)
			(unless probe
			  (cache-umls-api-call
			   call-args
			   (multiple-value-list
			    (catch 'catch-umls-error 
			      (handler-bind ((http-error-response 'handle-umls-error-response))
				(let ((url (format nil "http~a://uts-ws.nlm.nih.gov/rest~a" (if ,use-http "" "s")  path)))
				  (setq url (#"replaceAll" url "[{]version[}]" *current-umls-api-version*))
				  ,@(loop
				       for parameter-name in `,parameter-names
				       for arg in `,args
				       collect
					 `(setq url (#"replaceAll" url (format nil "[{]~a[}]" ,parameter-name) ,(print-db arg))))
				  (let ((page-of-results nil)
					(result-pages nil))
				    (when (member 'pagesize ',query-parameter-syms)
				      (setq pagenumber (or pagenumber 1)))
				    (loop for page-url = (format nil "~a?ticket~a~a" url "=" (get-umls-api-ticket))
				       do
					 (loop
					    for param in ',query-parameters
					    for value in (list ,@query-parameter-syms)
					    when value
					    do (setq page-url (format nil "~a&~a~a~a" page-url param "=" (#"encode" 'java.net.URLEncoder
							     (princ-to-string value) "UTF-8"))))
					 (setq page-of-results 
					       (if ignore-errors 
						   (ignore-errors (cl-json::decode-json (make-string-input-stream (get-url page-url :ignore-errors t))))
						   (cl-json::decode-json (make-string-input-stream (get-url page-url :ignore-errors nil)))))
					 (when page-of-results (push page-of-results result-pages))
				       until
					 ,(if (member 'pagesize query-parameter-syms)
					      '(prog1
						(>= pagenumber (or (cdr (assoc :page-count page-of-results)) 0))
						(incf pagenumber))
					      t))
				    (merge-result-pages result-pages ,one-result-only)
				    ))))))))))))
	  (setf (get function-symbol 'source-code) method)
	  method)))))

;; there's always something. Every function but concept-info returns a
;; list of list of pairs. That one returns a list of pairs. Sheesh.

(defun merge-result-pages (pages one-result-only)
  (apply 'concatenate 'list (mapcar (lambda(r)
				      (let ((result (cdr (assoc :result r))))
					(if one-result-only (list result) result)))
				    (reverse pages))))

(defun flush-umls-api-cache (&optional api-function)
  (if api-function
      (loop for key in (alexandria::hash-table-keys *umls-api-cache*)
	    when (eq (car key) api-function)
	      do (remhash key *umls-api-cache*))
      (setq *umls-api-cache* (make-hash-table :test 'equalp))))
  



