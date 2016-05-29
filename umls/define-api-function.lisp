(require 'cl-json) ; must do (ql:quickload "cl-json") once to install

(defvar *umls-username*)
(defvar *umls-password*)
(defvar *last-umls-tgt*)
(defvar *last-umls-ticket* nil)
(defvar *umls-api-cache* (make-hash-table :test 'equalp))

(defun get-umls-api-ticket-granting-ticket (&optional (username *umls-username*) (password *umls-password*))
  "This is like a session ticket. It is used to get a ticket, one of which is needed for each api call"
  (setq *last-umls-tgt* 
	(caar (all-matches 
	       (get-url "https://utslogin.nlm.nih.gov/cas/v1/tickets" 
			:post `(("username" ,username) ("password" ,password)))
	       "(TGT-[^\"]*)" 1))))

(defun get-umls-api-ticket (&optional (ticket-granting-ticket *last-umls-tgt*))
  "Gets a use-once ticket to be used when calling an api function"
  (get-url (format nil "https://utslogin.nlm.nih.gov/cas/v1/tickets/~a" ticket-granting-ticket) 
	   :post `(("service" "http://umlsks.nlm.nih.gov"))))

(defun handle-umls-error-response (condition)
  (let ((cleaned (caar (all-matches (slot-value condition 'message) "(?s)<u></u>.*<u>(.+)</u>" 1))))
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
  (setf (gethash args *umls-api-cache*) values)
  (values-list values))

(defun cached-umls-api-call (args)
  (values-list (gethash args *umls-api-cache*)))

(defun cached

(defmacro define-umls-api-function (function-name path doc &optional extended-doc parameters-doc)
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

The return values are left alone other than that they are parsed from
the json to sexp using cl-json"

  (let ((parameter-names (mapcar 'first (all-matches path "[{]([^}]+)}" 1))))
    (setq parameter-names (remove "version" parameter-names :test 'equal))
    (let* ((args (mapcar 'intern (mapcar 'string-upcase parameter-names)))
	   (function-symbol (intern (string-upcase function-name)))
	   (query-parameters (and parameters-doc (mapcar (lambda(line) (car (split-at-char line #\tab))) (split-at-char parameters-doc #\linefeed))))
	   (query-parameter-syms (mapcar 'intern (mapcar 'string-upcase query-parameters))))
      (let ((doc (format-umls-api-function-documentation doc parameters-doc extended-doc)))
	(let ((method
	       `(defun ,function-symbol (,@args &key ,@query-parameter-syms &aux (path ,path) ticket)
		  ,doc
		  (let ((call-args (list ',function-symbol ,@args ,@query-parameter-syms)))
		    (or (cached-umls-api-call call-args)
			(cache-umls-api-call
			 call-args
			 (multiple-value-list
			  (catch 'catch-umls-error 
			    (handler-bind ((http-error-response 'handle-umls-error-response))
			      (let ((url (format nil "https://uts-ws.nlm.nih.gov/rest~a" path)))
				(setq url (#"replaceAll" url "[{]version[}]" "current"))
				,@(loop
				     for parameter-name in `,parameter-names
				     for arg in `,args
				     collect
				       `(setq url (#"replaceAll" url (format nil "[{]~a[}]" ,parameter-name) ,arg)))
				(setq url (format nil "~a?ticket~a~a" url "=" (get-ticket)))
				(loop for param in ',query-parameters
				   for value in (list ,@query-parameter-syms)
				   when value do (setq url (format nil "~a&~a~a~a" url param "=" (princ-to-string value))))
				(cl-json::decode-json (make-string-input-stream (get-url url)))))))))))))
	  (setf (get function-symbol 'source-code) method)
	  method)))))





