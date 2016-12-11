(in-package :cl-user)

;;; Open a URL in a browser (works on Linux and osx, not tested elsewhere)

(defvar *browser* "firefox")

(defun list->jarray (list)
  (let* ((wid (length list))
	 (array (jnew-array "java.lang.String" wid))
	 (i 0))
    (loop for count from 0
	 for elt in list
	 do (setf (jarray-ref array i) elt)
	 (incf i))
    array))

(defun browse-url (url &optional (browser *browser*))
  (if (equal browser "mac-default")
      (#"exec" (#"getRuntime" 'java.lang.Runtime) (princ (format nil "open \"~a\"" url)))
      (if (equal browser "safari")
	  ;; this hack because the 'open' command doesn't pass the
	  ;; query parameters of its url argument. Then on top of that
	  ;; the java exec program doesn't respect spaces in quotes
	  ;; and goes ahead and uses space a delimeter regardless,
	  ;; thus mangling the comment. So we have to package this
	  ;; into an array
	  (#"exec" (#"getRuntime" 'java.lang.Runtime)
		   (list->jarray
		    (list
		    "/usr/bin/osascript"
		    "-e"
		    "tell application \"Safari\""
		    "-e"
		    (format nil "open location ~s"  url)
		    "-e"
		    "activate"
		    "-e"
		    "end tell")))
	  (#"exec" (#"getRuntime" 'java.lang.Runtime) (format nil "~A ~A" browser url)))))

(defvar *doc-base* "http://java.sun.com/javase/6/docs/api/")

;;; ex (document-class 'JFrame)
;; +++ todo: be smart about other packages, like prefuse.
(defun document-class (class-name)
  (browse-url 
   (format nil "~A~A.html"
	   *doc-base*
	   (substitute #\/ #\. (maybe-resolve-class-against-imports class-name)))))
  
  
