(in-package :cl-user)

;;; Open a URL in a browser (works on Linux, not tested elsewhere)

(defvar *browser* "firefox")

(defun browse-url (url)
  (#"exec" (#"getRuntime" 'java.lang.Runtime) (format nil "~A ~A" *browser* url)))

(defvar *doc-base* "http://java.sun.com/javase/6/docs/api/")

;;; ex (document-class 'JFrame)
;; +++ todo: be smart about other packages, like prefuse.
(defun document-class (class-name)
  (browse-url 
   (format nil "~A~A.html"
	   *doc-base*
	   (substitute #\/ #\. (maybe-resolve-class-against-imports class-name)))))
  
  
