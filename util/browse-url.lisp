(in-package :cl-user)

;;; Open a URL in a browser (works on Linux and osx, not tested elsewhere)

(defvar *browser* #+:os-macosx "Safari" #+(and :os-unix (not :os-macosx)) "Firefox")

(defun browse-url (url &optional (browser *browser*))
  (if (equal browser "mac-default")
      (#"exec" (#"getRuntime" 'java.lang.Runtime) (princ (format nil "open \"~a\"" (if (uri-p url) (uri-full url) url))))
      (if (equal browser "safari")
	  (sys::run-applescript 
	   '("tell application ~s" "Safari")
	   `("open location ~s"  ,(if (uri-p url) (uri-full url) url))
	   "activate"
	   "end tell")
	  (#"exec" (#"getRuntime" 'java.lang.Runtime) (format nil "~A ~A" browser (if (uri-p url) (uri-full url) url))))))

(defvar *doc-base* "http://java.sun.com/javase/6/docs/api/")

;;; ex (document-class 'JFrame)
;; +++ todo: be smart about other packages, like prefuse.
(defun document-class (class-name)
  (browse-url 
   (format nil "~A~A.html"
	   *doc-base*
	   (substitute #\/ #\. (maybe-resolve-class-against-imports class-name)))))
  
  
