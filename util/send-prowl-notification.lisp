(in-package :cl-user)

(defvar *prowl-api-key* nil)
(defvar *prowl-use-http* nil)

(defun prowl-notify (title note &key (priority 3) (app "LSW") )
  (if *prowl-api-key*
      (get-url (format nil "~a://api.prowlapp.com/publicapi/add" (if *prowl-use-http* "http" "https"))
	       :post `(("apikey" ,*prowl-api-key*) ("application" ,app) ("event" ,title) ("description" ,note) ("priority" ,(prin1-to-string (- priority 3)))))
      (format t "Notification from ~a: ~a - ~a ~%" app title note)))
