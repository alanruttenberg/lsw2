(in-package :cl-user)

(defvar *prowl-api-key* nil)

(defun prowl-notify (title note &key (priority 3) (app "LSW") )
  (if *prowl-api-key*
      (get-url "https://api.prowlapp.com/publicapi/add"
	       :post `(("apikey" ,*prowl-api-key*) ("application" ,app) ("event" ,title) ("description" ,note) ("priority" ,(prin1-to-string (- priority 3)))))
      (format t "Notification from ~a: ~a - ~a ~%" app title note)))
