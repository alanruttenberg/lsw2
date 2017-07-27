(in-package :cl-user)

(defvar *prowl-api-key* nil)

(defun prowl-notify (title note &key (app "LSW") )
  (get-url "https://api.prowlapp.com/publicapi/add"
	   :post `(("apikey" ,*prowl-api-key*) ("application" ,app) ("event" ,title) ("description" ,note))))
