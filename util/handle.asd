;;;; -*- Mode: LISP -*-

(in-package :asdf)

(setf (logical-pathname-translations "handle")
      `(("**;*.*" ,(make-pathname :directory (append (pathname-directory *load-pathname*)
						     '(:wild-inferiors))
				  :name :wild
				  :type :wild))))

(defsystem :handle
  :name "handle.net api"
  :author "Alan Ruttenberg"
  :components
  ((:mvn "org.dspace/handle")
   (:file "hdl"))
  )

;;;; eof
