;;;; -*- Mode: LISP -*-
;;;;

(in-package :asdf)

(setf (logical-pathname-translations "graph-dag")
      `(
	("**;*.*" ,(make-pathname :directory (append (pathname-directory *load-pathname*) '(:wild-inferiors))
				    :name :wild
				    :type :wild))
	))

(defsystem :graph-dag
  :name "Directed acyclic graph visualization"
  :licence "BSD"
  :components
  ((:module api
	    :pathname ""
	    :components
	    ((:file "dag-view"))
	    :depends-on ())))

;;;; eof
