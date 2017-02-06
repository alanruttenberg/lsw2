(setf (logical-pathname-translations "lsw")
      `(("**;*.*" ,(make-pathname :directory (append (pathname-directory *load-pathname*)
						     '(:wild-inferiors))
				  :name :wild
				  :type :wild
				  :defaults *load-pathname*))))

(require :quicklisp-abcl)

(ql:quickload 'owl2)
(ql:quickload 'inspect)
  

