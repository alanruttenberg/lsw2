(setf (logical-pathname-translations "lsw")
      `(("**;*.*" ,(make-pathname :directory (append (pathname-directory *load-pathname*)
						     '(:wild-inferiors))
				  :name :wild
				  :type :wild
				  :defaults *load-pathname*))))

(require :quicklisp-abcl)

(unless (member 'ql:quickload sys::*module-provider-functions*)
  (setq sys::*module-provider-functions*  (append sys::*module-provider-functions*  '(ql:quickload))))

(ql:quickload 'owl2)
(ql:quickload 'inspect)
  

