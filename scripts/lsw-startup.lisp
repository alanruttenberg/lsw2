(setf (logical-pathname-translations "lsw")
      `(("**;*.*" ,(make-pathname :directory (append (pathname-directory *load-pathname*)
						     '(:wild-inferiors))
				  :name :wild
				  :type :wild
				  :defaults *load-pathname*))))

(defvar *lsw-ql-directory* 
  (merge-pathnames (make-pathname :directory '(:relative "quicklisp")) (make-pathname :directory (butlast (pathname-directory  *load-pathname*)))))

(if (probe-file (merge-pathnames "setup.lisp" *lsw-ql-directory*))
    (load (merge-pathnames "setup.lisp" *lsw-ql-directory*))
    (progn 
      (format *debug-io* "; Installing quicklisp to ~s~%" *lsw-ql-directory*)
      (load "https://beta.quicklisp.org/quicklisp.lisp")
      (funcall (intern "INSTALL" 'quicklisp-quickstart) :path *lsw-ql-directory*)))

(let ((*load-verbose* nil)
      (*compile-verbose* nil)
      (*suppress-compiler-warnings* (not *load-verbose*)))
  (ql:quickload 'owl2)
  (ql:quickload 'inspect))
  
;  (asdf::oos 'asdf::load-op 'owl2 :verbose nil)
;  (asdf::oos 'asdf::load-op 'inspect :verbose nil))
