;; ADDED 19 March 2012
(require 'asdf)

;;;(require :asdf-jar)
;;;;;(require :asdf-install)
;;;;;(require :abcl-asdf)



(in-package :cl-user)

(defun register-all-asdf-sysdefs (directory) 
  "this would be easier if directory **/ worked"
  (loop with q = (system:list-directory directory) 
     while q 
     for top = (pop q)
     if (and (null (pathname-name top))
	     (not (member (car (last (pathname-directory top)))
			  '(".svn" ".webcache")
			  :test #'equal)))
     do (setq q (append q (register-all-asdf-sysdefs top)))
     if (equal (pathname-type top) "asd")
     do (pushnew (namestring directory)  asdf::*central-registry* :test 'equal)))

(setf (logical-pathname-translations "lsw")
      `(("**;*.*" ,(make-pathname :directory (append (butlast (pathname-directory *load-pathname*))
						     '(:wild-inferiors))
				  :name :wild
				  :type :wild))))

(register-all-asdf-sysdefs (logical-pathname "lsw:"))


(defvar *lsw-configuration* nil)

(unless (getf *lsw-configuration* :web-cache)
  (setf (getf *lsw-configuration* :web-cache) 
	(merge-pathnames 
	 (make-pathname :directory
			'(:relative ".webcache"))
	 (truename (logical-pathname "lsw:")))))

;; http://abcl-dev.blogspot.com/2009/09/loading-fasls-from-jar-files.html
;; these shouldn't be here.


(require :abcl-contrib)
(require :jss)
(jss::ensure-compatibility)
;(require :asdf-jar)

(let ((*suppress-compiler-warnings* (not *load-verbose*)))
;  (asdf::oos 'asdf::load-op 'jss :verbose *load-verbose*)
  (asdf::oos 'asdf::load-op 'patches :verbose *load-verbose*))

