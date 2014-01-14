(require 'asdf)

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

(defun register-all-parallel-sysdefs (directory) 
  "assuming directory in repository folder, find all other systems in ../*/"
  (let ((loaded (loop for k being the hash-keys of ASDF/FIND-SYSTEM:*DEFINED-SYSTEMS* collect k))
	(seen nil)
	(them (ignore-errors 
		(directory (merge-pathnames (make-pathname :directory '(:relative :up :wild) :type "asd" :name :wild) (truename directory))))))
    (loop for path in them 
	 for system-name =  (pathname-name path)
	 do
	 (if (member system-name loaded :test 'equalp) ;; don't add a second time
	     (progn 
	       (push path seen)
	       (warn "asd ~a is already loaded, so not loading ~a"  (pathname-name path) path))
	     (let ((already (find system-name seen :test 'equalp :key 'pathname-name)))
	       (if already
		 (warn "~a is already loaded, so not loading ~a" already path)
		 (progn 
		   (format t "Registering sysdef ~a~%" path)
		   (pushnew (directory-namestring path) asdf::*central-registry* :test 'equalp)
		   (push path seen))))))))

(setf (logical-pathname-translations "lsw")
      `(("**;*.*" ,(make-pathname :directory (append (butlast (pathname-directory *load-pathname*))
						     '(:wild-inferiors))
				  :name :wild
				  :type :wild))))

(register-all-asdf-sysdefs (logical-pathname "lsw:"))
(register-all-parallel-sysdefs (logical-pathname "lsw:"))

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

(let ((*suppress-compiler-warnings* (not *load-verbose*)))
  (asdf::oos 'asdf::load-op 'patches :verbose *load-verbose*))

