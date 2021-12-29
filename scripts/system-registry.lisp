(in-package :cl-user)
(require 'asdf)
(require :abcl-contrib)
(require :abcl-asdf)
(require :jss)
(jss::ensure-compatibility)

(setf (logical-pathname-translations "lsw")
      `(("**;*.*" ,(make-pathname :directory (append (butlast (pathname-directory *load-pathname*))
						     '(:wild-inferiors))
				  :name :wild
				  :type :wild))))

(defvar *lsw-root* (make-pathname  :directory (butlast (pathname-directory *load-pathname*))))


(defvar *lsw-configuration* nil)

(unless (getf *lsw-configuration* :web-cache)
  (setf (getf *lsw-configuration* :web-cache) 
	(asdf:system-relative-pathname 'lsw2 ".webcache")
	))

;; http://abcl-dev.blogspot.com/2009/09/loading-fasls-from-jar-files.html
;; these shouldn't be here.


(let ((*suppress-compiler-warnings* (not *load-verbose*)))
  (asdf::oos 'asdf::load-op 'patches :verbose *load-verbose*))

