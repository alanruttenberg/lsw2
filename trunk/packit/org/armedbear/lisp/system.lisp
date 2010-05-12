(SETF (LOGICAL-PATHNAME-TRANSLATIONS "sys") (QUOTE (("SYS:SRC;**;*.*" "/Users/alanr/repos/abcl1/src/org/armedbear/lisp/**/*.*") ("SYS:JAVA;**;*.*" "/Users/alanr/repos/abcl1/src/org/armedbear/lisp/../../../**/*.*"))))

(in-package :common-lisp-user)

(setq jvm::*suppress-compiler-warnings* t)
(setq *load-verbose* nil)
(require 'asdf)
(setq *inhibit-add-to-classpath* t)
(setq *default-pathname-defaults* (pathname "/"))
(setq asdf::*defined-systems* (make-hash-table :test 'equal))
(setq asdf::*asdf-verbose* nil)
;(setq *debugger-hook* #'system::%debugger-hook-function)


(defun directory-in-jar (pathname)
  (let* ((device (pathname-device pathname))
	 (jarfile (namestring (car device)))
	 (rest-pathname (namestring (make-pathname :directory `(:absolute ,@(cdr (pathname-directory pathname)))
						   :name (pathname-name pathname)
						   :type (pathname-type pathname)))))
    (if (or (position #\* (namestring rest-pathname))
	    (wild-pathname-p rest-pathname))
	(let ((jar (jnew  "java.util.zip.ZipFile" jarfile)))
	  (let ((els (jcall "entries" jar)))
	    (loop while (jcall "hasMoreElements" els)
	       for name = (jcall "getName" (jcall "nextElement" els))
	       when (pathname-match-p (concatenate 'string "/" name) rest-pathname)
	       collect (make-pathname :device (pathname-device pathname)
				      :name (pathname-name name)
				      :type (pathname-type name)
				      :directory `(:absolute ,@(cdr (pathname-directory name)))))))
	(let ((truename (probe-file pathname)))
	  (if truename
              (list truename)
              nil)))))

(defun abcl-module-candidates ()
  (remove-duplicates
   (append
    (loop for v across (jcall "split" (jstatic "getProperty" "java.lang.System" "java.class.path") ":") collect (jcall "toString" v))
    (loop for v across (jcall "split" (jcall "getClassPath" (jstatic "getRuntimeMXBean" '|java.lang.management.ManagementFactory|)) ":") collect (jcall "toString" v))
    (and (boundp '*added-to-classpath*) *added-to-classpath*)
    (let ((module (position "-m" *command-line-argument-list*  :test 'equal)))
      (when module (list (nth (1+ module) *command-line-argument-list*)))))
    :test 'equal))
  

(defvar *loading-jarfile*)

(loop for jar in  (abcl-module-candidates)
   for (candidate) = (directory-in-jar (format nil "jar:file:~a!/abcl-module-*.lisp" (namestring (merge-pathnames jar (format nil "~a/" (jstatic "getProperty" "java.lang.System" "user.dir"))))))
   when candidate
   do (let ((*loading-jarfile* (make-pathname :device (pathname-device candidate))))
	(load candidate)
	))

