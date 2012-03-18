(SETF (LOGICAL-PATHNAME-TRANSLATIONS "sys") (QUOTE (("SYS:SRC;**;*.*" "/Users/alanr/repos/abcl1/src/org/armedbear/lisp/**/*.*") ("SYS:JAVA;**;*.*" "/Users/alanr/repos/abcl1/src/org/armedbear/lisp/../../../**/*.*"))))

(in-package :common-lisp-user)

(setq *load-verbose* nil)
(require 'asdf)
(setq *default-pathname-defaults* (pathname "/"))
(setq asdf::*defined-systems* (make-hash-table :test 'equal))
(setq asdf::*asdf-verbose* nil)

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

(defun module-load-systems (systems)
  (let ((*inhibit-add-to-classpath* t)
	(jvm::*suppress-compiler-warnings* t)
	(asdf::*output-translations* 
	 `(((t ,(lambda(path wha) 
		       (let* ((dir (pathname-directory path))
			      (new (append '(:absolute) (list "bin") (cdr dir))))
			 (merge-pathnames path (pathname *loading-jarfile*)))))))))
    (declare (special *inhibit-add-to-classpath*))
    (asdf::initialize-source-registry
     `(:source-registry :ignore-inherited-configuration (:tree ,(pathname *loading-jarfile*))))
    (loop for s in systems do (asdf::oos 'asdf::load-op s))))

(defun current-directory ()
  (format nil "~a/" (jstatic "getProperty" "java.lang.System" "user.dir")))

(defun cmdl-named-arg (name &optional switch)
  (let ((pos (position name *command-line-argument-list*  :test 'equal)))
    (if (and switch pos)
	(values t pos)
	(when pos (values (nth (1+ pos) *command-line-argument-list*) pos)))))

(when (cmdl-named-arg "-verbose" t)
  (setq *load-verbose* t)
  (setq *compile-verbose* t)
  (setq asdf::*asdf-verbose* t))

(loop for jar in  (abcl-module-candidates)
   for (candidate) = (directory (format nil "jar:file:~a!/abcl-module-*.lisp" (namestring (merge-pathnames jar (format nil "~a/" (jstatic "getProperty" "java.lang.System" "user.dir"))))))
   when candidate
   do (let ((*loading-jarfile* (make-pathname :device (pathname-device candidate))))
	(when (cmdl-named-arg "-verbose" t)
	  (format t "Loading jarfile: ~a...~%" *loading-jarfile*)
	  (format t "Loading pathname: ~a...~%" candidate))
	(load candidate)
	))

