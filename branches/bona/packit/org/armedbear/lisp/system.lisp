(SETF (LOGICAL-PATHNAME-TRANSLATIONS "sys") (QUOTE (("SYS:SRC;**;*.*" "/Users/alanr/repos/abcl1/src/org/armedbear/lisp/**/*.*") ("SYS:JAVA;**;*.*" "/Users/alanr/repos/abcl1/src/org/armedbear/lisp/../../../**/*.*"))))

(in-package :common-lisp-user)

(setq jvm::*suppress-compiler-warnings* t)
(setq *load-verbose* nil)
(require 'asdf)
(setq *inhibit-add-to-classpath* t)
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

(loop for jar in  (abcl-module-candidates)
   for (candidate) = (directory (format nil "jar:file:~a!/abcl-module-*.lisp" (namestring (merge-pathnames jar (format nil "~a/" (jstatic "getProperty" "java.lang.System" "user.dir"))))))
   when candidate
   do (let ((*loading-jarfile* (make-pathname :device (pathname-device candidate))))
	(load candidate)
	))

