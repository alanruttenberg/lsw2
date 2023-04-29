(in-package :cl-user)

;; This provides a way to set an environment variable in the running java process.
;; See https://stackoverflow.com/questions/28158116/call-libc-function-from-jna
;; I've found it useful to be able to set an environment variable some java library that queries for an environment variable 

(defvar *jna-loaded* nil)

(defun ensure-jna-loaded ()
  (or *jna-loaded*
      (progn
        (when (not (ignore-errors (find-java-class 'jna.nativelibrary)))
          (add-to-classpath (ABCL-ASDF:RESOLVE "net.java.dev.jna/jna/LATEST")))
        (setq *jna-loaded* t))))

(defun c-setenv (variable value)
  (ensure-jna-loaded)
  (#"invokeInt" (#"getFunction" (#"getInstance" 'jna.nativelibrary "c") "putenv")
                (java:jnew-array-from-list "java.lang.Object" (list (format nil "~a=~a" variable value)))))

(defun c-getenv (variable)
  (ensure-jna-loaded)
  (let ((found (#"invokePointer" (#"getFunction" (#"getInstance" 'jna.nativelibrary "c") "getenv")
                                  (java:jnew-array-from-list "java.lang.Object" (list variable)))))
    (when found
      (#"getString" found  0))))
