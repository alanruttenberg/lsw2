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

(eval-when (:load-toplevel)
  (ensure-jna-loaded))

(defun c-setenv (variable value)
  (ensure-jna-loaded)
  (#"invokeInt" (#"getFunction" (#"getInstance" 'jna.nativelibrary "c") "putenv")
                (java:jnew-array-from-list "java.lang.Object" (list (format nil "~a=~a" variable value)))))

(defun c-getenv (variable)
  (ensure-jna-loaded)
  (let ((buffer (jnew-array "byte" 10000)))
    (let ((found (#"invokePointer" (#"getFunction" (#"getInstance" 'jna.nativelibrary "c") "getenv")
                                   (java:jnew-array-from-list "java.lang.Object" (list variable buffer)))))
      (when found
        (values (#"toString" 'jna.native buffer)
                (#"getString" found  0))))))

(defun c-chdir (dir)
  (ensure-jna-loaded)
  (#"invokePointer" (#"getFunction" (#"getInstance" 'jna.nativelibrary "c") "chdir")
                    (java:jnew-array-from-list "java.lang.Object" (list dir))))

(defun c-getcwd (&optional (maxlength 1000))
  (ensure-jna-loaded)
  (let* ((buffer (jnew-array "byte" maxlength))
         (found 
          (#"invokePointer" (#"getFunction" (#"getInstance" 'jna.nativelibrary "c") "getcwd")
                            (java:jnew-array-from-list "java.lang.Object" (list buffer maxlength)))))
    (assert found () "getcwd failed, presumably because the length of the result string was longer than ~a" maxlength)
    (and found
         (#"toString" 'jna.native buffer))))
