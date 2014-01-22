(in-package :system)

;; Save the arglist as otherwise it is only done during loading
;; see http://abcl.org/trac/ticket/343

(defmacro defun (name lambda-list &body body &environment env)
  (note-name-defined name)
  (multiple-value-bind (body decls doc)
      (parse-body body)
    (let* ((block-name (fdefinition-block-name name))
           (lambda-expression
            `(named-lambda ,name ,lambda-list
                           ,@decls
                           ,@(when doc `(,doc))
                           (block ,block-name ,@body))))
      (cond ((and (boundp 'jvm::*file-compilation*)
                  ;; when JVM.lisp isn't loaded yet, this variable isn't bound
                  ;; meaning that we're not trying to compile to a file:
                  ;; Both COMPILE and COMPILE-FILE bind this variable.
                  ;; This function is also triggered by MACROEXPAND, though
                  jvm::*file-compilation*)
             `(progn
                (fset ',name ,lambda-expression)
                ',name))
            (t
             (when (and env (empty-environment-p env))
               (setf env nil))
             (when (null env)
               (setf lambda-expression (precompiler:precompile-form lambda-expression nil)))
             `(prog1
                  (%defun ',name ,lambda-expression)
		;; alanr
		(%set-arglist (symbol-function ',name) ,(format nil "~{~s~^ ~}" (third lambda-expression)))
		;; alanr
                ,@(when doc
                   `((%set-documentation ',name 'function ,doc)))))))))
