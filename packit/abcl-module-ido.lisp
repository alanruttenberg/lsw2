(in-package :common-lisp-user)

(setq *PELLET-DIR* "")

(format t "Module ido...~%")

(when (find "-verbose" *command-line-argument-list* :test 'equal)
  (setq *load-verbose* t)
  (setq *compile-verbose* t)
  (setq asdf::*asdf-verbose* t)
  (print *loading-jarfile*)
  (print *load-pathname*))


(defmethod asdf::operation-done-p ((operation asdf::compile-op) (c t))
  (or cl-user::*inhibit-add-to-classpath*
   (call-next-method)))


(let ((asdf::*output-translations* 
       `(((t ,(lambda(path wha) 
		     (let* ((dir (pathname-directory path))
			    (new (append '(:absolute) (list "bin") (cdr dir))))
		       (merge-pathnames path (pathname *loading-jarfile*)))))))))

  (asdf::initialize-source-registry
   `(:source-registry :ignore-inherited-configuration
		      ,@
		      (mapcar
		       (lambda(e) `(:directory ,(make-pathname :directory (pathname-directory e) :device (pathname-device e))))
		       (directory-in-jar (merge-pathnames "/**/*.asd" (pathname *loading-jarfile*))))))
  (asdf::oos 'asdf::load-op 'jss)
  (asdf::oos 'asdf::load-op 'patches)
  (asdf::oos 'asdf::load-op 'util)
  (asdf::oos 'asdf::load-op 'ido)
  )

(when (find "-verbose" *command-line-argument-list* :test 'equal)
  (trace write-external.owl)
  (trace write-pathway.owl))

(let ((check (position "-check" *command-line-argument-list* :test 'equal)))
  (when check
    (check-pathway-spreadsheet-syntax :path (nth (1+ check) *command-line-argument-list*))))

(setq *debugger-hook* #'system::%debugger-hook-function)

(let ((toowl (position "-to-owl" *command-line-argument-list*  :test 'equal)))
  (when toowl
    (let* ((spreadsheet (nth (1+ toowl) *command-line-argument-list*))
	   (dest (nth (+ 2 toowl) *command-line-argument-list*))
	   (book (make-instance 'ido-pathway-book :book-path spreadsheet)))
      (when (not (jcall "matches" dest ".*/")) (setq dest (format nil "~a/" dest)))
      (locate-blocks-in-sheets book (mapcar 'car (block-types book)))
      (parse-book book)
      (write-external.owl book :destdir dest)
      (write-pathway.owl book :destdir dest))))

(unless (find "-no-quit" *command-line-argument-list* :test 'equal)
  (quit))


