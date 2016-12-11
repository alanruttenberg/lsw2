(in-package :cl-user)

(defun install-jss-condition ()
    (when (and (find-package 'swank-backend)
	       (fboundp (intern "DEFIMPLEMENTATION" 'swank-backend)))
      (eval (read-from-string "(swank-backend::defimplementation swank-backend::PRINT-CONDITION
	     (condition stream)
 (setq @@ nil)
	   (if (and (typep condition 'java-exception)
                    (java-exception-cause condition)
		    '(equal (jclass-name (jobject-class (java-exception-cause condition)))
			   \"jsint.BacktraceException\"))
               (or 
               ;(null '(report-java-exception (intern (jclass-name (jobject-class  (#\"getBaseException\" (java-exception-cause condition)))))
                ;   (#\"getBaseException\" (java-exception-cause condition))))
	       (progn
                (let ((s stream));with-output-to-string (s)
  (setq @@ 'hoppp)
		 (let ((writer (new 'stringwriter)))
		   (setq @@ 'hopppp)
		   (#\"printStackTrace\"  (setq @@ (java-exception-cause condition)) (setq @@ (new 'printwriter writer)))
   ;		   (setq @@ (#\"toString\" writer))
		   (write-string (setq @@ (#\"replaceFirst\" (#\"toString\" writer) \"(?s)\\\\s*at sun.reflect.*\" \"\")) s)
		 ))))
	       (write-string (#\"replaceFirst\" (princ-to-string condition) \"(?s)\\\\s*at jsint.E.*\" \"\") stream)))"))))

;; put the java stack trace in the condition string when using jscheme.
'(eval-when (:compile-toplevel :load-toplevel :execute)
  (if (find-package 'swank-backend )
    (install-jss-condition)
    (eval `(progn
	     (defpackage :swank-backend
	       (:use :common-lisp))
	     (defpackage :swank
	       (:use :common-lisp :swank-backend))
	     (set (intern "*INIT-HOOK*" :swank)
		  (list 'install-jss-condition))))))

(defmethod report-java-exception (class-name exception)
  nil)


