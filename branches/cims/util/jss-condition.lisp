(in-package :cl-user)

(defun install-jss-condition ()
    (when (and (find-package 'swank-backend)
	       (fboundp (intern "DEFIMPLEMENTATION" 'swank-backend)))
      (eval (read-from-string "(swank-backend::defimplementation swank-backend::FORMAT-SLDB-CONDITION
	     (condition)
	   (if (and (typep condition 'java-exception)
                    (java-exception-cause condition)
		    (equal (jclass-name (jobject-class (java-exception-cause condition)))
			   \"jsint.BacktraceException\"))
               (or
               (report-java-exception (intern (jclass-name (jobject-class (#\"getBaseException\" (java-exception-cause condition)))))
                   (#\"getBaseException\" (java-exception-cause condition)))
	       (progn
                (with-output-to-string (s)
		 (let ((writer (new 'stringwriter)))
		   (#\"printStackTrace\" (#\"getBaseException\" (java-exception-cause condition)) (new 'printwriter writer))
		   (write-string (#\"replaceFirst\" (#\"toString\" writer) \"(?s)\\\\s*at sun.reflect.*\" \"\") s))
		 )))
	       (#\"replaceFirst\" (princ-to-string condition) \"(?s)\\\\s*at jsint.E.*\" \"\")))"))))

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


