(in-package :system)

(if (>= SYSTEM:*FASL-VERSION* 35)
    (progn
;; not needed as of r12389
;; (defmethod print-object ((method standard-method) stream)
;; 	(print-unreadable-object (method stream :identity t)
;; 	  (format stream "~S ~S~{ ~S~} ~S"
;; 		  (class-name (class-of method))
;; 		  (%generic-function-name
;; 		   (%method-generic-function method))
;; 		  (method-qualifiers method)
;; 		  (mapcar #'(lambda (c)
;; 			      (if (typep c 'mop::eql-specializer)
;; 				  `(eql ,(mop::eql-specializer-object c))
;; 				  (if (typep c 'java::java-class)
;; 				      `(java::jclass ,(#"getName" (cl-user::get-java-field c "javaClass" t)))
;; 				      (class-name c))))
;; 			  (%method-specializers method))))
;; 	method)
      (defvar cl-user::*early-advise-forms* nil)
      (push '(:advise java::%find-java-class
	      (or (ignore-errors (:do-it))
	       (if (cl-user::find-java-class (car (:arglist)))
		   (#"findJavaClass" 'org.armedbear.lisp.JavaClass (cl-user::find-java-class (car (:arglist))))
		   (:do-it))) :when :around :name :dynamic-classpath)
	    cl-user::*early-advise-forms*)))
