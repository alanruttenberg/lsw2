#+abcl

(defun safely (f name)
  (let ((fname (gensym)))
    (compile fname
	     `(lambda(&rest args)
		(with-simple-restart (top-level
				      "Return from lisp method implementation for ~a." ,name)
		  (apply ,f args))))
    (symbol-function fname)))

;(defun safely (f name)
;  (lambda(&rest args)
;    (with-simple-restart (top-level
;			  "Return from lisp method implementation for ~a." name)
;      (apply f args))))

#+abcl
(defun jinterface-safe-implementation (interface &rest method-names-and-defs)
  "Creates and returns an implementation of a Java interface with
   methods calling Lisp closures as given in METHOD-NAMES-AND-DEFS.

   INTERFACE is either a Java interface or a string naming one.

   METHOD-NAMES-AND-DEFS is an alternating list of method names
   (strings) and method definitions (closures).

   For missing methods, a dummy implementation is provided that
   returns nothing or null depending on whether the return type is
   void or not. This is for convenience only, and a warning is issued
   for each undefined method."
  (let ((interface (jclass interface))
        (implemented-methods
         (loop for m in method-names-and-defs
	    for i from 0
	    if (evenp i) 
	    do (assert (stringp m) (m) "Method names must be strings: ~s" m) and collect m
	    else
	    do (assert (or (symbolp m) (functionp m)) (m) "Methods must be function designators: ~s" m))) 
	)
      (let ((safe-method-names-and-defs 
	      (loop for (name function) on method-names-and-defs by #'cddr
		collect name collect (safely  function name))))
	(loop for method across
	      (jclass-methods interface :declared nil :public t)
	      for method-name = (jmethod-name method)
	      when (not (member method-name implemented-methods :test #'string=))
	      do
	      (let* ((void-p (string= (jclass-name (jmethod-return-type method)) "void"))
		    (arglist (when (plusp (length (jmethod-params method))) '(&rest ignore)))
		    (def `(lambda
			      ,arglist
			    ,(when arglist '(declare (ignore ignore)))
			    ,(if void-p '(values) +null+))))
	       (push (coerce def 'function) safe-method-names-and-defs)
	       (push method-name safe-method-names-and-defs)
	       ))
	(apply #'java::%jnew-proxy interface  safe-method-names-and-defs))))
