(in-package :system)

(defparameter +ctx+
  (find "ctx" (#"getDeclaredFields" (java::jclass "org.armedbear.lisp.CompiledClosure")) :key #"getName" :test 'equal))

(defparameter +value+
  (find "value" (#"getDeclaredFields" (java::jclass "org.armedbear.lisp.ClosureBinding")) :key #"getName" :test 'equal))

(defun function-internal-fields (f)
  (if (symbolp f) 
      (setq f (symbol-function f)))
  ;; think about other fields
  (let ((fields (#"getDeclaredFields" (#"getClass" f))))
    (loop for field across fields
	  do (#"setAccessible" field t)
	  collect
	  (#"get" field f))))

(defun function-internals (f)
  (append (function-internal-fields f)
	  (and (#"isInstance" (java::jclass "org.armedbear.lisp.CompiledClosure") f)
	       (compiled-closure-context f))))

(defun compiled-closure-context (f)
  (let ((context (#"get" +ctx+ f)))
    (loop for binding across context
	  collect 
	  (#"get" +value+ binding))))
   
(defun foreach-internal-field (fn-fn not-fn-fn &optional (fns :all) (definer nil))
  "fn-n gets called with top, internal function, not-fn-fn gets called with top anything-but"
  (declare (optimize (speed 3) (safety 0)))
  ;; 10 times faster inlining and using with-constant-signature
  (jss::with-constant-signature ((fields "getDeclaredFields") (get "get") (access "setAccessible") (getclass "getClass") (name "getLambdaName"))
    (labels ((function-internal-fields (f)
	       (if (symbolp f) 
		   (setq f (symbol-function f)))
	       (let ((fields (fields (getclass f))))
		 (loop for field across fields
		       do (access field t)
		       collect
		       (get field f))))
	     (check (f top seen)
	       (declare (optimize (speed 3) (safety 0)))
	       (dolist (el (function-internal-fields f))
		 (if (functionp el)
		     (let ((name? (name el)))
		       (if (or (consp name?) (and name? (fboundp name?) (eq el (symbol-function name?))) )
			   (progn
			     (when not-fn-fn (funcall not-fn-fn top name?))
			     (when (not (member el seen :test #'eq))
			       (push el seen)
			       (check el top seen)))
			   (when (not (member el seen :test #'eq))
			     (when fn-fn (funcall fn-fn top el))
			     (push el seen)
			     (check el top seen))))
		     (when not-fn-fn 
		       (funcall not-fn-fn top el)
		       )))))
      (if (eq fns :all)
	  (progn
	    (dolist (p (list-all-packages))
	    (do-symbols (s p)
	      (when (fboundp s)
		(check (symbol-function s) s nil))))
	    (each-non-symbol-compiled-function (lambda (definer f) (check f definer nil))))
	  (dolist (f fns) 
	    (check (if (not (symbolp f)) f  (symbol-function f)) (or definer f) nil))
	  ))))

(defun callers (thing &aux them)
  (foreach-internal-field
   nil
   (lambda(top el)
     (when (equal el thing)
       (pushnew top them)
       )))
  them)

	      
(defun annotate-internal-functions (&optional (fns :all) definer)
  (foreach-internal-field 
   (lambda(top internal)
     (unless (eq (if (symbolp top) (symbol-function top) top) internal)
       (setf (getf (function-plist internal) :internal-to-function) (or definer top))
       ))
   nil
   fns
   definer))


(defun annotate-clos-methods (&optional (which :all))
  (flet ((annotate (method)
	   (let ((method-function (mop::std-method-function method))
		 (fast-function  (mop::std-method-fast-function method)))
	     (when (and method-function (compiled-function-p method-function)) 
	       (setf (getf (function-plist method-function) :method-function) method)
	       (annotate-internal-functions (list method-function) method))
	     (when (and fast-function (compiled-function-p fast-function))
	       (setf (getf (function-plist fast-function) :method-fast-function) method)
	       (annotate-internal-functions (list fast-function) method)))))
      (if (eq which :all)
	  (loop for q = (list (find-class t)) then q
		for focus = (pop q)
		while focus
		do (setq q (append q (mop::class-direct-subclasses  focus)))
		   (loop for method in (mop::class-direct-methods focus)
			 do (annotate method)))

	  (dolist (f which)
	    (annotate f)
	    ))))

(defun annotate-clos-slots (&optional (which :all))
  (flet ((annotate (slot)
	   (let ((initfunction (and (slot-boundp slot 'initfunction)
				    (slot-value slot 'initfunction))))
	     (when initfunction
	       (setf (getf (function-plist initfunction) :initfunction) slot)
	       (annotate-internal-functions (list initfunction) slot)))))
    (if (eq which :all)
	(loop for q = (list (find-class t)) then q
	      for focus = (pop q)
	      while focus
	      do (setq q (append q (mop::class-direct-subclasses  focus)))
		 (loop for slot in (mop::class-direct-slots focus)
		       do (annotate slot)))
	(dolist (f which)
	  (annotate f)
	  ))))

(defun method-spec-list (method)
  `(,(mop::generic-function-name  (mop::method-generic-function method))
    ,(mop::method-qualifiers method) 
    ,(mapcar #'(lambda (c)
		 (if (typep c 'mop:eql-specializer)
		     `(eql ,(mop:eql-specializer-object c))
		     (class-name c)))
	     (mop:method-specializers method))))

(defun function-name-by-where-loaded-from (function)
  (let* ((class (#"getClass" function))
	 (loaded-from (sys::get-loaded-from function))
	 (name (#"replace" (#"getName" class) "org.armedbear.lisp." ""))
	 (where (and loaded-from (concatenate 'string (pathname-name loaded-from) "." (pathname-type loaded-from)))))
    `(:anonymous-function ,name ,@(if (sys::arglist function)  (sys::arglist function)) 
			  ,@(if where (list (list :from where))))))
  
(defun any-function-name (function &aux it)
  (let ((plist (sys::function-plist function)))
    (cond ((setq it (getf plist :internal-to-function))
	   `(:local-function ,@(if (#"getLambdaName" function) (list (#"getLambdaName" function)))
			     :in ,@(if (typep it 'mop::standard-method)
				       (cons :method (method-spec-list it))
				       (list it))))
	  ((setq it (getf plist :method-function))
	   (cons :method-function (sys::method-spec-list it)))	   
	  ((setq it (getf plist :method-fast-function))
	   (cons :method-fast-function (sys::method-spec-list it)))
	  ((setq it (getf plist :initfunction))
	   (let ((class (and (slot-boundp it 'allocation-class) (slot-value it 'allocation-class))))
	     (list :slot-initfunction (slot-value it 'name ) :for (if class (class-name class) '??))))
	  (t (or (nth-value 2 (function-lambda-expression function))
		 (and (not (compiled-function-p function))
		      `(:anonymous-interpreted-function))
		 (function-name-by-where-loaded-from function))))))

(defmethod print-object ((f function) stream)
  (print-unreadable-object (f stream :identity t)
    (let ((name (any-function-name  f)))
       (if (consp name)
           (format stream "~{~a~^ ~}" name)
           (princ name stream)))))b

(defun each-non-symbol-compiled-function (f)
  (loop for q = (list (find-class t)) then q
	for focus = (pop q)
	while focus
	do (setq q (append q (mop::class-direct-subclasses  focus)))
	   (loop for method in (mop::class-direct-methods focus)
		 do (when (compiled-function-p (mop::method-function method)) (funcall f method (mop::method-function method))))
	   (loop for slot in (mop::class-direct-slots focus)
		 for initfunction = (and (slot-boundp slot 'initfunction) (slot-value slot 'initfunction))
		 do (and initfunction (compiled-function-p initfunction) (funcall f slot initfunction)))))
                                
;; hooks into defining 
 
(defvar *fset-hooks* nil)

(defvar *annotate-function-backlog?* t)

(defun fset-hook-annotate-internal-function (name function)
  (declare (ignore function))
  (when *annotate-function-backlog?* 
    (setq *annotate-function-backlog?* nil)
    (annotate-internal-functions)
    (annotate-clos-methods)
    (annotate-clos-slots)
    )
  (annotate-internal-functions (list name)))


(defmethod mop::add-direct-method :after (class method)
  (annotate-clos-methods (list method)))

(defmethod mop::ensure-class-using-class :after (class name  &key direct-slots
                                             direct-default-initargs 
                                             &allow-other-keys)
  (annotate-clos-slots (mop::class-direct-slots (find-class name))))

;; needs to be the last thing. Some interaction with the fasl loader
(pushnew 'fset-hook-annotate-internal-function sys::*fset-hooks*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		 
(defun get-pid ()
  (handler-case
      (let* ((runtime
              (java::jstatic "getRuntime" "java.lang.Runtime"))
             (command
              (java::jnew-array-from-array
               "java.lang.String" #("sh" "-c" "echo $PPID")))
             (runtime-exec-jmethod
              ;; Complicated because java.lang.Runtime.exec() is
              ;; overloaded on a non-primitive type (array of
              ;; java.lang.String), so we have to use the actual
              ;; parameter instance to get java.lang.Class
              (java::jmethod "java.lang.Runtime" "exec"
                            (java::jcall
                             (java::jmethod "java.lang.Object" "getClass")
                             command)))
             (process
              (java::jcall runtime-exec-jmethod runtime command))
             (output
              (java::jcall (java::jmethod "java.lang.Process" "getInputStream")
                          process)))
         (java::jcall (java::jmethod "java.lang.Process" "waitFor")
                     process)
	 (loop :with b :do
	    (setq b
		  (java::jcall (java::jmethod "java.io.InputStream" "read")
			      output))
	    :until (member b '(-1 #x0a))	; Either EOF or LF
	    :collecting (code-char b) :into result
	    :finally (return
		       (parse-integer (coerce result 'string)))))
    (t () 0)))
