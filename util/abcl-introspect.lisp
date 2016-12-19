(in-package :system)

(defun function-internal-fields (f)
  (if (symbolp f) 
      (setq f (symbol-function f)))
;; think about other fields
  (let ((fields (#"getDeclaredFields" (#"getClass" f))))
    (loop for field across fields
	  do (#"setAccessible" field t)
	  collect
	  (#"get" field f))))

   
(defun foreach-internal-field (fn-fn not-fn-fn &optional (fns :all) (definer nil))
  "fn-n gets called with top, internal function, not-fn-fn gets called with top anything-but"
  (declare (optimize (speed 3) (safety 0)))
  ;; 10 times faster inlining and using with-constant-signature
  (jss::with-constant-signature ((fields "getDeclaredFields") (get "get") (access "setAccessible") (getclass "getClass"))
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
		     (let ((name? (#"getLambdaName" el)))
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
	  (dolist (p (list-all-packages))
	    (do-symbols (s p)
	      (when (fboundp s)
		(check (symbol-function s) s nil))))
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
    ,@(mop::method-qualifiers method) 
    ,(mapcar #'(lambda (c)
		 (if (typep c 'mop:eql-specializer)
		     `(eql ,(mop:eql-specializer-object c))
		     (class-name c)))
	     (mop:method-specializers method))))



(defun any-function-name (function)
  (let ((top (getf (sys::function-plist function) :internal-to-function)))
    (if top
	`(:local-function ,@(if (#"getLambdaName" function) (list (#"getLambdaName" function)))
			  :in ,@(if (typep top 'mop::standard-method)
				    (cons :method (method-spec-list top))
				    (list top)))
        (let ((method (getf (sys::function-plist function) :method-function)))
          (if method
              (cons :method-function (sys::method-spec-list method))
              (let ((fast-function (getf (sys::function-plist function) :method-fast-function)))
                (if fast-function
                    (cons :method-fast-function (sys::method-spec-list fast-function))
		    (let ((slot (getf (sys::function-plist function) :initfunction)))
		      (if slot
			  (list :slot-initfunction (slot-value slot 'name ) :for (class-name (slot-value slot 'allocation-class)))
			  (or (nth-value 2 (function-lambda-expression function))
			      (and (not (compiled-function-p function))
				   `(:anonymous-interpreted-function))
			      (let* ((class (#"getClass" function))
				     (loaded-from (sys::get-loaded-from function))
				     (name (#"replace" (#"getName" class) "org.armedbear.lisp." ""))
				     (where (and loaded-from (concatenate 'string (pathname-name loaded-from) "." (pathname-type loaded-from)))))
				`(:anonymous-function ,name ,@(if (sys::arglist function)  (sys::arglist function)) 
						      ,@(if where (list (list :from where)))))))))))))))

(defmethod print-object ((f function) stream)
  (print-unreadable-object (f stream :identity t)
    (let ((name (any-function-name  f)))
       (if (consp name)
           (format stream "~{~a~^ ~}" name)
           (princ (any-function-name  f) stream)))))
                          
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
