(in-package :jss)

(defun invoke-find-method (method object args)
  (let ((jmethod nil)) ; temporary fix for cached method lookup issue (get-jmethod method object)))
    (unless jmethod
      (setf jmethod 
            (if (symbolp object)
                ;;; static method
                (apply #'jmethod (lookup-class-name object) 
                       method (mapcar #'jobject-class args))
                  ;;; instance method
                (apply #'jresolve-method 
                       method object args)))
      (if (null jmethod) (error "There's no method ~A with the signature ~A" method (cons object args)))
      (jcall +set-accessible+ jmethod +true+))
      ;; temporary: see above (set-jmethod method object jmethod))
    jmethod))


(in-package :jss)

(defun set-java-field (object field value &optional (try-harder *running-in-osgi*))
  (if try-harder
      (let* ((class (if (symbolp object)
			(setq object (find-java-class object))
		      (if (equal "java.lang.Class" (jclass-name (jobject-class object)) )
			  object
			(jobject-class object))))
	     (jfield (if (java-object-p field)
			 field
		       (find field (#"getDeclaredFields" class) :key 'jfield-name :test 'equal))))
	(#"setAccessible" jfield t)
	(values (#"set" jfield object value) jfield))
      ;; Below replaces Invoke.poke, etc
    (if (symbolp object)
	(let ((class (find-java-class object)))
	  (setf (jfield class field) value))
	(setf (jfield  field object) value))))

(in-package :cl-user)

(in-package :jss)

(defun invoke-restargs (method object args &optional (raw? nil))
;;  (pprint (type-of args))
  (let* ((object-as-class-name 
          (if (symbolp object) (maybe-resolve-class-against-imports object)))
         (object-as-class 
          (if object-as-class-name (find-java-class object-as-class-name))))
    (setf args (map 'list (lambda (x) (if (eq t x) +true+ x)) args)) ; make all the ts +true+
    (if (eq method 'new)
        (apply #'jnew (or object-as-class-name object) args)
        (if raw?
            (if (symbolp object)
                (apply #'jstatic-raw method object-as-class  args)
                (apply #'jcall-raw method object  args))
            (if (symbolp object)
                (apply #'jstatic method object-as-class args)
                (apply #'jcall method object args))))))




(in-package :cl-user)

