;; invoke.lisp v1.0
;;
;; Copyright (C) 2005 Alan Ruttenberg
;;
;; Since most of this code is derivative of the Jscheme System, it is
;; licensed under the same terms, namely:

;; This software is provided 'as-is', without any express or
;; implied warranty.

;; In no event will the author be held liable for any damages
;; arising from the use of this software.

;; Permission is granted to anyone to use this software for any
;; purpose, including commercial applications, and to alter it
;; and redistribute it freely, subject to the following
;; restrictions:

;; 1. The origin of this software must not be misrepresented; you
;;    must not claim that you wrote the original software. If you
;;    use this software in a product, an acknowledgment in the
;;    product documentation would be appreciated but is not
;;    required.

;; 2. Altered source versions must be plainly marked as such, and
;;    must not be misrepresented as being the original software.

;; 3. This notice may not be removed or altered from any source
;;    distribution.

;; This file uses invoke.java from jscheme
;; (http://jscheme.sourceforge.net/jscheme/src/jsint/Invoke.java).
;; The easiest way to use it is to download 
;; http://jscheme.sourceforge.net/jscheme/lib/jscheme.jar
;; and add it to the classpath in the file that invokes abcl.

;; Invoke.java  effectively implements dynamic dispatch of java methods. This
;; is used to make it real easy, if perhaps less efficient, to write
;; java code since you don't need to be bothered with imports, or with
;; figuring out which method to call.  The only time that you need to
;; know a class name is when you want to call a static method, or a
;; constructor, and in those cases, you only need to know enough of
;; the class name that is unique wrt to the classes on your classpath.
;;
;; Java methods look like this: #"toString". Java classes are
;; represented as symbols, which are resolved to the appropriate java
;; class name. When ambiguous, you need to be more specific. A simple example:

;; (let ((sw (new 'StringWriter)))
;;   (#"write" sw "Hello ")
;;   (#"write" sw "World")
;;   (print (#"toString" sw)))

;; What's happened here? First, all the classes in all the jars in the classpath have
;; been collected.  For each class a.b.C.d, we have recorded that
;; b.c.d, b.C.d, C.d, c.d, and d potentially refer to this class. In
;; your call to new, as long as the symbol can refer to only one class, we use that
;; class. In this case, it is java.io.StringWriter. You could also have written
;; (new 'io.stringwriter), (new '|io.StringWriter|), (new 'java.io.StringWriter)...

;; the call (#"write" sw "Hello "), uses the code in invoke.java to
;; call the method named "write" with the arguments sw and "Hello
;; ". Invoke.java figures out the right java method to call, and calls
;; it.

;; If you want to do a raw java call, use #0"toString". Raw calls
;; return their results as java objects, avoiding doing the usual java
;; object to lisp object conversions that abcl does.

;; (with-constant-signature ((name jname raw?)*) &body body)
;; binds a macro which expands to a jcall, promising that the same method 
;; will be called every time. Use this if you are making a lot of calls and 
;; want to avoid the overhead of a the dynamic dispatch. 
;; e.g. (with-constant-signature ((tostring "toString")) 
;;        (time (dotimes (i 10000) (tostring "foo"))))
;; runs about 3x faster than (time (dotimes (i 10000) (#"toString" "foo")))
;;
;; (with-constant-signature ((tostring "toString" t)) ...) will cause the 
;; toString to be a raw java call. see get-all-jar-classnames below for an example.
;; 
;; Implementation is that the first time the function is called, the
;; method is looked up based on the arguments passed, and thereafter
;; that method is called directly.  Doesn't work for static methods at
;; the moment (lazy)
;;
;; (japropos string) finds all class names matching string
;; (jcmn class-name) lists the names of all methods for the class
;;
;; TODO
;;   - Use a package other than common-lisp-user
;;   - Make with-constant-signature work for static methods too.
;;   - #2"toString" to work like function scoped (with-constant-signature ((tostring "toString")) ...)
;;   - #3"toString" to work like runtime scoped (with-constant-signature ((tostring "toString")) ...)
;;      (both probably need compiler support to work)
;;   - Maybe get rid of second " in reader macro. #"toString looks nicer, but might 
;;     confuse lisp mode.
;;   - write jmap, analogous to map, but can take java collections, java arrays etc.
;;   - write loop clauses for java collections. 
;;   - Register classes in .class files below classpath directories (when :wild-inferiors works)
;;   - Make documentation like Edi Weitz
;;
;; Thanks: Peter Graves, Jscheme developers, Mike Travers for skij,  
;; Andras Simon for jfli-abcl which bootstrapped me and taught me how to do 
;; get-all-jar-classnames
;; 

;; changelog 

;; Sat January 28, 2006, alanr: 

;; Change imports strategy. Only index by last part of class name,
;; case insensitive. Make the lookup-class-name logic be a bit more
;; complicated. This substantially reduces the time it takes to do the
;; auto imports and since class name lookup is relatively infrequent,
;; and in any case cached, this doesn't effect run time speed.  (did
;; try caching, but didn't pay - more time was spent reading and
;; populating large hash table)
;; 
;; Split class path by ";" in addition to ":" for windows.
;;
;; Tested on windows, linux.

(in-package :cl-user)

;; invoke takes it's arguments in a java array. In order to not cons
;; one up each time, but to be thread safe, we allocate a static array
;; of such arrays and save them in threadlocal storage. I'm lazy and
;; so I just assume you will never call a java method with more than
;; *max-java-method-args*. Fix this if it is a problem for you. We
;; don't need to worry about reentrancy as the array is used only
;; between when we call invoke and when invoke calls the actual
;; function you care about.

(defvar *max-java-method-args* 20 "Increase if you call java methods with more than 20 arguments")

(defun argvs ()
  (let ((get (load-time-value (jmethod (jclass "java.lang.ThreadLocal") "get")))
	(argvs (load-time-value (jnew (jconstructor "java.lang.ThreadLocal"))))
	(null (load-time-value (make-immediate-object nil :ref))))
    (let ((res (jcall-raw get argvs)))
      (if (equal res null)
	  (let ((it (jnew-array "java.lang.Object" *max-java-method-args*)))
	    (dotimes (i *max-java-method-args*)
	      (setf (jarray-ref it i) (jnew-array "java.lang.Object" i)))
	    (jcall (jmethod (jclass "java.lang.ThreadLocal") "set" "java.lang.Object")
		   argvs it)
	    it)
	  res))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *do-auto-imports* t))

(defvar *imports-resolved-classes* (make-hash-table :test 'equal))
(defvar *classpath-manager* nil)


(defun find-java-class (name)
  (jclass (maybe-resolve-class-against-imports name)))

(defmacro invoke-add-imports (&rest imports)
  "push these imports onto the search path. If multiple, earlier in list take precedence"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (clrhash *imports-resolved-classes*)
     (dolist (i (reverse ',imports))
       (setq *imports-resolved-classes* (delete i *imports-resolved-classes* :test 'equal))
       )))

(defun clear-invoke-imports ()
  (clrhash *imports-resolved-classes*))

(defun maybe-resolve-class-against-imports (classname)
  (or (gethash classname *imports-resolved-classes*)
      (let ((found (lookup-class-name classname)))
	(if found
	    (progn 
	      (setf (gethash classname *imports-resolved-classes*) found)
	      found)
	    (string classname)))))

(defvar *class-name-to-full-case-insensitive* (make-hash-table :test 'equalp))

;; This is the function that calls invoke to call your java method. The first argument is the 
;; method name or 'new. The second is the object you are calling it on, followed by the rest of the
;; arguments. If the "object" is a symbol, then that symbol is assumed to be a java class, and 
;; a static method on the class is called, otherwise a regular method is called. 

(defun invoke (method object &rest args)
    (invoke-restargs method object args))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *invoke-methods*
    (load-time-value (jcall (jmethod "java.lang.Class" "getMethods" ) (jclass "jsint.Invoke")))))

(defun invoke-restargs (method object args &optional (raw? nil))
  (symbol-macrolet 
      ((no-argss (load-time-value (jnew-array "java.lang.Object" 0)))
       (invoke-class (load-time-value (jclass "jsint.Invoke")))
       (ic (load-time-value (find "invokeConstructor" *invoke-methods* :key  'jmethod-name :test 'equal)))
       (is (load-time-value (find "invokeStatic"  *invoke-methods* :key  'jmethod-name :test 'equal)))
       (ii (load-time-value (find "invokeInstance"  *invoke-methods* :key  'jmethod-name :test 'equal)))
       (true (load-time-value (make-immediate-object t :boolean)))
       (false (load-time-value (make-immediate-object nil :boolean))))
    (let* (
	  ;; these two lookups happen before argv is filled, because they themselves call invoke.)
	 (object-as-class-name (if (symbolp object) (maybe-resolve-class-against-imports object)))
	 (object-as-class (if object-as-class-name (find-java-class object-as-class-name)))
	 )
;    (declare (optimize (speed 3) (safety 0)))
    (let ((argv (if (null (the list args))
		    no-argss
		    (let ((argv (jarray-ref-raw (argvs) (length (the list args))))
			  (i -1))
		      (dolist (arg args) 
			(setf (jarray-ref argv (incf (the fixnum i)))
			      (if (eq arg t) true (if (eq arg nil) false arg))))
		      argv))))
      (if (eq method 'new)
	  (progn
	    (jstatic-raw ic invoke-class (or object-as-class-name object) argv))
	  (if raw?
	      (if (symbolp object)
		  (jstatic-raw is invoke-class object-as-class method argv)
		  (jstatic-raw ii invoke-class object method argv true))
	      (if (symbolp object)
		  (jstatic is invoke-class object-as-class method argv)
		  (jstatic ii invoke-class object method argv true)
		  )))))))

;; (defconstant no-args (load-time-value (jnew-array "java.lang.Object" 0)))
;; (defconstant invoke-class (load-time-value (jclass "jsint.Invoke")))
;; (defconstant ic (load-time-value (find "invokeConstructor" *invoke-methods* :key  'jmethod-name :test 'equal)))
;; (defconstant is (load-time-value (find "invokeStatic"  *invoke-methods* :key  'jmethod-name :test 'equal)))
;; (defconstant ii (load-time-value (find "invokeInstance"  *invoke-methods* :key  'jmethod-name :test 'equal)))
;; (defconstant true (load-time-value (make-immediate-object t :boolean)))
;; (defconstant false (load-time-value (make-immediate-object nil :boolean)))

;; (defun invoke-restargs (method object args &optional (raw? nil))
;;   (let* (;; these two lookups happen before argv is filled, because they themselves call invoke.
;; 	 (object-as-class-name (if (symbolp object) (maybe-resolve-class-against-imports object)))
;; 	 (object-as-class (if object-as-class-name (find-java-class object-as-class-name)))
;; 	 )
;;     (declare (optimize (speed 3) (safety 0)))
;;     (let ((argv (if (null args) 
;; 		    no-args
;; 		    (let ((argv (jarray-ref-raw (argvs) (length args)))
;; 			  (i -1))
;; 		      (dolist (arg args) 
;; 			(setf (jarray-ref argv (incf (the fixnum i)))
;; 			      (if (eq arg t) true (if (eq arg nil) false arg))))
;; 		      argv))))
;;       (if (eq method 'new)
;; 	  (progn
;; 	    (jstatic-raw ic invoke-class object-as-class-name argv))
;; 	  (if raw?
;; 	      (if (symbolp object)
;; 		  (jstatic-raw is invoke-class object-as-class method argv)
;; 		  (jstatic-raw ii invoke-class object method argv true))
;; 	      (if (symbolp object)
;; 		  (jstatic is invoke-class object-as-class method argv)
;; 		  (jstatic ii invoke-class object method argv true)
;; 		  ))))))

(defun invoke-find-method (method object args)
  (let* ((no-args (load-time-value (jnew-array "java.lang.Object" 0)))
	 (invoke-class (load-time-value (jclass "jsint.Invoke")))
	 (ifm (load-time-value (jmethod (jclass "jsint.Invoke") "findMethod" (jclass "[Ljava.lang.Object;") (jclass "[Ljava.lang.Object;"))))
	 (imt (load-time-value (find "methodTable"  *invoke-methods* :key  'jmethod-name :test 'equal)))
	 (true (load-time-value (make-immediate-object t :boolean)))
	 (false (load-time-value (make-immediate-object nil :boolean))))
    (let ((args (if (null args) 
		    no-args
		    (let ((argv (jarray-ref-raw (argvs) (length args)))
			  (i -1))
		      (dolist (arg args) 
			(setf (jarray-ref argv (incf i))
			      (if (eq arg t) true (if (eq arg nil) false arg))))
		      argv))))
      (if (symbolp object)
	  (jstatic ifm invoke-class (jstatic-raw imt invoke-class (lookup-class-name object) method true true) args)
	  (jstatic ifm invoke-class (jstatic-raw imt invoke-class (jobject-class object) method false true) args)))))


;; This is the reader macro for java methods. it translates the method
;; into a lambda form that calls invoke. Which is nice because you
;; can, e.g. do this: (mapcar #"toString" list-of-java-objects). The reader
;; macro takes one arg. If 0, then jstatic-raw is called, so that abcl doesn't
;; automagically convert the returned java object into a lisp object. So
;; #0"toString" returns a java.lang.String object, where as #"toString" returns
;; a regular lisp string as abcl converts the java string to a lisp string.


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage lambdas (:use))
  (defvar *lcount* 0))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun read-invoke (stream char arg) 
    (unread-char char stream)
    (let ((name (read stream)))
      (if (and arg (eql (abs arg) 1))
	  (let ((cell (intern (format nil "G~a" (incf *lcount*)) 'lambdas)) ; work around bug that gensym here errors when compiling
		(object-var (intern (format nil "G~a" (incf *lcount*)) 'lambdas)) ;; work around bug in 0.18 symbol-macrolet
		(args-var (intern (format nil "G~a" (incf *lcount*)) 'lambdas)))
	    (proclaim `(special ,cell))
					;	    (set cell nil)
	    `(lambda (,object-var &rest ,args-var)
	       (declare (optimize (speed 3) (safety 0)))
	       (if (boundp ',cell) ;costing me 10% here because I can't force cell to be bound and hence do null test.
		   (if (null ,args-var)
		       (jcall ,cell ,object-var)
		       (if (null (cdr (the cons ,args-var)))
			   ,(if (minusp arg)
				`(jcall-static ,cell ,object-var (car (the cons ,args-var)))
				`(jcall ,cell ,object-var (car (the cons ,args-var))))
			   ,(if (minusp arg)
				`(apply 'jcall-static ,cell ,object-var (the list ,args-var))
				`(apply 'jcall ,cell ,object-var (the list ,args-var)))))
		   (progn
		     (setq ,cell (invoke-find-method ,name ,object-var ,args-var))
		     ,(if (minusp arg)
			  `(apply 'jcall-static ,cell ,object-var ,args-var)
			  `(apply 'jcall ,cell ,object-var ,args-var))))))
	  (let ((object-var (intern (format nil "G~a" (incf *lcount*)) 'lambdas)) ;; work around bug in 0.18 symbol-macrolet
		(args-var (intern (format nil "G~a" (incf *lcount*)) 'lambdas)))
	    `(lambda (,object-var &rest ,args-var) 
	       (invoke-restargs ,name  ,object-var ,args-var ,(eql arg 0)))))))
  (set-dispatch-macro-character #\# #\" 'read-invoke))

(defmacro with-constant-signature (fname-jname-pairs &body body)
  (if (null fname-jname-pairs)
      `(progn ,@body)
      (destructuring-bind ((fname jname &optional raw) &rest ignore) fname-jname-pairs
	(declare (ignore ignore))
	(let ((varname (gensym)))
	  `(let ((,varname nil))
	     (macrolet ((,fname (&rest args)
			  `(if ,',varname
			       (if ,',raw
				   (jcall-raw ,',varname ,@args)
				   (jcall ,',varname ,@args))
			       (progn
				 (setq ,',varname (invoke-find-method ,',jname ,(car args) (list ,@(rest args))))
				 (if ,',raw
				     (jcall-raw ,',varname ,@args)
				     (jcall ,',varname ,@args))))))
	       (with-constant-signature ,(cdr fname-jname-pairs)
		 ,@body)))))))

(defun lookup-class-name (name)
  (setq name (string name))
  (let* (;; cant (last-name-pattern (#"compile" '|java.util.regex.Pattern| ".*?([^.]*)$"))
	 ;; reason: bootstrap - the class name would have to be looked up...
	 (last-name-pattern (load-time-value (jstatic (jmethod "java.util.regex.Pattern" "compile"
							       (jclass "java.lang.String"))
						      (jclass "java.util.regex.Pattern") 
						      ".*?([^.]*)$")))

	 (last-name 
	  (let ((matcher (#0"matcher" last-name-pattern name)))
	    (#"matches" matcher)
	    (#"group" matcher 1))))
    (let* ((bucket (gethash last-name *class-name-to-full-case-insensitive*))
	   (bucket-length (length bucket)))
      (or (find name bucket :test 'equalp)
	  (flet ((matches-end (end full test)
		   (= (+ (or (search end full :from-end t :test test) -10)
			 (length end))
		      (length full)))
		 (ambiguous (choices)
		   (error "Ambiguous class name: ~a can be ~{~a~^, ~}" name choices)))
	    (if (zerop bucket-length)
		name
		(let ((matches (loop for el in bucket when (matches-end name el 'char=) collect el)))
		  (if (= (length matches) 1)
		      (car matches)
		      (if (= (length matches) 0)
			  (let ((matches (loop for el in bucket when (matches-end name el 'char-equal) collect el)))
			    (if (= (length matches) 1)
				(car matches)
				(if (= (length matches) 0)
				    name
				    (ambiguous matches))))
			  (ambiguous matches))))))))))

(defun get-all-jar-classnames (jar-file-name)
  (let* ((jar (jnew (jconstructor "java.util.jar.JarFile" (jclass "java.lang.String")) (namestring (truename jar-file-name))))
         (entries (#"entries" jar)))
    (with-constant-signature ((matcher "matcher" t) (substring "substring")
			      (jreplace "replace" t) (jlength "length")
			      (matches "matches") (getname "getName" t)
			      (next "nextElement" t) (hasmore "hasMoreElements")
			      (group "group"))
      (loop while (hasmore entries)
	 for name =  (getname (next entries))
	 with class-pattern = (#"compile" '|java.util.regex.Pattern| "[^$]*\\.class$")
	 with name-pattern = (#"compile" '|java.util.regex.Pattern| ".*?([^.]*)$")
	 when (matches (matcher class-pattern name))
	 collect
	   (let* ((fullname (substring (jreplace name #\/ #\.) 0 (- (jlength name) 6)))
		  (matcher (matcher name-pattern fullname))
		  (name (progn (matches matcher) (group matcher 1))))
	     (cons name fullname))
	 ))))

(defun jar-import (file)
  (when (probe-file file)
    (loop for (name . full-class-name) in (get-all-jar-classnames file)
       do 
	 (pushnew full-class-name (gethash name *class-name-to-full-case-insensitive*) 
		  :test 'equal))))

(defun new (class-name &rest args)
  (invoke-restargs 'new class-name args))

(defvar *running-in-osgi* (ignore-errors (jclass "org.osgi.framework.BundleActivator")))


(defun get-java-field (object field &optional (try-harder *running-in-osgi*))
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
	(values (#"get" jfield object) jfield))
    (if (symbolp object)
	(let ((class (find-java-class object)))
	  (#"peekStatic" 'invoke class field))
      (#"peek" 'invoke object field))))

;; use #"getSuperclass" and #"getInterfaces" to see whether there are fields in superclasses that we might set
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
    (if (symbolp object)
	(let ((class (find-java-class object)))
	  (#"pokeStatic" 'invoke class field value))
      (#"poke" 'invoke object field value))))

(defun find-java-class (name)
  (if *classpath-manager*
      (or (#1"classForName" *classpath-manager* (maybe-resolve-class-against-imports name))
	  (ignore-errors (jclass (maybe-resolve-class-against-imports name))))
    (jclass (maybe-resolve-class-against-imports name))))

(defmethod print-object ((obj (jclass "java.lang.Class")) stream) 
  (print-unreadable-object (obj stream :identity nil)
    (format stream "java class ~a" (jclass-name obj))))

(defmethod print-object ((obj (jclass "java.lang.reflect.Method")) stream) 
  (print-unreadable-object (obj stream :identity nil)
    (format stream "method ~a" (#"toString" obj))))

(defun do-auto-imports ()
  (flet ((import-class-path (cp)
	   (map nil
		(lambda(s) 
		  (setq s (jcall "toString" s))
		  (when *load-verbose*
		    (format t ";Importing ~a~%" s))
		  (cond 
		    ((file-directory-p s) )
		    ((equal (pathname-type s) "jar")
		     (jar-import (merge-pathnames (jcall "toString" s) (format nil "~a/" (jstatic "getProperty" "java.lang.System" "user.dir")))))))
		
		(jcall "split" cp (string (jstatic "peekStatic" '|jsint.Invoke| (jclass "java.io.File") "pathSeparatorChar")))
		)))
    (import-class-path (jcall "getClassPath" (jstatic "getRuntimeMXBean" '|java.lang.management.ManagementFactory|)))
    (import-class-path (jcall "getBootClassPath" (jstatic "getRuntimeMXBean" '|java.lang.management.ManagementFactory|)))
    ))

(eval-when (:load-toplevel :execute)
  (when *do-auto-imports* 
    (do-auto-imports)))

(defun japropos (string)
  (setq string (string string))
  (let ((matches nil))
    (maphash (lambda(key value) 
	       (declare (ignore key))
	       (loop for class in value
		  when (search string class :test 'string-equal)
		    do (pushnew (list class "Java Class") matches :test 'equal)))
	     *class-name-to-full-case-insensitive*)
    (loop for (match type) in (sort matches 'string-lessp :key 'car)
	 do (format t "~a: ~a~%" match type))
    ))

(defun jclass-method-names (class &optional full)
  (if (java-object-p class)
      (if (equal (jclass-name (jobject-class class)) "java.lang.Class")
	  (setq class (jclass-name class))
	  (setq class (jclass-name (jobject-class class)))))
  (union
   (remove-duplicates (map 'list (if full #"toString" 'jmethod-name) (#"getMethods" (find-java-class class))) :test 'equal)
   (ignore-errors (remove-duplicates (map 'list (if full #"toString" 'jmethod-name) (#"getConstructors" (find-java-class class))) :test 'equal))))

(defun jcmn (class &optional full)
  (if full 
      (dolist (method (jclass-method-names class t))
	(format t "~a~%" method))
      (jclass-method-names class)))

(defun path-to-class (classname)
  (let ((full (lookup-class-name classname)))
    (#"toString" 
     (#"getResource" 
      (find-java-class full)
      (concatenate 'string "/" (substitute #\/ #\. full) ".class")))))

;; http://www.javaworld.com/javaworld/javaqa/2003-07/02-qa-0725-classsrc2.html

(defun all-loaded-classes ()
  (let ((classes-field 
	 (find "classes" (#"getDeclaredFields" (jclass "java.lang.ClassLoader"))
	       :key #"getName" :test 'equal)))
    (#"setAccessible" classes-field t)
    (loop for classloader in 
	 (list* (#"getClassLoader" (jclass "org.armedbear.lisp.Lisp"))
		(and *classpath-manager* (list (#"getBaseLoader" *classpath-manager*))))
	 append
	 (loop with classesv = (#"get" classes-field classloader)
	    for i below (#"size" classesv)
	    collect (#"getName" (#"elementAt" classesv i)))
	 append
	 (loop with classesv = (#"get" classes-field (#"getParent" classloader))
	    for i below (#"size" classesv)
	    collect (#"getName" (#"elementAt" classesv i))))))
	 

;; Modifiy this from Java.java to add a lisp defined classloader.
;;     private static Class classForName(String className) throws ClassNotFoundException
;;     {
;;         try {
;;             return Class.forName(className);
;;         }
;;         catch (ClassNotFoundException e) {
;;             return Class.forName(className, true, JavaClassLoader.getPersistentInstance());
;;         }
;;     }
;; http://www.javaworld.com/javaworld/jw-10-1996/jw-10-indepth-p2.html

(defvar *classpath-manager* nil)

(defvar *added-to-classpath* nil)

(defun maybe-install-bsh-classloader ()
  (unless *classpath-manager*
    (when (ignore-errors (jclass "bsh.classpath.ClassManagerImpl"))
      (let* ((urls (jnew-array "java.net.URL" 0))
	     (manager (jnew "bsh.classpath.ClassManagerImpl"))
	     (bshclassloader (jnew "bsh.classpath.BshClassLoader" manager urls)))
	(#"setClassLoader" '|jsint.Import| bshclassloader)
	(setq *classpath-manager* manager)))))

(defun ensure-dynamic-classpath ()
  (assert *classpath-manager* () "Can't add to classpath unless bean shell jar is in your classpath"))

(defvar *inhibit-add-to-classpath* nil)

(defun add-to-classpath (path &optional force)
  (unless *inhibit-add-to-classpath*
    (ensure-dynamic-classpath)
    (clear-invoke-imports)
    (let ((absolute (namestring (truename path))))
;;       (when (not (equal (pathname-type absolute) (pathname-type path)))
;; 	(warn "HEY! ~a, ~a ~a, ~a" path (pathname-type path) absolute (pathname-type absolute))
;; 	(setq @ (list path absolute)))
      ;; NOTE: for jar files, specified as a component, the ".jar" is part of the pathname-name :(
      (when (or force (not (member absolute *added-to-classpath* :test 'equalp)))
	(#"addClassPath" *classpath-manager* (new 'java.net.url (#"replaceAll" (#"replaceAll" (concatenate 'string "file://" absolute) "\\\\" "/") "C:" "")))
	(#"setClassLoader" '|jsint.Import| (#"getBaseLoader" *classpath-manager*))
;	(format t "path=~a type=~a~%"  absolute (pathname-type absolute))
	(cond ((equal (pathname-type absolute) "jar")
	       (jar-import absolute))
	      ((file-directory-p absolute)
	       (classfiles-import absolute)))
	(push absolute *added-to-classpath*)))))

(defun get-dynamic-class-path ()
  (ensure-dynamic-classpath)
  (map 'list (lambda(el) 
	       (let ((path (#"toString" el)))
		 (if (eql (search "file:/" path) 0)
		     (subseq path 5)
		     path)))
       (#"getPathComponents" (#"getClassPath" *classpath-manager*))))

(eval-when (:load-toplevel :execute)
  (maybe-install-bsh-classloader))



; http://java.sun.com/j2se/1.5.0/docs/api/java/lang/management/MemoryMXBean.html
; http://java.sun.com/docs/hotspot/gc/
; http://www.javaworld.com/javaworld/jw-01-2002/jw-0111-hotspotgc-p2.html
; http://java.sun.com/docs/hotspot/VMOptions.html
; http://java.sun.com/docs/hotspot/gc5.0/gc_tuning_5.html
; http://java.sun.com/docs/hotspot/gc1.4.2/faq.html
; http://java.sun.com/developer/technicalArticles/Programming/turbo/
;-XX:MinFreeHeapRatio=
;-XX:MaxHeapFreeRatio=
;-XX:NewRatio=
;-XX:SurvivorRatio=
;-XX:SoftRefLRUPolicyMSPerMB=10000
;-XX:+PrintTenuringDistribution
;-XX:MaxLiveObjectEvacuationRatio


(defun java-gc ()
  (#"gc" (#"getRuntime" 'java.lang.runtime))
  (#"runFinalization" (#"getRuntime" 'java.lang.runtime))
  (#"gc" (#"getRuntime" 'java.lang.runtime))
  (java-room))

(defun java-room ()
  (let ((rt (#"getRuntime" 'java.lang.runtime)))
    (values (- (#"totalMemory" rt) (#"freeMemory" rt))
	   (#"totalMemory" rt)
	   (#"freeMemory" rt)
	   (list :used :total :free))))

(defun verbose-gc (&optional (new-value nil new-value-supplied))
  (if new-value-supplied
      (progn (#"setVerbose" (#"getMemoryMXBean"  'java.lang.management.ManagementFactory) new-value) new-value)
      (#"isVerbose" (#"getMemoryMXBean"  'java.lang.management.ManagementFactory))))

(defun all-jars-below (directory) 
  (loop with q = (system:list-directory directory) 
     while q for top = (pop q)
     if (null (pathname-name top)) do (setq q (append q (all-jars-below top))) 
     if (equal (pathname-type top) "jar") collect top))

(defun all-classfiles-below (directory) 
  (loop with q = (system:list-directory directory) 
     while q for top = (pop q)
     if (null (pathname-name top)) do (setq q (append q (all-classfiles-below top ))) 
     if (equal (pathname-type top) "class")
     collect top
     ))

(defun all-classes-below-directory (directory)
  (loop for file in (all-classfiles-below directory) collect
       (format nil "~{~a.~}~a"
	       (subseq (pathname-directory file) (length (pathname-directory directory)))
	       (pathname-name file))
       ))

(defun classfiles-import (directory)
  (setq directory (truename directory))
  (loop for full-class-name in (all-classes-below-directory directory)
       for name = (#"replaceAll" full-class-name "^.*\\." "")
     do
       (pushnew full-class-name (gethash name *class-name-to-full-case-insensitive*) 
		:test 'equal)))

(defun add-directory-jars-to-class-path (directory recursive-p)
  (if recursive-p
      (loop for jar in (all-jars-below directory) do (cl-user::add-to-classpath jar))
      (loop for jar in (directory (merge-pathnames "*.jar" directory)) do (cl-user::add-to-classpath jar))))

(defun need-to-add-directory-jar? (directory recursive-p)
  (if recursive-p
      (loop for jar in (all-jars-below directory)
	 do
	   (if (not (member (namestring (truename jar)) *added-to-classpath* :test 'equal))
	       (return-from need-to-add-directory-jar? t)))
      (loop for jar in (directory (merge-pathnames "*.jar" directory))
	 do
	   (if (not (member (namestring (truename jar)) *added-to-classpath* :test 'equal))
	       (return-from need-to-add-directory-jar? t))))
  nil)

(defun set-to-list (set)
  (declare (optimize (speed 3) (safety 0)))
  (with-constant-signature ((iterator "iterator" t) (hasnext "hasNext") (next "next"))
    (loop with iterator = (iterator set)
       while (hasNext iterator)
       for item = (next iterator)
       collect item)))

(defun list-to-list (list)
  (declare (optimize (speed 3) (safety 0)))
  (with-constant-signature ((isEmpty "isEmpty") (getfirst "getFirst")
			    (getNext "getNext"))
    (loop until (isEmpty list)
       collect (getFirst list)
       do (setq list (getNext list)))))

;; Contribution of Luke Hope. (Thanks!)

(defun iterable-to-list (iterable)
 (declare (optimize (speed 3) (safety 0)))
 (let ((it (#"iterator" iterable)))
   (with-constant-signature ((hasmore "hasMoreElements")
			     (next "nextElement"))
     (loop while (hasmore it)
	collect (next it)))))

(defun vector-to-list (vector)
 (declare (optimize (speed 3) (safety 0)))
 (with-constant-signature ((hasmore "hasMoreElements")
			   (next "nextElement"))
     (loop while (hasmore vector)
	collect (next vector))))

(defun hashmap-to-hashtable (hashmap &rest rest &key (keyfun #'identity) (valfun #'identity) (invert? nil)
				    table 
			       &allow-other-keys )
  (let ((keyset (#"keySet" hashmap))
	(table (or table (apply 'make-hash-table
				(loop for (key value) on rest by #'cddr
				   unless (member key '(:invert? :valfun :keyfun :table)) 
				   collect key and collect value)))))
    (with-constant-signature ((iterator "iterator" t) (hasnext "hasNext") (next "next"))
      (loop with iterator = (iterator keyset)
	 while (hasNext iterator)
	 for item = (next iterator)
	 do (if invert?
		(setf (gethash (funcall valfun (#"get" hashmap item)) table) (funcall keyfun item))
		(setf (gethash (funcall keyfun item) table) (funcall valfun (#"get" hashmap item)))))
    table)))
	   
(defun jclass-all-interfaces (class)
  "Return a list of interfaces the class implements"
  (unless (java-object-p class)
    (setq class (find-java-class class)))
  (loop for aclass = class then (#"getSuperclass" aclass)
     while aclass
     append (coerce (#"getInterfaces" aclass) 'list)))

(defun safely (f name)
  (let ((fname (gensym)))
    (compile fname
	     `(lambda(&rest args)
		(with-simple-restart (top-level
				      "Return from lisp method implementation for ~a." ,name)
		  (apply ,f args))))
    (symbol-function fname)))

(defun jdelegating-interface-implementation (interface dispatch-to &rest method-names-and-defs)
  "Creates and returns an implementation of a Java interface with
   methods calling Lisp closures as given in METHOD-NAMES-AND-DEFS.

   INTERFACE is an interface 

   DISPATCH-TO is an existing Java object

   METHOD-NAMES-AND-DEFS is an alternating list of method names
   (strings) and method definitions (closures).

   For missing methods, a dummy implementation is provided that
   calls the method on DISPATCH-TO"
  (let ((implemented-methods
         (loop for m in method-names-and-defs
	    for i from 0
	    if (evenp i) 
	    do (assert (stringp m) (m) "Method names must be strings: ~s" m) and collect m
	    else
	    do (assert (or (symbolp m) (functionp m)) (m) "Methods must be function designators: ~s" m))) 
        (null (make-immediate-object nil :ref)))
    (let ((safe-method-names-and-defs 
	   (loop for (name function) on method-names-and-defs by #'cddr
	      collect name collect (safely  function name))))
      (loop for method across
	   (jclass-methods interface :declared nil :public t)
	   for method-name = (jmethod-name method)
	   when (not (member method-name implemented-methods :test #'string=))
	   do
	   (let* ((def  `(lambda
			     (&rest args)
			   (cl-user::invoke-restargs ,(jmethod-name method) ,dispatch-to args t)
			   )))
	     (push (coerce def 'function) safe-method-names-and-defs)
	     (push method-name safe-method-names-and-defs)))
      (apply #'java::%jnew-proxy  interface safe-method-names-and-defs))))


(defun java-exception-report (condition)
  (if (and (typep condition 'java-exception)
	   (java-exception-cause condition)
	   (equal (jclass-name (jobject-class (java-exception-cause condition)))
		  "jsint.BacktraceException"))
      (with-output-to-string (s)
	(let ((writer (new 'stringwriter)))
	  (#"printStackTrace" (#"getBaseException"(java-exception-cause condition)) (new 'printwriter writer))
	  (write-string (#"replaceFirst" (#"toString" writer) "(?s)\\s*at sun.reflect.*" "") s))
	)
      (#"replaceFirst" (princ-to-string condition) "(?s)\\\\s*at jsint.E.*" "")))

(in-package :asdf)


(defclass jar-directory (static-file) ())

(defmethod perform ((operation compile-op) (c jar-directory))
  (unless cl-user::*inhibit-add-to-classpath*
    (cl-user::add-directory-jars-to-class-path (truename (component-pathname c)) t)))

(defmethod perform ((operation load-op) (c jar-directory))
  (unless cl-user::*inhibit-add-to-classpath*
    (cl-user::add-directory-jars-to-class-path (truename (component-pathname c)) t)))

(defmethod operation-done-p ((operation load-op) (c jar-directory))
  (or cl-user::*inhibit-add-to-classpath*
    (not (cl-user::need-to-add-directory-jar? (component-pathname c) t))))

(defmethod operation-done-p ((operation compile-op) (c jar-directory))
  t)

(defclass jar-file (static-file) ())

(defmethod perform ((operation compile-op) (c jar-file))
  (cl-user::add-to-classpath (component-pathname c)))

(defmethod perform ((operation load-op) (c jar-file))
  (or cl-user::*inhibit-add-to-classpath*
      (cl-user::add-to-classpath (component-pathname c))))

(defmethod operation-done-p ((operation load-op) (c jar-file))
  (or cl-user::*inhibit-add-to-classpath*
      (member (namestring (truename (component-pathname c))) cl-user::*added-to-classpath* :test 'equal)))

(defmethod operation-done-p ((operation compile-op) (c jar-file))
  t)

(defclass class-file-directory (static-file) ())

(defmethod perform ((operation compile-op) (c class-file-directory))
  (cl-user::add-to-classpath (component-pathname c)))

(defmethod perform ((operation load-op) (c class-file-directory))
  (cl-user::add-to-classpath (component-pathname c)))

;; ****************************************************************



