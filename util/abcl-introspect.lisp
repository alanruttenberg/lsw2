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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; JDI is the debugger API for java. Although it is typical that a
;; separate process runs the debugger, in our case we'll just have a
;; thread that does it, and be careful not to have it be suspended at the
;; wrong time.

;; The first step is to add the tool jar which is part of the JDK

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defun jdi-tools-jar ()
    (merge-pathnames
     (make-pathname :directory '(:relative "lib") :name "tools" :type "jar")
     (java::jstatic "getProperty" (jss::find-java-class 'system) "java.home")
					;(#"getProperty" 'system "java.home")
     ))
  (defvar *jdi-loaded* nil)

;; To start the fun and games, add the jar if necessary checking that it
;; properly loaded by looking for the class jdi.VirtualMachineImpl, an
;; instance of which will be a proxy for the JVM we are running in.

  (defun ensure-jdi-loaded ()
    (or *jdi-loaded*
	(if (ignore-errors (jss::find-java-class 'jdi.VirtualMachineImpl))
	    (setq *jdi-loaded* t)
	    (let ((tools-jar (jdi-tools-jar)))
	      (java::add-to-classpath tools-jar) 
	      (or (ignore-errors (jss::find-java-class 'com.sun.tools.jdi.VirtualMachineImpl))
		  (error "Couldn't find JDI"))
	      (setq *jdi-loaded* t)))))

  (ensure-jdi-loaded))

(defvar *jdi-jvm* nil)

(defun ensure-jdi ()
  (or *jdi-jvm*
      (connect-to-this-jvm)))

;; Then we need to connect. The connection parameters have to match what
;; was said when invoking java. We connect over port 8000. I think there
;; might be a way to connect just given the PID, which would be cleaner.
;; After this *jdi-jvm* will be our virtual machine proxy 

(defun connect-to-this-jvm ()
  (let* ((socketattacher 
	  (find  "com.sun.tools.jdi.SocketAttachingConnector"
		 (#"toArray" (#"attachingConnectors" (#"virtualMachineManager" 'Bootstrap))) 
		 :key (lambda(e)(#"getName" (java::jobject-class e)))
		 :test 'equalp))
	 (arguments (#"defaultArguments" socketattacher)))
    (#"setValue" (#"get" arguments "hostname") "localhost")
    ;; Assumes JAVA invoked with argument "-Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=8000"
    ;; In LSW this is the "-d" flag or setting env. variable ABCL_JDB
    (#"setValue" (#"get" arguments "port") "8000")
    (#"setValue" (#"get" arguments "timeout") "4000")
    (setq *jdi-jvm* (#"attach" socketattacher arguments))))

;; You don't have access to the live objects via the JDI API. Instead
;; there are "references" for all the entities you will be dealing with.

(defun jdi-find-thread-ref (name)
  "Find a thread for JDI use"
  (ensure-jdi)
  (find name (jss::j2list (#"allThreads" *jdi-jvm*)) :key #"name" :test 'equal))

(defun jdi-current-thread-thread-ref ()
  "Ref for the thread this function is running in"
  (jdi-find-thread-ref (#"getName" (#"currentThread" 'thread))))

(defun jdi-get-class-ref (class)
  "Get a debugger reference to a class"
  (ensure-jdi)
  (car 
   (jss::j2list
    (#"classesByName"
     *jdi-jvm* 
     (#"getName"
      (if (java::java-object-p class)
	  class
	  (jss::find-java-class class)))))))

;; In order to inspect stuff on the stack via jdi, one first needs to
;; have an "event" that causes the targeted thread to suspend. Here is an
;; example using an exception as event. We could register to have a
;; callback when this happens but we don't need that for now.

(defun jdi-request-suspend-on-exception (exception-class)
  (ensure-jdi)
  (let ((request 
	  (#"createExceptionRequest"
	   (#"eventRequestManager" sys::*jdi-jvm*)
	   (jdi-get-class-ref exception-class)
	   ;; event if exception caught
	   java:+true+
	   ;; or not
	   java:+true+)))
    ;; when the exception happens only suspend the thread where it happened
    (#"setSuspendPolicy" request (jss::get-java-field 'EventRequest "SUSPEND_EVENT_THREAD"))
    ;; Turn it on
    (#"enable" request)
    request))

(defun jdi-cancel-request (request)
  (#"deleteEventRequest" (#"eventRequestManager" *jdi-jvm*) request))

;; Once the thread is suspended we can ask our debugger thread to get us
;; some information. Here we're spawning a thread which expects that
;; target thread is already suspended, or suspends it itself. Later we'll
;; use a persistent thread allocated to JDI.
					;
(defvar *jdi-mailbox* (threads:make-mailbox))

(defun jdi-call-in-thread-context (f &key (thread-ref (jdi-current-thread-thread-ref)) (suspend nil))
  "Call f with arguments: jvm proxy, target thread ref, to send info back. "
  (threads::make-thread
   ;; Make sure our important bits are captured in the closure
   (let ((mailbox *jdi-mailbox*)
	 (jvm *jdi-jvm*))
     (lambda() 
       ;; In order to do *anything* the target thread needs to be
       ;; suspended. However suspending it from the outside only
       ;; allows some functions. In particular you can't look at the
       ;; stack frames.
       (if suspend (#"suspend" thread-ref))
       ;; be careful
       (multiple-value-bind (result error)
	   (ignore-errors (progn (funcall f jvm thread-ref)))
	 (if (not error)
	     (threads::mailbox-send mailbox result)
	     (threads::mailbox-send mailbox error)))
       ;; we suspended the thread, resume it
       (if suspend (#"resume" thread-ref))))
   :name "abcl jdi query")
  ;; now poll for the result. (This is prototype code)
  (loop while (not (threads:mailbox-peek *jdi-mailbox*))
	repeat 50
	do (sleep .1))
  (if (threads:mailbox-peek *jdi-mailbox*)
      (threads:mailbox-read *jdi-mailbox*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Now we can have some fun

;; this can be called any time - thread doesn't have to be suspended from within
(defun jdi-how-many-frames-on-stack ()
  (ensure-jdi)
  (jdi-call-in-thread-context 
   (lambda(jvm thread-ref) 
     (declare (ignore jvm))
     (#"frameCount" thread-ref)) 
   :suspend t))


;; this demonstrates how to invoke a method in the target jvm
;; suspended thread Get "this" for each frame and then call "toString"
;; on the JVM, collecting the results.  Note that not all frames are
;; the sort that have a "this". In this sample code we just wrap with
;; ignore errors. This won't work unless the thread has been suspended
;; by an "event" in the thread.

(defun jdi-get-most-specific-method (methods class-ref argument-refs)
  "Stub TBD"
  (car methods))

;; Like findMethod 
(defun jdi-method-ref (method-name object-ref &rest argument-refs)
  (let ((rt (#"referenceType" object-ref)))
    (let ((methods (#"methodsByName" (#"referenceType" object-ref) method-name)))
      (jdi-get-most-specific-method (coerce (#"toArray" methods) 'list) object-ref argument-refs))))

(defparameter +invoke-single-threaded+ (jss::jfield "com.sun.jdi.ObjectReference" "INVOKE_SINGLE_THREADED")
  "Option indicating that the invocation will not resume any threads other than the one we are invoking in")

(defun jdi-this-in-stack-frame (thread-ref stackframe)
  "Gets a reference to this for stackframe and calls toString in the
  thread that the stack frame is from. Returns a reference to a
  string, on which we call toString to unwrap"
  (let ((this-ref (#"thisObject" stackframe)))
    (let ((method-ref (jdi-method-ref "toString" this-ref))
	  (arglist (java::jnew (jss::find-java-class 'arraylist))))
      (#"value" (#"invokeMethod" this-ref thread-ref method-ref arglist +invoke-single-threaded+)))))

;; about values https://docs.oracle.com/javase/7/docs/jdk/api/jpda/jdi/

(defun jdi-those-on-stack-frames (thread-name)
  (let ((thread-ref (jdi-find-thread-ref thread-name)))
    (jdi-call-in-thread-context
     (lambda(jvm-ref thread-ref)
       (declare (ignore jvm-ref))
       (loop for i from 1 below  (#"frameCount" thread-ref)
	     ;; protect because not all frames have a this
	     for this-string = (ignore-errors (jdi-this-in-stack-frame thread-ref (#"frame" thread-ref i)))
	     when this-string collect this-string))
     :thread-ref thread-ref :suspend nil)))
 
;; invoke from repl-thread
(defun jdi-test ()
  (ensure-jdi)
  (let ((mail (threads:make-mailbox)))
    (threads:make-thread
     (lambda()
       ;; request suspend on PatternSyntaxException
       (let ((request (jdi-request-suspend-on-exception "java.util.regex.PatternSyntaxException")))
	 ;; let the repl thread its ok to create the exception
	 (threads:mailbox-send mail :ok)
	 (loop repeat 10 until (eq (threads:mailbox-peek mail) :roger) do (sleep .5))
	 (and (threads:mailbox-peek mail) (threads:mailbox-read mail))
	 ;; send it back the results
	 (threads:mailbox-send mail (jdi-those-on-stack-frames "repl-thread"))
	 ;; wake it up from its suspension
	 (#"resume" (jdi-find-thread-ref "repl-thread"))
	 ;; disable the request
	 (jdi-cancel-request request)
	 ))
     :name "jdi test")
    ;; wait until the suspend on exception is set up
    (threads:mailbox-read mail)
    ;; signal back
    (threads:mailbox-send mail :roger)
    ;; do something to trigger the exception
    (ignore-errors (#"replaceAll" "" "(?o" ""))
    ;; read the results
    (threads:mailbox-read mail)
    ))

(provide :abcl-introspect)

#|

(#9"toString" objectref)
->
(invoke-in-debugger-context "toString" objectRef)
->
(jmethod (jclass (#"name" (#"referenceType" object-ref))) "toString")
->
(jmethod "java.lang.Object" "toString") 
->
(find-methodref-matching *)
->

|#
