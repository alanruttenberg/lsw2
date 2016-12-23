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
(defun jdi-test (fn)
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
	 (threads:mailbox-send mail (funcall fn))
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


;(jdi-test (lambda() (jdi-those-on-stack-frames "repl-thread")))

