(in-package :ext)
#+abcl
(require 'jss)
#+abcl
(progn
  (defvar *exit-hooks* nil)

  (flet ((workaround ()
	   (#"addShutdownHook" 
	    (#"getRuntime" 'java.lang.Runtime) 
	    (jss::new 'thread (jss::jdelegating-interface-implementation
			  (jss::find-java-class "Runnable")
			  nil
			  "run"
			  (lambda(&rest ignore)
			    (dolist (h *exit-hooks*)
			      (ignore-errors (funcall h)))))))))
    (workaround))

  (export '*exit-hooks* 'ext)

)
