(in-package :cl-user)

 (defmacro eval-with-timeout (expression time &optional when-done )
  `(let (done it result)
     (unwind-protect
	  (progn 
	    (setq it (threads:make-thread 
		      (let ((*standard-output* *standard-output*)
			    (*debug-output* *debug-io*)
			    (*trace-output* *trace-output*))
			(lambda()
			    (catch 'abort
			      (flet ((doit ()
				       (progn ,expression)))
				(prog1
				    (setq result 
					  (if ,when-done
					      (funcall ,when-done (multiple-value-list (doit)))
					      (multiple-value-list (doit))))
				  (setq done t))))))))
	    (loop repeat (* 10 ,time) until done do (sleep .1)))
       (unless done
	 (threads::destroy-thread it))
       (unless done
	 (abort))
       )
     (values-list result)))
