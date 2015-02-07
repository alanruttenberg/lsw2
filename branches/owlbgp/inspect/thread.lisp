(in-package :cl-user)

;;; Doesn't really climb through all the threadgroups, which it could -- see 
;;; http://www.exampledepot.com/egs/java.lang/ListThreads.htm
(defun all-threads ()
  (let* ((group (#"getThreadGroup" (#"currentThread" 'Thread)))
	 (count (#"activeCount" group))
	 (array (jnew-array "java.lang.Thread" count)))
    (#"enumerate" group array)
    (jvector->list array)))

    
    
