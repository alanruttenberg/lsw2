(in-package :cl-user)

(defmethod print-object ((thread threads::thread) stream)
  (print-unreadable-object (thread stream :identity t)
    (format stream "Thread ~a [~a]"
	    (or (threads::thread-name thread) "(unnamed)")
	    (string-downcase #1"thread.javaThread.getState().toString()"))
    (let ((interrupted #1"thread.threadInterrupted")
	  (destroyed #1"thread.destroyed"))
      (if (or interrupted destroyed)
	  (format stream "marked as ~{~a~^, ~}" (append (if interrupted '("interrupted")) (if destroyed '("destroyed"))))))))

(setf (get 'threads::thread :has-print-object) t)


