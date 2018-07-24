(in-package :cl-user)

;; Act a little like shell

;; (& (form)) runs form in the background, returns number - slot in background
;; (jobs) shows background jobs giving status as running or done
;; (% number) returns results of job and empties its slot
;; (kill number) tries to abort the thread

(defvar *default-lparallel-worker-count* 10)

(defclass job ()
  ((future :accessor job-future :initarg :future)
   (form :accessor job-form :initarg :form)
   (started :accessor job-started :initarg :started)
   (slot :accessor job-slot :initarg :slot)
   (ended :accessor job-ended :initarg :ended :initform nil)
   (stdout :accessor job-stdout :initarg :stdout)
   ))

(defmethod print-object ((job job) stream)
  (if (slot-boundp job 'future)
      (print-unreadable-object (job stream)
	(write-char #\space stream)
	(write-string (status-line job) stream)
	(write-char #\space stream))
      (call-next-method)))

(defmethod status-line ((job job))
  (let ((done (job-ended job)))
    (if done 
	(format nil "%~a finished in ~,2f seconds ~a"
		(1+ (job-slot job))
		(/ (- (job-ended job) (job-started job)) 1000.0)
		 (truncating-print (job-form job) 90))
	(format nil "%~a running ~,2f  seconds ~a" 
		(1+ (job-slot job))
		(/ (- (#"currentTimeMillis" 'system)  (job-started job)) 1000.0)
		(truncating-print (job-form job) 90)))))

(defvar *jobs* (make-array 10 :adjustable t))

(defun reset-workers ()
  (setq lparallel:*kernel* (lparallel:make-kernel *default-lparallel-worker-count*)))

(unless lparallel:*kernel*
  (reset-workers))

(defmacro & (form)
  (let ((job (make-symbol "JOB")))
    `(let* ((,job (make-instance 
		   'job 
		   :started (#"currentTimeMillis" 'system) 
		   :slot (next-open-job-slot)
		   :form ',form
		   :stdout *standard-output*)))
       (setf (aref *jobs* (job-slot ,job)) ,job)
       (let (;; capture debugger hook otherwise not properly bound inside thread
	     (debugger-hook *debugger-hook*) 
	     ;; if we're running in swank we have to make sure *buffer-readtable* and *buffer-package* are bound
	     (maybe-swank-vars (if (find-package :swank)
				   (list (intern "*BUFFER-PACKAGE*" :swank) (intern "*BUFFER-READTABLE*" :swank))))
	     (maybe-swank-vals (if (find-package :swank)
				   (list *package* *readtable*))))
	 (setf (job-future ,job) 
	       (lparallel:future (let ((done nil)
				       (*debugger-hook* debugger-hook))
				   (progv maybe-swank-vars maybe-swank-vals
				     (unwind-protect (multiple-value-prog1
						       ,form
						       (setq done t))
				       (setf (job-ended ,job) (#"currentTimeMillis" 'system) )
				       (when done (princ (status-line ,job) (job-stdout ,job)))
				       (unless done (setf (aref *jobs* (job-slot ,job)) nil))))))))
       ,job)))

(defun next-open-job-slot ()
  (loop for el across *jobs*
	for count from 0
	if (null el)
	   do (return-from next-open-job-slot count))
  (vector-push-extend nil *jobs*))

			      
(defun jobs ()
  (loop for el across *jobs*
	for no from 1
	when el
	  do (write-string (status-line el) (terpri))))

(defun flush-jobs (&rest which)
  (if which
      (loop for i in which
	    do (setf (aref *jobs* i) nil))
      (loop for i below (length *jobs*)
	    do (setf (aref *jobs* i) nil))))

(defun % (number &optional (show-command t))
  (let ((job (aref *jobs* (1- number))))
    (if (null job)
	(format t "No job ~a~%" number)
	(progn
	  (setf (aref *jobs* (1- number)) nil)
	  (when show-command (pprint (job-form job)) (terpri))
	  (lparallel::force (job-future job))))))

(defun %. (number)
  (let ((job (aref *jobs* (1- number))))
    (if (null job)
	(format t "No job ~a~%" number)
	job)))

(defun %& (&optional number)
  (if number
      (let ((job (aref *jobs* (1- number))))
	(if (null job)
	    (format t "No job ~a~%" number)
	    (pprint (job-form job))))
      (map nil (lambda(j)
		 (when j
		   (print j)
		   (pprint (job-form j))))
	   *jobs*)))

;; Sometimes the kernel gets screwed for reasons I don't understand
;; Call this if you think it should be doing nothing
;; It asks to parallel sleep #workers
;; If they are all empty the sleep measured for the whole should be around the sleep for one.
;; Suppose only 3 of 10 are actually working, then it will take 4 times to do the 10, 3 at a time and 1 at the end.
(defun check-all-workers-alive ()
  (let ((start (#"currentTimeMillis" 'System))
	(howmany (length (lparallel.kernel::workers lparallel::*kernel*))))
    (lparallel::pmapcar 'sleep (loop repeat howmany collect 0.25))
    (values (< (- (#"currentTimeMillis" 'System) start)  300) (- (#"currentTimeMillis" 'System) start))))
  
