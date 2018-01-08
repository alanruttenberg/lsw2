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
   (ended :accessor job-ended :initarg :ended :initform nil)))

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
	(format nil "%~a finished in ~1,'0d seconds ~a"
		(1+ (job-slot job))
		(/ (- (job-ended job) (job-started job)) 1000.0)
		(job-form job))
	(format nil "%~a running ~1,'0d  seconds ~a form" 
		(1+ (job-slot job))
		(/ (- (#"currentTimeMillis" 'system)  (job-started job)) 1000.0)
		(job-form job)))))

(defvar *jobs* (make-array 10 :adjustable t))

(defmacro & (form)
  (let ((job (make-symbol "JOB")))
  `(let* ((,job (make-instance 
		'job 
		:started (#"currentTimeMillis" 'system) 
		:slot (next-open-job-slot)
		:form ',form)))
     (unless lparallel:*kernel*
       (setq lparallel:*kernel* (lparallel:make-kernel *default-lparallel-worker-count*)))
     (setf (aref *jobs* (job-slot ,job)) ,job)
     (setf (job-future ,job) (lparallel:future (unwind-protect ,form 
						 (setf (job-ended ,job) (#"currentTimeMillis" 'system) )
						 (print (status-line ,job)))))
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

(defun flush-jobs ()
  (loop for i below (length *jobs*)
	do (setf (aref *jobs* i) nil)))

(defun % (number)
  (let ((job (aref *jobs* (1- number))))
    (if (null job)
	(format t "No job ~a~%" number)
	(progn
	  (setf (aref *jobs* (1- number)) nil)
	  (lparallel::force (job-future job))))))

(defun %. (number)
  (let ((job (aref *jobs* (1- number))))
    (if (null job)
	(format t "No job ~a~%" number)
	job)))
