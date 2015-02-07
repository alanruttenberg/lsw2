(in-package :cl-user)

(ql:quickload "bordeaux-threads")

;; base class

(defclass resource-pool ()
  ((resource-pool :accessor resource-pool :initform nil)
   (taken-resources :accessor taken-resources :initform nil)
   (pool-name :accessor pool-name :initarg :pool-name :initform "POOL")
   (pool-lock :accessor pool-lock)
   (pool-limit :accessor pool-limit :initform nil :initarg :pool-limit)
   (checked-out-count :accessor checked-out-count :initform 0)
   (active? :accessor active? :initform nil)
   (promises :accessor promises :initarg :promises :initform nil)))

;; create a new instance of resource. (internal - specialize)
;; Protocol: 
;; Create a pool (make-instance 'resource-pool :pool-name "name")

;; Destroy a pool (destroy-pool pool). Finalize-resource is called on
;; each element of resource pool and pool is marked inactive. All
;; resources taken should have been put back before destroying. Pool
;; is expected to be active on entry.

;; active? t if a pool is still active

;; take-resource - retrieve a resource, creating a fresh one if there
;; are none and returning nil if not possible. Pool expected to be active.

;; put-resource - return a resource. Pool expected to be
;; active. Resource expected to have been previously taken from this
;; pool.

;; each-available-resource-in-pool function: (internal - specialize)
;; Call function on each of the resources currently available to be taken


(defmethod initialize-instance ((r resource-pool) &rest initargs)
  (call-next-method)
  (setf (pool-lock r) (bt:make-lock (concatenate 'string (pool-name r) "-LOCK")))
  (setf (active? r) t))

;; specialize this in your resource pool subclass
(defmethod create-resource ((r resource-pool) &rest args &key &allow-other-keys)
  (error "create resource needs to be defined for class ~a" (class-of r))
  )

;; check for active before and after locking because there could be a
;; race condition for retrieving the lock, and if the winner is
;; destroy-pool then the pool will be inactive even though it was
;; active when we first checked.

#|;;;;;;;;;;;;;;;;

How to handle the case of :wait? t ??

We need to block when we are waiting. Ideally we wouldn't consume a
thread for that. BUT: If we did we could make a condition variable,
save it, and when put-resource, check and condition-notify a the end
of put-resource.

Or we could sleep and check again. Also consumes a thread.

Can we use futures? So that the submitter is the one to block and we
get to exit?

Or submit task?

Proposal:

Broaden API to say that results are Promises.

If we can't get a resource, return the promise and spawn a task to
sleep a bit and try again but to fulfill the promise rather than care
about returning the result.


|#;;;;;;;;;;;;;;;;
(defmethod take-resource :around ((r resource-pool) &rest args &key (promise-if-unavailable? nil) sleep &allow-other-keys)
  (assert (active? r) () "~s is no longer active" (pool-name r))
  (bt:with-lock-held ((pool-lock r))
    (assert (active? r) () "~s is no longer active" (pool-name r)) 

    ;; handle outstanding promises    

    (when (pool-has-reached-limit? r)
      (if promise-if-unavailable?
	  (let ((promise (lparallel:promise)))
	    (push promise (promises r))
	    (lparallel:submit-timeout
	     (channel r)
	     (/ 1 (pool-poll-frequency r))
	     (lambda() 
	       (apply 'take-resource r args)
	    (return-from take-resource promise))
	  (return-from take-resource nil)))
    (let ((resource (call-next-method)))
      (when resource
	(incf (checked-out-count r))
	(checked-out r resource args))
      resource)))

(defmethod put-resource :around ((r resource-pool) resource)
  (assert (active? r) () "~s is no longer active" (pool-name r))
  (bt:with-lock-held ((pool-lock r))
    (assert (active? r) () "~s is no longer active" (pool-name r)) 
    (when (not (member resource (taken-resources r) :key 'first))
      (warn "attempt to put resource ~a into pool ~a but it wasn't taken from there. resource not returned"
	    resource r))
    (progn
      (call-next-method)
      (setf (taken-resources r) (remove resource (taken-resources r) :key 'car))
      (decf (checked-out-count r)))))

(defmethod destroy-pool :around ((r resource-pool))
  (assert (active? r) () "~s is no longer active" (pool-name r))
  (bt:with-lock-held ((pool-lock r))
    (assert (active? r) () "~s is no longer active" (pool-name r)) 
    (call-next-method)))

;; return a resource to caller or nil if one can't be created,
;; register as checked-out. base class keeps resources in a list. lock
;; has already been acquired.

(defmethod take-resource ((r resource-pool) &rest args &key &allow-other-keys)
  (let ((resource 
	 (or (pop (resource-pool r))
	     (apply 'create-resource r args))))
    resource))

;; keep track of taken resources. must be called with lock already held
(defmethod checked-out ((r resource-pool) resource args)
  (push (list resource args) (taken-resources r)))

(defmethod checked-in ((r resource-pool) resource)
  (let ((it (find resource (taken-resources r) :key 'first)))
    (setf (taken-resources r) (remove it (taken-resources r) :key 'first))
    (second it)
    ))

;; give back a resource that has been previously taken. checks if the
;; resource was taken from this resource. lock has already been acquired
(defmethod put-resource ((r resource-pool) resource)
  (push resource (resource-pool r)))

;; check if the pool capacity has been exceeded. default is that
;; limit, if supplied, denotes the maximum number of resources that
;; can be taken

(defmethod pool-has-reached-limit? ((r resource-pool))
  (and (pool-limit r) (>= (checked-out-count r) (pool-limit r))))

;; iterate over each available resource in pool calling function on
;; each. must be called with lock already held.

(defmethod each-available-resource-in-pool ((r resource-pool) function)
  (loop for res in (resource-pool r)
     do (funcall function res)))

;; free up any resources in pool and mark as inactive. lock has already been acquired
(defmethod destroy-pool ((r resource-pool))
  (setf (active? r) nil)
  (each-available-resource-in-pool r
   (lambda(res) 
     (finalize-resource r res))))

;; do any cleanup on a resource from a pool being destroyed
(defmethod finalize-resource ((r  resource-pool) resource)
  )

;;; test

(defclass string-resource-pool (resource-pool))

(defmethod create-resource ((r string-resource-pool) &rest args &key )
  (make-string 10 :initial-element (code-char (+ (char-code #\a) (floor (random 26))))))
  
'(let* ((sr (make-instance 'string-resource-pool))
	(three (list (take-resource sr) (take-resource sr) (take-resource sr))))
  (loop for el in three do (put-resource sr el))
  (destroy-pool sr)
  (take-resource sr))

