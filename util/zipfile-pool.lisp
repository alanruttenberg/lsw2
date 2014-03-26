#|
Goal: Balance read throughput from one or more compressed archives by parallization.

1) Create a queue the names of an entry in the zip (not parallel), but non blocking by pushing them into a queue. 
2) Done watcher: How 
2) Decompression Consumer:
   Peek at the queue 
   if the token is :done, then sleep and return
   Get a zipfile instance from a resource pool
   access and decompress the entry
   free the zipfile back to the resource pool
   push the buffer on another queue to processed downstream
|#

;;; zipfile pool. give a pathname, get a java zipfile entry. keep in a
;;; hash by filename.

(defclass zipfile-pool (resource-pool))

(defmethod initialize-instance ((r zipfile-pool) &rest args)
  (call-next-method)
  (setf (resource-pool r) (make-hash-table :test 'equalp)))

(defmethod each-available-resource-in-pool ((r zipfile-pool) function)
  (maphash (lambda(key value) (funcall function value))))

;; contract is to create a new resource, deposit it into the pool, and return it
(defmethod create-resource ((r zipfile-pool) &rest args &key filename &allow-other-keys)
  (and (probe-file filename)
       (let ((new (new 'zipfile filename)))
	 (push  new (gethash filename (resource-pool r)))
	 new)))

;; contract is to take one out of the pool, if there is one, or create one.
(defmethod take-resource ((r zipfile-pool) &rest args &key filename &allow-other-keys)
  (let ((resource (pop (gethash filename (resource-pool r)))))
    (unless resource
      (setq resource (apply 'create-resource r args))
      (setf (gethash filename (resource-pool r)) (remove resource (gethash filename (resource-pool r))))
      resource)))

;; contract is to put it back into the pool
(defmethod put-resource ((r zipfile-pool) resource)
  (let ((args (checked-in r resource)))
    (assert (getf args :filename) () "trying to put back a zipfile but don't know the filename")
    (push resource (gethash (getf args :filename) (resource-pool r)))))

    
