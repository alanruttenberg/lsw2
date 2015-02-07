(in-package :cl-user)

;;; Skij compatibility 

(defun instanceof (object jclass)
  (#"isInstance" (jclass jclass) object))

(defun member-static? (member)
  (#"isStatic" 'java.lang.reflect.Modifier (#"getModifiers" member)))

(defun xor (a b)
  (if a 
      (not b)
      b))

(defun vector->list (v)
  (coerce v 'list))

(defun list->vector (l)
  (coerce l 'vector))

;;; lazy!
(defun for-vector (proc vec) 
  (mapc proc (vector->list vec)))

(defun map-vector (proc vec) 
  (mapcar proc (vector->list vec)))

(defun string-append (&rest strings)
  (apply #'concatenate 'string strings))

(defun maptree (proc tree)
  (cond ((null tree) nil)
	((listp tree)
	 (cons (maptree proc (car tree))
	       (maptree proc (cdr tree))))
	(t (funcall proc tree))))

(defun map-jcollection (proc collection)
  (map-iterator proc (#"iterator" collection)))
 
(defun map-iterator (proc iterator)
    (loop until (not (#"hasNext" iterator))
	  collect (funcall proc (#"next" iterator))))

(defun jcollection->list (collection)
  (map-jcollection #'identity collection))

(defmacro in-own-thread (&body body)
  `(make-thread (safely (lambda() ,@body) "thread")))

;; needs to remain evaluated. yuck.


;;; kludgy but useful
;;; defines COLLECT-IT as a local function (COLLECT is in use elsewhere)
(defmacro collecting (&body body)
  `(let ((collect-result '()))
    (flet ((collect-it (collect-thing)
	     (push collect-thing collect-result)))
      ,@body)
    (nreverse collect-result)))
    
