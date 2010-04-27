(in-package :cl-user)

;;; Generalized Inspector, based on Skij

;;; Contains the non-interface dependent parts of inspector.

(defun sdescribe (x)
  (mapc #'(lambda (item)
	    (princ (car item))	;in some cases this should be write (ie, hashtables)
	    (princ ": ")
	    (princ (cadr item))
	    (terpri))
	(inspect-data x)))

(defun inspect-data (object)
  (cond ((vectorp object)
	 (inspect-data-vector object))
	((listp object)
	 (inspect-data-list object))
	((instanceof object "java.util.Enumeration")
	 (inspect-data-list (map-enumeration (lambda (x) x) object)))
	((instanceof object "java.util.Vector")
	 (inspect-data-list (map-enumeration (lambda (x) x) (invoke object 'elements))))
	((instanceof object "java.util.Hashtable")
	 (inspect-data-ht object))
	(t
	 (inspect-data-obj object))))

(defun inspect-data-list (lst)
  (let ((i -1))
    (cons '("Index" "Element")
	  (mapcar #'(lambda (elt)
		      (setq i (+ i 1))
		      (list i elt))
		  lst))))

(defun inspect-data-vector (vector)
  (let ((i -1))
    (cons '("Index" "Element")
	  (map-vector (lambda (elt)
			(setq i (+ i 1))
			(list i elt))
		      vector))))

(defun inspect-data-obj (object)
  (let* ((class (jobject-class object))
	 (data (inspect-data-obj1 object class nil)))

;;     (when (eq? class (jclass "java.lang.Class"))
;; 	(define static-data (inspect-data-obj1 object object #t))
;; 	(for-each (lambda (entry)
;; 		    (set-car! entry
;; 			      (string-append "[static] " (car entry))))
;; 		  static-data)
;; 	(set! data (nconc static-data data)))
    (cons '("Member" "Value") data)))

(defparameter no-args (jnew-array "java.lang.Object" 0))

(defun inspect-data-obj1 (object class static?)
  (let ((data '()))
    (flet ((make-entry (name value)
	     (setq data (cons (list name value) data))))
      ;; fields
      (mapc #'(lambda (ivar)
		(if (not (xor static? (member-static? ivar)))
		    (make-entry (#"getName" ivar)
				(#0"get" ivar object))))
	      ;(invoke class 'getFields)
	    (all-fields class))
      ;; getter methods
      (for-vector #'(lambda (method)
		      (if (not (xor static? (member-static? method)))
			  (let ((name (#"getName" method)))
			    (if (and (>= (length name) 3)
				     (or (equal (subseq name 0 3) "get")
					 (equal (subseq name 0 2) "is"))
				     (zerop (length (#"getParameterTypes" method))))
				(progn ; catch
				  (make-entry
				   (string-append name "( )")
				   (#0"invoke" method object no-args)
				   ))))))
		  (#"getMethods" class))
      (setq data (sort data #'string-lessp :key #'car))
      data)))

;;; new improved...
;;; +++ memoize
(defun all-fields (class) 
  (if nil ; +++ java2?
      (let ((basefields (vector->list (#"getDeclaredFields" class)))
	    (superclass  (#"getSuperclass" class)))
	(for-each (lambda (f) (#"setAccessible" f t)) basefields)
	(if (%%null? superclass)
	    basefields
	    (merge-fields basefields
			  (all-fields superclass))))
      (vector->list (#"getFields" class))))



