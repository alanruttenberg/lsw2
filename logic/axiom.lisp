(in-package :logic)

(defvar *axioms* (make-hash-table))

(defclass axiom () 
  ((sexp :accessor axiom-sexp :initarg :sexp)
   (name :accessor axiom-name :initarg :name)
   (description :accessor axiom-description :initarg :description)
   (plist :accessor axiom-plist :initarg :plist)))

(defmethod render ((g logic-generator) (a axiom))
  (generate-from-sexp g (axiom-sexp a)))

(defmethod render ((g symbol) (a axiom))
  (generate-from-sexp (make-instance g) (axiom-sexp a)))

(defmacro def-logic-axiom (name sexp &optional description &rest key-values)
  `(progn
     (sys::record-source-information-for-type  ',name 'def-logic-axiom)
     (setf (gethash (intern (string ',name) 'keyword) *axioms*)
	   (make-instance 'axiom :sexp 
			  (if  (and (consp ',sexp) (keywordp (car ',sexp)))
			       ',sexp
			       (list :fact ',sexp))
			  :description ,description :name ',name
			  :plist ',key-values))))

(defmethod print-object ((a axiom) stream)
  (print-unreadable-object (a stream :type nil :identity nil)
    (format stream "axiom ~a: ~a" (axiom-name a) 
	    (if (and (consp (axiom-sexp a))
		     (eq  (car (axiom-sexp a)) :fact))
		(second (axiom-sexp a))
		(axiom-sexp a)))))

(defun get-axiom (name)
  (gethash (intern (string name) 'keyword) *axioms*))

(defun get-axioms (&rest key-values)
  (let ((them nil))
    (maphash (lambda(name axiom)
	       (declare (ignore name))
	       (when
		   (loop for (k v) on key-values by #'cddr
			 always (find (list k v) (axiom-plist axiom)))
		 (push axiom them)))
	     *axioms*)
    them))

(defun delete-axiom (name)
  (remhash (intern (string name) 'keyword) *axioms*))





