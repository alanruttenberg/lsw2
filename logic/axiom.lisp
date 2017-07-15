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
			  :plist ',(loop for (k v) on key-values by #'cddr collect (list k v))))))

(defmethod print-object ((a axiom) stream)
  (print-unreadable-object (a stream :type nil :identity nil)
    (format stream "axiom ~a: ~a" (axiom-name a) 
	    (if (and (consp (axiom-sexp a))
		     (eq  (car (axiom-sexp a)) :fact))
		(second (axiom-sexp a))
		(axiom-sexp a)))))

(defun get-axiom (name &optional (errorp t))
  (let ((found (gethash (intern (string name) 'keyword) *axioms*)))
    (if (and (not found) errorp)
	(error "Couldn't find axiom named ~s" name))
    found))

(defun get-axioms ( &rest key-values &key (errorp t) &allow-other-keys)
  (remf key-values :errorp)
  (let ((them nil))
    (maphash (lambda(name axiom)
	       (declare (ignore name))
	       (when
		   (loop for (k v) on key-values by #'cddr
			 always (find (list k v) (axiom-plist axiom) :test 'equalp))
		 (push axiom them)))
	     *axioms*)
    (if (and (not them) errorp)
	(error "Couldn't find axiom with keys ~s" key-values))
    them))

(defun delete-axiom (name)
  (remhash (intern (string name) 'keyword) *axioms*))





