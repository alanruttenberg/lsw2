(in-package :cl-user)

;; Print out a semi-readable list of the actions that load-system would do.

(defun show-system-plan (system-name &key force (relative t))
  (let* ((system (asdf::find-system system-name))
	 (plan (asdf::make-plan 'asdf::sequential-plan 'asdf::load-op system 
	       :forcing (if force 
			    (asdf::make-forcing :system (asdf:find-system system-name) :force force)
			    nil))))

    (map nil (lambda(action) 
	       (let ((op (intern (string (class-name (class-of (car action)))) 'keyword))
		     (kind (intern (string (class-name (class-of (cdr action)))) 'keyword))
		     )
		 (cond ((and (member op '(:load-op :compile-op))
			     (member kind `(:cl-source-file :org)))
			(format t "~a ~a~%" op
				(if relative 
				    (#"replaceFirst"
				     (#"replaceFirst" (namestring (asdf::component-pathname (cdr action))) "/Users/alanr/repos/" "")
				     "/Users/alanr/quicklisp/dists/quicklisp/software/" "")
				    (namestring (asdf::component-pathname (cdr action))))
			       (asdf::component-name (cdr action)))))))
	 (subseq (asdf::plan-actions plan) 0 ))))
