(unless (gethash "ABCL-SRC" SYSTEM:*LOGICAL-PATHNAME-TRANSLATIONS*)
  (if (equalp (USER-HOMEDIR-PATHNAME) "/users/alanr/")
      (setf (logical-pathname-translations "abcl-src")
	    `(("**;*.*" ,(make-pathname :directory (append (pathname-directory "~/repos/j/src/")
							   '(:wild-inferiors))
					:name :wild
					:type :wild))))
      (unless *load-verbose*
	(unless (probe-file (car (get 'defun 'system::%source )))
	  (format t "~{~a~%~}"
		  '("If you don't know what this means you can ignore it:"
		    "ABCL source code meta-point information out of sync. Make sure you have the source code"
		    "installed, then set up the logical host ABCL-SRC to point the src directory under j, e.g.:"))
	  (let ((*print-case* :downcase))
	    (pprint '(setf (logical-pathname-translations "abcl-src")
		      `(("**;*.*" ,(make-pathname :directory (append (pathname-directory "~/repos/j/src/")
								     '(:wild-inferiors))
						  :name :wild
						  :type :wild))))))
	  (format t "~&Then call (fix-abcl-src-pointers). You can do this in your .abclrc file.~%")
	  (format t "~&Source code info available at http://sourceforge.net/cvs/?group_id=55057~%")))))v

(defun fix-abcl-src-pointers () 
  (do-all-symbols (sym) 
    (let ((s (get sym 'system::%source)))
      (when (and s (consp s) (pathnamep (car s)) (search '("org" "armedbear") (pathname-directory (car s)) :test 'equal))
	(let ((new 
	       (cond ((find #\! (namestring (car s)))
		      (format nil "abcl-src:~a"
			      (substitute #\; #\/ 
					  (subseq (namestring (car s))
						  (+ 2 (position #\! (namestring (car s))))))
			      ))
		     ((position "src" (pathname-directory (car s)) :test 'equal)
		      (format nil "abcl-src:~{~A;~}~a.~a"
			      (subseq (pathname-directory (car s))
				      (1+ (position "src" (pathname-directory (car s)) :test 'equal)))
			      (pathname-name (car s))
			      (pathname-type (car s)))))))
	  (when new
	    (setf (car s) (translate-logical-pathname new)))
	  )))))

'(when (gethash "ABCL-SRC" SYSTEM:*LOGICAL-PATHNAME-TRANSLATIONS*)
  (fix-abcl-src-pointers))