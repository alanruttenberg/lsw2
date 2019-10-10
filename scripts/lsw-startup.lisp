(setf (logical-pathname-translations "lsw")
      `(("**;*.*" ,(make-pathname :directory (append (pathname-directory *load-pathname*)
						     '(:wild-inferiors))
				  :name :wild
				  :type :wild
				  :defaults *load-pathname*))))

(require :quicklisp-abcl)

(unless (member 'ql:quickload sys::*module-provider-functions*)
  (setq sys::*module-provider-functions*  (append sys::*module-provider-functions*  '(ql:quickload))))

(push (make-pathname :directory (butlast (pathname-directory *load-pathname*))) asdf/find-system:*central-registry*)

;; (unless (quicklisp-client:where-is-system "winston-forward-chain")
;;   (restart-case
;;       (error "LSW needs winston-ai code: cd ~a; git clone https://github.com/alanruttenberg/winston-ai.git" 
;; 	     (car quicklisp-client:*local-project-directories*))
;;     (error ()
;;       :report "Try to clone winston-ai into quicklisp local project directory."
;;       (sys::run-program "git" '("clone" "https://github.com/alanruttenberg/winston-ai.git") :directory (car quicklisp-client:*local-project-directories*))
;;       (progn (sleep 5)))))
  
(flet ((load ()
	 (ql:quickload 'owl2 :verbose nil)
	 (ql:quickload 'inspect :verbose nil)))
  (if *load-verbose*
      (load)
      (let ((ext:*suppress-compiler-warnings* (not *load-verbose*))
	    (*compile-verbose* *load-verbose*))
	(uiop/utility:with-muffled-conditions ((list 'style-warning))
	  (load)))))
