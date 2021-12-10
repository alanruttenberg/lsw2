(in-package :system)

(defun run-applescript (&rest script-parts)
  "Each script part is either a (potentially multi-line) string, or a list with the car being a format string and the rest format arguments"
  #-:os-macosx (error "Applescript only runs on macOS")
  (let ((split-script (mapcan (lambda(s)
				(if (null s)
				    nil
				    (cl-user::split-at-char (if (listp s)
							    (apply 'format nil s)
							    s)
							#\newline)) )
			      script-parts)))
    (#"exec" (#"getRuntime" 'java.lang.Runtime)
	     (java::jarray-from-list
	      (cons "/usr/bin/osascript" (mapcan (lambda(e) (list "-e" e)) split-script))))))

(defun run-applescript-return-result (&rest script-parts)
  "Each script part is either a (potentially multi-line) string, or a list with the car being a format string and the rest format arguments"
  #-:os-macosx (error "Applescript only runs on macOS")
  (let ((split-script (mapcan (lambda(s)
				(if (null s)
				    nil
				    (cl-user::split-at-char (if (listp s)
							    (apply 'format nil s)
							    s)
							#\newline)) )
			      script-parts)))
    (cl-user::run-program-string->string "/usr/bin/osascript" (mapcan (lambda(e) (list "-e" e)) split-script))))

(defun notify (string &key (title "Armed Bear Common Lisp") (subtitle nil) (sound nil))
  "Use the system notification service to display a message"
  #+:os-macosx
  (run-applescript
   `("display notification ~s with title ~s~a~a"
     ,string ,title
     ,(if subtitle (format nil " subtitle ~s" subtitle) "")
     ,(if sound (format nil " sound name ~s" sound) "")) 
   )
  #+:os-macosx (format *trace-output* "~&~a~a~a~%" 
		       (if subtitle subtitle "")
		       (if subtitle ": " "")
		       string)
  string)

;; (notify "hello world" :subtitle "and it was said:")
  

