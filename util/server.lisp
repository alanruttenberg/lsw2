(defvar *active-evil-server* nil)

;; listens on 127.0.0.1 port 6666 for a post with single parameter. Then returns (print (eval (read ...)))
;; (start-server)

;; CL-USER> (get-url "http://127.0.0.1:6666/eval" :post '(("eval" "(+ 1 3)")) :force-refetch t)
;; "4"
;; (("Content-length" "1"))

(defun start-server (&key (port 6666) (host "127.0.0.1"))
  (let* ((object (new 'java.lang.object))
	 (wrapped 
	  (jdelegating-interface-implementation  
	   (find-java-class 'com.sun.net.httpserver.HttpHandler)
	   object
	   "handle" 
	   (lambda(exchange &aux to-eval)
	     (handler-case 
		 (multiple-value-bind (value errorp)
		     (ignore-errors 
		       (let* ((is (#"getRequestBody" exchange))
			      (request
			       (cond ((equal (#"getRequestMethod" exchange) "POST")
				      (coerce (loop for code = (#"read" is) until (= code -1) collect (code-char code)) 'string))
				     ((equal (#"getRequestMethod" exchange) "GET")
				      (#"getQuery" (#"getRequestURI" exchange)))
				     (t "\"I don't understand this request\""))))
			 (setq to-eval (read-from-string (#"replaceFirst" request ".*=" "")))
			 (if (equal to-eval '(quit)) (setq to-eval '(setq *quit* t)))
			 (with-output-to-string (s) (format s "~a" (eval to-eval)))))
		   (let ((response 
			  (if errorp
			      (format nil "Error:~a" errorp)
			      value)))
		     (#"sendResponseHeaders" exchange 200 (length response))
		     (let ((os (#"getResponseBody" exchange)))
		       (#"write" os (#"getBytes" response))
		       (#"close" os))))
	       (condition (c)
		 nil)))
	   )))
    (when *active-evil-server* (#"stop" *active-evil-server* 1))
    (let ((server (#"create"  'com.sun.net.httpserver.HttpServer (new 'InetSocketAddress host port) 2)))
      (setq *active-evil-server*  server)
      (#"createContext" server "/eval" wrapped)
      (#"setExecutor" server (make-immediate-object nil :ref))
      (#"start" server)
      )))

(defun stop-server ()
  (when *active-evil-server* (#"stop" *active-evil-server* 1) (setq *active-evil-server* nil)))


;; (get-url "http://127.0.0.1:6666/eval?eval=(+%201%202)" :force-refetch t)
;; (get-url "http://127.0.0.1:6666/eval" :post '(("eval" "(+ 1 2)")) :force-refetch t)
