;; Generalize running a process in a variety of ways: locally, using vagrant, using docker, using ssh, etc.

(defclass executable-environment ()())

(defgeneric run-program ((env executable-environment) executable args &key input wd &allow-other-keys)
  (:documentation "Run program. If input is a string, use it as stdin. Arguments are generally strings, but for  file arguments there is optional extra syntax for remote environments. (:in local-path remote-path) makes the file at in-path available on the remote as remote-path and passes that when running the program. (:out remote-path local-path) makes the file at remote-path, after running program, available at local path. If, in either case of :in or :out only one 
If remote path is a directory then local path needs to be a directory, and all files in the directory are maked available at local-path. remote-path is the argument to the program. If wd is passed the working directory in the executable environment is set to it."))

    
(defclass execute-locally (executable-environment) ())

(defmethod run-program ((env execute-locally) executable args &key input wd &allow-other-keys)
  (multiple-value-bind (args copy-input-files copy-output-files)
      (file-handler env args)
    (funcall copy-input-files)
    (let ((process (sys::run-program executable args :directory wd :input (if (pathnamep input) input :stream))))
      (unless (pathnamep input)
	(when input (write-string (or input "") (sys::process-input process)))
	(close (sys::process-input process)))
      (let ((output (with-output-to-string (s)
		      (loop for (line missing-newlinep) = (multiple-value-list (read-line (sys::process-output process) nil :eof))
			    until (eq line :eof)
			    do  (if missing-newlinep (write-string line s) (write-line line s)) ))))
	(let ((error (unless (eq (peek-char nil (sys::process-error process) nil :eof) :eof)
		       (with-output-to-string (s)
			 (loop for (line missing-newlinep) = (multiple-value-list (read-line (sys::process-error process) nil :eof))
			       until (eq line :eof)
			       do (if missing-newlinep (write-string line s) (write-line line s)))))))
	  (funcall copy-output-files)
	  (values output error))))))

(defmethod file-handler ((env execute-locally) args)
  (loop for arg in args
	with file-args = nil
	if (stringp arg) collect arg into args
	  else
	    if (pathnamep arg) collect (namestring (translate-logical-pathname arg)) into args
	      else
		collect 
		   (ecase (car arg)
		     (:in (if (third arg)
			      (progn 
				(push arg file-args)
				(namestring (third arg)))
			      (namestring (second arg))))
		     (:out (if (third arg)
			       (progn
				 (push arg file-args)
				 (namestring (second arg)))
			       (namestring (second arg)))))
	  into args
	  and do
	    (assert (pathname (second arg)) () "~a from ~a can't be made into a pathname" (second arg) arg)
	    (assert (or (null (third arg)) (pathname (third arg))) () "~a from ~a can't be made into a pathname" (third arg) arg)
	finally
	   (let ((file-args (print-db file-args)))
	     (return (values (print-db args)
			     (lambda()
			       (loop for (direction source dest) in file-args
				     when (eq direction :in)
				       do (uiop/stream:copy-file source dest)))
			     (lambda()
			       (loop for (direction source dest) in file-args
				     when (eq direction :out)
				       do (uiop/stream:copy-file source dest))))))))
		 
  
    


(defclass remote-executable-environment (executable-environment)())

(defgeneric string->file ((env remote-executable-environment) string filename))
(defgeneric file->file ((env remote-executable-environment) local-path remote-path))

(defclass execute-via-ssh (remote-executable-environment) 
  ((host :accessor ssh-host :initarg host)))

(defclass execute-via-web-service (remote-executable-environment) 
  ((host :accessor ssh-host :initarg host)))

(defclass virtualized-executable-environment (remote-executable-environment)())

(defgeneric maybe-start ((env virtualized-executable-environment)))

(defgeneric maybe-stop ((env virtualized-executable-environment)))

(defclass execute-in-vagrant-box (virtualized-executable-environment)
  ((box :accessor vagrant-box :initarg :box)))

(defclass execute-docker-image (virtualized-executable-environment execute-locally) 
  ((image :accessor docker-image :initarg :image)))

;; about 1 second overhead on my laptop
(defmethod run-program ((env execute-docker-image) executable args &key input wd &allow-other-keys)
  (call-next-method env "docker" (list* "run" "-i" "-a" "stdin" "-a" "stdout" "-a" "stderr" (docker-image env) executable args) :input input :wd wd))

;; cases for file arguments
;; (:in ...) -> mount volume in /echoed and tranform file name
;; (:out ...) -> mount volume in /echoed and tranform file name (what if you want to refer to an actual output path in remote? can't think of sensible case) (what if :out is a directory? still fine)

;; only handle one arg :in :out, implement with volume mounting
(defmethod file-handler ((env execute-docker-image) args)
  (loop for arg in args
	with file-args = nil
	if (stringp arg) collect arg into args
	  else
	    if (pathnamep arg) collect (namestring (translate-logical-pathname arg)) into args
	      else
		collect 
		   (ecase (car arg)
		     (:in (push arg file-args) (third arg))
		     (:out (push arg file-args) (second arg)))
	  into args
	  and do
	    (assert (pathname (second arg)) () "~a from ~a can't be made into a pathname" (second arg) arg)
	    (assert (pathname (third arg)) () "~a from ~a can't be made into a pathname" (third arg) arg)
	finally
	   (let ((file-args (print-db file-args)))
	     (return (values (print-db args)
			     (lambda()
			       (loop for (direction source dest) in file-args
				     when (eq direction :in)
				       do (uiop/stream:copy-file source dest)))
			     (lambda()
			       (loop for (direction source dest) in file-args
				     when (eq direction :out)
				       do (uiop/stream:copy-file source dest))))))))

;; for single-arg :in and :out, and pathname objects, collect the directories, which will be mounted on the remote host
(defun mount-directories-from-args (args)
  (loop for arg in args
	if (pathnamep arg)
	  collect (make-pathname
		   :directory (pathname-directory (if (probe-file arg)
						      (truename arg)
						      (translate-logical-pathname arg))))
	else if (and (listp arg) (= (length arg) 2) (member (car arg) '(:in :out)))
	       collect (directory-namestring
			(if (probe-file (second arg))
			    (truename (second arg))
			    (translate-logical-pathname (second arg))))))

(defclass execute-in-persistent-docker-container (virtualized-executable-environment execute-locally)  
  ((container :accessor docker-container :initarg :container :initform nil)
   (image  :accessor docker-image :initarg :image)))

(defconstant *docker-ps-headers* '("CONTAINER ID" "IMAGE" "COMMAND" "CREATED" "STATUS" "PORTS" "NAMES"))
(defconstant *docker-ps-keys* (mapcar (lambda(e) (intern (substitute #\- #\space e) :keyword)) *docker-ps-headers*))

(defun docker-ps ()
  (let ((info (run-program-string->string "docker" '("ps") "")))
    (let ((lines (split-at-char info #\newline)))
      (let ((headers (loop for header-line = (car lines)
			   for (header next) on *docker-ps-headers*
			   collect (list (search header header-line :test 'char=) (if next (search next header-line :test 'char=))))))
	(let ((data (loop for line in (cdr lines)
			  collect
			  (loop for (start end) in headers
				for key in *docker-ps-keys*
				collect
				(list key (if (eq key :command)
				    (read-from-string  (subseq line start end))
				    (string-trim '(#\space) (subseq line start end))))))))
	  data)))))

(defvar *lisp-created-docker-containers* nil)

(defun maybe-kill-lisp-created-docker-containers ()
  (when *lisp-created-docker-containers*
    (loop for container-info in (docker-ps)
	  for id = (second (assoc :container-id container-info))
	  when (some (lambda(created-id) (#"matches" created-id (format nil ".*~a" id))) *lisp-created-docker-containers*)
	    do (run-program-string->string "docker" `("kill" ,id) ""))))

(pushnew  'maybe-kill-lisp-created-docker-containers ext:*exit-hooks*)

(defmethod maybe-start ((env execute-in-persistent-docker-container))
  (unless (docker-container env)
    (let ((running (second (assoc :container-id (find (docker-image env) (docker-ps) :key (lambda(e) (second (assoc :image e))) :test 'equalp)))))
      (if running
	  (setf (docker-container env) running)
	  (progn
	      (setf (docker-container env) 
		    (string-trim '(#\newline) (run-program-string->string "docker" (list "run" "-td" (docker-image env)) "")))
	     (push (docker-container env) *lisp-created-docker-containers*)
	     )))))

;; about 1/10 second if container is running
(defmethod run-program ((env execute-in-persistent-docker-container) executable args &key input wd &allow-other-keys)
  (maybe-start env)
  (call-next-method env "docker" (list* "exec" "-i" (docker-container env) executable args) :input input :wd wd))

;; update docker for mac to use criu
;;  https://hub.docker.com/r/boucher/criu-for-mac/
;; when you get a complaint  like this:
;; Error response from daemon: Cannot checkpoint container priceless_wilson: rpc error: code = 2 desc = exit status 1: "criu failed: type NOTIFY errno 0\nlog file: /var/lib/docker/containers/90e3e4802ca70b5ab986b7ae9bde4d4c844436ab9eb6890871f92e27c7fc8943/checkpoints/c3/criu.work/dump.log\n"
;; that dump.log file is in the base, not your image.
;; You can see it with (run-in-dockers-base "/bin/cat" '("/var/lib/docker/containers/90e3e4802ca70b5ab986b7ae9bde4d4c844436ab9eb6890871f92e27c7fc8943/checkpoints/c3/criu.work/dump.log"))
;; https://github.com/jpetazzo/nsenter
;; hint at https://github.com/docker/for-mac/issues/371#issuecomment-329000788
;;
;; Better: use this to get a prompt in the docker vm.  screen ~/Library/Containers/com.docker.docker/Data/com.docker.driver.amd64-linux/tty 

(defun run-in-dockers-base (command args)
  (run-program-string->string "docker" `("run" "--rm" "--net=host" "--pid=host" "--privileged" "-i" "justincormack/nsenter1" ,command ,@args) nil))


;; to create a checkpoint LSW that can be restarted
;; docker run -id lsw2/checkpointable /home/lsw/repos/lsw2/bin/lsw
;; wait until loaded
;; docker ps to get container id e.g. 6025031e1867 (or it is output of run)
;; you can docker attach and then do eof to get out of the attachment.
;; docker checkpoint create <container-id> checkpoint-name creates the checkpoint
;; docker start --checkpoint checkpoint-name container-id
;; docker attach as above
;; If your original run started with -t then the checkpoint creation will fail
;; You can run something in the repl with
;; echo '(print (+ 1 3))' | docker attach <container id> ; the caveat is the output seems to be lost. It does appear in the log: docker logs <container id>

(defun checkpoints-for-docker-image (image)
  (let ((containers
	    (remove "" (split-at-char (run-program-string->string "sh" (list "-c"
								   (concatenate 'string
										"docker ps -a --filter \"ancestor="
										image "\"  | tail -n +2 | cut -f 1,1 -d \" \" ")) "")
				      #\newline) :test 'equalp)))
    (loop for container in containers
	  for run = (remove "" (split-at-char (run-program-string->string
		     "sh"
		     (list "-c"
			   (format nil "docker checkpoint ls ~a  | tail -n +2 | cut -f 1,1 -d \" \"" container))
		     "") #\newline) :test 'equalp)
	  when run collect (list* container  run))))


  
  (remove "" (split-at-char (run-program-string->string "sh" (list "-c"
								   (concatenate 'string
										"docker checkpoint ls `docker ps -a --filter \"ancestor="
										image "\"  | tail -n +2 | cut -f 1,1 -d \" \"` ")) "") #\newline) :test 'equalp))
  
