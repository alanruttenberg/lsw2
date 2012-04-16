(in-package :cl-user)

(defvar *page-cache* (make-hash-table :test 'equal))

(defvar *cookies* nil)

(defun head-url (url)
  (get-url url :head t))

(defun post-url-xml (url message)
  (get-url url :post message))

(defun get-url (url &key post (force-refetch  post) (dont-cache post) (persist (not post)) cookiestring nofetch verbose tunnel referer (follow-redirects t) 
		(ignore-errors nil) head accept extra-headers (appropriate-response (lambda(res) (and (numberp res) (>= res 200) (< res 400)))) verb
		&aux headers)
  "Get the contents of a page, saving it for this session in *page-cache*, so when debugging we don't keep fetching"
  (sleep 0.0001)			; give time for control-c
  (if (not (equal verb "GET"))
      (setq force-refetch t))
  (and head
       (setq force-refetch t dont-cache t persist nil follow-redirects nil))
  (or (and (not force-refetch) (gethash url *page-cache*))
      (and (not force-refetch) (probe-file (url-cached-file-name url))
	   (get-url-from-cache url))
      (and nofetch
	   (error "Didn't find cached version of ~a" url))
      (labels ((stream->string (stream)
		 (let ((buffer (jnew-array "byte" 4096)))
		   (apply 'concatenate 'string
			  (loop for count = (#"read" stream buffer)
				while (plusp count)
				do (sleep 0.0001)
				collect (#"toString" (new 'lang.string buffer 0 count))
				))))
	     (doit()
	       (when verbose (format t "~&;Fetching ~s~%" url))
	       (let ((connection (#"openConnection" (new 'java.net.url (maybe-rewrite-for-tunnel url tunnel))))
		     )
		 (if follow-redirects
		   (#"setInstanceFollowRedirects" connection t)
		   (#"setInstanceFollowRedirects" connection nil))
		 (when (or *cookies* cookiestring)
		   (#"setRequestProperty" connection "Cookie" (join-with-char (append *cookies* cookiestring) #\;)))
		 (when referer
		   (#"setRequestProperty" connection "Referer" referer))
		 (#"setRequestProperty" connection "User-Agent" "Mozilla/4.0 (compatible)")
		 (when verb (#"setRequestMethod" connection verb))
		 (when accept
		   (#"setRequestProperty" connection "Accept" accept))
		 (loop for (key value) in extra-headers
		      do (#"setRequestProperty" connection key value))
		 (when post
		   (#"setRequestMethod" connection "POST")
		   (#"setDoOutput" connection t)
		   (if (consp post)
		       (with-output-to-string (s)
			 (loop for (prop value) in post
			    do (format s "~a=~a&" prop (#"encode" 'java.net.URLEncoder (coerce value 'simple-string) "UTF-8")))
			 (setq post (get-output-stream-string s))
			 (setq post (subseq post 0 (- (length post) 1)))
			 (#"setRequestProperty" connection "Content-Type" "application/x-www-form-urlencoded"))
		       (#"setRequestProperty" connection "Content-Type" "text/xml"))
		   (let ((out (new 'PrintWriter (#"getOutputStream" connection))))
		       (#"print" out post)
		       (#"close" out)))
		 (when head
		   ;(#"setRequestMethod" connection "HEAD")
		   (let ((responsecode (#"getResponseCode" connection)))
		     (if (not (funcall appropriate-response responsecode))
			 (let ((errstream (#"getErrorStream" connection)))
			   (error "Bad HTTP response ~A: ~A" responsecode (if errstream (stream->string errstream) "No error stream"))) 
			 (return-from get-url (unpack-headers responsecode (prog1 (#"getHeaderFields" connection) (#"disconnect" connection)))))))
		 (setq headers (#"getHeaderFields" connection))
		 (let ((responsecode (#"getResponseCode" connection)))
		   (if (not (funcall appropriate-response responsecode))
		       (let ((errstream (#"getErrorStream" connection)))
			 (error "Bad HTTP response ~A: ~A" responsecode (if errstream (stream->string errstream) "No error stream"))) 
		       (let ((stream (ignore-errors (#"getInputStream" connection))))
			 (if (and (member responsecode '(301 302 303)) follow-redirects)
			     (progn (setq url (second (assoc "Location" (unpack-headers responsecode headers) :test 'equal)))
				    (doit))
			     (ignore-errors (stream->string stream)))

			 ))))))
	(if ignore-errors
	    (multiple-value-bind (value errorp) (ignore-errors (doit))
	      (if errorp
		  (progn
		    (when verbose (format t "~a" (java-exception-message errorp)))
		    (values (list :error errorp (java-exception-message errorp)) (unpack-headers nil headers)))
		  (progn
		    (if persist (save-url-contents-in-cache url value) value)
		    (if dont-cache
			(values value (unpack-headers nil headers))
			(values (setf (gethash url *page-cache*) value) (unpack-headers nil headers))))))
	    (progn
	      (let ((value (doit)))
		(if persist (save-url-contents-in-cache url value) value)
		(if dont-cache
		    (values value (unpack-headers nil headers))
		    (values (setf (gethash url *page-cache*) value) (unpack-headers nil headers)))))
	    ))))

(defun persist-page-cache ()
  (maphash
   (lambda(k v)
     (unless (probe-file (url-cached-file-name v))
       (save-url-contents-in-cache k v)))
   *page-cache*))


(defun header-value (header headers)
  (cadr (assoc header headers :test 'equal)))

(defun maybe-rewrite-for-tunnel (url tunnel)
  (if tunnel
      (destructuring-bind (protocol  path)
	  (car (all-matches url "^([a-z]*)://[^/]*(.*)" 1 2))
	(concatenate 'string protocol "://" tunnel path))
      url))

(defun unpack-headers (response headers)
  (append
   (if response
       (list (list "response-code" response))
       nil)
   (and headers

	(loop 
	   for key in (set-to-list (#"keySet" headers))
	   for value = (#"get" headers key)
	   when value
	   collect (cons key
			 (loop for i below (#"size" value)
			    collect (#"get" value i)))
	   ))))
	
    

#|(defun get-url (url &key force-refetch dont-cache persist)
  "Get the contents of a page, saving it for this session in *page-cache*, so when debugging we don't keep fetching"
  (or (and (not force-refetch) (gethash url *page-cache*))
      (and persist (probe-file (url-cached-file-name url))
	   (get-url-from-cache url))
      (multiple-value-bind (value errorp) 
	  (ignore-errors 
	    (let ((stream (#"openStream" (new 'net.url url)))
		  (buffer (jnew-array "byte" 4096)))
	      (apply 'concatenate 'string
		     (loop for count = (#"read" stream buffer)
			while (plusp count)
			collect (#"toString" (new 'lang.string buffer 0 count))
			))))
	(if errorp
	    (progn
	      (list :error (java-exception-message errorp)))
	    (if dont-cache
		value
		(setf (gethash url *page-cache*) (save-url-contents-in-cache url value)))))))|#


(defun cache-url ())

(defun url-cached-file-name (url)
  (let ((it (new 'com.hp.hpl.jena.shared.uuid.MD5 url)))
    (#"processString" it)
    (let* ((digest (#"getStringDigest" it))
	   (subdirs (coerce (subseq digest 0 4) 'list)))
      (merge-pathnames (make-pathname :directory (cons :relative (mapcar 'string subdirs))
				      :name digest :type "urlcache") (config :web-cache)))))

(defun save-url-contents-in-cache (url content)
  (let ((fname (url-cached-file-name url)))
    (ensure-directories-exist fname)
    (with-open-file (f fname :direction :output :if-does-not-exist :create)
      (format f "~s" url)
      (write-string content f))))

(defun get-url-from-cache (url)
  (let ((fname (url-cached-file-name url)))
    (with-open-file (f fname :direction :input)
      (let ((url-saved (read f)))
	(assert (equalp url url-saved) () "md5 collision(!) ~s, ~s" url url-saved)
	(let ((result (make-string (- (file-length f) (file-position f)))))
	  (read-sequence result f)
	  result)))))

(defun forget-cached-url (url)
  (delete-file (url-cached-file-name url))
  (remhash url *page-cache*))

(defun java-exception-message (exception)
  (ignore-errors (caar (all-matches (#"toString" (slot-value exception 'system::cause)) "(?s)=+\\s*(.*?)\\n" 1))))

(defun wikipedia (term)
  "Lookup a wikipedia page by name and return it's url. If ambiguous, return :ambiguous. If missing return :missing"
  (let ((page (get-url (format nil "http://en.wikipedia.org/wiki/~a" (#"replaceAll" term " "  "_")))))
    (if (consp page)
	page
	(let ((pagename (caar (all-matches page "<title>\\s*(.*?)\\s*- Wikipedia, the free encyclopedia</title>" 1))))
	  (setq pagename (#"replaceAll" pagename " "  "_"))
	  (if (or (#"matches" pagename "(?i)disambiguation")
		  (search "Category:Disambiguation" page))
	      (values :ambiguous
		      (format nil "http://en.wikipedia.org/wiki/~a" pagename))
	      (if (search "Wikipedia does not have an article with this exact name" page)
		  (values :missing
			  (format nil "http://en.wikipedia.org/wiki/~a" pagename))
		  (format nil "http://en.wikipedia.org/wiki/~a" pagename)))))))

(defun get-url-from-google-cache (url &rest args)
  (ignore-errors
    (#"replaceFirst" 
     (get-url (format nil "http://www.google.com/search?hl=en&lr=&client=safari&rls=en&q=cache:~a&btnG=Search"   
		      (regex-replace-all "^http://"
					 (regex-replace-all "="
							    (regex-replace-all "\\?" (regex-replace-all "&" url "%26") "%3F")
							    "%3D")
					 "")))
     "(?s)(?i)<table.*?<hr>" "")))

(defun cached-url-safari (url)
  (and (probe-file (url-cached-file-name url))
       (run-shell-command (format nil "osascript -e 'tell application \"Safari\"' -e 'open \"~a\"' -e 'end tell'"
					 (substitute #\: #\/ (namestring (truename (url-cached-file-name url))))))))

(defparameter *uri-workaround-character-fixes* 
  (load-time-value
   (loop for fixme in '(#\& #\  #\( #\)  )
      collect (list (#"compile" '|java.util.regex.Pattern| (format nil "[~c]" fixme))
		    (format nil "%~2x" (char-code fixme)) fixme))))

(defun clean-uri (site path &optional (protocol "http" ) (fragment "") (query nil) (nofix nil))
  (let ((null (load-time-value (make-immediate-object nil :ref))))
    (if (eq nofix t)
	(#"toString" (new 'java.net.uri protocol site path (or query null) (or fragment null)))
	(loop for (pattern replacement) in *uri-workaround-character-fixes*
	   with uri = (#0"toString" (new 'java.net.uri protocol site path (or query null) (or fragment null)))
	   for new = 
	   (#0"replaceAll" (#0"matcher" pattern uri) replacement)
	   then
	   (#0"replaceAll" (#0"matcher" pattern new) replacement)
	   finally (return  (#"toString" new)) )
	)))

(defmacro with-cookies-from (site &body body)
  (if (and (consp site) (eq (car site) 'get-url))
      `(with-cookies-from-f (lambda() ,site) (lambda() (progn ,@body)))
      `(with-cookies-from-f ,site (lambda() (progn ,@body)))))

(defun with-cookies-from-f (site continue)
  (if (functionp site)
      (multiple-value-bind (value headers) (funcall site)
	(with-cookies-from-f headers continue))
      (if (consp site)
	  (let ((*cookies* (append *cookies* (mapcar (lambda(e) (#"replaceAll" e ";.*" "")) (cdr (assoc "Set-Cookie" site :test 'equal))))))
	    (values (funcall continue) (cadr (assoc "Location" site :test 'equal))))
	  (let ((headers (head-url site)))
	    (let ((*cookies* (append *cookies* (mapcar (lambda(e) (#"replaceAll" e ";.*" "")) (cdr (assoc "Set-Cookie" headers :test 'equal))))))
	      (values (funcall continue) (cadr (assoc "Location" headers :test 'equal))))))))

(defmacro set-cookies-from (site)
  `(set-cookies-from-f ',site ))

(defun set-cookies-from-f (site)
  (let ((headers (get-url site :persist nil :dont-cache t :force-refetch t :follow-redirects nil)))
    (setq *cookies* (mapcar (lambda(e) (#"replaceAll" e ";.*" "")) (cdr (assoc "Set-Cookie" headers :test 'equal))))
    ))
