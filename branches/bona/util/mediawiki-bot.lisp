; See: http://en.wikipedia.org/wiki/Wikipedia:Creating_a_bot

(defclass mediawiki-bot ()
  ((user :accessor user :initarg :user )
   (pass :accessor pass :initarg :pass )
   (base-url :accessor base-url :initarg :url )
   (api-url :accessor api-url)
   (cookie :accessor cookie :initarg :cookie)
   (pages :accessor pages :initarg :pages )
   (users :accessor users :initarg :users )
   (login-result :accessor login-result)
   (api-token :accessor api-token)))

(defmethod print-object ((b mediawiki-bot) stream)
  (let ((*print-case* :downcase))
    (print-unreadable-object (b stream :type t)
      (format stream "~a@~a" (user b) (base-url b)))))

(defmethod initialize-instance ((b mediawiki-bot) &rest args &key url user pass )
  (call-next-method)
  (setf (api-url b) (format nil "~aapi.php" (base-url b)))
  (login-via-api b url user pass))

;; works
(defmethod login-via-api ((b mediawiki-bot) url user pass)
  (multiple-value-bind (result headers)
      (get-url (api-url b)
	       :post `(("action" "login") ("lgname" ,user) ("lgpassword" ,pass) ("format" "xml") ,@(if (slot-boundp b 'api-token) `(("lgtoken" ,(api-token b)))))
	       :force-refetch t)
    (setf (cookie b)
	  (mapcar (lambda(e) (#"replaceAll" e ";.*" "")) (cdr (assoc "Set-Cookie" headers :test 'equal))))
    (let ((returned (attribute-named (find-element-with-tag (nastybot-xml-parse result) "login") "result")))
      (if (equalp returned "NeedToken")
	  (progn
	    (assert (not (boundp '*mediawiki-getting-token*)) () "Login token acquisition loop")
	    (let ((*mediawiki-getting-token* t)
		  (*cookies* (cookie b)))
	      (declare (special *mediawiki-getting-token*))
	      (let ((token (attribute-named (find-element-with-tag (nastybot-xml-parse result) "login") "token")))
		(assert token () "Didn't get a token!")
		(setf (api-token b) token)
		(login-via-api b url user pass))))
	  (assert (equalp returned "Success") () (format nil "Failed to log in: ~a" result))))))


;; backup
(defmethod login-by-form ((b mediawiki-bot) url user pass)
  (with-cookies-from (get-url (format nil "~aindex.php?title=Special:UserLogin" url) :force-refetch t)
    (with-cookies-from (get-url (format nil "~aindex.php?title=Special:UserLogin&action=submitlogin&type=login" url)
				:post `(("wpName" ,user) ("wpPassword" ,pass) ("wpLoginattempt" "Log+in"))
				:follow-redirects nil :force-refetch t)
      (setf (cookie b) (remove "delete" *cookies* :test 'search)))))
  

(defmethod format-url ((b mediawiki-bot) action &rest pairs)
  (clean-uri
   (#"replaceFirst" (base-url b) "http://([^/]+?)/.*" "$1")
  (format nil "~aapi.php"
	  (#"replaceFirst" (base-url b) "http://[^/]+?(/.*)" "$1"))
  "http"
  nil
  (format nil "action=~a&~{~a=~a~^&~}" action pairs) t))


(defmethod get-url-update-session ((b mediawiki-bot) &rest get-url-args)
  (let ((*cookies* (cookie b)))
    (multiple-value-bind (res headers)
	(apply 'get-url (append get-url-args (list :force-refetch t)))
      (let ((cookie (find "_session=" (cdr (assoc "Set-Cookie" headers :test 'equal)) :test 'search)))
	(when cookie
	  (setf (cookie b) (cons (#"replaceFirst" cookie ";.*" "") (remove "_session=" (cookie b) :test 'search)))))
      (let ((error (cdr (assoc "MediaWiki-API-Error" headers :test 'equal))))
	(assert (null error) () (format nil "MediaWiki-API-Error: ~a" (car error))))
      res
      )))

(defmethod query ((b mediawiki-bot) &rest args)
  (nastybot-xml-parse
   (get-url-update-session b (apply 'format-url b "query" "format" "xml" "aplimit" "500" args))
   ))
  
(defmethod pages :around ((b mediawiki-bot))
  "Return a-list of name, pageid"
  (or (and (slot-boundp b 'pages) (slot-value b 'pages))
      (setf (pages b)
	    (mapcar (lambda(e) (list (attribute-named  e "title") (attribute-named  e "pageid")))
		    (find-elements-with-tag  (query b  "list" "allpages") "p")))))

(defmethod get-edit-token ((b mediawiki-bot) page)
  (let ((result (query b "prop" "info" "titles" page "intoken" "edit")))
    (let ((edittoken (attribute-named (find-element-with-tag  result "page") "edittoken")))
      (assert edittoken () (format nil "No edit token returned~%~a" result))
      (values 
       edittoken
       (attribute-named (find-element-with-tag  result "page") "starttimestamp")
       ))))

(defmethod set-page-text ((b mediawiki-bot) page text)
  (let ((token (get-edit-token b page)))
    (nastybot-xml-parse (get-url-update-session b (api-url b)
					:post `(("action" "edit") ("title" ,page) ("text" ,text) ("token" ,token) ("format" "xml"))
					))))

(defmethod append-page-text ((b mediawiki-bot) page text)
  (let ((token (get-edit-token b page)))
    (nastybot-xml-parse (get-url-update-session b (api-url b) 
					:post `(("action" "edit") ("title" ,page) ("appendtext" ,text) ("token"  ,token) ("format" "xml"))))))

(defmethod raw-page-content ((b mediawiki-bot) page)
  (let ((result (query b "prop" "revisions" "titles" page "rvprop" "content")))
    (third (find-element-with-tag result "rev"))))


(defun nastybot-xml-parse (s)
  (if (not (char= (char s 0) #\<)) (xmls:parse (subseq s (position #\< s))) (xmls:parse s)))
