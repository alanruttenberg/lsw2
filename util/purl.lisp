;; functions for interacting with purl.org

;; e.g. (create-new-purl "/NET/sandbox/1" "http://foo.com/" "alanruttenberg" "xxxxxx" '(melanie cmungall))
;; add optional last argument t to make it partial. In which case purl must end in a "/"
;; returns t if successful


(defun create-new-purl (purl url user password maintainers &optional (partial nil))
  (let* ((answer 
	  (get-url "http://purl.oclc.org/maint/new.pl.cgi"
		   :force-refetch t
		   :dont-cache t
		   :persist nil
		   :post `(("confirm" "true")
			   ("id" ,user)
			   ("password" ,password)
			   ("purl" ,purl)
			   ("url" ,url)
			   ("inst" ,(format nil "~a ~{~a~^ ~}" user maintainers))
			   ,@(if partial (list (list "partial" "on")))
			   ))))
    (#"matches" answer "(?s).*Added:.*")))

(defun split-into-sized-lists (list count)
  (let ((counter -1) (accum nil) (all nil))
    (loop while list
	 do
	 (loop while list
	    while (< (incf counter) count)
	    do (push (pop list) accum))
	 (push accum all)
	 (setq counter -1 accum nil))
    all))

;http://pir.georgetown.edu/cgi-bin/pro/entry_pro?id=PRO:000000009
(defun create-new-purls (purls user password maintainers)
  (let ((batches (split-into-sized-lists purls 90)))
    (loop for batch in batches
       for count from 1
       do
       (with-output-to-string  (s)
	 (write-string "<recs>" s)
	 (loop for (purl url) in batch
	    do
	    (format s "<rec><purl>~a</purl><url>~a</url>~{<id>~a</id>~}<type>User_Batch_Add</type></rec>" 
		    purl url maintainers))
	 (write-string "</recs>" s)
	 (let* ((answer 
		 (get-url "http://purl.oclc.org/maint/batch.pl.cgi"
			  :force-refetch t
			  :dont-cache t
			  :persist nil
			  :post `(("id" ,user)
				  ("password" ,password)
				  ("list" ,(get-output-stream-string s))
				  ))))
	   (format t "Batch ~a ~a~%" count (#"matches" (print answer) "(?s).*Added:.*")))))))

;; (update-purl "/NET/sandbox/1" "http://bar.com/" "alanruttenberg" "xxxxxx" '(melanie cmungall) "made a mistake")
;; returns (list purl url), if successful

(defun update-purl (purl url user password maintainers comment)
  (let* ((answer 
	  (get-url "http://purl.oclc.org/maint/modify.pl.cgi"
		   :force-refetch t
		   :dont-cache t
		   :persist nil
		   :post `(("confirm" "true")
			   ("id" ,user)
			   ("password" ,password)
			   ("purl" ,purl)
			   ("url" ,url)
			   ("inst" ,(format nil "~a ~{~a~^ ~}" user maintainers))
			   ("public" ,comment)
			   ))))
    (car (all-matches answer "(?s).*Replaced:\\s*<a href=\"(.*?)\">(.*?)</a>\\s*=&gt; (.*?)\\s+" 1 3))))
    

;; returns (list purl url partial? . maintainers )
;; (get-purl "/obo/obi.owl") ->
;; ("http://purl.oclc.org/obo/obi.owl"
;;  "http://obi.svn.sourceforge.net/svnroot/obi/tags/releases/2008-05-30/merged/OBI.owl" nil
;;  "MELANIE" "CMUNGALL" "ALANRUTTENBERG")
(defun get-purl (purl)
  (let* ((answer 
	  (get-url (format nil "http://purl.org/maint/display.pl.cgi?purlreg=~a&url=&maint=&inst=&noedit=on&id=nobody" purl)
		   :force-refetch t
		   :dont-cache t
		   :referer "http://purl.org/maint/display.html"
		   :persist nil)))
    (let ((did (all-matches  
		answer
		"(?s)PURL\\s*</b><a href=\"(.*?)\">.*?</a>\\s*<b>URL\\s*</b><a href=\".*?\">(.*?)</a>.*?Maintainers\\s+</b>(.*?)\\n" 1 2 3)))
      (and (car did)
	   (list* (first (car did)) (second (car did)) (#"matches" answer "(?s).*Partial Redirection\\s*</b>Enabled.*")
		   (split-at-regex (third (car did)) ",\\s*"))))))

(defun purls-matching (string &key start (refetch nil))
  (let* ((answer 
	  (get-url (format nil "http://purl.org/maint/display.pl.cgi?purlreg=~a&url=&maint=&inst=&noedit=on&id=nobody&start=~a" string (or start ""))
		   :force-refetch refetch
		   :referer "http://purl.org/maint/display.html"
		   :persist nil)))
    (let ((did (all-matches  
		answer
		(format nil "(?s)(?i)cookie=\">(.*?~a.*?)</a></td><td>.*?<\/td><td>(.*?)<\/td>" string) 1 2)))
      (if (all-matches answer "(?s)(Next records)" 1)
	  (append did (purls-matching string :start (+ (or start 0) 25)))
	  did))))


;; (create-purls '(("/NET/sandbox/5" "http://example.com/3")
;; 		("/NET/sandbox/8" "http://example.com/8"))
;; 	      "alanruttenberg" "xx" nil nil)
;; =>
;; (values purls-created purls-with-errors)

(defun create-purls (purl-url-pairs user password maintainers &optional (partial nil))
  (let* ((ids (format nil "<id>~a</id>~{<id>~a</id>~}" user maintainers))
	 (partial (if partial "<partial></partial>" ""))
	 (request
	  (with-output-to-string (s)
	    (write-string "<recs>" s)
	    (loop for (purl url) in purl-url-pairs
	       do (format s "<rec><purl>~a</purl><url>~a</url>~a~a</rec>"
			  purl url ids partial))
	    (write-string "</recs>" s)))
	 (answer 
	  (get-url "http://purl.oclc.org//maint/batch.pl.cgi"
		   :force-refetch t
		   :dont-cache t
		   :persist nil
		   :post `(("id" ,user)
			   ("password" ,password)
			   ("list" ,request)
			   ("add" "add")))))
    (values (mapcar 'car (all-matches answer "(?si)Added:\s*.*?>(.*?)<" 1))
	    (mapcar 'car (all-matches answer "Error:.*?>(.*?)<" 1)))))

;; (modify-purls '(("/NET/sandbox/5" "http://example.com/3")
;; 		   ("/NET/sandbox/8" "http://example.com/8"))
;; 	      "alanruttenberg" "xx" nil nil  "because")
;; =>
;; (values purls-created purls-with-errors)

(defun modify-purls (purl-url-pairs user password maintainers &optional (partial nil) (note ""))
  (let* ((ids (format nil "<id>~a</id>~{<id>~a</id>~}" user maintainers))
	 (partial (if partial "<partial></partial>" ""))
	 (request
	  (with-output-to-string (s)
	    (write-string "<recs>" s)
	    (loop for (purl url) in purl-url-pairs
	       do (format s "<rec><purl>~a</purl><url>~a</url>~a~a<type>Batch_Modified</type><note>~a</note></rec>"
			  purl url ids partial note))
	    (write-string "</recs>" s)))
	 (answer 
	  (get-url "http://purl.oclc.org//maint/batch.pl.cgi"
		   :force-refetch t
		   :dont-cache t
		   :persist nil
		   :post `(("id" ,user)
			   ("password" ,password)
			   ("list" ,request)
			   ("replace" "replace")))))
    (values (mapcar 'car (all-matches answer "(?si)Replaced:\s*.*?>(.*?)<" 1))
	    (union
	     (mapcar 'car (all-matches answer "Error:(?=Skip).*?>(.*?)<" 1))
	     (mapcar (lambda(e) (format nil "http://purl.oclc.org~a" (car e)))
		     (all-matches answer "Error:.*?Skipping PURL \\((.*?)\\)" 1))))))

;;(get-purl-group "obi")
;; -> values
;; ("ALANRUTTENBERG" "CMUNGALL" "MELANIE")
;; ("ALANRUTTENBERG" "CMUNGALL" "MELANIE" "JAR287")
;; "for managing the purls of the obi project http //purl.obofoundry.org/obo/obi"

(defun get-purl-group (name)
  (let* ((answer 
	  (get-url (format nil "http://purl.org/maint/search_group.pl.cgi?groupid=~a&groupown=&groupmem=&noedit=on&id=nobody" name)
		   :force-refetch t
		   :dont-cache t
		   :persist nil)))
    (flet ((userlist (header)
	     (mapcar 'car
		     (all-matches
		      (caar
		       (all-matches a (format nil "(?s)~a:\\s*</b>((\\s*<a.*?>.*?</a>)*)" header) 1 ))
		      "\\s*<a.*?>(.*?)</a>*" 1))))
      (values
       (userlist "Maintainers")
       (userlist "Members")
       (caar (all-matches answer "(?s)Comment:\\s*</b>\\s*(.*?)\\s*</pre>" 1))))))




