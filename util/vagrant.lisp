(defun get-vagrant-box-id (name)
  (let ((answer (run-program-string->string "vagrant" '("global-status") "")))
    (let ((found (caar (all-matches answer (format nil "(?si)([a-f0-9]*)\\s (~a)" name) 1 ))))
      (assert found  (name) "Couldn't find vagrant box named ~a" name)
      found)))

(defun get-vagrant-box-info ()
  (let ((debug-info (second (multiple-value-list (run-program-string->string "vagrant" (list "global-status" "--debug") "")))))
    (loop for match in 
		    (all-matches debug-info
				 " INFO interface: info: ([a-f0-9]+)\\s*INFO interface: info: (\\S+)\\s*INFO interface: info: (\\S+)\\s*INFO interface: info: (\\S+ )\\s*INFO interface: info: (\\S+)" 1 2 3 4 5)
	  for (id name provider status wd) = match
	  collect
	  `(:id ,id :name ,name :provider ,provider :status ,status :wd ,wd))))

(defun get-vagrant-wd (id-or-name)
  (let ((info (get-vagrant-box-info)))
    (getf (find-if (lambda(e) (equalp (getf e :id) id-or-name) (equalp (getf e :name) id-or-name)) info) :wd)))






