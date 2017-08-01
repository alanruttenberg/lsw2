(defun get-vagrant-box-id (name)
  (let ((answer (run-program-string->string "vagrant" '("global-status") "")))
    (let ((found (caar (all-matches answer (format nil "(?si)([a-f0-9]*)\\s (~a)" name) 1 ))))
      (assert found  (name) "Couldn't find vagrant box named ~a" name)
      found)))

(defun get-vagrant-box-info ()
  (let ((debug-info (second (multiple-value-list (run-program-string->string "vagrant" (list "global-status" "--debug") "")))))
    (loop for match in 
		    (all-matches debug-info
				 " INFO interface: info: ([a-f0-9]+)\\s*INFO interface: info: (\\S+)\\s*INFO interface: info: (\\S+)\\s*INFO interface: info: (\\S+)\\s*INFO interface: info: (\\S+)" 1 2 3 4 5)
	  for (id name provider status wd) = match
	  collect
	  `(:id ,id :name ,name :provider ,provider :status ,status :wd ,wd))))

(defun get-vagrant-box-wd (id-or-name)
  (let ((info (get-vagrant-box-info)))
    (getf (find-if (lambda(e) (or (equalp (getf e :id) id-or-name) (equalp (getf e :name) id-or-name))) info) :wd)))

(defun get-vagrant-box-status (id-or-name)
  (let ((info (get-vagrant-box-info)))
    (getf (find-if (lambda(e) (or (equalp (getf e :id) id-or-name) (equalp (getf e :name) id-or-name))) info) :status)))

(defun vagrant-box-up (id)
  (run-program-string->string  "vagrant" `("up" ,id) "" :wd (get-vagrant-box-wd id))
  (assert (equalp (get-vagrant-box-status id) "running") (id) "Failed to bring up vagrant box ~a" id))
  
  
  
(defun vagrant-tip ()
  (format t "Install or upgrade virtualbox: brew cask [re]install virtualbox~%")
  (format t "Download from https://www.vagrantup.com/downloads.html~%")
  (format t "Install virtualbox guest addition dwim: vagrant plugin install vagrant-vbguest~%")
  (format t "Name your box by adding in your Vagrantfile: config.vm.define :yourBoxName")
  )
	  
