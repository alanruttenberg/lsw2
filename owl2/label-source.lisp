(defparameter *ohd-label-source* nil)

;; An easier way to choose to make a label source
;;  (make-instance 'label-source :key :obi :sources '("~/repos/obi/trunk/src/ontology/branches/obi.owl"))
;; This will make !'assay'@obi work
;; This remains compatible with making a custom label resolver by specializing make-uri-from-label-source with the first argument being an (eql :key)

(defclass label-source ()
  ((sources :initarg :sources :initform nil :accessor sources)
   (hash :initform (make-hash-table :test 'equalp) :accessor hash)
   (key :initarg :key :initform nil :accessor key)
   (key2instance :initarg :key2instance :accessor key2instance :allocation :class)
   (include-imports :initarg :include-imports :initform nil :accessor include-imports)
   ))

(defmethod initialize-instance ((ls label-source) &rest ignore)
  (call-next-method)
  (let ((table (hash ls)))
    (loop for file in (sources ls)
	   do
	   (let ((kb (load-ontology (namestring (truename file)))))
	     (let ((labels (rdfs-labels kb)))
	       (maphash (lambda(uri label) 
			  (let ((label (car label)))
			    (if (gethash label table) 
				(unless (eq (gethash label table)  uri)
				  (warn "Uri label ~a in ~a are both for ~a" (gethash label table)  uri label)
				  (setf (gethash label table) :ambiguous))
				(setf (gethash label table) uri))))
			labels)))))
  (unless (slot-boundp ls 'key2instance) (setf (key2instance ls) nil))
  (setf (key2instance ls) (remove (key ls) (key2instance ls) :key 'car))
  (push (cons (key ls) ls) (key2instance ls)))


(defmethod make-uri-from-label-source (source name actual)
  (let ((instance (cdr (assoc source (key2instance (mop:class-prototype (find-class 'label-source)))))))
    (unless instance
	(error "don't know label source '~s'" () source))
    (let ((table (hash instance)))
      (let ((found (gethash name table)))
	(when (eq found :ambiguous)
	  (progn
	    (warn "Uri label ~a in ~a is ambiguous" name source)
	    (setq found nil)))
	(if found
	    (progn
	      (when actual
		(assert (eq found (make-uri nil actual)) (found actual)
			"Uri label lookup for '~a' - ~a doesn't match specified actual '~a'"
			name found actual))
	      found)
	    (if actual
		(progn
		  (warn "Uri label lookup for '~a' failed - using provided actual: '~a'" name actual)
		  (make-uri nil actual))
		(error "Couldn't determine which URI was meant by '~a' in ~a" name source)))))))

(defun compare-ontology-rdfs-labels (ont1 ont2)
  "Compares the rdfs:labels for the uris in two ontologies, and prints to the screen the uris (and the uri's label) in which the labels of each ontology do not match."
  (let ((ont1-labels nil)
	(ont2-labels nil)
	(diff-labels nil))

    ;; get the (uri rdfs:label) list of each ontology
    (setf ont1-labels 
	  (loop 
	     for k being the hash-keys in (rdfs-labels ont1) using (hash-value v) 
	     collect (format nil "~a ~a" k (car v))))
	     ;;collect (format nil "~a" (car v))))

    (setf ont2-labels 
	  (loop 
	     for k being the hash-keys in (rdfs-labels ont2) using (hash-value v)
	     collect (format nil "~a ~a" k (car v))))
	     ;;collect (format nil "~a" (car v))))


    ;; get the set difference of each
    (setf diff-labels (set-difference ont1-labels ont2-labels :test 'equalp))
    
    ;; return the diff labels (if any)
    diff-labels))
	

    
