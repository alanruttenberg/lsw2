(in-package :cl-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; helper functions to retrieve stuff from xmls read xml documents
; Structure is (name attributes children)

; return the first element with tag you find. If more-tags are supplied,
; then recurse and look for more within the first found element

(defun index-xml (element)
  ;; map all elements keeping path, hash path, tag -> elementb)
)

(defun find-element-with-tag (element tag &rest more-tags)
  (let ((found
	 (loop with q = (list element)
	    for this = (pop q)
	    for (name nil . children) = this
	    if (cond ((consp name) 
		      (if (consp tag)
			  (member (car name) tag :test 'equalp)
			  (string-equal (car name) tag)))
		     (t (if (consp tag)
			    (member name tag :test 'equalp)
			    (string-equal name tag))))
	    do (return this)
	    else do (when (listp children) (setq q (nconc q (remove-if-not 'consp children))))
	    while q)))
    (if (and found more-tags)
	(apply 'find-element-with-tag found more-tags)
	found)))

; return all elements with tag  (except don't look within such tagged elements for more)
; If more-tags is supplied then these are gathered from within the first found elements

(defun find-elements-with-tag (element tag &rest more-tags)
  (let ((found
	 (loop with q = (list element)
	    for this = (pop q)
	    for (name nil . children) = this
	    if (cond ((consp name) 
		      (if (consp tag)
			  (member (car name) tag :test 'equalp)
			  (string-equal (car name) tag)))
		     (t (if (consp tag)
			    (member name tag :test 'equalp)
			    (string-equal name tag))))
	    collect this
	    else do (when (listp children) (setq q (nconc q (remove-if-not 'consp children))))
	    while q)))
    (if (and found more-tags)
	(loop for el in found append (apply 'find-element-with-tag el more-tags))
	found)))

(defvar seen! nil)
(defvar yikes! 0)

(defun map-elements-1 (element fn)
  (declare (optimize (speed 3) (safety 0)))
  (incf yikes!)
  (and element
       (unless nil; (gethash element seen!)
	 ; (setf (gethash element seen!) t)
	 (when (consp element)
	   (let ((children (cddr (the list element))))
	     (when (consp children)
	       (dolist (c (the list children))
		 (funcall (the function fn) c)
		 (map-elements-1 c fn))))))))

(defun map-elements (element fn)
  (declare (optimize (speed 3) (safety 0)))
  (let ((seen (make-hash-table :test #'eql :size 4000000)))
    (setq seen! seen)
    (map-elements-1 element fn)))

(defun find-immediate-children-with-tag (element tag)
  (and (listp (cddr element))
       (loop for child in (cddr element)
	  for (name) = child
	  when (cond ((consp name) 
		      (string-equal (car name) tag))
		     (t (string-equal name tag)))
	  collect child)))

(defun attribute-named (element name)
  (second (assoc name (second element) :test 'equal)))

(defun what-tags (xmls)
  (let ((table (make-hash-table :test 'equal)))
    (labels ((one (f)
	       (cond
		 ((and f (consp f))
		  (setf (gethash (car f) table) t)
		  (map nil #'one (cddr f))))))
      (one xmls)
      (loop for k being the hash-keys of table collect k))))

(defun what-attributes (xmls)
  (let ((table (make-hash-table :test 'equal)))
    (labels ((one (f)
	       (when (consp f)
		 (when (second f)
		   (loop for (attr value) in (second f)
		      do (setf (gethash attr table) t)))
		 (map nil #'one (cddr f)))))
      (one xmls)
      (loop for k being the hash-keys of table collect k))))


;; patch so that unrecognized entities are just passed through.

(defun xmls::resolve-entity (ent)
  "Resolves the xml entity ENT to a character.  Numeric entities are
converted using CODE-CHAR, which only works in implementations that
internally encode strings in US-ASCII, ISO-8859-1 or UCS."
  (declare (type simple-base-string ent))
  (declare (type vector xmls::*entities*))
  (or (and (>= (length ent) 2)
           (char= (char ent 0) #\#)
           (code-char
            (if (char= (char ent 1) #\x)
                (parse-integer ent :start 2 :end (- (length ent) 1) :radix 16)
                (parse-integer ent :start 1 :end (- (length ent) 1)))))
      (second (find ent xmls::*entities* :test #'string= :key #'first))
      ent))

(defun xmls::push-string (c string)
  "Shorthand function for adding characters to an extendable string."
  (if (characterp  c)
      (vector-push-extend c string)
      (loop for ci across c do
	   (vector-push-extend ci string)))
  string)



