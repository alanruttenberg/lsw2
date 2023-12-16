;; This file implements a etag-based cache for OWL ontologies.
;; The cache is structured as hierarchical directories, with the top
;; level directory being the host name, and nested subdirectories for
;; each path element. (ensuring there aren't collisions)

;; In addition to the OWL files, two additional files are created.

;; <name>.headers
;;   Records the headers from the get-url request. The file-write-date
;; of this file records the last time the headers were fetched.

;; <name>.imports
;;  caches the imports for a requested ontology (but not
;; for it's imports separated), so that when checking the cache
;; validity, the ontology doesn't have to be loaded just to figure out
;; the imports. The first time is downloaded to into the cache it is
;; actually loaded in order to compute the imports.

;; An XML catalog-v0001.xml compatible with protege is created in every
;; directory where an OWL file is downloaded, listing all file in the
;; cache, so that when opening the file in protege knows where to find
;; any cached imports.

;; The caching policy is to use the etag from the saved header to
;; identify a version. Periodically a GET HEAD is executed to fetch
;; the current headers and check whether the Etag has changed. When
;; this happens the file is downloaded, and the headers and imports
;; files are updated. If the network is down or a network error
;; occurs, the cached version is used if present.
;;
;; In order to make the cache suitably fast for quick dev turnaround,
;; the cache will be consider valid for a settable period of time
;; before the Etag is fetched. That way, most of the time you're not
;; going to the web.

;; Functions:
;; 
;; (cache-ontology-and-imports url &key (force-recheck nil))

;;   Check whether the ontology and it's imports have been saved or
;; need to be updated in cache. Will generally be called by
;; load-ontology. If force-recheck the headers are fetched and checked
;; even if the assumed cache validity time hasn't passed.
;;
;; (load-ontology url)
;;   When cache is engaged will load from the file system if the
;; ontology at URL has been cached.
;; 
;; Variables
;;
;; *ontology-cache-directory*
;;   path to where the cache will live

;; *cache-check-etag-min-time*
;;  number of hours during which an etag will be considered
;;  stable. Default 24 hours means that we'll go to the web to check
;;  if the file changed at most once a day

;; *use-cache-aware-load-ontology*
;;   if t, then load-ontology always loads from the cache, which gets
;;   updated if either forced or if *cache-check-etag-min-time* hours
;;   has passed. Default nil for now (set it in .abclrc)

;; Only subtlety is that sometimes the ontology imported doesn't have
;; the same URI as the one requested. In order to handle this, the
;; ontology manager is examined and a map from actual loaded ontology
;; to request ontology is built.  When saving the catalog we also
;; include an entry for the ontology *named* in the import.

;; (save-ontology-and-imports-locally url location)
;; does the same thing but saves cache files to location
;;

(defpackage lsw2cache
  (:use cl jss ext)
  (:import-from :cl-user #:get-url #:uri-full #:uri-p 
		;; These are used in load-ontology which is loaded first. Really these should be in this package and exported, but
		;; package shit is annoying so just use the cl-user symbols
		#:*use-cache-aware-load-ontology*
		#:cache-ontology-and-imports
		#:ontology-cache-location
		#:uri-mapper-for-source
		))
(in-package lsw2cache)
;; First attempts to isolate something in LSW using packages


    
(defvar *ontology-cache-directory* "~/Desktop/ontology-cache/")
(defvar *cache-check-etag-min-time* 24)
(defvar *force-check-etag* nil)
(defvar *use-cache-aware-load-ontology* nil)

(defun ontology-cache-location (url)
  (let* ((path `(:absolute ,@(cdr (pathname-directory *ontology-cache-directory*))
			   ,(getf (pathname-host url) :authority)
			   ,@(cdr (pathname-directory url)))))
    (values
     (make-pathname :directory path)
     (make-pathname :directory path :name  (pathname-name url) :type (pathname-type url))
     (make-pathname :directory path :name  (pathname-name url) :type "headers"))))

(defun ontology-cache-etag (url)
  (multiple-value-bind (dir ont headers-file) (ontology-cache-location url)
      (declare (ignore dir ont))
    (and (probe-file headers-file)
	 (with-open-file (f headers-file :direction :input)
	   (let* ((headers (read f))
		  (value (second (assoc "Etag" headers :test 'equalp))))
	     (and (stringp value) (subseq value 1 (1- (length value)))))))))

(defun cache-one-ontology (url &optional from)
  (cl-user::forget-cached-url url);; just in case
  (cl-user::format *debug-io* "~&Downloading ~a~%" url)
  (multiple-value-bind (dir ont headers-file) (ontology-cache-location url)
    (let ((response (multiple-value-list (ignore-errors (get-url (or from url) :verb "HEAD" :dont-cache t :force-refetch t :persist nil)))))
      (if (and (consp response) (eq (car response) :error)
	       (equal (slot-value (second response) 'sys::format-control)
		      "Can only do GET on ftp connections"))
	  (with-open-file (f headers-file  :if-does-not-exist :create :direction :output :if-exists :supersede)
	    (print nil f))))
    (let ((headers (second (multiple-value-list (get-url (or from url) :verb "HEAD" :dont-cache t :force-refetch t :persist nil)))))
      (ensure-directories-exist dir)
      (with-open-file (f headers-file  :if-does-not-exist :create :direction :output :if-exists :supersede)
	(print headers f)))
    (with-open-file (o ont :direction :output :if-exists :supersede :if-does-not-exist :create))
    (let ((did nil))
      (unwind-protect (progn (get-url (or from url) :to-file ont :persist nil :dont-cache t) (setq did t))
	(unless did (uncache-ontology url))))
    (namestring (truename ont))))

(defun uncache-ontology (url)
  (multiple-value-bind (dir ont headers-file) (ontology-cache-location url)
    (let ((imports-file (merge-pathnames (make-pathname :type "imports" ) ont)))
      (loop for file in (list ont imports-file headers-file)
	    do
	       (when (probe-file file)
		 (delete-file file))))))

(defun have-current-copy (url &optional from)
  (multiple-value-bind (dir ont headers-file) (ontology-cache-location url)
    ;; if we're not forced to recheck, and sufficient time hasn't passed, and the file exists, return it
    (if (and (not *force-check-etag*)
	     (probe-file headers-file)
	     (file-write-date headers-file) 
	     (< (/ (- (get-universal-time) (file-write-date headers-file)) 3600)
		*cache-check-etag-min-time*))
	ont
	;; If we don't have an etag then fail
	(let ((last-etag (ontology-cache-etag url)))
	  (when last-etag
	    ;; Otherwise try to get the headers
	    (let ((head (multiple-value-list (get-url (or from url) :verb "HEAD" :dont-cache t :force-refetch t :ignore-errors t :persist nil))))
	      (if (and (consp head) (eq (car head) :error))
		  ;; If the get-url failed, return the file, since we can't check if there's a newer one
		  ont
		  (let* ((current-headers (second head))
			 (etag-raw (second (assoc "Etag" current-headers :test 'equalp))))
		    ;; Otherwise compare the Etags, bump the headers last-checked time.
		    ;; If the Etag matches, win and return the file
		    (set-file-write-date-now headers-file)
		    (and etag-raw
			 (equalp last-etag (subseq etag-raw 1 (1- (length etag-raw))))
			 ont)))))))))

(defun cached-imports (url)
  (multiple-value-bind (dir ont headers-file) (ontology-cache-location url)
    (let ((imports-path (merge-pathnames dir (make-pathname :name  (pathname-name url) :type "imports"))))
      (when (probe-file imports-path)
	(with-open-file (f imports-path :direction :input)
	  (read f))))))

(defun cache-ontology-imports (ontology url)
  (multiple-value-bind (dir ont headers-file) (ontology-cache-location url)
    (let ((imports-path (merge-pathnames dir (make-pathname :name  (pathname-name url) :type "imports")))
	  (imports (mapcar (lambda(el) (butlast el 1)) (cl-user::loaded-documents ontology))))
      ;; if an import was loaded from a file then we pretend it was loaded from its ontology-iri
      (let ((massaged-imports (loop for (i o) in imports
				    if (null (pathname-host i)) collect (list o o)
				      else collect (list i o))))
	(with-open-file (f imports-path :direction :output :if-exists :supersede :if-does-not-exist :create)
	  (print massaged-imports f))
	imports))))

(defun cache-ontology-and-imports (ontology &key (force-recheck nil) &aux imports there-was-a-download)
  (let ((*force-check-etag* force-recheck))
    (let ((url (cond ((stringp ontology) ontology)
		     ((uri-p ontology) (uri-full ontology)))))
      (when url
	(let ((have (have-current-copy url)))
	  (unless have
	    (setq there-was-a-download t)
	    (setq have (cache-one-ontology url)))
	  (setq imports (cached-imports ontology))
	  (unless imports
	    (setq ontology (cl-user::load-ontology (namestring have))))))
      (unless imports
	(setq imports (cache-ontology-imports ontology url)))
      (loop
	for (imported-from-iri ontology-iri) in imports
	do 
	   (when (and (#"matches" imported-from-iri "^http.*") (not (have-current-copy ontology-iri imported-from-iri)))
	     (cache-one-ontology ontology-iri imported-from-iri))
	   (sleep .01))
      (when there-was-a-download
	(make-cache-catalogs)))))

(defun save-ontology-and-imports-locally (url directory)
  (let ((*ontology-cache-directory* directory))
    (cache-ontology-and-imports url)))
    
;; Create a relative path - go up (..) until there's a common root, then add the path of destination relative to common
;; (relative-path-to "/a/b/c/f" "/a/b/d/g") -> ../d/g
(defun relative-path-to (from to)
  (let ((from-components (cdr (pathname-directory from)))
	(to-components (cdr (pathname-directory to))))
    (destructuring-bind (from-remainder to-remainder)
	(loop for (from  . from-rest) on from-components
	      for (to . to-rest) on to-components
	      until (not (equal from to))
	      finally (return (list (cons from from-rest) (cons to to-rest))))
      (when (equal from-remainder to-remainder)
	(setq from-remainder nil to-remainder nil))
	
      (make-pathname :directory `(:relative ,@(loop repeat (length from-remainder) collect :up) ,@to-remainder)
		     :name (pathname-name to)
		     :type (pathname-type to)))))

;; This is a workaround for https://github.com/armedbear/abcl/issues/74
(defun cache-directories-with-owl-files ()
  (remove-duplicates
   (mapcar (lambda(el) (make-pathname :directory (pathname-directory el)))
	   (directory (merge-pathnames (make-pathname :directory '(:relative :wild-inferiors)  :name :wild :type "owl")
				       *ontology-cache-directory*)))
	  :test 'equalp))

(defun url-for-cached-file (path)
  (let ((dir (subseq (pathname-directory path) (length (pathname-directory *ontology-cache-directory*)))))
    (format nil "http://~{~a/~}~a.~a" dir (pathname-name path) (pathname-type path))))

(defun make-cache-catalogs ()
  (let ((all-owl-files 
	  (directory (merge-pathnames (make-pathname :directory '(:relative :wild-inferiors)  :name :wild :type "owl")
				      *ontology-cache-directory*)))
	(all-imports-files (directory (merge-pathnames (make-pathname :directory '(:relative :wild-inferiors)  :name :wild :type "imports")
						       *ontology-cache-directory*))))
    ;; the imports files are lists of pairs of imported-from iris, and ontology iris. ;
    ;; If the cache file for the ontology iri exists, then collect a pair of ontology-iri and pathname. ;
    ;; If the imported-from differs from the ontology-iri then collect a pair of imported-from iri and same pathname ;
    ;; Then check all the owl files in the cache. If there's a file that isn't mentioned in an import, collect ;
    ;; a pair of a URL derived from the path (assuming http, though we could include both) and the file. 
    ;; These are now the entries that will go into the catalog ;

    ;; Now, in each directory that has an owl file, write catalog.xml with the filenames in the map created above relativized to that directory.

    (let ((all-imports (loop for path in all-imports-files
			     append (with-open-file (f path :direction :input) (read f)))))
      (let ((map (loop for (imported-iri ontology-iri) in all-imports
		       append  (multiple-value-bind (dir file) (ontology-cache-location ontology-iri)
				 (list* (list ontology-iri file)
					(and (not (equal ontology-iri imported-iri))
					     (list (list imported-iri file))))))))
	(setq map (remove-duplicates map :test 'equalp))
	(let ((remaining-owl-files (set-difference all-owl-files (mapcar 'second map) :test 'equalp)))
	  (setq map
		(append map
			(loop for remaining in remaining-owl-files
			      collect (list (url-for-cached-file remaining) remaining))))
	  map)
	(loop for directory in (cache-directories-with-owl-files)
	      do
		 (with-open-file (f (merge-pathnames "catalog-v001.xml" directory) :if-does-not-exist :create :direction :output :if-exists :supersede)
		   (format f "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>~%")
		   (format f "<catalog prefer=\"public\" xmlns=\"urn:oasis:names:tc:entity:xmlns:xml:catalog\">~%")
		   (loop for (url path) in map
			 for relatived-path = (namestring (relative-path-to directory path))
			 do
			    (format f "  <uri name=~s uri =~s/>~%" url relatived-path))
		   (format f "</catalog>~%")
		   ))))))

;; These are helper functions that have load-ontology respect the same
;; xml catalog that protege uses to find its imports.
(defun uri-mapper-from-xml-catalog (catalog)
  (with-open-file (f catalog) 
    (loop
       with dir = (namestring (make-pathname :directory (pathname-directory catalog)))
       with mapper = (new 'OWLOntologyIRIMapperImpl)
       for el in (cl-user::find-elements-with-tag (xmls::parse f) "uri")
       for uri = (coerce (cl-user::attribute-named el "uri") 'simple-base-string)
       for name = (coerce (cl-user::attribute-named el "name") 'simple-base-string)
       for physical-uri = (if (find #\: uri) uri (format nil "file://~a" (namestring (translate-logical-pathname (format nil "~a~a" dir uri)))))
       when (and uri name)
       do 
       (#"addMapping" mapper (#"create" 'org.semanticweb.owlapi.model.IRI name) (#"create" 'org.semanticweb.owlapi.model.IRI physical-uri))
       finally (return mapper))))
	 
(defun uri-mapper-for-source (source)
  (cond ((and (uri-p source) (#"matches" (uri-full source) "^file:.*")) 
	 (setq source (#"replaceAll" (uri-full source) "^file:/*" "/"))))
  (when (and (not (consp (pathname-host source)))
	     (probe-file source))
    (let ((catalog (merge-pathnames "catalog-v001.xml" source)))
      (if (probe-file catalog)
	  (uri-mapper-from-xml-catalog catalog)
	  (new 'AutoIRIMapper (new 'java.io.file (namestring (truename (make-pathname :directory (pathname-directory (truename source)))))) t)))))


(defun set-file-write-date-now (path)
  (#"setLastModified" (new 'file (namestring path)) (#0"currentTimeMillis" 'System)))
