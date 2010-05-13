(in-package :common-lisp-user)

(setq *PELLET-DIR* "")

(format t "Module owltool...~%")

(defvar *usage* nil)
(defvar *did-something* nil)


(push "-download <ontology-uri> <directory> <wget-command>

Downloads ontologies and its imports closure to <directory>. Uses the wget
command, the location of which must be supplied as an argument. An XML Catalog
suitable for use with Protege 4.1 is created so that protege can use this local 
version.

Example: -download http://purl.obolibrary.org/obo/obi.owl ~/Desktop/save /sw/bin/wget" *usage*)

(push "-treeview <ontology-uri> [-no-classify] [-pellet|-fact++|-hermit] 

Presents an interactive treeview of ontology. By default uses the HermiT reasoner,
-pellet switches to the pellet reasoner, -fact++ switches to the FaCT++ reasoner.
By default classifies the ontology first, which might be slow. -no-classify when provided
will show the ontology as told - without classification.

Example: -treeview http://purl.obolibrary.org/obo/iao.owl" *usage*)


(defun usage (&optional force)
  (when (or (intersection '("-help" "-h" "--help" "-?") *command-line-argument-list* :test 'equalp)
	    (not *did-something*)
	    force)
    (loop for usage in *usage* do
	 (format t "-h|--help|-?|-help - this message~%~%-no-quit - stay in lisp listener when finished~%~%-verbose - output various debug information~%~%~{~a~%~%~}" *usage*))))

(when (< (length *command-line-argument-list*) 3)
  (usage t)
  (quit))

(when (find "-verbose" *command-line-argument-list* :test 'equal)
  (setq *load-verbose* t)
  (setq *compile-verbose* t)
  (setq asdf::*asdf-verbose* t)
  (print *loading-jarfile*)
  (print *load-pathname*))


(defmethod asdf::operation-done-p ((operation asdf::compile-op) (c t))
  (or cl-user::*inhibit-add-to-classpath*
   (call-next-method)))

(let ((asdf::*output-translations* 
       `(((t ,(lambda(path wha) 
		     (let* ((dir (pathname-directory path))
			    (new (append '(:absolute) (list "bin") (cdr dir))))
		       (merge-pathnames path (pathname *loading-jarfile*)))))))))

  (asdf::initialize-source-registry
   `(:source-registry :ignore-inherited-configuration
		      ,@
		      (mapcar
		       (lambda(e) `(:directory ,(make-pathname :directory (pathname-directory e) :device (pathname-device e))))
		       (directory-in-jar (merge-pathnames "/**/*.asd" (pathname *loading-jarfile*))))))
  (asdf::oos 'asdf::load-op 'jss)
  (asdf::oos 'asdf::load-op 'patches)
  (asdf::oos 'asdf::load-op 'util)
  (asdf::oos 'asdf::load-op 'owl2)
  )


(defun current-directory ()
  (format nil "~a/" (jstatic "getProperty" "java.lang.System" "user.dir")))

(when (probe-file (merge-pathnames "commands.lisp" (car (pathname-device *loading-jarfile*))))
  (format t "Loading ~a...~%" (namestring (merge-pathnames "commands.lisp" (car (pathname-device *loading-jarfile*)))))
  (load (merge-pathnames "commands.lisp" (car (pathname-device *loading-jarfile*)))))

(let ((download-cmd (position "-download" *command-line-argument-list*  :test 'equal)))
  (when download-cmd
    (let* ((url (nth (1+ download-cmd) *command-line-argument-list*))
	   (dest (nth (+ 2 download-cmd) *command-line-argument-list*))
	   (wget-command (or (nth (+ 3 download-cmd) *command-line-argument-list*) (wget-command))))
      (unless wget-command
	(error "No wget command given or found"))
      (unless dest
	(error "No destination for saved ontology given"))
      (when (not (jcall "matches" dest ".*/")) (setq dest (format nil "~a/" dest)))
      (when (eq (car (pathname-directory dest)) :relative)
	(setq dest (merge-pathnames dest (current-directory))))
      (setq *did-something* t)
      (save-ontology-and-imports-locally url dest :wget-command wget-command)
      (print "OK")
      )))

(let ((treeview-cmd (position "-treeview" *command-line-argument-list*  :test 'equal)))
  (when treeview-cmd
    (let* ((url (nth (1+ treeview-cmd) *command-line-argument-list*))
	   (noinfer (member "-no-classify" *command-line-argument-list* :test 'equal))
	   (factpp (or (member "-factpp" *command-line-argument-list* :test 'equal)
		       (member "-fact++" *command-line-argument-list* :test 'equal)))
	   (pellet (member "-pellet" *command-line-argument-list* :test 'equal))
	   (hermit (member "-hermit" *command-line-argument-list* :test 'equal)))
      (unless url
	(error "No URL for ontology to view given"))
      (when (>  (+ (if hermit 1 0) (if pellet 1 0) (if factpp 1 0)) 1)
	(error "Only one reasoner: -fact++, -hermit, or -pellet should be specified"))
      (let ((*default-reasoner* (if factpp :factpp (if hermit :hermit (if pellet :pellet *default-reasoner*)))))
	(setq *did-something* t)
	(show-classtree url :inferred (not noinfer) 
			:dont-show
			(if (or (search "purl.obolibrary.org" url)
				(search "purl.org/obo/owl" url))
			    *obi-noise-classes*
			    nil)
			:depth 2)
	(format t "hit return to quit~%")
	(force-output t)
	(read-line)))))

(usage)

(unless (find "-no-quit" *command-line-argument-list* :test 'equal)
  (quit))


