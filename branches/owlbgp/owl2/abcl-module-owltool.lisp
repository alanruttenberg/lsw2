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

(push "-check <ontology-uri> [-pellet|-fact++|-hermit] 

Does a consistency check, reports the result. If consistent classifies the ontology
and reports whether there are any unsatisfiable classes.

Examples:
 -check http://purl.obolibrary.org/obo/iao.owl
 -check ~/obi/ontology/obi.owl" *usage*)


(push "-create-external-derived|-ced <external.owl> -template <template.txt> -dest <output.owl> -ontology-iri <iri of for output.owl>

Implementation of MIREOT as used by OBI. For an example of input file see http://purl.obolibrary.org/obo/obi/external.owl

Example:
-create-external-derived salo-external.owl -template ~/repos/obi/tools/external-templates.txt -dest salo-external-derived.owl -ontology-iri http://purl.obolibrary.org/obo/salo/external-derived.owl"
      *usage*)

(push "-trimmed-imports-module <ontology.owl>  -dest <output.owl> [-nested|-top|-bottom]

Apply the syntactic locality modularity algoritms with the signature
being all terms in ontology.owl. The resultant output.owl will contain
the module as well as annotations for all terms that are in any of the
imported ontologies. 

Add -nested or -top or -bottom to select the type of module. -nested is default.

Example:
-trimmed-imports-module http://purl.obolibrary.org/obo/obi.owl -dest ~/Desktop/obi-module.owl"
      *usage*)

(setq *usage* (nreverse *usage*))

(defun maybe-usage (&optional force)
  (when (or (intersection '("-help" "-h" "--help" "-?") *command-line-argument-list* :test 'equalp)
	    (not *did-something*)
	    force)
    (format t "Note: FaCT++ is not working in this version.~%~%-h|--help|-?|-help - this message~%~%-no-quit - stay in lisp listener when finished~%~%-verbose - output various debug information~%~%~{~a~%~%~}" *usage*)))

(when (< (length *command-line-argument-list*) 3)
  (maybe-usage t)
  (quit))

;; (when (find "-verbose" *command-line-argument-list* :test 'equal)
;;   (setq *load-verbose* t)
;;   (setq *compile-verbose* t)
;;   (setq asdf::*asdf-verbose* t)
;;   (print *loading-jarfile*)
;;   (print *load-pathname*))


(defmethod asdf::operation-done-p ((operation asdf::compile-op) (c t))
  (or cl-user::*inhibit-add-to-classpath*
   (call-next-method)))

(module-load-systems '(jss patches util owl2))

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


(let ((factpp (or (member "-factpp" *command-line-argument-list* :test 'equal)
		  (member "-fact++" *command-line-argument-list* :test 'equal)))
      (pellet (member "-pellet" *command-line-argument-list* :test 'equal))
      (hermit (member "-hermit" *command-line-argument-list* :test 'equal)))
  (when (>  (+ (if hermit 1 0) (if pellet 1 0) (if factpp 1 0)) 1)
    (error "Only one reasoner: -fact++, -hermit, or -pellet should be specified"))
  (setq *default-reasoner* (if factpp :factpp (if hermit :hermit (if pellet :pellet *default-reasoner*)))))

(let ((treeview-cmd (position "-treeview" *command-line-argument-list*  :test 'equal)))
  (when treeview-cmd
    (let* ((url (nth (1+ treeview-cmd) *command-line-argument-list*))
	   (noinfer (member "-no-classify" *command-line-argument-list* :test 'equal)))
      (unless url
	(error "No URL for ontology to view given"))
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
      (read-line))))

(defun check-ontology (ont  &key classify reasoner (log "OFF") profile)
  (let ((reasoner (or reasoner (v3kb-default-reasoner ont) *default-reasoner*)))
    (instantiate-reasoner ont reasoner profile)
    (when (eq reasoner :pellet)
      (pellet-log-level (jcall "getKB" (v3kb-reasoner ont)) log))
					;  (if classify (#"prepareReasoner" (v3kb-reasoner ont)))
    (if classify
	(ecase reasoner 
	  (:hermit (jcall "classify" (v3kb-reasoner ont)))
	  ((:pellet :factpp) (jcall "prepareReasoner" (v3kb-reasoner ont))))) 
    (prog1
	(jcall "isConsistent" (v3kb-reasoner ont))
      (when (eq reasoner :pellet)
	(pellet-log-level (jcall "getKB" (v3kb-reasoner ont)) "OFF")
	))))

(let ((check-cmd (position "-check" *command-line-argument-list*  :test 'equal)))
  (when check-cmd
    (let* ((url (nth (1+ check-cmd) *command-line-argument-list*)))
      (unless url
	(error "No URL for ontology to view given"))
      (setq *did-something* t)
      (format t "Loading ontology ~a...~%" url)
      (let* ((ont (load-ontology url))
	     (consistent (check-ontology ont)))
	(if consistent
	    (progn
	      (format t "Ontology is consistent.~%")
	      (check-ontology ont :classify t)
	      (let ((unsat (unsatisfiable-classes ont)))
		(if unsat
		    (format t "There are ~a unsatisfiable classes: ~{~a,~^ ~}~%" unsat)
		    (format t "All classes are satisfiable~%.")))
	      (force-output t))
	    (format t "Ontology is inconsistent.~%")		       
	    )))))

(let ((external-cmd (or (cmdl-named-arg "-create-external-derived") (cmdl-named-arg "-ced"))))
  (when external-cmd
    (let* ((uri external-cmd))
      (unless uri
	(error "No URL for externals file"))
      (let ((template (cmdl-named-arg "-template"))
	    (output (cmdl-named-arg "-dest"))
	    (ont-uri (cmdl-named-arg "-ontology-iri")))
	(unless (and (probe-file template) output ont-uri)
	  (maybe-usage t))
	(setq *did-something* t)
	(when (not (jcall "matches" uri "^(/(|)(file)|(http)).*"))
	  (setq uri (namestring (merge-pathnames uri (current-directory)))))
	(format t "Loading ontology ~a...~%" uri)
	(create-external-derived
	 :kb (load-ontology uri) 
	 :templates-path (merge-pathnames template (current-directory))
	 :output-path (merge-pathnames output (current-directory))
	 :ontology-uri ont-uri
	 :endpoint (cmdl-named-arg "-sparql-endpoint"))))))

(let ((cmd (cmdl-named-arg "-trimmed-imports-module")))
  (when cmd
    (let ((output (cmdl-named-arg "-dest"))
	  (type (cmdl-named-arg "-module-type"))
	  (uri cmd))
      (when (member type '(nil "nested" "top" "bottom"))
	(setq type (cdr (assoc type '((nil . "STAR") ("nested" . "STAR") ("top" . "TOP") ("bottom" . "BOTTOM")))))
	(when (not (jcall "matches" uri "^(/(|)(file)|(http)).*"))
	  (setq uri (namestring (merge-pathnames uri (current-directory)))))
	(format t "Loading ontology ~a...~%" uri)
	(setq *did-something* t)
	(trimmed-imports-module (load-ontology uri) :dest output)
	(format t "Wrote ontology ~a...~%" uri)
	))))

(maybe-usage)

(unless (find "-no-quit" *command-line-argument-list* :test 'equal)
  (quit))


