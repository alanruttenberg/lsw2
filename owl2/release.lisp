;; Typical usage:
;; (make-release  "iao" "~/repos/information-artifact-ontology/src/ontology/iao.owl"
;;               :when 3692716953
;;               :additional-products '("ontology-metadata.owl" "iao-main"))
;;
;; Arguments:
;; First argument is namespace. It is used in defining purls.
;;
;; Second argument is location of top-level ontology that will be merged
;;
;; :when is optional, defaulting to now = (get-universal-time). Passing an
;;   explicit time is useful when you started release one day but
;;   finished it on some other. Synonym :release-time
;;
;; :additional products name other ontology files parallel to the main
;;   source. These will be given purls like http://purl.obolibrary.org/obo/<namespace>/<name of additional product>
;;   as well as dated PURLs
;;
;; :version-date-string defaults to YYYY-MM-DD computed from when
;;
;; :release-purl-base is where the release files will be served from.
;;   defaults to https://raw.githubusercontent.com/<repo owner>/<repo name>/master/
;;   where <repo owner> and <repo name> are computed by call to git in the
;;   directory of the source file argument.
;;
;; :release-directory-base defaults to ../../releases from directory of source file argument
;;   This is where the build products are put
;;
;; :license-annotations is an alist with entries (<uri> value).
;;   Default is to read a file called LICENSE in the same directory where
;;   the first line is expected to be a URL, which becomes the value of
;;   dc:license with subsequent lines being a textual description added
;;   as a comment. Example: https://github.com/information-artifact-ontology/IAO/blob/master/src/ontology/LICENSE
;;   
;; :reasoner is the reasoner to use for checking consistency and adding inferences.
;;   Default is fact++ (:factpp). Other choices :elk,:hermit,:pellet
;;
;; The global *current-release* is set to the release object.
;; 
;; Defaults are computed based on the following assumptions:
;; Setup is, starting at repo root, /releases/<date>/<release files> /src/ontology/
;; PURLs are based at purl.obolibrary.org/obo/<namespace>...
;; Versioned PURLs are of the form purl.obolibrary.org/obo/<namespace>/<date>/...
;; Version control is via git
;; Release files served from github
;; If a /dev/ version is published then each ontology purl.obolibrary.org/obo/dev/xxx.owl must have a
;;   versionIRI in the source should be purl.obolibrary.org/obo/dev/xxx.owl  for ontologyIRI 
;;   purl.obolibrary.org/obo/xxx.owl

;; A release consists of:
;;   - A  merged/reasoned version of the ontology copied to the release directory
;;     called <namespace>-merged.owl, served at http://purl.obolibrary.org/obo/<namespace>.owl
;;   - The unmerged/unreasoned version served at http://purl.obolibrary.org/obo/<namespace>/<namespace>-stated.owl
;;   - Each additional product served at http://purl.obolibrary.org/obo/<namespace>/<product>.owl
;;   - versioned purls for each of the above and imports at http://purl.obolibrary.org/obo/<namespace>/<date>/<filename>.owl
;;   - The original ontology, local imports, and external imports without a version IRI copied
;;     to the release directory and given a dated purl in the ontology's namespace
;;   - OntologyIRIs and VersionIRIs for the stated version and its imports in the release directory
;;      rewritten to use dated PURLs.
;;   - A catalog-v001.xml file in the release directory to tell protege how to load the release
;;   - A file purls.yml in the release directory with lines to copy to the purl.obolibrary.org
;;       configuration file
;; Accessing a dated PURL will always get the same results, assuming the targets of dated PURLs aren't
;;  changed (which they shouldn't be)

;; Implementation:
;; Rerun the ontofox script to update externals (todo)
;; Load the ontology, merge all axioms into one ontology. (done)
;; Run reasoner. Add inferred subclass axioms.  (done)
:; Do a little materialization.  (todo)
;; Copy the ontology annotations from the top ontology (not the imports)  (done)
;; Don't add certain duplicates, such as extra copies of definitions or annotation. Be aware of duplicate 
;;   axioms that might be pulled in by chains of imports and prefer the proximate one. (todo)
;; Add imported-from annotations to the imported ontologies (done)
;; Write the merged ontology to the release directory (done)
;; Copy our own imports and others' unversioned imports to the release directory (done)
;; Add versionIRIs and rewrite imports to use them (done)
;; Add an ontology annotation saying how this was created (done)
;; Write out a catalog-v001.xml because protege can't find ontologies in front of its nose (done)
;; Write out what will be needed to be added to the the PURL config (purls.yml in release dir) (done)
;; Write out some basic facts / release notes template (todo)
;;
;; Ideas for future:
;;  - subclasses for projects hosted at sourceforge, gitlab
;;  - products that are materialized sufficiently to get reasonable results
;;      when queried using SPARQL
;;  - Other variants people tend to use - subsets, no axioms
;;  - OBO product
;;  - package LSW so the build can be specified by a maven .pom
;;
;; Related:
;;  - http://www.obofoundry.org/id-policy.html
;;  - http://obofoundry.org/principles/fp-000-summary.html
;;  - https://github.com/ontodev/robot

(defclass foundry-release ()
  ((ontology-source-file :accessor ontology-source-file :initarg :ontology-source-file)
   (project-root-directory :accessor project-root-directory :initarg :project-root-directory)
   (ontology :accessor ontology :initarg :ontology)
   (reasoner :accessor reasoner :initarg :reasoner :initform :factpp)
   (namespace :accessor namespace :initarg :namespace)
   (release-time :accessor release-time :initarg :release-time )
   (version-date-string :accessor version-date-string :initarg :version-date-string)
   (release-directory-base :accessor release-directory-base :initarg :release-directory-base)
   (release-purl-base :accessor release-purl-base :initarg :release-purl-base)
   (dispositions :accessor dispositions :initarg :dispositions )
   (license-annotations :accessor license-annotations :initarg :license-annotations)
   (phases :accessor phases :initarg :phases :initform nil)
   (versioned-uri-map :accessor versioned-uri-map :initarg :versioned-uri-map)
   (additional-products :accessor additional-products :initarg :additional-products)
   ))

(setq last-kbd-macro "\213\C-y :accessor \C-y :initarg :\C-y\C-n\C-a\C-f\C-f\C-f")


(defmethod initialize-instance ((r foundry-release) &rest initiargs)
  (call-next-method)
  (assert (probe-file (ontology-source-file r)) () "Didn't find ontology-source-file - given as ~a" (ontology-source-file r))
  (when (and (not (slot-boundp r 'version-date-string) ) (not (slot-boundp r 'release-time)))
    (setf (release-time r) (get-universal-time)))
  (when (and (slot-boundp r 'version-date-string) (not (slot-boundp r 'release-time)))
    (setf (release-time r) 
	  (apply 'encode-universal-time 0 0 12
		 (car (all-matches (version-date-string r) "(\\d+)-(\\d+)-(\\d+)" 3 2 1))))))

(defmethod print-object ((r foundry-release) stream)
  (if (not (slot-boundp r 'namespace))
      (call-next-method)
      (print-unreadable-object (r stream :type t)
	(format stream "~a@~a~a~{~a~^,~}" (namespace r) (version-date-string r) (if (phases r) " : " ", not yet started") (phases r)))))

(defvar *current-release* )

;; this is the main call
(defun make-release (namespace source-file  &rest initargs &key (when (get-universal-time)) hold  &allow-other-keys)
  (remf initargs :when) (remf initargs :hold)
  (setq *current-release* (apply 'make-instance 'foundry-release
				 :release-time when
				 :ontology-source-file source-file
				 :namespace namespace
				 initargs))
  (unless hold
    (do-release *current-release*)))

(defmethod do-release  ((r foundry-release))
  (create-merged-ontology r)
  (copy-files-to-release-directory r)
  (log-progress r "Creating catalog-v0001.xml for protege")
  (write-catalog.xml r)
  (log-progress r "Creating purl.yaml with lines to be pasted into the PURL config")
  (write-purl-yaml r))

;; (make-release "iao" "~/repos/information-artifact-ontology/src/ontology/iao.owl" when
;;    :additional-products '("ontology-metadata.owl"))
 
(defmethod  version-date-string  :around ((r foundry-release))
  "The date of the release in the form YYYY-MM-DD"
  (if (slot-boundp r 'version-date-string)
      (call-next-method)
      (setf (version-date-string r)
	    (multiple-value-bind (second minute hour date month year day)  (decode-universal-time (release-time r))
	      (declare (ignore second minute hour  day))
	      (format nil "~a-~2,'0D-~2,'0D" year month date)))))

(defmethod release-directory-base :around ((r foundry-release))
  "Usual setup is 'releases' parallel to 'src' and ontology below
   'ontology'. Guess the release directory from our input, check if it is
   there and let us know where we stand"
  (if (slot-boundp r 'release-directory-base)
      (call-next-method)
      (setf (release-directory-base r)
	    (let* ((source-path (ontology-source-file r))
		   (release-base (make-pathname :directory 
						(append
						 (remove-if
						  (lambda(e) (member e '("src" "ontology") :test'equal))
						  (pathname-directory source-path))
						 '("releases")))))
	      (if (probe-file release-base)
		  (log-progress r "Using release directory: ~a~%" release-base)
		  (cerror "Can't figure out release base (guessed ~a). Pass it to the function" release-base))
	      release-base))))

(defmethod project-root-directory :around ((r foundry-release))
  (if (slot-boundp r 'project-root-directory)
      (call-next-method)
      (setf (project-root-directory r)
	    (car (directory
		  (read-line 
		   (sys::process-output
		    (sys::run-program
		     "git" (list "rev-parse" "--show-toplevel")
		     :wait t :directory (release-directory-base r)))))))))

(defmethod project-root-relative-path ((r foundry-release) path)
  (make-pathname
   :directory
   `(:relative ,@(subseq (cdr (pathname-directory path))
			 (length (cdr (pathname-directory (project-root-directory r)))))) 
   :name (pathname-name path)
   :type (pathname-type path)))

(defmethod git-project-info ((r foundry-release))
  (loop with stream = (sys::process-output
		       (sys::run-program "git" (list "remote" "-v") :output
					 :stream :wait t :directory (release-directory-base r)))
	for line = (read-line stream nil :eof)
	until (eq line :eof)
	for match = (caar (all-matches line "origin\\s+(.*)\\s+\\(fetch\\)$" 1))
	when match do (return (car (all-matches match ".*?([^/]+)/([^/]+)\\.git$" 1 2)))))
  
;; git URLs are case sensitive. watch out
(defmethod release-purl-base :around ((r foundry-release))
  (if (slot-boundp r 'release-purl-base)
      (call-next-method)
      (destructuring-bind (owner project)
	  (git-project-info r)
	(let* (	;; (raw-base (format nil "http://cdn.rawgit.com/~a/~a/master/" owner project))
	       ;; cdn.rawgit would be faster/nicer but deployments are permanent! No fixing mistakes.
	       ;; I could embed the commit into the URL as below but then I'd have to generate the redirects after committing
	       ;; the main files. Too likely to lead to an error. (maybe just a PURL prefix - will think about it)
	       ;; (raw-base (raw-base (format nil "https://github.com/~a/~a/<commit>/" owner project)))
	       ;; For now just serve the file you'de find after clicking "raw"
	       (raw-base (format nil "https://raw.githubusercontent.com/~a/~a/master/" owner project))
	       (probe-test (merge-pathnames (project-root-relative-path r (truename (ontology-source-file r))) raw-base)))
	  (assert (probe-file probe-test) 
		  (raw-base probe-test) "Calculated Release-url-base incorrectly: ~a" raw-base)
	  (setf (release-purl-base r) raw-base)))))

(defmethod license-annotations :around ((r foundry-release))
  "File license in in the ontology directory is consulted. The first
   line is the URL to the license and is recorded in
   dc:license. Subsequent lines are a textual gloss and are put in an
   rdfs:comment"
  (if (slot-boundp r 'license-annotations)
      (call-next-method)
      (setf (license-annotations r)
	    (let ((license-path (make-pathname :directory (pathname-directory (ontology-source-file r)) :name "LICENSE" :type nil )))
	      (if (probe-file license-path)
		  (with-open-file (l license-path)
		    (list (list !dc:license (read-line l))
			  (let ((license-explanation
				  (with-output-to-string (s)
				    (loop for line = (read-line l nil :eof)
					  until (eq line :eof)
					  do (write-line line s)))))
			    (setq license-explanation (#"replaceAll" license-explanation "(^\\s*|\\s*$)" ""))
			    (list !rdfs:comment license-explanation))))
		  (warn "LICENSE file not found so can't add info to release files"))))))

(defmethod dispositions :around ((r foundry-release))
  (if (slot-boundp r 'dispositions )
      (call-next-method)
      (setf (dispositions r) (compute-imports-disposition r))))

(defmethod versioned-uri-map :around ((r foundry-release))
  (if (slot-boundp r 'versioned-uri-map)
      (call-next-method)
      (setf (versioned-uri-map r) 
	    (mapcan (lambda(d)
		      `(,@(if (getf d :original-versioniri)
			      (list (list (getf d :original-versioniri) (getf d :versioniri))))
			(,(getf d :ontologyiri) ,(getf d :versioniri))))
		    (dispositions r)))))

(defmethod versioned-uri-for ((r foundry-release) uri)
  (second (assoc uri (versioned-uri-map r))))

(defun new-empty-kb (ontology-iri &key reasoner)
  "Make an empty KB into which we'll copy the merged file and the inferences"
  (let* ((manager (#"createOWLOntologyManager" 'org.semanticweb.owlapi.apibinding.OWLManager))
	 (ont (#"createOntology" manager (to-iri (uri-full ontology-iri)))))
    (make-v3kb :name ontology-iri :manager manager :ont ont :datafactory (#"getOWLDataFactory" manager) :default-reasoner reasoner)))

(defmethod log-progress ((r foundry-release) format-string &rest format-args)
  (fresh-line *debug-io*)
  (apply 'format *debug-io* format-string format-args)
  (force-output *debug-io*))

(defmethod ontology :around ((r foundry-release))
  (if (slot-boundp r 'ontology)
      (call-next-method)
      (setf (ontology r) (init-ontology r))))

(defmethod init-ontology ((r foundry-release))
  (log-progress r  "Loading ontology")
  (let ((ontology (load-ontology (ontology-source-file r))))
    (instantiate-reasoner ontology (reasoner r) nil
			  (new 'SimpleConfiguration (new 'NullReasonerProgressMonitor)))
    (log-progress r  "Checking consistency and classifying")
    (assert (check-ontology ontology :classify t) () "Ontology is inconsistent")
    (assert (not (unsatisfiable-classes ontology)) () "Ontology has unsatisfied classes")
    ontology))

(defmethod copy-ontology-annotations ((r foundry-release) source dest &optional filter)
  (loop for annotation in (jss::j2list (#"getAnnotations" (v3kb-ont source)))
	for prop = (make-uri (#"toString" (#"getIRI" (#"getProperty"  annotation))))
	unless (and filter (not (funcall filter prop)))
	  do (add-ontology-annotation  annotation dest)))

(defmethod note-ontologies-merged ((r foundry-release) to-ont)
  (loop with imported-from = !<http://purl.obolibrary.org/obo/IAO_0000412>
	for disp in (dispositions r)
	for versioniri = (getf disp :versioniri) 
	do (add-ontology-annotation (list imported-from versioniri) to-ont)))

(defmethod merged-ontology-pathname ((r foundry-release))
  (merge-pathnames (format nil "~a-merged.owl" (namespace r)) (ensure-release-dir r)))

(defmethod create-merged-ontology ((r foundry-release))
  (let* ((source (ontology r))
	 (destont (new-empty-kb (make-uri (format nil "http://purl.obolibrary.org/obo/~a.owl" (namespace r)))))
	 (dispositions (dispositions r))
	 (license-annotations (license-annotations r)))
    (copy-ontology-annotations r source destont (lambda(prop) (not (eq prop !owl:versionIRI))))
    (note-ontologies-merged r destont)
    (add-ontology-annotation `(,!owl:versionIRI ,(make-versioniri (namespace r) (version-date-string r)))  destont)
    (add-ontology-annotation `(,!rdfs:comment "This version of the ontology is the merge of all its imports and has added axioms inferred by an OWL reasoner") destont)
    (log-progress r "Merging")
    (loop for disp in dispositions
	  for ont = (getf disp :ontology)
	  do (each-axiom ont (lambda(ax) (add-axiom ax destont)) nil))
    (log-progress r "Adding inferences")
    (add-inferred-axioms source :to-ont destont :types
			 (set-difference *all-inferred-axiom-types* '(:disjoint-classes :class-assertions :object-property-characteristics)))
    (when license-annotations (log-progress r "Adding license"))
    (dolist (a license-annotations) (add-ontology-annotation a destont))
    (let ((dest (merged-ontology-pathname r)))
      (to-owl-syntax destont :rdfxml dest))
    (multiple-value-bind (res errorp) (ignore-errors (check-ontology destont))
      (assert (not errorp) (destont) "Hey! The merged ontology (~a) has an error during the consistency test but the original didn't!~%~a" (merged-ontology-pathname r) errorp)
      (unless errorp
	(assert (check-ontology destont) (destont)
		"Hey! The merged ontology (~a) is inconsistent but the original wasn't!~%~a")
	(assert (null (unsatisfiable-classes destont)) (destont) 
		"Hey! The merged ontology (~a) has unsatisfiable classes but the original didn't!~%~a"
		(merged-ontology-pathname r) (replace-with-labels (unsatisfiable-classes destont) destont))))
    ))

(defun make-v3kb-facade (ontology-path)
  (let ((manager (#"createOWLOntologyManager" 'org.semanticweb.owlapi.apibinding.OWLManager)))
    (let* ((ontology (#"loadOntologyFromOntologyDocument" manager (new 'filedocumentsource (new 'java.io.file  (namestring ontology-path))))))
      (make-v3kb :manager (#"getOWLOntologyManager" ontology)
		 :datafactory (#"getOWLDataFactory" (#"getOWLOntologyManager" ontology))
		 :ont ontology
		 :changes (new 'arraylist)))))
  
(defmethod add-version-iris-license-and-rewrite-imports ((r foundry-release) ontology-path add-license)
  "Walk the loaded files. For our files copy to release directory and
  rewrite the imports and add versionIRIs. For other-than-our files
  that don't have versionIRIs we fetch their latest (? what about
  cached versions), save to release directory, and adjust their PURLs"
  (let ((kb (make-v3kb-facade ontology-path)))
    (loop for import-uri in (get-imports-declarations kb)
	  for replacement = (versioned-uri-for r (make-uri import-uri))
	  when replacement
	    do (remove-ontology-imports import-uri kb)
	       (add-ontology-imports replacement kb))
    (when (and add-license (license-annotations r))
      (log-progress r "Adding license to ~a" ontology-path)
      (dolist (a (license-annotations r)) (add-ontology-annotation  a kb)))
    (let ((newversioniri (versioned-uri-for r (get-ontology-iri kb))))
      (when newversioniri
	(set-version-iri kb newversioniri)
	(apply-changes kb)))
    (log-progress r "Rewriting imports for ~a~%" ontology-path)
    (to-owl-syntax kb :rdfxml ontology-path)))

;; If we want to have a live "dev" version, the checked-in files, as
;; with all, need to have a versionIRI that distinguishes them from
;; the released files. Q: Do we need /dev/
;; A: we're going to redirect from /obo/iao/dev/foo.owl to the repo. So yes, they need /dev.
;; However, the release ones shouldn't mention dev.

(defmethod compute-imports-disposition ((r foundry-release))
  "For each of the imports decide if we are using a published
   versioned import or a version we serve. Return a data structure that
   will guide further work, including where to copy something from (or
   nil if we're not going to copy it) the ontologyiri the versioniri and
   where the import was loaded from"
  (loop with date = (version-date-string r)
	for (loaded-from uri ont) in (loaded-documents (ontology r))
	for id = (#"getOntologyID" ont)
	for ontologyiri = (#"toString" (#"get" (#"getOntologyIRI" id)))
	for versioniri = (ignore-errors (#"toString" (#"get" (#"getVersionIRI" id))))
	do
	   (assert (equal ontologyiri uri) (uri ontologyiri)
		   "What's up - uri and ontologyiri don't match ~a - ~a" uri ontologyiri)
	collect
	(list* :ontologyiri (make-uri ontologyiri) :ontology ont 
	       (cond ((equal (third (pathname-directory ontologyiri)) (namespace r))
		      ;; This is one of ours, so make date relative purl <namespace>/<file.owl> -> <namespace>/<date>/<file.owl>
		      (let ((versioned (namestring (merge-pathnames (make-pathname :directory `(:relative ,date)) ontologyiri))))
			(assert (null (pathname-host loaded-from)) () "~a is one of ours but is NOT loaded from file - instead: ~a. Probably an error in catalog-v0001.xml"
				ontologyiri loaded-from)
			(list :copy loaded-from :ontologyiri ontologyiri :original-versioniri (make-uri versioniri)
			      :versioniri (make-uri versioned) :add-license t )))
		     ((equal (pathname-name ontologyiri) (namespace r))
		      ;; This is the main file. <namespace.owl> -> <namespace>/<date><namespace.owl>
		      (let ((versioned (namestring (merge-pathnames (make-pathname :directory `(:relative ,(namespace r) ,date)) ontologyiri))))
			(list :copy loaded-from :versioniri (make-uri versioned) :add-license t :original-versioniri versioniri)))
		     ;; This is an external import. Use its versionIRI
		     ;; Might neede to be careful - if locally cached it could be stale. Not the case for IAO
		     ((not versioniri)
		      (multiple-value-bind (iri dir)
			  (make-local-version-uri r ontologyiri)
			(log-progress r "Didn't get versionIRI for ~a so copying to ~a in release dir" ontologyiri (make-pathname :directory dir))
		      (list :copy ontologyiri :versioniri iri :save-to-dir dir :add-license nil)))
		     (t (list :copy nil :versioniri (make-uri versioniri) :add-license nil))))))

(defmethod make-local-version-uri ((r foundry-release) ontology-iri)
  "We have http://auth/path/file.owl - We make:
  http://purl.obolibrary.org/obo/namespace/date/auth/path/file.owl (if
  path starts with /obo/ we don't include that. Ditto if auth is
  purl.obolibrary.org)."
  (let* ((auth (getf (pathname-host ontology-iri) :authority))
	 (path (pathname-directory ontology-iri))
	 (file (pathname-name ontology-iri))
	 (type (pathname-type ontology-iri)))
    (let ((path (if (equal "obo" (second path))
		    (cons (car path) (cddr path))
		    path))
	  (auth (if (equal auth "purl.obolibrary.org")
		    nil
		    `(:relative ,auth))))
      (values
       (make-uri (namestring (merge-pathnames (make-pathname :directory (rplaca path :relative))
					      (merge-pathnames (make-pathname :directory auth)
							       (make-pathname
								:directory `(:absolute  "obo" ,(namespace r) ,(version-date-string r))
								:name file
								:type type
								:host `(:scheme "http" :authority "purl.obolibrary.org"))
							       )
					      )))
       path))))
      
(defun make-versioniri (namespace &optional (date (version-date-string)))
  "The versioniri for the main artifact"
  (make-uri (format nil "http://purl.obolibrary.org/obo/~a/~a/~a.owl" namespace date namespace)))

(defmethod ensure-release-dir ((r foundry-release))
  "Make sure that the dated release directory is present and if not create it"
  (let ((basename (namestring (translate-logical-pathname (release-directory-base r)))))
    (when (not (#"matches" basename ".*/$"))
      (setq basename (concatenate 'string basename "/")))
    (ensure-directories-exist 
     (merge-pathnames (make-pathname :directory `(:relative ,(version-date-string r)))
		      basename))))

(defmethod write-catalog.xml ((r foundry-release)) 
  (with-open-file (c (merge-pathnames (make-pathname :directory `(:relative ,(version-date-string r))
						     :name "catalog-v001" :type "xml")
				      (release-directory-base r))
		     :if-exists :supersede :direction :output)
    (write-line "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>" c)
    (write-line "<catalog prefer=\"public\" xmlns=\"urn:oasis:names:tc:entity:xmlns:xml:catalog\">" c)
    (loop for d in (dispositions r)
	  for versioniri = (uri-full (getf d :versioniri))
	  for ontologyiri = (uri-full (getf d :ontologyiri))
	  for dir = (getf d  :save-to-dir)
	  when (getf d :copy)
	  do
	     (setq dir (if dir (namestring (make-pathname :directory dir)) "")) 
	     (format c "    <uri name=\"~a\" uri=\"~a~a.~a\"/>~%" versioniri dir (pathname-name versioniri) (pathname-type versioniri))
	     (format c "    <uri name=\"~a\" uri=\"~a~a.~a\"/>~%" ontologyiri dir (pathname-name ontologyiri) (pathname-type ontologyiri)))
    (write-line "</catalog>" c)))

(defmethod copy-files-to-release-directory ((r foundry-release))
  (loop for disp in (dispositions r)
	for copy = (getf disp :copy)
	for dir = (getf disp  :save-to-dir)
	when copy
	  do (let ((dest (merge-pathnames (make-pathname :directory dir :name (pathname-name copy) :type "owl") (ensure-release-dir r))))
	       (log-progress r "Copying ~a.~a to ~a~%" (pathname-name copy) (pathname-type copy) dest)
	       ;; I would use copy-file but it doesn't know about redirects
	       ;; (uiop/stream:copy-file copy dest)
	       (sys::run-program "curl" (list "-L" "--create-dirs" (uri-full (make-uri (namestring copy))) "-o" (namestring dest)))
	       (add-version-iris-license-and-rewrite-imports r dest (getf disp :add-license))))
  (let ((to-be-deleted (directory (merge-pathnames (make-pathname :directory `(:relative :wild) :name :wild :type "bak") (ensure-release-dir r)))))
    (loop for file in to-be-deleted
	  do (log-progress r "Deleting ~a~%" file)
	     '(delete-file file))))

(defmethod write-purl-yaml ((r foundry-release)) 
  (with-open-file (c (merge-pathnames (make-pathname
				       :directory `(:relative ,(version-date-string r))
				       :name "purls"
				       :type "yml")
				      (release-directory-base r))
		     :if-exists :supersede :direction :output)
    (format c "# For 'products:' section~%")
    (format c "- ~a.owl: ~areleases/~a/~a-merged.owl~%~%" (namespace r) (release-purl-base r) (version-date-string r) (namespace r))
    (format c "# For 'entries: (current)' section~%")
    ;; stated main product
    (format c "- exact: /~a-stated.owl~%"  (namespace r))
    (format c "  replacement: ~areleases/~a/~a.owl~%~%" (release-purl-base r) (version-date-string r) (namespace r))
    (loop for product in (additional-products r)
	  do (format c "- exact: /~a~%" product)
	     (format c "  replacement: ~areleases/~a/~a~%~%" (release-purl-base r) (version-date-string r) product))
    (format c "# For 'entries: (versions)' section~%")
    ;; dated main product
    (format c "- exact: /~a/~a.owl~%" (version-date-string r) (namespace r))
    (format c "  replacement: ~areleases/~a/~a-merged.owl~%~%" (release-purl-base r) (version-date-string r) (namespace r))
    ;; dated main product (stated)
    (format c "- exact: /~a/~a-stated.owl~%" (version-date-string r) (namespace r))
    (format c "  replacement: ~areleases/~a/~a.owl~%~%" (release-purl-base r) (version-date-string r) (namespace r))
    ;; Fallthrough for anything else relative to release dir
    (format c "- prefix: /~a/ ~%" (version-date-string r))
    (format c "  replacement: ~areleases/~a/~%~%" (release-purl-base r) (version-date-string r))))


