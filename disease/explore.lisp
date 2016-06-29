(register-namespace "snomed:" "http://snomed.info/id/")
(defun browse-snomed-parent-hierarchy (term &optional (ont *default-kb*))
  (let ((snomed-uri
	 (if (stringp term)
	     (if (#"matches" term "C\\d+")
		 (setq term (make-uri (concatenate 'string "http://snomed.info/id/" (car (snomeds-from-cui term)))))
		 (setq term (parse-manchester-expression ont term)))
	     term)))
    (let ((label (#"replaceAll" (#"replaceFirst" (label-from-uri ont snomed-uri) " \\(.*" "" ) " " "_")))
      (let ((trees (parents-tree snomed-uri ont)))
	(let ((spec (emit-dagre-d3-javascript trees ont))
	      (data-path (temp-directory-path (concatenate 'string label ".js"))))
	  (print data-path)
	  (with-open-file (out data-path :direction :output :if-exists :supersede)
	    (write-string spec out))
	  (show-dag data-path))))))

(defun to-labels (uris)
  (loop for uri in uris do (princ (car (rdfs-label uri))) (terpri)))


  
(defun count-diseases-with-snomed-mappings ()
  (with-open-file (f (namestring (truename "disease:data;disease-families.csv")))
    (loop with count = 1
       for line = (read-line f nil :eof)
       until (eq line :eof) 
       for (nil nil nil descendantId nil) = (split-at-char line #\,)
       for atoms = (umls-concept-atoms descendantId)
       when (some #'identity (mapcar (lambda(el) (equal (cdr (assoc  :ROOT-SOURCE el)) "SNOMEDCT_US")) atoms))
       do (incf count)
       when (zerop (mod count 100))
       do (princ ".")
	 finally (return count))))


(defun uncached-source-terms-for-initial-concepts (&aux (todo (make-hash-table :test 'equalp)))
  (with-open-file (f (namestring (truename "disease:data;disease-families.csv")))
    (loop 
       for line = (read-line f nil :eof)
       until (eq line :eof) 
       for (nil nil nil descendantId nil) = (split-at-char line #\,)
       for atoms = (umls-concept-atoms descendantId)
       do
	 (loop for result in atoms
	    for code = (cdr (assoc :code result))
	    for (source id) = (source-term-source-and-id-from-url code)
	    unless (umls-source-term-info source id :probe t)
	    do (setf (gethash code todo) t))))
  todo)


(defun count-cuis ()
  (let ((table (make-hash-table :test 'equalp)))
    (with-open-file (f (namestring (truename "disease:data;disease-families.csv")))
      (loop with diseases = (make-hash-table :test 'equalp)
	 for line = (read-line f nil :eof)
	 until (eq line :eof) 
	 for (familyId familyLabel numDescendants descendantId descendantLabel) = (split-at-char line #\,)
	 do (setf (gethash descendantId table) t)(setf (gethash familyid table) t)))
    table))

(defun dwim-class (string &optional (ont *default-kb*))
  (if (uri-p string)
      string
      (if (stringp string)
	  (if (#"matches" string "C\\d+")
	      (make-uri (concatenate 'string "http://snomed.info/id/" (car (snomeds-from-cui string))))
	      (make-uri (#"toString" (#"getIRI" (parse-manchester-expression ont (if (find #\space string) (concatenate 'string "'" string "'") string)))))))))
	  
(defun all-relevant-axioms (class ont &key (rendered t))
  (mapcar 'car
	  (append 
	   (get-rendered-referencing-axioms (dwim-class class ont) :class ont t)
	   (loop for c in (ancestors (dwim-class class ont) ont)
	      append (get-rendered-referencing-axioms c :class ont t)
		))))


(defun all-relevant-axioms (class &optional (ont *default-kb*))
  (mapcar 'car
	  (append 
	   (get-rendered-referencing-axioms (dwim-class class ont) :class ont t)
	   (loop for c in (ancestors (dwim-class class ont) ont)
	      append (get-rendered-referencing-axioms c :class ont t)
		))))



(defun prettyify-axioms (rendered-axioms &optional (ont *default-kb*))
  (sort (mapcar (lambda(el)
	    (let ((html el))
	      (#"replaceAll" (#"replaceFirst" (#"replaceAll" (#"replaceAll" html "\\n" "") "<[^>]+>" "") "^.*?(SubClassOf|EquivalentTo) " "") "\\s\\([^(]+?\\)" "")))
		rendered-axioms)
	'string-lessp))

(defun labels-matching(regex &optional (ont *default-kb*))
  (let ((them nil))
    (maphash (lambda(uri label)
	       (when (#"matches" (car label) regex) (push (car label) them)))
	     (rdfs-labels ont))
    them))
		       
;; (defun most-specific-axioms (class)
  
       
;  (read-from-string  (concatenate 'string "(" (substitute #\" #\' "'Vasculitis' and ('Role group' some (('Associated morphology' some 'Inflammation') and ('Finding site' some 'Systemic vascular structure')))") ")"))
