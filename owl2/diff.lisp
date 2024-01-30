(in-package :cl-user)
(defun get-ontology-annotations (ont)
  (jss::j2list (#"getAnnotations" (v3kb-ont ont))))

(defun diff-ontologies (ont1 ont2 &key (report t)  (compare-with-labels nil) (include-imports t) additional-label-properties
                                    (label-ont1 ont1)  (label-ont2 ont2))
  "Print out differences between ont1 and ont2, including ontology and version IRI, imports, ontology annotations, and axioms. Inputs can either be v3kbs, java ontology objects, uris or strings. In the latter 2 cases the ontologies will be loaded"
  (flet ((format-if (&rest args)
           (when report (apply 'format args)))
         (ppax-if (&rest args)
           (when report (apply 'ppax args)))
         (maybe-labels (ont axs)
           (if compare-with-labels
               (let ((*default-kb* ont))
                 (replace-with-labels axs))
               axs)))
  (let ((table (make-hash-table :test 'equalp))
	(only-in-1 nil)
	(only-in-2 nil)
	(in-both nil)
	(ont1 (if (v3kb-p ont1) ont1 (load-ontology ont1)))

	(ont2 (if (v3kb-p ont2) ont2 (load-ontology ont2)))
        (labels1 (when compare-with-labels
                   (make-instance
                    'label-source :key :diffont-1
                    :sources (list label-ont1)
                    :include-imports-closure t
                    :label-annotation-properties (list* !rdfs:label !skos:prefLabel additional-label-properties  ))))
        (labels2 (when compare-with-labels
                   (make-instance
                    'label-source :key :diffont-2
                    :sources (list label-ont2)
                    :include-imports-closure t
                    :label-annotation-properties (list* !rdfs:label !skos:prefLabel additional-label-properties)))))
        
    (loop for ax in (maybe-labels ont1 (mapcar 'axiom-to-lisp-syntax (get-ontology-annotations ont1)))
	  do (setf (gethash ax table) '(1)))
    (loop for ax in (maybe-labels ont2 (mapcar 'axiom-to-lisp-syntax (get-ontology-annotations ont2)))
	  do (push  2 (gethash ax table)))
    (let ((*default-kb* ont1))
      (each-axiom ont1
	  (lambda(ax) 
            (let ((lax (axiom-to-lisp-syntax ax)))
              (when compare-with-labels (setq lax (replace-with-labels-from-label-source labels1 lax)))
              (pushnew 1 (gethash lax table))))
        include-imports))
    (let ((*default-kb* ont2))
      (each-axiom ont2
	  (lambda(ax) 
            (let ((lax (axiom-to-lisp-syntax ax)))
              (when compare-with-labels (setq lax (replace-with-labels-from-label-source labels2 lax)))
              (pushnew 2 (gethash lax table))))
        include-imports))
    (maphash (lambda(k v)
	       (cond ((equal v '(1))
		      (push k only-in-1))
		     ((equal v '(2))
		      (push k only-in-2))
		     (t (push k in-both))))
	     table)
    (multiple-value-bind (annotations axioms) (partition-if (lambda(e) (eq (car e) 'annotation-assertion)) in-both)
      (format-if t "Common to both: ~a axioms and ~a ontology annotations~%" (length axioms) (length annotations)))
    (unless (eq (get-ontology-iri ont1) (get-ontology-iri ont2))
      (format-if t "~%Ontology IRIs differ: ~a, ~a~%" (get-ontology-iri ont1) (get-ontology-iri ont2)))
    (unless (eq (get-version-iri ont1) (get-version-iri ont2))
      (format-if t "~%Version IRIs differ: ~a, ~a~%" (get-version-iri ont1) (get-version-iri ont2)))
    (let ((imports1 (get-imports-declarations ont1))
	  (imports2 (get-imports-declarations ont2)))
      (unless (equalp imports1 imports2)
	(let ((only-in-1 (set-difference imports1 imports2 ))
	      (only-in-2 (set-difference imports2 imports1 )))
	  (when only-in-1
	    (format-if t "Imports only in first: ~{~a~^, ~}~%" only-in-1))
	  (when only-in-2
	    (format-if t "Imports only in second: ~{~a~^, ~}~%" only-in-2)))))
    (multiple-value-bind (annotations axioms) (partition-if (lambda(e) (eq (car e) 'annotation-assertion)) only-in-1)
      (when (or annotations axioms)
	(format-if t "~%~a only in first~%" (+ (length annotations) (length axioms)))
	(when annotations
	  (let ((*default-kb* ont1))
	    (ppax-if annotations)))
	(when axioms
	  (let ((*default-kb* ont1))
	    (ppax-if axioms)))))
    (multiple-value-bind (annotations axioms) (partition-if (lambda(e) (eq (car e) 'annotation-assertion)) only-in-2)
      (when (or annotations axioms)
	(format-if t "~%~%~a Only in second~%" (+ (length annotations) (length axioms)))
	(when annotations
	  (let ((*default-kb* ont2))
	    (ppax-if annotations)))
	(when axioms
	  (let ((*default-kb* ont2))
	    (ppax-if axioms)))))
    table)))

(defun diff-ontologies-by-import (ont1 ont2 &key (compare-with-labels nil) additional-label-properties)
  (loop for (path1 nil before) in (loaded-documents ont1)
        for (path2 nil after) = (find (pathname-name path1) (loaded-documents ont2) :test 'equalp :key (lambda(x) (pathname-name (car x))))
        when after
          do (let ((before-ont (make-kb-from-java-object before))
                   (after-ont (make-kb-from-java-object after)))
               (format t "~&~%************************************************************~%")
               (terpri) (:print-db path1 path2) (terpri)
               (diff-ontologies before-ont
                                after-ont
                                :label-ont1 ont1
                                :label-ont2 ont2
                                :compare-with-labels compare-with-labels
                                :additional-label-properties additional-label-properties
                                :include-imports nil))))

'(diff-ontologies (load-ontology "~/repos/ngiis/Ontology/crs-db.owl") (load-ontology "~/repos/ngiis/Ontology/crs-db-protege.owl") :include-imports nil :compare-with-labels t :label-ont1 before :label-ont2 before)
