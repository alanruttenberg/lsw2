(in-package :cl-user)

;; add-rdfxml-label-comments will write a copy of the input ontology in
;; RDF/XML that adds XML comments for opaque IRIs, making the file easier
;; to view as text.

;; For XML elements it writes the comment before the usage of the
;; property. For attributes it writes the comment after the attribute. It
;; also replaces the comment IRIs that the OWLAPI adds before each axiom
;; with the label.

;; in-path is the location of the source ontology
;; out-pathn is where the copy should be written 

;; label-source is an instance of label-source that serves as the
;; dictionary mapping IRIs to labels. It is constructued with a form like

;; (make-instance 'label-source
;;                :sources <a list of ontologies loaded using load-ontology>
;;                :include-imports-closure t 
;;                :label-annotation-properties
;;                <properties to be used as labels, in order of priority>
;;                )

;; The flag :include-imports-closure ensures that labels from imported
;; ontologies are registered.

;; Generally, you will only need to the sources load a single ontology,
;; because of the imports closure. But suppose you want to rewrite an
;; imported file. e.g.

;; Ontology A imports Ontology B

;; You would create the label source for A, but and use that for both
;; the rewrite of A and the rewrite of B, since a label source constructed
;; from B only would will terms defined in A and only used by IRI in B.


(defun add-rdfxml-label-comments (in-path out-path label-source)
  (assert (not (equalp in-path out-path)) ()
          "Can't overwrite the original file")
  (with-open-file (in in-path :external-format :utf-8)
    (with-open-file (out out-path
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :supersede
                         :external-format :utf-8)
      (loop for line = (read-line in nil :eof)
	    until (eq line :eof)
            with look-for-namespaces = t
            with tag-regex
            with namespaces
            ;; a regex to get the target IRI for rdf:about= or rdf:resource=
            with attribute-value-regex
              = (#"compile" 'java.util.regex.pattern "(rdf:(about|resource)=(\"[^\"]*?\")/?>)")
            do
               ;; Look for the namespace definitions and collect into an
               ;; alist. Look for namespaces will become false once
               ;; we've read at least one and then don't find one on a
               ;; line.
               (tagbody next
                  (when look-for-namespaces
                    (let ((namespace (all-matches line "xmlns:([^=]*?)=\"([^\"]*?)\"" 1 2 )))
                      (if namespace
                          (progn (push (car namespace) namespaces))
                          (when namespaces
                            (setq look-for-namespaces nil)
                            ;; Create a regex which looks for any of the
                            ;; namespaces followed by : followed by an
                            ;; opaque IRI
                            (setq tag-regex (format nil "<(~{~a~^|~}):(\\w+_\\d+)"
                                                    (mapcar 'car namespaces))))
                          ))
                    (write-line line out)
                    (setq line (read-line in))
                    (go next)))
               ;; Now we're looking for either a tag or an attribute value 
               (when tag-regex

                 ;; but first, in order to make sure we don't add a
                 ;; comment more than once, delete any xml comments
                 ;; unless they are on a line of their own.
                 (unless (#"matches" line "^\\s*<!-.*->\\s*$")
                   (setq line (#"replaceAll" line "<!-.*?->" "")))

                 (let ((tag-replaced
                         (replace-all
                          line  tag-regex
			  (lambda(namespace id)
                            (let ((label (label-from-uri
                                          label-source
                                          (make-uri (format nil "~a~a"
                                                            (second (assoc namespace namespaces :test 'equalp)) id)))))
			      (format nil "~a<~a:~a"
                                      (if label (format nil "<!- ~a ->" label) "")
                                      namespace id)))
                                                   1 2)))
                   ;; If we've found a tag, we've prepended the label in
                   ;; a comment. Check if we found a tag by comparing to
                   ;; the line as read
                   (if (not (equalp tag-replaced line))
                       (write-line tag-replaced out)
                       ;; If not, we'll check for rdf:about/resources
                       (let ((attribute-value-replaced
                               (replace-all
                                line  attribute-value-regex
				(lambda(whole iri)
                                  (let ((label (label-from-uri label-source (make-uri (subseq iri 1 (- (length iri) 1))))))
			            (format nil "~a~a"
				            whole
                                            (if label (format nil "<!- ~a ->"  label) ""))))
                                1 3)
                               ))
                         ;; If we've found a rdf:about/resource, we've
                         ;; appended the label in a comment. Check if we
                         ;; found a that by comparing to the line as
                         ;; read
                         (if (not (equalp attribute-value-replaced line))
                             (write-line attribute-value-replaced out)
                             ;; if not, we'll deal with the IRI comments
                             ;; that the OWLAPI puts before each
                             ;; defining axiom.  by replacing the IRI
                             ;; with the label
                             (let ((comment-replaced
                                     (replace-all line  "<!- (\\S+) ->" 
						  (lambda(iri)
                                                    (let ((label (label-from-uri label-source (make-uri iri))))
                                                      (format nil "<!- ~a ->" (or label iri))))
                                                  1)))
                               ;; we don't care if there was a change or
                               ;; not - just write the (possibly null)
                               ;; replacement.
                               (write-line comment-replaced out)))))))
            ))))

