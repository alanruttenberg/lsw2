;; Takes a text-rendered axiom or class expression. If a subclass or
;; equivalentclass axiom, only the right side class expression is
;; returned, otherwise it is left alone.

(defun trim-base-term (string)
  (if (consp string)
      (if (> (length string) 1)
	  (break)
	  (trim-base-term (car string)))
      (#"replaceFirst" (#"replaceAll" (#"replaceAll" string "\\n" "") "<[^>]+>" "") "^.*?(SubClassOf|EquivalentTo) " "")))

;; old version. Clean up later.
'(defun trim-base-term (html)
  (print-db html)
  (let ((res (#"replaceFirst" html "^.*?(SubClassOf|EquivalentTo) " "")))
    (print-db res)
    res))

;; Given a label for a snomed term, remove the parenthetical term type that is part of the label/fully qualified name.
(defun remove-parenthetical (string)
  (and string
       (#"replaceAll" string "\\s\\([^(]+?\\)" "")))

(defun keep-only-parenthetical (string)
  (if (#"matches" string "(?i).*disease.*")
      "disease"
      (#"replaceAll" string ".*\\s\\(([^(]+?)\\)" "$1")))

;; 
(defun manchester-axiom-to-sexp (axiom)
  (if (null axiom) nil
  (read (make-string-input-stream  (concatenate 'string "(" (#"replaceAll"  axiom "'" "\"") ")")))))

;; Take an sexp-form axiom or class expression and render it as text.
;; Shouldn't have to do this, see note above re consistent use of sexps.
(defun sexp-back-to-manchester(sexp)
  (let ((*print-case* :downcase))
    (#"replaceAll" (prin1-to-string sexp) "\"" "'")))



;; ## FIXME Somewhere along the line labels without spaces didn't get quoted
;; Note: Probably after using result of sexp-back-to-manchester, which
;; renders all labels with #\" and then changes it to #\'
;; Replaces all string-enclosed bits with a token, then put them back,
;; being careful to add #\' to only those that have a space in them.

(defun replace-all-protecting (string regex function protected which-protected &rest which)
  (let ((strings-protecting (mapcar 'car (all-matches string protected which-protected))))
    (let ((count -1))
      (let ((protected-string (replace-all string protected (lambda(e)  (format nil "&~a&" (incf count))) 1)))
	(let ((processed-string (apply 'replace-all protected-string regex function which)))
	  (replace-all processed-string "(&\\d+&)" (lambda(token) 
						    (let ((which (parse-integer (#"replaceAll" token "&" ""))))
								   (nth which strings-protecting)))
		       1))))))

(defun fix-unquoted-manchester-label (string)
  (replace-all-protecting
   string 
   "(\\w+)" 
   (lambda(s) 
     (if (and (upper-case-p (char s 0)) (not (member s '("EquivalentTo" "SubClassOf") :test 'equal)))
	 (concatenate 'string "'" s "'")
	 s))
   "('[^']+?')" 1 1))

;; this works:
;;(axiom-to-lisp-syntax (car (set-to-list (#"getAxioms" (v3kb-ont *snomed*) (caar (gethash !'Disease'@snomed (v3kb-uri2entity *snomed*)))))))

uri = !'disease'@snomed - make-uri-from-label-source (we don't specify the property)
(uri-full !'disease'@snomed)
(rdfs-label !'disease'@snomed)
(annotation !<http://snomed.info/field/Description.term.en-us.preferred> !'disease'@snomed)
(replace-with-labels expr 

#|
What we want with labels.

Easy input, display, automagic works
First iteration: *default-label-source* *default-kb* 
Didn't have *default-label-annotation*
Dangerous, though. Label might not be unique, in which case we can't round trip
What is round trip? Retrieve an axiom from an ontology, render, read, have an equivalent axiom. 
We want to take apart and put back together axioms.
To make sure we can round trip, we need to ensure that IRIs are carried along.
Options:
 1. Always use URI object, never translating to labels. Explicitly coerce to string when needed. If *print-readably* = t then render when printing as ID. Otherwise control what is printed dynamically. To control printing we need to hook print-object or pprint?
 2. Axiom/Class expression objects. They encapsulate sexp of URIs + choices of how to render. Print method renders nicely. When manipulating axioms use modified car, cdr, nth etc to ensure the the results remain wrapped




Currently:

(defmethod make-uri-from-label-source ((instance v3kb) name &optional actual)
  (make-uri (#"toString" (#"getIRI" (to-class-expression (if (find #\space name) (concatenate 'string "'" name "'") name) instance)))))


Desiderata for label source

- Convert URI to label quickly both directions
- Accomodate different annotation properties
- Accomodate different strategies
- list of properties, first found
- label is ambiguous: error
- label is ambiguous: prevent by disambiguating before
- multiple lable for uri: choose one


Currently we name a source and write !'adsads'@source
Sources are ontologies, rdfs:label assumed.

Modify: let a label source also specify an priority list of annotation properties 

Interactions: Manchester renderer, short form provider. Do we need the manchester renderer?
