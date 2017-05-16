(in-package :cl-user)

;; The essential ideas are here: http://helix-web.stanford.edu/psb06/zhang_s.pdf

(def-uri-alias "definition" !obi:IAO_0000115)
(def-uri-alias "definition-source" !obi:IAO_0000119)
(def-uri-alias "definition-editor" !obi:IAO_0000117)
(def-uri-alias "preferred-term" !obi:IAO_0000111)
(def-uri-alias "alternative-term" !obi:IAO_0000118)
(def-uri-alias "example-of-usage" !obi:IAO_0000112)
(def-uri-alias "curation-status" !obi:IAO_0000078)
(def-uri-alias "editor-note" !obo:IAO_0000116)
(def-uri-alias "curator-note" !obo:IAO_0000232)

(defun reactome-term-uri (label)
  (make-uri (concatenate 'string "http://purl.obolibrary.org/obo/reactome/record/" (string label))))

(defun generate-class-axioms (frames &optional (uri-constructor 'reactome-term-uri))
  (let ((all-classes frames)
	(all-properties (mapcar 'second (cdddr (gethash :clips_top_level_slot_class frames)))))
    (collecting-axioms
     (maphash (lambda(class list)
		(unless (or (keywordp class) (eq class 'user))
		  (let ((is-as (cdr (find-if (lambda(el) (and (consp el) (eq (car el) '|is-a|))) list)))
			(comment (and (stringp (car list)) (list (car list)))))
		    (as `(declaration (class ,(funcall uri-constructor class))))
		    (as `(annotation-assertion !rdfs:label ,(funcall uri-constructor class)
					       ,(replace-all (string class) "([a-z])([A-Z])"
							     (lambda(a b) (format nil "~a ~a" a (string-downcase b))) 1 2)))
		    (and comment (as `(annotation-assertion !rdfs:comment ,(funcall uri-constructor class) ,comment)))
		    (as  (generate-local-class-restrictions class list all-classes all-properties uri-constructor))
		    (print-db is-as class list )
		    (dolist (is-a is-as)
		      (if (and is-a (not (keywordp is-a)) (not (eq is-a 'user)))
			(as `(sub-class-of ,(funcall uri-constructor class) ,(funcall uri-constructor is-a))))))))
	      all-classes)
     )))

(defun generate-local-class-restrictions (class props all-classes all-properties uri-constructor)
  (collecting-axioms
   (loop for item in props
	 when (and (consp item)
		   (member (car item) '(|single-slot| |multislot|)))
	   do
	      (assert  (or (eq (second item) '|name_|)
			   (member (second item) all-properties) () "there's a local restriction on ~a but ~a isn't declared a property" (second item) (second item)))
	   and append
	       (let ((base (find (second item)
				 (gethash :clips_top_level_slot_class all-classes)
				 :key (lambda(el) (if (consp el) (second el))))))
		 (print-db base)
		 (let ((diff (remove '|value| (remove '|user-facet| (set-difference (cddr item) (cddr base) :test 'equalp)
						      :key 'car) :key 'car)))
		   (print-db diff)
		   (let* ((type (second (find '|type| (cddr base) :key 'car)))
			  (cardinality (cdr (find '|cardinality| diff :key 'car)))
			  (object-property-range (cdr (find '|allowed-classes| diff :key 'car)))
			  ;; in the general case we would need to handle this, but in this case all data property ranges are global
			  ;;		(data-property-range (unless (eq type 'instance) (cons type (cdr (find '|allowed-values| diff :key 'car)))))
			  (object-property? (eq 'instance type)))
		     (print-db type cardinality object-property-range object-property?)
		     (append
		      (if cardinality
			  (progn
			    (unless (eql 0 (first cardinality))
			      (as `(sub-class-of ,(funcall uri-constructor  class) 
						 (,(if object-property? 'object-min-cardinality 'data-min-cardinality)
						  ,(first cardinality) ,(funcall uri-constructor  (second item))))))
			    (unless (eq (second cardinality) '?variable)
			      (as `(sub-class-of ,(funcall uri-constructor  class)
						 (,(if object-property? 'object-max-cardinality 'data-max-cardinality)
						  ,(second cardinality) ,(funcall uri-constructor  (second item)))))))
			  (when object-property-range
			    (assert object-property? () "Curious about that - I don't expect data properties (~a:~a) to have allowed-classes ~a" item type diff)
			    (progn
			      (if (= (length object-property-range) 1)
				  (as `(sub-class-of ,(funcall uri-constructor  class)
						     (object-all-values-from ,(funcall uri-constructor  (second item))
									     ,@(mapcar  uri-constructor  object-property-range))))
				  (as `(sub-class-of ,(funcall uri-constructor  class)
						     (object-all-values-from ,(funcall uri-constructor  (second item))
									     (object-union-of ,@(mapcar  uri-constructor  object-property-range)))))
				  )))
			  ))))))))

(defun generate-property-axioms (frames uri-constructor)
  (collecting-axioms
   (let ((all-classes frames)
	 (all-properties (mapcar 'second (cdddr (gethash :clips_top_level_slot_class frames)))))
     (let ((top (gethash :clips_top_level_slot_class all-classes))
	   (slot2class (generate-slot2class all-classes all-properties uri-constructor)))
       (loop for def in (remove '|role| (cdr top) :key 'car) 
	  for name = (second (print-db def))
	     for functional = (eq (car def) '|single-slot|)
	     for is-as =  (cdr (find '|subslot-of| def :key (lambda(e) (and (consp e) (car e)))))
	  for inverse = (second (assoc '|inverse-slot| (cddr def)))
	  for range = (remove :thing (cdr (assoc '|allowed-classes| (cddr def))))
	  for comment = (second (assoc '|comment| (cddr def)))
	  for values = (second (assoc '|values| (cddr def)))
	  for cardinality = (rest (assoc '|cardinality| (cddr def)))
	  for type = (second (assoc '|type| (cddr def)))
	  for domain = (gethash name slot2class)
	  for uri = (funcall uri-constructor  name)
	  when (member name all-properties)
	    do
	       (when is-as
		 (dolist (is-a is-as)
		   (unless (or (keywordp is-a) (eq is-a 'user))
		   (as `(sub-object-property-of ,uri ,(funcall uri-constructor is-a))))))
	    (as `(annotation-assertion !rdfs:label ,uri ,(replace-all (string name) "([a-z])([A-Z])" (lambda(a b) (format nil "~a ~a" a (string-downcase b))) 1 2)))
	  (cond ((member type '(string symbol integer))
		 (as `(declaration (data-property ,uri)))
		 (when comment (as `(annotation-assertion !rdfs:comment ,uri ,comment)))
		 (when (or functional (equal cardinality '(0 1))) (as `(functional-data-property ,uri)))
		 (when domain 
		   (if (= (length domain) 1)
		       (as `(data-property-domain ,uri ,@(mapcar uri-constructor  domain)))
		       (as `(data-property-domain ,uri (object-union-of ,@(mapcar uri-constructor  domain))))))
		 (when (eq type 'integer)
		   (as `(data-property-range ,uri !xsd:int)))
		 (when values
		   (as `(data-property-range ,uri (data-one-of ,@values))))
		 (when inverse
		   (as `(inverse-object-properties ,uri ,(funcall uri-constructor  inverse)))))
		(t
		 (as `(declaration (object-property ,uri)))
		 (when comment (as `(annotation-assertion !rdfs:comment ,uri ,comment)))
		 (when (or functional (equal cardinality '(0 1))) (as `(functional-object-property ,uri)))
		 (when domain 
		   (if (= (length domain) 1)
		       (as `(object-property-domain ,uri ,@(mapcar uri-constructor  domain)))
		       (as `(object-property-domain ,uri (object-union-of ,@(mapcar uri-constructor  domain))))))
		 (when  range 
		   (if (= (length range) 1)
		       (as `(object-property-range ,uri ,@(mapcar uri-constructor  range)))
		       (as `(object-property-range ,uri (object-union-of ,@(mapcar uri-constructor  range))))))))
	  )))))

(defun generate-slot2class (all-classes all-properties uri-constructor)
  (declare (ignorable all-properties uri-constructor))
  (let ((slot2class (make-hash-table)))
    (maphash (lambda(class list)
	       (unless (or (keywordp class) (eq class 'user))
		 (loop for el in list
		      when (and (consp el) (member (car el) '(|single-slot| |multislot|)))
		      do (pushnew class (gethash (second el) slot2class)))))
	     all-classes)
    slot2class))  
		   
(defun generate-disjoints (all-classes uri-constructor)
  (collecting-axioms
   (let ((parent2child (make-hash-table))
	 ;;(all-disjoints nil)
	 )
     (maphash (lambda(class list)
		(unless (or (keywordp class) (eq class 'user))
		  (let ((is-a (second (find-if (lambda(el) (and (consp el) (eq (car el) '|is-a|))) list))))
		    (if is-a
			(unless (or (keywordp is-a) (eq is-a 'user))
			  (pushnew (funcall uri-constructor  class) (gethash (funcall uri-constructor  is-a) parent2child)))))))
	      all-classes)
     (loop with queue = (list (funcall uri-constructor  '|DatabaseObject|))
	for root = (pop queue)
	for children = (gethash root parent2child)
	when children
	do 
	(when (>= (length children) 2)
	  (as `(disjoint-classes ,@children)))
	do (setf queue (append queue children))
	until (null queue)
	))))


(defun owl-ontology-from-frames (frames prologue uri-constructor ontology-name)
  (with-ontology from-frames (:collecting t
			       :base (funcall uri-constructor "")
			       :about (funcall uri-constructor (concatenate 'string ontology-name ".owl")))
    ((as prologue )
     (asq (declaration (annotation-property !definition))
	  (annotation-assertion !rdfs:label !definition  "definition")
	  (declaration (annotation-property !definition-source))
	  (annotation-assertion !rdfs:label !definition-source  "definition source")
	  (declaration (annotation-property !editor-note))
	  (declaration (annotation-property !dc:creator))
	  (declaration (annotation-property !definition-editor))
	  (declaration (annotation-property !dc:contributor))
	  (annotation-assertion !rdfs:label !editor-note  "editor note")
	  (annotation-assertion !rdfs:label !definition-editor  "definition editor"))
     (as (generate-property-axioms frames uri-constructor) 
	 (generate-class-axioms frames uri-constructor)
	 (generate-disjoints frames uri-constructor)
	 )
     (loop for a in (gethash 'additional-assertions frames) do
       (as (with-open-file (f a)
	     (loop for form = (read f nil :eof) until (eq form :eof) collect form)))))
    from-frames))



(defun read-pont (pont &optional (hash (make-hash-table)))
  (let ((readtable (copy-readtable)))
    (setf (readtable-case readtable) :preserve)
    (set-syntax-from-char #\; #\space readtable *saved-readtable*)
    (set-syntax-from-char #\+ #\space readtable *saved-readtable*)
    (set-syntax-from-char #\: #\space readtable *saved-readtable*)
    (flet ((merge-to-hash (key value)
	     (if (eq key :clips_top_level_slot_class)
		 (setf (gethash key hash) (union (gethash key hash) value :test 'equalp))
		 (setf (gethash key hash) value))))
      (let ((*readtable* readtable))
					;      (with-input-from-string (f string)
	(with-open-file (f pont)
	  (loop for form = (read f nil :eof)
		until (eq form :eof)
		collect form into forms
		finally (return (loop for el in
					     (tree-replace 
					      (lambda(e)
						(if (eq e '|defclass|) 
						    'defclass-frames
						    (if (and (symbolp e) (eql (search "%3A" (string e)) 0))
							(intern (string-upcase (subseq (string e) 3)) 'keyword)
							e)))
					      forms)
				      do (if (and (consp el) (eq (car el) 'defclass-frames))
					     (merge-to-hash (second el) (if (stringp (caddr el)) (cdddr el) (cddr el)))
					     )
				      finally (return hash)))))))))

(defun read-directory-of-pont (dir)
  (let ((e (make-hash-table)))
    (dolist (f (directory (merge-pathnames "*.pont" dir)))
      (read-pont f e))
    e))

;
(owl-ontology-from-frames (setq e1 (read-directory-of-pont "/Volumes/Chair/ Downloads/2017-05-12/EngineeringOntologies/"))
			  nil
			  (lambda(u) (make-uri nil (format nil "ex:~a" (string u))))
			  "engineering")
