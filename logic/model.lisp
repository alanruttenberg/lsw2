(in-package :logic)

#|
Models

Mace4 and z3 create models, and I create them by hand. There are probably other model builders that we can use.
What matters:
 - How was the model made? E.g. mace4 lets you specify domain size bounds, and both allow timeouts. 
   For hand-made models there's a seed, the generated rules, and the full models
 - How to transform the output of the model builder to a form that is understandable. In our case that 
   begins by converting the representation to a set of tuples. There could be a need to go beyond tuples
   for example z3 with numerical types might yield inequalities.
 - Names of the domain elements. Both mace4 and z3 allow names specified in the theory to appear in the model.
   - There may be equalities. Mace4, for example, will assign different names the same number, where number is the
     identifier for the element of the domain. I don't think this happens in z3.
   - Ideally the names given to generated domain elements should be informative. Since the tools don't know what
     is informative to you, they can't do that. For example, for BFO, third arguments for ternary relationships are 
     times, and it would be useful to name them t1,t2...
     Another strategy would be to look at unary predicates and designate some as naming relevant, using either the predicate
     name or abbreviation as a base for numbered individuals.
 - Models might not be correct. While mace4 and z3 generate models, I write them and they need to be evaluated in order to
   determine whether a putative model is in fact a model of the theory. 
 - Output options. While currently I print the tuples one could imaging other outputs, for example a graphical one 
   laid out by dot.
 - Presentation focus. Sometimes I am not interested in instantiation, existence - I just want to see what happened with 
   some of the relations. In that case display should filter out what I don't want to see. As another example, in reviewing
   BFO models I'm interested in only the most specific type. So roughly, I come in with a hierarchy (bfo classtree), 
   know that class if the third element of an instance-of tuple, need to gather all tuples that only differ in this position
   and then select the most specific, ignoring the rest.
   Another example: In many cases I'm not interested in existence and instantiation of temporal regions, so I want to ignore them.
 - Goodness. Sometimes I'm checking models to see whether some propery holds. While I can in cases instead look for a proof,
   in other cases it might be easier to check certain things with codes.
 - Interactive use. Sometimes I am exploring models in the repl repeatly. It should be as easy as possible to 
   select things like filtering, sorting, and naming strategies.

External
 - A model is called for.
 - If successful the model is transformed to unordered tuples.

Handmade
 - A seed is given
 - The seed is expanded (different ways to do this)

Both
 - Model transformed for display. Transformation involves some combination of renaming, filtering, sorting.
 - Model is displayed - generally just printed
 - Model checked for properties

Division of labor.

Class specialization certainly for different external tools, to get them to the point 
of tuples.

Filter/sort/relabel are methods on tuple-model but take an optional tuples argument, 
which defaults to the model tuples.

|#

(defclass tuple-model ()
  ((tuples :accessor tuples :initarg :tuples :initform nil)
   (equivalences :accessor equivalences :initarg :equivalences :initform nil)))
   
(defclass model-generator-model (tuple-model)
  ((model-theory :accessor model-theory :initarg :model-theory :initform nil)
   (invocation :accessor invocation :initarg :invocation :initform nil) 
   (raw-form :accessor raw-form :initarg :raw-form :initform nil)))

(defclass mace4-model (model-generator-model)
  ((cooked-output :accessor cooked-output :initarg :cooked-output :initform nil)))

(defclass z3-model (model-generator-model)())

(defclass seeded-model (tuple-model)
  ((model-seed :accessor model-seed :initarg :model-seed :initform nil)))

(defmethod pprint-model ((m tuple-model) &optional (tuples (tuples m)))
  (loop for form in tuples
	do (let ((*print-case* :downcase)) (format t "~a~%" form))))

(defmethod sort-model-alphabetically ((m tuple-model) &optional (tuples (tuples m)))
  (sort (copy-list tuples) 'string-lessp :key 'prin1-to-string))

(defmethod sort-model-by-position ((m tuple-model) position &optional (tuples (tuples m)))
  (flet ((compare-natural (a b)
	   (if (and (some 'digit-char-p (string a))
		    (some 'digit-char-p (string b)))
	       (< (parse-integer (caar (all-matches (string a) "(\\d+)" 1)))
		  (parse-integer (caar (all-matches (string b) "(\\d+)" 1))))
	       (string-lessp (string a) (string b)))))
    (sort (copy-list tuples) #'compare-natural :key #'(lambda(e) (nth position e)))))

(defmethod sort-temporal ((m tuple-model) &optional (tuples (tuples m)))
  (let* ((other (tuples-without-arity m 3 tuples))
	 (ternary (tuples-with-arity m 3 tuples)))
    (append (sort-model-alphabetically m other)
	    (sort-model-by-position m 3 ternary))))

(defmethod only-keep-predicates ((m tuple-model) predicates &optional (tuples (tuples m)))
  (remove-if-not (lambda(e) (member e predicates)) tuples :key 'car))

(defmethod discard-predicates ((m tuple-model) predicates &optional (tuples (tuples m)))
  (remove-if (lambda(e) (member e predicates)) tuples :key 'car))

(defmethod tuples-with-arity ((m tuple-model) arity &optional (tuples (tuples m)))
  (remove (1+ arity) tuples :key 'length :test-not '=))

(defmethod tuples-without-arity ((m tuple-model) arity &optional (tuples (tuples m)))
  (remove (1+ arity) tuples :key 'length :test '=))

(defmethod model-domain ((m tuple-model))
 (remove-duplicates (apply 'append (mapcar 'cdr (tuples m)))))

(defmethod model-domain-size ((m tuple-model))
  (length (model-domain m)))

(defmethod model-predicates ((m tuple-model))
  (remove-duplicates (mapcar 'car (tuples m))))

(defmethod print-object ((m tuple-model) stream)
  (print-unreadable-object (m stream :type t :identity nil)
    (format stream "domain size ~a, ~a predicates, ~a tuples"
	    (model-domain-size m)
	    (length (model-predicates m))
	    (length (tuples m)))))

(defun find-model (with spec &rest keys)
  (let ((model
	  (ecase with
	    (:mace4 (apply 'mace4-find-model spec  keys))
	    (:z3 (apply 'z3-find-model spec  keys)))))
    (when (typep model 'tuple-model)
      (setf (model-theory model)  spec)
      (setf (invocation model)
	    (ecase with
	      (:z3 `(z3-find-model ',spec ,@keys))
	      (mace4 `(mace4-find-model ',spec ,@keys)))))
    model))

;; Relabeling functions

(defmethod relabel-simple ((m tuple-model) &optional (tuples (tuples m)))
  (let* ((to-be-replaced (remove-duplicates (mapcan #'(lambda(e) (copy-list (cdr e))) tuples )))
	 (replacements 
	   (loop for name in to-be-replaced
		 for i from 1 
		 collect (list name (intern (format nil "~a" (code-char (+ -1 i (char-code #\A))) 'keyword))))))
    (tree-replace (lambda(e)
		    (or (second (assoc e replacements))
			e))
		  tuples)))

(defmethod relabel-by-position ((m tuple-model) position prefix &optional (tuples (tuples m)))
  (relabel-by-positions m (list position) prefix tuples))

(defmethod relabel-by-positions ((m tuple-model) positions prefix &optional (tuples (tuples m)))
  (let* ((to-be-replaced (remove nil (remove-duplicates (mapcan #'(lambda(e) (mapcar (lambda(n) (nth n e)) positions)) tuples ))))
	 (replacements 
	   (loop for name in to-be-replaced
		 for i from 1 
		 collect (list name (intern (format nil "~a~a" prefix i) 'keyword)))))
    (tree-replace (lambda(e)
		    (or (second (assoc e replacements))
			e))
		  tuples)))

(defmethod relabel-by-predicate-and-positions ((m tuple-model) predicates-and-positions prefix &optional (tuples (tuples m)))
  (let* ((to-be-replaced 
	   (remove nil (remove-duplicates
			(mapcan #'(lambda(e)
				    (loop for (pred . positions) in predicates-and-positions
					  when (eq (car e) pred)
					    append
					    (mapcar (lambda(n) (nth n e)) positions)))
				tuples ))))
	 (replacements 
	   (loop for name in to-be-replaced
		 for i from 1 
		 collect (list name (intern (format nil "~a~a" prefix i) 'keyword)))))
    (tree-replace (lambda(e)
		    (or (second (assoc e replacements))
			e))
		  tuples)))

(defmethod relabel-multiple ((m tuple-model) prefix-pred-positions &optional (tuples (tuples m)))
  (loop for (prefix . pred-positions) in prefix-pred-positions
	do (setq tuples (relabel-by-predicate-and-positions m pred-positions prefix tuples)))
  tuples)
							    


;; example

(defmethod display-part-of-model ((m tuple-model))
  (pprint-model m
		(sort-temporal m
			       (only-keep-predicates m '(:b-has-part :b-part-of :part-of)
						     (relabel-multiple m
								       '(("U" (:universal 1))
									 ("P" (:b-part-of 1 2) (:b-has-part 1 2))
									 ("T" (:part-of 3))))))))

(defun find-part-of-model (spec)
  (let ((model (find-model :z3 spec)))
    (if (not (typep model 'tuple-model))
	model
	(progn
	  (display-part-of-model model)
	  model))))

