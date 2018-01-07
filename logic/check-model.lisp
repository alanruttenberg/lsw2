(in-package :logic)

;; create a set of quantified sentences negating everything but the supplied assertions.
(defun positive-ground-model-closure-axiom (props)
  (multiple-value-bind (predicates constants) (formula-elements `(:and ,@props))
    (list* :and 
	   `(:forall (?x) (:or ,@(loop for c in constants collect
				       `(:= ?x ,c))))
	   (loop for (predicate arity) in predicates
		 collect
		 (let ((v (with-logic-vars (v arity) v)))
		   `(:forall ,v
		      (:implies (,predicate ,@v)
			  ,(let ((disjuncts
				  (loop for prop in props
				   when (and (eq (car prop) predicate)
					     (= (length (cdr prop)) arity))
				     collect
				     (let ((conjuncts (loop for var in v
							    for arg in (cdr prop)
							    collect `(:= ,var ,arg))))
				       (if (= (length conjuncts) 1)
					   (car conjuncts)
					   (list* :and conjuncts)
					   )))))
			    (if (= (length disjuncts) 1)
				(car disjuncts)
				(list* :or disjuncts))))))))))

;; One way of checking a model: Prover the theory given the model. Surprisingly prover9 succeeds on this for a simple
;; model where z3 and vampire take longer than I want to wait.
(defun prove-positive-ground-model (theory model &rest keys &key (with 'prover9-prove) 
					  &allow-other-keys)
  (remf :keys with)
  (apply with (append (mapcar (lambda(e) `(:fact ,e)) model)
		    (list (positive-ground-model-closure-axiom model)))
	    theory keys))

;; Write out a model as an intepretation that the LADR tools will use.
;; Lookes like clausetester and clausefilter are what I want for model checking.

;;; BIG PAIN IN THE ASS

;;; Turns out there's 's no way to tell clausefilter to use prolog style variables, which means that constants that
;;; start with lower case u..z are considered free variables. This hurts, since we've assumed prolog style variables
;;; (initial capital letters, which we don't use). So here we mangle all the non-variable names to have an "_" prefix,
;;; which keeps them out of trouble. This shouldn't be visible since we only return which axioms the interpretation
;;; succeeded or failed and those names are in the labels.
;;; For generating the formula for the model I added an option to prover9 logic generator to have names have a prefix.
;;; The generator naming doesn't distinguish between predicate and constant names, we we prefix all of them by
;;; passing :name-prefix "_" when instantiating the generator below.

(defun ladr-write-positive-ground-model (model theory &optional (stream t) &aux writing-to-string)
  (when (eq stream 'string)
    (setq stream (make-string-output-stream))
    (setq writing-to-string t))
  (let ((c2num (make-hash-table :test 'equalp))
	(num2c (make-hash-table :test 'eql))
	(pred2tuple (make-hash-table))
	(comma nil)
	(comma2 nil)
	(g (make-instance 'prover9-logic-generator)))
    (multiple-value-bind (predicates constants)  (formula-elements `(:and ,@model))
      (multiple-value-bind (predicates-m constants-m) (and theory 
							   (formula-elements 
							    `(:and ,@(mapcar 'axiom-sexp (remove-duplicates (collect-axioms-from-spec theory))))))
	(setq predicates (union predicates predicates-m :test 'equalp) constants (union constants constants-m)))
      (let ((domain-size (length constants)))
;	(write-string *ladr-header* stream)
	(format stream "~%interpretation( ~a, [number=1, seconds=0], [~%" domain-size)
	(loop for c in constants
	      for num from 0 
	      do (format stream "  function(~a, [ ~a ]),~%" (concatenate 'string "_" (normalize-names g c)) num)
		 (setf (gethash c c2num) num)
		 (setf (gethash num num2c) c))
	(loop for prop in model
	      when (eq (car prop) :fact) do (setq prop (second prop))
		do (push (reverse (mapcar (lambda(e) (gethash e c2num)) (cdr prop))) (gethash (car prop) pred2tuple)))
	(loop for (predicate arity) in predicates
	      with count-on-line = 0

	      do
		 (when comma2 (write-string "," stream))
		 (format stream "  function(~a(~a), [" (concatenate 'string "_" (normalize-names g predicate))
			 (cl-user::join-with-char (loop repeat arity collect "_") #\,))
		 (setq comma2 t)
		 (setq comma nil)
		 (iterate-n-arity arity domain-size
				  (lambda(&rest args)
				    (if (member args (gethash predicate pred2tuple) :test 'equalp)
					(format stream "~a~a" (if comma ", " "") 1)
					(format stream "~a~a" (if comma ", " "") 0))
				    (when (= (incf count-on-line) domain-size)
				      (format stream "~%  ")
				      (setq count-on-line 0))
				    (setq comma t)))
				    (format stream " ])~%"))
				  (format stream "~%]).~%")
				  (if writing-to-string
				      (get-output-stream-string stream))))))


(defun iterate-n-arity (n count fun &rest indices)
  (if (= n 0)
      (apply fun indices)
      (loop for i below count
	    do
	       (apply 'iterate-n-arity (1- n)  count fun i indices))))

(defvar *last-clausetester-input* nil)
(defvar *last-clausetester-output* nil)
(defun clausetester-check-model (model theory)
  (let ((interpretation-file (ext:make-temp-file :prefix "model" :prefix "interp")))
    (with-open-file (f interpretation-file :direction :output)
      (setq *last-clausetester-input* (ladr-write-positive-ground-model model theory 'string))
      (write-string *last-clausetester-input* f))
    (let ((results  (run-program-string->string
		    (prover-binary "clausetester")
		    (list (namestring (truename interpretation-file)))
		    (second (setq *last-clausetester-input* 
				  (list *last-clausetester-input*  
					(render-axioms (make-instance 'prover9-logic-generator :name-prefix "_")
						       (collect-axioms-from-spec theory)))))
		    )))
      (setq *last-clausetester-output* results)
      (let ((error? (search "%ready to abort" results )))
	(when error?
	  (let ((last-newline (or (position #\newline results :from-end t :end error?) 0)))
	    (error "Clausetester error: ~a" (subseq results (1+ last-newline))))))
      (with-input-from-string (s results)
	(let ((s2 (make-string-output-stream)))
	  (loop for line = (read-line s nil :eof)
		until (eq line :eof)
		with failures with successes
		do (sleep .1)
		   (format s2 "~a~%" line)
		   (let ((summary (car (all-matches line "% interp 1 models (\\d+) of (\\d+) clauses." 1 2))))
		     (if summary
			 (progn 
			   (get-output-stream-string s2)
			   (if (equal (first summary) (second summary))
			       (return-from clausetester-check-model :satisfying-model)))
			 (destructuring-bind (formula label worked?)
			     (let ((matches 
				     (car (all-matches line "(.*?)\\s*(#\\s*label\\(\"(.*?)\"\\)\\.){0,1}\\s*%\\s*(.*){0,1}" 1 3 4))))
			       (assert matches (line) "clausetester-ouput didn't match expectations: ~s"  line)
			       matches)
			   (declare (ignore formula))
			   (if (not (equal worked? "1"))
			       (push (intern (string-upcase label) 'keyword) failures)
			       (push (intern (string-upcase label) 'keyword) successes))
			   )))
		finally
		   (return (values :failed failures))
		))))))
  
    
;(prove-positive-ground-model `((:forall (?x) (:or (c ?x) (d ?x)))) '((c a) (c b) (d t)))

#|(defun in-what-predicates (var form) 
  (let ((them nil))
    (labels ((its-in (x form)
	       (found 
	       (pushnew (list (car form) (position x form)) them :test 'equalp))
	     (in-what (form)
	       (format nil "checking ~a~%")
	       (cond ((or (eq (car form) :forall) (eq (car form) :exists))
		      (unless (member var (second form))
			(in-what (third form))))
		     ((not (keywordp (car form)))
		      (when (member var form)
			(its-in var form)))
		     ((keywordp (car form))
		      (dolist (f (cdr form)) (in-what f))))))
      (in-what form))
    them))


(defun transform (form db)
  (cond ((or (eq (car form) :forall) (eq (car form) :exists))
	 (let ((joiner (if (eq (car form) :forall) 'always 'thereis)))
	   (if (second form)
	       (let* ((first (car (second form)))
		      (in-what (in-what-predicates first (third form)))
		      (tuples (remove-duplicates (apply 'concatenate 'list (mapcar (lambda(e) (in-pos e db)) in-what)))))
		 `(loop for ,first in ',tuples
			,joiner
			,(transform `(:forall ,(rest (second form)) ,(third form)) db)))
	       (transform (third form) db))))
	((keywordp (car form))
	 `(,(car form) ,@(mapcar (lambda(e) (transform e db)) (rest form))))
	((consp form)
	 `(lookup (cons ',(car form) ,@(rest form)) '*tuples*))
	(t form)))

(defun in-pos (what db)
  (remove-duplicates
   (loop for tup in db
	 when '(eq (car tup) (car what))
	   collect (nth (second what) tup))))

(defun lookup (tuple db)
  (member tuple db :test 'equalp))

(in-pos '(ppo 3) *tuples*)

 (eq (car form) :exists))
	 (unless (member var (second form))
	   (in-what (third form))))
	((not (keywordp (car form)))
	 (when (member var form)
	   (its-in var form)))
	((keywordp (car form))
	 (dolist (f (cdr form)) (in-what f))))

(in-what-predicates '?x
		    '(:implies (:and (cont ?x) (cont ?y) (time ?t))
		      (:implies (ov ?x ?y ?t)
			  (:exists (?z)
			    (:and (cont ?z)
				  (:forall (?w)
				    (:implies (cont ?w)
					(:iff (po ?w ?z ?t) (:and (po ?w ?x ?t) (po ?w ?y ?t) )))))))))

(pprint (transform '(:forall (?x)
	     (:implies (:and (cont ?x) (cont ?y) (time ?t))
		 (:implies (ov ?x ?y ?t)
		     (:exists (?z)
		       (:and (cont ?z)
			     (:forall (?w)
			       (:implies (cont ?w)
				   (:iff (po ?w ?z ?t) (:and (po ?w ?x ?t) (po ?w ?y ?t) )))))))))
	   *tuples*))

(transform `(:forall (?a) (:or (cont ?a) (time ?a)) *tuples*)

(defvar *tuples*
  '((cont a)
    (cont b)
    (time t)
    (ppo a b t)))
    


(forall () (imples .. 


Suppose my sorts were described for predicates.
Propagate up to quantifiers


continuant-part-of: particular particular time
ov: particular particular time

    (:and (:forall (?x ?y ?t) particular, particular, time
	    (:iff (ov ?x ?y ?t) particular particular time (consistent)
		(:exists (?w)particular
		  (:and (continuant-part-of ?w ?x ?t)
			(continuant-part-of ?w ?y ?t)))))
	  (:forall (?x ?y ?t)
	    (:implies (:= ?t t1)
		(:implies
		    (ov ?x ?y ?t)
		    (:exists (?z)
		      (:forall (?w)
			(:iff  (continuant-part-of ?w ?z ?t)
			    (:and (continuant-part-of ?w ?x ?t)
				  (continuant-part-of ?w ?y ?t)))))))))

(:and (:forall ((?x particular) (?y particular) (?t time))
	    (:iff (ov ?x ?y ?t)
		(:exists ((?w particular))
		  (:and (continuant-part-of ?w ?x ?t)
			(continuant-part-of ?w ?y ?t)))))
	  (:forall ((?x particular) (?y particular) (?t time))
	    (:implies (:= ?t t1)
		(:implies
		    (ov ?x ?y ?t)
		    (:exists (?z)
		      (:forall (?w)
			(:iff  (continuant-part-of ?w ?z ?t)
			    (:and (continuant-part-of ?w ?x ?t)
				  (continuant-part-of ?w ?y ?t)))))))))




(def-bfo-axiom continuant-part-of-domain-range
    (:and
     (:forall (?a ?b ?t)
       (:implies (continuant-part-of ?a ?b ?t)
	   (continuant-part-of-aux ?a ?b ?t)))
     (:forall (?a ?b ?t)
       (:implies (continuant-part-of-aux ?a ?b ?t)
	   (:and(continuant-part-of-aux ?a ?b ?t)
		(instance-of ?t temporal-region ?t)
		(instance-of ?a continuant ?t)
		(instance-of ?b continuant ?t)))))
  ""
                      :kind
                      :domain-range
                      :kind
  continuant-mereology)

(any axiom matching (continuant-mereology ?x temporal-region ?y) -> drop, x,y -> time 
 any axiom (instance-of ?b continuant ?t) -> continuant universal time
     (instance-of ?b occurrent ?t)
)
|#

