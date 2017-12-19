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
(defun check-positive-ground-model (theory model &rest keys &key (with 'prover9-prove) 
					  &allow-other-keys)
  (remf :keys with)
  (apply with (append (mapcar (lambda(e) `(:fact ,e)) model)
		    (list (positive-ground-model-closure-axiom model)))
	    theory keys))

;; Write out a model as an intepretation that the LADR tools will use.
;; Lookes like clausetester and clausefilter are what I want for model checking.

(defun ladr-write-positive-ground-model (model &optional (stream t))
  (when (eq stream 'string)
    (setq stream (make-string-output-stream)))
  (let ((c2num (make-hash-table))
	(num2c (make-hash-table :test 'eql))
	(pred2tuple (make-hash-table))
	(comma nil)
	(comma2 nil))
  (multiple-value-bind (predicates constants) (formula-elements `(:and ,@model))
    (format stream "interpretation( ~a, [number=1, seconds=0], [~%" (length constants))
    (loop for c in constants
	  for num from 0 
	  do (format stream "  function(~a, [ ~a ]),~%" c num)
	  (setf (gethash c c2num) num)
	  (setf (gethash num num2c) c))
    (loop for prop in model
	  do (push (mapcar (lambda(e) (gethash e c2num)) (cdr prop)) (gethash (car prop) pred2tuple)))
    (loop for (predicate arity) in predicates
	  do
	     (when comma2 (write-string "," stream))
	     (format stream "  function(~a(~a), [" predicate
		     (cl-user::join-with-char (loop repeat arity collect "_") #\,))
	     (setq comma2 t)
	     (setq comma nil)
	     (iterate-n-arity arity (length constants)
			      (lambda(&rest args)
				(if (member args (gethash predicate pred2tuple) :test 'equalp)
				    (format stream "~a~a" (if comma ", " "") 1)
				    (format stream "~a~a" (if comma ", " "") 0))
				(setq comma t)))
	     ;; blah
	     (format stream " ])~%"))
    (format stream "~%]).~%")
    (if (typep stream 'sys::string-output-stream)
	(get-output-stream-string stream)))))

(defun iterate-n-arity (n count fun &rest indices)
  (if (= n 0)
      (apply fun indices)
      (loop for i below count
	    do
	       (apply 'iterate-n-arity (1- n)  count fun i indices))))
    
;(check-positive-ground-model `((:forall (?x) (:or (c ?x) (d ?x)))) '((c a) (c b) (d t)))

