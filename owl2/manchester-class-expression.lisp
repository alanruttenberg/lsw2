(in-package :cl-user)
;; start of manchester syntax reader

#|
This file provides a way to write manchesterish syntax for class expressions via sexps.
The forms are:

(and &rest conjuncts) ->   (object-intersection-of conjuncts)
(or &rest disjunctions) -> (object-union-of conjuncts)
(some prop class) ->       (object-some-values-from prop class)
(all prop class) ->        (object-all-values-from prop class)
(min n class) ->           (object-min-cardinality n class)
(max n class) ->           (object-max-cardinality n class)
(exactly n class) ->       (object-exact-cardinality n class)
(not class) ->             (object-complement-of class)
(value class instance) ->  (object-has-value class instance)

In all cases a keyword can be used as the first element instead of the symbol 

Within a with-ontology form, you can use this syntax as follows. 

(subclass-of !a (ce (or !b (min 1 !c))))

|#


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun manchester-expression (form)
    (cond ((atom form) form)
	  ((and (consp form) (symbolp (car form)))
	   (case (car form)
	     ((and :and)
	      `(object-intersection-of ,@(mapcar 'manchester-expression (cdr form))))
	     ((or :or)
	      `(object-union-of ,@(mapcar 'manchester-expression (cdr form))))
	     ((some :some)
	      `(object-some-values-from ,(second form) ,(manchester-expression (third form))))
	     ((all only :all :only)
	      `(object-all-values-from ,(second form) ,(manchester-expression (third form))))
	     ((has that :has :that)
	      `(object-has-value ,(second form) ,(manchester-expression (third form))))
	     ((:min min)
	      `(object-min-cardinality ,(second form)  ,(manchester-expression (third form))))
	     ((max :max)
	      `(object-max-cardinality ,(second form)  ,(manchester-expression (third form))))
	     ((exactly :exactly)
	      `(object-exact-cardinality ,(second form)  ,(manchester-expression (third form))))
	     ((not :not)
	      `(object-complement-of ,(manchester-expression (second form))))
	     ((value :value)
	      `(object-has-value ,(manchester-expression (second form)) ,(manchester-expression (third form))))
	     (otherwise form)))
	  ((and (consp form) (member (second form)
				     '(and :and or :or some :some all :all min :min max :max exactly :exactly only :only value :value that :that  not not)))
					;	   (print 'second)
;	   (print-db form)
	   (let ((reordered 
		  (if (and (member (second form) '(exactly)) (= (length form) 4))
		      (list (second form) (third form) (first form) (nth 3 form))
		      (list* (second form) (first form) (cddr form)))))
;	     (print (eval-uri-reader-macro reordered))
	     (manchester-expression reordered)))
	  ))

  (defmacro ce (form)
    `(eval-uri-reader-macro ',(manchester-expression form))))

(assert (equal 
	 (ce (and (some !part_of !head) (some !develops_from !Ectoderm)))
	 `(object-intersection-of
	   (object-some-values-from ,!ex:part_of ,!ex:head)
	    (object-some-values-from ,!ex:develops_from ,!ex:Ectoderm)))
	(equal 
	 (ce (and (!part_of some !head) (!develops_from some !Ectoderm)))
	 `(object-intersection-of
	   (object-some-values-from ,!ex:part_of ,!ex:head)
	    (object-some-values-from ,!ex:develops_from ,!ex:Ectoderm)))
	()
	"Manch macro not working!")

;; make it work as part of lispy owl syntax
(defrdfm manchester-class-expression
    (:pattern (ce ?class-expression) :case :subscript-free)
  (t (eval-uri-reader-macro (manchester-expression ?class-expression))))

(defun manchester-expression-from-sexp (exp)
  (labels ((one (form)
	   (cond ((atom form) form)
		 ((and (consp form) (symbolp (car form)))
		  (case (car form)
		    (object-intersection-of
		     `(:and ,@(mapcar #'one (cdr form))))
		    (object-union-of
		     `(:or ,@(mapcar #'one (cdr form))))
		    (object-some-values-from
		     `(:some ,(second form) ,(one (third form))))
		    (object-all-values-from
		     `(:all ,(second form) ,(one (third form))))
		    (object-has-value
		     `(:has ,(second form) ,(one (third form))))
		    (object-min-cardinality
		     `(:min ,(second form)  ,(one (third form))))
		    (object-max-cardinality
		     `(:max ,(second form)  ,(one (third form))))
		    (object-exact-cardinality
		     `(:exactly ,(second form)  ,(one (third form))))
		    (object-complement-of
		     `(not ,(one (second form))))
		    (otherwise (mapcar #'one form))))
		 (t form))))
    (mapcar #'one exp)))
