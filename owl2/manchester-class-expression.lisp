;; start of manchester syntax reader

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
