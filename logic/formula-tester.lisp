(in-package :logic)

(defmacro with-lists (&body body)
  `(let ((l2 (make-list 2))
	 (l3 (make-list 3))
	 (l4 (make-list 4))
	 (l5 (make-list 5)))
     (declare (ignorable l2 l3 l4 l5))
     ,@body))
  
(defmacro list2 (arg1 arg2)
    `(progn (setf (first l2) ,arg1 (second l2) ,arg2) l2))

(defmacro list3 (arg1 arg2 arg3)
  `(progn (setf (first l3) ,arg1 (second l3) ,arg2 (third l3) ,arg3) l3))

(defmacro list4 (arg1 arg2 arg3 arg4)
  `(progn (setf (first l4) ,arg1 (second l4) ,arg2 (third l4) ,arg3 (fourth l4) ,arg4) l4))

(defmacro getenv ( &environment env)
  env)

(defvar *debug-eval-formula* nil)

(defun evaluate-formula (formula interpretation)
  "Take a logical formula and test against an interpretation given as the list of positive propositions. 
Works by transforming the formula into a function, compiling it, and running it. The list macros are used 
to avoid consing, which can land up be quite a lot"
  (let ((form 
	  (tree-replace (lambda(form)
			  (if (and (consp form)
				   (not (keywordp (car form)))
				   (not (logic-var-p (car form))))
			      `(funcall check 
					,(let* ((form (if (eq (car form) :fact) (second form) form))
						(length (length form)))
					   (cond ((= length 2) `(list2 ,@(mapcar (lambda(e) (if (logic-var-p e) e `(quote ,e))) form )))
						 ((= length 3) `(list3 ,@(mapcar (lambda(e) (if (logic-var-p e) e `(quote ,e))) form )))
						 ((= length 4) `(list4 ,@(mapcar (lambda(e) (if (logic-var-p e) e `(quote ,e))) form )))
						 ((= length 5) `(list5 ,@(mapcar (lambda(e) (if (logic-var-p e) e `(quote ,e))) form )))
						 (t (error "Didn't think there'd be arity > 4")))))
			      form))
			formula)))
    (let ((macros '((:forall ((&rest vars) body)
		      (if (null (cdr vars))
			  (let ((label (gensym)))
			    `(block ,label
			       (let ((success t))
				 (dolist (,@vars universe)
				   (unless ,body
				     (progn (when *debug-eval-formula*
					      (when (member (car ',(third body)) '(list2 list3 list4 list5))
						(let ((form ',(cdr (third body))))
						  (setq form (list* (second (car form)) (cdr form)))
						  (format t ":forall ~a missed ~a=~a~%"  form ',(first vars) ,(first vars)   ))))
					    (return-from ,label (setq success nil)))))
				 success)))
			  `(:forall (,(car vars))
			     (:forall (,@(cdr vars))
			       ,body))))
		    (:exists ((&rest vars) body)
		      (if (null (cdr vars))
			  (let ((label (gensym)))
			    `(block ,label
			       (dolist (,@vars universe nil)
				 (if ,body
				     (return-from ,label t)))
			       (when *debug-eval-formula*
				 (when (member (car ',(third body)) '(list2 list3 list4 list5))
				   (let ((form ',(cdr (third body))))
				     (setq form (list* (second (car form)) (cdr form)))
				     (format t ":exists ~a no success~%"  form    ))))))
			  `(:exists (,(car vars))
			     (:exists (,@(cdr vars))
			       ,body))))
		    (:and (&rest forms)
		     `(and ,@forms))
		    (:or (&rest forms)
		     `(or ,@forms))
		    (:implies (ant cons)
			`(not (and ,ant (not ,cons))))
		    (:iff (ant cons)
			`(eq ,ant ,cons))
		    (:fact (prop)
		     prop)
		    (:distinct (&rest syms)
		     t)
		    (:= (a b)
		     `(eq ,(if (logic-var-p a) a `(quote ,a))
		       ,(if (logic-var-p b) b `(quote ,b))))
		    (:not (prop)
		      `(not ,prop)))))
      (let ((fun `(lambda (universe check)
		    (declare (optimize (speed 3) (safety 0)))
		    (declare (ignorable universe check))
		    (with-lists (macrolet ,macros ,form))))
	    (kb-lookup (make-hash-table :test 'equalp))
	    (universe (remove-duplicates (apply 'append (mapcar 'cdr interpretation)))))
	(dolist (prop interpretation) (setf (gethash prop kb-lookup) t))
	(funcall (compile nil fun)  universe
		 (lambda (prop) (declare (optimize (speed 3) (safety 0))) (gethash prop kb-lookup)))))))

(defun rewrite-inverses (spec &key copy-names? binary-inverses ternary-inverses)
  "Rewrites the formulas in spec to rewrite inverse relationships to use only 1. Inverses are specified as pairs in arguments to :binary-inverses :ternary-inverses. With copy-names? t returns a list of axioms with the same names as the originals. Without just returns the sexps"
  (let ((axioms (collect-axioms-from-spec spec)))
    (loop for ax in axioms
	  for rewritten-sexp in
	      (mapcar (lambda(e) (if (keywordp (car e)) e `(:fact ,e)))
		      (render-axioms (make-instance 'logic-generator 
						    :binary-inverses binary-inverses
						    :ternary-inverses ternary-inverses)
				     (collect-axioms-from-spec spec)))
	  collect
	  (if copy-names?
	      (make-instance 'axiom :sexp rewritten-sexp :name (axiom-name ax))
	      rewritten-sexp))))

(defun evaluate-formulas (spec model &key binary-inverses ternary-inverses)
  "Take spec and interpreation as list of positive propositions, with optional pairs of inverse relations to rewrite, and 
check each of them, as would ladr's clausetester. Return either :satisfying-model or :failed. In the latter case the 
second value is the list of formulas that failed"
  (let ((rewritten (rewrite-inverses spec
				     :binary-inverses binary-inverses
				     :ternary-inverses ternary-inverses
				     :copy-names? t)))
    (let ((results 
	    (lparallel:pmapcan 
	     (lambda(e) (if (evaluate-formula
			     (car (rewrite-inverses (list (axiom-sexp e))
						    :binary-inverses binary-inverses
						    :ternary-inverses ternary-inverses))
			     model)
			    nil
			    (list (axiom-name e))))
	     (coerce rewritten 'vector))))
      (if results
	  (values :failed results)
	  :satisfying-model))))



