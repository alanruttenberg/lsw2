(in-package :logic)

;; functions transforming formulas into skolem- disjunctive-, and conjunctive-normal-form
;; https://en.wikipedia.org/wiki/Conjunctive_normal_form

(defun eliminate-implications (form)
  (cond ((atom form) form)
	((eq (car form) :implies)
	 `(:or (:not ,(eliminate-implications (second form))) ,(eliminate-implications (third form))))
	((eq (car form) :iff)
	 `(:and (:or (:not ,(eliminate-implications (second form))) ,(eliminate-implications (third form)))
		(:or (:not ,(eliminate-implications (third form))) ,(eliminate-implications (second form)))))
	(t (mapcar #'eliminate-implications form))))

(defun move-negation-inward-aux (form)
  (cond ((atom form) form)
	((eq (car form) :not)
	 (cond ((eq (car (second form)) :and)
		`(:or ,@(mapcar (lambda(e) `(:not ,(move-negation-inward-aux e))) (cdr (second form)))))
	       ((eq (car (second form)) :or)
		`(:and ,@(mapcar (lambda(e) `(:not ,(move-negation-inward-aux e))) (cdr (second form)))))
	       ((eq (car (second form)) :not)
		(move-negation-inward-aux (second (second form))))
	       ((eq (car (second form)) :forall)
		`(:exists ,(second (second form)) (:not ,@(mapcar #'move-negation-inward-aux (cddr (second form))))))
	       ((eq (car (second form)) :exists)
		`(:forall ,(second (second form)) (:not ,@(mapcar #'move-negation-inward-aux (cddr (second form))))))
	       (t form)))
	((member (car form) '(:and :or))
	 `(,(car form) ,@(mapcar #'move-negation-inward-aux (cdr form))))
	((member (car form) '(:forall :exists))
	 `(,(car form) ,(second form) ,@(mapcar #'move-negation-inward-aux (cddr form))))
	(t form)))

(defun move-negation-inward (form)
  (loop for last = nil then next
	for next = (move-negation-inward-aux form) then (move-negation-inward-aux next)
	until (equal last next)
	finally (return last)))

;; assumes implications already removed.
(defun move-quantifiers-outwards (form)
  (labels ((move (form)
	     (cond ((member (car form) '(:and :or))
		    (loop for el in (cdr form)
			  with stack
			  if (member (car el) '(:forall :exists))
			    collect (if (= (length (cddr el)) 1)
					(car (cddr el))
					`(,(car form) ,@(cddr el))) into forms
			    and do (if stack  (setq stack (substitute `(,(car el) ,(second el) @@@) '@@@ stack))
				       (setq stack `(,(car el) ,(second el) @@@)))
			  else collect el into forms
			  finally (progn 
				   (let ((inner forms))
				    (if (= (length forms) 1) 
					(setq inner (move (car forms)))
					(setq inner `(,(car form) ,@(mapcar #'move forms))))
				    (return-from move (if stack (subst inner '@@@ stack ) 
							  inner))))))
		   ((member (car form) '(:forall :exists))
		    `(,(car form) ,(second form) ,@(mapcar #'move  (cddr form))))
		   (t form))))
    (loop for last = nil then next
	  for next = (move form) then (move next)
	  until (equal last next)
	  finally (return last))))

;; skolem functions are :sk-1, :sk-2... (in keyword package, so won't conflict with existing functions)
(defun skolemize (form)
  (let ((bound (if (boundp 'bound) (symbol-value 'bound) nil)))
    (declare (special bound))
    (let ((count 0))
      (flet ((fresh-skolemf (args count)
	       `(,(intern (format nil "SK-~a" count) 'keyword) ,@args)))
	(labels ((doit (form)
		   (cond ((atom form) form)
			 ((eq (car form) :forall)
			  (let ((bound (append (second form) bound)))
			    (declare (special bound))
			    `(:forall ,(second form) ,@(mapcar #'doit (cddr form)))))
			 ((eq (car form) :exists)
			  (let ((rest (cddr form)))
			    (loop for v in (second form)
				  do (setq rest (subst (fresh-skolemf bound (incf count)) v rest)))
			    (if (= (length rest) 1) 
				(doit (car rest))
				(mapcar #'doit rest))))
			 (t form))))
	  (doit form))))))

(defun strip-outer-universals (form)
  (cond ((atom form) form)
	((eq (car form) :forall )
	 (let ((rest (cddr form)))
	   (if (= (length rest) 1)
	       (strip-outer-universals (car rest))
	       `(:and ,@(mapcar 'strip-outer-universals rest)))))
	(t form)))

(defun negation-normal-form (form)
  (move-negation-inward
   (eliminate-implications form)))

(defun skolem-normal-form (form)
  (strip-outer-universals
   (skolemize
    (move-quantifiers-outwards
     (rewrite-standardizing-apart
      (negation-normal-form form))))))

;; form is skolem normal
;; distribute is either :and or :or
;; output is a list of forms
;; e.g. (:and (:or (f ?a) (g ?a)) (h ?a)) distributed over :or is ((:and (f ?a) (h ?a)) (:and (g ?a) (h ?a)))
(defun distribute-and-or (distribute form)
  (flet ((distribute-1 (form)
	   (let ((to-distribute
		   (block find
		     (tree-walk form (lambda(e)
				       (when (and (consp e) (eq (car e) distribute))
					 (return-from find e)))))))
		     (if to-distribute
			 (mapcar (lambda(disjunct) (subst disjunct to-distribute form)) (cdr to-distribute))
			 (list form)))))
    (loop for last = nil then next
	  for next = (mapcan #'distribute-1 (list form)) then (mapcan #'distribute-1 (copy-list next))
	  until (equal last next)
	  finally (return last))))

;; A limited kind of flattening, in which a formula with only :and or :or, and :not, with the :not already have been
;; moved inward, i.e, there is no :and or :or in the scope of a :not.
;; The form is flattened to a list of conjuncts, some possibly with negation.
;; head is either :and or :or
;; e.g (:and (:and (f ?z))) -> (:and (f ?a))

(defun flatten-and-or (form head)
  (flet ((leavealone (form)
	   (or (atom form) (not (tree-find head form)))))
    (let ((conjuncts nil))
      (labels ((flatten-1 (form)
		 (if (and (consp form) (eq (car form) head))
		     (map nil #'flatten-1 (cdr form))
		     (cl-user::tree-walk-conditional form
				 (lambda(e)
				   (cond  ((leavealone form) (push form conjuncts) nil)
					  ((eq (car e) :not) (push form conjuncts) nil)
					  (t t)))
				 ))))
	(flatten-1 form)
	(reverse conjuncts)))))
	
(defun disjunctive-normal-form (form)
  (mapcar (lambda(e) (flatten-and-or e :and)) (distribute-and-or :or (skolem-normal-form form))))

(defun conjunctive-normal-form (form)
  (mapcar (lambda(e) (flatten-and-or e :or)) (distribute-and-or :and (skolem-normal-form form))))

