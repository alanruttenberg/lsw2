(in-package :logic)

;; If there's a long list of constants that are distinct, the usual distinct transform is n^2.
;; Instead we can use a functional relationship and peano numbers to make them distinct, which is order n instead.
;; This works natively in prover9, but I need to fix z3 and vampire rendering to properly handle functions.

;(map nil 'print (all-distinct-peano '(a b c d e) :successor-pred-prefix 's :function-relation-name 'number-of  :vars '(a b c d)))
;->
;; (:forall (?a ?b) (:iff (:= ?a ?b) (:= (s1 ?a) (s1 ?b)))) 
;; (:forall (?a ?b ?c ?d) (:implies (:and (number-of ?a ?c) (number-of ?b ?d) (:not (:= ?c ?d))) (:not (:= ?a ?b)))) 
;; (:forall (?a) (:= (s2 ?a) (s1 (s1 ?a)))) 
;; (:forall (?a) (:= (s4 ?a) (s2 (s2 ?a)))) 
;; (:fact (number-of a zero)) 
;; (:fact (number-of b (s1 zero))) 
;; (:fact (number-of c (s2 zero))) 
;; (:fact (number-of d (s1 (s2 zero)))) 
;; (:fact (number-of e (s4 zero))) nil


(defun collect-power-2-successor-functions (max &key (successor-pred-prefix 'f) (var '?x)) 
  (flet ((f (n)
	   (intern (concatenate 'string (string successor-pred-prefix) (prin1-to-string n)) (symbol-package successor-pred-prefix))))
    (loop for i from 1 below (1+ (floor (log max 2)))
	  for ii = i then (* 2 ii)
	  collect `(:forall (,var) (:= (,(f (* 2 ii)) ,var) (,(f ii) (,(f ii) ,var)))))))

(defun power-2-successor-function-name (n &key (successor-pred-prefix 'f))
  (intern (format nil "~a~a" (string-upcase (string successor-pred-prefix))  (expt 2 n)) (symbol-package successor-pred-prefix)))

(defun peano (num &key (successor-pred-prefix 'f))
  (loop with n = num while (> n 0)
	with p = 'zero 
	do
	   (sleep .1)
	   (loop for f from (floor (log num 2)) downto 0
		 do
		 (when (>= n (expt 2 f))
		   (setq n (- n (expt 2 f)))
		   (setq p (list (power-2-successor-function-name f :successor-pred-prefix successor-pred-prefix) p))))
	finally (return p)))

(defun all-distinct-peano (syms &key (successor-pred-prefix 'f) (vars '(?x1 ?x2 ?y1 ?y2)) (function-relation-name 'number-of))
  (destructuring-bind (v1 v2 v3 v4) (mapcar (lambda(v) (if (char= (char (string v) 0) #\?) v (intern (concatenate 'string "?" (string v)) (symbol-package v))))
					    vars)
    (flet ((f (n)
	     (intern (format nil "~a~a" (string-upcase (string successor-pred-prefix))  n) (symbol-package successor-pred-prefix))))
      `( ;;m=n iff f(m)=f(n)
	(:forall (,v1 ,v2) (:iff (:= ,v1 ,v2) (:= (,(f 1) ,v1) (,(f 1) ,v2))))
	;; f(x) != zero
	(:forall (?x) (:not (:= ?x (,(f 1) ?x))))
	;; (u a n) and (u b m) and (:not (:= m n) implies (:not (:= a b))
	;; where m and n are (peano) numbers
	(:forall (,v1 ,v2 ,v3 ,v4) (:implies (:and (,function-relation-name  ,v1 ,v3) 
						   (,function-relation-name  ,v2 ,v4)
						   (:not (:= ,v3 ,v4))) (:not (:= ,v1 ,v2))))
	;; power of 2 successor functions (f1, f2 ,f4 ..)
	,@(collect-power-2-successor-functions (length syms) :successor-pred-prefix successor-pred-prefix :var v1)
	;; The assignment of each sym to a different number
	,@(loop for i from 0
		for sym in syms
		collect `(:fact (,function-relation-name ,sym ,(peano i :successor-pred-prefix successor-pred-prefix))))))))


