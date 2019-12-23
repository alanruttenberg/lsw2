(in-package :logic)

(defun smatch (a b body)
  (let ((menv (unify:unify* a b)))
    (unless (null menv) (unify:apply-substitution menv  body))))
    
(defun trr (expression)
  (cond ((symbolp expression)
         (lambda(a b) `(,expression ,a ,b)))
        (t (trf expression))))

(defun trc (expression)
  (cond ((symbolp expression)
	 (cond ((eq expression 'owl-thing)
		(lambda(c) (:= c c)))
	       ((eq expression 'owl-nothing)
		(lambda(c) (:not (:= c c))))
	       (t 
		(lambda(c) `(,expression ,c)))))
        (t (trf expression))))

(defun trf (expression)
  (maphash (lambda(k v)
             (let ((res (funcall 'smatch k expression v)))
               (when res (return-from trf res))))
           *trs*))

;; funcall translate class 
(defmacro ftrc (c v)
  `(funcall (trc ,c) ,v))

;; funcall translate relation
(defmacro ftrr (r a b)
  `(funcall (trr ,r) ,a ,b))

;; funcall tr on a non-symbol form - This is the one you'll want to use
(defmacro tr (expression &rest args)
  `(apply (trf ,expression) ',args))

;; ****************************************************************
;; Macro to define translations

(defparameter *trs* (make-hash-table :test 'equalp))
(defparameter *tr-tests* (make-hash-table :test 'equalp))

(defmacro def-tr (form translation &key tests)
  `(progn
     (setf (gethash ',form *trs*) ',translation)
     (setf (gethash ',form *tr-tests*) ',tests)
     ))
                 
;; ****************************************************************
;; Class expression Translations

(def-tr (object-union-of . ?classes)
    (lambda(c) 
      `(:or ,@(mapcar (lambda(e) (ftrc e c)) '?classes)))
  :tests
  (((tr '(object-union-of a b c) ?x)
    (:or (a ?x) (b ?x) (c ?x)))
   ((tr '(object-union-of (object-intersection-of a b) c) ?x)
    (:or (:and (a ?x) (b ?x)) (c ?x)))))

(def-tr (object-intersection-of . ?classes)
    (lambda(c) 
      `(:and ,@(mapcar (lambda(e) (ftrc e c)) '?classes)))
  :tests
  (((tr '(object-intersection-of a b) ?x) (:and (a ?x) (b ?x)))
   ((tr '(object-intersection-of a b c) x)
    (:and (a x) (b x) (c x)))))

(def-tr (object-some-values-from ?r ?c)
    (lambda(c)
      (with-logic-var x
        `(:exists (,x) (:and ,(ftrc '?c x) (?r ,c ,x)))))
  :tests
  (((tr '(object-some-values-from r c) ?x)
    (:exists (?p) (:and (c ?p) (r ?x ?p))))))

(def-tr (object-all-values-from ?r ?c) 
    (lambda(c)
      (with-logic-var x
        `(:forall (,x) (:implies (?r ,c ,x) ,(ftrc '?c x)))))
  :tests
  (((tr '(object-all-values-from r c) ?x)
    (:forall (?p) (:implies (r ?x ?p) (c ?p))))))

(def-tr (subclass-of ?c1 ?c2)
    (lambda()
      (with-logic-var v `(:forall (,v) (:implies ,(ftrc '?c1 v) ,(ftrc '?c2 v)))))
  :tests
  (((tr '(subclass-of a b)) (:forall (?p) (:implies (a ?p) (b ?p))))))

(def-tr (object-has-self ?r)
    (lambda(c)
      `(?r ,c ,c))
  :tests
  (((tr '(subclass-of c (object-has-self r)))
    (:forall (?p) (:implies (c ?p) (r ?p ?p))))))

(def-tr (object-one-of . ?is)
    (lambda(c)
      `(:or ,@(mapcar (lambda(i) `(:= ,c ,i)) '?is)))
  :tests
    (((tr '(subclass-of c (object-one-of x y z)))
     (:forall (?p) (:implies (c ?p) (:or (:= ?p x) (:= ?p y) (:= ?p z))))))
  )

(def-tr (object-has-value  ?v) 
    (lambda(c)
      `(:= ,c ,'?v))
  :tests
  (((tr '(subclass-of c (object-has-value i)))
    (:forall (?p) (:implies (c ?p) (:= ?p i))))))

(def-tr (equivalent-classes ?c1 ?c2 ?c3 . ?rest)
    (lambda ()
      `(:and ,(tr '(equivalent-classes ?c1 ?c2))
             ,(tr '(equivalent-classes ?c2 ?c3))
             ,@(if '?rest
                    (list (tr `(equivalent-classes ?c3 ,@'?rest))))))
  :tests
  (((tr '(equivalent-classes a b c d))
    (:and (:forall (?p) (:iff (a ?p) (b ?p)))
          (:forall (?p) (:iff (b ?p) (c ?p)))
          (:forall (?p) (:iff (c ?p) (d ?p)))))))

(def-tr (equivalent-classes ?c1 ?c2)
    (lambda()
      (with-logic-var v `(:forall (,v) (:iff ,(ftrc '?c1 v) ,(ftrc '?c2 v)))))
  :tests
  (((tr '(equivalent-classes a (object-all-values-from r c)))
    (:forall (?p)
      (:iff (a ?p) (:forall (?q) (:implies (r ?p ?q) (c ?q))))))
   ))

#|

(def-tr (object-min-cardinality ?r ?i)
  )

(def-tr (object-max-cardinality ?r ?i)
  )

(def-tr (object-exact-cardinality ?r ?i)
  )

|#



(def-tr (disjoint-classes ?c1 ?c2)
    (lambda ()
      (with-logic-var x
        `(:forall (,x) (:and (:not (:and ,(ftrc '?c1 x)))
                             (:not (:and ,(ftrc '?c2 x)))))))
  :tests
  (((tr '(disjoint-classes a b))
    (:forall (?p)
      (:and (:not (:and (a ?p)))
            (:not (:and (b ?p)))))
    )))

(def-tr (disjoint-classes ?c1 ?c2 ?c3 . ?rest)
    (lambda ()
      `(:and ,(tr '(disjoint-classes ?c1 ?c2))
             ,(tr '(disjoint-classes ?c2 ?c3))
             ,(tr '(disjoint-classes ?c1 ?c3))
             ,@(when '?rest
                 (list (tr `(disjoint-classes ?c1 ,@'?rest))
                       (tr `(disjoint-classes ?c2 ,@'?rest))
                       (tr `(disjoint-classes ?c3 ,@'?rest))))))
  :tests
  (((tr '(disjoint-classes a b c))
    (:and (:forall (?p) (:and (:not (:and (a ?p))) (:not (:and (b ?p)))))
          (:forall (?p) (:and (:not (:and (b ?p))) (:not (:and (c ?p)))))
          (:forall (?p) (:and (:not (:and (a ?p))) (:not (:and (c ?p)))))))))


(def-tr (functional-object-property ?r)
  (lambda()
    (with-logic-var (x y1 y2)
      `(:forall (,x ,y1 ,y2) (:implies (:and ,(ftrr '?r x y1)
                                             ,(ftrr '?r x y2))
                                 (:= ,y1 ,y2)))))
  :tests
  (((tr '(functional-object-property p1))
    (:forall (?p ?q ?r)
      (:implies (:and (p1 ?p ?q)
                      (p1 ?p ?r))
          (:= ?q ?r))))))

(def-tr (inverse-functional-object-property ?r)
  (lambda()
    (with-logic-var (x1 x2 y)
      `(:forall (,x1 ,x2 ,y) (:implies (:and ,(ftrr '?r x1 y)
                                             ,(ftrr '?r x2 y))
                                 (:= ,x1 ,x2)))))
  :tests
  (((tr '(inverse-functional-object-property  p1))
    (:forall (?p ?q ?r)
      (:implies (:and (p1 ?p ?r) (p1 ?q ?r)) (:= ?p ?q))))))

(def-tr (reflexive-object-property ?r)
    (lambda () (with-logic-var x
                 `(:forall (,x) ,(ftrr '?r x x))))
  :tests
  (((tr '(reflexive-object-property p1))
    (:forall (?p) (p1 ?p ?p)))))

(def-tr (irreflexive-object-property ?r)
    (lambda () (with-logic-var x
                 `(:forall (,x) (:not ,(ftrr '?r x x)))))
  :tests
  (((tr '(irreflexive-object-property p1))
    (:forall (?p) (:not (p1 ?p ?p))))))

(def-tr (symmetric-object-property ?r)
    (lambda () (with-logic-var (x y)
                 `(:forall (,x) (:implies ,(ftrr '?r x y) ,(ftrr '?r y x)))))
  :tests
  (((tr '(symmetric-object-property p1))
    (:forall (?p)
      (:implies (p1 ?p ?q) (p1 ?q ?p))))))

(def-tr (asymmetric-object-property ?r)
    (lambda () (with-logic-var (x y)
                 `(:forall (,x) (:implies ,(ftrr '?r x y) (:not ,(ftrr '?r y x))))))
  :tests
  (((tr '(asymmetric-object-property p1))
    (:forall (?p)
      (:implies (p1 ?p ?q) (:not (p1 ?q ?p)))))))

(def-tr (transitive-object-property ?r)
    (lambda ()
      (tr '(sub-object-property-of (object-property-chain ?r ?r) ?r)))
  :tests 
  (((tr '(transitive-object-property po))
    (:forall (?p ?q)
      (:implies (:exists (?r) 
                  (:and (po ?p ?r) (po ?r ?q)))
          (po ?p ?q))))))

;; ****************************************************************
;; property-related axioms

(def-tr (object-property-domain ?r ?c)
  (lambda()
    (with-logic-var (x y)
      `(:forall (,x ,y) (:implies ,(ftrr '?r x y) ,(ftrc '?c x)))))
  :tests
  (((tr '(object-property-domain p1 c1))
    (:forall (?p ?q)
      (:implies (p1 ?p ?q) (c1 ?p))))))

(def-tr (object-property-range ?r ?c)
  (lambda()
    (with-logic-var (x y)
      `(:forall (,x ,y) (:implies ,(ftrr '?r x y) ,(ftrc '?c y)))))
  :tests
  (((tr '(object-property-range p1 c1))
    (:forall (?p ?q)
      (:implies (p1 ?p ?q) (c1 ?q))))))

(def-tr (functional-object-property ?r)
  (lambda()
    (with-logic-var (x y1 y2)
      `(:forall (,x ,y1 ,y2) (:implies (:and ,(ftrr '?r x y1)
                                             ,(ftrr '?r x y2))
                                 (:= ,y1 ,y2)))))
  :tests
  (((tr '(functional-object-property p1))
    (:forall (?p ?q ?r)
      (:implies (:and (p1 ?p ?q)
                      (p1 ?p ?r))
          (:= ?q ?r))))))

(def-tr (inverse-functional-object-property ?r)
  (lambda()
    (with-logic-var (x1 x2 y)
      `(:forall (,x1 ,x2 ,y) (:implies (:and ,(ftrr '?r x1 y)
                                             ,(ftrr '?r x2 y))
                                 (:= ,x1 ,x2)))))
  :tests
  (((tr '(inverse-functional-object-property  p1))
    (:forall (?p ?q ?r)
      (:implies (:and (p1 ?p ?r) (p1 ?q ?r)) (:= ?p ?q))))))

(def-tr (reflexive-object-property ?r)
    (lambda () (with-logic-var x
                 `(:forall (,x) ,(ftrr '?r x x))))
  :tests
  (((tr '(reflexive-object-property p1))
    (:forall (?p) (p1 ?p ?p)))))

(def-tr (irreflexive-object-property ?r)
    (lambda () (with-logic-var x
                 `(:forall (,x) (:not ,(ftrr '?r x x)))))
  :tests
  (((tr '(irreflexive-object-property p1))
    (:forall (?p) (:not (p1 ?p ?p))))))

(def-tr (symmetric-object-property ?r)
    (lambda () (with-logic-var (x y)
                 `(:forall (,x) (:implies ,(ftrr '?r x y) ,(ftrr '?r y x)))))
  :tests
  (((tr '(symmetric-object-property p1))
    (:forall (?p)
      (:implies (p1 ?p ?q) (p1 ?q ?p))))))

(def-tr (asymmetric-object-property ?r)
    (lambda () (with-logic-var (x y)
                 `(:forall (,x) (:implies ,(ftrr '?r x y) (:not ,(ftrr '?r y x))))))
  :tests
  (((tr '(asymmetric-object-property p1))
    (:forall (?p)
      (:implies (p1 ?p ?q) (:not (p1 ?q ?p)))))))

(def-tr (transitive-object-property ?r)
    (lambda ()
      (tr '(sub-object-property-of (object-property-chain ?r ?r) ?r)))
  :tests 
  (((tr '(transitive-object-property po))
    (:forall (?p ?q)
      (:implies (:exists (?r) 
                  (:and (po ?p ?r) (po ?r ?q)))
          (po ?p ?q))))))

;; ****************************************************************
;; Object property expressions

(def-tr (object-property-chain ?r1 ?r2)
    (lambda (a b)
      (with-logic-var middle
        `(:exists (,middle)
           (:and ,(ftrr '?r1 a middle) ,(ftrr '?r2 middle b)))))
  :tests (((tr '(object-property-chain a b) ?x ?y)  (:exists (?p) (:and (a ?x ?p) (b ?p ?y))))))
  

(def-tr (object-property-chain ?r1 ?r2 ?r3 . ?rest)
    (lambda (a b)
      (with-logic-var middle
        `(:exists (,middle)
           (:and ,(ftrr '?r1 a middle)
                 ,(ftrr `(object-property-chain ?r2 ?r3 ,@'?rest)  middle b)))))
  :tests (((tr '(object-property-chain a b c) ?x ?y)
           (:exists (?p)
             (:and (a ?x ?p) (:exists (?q) (:and (b ?p ?q) (c ?q ?y))))))
          ((tr '(object-property-chain a b c d) ?x ?y)
           (:exists (?p)
             (:and (a ?x ?p)
                   (:exists (?q)
                     (:and (b ?p ?q) (:exists (?r) (:and (c ?q ?r) (d ?r ?y))))))))))

(def-tr (sub-object-property-of ?sub ?super)
  (lambda ()
    (with-logic-var (x y)
        `(:forall (,x ,y) (:implies ,(ftrr '?sub x y) ,(ftrr '?super x y)))))
  :tests
  (((tr '(sub-object-property-of m n))
    (:forall (?p ?q) (:implies (m ?p ?q) (n ?p ?q))))
   ((tr '(sub-object-property-of (object-property-chain m n) o))
    (:forall (?p ?q)
      (:implies (:exists (?r) (:and (m ?p ?r) (n ?r ?q))) (o ?p ?q))))
   ))

(def-tr (equivalent-object-properties ?sub ?super)
    (lambda ()
      (with-logic-var (x y)
        `(:forall (,x ,y) (:iff ,(ftrr '?sub x y) ,(ftrr '?super x y)))))
  :tests
  (((tr '(equivalent-object-properties m n))
    (:forall (?p ?q) (:iff (m ?p ?q) (n ?p ?q))))
   ))

(def-tr (disjoint-object-properties ?p1 ?p2)
    (lambda ()
      (with-logic-var (x y)
        `(:forall (,x ,y) (:and (:not (:and ,(ftrr '?p1 x y)))
                                (:not (:and ,(ftrr '?p2 x y)))))))
  :tests
  (((tr '(disjoint-object-properties m n))
    (:forall (?p ?q)
      (:and
       (:not (:and (m ?p ?q)))
       (:not (:and (n ?p ?q)))))
    )))

(def-tr (disjoint-object-properties ?p1 ?p2 ?p3 . ?rest)
    (lambda ()
      `(:and ,(tr '(disjoint-object-properties ?p1 ?p2))
             ,(tr '(disjoint-object-properties ?p2 ?p3))
             ,(tr '(disjoint-object-properties ?p1 ?p3))
             ,@(when '?rest
                   (list (tr `(disjoint-object-properties ?p1 ,@'?rest))
                         (tr `(disjoint-object-properties ?p2 ,@'?rest))
                         (tr `(disjoint-object-properties ?p3 ,@'?rest))))))
  :tests
  (((tr '(disjoint-object-properties p1 p2 p3 p4))
    (:and
     (:forall (?p ?q)
       (:and (:not (:and (p1 ?p ?q))) (:not (:and (p2 ?p ?q)))))
     (:forall (?p ?q)
       (:and (:not (:and (p2 ?p ?q))) (:not (:and (p3 ?p ?q)))))
     (:forall (?p ?q)
       (:and (:not (:and (p1 ?p ?q))) (:not (:and (p3 ?p ?q)))))
     (:forall (?p ?q)
       (:and (:not (:and (p1 ?p ?q))) (:not (:and (p4 ?p ?q)))))
     (:forall (?p ?q)
       (:and (:not (:and (p2 ?p ?q))) (:not (:and (p4 ?p ?q)))))
     (:forall (?p ?q)
       (:and (:not (:and (p3 ?p ?q))) (:not (:and (p4 ?p ?q)))))))))

(def-tr (disjoint-union ?class . ?classes)
    (lambda()
      `(:and ,(tr `(disjoint-classes ,@'?classes))
             ,(tr `(equivalent-classes ?class (object-union-of ,@'?classes)))))
  :tests
  (((tr '(disjoint-union a c1 c2 c3))
    (:and (:and (:forall (?p) (:and (:not (:and (c1 ?p))) (:not (:and (c2 ?p)))))
                (:forall (?p) (:and (:not (:and (c2 ?p))) (:not (:and (c3 ?p)))))
                (:forall (?p) (:and (:not (:and (c1 ?p))) (:not (:and (c3 ?p))))))
          (:forall (?p) (:iff (a ?p) (:or (c1 ?p) (c2 ?p) (c3 ?p))))))))

(def-tr (disjoint-object-properties ?p1 ?p2)
    (lambda()
      (with-logic-var (x y)
        `(:forall (,x ,y) (:and (:not (:and ,(ftrr '?p1 x y)))
                                (:not (:and ,(ftrr '?p2 x y)))))))
  :tests
  (((tr '(disjoint-object-properties m n))
    (:forall (?p ?q)
      (:and
       (:not (:and (m ?p ?q)))
       (:not (:and (n ?p ?q)))))
    )))

(def-tr (object-inverse-of ?p)
  (lambda (x y)
    (let ((trans (ftrr '?p y x)))
      (if (keywordp trans)
          trans
          `(:fact ,trans))))
  :tests 
  (((tr '(equivalent-object-properties r-inv (object-inverse-of r)))
    (:forall (?p ?q) (:iff (r-inv ?p ?q) (:fact (r ?q ?p)))))))

(def-tr (inverse-object-properties ?p1 ?p2)
    (lambda()
      (with-logic-var (x y)
        `(:forall (,x ,y) (:iff (?p1 ,x ,y) (?p2 ,y ,x)))))
  :tests 
  (((tr '(inverse-object-properties r1 r2))
    (:forall (?p ?q)
      (:iff (r1 ?p ?q) (r2 ?q ?p))))))

;; ****************************************************************
;; Individual translations

(def-tr (class-assertion ?c)
    (lambda(i)
      (let ((trans (ftrc '?c i)))
        (if (keywordp (car trans))
            trans
            `(:fact ,trans))))
  :tests 
  (((tr '(class-assertion k ) u)
    (:fact (k u)))
   ((tr '(class-assertion (object-some-values-from p k)) u)
    (:exists (?p)
      (:and (k ?p) (p u ?p))))))

(def-tr (same-individuals ?i1 ?i2)
    (lambda()
      '(:= ?i1 ?i2))
  :tests 
  (((tr '(same-individuals c v))
    (:= c v))))

(def-tr (different-individuals ?i1 ?i2)
    (lambda()
      '(:not (:= ?i1 ?i2)))
  :tests 
  (((tr '(different-individuals c v))
    (:not (:= c v)))))

(def-tr (object-property-assertion ?p ?a ?b)
  (lambda()
    (let ((trans (ftrr '?p '?a '?b)))
      (if (keywordp (car trans))
          trans
          `(:fact ,trans))))
  :tests 
  (((tr '(object-property-assertion p1 me you))
   (:fact (p1 me you)))))


;; ****************************************************************
;; Tests

(defun test-owl-to-fol-lamda ()
  (let ((tests
          '(((smatch '(foo ?x ?y) '(foo a b) '(?x ?y)) (a b))
            ((smatch '(foo ?x) '(foo a b) '(?x ?y)) nil)
            ((smatch '(foo ?x . ?rest) '(foo a b c d) '(?x . ?rest)) (a b c d))
            )))
    (maphash (lambda (form local-tests)
               (declare (ignore form))
;               (print-db form local-tests)
               (when local-tests
                 (setq tests (append tests local-tests))))
             *tr-tests*)
;    (inspect tests)
    (prove:plan (length tests))
;    (setq tests (remove 'quote tests))
    (loop for (in out) in tests
          do ;(print-db in out)
             (eval `(prove:is ,in ',out (let ((*print-pretty* t)) (format nil "~a" ',in)))))
    (prove:finalize)))
        
(defun owl-to-fol-report ()
  (maphash (lambda (pattern function)
             (let ((*print-pretty* t))
               (format t "~s~%~s~%~%" pattern function)))
           *trs*))
  
