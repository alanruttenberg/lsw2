;;; This file was stolen from Matt Ginsberg's MVL 3/8/93 via https://github.com/cgay/lisp-to-dylan/blob/master/test/cnf.lisp
;;; modified for LSW

(defpackage :ginsberg-cnf-dnf)
(in-package :ginsberg-cnf-dnf)

(defparameter *logical-operators* '(<= => <=> and or not if))

(defun sentence-to-cnf (sentence)
  "Rewrite constant predicates, then call CNF"
  (when (find (car sentence) *logical-operators*)
    (setq sentence
      (cons (car sentence)
	    (mapcar #'rewrite-predicate (cdr sentence)) )))
  (cnf sentence) )

(defun rewrite-predicate (predicate)
  "P becomes (P), and (NOT Q) becomes (NOT (Q)), else unchanged"
  (cond
   ((atom predicate)
    (list predicate) )
   ((and (eq 'not (first predicate))
	 (atom (second predicate)) )
    (list 'not (list (second predicate))) )
   (t
    predicate )))

(defun simplify-dnf (dnf)
  (if (and (eq (car dnf) 'or)
	   (null (cddr dnf)) )
      (second dnf)
    dnf ))

;;;----------------------------------------------------------------------------

;; stuff to manipulate propositions

;; functions defined:
;;
;; cnf (p)			returns conjunctive normal form
;; dnf (p)			disjunctive normal form
;; standardize-operators	removes => <= <=> if iff
;; negate (p)			negates it
;; cnf-to-logic (cnf)		inverts cnf
;; dnf-to-logic (dnf)		inverts dnf
;; normal-form (p)		returns something suitable for stating

;; conjunctive normal form.  Remove the nonstandard operators from p and
;; then look at (car p):
;;  1.  If it's NOT, then (cnf p) is the result of negating each term
;;  inside (dnf (not (p))).
;;  2.  If it's OR, then we have to combine the results of cnf'ing each
;;  of the disjuncts.  cnf-merge-lists does this, one disjunct at a
;;  time.
;;  3.  If it's AND, we just call the cnf'er and nconc the results
;;  together.
;;  4.  If it's none of these, it must be a term and we return ((p)).

(defun cnf (p)
  (case (car (setq p (standardize-operators p)))
    (not (mapcar #'(lambda (l) (mapcar #'negate l)) (dnf (second p))))
    (or
      (let (ans)
	(dolist (item (cdr p) (nreverse ans))
	  (setq ans (cnf-merge-lists (cnf item) ans)))))
    (and (mapcan #'cnf (cdr p)))
    (otherwise (list (list p)))))

;; given a partial cnf expression d, and a new cnf exp c to be merged,
;; construct a list of all entries that have an entry from d followed by
;; one from c.  As we go through, the list is maintained backwards, so
;; that we have terms with entries late in d early in the returned
;; answer.  Three cases:
;;  1.  If c is NIL, there is nothing to do.  Return d.
;;  2.  If d is NIL, there is still nothing to do, but we reverse c to
;;  get d into "backwards" form.
;;  3.  Otherwise, work through c and for each entry in it, work through
;;  d, appending each entry of d onto that entry of c and pushing the
;;  result onto the answer being accumulated.  Note that this maintains
;;  the "backwardness" of the answer.  We make sure that each term in
;;  the final answer is a fresh copy.

(defun cnf-merge-lists (c d)
  (cond ((null c) d)
	((null d) (nreverse c))
	(t (let (ans)
	     (dolist (item c ans)
	       (push (append (car d) item) ans)
	       (dolist (x (cdr d)) (push (append x (copy-list item)) ans)))))))

;; remove nonstandard connectives from p.  Handles =>, <=, <=>, if and
;; iff.
;;  1.  (=> p1 ... pn q) means (if (and p1 ... pn) q), or
;;  (or (not (and p1 ... pn)) q).
;;  2.  (<= q p1 ... pn) means (if (and p1 ... pn) q) as well.
;;  3.  (if p q) means (or (not p) q)
;;  4.  (iff p q) means (or (and p q) (and (not p) (not q))); <=> is
;;  similar.

(defun standardize-operators (p)
  (case (car p)
    (|=>| `(or (not (and . ,(butlast (cdr p)))) ,(car (last p))))
    (|<=| `(or ,(second p) (not (and . ,(cddr p)))))
    (if `(or ,(third p) (not ,(second p))))
    ((|<=>| iff) `(or (and ,(second p) ,(third p))
		      (and ,(negate (second p)) ,(negate (third p)))))
    (otherwise p)))

;; dnf is a lot like cnf.

(defun dnf (p)
  (case (car (setq p (standardize-operators p)))
    (not (mapcar #'(lambda (l) (mapcar #'negate l)) (cnf (second p))))
    (and
      (let (ans)
	(dolist (item (cdr p) (nreverse ans))
	  (setq ans (cnf-merge-lists (dnf item) ans)))))
    (or (mapcan #'dnf (cdr p)))
    (otherwise (list (list p)))))

;; negate a sentence.  If it begins with not, return what's left.
;; Otherwise put a not on the front and return that.

(defun negate (p)
  (if (eq 'not (car p)) (cadr p) (list 'not p)))

;; utility functions to turn a cnf expression, or a conjunct/disjunct,
;; into a logical sentence.  They're all simple -- if the thing is a
;; list, push "and" or "or" on the front and proceed.  Otherwise, just
;; return it.

(defun cnf-to-logic (cnf)
  (conj-to-logic (mapcar #'disj-to-logic cnf)))

(defun dnf-to-logic (dnf)
  (disj-to-logic (mapcar #'conj-to-logic dnf)))

(defun conj-to-logic (list)
  (if (cdr list) (cons 'and list) (car list)))

(defun disj-to-logic (list)
  (if (cdr list) (cons 'or list) (car list)))

;; *if-translation* controls how if is converted.  If bc (the default),
;; two backward-chaining versions are created.  If fc, two
;; forward-chaining versions.  If mix, (if a b) translates to (<= b a)
;; and (=> a b).

(defparameter *if-translation* 'bc)

;; *equivalence-translation* controls how <=> is converted.  If bc (the
;; default), two backward-chaining versions are created.  If fc, two
;; forward-chaining versions.  If mix, (<=> a b) translates to (<= a b)
;; and (=> a b).

(defparameter *equivalence-translation* 'bc)

;; normal form.  Depends on (car p):
;;  => invokes nf-forward
;;  <= invokes nf-backward
;;  if is treated by examining *if-translation*
;;  <=>is treated by examining *equivalence-translation*
;;  (or p1 ... pn) is turned into (<= p1 (not (and p2 ... pn)))
;;  (iff a b) is (and (if a b) (if b a))
;;  everything else is treated by calling the cnf'er and then calling
;;  disj-to-logic on each term

(defun normal-form (p)
  (case (car p)
    (|=>| (nf-forward p))
    (|<=| (nf-backward p))
    (if
      (case *if-translation*
	(bc (nconc (nf-backward p)
		   (nf-backward `(if (not ,(third p)) (not ,(second p))))))
	(fc (nconc (nf-forward p)
		   (nf-forward `(if (not ,(third p)) (not ,(second p))))))
	(mix (nconc (nf-backward p) (nf-forward p)))))
    (|<=>|
      (case *equivalence-translation*
	(bc (nconc (nf-backward (cons '|<=| (cdr p)))
		   (nf-backward (list '|<=| (third p) (second p)))))
	(fc (nconc (nf-forward (cons '|=>| (cdr p)))
		   (nf-forward (list '|=>| (third p) (second p)))))
	(mix (nconc (nf-backward (cons '|<=| (cdr p)))
		    (nf-forward (cons '|=>| (cdr p)))))))
    (or (if (cddr p) (normal-form `(<= ,(second p) (not (and . ,(cddr p)))))
	    (normal-form (second p))))
    (iff (nconc (normal-form (cons 'if (cdr p)))
		(normal-form (list 'if (third p) (second p)))))
    (and (mapcan #'normal-form (cdr p)))
    (otherwise (mapcar #'disj-to-logic (cnf p)))))

;; normal form for forward-chaining.  Convert to cnf, then for each
;; term (or p1 ... pn), rewrite it as p1 (if n=1) or
;; (=> (not p1) ... (not pn-1) pn)

(defun nf-forward (p)
  (mapcar #'(lambda (x)
	      (if (cdr x)
		  `(=> ,.(mapcar #'negate (butlast x)) ,(car (last x)))
		(car x)))
	  (cnf p)))

;; backward-chaining.  Convert to cnf, then for each term (or p1 ... pn),
;; rewrite it as p1 (if n=1) or (<= p1 (not p2) ... (not pn))

(defun nf-backward (p)
  (mapcar #'(lambda (x)
	      (if (cdr x)
		  `(<= ,(car x) . ,(mapcar #'negate (cdr x)))
		(car x)))
	  (cnf p)))
