;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: wffs.lisp
;;; The contents of this file are subject to the Mozilla Public License
;;; Version 1.1 (the "License"); you may not use this file except in
;;; compliance with the License. You may obtain a copy of the License at
;;; http://www.mozilla.org/MPL/
;;;
;;; Software distributed under the License is distributed on an "AS IS"
;;; basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
;;; License for the specific language governing rights and limitations
;;; under the License.
;;;
;;; The Original Code is SNARK.
;;; The Initial Developer of the Original Code is SRI International.
;;; Portions created by the Initial Developer are Copyright (C) 1981-2011.
;;; All Rights Reserved.
;;;
;;; Contributor(s): Mark E. Stickel <stickel@ai.sri.com>.

(in-package :snark)

;;; wff = well-formed formula
;;; atom = atomic fomula

(defun map-atoms-in-clause (cc wff0)
  (labels
    ((map-atoms (wff polarity)
       (dereference
        wff nil
        :if-constant (cond
                      ((eq true wff)
                       (when (eq :pos polarity)
                         (not-clause-error wff0)))
                      ((eq false wff)
                       (when (eq :neg polarity)
                         (not-clause-error wff0)))		
                      (t
                       (funcall cc wff polarity)))
        :if-variable (not-clause-error wff0)
        :if-compound-cons (not-clause-error wff0)
        :if-compound-appl (case (function-logical-symbol-p (heada wff))
                            ((nil)
                             (funcall cc wff polarity))
                            (not
                             (map-atoms (arg1a wff) (if (eq :pos polarity) :neg :pos)))
                            (and
                             (if (eq :pos polarity)
                                 (not-clause-error wff0)
                                 (dolist (arg (argsa wff))
                                   (map-atoms arg :neg))))
                            (or
                             (if (eq :neg polarity)
                                 (not-clause-error wff0)
                                 (dolist (arg (argsa wff))
                                   (map-atoms arg :pos))))
                            (implies
                             (if (eq :neg polarity)
                                 (not-clause-error wff0)
                                 (let ((args (argsa wff)))
                                   (map-atoms (first args) :neg)
                                   (map-atoms (second args) :pos))))
                            (implied-by
                             (if (eq :neg polarity)
                                 (not-clause-error wff0)
                                 (let ((args (argsa wff)))
                                   (map-atoms (first args) :pos)
                                   (map-atoms (second args) :neg))))))))
    (map-atoms wff0 :pos)))

(defun map-atoms-in-wff (cc wff &optional (polarity :pos))
  (dereference
    wff nil
    :if-constant (unless (or (eq true wff) (eq false wff))
		   (funcall cc wff polarity))
    :if-variable (not-wff-error wff)
    :if-compound-cons (not-wff-error wff)
    :if-compound-appl (let ((head (heada wff)))
		        (if (function-logical-symbol-p head)
		            (map-atoms-in-list-of-wffs cc (argsa wff) (function-polarity-map head) polarity)
		            (funcall cc wff polarity))))
  nil)

(defun map-atoms-in-wff-and-compose-result (cc wff &optional (polarity :pos))
  (dereference
    wff nil
    :if-constant (if (or (eq true wff) (eq false wff))
		     wff
		     (funcall cc wff polarity))
    :if-variable (not-wff-error wff)
    :if-compound-cons (not-wff-error wff)
    :if-compound-appl (prog->
		        (heada wff -> head)
		        (cond
		         ((function-logical-symbol-p head)
		          (argsa wff -> args)
		          (cond
			   ((null args)
			    wff)
			   ((null (rest args))
			    (first args -> arg)
			    (map-atoms-in-wff-and-compose-result cc arg (map-polarity (first (function-polarity-map head)) polarity) -> arg*)
			    (if (eq arg arg*) wff (fancy-make-compound* head (list arg*))))
			   (t
			    (map-atoms-in-list-of-wffs-and-compose-result cc args (function-polarity-map head) polarity -> args*)
			    (if (eq args args*) wff (fancy-make-compound* head args*)))))
		         (t
		          (funcall cc wff polarity))))))

(defun map-terms-in-wff (cc wff &optional subst (polarity :pos))
  (prog->
    (map-atoms-in-wff wff polarity ->* atom polarity)
    (map-terms-in-atom cc atom subst polarity)))

(defun map-terms-in-wff-and-compose-result (cc wff &optional subst (polarity :pos))
  (prog->
    (map-atoms-in-wff-and-compose-result wff polarity ->* atom polarity)
    (map-terms-in-atom-and-compose-result cc atom subst polarity)))

(defun map-terms-in-atom (cc atom &optional subst (polarity :pos))
  (dereference
    atom nil
    :if-variable (not-wff-error atom)
    :if-compound-cons (not-wff-error atom)
    :if-compound-appl (map-terms-in-list-of-terms cc nil (argsa atom) subst polarity)))

(defun map-terms-in-atom-and-compose-result (cc atom &optional subst (polarity :pos))
  (dereference
    atom nil
    :if-constant atom
    :if-variable (not-wff-error atom)
    :if-compound-cons (not-wff-error atom)
    :if-compound-appl (let* ((args (argsa atom))
			     (args* (map-terms-in-list-of-terms-and-compose-result cc nil args subst polarity)))
		        (if (eq args args*)
		            atom
		            (make-compound* (heada atom) args*)))))

(defun map-terms-in-term (cc term &optional subst (polarity :pos))
  (dereference
    term subst
    :if-constant (funcall cc term polarity)
    :if-variable (funcall cc term polarity)
    :if-compound-cons (progn
                        (map-terms-in-term cc (carc term) subst polarity)
                        (map-terms-in-term cc (cdrc term) subst polarity)
                        (funcall cc term polarity))
    :if-compound-appl (let* ((head (heada term))
			     (head-if-associative (and (function-associative head) head)))
		        (map-terms-in-list-of-terms cc head-if-associative (argsa term) subst polarity)
                        (funcall cc term polarity))))

(defun map-terms-in-term-and-compose-result (cc term &optional subst (polarity :pos))
  (dereference
    term subst
    :if-constant (funcall cc term polarity)
    :if-variable (funcall cc term polarity)
    :if-compound-cons (lcons (map-terms-in-term-and-compose-result cc (car term) subst polarity)
                             (map-terms-in-term-and-compose-result cc (cdr term) subst polarity)
                             term)
    :if-compound-appl (let* ((head (heada term))
			     (head-if-associative (and (function-associative head) head)))
		        (funcall cc
			         (let* ((args (argsa term))
				        (args* (map-terms-in-list-of-terms-and-compose-result cc head-if-associative args subst polarity)))
			           (if (eq args args*)
				       term
				       (make-compound* (head term) args*)))
			         polarity))))

(defun map-terms-in-list-of-terms (cc head-if-associative terms subst polarity)
  (dolist (term terms)
    (dereference
      term subst
      :if-variable (funcall cc term polarity)
      :if-constant (funcall cc term polarity)
      :if-compound-cons (progn
                          (map-terms-in-term cc (carc term) subst polarity)
                          (map-terms-in-term cc (cdrc term) subst polarity)
                          (funcall cc term polarity))
      :if-compound-appl (let ((head (heada term)))
		          (map-terms-in-list-of-terms
                           cc (and (function-associative head) head) (argsa term) subst polarity)
		          (unless (and head-if-associative (eq head head-if-associative))
		            (funcall cc term polarity))))))

(defvar map-atoms-first nil)

(defun map-atoms-in-list-of-wffs (cc wffs polarity-map polarity)
  (cond
   (map-atoms-first
    (let ((polarity-map polarity-map))
      (dolist (wff wffs)
        (let ((polarity-fun (pop polarity-map)))
          (unless (head-is-logical-symbol wff)
            (map-atoms-in-wff cc wff (map-polarity polarity-fun polarity))))))
    (let ((polarity-map polarity-map))
      (dolist (wff wffs)
        (let ((polarity-fun (pop polarity-map)))
          (when (head-is-logical-symbol wff)
            (map-atoms-in-wff cc wff (map-polarity polarity-fun polarity)))))))
   (t
    (let ((polarity-map polarity-map))
      (dolist (wff wffs)
        (let ((polarity-fun (pop polarity-map)))
          (map-atoms-in-wff cc wff (map-polarity polarity-fun polarity))))))))

(defun map-terms-in-list-of-terms-and-compose-result (cc head-if-associative terms subst polarity)
  (cond
    ((null terms)
     nil)
    (t
     (let ((term (first terms)))
       (dereference
	 term subst
	 :if-constant (lcons (funcall cc term polarity)
			     (map-terms-in-list-of-terms-and-compose-result
                              cc head-if-associative (rest terms) subst polarity)
			     terms)
	 :if-variable (lcons (funcall cc term polarity)
			     (map-terms-in-list-of-terms-and-compose-result
			       cc head-if-associative (rest terms) subst polarity)
			     terms)
	 :if-compound (cond
			((and head-if-associative (eq (head term) head-if-associative))
			 (append (map-terms-in-list-of-terms-and-compose-result
                                  cc head-if-associative (args term) subst polarity)
				 (map-terms-in-list-of-terms-and-compose-result
                                  cc head-if-associative (rest terms) subst polarity)))
			(t
			 (lcons (map-terms-in-term-and-compose-result
                                 cc term subst polarity)
				(map-terms-in-list-of-terms-and-compose-result
                                 cc head-if-associative (rest terms) subst polarity)
				terms))))))))

(defun map-atoms-in-list-of-wffs-and-compose-result (cc wffs polarity-map polarity)
  ;; always called with at least two wffs
  (let* ((x (first wffs))
	 (x* (map-atoms-in-wff-and-compose-result
              cc x (map-polarity (first polarity-map) polarity)))
	 (y (rest wffs)))
    (cond
      ((null (rest y))
       (let* ((z (first y))
	      (z* (map-atoms-in-wff-and-compose-result
                   cc z (map-polarity (second polarity-map) polarity))))
	 (cond
	   ((eq z z*)
	    (cond
	      ((eq x x*)
	       wffs)
	      (t
	       (cons x* y))))
	   (t
	    (list x* z*)))))
      (t
       (lcons x*
	      (map-atoms-in-list-of-wffs-and-compose-result
               cc (rest wffs) (rest polarity-map) polarity)
	      wffs)))))

(defun map-atoms-in-alist-of-wffs-and-compose-result (cc alist &optional polarity)
  (lcons (let ((p (first alist)))
           (lcons (car p) (map-atoms-in-wff-and-compose-result cc (cdr p) polarity) p))
         (map-atoms-in-alist-of-wffs-and-compose-result cc (rest alist) polarity)
         alist))

(defun map-terms-in-list-of-wffs-and-compose-result (cc wffs subst polarity)
  (lcons (map-terms-in-wff-and-compose-result cc (first wffs) subst polarity)
	 (map-terms-in-list-of-wffs-and-compose-result cc (rest wffs) subst polarity)
	 wffs))

(defun map-conjuncts (cc wff)
  (if (conjunction-p wff)
      (mapc (lambda (wff) (map-conjuncts cc wff)) (args wff))
      (funcall cc wff))
  nil)

(defun replace-atom-in-wff (wff atom value)
  (let* ((replaced nil)
         (wff* (prog->
                 (map-atoms-in-wff-and-compose-result wff ->* a p)
                 (declare (ignore p))
                 (if (equal-p atom a)		;would prefer to use eq
                     (progn (setf replaced t) value)
                     a))))
      (cl:assert replaced)
      wff*))

(defun atoms-in-wff (wff &optional subst atoms)
  (prog->
    (last atoms -> atoms-last)
    (map-atoms-in-wff wff :pos ->* atom polarity)
    (declare (ignore polarity))
    (unless (member-p atom atoms subst)
      (collect atom atoms)))
  atoms)

(defun atoms-in-wffs (wffs &optional subst atoms)
  (prog->
    (dolist wffs ->* wff)
    (setf atoms (atoms-in-wff wff subst atoms)))
  atoms)

(defun atoms-in-wff2 (wff &optional subst (polarity :pos) variable-block)
  (let ((atoms-and-polarities nil) atoms-and-polarities-last)
    (prog->
      (map-atoms-in-wff wff polarity ->* atom polarity)
      (when variable-block
	(setf atom (instantiate atom variable-block)))
      (assoc-p atom atoms-and-polarities subst -> v)
      (cond
	((null v)
         (collect (list atom polarity) atoms-and-polarities))
	((neq polarity (second v))
	 (setf (second v) :both))))
    atoms-and-polarities))

(defun atoms-in-clause2 (clause &optional except-atom renumber)
  (let ((atoms-and-polarities nil) atoms-and-polarities-last
        (except-atom-found nil)
        (rsubst nil))
    (prog->
      (map-atoms-in-clause clause ->* atom polarity)
      (cond
       ((equal-p except-atom atom)		;would prefer to use eq
        (setf except-atom-found t))
       (t
        (when renumber
          (setf (values atom rsubst) (renumber-new atom nil rsubst)))
        (collect (list atom polarity) atoms-and-polarities))))
    (cl:assert (implies except-atom except-atom-found))
    atoms-and-polarities))

(defun atoms-to-clause2 (atoms-and-polarities)
  ;; inverse of atoms-in-clause2
  (cond
   ((null atoms-and-polarities)
    false)
   ((null (rest atoms-and-polarities))
    (let ((x (first atoms-and-polarities)))
      (if (eq :pos (second x)) (first x) (make-compound *not* (first x)))))
   (t
    (make-compound*
     *or*
     (mapcar (lambda (x) (if (eq :pos (second x)) (first x) (make-compound *not* (first x))))
             atoms-and-polarities)))))

(defun atoms-in-clause3 (clause &optional except-atom renumber)
  (let ((negatoms nil) negatoms-last
        (posatoms nil) posatoms-last
        (except-atom-found nil)
        (rsubst nil))
    (prog->
      (map-atoms-in-clause clause ->* atom polarity)
      (cond
       ((equal-p except-atom atom)		;would prefer to use eq
        (setf except-atom-found t))
       (t
        (when renumber
          (setf (values atom rsubst) (renumber-new atom nil rsubst)))
        (ecase polarity
          (:neg
           (collect atom negatoms))
          (:pos
           (collect atom posatoms))))))
    (cl:assert (implies except-atom except-atom-found))
    (values negatoms posatoms)))

(defun atoms-to-clause3 (negatoms posatoms)
  ;; inverse of atoms-in-clause3
  (let ((literals nil) literals-last)
    (dolist (atom negatoms)
      (collect (make-compound *not* atom) literals))
    (dolist (atom posatoms)
      (collect atom literals))
    (literals-to-clause literals)))

(defun literals-in-clause (clause &optional except-atom renumber)
  (let ((literals nil) literals-last
        (except-atom-found nil)
        (rsubst nil))
    (prog->
      (map-atoms-in-clause clause ->* atom polarity)
      (cond
       ((equal-p except-atom atom)		;would prefer to use eq
        (setf except-atom-found t))
       (t
        (when renumber
          (setf (values atom rsubst) (renumber-new atom nil rsubst)))
        (ecase polarity
          (:pos
           (collect atom literals))
          (:neg
           (collect (make-compound *not* atom) literals))))))
    (cl:assert (implies except-atom except-atom-found))
    literals))

(defun literals-to-clause (literals)
  ;; inverse of literals-in-clause
  (cond
   ((null literals)
    false)
   ((null (rest literals))
    (first literals))
   (t
    (make-compound* *or* literals))))

(defun first-negative-literal-in-wff (wff)
  (prog->
    (map-atoms-in-wff wff ->* atom polarity)
    (when (eq :neg polarity)
      (return-from first-negative-literal-in-wff atom)))
  nil)

(defun first-positive-literal-in-wff (wff)
  (prog->
    (map-atoms-in-wff wff ->* atom polarity)
    (when (eq :pos polarity)
      (return-from first-positive-literal-in-wff atom)))
  nil)

(defun do-not-resolve (atom &optional subst)
  (dereference
   atom subst
   :if-compound (function-do-not-resolve (head atom))
   :if-constant (constant-do-not-resolve atom)))

(defun do-not-factor (atom &optional subst)
  (dereference
   atom subst
   :if-compound (function-do-not-factor (head atom))))

(defun wff-positive-or-negative (wff)
  ;; :pos if wff contains at least one atom and all atom occurrences are positive
  ;; :neg if wff contains at least one atom and all atom occurrences are negative
  ;; nil otherwise
  (let ((result nil))
    (prog->
      (map-atoms-in-wff wff ->* atom polarity)
      (unless (or (do-not-resolve atom) (eq result polarity))
        (if (and (null result) (or (eq :pos polarity) (eq :neg polarity)))
            (setf result polarity)
            (return-from wff-positive-or-negative nil))))
    result))

(defun atom-satisfies-sequential-restriction-p (atom wff &optional subst)
  (dereference
   wff nil
   :if-constant (equal-p atom wff subst)
   :if-compound (if (function-logical-symbol-p (head wff))
                    (atom-satisfies-sequential-restriction-p atom (arg1 wff) subst)
                    (equal-p atom wff subst))))

(defun term-satisfies-sequential-restriction-p (term wff &optional subst)
  (dereference
   wff nil
   :if-compound (if (function-logical-symbol-p (head wff))
                    (term-satisfies-sequential-restriction-p term (arg1 wff) subst)
                    (occurs-p term wff subst))))

(defun salsify (sat wff interpretation continuation)
  #+(or symbolics ti) (declare (sys:downward-funarg continuation))
  ;; SAT = T if trying to satisfy WFF, NIL if trying to falsify	WFF
  (cond
    ((eq true wff)
     (when sat
       (funcall continuation interpretation)))
    ((eq false wff)
     (unless sat
       (funcall continuation interpretation)))
    (t
     (let* ((head (and (compound-p wff) (head wff)))
	    (kind (and head (function-logical-symbol-p head))))
       (ecase kind
	 (not
	   (salsify (not sat) (arg1 wff) interpretation continuation))
	 (and
	   (let ((args (args wff)))
	     (cond
	       ((null args)
		(when sat
		  (funcall continuation interpretation)))
	       ((null (rest args))
		(salsify sat (first args) interpretation continuation))
	       (sat
		(let ((arg2 (if (null (cddr args))
				(second args)
				(make-compound* *and* (rest args)))))
		  (salsify sat (first args) interpretation
			   (lambda (i) (salsify sat arg2 i continuation)))))
	       (t
		(dolist (arg args)
		  (salsify sat arg interpretation continuation))))))
	 (or
	   (let ((args (args wff)))
	     (cond
	       ((null args)
		(unless sat
		  (funcall continuation interpretation)))
	       ((null (rest args))
		(salsify sat (first args) interpretation continuation))
	       ((not sat)
		(let ((arg2 (if (null (cddr args))
				(second args)
				(make-compound* *or* (rest args)))))
		  (salsify sat (first args) interpretation
			   (lambda (i) (salsify sat arg2 i continuation)))))
	       (t
		(dolist (arg args)
		  (salsify sat arg interpretation continuation))))))
	 (implies
	   (let ((args (args wff)))
	     (cond
	       (sat
		(salsify nil (first args) interpretation continuation)
		(salsify t (second args) interpretation continuation))
	       (t
		(salsify t (first args) interpretation
			 (lambda (i) (salsify nil (second args) i continuation)))))))
	 (implied-by
	   (let ((args (args wff)))
	     (cond
	       (sat
		(salsify nil (second args) interpretation continuation)
		(salsify t (first args) interpretation continuation))
	       (t
		(salsify t (second args) interpretation
			 (lambda (i) (salsify nil (first args) i continuation)))))))
	 ((iff xor)
	  (let* ((args (args wff))
		 (arg1 (first args))
		 (arg2 (if (null (cddr args)) (second args) (make-compound* head (rest args)))))
	    (salsify (if (eq 'iff kind) sat (not sat))
		     (make-compound *and*
				    (make-compound *or* (make-compound *not* arg1) arg2)
				    (make-compound *or* (make-compound *not* arg2) arg1))
		     interpretation
		     continuation)))
	 ((if answer-if)
          (let ((args (args wff)))
            (salsify t (first args) interpretation (lambda (i) (salsify sat (second args) i continuation)))
            (salsify nil (first args) interpretation (lambda (i) (salsify sat (third args) i continuation)))))
	 ((nil)					;atomic
	  (let ((v (assoc wff interpretation :test #'equal-p)))
	    (cond
	      ((null v)
	       (funcall continuation (cons (cons wff (if sat true false)) interpretation)))
	      ((eq (if sat true false) (cdr v))
	       (funcall continuation interpretation))))))))))

(defun propositional-contradiction-p (wff)
  (salsify t wff nil (lambda (i)
                       (declare (ignore i))
                       (return-from propositional-contradiction-p nil)))
  t)

(defun propositional-tautology-p (wff)
  (propositional-contradiction-p (negate wff)))

(defun flatten-term (term subst)
  (dereference
    term subst
    :if-constant term
    :if-variable term
    :if-compound (let* ((head (head term))
			(head-if-associative (and (function-associative head) head))
			(args (args term))
			(args* (flatten-list args subst head-if-associative)))
		   (if (eq args args*)		;CHECK (<= (LENGTH ARGS*) 2)??????
		       term
		       (make-compound* head args*)))))

(defun flatten-list (terms subst head-if-associative)
  (cond
    ((null terms)
     nil)
    (t
     (let ((term (first terms)))
       (cond
	 ((and head-if-associative (dereference term subst :if-compound (eq (head term) head-if-associative)))
	  (flatten-list (append (args term) (rest terms)) subst head-if-associative))
	 (t
	  (lcons (flatten-term term subst)
		 (flatten-list (rest terms) subst head-if-associative)
		 terms)))))))

(defun unflatten-term1 (term subst)
  ;; when f is associative, (f a b c) -> (f a (f b c)); leaves (f) and (f a) alone; doesn't unflatten subterms
  (dereference
   term subst
   :if-constant term
   :if-variable term
   :if-compound (let ((head (head term))
                      (args (args term)))
                  (cond
                   ((and (function-associative head) (rrest args))
                    (let* ((l (reverse args))
                           (term* (first l)))
                      (dolist (x (rest l))
                        (setf term* (make-compound head x term*)))
                      term*))
                   (t
                    term)))))                      

(defun unflatten-term (term subst)
  ;; when f is associative, (f a b c) -> (f a (f b c)); leaves (f) and (f a) alone; unflattens subterms too
  (dereference
   term subst
   :if-constant term
   :if-variable term
   :if-compound (labels
                  ((unflatten-list (terms)
                     (lcons (unflatten-term (first terms) subst)
                            (unflatten-list (rest terms))
                            terms)))
                  (let* ((args (args term))
                         (args* (unflatten-list args)))
                    (unflatten-term1 (if (eq args args*) term (make-compound* (head term) args*)) subst)))))

(defun flatten-args (fn args subst)
  (labels
    ((fa (args)
       (if (null args)
           args
           (let ((arg (first args)))
             (cond
              ((dereference arg subst :if-compound-appl (eq fn (heada arg)))
               (fa (append (argsa arg) (rest args))))
              (t
               (let* ((args1 (rest args))
                      (args1* (fa args1)))
                 (if (eq args1 args1*) args (cons arg args1*)))))))))
    (fa args)))

(defun fn-chain-tail (fn x subst &optional (len 0))
  ;; for a fn chain, return tail and length
  ;; (bag a b) = (bag-cons a (bag-cons b empty-bag)) -> empty-bag,2
  ;; (bag* a b) = (bag-cons a b) -> b,1
  (loop
    (dereference
     x subst
     :if-variable (return-from fn-chain-tail (values x len))
     :if-constant (return-from fn-chain-tail (values x len))
     :if-compound (if (eq fn (head x))
                      (setf x (second (args x)) len (+ 1 len))
                      (return-from fn-chain-tail (values x len))))))

(defun fn-chain-items (fn x subst)
  ;; (bag a b) = (bag-cons a (bag-cons b empty-bag)) -> (a b)
  ;; (bag* a b) = (bag-cons a b) -> (a)
  (let ((items nil) items-last)
    (loop
      (dereference
       x subst
       :if-variable (return)
       :if-constant (return)
       :if-compound (if (eq fn (head x))
                        (let ((args (args x)))
                          (collect (first args) items)
                          (setf x (second args)))
                        (return))))
    items))

(defun make-fn-chain (fn items tail)
  (labels
    ((mfc (items)
       (if (null items) tail (make-compound fn (first items) (mfc (rest items))))))
    (mfc items)))

(defun make-compound1 (fn identity arg1 arg2)
  (cond
   ((eql identity arg1)
    arg2)
   ((eql identity arg2)
    arg1)
   (t
    (make-compound fn arg1 arg2))))

;;; wffs.lisp EOF
