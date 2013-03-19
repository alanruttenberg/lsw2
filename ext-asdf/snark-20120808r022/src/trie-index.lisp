;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: trie-index.lisp
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
;;; Portions created by the Initial Developer are Copyright (C) 1981-2012.
;;; All Rights Reserved.
;;;
;;; Contributor(s): Mark E. Stickel <stickel@ai.sri.com>.

(in-package :snark)

(defvar *trie-index*)

(defstruct (trie-index
	     (:constructor make-trie-index0 (entry-constructor))
	     (:copier nil))
  (entry-constructor nil :read-only t)		;term->entry function for new entry insertion
  (node-counter (make-counter 1) :read-only t)
  (entry-counter (make-counter) :read-only t)
  (top-node (make-trie-index-internal-node) :read-only t)
  (retrieve-generalization-calls 0 :type integer)	;number of generalization retrieval calls
  (retrieve-generalization-count 0 :type integer)
  (retrieve-instance-calls 0 :type integer)		;    "     instance          "
  (retrieve-instance-count 0 :type integer)
  (retrieve-unifiable-calls 0 :type integer)		;    "     unifiable         "
  (retrieve-unifiable-count 0 :type integer)
  (retrieve-variant-calls 0 :type integer)		;    "     variant           "
  (retrieve-variant-count 0 :type integer)
  (retrieve-all-calls 0 :type integer)			;    "     all               "
  (retrieve-all-count 0 :type integer))

(defstruct (trie-index-internal-node
            (:copier nil))
  (variable-child-node nil)			;nil or node
  (constant-indexed-child-nodes nil)		;constant# -> node sparse-vector
  (function-indexed-child-nodes nil))		;function# -> node sparse-vector

(defstruct (trie-index-leaf-node
            (:include sparse-vector (snark-sparse-array::default-value0 none :read-only t))
            (:copier nil))
  )

(defmacro trie-index-leaf-node-entries (n)
  n)

(defstruct (index-entry
	     (:constructor make-index-entry (term))
	     (:copier nil))
  (term nil :read-only t))

(defun make-trie-index (&key (entry-constructor #'make-index-entry))
  (setf *trie-index* (make-trie-index0 entry-constructor)))

(definline trie-index-internal-node-variable-indexed-child-node (node &optional create internal)
  (or (trie-index-internal-node-variable-child-node node)
      (and create
           (progn
             (increment-counter (trie-index-node-counter *trie-index*))
             (setf (trie-index-internal-node-variable-child-node node)
                   (if internal
                       (make-trie-index-internal-node)
                       (make-trie-index-leaf-node)))))))

(definline trie-index-internal-node-constant-indexed-child-node (const node &optional create internal)
  (let ((children (trie-index-internal-node-constant-indexed-child-nodes node)))
    (unless children
      (when create
        (setf children (setf (trie-index-internal-node-constant-indexed-child-nodes node) (make-sparse-vector)))))
    (and children
         (let ((const# (constant-number const)))
           (or (sparef children const#)
               (and create
                    (progn
                      (increment-counter (trie-index-node-counter *trie-index*))
                      (setf (sparef children const#)
                            (if internal
                                (make-trie-index-internal-node)
                                (make-trie-index-leaf-node))))))))))

(definline trie-index-internal-node-function-indexed-child-node (fn node &optional create internal)
  (let ((children (trie-index-internal-node-function-indexed-child-nodes node)))
    (unless children
      (when create
        (setf children (setf (trie-index-internal-node-function-indexed-child-nodes node) (make-sparse-vector)))))
    (and children
         (let ((fn# (function-number fn)))
           (or (sparef children fn#)
               (and create
                    (progn
                      (increment-counter (trie-index-node-counter *trie-index*))
                      (setf (sparef children fn#)
                            (if internal
                                (make-trie-index-internal-node)
                                (make-trie-index-leaf-node))))))))))

(definline function-trie-index-lookup-args (fn term)
  ;; fn = (head term) unless term is nil (not specified)
  (ecase (function-index-type fn)
    ((nil)
     (cond
      ((function-unify-code fn)
       nil)
      (t
       (let ((arity (function-arity fn)))
         (if (eq :any arity) (list (args term)) (args term))))))
    (:commute
     ;; index all arguments, lookup with first two in order and commuted
     ;; (a b c d) -> 4, (c d a b), (c d (%index-or (a b) (b a))) for arity 4
     ;; (a b c d) -> 3, ((c d) a b), ((c d) (%index-or (a b) (b a))) for arity :any
     (let ((arity (function-arity fn)))
       (let* ((args (args term))
              (l (rest (rest args)))
              (a (first args))
              (b (second args))
              (v (list (list '%index-or (if l (list a b) args) (list b a)))))
         (cond
          ((eq :any arity)
           (cons l v))
          (l
           (append l v))
          (t
           v)))))
    (:jepd
     ;; index only first two arguments, lookup with first two in order and commuted
     ;; (a b c) -> 2, (a b), ((%index-or (a b) (b a)))
     (let* ((args (args term))
            (a (first args))
            (b (second args)))
       (list (list '%index-or (list a b) (list b a)))))
    (:hash-but-dont-index
     nil)))

(definline function-trie-index-args (fn term)
  (ecase (function-index-type fn)
    ((nil)
     (cond
      ((function-unify-code fn)
       nil)
      (t
       (let ((arity (function-arity fn)))
         (if (eq :any arity) (list (args term)) (args term))))))
    (:commute
     (let ((arity (function-arity fn)))
       (let* ((args (args term))
              (l (rest (rest args)))
              (v (if l (list (first args) (second args)) args)))
         (cond
          ((eq :any arity)
           (cons l v))
          (l
           (append l v))
          (t
           v)))))
    (:jepd
     (let ((args (args term)))
       (list (first args) (second args))))
    (:hash-but-dont-index
     nil)))

(definline function-trie-index-arity (fn)
  (ecase (function-index-type fn)
    ((nil)
     (cond
      ((function-unify-code fn)
       0)
      (t
       (let ((arity (function-arity fn)))
         (if (eq :any arity) 1 arity)))))
    (:commute
     (let ((arity (function-arity fn)))
       (if (eq :any arity) 3 arity)))
    (:jepd
     2)
    (:hash-but-dont-index
     0)))

(defun simply-indexed-p (term &optional subst)
  (dereference
   term subst
   :if-variable t
   :if-constant t
   :if-compound-cons (and (simply-indexed-p (carc term))
                          (simply-indexed-p (cdrc term)))
   :if-compound-appl (and (let ((fn (heada term)))
                            (ecase (function-index-type fn)
                              ((nil)
                               (null (function-unify-code fn)))
                              (:commute
                               nil)
                              (:hash-but-dont-index
                               t)
                              (:jepd
                               nil)))
                          (dolist (arg (argsa term) t)
                            (unless (simply-indexed-p arg subst)
                              (return nil))))))

(definline trie-index-build-path-for-terms (terms node internal)
  (if internal
      (dolist (x terms node)
        (setf node (trie-index-build-path-for-term x node t)))
      (dotails (l terms node)
        (setf node (trie-index-build-path-for-term (first l) node (rest l))))))

(defun trie-index-build-path-for-term (term node &optional internal)
  (dereference
   term nil
   :if-variable (trie-index-internal-node-variable-indexed-child-node node t internal)
   :if-constant (trie-index-internal-node-constant-indexed-child-node term node t internal)
   :if-compound (let* ((head (head term))
                       (args (function-trie-index-args head term)))
                  (if (null args)
                      (trie-index-internal-node-function-indexed-child-node head node t internal)
                      (trie-index-build-path-for-terms args (trie-index-internal-node-function-indexed-child-node head node t t) internal)))))

(definline trie-index-path-for-terms (terms path)
  (dolist (x terms path)
    (when (null (setf path (trie-index-path-for-term x path)))
      (return nil))))

(defun trie-index-path-for-term (term path)
  (let ((node (first path)))
    (dereference
     term nil
     :if-variable (let ((n (trie-index-internal-node-variable-indexed-child-node node)))
		    (and n (list* n 'variable path)))
     :if-constant (let ((n (trie-index-internal-node-constant-indexed-child-node term node)))
		    (and n (list* n 'constant term path)))
     :if-compound (let* ((head (head term))
			 (n (trie-index-internal-node-function-indexed-child-node head node)))
		    (and n (let ((args (function-trie-index-args head term)))
			     (if (null args)
				 (list* n 'function head path)
				 (trie-index-path-for-terms args (list* n 'function head path)))))))))

(defun trie-index-insert (term &optional entry)
  (let* ((trie-index *trie-index*)
         (entries (trie-index-leaf-node-entries (trie-index-build-path-for-term term (trie-index-top-node trie-index)))))
    (cond
     ((null entry)
      (prog->
        (map-sparse-vector entries :reverse t ->* e)
        (when (or (eql term (index-entry-term e)) (and (test-option38?) (equal-p term (index-entry-term e))))
          (return-from trie-index-insert e)))
      (setf entry (funcall (trie-index-entry-constructor trie-index) term)))
     (t
      (cl:assert (eql term (index-entry-term entry)))
      (prog->
        (map-sparse-vector entries :reverse t ->* e)
        (when (eq entry e)
          (return-from trie-index-insert e))
        (when (or (eql term (index-entry-term e)) (and (test-option38?) (equal-p term (index-entry-term e))))
          (error "There is already a trie-index entry for term ~A." term)))))
    (increment-counter (trie-index-entry-counter trie-index))
    (setf (sparef entries (nonce)) entry)))

(defun trie-index-delete (term &optional entry)
  (let* ((trie-index *trie-index*)
	 (path (trie-index-path-for-term term (list (trie-index-top-node trie-index)))))
    (when path
      (let* ((entries (trie-index-leaf-node-entries (pop path)))
	     (k (cond
		  ((null entry)
		   (prog->
		     (map-sparse-vector-with-indexes entries :reverse t ->* e k)
		     (when (eql term (index-entry-term e))
		       (return-from prog-> k))))
		  (t
		   (cl:assert (eql term (index-entry-term entry)))
		   (prog->
		     (map-sparse-vector-with-indexes entries :reverse t ->* e k)
		     (when (eq entry e)
		       (return-from prog-> k)))))))
	(when k
	  (decrement-counter (trie-index-entry-counter trie-index))
	  (setf (sparef entries k) none)
	  (when (eql 0 (sparse-vector-count entries))
            (let ((node-counter (trie-index-node-counter trie-index))
                  parent)
	      (loop
	        (ecase (pop path)
		  (function
                   (let ((k (function-number (pop path))))
		     (setf (sparef (trie-index-internal-node-function-indexed-child-nodes (setf parent (pop path))) k) nil)))
		  (constant
                   (let ((k (constant-number (pop path))))
		     (setf (sparef (trie-index-internal-node-constant-indexed-child-nodes (setf parent (pop path))) k) nil)))
		  (variable
		   (setf (trie-index-internal-node-variable-child-node (setf parent (pop path))) nil)))
	        (decrement-counter node-counter)
	        (unless (and (rest path)		;not top node
                             (null (trie-index-internal-node-variable-child-node parent))
			     (eql 0 (sparse-vector-count (trie-index-internal-node-function-indexed-child-nodes parent)))
			     (eql 0 (sparse-vector-count (trie-index-internal-node-constant-indexed-child-nodes parent))))
		  (return)))))
	  t)))))

(defmacro map-trie-index-entries (&key if-variable if-constant if-compound count-call count-entry)
  (declare (ignorable count-call count-entry))
  `(labels
     ((map-for-term (cc term node)
        (dereference
         term subst
         :if-variable ,if-variable
         :if-constant ,if-constant
         :if-compound ,if-compound))
      (map-for-terms (cc terms node)
        (cond
         ((null terms)
          (funcall cc node))
         (t
          (let ((term (pop terms)))
            (cond
             ((and (consp term) (eq '%index-or (first term)))
              (cond
               ((null terms)
                (prog->
                  (dolist (rest term) ->* terms1)
                  (map-for-terms terms1 node ->* node)
                  (funcall cc node)))
               (t
                (prog->
                  (dolist (rest term) ->* terms1)
                  (map-for-terms terms1 node ->* node)
                  (map-for-terms terms node ->* node)
                  (funcall cc node)))))
             (t
              (cond
               ((null terms)
                (prog->
                  (map-for-term term node ->* node)
                  (funcall cc node)))
               (t
                (prog->
                  (map-for-term term node ->* node)
                  (map-for-terms terms node ->* node)
                  (funcall cc node))))))))))
      (skip-terms (cc n node)
        (declare (type fixnum n))
        (cond
         ((= 1 n)
          (progn
            (prog->
              (trie-index-internal-node-variable-indexed-child-node node ->nonnil node)
              (funcall cc node))
            (prog->
              (trie-index-internal-node-constant-indexed-child-nodes node ->nonnil constant-indexed-children)
              (map-sparse-vector constant-indexed-children ->* node)
              (funcall cc node))
            (prog->
              (trie-index-internal-node-function-indexed-child-nodes node ->nonnil function-indexed-children)
              (map-sparse-vector-with-indexes function-indexed-children ->* node fn#)
              (skip-terms (function-trie-index-arity (symbol-numbered fn#)) node ->* node)
              (funcall cc node))))
         ((= 0 n)
          (funcall cc node))
         (t
          (progn
            (decf n)
            (prog->
              (trie-index-internal-node-variable-indexed-child-node node ->nonnil node)
              (skip-terms n node ->* node)
              (funcall cc node))
            (prog->
              (trie-index-internal-node-constant-indexed-child-nodes node ->nonnil constant-indexed-children)
              (map-sparse-vector constant-indexed-children ->* node)
              (skip-terms n node ->* node)
              (funcall cc node))
            (prog->
              (trie-index-internal-node-function-indexed-child-nodes node ->nonnil function-indexed-children)
              (map-sparse-vector-with-indexes function-indexed-children ->* node fn#)
              (skip-terms (+ n (function-trie-index-arity (symbol-numbered fn#))) node ->* node)
              (funcall cc node)))))))
     (let ((trie-index *trie-index*))
;;     ,count-call
       (cond
        ((simply-indexed-p term subst)
         (prog->
           (map-for-term term (trie-index-top-node trie-index) ->* leaf-node)
           (map-sparse-vector (trie-index-leaf-node-entries leaf-node) :reverse t ->* e)
;;         ,count-entry
           (funcall cc e)))
        (t
         (prog->
           (quote nil -> seen)
           (map-for-term term (trie-index-top-node trie-index) ->* leaf-node)
           (when (do ((s seen (cdrc s)))	;(not (member leaf-node seen))
                     ((null s)
                      t)
                   (when (eq leaf-node (carc s))
                     (return nil)))
             (prog->
               (map-sparse-vector (trie-index-leaf-node-entries leaf-node) :reverse t ->* e)
;;             ,count-entry
               (funcall cc e))
             (setf seen (cons leaf-node seen)))))))
     nil))

(defun map-trie-index-instance-entries (cc term subst)
  (map-trie-index-entries
   :count-call (incf (trie-index-retrieve-instance-calls trie-index))
   :count-entry (incf (trie-index-retrieve-instance-count trie-index))
   :if-variable (prog->
                  (skip-terms 1 node ->* node)
                  (funcall cc node))
   :if-constant (prog->
                  (trie-index-internal-node-constant-indexed-child-node term node ->nonnil node)
                  (funcall cc node))
   :if-compound (prog->
                  (head term -> head)
                  (trie-index-internal-node-function-indexed-child-node head node ->nonnil node)
                  (map-for-terms (function-trie-index-lookup-args head term) node ->* node)
                  (funcall cc node))))

(defun map-trie-index-generalization-entries (cc term subst)
  ;; in snark-20060805 vs. snark-20060806 test over TPTP,
  ;; constant and compound lookup before variable lookup outperforms
  ;; variable lookup before constant and compound lookup
  (map-trie-index-entries
   :count-call (incf (trie-index-retrieve-generalization-calls trie-index))
   :count-entry (incf (trie-index-retrieve-generalization-count trie-index))
   :if-variable (prog->
                  (trie-index-internal-node-variable-indexed-child-node node ->nonnil node)
                  (funcall cc node))
   :if-constant (progn
                  (prog->
                    (trie-index-internal-node-constant-indexed-child-node term node ->nonnil node)
                    (funcall cc node))
                  (prog->
                    (trie-index-internal-node-variable-indexed-child-node node ->nonnil node)
                    (funcall cc node)))
   :if-compound (progn
                  (prog->
                    (head term -> head)
                    (trie-index-internal-node-function-indexed-child-node head node ->nonnil node)
                    (map-for-terms (function-trie-index-lookup-args head term) node ->* node)
                    (funcall cc node))
                  (prog->
                    (trie-index-internal-node-variable-indexed-child-node node ->nonnil node)
                    (funcall cc node)))))

(defun map-trie-index-unifiable-entries (cc term subst)
  (map-trie-index-entries
   :count-call (incf (trie-index-retrieve-unifiable-calls trie-index))
   :count-entry (incf (trie-index-retrieve-unifiable-count trie-index))
   :if-variable (prog->
                  (skip-terms 1 node ->* node)
                  (funcall cc node))
   :if-constant (progn
                  (prog->
                    (trie-index-internal-node-variable-indexed-child-node node ->nonnil node)
                    (funcall cc node))
                  (prog->
                    (trie-index-internal-node-constant-indexed-child-node term node ->nonnil node)
                    (funcall cc node)))
   :if-compound (progn
                  (prog->
                    (trie-index-internal-node-variable-indexed-child-node node ->nonnil node)
                    (funcall cc node))
                  (prog->
                    (head term -> head)
                    (trie-index-internal-node-function-indexed-child-node head node ->nonnil node)
                    (map-for-terms (function-trie-index-lookup-args head term) node ->* node)
                    (funcall cc node)))))

(defun map-trie-index-variant-entries (cc term subst)
  (map-trie-index-entries
   :count-call (incf (trie-index-retrieve-variant-calls trie-index))
   :count-entry (incf (trie-index-retrieve-variant-count trie-index))
   :if-variable (prog->
                  (trie-index-internal-node-variable-indexed-child-node node ->nonnil node)
                  (funcall cc node))
   :if-constant (prog->
                  (trie-index-internal-node-constant-indexed-child-node term node ->nonnil node)
                  (funcall cc node))
   :if-compound (prog->
                  (head term -> head)
                  (trie-index-internal-node-function-indexed-child-node head node ->nonnil node)
                  (map-for-terms (function-trie-index-lookup-args head term) node ->* node)
                  (funcall cc node))))

(defun map-trie-index-all-entries (cc)
  (let ((term (make-variable nil 0))
        (subst nil))
    (map-trie-index-entries
     :count-call (incf (trie-index-retrieve-all-calls trie-index))
     :count-entry (incf (trie-index-retrieve-all-count trie-index))
     :if-variable (prog->
                    (skip-terms 1 node ->* node)
                    (funcall cc node)))))

(definline map-trie-index (cc type term &optional subst)
  (ecase type
    (:generalization
     (map-trie-index-generalization-entries cc term subst))
    (:instance
     (map-trie-index-instance-entries cc term subst))
    (:unifiable
     (map-trie-index-unifiable-entries cc term subst))
    (:variant
     (map-trie-index-variant-entries cc term subst))))

(defun print-trie-index (&key terms nodes)
  (let ((index *trie-index*))
    (mvlet (((:values current peak added deleted) (counter-values (trie-index-entry-counter index))))
      (format t "~%; Trie-index has ~:D entr~:@P (~:D at peak, ~:D added, ~:D deleted)." current peak added deleted))
    (mvlet (((:values current peak added deleted) (counter-values (trie-index-node-counter index))))
      (format t "~%; Trie-index has ~:D node~:P (~:D at peak, ~:D added, ~:D deleted)." current peak added deleted))
    (unless (eql 0 (trie-index-retrieve-variant-calls index))
      (format t "~%; Trie-index retrieved ~:D variant term~:P in ~:D call~:P."
              (trie-index-retrieve-variant-count index)
              (trie-index-retrieve-variant-calls index)))
    (unless (eql 0 (trie-index-retrieve-generalization-calls index))
      (format t "~%; Trie-index retrieved ~:D generalization term~:P in ~:D call~:P."
              (trie-index-retrieve-generalization-count index)
              (trie-index-retrieve-generalization-calls index)))
    (unless (eql 0 (trie-index-retrieve-instance-calls index))
      (format t "~%; Trie-index retrieved ~:D instance term~:P in ~:D call~:P."
              (trie-index-retrieve-instance-count index)
              (trie-index-retrieve-instance-calls index)))
    (unless (eql 0 (trie-index-retrieve-unifiable-calls index))
      (format t "~%; Trie-index retrieved ~:D unifiable term~:P in ~:D call~:P."
              (trie-index-retrieve-unifiable-count index)
              (trie-index-retrieve-unifiable-calls index)))
    (unless (eql 0 (trie-index-retrieve-all-calls index))
      (format t "~%; Trie-index retrieved ~:D unrestricted term~:P in ~:D call~:P."
              (trie-index-retrieve-all-count index)
              (trie-index-retrieve-all-calls index)))
    (when (or nodes terms)
      (print-index* (trie-index-top-node index) nil terms))))

(defun print-index* (node revpath print-terms)
  (prog->
    (map-index-leaf-nodes node revpath ->* node revpath)
    (print-index-leaf-node node revpath print-terms)))

(defmethod map-index-leaf-nodes (cc (node trie-index-internal-node) revpath)
  (prog->
    (trie-index-internal-node-variable-indexed-child-node node ->nonnil node)
    (map-index-leaf-nodes node (cons '? revpath) ->* node revpath)
    (funcall cc node revpath))
  (prog->
    (map-sparse-vector-with-indexes (trie-index-internal-node-constant-indexed-child-nodes node) ->* node const#)
    (map-index-leaf-nodes node (cons (symbol-numbered const#) revpath) ->* node revpath)
    (funcall cc node revpath))
  (prog->
    (map-sparse-vector-with-indexes (trie-index-internal-node-function-indexed-child-nodes node) ->* node fn#)
    (map-index-leaf-nodes node (cons (symbol-numbered fn#) revpath) ->* node revpath)
    (funcall cc node revpath)))

(defmethod map-index-leaf-nodes (cc (node trie-index-leaf-node) revpath)
  (funcall cc node revpath))

(defmethod print-index-leaf-node ((node trie-index-leaf-node) revpath print-terms)
  (with-standard-io-syntax2
    (prog->
      (trie-index-leaf-node-entries node -> entries)
      (format t "~%; Path ~A has ~:D entr~:@P." (reverse revpath) (sparse-vector-count entries))
      (when print-terms
        (map-sparse-vector entries :reverse t ->* entry)
        (format t "~%;    ")
        (print-term (index-entry-term entry))))))

;;; trie-index.lisp EOF
