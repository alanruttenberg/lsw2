;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: path-index.lisp
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

(declaim (special *terpri-indent*))

(defvar *path-index*)

(defstruct (path-index
	     (:constructor make-path-index0 (entry-constructor entries))
	     (:copier nil))
  (entry-constructor nil :read-only t)		;term->entry function for new entry insertion
  (node-counter (make-counter 1) :read-only t)
  (entry-counter (make-counter) :read-only t)
  (top-node (make-path-index-internal-node1 :mark nil) :read-only t)
  (entries nil :read-only t))			;term->entry hash-table for entry lookup

(defstruct (path-index-node
	     (:copier nil))
  (parent-node nil :read-only t)
  (mark (increment-counter (path-index-node-counter *path-index*))))

(defstruct (path-index-internal-node1
            (:include path-index-node)
            (:copier nil))
  (variable-child-node nil)			;nil or internal-node
  (constant-indexed-child-nodes (make-sparse-vector))	;constant# -> leaf-node sparse-vector
  (function-indexed-child-nodes (make-sparse-vector)))	;function# -> internal-node sparse-vector

(defstruct (path-index-internal-node2
	     (:include path-index-node)
	     (:copier nil))
  (integer-indexed-child-nodes nil :read-only t)	;vector of internal-nodes (or nil) indexed by argument position
  query)					;node in integer-indexed-child-nodes to use to generate all instances

(defstruct (path-index-leaf-node
	     (:include path-index-node)
	     (:copier nil))
  (entries (make-sparse-vector) :read-only t))

(defstruct (path-index-entry
             (:include index-entry)
	     (:constructor make-path-index-entry (term))
	     (:copier nil))
  in-nodes					;vector of (possible query) nodes that contain entry
  in-nodes-last					;last index into in-nodes
  (mark nil))

(defun make-path-index (&key (entry-constructor #'make-path-index-entry))
  (setf *path-index* (make-path-index0 entry-constructor (make-sparse-vector))))

(defmacro path-index-internal-node1-function-indexed-child-node (head node1)
  `(sparef (path-index-internal-node1-function-indexed-child-nodes ,node1) (function-number ,head)))

(defmacro path-index-internal-node1-constant-indexed-child-node (const node1)
  `(sparef (path-index-internal-node1-constant-indexed-child-nodes ,node1) (constant-number ,const)))

(defmacro add-path-index-internal-node1-function-indexed-child-node (head node1 node)
  `(setf (path-index-internal-node1-function-indexed-child-node ,head ,node1) ,node))

(defmacro add-path-index-internal-node1-constant-indexed-child-node (const node1 node)
  `(setf (path-index-internal-node1-constant-indexed-child-node ,const ,node1) ,node))

(defun path-index-entry (term)
  ;; return path-index-entry for term
  ;; create one if there isn't one
  (let ((term# (funcall *standard-eql-numbering* :lookup term)))
    (or (sparef (path-index-entries *path-index*) term#)
        (path-index-insert term))))

(defun the-path-index-entry (term)
  ;; return path-index-entry for term
  ;; error if there isn't one
  (let ((term# (funcall *standard-eql-numbering* :lookup term)))
    (or (sparef (path-index-entries *path-index*) term#)
        (progn
	  (cl:assert (eql term (hash-term term)))
	  (error "No path-index-entry for term.")))))

(defun some-path-index-entry (term)
  ;; return path-index-entry for term
  ;; return nil if there isn't one
  (let ((term# (funcall *standard-eql-numbering* :lookup term)))
    (or (sparef (path-index-entries *path-index*) term#)
        (progn
	  #+ignore (cl:assert (eql term (hash-term term)))
	  nil))))

(defun path-index-delete (term)
  (let* ((path-index *path-index*)
         (term# (funcall *standard-eql-numbering* :lookup term))
	 (entry (or (sparef (path-index-entries path-index) term#)
		    (progn
		      #+ignore (cl:assert (eql term (hash-term term)))
		      nil))))
    (when entry
      (every (lambda (node)
               (when (path-index-leaf-node-p node)
                 (let ((entries (path-index-leaf-node-entries node)))
                   (setf (sparef entries (tme-number entry)) nil)
                   (when (= 0 (sparse-vector-count entries))
                     (path-index-delete-leaf-node node))))
               t)
             (path-index-entry-in-nodes entry))
      (setf (sparef (path-index-entries path-index) term#) nil)
      (decrement-counter (path-index-entry-counter path-index)))
    entry))

(defun path-index-delete-leaf-node (node)
  (let ((path-index *path-index*)
	(parent (path-index-node-parent-node node)))
    (cond
      ((eq node (path-index-internal-node1-variable-child-node parent))
       (setf (path-index-internal-node1-variable-child-node parent) nil))
      (t
       (let ((table (path-index-internal-node1-constant-indexed-child-nodes parent)))
         (map-sparse-vector-with-indexes
          (lambda (value key)
            (when (eq node value)
              (setf (sparef table key) nil)))
          table))))
    (decrement-counter (path-index-node-counter path-index))))

(defvar *path-index-insert-entry*)
(defvar *path-index-insert-entry-leaf-nodes*)
(defvar *path-index-insert-entry-internal-nodes*)

(defun path-index-insert (term)
  #+ignore (cl:assert (eql term (hash-term term)))
  (let* ((path-index *path-index*)
	 (entry (funcall (path-index-entry-constructor path-index) term)))
    (increment-counter (path-index-entry-counter path-index))
    (let ((term# (funcall *standard-eql-numbering* :lookup term)))
      (setf (sparef (path-index-entries path-index) term#) entry))
    (let ((*path-index-insert-entry* entry)
	  (*path-index-insert-entry-leaf-nodes* nil)
	  (*path-index-insert-entry-internal-nodes* nil))
      ;; FOR EMBEDDINGS
      (when (compound-p term)
	(let ((head (head term)))
	  (when (function-associative head)
	    (setf term (make-compound* head (make-variable) (args term))))))
      (path-index-insert* term (path-index-top-node path-index))
      (let* ((l (nconc *path-index-insert-entry-internal-nodes* *path-index-insert-entry-leaf-nodes*))
             (n (length l)))
        (setf (path-index-entry-in-nodes entry) (make-array n :initial-contents l))
        (setf (path-index-entry-in-nodes-last entry) (- n 1))))
    entry))

(defun path-index-insert* (term node1 &optional head-if-associative)
  ;; find or create paths for term so that term can be inserted in path-index
  (dereference
    term nil
    :if-variable (let ((leaf (path-index-internal-node1-variable-child-node node1)))
		   (unless leaf
		     (setf leaf (make-path-index-leaf-node :parent-node node1))
		     (setf (path-index-internal-node1-variable-child-node node1) leaf))
		   (path-index-insert-at-leaf leaf))
    :if-constant (let ((leaf (path-index-internal-node1-constant-indexed-child-node term node1)))
		   (unless leaf
		     (setf leaf (make-path-index-leaf-node :parent-node node1))
		     (add-path-index-internal-node1-constant-indexed-child-node term node1 leaf))
		   (path-index-insert-at-leaf leaf))
    :if-compound (let ((args (args term)))
		   (if args
		       (path-index-insert-appl (head term) args node1 head-if-associative)
		       (path-index-insert* (function-name (head term)) node1 head-if-associative)))))	;handle 0-ary as constant

(defun path-index-insert-appl (head args node1 head-if-associative)
  (cond
    ((eq head-if-associative head)
     (dolist (arg args)
       (path-index-insert* arg node1 head-if-associative)))
    ((no-integer-indexed-child-nodes-p head)
     (let ((node1a (path-index-internal-node1-function-indexed-child-node head node1)))
       (unless node1a
	 (setf node1a (make-path-index-internal-node1 :parent-node node1))
	 (add-path-index-internal-node1-function-indexed-child-node head node1 node1a))
       (let ((l *path-index-insert-entry-internal-nodes*))
	 (unless (member node1a l)
	   (setf *path-index-insert-entry-internal-nodes* (cons node1a l))))
       (ecase (function-index-type head)
         (:commute					;no integer indexed child nodes => arity=2
          (path-index-insert* (first args) node1a)
          (path-index-insert* (second args) node1a))
         (:jepd
          (path-index-insert* (first args) node1a)
          (path-index-insert* (second args) node1a))
         (:hash-but-dont-index
          (path-index-insert* (function-name head) node1 head-if-associative))	;as if there were no arguments
         ((nil)
          (case (function-arity head)
            (otherwise
             (let ((head-if-associative (and (function-associative head) head)))
               (dolist (arg args)
                 (path-index-insert* arg node1a head-if-associative)))))))))
    (t
     (ecase (function-index-type head)
       ((nil)
        (path-index-insert-list head args node1))
       (:commute
        (path-index-insert-list head args node1 #'c-index))))))

(defun path-index-insert-list (head args node1 &optional indexfun)
  (loop with node2 = (path-index-insert-list1 head (length args) node1 indexfun)
	with iinodes = (path-index-internal-node2-integer-indexed-child-nodes node2)
        for arg in args
        as i from 0
        do (path-index-insert* arg (svref iinodes (if indexfun (funcall indexfun head i) i)))))

(defun path-index-insert-list1 (head arity node1 indexfun)
  (let ((node2 (path-index-internal-node1-function-indexed-child-node head node1)))
    (unless node2
      (let ((iinodes (make-array arity :initial-element nil)))
	(setf node2 (make-path-index-internal-node2 :parent-node node1 :integer-indexed-child-nodes iinodes))
	(dotimes (i arity)
	  (let ((i* (if indexfun (funcall indexfun head i) i)))
	    (unless (svref iinodes i*)
	      (setf (svref iinodes i*) (make-path-index-internal-node1 :parent-node node2)))))
	(loop for i downfrom (- arity 1)
	      as v = (svref iinodes i)
	      do (when v
		   (setf (path-index-internal-node2-query node2) v)
		   (return))))
      (add-path-index-internal-node1-function-indexed-child-node head node1 node2))
    (let ((l *path-index-insert-entry-internal-nodes*)
	  (n (path-index-internal-node2-query node2)))
      (unless (member n l)
	(setf *path-index-insert-entry-internal-nodes* (cons n l))))
    node2))

(defun path-index-insert-at-leaf (leaf)
  (let ((entry *path-index-insert-entry*)
	(entries (path-index-leaf-node-entries leaf)))
    (let ((num (tme-number entry)))
      (unless (sparef entries num)
        (push leaf *path-index-insert-entry-leaf-nodes*)
        (setf (sparef entries num) entry)))))

(defun no-integer-indexed-child-nodes-p (head)
  (ecase (function-index-type head)
    (:commute
     (or (eql 2 (function-arity head)) (eq *=* head)))
    ((:jepd :hash-but-dont-index)
     t)
    ((nil)
     (let ((arity (function-arity head)))
       (or (eql 1 arity)
           (function-associative head)
           (eq :any arity))))))

(defun c-index (head i)
  (declare (ignore head))
  (if (eql 1 i) 0 i))

(defmacro path-index-variable-leaf (node1)
  `(let ((v (path-index-internal-node1-variable-child-node ,node1)))
     (and v
	  (neql 0 (sparse-vector-count (path-index-leaf-node-entries v)))
	  v)))

(defmacro path-index-constant-leaf (node1 const)
  `(let ((v (path-index-internal-node1-constant-indexed-child-node ,const ,node1)))
     (and v
	  (neql 0 (sparse-vector-count (path-index-leaf-node-entries v)))
	  v)))

(defun make-path-index-query (type term &optional subst)
;;(print type) (print-term term subst)
  (let ((query
	  (ecase type
	    (:generalization
	      (make-path-index-query-g term subst (path-index-top-node *path-index*)))
	    (:instance
	      (make-path-index-query-i term subst (path-index-top-node *path-index*)))
	    (:unifiable
	      (make-path-index-query-u term subst (path-index-top-node *path-index*)))
	    (:variant
	      (make-path-index-query-v term subst (path-index-top-node *path-index*))))))
    #+ignore
    (progn
      (terpri-comment-indent)
      (print-term term subst)
      (format t " ~(~A~) query:" type)
      (print-path-index-query query)
      (terpri))
    query))

(defun make-path-index-query-v (term subst node1 &optional head-if-associative)
  (dereference
    term subst
    :if-variable (path-index-variable-leaf node1)
    :if-constant (path-index-constant-leaf node1 term)
    :if-compound (let ((head (head term))
                       (args (args term)))
		   (if (and args (not (eq :hash-but-dont-index (function-index-type head))))
		       (make-path-index-query-appl #'make-path-index-query-v head args subst node1 head-if-associative)
		       (make-path-index-query-v (function-name head) subst node1 head-if-associative)))))	;handle 0-ary as constant

(defun make-path-index-query-i (term subst node1 &optional head-if-associative)
  (dereference
    term subst
    :if-variable t
    :if-constant (path-index-constant-leaf node1 term)
    :if-compound (let ((head (head term))
                       (args (args term)))
		   (if (and args (not (eq :hash-but-dont-index (function-index-type head))))
		       (make-path-index-query-appl #'make-path-index-query-i head args subst node1 head-if-associative)
		       (make-path-index-query-i (function-name head) subst node1 head-if-associative)))))	;handle 0-ary as constant

(defun make-path-index-query-g (term subst node1 &optional head-if-associative)
  (dereference
    term subst
    :if-variable (path-index-variable-leaf node1)
    :if-constant (make-uniond-query2
		   (path-index-constant-leaf node1 term)
		   (path-index-variable-leaf node1))
    :if-compound (let ((head (head term))
                       (args (args term)))
		   (if (and args (not (eq :hash-but-dont-index (function-index-type head))))
		       (make-uniond-query2
			 (make-path-index-query-appl #'make-path-index-query-g head args subst node1 head-if-associative)
			 (path-index-variable-leaf node1))
		       (make-path-index-query-g (function-name head) subst node1 head-if-associative)))))	;handle 0-ary as constant

(defun make-path-index-query-u (term subst node1 &optional head-if-associative)
  (dereference
    term subst
    :if-variable t
    :if-constant (make-uniond-query2
		   (path-index-constant-leaf node1 term)
		   (path-index-variable-leaf node1))
    :if-compound (let ((head (head term))
                       (args (args term)))
		   (if (and args (not (eq :hash-but-dont-index (function-index-type head))))
		       (make-uniond-query2
			 (make-path-index-query-appl #'make-path-index-query-u head args subst node1 head-if-associative)
			 (path-index-variable-leaf node1))
		       (make-path-index-query-u (function-name head) subst node1 head-if-associative)))))	;handle 0-ary as constant

(defun make-path-index-query-appl (make-query head args subst node1 head-if-associative)
  (cond
    ((eq head-if-associative head)
     (let ((v (let ((qq nil) qq-last)
		(dolist (arg args)
		  (let ((q (funcall make-query arg subst node1 head-if-associative)))
		    (cond
		      ((null q)
		       (return-from make-path-index-query-appl nil))
		      ((neq t q)
		       (collect q qq)))))
		(make-boolean-query 'intersection qq))))
       (if (eq t v) node1 v)))
    ((no-integer-indexed-child-nodes-p head)
     (let ((node1a (path-index-internal-node1-function-indexed-child-node head node1)))
       (and node1a
	    (let ((v (let ((qq nil) qq-last)
                       (ecase (function-index-type head)
                         ((nil :commute)
		          (case (function-arity head)
			    (otherwise
			     (let ((head-if-associative (and (function-associative head) head)))
			       (dolist (arg args)
			         (let ((q (funcall make-query arg subst node1a head-if-associative)))
				   (cond
				    ((null q)
				     (return-from make-path-index-query-appl nil))
				    ((neq t q)
				     (collect q qq)))))))))
                         (:jepd
                          (dolist (arg (firstn args 2))
                            (let ((q (funcall make-query arg subst node1a)))
                              (cond
                               ((null q)
                                (return-from make-path-index-query-appl nil))
                               ((neq t q)
                                (collect q qq)))))))
		       (make-boolean-query 'intersection qq))))
	      (if (eq t v) node1a v)))))
    (t
     (ecase (function-index-type head)
       ((nil)
        (make-path-index-query-list make-query head args subst node1))
       (:commute
        (make-path-index-query-list make-query head args subst node1 #'c-index))))))

(defun make-path-index-query-list (make-query head args subst node1 &optional indexfun)
  (let ((node2 (path-index-internal-node1-function-indexed-child-node head node1)))
    (and node2
	 (let ((v (make-boolean-query
                   'intersection
		    (loop with iinodes = (path-index-internal-node2-integer-indexed-child-nodes node2)
			  for arg in args
			  as i from 0
			  as q = (funcall make-query arg subst (svref iinodes (if indexfun (funcall indexfun head i) i)))
			  when (null q)
			    do (return-from make-path-index-query-list nil)
                          unless (eq t q)
			    collect q))))
	   (if (eq t v) (path-index-internal-node2-query node2) v)))))

(defmacro map-leaf0 (leaf x &optional y)
  `(prog->
     (map-sparse-vector (path-index-leaf-node-entries ,leaf) ->* entry)
     (cond
      ((eq query-id (path-index-entry-mark entry))
       )
      ,@(when y (list y))
      ((or (null queries) (path-index-entry-satisfies-query-p entry (first queries) (rest queries)))
       ,x
       (setf (path-index-entry-mark entry) query-id)))))

(defmacro map-leaf (leaf)
  `(if (null test)
       (map-leaf0 ,leaf (funcall cc entry))
       (map-leaf0 ,leaf (funcall cc entry test-value)
                  ((null (setf test-value (funcall test entry)))
                   (setf (path-index-entry-mark entry) query-id)))))

;;; test is a predicate applied to a path-index-entry before path-index
;;; query evaluation is complete to quickly determine whether the
;;; path-index-entry should be retrieved if it satisfies the query
;;; the result of test is also passed as second argument to cc

(defun map-path-index-entries (cc type term &optional subst test query-id)
  (let ((query (make-path-index-query type term subst)))
    (when query
      (map-path-index-by-query cc query test query-id))))

(defun map-path-index-by-query (cc query &optional test query-id)
  (let ((optimized nil))
    (unless query-id
      (setf query-id (cons 'query-id nil)))	;query-id unique, eq testable
    (cond
     ((test-option14?)
      (when (path-index-sparse-vector-expression-p query)
        (setf query (fix-path-index-sparse-vector-expression query))
        (setf query (if (trace-optimize-sparse-vector-expression?)
                        (traced-optimize-sparse-vector-expression query)
                        (optimize-sparse-vector-expression query)))
        (let ((n (test-option21?)))
          (when (and n (consp query) (eq 'intersection (first query)))
            (setf query (firstn query (+ n 1)))))		;keep only first n terms of intersection
        (if test
            (let (test-value)
              (flet ((filter (entry) (setf test-value (funcall test entry))))
                (declare (dynamic-extent #'filter))
                (prog->
                  (map-sparse-vector-expression query :reverse t :filter #'filter ->* entry)
                  (unless (eq query-id (path-index-entry-mark entry))
                    (funcall cc entry test-value)
                    (setf (path-index-entry-mark entry) query-id)))))
            (prog->
              (map-sparse-vector-expression query :reverse t ->* entry)
              (unless (eq query-id (path-index-entry-mark entry))
                (funcall cc entry)
                (setf (path-index-entry-mark entry) query-id))))
        (return-from map-path-index-by-query))))
    (let (test-value)
      (labels
        ((map-path-index-by-query* (query queries)
	   (loop
	     (cond
              ((not (consp query))
               (cond
                ((path-index-leaf-node-p query)
                 (map-leaf query)
                 (return))
                (t
                 (when (path-index-internal-node2-p query)
                   (setf query (path-index-internal-node2-query query)))
                 (map-sparse-vector
                  (lambda (v) (map-leaf v))
                  (path-index-internal-node1-constant-indexed-child-nodes query)
                  :reverse t)
                 (let ((var-leaf (path-index-internal-node1-variable-child-node query)))
                   (when var-leaf
                     (map-leaf var-leaf)))
                 (let ((q nil))
                   (map-sparse-vector
                    (lambda (v)
                      (when q
                        (map-path-index-by-query* q queries))
                      (setf q v))
                    (path-index-internal-node1-function-indexed-child-nodes query)
                    :reverse t)
                   (if q
                       (setf query q)
                       (return))))))
              ((eq 'intersection (first query))
               (dolist (q (prog1 (setf query (rest query))
                            (setf query (if optimized (first query) (select-query query)))))
                 (unless (eq q query)
                   (push q queries))))
              (t
;;             (cl:assert (member (first query) '(union uniond)))
               (do* ((l (rest query) l1)
                     (l1 (rest l) (rest l1)))
                    ((null l1)
                     (setf query (first l)))
                 (map-path-index-by-query* (first l) queries)))))))
        #+ignore (cl:assert query)
        (when (eq t query)
	  (setf query (path-index-top-node *path-index*)))
        (map-path-index-by-query* query nil)))))

(defmacro mark-path-index-entry-in-nodes (entry)
  (cl:assert (symbolp entry))
  (let ((v (gensym)) (i (gensym)))
    `(let ((,v (path-index-entry-in-nodes ,entry))
           (,i (path-index-entry-in-nodes-last ,entry)))
       (declare (type vector ,v) (type fixnum ,i))
       (loop
         (setf (path-index-node-mark (svref ,v ,i)) ,entry)
         (if (eql 0 ,i)
             (return)
             (decf ,i))))))

(defmacro member-path-index-entry-in-nodes (query entry)
  (cl:assert (symbolp query))
  (cl:assert (symbolp entry))
  (let ((v (gensym)) (i (gensym)))
    `(let ((,v (path-index-entry-in-nodes ,entry))
           (,i (path-index-entry-in-nodes-last ,entry)))
       (declare (type vector ,v) (type fixnum ,i))
       (loop
         (when (eq (svref ,v ,i) ,query)
           (return t))
         (if (eql 0 ,i)
             (return nil)
             (decf ,i))))))

(defun path-index-entry-satisfies-query-p (entry query &optional more-queries)
  (cond
    (more-queries
     (mark-path-index-entry-in-nodes entry)
     (and (path-index-entry-satisfies-query-p* entry query)
	  (path-index-entry-satisfies-query-p* entry (first more-queries))
	  (dolist (query (rest more-queries) t)
	    (unless (path-index-entry-satisfies-query-p* entry query)
	      (return nil)))))
    ((consp query)
     (mark-path-index-entry-in-nodes entry)
     (path-index-entry-satisfies-query-p* entry query))
    (t
     (member-path-index-entry-in-nodes query entry))))

(defun path-index-entry-satisfies-query-p* (entry query)
  (loop
    (cond
      ((not (consp query))			;query is a node
       (return-from path-index-entry-satisfies-query-p*
	 (eq (path-index-node-mark query) entry)))
      ((eq 'intersection (first query))		;intersection
       (do* ((l (rest query) l1)
	     (l1 (rest l) (rest l1)))
	    ((null l1)
	     (setf query (first l)))
	 (unless (path-index-entry-satisfies-query-p* entry (first l))
	   (return-from path-index-entry-satisfies-query-p*
	     nil))))
      (t
;;     (cl:assert (member (first query) '(union uniond)))
       (do* ((l (rest query) l1)
	     (l1 (rest l) (rest l1)))
	    ((null l1)
	     (setf query (first l)))
	 (when (path-index-entry-satisfies-query-p* entry (first l))
	   (return-from path-index-entry-satisfies-query-p*
	     t)))))))

(defun retrieval-size (query bound)
  (cond
    ((not (consp query))
     (cond
       ((path-index-leaf-node-p query)
	(sparse-vector-count (path-index-leaf-node-entries query)))
       (t
	(when (path-index-internal-node2-p query)
	  (setf query (path-index-internal-node2-query query)))
	(let ((total-size 0))
	  (let ((var-leaf (path-index-internal-node1-variable-child-node query)))
	    (when var-leaf
	      (incf total-size (sparse-vector-count (path-index-leaf-node-entries var-leaf)))
	      (when (>= total-size bound)
		(return-from retrieval-size bound))))
          (map-sparse-vector
           (lambda (v)
             (incf total-size (sparse-vector-count (path-index-leaf-node-entries v)))
             (when (>= total-size bound)
               (return-from retrieval-size bound)))
           (path-index-internal-node1-constant-indexed-child-nodes query))
          (map-sparse-vector
           (lambda (v)
             (incf total-size (retrieval-size v (- bound total-size)))
             (when (>= total-size bound)
               (return-from retrieval-size bound)))
           (path-index-internal-node1-function-indexed-child-nodes query))
	  total-size))))
    ((eq 'intersection (first query))
     (let* ((args (rest query))
	    (min-size (retrieval-size (first args) bound)))
       (dolist (arg (rest args))
	 (let ((n (retrieval-size arg min-size)))
	   (when (< n min-size)
	     (when (<= (setf min-size n) 1)
	       (return)))))
       min-size))
    (t
;;   (cl:assert (member (first query) '(union uniond)))
     (let ((total-size 0))
       (dolist (arg (rest query))
	 (incf total-size (retrieval-size arg (- bound total-size)))
	 (when (>= total-size bound)
	   (return-from retrieval-size bound)))
       total-size))))

(defun select-query (args)
  (let* ((best (first args))
	 (min-size (retrieval-size best 1000000)))
    (dolist (arg (rest args))
      (let ((n (retrieval-size arg min-size)))
	(when (< n min-size)
	  (setf best arg)
	  (when (<= (setf min-size n) 1)
	    (return)))))
    best))

(defun make-boolean-query* (fn l)
  (let ((a (first l)) 
	(d (rest l)))
    (if (null d)
	(if (and (consp a) (eq fn (first a)))
	    (rest a)
	    l)
	(let ((d* (make-boolean-query* fn d)))
	  (cond
	    ((and (consp a) (eq fn (first a)))
	     (nodup-append (rest a) d*))
	    ((equal a (first d*))
	     d*)
	    ((member a (rest d*) :test #'equal)
	     (cons a (cons (first d*) (remove a (rest d*) :test #'equal))))
	    ((eq d d*)
	     l)
	    (t
	     (cons a d*)))))))

(defun make-boolean-query (fn l)
  (cond
   ((null l)
    (ecase fn
      (intersection
       t)
      ((union uniond)
       nil)))
   (t
    (let ((l* (make-boolean-query* fn l)))
      (cond
       ((null (rest l*))
        (first l*))
       (t
        (cons fn l*)))))))

(defun make-uniond-query2 (q1 q2)
  (cond
   ((null q1)
    q2)
   ((null q2)
    q1)
   (t
    (make-boolean-query 'uniond (list q1 q2)))))

(defun nodup-append (l1 l2 &optional (l2* nil))
  ;; append l1 and l2 eliminating items in l2 that appear in l1
  (if (null l2)
      (if (null l2*)
	  l1
	  (append l1 (nreverse l2*)))
      (nodup-append l1
		    (rest l2)
		    (if (member (first l2) l1 :test #'equal)
			l2*
			(cons (first l2) l2*)))))

(defun path-index-sparse-vector-expression-p (x)
  (cond
   ((atom x)
    (when (path-index-leaf-node-p x)
      (setf x (path-index-leaf-node-entries x)))
    (and (sparse-vector-p x) (null (sparse-vector-default-value x))))
   (t
    (let ((fn (first x))
          (args (rest x)))
      (and (or (eq 'intersection fn) (eq 'union fn) (eq 'uniond fn))
           args
           (dolist (arg args t)
             (unless (path-index-sparse-vector-expression-p arg)
               (return nil))))))))

(defun fix-path-index-sparse-vector-expression (x)
  (cond
   ((atom x)
    (if (path-index-leaf-node-p x)
        (path-index-leaf-node-entries x)
        x))
   (t
    (dotails (l (rest x))
      (setf (first l) (fix-path-index-sparse-vector-expression (first l))))
    x)))

(defun sparse-vector-expression-description (expr)
  (cond
   ((atom expr)
    (sparse-vector-count expr))
   (t
    (cons (ecase (first expr) (intersection '&) (union 'u) (uniond 'v))
          (mapcar #'sparse-vector-expression-description (rest expr))))))

(defun sz (x)
  (if (atom x) 0 (+ (sz (car x)) (sz (cdr x)) 1)))

(defun traced-optimize-sparse-vector-expression (expr)
  (let* ((desc (sparse-vector-expression-description expr))
         (expr* (optimize-sparse-vector-expression expr))
         (desc* (sparse-vector-expression-description expr*)))
    (format t "~%~A" desc*)
    (unless (eql (sz desc) (sz desc*))
      (format t " optimized from ~A" desc))
    expr*))

(defun print-path-index (&key terms nodes)
  (let ((index *path-index*))
    (mvlet (((:values current peak added deleted) (counter-values (path-index-entry-counter index))))
      (format t "~%; Path-index has ~:D entr~:@P (~:D at peak, ~:D added, ~:D deleted)." current peak added deleted))
    (mvlet (((:values current peak added deleted) (counter-values (path-index-node-counter index))))
      (format t "~%; Path-index has ~:D node~:P (~:D at peak, ~:D added, ~:D deleted)." current peak added deleted))
    (when (or nodes terms)
      (print-index* (path-index-top-node index) nil terms))))

(defmethod print-index-leaf-node ((node path-index-leaf-node) revpath print-terms)
  (with-standard-io-syntax2
    (prog->
      (format t "~%; Path ")
      (print-revpath revpath)
      (path-index-leaf-node-entries node -> entries)
      (format t " has ~:D entr~:@P." (sparse-vector-count entries))
      (when print-terms
	(map-sparse-vector entries ->* entry)
	(format t "~%;    ")
	(print-term (index-entry-term entry))))))

(defmethod map-index-leaf-nodes (cc (node path-index-internal-node1) revpath)
  (let ((v (path-index-internal-node1-variable-child-node node)))
    (when v
      (map-index-leaf-nodes cc v (cons "variable" revpath))))
  (map-sparse-vector-with-indexes
   (lambda (v k)
     (map-index-leaf-nodes cc v (cons (symbol-numbered k) revpath)))
   (path-index-internal-node1-constant-indexed-child-nodes node)
   :reverse t)
  (map-sparse-vector-with-indexes
   (lambda (v k)
     (map-index-leaf-nodes cc v (cons (symbol-numbered k) revpath)))
   (path-index-internal-node1-function-indexed-child-nodes node)
   :reverse t))

(defmethod map-index-leaf-nodes (cc (node path-index-internal-node2) revpath)
  (let ((iinodes (path-index-internal-node2-integer-indexed-child-nodes node)))
    (dotimes (i (array-dimension iinodes 0))
      (let ((v (svref iinodes i)))
        (when v
          (map-index-leaf-nodes cc v (cons i revpath)))))))

(defmethod map-index-leaf-nodes (cc (node path-index-leaf-node) revpath)
  (funcall cc node revpath))

(defun print-revpath (revpath)
  (princ "[")
  (dolist (x (reverse (rest revpath)))
    (cond
      ((function-symbol-p x)
       (prin1 x))
      (t
       (cl:assert (integerp x))
       (cond
	 ((< x 0)
	  (princ "list")
	  (princ (- x)))
	 (t
	  (princ "arg")
	  (princ (+ x 1))))))
    (princ ","))
  (prin1 (first revpath) *standard-output*)
  (princ "]"))

(defun path-index-key-for-value (value table)
  (map-sparse-vector-with-indexes
   (lambda (v k)
     (when (eq value v)
       (return-from path-index-key-for-value (symbol-numbered k))))
   table))

(defun path-index-node-revpath (node)
  (let ((parent-node (path-index-node-parent-node node)))
    (cond
      ((path-index-internal-node1-p parent-node)
       (cons (or (if (eq node (path-index-internal-node1-variable-child-node parent-node)) "variable" nil)
		 (path-index-key-for-value node (path-index-internal-node1-function-indexed-child-nodes parent-node))
		 (path-index-key-for-value node (path-index-internal-node1-constant-indexed-child-nodes parent-node)))
	     (path-index-node-revpath parent-node)))
      ((path-index-internal-node2-p parent-node)
       (cons (position node (path-index-internal-node2-integer-indexed-child-nodes parent-node))
	     (path-index-node-revpath parent-node)))
      (t
       nil))))

(defun print-path-index-query (query &key terms)
  (cond
    ((or (null query) (eq t query))
     (terpri-comment-indent)
     (princ query))
    ((and (consp query) (eq 'intersection (first query)))
     (terpri-comment-indent)
     (princ "(intersection")
     (let ((*terpri-indent* (+ *terpri-indent* 3)))
       (mapc (lambda (q) (print-path-index-query q :terms terms)) (rest query)))
     (princ ")"))
    ((and (consp query) (eq 'union (first query)))
     (terpri-comment-indent)
     (princ "(union")
     (let ((*terpri-indent* (+ *terpri-indent* 3)))
       (mapc (lambda (q) (print-path-index-query q :terms terms)) (rest query)))
     (princ ")"))
    ((and (consp query) (eq 'uniond (first query)))
     (terpri-comment-indent)
     (princ "(uniond")
     (let ((*terpri-indent* (+ *terpri-indent* 3)))
       (mapc (lambda (q) (print-path-index-query q :terms terms)) (rest query)))
     (princ ")"))
    ((path-index-leaf-node-p query)
     (print-index* query (path-index-node-revpath query) terms))
    (t
     (terpri-comment-indent)
     (let ((revpath (path-index-node-revpath query)))
       (princ "(all-entries ")
       (print-revpath (cons "..." revpath))
;;     (let ((*terpri-indent* (+ *terpri-indent* 3)))
;;	 (print-path-index* query revpath terms))
       (princ ")"))))
  nil)

;;; path-index.lisp EOF
