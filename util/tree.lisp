(in-package :cl-user)

(defun tree-walk (tree fn)
  (funcall fn tree)
  (when (consp tree)
    (map nil (lambda(el) (tree-walk el fn)) tree)))

;; Call function on tree. If function returns t recurse, otherwise stop.
(defun tree-walk-conditional (tree fn)
  (if (funcall fn tree)
      (when (consp tree)
	(map nil (lambda(el) (tree-walk-conditional el fn)) tree))))

(defun tree-find (sym tree &key (test #'eq))
  (cond ((atom tree)
	 (if (funcall test sym tree) tree))
	(t (some (lambda(el) (tree-find sym el :test test)) tree))))

(defun tree-find-if (tree fn)
  (tree-walk tree (lambda(e) (when (funcall fn e) (return-from tree-find-if e)))))

(defun f (tree fn)
  (cond ((atom tree)
	 (funcall fn tree))
	(t (mapcar (lambda(el) (f el fn)) tree))))

(defun tree-replace (replace-fn tree)
  "create new tree replacing each element with the result of calling replace-fn on it"
  (labels ((tr-internal (tree)
	     (cond ((atom tree) (funcall replace-fn tree))
		   (t (let ((replacement (funcall replace-fn tree)))
			(if (eq replacement tree)
			    (mapcar #'tr-internal tree)
			    replacement))))))
    (tr-internal tree)))

;; Like nsubst - replacing atoms in the tree without disturbing the list structure.
(defun tree-nsubst-if (replace-fn test-fn tree)
  "destructively modify tree replacing each atom that satisfies test-fn  with the result of calling replace-fn on it"
  (labels ((tr-internal (tree)
	     (cond ((and (atom tree) (funcall test-fn tree)) (funcall replace-fn tree))
		   ((atom tree) tree)
		   ((and (atom (cdr tree)) (not (null (cdr tree))))
		    (when (and (atom (car tree)) (funcall test-fn (car tree)))
		      (rplaca tree (funcall replace-fn (car tree))))
		    (when (funcall test-fn (cdr tree))
		      (rplacd tree (funcall replace-fn (cdr tree))))
		    tree)
		   (t (loop for (this . rest) on tree
			    for pos from 0
			    do (setf (nth pos tree) (tr-internal this))
			    when (and rest (atom rest) (funcall test-fn rest))
			      do (setf (cdr (last tree)) (funcall replace-fn rest))
				 (return tree)
			    )
		      tree))))
    (tr-internal tree)
    tree))


(defun tree-remove-if (test tree)
  "create new tree without any expressions that match test"
  (cond ((atom tree) tree)
        (t (let ((tree (remove-if test tree)))
	     (let ((car (tree-remove-if test (car tree)))
		   (cdr (tree-remove-if test (cdr tree))))
	       (if (and (eq car (car tree))
			(eq cdr (cdr tree)))
		   tree
		   (cons car cdr)))))))
