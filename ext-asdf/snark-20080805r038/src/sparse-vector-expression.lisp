;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark-sparse-array -*-
;;; File: sparse-vector-expression.lisp
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
;;; Portions created by the Initial Developer are Copyright (C) 1981-2005.
;;; All Rights Reserved.
;;;
;;; Contributor(s): Mark E. Stickel <stickel@ai.sri.com>.

(in-package :snark-sparse-array)

;;; compute intersection and union of sparse-vectors
;;; <sparse-vector-expression> ::=
;;;   <sparse-vector> |
;;;   (intersection <sparse-vector-expression>+) |
;;;   (union <sparse-vector-expression>+) |
;;;   (uniond <sparse-vector-expression>+)
;;; assumes that default-value for sparse-vectors is nil
;;; elements of unions are not mapped in order

(defun sparse-vector-expression-p (x)
  (cond
   ((atom x)
    (and (sparse-vector-p x) (null (sparse-vector-default-value x))))
   (t
    (let ((fn (first x))
          (args (rest x)))
      (and (or (eq 'intersection fn) (eq 'union fn) (eq 'uniond fn))
           args
           (dolist (arg args t)
             (unless (sparse-vector-expression-p arg)
               (return nil))))))))

(definline mem-sparse-vector-expression (index expr)
  (if (atom expr) (sparef expr index) (mem-sparse-vector-expression1 index expr)))

(defun mem-sparse-vector-expression1 (index expr)
  (declare (type cons expr))
  (cond
   ((eq 'intersection (first expr))
    (dolist (e (rest expr) t)
      (unless (mem-sparse-vector-expression index e)
        (return nil))))
   (t							;union, uniond
    (dolist (e (rest expr) nil)
      (when (mem-sparse-vector-expression index e)
        (return t))))))

;;; (intersection sve1 sve2 ... sven) is mapped by generating elements of
;;; sve1 and testing them for membership in sve2 ... sven
;;;
;;; (union sve1 sve2 ... sven) is mapped by generating elements of each svei
;;; and testing them for membership in sve1 ... svei-1 to omit duplicates
;;;
;;; (uniond sve1 sve2 ... sven) is mapped by generating elements of each svei;
;;; either the union of sets is assumed to be disjoint or we don't care about duplicates,
;;; so there is no duplicate elimination during mapping for uniond

(defmacro map-sparse-vector-expression-macro (mapexp2 mapexp funcallexp)
  `(cond
    ((atom expr)
     ,mapexp2)
    (t
     (ecase (pop expr)
       (intersection
        (prog->
          (first expr -> e1)
          (rest expr -> l2)
          (if l2 (cons 'intersection l2) nil -> exprest)
          (if exprest (sparse-vector-expression-index-bounds exprest) nil -> min max)
          (when (implies exprest (and (<= min max)
                                      (prog->
                                        (sparse-vector-expression-index-bounds e1 -> min1 max1)
                                        (and (<= min1 max1) (<= min max1) (>= max min1)))))
            (if exprest (sparse-vector-expression-generates-in-order-p e1) nil -> inorder)
            ,mapexp
            ;; avoid membership tests if index k is out of range
            ;; return quickly if generating indexes in order and beyond range
            (when (implies exprest (if reverse
                                       (and (>= max k) (or (<= min k) (if inorder (return-from prog->) nil)))
                                       (and (<= min k) (or (>= max k) (if inorder (return-from prog->) nil)))))
              (dolist l2 ,funcallexp ->* e2)
              (unless (mem-sparse-vector-expression k e2)
                (return))))))
       (uniond
        (prog->
          (dolist expr ->* e1)
          ,mapexp
          (declare (ignorable k))
          ,funcallexp))
       (union
        (prog->
          (dolist expr ->* e1)
          ,mapexp
          (dolist expr ->* e2)
          (cond
           ((eq e1 e2)
            ,funcallexp
            (return))
           ((mem-sparse-vector-expression k e2)
            (return)))))))))

;;; if it is provided, the predicate 'filter' is applied to elements immediately
;;; when mapped (e.g., before checking membership in rest of intersection)
;;; in order to ignore unwanted elements quickly

(defun map-sparse-vector-expression-with-indexes0 (function expr reverse filter)
  (map-sparse-vector-expression-macro
   (if (null filter)
       (map-sparse-vector-with-indexes function expr :reverse reverse)
       (prog->
         (map-sparse-vector-with-indexes expr :reverse reverse ->* v k)
         (when (funcall filter v k)
           (funcall function v k))))
   (map-sparse-vector-expression-with-indexes0 e1 reverse filter ->* v k)
   (funcall function v k)))

(defun map-sparse-vector-expression-indexes-only0 (function expr reverse filter)
  (map-sparse-vector-expression-macro
   (if (null filter)
       (map-sparse-vector-indexes-only function expr :reverse reverse)
       (prog->
         (map-sparse-vector-indexes-only expr :reverse reverse ->* k)
         (when (funcall filter k)
           (funcall function k))))
   (map-sparse-vector-expression-indexes-only0 e1 reverse filter ->* k)
   (funcall function k)))

(defun map-sparse-vector-expression0 (function expr reverse filter)
  (map-sparse-vector-expression-macro
   (if (null filter)
       (map-sparse-vector function expr :reverse reverse)
       (prog->
         (map-sparse-vector expr :reverse reverse ->* v)
         (when (funcall filter v)
           (funcall function v))))
   (map-sparse-vector-expression-values2 e1 reverse filter ->* v k)
   (funcall function v)))

(defun map-sparse-vector-expression-values2 (function expr reverse filter)
  (map-sparse-vector-expression-macro
   (if (null filter)
       (map-sparse-vector-with-indexes function expr :reverse reverse)
       (prog->
         (map-sparse-vector-with-indexes expr :reverse reverse ->* v k)
         (when (funcall filter v)
           (funcall function v k))))
   (map-sparse-vector-expression-values2 e1 reverse filter ->* v k)
   (funcall function v k)))

(definline map-sparse-vector-expression (function expr &key reverse filter)
  (map-sparse-vector-expression0 function expr reverse filter))

(definline map-sparse-vector-expression-with-indexes (function expr &key reverse filter)
  (map-sparse-vector-expression-with-indexes0 function expr reverse filter))

(definline map-sparse-vector-expression-indexes-only (function expr &key reverse filter)
  (map-sparse-vector-expression-indexes-only0 function expr reverse filter))

(defun sparse-vector-expression-size (expr)
  ;; number of sparse-vectors in expression
  (cond
   ((atom expr)
    1)
   (t
    (setf expr (rest expr))
    (let ((size (sparse-vector-expression-size (first expr))))
      (dolist (e (rest expr) size)
        (incf size (sparse-vector-expression-size e)))))))

(defun sparse-vector-expression-maxcount (expr)
  ;; upper bound on count for expression
  (cond
   ((atom expr)
    (sparse-vector-count expr))
   ((eq 'intersection (pop expr))
    (let ((count (sparse-vector-expression-maxcount (first expr))))
      (dolist (e (rest expr) count)
        (let ((n (sparse-vector-expression-maxcount e)))
          (when (< n count)
            (setf count n))))))
   (t							;union, uniond
    (let ((count (sparse-vector-expression-maxcount (first expr))))
      (dolist (e (rest expr) count)
        (incf count (sparse-vector-expression-maxcount e)))))))

(defun optimized-sparse-vector-expression-maxcount (expr)
  ;; upper bound on count for expression
  ;; assumes that intersections are ordered in ascending value
  (cond
   ((atom expr)
    (sparse-vector-count expr))
   ((eq 'intersection (pop expr))
    (optimized-sparse-vector-expression-maxcount (first expr)))
   (t							;union, uniond
    (let ((count (optimized-sparse-vector-expression-maxcount (first expr))))
      (dolist (e (rest expr) count)
        (incf count (optimized-sparse-vector-expression-maxcount e)))))))

(defun sparse-vector-expression-index-bounds (expr)
  ;; returns smallest and largest indexes that might be expr
  (cond
   ((atom expr)
    (values (nth-value 1 (first-sparef expr)) (nth-value 1 (last-sparef expr))))
   ((eq 'intersection (pop expr))
    (prog->
      (sparse-vector-expression-index-bounds (first expr) -> min max)
      (dolist (rest expr) (values min max) ->* e)
      (sparse-vector-expression-index-bounds e -> m n)
      ;; narrow bounds of intersections
      (when (< min m)
        (setf min m))
      (when (> max n)
        (setf max n))))
   (t							;union, uniond
    (prog->
      (sparse-vector-expression-index-bounds (first expr) -> min max)
      (dolist (rest expr) (values min max) ->* e)
      (sparse-vector-expression-index-bounds e -> m n)
      ;; widen bounds of unions
      (when (> min m)
        (setf min m))
      (when (< max n)
        (setf max n)))))) 

(defun sparse-vector-expression-generates-in-order-p (expr)
  (or (atom expr)
      (and (eq 'intersection (first expr))
           (sparse-vector-expression-generates-in-order-p (second expr)))))   

(defun equal-sparse-vector-expression-p (x y)
  (or (eq x y)
      (and (consp x)
           (consp y)
           (eq (pop x) (pop y))
           (subsetp x y :test #'equal-sparse-vector-expression-p)
           (subsetp y x :test #'equal-sparse-vector-expression-p))))

(defun equal-optimized-sparse-vector-expression-p (x y)
  (or (eq x y)
      (and (consp x)
           (consp y)
           (eq (pop x) (pop y))
           (length= x y)
           (subsetp x y :test #'equal-optimized-sparse-vector-expression-p))))

(definline optimize-sparse-vector-expression (expr)
  (cond
   ((atom expr)
    expr)
   ((eq 'intersection (first expr))
    (optimize-sparse-vector-expression1 expr #'<))	;intersection ordered by increasing maxcount
   (t
    (optimize-sparse-vector-expression1 expr #'>))))	;union, uniond ordered by decreasing maxcount

(definline optimize-and-sort-short-lists-of-sparse-vector-expressions (l1 predicate)
  ;; returns t and destructively stably sorts l1 if length is <= 3, returns nil otherwise
  (if (null l1)
      t
      (let ((l2 (rest l1)))
        (if (null l2)
            t
            (let ((l3 (rest l2)))
              (if (null l3)
                  (let* ((v1 (optimize-sparse-vector-expression (first l1)))
                         (v2 (optimize-sparse-vector-expression (first l2)))
                         (n1 (optimized-sparse-vector-expression-maxcount v1))
                         (n2 (optimized-sparse-vector-expression-maxcount v2)))
                    (cond
                     ((funcall predicate n2 n1)
                      (setf (first l1) v2 (first l2) v1)))
                    t)
                  (if (null (rest l3))
                      (let* ((v1 (optimize-sparse-vector-expression (first l1)))
                             (v2 (optimize-sparse-vector-expression (first l2)))
                             (v3 (optimize-sparse-vector-expression (first l3)))
                             (n1 (optimized-sparse-vector-expression-maxcount v1))
                             (n2 (optimized-sparse-vector-expression-maxcount v2))
                             (n3 (optimized-sparse-vector-expression-maxcount v3)))
                        (cond
                         ((funcall predicate n2 n1)
                          (cond
                           ((funcall predicate n3 n2)
                            (setf (first l1) v3 (first l2) v2 (first l3) v1))
                           ((funcall predicate n3 n1)
                            (setf (first l1) v2 (first l2) v3 (first l3) v1))
                           (t
                            (setf (first l1) v2 (first l2) v1))))
                         ((funcall predicate n3 n2)
                          (cond
                           ((funcall predicate n3 n1)
                            (setf (first l1) v3 (first l2) v1 (first l3) v2))
                           (t
                            (setf (first l2) v3 (first l3) v2)))))
                        t)
                      nil)))))))

(defun optimize-sparse-vector-expression1 (expr predicate)
  ;; destructive
  (let ((fn (first expr))
        (args (rest expr)))
;;  (cl:assert args)
    (cond
     ((null (rest args))
      (optimize-sparse-vector-expression (first args)))
     (t
      ;; optimize and sort arguments
      (or (optimize-and-sort-short-lists-of-sparse-vector-expressions args predicate)
          (progn
            (dotails (l args)
              (let ((x (optimize-sparse-vector-expression (car l))))
                (setf (car l) (cons (optimized-sparse-vector-expression-maxcount x) x))))
            (setf args (stable-sort args predicate :key #'car))
            (dotails (l args)
              (setf (car l) (cdar l)))))
      ;; eliminate duplicate arguments
      (setf args (delete-duplicates args :test #'equal-optimized-sparse-vector-expression-p :from-end t))
      ;; apply absorption laws
      ;; (union a (intersection a b) c) -> (union a c)
      ;; (intersection a (union a b) c) -> (intersection a c)
      (setf args (delete-if (lambda (arg)
                                (and (consp arg)
                                     (not (iff (eq 'intersection fn) (eq 'intersection (first arg))))
                                     (dolist (x args)
                                       (cond
                                        ((eq arg x)
                                         (return nil))
                                        ((member x (rest arg) :test #'equal-optimized-sparse-vector-expression-p)
                                         (return t))))))
                            args))
      (if (null (rest args)) (first args) (rplacd expr args))))))

;;; sparse-vector-expression.lisp EOF
