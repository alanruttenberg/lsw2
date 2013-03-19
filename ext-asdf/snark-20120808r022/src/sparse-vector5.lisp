;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark-sparse-array -*-
;;; File: sparse-vector5.lisp
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

(in-package :snark-sparse-array)

;;; ****if* snark-sparse-array/sparse-vector-types
;;; SOURCE

(deftype sparse-vector-index () 'integer)	;indexes are integers
(deftype sparse-vector-count () 'fixnum)	;number of entries is a fixnum
;;; ***

;;; more implementation independent sparse-vector functions are defined in sparse-array.lisp

;;; ****s* snark-sparse-array/sparse-vector
;;; NAME
;;;   sparse-vector structure
;;;   sparse-vector type
;;; SOURCE

(defstruct (sparse-vector
            (:constructor make-sparse-vector0 (default-value0))
            (:print-function print-sparse-vector3)
            (:copier nil))
  (default-value0 nil :read-only t)			;default value, or 'bool (unexported symbol denotes boolean sparse-vector)
  (type nil)
  (count0 0 :type sparse-vector-count)
  (cached-key 0 :type sparse-vector-index)
  cached-value						;initialize in make-sparse-vector
  (b-tree-root-node nil))
;;; ***

;;; ****f* snark-sparse-array/make-sparse-vector
;;; USAGE
;;;   (make-sparse-vector &key boolean default-value)
;;; RETURN VALUE
;;;   sparse-vector
;;; SOURCE

(defun make-sparse-vector (&key boolean default-value)
  (when boolean
    (unless (null default-value)
      (error "Default-value must be NIL for Boolean sparse-arrays.")))
  (let ((sparse-vector (make-sparse-vector0 (if boolean 'bool default-value))))
    (setf (sparse-vector-cached-value sparse-vector) default-value)
    sparse-vector))
;;; ***

;;; ****f* snark-sparse-array/sparse-vector-p
;;; USAGE
;;;   (sparse-vector-p x)
;;; RETURN VALUE
;;;   true if x if a sparse-vector, false otherwise
;;; SOURCE

      ;;sparse-vector-p is defined by the sparse-vector defstruct
;;; ***

;;; ****f* snark-sparse-array/sparse-vector-boolean
;;; USAGE
;;;   (sparse-vector-boolean sparse-vector)
;;; RETURN VALUE
;;;   true if x is a boolean sparse-vector, false otherwise
;;; SOURCE

(definline sparse-vector-boolean (sparse-vector)
  (eq 'bool (sparse-vector-default-value0 sparse-vector)))
;;; ***

;;; ****f* snark-sparse-array/sparse-vector-default-value
;;; USAGE
;;;   (sparse-vector-boolean sparse-vector)
;;; RETURN VALUE
;;;   the default-value for unstored entries of sparse-vector
;;; SOURCE

(definline sparse-vector-default-value (sparse-vector)
  (let ((v (sparse-vector-default-value0 sparse-vector)))
    (if (eq 'bool v) nil v)))
;;; ***

;;; ****f* snark-sparse-array/sparse-vector-count
;;; USAGE
;;;   (sparse-vector-count sparse-vector)
;;; RETURN VALUE
;;;   integer number of entries in sparse-vector
;;; NOTES
;;;   returns 0 if sparse-vector is nil
;;; SOURCE

(definline sparse-vector-count (sparse-vector)
  (if (null sparse-vector) 0 (sparse-vector-count0 sparse-vector)))
;;; ***

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant b-tree-node-size 16)			;must be even
  (defconstant b-tree-node-size-1 (- b-tree-node-size 1))
  (defconstant b-tree-node-size/2 (floor b-tree-node-size 2))
  (defconstant b-tree-node-size/2+1 (+ b-tree-node-size/2 1))
  (defconstant b-tree-node-size/2-1 (- b-tree-node-size/2 1)))

#+ignore
(defstruct (b-tree-node
            (:constructor make-b-tree-node (alist nonleaf-last-value))
            )
  ;; b-tree nodes must be nonempty
  ;; leaf nodes have at least one key and the same number of values
  ;; nonleaf nodes have at one key and one more value
  (alist nil :read-only t)				;alist of keys and values (or just list of keys for leaf nodes of boolean sparse vectors)
  (nonleaf-last-value nil :read-only t))		;nonleaf nodes have one more value than keys, nil for leaf nodes

(defmacro make-b-tree-node (alist nonleaf-last-value)
  `(cons ,alist ,nonleaf-last-value))

(defmacro b-tree-node-alist (n)
  `(carc ,n))

(defmacro b-tree-node-nonleaf-last-value (n)
  `(cdrc ,n))

(definline b-tree-nonleaf-node-alist-search (alist index)
  ;; each node has one or more keys in descending order
  (declare (type sparse-vector-index index))
  (loop
    (when (or (>= index (the sparse-vector-index (carc (carc alist)))) (null (setf alist (cdrc alist))))
      (return alist))))

(definline lastc (list)
  (let (rest)
    (loop
      (if (null (setf rest (cdrc list)))
          (return (carc list))
          (setf list rest)))))

(definline smallest-key (x)
  (let ((p (lastc x)))
    (if (atom p) p (carc p))))

(definline largest-key (x)
  (let ((p (carc x)))
    (if (atom p) p (carc p))))

(definline b-tree-node-smallest-key* (n)
  (loop
    (let ((last-value (b-tree-node-nonleaf-last-value n)))
      (cond
       ((null last-value)
        ;; leaf node
        (let ((v (lastc (b-tree-node-alist n))))
          (if (atom v)	;boolean sparse vector?
              (return (values v v))
              (return (values (carc v) (cdrc v))))))
       (t
        (setf n last-value))))))

(definline b-tree-node-largest-key* (n)
  (loop
    (let ((last-value (b-tree-node-nonleaf-last-value n)))
      (cond
       ((null last-value)
        ;; leaf node
        (let ((v (carc (b-tree-node-alist n))))
          (if (atom v)	;boolean sparse vector?
              (return (values v v))
              (return (values (carc v) (cdrc v))))))
       (t
        (setf n (cdrc (carc (b-tree-node-alist n)))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun nestn (x y n)
    (dotimes (i n)
      (setf y (subst y '*** x)))
    y))

(defmacro unroll-sparef1-leaf ()
  `(let ((p (carc alist)))
     (if (atom p)
         ;; boolean sparse-vector leaf node, alist is nonempty list of indexes in descending order
         ,(let ((l nil))
            (dotimes (i b-tree-node-size)
              (cond
               ((= 0 i)
                (push `((or (null (setf alist (cdrc alist))) (>= index k)) (= index k)) l))
               ((> b-tree-node-size-1 i)
                (push `((progn (setf k (carc alist)) (or (null (setf alist (cdrc alist))) (>= index k))) (= index k)) l))
               (t
                (push `(t (= index (the sparse-vector-index (carc alist)))) l))))
            `(let ((k p))
               (declare (type sparse-vector-index k))
               (if (cond ,@(reverse l)) index nil)))
         ;; nonboolean sparse-vector leaf node, alist is nonempty alist of keys (in descending order) and values
         ,(let ((l nil))
            (dotimes (i b-tree-node-size)
              (cond
               ((= 0 i)
                (push `((or (null (setf alist (cdrc alist))) (>= index k)) (= index k)) l))
               ((> b-tree-node-size-1 i)
                (push `((progn (setf k (carc (setf p (carc alist)))) (or (null (setf alist (cdrc alist))) (>= index k))) (= index k)) l))
               (t
                (push `(t (= index (the sparse-vector-index (carc (setf p (carc alist)))))) l))))
            `(let ((k (carc p)))
               (declare (type sparse-vector-index k))
               (if (cond ,@(reverse l)) (cdrc p) (sparse-vector-default-value sparse-vector)))))))

(defmacro unroll-sparef1-nonleaf ()
  ;; nonleaf node, alist is nonempty alist of keys (in descending order) and values
  (let ((l nil))
    (dotimes (i b-tree-node-size)
      (cond
       ((= 0 i)
        (push `((>= index (the sparse-vector-index (carc p))) (cdrc p)) l))
       (t
        (push `((null (setf alist (cdrc alist))) nil) l)
        (push `((>= index (the sparse-vector-index (carc (setf p (carc alist))))) (cdrc p)) l))))
    `(let* ((p (carc alist)))
       (cond ,@(reverse l)))))

(defmacro unroll-full-alist ()
  (let ((l nil))
    (dotimes (i b-tree-node-size-1)
      (push `(setf l (cdrc l)) l))
    `(and ,@l)))

(definline full-alist (l)
  (unroll-full-alist))

;;; ****if* snark-sparse-array/sparef1
;;; USAGE
;;;   (sparef1 sparse-vector index)
;;; NOTES
;;;   (sparef sparse-vector index) macroexpands to this
;;; SOURCE

(defun sparef1 (sparse-vector index)
  (declare (type sparse-vector sparse-vector) (type sparse-vector-index index))
  (let ((n (sparse-vector-b-tree-root-node sparse-vector)))
    (cond
     ((null n)
      (sparse-vector-default-value sparse-vector))
     ((= (sparse-vector-cached-key sparse-vector) index)
      (sparse-vector-cached-value sparse-vector))
     (t
      (loop
        (let ((alist (b-tree-node-alist n))
              (last-value (b-tree-node-nonleaf-last-value n)))
          (cond
           ((null last-value)
            ;; leaf node
            (setf (sparse-vector-cached-key sparse-vector) index)
            (return (setf (sparse-vector-cached-value sparse-vector) (unroll-sparef1-leaf))))
           (t
            (setf n (or (unroll-sparef1-nonleaf) last-value))))))))))
;;; ***

;;; ****f* snark-sparse-array/sparef
;;; USAGE
;;;   (sparef sparse-vector index)
;;;   (setf (sparef sparse-vector index) value)
;;;
;;;   (sparef sparse-matrix row-index column-index)
;;;   (setf (sparef sparse-matrix row-index column-index) value)
;;; SOURCE

(defmacro sparef (sparse-array index1 &optional index2)
  (if (null index2)
      `(sparef1 ,sparse-array ,index1)
      `(sparef2 ,sparse-array ,index1 ,index2)))
;;; ***

;;; ****if* snark-sparse-array/sparse-vector-setter
;;; USAGE
;;;   (sparse-vector-setter value sparse-vector index)
;;; SOURCE

(defun sparse-vector-setter (value sparse-vector index &optional copy)
  ;; sparse-vector-setter destructively modifies slots of sparse-vector
  ;; it will make a copy of sparse-vector and modify it instead if copy is true
  ;; this is used by spacons that returns a new sparse-vector and leaves the original unmodified
  ;; the b-tree structure nodes themselves are not destructively modified
  ;; so that map-sparse-vector traversals are unaltered by
  ;; additions, deletions, and modifications done during the traversal
  (declare (type sparse-vector sparse-vector) (type sparse-vector-index index))
  (when (and (= (sparse-vector-cached-key sparse-vector) index)
             (if (sparse-vector-boolean sparse-vector)
                 (iff (sparse-vector-cached-value sparse-vector) value)
                 (eql (sparse-vector-cached-value sparse-vector) value)))
    (return-from sparse-vector-setter (if copy sparse-vector value)))
  (let ((n (sparse-vector-b-tree-root-node sparse-vector)))
    (cond
     ((null n)
      ;; sparse-vector is empty
      (unless (eql (sparse-vector-default-value sparse-vector) value)
        ;; add single element
        (when copy
          (setf sparse-vector (copy-sparse-vector sparse-vector)))
        (setf (sparse-vector-count0 sparse-vector) 1)
        (setf (sparse-vector-b-tree-root-node sparse-vector) (make-b-tree-node (if (sparse-vector-boolean sparse-vector) (list index) (list (cons index value))) nil))))
     (t
      (labels
        ((split-leaf-alist (list num)
           (declare (type fixnum num))
           (let (rest)
             (labels
               ((spl ()
                  (cond
                   ((= 0 num)
                    (setf rest list)
                    nil)
                   (t
                    (cons (carc list) (progn (setf list (cdrc list)) (setf num (- num 1)) (spl)))))))
               (values (spl) rest))))
         (split-nonleaf-alist (list num)
           (declare (type fixnum num))
           (let (k v rest)
             (labels
               ((spl ()
                  (cond
                   ((= 0 num)
                    (let ((p (carc list)))
                      (setf k (carc p))
                      (setf v (cdrc p))
                      (setf rest (cdrc list)))
                    nil)
                   (t
                    (cons (carc list) (progn (setf list (cdrc list)) (setf num (- num 1)) (spl)))))))
               (values (spl) k v rest))))
         (list-update (list index value)
           (declare (type sparse-vector-index index))
           (let ((diff 0))
             (labels
               ((update (list)
                  (cond
                   ((null list)
                    (cond
                     ((null value)
                      nil)
                     (t
                      (setf diff +1)
                      (cons index nil))))
                   (t
                    (let ((k (carc list)))
                      (declare (type sparse-vector-index k))
                      (cond
                       ((>= index k)
                        (if (= index k)
                            (cond
                             ((null value)
                              (setf diff -1)
                              (cdrc list))
                             (t
                              list))
                            (cond
                             ((null value)
                              list)
                             (t
                              (setf diff +1)
                              (cons index list)))))
                       (t
                        (let* ((l (cdrc list))
                               (l* (update l)))
                          (if (eq l l*) list (cons k l*))))))))))
               (values (update list) diff))))
         (alist-update (alist index value default-value)
           (declare (type sparse-vector-index index))
           (let ((diff 0))
             (labels
               ((update (alist)
                  (cond
                   ((null alist)
                    (cond
                     ((eql default-value value)
                      nil)
                     (t
                      (setf diff +1)
                      (cons (cons index value) nil))))
                   (t
                    (let* ((p (carc alist))
                           (k (carc p)))
                      (declare (type sparse-vector-index k))
                      (cond
                       ((>= index k)
                        (if (= index k)
                            (cond
                             ((eql default-value value)
                              (setf diff -1)
                              (cdrc alist))
                             ((eql value (cdrc p))
                              alist)
                             (t
                              (cons (cons index value) (cdrc alist))))
                            (cond
                             ((eql default-value value)
                              alist)
                             (t
                              (setf diff +1)
                              (cons (cons index value) alist)))))
                       (t
                        (let* ((l (cdrc alist))
                               (l* (update l)))
                          (if (eq l l*) alist (cons p l*))))))))))
               (values (update alist) diff))))
         (sparse-vector-setter1 (n)
           (let ((alist (b-tree-node-alist n))
                 (last-value (b-tree-node-nonleaf-last-value n)))
             (cond
              ((null last-value)
               ;; leaf node of b-tree index
               (mvlet (((values alist1 diff)
                        (if (atom (carc alist))		;boolean sparse vector?
                            (list-update alist index value)
                            (alist-update alist index value (sparse-vector-default-value sparse-vector)))))
                 (declare (type fixnum diff))
                 (cond
                  ((eq alist alist1)
                   n)
                  (t
                   (when copy
                     (setf sparse-vector (copy-sparse-vector sparse-vector)))
                   (unless (= 0 diff)
                     (incf (sparse-vector-count0 sparse-vector) diff))
                   (cond
                    ((null alist1)
                     :delete)
                    ((and (= 1 diff) (full-alist alist))
                     (mvlet (((values alist2 alist1) (split-leaf-alist alist1 b-tree-node-size/2)))
                       (values
                        (make-b-tree-node alist1 nil)	;replacement for this node
                        (make-b-tree-node alist2 nil)	;new node to go before it
                        (floor (+ (smallest-key alist2) (+ (largest-key alist1) 1)) 2))))
                    (t
                     (make-b-tree-node alist1 nil)))))))
              (t
               ;; descend toward correct leaf node of b-tree index
               (let ((tail (b-tree-nonleaf-node-alist-search alist index)))
                 (if tail
                     (mvlet* ((p (carc tail))
                              (k (carc p))
                              (v (cdrc p))
                              ((values v1 n2 k2) (sparse-vector-setter1 v)))
                       (cond
                        ((eq v v1)
                         n)
                        ((eq :delete v1)
                         (cond
                          ((null (cdrc alist))		;if only one value remains
                           last-value)			;move it up in b-tree
                          (t
                           (make-b-tree-node (alist-update alist k nil nil) last-value))))
                        (n2
                         (let ((alist1 (alist-update (alist-update alist k v1 nil) k2 n2 nil)))
                           (cond
                            ((full-alist alist)
                             (mvlet (((values alist2 k v alist1) (split-nonleaf-alist alist1 b-tree-node-size/2)))
                               (values
                                (make-b-tree-node alist1 last-value)
                                (make-b-tree-node alist2 v)
                                k)))
                            (t
                             (make-b-tree-node alist1 last-value)))))
                        (t
                         (make-b-tree-node (alist-update alist k v1 nil) last-value))))
                     (mvlet* ((v last-value)
                              ((values v1 n2 k2) (sparse-vector-setter1 v)))
                       (cond
                        ((eq v v1)
                         n)
                        ((eq :delete v1)
                         (cond
                          ((null (cdrc alist))		;if only one value remains
                           (cdrc (carc alist)))		;move it up in b-tree
                          (t
                           (make-b-tree-node (butlast alist) (cdrc (lastc alist))))))
                        (n2
                         (let ((alist1 (alist-update alist k2 n2 nil)))
                           (cond
                            ((full-alist alist)
                             (mvlet (((values alist2 k v alist1) (split-nonleaf-alist alist1 b-tree-node-size/2)))
                               (values
                                (make-b-tree-node alist1 v1)
                                (make-b-tree-node alist2 v)
                                k)))
                            (t
                             (make-b-tree-node alist1 v1)))))
                        (t
                         (make-b-tree-node alist v1)))))))))))
        (mvlet (((values n1 n2 k2) (sparse-vector-setter1 n)))
          (cond
           ((eq n n1)
            )
           ((eq :delete n1)
            (setf (sparse-vector-b-tree-root-node sparse-vector) nil))
           (n2
            (setf (sparse-vector-b-tree-root-node sparse-vector) (make-b-tree-node (list (cons k2 n2)) n1)))
           (t
            (setf (sparse-vector-b-tree-root-node sparse-vector) n1))))))))
  (setf (sparse-vector-cached-key sparse-vector) index)
  (setf (sparse-vector-cached-value sparse-vector) (if value (if (sparse-vector-boolean sparse-vector) index value) nil))
  (if copy sparse-vector value))
;;; ***

(defun copy-sparse-vector (sparse-vector)
  (declare (type sparse-vector sparse-vector))
  (cond
   ((null (sparse-vector-type sparse-vector))
    (copy-structure sparse-vector))
   (t
    (error "Type ~A sparse-vector cannot be copied." (sparse-vector-type sparse-vector)))))

(definline spacons (index value sparse-vector)
  ;; does the following, except does not copy sparse-vector if it is not changed by the assignment
  ;; (let ((sv (copy-sparse-vector sparse-vector)))
  ;;   (setf (sparef sv index) value)
  ;;   sv)
  (sparse-vector-setter value sparse-vector index t))

(defmacro do-map-sparse-vector-backward (min max boolean map)
  ;; always returns nil
  (let ((p (and (not boolean) (not (eq :indexes-only map))))
        (k (or boolean map min max)))
    `(labels
       ((map1 (n)
          (let ((alist (b-tree-node-alist n))
                (last-value (b-tree-node-nonleaf-last-value n)))
            (cond
             ((null last-value)
              ;; leaf node
              (let (,@(when p (list `p)) ,@(when k (list `(k 0))))
                ,@(when (and k (or min max)) (list `(declare (type sparse-vector-index k))))
                (loop
                  ,@(cond
                     (boolean
                      (list
                       `(setf k (carc alist))))
                     ((and p k)
                      (list
                       `(setf k (carc (setf p (carc alist))))))
                     (p
                      (list
                       `(setf p (carc alist))))
                     (k
                      (list
                       `(setf k (carc (carc alist))))))
                  (cond
                   ,@(when max (list
                                `((and max (or (< (the sparse-vector-index max) k) (setf max nil)))
                                  )))
                   ,@(when min (list
                                `((and min (> (the sparse-vector-index min) k))
                                  (return-from map-sparse-vector-backward nil))))
                   (t
                    ,(cond
                      ((null map)
                       `(funcall function ,(if boolean `k `(cdrc p))))
                      ((eq :with-indexes map)
                       `(funcall function ,(if boolean `k `(cdrc p)) k))
                      (t ;(eq :indexes-only map)
                       `(funcall function k)))))
                  (when (null (setf alist (cdrc alist)))
                    (return nil)))))
             (t
              ;; nonleaf node
              (let (p)
                (loop
                  (setf p (carc alist))
                  (cond
                   ,@(when max (list
                                `((and max (< (the sparse-vector-index max) (the sparse-vector-index (carc p))))
                                  )))
                   (t
                    (map1 (cdrc p))))
                  (when (null (setf alist (cdrc alist)))
                    (return nil))))
              (cond
               ,@(when max (list
                            `((and max (< (the sparse-vector-index max) (the sparse-vector-index (b-tree-node-smallest-key* last-value))))
                              )))
               (t
                (map1 last-value))))))))
       (map1 n))))

(defmacro do-map-sparse-vector-forward (min max boolean map)
  ;; always returns nil
  (let ((p (and (not boolean) (not (eq :indexes-only map))))
        (k (or boolean map min max)))
    `(labels
       ((map1 (n)
          (let ((alist (b-tree-node-alist n))
                (last-value (b-tree-node-nonleaf-last-value n)))
            (cond
             ((null last-value)
              ;; leaf node
              (macrolet
                ((domap1 ()
                   (nestn '(progn
                             (let ((alist (cdrc alist)))
                               (when alist
                                 ***))
                             ,@(cond
                                (boolean
                                 (list
                                  `(setf k (carc alist))))
                                ((and p k)
                                 (list
                                  `(setf k (carc (setf p (carc alist))))))
                                (p
                                 (list
                                  `(setf p (carc alist))))
                                (k
                                 (list
                                  `(setf k (carc (carc alist))))))
                             (cond
                              ,@(when min (list
                                           `((and min (or (> (the sparse-vector-index min) k) (setf min nil)))
                                             )))
                              ,@(when max (list
                                           `((and max (< (the sparse-vector-index max) k))
                                             (return-from map-sparse-vector-forward nil))))
                              (t
                               ,(cond
                                 ((null map)
                                  `(funcall function ,(if boolean `k `(cdrc p))))
                                 ((eq :with-indexes map)
                                  `(funcall function ,(if boolean `k `(cdrc p)) k))
                                 (t ;(eq :indexes-only map)
                                  `(funcall function k))))))
                          nil
                          b-tree-node-size)))
                (let (,@(when p (list `p)) ,@(when k (list `(k 0))))
                  ,@(when (and k (or min max)) (list `(declare (type sparse-vector-index k))))
                  (domap1))))
             (t
              ;; nonleaf node
              (cond
               ,@(when min (list
                            `((and min (> (the sparse-vector-index min) (the sparse-vector-index (b-tree-node-largest-key* last-value))))
                              )))
               (t
                (map1 last-value)))
              (macrolet
                ((domap1 ()
                   (nestn '(progn
                             (let ((alist (cdrc alist)))
                               (when alist
                                 ***))
                             (setf v (cdrc (carc alist)))
                             (cond
                              ,@(when min (list
                                           `((and min (> (the sparse-vector-index min) (the sparse-vector-index (b-tree-node-largest-key* v))))
                                             )))
                              (t
                               (map1 v))))
                          nil
                          b-tree-node-size)))
                (let (v)
                  (domap1))))))))
       (map1 n)
       nil)))

(defun map-sparse-vector-backward (function n)
  (do-map-sparse-vector-backward nil nil nil nil))

(defun map-sparse-vector-backward-with-indexes (function n)
  (do-map-sparse-vector-backward nil nil nil :with-indexes))

(defun map-sparse-vector-backward-indexes-only (function n)
  (do-map-sparse-vector-backward nil nil nil :indexes-only))

(defun map-sparse-vector-forward (function n)
  (do-map-sparse-vector-forward nil nil nil nil))

(defun map-sparse-vector-forward-with-indexes (function n)
  (do-map-sparse-vector-forward nil nil nil :with-indexes))

(defun map-sparse-vector-forward-indexes-only (function n)
  (do-map-sparse-vector-forward nil nil nil :indexes-only))

(defun map-sparse-vector-backward-bounded (function n min max)
  (block map-sparse-vector-backward
    (do-map-sparse-vector-backward t t nil nil)))

(defun map-sparse-vector-backward-bounded-with-indexes (function n min max)
  (block map-sparse-vector-backward
    (do-map-sparse-vector-backward t t nil :with-indexes)))

(defun map-sparse-vector-backward-bounded-indexes-only (function n min max)
  (block map-sparse-vector-backward
    (do-map-sparse-vector-backward t t nil :indexes-only)))

(defun map-sparse-vector-forward-bounded (function n min max)
  (block map-sparse-vector-forward
    (do-map-sparse-vector-forward t t nil nil)))

(defun map-sparse-vector-forward-bounded-with-indexes (function n min max)
  (block map-sparse-vector-forward
    (do-map-sparse-vector-forward t t nil :with-indexes)))

(defun map-sparse-vector-forward-bounded-indexes-only (function n min max)
  (block map-sparse-vector-forward
    (do-map-sparse-vector-forward t t nil :indexes-only)))

(defun map-boolean-sparse-vector-backward (function n)
  (do-map-sparse-vector-backward nil nil t nil))

(defun map-boolean-sparse-vector-backward-with-indexes (function n)
  (do-map-sparse-vector-backward nil nil t :with-indexes))

(defun map-boolean-sparse-vector-forward (function n)
  (do-map-sparse-vector-forward nil nil t nil))

(defun map-boolean-sparse-vector-forward-with-indexes (function n)
  (do-map-sparse-vector-forward nil nil t :with-indexes))

(defun map-boolean-sparse-vector-backward-bounded (function n min max)
  (block map-sparse-vector-backward
    (do-map-sparse-vector-backward t t t nil)))

(defun map-boolean-sparse-vector-backward-bounded-with-indexes (function n min max)
  (block map-sparse-vector-backward
    (do-map-sparse-vector-backward t t t :with-indexes)))

(defun map-boolean-sparse-vector-forward-bounded (function n min max)
  (block map-sparse-vector-forward
    (do-map-sparse-vector-forward t t t nil)))

(defun map-boolean-sparse-vector-forward-bounded-with-indexes (function n min max)
  (block map-sparse-vector-forward
    (do-map-sparse-vector-forward t t t :with-indexes)))

;;; ****if* snark-sparse-array/map-sparse-vector0
;;; USAGE
;;;   (map-sparse-vector0 function sparse-vector reverse min max map)
;;; SOURCE

(defun map-sparse-vector0 (function sparse-vector reverse min max map)
  (declare (type sparse-vector sparse-vector))
  ;; always returns nil
  (let ((n (sparse-vector-b-tree-root-node sparse-vector)))
    (unless (null n)
      (let ((boolean (sparse-vector-boolean sparse-vector)))
        (cond
         ((and (null min) (null max))
          (let ((alist (b-tree-node-alist n)))
            (when (and (null (cdrc alist)) (null (b-tree-node-nonleaf-last-value n)))
              (let ((p (carc alist)))	;(= 1 (sparse-vector-count sparse-vector)) special case
                (if boolean
                    (cond
                     ((null map)
                      (funcall function p))
                     ((eq :with-indexes map)
                      (funcall function p p))
                     (t ;(eq :indexes-only map)
                      (funcall function p)))
                    (cond
                     ((null map)
                      (funcall function (cdrc p)))
                     ((eq :with-indexes map)
                      (funcall function (cdrc p) (carc p)))
                     (t ;(eq :indexes-only map)
                      (funcall function (carc p))))))
              (return-from map-sparse-vector0 nil)))
          (if reverse
              (cond
               ((null map)
                (if boolean
                    (map-boolean-sparse-vector-backward function n)
                    (map-sparse-vector-backward function n)))
               ((eq :with-indexes map)
                (if boolean
                    (map-boolean-sparse-vector-backward-with-indexes function n)
                    (map-sparse-vector-backward-with-indexes function n)))
               (t ;(eq :indexes-only map)
                (if boolean
                    (map-boolean-sparse-vector-backward function n)
                    (map-sparse-vector-backward-indexes-only function n))))
              (cond
               ((null map)
                (if boolean
                    (map-boolean-sparse-vector-forward function n)
                    (map-sparse-vector-forward function n)))
               ((eq :with-indexes map)
                (if boolean
                    (map-boolean-sparse-vector-forward-with-indexes function n)
                    (map-sparse-vector-forward-with-indexes function n)))
               (t ;(eq :indexes-only map)
                (if boolean
                    (map-boolean-sparse-vector-forward function n)
                    (map-sparse-vector-forward-indexes-only function n))))))
         (t
          (if reverse
              (cond
               ((null map)
                (if boolean
                    (map-boolean-sparse-vector-backward-bounded function n min max)
                    (map-sparse-vector-backward-bounded function n min max)))
               ((eq :with-indexes map)
                (if boolean
                    (map-boolean-sparse-vector-backward-bounded-with-indexes function n min max)
                    (map-sparse-vector-backward-bounded-with-indexes function n min max)))
               (t ;(eq :indexes-only map)
                (if boolean
                    (map-boolean-sparse-vector-backward-bounded function n min max)
                    (map-sparse-vector-backward-bounded-indexes-only function n min max))))
              (cond
               ((null map)
                (if boolean
                    (map-boolean-sparse-vector-forward-bounded function n min max)
                    (map-sparse-vector-forward-bounded function n min max)))
               ((eq :with-indexes map)
                (if boolean
                    (map-boolean-sparse-vector-forward-bounded-with-indexes function n min max)
                    (map-sparse-vector-forward-bounded-with-indexes function n min max)))
               (t ;(eq :indexes-only map)
                (if boolean
                    (map-boolean-sparse-vector-forward-bounded function n min max)
                    (map-sparse-vector-forward-bounded-indexes-only function n min max)))))))))))
;;; ***

;;; ****f* snark-sparse-array/map-sparse-vector
;;; USAGE
;;;   (map-sparse-vector function sparse-vector &key reverse min max)
;;; RETURN VALUE
;;;   nil
;;; DESCRIPTION
;;;   The map-sparse-vector function applies its unary-function argument to
;;;   each value (or index, if sparse-vector is boolean) in sparse-vector.
;;;   It does nothing if sparse-vector is nil.
;;;
;;;   The function is applied only to values whose index is >= min
;;;   and <= max if they are specified.  If reverse is nil, the
;;;   function is applied to values in ascending order by index;
;;;   otherwise, the order is reversed.
;;; SEE ALSO
;;;   map-sparse-vector-with-indexes
;;;   map-sparse-vector-indexes-only
;;; SOURCE

(definline map-sparse-vector (function sparse-vector &key reverse min max)
  (when sparse-vector
    (map-sparse-vector0 function sparse-vector reverse min max nil)))
;;; ***

;;; ****f* snark-sparse-array/map-sparse-vector-with-indexes
;;; USAGE
;;;   (map-sparse-vector-with-indexes function sparse-vector &key reverse min max)
;;; RETURN VALUE
;;;   nil
;;; DESCRIPTION
;;;   The map-sparse-vector-with-indexes function is like map-sparse-vector,
;;;   but applies its binary-function argument to each value and index in sparse-vector.
;;; SEE ALSO
;;;   map-sparse-vector
;;;   map-sparse-vector-indexes-only
;;; SOURCE

(definline map-sparse-vector-with-indexes (function sparse-vector &key reverse min max)
  (when sparse-vector
    (map-sparse-vector0 function sparse-vector reverse min max :with-indexes)))
;;; ***

;;; ****f* snark-sparse-array/map-sparse-vector-indexes-only
;;; USAGE
;;;   (map-sparse-vector-indexes-only function sparse-vector &key reverse min max)
;;; RETURN VALUE
;;;   nil
;;; DESCRIPTION
;;;   The map-sparse-vector-indexes-only function is like map-sparse-vector,
;;;   but applies its unary-function argument to each index in sparse-vector.
;;;   map-sparse-vector and map-sparse-vector-indexes-only operate identically
;;;   on boolean sparse-vectors.
;;; SEE ALSO
;;;   map-sparse-vector
;;;   map-sparse-vector-with-indexes
;;; SOURCE

(definline map-sparse-vector-indexes-only (function sparse-vector &key reverse min max)
  (when sparse-vector
    (map-sparse-vector0 function sparse-vector reverse min max :indexes-only)))
;;; ***

;;; ****f* snark-sparse-array/first-sparef
;;; USAGE
;;;   (first-sparef sparse-vector)
;;; RETURN VALUE
;;;   (values (sparef sparse-vector first-index) first-index) or
;;;   (values default-value nil) if sparse-vector is empty
;;; SEE ALSO
;;;   pop-first-sparef
;;; SOURCE

(defun first-sparef (sparse-vector)
  (declare (type sparse-vector sparse-vector))
  (let ((n (sparse-vector-b-tree-root-node sparse-vector)))
    (cond
     ((null n)
      (values (sparse-vector-default-value sparse-vector) nil))
     (t
      (mvlet (((values index value) (b-tree-node-smallest-key* n)))
        (values
         (setf (sparse-vector-cached-value sparse-vector) value)
         (setf (sparse-vector-cached-key sparse-vector) index)))))))
;;; ***

;;; ****f* snark-sparse-array/last-sparef
;;; USAGE
;;;   (last-sparef sparse-vector)
;;; RETURN VALUE
;;;   (values (sparef sparse-vector last-index) last-index) or
;;;   (values default-value nil) if sparse-vector is empty
;;; SEE ALSO
;;;   pop-last-sparef
;;; SOURCE

(defun last-sparef (sparse-vector)
  (declare (type sparse-vector sparse-vector))
  (let ((n (sparse-vector-b-tree-root-node sparse-vector)))
    (cond
     ((null n)
      (values (sparse-vector-default-value sparse-vector) nil))
     (t
      (mvlet (((values index value) (b-tree-node-largest-key* n)))
        (values
         (setf (sparse-vector-cached-value sparse-vector) value)
         (setf (sparse-vector-cached-key sparse-vector) index)))))))
;;; ***

;;; ****f* snark-sparse-array/pop-first-sparef
;;; USAGE
;;;   (pop-first-sparef sparse-vector)
;;; RETURN VALUE
;;;   (values (sparef sparse-vector first-index) first-index) or
;;;   (values default-value nil) if sparse-vector is empty
;;; SIDE EFFECTS
;;;   removes it from sparse-vector
;;; SEE ALSO
;;;   first-sparef
;;; SOURCE

(defun pop-first-sparef (sparse-vector)
  (declare (type sparse-vector sparse-vector))
  (mvlet (((values value index) (first-sparef sparse-vector)))
    (when index
      (sparse-vector-setter (sparse-vector-default-value sparse-vector) sparse-vector index))
    (values value index)))
;;; ***

;;; ****f* snark-sparse-array/pop-last-sparef
;;; USAGE
;;;   (pop-last-sparef sparse-vector)
;;; RETURN VALUE
;;;   (values (sparef sparse-vector last-index) last-index) or
;;;   (values default-value nil) if sparse-vector is empty
;;; SIDE EFFECTS
;;;   removes it from sparse-vector
;;; SEE ALSO
;;;   last-sparef
;;; SOURCE

(defun pop-last-sparef (sparse-vector)
  (declare (type sparse-vector sparse-vector))
  (mvlet (((values value index) (last-sparef sparse-vector)))
    (when index
      (sparse-vector-setter (sparse-vector-default-value sparse-vector) sparse-vector index))
    (values value index)))
;;; ***

;;; sparse-vector5.lisp EOF
