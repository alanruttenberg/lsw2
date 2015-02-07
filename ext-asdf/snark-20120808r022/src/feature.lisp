;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark-feature -*-
;;; File: feature.lisp
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

(in-package :snark-feature)

;;; a tree of features
;;;
;;; in the tree of features, if s2 is a descendant of s1,
;;; then s1 is less deep than s2 on same branch (feature< s1 s2)
;;; and s2 is more specific than s1 (feature> s2 s1)
;;;
;;; feature expressions are single features or length>1 lists of features
;;; feature expressions are maximally specific and nonredundant;
;;; in a list of features, no feature is >= another
;;; lists of features are ordered by feature-preorder-min
;;;
;;; when combining features, the union is formed of feature expressions
;;;
;;; children of a feature can be declared to be incompatible
;;; they and their descendants cannot be used together
;;; their union is nil (bottom value denoting incompatible features)
;;;
;;; features can be deleted rendering feature expressions that contain them "not live"
;;; deleting a feature also causes deletion of its descendant (more specific) features
;;;
;;; initialize-features - creates tree of features *feature-tree* with an undeletable root feature
;;; make-feature        - creates a feature, can specify name and parent and children-incompatible=t/nil
;;; declare-feature     - returns or creates a feature or associates a name with a conjunction of features
;;; declare-features-incompatible - declares a pair (or larger set) of features to be incompatible
;;; feature?            - returns t for single feature, nil otherwise
;;; feature-parent      - returns parent of feature, nil if root
;;; the-feature         - coerces name to feature, nil, warn, or error if doesn't exist or deleted
;;; delete-feature      - deletes feature from tree of features
;;; feature-live?       - returns feature expression arg if its features are undeleted, nil otherwise
;;; feature-union       - returns union of two feature expressions, nil if incompatible
;;; feature-subsumes?   - returns t if 2nd arg is more specific feature or list of features than 1st, nil otherwise
;;; print-feature-tree  - prints feature tree
;;;
;;; features can be declared only once
;;; features must be declared before they are used
;;; feature incompatibilities must be declared before incompatible features are used

(defvar *feature-tree*)

(defstruct (feature-tree
            (:copier nil))
  (root nil :read-only t)
  (name-table (make-hash-table) :read-only t)
  (canonical-lists (make-hash-table :test #'equal)))

(defstruct (feature
            (:constructor make-feature0 (name parent children-incompatible depth))
            (:print-function print-feature3)
            (:predicate feature?)
            (:copier nil))
  (name nil)
  (parent nil)
  (children-incompatible nil)
  (depth 0 :read-only t)
  (type nil)				;nil, :deleted, or (:characteristic-feature ...)
  (preorder-min 0)			;feature number
  (preorder-max 0)			;subfeature numbers in [preorder-min+1,preorder-max]
  (children nil)
  (incompatible-features nil)		;(N incompat1 ... incompatN) for 2-ary nogoods
  (users-in-name-table nil)
  (users-in-canonical-lists nil)
  (nogoods nil)
  (code nil))

(defstruct (feature-combo
            (:constructor make-feature-combo (list))
            (:print-function print-feature-combo3)
            (:predicate feature-combo?)
            (:copier nil))
  (name nil)
  (list nil :read-only t))

(defun initialize-features ()
  (let ((root (make-feature0 'top nil nil 0)))
    (setf *feature-tree* (make-feature-tree :root root))
    (setf (gethash 'top (feature-tree-name-table *feature-tree*)) root)
    (declare-feature 'characteristic-feature)
    root))

(defun make-feature1 (name parent children-incompatible)
  (let* ((tree *feature-tree*)
         (root (feature-tree-root tree)))
    (unless parent
      (setf parent root))
    (let ((new-node (make-feature0 name parent children-incompatible (+ (feature-depth parent) 1))))
      (when name
        (setf (gethash name (feature-tree-name-table tree)) new-node))
      (let ((children (feature-children parent)) (n (feature-preorder-max parent)) m)
        (cond
         (children
          (let ((last (last children)))
            (setf m (+ (feature-preorder-max (first last)) 1))
            (setf (cdr last) (list new-node))))
         (t
          (setf m (+ (feature-preorder-min parent) 1))
          (setf (feature-children parent) (list new-node))))
        (cond
         ((<= m n)
          (setf (feature-preorder-min new-node) m)
          (setf (feature-preorder-max new-node) (floor (+ m n) 2)))
         (t
          (feature-tree-preorder-labeling root -1))))
      new-node)))

(defun make-feature (&key name parent children-incompatible)
  ;; always makes a new feature even if one by this name already exists
  (when parent
    (unless (feature? parent)
      (let ((parent* (and (can-be-feature-name parent nil) (the-feature parent nil))))
        (if (feature? parent*)
            (setf parent parent*)
            (error "There is no feature ~S." parent)))))
  (when name
    (if (can-be-feature-name name 'error)
        (delete-feature-name name)
        (setf name nil)))
  (make-feature1 name parent children-incompatible))

(defun declare-feature (name &key parent children-incompatible iff implies new-name alias)
  ;; does not make a new feature if one by this name already exists
  ;; should check that parent, children-incompatible, iff definition are compatible
  (can-be-feature-name name 'error)
  (declare-feature-aliases
   (or (and new-name (not (eq name new-name)) (rename-feature name new-name))
       (lookup-feature-name name)
       (cond
        ((or implies iff)
         (cl:assert (not (and iff children-incompatible)))
         (cl:assert (null parent))
         (let ((cf nil))
           (when implies
             (cl:assert (null iff))
             (setf implies (the-feature implies 'error 'error :dont-canonize))
             ;; use implies as parent if possible
             (when (feature? implies)
               (return-from declare-feature
                 (make-feature :name name :parent implies :children-incompatible children-incompatible)))
             (setf iff (cons (setf cf (make-feature :parent (or (extract-a-characteristic-feature implies) 'characteristic-feature)
                                                    :children-incompatible children-incompatible))
                             (mklist implies))))
           ;; make name designate the iff feature expression (a feature or list of features)
           (let ((v (the-feature iff 'error)))
             (setf (gethash name (feature-tree-name-table *feature-tree*)) v)
             (cond
              ((feature-combo? v)
               (unless (eq v (lookup-feature-name (feature-combo-name v)))
                 (setf (feature-combo-name v) name))
               (dolist (v (feature-combo-list v))
                 (push name (feature-users-in-name-table v))))
              (t
               (push name (feature-users-in-name-table v))))
             (when cf
               (setf (feature-name cf) (make-symbol (to-string "*" name "*")))
               (setf (feature-type cf) (list :characteristic-feature v)))
             v)))
        (t
         (make-feature :name name :parent parent :children-incompatible children-incompatible))))
   alias))

(defun declare-feature-aliases (n alias)
  (mapc #'(lambda (alias) (declare-feature alias :iff n)) (mklist alias))
  n)

(defun characteristic-feature-type (n)
  (let ((type (feature-type n)))
    (and (consp type) (eq :characteristic-feature (first type)) type)))

(defun extract-a-characteristic-feature (x)
  (let ((l (characteristic-feature-restriction (feature-combo-list x))))
    (cond
     ((null (rest l))
      (if (characteristic-feature-type (first l)) (first l) nil))
     (t
      (dolist (x l nil)
        (when (and (characteristic-feature-type x) (not (feature-children-incompatible x)))
          (return x)))))))        

(defun rename-feature (name new-name)
  (can-be-feature-name new-name 'error)
  (when (lookup-feature-name new-name)
    (error "Feature name ~S is already in use." new-name))
  (let ((v (lookup-feature-name name 'error))
        (name-table (feature-tree-name-table *feature-tree*)))
    (remhash name name-table)
    (setf (gethash new-name name-table) v)
    (cond
     ((eq name (feature-name v))
      (when (feature-combo? v)
        (dolist (x (feature-combo-list v))
          (setf (feature-users-in-name-table x) (nsubstitute new-name name (feature-users-in-name-table x)))))
      (setf (feature-name v) new-name))
     (t
      (setf (feature-users-in-name-table v) (nsubstitute new-name name (feature-users-in-name-table v)))))
    v))

(defun delete-feature (n1)
  (let* ((tree *feature-tree*)
         (name-table (feature-tree-name-table tree)))
    (labels
      ((delete-feature1 (n)
         (setf (feature-type n) :deleted)
         (setf (feature-parent n) nil)
         ;; delete this feature from the name table
         (let ((name (feature-name n)))
           (when name
             (remhash name name-table)
             (setf (feature-name n) nil)))
         (let ((names (feature-users-in-name-table n)))
           (when names
             (dolist (name names)
               (remhash name name-table))
             (setf (feature-users-in-name-table n) nil)))
         ;; delete every canonical list that contains this feature
         ;; also delete references to deleted canonical lists from this and other features
         (let ((cls (feature-users-in-canonical-lists n)))
           (when cls
             (let ((canonical-lists (feature-tree-canonical-lists tree)))
               (dolist (cl cls)
                 (multiple-value-bind (v found) (gethash (feature-canonical-list-key cl) canonical-lists)
                   (cl:assert found)
                   (dolist (n2 cl)
                     (unless (eq n n2)
                       (setf (feature-users-in-canonical-lists n2) (delete cl (feature-users-in-canonical-lists n2) :count 1))
                       (when (null v)
                         (setf (feature-nogoods n2) (delete cl (feature-nogoods n2) :count 1)))))
                   (remhash cl canonical-lists))))
             (setf (feature-users-in-canonical-lists n) nil)
             (setf (feature-nogoods n) nil)))
         ;; update information about incompatible pair of features
         (let ((incompat (feature-incompatible-features n)))
           (when incompat
             (dolist (n2 (rest incompat))
               (let* ((incompat2 (feature-incompatible-features n2))
                      (c (- (first incompat2) 1)))
                 (if (eql 0 c)
                     (setf (feature-incompatible-features n2) nil)
                     (let ((l (rest incompat2)))
                       (setf (rest incompat2) (if (eq n (first l)) (rest l) (delete n l :count 1))
                             (first incompat2) c)))))
             (setf (feature-incompatible-features n) nil)))
         (let ((children (feature-children n)))
           (when children
             (dolist (child children)
               (delete-feature1 child))
             (setf (feature-children n) nil)))))
      (cl:assert (or (feature? n1) (can-be-feature-name n1 nil)))
      (let ((n (the-feature n1 nil)))
        (when n
          (cond
           ((feature-combo? n)
            (delete-feature-name n1)			;delete the name of a list of features
            (dolist (x (feature-combo-list n))		;delete its characteristic feature if there is one
              (let ((v (characteristic-feature-type x)))
                (when (and v (eq n (second v)))
                  (delete-feature x)
                  (return)))))
           (t
            (let ((parent (feature-parent n)))
              (cl:assert parent)			;can't delete root node
              ;; detach this feature from the tree of features
              (let ((l (feature-children parent)))
                (setf (feature-children parent) (if (eq n (first l)) (rest l) (delete n l :count 1))))
              ;; mark this feature and all its descendants as deleted
              (delete-feature1 n))))
          t)))))

(definline feature-deleted? (node)
  (eq :deleted (feature-type node)))

(defun can-be-feature-name (x &optional action)
  (or (and x (symbolp x) (not (eq 'and x)) (not (eq 'or x)) (not (eq 'not x)))
      (and action (funcall action "~S cannot be the name of a feature." x))))

(defun lookup-feature-name (name &optional action)
  (or (gethash name (feature-tree-name-table *feature-tree*))
      (and action (funcall action "There is no feature named ~S." name))))

(defun delete-feature-name (name)
  (let* ((name-table (feature-tree-name-table *feature-tree*))
         (v (gethash name name-table)))
    (when v
      (cond
       ((feature-combo? v)
        (when (eq name (feature-combo-name v))
          (setf (feature-combo-name v) nil))
        (dolist (x (feature-combo-list v))
          (setf (feature-users-in-name-table x) (delete name (feature-users-in-name-table x) :count 1))))
       (t
        (when (eq name (feature-name v))
          (setf (feature-name v) nil))
        (setf (feature-users-in-name-table v) (delete name (feature-users-in-name-table v) :count 1))))
      (remhash name name-table))))

(defun the-feature (x &optional (action 'error) (action2 action) canonize-option)
  ;; returns
  ;;   feature from its name
  ;;   or conjunction of features from list of names
  ;; feature or feature-combo structures can be used in place of names
  (flet ((the-feature0 (x)
           (if (or (feature? x) (feature-combo? x))
               (feature-live? x action)
               (lookup-feature-name x action))))
    (cond
     ((atom x)
      (the-feature0 x))
     (t
      (when (eq 'and (first x))
        (setf x (rest x)))
      (let ((l (the-feature (first x) action action2 :dont-canonize)))
        (cond
         ((null l)
          (return-from the-feature nil))
         (t
          (dolist (x1 (rest x))
            (let ((x1* (the-feature x1 action action2 :dont-canonize)))
              (if (null x1*)
                  (return-from the-feature nil)
                  (setf l (feature-union x1* l nil)))))))
        (or (feature-canonize l canonize-option)
            (and action2 (funcall action2 "The conjunction of ~A~{ and ~A~} are incompatible." (first x) (rest x)))))))))

(defun feature-tree-preorder-labeling (node n)
  (setf (feature-preorder-min node) (incf n))
  (dolist (c (feature-children node))
    (setf n (feature-tree-preorder-labeling c n)))
  (setf (feature-preorder-max node) (+ n 999)))

(definline feature> (n1 n2)
  ;; is n1 a descendant of n2?
  (and (not (eq n1 n2))
       (>= (feature-preorder-max n2)
           (feature-preorder-min n1)
           (feature-preorder-min n2))))

(definline feature>= (n1 n2)
  (or (eq n1 n2)
      (>= (feature-preorder-max n2)
          (feature-preorder-min n1)
          (feature-preorder-min n2))))

(definline feature< (n1 n2)
  (feature> n2 n1))

(definline feature<= (n1 n2)
  (feature>= n2 n1))

(defun feature-ancestor (node &optional (n 1))
;;(cl:assert (<= 0 n (feature-depth node)))
  (dotimes (i n)
    (declare (ignorable i))
    (setf node (feature-parent node)))
  node)

(definline nearest-common-feature-ancestor (node1 node2)
  ;; returns the nearest common ancestor of node1 and node2
  ;; also returns the counts of declared-incompatible-features along each path
  (let ((d1 (feature-depth node1))
        (d2 (feature-depth node2))
        (nincompat1 0)
        (nincompat2 0))
    (cond
     ((> d1 d2)
      (dotimes (i (- d1 d2))
        (declare (ignorable i))
        (let ((incompat (feature-incompatible-features node1)))
          (when incompat
            (incf nincompat1 (first incompat))))
        (setf node1 (feature-parent node1))))
     ((< d1 d2)
      (dotimes (i (- d2 d1))
        (declare (ignorable i))
        (let ((incompat (feature-incompatible-features node2)))
          (when incompat
            (incf nincompat2 (first incompat))))
        (setf node2 (feature-parent node2)))))
    (loop
      (if (eq node1 node2)
          (return (values node1 nincompat1 nincompat2))
          (progn
            (let ((incompat (feature-incompatible-features node1)))
              (when incompat
                (incf nincompat1 (first incompat))))
            (let ((incompat (feature-incompatible-features node2)))
              (when incompat
                (incf nincompat2 (first incompat))))
            (setf node1 (feature-parent node1)
                  node2 (feature-parent node2)))))))

(defun feature-incompatible0 (s1 s2)
  ;; s1 and s2 are single features
  (and (not (eq s1 s2))
       (multiple-value-bind (s nincompat1 nincompat2) (nearest-common-feature-ancestor s1 s2)
         (and (not (eq s s1))
              (not (eq s s2))
              (or (feature-children-incompatible s)
                  (and (not (eql 0 nincompat1))
                       (not (eql 0 nincompat2))
                       (progn
                         (when (> nincompat1 nincompat2)
                           (psetf s1 s2 s2 s1))
                         (loop				;is s2 a descendant of any feature in incompat1?
                           (cond
                            ((let ((incompat (feature-incompatible-features s1)))
                               (and incompat
                                    (dolist (y (rest incompat) nil)
                                      (when (feature<= y s2)
                                        (return t)))))
                             (return t))
                            ((eq s (setf s1 (feature-parent s1)))
                             (return nil)))))))))))

(definline feature-incompatible1 (s1 s2)
  ;; s1 is single feature, s2 is nonempty list of features
  (dolist (s2 s2 nil)
    (when (feature-incompatible0 s1 s2)
      (return t))))

(definline feature-incompatible2 (s1 s2)
  ;; s1 and s2 are nonempty lists of features
  (dolist (s1 s1 nil)
    (when (feature-incompatible1 s1 s2)
      (return t))))

(defun feature-merge1 (s1 s2 &optional (n1 (feature-preorder-min s1)))
  ;; s1 is single feature, s2 is nonempty list of features that does not contain s1
  (if (< n1 (feature-preorder-min (first s2)))
      (cons s1 s2)
      (cons (pop s2) (if (null s2) (list s1) (feature-merge1 s1 s2 n1)))))

(defun feature-merge2 (s1 s2 &optional (n1 (feature-preorder-min (first s1))) (n2 (feature-preorder-min (first s2))))
  ;; s1 and s2 are nonempty lists of features with no common elements
   (if (< n1 n2)
       (cons (pop s1) (if (null s1) s2 (feature-merge2 s2 s1 n2)))
       (cons (pop s2) (if (null s2) s1 (feature-merge2 s1 s2 n1)))))
  
(defun feature-set-difference (s1 s2 test)
  ;; need something like this because set-difference is not guaranteed to preserve order (and doesn't in MCL)
;;(cl:assert (not (null s1)))
  (labels
    ((fsd (s1)
       (let ((x (first s1))
             (l (rest s1)))
         (if (member x s2 :test test)
             (if (null l)
                 nil
                 (fsd l))
             (if (null l)
                 s1
                 (let ((l* (fsd l)))
                   (if (eq l l*)
                       s1
                       (cons x l*))))))))
    (fsd s1)))

(definline feature-subsumes1 (s1 s2)
  (let ((s1min (feature-preorder-min s1))
        (s1max (feature-preorder-max s1)))
    (dotails (l s2 nil)					;(some (lambda (s2) (feature<= s1 s2)) s2)
      (let ((s2 (first l)) s2min)
        (cond
         ((eq s1 s2)
          (return l))
         ((not (<= (setf s2min (feature-preorder-min s2)) s1max))
          (return nil))
         ((<= s1min s2min)
          (return l)))))))

(definline feature-subsumes2 (s1 s2)
  ;; s1 and s2 are nonempty lists of features
  (and (length<= s1 s2)
       (dolist (s1 s1 t)				;(subsetp s1 s2 :test #'feature<=)))
         (if (or (null s2) (null (setf s2 (feature-subsumes1 s1 s2))))
             (return nil)
             (setf s2 (rest s2))))))

(defun feature-subsumes? (s1 s2)
  ;; s1 and s2 are features or lists of features
  ;; handle bottom value too: return nil if s1 or s2 is nil
  (and s1
       s2
       (if (feature-combo? s1)
           (if (feature-combo? s2)
               (feature-subsumes2 (feature-combo-list s1) (feature-combo-list s2))
               nil)					;(every (lambda (s1) (feature<= s1 s2)) s1), can't happen if s1 is nonredundant
       (if (feature-combo? s2)
           (and (feature-subsumes1 s1 (feature-combo-list s2)) t)
           (feature<= s1 s2)))))

(defun feature-canonical-list-key (s)
  (cons (let ((n 0))
          (dolist (s s)
            (setf n (logxor n (or (feature-code s) (setf (feature-code s) (random most-positive-fixnum))))))
          n)
        s))

(defun feature-canonical-list-unkey (k)
  (rest k))

(defun feature-canonize (s &optional option)
  ;; returns nil, a feature struct, or a canonical-list-indexed feature-combo struct
  (when (and (eq :incompatible option) (consp s) (rest s))
    (setf s (characteristic-feature-restriction s)))
  (cond
   ((null s)
    nil)
   ((feature? s)
    (if (eq :incompatible option) (error "Cannot declare single feature ~A to be incompatible." s) s))
   ((feature-combo? s)
    (if (eq :incompatible option) (error "Incompatible features already used together.") s))
   ((null (rest s))
    (if (eq :incompatible option) (error "Cannot declare single feature ~A to be incompatible." (first s)) (first s)))
   ((eq :dont-canonize option)
    s)
   (t
    (let ((table (feature-tree-canonical-lists *feature-tree*))
          (k (feature-canonical-list-key s)))
      (multiple-value-bind (v found) (gethash k table)
        (cond
         (found
          (if (and v (eq :incompatible option)) (error "Incompatible features already used together.") v))
         ;; lists of features created by feature-union are certain to be pairwise compatible
         ;; check them for n-ary incompatibility
         ;; inefficient test of s being subsumed by >=3-ary incompatiblity constraint
         ((and (rrest s)
               (let ((s* nil) (x nil))
                 (and (let ((len 0) (n 0))
                        (dolist (s1 s (<= 3 len))		;find at least 3 features relevant to nogoods
                          (let ((y s1) (m 0))
                            (loop
                              (let ((ngs (feature-nogoods y)))
                                (when ngs
                                  (incf m (if (null (rest ngs)) 1 (length ngs)))))
                              (when (null (setf y (feature-parent y)))
                                (unless (eql 0 m)
                                  (push s1 s*)
                                  (incf len)
                                  (when (or (null x) (> n m))
                                    (setf x s1 n m)))
                                (return))))))
                      (let ((y x))				;x in s* has fewest nogoods; test s* against them
                        (loop
                          (when (dolist (ng (feature-nogoods y) nil)
                                  (when (feature-subsumes2 ng (nreverse s*))
                                    (return t)))
                            (return t))
                          (when (null (setf y (feature-parent y)))
                            (return nil)))))))
          nil)
         ((eq :incompatible option)
          (cond
           ((null (rrest s))
            ;; add 2-ary incompatibility constraint
            (let* ((n1 (first s))
                   (n2 (second s))
                   (incompat1 (feature-incompatible-features n1))
                   (incompat2 (feature-incompatible-features n2)))
              (if incompat1
                  (setf (first incompat1) (+ (first incompat1) 1) (rest incompat1) (cons n2 (rest incompat1)))
                  (setf (feature-incompatible-features n1) (list 1 n2)))
              (if incompat2
                  (setf (first incompat2) (+ (first incompat2) 1) (rest incompat2) (cons n1 (rest incompat2)))
                  (setf (feature-incompatible-features n2) (list 1 n1))))
            nil)
           (t
            ;; add n-ary incompatibility constraint
            (dolist (x s)
              (push s (feature-nogoods x))
              (push s (feature-users-in-canonical-lists x)))
            (setf (gethash k table) nil))))
         (t
          (dolist (x s)
            (push s (feature-users-in-canonical-lists x)))
          (setf (gethash k table) (make-feature-combo s)))))))))

(defun characteristic-feature-restriction (l)
  ;; removes other features from feature list for which there are characteristic features
  ;; so that restricted list can be used as shorter nogood
  (remove-if (lambda (n1)
               (some (lambda (n2)
                       (and (not (eq n1 n2))
                            (let ((v (characteristic-feature-type n2)))
                              (and v (member n1 (feature-combo-list (second v)))))))
                     l))
             l))

(definline feature-union0 (s1 s2)
  ;; s1 and s2 are single features
  (cond
   ((eq s1 s2)
    s1)
   (t
    (let ((mins1 (feature-preorder-min s1))
          (mins2 (feature-preorder-min s2)))
      (cond
       ((< mins1 mins2)
        (cond
         ((<= mins2 (feature-preorder-max s1))		;(feature> s2 s1)
          s2)
         ((feature-incompatible0 s1 s2)
          nil)
         (t
          (list s1 s2))))
       (t ;(> mins2 mins1)
        (cond
         ((<= mins1 (feature-preorder-max s2))		;(feature> s1 s2)
          s1)
         ((feature-incompatible0 s1 s2)
          nil)
         (t
          (list s2 s1)))))))))

(definline feature-union1 (s1 s2)
  ;; s1 is single feature, s2 is nonempty list of features
  (cond
   ((feature-subsumes1 s1 s2)
    s2)
   ((null (setf s2 (remove s1 s2 :test #'feature>)))
    s1)
   ((feature-incompatible1 s1 s2)
    nil)
   (t
    (feature-merge1 s1 s2))))

(definline feature-union2 (s1 s2)
  ;; s1 and s2 are nonempty lists of features
  (cond
   ((null (setf s1 (feature-set-difference s1 s2 #'feature<=)))
    s2)
   ((null (setf s2 (feature-set-difference s2 s1 #'feature<)))
    s1)
   ((feature-incompatible2 s1 s2)
    nil)
   (t
    (feature-merge2 s1 s2))))

(defun feature-union (s1 s2 &optional (canonize t))
  ;; s1 and s2 are features or lists of compatible features sorted by feature-preorder-min
  ;; return their nonredundant union sorted by feature-preorder-min if compatible, nil if incompatible
  ;; handle bottom value too: return nil if s1 or s2 is nil
  (and s1
       s2
       (let ((v (if (or (consp s1) (feature-combo? s1))
                    (if (or (consp s2) (feature-combo? s2))
                        (feature-union2 (if (consp s1) s1 (feature-combo-list s1)) (if (consp s2) s2 (feature-combo-list s2)))
                        (feature-union1 s2 (if (consp s1) s1 (feature-combo-list s1))))
                    (if (or (consp s2) (feature-combo? s2))
                        (feature-union1 s1 (if (consp s2) s2 (feature-combo-list s2)))
                        (feature-union0 s1 s2)))))
         (cond
          ((atom v)
           v)
          ((null (rest v))
           (first v))
          ((and (feature-combo? s1) (eq (feature-combo-list s1) v))
           s1)
          ((and (feature-combo? s2) (eq (feature-combo-list s2) v))
           s2)
          ((not canonize)
           v)
          (t
           (feature-canonize v))))))

(defun feature-live? (s &optional action)
  ;; returns s if s is undeleted feature or list of undeleted features, nil otherwise
  (and s
       (if (feature-combo? s)
           (dolist (s (feature-combo-list s) t)
             (when (feature-deleted? s)
               (return (and action (funcall action "Feature ~A has been deleted." s)))))
           (or (not (feature-deleted? s))
               (and action (funcall action "Feature ~A has been deleted." s))))
       s))

(defun declare-features-incompatible (n1 n2 &rest more)
  (the-feature (list* n1 n2 more) 'error nil :incompatible))

(defun unthe-feature (x)
  ;; inverse of the-feature:
  ;; if x is composed of named features,
  ;; creates an expression such that (the-feature expr) = x
  (cond
   ((feature? x)
    (feature-name x))
   ((feature-combo? x)
    (or (let ((name (feature-combo-name x)))
          (and name (symbol-package name) name))	;don't return uninterned symbols created by feature-sym
        (let ((l nil))
          (dolist (x (characteristic-feature-restriction (feature-combo-list x)) (if (null (rest l)) (first l) (cons 'and (nreverse l))))
            (let ((v (characteristic-feature-type x)))
              (if (setf v (if v (feature-combo-name (second v)) (feature-name x)))
                  (setf l (cons v l))
                  (return nil)))))))
   (t
    nil)))

(defun feature-sym (x)
  (cond
   ((feature? x)
    (feature-name x))
   ((feature-combo? x)
    (or (feature-combo-name x)
        (let ((expr (unthe-feature x)))
          (if (atom expr) expr (setf (feature-combo-name x) (make-symbol (apply 'to-string (second expr) (mapcan #'(lambda (x) (list "&" x)) (rrest expr)))))))))
   (t
    nil)))

(defun print-feature3 (node stream depth)
  (declare (ignore depth))
  (let ((n node) (l nil))
    (loop
      (cond
       ((null n)
        (print-unreadable-object (node stream :type t :identity nil)
          (format stream "~S~{ ~S~}" (first l) (rest l)))
        (return))
       ((feature-name n)
        (if (null l)
            (format stream "~A" (feature-name n))
            (print-unreadable-object (node stream :type t :identity nil)
              (format stream "~S~{ ~S~}" (feature-name n) l)))
        (return))
       (t
        (push (feature-preorder-min n) l)
        (setf n (feature-parent n)))))))

(defun print-feature-combo3 (x stream depth)
  (declare (ignore depth))
  (let ((name (feature-sym x)))
    (if name
        (princ name stream)
        (print-unreadable-object (x stream :type t :identity nil)
          (format stream "~S~{ ~S~}" (first (feature-combo-list x)) (rest (feature-combo-list x)))))))

(defun print-feature (n)
  (prin1 (or (feature-name n) (feature-preorder-min n)))
  n)

(defun print-feature-list (l)
  (print-feature (first l))
  (dolist (x (rest l))
    (princ " and ")
    (print-feature x))
  l)

(defun print-feature-tree (&key node numbers)
  (labels
    ((print-node (n)
       (terpri)
       (when numbers
         (format t "[~9D,~9D] " (feature-preorder-min n) (feature-preorder-max n)))
       (let ((depth (if node (- (feature-depth n) (feature-depth node)) (feature-depth n))))
         (unless (eql 0 depth)
           (dotimes (i depth)
             (princ (if (eql 0 (mod i 5)) (if (eql 0 i) "   " "|  ") ":  ")))))
       (print-feature n)
       (when (feature-children-incompatible n)
         (princ ", with incompatible children"))
       (let ((incompat (feature-incompatible-features n)))
         (when (and incompat (< 0 (first incompat)))
           (princ ", incompatible with ")
           (print-feature-list (rest incompat))))         
       (dolist (child (feature-children n))
         (print-node child)))
     (print-defn (name defn)
       (terpri)
       (prin1 name)
       (princ " is defined as ")
       (cond
        ((feature-combo? defn)
         (princ "the conjunction of ")
         (print-feature-list (feature-combo-list defn)))
        (t
         (print-feature defn)))
       (princ ".")))
    (let ((tree *feature-tree*))
      (unless (or (null node) (feature? node))
        (let ((node* (and (can-be-feature-name node 'warn) (the-feature node 'warn))))
          (cond
           ((feature-combo? node*)
            (print-defn node node*)
            (return-from print-feature-tree))
           (t
            (setf node node*)))))
      (print-node (or node (feature-tree-root tree)))
      (let ((l nil))
        (maphash (lambda (k v)
                   (let ((s (feature-canonical-list-unkey k)))
                     (when (and (null v) (implies node (some (lambda (x) (feature<= node x)) s)))
                       (push s l))))
                 (feature-tree-canonical-lists tree))
        (when l
          (terpri)
          (dolist (k l)
            (terpri)
            (princ "The conjunction of ")
            (print-feature-list k)
            (princ " is incompatible."))))
      (let ((l nil))
        (maphash (lambda (name v)
                   (when (if (feature-combo? v)
                             (implies node (some (lambda (x) (feature<= node x)) (feature-combo-list v)))
                             (and (not (eq name (feature-name v))) (implies node (feature<= node v))))
                     (push (cons name v) l)))
                 (feature-tree-name-table tree))
        (when l
          (terpri)
          (dolist (v (sort l #'string< :key #'car))
            (print-defn (car v) (cdr v))))))))

;;; feature.lisp EOF
