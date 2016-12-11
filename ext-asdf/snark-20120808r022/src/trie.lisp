;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: trie.lisp
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

;;; trie indexed by list of integers

(defmacro make-trie-node ()
  `(cons nil nil))

(defmacro trie-node-data (node)
  `(car ,node))

(defmacro trie-node-branches (node)
  `(cdr ,node))

(defstruct (trie
            (:copier nil))
  (top-node (make-trie-node) :read-only t)
  (node-counter (make-counter 1) :read-only t))

(defun trieref (trie keys)
  (do ((keys keys (rest keys))
       (node (trie-top-node trie) (let ((b (trie-node-branches node)))
                                    (if b (sparef b (first keys)) nil))))
      ((or (null node) (null keys))
       (if node (trie-node-data node) nil))))

(defun (setf trieref) (data trie keys)
  (if data
      (do ((keys keys (rest keys))
           (node (trie-top-node trie) (let ((b (trie-node-branches node))
                                            (key (first keys)))
                                        (if b
                                            (or (sparef b key)
                                                (setf (sparef b key)
                                                      (progn (increment-counter (trie-node-counter trie)) (make-trie-node))))
                                            (setf (sparef (setf (trie-node-branches node) (make-sparse-vector)) key)
                                                  (progn (increment-counter (trie-node-counter trie)) (make-trie-node)))))))
          ((null keys)
           (setf (trie-node-data node) data)))
      (labels
        ((trie-delete (node keys)
           ;; return t to delete this node from parent when data and branches are both empty
           (cond
            ((null keys)
             (setf (trie-node-data node) nil)
             (null (trie-node-branches node)))
            (t
             (let ((b (trie-node-branches node)))
               (when b
                 (let* ((key (first keys))
                        (node1 (sparef b key)))
                   (when (and node1 (trie-delete node1 (rest keys)))
                     (decrement-counter (trie-node-counter trie))
                     (if (= 1 (sparse-vector-count b))
                         (progn (setf (trie-node-branches node) nil) (null (trie-node-data node)))
                         (setf (sparef b key) nil))))))))))
        (trie-delete (trie-top-node trie) keys)
        nil)))

(defun trie-size (trie &optional count-only-data-nodes?)
  (labels
    ((ts (node)
       (let ((size (if (and count-only-data-nodes? (null (trie-node-data node))) 0 1)))
         (prog-> 
           (trie-node-branches node ->nonnil b)
           (map-sparse-vector b ->* node)
           (setf size (+ size (trie-size node count-only-data-nodes?))))
         size)))
    (ts (trie-top-node trie))))

(defun map-trie (function trie-or-node)
  (labels
    ((mt (node)
       (let ((d (trie-node-data node)))
         (when d
           (funcall function d)))
       (let ((b (trie-node-branches node)))
         (when b
           (map-sparse-vector #'mt b)))))
    (declare (dynamic-extent #'mt))
    (mt (if (trie-p trie-or-node) (trie-top-node trie-or-node) trie-or-node))))

;;; trie.lisp EOF
