;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark-deque -*-
;;; File: deque.lisp
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

(in-package :snark-deque)

;;; doubly-linked list implementation of double-ended queue
;;;
;;; all operations are constant-time except:
;;;   deque-length
;;;   deque-contains
;;;   deque-delete
;;;   deque-delete-if
;;;   mapnconc-deque

(defmacro make-deque-node (value prev next)
  `(cons ,value (cons ,prev ,next)))

(defmacro deque-node-value (n)
  `(car ,n))

(defmacro deque-node-ptrs (n)
  `(cdr ,n))

(defmacro deque-prev-ptr (p)
  `(car ,p))

(defmacro deque-next-ptr (p)
  `(cdr ,p))

(defmacro deque-prev-node (n)
  `(deque-prev-ptr (deque-node-ptrs ,n)))

(defmacro deque-next-node (n)
  `(deque-next-ptr (deque-node-ptrs ,n)))

(definline deque? (x)
  (and (consp x) (eq 'deque (deque-node-value x))))

(definline deque-insert-after-node (prev-node value)
  (let ((next-node (deque-next-node prev-node)))
    (setf (deque-prev-node next-node) (setf (deque-next-node prev-node) (make-deque-node value prev-node next-node)))))

(definline deque-insert-before-node (next-node value)
  (let ((prev-node (deque-prev-node next-node)))
    (setf (deque-prev-node next-node) (setf (deque-next-node prev-node) (make-deque-node value prev-node next-node)))))

(definline deque-delete-node (node)
  (when node
;;  (cl:assert (not (eq 'deque (deque-node-value node))))	;can't delete initial node
    (let ((ptrs (deque-node-ptrs node)))
      (when ptrs				;guard against double deletion
        (setf (deque-node-ptrs node) nil)
        (let ((next-node (deque-next-ptr ptrs))
              (prev-node (deque-prev-ptr ptrs)))
          (setf (deque-next-node prev-node) next-node)
          (setf (deque-prev-node next-node) prev-node)
          t)))))

(defun make-deque ()
  (let ((start-end-node (make-deque-node 'deque nil nil)))
    (setf (deque-prev-node start-end-node) (setf (deque-next-node start-end-node) start-end-node))))

(definline deque-empty? (deque)
  (eq deque (deque-next-node deque)))

(definline deque-push-first (deque value)
  ;; returns the value-containing node inserted at the front end of deque
  (deque-insert-after-node deque value))

(definline deque-push-last (deque value)
  ;; returns the value-containing node inserted at the back end of deque
  (deque-insert-before-node deque value))

(definline deque-pop-first (deque)
  ;; removes and returns the value stored at the front end of deque
  ;; returns nil if deque is empty
  (let ((n (deque-next-node deque)))
    (and (not (eq deque n)) (progn (deque-delete-node n) (deque-node-value n)))))

(definline deque-pop-last (deque)
  ;; removes and returns the value stored at the back end of deque
  ;; returns nil if deque is empty
  (let ((n (deque-prev-node deque)))
    (and (not (eq deque n)) (progn (deque-delete-node n) (deque-node-value n)))))

(definline deque-first (deque)
  ;; returns the value stored at the front end of deque
  ;; returns nil if deque is empty
  (let ((n (deque-next-node deque)))
    (and (not (eq deque n)) (deque-node-value n))))

(definline deque-last (deque)
  ;; returns the value stored at the back end of deque
  ;; returns nil if deque is empty
  (let ((n (deque-prev-node deque)))
    (and (not (eq deque n)) (deque-node-value n))))

(defun deque-length (deque)
  (let ((len 0)
        (n deque))
    (loop
      (cond
       ((eq deque (setf n (deque-next-node n)))
        (return len))
       (t
        (incf len))))))

(defun deque-contains (deque value)
  (let ((n deque))
    (loop
      (cond
       ((eq deque (setf n (deque-next-node n)))
        (return nil))
       ((eql value (deque-node-value n))
        (return n))))))

(defun deque-delete (deque value)
  (deque-delete-node (deque-contains deque value)))

(defun deque-delete-if (function deque &key reverse)
  (let ((n (if reverse (deque-prev-node deque) (deque-next-node deque))))
    (loop
      (cond
       ((eq deque n)
        (return nil))
       (t
        (let ((n1 n))
          (setf n (if reverse (deque-prev-node n) (deque-next-node n)))
          (when (funcall function (deque-node-value n1))
            (deque-delete-node n1))))))))

(defun mapnconc-deque (function deque &key reverse)
  (let ((result nil) result-last
        (n (if reverse (deque-prev-node deque) (deque-next-node deque))))
    (loop
      (cond
       ((eq deque n)
        (return result))
       (t
        (let ((v (deque-node-value n)))
          (setf n (if reverse (deque-prev-node n) (deque-next-node n)))
          (cond
           ((or (null function) (eq 'list function) (eq #'list function))
            (collect v result))
           (t
            (ncollect (funcall function v) result)))))))))

;;; deque.lisp EOF
