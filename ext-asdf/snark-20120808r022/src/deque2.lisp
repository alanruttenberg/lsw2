;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark-deque -*-
;;; File: deque2.lisp
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

(in-package :snark-deque)

(defstruct (deque
            (:predicate deque?))
  (front nil :type list)
  (last-of-front nil)
  (rear nil :type list)
  (last-of-rear nil))

(defun deque-empty? (deque)
  (and (null (deque-front deque)) (null (deque-rear deque))))

(defun deque-first (deque)
  ;; returns first item in deque, nil if deque is empty
  (let ((front (deque-front deque)))
    (if front (first front) (deque-last-of-rear deque))))

(defun deque-last (deque)
  ;; returns last item in deque, nil if deque is empty
  (let ((rear (deque-rear deque)))
    (if rear (first rear) (deque-last-of-front deque))))

(defun deque-rest (deque)
  ;; returns new deque with first item removed, deque if it is empty
  (let ((front (deque-front deque))
        (rear (deque-rear deque)))
    (cond
     (front
      (let ((front* (rest front)))
        (make-deque
         :front front*
         :last-of-front (if front* (deque-last-of-front deque) nil)
         :rear rear
         :last-of-rear (deque-last-of-rear deque))))
     (rear
      (let ((front* (rest (reverse rear))))
        (make-deque
         :front front*
         :last-of-front (if front* (first rear) nil)
         :rear nil
         :last-of-rear nil)))
     (t
      deque))))

(defun deque-butlast (deque)
  ;; returns new deque with last item removed, deque if it is empty
  (let ((front (deque-front deque))
        (rear (deque-rear deque)))
    (cond
     (rear
      (let ((rear* (rest rear)))
        (make-deque
         :rear rear*
         :last-of-rear (if rear* (deque-last-of-rear deque) nil)
         :front front
         :last-of-front (deque-last-of-front deque))))
     (front
      (let ((rear* (rest (reverse front))))
        (make-deque
         :rear rear*
         :last-of-rear (if rear* (first front) nil)
         :front nil
         :last-of-front nil)))
     (t
      deque))))

(defun deque-pop-first (deque)
  ;; like deque-rest, but return first item and destructively remove it from deque
  (let ((front (deque-front deque))
        (rear (deque-rear deque)))
    (cond
     (front
      (let ((front* (rest front)))
        (setf (deque-front deque) front*)
        (when (null front*)
          (setf (deque-last-of-front deque) nil))
        (first front)))
     (rear
      (let ((item (deque-last-of-rear deque))
            (front* (rest (reverse rear))))
        (setf (deque-front deque) front*)
        (setf (deque-last-of-front deque) (if front* (first rear) nil))
        (setf (deque-rear deque) nil)
        (setf (deque-last-of-rear deque) nil)
        item))
     (t
      nil))))

(defun deque-pop-last (deque)
  ;; like deque-butlast, but return last item and destructively remove it from deque
  (let ((front (deque-front deque))
        (rear (deque-rear deque)))
    (cond
     (rear
      (let ((rear* (rest rear)))
        (setf (deque-rear deque) rear*)
        (when (null rear*)
          (setf (deque-last-of-rear deque) nil))
        (first rear)))
     (front
      (let ((item (deque-last-of-front deque))
            (rear* (rest (reverse front))))
        (setf (deque-rear deque) rear*)
        (setf (deque-last-of-rear deque) (if rear* (first front) nil))
        (setf (deque-front deque) nil)
        (setf (deque-last-of-front deque) nil)
        item))
     (t
      nil))))

(defun deque-add-first (deque item)
  ;; returns new deque with new first item added
  (let ((front (deque-front deque)))
    (make-deque
     :front (cons item front)
     :last-of-front (if front (deque-last-of-front deque) item)
     :rear (deque-rear deque)
     :last-of-rear (deque-last-of-rear deque))))

(defun deque-add-last (deque item)
  ;; returns new deque with new last item added
  (let ((rear (deque-rear deque)))
    (make-deque
     :rear (cons item rear)
     :last-of-rear (if rear (deque-last-of-rear deque) item)
     :front (deque-front deque)
     :last-of-front (deque-last-of-front deque))))

(defun deque-push-first (deque item)
  ;; like deque-add-first, but returns same deque with new first item added destructively
  (let ((front (deque-front deque)))
    (setf (deque-front deque) (cons item front))
    (when (null front)
      (setf (deque-last-of-front deque) item))
    deque))

(defun deque-push-last (deque item)
  ;; like deque-add-last, but returns same deque with new last item added destructively
  (let ((rear (deque-rear deque)))
    (setf (deque-rear deque) (cons item rear))
    (when (null rear)
      (setf (deque-last-of-rear deque) item))
    deque))

(defun deque-length (deque)
  (+ (length (deque-front deque)) (length (deque-rear deque))))

(defun deque-delete (deque item)
  ;; ad hoc function to delete single occurrence of item from deque destructively
  (let ((front (deque-front deque))
        (rear (deque-rear deque)))
    (cond
     ((and front (eql item (first front)))
      (when (null (setf (deque-front deque) (rest front)))
        (setf (deque-last-of-front deque) nil))
      t)
     ((and rear (eql item (first rear)))
      (when (null (setf (deque-rear deque) (rest rear)))
        (setf (deque-last-of-rear deque) nil))
      t)
     ((dotails (l front nil)
        (when (and (rest l) (eql item (second l)))
          (when (null (setf (rest l) (rrest l)))
            (setf (deque-last-of-front deque) (first l)))
          (return t))))
     ((dotails (l rear nil)
        (when (and (rest l) (eql item (second l)))
          (when (null (setf (rest l) (rrest l)))
            (setf (deque-last-of-rear deque) (first l)))
          (return t))))
     (t
      nil))))

(defun deque-delete-if (function deque)
  ;; ad hoc function to delete items from deque destructively
  (let* ((deleted nil)
         (front* (prog->
                   (delete-if (deque-front deque) ->* item)
                   (when (funcall function item)
                     (setf deleted t)))))
    (when deleted
      (setf (deque-front deque) front*)
      (setf (deque-last-of-front deque) (first (last front*)))))
  (let* ((deleted nil)
         (rear* (prog->
                  (delete-if (deque-rear deque) :from-end t ->* item)
                  (when (funcall function item)
                    (setf deleted t)))))
    (when deleted
      (setf (deque-rear deque) rear*)
      (setf (deque-last-of-rear deque) (first (last rear*)))))
  deque)

(defun mapnconc-deque (function deque &key reverse)
  ;; ad hoc function to nconc results of applying function to items in deque
  (let ((front (deque-front deque))
        (rear (deque-rear deque))
        (result nil) result-last)
    (dolist (item (if reverse rear front))
      (if (or (null function) (eq 'list function) (eq #'list function))
          (collect item result)
          (ncollect (funcall function item) result)))
    (dolist (item (if reverse (reverse front) (reverse rear)))
      (if (or (null function) (eq 'list function) (eq #'list function))
          (collect item result)
          (ncollect (funcall function item) result)))
    result))

;;; deque2.lisp EOF
