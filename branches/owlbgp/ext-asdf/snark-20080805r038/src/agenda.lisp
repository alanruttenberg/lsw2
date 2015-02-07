;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark-agenda -*-
;;; File: agenda.lisp
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
;;; Portions created by the Initial Developer are Copyright (C) 1981-2008.
;;; All Rights Reserved.
;;;
;;; Contributor(s): Mark E. Stickel <stickel@ai.sri.com>.

(in-package :snark-agenda)

(defstruct (agenda
            (:print-function print-agenda3)
            (:copier nil))
  (name "" :read-only t)
  (length 0)
  (length-limit nil)
  (length-limit-deletion-action #'identity :read-only t)
  (same-item-p #'eql :read-only t)
  (buckets (make-sparse-vector)))

;;; an agenda index value (priority) is (list integer_1 ... integer_n) or (list* integer_1 ... integer_n)
;;; which are both treated as the same sequence integer_1 ... integer_n
;;; this includes (list* integer) = integer as an agenda index value
;;; agenda index values are compared lexicographically in left-to-right order
;;; if one is prefix of another, they must be equal, e.g., can't have (2 18) and (2 18 1)
;;; agenda buckets are deques stored in nested sparse-vectors indexed by agenda index values

(defun find-agenda-bucket (buckets value &optional create)
  (labels
    ((find-agenda-bucket* (buckets value)
       (cond
        ((atom value)
         (or (sparef buckets value)
             (if create (setf (sparef buckets value) (make-deque)) nil)))
        ((null (rest value))
         (or (sparef buckets (first value))
             (if create (setf (sparef buckets (first value)) (make-deque)) nil)))
        (t
         (let ((v (sparef buckets (first value))))
           (cond
            (v
             (find-agenda-bucket* v (rest value)))
            (create
             (find-agenda-bucket* (setf (sparef buckets (first value)) (make-sparse-vector)) (rest value)))
            (t
             nil)))))))
    (find-agenda-bucket* buckets value)))

(defun first-or-last-nonempty-agenda-bucket (buckets last)
  (labels
    ((first-or-last-nonempty-agenda-bucket* (buckets)
       (prog->
         (map-sparse-vector-with-indexes buckets :reverse last ->* x i)
         (cond
          ((sparse-vector-p x)
           (first-or-last-nonempty-agenda-bucket* x))
          ((deque-empty? x)
           (setf (sparef buckets i) nil))
          (t
           (return-from first-or-last-nonempty-agenda-bucket x))))))
    (first-or-last-nonempty-agenda-bucket* buckets)
    nil))

(definline first-nonempty-agenda-bucket (buckets)
  (first-or-last-nonempty-agenda-bucket buckets nil))

(definline last-nonempty-agenda-bucket (buckets)
  (first-or-last-nonempty-agenda-bucket buckets t))

(defun collect-agenda-buckets (buckets)
  (let ((result nil) result-last)
    (labels
      ((collect-agenda-buckets* (buckets revalue)
         (prog->
           (map-sparse-vector-with-indexes buckets ->* x i)
           (cond
            ((sparse-vector-p x)
             (collect-agenda-buckets* x (cons i revalue)))
            ((deque-empty? x)
             )
            (t
             (collect (list x (if (null revalue) i (reverse (cons i revalue)))) result))))))
      (collect-agenda-buckets* buckets nil)
      result)))

(defun agenda-insert (item value agenda &optional at-front)
  (let* ((buckets (agenda-buckets agenda))
         (q (find-agenda-bucket buckets value :create)))
    (unless (and (not (deque-empty? q)) (funcall (agenda-same-item-p agenda) item (if at-front (deque-first q) (deque-last q))))
      (if at-front (deque-push-first q item) (deque-push-last q item))
      (let ((limit (agenda-length-limit agenda))
            (length (agenda-length agenda)))
        (cond
         ((and limit (<= limit length))
          (let ((deleted-item (deque-pop-last (last-nonempty-agenda-bucket buckets))))
            (cond
             ((eql item deleted-item)
              nil)
             (t
              (funcall (agenda-length-limit-deletion-action agenda) deleted-item)
              t))))
         (t
          (setf (agenda-length agenda) (+ length 1))
          t))))))

(defun agenda-delete (item value agenda)
  (let ((length (agenda-length agenda)))
    (unless (eql 0 length)
      (let ((q (find-agenda-bucket (agenda-buckets agenda) value)))
        (when (and q (deque-delete q item))
          (setf (agenda-length agenda) (- length 1))
          t)))))

(defun agenda-first (agenda &optional delete)
  (cond
   ((listp agenda)
    (dolist (agenda agenda)
      (unless (eql 0 (agenda-length agenda))
        (return (agenda-first agenda delete)))))
   (t
    (let ((length (agenda-length agenda)))
      (unless (eql 0 length)
        (let ((q (first-nonempty-agenda-bucket (agenda-buckets agenda))))
          (cond
           (delete
            (setf (agenda-length agenda) (- length 1))
            (deque-pop-first q))
           (t
            (deque-first q)))))))))

(defun pop-agenda (agenda)
  (agenda-first agenda t))

(defun map-agenda-buckets (function buckets)
  (prog->
    (map-sparse-vector buckets ->* x)
    (cond
     ((sparse-vector-p x)
      (map-agenda-buckets function x))
     (t
      (funcall function x)))))

(defun mapnconc-agenda (function agenda)
  (let ((result nil) result-last)
    (prog->
      (map-agenda-buckets (agenda-buckets agenda) ->* q)
      (mapnconc-deque q ->* item)
      (cond
       ((or (null function) (eq 'list function) (eq #'list function))
        (collect item result))
       (t
        (ncollect (funcall function item) result))))))

(defun agenda-delete-if (function agenda &optional apply-length-limit-deletion-action)
  (prog->
    (and apply-length-limit-deletion-action (agenda-length-limit-deletion-action agenda) -> deletion-action)
    (map-agenda-buckets (agenda-buckets agenda) ->* q)
    (deque-delete-if q ->* v)
    (when (funcall function v)
      (decf (agenda-length agenda))
      (when deletion-action
        (funcall deletion-action v))
      t)))

(defun limit-agenda-length (agenda limit)
  (let ((length (agenda-length agenda)))
    (setf (agenda-length-limit agenda) limit)
    (when (and limit (< limit length))
      (let ((i 0))
        (agenda-delete-if (lambda (item) (declare (ignore item)) (> (incf i) limit)) agenda t)))))

(defvar *agenda*)		;default agenda(s) for print-agenda to display

(defun print-agenda (&key (agenda *agenda*) entries)
  (cond
   ((listp agenda)
    (let ((all-empty t))
      (dolist (agenda agenda)
        (unless (eql 0 (agenda-length agenda))
          (setf all-empty nil)
          (print-agenda :agenda agenda :entries entries)))
      (when all-empty
        (format t "~%; All agendas are empty."))))
   (t
    (with-standard-io-syntax2
      (format t "~%; The agenda of ~A has ~D entr~:@P~A"
              (agenda-name agenda)
              (agenda-length agenda)
              (if (eql 0 (agenda-length agenda)) "." ":"))
      (unless (eql 0 (agenda-length agenda))
        (let ((buckets (collect-agenda-buckets (agenda-buckets agenda))))
          (do* ((k (length buckets))
                (k1 (ceiling k 3))
                (k2 (ceiling (- k k1) 2))
                (buckets3 (nthcdr (+ k1 k2) buckets))
                (buckets2 (nbutlast (nthcdr k1 buckets) (- k k1 k2)))
                (buckets1 (nbutlast buckets k2))
                b)
               ((null buckets1))
            (setf b (pop buckets1))
            (format t "~%; ~5D with value ~A" (deque-length (first b)) (second b))
            (unless (null buckets2)
              (setf b (pop buckets2))
              (format t "~31T~5D with value ~A" (deque-length (first b)) (second b))
              (unless (null buckets3)
                (setf b (pop buckets3))
                (format t "~61T~5D with value ~A" (deque-length (first b)) (second b))))))
        (when (and entries (not (eql 0 (agenda-length agenda))))
          (prog->
            (dolist (collect-agenda-buckets (agenda-buckets agenda)) ->* x)
            (first x -> q)
            (second x -> value)
            (unless (deque-empty? q)
              (format t "~%;~%; Entries with value ~A:" value)
              (mapnconc-deque (lambda (x) (format t "~%; ~A" x)) q))))))
    nil)))

(defun print-agenda3 (agenda stream depth)
  (declare (ignore depth))
  (print-unreadable-object (agenda stream :type t :identity nil)
    (format stream "~S" (agenda-name agenda))))

;;; agenda.lisp EOF
