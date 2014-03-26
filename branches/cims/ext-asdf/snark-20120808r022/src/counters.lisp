;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark-lisp -*-
;;; File: counters.lisp
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
;;; Portions created by the Initial Developer are Copyright (C) 1981-2010.
;;; All Rights Reserved.
;;;
;;; Contributor(s): Mark E. Stickel <stickel@ai.sri.com>.

(in-package :snark-lisp)

(defstruct (counter
            (:constructor make-counter (&optional (increments 0)))
            (:copier nil))
  (increments 0 :type integer)
  (decrements 0 :type integer)
  (previous-peak-value 0 :type integer))

(defun increment-counter (counter &optional (n 1))
  (declare (type integer n))
;;(cl:assert (<= 0 n))
  (incf (counter-increments counter) n)
  nil)

(defun decrement-counter (counter &optional (n 1))
  (declare (type integer n))
;;(cl:assert (<= 0 n))
  (let* ((d (counter-decrements counter))
         (v (- (counter-increments counter) d)))
    (when (> v (counter-previous-peak-value counter))
      (setf (counter-previous-peak-value counter) v))
    (setf (counter-decrements counter) (+ d n))
    nil))

(defun counter-value (counter)
  (- (counter-increments counter) (counter-decrements counter)))

(defun counter-values (counter)
  ;; returns 4 values: current value, peak value, #increments, #decrements
  (let* ((i (counter-increments counter))
         (d (counter-decrements counter))
         (v (- i d)))
  (values v (max v (counter-previous-peak-value counter)) i d)))

(definline show-count-p (n)
  (dolist (v '(1000000 100000 10000 1000 100 10) t)
    (when (>= n v)
      (return (eql 0 (rem n v))))))

(defun show-count (n)
  (princ #\Space)
  (let (q r)
    (cond
     ((eql 0 n)
      (princ 0))
     ((progn (setf (values q r) (truncate n 1000000)) (eql 0 r))
      (princ q) (princ #\M))
     ((progn (setf (values q r) (truncate n 1000)) (eql 0 r))
      (princ q) (princ #\K))
     (t
      (princ n))))
  (princ #\Space)
  (force-output)
  n)

(defun show-count0 (n)
  (if (and (neql 0 n) (show-count-p n)) n (show-count n)))

(defun show-count1 (n)
  (if (show-count-p n) (show-count n) n))

(defmacro princf (place &optional (delta 1))
  ;; increment counter and maybe print it
  ;; if delta is 0, print the counter unless the previous increment did
  (cl:assert (member delta '(0 1)))
  (if (eql 0 delta)
      `(show-count0 ,place)
      `(show-count1 (incf ,place))))

;;; counters.lisp EOF
