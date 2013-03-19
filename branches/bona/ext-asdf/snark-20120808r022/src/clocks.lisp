;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark-lisp -*-
;;; File: clocks.lisp
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

(in-package :snark-lisp)

(defvar *clocks* nil)

(defun make-clock-variable (name)
    (cl:assert (symbolp name))
    (let* ((s (symbol-name name))
	   (v (intern (to-string "*%" s :-time%*) :snark-lisp))
           (w (intern (to-string "*%" s :-count%*) :snark-lisp)))
      (unless (assoc v *clocks*)
	(setf *clocks* (nconc *clocks* (list (list v w))))
	(proclaim `(special ,v ,w)))
      (values v w)))

(mapc #'make-clock-variable
	'(
          read-assertion-file
          assert
          process-new-row
	  resolution
	  paramodulation
	  factoring
          equality-factoring
          embedding
          condensing
	  forward-subsumption
	  backward-subsumption
          clause-clause-subsumption
	  forward-simplification
	  backward-simplification
	  ordering
          ordering-ac
	  sortal-reasoning
          temporal-reasoning
	  constraint-simplification
	  term-hashing
	  path-indexing
          instance-graph-insertion
          purity-testing
          relevance-testing
          satisfiability-testing
	  printing
	  halted
	  test1
	  test2
	  test3
	  ))

(defvar *excluded-clocks* '(*%printing-time%* *%halted-time%*))

(defvar *running-clocks* nil)
(defvar *first-real-time-value* 0)
(defvar *first-run-time-value* 0)
(defvar *last-run-time-value* 0)
(defvar *run-time-mark* 0)
(declaim (type integer *first-real-time-value* *first-run-time-value* *last-run-time-value* *run-time-mark*))
(defvar *total-seconds* 0.0)

(defun initialize-clocks (&optional (excluded-clocks *excluded-clocks*))
  (cl:assert (null *running-clocks*))
  (setf *first-real-time-value* (get-internal-real-time))
  (setf *run-time-mark* (setf *first-run-time-value* (get-internal-run-time)))
  (setf *excluded-clocks* excluded-clocks)
  (dolist (l *clocks*)
    (dolist (v l)
      (setf (symbol-value v) 0))))

(defmacro with-clock-on (clock &body body)
  (let (count)
    (setf (values clock count) (make-clock-variable clock))
    (let ((previously-running-clocks (make-symbol (symbol-name 'previously-running-clocks)))
          (first-previously-running-clock (make-symbol (symbol-name 'first-previously-running-clock))))
      `(let* ((,previously-running-clocks *running-clocks*)
              (,first-previously-running-clock (first ,previously-running-clocks)))
         (unless (eq ',clock ,first-previously-running-clock)
           (if ,previously-running-clocks
               (decf (symbol-value ,first-previously-running-clock) (- *last-run-time-value* (setf *last-run-time-value* (get-internal-run-time))))
               (setf *last-run-time-value* (get-internal-run-time)))
           (incf (symbol-value ',count))
           (setf *running-clocks* (cons ',clock ,previously-running-clocks)))
         (unwind-protect
           (progn ,@body)
           (unless (eq ',clock ,first-previously-running-clock)
             (setf *running-clocks* ,previously-running-clocks)
             (decf (symbol-value ',clock) (- *last-run-time-value* (setf *last-run-time-value* (get-internal-run-time))))))))))

(defmacro with-clock-off (clock &body body)
  ;; dummy with-clock-on
  (make-clock-variable clock)
  `(progn ,@body))

(defun clock-name (clock)
  (let ((name (symbol-name clock)))
    (nsubstitute #\  #\- (subseq name 2 (- (length name) 7)))))

(defun print-clocks (&optional (excluded-clocks *excluded-clocks*))
  (let ((total-ticks (- (get-internal-run-time) *first-run-time-value*))
        (time-included 0)
        (time-excluded 0))
    (format t "~%; Run time in seconds")
    (dolist (l *clocks*)
      (let* ((clk (first l))
             (run-time (symbol-value clk)))
        (cond
         ((eql 0 run-time)
          )
         ((member clk excluded-clocks)
          (format t (if (eql 0 time-excluded) " excluding ~(~A~)" ", ~(~A~)") (clock-name clk))
          (incf time-excluded run-time))
         (t
          (incf time-included run-time)))))
    (unless (eql 0 time-excluded)
      (decf total-ticks time-excluded)
      (format t " time"))
    (princ ":")
    (dolist (l *clocks*)
      (let ((clk (first l))
            (cnt (second l)))
        (unless (member clk excluded-clocks)
	  (let ((run-time (symbol-value clk))
                (count (symbol-value cnt)))
	    (unless (eql 0 count)
              (format t "~%;~10,3F ~3D%   ~@(~A~)~48T(~:D call~:P)"
                      (/ run-time (float internal-time-units-per-second))
                      (if (eql 0 total-ticks) 0 (percentage run-time total-ticks))
                      (clock-name clk)
                      count))))))
    (let ((other-time (- total-ticks time-included)))
      (format t "~%;~10,3F ~3D%   Other"
              (/ other-time (float internal-time-units-per-second))
              (if (eql 0 total-ticks) 0 (percentage other-time total-ticks))))
    (setf *total-seconds* (/ total-ticks (float internal-time-units-per-second)))
    (format t "~%;~10,3F        Total" *total-seconds*)
    (format t "~%;~10,3F        Real time" (/ (- (get-internal-real-time) *first-real-time-value*) (float internal-time-units-per-second)))
    *total-seconds*))

(defun total-run-time (&optional (excluded-clocks *excluded-clocks*))
  (let ((total-ticks (- (get-internal-run-time) *first-run-time-value*)))
    (dolist (l *clocks*)
      (let ((clk (first l)))
        (when (member clk excluded-clocks)
          (decf total-ticks (symbol-value clk)))))
    (/ total-ticks (float internal-time-units-per-second))))

(defun print-incremental-time-used ()
  (let ((time (get-internal-run-time)))
    (format t "     ;~,3Fsec" (/ (- time *run-time-mark*) (float internal-time-units-per-second)))
    (setf *run-time-mark* time)))

;;; clocks.lisp EOF
