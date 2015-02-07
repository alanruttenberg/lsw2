;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: date-reasoning2.lisp
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

(in-package :snark)

;;; $$date-point and $$date-interval are external (solely for user convenience) function symbols
;;; for date points and intervals; they are replaced by $$utime-point and $$utime-interval when
;;; formulas are input
;;;
;;; $$utime-point and $$utime-interval are internal function symbols for dates
;;; they use Lisp universal time representation (which counts seconds since 1900-01-01T00:00:00)
;;;
;;; $$date-point and $$date-interval use 1 to 6 integer arguments
;;;   year, month, day, hour, minute, second
;;; to specify dates
;;;
;;; examples of SNARK dates and their translations:
;;; ($$date-point 2002 4 1 16 27 20)                       -> ($$utime-point 3226667240)
;;; ($$date-interval 2002 4 1 16 34)                       -> ($$utime-interval 3226667640 3226667700)
;;; ($$date-interval 2002 4 1 16 34 :until 2002 4 1 16 35) -> ($$utime-interval 3226667640 3226667700)
;;; ($$date-interval 2002 4 1 16 34 :until 2002 4 1 17)    -> ($$utime-interval 3226667640 3226669200)
;;;
;;; 20071215: avoid use of $$date-interval (and $$utime-interval)
;;; reasoning is more complete and effective if just $$date-point (and $$utime-point) are used

(defvar *date-point*)
(defvar *utime-point*)
(defvar *date-interval*)
(defvar *utime-interval*)

(defun declare-code-for-dates ()
  ;; declare symbols without some properties here
  ;; defer full definition until declare-time-relations is called
  (setf *date-point* (declare-function1 '$$date-point :any :macro t :input-code 'input-date-point))
  (setf *utime-point* (declare-function
                       '$$utime-point 1
                       :constructor t
;;                     :index-type :hash-but-dont-index
                       :to-lisp-code 'utime-point-term-to-lisp))
  (setf *date-interval* (declare-function1 '$$date-interval :any :macro t :input-code 'input-date-interval))
  (setf *utime-interval* (declare-function
                          '$$utime-interval 2
                          :constructor t
;;                        :index-type :hash-but-dont-index
                          :to-lisp-code 'utime-interval-term-to-lisp))
  nil)

(defun can-be-date-p (list &optional action)
  ;; a proper date is a list of 1 to 6 integers with appropriate values
  ;; interpreted as year, month, day, hour, minute, and second
  (or (let* ((list list)
             (year (pop list)))
        (and (integerp year)
             (<= 1900 year)
             (implies
              list
              (let ((month (pop list)))
                (and (integerp month)
                     (<= 1 month 12)
                     (implies
                      list
                      (let ((day (pop list)))
                        (and (integerp day)
                             (<= 1 day (days-per-month month year))
                             (implies
                              list
                              (let ((hour (pop list)))
                                (and (integerp hour)
                                     (<= 0 hour 23)
                                     (implies
                                      list
                                      (let ((minute (pop list)))
                                        (and (integerp minute)
                                             (<= 0 minute 59)
                                             (implies
                                              list
                                              (let ((second (pop list)))
                                                (and (integerp second)
                                                     (<= 0 second 59)	;no leap seconds!
                                                     (null list))))))))))))))))))
      (and action (funcall action "~A cannot be a date." list))))

(defun encode-universal-time-point (year &optional month day hour minute second)
  (can-be-date-p (list year (or month 1) (or day 1) (or hour 0) (or minute 0) (or second 0)) 'error)
  (encode-universal-time
   (or second 0)
   (or minute 0)
   (or hour 0)
   (or day 1)
   (or month 1)
   year
   0))

(defun decode-universal-time-point (universal-time-point)
  (mvlet (((values second minute hour day month year)
           (decode-universal-time universal-time-point 0)))
    (cond
     ((/= 0 second)
      (list year month day hour minute second))
     ((/= 0 minute)
      (list year month day hour minute))
     ((/= 0 hour)
      (list year month day hour))
     ((/= 1 day)
      (list year month day))
     ((/= 1 month)
      (list year month))
     (t
      (list year)))))

(defun encode-universal-time-interval (year &optional month day hour minute second)
  (let ((v (encode-universal-time-point year month day hour minute second)))
    (list v
          (+ v (or (and second 1)					;1 second long interval
                   (and minute 60)					;1 minute long interval
                   (and hour 3600)					;1 hour long interval
                   (and day 86400)					;1 day long interval
                   (and month (* (days-per-month month year) 86400))	;1 month long interval
                   (* (if (leap-year-p year) 366 365) 86400))))))	;1 year long interval

(defun decode-universal-time-interval (universal-time-interval)
  (mvlet (((list start finish) universal-time-interval))
    (values (decode-universal-time-point start) (decode-universal-time-point finish))))

(defun pp-compare-universal-times (point1 point2)
  (cond
   ((< point1 point2)
    'p<p)
   ((> point1 point2)
    'p>p)
   (t
    'p=p)))

(defun ii-compare-universal-times (interval1 interval2)
  (mvlet (((list start1 finish1) interval1)
          ((list start2 finish2) interval2))
    (cond
     ((= start1 start2)
      (if (< finish1 finish2) 's (if (> finish1 finish2) 'si '=)))
     ((= finish1 finish2)
      (if (> start1 start2) 'f 'fi))
     ((<= finish1 start2)
      (if (= finish1 start2) 'm '<))
     ((>= start1 finish2)
      (if (= start1 finish2) 'mi '>))
     ((< start1 start2)
      (if (> finish1 finish2) 'di 'o))
     (t
      (if (< finish1 finish2) 'd 'oi)))))

(defun pi-compare-universal-times (point interval)
  (mvlet (((list start finish) interval))
    (cond
     ((<= point start)
      (if (= point start) 'p_s_i 'p<i))
     ((>= point finish)
      (if (= point finish) 'p_f_i 'p>i))
     (t
      'p_d_i))))

(defun declare-date-functions (&key intervals points)
  (when points
    (declare-function1 '$$utime-point 1 :sort (list (time-point-sort-name?))))
  (when intervals
    (declare-function1 '$$utime-interval 2 :sort (list (time-interval-sort-name?))))
  (when points
    (declare-relation1 '$$time-pp 3 :locked nil :rewrite-code 'time-pp-atom-rewriter-for-dates)
    (declare-utime-pp-composition))
  (when intervals
    (declare-relation1 '$$time-ii 3 :locked nil :rewrite-code 'time-ii-atom-rewriter-for-dates))
  (when (and points intervals)
    (declare-relation1 '$$time-pi 3 :locked nil :rewrite-code 'time-pi-atom-rewriter-for-dates)
    (declare-utime-pi-composition))
  nil)
  
(defun input-date-point (head args polarity)
  (declare (ignore head polarity))
  (make-compound *utime-point* (declare-constant (apply 'encode-universal-time-point args))))

(defun input-date-interval (head args polarity)
  (declare (ignore head polarity))
  (let (v start finish)
    (cond
     ((setf v (member :until args))
      (setf start (apply 'encode-universal-time-point (ldiff args v)))
      (setf finish (apply 'encode-universal-time-point (rest v)))
      (cl:assert (< start finish)))
     (t
      (setf v (apply 'encode-universal-time-interval args))
      (setf start (first v))
      (setf finish (second v))))
    (declare-constant start)
    (declare-constant finish)
    (make-compound *utime-interval* start finish)))

(defun utime-point-term-to-lisp (head args subst)
  (declare (ignore head))
  (or (let ((arg1 (first args)))
        (and (dereference arg1 subst :if-constant (integerp arg1))
             (cons (function-name *date-point*)
                   (decode-universal-time-point arg1))))
      none))

(defun utime-interval-term-to-lisp (head args subst)
  (declare (ignore head))
  (or (let ((arg1 (first args))
            (arg2 (second args)))
        (and (dereference arg1 subst :if-constant (integerp arg1))
             (dereference arg2 subst :if-constant (integerp arg2))
             (cons (function-name *date-interval*)
                   (append (decode-universal-time-point arg1)
                           (cons :until (decode-universal-time-point arg2))))))
      none))

(defun utime-point-term-p (term subst)
  (dereference
   term subst
   :if-compound-appl (and (eq *utime-point* (heada term))
                          (let* ((args (argsa term))
                                 (arg1 (first args)))
                            (and (dereference arg1 subst :if-constant (integerp arg1))
                                 arg1)))))

(defun utime-interval-term-p (term subst)
  (dereference
   term subst
   :if-compound-appl (and (eq *utime-interval* (heada term))
                          (let* ((args (argsa term))
                                 (arg1 (first args))
                                 (arg2 (second args)))
                            (and (dereference arg1 subst :if-constant (integerp arg1))
                                 (dereference arg2 subst :if-constant (integerp arg2))
                                 (if (and (eql arg1 (first args))
                                          (eql arg2 (second args)))
                                     args
                                     (list arg1 arg2)))))))

(defun time-ii-atom-rewriter-for-dates (term subst)
  (let ((args (args term)) m n v)
    (cond
     ((and (setf m (utime-interval-term-p (first args) subst))
           (setf n (utime-interval-term-p (second args) subst))
           (progn (setf v (third args)) (dereference v subst :if-compound-cons t)))
      (setf v (nth (jepd-relation-code (ii-compare-universal-times m n) $time-ii-relation-code) v))
      (if (dereference v subst :if-variable t) false true))
     (t
      none))))

(defun time-pp-atom-rewriter-for-dates (term subst)
  (let ((args (args term)) m n v)
    (cond
     ((and (setf m (utime-point-term-p (first args) subst))
           (setf n (utime-point-term-p (second args) subst))
           (progn (setf v (third args)) (dereference v subst :if-compound-cons t)))
      (setf v (nth (jepd-relation-code (pp-compare-universal-times m n) $time-pp-relation-code) v))
      (if (dereference v subst :if-variable t) false true))
     (t
      none))))

(defun time-pi-atom-rewriter-for-dates (term subst)
  (let ((args (args term)) m n v)
    (cond
     ((and (setf m (utime-point-term-p (first args) subst))
           (setf n (utime-interval-term-p (second args) subst))
           (progn (setf v (third args)) (dereference v subst :if-compound-cons t)))
      (setf v (nth (jepd-relation-code (pi-compare-universal-times m n) $time-pi-relation-code) v))
      (if (dereference v subst :if-variable t) false true))
     (t
      none))))

(defun declare-utime-pp-composition ()
  ;; use relations between x&z and z&y to constrain relation between x&y where x and z are utimes and y is a point
  (declare-relation1
   '$$utime-pp-composition
   5
   :rewrite-code
   (list
    (lambda (atom subst)
      (let ((args (args atom)) m n)
        (or (and (setf m (utime-point-term-p (third args) subst))
                 (setf n (utime-point-term-p (fifth args) subst))
                 (if (/= m n)
                     (make-compound
                      (input-relation-symbol '$$time-pp-composition 5)
                      (if (< m n)
                          (list 1 (make-and-freeze-variable) (make-and-freeze-variable))
                          (list (make-and-freeze-variable) (make-and-freeze-variable) 1))
                      (second (args atom))
                      (third (args atom))
                      (fifth (args atom))
                      (fourth (args atom)))
                     true))
            none)))))
  (assert `(forall (?x (?y :sort ,(time-point-sort-name?)) ?z ?l1 ?l2)
                   (implies (and ($$time-pp ($$utime-point ?x) ?y ?l1)
                                 ($$time-pp ($$utime-point ?z) ?y ?l2))
                            ($$utime-pp-composition ?l1 ?l2 ($$utime-point ?x) ?y ($$utime-point ?z))))
          :name :$$utime-pp-composition
          :supported nil))

(defun declare-utime-pi-composition ()
  ;; use relations between x&z and z&y to constrain relation between x&y where x and z are utimes and y is an interval
  (declare-relation1
   '$$utime-pi-composition
   5
   :rewrite-code
   (list
    (lambda (atom subst)
      (let ((args (args atom)) m n)
        (or (and (setf m (utime-point-term-p (third args) subst))
                 (setf n (utime-point-term-p (fifth args) subst))
                 (if (/= m n)
                     (make-compound
                      (input-relation-symbol '$$time-pi-pp-composition 5)
                      (if (< m n)
                          (list 1 (make-and-freeze-variable) (make-and-freeze-variable))
                          (list (make-and-freeze-variable) (make-and-freeze-variable) 1))
                      (second (args atom))
                      (third (args atom))
                      (fifth (args atom))
                      (fourth (args atom)))
                     true))
            none)))))
  (assert `(forall (?x (?y :sort ,(time-interval-sort-name?)) ?z ?l1 ?l2)
                   (implies (and ($$time-pi ($$utime-point ?x) ?y ?l1)
                                 ($$time-pi ($$utime-point ?z) ?y ?l2))
                            ($$utime-pi-composition ?l1 ?l2 ($$utime-point ?x) ?y ($$utime-point ?z))))
          :name :$$utime-pi-composition
          :supported nil))

;;; date-reasoning2.lisp EOF
