;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: solve-sum.lisp
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

(in-package :snark)

(defun solve-sum (cc sum coefs &optional bounds)
  ;; find xi such that 0 <= xi <= boundi and coef1*x1 + ... + coefn*xn = sum
  ;; sum >= 0, each coefi > 0, each boundi >= 0, all integers
  ;; |coefs| = |bounds| > 0 (bounds can also be nil)
  ;; applies cc to each solution
  ;; returns nil if unsolvable due to bounds
  ;; (solve-sum #'print 29 '(1 5 10 25) '(4 3))
  ;; prints how to make 29 cents using at most 4 pennies and 3 nickels
  (cond
   ((eql 0 sum)
    (funcall cc nil)		;use nil instead of final zeroes
    t)
   (t
    (let ((c (pop coefs))
          (b (pop bounds)))
      (cond
       ((null coefs)
        (mvlet (((values q r) (truncate sum c)))
          (when (or (null b) (>= b q))
            (when (eql 0 r)
              (funcall cc (list q)))
            t)))
       ((eql 0 b)
        (solve-sum (lambda (sol) (funcall cc (cons 0 sol))) sum coefs bounds))
       (t
        (let* ((k (if b (min b (truncate sum c)) (truncate sum c)))
               (k1 k))
          (decf sum (* k1 c))
          (loop
            (cond
             ((solve-sum (lambda (sol) (funcall cc (cons k1 sol))) sum coefs bounds)
              (cond
               ((eql 0 k1)
                (return t))
               (t
                (decf k1)
                (incf sum c))))
             (t
              (return (neql k k1))))))))))))

(defun solve-sum-p (sum coefs &optional bounds)
  (or (eql 0 sum)
      (and (null bounds)
	   (member 1 coefs))
      (block it
	(solve-sum (lambda (sol)
                     (declare (ignore sol))
                     (return-from it t))
		   sum coefs bounds)
	nil)))

(defun solve-sum-solutions (sum coefs &optional bounds)
  (cond
    ;; handle some frequent special cases first
    ;; (solve-sum-solutions 1 '(1)) => ((1))
    ((and (eql 1 sum)
	  (null (rest coefs)))
     (and (eql 1 (first coefs))
	  (neql 0 (first bounds))
	  '((1))))
    ;; (solve-sum-solutions 1 '(1 1)) => ((1) (0 1))
    ((and (eql 1 sum)
	  (null (rrest coefs))
	  (eql 1 (first coefs))
	  (neql 0 (first bounds))
	  (eql 1 (second coefs))
	  (neql 0 (second bounds)))
     '((1) (0 1)))
    (t
     (let ((sols nil) sols-last)
       (solve-sum (lambda (sol) (collect sol sols)) sum coefs bounds)
       sols))))

;;; solve-sum.lisp EOF
