;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark-lisp -*-
;;; File: topological-sort.lisp
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
;;; Portions created by the Initial Developer are Copyright (C) 1981-2006.
;;; All Rights Reserved.
;;;
;;; Contributor(s): Mark E. Stickel <stickel@ai.sri.com>.

(in-package :snark-lisp)

(defun topological-sort* (items map-predecessors)
  ;; see Cormen, Leiserson, Rivest text
  ;; (funcall map-predecessors cc u items) iterates over u in items
  ;; that must occur before v and executes (funcall cc u)
  ;; note: also eliminates EQL duplicates
  (let ((color (make-hash-table))
	(result nil) result-last)
    (labels
      ((dfs-visit (v)
         (when (eq :white (gethash v color :white))
           (setf (gethash v color) :gray)
           (funcall map-predecessors #'dfs-visit v items)
           (collect v result))))
      (loop
        (if (null items)
            (return result)
            (dfs-visit (pop items)))))))

(defun topological-sort (items must-precede-predicate)
  (topological-sort*
   items
   (lambda (cc v items)
     (mapc (lambda (u)
             (when (and (neql u v) (funcall must-precede-predicate u v))
               (funcall cc u)))
           items))))

#+ignore
(defun test-topological-sort* ()
  (topological-sort*
   '(belt jacket pants shirt shoes socks tie undershorts watch)
   (lambda (cc v items)
     (declare (ignore items))
     (dolist (x '((undershorts . pants)
                  (undershorts . shoes)
                  (pants . belt)
                  (pants . shoes)
                  (belt . jacket)
                  (shirt . belt)
                  (shirt . tie)
                  (tie . jacket)
                  (socks . shoes)))
       (when (eql v (cdr x))
         (funcall cc (car x)))))))

#+ignore
(defun test-topological-sort ()
  (topological-sort
   '(belt jacket pants shirt shoes socks tie undershorts watch)
   (lambda (u v)
     (member v
             (cdr (assoc u
                         '((undershorts pants shoes)
                           (pants belt shoes)
                           (belt jacket)
                           (shirt belt tie)
                           (tie jacket)
                           (socks shoes))))))))

;;; topological-sort.lisp EOF
