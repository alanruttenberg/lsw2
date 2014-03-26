;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark-lisp -*-
;;; File: pattern-match.lisp
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

(in-package :snark-lisp)

(defun pattern-match (pat expr &optional alist)
  ;; matches pat to expr creating bindings in alist for ?vars in pat
  ;; sublis can be used to make instances of other expressions that contain ?vars
  ;; (nil) is used as value for successful match with no bindings
  (cond
   ((consp pat)
    (and (consp expr)
         (setf alist (pattern-match (car pat) (car expr) alist))
         (pattern-match (cdr pat) (cdr expr) alist)))
   ((and pat (symbolp pat) (eql #\? (char (symbol-name pat) 0)))
    (cond
     ((null (first alist))
      (acons pat expr nil))
     (t
      (let ((v (assoc pat alist)))
        (if v
            (if (equal (cdr v) expr) alist nil)
            (acons pat expr alist))))))
   ((eql pat expr)
    (or alist '(nil)))
   (t
    nil)))

;;; pattern-match.lisp EOF
