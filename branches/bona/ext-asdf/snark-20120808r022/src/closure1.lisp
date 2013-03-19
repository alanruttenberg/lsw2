;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: closure1.lisp
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

;;; simple closure algorithm for small deduction tasks
;;; that do not require features like indexing for performance

(defun closure1 (items &key done unary-rules binary-rules ternary-rules (subsumption-test #'equal))
  ;; compute closure of the union of items and done using rules and subsumption-test
  ;; if done is given as an argument, it is assumed to be closed already
  (flet ((unsubsumed (l1 l2 subsumption-test)
           ;; return items in l2 that are not subsumed by any item in l1
           (delete-if #'(lambda (item2)
                          (some #'(lambda (item1)
                                    (funcall subsumption-test item1 item2))
                                l1))
                      l2)))
    (let ((todo (make-deque)))
      (dolist (item items)
        (deque-push-last todo item))
      (loop
        (when (deque-empty? todo)
          (return done))
        (let ((item1 (deque-pop-first todo)))
          (when (unsubsumed done (list item1) subsumption-test)
            (setf done (cons item1 (unsubsumed (list item1) done subsumption-test)))
            (prog->
              (dolist unary-rules ->* rule)
              (funcall rule item1 ->* new-item)
              (when (eq :inconsistent new-item)
                (return-from closure1 new-item))
              (deque-push-last todo new-item))
            (prog->
              (dolist binary-rules ->* rule)
              (dolist done ->* item2)
              (funcall rule item1 item2 ->* new-item)
              (when (eq :inconsistent new-item)
                (return-from closure1 new-item))
              (deque-push-last todo new-item))
            (prog->
              (dolist ternary-rules ->* rule)
              (dolist done ->* item2)
              (dolist done ->* item3)
              (funcall rule item1 item2 item3 ->* new-item)
              (when (eq :inconsistent new-item)
                (return-from closure1 new-item))
              (deque-push-last todo new-item))))))))

;;; closure1.lisp EOF
