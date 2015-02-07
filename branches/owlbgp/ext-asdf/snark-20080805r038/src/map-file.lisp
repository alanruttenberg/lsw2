;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark-lisp -*-
;;; File: map-file.lisp
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

(defun mapnconc-file-forms (function filespec &key (if-does-not-exist :error) (package *package*))
  ;; apply function to each form in file and return the result of nconc'ing the values
  (with-open-file (stream filespec :direction :input :if-does-not-exist if-does-not-exist)
    (when stream
      (mapnconc-stream-forms function stream :package package))))

(defun mapnconc-file-lines (function filespec &key (if-does-not-exist :error) (package *package*))
  ;; apply function to each line in file and return the result of nconc'ing the values
  (with-open-file (stream filespec :direction :input :if-does-not-exist if-does-not-exist)
    (when stream
      (mapnconc-stream-lines function stream :package package))))

(defun mapnconc-stream-forms (function stream &key (package *package*))
  ;; apply function to each form in stream and return the result of nconc'ing the values
  (prog->
    (find-or-make-package package -> *package*)
    (mapnconc-stream0 stream #'read ->* form)
    (cond
     ((and (consp form) (eq 'in-package (first form)))
      (eval form)
      nil)
     ((or (null function) (eq 'list function) (eq #'list function))
      (list form))
     (t
      (funcall function form)))))

(defun mapnconc-stream-lines (function stream &key (package *package*))
  ;; apply function to each line in stream and return the result of nconc'ing the values
  (prog->
    (find-or-make-package package -> *package*)
    (mapnconc-stream0 stream #'read-line ->* line)
    (cond
     ((or (null function) (eq 'list function) (eq #'list function))
      (list line))
     (t
      (funcall function line)))))

(defun mapnconc-stream0 (function stream read-function)
  (let ((eof (cons nil nil))
        (result nil) result-last)
    (loop
      (let ((x (funcall read-function stream nil eof)))
        (if (eq eof x)
            (return result)
            (ncollect (funcall function x) result))))))

(defun read-file (filespec &rest mapnconc-file-forms-options)
  (declare (dynamic-extent mapnconc-file-forms-options))
  (apply #'mapnconc-file-forms nil filespec mapnconc-file-forms-options))

(defun read-file-lines (filespec &rest mapnconc-file-lines-options)
  (declare (dynamic-extent mapnconc-file-lines-options))
  (apply #'mapnconc-file-lines nil filespec mapnconc-file-lines-options))

(defun read-file-to-string (filespec &key (if-does-not-exist :error))
  (with-open-file (stream filespec :direction :input :if-does-not-exist if-does-not-exist)
    (with-output-to-string (string)
      (loop
        (let ((ch (read-char stream nil :eof)))
          (if (eq :eof ch)
              (return string)
              (write-char ch string)))))))

;;; map-file.lisp EOF
