;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: assertion-file.lisp
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

(defmacro in-language (language)
  (declare (ignore language))
  `(warn "Ignoring IN-LANGUAGE form."))

(defmacro in-kb (kb)
  ;; use suspend/resume for this?  okbc calls?
  (declare (ignore kb))
  `(warn "Ignoring IN-KB form."))

(defmacro has-author (author)
  `(setf *form-author* ',author))

(defmacro has-documentation (documentation)
  `(setf *form-documentation* ',documentation))

(defmacro has-name (name)
  `(setf *form-name* ',name))

(defmacro has-source (source)
  `(setf *form-source* ',source))

(declare-snark-option assertion-file-commands
                      '(assertion
                        has-author	;has-xxx specifies xxx for later assertions
                        has-documentation
                        has-name
                        has-source
                        in-package
                        in-language
                        in-kb
                        declare-constant
                        declare-function
                        declare-relation
                        declare-sort
                        declare-subsort
                        declare-sorts-incompatible
                        )		;every other form is an assertion
                      :never-print)

(declare-snark-option assertion-file-keywords
                      '((:author *form-author*)
                        (:documentation *form-documentation*)
                        (:name *form-name*)
                        (:source *form-source*))
                      :never-print)

(declare-snark-option assertion-file-format nil :never-print)
(declare-snark-option assertion-file-if-does-not-exist :error :never-print)
(declare-snark-option assertion-file-verbose nil :never-print)
(declare-snark-option assertion-file-package :snark-user :never-print)
(declare-snark-option assertion-file-readtable nil :never-print)
(declare-snark-option assertion-file-negate-conjectures nil :never-print)

(defun read-assertion-file (filespec
                            &key
                            (format (assertion-file-format?))
                            (if-does-not-exist (assertion-file-if-does-not-exist?))
                            (verbose (assertion-file-verbose?))
                            (package (or (assertion-file-package?) *package*))
                            (readtable (or (assertion-file-readtable?) *readtable*))
                            (negate-conjectures (assertion-file-negate-conjectures?))
                            hash-dollar
                            (clock t))
  ;; read-asssertion-file executes commands and return a list of calls on 'assertion'
  ;; every form that is not a command (commands are named in (assertion-file-commands?))
  ;; is treated as a formula to be asserted
  (declare (ignorable verbose hash-dollar))
  (let ((sort-declarations nil)
        (subsort-declarations nil))
    (labels
      ((raf0 ()
         (prog->
           (identity readtable -> *readtable*)
           (identity (assertion-file-commands?) -> commands)
           (identity (assertion-file-keywords?) -> keywords)
           (progv (mapcar #'second keywords)
                  (consn nil nil (length keywords))
             (funcall (cond
                       ((eq :tptp format)
                        'mapnconc-tptp-file-forms)
                       (t
                        'mapnconc-file-forms))
                      filespec
                      :if-does-not-exist if-does-not-exist
                      :package package
                      ->* form)
             (and (consp form)
                  (symbolp (first form))
                  (first (member (first form) commands
                                 :test #'string-equal		;command matching ignores package and case
                                 :key #'symbol-name))
                  -> command)
             (case command
               ((nil)
                (setf form (list 'assertion form)))
               (assertion
                (setf form (cons command (append (rest form) nil)))
                (setf command nil))
               (declare-sort
                (setf form (cons command (rest form)))
                (push form sort-declarations))
               (declare-subsort
                (setf form (cons command (rest form)))
                (push form subsort-declarations))
               ((declare-sorts-incompatible declare-constant declare-function declare-relation)
                (setf form (cons command (rest form)))
                (setf command nil))
               (otherwise
                (eval (cons command (rest form)))))
             (unless command
               (case (and (consp form) (first form))
                 (assertion
                  (cond
                   ((getf (cddr form) :ignore)
                    nil)
                   (t
                    (when (and negate-conjectures (eq 'conjecture (getf (cddr form) :reason)))
                      (setf (second form) (list 'not (second form)))
                      (setf (getf (cddr form) :reason) 'negated_conjecture))
                    (dolist (x keywords)
                      (let ((v (symbol-value (second x))))
                        (when (and v (eq none (getf (cddr form) (first x) none)))
                          (nconc form (list (first x) v)))))
                    (list form))))
                 (otherwise
                  (list form)))))))
       (raf ()
         (let ((l (raf0)))
           (cond
            (subsort-declarations
             (setf subsort-declarations (topological-sort (nreverse subsort-declarations) 'must-precede-in-assertion-file))
             (setf l (append subsort-declarations l))
             (dolist (x sort-declarations)
               (unless (member (unquote (second x)) subsort-declarations :key #'(lambda (x) (unquote (second x))))
                 (push x l))))
            (t
             (dolist (x sort-declarations)
               (push x l))))
           l)))
      (if clock
          (with-clock-on read-assertion-file (raf))
          (raf)))))

(defun must-precede-in-assertion-file (x y)
  (ecase (first x)
    ((declare-sort declare-subsort)
     (ecase (first y)
       ((declare-sort declare-subsort)
        (leafp (unquote (second x)) y))
       ((declare-sorts-incompatible declare-constant declare-function declare-relation declare-proposition assertion)
        t)))
    (declare-sorts-incompatible
     (ecase (first y)
       ((declare-sort declare-subsort declare-sorts-incompatible)
        nil)
       ((declare-constant declare-function declare-relation declare-proposition assertion)
        t)))
    ((declare-constant declare-function declare-relation declare-proposition)
     (eq 'assertion (first y)))
    (assertion
     nil)))

(declare-snark-option refute-file-initialize t :never-print)
(declare-snark-option refute-file-closure t :never-print)
(declare-snark-option refute-file-options nil :never-print)
(declare-snark-option refute-file-actions nil :never-print)
(declare-snark-option refute-file-ignore-errors nil :never-print)
(declare-snark-option refute-file-verbose t :never-print)
(declare-snark-option refute-file-output-file nil :never-print)
(declare-snark-option refute-file-if-exists nil :never-print)

(defun refute-file (filespec
                    &key
                    (initialize (refute-file-initialize?))
                    (closure (refute-file-closure?))
                    (format (assertion-file-format?))
		    (options (refute-file-options?))
                    (actions (refute-file-actions?))
                    (ignore-errors (refute-file-ignore-errors?))
		    (verbose (refute-file-verbose?))
                    (output-file (refute-file-output-file?))
                    (if-exists (refute-file-if-exists?))
                    (package (or (assertion-file-package?) *package*))
                    (readtable (or (assertion-file-readtable?) *readtable*)))
  (labels
    ((refute-file0 ()
       (when initialize
         (initialize))
       (mapc #'eval options)
       (mapc #'eval (funcall 'read-assertion-file filespec
                             :format format
                             :package package
                             :readtable readtable))
       (mapc #'eval actions)
       (when closure
         (or (closure) :done)))
     (refute-file1 ()
       (if verbose
           (print (time (refute-file0)))
           (refute-file0)))
     (refute-file2 ()
       (prog2
        (when verbose
          (format t "~&; Begin refute-file ~A " filespec) (print-current-time) (terpri))
        (if ignore-errors
            (mvlet (((:values value condition) (ignore-errors (refute-file1))))
              (or value (princ condition)))
            (refute-file1))
        (when verbose
          (format t "~&; End refute-file ~A "   filespec) (print-current-time) (terpri)))))
    (if output-file
        (with-open-file (stream output-file :direction :output :if-exists if-exists)
          (when stream
            (let ((*standard-output* stream) (*error-output* stream) (*trace-output* stream))
              (refute-file2))))
        (refute-file2))))

;;; assertion-file.lisp EOF
