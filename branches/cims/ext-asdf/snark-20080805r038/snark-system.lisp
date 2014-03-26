;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: common-lisp-user -*-
;;; File: snark-system.lisp
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

(in-package :common-lisp-user)

;;; load files from the same directory that this file was loaded from

(defparameter *snark-system-pathname* *load-truename*)

(defparameter *snark-files2*
  '("loads"
    "lisp-system"
    "deque-system"
    "sparse-array-system"
    "numbering-system"
    "agenda-system"
    "infix-reader-system"
    "feature-system"
    "dpll-system"
    "snark-pkg"))

(defparameter *snark-files*
  '("useful"
    "assoc-cache"
    "posets"
    "solve-sum"
    "globals"
    "options"
    "terms"
    "rows"
    "row-contexts"
    "constants"
    "functions"
    "variables"
    "subst"
    "substitute"
    "symbol-table"
    "symbol-definitions"
    "assertion-analysis"
    "jepd-relations-tables" "jepd-relations" "date-reasoning2"
    "constraints"
    "constraint-purify"
    "connectives"
    "wffs"
;;  "equality-elimination2"
    "nonhorn-magic-set"
    "dp-refute"
    "sorts-functions"
    "sorts-interface"
    "sorts"
    "argument-bag-ac"
    "argument-list-a1"
    "unify"
    "unify-bag" "subsume-bag"
    "unify-vector"
    "equal"
    "variant"
    "alists"
    "term-hash"
    "trie-index"
    "path-index"
    "term-memory"
;;  "instance-graph" "instance-graph2"
    "weight"
    "eval"
    "input"
    "output"
    "simplification-ordering"
    "symbol-ordering"
    "recursive-path-ordering" "ac-rpo"
    "knuth-bendix-ordering"
    "rewrite"
    "rewrite-code"
    "code-for-strings2"
    "code-for-numbers2"
    "code-for-lists2"
    "code-for-bags2"
    "resolve-code"
    "resolve-code-tables"
    "main"
    "subsume" "subsume-clause"
    "interactive"
    "assertion-file"
    "tptp"
    "tptp-symbols"
    "coder"
    ("examples" "overbeek-test")
    ("examples" "front-last-example")
    ("examples" "steamroller-example")
    ("examples" "reverse-example")
    ("examples" "hot-drink-example")
    ("examples" "coder-examples")
    ("examples" "latin-squares")
    "patches"
    ))

(defvar *compile-me* nil)

(defun make-snark-system (&optional (*compile-me* *compile-me*))
  (pushnew :snark *features*)
  #+cmu (setf extensions::*gc-verbose* nil)
  (when (eq *compile-me* :optimize)
    (proclaim (print '(optimize (safety 1) (space 1) (speed 3) (debug 1)))))
  (with-compilation-unit ()
    (dolist (name *snark-files2*)
      (let* ((dir (if (consp name)
                      (append (pathname-directory *snark-system-pathname*) (butlast name))
                      (append (pathname-directory *snark-system-pathname*) (list "src"))))
             (name (if (consp name) (first (last name)) name))
             (file (make-pathname :directory dir :name name :defaults *snark-system-pathname*)))
        (load file)))
    (setf *package* (find-package :snark))
    #+gcl (shadow '(#:assert #:substitute #:variable))
    (dolist (name *snark-files*)
      (let* ((dir (if (consp name)
                      (append (pathname-directory *snark-system-pathname*) (butlast name))
                      (append (pathname-directory *snark-system-pathname*) (list "src"))))
             (name (if (consp name) (first (last name)) name))
             (file (make-pathname :directory dir :name name :defaults *snark-system-pathname*)))
        (load (if *compile-me*
                  (compile-file file)
                  (or (probe-file (compile-file-pathname file)) file))))))
;;#-(or symbolics mcl) (load "/home/pacific1/stickel/spice/build.lisp")
  (setf *package* (find-package :snark-user))
  (setf *print-pretty* nil)
  #+openmcl (egc nil)
  (funcall (intern (symbol-name :initialize) :snark)))

#+ignore
(defun fix-snark-files ()
  (let ((dir (pathname-directory cl-user::*snark-system-pathname*)))
  (dolist (x (append
              (directory 
               (make-pathname :directory (append dir (list "src")) :name :wild :type "lisp"))
              (directory 
               (make-pathname :directory (append dir (list "Private")) :name :wild :type "lisp"))
              (directory 
               (make-pathname :directory (append dir (list "examples")) :name :wild :type "lisp"))
              (directory 
               (make-pathname :directory (append dir (list "examples")) :name :wild :type "kif"))))
    (ccl:set-mac-file-type x :text)
    (ccl:set-mac-file-creator x :ccl2))))

;;; snark-system.lisp EOF
