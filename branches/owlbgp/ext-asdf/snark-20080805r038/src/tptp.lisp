;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: tptp.lisp
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

;;; TSTP justifications are incomplete:
;;;   cnf and other transformations aren't named
;;;   use of AC, other theories aren't named
;;;   constraints aren't shown

(defun print-row-in-tptp-format (row)
  (let ((wff (row-wff row)))
    (dolist (x (row-constraints row))
      (when (member (car x) '(arithmetic equality))
        (unless (eq true (cdr x))
          (setf wff (make-reverse-implication wff (cdr x))))))
    (print-wff-in-tptp-format1 wff (row-name-or-number row) (row-reason row) (row-source row))
    row))

(defun print-wff-in-tptp-format1 (wff name-or-number reason source)
  (let ((vars (variables wff)))
    (cond
     ((some #'(lambda (var) (not (top-sort? (variable-sort var)))) vars)
      (let ((*renumber-ignore-sort* t))
        (setf wff (renumber (make-compound *forall* (mapcar #'(lambda (var) (list var (tptp-sort-name (variable-sort var)))) (reverse vars)) wff))))
      (princ "tff("))
     ((not (unsorted-p wff))
      (princ "tff("))
     ((clause-p wff nil t)
      (princ "cnf("))
     (t
      (princ "fof("))))
  (print-row-name-or-number-in-tptp-format name-or-number)
  (princ ", ")
  (print-row-reason-in-tptp-format reason)
  (princ ",")
  (terpri)
  (princ "    ")
  (print-wff-in-tptp-format wff)
  (let ((v (print-row-reason-in-tptp-format2 reason)))
    (print-row-source-in-tptp-format source v))
  (princ ").")
  wff)

(defun print-row-reason-in-tptp-format (reason)
  (princ (case reason
           (assertion "axiom")
           (assumption "hypothesis")
           (conjecture "conjecture")
           (negated_conjecture "negated_conjecture")
           (hint "hint")
           (otherwise "plain"))))

(defun print-row-name-or-number-in-tptp-format (name-or-number)
  (print-symbol-in-tptp-format name-or-number))

(defun print-row-reason-in-tptp-format2 (reason)
  (case reason
    ((assertion assumption conjecture negated_conjecture hint nil)
     nil)
    (otherwise
     (princ ",")
     (terpri)
     (princ "    ")
     (print-row-reason-in-tptp-format3 reason)
     t)))

(defun print-row-reason-in-tptp-format3 (x)
  (cond
   ((consp x)
    (princ "inference(")
    (cond
     ((eq 'paramodulate (first x))
      (setf x (append x '(|theory(equality)|))))
     ((eq 'rewrite (first x))
      (cond
       ((member :code-for-= (rrest x))
        (setf x (append (remove :code-for-= x) '(|theory(equality)|))))
       ((some (lambda (row) (and (row-p row) (compound-p (row-wff row)) (eq *=* (head (row-wff row))))) (rrest x))
        (setf x (append x '(|theory(equality)|)))))))
    (print-symbol-in-tptp-format (first x))
    (princ ",")
    (princ "[status(thm)]")
    (princ ",")
    (princ "[")
    (let ((first t))
      (dolist (arg (rest x))
        (if first (setf first nil) (princ ","))
        (print-row-reason-in-tptp-format3 arg)))
    (princ "]")
    (princ ")"))
   ((row-p x)
    (print-row-name-or-number-in-tptp-format (row-name-or-number x)))
   ((or (eq '|theory(equality)| x) (eq :code-for-= x))
    (princ '|theory(equality)|))
   (t
    (print-symbol-in-tptp-format x))))

(defun print-row-source-in-tptp-format (source &optional list)
  ;; "file('foo.tptp',ax1)"  or  (|file| |foo.tptp| |ax1|)
  (when source
    (cond
     ((and (stringp source) (< 6 (length source)) (string= "file(" source :end2 4))
      (princ ",")
      (terpri)
      (princ (if list "    [" "    "))
      (princ source)
      (when list (princ "]")))
     ((and (consp source) (eq '|file| (first source)) (<= 2 (length source) 3))
      (princ ",")
      (terpri)
      (princ (if list "    [" "    "))
      (princ "file(")
      (print-symbol-in-tptp-format (second source))
      (when (rrest source)
        (princ ",")
        (print-symbol-in-tptp-format (third source)))
      (princ ")")
      (when list (princ "]")))))
  source)

(defun print-wff-in-tptp-format (wff &optional subst)
  (dereference
   wff subst
   :if-variable (print-term-in-tptp-format wff)
   :if-constant (cond
                 ((eq true wff)
                  (princ "$true"))
                 ((eq false wff)
                  (princ "$false"))
                 (t
                  (print-term-in-tptp-format wff)))
   :if-compound (cond
                 ((equality-p wff)
                  (print-term-in-tptp-format (arg1 wff) subst) (princ " = ") (print-term-in-tptp-format (arg2 wff) subst))
                 ((negation-p wff)
                  (let ((wff (arg1 wff)))
                    (dereference wff subst)
                    (cond
                     ((equality-p wff)
                      (print-term-in-tptp-format (arg1 wff) subst) (princ " != ") (print-term-in-tptp-format (arg2 wff) subst))
                     (t
                      (princ "~ ") (print-wff-in-tptp-format wff subst)))))
                 ((disjunction-p wff)
                  (princ "(") (print-wffs-in-tptp-format (args wff) subst " | ") (princ ")"))
                 ((conjunction-p wff)
                  (princ "(") (print-wffs-in-tptp-format (args wff) subst " & ") (princ ")"))
                 ((equivalence-p wff)
                  (princ "(") (print-wffs-in-tptp-format (args wff) subst " <=> ") (princ ")"))
                 ((exclusive-or-p wff)
                  (princ "(") (print-wffs-in-tptp-format (args wff) subst " <~> ") (princ ")"))
                 ((implication-p wff)
                  (princ "(") (print-wffs-in-tptp-format (args wff) subst " => ") (princ ")"))
                 ((reverse-implication-p wff)
                  (princ "(") (print-wffs-in-tptp-format (args wff) subst " <= ") (princ ")"))
                 ((universal-quantification-p wff)
                  (princ "(! ") (print-varspecs (arg1 wff)) (princ " : ") (print-wff-in-tptp-format (arg2 wff) subst) (princ ")"))
                 ((existential-quantification-p wff)
                  (princ "(? ") (print-varspecs (arg1 wff)) (princ " : ") (print-wff-in-tptp-format (arg2 wff) subst) (princ ")"))
                 (t
                  (print-term-in-tptp-format wff subst))))
  wff)

(defun print-wffs-in-tptp-format (wffs subst sep)
  (let ((first t))
    (dolist (wff wffs)
      (if first (setf first nil) (princ sep))
      (print-wff-in-tptp-format wff subst))))

(defun tptp-function-name (fn)
  ;; if symbol begins with $$, return an alias if it is lower case and begins with $
  (let* ((name (function-name fn))
         (s (symbol-name name)))
    (or (and (< 2 (length s))
             (eql #\$ (char s 1))
             (eql #\$ (char s 0))
             (some #'(lambda (alias)
                       (let ((s (symbol-name alias)))
                         (and (< 1 (length s))
                              (eql #\$ (char s 0))
                              (neql #\$ (char s 1))
                              (notany #'upper-case-p s)
                              alias)))
                   (symbol-aliases fn)))
        name)))

(defun print-term-in-tptp-format (term &optional subst)
  (dereference
   term subst
   :if-variable (progn
                  (cl:assert (top-sort? (variable-sort term)))
                  (mvlet (((values i j) (floor (variable-number term) 6)))
                    (princ (char "XYZUVW" j))
                    (unless (= 0 i)
                      (write i :radix nil :base 10))))
   :if-constant (print-symbol-in-tptp-format (constant-name term))
   :if-compound (let ((head (head term)))
                  (cond
                   ((eq *cons* head)
                    (princ "[")
                    (print-list-in-tptp-format term subst)
                    (princ "]"))
                   (t
                    (print-symbol-in-tptp-format (tptp-function-name head))
                    (princ "(")
                    (print-list-in-tptp-format (args (unflatten-term1 term subst)) subst)
                    (princ ")")))))
  term)

(defun print-varspecs (l)
  (princ "[")
  (let ((first t))
    (dolist (x l)
      (if first (setf first nil) (princ ", "))
      (cond
       ((variable-p x)
        (print-term-in-tptp-format x))
       (t
        (print-term-in-tptp-format (first x))
        (princ ": ")
        (print-term-in-tptp-format (second x))))))
  (princ "]"))          

(defun print-list-in-tptp-format (l subst)
  (let ((first t))
    (loop
      (cond
       ((dereference l subst :if-compound-cons t)
        (if first (setf first nil) (princ ","))
        (print-term-in-tptp-format (car l) subst)
        (setf l (cdr l)))
       ((null l)
        (return))
       (t
        (princ "|")
        (print-term-in-tptp-format l subst)
        (return))))))

(defun quote-tptp-symbol? (x)
  ;; returns t (or :escape) if symbol must be quoted as in 'a=b'
  ;; returns :escape if some characters must be escaped as in 'a\'b'
  ;; returns nil for <lower_word>, <dollar><lower_word>, <dollar><dollar><lower_word>
  (and (symbolp x)
       (let* ((string (symbol-name x))
              (len (length string)))
         (or (= 0 len)
             (let ((quote nil)
                   (dollar nil))
               (dotimes (i len (or quote dollar))
                 (let ((ch (char string i)))
                   (cond
                    ((or (eql #\' ch) (eql #\\ ch))
                     (return :escape))
                    ((= 0 i)
                     (if (eql #\$ ch)
                         (setf dollar t)
                         (setf quote (not (lower-case-p ch)))))
                    (dollar
                     (unless (and (= 1 i) (eql #\$ ch))
                       (setf dollar nil)
                       (setf quote (not (lower-case-p ch)))))
                    ((not quote)
                     (setf quote (not (or (alphanumericp ch) (eql #\_ ch)))))))))))))

(defun print-symbol-in-tptp-format (x)
  (etypecase x
    (symbol
     (let ((quote (quote-tptp-symbol? x)))
       (when quote
         (princ #\'))
       (if (eq :escape quote)
           (map nil
                (lambda (ch)
                  (when (or (eq #\\ ch) (eq #\' ch))
                    (princ #\\))
                  (princ ch))
                (symbol-name x))
           (princ x))
       (when quote
         (princ #\')))
     x)
    (number
     (write x :radix nil :base 10))
    (string
     (prin1 x))))

(defun tptp-sort-name (sort)
  (let ((name (sort-name sort)))
    (case name
      (integer '|$int|)
      (rational '|$rat|)
      (real '|$real|)
      (otherwise name))))

(defvar *tptp-environment-variable*
  #-MCL "/Users/mark/tptp"
  #+MCL "Ame:Users:mark:tptp")

(defun tptp-include-file-name (filename filespec)
  ;; filename is file name argument of an include directive
  ;; filespec specifies the file that contains the include directive
  (or (let (pathname)
        (cond
         ((and (setf pathname (merge-pathnames (string filename) filespec))
               (probe-file pathname))
          pathname)
         ((and *tptp-environment-variable*
               (setf pathname (merge-pathnames (concatenate 'string *tptp-environment-variable* #-MCL "/" #+MCL ":" (string filename)) filespec))
               (probe-file pathname))
          pathname)))
      ;; as backup, use this older ad hoc code for TPTP/Problems & TPTP/Axioms directory structure
      (let ((revdir (reverse (pathname-directory filespec))) v)
        (cond
         ((setf v (member "Problems" revdir :test #'string-equal))
          (setf revdir (rest v)))
         ((setf v (member-if #'(lambda (x) (and (stringp x) (<= 4 (length x)) (string-equal "TPTP" x :end2 4))) revdir))
          (setf revdir v)))
        (setf filename (string filename))
        (loop
          (let ((pos (position-if #'(lambda (ch) (or (eq '#\/ ch) (eq '#\: ch))) filename)))
            (cond
             ((null pos)
              (return))
             (t
              (setf revdir (cons (subseq filename 0 pos) revdir))
              (setf filename (subseq filename (+ pos 1)))))))
        (make-pathname
         :directory (nreverse revdir)
         :name (pathname-name filename)
         :type (pathname-type filename)))))

(defun tptp-file-source-string (filename &optional (name none))
  (if (eq none name)
      (list '|file| filename)
      (list '|file| filename name)))

(defun mapnconc-tptp-file-forms (function filespec &key (if-does-not-exist :error) (package *package*))
  (let ((*package* (find-or-make-package package))
        (snark-infix-reader::*infix-operators* snark-infix-reader::*infix-operators*)
        (snark-infix-reader::*prefix-operators* snark-infix-reader::*prefix-operators*)
        (snark-infix-reader::*postfix-operators* snark-infix-reader::*postfix-operators*))
    (declare-tptp-operators)
    (labels
      ((mapnconc-tptp-file-forms1 (filespec if-does-not-exist formula-selection)
         (let ((filename (intern (namestring filespec)))
               (tokens (with-open-file (stream filespec :direction :input :if-does-not-exist if-does-not-exist)
                         (tokenize stream :rationalize t)))
               (result nil) result-last form)
           (loop
             (when (null tokens)
               (return result))
             (setf (values form tokens) (tokens-to-lisp tokens))
             (setf form (tptp-to-snark-input form))
             (ecase (if (consp form) (first form) form)
               ((|cnf| |fof| |tff|)
                (when (implies formula-selection (member (second form) formula-selection))
                  (ncollect (funcall function
                                     (cond
                                      ((eq '|type| (third form))
                                       (input-tptp-type-declaration (fourth form)))
                                      (t
                                       (let ((ask-for-answer (and (consp (fourth form)) (eq 'tptp-double-question-mark (first (fourth form)))))
                                             (ask-for-answer2 (member (third form) '(|question| |negated_question|))))
                                         (let ((args nil))
                                           (when (or ask-for-answer ask-for-answer2)
                                             (setf args (list* :answer 'from-wff args)))
                                           (let ((reason (tptp-to-snark-reason (third form))))
                                             (unless (eq 'assertion reason)
                                               (setf args (list* :reason reason args))))
                                           (when (and (eq '|cnf| (first form)) (can-be-row-name (second form)))
                                             (setf args (list* :name (second form) args)))
                                           (setf args (list* :source (tptp-file-source-string filename (second form)) args))
                                           (list* 'assertion (if ask-for-answer (cons 'exists (rest (fourth form))) (fourth form)) args))))))
                            result)))
               (|include|
                (cl:assert (implies (rrest form) (and (consp (third form)) (eq '$$list (first (third form))))))
                (ncollect (mapnconc-tptp-file-forms1 (tptp-include-file-name (second form) filespec) :error (rest (third form))) result)))))))
      (mapnconc-tptp-file-forms1 filespec if-does-not-exist nil))))

(defun tptp-to-snark-reason (reason)
  (case reason
    (|axiom| 'assertion)
    ((|assumption| |hypothesis|) 'assumption)
    ((|negated_conjecture| |negated_question|) 'negated_conjecture)
    ((|conjecture| |question|) 'conjecture)
    (otherwise 'assertion)))

(defun input-tptp-type-declaration (x)
  (cond
   ((and (consp x) (eq 'tptp-colon (first x)))
    (cond
     ((eq '|$tType| (third x))
      ;; default declaration that can be overridden by subtype declaration,
      ;; assumes (declare-root-sort?) is :top-sort-a for disjointness with other sorts
      `(declare-sort ',(second x) :subsorts-incompatible t))
     ((symbolp (third x))
      (if (eq '|$o| (third x))
          `(declare-proposition ',(second x))
          `(declare-constant ',(second x) :sort ',(third x))))
     (t
      (cl:assert (and (consp (third x))
                      (eq 'tptp-type-arrow (first (third x)))
                      (tptp-type-product-p (second (third x)))))
      (let* ((argsorts (number-list (tptp-type-product-list (second (third x)))))
             (arity (length argsorts)))
        (if (eq '|$o| (third (third x)))
            `(declare-relation ',(second x) ,arity :sort ',argsorts)
            `(declare-function ',(second x) ,arity :sort ',(cons (third (third x)) argsorts)))))))
   ((and (consp x) (eq 'tptp-subtype (first x)) (symbolp (second x)) (symbolp (third x)))
    `(declare-subsort ',(second x) ',(third x) :subsorts-incompatible t))
   (t
    (error "Could not interpret type declaration ~S." x))))

(defun tptp-type-product-p (x)
  (or (symbolp x)
      (and (consp x)
           (eq 'tptp-type-product (pop x))
           (consp x)
           (tptp-type-product-p (pop x))
           (consp x)
           (tptp-type-product-p (pop x))
           (null x))))

(defun tptp-type-product-list (x)
  (if (symbolp x)
      (list x)
      (append (tptp-type-product-list (second x))
              (tptp-type-product-list (third x)))))

(defun number-list (l &optional (n 1))
  (if (endp l)
      nil
      (cons (list n (first l))
            (number-list (rest l) (+ 1 n)))))

(defvar *tptp-format* :tptp)

;(defvar *tptp-input-directory* '(:absolute #+(and mcl (not openmcl)) "Ame" "Users" "mark" "tptp" "snark" "in"))
;(defvar *tptp-input-directory-domains?* nil)
;(defvar *tptp-input-file-type* "tptp")

(defvar *tptp-input-directory* '(:absolute #+(and mcl (not openmcl)) "Ame" "Users" "mark" "tptp" "Problems"))
(defvar *tptp-input-directory-has-domain-subdirectories* t)
(defvar *tptp-input-file-type* "p")

(defvar *tptp-output-directory* '(:absolute #+(and mcl (not openmcl)) "Ame" "Users" "mark" "tptp" "snark" "out"))
(defvar *tptp-output-directory-has-domain-subdirectories* nil)
(defvar *tptp-output-file-type* "out")

(defun tptp-problem-pathname0 (name type directory has-domain-subdirectories)
  (let ((pn (merge-pathnames (parse-namestring (concatenate 'string (string name) "." type)) (make-pathname :directory directory))))
    (if has-domain-subdirectories
        (merge-pathnames (make-pathname :directory (append (pathname-directory pn) (list (subseq (pathname-name pn) 0 3)))) pn)
        pn)))

(defun tptp-problem-input-pathname (problem)
  (tptp-problem-pathname0
   problem
   *tptp-input-file-type*
   *tptp-input-directory*
   *tptp-input-directory-has-domain-subdirectories*))

(defun tptp-problem-output-pathname (problem)
  (tptp-problem-pathname0
   problem
   *tptp-output-file-type*
   *tptp-output-directory*
   *tptp-output-directory-has-domain-subdirectories*))

(defun do-tptp-problem (problem &key (format *tptp-format*) options)
  (refute-file
   (tptp-problem-input-pathname problem)
   :format format
   :options options
   :ignore-errors t
   :verbose t
   :output-file (tptp-problem-output-pathname problem)
   :if-exists nil))

(defun do-tptp-problem0 (problem &key (format *tptp-format*) options)
  (refute-file
   (tptp-problem-input-pathname problem)
   :format format
   :options options))

(defun do-tptp-problem1 (problem &key (format *tptp-format*) options)
  (do-tptp-problem0
   problem
   :format format
   :options (append '((agenda-length-limit nil)
                      (agenda-length-before-simplification-limit nil)
                      (use-hyperresolution t)
                      (use-paramodulation t)
                      (use-factoring :pos)
                      (use-literal-ordering-with-hyperresolution 'literal-ordering-p)
                      (use-literal-ordering-with-paramodulation  'literal-ordering-p)
	              (ordering-functions>constants t)
                      (assert-context :current)
                      (use-closure-when-satisfiable t)
                      (print-options-when-starting nil)
                      (use-variable-name-sorts nil)
	              (use-purity-test t)
	              (use-relevance-test t)
                      (snark-user::declare-tptp-symbols1))
                    options)))

(defun translate-assertion-file-to-tptp-format (inputfilespec &optional outputfilespec &rest read-assertion-file-options)
  (let ((snark-state (suspend-snark)))
    (unwind-protect
      (progn
        (initialize)
        (use-subsumption nil)
        (use-simplification-by-units nil)
        (use-simplification-by-equalities nil)
        (print-options-when-starting nil)
        (print-summary-when-finished nil)
        (print-rows-when-derived nil)
        (mapc #'eval (apply #'read-assertion-file inputfilespec read-assertion-file-options))
        (closure)
        (cond
         (outputfilespec
          (with-open-file (*standard-output* outputfilespec :direction :output)
            (print-rows :format :tptp)))
         (t
          (print-rows :format :tptp))))
      (resume-snark snark-state))
    nil))

(defun declare-tptp-operators ()
  (declare-operator-syntax "<=>" :xfy 505 'iff)
  (declare-operator-syntax "<~>" :xfy 505 'xor)
  (declare-operator-syntax "=>"  :xfy 504 'implies)
  (declare-operator-syntax "<="  :xfy 504 'implied-by)
  (declare-operator-syntax "&"   :xfy 503 'and)
  (declare-operator-syntax "~&"  :xfy 503 'nand)
  (declare-operator-syntax "|"   :xfy 502 'or)
  (declare-operator-syntax "~|"  :xfy 502 'nor)
;;(declare-operator-syntax "@"   :yfx 501)
  (declare-operator-syntax "*"   :yfx 480 'tptp-type-product)
;;(declare-operator-syntax "+"   :yfx 480 'tptp-type-union)
  (declare-operator-syntax ":"   :xfy 450 'tptp-colon)
  (declare-operator-syntax "~"   :fy  450 'not)
  (declare-operator-syntax "<<"  :xfx 450 'tptp-subtype)
  (declare-operator-syntax ">"   :xfy 440 'tptp-type-arrow)
  (declare-operator-syntax "="   :xfx 405 '=)
  (declare-operator-syntax "!="  :xfx 405 '/=)
;;(declare-operator-syntax "~="  :xfx 405)
  (declare-operator-syntax "!"   :fx  400 'forall)
  (declare-operator-syntax "?"   :fx  400 'exists)
  (declare-operator-syntax "??"  :fx  400 'tptp-double-question-mark)
;;(declare-operator-syntax "^"   :fx  400)
;;(declare-operator-syntax ".."  :xfx 400)
;;(declare-operator-syntax "!"   :xf  100)
  nil)

(defun tptp-to-snark-input (x)
  (cond
   ((atom x)
    (cond
     ((eq '|$true| x)
      true)
     ((eq '|$false| x)
      false)
     (t
      (fix-tptp-symbol x))))
   ((and (eq 'tptp-colon (first x))
         (consp (second x))
         (member (first (second x)) '(forall exists tptp-double-question-mark))
         (consp (second (second x)))
         (eq '$$list (first (second (second x)))))
    ;; (: (quantifier (list . variables)) form) -> (quantifer variables form)
    (list (first (second x)) (strip-colons (rest (second (second x)))) (tptp-to-snark-input (third x))))
   (t
    (lcons (fix-tptp-symbol (first x)) (tptp-to-snark-input-args (rest x)) x))))

(defun fix-tptp-symbol (x)
  ;; this is to allow users to input '?x' to create a constant ?x instead of a variable
  ;; '?...' is tokenized as |^A?...| and '^A...' is tokenized as |^A^A...| by the infix reader
  ;; this code removes the front ^A and wraps the symbol in a $$quote form if second character is ?
  (let (name)
    (cond
     ((and (symbolp x) (< 0 (length (setf name (symbol-name x)))) (eql (code-char 1) (char name 0)))
      (if (and (< 0 (length (setf name (subseq name 1)))) (eql (code-char 1) (char name 0)))
          (intern name)
          (list '$$quote (intern name))))
     (t
      x))))

(defun tptp-to-snark-input-args (l)
  (lcons (tptp-to-snark-input (first l))
         (tptp-to-snark-input-args (rest l))
         l))

(defun strip-colons (l)
  ;; (: var type) -> (var type) in quantifier variables
  ;; no transformation yet for (: integer var) or (: integer (: var type))
  (lcons (if (and (consp (first l))
                  (eq 'tptp-colon (first (first l)))
                  (symbolp (second (first l)))
                  (symbolp (third (first l))))
             (rest (first l))
             (first l))
         (strip-colons (rest l))
         l))

(defun read-tptp-term (string &rest options)
  (declare (dynamic-extent options))
  (let ((snark-infix-reader::*infix-operators* snark-infix-reader::*infix-operators*)
        (snark-infix-reader::*prefix-operators* snark-infix-reader::*prefix-operators*)
        (snark-infix-reader::*postfix-operators* snark-infix-reader::*postfix-operators*))
    (declare-tptp-operators)
    (multiple-value-bind (term rest) (apply #'read-infix-term string (append options (list :rationalize t)))
      (values (tptp-to-snark-input term) rest))))

;;; tptp.lisp EOF
