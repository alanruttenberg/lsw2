;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: feature-vector.lisp
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

(defconstant $fv-maximum-feature-value 999)
(defconstant $fv-features-per-symbol 10)
(defconstant $fv-offset-pos-count 0)		;number of occurrences in positive literals
(defconstant $fv-offset-neg-count 1)		;number of occurrences in negative literals
(defconstant $fv-offset-pos-max-depth 2)	;maximum depth of occurrences in positive literals
(defconstant $fv-offset-neg-max-depth 3)	;maximum depth of occurrences in negative literals
(defconstant $fv-offset-pos-min-depth 4)	;minimum depth of occurrences in positive literals (negated)
(defconstant $fv-offset-neg-min-depth 5)	;minimum depth of occurrences in negative literals (negated)
(defconstant $fv-number-ground 0)		;pseudo symbol-number for ground literal counts, doesn't match any actual symbol-number

(declare-snark-option feature-vector-symbol-number-folding 10 10)

(defun new-feature-vector ()
  (make-sparse-vector :default-value 0))

(defun feature-vector-list (fv)
  ;; convert to list form suitable for input to trie.lisp operations
  (let ((fv* nil))
    (prog->
      (map-sparse-vector-with-indexes fv :reverse t ->* v k)
      (cl:assert (< 0 v))
      (setf fv* (list* (fv-trie-key k v) fv*)))
    fv*))

(defun update-feature-vector (symbol-number relation-symbol? arity polarity count depth fv)
  (let* ((symbol-number* (let ((n (feature-vector-symbol-number-folding?)))
                           (if n
                               (+ (mod symbol-number n)
                                  (if relation-symbol?	;fold relations and functions separately
                                      (+ 1 (case arity (0 (* 1 n)) (1 (* 2 n)) (2 (* 3 n)) (otherwise (* 4 n))))
                                      (+ 1 (case arity (0 (* 5 n)) (1 (* 6 n)) (2 (* 7 n)) (otherwise (* 8 n))))))
                               symbol-number)))
         (base (* $fv-features-per-symbol symbol-number*))
         (pos (ecase polarity (:pos t) (:neg nil))))
    (cl:assert (and (<= 1 count) (<= 0 depth)))
    (cond
     (relation-symbol?
      (let* ((count-index (+ base (if pos $fv-offset-pos-count $fv-offset-neg-count)))
             (v (sparef fv count-index))
             (v* (min $fv-maximum-feature-value (+ v count))))
        (unless (= v v*)
          (setf (sparef fv count-index) v*))))
     (t
      (let* ((max-depth-index (+ base (if pos $fv-offset-pos-max-depth $fv-offset-neg-max-depth)))
             (v (sparef fv max-depth-index))
             (v* (min $fv-maximum-feature-value (max v depth))))
        (unless (= v v*)
          (setf (sparef fv max-depth-index) v*)))
      (cond
       ((test-option49?)
        (let* ((count-index (+ base (if pos $fv-offset-pos-count $fv-offset-neg-count)))
               (v (sparef fv count-index))
               (v* (min $fv-maximum-feature-value (+ v count))))
          (unless (= v v*)
            (setf (sparef fv count-index) v*))))
       (t
        (let* ((min-depth-index (+ base (if pos $fv-offset-pos-min-depth $fv-offset-neg-min-depth)))
               (v (sparef fv min-depth-index))	;translate lower depths to higher feature values
               (v* (max 1 (max v (- $fv-maximum-feature-value depth)))))
          (unless (= v v*)
            (setf (sparef fv min-depth-index) v*))
          (cond
           ((and (= 0 v) (< 1 count))
            (let ((count-index (+ base (if pos $fv-offset-pos-count $fv-offset-neg-count))))
              (setf (sparef fv count-index) (min $fv-maximum-feature-value count))))
           ((< 0 v)				;don't store count for single occurrence
            (let* ((count-index (+ base (if pos $fv-offset-pos-count $fv-offset-neg-count)))
                   (v (sparef fv count-index))
                   (v* (min $fv-maximum-feature-value (if (= 0 v) (+ 1 count) (+ v count)))))
              (unless (= v v*)
                (setf (sparef fv count-index) v*))))))))))
    fv))

(defun clause-feature-vector (clause &optional subst (convert-to-list? t))
  (let ((fv (new-feature-vector)))
    (prog->
      (map-atoms-in-clause clause ->* atom polarity)
      (atom-feature-vector atom subst polarity fv)
      (unless (test-option50?)
        (when (ground-p atom subst)
          (incf (sparef fv (+ $fv-number-ground (if (eq :pos polarity) $fv-offset-pos-count $fv-offset-neg-count)))))))
    (if convert-to-list? (feature-vector-list fv) fv)))

(defun atom-or-term-feature-vector (x &optional subst (convert-to-list? t))
  (let ((fv (new-feature-vector)))
    (if (dereference
         x subst
         :if-constant (constant-boolean-valued-p x)
         :if-compound-appl (function-boolean-valued-p (heada x)))
        (atom-feature-vector x subst :pos fv)
        (term-feature-vector x subst :pos 0 fv))
    (if convert-to-list? (feature-vector-list fv) fv)))

(defun atom-feature-vector (atom &optional subst (polarity :pos) (fv (new-feature-vector)))
  (dereference
   atom subst
   :if-constant (update-feature-vector (constant-number atom) t 0 polarity 1 0 fv)
   :if-compound (progn
                  (update-feature-vector (function-number (head atom)) t (function-arity (head atom)) polarity 1 0 fv)
                  (mapc #'(lambda (arg) (term-feature-vector arg subst polarity 0 fv)) (args atom))))
  fv)

(defun term-feature-vector (term &optional subst (polarity :pos) (depth 0) (fv (new-feature-vector)))
  ;; in (p a (f b)), depth(p)=depth(a)=depth(f)=0, depth(b)=1
  ;; compute count of associative function symbols as if term is in unflattened form
  ;; count(f)=2 for f(a,b,c)
  ;; compute depth of terms with associatve function symbols as if term is in flattened form
  ;; depth(a)=1 for f(f(a,b),c)
  (labels
    ((tfv (term depth)
       (dereference
        term subst
        :if-constant (update-feature-vector (constant-number term) nil 0 polarity 1 depth fv)
        :if-compound (prog->
                       (head term -> head)
                       (args term -> args)
                       (if (function-associative head) head nil -> head-if-associative)
                       (if head-if-associative
                           (update-feature-vector (function-number head) nil (function-arity head) polarity (max (- (length args) 1) 1) depth fv)
                           (update-feature-vector (function-number head) nil (function-arity head) polarity 1 depth fv))
                       (mapc #'(lambda (arg)
                                 (if (and head-if-associative
                                          (dereference
                                           arg subst
                                           :if-compound (and head-if-associative (eq head-if-associative (head arg)))))
                                     (tfv arg depth)
                                     (tfv arg (+ depth 1))))
                             args)))))
    (tfv term depth))
  fv)

;;; feature-vector.lisp EOF
