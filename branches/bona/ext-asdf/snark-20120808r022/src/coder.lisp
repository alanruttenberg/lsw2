;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark-user -*-
;;; File: coder.lisp
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

(in-package :snark-user)

;;; coder finds shortest condensed-detachment proofs

(defstruct (proof-line
            (:constructor make-proof-line (number
                                           just
                                           wff
                                           &optional
                                           (wff-size (snark::size wff))
                                           (wff-vars (snark::variables wff))))
            (:copier nil))
  (number 0 :read-only t)
  (just nil :read-only t)
  (wff nil :read-only t)
  (wff-size 0 :read-only t)
  (wff-vars nil :read-only t)
  (target nil)
  (hint nil)
  (cut nil))

(defvar *coder-start-time*)
(defvar *coder-run-time-limit*)
(defvar *coder-step-count*)
(defvar *coder-derivation-count*)
(defvar *coder-print-state-interval* 1000000)
(defvar *coder-maximum-term-size-found*)
(defvar *coder-maximum-target-size*)
(defvar *coder-term-size-limit*)
(defvar *coder-term-vars-limit*)
(defvar *coder-ordering* :rpo)
(defvar *coder-do-reverse-cd*)

(defvar *test1* nil)
(defvar *test2* nil)

(defun coder (axioms target &rest options
              &key (max 100) (min 1) (max-syms nil) (max-vars nil) (op nil) (variables nil)
              kill avoid all-proofs must-use resume hints reverse-cd
              (steps-to-use nil) (steps-to-use-count (length steps-to-use))
              ((:run-time-limit *coder-run-time-limit*) nil)
              (*test1* *test1*) (*test2* *test2*))
  (let ((*print-pretty* nil))
    (print (cons 'coder (mapcar (lambda (x) (kwote x t)) (list* axioms target options))))
    (initialize)
    (cl:assert (>= (length steps-to-use) steps-to-use-count 0))
    (setf steps-to-use (if (= 0 steps-to-use-count) nil (mapcar #'coder-input-term steps-to-use)))
    (setf variables (mapcar (lambda (x) (cons x (snark::make-variable))) variables))
    (setf avoid (mapcar #'(lambda (x) (coder-input-term x variables)) avoid))
    (use-term-ordering *coder-ordering*)
    (use-default-ordering 'coder-default-symbol-ordering)
    (ordering-functions>constants t)
    (test-option19 t)
    (prog->
      (identity 0 -> naxioms)
      (mapcar (lambda (x) (make-proof-line (incf naxioms) naxioms (coder-input-term x variables))) axioms  -> axioms)
      (unless op
        (dolist (x axioms)
          (let ((x (proof-line-wff x)))
            (when (and (compound-p x) (eql 2 (length (args x))))
              (cond
               ((null op)
                (setf op (snark::function-name (head x))))
               ((not (eq op (snark::function-name (head x))))
                (warn "There is more than one binary relation; using condensed detachment for ~A." op)
                (return)))))))
      (reverse axioms -> axioms)
      (declare-function (if reverse-cd 'rcd 'cd) 2 :ordering-status :left-to-right -> cd)
      (input-target target -> target target-alist)
      (and (not (contains-test-target? target))
           (reduce #'max target-alist :key (lambda (x) (snark::size (cdr x))))
           -> *coder-maximum-target-size*)
      (mapcar #'coder-input-term hints -> hints)
      (identity max-syms -> *coder-term-size-limit*)
      (identity max-vars -> *coder-term-vars-limit*)
      (identity reverse-cd -> *coder-do-reverse-cd*)
      (identity nil -> all-targets-found)
      (setf *coder-step-count* 0)
      (setf *coder-derivation-count* 0)
      (setf *coder-maximum-term-size-found* 0)
      (get-internal-run-time -> *coder-start-time*)
      (loop for nsteps from min to max
            do (let (targets-found)
                 (format t "~2%Search for ~D-step proof... " nsteps)
                 (force-output)
	         (setf targets-found (coder1 axioms target nsteps cd op kill avoid all-proofs must-use resume hints steps-to-use steps-to-use-count))
	         (setf resume nil)
                 (let ((run-time (round (- (get-internal-run-time) *coder-start-time*) internal-time-units-per-second)))
                   (format t "~%~D steps in ~D seconds" *coder-step-count* run-time)
                   (when (and *coder-run-time-limit* (< *coder-run-time-limit* run-time))
                     (format t "; time limit exceeded")
                     (return)))
                 (when targets-found
                   (setf target (remove-target target targets-found))
                   (setf all-targets-found (nconc targets-found all-targets-found))
                   (when (null target)
                     (return)))))
      (format t ".")
      (mapcar (lambda (x) (or (car (rassoc x target-alist)) x)) all-targets-found))))

(defun coder1 (axioms target nsteps cd op kill avoid all-proofs must-use resume hints steps-to-use steps-to-use-count)
  (let ((together-target? (together-target? target))
        (targets-found nil))
    (labels
      ((coder2 (lines nsteps unused target* ntargets steps-to-use steps-to-use-count)
         ;; target* is used to record remaining targets only if target is a together-target
         (cond
          ((eql 0 nsteps)
           (incf *coder-derivation-count*)
           (cond
            (together-target?
             (cl:assert (null target*))		;all targets should have been matched
             (print-proof lines)
             (print-proof-for-otter-verification lines op)
             (force-output)
             (setf targets-found (rest target))
             (unless all-proofs
               (return-from coder1 targets-found)))
            (t
             (let ((found (target? target (proof-line-wff (first lines)))))	;is final wff a target?
               (when found
                 (setf (proof-line-target (first lines)) found)
                 (print-proof lines)
                 (print-proof-for-otter-verification lines op)
                 (force-output)
                 (dolist (v found)
                   (pushnew v targets-found))
                 (unless all-proofs
                   (when (null (setf target (remove-target target found)))
                     (return-from coder1 targets-found))))))))
          (t
           (flet
             ((coder3 (x y xunused? yunused? new-line)
                (let ((found (and together-target? (target? target* (proof-line-wff new-line)))))
                  (cond
                   (found
                    ;;(princf *coder-step-count*)
                    (cl:assert (null (rest found)) () "More than one together-target simultaneously satisfied.")
                    (when (eql 0 (rem (incf *coder-step-count*) *coder-print-state-interval*))
                      (let ((run-time (- (get-internal-run-time) *coder-start-time*)))
                        (print-coder-state (cons new-line lines) run-time)
                        (when (and *coder-run-time-limit* (< *coder-run-time-limit* (round run-time internal-time-units-per-second)))
                          (return-from coder1 targets-found))))
                    (setf (proof-line-target new-line) found)
                    (coder2
                     (cons new-line lines)
                     (- nsteps 1)
                     (let ((unused (if xunused? (remove x unused) unused)))
                       (if yunused? (remove y unused) unused))
                     (remove-target target* found)
                     (- ntargets 1)
                     steps-to-use
                     steps-to-use-count))
                   (t
                    (let ((new-steps-to-use steps-to-use) (new-steps-to-use-count steps-to-use-count))
                      (when (< 0 steps-to-use-count)
                        (setf new-steps-to-use (remove-step-to-use (proof-line-wff new-line) steps-to-use))
                        (unless (eq steps-to-use new-steps-to-use)
                          (decf new-steps-to-use-count)))
                      (cond
                       ((if together-target?
                            (>= (- nsteps 1) (+ ntargets new-steps-to-use-count))
                            (if (= 1 nsteps)
                                (= 0 steps-to-use-count)
                                (> (- nsteps 1) new-steps-to-use-count)))
                        ;;(princf *coder-step-count*)
                        (when (eql 0 (rem (incf *coder-step-count*) *coder-print-state-interval*))
                          (let ((run-time (- (get-internal-run-time) *coder-start-time*)))
                            (print-coder-state (cons new-line lines) run-time)
                            (when (and *coder-run-time-limit* (< *coder-run-time-limit* (round run-time internal-time-units-per-second)))
                              (return-from coder1 targets-found))))
                        (coder2
                         (cons new-line lines)
                         (- nsteps 1)
                         (let ((unused (if xunused? (remove x unused) unused)))
                           (cons new-line (if yunused? (remove y unused) unused)))
                         target*
                         ntargets
                         new-steps-to-use
                         new-steps-to-use-count)))))))))
             (declare (dynamic-extent #'coder3))
             (let ((new-lines nil)
                   (new-line-number (+ (proof-line-number (first lines)) 1)))
               (let ((nunused (length unused))
                     (revlines (reverse lines)))
                 (dolist (x revlines)		;use reverse to reorder search 2003-04-17
                   (let ((xunused? (member x unused))
                         (big nil))
                     (dolist (y revlines)	;use reverse to reorder search 2004-01-10
                       (let ((yunused? (and (not (eq x y)) (member y unused))))
                         (unless (> (if xunused?
                                        (if yunused? (- nunused 1) nunused)
                                        (if yunused? nunused (+ nunused 1)))
                                    (if (eql 1 ntargets) nsteps (+ nsteps ntargets -1)))
                           (let ((just (make-compound cd (proof-line-just x) (proof-line-just y))))
                             (when (or big
                                       (and (eq '> (snark::simplification-ordering-compare-terms0
                                                    just (proof-line-just (first lines)) nil '>))
                                            (setf big t)))
                             (prog->
                               (do-cd (proof-line-wff x) (proof-line-wff y) op (eql ntargets nsteps) ->* new-wff new-wff-size cut)
                               (if new-wff-size
                                   (make-proof-line new-line-number just new-wff new-wff-size)
                                   (make-proof-line new-line-number just new-wff)
                                   -> new-line)
                               (when cut
                                 (setf (proof-line-cut new-line) t))
                               (cond
                                ((and resume
                                      (let ((l1 resume) (l2 (coder-state (cons new-line lines))))
                                        (loop
                                          (cond
                                           ((null l1)
                                            (setf resume nil)
                                            (setf *coder-step-count* -1)
                                            (return nil))
                                           ((null l2)
                                            (return nil))
                                           ((not (equal (pop l1) (pop l2)))
                                            (return t))))))
                                 )
                                ((or hints *test1* *test2*)
                                 (cond
                                  ((and kill (funcall kill new-line))
                                   )
                                  ((and *test2* (backward-subsumes? new-line lines))
                                   ;; reject all derivations beginning with lines
                                   ;; when new-line is equal to an earlier line
                                   ;; as well as when it strictly subsumes it
                                   ;; as in the case below
                                   (return-from coder2))
                                  ((forward-subsumed? new-line lines)
                                   )
                                  ((and (not *test2*) (backward-subsumes? new-line lines))
                                   ;; don't just block adding new-line to lines but
                                   ;; reject all derivations beginning with lines
                                   (return-from coder2))
                                  (t
                                   (push (list x y xunused? yunused? new-line) new-lines))))
                                (t
                                 (unless (or (and kill (funcall kill new-line))
                                             (and avoid (member (proof-line-wff new-line) avoid :test #'snark::variant-p))
                                             (forward-subsumed? new-line lines)
                                             (backward-subsumes? new-line lines))
                                   (coder3 x y xunused? yunused? new-line))))
                               (when cut
                                 (return)))))))))))
               (when new-lines
                 (dolist (new-line (if hints (sort-new-lines new-lines hints) (nreverse new-lines)))
                   (apply #'coder3 new-line)))))))))
      (let ((ntargets (if together-target? (length (rest target)) 1)))
        (unless (> (+ ntargets steps-to-use-count) nsteps)
          (coder2 axioms nsteps (selected-lines axioms must-use) target ntargets steps-to-use steps-to-use-count)))
      targets-found)))

(defun sort-new-lines (new-lines hints)
  (dolist (x new-lines)
    (when (member (proof-line-wff (fifth x)) hints :test #'snark::subsumes-p)
      (setf (proof-line-hint (fifth x)) t)))
  (stable-sort (nreverse new-lines)
               (lambda (x y)
                 (and (proof-line-hint (fifth x))
                      (not (proof-line-hint (fifth y)))))))

(defun selected-lines (lines nums)
  (cond
   ((eq t nums)
    lines)
   (t
    (remove-if (lambda (line) (not (member (proof-line-number line) nums))) lines))))

(defun coder-default-symbol-ordering (x y)
  (if (numberp x)
      (if (and (numberp y) (> x y)) '> '<)
      '>))

(defun forward-subsumed? (new-line lines)
  ;; return true iff new-line is subsumed by an earlier line
  (let ((new-wff (proof-line-wff new-line))
        (new-wff-size (proof-line-wff-size new-line))
        (new-wff-vars (proof-line-wff-vars new-line)))
    (dolist (l lines nil)
      (when (and (>= new-wff-size (proof-line-wff-size l))
                 (snark::subsumed-p1 new-wff (proof-line-wff l) new-wff-vars))
        (return t)))))

(defun backward-subsumes? (new-line lines)
  ;; return true iff new-line subsumes an earlier line that is not used to derive new-line
  (let ((new-wff (proof-line-wff new-line))
        (new-wff-size (proof-line-wff-size new-line)))
    (dolist (l lines nil)
      (let ((j (proof-line-just l)))
        ;; don't backward subsume axioms or ancestors
        (cond
         ((not (compound-p j))	;l and rest of lines are all axioms
          (return nil))
         ((and (<= new-wff-size (proof-line-wff-size l))
               (snark::subsumes-p1 new-wff (proof-line-wff l) (proof-line-wff-vars l))
               (not (snark::occurs-p j (proof-line-just new-line) nil)))
          (return t)))))))

(defun do-cd (function x y op &optional last-line)
  ;; perform condensed detachment operation
  ;; with x as major premise and y as minor premise
  ;; assume x and y are variable disjoint unless (eq x y)
  ;; return result with new variables
  (prog->
    (when (and (compound-p x) (eq op (function-name (head x))))
      (args x -> args)
      (first args -> x1)
      (second args -> x2)
      (when *coder-do-reverse-cd*
        (psetf x1 x2 x2 x1))
      ;; (cd (i x t) s) always yields t for any s if x does not occur in t
      ;; producing alternative derivations which differ only in which minor premise is used
      ;; used to be enabled by *test3*, default since 2003-08-14
      (and (snark::variable-p x1) (not (snark::occurs-p x1 x2)) -> cut)
      ;; in this case, use same wff as major and minor premise, to avoid unnecessary use of y
      ;; added 2003-11-30
      (when (and cut (not (eq x y)))
        (return-from do-cd))
      (unify x1 (if (eq x y) (snark::renumber-new y) y) ->* subst)
      (snark::size x2 subst -> n)
      ;; don't create big terms that cannot subsume a target for the last line of proof
      (unless (or (and last-line *coder-maximum-target-size* (< *coder-maximum-target-size* n))
                  (and *coder-term-size-limit* (< *coder-term-size-limit* n))
                  (and *coder-term-vars-limit* (< *coder-term-vars-limit* (length (snark::variables x2 subst)))))
        (when (and (not *coder-term-size-limit*) (< *coder-maximum-term-size-found* n))
          (format t " ~D syms " n)
          (force-output)
          (setf *coder-maximum-term-size-found* n))
        (snark::renumber-new x2 subst -> x2*)
        (unless cut
          (setf cut (snark::variant-p x2 x2*)))
        (funcall function x2* n cut)))))

(defun just-line-number (j lines)
  (proof-line-number (first (member j lines :key #'proof-line-just :test #'equal-p))))

(defun just-list (j lines)
  (if (compound-p j)
      (cons (function-name (head j))
            (mapcar (lambda (x)
                      (if (compound-p x) (just-line-number x lines) x))
                    (args j)))
      j))

(defun print-proof-line-just (line lines)
  (let ((n (proof-line-number line))
        (j (just-list (proof-line-just line) lines)))
    (format t "~2D ~A" n (if (eql n j) 'ax j)))
  (when (proof-line-cut line)
    (format t "!")))

(defun print-proof-line (line lines)
  (let ((j (proof-line-just line)))
    (format t "~%(") (print-proof-line-just line lines) (format t "~15T")
    (print-term (snark::renumber (proof-line-wff line)))
    (format t ")")
    (cond
     ((compound-p j)
      (format t "~84T;~2D sym~:P, ~D var~:P"
              (snark::size (proof-line-wff line))
              (length (snark::variables (proof-line-wff line))))
      (when (proof-line-target line)
        (format t " target")))
     ((not (member j lines
                   :key #'proof-line-just
                   :test (lambda (x y) (and (not (snark::equal-p x y)) (snark::occurs-p x y nil)))))
      (format t "~84T;unused")))))

(defun print-proof-lines (lines)
  (mapc (lambda (line) (print-proof-line line lines)) lines))

(defun print-proof (lines)
  (format t "~2%Proof:")
  (print-proof-lines (reverse lines))
  (format t "~%End proof.")
  (terpri))

(defun coder-state (lines)
  (let ((lines (reverse lines)))
    (mapcan (lambda (line)
              (let ((j (just-list (proof-line-just line) lines)))
                (if (consp j) (list j) nil)))
            lines)))

(defun print-coder-state (lines &optional (run-time (- (get-internal-run-time) *coder-start-time*)))
  (format t "~%  ~A ~5Dm "
          (subseq (print-current-time nil t) 4 13)
          (round run-time (* 60 internal-time-units-per-second)))
  (mapc (lambda (x) (princ x) (princ " ")) (coder-state lines))
  (force-output))

;;; coder's target argument is either a normal-target or a together-target
;;;
;;; a single-target is one of
;;;   a term - find generalization (or variant) of this term
;;;   (TEST predicate)
;;;
;;; a normal-target is one of
;;;   a single-target
;;;   (OR normal-target1 ... normal-targetn)  - search until one target is found
;;;   (AND normal-target1 ... normal-targetn) - search until all targets are found
;;;
;;; a together-target is
;;;   (TOGETHER single-target1 ... single-targetn) - search until all targets are found in a single derivation
;;;   it is assumed that no single formula will satisfy more than one of these targets

(defvar *input-target-alist*)

(defun input-target (target)
  (let ((*input-target-alist* nil))
    (values (cond
             ((together-target? target)
              (input-together-target target))
             (t
              (input-normal-target target)))
            *input-target-alist*)))

(defun together-target? (target)
  (and (consp target) (eq 'together (first target))))

(defun contains-test-target? (target)
  (case (and (consp target) (first target))
    (test
     t)
    ((and or together)
     (some #'contains-test-target? (rest target)))))

(defun wrap2 (f l)
  (cl:assert (consp l))
  (if (null (rest l)) (first l) (cons f l)))

(defun coder-input-term (x &optional variables)
  (snark::renumber-new
   (snark::input-term
    (if (stringp x) (read-infix-term x :case (readtable-case *readtable*)) x)
    :*input-wff-substitution* variables)))

(defun input-together-target (target)
  (wrap2 (first target) (mapcar #'input-single-target (rest target))))

(defun input-normal-target (target)
  (cond
   ((and (consp target) (member (first target) '(or and)))
    (wrap2 (first target) (mapcar #'input-normal-target (rest target))))
   (t
    (input-single-target target))))

(defun input-single-target (target)
  (cl:assert (not (and (consp target) (member (first target) '(or and together)))))
  (cond
   ((and (consp target) (eq 'test (first target)))
    target)
   (t
    (let ((target* (coder-input-term target)))
      (push (cons target target*) *input-target-alist*)
      target*))))

(defun target? (target x &optional l)
  ;; does x generalize a term in target?
  (cond
   ((and (consp target) (member (first target) '(or and together)))
    (dolist (y (rest target) l)
      (setf l (target? y x l))))
   ((and (consp target) (eq 'test (first target)))
    (if (funcall (second target) x) (adjoin target l) l))
   (t
    (if (snark::subsumes-p x target) (adjoin target l) l))))

(defun remove-target (target l)
  (cond
   ((and (consp target) (eq 'or (first target)))
    (let ((v (mapcar (lambda (y)
                        (let ((y* (remove-target y l)))
                          (or y* (return-from remove-target nil))))
                      (rest target))))
      (wrap2 'or v)))
   ((and (consp target) (member (first target) '(and together)))
    (let ((v (mapcan (lambda (y)
                       (let ((y* (remove-target y l)))
                         (and y* (list y*))))
                     (rest target))))
      (and v (wrap2 (first target) v))))
   (t
    (if (member target l) nil target))))

(defun remove-step-to-use (wff steps-to-use)
  (cond
   ((null steps-to-use)
    nil)
   ((snark::subsumes-p wff (first steps-to-use))
    (rest steps-to-use))
   (t
    (let* ((l (rest steps-to-use))
           (l* (remove-step-to-use wff l)))
      (if (eq l l*) steps-to-use (cons (first steps-to-use) l*))))))

(defun print-proof-for-otter-verification (lines op)
  ;; Bob Veroff provided the template for this script
  (let ((lines (reverse lines)))
    (format t "~%% OTTER SCRIPT TO TRY TO FIND SAME PROOF")
    (format t "~%  set(hyper_res). clear(print_kept). clear(print_back_sub). assign(stats_level,0).")
    (format t "~%  assign(bsub_hint_add_wt,-1000000). set(keep_hint_subsumers). assign(max_weight,1).")
    (format t "~%  list(sos).                    % AXIOMS:")
    (dolist (l lines)
      (unless (compound-p (proof-line-just l))
        (format t "~%    ") (print-term-for-otter2 (proof-line-wff l)) (format t ".")))
    (format t "~%    end_of_list.")
    (cond
     (*coder-do-reverse-cd*
      (format t "~%  list(usable).                 % REVERSED CONDENSED DETACHMENT RULE:")
      (format t "~%    -P(~A(x,y)) | -P(y) | P(x)." (string-downcase (string op))))
     (t
      (format t "~%  list(usable).                 % CONDENSED DETACHMENT RULE:")
      (format t "~%    -P(~A(x,y)) | -P(x) | P(y)." (string-downcase (string op)))))
    (format t "~%    end_of_list.")
    (let ((first t))
      (dolist (l lines)
        (when (proof-line-target l)
          (cond
           (first
            (setf first nil)
            (format t "~%  list(usable).                 % TARGET:"))
           (t
            (format t " |")))
          (format t "~%    -") (print-term-for-otter2 (proof-line-wff l) t)))
      (unless first
        (format t ".~%    end_of_list.")))
    (format t "~%  list(hints).                  % PROOF LINES:")
    (dolist (l lines)
      (format t "~%    ") (print-term-for-otter2 (proof-line-wff l)) (format t ".")
      (format t "~72T%") (print-proof-line-just l lines)
      (when (proof-line-target l)
        (format t " TARGET")))
    (format t "~%    end_of_list.")
    (format t "~%% OTTER SCRIPT END~%")
    ))

(defun print-term-for-otter2 (term &optional ground)
  (princ "P(")
  (print-term-for-otter (snark::renumber term) ground)
  (princ ")")
  term)

(defun print-term-for-otter (term &optional ground)
  (dereference
   term nil
   :if-variable (cond
                 (ground
                  (princ #\c)
                  (princ (snark::variable-number term)))
                 (t
                  (let ((n (snark::variable-number term)))
                    (cond
                     ((> 6 n)
                      (princ (ecase n (0 #\x) (1 #\y) (2 #\z) (3 #\u) (4 #\v) (5 #\w))))
                     (t
                      (princ #\v)
                      (princ n))))))
   :if-constant (cond
                 ((numberp term)
                  (princ term))
                 (t
                  (princ #\c)
                  (princ (string-downcase (string term)))))
   :if-compound (progn 
                  (princ (string-downcase (string (function-name (head term)))))
                  (princ "(")
                  (let ((first t))
                    (dolist (arg (args term))
                      (if first (setf first nil) (princ ","))
                      (print-term-for-otter arg ground)))
                  (princ ")")))
  term)

(defun comb (n m)
  (/ (let ((v 1))
       (dotimes (i m)
         (setf v (* v (- n i))))
       v)
     (let ((v 1))
       (dotimes (i (- m 1))
         (setf v (* v (+ i 2))))
       v)))

(defun shorten-proof (proof &rest options
                            &key (drop 3) (shorten-by 1) (naxioms 1) (targets '(-1)) all-proofs skip from to min max
                            (variables '(x y z u v w v0
					 x1 y1 z1 u1 v1 w1  v6  v7  v8  v9 v10 v11
					 x2 y2 z2 u2 v2 w2 v12 v13 v14 v15 v16 v17
					 x3 y3 z3 u3 v3 w3 v18 v19 v20 v21 v22 v23
					 x4 y4 z4 u4 v4 w4 v24 v25 v26 v27 v28 v29
					 x5 y5 z5 u5 v5 w5 v30 v31 v32 v33 v34 v35)))
  ;; attempt to find a shorter proof than argument proof (a list of formulas)
  ;; default is to assume there is a single axiom (first in proof) and single target (last in proof)
  ;; to try to find a shorter proof,
  ;; omit drop steps and search for a proof with fewer than drop steps to replace them
  ;;
  ;; :drop 0 :shorten-by 0 options can be used to reproduce proof
  (print (cons 'shorten-proof (mapcar (lambda (x) (kwote x t)) (list* proof options))))
  (when skip
    (cl:assert (null from))
    (setf from (+ skip 1)))
  (let* ((l proof)
         (proof-length (length proof))
         (nsteps (- proof-length naxioms))
         (target nil)
         (source nil)
         (found nil)
         (count 0))
    (dolist (i (reverse targets))		;collect targets into target
      (push (nth (if (> 0 i) (+ proof-length i) i) proof) target))
    (dotimes (i naxioms)	;collect axioms into source
      (declare (ignorable i))
      (push (pop l) source))
    (when (eql 1 naxioms)			;if there is only one axiom, first step is forced,
      (unless (or (member 2 targets) (member (- 1 proof-length) targets))
        (setf l (rest l))))			;so omit it from candidates to be replaced
    (setf l (set-difference l target))		;l is now list of potentially replaceable nontarget steps
    (prog->
      (length l -> len)
      (comb len drop -> ncombs)
      (choose l (- len drop) ->* kept-steps)	;shorten l by drop steps in all ways
      (incf count)
      (when (and to (< to count))
        (return-from prog->))
      (when (implies from (<= from count))
        (format t "~2%Shorten proof attempt ~D of ~D" count ncombs)
        (when (coder source
                     (cons 'together (append target kept-steps))
                     :min (or min (- nsteps drop))
                     :max (or max (- nsteps shorten-by))
                     :all-proofs all-proofs
                     :variables variables)
          (setf found t)
          (unless all-proofs
            (return-from prog->)))))
    found))

(defun strip-ors (wff)
  (cond
   ((and (consp wff) (eq 'or (first wff)))
    (reduce #'append (mapcar #'strip-ors (rest wff))))
   (t
    (list wff))))

(defun condensed-detachment-rule-p (wff)
  ;; recognizer for (or (not (p (i ?x ?y))) (or (not (p ?x)) (p ?y)))
  (let ((l (strip-ors wff)))
    (and (= 3 (length l))
         (let ((subst (some (lambda (x)
                              (let ((subst (pattern-match '(not (?pred (?fun ?var1 ?var2))) x)))
                                (and subst
                                     (let ((var1 (sublis subst '?var1))
                                           (var2 (sublis subst '?var2)))
                                       (and (neq var1 var2)
                                            (can-be-free-variable-name var1)
                                            (can-be-free-variable-name var2)))
                                     subst)))
                            l)))
           (and (member (sublis subst '(not (?pred ?var1))) l :test #'equal)
                (member (sublis subst '(?pred ?var2)) l :test #'equal)
                subst)))))

(defun condensed-detachment-problem-p (assertions)
  (and (every (lambda (x) (and (consp x) (eq 'assertion (first x)))) assertions)
       (multiple-value-bind
         (cd-rule subst)
         (dolist (x assertions)
           (let ((x (second x)))
             (let ((subst (condensed-detachment-rule-p x)))
               (when subst
                 (return (values x subst))))))
         (and cd-rule
              (let ((axioms nil)
                    (target nil)
                    (axiom-pattern (sublis subst '((?pred ?x))))
                    (target-pattern (sublis subst '(not (?pred ?x)))))
                (dolist (x assertions (and axioms target (values (reverse axioms) target (sublis subst '?fun) (sublis subst '?pred))))
                  (let ((x (second x)))
                    (unless (eq cd-rule x)
                      (let ((x (strip-ors x)))
                        (cond
                         ((pattern-match axiom-pattern x)
                          (push (second (first x)) axioms))
                         ((and (null target) (every (lambda (x) (pattern-match target-pattern x)) x))
                          (setf target (if (null (rest x))
                                           (second (second (first x)))
                                           (cons 'together (mapcar (lambda (x) (second (second x))) x)))))
                         (t
                          (return nil))))))))))))

;;; coder.lisp EOF
