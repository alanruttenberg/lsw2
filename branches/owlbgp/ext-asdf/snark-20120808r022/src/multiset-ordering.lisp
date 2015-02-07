;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark -*-
;;; File: multiset-ordering.lisp
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

;;; comparison function should return >, <, =, or ?
;;;
;;; if testval is non-nil, it should be one of >, <, or =,
;;;   (eq testval (compare ... testval)) is true
;;; iff
;;;   (eq testval (compare ...))) is true,
;;; but faster

(defun compare-multisets (compare list1 list2 &optional testval)
  (let ((eql-alist nil))
    (dolist (x list1)
      (setf eql-alist (acons+ x 1 eql-alist)))
    (dolist (y list2)
      (setf eql-alist (acons+ y -1 eql-alist)))
    (cond
     ((alist-notany-minusp eql-alist)
      (if (alist-notany-plusp eql-alist) '= '>))
     ((alist-notany-plusp eql-alist)
      '<)
     (t
      (let ((alist nil))
        (flet ((equal0 (x y) (eq '= (funcall compare x y '=))))
          (declare (dynamic-extent #'equal0))
          (dolist (x eql-alist)
            (setf alist (acons+ (car x) (cdr x) alist :test #'equal0))))
        (cond
         ((alist-notany-minusp alist)
          (if (alist-notany-plusp alist) '= '>))
         ((alist-notany-plusp alist)
          '<)
         ((and (or (null testval) (eq '> testval))
               (dolist (y alist t)
                 (declare (type cons y))
                 (when (minusp (cdr y))
                   (unless (dolist (x alist nil)
                             (declare (type cons x))
                             (when (plusp (cdr x))
                               (if (or testval (not (test-option39?)))
                                   (when (eq '> (funcall compare (car x) (car y) '>))
                                     (return t))
                                   (case (funcall compare (car x) (car y))
                                     (>
                                      (return t))
                                     (<
                                      (setf (cdr x) 0))))))
                     (return nil)))))
          '>)
         ((and (or (null testval) (eq '< testval))
               (dolist (x alist t)
                 (declare (type cons x))
                 (when (plusp (cdr x))
                   (unless (dolist (y alist nil)
                             (declare (type cons y))
                             (when (minusp (cdr y))
                               (when (eq '< (funcall compare (car x) (car y) '<))
                                 (return t))))
                     (return nil)))))
          '<)
         (t
          (if (null testval) '? nil))))))))

(defun compare-term-multisets (compare xargs yargs &optional subst testval)

  ;; first, strip off initial eql arguments
  (loop
    (cond
     ((null xargs)
      (return-from compare-term-multisets (if (null yargs) '= '<)))
     ((null yargs)
      (return-from compare-term-multisets '>))
     ((eql (first xargs) (first yargs))
      (setf xargs (rest xargs))
      (setf yargs (rest yargs)))
     (t
      (return))))

  ;; quick comparison of singleton multisets
  (cond
   ((null (rest xargs))
    (cond
     ((null (rest yargs))
      (return-from compare-term-multisets (funcall compare (first xargs) (first yargs) subst testval)))
     ((member (first xargs) yargs)
      (return-from compare-term-multisets '<))))
   ((null (rest yargs))
    (cond
     ((member (first yargs) xargs)
      (return-from compare-term-multisets '>)))))

  (let ((variable-counts nil) (constant-counts nil) (compound-counts nil)
        (xargs-compound-exists nil) (yargs-compound-exists nil)
        (xargs-remain nil) (yargs-remain nil) term)

    ;; destructively updates lists of
    ;; variable and count pairs,
    ;; constant and count pairs, and
    ;; compound and count paris
    ;; term and count pair is represented as (term . count)
    (let (v)					;count variables and constants in xargs
      (dolist (term xargs)
        (dereference
         term subst
         :if-compound (setf xargs-compound-exists t)
         :if-variable (cond
                       ((null variable-counts)
                        (setf variable-counts (cons (make-tc term 1) nil)))
                       ((setf v (assoc/eq term variable-counts))
                        (incf (tc-count v)))
                       (t
                        (push (make-tc term 1) variable-counts)))
         :if-constant (cond
                       ((null constant-counts)
                        (setf constant-counts (cons (make-tc term 1) nil)))
                       ((setf v (assoc term constant-counts))
                        (incf (tc-count v)))
                       (t
                        (push (make-tc term 1) constant-counts))))))
    
    (let (v)					;count variables and constants in yargs
      (dolist (term yargs)
        (dereference
         term subst
         :if-compound (setf yargs-compound-exists t)
         :if-variable (cond
                       ((null variable-counts)
                        (if (eq '= testval)
                            (return-from compare-term-multisets nil)
                            (setf variable-counts (cons (make-tc term -1) nil))))
                       ((setf v (assoc/eq term variable-counts))
                        (if (and (eq '= testval) (eql 0 (tc-count v)))
                            (return-from compare-term-multisets nil)
                            (decf (tc-count v))))
                       (t
                        (if (eq '= testval)
                            (return-from compare-term-multisets nil)
                            (push (make-tc term -1) variable-counts))))
         :if-constant (cond
                       ((null constant-counts)
                        (if (eq '= testval)
                            (return-from compare-term-multisets nil)
                            (setf constant-counts (cons (make-tc term -1) nil))))
                       ((setf v (assoc term constant-counts))
                        (if (and (eq '= testval) (eql 0 (tc-count v)))
                            (return-from compare-term-multisets nil)
                            (decf (tc-count v))))
                       (t
                        (if (eq '= testval)
                            (return-from compare-term-multisets nil)
                            (push (make-tc term -1) constant-counts)))))))
    
    (when (eq '= testval)
      (dolist (v constant-counts)
        (unless (eql 0 (tc-count v))
          (return-from compare-term-multisets nil)))
      (dolist (v variable-counts)
        (unless (eql 0 (tc-count v))
          (return-from compare-term-multisets nil)))
      (cond
       ((not xargs-compound-exists)
        (return-from compare-term-multisets (if yargs-compound-exists nil '=)))
       ((not yargs-compound-exists)
        (return-from compare-term-multisets nil))))
    
    (when (or xargs-compound-exists yargs-compound-exists)
      (flet ((equal0 (x y) (eq '= (funcall compare x y subst '=))))
        (declare (dynamic-extent #'equal0))
        
        (when xargs-compound-exists
          (let (v)				;count compounds in xargs
            (dolist (term xargs)
              (dereference
               term subst
               :if-compound (cond
                             ((null compound-counts)
                              (setf compound-counts (cons (make-tc term 1) nil)))
                             ((setf v (or (assoc/eq term compound-counts)
                                          (assoc term compound-counts :test #'equal0)))
                              (incf (tc-count v)))
                             (t
                              (push (make-tc term 1) compound-counts)))))))
        
        (when yargs-compound-exists
          (let (v)				;count compounds in yargs
            (dolist (term yargs)
              (dereference
               term subst
               :if-compound (cond
                             ((null compound-counts)
                              (if (eq '= testval)
                                  (return-from compare-term-multisets nil)
                                  (setf compound-counts (cons (make-tc term -1) nil))))
                             ((setf v (or (assoc/eq term compound-counts)
                                          (assoc term compound-counts :test #'equal0)))
                              (if (and (eq '= testval) (eql 0 (tc-count v)))
                                  (return-from compare-term-multisets nil)
                                  (decf (tc-count v))))
                             (t
                              (if (eq '= testval)
                                  (return-from compare-term-multisets nil)
                                  (push (make-tc term -1) compound-counts))))))))))

    (when (eq '= testval)
      (dolist (v compound-counts)
        (unless (eql 0 (tc-count v))
          (return-from compare-term-multisets nil)))
      (return-from compare-term-multisets '=))
    
    (dolist (x variable-counts)
      (when (plusp (tc-count x))
        (setf term (tc-term x))
        (or (dolist (y compound-counts nil)
              (when (minusp (tc-count y))
                (when (eq '> (funcall compare (tc-term y) term subst '>))
                  (setf (tc-count x) 0)
                  (return t))))
            (cond				;uneliminated xarg variable
             ((and testval (neq '> testval))
              (return-from compare-term-multisets nil))
             (t
              (setf xargs-remain t))))))
    
    (dolist (y variable-counts)
      (when (minusp (tc-count y))
        (setf term (tc-term y))
        (or (dolist (x compound-counts nil)
              (when (plusp (tc-count x))
                (when (eq '> (funcall compare (tc-term x) term subst '>))
                  (setf (tc-count y) 0)
                  (return t))))
            (cond				;uneliminated yarg variable
             ((and testval (neq '< testval))
              (return-from compare-term-multisets nil))
             (xargs-remain
              (return-from compare-term-multisets '?))
             (t
              (setf yargs-remain t))))))
    
    (dolist (x constant-counts)
      (when (plusp (tc-count x))
        (setf term (tc-term x))
        (dolist (y constant-counts nil)
          (when (minusp (tc-count y))
            (ecase (symbol-ordering-compare term (tc-term y))
              (<
               (setf (tc-count x) 0)
               (return t))
              (>
               (setf (tc-count y) 0))
              (?
               ))))))
    
    (dolist (x constant-counts)
      (when (plusp (tc-count x))
        (setf term (tc-term x))
        (or (dolist (y compound-counts nil)
              (when (minusp (tc-count y))
                (ecase (funcall compare (tc-term y) term subst nil)
                  (>
                   (setf (tc-count x) 0)
                   (return t))
                  (<
                   (setf (tc-count y) 0))
                  (?
                   ))))
            (cond				;uneliminated xarg constant
             ((and testval (neq '> testval))
              (return-from compare-term-multisets nil))
             (yargs-remain
              (return-from compare-term-multisets '?))
             (t
              (setf xargs-remain t))))))
    
    (dolist (y constant-counts)
      (when (minusp (tc-count y))
        (setf term (tc-term y))
        (or (dolist (x compound-counts nil)
              (when (plusp (tc-count x))
                (ecase (funcall compare (tc-term x) term subst nil)
                  (>
                   (setf (tc-count y) 0)
                   (return t))
                  (<
                   (setf (tc-count x) 0))
                  (?
                   ))))
            (cond				;uneliminated yarg constant
             ((and testval (neq '< testval))
              (return-from compare-term-multisets nil))
             (xargs-remain
              (return-from compare-term-multisets '?))
             (t
              (setf yargs-remain t))))))
    
    (dolist (x compound-counts)
      (when (plusp (tc-count x))
        (setf term (tc-term x))
        (or (dolist (y compound-counts nil)
              (when (minusp (tc-count y))
                (ecase (funcall compare term (tc-term y) subst nil)
                  (<
                   (setf (tc-count x) 0)
                   (return t))
                  (>
                   (setf (tc-count y) 0))
                  (?
                   ))))
            (cond				;uneliminated xarg compound
             ((and testval (neq '> testval))
              (return-from compare-term-multisets nil))
             (yargs-remain
              (return-from compare-term-multisets '?))
             (t
              (setf xargs-remain t))))))
    
    ;;(cl:assert (not (and xargs-remain yargs-remain)))
    (cond
     (yargs-remain
      '<)
     ((dolist (y compound-counts nil)
        (when (minusp (tc-count y))
          (return t)))				;uneliminated yarg compound
      (if xargs-remain '? '<))
     (xargs-remain
      '>)
     (t
      '=))))

;;; multiset-ordering.lisp EOF
