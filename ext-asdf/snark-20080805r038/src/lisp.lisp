;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark-lisp -*-
;;; File: lisp.lisp
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

(in-package :snark-lisp)

(defconstant none '$$none)			;special null value to use when NIL won't do
(defconstant true '$$true)
(defconstant false '$$false)

(defmacro definline (name lambda-list &body body)
  #-clisp
  `(progn 
     (defun ,name ,lambda-list ,@body)
     (define-compiler-macro ,name (&rest arg-list)
       (cons '(lambda ,lambda-list ,@body) arg-list)))
  #+clisp
  `(defun ,name ,lambda-list ,@body))

(definline neq (x y)
  (not (eq x y)))

(definline neql (x y)
  (not (eql x y)))

(definline nequal (x y)
  (not (equal x y)))

(definline nequalp (x y)
  (not (equalp x y)))

(definline iff (x y)
  (eq (not x) (not y)))

(defmacro implies (x y)
  ;; implies is a macro so that y is not evaluated if x is false
  `(if ,x ,y t))

(defmacro if-let (binding thenform elseform)
  (let ((block (gensym)) (temp (gensym)))
    `(block ,block
       (let ((,temp ,(second binding)))
         (when ,temp
           (return-from ,block
             (let ((,(first binding) ,temp))
               ,thenform))))
       ,elseform)))

(defmacro when-let (binding &rest forms)
  `(if-let ,binding (progn ,@forms) nil))

(defun kwote (x &optional selectively)
  (if (implies selectively (not (constantp x)))
      (list 'quote x)
      x))

(defun unquote (x)
  (if (and (consp x) (eq 'quote (first x)))
      (second x)
      x))

(definline rrest (list)
  (cddr list))

(definline rrrest (list)
  (cdddr list))

(definline rrrrest (list)
  (cddddr list))

(definline mklist (x)
  (if (listp x) x (list x)))

(defun firstn (list num)
  ;; return a new list that contains the first num elements of list
  (declare (type integer num))
  (cond
   ((or (eql 0 num) (atom list))
    nil)
   (t
    (cons (first list) (firstn (rest list) (- num 1))))))

(defun consn (x y num)
  ;; cons x and y n times
  ;; (cons 'a '(b) 3) = (a a a b)
  (declare (type integer num))
  (dotimes (dummy num)
    (declare (type integer dummy) (ignorable dummy))
    (push x y))
  y)

(defun leafp (x y)
  (if (atom y)
      (eql x y)
      (or (leafp x (car y)) (leafp x (cdr y)))))

(defun naturalp (x)
  (and (integerp x) (not (minusp x))))

(defun ratiop (x)
  (and (rationalp x) (not (integerp x))))

(defmacro carc (x)
  `(car (the cons ,x)))

(defmacro cdrc (x)
  `(cdr (the cons ,x)))

(defmacro caarcc (x)
  `(carc (carc ,x)))

(defmacro cadrcc (x)
  `(carc (cdrc ,x)))

(defmacro cdarcc (x)
  `(cdrc (carc ,x)))

(defmacro cddrcc (x)
  `(cdrc (cdrc ,x)))

(defmacro lcons (a* b* ab)
  ;; (lcons a* b* ab) does lazy cons of a* and b*
  ;; lcons does not evaluate a* or b* and returns nil if ab is nil
  ;; lcons does not evaluate b* and treats it as nil if (cdr ab) is nil
  ;; lcons returns ab if a* = (car ab) and b* = (cdr ab)
  ;; otherwise, lcons conses a* and b*
  ;;
  ;; lcons is useful for writing functions that map over lists
  ;; and return a modified list without unnecessary consing
  ;; for example, the following applies a substitution to a list of terms
  ;; (defun instantiate-list (terms subst)
  ;;   (lcons (instantiate-term (first terms) subst)
  ;;          (instantiate-list (rest terms) subst)
  ;;          terms))
  (assert (symbolp ab))
  (let ((tempa (gensym)) (tempb (gensym)) (tempa* (gensym)) (tempb* (gensym)))
    (setf a* (sublis (list (cons `(car ,ab) tempa)
                           (cons `(carc ,ab) tempa)
			   (cons `(first ,ab) tempa)
			   (cons `(nth 0 ,ab) tempa))
		     a*
		     :test #'equal))
    (setf b* (sublis (list (cons `(cdr ,ab) tempb)
                           (cons `(cdrc ,ab) tempb)
			   (cons `(rest ,ab) tempb)
			   (cons `(nthcdr 1 ,ab) tempb))
		     b*
		     :test #'equal))
    `(if (null ,ab)
	 nil
	 (let* ((,tempa (car ,ab))
		(,tempa* ,a*)
		(,tempb (cdrc ,ab)))
	   (if (null ,tempb)
	       (if (eql ,tempa ,tempa*)
		   ,ab
		   (cons ,tempa* nil))
	       (let ((,tempb* ,b*))
		 (if (and (eql ,tempb ,tempb*)
			  (eql ,tempa ,tempa*))
		     ,ab
		     (cons ,tempa* ,tempb*))))))))

(definline cons-unless-nil (x &optional y)
  ;; returns y if x is nil, otherwise returns (cons x y)
  ;; if y is omitted: returns nil if x is nil, otherwise (list x)
  (if (null x) y (cons x y)))

(defmacro push-unless-nil (item place)
  ;; doesn't evaluate place if item is nil
  ;; always returns nil
  (let ((v (gensym)))
    `(let ((,v ,item))
       (unless (null ,v)
         (push ,v ,place)
         nil))))

(defmacro pushnew-unless-nil (item place &rest options)
  ;; doesn't evaluate place or options if item is nil
  ;; always returns nil
  (let ((v (gensym)))
    `(let ((,v ,item))
       (unless (null ,v)
	 (pushnew ,v ,place ,@options)
         nil))))

(defmacro dotails ((var listform &optional resultform) &body body)
  ;; dotails is just like dolist except the variable is bound
  ;; to successive tails instead of successive elements of the list
  `(do ((,var ,listform (rest ,var)))
       ((endp ,var)
        ,resultform)
     ,@body))

(defmacro dopairs ((var1 var2 listform &optional resultform) &body body)
  ;; (dopairs (x y '(a b c)) (print (list x y))) prints (a b), (a c), and (b c)
  ;; doesn't handle declarations in body correctly
  (let ((l1 (gensym)) (l2 (gensym)) (loop (gensym)))
    `(do ((,l1 ,listform) ,var1 ,var2 ,l2)
         ((endp ,l1)
          ,resultform)
       (setf ,var1 (pop ,l1))
       (setf ,l2 ,l1)
       ,loop
       (unless (endp ,l2)
         (setf ,var2 (pop ,l2))
         ,@body
         (go ,loop)))))

(defun choose (function list k)
  ;; apply function to lists of k items taken from list
  (labels
    ((choose* (cc l k n)
       (cond
        ((eql 0 k)
         (funcall cc nil))
        ((eql n k)
         (funcall cc l))
        (t
         (prog->
           (decf n)
           (pop l -> x)
           (choose* l (- k 1) n ->* res)
           (funcall cc (cons x res)))
         (prog->
           (choose* l k n ->* res)
           (funcall cc res))))))
    (let ((len (length list)))
      (when (minusp k)
        (incf k len))
      (cl:assert (<= 0 k len))
      (choose* function list k len)
      nil)))

(defun integers-between (low high)
  ;; list of integers in [low,high]
  (let ((i high)
	(result nil))
    (loop
      (when (< i low)
	(return result))
      (push i result)
      (decf i))))

(defun ints (low high)
  ;; list of integers in [low,high]
  (integers-between low high))

(defun length= (x y)
  ;; if y is an integer then (= (length x) y)
  ;; if x is an integer then (= x (length y))
  ;; otherwise (= (length x) (length y))
  (cond
   ((or (not (listp y)) (when (not (listp x)) (psetq x y y x) t))
    (and (<= 0 y)
         (loop
           (cond
            ((endp x)
             (return (eql 0 y)))
            ((eql 0 y)
             (return nil))
            (t
             (setf x (rest x) y (- y 1)))))))
   (t
    (loop
      (cond
       ((endp x)
        (return (endp y)))
       ((endp y)
        (return nil))
       (t
        (setf x (rest x) y (rest y))))))))

(defun length< (x y)
  ;; if y is an integer then (< (length x) y)
  ;; if x is an integer then (< x (length y))
  ;; otherwise (< (length x) (length y))
  (cond
   ((not (listp y))
    (and (<= 1 y)
         (loop
           (cond
            ((endp x)
             (return t))
            ((eql 1 y)
             (return nil))
            (t
             (setf x (rest x) y (- y 1)))))))
   ((not (listp x))
    (or (> 0 x)
        (loop
          (cond
           ((endp y)
            (return nil))
           ((eql 0 x)
            (return t))
           (t
            (setf x (- x 1) y (rest y)))))))
   (t       
    (loop
      (cond
       ((endp x)
        (return (not (endp y))))
       ((endp y)
        (return nil))
       (t
        (setf x (rest x) y (rest y))))))))

(defun length<= (x y)
  ;; if y is an integer then (<= (length x) y)
  ;; if x is an integer then (<= x (length y))
  ;; otherwise (<= (length x) (length y))
  (cond
   ((not (listp y))
    (and (<= 0 y)
         (loop
           (cond
            ((endp x)
             (return t))
            ((eql 0 y)
             (return nil))
            (t
             (setf x (rest x) y (- y 1)))))))
   ((not (listp x))
    (or (> 1 x)
        (loop
          (cond
           ((endp y)
            (return nil))
           ((eql 1 x)
            (return t))
           (t
            (setf x (- x 1) y (rest y)))))))
   (t
    (loop
      (cond
       ((endp x)
        (return t))
       ((endp y)
        (return nil))
       (t
        (setf x (rest x) y (rest y))))))))

(definline length> (x y)
  (length< y x))

(definline length>= (x y)
  (length<= y x))

(defun acons+ (key delta alist &key test)
  ;; creates a new association list with datum associated with key adjusted up or down by delta
  ;; omits pairs with datum 0
  (labels
    ((ac+ (alist)
       (declare (type cons alist))
       (let ((pair (first alist))
             (alist1 (rest alist)))
         (declare (type cons pair))
         (cond
          ((if test (funcall test key (car pair)) (eql key (car pair)))
           (let ((datum (+ (cdr pair) delta)))
             (if (= 0 datum) alist1 (cons (cons key datum) alist1))))
          ((null alist1)
           alist)
          (t
           (let ((alist1* (ac+ alist1)))
             (if (eq alist1 alist1*) alist (cons pair alist1*))))))))
    (cond
     ((= 0 delta)
      alist)
     ((null alist)
      (cons (cons key delta) nil))
     (t
      (let ((alist* (ac+ alist)))
        (if (eq alist alist*) (cons (cons key delta) alist) alist*))))))

(defun alist-notany-plusp (alist)
  (dolist (pair alist t)
    (declare (type cons pair))
    (when (plusp (cdr pair))
      (return nil))))

(defun alist-notany-minusp (alist)
  (dolist (pair alist t)
    (declare (type cons pair))
    (when (minusp (cdr pair))
      (return nil))))

(defun cons-count (x)
  (do ((n 0 (+ 1 (cons-count (carc x)) n))
       (x x (cdrc x)))
      ((atom x)
       n)))

(defun find-or-make-package (pkg)
  (cond
   ((packagep pkg)
    pkg)
   ((find-package pkg)
    )
   (t
    (cerror "Make a package named ~A." "There is no package named ~A." (string pkg))
    (make-package pkg :use '(:common-lisp)))))

(defun percentage (m n)
  (values (round (* 100 m) n)))

(defun print-time (year month date hour minute second &optional (destination *standard-output*) (basic nil))
  ;; per the ISO 8601 standard
  (format destination
          (if basic
              "~4D~2,'0D~2,'0DT~2,'0D~2,'0D~2,'0D"		;20020405T011216
              "~4D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D")		;2002-04-05T01:12:16
          year month date hour minute second))

(defun print-universal-time (utime &optional (destination *standard-output*) (basic nil))
  (mvlet (((values second minute hour date month year) (decode-universal-time utime)))
    (print-time year month date hour minute second destination basic)))

(defun print-current-time (&optional (destination *standard-output*) (basic nil))
  (print-universal-time (get-universal-time) destination basic))

(defun leap-year-p (year)
  (and (eql 0 (mod year 4))
       (implies (eql 0 (mod year 100))
                (eql 0 (mod year 400)))))

(defun days-per-month (month year)
  (let ((month (month-number month)))
    (cl:assert month)
    (case month
      (2
       (if (leap-year-p year) 29 28))
      ((4 6 9 11)
       30)
      (otherwise
       31))))

(defun month-number (month)
  (cond
   ((or (symbolp month) (stringp month))
    (cdr (assoc (string month)
                '(("JAN" . 1) ("JANUARY" . 1)
                  ("FEB" . 2) ("FEBRUARY" . 2)
                  ("MAR" . 3) ("MARCH" . 3)
                  ("APR" . 4) ("APRIL" . 4)
                  ("MAY" . 5)
                  ("JUN" . 6) ("JUNE" . 6)
                  ("JUL" . 7) ("JULY" . 7)
                  ("AUG" . 8) ("AUGUST" . 8)
                  ("SEP" . 9) ("SEPTEMBER" . 9)
                  ("OCT" . 10) ("OCTOBER" . 10)
                  ("NOV" . 11) ("NOVEMBER" . 11)
                  ("DEC" . 12) ("DECEMBER" . 12))
                :test #'string-equal)))
   ((and (integerp month) (<= 1 month 12))
    month)
   (t
    nil)))

(defun print-args (&rest args)
  (declare (dynamic-extent args))
  (print args)
  nil)

(defmacro define-plist-slot-accessor (type name)
  (let ((fun (intern (format nil "~A-~A" type name) :snark))
        (plist (intern (format nil "~A-~A" type :plist) :snark)))
    `(progn
       (#-(or allegro lispworks) definline #+(or allegro lispworks) defun ,fun (x)
         (getf (,plist x) ',name))
       (defun (setf ,fun) (value x)
         (if (null value)
             (progn (remf (,plist x) ',name) nil)
             (setf (getf (,plist x) ',name) value))))))

(defvar *print-pretty2* nil)

(defmacro with-standard-io-syntax2 (&body forms)
  (let ((pkg (gensym)))
    `(let ((,pkg *package*))
       (with-standard-io-syntax
         (let ((*package* ,pkg)
               (*print-case* :downcase)
               (*print-pretty* *print-pretty2*)
;;             #+ccl (ccl:*print-abbreviate-quote* nil)
;;             #+cmu (pretty-print::*print-pprint-dispatch* (pretty-print::make-pprint-dispatch-table))
;;             #+sbcl (sb-pretty::*print-pprint-dispatch* (sb-pretty::make-pprint-dispatch-table))
               #+(or abcl clisp) (*print-readably* nil)	;stop clisp from printing decimal points, #1=, etc
               )
           ,@forms)))))

(defun quit ()
  #+(or ccl cmu sbcl clisp lispworks) (common-lisp-user::quit)
  #+allegro (excl::exit))

;;; lisp.lisp EOF
