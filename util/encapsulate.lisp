;;;-*-Mode: LISP; Package: ENCAPSULATE -*-
;;;
;;; Copyright 1994-2009 Clozure Associates
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

;;; Adapted for ABCL by Alan Ruttenberg (alanruttenberg@gmail.com)
;;; Differs from CCL version in that trace will search across packages
;;; for symbols with the same name, if the symbol doesn't exist in the
;;; current package

(defpackage "ENCAPSULATE" (:USE "CL" "SYSTEM")
	    (:export "TRACE" "UNTRACE" "ADVISE" "UNADVISE" "ARGLIST" "ARGS"))

(in-package "ENCAPSULATE")

(shadowing-import '(trace untrace advise unadvise arglist args) 'cl-user)

(defvar *loading-removes-encapsulation* nil
  "If true, loading a new method definition from a file will remove any tracing and advice on the method")

#-abcl(defvar *trace-pfun-list* nil)
(defvar *trace-enable* t)
(defvar *trace-level* 0)
(defparameter *trace-max-indent* 40)
(defvar *trace-print-level* :default)
(defvar *trace-print-length* :default)
;(defparameter *trace-define-if-undefined* nil)
(defparameter *trace-bar-frequency* nil)
(defvar *trace-hook* nil)
(defvar *untrace-hook* nil)
(defvar *trace-print-hook* nil)

;;;
;;;  We support encapsulating three types of objects, i.e. modifying their definition
;;;  without changing their identity:
;;;    1. symbol - via symbol-function 
;;;    2. method - via %method-function 
;;;    3. standard-generic-function - via the mop::funcallable-instance-function
;;;
;;; Encapsulation is effected by creating a new compiled function and storing it in the
;;; slot above. The new function references a gensym fbound to the original definition
;;; (except in the case of a gf, the gensym is fbound to a copy of the gf which in
;;; turn contains the original dcode, since we can't invoke the dcode directly).
;;; In addition, an ENCAPSULATION struct describing the encapsulation is created and
;;; stored in the *encapsulation-table* with the new compiled function as the key.

(defparameter *encapsulation-table*
  (make-hash-table :test #'eq :rehash-size 2 :size 2 :weakness :key-and-value))

(defstruct (encapsulation)
  symbol         ; the uninterned name containing original def
  type           ; trace or advise
  spec           ; the original function spec
  advice-name    ; optional
  advice-when    ; :before, :after, :around 
  owner          ; where encapsulation is installed (can change)
  )

(defun find-method-by-names (name qualifiers specializers)
  (let ((gf (and (fboundp name) (symbol-function name))))
    (when gf
      (if (not (mop::std-generic-function-p gf))
        (error "~S is not a generic-function." gf)
        (let ((methods (mop::generic-function-methods gf)))
          (when methods
            (let* ((spec-len (length (mop::method-specializers (car methods))))
                   (new-specs (make-list spec-len :initial-element (find-class t))))
              (declare (dynamic-extent new-specs))
              (do ((specs specializers (cdr specs))
                   (nspecs new-specs (cdr nspecs)))
                  ((or (null specs) (null nspecs)))
                (let ((s (car specs)))
                  (rplaca nspecs (if (consp s) s (find-class s nil)))))
              (find-method gf qualifiers new-specs nil))))))))

(defun copy-method-function-bits (el new) t)
(defun remove-obsoleted-combined-methods (el) t)

(defun method-function (method)
  (or (slot-value method 'sys::fast-function)
      (mop::method-function method)))

(defsetf mop::funcallable-instance-function set-fif)

(defun set-fif (gf f) (mop::set-funcallable-instance-function gf f))


(defun %copy-function (original)
  (let* ((class (class-of original))
         (copy (allocate-instance class)))
    (dolist (slot (mapcar #'mop::slot-definition-name (mop::class-slots class)))
      (when (slot-boundp original slot)
        (setf (slot-value copy slot)
              (slot-value original slot))))
    (mop::set-funcallable-instance-function copy (mop::funcallable-instance-function original))
    copy))


(defun %fhave (sym fun)
  (setf (symbol-function sym) fun)
  fun)

(defun default-print-level (val)
  (if (eq val :default) *print-level* val))

(defun default-print-length (val)
  (if (eq val :default) *print-length* val))

(defun encapsulation-old-def (cap)
  (and (fboundp (encapsulation-symbol cap))
       (symbol-function (encapsulation-symbol cap))))

(defun non-nil-symbol-p (x)
  "Returns symbol if true"
  (if (symbolp x) x))

(defun setf-function-spec-name (spec)
  (if (setf-function-name-p spec)
    (let ((name (%setf-method (cadr spec))))
      (if (non-nil-symbol-p name)  ; this can be an anonymous function
        name
        (setf-function-name (cadr spec))))
    spec))

(defun trace-tab (direction &aux (n (min *trace-level* *trace-max-indent*)))
  (fresh-line *trace-output*)
  (dotimes (i (1- n))
    (declare (fixnum i))
    (write-char (if (and *trace-bar-frequency* 
                         (eq 0 (mod i *trace-bar-frequency*)))
                  #\| #\Space) *trace-output*))
  (if (eq direction :in)
    (format *trace-output* "~d> " (1- *trace-level*))
    (format *trace-output* "<~d " (1- *trace-level*))))

(defun trace-before  (&rest args)
  (declare (dynamic-extent args))
  (trace-tab :in)
  (let* ((*print-level* (default-print-level *trace-print-level*))
         (*print-length* (default-print-length *trace-print-length*))
         (*print-readably* nil))
    (format *trace-output* "Calling ~S ~%" args)
    (force-output *trace-output*)))

(defun trace-after (sym &rest args &aux (n (length args)))
  (declare (dynamic-extent args))
  (let* ((*print-level* (default-print-level *trace-print-level*))
         (*print-length* (default-print-length *trace-print-length*))
         (*print-readably* nil))
    (if (eq n 1)
      (progn
        (trace-tab :out)
        (format *trace-output* "~S returned ~S~%" sym (car args)))
      (progn
        (trace-tab :out)
        (format *trace-output* "~S returned ~S values :" sym n)
        (dolist (val args)
          (trace-tab :out)
          (format *trace-output* "     ~S" val))))
    (force-output *trace-output*)))

(defun forget-encapsulations (name)
  (when (%traced-p name)
    (format t "~%... Untracing ~a" name) 
    (%untrace-1 name))
  (when (%advised-p name)
    (format t "~%... Unadvising ~a" name) 
    (%unadvise-1 name))
  nil)

(defun function-encapsulated-p (fn-or-method)
  (get-encapsulation fn-or-method))

(defun %encap-fboundp (thing)
  (etypecase thing
    (symbol (and (fboundp thing) (symbol-function thing)))
    (method (method-function thing))))
  
(defun %encap-binding (thing)
  (require-type (etypecase thing
                  (symbol (and (fboundp thing) (symbol-function thing)))
                  (method (method-function thing)))
                'function))

(defun get-encapsulation (spec)
  (let* ((key (typecase spec
                (symbol (let* ((def (and (fboundp spec) (symbol-function spec))))
			  (if (mop::std-generic-function-p def)
			      (mop::funcallable-instance-function def)
			      def)))
		(method (method-function spec))
		(standard-generic-function (mop::funcallable-instance-function spec))
		(function spec)))
	 (cap (gethash key *encapsulation-table*)))
    #+gz (assert (or (null cap)
		     (let ((fn (%encap-binding (encapsulation-owner cap))))
		       (eq (if (standard-generic-function-p fn) (mop::funcallable-instance-function fn) fn) key))))
    cap))

(defun set-encapsulation-owner (fn owner)
  (let ((cap (get-encapsulation fn)))
    (when cap
      (setf (encapsulation-owner cap) owner))))

(defun put-encapsulation (fn cap)
  (let* ((owner (encapsulation-owner cap))
         (old-def (%encap-binding owner))
         (newsym (encapsulation-symbol cap)))
    (setf (gethash fn *encapsulation-table*) cap)
    (setf (getf (sys::function-plist fn) :capsule) cap)
    (set-encapsulation-owner old-def newsym)
    (etypecase owner
      (symbol
       (cond ((mop::std-generic-function-p old-def)
              (%fhave newsym (%copy-function old-def))
              (setf (mop::funcallable-instance-function old-def) fn))
             (t
              (%fhave newsym old-def)
              (%fhave owner fn))))
      (method
       (%fhave newsym old-def)
       #-abcl
       (if (mop::std-slot-value owner 'sys::fast-function)
	   (setf (mop::std-slot-value owner 'sys::fast-function) fn)
	   (progn
	     '(error "Sorry, tracing methods with other than simple argument lists and no next-methods doesn't work yet")
	     (setf (mop::std-slot-value owner 'sys::%function) fn)))
       (mop::finalize-standard-generic-function (mop::method-generic-function owner))
#-abcl (setf (mop::method-function owner) fn)
       (remove-obsoleted-combined-methods owner)))))

(defun remove-encapsulation (cap)
  (let* ((owner (encapsulation-owner cap))
         (cur-def (%encap-fboundp owner))
         (old-def (encapsulate::encapsulation-old-def cap)))
    (typecase owner
      (symbol
       (cond ((or (null cur-def)
			     (not (eq cap (get-encapsulation cur-def))) )
              ;; rebound behind our back, oh well.
              nil)
             ((mop::std-generic-function-p cur-def)
              (remhash (mop::funcallable-instance-function cur-def) *encapsulation-table*)
              (encapsulate::set-encapsulation-owner old-def owner)
              (setf (mop::funcallable-instance-function cur-def) (mop::funcallable-instance-function old-def)))
             (t (remhash cur-def *encapsulation-table*)
		(encapsulate::set-encapsulation-owner old-def owner)
		(%fhave owner old-def))))
      (method
       (remhash cur-def *encapsulation-table*)
       (encapsulate::set-encapsulation-owner old-def owner)
       (if (mop::std-slot-value owner 'sys::fast-function)
	   (setf (mop::std-slot-value owner 'sys::fast-function) old-def)
	   (progn
	     '(error "Sorry, tracing methods with other than simple argument lists and no next-methods doesn't work yet")
	     (setf (mop::std-slot-value owner 'sys::%function) old-def)))
       (mop::finalize-standard-generic-function (mop::method-generic-function owner))
       (remove-obsoleted-combined-methods owner)))))


(defun encapsulate (owner newdef type spec newsym &optional advice-name advice-when)
  (let ((cap (make-encapsulation
	      :owner owner
	      :symbol newsym
	      :type type
	      :spec spec
	      :advice-name advice-name
	      :advice-when advice-when)))
    (put-encapsulation newdef cap)
    cap))

(defun find-unencapsulated-definition (fn)
  (when fn
    (loop for cap = (get-encapsulation fn) while cap
      do (setq fn (encapsulation-old-def cap)))
    fn))

(defun set-unencapsulated-definition (cap newdef)
  (loop for owner = (encapsulation-symbol cap)
    do (setq cap (get-encapsulation owner)) while cap
    finally (%fhave owner newdef)))

(defun %encapsulation-thing (spec &optional define-if-not (error-p t))
  ;; Returns either an fboundp symbol or a method, or nil.
  (typecase spec
    (symbol
     ;; weed out macros and special-forms
     (if (or (null spec) (special-operator-p spec) (macro-function spec))
	 (if error-p
	     (error "Cannot trace or advise ~a~S"
		    (cond ((null spec) "")
			  ((special-operator-p spec) "special operator ")
			  (t "macro "))
		    spec)
	     nil)
	 (if (or (and (fboundp spec) (symbol-function spec))
		 (and define-if-not
		      (progn
			(warn "~S was undefined" spec)
			(%fhave spec (%function 'trace-null-def))
			t)))
	     spec
	     (let ((maybe-elsewhere 
		     (loop for pkg in (list-all-packages)
			   for found = (find-symbol (symbol-name spec) pkg)
			   for fboundp = (and found (fboundp found))
			   when (and fboundp (eq (symbol-package found) pkg))
			     collect (find-symbol (symbol-name spec) pkg))))
	       (let ((*print-case* :downcase)
		     (*print-escape* t))
		 (cond ((null maybe-elsewhere)
			(if error-p
			    (error "~S is undefined." spec)
			    nil))    
		       ((= (length maybe-elsewhere) 1)
			(format t "assuming ~s rather than ~s"
				(car maybe-elsewhere)
				spec)
			(return-from %encapsulation-thing (car maybe-elsewhere)))
		       (t (error "Ambiguous, could be ~{~s~^ or ~}" maybe-elsewhere))))))))
    (method spec)
    (cons
     (case (car spec)
       (:method 
        (let ((gf (cadr spec))
              (qualifiers (butlast (cddr spec)))
              (specializers (car (last (cddr spec))))
              method)
          (setq specializers (require-type specializers 'list))
          (prog ()
            AGN
            (cond ((setq method
                         (find-method-by-names gf qualifiers specializers))
                   (return method))
                  (define-if-not
                    (when (define-undefined-method spec gf qualifiers specializers)
                      (go AGN)))
                  (t (if error-p
                       (error "Method ~s qualifiers ~s specializers ~s not found."
                              gf qualifiers specializers)
                       (return nil)))))))
       (setf
        (let ((name-or-fn (setf-function-spec-name spec)))
          (cond ((symbolp name-or-fn) (%encapsulation-thing name-or-fn))
                ((functionp name-or-fn) ; it's anonymous - give it a name
                 (let ((newname (gensym)))
                   (%fhave newname name-or-fn)
                   (store-setf-method (cadr spec) newname)
                   newname)))))))
    (t (if error-p
         (error "Invalid trace spec ~s" spec)
         nil))))

(defun trace-null-def (&rest ignore)
  (declare (ignore ignore)))

(defun define-undefined-method (spec gf qualifiers specializers)
  (let (vars def)    
    (flet ((blob (e)
                 (let ((v (gensym)))
                   (push v vars)
                   (list v e))))
      (declare (dynamic-extent #'blob))
      (setq def
            (let ((lambda-list (mapcar #' blob specializers)))
              (eval
               `(defmethod ,gf ,@qualifiers (,@lambda-list &rest ignore)
                  (declare (ignore ignore ,@vars))))))
      (when def (warn "~S was undefined" spec))
      def)))

(defun traceable-symbol-p (sym)
  (and sym
       (not (special-operator-p sym))
       (not (macro-function sym))
       (and (fboundp sym) (symbol-function sym))))

(defun %trace-package (pkg &rest args)
  (declare (dynamic-extent args))
  (do-symbols (sym pkg)
    ;; Don't auto-trace imported symbols, because too often these are imported
    ;; system functions...
    (when (eq (symbol-package sym) pkg)
      (when (traceable-symbol-p sym)
        (apply #'trace-function sym args))
      (when (or (%setf-method sym)
                ;; Not really right.  Should construct the name if doesn't exist.
                ;; But that would create a lot of garbage for little gain...
                (let ((name (existing-setf-function-name sym)))
                  (traceable-symbol-p name)))
        (apply #'trace-function `(setf ,sym) args)))))

(defun trace-print-body (print-form)
  (when print-form
    (if (and (consp print-form) (eq (car print-form) 'values))
      `((mapcar #'(lambda (name object)
                    (trace-tab :in)
                    (format *trace-output* "~s = ~s" name object))
         ',(cdr print-form)
         (list ,@(cdr print-form))))
      `((let ((objects (multiple-value-list ,print-form))
              (i -1))
          (if (and objects (not (cdr objects)))
            (progn
              (trace-tab :in)
              (format *trace-output* "~s = ~s" ',print-form (car objects)))
            (dolist (object objects)
              (trace-tab :in)
              (format *trace-output* "~s [~d] = ~s" ',print-form (incf i) object))))))))

(defun trace-backtrace-body (test-form)
  (when test-form
    `((let ((test ,test-form))
        (when test
          (multiple-value-bind (detailed-p count)
              (cond ((member test '(:detailed :verbose :full))
                     (values t nil))
                    ((integerp test)
                     (values nil test))
                    ((and (consp test)
                          (keywordp (car test))
                          (consp (cdr test))
                          (null (cddr test)))
                     (values (member (car test) '(:detailed :verbose :full))
                             (and (integerp (cadr test)) (cadr test))))
                    (t (values nil nil)))
            (let ((*debug-io* *trace-output*))
              (print-call-history :detailed-p detailed-p
                                  :count (or count most-positive-fixnum))
              (terpri *trace-output*))))))))

;; (defun trace-inside-frame-p (name)
;;   (if (packagep name)
;;     (map-call-frames #'(lambda (p context)
;;                          (declare (ignore context))
;;                          (let* ((fn (cfp-lfun p))
;;                                 (fname (and fn (function-name fn)))
;;                                 (sym (typecase fname
;;                                        (method (method-name fname))
;;                                        (cons (and (setf-function-name-p fname) (cadr fname)))
;;                                        (symbol fname)
;;                                        (t nil))))
;;                            (when (and sym (eq (symbol-package sym) name))
;;                              (return-from trace-inside-frame-p t)))))
;;     (let ((fn (%encap-binding name)))
;;       (when fn
;;         (map-call-frames #'(lambda (p context)
;;                              (declare (ignore context))
;;                              (when (eq (cfp-lfun p) fn)
;;                                (return-from trace-inside-frame-p t))))))))

(defun trace-package-spec (spec)
  (when (or (stringp spec)
            (packagep spec)
            (and (consp spec) (eq (car spec) :package)))
    (let ((pkg (if (consp spec)
                 (destructuring-bind (pkg) (cdr spec) pkg)
                 spec)))
      (pkg-arg pkg))))

(defun trace-function (spec &rest args &key before after methods
                            (if t) (before-if t) (after-if t)
                            print print-before print-after
                            eval eval-before eval-after
                            break break-before break-after
                            backtrace backtrace-before backtrace-after
                            inside
                            define-if-not
                            ;; Some synonyms, just to be nice
                            (condition t) (if-before t) (if-after t) (wherein nil))

  (declare (dynamic-extent args))
  
  (let ((pkg (trace-package-spec spec)))
    (when pkg
      (return-from trace-function (apply #'%trace-package pkg args))))

  ;; A little bit of dwim, after all this _is_ an interactive tool...
  (unless (eq condition t)
    (setq if (if (eq if t) condition `(and ,if ,condition))))
  (unless (eq if-before t)
    (setq before-if (if (eq before-if t) if-before `(and ,before-if ,if-before))))
  (unless (eq if-after t)
    (setq after-if (if (eq after-if t) if-after `(and ,after-if ,if-after))))
  (when (and inside (trace-spec-p inside))
    (setq inside (list inside)))
  (when wherein
    (setq inside (append inside (if (trace-spec-p wherein) (list wherein) wherein))))
  (case break
    (:before (setq break-before (or break-before t) break nil))
    (:after (setq break-after (or break-after t) break nil)))
  (case backtrace
    (:before (setq backtrace-before (or backtrace-before t) backtrace nil))
    (:after (setq backtrace-after (or backtrace-after t) backtrace nil)))
  (case before
    (:break (setq before :print break-before t))
    (:backtrace (setq before :print backtrace-before t)))
  (case after
    (:break (setq after :print break-after t))
    (:backtrace (setq after :print backtrace-after t)))

  (when break
    (setq break-before (if break-before
                         `(and ,break ,break-before)
                         break))
    (setq break-after (if break-after
                        `(and ,break ,break-after)
                        break)))
  (unless backtrace-before
    (setq backtrace-before backtrace))
  (when (and (consp backtrace-before) (keywordp (car backtrace-before)))
    (setq backtrace-before `',backtrace-before))
  (when (and (consp backtrace-after) (keywordp (car backtrace-after)))
    (setq backtrace-after `',backtrace-after))

  (when (and (null before) (null after))
    (setq before :print)
    (setq after :print))
  (when (and (null before) backtrace-before)
    (setq before :print))

  (case before
    ((:print :default) (setq before #'trace-before)))
  (case after
    ((:print :default) (setq after #'trace-after)))

  (when (or (non-nil-symbol-p before) (functionp before))
    (setq before `',before))
  (when (or (non-nil-symbol-p after) (functionp after))
    (setq after `',after))

  (when inside
    (let ((tests (loop for spec in inside
                       as name = (or (trace-package-spec spec)
                                     (%encapsulation-thing spec nil nil)
                                     (error "Cannot trace inside ~s" spec))
                       collect `(trace-inside-frame-p ',name))))
      (setq if `(and ,if (or ,@tests)))))

  (setq eval-before `(,@(trace-print-body print-before)
                      ,@(trace-print-body print)
                      ,@(and eval-before `(,eval-before))
                      ,@(and eval `(,eval))
                      ,@(and before `((apply ,before ',spec args)))
                      ,@(trace-backtrace-body backtrace-before)
                      ,@(and break-before `((when ,break-before
                                              (force-output *trace-output*)
                                              (break "~s trace entry: ~s" ',spec args))))))
  (setq eval-after `(,@(trace-backtrace-body backtrace-after)
                     ,@(and after `((apply ,after ',spec vals)))
                     ,@(and eval `(,eval))
                     ,@(and eval-after `(,eval-after))
                     ,@(trace-print-body print)
                     ,@(trace-print-body print-after)
                     ,@(and break-after `((when ,break-after
                                            (force-output *trace-output*)
                                            (break "~s trace exit: ~s" ',spec vals))))))

  (prog1
      (block %trace-block
        ;;
        ;; see if we're a callback
        ;;
        #-abcl
	(when (and (typep spec 'symbol)
                   (boundp spec)
                   (macptrp (symbol-value spec)))
          (let ((len (length %pascal-functions%))
                (sym-name (symbol-name spec)))
            (declare (fixnum len))
            (dotimes (i len)
              (let ((pfe (%svref %pascal-functions% i)))
                (when (and (vectorp pfe)
                           (string= sym-name (symbol-name (pfe.sym pfe))))
                  (when backtrace
                    (if (null before)
			(setq before :print)))
                  (setf (pfe.trace-p pfe)
                        `(,@(if before `((:before . ,before)))
                          ,@(if after `((:after . ,after)))
                          ,@(if backtrace `((:backtrace . ,backtrace)))))
                  (push spec *trace-pfun-list*)))))
          (return-from %trace-block))
        ;;
        ;; now look for traceable methods.
        ;; It's possible, but not likely, that we will be both
        ;; a callback and a function or method, if so we trace both.
        ;; This isn't possible.
        ;; If we're neither, signal an error.
        ;;
        (let* ((trace-thing (%encapsulation-thing spec define-if-not)) def)
          (%untrace-1 trace-thing)
          (setq def (%encap-binding trace-thing))
          (when (and methods (typep def 'standard-generic-function))
            (dolist (m (%gf-methods def))
              (apply #'trace-function m args)))
          #+old
          (when step               ; just check if has interpreted def
            (if (typep def 'standard-generic-function)
              (let ((methods (%gf-methods def)))
                ; should we complain if no methods? naah
                (dolist (m methods) ; stick :step-gf in advice-when slot
                  (%trace m :step t)
                  (let ((e (function-encapsulation m)))
                    (when e (setf (encapsulation-advice-when e) :step-gf))))
                ; we choose to believe that before and after are intended for the gf
                (if  (or before after)
                  (setq step nil)                
                  (return-from %trace-block)))
              #|(uncompile-for-stepping trace-thing nil t)|#))
          (let* ((newsym (gensym "TRACE"))
                 (method-p (and (typep trace-thing 'method) (not (mop::std-method-fast-function trace-thing))))
                 (newdef (trace-global-def 
                          spec newsym if before-if eval-before after-if eval-after method-p)))
            (when method-p
              (copy-method-function-bits def newdef))
            (encapsulate trace-thing newdef 'trace spec newsym))))
    (when *trace-hook*
      (apply *trace-hook* spec args))))


(defun %traced-p (thing)
  (let ((cap (get-encapsulation thing)))
    (and cap (eq (encapsulation-type cap) 'trace))))

(defmacro untrace (&rest syms)
  "Remove tracing from the specified functions. With no args, untrace all
   functions."
  (if syms
    `(%untrace-0 ',syms)
    `(%untrace-all)))

(defun %untrace-0 (syms)
  (let (val x)
    (dolist (symbol syms)
      (setq x (%untrace symbol))
      (when x (push x val)))
    val))

(defun %untrace-all ()
  #-abcl
  (dolist (pfun *trace-pfun-list*)
    (%untrace pfun)
    (when *untrace-hook*
      (funcall *untrace-hook* pfun)))
  (loop for cap being the hash-value of *encapsulation-table*
    when (eq (encapsulation-type cap) 'trace)
    collect (let ((spec (encapsulation-spec cap)))
              (remove-encapsulation cap)
              (when *untrace-hook*
                (funcall *untrace-hook* spec))
              spec)))

(defun %untrace (sym &aux val)
  (when (and (consp sym)(consp (car sym)))
    (setq sym (car sym)))
  (cond
   #-abcl((and (typep sym 'symbol)
         (boundp sym)
         (macptrp (symbol-value sym)))
    (%untrace-pfun sym))
   (t 
    (let* ((trace-thing (%encapsulation-thing sym))
           (def (%encap-binding trace-thing)))
      (when (typep def 'standard-generic-function)
        (let ((methods (mop::generic-function-methods def)))
          (dolist (m methods)
            (let ((cap (get-encapsulation m)))
              (when (and cap (eq (encapsulation-advice-when cap) :step-gf))
                (remove-encapsulation cap)
                (push m val))))))
      ; gf could have first been traced :step, and then just plain traced
      ; maybe the latter trace should undo the stepping??
      (let ((spec (%untrace-1 trace-thing)))
        (when spec
          (push spec val))))))
  (when *untrace-hook*
    (funcall *untrace-hook* sym))
  (if (null (cdr val)) (car val) val))

;; thing is a symbol or method - def is current definition
;; we already know its traced
(defun %untrace-1 (thing)
  (let ((cap (get-encapsulation thing)))
    (when (and cap (eq (encapsulation-type cap) 'trace))
      (remove-encapsulation cap)
      (encapsulation-spec cap))))

#-abcl
(defun %untrace-pfun (sym)
  (let ((len (length %pascal-functions%))
        (sym-name (symbol-name sym)))
    (declare (fixnum len))
    (dotimes (i len)
      (let ((pfe (%svref %pascal-functions% i)))
        (when (and (vectorp pfe)
                   (string= sym-name (symbol-name (pfe.sym pfe))))
          (setf (pfe.trace-p pfe) nil
                *trace-pfun-list* (remove sym *trace-pfun-list*))
          (return-from %untrace-pfun sym))))
    nil))



(defmacro trace (&rest syms)
  "TRACE {Option Global-Value}* { Name | (Name {Option Value}*) }*

TRACE is a debugging tool that provides information when specified
functions are called."
  (if syms
    (let ((options (loop while (keywordp (car syms))
                     nconc (list (pop syms) (pop syms)))))
      `(progn (%trace-0 ',syms ',options) (%trace-list)))
    `(%trace-list)))

(defun trace-spec-p (arg)
  (or (atom arg)
      (member (car arg) '(:method setf :package))))


(defun %trace-0 (syms &optional global-options)
  (dolist (spec syms)
    (if (trace-spec-p spec)
      (apply #'trace-function spec global-options)
      (apply #'trace-function (append spec global-options)))))

(defun %trace-list ()
  (let (res)
    (loop for x being the hash-value of *encapsulation-table*
	 when (eq (encapsulation-type x) 'trace)
	 do (push (encapsulation-spec x) res))
    #-abcl(dolist (x *trace-pfun-list*)
      (push x res))
    res))

(defmacro with-traces (syms &body body)
 `(unwind-protect
       (progn
         (let ((*trace-output* (make-broadcast-stream)))
           ;; if you're tracing ccl internals you'll get trace output as it encapsulates the
           ;; functions so hide all the trace output while eval'ing the trace form itself.
           (trace ,@syms))
         ,@body)
    (untrace ,@syms)))

;; this week def is the name of an uninterned gensym whose fn-cell is original def

;; method-p set to nil means that methods with fast functions will be
;; able to be traced.  However, to do this correctly it looks like we
;; have to trace both the fast-function, when it exists, and also the
;; method. For now we do it for the fast-function if there is one,
;; otherwise the method
;; If the fast function, then called like (apply fast-function arg1 arg2..)
;; If the method then called with (funcall method args next-empfun) 

(defun trace-global-def (sym def if before-if eval-before after-if eval-after &optional method-p)
  (let ((enable (gensym))
        do-it)
    (setq do-it
          (cond #+old (step
                       (setq step-it            
                             `(step-apply-simple ',def args))
                       (if (eq step t)
                         step-it
                         `(if (apply ',step ',sym args) ; gaak
                           ,step-it
                           ,(if (and before method-p)
                                `(apply-with-method-context ,saved-method-var (symbol-function ',def) args)
                                `(apply ',def args)))))
                (t 
		 (if method-p 
		     `(funcall ',def args next-emfun)
		     `(apply ',def args))
		 
		 )))
    (compile-named-function-warn
     `(lambda (,@(if method-p '(args next-emfun) '(&rest args)))
       (declare (dynamic-extent args))
       (declare (ftype function ,def))
       (let ((*trace-level* (1+ *trace-level*))
             (,enable ,if))
         (declare (special *trace-enable* *trace-level*))
         ,(when eval-before
           `(when (and ,enable ,before-if *trace-enable*)
             (when *trace-print-hook*
               (funcall *trace-print-hook* ',sym t))
             (let* ((*trace-enable* nil))
               ,@eval-before)
             (when *trace-print-hook*
               (funcall *trace-print-hook* ',sym nil))))
         ,(if eval-after
           `(let ((vals (multiple-value-list ,do-it)))
             (when (and ,enable ,after-if *trace-enable*)
               (when *trace-print-hook* 
                 (funcall *trace-print-hook* ',sym t))
               (let* ((*trace-enable* nil))
                 ,@eval-after)
               (when *trace-print-hook* 
                 (funcall *trace-print-hook* ',sym nil)))
             (values-list vals))
           do-it)))
     `(traced ,sym)
     :keep-symbols t)))

; &method var tells compiler to bind var to contents of next-method-context
(defun advise-global-def (def when stuff &optional method-p dynamic-extent-arglist)
  (let* ((callit `(if ,method-p
		      (funcall ',def args next-emfun)
		      (apply ',def args))))
    `(lambda ,(if method-p '(args next-emfun) '(&rest args))
       ,@(and dynamic-extent-arglist '((declare (dynamic-extent args))))
       (declare (ftype function ,def))
       (declare (ignorable args))
       (let ()
         ,(ecase
            when
            (:before
             `(block nil
                ,stuff                  
                (return ,callit)))
            (:after         
             `(block nil
                (let ((values (multiple-value-list ,callit)))
                  ;(declare (dynamic-extent values))
                  ,stuff
                  (return (values-list values)))))
            (:around
	     `(macrolet ((:do-it ()
			   ,callit))
		    
                  (block nil
                    (return  ,stuff))))
	    )))))


(defun compile-named-function-warn (fn name &rest keys)
  (declare (dynamic-extent keys))
  (multiple-value-bind (result warnings) (let ((it (eval fn)))
					   (sys::%set-lambda-name it name)
					   it)

    (when warnings 
      (let ((first t))
        (dolist (w warnings)
          (signal-compiler-warning w first nil nil nil)
          (setq first nil))))
    result))

       
(defun %advised-p (thing)
  (loop for nx = thing then (encapsulation-symbol cap)
    as cap = (get-encapsulation nx) while cap
    thereis (eq (encapsulation-type cap) 'advice)))

(defun %advice-encapsulations (thing when advice-name)
  (loop for nx = thing then (encapsulation-symbol cap)
    as cap = (get-encapsulation nx) while cap
    when (and (eq (encapsulation-type cap) 'advice)
              (or (null when) (eq when (encapsulation-advice-when cap)))
              (or (null advice-name) (equal advice-name (encapsulation-advice-name cap))))
    collect cap))

(defun advise-2 (newdef newsym method-p function-spec when advice-name define-if-not)      
  (let* ((advise-thing (%encapsulation-thing function-spec define-if-not))
         orig-sym)
    (let ((capsules (%advice-encapsulations advise-thing when advice-name)))
      (when capsules 
        (unadvise-capsules capsules)))
    (when (%traced-p advise-thing)
      ; make traced call advised
      (setq orig-sym
            (encapsulation-symbol (get-encapsulation advise-thing))))
    (sys::%set-lambda-name newdef `(advised ',function-spec))
    (if method-p (copy-method-function-bits (%encap-binding advise-thing) newdef))
    (encapsulate (or orig-sym advise-thing) newdef 'advice function-spec newsym advice-name when)
    newdef))

(defmacro advise (function form &key (when :before) name define-if-not dynamic-extent-arglist)
  (let* ((newsym (gensym "ADVICE"))
         ; WAS typep advise-thing 'method
         (method-p (or (typep function 'method) ; can this happen?
                       (and (consp function)(eq (car function) :method))))
         (newdef (advise-global-def newsym when form method-p dynamic-extent-arglist)))
      `(advise-2 ,newdef ',newsym ,method-p ',function ',when ',name
                 ,define-if-not)))

(defmacro advisedp (function-spec &key when name)
  `(advisedp-1 ',function-spec ',when ',name))

(defun encapsulation-advice-spec (cap)
  (list (encapsulation-spec cap)
        (encapsulation-advice-when cap)
        (encapsulation-advice-name cap)))
  
(defun advisedp-1 (function-spec when name)
  (cond ((eq t function-spec)
         (loop for c being the hash-value of *encapsulation-table*
           when (and (eq (encapsulation-type c) 'advice)
                     (or (null when)(eq when (encapsulation-advice-when c)))
                     (or (null name)(equal name (encapsulation-advice-name c))))
           collect (encapsulation-advice-spec c)))
        (t (let* ((advise-thing (%encapsulation-thing function-spec))
                  (capsules (%advice-encapsulations advise-thing when name)))
             (mapcar #'encapsulation-advice-spec capsules)))))

(defun %unadvise-1 (function-spec &optional when advice-name ignore)
  (declare (ignore ignore))
  (let ((advise-thing (%encapsulation-thing function-spec)))
    (let ((capsules (%advice-encapsulations advise-thing when advice-name)))
      (when capsules (unadvise-capsules capsules)))))

(defun unadvise-capsules (capsules)
  (let (val)
    (dolist (capsule capsules)
        (push (encapsulation-advice-spec capsule) val)
        (remove-encapsulation capsule))
    val))

(defmacro unadvise (function &key when name)
  (cond ((not (eq function t))
         `(%unadvise-1 ',function ',when ',name))
        (t `(%unadvise-all ',when ',name))))

(defun %unadvise-all (&optional when name)
  (loop for cap being the hash-value of *encapsulation-table*
    when (and (eq (encapsulation-type cap) 'advice)
              (or (null when)(eq when (encapsulation-advice-when cap)))
              (or (null name)(equal name (encapsulation-advice-name cap))))
    collect (progn
              (remove-encapsulation cap)
              (encapsulation-advice-spec cap))))

;;; ****************************************************************
;; NOT YET IMPLEMENTED IN ABCL
;;; Interaction with the rest of the system to make sure advise and trace stick


(in-package :system)

(defun get-encapsulated-by-name (name)
  (maphash (lambda(k v) 
	     (when (eq (encapsulate::encapsulation-spec v) name)
	       (return-from get-encapsulated-by-name k)))
	   encapsulate::*encapsulation-table*))


;; Called from fset, after the new symbol function has been set
(defun trace-redefined-update (name newdef)
  (assert (not (encapsulate::get-encapsulation newdef)))
  (let ((old-def (get-encapsulated-by-name name)) cap)
    (when (and old-def (setq cap (encapsulate::get-encapsulation old-def)))
      (cond ((or (and *load-truename* encapsulate::*loading-removes-encapsulation*)
                 ;; redefining a gf as a fn.
                 (typep *load-truename* 'standard-generic-function))
             (encapsulate::forget-encapsulations old-def)
             nil)
            (t (encapsulate::set-unencapsulated-definition cap newdef)
	       (setf (symbol-function name) old-def)
               T)))))

(defvar *inside-set-funcallable-instance-function* nil)
  
;; (advise set-funcallable-instance-function :around
;; 	(let ((instance (car args))
;; 	      (new-fn (cdr args)))
	  
	
;(defun trace-generic-function-redefined-update ()
;(defun trace-method-redefined-update ()

(defun move-funcallable-instance-encapsulations-maybe (instance new-fn &aux cap)
  (let ((old-fn (mop::funcallable-instance-function instance)))
    (unless (eq old-fn new-fn)
    (cond ((and encapsulate::*loading-removes-encapsulation* *load-truename*)
           (when (encapsulate::%traced-p old-fn)
             (warn "~%... Untracing ~s" (encapsulate::%untrace-1 old-fn)))
           (when (encapsulate::%advised-p old-fn)
             (format t "~%... Unadvising ~s" (encapsulate::%unadvise-1 old-fn))))
          (t (when (setq cap (encapsulate::get-encapsulation old-fn))
               (let* ((old-inner-def (encapsulate::find-unencapsulated-definition oldmethod))
                      )
                 ;; make last encapsulation call new definition
                 (set-unencapsulated-definition cap new-fn)

                 (setf (%method-function newmethod) olddef)
                 (set-encapsulation-owner olddef newmethod)
                 (setf (%method-function oldmethod) old-inner-def)
                 (loop
                   for def = olddef then (encapsulation-old-def cap)
                   for cap = (encapsulate::get-encapsulation def) while cap
                   do (copy-method-function-bits newdef def)))))))))


;  whenever we set-funcallable-instance-function
;  check if the current instance is encapsulated and fix as necesary
  
#-abcl
;; Called from clos when change dcode
(defun sys::%set-encapsulated-gf-dcode (gf new-dcode)
  (loop with cap = (get-encapsulation gf)
    for gf-copy = (encapsulation-old-def cap)
    as cur-dcode = (mop::funcallable-instance-function gf-copy)
    do (setq cap (get-encapsulation cur-dcode))
    ;; refresh all the gf copies, in case other info in gf changed
    do (%copy-function gf gf-copy)
    do (setf (mop::funcallable-instance-function gf-copy) (if cap cur-dcode new-dcode))
    while cap))

#-abcl
;; Called from clos when oldmethod is being replaced by newmethod in a gf.
(defun sys::%move-method-encapsulations-maybe (oldmethod newmethod &aux cap)
  (unless (eq oldmethod newmethod)
    (cond ((and encapsulate::*loading-removes-encapsulation* *loading-files*)
           (when (%traced-p oldmethod)
             (warn "~%... Untracing ~s" (%untrace-1 oldmethod)))
           (when (%advised-p oldmethod)
             (format t "~%... Unadvising ~s" (%unadvise-1 oldmethod))))
          (t (when (setq cap (get-encapsulation oldmethod))
               (let* ((old-inner-def (find-unencapsulated-definition oldmethod))
                      (newdef (%method-function newmethod))
                      (olddef (%method-function oldmethod)))
                 ;; make last encapsulation call new definition
                 (set-unencapsulated-definition cap newdef)
                 (setf (%method-function newmethod) olddef)
                 (set-encapsulation-owner olddef newmethod)
                 (setf (%method-function oldmethod) old-inner-def)
                 (loop
                   for def = olddef then (encapsulation-old-def cap)
                   for cap = (get-encapsulation def) while cap
                   do (copy-method-function-bits newdef def))))))))

