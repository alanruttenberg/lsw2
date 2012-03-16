;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 1994-2001 Digitool, Inc
;;;   This file is part of OpenMCL.  
;;;
;;;   OpenMCL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License , known as the LLGPL and distributed with OpenMCL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;   which is distributed with OpenMCL as the file "LGPL".  Where these
;;;   conflict, the preamble takes precedence.  
;;;
;;;   OpenMCL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html


;;;  Modified by alanr Wed May 24, 2006 to be usable in abcl. None of
;;;  the generic function stuff works but I'll leave it there in case I want
;;;  to try to make it work
;;;  Advising setf methods not tried yet (probably won't work)

(defpackage "ENCAPSULATE" (:USE "CL" "SYSTEM")
	    (:export "TRACE" "UNTRACE" "ADVISE" "UNADVISE" "ARGLIST"))
(in-package encapsulate)
(shadowing-import '(trace untrace advise unadvise arglist) 'cl-user)

;; Lets try encapsulations
;; trace is here too
;; Make trace like 1.3, trace methods, trace (setf car)


(defvar *trace-alist* nil)
(defvar *trace-enable* t)
(defvar *trace-level* 0)
(defparameter *trace-max-indent* 40)
(defvar *trace-print-level* nil)
(defvar *trace-print-length* nil)
(defparameter *trace-bar-frequency* nil)

(defvar *advise-alist* nil)

(defparameter *encapsulation-table*
  (make-hash-table :test #'eq :rehash-size 2 :size 2))

(defstruct (encapsulation)
  symbol         ; the uninterned name containing original def
  type           ; trace or advise
  spec           ; the original function spec
  advice-name    ; optional
  advice-when    ; :before, :after, :around 
  owner          ; where encapsulation is installed
)

(defun setf-function-spec-name (spec)
  (if (and (consp spec) (eq (car spec) 'setf))
      (fdefinition spec) 
      spec))


(defun trace-tab (&aux (n (min *trace-level* *trace-max-indent*)))
  (fresh-line *trace-output*)
  (dotimes (i n)
    (declare (fixnum i))
    (write-char (if (and *trace-bar-frequency* 
			 (eq 0 (mod i *trace-bar-frequency*)))
		  #\| #\Space) *trace-output*)))

(defun trace-before  (&rest args)
  (declare (dynamic-extent args))
  (trace-tab)
  (let* ((*print-level* *trace-print-level*)
         (*print-length* *trace-print-length*)
         (*print-readably* nil))
    (format *trace-output* "Calling ~S ~%" args)
    (force-output *trace-output*)))

(defun trace-after (sym &rest args &aux (n (length args)))
  (declare (dynamic-extent args))
  (let* ((*print-level* *trace-print-level*)
         (*print-length* *trace-print-length*)
         (*print-readably* nil))
    (if (eq n 1)
      (progn
        (trace-tab)
        (format *trace-output* "~S returned ~S~%" sym (car args)))
      (progn
        (trace-tab)
        (format *trace-output* "~S returned ~S values :" sym n)
        (dolist (val args)
          (trace-tab)
          (format *trace-output* "     ~S" val))))
    (force-output *trace-output*)))

(defun forget-encapsulations (name)
  (when (%traced-p name)
    (format t "~%... Untracing ~a" name) 
    (%untrace-1 name))
  (when (%advised-p name nil nil t)
    (format t "~%... Unadvising ~a" name) 
    (unadvise-1 name))
  nil)

(defun function-encapsulated-p (fn-or-method)
  (typecase fn-or-method
    ((or method symbol cons)(function-encapsulation fn-or-method))
    (function
     (or (function-traced-p fn-or-method)
         (function-advised-p fn-or-method )))))

(defun function-traced-p (fn)
  (%function-in-alist fn *trace-alist*))

(defun function-advised-p (fn)
  (%function-in-alist fn *advise-alist*))                           

(defun %function-in-alist (def list)
  (dolist (cap list)
    (let ((symbol (encapsulation-owner cap)))
      (typecase symbol
        (symbol (when (eq (and (fboundp symbol) (symbol-function symbol)) def)
                  (return cap)))
        (method (when (eq (%method-function symbol) def)
                  (return cap)))
        (standard-generic-function
         (when (eq symbol def) (return cap)))))))

(defun function-encapsulation (spec)
  (typecase spec
    ((or symbol method)
     (gethash spec *encapsulation-table*))
    (function (function-encapsulated-p spec))
    (cons (gethash (setf-function-spec-name spec) *encapsulation-table*))))


(defun standard-generic-function-p (f)
  (typep f 'standard-generic-function))

(defun %fhave (sym fun)
  (setf (symbol-function sym) fun)
  fun)

(defun store-setf-method (name sym)
  (setf (get name 'system::setf-function)  sym))

(defun encapsulate (fn-spec old-def type trace-spec newsym
                            &optional advice-name advice-when)
  (let ((capsule (function-encapsulation fn-spec))
        gf-dcode old-encapsulation)
    (%fhave newsym
            (if nil;(standard-generic-function-p old-def)   ; stub out mcl version
		(progn (error "NOT YET")
		       (let ((dcode (%gf-dcode old-def)))
			 (setq gf-dcode
			       (if (and (combined-method-p dcode)
					(eq '%%call-gf-encapsulation
					    (function-name (%combined-method-dcode dcode))))
				   (let ((stuff (%combined-method-methods dcode)))
				     (setq old-encapsulation (car stuff))
				     (cdr stuff))
				   (cons old-def dcode)))
			 (setf (uvref old-def 0)(uvref *gf-proto* 0)) ; <<  gotta remember to fix it
			 (or old-encapsulation
			     (%cons-combined-method old-def gf-dcode #'%%call-encapsulated-gf))))
              old-def))                 ; make new symbol call old definition
    ;; move the encapsulation from fn-spec to sym    
    (cond (capsule (put-encapsulation newsym capsule)))    
    (put-encapsulation fn-spec
                       (make-encapsulation
                        :symbol newsym
                        :type type
                        :spec trace-spec
                        :advice-name advice-name
                        :advice-when advice-when))
    (values newsym gf-dcode)))
 

;; call with cap nil to remove - for symbol anyway
;; maybe advising methods is silly - just define a before method

(defun put-encapsulation (spec cap)
  (when cap
    (setf (encapsulation-owner cap) spec)
    (record-encapsulation cap)
    )
  (let ((key (typecase spec
               ((or symbol method standard-generic-function) spec)
               (cons (setf-function-spec-name spec))
               (t (report-bad-arg spec '(or symbol method cons))))))
    (if cap
      (setf (gethash key *encapsulation-table*) cap)
      (remhash key *encapsulation-table*)))
  cap)

(defmacro without-interrupts (&body body)
  `(progn ,@body))

(defun remove-encapsulation (capsule &optional dont-replace)
  ; optional don't replace is for unadvising, tracing all on a method
  (let (spec nextsym newdef def)
    (setq spec (encapsulation-owner capsule))
    (setq def (typecase spec
                (symbol (and (fboundp spec) (symbol-function spec)))
                (method spec)))
    (setq nextsym (encapsulation-symbol capsule))
    (setq newdef (and (fboundp nextsym) (symbol-function nextsym)))
    (without-interrupts
     (if nil; (standard-generic-function-p def) stub out mcl stuff
	 (progn (error "NOT YET")
		(if (and (combined-method-p newdef)
			 (eq '%%call-encapsulated-gf (function-name (%combined-method-dcode newdef))))
		    (let* ((orig-decode (require-type (cdr (%combined-method-methods newdef)) 'function))
			   (proto (cdr (assq orig-decode dcode-proto-alist)))
			   )		; <<
		      (setf (%gf-dcode def) orig-decode)
		      (setf (uvref def 0)(uvref (or proto *gf-proto*) 0)))
		    (setf (car (%combined-method-methods (%gf-dcode def))) newdef)))
       (typecase spec
         (symbol (%fhave spec newdef))
         (method (error "NOT YET")
		 (setf (%method-function spec) newdef)
                 (remove-obsoleted-combined-methods spec)
                 newdef)))
     (put-encapsulation spec
                        (if (null dont-replace)
                          (function-encapsulation nextsym)))
     (put-encapsulation nextsym nil)
     (unrecord-encapsulation capsule)
     )))

;; don't need in abcl because no fancy combined method stuff
(defun remove-obsoleted-combined-methods (spec)
  nil)



(defun record-encapsulation (capsule)
  (ecase (encapsulation-type capsule)
    (trace
     (when (not (ext:memq capsule *trace-alist*))
       (push capsule *trace-alist*)))
    (advice
     (when (not (ext:memq capsule *advise-alist*))
       (push capsule *advise-alist*)))))

(defun delq (thing list)
  (delete thing list :test 'eq))


(defun unrecord-encapsulation (capsule)
  (ecase (encapsulation-type capsule)
    (trace
      (setq *trace-alist* (delq capsule *trace-alist*)))
    (advice
     (setq *advise-alist* (delq capsule *advise-alist*)))))

(defun sys::untraced-function (sym)
  (encapsulate::find-unencapsulated-definition sym))

(defun find-unencapsulated-definition (spec)
  ;; spec is a symbol, function, or method object
  ;; returns a raw function ?? 
  (let (foo)
    (loop while (setq foo (function-encapsulation spec))
       do (setq spec (encapsulation-symbol foo)))
    (values
     (typecase spec
       (symbol (and (fboundp spec) (symbol-function spec)))
       (method (%method-function spec))
       (t spec))
     spec)))

(defun %trace-fboundp (spec)
  (typecase spec
    (symbol (and (fboundp spec) (symbol-function spec)))
    (method (%method-function spec))))


(defun %trace-function-spec-p (spec &optional define-if-not)
  ;; weed out macros and special-forms
  (typecase spec
    (symbol
     (when (or (null spec)(special-operator-p spec)(macro-function spec))
       (error "Cannot trace or advise ~S." spec))
;     (cl-user::print-db (and (fboundp spec) (symbol-function spec)))
     (let ((res (or (and (fboundp spec) (symbol-function spec))
		    (and define-if-not
                                       (progn (warn "~S was undefined" spec)
                                              (%fhave spec (%function 'trace-null-def)))))))
       (when (not res)(error "~S is undefined." spec))
       (values res spec)))
    (method
     (values (%method-function spec) spec))
    (cons
     (case (car spec)
       (:method 
        (let ((gf (cadr spec))
              (qualifiers (butlast (cddr spec)))
              (specializers (car (last (cddr spec))))
              method)
;	  (cl-user::print-db gf qualifiers specializers)
          (require-type specializers 'list)
          (prog ()
            AGN
            (cond ((setq method
                         (find-method-by-names gf qualifiers specializers))
                   (return (values (%method-function method) method)))
                  (define-if-not
                    (when (define-undefined-method spec gf qualifiers specializers)
                      (go AGN)))
                  (t (error "Method ~s qualifiers ~s specializers ~s not found."
                            gf qualifiers specializers))))))
       (setf
        (let ((name-or-fn (setf-function-spec-name spec)))
          (cond ((symbolp name-or-fn)(%trace-function-spec-p name-or-fn))
                ((functionp name-or-fn) ; its anonymous - give it a name
                 (let ((newname (gensym)))
                   (%fhave newname name-or-fn)
		   (store-setf-method (cadr spec) newname)
                   (values name-or-fn newname))))))))))

(defun find-method-by-names (gf qualifiers specializers)
  (and (symbol-function gf)
       (find-method (symbol-function gf) qualifiers specializers nil)))

    

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

(defun %trace (sym &key before after step define-if-not)  
  (let (def newdef trace-thing)
    (multiple-value-setq (def trace-thing) 
      (%trace-function-spec-p sym define-if-not))
    (if def
      (let ()
        (when (%traced-p trace-thing)
          (%untrace-1 trace-thing)
          (setq def (%trace-fboundp trace-thing)))
        (when step   ; just check if has interpreted def
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
                (return-from %trace)))
            #|(uncompile-for-stepping trace-thing nil t)|#))
        (let ((newsym (gensym "TRACE"))
              (method-p (typep trace-thing 'method)))
          (when (and (null before)(null after)(null step))
            (setq before #'trace-before)
            (setq after #'trace-after))
          (case before 
            (:print (setq before #'trace-before)))
          (case after
            (:print (setq after #'trace-after)))
          (setq newdef (trace-global-def 
                        sym newsym before after step method-p))
          (without-interrupts
           (multiple-value-bind (ignore gf-dcode) (encapsulate trace-thing def 'trace sym newsym)
             (declare (ignore ignore))
             (cond (gf-dcode 
                    (setf (%gf-dcode def)
                          (%cons-combined-method def (cons newdef gf-dcode) #'%%call-gf-encapsulation)))
                   ((symbolp trace-thing) (%fhave trace-thing newdef))
                   ((typep trace-thing 'method)
		    (SYSTEM:%SET-METHOD-FUNCTION  trace-thing newdef)
                    (remove-obsoleted-combined-methods trace-thing)
                    newdef))))))
      (error "Trace does not understand ~S." sym))))

;; sym is either a symbol or a method

(defun %traced-p (sym)
  (let ((foo (function-encapsulation sym)))
    (and foo (eq (encapsulation-type foo) 'trace))))

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


(defun %untrace (sym)
  (when (and (consp sym)(consp (car sym)))
    (setq sym (car sym)))
  (multiple-value-bind (def trace-thing) (%trace-function-spec-p sym)
    (let (val)
      (when (typep def 'standard-generic-function)
        (let ((methods (%gf-methods def)))
          (dolist (m methods)
            (let ((e (function-encapsulation m)))
              (when (and e (eq (encapsulation-advice-when e) :step-gf))
                (remove-encapsulation e)
                (push m  val))))))
      ; gf could have first been traced :step, and then just plain traced
      ; maybe the latter trace should undo the stepping??
      (when (%traced-p trace-thing)
        (%untrace-1 trace-thing)
        (push trace-thing val))
      (if (null (cdr val))(car val) val))))

(defun %untrace-all ()
  (let ((val nil))
    (dolist (cap *trace-alist*)
      (push (encapsulation-spec cap) val)
      (remove-encapsulation cap))
    val))

;; thing is a symbol or method - def is current definition
;; we already know its traced
(defun %untrace-1 (thing)
  (let (capsule)
    (setq capsule (function-encapsulation thing))
    ;; trace encapsulations must be first      
    (when (not (eq (encapsulation-type capsule) 'trace))
      (error "~S was not traced." thing))
    (remove-encapsulation capsule)
    (encapsulation-spec capsule)))


(defmacro trace (&rest syms)
  "TRACE {Option Global-Value}* {Name {Option Value}*}*

TRACE is a debugging tool that provides information when specified
functions are called."
  (if syms
    `(%trace-0 ',syms)
    `(%trace-list)))

(defun %trace-0 (syms)
  (dolist (symbol syms)
       (cond ((consp symbol)
              (cond ((null (cdr symbol))
                     (%trace (car symbol) :before :print :after :print))
                    ((ext:memq (car symbol) '(:method setf))
                     (%trace symbol :before :print :after :print))
                    (t (apply #'%trace symbol))))
             (t (%trace symbol :before :print :after :print)))))

(defun %trace-list ()
  (let (res)
    (dolist (x *trace-alist*)
      (push (encapsulation-spec x) res))
    res))


;; this week def is the name of an uninterned gensym whose fn-cell is original def

(defun trace-global-def (sym def before after step &optional method-p)
  (let ((saved-method-var (gensym)) do-it step-it)
    (when step
      (setq step-it            
            `(step-apply-simple ',def args)))
    (setq do-it
          (cond (step
                 (if (eq step t)
                   step-it
                   `(if (apply ',step ',sym args) ; gaak
                      ,step-it
                      ,(if (and before method-p)
                         `(apply-with-method-context ,saved-method-var (symbol-function ',def) args)
                         `(apply ',def args)))))
                (t (if (and before method-p)
                     `(apply-with-method-context ,saved-method-var (symbol-function ',def) args)
                     `(apply ',def args)))))
    (flet ((quoted-p (x)
             (and (consp x)
                  (case (car x)
                    ((function quote) t)))))
      (compile-named-function-warn
       `(lambda (,@(if (and before method-p)
                     `(&method ,saved-method-var))
                 &rest args) ; if methodp put &method on front of args - vs get-saved-method-var?
          (declare (dynamic-extent args))
          (let ((*trace-level* (1+ *trace-level*)))
            (declare (special *trace-enable* *trace-level*))
            ,(if before
               `(when *trace-enable*
                  (let* ((*trace-enable* nil))
                    ,(cond
                      ((eq before :break)
                       `(progn (apply #'trace-before ',sym args)
                               (break "~S" args)))
                      (t `(apply ,(if (quoted-p before) before `',before) ',sym args))))))           
            ,(if after
               `(let ((vals (multiple-value-list ,do-it)))
                  (when *trace-enable*
                    (let* ((*trace-enable* nil))
                      ,(cond ((eq after :break)
                              `(progn
                                 (apply #'trace-after ',sym vals)
                                 (break "~S" vals)))
                             (t `(apply ,(if (quoted-p after) after `',after) ',sym  vals)))))
                  (values-list vals))
               do-it)))
       `(traced ,sym)))))

; &method var tells compiler to bind var to contents of next-method-context
(defun advise-global-def (function-spec def when stuff &optional method-p)
  (declare (ignore function-spec))
  (let* ((saved-method-var (gensym)))
    (compile nil
    `(lambda (,@(if (and method-p (not (eq when :after)))
                  `(&method ,saved-method-var))
              &rest arglist)
       (declare (dynamic-extent arglist))
       (let ()
         ,(ecase
            when
            (:before
             `(block nil
                ,stuff                  
                (return ,(if method-p
                           `(apply-with-method-context ,saved-method-var (symbol-function ',def) arglist)
                           `(apply ',def arglist)))))
            (:after         
             `(block nil
                (let ((values (multiple-value-list (apply (function ,def) arglist))))
                  (declare (dynamic-extent values))
                  ,stuff
                  (return (values-list values)))))
            (:around
             ;; stuff is e.g. (+ 5 (:do-it))
             (if method-p 
               `(macrolet ((:do-it ()
                             `(apply-with-method-context ,',saved-method-var 
                                                         (symbol-function ',',def)
                                                         arglist)))
                  (block nil
                    (return  ,stuff)))
               `(macrolet ((:do-it ()
                             `(apply (function ,',def) arglist)))
                  (block nil
                    (return  ,stuff)))))))))))


(defun compile-named-function-warn (fn name)
  (multiple-value-bind (result warnings)(compile nil fn)    
    (SYSTEM::%SET-LAMBDA-NAME  result name)
    (when warnings 
      (let ((first t))
        (dolist (w warnings)
          (signal-compiler-warning w first nil nil nil)
          (setq first nil))))
    result))

;; want to look like
;; (setq values (multiple-value-list (progn ,@frob)))
     
       
(defun %advised-p (thing &optional when advice-name quick)
  ;; thing is a symbol, result is list of encapsulations
  ;; Quick when used as a simple predicate
  (let ((nx thing) cap val)
    (loop while (setq cap (function-encapsulation nx))
	 do (when (eq (encapsulation-type cap) 'advice)
        (if quick (return-from %advised-p cap))
        (when (or (and (null when)(null advice-name))
                  (and (eq when (encapsulation-advice-when cap))
                       (equal advice-name (encapsulation-advice-name cap))))
          (push cap val)))
	 (setq nx (encapsulation-symbol cap)))
    val))  


(defun advise-2 (newdef newsym method-p function-spec when advice-name define-if-not)      
  (let (advise-thing def orig-sym orig-def)
    (multiple-value-setq (def advise-thing) 
      (%trace-function-spec-p function-spec define-if-not))
    (when (not def)(error "Advise does not understand ~s." function-spec))
    (when (%traced-p advise-thing)
      (setq orig-sym
            (encapsulation-symbol (function-encapsulation advise-thing)))
      (setq orig-def (and (fboundp orig-sym) (symbol-function orig-sym))))
    (let ((capsules (%advised-p advise-thing when advice-name)))
      (when capsules 
        (unadvise-capsules capsules)
        ; get the right def you fool!
        (setq def (%trace-function-spec-p function-spec))))
;    (cl-user::print-db orig-def def)
    (without-interrupts 
     (multiple-value-bind (ignore gf-dcode)
                          (encapsulate (or orig-sym advise-thing) (or orig-def def) 
                                       'advice function-spec newsym
                                       advice-name when)
       (declare (ignore ignore))
       (SYSTEM::%SET-LAMBDA-NAME  newdef `(advised ',function-spec))
       (if method-p (copy-method-function-bits def newdef))
       (if gf-dcode (setq newdef (%cons-combined-method def (cons newdef gf-dcode)
                                                        #'%%call-gf-encapsulation)))                     
;       (cl-user::print-db 'here)
       (cond (orig-sym
              (%fhave orig-sym newdef))  ; make traced call advised
             (t  (cond (gf-dcode (setf (%gf-dcode def) newdef))
                       ((symbolp advise-thing)
                        (%fhave advise-thing newdef))
                       ((typep advise-thing 'method)
                        (progn 
                          (setf (%method-function advise-thing) newdef)
                          (remove-obsoleted-combined-methods advise-thing)
                          newdef)))))))))

;; workaround the fact that you can't compile a function named by a gensym in abcl
(defpackage ".advise")
(defvar *advise-counter* 0)

(defun advise-gensym (function)
  (loop for sym = (intern (format nil "~a-ADVICE-~a" function (incf *advise-counter*)) '|.advise|)
     until (not (fboundp sym)) 
     finally (return sym)))

(defmacro advise (function form &key (when :before) name define-if-not)
  (let* ((newsym (advise-gensym function))
         ; WAS typep advise-thing 'method
         (method-p (or (typep function 'method) ; can this happen?
                       (and (consp function)(eq (car function) :method))))
         (newdef (advise-global-def function newsym when form method-p)))
      `(advise-2 ,newdef ',newsym ,method-p ',function ',when ',name
                 ,define-if-not)))

(defmacro advisedp (function-spec &key when name)
  `(advisedp-1 ',function-spec ',when ',name))

(defun advisedp-1 (function-spec when name)
  (let (val)
    (flet ((xtract-capsule (c)
             (list (encapsulation-spec c)
                   (encapsulation-advice-when c)
                   (encapsulation-advice-name c))))
      (cond ((eq t function-spec)
             (dolist (c *advise-alist*)
               (when (and
                      (or (null when)(eq when (encapsulation-advice-when c)))
                      (or (null name)(equal name (encapsulation-advice-name c))))
                 (push (xtract-capsule c) val))))
            (t (let* ((advise-thing (nth-value 1  (%trace-function-spec-p function-spec)))
                      (capsules (%advised-p advise-thing when name)))
                 (dolist (capsule capsules)
                   (push (xtract-capsule capsule) val)))))
      val)))               


(defun unadvise-1 (function-spec &optional when advice-name ignore)
  (declare (ignore ignore))
  (let ((advise-thing (nth-value 1 (%trace-function-spec-p function-spec))))
    (let ((capsules (%advised-p advise-thing when advice-name)))
      (when capsules (unadvise-capsules capsules)))))

(defun unadvise-capsules (capsules)
  (let (val)
    (dolist (capsule capsules)
        (push (list (encapsulation-spec capsule)
                    (encapsulation-advice-when capsule)
                    (encapsulation-advice-name capsule))
              val)
        (remove-encapsulation capsule))
    val))

(defmacro unadvise (function &key when name)
  (cond ((not (eq function t))
         `(unadvise-1 ',function ',when ',name))
        (t '(%unadvise-all))))

(defun %unadvise-all ()
  (unadvise-capsules *advise-alist*))

(defun %set-unencapsulated-definition (spec newdef)
  (let (foo)
    (loop while (setq foo (function-encapsulation spec)) 
       do (setq spec (encapsulation-symbol foo)))
    (typecase spec
      (symbol
       (%fhave spec newdef)) ;; or fset ??  
      (method
       (setf (%method-function spec) newdef)
       (remove-obsoleted-combined-methods spec)
       newdef))))


;; return t if we defined it, nil otherwise
(defun sys::trace-redefined-update (name newdef)
;  (cl-user::print-db name newdef)
  (let ((def (fdefinition name)))
    (when (and def (function-encapsulated-p name))
      (%set-unencapsulated-definition name newdef)
      t)))

(defun %move-method-encapsulations-maybe (oldmethod newmethod)
  ;; deal with method redefinition
  (let (cap newdef olddef old-inner-def)
    (when (and (setq cap (function-encapsulation oldmethod))
               (not (eq oldmethod newmethod)))      
      (cond (*loading-files*
             (when (%traced-p oldmethod)
               (warn "~%... Untracing ~s" (%untrace-1 oldmethod)))
             (when (%advised-p oldmethod nil nil t)
               (format t "~%... Unadvising ~s" (unadvise-1 oldmethod))))
            (t (setq newdef (%method-function newmethod))
               (setq olddef (%method-function oldmethod))
               (setq old-inner-def (find-unencapsulated-definition oldmethod))
               ;; make last encapsulation call new definition            
               (%set-unencapsulated-definition oldmethod newdef)
               (setf (%method-function newmethod) olddef)
               (remove-encapsulation cap t)
               (put-encapsulation newmethod cap)
               (setf (%method-function oldmethod) old-inner-def)
               (advise-set-method-bits newmethod newdef)
               )))))

(defun advise-set-method-bits (spec newdef)
  ;; spec is a symbol, function, or method object
  (let (foo)
    (loop while (setq foo (function-encapsulation spec)) 
       do      
       (let ((def (typecase spec
		    (symbol (and (fboundp spec) (symbol-function spec)))
		    (method (%method-function spec))
		    (t nil))))
	 (if def
	     (copy-method-function-bits newdef def)
	     (error "whats going on here anyway"))))
    (setq spec (encapsulation-symbol foo))))


#|
	Change History (most recent last):
	2	12/29/94	akh	merge with d13
|# ;(do not edit past this line!!)


(defun mop::find-generic-function (name &optional (errorp t))
  (let ((function (and (fboundp name) (fdefinition name))))
    (when function
      (when (typep function 'generic-function)
        (return-from mop::find-generic-function function))
      (when (%traced-p name)
        (setf function (find-unencapsulated-definition name))
        (when (typep function 'generic-function)
          (return-from mop::find-generic-function function)))))
  (if errorp
      (error "There is no generic function named ~S." name)
      nil))

(if (boundp 'cl-user::*early-advise-forms*)
    (loop for form in cl-user::*early-advise-forms* do
	 (if (keywordp (car form))
	     (setq form (cons (intern (car form) 'cl-user) (subst 'arglist '(:arglist) (cdr form) :test 'equal))))
	 (if *load-verbose* (format t ";;Applying early advise to ~a~%" (second form)))
	 (eval form)
	 finally (setq cl-user::*early-advise-forms* nil))) 

