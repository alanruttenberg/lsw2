(in-package :logic)

;; brew install z3

(defvar *z3-executable* (or (ignore-errors
                             (string-trim (list #\space #\newline)
                                          (uiop::run-program  "which z3"
                                                              :output :string
                                                              :ignore-error-status t)))
                            "/usr/local/bin/z3"))

(defvar *last-z3-output* nil)
(defvar *last-z3-input* nil)
(defvar *last-z3-error* nil)

(defun does-z3-output-say-sat (output)
  (if (cl-user::all-matches output "\\bsat\\b") t nil))

(defun does-z3-output-say-unsat (output)
  (if (cl-user::all-matches output "\\bunsat\\b") t nil))

(defun does-z3-output-say-timeout (output)
  (if (cl-user::all-matches output "timeout\\b") t nil))

(defun z3-output-errors (output)
  (cl-user::all-matches output "(?s)\\(error .*" 0))

(defvar *extra-z3-switches* nil)

(defun run-z3 (input timeout)
  (setq *last-z3-command* (format nil "~a ~{~a~^ ~}" *z3-executable* (list*  "-in" (format nil "-T:~a" timeout) *extra-z3-switches*)))
  (multiple-value-bind (result error)
       (run-program-string->string
   *z3-executable* 
   (list*  "-in" (format nil "-T:~a" timeout) *extra-z3-switches*)
   (setq *last-z3-input* input)
   )
    (setq *last-z3-output* result)
    (setq *last-z3-error* error)
    (assert (or (null error) (equal error "")) () "Error running z3: ~a" error)
    result
))

;;otherz3
;; (get-value ((continuantPartOfAux a b a))
;; (eval (instanceOf d1 d1 d1)) (not sure what the difference is)

(defun z3-syntax-check (assumptions &optional goals (errorp t))
  (let ((answer 
	  (if (stringp assumptions)
	      (run-z3 assumptions 10)
	      (run-z3
	       (z3-render (collect-axioms-from-spec assumptions)
			  (mapcar (lambda(e) (negate-axiom e)) (collect-axioms-from-spec goals)) nil)
	       10))))
    (when (search "error" answer)
         (princ (if (stringp assumptions) assumptions
		    (z3-render assumptions
			       goals))))
    (if (z3-output-errors answer)
	(if errorp
	    (error "Z3 syntax error:~a~%"  (z3-output-errors answer))
	    (z3-output-errors answer))
	t)))
  
(defun z3-render (assumptions &optional goals pre-commands post-commands)
  (concatenate 'string
	 (format nil "~{~a~}~^" pre-commands)
	 (if (stringp assumptions) assumptions (render :z3 assumptions goals))
	 (format nil "~{~a~}~^" post-commands)))
  
(defvar *default-z3-timeout* 30)
(defun z3-prove (assumptions goals &key (include-output nil) (timeout *default-z3-timeout*) (return-contradiction nil) expected-proof)
  (if (and goals (atom goals)) (setq goals (list goals)))
  (z3-syntax-check assumptions goals)
  (let* ((input (z3-render assumptions goals nil '("(check-sat)")))
	 (answer 
	   (setq *last-z3-output* (run-z3 (setq *last-z3-input* input) timeout))
	         ))
    (when expected-proof
      (setf  (prover-output expected-proof) (setq output answer))
      (setf (prover-input expected-proof) input))
    (let ((result 
	    (or (z3-output-errors answer)
		(if (does-z3-output-say-timeout answer)
		    :timeout
		    (if (does-z3-output-say-unsat answer)
                        (if include-output
			    (values :proved answer)
                            :proved)
			(if (does-z3-output-say-sat answer)
			    :disproved))))))
      (if (and (eq result :disproved) return-contradiction)
	  (let ((model (z3-find-model (append assumptions goals) :timeout timeout :expected-proof expected-proof)))
	    (when expected-proof
	      (setf (prover-model expected-proof) nil)
	      (setf (prover-unsat-explanation expected-proof) model))
            result))
      (if expected-proof
	  (setf (result expected-proof) result))
      (if include-output 
          result
	      result))))


(defun z3-find-model (assumptions &key (timeout 10) expected-proof (pprint nil) pre &allow-other-keys)
  (z3-syntax-check assumptions nil)
  (let* ((input	(z3-render assumptions nil pre  (list "(check-sat)" "(get-model)")))
	 (output (run-z3 input timeout)))
    (when expected-proof
      (setf (prover-model expected-proof) output)
      (setf (prover-input expected-proof) input)
      (unless (prover-output expected-proof)
	(setf (prover-output expected-proof) output)))
    (if (#"matches" output "(?s)^unsat.*")
	:unsat
	(if  (#"matches" output "(?s).*timeout.*")
	    :timeout
	    (if (not (#"matches" output "(?s).*sat.*"))
		output
		(let ((model (make-instance 'z3-model :raw-form output :tuples (cdr (z3-model-form output)))))
		  (when pprint (pprint-model model))
		  model))))))

(defun find-counterexample (assumptions goal &rest rest &key (with 'z3) &allow-other-keys)
  (setq *last-checked-spec* (list* `(:not ,(axiom-sexp goal)) assumptions))
  (setq *last-expanded-model* (apply (if (eq with 'z3) 'z3-find-model 'mace4-find-model)
	 (list* `(:not ,(axiom-sexp goal)) assumptions)
	 (progn (remf rest :with) rest)))) 
  

(defun z3-get-unsat-core (assumptions &key (timeout 10) expected-proof &allow-other-keys)
  (z3-syntax-check assumptions nil)
  (let* ((input   (concatenate 'string "(set-option :produce-unsat-cores true)" 
			       (if (stringp assumptions)
				   (if (search "(get-unsat-core)" assumptions)
				       assumptions
				       (format nil "~a~%(get-unsat-core)~%" assumptions))
				   (z3-render assumptions nil nil (list "(check-sat)""(get-unsat-core)")))))
	 (answer (run-z3 input timeout)))
    (when (not (search "(check-unsat)" input))
	(setq input (concatenate 'string input "(check-unsat)")))
    (when  (not (search "(get-unsat-core)" input))
	(setq input (concatenate 'string input "(get-unsat-core)")))
    (when expected-proof
      (setf (prover-unsat-explanation expected-proof) answer)
      (setf (prover-input expected-proof) input)
      (setf (prover-output expected-proof) answer))
    (if (#"matches" answer "(?s)^unsat.*")
	(let ((names (mapcar 'car (all-matches (subseq answer 6)  "([A-Za-z.+-]+)" 1))))
	  (mapcar (lambda(e) 
		    (or (gethash (keywordify (string-upcase e)) *autonamed-axioms*)
			(keywordify 
			      (string-upcase
			       (#"replaceAll" 
			       (#"replaceAll" 
			       (replace-all
				e
				"(?i)\\.((zero)|(one)|(two)|(three)|(four)|(five)|(six)|(seven)|(eight)|(nine))"
				(lambda(fix) (second (assoc fix '(("zero" "0") ("one" "1") ("two" "2") ("three" "3")
								  ("four" "4") ("five" "5") ("six" "6") ("seven" "7")
								  ("eight" "8") ("nine" "9")) :test 'equalp))) 1)
			       "\\.gt\\." ">") ;; unmangle names
			       "\\.lt\\." "<"))))) 
		  names))
        (if (does-z3-output-say-timeout answer)
            :timeout
            (if (does-z3-output-say-sat answer)
                :sat
                (error "Unexpected output from z3-get-unsat-core for ~s" assumptions))))))

(defun get-z3-proof-support (&optional (input *last-z3-input*) &rest keys)
  (apply 'z3-get-unsat-core input keys))

(defun z3-check-satisfiability (assumptions &key (timeout 10) expected-proof &allow-other-keys)
  (let* ((input (concatenate 'string (z3-render assumptions) "(check-sat)" (string #\newline))))
    (when expected-proof
      (setf (prover-input expected-proof) input))
    (z3-syntax-check input)
    (let ((answer
	    (run-program-string->string
	     *z3-executable* 
	     (list  "-in" (format nil "-T:~a" timeout))
	     input)))
      (when expected-proof
	(setf (prover-output expected-proof) answer))
      (setq *last-z3-output* answer)
      (let ((result (if (does-z3-output-say-timeout answer)
			:timeout
			(if (does-z3-output-say-unsat answer)
			    :unsat
			    (if (does-z3-output-say-sat answer)
				:sat
				answer)))))
	(when expected-proof
	  (setf (result expected-proof) result))
	result))))

;; to match the vampire and prover9 functions, just another name for z3-check-satisfiability
(defun z3-check-unsatisfiable (assumptions &key (timeout 10))
  (z3-check-satisfiability assumptions :timeout timeout))

(defun z3-check-true (axiom &rest keys)
  (let ((result (apply 'z3-check-satisfiability (negate-axiom axiom) keys)))
    (if (eq result :unsat)
	:proved
	(if (eq result :sat)
	    :disproved
	    result))))
      

(defun z3-model-symbol (s)
  (intern (string-upcase (#"replaceAll" (string s) "([A-Z])" "-$1")) 'keyword))
 
(defvar *z3-model-symbol-package* (make-package "Z3Z3" :use '(cl)))

;; 2022-09-18 20:33:02 alanr
;; (z3-find-model '((:and (:not (:= c1 p1)) (:not (:= c1 t1)) (:not (:= p1 t1))) :dr-pi :dr-po :dr-pow :disjoint-cover :part-of-while-operating :trans-part-of (:fact (part-of c1 c1 t1))))
;; has one of the values = '(- 1)
;; hack to replace '(- 1) with -1

;; How this works

;; 1) Extract the domain along with map to symbols. There
;; will be declarations of the symbols in the formula as well as some
;; extra one which we will eventually give names to (usually c1,c2...)
;; When the domain is Int, the interval value will be a number. There
;; isn't an explicit list of the domain values, just the integers in
;; declarations and function. If the Domain is a sort there will be a
;; declation of the universe at the top, from which we get the model.

#| Domain declaration for custom sorts.
;; definitions for universe elements:
  (declare-fun Entity!val!3 () Entity)
  (declare-fun Entity!val!2 () Entity)
  (declare-fun Entity!val!0 () Entity)
  (declare-fun Entity!val!1 () Entity)
  ;; cardinality constraint:
  (forall ((x Entity))
          (or (= x Entity!val!3)
              (= x Entity!val!2)
              (= x Entity!val!0)
              (= x Entity!val!1)))
|#

;; 2) Following will be a mapping of our symbols to theirs like so
;;     (define-fun test2 () Entity
;;          Entity!val!1)
;;     ...
;;    or for Int
;;     (define-fun test2 () Int
;;          1)

;;   In the case of custom sorts, all of the domain (including elements
;;   generated for the model) will have a mapping. In the case of Int
;;   domain only our symbols will have a mapping and the rest will
;;   be constants embedded in the functions. e.g.
;;
;;     (define-fun f ((x!0 Int) (x!1 Int)) Bool
;;        (and (not (= x!0 2)) (not (= x!1 3))))
;;
;;   There are explicit mappings for the integers 0 and 1, but this
;;   function mentions 2 and 3 which are new elements of the domain 

;; 3) Finally, we want to get the model as a set of propositions.
;;    Z3 represents models as a function per relation, which returns true
;;    when the relation holds.
;;    To do that, first we convert the functions to lisp functions
;;    Via substitutions and some other monging and figure out their arity
;;    Then we call the function with every element of the domain (or
;;    cross of the domain for binary,etc, collecting those

;; Worked Example
;;
;; (z3-find-model '((:and (:forall (?x ?y)
;;                          (:implies (f ?x ?y)
;;                              (g ?x ?y)))
;;                   (f test1 test2)
;;                   (:exists (?a ?b)
;;                     (:not (g ?a ?b) )))))

;; transform-z3-model returns 3 values :
;; <1      ((:g 2) (:f 2)) -- functions and arities
;; <1      #<eql hash-table 4 entries, 11 buckets {2D0C4860}> -- map of z3 symbols to our symbols
;; <1      ((|g| (|x!0| |x!1|) (and (not (= |x!0| 2)) (not (= |x!1| 3)))) -- Functions for evaluating relations f & g.
;;          (|f| (|x!0| |x!1|) (and (not (= |x!0| 2)) (not (= |x!1| 3)))))
;;
;; z3-model-form returns
;; ((universe test2 test1 c1 c2) (g test2 test2)
;;  (g test2 test1) (g test2 c1) (g test1 test2)
;;  (g test1 test1) (g test1 c1) (g c2 test2) (g c2 test1)
;;  (g c2 c1) (f test2 test2) (f test2 test1) (f test2 c1)
;;  (f test1 test2) (f test1 test1) (f test1 c1)
;;  (f c2 test2) (f c2 test1) (f c2 c1))

;; Note: In the above I've removed package prefixes for legibility

(defun transform-z3-model (z3-output)
  (if (equal z3-output "") 
      z3-output
      (let ((z3-readtable (copy-readtable)))
	(setf (readtable-case z3-readtable) :preserve)
	(set-syntax-from-char #\! #\- *readtable* z3-readtable)
	(let* ((s (make-string-input-stream z3-output))
	       (named (make-hash-table :test 'eql))
	       (forms nil)
	       (relations nil)
               (model nil)
	       (result nil))
	  (let ((*package* *z3-model-symbol-package*)
		(*readtable* z3-readtable))
	    (setq result (read s))
	    (if  (and (symbolp result) (equal (string result) "sat"))
	         (setq model (read s))
                 (setq model result)))
	  (if (not model)
	      z3-output
	      (let ((ints (let ((them nil))
			    (subst-if :int (lambda(e) (if (integerp e)
                                                          (pushnew e them)
                                                          (if (and (consp e) (eq (car e) '-) (integerp (second e)))
                                                              (pushnew (- (second e)) them))
                                                                   
                                                          )()) model)
			    them)))
		(loop for form  in (cdr model)
		      do
			 (if (equal (string (car form)) "define-fun")
			     (destructuring-bind (name args type . body) (cdr form)
                               (if (and (consp (car body)) (eq (caar body) '-)) (setf (car body) (- (second (car body)))))
			       (cond ((null args)
				      (unless (find #\! (string name))
					(push  (z3-model-symbol name) (gethash (car body) named))))
				     ((and (equal (string type) "Bool")
					   (not (find #\! (string name))))
				      (push (list (z3-model-symbol name) (length args)) relations)
				      ))
			       (if args
				   (let ((ite (intern "ite" *z3-model-symbol-package*))
					 (and (intern "and" *z3-model-symbol-package*))
					 (or (intern "or"  *z3-model-symbol-package*))
					 (not (intern "not"  *z3-model-symbol-package*))
					 (lets (intern "let" *z3-model-symbol-package*)))
				     (push `(,name ,(mapcar 'car args) ,@(tree-replace (lambda(x) (if (and (consp x) (eq (car x) '-))
                                                                                                      (- (second x))
                                                                                                      x))
                                                                                       (subst 'let lets (subst 'or  or (subst 'not not (subst 'and  and (subst 'if ite body))))))) forms)
				     )))))
		(loop for el in (set-difference ints (alexandria::hash-table-keys named))
		      with count = 0
		      do (push (intern (format nil "C~a" (incf count)) :keyword) (gethash el named)))
		(values relations named forms)))))))

(defun z3-model-form (model &key (compile? t))
  (multiple-value-bind (relations named forms)
      (transform-z3-model model)
    (if (stringp relations)
	relations
	(let ((constants (loop for no being the hash-keys of named using (hash-value name)
			       collect (list  (intern (string-upcase (format nil "~{~a~^=~}" name)) 'keyword) no))))
	  (let* ((function-source  `(lambda()
				     (let ((,(intern "true" *z3-model-symbol-package*) t)
					   (,(intern "false" *z3-model-symbol-package*) nil))
				       (labels ,forms
					 (list ,@(mapcar (lambda(f) `(function ,(car f))) forms))))))
		 (funs (progn (:print-db function-source) (funcall (if compile?
			   (let ((ext::*suppress-compiler-warnings* t))
			     (compile nil  function-source))
			   (eval function-source))))))
	    (let ((table (make-hash-table)))
	      (loop for (name) in forms
		    for fun in funs
		    do (setf (gethash (z3-model-symbol name) table) fun))
	      (list* `(:universe ,@(mapcar 'car constants))
		    (loop for (pred args) in relations
			  if (= args 1)
			    append
			    (loop for (name value) in constants
				  when  (funcall (gethash pred table) value)
				    collect `(,pred ,name))
			  else
			    if (= args 2) 
			      append (loop for (name1 value1) in constants
					   append
					   (loop for (name2 value2) in constants
						 when (funcall (gethash pred table) value1 value2)
						   collect `(,pred ,name1 ,name2)))
			  else
			    if (= args 3) 
			      append (loop for (name1 value1) in constants
					   append
					   (loop for (name2 value2) in constants
						 append
						 (loop for (name3 value3) in constants
;                                                       do (break)
						       when (funcall (gethash pred table) value1 value2 value3)
							 collect `(,pred ,name1 ,name2 ,name3))))))))))))

(defun pprint-z3-model (model)
  (let ((model (z3-model-form model :compile? t)))
    (if (stringp model)
	model
	(loop for form in model
	      do (let ((*print-case* :downcase)) (format t "~a~%" form))))))
