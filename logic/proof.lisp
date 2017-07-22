(in-package :logic)

(defvar *expected-proofs* (make-hash-table))

(defclass expected-proof ()
  ((assumptions :accessor assumptions :initarg :assumptions)
   (goals :accessor goals :initarg :goals)
   (options :accessor options :initarg :options)
   (expected-result :accessor expected-result  :initarg :expected-result)))

(defmacro def-expect-satisfiable (name (&rest options &key (with 'z3-check-satisfiability) (timeout 10)) &body assumptions)
  `(setf (gethash ,(intern (string name) :keyword) *expected-proofs*)
	 (make-instance 'expected-proof :options ',options :goals nil :assumptions ',assumptions :expected-result :sat)))

(defmacro def-expect-unsatisfiable (name (&rest options &key (with 'z3-check-satisfiability) (timeout 10)) &body assumptions)
  `(setf (gethash ,(intern (string name) :keyword) *expected-proofs*)
	 (make-instance 'expected-proof :options ',options :goals nil :assumptions ',assumptions :expected-result :unsat)))

(defmacro def-expect-provable (name (&rest options &key (with 'z3-prove) (timeout 10)) goal &body assumptions)
  `(setf (gethash ,(intern (string name) :keyword) *expected-proofs*)
	 (make-instance 'expected-proof :options ',options :goals ',goal :assumptions ',assumptions :expected-result :proved)))


