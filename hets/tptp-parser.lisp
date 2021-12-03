(in-package :cl-tptp-parser.parser)

(defmethod lsw-form ((statement fof-statement))
  (logic::simplify-copulas 
   (lsw-form (formula statement))))

(defmethod lsw-form ((formula fof-binary-formula))
  (let ((right (lsw-form (right formula)))
	(left (lsw-form (left formula))))
    (case (binop formula)
      (implies `(:implies ,left ,right))
      (implies-and-implied-by `(:iff ,left ,right))
      (implied-by `(:implies ,right  ,left))
      (not-implies-and-implied-by `(:not (:iff ,left ,right)))
      (otherwise (list (lsw-form  (binop formula)) left right)))))

(defmethod lsw-form ((qf fof-quantified-formula) )
  (list (if (eq (quantifier qf) 'any) :exists :forall)
	(mapcar 'lsw-form (variables qf))
	(lsw-form (formula qf))))

(defmethod lsw-form ((v var))
  (intern (concatenate 'string "?" (string-upcase (ast-name v)))))

(defmethod lsw-form ((uf fof-unary-formula))
  (list (if (eq (op uf) 'neg)
	    :not
	    (op uf))
	(lsw-form (formula uf))))

(defmethod lsw-form ((formula plain-atomic-formula))
  (if (null (arguments formula))
      (list (predicate formula))
      (list* (predicate formula) (mapcar 'lsw-form (arguments formula)))))

(defmethod lsw-form ((term function-call))
  (let ((functor (functor term)))
    (list* (intern (string-upcase functor)) (mapcar 'lsw-form (arguments term)))))

(defmethod lsw-form ((s symbol))
  (intern (string s) :keyword))

(defmethod lsw-form ((constant constant))
  (intern (string-upcase (value constant))))

(defmethod lsw-form ((l cons))
  (list* (lsw-form (car l)) (mapcar 'lsw-form (cdr l))))


