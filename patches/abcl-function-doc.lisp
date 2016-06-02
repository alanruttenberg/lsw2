(in-package #:system)

;; function documentation wasn't getting described when passing compiled function vs symbol.

(defmethod describe-object ((object t) stream)
  (let ((*print-pretty* t))
    (typecase object
      (SYMBOL
       (let ((package (symbol-package object)))
         (if package
             (multiple-value-bind
                 (sym status)
                 (find-symbol (symbol-name object) package)
               (format stream "~S is an ~A symbol in the ~A package.~%"
                       object
                       (if (eq status :internal) "internal" "external")
                       (package-name package)))
             (format stream "~S is an uninterned symbol.~%" object))
         (cond ((special-variable-p object)
                (format stream "It is a ~A; "
                        (if (constantp object) "constant" "special variable"))
                (if (boundp object)
                    (format stream "its value is ~S.~%" (symbol-value object))
                    (format stream "it is unbound.~%")))
               ((boundp object)
                (format stream "It is an undefined variable; its value is ~S.~%"
                        (symbol-value object)))))
       (when (autoloadp object)
         (resolve object))
       (let ((function (and (fboundp object) (symbol-function object))))
         (when function
           (format stream "Its function binding is ~S.~%" function)
           (describe-arglist function stream)))
       (let ((doc (documentation object 'function)))
         (when doc
           (format stream "Function documentation:~%  ~A~%" doc)))
       (let ((plist (symbol-plist object)))
         (when plist
           (format stream "The symbol's property list contains these indicator/value pairs:~%")
           (loop
             (when (null plist) (return))
             (format stream "  ~S ~S~%" (car plist) (cadr plist))
             (setf plist (cddr plist))))))
      (FUNCTION
       (%describe-object object stream)
       (describe-arglist object stream)
       ;;---- alanr
       (let ((function-symbol (nth-value 2 (function-lambda-expression object))))
	 (if (and (consp function-symbol) (eq (car function-symbol) 'macro-function))
	     (setq function-symbol (second function-symbol)))
	 (when  function-symbol
	   (let ((doc (documentation function-symbol 'function)))
	     (when doc
	       (format stream "Function documentation:~%  ~A~%" doc)))
	   )))
       ;;---- alanr
      (INTEGER
       (%describe-object object stream)
       (format stream "~D.~%~
                       #x~X~%~
                       #o~O~%~
                       #b~B~%"
               object object object object))
      (t
       (%describe-object object stream))))
  (values))
