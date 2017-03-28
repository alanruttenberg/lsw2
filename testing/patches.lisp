(in-package :prove.test)

(defmacro is (got expected &rest args)
  (with-gensyms (duration result new-args desc)
    (once-only (expected)
      `(let* ((,new-args (list ,@args))
              (,desc (parse-description-and-test ,new-args)))
         (with-catching-errors (:description ,desc :expected ,expected)
           (with-duration ((,duration ,result) ,got)
             (test ,result ,expected ,new-args
                   :duration ,duration
		   ;; 
		   :got-form (cl-user::eval-uri-reader-macro ',got))))))))

(in-package :prove.suite)

(defun plan (num &optional description)
  (and description (princ description prove.output:*test-result-output*))
  (let ((suite (current-suite)))
    (setf (slot-value suite 'plan) num)
    (reset-suite suite))
  (print-plan-report nil num *test-result-output*))

