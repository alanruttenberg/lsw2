(in-package #:system)

(defun traced-function (name info untraced-function)
  (let ((breakp (trace-info-breakp info))
	(*trace-depth* *trace-depth*))
    (lambda (&rest args)
      (with-standard-io-syntax
        (let ((*print-readably* nil)
              (*print-structure* nil))
          (format *trace-output* (indent "~D: ~S~%") *trace-depth*
                  (cons name args))))
      (when breakp
        (break))
      (incf *trace-depth*)
      (let ((r (multiple-value-list (apply untraced-function args))))
        (decf *trace-depth*)
        (with-standard-io-syntax
          (let ((*print-readably* nil)
                (*print-structure* nil))
            (format *trace-output* (indent "~D: ~A returned") *trace-depth* name)
            (dolist (val r)
              (format *trace-output* " ~S" val))
            (terpri *trace-output*)))
        (values-list r)))))