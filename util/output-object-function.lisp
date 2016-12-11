(in-package #:system)

;; patch so that print-object gets called on functions

(defun output-ugly-object (object stream)
  (cond ((consp object)
         (output-list object stream))
        ((and (vectorp object)
              (not (stringp object))
              (not (bit-vector-p object)))
         (output-vector object stream))
        ((structure-object-p object)
         (cond
           ((and (null *print-readably*)
                 *print-level*
                 (>= *current-print-level* *print-level*))
            (write-char #\# stream))
           (t
            (print-object object stream))))
        ((standard-object-p object)
         (print-object object stream))
	((java::java-object-p object)
	 (print-object object stream))
	((functionp object)
	 (print-object object stream))
        ((xp::xp-structure-p stream)
         (let ((s (sys::%write-to-string object)))
           (xp::write-string++ s stream 0 (length s))))
        (t
         (%output-object object stream))))
