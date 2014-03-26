;; all I can say is WOW

(defun today-verbose-date ()
  (#"toString"(#"format" (new 'java.text.SimpleDateFormat "EEEE MMMM d, yyyy")
	     (new 'java.util.Date)
	     (new 'stringbuffer "")
	     (new 'fieldposition (get-java-field 'java.text.DateFormat "DATE_FIELD")))))

(defun today-iso-date ()
  (multiple-value-bind (second minute hour day month year dow dst zone)
      (decode-universal-time (get-universal-time))
    (declare (ignore second hour minute dow dst zone))
    (format nil "~4,'0D-~2,'0D-~2,'0D" year month day)))