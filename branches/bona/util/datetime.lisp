;; all I can say is WOW

(defun today-verbose-date ()
  (#"toString"(#"format" (new 'java.text.SimpleDateFormat "EEEE MMMM d, yyyy")
	     (new 'java.util.Date)
	     (new 'stringbuffer "")
	     (new 'fieldposition (get-java-field 'java.text.DateFormat "DATE_FIELD")))))