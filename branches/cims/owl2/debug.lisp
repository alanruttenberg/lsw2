(defun maybe-initialize-appender (logger)
;  (when (zerop (length (#"getHandlers" logger)))
;    (#"addHandler" logger (new 'consolehandler))
  )
  
;  (unless (or (#"hasMoreElements" (#"getAllAppenders" logger))
;	      (and (#"getParent" logger) (#"hasMoreElements" (#"getAllAppenders" (#"getParent" logger)))))
;    (#"addAppender" logger (new 'consoleappender (new 'simplelayout)))))

(defun log-abox (&optional (level "ALL") (kb *default-kb*))
  (let ((logger  (get-java-field (#"getABox" kb) "log")))
    (maybe-initialize-appender logger)
    (#"setLevel" logger (get-java-field  'java.util.logging.Level level))))

(defun log-tbox (&optional (level "ALL") (kb *default-kb*))
  (ignore-errors
    (let ((logger (get-java-field (#"getTBox" kb) "log")))
      (maybe-initialize-appender logger)
      (#"setLevel" logger (get-java-field  'java.util.logging.Level level)))))

(defun log-kb (&optional (level "ALL") (kb *default-kb*))
  (let ((logger (get-java-field kb "log")))
    (maybe-initialize-appender logger)
    (#"setLevel" logger (get-java-field  'java.util.logging.Level level))))

(defun log-taxonomy-builder (&optional (level "ALL") (kb *default-kb*))
  (ignore-errors
    (let ((logger (get-java-field 'taxonomy "log")))
      (maybe-initialize-appender logger)
      (#"setLevel" logger (get-java-field  'java.util.logging.Level level)))))

(defun pellet-log-level (kb level)
  (#"setLevel" (#"getParent" (get-java-field kb "log"))  (get-java-field  'java.util.logging.Level level))
  (#"setLevel" (elt (#"getHandlers"   (#"getParent" (get-java-field kb "log"))) 0)
	       (get-java-field  'java.util.logging.Level level))
  (log-taxonomy-builder level kb)
  (log-kb level kb)
  (log-tbox level kb)
  (log-abox level kb))

(defun log-info (kb)
  (pellet-log-level kb "INFO"))

(defun log-off (kb)
  (pellet-log-level kb "OFF"))

(defun log-debug (kb)
  (pellet-log-level kb "DEBUG"))

(defun log-fine (kb)
  (pellet-log-level kb "FINE"))

(defun log-finest (kb)
  (pellet-log-level kb "FINEST"))