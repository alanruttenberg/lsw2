(in-package :cl-user)

;; From https://github.com/gwkkwg/metatilities/blob/master/dev/utilities/dates-and-times.lisp

(defun print-time-interval (seconds &optional stream print-seconds?)
  "Prints the time elapsed by `seconds.' For example, 125 prints ``2 minutes, 5
seconds'' If `print-seconds?' is true, the residual seconds are reported,
otherwise the result is rounded to the nearest minute."
  (let (days hours minutes secs)
    (multiple-value-setq (days secs) (floor (abs seconds) (* 60 60 24)))
    (multiple-value-setq (hours secs) (floor secs (* 60 60)))
    (multiple-value-setq (minutes secs) (if print-seconds?
					    (floor secs 60)
					    (values (round secs 60) 0)))
    #+ignore
    (spy days hours minutes secs)
    (if print-seconds?
	(if (zerop seconds)
	    (format stream "0 seconds")
	    (format stream "~:[~d day~:p, ~;~*~]~:[~d hour~:p, ~;~*~]~:[~d minute~:p, ~;~*~]~:[~d second~:p~;~*~]~:[ ago ~;~]"
		    (zerop days) days
		    (zerop hours) hours
		    (zerop minutes) minutes
		    (zerop secs) secs
		    (plusp seconds)))
	(if (= 0 days hours minutes)
	    (format stream "0 minutes")
	    (format stream "~:[~d day~:p, ~;~*~]~:[~d hour~:p, ~;~*~]~:[~d minute~:p~;~*~]~:[ ago~;~]"
		    (zerop days) days
		    (zerop hours) hours
		    (zerop minutes) minutes
		    (plusp seconds))))))
