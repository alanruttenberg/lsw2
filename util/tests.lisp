(loop for sym in 
     '("DEF-TEST-FIXTURE" "SETUP" "TEARDOWN" "FAILURE" "MAKE-TEST-SUITE" "MAKE-TEST-CASE" "TEST-SUITE" "ADD-TEST"
       "REPORT-RESULT" "RUN-TEST")
     do
     (when (find-symbol sym)
       (unintern (find-symbol sym)))
     (shadowing-import (intern sym 'xp-test)))

(defmacro simple-test-suite (name description fixture &rest tests)
  `(let ((suite (make-instance 'test-suite :name ,name :description ,description)))
     (loop for test in ',tests
	for english = (string-downcase (substitute #\space #\- (string test)))
	do (add-test (make-test-case english ',fixture :test-thunk test :description english) suite))
     suite))

(defparameter *all-lsw-test-suites* nil)

(defun run-all-tests ()
  (loop for suite in *all-lsw-test-suites* 
       do (report-result (run-test (symbol-value suite)) :verbose t)))

(in-package :xp-test)

;; something is fishy in condition reporting. Override this stuff from xptest to get a reasonable report.

(define-condition test-failure (simple-condition) ((message :reader message :initarg :message))
  (:documentation "Base class for all test failures.")
  )

(defun failure (format-str &rest args)
  "Signal a test failure and exit the test."
  (signal 'test-failure
	  #+(or cmu allegro openmcl) :format-control
	  #-(or cmu allegro openmcl) :format-string
	  format-str
	  :format-arguments args
	  :message (apply 'format nil format-str args)))

(defmethod report-result ((result test-result) &key (stream t) (verbose nil))
  "Print out a test-result object for a report to STREAM, default to
standard-output.  If VERBOSE is non-nil then will produce a lengthy
and informative report, otherwise just prints wether the test passed
or failed or errored out."
  (if verbose (format stream
		      "~&------------------------------------------------------~%"))
  (format stream "Test ~A ~A ~%"
	  (test-name (result-test result))
	  (cond
	   ((test-failures result) "Failed")
	   ((test-errors result) "Errored")
	   (t "Passed")))
  (if verbose
      (progn
	(format stream "Description: ~A~%" (description (result-test result)))
	(if (test-failures result)
	    (progn
	      (format stream "Failures:~%")
	      (mapcar #'(lambda (fail) (format stream "    ~A" (message fail)))
		       (test-failures result))))
	(if (test-errors result)
	    (progn
	      (format stream "Errors:~%")
	      (mapcar #'(lambda (fail) (format stream "    ~A" fail))
		      (test-errors result))))))
  ;(format stream "~%~%") ; debian bug #190398 
  )