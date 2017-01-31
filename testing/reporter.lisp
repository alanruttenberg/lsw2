(defpackage :prove.reporter.fiveam.pprint
  (:use :prove.reporter :prove.report :prove.reporter.fiveam :cl ))

(in-package :prove.reporter.fiveam.pprint)

(defclass fiveam.pprint-reporter (prove.reporter.fiveam::fiveam-reporter) ())

(defmethod print-error-report :around ((report fiveam.pprint-reporter)  (report failed-test-report) stream)
  (let ((*print-pretty* t))
    (call-next-method)))

(setq prove::*default-reporter* :fiveam.pprint)
