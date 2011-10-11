;;; -*- Lisp -*-
;;; $Id: xmls.asd 611 2011-02-23 21:14:47Z rpgoldman $

(defpackage #:xmls-system (:use #:cl #:asdf))
(in-package :xmls-system)

(defsystem :xmls
    :version "1.4.1"
    :depends-on
    #+xmls-debug (:norvig)
    #-xmls-debug ()
    :components ((:file "xmls")
                 (:file "xmlrep-helpers"
                        ;; package is defined in XMLS. [2009/02/24:rpg]
                        :depends-on ("xmls"))))

;;; These are additional standalone tests
(defsystem :xmls-test
  :version "1.4.1"
  :depends-on (xmls nst)
  :components ((:file "nst-tests")))