;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-

;;;; Toplevel ASDF definition to load LSW2 using JSS 3 

;;; TODO Make this stanza part of ASDF description for owl2
;;; Start LSW2 from a stock abcl-1.3.1 or better jar
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf)
  (require :abcl-contrib)
  (asdf:load-system :jss)
  (funcall (intern :ensure-compatibility :jss))
  (asdf:load-system :abcl-asdf))

(asdf:defsystem :lsw2 :version "0.0.4" 
                :depends-on (owl2))



