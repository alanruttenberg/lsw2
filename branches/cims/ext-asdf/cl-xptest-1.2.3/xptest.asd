;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          xptest.asd
;;;; Purpose:       ASDF definition file for Xptest
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Sep 2002
;;;;
;;;; $Id: xptest.asd,v 1.8 2003/05/06 16:23:12 kevin Exp $
;;;; *************************************************************************

(defpackage #:xptest-system (:use #:asdf #:cl))
(in-package #:xptest-system)

(defsystem :xptest
  :name "cl-xptest"
  :author "Craig Brozensky"
  :version "2003.04.21"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>"
  :licence "Public domain"
  :description "Extreme Programming Testing Suite"
  :long-description "The XPTEST package is toolkit for building test suites, very much inspired by the test frameworks that the Extreme Programming crew made available for Smalltalk an other languages."
  
  :components
  ((:file "xptestsuite")))
