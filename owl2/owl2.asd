;;;; -*- Mode: LISP -*-
;;;;

(in-package :asdf)

(setf (logical-pathname-translations "owl2")
      `(("**;*.*" ,(make-pathname :directory (append (pathname-directory *load-pathname*)
						     '(:wild-inferiors))
				  :name :wild
				  :type :wild
				  :defaults *load-pathname*))))
(defsystem :owl2
  :name "OWL"
  :author "Alan Ruttenberg"
  :license "BSD"
  :components
  ((:module "basics"
    :pathname ""
    :components
    ((:file "owlapi")
     (:file "debug"))
    :serial t)
   (:module matcher
    :pathname ""
    :components
    ((:file "auxfns")
     (:file "patmatch"))
    :depends-on ("basics"))
   (:module mapping
    :pathname ""
    :components
    ((:file "parse-mapping-spec")
     (:file "generate-mapping")
     (:file "the-mapping")
     (:file "swrl-rdf-mapping")	     
     (:file "terminal-alternate-symbols"))
     :serial t)
   (:module syntax
    :pathname ""
    :components
    ((:file "manchester-class-expression")
     (:file "to-owl-syntax")
     (:file "owl-to-lisp-syntax")
     (:file "to-owlapi-class-expression")
    ))
   (:module main
	    :pathname ""
;	    :serial t
	    :components
	    ((:file "graph")
	     (:file "preferred-label")
	     (:file "axioms")
	     (:file "weaken")
	     (:file "filter")
;	     (:file "materialize-restrictions-for-triplestore")
	     (:file "explanation")
	     (:file "local")
	     (:file "module")
	     (:file "signature-module")
	     (:file "inferred-axioms")
	     (:file "create-external-derived")
	     (:file "jena")
	     (:file "label-source")
	     (:file "clean-subclass-tree")
	     (:file "check-owl-profile")
	     (:file "text-classtree")
	     (:file "hermit-debug")
	     (:file "domain-and-range")
	     (:file "short-form-providers")
	     (:file "diff")
	     (:file "release")
;	     (:file "html-description")
	     (:file "describe")
	     (:file "logger-control")
             (:org "owldoc")
	     )
	    :depends-on (matcher basics mapping syntax)
	    )
   (:module "query" :pathname ""
    :components ((:file "dl-query")
                 (:file "annotation-property-query")
		 (:file "sparql")
		 (:file "sparql-owlbgp")
		 (:file "sparql-twerpish"))
    :depends-on ("main"))
   (:module api :pathname ""
    :components
    ((:file "api-symbols"))
    :depends-on (main query)))
  :depends-on (util xmls owl2libs-mvn2)
  :defsystem-depends-on (lilith))

(defun cl-user::test-lsw2-owlapi ()
  (let ((*load-verbose* nil))
    (uiop/utility:with-muffled-conditions ((list 'style-warning 'asdf::bad-system-name))
      (require :testing)))
  (let ((cl-user::*read-time-uri* t))
    (funcall (intern "RUN" 'prove) #P"owl2:test-lsw2-owl2.lisp")))

;;;; eof
