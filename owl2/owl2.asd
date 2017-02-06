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
	     (:file "debug")))
   (:module matcher
	    :pathname ""
	    :components
	    ((:file "auxfns")
	     (:file "patmatch")))
   (:module translate
	    :pathname ""
	    :serial t
	    :components
	    ((:file "parse-mapping-spec")
	     (:file "generate-mapping")
	     (:file "the-mapping")
	     (:file "swrl-rdf-mapping")
	     (:file "manchester-class-expression")
	     (:file "to-owl-syntax")
	     (:file "sparql")
	     (:file "sparql-owlbgp")
	     (:file "sparql-twerpish")
	     (:file "graph")
	     (:file "axioms")
	     (:file "weaken")
	     (:file "materialize-restrictions-for-triplestore")
	     (:file "explanation")
	     (:file "local")
	     (:file "module")
	     (:file "inferred-axioms")
	     (:file "create-external-derived")
	     (:file "terminal-alternate-symbols")
	     (:file "jena")
	     (:file "label-source")
	     (:file "owl-to-lisp-syntax");
	     (:file "clean-subclass-tree")
	     (:file "violations")
	     (:file "to-owlapi-class-expression")
	     (:file "text-classtree")
	     (:file "dl-query")
	     (:file "hermit-debug")
	     (:file "domain-and-range")
	     (:file "release")
	     (:file "logger-control")
	     )
	    :depends-on (matcher basics)
	    ))
  :depends-on (util xmls owl2libs-mvn))

(defun test-owlapi ()
  (prove::run #P"owl2:test-owlapi.lisp"))

;;;; eof
