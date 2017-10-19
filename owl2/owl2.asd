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
	    ((:file "package")
	     (:file "owlapi")
	     (:file "debug"))
	    :serial t)
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
	     (:file "short-form-providers")
	     (:file "release")
	     (:file "html-description")
	     (:file "describe")
	     (:file "logger-control")
	     (:file "owl-to-fol")
	     )
	    :depends-on (matcher basics)
	    ))
  :depends-on (util xmls owl2libs-mvn2 logic))

(defun cl-user::test-lsw2-owlapi ()
  (let ((*load-verbose* nil))
    (uiop/utility:with-muffled-conditions ((list 'style-warning 'asdf::bad-system-name))
      (require :testing)))
  (let ((cl-user::*read-time-uri* t))
    (funcall (intern "RUN" 'prove) #P"owl2:test-lsw2-owl2.lisp")))

;;;; eof
