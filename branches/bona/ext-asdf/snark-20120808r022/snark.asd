;;;; -*- Mode: LISP -*-
;;;;

(in-package :asdf)

(setf (logical-pathname-translations "snark")
      `(("**;*.*" ,(make-pathname :directory (append (pathname-directory *load-pathname*)
						     '(:wild-inferiors))
				  :name :wild
				  :type :wild
				  :defaults *load-pathname*))))

(in-package :asdf)

(defvar common-lisp-user::*compile-me* nil)
(defvar common-lisp-user::*snark-system-pathname* *load-truename*)
(push :snark-asdf *features*)

(defsystem :snark
  :name "snark"
  :author "SRI" 
  :license "Mozilla Public Licence 1.1"
  :components
  (
   (:module snark-files-1 
	    :pathname "src"
	    :components
	    ((:file "loads")
	     (:file "lisp-system")
	     ;; from loads in lisp-system
	     (:file "mvlet" )
	     (:file "progc")
	     (:file "lisp")
	     (:file "collectors")
	     (:file "map-file")
	     (:file "clocks")
	     (:file "counters")
	     (:file "pattern-match")
	     (:file "topological-sort")
	     ;;
	     (:file "deque-system")
	     ;; from loads
	     (:file "deque2")
	     ;;
	     (:file "sparse-array-system")
	     ;; from loads	     ;; from loads
	     (:file "sparse-vector5")
	     (:file "sparse-array")
	     (:file "sparse-vector-expression")
	     ;;
	     (:file "numbering-system")
	     ;; from loads
	     (:file "numbering")
	     ;;
	     (:file "agenda-system")
	     ;; from loads
	     (:file "agenda")
	     ;; 
	     (:file "infix-reader-system")
	     ;; from loads
	     (:file "infix-operators")
	     (:file "infix-reader")
	     ;;
	     (:file "feature-system")
	     ;; from loads
	     (:file "feature")
	     ;;
	     (:file "dpll-system")
	     ;; from loads
	     (:file "davis-putnam3")
	     ;;
	     (:file "snark-pkg"))
	    )
   (:module snark-files-2
	    :pathname "src"
	    :components
	    ((:file "useful")
	     (:file "posets")
	     (:file "solve-sum")
	     (:file "globals")
	     (:file "options")
	     (:file "terms2")
	     (:file "rows")
	     (:file "row-contexts")
	     (:file "constants")
	     (:file "functions")
	     (:file "variables")
	     (:file "subst")
	     (:file "substitute")
	     (:file "symbol-table2")
	     (:file "symbol-definitions")
	     (:file "assertion-analysis")
	     (:file "jepd-relations-tables")
	     (:file "jepd-relations")
	     (:file "date-reasoning2")
	     (:file "constraints")
	     ;;  "constraint-purify"
	     (:file "connectives")
	     (:file "wffs")
	     ;;  "equality-elimination2"
	     (:file "nonhorn-magic-set")
	     (:file "dp-refute")
	     (:file "sorts-functions")
	     (:file "sorts-interface")
	     (:file "sorts")
	     (:file "argument-bag-ac")
	     (:file "argument-list-a1")
	     (:file "unify")
	     (:file "unify-bag")
	     (:file "subsume-bag")
	     (:file "unify-vector")
	     (:file "equal")
	     (:file "variant")
	     (:file "alists")
	     (:file "term-hash")
	     (:file "trie-index")
	     (:file "path-index")
	     (:file "trie" )
	     (:file "feature-vector" )
	     (:file "feature-vector-trie")
	     (:file "feature-vector-index")
	     (:file "term-memory")
	     ;;  "instance-graph" "instance-graph2"
	     (:file "weight")
	     (:file "eval")
	     (:file "input")
	     (:file "output")
	     (:file "simplification-ordering")
	     (:file "symbol-ordering")
	     (:file "multiset-ordering")
	     (:file "recursive-path-ordering" )
	     (:file "ac-rpo")
	     (:file "knuth-bendix-ordering2")
	     (:file "rewrite")
	     (:file "rewrite-code")
	     (:file "code-for-strings2")
	     (:file "code-for-numbers3")
	     (:file "code-for-lists2")
	     (:file "code-for-bags4")
	     (:file "resolve-code")
	     (:file "resolve-code-tables")
	     (:file "main")
	     (:file "subsume")
	     (:file "subsume-clause")
	     (:file "assertion-file")
	     (:file "tptp")
	     (:file "tptp-symbols")
	     (:file "coder"))
	    :depends-on (snark-files-1))
  
   (:module examples
	    :components
	    ((:file "overbeek-test")
	     (:file "front-last-example")
	     (:file "steamroller-example")
	     (:file "reverse-example")
	     (:file "hot-drink-example")
	     (:file "coder-examples")
	     (:file "latin-squares"))
	    :depends-on (snark-files-2)
	    )
  
   (:module patches
	    :pathname "src"
	    :components
	    ((:file "patches"))
	    :depends-on (snark-files-1 snark-files-2)
	    )
   (:module startup
	    :pathname ""
	    :components
	    ((:file "startup-after-asdf-load"))
	    :depends-on (patches))
   ))


   
