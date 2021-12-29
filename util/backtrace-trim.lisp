(in-package :cl-user)

 (defparameter *backtrace-trim-pattern* "(?i).*org\\.armedbear\\.lisp\\.(Primitives|(Lisp\\.error)|clos|LispThread|Standard|Lisp\\.eval).*")

(defun trim-backtrace (bt) 
  (append
   (remove-if (lambda(e) (and (typep e 'system::java-stack-frame)
			      (#"matches"  (prin1-to-string e)  *backtrace-trim-pattern*)))
	      bt)))

(eval '(advise sys:backtrace (trim-backtrace (:do-it)) :when :around :name trim))


;;   0: org.armedbear.lisp.Symbol.symbolValue(Symbol.java:333)
;;   1: org.armedbear.lisp.blocks_51.execute(blocks.lisp:61)
;;   2: org.armedbear.lisp.clos_151.execute(clos.lisp:1421)
;;   3: org.armedbear.lisp.clos_132.execute(clos.lisp:1208)
;;   4: org.armedbear.lisp.StandardGenericFunction.execute(StandardGenericFunction.java:131)
;;   5: org.armedbear.lisp.Symbol.execute(Symbol.java:775)
;;   6: org.armedbear.lisp.LispThread.execute(LispThread.java:562)
;;   7: org.armedbear.lisp.blocks_25.execute(blocks.lisp:26)
;;   8: org.armedbear.lisp.clos_151.execute(clos.lisp:1421)
;;   9: org.armedbear.lisp.clos_132.execute(clos.lisp:1208)
;;  10: org.armedbear.lisp.StandardGenericFunction.execute(StandardGenericFunction.java:131)
;;  11: org.armedbear.lisp.Symbol.execute(Symbol.java:775)
;;  12: org.armedbear.lisp.LispThread.execute(LispThread.java:562)
;;  13: org.armedbear.lisp.blocks_14.execute(blocks.lisp:14)
;;  14: org.armedbear.lisp.clos_151.execute(clos.lisp:1421)
;;  15: org.armedbear.lisp.clos_132.execute(clos.lisp:1208)
;;  16: org.armedbear.lisp.StandardGenericFunction.execute(StandardGenericFunction.java:131)
;;  17: org.armedbear.lisp.Symbol.execute(Symbol.java:775)
;;  18: org.armedbear.lisp.LispThread.execute(LispThread.java:562)
;;  19: org.armedbear.lisp.pathway_to_owl_118.execute(pathway-to-owl.lisp:271)
;;  20: org.armedbear.lisp.LispThread.execute(LispThread.java:546)
;;  21: org.armedbear.lisp.Lisp.evalCall(Lisp.java:488)
;;  22: org.armedbear.lisp.Lisp.eval(Lisp.java:459)

