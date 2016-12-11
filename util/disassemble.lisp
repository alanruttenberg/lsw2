(in-package :system)

(defun disassemble-function (arg)
  (let ((function (cond ((#"isInstance" (java:jclass "org.armedbear.lisp.CompiledClosure") arg)
			 (return-from disassemble-function "don't know how to disassemble CompiledClosure"))
			((java::java-object-p arg) 
			 (cond ((java::jinstance-of-p arg "java.lang.Class")
				arg)
			       ((java::jinstance-of-p arg "java.lang.reflect.Method")
				(java::jmethod-declaring-class arg))
			       ;; use isInstance instead of jinstance-of-p
			       ;; because the latter checked java-object-p
			       ;; which fails since its a lisp object
			       ((and (#"isInstance"  (java:jclass "org.armedbear.lisp.Closure") arg)
				     (not (#"isInstance"  (java:jclass "org.armedbear.lisp.CompiledClosure") arg)))
				(return-from disassemble-function 
				  (with-output-to-string (s)
				    (format s "Not a compiled function: ~%")
				    (pprint (#"getBody" arg) s))))
			       ))
			((functionp arg)
                         arg)
                        ((symbolp arg)
                         (or (macro-function arg) (symbol-function arg)))
			(t arg))))
    (when (typep function 'generic-function)
      (setf function (mop::funcallable-instance-function function)))
    (print function)
    (let ((bytes (and nil (and function (not (java::java-object-p function)) (system::function-class-bytes  function)))))
      ;; we've got bytes here then we've covered the case that the diassembler already handled
      ;; If not then we've either got a primitive (in function) or we got passed a method object as arg.
      (if bytes
	  (system::disassemble-class-bytes bytes)
	  (let ((class (if (java:java-object-p function) function (#"getClass" function))))
	    (let ((classloader (#"getClassLoader" class)))
	      (if (or (java:jinstance-of-p classloader "org.armedbear.lisp.MemoryClassLoader")
		      (java:jinstance-of-p classloader "org.armedbear.lisp.FaslClassLoader"))
		  (system::disassemble-class-bytes 
		   (#"getFunctionClassBytes" classloader class))
		  (let ((path (jss::path-to-class (#"getName" class))))
		    (let ((split (cl-user::split-at-char (#"replaceFirst" path "jar:file:" "") #\!)))
		      (let ((jar (jss::new 'jarfile (car split))))
			(system::disassemble-class-bytes 
			 (#"toByteArray" 'ByteStreams 
					 (#"getInputStream" jar 
							    (#"getJarEntry" jar (subseq (second split) 1)))))))))))))))

(defun disassemble (arg)
  (write-string (disassemble-function arg) *standard-output*))

;;(disassemble '+)
;;(disassemble #'+)
;;(disassemble (#"getClass" #'+))
;;(disassemble 'defun)
;; Next disassembles all of LispObject - might be nice to just narrow to the method
;; However better than nothing for disassemble-frame
;;(disassemble (jmethod (jclass "org.armedbear.lisp.Lisp") "error" "org.armedbear.lisp.LispObject"))
;; in the repl (interpreted function, so just print body)
;; (let ((arg (flet ((a ())) #'a)))
;;   (disassemble arg))
