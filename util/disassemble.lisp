(defun disassemble-function (arg)
  (let ((function (cond ((functionp arg)
                         arg)
                        ((symbolp arg)
                         (or (macro-function arg) (symbol-function arg))))))
    (when (typep function 'generic-function)
      (setf function (mop::funcallable-instance-function function)))
    (let ((bytes (and function (system::function-class-bytes  function))))
      (if bytes
	  (system::disassemble-class-bytes bytes)
	  (let ((path (jss::path-to-class (#"getName" (#"getClass" (or function arg))))))
	    (let ((split (split-at-char (#"replaceFirst" path "jar:file:" "") #\!)))
	      (let ((jar (new 'jarfile (car split))))
		(system::disassemble-class-bytes 
		 (#"toByteArray" 'ByteStreams 
				 (#"getInputStream" jar 
						    (#"getJarEntry" jar (subseq (second split) 1))))))))))))

