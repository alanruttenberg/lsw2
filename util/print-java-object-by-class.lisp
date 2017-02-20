(defmethod java::print-java-object-by-class ((class (eql :|java.lang.Class|)) obj stream) 
	   (format stream "#<~a ~a ~x>" 
		    (if (jcall "isInterface" obj) "java interface " "java class ")
		    (jcall "getName" obj)
		    (sys::identity-hash-code obj)))

(defmethod java::print-java-object-by-class ((class (eql :|java.lang.reflect.Proxy|)) obj stream) 
  (format stream "#<proxy instance ~{~a~^, ~} {~a}>"
	  (mapcar #"getName" (coerce (#"getInterfaces" (#"getClass" obj)) 'list))
	  (sys::identity-hash-code obj)))



