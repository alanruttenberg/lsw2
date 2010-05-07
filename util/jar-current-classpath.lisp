(defun make-jar-manifest (main-class)
  (let* ((contents (format nil "Manifest-Version: 1.0!%~%Main-Class: ~a~%" main-class) )
	 (is (new 'java.io.ByteArrayInputStream (#"getBytes" contents "UTF-8"))))
    (new 'java.util.jar.Manifest is)))

(defun jar-merge (out &key dont-include-jar dont-include-entry in)
  (let ((out (new 'JarOutputStream (new 'FileOutputStream (namestring (translate-logical-pathname out)))
		  (make-jar-manifest "org.armedbear.lisp.Main")))
	(already (make-hash-table :test 'equal)))
    (loop for one in dont-include-entry
       do (setf (gethash one already) t))
    (flet ((maybe-rename-entry (entry jar)
	     (let* ((name (#"getName" entry)))
	       (if (#"matches" name "^((LICENSE)|(META-INF)|(log4j.properties)|(NOTICE)).*")
		   (let ((new (loop for counter = "" then (1+ (if (equal counter "") 1 counter))
				 for new = (format nil "~a-~a~a" (pathname-name jar) name counter)
				 until (not (gethash new already)) 
				 finally (progn (setf (gethash new already) t) (return new)))))
		     (new 'jarentry new))
		   (unless (gethash name already)
		     (setf (gethash name already) t)
		     (new 'jarentry entry))
		   ))))
      (loop for jar in (set-difference in dont-include-jar :test 'equal :key 'pathname-name) 
	 for jarfile = (new 'jarfile (new 'file (namestring (truename jar))))
	 for entries = (#"entries" jarfile)
	 with buffer = (jnew-array "byte" 4096)
	 do
	 (format t "adding ~a~%" jar)
	 (loop while (#"hasMoreElements" entries)
	    for next-in =  (#"nextElement" entries)
	    for next-out = (maybe-rename-entry next-in jar)
	    for in-stream = (when next-out (#"getInputStream" jarfile next-in))
	    do
	    (when next-out
	      (#"putNextEntry" out next-out)
	      (loop for n = (#"read" in-stream buffer)
		 while (> n 0)
		 do (#"write" out buffer 0 n))
	      (#"closeEntry" out)
	      (#"close" in-stream))))
      (#"close" out))))

'(jar-merge "/Users/alanr/Desktop/test.jar"
       :dont-include-jar '("owlapi-src")
       :dont-include-entry '("org/armedbear/lisp/system.lisp")
       :in (append (split-at-char (#"getProperty" 'System "java.class.path") #\:) *added-to-classpath*))

; jar uf ~/Desktop/lsw.jar -C testlsw lsw -C testlsw org/armedbear/lisp/system.lisp -C testlsw log4j.properties

;; don't
(defun jar-current-classpath (&optional (to-path "~/Desktop/dist.jar"))
  (let* ((where  "/Volumes/Disk Image/");(TEMP-DIRECTORY-PATH "combined/"))
	 (wheref (new 'file where))
	 (env (jnew-array-from-array "java.lang.String" #()))
	 (common-names '("LICENSE.txt" "META-INF" "log4j.properties"
			 "NOTICE"
			 "NOTICE.txt" "license"))
	 (dont-include '("owlapi-src")))
    (ensure-directories-exist where)
    (let ((initial-jars
	   (split-at-char (#"getProperty" 'System "java.class.path") #\:)))
      (loop for jar in  (set-difference (append initial-jars *added-to-classpath*) dont-include :test 'equal :key 'pathname-name) 
	 for response = (#"waitFor" (#"exec" (#"getRuntime" 'java.lang.Runtime)
					     (jnew-array-from-array "java.lang.String" (coerce (list "jar" "xf" jar) 'vector))
					     env
					     wheref))
	 do (format t "jar xf ~a ... ~a~%" jar response)
	 (loop for name in common-names
	      do (when (probe-file (merge-pathnames name where))
		   (if (null (pathname-name (probe-file (merge-pathnames name where))))
		       (loop for counter = "" then (1+ (if (equal counter "") 1 counter))
			  for dest = (let ((dir (pathname-directory (probe-file (merge-pathnames name where)))))
				       (make-pathname :directory
						      (append (butlast dir) (list (format nil (format nil "~a-~a~a" (pathname-name jar) (car (last dir)) counter))))))
			  unless (probe-file dest)
			  return (rename-file (probe-file (merge-pathnames name where))
				       dest))
		       (rename-file (merge-pathnames name where)
				    (merge-pathnames
				     (make-pathname :name (format nil "~a-~a" (pathname-name jar) (pathname-name name))
						    :type (pathname-type name)
						    )
				     where)))))))
    (#"waitFor" (#"exec" (#"getRuntime" 'java.lang.Runtime)
			 (jnew-array-from-array "java.lang.String" (coerce (list "jar" "cf" to-path ".") 'vector))
			 env
			 wheref))
    where))

;; http://code.google.com/p/jarjar/wiki/CommandLineDocs




	      
