;; Author: Alan Ruttenberg
;; Date: September 24, 2013

(defun jar-map (jar-or-jars fn)
  "given a jar file or a list of jar files, call fn on the string that is the decompressed entry.
TODO: Add filtering by path name, so we can look only in, say, the XML files"
    (loop for jar in (if (consp jar-or-jars) jar-or-jars (list jar-or-jars))
       with buffer-size = 0
       with buffer = nil
       for jarfile = (new 'jarfile (new 'file (namestring (truename jar))))
       for entries = (#"entries" jarfile)
       do
	 (loop while (#"hasMoreElements" entries)
	    for next-in =  (#"nextElement" entries)
	    for in-stream = (#"getInputStream" jarfile next-in)
	    for size = (#"getSize" next-in)
	    do
	      (when (> size buffer-size) 
		(setq buffer (jnew-array "byte" size)))
	      (when (> size 0)
		(#"read" in-stream buffer)
		(setq @ buffer)
		(unwind-protect
		     (let ((name (#"getName" next-in)))
		       (funcall fn (new 'java.lang.string buffer size) name))
		  (#"close" in-stream))))))


;; Create a thread for each jar file. Each thread executes
;; thread-run-function passed the name of a jar file.  Call
;; thread-join on each to wait until they are all finished. Use (time
;; .. ) to get timings.  

(defun thread-per-jar (thread-run-function jar-filenames &key
		       (thread-name-prefix "per-jar-")
		       (nthreads (length jar-filenames)))
  (time (loop for thread in
	     (loop
		for i from 0 below nthreads
		for f in jar-filenames
		collect (threads:make-thread
			 (lambda()
			   (funcall thread-run-function f))
			 :name (format nil "~a~a" thread-name-prefix i)))
	     do (threads:thread-join thread)))
  (print (hash-table-count *hits*)))

;; One global variable to hold our results hash
(defvar *hits*)

;; And a method to add a result. There is no duplication of the entry
;; names across the jar files.  I had hoped this was thread safe, but
;; I get different numbers of entries in the hash table in diffreent
;; runs of the job.
(defun add-hit (entry-name jarfile data)
  (setf (gethash entry-name *hits*) 
	(list jarfile data)))

;; This uses the java regex package and is substantially slower than
;; the dk.brics.automaton. Optimizations for regex coding from
;; http://www.fasterj.com/articles/regex2.shtml

(defun jar-map-threads-regex-find (regex jar-filenames &key (threads (length jar-filenames)))
  (setq *hits* (make-hash-table :test 'equal)) ;; initialize results
  (thread-per-jar
   (lambda (jarfile)
     (let* ((pat (#"compile" 'java.util.regex.Pattern regex))
	    (matcher (#"matcher" pat "notused")))
       (with-constant-signature ((find "find") (reset "reset" t))
	 (jar-map 
	  jarfile
	  (lambda(s name)
;	    (declare (optimize (speed 3) (safety 0)))
	    (reset matcher s)
	    (when (find matcher)
	      (add-hit name jarfile s)
	      ))))))
   jar-filenames
   :nthreads threads))

;; Prepare the automaton, analogous to compiling the regular expression
(defun compile-regex-automaton (pattern)
  (new 'dk.brics.automaton.RunAutomaton
	(#"toAutomaton" (new 'dk.brics.automaton.RegExp pattern (get-java-field 'dk.brics.automaton.RegExp "ALL")))))

(defun jar-map-threads-automaton-find (regex  jar-filenames &key (threads (length jar-filenames)))
  (setq *hits* (make-hash-table :test 'equal))
  (thread-per-jar
   (lambda(jarfile)
     (let* ((pat (compile-regex-automaton regex)))
       (with-constant-signature ((find "find") (newmatcher "newMatcher" t))
	 (jar-map 
	  jarfile
	  (lambda(s name)
;	    (declare (optimize (speed 3) (safety 1)))
	    (when (find (newmatcher pat s))
	      (add-hit name jarfile s)
	      ))))))
   jar-filenames
   :nthreads threads))

(defun generate-filename-sequence (template digits from to)
  (let ((format-string (#"replaceFirst" template "#" (format nil "~~~a,'0d" digits))))
    (loop for i from from to to collect (format nil format-string i))))

