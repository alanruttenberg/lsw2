;; Author: Alan Ruttenberg
;; Date: September 24, 2013

;; See http://trac.ihidev.net:8000/Trac_Repo/ticket/70
;; Threads documentation at http://trac.common-lisp.net/bordeaux-threads/wiki/ApiDocumentation

(defun make-regex-automaton (pattern)
  (new 'dk.brics.automaton.RunAutomaton
	(#"toAutomaton" (new 'dk.brics.automaton.RegExp pattern (get-java-field 'dk.brics.automaton.RegExp "ALL")))))

(defun jar-map (jar-or-jars fn)
  "given a jar file or a list of jar files, call fn on the string that is the decompressed entry.
TODO: Add filtering by path name, so we can look only in, say, the XML files
TODO: Pass function the name of the entry too"
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

;; These are global variables that all threads can see. 
;; TODO: Get rid of these and use proper mutexes.
(defvar *regex*)
(defvar *count*)
(defvar *hits*)


;; Create nthreads threads, each of which expects a jar file called note#.jar in /media/ihishared/note-headers-jard
;; Spawn a thread for each of jars, looking for regex in each of the entries, and counting how many are found
;; This uses the java regex package and is substantially slower than the dk.brics.automaton
;; Call thread-join on each to wait until they are all finished. Use (time .. ) to get timings.
;; optimizations for regex coding from http://www.fasterj.com/articles/regex2.shtml


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
  (print *count*))

(defun jar-map-threads-regex-find (regex jar-filenames &key (threads (length jar-filenames)))
  (setq *regex* regex *count* 0 *hits* nil)
  (thread-per-jar
   (lambda (jarfile)
     (let* ((pat (#"compile" 'java.util.regex.Pattern *regex*))
	    (matcher (#"matcher" pat "notused")))
       (with-constant-signature ((find "find") (reset "reset" t))
	 (jar-map 
	  jarfile
	  (lambda(s name)
	    (declare (optimize (speed 3) (safety 0)))
	    (reset matcher s)
	    (when (find matcher)
	      (incf *count*)
	      (push name *hits*)
	      ))))))
   jar-filenames
   :nthreads threads))

(defun jar-map-threads-automaton-find (regex  jar-filenames &key (threads (length jar-filenames)))
  (setq *regex* regex *count* 0 *hits* nil)
  (thread-per-jar
   (lambda(jarfile)
     (let* ((pat (make-regex-automaton regex)))
       (with-constant-signature ((find "find") (newmatcher "newMatcher" t))
	 (jar-map 
	  jarfile
	  (lambda(s name)
	    (declare (optimize (speed 3) (safety 0)))
	    (when (find (newmatcher pat s))
	      (incf *count*)
	      (push name *hits*)
	      ))))))
   jar-filenames
   :nthreads threads))
    

(defun generate-filename-sequence (template digits from to)
  (let ((format-string (#"replaceFirst" template "#" (format nil "~~~a,'0d" digits))))
    (loop for i from from to to collect (format nil format-string i))))


