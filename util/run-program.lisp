;; Originally I read error first, and it would sometimes hang (on one vm but not another???)  So I switched it to
;; reading output first, and just in case, peek-char the error stream before doing a read-line
(defun run-program-string->string (executable switches input &key wd)
  (let ((process (sys::run-program executable switches :directory wd :input (if input :stream))))
    (when input (write-string input (sys::process-input process))
	  (close (sys::process-input process)))
    (let ((output (with-output-to-string (s)
                    (loop for line = (read-line (sys::process-output process) nil :eof)
                          until (eq line :eof)
                          do (write-line line s) ))))
      (let ((error (unless (eq (peek-char nil (sys::process-error process) nil :eof) :eof)
                     (with-output-to-string (s)
                       (loop for line = (read-line (sys::process-error process) nil :eof)
                             until (eq line :eof)
                             do (write-line line s))))))
        (values output error)))))



