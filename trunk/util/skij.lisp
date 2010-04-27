(defun skij-eval (form)
  (let ((*print-case* :downcase))
    (#"evalString" 'com.ibm.jikes.skij.Scheme (prin1-to-string form))))