;; collect random bits of collection code not already in jss

(defun list-to-java-set (list)
  (let ((set (new 'java.util.hashset)))
    (loop for el in list do (#"add" set el))
    set))