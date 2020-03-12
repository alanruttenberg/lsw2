(in-package :cl-user)

;; https://stackoverflow.com/questions/4618503/format-help-with-printing-a-table

(defun table-widths (data)
  (let ((n (length (car data))))
    (loop for i below n
	  collect (apply 'max (mapcar '1+ (mapcar 'length (mapcar 'prin1-to-string (mapcar (lambda(e) (nth i e)) data))))))))

(defun format-as-table (data &key (stream t) (item-directive "s") (header-directive "a"))
  (if (and (stringp (car (second data)))
	   (#"matches" (string (car (second data))) "^[-]$")
	     (atom (third data)))
      (map nil 'print (cddr data))
      (let ((widths (table-widths data)))
	(format stream (concatenate 'string "~{ ~{~V" header-directive "~}~}~%")
		(mapcar #'(lambda (v width) (list width v)) (car data) widths))
	(format stream (concatenate 'string "~{~{ ~{~V" item-directive "~}~}~%~}")
		(mapcar #'(lambda (r) (mapcar #'(lambda (v width) (list width v)) r widths)) (cdr data) )))))

(defun format-as-org-table (data &key (stream t) (item-directive "s") (header-directive "a"))
  (let ((widths (table-widths data)))
    (format stream (concatenate 'string "|~{ ~{~V" header-directive "|~}~}~%")
	    (mapcar #'(lambda (v width) (list width v)) (car data) widths))
    (format stream (concatenate 'string "~{|~{ ~{~V" item-directive "|~}~}~%~}")
	    (mapcar #'(lambda (r) (mapcar #'(lambda (v width) (list width v)) r widths)) (cdr data) ))))
