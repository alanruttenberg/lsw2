(in-package :cl-user)
;; https://rxnav.nlm.nih.gov/SnomedCTAPI.html

(defun get-snomed-id-status (id)
  (let ((info (xmls::parse (get-url (format nil "https://rxnav.nlm.nih.gov/REST/SnomedCT/status?id=~a" id)))))
    (let ((status
	    (cdr (assoc (third (first (find-immediate-children-with-tag info "status")))
			'(("-1" . :unknown)
			  ("0" . :retired)
			  ("1" . :active)
			  ("2" . :moved))
			:test 'equalp))))
      (if (eq status :moved)
	  (list* :moved
		 (mapcar 'third (find-immediate-children-with-tag info "mappedId")))
	  status))))
	  



