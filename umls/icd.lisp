(defun icds-for-cui (cui)
  (mapcan (lambda(e) (let ((found (cdr (assoc :code e))))
		       (if found
			   (list (#"replaceFirst" found ".*/" ""))
			   nil)))
	  (remove "ICD10" (umls-concept-atoms cui :language "ENG") :key (lambda(e) (cdr (assoc :root-source e))) :test-not 'equalp)))
