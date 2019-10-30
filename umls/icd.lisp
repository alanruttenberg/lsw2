(defun icds-for-cui (cui)
  (mapcan (lambda(e) (let ((found (cdr (assoc :code e))))
		       (if found
			   (list (#"replaceFirst" found ".*/" ""))
			   nil)))
	  (remove "ICD10" (umls-concept-atoms cui :language "ENG") :key (lambda(e) (cdr (assoc :root-source e))) :test-not 'equalp)))

(defun snomeds-for-cui (cui)
  (mapcan (lambda(e) (let ((found (cdr (assoc :code e))))
		       (if found
			   (list (#"replaceFirst" found ".*/" ""))
			   nil)))
	  (remove "SNOMEDCT_US" (umls-concept-atoms cui :language "ENG") :key (lambda(e) (cdr (assoc :root-source e))) :test-not 'equalp)))

(defun icd-cuis (icd)
  (mapcan (lambda(e)
	    (let ((found (cdr (assoc :concept e))))
	      (if found (list (#"replaceFirst" found ".*/" "")) nil)))
   (umls-source-term-atoms "ICD10" icd)))


(defun extract-icds (source-atom-cluster)
  (mapcan (lambda(e) (let ((found (and (find '(:root-source . "ICD10") e :test 'equalp)
				       (cdr (assoc :ui e)))))
		       (if found
			   (list found)
			   nil)))
	  (remove "ICD10" source-atom-cluster :key (lambda(e) (cdr (assoc :root-source e))) :test-not 'equalp)))
  
(defun mapped-icd (from id)
  (ignore-errors (extract-icds (umls-mapped-identifiers-for-source from (princ-to-string id) :targetsource "ICD10"))))

#|
Retrieval of icd codes mapped from SNOMED using i-magic web form.

Maybe there's an API, but I'm guessing not since there are interactive choices
to be made. I didn't find one, in any case.

Following is a URL that is submitted when you fill in a snomed term, get a
choice to remove ambiguities, and select the exact match. Below, query
parameters are split into separate lines, with the ones starred the essential
ones.

https://imagic.nlm.nih.gov/imagic/code/map?v=5
js=true *
pabout=
pinstructions=
p=f13c6fc4zd5378314e10 
p.f13c6fc4zd5378314e10.s=213282006 
init-params=
pat=My+Patient+%28modified%29 
pat.init=My+Patient+%28modified%29
q.f=
q.dob=
p=f13c6fc4zd5378314e225 *
p.f13c6fc4zd5378314e225.s=59857007 *
p.f13c6fc4zd5378314e225.e=Branchial+cleft+cyst *
p.f13c6fc4zd5378314e225.o=Branchial+cleft+cyst *
pdone=Get+ICD+Codes
qadd=



The p=<xxx> is some sort of session id. I generate one randomly for each request.
I set js false, in case that helps.
p.<xxx>.s is the snomed id
p.<xxx>.e is the snomed term
p.<xxx>.o is the snomed term again

The HTML response comes back, and we pull out the ICD codes in <span class="icdcode"...
If there are more than one code, manual intervention is needed.

The page might offer a problem refinement. If it does this is a list of possible diagnoses is parsed from the form, and
is returned as a second value. Probably need manual intervention there too, but maybe not if there's only one ICD code
returned.

See 
Documentation on what it does: https://www.nlm.nih.gov/research/umls/mapping_projects/IMAGICImplementationGuide.pdf
The web demo https://imagic.nlm.nih.gov/imagic/code/map

|#
(defun imagic-snomed-to-icd (snomed-id snomed-description &key (debug t))
  (let ((session (string-downcase (format nil "~x" (random most-positive-java-long)))))	;"7e485f37zd52817e10"));
    (macrolet ((session-var (key &optional value)					; key is list of one or 2 
		 `(if (atom ',key)
		      (list (string-downcase (string ',key)) session)
		      (list (format nil "~a.~a.~a" 
				    (string-downcase (string (car ',key)))
				    session
				    (string-downcase (string (second ',key))))
			    (princ-to-string ,value)))))
      (let ((parameters 
	      `(("v" "5")
		("js" "false")
		,(session-var p)
		,(session-var (p s) snomed-id)
		,(session-var (p e) snomed-description)
		,(session-var (p o) snomed-description)
		(pdone "Get ICD Codes")
		)))
	(let ((url (format nil "https://imagic.nlm.nih.gov/imagic/code/map?~{~a=~a~^&~}"
			   (apply 'append
				  (mapcar (lambda(el)
					    (list (string-downcase (string (car el)))
						  (#"encode" 'java.net.URLEncoder
							     (coerce (second el) 'simple-string) "UTF-8")))
					  parameters)))))
	  (let ((response (get-url url :force-refetch t)))
	    (if debug
		(progn
		  (setq @ response)
		  (values
		   (mapcar 'car (all-matches 
		    response
		    "<span class=\"icdcode\">([^<]*)</span>" 1))
		   (map-refinements? response)
		   url))
		(values
		 (mapcar 'car (all-matches 
		 response
		 "<span class=\"icdcode\">([^<]*)</span>" 1))
		 (map-refinements? response))
		)))))))

(defun map-refinements? (response)
  (if (all-matches response ">Problem refinement</th>" 0)
      (let ((choices-text (all-matches response "(?s)<p>Would one of the following diagnoses apply\\?  Choose the most specific one:</p>(.*?)submit-bu
ttons" 0)))
	(remove "" (mapcar 'car (all-matches (caar choices-text) "<input type=\"radio\" [^>]*?><label [^>]*?>([^<]*)" 1)) :test 'equalp))))
