(in-package :cl-user)
(defun icds-for-cui (cui)
  (mapcan (lambda(e) (let ((found (cdr (assoc :code e))))
		       (if found
			   (list (#"replaceFirst" found ".*/" ""))
			   nil)))
	  (remove "ICD10" (umls-concept-atoms cui :language "ENG") :key (lambda(e) (cdr (assoc :root-source e))) :test-not 'equalp)))

(defun snomeds-for-cui (cui)
  (remove-duplicates (mapcan (lambda(e) (let ((found (cdr (assoc :code e))))
		       (if found
			   (list (#"replaceFirst" found ".*/" ""))
			   nil)))
	  (remove "SNOMEDCT_US" (umls-concept-atoms cui :language "ENG") :key (lambda(e) (cdr (assoc :root-source e))) :test-not 'equalp)) :test 'equalp))

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

(defun cuis-for-snomed-id (id)
  (let ((atoms (umls-source-term-atoms  "SNOMEDCT_US"  id)))
    (remove-duplicates
     (remove nil
	    (mapcar (lambda(entry)
	      (let ((found (cdr (assoc :concept entry))))
		(when found
		  (#"replaceAll" found ".*/" ""))))
		    atoms)) :test 'equalp)))


(defun definitions-for-snomed-id (id)
  (let ((cuis (cuis-for-snomed-id id))
	(them nil))
    (if (> (length cuis) 1)
	(values nil cuis)
	(mapcar (lambda(e)
		  (let ((source (cdr (assoc :root-source e))))
		    (unless (or (member source '("SCTSPA") :test 'equalp)
				(#"matches" source "MSH.?.*"))
		      (push (cons (cdr (assoc :value e)) source) them)
			    (cdr (assoc :value e)))))
		(car (umls-concept-definitions (car cuis) :ignore-errors t))))
    them))

;; snomed-ids-for-label
;;
;; Input is a label, purportedly of a SNOMED term.
;; Output is a list of pairs. The first element is a SNOMED id, the second
;; element a list of synonyms.
;;
;; (snomed-ids-for-label "Cleft hard and soft palate with bilateral cleft lip")
;; ->
;; (("1085331000119107"
;;   "Cleft hard and soft palate and bilateral cleft lip"
;;   "Cleft palate and bilateral cleft lip"
;;   "Cleft palate and bilateral cleft lip (disorder)"
;;   "Uranostaphyloschisis with bilateral cleft lip")
;;  ("762586009"
;;   "Cleft hard and soft palate with bilateral cleft lip and alveolus"
;;   "Cleft hard and soft palate with bilateral cleft lip and alveolus (disorder)"))
;;
;; Strategy
;; We're looking through search results for label within SNOMED
;; Maybe there's an exact match, maybe not.
;; If there's an exact match, then we have a cui, which one might think uniquely identifies a SNOMED term, but nope
;; We ask for the SNOMED ids for that CUI (which might have similar terms) and for each CUI returned
;; If the match is unambiguous then only that is returned, otherwise there can be multiple ids/synonyms and
;; it has to be manually disambiguated.

(defun snomed-ids-for-label (label)
  (let* ((results (umls-search :string label :sabs "SNOMEDCT_US" ))
	 (name-cuis (loop for result in (cdr (assoc :results results))
			  for name = (cdr (assoc :name result))
			  for cui =  (cdr (assoc :ui result))
			  unless (equalp cui "NONE")
			    collect (list name cui))))
    (let* ((lookups 
	    (if  (find label name-cuis :test 'equalp :key 'car)
		 (snomed-id-for-cui (second (find label name-cuis :test 'equalp :key 'car)))
		  (mapcan (lambda (e)
			    (snomed-id-for-cui (second e) ))
			  name-cuis)))
	   (exact (find label lookups :key 'car :test 'equalp)))
      (let ((maybe 
	      (if exact
		  (let ((id (second exact)))
		    (list (list* id (mapcar 'car (remove (second exact) lookups :test-not 'equalp :key 'second)))))
		  (loop for id in (remove-duplicates (mapcar 'second lookups)  :test 'equalp)
			collect (list* id (mapcar 'car (remove id lookups :test-not 'equalp :key 'second)))))))
	(or maybe
	    (and (#"matches" label ".*\\(.*")
		 (snomed-ids-for-label (#"replaceAll" label "\\s*\\(.*" ""))))))))
	
	    
	    
;; snomed-id-for-cui
;; 
;; Input is a CUI
;; Output is a list of pairs of id and label.

;; (snomed-id-for-cui "C0206067" )
;; ->
;; (("Focal epithelial hyperplasia" "36949004")
;;  ("Focal epithelial hyperplasia (morphologic abnormality)" "36949004")
;;  ("Focal epithelial hyperplasia of mouth" "6121001")
;;  ("Focal epithelial hyperplasia of mouth (disorder)" "6121001")
;;  ("Heck's disease" "6121001")
;;  ("Multifocal epithelial hyperplasia" "6121001")
;;  ("Oral focal epithelial hyperplasia" "6121001"))

;; Worthy of note
;; - A CUI can be associated with multiple SNOMED terms
;; - A SNOMED term can have multiple synonyms.
;; - The labels have annotations to the effect of whether they are full names,
;; - preferred names, etc. We don't return that information as it isn't important
;; for this application.

(defun snomed-id-for-cui (cui)
  (let* ((entries
	    (umls-concept-atoms cui :sabs "SNOMEDCT_US"))
	 (name-sctids
	   (loop for result in entries
		 for name = (cdr (assoc :name result))
		 for sctid =  (#"replaceAll" (cdr (assoc :code result)) ".*/" "")
		 collect (list name sctid))))
    name-sctids))

(defun snomed-labels-for-id (id)
  (remove-duplicates (mapcar (lambda(e) (cdr (assoc :name e))) (umls-source-term-atoms  "SNOMEDCT_US" id))    :test 'equalp))

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
	  (let ((response (get-url url )))
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
  (append
   (when (all-matches response ">Problem refinement</th>" 0)
       (let ((choices-text (all-matches response "(?s)<p>Would one of the following diagnoses apply\\?  Choose the most specific one:</p>(.*?)<td>" 0)))
	 (remove "" (mapcar 'car (all-matches (caar choices-text) "<input type=\"radio\" [^>]*?><label [^>]*?>([^<]*)" 1)) :test 'equalp)))
   (when (all-matches ">Required Episode refinement<" 0)
       (let ((choices-text (all-matches response 

			)))))))

(defun map-refinements? (response)
  (let ((refinement-sections (all-matches response "((?s)<tr[^>]*>(.*?)</tr>)" 1)))
    (loop for (section) in refinement-sections
	  for icd =  (caar (all-matches section "(?s)<span class=\"icdcode\">(.*?)</span>" 1))
	  for episode? = (all-matches section "(?s)(Required Episode refinement)" 1)
	  for options = (let ((choices-text (all-matches section "(?s)<p>Would one of the following diagnoses apply\\?  Choose the most specific one:</p>(.*?)</td>" 0)))
			  (when choices-text
			    (remove "NoneOfTheAbove"
				    (all-matches (caar choices-text)
						 "<input type=\"radio\"\\s*value=\"([^\"]*)\"[^>]*?><label [^>]*?>([^<]*)" 1 2)
				    :key 'car :test 'equalp)))
	  for episodes = (all-matches section
				      "(?s)<div class=\"option\"><input.*?value=\"([^\"]*)\".*?(<span.*?</div>)"
				      1 2)
	  when (or episode? options episodes icd)
	    collect
	    (if (or episode? options)
		(or episode? options)
		(if icd
		    (list :icd icd
			  (caar (all-matches section "(?s)icdname\">([^>]*?)<" 1)))
			  
		    (loop for (value gunk) in  episodes
			  collect (list value (#"replaceAll" gunk "<.*?>" ""))))))))



;(all-matches h "<div class=\"option\"><input type=\"radio\" value=\"[^\"]*\".*(<span>.*<div>)"  1 2) 
