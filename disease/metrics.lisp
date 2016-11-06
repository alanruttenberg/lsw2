;; How many terms have no effective differentia, i.e. same parents and axioms
;; Example Alopecia Febrilis &same Alopecia Totalis

;; Can we distinguish all the "disorder of X terms?" We ought to be able
;; to recreate them if we wanted using side information.

;; Can we distinguish all the "foo finding" terms. Same as above.


;; input a csv with one column being a CUI
;; count the ones where there is a corresponding SNOMED Id
(defun count-diseases-with-snomed-mappings (&key 
					      (sheet-path "disease:data;disease-families.csv")
					      (field-number 3))
  (with-open-file (f (namestring (truename sheet-path)))
    (loop with count = 1
       for line = (read-line f nil :eof)
       until (eq line :eof)
       for descendentId = (nth field-number (split-at-char line #\,))
       for atoms = (umls-concept-atoms descendantId)
       when (some #'identity (mapcar (lambda(el) (equal (cdr (assoc  :ROOT-SOURCE el)) "SNOMEDCT_US")) atoms))
       do (incf count)
       when (zerop (mod count 100))
       do (princ ".")
	 finally (return count))))

;; count the number of occurrences of thing in tree using test

(defun tree-count (thing tree &key (test #'eq))
  (cond ((null tree) 0)
	((atom tree)
	 (if (funcall test thing tree) 1 0))
	(t (apply '+ (mapcar (lambda(el) (tree-count thing el :test test)) tree)))))

;; Given a node in the SNOMED hierarchy, count the number of time a
;; term uses two or more role groups in its axioms.  Currently does
;; this by text search for occurrences of "(Role group)", the suffix
;; on the label of each term in the axiom rendered as manchester 
;; **Fix to look in the sexp form and actually check the relation id,
;; since this is sensitive to the label used in rendering the axioms.

(defun how-often-more-than-one-role-group (&optional (root !'Disease'@snomed) (ont *default-kb*))
  (loop with memo = (make-hash-table :test 'equal)
     for f in (descendants root ont) do (setq @ memo)
     sum
       (loop for ax in (all-relevant-axioms f ont)
	    for axfix = (if (consp ax) (car ax) ax)
	  unless (gethash axfix memo)
	  when (consp axfix) do (progn (print-db axfix (all-relevant-axioms f ont)) (break))
	  when (> (length (all-matches axfix "(Role group)" 1)) 2) sum 1 into multiple
	  do (setf (gethash axfix memo) (length (all-matches axfix "(Role group)" 1)))
	   finally (return (values multiple memo)))))
