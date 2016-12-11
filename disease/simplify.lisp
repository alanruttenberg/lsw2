;; This file concerns itself with getting, rendering, and simplifying
;; axioms, reducing them to a list of simple conjuncts

(defun all-relevant-axioms (class &optional (ont *default-kb*))
  (append 
   (get-referencing-axioms (dwim-class class ont) :class ont t)
   (loop for c in (ancestors (dwim-class class ont) ont)
      append (get-referencing-axioms c :class ont t)))
  )

;; in a list of axioms. Out a list of class expressions.
(defun transform-axioms (axioms &optional
				  (ont *default-kb*) 
			 &key (split-conjunctions t) 
			   (remove-noise-classes t)
			   (remove-role-groups t)
			   (remove-parenthetical nil))
  (remove-duplicates
   (remove-if 'null 
	      (mapcan (lambda(axiom)
			(funcall (if remove-noise-classes 'remove-noise-classes 'identity)
				 (funcall (if split-conjunctions 'split-conjunctions 'identity)
					  (funcall (if remove-role-groups 'remove-role-groups 'identity)
						   (if (member (car axiom) '(subclass-of equivalent-classes))
						       (third axiom)
						       )))))
		      axioms)) 
   :test 'equalp))

;; in a class expression, out a class expression
(defun remove-role-groups (class-expression)
  (loop for term in class-expression
     if (and (consp term) (eq (second term) !'Role group'@snomed))
     if (consp (car (third term)))
     append (third term)
     else
     collect (third term)
     else collect term))

;; A list of SNOMED terms that are noise as far as having useful
;; information. These break down into different categories
;; - "by site" terms, which are strictly navigational (should-be) defined classes.
;; - overgeneral terms, which are terms that in any interesting case are specialized to something more specific

(defparameter *snomed-uninformative-classes*
  (mapcar (lambda(l) (make-uri-from-label-source :snomed l nil)  )  
	  (append
   
	   '("Finding by site"
	     "Disorder by body site"
	     "Clinical finding"
	     "Disease"
	     "SNOMED CT Concept"
	     "Body organ structure"
	     "Body system structure"
	     "Body region structure"
	     "Anatomical or acquired body structure"
	     "Structure of integumentary system"
	     "Skin AND subcutaneous tissue structure"
	     "Inflammation of specific body systems"
	     "Inflammatory morphology")
	   ;;(mapcar 'remove-parenthetical (labels-matching ".* by body site .*"))
	   '("Radiotherapy by body site" "Surgical repair procedure by body site" "Connective tissue disorder by body site" "Melanoma in situ by body site" "Allergic disorder by body site affected" "Imaging by body site" "Diathermy procedure by body site" "Neoplasm by body site" "Manipulation procedure by body site" "Disorder by body site" "Stimulation procedure by body site" "Introduction of substance by body site")

	   ;;(mapcar 'remove-parenthetical (labels-matching ".* by site .*"))
	   '("Immune system procedure by site" "Diagnostic procedure by site" "Bacterial infection by site" "Dose form by site prepared for" "Finding of sensation by site" "Radiographic imaging procedure by site" "Infected foreign body by site" "Ultrasound studies by site" "Connective tissue by site" "Procedure on respiratory system structure by site" "Infected superficial injury, by site" "Traumatic injury by site" "Fungal infection by site" "Infected insect bite by site" "Finding by site" "Diagnostic procedure on respiratory system structure by site" "Contrast radiology study by site" "Procedure by site" "Viral infection by site" "Tumor invasion by site" "Nuclear medicine study by site" "Thermography by site" "Infection by site")

	   ;; (labels-matching ".*systems.*" ) ;; some of these, but not al
	   )))


;; returns a list of axioms minus those that use one of the above
;; "noise" classes axioms here are in sexp form

;; in a list of class expressions, out a list of class expressions
(defun remove-noise-classes (class-expressions)
  (loop for class-expression in class-expressions
     unless (null class-expression)
     append
       (if (and class-expression (member  
				  (if (atom class-expression) class-expression (third class-expression))
				  *snomed-uninformative-classes*
				  :test 'equalp))
	   nil
	   (if (maybe-exclude-and-or class-expression)
	       nil
	       (list class-expression)))))

;; Given a class expression return a list of class expressions which
;; in any case that the initial class-expression is a conjunction, the
;; conjuncts are included, otherwise a list containing the original
;; class expression is included.

(defun split-conjunctions (class-expression)
  (if (and (consp class-expression) (eq (car class-expression) :and))
      (apply 'append (mapcar 'split-conjunctions (rest class-expression)))
      (list class-expression)))

(defparameter *and-or-override*
  (list !'Sudden onset AND/OR short duration'@snomed))

;; in a class expression, out t if it should be excluded
(defun maybe-exclude-and-or (class-expression)
  (if (atom class-expression)
       (and (not (member class-expression *and-or-override* ))
	    (#"matches" (label-from-uri *default-label-source* class-expression) "(?i).*and/or.*"))
        (and (not (member (third class-expression) *and-or-override* ))
	    (#"matches" (label-from-uri *default-label-source* (third class-expression)) "(?i).*and/or.*"))))
