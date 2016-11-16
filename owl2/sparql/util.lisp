(defpackage :sparql (:include cl-user))
(in-package :sparql)

(defun endpoint-root-classes (endpoint)
 (sparql '(:select (?s) ()
	   (?s !rdf:type !owl:Class)
	   (:minus (?s !owl:deprecated ?v))
	   ;; hangs (:minus (?s !sesame:directSubClassOf ?super)) 
	   (:minus (?s !rdfs:subClassOf ?super))
	   (:filter (not (isBlank ?s))))
	 :endpoint endpoint :flatten t))


