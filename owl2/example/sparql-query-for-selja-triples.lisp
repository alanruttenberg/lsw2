;; This query finds all triples S R O
;; 
(let ((all-classes (sparql '(:select (?class) () (?class !rdf:type !owl:Class)) :reasoner :none :kb b :flatten t))
      (all-ops (sparql '(:select (?class) () (?class !rdf:type !owl:ObjectProperty)) :reasoner :none :kb b :flatten t)))
  (loop for prop in all-ops do
       (loop for class in (remove !owl:Nothing all-classes)
	  do
	    (loop for target in all-classes
	       do (if (is-subclass-of? class `(object-some-values-from ,prop ,target) b) (print-db (rdfs-label class b) (rdfs-label prop b) (rdfs-label target b)))))))
