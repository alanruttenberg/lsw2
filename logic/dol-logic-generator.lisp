(in-package :logic)

(defclass dol-logic-generator (clif-logic-generator)())

(defmethod render-ontology  ((g dol-logic-generator) name-uri axioms &key default-prefix imports)
  (declare (ignorable imports))
  (with-output-to-string (s)
    (if default-prefix (format s "%prefix(: <~a>)%~%~%" (uri-full (or default-prefix *default-uri-base*))) "")
    (if (uri-p name-uri) (format s "distributed-ontology <~a>~%~%" (uri-full name-uri)) "")
    (format s "logic CommonLogic~%")
    (format s "ontology ~a = ~%" (if (uri-p name-uri)
				     (#"replaceAll" (#"replaceAll" (uri-full name-uri) ".*/" "") "\\..*" "")
				     name-uri))
    (format s "(and)~%") ;; because hets commonlogic parser doesn't want to start with an atom
    (format s (render-axioms g axioms))
    (format s "~%end~%")))
  
(defmethod render-ontology  ((g symbol) name-uri axioms &key default-prefix imports)
  (render-ontology (make-instance g) name-uri axioms :default-prefix default-prefix :imports imports))
