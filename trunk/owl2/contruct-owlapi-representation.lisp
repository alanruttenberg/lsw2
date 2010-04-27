(defun get-owlapi-constructors ()
  (loop for desc in (jclass-method-names "uk.ac.manchester.cs.owl.owlapi.OWLDataFactoryImpl" t)
     for (result method args) = (car (all-matches desc "(((public)|(transient)|(static)|(final)|(native))\\s+)*(\\S+)\\s+([^(]*)\\(([^)]*)\\)" 8 9 10))
       for stripped-method = (#"replaceFirst" method "uk.ac.manchester.cs.owl.owlapi.OWLDataFactoryImpl." "")
       for lisp-method = (#"replaceFirst" (replace-all stripped-method "([A-Z])" (lambda(c) (concatenate 'string "-" (string-downcase c))) 1) "get-o-w-l-" "")
     when (#"matches" method ".*\.getOWL.*")
     unless (member "PrefixManager" args :test 'equal)
     collect (list (#"replaceFirst" result "org.semanticweb.owlapi.model." "")
		   stripped-method
		   lisp-method
		   (mapcar (lambda(e)
			     (#"replaceFirst" e "((org.semanticweb.owlapi.model.)|(org.semanticweb.owlapi.vocab.)|(java.util.)|(java.lang.))" "")) 
			   (split-at-char args #\,)))))

