(in-package :cl-user)
(defun mute-owlapi-loggers ()
  (loop for loggerkey in (list (get-java-field 'slf4j.logger "ROOT_LOGGER_NAME" t)
			       'org.semanticweb.owlapi.rdf.rdfxml.parser.OWLRDFConsumer
			       'org.semanticweb.owlapi.io.AbstractOWLParser
			       'org.semanticweb.owlapi.rdf.rdfxml.parser.AbstractState)
	for logger = (#"getLogger" 'slf4j.LoggerFactory (if (symbolp loggerkey) (find-java-class loggerkey) loggerkey))
	do
           ;; obnoxious protected fields 
           (ignore-errors
            (set-java-field logger (find "currentLogLevel" (#"getDeclaredFields" (find-java-class 'org.slf4j.impl.SimpleLogger)) :key #"getName" :test 'equalp)
                            (get-java-field logger "LOG_LEVEL_WARN" t) t))
	   (ignore-errors
            (set-java-field logger "currentLogLevel" (get-java-field logger "LOG_LEVEL_WARN" t) t))
        finally (return logger)))

(mute-owlapi-loggers)

  
