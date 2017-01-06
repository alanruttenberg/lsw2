(defun mute-owlapi-loggers ()
  (loop for loggerkey in (list (get-java-field 'slf4j.logger "ROOT_LOGGER_NAME" t)
			       'org.semanticweb.owlapi.rdf.rdfxml.parser.OWLRDFConsumer
			       'org.semanticweb.owlapi.io.AbstractOWLParser)
	for logger = (#"getLogger" 'slf4j.LoggerFactory (if (symbolp loggerkey) (find-java-class loggerkey) loggerkey))
	do
	   (set-java-field logger "currentLogLevel" (get-java-field logger "LOG_LEVEL_WARN" t) t)))

(mute-owlapi-loggers)

  
