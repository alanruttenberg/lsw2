(in-package :cl-user)
(defun mute-owlapi-loggers (&optional (level "LOG_LEVEL_WARN"))
  (loop for loggerkey in (list (get-java-field 'slf4j.logger "ROOT_LOGGER_NAME" t)
			       'org.semanticweb.owlapi.rdf.rdfxml.parser.OWLRDFConsumer
			       'org.semanticweb.owlapi.io.AbstractOWLParser
			       'org.semanticweb.owlapi.rdf.rdfxml.parser.AbstractState
                               'org.semanticweb.owlapi.util.OWLAnnotationPropertyTransformer
                               'org.obolibrary.oboformat.parser.OBOFormatParser)
	for logger = (#"getLogger" 'slf4j.LoggerFactory (if (symbolp loggerkey) (find-java-class loggerkey) loggerkey))
	do
           ;; obnoxious protected fields 
           (ignore-errors
            (set-java-field logger (find "currentLogLevel" (#"getDeclaredFields" (find-java-class 'org.slf4j.impl.SimpleLogger)) :key #"getName" :test 'equalp)
                             (get-java-field logger level t) t))
	   (ignore-errors
            (set-java-field logger "currentLogLevel" (get-java-field logger level t) t))
        finally (return logger)))

(mute-owlapi-loggers)

(defmacro with-owl-logging-suppressed (&body body)
  `(unwind-protect
        (progn (mute-owlapi-loggers "LOG_LEVEL_OFF") ,@body)
     (mute-owlapi-loggers)))

  
