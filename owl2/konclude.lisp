
;; default configuration
;; <?xml version="1.0" encoding="UTF-8"?>
;; <RequestMessage xmlns="http://www.owllink.org/owllink-xml#"
;;     xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
;;     xmlns:owl="http://www.w3.org/2002/07/owl#"
;;     xml:base="http://www.owllink.org/testsuite/1a#"
;;     xsi:schemaLocation="http://www.owllink.org/owllink-xml# http://www.owllink.org/owllink-xml.xsd">
	
;; 	<!--Configurations for the Konclude reasoner.-->	
	
	
	
;; 	<!--If enabled (true) the reasoner writes unsatisfiable concept compinations to the unsatisfiability cache.-->
;; 	<Set key='Konclude.Calculation.Optimization.UnsatisfiableCacheSingleLevelWriting'>
;; 		<Literal>true</Literal>
;; 	</Set>

	
;; 	<!--If enabled (true) the reasoner saves satisfiability and expansion information about sets of 
;; 		concepts into the satisfiable expander cache.-->
;; 	<Set key='Konclude.Calculation.Optimization.SatisfiableExpansionCacheWriting'>
;; 		<Literal>true</Literal>
;; 	</Set>


	
;; </RequestMessage>


;; don't forget about racer: https://github.com/ha-mo-we/Racer and http://sourceforge.net/projects/reasoner/?source=directory, http://sourceforge.net/projects/ontolisp/
