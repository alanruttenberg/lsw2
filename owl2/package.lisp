(eval-when (:compile-toplevel :load-toplevel :execute)
  (intern "LOAD-ONTOLOGY" 'cl-user)
  (intern "LOADED-DOCUMENTS" 'cl-user)
  (intern "*USE-CACHE-AWARE-LOAD-ONTOLOGY*" 'cl-user)
  (intern "CLASS-QUERY" 'cl-user))

;; First attempts to isolate something in LSW using packages
(defpackage lsw2cache
  (:use cl jss ext)
  (:import-from :cl-user #:get-url #:uri-full #:uri-p #:find-elements-with-tag #:attribute-named #:load-ontology #:loaded-documents #:*use-cache-aware-load-ontology* #:forget-cached-url)
  (:export #:cache-ontology-and-imports #:uncache-ontology))

(defpackage :lsw2/owlterm (:use))

(defpackage lsw2/dlquery
  (:use cl)
  (:import-from :cl-user cl-user::class-query cl-user::instance-query cl-user::property-query cl-user::get-entity
		cl-user::make-uri cl-user::instantiate-reasoner cl-user::to-axiom-expression cl-user::list-to-java-set
		cl-user::to-class-expression cl-user::v3kb-reasoner cl-user::*default-kb* cl-user::to-iri cl-user::v3kb-datafactory
		cl-user::v3kb-uri2entity cl-user::get-inferred-axioms cl-user::get-owl-literal-value)
  (:import-from :jss #:set-to-list)
  (:import-from :java java::jclass java::jinstance-of-p java::jequal)
  (:export  #:property-children #:descendants #:property-descendants #:parents
	   #:property-parents #:ancestors #:property-ancestors #:instances #:instance-types
	   #:direct-instances #:property-equivalents #:equivalents #:leaves #:same-individuals #:instance-properties
	   #:entailed?  #:satisfiable?  #:is-subclass-of?  #:equivalent-classes?))
(use-package :lsw2/dlquery :cl-user)

(shadowing-import '(lsw2cache::cache-ontology-and-imports lsw2cache::uncache-ontology lsw2cache::ontology-cache-location lsw2cache::cache-ontology-and-imports lsw2cache::uri-mapper-for-source) 'cl-user) 
		
  
