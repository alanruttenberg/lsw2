(eval-when (:compile-toplevel :load-toplevel :execute)
  (intern "LOAD-ONTOLOGY" 'cl-user)
  (intern "LOADED-DOCUMENTS" 'cl-user)
  (intern "*USE-CACHE-AWARE-LOAD-ONTOLOGY*" 'cl-user))

;; First attempts to isolate something in LSW using packages
(defpackage lsw2cache
  (:use cl jss ext)
  (:import-from :cl-user #:get-url #:uri-full #:uri-p #:find-elements-with-tag #:attribute-named #:load-ontology #:loaded-documents #:*use-cache-aware-load-ontology* #:forget-cached-url)
  (:export #:cache-ontology-and-imports #:uncache-ontology))

(defpackage :owlterm (:use))

(shadowing-import '(lsw2cache::cache-ontology-and-imports lsw2cache::uncache-ontology lsw2cache::ontology-cache-location lsw2cache::cache-ontology-and-imports lsw2cache::uri-mapper-for-source) 'cl-user) 
		
  
