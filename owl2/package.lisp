;; First attempts to isolate something in LSW using packages
(defpackage lsw2cache
  (:use cl jss ext)
  (:import-from :cl-user #:get-url #:uri-full #:uri-p #:find-elements-with-tag #:attribute-named )
  (:export #:cache-ontology-and-imports #:uncache-ontology))

(shadowing-import '(lsw2cache::cache-ontology-and-imports lsw2cache::uncache-ontology) 'cl-user) 
		
  
