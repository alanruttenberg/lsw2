(in-package :asdf)

(asdf:defsystem :testing
  :author "Alan Ruttenberg"
  :description "prove + mods"
  :components ((:file "patches")
	       (:file "reporter"))
  :depends-on (prove))
