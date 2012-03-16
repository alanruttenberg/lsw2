(defpackage xmls-nst
  (:nicknames xmls-test)
  (:use :common-lisp :nst :xmls)
  )

(in-package :xmls-nst)

(def-test-group xmls-test ()
  (def-test check-cdata-backtrack (:equalp (list "name" nil "x]"))
    (parse "<name><![CDATA[x]]]></name>"))
  (def-test bigger-check-cdata-backtrack (:equalp (list "description" NIL
 "Link to Catalog In this sequel to 2010's surprise hit, Greg Heffley, the kid who made \"wimpy\" cool is back in an all-new family comedy based on the best-selling follow-up novel by Jeff Kinney. (Kinney's Wimpy Kid\" series has thus far sold 42 million books.) As he begins seventh grade, Greg and his older brother [...]"))
    (parse "<description><![CDATA[Link to Catalog In this sequel to 2010's surprise hit, Greg Heffley, the kid who made \"wimpy\" cool is back in an all-new family comedy based on the best-selling follow-up novel by Jeff Kinney. (Kinney's Wimpy Kid\" series has thus far sold 42 million books.) As he begins seventh grade, Greg and his older brother [...]]]></description>"))
  )