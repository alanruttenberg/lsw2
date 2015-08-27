;; create an ontology with one import
(loadOnt '((IMPORTS (MAKE-URI "http://purl.obolibrary.org/obo/iao.owl"))))

;; set the last returned object to a variable
(set ont **)**

;; search for the string "immaterial" in the comment of every object in the ontology

(sparql '(:select (?thing ?comment) () (?thing !rdfs:comment ?comment)(:filter (regex ?comment "immaterial" "i"))) :kb ont
> :use-reasoner :sparqldl)

((!oborel:contained\_in "Containment obtains in each case between material and immaterial continuants, for instance: lung contained\_in thoracic cavity; bladder contained\_in pelvic cavity. Hence containment is not a transitive relation.    If c part\_of c1 at t then we have also, by our definition and by the axioms of mereology applied to spatial regions, c located\_in c1 at t. Thus, many examples of instance-level location relations for continuants are in fact cases of instance-level parthood. For material continuants location and parthood coincide. Containment is location not involving parthood, and arises only where some immaterial continuant is involved. To understand this relation, we first define overlap for continuants as follows:    c1 overlap c2 at t =def for some c, c part\_of c1 at t and c part\_of c2 at t. The containment relation on the instance level can then be defined (see definition):"))
CL-USER>