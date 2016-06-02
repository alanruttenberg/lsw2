### *Status of 2016-06-01*

This is a status report on work using the UMLS to try to better structure about 26000 immune related diseases extracted from UMLS concepts for use with [ImmuneExpresso](http://shenorrlab.technion.ac.il/Templates/showpage.asp?DBID=1&LNGID=%201&TMID=302&FID=915&PID=0&IID=1270) related analyses.
Since the disease ontology only covers about 6000 of the >25000 terms in the file, I am seeing what I can do with UMLS. 

UMLS's structure includes "concepts", "atoms" and "source terms"
* Concepts group multiple atoms.
* Atoms are 1:1 with source terms
* A source term is one that has been collected from one of about 100 sources.

I am interested first in hierarchical relationships, and some immediate relationships of other kinds.

There are parent-child relations between (some) concepts, between atoms, and between source terms. The ones from concepts and atoms are asserted by UMLS. The ones on source terms are taken from the source. 

I'm looking Immediate relationships (for example things like part-of, but also including ad-hoc hierarchical relations like "classified-as")  between the source terms, though I will check the immediate relations between concepts and atoms as well.

The plan is to get a feel for what the path to root(s) for terms and immediate relata in the disease list look like. From inspection, and then verified by statistics and spot checking I will see which elements of the path/relationship might be translated into (not too unreliable) axioms. For example one source might have a reliable part relationship, whereas another has a reliable infectious disease hierarchy, etc. Once axioms are placed I can use the classifier to organize the disease terms in different ways. At around this point I will rendezvous with the disease ontology to collect any useful structure I can to augment what I have.

I'm at the stage where I've grokked the UMLS structure and have downloaded a couple hundred thousand UMLS entities that emanate from the disease terms. I'm starting to write exploration functions so I can understand what I see and do various experiments. I'm just about to do the part showing parent/child relations, and following path to root(s). I expect to have some communicable insights based on exploring that in the next couple of days. Today I'm ending with being able to summarize the concept concisely. That looks like:

```
(describe-umls-concept "C0393639")
->
Name
("Hashimoto's encephalitis"
Definitions
 (("Inflammation of the brain secondary to an immune response triggered by the body itself."
   "Child Health and Human Development" "NCI Thesaurus"))
Concept relations (crap, I know)
 (("has relationship other than synonymous, narrower, or broader"
   "Allergic encephalitis"  "C0521657"))
Source terms.
 (("Autoimmune encephalitis, NOS" "SNMI")
  ("Autoimmune Encephalitis" "SNOMED" "MeSH" "NCI Thesaurus"
   "Child Health and Human Development")
  ("Encephalitis autoimmune" "MedDRA")
  ("Autoimmune encephalopathy" "MedDRA" "SNOMED" "Read Codes")
  ("Hashimoto's encephalopathy" "MeSH" "MedDRA")
  ("Steroid-responsive encephalopathy associated with autoimmune thyroiditis"
   "MeSH")
  ("Hashimoto's encephalitis" "MeSH")
  ("Autoimmune encephalitis (disorder)" "SNOMED")
  ("Encephalitis allergic (autoimmune)" "MedDRA")))
  ```

There is a lot of redundancy, so these have been organized so that there is only one of a string, followed by the sources. For example   

```("Hashimoto's encephalopathy" "MeSH" "MedDRA")```

means that in MeSH and MedDRA there is a term with the label  "Hashimoto's encephalopathy" 
