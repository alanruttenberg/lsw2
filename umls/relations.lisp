(in-package :cl-user)

;; from https://www.nlm.nih.gov/research/umls/knowledge_sources/metathesaurus/release/abbreviations.html

(defconstant *umls-relations*
    '(("AQ" "Allowed qualifier")
      ("CHD" "has child relationship in a Metathesaurus source vocabulary")
      ("DEL" "Deleted concept")
      ("PAR" "has parent relationship in a Metathesaurus source vocabulary")
      ("QB" "can be qualified by.")
      ("RB" "has a broader relationship")
      ("RL" "the relationship is similar or "alike". the two concepts are similar or "alike". In the current edition of the Metathesaurus, most relationships with this attribute are mappings provided by a source, named in SAB and SL; hence concepts linked by this relationship may be synonymous, i.e. self-referential: CUI1 = CUI2. In previous releases, some MeSH Supplementary Concept relationships were represented in this way.")
      ("RN" "has a narrower relationship")
      ("RO" "has relationship other than synonymous, narrower, or broader")
      ("RQ" "related and possibly synonymous.")
      ("RU" "Related, unspecified")
      ("SIB" "has sibling relationship in a Metathesaurus source vocabulary.")
      ("SY" "source asserted synonymy.")
      ("XR" "Not related, no mapping")))

AUI	Atom identifier
CODE	Unique Identifier or code for string in source
CUI	Concept unique identifier
RUI	Relationship identifier
SCUI	Source asserted concept unique identifier
SDUI	Source asserted descriptor identifier
