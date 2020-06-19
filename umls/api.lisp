(in-package :cl-user)

;; see https://documentation.uts.nlm.nih.gov/rest/home.html

(define-umls-api-function
 umls-search
 "/search/{version}"
 "Retrieves CUIs when searching by term or code"
 "Return a list of CUIs and their names when searching a human readable term.
Return a list of source-asserted identifiers (codes) and their names when searching a human readable term.
Map source-asserted identifiers to UMLS CUIs.
Note that ‘current’ in the URI can always be used to search against the latest UMLS publication.
You may use any valid UMLS release back to 2008AA in your URI if you would like to search against a particular version of the UMLS."
"string	Y	A human readable term, such as ‘gestatational diabetes’, or a code from a source vocabulary, such as 11687002 from SNOMEDCT_US.	Any term or code in the UMLS.	n/a	n/a
inputType	N	Specifies the data type you are using as your search parameter.	‘atom’, ‘code’,‘sourceConcept’,‘sourceDescriptor’,‘sourceUi’	‘atom’	Use ‘sourceUi’ if you aren’t sure if the identifier you’re providing is a code, source concept, or source descriptor.
includeObsolete	N	Return content that is a result of matches on obsolete terms.	true or false	false	n/a
includeSuppressible	N	Return content that is a result of matches on suppressible terms.	true or false	false	n/a
returnIdType	N	Specifies the type of identifier you wish to retrieve.	‘aui’,‘concept’,‘code’,‘sourceConcept’,‘sourceDescriptor’, ‘sourceUi’	‘concept’	Use ‘code’,‘sourceConcept’, ‘sourceDescriptor’, or ‘sourceUi’ if you prefer source-asserted identifiers rather than CUIs in your search results.
sabs	N	Comma-separated list of source vocabularies to include in your search	Any root source abbreviation in the UMLS.	All UMLS sources	n/a
searchType	N	Type of search you wish to use	‘exact’,‘words’,‘leftTruncation’, ‘rightTruncation’,‘approximate’, ‘normalizedString’	‘words’	Use ‘exact’ when using inputType = ‘code’, ‘sourceConcept’, ‘sourceDescriptor’, or ‘sourceUi’.
pageNumber	N	Whole number that specifies which page of results to fetch.	1,2,3, etc	1	n/a
pageSize	N	Whole number that specifies the number of results to include per page.	1,2,3, etc	25	n/a"
  :use-http t)

(define-umls-api-function
  umls-concept-info
 "/content/{version}/CUI/{CUI}"
 "Retrieves information about a known CUI"
"Retrieve information (name, semantic types, number of atoms, etc) for a known CUI from latest UMLS version or a specific release.
Retrieve atoms for a known CUI.
Retrieve all definitions of a known CUI.
Retrieve relationships to other CUIs (broader/narrower).
Note that ‘current’ in the URI can always be used to search against the latest UMLS publication.
You may use any valid UMLS release back to 2008AA in your URI if you would like to search against a particular version of the UMLS.
"
 "sabs	N	One or more source abbreviations	Any root source abbreviation in the UMLS	n/a	Use a comma between each source abbreviation to specify more than one.
ttys	N	One or more term types	Any valid term type in the UMLS	n/a	Use a comma between each term type to specify more than one.
language	N	Retrieve only atoms that have a specific language	Any 3 letter language abbreviation in the UMLS, such as “ENG”,“FRE”,“SPA”,“GER”,“DUT”,“JPN”, etc	All languages are returned by default	Currently filtering by one language is supported.
includeObsolete	N	Include content that is obsolete according to the content provider or NLM.	true or false	false	n/a
includeSuppressible	N	Include content that is suppressible according to NLM Editors .	true or false	false	n/a
pageNumber	N	Whole number that specifies which page of results to fetch.	1,2,3, etc	1	n/a
pageSize	N	Whole number that specifies the number of results to include per page.	1,2,3, etc	25	n/a"
 :one-result-only t)

(define-umls-api-function
  umls-concept-atoms
 "/content/{version}/CUI/{CUI}/atoms"
 "Retrieve all atoms of a UMLS concept (CUI) or source-asserted identifier.
Retrieve the default preferred atom of a UMLS CUI or source-asserted identifier.
Retrieve information about an individual atom (AUI).
Retrieve atom information, such as relations to other atoms, attributes, or definitions.
Retrieve membership information of an atom (to which CUI, code, or content view does an atom belong).
Retrieve atom information such as name,term type, language, and suppressibility/obsolescence.
Note that ‘current’ in the URI can always be used to search against the latest UMLS publication.
You may use any valid UMLS release back to 2008AA in your URI if you would like to search against a particular version of the UMLS."
 "Retrieves atoms and information about atoms for a known CUI"
"sabs	N	One or more source abbreviations	Any root source abbreviation in the UMLS	n/a	Use a comma between each source abbreviation to specify more than one.
ttys	N	One or more term types	Any valid term type in the UMLS	n/a	Use a comma between each term type to specify more than one.
language	N	Retrieve only atoms that have a specific language	Any 3 letter language abbreviation in the UMLS, such as “ENG”,“FRE”,“SPA”,“GER”,“DUT”,“JPN”, etc	All languages are returned by default	Currently filtering by one language is supported.
includeObsolete	N	Include content that is obsolete according to the content provider or NLM.	true or false	false	n/a
includeSuppressible	N	Include content that is suppressible according to NLM Editors .	true or false	false	n/a
pageNumber	N	Whole number that specifies which page of results to fetch.	1,2,3, etc	1	n/a
pageSize	N	Whole number that specifies the number of results to include per page.	1,2,3, etc	25	n/a"
)

(define-umls-api-function
    umls-concept-definitions
    "/content/{version}/CUI/{CUI}/definitions"
    "Retrieves definitions for a known CUI"
    "Retrieve the source-asserted definitions for a known CUI.
Note that ‘current’ in the URI can always be used to search against the latest UMLS publication.
You may use any valid UMLS release back to 2008AA in your URI if you would like to search against a particular version of the UMLS."
    "ticket	Y	A single-use service ticket is required for each call to the API. See authentication for more information	n/a	n/a	n/a
sabs	N	One or more source abbreviations	Any root source abbreviation in the UMLS	n/a	Use a comma between each source abbreviation to specify more than one.
pageNumber	N	Whole number that specifies which page of results to fetch.	1,2,3, etc	1	n/a
pageSize	N	Whole number that specifies the number of results to include per page.	1,2,3, etc	25	n/a"
    )

(define-umls-api-function
    umls-concept-relations
    "/content/{version}/CUI/{CUI}/relations"
  "Retrieves NLM-asserted relationships for a known CUI"
  "Retrieve the NLM-asserted relationships for a known CUI.
NLM does not assert parent or child relationships between concepts.

Note that ‘current’ in the URI can always be used to search against the latest UMLS publication.
You may use any valid UMLS release back to 2008AA in your URI if you would like to search against a particular version of the UMLS."
  "sabs	N	One or more source abbreviations	Any root source abbreviation in the UMLS	n/a	Use a comma between each source abbreviation to specify more than one.
pageNumber	N	Whole number that specifies which page of results to fetch.	1,2,3, etc	1	n/a
pageSize	N	Whole number that specifies the number of results to include per page.	1,2,3, etc	25	n/a"
  )

(define-umls-api-function
    umls-source-term-info
    "/content/{version}/source/{source}/{id}"
    "Retrieve information (name, attributes, term types, number of atoms, memberships etc) for a source concept, source descriptor or code from latest UMLS version or a specific release.
Retrieve atoms for a known source-asserted identifier.
Retrieve immediate parents of a known source-asserted identifier.
Retrieve immediate children of a known source-asserted identifier.
Note that ‘current’ in the URI can always be used to search against the latest UMLS publication.
You may use any valid UMLS release back to 2008AA in your URI if you would like to search against a particular version of the UMLS."
    ""
    ""
    :one-result-only t
    )

(define-umls-api-function
    umls-source-term-atoms
    "/content/{version}/source/{source}/{id}/atoms"
  "Retrieves information about atoms for a known source-asserted identifier"
  "Retrieve all atoms of a UMLS concept (CUI) or source-asserted identifier.
Retrieve the default preferred atom of a UMLS CUI or source-asserted identifier.
Retrieve information about an individual atom (AUI).
Retrieve atom information, such as relations to other atoms, attributes, or definitions.
Retrieve membership information of an atom (to which CUI, code, or content view does an atom belong).
Retrieve atom information such as name,term type, language, and suppressibility/obsolescence.
Note that ‘current’ in the URI can always be used to search against the latest UMLS publication.
You may use any valid UMLS release back to 2008AA in your URI if you would like to search against a particular version of the UMLS."
  "sabs	N	One or more source abbreviations	Any root source abbreviation in the UMLS	n/a	Use a comma between each source abbreviation to specify more than one.
ttys	N	One or more term types	Any valid term type in the UMLS	n/a	Use a comma between each term type to specify more than one.
language	N	Retrieve only atoms that have a specific language	Any 3 letter language abbreviation in the UMLS, such as “ENG”,“FRE”,“SPA”,“GER”,“DUT”,“JPN”, etc	All languages are returned by default	Currently filtering by one language is supported.
includeObsolete	N	Include content that is obsolete according to the content provider or NLM.	true or false	false	n/a
includeSuppressible	N	Include content that is suppressible according to NLM Editors .	true or false	false	n/a
pageNumber	N	Whole number that specifies which page of results to fetch.	1,2,3, etc	1	n/a
pageSize	N	Whole number that specifies the number of results to include per page.	1,2,3, etc	25	n/a"
  )

(define-umls-api-function
    umls-source-term-parents
    "/content/{version}/source/{source}/{id}/parents"
  "Retrieves immediate parents of a source-asserted identifier"
  "Retrieve all immediate children of a known source asserted identifier.
Note that ‘current’ in the URI can always be used to search against the latest UMLS publication.
You may use any valid UMLS release back to 2008AA in your URI if you would like to search against a particular version of the UMLS."
  "pageNumber	N	Whole number that specifies which page of results to fetch.	1,2,3, etc	1	n/a
pageSize	N	Whole number that specifies the number of results to include per page.	1,2,3, etc	25	n/a
")

(define-umls-api-function
    umls-source-term-children
    "/content/{version}/source/{source}/{id}/children"
  "Retrieves immediate children of a source-asserted identifier"
  "Note that ‘current’ in the URI can always be used to search against the latest UMLS publication.
You may use any valid UMLS release back to 2008AA in your URI if you would like to search against a particular version of the UMLS."
  "pageNumber	N	Whole number that specifies which page of results to fetch.	1,2,3, etc	1	n/a
pageSize	N	Whole number that specifies the number of results to include per page.	1,2,3, etc	25	n/a"
  )

(define-umls-api-function
    umls-source-term-ancestors
    "/content/{version}/source/{source}/{id}/ancestors"
  "Retrieves all ancestors of a source-asserted identifier"
  "Note that ‘current’ in the URI can always be used to search against the latest UMLS publication.
You may use any valid UMLS release back to 2008AA in your URI if you would like to search against a particular version of the UMLS."
  "pageNumber	N	Whole number that specifies which page of results to fetch.	1,2,3, etc	1	n/a
pageSize	N	Whole number that specifies the number of results to include per page.	1,2,3, etc	25	n/a"
  )

(define-umls-api-function
    umls-source-term-descendants
    "/content/{version}/source/{source}/{id}/descendants"
    "Retrieves all descendants of a source-asserted identifier"
"Note that ‘current’ in the URI can always be used to search against the latest UMLS publication.
You may use any valid UMLS release back to 2008AA in your URI if you would like to search against a particular version of the UMLS."
"pageNumber	N	Whole number that specifies which page of results to fetch.	1,2,3, etc	1	n/a
pageSize	N	Whole number that specifies the number of results to include per page.	1,2,3, etc	25	n/a"
  )

(define-umls-api-function
    umls-source-term-relations
    "/content/{version}/source/{source}/{id}/relations"
    "Retrieves all relationships of a source-asserted identifier"
  "Retrieve the source-asserted relationships for a known source-asserted identifier.
Note that ‘current’ in the URI can always be used to search against the latest UMLS publication.
You may use any valid UMLS release back to 2008AA in your URI if you would like to search against a particular version of the UMLS."
  "includeRelationLabels	N	One or more relation labels	Any relation label in the UMLS	n/a	Use a comma between each relation label to specify more than one.
includeAdditionalRelationLabels	N	One or more relation attribute	Any relation attribute in the UMLS	n/a	Use a comma between each relation attribute to specify more than one.
pageNumber	N	Whole number that specifies which page of results to fetch.	1,2,3, etc	1	n/a
pageSize	N	Whole number that specifies the number of results to include per page.	1,2,3, etc	25	n/a"
  )

(define-umls-api-function
    umls-source-subsets
    "/content/{version}/source/{source}/{id}/subsets"
    "Retrieves information about source-asserted subsets"
  "Retrieve all subsets for a known UMLS Release.
Retrieve information (name, description, member count, etc.) for a known subset.
Retrieve all attributes of a known subset.
Retrieve all members of a known subset.
Retrieve all attributes for a member of a known subset.
Note that ‘current’ in the URI can always be used to search against the latest UMLS publication.
You may use any valid UMLS release back to 2008AA in your URI if you would like to search against a particular version of the UMLS."
  "pageNumber	N	Whole number that specifies which page of results to fetch.	1,2,3, etc	1	n/a
pageSize	N	Whole number that specifies the number of results to include per page.	1,2,3, etc	25	n/a
language	N	3-letter abbreviation for language	ENG, SPA, GER, etc	n/a	n/a"
  )

(define-umls-api-function
    umls-source-attributes
    "/content/{version}/source/{source}/{id}/attributes"
    "Retrieves information about source-asserted attributes"
"Retrieve the source-asserted attributes for a known source-asserted identifier.
Note that ‘current’ in the URI can always be used to search against the latest UMLS publication.
You may use any valid UMLS release back to 2008AA in your URI if you would like to search against a particular version of the UMLS."
  "pageNumber	N	Whole number that specifies which page of results to fetch.	1,2,3, etc	1	n/a
pageSize	N	Whole number that specifies the number of results to include per page.	1,2,3, etc	25	n/a
includeAttributeNames	N	One or more attribute names	Any attribute name in the UMLS	n/a	Use a comma between each attribute name to specify more than one."
  )

(define-umls-api-function
    umls-semantic-type-info
    "/semantic-network/{version}/TUI/{id}"
    "Retrieves information for a known Semantic Type identifier (TUI)"
  "UMLS CUIs are assigned semantic types. When you make a call to the /CUI/{CUI} service, you will receive back semantic type
information as part of your Concept object."
  )

(define-umls-api-function
    umls-content-views
    "/content-views/{version}"
    "Retrieve all available content views in the UMLS"
  "Retrieve all content views for a known UMLS Release.
Retrieve information (name, description, member count, etc.) for a known content view.
Retrieve all members of a known content view.
Note that ‘current’ in the URI can always be used to search against the latest UMLS publication.
You may use any valid UMLS release back to 2008AA in your URI if you would like to search against a particular version of the UMLS."
  "pageNumber	N	Whole number that specifies which page of results to fetch.	1,2,3, etc.	1	Most content views contain several thousand members. When retrieving members of a content view, use paging to retrieve the complete set.
pageSize	N	Whole number that specifies the number of results to include per page.	1,2,3, etc	25	n/a"
  )

(define-umls-api-function
    umls-content-view-members
    "/content-views/{version}/CUI/{CUI}/members"
    "Retrieve members for a specific content view"
  "Retrieve all content views for a known UMLS Release.
Retrieve information (name, description, member count, etc.) for a known content view.
Retrieve all members of a known content view.
Note that ‘current’ in the URI can always be used to search against the latest UMLS publication.
You may use any valid UMLS release back to 2008AA in your URI if you would like to search against a particular version of the UMLS."
  "pageNumber	N	Whole number that specifies which page of results to fetch.	1,2,3, etc.	1	Most content views contain several thousand members. When retrieving members of a content view, use paging to retrieve the complete set.
pageSize	N	Whole number that specifies the number of results to include per page.	1,2,3, etc	25	n/a"
  )

;; ****************
;; undocumented AUI relations

(define-umls-api-function
    umls-atom-info
    "/content/{version}/AUI/{AUI}"
  "Retrieves information about a known AUI"
  "Retrieves information about a known AUI"
  ""
  :one-result-only t)

(define-umls-api-function
    umls-atom-relations
    "/content/{version}/AUI/{AUI}/relations"
  "Retrieves NLM-asserted relationships for a known AUI"
  "Retrieves NLM-asserted relationships for a known AUI"
  "")

(define-umls-api-function
    umls-atom-parents
    "/content/{version}/AUI/{AUI}/parents"
  "Retrieves NLM-asserted parents for a known AUI"
  "Retrieves NLM-asserted parents for a known AUI"
  "")

(define-umls-api-function
    umls-atom-ancestors
    "/content/{version}/AUI/{AUI}/ancestors"
  "Retrieves ancestors for a known AUI"
  "Retrieves ancestors for a known AUI"
  "")

(define-umls-api-function
    umls-atom-attributes
    "/content/{version}/AUI/{AUI}/attributes"
  "Retrieves NLM-asserted attributes for a known AUI"
  "Retrieves NLM-asserted attributes for a known AUI"
  "")

(define-umls-api-function
    umls-concept-preferred-atom
    "/content/{version}/CUI/{CUI}/atoms/preferred"
  "Retrieves NLM-asserted preferred atom for a CUI"
  "Retrieves NLM-asserted preferred atom for a CUI"
  ""
  :one-result-only t)

(define-umls-api-function
    umls-concept-definitions
    "/content/{version}/CUI/{CUI}/definitions"
  "Retrieves definitions for a CUI"
  "Retrieves definitions for a CUI"
  ""
  :one-result-only t)

(define-umls-api-function
    umls-mapped-identifiers-for-source
  "/crosswalk/{version}/source/{source}/{id}"
  "Retrieves all source-asserted identifiers that share a UMLS CUI with a particular code"
  "Retrieves all source-asserted identifiers that share a UMLS CUI with a particular code"
  "targetSource	N	Returns codes from the specified UMLS vocabulary	Any root source abbreviation in the UMLS. See the \“Abbreviation\” column for a list of UMLS source vocabulary abbreviations.	All UMLS source vocabularies	Use a comma between each source abbreviation to specify more than one.
includeObsolete	N	Determines whether to return obsolete codes.	true,false	false	n/a
pageNumber	N	Whole number that specifies which page of results to fetch.	1,2,3, etc	1	n/a
pageSize	N	Whole number that specifies the number of results to include per page.	1,2,3, etc	25	n/a"  
  )


