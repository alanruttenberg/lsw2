(in-package :cl-user)

;; Create a package with all useful symbols and export them, so that non-cl-user packages can just "use" them

(uiop::define-package  lsw2
    (:use lsw2/owlterm)
  (:import-from
   cl-user 
   ;; owlapi.lisp
   #:with-ontology
   #:print-v3kb-struct
   #:v3kb-p
   #:to-iri
   #:load-ontology
   #:check-loaded-versus-mapped
   #:maybe-reorder-assertions
   #:load-kb-jena
   #:include-out-of-line-axioms
   #:include-out-of-line-metadata
   #:pellet-reasoner-config
   #:vanilla-reasoner-config
   #:quiet-reasoner-config
   #:factpp-reasoner-config
   #:jfact-reasoner-config
   #:hermit-reasoner-config
   #:elk-reasoner-config
   #:chainsaw-reasoner-config
   #:reset-reasoner
   #:get-reasoner-factory
   #:instantiate-reasoner
   #:check-ontology
   #:to-class-expression
   #:class-query
   #:instance-query
   #:property-query
   #:get-property-iri-maybe-inverse
   #:signature-query
   #:annotation-properties
   #:object-properties
   #:data-properties
   #:kb-classes
   #:named-individuals
   #:kb-entities
   #:get-asserted-property-characteristics
   #:are-property-characteristic-allowed
   #:get-owl-literal
   #:get-owl-literal-value
   #:entity-annotations
   #:entity-annotation-value
   #:entity-label
   #:entity-annotations-2
   #:loaded-documents
   #:unsatisfiable-classes
   #:unsatisfiable-properties
   #:get-ontology-iri
   #:get-version-iri
   #:get-imports-declarations
   #:add-change
   #:set-version-iri
   #:manchester-parser
   #:parse-manchester-expression
   #:manchester-renderer
   #:compute-uri2entity
   #:to-jena-model
   #:write-rdfxml
   #:get-referencing-axioms
   #:get-rendered-referencing-axioms
   #:manchester-render-axiom
   #:functional-render-axiom
   #:print-latex-rendered-axiom
   #:get-entity
   #:is-property-simple?
   #:make-v3kb
   #:v3kb-name
   #:v3kb-manager
   #:v3kb-ont
   #:v3kb-reasoner
   #:v3kb-datafactory
   #:v3kb-reasoner-factory
   #:v3kb-hermit-monitor
   #:v3kb-short-form-provider
   #:v3kb-manchester-parser
   #:v3kb-manchester-renderer
   #:v3kb-manchester-renderer-writer
   #:v3kb-pellet-jena-model
   #:v3kb-told-jena-model
   #:v3kb-uri2label
   #:v3kb-uri2entity
   #:v3kb-changes
   #:v3kb-weakened-from
   #:v3kb-default-reasoner
   #:v3kb-mapper
   #:v3kb-sparql-ontology-graph
   #:v3kb-sparql-engine
   #:v3kb-sparql-dataset
   #:make-kb-from-java-object
   ;; dl-query.lisp
   #:children
   #:property-children
   #:descendants
   #:property-descendants
   #:parents
   #:property-parents
   #:ancestors
   #:property-ancestors
   #:instances
   #:instance-types
   #:instance-direct-types
   #:direct-instances
   #:property-equivalents
   #:individual-properties
   #:equivalents
   #:leaves
   #:same-individuals
   #:entailed?
   #:satisfiable?
   #:is-subclass-of?
   #:equivalent-classes?
   #:classtree-depth
   #:get-entity-type
   ;; axioms.lisp
   #:each-axiom
   #:simple-subclassof-axiom?
   #:make-subclass-axioms-from-equivalents
   #:simple-equivalentclasses-axiom?
   #:owl-declaration-type
   #:remove-axiom
   #:add-axiom
   #:add-ontology-annotation
   #:add-ontology-imports
   #:remove-ontology-imports
   #:add-version-iri
   #:apply-changes
   #:subclassof-axiom
   #:count-descendants-with-axioms
   #:obo-axiom-element-normalizer
   #:simple-axiom-element-normalizer
   #:property-axiom-terms
   #:axiom-shape
   #:axiom-shapes
   #:examples-of-shape
   #:ppax
   #:*use-cache-aware-load-ontology*
   #:make-kb-from-java-object
   ;; domain-and-ranges.lisp
   #:domains
   #:ranges
   ;; explanation.lisp
   #:explain-inconsistency
   #:explain-unsatisfiable-class
   #:justify-axiom
   #:justified-entailments
   #:axioms-signature
   ;; to-owlapi-class-expression.lisp
   #:to-owlapi-axiom
   ;; check-ontology.lisp
   #:check-profile
   ;; diff.lisp
   #:diff-ontologies
   ;; inferred-axioms.lisp
   #:add-inferred-axioms
   ;; jena.lisp
   #:create-empty-jena-model
   #:make-jena-kb
   #:read-jena-model
   #:each-jena-statement
   #:write-jena-model
   #:write-jena-model-turtle
   #:add-jena-triple
   #:make-jena-literal
   #:fresh-jena-blank
   #:add-jena-triple
   #:make-jena-literal
   ;; label-source.lisp
   #:all-label-sources
   #:each-entity-label
   #:label-uri
   #:uri-label
   #:print-uris-from
   #:rdfs-label
   #:rdfs-labels
   #:labels-matching
   #:labels/terms-matching
   #:to-labels
   #:replace-with-labels
   #:entity-annotations
   #:label-source
   #:careful-label-source
   #:augment-labels
   #:ensure-unique-label
   #:label-from-uri
   #:new-label-source
   #:new-careful-label-source
   #:label-from-uri
   #:make-uri-from-label-source
   ;; manchester-class-expression.lisp
   #:manchester-expression
   #:ce
   ;; signature-module.lisp
   #:create-module-given-terms
   #:sparql-update-load
   #:sparql-endpoint-query
   #:sparql
   #:sparql-stringify
   ;; to-owl-syntax.lisp
   #:to-owl-syntax
   ;; filter.lisp
   #:filter-just-branch-of-ontology
   #:filter-just-subclasses
   #:copy-annotations-between-ontologies
   #:add-axioms-to-ontology
   #:axiom-within-signature?
   #:get-axiom-set-signature
   #:merge-ontologies
   #:as
   #:asq
   ;; utils/namespace.lisp
   #:*namespace-replacements*
   #:maybe-abbreviate-namespace
   #:unabbreviate-namespace
   #:maybe-unabbreviate-namespace
   #:register-namespace
   #:*allow-unknown-namespaces*
   #:with-default-namespace
   ;; utils/uri.lis
   #:*default-uri-base*
   #:make-uri-base-relative
   #:uri-p
   #:make-uri
   #:swrl-uri-p
   #:def-uri-alias
   #:*print-uri-with-labels-from*
   #:*print-uri-with-labels-show-source*
   #:*print-uri-full*
   #:*inhibit-read-uri*
   #:print-uri
   #:decache-uri-abbreviated
   #:*read-time-uri*
   #:read-uri
   #:make-swrl-variable-uri
   #:*saved-readtable*
   #:use-uri-readtable
   #:eval-uri-reader-macro
   #:*default-uri-label-source*
   #:make-uri-from-label-source
   #:uri-full
   #:uri-abbreviated
   #:uri-blank-p
   ;; preferred-label.lis
   #:get-preferred-labels
   ;; tree.lisp
   #:tree-walk
   #:tree-walk-conditional
   #:tree-find
   #:tree-find-if
   #:tree-replace
   #:tree-nsubst-if
   #:tree-remove-if
   ;; owl-to-lisp-syntax.lisp
   #:owl-to-lisp-syntax
   #:axiom-to-lisp-syntax
   #:swrl-rule-to-lisp-syntax
   ;; axioms.lisp
   #:each-axiom
   #:simple-subclassof-axiom?
   #:make-subclass-axioms-from-equivalents
   #:simple-equivalentclasses-axiom?
   #:axiom-typecase
   #:owl-declaration-type
   #:remove-axiom
   #:add-axiom
   #:add-ontology-annotation
   #:add-ontology-imports
   #:remove-ontology-imports
   #:add-version-iri
   #:apply-changes
   #:subclassof-axiom
   #:count-descendants-with-axioms
   #:property-axiom-terms
   ;; graphdb.lisp graphdb9.lisp
   #:graphdb9-instance
   #:json-parse 
   #:json-encode
   #:repository-class
   #:graphdb9-repository
   #:repository-named
   #:get-repository-parameters
   #:json-api-call-post
   #:json-api-call-get
   #:load-server-file
   #:clear-repository
   #:sparql-query
   #:sparql-update
   #:total-triples
   #:geosparql-configuration
   #:enable-geosparql
   #:disable-geosparql
   #:reindex-geosparql
   #:update-geosparql-configuation
   #:re-infer
   #:get-rulesets
   #:explore-ruleset
   #:add-ruleset
   #:change-ruleset
   #:remove-ruleset
   #:*graphdb9-ruleset-directory*
   #:*graphdb9-builtin-rulesets*
   #:sparql-repository-set 
   #:endpoint-named
   #:graphdb-instance
   #:repository-class 
   #:get-repositories 
   #:get-timeout 
   #:get-endpoint-parameter 
   #:set-endpoint-parameter 
   #:get-repository-prefixes
   #:set-repository-prefixes
   ;; generate-mapping.lisp
   #:t
   #:t-jena
   #:t-collect
   ))

(do-symbols (s 'lsw2) (export s 'lsw2))

