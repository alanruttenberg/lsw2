#|
2. Abstract Syntax

The syntax for SWRL in this section abstracts from any exchange syntax for OWL and thus facilitates access to and evaluation of the language. This syntax extends the abstract syntax of OWL described in the OWL Semantcs and Abstract Syntax document [OWL S&AS]. Even this abstract syntax is not particularly readable for rules. Examples will thus often be given in an informal syntax. This informal syntax will neither be given an exact syntax nor a mapping to any of the fully-specified syntaxes for SWRL.

The abstract syntax is specified here by means of a version of Extended BNF, very similar to the EBNF notation used for XML [XML]. Terminals are quoted; non-terminals are bold and not quoted. Alternatives are either separated by vertical bars (|) or are given in different productions. Components that can occur at most once are enclosed in square brackets ([…]); components that can occur any number of times (including zero) are enclosed in braces ({…}). Whitespace is ignored in the productions here.

Names in the abstract syntax are RDF URI references [RDF Concepts]. These names may be abbreviated into qualified names, using one of the following namespace names:

Namespace name	Namespace
rdf	http://www.w3.org/1999/02/22-rdf-syntax-ns#
rdfs	http://www.w3.org/2000/01/rdf-schema#
xsd	http://www.w3.org/2001/XMLSchema#
owl	http://www.w3.org/2002/07/owl#
The meaning of each construct in the abstract syntax is informally described when it is introduced. The formal meaning of these constructs is given in Section 3 via an extension of the OWL DL model-theoretic semantics [OWL S&AS].

2.1. Rules

An OWL ontology in the abstract syntax contains a sequence of axioms and facts. Axioms may be of various kinds, e.g., subClass axioms and equivalentClass axioms. It is proposed to extend this with rule axioms.

axiom ::= rule 
A rule axiom consists of an antecedent (body) and a consequent (head), each of which consists of a (posibly empty) set of atoms. A rule axiom can also be assigned a URI reference, which could serve to identify the rule.

rule ::= 'Implies(' [ URIreference ] { annotation } antecedent consequent ')'
antecedent ::= 'Antecedent(' { atom } ')'
consequent ::= 'Consequent(' { atom } ')'
Informally, a rule may be read as meaning that if the antecedent holds (is "true"), then the consequent must also hold. An empty antecedent is treated as trivially holding (true), and an empty consequent is treated as trivially not holding (false). Rules with an empty antecedent can thus be used to provide unconditional facts; however such unconditional facts are better stated in OWL itself, i.e., without the use of the rule construct. Non-empty antecedents and consequents hold iff all of their constituent atoms hold, i.e., they are treated as conjunctions of their atoms. As mentioned above, rules with conjunctive consequents could easily transformed (via the Lloyd-Topor transformations [Lloyd87]) into multiple rules each with an atomic consequent.

atom ::= description '(' i-object ')'
	 | dataRange '(' d-object ')'
	 | individualvaluedPropertyID '(' i-object i-object ')'
	 | datavaluedPropertyID '(' i-object d-object ')'
	 | sameAs '(' i-object i-object ')'
	 | differentFrom '(' i-object i-object ')'
	 | builtIn '(' builtinID { d-object } ')'
builtinID ::= URIreference
Atoms can be of the form C(x), P(x,y), sameAs(x,y) differentFrom(x,y), or builtIn(r,x,...) where C is an OWL description or data range, P is an OWL property, r is a built-in relation, x and y are either variables, OWL individuals or OWL data values, as appropriate. In the context of OWL Lite, descriptions in atoms of the form C(x) may be restricted to class names.

Informally, an atom C(x) holds if x is an instance of the class description or data range C, an atom P(x,y) holds if x is related to y by property P, an atom sameAs(x,y) holds if x is interpreted as the same object as y, an atom differentFrom(x,y) holds if x and y are interpreted as different objects, and builtIn(r,x,...) holds if the built-in relation r (see Sections 3 and 8) holds on the interpretations of the arguments. Note that the sameAs and differentFrom two forms can be seen as "syntactic sugar": they are convenient, but do not increase the expressive power of the language (i.e., such (in)equalities can already be expressed using the combined power of OWL and rules without explicit (in)equality atoms).

i-object ::= i-variable | individualID
d-object ::= d-variable | dataLiteral
Atoms may refer to individuals, data literals, individual variables or data variables. Variables are treated as universally quantified, with their scope limited to a given rule. As usual, only variables that occur in the antecedent of a rule may occur in the consequent (a condition usually referred to as "safety"). This safety condition does not, in fact, restrict the expressive power of the language (because existentials can already be captured using OWL someValuesFrom restrictions).

i-variable ::= 'I-variable(' URIreference ')'
d-variable ::= 'D-variable(' URIreference ')'
2.2. Human Readable Syntax

While the abstract EBNF syntax is consistent with the OWL specification, and is useful for defining XML and RDF serialisations, it is rather verbose and not particularly easy to read. In the following we will, therefore, often use a relatively informal "human readable" form similar to that used in many published works on rules.

In this syntax, a rule has the form:

antecedent ⇒ consequent
where both antecedent and consequent are conjunctions of atoms written a1 ∧ ... ∧ an. Variables are indicated using the standard convention of prefixing them with a question mark (e.g., ?x). Using this syntax, a rule asserting that the composition of parent and brother properties implies the uncle property would be written:

parent(?x,?y) ∧ brother(?y,?z) ⇒ uncle(?x,?z)
In this syntax, built-in relations that are functional can be written in functional notation, i.e., op:numeric-add(?x,3,?z) can be written instead as

?x = op:numeric-add(3,?z)


Implies(' [ URIreference ] { annotation } antecedent consequent ')

(add-rule
   '(seq ?el)
   (lambda(bindings)
     (let ((*bindings* bindings))
       (let ((*head* (fresh-blank)))
	 (triple *head* !rdf:first (t (? ?el)))
	 (triple *head* !rdf:rest !rdf:nil)
	 *head*))))

  (add-rule
   '(seq (?+ ?el))
   (lambda(bindings)
     (let ((*bindings* bindings))
       (let ((*head* (fresh-blank)))
	 (triple *head* !rdf:first (t (first (? ?el))))
	 (when (rest (? ?el))
	   (triple *head* !rdf:rest (t `(seq ,@(rest (? ?el))))))
	 *head*))))

|#

(add-rule
 '(swrlseq ?el)
 (lambda(bindings)
   (let ((*bindings* bindings))
     (let ((*head* (fresh-blank)))
       (triple *head* !rdf:type !swrl:AtomList)
       (triple *head* !rdf:first (t (? ?el)))
       (triple *head* !rdf:rest !rdf:nil)
       *head*))))

(add-rule
 '(swrlseq (?+ ?el))
 (lambda(bindings)
   (let ((*bindings* bindings))
     (let ((*head* (fresh-blank)))
       (triple *head* !rdf:type !swrl:AtomList)
       (triple *head* !rdf:first (t (first (? ?el))))
       (when (rest (? ?el))
	 (triple *head* !rdf:rest (t `(seq ,@(rest (? ?el))))))
       *head*))))

(defrdfm declaration 
    (:pattern (declaration (variable ?datatype)) :case :subscript-free)
  (triple (t ?datatype) !rdf:type !swrl:Variable))

(defrdfm implies
    (:pattern (implies (antecedent (?+ ?aatom)) (consequent (?+ ?catom)))
	      :case :subscript-free)
  (triple (:blank ?x) !rdf:type !swrl:Imp)
  (triple (:blank ?x) !swrl:body (t `(swrlseq ,@(mapcar 't ?aatom))))
  (triple (:blank ?x) !swrl:head (t `(swrlseq ,@(mapcar 't ?catom)))))

(defrdfm class-atom
    (:pattern (class-atom ?class ?var) :case :subscript-free :head (:blank ?x))
  (triple (:blank ?x) !rdf:type !swrl:ClassAtom)
  (triple (:blank ?x) !swrl:classPredicate (t ?class))
  (triple (:blank ?x) !swrl:argument1 ?var)
  )

(defrdfm individual-property-atom
    (:pattern (individual-property-atom ?property ?var1 ?var2) :case :subscript-free :head (:blank ?x))
  (triple (:blank ?x) !rdf:type !swrl:IndividualPropertyAtom)
  (triple (:blank ?x) !swrl:propertyPredicate (t ?property))
  (triple (:blank ?x) !swrl:argument1 ?var1)
  (triple (:blank ?x) !swrl:argument2 ?var2)
  )

(defrdfm same-as-atom
    (:pattern (same-as ?var1 ?var2) :case :subscript-free :head (:blank ?x))
  (triple (:blank ?x) !rdf:type !swrl:SameIndividualAtom)
  (triple (:blank ?x) !swrl:argument1 ?var1)
  (triple (:blank ?x) !swrl:argument2 ?var2)
  )

(defrdfm different-from-atom
    (:pattern (different-from ?var1 ?var2) :case :subscript-free :head (:blank ?x))
  (triple (:blank ?x) !rdf:type !swrl:DifferentIndividualsAtom)
  (triple (:blank ?x) !swrl:argument1 ?var1)
  (triple (:blank ?x) !swrl:argument2 ?var2)
  )

(defrdfm datavalued-property-atom
    (:pattern (datavalued-property-atom ?property ?var1 ?var2) :case :subscript-free :head (:blank ?x))
  (triple (:blank ?x) !rdf:type !swrl:DatavaluedPropertyAtom)
  (triple (:blank ?x) !swrl:propertyPredicate (t ?property))
  (triple (:blank ?x) !swrl:argument1 ?var1)
  (triple (:blank ?x) !swrl:argument2 ?var2)
  )

(defrdfm dataoneof 
    (:pattern (dataoneof (:subscript ?literal 1) :elipsis (:subscript ?literal ?n)) :head (:blank ?x) :case :sequence)
  (triple (:blank ?x) !rdf:type !rdfs:Datatype)
  (triple (:blank ?x) !owl:oneOf (t (seq (:subscript ?literal 1) :elipsis (:subscript ?literal ?n)))))

(defrdfm builtin-atom
    (:pattern (builtin-atom ?builtin (:subscript ?var 1) :elipsis (:subscript ?var ?n)) :case :sequence :head (:blank ?x))
  (triple (:blank ?x) !rdf:type !swrl:BuiltinAtom)
  (triple (:blank ?x) !swrl:builtin ?builtin)
  (triple (:blank ?x) !swrl:arguments (t (seq (:subscript ?var 1) :elipsis (:subscript ?var ?n))))
  )

(defrdfm data-range-atom
    (:pattern (data-range-atom ?datarange ?var) :case :subscript-free :head (:blank ?x))
  (triple (:blank ?x) !rdf:type !swrl:DataRangeAtom)
  (triple (:blank ?x) !swrl:datarange (t ?datarange))
  (triple (:blank ?x) !swrl:argument1 ?var)
  )

(defrdfm rule
    (:pattern (rule (?+ ?ant) -> (?+ ?cons)) :case :subscript-free)
  (multiple-value-bind (form variables) (expand-simple-rule-syntax ?ant ?cons)
    (loop for variable in variables do (t `(declaration (variable ,variable))))
    (t form)
  ))

(defun swrl-argument-predicate (i)
  (make-uri nil (format nil "swrl:argument~a" i)))

(defun expand-simple-rule-syntax (ants cons &aux variables)
  (labels ((maybe-variable (term)
	     (when (swrl-uri-p term)
	       (pushnew term variables)))
	   (expand-one (term)
	     (cond ((and (consp term)
			 (uri-p (car term))
			 (equal (length term) 2))
		    (maybe-variable (second term))
		    `(class-atom ,@term))
		   ((and (consp term)
			 (uri-p (car term))
			 (equal (length term) 3))
		    (maybe-variable (second term))
		    (maybe-variable (third term))
		    `(individual-property-atom ,@term))
		   ((and (consp term)
			 (eq (car term) 'data)
			 (uri-p (second term))
			 (equal (length term) 4))
		    (maybe-variable (third term))
		    (maybe-variable (fourth term))
		    `(datavalued-property-atom ,@(rest term)))
		   ((and (consp term)
			 (eq (car term) 'same-as)
			 (equal (length term) 3))
		    (maybe-variable (second term))
		    (maybe-variable (third term))
		    `(same-as-atom ,@(rest term)))
		   ((and (consp term)
			 (eq (car term) 'different-from)
			 (equal (length term) 3))
		    (maybe-variable (second term))
		    (maybe-variable (third term))
		    `(different-from-atom ,@(rest term)))
		   ((and (consp term)
			 (assoc (car term) *swrl-builtins*))
		    (map nil #'maybe-variable (rest term))
		    `(builtin-atom ,(eval-uri-reader-macro (second (assoc (car term) *swrl-builtins*))) ,@(rest term))))))
    (values
	`(implies 
	  (antecedent
	   ,@(loop for item in ants collect (expand-one item)))
	  (consequent
	   ,@(loop for item in cons collect (expand-one item))))
      variables)))

(defvar *swrl-builtins* 
  '((equal !swrlb:equal )
    (notEqual !swrlb:notEqual )
    (lessThan !swrlb:lessThan )
    (lessThanOrEqual !swrlb:lessThanOrEqual )
    (greaterThan !swrlb:greaterThan )
    (greaterThanOrEqual !swrlb:greaterThanOrEqual )
    (add !swrlb:add )
    (subtract !swrlb:subtract )
    (multiply !swrlb:multiply )
    (divide !swrlb:divide )
    (integerDivide !swrlb:integerDivide )
    (mod !swrlb:mod )
    (pow !swrlb:pow)
    (unaryPlus !swrlb:unaryPlus )
    (unaryMinus !swrlb:unaryMinus )
    (abs !swrlb:abs )
    (ceiling !swrlb:ceiling )
    (floor !swrlb:floor )
    (round !swrlb:round )
    (roundHalfToEven !swrlb:roundHalfToEven )
    (sin !swrlb:sin)
    (cos !swrlb:cos)
    (tan !swrlb:tan)
    (booleanNot !swrlb:booleanNot )
    (stringEqualIgnoreCase !swrlb:stringEqualIgnoreCase)
    (stringConcat !swrlb:stringConcat )
    (substring !swrlb:substring )
    (stringLength !swrlb:stringLength )
    (normalizeSpace !swrlb:normalizeSpace )
    (upperCase !swrlb:upperCase )
    (lowerCase !swrlb:lowerCase )
    (translate !swrlb:translate )
    (contains !swrlb:contains )
    (containsIgnoreCase !swrlb:containsIgnoreCase)
    (startsWith !swrlb:startsWith )
    (endsWith !swrlb:endsWith )
    (substringBefore !swrlb:substringBefore )
    (substringAfter !swrlb:substringAfter )
    (matches !swrlb:matches )
    (replace !swrlb:replace )
    (tokenize !swrlb:tokenize )
    (yearMonthDuration !swrlb:yearMonthDuration )
    (dayTimeDuration !swrlb:dayTimeDuration )
    (dateTime !swrlb:dateTime )
    (date !swrlb:date )
    (time !swrlb:time )
    (addYearMonthDurations !swrlb:addYearMonthDurations )
    (subtractYearMonthDurations !swrlb:subtractYearMonthDurations )
    (multiplyYearMonthDuration !swrlb:multiplyYearMonthDuration )
    (divideYearMonthDurations !swrlb:divideYearMonthDurations )
    (addDayTimeDurations !swrlb:addDayTimeDurations )
    (subtractDayTimeDurations !swrlb:subtractDayTimeDurations )
    (multiplyDayTimeDurations !swrlb:multiplyDayTimeDurations )
    (divideDayTimeDuration !swrlb:divideDayTimeDuration )
    (subtractDates !swrlb:subtractDates )
    (subtractTimes !swrlb:subtractTimes )
    (addYearMonthDurationToDateTime !swrlb:addYearMonthDurationToDateTime )
    (addDayTimeDurationToDateTime !swrlb:addDayTimeDurationToDateTime )
    (subtractYearMonthDurationFromDateTime !swrlb:subtractYearMonthDurationFromDateTime )
    (subtractDayTimeDurationFromDateTime !swrlb:subtractDayTimeDurationFromDateTime )
    (addYearMonthDurationToDate !swrlb:addYearMonthDurationToDate )
    (addDayTimeDurationToDate !swrlb:addDayTimeDurationToDate )
    (subtractYearMonthDurationFromDate !swrlb:subtractYearMonthDurationFromDate )
    (subtractDayTimeDurationFromDate !swrlb:subtractDayTimeDurationFromDate )
    (addDayTimeDurationToTime !swrlb:addDayTimeDurationToTime )
    (subtractDayTimeDurationFromTime !swrlb:subtractDayTimeDurationFromTime )
    (subtractDateTimesYieldingYearMonthDuration !swrlb:subtractDateTimesYieldingYearMonthDuration )
    (subtractDateTimesYieldingDayTimeDuration !swrlb:subtractDateTimesYieldingDayTimeDuration )
    (resolveURI !swrlb:resolveURI )
    (anyURI !swrlb:anyURI )
    (listConcat !swrlb:listConcat )
    (listIntersection !swrlb:listIntersection)
    (listSubtraction !swrlb:listSubtraction)
    (member !swrlb:member)
    (length !swrlb:length )
    (first !swrlb:first )
    (rest !swrlb:rest )
    (sublist !swrlb:sublist)
    (empty !swrlb:empty )
    ))

(defun test-swrl ()
  (with-ontology foo () 
      ((declaration (class !a))
       (declaration (object-property !p))
       (declaration (variable !?x))
       (implies 
	(antecedent (class-atom !a !?x))
	(consequent (individual-property-atom !p !?x !?x)))
       )
    (to-owl-syntax foo :rdfxml))
  (with-ontology foo () 
      ((declaration (class !a))
       (declaration (object-property !p))
       (rule (!a !?x) -> (!p !?x !?x)) ; same rule as above
       )
    (to-owl-syntax foo :rdfxml))
  (with-ontology foo () 
      ((declaration (data-property !total))
       (declaration (data-property !used))
       (declaration (data-property !load))
	    
       (declaration (class !memory))
       (rule (!memory !?x) (data !used !?x !?u) (data !total !?x !?t) (divide  !?l !?u !?t) ->  (data !load !?x !?l))
       (class-assertion !memory !m)
       (data-property-assertion !total !m (:literal 100 !xsd:float))
       (data-property-assertion !used !m (:literal 50 !xsd:float)))
    (princ (to-owl-syntax foo :functional))
    (sparql '(:select (?v) () (!m !load ?v)) :use-reasoner :sparqldl :kb foo)
    )
  )
