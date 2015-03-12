
(defvar *hermit-reasoning-tasks*
  '((:concept_satisfiability "satisfiability of concept '{0}'")
    (:consistency "ABox satisfiability")
    (:concept_subsumption "concept subsumption '{0}' => '{1}'")
    (:object_role_satisfiability "satisfiability of object role '{0}'")
    (:data_role_satisfiability "satisfiability of data role '{0}'")
    (:object_role_subsumption "object role subsumption '{0}' => '{1}'")
    (:data_role_subsumption "data role subsumption '{0}' => '{1}'")
    (:instance_OF "class instance '{0}'('{1}')")
    (:object_role_instance_of "object role instance '{0}'('{1}', '{2}')")
    (:data_role_instance_of "data role instance '{0}'('{1}', '{2}')")
    (:entailment "entailment of '{0}'")
    (:domain "check if {0} is domain of {1}")
    (:range "check if {0} is range of {1}")))

(defvar *hermit-debugging-types*
  '((:none "The standard setting is no monitor, i.e., no information is
recorded and printed about what the reasoner does.")

    (:timing "The TIMING tableau monitor print information about the
tableau (number of nodes etc) in certain time intervals.")

    (:timing_with_pause "Waits at certain points (e.g., before building a
tableau) for a keystroke by the user and is apart from that like
TIMING.")

    (:debugger_no_history "This opens a debugging application for
HermiT. HermiT can be controlled with special commands from within the
debugging application. Without history HermiT does not record
information about how assertions have been derived, so one cannot see
the derivation history for an assertion.")
        
    (:debugger_history_on "This opens a debugging application for
HermiT. HermiT can be controlled with special commands from within the
debugging application. HermiT will keep, for each derived
fact/assertion, how the assertion was derived. This is obviously using
a lot more memory than normal, but can be useful when debugging the
reasoner.")))


(defun hermit-reasoning-test-types ()
  (mapcar (lambda(s) (intern s 'keyword))
	  (map 'list #"name" (#"getEnumConstants" (find-java-class "org.semanticweb.HermiT.tableau.ReasoningTaskDescription$StandardTestType")))))

(defun get-hermit-reasoning-test-type (key)
  (find (string key)
	(#"getEnumConstants" (find-java-class "org.semanticweb.HermiT.tableau.ReasoningTaskDescription$StandardTestType"))
	:key #"name"
	:test 'equalp))

(defun hermit-debugging-types 
  (mapcar (lambda(s) (intern s 'keyword))
   (map 'list #"name" (#"getEnumConstants" (find-java-class "org.semanticweb.HermiT.Configuration$TableauMonitorType")))))

(defun get-hermit-debugging-type (key)
  (find (string key)
	(#"getEnumConstants" (find-java-class "org.semanticweb.HermiT.Configuration$TableauMonitorType"))
	:key #"name"
	:test 'equalp))

(defun debug-reasoning (ont timeout)
  "timeout in milliseconds"
  (let ((config (new 'org.semanticweb.HermiT.Configuration))
	(monitor (new 'org.semanticweb.HermiT.monitor.CountingMonitor)))
    (jss::set-java-field config "monitor" monitor)
    (setf (v3kb-hermit-monitor ont) monitor)
    (and timeout (set-java-field config "individualTaskTimeout" (new 'long (prin1-to-string timeout))))
    (set-java-field config "tableauMonitorType" (get-hermit-monitor-type "TIMING"))
    (let ((reasoner (#"createReasoner" (new "org.semanticweb.HermiT.Reasoner$ReasonerFactory") (v3kb-ont ont) config)))
      (#"precomputeInferences" reasoner
			       (jnew-array-from-array
				(find-java-class 'org.semanticweb.owlapi.reasoner.InferenceType)
				(make-array 1 :initial-contents (list (get-java-field 'inferencetype "CLASS_HIERARCHY")))))
      reasoner)))







#|OWLOntologyManager m=OWLManager.createOWLOntologyManager();
OWLOntology o=m.loadOntologyFromOntologyDocument(...);
CountingMonitor cm=new CountingMonitor();
Configuration c=new Configuration();
c.monitor=cm;
Reasoner reasoner=new Reasoner(c, m, o);
long t=System.currentTimeMillis();
reasoner.classify();
System.out.println("Done in "+cm.getOverallTime()+"ms. ");
System.out.println("Performed "+cm.getOverallNumberOfTests()+" tests: "
       +cm.getOverallNumberOfSatTests()+" sat tests, "
       +cm.getOverallNumberOfSubsumptionTests()+" subsumption tests. ");
System.out.println("Top 10 expensive tests:");
for (TestRecord tr : cm.getTimeSortedTestRecords(10)) {
   System.out.println(tr.toString());
}


Alan,
you can get HermiT to print information about individual tests by
setting the timing monitor in HermiT's configuration object, which is
also an OWLReasonerConfiguration.
Configuration c=new Configuration();
c.tableauMonitorType=TableauMonitorType.TIMING;

You can also set a monitor that does not print anything, but just
keeps records over the performed tests:
CountingMonitor cm=new CountingMonitor();
Configuration c=new Configuration();
c.monitor=cm;
After classification (or whatever you did) finishes, this object has
records of the performed tests. At the moment that is just overall
time, overall number of backtrackings, overall number of sat tests,
etc, because I didn't need more. I am now extending that to keep time
and descriptions for each test. Then you can get your top (expensive)
n sat tests, top n subsumption tests, top n any test etc.

I am not sure how to communicate that to Protege, but if there is a
suitable way, I don't see that this is complicated from our side.

Birte

PS: My current (local) extension to get the top 10 most expensive
tests works as follows:

OWLOntologyManager m=OWLManager.createOWLOntologyManager();
OWLOntology o=m.loadOntologyFromOntologyDocument(...);
CountingMonitor cm=new CountingMonitor();
Configuration c=new Configuration();
c.monitor=cm;
Reasoner reasoner=new Reasoner(c, m, o);
long t=System.currentTimeMillis();
reasoner.classify();
System.out.println("Done in "+cm.getOverallTime()+"ms. ");
System.out.println("Performed "+cm.getOverallNumberOfTests()+" tests: "
       +cm.getOverallNumberOfSatTests()+" sat tests, "
       +cm.getOverallNumberOfSubsumptionTests()+" subsumption tests. ");
System.out.println("Top 10 expensive tests:");
for (TestRecord tr : cm.getTimeSortedTestRecords(10)) {
   System.out.println(tr.toString());
}

gives:

Done in 403044ms.
Performed 3037 tests: 1236 sat tests, 1801 subsumption tests.
Top 10 expensive tests:
416 ms for http://purl.obolibrary.org/obo/OBI_1110106 ->
http://purl.obolibrary.org/obo/OBI_1110048 (result: false)
331 ms for http://purl.obolibrary.org/obo/OBI_1110195 ->
http://www.ifomis.org/bfo/1.1/span#FiatProcessPart (result: false)
329 ms for http://purl.org/obo/owl/NCBITaxon#NCBITaxon_3482 ->
http://purl.org/obo/owl/NCBITaxon#NCBITaxon_3483 (result: false)
317 ms for http://purl.obolibrary.org/obo/OBI_0000073 ->
http://www.ifomis.org/bfo/1.1/span#FiatProcessPart (result: false)
316 ms for http://purl.obolibrary.org/obo/OBI_0000433 (result: true)
316 ms for http://purl.org/obo/owl/CL#CL_0000097 ->
http://www.ifomis.org/bfo/1.1/snap#FiatObjectPart (result: false)
305 ms for http://purl.org/obo/owl/NCBITaxon#NCBITaxon_32064 ->
http://purl.org/obo/owl/NCBITaxon#NCBITaxon_1106 (result: false)
300 ms for http://purl.org/obo/owl/NCBITaxon#NCBITaxon_1311 ->
http://www.ifomis.org/bfo/1.1/snap#FiatObjectPart (result: false)
298 ms for http://purl.obolibrary.org/obo/OBI_0200064 (result: true)
298 ms for http://purl.org/obo/owl/NCBITaxon#NCBITaxon_8032 (result: true)
- Show quoted text -
--
Dr. Birte Glimm, Room 306
Computing Laboratory
Parks Road
Oxford
OX1 3QD
United Kingdom
+44 (0)1865 283529|#
