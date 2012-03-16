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
+44 (0)1865 283529