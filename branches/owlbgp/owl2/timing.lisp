                                             
public static void main(String[] args) throws Exception {
    OWLOntologyManager m=OWLManager.createOWLOntologyManager();
    // load OBI
    OWLOntology o=m.loadOntologyFromOntologyDocument(IRI.create("file:/Users/bglimm/Documents/workspace/TestOntologiesPublic/WINE/winePlusFoodNoImports.rdf"));
    System.out.println("Loaded into an OWLOntology.");
    Configuration c=new Configuration();
    c.reasonerProgressMonitor=new ConsoleProgressMonitor();
    Reasoner reasoner=new Reasoner(c, m, o);
    System.out.println("Loaded into HermiT.");
    // domains and ranges use the class hierarchy, so classify the classes first
    // if not called directly, HermiT will do that for the first range or domain call
    long t=System.currentTimeMillis();
    reasoner.classify();
    System.out.println("Computed the class hierarchy in "+millisToHoursMinutesSecondsString(System.currentTimeMillis()-t)+".");
    System.out.println("There are "+o.getObjectPropertiesInSignature(true).size()+" object properties. ");
    int current=0;
    System.out.print("Computing domains for property: ");
    t=System.currentTimeMillis();
    for (OWLObjectProperty op : o.getObjectPropertiesInSignature(true)) {
        current++;
        System.out.print(current+" ");
         // not sure whether Protege asks for direct only or also indirect, should make a huge difference
        reasoner.getObjectPropertyDomains(op, true); 
    }
    System.out.println();
    System.out.println("Computed domains in "+millisToHoursMinutesSecondsString(System.currentTimeMillis()-t)+".");
    current=0;
    System.out.print("Computing ranges for property: ");
    t=System.currentTimeMillis();
    for (OWLObjectProperty op : o.getObjectPropertiesInSignature(true)) {
        current++;
        System.out.print(current+" ");
        // not sure whether Protege asks for direct only or also indirect, should make a huge difference
        reasoner.getObjectPropertyRanges(op, true); 
    }
    System.out.println();
    System.out.println("Computed ranges in "+millisToHoursMinutesSecondsString(System.currentTimeMillis()-t)+"ms.");
    t=System.currentTimeMillis();
    reasoner.classifyObjectProperties();
    System.out.println("Computed object property hierarchy in "+millisToHoursMinutesSecondsString(System.currentTimeMillis()-t)+"ms.");
}        
public static String millisToHoursMinutesSecondsString(long millis) {
    long time=millis/1000;
    long ms=time%1000;
    String timeStr=String.format(String.format("%%0%dd", 3), ms)+"ms";
    String format=String.format("%%0%dd", 2);
    long secs=time%60;
    if (secs>0) timeStr=String.format(format, secs)+"s"+timeStr;
    long mins=(time%3600)/60;
    if (mins>0) timeStr=String.format(format, mins)+"m"+timeStr;
    long hours=time/3600;  
    if (hours>0) timeStr=String.format(format, hours)+"h"+timeStr;
    return timeStr;  
}