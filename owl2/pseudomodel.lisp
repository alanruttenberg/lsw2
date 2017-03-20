(with-ontology a () 
	   ((asq (< !a !b) 
		 (classassertion !a !a1)
		 (classassertion !b !b1)
		 (declaration (object-property !p))
		 (< !a (some !p !b))))
	   a)

After check-ontology:

(eval  `(let ((reasoner (v3kb-reasoner a)))
	  ,(jss::read-java-expression "reasoner.m_tableau.m_extensionManager.m_binaryExtensionTable.m_tupleTable.m_pages[0].m_objects")))

binaryextension table

0: #<org.semanticweb.HermiT.model.AtomicConcept <http://example.com/a> {2698E0CE}>
1: #<org.semanticweb.HermiT.tableau.Node 1 {378C98A7}>
2: #<org.semanticweb.HermiT.model.AtomicConcept <http://example.com/b> {170018A0}>
3: #<org.semanticweb.HermiT.tableau.Node 2 {635CF20B}>
4: #<org.semanticweb.HermiT.model.AtLeastConcept atLeast(1 <http://example.com/p> <http://example.com/b>) {2A2C89D0}>
5: #<org.semanticweb.HermiT.tableau.Node 1 {2CFBEE02}>
6: #<org.semanticweb.HermiT.model.AtomicConcept <http://example.com/b> {18ADD27B}>
7: #<org.semanticweb.HermiT.tableau.Node 1 {3969E188}>
8: #<org.semanticweb.HermiT.model.AtomicConcept <http://example.com/b> {77C58D7C}>
9: #<org.semanticweb.HermiT.tableau.Node 3 {4BDE678}>

Node 1 is given instance of a
Node 2 is given instance of b
Node 3 is existential instance of b

tuples
1: node 1 instance of a
2: node 2 instance of b
3: node 1 instance of p min 1 b
4: node is also instance of b (since a<b)
5: node 3 instance of b

Don't see a1 b1 - abox?

Ternary expression table:

#<org.semanticweb.HermiT.model.AtomicRole <http://example.com/p> {273AB5DE}> 
#<org.semanticweb.HermiT.tableau.Node 1 {3594A808}> 
#<org.semanticweb.HermiT.tableau.Node 3 {6654A4B2}>

tuples:
Node 1 p Node 3 (the existential).

--- That's a model
(with-ontology a () 
	   ((asq (classassertion !a !a1)
		 (declaration (object-property !p))
		 (< !a (some !p !a))))
	   a)

For this it makes
Binary:

a1 instanceof a
a2 instanceof a
a3 instanceof a
a1 min p 1 a
a2 min p 1 a
a3 min p 1 a

In ternary:
a1 p a2
a2 p a3

Must also be a block
- its on the object a3
$assertionsDisabled: #<java.lang.Boolean true {50D06CE}>
m_blockingObject: #<org.semanticweb.HermiT.blocking.SingleDirectBlockingChecker$SingleBlockingObject org.semanticweb.HermiT.blocking.SingleDirectBlockingChecker$SingleBlockingObject@7dd1e082 {4AF7F224}>
m_directlyBlocked: #<java.lang.Boolean true {27A1A6C2}>
m_blocker: #<org.semanticweb.HermiT.tableau.Node 2 {7B77B829}>
m_unprocessedExistentials: #<java.util.ArrayList [atLeast(1 <http://example.com/p> <http://example.com/a>)] {2E965302}>
m_treeDepth: #<java.lang.Integer 2 {4F1D03CB}>
m_nodeType: #<org.semanticweb.HermiT.tableau.NodeType TREE_NODE {4F0986BD}>
m_parent: #<org.semanticweb.HermiT.tableau.Node 2 {2A459F3}>
m_nodeState: #<org.semanticweb.HermiT.tableau.Node$NodeState ACTIVE {2BA5FD40}>
m_nodeID: #<java.lang.Integer 3 {C81B0A4}>
m_tableau: #<org.semanticweb.HermiT.tableau.Tableau org.semanticweb.HermiT.tableau.Tableau@68c2b40e {FBEF3E7}>
SIGNATURE_CACHE_BLOCKER: #<org.semanticweb.HermiT.tableau.Node -1 {2FF54822}>
NO_EXISTENTIALS: #<java.util.Collections$EmptyList [] {2B3EEF61}>
serialVersionUID: #<java.lang.Long -2549229429321484690 {2ABB7E32}>
Java class: #<java class org.semanticweb.HermiT.tableau.Node {52E28149}>
intendedClass: "org.semanticweb.HermiT.tableau.Node"

(defun describe-model (ontology)
  (let ((binary-tuple-objects
	  (eval  `(let ((reasoner (v3kb-reasoner ,ontology)))
		    ,(jss::read-sharp-quote-expression "reasoner.m_tableau.m_extensionManager.m_binaryExtensionTable.m_tupleTable.m_pages[0].m_objects")))))
    (list
     (loop for i from 0 by 2
	   until (null  (aref binary-tuple-objects i))
	   collect (list (aref binary-tuple-objects i) (aref binary-tuple-objects (1+ i))))
     (let ((ternary-tuple-objects
	     (eval  `(let ((reasoner (v3kb-reasoner ,ontology)))
		       ,(jss::read-sharp-quote-expression "reasoner.m_tableau.m_extensionManager.m_ternaryExtensionTable.m_tupleTable.m_pages[0].m_objects")))))
       (loop for i from 0 by 3
	     until (null  (aref ternary-tuple-objects i))
	     collect (list (aref ternary-tuple-objects i) (aref ternary-tuple-objects (1+ i)) (aref ternary-tuple-objects (+ i 2)))))
     )))

(defun debug-ontology ()
  (let ((config (new 'hermit.configuration)))
    (setf #"{config}.tableauMonitorType" #"org.semanticweb.HermiT.Configuration$TableauMonitorType.DEBUGGER_HISTORY_ON")
    (with-ontology a () 
      ((asq (classassertion !a !a1)
	    (declaration (object-property !p))
	    (< !a (some !p !a))))
      (setq @ a)
      (setq th (threads::make-thread (lambda() 
			      (instantiate-reasoner a :hermit nil  config)
				       (loop do (threads:yield)))))
      (threads:yield)
      (block fix
	(loop repeat 10
	      for frame = (find "HermiT Debugger" (remove-if-not #"isDisplayable" (#"getFrames" 'awt.Frame)) :key #"getTitle" :test 'equalp :from-end t)
	      when frame do (progn (#"setDefaultCloseOperation" frame 2) (return-from fix  (values)))
		do (sleep 1)))
      (sleep 1)
      (get-java-field (get-java-field (v3kb-reasoner @) "m_tableau" t) "m_tableauMonitor" t)
      )))

    
(defun cmd (s) 
  (#"processCommandLine" debugger s)
  (#"flush" #"{debugger}.m_output")
  (#"interrupt" #"{th}.javaThread"))
