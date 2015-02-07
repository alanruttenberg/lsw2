package org.sciencecommons.protege.lisp;

import org.armedbear.lisp.Interpreter;
import org.armedbear.lisp.Lisp;
import org.armedbear.lisp.Symbol;
import org.armedbear.lisp.LispObject;
import bsh.util.JConsole;
import java.io.InputStreamReader;
import snow.swing.ConsoleDocument;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.text.JTextComponent;


/*
 * The singleton approach is used here because we are relying on
 * the bundle being a singleton bundle.
 */
public class LispContext {
    private static LispContext instance;
    public Interpreter interpreter;
    //    public JConsole console;
    public JScrollPane console;
    public Thread lispThread;

    private LispContext() {
	//	console = new JConsole();
	console = new JScrollPane();
	Runnable r = new Runnable()
	    {
// 		public void run()
// 		{
// 		    System.setIn(console.getInputStream());
// 		    System.setOut(console.getOut());
// 		    System.setErr(console.getErr());
// 		    String[] args = {"--noinit"};
// 				    //		    String[] args = {};
// 		    interpreter = Interpreter.createDefaultInstance(args);
// 		    Symbol.LOAD_VERBOSE.setSymbolValue(LispObject.getInstance(true));
// 		    if (interpreter == null)
// 			{ console.print("null interpreter!"); }
// 		    interpreter.eval("(print \"Starting up protege lisp interpreter\")");
// 		    //		    Lisp.resetIO(console.getInputStream(),console.getOut());
// 		    //		    interpreter.eval("(load \"/Users/alanr/repos/slimenew/slime/swank-loader.lisp\")");
// 		    interpreter.eval("(progn (loop do (print (read-char))))");
// 		    interpreter.run();
// 		};

		public void run()
		{
		    try {		
			String[] args = {"--noinit"};
			interpreter = Interpreter.createInstance();
		    } catch (Throwable e) {
			e.printStackTrace();
			System.exit(1);
		    }
		    final ConsoleDocument d = new ConsoleDocument(interpreter.eval("#'top-level::top-level-loop"));
		    final JTextComponent txt = new JTextArea(d);
		    d.setupTextComponent(txt);
		    console.setViewportView(txt);
		    // interpreter.run();
		}
		
	    };
		new Thread(null, r, "interpreter", 4194304L).start();
    }
    
    public static LispContext getInstance() {
	if (instance == null) {
	    instance = new LispContext();
	}
        return instance;
    }
    
    public void dispose() {
	interpreter.dispose();
        instance = null;
    }
}
