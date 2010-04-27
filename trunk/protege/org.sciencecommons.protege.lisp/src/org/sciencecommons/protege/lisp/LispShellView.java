package org.sciencecommons.protege.lisp;

import org.armedbear.lisp.Interpreter;
import org.armedbear.j.Editor;
import bsh.util.JConsole;
import java.io.InputStream;
import java.io.OutputStream;
import javax.swing.JScrollPane;

import org.protege.editor.core.ui.util.UIUtil;
import org.protege.editor.core.ui.view.DisposableAction;
import org.protege.editor.owl.ui.view.AbstractOWLViewComponent;
import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;
import java.util.HashSet;/*

* Copyright (C) 2007, University of Manchester
*
* Modifications to the initial code base are copyright of their
* respective authors, or their employers as appropriate.  Authorship
* of the modifications may be determined from the ChangeLog placed at
* the end of this file.
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.

* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.

* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*/

/**
 * Author: drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jun 18, 2008<br><br>

 * Adapted by Alan Ruttenberg to run ABCL shell (with thanks to logicmoo)
 * 2010-04-01

 */
public class LispShellView extends AbstractOWLViewComponent {

    private Interpreter interpreter;
    //    private JConsole console;
    private JScrollPane console;
    private File lastRunScript = null;

    private DisposableAction runScriptAction = new DisposableAction("Load file", null){

        public void actionPerformed(ActionEvent event) {
            handleRunScript();
        }

        public void dispose() {
        }
    };

    private DisposableAction reRunScriptAction = new DisposableAction("Reload file", null){

        public void actionPerformed(ActionEvent event) {
            handleReRunScript();
        }

        public void dispose() {
        }
    };


    private void handleRunScript() {
        Window f = (Window) SwingUtilities.getAncestorOfClass(Window.class, this);
        File file = UIUtil.openFile(f, "Select a file to load", new HashSet<String>());
        if (file != null){
            lastRunScript = file;
	    reRunScriptAction.setEnabled(true);
	    runScript(file);
        }
    }


    private void handleReRunScript() {
        if (lastRunScript != null){
	    runScript(lastRunScript);
        }
    }

    private void runScript(File script){
	        if (script != null){
		    //            try {
		    LispContext.getInstance().interpreter.eval("(load \""+script.toString()+"\")");
		    LispContext.getInstance().interpreter.eval("(force-output t)");
		}
		//                if (result != null){
		//  console.println(result);
                //}
	//            }
	    //            catch (IOException e) {
	    //                throw new RuntimeException(e);
	    //            }
	    //            catch (EvalError evalError) {
	    // console.print(evalError, Color.RED);
	    //            }
	//        }
    }


    protected void initialiseOWLView() throws Exception {
        installUIClasses();
        org.armedbear.j.Editor.main(new String[0]);
        setLayout(new BorderLayout());

        console = LispContext.getInstance().console;
        add(console, BorderLayout.CENTER);
        addAction(runScriptAction, "A", "A");
        addAction(reRunScriptAction, "A", "B");

        reRunScriptAction.setEnabled(false);
    }
    
    private void installUIClasses() {
        UIManager.getDefaults().put(org.armedbear.j.ButtonUI.class.getCanonicalName(), org.armedbear.j.ButtonUI.class);
        UIManager.getDefaults().put(org.armedbear.j.LabelUI.class.getCanonicalName(), org.armedbear.j.LabelUI.class);
        UIManager.getDefaults().put(org.armedbear.j.ScrollBarUI.class.getCanonicalName(), org.armedbear.j.ScrollBarUI.class);
        UIManager.getDefaults().put(org.armedbear.j.ToolBarUI.class.getCanonicalName(), org.armedbear.j.ToolBarUI.class);
        UIManager.getDefaults().put(org.armedbear.j.ToolTipUI.class.getCanonicalName(), org.armedbear.j.ToolTipUI.class);
    }

    protected void disposeOWLView() {
        // do nothing
    }
}
