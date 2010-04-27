package org.sc.prefuse;

import prefuse.util.ui.JPrefuseApplet;
import java.net.*;
import javax.swing.ToolTipManager;
import org.sc.prefuse.TreeView;

public class TreeViewApplet extends JPrefuseApplet {

    public TreeViewApplet() {
    }

    public void init() {
      ToolTipManager.sharedInstance().setInitialDelay(10);
      ToolTipManager.sharedInstance().setDismissDelay(30000);
      setContentPane(TreeView.demo(getParameter("treeml-uri"),getParameter("label"),getParameter("initial-depth"),
				   getParameter("maximum-depth")));
    }
}
