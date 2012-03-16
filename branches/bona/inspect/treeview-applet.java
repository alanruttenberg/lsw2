package prefuse.demos.applets;

import prefuse.util.ui.JPrefuseApplet;
import java.net.*;

public class TreeView extends JPrefuseApplet {

    public TreeView() {
    }

    public void init() {
        setContentPane(prefuse.demos.TreeView.demo(getParameter("treeml-uri")));
    }
}
