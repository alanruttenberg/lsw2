package org.sciencecommons.protege.lisp;

import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

public class Activator implements BundleActivator {

    /*
     * It is not clear that this project needs an activator but I am using it 
     * to do any one-time startup of the environment.
     */
    public void start(BundleContext context) throws Exception {
	//      LispContext.getInstance();
    }

    public void stop(BundleContext context) throws Exception {
        // LispContext.getInstance().dispose();
    }

}
