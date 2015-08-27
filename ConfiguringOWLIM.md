# Introduction #

This page describes how to install OWLIM-Lite and configure it for use with the [R21](https://code.google.com/p/lsw2/source/detail?r=21) system.



# Preliminaries #
  1. Have Java (get it [here](http://www.oracle.com/technetwork/java/javase/downloads/index.html))
  1. Download [Apache Tomcat 7](http://tomcat.apache.org/download-70.cgi) (get the zip immediately under "Core" under "Binary Distributions". There's a copy of a version known to work in OSX/Unix in trunk/owl2/lib/tomcat/
  1. You might need to do: chmod a+x tomcat/bin/**1. Register for and download [OWLIM-Lite](http://www.ontotext.com/owlim/owlim-lite-registration)
  1. I'll assume you have an environment variable $LSW that points to the trunk of LSW
# Installation #**

  1. Unzip and Configure Tomcat
    * Configuration involves setting a few environment variables, as described in the file '[RUNNING.txt](http://tomcat.apache.org/tomcat-7.0-doc/RUNNING.txt)' in the tomcat directory.
  * export JAVA\_HOME=$(/usr/libexec/java\_home)
  * export CATALINA\_HOME=$LSW/owl2/lib/tomcat/
    1. Unzip OWLIM
    1. Deploy OWLIM's version **openrdf-sesame.war** and **openrdf-workbench.war** by copying them into _$CATALINA\_HOME/webapps_ as described in the [Easy Install](http://owlim.ontotext.com/display/OWLIMv52/OWLIM-Lite+Installation#OWLIM-LiteInstallation-Easyinstall) section of the OWLIM Installation Guide


# Running and Creating a Repository #
  1. Increase the Java VM size for Tomcat. Below we set it to 2 gig, but you might have to set it higher depending on your KB
> > export CATALINA\_OPTS="-server -Xmx2048m"
  1. Run Tomcat:
> > _$CATALINA\_HOME/bin/startup.sh_
  1. Go to http://localhost:8080/openrdf-workbench/ in a browser
  1. Click on "New Repository" in the upper left hand corner
  1. Select _OWLIM-Lite_, fill in an ID and Type, and click _Next_
  1. On the next page, select any desired options and click _Create_


# Using the Repository #
  * Return to http://localhost:8080/openrdf-workbench to view and select the repository
  * Note the repository location (e.g., http://localhost:8080/openrdf-sesame/repositories/owlim-lite-test). The SPARQL endpoint for a repository is _$LOCATION_/query (e.g., http://localhost:8080/openrdf-sesame/repositories/owlim-lite-test/query)
  * Queries can also be run within the openrdf workbench by clicking on "Query" on the left-hand side
  * Load OWL files using SPARQL Update (also on the LHS), e.g.
```
load <file:///Users/jonathanbona/ontology/ohd-ontology/src/ontology/pitt-ub-ohsu-r21/imports/BFO2/bfo2.owl>; 
load <file:///Users/jonathanbona/ontology/ohd-ontology/src/ontology/pitt-ub-ohsu-r21/ohd.owl>;
```
> > This takes forever.