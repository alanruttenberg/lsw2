# Introduction #

These instruction explain how to run prototype code to convert aspects of the FMA into OWL


# Installation #

Assume are installing into ~/repos/

```

cd ~/repos
svn co https://lsw2.googlecode.com/svn/trunk lsw2
cd lsw2
svn co http://svn.neurocommons.org/svn/trunk/convert/fma
```

get a version of the SQL dump of the FMA. Here's one, in protege 3.4 database format

```
wget http://dl.dropbox.com/u/4905123/fma_11-20-09_p3.4.zip```

unzip it to get  fma\_11\_20\_09\_p3.4.sql

```
unzip fma_11-20-09_p3.4.zip```

Start up lsw:

```
bin/lsw```

Tell LSW where the sql is:

```
(setq *fma-sql-path* "path to fma sql")```

Load the FMA code:

```
(asdf::oos 'asdf::load-op 'fma2)```

Read in FMA

```
(read-fma-sql)```

Run the kidney conversion

```
(kidney-experiment)```

Will print some stuff. `(setq *log-assertions* t)` if you want to see more.

After a few minutes it will have written the following 6 files, 3 in the owlfiles directory, and 3 in the treeviews directory:

```

ls -l owlfiles

-rw-r--r--  1 alanr  alanr    108113 May  5 13:25 kidney-partonomy.owl
-rw-r--r--  1 alanr  alanr   1249887 May  5 13:22 kidney-hierarchy-with-part-types.owl
-rw-r--r--  1 alanr  alanr   1049349 May  5 13:21 kidney-hierarchy.owl

ls -l treeviews

-rw-r--r--  1 alanr  alanr    776907 May  5 13:26 kidney-partonomy-treeml.xml
-rw-r--r--  1 alanr  alanr  21074336 May  5 13:25 kidney-hierarchy-with-part-types-treeml.xml
-rw-r--r--  1 alanr  alanr   1531214 May  5 13:22 kidney-hierarchy-treeml.xml
```

The treeml.xml files are what are behind the web pages such as http://ashby.csail.mit.edu/treeview/kidney-partonomy.html

There are three OWL files generated:

  * kidney-hierarchy.owl - the hierarchy plus part relationships
  * kidney-hierarchy-with-part-types.owl - the above plus classes "part some x" for all classes in the hierarchy. This file is used to derived the partonomy
  * kidney-partonomy.owl - a derived partonomy - note that the subclass relations here mean "part" not "is\_a" - this is for visualization.

In the lisp environment these are bound to the variables k1, k2, k3, respectively.

To browse the partonomy in lisp,

```
(show-classtree k3)```

Anything at the top level is probably something that needs to be repaired.