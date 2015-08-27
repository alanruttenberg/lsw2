#Instructions on how to use the OBO to OWL Conversion Script

# Introduction #

To use this script, SVN software is required, and can be obtained here

Windows Users: http://tortoisesvn.net/downloads.html
# Details #

GETTING THE CODE

For now there are no releases; get the code via svn:

> svn checkout http://oboformat.googlecode.com/svn/trunk/

RUNNING THE CONVERTER ON THE COMMAND LINE:

From the command line, navigate to the home directory where the download was set. From there, type in bin\obolib-gui.bat

This will bring up a GUI with options for adding/removing files, as well as the directory to export to and the filename for the converted file.

Select the file you want to convert by clicking on "ADD", and navigate to the location and select.

Next choose the folder to export to in the folder line, and type the filename in the filename line. Select whether you are converting from OBO to OWL, or OWL to OBO. Click "Run Conversion" to convert the file.