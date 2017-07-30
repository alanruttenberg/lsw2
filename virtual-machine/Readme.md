This uses vagrant and ansible to build a virtual machine that has LSW installed, as well as emacs and
slime to run it.  

You say "vagrant up", and the first time it will install what's needed.

If there's an error and you can fix it, you can restart the deployment with

"vagrant --verbose provision --provision-with resume"

Once the box is running you get into it with "vagrant ssh"

Inside you run emacs, and within emacs M-x slime, which starts up the environment.

You will have access to hermit, pellet, elk,  fact++, z3, vampire, and prover9 

You should probably do "vagrant plugin install vagrant-vbguest" before you do any of this so that the VM automatically
gets the latest os-specific tools installed. If you don't know what this means, then doubly so.

Software
- vagrant: https://www.vagrantup.com/downloads.html
- ansible: http://docs.ansible.com/ansible/latest/intro_installation.html
- virtualbox: https://www.virtualbox.org/wiki/Downloads

notes:
 - I have had trouble with recent versions of virtualbox on OS X 10.10, so I use version 5.1.10
 - There are installers for vagrant and for virtualbox, but ansible not, so:
     - Instructions for OS X https://valdhaus.co/writings/ansible-mac-osx/
     - Instructions for Windows https://ericsysmin.com/2016/07/28/install-ansible-on-windows/

