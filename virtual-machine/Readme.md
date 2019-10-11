# Virtualizing LSW

There are two options for virtualizing lsw: vagrant and docker. Vagrant creates a full image which can be suspended,
which means you can start LSW and when restarting the vm it (and all the java/lisp state) will be saved. Docker has the
advantage of using less space (1GB vs 4GB), starting up more quickly, and being able to run multiple instances.
Note 10-11-2019: I haven't been using the vagrant virtualization for a while. Use Docker.

## Vagrant

This uses ansible to build a virtual machine that has LSW installed, as well as emacs and
slime to run it.  

In the virtual-machines directory, execute "vagrant up", and the first time it will install what's needed, so it takes a
little longer.

If there's an error and you can fix it, you can restart the deployment with

"vagrant --verbose provision --provision-with resume"

Once the box is running you get into it with "vagrant ssh"

Inside you run emacs, and within emacs M-x slime, which starts up the environment.

You will have access to hermit, pellet, elk,  fact++, z3, vampire, and prover9 

Software
- vagrant: https://www.vagrantup.com/downloads.html
- ansible: http://docs.ansible.com/ansible/latest/intro_installation.html
- virtualbox: https://www.virtualbox.org/wiki/Downloads

notes:
 - I have had trouble with recent versions of virtualbox on OS X 10.10, so I use version 5.1.10
 - There are installers for vagrant and for virtualbox, but ansible not, so:
     - Instructions for OS X https://valdhaus.co/writings/ansible-mac-osx/
     - Instructions for Windows https://ericsysmin.com/2016/07/28/install-ansible-on-windows/

## Docker

You must install docker first from https://www.docker.com/get-docker

To build and image with lsw, abcl, and required binaries and libraries, you will need this directory
downloaded. Until I split this into a separate repository check out a minimal LSW

```
git clone --branch owlapiv4 --depth 1 https://github.com/alanruttenberg/lsw2.git
cd lsw2/virtual-machine
```

Build the image
```
make docker-lsw
```
To run LSW and interact with a repl
```
docker run -it lsw2/lisp
```
To run LSW from docker image inside your local emacs:
 - put https://github.com/emacs-pe/docker-tramp.el somewhere, add the path to the emacs load-path, and (require 'docker-tramp)
 - clone https://github.com/daewok/slime-docker and add the path you cloned in to load-path, and (require 'slime-docker)
 - Configure slime-docker with (setq slime-docker-implementations `((lsw ("/home/lsw/repos/lsw2/bin/lsw") :image-name "lsw2/lisp")))
 - add slime-tramp as one of your slime-contribs

Run it with M-x slime-docker

Note: 
- This is relatively new tech for me. Please contact me if try but have trouble with these instructions.
- http://kartoza.com/en/blog/how-to-run-a-linux-gui-application-on-osx-using-docker/ gives instructions on how to set
  things up so that X windows will open on a mac running Xquartz, so show-classtree works. Summary,
  (assuming you've installed brew)
```
    brew install socat
    socat TCP-LISTEN:6000,reuseaddr,fork UNIX-CLIENT:\"$DISPLAY\"&  # (do that only once per login session)
```
    in .emacs
(setq slime-docker-implementations `((lsw ("/home/lsw/repos/lsw2/bin/lsw") :image-name "lsw2/lisp" :env (("DISPLAY" . ,(concat (get-ip-address) ":0"))))))
```
    I added the below definition of get-ip-address to my .emacs, which seems to work. YMMV 
```lisp
(defun get-ip-address ()
   (substring (shell-command-to-string
       "ifconfig | grep 'inet ' | grep -v 127.0.0.1 | head -1 | sed 's/.*inet \\(\\([0-9\\.]\\)*\\).*/\\1/'")
	     0 -1))
```
Inside emacs M-x slime-docker <RET>
