lsw2 installation instructions

# Introduction #

I put some notes together on setting up lsw2. I was going to send this to Joseph and Bill, but wanted to run it by you first. I can certainly be available when they try it, but this helps automate things. -Patrice


# Details #

1)download and install GNU emacs

2)install a tool for using subversion, and checkout lsw2 googlecode project

3)download and install perl (activestate perl if on windows)

**If you are on a Mac you may skip to step # 6. (although check to see if there are any path configuration specific lines in bin/lsw or bin/abcl files.)**

4)in lsw2/bin create a file called lsw.bat and paste the following in it (with the paths for your configuration):
perl "C:\Users\apseyed\Documents\ONT\lsw2\bin\abcl"  --load "C:\Users\apseyed\Documents\ONT\lsw2\scripts\lsw-startup.lisp"

5)make a backup of lsw2/bin/abcl, create a new one, and paste the perl script below. edit with the paths of your configuration.
```
### begin ###

#!/usr/bin/perl

# Either use ABCL_WD, or figure out where with are using $0. Latter works 
# on both on cygwin and os x

$ENV{ABCL_WD} = "C:\\Users\\apseyed\\Documents\\ONT\\lsw2";
#$ENV{ABCL_BITS} = "";

#if ($ENV{ABCL_BITS}) 
#  { $bits = "-d$ENV{ABCL_BITS}" }
#else
#  { $bits = "-d32";
#  }

$here = $ENV{ABCL_WD};

if ($ENV{ABCL_RAM}) 
  { $ram = "-Xmx$ENV{ABCL_RAM}m" }
else
  { $ram = "-Xmx1024m";
  }

if ($ENV{ABCL_STACK}) 
  { $stack = "-Xss$ENV{ABCL_STACK}m" }
else
  { $stack = "-Xss24m";
  }
  
$java = $ENV{ABCL_JAVA} || "java";
@version = `$java -version 2>&1`;
$sep = "================================================================";

# flushed: chdir $here;

if ($ENV{OS} =~ /windows/i) {$pathJoin = ";"} else {$pathJoin = ":" }
if (!(@version[0] =~ /version/) ) 
  { complainCantTellVersion() }
elsif (!(@version[0] =~ /"1.[56]/) ) 
  { complainWrongVersion(); exit() }
#@libs = grep(/.jar$/,split /\n/,`find "$here/lib" -follow`);

@libs = ("C:\\Users\\apseyed\\Documents\\ONT\\lsw2\\lib\\abcl.jar", 
"C:\\Users\\apseyed\\Documents\\ONT\\lsw2\\lib\\asm-all-3.1.jar", 
"C:\\Users\\apseyed\\Documents\\ONT\\lsw2\\lib\\bsh-2.0b4.jar",
"C:\\Users\\apseyed\\Documents\\ONT\\lsw2\\lib\\httpserver.jar",
"C:\\Users\\apseyed\\Documents\\ONT\\lsw2\\lib\\jscheme.jar",
"C:\\Users\\apseyed\\Documents\\ONT\\lsw2\\lib\\script-api.jar",
 "C:\\Users\\apseyed\\Documents\\ONT\\lsw2\\lib\\skij.jar",
"C:\\Users\\apseyed\\Documents\\ONT\\lsw2\\lib\\virtjbdc3" );

# flushed: map {s/$here\///} @libs;

#if ($ENV{ABCL_JAR})
#{ @libs = ($ENV{ABCL_JAR},grep(!/abcl.jar/,@libs))}

$libjar = join($pathJoin,@libs);


if ($ENV{ABCL_PROFILE_SHARK})
  { @profileargs = ("-XrunShark") }

#if (`uname` =~ /Darwin/)
# { @dock = ("-Xdock:icon=$here/images/lswicon.icns","-Xdock:name=Armed Bear Common Lisp")}

@args  = ($ram,$stack,$bits,@profileargs,"-cp", $libjar, @dock);

if (!($ENV{OS} =~ /windows/i)) { @args = ("-server",@args);}
print "$java @args org.armedbear.lisp.Main --load $here\\scripts\\system-registry.lisp @ARGV\n";
exec("$java",@args,"org.armedbear.lisp.Main","--load","$here\\scripts\\system-registry.lisp",@ARGV);

sub complainCantTellVersion ()
{ print("I'm trying to tell which version of java you are running, but \"$java -version\" prints\n$sep\n");
  print @version;
  print("$sep\nGoing to try anyways, but this may not work...\n");
}

sub complainWrongVersion ()
{ print("This project needs java 1.5 or 1.6 , but \"$java -version\" prints\n$sep\n");
  print @version;
  print("$sep\nYou either need to install java version 1.5 or 1.6, and/or first set the\n");
  print("environment variable ABCL_JAVA to the appropriate java executable (e.g.\n");
  print("in Mac OS X: /System/Library/Frameworks/JavaVM.framework/Versions/1.5/Commands/java)\n"); 
}

### end ###
```

6) Put the below lisp code in your .emacs file and again correct the paths.

You can always find out where Emacs thinks is your home directory's location by typing C-x d ~/ 

&lt;RET&gt;

. This should present the list of files in the home directory, and show its full name on the first line. Likewise, to visit your init file, type C-x C-f ~/.emacs 

&lt;RET&gt;

.

```
;;; begin ;;;

(show-paren-mode 1)
(setq show-paren-delay 0)

(defalias 'perl-mode 'cperl-mode)
(add-hook 'cperl-mode-hook
               (function (lambda ()
                            (local-set-key (kbd "<f8>") 'cperl-db)
                            )))
                            
(add-hook 'cperl-mode-hook
               (function (lambda ()
                            (local-set-key (kbd "<f5>") 'cperl-mode)
                            )))
                            

(progn
     (add-to-list 'load-path "C:\\Users\\apseyed\\Documents\\ONT\\lsw2\\slime")
     (require 'slime-autoloads)
     (setq slime-net-coding-system 'utf-8-unix)
     (slime-setup '( slime-repl
                     slime-asdf
                     slime-fancy
                     slime-package-fu))
     )

 (progn
   (setq slime-lisp-implementations
           '((lsw ("C:\\Users\\apseyed\\Documents\\ONT\\lsw2\\bin\\lsw.bat")))))


(setq auto-mode-alist
     (append '(("\\.system$" . lisp-mode)
               ("\\.asd$" . lisp-mode)
               ("\\.translations$" . lisp-mode)
               ("\\.cl$" . lisp-mode)
               )
             auto-mode-alist))

(setq slime-connected-hook
     (cons (lambda(&rest args)
             (set-process-filter
              (slime-inferior-process)
              (lambda (process string)
                (slime-write-string string)))
             )
           (if (boundp 'slime-connected-hook) slime-connected-hook nil)))

;;; end ;;;
```


7)start emacs, then start slime: M-x slime ('M' is the meta key, which is ESC)