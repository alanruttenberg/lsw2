#!/usr/bin/perl

use LWP::Simple;

# perl lsw-eval.pl '(print (multiple-value-list (check (load-kb-jena "obi:branches;obil.owl"))))
my $eval = shift(@ARGV);

if (!lispeval(1))
{
    if ($ENV{ABCL_WD}) { $here = $ENV{ABCL_WD} }
    else {
	$here = `dirname $0`;
	chomp $here;
	$here = $here."/..";
    }

    chdir $here;
    $here = `pwd`;
    chop $here;
    $here.="/";

    my $childpid = fork;

    if (!$childpid)
    { 
	close(STDOUT);
	close(STDERR);
	close(STDIN);
#	open(STERR,">>/tmp/lswerr");
#	open(STDOUT,">>/tmp/lswout");
	system("./abcl --no-init --load $here/scripts/lsw-server-startup.lisp");
    }
    else
    {
	$|=1;
	while (!lispeval(1))
	{ sleep 1; }
	print lispeval($eval),"\n"; 
    }
}
else
{
    print lispeval($eval),"\n"; 
}

sub lispeval 
  {  my $eval = shift @_;
     $eval =~ s/([^A-Za-z0-9])/sprintf("%%%02X", ord($1))/seg;
     get "http://127.0.0.1:6666/eval?eval=$eval"; 
  }

