#!/spec/cpu2006/bin/specperl
#!/spec/cpu2006/bin/specperl -d
#!/usr/bin/perl
#
# printpath.pl
# No support is provided for this script.
#
# Copyright 1999-2011 Standard Performance Evaluation Corporation
#  All Rights Reserved
#
# $Id: printpath.pl 6364 2011-03-05 00:41:51Z cloyce $
# Print an NT path with some carriage returns for legibility
# Include hats so the output can be captured for re-use if desired.
# J.Henning 22 April 1999
#
@path = split ";", $ENV{'PATH'};
$path[1] =~ s/^PATH=//;
print "\nPATH=^\n";
print join (";^\n", @path), "\n";
