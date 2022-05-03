#
# flagutils.pl
#
# Copyright 2005-2011 Standard Performance Evaluation Corporation
#  All Rights Reserved
#
# $Id: flagutils.pl 6710 2011-08-05 21:53:46Z CloyceS $

use strict;

require 'util.pl';

my $version = '$LastChangedRevision: 6710 $ '; # Make emacs happier
$version =~ s/^\044LastChangedRevision: (\d+) \$ $/$1/;
$::tools_versions{'flagutils.pl'} = $version;

# runspec's requirements are actually quite modest
require 'flagutils_common.pl';

1;
