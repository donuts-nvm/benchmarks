#
#  output_raw.pl - produces RAW output
#  Copyright 1995-2011 Standard Performance Evaluation Corporation
#   All Rights Reserved
#
#  Authors:  Christopher Chan-Nui
#            Cloyce D. Spradling
#
# $Id: output_raw.pl 6710 2011-08-05 21:53:46Z CloyceS $

package Spec::Format::raw;

use strict;
use IO::File;
use UNIVERSAL qw(isa);

@Spec::Format::raw::ISA = qw(Spec::Format);

require 'format_raw.pl';

1;
