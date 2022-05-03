#  Copyright 1999-2005 ImageMagick Studio LLC, a non-profit organization
#  dedicated to making software imaging solutions freely available.
#
#  You may not use this file except in compliance with the License.  You may
#  obtain a copy of the License at
#
#    http://www.imagemagick.org/script/license.php
#
#  Unless required by applicable law or agreed to in writing, software
#  distributed under the License is distributed on an "AS IS" BASIS,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#  See the License for the specific language governing permissions and
#  limitations under the License.
#
#  Exercise all regression tests:
#  
#    make test
#
#  Exersise one regression test:
#
#    make TEST_VERBOSE=1 TEST_FILES=t/filter.t test
#

use ExtUtils::MakeMaker;
use Config;

# Compute test specification
my $delegate_tests='t/*.t';
my $delegate;
foreach $delegate (qw//) {
  if( -d "t/$delegate" ) {
    $delegate_tests .= " t/$delegate/*.t";
  }
}

# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile
  (
   # Module description
   'ABSTRACT'	=> 'ImageMagick PERL Extension',

   # Perl module name is Image::Magick
   'NAME'	=> 'Image::Magick',

   # Module author
   'AUTHOR'    => 'ImageMagick Studio LLC',

   # Module version
   'VERSION'   => '6.3.6',

   # Preprocessor defines
   'DEFINE'	=> ' -D_LARGE_FILES=1 -DHAVE_CONFIG_H',     # e.g., '-DHAVE_SOMETHING' 

   # Header search specfication and preprocessor flags
   'INC'	=> '-I../ -I/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/libs/imagick/src ',

   # C pre-processor flags (e.g. -I & -D options)
   # 'CPPFLAGS'    => "$Config{'cppflags'} ",

   # C compiler flags (e.g. -O -g)
   'CCFLAGS'     => "$Config{'ccflags'} -O3 -funroll-loops -fprefetch-loop-arrays -static-libgcc -Wl,--hash-style=both,--as-needed -DPARSEC_VERSION=2.1 -DENABLE_PARSEC_HOOKS -I/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/libs/hooks/inst/amd64-linux.gcc-sniper/include -I/home/kleber.kruger/sniper-benchmarks/tools/hooks -I/home/kleber.kruger/snipersim/sniper/include -DENABLE_PARMACS -O2 -Wall -W",

   # Linker flags for building an executable
   'LDFLAGS'    =>  "-L../magick/.libs -lMagick -L../wand/.libs -lWand $Config{'ldflags'} -L/usr/lib64 -L/usr/lib -L/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/libs/hooks/inst/amd64-linux.gcc-sniper/lib -uparmacs_roi_end -uparmacs_roi_start -L/home/kleber.kruger/sniper-benchmarks/tools/hooks -lhooks_base -lrt -pthread",

   # Linker flags for building a dynamically loadable module
   'LDDLFLAGS'  => "-L../magick/.libs -lMagick -L../wand/.libs -lWand $Config{'lddlflags'} -L/usr/lib64 -L/usr/lib -L/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/libs/hooks/inst/amd64-linux.gcc-sniper/lib -uparmacs_roi_end -uparmacs_roi_start -L/home/kleber.kruger/sniper-benchmarks/tools/hooks -lhooks_base -lrt -pthread",

   # Install PerlMagick binary into ImageMagick bin directory
   'INSTALLBIN'	=> '/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/libs/imagick/inst/amd64-linux.gcc-sniper/bin',

   # Library specification
   'LIBS'	=> [ '-L/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/libs/imagick/inst/amd64-linux.gcc-sniper/lib -L../magick/.libs -lMagick -L../wand/.libs -lWand' ],

   # Perl binary name (if a Perl binary is built)
   'MAP_TARGET'	=> 'PerlMagick',

   # Let CFLAGS drive optimization flags by setting OPTIMIZE to empty
   # 'OPTIMIZE'	=> '',

   # Use same compiler as ImageMagick
   'PERLMAINCC'	=> 'gcc',

   # Set Perl installation prefix to ImageMagick installation prefix
#   'PREFIX'	=> '/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/libs/imagick/inst/amd64-linux.gcc-sniper',

   # Include delegate directories in tests
   test		=> { TESTS	=>	$delegate_tests},

   ($Config{'archname'} =~ /-object$/i ? ('CAPI' => 'TRUE') : ()),
);


#
# Substitutions for "makeaperl" section.
#
sub MY::makeaperl {
     package MY; # so that "SUPER" works right
     my $inherited = shift->SUPER::makeaperl(@_);

     # Stinky ExtUtils::MM_Unix likes to append its own library path to $(CC),
     # prior to any user-specified library path so that an installed library is
     # used rather than the library just built.  This substitution function
     # tries to insert our library path first. Also, use the same compiler used
     # to build perlmain.c to link so that a C++ compiler may be used if
     # necessary.
     $inherited =~ s:MAP_LINKCMD\s.*\s*\$\(CC\):MAP_LINKCMD   = \$(PERLMAINCC) -L/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/libs/imagick/inst/amd64-linux.gcc-sniper/lib: ;
     $inherited;
 }

#
# Substitutions for "test" section.
#
sub MY::test {
     package MY; # so that "SUPER" works right
     my $inherited = shift->SUPER::test(@_);

     # Run tests in our environment
     $inherited =~ s:PERL_DL_NONLAZY=1:/bin/bash ../magick.sh PERL_DL_NONLAZY=1:g ;
     $inherited;
 }
