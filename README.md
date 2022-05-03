# sniper-benchmarks

Benchmarks for Sniper Simulator

* SPLASH-2
* PARSEC 2.1
* SPEC CPU2006
* SPEC CPU2006 - Pinballs
* SPEC CPU2017


Instalation Steps:

1. Rename this directory "sniper-benchmarks" to "benchmarks"
2. cd benchmarks
3. export GRAPHITE_ROOT=/path/to/sniper
4. export BENCHMARKS_ROOT=$(pwd)
5. sudo apt-get install gfortran m4 xsltproc pkg-config gettext libx11-dev libxext-dev libxt-dev libxmu-dev libxi-dev
6. make
7. ./run-sniper -p splash2-fft -i test -n 4 -c gainestown


Useful Links:

* Compile benchmarks on Ubuntu 20.04
- https://groups.google.com/g/snipersim/c/2yL2x6nNfVs/m/EmqQlVf6EwAJ
