c CLASS = C
c  
c  
c  This file is generated automatically by the setparams utility.
c  It sets the number of processors and the class of the NPB
c  in this directory. Do not modify it by hand.
c  
        integer nx_default, ny_default, nz_default
        parameter (nx_default=512, ny_default=512, nz_default=512)
        integer nit_default, lm, lt_default
        parameter (nit_default=20, lm = 9, lt_default=9)
        integer debug_default
        parameter (debug_default=0)
        integer ndim1, ndim2, ndim3
        parameter (ndim1 = 9, ndim2 = 9, ndim3 = 9)
        integer one, nr, nv, ir
        parameter (one=1)
        logical  convertdouble
        parameter (convertdouble = .false.)
        character compiletime*11
        parameter (compiletime='03 May 2022')
        character npbversion*5
        parameter (npbversion='3.3.1')
        character cs1*11
        parameter (cs1='$(HOOKS_FC)')
        character cs2*6
        parameter (cs2='$(F77)')
        character cs3*20
        parameter (cs3='${HOOKS_LDFLAGS_DYN}')
        character cs4*6
        parameter (cs4='(none)')
        character cs5*16
        parameter (cs5='$(GLOBAL_CFLAGS)')
        character cs6*17
        parameter (cs6='$(GLOBAL_LDFLAGS)')
        character cs7*6
        parameter (cs7='randi8')
