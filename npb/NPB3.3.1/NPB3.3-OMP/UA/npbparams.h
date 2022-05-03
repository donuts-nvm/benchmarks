c CLASS = C
c  
c  
c  This file is generated automatically by the setparams utility.
c  It sets the number of processors and the class of the NPB
c  in this directory. Do not modify it by hand.
c  
        integer          lelt, lmor, refine_max, fre_default
        integer          niter_default, nmxh_default
        character        class_default
        double precision alpha_default
        parameter(  lelt=33500,
     >             lmor=1262100,
     >              refine_max=8,
     >              fre_default=5,
     >              niter_default=200,
     >              nmxh_default=10,
     >              class_default="C",
     >              alpha_default=0.067d0 )
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
