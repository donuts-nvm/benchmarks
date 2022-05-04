!-------------------------------------------------------------------------
!
! RYS : get rys roots and weights
!
! Note: x = rho QP^2, output root = t^2
!
! Copyright (C) Daniel Grimwood, 1998
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Library General Public
! License as published by the Free Software Foundation; either
! version 2 of the License, or (at your option) any later version.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Library General Public License for more details.
!
! You should have received a copy of the GNU Library General Public
! License along with this library; if not, write to the
! Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! Boston, MA  02111-1307, USA.
!
! $Id: rys.foo,v 1.12 2003/02/19 07:48:58 reaper Exp $
!-------------------------------------------------------------------------

module RYS_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

   use REALVEC_MODULE, only: create_
   use REALVEC_MODULE, only: create_copy_
   use REALVEC_MODULE, only: destroy_

   implicit none

! Macros file
! $Id: macros,v 1.37.2.11 2003/10/14 02:28:29 reaper Exp $

!----------------------------------------------------------------------
! Kind parameters for value types
!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Size parameters for value types
!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Value types ...
!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Memory/Call stack management macros ...
! These define STACK, CHECK, as well as ENSURE, DIE and WARN ...
!----------------------------------------------------------------------

!----------------------------------------------------------------------
! Precondition and Postcodition macros ...
! These define ENSURE as well as DIE and WARN ...
!----------------------------------------------------------------------

!----------------------------------------------------------------------
! Error macros ...
! These define only DIE and WARN ...
!----------------------------------------------------------------------

!####   define pure
!####   define elemental
!----------------------------------------------------------------------
! Profiling macros ...
! These define START_TIMER, STOP_TIMER ...
!----------------------------------------------------------------------

!-------------------------------------------------------------------------------
! Some compilers cannot handle Fortran 95 features ...
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! Some Fortran 95 compilers can't handle pure routines ...
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! Some Fortran 95 compilers can't handle default initialisation properly ...
!-------------------------------------------------------------------------------

!----------------------------------------------------------------------
! Convenience macros ...
!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Fundamental constants and conversion factors ...
!----------------------------------------------------------------------

!----------------------------------------------------------------------
! Program constants ...
! The first word identifies the module to which the constant belongs
!----------------------------------------------------------------------

!----------------------------------------------------------------------
! Program default input orders for vectors of derived type
! The first word identifies the module to which the constant belongs
!----------------------------------------------------------------------

!----------------------------------------------------------------------
! Derived non-array types ... (in alphabetical order)
!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Parameterised abstract array types ...
!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Derived array types ...
!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Type sizes ...
!----------------------------------------------------------------------
   private

   public    nullify_ptr_part_
   interface nullify_ptr_part_
      module procedure nullify_ptr_part
   end interface

   public    create_
   interface create_
      module procedure create
   end interface

   public    create_copy_
   interface create_copy_
      module procedure create_copy
   end interface

   public    destroy_ptr_part_
   interface destroy_ptr_part_
      module procedure destroy_ptr_part
   end interface

   public    destroy_
   interface destroy_
      module procedure destroy
   end interface

   public    roots_
   interface roots_
      module procedure roots
   end interface

   private    ryssmt_
   interface ryssmt_
      module procedure ryssmt
   end interface

   public    weights_
   interface weights_
      module procedure weights
   end interface

   private    rysfun_
   interface rysfun_
      module procedure rysfun
   end interface

   public    weight_
   interface weight_
      module procedure weight
   end interface

   public    get_weights_and_t2_roots_
   interface get_weights_and_t2_roots_
      module procedure get_weights_and_t2_roots
   end interface

   private    rysnod_
   interface rysnod_
      module procedure rysnod
   end interface

   public    root_
   interface root_
      module procedure root
   end interface

   private    get_weights1_
   interface get_weights1_
      module procedure get_weights1
   end interface

   private    get_weights1_t2_
   interface get_weights1_t2_
      module procedure get_weights1_t2
   end interface

   private    get_weights2_
   interface get_weights2_
      module procedure get_weights2
   end interface

   private    get_weights3_
   interface get_weights3_
      module procedure get_weights3
   end interface

   private    get_weights4_
   interface get_weights4_
      module procedure get_weights4
   end interface

   private    get_weights5_
   interface get_weights5_
      module procedure get_weights5
   end interface

   private    get_weights2_t2_
   interface get_weights2_t2_
      module procedure get_weights2_t2
   end interface

   private    get_weights6_
   interface get_weights6_
      module procedure get_weights6
   end interface

   public    get_weights_and_u_roots_
   interface get_weights_and_u_roots_
      module procedure get_weights_and_u_roots
   end interface

   private    get_weights3_t2_
   interface get_weights3_t2_
      module procedure get_weights3_t2
   end interface

   public    copy_
   interface copy_
      module procedure copy
   end interface

    !pie4 :: real(kind=kind(1.0d0)), parameter, private = 7.85398163397448d-01
   real(kind=kind(1.0d0)), parameter, private :: sqrt_pie4 = 8.862269254527578D-01
   real(kind=kind(1.0d0)), parameter, private :: r12 = 2.75255128608411d-01
   real(kind=kind(1.0d0)), parameter, private :: r22 = 2.72474487139158d+00
   real(kind=kind(1.0d0)), parameter, private :: r13 = 1.90163509193487d-01
   real(kind=kind(1.0d0)), parameter, private :: r23 = 1.78449274854325d+00
   real(kind=kind(1.0d0)), parameter, private :: r33 = 5.52534374226326d+00
   real(kind=kind(1.0d0)), parameter, private :: w22 = 9.17517095361369d-02
   real(kind=kind(1.0d0)), parameter, private :: w23 = 1.77231492083829d-01
   real(kind=kind(1.0d0)), parameter, private :: w33 = 5.11156880411248d-03
   real(kind=kind(1.0d0)), parameter, private :: r14 = 1.45303521503316d-01
   real(kind=kind(1.0d0)), parameter, private :: r24 = 1.33909728812636d+00
   real(kind=kind(1.0d0)), parameter, private :: r34 = 3.92696350135829d+00
   real(kind=kind(1.0d0)), parameter, private :: r44 = 8.58863568901199d+00
   real(kind=kind(1.0d0)), parameter, private :: w24 = 2.34479815323517d-01
   real(kind=kind(1.0d0)), parameter, private :: w34 = 1.92704402415764d-02
   real(kind=kind(1.0d0)), parameter, private :: w44 = 2.25229076750736d-04
   real(kind=kind(1.0d0)), parameter, private :: r15 = 1.17581320211778d-01
   real(kind=kind(1.0d0)), parameter, private :: r25 = 1.07456201243690d+00
   real(kind=kind(1.0d0)), parameter, private :: r35 = 3.08593744371754d+00
   real(kind=kind(1.0d0)), parameter, private :: r45 = 6.41472973366203d+00
   real(kind=kind(1.0d0)), parameter, private :: r55 = 1.18071894899717d+01
   real(kind=kind(1.0d0)), parameter, private :: w25 = 2.70967405960535d-01
   real(kind=kind(1.0d0)), parameter, private :: w35 = 3.82231610015404d-02
   real(kind=kind(1.0d0)), parameter, private :: w45 = 1.51614186862443d-03
   real(kind=kind(1.0d0)), parameter, private :: w55 = 8.62130526143657d-06

   public get_weights_; interface get_weights_
    module procedure get_weights_and_t2_roots
  end interface

contains

   subroutine create(self,nroots)
    type(rys_type) :: self
    ! Create the Rys root object with optional "nroots" to specify the number of
    ! roots.
     pointer :: self
     integer(kind=kind(1)), intent(in), optional :: nroots

     nullify(self)
     allocate(self)

     call nullify_ptr_part_(self)
     if (present(nroots)) then
       self%nroots=nroots
       call create_(self%w,nroots)
       call create_(self%r,nroots)
     end if

   end subroutine

   subroutine destroy(self)
    type(rys_type) :: self
    ! Destroy the Rys root object with "nroots"
     pointer :: self

     if (.not. associated(self)) then;   return; end if
     call destroy_ptr_part_(self)

     deallocate(self)

   end subroutine

   subroutine nullify_ptr_part(self)
    type(rys_type) :: self
    ! Nullify the pointer parts of self

     nullify(self%w)
     nullify(self%r)

   end subroutine

   subroutine destroy_ptr_part(self)
    type(rys_type) :: self
    ! Destroy the pointer parts of self

     call destroy_(self%w)
     call destroy_(self%r)

   end subroutine

!   created result(res)
!   ! Returns true if self has been created
!     self :: pointer
!     res :: logical(kind=kind(.true.))
!     res = associated(self)
!   end

!   destroyed result(res)
!   ! Returns true if self has *not* been created
!     self :: pointer
!     res :: logical(kind=kind(.true.))
!     res = .not. associated(self)
!   end

   subroutine create_copy(self,rys)
    type(rys_type) :: self
    ! Create a copy of "rys"
      pointer :: self
      type(rys_type) :: rys

      call create_(self)
      call copy_(self,rys)

   end subroutine

   subroutine copy(self,rys)
    type(rys_type) :: self
    ! Make a copy of "rys"
      type(rys_type) :: rys

      self = rys
      if (associated(rys%r)) call create_copy_(self%r,rys%r)
      if (associated(rys%w)) call create_copy_(self%w,rys%w)

   end subroutine

   pure function weights(self) result(res)
    type(rys_type) :: self
    ! Return the Rys weights
     intent(in) :: self
     real(kind=kind(1.0d0)), dimension(size(self%w)) :: res
     res=self%w

   end function

   pure function weight(self,i) result(res)
    type(rys_type) :: self
    ! Return the i-th Rys weights
     intent(in) :: self
     real(kind=kind(1.0d0)) :: res
      integer(kind=kind(1)), intent(in) :: i
     res=self%w(i)

   end function

   pure function roots(self) result(res)
    type(rys_type) :: self
    ! Return the Rys roots
     intent(in) :: self
     real(kind=kind(1.0d0)), dimension(size(self%r)) :: res
     res=self%r

   end function

   pure function root(self,i) result(res)
    type(rys_type) :: self
    ! Return the i-th Rys roots
     intent(in) :: self
     real(kind=kind(1.0d0)) :: res
      integer(kind=kind(1)), intent(in) :: i
     res=self%r(i)

   end function

   subroutine get_weights_and_t2_roots(self,X)
    type(rys_type) :: self
    ! Return the weights and t2 roots
     intent(inout) :: self
     real(kind=kind(1.0d0)), intent(in) :: X
     real(kind=kind(1.0d0)) :: rr
     integer(kind=kind(1)) :: i

     call ensure_(tonto,X>=0.0d0,"RYS:get_weights_and_t2_roots ... Rys X must be positive")
     select case ( self%nroots )
       case (1)
         call get_weights1_t2_(self,X)
       case (2)
         call get_weights2_t2_(self,X)
       case (3)
         call get_weights3_t2_(self,X)
       case (4)
         call get_weights4_(self,X)
         rr = self%r(1);   self%r(1)=rr/(rr+1.0d0)
         rr = self%r(2);   self%r(2)=rr/(rr+1.0d0)
         rr = self%r(3);   self%r(3)=rr/(rr+1.0d0)
         rr = self%r(4);   self%r(4)=rr/(rr+1.0d0)
       case (5)
         call get_weights5_(self,X)
         rr = self%r(1);   self%r(1)=rr/(rr+1.0d0)
         rr = self%r(2);   self%r(2)=rr/(rr+1.0d0)
         rr = self%r(3);   self%r(3)=rr/(rr+1.0d0)
         rr = self%r(4);   self%r(4)=rr/(rr+1.0d0)
         rr = self%r(5);   self%r(5)=rr/(rr+1.0d0)
       case default
         call get_weights6_(self,X)
         do i=1,self%nroots
           rr = self%r(i)
           self%r(i) = rr/(rr+1.0d0)
         end do
     end select

   end subroutine

   subroutine get_weights_and_u_roots(self,X)
    type(rys_type) :: self
    ! Return the weights and U roots
     intent(inout) :: self
     real(kind=kind(1.0d0)), intent(in) :: X

     call ensure_(tonto,X>=0.0d0,"RYS:get_weights_and_u_roots ... Rys X must be positive")
     select case ( self%nroots )
       case (1)
         call get_weights1_(self,X)
       case (2)
         call get_weights2_(self,X)
       case (3)
         call get_weights3_(self,X)
       case (4)
         call get_weights4_(self,X)
       case (5)
         call get_weights5_(self,X)
       case default
         call get_weights6_(self,X)
     end select

   end subroutine

   pure subroutine get_weights1(self,x)
    type(rys_type) :: self
    ! For when have one root.
     intent(inout) :: self
     real(kind=kind(1.0d0)), intent(in) :: x
     real(kind=kind(1.0d0)) :: y,e,f,w1

     if (x < 5.0d0) then
       if (x < 3.0d-07) then
         self%r(1)=0.5d+00-x/5.0d+00
         self%w(1)=1.0d0-x*0.33333333333333333333333d0
       else if (x < 1.0d0) then
         f=((((((((-8.36313918003957d-08*x+1.21222603512827d-06 )*x- &
               1.15662609053481d-05 )*x+9.25197374512647d-05 )*x- &
               6.40994113129432d-04 )*x+3.78787044215009d-03 )*x- &
               1.85185172458485d-02 )*x+7.14285713298222d-02 )*x- &
               1.99999999997023d-01 )*x+3.33333333333318d-01
         w1   =(x+x)*f+exp(-x)
         self%w(1)=w1
         self%r(1)=f/(w1-f)
       else if (x < 3.0d0) then
         y = x-2.0d+00
         f=((((((((((-1.61702782425558d-10*y+1.96215250865776d-09 )*y- &
               2.14234468198419d-08 )*y+2.17216556336318d-07 )*y- &
               1.98850171329371d-06 )*y+1.62429321438911d-05 )*y- &
               1.16740298039895d-04 )*y+7.24888732052332d-04 )*y- &
               3.79490003707156d-03 )*y+1.61723488664661d-02 )*y- &
               5.29428148329736d-02 )*y+1.15702180856167d-01
         w1   =(x+x)*f+exp(-x)
         self%w(1)=w1
         self%r(1)=f/(w1-f)
       else
         y=x-4.0d+00
         f=((((((((((-2.62453564772299d-11*y+3.24031041623823d-10 )*y- &
               3.614965656163d-09)*y+3.760256799971d-08)*y- &
               3.553558319675d-07)*y+3.022556449731d-06)*y- &
               2.290098979647d-05)*y+1.526537461148d-04)*y- &
               8.81947375894379d-04 )*y+4.33207949514611d-03 )*y- &
               1.75257821619926d-02 )*y+5.28406320615584d-02
         w1   = (x+x)*f+exp(-x)
         self%w(1)=w1
         self%r(1)=f/(w1-f)
       end if
     else
       if (x < 10.0d0) then
         e=exp(-x)
         w1=(((((( 4.6897511375022d-01/x-6.9955602298985d-01)/x + &
                 5.3689283271887d-01)/x-3.2883030418398d-01)/x + &
                 2.4645596956002d-01)/x-4.9984072848436d-01)/x - &
                 3.1501078774085d-06)*e + sqrt_pie4/sqrt(x)
         f=(w1-e)/(x+x)
         self%w(1)=w1
         self%r(1)=f/(w1-f)
       else if (x < 15.0d0) then
         e=exp(-x)
         w1=(((-1.8784686463512d-01/x+2.2991849164985d-01)/x - &
                 4.9893752514047d-01)/x-2.1916512131607d-05)*e + sqrt_pie4/sqrt(x)
         f=(w1-e)/(x+x)
         self%w(1)=w1
         self%r(1)=f/(w1-f)
       else if (x < 33.0d0) then
         e=exp(-x)
         w1=(( 1.9623264149430d-01/x-4.9695241464490d-01)/x - &
                   6.0156581186481d-05)*e + sqrt_pie4/sqrt(x)
         f=(w1-e)/(x+x)
         self%w(1)=w1
         self%r(1)=f/(w1-f)
       else
         self%w(1)=sqrt_pie4/sqrt(x)
         self%r(1)=0.5d+00/(x-0.5d+00)
       end if
     end if

   end subroutine

   pure subroutine get_weights1_t2(self,x)
    type(rys_type) :: self
    ! For when have one root.
     intent(inout) :: self
     real(kind=kind(1.0d0)), intent(in) :: x
     real(kind=kind(1.0d0)) :: y,e,f,w1

     if (x < 3.0d-07) then
       self%r(1)=1.0d0+10.0d0/(x+x-15d0)
       self%w(1)=1.0d0-x*0.33333333333333333333333d0
     else if (x < 5.0d0) then
       if (x < 1.0d0) then
         f=((((((((-8.36313918003957d-08*x+1.21222603512827d-06 )*x- &
               1.15662609053481d-05 )*x+9.25197374512647d-05 )*x- &
               6.40994113129432d-04 )*x+3.78787044215009d-03 )*x- &
               1.85185172458485d-02 )*x+7.14285713298222d-02 )*x- &
               1.99999999997023d-01 )*x+3.33333333333318d-01
       else if (x < 3.0d0) then
         y = x-2.0d+00
         f=((((((((((-1.61702782425558d-10*y+1.96215250865776d-09 )*y- &
               2.14234468198419d-08 )*y+2.17216556336318d-07 )*y- &
               1.98850171329371d-06 )*y+1.62429321438911d-05 )*y- &
               1.16740298039895d-04 )*y+7.24888732052332d-04 )*y- &
               3.79490003707156d-03 )*y+1.61723488664661d-02 )*y- &
               5.29428148329736d-02 )*y+1.15702180856167d-01
       else
         y=x-4.0d+00
         f=((((((((((-2.62453564772299d-11*y+3.24031041623823d-10 )*y- &
               3.614965656163d-09)*y+3.760256799971d-08)*y- &
               3.553558319675d-07)*y+3.022556449731d-06)*y- &
               2.290098979647d-05)*y+1.526537461148d-04)*y- &
               8.81947375894379d-04 )*y+4.33207949514611d-03 )*y- &
               1.75257821619926d-02 )*y+5.28406320615584d-02
       end if
       w1   = (x+x)*f+exp(-x)
       self%w(1)=w1
       self%r(1)=f/w1
     else if (x < 33.0d0) then
       e=exp(-x)
       if (x < 10.0d0) then
         w1=(((((( 4.6897511375022d-01/x-6.9955602298985d-01)/x + &
                 5.3689283271887d-01)/x-3.2883030418398d-01)/x + &
                 2.4645596956002d-01)/x-4.9984072848436d-01)/x - &
                 3.1501078774085d-06)*e + sqrt_pie4/sqrt(x)
       else if (x < 15.0d0) then
         w1=(((-1.8784686463512d-01/x+2.2991849164985d-01)/x - &
                 4.9893752514047d-01)/x-2.1916512131607d-05)*e + sqrt_pie4/sqrt(x)
       else
         w1=(( 1.9623264149430d-01/x-4.9695241464490d-01)/x - &
                   6.0156581186481d-05)*e + sqrt_pie4/sqrt(x)
       end if
       f=(w1-e)/(x+x)
       self%w(1)=w1
       self%r(1)=f/w1
     else
       self%w(1)=sqrt_pie4/sqrt(x)
       self%r(1)=0.50d0/x
     end if

   end subroutine

   pure subroutine get_weights2(self,x)
    type(rys_type) :: self
    ! For two roots
     intent(inout) :: self
     real(kind=kind(1.0d0)), intent(in) :: x
     real(kind=kind(1.0d0)) :: y,e,f,r2,r1,w1,w2

     if (x < 3.0d-07) then
       self%r(1)=1.30693606237085d-01 -2.90430236082028d-02 *x
       self%r(2)=2.86930639376291d+00 -6.37623643058102d-01 *x
       self%w(1)=6.52145154862545d-01 -1.22713621927067d-01 *x
       self%w(2)=3.47854845137453d-01 -2.10619711404725d-01 *x
     else if (x < 1.0d0) then
       f=((((((((-8.36313918003957d-08*x+1.21222603512827d-06 )*x- &
             1.15662609053481d-05 )*x+9.25197374512647d-05 )*x- &
             6.40994113129432d-04 )*x+3.78787044215009d-03 )*x- &
             1.85185172458485d-02 )*x+7.14285713298222d-02 )*x- &
             1.99999999997023d-01 )*x+3.33333333333318d-01
       w1=(x+x)*f+exp(-x)
       r1=(((((((-2.35234358048491d-09*x+2.49173650389842d-08)*x- &
           4.558315364581d-08)*x-2.447252174587d-06)*x+ &
           4.743292959463d-05)*x-5.33184749432408d-04 )*x+ &
           4.44654947116579d-03 )*x-2.90430236084697d-02 )*x+ &
           1.30693606237085d-01
       r2=(((((((-2.47404902329170d-08*x+2.36809910635906d-07)*x+ &
           1.835367736310d-06)*x-2.066168802076d-05)*x- &
           1.345693393936d-04)*x-5.88154362858038d-05 )*x+ &
           5.32735082098139d-02 )*x-6.37623643056745d-01 )*x+ &
           2.86930639376289d+00
       w2=((f-w1)*r1+f)*(1.0d0+r2)/(r2-r1)
       self%r(1)=r1
       self%r(2)=r2
       self%w(1)=w1-w2
       self%w(2)=w2
     else if (x < 3.0d0) then
       y = x-2.0d+00
       f=((((((((((-1.61702782425558d-10*y+1.96215250865776d-09)*y- &
             2.14234468198419d-08)*y+2.17216556336318d-07)*y- &
             1.98850171329371d-06)*y+1.62429321438911d-05)*y- &
             1.16740298039895d-04)*y+7.24888732052332d-04)*y- &
             3.79490003707156d-03)*y+1.61723488664661d-02)*y- &
             5.29428148329736d-02)*y+1.15702180856167d-01
       w1=(x+x)*f+exp(-x)
       r1=(((((((((-6.36859636616415d-12*y+8.47417064776270d-11)*y- &
           5.152207846962d-10)*y-3.846389873308d-10)*y+ &
           8.472253388380d-08)*y-1.85306035634293d-06)*y+ &
           2.47191693238413d-05)*y-2.49018321709815d-04)*y+ &
           2.19173220020161d-03)*y-1.63329339286794d-02)*y+ &
           8.68085688285261d-02
       r2=((((((((( 1.45331350488343d-10*y+2.07111465297976d-09)*y- &
           1.878920917404d-08)*y-1.725838516261d-07)*y+ &
           2.247389642339d-06)*y+9.76783813082564d-06)*y- &
           1.93160765581969d-04)*y-1.58064140671893d-03)*y+ &
           4.85928174507904d-02)*y-4.30761584997596d-01)*y+ &
           1.80400974537950d+00
       w2=((f-w1)*r1+f)*(1.0d0+r2)/(r2-r1)
       self%r(1)=r1
       self%r(2)=r2
       self%w(1)=w1-w2
       self%w(2)=w2
     else if (x < 5.0d0) then
       y=x-4.0d+00
       f=((((((((((-2.62453564772299d-11*y+3.24031041623823d-10 )*y- &
             3.614965656163d-09)*y+3.760256799971d-08)*y- &
             3.553558319675d-07)*y+3.022556449731d-06)*y- &
             2.290098979647d-05)*y+1.526537461148d-04)*y- &
             8.81947375894379d-04 )*y+4.33207949514611d-03 )*y- &
             1.75257821619926d-02 )*y+5.28406320615584d-02
       w1 = (x+x)*f+exp(-x)
       r1=((((((((-4.11560117487296d-12*y+7.10910223886747d-11)*y- &
           1.73508862390291d-09 )*y+5.93066856324744d-08 )*y- &
           9.76085576741771d-07 )*y+1.08484384385679d-05 )*y- &
           1.12608004981982d-04 )*y+1.16210907653515d-03 )*y- &
           9.89572595720351d-03 )*y+6.12589701086408d-02
       r2=(((((((((-1.80555625241001d-10*y+5.44072475994123d-10)*y+ &
           1.603498045240d-08)*y-1.497986283037d-07)*y- &
           7.017002532106d-07)*y+1.85882653064034d-05 )*y- &
           2.04685420150802d-05 )*y-2.49327728643089d-03 )*y+ &
           3.56550690684281d-02 )*y-2.60417417692375d-01 )*y+ &
           1.12155283108289d+00
       w2=((f-w1)*r1+f)*(1.0d0+r2)/(r2-r1)
       self%r(1)=r1
       self%r(2)=r2
       self%w(1)=w1-w2
       self%w(2)=w2
     else if (x < 10.0d0) then
       e=exp(-x)
       w1=(((((( 4.6897511375022d-01/x-6.9955602298985d-01)/x + &
               5.3689283271887d-01)/x-3.2883030418398d-01)/x + &
               2.4645596956002d-01)/x-4.9984072848436d-01)/x - &
               3.1501078774085d-06)*e + sqrt_pie4/sqrt(x)
       f=(w1-e)/(x+x)
       y=x-7.5d+00
       r1=(((((((((((((-1.43632730148572d-16*y+2.38198922570405d-16)* &
             y+1.358319618800d-14)*y-7.064522786879d-14)*y- &
             7.719300212748d-13)*y+7.802544789997d-12)*y+ &
             6.628721099436d-11)*y-1.775564159743d-09)*y+ &
             1.713828823990d-08)*y-1.497500187053d-07)*y+ &
             2.283485114279d-06)*y-3.76953869614706d-05 )*y+ &
             4.74791204651451d-04 )*y-4.60448960876139d-03 )*y+ &
             3.72458587837249d-02
       r2=(((((((((((( 2.48791622798900d-14*y-1.36113510175724d-13)*y- &
             2.224334349799d-12)*y+4.190559455515d-11)*y- &
             2.222722579924d-10)*y-2.624183464275d-09)*y+ &
             6.128153450169d-08)*y-4.383376014528d-07)*y- &
             2.49952200232910d-06 )*y+1.03236647888320d-04 )*y- &
             1.44614664924989d-03 )*y+1.35094294917224d-02 )*y- &
             9.53478510453887d-02 )*y+5.44765245686790d-01
       w2=((f-w1)*r1+f)*(1.0d0+r2)/(r2-r1)
       self%r(1)=r1
       self%r(2)=r2
       self%w(1)=w1-w2
       self%w(2)=w2
     else if (x < 15.0d0) then
       e=exp(-x)
       w1=(((-1.8784686463512d-01/x+2.2991849164985d-01)/x - &
                 4.9893752514047d-01)/x-2.1916512131607d-05)*e + sqrt_pie4/sqrt(x)
       f=(w1-e)/(x+x)
       r1=((((-1.01041157064226d-05*x+1.19483054115173d-03)*x - &
             6.73760231824074d-02)*x+1.25705571069895d+00)*x + (((- &
             8.57609422987199d+03/x+5.91005939591842d+03)/x - &
             1.70807677109425d+03)/x+2.64536689959503d+02)/x - &
             2.38570496490846d+01)*e + r12/(x-r12)
       r2=((( 3.39024225137123d-04*x-9.34976436343509d-02)*x - &
             4.22216483306320d+00)*x + (((-2.08457050986847d+03/x - &
             1.04999071905664d+03)/x+3.39891508992661d+02)/x - &
             1.56184800325063d+02)/x+8.00839033297501d+00)*e + r22/(x-r22)
       w2=((f-w1)*r1+f)*(1.0d0+r2)/(r2-r1)
       self%r(1)=r1
       self%r(2)=r2
       self%w(1)=w1-w2
       self%w(2)=w2
     else if (x < 33.0d0) then
       e=exp(-x)
       w1=(( 1.9623264149430d-01/x-4.9695241464490d-01)/x - &
                 6.0156581186481d-05)*e + sqrt_pie4/sqrt(x)
       f=(w1-e)/(x+x)
       r1=((((-1.14906395546354d-06*x+1.76003409708332d-04)*x - &
             1.71984023644904d-02)*x-1.37292644149838d-01)*x + (- &
             4.75742064274859d+01/x+9.21005186542857d+00)/x - &
             2.31080873898939d-02)*e + r12/(x-r12)
       r2=((( 3.64921633404158d-04*x-9.71850973831558d-02)*x - &
             4.02886174850252d+00)*x + (-1.35831002139173d+02/x - &
             8.66891724287962d+01)/x+2.98011277766958d+00)*e + r22/(x-r22)
       w2=((f-w1)*r1+f)*(1.0d0+r2)/(r2-r1)
       self%r(1)=r1
       self%r(2)=r2
       self%w(1)=w1-w2
       self%w(2)=w2
     else if (x < 40.0d0) then
       e=exp(-x)
       w1=sqrt_pie4/sqrt(x)
       w2=( 4.46857389308400d+00*x-7.79250653461045d+01)*e + w22*w1
       self%r(1)=(-8.78947307498880d-01*x+1.09243702330261d+01)*e + r12/(x-r12)
       self%r(2)=(-9.28903924275977d+00*x+8.10642367843811d+01)*e + r22/(x-r22)
       self%w(1)=w1-w2
       self%w(2)=w2
     else
       self%r(1)=r12/(x-r12)
       self%r(2)=r22/(x-r22)
       w1=sqrt_pie4/sqrt(x)
       w2=w22*w1
       self%w(1)=w1-w2
       self%w(2)=w2
     end if

   end subroutine

   pure subroutine get_weights2_t2(self,x)
    type(rys_type) :: self
    ! For two roots
     intent(inout) :: self
     real(kind=kind(1.0d0)), intent(in) :: x
     real(kind=kind(1.0d0)) :: y,e,f,r2,r1,w1,w2

     if (x < 3.0d-07) then
       r1=1.30693606237085d-01 -2.90430236082028d-02 *x
       r2=2.86930639376291d+00 -6.37623643058102d-01 *x
       self%r(1)=r1/(r1+1.0d0)
       self%r(2)=r2/(r2+1.0d0)
       self%w(1)=6.52145154862545d-01 -1.22713621927067d-01 *x
       self%w(2)=3.47854845137453d-01 -2.10619711404725d-01 *x
     else if (x < 40.0d0) then
       if (x < 5.0d0) then
         if (x < 1.0d0) then
           f=((((((((-8.36313918003957d-08*x+1.21222603512827d-06 )*x- &
                 1.15662609053481d-05 )*x+9.25197374512647d-05 )*x- &
                 6.40994113129432d-04 )*x+3.78787044215009d-03 )*x- &
                 1.85185172458485d-02 )*x+7.14285713298222d-02 )*x- &
                 1.99999999997023d-01 )*x+3.33333333333318d-01
           r1=(((((((-2.35234358048491d-09*x+2.49173650389842d-08)*x- &
               4.558315364581d-08)*x-2.447252174587d-06)*x+ &
               4.743292959463d-05)*x-5.33184749432408d-04 )*x+ &
               4.44654947116579d-03 )*x-2.90430236084697d-02 )*x+ &
               1.30693606237085d-01
           r2=(((((((-2.47404902329170d-08*x+2.36809910635906d-07)*x+ &
               1.835367736310d-06)*x-2.066168802076d-05)*x- &
               1.345693393936d-04)*x-5.88154362858038d-05 )*x+ &
               5.32735082098139d-02 )*x-6.37623643056745d-01 )*x+ &
               2.86930639376289d+00
         else if (x < 3.0d0) then
           y = x-2.0d+00
           f=((((((((((-1.61702782425558d-10*y+1.96215250865776d-09)*y- &
                 2.14234468198419d-08)*y+2.17216556336318d-07)*y- &
                 1.98850171329371d-06)*y+1.62429321438911d-05)*y- &
                 1.16740298039895d-04)*y+7.24888732052332d-04)*y- &
                 3.79490003707156d-03)*y+1.61723488664661d-02)*y- &
                 5.29428148329736d-02)*y+1.15702180856167d-01
           r1=(((((((((-6.36859636616415d-12*y+8.47417064776270d-11)*y- &
               5.152207846962d-10)*y-3.846389873308d-10)*y+ &
               8.472253388380d-08)*y-1.85306035634293d-06)*y+ &
               2.47191693238413d-05)*y-2.49018321709815d-04)*y+ &
               2.19173220020161d-03)*y-1.63329339286794d-02)*y+ &
               8.68085688285261d-02
           r2=((((((((( 1.45331350488343d-10*y+2.07111465297976d-09)*y- &
               1.878920917404d-08)*y-1.725838516261d-07)*y+ &
               2.247389642339d-06)*y+9.76783813082564d-06)*y- &
               1.93160765581969d-04)*y-1.58064140671893d-03)*y+ &
               4.85928174507904d-02)*y-4.30761584997596d-01)*y+ &
               1.80400974537950d+00
         else
           y=x-4.0d+00
           f=((((((((((-2.62453564772299d-11*y+3.24031041623823d-10 )*y- &
                 3.614965656163d-09)*y+3.760256799971d-08)*y- &
                 3.553558319675d-07)*y+3.022556449731d-06)*y- &
                 2.290098979647d-05)*y+1.526537461148d-04)*y- &
                 8.81947375894379d-04 )*y+4.33207949514611d-03 )*y- &
                 1.75257821619926d-02 )*y+5.28406320615584d-02
           r1=((((((((-4.11560117487296d-12*y+7.10910223886747d-11)*y- &
               1.73508862390291d-09 )*y+5.93066856324744d-08 )*y- &
               9.76085576741771d-07 )*y+1.08484384385679d-05 )*y- &
               1.12608004981982d-04 )*y+1.16210907653515d-03 )*y- &
               9.89572595720351d-03 )*y+6.12589701086408d-02
           r2=(((((((((-1.80555625241001d-10*y+5.44072475994123d-10)*y+ &
               1.603498045240d-08)*y-1.497986283037d-07)*y- &
               7.017002532106d-07)*y+1.85882653064034d-05 )*y- &
               2.04685420150802d-05 )*y-2.49327728643089d-03 )*y+ &
               3.56550690684281d-02 )*y-2.60417417692375d-01 )*y+ &
               1.12155283108289d+00
         end if
         w1 = (x+x)*f+exp(-x)
         w2=((f-w1)*r1+f)*(1.0d0+r2)/(r2-r1)
       else if (x < 33.0d0) then
         e=exp(-x)
         if (x < 10.0d0) then
           w1=(((((( 4.6897511375022d-01/x-6.9955602298985d-01)/x + &
                   5.3689283271887d-01)/x-3.2883030418398d-01)/x + &
                   2.4645596956002d-01)/x-4.9984072848436d-01)/x - &
                   3.1501078774085d-06)*e + sqrt_pie4/sqrt(x)
           y=x-7.5d+00
           r1=(((((((((((((-1.43632730148572d-16*y+2.38198922570405d-16)* &
                 y+1.358319618800d-14)*y-7.064522786879d-14)*y- &
                 7.719300212748d-13)*y+7.802544789997d-12)*y+ &
                 6.628721099436d-11)*y-1.775564159743d-09)*y+ &
                 1.713828823990d-08)*y-1.497500187053d-07)*y+ &
                 2.283485114279d-06)*y-3.76953869614706d-05 )*y+ &
                 4.74791204651451d-04 )*y-4.60448960876139d-03 )*y+ &
                 3.72458587837249d-02
           r2=(((((((((((( 2.48791622798900d-14*y-1.36113510175724d-13)*y- &
                 2.224334349799d-12)*y+4.190559455515d-11)*y- &
                 2.222722579924d-10)*y-2.624183464275d-09)*y+ &
                 6.128153450169d-08)*y-4.383376014528d-07)*y- &
                 2.49952200232910d-06 )*y+1.03236647888320d-04 )*y- &
                 1.44614664924989d-03 )*y+1.35094294917224d-02 )*y- &
                 9.53478510453887d-02 )*y+5.44765245686790d-01
         else if (x < 15.0d0) then
           w1=(((-1.8784686463512d-01/x+2.2991849164985d-01)/x - &
                     4.9893752514047d-01)/x-2.1916512131607d-05)*e + sqrt_pie4/sqrt(x)
           r1=((((-1.01041157064226d-05*x+1.19483054115173d-03)*x - &
                 6.73760231824074d-02)*x+1.25705571069895d+00)*x + (((- &
                 8.57609422987199d+03/x+5.91005939591842d+03)/x - &
                 1.70807677109425d+03)/x+2.64536689959503d+02)/x - &
                 2.38570496490846d+01)*e + r12/(x-r12)
           r2=((( 3.39024225137123d-04*x-9.34976436343509d-02)*x - &
                 4.22216483306320d+00)*x + (((-2.08457050986847d+03/x - &
                 1.04999071905664d+03)/x+3.39891508992661d+02)/x - &
                 1.56184800325063d+02)/x+8.00839033297501d+00)*e + r22/(x-r22)
         else
           w1=(( 1.9623264149430d-01/x-4.9695241464490d-01)/x - &
                     6.0156581186481d-05)*e + sqrt_pie4/sqrt(x)
           r1=((((-1.14906395546354d-06*x+1.76003409708332d-04)*x - &
                 1.71984023644904d-02)*x-1.37292644149838d-01)*x + (- &
                 4.75742064274859d+01/x+9.21005186542857d+00)/x - &
                 2.31080873898939d-02)*e + r12/(x-r12)
           r2=((( 3.64921633404158d-04*x-9.71850973831558d-02)*x - &
                 4.02886174850252d+00)*x + (-1.35831002139173d+02/x - &
                 8.66891724287962d+01)/x+2.98011277766958d+00)*e + r22/(x-r22)
         end if
         f=(w1-e)/(x+x)
         w2=((f-w1)*r1+f)*(1.0d0+r2)/(r2-r1)
       else
         e=exp(-x)
         w1=sqrt_pie4/sqrt(x)
         w2=( 4.46857389308400d+00*x-7.79250653461045d+01)*e + w22*w1
         r1=(-8.78947307498880d-01*x+1.09243702330261d+01)*e + r12/(x-r12)
         r2=(-9.28903924275977d+00*x+8.10642367843811d+01)*e + r22/(x-r22)
       end if
       self%r(1)=r1/(r1+1.0d0)
       self%r(2)=r2/(r2+1.0d0)
       self%w(1)=w1-w2
       self%w(2)=w2
     else
       self%r(1)=r12/x
       self%r(2)=r22/x
       w1=sqrt_pie4/sqrt(x)
       w2=w22*w1
       self%w(1)=w1-w2
       self%w(2)=w2
     end if

   end subroutine

   pure subroutine get_weights3(self,x)
    type(rys_type) :: self
    ! For when have three roots.
     intent(inout) :: self
     real(kind=kind(1.0d0)), intent(in) :: x
     real(kind=kind(1.0d0)) :: y,e, f1, f2, a1, a2, t1, t2, t3

     if (x < 5.0d0) then
       if (x < 3.0d-07) then
         self%r(1)=6.03769246832797d-02 -9.28875764357368d-03 *x
         self%r(2)=7.76823355931043d-01 -1.19511285527878d-01 *x
         self%r(3)=6.66279971938567d+00 -1.02504611068957d+00 *x
         self%w(1)=4.67913934572691d-01 -5.64876917232519d-02 *x
         self%w(2)=3.60761573048137d-01 -1.49077186455208d-01 *x
         self%w(3)=1.71324492379169d-01 -1.27768455150979d-01 *x
       else if (x < 1.0d0) then
         self%r(1)=((((((-5.10186691538870d-10*x+2.40134415703450d-08)*x- &
               5.01081057744427d-07 )*x+7.58291285499256d-06 )*x- &
               9.55085533670919d-05 )*x+1.02893039315878d-03 )*x- &
               9.28875764374337d-03 )*x+6.03769246832810d-02
         self%r(2)=((((((-1.29646524960555d-08*x+7.74602292865683d-08)*x+ &
               1.56022811158727d-06 )*x-1.58051990661661d-05 )*x- &
               3.30447806384059d-04 )*x+9.74266885190267d-03 )*x- &
               1.19511285526388d-01 )*x+7.76823355931033d-01
         self%r(3)=((((((-9.28536484109606d-09*x-3.02786290067014d-07)*x- &
               2.50734477064200d-06 )*x-7.32728109752881d-06 )*x+ &
               2.44217481700129d-04 )*x+4.94758452357327d-02 )*x- &
               1.02504611065774d+00 )*x+6.66279971938553d+00
         f2 = ((((((((-7.60911486098850d-08*x+1.09552870123182d-06 )*x- &
               1.03463270693454d-05 )*x+8.16324851790106d-05 )*x- &
               5.55526624875562d-04 )*x+3.20512054753924d-03 )*x- &
               1.51515139838540d-02 )*x+5.55555554649585d-02 )*x- &
               1.42857142854412d-01 )*x+1.99999999999986d-01
         e=exp(-x)
         f1=((x+x)*f2+e)*0.33333333333333333333333d0
         self%w(1)=(x+x)*f1+e
         t1=self%r(1)/(self%r(1)+1.0d0)
         t2=self%r(2)/(self%r(2)+1.0d0)
         t3=self%r(3)/(self%r(3)+1.0d0)
         a2=f2-t1*f1
         a1=f1-t1*self%w(1)
        self%w(3)=(a2-t2*a1)/((t3-t2)*(t3-t1))
        self%w(2)=(t3*a1-a2)/((t3-t2)*(t2-t1))
        self%w(1)=self%w(1)-self%w(2)-self%w(3)
       else if (x < 3.0d0) then
         y = x-2.0d+00
         self%r(1)=(((((((( 1.44687969563318d-12*y+4.85300143926755d-12)*y- &
             6.55098264095516d-10 )*y+1.56592951656828d-08 )*y- &
             2.60122498274734d-07 )*y+3.86118485517386d-06 )*y- &
             5.13430986707889d-05 )*y+6.03194524398109d-04 )*y- &
             6.11219349825090d-03 )*y+4.52578254679079d-02
         self%r(2)=((((((( 6.95964248788138d-10*y-5.35281831445517d-09)*y- &
             6.745205954533d-08)*y+1.502366784525d-06)*y+ &
             9.923326947376d-07)*y-3.89147469249594d-04 )*y+ &
             7.51549330892401d-03 )*y-8.48778120363400d-02 )*y+ &
             5.73928229597613d-01
         self%r(3)=((((((((-2.81496588401439d-10*y+3.61058041895031d-09)*y+ &
             4.53631789436255d-08 )*y-1.40971837780847d-07 )*y- &
             6.05865557561067d-06 )*y-5.15964042227127d-05 )*y+ &
             3.34761560498171d-05 )*y+5.04871005319119d-02 )*y- &
             8.24708946991557d-01 )*y+4.81234667357205d+00
         f2=((((((((((-1.48044231072140d-10*y+1.78157031325097d-09 )*y- &
             1.92514145088973d-08 )*y+1.92804632038796d-07 )*y- &
             1.73806555021045d-06 )*y+1.39195169625425d-05 )*y- &
             9.74574633246452d-05 )*y+5.83701488646511d-04 )*y- &
             2.89955494844975d-03 )*y+1.13847001113810d-02 )*y- &
             3.23446977320647d-02 )*y+5.29428148329709d-02
         e=exp(-x)
         f1=((x+x)*f2+e)*0.33333333333333333333333d0
         self%w(1)=(x+x)*f1+e
         t1=self%r(1)/(self%r(1)+1.0d0)
         t2=self%r(2)/(self%r(2)+1.0d0)
         t3=self%r(3)/(self%r(3)+1.0d0)
         a2=f2-t1*f1
         a1=f1-t1*self%w(1)
         self%w(3) = (a2-t2*a1)/((t3-t2)*(t3-t1))
         self%w(2) = (t3*a1-a2)/((t3-t2)*(t2-t1))
         self%w(1) = self%w(1)-self%w(2)-self%w(3)
       else
         y=x-4.0d+00
         self%r(1)=((((((( 1.44265709189601d-11*y-4.66622033006074d-10)*y+ &
               7.649155832025d-09)*y-1.229940017368d-07)*y+ &
               2.026002142457d-06)*y-2.87048671521677d-05 )*y+ &
               3.70326938096287d-04 )*y-4.21006346373634d-03 )*y+ &
               3.50898470729044d-02
         self%r(2)=((((((((-2.65526039155651d-11*y+1.97549041402552d-10)*y+ &
               2.15971131403034d-09 )*y-7.95045680685193d-08 )*y+ &
               5.15021914287057d-07 )*y+1.11788717230514d-05 )*y- &
               3.33739312603632d-04 )*y+5.30601428208358d-03 )*y- &
               5.93483267268959d-02 )*y+4.31180523260239d-01
         self%r(3)=((((((((-3.92833750584041d-10*y-4.16423229782280d-09)*y+ &
               4.42413039572867d-08 )*y+6.40574545989551d-07 )*y- &
               3.05512456576552d-06 )*y-1.05296443527943d-04 )*y- &
               6.14120969315617d-04 )*y+4.89665802767005d-02 )*y- &
               6.24498381002855d-01 )*y+3.36412312243724d+00
         f2=((((((((((-2.36788772599074d-11*y+2.89147476459092d-10 )*y- &
               3.18111322308846d-09 )*y+3.25336816562485d-08 )*y- &
               3.00873821471489d-07 )*y+2.48749160874431d-06 )*y- &
               1.81353179793672d-05 )*y+1.14504948737066d-04 )*y- &
               6.10614987696677d-04 )*y+2.64584212770942d-03 )*y- &
               8.66415899015349d-03 )*y+1.75257821619922d-02
         e = exp(-x)
         f1 = ((x+x)*f2+e)*0.33333333333333333333333d0
         self%w(1) = (x+x)*f1+e
         t1 = self%r(1)/(self%r(1)+1.0d0)
         t2 = self%r(2)/(self%r(2)+1.0d0)
         t3 = self%r(3)/(self%r(3)+1.0d0)
         a2 = f2-t1*f1
         a1 = f1-t1*self%w(1)
         self%w(3) = (a2-t2*a1)/((t3-t2)*(t3-t1))
         self%w(2) = (t3*a1-a2)/((t3-t2)*(t2-t1))
         self%w(1) = self%w(1)-self%w(2)-self%w(3)
       end if
     else
       if (x < 10.0d0) then
         e=exp(-x)
         self%w(1)=(((((( 4.6897511375022d-01/x-6.9955602298985d-01)/x + &
               5.3689283271887d-01)/x-3.2883030418398d-01)/x + &
               2.4645596956002d-01)/x-4.9984072848436d-01)/x - &
               3.1501078774085d-06)*e + sqrt_pie4/sqrt(x)
         f1=(self%w(1)-e)/(x+x)
         f2=(f1+f1+f1-e)/(x+x)
         y=x-7.5d+00
         self%r(1)=((((((((((( 5.74429401360115d-16*y+7.11884203790984d-16)*y- &
               6.736701449826d-14)*y-6.264613873998d-13)*y+ &
               1.315418927040d-11)*y-4.23879635610964d-11 )*y+ &
               1.39032379769474d-09 )*y-4.65449552856856d-08 )*y+ &
               7.34609900170759d-07 )*y-1.08656008854077d-05 )*y+ &
               1.77930381549953d-04 )*y-2.39864911618015d-03 )*y+ &
               2.39112249488821d-02
         self%r(2)=((((((((((( 1.13464096209120d-14*y+6.99375313934242d-15)*y- &
               8.595618132088d-13)*y-5.293620408757d-12)*y- &
               2.492175211635d-11)*y+2.73681574882729d-09 )*y- &
               1.06656985608482d-08 )*y-4.40252529648056d-07 )*y+ &
               9.68100917793911d-06 )*y-1.68211091755327d-04 )*y+ &
               2.69443611274173d-03 )*y-3.23845035189063d-02 )*y+ &
               2.75969447451882d-01
         self%r(3)=(((((((((((( 6.66339416996191d-15*y+1.84955640200794d-13)*y- &
               1.985141104444d-12)*y-2.309293727603d-11)*y+ &
               3.917984522103d-10)*y+1.663165279876d-09)*y- &
               6.205591993923d-08)*y+8.769581622041d-09)*y+ &
               8.97224398620038d-06 )*y-3.14232666170796d-05 )*y- &
               1.83917335649633d-03 )*y+3.51246831672571d-02 )*y- &
               3.22335051270860d-01 )*y+1.73582831755430d+00
         t1 = self%r(1)/(self%r(1)+1.0d0)
         t2 = self%r(2)/(self%r(2)+1.0d0)
         t3 = self%r(3)/(self%r(3)+1.0d0)
         a2 = f2-t1*f1
         a1 = f1-t1*self%w(1)
         self%w(3) = (a2-t2*a1)/((t3-t2)*(t3-t1))
         self%w(2) = (t3*a1-a2)/((t3-t2)*(t2-t1))
         self%w(1) = self%w(1)-self%w(2)-self%w(3)
       else if (x < 15.0d0) then
         e=exp(-x)
         self%w(1)=(((-1.8784686463512d-01/x+2.2991849164985d-01)/x - &
             4.9893752514047d-01)/x-2.1916512131607d-05)*e + sqrt_pie4/sqrt(x)
         f1=(self%w(1)-e)/(x+x)
         f2=(f1+f1+f1-e)/(x+x)
         y=x-12.5d+00
         self%r(1)=((((((((((( 4.42133001283090d-16*y-2.77189767070441d-15)*y- &
             4.084026087887d-14)*y+5.379885121517d-13)*y+ &
             1.882093066702d-12)*y-8.67286219861085d-11 )*y+ &
             7.11372337079797d-10 )*y-3.55578027040563d-09 )*y+ &
             1.29454702851936d-07 )*y-4.14222202791434d-06 )*y+ &
             8.04427643593792d-05 )*y-1.18587782909876d-03 )*y+ &
             1.53435577063174d-02
         self%r(2)=((((((((((( 6.85146742119357d-15*y-1.08257654410279d-14)*y- &
             8.579165965128d-13)*y+6.642452485783d-12)*y+ &
             4.798806828724d-11)*y-1.13413908163831d-09 )*y+ &
             7.08558457182751d-09 )*y-5.59678576054633d-08 )*y+ &
             2.51020389884249d-06 )*y-6.63678914608681d-05 )*y+ &
             1.11888323089714d-03 )*y-1.45361636398178d-02 )*y+ &
             1.65077877454402d-01
         self%r(3)=(((((((((((( 3.20622388697743d-15*y-2.73458804864628d-14)*y- &
             3.157134329361d-13)*y+8.654129268056d-12)*y- &
             5.625235879301d-11)*y-7.718080513708d-10)*y+ &
             2.064664199164d-08)*y-1.567725007761d-07)*y- &
             1.57938204115055d-06 )*y+6.27436306915967d-05 )*y- &
             1.01308723606946d-03 )*y+1.13901881430697d-02 )*y- &
             1.01449652899450d-01 )*y+7.77203937334739d-01
         t1 = self%r(1)/(self%r(1)+1.0d0)
         t2 = self%r(2)/(self%r(2)+1.0d0)
         t3 = self%r(3)/(self%r(3)+1.0d0)
         a2 = f2-t1*f1
         a1 = f1-t1*self%w(1)
         self%w(3) = (a2-t2*a1)/((t3-t2)*(t3-t1))
         self%w(2) = (t3*a1-a2)/((t3-t2)*(t2-t1))
         self%w(1) = self%w(1)-self%w(2)-self%w(3)
       else if (x < 33.0d0) then
         e=exp(-x)
         self%w(1)=(( 1.9623264149430d-01/x-4.9695241464490d-01)/x - &
           6.0156581186481d-05)*e + sqrt_pie4/sqrt(x)
         f1=(self%w(1)-e)/(x+x)
         f2=(f1+f1+f1-e)/(x+x)
         if (x < 20.0d+00) then
           self%r(1)=((((((-2.43270989903742d-06*x+3.57901398988359d-04)*x - &
               2.34112415981143d-02)*x+7.81425144913975d-01)*x - &
               1.73209218219175d+01)*x+2.43517435690398d+02)*x + (- &
               1.97611541576986d+04/x+9.82441363463929d+03)/x - &
               2.07970687843258d+03)*e + r13/(x-r13)
           self%r(2)=(((((-2.62627010965435d-04*x+3.49187925428138d-02)*x - &
               3.09337618731880d+00)*x+1.07037141010778d+02)*x - &
               2.36659637247087d+03)*x + ((-2.91669113681020d+06/x + &
               1.41129505262758d+06)/x-2.91532335433779d+05)/x + &
               3.35202872835409d+04)*e + r23/(x-r23)
           self%r(3)=((((( 9.31856404738601d-05*x-2.87029400759565d-02)*x - &
               7.83503697918455d-01)*x-1.84338896480695d+01)*x + &
               4.04996712650414d+02)*x + (-1.89829509315154d+05/x + &
               5.11498390849158d+04)/x-6.88145821789955d+03)*e + r33/(x-r33)
         else
           self%r(1)=((((-4.97561537069643d-04*x-5.00929599665316d-02)*x + &
               1.31099142238996d+00)*x-1.88336409225481d+01)*x - &
               6.60344754467191d+02 /x+1.64931462413877d+02)*e + r13/(x-r13)
           self%r(2)=((((-4.48218898474906d-03*x-5.17373211334924d-01)*x + &
               1.13691058739678d+01)*x-1.65426392885291d+02)*x - &
               6.30909125686731d+03 /x+1.52231757709236d+03)*e + r23/(x-r23)
           self%r(3)=((((-1.38368602394293d-02*x-1.77293428863008d+00)*x + &
               1.73639054044562d+01)*x-3.57615122086961d+02)*x - &
               1.45734701095912d+04 /x+2.69831813951849d+03)*e + r33/(x-r33)
         end if
         t1 = self%r(1)/(self%r(1)+1.0d0)
         t2 = self%r(2)/(self%r(2)+1.0d0)
         t3 = self%r(3)/(self%r(3)+1.0d0)
         a2 = f2-t1*f1
         a1 = f1-t1*self%w(1)
         self%w(3) = (a2-t2*a1)/((t3-t2)*(t3-t1))
         self%w(2) = (t3*a1-a2)/((t3-t2)*(t2-t1))
         self%w(1) = self%w(1)-self%w(2)-self%w(3)
       else
         self%w(1)=sqrt_pie4/sqrt(x)
         if (x < 47.0d0) then
           e=exp(-x)
           self%r(1)=((-7.39058467995275d+00*x+3.21318352526305d+02)*x - &
               3.99433696473658d+03)*e + r13/(x-r13)
           self%r(2)=((-7.38726243906513d+01*x+3.13569966333873d+03)*x - &
               3.86862867311321d+04)*e + r23/(x-r23)
           self%r(3)=((-2.63750565461336d+02*x+1.04412168692352d+04)*x - &
               1.28094577915394d+05)*e + r33/(x-r33)
           self%w(3)=((( 1.52258947224714d-01*x-8.30661900042651d+00)*x + &
               1.92977367967984d+02)*x-1.67787926005344d+03)*e + w33*self%w(1)
           self%w(2)=(( 6.15072615497811d+01*x-2.91980647450269d+03)*x + &
               3.80794303087338d+04)*e + w23*self%w(1)
           self%w(1)=self%w(1)-self%w(2)-self%w(3)
         else
           self%r(1)=r13/(x-r13)
           self%r(2)=r23/(x-r23)
           self%r(3)=r33/(x-r33)
           self%w(2)=w23*self%w(1)
           self%w(3)=w33*self%w(1)
           self%w(1)=self%w(1)-self%w(2)-self%w(3)
         end if
       end if
     end if

   end subroutine

   pure subroutine get_weights3_t2(self,x)
    type(rys_type) :: self
    ! For when have three roots.
     intent(inout) :: self
     real(kind=kind(1.0d0)), intent(in) :: x
     real(kind=kind(1.0d0)) :: r1,r2,r3,w1,y,e, f1, f2, a1, a2, t1, t2, t3

     if (x < 3.0d-07) then
       r1=6.03769246832797d-02 -9.28875764357368d-03 *x
       r2=7.76823355931043d-01 -1.19511285527878d-01 *x
       r3=6.66279971938567d+00 -1.02504611068957d+00 *x
       self%r(1)=r1/(r1+1.0d0)
       self%r(2)=r2/(r2+1.0d0)
       self%r(3)=r3/(r3+1.0d0)
       self%w(1)=4.67913934572691d-01 -5.64876917232519d-02 *x
       self%w(2)=3.60761573048137d-01 -1.49077186455208d-01 *x
       self%w(3)=1.71324492379169d-01 -1.27768455150979d-01 *x
     else if (x < 33.0d0) then
       e=exp(-x)
       if (x < 5.0d0) then
         if (x < 1.0d0) then
           r1=((((((-5.10186691538870d-10*x+2.40134415703450d-08)*x- &
                 5.01081057744427d-07 )*x+7.58291285499256d-06 )*x- &
                 9.55085533670919d-05 )*x+1.02893039315878d-03 )*x- &
                 9.28875764374337d-03 )*x+6.03769246832810d-02
           r2=((((((-1.29646524960555d-08*x+7.74602292865683d-08)*x+ &
                 1.56022811158727d-06 )*x-1.58051990661661d-05 )*x- &
                 3.30447806384059d-04 )*x+9.74266885190267d-03 )*x- &
                 1.19511285526388d-01 )*x+7.76823355931033d-01
           r3=((((((-9.28536484109606d-09*x-3.02786290067014d-07)*x- &
                 2.50734477064200d-06 )*x-7.32728109752881d-06 )*x+ &
                 2.44217481700129d-04 )*x+4.94758452357327d-02 )*x- &
                 1.02504611065774d+00 )*x+6.66279971938553d+00
           f2 = ((((((((-7.60911486098850d-08*x+1.09552870123182d-06 )*x- &
                 1.03463270693454d-05 )*x+8.16324851790106d-05 )*x- &
                 5.55526624875562d-04 )*x+3.20512054753924d-03 )*x- &
                 1.51515139838540d-02 )*x+5.55555554649585d-02 )*x- &
                 1.42857142854412d-01 )*x+1.99999999999986d-01
         else if (x < 3.0d0) then
           y = x-2.0d+00
           r1=(((((((( 1.44687969563318d-12*y+4.85300143926755d-12)*y- &
               6.55098264095516d-10 )*y+1.56592951656828d-08 )*y- &
               2.60122498274734d-07 )*y+3.86118485517386d-06 )*y- &
               5.13430986707889d-05 )*y+6.03194524398109d-04 )*y- &
               6.11219349825090d-03 )*y+4.52578254679079d-02
           r2=((((((( 6.95964248788138d-10*y-5.35281831445517d-09)*y- &
               6.745205954533d-08)*y+1.502366784525d-06)*y+ &
               9.923326947376d-07)*y-3.89147469249594d-04 )*y+ &
               7.51549330892401d-03 )*y-8.48778120363400d-02 )*y+ &
               5.73928229597613d-01
           r3=((((((((-2.81496588401439d-10*y+3.61058041895031d-09)*y+ &
               4.53631789436255d-08 )*y-1.40971837780847d-07 )*y- &
               6.05865557561067d-06 )*y-5.15964042227127d-05 )*y+ &
               3.34761560498171d-05 )*y+5.04871005319119d-02 )*y- &
               8.24708946991557d-01 )*y+4.81234667357205d+00
           f2=((((((((((-1.48044231072140d-10*y+1.78157031325097d-09 )*y- &
               1.92514145088973d-08 )*y+1.92804632038796d-07 )*y- &
               1.73806555021045d-06 )*y+1.39195169625425d-05 )*y- &
               9.74574633246452d-05 )*y+5.83701488646511d-04 )*y- &
               2.89955494844975d-03 )*y+1.13847001113810d-02 )*y- &
               3.23446977320647d-02 )*y+5.29428148329709d-02
         else
           y=x-4.0d+00
           r1=((((((( 1.44265709189601d-11*y-4.66622033006074d-10)*y+ &
                 7.649155832025d-09)*y-1.229940017368d-07)*y+ &
                 2.026002142457d-06)*y-2.87048671521677d-05 )*y+ &
                 3.70326938096287d-04 )*y-4.21006346373634d-03 )*y+ &
                 3.50898470729044d-02
           r2=((((((((-2.65526039155651d-11*y+1.97549041402552d-10)*y+ &
                 2.15971131403034d-09 )*y-7.95045680685193d-08 )*y+ &
                 5.15021914287057d-07 )*y+1.11788717230514d-05 )*y- &
                 3.33739312603632d-04 )*y+5.30601428208358d-03 )*y- &
                 5.93483267268959d-02 )*y+4.31180523260239d-01
           r3=((((((((-3.92833750584041d-10*y-4.16423229782280d-09)*y+ &
                 4.42413039572867d-08 )*y+6.40574545989551d-07 )*y- &
                 3.05512456576552d-06 )*y-1.05296443527943d-04 )*y- &
                 6.14120969315617d-04 )*y+4.89665802767005d-02 )*y- &
                 6.24498381002855d-01 )*y+3.36412312243724d+00
           f2=((((((((((-2.36788772599074d-11*y+2.89147476459092d-10 )*y- &
                 3.18111322308846d-09 )*y+3.25336816562485d-08 )*y- &
                 3.00873821471489d-07 )*y+2.48749160874431d-06 )*y- &
                 1.81353179793672d-05 )*y+1.14504948737066d-04 )*y- &
                 6.10614987696677d-04 )*y+2.64584212770942d-03 )*y- &
                 8.66415899015349d-03 )*y+1.75257821619922d-02
         end if
         f1 = ((x+x)*f2+e)*0.33333333333333333333333d0
         w1 = (x+x)*f1+e
       else
         if (x < 10.0d0) then
           w1=(((((( 4.6897511375022d-01/x-6.9955602298985d-01)/x + &
                 5.3689283271887d-01)/x-3.2883030418398d-01)/x + &
                 2.4645596956002d-01)/x-4.9984072848436d-01)/x - &
                 3.1501078774085d-06)*e + sqrt_pie4/sqrt(x)
           y=x-7.5d+00
           r1=((((((((((( 5.74429401360115d-16*y+7.11884203790984d-16)*y- &
                 6.736701449826d-14)*y-6.264613873998d-13)*y+ &
                 1.315418927040d-11)*y-4.23879635610964d-11 )*y+ &
                 1.39032379769474d-09 )*y-4.65449552856856d-08 )*y+ &
                 7.34609900170759d-07 )*y-1.08656008854077d-05 )*y+ &
                 1.77930381549953d-04 )*y-2.39864911618015d-03 )*y+ &
                 2.39112249488821d-02
           r2=((((((((((( 1.13464096209120d-14*y+6.99375313934242d-15)*y- &
                 8.595618132088d-13)*y-5.293620408757d-12)*y- &
                 2.492175211635d-11)*y+2.73681574882729d-09 )*y- &
                 1.06656985608482d-08 )*y-4.40252529648056d-07 )*y+ &
                 9.68100917793911d-06 )*y-1.68211091755327d-04 )*y+ &
                 2.69443611274173d-03 )*y-3.23845035189063d-02 )*y+ &
                 2.75969447451882d-01
           r3=(((((((((((( 6.66339416996191d-15*y+1.84955640200794d-13)*y- &
                 1.985141104444d-12)*y-2.309293727603d-11)*y+ &
                 3.917984522103d-10)*y+1.663165279876d-09)*y- &
                 6.205591993923d-08)*y+8.769581622041d-09)*y+ &
                 8.97224398620038d-06 )*y-3.14232666170796d-05 )*y- &
                 1.83917335649633d-03 )*y+3.51246831672571d-02 )*y- &
                 3.22335051270860d-01 )*y+1.73582831755430d+00
         else if (x < 15.0d0) then
           w1=(((-1.8784686463512d-01/x+2.2991849164985d-01)/x - &
               4.9893752514047d-01)/x-2.1916512131607d-05)*e + sqrt_pie4/sqrt(x)
           y=x-12.5d+00
           r1=((((((((((( 4.42133001283090d-16*y-2.77189767070441d-15)*y- &
               4.084026087887d-14)*y+5.379885121517d-13)*y+ &
               1.882093066702d-12)*y-8.67286219861085d-11 )*y+ &
               7.11372337079797d-10 )*y-3.55578027040563d-09 )*y+ &
               1.29454702851936d-07 )*y-4.14222202791434d-06 )*y+ &
               8.04427643593792d-05 )*y-1.18587782909876d-03 )*y+ &
               1.53435577063174d-02
           r2=((((((((((( 6.85146742119357d-15*y-1.08257654410279d-14)*y- &
               8.579165965128d-13)*y+6.642452485783d-12)*y+ &
               4.798806828724d-11)*y-1.13413908163831d-09 )*y+ &
               7.08558457182751d-09 )*y-5.59678576054633d-08 )*y+ &
               2.51020389884249d-06 )*y-6.63678914608681d-05 )*y+ &
               1.11888323089714d-03 )*y-1.45361636398178d-02 )*y+ &
               1.65077877454402d-01
           r3=(((((((((((( 3.20622388697743d-15*y-2.73458804864628d-14)*y- &
               3.157134329361d-13)*y+8.654129268056d-12)*y- &
               5.625235879301d-11)*y-7.718080513708d-10)*y+ &
               2.064664199164d-08)*y-1.567725007761d-07)*y- &
               1.57938204115055d-06 )*y+6.27436306915967d-05 )*y- &
               1.01308723606946d-03 )*y+1.13901881430697d-02 )*y- &
               1.01449652899450d-01 )*y+7.77203937334739d-01
         else if (x < 33.0d0) then
           w1=(( 1.9623264149430d-01/x-4.9695241464490d-01)/x - &
             6.0156581186481d-05)*e + sqrt_pie4/sqrt(x)
           if (x < 20.0d+00) then
             r1=((((((-2.43270989903742d-06*x+3.57901398988359d-04)*x - &
                 2.34112415981143d-02)*x+7.81425144913975d-01)*x - &
                 1.73209218219175d+01)*x+2.43517435690398d+02)*x + (- &
                 1.97611541576986d+04/x+9.82441363463929d+03)/x - &
                 2.07970687843258d+03)*e + r13/(x-r13)
             r2=(((((-2.62627010965435d-04*x+3.49187925428138d-02)*x - &
                 3.09337618731880d+00)*x+1.07037141010778d+02)*x - &
                 2.36659637247087d+03)*x + ((-2.91669113681020d+06/x + &
                 1.41129505262758d+06)/x-2.91532335433779d+05)/x + &
                 3.35202872835409d+04)*e + r23/(x-r23)
             r3=((((( 9.31856404738601d-05*x-2.87029400759565d-02)*x - &
                 7.83503697918455d-01)*x-1.84338896480695d+01)*x + &
                 4.04996712650414d+02)*x + (-1.89829509315154d+05/x + &
                 5.11498390849158d+04)/x-6.88145821789955d+03)*e + r33/(x-r33)
           else
             r1=((((-4.97561537069643d-04*x-5.00929599665316d-02)*x + &
                 1.31099142238996d+00)*x-1.88336409225481d+01)*x - &
                 6.60344754467191d+02 /x+1.64931462413877d+02)*e + r13/(x-r13)
             r2=((((-4.48218898474906d-03*x-5.17373211334924d-01)*x + &
                 1.13691058739678d+01)*x-1.65426392885291d+02)*x - &
                 6.30909125686731d+03 /x+1.52231757709236d+03)*e + r23/(x-r23)
             r3=((((-1.38368602394293d-02*x-1.77293428863008d+00)*x + &
                 1.73639054044562d+01)*x-3.57615122086961d+02)*x - &
                 1.45734701095912d+04 /x+2.69831813951849d+03)*e + r33/(x-r33)
           end if
         end if
         f1=(w1-e)/(x+x)
         f2=(f1+f1+f1-e)/(x+x)
       end if
       t1=r1/(r1+1.0d0)
       t2=r2/(r2+1.0d0)
       t3=r3/(r3+1.0d0)
       self%r(1)=t1
       self%r(2)=t2
       self%r(3)=t3
       a2 = f2-t1*f1
       a1 = f1-t1*w1
       self%w(3) = (a2-t2*a1)/((t3-t2)*(t3-t1))
       self%w(2) = (t3*a1-a2)/((t3-t2)*(t2-t1))
       self%w(1) = w1-self%w(2)-self%w(3)
     else if (x < 47.0d0) then
       w1=sqrt_pie4/sqrt(x)
       e=exp(-x)
       r1=((-7.39058467995275d+00*x+3.21318352526305d+02)*x - &
           3.99433696473658d+03)*e + r13/(x-r13)
       r2=((-7.38726243906513d+01*x+3.13569966333873d+03)*x - &
           3.86862867311321d+04)*e + r23/(x-r23)
       r3=((-2.63750565461336d+02*x+1.04412168692352d+04)*x - &
           1.28094577915394d+05)*e + r33/(x-r33)
       self%w(3)=((( 1.52258947224714d-01*x-8.30661900042651d+00)*x + &
           1.92977367967984d+02)*x-1.67787926005344d+03)*e + w33*w1
       self%w(2)=(( 6.15072615497811d+01*x-2.91980647450269d+03)*x + &
           3.80794303087338d+04)*e + w23*w1
       self%w(1)=w1-self%w(2)-self%w(3)
       self%r(1)=r1/(r1+1.0d0)
       self%r(2)=r2/(r2+1.0d0)
       self%r(3)=r3/(r3+1.0d0)
     else
       w1=sqrt_pie4/sqrt(x)
       self%r(1)=r13/x
       self%r(2)=r23/x
       self%r(3)=r33/x
       self%w(2)=w23*w1
       self%w(3)=w33*w1
       self%w(1)=w1-self%w(2)-self%w(3)
     end if

   end subroutine

   pure subroutine get_weights4(self,x)
    type(rys_type) :: self
    ! For four roots
     intent(inout) :: self
     real(kind=kind(1.0d0)), intent(in) :: x
     real(kind=kind(1.0d0)) :: y,e

     if (x < 3.0d-07) then
       self%r(1)=3.48198973061471d-02 -4.09645850660395d-03 *x
       self%r(2)=3.81567185080042d-01 -4.48902570656719d-02 *x
       self%r(3)=1.73730726945891d+00 -2.04389090547327d-01 *x
       self%r(4)=1.18463056481549d+01 -1.39368301742312d+00 *x
       self%w(1)=3.62683783378362d-01 -3.13844305713928d-02 *x
       self%w(2)=3.13706645877886d-01 -8.98046242557724d-02 *x
       self%w(3)=2.22381034453372d-01 -1.29314370958973d-01 *x
       self%w(4)=1.01228536290376d-01 -8.28299075414321d-02 *x
     else if (x < 1.0d0) then
       self%r(1)=((((((-1.95309614628539d-10*x+5.19765728707592d-09)*x- &
                 1.01756452250573d-07 )*x+1.72365935872131d-06 )*x- &
                 2.61203523522184d-05 )*x+3.52921308769880d-04 )*x- &
                 4.09645850658433d-03 )*x+3.48198973061469d-02
       self%r(2)=(((((-1.89554881382342d-08*x+3.07583114342365d-07)*x+ &
                 1.270981734393d-06)*x-1.417298563884d-04)*x+ &
                 3.226979163176d-03)*x-4.48902570678178d-02 )*x+ &
                 3.81567185080039d-01
       self%r(3)=(((((( 1.77280535300416d-09*x+3.36524958870615d-08)*x- &
                 2.58341529013893d-07 )*x-1.13644895662320d-05 )*x- &
                 7.91549618884063d-05 )*x+1.03825827346828d-02 )*x- &
                 2.04389090525137d-01 )*x+1.73730726945889d+00
       self%r(4)=(((((-5.61188882415248d-08*x-2.49480733072460d-07)*x+ &
                 3.428685057114d-06)*x+1.679007454539d-04)*x+ &
                 4.722855585715d-02)*x-1.39368301737828d+00 )*x+ &
                 1.18463056481543d+01
       self%w(1)=((((((-1.14649303201279d-08*x+1.88015570196787d-07)*x- &
                 2.33305875372323d-06 )*x+2.68880044371597d-05 )*x- &
                 2.94268428977387d-04 )*x+3.06548909776613d-03 )*x- &
                 3.13844305680096d-02 )*x+3.62683783378335d-01
       self%w(2)=((((((((-4.11720483772634d-09*x+6.54963481852134d-08)*x- &
                 7.20045285129626d-07 )*x+6.93779646721723d-06 )*x- &
                 6.05367572016373d-05 )*x+4.74241566251899d-04 )*x- &
                 3.26956188125316d-03 )*x+1.91883866626681d-02 )*x- &
                 8.98046242565811d-02 )*x+3.13706645877886d-01
       self%w(3)=((((((((-3.41688436990215d-08*x+5.07238960340773d-07)*x- &
                 5.01675628408220d-06 )*x+4.20363420922845d-05 )*x- &
                 3.08040221166823d-04 )*x+1.94431864731239d-03 )*x- &
                 1.02477820460278d-02 )*x+4.28670143840073d-02 )*x- &
                 1.29314370962569d-01 )*x+2.22381034453369d-01
       self%w(4)=((((((((( 4.99660550769508d-09*x-7.94585963310120d-08)*x+ &
                 8.359072409485d-07)*x-7.422369210610d-06)*x+ &
                 5.763374308160d-05)*x-3.86645606718233d-04 )*x+ &
                 2.18417516259781d-03 )*x-9.99791027771119d-03 )*x+ &
                 3.48791097377370d-02 )*x-8.28299075413889d-02 )*x+ &
                 1.01228536290376d-01
     else if (x < 5.0d+00) then
       y = x-3.0d+00
       self%r(1)=(((((((((-1.48570633747284d-15*y-1.33273068108777d-13)*y+ &
                 4.068543696670d-12)*y-9.163164161821d-11)*y+ &
                 2.046819017845d-09)*y-4.03076426299031d-08 )*y+ &
                 7.29407420660149d-07 )*y-1.23118059980833d-05 )*y+ &
                 1.88796581246938d-04 )*y-2.53262912046853d-03 )*y+ &
                 2.51198234505021d-02
       self%r(2)=((((((((( 1.35830583483312d-13*y-2.29772605964836d-12)*y- &
                 3.821500128045d-12)*y+6.844424214735d-10)*y- &
                 1.048063352259d-08)*y+1.50083186233363d-08 )*y+ &
                 3.48848942324454d-06 )*y-1.08694174399193d-04 )*y+ &
                 2.08048885251999d-03 )*y-2.91205805373793d-02 )*y+ &
                 2.72276489515713d-01
       self%r(3)=((((((((( 5.02799392850289d-13*y+1.07461812944084d-11)*y- &
                 1.482277886411d-10)*y-2.153585661215d-09)*y+ &
                 3.654087802817d-08)*y+5.15929575830120d-07 )*y- &
                 9.52388379435709d-06 )*y-2.16552440036426d-04 )*y+ &
                 9.03551469568320d-03 )*y-1.45505469175613d-01 )*y+ &
                 1.21449092319186d+00
       self%r(4)=(((((((((-1.08510370291979d-12*y+6.41492397277798d-11)*y+ &
                 7.542387436125d-10)*y-2.213111836647d-09)*y- &
                 1.448228963549d-07)*y-1.95670833237101d-06 )*y- &
                 1.07481314670844d-05 )*y+1.49335941252765d-04 )*y+ &
                 4.87791531990593d-02 )*y-1.10559909038653d+00 )*y+ &
                 8.09502028611780d+00
       self%w(1)=((((((((((-4.65801912689961d-14*y+7.58669507106800d-13)*y- &
                 1.186387548048d-11)*y+1.862334710665d-10)*y- &
                 2.799399389539d-09)*y+4.148972684255d-08)*y- &
                 5.933568079600d-07)*y+8.168349266115d-06)*y- &
                 1.08989176177409d-04 )*y+1.41357961729531d-03 )*y- &
                 1.87588361833659d-02 )*y+2.89898651436026d-01
       self%w(2)=((((((((((((-1.46345073267549d-14*y+2.25644205432182d-13)*y- &
                 3.116258693847d-12)*y+4.321908756610d-11)*y- &
                 5.673270062669d-10)*y+7.006295962960d-09)*y- &
                 8.120186517000d-08)*y+8.775294645770d-07)*y- &
                 8.77829235749024d-06 )*y+8.04372147732379d-05 )*y- &
                 6.64149238804153d-04 )*y+4.81181506827225d-03 )*y- &
                 2.88982669486183d-02 )*y+1.56247249979288d-01
       self%w(3)=((((((((((((( 9.06812118895365d-15*y-1.40541322766087d-13)* &
                 y+1.919270015269d-12)*y-2.605135739010d-11)*y+ &
                 3.299685839012d-10)*y-3.86354139348735d-09 )*y+ &
                 4.16265847927498d-08 )*y-4.09462835471470d-07 )*y+ &
                 3.64018881086111d-06 )*y-2.88665153269386d-05 )*y+ &
                 2.00515819789028d-04 )*y-1.18791896897934d-03 )*y+ &
                 5.75223633388589d-03 )*y-2.09400418772687d-02 )*y+ &
                 4.85368861938873d-02
       self%w(4)=((((((((((((((-9.74835552342257d-16*y+1.57857099317175d-14)* &
                 y-2.249993780112d-13)*y+3.173422008953d-12)*y- &
                 4.161159459680d-11)*y+5.021343560166d-10)*y- &
                 5.545047534808d-09)*y+5.554146993491d-08)*y- &
                 4.99048696190133d-07 )*y+3.96650392371311d-06 )*y- &
                 2.73816413291214d-05 )*y+1.60106988333186d-04 )*y- &
                 7.64560567879592d-04 )*y+2.81330044426892d-03 )*y- &
                 7.16227030134947d-03 )*y+9.66077262223353d-03
     else if (x < 10.0d+00) then
       y=x-7.5d+00
       self%r(1)=((((((((( 4.64217329776215d-15*y-6.27892383644164d-15)*y+ &
                 3.462236347446d-13)*y-2.927229355350d-11)*y+ &
                 5.090355371676d-10)*y-9.97272656345253d-09 )*y+ &
                 2.37835295639281d-07 )*y-4.60301761310921d-06 )*y+ &
                 8.42824204233222d-05 )*y-1.37983082233081d-03 )*y+ &
                 1.66630865869375d-02
       self%r(2)=((((((((( 2.93981127919047d-14*y+8.47635639065744d-13)*y- &
                 1.446314544774d-11)*y-6.149155555753d-12)*y+ &
                 8.484275604612d-10)*y-6.10898827887652d-08 )*y+ &
                 2.39156093611106d-06 )*y-5.35837089462592d-05 )*y+ &
                 1.00967602595557d-03 )*y-1.57769317127372d-02 )*y+ &
                 1.74853819464285d-01
       self%r(3)=(((((((((( 2.93523563363000d-14*y-6.40041776667020d-14)*y- &
                 2.695740446312d-12)*y+1.027082960169d-10)*y- &
                 5.822038656780d-10)*y-3.159991002539d-08)*y+ &
                 4.327249251331d-07)*y+4.856768455119d-06)*y- &
                 2.54617989427762d-04 )*y+5.54843378106589d-03 )*y- &
                 7.95013029486684d-02 )*y+7.20206142703162d-01
       self%r(4)=(((((((((((-1.62212382394553d-14*y+7.68943641360593d-13)*y+ &
                 5.764015756615d-12)*y-1.380635298784d-10)*y- &
                 1.476849808675d-09)*y+1.84347052385605d-08 )*y+ &
                 3.34382940759405d-07 )*y-1.39428366421645d-06 )*y- &
                 7.50249313713996d-05 )*y-6.26495899187507d-04 )*y+ &
                 4.69716410901162d-02 )*y-6.66871297428209d-01 )*y+ &
                 4.11207530217806d+00
       self%w(1)=((((((((((-1.65995045235997d-15*y+6.91838935879598d-14)*y- &
                 9.131223418888d-13)*y+1.403341829454d-11)*y- &
                 3.672235069444d-10)*y+6.366962546990d-09)*y- &
                 1.039220021671d-07)*y+1.959098751715d-06)*y- &
                 3.33474893152939d-05 )*y+5.72164211151013d-04 )*y- &
                 1.05583210553392d-02 )*y+2.26696066029591d-01
       self%w(2)=((((((((((((-3.57248951192047d-16*y+6.25708409149331d-15)*y- &
                 9.657033089714d-14)*y+1.507864898748d-12)*y- &
                 2.332522256110d-11)*y+3.428545616603d-10)*y- &
                 4.698730937661d-09)*y+6.219977635130d-08)*y- &
                 7.83008889613661d-07 )*y+9.08621687041567d-06 )*y- &
                 9.86368311253873d-05 )*y+9.69632496710088d-04 )*y- &
                 8.14594214284187d-03 )*y+8.50218447733457d-02
       self%w(3)=((((((((((((( 1.64742458534277d-16*y-2.68512265928410d-15)* &
                 y+3.788890667676d-14)*y-5.508918529823d-13)*y+ &
                 7.555896810069d-12)*y-9.69039768312637d-11 )*y+ &
                 1.16034263529672d-09 )*y-1.28771698573873d-08 )*y+ &
                 1.31949431805798d-07 )*y-1.23673915616005d-06 )*y+ &
                 1.04189803544936d-05 )*y-7.79566003744742d-05 )*y+ &
                 5.03162624754434d-04 )*y-2.55138844587555d-03 )*y+ &
                 1.13250730954014d-02
       self%w(4)=((((((((((((((-1.55714130075679d-17*y+2.57193722698891d-16)* &
                 y-3.626606654097d-15)*y+5.234734676175d-14)*y- &
                 7.067105402134d-13)*y+8.793512664890d-12)*y- &
                 1.006088923498d-10)*y+1.050565098393d-09)*y- &
                 9.91517881772662d-09 )*y+8.35835975882941d-08 )*y- &
                 6.19785782240693d-07 )*y+3.95841149373135d-06 )*y- &
                 2.11366761402403d-05 )*y+9.00474771229507d-05 )*y- &
                 2.78777909813289d-04 )*y+5.26543779837487d-04
     else if (x < 15.0d+00) then
       y=x-12.5d+00
       self%r(1)=((((((((((( 4.94869622744119d-17*y+8.03568805739160d-16)*y- &
                 5.599125915431d-15)*y-1.378685560217d-13)*y+ &
                 7.006511663249d-13)*y+1.30391406991118d-11 )*y+ &
                 8.06987313467541d-11 )*y-5.20644072732933d-09 )*y+ &
                 7.72794187755457d-08 )*y-1.61512612564194d-06 )*y+ &
                 4.15083811185831d-05 )*y-7.87855975560199d-04 )*y+ &
                 1.14189319050009d-02
       self%r(2)=((((((((((( 4.89224285522336d-16*y+1.06390248099712d-14)*y- &
                 5.446260182933d-14)*y-1.613630106295d-12)*y+ &
                 3.910179118937d-12)*y+1.90712434258806d-10 )*y+ &
                 8.78470199094761d-10 )*y-5.97332993206797d-08 )*y+ &
                 9.25750831481589d-07 )*y-2.02362185197088d-05 )*y+ &
                 4.92341968336776d-04 )*y-8.68438439874703d-03 )*y+ &
                 1.15825965127958d-01
       self%r(3)=(((((((((( 6.12419396208408d-14*y+1.12328861406073d-13)*y- &
                 9.051094103059d-12)*y-4.781797525341d-11)*y+ &
                 1.660828868694d-09)*y+4.499058798868d-10)*y- &
                 2.519549641933d-07)*y+4.977444040180d-06)*y- &
                 1.25858350034589d-04 )*y+2.70279176970044d-03 )*y- &
                 3.99327850801083d-02 )*y+4.33467200855434d-01
       self%r(4)=((((((((((( 4.63414725924048d-14*y-4.72757262693062d-14)*y- &
                 1.001926833832d-11)*y+6.074107718414d-11)*y+ &
                 1.576976911942d-09)*y-2.01186401974027d-08 )*y- &
                 1.84530195217118d-07 )*y+5.02333087806827d-06 )*y+ &
                 9.66961790843006d-06 )*y-1.58522208889528d-03 )*y+ &
                 2.80539673938339d-02 )*y-2.78953904330072d-01 )*y+ &
                 1.82835655238235d+00
       self%w(4)=((((((((((((( 2.90401781000996d-18*y-4.63389683098251d-17)* &
                 y+6.274018198326d-16)*y-8.936002188168d-15)*y+ &
                 1.194719074934d-13)*y-1.45501321259466d-12 )*y+ &
                 1.64090830181013d-11 )*y-1.71987745310181d-10 )*y+ &
                 1.63738403295718d-09 )*y-1.39237504892842d-08 )*y+ &
                 1.06527318142151d-07 )*y-7.27634957230524d-07 )*y+ &
                 4.12159381310339d-06 )*y-1.74648169719173d-05 )*y+ &
                 8.50290130067818d-05
       self%w(3)=((((((((((((-4.19569145459480d-17*y+5.94344180261644d-16)*y- &
                 1.148797566469d-14)*y+1.881303962576d-13)*y- &
                 2.413554618391d-12)*y+3.372127423047d-11)*y- &
                 4.933988617784d-10)*y+6.116545396281d-09)*y- &
                 6.69965691739299d-08 )*y+7.52380085447161d-07 )*y- &
                 8.08708393262321d-06 )*y+6.88603417296672d-05 )*y- &
                 4.67067112993427d-04 )*y+5.42313365864597d-03
       self%w(2)=((((((((((-6.22272689880615d-15*y+1.04126809657554d-13)*y- &
                 6.842418230913d-13)*y+1.576841731919d-11)*y- &
                 4.203948834175d-10)*y+6.287255934781d-09)*y- &
                 8.307159819228d-08)*y+1.356478091922d-06)*y- &
                 2.08065576105639d-05 )*y+2.52396730332340d-04 )*y- &
                 2.94484050194539d-03 )*y+6.01396183129168d-02
       self%w(1)=(((-1.8784686463512d-01/x+2.2991849164985d-01)/x - &
                 4.9893752514047d-01)/x-2.1916512131607d-05)*exp(-x) + &
                 sqrt_pie4/sqrt(x)-self%w(4)-self%w(3)-self%w(2)
     else
       self%w(1)=sqrt_pie4/sqrt(x)
       if (x < 20.0d+00) then
         y=x-17.5d+00
         self%r(1)=((((((((((( 4.36701759531398d-17*y-1.12860600219889d-16)*y- &
                 6.149849164164d-15)*y+5.820231579541d-14)*y+ &
                 4.396602872143d-13)*y-1.24330365320172d-11 )*y+ &
                 6.71083474044549d-11 )*y+2.43865205376067d-10 )*y+ &
                 1.67559587099969d-08 )*y-9.32738632357572d-07 )*y+ &
                 2.39030487004977d-05 )*y-4.68648206591515d-04 )*y+ &
                 8.34977776583956d-03
         self%r(2)=((((((((((( 4.98913142288158d-16*y-2.60732537093612d-16)*y- &
                 7.775156445127d-14)*y+5.766105220086d-13)*y+ &
                 6.432696729600d-12)*y-1.39571683725792d-10 )*y+ &
                 5.95451479522191d-10 )*y+2.42471442836205d-09 )*y+ &
                 2.47485710143120d-07 )*y-1.14710398652091d-05 )*y+ &
                 2.71252453754519d-04 )*y-4.96812745851408d-03 )*y+ &
                 8.26020602026780d-02
         self%r(3)=((((((((((( 1.91498302509009d-15*y+1.48840394311115d-14)*y- &
                 4.316925145767d-13)*y+1.186495793471d-12)*y+ &
                 4.615806713055d-11)*y-5.54336148667141d-10 )*y+ &
                 3.48789978951367d-10 )*y-2.79188977451042d-09 )*y+ &
                 2.09563208958551d-06 )*y-6.76512715080324d-05 )*y+ &
                 1.32129867629062d-03 )*y-2.05062147771513d-02 )*y+ &
                 2.88068671894324d-01
         self%r(4)=(((((((((((-5.43697691672942d-15*y-1.12483395714468d-13)*y+ &
                 2.826607936174d-12)*y-1.266734493280d-11)*y- &
                 4.258722866437d-10)*y+9.45486578503261d-09 )*y- &
                 5.86635622821309d-08 )*y-1.28835028104639d-06 )*y+ &
                 4.41413815691885d-05 )*y-7.61738385590776d-04 )*y+ &
                 9.66090902985550d-03 )*y-1.01410568057649d-01 )*y+ &
                 9.54714798156712d-01
         self%w(4)=((((((((((((-7.56882223582704d-19*y+7.53541779268175d-18)*y- &
                 1.157318032236d-16)*y+2.411195002314d-15)*y- &
                 3.601794386996d-14)*y+4.082150659615d-13)*y- &
                 4.289542980767d-12)*y+5.086829642731d-11)*y- &
                 6.35435561050807d-10 )*y+6.82309323251123d-09 )*y- &
                 5.63374555753167d-08 )*y+3.57005361100431d-07 )*y- &
                 2.40050045173721d-06 )*y+4.94171300536397d-05
         self%w(3)=(((((((((((-5.54451040921657d-17*y+2.68748367250999d-16)*y+ &
                 1.349020069254d-14)*y-2.507452792892d-13)*y+ &
                 1.944339743818d-12)*y-1.29816917658823d-11 )*y+ &
                 3.49977768819641d-10 )*y-8.67270669346398d-09 )*y+ &
                 1.31381116840118d-07 )*y-1.36790720600822d-06 )*y+ &
                 1.19210697673160d-05 )*y-1.42181943986587d-04 )*y+ &
                 4.12615396191829d-03
         self%w(2)=(((((((((((-1.86506057729700d-16*y+1.16661114435809d-15)*y+ &
                 2.563712856363d-14)*y-4.498350984631d-13)*y+ &
                 1.765194089338d-12)*y+9.04483676345625d-12 )*y+ &
                 4.98930345609785d-10 )*y-2.11964170928181d-08 )*y+ &
                 3.98295476005614d-07 )*y-5.49390160829409d-06 )*y+ &
                 7.74065155353262d-05 )*y-1.48201933009105d-03 )*y+ &
                 4.97836392625268d-02
         self%w(1)=(( 1.9623264149430d-01/x-4.9695241464490d-01)/x - &
                 6.0156581186481d-05)*exp(-x)+self%w(1)-self%w(2)-self%w(3)-self%w(4)
       else if (x < 35.0d+00) then
         e=exp(-x)
         self%r(1)=((((((-4.45711399441838d-05*x+1.27267770241379d-03)*x - &
                 2.36954961381262d-01)*x+1.54330657903756d+01)*x - &
                 5.22799159267808d+02)*x+1.05951216669313d+04)*x + (- &
                 2.51177235556236d+06/x+8.72975373557709d+05)/x - &
                 1.29194382386499d+05)*e + r14/(x-r14)
         self%r(2)=(((((-7.85617372254488d-02*x+6.35653573484868d+00)*x - &
                 3.38296938763990d+02)*x+1.25120495802096d+04)*x - &
                 3.16847570511637d+05)*x + ((-1.02427466127427d+09/x + &
                 3.70104713293016d+08)/x-5.87119005093822d+07)/x + &
                 5.38614211391604d+06)*e + r24/(x-r24)
         self%r(3)=(((((-2.37900485051067d-01*x+1.84122184400896d+01)*x - &
                 1.00200731304146d+03)*x+3.75151841595736d+04)*x - &
                 9.50626663390130d+05)*x + ((-2.88139014651985d+09/x + &
                 1.06625915044526d+09)/x-1.72465289687396d+08)/x + &
                 1.60419390230055d+07)*e + r34/(x-r34)
         self%r(4)=((((((-6.00691586407385d-04*x-3.64479545338439d-01)*x + &
                 1.57496131755179d+01)*x-6.54944248734901d+02)*x + &
                 1.70830039597097d+04)*x-2.90517939780207d+05)*x + (+ &
                 3.49059698304732d+07/x-1.64944522586065d+07)/x + &
                 2.96817940164703d+06)*e + r44/(x-r44)
         if (x < 25.0d+00) then
           self%w(4)=((((((( 2.33766206773151d-07*x- &
                 3.81542906607063d-05)*x +3.51416601267000d-03)*x- &
                 1.66538571864728d-01)*x +4.80006136831847d+00)*x- &
                 8.73165934223603d+01)*x +9.77683627474638d+02)*x + &
                 1.66000945117640d+04/x -6.14479071209961d+03)*e + w44*self%w(1)
         else
           self%w(4)=(((((( 5.74245945342286d-06*x- &
                 7.58735928102351d-05)*x +2.35072857922892d-04)*x- &
                 3.78812134013125d-03)*x +3.09871652785805d-01)*x- &
                 7.11108633061306d+00)*x +5.55297573149528d+01)*e + w44*self%w(1)
         end if
         self%w(3)=(((((( 2.36392855180768d-04*x-9.16785337967013d-03)*x + &
                 4.62186525041313d-01)*x-1.96943786006540d+01)*x + &
                 4.99169195295559d+02)*x-6.21419845845090d+03)*x + ((+ &
                 5.21445053212414d+07/x-1.34113464389309d+07)/x + &
                 1.13673298305631d+06)/x-2.81501182042707d+03)*e + w34*self%w(1)
         self%w(2)=(((((( 7.29841848989391d-04*x-3.53899555749875d-02)*x + &
                 2.07797425718513d+00)*x-1.00464709786287d+02)*x + &
                 3.15206108877819d+03)*x-6.27054715090012d+04)*x + (+ &
                 1.54721246264919d+07/x-5.26074391316381d+06)/x + &
                 7.67135400969617d+05)*e + w24*self%w(1)
         self%w(1)=(( 1.9623264149430d-01/x-4.9695241464490d-01)/x - &
                 6.0156581186481d-05)*e + self%w(1)-self%w(2)-self%w(3)-self%w(4)
       else if (x < 53.0d+00) then
         e=exp(-x)*(x*x)**2
         self%r(4)=((-2.19135070169653d-03*x-1.19108256987623d-01)*x - &
                 7.50238795695573d-01)*e + r44/(x-r44)
         self%r(3)=((-9.65842534508637d-04*x-4.49822013469279d-02)*x + &
                 6.08784033347757d-01)*e + r34/(x-r34)
         self%r(2)=((-3.62569791162153d-04*x-9.09231717268466d-03)*x + &
                 1.84336760556262d-01)*e + r24/(x-r24)
         self%r(1)=((-4.07557525914600d-05*x-6.88846864931685d-04)*x + &
                 1.74725309199384d-02)*e + r14/(x-r14)
         self%w(4)=(( 5.76631982000990d-06*x-7.89187283804890d-05)*x + &
                 3.28297971853126d-04)*e + w44*self%w(1)
         self%w(3)=(( 2.08294969857230d-04*x-3.77489954837361d-03)*x + &
                 2.09857151617436d-02)*e + w34*self%w(1)
         self%w(2)=(( 6.16374517326469d-04*x-1.26711744680092d-02)*x + &
                 8.14504890732155d-02)*e + w24*self%w(1)
         self%w(1)=self%w(1)-self%w(2)-self%w(3)-self%w(4)
       else
         self%r(1)=r14/(x-r14)
         self%r(2)=r24/(x-r24)
         self%r(3)=r34/(x-r34)
         self%r(4)=r44/(x-r44)
         self%w(4)=w44*self%w(1)
         self%w(3)=w34*self%w(1)
         self%w(2)=w24*self%w(1)
         self%w(1)=self%w(1)-self%w(2)-self%w(3)-self%w(4)
       end if
     end if

   end subroutine

   pure subroutine get_weights5(self,x)
    type(rys_type) :: self
    ! For five roots.
     intent(inout) :: self
     real(kind=kind(1.0d0)), intent(in) :: x
     real(kind=kind(1.0d0)) :: y,xx, e

     if (x < 3.0d-07) then
       self%r(1)=2.26659266316985d-02 -2.15865967920897d-03 *x
       self%r(2)=2.31271692140903d-01 -2.20258754389745d-02 *x
       self%r(3)=8.57346024118836d-01 -8.16520023025515d-02 *x
       self%r(4)=2.97353038120346d+00 -2.83193369647137d-01 *x
       self%r(5)=1.84151859759051d+01 -1.75382723579439d+00 *x
       self%w(1)=2.95524224714752d-01 -1.96867576909777d-02 *x
       self%w(2)=2.69266719309995d-01 -5.61737590184721d-02 *x
       self%w(3)=2.19086362515981d-01 -9.71152726793658d-02 *x
       self%w(4)=1.49451349150580d-01 -1.02979262193565d-01 *x
       self%w(5)=6.66713443086877d-02 -5.73782817488315d-02 *x
     else if (x < 1.0d-00) then
       self%r(1)=((((((-4.46679165328413d-11*x+1.21879111988031d-09)*x- &
                 2.62975022612104d-08 )*x+5.15106194905897d-07 )*x- &
                 9.27933625824749d-06 )*x+1.51794097682482d-04 )*x- &
                 2.15865967920301d-03 )*x+2.26659266316985d-02
       self%r(2)=(((((( 1.93117331714174d-10*x-4.57267589660699d-09)*x+ &
                 2.48339908218932d-08 )*x+1.50716729438474d-06 )*x- &
                 6.07268757707381d-05 )*x+1.37506939145643d-03 )*x- &
                 2.20258754419939d-02 )*x+2.31271692140905d-01
       self%r(3)=((((( 4.84989776180094d-09*x+1.31538893944284d-07)*x- &
                 2.766753852879d-06)*x-7.651163510626d-05)*x+ &
                 4.033058545972d-03)*x-8.16520022916145d-02 )*x+ &
                 8.57346024118779d-01
       self%r(4)=((((-2.48581772214623d-07*x-4.34482635782585d-06)*x- &
                 7.46018257987630d-07 )*x+1.01210776517279d-02 )*x- &
                 2.83193369640005d-01 )*x+2.97353038120345d+00
       self%r(5)=(((((-8.92432153868554d-09*x+1.77288899268988d-08)*x+ &
                 3.040754680666d-06)*x+1.058229325071d-04)*x+ &
                 4.596379534985d-02)*x-1.75382723579114d+00 )*x+ &
                 1.84151859759049d+01
       self%w(1)=((((((-2.03822632771791d-09*x+3.89110229133810d-08)*x- &
                 5.84914787904823d-07 )*x+8.30316168666696d-06 )*x- &
                 1.13218402310546d-04 )*x+1.49128888586790d-03 )*x- &
                 1.96867576904816d-02 )*x+2.95524224714749d-01
       self%w(2)=((((((( 8.62848118397570d-09*x-1.38975551148989d-07)*x+ &
                 1.602894068228d-06)*x-1.646364300836d-05)*x+ &
                 1.538445806778d-04)*x-1.28848868034502d-03 )*x+ &
                 9.38866933338584d-03 )*x-5.61737590178812d-02 )*x+ &
                 2.69266719309991d-01
       self%w(3)=((((((((-9.41953204205665d-09*x+1.47452251067755d-07)*x- &
                 1.57456991199322d-06 )*x+1.45098401798393d-05 )*x- &
                 1.18858834181513d-04 )*x+8.53697675984210d-04 )*x- &
                 5.22877807397165d-03 )*x+2.60854524809786d-02 )*x- &
                 9.71152726809059d-02 )*x+2.19086362515979d-01
       self%w(4)=((((((((-3.84961617022042d-08*x+5.66595396544470d-07)*x- &
                 5.52351805403748d-06 )*x+4.53160377546073d-05 )*x- &
                 3.22542784865557d-04 )*x+1.95682017370967d-03 )*x- &
                 9.77232537679229d-03 )*x+3.79455945268632d-02 )*x- &
                 1.02979262192227d-01 )*x+1.49451349150573d-01
       self%w(5)=((((((((( 4.09594812521430d-09*x-6.47097874264417d-08)*x+ &
                 6.743541482689d-07)*x-5.917993920224d-06)*x+ &
                 4.531969237381d-05)*x-2.99102856679638d-04 )*x+ &
                 1.65695765202643d-03 )*x-7.40671222520653d-03 )*x+ &
                 2.50889946832192d-02 )*x-5.73782817487958d-02 )*x+ &
                 6.66713443086877d-02
     else if (x < 5.0d-00) then
       y=x-3.0d+00
       self%r(1)=((((((((-2.58163897135138d-14*y+8.14127461488273d-13)*y- &
                 2.11414838976129d-11 )*y+5.09822003260014d-10 )*y- &
                 1.16002134438663d-08 )*y+2.46810694414540d-07 )*y- &
                 4.92556826124502d-06 )*y+9.02580687971053d-05 )*y- &
                 1.45190025120726d-03 )*y+1.73416786387475d-02
       self%r(2)=((((((((( 1.04525287289788d-14*y+5.44611782010773d-14)*y- &
                 4.831059411392d-12)*y+1.136643908832d-10)*y- &
                 1.104373076913d-09)*y-2.35346740649916d-08 )*y+ &
                 1.43772622028764d-06 )*y-4.23405023015273d-05 )*y+ &
                 9.12034574793379d-04 )*y-1.52479441718739d-02 )*y+ &
                 1.76055265928744d-01
       self%r(3)=(((((((((-6.89693150857911d-14*y+5.92064260918861d-13)*y+ &
                 1.847170956043d-11)*y-3.390752744265d-10)*y- &
                 2.995532064116d-09)*y+1.57456141058535d-07 )*y- &
                 3.95859409711346d-07 )*y-9.58924580919747d-05 )*y+ &
                 3.23551502557785d-03 )*y-5.97587007636479d-02 )*y+ &
                 6.46432853383057d-01
       self%r(4)=((((((((-3.61293809667763d-12*y-2.70803518291085d-11)*y+ &
                 8.83758848468769d-10 )*y+1.59166632851267d-08 )*y- &
                 1.32581997983422d-07 )*y-7.60223407443995d-06 )*y- &
                 7.41019244900952d-05 )*y+9.81432631743423d-03 )*y- &
                 2.23055570487771d-01 )*y+2.21460798080643d+00
       self%r(5)=((((((((( 7.12332088345321d-13*y+3.16578501501894d-12)*y- &
                 8.776668218053d-11)*y-2.342817613343d-09)*y- &
                 3.496962018025d-08)*y-3.03172870136802d-07 )*y+ &
                 1.50511293969805d-06 )*y+1.37704919387696d-04 )*y+ &
                 4.70723869619745d-02 )*y-1.47486623003693d+00 )*y+ &
                 1.35704792175847d+01
       self%w(1)=((((((((( 1.04348658616398d-13*y-1.94147461891055d-12)*y+ &
                 3.485512360993d-11)*y-6.277497362235d-10)*y+ &
                 1.100758247388d-08)*y-1.88329804969573d-07 )*y+ &
                 3.12338120839468d-06 )*y-5.04404167403568d-05 )*y+ &
                 8.00338056610995d-04 )*y-1.30892406559521d-02 )*y+ &
                 2.47383140241103d-01
       self%w(2)=((((((((((( 3.23496149760478d-14*y-5.24314473469311d-13)*y+ &
                 7.743219385056d-12)*y-1.146022750992d-10)*y+ &
                 1.615238462197d-09)*y-2.15479017572233d-08 )*y+ &
                 2.70933462557631d-07 )*y-3.18750295288531d-06 )*y+ &
                 3.47425221210099d-05 )*y-3.45558237388223d-04 )*y+ &
                 3.05779768191621d-03 )*y-2.29118251223003d-02 )*y+ &
                 1.59834227924213d-01
       self%w(3)=((((((((((((-3.42790561802876d-14*y+5.26475736681542d-13)*y- &
                 7.184330797139d-12)*y+9.763932908544d-11)*y- &
                 1.244014559219d-09)*y+1.472744068942d-08)*y- &
                 1.611749975234d-07)*y+1.616487851917d-06)*y- &
                 1.46852359124154d-05 )*y+1.18900349101069d-04 )*y- &
                 8.37562373221756d-04 )*y+4.93752683045845d-03 )*y- &
                 2.25514728915673d-02 )*y+6.95211812453929d-02
       self%w(4)=((((((((((((( 1.04072340345039d-14*y-1.60808044529211d-13)* &
                 y+2.183534866798d-12)*y-2.939403008391d-11)*y+ &
                 3.679254029085d-10)*y-4.23775673047899d-09 )*y+ &
                 4.46559231067006d-08 )*y-4.26488836563267d-07 )*y+ &
                 3.64721335274973d-06 )*y-2.74868382777722d-05 )*y+ &
                 1.78586118867488d-04 )*y-9.68428981886534d-04 )*y+ &
                 4.16002324339929d-03 )*y-1.28290192663141d-02 )*y+ &
                 2.22353727685016d-02
       self%w(5)=((((((((((((((-8.16770412525963d-16*y+1.31376515047977d-14)* &
                 y-1.856950818865d-13)*y+2.596836515749d-12)*y- &
                 3.372639523006d-11)*y+4.025371849467d-10)*y- &
                 4.389453269417d-09)*y+4.332753856271d-08)*y- &
                 3.82673275931962d-07 )*y+2.98006900751543d-06 )*y- &
                 2.00718990300052d-05 )*y+1.13876001386361d-04 )*y- &
                 5.23627942443563d-04 )*y+1.83524565118203d-03 )*y- &
                 4.37785737450783d-03 )*y+5.36963805223095d-03
     else if (x < 10.0d-00) then
       y=x-7.5d+00
       self%r(1)=((((((((-1.13825201010775d-14*y+1.89737681670375d-13)*y- &
                 4.81561201185876d-12 )*y+1.56666512163407d-10 )*y- &
                 3.73782213255083d-09 )*y+9.15858355075147d-08 )*y- &
                 2.13775073585629d-06 )*y+4.56547356365536d-05 )*y- &
                 8.68003909323740d-04 )*y+1.22703754069176d-02
       self%r(2)=(((((((((-3.67160504428358d-15*y+1.27876280158297d-14)*y- &
                 1.296476623788d-12)*y+1.477175434354d-11)*y+ &
                 5.464102147892d-10)*y-2.42538340602723d-08 )*y+ &
                 8.20460740637617d-07 )*y-2.20379304598661d-05 )*y+ &
                 4.90295372978785d-04 )*y-9.14294111576119d-03 )*y+ &
                 1.22590403403690d-01
       self%r(3)=((((((((( 1.39017367502123d-14*y-6.96391385426890d-13)*y+ &
                 1.176946020731d-12)*y+1.725627235645d-10)*y- &
                 3.686383856300d-09)*y+2.87495324207095d-08 )*y+ &
                 1.71307311000282d-06 )*y-7.94273603184629d-05 )*y+ &
                 2.00938064965897d-03 )*y-3.63329491677178d-02 )*y+ &
                 4.34393683888443d-01
       self%r(4)=((((((((((-1.27815158195209d-14*y+1.99910415869821d-14)*y+ &
                 3.753542914426d-12)*y-2.708018219579d-11)*y- &
                 1.190574776587d-09)*y+1.106696436509d-08)*y+ &
                 3.954955671326d-07)*y-4.398596059588d-06)*y- &
                 2.01087998907735d-04 )*y+7.89092425542937d-03 )*y- &
                 1.42056749162695d-01 )*y+1.39964149420683d+00
       self%r(5)=((((((((((-1.19442341030461d-13*y-2.34074833275956d-12)*y+ &
                 6.861649627426d-12)*y+6.082671496226d-10)*y+ &
                 5.381160105420d-09)*y-6.253297138700d-08)*y- &
                 2.135966835050d-06)*y-2.373394341886d-05)*y+ &
                 2.88711171412814d-06 )*y+4.85221195290753d-02 )*y- &
                 1.04346091985269d+00 )*y+7.89901551676692d+00
       self%w(1)=((((((((( 7.95526040108997d-15*y-2.48593096128045d-13)*y+ &
                 4.761246208720d-12)*y-9.535763686605d-11)*y+ &
                 2.225273630974d-09)*y-4.49796778054865d-08 )*y+ &
                 9.17812870287386d-07 )*y-1.86764236490502d-05 )*y+ &
                 3.76807779068053d-04 )*y-8.10456360143408d-03 )*y+ &
                 2.01097936411496d-01
       self%w(2)=((((((((((( 1.25678686624734d-15*y-2.34266248891173d-14)*y+ &
                 3.973252415832d-13)*y-6.830539401049d-12)*y+ &
                 1.140771033372d-10)*y-1.82546185762009d-09 )*y+ &
                 2.77209637550134d-08 )*y-4.01726946190383d-07 )*y+ &
                 5.48227244014763d-06 )*y-6.95676245982121d-05 )*y+ &
                 8.05193921815776d-04 )*y-8.15528438784469d-03 )*y+ &
                 9.71769901268114d-02
       self%w(3)=((((((((((((-8.20929494859896d-16*y+1.37356038393016d-14)*y- &
                 2.022863065220d-13)*y+3.058055403795d-12)*y- &
                 4.387890955243d-11)*y+5.923946274445d-10)*y- &
                 7.503659964159d-09)*y+8.851599803902d-08)*y- &
                 9.65561998415038d-07 )*y+9.60884622778092d-06 )*y- &
                 8.56551787594404d-05 )*y+6.66057194311179d-04 )*y- &
                 4.17753183902198d-03 )*y+2.25443826852447d-02
       self%w(4)=((((((((((((((-1.08764612488790d-17*y+1.85299909689937d-16)* &
                 y-2.730195628655d-15)*y+4.127368817265d-14)*y- &
                 5.881379088074d-13)*y+7.805245193391d-12)*y- &
                 9.632707991704d-11)*y+1.099047050624d-09)*y- &
                 1.15042731790748d-08 )*y+1.09415155268932d-07 )*y- &
                 9.33687124875935d-07 )*y+7.02338477986218d-06 )*y- &
                 4.53759748787756d-05 )*y+2.41722511389146d-04 )*y- &
                 9.75935943447037d-04 )*y+2.57520532789644d-03
       self%w(5)=((((((((((((((( 7.28996979748849d-19*y-1.26518146195173d-17) &
                 *y+1.886145834486d-16)*y-2.876728287383d-15)*y+ &
                 4.114588668138d-14)*y-5.44436631413933d-13 )*y+ &
                 6.64976446790959d-12 )*y-7.44560069974940d-11 )*y+ &
                 7.57553198166848d-10 )*y-6.92956101109829d-09 )*y+ &
                 5.62222859033624d-08 )*y-3.97500114084351d-07 )*y+ &
                 2.39039126138140d-06 )*y-1.18023950002105d-05 )*y+ &
                 4.52254031046244d-05 )*y-1.21113782150370d-04 )*y+ &
                 1.75013126731224d-04
     else if (x < 15.0d-00) then
       y=x-12.5d+00
       self%r(1)=((((((((((-4.16387977337393d-17*y+7.20872997373860d-16)*y+ &
                 1.395993802064d-14)*y+3.660484641252d-14)*y- &
                 4.154857548139d-12)*y+2.301379846544d-11)*y- &
                 1.033307012866d-09)*y+3.997777641049d-08)*y- &
                 9.35118186333939d-07 )*y+2.38589932752937d-05 )*y- &
                 5.35185183652937d-04 )*y+8.85218988709735d-03
       self%r(2)=((((((((((-4.56279214732217d-16*y+6.24941647247927d-15)*y+ &
                 1.737896339191d-13)*y+8.964205979517d-14)*y- &
                 3.538906780633d-11)*y+9.561341254948d-11)*y- &
                 9.772831891310d-09)*y+4.240340194620d-07)*y- &
                 1.02384302866534d-05 )*y+2.57987709704822d-04 )*y- &
                 5.54735977651677d-03 )*y+8.68245143991948d-02
       self%r(3)=((((((((((-2.52879337929239d-15*y+2.13925810087833d-14)*y+ &
                 7.884307667104d-13)*y-9.023398159510d-13)*y- &
                 5.814101544957d-11)*y-1.333480437968d-09)*y- &
                 2.217064940373d-08)*y+1.643290788086d-06)*y- &
                 4.39602147345028d-05 )*y+1.08648982748911d-03 )*y- &
                 2.13014521653498d-02 )*y+2.94150684465425d-01
       self%r(4)=((((((((((-6.42391438038888d-15*y+5.37848223438815d-15)*y+ &
                 8.960828117859d-13)*y+5.214153461337d-11)*y- &
                 1.106601744067d-10)*y-2.007890743962d-08)*y+ &
                 1.543764346501d-07)*y+4.520749076914d-06)*y- &
                 1.88893338587047d-04 )*y+4.73264487389288d-03 )*y- &
                 7.91197893350253d-02 )*y+8.60057928514554d-01
       self%r(5)=(((((((((((-2.24366166957225d-14*y+4.87224967526081d-14)*y+ &
                 5.587369053655d-12)*y-3.045253104617d-12)*y- &
                 1.223983883080d-09)*y-2.05603889396319d-09 )*y+ &
                 2.58604071603561d-07 )*y+1.34240904266268d-06 )*y- &
                 5.72877569731162d-05 )*y-9.56275105032191d-04 )*y+ &
                 4.23367010370921d-02 )*y-5.76800927133412d-01 )*y+ &
                 3.87328263873381d+00
       self%w(1)=((((((((( 8.98007931950169d-15*y+7.25673623859497d-14)*y+ &
                 5.851494250405d-14)*y-4.234204823846d-11)*y+ &
                 3.911507312679d-10)*y-9.65094802088511d-09 )*y+ &
                 3.42197444235714d-07 )*y-7.51821178144509d-06 )*y+ &
                 1.94218051498662d-04 )*y-5.38533819142287d-03 )*y+ &
                 1.68122596736809d-01
       self%w(2)=((((((((((-1.05490525395105d-15*y+1.96855386549388d-14)*y- &
                 5.500330153548d-13)*y+1.003849567976d-11)*y- &
                 1.720997242621d-10)*y+3.533277061402d-09)*y- &
                 6.389171736029d-08)*y+1.046236652393d-06)*y- &
                 1.73148206795827d-05 )*y+2.57820531617185d-04 )*y- &
                 3.46188265338350d-03 )*y+7.03302497508176d-02
       self%w(3)=((((((((((( 3.60020423754545d-16*y-6.24245825017148d-15)*y+ &
                 9.945311467434d-14)*y-1.749051512721d-12)*y+ &
                 2.768503957853d-11)*y-4.08688551136506d-10 )*y+ &
                 6.04189063303610d-09 )*y-8.23540111024147d-08 )*y+ &
                 1.01503783870262d-06 )*y-1.20490761741576d-05 )*y+ &
                 1.26928442448148d-04 )*y-1.05539461930597d-03 )*y+ &
                 1.15543698537013d-02
       self%w(4)=((((((((((((( 2.51163533058925d-18*y-4.31723745510697d-17)* &
                 y+6.557620865832d-16)*y-1.016528519495d-14)*y+ &
                 1.491302084832d-13)*y-2.06638666222265d-12 )*y+ &
                 2.67958697789258d-11 )*y-3.23322654638336d-10 )*y+ &
                 3.63722952167779d-09 )*y-3.75484943783021d-08 )*y+ &
                 3.49164261987184d-07 )*y-2.92658670674908d-06 )*y+ &
                 2.12937256719543d-05 )*y-1.19434130620929d-04 )*y+ &
                 6.45524336158384d-04
       self%w(5)=((((((((((((((-1.29043630202811d-19*y+2.16234952241296d-18)* &
                 y-3.107631557965d-17)*y+4.570804313173d-16)*y- &
                 6.301348858104d-15)*y+8.031304476153d-14)*y- &
                 9.446196472547d-13)*y+1.018245804339d-11)*y- &
                 9.96995451348129d-11 )*y+8.77489010276305d-10 )*y- &
                 6.84655877575364d-09 )*y+4.64460857084983d-08 )*y- &
                 2.66924538268397d-07 )*y+1.24621276265907d-06 )*y- &
                 4.30868944351523d-06 )*y+9.94307982432868d-06
     else if (x < 20.0d-00) then
       y=x-17.5d+00
       self%r(1)=(((((((((( 1.91875764545740d-16*y+7.8357401095707d-16)*y- &
                 3.260875931644d-14)*y-1.186752035569d-13)*y+ &
                 4.275180095653d-12)*y+3.357056136731d-11)*y- &
                 1.123776903884d-09)*y+1.231203269887d-08)*y- &
                 3.99851421361031d-07 )*y+1.45418822817771d-05 )*y- &
                 3.49912254976317d-04 )*y+6.67768703938812d-03
       self%r(2)=(((((((((( 2.02778478673555d-15*y+1.01640716785099d-14)*y- &
                 3.385363492036d-13)*y-1.615655871159d-12)*y+ &
                 4.527419140333d-11)*y+3.853670706486d-10)*y- &
                 1.184607130107d-08)*y+1.347873288827d-07)*y- &
                 4.47788241748377d-06 )*y+1.54942754358273d-04 )*y- &
                 3.55524254280266d-03 )*y+6.44912219301603d-02
       self%r(3)=(((((((((( 7.79850771456444d-15*y+6.00464406395001d-14)*y- &
                 1.249779730869d-12)*y-1.020720636353d-11)*y+ &
                 1.814709816693d-10)*y+1.766397336977d-09)*y- &
                 4.603559449010d-08)*y+5.863956443581d-07)*y- &
                 2.03797212506691d-05 )*y+6.31405161185185d-04 )*y- &
                 1.30102750145071d-02 )*y+2.10244289044705d-01
       self%r(4)=(((((((((((-2.92397030777912d-15*y+1.94152129078465d-14)*y+ &
                 4.859447665850d-13)*y-3.217227223463d-12)*y- &
                 7.484522135512d-11)*y+7.19101516047753d-10 )*y+ &
                 6.88409355245582d-09 )*y-1.44374545515769d-07 )*y+ &
                 2.74941013315834d-06 )*y-1.02790452049013d-04 )*y+ &
                 2.59924221372643d-03 )*y-4.35712368303551d-02 )*y+ &
                 5.62170709585029d-01
       self%r(5)=((((((((((( 1.17976126840060d-14*y+1.24156229350669d-13)*y- &
                 3.892741622280d-12)*y-7.755793199043d-12)*y+ &
                 9.492190032313d-10)*y-4.98680128123353d-09 )*y- &
                 1.81502268782664d-07 )*y+2.69463269394888d-06 )*y+ &
                 2.50032154421640d-05 )*y-1.33684303917681d-03 )*y+ &
                 2.29121951862538d-02 )*y-2.45653725061323d-01 )*y+ &
                 1.89999883453047d+00
       self%w(1)=(((((((((( 1.74841995087592d-15*y-6.95671892641256d-16)*y- &
                 3.000659497257d-13)*y+2.021279817961d-13)*y+ &
                 3.853596935400d-11)*y+1.461418533652d-10)*y- &
                 1.014517563435d-08)*y+1.132736008979d-07)*y- &
                 2.86605475073259d-06 )*y+1.21958354908768d-04 )*y- &
                 3.86293751153466d-03 )*y+1.45298342081522d-01
       self%w(2)=((((((((((-1.11199320525573d-15*y+1.85007587796671d-15)*y+ &
                 1.220613939709d-13)*y+1.275068098526d-12)*y- &
                 5.341838883262d-11)*y+6.161037256669d-10)*y- &
                 1.009147879750d-08)*y+2.907862965346d-07)*y- &
                 6.12300038720919d-06 )*y+1.00104454489518d-04 )*y- &
                 1.80677298502757d-03 )*y+5.78009914536630d-02
       self%w(3)=((((((((((-9.49816486853687d-16*y+6.67922080354234d-15)*y+ &
                 2.606163540537d-15)*y+1.983799950150d-12)*y- &
                 5.400548574357d-11)*y+6.638043374114d-10)*y- &
                 8.799518866802d-09)*y+1.791418482685d-07)*y- &
                 2.96075397351101d-06 )*y+3.38028206156144d-05 )*y- &
                 3.58426847857878d-04 )*y+8.39213709428516d-03
       self%w(4)=((((((((((( 1.33829971060180d-17*y-3.44841877844140d-16)*y+ &
                 4.745009557656d-15)*y-6.033814209875d-14)*y+ &
                 1.049256040808d-12)*y-1.70859789556117d-11 )*y+ &
                 2.15219425727959d-10 )*y-2.52746574206884d-09 )*y+ &
                 3.27761714422960d-08 )*y-3.90387662925193d-07 )*y+ &
                 3.46340204593870d-06 )*y-2.43236345136782d-05 )*y+ &
                 3.54846978585226d-04
       self%w(5)=((((((((((((( 2.69412277020887d-20*y-4.24837886165685d-19)* &
                 y+6.030500065438d-18)*y-9.069722758289d-17)*y+ &
                 1.246599177672d-15)*y-1.56872999797549d-14 )*y+ &
                 1.87305099552692d-13 )*y-2.09498886675861d-12 )*y+ &
                 2.11630022068394d-11 )*y-1.92566242323525d-10 )*y+ &
                 1.62012436344069d-09 )*y-1.23621614171556d-08 )*y+ &
                 7.72165684563049d-08 )*y-3.59858901591047d-07 )*y+ &
                 2.43682618601000d-06
     else if (x < 25.0d-00) then
       y=x-22.5d+00
       self%r(1)=(((((((((-1.13927848238726d-15*y+7.39404133595713d-15)*y+ &
                 1.445982921243d-13)*y-2.676703245252d-12)*y+ &
                 5.823521627177d-12)*y+2.17264723874381d-10 )*y+ &
                 3.56242145897468d-09 )*y-3.03763737404491d-07 )*y+ &
                 9.46859114120901d-06 )*y-2.30896753853196d-04 )*y+ &
                 5.24663913001114d-03
       self%r(2)=(((((((((( 2.89872355524581d-16*y-1.22296292045864d-14)*y+ &
                 6.184065097200d-14)*y+1.649846591230d-12)*y- &
                 2.729713905266d-11)*y+3.709913790650d-11)*y+ &
                 2.216486288382d-09)*y+4.616160236414d-08)*y- &
                 3.32380270861364d-06 )*y+9.84635072633776d-05 )*y- &
                 2.30092118015697d-03 )*y+5.00845183695073d-02
       self%r(3)=(((((((((( 1.97068646590923d-15*y-4.89419270626800d-14)*y+ &
                 1.136466605916d-13)*y+7.546203883874d-12)*y- &
                 9.635646767455d-11)*y-8.295965491209d-11)*y+ &
                 7.534109114453d-09)*y+2.699970652707d-07)*y- &
                 1.42982334217081d-05 )*y+3.78290946669264d-04 )*y- &
                 8.03133015084373d-03 )*y+1.58689469640791d-01
       self%r(4)=(((((((((( 1.33642069941389d-14*y-1.55850612605745d-13)*y- &
                 7.522712577474d-13)*y+3.209520801187d-11)*y- &
                 2.075594313618d-10)*y-2.070575894402d-09)*y+ &
                 7.323046997451d-09)*y+1.851491550417d-06)*y- &
                 6.37524802411383d-05 )*y+1.36795464918785d-03 )*y- &
                 2.42051126993146d-02 )*y+3.97847167557815d-01
       self%r(5)=((((((((((-6.07053986130526d-14*y+1.04447493138843d-12)*y- &
                 4.286617818951d-13)*y-2.632066100073d-10)*y+ &
                 4.804518986559d-09)*y-1.835675889421d-08)*y- &
                 1.068175391334d-06)*y+3.292234974141d-05)*y- &
                 5.94805357558251d-04 )*y+8.29382168612791d-03 )*y- &
                 9.93122509049447d-02 )*y+1.09857804755042d+00
       self%w(1)=(((((((((-9.10338640266542d-15*y+1.00438927627833d-13)*y+ &
                 7.817349237071d-13)*y-2.547619474232d-11)*y+ &
                 1.479321506529d-10)*y+1.52314028857627d-09 )*y+ &
                 9.20072040917242d-09 )*y-2.19427111221848d-06 )*y+ &
                 8.65797782880311d-05 )*y-2.82718629312875d-03 )*y+ &
                 1.28718310443295d-01
       self%w(2)=((((((((( 5.52380927618760d-15*y-6.43424400204124d-14)*y- &
                 2.358734508092d-13)*y+8.261326648131d-12)*y+ &
                 9.229645304956d-11)*y-5.68108973828949d-09 )*y+ &
                 1.22477891136278d-07 )*y-2.11919643127927d-06 )*y+ &
                 4.23605032368922d-05 )*y-1.14423444576221d-03 )*y+ &
                 5.06607252890186d-02
       self%w(3)=((((((((( 3.99457454087556d-15*y-5.11826702824182d-14)*y- &
                 4.157593182747d-14)*y+4.214670817758d-12)*y+ &
                 6.705582751532d-11)*y-3.36086411698418d-09 )*y+ &
                 6.07453633298986d-08 )*y-7.40736211041247d-07 )*y+ &
                 8.84176371665149d-06 )*y-1.72559275066834d-04 )*y+ &
                 7.16639814253567d-03
       self%w(4)=(((((((((((-2.14649508112234d-18*y-2.45525846412281d-18)*y+ &
                 6.126212599772d-16)*y-8.526651626939d-15)*y+ &
                 4.826636065733d-14)*y-3.39554163649740d-13 )*y+ &
                 1.67070784862985d-11 )*y-4.42671979311163d-10 )*y+ &
                 6.77368055908400d-09 )*y-7.03520999708859d-08 )*y+ &
                 6.04993294708874d-07 )*y-7.80555094280483d-06 )*y+ &
                 2.85954806605017d-04
       self%w(5)=((((((((((((-5.63938733073804d-21*y+6.92182516324628d-20)*y- &
                 1.586937691507d-18)*y+3.357639744582d-17)*y- &
                 4.810285046442d-16)*y+5.386312669975d-15)*y- &
                 6.117895297439d-14)*y+8.441808227634d-13)*y- &
                 1.18527596836592d-11 )*y+1.36296870441445d-10 )*y- &
                 1.17842611094141d-09 )*y+7.80430641995926d-09 )*y- &
                 5.97767417400540d-08 )*y+1.65186146094969d-06
     else
       self%w(1)=sqrt_pie4/sqrt(x)
       if (x < 40.0d-00) then
         e=exp(-x)
         self%r(1)=((((((((-1.73363958895356d-06*x+1.19921331441483d-04)*x - &
                 1.59437614121125d-02)*x+1.13467897349442d+00)*x - &
                 4.47216460864586d+01)*x+1.06251216612604d+03)*x - &
                 1.52073917378512d+04)*x+1.20662887111273d+05)*x - &
                 4.07186366852475d+05)*e + r15/(x-r15)
         self%r(2)=((((((((-1.60102542621710d-05*x+1.10331262112395d-03)*x - &
                 1.50043662589017d-01)*x+1.05563640866077d+01)*x - &
                 4.10468817024806d+02)*x+9.62604416506819d+03)*x - &
                 1.35888069838270d+05)*x+1.06107577038340d+06)*x - &
                 3.51190792816119d+06)*e + r25/(x-r25)
         self%r(3)=((((((((-4.48880032128422d-05*x+2.69025112122177d-03)*x - &
                 4.01048115525954d-01)*x+2.78360021977405d+01)*x - &
                 1.04891729356965d+03)*x+2.36985942687423d+04)*x - &
                 3.19504627257548d+05)*x+2.34879693563358d+06)*x - &
                 7.16341568174085d+06)*e + r35/(x-r35)
         self%r(4)=((((((((-6.38526371092582d-05*x-2.29263585792626d-03)*x - &
                 7.65735935499627d-02)*x+9.12692349152792d+00)*x - &
                 2.32077034386717d+02)*x+2.81839578728845d+02)*x + &
                 9.59529683876419d+04)*x-1.77638956809518d+06)*x + &
                 1.02489759645410d+07)*e + r45/(x-r45)
         self%r(5)=((((((((-3.59049364231569d-05*x-2.25963977930044d-02)*x + &
                 1.12594870794668d+00)*x-4.56752462103909d+01)*x + &
                 1.05804526830637d+03)*x-1.16003199605875d+04)*x - &
                 4.07297627297272d+04)*x+2.22215528319857d+06)*x - &
                 1.61196455032613d+07)*e + r55/(x-r55)
         self%w(5)=(((((((((-4.61100906133970d-10*x+1.43069932644286d-07)*x - &
                 1.63960915431080d-05)*x+1.15791154612838d-03)*x - &
                 5.30573476742071d-02)*x+1.61156533367153d+00)*x - &
                 3.23248143316007d+01)*x+4.12007318109157d+02)*x - &
                 3.02260070158372d+03)*x+9.71575094154768d+03)*e + w55*self%w(1)
         self%w(4)=(((((((((-2.40799435809950d-08*x+8.12621667601546d-06)*x - &
                 9.04491430884113d-04)*x+6.37686375770059d-02)*x - &
                 2.96135703135647d+00)*x+9.15142356996330d+01)*x - &
                 1.86971865249111d+03)*x+2.42945528916947d+04)*x - &
                 1.81852473229081d+05)*x+5.96854758661427d+05)*e + w45*self%w(1)
         self%w(3)=(((((((( 1.83574464457207d-05*x-1.54837969489927d-03)*x + &
                 1.18520453711586d-01)*x-6.69649981309161d+00)*x + &
                 2.44789386487321d+02)*x-5.68832664556359d+03)*x + &
                 8.14507604229357d+04)*x-6.55181056671474d+05)*x + &
                 2.26410896607237d+06)*e + w35*self%w(1)
         self%w(2)=(((((((( 2.77778345870650d-05*x-2.22835017655890d-03)*x + &
                 1.61077633475573d-01)*x-8.96743743396132d+00)*x + &
                 3.28062687293374d+02)*x-7.65722701219557d+03)*x + &
                 1.10255055017664d+05)*x-8.92528122219324d+05)*x + &
                 3.10638627744347d+06)*e + w25*self%w(1)
         self%w(1)=self%w(1)-0.01962d+00*e-self%w(2)-self%w(3)-self%w(4)-self%w(5)
       else if (x < 59.0d-00) then
         xx=x**3
         e=xx*exp(-x)
         self%r(1)=(((-2.43758528330205d-02*x+2.07301567989771d+00)*x - &
                 6.45964225381113d+01)*x+7.14160088655470d+02)*e + r15/(x-r15)
         self%r(2)=(((-2.28861955413636d-01*x+1.93190784733691d+01)*x - &
                 5.99774730340912d+02)*x+6.61844165304871d+03)*e + r25/(x-r25)
         self%r(3)=(((-6.95053039285586d-01*x+5.76874090316016d+01)*x - &
                 1.77704143225520d+03)*x+1.95366082947811d+04)*e + r35/(x-r35)
         self%r(4)=(((-1.58072809087018d+00*x+1.27050801091948d+02)*x - &
                 3.86687350914280d+03)*x+4.23024828121420d+04)*e + r45/(x-r45)
         self%r(5)=(((-3.33963830405396d+00*x+2.51830424600204d+02)*x - &
                 7.57728527654961d+03)*x+8.21966816595690d+04)*e + r55/(x-r55)
         e=xx*e
         self%w(5)=(( 1.35482430510942d-08*x-3.27722199212781d-07)*x + &
                 2.41522703684296d-06)*e + w55*self%w(1)
         self%w(4)=(( 1.23464092261605d-06*x-3.55224564275590d-05)*x + &
                 3.03274662192286d-04)*e + w45*self%w(1)
         self%w(3)=(( 1.34547929260279d-05*x-4.19389884772726d-04)*x + &
                 3.87706687610809d-03)*e + w35*self%w(1)
         self%w(2)=(( 2.09539509123135d-05*x-6.87646614786982d-04)*x + &
                 6.68743788585688d-03)*e + w25*self%w(1)
         self%w(1)=self%w(1)-self%w(2)-self%w(3)-self%w(4)-self%w(5)
       else
         self%r(1)=r15/(x-r15)
         self%r(2)=r25/(x-r25)
         self%r(3)=r35/(x-r35)
         self%r(4)=r45/(x-r45)
         self%r(5)=r55/(x-r55)
         self%w(2)=w25*self%w(1)
         self%w(3)=w35*self%w(1)
         self%w(4)=w45*self%w(1)
         self%w(5)=w55*self%w(1)
         self%w(1)=self%w(1)-self%w(2)-self%w(3)-self%w(4)-self%w(5)
       end if
     end if

   end subroutine

   subroutine get_weights6(self,x)
    type(rys_type) :: self
    ! For 6 or more roots.
     intent(inout) :: self
      real(kind=kind(1.0d0)), intent(in) :: x
     real(kind=kind(1.0d0)), dimension(self%nroots+1,self%nroots+1) :: c,s
     real(kind=kind(1.0d0)), dimension(self%nroots,self%nroots) :: rr,ww
     real(kind=kind(1.0d0)), dimension(2*self%nroots+1) :: ff
     real(kind=kind(1.0d0)), dimension(self%nroots+1) :: a,rt
     real(kind=kind(1.0d0)) :: dum, poly, root, wsum
     integer(kind=kind(1)) :: n, n1, nn, i, j, j1, k, k1, jmax, m

     n=self%nroots

     if(n < 2) n=2
     n1=n+1
     nn=n+n
     call rysfun_(self,x,nn,ff)

     do i=1,n1
       do j=1,n1
         s(i,j)=ff(i+j-1)
       end do
     end do
     call ryssmt_(self,c,s,n1)

     do i=1,n
       do j=1,i
         ww(i,j)= 0.0d0
         rr(i,j)= 0.0d0
       end do
     end do
     wsum=ff(1)
     ww(1,1)=wsum
     rr(1,1)=ff(2)/wsum
     dum= sqrt(c(2,3)**2-4.0d0 *c(1,3)*c(3,3))
     rr(1,2)= 0.50d0*(-c(2,3)-dum)/c(3,3)
     rr(2,2)= 0.50d0*(-c(2,3)+dum)/c(3,3)
     if (n /= 2) then
       do i=3,n1
         rt(i)= 1.0d0
       end do
       rt(1)=rr(1,2)
       rt(2)=rr(2,2)

       do k=3,n
         k1=k+1
         do i=1,k1
           a(i)=c(i,k1)
         end do
         call rysnod_(self,a,rt,k)
         do i=1,k
           rr(i,k)=rt(i)
         end do
       end do
     end if

     do k=2,n
       jmax=k-1
       do i=1,k
         root=rr(i,k)
         dum=1.0d0/ff(1)
         do j=1,jmax
           j1=j+1
           poly=c(j1,j1)
           do m=1,j
             poly=poly*root+c(j1-m,j1)
           end do
           dum=dum+poly*poly
         end do
         ww(i,k)=1.0d0/dum
       end do
     end do

     do k=1,self%nroots
       dum=rr(k,self%nroots)
      self%r(k)=dum/(1.0d0-dum)
      self%w(k)=ww(k,self%nroots)
     end do

   end subroutine

   pure subroutine rysfun(self,x,n,ff)
    type(rys_type) :: self
    !
     intent(in) :: self
     real(kind=kind(1.0d0)), intent(in) :: x
     integer(kind=kind(1)), intent(in) :: n
     real(kind=kind(1.0d0)), dimension(:), intent(out) :: ff
     real(kind=kind(1.0d0)) :: esave, tmax, tol, xx, fac, facmin, term, sumem
     real(kind=kind(1.0d0)) :: a, e, s, t
     integer(kind=kind(1)) :: i, m

     tol=1.0d-14
     xx=x+x
     facmin=xx

     if (facmin < 360.0d+00) then
          e=exp(-x)
     else
          e=0.0d0
     end if

     if (facmin < 80.0d+00) then
       term=1.0d0
       sumem=1.0d0
       fac=n
       fac=fac+0.50d0

       do
         fac=fac+1.0d0
         term= term*x/fac
         sumem=sumem+term
         if (fac > facmin) then
           t=term
           s=sumem
           if (t < s*tol) exit
         end if
       end do

       fac=n+n+1
       ff(n+1)=sumem*e/fac
       m=n-1
       fac=m+m+1

       do
         if (m<0) exit
         ff(m+1)=(e+xx*ff(m+2))/fac
         m=m-1
         fac=fac-2.0d0
       end do

     else
       if (e == 0.0d0) then
         esave=1.0d-35
         do i=1,10
           e=esave*0.10d0
           if (e == 0.0d0) exit
           esave=e
         end do
         e=esave
       end if

       a=sqrt(.7853981633974483096156608d+00/x)
       tmax=a*tol/e
       term=1.0d0/xx
       sumem=term
       fac=1.0d0

       do
         fac=fac-2.0d0
         term=fac*term/xx
         sumem=term+sumem
         t=term
         if (abs(t) < tmax) exit
       end do

       ff(1)=a-e*sumem
       fac=-1.0d0
       m=0

       do
         if (m==n) exit
         m=m+1
         fac=fac+2.0d0
         ff(m+1)=(fac*ff(m)-e)/xx
       end do

     end if

   end subroutine

   subroutine rysnod(self,a,rt,k)
    type(rys_type) :: self
    !
     intent(in) :: self
      real(kind=kind(1.0d0)), dimension(:), intent(in) :: a
     real(kind=kind(1.0d0)), dimension(:), intent(inout) :: rt
      integer(kind=kind(1)), intent(in) :: k
     real(kind=kind(1.0d0)) :: p1,p2,p3,p4,p5,p6,r1,r2,r3,r4,r5,r6,r
     real(kind=kind(1.0d0)) :: dr, tol, prod
     integer(kind=kind(1)) :: k1, i, m

     tol=1.0d-11
     k1=k+1
     r2=0.0d0
     p2=a(1)
     do m=1,k
       r1=r2
       p1=p2
       r2=rt(m)
       p2=a(k1)
       do i=1,k
         p2=p2*r2+a(k1-i)
       end do
       prod=p1*p2
       call die_if_(tonto,prod >= 0.0d0,"RYS:rysnod ... Rys root not found")
       r5=r1
       p5=p1
       r6=r2
       p6=p2
       do
         r3=r5
         p3=p5
         r4=r6
         p4=p6
         r =(r3*p4-r4*p3)/(p4-p3)
         dr=r4-r3
         if (abs(dr) < tol) exit
         dr=0.0625d+00*dr
         r5=r-dr
         if (r5 < r3) r5=r3
         r6=r+dr
         if (r6 > r4) r6=r4
         p5=a(k1)
         p6=p5
         do i=1,k
           p5=p5*r5+a(k1-i)
           p6=p6*r6+a(k1-i)
         end do
         do
           prod=p5*p6
           if (prod < 0.0d0) exit
           prod=p3*p5
           if (prod < 0.0d0) then
             r5=0.25d+00*r3+0.75d+00*r5
             p5=a(k1)
             do i=1,k
               p5=p5*r5 + a(k1-i)
             end do
           else
             r6=0.25d+00*r4 + 0.75d+00*r6
             p6=a(k1)
             do i=1,k
               p6=p6*r6 + a(k1-i)
             end do
           end if
         end do
       end do
       rt(m)=r
     end do

   end subroutine

  subroutine ryssmt(self,c,s,n)
    type(rys_type) :: self
   !
    intent(in) :: self
    real(kind=kind(1.0d0)), dimension(:,:), intent(out) :: c
    real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: s
    integer(kind=kind(1)), intent(in) :: n
    integer(kind=kind(1)) :: i,j,k,kmax
    real(kind=kind(1.0d0)), dimension(n) :: v,y
    real(kind=kind(1.0d0)) :: dot, fac

    do i=1,n
      do j=1,i
        c(i,j)= 0.0d0
      end do
    end do

    do j=1,n
      kmax=j-1
      fac=s(j,j)
      if (kmax /= 0) then
        do k=1,kmax
          v(k)= 0.0d0
          y(k)=s(k,j)
        end do
        do k=1,kmax
          dot= 0.0d0
          do i=1,k
            dot=c(i,k)*y(i)+dot
          end do
          do i=1,k
            v(i)=v(i)-dot*c(i,k)
          end do
          fac=fac-dot*dot
        end do
      end if
      call die_if_(tonto,fac < 0.0d0,"RYS:ryssmt ... Tried to square root a negative number")
      if (fac /= 0.0d0) then
        fac=1.0d0/sqrt(fac)
      else
        fac=1.0d+25
      end if

      c(j,j)=fac
      if (kmax /= 0) then
        do k=1,kmax
          c(k,j)=fac*v(k)
        end do
      end if
    end do

  end subroutine

end