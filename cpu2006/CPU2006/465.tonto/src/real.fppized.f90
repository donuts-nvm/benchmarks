!---------------------------------------------------------------------------
!
!  REAL: methods which apply to double precision numbers
!
! Copyright (C) Daniel Grimwood, 1999
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
! $Id: real.foo,v 1.31.2.2 2003/11/13 05:36:07 reaper Exp $
!---------------------------------------------------------------------------

module REAL_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

   use INT_MODULE, only: hermite_polynomial_

   use STR_MODULE, only: is_known_unit_
   use STR_MODULE, only: left_justify_
   use STR_MODULE, only: conversion_factor_

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

   public    harmonic_vibrational_fn_
   interface harmonic_vibrational_fn_
      module procedure harmonic_vibrational_fn
   end interface

   public    from_units_
   interface from_units_
      module procedure from_units
   end interface

   public    to_str_
   interface to_str_
      module procedure to_str
   end interface

   public    same_as_
   interface same_as_
      module procedure same_as
   end interface

   public    hermite_polynomial_
   interface hermite_polynomial_
      module procedure hermite_polynomial
   end interface

   public    convert_to_
   interface convert_to_
      module procedure convert_to
   end interface

   public    is_zero_
   interface is_zero_
      module procedure is_zero
   end interface

   public    plus_
   interface plus_
      module procedure plus
   end interface

   public    is_int_
   interface is_int_
      module procedure is_int
   end interface

   public    convert_from_
   interface convert_from_
      module procedure convert_from
   end interface

   public    raised_to_
   interface raised_to_
      module procedure raised_to
   end interface

   public    integral_
   interface integral_
      module procedure integral
   end interface

   public    minimise_bisect_
   interface minimise_bisect_
      module procedure minimise_bisect
   end interface

   public    arcsin_
   interface arcsin_
      module procedure arcsin
   end interface

   public    minimise_brent_
   interface minimise_brent_
      module procedure minimise_brent
   end interface

   public    minus_
   interface minus_
      module procedure minus
   end interface

   public    integrate_adaptive_simpson_
   interface integrate_adaptive_simpson_
      module procedure integrate_adaptive_simpson
   end interface

   public    to_str_no_zeros_
   interface to_str_no_zeros_
      module procedure to_str_no_zeros
      module procedure to_str_no_zeros_1
   end interface

   public    to_random_normal_
   interface to_random_normal_
      module procedure to_random_normal
   end interface

   public    arcsinh_
   interface arcsinh_
      module procedure arcsinh
   end interface

   public    fermi_population_
   interface fermi_population_
      module procedure fermi_population
   end interface

   public    test_
   interface test_
      module procedure test
   end interface

   public    equals_
   interface equals_
      module procedure equals
   end interface

   public    swap_with_
   interface swap_with_
      module procedure swap_with
   end interface

   public    bracket_minimum_
   interface bracket_minimum_
      module procedure bracket_minimum
   end interface

   public    find_root_brent_
   interface find_root_brent_
      module procedure find_root_brent
   end interface

   public    integrate_adaptive_trapezoid_
   interface integrate_adaptive_trapezoid_
      module procedure integrate_adaptive_trapezoid
   end interface

   public    z_from_p_
   interface z_from_p_
      module procedure z_from_p
   end interface

   public    minimise_golden_
   interface minimise_golden_
      module procedure minimise_golden
   end interface

   public    to_units_
   interface to_units_
      module procedure to_units
   end interface

   public    arccos_
   interface arccos_
      module procedure arccos
   end interface

   public    is_in_range_
   interface is_in_range_
      module procedure is_in_range
   end interface

   public    is_convertible_to_
   interface is_convertible_to_
      module procedure is_convertible_to
   end interface

   public    times_
   interface times_
      module procedure times
   end interface

   public    bracket_root_
   interface bracket_root_
      module procedure bracket_root
   end interface

contains

   pure subroutine plus(self,val)
    real(kind=kind(1.0d0)) :: self
    ! Add "val" to self
      intent(inout) :: self
      real(kind=kind(1.0d0)), intent(in) :: val
      self = self + val

   end subroutine

   pure subroutine minus(self,val)
    real(kind=kind(1.0d0)) :: self
    ! Subtract "val" to self
      intent(inout) :: self
      real(kind=kind(1.0d0)), intent(in) :: val
      self = self - val

   end subroutine

   pure subroutine times(self,val)
    real(kind=kind(1.0d0)) :: self
    ! Multiply "val" by self
      intent(inout) :: self
      real(kind=kind(1.0d0)), intent(in) :: val
      self = self * val

   end subroutine

   pure function to_str(self,format) result(string)
    real(kind=kind(1.0d0)) :: self
    ! Change self to a "string" using default format, or the specified fortran
    ! "format", if present.
      intent(in) :: self
      character(*), intent(in), optional :: format
      character(128) :: string
      string = " "
      if (present(format)) then; write(string,fmt="("//trim(format)//")") self
      else;                      write(string,fmt=*) self
      end if
      call left_justify_(string)

   end function

   pure function to_str_no_zeros(self) result(string)
    real(kind=kind(1.0d0)) :: self
    ! Change self to a "string" but remove trailing zeros
      intent(in) :: self
      character(128) :: string
      integer(kind=kind(1)) :: i
      string = " "
      write(string,fmt=*) self
      string=adjustl(string)
      do i = len_trim(string),1,-1
          if (string(i:i)=="0") then
            string(i:i)=" "
          else if (string(i:i)==".") then
            string(i:i)=" "
            exit
          else
            exit
          end if
      end do

   end function

   pure function to_str_no_zeros_1(self,format) result(string)
    real(kind=kind(1.0d0)) :: self
    ! Change self to a "string" using specified format "format"
    ! but remove trailing zeros
      intent(in) :: self
      character(*), intent(in) :: format
      character(128) :: string
      integer(kind=kind(1)) :: i,n
      string = " "
      write(string,fmt="("//trim(format)//")") self
      string=adjustl(string)
      n = index(string,achar(46))
      if (n/=0) then
        do i = len_trim(string),n,-1
          if (string(i:i)=="0") then
            string(i:i)=" "
          else
            exit
          end if
        end do
      end if

   end function

   pure function equals(self,x,eps) result(res)
    real(kind=kind(1.0d0)) :: self
    ! Return .true. is self is near enough to "x". If present, "eps"
    ! defines how close before the number is considered the same.
      intent(in) :: self
      real(kind=kind(1.0d0)), intent(in) :: x
      real(kind=kind(1.0d0)), intent(in), optional :: eps
      logical(kind=kind(.true.)) :: res
      real(kind=kind(1.0d0)) :: tol
      tol = 10.0d0**(-6)
      if (present(eps)) tol = abs(eps)
      res = abs(self-x) < tol

   end function

   pure function same_as(self,x,eps) result(res)
    real(kind=kind(1.0d0)) :: self
    ! Return .true. is self is near enough to "x". If present, "eps"
    ! defines how close before the number is considered the same.
      intent(in) :: self
      real(kind=kind(1.0d0)), intent(in) :: x
      real(kind=kind(1.0d0)), intent(in), optional :: eps
      logical(kind=kind(.true.)) :: res
      res = equals_(self,x,eps)

   end function

   pure function is_zero(self,eps) result(res)
    real(kind=kind(1.0d0)) :: self
    ! Return .true. if self is near enough zero. If present, "eps" defines how
    ! close to zero before the number is considered zero.
      intent(in) :: self
      real(kind=kind(1.0d0)), intent(in), optional :: eps
      logical(kind=kind(.true.)) :: res
      real(kind=kind(1.0d0)) :: tol
      tol = 10.0d0**(-6)
      if (present(eps)) tol = abs(eps)
      res = abs(self) < tol

   end function

   pure function is_int(self,eps) result(res)
    real(kind=kind(1.0d0)) :: self
    ! Return .true. if self is near enough to an integer. If present, "eps" defines
    ! how close to zero the non-integer part is before the number is considered
    ! an integer.
      intent(in) :: self
      real(kind=kind(1.0d0)), intent(in), optional :: eps
      logical(kind=kind(.true.)) :: res
      real(kind=kind(1.0d0)) :: val
      val = self - int(self)
      res = is_zero_(val,eps)

   end function

   pure function is_in_range(self,range) result(res)
    real(kind=kind(1.0d0)) :: self
    ! Return .true. if self is within the specified "range".
      intent(in) :: self
      real(kind=kind(1.0d0)), dimension(2), intent(in) :: range
      logical(kind=kind(.true.)) :: res
      res = range(1) <= self .and. self <= range(2)

   end function

   pure function raised_to(self,n) result(res)
    real(kind=kind(1.0d0)) :: self
    ! Raise "self" to the power "n"
      intent(in) :: self
      integer(kind=kind(1)), intent(in) :: n
      real(kind=kind(1.0d0)) :: res
       integer(kind=kind(1)) :: i
      res = 1.0d0
      do i = 1,abs(n)
         res = res*self
      end do
      if (n<0) res = 1.0d0/res

   end function

   pure function arccos(self) result(res)
    real(kind=kind(1.0d0)) :: self
    ! Return the arccosine for self. Corrects bug for numbers close to 1.
      intent(in) :: self
      real(kind=kind(1.0d0)) :: res
      if (abs(abs(self)-1.0d0)<10.0d0**(-5)) then
         if (self<0) then; res = 3.141592653589793d0
         else;             res = 0.0d0
         end if
      else
         res = acos(self)
      end if

   end function

   pure function arcsin(self) result(res)
    real(kind=kind(1.0d0)) :: self
    ! Return the arcsine for self. Corrects bug for numbers close to 1.
      intent(in) :: self
      real(kind=kind(1.0d0)) :: res
      if (abs(abs(self)-1.0d0)<10.0d0**(-5)) then
         if (self<0) then; res = -3.141592653589793d0/2.0d0
         else;             res = +3.141592653589793d0/2.0d0
         end if
      else
         res = asin(self)
      end if

   end function

   pure function arcsinh(self) result(res)
    real(kind=kind(1.0d0)) :: self
    ! Return the arcsinh of self. Corrects bug for numbers close to 1.
    ! Note that self can be any real number.
      intent(in) :: self
      real(kind=kind(1.0d0)) :: res
      res = log(self + sqrt(1.0d0+self*self))

   end function

   function hermite_polynomial(self,n,normalise) result(res)
    real(kind=kind(1.0d0)) :: self
    ! Return "res", the value of the "n"th hermite polynomial H_n(self).
    ! If present and .true., "normalise" gives values normalised for use
    ! in generating harmonic vibrational wavefunctions. See below.
      intent(in) :: self
      integer(kind=kind(1)), intent(in) :: n
      logical(kind=kind(.true.)), intent(in), optional :: normalise
      real(kind=kind(1.0d0)) :: res
      real(kind=kind(1.0d0)), dimension(:), pointer :: coeff
      real(kind=kind(1.0d0)) :: x
      integer(kind=kind(1)) :: i

      coeff => hermite_polynomial_(n,normalise)
      res = 0.0d0
      x = 1.0d0
      do i = 0,n
         res = res + coeff(i+1)*x
         x = x*self
      end do
      deallocate(coeff)

   end function

   function harmonic_vibrational_fn(self,n) result(res)
    real(kind=kind(1.0d0)) :: self
    ! Return "res", the value of the "n"th harmonic vibrational wavefunction
    ! as a function of the dimensionless normal coordinate "self"
      intent(in) :: self
      integer(kind=kind(1)), intent(in) :: n
      real(kind=kind(1.0d0)) :: res
      real(kind=kind(1.0d0)) :: q

      q = self
    !  fac = sqrt( 1.0d0/ (sqrt(3.141592653589793d0) * 2**n * n.factorial) )
      res = hermite_polynomial_(q,n,normalise=.true.)*exp(-0.50d0*q*q)

   end function

   recursive function integral(self,a,b,accuracy) result(res)
    ! Integrate the function "self" between the limits "a" and "b"
    ! using adaptive trapezoidal rule with Simpsons approximation acceleration.
    ! If present, "accuracy" is the required accuracy of the integral.
      interface
         function self(x) result(res)
            real(kind=kind(1.0d0)), intent(in) :: x
            real(kind=kind(1.0d0)) :: res
         end function
      end interface
      real(kind=kind(1.0d0)), intent(in) :: a,b
      real(kind=kind(1.0d0)), intent(in), optional :: accuracy
      real(kind=kind(1.0d0)) :: res

      res = integrate_adaptive_trapezoid_(self,a,b,accuracy)

   end function

   recursive function integrate_adaptive_trapezoid(self,a,b,accuracy) result(res)
    ! Integrate the function "self" between the limits "a" and "b"
    ! using adaptive trapezoidal rule with Simpsons approximation acceleration.
    ! If present, "accuracy" is the required accuracy of the integral.
      interface
         function self(x) result(res)
            real(kind=kind(1.0d0)), intent(in) :: x
            real(kind=kind(1.0d0)) :: res
         end function
      end interface
      real(kind=kind(1.0d0)), intent(in) :: a,b
      real(kind=kind(1.0d0)), intent(in), optional :: accuracy
      real(kind=kind(1.0d0)) :: res
      real(kind=kind(1.0d0)) :: tol,h,m,fa,fb,fm,one_trap,two_trap,left,right

      tol = 10.0d0**(-6)
      if (present(accuracy)) tol = accuracy
      h  = b-a
      m  = (a+b)/2.0d0
      fa = self(a)
      fb = self(b)
      fm = self(m)
      one_trap = h*(fa+fb)/2.0d0
      two_trap = h*(fa+2.0d0*fm+fb)/4.0d0
      if (abs(one_trap-two_trap)<3.0d0*tol) then
         res = (4.0d0*two_trap - one_trap)/3.0d0
      else
         left  = integrate_adaptive_trapezoid_(self,a,m,tol/2.0d0)
         right = integrate_adaptive_trapezoid_(self,m,b,tol/2.0d0)
         res = left + right
      end if

   end function

   recursive function integrate_adaptive_simpson(self,a,b,accuracy) result(res)
    ! Integrate the function "self" between the limits "a" and "b"
    ! using adaptive Simpson rule with acceleration.
    ! If present, "accuracy" is the required accuracy of the integral.
      interface
         function self(x) result(res)
            real(kind=kind(1.0d0)), intent(in) :: x
            real(kind=kind(1.0d0)) :: res
         end function
      end interface
      real(kind=kind(1.0d0)), intent(in) :: a,b
      real(kind=kind(1.0d0)), intent(in), optional :: accuracy
      real(kind=kind(1.0d0)) :: res
      real(kind=kind(1.0d0)) :: tol,h,m1,m2,m3,fa,fb,f1,f2,f3,s1,s2,left,right

      tol = 10.0d0**(-6)
      if (present(accuracy)) tol = accuracy
      h   = (b-a)/2.0d0
      m1  = (3.0d0*a+b)/4.0d0
      m2  = (a+b)/2.0d0
      m3  = (a+3.0d0*b)/4.0d0
      fa = self(a)
      fb = self(b)
      f1 = self(m1)
      f2 = self(m2)
      f3 = self(m3)
      s1 = h*(fa+4.0d0*f2+fb)/3.0d0
      s2 = h*(fa+4.0d0*f1+2.0d0*f2+4.0d0*f3+fb)/6.0d0
      if (abs(s1-s2)<15.0d0*tol) then
         res = (16.0d0*s2 - s1)/15.0d0
      else
         left  = integrate_adaptive_simpson_(self,a,m2,tol/2.0d0)
         right = integrate_adaptive_simpson_(self,m2,b,tol/2.0d0)
         res = left + right
      end if

   end function

   pure function fermi_population(self,E_fermi,temperature) result(res)
    real(kind=kind(1.0d0)) :: self
    ! Returns the population of the level with energy "self".
    ! Input energies in Hartrees, temperature in Kelvin!!!!!
     intent(in) :: self
     real(kind=kind(1.0d0)), intent(in) :: E_fermi,temperature
     real(kind=kind(1.0d0)) :: res
     real(kind=kind(1.0d0)) :: de,temp,de_on_temp
     de = (self - E_fermi) * 4.3597482d-18
     temp = temperature * 1.38066d-23
     de_on_temp = de/temp
     if (de_on_temp > 500.0d0) then
       res = 0.0d0
     else
       if (de_on_temp < (-500.0d0)) then
         res = 1.0d0
       else
         res = 1.0d0 / (2.718281828459045d0**(de_on_temp) + 1.0d0)
         if (res < 1d-20) res = 0.0d0
         if (res > (1 - 1d-20)) res = 1.0d0
       end if
     end if

   end function

   subroutine minimise_bisect(self,val,previous,kept,tolerance,delta,done)
    real(kind=kind(1.0d0)) :: self
    ! Returns new "self" which should be better than old "self".  Do at least
    ! twice to be sure.  Works by the bisection method.
    ! Dummy variable "done" is returned true if it has converged.
    ! "previous" s automatically created and destroyed.
    ! row 1 is for y, row 2 is for x, for y=f(x).
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: previous
     real(kind=kind(1.0d0)), intent(in) :: val,tolerance,delta
     integer(kind=kind(1)), intent(inout) :: kept
     logical(kind=kind(.true.)), intent(inout) :: done
     real(kind=kind(1.0d0)) :: x1,x2,x3,y1,y2,y3

     done = .false.
     if (.not. associated(previous)) then
       allocate(previous(3,2))
       kept = 0
     else
       call ensure_(tonto,size(previous,1) == 3,"REAL:minimise_bisect ... matrix has wrong dimension")
       call ensure_(tonto,size(previous,2) == 2,"REAL:minimise_bisect ... matrix has wrong dimension")
       call ensure_(tonto,kept <= 3 .and. kept >=0,"REAL:minimise_bisect ... invalid value for variable 'kept'")
     end if
     select case (kept)
       case (0);      kept = 1
         previous(:,1) = (/val ,0.0d0,0.0d0/)
         previous(:,2) = (/self,0.0d0,0.0d0/)
         self = self + delta
         done = .false.
       case (1);      kept = 2
         x1 = previous(1,2);      y1 = previous(1,1)
         if (self<x1) then
           previous(:,1) = (/val ,y1,0.0d0/)
           previous(:,2) = (/self,x1,0.0d0/)
           self = self - (x1-self)
         else
           previous(:,1) = (/y1,val ,0.0d0/)
           previous(:,2) = (/x1,self,0.0d0/)
           self = self + (self-x1)
         end if
         x1 = previous(1,2);      y1 = previous(1,1)
         x2 = previous(2,2);      y2 = previous(2,1)
         if (y1<y2) then
           self = x1 - delta
         else
           self = x2 + delta
         end if
         done = .false.
       case (2);      kept = 3
         x1 = previous(1,2);      y1 = previous(1,1)
         x2 = previous(2,2);      y2 = previous(2,1)
         if (self<x1) then
           previous(:,1) = (/val ,y1,y2/)
           previous(:,2) = (/self,x1,x2/)
         else if (self>x2) then
           previous(:,1) = (/y1,y2,val /)
           previous(:,2) = (/x1,x2,self/)
         else
           previous(:,1) = (/y1,val ,y2/)
           previous(:,2) = (/x1,self,x2/)
         end if
         x1 = previous(1,2);      y1 = previous(1,1)
         x2 = previous(2,2);      y2 = previous(2,1)
         x3 = previous(3,2);      y3 = previous(3,1)

         if (y2<y1) then
           if (y2<y3) then
             if (y1<y3) then
               self = (x1+x2)/2
             else
               self = (x2+x3)/2
             end if
           else
             self = x3 + 0.50d0*(x3-x1)
           end if
         else  !y1<y2
           self = x1 - 0.50d0*(x3-x1)
         end if

         x1 = abs(previous(1,2)-self)
         x2 = abs(previous(2,2)-self)
         x3 = abs(previous(3,2)-self)
         y1 = max(x1,x2,x3)
         if (y1 < tolerance) then
           done = .true.
           deallocate(previous)
         end if
       case (3)
         x1 = previous(1,2);      y1 = previous(1,1)
         x2 = previous(2,2);      y2 = previous(2,1)
         x3 = previous(3,2);      y3 = previous(3,1)
         if (self<x1) then
           previous(:,1) = (/val ,y1,y2/)
           previous(:,2) = (/self,x1,x2/)
         else if (self>x3) then
           previous(:,1) = (/y2,y3,val /)
           previous(:,2) = (/x2,x3,self/)
         else
           if (self<x2) then
             if (y1<y3) then
               previous(:,1) = (/y1,val ,y2/)
               previous(:,2) = (/x1,self,x2/)
             else
               previous(:,1) = (/y1,y2,val /)
               previous(:,2) = (/x1,x2,self/)
             end if
           else
             if (y1>y3) then
               previous(:,1) = (/y2,val ,y3/)
               previous(:,2) = (/x2,self,x3/)
             else
               previous(:,1) = (/y1,y2,val /)
               previous(:,2) = (/x1,x2,self/)
             end if
           end if
         end if
         x1 = previous(1,2);      y1 = previous(1,1)
         x2 = previous(2,2);      y2 = previous(2,1)
         x3 = previous(3,2);      y3 = previous(3,1)

         if (y2<y1) then
           if (y2<y3) then
             if (y1<y3) then
               self = (x1+x2)/2
             else
               self = (x2+x3)/2
             end if
           else
             self = x3 + 0.50d0*(x3-x1)
           end if
         else  !y1<y2
           self = x1 - 0.50d0*(x3-x1)
         end if

         x1 = abs(previous(1,2)-self)
         x2 = abs(previous(2,2)-self)
         x3 = abs(previous(3,2)-self)
         y1 = max(x1,x2,x3)
         if (y1 < tolerance) then
           done = .true.
           deallocate(previous)
         end if
     end select

   end subroutine

   pure function z_from_p(self) result(res)
    real(kind=kind(1.0d0)) :: self
    ! Produces the normal deviate Z corresponding to "self", a given lower
    ! tail area of P; Z is accurate to about 1 part in 10**16.
    ! Adapted from the Royal Statistical Society f77 routine "PPND16".
    ! algorithm AS241  appl. statist. (1988) vol 37, no 3.
     intent(in) :: self
     real(kind=kind(1.0d0)) :: res
     real(kind=kind(1.0d0)) :: q,r
     q = self - 0.50d0
     if (abs(q) < 0.425D0) then
       r = 0.180625D0 - q * q
       res = q * (((((((2.5090809287301226727D+3 * r + &
       3.3430575583588128105D+4) * r + 6.7265770927008700853D+4) * r + &
       4.5921953931549871457D+4) * r + 1.3731693765509461125D+4) * r + &
       1.9715909503065514427D+3) * r + 1.3314166789178437745D+2) * r + &
       3.3871328727963666080D0) / (((((((5.2264952788528545610D+3 * r + &
       2.8729085735721942674D+4) * r + 3.9307895800092710610D+4) * r + &
       2.1213794301586595867D+4) * r + 5.3941960214247511077D+3) * r + &
       6.8718700749205790830D+2) * r + 4.2313330701600911252D+1) * r + 1.0d0)
     else
       if (q < 0.0d0) then
         r = self
       else
         r = 1.0d0 - self
       end if
       if (r < 0.0d0) then

         res = 0.0d0
       else
         r = sqrt(-log(r))
         if (r < 5.0d0) then
           r = r - 1.6D0
           res = (((((((7.74545014278341407640D-4 * r + &
           2.27238449892691845833D-2) * r + 2.41780725177450611770D-1) * r + &
           1.27045825245236838258D0) * r + 3.64784832476320460504D0) * r + &
           5.76949722146069140550D0) * r + 4.63033784615654529590D0) * r + &
           1.42343711074968357734D0) / (((((((1.05075007164441684324D-9 * r + &
           5.47593808499534494600D-4) * r + 1.51986665636164571966D-2) * r + &
           1.48103976427480074590D-1) * r + 6.89767334985100004550D-1) * r + &
           1.67638483018380384940D0) * r + 2.05319162663775882187D0) * r + 1.0d0)
         else
           r = r - 5.0d0
           res = (((((((2.01033439929228813265D-7 * r + &
           2.71155556874348757815D-5) * r + 1.24266094738807843860D-3) * r + &
           2.65321895265761230930D-2) * r + 2.96560571828504891230D-1) * r + &
           1.78482653991729133580D0) * r + 5.46378491116411436990D0) * r + &
           6.65790464350110377720D0) / (((((((2.04426310338993978564D-15 * r + &
           1.42151175831644588870D-7) * r + 1.84631831751005468180D-5) * r + &
           7.86869131145613259100D-4) * r + 1.48753612908506148525D-2) * r + &
           1.36929880922735805310D-1) * r + 5.99832206555887937690D-1) * r + 1.0d0)
         end if
         if (q < 0.0d0) res = - res
       end if
     end if

   end function

!   s_from_p(p,mean)
!   ! Calculate the Poisson upper limit of cumulation "s" from the cumulation "p".
!   ! self is p, function returns s.
!       p,mean :: real(kind=kind(1.0d0)), intent(in)
!
!C      ..
!C      .. Scalar Arguments ..
!       real(kind=kind(1.0d0)) bound,p,s,xlam
!       INTEGER status,which
!C      ..
!C      .. Local Scalars ..
!       real(kind=kind(1.0d0)) fx,pp,ss,xxlam
!       LOGICAL qhi,qleft
!C      ..
!C      .. External Functions ..
!       real(kind=kind(1.0d0)) cumpoi
!       EXTERNAL cumpoi
!C      ..
!C      .. External Subroutines ..
!       EXTERNAL invr,stinvr
!
!       call ensure_(tonto,p>0.0d0,"p not greater than zero")
!       call ensure_(tonto,p<1.0d0,"p not less than one")
!       call ensure_(tonto,xlam>0.0d0,"xlam not > 0")
!
!           res = 5.0d0
!           CALL stinvr(0.0d0,10.0d0**30,0.50d0,0.50d0,5.0d0,10.0d0**(-4),10.0d0**(-4))
!           status = 0
!           CALL invr(status,res,fx,qleft,qhi)
!           do
!             if (status/=1) exit
!             fx = cumpoi(res,xlam) - p
!             CALL invr(status,res,fx,qleft,qhi)
!           end
!
!           if (status==-1) then
!             if (.not. (qleft)) then
!               status = 2
!               bound = inf
!             else
!               status = 1
!               bound = 0.0
!             end
!           end
!   end

   subroutine convert_to(self,units)
    real(kind=kind(1.0d0)) :: self
    ! Convert the number "self" in atomic units or generic units to a
    ! new number in "units".
      intent(inout) :: self
      character(*), intent(in) :: units
      real(kind=kind(1.0d0)) :: factor

      call ensure_(tonto,is_known_unit_(units),"REAL:convert_to ... unknown units, " // units)
      factor = conversion_factor_(units)
      self = self * factor

   end subroutine

   function to_units(self,units) result(res)
    real(kind=kind(1.0d0)) :: self
    ! Convert the number "self" in atomic units or generic units to a
    ! new number in "units".
      intent(in) :: self
      character(*), intent(in) :: units
      real(kind=kind(1.0d0)) :: res
      real(kind=kind(1.0d0)) :: factor

      call ensure_(tonto,is_known_unit_(units),"REAL:to_units ... unknown units, " // units)
      factor = conversion_factor_(units)
      res = self * factor

   end function

   subroutine convert_from(self,units)
    real(kind=kind(1.0d0)) :: self
    ! Convert the number "self" from "units" system to a new number
    ! in atomic units or generic units.  Returns "err" whether it was successful.
      intent(inout) :: self
      character(*), intent(in) :: units
      real(kind=kind(1.0d0)) :: factor

   call ensure_(tonto,is_known_unit_(units),"REAL:convert_from ... unknown units, " // units)
      factor = 1.0d0/(conversion_factor_(units))
      self = self * factor

   end subroutine

   function from_units(self,units) result(res)
    real(kind=kind(1.0d0)) :: self
    ! Convert the number "self" from "units" system to a new number
    ! in atomic units or generic units.  Returns "err" whether it was successful.
      intent(in) :: self
      character(*), intent(in) :: units
      real(kind=kind(1.0d0)) :: res
      real(kind=kind(1.0d0)) :: factor

   call ensure_(tonto,is_known_unit_(units),"REAL:from_units ... unknown units, " // units)
      factor = 1.0d0/(conversion_factor_(units))
      res = self * factor

   end function

   function is_convertible_to(self,unit) result(res)
    real(kind=kind(1.0d0)) :: self
    ! Return .true. if "unit" is a known unit which can be used for conversion
    ! of "self".
      character(*), intent(in) :: unit
      logical(kind=kind(.true.)) :: res

      res = is_known_unit_(unit)

   end function

   subroutine swap_with(self,x)
    real(kind=kind(1.0d0)) :: self
    ! Swap the value of "self" and "x"
       real(kind=kind(1.0d0)) :: x
      real(kind=kind(1.0d0)) :: keep

      keep = self
      self = x
      x = keep

   end subroutine

! *********************
! Root finding routines
! *********************

   subroutine bracket_root(self,x1,x2,factor,max_it)
    ! Given a function self(x) and initial points "x1" and "x2", bracket a root
    ! of self(x) by expansion. If "factor" is present it is used as the (linear)
    ! interval expansion factor. If "max_it" is present then it is the number of
    ! times the interval is expanded.
      interface
         function self(x) result(res)
            real(kind=kind(1.0d0)) :: x,res
         end function
      end interface
      real(kind=kind(1.0d0)) :: x1,x2
      real(kind=kind(1.0d0)), optional :: factor
      integer(kind=kind(1)), optional :: max_it
      integer(kind=kind(1)) :: j,maxit
      real(kind=kind(1.0d0)) :: f1,f2,fac

      call ensure_(tonto,x1/=x2,"REAL:bracket_root ... non-zero range (x1,x2) required")
      fac = 1.6d0
      if (present(factor)) fac = factor
      maxit = 50
      if (present(max_it)) maxit = max_it
      f1 = self(x1)
      f2 = self(x2)
      do j = 1,maxit
         if (f1*f2<0.0d0) then;   return; end if
         if (abs(f1)<abs(f2)) then
            x1 = x1 + fac*(x1-x2)
            f1 = self(x1)
         else
            x2 = x2 + fac*(x2-x1)
            f2 = self(x2)
         end if
      end do
      call die_(tonto,"REAL:bracket_root ... Exceeded maximum number of iterations")

   end subroutine

   subroutine find_root_brent(self,x1,x2,root,tol,val,max_it)
    ! Given a function self(x) and initial points "x1" and "x2" which bracket a
    ! root of self, find the "root" to a precision "tol". If "val" present, then
    ! the root which makes self have this value is returned (an isovalue).
    ! If "max_it" is present it is set to the maximum number of iterations.
      interface
         function self(x) result(res)
            real(kind=kind(1.0d0)) :: x,res
         end function
      end interface
      real(kind=kind(1.0d0)) :: x1,x2,root,tol
      real(kind=kind(1.0d0)), optional :: val
      integer(kind=kind(1)), optional :: max_it
      integer(kind=kind(1)) :: iter
      real(kind=kind(1.0d0)) :: a,b,c,d,e,fa,fb,fc,p,q,r,s,tol1,xm
      integer(kind=kind(1)) :: maxit = 100
      real(kind=kind(1.0d0)) :: iso = 0.0d0
      real(kind=kind(1.0d0)) :: eps = 10.0d0**(-8)

      if (present(max_it)) maxit = max_it
      if (present(val))    iso = val
      a  = x1
      b  = x2
      fa = self(a) - iso
      fb = self(b) - iso
      call ensure_(tonto,(fa>0 .and. fb>0) .or. (fa<0 .and. fb<0),"REAL:find_root_brent ... root is not bracketed")
      c = b
      fc = fb
      do iter = 1,maxit
         if ((fb>0 .and. fc>0) .or. (fb<0 .and. fc<0)) then
            c = a     ! Rename a,b,c and adjust bounding interval d
            fc = fa
            d = b - a
            e = d
         end if
         if (abs(fc)<abs(fb)) then
            a  = b ; b  = c ; c  = a
            fa = fb; fb = fc; fc = fa
         end if
          ! Test convergence
         tol1 = 2.0d0*eps*abs(b)+0.50d0*tol
         xm = 0.50d0*(c-b)
         if (abs(xm)<=tol1 .or. fb==0.0d0) then
            root = b
              return
         end if
         if (abs(e)>=tol1 .and. abs(fa)>abs(fb)) then
            s = fb/fa  ! Attempt inverse quadratic interpolation
            if (a==c) then
               p = 2.0d0*xm*s
               q = 1.0d0 - s
            else
               q = fa/fc
               r = fb/fc
               p = s*(2.0d0*xm*q*(q-r)-(b-a)*(r-1.0d0))
               q = (q-1.0d0)*(r-1.0d0)*(s-1.0d0)
            end if
            if (p>0.0d0) q = -q
            p = abs(p)
            if (2.0d0*p<min(3.0d0*xm*q-abs(tol1*q),abs(e*q))) then
               e = d   ! Accept interpolation
               d = p/q
            else
               d = xm  ! Interpolation failed, use bisection
               e = d
            end if
         else
            d = xm     ! Bounds decreasing too slowly, use bisection
            e = d
         end if
         a = b; fa = fb
         if (abs(d)> tol1) then; b = b + d
         else;                   b = b+sign(tol1,xm)
         end if
         fb = self(b) - iso
      end do
      call die_(tonto,"REAL:find_root_brent ... maximum iterations exceeded")

   end subroutine

! *********************
! Minimization routines
! *********************

   subroutine bracket_minimum(self,a,b,c,fa,fb,fc)
    ! Given a function self(x) and initial points "a" and "b" search in
    ! the downhill direction and return points "a", "b" and "c" that bracket
    ! a minimum of the function, and return the value of the function
    ! "fa", "fb", and "fc" at these points. NOTE: "c" is not used initially.
      interface
         function self(x) result(res)
            real(kind=kind(1.0d0)) :: x,res
         end function
      end interface
      real(kind=kind(1.0d0)) :: a,b,c,fa,fb,fc
      real(kind=kind(1.0d0)) :: gold = 1.618034
      real(kind=kind(1.0d0)) :: glimit = 100
      real(kind=kind(1.0d0)) :: tiny = 1.0d-20
      real(kind=kind(1.0d0)) :: fu,q,r,u,ulim

      fa = self(a)
      fb = self(b)
      if (fb>fa) then
        call swap_with_(a,b)
        call swap_with_(fa,fb)
      end if
      c  = b + gold*(b-a)
      fc = self(c)
      do
         if (fb<fc) exit                   ! bracket found
         r = (b-a)*(fb-fa)                 ! get u by parabolic extrapolation
         q = (b-c)*(fb-fa)
         u = b - ((b-c)*q-(b-a)*r)/(2.0d0*sign(max(abs(q-r),tiny),q-r))
         ulim = b + glimit*(c-b)
         if ((b-u)*(u-c)>0.0d0) then        ! Parabolic u lies between b and c
            fu = self(u)
            if (fu<fc) then                ! got a minimum between b and c
               a = b; fa = fb
               b = u; fb = fu
               exit
            else if (fu>fb) then           ! got a minimum between a and u
               c = u; fc = fu
               exit
            end if
            u = c + gold*(c-b)             ! parabolic fit no use, so magnify
            fu = self(u)
         else if ((c-u)*(u-ulim)>0) then  ! Fit is between c and its allowed limit
            fu = self(u)
            if (fu<fc) then
               b = c; fb = fc
               c = u; fc = fu
               u = c + gold*(c-b)
               fu = self(u)
            end if
         else if ((u-ulim)*(ulim-c)>0) then
            u = ulim
            fu = self(u)
         else
            u = c + gold*(c-b)             ! magnify
            fu = self(u)
         end if
         a = b; fa = fb
         b = c; fb = fc
         c = u; fc = fu
      end do
      if (a>c) then
         call swap_with_(a,c)
         call swap_with_(fa,fc)
      end if

   end subroutine

   subroutine minimise_golden(self,a,b,c,xmin,f,tol)
    ! Given a function self(x) and initial points "a", "b" and "c"
    ! which bracket a minimum, return the minimum point "xmin" and the
    ! value "f" at the minimum to a precision "tol" using the golden
    ! section search method.
      interface
         function self(x) result(res)
            real(kind=kind(1.0d0)) :: x,res
         end function
      end interface
      real(kind=kind(1.0d0)) :: a,b,c,xmin,f,tol
      real(kind=kind(1.0d0)) :: r = 0.618033399
      real(kind=kind(1.0d0)) :: s,f1,f2,x0,x1,x2,x3

      s = 1.0d0 - r
      x0 = a
      x3 = c
      if (abs(c-b)>abs(b-a)) then
         x1 = b; x2 = b + s*(c-b)
      else
         x2 = b; x1 = b - s*(b-a)
      end if
      f1 = self(x1)
      f2 = self(x2)
      do
         if (abs(x3-x0)<=tol*(abs(x1)+abs(x2))) exit
         if (f2<f1) then
            x0 = x1
            x1 = x2
            x2 = r*x1 + s*x3
            f1 = f2
            f2 = self(x2)
         else
            x3 = x2
            x2 = x1
            x1 = r*x2 + s*x0
            f2 = f1
            f1 = self(x1)
         end if
      end do
      if (f1<f2) then; f = f1; xmin = x1
      else;            f = f2; xmin = x2
      end if

   end subroutine

   subroutine minimise_brent(self,a,b,c,xmin,f,tol)
    ! Given a function self(x) and initial points "a", "b" and "c"
    ! which bracket a minimum, return the minimum point "xmin" and the
    ! value "f" at the minimum to a precision "tol" using Brent's method.
      interface
         function self(x) result(res)
            real(kind=kind(1.0d0)) :: x,res
         end function
      end interface
      real(kind=kind(1.0d0)) :: a,b,c,xmin,f,tol
      integer(kind=kind(1)) :: itmax = 100
      real(kind=kind(1.0d0)) :: cgold = 0.3819660
      real(kind=kind(1.0d0)) :: zeps = 10.0d0**(-10)
      real(kind=kind(1.0d0)) :: d,e,etemp,fu,fv,fw,fx,p,q,r,tol1,tol2,u,v,w,x,xm
      integer(kind=kind(1)) :: iter
      logical(kind=kind(.true.)) :: failed

      if (a>c) call swap_with_(a,c)
      v = b
      b = c
      w = v
      x = v
      fx = self(x)
      fv = fx
      fw = fx
      e = 0.0d0
      failed = .true.
      do iter = 1,itmax
         xm = 0.50d0*(a+b)
         tol1 = tol*abs(x) + zeps
         tol2 = 2.0d0*tol1
         if (abs(x-xm)<(tol2-0.50d0*(b-a))) then
            failed = .false.
            exit
         end if
         if (abs(e)>tol1) then
            r = (x-w)*(fx-fv)
            q = (x-v)*(fx-fw)
            p = (x-v)*q - (x-w)*r
            q = 2.0d0*(q-r)
            if (q>0.0d0) p = -p
            q = abs(q)
            etemp = e
            e = d
            if (abs(p)>=abs(0.50d0*q*etemp) .or. p<=q*(a-x) .or. p>=q*(b-x)) then
              if (x>=xm) then; e = a-x
              else;            e = b-x
              end if
              d = cgold*e
            else
              d = p/q
              u = x + d
              if ((u-a)<tol2 .or. (b-u)<tol2) d = sign(tol1,xm-x)
            end if
         else
           if (x>=xm) then; e = a-x
           else;            e = b-x
           end if
           d = cgold*e
         end if
         if (abs(d)>=tol1) then; u = x + d
         else;                   u = x + sign(tol1,d)
         end if
         fu = self(u)
         if (fu<=fx) then
            if (u>=x) then; a = x
            else;           b = x
            end if
            v = w; fv = fw
            w = x; fw = fx
            x = u; fx = fu
         else
            if (u<x) then; a = u
            else;          b = u
            end if
            if (fu<=fw .or. w==x) then
               v = w; fv = fw
               w = u; fw = fu
            else if (fu<=fv .or. v==x .or. v==w) then
               v = u; fv = fu
            end if
         end if
      end do
      f = fx
      xmin = x
      call ensure_(tonto,.not. failed,"REAL:minimise_brent ... maximum iterations exceeded")

   end subroutine

   function test(self) result(res)
    real(kind=kind(1.0d0)) :: self
    !  A test function for minimising
      real(kind=kind(1.0d0)) :: res
      real(kind=kind(1.0d0)) :: x

      x = self
      res = (x-1)*(x-1) + 1

   end function

   subroutine to_random_normal(self)
    real(kind=kind(1.0d0)) :: self
    ! Set self to be a normal random number.
    ! From 488 in toms from Netlib.
    ! ALGORITHM APPEARED intent(in) COMM. ACM, VOL. 17, NO. 12, P. 704.
     intent(out) :: self
      real(kind=kind(1.0d0)), dimension(60) :: D = (/0.674489750,0.475859630,0.383771164, &
     0.328611323,0.291142827,0.263684322, &
     0.242508452,0.225567444,0.211634166,0.199924267,0.189910758,0.181225181, &
     0.173601400,0.166841909,0.160796729,0.155349717,0.150409384,0.145902577, &
     0.141770033,0.137963174,0.134441762,0.131172150,0.128125965,0.125279090, &
     0.122610883,0.120103560,0.117741707,0.115511892,0.113402349,0.111402720, &
     0.109503852,0.107697617,0.105976772,0.104334841,0.102766012,0.101265052, &
     0.099827234,0.098448282,0.097124309,0.095851778,0.094627461,0.093448407, &
     0.092311909,0.091215482,0.090156838,0.089133867,0.088144619,0.087187293, &
     0.086260215,0.085361834,0.084490706,0.083645487,0.082824924,0.082027847, &
     0.081253162,0.080499844,0.079766932,0.079053527,0.078358781,0.077681899/)
     real(kind=kind(1.0d0)) :: A,W,V
     real(kind=kind(1.0d0)), SAVE :: U
     integer(kind=kind(1)) :: I,J,N
     logical(kind=kind(.true.)), SAVE :: first

     DATA U /0.0d0/
     DATA first /.true./

      ! Note that on the first call, self is returned as zero, so to get around
      ! this, we iterate twice.  On all subsequent calls, iterate only once.
     N=1
     if (first) N=2
     do J=1,N
       A = 0.0d0
       I = 0
       do
         U = U + U
         if (U < 1.0d0) exit
         U = U - 1.0d0
         I = I + 1
         A = A - D(I)
       end do
       outer : do
         W = D(I+1)*U
         V = W*(0.50d0*W-A)
         do
           call random_number(U)
           if (V<=U) exit outer
           call random_number(V)
           if (U<=V) exit
         end do
         U = (V-U)/(1.0d0-U)
       end do outer
       U = (U-V)/(1.0d0-V)
       U = U + U
       if (U >= 1.0d0) then
         U = U - 1.0d0
         self = W - A
       else
         self = A - W
       end if
     end do
     first=.false.

   end subroutine

end

