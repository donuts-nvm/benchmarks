!---------------------------------------------------------------------------
!
!  INTMAT3: 3 dimensional integer matrices
!
! Copyright (C) Dylan Jayatilaka, 1998
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
! $Id: intmat3.foo,v 1.4 2003/02/19 07:48:57 reaper Exp $
!---------------------------------------------------------------------------

module INTMAT3_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

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

   public    make_index_of_components_
   interface make_index_of_components_
      module procedure make_index_of_components
   end interface

   public    create_
   interface create_
      module procedure create
      module procedure create_1
      module procedure create_2
   end interface

   public    to_gaussian_xyz_indices_
   interface to_gaussian_xyz_indices_
      module procedure to_gaussian_xyz_indices
   end interface

   public    destroy_
   interface destroy_
      module procedure destroy
   end interface

contains

   subroutine create(self,dim1,dim2,dim3)
    integer(kind=kind(1)), dimension(:,:,:) :: self
    ! Create the object with the desired bounds
      pointer :: self
      integer(kind=kind(1)), intent(in) :: dim1,dim2,dim3

      nullify(self)
      allocate(self(dim1,dim2,dim3))

   end subroutine

   subroutine create_1(self,lb1,ub1,lb2,ub2,lb3,ub3)
    integer(kind=kind(1)), dimension(:,:,:) :: self
    ! Create the object with the desired bounds
      pointer :: self
      integer(kind=kind(1)), intent(in) :: lb1,lb2,lb3,ub1,ub2,ub3

      nullify(self)
      allocate(self(lb1:ub1,lb2:ub2,lb3:ub3))

   end subroutine

   subroutine create_2(self,bounds1,bounds2,bounds3)
    integer(kind=kind(1)), dimension(:,:,:) :: self
    ! Create the object with the desired bounds
      pointer :: self
      integer(kind=kind(1)), dimension(2) :: bounds1,bounds2,bounds3

      call create_(self,bounds1(1),bounds1(2),bounds2(1),bounds2(2),bounds3(1),bounds3(2))

   end subroutine

   subroutine destroy(self)
    integer(kind=kind(1)), dimension(:,:,:) :: self
    ! Destroy the object
      pointer :: self

      deallocate(self)

   end subroutine

   pure subroutine make_index_of_components(self,components)
    integer(kind=kind(1)), dimension(:,:,:) :: self
    ! Returns the index matrix corresponding to the components.
    ! Each dimension of self is size(components,2).
     integer(kind=kind(1)), dimension(:,:), intent(in) :: components
     intent(out) :: self
     integer(kind=kind(1)) :: ind,a,b,c
     do ind = 1,size(components,2)
       a = components(1,ind)
       b = components(2,ind)
       c = components(3,ind)
       self(a+1,b+1,c+1) = ind
     end do

   end subroutine

   subroutine to_gaussian_xyz_indices(self,l_max)
    integer(kind=kind(1)), dimension(:,:,:) :: self
    ! Make "self", which maps the three defining xyz powers of each cartesian
    ! gaussian, for all gaussians up to angular momnetum "l_max", back to its
    ! lexical index *within a shell of the same angular momentum* i.e. not the
    ! total lexical index. NOTE: "self" has lower bounds of 0, and so is passed
    ! in as a pointer.
     pointer :: self
     integer(kind=kind(1)), intent(in) :: l_max
     integer(kind=kind(1)) :: L,k,a,b,c

   call ensure_(tonto,lbound(self,1)==0,"INTMAT3:to_gaussian_xyz_indices ... wrong lower bound")
   call ensure_(tonto,lbound(self,2)==0,"INTMAT3:to_gaussian_xyz_indices ... wrong lower bound")
   call ensure_(tonto,lbound(self,3)==0,"INTMAT3:to_gaussian_xyz_indices ... wrong lower bound")
   call ensure_(tonto,ubound(self,1)==l_max,"INTMAT3:to_gaussian_xyz_indices ... wrong upper bound")
   call ensure_(tonto,ubound(self,2)==l_max,"INTMAT3:to_gaussian_xyz_indices ... wrong upper bound")
   call ensure_(tonto,ubound(self,3)==l_max,"INTMAT3:to_gaussian_xyz_indices ... wrong upper bound")
     do L = 0,l_max   ! Loop over all shells with momentum L
       k = 1          ! This is the local shell lexical index
                      ! Loop over powers a, b, c
       do a = L,floor((L+2)*0.33333333333333333333333d0),-1
         do b = min(L-a,a),floor((L-a+1)*0.50d0),-1
           c = L-a-b
           if (a==b .and. b==c) then
             self(a,a,a) = k
             k = k+1
           else if (a>b .and. b==c) then
             self(a,b,b) = k
             self(b,a,b) = k+1
             self(b,b,a) = k+2
             k = k+3
           else if (a==b .and. b>c) then
             self(a,a,c) = k
             self(a,c,a) = k+1
             self(c,a,a) = k+2
             k = k+3
           else
             self(a,b,c) = k
             self(a,c,b) = k+1
             self(b,a,c) = k+2
             self(c,a,b) = k+3
             self(b,c,a) = k+4
             self(c,b,a) = k+5
             k = k+6
           end if
         end do
       end do
     end do

   end subroutine

end
