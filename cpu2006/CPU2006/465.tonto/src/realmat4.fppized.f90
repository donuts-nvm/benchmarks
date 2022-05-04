!---------------------------------------------------------------------------
!
!  REALMAT4: 4 dimensional matrices
!
! Copyright (C) Dylan Jayatilaka and Daniel Grimwood, 1998
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
! $Id: realmat4.foo,v 1.5 2003/02/19 07:48:57 reaper Exp $
!---------------------------------------------------------------------------

module REALMAT4_MODULE

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

   public    plus_product_
   interface plus_product_
      module procedure plus_product
   end interface

   public    create_
   interface create_
      module procedure create
      module procedure create_1
      module procedure create_2
      module procedure create_3
      module procedure create_4
   end interface

   public    create_copy_
   interface create_copy_
      module procedure create_copy
   end interface

   public    destroy_
   interface destroy_
      module procedure destroy
   end interface

contains

   subroutine create(self,dim1,dim2,dim3,dim4)
    real(kind=kind(1.0d0)), dimension(:,:,:,:) :: self
    ! Create a 4-d array
     pointer :: self
     integer(kind=kind(1)), intent(in) :: dim1,dim2,dim3,dim4

     nullify(self)
     allocate(self(dim1,dim2,dim3,dim4))

   end subroutine

   subroutine create_1(self,bounds)
    real(kind=kind(1.0d0)), dimension(:,:,:,:) :: self
    ! Create a 4-d array with all upper "bounds" specified
     pointer :: self
     integer(kind=kind(1)), dimension(4), intent(in) :: bounds

     call create_(self,bounds(1),bounds(2),bounds(3),bounds(4))

   end subroutine

   subroutine create_2(self,lb1,ub1,lb2,ub2,lb3,ub3,lb4,ub4)
    real(kind=kind(1.0d0)), dimension(:,:,:,:) :: self
    ! Create a 4-d array with all bound specified
     pointer :: self
     integer(kind=kind(1)), intent(in) :: lb1,lb2,lb3,lb4,ub1,ub2,ub3,ub4

     nullify(self)
     allocate(self(lb1:ub1,lb2:ub2,lb3:ub3,lb4:ub4))

   end subroutine

   subroutine create_3(self,bounds1,bounds2,bounds3,bounds4)
    real(kind=kind(1.0d0)), dimension(:,:,:,:) :: self
    ! Create a 4-d array with all pair bounds specified
     pointer :: self
     integer(kind=kind(1)), dimension(2), intent(in) :: bounds1,bounds2,bounds3,bounds4

     call create_(self,bounds1(1),bounds1(2),bounds2(1),bounds2(2), &
             bounds3(1),bounds3(2),bounds4(1),bounds4(2))

   end subroutine

   subroutine create_4(self,bounds)
    real(kind=kind(1.0d0)), dimension(:,:,:,:) :: self
    ! Create a 4-d array with all bounds specified
     pointer :: self
     integer(kind=kind(1)), dimension(4,2), intent(in) :: bounds

     call create_(self,bounds(1,1),bounds(1,2),bounds(2,1),bounds(2,2), &
             bounds(3,1),bounds(3,2),bounds(4,1),bounds(4,2))

   end subroutine

   subroutine create_copy(self,m)
    real(kind=kind(1.0d0)), dimension(:,:,:,:) :: self
    ! Create a copy of matrix "m"
      pointer :: self
       real(kind=kind(1.0d0)), dimension(:,:,:,:), pointer :: m

      call create_(self,lbound(m,1),ubound(m,1), &
              lbound(m,2),ubound(m,2), &
              lbound(m,3),ubound(m,3), &
              lbound(m,4),ubound(m,4)  )
      self = m

   end subroutine

   subroutine destroy(self)
    real(kind=kind(1.0d0)), dimension(:,:,:,:) :: self
    ! Destroy the object
      pointer :: self

      if (.not. associated(self)) then;   return; end if

      deallocate(self)

   end subroutine

!   created result(res)
!   ! Returns true if self has been created
!      self :: pointer
!      res :: logical(kind=kind(.true.))
!      res = associated(self)
!   end

!   destroyed result(res)
!   ! Returns true if self has *not* been created
!      self :: pointer
!      res :: logical(kind=kind(.true.))
!      res = .not. associated(self)
!   end

   subroutine plus_product(self,a,b)
    real(kind=kind(1.0d0)), dimension(:,:,:,:) :: self
    ! The product of "b" with scalar "a" is added to "self".
      real(kind=kind(1.0d0)), intent(in) :: a
      real(kind=kind(1.0d0)), dimension(:,:,:,:), intent(in) :: b
     integer(kind=kind(1)) :: d1,d2,d3,d4,dim1,dim2,dim3,dim4

     dim1=size(self,1)
     dim2=size(self,2)
     dim3=size(self,3)
     dim4=size(self,4)
   call ensure_(tonto,size(self,1)==dim1,"REALMAT4:plus_product ... array dimension mismatch")
   call ensure_(tonto,size(self,2)==dim2,"REALMAT4:plus_product ... array dimension mismatch")
   call ensure_(tonto,size(self,3)==dim3,"REALMAT4:plus_product ... array dimension mismatch")
   call ensure_(tonto,size(self,4)==dim4,"REALMAT4:plus_product ... array dimension mismatch")
     do d4=1,dim4
       do d3=1,dim3
         do d2=1,dim2
           do d1=1,dim1
             self(d1,d2,d3,d4) = self(d1,d2,d3,d4) + a * b(d1,d2,d3,d4)
           end do
         end do
       end do
     end do

   end subroutine

end
