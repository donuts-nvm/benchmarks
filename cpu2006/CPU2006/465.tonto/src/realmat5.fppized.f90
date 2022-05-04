!---------------------------------------------------------------------------
!
!  REALMAT5: 5 dimensional matrices
!
! Copyright (C) dylan jayatilaka, 1998
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
! $Id: realmat5.foo,v 1.5 2003/02/19 07:48:57 reaper Exp $
!---------------------------------------------------------------------------

module REALMAT5_MODULE

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

   public    create_
   interface create_
      module procedure create
      module procedure create_1
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

   subroutine create(self,dim1,dim2,dim3,dim4,dim5)
    real(kind=kind(1.0d0)), dimension(:,:,:,:,:) :: self
    ! Create a mat5 object
      pointer :: self
      integer(kind=kind(1)) :: dim1,dim2,dim3,dim4,dim5

      nullify(self)
      allocate(self(dim1,dim2,dim3,dim4,dim5))

   end subroutine

   subroutine create_1(self,lb1,ub1,lb2,ub2,lb3,ub3,lb4,ub4,lb5,ub5)
    real(kind=kind(1.0d0)), dimension(:,:,:,:,:) :: self
    ! Create a mat5 object with the specified bounds
      pointer :: self
      integer(kind=kind(1)) :: lb1,lb2,lb3,lb4,lb5
      integer(kind=kind(1)) :: ub1,ub2,ub3,ub4,ub5

      nullify(self)
      allocate(self(lb1:ub1,lb2:ub2,lb3:ub3,lb4:ub4,lb5:ub5))

   end subroutine

   subroutine create_copy(self,m)
    real(kind=kind(1.0d0)), dimension(:,:,:,:,:) :: self
    ! Create a copy of matrix "m"
      pointer :: self
       real(kind=kind(1.0d0)), dimension(:,:,:,:,:), pointer :: m

      call create_(self,lbound(m,1),ubound(m,1), &
              lbound(m,2),ubound(m,2), &
              lbound(m,3),ubound(m,3), &
              lbound(m,4),ubound(m,4), &
              lbound(m,5),ubound(m,5)  )
      self = m

   end subroutine

   subroutine destroy(self)
    real(kind=kind(1.0d0)), dimension(:,:,:,:,:) :: self
    ! Destroy a mat5 object
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

end
