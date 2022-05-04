!-------------------------------------------------------------------------------
!
! SHELLPAIRVEC : a vector of pairs of gaussian SHELLPAIR's.
!
! This object is used to store precalculated information used in ERI integral
! evaluation.
!
! Copyright (C) Dylan Jayatilaka, 2000
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
! $Id: shellpairvec.foo,v 1.2.2.1 2003/03/24 01:28:54 dylan Exp $
!-------------------------------------------------------------------------------

module SHELLPAIRVEC_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

   use SHELLPAIR_MODULE, only: nullify_ptr_part_
   use SHELLPAIR_MODULE, only: destroy_ptr_part_
   use SHELLPAIR_MODULE, only: copy_
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

   public    destroy_ptr_part_
   interface destroy_ptr_part_
      module procedure destroy_ptr_part
   end interface

   public    create_
   interface create_
      module procedure create
   end interface

   public    create_copy_
   interface create_copy_
      module procedure create_copy
   end interface

   public    destroy_
   interface destroy_
      module procedure destroy
   end interface

   public    copy_
   interface copy_
      module procedure copy
   end interface

contains

!  ******************
!  Allocation methods
!  ******************

   subroutine create(self,dim)
    type(shellpair_type), dimension(:) :: self
    ! Create space for a the shellpair vector
      pointer :: self
      integer(kind=kind(1)), intent(in) :: dim

      nullify(self)
      allocate(self(dim))

      call nullify_ptr_part_(self)

   end subroutine

   subroutine destroy(self)
    type(shellpair_type), dimension(:) :: self
    ! Destroy space for object
      pointer :: self
    ! The following code is inherited from OBJECTVEC

      if (.not. associated(self)) then;   return; end if
      call destroy_ptr_part_(self)

      deallocate(self)

   end subroutine

   subroutine nullify_ptr_part(self)
    type(shellpair_type), dimension(:) :: self
    ! Nullify the pointer parts of self
    ! The following code is inherited from OBJECTVEC
      integer(kind=kind(1)) :: a

      do a = 1,size(self)
         call nullify_ptr_part_(self(a))
      end do

   end subroutine

   subroutine destroy_ptr_part(self)
    type(shellpair_type), dimension(:) :: self
    ! Destroy the pointer parts of self
    ! The following code is inherited from OBJECTVEC
      integer(kind=kind(1)) :: a

      do a = 1,size(self)
        call destroy_ptr_part_(self(a))
      end do

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

   subroutine create_copy(self,vec)
    type(shellpair_type), dimension(:) :: self
    ! Create a replica copy of "vec".
      type(shellpair_type), dimension(:), intent(in) :: vec
      pointer :: self
    ! The following code is inherited from OBJECTVEC

      call create_(self,size(vec))
      call copy_(self,vec)

   end subroutine

   subroutine copy(self,vec)
    type(shellpair_type), dimension(:) :: self
    ! Copy "vec".
      type(shellpair_type), dimension(:), intent(in) :: vec
    ! The following code is inherited from OBJECTVEC
      integer(kind=kind(1)) :: a

      call ensure_(tonto,size(self)==size(vec),"SHELLPAIRVEC:copy ... vec size does not match")
      do a = 1,size(vec)
        call copy_(self(a),vec(a))
      end do

   end subroutine

end
