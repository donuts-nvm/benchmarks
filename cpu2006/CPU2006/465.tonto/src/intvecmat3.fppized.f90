!---------------------------------------------------------------------------
!
!  INTVECMAT3 : a 3-D matrix where each element is a vector of (possibly)
!  a different length
!
! Copyright (C) Dylan Jayatilaka, 2002
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
! $Id: intvecmat3.foo,v 1.2 2003/02/19 07:48:57 reaper Exp $
!---------------------------------------------------------------------------

module INTVECMAT3_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

   use INTVEC_MODULE, only: set_to_
   use INTVEC_MODULE, only: create_
   use INTVEC_MODULE, only: create_copy_
   use INTVEC_MODULE, only: destroy_

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

   public    dim1_
   interface dim1_
      module procedure dim1
   end interface

   public    dim2_
   interface dim2_
      module procedure dim2
   end interface

   public    nullify_ptr_part_
   interface nullify_ptr_part_
      module procedure nullify_ptr_part
   end interface

   public    dim3_
   interface dim3_
      module procedure dim3
   end interface

   public    set_to_
   interface set_to_
      module procedure set_to
   end interface

   public    create_
   interface create_
      module procedure create
      module procedure create_1
      module procedure create_2
      module procedure create_3
   end interface

   public    create_copy_
   interface create_copy_
      module procedure create_copy
   end interface

   public    zero_
   interface zero_
      module procedure zero
   end interface

   public    create_vec_
   interface create_vec_
      module procedure create_vec
      module procedure create_vec_1
   end interface

   public    destroy_ptr_part_
   interface destroy_ptr_part_
      module procedure destroy_ptr_part
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

   subroutine create(self,dim1,dim2,dim3)
    type(intvec__type), dimension(:,:,:) :: self
    ! Create space
      pointer :: self
      integer(kind=kind(1)) :: dim1,dim2,dim3

      nullify(self)
      allocate(self(dim1,dim2,dim3))

      call nullify_ptr_part_(self)

   end subroutine

   subroutine create_1(self,dim1,dim2,dim3,dimv)
    type(intvec__type), dimension(:,:,:) :: self
    ! Create space
      pointer :: self
      integer(kind=kind(1)) :: dim1,dim2,dim3,dimv

      nullify(self)
      allocate(self(dim1,dim2,dim3))

      call create_vec_(self,dimv)

   end subroutine

   subroutine create_2(self,dim1,dim2,dim3,dimv)
    type(intvec__type), dimension(:,:,:) :: self
    ! Create space
      pointer :: self
      integer(kind=kind(1)) :: dim1,dim2,dim3
      integer(kind=kind(1)), dimension(2) :: dimv

      nullify(self)
      allocate(self(dim1,dim2,dim3))

      call create_vec_(self,dimv)

   end subroutine

   subroutine create_3(self,dim)
    type(intvec__type), dimension(:,:,:) :: self
    ! Create space
      pointer :: self
      integer(kind=kind(1)), dimension(3) :: dim

      nullify(self)
      allocate(self(dim(1),dim(2),dim(3)))

      call nullify_ptr_part_(self)

   end subroutine

   subroutine create_vec(self,dimv)
    type(intvec__type), dimension(:,:,:) :: self
    ! Allocate the "vec" pointer parts
      integer(kind=kind(1)) :: dimv
      integer(kind=kind(1)) :: i,j,k

      do i = 1,size(self,1)
      do j = 1,size(self,2)
      do k = 1,size(self,3)
         call create_(self(i,j,k)%element,dimv)
      end do
      end do
      end do

   end subroutine

   subroutine create_vec_1(self,dimv)
    type(intvec__type), dimension(:,:,:) :: self
    ! Allocate the "vec" pointer parts
      integer(kind=kind(1)), dimension(2) :: dimv
      integer(kind=kind(1)) :: i,j,k

      do i = 1,size(self,1)
      do j = 1,size(self,2)
      do k = 1,size(self,3)
         call create_(self(i,j,k)%element,dimv)
      end do
      end do
      end do

   end subroutine

   subroutine destroy(self)
    type(intvec__type), dimension(:,:,:) :: self
    ! Destroy allocated space
      pointer :: self

      if (.not. associated(self)) then;   return; end if
      call destroy_ptr_part_(self)

      deallocate(self)

   end subroutine

   subroutine nullify_ptr_part(self)
    type(intvec__type), dimension(:,:,:) :: self
    ! Nullify the pointer parts
      pointer :: self
      integer(kind=kind(1)) :: i,j,k

      do i = 1,size(self,1)
      do j = 1,size(self,2)
      do k = 1,size(self,3)
         nullify(self(i,j,k)%element)
      end do
      end do
      end do

   end subroutine

   subroutine destroy_ptr_part(self)
    type(intvec__type), dimension(:,:,:) :: self
    ! Destroy the pointer parts of an ivecvec
      pointer :: self
      integer(kind=kind(1)) :: i,j,k

      do i = 1,size(self,1)
      do j = 1,size(self,2)
      do k = 1,size(self,3)
         call destroy_(self(i,j,k)%element)
      end do
      end do
      end do

   end subroutine

!   created result (res)
!   ! Returns true if self has been created
!      self :: pointer
!      res :: logical(kind=kind(.true.))
!      res = associated(self)
!   end

!   destroyed result (res)
!   ! Returns true if self has *not* been created
!      self :: pointer
!      res :: logical(kind=kind(.true.))
!      res = .not. associated(self)
!   end

   subroutine create_copy(self,v)
    type(intvec__type), dimension(:,:,:) :: self
    ! Create a copy of "v"
      pointer :: self
      type(intvec__type), dimension(:,:,:) :: v

      call create_(self,size(v,1),size(v,2),size(v,3))
      call copy_(self,v)

   end subroutine

   subroutine copy(self,v)
    type(intvec__type), dimension(:,:,:) :: self
    ! Make a copy of "v"
      type(intvec__type), dimension(:,:,:) :: v
      integer(kind=kind(1)) :: i,j,k

      do i = 1,size(v,1)
      do j = 1,size(v,2)
      do k = 1,size(v,3)
         call create_copy_(self(i,j,k)%element,v(i,j,k)%element)
      end do
      end do
      end do

   end subroutine

   subroutine set_to(self,v)
    type(intvec__type), dimension(:,:,:) :: self
    ! Set self to "v". Up to you to make sure they are compatible!
      intent(inout) :: self
      type(intvec__type), dimension(:,:,:), intent(in) :: v
      integer(kind=kind(1)) :: i,j,k

      do i = 1,size(v,1)
      do j = 1,size(v,2)
      do k = 1,size(v,3)
         call set_to_(self(i,j,k)%element,v(i,j,k)%element)
      end do
      end do
      end do

   end subroutine

   subroutine zero(self)
    type(intvec__type), dimension(:,:,:) :: self
    ! Zero all elements
      intent(inout) :: self
      integer(kind=kind(1)) :: i,j,k

      do i = 1,size(self,1)
      do j = 1,size(self,2)
      do k = 1,size(self,3)
         self(i,j,k)%element = 0
      end do
      end do
      end do

   end subroutine

   function dim1(self) result(res)
    type(intvec__type), dimension(:,:,:) :: self
    ! The first dimension of self
      integer(kind=kind(1)) :: res

      res = size(self,1)

   end function

   function dim2(self) result(res)
    type(intvec__type), dimension(:,:,:) :: self
    ! The second dimension of self
      integer(kind=kind(1)) :: res

      res = size(self,2)

   end function

   function dim3(self) result(res)
    type(intvec__type), dimension(:,:,:) :: self
    ! The third dimension of self
      integer(kind=kind(1)) :: res

      res = size(self,3)

   end function

end
