!---------------------------------------------------------------------------
!
!  REALMAT3VEC: Vector of REALMAT3 matrices
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
! $Id: realmat3vec.foo,v 1.4.2.1 2003/10/13 06:22:46 reaper Exp $
!---------------------------------------------------------------------------

module REALMAT3VEC_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

   use REALMAT3_MODULE, only: gaussian_d_xyz_matrices_
   use REALMAT3_MODULE, only: create_
   use REALMAT3_MODULE, only: gaussian_g_xyz_matrices_
   use REALMAT3_MODULE, only: gaussian_f_xyz_matrices_
   use REALMAT3_MODULE, only: destroy_

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
      module procedure create_1
   end interface

   public    make_gaussian_xyz_matrices_
   interface make_gaussian_xyz_matrices_
      module procedure make_gaussian_xyz_matrices
   end interface

   public    destroy_
   interface destroy_
      module procedure destroy
   end interface

contains

   subroutine create(self,dim)
    type(realmat3__type), dimension(:) :: self
    ! Create a mat3vec
      pointer :: self
      integer(kind=kind(1)) :: dim

      allocate(self(dim))

      call nullify_ptr_part_(self)

   end subroutine

   subroutine create_1(self,lb,ub)
    type(realmat3__type), dimension(:) :: self
    ! Create a mat3vec
      pointer :: self
      integer(kind=kind(1)) :: lb,ub

      call ensure_(tonto,ub>=lb,"REALMAT3VEC:create_1 ... upper bound must be greater than or equal to lower bound")
      allocate(self(lb:ub))

      call nullify_ptr_part_(self)

   end subroutine

   subroutine destroy(self)
    type(realmat3__type), dimension(:) :: self
    ! Destroy a mat3vec
      pointer :: self

      if (.not. associated(self)) then;   return; end if
      call destroy_ptr_part_(self)

      deallocate(self)

   end subroutine

   subroutine nullify_ptr_part(self)
    type(realmat3__type), dimension(:) :: self
    ! Nullify the pointer parts of a mat3vec
       integer(kind=kind(1)) :: i

      do i = 1,size(self,1)
         nullify(self(i)%element)
      end do

   end subroutine

   subroutine destroy_ptr_part(self)
    type(realmat3__type), dimension(:) :: self
    ! Destroy the pointer parts of a mat3vec
       integer(kind=kind(1)) :: i

      do i = 1,size(self,1)
         call destroy_(self(i)%element)
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

   subroutine make_gaussian_xyz_matrices(self,ptr)
    type(realmat3__type), dimension(:) :: self
    ! Return representation matrices for the s, p, d, f, and g (l = 0 ... 4)
    ! xyz cartesian gaussian shell components from a list of the p xyz
    ! representation matrices.
    ! NOTE: nothing in self in pointer assigned, so it can be destroyed safely.
      pointer :: self
      real(kind=kind(1.0d0)), dimension(:,:,:) :: ptr
      integer(kind=kind(1)) :: order

      call ensure_(tonto,size(ptr,1)==3,"REALMAT3VEC:make_gaussian_xyz_matrices ... wrong 1st dimension, self")
      call ensure_(tonto,size(ptr,2)==3,"REALMAT3VEC:make_gaussian_xyz_matrices ... wrong 1st dimension, self")
      call ensure_(tonto,size(ptr,3)>0,"REALMAT3VEC:make_gaussian_xyz_matrices ... no p-type representation matrices")
      order = size(ptr,3)
      call create_(self,0,4)
      call create_(self(0)%element,1,1,order)
      call create_(self(1)%element,3,3,order)
       ! Now assign the transformation matrices
      self(0)%element = 1.0d0
      self(1)%element = ptr
      self(2)%element => gaussian_d_xyz_matrices_(ptr)
      self(3)%element => gaussian_f_xyz_matrices_(ptr)
      self(4)%element => gaussian_g_xyz_matrices_(ptr)

   end subroutine

end
