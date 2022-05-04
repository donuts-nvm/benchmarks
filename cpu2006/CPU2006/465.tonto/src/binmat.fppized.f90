!---------------------------------------------------------------------------
!
!  BINMAT: Logical matrices
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
! $Id: binmat.foo,v 1.2 2003/02/19 07:48:56 reaper Exp $
!---------------------------------------------------------------------------

module BINMAT_MODULE

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

   public    shrink_
   interface shrink_
      module procedure shrink
   end interface

   public    is_transposed_shape_of_
   interface is_transposed_shape_of_
      module procedure is_transposed_shape_of
   end interface

   public    is_same_shape_as_
   interface is_same_shape_as_
      module procedure is_same_shape_as
   end interface

   public    create_
   interface create_
      module procedure create
      module procedure create_1
      module procedure create_2
   end interface

   public    expand_
   interface expand_
      module procedure expand
   end interface

   public    create_copy_
   interface create_copy_
      module procedure create_copy
   end interface

   public    destroy_
   interface destroy_
      module procedure destroy
   end interface

   public    is_square_
   interface is_square_
      module procedure is_square
   end interface

contains

   subroutine create(self,dim1,dim2)
    logical(kind=kind(.true.)), dimension(:,:) :: self
    ! Create a matrix with the given dimensions
      pointer :: self
      integer(kind=kind(1)), intent(in) :: dim1,dim2
    ! The following code is inherited from INTRINSICMAT

      nullify(self)
      allocate(self(dim1,dim2))

   end subroutine

   subroutine create_1(self,lb1,ub1,lb2,ub2)
    logical(kind=kind(.true.)), dimension(:,:) :: self
    ! Create a matrix with the given dimensions
      pointer :: self
      integer(kind=kind(1)), intent(in) :: lb1,ub1,lb2,ub2
    ! The following code is inherited from INTRINSICMAT

      nullify(self)
      allocate(self(lb1:ub1,lb2:ub2))

   end subroutine

   subroutine create_2(self,bounds1,bounds2)
    logical(kind=kind(.true.)), dimension(:,:) :: self
    ! Create a matrix with the specified bounds for each dimension
      pointer :: self
      integer(kind=kind(1)), dimension(:), intent(in) :: bounds1,bounds2
    ! The following code is inherited from INTRINSICMAT

      call create_(self,bounds1(1),bounds1(2),bounds2(1),bounds2(2))

   end subroutine

   subroutine create_copy(self,matrix)
    logical(kind=kind(.true.)), dimension(:,:) :: self
    ! Create a replica copy of matrix
      pointer :: self
      logical(kind=kind(.true.)), dimension(:,:), intent(in) :: matrix
    ! The following code is inherited from INTRINSICMAT

      call create_(self,lbound(matrix,1),ubound(matrix,1), &
              lbound(matrix,2),ubound(matrix,2)  )
      self = matrix

   end subroutine

   subroutine destroy(self)
    logical(kind=kind(.true.)), dimension(:,:) :: self
    ! Destroy the object
      pointer :: self
    ! The following code is inherited from INTRINSICMAT

      if (.not. associated(self)) then;   return; end if

      deallocate(self)

   end subroutine

   function is_square(self) result(res)
    logical(kind=kind(.true.)), dimension(:,:) :: self
    ! Returns .true. if the matrix is square
      intent(in) :: self
      logical(kind=kind(.true.)) :: res

      res = size(self,1)==size(self,2)
    ! The following code is inherited from INTRINSICMAT

   end function

   function is_same_shape_as(self,b) result(res)
    logical(kind=kind(.true.)), dimension(:,:) :: self
    ! Returns .true. if the matrix "b" has the same shape as self
      intent(in) :: self
      logical(kind=kind(.true.)), dimension(:,:), intent(in) :: b
      logical(kind=kind(.true.)) :: res
    ! The following code is inherited from INTRINSICMAT

      res = size(self,1)==size(b,1) .and. size(self,2)==size(b,2)

   end function

   function is_transposed_shape_of(self,b) result(res)
    logical(kind=kind(.true.)), dimension(:,:) :: self
    ! Returns .true. if the matrix "b" is the transposed shape of self
      intent(in) :: self
      logical(kind=kind(.true.)), dimension(:,:), intent(in) :: b
      logical(kind=kind(.true.)) :: res
    ! The following code is inherited from INTRINSICMAT

      res = size(self,1)==size(b,2) .and. size(self,2)==size(b,1)

   end function

   subroutine shrink(self,dim1,dim2)
    logical(kind=kind(.true.)), dimension(:,:) :: self
    ! Shrinks self to dimension dim1xdim2.  Contents are retained.
     pointer :: self
     integer(kind=kind(1)), intent(in) :: dim1,dim2
    ! The following code is inherited from INTRINSICMAT
     logical(kind=kind(.true.)), dimension(:,:), pointer :: old

     call ensure_(tonto,associated(self),"BINMAT:shrink ... matrix not allocated")
     call ensure_(tonto,dim1<=size(self,1),"BINMAT:shrink ... 1st dimension given is too large.")
     call ensure_(tonto,dim2<=size(self,2),"BINMAT:shrink ... 2nd dimension given is too large.")
     if (dim1==size(self,1) .and. dim2==size(self,2)) then;   return; end if
     old => self
     nullify(self)
     call create_(self,dim1,dim2)
     self=old(1:dim1,1:dim2)
     call destroy_(old)

   end subroutine

   subroutine expand(self,dim1,dim2)
    logical(kind=kind(.true.)), dimension(:,:) :: self
    ! Expands self to dimension dim1xdim2.  Contents are retained.
     pointer :: self
     integer(kind=kind(1)), intent(in) :: dim1,dim2
    ! The following code is inherited from INTRINSICMAT
     logical(kind=kind(.true.)), dimension(:,:), pointer :: old
     integer(kind=kind(1)) :: old_size1,old_size2

     if (.not. associated(self)) then
       call create_(self,0,0)
     else
     call ensure_(tonto,dim1>=size(self,1),"BINMAT:expand ... 1st dimension given is too small")
     call ensure_(tonto,dim2>=size(self,2),"BINMAT:expand ... 2nd dimension given is too small")
     end if
     old => self
     old_size1 = size(old,1)
     old_size2 = size(old,2)
     nullify(self)
     call create_(self,dim1,dim2)
     self(1:old_size1,1:old_size2)=old
     call destroy_(old)

   end subroutine

end
