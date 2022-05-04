!---------------------------------------------------------------------------
!
! BINVEC : Boolean vector operations ...
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
! $Id: binvec.foo,v 1.6.2.1 2003/03/24 01:28:52 dylan Exp $
!
!---------------------------------------------------------------------------

module BINVEC_MODULE

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

   public    append_
   interface append_
      module procedure append
      module procedure append_1
   end interface

   public    shrink_
   interface shrink_
      module procedure shrink
   end interface

   public    n_true_
   interface n_true_
      module procedure n_true
   end interface

   public    n_false_
   interface n_false_
      module procedure n_false
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

   public    index_of_first_true_element_
   interface index_of_first_true_element_
      module procedure index_of_first_true_element
   end interface

   public    copy_
   interface copy_
      module procedure copy
   end interface

contains

   subroutine create(self,dim)
    logical(kind=kind(.true.)), dimension(:) :: self
    ! Create space for object
      pointer :: self
      integer(kind=kind(1)), intent(in) :: dim
    ! The following code is inherited from INTRINSICVEC

      call ensure_(tonto,dim>=0,"BINVEC:create ... dimension of array not 1 or greater")
      nullify(self)
      allocate(self(dim))

   end subroutine

   subroutine create_1(self,lb,ub)
    logical(kind=kind(.true.)), dimension(:) :: self
    ! Create the vector with lower bound "lb", upper bound "ub"
      pointer :: self
      integer(kind=kind(1)), intent(in) :: lb,ub
    ! The following code is inherited from INTRINSICVEC

      nullify(self)
      allocate(self(lb:ub))

   end subroutine

   subroutine create_2(self,bounds)
    logical(kind=kind(.true.)), dimension(:) :: self
    ! Create the vector with "bounds"
      pointer :: self
      integer(kind=kind(1)), dimension(2), intent(in) :: bounds
    ! The following code is inherited from OBJECTVEC

      call create_(self,bounds(1),bounds(2))

   end subroutine

   subroutine create_copy(self,vec)
    logical(kind=kind(.true.)), dimension(:) :: self
    ! Create a replica copy of vec.
      logical(kind=kind(.true.)), dimension(:), intent(in) :: vec
      pointer :: self
    ! The following code is inherited from INTRINSICVEC

      call create_(self,size(vec))
      self = vec

   end subroutine

   subroutine copy(self,vec)
    logical(kind=kind(.true.)), dimension(:) :: self
    ! Copy "vec".
      logical(kind=kind(.true.)), dimension(:), intent(in) :: vec
    ! The following code is inherited from INTRINSICVEC

      call ensure_(tonto,size(self)==size(vec),"BINVEC:copy ... vec size does not match")
      self = vec

   end subroutine

   subroutine destroy(self)
    logical(kind=kind(.true.)), dimension(:) :: self
    ! Destroy space for object
      pointer :: self
    ! The following code is inherited from INTRINSICVEC

      if (.not. associated(self)) then;   return; end if

      deallocate(self)

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

   function n_true(self) result(res)
    logical(kind=kind(.true.)), dimension(:) :: self
    ! Returns the number of true items in self.
     integer(kind=kind(1)) :: res

     res = count(self)

   end function

   function n_false(self) result(res)
    logical(kind=kind(.true.)), dimension(:) :: self
    ! Returns the number of false items in self.
     integer(kind=kind(1)) :: res

     res = size(self) - count(self)

   end function

   function index_of_first_true_element(self) result(res)
    logical(kind=kind(.true.)), dimension(:) :: self
    ! Returns the index of the first true element in self, or zero if there is no
    ! true element.
     integer(kind=kind(1)) :: res
     integer(kind=kind(1)) :: i

     res = 0
     do i = 1,size(self)
        if (self(i)) then
           res = i
           exit
        end if
     end do

   end function

   subroutine shrink(self,dim)
    logical(kind=kind(.true.)), dimension(:) :: self
    ! Shrink self to dimension dim.  Contents are retained.
     pointer :: self
     integer(kind=kind(1)), intent(in) :: dim
    ! The following code is inherited from INTRINSICVEC
     logical(kind=kind(.true.)), dimension(:), pointer :: old
     integer(kind=kind(1)) :: n

     call ensure_(tonto,associated(self),"BINVEC:shrink ... no self array")
     call ensure_(tonto,dim<=size(self),"BINVEC:shrink ... dim too large")
     if (dim==size(self)) then;   return; end if
     old => self
     nullify(self)
     call create_(self,dim)
     do n=1,dim
       self(n) = old(n)
     end do
     call destroy_(old)

   end subroutine

   subroutine expand(self,dim)
    logical(kind=kind(.true.)), dimension(:) :: self
    ! Expand self to dimension dim. New slots are left undefined.
     pointer :: self
     integer(kind=kind(1)), intent(in) :: dim
    ! The following code is inherited from INTRINSICVEC
     logical(kind=kind(.true.)), dimension(:), pointer :: old
     integer(kind=kind(1)) :: old_dim

     if (.not. associated(self)) then
        call create_(self,dim)
     else
        call ensure_(tonto,dim>=size(self),"BINVEC:expand ... dim not large enough")
        old => self
        old_dim = size(old)
        nullify(self)
        call create_(self,dim)
        self(1:old_dim) = old
        call destroy_(old)
     end if

   end subroutine

   subroutine append(self,v)
    logical(kind=kind(.true.)), dimension(:) :: self
    ! Expands self and appends the contents of vector "v".
     pointer :: self
     logical(kind=kind(.true.)), dimension(:), intent(in) :: v
    ! The following code is inherited from INTRINSICVEC
     integer(kind=kind(1)) :: dim

     if (.not. associated(self)) then; dim = 0
     else;                 dim = size(self)
     end if
     call expand_(self,dim+size(v))
     self(dim+1:) = v

   end subroutine

   subroutine append_1(self,value)
    logical(kind=kind(.true.)), dimension(:) :: self
    ! Expands self by 1, and appends the single scalar "value" onto the end.
     pointer :: self
     logical(kind=kind(.true.)), intent(in) :: value
    ! The following code is inherited from INTRINSICVEC
     integer(kind=kind(1)) :: dim

     if (.not. associated(self)) then; dim = 0
     else;                 dim = size(self)
     end if
     call expand_(self,dim+1)
     self(dim+1) = value

   end subroutine

end
