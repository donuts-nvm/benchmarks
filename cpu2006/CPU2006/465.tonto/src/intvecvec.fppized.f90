!---------------------------------------------------------------------------
!
!  INTVECVEC : a matrix where each column is (possibly) a different length
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
! $Id: intvecvec.foo,v 1.4.2.3 2003/11/13 05:35:09 reaper Exp $
!---------------------------------------------------------------------------

module INTVECVEC_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

   use INTVEC_MODULE, only: append_
   use INTVEC_MODULE, only: join_
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

   public    join_
   interface join_
      module procedure join
   end interface

   public    nullify_ptr_part_
   interface nullify_ptr_part_
      module procedure nullify_ptr_part
   end interface

   public    create_
   interface create_
      module procedure create
      module procedure create_1
      module procedure create_2
   end interface

   public    create_copy_
   interface create_copy_
      module procedure create_copy
   end interface

   public    append_
   interface append_
      module procedure append
      module procedure append_1
   end interface

   public    shrink_
   interface shrink_
      module procedure shrink
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

   public    expand_
   interface expand_
      module procedure expand
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

   subroutine create(self,dim)
    type(intvec__type), dimension(:) :: self
    ! Create space for a vector of integer vectors
      pointer :: self
      integer(kind=kind(1)) :: dim

      nullify(self)
      allocate(self(dim))

      call nullify_ptr_part_(self)

   end subroutine

   subroutine create_1(self,dim,dimv)
    type(intvec__type), dimension(:) :: self
    ! Create space for a vector of integer vectors
      pointer :: self
      integer(kind=kind(1)) :: dim,dimv

      nullify(self)
      allocate(self(dim))

      call nullify_ptr_part_(self)
      call create_vec_(self,dimv)

   end subroutine

   subroutine create_2(self,dim,dimv)
    type(intvec__type), dimension(:) :: self
    ! Create space for a vector of integer vectors
      pointer :: self
      integer(kind=kind(1)) :: dim
      integer(kind=kind(1)), dimension(2) :: dimv

      nullify(self)
      allocate(self(dim))

      call nullify_ptr_part_(self)
      call create_vec_(self,dimv)

   end subroutine

   subroutine create_vec(self,dimv)
    type(intvec__type), dimension(:) :: self
    ! Allocate the "vec" pointer parts
      integer(kind=kind(1)) :: dimv
      integer(kind=kind(1)) :: i

      do i = 1,size(self)
         call create_(self(i)%element,dimv)
      end do

   end subroutine

   subroutine create_vec_1(self,dimv)
    type(intvec__type), dimension(:) :: self
    ! Allocate the "vec" pointer parts
      integer(kind=kind(1)), dimension(2) :: dimv
      integer(kind=kind(1)) :: i

      do i = 1,size(self)
         call create_(self(i)%element,dimv)
      end do

   end subroutine

   subroutine destroy(self)
    type(intvec__type), dimension(:) :: self
    ! Destroy allocated space for ivec vector
      pointer :: self

      if (.not. associated(self)) then;   return; end if
      call destroy_ptr_part_(self)

      deallocate(self)

   end subroutine

   subroutine nullify_ptr_part(self)
    type(intvec__type), dimension(:) :: self
    ! Nullify the pointer parts of an ivecvec
      pointer :: self
      integer(kind=kind(1)) :: s

      do s = 1,size(self)
         nullify(self(s)%element)
      end do

   end subroutine

   subroutine destroy_ptr_part(self)
    type(intvec__type), dimension(:) :: self
    ! Destroy the pointer parts of an ivecvec
      pointer :: self
       integer(kind=kind(1)) :: s

      do s = 1,size(self)
         call destroy_(self(s)%element)
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
    type(intvec__type), dimension(:) :: self
    ! Create a replica copy of "vec".
      type(intvec__type), dimension(:), intent(in) :: vec
      pointer :: self
    ! The following code is inherited from OBJECTVEC

      call create_(self,size(vec))
      call copy_(self,vec)

   end subroutine

   subroutine copy(self,v)
    type(intvec__type), dimension(:) :: self
    ! Make a copy of "v".
      type(intvec__type), dimension(:) :: v
      integer(kind=kind(1)) :: i

      call ensure_(tonto,size(self)==size(v),"INTVECVEC:copy ... v has incompatible size")
      do i = 1,size(v)
         call create_copy_(self(i)%element,v(i)%element)
      end do

   end subroutine

   subroutine shrink(self,dim)
    type(intvec__type), dimension(:) :: self
    ! Shrinks self to dimension dim.  Contents are retained.
     pointer :: self
     integer(kind=kind(1)), intent(in) :: dim
     integer(kind=kind(1)) :: i,old_size
     type(intvec__type), dimension(:), pointer :: old

     call ensure_(tonto,associated(self),"INTVECVEC:shrink ... no self array")
     call ensure_(tonto,dim<=size(self),"INTVECVEC:shrink ... dim not small enough")
     if (dim==size(self)) then;   return; end if
     old => self
     old_size = size(old)
     nullify(self)
     call create_(self,dim)
     do i = 1,dim
        self(i)%element => old(i)%element
        nullify(old(i)%element)
     end do
     do i = dim+1,old_size
        call destroy_(old(i)%element)
     end do
     call destroy_(old)

   end subroutine

   subroutine expand(self,dim)
    type(intvec__type), dimension(:) :: self
    ! Expands self to dimension dim.  Contents are retained.
    ! Note: pointer assignment is used to retain contents.
    ! Note: Elements which are added are nullified.
     pointer :: self
     integer(kind=kind(1)), intent(in) :: dim
     type(intvec__type), dimension(:), pointer :: old
     integer(kind=kind(1)) :: old_size,i

     if (.not. associated(self)) then
        call create_(self,dim)
     else
        call ensure_(tonto,dim>=size(self),"INTVECVEC:expand ... dim not large enough")
        old => self
        old_size = size(old)
        nullify(self)
        call create_(self,dim)
        do i = 1,old_size
           self(i)%element => old(i)%element
           nullify(old(i)%element)
        end do
        call destroy_(old)
        do i = old_size+1,dim
           nullify(self(i)%element)
        end do
     end if

   end subroutine

   subroutine append(self,v)
    type(intvec__type), dimension(:) :: self
    ! Expands self to the required dimension, and append the contents
    ! of encapsulated vector "v". Note: appended element parts are pointer
    ! assigned, not copied.
     pointer :: self
     type(intvec__type), dimension(:), intent(in) :: v
     integer(kind=kind(1)) :: dim,i

     if (.not. associated(self)) then; dim = 0
     else;                 dim = size(self)
     end if
     call expand_(self,dim+size(v))
     do i = 1,size(v)
        self(dim+i)%element => v(i)%element
     end do

   end subroutine

   subroutine append_1(self,value)
    type(intvec__type), dimension(:) :: self
    ! Expands self to the required dimension, and append the single
    ! INTVEC "value" onto the end of self.
     pointer :: self
     integer(kind=kind(1)), dimension(:), intent(in) :: value
     integer(kind=kind(1)) :: dim

     if (.not. associated(self)) then; dim = 0
     else;                 dim = size(self)
     end if
     call expand_(self,dim+1)
     call append_(self(dim+1)%element,value)

   end subroutine

   function join(self,list) result(res)
    type(intvec__type), dimension(:) :: self
    ! Join together the vectors in self whose indices are listed in "list".
      integer(kind=kind(1)), dimension(:) :: list
      integer(kind=kind(1)), dimension(:), pointer :: res
      integer(kind=kind(1)), dimension(:), pointer :: temp
      integer(kind=kind(1)) :: i,l

      call ensure_(tonto,size(list)>=1,"INTVECVEC:join ... list must not have zero dimension")
      call ensure_(tonto,maxval(list)<=size(self),"INTVECVEC:join ... some elements of list are too large")
      call ensure_(tonto,minval(list)>=1,"INTVECVEC:join ... list elements must be +ve")
      l = list(1)
      call create_copy_(res,self(l)%element)
      do i = 2,size(list)
         l = list(i)
         temp => join_(res,self(l)%element)
         call destroy_(res)
         res => temp
      end do

   end function

end
