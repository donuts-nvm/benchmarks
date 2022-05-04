!---------------------------------------------------------------------------
!
! INTVECINTVECHASH :
!
! A hash table (or associative array) with INTVEC keys and INTVEC values.
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
! $Id: intvecintvechash.foo,v 1.2.2.1 2003/09/18 05:33:22 reaper Exp $
!---------------------------------------------------------------------------

module INTVECINTVECHASH_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

   use INTVEC_MODULE, only: to_str_

   use INTMAT_MODULE, only: shrink_columns_
   use INTMAT_MODULE, only: create_
   use INTMAT_MODULE, only: expand_columns_
   use INTMAT_MODULE, only: create_copy_
   use INTMAT_MODULE, only: destroy_
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

   public    set_
   interface set_
      module procedure set
      module procedure set_1
   end interface

   public    set_defaults_
   interface set_defaults_
      module procedure set_defaults
   end interface

   public    nullify_ptr_part_
   interface nullify_ptr_part_
      module procedure nullify_ptr_part
   end interface

   public    append_pair_
   interface append_pair_
      module procedure append_pair
   end interface

   public    create_
   interface create_
      module procedure create
      module procedure create_1
   end interface

   public    create_copy_
   interface create_copy_
      module procedure create_copy
   end interface

   public    value_for_item_
   interface value_for_item_
      module procedure value_for_item
   end interface

   public    set_reverse_search_
   interface set_reverse_search_
      module procedure set_reverse_search
   end interface

   public    destroy_ptr_part_
   interface destroy_ptr_part_
      module procedure destroy_ptr_part
   end interface

   public    expand_
   interface expand_
      module procedure expand
   end interface

   public    value_for_key_
   interface value_for_key_
      module procedure value_for_key
   end interface

   public    destroy_
   interface destroy_
      module procedure destroy
   end interface

   public    delete_
   interface delete_
      module procedure delete
   end interface

   public    delete_item_
   interface delete_item_
      module procedure delete_item
   end interface

   public    index_of_key_
   interface index_of_key_
      module procedure index_of_key
   end interface

   public    has_key_
   interface has_key_
      module procedure has_key
   end interface

   public    shrink_
   interface shrink_
      module procedure shrink
      module procedure shrink_1
   end interface

   public    append_pairs_
   interface append_pairs_
      module procedure append_pairs
   end interface

   public    copy_
   interface copy_
      module procedure copy
   end interface

   integer(kind=kind(1)), save :: index_of_last_key

contains

   subroutine create(self)
    type(intvecintvechash_type) :: self
    ! Create the hash object.
      pointer :: self

      allocate(self)

      call nullify_ptr_part_(self)
      call set_defaults_(self)

   end subroutine

   subroutine create_1(self,n_size,keydim,valdim)
    type(intvecintvechash_type) :: self
    ! Create the hash with key array and value array with length "n_size".  The
    ! dimension of each key is "keydim". The dimension of each values is "valdim"
      pointer :: self
      integer(kind=kind(1)) :: n_size,keydim,valdim

      call create_(self)
      self%n_keys = 0
      self%n_size = n_size
      call create_(self%keys,keydim,n_size)
      call create_(self%values,valdim,n_size)

   end subroutine

   subroutine destroy(self)
    type(intvecintvechash_type) :: self
    ! Destroy the object.
      pointer :: self

      if (.not. associated(self)) then;   return; end if
      call destroy_ptr_part_(self)

      deallocate(self)

   end subroutine

   subroutine nullify_ptr_part(self)
    type(intvecintvechash_type) :: self
    ! Nullify the pointer parts

     nullify(self%keys)
     nullify(self%values)

   end subroutine

   subroutine destroy_ptr_part(self)
    type(intvecintvechash_type) :: self
    ! Destroy the pointer parts

      call destroy_(self%keys)
      call destroy_(self%values)

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

   subroutine set_defaults(self)
    type(intvecintvechash_type) :: self
    ! Set defaults

      self%n_keys = 0
      self%n_size = 0
      self%reverse_search = .false.

   end subroutine

   subroutine set_reverse_search(self,value)
    type(intvecintvechash_type) :: self
    ! Set the .reverse_search switch to "value". This determines whether the
    ! search through the keys arrays occurs in reverse order, which may be useful
    ! if you know that the most recent keys added are more likely to contain the
    ! values you are looking for.
      logical(kind=kind(.true.)) :: value

      self%reverse_search = value

   end subroutine

   subroutine create_copy(self,hash)
    type(intvecintvechash_type) :: self
    ! Create a copy of "hash"
      pointer :: self
      type(intvecintvechash_type) :: hash

      call create_(self)
      call copy_(self,hash)

   end subroutine

   subroutine copy(self,hash)
    type(intvecintvechash_type) :: self
    ! Make a "self" copy of "hash". NOTE: pointer parts are pointer copied.
      type(intvecintvechash_type) :: hash

      self = hash
      call create_copy_(self%keys,hash%keys)
      call create_copy_(self%values,hash%values)

   end subroutine

   subroutine shrink(self)
    type(intvecintvechash_type) :: self
    ! Shrinks the size of the keys and values arrays to dimension ".n_keys",
    ! *only* when ".n_size" is bigger than the number of keys stored.  Contents
    ! are retained. This is used to save memory.

   call ensure_(tonto,self%n_size>0,"INTVECINTVECHASH:shrink ... size of hash is 0")
     if (self%n_size<=self%n_keys) then;   return; end if
     call shrink_(self,self%n_keys)

   end subroutine

   subroutine shrink_1(self,n_size)
    type(intvecintvechash_type) :: self
    ! Shrinks the size of the keys and values arrays to dimension "n_size".
    ! Contents are retained, where possible.
     integer(kind=kind(1)), intent(in) :: n_size

   call ensure_(tonto,self%n_size>0,"INTVECINTVECHASH:shrink_1 ... size of hash is 0")
     self%n_keys = min(self%n_keys,n_size)
     self%n_size = n_size
     call shrink_columns_(self%keys,n_size)
     call shrink_columns_(self%values,n_size)

   end subroutine

   subroutine expand(self,n_size)
    type(intvecintvechash_type) :: self
    ! Expands the size of the keys and values arrays to dimension "n_size".
    ! Contents are retained.
     integer(kind=kind(1)), intent(in) :: n_size

   call ensure_(tonto,n_size>self%n_size,"INTVECINTVECHASH:expand ... keys is already big enough")
   call ensure_(tonto,n_size>0,"INTVECINTVECHASH:expand ... keys and values not created")
     self%n_size = n_size
     call expand_columns_(self%keys,n_size)
     call expand_columns_(self%values,n_size)

   end subroutine

   subroutine append_pair(self,key,value)
    type(intvecintvechash_type) :: self
    ! Append the "key" and the corresponding "value" to the hash table.
    ! If the .keys or .values arrays are too small, they are doubled in size.
     integer(kind=kind(1)), dimension(:), intent(in) :: key
     integer(kind=kind(1)), dimension(:), intent(in) :: value
     integer(kind=kind(1)) :: n

     if (self%n_keys+1>self%n_size) call expand_(self,2*(self%n_keys+1))
     n = self%n_keys + 1
     self%n_keys = n
     self%keys(:,n) = key
     self%values(:,n) = value

   end subroutine

   subroutine set(self,key,value)
    type(intvecintvechash_type) :: self
    ! Append the "key" and the corresponding "value" to the hash table.
    ! If the .keys or .values arrays are too small, they are doubled in size.
     integer(kind=kind(1)), dimension(:), intent(in) :: key
     integer(kind=kind(1)), dimension(:), intent(in) :: value

     call append_pair_(self,key,value)

   end subroutine

   subroutine append_pairs(self,keys,values)
    type(intvecintvechash_type) :: self
    ! Append the "keys" and the corresponding "values" to the hash table.
    ! If the .keys or .values arrays are too small, they are doubled in size.
     integer(kind=kind(1)), dimension(:,:), intent(in) :: keys
     integer(kind=kind(1)), dimension(:,:), intent(in) :: values
     integer(kind=kind(1)) :: new,f,l

   call ensure_(tonto,size(keys,2)==size(values),"INTVECINTVECHASH:append_pairs ... keys and values must have same length")
     new = size(keys,2)
     if (self%n_keys+new>self%n_size) call expand_(self,2*(self%n_keys+new))
     f = self%n_keys + 1
     l = self%n_keys + new
     self%n_keys = l
     self%keys(:,f:l) = keys
     self%values(:,f:l) = values

   end subroutine

   subroutine set_1(self,keys,values)
    type(intvecintvechash_type) :: self
    ! Append the "keys" and the corresponding "values" to the hash table.
    ! If the .keys or .values arrays are too small, they are doubled in size.
     integer(kind=kind(1)), dimension(:,:), intent(in) :: keys
     integer(kind=kind(1)), dimension(:,:), intent(in) :: values

   call ensure_(tonto,size(keys,2)==size(values),"INTVECINTVECHASH:set_1 ... keys and values must have same length")
     call append_pairs_(self,keys,values)

   end subroutine

   subroutine delete(self,key,has_key)
    type(intvecintvechash_type) :: self
    ! Delete the "key" and the corresponding "value" from the hash table.  A
    ! fatal error occurs if the "key" is not there, unless "has_key" is present.
    ! If it is present, "has_key" is set to .true. if element *was* there
    ! (after the delete operation it is not there!), otherwise it is set false.
    ! There is no change made to the size of the hash table.
     integer(kind=kind(1)), dimension(:), intent(in) :: key
     logical(kind=kind(.true.)), intent(out), optional :: has_key
     integer(kind=kind(1)) :: i

     i = index_of_key_(self,key)
     if (i>0) then
        if (present(has_key)) has_key = .true.
        call delete_item_(self,i)
     else
        if (present(has_key)) then; has_key = .false.
        else;  call die_(tonto,"INTVECINTVECHASH:delete ... no value exists for key = "//trim(to_str_(key)))
        end if
     end if

   end subroutine

   subroutine delete_item(self,index)
    type(intvecintvechash_type) :: self
    ! Delete element "index" from the hash table.  No change is made to the size
    ! of the hash table.
     integer(kind=kind(1)), intent(in) :: index
     integer(kind=kind(1)) :: i,n

   call ensure_(tonto,index>0,"INTVECINTVECHASH:delete_item ... index must be positive")
   call ensure_(tonto,index<=self%n_keys,"INTVECINTVECHASH:delete_item ... index must be less than number of keys")
     i = index
     n = self%n_keys - 1
     self%keys(:,i:n) = self%keys(:,i+1:self%n_keys)
     self%values(:,i:n) = self%values(:,i+1:self%n_keys)
     self%n_keys = n

   end subroutine

   function index_of_key(self,key) result(res)
    type(intvecintvechash_type) :: self
    ! Returns the index of any key which matches "key" in the hash. If there is
    ! no match, then the result is 0.
     integer(kind=kind(1)), dimension(:), intent(in) :: key
     integer(kind=kind(1)) :: res
     integer(kind=kind(1)) :: i

     res = 0
     if (self%reverse_search) then
        do i = self%n_keys,1,-1
           if (any(key/=self%keys(:,i))) cycle
           res = i
           exit
        end do
     else
        do i = 1,self%n_keys
           if (any(key/=self%keys(:,i))) cycle
           res = i
           exit
        end do
     end if
     index_of_last_key = res

   end function

   function has_key(self,key,index) result(res)
    type(intvecintvechash_type) :: self
    ! Returns .true. if the hash contains a specific "key". If present, "index" is
    ! set to the index of that key.
     integer(kind=kind(1)), dimension(:), intent(in) :: key
     integer(kind=kind(1)), intent(out), optional :: index
     logical(kind=kind(.true.)) :: res
     integer(kind=kind(1)) :: i

     i = index_of_key_(self,key)
     res = i > 0
     if (present(index)) index = i

   end function

   function value_for_key(self,key,has_key,index) result(res)
    type(intvecintvechash_type) :: self
    ! Returns the value corresponding to the hash "key". If "has_key" is not
    ! present, an error is generated if no matching "key" can be found in the
    ! hash; if it is present, it is set to .true. if the the key is present, or
    ! .false. otherwise. If "index" is present, then it is set to the value of the
    ! index of "key" in the hash table.
     integer(kind=kind(1)), dimension(:), intent(in) :: key
     logical(kind=kind(.true.)), intent(out), optional :: has_key
     integer(kind=kind(1)), intent(out), optional :: index
     integer(kind=kind(1)), dimension(size(self%values,1)) :: res
     integer(kind=kind(1)) :: i

     i = index_of_key_(self,key)
     if (i>0) then
        if (present(has_key)) has_key = .true.
        res = self%values(:,i)
     else
        if (present(has_key)) then; has_key = .false.
        else; call die_(tonto,"INTVECINTVECHASH:value_for_key ... no value exists for key = "//trim(to_str_(key)))
        end if
     end if
     if (present(index)) index = i

   end function

   function value_for_item(self,index) result(res)
    type(intvecintvechash_type) :: self
    ! Returns the "index"-th element in the hash table.
     integer(kind=kind(1)), intent(in) :: index
     integer(kind=kind(1)), dimension(size(self%values,1)) :: res

   call ensure_(tonto,index<=self%n_keys,"INTVECINTVECHASH:value_for_item ... index out of range")
   call ensure_(tonto,index>0,"INTVECINTVECHASH:value_for_item ... index out of range")
     res = self%values(:,index)

   end function

end
