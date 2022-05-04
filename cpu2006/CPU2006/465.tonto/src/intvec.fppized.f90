!---------------------------------------------------------------------------
!
!  INTVEC : Integer vector operations ...
!
! Copyright (C) Dylan Jayatilaka, 1997
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
! $Id: intvec.foo,v 1.14.2.3 2003/11/06 10:06:14 dylan Exp $
!---------------------------------------------------------------------------

module INTVEC_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

   use INT_MODULE, only: choose_
   use INT_MODULE, only: to_str_

   use REAL_MODULE, only: is_in_range_
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

   public    index_of_minimum_
   interface index_of_minimum_
      module procedure index_of_minimum
   end interface

   public    join_
   interface join_
      module procedure join
      module procedure join_1
   end interface

   public    set_to_
   interface set_to_
      module procedure set_to
   end interface

   public    norm_
   interface norm_
      module procedure norm
   end interface

   public    index_of_maximum_
   interface index_of_maximum_
      module procedure index_of_maximum
   end interface

   public    to_str_
   interface to_str_
      module procedure to_str
   end interface

   public    same_as_
   interface same_as_
      module procedure same_as
   end interface

   public    plus_product_of_
   interface plus_product_of_
      module procedure plus_product_of
   end interface

   public    to_pair_vec_from_
   interface to_pair_vec_from_
      module procedure to_pair_vec_from
   end interface

   public    swap_elements_
   interface swap_elements_
      module procedure swap_elements
   end interface

   public    to_product_of_
   interface to_product_of_
      module procedure to_product_of
      module procedure to_product_of_1
   end interface

   public    is_zero_
   interface is_zero_
      module procedure is_zero
   end interface

   public    plus_
   interface plus_
      module procedure plus
   end interface

   public    index_of_first_zero_value_
   interface index_of_first_zero_value_
      module procedure index_of_first_zero_value
   end interface

   private    quick_sort_increasing_
   interface quick_sort_increasing_
      module procedure quick_sort_increasing
      module procedure quick_sort_increasing_1
   end interface

   public    index_of_smallest_value_
   interface index_of_smallest_value_
      module procedure index_of_smallest_value
   end interface

   public    shrink_
   interface shrink_
      module procedure shrink
   end interface

   public    in_range_
   interface in_range_
      module procedure in_range
   end interface

   public    minus_
   interface minus_
      module procedure minus
   end interface

   public    to_cross_product_
   interface to_cross_product_
      module procedure to_cross_product
   end interface

   public    sort_
   interface sort_
      module procedure sort
   end interface

   public    copy_
   interface copy_
      module procedure copy
   end interface

   public    index_of_largest_value_
   interface index_of_largest_value_
      module procedure index_of_largest_value
   end interface

   public    create_
   interface create_
      module procedure create
      module procedure create_1
      module procedure create_2
   end interface

   public    chop_large_values_
   interface chop_large_values_
      module procedure chop_large_values
   end interface

   public    all_in_range_
   interface all_in_range_
      module procedure all_in_range
   end interface

   public    create_copy_
   interface create_copy_
      module procedure create_copy
   end interface

   public    plus_scaled_vec_
   interface plus_scaled_vec_
      module procedure plus_scaled_vec
   end interface

   public    remove_repetitions_
   interface remove_repetitions_
      module procedure remove_repetitions
   end interface

   public    reverse_order_
   interface reverse_order_
      module procedure reverse_order
   end interface

   public    is_z_axis_
   interface is_z_axis_
      module procedure is_z_axis
   end interface

   public    append_only_if_unique_
   interface append_only_if_unique_
      module procedure append_only_if_unique
   end interface

   public    dot_
   interface dot_
      module procedure dot
   end interface

   public    equals_
   interface equals_
      module procedure equals
   end interface

   public    to_scaled_vec_
   interface to_scaled_vec_
      module procedure to_scaled_vec
   end interface

   public    range_
   interface range_
      module procedure range
   end interface

   public    no_of_unique_elements_
   interface no_of_unique_elements_
      module procedure no_of_unique_elements
   end interface

   public    expand_
   interface expand_
      module procedure expand
   end interface

   public    destroy_
   interface destroy_
      module procedure destroy
   end interface

   public    cross_
   interface cross_
      module procedure cross
   end interface

   public    largest_value_
   interface largest_value_
      module procedure largest_value
   end interface

   public    minus_product_of_
   interface minus_product_of_
      module procedure minus_product_of
   end interface

   private    quick_sort_decreasing_
   interface quick_sort_decreasing_
      module procedure quick_sort_decreasing
      module procedure quick_sort_decreasing_1
   end interface

   public    index_of_value_
   interface index_of_value_
      module procedure index_of_value
   end interface

   public    quick_sort_
   interface quick_sort_
      module procedure quick_sort
      module procedure quick_sort_1
   end interface

   public    no_of_elements_larger_than_
   interface no_of_elements_larger_than_
      module procedure no_of_elements_larger_than
   end interface

   public    index_of_first_nonzero_value_
   interface index_of_first_nonzero_value_
      module procedure index_of_first_nonzero_value
   end interface

   public    append_
   interface append_
      module procedure append
      module procedure append_1
   end interface

   public    combinations_of_length_
   interface combinations_of_length_
      module procedure combinations_of_length
   end interface

   public    has_repetitions_
   interface has_repetitions_
      module procedure has_repetitions
   end interface

   public    smallest_value_
   interface smallest_value_
      module procedure smallest_value
   end interface

   public    bin_XY_data_
   interface bin_XY_data_
      module procedure bin_XY_data
   end interface

contains

   subroutine create(self,dim)
    integer(kind=kind(1)), dimension(:) :: self
    ! Create space for object
      pointer :: self
      integer(kind=kind(1)), intent(in) :: dim
    ! The following code is inherited from INTRINSICVEC

      call ensure_(tonto,dim>=0,"INTVEC:create ... dimension of array not 1 or greater")
      nullify(self)
      allocate(self(dim))

   end subroutine

   subroutine create_1(self,lb,ub)
    integer(kind=kind(1)), dimension(:) :: self
    ! Create the vector with lower bound "lb", upper bound "ub"
      pointer :: self
      integer(kind=kind(1)), intent(in) :: lb,ub
    ! The following code is inherited from INTRINSICVEC

      nullify(self)
      allocate(self(lb:ub))

   end subroutine

   subroutine create_2(self,bounds)
    integer(kind=kind(1)), dimension(:) :: self
    ! Create the vector with "bounds"
      pointer :: self
      integer(kind=kind(1)), dimension(2), intent(in) :: bounds
    ! The following code is inherited from OBJECTVEC

      call create_(self,bounds(1),bounds(2))

   end subroutine

   subroutine destroy(self)
    integer(kind=kind(1)), dimension(:) :: self
    ! Destroy space for object
      pointer :: self
    ! The following code is inherited from INTRINSICVEC

      if (.not. associated(self)) then;   return; end if

      deallocate(self)

   end subroutine

!   created result(res) ::: get_from(OBJECTVEC)
!   ! Returns true if self has been created
!      self :: pointer
!      res :: logical(kind=kind(.true.))
!   end

!   destroyed result(res) ::: get_from(OBJECTVEC)
!   ! Returns true if self has *not* been created
!      self :: pointer
!      res :: logical(kind=kind(.true.))
!   end

   subroutine create_copy(self,vec)
    integer(kind=kind(1)), dimension(:) :: self
    ! Create a replica copy of vec.
      integer(kind=kind(1)), dimension(:), intent(in) :: vec
      pointer :: self
    ! The following code is inherited from INTRINSICVEC

      call create_(self,size(vec))
      self = vec

   end subroutine

   subroutine copy(self,vec)
    integer(kind=kind(1)), dimension(:) :: self
    ! Copy "vec".
      integer(kind=kind(1)), dimension(:), intent(in) :: vec
    ! The following code is inherited from INTRINSICVEC

      call ensure_(tonto,size(self)==size(vec),"INTVEC:copy ... vec size does not match")
      self = vec

   end subroutine

   subroutine shrink(self,dim)
    integer(kind=kind(1)), dimension(:) :: self
    ! Shrink self to dimension dim.  Contents are retained.
     pointer :: self
     integer(kind=kind(1)), intent(in) :: dim
    ! The following code is inherited from INTRINSICVEC
     integer(kind=kind(1)), dimension(:), pointer :: old
     integer(kind=kind(1)) :: n

     call ensure_(tonto,associated(self),"INTVEC:shrink ... no self array")
     call ensure_(tonto,dim<=size(self),"INTVEC:shrink ... dim too large")
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
    integer(kind=kind(1)), dimension(:) :: self
    ! Expand self to dimension dim. New slots are left undefined.
     pointer :: self
     integer(kind=kind(1)), intent(in) :: dim
    ! The following code is inherited from INTRINSICVEC
     integer(kind=kind(1)), dimension(:), pointer :: old
     integer(kind=kind(1)) :: old_dim

     if (.not. associated(self)) then
        call create_(self,dim)
     else
        call ensure_(tonto,dim>=size(self),"INTVEC:expand ... dim not large enough")
        old => self
        old_dim = size(old)
        nullify(self)
        call create_(self,dim)
        self(1:old_dim) = old
        call destroy_(old)
     end if

   end subroutine

   subroutine append(self,v)
    integer(kind=kind(1)), dimension(:) :: self
    ! Expands self and appends the contents of vector "v".
     pointer :: self
     integer(kind=kind(1)), dimension(:), intent(in) :: v
    ! The following code is inherited from INTRINSICVEC
     integer(kind=kind(1)) :: dim

     if (.not. associated(self)) then; dim = 0
     else;                 dim = size(self)
     end if
     call expand_(self,dim+size(v))
     self(dim+1:) = v

   end subroutine

   subroutine append_1(self,value)
    integer(kind=kind(1)), dimension(:) :: self
    ! Expands self by 1, and appends the single scalar "value" onto the end.
     pointer :: self
     integer(kind=kind(1)), intent(in) :: value
    ! The following code is inherited from INTRINSICVEC
     integer(kind=kind(1)) :: dim

     if (.not. associated(self)) then; dim = 0
     else;                 dim = size(self)
     end if
     call expand_(self,dim+1)
     self(dim+1) = value

   end subroutine

   subroutine append_only_if_unique(self,value)
    integer(kind=kind(1)), dimension(:) :: self
    ! Expands self by 1, and appends the single scalar "value" onto the end, but
    ! only if the "value" is unique
     pointer :: self
     integer(kind=kind(1)), intent(in) :: value
    ! The following code is inherited from INTRINSICVEC

     if (any(self==value)) then;   return; end if
     call append_(self,value)

   end subroutine

   subroutine remove_repetitions(self)
    integer(kind=kind(1)), dimension(:) :: self
    ! Sort through the vector and remove repeated elements which come later in
    ! the list.  NOTE: the vector may shrink
      pointer :: self
    ! The following code is inherited from INTRINSICVEC
      integer(kind=kind(1)), dimension(:), pointer :: unique
      integer(kind=kind(1)) :: i,n

   call ensure_(tonto,associated(self),"INTVEC:remove_repetitions ... no vector")
      if (size(self)==1) then;   return; end if
      call create_(unique,size(self))
      n = 1
      unique(1) = self(1)
      do i = 2,size(self)
         if (any(self(i)==unique(1:n))) cycle
         n = n + 1
         unique(n) = self(i)
      end do
      call destroy_(self)
      call create_(self,n)
      self = unique(1:n)
      call destroy_(unique)

   end subroutine

   function has_repetitions(self) result(res)
    integer(kind=kind(1)), dimension(:) :: self
    ! Return .true. if self has at least one repeated element.
      logical(kind=kind(.true.)) :: res
    ! The following code is inherited from INTRINSICVEC
      integer(kind=kind(1)), dimension(:), pointer :: unique
      integer(kind=kind(1)) :: i,n

      res = .false.
      if (size(self)==1) then;   return; end if
      call create_(unique,size(self))
      n = 1
      unique(1) = self(1)
      do i = 2,size(self)
         if (any(self(i)==unique(1:n))) then
            res = .true.
         else
            n = n + 1
            unique(n) = self(i)
         end if
      end do
      call destroy_(unique)

   end function

   function join(self,v) result(res)
    integer(kind=kind(1)), dimension(:) :: self
    ! Yield a vector which is the concatenation of "self" and "v"
     integer(kind=kind(1)), dimension(:), intent(in) :: v
     integer(kind=kind(1)), dimension(:), pointer :: res
    ! The following code is inherited from INTRINSICVEC
     integer(kind=kind(1)) :: dim, dim_v

     dim   = size(self)
     dim_v = size(v)
     call create_(res,dim+dim_v)
     res(    1:dim      ) = self
     res(dim+1:dim+dim_v) = v

   end function

   function join_1(self,v1,v2) result(res)
    integer(kind=kind(1)), dimension(:) :: self
    ! Yield a vector which is the concatenation of "self" and "v1" and "v2"
     integer(kind=kind(1)), dimension(:), intent(in) :: v1,v2
     integer(kind=kind(1)), dimension(:), pointer :: res
    ! The following code is inherited from INTRINSICVEC
     integer(kind=kind(1)) :: dim, dim_v1, dim_v2

     dim    = size(self)
     dim_v1 = size(v1)
     dim_v2 = size(v2)
     call create_(res,dim+dim_v1+dim_v2)
     res(           1:dim              ) = self
     res(dim+       1:dim+dim_v1       ) = v1
     res(dim+dim_v1+1:dim+dim_v1+dim_v2) = v2

   end function

   subroutine to_product_of(self,mat,vec,transpose)
    integer(kind=kind(1)), dimension(:) :: self
    ! Set "self" to the product of the matrix "mat" and vector "vec". If present,
    ! "transpose" can be set to .true. if the matrix needs to be transposed.
      integer(kind=kind(1)), dimension(:,:), intent(in) :: mat
      integer(kind=kind(1)), dimension(:), intent(in) :: vec
      logical(kind=kind(.true.)), optional :: transpose
      integer(kind=kind(1)) :: i,k
      logical(kind=kind(.true.)) :: trans
      real(kind=kind(1.0d0)) :: temp

      trans = .false.
      if (present(transpose)) trans = transpose
      if (trans) then
        call ensure_(tonto,size(mat,2)==size(self),"INTVEC:to_product_of ... array dimensions do not agree")
        call ensure_(tonto,size(mat,1)==size(vec),"INTVEC:to_product_of ... array dimensions do not agree")
        do i = 1,size(self)
          temp = mat(1,i) * vec(1)
          do k=2,size(vec)
            temp = temp + mat(k,i) * vec(k)
          end do
          self(i) = temp
        end do
      else
        call ensure_(tonto,size(mat,1)==size(self),"INTVEC:to_product_of ... array dimensions do not agree")
        call ensure_(tonto,size(mat,2)==size(vec),"INTVEC:to_product_of ... array dimensions do not agree")
        do i = 1,size(self)
          temp = mat(i,1) * vec(1)
          do k=2,size(vec)
            temp = temp + mat(i,k) * vec(k)
          end do
          self(i) = temp
        end do
      end if

   end subroutine

   subroutine to_product_of_1(self,mat_a,mat_b,transpose_a,transpose_b)
    integer(kind=kind(1)), dimension(:) :: self
    ! Set "self" to the matrix product of "mat_a" and "mat_b". If present,
    ! "transpose_a" and "transpose_b" can be set to .true. if "mat_a" and "mat_b"
    ! neeb to be transposed.
     integer(kind=kind(1)), dimension(:,:) :: mat_a, mat_b
     logical(kind=kind(.true.)), optional :: transpose_a, transpose_b
     integer(kind=kind(1)) :: dim_a1,dim_a2,dim_b1,dim_b2,dim1,i,k,opt
     logical(kind=kind(.true.)) :: trans_a,trans_b
     real(kind=kind(1.0d0)) :: temp

     trans_a = .false.;       trans_b = .false.
     if (present(transpose_a)) trans_a = transpose_a
     if (present(transpose_b)) trans_b = transpose_b

     dim_a1 = size(mat_a,1);           dim_a2 = size(mat_a,2)
     dim_b1 = size(mat_b,1);           dim_b2 = size(mat_b,2)
     dim1   = size(self,1)

     opt = 0
     if (trans_a) opt = opt + 1
     if (trans_b) opt = opt + 2

     select case (opt)
       case (0)
         call ensure_(tonto,dim1==dim_a1,"INTVEC:to_product_of_1 ... array dimensions do not agree")
         call ensure_(tonto,dim_a2==dim_b1,"INTVEC:to_product_of_1 ... array dimensions do not agree")
         call ensure_(tonto,dim_b2==1,"INTVEC:to_product_of_1 ... b array dimensions incorrect")
         do i=1,dim1
           temp = 0.0d0
           do k=1,dim_a2
             temp = temp + mat_a(i,k) * mat_b(k,1)
           end do
           self(i) = temp
         end do
       case (1)
         call ensure_(tonto,dim1==dim_a2,"INTVEC:to_product_of_1 ... array dimensions do not agree")
         call ensure_(tonto,dim_a1==dim_b1,"INTVEC:to_product_of_1 ... array dimensions do not agree")
         call ensure_(tonto,dim_b2==1,"INTVEC:to_product_of_1 ... b array dimension incorrect")
         do i=1,dim1
           temp = 0.0d0
           do k=1,dim_a1
             temp = temp + mat_a(k,i) * mat_b(k,1)
           end do
           self(i) = temp
         end do
       case (2)
         call ensure_(tonto,dim1==dim_a1,"INTVEC:to_product_of_1 ... array dimensions do not agree")
         call ensure_(tonto,dim_a2==dim_b2,"INTVEC:to_product_of_1 ... array dimensions do not agree")
         call ensure_(tonto,dim_b1==1,"INTVEC:to_product_of_1 ... b array dimension incorrect")
         do i=1,dim1
           temp = 0.0d0
           do k=1,dim_a2
             temp = temp + mat_a(i,k) * mat_b(1,k)
           end do
           self(i) = temp
         end do
       case (3)
         call ensure_(tonto,dim1==dim_a2,"INTVEC:to_product_of_1 ... array dimensions do not agree")
         call ensure_(tonto,dim_a1==dim_b2,"INTVEC:to_product_of_1 ... array dimensions do not agree")
         call ensure_(tonto,dim_b1==1,"INTVEC:to_product_of_1 ... b array dimension incorrect")
         do i=1,dim1
           temp = 0.0d0
           do k=1,dim_a1
             temp = temp + mat_a(k,i) * mat_b(1,k)
           end do
           self(i) = temp
         end do
     end select

   end subroutine

   subroutine plus_product_of(self,mat,vec,transpose)
    integer(kind=kind(1)), dimension(:) :: self
    ! Add to "self" the product of the matrix and vector. If present,
    ! "transpose" can be set to .true. if the matrix needs to be transposed.
      integer(kind=kind(1)), dimension(:,:), intent(in) :: mat
      integer(kind=kind(1)), dimension(:), intent(in) :: vec
      logical(kind=kind(.true.)), optional :: transpose
      integer(kind=kind(1)) :: dim_a1,dim_a2,dim_b1,dim1,i,k
      logical(kind=kind(.true.)) :: trans
      real(kind=kind(1.0d0)) :: temp

      trans = .false.
      if (present(transpose)) trans = transpose
      dim_a1 = size(mat,1);   dim_a2 = size(mat,2)
      dim_b1 = size(vec,1)
      dim1   = size(self)
      if (trans) then
        call ensure_(tonto,dim1==dim_a2,"INTVEC:plus_product_of ... array dimensions do not agree")
        call ensure_(tonto,dim_b1==dim_a1,"INTVEC:plus_product_of ... array dimensions do not agree")
        do i=1,dim1
          temp = mat(1,i) * vec(1)
          do k=2,dim_b1
            temp = temp + mat(k,i) * vec(k)
          end do
          self(i) = self(i) + temp
        end do
      else
        call ensure_(tonto,dim1==dim_a1,"INTVEC:plus_product_of ... array dimensions do not agree")
        call ensure_(tonto,dim_b1==dim_a2,"INTVEC:plus_product_of ... array dimensions do not agree")
        do i=1,dim1
          temp = mat(i,1) * vec(1)
          do k=2,dim_b1
            temp = temp + mat(i,k) * vec(k)
          end do
          self(i) = self(i) + temp
        end do
      end if

   end subroutine

   subroutine minus_product_of(self,mat,vec,transpose)
    integer(kind=kind(1)), dimension(:) :: self
    ! Subtract from "self" the product of the matrix and vector. If present,
    ! "transpose" can be set to .true. if the matrix needs to be transposed.
      integer(kind=kind(1)), dimension(:,:), intent(in) :: mat
      integer(kind=kind(1)), dimension(:), intent(in) :: vec
      logical(kind=kind(.true.)), optional :: transpose
      integer(kind=kind(1)) :: dim_a1,dim_a2,dim_b1,dim1,i,k
      logical(kind=kind(.true.)) :: trans
      real(kind=kind(1.0d0)) :: temp

      trans = .false.
      if (present(transpose)) trans = transpose
      dim_a1 = size(mat,1);   dim_a2 = size(mat,2)
      dim_b1 = size(vec,1)
      dim1   = size(self)
      if (trans) then
        call ensure_(tonto,dim1==dim_a2,"INTVEC:minus_product_of ... array dimensions do not agree")
        call ensure_(tonto,dim_b1==dim_a1,"INTVEC:minus_product_of ... array dimensions do not agree")
        do i=1,dim1
          temp = mat(1,i) * vec(1)
          do k=2,dim_b1
            temp = temp + mat(k,i) * vec(k)
          end do
          self(i) = self(i) - temp
        end do
      else
        call ensure_(tonto,dim1==dim_a1,"INTVEC:minus_product_of ... array dimensions do not agree")
        call ensure_(tonto,dim_b1==dim_a2,"INTVEC:minus_product_of ... array dimensions do not agree")
        do i=1,dim1
          temp = mat(i,1) * vec(1)
          do k=2,dim_b1
            temp = temp + mat(i,k) * vec(k)
          end do
          self(i) = self(i) - temp
        end do
      end if

   end subroutine

   pure function dot(self,b) result(res)
    integer(kind=kind(1)), dimension(:) :: self
    ! Return the dot product with "b"
      intent(in) :: self
      integer(kind=kind(1)), dimension(:), intent(in) :: b
      integer(kind=kind(1)) :: res
    ! The following code is inherited from INTRINSICVEC
      res = dot_product(self,b)

   end function

   pure function norm(self) result(res)
    integer(kind=kind(1)), dimension(:) :: self
    ! Return the norm of the vector
      intent(in) :: self
      real(kind=kind(1.0d0)) :: res
      res = sqrt(real(sum(self*self),kind=kind(1.0d0)))

   end function

   pure function cross(self,b) result(res)
    integer(kind=kind(1)), dimension(:) :: self
    ! Return the cross product of "self" and "b".
      intent(in) :: self
      integer(kind=kind(1)), dimension(:), intent(in) :: b
      integer(kind=kind(1)), dimension(3) :: res
    ! The following code is inherited from INTRINSICVEC

      res(1) = self(2)*b(3) - b(2)*self(3)
      res(2) = self(3)*b(1) - b(3)*self(1)
      res(3) = self(1)*b(2) - b(1)*self(2)

   end function

   pure subroutine to_cross_product(self,a,b)
    integer(kind=kind(1)), dimension(:) :: self
    ! Set the vector to the cross product of "a" and "b".
      intent(inout) :: self
      integer(kind=kind(1)), dimension(:), intent(in) :: a,b
    ! The following code is inherited from INTRINSICVEC

      self(1) = a(2)*b(3) - b(2)*a(3)
      self(2) = a(3)*b(1) - b(3)*a(1)
      self(3) = a(1)*b(2) - b(1)*a(2)

   end subroutine

   pure subroutine reverse_order(self)
    integer(kind=kind(1)), dimension(:) :: self
    ! Reverse the order of the elements of self
     intent(inout) :: self
    ! The following code is inherited from OBJECTVEC
     integer(kind=kind(1)) :: n,dim
     dim = size(self)
     do n = 1,dim/2
       call swap_elements_(self,n,dim-n+1)
     end do

   end subroutine

   function equals(self,b) result(res)
    integer(kind=kind(1)), dimension(:) :: self
    ! Return true if "self" is the same as "b".
      intent(in) :: self
      integer(kind=kind(1)), dimension(:), intent(in) :: b
      logical(kind=kind(.true.)) :: res

      res = same_as_(self,b)

   end function

   function same_as(self,b) result(res)
    integer(kind=kind(1)), dimension(:) :: self
    ! Return true if "self" is the same as "b".
      intent(in) :: self
      integer(kind=kind(1)), dimension(:), intent(in) :: b
      logical(kind=kind(.true.)) :: res

      if (size(self)/=size(b)) then; res = .false.
      else;                  res = all(self==b)
      end if

   end function

   pure subroutine to_scaled_vec(self,fac,b)
    integer(kind=kind(1)), dimension(:) :: self
    ! Set the vector to "b" scaled by "fac"
      intent(inout) :: self
      integer(kind=kind(1)), dimension(:), intent(in) :: b
      integer(kind=kind(1)), intent(in) :: fac
    ! The following code is inherited from INTRINSICVEC
      self = fac*b

   end subroutine

   pure subroutine plus_scaled_vec(self,fac,b)
    integer(kind=kind(1)), dimension(:) :: self
    ! Add a vector "b" scaled by "fac" to "self"
      intent(inout) :: self
      integer(kind=kind(1)), dimension(:), intent(in) :: b
      integer(kind=kind(1)), intent(in) :: fac
    ! The following code is inherited from INTRINSICVEC
      self = self + fac*b

   end subroutine

   pure subroutine minus(self,b,mask)
    integer(kind=kind(1)), dimension(:) :: self
    ! Subtract vector "b" from "self"
      intent(inout) :: self
      integer(kind=kind(1)), dimension(:), intent(in) :: b
      logical(kind=kind(.true.)), dimension(:), intent(in), optional :: mask
    ! The following code is inherited from INTRINSICVEC
       integer(kind=kind(1)) :: i
      if (.not. present(mask)) then
         self = self - b
      else
         do i = 1,size(self)
            if (mask(i)) self(i) = self(i) - b(i)
         end do
      end if

   end subroutine

   pure subroutine plus(self,b,mask)
    integer(kind=kind(1)), dimension(:) :: self
    ! Add vector "b" to "self"
      intent(inout) :: self
      logical(kind=kind(.true.)), dimension(:), intent(in), optional :: mask
      integer(kind=kind(1)), dimension(:), intent(in) :: b
    ! The following code is inherited from INTRINSICVEC
      integer(kind=kind(1)) :: i

      if (.not. present(mask)) then
         self = self + b
      else
         do i = 1,size(self)
            if (mask(i)) self(i) = self(i) + b(i)
         end do
      end if

   end subroutine

   pure subroutine set_to(self,b)
    integer(kind=kind(1)), dimension(:) :: self
    ! Set the vector to "b". See also the "copy" routine.
      intent(inout) :: self
      integer(kind=kind(1)), dimension(:), intent(in) :: b
    ! The following code is inherited from INTRINSICVEC
      self = b

   end subroutine

   pure subroutine swap_elements(self,e1,e2)
    integer(kind=kind(1)), dimension(:) :: self
    ! Swap elements "e1" and "e2" in "self".
      intent(inout) :: self
      integer(kind=kind(1)), intent(in) :: e1,e2
    ! The following code is inherited from INTRINSICVEC
      integer(kind=kind(1)) :: val

      val = self(e1)
      self(e1) = self(e2)
      self(e2) = val

   end subroutine

   pure function index_of_first_nonzero_value(self) result(res)
    integer(kind=kind(1)), dimension(:) :: self
    ! Returns the index of the first nonzero component of self.
     intent(in) :: self
     integer(kind=kind(1)) :: res
     integer(kind=kind(1)) :: i
     res=0
     do i=1,size(self)
       if (self(i)/=0) then
         res=i
         exit
       end if
     end do

   end function

   pure function index_of_first_zero_value(self) result(res)
    integer(kind=kind(1)), dimension(:) :: self
    ! Returns the index of the first zero component of self.
     intent(in) :: self
     integer(kind=kind(1)) :: res
     integer(kind=kind(1)) :: i
     res=0
     do i=1,size(self)
       if (self(i)==0) then
         res=i
         exit
       end if
     end do

   end function

   pure function index_of_value(self,val) result(pos)
    integer(kind=kind(1)), dimension(:) :: self
    ! Returns the first index in "self" which has the value "val", or 0 if "val"
    ! is not present in the array.
     intent(in) :: self
     integer(kind=kind(1)), intent(in) :: val
     integer(kind=kind(1)) :: pos
     integer(kind=kind(1)) :: i
     pos = 0
     do i = 1,size(self)
        if (self(i)/=val) cycle
        pos = i
        exit
     end do

   end function

   pure subroutine chop_large_values(self,val)
    integer(kind=kind(1)), dimension(:) :: self
    ! Set all values in the self whose absolute value is larger than "val" to
    ! "val" times the sign of the original value.
      intent(inout) :: self
      integer(kind=kind(1)), intent(in) :: val
    ! The following code is inherited from INTRINSICVEC
      integer(kind=kind(1)) :: dim,i
      integer(kind=kind(1)) :: bb,ba,sign
      dim = size(self)
      do i = 1,dim
         bb = self(i)
         if (bb==0.0d0) cycle
         ba = abs(bb)
         sign = bb/ba
         self(i) = sign*min(val,ba)
     end do

   end subroutine

   pure function index_of_maximum(self) result(ind)
    integer(kind=kind(1)), dimension(:) :: self
    ! Return the index of the maximum in the vector
      intent(in) :: self
      integer(kind=kind(1)) :: ind
    ! The following code is inherited from INTRINSICVEC
      ind = maxval(maxloc(self))

   end function

   pure function index_of_minimum(self) result(ind)
    integer(kind=kind(1)), dimension(:) :: self
    ! Return the index of the minimum in the vector
      intent(in) :: self
      integer(kind=kind(1)) :: ind
    ! The following code is inherited from INTRINSICVEC
      ind = minval(minloc(self))

   end function

   function is_zero(self) result(res)
    integer(kind=kind(1)), dimension(:) :: self
    ! Return true if the vector is zero
      logical(kind=kind(.true.)) :: res

      res = all(self==0)

   end function

   function all_in_range(self,range) result(res)
    integer(kind=kind(1)), dimension(:) :: self
    ! Return .true. if all values of self are within the specified "range".
      intent(in) :: self
      integer(kind=kind(1)), dimension(2), intent(in) :: range
      logical(kind=kind(.true.)) :: res
    ! The following code is inherited from INTRINSICVEC

      res = all(range(1) <= self .and. self <= range(2))

   end function

   function in_range(self,range) result(res)
    integer(kind=kind(1)), dimension(:) :: self
    ! Return element i as .true. if self(i) is within the specified "range".
      intent(in) :: self
      integer(kind=kind(1)), dimension(2), intent(in) :: range
      logical(kind=kind(.true.)), dimension(size(self)) :: res
    ! The following code is inherited from INTRINSICVEC

      res = (range(1) <= self .and. self <= range(2))

   end function

   function range(self) result(res)
    integer(kind=kind(1)), dimension(:) :: self
    ! Return the range (smallest and largest value) of self.
      intent(in) :: self
      integer(kind=kind(1)), dimension(2) :: res
    ! The following code is inherited from INTRINSICVEC

      res(1) = minval(self)
      res(2) = maxval(self)

   end function

   function is_z_axis(self) result(res)
    integer(kind=kind(1)), dimension(:) :: self
    ! Return true if the vector is set to the z-axis
      logical(kind=kind(.true.)) :: res

      call ensure_(tonto,size(self)==3,"INTVEC:is_z_axis ... must supply a 3 dimensional vector!")
      res = self(1) == 0
      res = self(2) == 0 .and. res
      res = (1-self(3)) == 0 .and. res

   end function

   pure function largest_value(self) result(maxval)
    integer(kind=kind(1)), dimension(:) :: self
    ! Return the maximum absolute value in the vector
      intent(in) :: self
      integer(kind=kind(1)) :: maxval
    ! The following code is inherited from INTRINSICVEC
      integer(kind=kind(1)) :: val
      integer(kind=kind(1)) :: i
      maxval = abs(self(1))
      do i = 2,size(self)
        val = abs(self(i))
        if (val > maxval) maxval = val
      end do

   end function

   pure function smallest_value(self) result(minval)
    integer(kind=kind(1)), dimension(:) :: self
    ! Return minimum absolute value in the vector
      intent(in) :: self
      integer(kind=kind(1)) :: minval
    ! The following code is inherited from INTRINSICVEC
      integer(kind=kind(1)) :: val
      integer(kind=kind(1)) :: i
      minval = abs(self(1))
      do i = 2,size(self)
        val = abs(self(i))
        if (val < minval) minval = val
      end do

   end function

   pure function index_of_largest_value(self) result(ind)
    integer(kind=kind(1)), dimension(:) :: self
    ! Return the index "ind" of the largest absolute value in the vector
      intent(in) :: self
      integer(kind=kind(1)) :: ind
    ! The following code is inherited from INTRINSICVEC
      integer(kind=kind(1)) :: i
      integer(kind=kind(1)) :: maxval,val
      maxval = abs(self(1))
      ind = 1
      do i = 2,size(self)
        val = abs(self(i))
        if (val > maxval) then
          maxval = val
          ind = i
        end if
      end do

   end function

   pure function index_of_smallest_value(self) result(ind)
    integer(kind=kind(1)), dimension(:) :: self
    ! Return the index "ind" of the smallest value in the vector
      intent(in) :: self
      integer(kind=kind(1)) :: ind
    ! The following code is inherited from INTRINSICVEC
      integer(kind=kind(1)) :: i
      integer(kind=kind(1)) :: minval,val
      minval = abs(self(1))
      ind = 1
      do i = 2,size(self)
        val = abs(self(i))
        if (val < minval) then
          minval = val
          ind = i
        end if
      end do

   end function

   pure function no_of_elements_larger_than(self,tol) result(res)
    integer(kind=kind(1)), dimension(:) :: self
    ! Return the number of elements larger than "tol".
      intent(in) :: self
      integer(kind=kind(1)), intent(in) :: tol
      integer(kind=kind(1)) :: res
    ! The following code is inherited from INTRINSICVEC
      res = count(self>tol)

   end function

   pure subroutine sort(self,decreasing_order)
    integer(kind=kind(1)), dimension(:) :: self
    ! Sort array "self" from lowest to highest, using simple insertion sort.  If
    ! "decreasing_order" is present and .true. sort from highest to lowest instead.
     intent(inout) :: self
     logical(kind=kind(.true.)), intent(in), optional :: decreasing_order
    ! The following code is inherited from OBJECTVEC
     integer(kind=kind(1)) :: i,j,n
     logical(kind=kind(.true.)) :: lowest_first
     lowest_first = .true.
     if (present(decreasing_order)) lowest_first = .not. decreasing_order
     n = size(self)
     if (lowest_first) then
       do i=1,n-1
       do j=i+1,n
          if (self(j) < self(i)) call swap_elements_(self,i,j)
       end do
       end do
     else
       do i=1,n-1
       do j=i+1,n
          if (self(j) > self(i)) call swap_elements_(self,i,j)
       end do
       end do
     end if

   end subroutine

   subroutine quick_sort(self,decreasing_order)
    integer(kind=kind(1)), dimension(:) :: self
    ! Sort the vector into increasing order.If "decreasing_order" is present and
    ! .true., the vector is sorted from largest to smallest
      intent(in) :: self
      logical(kind=kind(.true.)), optional, intent(in) :: decreasing_order
    ! The following code is inherited from OBJECTVEC
      logical(kind=kind(.true.)) :: decreasing

      decreasing = .false.
      if (present(decreasing_order)) decreasing = decreasing_order
      if (.not. decreasing) then; call quick_sort_increasing_(self)
      else;                     call quick_sort_decreasing_(self)
      end if

   end subroutine

   recursive subroutine quick_sort_increasing(self)
    integer(kind=kind(1)), dimension(:) :: self
    ! Sort the vector into order, smallest to largest
    ! The following code is inherited from OBJECTVEC
      integer(kind=kind(1)), dimension(:), pointer :: smaller,larger
      integer(kind=kind(1)) :: n, ns, ne, nl
      integer(kind=kind(1)) :: chosen

      if (size(self)<=1) then;   return; end if
      n = size(self)
      chosen = self(1)
      ns = count(self<chosen)
      nl = count(self>chosen)
      ne = n - ns - nl
      call create_(smaller,ns)
      call create_(larger,nl)
      smaller = pack(self,self<chosen)
      larger  = pack(self,self>chosen)
      call quick_sort_(smaller)
      call quick_sort_(larger)
      self(1:ns)       = smaller
      self(ns+1:ns+ne) = chosen
      self(ns+ne+1:)   = larger
      call destroy_(larger)
      call destroy_(smaller)

   end subroutine

   recursive subroutine quick_sort_decreasing(self)
    integer(kind=kind(1)), dimension(:) :: self
    ! Sort the vector into order, largest to smallest
    ! The following code is inherited from OBJECTVEC
      integer(kind=kind(1)), dimension(:), pointer :: smaller,larger
      integer(kind=kind(1)) :: n, ns, ne, nl
      integer(kind=kind(1)) :: chosen

      if (size(self)<=1) then;   return; end if
      n = size(self)
      chosen = self(1)
      ns = count(self<chosen)
      nl = count(self>chosen)
      ne = n - ns - nl
      call create_(smaller,ns)
      call create_(larger,nl)
      smaller = pack(self,self<chosen)
      larger  = pack(self,self>chosen)
      call quick_sort_(smaller)
      call quick_sort_(larger)
      self(1:nl)       = larger
      self(nl+1:nl+ne) = chosen
      self(nl+ne+1:)   = smaller
      call destroy_(larger)
      call destroy_(smaller)

   end subroutine

   subroutine quick_sort_1(self,indices,decreasing_order)
    integer(kind=kind(1)), dimension(:) :: self
    ! Return the "indices" which sort self from smallest to largest, i.e. on
    ! return "self(indices)" is sorted. NOTE: self is *not* sorted.
    ! If "decreasing_order" is present and .true., the indices are sorted from
    ! largest to smallest
      intent(in) :: self
      integer(kind=kind(1)), dimension(:), intent(inout) :: indices
      logical(kind=kind(.true.)), optional, intent(in) :: decreasing_order
    ! The following code is inherited from OBJECTVEC
      logical(kind=kind(.true.)) :: decreasing
      integer(kind=kind(1)) :: i

      call ensure_(tonto,size(indices)==size(self),"INTVEC:quick_sort_1 ... wrong size, indices")
      decreasing = .false.
      if (present(decreasing_order)) decreasing = decreasing_order
      indices = (/(i,i=1,size(self))/)  ! initialise indices
      if (.not. decreasing) then; call quick_sort_increasing_(self,indices)
      else;                     call quick_sort_decreasing_(self,indices)
      end if

   end subroutine

   recursive subroutine quick_sort_increasing_1(self,indices)
    integer(kind=kind(1)), dimension(:) :: self
    ! Return the indices which sort vector from smallest to largest, i.e. on
    ! return "self(indices)" is sorted. NOTE: self is *not* sorted.
      intent(in) :: self
      integer(kind=kind(1)), dimension(:), intent(inout) :: indices
    ! The following code is inherited from OBJECTVEC
      integer(kind=kind(1)), dimension(:), pointer :: list,small,equal,large,small_indices,equal_indices,large_indices
      integer(kind=kind(1)) :: n, i, ns, ne, nl
      integer(kind=kind(1)) :: chosen

      if (size(indices)<=1) then;   return; end if
      n = size(indices)
      call create_(list,n); list = (/(i,i=1,n)/)
      chosen = self(1)
      ns = count(self<chosen)
      nl = count(self>chosen)
      ne = n - ns - nl
      call create_(small,ns); call create_(small_indices,ns)
      call create_(equal,ne); call create_(equal_indices,ne)
      call create_(large,nl); call create_(large_indices,nl)
      small = pack(list,self <chosen)  ! indices of small self elements
      equal = pack(list,self==chosen)  ! indices of equal self elements
      large = pack(list,self >chosen)  ! indices of large self elements
      small_indices = indices(small)
      equal_indices = indices(equal)
      large_indices = indices(large)
      if (ns>1) call quick_sort_increasing_(self(small),small_indices)
      if (nl>1) call quick_sort_increasing_(self(large),large_indices)
      indices(1:ns)       = small_indices
      indices(ns+1:ns+ne) = equal_indices
      indices(ns+ne+1:)   = large_indices
      call destroy_(large_indices); call destroy_(large)
      call destroy_(equal_indices); call destroy_(equal)
      call destroy_(small_indices); call destroy_(small)
      call destroy_(list)

   end subroutine

   recursive subroutine quick_sort_decreasing_1(self,indices)
    integer(kind=kind(1)), dimension(:) :: self
    ! Return the indices which sort vector from largest to smallest, i.e. on
    ! return "self(indices)" is sorted. NOTE: self is *not* sorted.
      intent(in) :: self
      integer(kind=kind(1)), dimension(:), intent(inout) :: indices
    ! The following code is inherited from OBJECTVEC
      integer(kind=kind(1)), dimension(:), pointer :: list,small,equal,large,small_indices,equal_indices,large_indices
      integer(kind=kind(1)) :: n, i, ns, ne, nl
      integer(kind=kind(1)) :: chosen

      if (size(indices)<=1) then;   return; end if
      n = size(indices)
      call create_(list,n); list = (/(i,i=1,n)/)
      chosen = self(1)
      ns = count(self>chosen)
      nl = count(self<chosen)
      ne = n - ns - nl
      call create_(small,ns); call create_(small_indices,ns)
      call create_(equal,ne); call create_(equal_indices,ne)
      call create_(large,nl); call create_(large_indices,nl)
      small = pack(list,self >chosen)  ! indices of large self elements
      equal = pack(list,self==chosen)  ! indices of equal self elements
      large = pack(list,self <chosen)  ! indices of small self elements
      small_indices = indices(small)
      equal_indices = indices(equal)
      large_indices = indices(large)
      if (ns>1) call quick_sort_decreasing_(self(small),small_indices)
      if (nl>1) call quick_sort_decreasing_(self(large),large_indices)
      indices(1:ns)       = small_indices
      indices(ns+1:ns+ne) = equal_indices
      indices(ns+ne+1:)   = large_indices
      call destroy_(large_indices); call destroy_(large)
      call destroy_(equal_indices); call destroy_(equal)
      call destroy_(small_indices); call destroy_(small)
      call destroy_(list)

   end subroutine

   function to_str(self,format,separator) result(string)
    integer(kind=kind(1)), dimension(:) :: self
    ! Change self to a "string" using default format.
     character(*), optional :: format
     character(*), optional :: separator
     character(128) :: string
    ! The following code is inherited from OBJECTVEC
     character(128) :: str1,str2
     integer(kind=kind(1)) :: n

     string = " "
     do n = 1,size(self)
       str1 = to_str_(self(n),format)
       call ensure_(tonto,len_trim(string) + len_trim(str1) < len(string),"INTVEC:to_str ... string too long")
       if (present(separator)) then; str2 = trim(string) // separator // trim(str1)
       else;                         str2 = trim(string) //    " "    // trim(str1)
       end if
       string = str2
     end do

   end function

   recursive function combinations_of_length(self,k) result(C)
    integer(kind=kind(1)), dimension(:) :: self
    ! Returns the combination matrix "C" of all p distinct combinations
    ! C(:,p) of the elements in self(:) of length "k"
     integer(kind=kind(1)) :: k
     integer(kind=kind(1)), dimension(:,:), pointer :: C
     integer(kind=kind(1)) :: s,s_k,s1_k1,s1_k
     integer(kind=kind(1)), dimension(:,:), pointer :: L,R

     call ensure_(tonto,k<=size(self),"INTVEC:combinations_of_length ... k is too large")
     call ensure_(tonto,k>0,"INTVEC:combinations_of_length ... k must be positive")
     s     = size(self)
     s_k   = nint(    choose_(s,k),  kind=kind(1))
     s1_k  = nint(choose_((s-1),k),  kind=kind(1))
     s1_k1 = nint(choose_((s-1),k-1),kind=kind(1))
     allocate(C(k,s1_k))
     if (k==1) &
       C(1  ,:)             = self(:)
     if ((s>k) .and. (k/=1)) then
       C(1  ,      1:s1_k1) = self(1)
       L                   => combinations_of_length_(self(2:),k-1)
       C(2:k,      1:s1_k1) = L
       deallocate(L)
       R                   => combinations_of_length_(self(2:),k)
       C(:  ,s1_k1+1: s_k ) = R
       deallocate(R)
     end if
     if (k==s) &
       C(:  ,1)             = self(:)

   end function

   subroutine to_pair_vec_from(self,v1,v2)
    integer(kind=kind(1)), dimension(:) :: self
    ! If "v1" and "v2" are vectors which contain some common elements, set "self"
    ! so that self(i)=j implies that v1(i)=v2(j). Further, self(i)=0 that implies
    ! that the vector element i in "v1" is unpaired with any in "v2".
     intent(inout) :: self
     integer(kind=kind(1)), dimension(:) :: v1, v2
     integer(kind=kind(1)) :: i, j

   call ensure_(tonto,size(v1)==size(v2),"INTVEC:to_pair_vec_from ... vectors are not compatible sizes")
     self = 0.0d0
     do i = 1, size(v1)
        do j = 1, size(v2)
           if ((v1(i)==v2(j)) .and. (.not.(any(self==j)))) self(i)=j
        end do
     end do

   end subroutine

   pure function no_of_unique_elements(self) result(res)
    integer(kind=kind(1)), dimension(:) :: self
    ! Return the number of unique elements in the vector.
      intent(in) :: self
      integer(kind=kind(1)) :: res
      integer(kind=kind(1)) :: n,i
      logical(kind=kind(.true.)) :: same
      res = 1
      do n = 2,size(self)
         same = .false.
         do i = 1,n-1
            if (self(n)==self(i)) then
               same = .true.
               exit
            end if
         end do
         if (.not. same) res = res + 1
      end do

   end function

   subroutine bin_XY_data(self,X,Y,bin_side_length)
    integer(kind=kind(1)), dimension(:) :: self
    ! Return a vector of the same length as "X" and "Y", whose k-th element
    ! contains the number of data points [X(k),Y(k)] which lie in the k-th bin.
    ! A bin is simply a range of values (a square, in fact) of side length
    ! "bin_side_length" covering the total set of data points from [X_min,Y_min]
    ! to [X_max,Y_max], where X_min and X_max are the minimum and maximum data
    ! values in X (and likewise for Y).
      real(kind=kind(1.0d0)), dimension(:), intent(in) :: X,Y
      real(kind=kind(1.0d0)), intent(in) :: bin_side_length
      real(kind=kind(1.0d0)) :: X_min,X_max,X_mid,X_ran
      real(kind=kind(1.0d0)) :: Y_min,Y_max,Y_mid,Y_ran
      real(kind=kind(1.0d0)), dimension(2) :: X_range,Y_range
      integer(kind=kind(1)) :: n_X,n_Y,i,j,c,k

   call ensure_(tonto,size(X)==size(self),"INTVEC:bin_XY_data ... incompatible data points")
   call ensure_(tonto,size(X)==size(Y),"INTVEC:bin_XY_data ... incompatible data points")
      X_min = minval(X); Y_min = minval(Y)
      X_max = maxval(X); Y_max = maxval(Y)
      X_mid = 0.50d0*(X_min+X_max)
      Y_mid = 0.50d0*(Y_min+Y_max)
      X_ran = X_max-X_min
      Y_ran = Y_max-Y_min
      n_X = ceiling(X_ran/bin_side_length)
      n_Y = ceiling(Y_ran/bin_side_length)
      X_min = X_mid - (n_X/2.0d0)*bin_side_length
      Y_min = Y_mid - (n_Y/2.0d0)*bin_side_length
      X_ran = X_ran/n_X; Y_ran = Y_ran/n_Y
       ! Now do the binning ...
      do i = 1,n_X
      do j = 1,n_Y
         X_range(1) = X_min + (i-1)*bin_side_length
         X_range(2) = X_range(1)  + bin_side_length
         Y_range(1) = Y_min + (j-1)*bin_side_length
         Y_range(2) = Y_range(1)  + bin_side_length
         c = count(X_range(1)<=X .and. X<=X_range(2) .and. Y_range(1)<=Y .and. Y<=Y_range(2))
         do k = 1,size(self)
            if (is_in_range_(X(k),X_range) .and. is_in_range_(Y(k),Y_range)) then
            self(k) = c
            end if
         end do
      end do
      end do

   end subroutine

end
