!---------------------------------------------------------------------------
!
!  REALVEC: Vector operations ...
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
! $Id: realvec.foo,v 1.38.2.7 2003/11/13 05:36:07 reaper Exp $
!---------------------------------------------------------------------------

module REALVEC_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

   use INTVEC_MODULE, only: create_
   use INTVEC_MODULE, only: destroy_

   use STR_MODULE, only: is_known_unit_
   use STR_MODULE, only: conversion_factor_

   use INT_MODULE, only: is_even_
   use INT_MODULE, only: double_factorial_
   use INT_MODULE, only: n_comp_
   use INT_MODULE, only: make_gaussian_xyz_powers_

   use REAL_MODULE, only: same_as_
   use REAL_MODULE, only: equals_
   use REAL_MODULE, only: swap_with_
   use REAL_MODULE, only: is_zero_
   use REAL_MODULE, only: to_str_
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

   public    minimise_powell_
   interface minimise_powell_
      module procedure minimise_powell
   end interface

   public    index_of_maximum_
   interface index_of_maximum_
      module procedure index_of_maximum
   end interface

   public    line_minimise_from_
   interface line_minimise_from_
      module procedure line_minimise_from
   end interface

   public    distance_to_
   interface distance_to_
      module procedure distance_to
   end interface

   public    same_as_
   interface same_as_
      module procedure same_as
   end interface

   public    seitz_multiply_
   interface seitz_multiply_
      module procedure seitz_multiply
   end interface

   public    minimise_FRPR_
   interface minimise_FRPR_
      module procedure minimise_FRPR
   end interface

   public    rotate_
   interface rotate_
      module procedure rotate
   end interface

   public    convert_to_
   interface convert_to_
      module procedure convert_to
   end interface

   public    zero_small_values_
   interface zero_small_values_
      module procedure zero_small_values
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

   public    in_range_
   interface in_range_
      module procedure in_range
   end interface

   public    find_isopoint_
   interface find_isopoint_
      module procedure find_isopoint
   end interface

   public    sort_
   interface sort_
      module procedure sort
   end interface

   public    copy_
   interface copy_
      module procedure copy
   end interface

   public    outer_product_
   interface outer_product_
      module procedure outer_product
   end interface

   public    variance_
   interface variance_
      module procedure variance
   end interface

   public    arcsinh_
   interface arcsinh_
      module procedure arcsinh
   end interface

   public    all_in_range_
   interface all_in_range_
      module procedure all_in_range
   end interface

   public minimise_BFGS
   public    minimise_BFGS_
   interface minimise_BFGS_
      module procedure minimise_BFGS
   end interface

   public    is_z_axis_
   interface is_z_axis_
      module procedure is_z_axis
   end interface

   public    equals_
   interface equals_
      module procedure equals
   end interface

   public    find_opposite_pairs_
   interface find_opposite_pairs_
      module procedure find_opposite_pairs
   end interface

   public    range_
   interface range_
      module procedure range
   end interface

   public    bracket_minimum_
   interface bracket_minimum_
      module procedure bracket_minimum
   end interface

   public    set_alpha_
   interface set_alpha_
      module procedure set_alpha
   end interface

   public    destroy_
   interface destroy_
      module procedure destroy
   end interface

   public    cross_
   interface cross_
      module procedure cross
   end interface

   public    minus_product_of_
   interface minus_product_of_
      module procedure minus_product_of
   end interface

   public    minimise_golden_
   interface minimise_golden_
      module procedure minimise_golden
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

   public    find_pairs_
   interface find_pairs_
      module procedure find_pairs
   end interface

   public    rotate_by_
   interface rotate_by_
      module procedure rotate_by
   end interface

   public    standard_deviation_
   interface standard_deviation_
      module procedure standard_deviation
   end interface

   public    bracket_root_
   interface bracket_root_
      module procedure bracket_root
   end interface

   public    norm_
   interface norm_
      module procedure norm
   end interface

   public    to_str_
   interface to_str_
      module procedure to_str
   end interface

   public    plus_product_of_
   interface plus_product_of_
      module procedure plus_product_of
   end interface

   public    set_beta_
   interface set_beta_
      module procedure set_beta
   end interface

   public    to_product_of_
   interface to_product_of_
      module procedure to_product_of
      module procedure to_product_of_1
   end interface

   public    swap_elements_
   interface swap_elements_
      module procedure swap_elements
   end interface

   public    convert_from_
   interface convert_from_
      module procedure convert_from
   end interface

   public    shrink_
   interface shrink_
      module procedure shrink
   end interface

   public    index_of_smallest_value_
   interface index_of_smallest_value_
      module procedure index_of_smallest_value
   end interface

   public    normalise_
   interface normalise_
      module procedure normalise
   end interface

   public    minimise_
   interface minimise_
      module procedure minimise
   end interface

   public    translate_
   interface translate_
      module procedure translate
   end interface

   public    minimise_brent_
   interface minimise_brent_
      module procedure minimise_brent
   end interface

   public    are_all_equal_
   interface are_all_equal_
      module procedure are_all_equal
   end interface

   public    minus_
   interface minus_
      module procedure minus
   end interface

   public    to_cross_product_
   interface to_cross_product_
      module procedure to_cross_product
   end interface

   public    mean_
   interface mean_
      module procedure mean
   end interface

   public    index_of_largest_value_
   interface index_of_largest_value_
      module procedure index_of_largest_value
   end interface

   public    normalising_factors_
   interface normalising_factors_
      module procedure normalising_factors
   end interface

   public    chop_large_values_
   interface chop_large_values_
      module procedure chop_large_values
   end interface

   public    create_
   interface create_
      module procedure create
      module procedure create_1
      module procedure create_2
   end interface

   public    plus_scaled_vec_
   interface plus_scaled_vec_
      module procedure plus_scaled_vec
   end interface

   public    create_copy_
   interface create_copy_
      module procedure create_copy
   end interface

   public    alpha_
   interface alpha_
      module procedure alpha
   end interface

   public    reverse_order_
   interface reverse_order_
      module procedure reverse_order
   end interface

   public    dot_
   interface dot_
      module procedure dot
   end interface

   public    to_scaled_vec_
   interface to_scaled_vec_
      module procedure to_scaled_vec
   end interface

   public    beta_
   interface beta_
      module procedure beta
   end interface

   public    expand_
   interface expand_
      module procedure expand
   end interface

   public    find_root_brent_
   interface find_root_brent_
      module procedure find_root_brent
   end interface

   public    largest_value_
   interface largest_value_
      module procedure largest_value
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

   public    smallest_value_
   interface smallest_value_
      module procedure smallest_value
   end interface

contains

   subroutine create(self,dim)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Create space for object
      pointer :: self
      integer(kind=kind(1)), intent(in) :: dim
    ! The following code is inherited from INTRINSICVEC

      call ensure_(tonto,dim>=0,"REALVEC:create ... dimension of array not 1 or greater")
      nullify(self)
      allocate(self(dim))

   end subroutine

   subroutine create_1(self,lb,ub)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Create the vector with lower bound "lb", upper bound "ub"
      pointer :: self
      integer(kind=kind(1)), intent(in) :: lb,ub

      nullify(self)
      allocate(self(lb:ub))

    ! The following code is inherited from INTRINSICVEC

   end subroutine

   subroutine create_2(self,bounds)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Create the vector with "bounds"
      pointer :: self
      integer(kind=kind(1)), dimension(2), intent(in) :: bounds

      call create_(self,bounds(1),bounds(2))
    ! The following code is inherited from OBJECTVEC

   end subroutine

   subroutine destroy(self)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Destroy space for object
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

   subroutine create_copy(self,vec)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Create a replica copy of vec.
      real(kind=kind(1.0d0)), dimension(:), intent(in) :: vec
      pointer :: self

      call create_(self,size(vec))
      self = vec
    ! The following code is inherited from INTRINSICVEC

   end subroutine

   subroutine copy(self,vec)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Copy a vector "vec".
      real(kind=kind(1.0d0)), dimension(:), intent(in) :: vec

      self = vec

   end subroutine

   subroutine shrink(self,dim)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Shrink self to dimension dim.  Contents are retained.
     pointer :: self
     integer(kind=kind(1)), intent(in) :: dim
    ! The following code is inherited from INTRINSICVEC
     real(kind=kind(1.0d0)), dimension(:), pointer :: old
     integer(kind=kind(1)) :: n

     call ensure_(tonto,associated(self),"REALVEC:shrink ... no self array")
     call ensure_(tonto,dim<=size(self),"REALVEC:shrink ... dim too large")
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
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Expand self to dimension dim. New slots are left undefined.
     pointer :: self
     integer(kind=kind(1)), intent(in) :: dim
    ! The following code is inherited from INTRINSICVEC
     real(kind=kind(1.0d0)), dimension(:), pointer :: old
     integer(kind=kind(1)) :: old_dim

     if (.not. associated(self)) then
        call create_(self,dim)
     else
        call ensure_(tonto,dim>=size(self),"REALVEC:expand ... dim not large enough")
        old => self
        old_dim = size(old)
        nullify(self)
        call create_(self,dim)
        self(1:old_dim) = old
        call destroy_(old)
     end if

   end subroutine

   subroutine append(self,v)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Expands self and appends the contents of vector "v".
     pointer :: self
     real(kind=kind(1.0d0)), dimension(:), intent(in) :: v
    ! The following code is inherited from INTRINSICVEC
     integer(kind=kind(1)) :: dim

     if (.not. associated(self)) then; dim = 0
     else;                 dim = size(self)
     end if
     call expand_(self,dim+size(v))
     self(dim+1:) = v

   end subroutine

   subroutine append_1(self,value)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Expands self by 1, and appends the single scalar "value" onto the end.
     pointer :: self
     real(kind=kind(1.0d0)), intent(in) :: value
    ! The following code is inherited from INTRINSICVEC
     integer(kind=kind(1)) :: dim

     if (.not. associated(self)) then; dim = 0
     else;                 dim = size(self)
     end if
     call expand_(self,dim+1)
     self(dim+1) = value

   end subroutine

   function join(self,v) result(res)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Yield a vector which is the concatenation of "self" and "v"
     real(kind=kind(1.0d0)), dimension(:), intent(in) :: v
     real(kind=kind(1.0d0)), dimension(:), pointer :: res
    ! The following code is inherited from INTRINSICVEC
     integer(kind=kind(1)) :: dim, dim_v

     dim   = size(self)
     dim_v = size(v)
     call create_(res,dim+dim_v)
     res(    1:dim      ) = self
     res(dim+1:dim+dim_v) = v

   end function

   function join_1(self,v1,v2) result(res)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Yield a vector which is the concatenation of "self" and "v1" and "v2"
     real(kind=kind(1.0d0)), dimension(:), intent(in) :: v1,v2
     real(kind=kind(1.0d0)), dimension(:), pointer :: res
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

!*******************************************************************************

   subroutine to_product_of(self,mat,vec,transpose)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Set "self" to the product of the matrix "mat" and vector "vec". If present,
    ! "transpose" can be set to .true. if the matrix needs to be transposed.
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: mat
      real(kind=kind(1.0d0)), dimension(:), intent(in) :: vec
      logical(kind=kind(.true.)), optional :: transpose
      integer(kind=kind(1)) :: i,k
      logical(kind=kind(.true.)) :: trans
      real(kind=kind(1.0d0)) :: temp

      trans = .false.
      if (present(transpose)) trans = transpose
      if (trans) then
        call ensure_(tonto,size(mat,2)==size(self),"REALVEC:to_product_of ... array dimensions do not agree")
        call ensure_(tonto,size(mat,1)==size(vec),"REALVEC:to_product_of ... array dimensions do not agree")
        do i = 1,size(self)
          temp = mat(1,i) * vec(1)
          do k=2,size(vec)
            temp = temp + mat(k,i) * vec(k)
          end do
          self(i) = temp
        end do
      else
        call ensure_(tonto,size(mat,1)==size(self),"REALVEC:to_product_of ... array dimensions do not agree")
        call ensure_(tonto,size(mat,2)==size(vec),"REALVEC:to_product_of ... array dimensions do not agree")
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
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Set "self" to the matrix product of "mat_a" and "mat_b". If present,
    ! "transpose_a" and "transpose_b" can be set to .true. if "mat_a" and "mat_b"
    ! neeb to be transposed.
     real(kind=kind(1.0d0)), dimension(:,:) :: mat_a, mat_b
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
         call ensure_(tonto,dim1==dim_a1,"REALVEC:to_product_of_1 ... array dimensions do not agree")
         call ensure_(tonto,dim_a2==dim_b1,"REALVEC:to_product_of_1 ... array dimensions do not agree")
         call ensure_(tonto,dim_b2==1,"REALVEC:to_product_of_1 ... b array dimensions incorrect")
         do i=1,dim1
           temp = 0.0d0
           do k=1,dim_a2
             temp = temp + mat_a(i,k) * mat_b(k,1)
           end do
           self(i) = temp
         end do
       case (1)
         call ensure_(tonto,dim1==dim_a2,"REALVEC:to_product_of_1 ... array dimensions do not agree")
         call ensure_(tonto,dim_a1==dim_b1,"REALVEC:to_product_of_1 ... array dimensions do not agree")
         call ensure_(tonto,dim_b2==1,"REALVEC:to_product_of_1 ... b array dimension incorrect")
         do i=1,dim1
           temp = 0.0d0
           do k=1,dim_a1
             temp = temp + mat_a(k,i) * mat_b(k,1)
           end do
           self(i) = temp
         end do
       case (2)
         call ensure_(tonto,dim1==dim_a1,"REALVEC:to_product_of_1 ... array dimensions do not agree")
         call ensure_(tonto,dim_a2==dim_b2,"REALVEC:to_product_of_1 ... array dimensions do not agree")
         call ensure_(tonto,dim_b1==1,"REALVEC:to_product_of_1 ... b array dimension incorrect")
         do i=1,dim1
           temp = 0.0d0
           do k=1,dim_a2
             temp = temp + mat_a(i,k) * mat_b(1,k)
           end do
           self(i) = temp
         end do
       case (3)
         call ensure_(tonto,dim1==dim_a2,"REALVEC:to_product_of_1 ... array dimensions do not agree")
         call ensure_(tonto,dim_a1==dim_b2,"REALVEC:to_product_of_1 ... array dimensions do not agree")
         call ensure_(tonto,dim_b1==1,"REALVEC:to_product_of_1 ... b array dimension incorrect")
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
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Add to "self" the product of the matrix and vector. If present,
    ! "transpose" can be set to .true. if the matrix needs to be transposed.
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: mat
      real(kind=kind(1.0d0)), dimension(:), intent(in) :: vec
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
        call ensure_(tonto,dim1==dim_a2,"REALVEC:plus_product_of ... array dimensions do not agree")
        call ensure_(tonto,dim_b1==dim_a1,"REALVEC:plus_product_of ... array dimensions do not agree")
        do i=1,dim1
          temp = mat(1,i) * vec(1)
          do k=2,dim_b1
            temp = temp + mat(k,i) * vec(k)
          end do
          self(i) = self(i) + temp
        end do
      else
        call ensure_(tonto,dim1==dim_a1,"REALVEC:plus_product_of ... array dimensions do not agree")
        call ensure_(tonto,dim_b1==dim_a2,"REALVEC:plus_product_of ... array dimensions do not agree")
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
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Subtract from "self" the product of the matrix and vector. If present,
    ! "transpose" can be set to .true. if the matrix needs to be transposed.
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: mat
      real(kind=kind(1.0d0)), dimension(:), intent(in) :: vec
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
        call ensure_(tonto,dim1==dim_a2,"REALVEC:minus_product_of ... array dimensions do not agree")
        call ensure_(tonto,dim_b1==dim_a1,"REALVEC:minus_product_of ... array dimensions do not agree")
        do i=1,dim1
          temp = mat(1,i) * vec(1)
          do k=2,dim_b1
            temp = temp + mat(k,i) * vec(k)
          end do
          self(i) = self(i) - temp
        end do
      else
        call ensure_(tonto,dim1==dim_a1,"REALVEC:minus_product_of ... array dimensions do not agree")
        call ensure_(tonto,dim_b1==dim_a2,"REALVEC:minus_product_of ... array dimensions do not agree")
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
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Return the dot product with "b"
      intent(in) :: self
      real(kind=kind(1.0d0)), dimension(:), intent(in) :: b
      real(kind=kind(1.0d0)) :: res
    ! The following code is inherited from INTRINSICVEC
      res = dot_product(self,b)

   end function

   pure function cross(self,b) result(res)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Return the cross product of "self" and "b".
      intent(in) :: self
      real(kind=kind(1.0d0)), dimension(:), intent(in) :: b
      real(kind=kind(1.0d0)), dimension(3) :: res
    ! The following code is inherited from INTRINSICVEC

      res(1) = self(2)*b(3) - b(2)*self(3)
      res(2) = self(3)*b(1) - b(3)*self(1)
      res(3) = self(1)*b(2) - b(1)*self(2)

   end function

   pure subroutine to_cross_product(self,a,b)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Set the vector to the cross product of "a" and "b".
      intent(inout) :: self
      real(kind=kind(1.0d0)), dimension(:), intent(in) :: a,b
    ! The following code is inherited from INTRINSICVEC

      self(1) = a(2)*b(3) - b(2)*a(3)
      self(2) = a(3)*b(1) - b(3)*a(1)
      self(3) = a(1)*b(2) - b(1)*a(2)

   end subroutine

   pure subroutine reverse_order(self)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Reverse the order of the elements of self
     intent(inout) :: self
    ! The following code is inherited from OBJECTVEC
     integer(kind=kind(1)) :: n,dim
     dim = size(self)
     do n = 1,dim/2
       call swap_elements_(self,n,dim-n+1)
     end do

   end subroutine

   function equals(self,b,tol) result(res)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Return true if "self" is the same as "b", within range "tol" if supplied
      intent(in) :: self
      real(kind=kind(1.0d0)), dimension(:), intent(in) :: b
      real(kind=kind(1.0d0)), optional, intent(in) :: tol
      logical(kind=kind(.true.)) :: res

      res = same_as_(self,b,tol)

   end function

   function same_as(self,b,tol) result(res)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Return true if "self" is the same as "b", within range "tol" if supplied
      intent(in) :: self
      real(kind=kind(1.0d0)), dimension(:), intent(in) :: b
      real(kind=kind(1.0d0)), optional, intent(in) :: tol
      logical(kind=kind(.true.)) :: res
      real(kind=kind(1.0d0)) :: tolerance
      real(kind=kind(1.0d0)), dimension(size(b)) :: tmp

      if (size(self)/=size(b)) then
         res = .false.
      else
         tolerance = 10.0d0**(-6)
         if (present(tol)) tolerance = tol
         tmp = self - b
         tmp = abs(tmp)
         if (any(tmp>tolerance)) then
            res = .false.
         else
            res = dot_product(tmp,tmp)<tolerance*tolerance
         end if
      end if

   end function

   function are_all_equal(self,tol) result(res)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Return .true. if "self" contains qall the same elements, to within precision
    ! "tol", if supplied.
      intent(in) :: self
      real(kind=kind(1.0d0)), optional, intent(in) :: tol
      logical(kind=kind(.true.)) :: res
      integer(kind=kind(1)) :: i

      res = .true.
      do i = 2,size(self)
         if (equals_(self(1),self(i),tol)) cycle
         res = .false.
         exit
      end do

   end function

   pure subroutine to_scaled_vec(self,fac,b)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Set the vector to "b" scaled by "fac"
      intent(inout) :: self
      real(kind=kind(1.0d0)), dimension(:), intent(in) :: b
      real(kind=kind(1.0d0)), intent(in) :: fac
    ! The following code is inherited from INTRINSICVEC
      self = fac*b

   end subroutine

   pure subroutine plus_scaled_vec(self,fac,b)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Add a vector "b" scaled by "fac" to "self"
      intent(inout) :: self
      real(kind=kind(1.0d0)), dimension(:), intent(in) :: b
      real(kind=kind(1.0d0)), intent(in) :: fac
    ! The following code is inherited from INTRINSICVEC
      self = self + fac*b

   end subroutine

   pure subroutine minus(self,b,mask)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Subtract vector "b" from "self"
      intent(inout) :: self
      real(kind=kind(1.0d0)), dimension(:), intent(in) :: b
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
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Add vector "b" to "self"
      intent(inout) :: self
      logical(kind=kind(.true.)), dimension(:), intent(in), optional :: mask
      real(kind=kind(1.0d0)), dimension(:), intent(in) :: b
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
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Set the vector to "b". See also the "copy" routine.
      intent(inout) :: self
      real(kind=kind(1.0d0)), dimension(:), intent(in) :: b
    ! The following code is inherited from INTRINSICVEC
      self = b

   end subroutine

   pure subroutine swap_elements(self,e1,e2)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Swap elements "e1" and "e2" in "self".
      intent(inout) :: self
      integer(kind=kind(1)), intent(in) :: e1,e2
    ! The following code is inherited from INTRINSICVEC
      real(kind=kind(1.0d0)) :: val

      val = self(e1)
      self(e1) = self(e2)
      self(e2) = val

   end subroutine

   pure function index_of_first_nonzero_value(self,eps) result(res)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Returns the index of the first nonzero component of self.
     intent(in) :: self
     real(kind=kind(1.0d0)), intent(in), optional :: eps
     integer(kind=kind(1)) :: res
     integer(kind=kind(1)) :: i
     res=0
     do i=1,size(self)
       if (.not. is_zero_(self(i),eps)) then
         res=i
         exit
       end if
     end do

   end function

   function index_of_first_zero_value(self) result(res)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Returns the index of the first zero component of self.
     intent(in) :: self
     integer(kind=kind(1)) :: res
     integer(kind=kind(1)) :: i

     res=0
     do i=1,size(self)
       if (is_zero_(self(i))) then
         res=i
         exit
       end if
     end do

   end function

   pure function index_of_value(self,val,eps) result(pos)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Returns the first index in "self" which has the value "val", or 0 if "val"
    ! is not present in the array. If present, "eps" is used to test equality
    ! with "val".
     intent(in) :: self
     real(kind=kind(1.0d0)), intent(in) :: val
     real(kind=kind(1.0d0)), optional, intent(in) :: eps
     integer(kind=kind(1)) :: pos
     integer(kind=kind(1)) :: i
     pos = 0
     do i = 1,size(self)
        if (same_as_(self(i),val,eps)) then
           pos = i
           exit
        end if
     end do

   end function

   pure subroutine chop_large_values(self,val)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Set all values in the self whose absolute value is larger than "val" to
    ! "val" times the sign of the original value.
      intent(inout) :: self
      real(kind=kind(1.0d0)), intent(in) :: val
    ! The following code is inherited from INTRINSICVEC
      integer(kind=kind(1)) :: dim,i
      real(kind=kind(1.0d0)) :: bb,ba,sign
      dim = size(self)
      do i = 1,dim
         bb = self(i)
         if (bb==0.0d0) cycle
         ba = abs(bb)
         sign = bb/ba
         self(i) = sign*min(val,ba)
     end do

   end subroutine

!   maximum result (val) ::: pure
!   ! Return the maximum value in the vector
!      self :: intent(in)
!      val :: ELEMENT_TYPE
!      val = maxval(self)
!   end

!   minimum result (val) ::: pure
!   ! Return the minimum value in the vector
!      self :: intent(in)
!      val :: ELEMENT_TYPE
!      val = minval(self)
!   end

   pure function index_of_maximum(self) result(ind)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Return the index of the maximum in the vector
      intent(in) :: self
      integer(kind=kind(1)) :: ind
    ! The following code is inherited from INTRINSICVEC
      ind = maxval(maxloc(self))

   end function

   pure function index_of_minimum(self) result(ind)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Return the index of the minimum in the vector
      intent(in) :: self
      integer(kind=kind(1)) :: ind
    ! The following code is inherited from INTRINSICVEC
      ind = minval(minloc(self))

   end function

   pure function is_zero(self,eps) result(res)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Return true if the vector is zero (within "eps", if supplied)
      intent(in) :: self
      real(kind=kind(1.0d0)), intent(in), optional :: eps
      logical(kind=kind(.true.)) :: res
      real(kind=kind(1.0d0)) :: val
      val = norm_(self)
      res = is_zero_(val,eps)

   end function

   pure function all_in_range(self,range) result(res)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Return .true. if all values of self are within the specified "range".
      intent(in) :: self
      real(kind=kind(1.0d0)), dimension(2), intent(in) :: range
      logical(kind=kind(.true.)) :: res
      res = all(range(1) <= self .and. self <= range(2))
    ! The following code is inherited from INTRINSICVEC

   end function

   pure function in_range(self,range) result(res)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Return element i as .true. if self(i) is within the specified "range".
      intent(in) :: self
      real(kind=kind(1.0d0)), dimension(2), intent(in) :: range
      logical(kind=kind(.true.)), dimension(size(self)) :: res
      res = (range(1) <= self .and. self <= range(2))
    ! The following code is inherited from INTRINSICVEC

   end function

   pure function range(self) result(res)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Return the range (smallest and largest value) of self.
      intent(in) :: self
      real(kind=kind(1.0d0)), dimension(2) :: res
      res(1) = minval(self)
      res(2) = maxval(self)
    ! The following code is inherited from INTRINSICVEC

   end function

   function is_z_axis(self,eps) result(res)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Return true if the vector is set to the z-axis (within "eps", if supplied)
      real(kind=kind(1.0d0)), optional :: eps
      logical(kind=kind(.true.)) :: res

      call ensure_(tonto,size(self)==3,"REALVEC:is_z_axis ... must supply a 3 dimensional vector!")
      res = is_zero_(self(1),eps)
      res = is_zero_(self(2),eps) .and. res
      res = is_zero_((1.0d0-self(3)),eps) .and. res

   end function

   pure function largest_value(self) result(maxval)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Return the maximum absolute value in the vector
      intent(in) :: self
      real(kind=kind(1.0d0)) :: maxval
      real(kind=kind(1.0d0)) :: val
    ! The following code is inherited from INTRINSICVEC
      integer(kind=kind(1)) :: i
      maxval = abs(self(1))
      do i = 2,size(self)
        val = abs(self(i))
        if (val > maxval) maxval = val
      end do

   end function

   pure function smallest_value(self) result(minval)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Return minimum absolute value in the vector
      intent(in) :: self
      real(kind=kind(1.0d0)) :: minval
      real(kind=kind(1.0d0)) :: val
    ! The following code is inherited from INTRINSICVEC
      integer(kind=kind(1)) :: i
      minval = abs(self(1))
      do i = 2,size(self)
        val = abs(self(i))
        if (val < minval) minval = val
      end do

   end function

   pure function index_of_largest_value(self) result(ind)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Return the index "ind" of the largest absolute value in the vector
      intent(in) :: self
      integer(kind=kind(1)) :: ind
    ! The following code is inherited from INTRINSICVEC
      integer(kind=kind(1)) :: i
      real(kind=kind(1.0d0)) :: maxval,val
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
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Return the index "ind" of the smallest value in the vector
      intent(in) :: self
      integer(kind=kind(1)) :: ind
    ! The following code is inherited from INTRINSICVEC
      integer(kind=kind(1)) :: i
      real(kind=kind(1.0d0)) :: minval,val
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
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Return the number of elements larger than "tol".
      intent(in) :: self
      real(kind=kind(1.0d0)), intent(in) :: tol
      integer(kind=kind(1)) :: res
    ! The following code is inherited from INTRINSICVEC
      res = count(self>tol)

   end function

   pure subroutine normalising_factors(self,l)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Return the normalising factors for cartesian gaussian basis functions used
    ! in quantum chemical calculations.
     integer(kind=kind(1)), intent(in) :: l
     intent(out) :: self
     integer(kind=kind(1)), dimension(:,:), allocatable :: xyz_momenta
     real(kind=kind(1.0d0)) :: fac
     integer(kind=kind(1)) :: i,ncomp

     if (l<2) then
       self = 1.0d0
     else if (l==2) then
       self(1)=1.0d0
       self(2)=1.0d0
       self(3)=1.0d0
       self(4)=sqrt(3.0d0)
       self(5)=sqrt(3.0d0)
       self(6)=sqrt(3.0d0)
     else
       ncomp=n_comp_(l)
       allocate(xyz_momenta(3,ncomp))
       call make_gaussian_xyz_powers_(l,xyz_momenta)
       do i=1,ncomp
         fac = double_factorial_(l) / &
               (double_factorial_(xyz_momenta(1,i))   &
               *double_factorial_(xyz_momenta(2,i))   &
               *double_factorial_(xyz_momenta(3,i)))
         self(i)=sqrt(fac)
       end do
       deallocate(xyz_momenta)
     end if

   end subroutine

   pure function norm(self) result(res)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Return the norm of the vector
      intent(in) :: self
      real(kind=kind(1.0d0)) :: res
      res = sqrt(sum(self*self))

   end function

   subroutine normalise(self)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Normalise the vector
      intent(inout) :: self

      self = self/norm_(self)

   end subroutine

   function distance_to(self,b) result(res)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Return the distance to "b" if supplied
      real(kind=kind(1.0d0)), dimension(:), intent(in) :: b
      real(kind=kind(1.0d0)) :: res
      real(kind=kind(1.0d0)) :: temp
      integer(kind=kind(1)) :: i

      call ensure_(tonto,size(self)==size(b),"REALVEC:distance_to ... incompatible dimensions")
      res = 0.0d0
      do i = 1,size(self)
         temp = self(i)-b(i)
         res = res + temp*temp
      end do
      res = sqrt(res)

   end function

   pure subroutine zero_small_values(self,tol)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Zero elements of the vector which are less than "tol" in magnitude
      intent(inout) :: self
      real(kind=kind(1.0d0)), intent(in) :: tol
      where (abs(self)<tol)
        self = 0.0d0
      end where

   end subroutine

   pure function mean(self) result(res)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Return the mean of the vector
     intent(in) :: self
     real(kind=kind(1.0d0)) :: res
     res = sum(self)/size(self)

   end function

   pure function variance(self) result(res)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Return the variance of the vector from its mean
     intent(in) :: self
     real(kind=kind(1.0d0)) :: res
     real(kind=kind(1.0d0)) :: mean
     mean = mean_(self)
     res = (sum(self*self))/size(self)-mean*mean

   end function

   pure function standard_deviation(self) result(res)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Return the standard deviation of the vector from its mean
     intent(in) :: self
     real(kind=kind(1.0d0)) :: res
     real(kind=kind(1.0d0)) :: variance
     variance = variance_(self)
     if (variance/=0.0d0) res = sqrt(variance)

   end function

   function arcsinh(self) result(res)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Return the arcsinh of self, where self is a vector of any real numbers.
      intent(in) :: self
      real(kind=kind(1.0d0)), dimension(size(self)) :: res

      res = log(self + sqrt(1.0d0+self*self))

   end function

   function alpha(self) result(res)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! return the alpha sector of the vector
      TARGET :: self
      real(kind=kind(1.0d0)), dimension(:), pointer :: res
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_even_(size(self)),"REALVEC:alpha ... self is not even-dimensioned")
      n = size(self)/2
      res => self(1:n)

   end function

   function beta(self) result(res)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! return the beta sector of the vector
      TARGET :: self
      real(kind=kind(1.0d0)), dimension(:), pointer :: res
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_even_(size(self)),"REALVEC:beta ... self is not even-dimensioned")
      n = size(self)/2
      res => self(n+1:2*n)

   end function

   subroutine set_alpha(self,X)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Set the alpha sector of the vector
      real(kind=kind(1.0d0)), dimension(:) :: X
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_even_(size(self)),"REALVEC:set_alpha ... self is not even-dimensioned")
      n = size(self)/2
      self(1:n) = X

   end subroutine

   subroutine set_beta(self,X)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Set the beta sector of the vector
      real(kind=kind(1.0d0)), dimension(:) :: X
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_even_(size(self)),"REALVEC:set_beta ... self is not even-dimensioned")
      n = size(self)/2
      self(n+1:2*n) = X

   end subroutine

!   integrate(a,b,accuracy) result(res) ::: recursive, functional
!   ! Integrate the vector valued scalar function "self" between the limits
!   ! "a" and "b" using adaptive trapezoidal rule with Simpsons approximation.
!   ! If present, "accuracy" is the required accuracy of the integral.
!      interface
!         self(x) result(res)
!             x :: real(kind=kind(1.0d0))
!            res :: SELF_TYPE*
!         end
!      end
!      a,b :: real(kind=kind(1.0d0))
!      accuracy :: real(kind=kind(1.0d0)), optional
!      res :: SELF_TYPE*
!       n :: integer(kind=kind(1))
!      same :: logical(kind=kind(.true.))
!      tol,h,m :: real(kind=kind(1.0d0))
!      fa,fb,fm,one_trap,two_trap,left,right :: SELF_TYPE,pointer
!      tol = 10.0d0**(-6)
!      if (present(accuracy)) tol = accuracy
!      h  = b-a
!      m  = (a+b)/2.0d0
!      fa => self(a)
!      fb => self(b)
!      fm => self(m)
!      n = size(fa)
!      one_trap.create(n)
!      two_trap.create(n)
!      one_trap = h*(fa+fb)/2.0d0
!      two_trap = h*(fa+2.0d0*fm+fb)/4.0d0
!      fm.destroy
!      fb.destroy
!      fa.destroy
!      res.create(n)
!      res = abs(one_trap-two_trap)
!      same = maxval(res) < 3.0d0*tol
!      if (same) then
!         res = (4.0d0*two_trap - one_trap)/3.0d0
!         two_trap.destroy
!         one_trap.destroy
!      else
!         two_trap.destroy
!         one_trap.destroy
!         left  => .integrate(a,m,tol/2.0d0)
!         right => .integrate(m,b,tol/2.0d0)
!         res = left + right
!         right.destroy
!         left.destroy
!      end
!   end

   subroutine seitz_multiply(self,seitz)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Self is operated on by the seitz matrix.  Self must be in fractional
    !  coordinates.
     intent(inout) :: self
     real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: seitz

     call ensure_(tonto,size(seitz,1)==4,"REALVEC:seitz_multiply ... seitz matrix must be 4x4")
     call ensure_(tonto,size(seitz,2)==4,"REALVEC:seitz_multiply ... seitz matrix must be 4x4")
     call ensure_(tonto,size(self)==3,"REALVEC:seitz_multiply ... vector not of dimension 3")
     call rotate_(self,seitz(1:3,1:3))
     call translate_(self,seitz(4,1:3))

   end subroutine

   subroutine translate(self,vector)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Translate self by vector.
     intent(inout) :: self
     real(kind=kind(1.0d0)), dimension(:), intent(in) :: vector
    ! The following code is inherited from INTRINSICVEC

     call ensure_(tonto,size(self)==size(vector),"REALVEC:translate ... vectors not of same dimension")
     self = self + vector

   end subroutine

   subroutine rotate(self,matrix)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Rotate self by the rotation matrix
     intent(inout) :: self
     real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: matrix

     call ensure_(tonto,size(matrix,1)==size(matrix,2),"REALVEC:rotate ... matrix must be square")
     call ensure_(tonto,size(matrix,2)==size(self),"REALVEC:rotate ... matrix and vector dimensions inconsistent")
     self = matmul(matrix,self)

   end subroutine

   subroutine rotate_by(self,matrix)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Rotate self by the rotation matrix, treating self as a column vector
     intent(inout) :: self
     real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: matrix

     call ensure_(tonto,size(matrix,1)==size(matrix,2),"REALVEC:rotate_by ... matrix must be square")
     call ensure_(tonto,size(matrix,2)==size(self),"REALVEC:rotate_by ... matrix and vector dimensions inconsistent")
     self = matmul(matrix,self)

   end subroutine

   function outer_product(self,b) result(res)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Returns the outer product of self with b.
     intent(in) :: self
     real(kind=kind(1.0d0)), dimension(:), intent(in) :: b
     real(kind=kind(1.0d0)), dimension(size(b),size(self)) :: res

     res = spread(self,2,size(b)) * spread(b,1,size(self))

   end function

   pure subroutine sort(self,decreasing_order)
    real(kind=kind(1.0d0)), dimension(:) :: self
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
    real(kind=kind(1.0d0)), dimension(:) :: self
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
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Sort the vector into order, smallest to largest
    ! The following code is inherited from OBJECTVEC
      real(kind=kind(1.0d0)), dimension(:), pointer :: smaller,larger
      integer(kind=kind(1)) :: n, ns, ne, nl
      real(kind=kind(1.0d0)) :: chosen

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
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Sort the vector into order, largest to smallest
    ! The following code is inherited from OBJECTVEC
      real(kind=kind(1.0d0)), dimension(:), pointer :: smaller,larger
      integer(kind=kind(1)) :: n, ns, ne, nl
      real(kind=kind(1.0d0)) :: chosen

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
    real(kind=kind(1.0d0)), dimension(:) :: self
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

      call ensure_(tonto,size(indices)==size(self),"REALVEC:quick_sort_1 ... wrong size, indices")
      decreasing = .false.
      if (present(decreasing_order)) decreasing = decreasing_order
      indices = (/(i,i=1,size(self))/)  ! initialise indices
      if (.not. decreasing) then; call quick_sort_increasing_(self,indices)
      else;                     call quick_sort_decreasing_(self,indices)
      end if

   end subroutine

   recursive subroutine quick_sort_increasing_1(self,indices)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Return the indices which sort vector from smallest to largest, i.e. on
    ! return "self(indices)" is sorted. NOTE: self is *not* sorted.
      intent(in) :: self
      integer(kind=kind(1)), dimension(:), intent(inout) :: indices
    ! The following code is inherited from OBJECTVEC
      integer(kind=kind(1)), dimension(:), pointer :: list,small,equal,large,small_indices,equal_indices,large_indices
      integer(kind=kind(1)) :: n, i, ns, ne, nl
      real(kind=kind(1.0d0)) :: chosen

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
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Return the indices which sort vector from largest to smallest, i.e. on
    ! return "self(indices)" is sorted. NOTE: self is *not* sorted.
      intent(in) :: self
      integer(kind=kind(1)), dimension(:), intent(inout) :: indices
    ! The following code is inherited from OBJECTVEC
      integer(kind=kind(1)), dimension(:), pointer :: list,small,equal,large,small_indices,equal_indices,large_indices
      integer(kind=kind(1)) :: n, i, ns, ne, nl
      real(kind=kind(1.0d0)) :: chosen

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

   subroutine find_opposite_pairs(self,pair,min,max)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! "pair(i)" returns the location of the element which has the opposite
    ! value to self(i) *and* is also negative, i.e. self(pair(i)) = -self(i) < 0.
    ! If no such match can be found, pair(i) is set to 0.
    ! unless self(i) is greater than max, in which case pair(i) is set to -1.
      integer(kind=kind(1)), dimension(:) :: pair
      real(kind=kind(1.0d0)) :: min,max
      integer(kind=kind(1)) :: i,j

      pair = 0
      do i = 1,size(self)
         if      (abs(self(i))<min) then; pair(i) =  0
         else if (abs(self(i))>max) then
             if (self(i)<0)  pair(i) = -1
             if (self(i)>0)  pair(i) = -2
         else
            do j = 1,i-1
               if (any(pair==j)) cycle
               if (abs(self(j)+self(i))<min) then
                  if (self(i)>0) then; pair(i)=j
                  else;                pair(i)=0
                  end if
               end if
            end do
         end if
      end do

   end subroutine

   subroutine find_pairs(self,pair,match_function,tol)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Given a vector "self" find pairs of values self(i) and self(pair(i))
    ! where: match_function(self(i),self(pair(i))) = 0. If no such pair can be
    ! found, then pair(i) is set to 0. If more than one match is found then the
    ! first match from the start of the list is the one that is paired.
      integer(kind=kind(1)), dimension(:) :: pair
      interface
         function match_function(arg1,arg2) result(res)
            real(kind=kind(1.0d0)) :: arg1,arg2,res
         end function
      end interface
      real(kind=kind(1.0d0)), optional :: tol
      integer(kind=kind(1)) :: i,j,dim
      real(kind=kind(1.0d0)) :: val

      dim = size(self)
      pair = 0
      do i = 1,dim
      do j = 1,dim
         val = match_function(self(i),self(j))
         if (is_zero_(val,tol) .and. self(i)>self(j) .and. .not. any(pair==j)) then
            pair(i) = j
            pair(j) = i
            exit
         end if
      end do
      end do

   end subroutine

   subroutine convert_to(self,units)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Convert the number "self" in atomic units or generic units to a
    ! new number in "units".
      intent(inout) :: self
      character(*), intent(in) :: units
      real(kind=kind(1.0d0)) :: factor

   call ensure_(tonto,is_known_unit_(units),"REALVEC:convert_to ... unknown units, " // units)
      factor = conversion_factor_(units)
      self = self * factor

   end subroutine

   subroutine convert_from(self,units)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Convert the number "self" from "units" system to a new number
    ! in atomic units or generic units.  Returns "err" whether it was successful.
      intent(inout) :: self
      character(*), intent(in) :: units
      real(kind=kind(1.0d0)) :: factor

   call ensure_(tonto,is_known_unit_(units),"REALVEC:convert_from ... unknown units, " // units)
      factor = 1.0d0/(conversion_factor_(units))
      self = self * factor

   end subroutine

   function to_str(self,format,separator) result(string)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Change self to a "string" using default format.
     character(*), optional :: format
     character(*), optional :: separator
     character(128) :: string
     character(128) :: str1,str2
     integer(kind=kind(1)) :: n

     string = " "
     do n = 1,size(self)
       str1 = to_str_(self(n),format)
       call ensure_(tonto,len_trim(string) + len_trim(str1) < len(string),"REALVEC:to_str ... string too long")
       if (present(separator)) then; str2 = trim(string) // separator // trim(str1)
       else;                         str2 = trim(string) //    " "    // trim(str1)
       end if
       string = str2
     end do

   end function

! *********************
! Root finding routines
! *********************

   subroutine bracket_root(self,z,direction,x1,x2,factor,val,max_it)
    ! Given a vector function self(z), an initial point "z" and a "direction",
    ! and initial distances "x1" and "x2" along "direction" from "z", bracket
    ! a root of self along "direction" by expansion. If "factor" is present it is
    ! used as the (linear) interval expansion factor. If "val" is present the
    ! values "x1" and "x2" will bracket the value x where self(x) = val.
    ! If "max_it" is present then it is the number of times the interval is
    ! expanded.
      interface
         function self(z) result(res)
             real(kind=kind(1.0d0)), dimension(:) :: z
            real(kind=kind(1.0d0)) :: res
         end function
      end interface
      real(kind=kind(1.0d0)), dimension(:) :: z,direction
      real(kind=kind(1.0d0)) :: x1,x2
      real(kind=kind(1.0d0)), optional :: factor,val
      integer(kind=kind(1)), optional :: max_it
      integer(kind=kind(1)) :: j
      real(kind=kind(1.0d0)) :: f1,f2
      real(kind=kind(1.0d0)) :: fac = 1.6d0
      real(kind=kind(1.0d0)) :: iso = 0.0d0
      integer(kind=kind(1)) :: maxit = 500

      call ensure_(tonto,x1/=x2,"REALVEC:bracket_root ... non-zero range (x1,x2) required")
      if (present(factor)) fac = factor
      if (present(val))    iso = val
      if (present(max_it)) maxit = max_it
      if (x1>x2) call swap_with_(x1,x2)
      f1 = self(z + x1*direction) - iso
      f2 = self(z + x2*direction) - iso
      do j = 1,maxit
         if (f1*f2<0.0d0) then;   return; end if
         if (abs(f1)<abs(f2)) then
            x1 = x1 + fac*(x1-x2)
            f1 = self(z + x1*direction) - iso
         else
            x2 = x2 + fac*(x2-x1)
            f2 = self(z + x2*direction) - iso
         end if
      end do
      call die_(tonto,"REALVEC:bracket_root ... Exceeded maximum number of iterations")

   end subroutine

   subroutine find_root_brent(self,z,direction,x1,x2,tol,root,val,max_it)
    ! Given a vector function self(x), an initial point "z", a "direction", and
    ! initial values "x1" and "x2" along this "direction" which bracket a root
    ! of self, return the "root" of self along "direction" to a precision "tol"
    ! using Brent's method. The point "z" is reset to be the vector point
    ! corresponding to this root. If "val" present, then root is set so that
    ! self(root) = val, i.e.  root is set top be an iso-value of self. If
    ! "max_it" is present it is set to the maximum number of iterations.
      interface
         function self(z) result(res)
             real(kind=kind(1.0d0)), dimension(:) :: z
            real(kind=kind(1.0d0)) :: res
         end function
      end interface
      real(kind=kind(1.0d0)), dimension(:) :: z,direction
      real(kind=kind(1.0d0)) :: x1,x2,tol
      real(kind=kind(1.0d0)), optional :: root,val
      integer(kind=kind(1)), optional :: max_it
      integer(kind=kind(1)) :: iter
      real(kind=kind(1.0d0)) :: a,b,c,d,e,fa,fb,fc,p,q,r,s,tol1,xm
      integer(kind=kind(1)) :: maxit = 100
      real(kind=kind(1.0d0)) :: iso = 0.0d0
      real(kind=kind(1.0d0)) :: eps = 10.0d0**(-8)

      if (present(max_it)) maxit = max_it
      if (present(val))    iso = val
      a  = x1
      b  = x2
      fa = self(z + a*direction) - iso
      fb = self(z + b*direction) - iso
      call ensure_(tonto,(fa>0 .and. fb<0) .or. (fa<0 .and. fb>0),"REALVEC:find_root_brent ... root is not bracketed")
      c = b
      fc = fb
      do iter = 1,maxit
         if ((fb>0 .and. fc>0) .or. (fb<0 .and. fc<0)) then
            c = a     ! Rename a,b,c and adjust bounding interval d
            fc = fa
            d = b - a
            e = d
         end if
         if (abs(fc)<abs(fb)) then
            a  = b ; b  = c ; c  = a
            fa = fb; fb = fc; fc = fa
         end if
          ! Test convergence
         tol1 = 2.0d0*eps*abs(b)+0.50d0*tol
         xm = 0.50d0*(c-b)
         if (abs(xm)<=tol1 .or. fb==0.0d0) then
            if (present(root)) root = b
            z = z + b*direction
              return
         end if
         if (abs(e)>=tol1 .and. abs(fa)>abs(fb)) then
            s = fb/fa  ! Attempt inverse quadratic interpolation
            if (a==c) then
               p = 2.0d0*xm*s
               q = 1.0d0 - s
            else
               q = fa/fc
               r = fb/fc
               p = s*(2.0d0*xm*q*(q-r)-(b-a)*(r-1.0d0))
               q = (q-1.0d0)*(r-1.0d0)*(s-1.0d0)
            end if
            if (p>0.0d0) q = -q
            p = abs(p)
            if (2.0d0*p<min(3.0d0*xm*q-abs(tol1*q),abs(e*q))) then
               e = d   ! Accept interpolation
               d = p/q
            else
               d = xm  ! Interpolation failed, use bisection
               e = d
            end if
         else
            d = xm     ! Bounds decreasing too slowly, use bisection
            e = d
         end if
         a = b; fa = fb
         if (abs(d)> tol1) then; b = b + d
         else;                   b = b+sign(tol1,xm)
         end if
         fb = self(z + b*direction) - iso
      end do
      call die_(tonto,"REALVEC:find_root_brent ... maximum iterations exceeded")

   end subroutine

   subroutine find_isopoint(self,z,direction,isovalue,tol,x1,x2)
    ! Given an initial point "z" and a "direction" to search along, return the
    ! isopoint "z" where the function "self" has value "isovalue", i.e. where
    ! self(p) = isovalue. "tol" is the accuracy to which "z" is determined.
    ! If present, "x1" and "x2" are an initial interval along "direction" from
    ! "p" where the isopoint is suspected to lie.
      real(kind=kind(1.0d0)), dimension(:) :: z,direction
      interface
         function self(z) result(res)
            real(kind=kind(1.0d0)), dimension(:) :: z
            real(kind=kind(1.0d0)) :: res
         end function
      end interface
      real(kind=kind(1.0d0)) :: isovalue,tol
      real(kind=kind(1.0d0)), optional :: x1,x2
      real(kind=kind(1.0d0)) :: y1,y2

      y1 = 0.0d0; if (present(x1)) y1 = x1
      y2 = 1.0d0;  if (present(x2)) y2 = x2
      call bracket_root_(self,z,direction,y1,y2,val=isovalue)
      call find_root_brent_(self,z,direction,y1,y2,tol,val=isovalue)

   end subroutine

! *********************
! Minimisation routines
! *********************

   subroutine minimise(self,new_direction,gradient,old_val,old_gradient,hessian)
    real(kind=kind(1.0d0)), dimension(:) :: self
    ! Move the vector "self" to a position closer to the minimum, based on the
    ! gradient.  Uses the BFGS scheme.  Doesn't check for convergence, just does
    ! one iteration.
     real(kind=kind(1.0d0)), dimension(:), intent(out) :: new_direction
     real(kind=kind(1.0d0)), dimension(:), intent(in) :: gradient
     real(kind=kind(1.0d0)), dimension(:), intent(inout) :: old_val, old_gradient
     real(kind=kind(1.0d0)), dimension(:,:), intent(inout) :: hessian
     real(kind=kind(1.0d0)), dimension(:), pointer :: dx,dg,hdg,u
     real(kind=kind(1.0d0)) :: dxdg,dghdg,dghdg_inv
     integer(kind=kind(1)) :: dim

     call ensure_(tonto,size(self)==size(new_direction),"REALVEC:minimise ... vector size mismatch")
     call ensure_(tonto,size(self)==size(gradient),"REALVEC:minimise ... vector size mismatch")
     call ensure_(tonto,size(self)==size(old_val),"REALVEC:minimise ... vector size mismatch")
     call ensure_(tonto,size(self)==size(old_gradient),"REALVEC:minimise ... vector size mismatch")
     call ensure_(tonto,size(self)==size(hessian,1),"REALVEC:minimise ... incorrect dimension for hessian matrix")
     call ensure_(tonto,size(self)==size(hessian,2),"REALVEC:minimise ... incorrect dimension for hessian matrix")
     dim = size(self)
     call create_(dx,dim); call create_(dg,dim); call create_(hdg,dim); call create_(u,dim)
     dg = gradient - old_gradient
     dx = self - old_val
     call to_product_of_(hdg,hessian,dg)
     dxdg = 1.0d0 / dot_product(dx,dg)
     dghdg = dot_product(dg,hdg)
     dghdg_inv = 1.0d0 / dghdg
     u  = dxdg * dx - dghdg_inv * hdg
     hessian = hessian + dxdg * outer_product_(dx,dx) - &
               dghdg_inv * outer_product_(hdg,hdg) + dghdg * outer_product_(u,u)
     call to_product_of_(dx,hessian,gradient)
     old_val = self
     old_gradient = gradient
     new_direction = - dx
     call destroy_(dx); call destroy_(dg); call destroy_(hdg); call destroy_(u)

   end subroutine

   subroutine minimise_BFGS(self,dself,p,fret,tol,gtol,step,max_it)
    ! Use the Broyden-Fletcher-Goldfarb-Shanno method to minimise a vector
    ! function self(p) with derivative function dself(p) starting from
    ! an initial point "p", returning the minimum point in "p", to a
    ! tolerance "gtol" in the gradient. If "step" is present it determines
    ! the size of the initial step in the line minimisation along the
    ! gradient direction. If "max_it" is present, set that to be the
    ! maximum iterations
      interface
         function self(p) result(res)
             real(kind=kind(1.0d0)), dimension(:) :: p
            real(kind=kind(1.0d0)) :: res
         end function
      end interface
      interface
         function dself(p) result(res)
             real(kind=kind(1.0d0)), dimension(:) :: p
            real(kind=kind(1.0d0)), dimension(size(p)) :: res
         end function
      end interface
      real(kind=kind(1.0d0)), dimension(:) :: p
      real(kind=kind(1.0d0)) :: fret,tol,gtol
      real(kind=kind(1.0d0)), optional :: step
      integer(kind=kind(1)), optional, intent(in) :: max_it
      integer(kind=kind(1)) :: n,iter,i,itmax
      real(kind=kind(1.0d0)) :: fac,fad,fae,fp,sumdg,sumxi,eps, stp,this_stp
      real(kind=kind(1.0d0)), dimension(:), pointer :: g,dg,hdg,pnew,xi
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: hessian
      logical(kind=kind(.true.)) :: fail

      eps = tol/4.0d0
      stp = 1.0d0
      if (present(step)) stp = step
      itmax = 200
      if (present(max_it)) itmax = max_it
      n = size(p)
      call create_(g,n)
      call create_(dg,n)
      call create_(hdg,n)
      call create_(pnew,n)
      call create_(xi,n)
      allocate(hessian(n,n))
      fp = self(p)
      g = dself(p)
      hessian = 0.0d0
      do i = 1,n
         hessian(i,i) = 1.0d0
      end do
      xi = -g
      iter = 0
      fail = .false.
      do
         iter = iter + 1
         pnew = p
         xi = (stp/norm_(xi)) * xi  ! This is new
         call line_minimise_from_(self,pnew,xi,fret,tol,fp,this_stp)
         stp = stp*sqrt(abs(this_stp))
         fp = fret
         xi = pnew - p
         p = pnew
         if (largest_value_(xi)<tol) exit
         dg = g
         g = dself(p)
         if (largest_value_(g)<gtol) exit
         dg = g - dg
         call to_product_of_(hdg,hessian,dg)
         fac = sum(dg*xi)
         fae = sum(dg*hdg)
         sumdg = sum(dg*dg)
         sumxi = sum(xi*xi)
         if (fac**2>eps*sumdg*sumxi) then
            fac = 1.0d0/fac
            fad = 1.0d0/fae
            dg = fac*xi - fad*hdg
            hessian = hessian + fac * outer_product_(xi,xi)   &
                              - fad * outer_product_(hdg,hdg) &
                              + fae * outer_product_(dg,dg)
            call minus_product_of_(xi,hessian,g)
         end if
         if (i > itmax) fail = .true.
      end do
      call die_if_(tonto,fail,"REALVEC:minimise_BFGS ... exceeded allowed iterations")
      deallocate(hessian)
      call destroy_(xi)
      call destroy_(pnew)
      call destroy_(hdg)
      call destroy_(dg)
      call destroy_(g)

   end subroutine

   subroutine minimise_FRPR(self,dself,p,fret,tol,ftol,algorithm,step)
    ! Use the Fletcher-Reeves-Polak-Ribiere method to minimise a vector
    ! function self(p) with derivative function dself(p) starting from
    ! an initial point "p", returning the minimum point in "p", to a
    ! tolerance "tol" and the minimum function value in "fret" to a
    ! tolerance "ftol". "algorithm" (if present) can be set to
    ! "Polak-Ribiere" (default) or "Fletcher-Reeves".
      interface
         function self(p) result(res)
            real(kind=kind(1.0d0)), dimension(:) :: p
            real(kind=kind(1.0d0)) :: res
         end function
      end interface
      interface
         function dself(p) result(res)
            real(kind=kind(1.0d0)), dimension(:) :: p
            real(kind=kind(1.0d0)), dimension(size(p)) :: res
         end function
      end interface
      real(kind=kind(1.0d0)), dimension(:) :: p
      real(kind=kind(1.0d0)) :: fret,tol,ftol
      real(kind=kind(1.0d0)), optional :: step
      character(128), optional :: algorithm
      integer(kind=kind(1)) :: itmax = 200
      integer(kind=kind(1)) :: n,iter
!      eps :: real(kind=kind(1.0d0)) = 10.0d0**(-10)
      real(kind=kind(1.0d0)) :: stp,this_stp,dgg,fp,gam,gg,dtol
      real(kind=kind(1.0d0)), dimension(:), pointer :: g,h,xi
      character(128) :: alg
      logical(kind=kind(.true.)) :: fail

      alg = "Polak-Ribiere"
      if (present(algorithm)) alg = algorithm
      stp = 1.0d0
      if (present(step)) stp = step
      n = size(p)
      call create_(g,n)
      call create_(h,n)
      call create_(xi,n)
      fp = self(p)
      fret = fp
      xi = dself(p)
      g = -xi
      h = g
      xi = h
      iter = 0
      fail = .false.
      do
         iter = iter + 1
         dtol = largest_value_(xi)
         if (dtol<tol) exit
         xi = (stp/norm_(xi)) * xi  ! This is new
         call line_minimise_from_(self,p,xi,fret,tol,fp,this_stp)
         stp = stp*sqrt(abs(this_stp))
        ! if (2.0d0*abs(fret-fp)<=ftol*(abs(fret)+abs(fp)+eps)) exit
         if (abs(fret-fp)<=ftol .and. dtol<tol) exit
         fail = iter>=itmax
         if (fail) exit
        ! fp = self(p)
         fp = fret
         xi = dself(p)
         gg = 0.0d0
         dgg = 0.0d0
         gg = sum(g*g)
         select case (alg)
            case("Polak-Ribiere  "); dgg = sum((xi+g)*xi)
            case("Fletcher-Reeves"); dgg = sum(xi*xi)
            case default;        allocate(tonto%known_keywords(2))
            tonto%known_keywords(1) = "Polak-Ribiere  "
            tonto%known_keywords(2) = "Fletcher-Reeves"
            call unknown_(tonto,alg,"REALVEC:minimise_FRPR")
            deallocate(tonto%known_keywords)
         end select
         if (gg==0.0d0) exit
         gam = dgg/gg
         g = -xi
         h = g + gam*h
         xi = h
      end do
      call die_if_(tonto,fail,"REALVEC:minimise_FRPR ... exceeded allowed iterations")
      call destroy_(xi)
      call destroy_(h)
      call destroy_(g)

   end subroutine

   subroutine minimise_powell(self,p,directions,fret,tol,ftol)
    ! Use Powell's method to minimise a vector function self(p) starting from
    ! an initial point "p" along the initial (columns of) "directions", returning
    ! the minimum point in "p", the minimum function value in "fret" to a
    ! tolerance "ftol", and "tol" in the vector coordinates "p".
      interface
         function self(p) result(res)
             real(kind=kind(1.0d0)), dimension(:) :: p
            real(kind=kind(1.0d0)) :: res
         end function
      end interface
      real(kind=kind(1.0d0)), dimension(:) :: p
      real(kind=kind(1.0d0)), dimension(:,:) :: directions
      real(kind=kind(1.0d0)) :: fret,tol,ftol
      integer(kind=kind(1)) :: itmax = 200
      integer(kind=kind(1)) :: n,iter,i,ibig
      real(kind=kind(1.0d0)) :: del,fp,fptt,t
      real(kind=kind(1.0d0)), dimension(:), pointer :: pt,ptt,xit
      logical(kind=kind(.true.)) :: fail

      call ensure_(tonto,size(p)==size(directions,1),"REALVEC:minimise_powell ... incompatible initial data")
      call ensure_(tonto,size(p)==size(directions,2),"REALVEC:minimise_powell ... incompatible initial data")
      n = size(p)
      call create_(pt,n)
      call create_(ptt,n)
      call create_(xit,n)
      fret = self(p)
      pt = p                                     ! Save initial point
      iter = 0
      fail = .false.
      do                                         ! Iteration loop
         iter = iter + 1
         fp = fret
         ibig = 0
         del = 0
         do i = 1,n                              ! Loop over all directions in set
            xit = directions(:,i)                ! Get direction
            fptt = fret
            call line_minimise_from_(self,p,xit,fret,tol)  ! Minimize along direction
            if (abs(fptt-fret)>del) then         ! Save if largest decrease so far
               del = abs(fptt-fret)
               ibig = i
            end if
         end do
         if (2.0d0*abs(fp-fret)<=ftol*(abs(fp)+abs(fret))) exit
         fail = iter>=itmax
         if (fail) exit
         ptt = 2.0d0*p - pt                        ! Construct extrapolated point and the
         xit = p - pt                            ! average direction moved; save old
         pt = p                                  ! starting point
         fptt = self(ptt)
         if (fptt>=fp) cycle
         t = 2.0d0*(fp-2.0d0*fret+fptt)*(fp-fret-del)**2-del*(fp-fptt)**2
         if (t>=0) cycle
         call line_minimise_from_(self,p,xit,fret,tol)     ! Move to minimum of the new direction
         directions(:,ibig) = directions(:,n)
         directions(:,n) = xit
      end do
      call die_if_(tonto,fail,"REALVEC:minimise_powell ... exceeded allowed iterations")
      call destroy_(xit)
      call destroy_(ptt)
      call destroy_(pt)

   end subroutine

   subroutine line_minimise_from(self,p,direction,fret,tol,f0,del)
    ! Given a vector function self(x), minimise from point "p" along "direction".
    ! Return the minimum point in "p" and the minimum value "fret", with an
    ! accuracy "tol". If "f0" is present it is used as the value of the function
    ! "self" at p, f0 = self(p). If "del" is present it is set to the length
    ! along "direction" where p achieved its minimum, useful for monitoring the
    ! step size.
      interface
         function self(p) result(res)
            real(kind=kind(1.0d0)), dimension(:) :: p
            real(kind=kind(1.0d0)) :: res
         end function
      end interface
      real(kind=kind(1.0d0)), dimension(:) :: p,direction
      real(kind=kind(1.0d0)) :: fret,tol
      real(kind=kind(1.0d0)), optional :: f0,del
      real(kind=kind(1.0d0)) :: a,x,b,fa,fx,fb,xmin

      a = 0.0d0
      x = 1.0d0
      call bracket_minimum_(self,p,direction,a,x,b,fa,fx,fb,f0)
      call minimise_brent_(self,p,direction,a,x,b,xmin,fret,tol,fx)
      if (present(del)) del = xmin

   end subroutine

   subroutine bracket_minimum(self,p,direction,a,b,c,fa,fb,fc,fa0,fb0)
    ! Given a vector function self(p), an initial point "p" and a "direction",
    ! and initial distances "a" and "b" along "direction" from "p", search in
    ! the downhill direction and return new distances "a", "b" and "c" along
    ! "direction" from "p" that bracket a minimum of the function self, and
    ! return the values of the function "fa", "fb", and "fc" at these points.
    ! NOTE: "c" is not used initially.
    ! If present, "fa0" is the initial value of self at a, fa0 = self(a).
    ! If present, "fb0" is the initial value of self at b, fb0 = self(b).
      interface
         function self(p) result(res)
             real(kind=kind(1.0d0)), dimension(:) :: p
            real(kind=kind(1.0d0)) :: res
         end function
      end interface
      real(kind=kind(1.0d0)), dimension(:) :: p,direction
      real(kind=kind(1.0d0)) :: a,b,c,fa,fb,fc
      real(kind=kind(1.0d0)), optional :: fa0,fb0
      real(kind=kind(1.0d0)) :: gold = 1.618034
      real(kind=kind(1.0d0)) :: glimit = 100
      real(kind=kind(1.0d0)) :: tiny = 1.0d-20
      real(kind=kind(1.0d0)) :: fu,q,r,u,ulim
      integer(kind=kind(1)) :: iter
      logical(kind=kind(.true.)) :: fail

      call ensure_(tonto,size(p)==size(direction),"REALVEC:bracket_minimum ... incompatible vectors")
      if (present(fa0)) then; fa = fa0
      else;                   fa = self(p + a*direction)
      end if
      if (present(fb0)) then; fb = fb0
      else;                   fb = self(p + b*direction)
      end if
      if (fb>fa) then
        call swap_with_(a,b)
        call swap_with_(fa,fb)
      end if
      c  = b + gold*(b-a)
      fc = self(p + c*direction)
      fail = .true.
      do iter = 1, 100
         if (fb<fc) then
           fail = .false.
           exit                            ! bracket found
         end if
         r = (b-a)*(fb-fc)                 ! get u by parabolic extrapolation
         q = (b-c)*(fb-fa)
         u = b - ((b-c)*q-(b-a)*r)/(2.0d0*sign(max(abs(q-r),tiny),q-r))
         ulim = b + glimit*(c-b)
         if ((b-u)*(u-c)>0.0d0) then        ! Parabolic u lies between b and c
            fu = self(p + u*direction)
            if (fu<fc) then                ! got a minimum between b and c
               a = b; fa = fb
               b = u; fb = fu
               fail = .false.
               exit
            else if (fu>fb) then           ! got a minimum between a and u
               c = u; fc = fu
               fail = .false.
               exit
            end if
            u = c + gold*(c-b)             ! parabolic fit no use, so magnify
            fu = self(p + u*direction)
         else if ((c-u)*(u-ulim)>0) then  ! Fit is between c and its allowed limit
            fu = self(p + u*direction)
            if (fu<fc) then
               b = c; fb = fc
               c = u; fc = fu
               u = c + gold*(c-b)
               fu = self(p + u*direction)
            end if
         else if ((u-ulim)*(ulim-c)>0) then
            u = ulim
            fu = self(p + u*direction)
         else
            u = c + gold*(c-b)             ! magnify
            fu = self(p + u*direction)
         end if
         a = b; fa = fb
         b = c; fb = fc
         c = u; fc = fu
      end do
      call warn_if_(tonto,fail,"REALVEC:bracket_minimum ... exceeded maximum iterations")
      if (a>c) then
         call swap_with_(a,c)
         call swap_with_(fa,fc)
      end if

   end subroutine

   subroutine minimise_golden(self,p,direction,a,b,c,xmin,f,tol)
    ! Given a vector function self(p), an initial point "p" and a "direction",
    ! and initial distances "a", "b" and "c" along "direction" from "p" which
    ! bracket a minimum in function self, return the minimum point "p" and its
    ! distance "xmin" along the "direction" from "p", and also the value "f" at
    ! the minimum to a precision "tol", using the golden section search method.
      interface
         function self(p) result(res)
             real(kind=kind(1.0d0)), dimension(:) :: p
            real(kind=kind(1.0d0)) :: res
         end function
      end interface
      real(kind=kind(1.0d0)), dimension(:) :: p,direction
      real(kind=kind(1.0d0)) :: a,b,c,xmin,f,tol
       real(kind=kind(1.0d0)) :: r = 0.618033399
      real(kind=kind(1.0d0)) :: s,f1,f2,x0,x1,x2,x3

      call ensure_(tonto,size(p)==size(direction),"REALVEC:minimise_golden ... incompatible vectors")
      s = 1.0d0 - r
      x0 = a
      x3 = c
      if (abs(c-b)>abs(b-a)) then
         x1 = b; x2 = b + s*(c-b)
      else
         x2 = b; x1 = b - s*(b-a)
      end if
      f1 = self(p + x1*direction)
      f2 = self(p + x2*direction)
      do
         if (abs(x3-x0)<=tol*(abs(x1)+abs(x2))) exit
         if (f2<f1) then
            x0 = x1
            x1 = x2
            x2 = r*x1 + s*x3
            f1 = f2
            f2 = self(p + x2*direction)
         else
            x3 = x2
            x2 = x1
            x1 = r*x2 + s*x0
            f2 = f1
            f1 = self(p + x1*direction)
         end if
      end do
      if (f1<f2) then; f = f1; xmin = x1; p = p + x1*direction
      else;            f = f2; xmin = x2; p = p + x2*direction
      end if

   end subroutine

   subroutine minimise_brent(self,p,direction,a,b,c,xmin,f,tol,fb_in)
    ! Given a vector function self(x), an initial point "p", a "direction", and
    ! initial distances "a", "b" and "c" along "direction" from "p" which
    ! bracket a minimum in function self, return the minimum point "p" and its
    ! distance "xmin" along the "direction" from "p", and also the value "f" at
    ! the minimum to a precision "tol", using Brent's search method.
      interface
         function self(p) result(res)
             real(kind=kind(1.0d0)), dimension(:) :: p
            real(kind=kind(1.0d0)) :: res
         end function
      end interface
      real(kind=kind(1.0d0)), dimension(:) :: p,direction
      real(kind=kind(1.0d0)) :: a,b,c,xmin,f,tol
      real(kind=kind(1.0d0)), intent(in), optional :: fb_in
      integer(kind=kind(1)) :: itmax = 100
      real(kind=kind(1.0d0)) :: cgold = 0.3819660d0
      real(kind=kind(1.0d0)) :: zeps = 10.0d0**(-10)
      real(kind=kind(1.0d0)) :: d,e,etemp,fu,fv,fw,fx,pp,qq,r,tol1,tol2,u,v,w,x,xm
      integer(kind=kind(1)) :: iter,iters
      logical(kind=kind(.true.)) :: fail

      call ensure_(tonto,size(p)==size(direction),"REALVEC:minimise_brent ... incompatible vectors")
      if (a>c) call swap_with_(a,c)
      v = b
      w = b
      x = b
      if (present(fb_in)) then
        fx = fb_in
      else
        fx = self(p + x*direction)
      end if
      fv = fx
      fw = fx
      e = 0.0d0
      fail = .true.
      b = c
      iters=0
      do iter = 1,itmax
         xm = 0.50d0*(a+b)
         tol1 = tol*abs(x) + zeps
         tol2 = 2.0d0*tol1
         if (abs(x-xm)<(tol2-0.50d0*(b-a))) then
            fail = .false.
            exit
         end if
         if (abs(e)>tol1) then
            r = (x-w)*(fx-fv)
            qq = (x-v)*(fx-fw)
            pp = (x-v)*qq - (x-w)*r
            qq = 2.0d0*(qq-r)
            if (qq>0.0d0) pp = -pp
            qq = abs(qq)
            etemp = e
            e = d
            if (abs(pp)>=abs(0.50d0*qq*etemp) .or. pp<=qq*(a-x) .or. pp>=qq*(b-x)) then
              if (x>=xm) then; e = a-x
              else;            e = b-x
              end if
              d = cgold*e
            else
              d = pp/qq
              u = x + d
              if ((u-a)<tol2 .or. (b-u)<tol2) d = sign(tol1,xm-x)
            end if
         else
           if (x>=xm) then; e = a-x
           else;            e = b-x
           end if
           d = cgold*e
         end if
         if (abs(d)>=tol1) then; u = x + d
         else;                   u = x + sign(tol1,d)
         end if
         fu = self(p + u*direction)
         if (fu<=fx) then
            if (u>=x) then; a = x
            else;           b = x
            end if
            v = w; fv = fw
            w = x; fw = fx
            x = u; fx = fu
         else
            if (u<x) then; a = u
            else;          b = u
            end if
            if (fu<=fw .or. w==x) then
               v = w; fv = fw
               w = u; fw = fu
            else if (fu<=fv .or. v==x .or. v==w) then
               v = u; fv = fu
            end if
         end if
         iters=iters+1
      end do
      call die_if_(tonto,fail,"REALVEC:minimise_brent ... maximum iterations exceeded")
      f = fx
      xmin = x
      p = p + x*direction

   end subroutine

end
