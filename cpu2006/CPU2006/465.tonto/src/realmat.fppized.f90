!-------------------------------------------------------------------------------
!
! REALMAT: Matrix operations ...
!
! Copyright (C) Dylan Jayatilaka, 1996
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
! $Id: realmat.foo,v 1.58.2.9 2003/11/13 05:36:07 reaper Exp $
!-------------------------------------------------------------------------------

module REALMAT_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

   use REALVEC_MODULE, only: same_as_
   use REALVEC_MODULE, only: norm_
   use REALVEC_MODULE, only: create_
   use REALVEC_MODULE, only: to_product_of_
   use REALVEC_MODULE, only: zero_small_values_
   use REALVEC_MODULE, only: destroy_
   use REALVEC_MODULE, only: reverse_order_

   use INTVEC_MODULE, only: create_
   use INTVEC_MODULE, only: destroy_

   use STR_MODULE, only: is_known_unit_
   use STR_MODULE, only: conversion_factor_

   use INT_MODULE, only: is_even_
   use INT_MODULE, only: is_odd_
   use INT_MODULE, only: to_str_

   use BINMAT_MODULE, only: create_
   use BINMAT_MODULE, only: destroy_

   use REAL_MODULE, only: is_zero_
   use REAL_MODULE, only: to_str_

   use CPXVEC_MODULE, only: create_
   use CPXVEC_MODULE, only: to_product_of_
   use CPXVEC_MODULE, only: destroy_
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

   public    compress_to_triangle_
   interface compress_to_triangle_
      module procedure compress_to_triangle
   end interface

   public    beta_beta_
   interface beta_beta_
      module procedure beta_beta
   end interface

   public    to_inverse_of_
   interface to_inverse_of_
      module procedure to_inverse_of
   end interface

   public    set_to_
   interface set_to_
      module procedure set_to
   end interface

   public    to_power_series_inverse_of_
   interface to_power_series_inverse_of_
      module procedure to_power_series_inverse_of
   end interface

   public    solve_linear_equations_
   interface solve_linear_equations_
      module procedure solve_linear_equations
   end interface

   public    same_as_
   interface same_as_
      module procedure same_as
   end interface

   public    rotate_
   interface rotate_
      module procedure rotate
   end interface

   public    convert_to_
   interface convert_to_
      module procedure convert_to
   end interface

   public    alpha_alpha_
   interface alpha_alpha_
      module procedure alpha_alpha
   end interface

   public    is_zero_
   interface is_zero_
      module procedure is_zero
   end interface

   public    zero_small_values_
   interface zero_small_values_
      module procedure zero_small_values
   end interface

   public    solve_symmetric_eigenproblem_
   interface solve_symmetric_eigenproblem_
      module procedure solve_symmetric_eigenproblem
   end interface

   public    plus_
   interface plus_
      module procedure plus
   end interface

   public    swap_columns_
   interface swap_columns_
      module procedure swap_columns
      module procedure swap_columns_1
   end interface

   public    sum_column_vectors_
   interface sum_column_vectors_
      module procedure sum_column_vectors
   end interface

   public    gaussian_g_xyz_matrix_
   interface gaussian_g_xyz_matrix_
      module procedure gaussian_g_xyz_matrix
   end interface

   public    alpha_beta_set_to_
   interface alpha_beta_set_to_
      module procedure alpha_beta_set_to
   end interface

   public    has_unit_diagonal_
   interface has_unit_diagonal_
      module procedure has_unit_diagonal
   end interface

   private    solve_linear_equations_ESSL_
   interface solve_linear_equations_ESSL_
      module procedure solve_linear_equations_ESSL
   end interface

   public    is_diagonal_
   interface is_diagonal_
      module procedure is_diagonal
   end interface

   private    solve_linear_equation_ESSL_
   interface solve_linear_equation_ESSL_
      module procedure solve_linear_equation_ESSL
   end interface

   public    to_inverse_sqrt_
   interface to_inverse_sqrt_
      module procedure to_inverse_sqrt
   end interface

   public    to_product_with_diagonal_
   interface to_product_with_diagonal_
      module procedure to_product_with_diagonal
      module procedure to_product_with_diagonal_1
   end interface

   public    shrink_columns_
   interface shrink_columns_
      module procedure shrink_columns
   end interface

   public    alpha_alpha_set_to_
   interface alpha_alpha_set_to_
      module procedure alpha_alpha_set_to
   end interface

   public    in_range_
   interface in_range_
      module procedure in_range
   end interface

   public    to_power_series_inv_sqrt_of_
   interface to_power_series_inv_sqrt_of_
      module procedure to_power_series_inv_sqrt_of
   end interface

   public    beta_alpha_put_to_
   interface beta_alpha_put_to_
      module procedure beta_alpha_put_to
   end interface

   private    solve_symm_eigenproblem_ESSL_
   interface solve_symm_eigenproblem_ESSL_
      module procedure solve_symm_eigenproblem_ESSL
   end interface

   public    to_scaled_product_of_
   interface to_scaled_product_of_
      module procedure to_scaled_product_of
   end interface

   public    determinant_
   interface determinant_
      module procedure determinant
   end interface

   public    beta_beta_set_to_
   interface beta_beta_set_to_
      module procedure beta_beta_set_to
   end interface

   public    to_inverse_of_ESSL_
   interface to_inverse_of_ESSL_
      module procedure to_inverse_of_ESSL
   end interface

   public    add_to_diagonal_
   interface add_to_diagonal_
      module procedure add_to_diagonal
   end interface

   public    all_in_range_
   interface all_in_range_
      module procedure all_in_range
   end interface

   public    symmetrically_orthonormalise_
   interface symmetrically_orthonormalise_
      module procedure symmetrically_orthonormalise
   end interface

   public    append_column_
   interface append_column_
      module procedure append_column
   end interface

   public    equals_
   interface equals_
      module procedure equals
   end interface

   public    to_transpose_
   interface to_transpose_
      module procedure to_transpose
      module procedure to_transpose_1
   end interface

   public    from_diagonal_
   interface from_diagonal_
      module procedure from_diagonal
   end interface

   public    trace_
   interface trace_
      module procedure trace
   end interface

   public    max_abs_column_difference_
   interface max_abs_column_difference_
      module procedure max_abs_column_difference
   end interface

   public    range_
   interface range_
      module procedure range
   end interface

   public    get_diagonal_
   interface get_diagonal_
      module procedure get_diagonal
   end interface

   public    gaussian_d_xyz_matrix_
   interface gaussian_d_xyz_matrix_
      module procedure gaussian_d_xyz_matrix
   end interface

   public    destroy_
   interface destroy_
      module procedure destroy
   end interface

   private    solve_linear_equations_LAPACK_
   interface solve_linear_equations_LAPACK_
      module procedure solve_linear_equations_LAPACK
   end interface

   public    to_exponential_of_
   interface to_exponential_of_
      module procedure to_exponential_of
   end interface

   public    plus_scaled_
   interface plus_scaled_
      module procedure plus_scaled
   end interface

   public    alpha_beta_put_to_
   interface alpha_beta_put_to_
      module procedure alpha_beta_put_to
   end interface

   public    mean_column_vector_
   interface mean_column_vector_
      module procedure mean_column_vector
   end interface

   public    expand_columns_
   interface expand_columns_
      module procedure expand_columns
   end interface

   public    reverse_schmidt_orthonormalise_
   interface reverse_schmidt_orthonormalise_
      module procedure reverse_schmidt_orthonormalise
   end interface

   public    alpha_alpha_put_to_
   interface alpha_alpha_put_to_
      module procedure alpha_alpha_put_to
   end interface

   public    is_unit_matrix_
   interface is_unit_matrix_
      module procedure is_unit_matrix
   end interface

   public    is_same_shape_as_
   interface is_same_shape_as_
      module procedure is_same_shape_as
   end interface

   public    cofactor_
   interface cofactor_
      module procedure cofactor
   end interface

   public    antisymmetric_exponential_
   interface antisymmetric_exponential_
      module procedure antisymmetric_exponential
   end interface

   public    set_diagonal_
   interface set_diagonal_
      module procedure set_diagonal
   end interface

   public    index_of_minimum_column_norm_
   interface index_of_minimum_column_norm_
      module procedure index_of_minimum_column_norm
   end interface

   public    to_unit_matrix_
   interface to_unit_matrix_
      module procedure to_unit_matrix
   end interface

   public    antisymmetric_fold_
   interface antisymmetric_fold_
      module procedure antisymmetric_fold
   end interface

   public    append_columns_
   interface append_columns_
      module procedure append_columns
   end interface

   public    solve_eigenproblem_
   interface solve_eigenproblem_
      module procedure solve_eigenproblem
   end interface

   public    beta_beta_put_to_
   interface beta_beta_put_to_
      module procedure beta_beta_put_to
   end interface

   public    symmetrize_
   interface symmetrize_
      module procedure symmetrize
   end interface

   public    plus_scaled_product_of_
   interface plus_scaled_product_of_
      module procedure plus_scaled_product_of
   end interface

   public    gaussian_f_xyz_matrix_
   interface gaussian_f_xyz_matrix_
      module procedure gaussian_f_xyz_matrix
   end interface

   public    is_square_
   interface is_square_
      module procedure is_square
   end interface

   public    plus_product_of_
   interface plus_product_of_
      module procedure plus_product_of
   end interface

   public    tri_size_
   interface tri_size_
      module procedure tri_size
   end interface

   private    to_inverse_of_LAPACK_
   interface to_inverse_of_LAPACK_
      module procedure to_inverse_of_LAPACK
   end interface

   public    is_symmetric_
   interface is_symmetric_
      module procedure is_symmetric
   end interface

   public    change_basis_
   interface change_basis_
      module procedure change_basis
      module procedure change_basis_1
      module procedure change_basis_2
      module procedure change_basis_3
      module procedure change_basis_4
   end interface

   public    exponentiate_to_
   interface exponentiate_to_
      module procedure exponentiate_to
   end interface

   public    weight_diagonal_
   interface weight_diagonal_
      module procedure weight_diagonal
   end interface

   public    to_product_of_
   interface to_product_of_
      module procedure to_product_of
      module procedure to_product_of_1
   end interface

   public    alpha_beta_
   interface alpha_beta_
      module procedure alpha_beta
   end interface

   public    sum_row_vectors_
   interface sum_row_vectors_
      module procedure sum_row_vectors
   end interface

   public    reverse_schmidt_orthogonalise_
   interface reverse_schmidt_orthogonalise_
      module procedure reverse_schmidt_orthogonalise
   end interface

   public    antisymmetric_reflect_
   interface antisymmetric_reflect_
      module procedure antisymmetric_reflect
   end interface

   public    convert_from_
   interface convert_from_
      module procedure convert_from
   end interface

   public    beta_alpha_
   interface beta_alpha_
      module procedure beta_alpha
   end interface

   public    shrink_
   interface shrink_
      module procedure shrink
   end interface

   public    zero_off_diagonal_
   interface zero_off_diagonal_
      module procedure zero_off_diagonal
   end interface

   public    max_abs_diagonal_element_
   interface max_abs_diagonal_element_
      module procedure max_abs_diagonal_element
   end interface

   public    max_diagonal_element_
   interface max_diagonal_element_
      module procedure max_diagonal_element
   end interface

   public    symmetric_reflect_
   interface symmetric_reflect_
      module procedure symmetric_reflect
   end interface

   public    column_index_
   interface column_index_
      module procedure column_index
   end interface

   public    is_inversion_matrix_
   interface is_inversion_matrix_
      module procedure is_inversion_matrix
   end interface

   public    minus_
   interface minus_
      module procedure minus
   end interface

   public    reverse_column_order_
   interface reverse_column_order_
      module procedure reverse_column_order
   end interface

   public    has_column_
   interface has_column_
      module procedure has_column
   end interface

   public    get_column_dot_products_
   interface get_column_dot_products_
      module procedure get_column_dot_products
   end interface

   public    get_column_norms_
   interface get_column_norms_
      module procedure get_column_norms
   end interface

   public    create_
   interface create_
      module procedure create
      module procedure create_1
      module procedure create_2
      module procedure create_3
   end interface

   public    trace_product_with_
   interface trace_product_with_
      module procedure trace_product_with
      module procedure trace_product_with_1
      module procedure trace_product_with_2
   end interface

   public    create_copy_
   interface create_copy_
      module procedure create_copy
   end interface

   public    make_corresponding_orbitals_
   interface make_corresponding_orbitals_
      module procedure make_corresponding_orbitals
   end interface

   public    antisymmetrize_
   interface antisymmetrize_
      module procedure antisymmetrize
   end interface

   public    zero_diagonal_
   interface zero_diagonal_
      module procedure zero_diagonal
   end interface

   public    dot_
   interface dot_
      module procedure dot
      module procedure dot_1
   end interface

   public    solve_linear_equation_
   interface solve_linear_equation_
      module procedure solve_linear_equation
   end interface

   public    symmetric_fold_to_tri_
   interface symmetric_fold_to_tri_
      module procedure symmetric_fold_to_tri
   end interface

   public    to_unit_mat_
   interface to_unit_mat_
      module procedure to_unit_mat
   end interface

   private    solve_linear_equation_LAPACK_
   interface solve_linear_equation_LAPACK_
      module procedure solve_linear_equation_LAPACK
   end interface

   public    symmetric_fold_
   interface symmetric_fold_
      module procedure symmetric_fold
   end interface

   private    solve_symm_eigenproblem_LAPACK_
   interface solve_symm_eigenproblem_LAPACK_
      module procedure solve_symm_eigenproblem_LAPACK
   end interface

   public    expand_
   interface expand_
      module procedure expand
   end interface

   public    column_norms_
   interface column_norms_
      module procedure column_norms
   end interface

   public    to_sqrt_
   interface to_sqrt_
      module procedure to_sqrt
   end interface

   public    schmidt_orthonormalise_
   interface schmidt_orthonormalise_
      module procedure schmidt_orthonormalise
      module procedure schmidt_orthonormalise_1
      module procedure schmidt_orthonormalise_2
   end interface

   public    is_transposed_shape_of_
   interface is_transposed_shape_of_
      module procedure is_transposed_shape_of
   end interface

   public    plus_scaled_mat_
   interface plus_scaled_mat_
      module procedure plus_scaled_mat
   end interface

   public    make_enclosing_sphere_
   interface make_enclosing_sphere_
      module procedure make_enclosing_sphere
   end interface

   public    minus_scaled_
   interface minus_scaled_
      module procedure minus_scaled
   end interface

   public    to_scaled_mat_
   interface to_scaled_mat_
      module procedure to_scaled_mat
   end interface

   public    make_diagonally_dominant_
   interface make_diagonally_dominant_
      module procedure make_diagonally_dominant
   end interface

   public    uncompress_from_triangle_
   interface uncompress_from_triangle_
      module procedure uncompress_from_triangle
   end interface

   public    similarity_transform_
   interface similarity_transform_
      module procedure similarity_transform
   end interface

   public    beta_alpha_set_to_
   interface beta_alpha_set_to_
      module procedure beta_alpha_set_to
   end interface

   public    back_transform_
   interface back_transform_
      module procedure back_transform
      module procedure back_transform_1
      module procedure back_transform_2
   end interface

   public trace_of_product_; interface trace_of_product_
     module procedure trace_product_with
   end interface

contains

   subroutine create(self,dim1,dim2)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Create a matrix with the given dimensions
      pointer :: self
      integer(kind=kind(1)), intent(in) :: dim1,dim2
    ! The following code is inherited from INTRINSICMAT

      nullify(self)
      allocate(self(dim1,dim2))

   end subroutine

   subroutine create_1(self,lb1,ub1,lb2,ub2)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Create a matrix with the given dimensions
      pointer :: self
      integer(kind=kind(1)), intent(in) :: lb1,ub1,lb2,ub2
    ! The following code is inherited from INTRINSICMAT

      nullify(self)
      allocate(self(lb1:ub1,lb2:ub2))

   end subroutine

   subroutine create_2(self,bounds1,bounds2)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Create a matrix with the specified bounds for each dimension
      pointer :: self
      integer(kind=kind(1)), dimension(:), intent(in) :: bounds1,bounds2
    ! The following code is inherited from INTRINSICMAT

      call create_(self,bounds1(1),bounds1(2),bounds2(1),bounds2(2))

   end subroutine

   subroutine create_3(self,bounds)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Create a matrix with the given bounds for all dimensions
      pointer :: self
      integer(kind=kind(1)), dimension(2,2), intent(in) :: bounds
    ! The following code is inherited from INTRINSICMAT

      call create_(self,bounds(1,1),bounds(1,2),bounds(2,1),bounds(2,2))

   end subroutine

   subroutine create_copy(self,matrix)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Create a replica copy of matrix
      pointer :: self
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: matrix
    ! The following code is inherited from INTRINSICMAT

      call create_(self,lbound(matrix,1),ubound(matrix,1), &
              lbound(matrix,2),ubound(matrix,2)  )
      self = matrix

   end subroutine

   subroutine destroy(self)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Destroy the object
      pointer :: self
    ! The following code is inherited from INTRINSICMAT

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

   pure function is_square(self) result(res)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Returns .true. if the matrix is square
      intent(in) :: self
      logical(kind=kind(.true.)) :: res
    ! The following code is inherited from INTRINSICMAT
      res = size(self,1)==size(self,2)

   end function

   pure function is_same_shape_as(self,b) result(res)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Returns .true. if the matrix "b" has the same shape as self
      intent(in) :: self
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: b
      logical(kind=kind(.true.)) :: res
    ! The following code is inherited from INTRINSICMAT
      res = size(self,1)==size(b,1) .and. size(self,2)==size(b,2)

   end function

   pure function is_transposed_shape_of(self,b) result(res)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Returns .true. if the matrix "b" is the transposed shape of self
      intent(in) :: self
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: b
      logical(kind=kind(.true.)) :: res
    ! The following code is inherited from INTRINSICMAT
      res = size(self,1)==size(b,2) .and. size(self,2)==size(b,1)

   end function

   function all_in_range(self,range) result(res)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Return .true. if all values of self are within the specified "range".
      real(kind=kind(1.0d0)), dimension(2) :: range
      logical(kind=kind(.true.)) :: res

      res = all(range(1) <= self .and. self <= range(2))

   end function

   function in_range(self,range) result(res)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Return element ij as .true. if self(i,j) is within the specified "range".
      real(kind=kind(1.0d0)), dimension(2) :: range
      logical(kind=kind(.true.)), dimension(size(self,1),size(self,2)) :: res

      res = (range(1) <= self .and. self <= range(2))

   end function

   function range(self) result(res)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Return the range (smallest and largest value) of self.
      real(kind=kind(1.0d0)), dimension(2) :: res

      res(1) = minval(self)
      res(2) = maxval(self)

   end function

   subroutine shrink(self,dim1,dim2)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Shrinks self to dimension dim1xdim2.  Contents are retained.
     pointer :: self
     integer(kind=kind(1)), intent(in) :: dim1,dim2
    ! The following code is inherited from INTRINSICMAT
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: old

     call ensure_(tonto,associated(self),"REALMAT:shrink ... matrix not allocated")
     call ensure_(tonto,dim1<=size(self,1),"REALMAT:shrink ... 1st dimension given is too large.")
     call ensure_(tonto,dim2<=size(self,2),"REALMAT:shrink ... 2nd dimension given is too large.")
     if (dim1==size(self,1) .and. dim2==size(self,2)) then;   return; end if
     old => self
     nullify(self)
     call create_(self,dim1,dim2)
     self=old(1:dim1,1:dim2)
     call destroy_(old)

   end subroutine

   subroutine shrink_columns(self,dim2)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Shrinks columns of self to dimension dim2. Contents are retained.
     pointer :: self
     integer(kind=kind(1)), intent(in) :: dim2
    ! The following code is inherited from INTRINSICMAT
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: old
     integer(kind=kind(1)) :: dim1

     call ensure_(tonto,associated(self),"REALMAT:shrink_columns ... matrix not allocated")
     call ensure_(tonto,dim2<=size(self,2),"REALMAT:shrink_columns ... 2nd dimension given is too large.")
     if (dim2==size(self,2)) then;   return; end if
     dim1 = size(self,1)
     old => self
     nullify(self)
     call create_(self,dim1,dim2)
     self=old(1:dim1,1:dim2)
     call destroy_(old)

   end subroutine

   subroutine expand(self,dim1,dim2)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Expands self to dimension dim1xdim2.  Contents are retained.
     pointer :: self
     integer(kind=kind(1)), intent(in) :: dim1,dim2
    ! The following code is inherited from INTRINSICMAT
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: old
     integer(kind=kind(1)) :: old_size1,old_size2

     if (.not. associated(self)) then
       call create_(self,0,0)
     else
     call ensure_(tonto,dim1>=size(self,1),"REALMAT:expand ... 1st dimension given is too small")
     call ensure_(tonto,dim2>=size(self,2),"REALMAT:expand ... 2nd dimension given is too small")
     end if
     old => self
     old_size1 = size(old,1)
     old_size2 = size(old,2)
     nullify(self)
     call create_(self,dim1,dim2)
     self(1:old_size1,1:old_size2)=old
     call destroy_(old)

   end subroutine

   subroutine expand_columns(self,dim2)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Expands the columns self to dim2.  Contents are retained.
     pointer :: self
     integer(kind=kind(1)), intent(in) :: dim2
    ! The following code is inherited from INTRINSICMAT
     integer(kind=kind(1)) :: dim1,old_dim2
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: old

     call ensure_(tonto,associated(self),"REALMAT:expand_columns ... matrix not allocated")
     call ensure_(tonto,dim2>=size(self,2),"REALMAT:expand_columns ... 2nd dimension given is too small")
     dim1 = size(self,1)
     old => self
     old_dim2 = size(old,2)
     nullify(self)
     call create_(self,dim1,dim2)
     self(:,1:old_dim2) = old
     call destroy_(old)

   end subroutine

   subroutine append_column(self,col)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Append the column "col" onto the end of self.
     pointer :: self
     real(kind=kind(1.0d0)), dimension(:) :: col
     integer(kind=kind(1)) :: old_dim2,new_dim2

   call ensure_(tonto,associated(self),"REALMAT:append_column ... self not allocated")
   call ensure_(tonto,size(self,1)==size(col),"REALMAT:append_column ... 2nd dimension given is too small")
     old_dim2 = size(self,2)
     new_dim2 = size(self,2) + 1
     call expand_columns_(self,new_dim2)
     self(:,new_dim2) = col

   end subroutine

   subroutine append_columns(self,cols)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Append the columns "cols" onto the end of self.
     pointer :: self
     real(kind=kind(1.0d0)), dimension(:,:) :: cols
    ! The following code is inherited from INTRINSICMAT
     integer(kind=kind(1)) :: old_dim2,new_dim2

     call ensure_(tonto,associated(self),"REALMAT:append_columns ... self not allocated")
     call ensure_(tonto,size(self,1)==size(cols,1),"REALMAT:append_columns ... 1st dimension wrong, cols")
     old_dim2 = size(self,2)
     new_dim2 = size(self,2) + size(cols,2)
     call expand_columns_(self,new_dim2)
     self(:,old_dim2+1:new_dim2) = cols

   end subroutine

   function determinant(self) result(res)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Return the determinant a 3x3 matrix
      intent(in) :: self
      real(kind=kind(1.0d0)) :: res

   call ensure_(tonto,is_square_(self),"REALMAT:determinant ... non-square matrix")
   call ensure_(tonto,size(self,1)<4,"REALMAT:determinant ... only works for up to size 3 matrices")
      select case (size(self,1))
          case (1)
             res = self(1,1)
          case (2)
             res = self(1,1)*self(2,2) - self(2,1)*self(1,2)
          case (3)
             res = self(1,3)*(self(2,1)*self(3,2) - self(3,1)*self(2,2)) &
                 + self(2,3)*(self(3,1)*self(1,2) - self(1,1)*self(3,2)) &
                 + self(3,3)*(self(1,1)*self(2,2) - self(2,1)*self(1,2))
      end select

   end function

   function cofactor(self) result(res)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Return the cofactor fo a 3x3 matrix
      intent(in) :: self
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: res

      call ensure_(tonto,is_square_(self),"REALMAT:cofactor ... non-square matrix")
      call ensure_(tonto,size(self,1)==3,"REALMAT:cofactor ... only works for 3x3 matrices")
      nullify(res)
      call create_(res,3,3)
      res(1,1) =  (self(2,2)*self(3,3) - self(2,3)*self(3,2))
      res(1,2) = -(self(2,1)*self(3,3) - self(2,3)*self(3,1))
      res(1,3) =  (self(2,1)*self(3,2) - self(2,2)*self(3,1))
      res(2,1) = -(self(1,2)*self(3,3) - self(1,3)*self(3,2))
      res(2,2) =  (self(1,1)*self(3,3) - self(1,3)*self(3,1))
      res(2,3) = -(self(1,1)*self(3,2) - self(1,2)*self(3,1))
      res(3,1) =  (self(1,2)*self(2,3) - self(1,3)*self(2,2))
      res(3,2) = -(self(1,1)*self(2,3) - self(1,3)*self(2,1))
      res(3,3) =  (self(1,1)*self(2,2) - self(1,2)*self(2,1))

   end function

   function dot(self,l,r) result(res)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Multiply the matrix self by vector "l" on the left and vector "r" on the
    ! right ie:  res = l^T self r. Useful for non-unit metric dot_products.
     intent(in) :: self
     real(kind=kind(1.0d0)), dimension(:), intent(in) :: l,r
     real(kind=kind(1.0d0)) :: res
     real(kind=kind(1.0d0)), dimension(:), pointer :: w

     call ensure_(tonto,size(self,1)==size(l),"REALMAT:dot ... wrong size, r")
     call ensure_(tonto,size(self,2)==size(r),"REALMAT:dot ... wrong size, r")
     call create_(w,size(l))
     call to_product_of_(w,self,r)
     res = dot_product(l,w)
     call destroy_(w)

   end function

   function dot_1(self,l,r) result(res)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Multiply the matrix self by vector "l" on the left and vector "r" on the
    ! right ie:  res = l^T self r. Useful for non-unit metric dot_products.
     intent(in) :: self
     complex(kind=kind((1.0d0,1.0d0))), dimension(:), intent(in) :: l,r
     complex(kind=kind((1.0d0,1.0d0))) :: res
     complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: w

     call ensure_(tonto,size(self,1)==size(l),"REALMAT:dot_1 ... wrong size, r")
     call ensure_(tonto,size(self,2)==size(r),"REALMAT:dot_1 ... wrong size, r")
     call create_(w,size(l))
     call to_product_of_(w,self,r)
     res = dot_product(l,w)
     call destroy_(w)

   end function

   subroutine rotate(self,v)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Rotate vector "v" by self
     real(kind=kind(1.0d0)), dimension(:), intent(inout) :: v
     integer(kind=kind(1)) :: dim1,dim2,i,j
     real(kind=kind(1.0d0)), dimension(:), pointer :: w
     real(kind=kind(1.0d0)) :: val

     call ensure_(tonto,is_square_(self),"REALMAT:rotate ... incompatible arrays sizes")
     call ensure_(tonto,size(self,2)==size(v),"REALMAT:rotate ... incompatible arrays sizes")
     dim1 = size(self,1)
     dim2 = size(self,2)
     call create_(w,dim2)
     do i = 1,dim1
       val = 0.0d0
       do j = 1,dim2
         val = val + self(i,j) * v(j)
       end do
       w(i) = val
     end do
     v = w
     call destroy_(w)

   end subroutine

   subroutine to_product_of(self,a,b,transpose_a,transpose_b)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Set "self" to the matrix product of "a" and "b". If present,
    ! "transpose_a" and "transpose_b" can be set to .true. if "a" and "b"
    ! neeb to be transposed.
     intent(inout) :: self
     real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: a, b
     logical(kind=kind(.true.)), optional, intent(in) :: transpose_a, transpose_b
     logical(kind=kind(.true.)) :: trans_a,trans_b
     integer(kind=kind(1)) :: dim1,dim2,i,j,opt

     trans_a = .false.;       trans_b = .false.
     if (present(transpose_a)) trans_a = transpose_a
     if (present(transpose_b)) trans_b = transpose_b
     opt = 0
     if (trans_a) opt = opt + 1
     if (trans_b) opt = opt + 2
     select case (opt)
       case (0)  ! .to_product_an_bn
         call ensure_(tonto,size(self,1)==size(a,1),"REALMAT:to_product_of ... incompatible arrays")
         call ensure_(tonto,size(self,2)==size(b,2),"REALMAT:to_product_of ... incompatible arrays")
         call ensure_(tonto,size(a,2)==size(b,1),"REALMAT:to_product_of ... incompatible arrays")
         dim1 = size(self,1)
         dim2 = size(self,2)
         do i=1,dim1
           do j=1,dim2
             self(i,j) = sum(a(i,:)*b(:,j))
           end do
         end do
       case (1)  ! .to_product_at_bn
         call ensure_(tonto,size(self,1)==size(a,2),"REALMAT:to_product_of ... incompatible arrays")
         call ensure_(tonto,size(self,2)==size(b,2),"REALMAT:to_product_of ... incompatible arrays")
         call ensure_(tonto,size(a,1)==size(b,1),"REALMAT:to_product_of ... incompatible arrays")
         dim1 = size(self,1)
         dim2 = size(self,2)
         do i=1,dim1
           do j=1,dim2
             self(i,j) = sum(a(:,i)*b(:,j))
           end do
         end do
       case (2)  ! .to_product_an_bt
         call ensure_(tonto,size(self,1)==size(a,1),"REALMAT:to_product_of ... incompatible arrays")
         call ensure_(tonto,size(self,2)==size(b,1),"REALMAT:to_product_of ... incompatible arrays")
         call ensure_(tonto,size(a,2)==size(b,2),"REALMAT:to_product_of ... incompatible arrays")
         dim1 = size(self,1)
         dim2 = size(self,2)
         do i=1,dim1
           do j=1,dim2
             self(i,j) = sum(a(i,:)*b(j,:))
           end do
         end do
       case (3)  ! .to_product_at_bt
         call ensure_(tonto,size(self,1)==size(a,2),"REALMAT:to_product_of ... incompatible arrays")
         call ensure_(tonto,size(self,2)==size(b,1),"REALMAT:to_product_of ... incompatible arrays")
         call ensure_(tonto,size(a,1)==size(b,2),"REALMAT:to_product_of ... incompatible arrays")
         dim1 = size(self,1)
         dim2 = size(self,2)
         do i=1,dim1
           do j=1,dim2
             self(i,j) = sum(a(:,i)*b(j,:))
           end do
         end do
     end select

   end subroutine

   subroutine to_product_of_1(self,a,b,dagger_a,dagger_b)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Set "self" to the complex matrix product of "a" and "b". If present,
    ! "dagger_a" and "dagger_b" can be set to .true. if "a" and "b" need
    ! to be daggered. WARNING: the complex part is thrown away.
     intent(inout) :: self
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), intent(in) :: a, b
     logical(kind=kind(.true.)), optional, intent(in) :: dagger_a, dagger_b
     logical(kind=kind(.true.)) :: dagg_a,dagg_b
     integer(kind=kind(1)) :: opt
     integer(kind=kind(1)) :: dima,dim1,dim2,i,j,k
     real(kind=kind(1.0d0)) :: temp

     dagg_a = .false.;        dagg_b = .false.
     if (present(dagger_a)) dagg_a = dagger_a
     if (present(dagger_b)) dagg_b = dagger_b
     opt = 0
     if (dagg_a) opt = opt + 1
     if (dagg_b) opt = opt + 2
     select case (opt)
       case (0)  ! .to_product_an_bn
         call ensure_(tonto,size(self,1)==size(a,1),"REALMAT:to_product_of_1 ... incompatible arrays")
         call ensure_(tonto,size(self,2)==size(b,2),"REALMAT:to_product_of_1 ... incompatible arrays")
         call ensure_(tonto,size(a,2)==size(b,1),"REALMAT:to_product_of_1 ... incompatible arrays")
         dima = size(a,2)
         dim1 = size(self,1)
         dim2 = size(self,2)
         do i=1,dim1
         do j=1,dim2
           temp = 0.0d0
           do k=1,dima
             temp = temp + a(i,k) * b(k,j)
           end do
           self(i,j) = temp
         end do
         end do
       case (1)  ! .to_product_ad_bn
         call ensure_(tonto,size(self,1)==size(a,2),"REALMAT:to_product_of_1 ... incompatible arrays")
         call ensure_(tonto,size(self,2)==size(b,2),"REALMAT:to_product_of_1 ... incompatible arrays")
         call ensure_(tonto,size(a,1)==size(b,1),"REALMAT:to_product_of_1 ... incompatible arrays")
         dima = size(a,1)
         dim1 = size(self,1)
         dim2 = size(self,2)
         do i=1,dim1
         do j=1,dim2
           temp = 0.0d0
           do k=1,dima
             temp = temp + conjg(a(k,i)) * b(k,j)
           end do
           self(i,j) = temp
         end do
         end do
       case (2)  ! .to_product_an_bd
         call ensure_(tonto,size(self,1)==size(a,1),"REALMAT:to_product_of_1 ... incompatible arrays")
         call ensure_(tonto,size(self,2)==size(b,1),"REALMAT:to_product_of_1 ... incompatible arrays")
         call ensure_(tonto,size(a,2)==size(b,2),"REALMAT:to_product_of_1 ... incompatible arrays")
         dima = size(a,2)
         dim1 = size(self,1)
         dim2 = size(self,2)
         do i=1,dim1
         do j=1,dim2
           temp = 0.0d0
           do k=1,dima
             temp = temp + a(i,k) * conjg(b(j,k))
           end do
           self(i,j) = temp
         end do
         end do
       case (3)  ! .to_product_ad_bd
         call ensure_(tonto,size(self,1)==size(a,2),"REALMAT:to_product_of_1 ... incompatible arrays")
         call ensure_(tonto,size(self,2)==size(b,1),"REALMAT:to_product_of_1 ... incompatible arrays")
         call ensure_(tonto,size(a,1)==size(b,2),"REALMAT:to_product_of_1 ... incompatible arrays")
         dima = size(a,1)
         dim1 = size(self,1)
         dim2 = size(self,2)
         do i=1,dim1
         do j=1,dim2
           temp = 0.0d0
           do k=1,dima
             temp = temp + a(k,i) * b(j,k)
           end do
           self(i,j) = temp  ! conjugate not reqd
         end do
         end do
     end select

   end subroutine

   subroutine to_scaled_product_of(self,fac,a,b,transpose_a,transpose_b)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Set "self" to the matrix product of "a" and "b" scaled by "fac".
    ! If present, "transpose_a" and "transpose_b" can be set to .true. if "a"
    ! and "b" neeb to be transposed.
     intent(inout) :: self
     real(kind=kind(1.0d0)), intent(in) :: fac
     real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: a, b
     logical(kind=kind(.true.)), optional, intent(in) :: transpose_a, transpose_b
     logical(kind=kind(.true.)) :: trans_a,trans_b
     integer(kind=kind(1)) :: opt
     integer(kind=kind(1)) :: dima,dim1,dim2,i,j,k
     real(kind=kind(1.0d0)) :: temp

     trans_a = .false.;          trans_b = .false.
     if (present(transpose_a)) trans_a = transpose_a
     if (present(transpose_b)) trans_b = transpose_b
     opt = 0
     if (trans_a) opt = opt + 1
     if (trans_b) opt = opt + 2
     select case (opt)
       case (0)  ! .to_scaled_product_an_bn
         call ensure_(tonto,size(self,1)==size(a,1),"REALMAT:to_scaled_product_of ... incompatible arrays")
         call ensure_(tonto,size(self,2)==size(b,2),"REALMAT:to_scaled_product_of ... incompatible arrays")
         call ensure_(tonto,size(a,2)==size(b,1),"REALMAT:to_scaled_product_of ... incompatible arrays")
         dima = size(a,2)
         dim1 = size(self,1)
         dim2 = size(self,2)
         do i=1,dim1
         do j=1,dim2
           temp = 0.0d0
           do k=1,dima
             temp = temp + a(i,k) * b(k,j)
           end do
           self(i,j) = fac * temp
         end do
         end do
       case (1)  ! .to_scaled_product_at_bn
         call ensure_(tonto,size(self,1)==size(a,2),"REALMAT:to_scaled_product_of ... incompatible arrays")
         call ensure_(tonto,size(self,2)==size(b,2),"REALMAT:to_scaled_product_of ... incompatible arrays")
         call ensure_(tonto,size(a,1)==size(b,1),"REALMAT:to_scaled_product_of ... incompatible arrays")
         dima = size(a,1)
         dim1 = size(self,1)
         dim2 = size(self,2)
         do i=1,dim1
         do j=1,dim2
           temp = 0.0d0
           do k=1,dima
             temp = temp + a(k,i) * b(k,j)
           end do
           self(i,j) = fac * temp
         end do
         end do
       case (2)  ! .to_scaled_product_an_bt
         call ensure_(tonto,size(self,1)==size(a,1),"REALMAT:to_scaled_product_of ... incompatible arrays")
         call ensure_(tonto,size(self,2)==size(b,1),"REALMAT:to_scaled_product_of ... incompatible arrays")
         call ensure_(tonto,size(a,2)==size(b,2),"REALMAT:to_scaled_product_of ... incompatible arrays")
         dima = size(a,2)
         dim1 = size(self,1)
         dim2 = size(self,2)
         do i=1,dim1
         do j=1,dim2
           temp = 0.0d0
           do k=1,dima
             temp = temp + a(i,k) * b(j,k)
           end do
           self(i,j) = fac * temp
         end do
         end do
       case (3)  ! .to_scaled_product_at_bt
         call ensure_(tonto,size(self,1)==size(a,2),"REALMAT:to_scaled_product_of ... incompatible arrays")
         call ensure_(tonto,size(self,2)==size(b,1),"REALMAT:to_scaled_product_of ... incompatible arrays")
         call ensure_(tonto,size(a,1)==size(b,2),"REALMAT:to_scaled_product_of ... incompatible arrays")
         dima = size(a,1)
         dim1 = size(self,1)
         dim2 = size(self,2)
         do i=1,dim1
         do j=1,dim2
           temp = 0.0d0
           do k=1,dima
             temp = temp + a(k,i) * b(j,k)
           end do
           self(i,j) = fac * temp
         end do
         end do
     end select

   end subroutine

   subroutine plus_product_of(self,a,b,transpose_a,transpose_b)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Add to  "self" the matrix product of "a" and "b". If present,
    ! "transpose_a" and "transpose_b" can be set to .true. if "a" and "b"
    ! neeb to be transposed.
     intent(inout) :: self
     real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: a, b
     logical(kind=kind(.true.)), optional, intent(in) :: transpose_a, transpose_b
     logical(kind=kind(.true.)) :: trans_a,trans_b
     integer(kind=kind(1)) :: opt
     integer(kind=kind(1)) :: dima,dim1,dim2,i,j,k
     real(kind=kind(1.0d0)) :: temp

     trans_a = .false.;       trans_b = .false.
     if (present(transpose_a)) trans_a = transpose_a
     if (present(transpose_b)) trans_b = transpose_b
     opt = 0
     if (trans_a) opt = opt + 1
     if (trans_b) opt = opt + 2
     select case (opt)
       case (0)  ! .plus_product_an_bn
         call ensure_(tonto,size(self,1)==size(a,1),"REALMAT:plus_product_of ... incompatible arrays")
         call ensure_(tonto,size(self,2)==size(b,2),"REALMAT:plus_product_of ... incompatible arrays")
         call ensure_(tonto,size(a,2)==size(b,1),"REALMAT:plus_product_of ... incompatible arrays")
         dima = size(a,2)
         dim1 = size(self,1)
         dim2 = size(self,2)
         do i=1,dim1
         do j=1,dim2
           temp = 0.0d0
           do k=1,dima
             temp = temp + a(i,k) * b(k,j)
           end do
           self(i,j) = self(i,j) + temp
         end do
         end do
       case (1)  ! .plus_product_at_bn
         call ensure_(tonto,size(self,1)==size(a,2),"REALMAT:plus_product_of ... incompatible arrays")
         call ensure_(tonto,size(self,2)==size(b,2),"REALMAT:plus_product_of ... incompatible arrays")
         call ensure_(tonto,size(a,1)==size(b,1),"REALMAT:plus_product_of ... incompatible arrays")
         dima = size(a,1)
         dim1 = size(self,1)
         dim2 = size(self,2)
         do i=1,dim1
         do j=1,dim2
           temp = 0.0d0
           do k=1,dima
             temp = temp + a(k,i) * b(k,j)
           end do
           self(i,j) = self(i,j) + temp
         end do
         end do
       case (2)  ! .plus_product_an_bt
         call ensure_(tonto,size(self,1)==size(a,1),"REALMAT:plus_product_of ... incompatible arrays")
         call ensure_(tonto,size(self,2)==size(b,1),"REALMAT:plus_product_of ... incompatible arrays")
         call ensure_(tonto,size(a,2)==size(b,2),"REALMAT:plus_product_of ... incompatible arrays")
         dima = size(a,2)
         dim1 = size(self,1)
         dim2 = size(self,2)
         do i=1,dim1
         do j=1,dim2
           temp = 0.0d0
           do k=1,dima
             temp = temp + a(i,k) * b(j,k)
           end do
           self(i,j) = self(i,j) + temp
         end do
         end do
       case (3)  ! .plus_product_at_bt
         call ensure_(tonto,size(self,1)==size(a,2),"REALMAT:plus_product_of ... incompatible arrays")
         call ensure_(tonto,size(self,2)==size(b,1),"REALMAT:plus_product_of ... incompatible arrays")
         call ensure_(tonto,size(a,1)==size(b,2),"REALMAT:plus_product_of ... incompatible arrays")
         dima = size(a,1)
         dim1 = size(self,1)
         dim2 = size(self,2)
         do i=1,dim1
         do j=1,dim2
           temp = 0.0d0
           do k=1,dima
             temp = temp + a(k,i) * b(j,k)
           end do
           self(i,j) = self(i,j) + temp
         end do
         end do
     end select

   end subroutine

   subroutine plus_scaled_product_of(self,fac,a,b,transpose_a,transpose_b)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Add to "self" the matrix product of "a" and "b" scaled by "fac".
    ! If present, "transpose_a" and "transpose_b" can be set to .true. if "a"
    ! and "b" need to be transposed.
     intent(inout) :: self
     real(kind=kind(1.0d0)), intent(in) :: fac
     real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: a, b
     logical(kind=kind(.true.)), optional, intent(in) :: transpose_a, transpose_b
     logical(kind=kind(.true.)) :: trans_a,trans_b
     integer(kind=kind(1)) :: opt
     integer(kind=kind(1)) :: dima,dim1,dim2,i,j,k
     real(kind=kind(1.0d0)) :: temp

     trans_a = .false.;       trans_b = .false.
     if (present(transpose_a)) trans_a = transpose_a
     if (present(transpose_b)) trans_b = transpose_b
     opt = 0
     if (trans_a) opt = opt + 1
     if (trans_b) opt = opt + 2
     select case (opt)
       case (0)  ! .plus_scaled_product_an_bn
         call ensure_(tonto,size(self,1)==size(a,1),"REALMAT:plus_scaled_product_of ... incompatible arrays")
         call ensure_(tonto,size(self,2)==size(b,2),"REALMAT:plus_scaled_product_of ... incompatible arrays")
         call ensure_(tonto,size(a,2)==size(b,1),"REALMAT:plus_scaled_product_of ... incompatible arrays")
         dima = size(a,2)
         dim1 = size(self,1)
         dim2 = size(self,2)
         do i=1,dim1
         do j=1,dim2
           temp = 0.0d0
           do k=1,dima
             temp = temp + a(i,k) * b(k,j)
           end do
           self(i,j) = self(i,j) + fac * temp
         end do
         end do
       case (1)  ! .plus_scaled_product_at_bn
         call ensure_(tonto,size(self,1)==size(a,2),"REALMAT:plus_scaled_product_of ... incompatible arrays")
         call ensure_(tonto,size(self,2)==size(b,2),"REALMAT:plus_scaled_product_of ... incompatible arrays")
         call ensure_(tonto,size(a,1)==size(b,1),"REALMAT:plus_scaled_product_of ... incompatible arrays")
         dima = size(a,1)
         dim1 = size(self,1)
         dim2 = size(self,2)
         do i=1,dim1
         do j=1,dim2
           temp = 0.0d0
           do k=1,dima
             temp = temp + a(k,i) * b(k,j)
           end do
           self(i,j) = self(i,j) + fac * temp
         end do
         end do
       case (2)  ! .plus_scaled_product_an_bt
         call ensure_(tonto,size(self,1)==size(a,1),"REALMAT:plus_scaled_product_of ... incompatible arrays")
         call ensure_(tonto,size(self,2)==size(b,1),"REALMAT:plus_scaled_product_of ... incompatible arrays")
         call ensure_(tonto,size(a,2)==size(b,2),"REALMAT:plus_scaled_product_of ... incompatible arrays")
         dima = size(a,2)
         dim1 = size(self,1)
         dim2 = size(self,2)
         do i=1,dim1
         do j=1,dim2
           temp = 0.0d0
           do k=1,dima
             temp = temp + a(i,k) * b(j,k)
           end do
           self(i,j) = self(i,j) + fac * temp
         end do
         end do
       case (3)  ! .plus_scaled_product_at_bt
         call ensure_(tonto,size(self,1)==size(a,2),"REALMAT:plus_scaled_product_of ... incompatible arrays")
         call ensure_(tonto,size(self,2)==size(b,1),"REALMAT:plus_scaled_product_of ... incompatible arrays")
         call ensure_(tonto,size(a,1)==size(b,2),"REALMAT:plus_scaled_product_of ... incompatible arrays")
         dima = size(a,1)
         dim1 = size(self,1)
         dim2 = size(self,2)
         do i=1,dim1
         do j=1,dim2
           temp = 0.0d0
           do k=1,dima
             temp = temp + a(k,i) * b(j,k)
           end do
           self(i,j) = self(i,j) + fac * temp
         end do
         end do
     end select

   end subroutine

   subroutine plus_scaled(self,mat,fac)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Add the matrix "mat" scaled by "fac" to "self".
     intent(inout) :: self
     real(kind=kind(1.0d0)), intent(in) :: fac
     real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: mat
     integer(kind=kind(1)) :: dim1,dim2,i,j

     call ensure_(tonto,size(self,1)==size(mat,1),"REALMAT:plus_scaled ... incompatible arrays")
     call ensure_(tonto,size(self,2)==size(mat,2),"REALMAT:plus_scaled ... incompatible arrays")
     dim1 = size(self,1)
     dim2 = size(self,2)
     do j=1,dim2
       do i=1,dim1
         self(i,j) = self(i,j) + fac * mat(i,j)
       end do
     end do

   end subroutine

   subroutine minus_scaled(self,mat,fac)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Subtract the matrix "mat" scaled by "fac" from "self".
     intent(inout) :: self
     real(kind=kind(1.0d0)), intent(in) :: fac
     real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: mat
     integer(kind=kind(1)) :: dim1,dim2,i,j

     call ensure_(tonto,size(self,1)==size(mat,1),"REALMAT:minus_scaled ... incompatible arrays")
     call ensure_(tonto,size(self,2)==size(mat,2),"REALMAT:minus_scaled ... incompatible arrays")
     dim1 = size(self,1)
     dim2 = size(self,2)
     do j=1,dim2
       do i=1,dim1
         self(i,j) = self(i,j) - fac * mat(i,j)
       end do
     end do

   end subroutine

   subroutine to_product_with_diagonal(self,a,diag,transpose_a)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Set "self" to the matrix product of "a" with diagonal matrix "diag" (stored
    ! as a vector).  If present, "transpose_a" can be set to .true. if "a" needs to
    ! be transposed.
      intent(inout) :: self
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: a
      real(kind=kind(1.0d0)), dimension(:), intent(in) :: diag
      logical(kind=kind(.true.)), optional, intent(in) :: transpose_a
      integer(kind=kind(1)) :: a1,a2,s1,s2,d1,i,j
      real(kind=kind(1.0d0)) :: temp

   call ensure_(tonto,is_same_shape_as_(self,a),"REALMAT:to_product_with_diagonal ... incompatible dimensions")
      s1 = size(self,1); s2 = size(self,2)
      a1 = size(a,1);    a2 = size(a,2)
      d1 = size(diag)
      if (present(transpose_a)) then
         call ensure_(tonto,a1==d1,"REALMAT:to_product_with_diagonal ... incompatible dimensions")
         do j=1,s2
           temp = diag(j)
           do i=1,s1
             self(i,j) = a(j,i)*temp
           end do
         end do
      else
         call ensure_(tonto,a2==d1,"REALMAT:to_product_with_diagonal ... incompatible dimensions")
         do j=1,s2
           temp = diag(j)
           do i=1,s1
             self(i,j) = a(i,j)*temp
           end do
         end do
      end if

   end subroutine

   subroutine to_product_with_diagonal_1(self,dg,a,transpose_a)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Set "self" to the matrix product of diagonal matrix "dg" (stored as a
    ! vector) and "a".  If present, "transpose_a" can be set to .true. if "a" needs
    ! to be transposed.
      intent(inout) :: self
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: a
      real(kind=kind(1.0d0)), dimension(:), intent(in) :: dg
      logical(kind=kind(.true.)), optional, intent(in) :: transpose_a
      integer(kind=kind(1)) :: a1,a2,s1,s2,d1,i,j
      real(kind=kind(1.0d0)) :: temp

      call ensure_(tonto,is_same_shape_as_(self,a),"REALMAT:to_product_with_diagonal_1 ... incompatible dimensions")
      s1 = size(self,1); s2 = size(self,2)
      a1 = size(a,1);    a2 = size(a,2)
      d1 = size(dg)
      if (present(transpose_a)) then
         call ensure_(tonto,a2==d1,"REALMAT:to_product_with_diagonal_1 ... incompatible dimensions")
         do i=1,s1
           temp = dg(i)
           do j=1,s2
             self(i,j) = temp*a(j,i)
           end do
         end do
      else
         call ensure_(tonto,a1==d1,"REALMAT:to_product_with_diagonal_1 ... incompatible dimensions")
         do i=1,s1
           temp = dg(i)
           do j=1,s2
             self(i,j) = temp*a(i,j)
           end do
         end do
      end if

   end subroutine

! *********************
! Eigenproblem routines
! *********************

   subroutine solve_eigenproblem(self,eigenvalues,eigenvectors)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Solve the symmetric eigenproblem for "self", yeilding a vector of
    ! "eigenvalues" and a matrix of "eigenvectors"
      real(kind=kind(1.0d0)), dimension(:) :: eigenvalues
      real(kind=kind(1.0d0)), dimension(:,:) :: eigenvectors

      call solve_symmetric_eigenproblem_(self,eigenvalues,eigenvectors)

   end subroutine

!   solve_general_eigenproblem(eigenvalues,left,right,normalize)
!   ! Solve the eigenproblem for "self", yeilding a vector of "eigenvalues" and
!   ! a matrix of "left" and "right" eigenvectors. If "normalize" is present
!   ! and .false., the left and right eigenvectors are not automatically
!   ! renormalized so that (left)^T (right) = 1
!      eigenvalues :: CPXVEC
!      left,right :: CPXMAT
!      normalize :: logical(kind=kind(.true.)), optional
!      er,ei,W :: REALVEC*
!      A,le,re :: SELF_TYPE*
!      i,dim,dimW, info :: integer(kind=kind(1))
!      normalise :: logical(kind=kind(.true.))
!      dot :: real(kind=kind(1.0d0))
!      call ensure_(tonto,.is_square,"non-square matrix")
!      call ensure_(tonto,size(eigenvalues)>=.dim1,"eigenvalue array too small")
!      call ensure_(tonto,size(left)>=size(self),"left eigenvector matrix too small")
!      call ensure_(tonto,size(right)>=size(self),"right eigenvector matrix too small")
!      dim = size(self,1)
!      normalise = .true.
!      if (present(normalize)) normalise = normalize
!      if (self.is_symmetric) then
!         A.create(dim,dim)
!         er.create(dim)
!         .solve_symmetric_eigenproblem(er,A)
!         eigenvalues = er
!         right = A
!         left  = A
!         er.destroy
!         A.destroy
!      else
!         A.create(dim,dim)
!         er.create(dim); ei.create(dim)
!         le.create(dim,dim); re.create(dim,dim)
!         dimW = 8*dim
!         W.create(dimW)
!         A = self
!         ! Solve the eigenvalueproblem
!         call dgeev('V','V',dim,A,dim,er,ei,le,dim,re,dim,W,dimW,info)
!         call ensure_(tonto,info==0,"error, info="// trim(info.to_str))
!         ! Search for the complex eigenvalues/vectors
!         i = 1
!         do
!            if (.not. ei(i).is_zero(10.0d0**(-20))) then
!               eigenvalues(i)   = cmplx(er(i)  ,ei(i),  kind=kind((1.0d0,1.0d0)))
!               eigenvalues(i+1) = cmplx(er(i+1),ei(i+1),kind=kind((1.0d0,1.0d0)))
!               left(:,i)    = cmplx(le(:,i), le(:,i+1),kind=kind((1.0d0,1.0d0)))
!               left(:,i+1)  = cmplx(le(:,i),-le(:,i+1),kind=kind((1.0d0,1.0d0)))
!               right(:,i)   = cmplx(re(:,i), re(:,i+1),kind=kind((1.0d0,1.0d0)))
!               right(:,i+1) = cmplx(re(:,i),-re(:,i+1),kind=kind((1.0d0,1.0d0)))
!               i = i + 2
!            else
!               eigenvalues(i)   = cmplx(er(i)  , 0.0d0,  kind=kind((1.0d0,1.0d0)))
!               left(:,i)    = cmplx(le(:,i),0.0d0,kind=kind((1.0d0,1.0d0)))
!               right(:,i)   = cmplx(re(:,i),0.0d0,kind=kind((1.0d0,1.0d0)))
!               i = i + 1
!            end
!            if (i>dim) exit
!         end
!         W.destroy
!         re.destroy; le.destroy
!         ei.destroy; er.destroy
!         A.destroy
!      end
!      if (normalise) then
!         do i = 1,dim
!            dot = dot_product(left(:,i),right(:,i))
!            dot = 1.0d0/sqrt(dot)
!            left(:,i)  = dot*left(:,i)
!            right(:,i) = dot*right(:,i)
!         end
!      end
!   end

   subroutine solve_symmetric_eigenproblem(self,eigenvalues,eigenvectors)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Solve the symmetric eigenproblem for "self", yeilding a vector of
    ! "eigenvalues" and a matrix of "eigenvectors"
      real(kind=kind(1.0d0)), dimension(:) :: eigenvalues
      real(kind=kind(1.0d0)), dimension(:,:) :: eigenvectors

      call solve_symm_eigenproblem_LAPACK_(self,eigenvalues,eigenvectors)
   end subroutine

   subroutine solve_symm_eigenproblem_ESSL(self,eigenvalues,eigenvectors)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Solve the symmetric eigenproblem for "self", yeilding a vector of
    ! "eigenvalues" and a matrix of "eigenvectors". ESSL version.
      real(kind=kind(1.0d0)), dimension(:) :: eigenvalues
      real(kind=kind(1.0d0)), dimension(:,:) :: eigenvectors
      real(kind=kind(1.0d0)), dimension(:), pointer :: ap,W
      integer(kind=kind(1)) :: dim

      call ensure_(tonto,is_square_(self),"REALMAT:solve_symm_eigenproblem_ESSL ... non-square matrix")
      call ensure_(tonto,size(eigenvalues)>=size(self,1),"REALMAT:solve_symm_eigenproblem_ESSL ... eigenvalue array too small&
&")
      call ensure_(tonto,size(eigenvectors)>=size(self),"REALMAT:solve_symm_eigenproblem_ESSL ... eigenvector matrix too smal&
&l")
      dim = size(self,1)
      call create_(ap,dim*(dim+1)/2)
      call compress_to_triangle_(self,ap)
      call create_(W,2*dim)
      call destroy_(W)
      call destroy_(ap)

   end subroutine

   subroutine solve_symm_eigenproblem_LAPACK(self,eigenvalues,eigenvectors)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Solve the symmetric eigenproblem for "self", yeilding a vector of
    ! "eigenvalues" and a matrix of "eigenvectors". LAPACK version.
      real(kind=kind(1.0d0)), dimension(:) :: eigenvalues
      real(kind=kind(1.0d0)), dimension(:,:) :: eigenvectors
      real(kind=kind(1.0d0)), dimension(:), pointer :: W
      integer(kind=kind(1)) :: dim,fail,lwork

      call ensure_(tonto,is_square_(self),"REALMAT:solve_symm_eigenproblem_LAPACK ... non-square matrix")
      call ensure_(tonto,size(eigenvalues)>=size(self,1),"REALMAT:solve_symm_eigenproblem_LAPACK ... eigenvalue array too sma&
&ll")
      call ensure_(tonto,size(eigenvectors)>=size(self),"REALMAT:solve_symm_eigenproblem_LAPACK ... eigenvector matrix too sm&
&all")
      dim = size(self,1)
      lwork = max(dim*dim,3*dim-1)
      call create_(W,lwork)
      eigenvectors = self
      fail = 0
      call dsyev("V","L",dim,eigenvectors,dim,eigenvalues,W,lwork,fail)
   call ensure_(tonto,fail==0,"REALMAT:solve_symm_eigenproblem_LAPACK ... no solution, error found")
      call destroy_(W)

   end subroutine

   subroutine solve_linear_equation(self,rhs,solution)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Solve the linear equations posed by "self", with "rhs" as the RHS vector,
    ! yeilding vector "solution" as the answer
      real(kind=kind(1.0d0)), dimension(:) :: rhs, solution

      call solve_linear_equation_LAPACK_(self,rhs,solution)
   end subroutine

   subroutine solve_linear_equation_ESSL(self,rhs,solution)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Solve the linear equations posed by "self", with "rhs" as the RHS vector,
    ! yeilding vector "solution" as the answer. ESSL version
      real(kind=kind(1.0d0)), dimension(:) :: rhs, solution
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: LU
      integer(kind=kind(1)), dimension(:), pointer :: pivot
      integer(kind=kind(1)) :: dim

      call ensure_(tonto,is_square_(self),"REALMAT:solve_linear_equation_ESSL ... non-square matrix")
      call ensure_(tonto,size(rhs)==size(self,1),"REALMAT:solve_linear_equation_ESSL ... incompatible rhs")
      dim = size(rhs)
      call create_(LU,dim,dim)
      call create_(pivot,dim)
      LU = self
      solution = rhs
      call destroy_(pivot)
      call destroy_(LU)

   end subroutine

   subroutine solve_linear_equation_LAPACK(self,rhs,solution)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Solve the linear equations posed by "self", with "rhs" as the RHS vector,
    ! yeilding vector "solution" as the answer. LAPACK version.
      real(kind=kind(1.0d0)), dimension(:) :: rhs, solution
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: LU
      integer(kind=kind(1)), dimension(:), pointer :: pivot
      integer(kind=kind(1)) :: dim,nrhs,err

      call ensure_(tonto,is_square_(self),"REALMAT:solve_linear_equation_LAPACK ... non-square matrix")
      call ensure_(tonto,size(rhs)==size(self,1),"REALMAT:solve_linear_equation_LAPACK ... incompatible rhs")
      dim = size(rhs)
      nrhs = 1
      nullify(LU); call create_(LU,dim,dim)
      nullify(pivot); call create_(pivot,dim)
      LU = self
      solution = rhs
      call dgesv(dim,nrhs,LU,dim,pivot,solution,dim,err)
      call ensure_(tonto,err==0,"REALMAT:solve_linear_equation_LAPACK ... no solution, error found")
      call destroy_(pivot)
      call destroy_(LU)

   end subroutine

   subroutine solve_linear_equations(self,rhs,solution)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Solve the linear equations posed by "self", with "rhs" as a matrix of RHS
    ! vectors, yeilding matrix "solution" as a matrix of solution vectors.
      real(kind=kind(1.0d0)), dimension(:,:) :: rhs, solution

      call solve_linear_equations_LAPACK_(self,rhs,solution)
   end subroutine

   subroutine solve_linear_equations_ESSL(self,rhs,solution)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Solve the linear equations posed by "self", with "rhs" as a matrix of RHS
    ! vectors, yeilding matrix "solution" as a matrix of solution vectors.
    ! ESSL version.
      real(kind=kind(1.0d0)), dimension(:,:) :: rhs, solution
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: LU
      integer(kind=kind(1)), dimension(:), pointer :: pivot
      integer(kind=kind(1)) :: dim1,nrhs

      call ensure_(tonto,is_square_(self),"REALMAT:solve_linear_equations_ESSL ... non-square matrix")
      call ensure_(tonto,size(rhs,1)==size(self,2),"REALMAT:solve_linear_equations_ESSL ... rhs incompatible with coefficient&
& matrix")
      call ensure_(tonto,nrhs>0,"REALMAT:solve_linear_equations_ESSL ... no rhs vectors")
      dim1 = size(rhs,1)
      nrhs = size(rhs,2)
      call create_(LU,dim1,dim1)
      call create_(pivot,dim1)
      LU = self
      solution = rhs
      call destroy_(pivot)
      call destroy_(LU)

   end subroutine

   subroutine solve_linear_equations_LAPACK(self,rhs,solution)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Solve the linear equations posed by "self", with "rhs" as a matrix of RHS
    ! vectors, yeilding matrix "solution" as a matrix of solution vectors.
    ! LAPACK version
      real(kind=kind(1.0d0)), dimension(:,:) :: rhs, solution
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: LU
      integer(kind=kind(1)), dimension(:), pointer :: pivot
      integer(kind=kind(1)) :: dim1,nrhs,err

      call ensure_(tonto,is_square_(self),"REALMAT:solve_linear_equations_LAPACK ... non-square matrix")
      call ensure_(tonto,size(rhs,1)==size(self,2),"REALMAT:solve_linear_equations_LAPACK ... rhs incompatible with coefficie&
&nt matrix")
      call ensure_(tonto,nrhs>0,"REALMAT:solve_linear_equations_LAPACK ... no rhs vectors")
      dim1 = size(rhs,1)
      nrhs = size(rhs,2)
      call create_(LU,dim1,dim1)
      call create_(pivot,dim1)
      LU = self
      solution = rhs
      call dgesv(dim1,nrhs,LU,dim1,pivot,solution,dim1,err)
      call destroy_(pivot)
      call destroy_(LU)
      call ensure_(tonto,err==0,"REALMAT:solve_linear_equations_LAPACK ... no solution, error found")

   end subroutine

!  Unused ESSL routines

!   solve_general_eigenproblem(eigenvalues,eigenvectors)
!   ! Solve the eigenproblem for "self", yeilding a vector of "eigenvalues" and
!   ! a matrix of "eigenvectors"
!      eigenvalues :: CPXVEC
!      eigenvectors :: CPXMAT
!       W :: REALVEC*
!      dim1,dim2,dime,dimv :: integer(kind=kind(1))
!      select :: logical(kind=kind(.true.))
!      dim1 = size(self,1)
!      dim2 = size(self,2)
!      dime = size(eigenvalues)
!      dimv = size(eigenvectors)
!      call ensure_(tonto,dim1==dim2,"non-square matrix")
!      call ensure_(tonto,dime>=dim1,"eigenvalue array too small")
!      call ensure_(tonto,dimv>=dim1*dim1,"eigenvector matrix too small")
!      W.create(2*dim1)
!      call dgeev(1,self,dim1,eigenvalues,eigenvectors,dim1,select,dim1,W,2*dim1)
!      W.destroy
!   end
!
!   solve_general_eigenproblem(eigenvalues,left,right,normalize)
!   ! Solve the eigenproblem for "self", yeilding a vector of "eigenvalues" and
!   ! a matrix of "left" and "right" eigenvectors. If "normalize" is present
!   ! and .false., the left and right eigenvectors are not automatically
!   ! renormalized so that (left)^T (right) = 1.
!   ! NOTE : this routine fails if there are complex eigenvalues. Use the complex
!   ! routine in this case.
!      eigenvalues :: REALVEC, target
!      left,right :: SELF_TYPE, target
!      normalize :: logical(kind=kind(.true.)), optional
!      er,ei,W :: REALVEC*
!      A,le,re :: SELF_TYPE*
!      i,dim,dimW, info :: integer(kind=kind(1))
!      normalise :: logical(kind=kind(.true.))
!      dot :: real(kind=kind(1.0d0))
!      call ensure_(tonto,.is_square,"non-square matrix")
!      call ensure_(tonto,size(eigenvalues)>=.dim1,"eigenvalues array too small")
!      call ensure_(tonto,size(left)>=size(self),"left eigenvector matrix too small")
!      call ensure_(tonto,size(right)>=size(self),"right eigenvector matrix too small")
!      dim = size(self,1)
!      normalise = .true.
!      if (present(normalize)) normalise = normalize
!      if (.is_symmetric) then
!         .solve_symmetric_eigenproblem(eigenvalues,right)
!         left = right
!      else
!         A.create(dim,dim)
!         ei.create(dim)
!         er => eigenvalues
!         le => left
!         re => right
!         dimW = 8*dim
!         W.create(dimW)
!         A = self
!         ! Solve the eigenvalueproblem
!         call dgeev('V','V',dim,A,dim,er,ei,le,dim,re,dim,W,dimW,info)
!         call ensure_(tonto,info==0,"error, info="// trim(info.to_str))
!         ! Search for the complex eigenvalues/vectors
!         do i = 1,dim
!            if (.not. ei(i).is_zero(10.0d0**(-20))) then
!               call die_(tonto,"There are complex eigenvalues, use the complex routine")
!            end
!         end
!         W.destroy
!         ei.destroy
!         A.destroy
!      end
!      if (normalise) then
!         do i = 1,dim
!            dot = dot_product(left(:,i),right(:,i))
!            dot = 1.0d0/sqrt(dot)
!            left(:,i)  = dot*left(:,i)
!            right(:,i) = dot*right(:,i)
!         end
!      end
!   end

   function trace(self) result(res)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Return the trace of self
      intent(in) :: self
      real(kind=kind(1.0d0)) :: res
    ! The following code is inherited from INTRINSICMAT
      integer(kind=kind(1)) :: dim,i

      call ensure_(tonto,size(self,1)==size(self,2),"REALMAT:trace ... non-square matrix")
      dim = size(self,1)
      res = 0.0d0
      do i = 1,dim
         res = res + self(i,i)
      end do

   end function

   function trace_product_with(self,b) result(res)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Return the trace of the product of "self" with matrix b.
      intent(in) :: self
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: b
      real(kind=kind(1.0d0)) :: res
    ! The following code is inherited from INTRINSICMAT
      integer(kind=kind(1)) :: i

      call ensure_(tonto,is_transposed_shape_of_(self,b),"REALMAT:trace_product_with ... incompatible dimensions")
      res = 0.0d0
      do i = 1,size(self,1)
         res = res + sum( self(i,:)*b(:,i) )
      end do

   end function

   function trace_product_with_1(self,b,c,d) result(res)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Return the trace of the product of "self" with matrices "b" ... "d".
      real(kind=kind(1.0d0)), dimension(:,:) :: b,c,d
      real(kind=kind(1.0d0)) :: res
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: W1,W2

      call create_(W2,size(b,1),size(d,2))
      call create_(W1,size(c,1),size(d,2))
      call to_product_of_(W1,c,d)
      call to_product_of_(W2,b,W1)
      call destroy_(W1)
      call create_(W1, size(self,1),size(d,2))
      call to_product_of_(W1,self,W2)
      res = trace_(W1)
      call destroy_(W1)
      call destroy_(W2)

   end function

   function trace_product_with_2(self,b,c,d,e,f) result(res)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Return the trace of the product of "self" with matrices "b" ... "f".
      real(kind=kind(1.0d0)), dimension(:,:) :: b,c,d,e,f
      real(kind=kind(1.0d0)) :: res
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: W1,W2

      call create_(W1,size(e,1),size(f,2))
      call to_product_of_(W1,e,f)      ! e*f
      call create_(W2,size(d,1),size(f,2))
      call to_product_of_(W2,d,W1)     ! d*e*f
      call destroy_(W1); W1 => W2
      call create_(W2,size(c,1),size(f,2))
      call to_product_of_(W2,c,W1)     ! c*d*e*f
      call destroy_(W1); W1 => W2
      call create_(W2,size(b,1),size(f,2))
      call to_product_of_(W2,b,W1)     ! b*c*d*e*f
      call destroy_(W1); W1 => W2
      call create_(W2, size(self,1),size(f,2))
      call to_product_of_(W2,self,W1)  ! self*b*c*d*e*f
      res = trace_(W2)
      call destroy_(W2)
      call destroy_(W1)

   end function

   function equals(self,b) result(res)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Check if the matrix is the same as "b".
      intent(in) :: self
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: b
      logical(kind=kind(.true.)) :: res

      res = same_as_(self,b)

   end function

   function same_as(self,b,eps,diff) result(res)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Check if the matrix is the same as "b", within "eps", and return the
    ! actual difference in "diff"
      intent(in) :: self
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: b
      real(kind=kind(1.0d0)), intent(in), optional :: eps
      real(kind=kind(1.0d0)), optional :: diff
      logical(kind=kind(.true.)) :: res
      integer(kind=kind(1)) :: i
      real(kind=kind(1.0d0)) :: del,tolerance

   call ensure_(tonto,is_same_shape_as_(self,b),"REALMAT:same_as ... incompatible dimensions")
      tolerance = 10.0d0**(-6)
      if (present(eps)) tolerance = eps
      del = 0.0d0
      do i = 1,size(self,2)
         del = del + sum( (self(:,i)-b(:,i))**2 )
      end do
      del = sqrt(del)
      res = .false.
      if (del<tolerance) res=.true.
      if (present(diff)) diff=del

   end function

   function has_column(self,c,eps,col) result(res)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Returns .true. if the matrix "self" has a column "c", with "eps" tolerance.
    ! If present, the matching column index "col" is also returned.
      intent(in) :: self
      real(kind=kind(1.0d0)), dimension(:), intent(in) :: c
      real(kind=kind(1.0d0)), optional :: eps
      integer(kind=kind(1)), optional :: col
      logical(kind=kind(.true.)) :: res
      integer(kind=kind(1)) :: n

   call ensure_(tonto,size(c)==size(self,1),"REALMAT:has_column ... incompatible column size")
      res = .false.
      do n = 1,size(self,2)
         res = same_as_(self(:,n),c,eps)
         if (res) then
            if (present(col)) col = n
            exit
         end if
      end do

   end function

   function column_index(self,c,eps) result(res)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! The matching column index "col" is returned, if the column matches "col" to
    ! tolerance "tol".  "tol" is optional.
      intent(in) :: self
      real(kind=kind(1.0d0)), dimension(:), intent(in) :: c
      integer(kind=kind(1)) :: res
      real(kind=kind(1.0d0)), optional :: eps
      integer(kind=kind(1)) :: n

      call ensure_(tonto,size(c)==size(self,1),"REALMAT:column_index ... incompatible column size")
      res = 0
      do n = 1,size(self,2)
         if (same_as_(self(:,n),c,eps)) then
            res = n
            exit
         end if
      end do

   end function

   function is_diagonal(self) result(res)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Returns .true. if the matrix "self" is a diagonal matrix
      intent(in) :: self
      logical(kind=kind(.true.)) :: res
      integer(kind=kind(1)) :: dim,i,j
      logical(kind=kind(.true.)) :: off_diagonal_is_zero

      call ensure_(tonto,is_square_(self),"REALMAT:is_diagonal ... Non-square matrix")
      dim = size(self,1)
      res = .true.
      do i = 1,dim
      do j = 1,dim
         if (i==j) cycle
         off_diagonal_is_zero = is_zero_(self(i,j))
         if (off_diagonal_is_zero) cycle
         res = .false.
           return
      end do
      end do

   end function

   function has_unit_diagonal(self) result(res)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Returns .true. if the matrix "self" has 1's as diagonal elements
      intent(in) :: self
      logical(kind=kind(.true.)) :: res
      integer(kind=kind(1)) :: i
      logical(kind=kind(.true.)) :: diagonal_is_one

      call ensure_(tonto,is_square_(self),"REALMAT:has_unit_diagonal ... Non-square matrix")
      res = .true.
      do i = 1,size(self,1)
         diagonal_is_one = is_zero_((1.0d0 - self(i,i)))
         if (diagonal_is_one) cycle
         res = .false.
           return
      end do

   end function

   function is_symmetric(self) result(res)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Returns .true. if the matrix "self" is a symmetric matrix
      intent(in) :: self
      logical(kind=kind(.true.)) :: res
      integer(kind=kind(1)) :: dim,i,j
      real(kind=kind(1.0d0)) :: diff

      call ensure_(tonto,is_square_(self),"REALMAT:is_symmetric ... Non-square matrix")
      dim = size(self,1)
      res = .true.
      do i = 1,dim
      do j = 1,i-1
         diff = abs(self(i,j)-self(j,i))
         if (is_zero_(diff)) cycle
         res = .false.
           return
      end do
      end do

   end function

   function is_unit_matrix(self) result(res)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Returns .true. if the matrix "self" is the unit matrix
      intent(in) :: self
      logical(kind=kind(.true.)) :: res
      integer(kind=kind(1)) :: dim,i
      logical(kind=kind(.true.)) :: diagonal_is_one

      call ensure_(tonto,is_square_(self),"REALMAT:is_unit_matrix ... Non-square matrix")
      dim = size(self,1)
      res = .true.
      do i = 1,dim
         diagonal_is_one = is_zero_((self(i,i)-1.0d0))
         if (diagonal_is_one) cycle
         res = .false.
         exit
      end do
      if (res) res = is_diagonal_(self)

   end function

   function is_inversion_matrix(self) result(res)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Returns .true. if the matrix "self" is an inversion matrix
    ! i.e. minus the unit matrix
      intent(in) :: self
      logical(kind=kind(.true.)) :: res
      integer(kind=kind(1)) :: dim,i
      logical(kind=kind(.true.)) :: diagonal_is_minus_one

      call ensure_(tonto,is_square_(self),"REALMAT:is_inversion_matrix ... Non-square matrix")
      dim = size(self,1)
      res = .true.
      do i = 1,dim
         diagonal_is_minus_one = is_zero_((self(i,i)+1.0d0))
         if (diagonal_is_minus_one) cycle
         res = .false.
         exit
      end do
      if (res) res = is_diagonal_(self)

   end function

   function sum_row_vectors(self) result(res)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Sum the row vectors (i.e. columns) in "self".
      real(kind=kind(1.0d0)), dimension(size(self,2)) :: res
      integer(kind=kind(1)) :: j

      do j = 1,size(self,2)
         res(j) = sum(self(:,j))
      end do

   end function

   function sum_column_vectors(self) result(res)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Sum the column vectors (i.e. rows) in "self".
      real(kind=kind(1.0d0)), dimension(size(self,1)) :: res
      integer(kind=kind(1)) :: i

      do i = 1,size(self,1)
         res(i) = sum(self(i,:))
      end do

   end function

   subroutine swap_columns(self,col1,col2)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Swap columns "col1" and "col2" of self
      integer(kind=kind(1)), intent(in) :: col1,col2
    ! The following code is inherited from INTRINSICMAT
      integer(kind=kind(1)) :: a1,a2,i
      real(kind=kind(1.0d0)) :: val

      call ensure_(tonto,col1<=size(self,2) .and. col2<=size(self,2),"REALMAT:swap_columns ... columns exceed dimesions")
      a1 = size(self,1)
      a2 = size(self,2)
      do i = 1,a1
         val = self(i,col1)
         self(i,col1) = self(i,col2)
         self(i,col2) = val
      end do

   end subroutine

   subroutine swap_columns_1(self,list)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Sequentially swap all columns in a column "list",
    ! self(:,i)      = self(:,list(i))
    ! self(:,col(i)) = self(:,i)
      integer(kind=kind(1)), dimension(:), intent(in) :: list
      integer(kind=kind(1)) :: l

      call ensure_(tonto,maxval(list)<=size(self,2),"REALMAT:swap_columns_1 ... list value exceed column dimension")
      do l = 1,size(list)
         call swap_columns_(self,l,list(l))
      end do

   end subroutine

   function column_norms(self) result(res)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Return the norms of every column
      real(kind=kind(1.0d0)), dimension(size(self,2)) :: res
      integer(kind=kind(1)) :: i

      do i = 1,size(self,2)
         res(i) = norm_(self(:,i))
      end do

   end function

   subroutine get_column_norms(self,res)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Return the norms of every column
      real(kind=kind(1.0d0)), dimension(:) :: res
      integer(kind=kind(1)) :: i

   call ensure_(tonto,size(res)==size(self,2),"REALMAT:get_column_norms ... wrong size, res array")
      do i = 1,size(self,2)
         res(i) = norm_(self(:,i))
      end do

   end subroutine

   subroutine get_column_dot_products(self,res)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Return the dot products of every column with itself.
    ! Goot for testing distances without using a sqrt.
      real(kind=kind(1.0d0)), dimension(:) :: res
      integer(kind=kind(1)) :: i

   call ensure_(tonto,size(res)==size(self,2),"REALMAT:get_column_dot_products ... wrong size, res array")
      do i = 1,size(self,2)
         res(i) = dot_product(self(:,i),self(:,i))
      end do

   end subroutine

   function index_of_minimum_column_norm(self,offset) result(res)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Return the column index of the column with the *minimum* norm. If present,
    ! "offset" is subtracted from every column beforehand, and then added back
    ! afterwards. This is useful for finding the index of the column with minimum
    ! distance to "offset", for a list of points held in "self".
      real(kind=kind(1.0d0)), dimension(3), optional :: offset
      integer(kind=kind(1)) :: res
      integer(kind=kind(1)) :: i
      real(kind=kind(1.0d0)) :: val,tmp

      if (present(offset)) &
         self = self - spread(offset,dim=2,ncopies=size(self,2))
      res = 1
      val = norm_(self(:,1))
      do i = 2,size(self,2)
         tmp = norm_(self(:,i))
         if (tmp>=val) cycle
         val = tmp
         res = i
      end do
      if (present(offset)) &
         self = self + spread(offset,dim=2,ncopies=size(self,2))

   end function

   function mean_column_vector(self) result(res)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Return the mean of the column vectors.
      real(kind=kind(1.0d0)), dimension(size(self,1)) :: res

      res = sum_column_vectors_(self)/size(self,2)

   end function

   function max_abs_column_difference(self) result(res)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Return the maximum of the absolute difference between all the column vector
    ! pairs of the matrix.
      real(kind=kind(1.0d0)), dimension(size(self,1)) :: res
      integer(kind=kind(1)) :: i,j,dim
      real(kind=kind(1.0d0)), dimension(size(self,1)) :: diff,col_i,col_j

      dim = size(self,2)
      diff = 0.0d0
      do i = 1,size(self,2)
         col_i = self(:,i)
         do j = 1,i-1
            col_j = self(:,j)
            diff = max(abs(col_i-col_j),diff)
         end do
      end do
      res = diff

   end function

   subroutine set_to(self,b)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Set self to "b"
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: b
    ! The following code is inherited from INTRINSICMAT

      call ensure_(tonto,is_same_shape_as_(self,b),"REALMAT:set_to ... incompatible shape")
      self = b

   end subroutine

   subroutine plus(self,b)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Add to self the matrix "b"
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: b
    ! The following code is inherited from INTRINSICMAT

      call ensure_(tonto,is_same_shape_as_(self,b),"REALMAT:plus ... incompatible shape")
      self = self+b

   end subroutine

   subroutine minus(self,b)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Subtract from self the matrix "b"
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: b
    ! The following code is inherited from INTRINSICMAT

      call ensure_(tonto,is_same_shape_as_(self,b),"REALMAT:minus ... incompatible shape")
      self = self-b

   end subroutine

   subroutine to_scaled_mat(self,fac,b)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Set "self" to matrix "b" scaled by "fac"
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: b
      real(kind=kind(1.0d0)), intent(in) :: fac

      call ensure_(tonto,is_same_shape_as_(self,b),"REALMAT:to_scaled_mat ... different shapes")
      self = fac*b

   end subroutine

   subroutine plus_scaled_mat(self,fac,b)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Add to "self" matrix "b" scaled by "fac"
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: b
      real(kind=kind(1.0d0)), intent(in) :: fac

      call ensure_(tonto,is_same_shape_as_(self,b),"REALMAT:plus_scaled_mat ... different shapes")
      self = self+fac*b

   end subroutine

   pure subroutine zero_small_values(self,tol)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Zero elements of the matrix which are less than "tol" in magnitude
      intent(inout) :: self
      real(kind=kind(1.0d0)), intent(in) :: tol
      where (abs(self)<tol)
        self = 0.0d0
      end where

   end subroutine

   function is_zero(self,eps) result(res)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Return .true. is "self" is the zero matrix, i.e. every element is zero.
    ! If present, "eps" is used to decide when a small number is zero.
      real(kind=kind(1.0d0)), optional, intent(in) :: eps
      logical(kind=kind(.true.)) :: res
      integer(kind=kind(1)) :: dim1,dim2,i,j
      logical(kind=kind(.true.)) :: ij_is_zero

      dim1 = size(self,1)
      dim2 = size(self,2)
      res = .true.
      do i = 1,dim1
      do j = 1,dim2
         ij_is_zero = is_zero_(self(i,j),eps)
         if (ij_is_zero) cycle
         res = .false.
         exit
      end do
      end do

   end function

   subroutine change_basis(self,V)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Change the basis of "self" using vectors "V"; self = V^T self V
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: V
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: W
      integer(kind=kind(1)) :: o1,v2

      call ensure_(tonto,is_square_(self),"REALMAT:change_basis ... Non-square matrix")
      call ensure_(tonto,is_square_(V),"REALMAT:change_basis ... Non-square matrix")
      call ensure_(tonto,size(self,2)==size(V,1),"REALMAT:change_basis ... wrong shapes")
      o1 = size(self,1)
      v2 = size(V,2)
      call create_(W,o1,v2)
      call to_product_of_(W,self,V)
      call to_product_of_(self,V,W,transpose_a=.true.)
      call destroy_(W)

   end subroutine

   subroutine change_basis_1(self,new,V)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Change the basis of "self" using vectors "V", and place the result in
    ! "new".  new = V^T self V
      real(kind=kind(1.0d0)), dimension(:,:) :: new,V
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: W
      integer(kind=kind(1)) :: o1,v2

      call ensure_(tonto,is_square_(self),"REALMAT:change_basis_1 ... Non-square matrix")
      call ensure_(tonto,is_square_(new),"REALMAT:change_basis_1 ... Non-square matrix")
      call ensure_(tonto,size(V,1)==   size(self,2),"REALMAT:change_basis_1 ... wrong shapes")
      call ensure_(tonto,size(V,2)==size(new,2),"REALMAT:change_basis_1 ... wrong shapes")
      o1 = size(self,1)
      v2 = size(V,2)
      call create_(W,o1,v2)
      call to_product_of_(W,self,V)
      call to_product_of_(new,V,W,transpose_a=.true.)
      call destroy_(W)

   end subroutine

   subroutine change_basis_2(self,new,L,R)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Change the basis of "self" using left and right matrices "L" and "R"
    ! and place the result in "new", new = L^T self R
      real(kind=kind(1.0d0)), dimension(:,:) :: new,L,R
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: W
      integer(kind=kind(1)) :: o1,r2

      call ensure_(tonto,size(self,2)==size(R,1),"REALMAT:change_basis_2 ... incompatible sizes")
      call ensure_(tonto,size(self,1)==size(L,1),"REALMAT:change_basis_2 ... incompatible sizes")
      call ensure_(tonto,size(new,2)==size(R,2),"REALMAT:change_basis_2 ... incompatible sizes")
      call ensure_(tonto,size(new,1)==size(L,2),"REALMAT:change_basis_2 ... incompatible sizes")
      o1 = size(self,1)
      r2 = size(R,2)
      call create_(W,o1,r2)
      call to_product_of_(W,self,R)
      call to_product_of_(new,L,W,transpose_a=.true.)
      call destroy_(W)

   end subroutine

   subroutine change_basis_3(self,L,R)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Change the basis of "self" using diagonal matrices "L" and "R" (stored as
    ! vectors).  self = L self R
      real(kind=kind(1.0d0)), dimension(:) :: L,R
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: W
      integer(kind=kind(1)) :: l1,r1

      call ensure_(tonto,size(self,1)==size(L) .and. size(self,2)==size(R),"REALMAT:change_basis_3 ... incompatible sizes")
      l1 = size(L)
      r1 = size(R)
      call create_(W,l1,r1)
      call to_product_with_diagonal_(W,self,R)
      call to_product_with_diagonal_(self,L,W)
      call destroy_(W)

   end subroutine

   subroutine change_basis_4(self,V)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Change the basis of "self" using diagonal matrix "V" (stored as a vectors).
    ! self = V self V
      real(kind=kind(1.0d0)), dimension(:) :: V
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: W
      integer(kind=kind(1)) :: v1

      call ensure_(tonto,is_square_(self),"REALMAT:change_basis_4 ... non-square matrix")
      call ensure_(tonto,size(self,1)==size(V),"REALMAT:change_basis_4 ... incompatible sizes")
      v1 = size(V)
      call create_(W,v1,v1)
      call to_product_with_diagonal_(W,self,V)
      call to_product_with_diagonal_(self,V,W)
      call destroy_(W)

   end subroutine

   subroutine back_transform(self,V)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Back transform "self" using vectors "V", and place the result in "self".
    ! self = V self V^T
      real(kind=kind(1.0d0)), dimension(:,:) :: V
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: W
      integer(kind=kind(1)) :: o2,v1

      call ensure_(tonto,is_square_(self),"REALMAT:back_transform ... non-square matrix")
      call ensure_(tonto,is_same_shape_as_(self,V),"REALMAT:back_transform ... incompatible shape")
      o2 = size(self,2)
      v1 = size(V,1)
      call create_(W,v1,o2)
      call to_product_of_(W,V,self)
      call to_product_of_(self,W,V,transpose_b=.true.)
      call destroy_(W)

   end subroutine

   subroutine back_transform_1(self,new,V)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Back transform "self" using vectors "V", and place the result in "new".
    ! new = V self V^T
      real(kind=kind(1.0d0)), dimension(:,:) :: new,V
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: W
      integer(kind=kind(1)) :: o2,v1

      call ensure_(tonto,is_square_(self),"REALMAT:back_transform_1 ... non-square matrix")
      call ensure_(tonto,is_square_(new),"REALMAT:back_transform_1 ... non-square matrix")
      call ensure_(tonto,size(V,2)==size(self,1),"REALMAT:back_transform_1 ... incompatible sizes")
      call ensure_(tonto,size(V,1)==size(new,1),"REALMAT:back_transform_1 ... incompatible sizes")
      o2 = size(self,2)
      v1 = size(V,1)
      call create_(W,v1,o2)
      call to_product_of_(W,V,self)
      call to_product_of_(new,W,V,transpose_b=.true.)
      call destroy_(W)

   end subroutine

   subroutine back_transform_2(self,new,L,R)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Back transform "self" using left and right matrices "L" and "R"
    ! and place the result in "new", new = L self R^T
      real(kind=kind(1.0d0)), dimension(:,:) :: new,L,R
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: W
      integer(kind=kind(1)) :: o1,r1

      call ensure_(tonto,size(self,2)==size(R,2),"REALMAT:back_transform_2 ... incompatible sizes")
      call ensure_(tonto,size(self,1)==size(L,2),"REALMAT:back_transform_2 ... incompatible sizes")
      call ensure_(tonto,size(new,2)==size(R,1),"REALMAT:back_transform_2 ... incompatible sizes")
      call ensure_(tonto,size(new,1)==size(L,1),"REALMAT:back_transform_2 ... incompatible sizes")
      o1 = size(self,1)
      r1 = size(R,1)
      call create_(W,o1,r1)
      call to_product_of_(W,self,R,transpose_b=.true.)
      call to_product_of_(new,L,W)
      call destroy_(W)

   end subroutine

   subroutine similarity_transform(self,V)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Do a similarity transform of "self" using vectors "V": self = V self V^-1
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: V
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: V1,W
      integer(kind=kind(1)) :: n

   call ensure_(tonto,is_square_(self),"REALMAT:similarity_transform ... Non-square matrix")
   call ensure_(tonto,is_square_(V),"REALMAT:similarity_transform ... Non-square matrix")
   call ensure_(tonto,size(self,1)==size(V,1),"REALMAT:similarity_transform ... wrong shapes")
      n = size(self,1)
      call create_(V1,n,n)
      call to_inverse_of_(V1,V)
      call create_(W,n,n)
      call to_product_of_(W,self,V1)
      call to_product_of_(self,V,W)
      call destroy_(W)
      call destroy_(V1)

   end subroutine

   subroutine compress_to_triangle(self,tr)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Converts the lower triangle of matrix self to the triangle "tr".
    ! using row order.
      intent(in) :: self
      real(kind=kind(1.0d0)), dimension(:) :: tr
      integer(kind=kind(1)) :: dim,i,j,ij

      call ensure_(tonto,is_square_(self),"REALMAT:compress_to_triangle ... non-square matrix")
      call ensure_(tonto,size(tr)>=tri_size_(self),"REALMAT:compress_to_triangle ... triangle array too small")
      dim = size(self,1)
      ij = 0
      do i = 1,dim
         do j = 1,i
            tr(ij+j) = self(i,j)
!            tr(ij+j) = self(j,i)
         end do
         ij = ij+i
      end do

   end subroutine

   subroutine uncompress_from_triangle(self,tr)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Converts the triangle "tr" into the symmetric matrix "self".
      real(kind=kind(1.0d0)), dimension(:) :: tr
      real(kind=kind(1.0d0)) :: tmp
      integer(kind=kind(1)) :: dim,i,j,ij

      call ensure_(tonto,is_square_(self),"REALMAT:uncompress_from_triangle ... non-square matrix")
      call ensure_(tonto,size(tr)>=tri_size_(self),"REALMAT:uncompress_from_triangle ... triangle array too small")
      dim = size(self,1)
      ij = 0
      do i = 1,dim
         do j = 1,i
            tmp = tr(ij+j)
            self(j,i) = tmp
            self(i,j) = tmp
         end do
         ij = ij+i
      end do

   end subroutine

   subroutine from_diagonal(self,d)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Converts the diagonal vector "d" to matrix "self".
      real(kind=kind(1.0d0)), dimension(:) :: d
      integer(kind=kind(1)) :: dim,i

      call ensure_(tonto,is_square_(self),"REALMAT:from_diagonal ... non-square matrix")
      call ensure_(tonto,size(d)==size(self,1),"REALMAT:from_diagonal ... incompatibale diagonal length")
      dim  = size(d)
      self = 0.0d0
      do i = 1,dim
         self(i,i) = d(i)
      end do

   end subroutine

   function tri_size(self) result(ltr)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Returns the size of the lower triangle needed to store self.
      intent(in) :: self
      integer(kind=kind(1)) :: ltr
      integer(kind=kind(1)) :: dim

      call ensure_(tonto,is_square_(self),"REALMAT:tri_size ... non-square matrix")
      dim = size(self,1)
      ltr = dim*(dim+1)/2

   end function

   subroutine to_unit_matrix(self)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Set "self" to the unit matrix

      call to_unit_mat_(self)

   end subroutine

   subroutine to_unit_mat(self)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Set "self" to the unit matrix
      integer(kind=kind(1)) :: i

   call ensure_(tonto,is_square_(self),"REALMAT:to_unit_mat ... non-square matrix")
      self = 0.0d0
      do i = 1,size(self,1)
         self(i,i) = 1.0d0
      end do

   end subroutine

   subroutine set_diagonal(self,val)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Set the diagonal of "self" to "val"
      real(kind=kind(1.0d0)) :: val
      integer(kind=kind(1)) :: dim,i

      call ensure_(tonto,is_square_(self),"REALMAT:set_diagonal ... non-square matrix")
      dim = size(self,1)
      do i = 1,dim
         self(i,i) = val
      end do

   end subroutine

   subroutine add_to_diagonal(self,val)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Add "val" to the diagonal of "self"
      real(kind=kind(1.0d0)) :: val
      integer(kind=kind(1)) :: dim,i

      call ensure_(tonto,is_square_(self),"REALMAT:add_to_diagonal ... non-square matrix")
      dim = size(self,1)
      do i = 1,dim
         self(i,i) = self(i,i) + val
      end do

   end subroutine

   subroutine zero_diagonal(self)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Zero the diagonal elements of "self"
      integer(kind=kind(1)) :: dim,i

      call ensure_(tonto,is_square_(self),"REALMAT:zero_diagonal ... non-square matrix")
      dim = size(self,1)
      do i = 1,dim
         self(i,i) = 0.0d0
      end do

   end subroutine

   subroutine zero_off_diagonal(self)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Zero the off diagonal elements of "self"
      integer(kind=kind(1)) :: dim,i,j

      call ensure_(tonto,is_square_(self),"REALMAT:zero_off_diagonal ... non-square matrix")
      dim = size(self,1)
      do i = 1,dim
      do j = 1,dim
         if (i==j) cycle
         self(i,j) = 0.0d0
      end do
      end do

   end subroutine

   subroutine weight_diagonal(self,fac)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Weight the diagonal elements of "self" by "fac"
      real(kind=kind(1.0d0)), intent(in) :: fac
      integer(kind=kind(1)) :: dim,i

      call ensure_(tonto,is_square_(self),"REALMAT:weight_diagonal ... non-square matrix")
      dim = size(self,1)
      do i = 1,dim
         self(i,i) = fac*self(i,i)
      end do

   end subroutine

   subroutine get_diagonal(self,diag)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Get the diagonal elements of "self" in vector "diag"
      real(kind=kind(1.0d0)), dimension(:) :: diag
      integer(kind=kind(1)) :: dim,i

      call ensure_(tonto,size(diag)==min(size(self,1),size(self,2)),"REALMAT:get_diagonal ... diag vector is incompatible")
      dim  = size(diag)
      do i = 1,dim
         diag(i) = self(i,i)
      end do

   end subroutine

   function max_diagonal_element(self) result(res)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Get the maximum element on the diagonal of the matrix
      real(kind=kind(1.0d0)) :: res
      integer(kind=kind(1)) :: dim,i

      call ensure_(tonto,min(size(self,1),size(self,2))>0,"REALMAT:max_diagonal_element ... cannot have zero sized dimensions&
&")
      dim = min(size(self,1),size(self,2))
      res = self(1,1)
      do i = 2,dim
         res = max(self(i,i),res)
      end do

   end function

   function max_abs_diagonal_element(self) result(res)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Get the maximum absolute value of the diagonal elements of the self matrix
      real(kind=kind(1.0d0)) :: res
      integer(kind=kind(1)) :: dim,i

      call ensure_(tonto,min(size(self,1),size(self,2))>0,"REALMAT:max_abs_diagonal_element ... cannot have zero sized dimens&
&ions")
      dim = min(size(self,1),size(self,2))
      res = abs(self(1,1))
      do i = 2,dim
         res = max(abs(self(i,i)),res)
      end do

   end function

   subroutine symmetrize(self)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Set self to half of itself plus half its transpose, i.e.
    ! self = 1/2 (self + self^T)
      integer(kind=kind(1)) :: dim,i,j
      real(kind=kind(1.0d0)) :: val

   call ensure_(tonto,is_square_(self),"REALMAT:symmetrize ... non-square matrix")
      dim = size(self,1)
      do i = 1,dim
         do j=1,i-1
            val = 0.50d0*(self(i,j)+self(j,i))
            self(i,j) = val
            self(j,i) = val
         end do
      end do

   end subroutine

   subroutine antisymmetrize(self)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Set self to half of itself minus half its transpose, i.e.
    ! self = 1/2 (self - self^T)
      integer(kind=kind(1)) :: dim,i,j
      real(kind=kind(1.0d0)) :: val

   call ensure_(tonto,is_square_(self),"REALMAT:antisymmetrize ... non-square matrix")
      dim = size(self,1)
      do i = 1,dim
         do j=1,i
            val = 0.50d0*(self(i,j)-self(j,i))
            self(i,j) =  val
            self(j,i) = -val
         end do
      end do

   end subroutine

   subroutine symmetric_fold(self)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Add the upper triangle of "self" into the lower triangle
      integer(kind=kind(1)) :: dim,i,j

      call ensure_(tonto,is_square_(self),"REALMAT:symmetric_fold ... non-square matrix")
      dim = size(self,1)
      do i = 1,dim
         do j=1,i-1
            self(i,j) = self(i,j)+self(j,i)
         end do
      end do

   end subroutine

   subroutine antisymmetric_fold(self)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Subtract the upper triangle of "self" into the lower triangle
      integer(kind=kind(1)) :: dim,i,j

      call ensure_(tonto,is_square_(self),"REALMAT:antisymmetric_fold ... non-square matrix")
      dim = size(self,1)
      do i = 1,dim
         do j=1,i-1
            self(i,j) = self(i,j)-self(j,i)
         end do
      end do

   end subroutine

   subroutine symmetric_fold_to_tri(self,tr)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Add the upper triangle of "self" into the lower triangle and return
    ! the lower triangle "tr"
      real(kind=kind(1.0d0)), dimension(:) :: tr
      integer(kind=kind(1)) :: dim,i,j,ij

      call ensure_(tonto,is_square_(self),"REALMAT:symmetric_fold_to_tri ... non-square matrix")
      call ensure_(tonto,size(tr)>=tri_size_(self),"REALMAT:symmetric_fold_to_tri ... triangle array too small")
      dim = size(self,1)
      ij = 0
      do i = 1,dim
         do j = 1,i
            ij = ij+1
            if (i==j) then
               tr(ij) = self(i,j)
            else
               tr(ij) = self(i,j)+self(i,j)
            end if
         end do
      end do

   end subroutine

   subroutine symmetric_reflect(self)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Make the upper triangle of "self" the same as the lower triangle
      integer(kind=kind(1)) :: dim,i,j

      call ensure_(tonto,is_square_(self),"REALMAT:symmetric_reflect ... non-square matrix")
      dim = size(self,1)
      do i = 1,dim
         do j = 1,i-1
            self(j,i) = self(i,j)
         end do
      end do

   end subroutine

   subroutine antisymmetric_reflect(self)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Make the upper triangle of "self" the negative of the lower triangle
      integer(kind=kind(1)) :: dim,i,j

      call ensure_(tonto,is_square_(self),"REALMAT:antisymmetric_reflect ... non-square matrix")
      dim = size(self,1)
      do i = 1,dim
         do j = 1,i-1
            self(j,i) = -self(i,j)
         end do
      end do
      do i = 1,dim
         self(i,i) = 0.0d0
      end do

   end subroutine

   subroutine schmidt_orthonormalise(self,S,scale)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Schmidt orthonormalise the column vectors in "self" using "S" as the
    ! metric. If "scale" is present, it is set to the product of the
    ! normalisation factors used to normalise each column after the Schmidt
    ! procedure.
     real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: S
     real(kind=kind(1.0d0)), optional :: scale
     target :: self
     integer(kind=kind(1)) :: dim,n,o
     real(kind=kind(1.0d0)), dimension(:), pointer :: old,new
     integer(kind=kind(1)) :: j,k
     real(kind=kind(1.0d0)) :: proj,norm
     real(kind=kind(1.0d0)), dimension(:), pointer :: T
     real(kind=kind(1.0d0)) :: fac

     call ensure_(tonto,is_square_(S),"REALMAT:schmidt_orthonormalise ... metric S is not square")
     call ensure_(tonto,size(self,1)==size(S,1),"REALMAT:schmidt_orthonormalise ... incompatible metric S")
     call ensure_(tonto,.not. is_zero_(S),"REALMAT:schmidt_orthonormalise ... S is zero matrix")
     call ensure_(tonto,.not. is_zero_(self),"REALMAT:schmidt_orthonormalise ... self is zero matrix")
     if (present(scale)) scale = 1.0d0

     if (present(scale)) then
       do n = 1,size(self,2)
          new => self(:,n)
          do o = 1,n-1
             old => self(:,o)
             fac = dot_(S,old,new)
             new = new - fac*old
          end do
          norm = dot_(S,new,new)
          call ensure_(tonto,norm>10.0d0**(-10),"REALMAT:schmidt_orthonormalise ... linear dependence in vector "//to_str_(n)&
&)
          norm = 1.0d0/sqrt(norm)
          new = new*norm
          scale = scale*norm
       end do
     end if

    dim = size(self,1)
    call create_(T,dim)
    do n=1,dim
      do j=1,dim
        T(j) = dot_product(self(:j,n),S(:j,j))
        T(1:j-1) = T(1:j-1) + self(j,n) * S(:j-1,j)
      end do
      do k=1,n-1
        proj = - dot_product(self(:,k),T)
        self(:,n) = self(:,n) + proj * self(:,k)
      end do
      do j=1,dim
        T(j) = dot_product(self(:j,n), S(:j,j))
        T(:j-1) = T(:j-1) + self(j,n) * S(:j-1,j)
      end do
      norm = dot_product(T,self(:,n))
      call ensure_(tonto,norm>10.0d0**(-10),"REALMAT:schmidt_orthonormalise ... linear dependence in vector " // to_str_(n))
      self(:,n) = self(:,n) / sqrt(norm)
    end do
    call destroy_(T)

   end subroutine

   subroutine reverse_schmidt_orthonormalise(self,S)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Schmidt orthonormalise the column vectors in "self" using "S" as the
    ! metric.
     target :: self
     real(kind=kind(1.0d0)), dimension(:,:) :: S
     real(kind=kind(1.0d0)), dimension(:), pointer :: new,old,w
     real(kind=kind(1.0d0)) :: fac,norm
     integer(kind=kind(1)) :: dim,n,k

     call ensure_(tonto,is_square_(self),"REALMAT:reverse_schmidt_orthonormalise ... non-square matrix")
     call ensure_(tonto,is_same_shape_as_(self,S),"REALMAT:reverse_schmidt_orthonormalise ... not same shape as S")
     call ensure_(tonto,.not. is_zero_(self),"REALMAT:reverse_schmidt_orthonormalise ... self is zero matrix")
     call ensure_(tonto,.not. is_zero_(self),"REALMAT:reverse_schmidt_orthonormalise ... self is zero matrix")
     dim = size(self,1)
     call create_(w,dim)
     do n = dim,1,-1
        new => self(:,n)
        do k = n-1,1,-1
           old => self(:,k)
           call to_product_of_(w,S,new)
           fac = dot_product(old,w)
           new = new - fac*old
        end do
        call to_product_of_(w,S,new)
        norm = dot_product(new,w)
        call ensure_(tonto,norm>10.0d0**(-10),"REALMAT:reverse_schmidt_orthonormalise ... linear dependence in vector " // to&
&_str_(n))
        new = new/sqrt(norm)
     end do
     call destroy_(w)

   end subroutine

   subroutine schmidt_orthonormalise_1(self)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Schmidt orthonormalise the column vectors in "self".
     target :: self
     real(kind=kind(1.0d0)), dimension(:), pointer :: new,old
     real(kind=kind(1.0d0)) :: fac,norm
     integer(kind=kind(1)) :: n,k

     call ensure_(tonto,size(self,1)>=size(self,2),"REALMAT:schmidt_orthonormalise_1 ... more vectors than dimension of vecto&
&r space")
     do n = 1,size(self,2)
        new => self(:,n)
        do k = 1,n-1
           old => self(:,k)
           fac = dot_product(old,new)
           new = new - fac*old
        end do
        norm = dot_product(new,new)
        call ensure_(tonto,norm>10.0d0**(-10),"REALMAT:schmidt_orthonormalise_1 ... linear dependence in vector " // to_str_(&
&n))
        new = new/sqrt(norm)
     end do

   end subroutine

   subroutine reverse_schmidt_orthogonalise(self)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Schmidt orthonormalise the column vectors in "self" using unit metric.
      target :: self
      real(kind=kind(1.0d0)), dimension(:), pointer :: new,old
      real(kind=kind(1.0d0)) :: fac,norm
      integer(kind=kind(1)) :: dim,n,k

      call ensure_(tonto,is_square_(self),"REALMAT:reverse_schmidt_orthogonalise ... non square matrix")
      dim = size(self,1)
      do n = dim,1,-1
         new => self(:,n)
         do k = n-1,1,-1
            old => self(:,k)
            fac = dot_product(old,new)
            new = new - fac*old
         end do
         norm = dot_product(new,new)
         call ensure_(tonto,norm>10.0d0**(-10),"REALMAT:reverse_schmidt_orthogonalise ... linear dependence in vector " // to&
&_str_(n))
         new = new/sqrt(norm)
      end do

   end subroutine

   subroutine symmetrically_orthonormalise(self,S)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Symmetrically orthonormalise the column vectors in "self" using "S" as the
    ! metric.
     real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: S
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: SS,SI
     integer(kind=kind(1)) :: dim

     call ensure_(tonto,.not. is_zero_(self),"REALMAT:symmetrically_orthonormalise ... self is zero matrix")
     dim = size(self,2)
     call create_(SI,dim,dim)
     call create_(SS,dim,dim)
     call change_basis_(S,SS,self)
     call to_inverse_sqrt_(SI,SS)
     call destroy_(SS)
     call create_(SS,size(self,1),size(self,2))
     call to_product_of_(SS,self,SI)
     self = SS
     call destroy_(SS)
     call destroy_(SI)

   end subroutine

   subroutine reverse_column_order(self)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Reverse the order of the columns of self.
      real(kind=kind(1.0d0)), dimension(:), pointer :: tmp
      integer(kind=kind(1)) :: n,n_col

      n_col = size(self,2)
      call create_(tmp,size(self,1))
      do n=1,n_col/2
        tmp = self(:,n_col-n+1)
        self(:,n_col-n+1) = self(:,n)
        self(:,n) = tmp
      end do
      call destroy_(tmp)

   end subroutine

   subroutine make_diagonally_dominant(self,permutation)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Rearrange the order of the columns of self so that the largest magnitude
    ! elements in each column occur along the diagonal. If "permutation" is
    ! present, it is a matrix which achieves this ordering, i.e. at the
    ! conclusion of the routine, self = self(:,permutation).
      integer(kind=kind(1)), dimension(:), optional :: permutation
      integer(kind=kind(1)), dimension(:), pointer :: perm
      logical(kind=kind(.true.)), dimension(:,:), pointer :: mask
      integer(kind=kind(1)) :: i,n
      integer(kind=kind(1)), dimension(2) :: loc

      call ensure_(tonto,is_square_(self),"REALMAT:make_diagonally_dominant ... not square")
      if (present(permutation)) &
      call ensure_(tonto,size(permutation)==size(self,2),"REALMAT:make_diagonally_dominant ... wrong size, perm")
      n = size(self,2)
      call create_(perm,n)
      call create_(mask,n,n)
      mask = .true.
      do i = 1,n
        loc = maxloc(abs(self),mask=mask)
        perm(loc(1))   = loc(2)
        mask(:,loc(2)) = .false.  ! eliminate this column next time
        mask(loc(1),:) = .false.  ! eliminate this row also
       ! write(*,*) " loc  =",loc
       ! write(*,*) " mask =",mask
      end do
      self = self(:,perm)
      if (present(permutation)) permutation = perm
      call destroy_(mask)
      call destroy_(perm)

   end subroutine

   subroutine to_sqrt(self,R)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! self = sqrt(R), cannot have R=self
      real(kind=kind(1.0d0)), dimension(:,:) :: R
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: evec
      real(kind=kind(1.0d0)), dimension(:), pointer :: eval,veci,vecj
      integer(kind=kind(1)) :: d,i,j
      real(kind=kind(1.0d0)) :: temp

      call ensure_(tonto,is_square_(self),"REALMAT:to_sqrt ... not square")
      d = size(R,1)
      call create_(eval,d)
      call create_(evec,d,d)
      call solve_eigenproblem_(R,eval,evec)
      do i = 1,d
         temp = eval(i)
         if (temp <= 0.0d0) then
           call warn_(tonto,"REALMAT:to_sqrt ... non-positive eigenvalue, " // trim(to_str_(temp,"e15.8")))
         end if
         eval(i) = sqrt(abs(temp))
      end do
      do i=1,d
        veci => evec(i,:)
        do j=1,d
          vecj => evec(j,:)
          self(i,j) = sum(veci*eval*vecj)
        end do
      end do
      call destroy_(evec)
      call destroy_(eval)

   end subroutine

   subroutine to_inverse_sqrt(self,R)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! self = sqrt(R)^(-1), cannot have R=self
      real(kind=kind(1.0d0)), dimension(:,:) :: R
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: evec
      real(kind=kind(1.0d0)), dimension(:), pointer :: eval,veci,vecj => NULL()
      integer(kind=kind(1)) :: d,i,j
      character(128) :: val
      real(kind=kind(1.0d0)) :: temp

      call ensure_(tonto,is_square_(self),"REALMAT:to_inverse_sqrt ... not square")
      d = size(R,1)
      call create_(eval,d)
      call create_(evec,d,d)
      call solve_eigenproblem_(R,eval,evec)
      do i = 1,d
         temp = eval(i)
         val = to_str_(temp,"e15.8")
         call warn_if_(tonto,temp<=0.0d0,"REALMAT:to_inverse_sqrt ... non-positive eigenvalue, "// trim(val))
         eval(i) = 1.0d0/sqrt(abs(temp))
      end do
      do i=1,d
        veci => evec(i,:)
        do j=1,d
          vecj => evec(j,:)
          self(i,j) = sum(veci*eval*vecj)
        end do
      end do
      call destroy_(evec)
      call destroy_(eval)

   end subroutine

   subroutine to_inverse_of(self,R)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! self = (R)^(-1); can have R=self
       real(kind=kind(1.0d0)), dimension(:,:) :: R

      call to_inverse_of_LAPACK_(self,R)
   end subroutine

   subroutine to_inverse_of_ESSL(self,R)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! self = (R)^(-1); can have R=self. ESSL version.
    ! This ESSL version is untested.
      real(kind=kind(1.0d0)), dimension(:,:) :: R
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: W
      integer(kind=kind(1)), dimension(:), pointer :: ipiv
      integer(kind=kind(1)) :: d,d2

      call ensure_(tonto,is_square_(self),"REALMAT:to_inverse_of_ESSL ... not square")
      call ensure_(tonto,is_same_shape_as_(self,R),"REALMAT:to_inverse_of_ESSL ... not same shape as R")
      d  = size(R,1)
      d2 = d*d
      self = R
      call create_(ipiv,d)
      call create_(W,d,d)
      call destroy_(W)
      call destroy_(ipiv)

   end subroutine

   subroutine to_inverse_of_LAPACK(self,R)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! self = (R)^(-1); can have R=self. LAPACK version.
      real(kind=kind(1.0d0)), dimension(:,:) :: R
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: W
      integer(kind=kind(1)), dimension(:), pointer :: ipiv
      integer(kind=kind(1)) :: d,d2,fail

      call ensure_(tonto,is_square_(self),"REALMAT:to_inverse_of_LAPACK ... not square")
      call ensure_(tonto,is_same_shape_as_(self,R),"REALMAT:to_inverse_of_LAPACK ... not same shape as R")
      d  = size(R,1)
      d2 = d*d
      self = R
      call create_(ipiv,d)
      call create_(W,d,d)
      fail = 0
      call dgetrf(d,d,self,d,ipiv,fail)
      call ensure_(tonto,fail==0,"REALMAT:to_inverse_of_LAPACK ... failed LU factorisation")
      call dgetri(d,self,d,ipiv,W,d2,fail)
      call ensure_(tonto,fail==0,"REALMAT:to_inverse_of_LAPACK ... failed back substitution")
      call destroy_(ipiv)
      call destroy_(W)

   end subroutine

   subroutine to_power_series_inverse_of(self,S,tol,max_it)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Set self to the power series inverse square root of "S".
    ! If "tol" is present, make sure that the maximum deviation from the exact
    ! answer is less than "tol" times the smallest element of "S". If "max_it"
    ! is present, use this as the maximum number of terms in the power series,
    ! before termination with an error.
      real(kind=kind(1.0d0)), dimension(:,:) :: S
      real(kind=kind(1.0d0)), optional :: tol
      integer(kind=kind(1)), optional :: max_it
      real(kind=kind(1.0d0)), dimension(:), pointer :: d
      integer(kind=kind(1)), dimension(:), pointer :: perm
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: W,X
      integer(kind=kind(1)) :: max_iter,n,k
      real(kind=kind(1.0d0)) :: eps

      call ensure_(tonto,is_square_(S),"REALMAT:to_power_series_inverse_of ... S not square")
      call ensure_(tonto,is_same_shape_as_(self,S),"REALMAT:to_power_series_inverse_of ... wrong shape")
      eps = 10.0d0**(-6)
      if (present(tol)) eps = tol
      max_iter = 100
      if (present(max_it)) max_iter = max_it
      n = size(S,1)
      call create_(perm,n)
      call create_(d,n)
      call create_(W,n,n)
      call create_(X,n,n)
      W = S
      call make_diagonally_dominant_(W,perm)
      call get_diagonal_(W,d)
      d = 1.0d0/d
      call to_product_with_diagonal_(X,d,W)
      call to_unit_matrix_(self)
      X = X - self
      W = X
      self = self - W
      k = 1
      do
         k = k + 1
         W = matmul(X,W)
         if (is_odd_(k)) then; self = self - W
         else;               self = self + W
         end if
         if (maxval(abs(W)) < eps) exit
         call ensure_(tonto,k<=max_iter,"REALMAT:to_power_series_inverse_of ... power series too long")
        ! write(*,*) "k = ",k
        ! write(*,*) "W = ",W
        ! write(*,*) "s = ",self
      end do
      call to_product_with_diagonal_(X,self,d)
      self = X
      self(perm,:) = self
      call destroy_(X)
      call destroy_(W)
      call destroy_(d)
      call destroy_(perm)

   end subroutine

   subroutine to_power_series_inv_sqrt_of(self,S,tol,max_it)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Set self to the inverse square root of "S", a matrix which is required to
    ! have a unit diagonal. The method uses a binomial power series expansion.
    ! If "tol" is present, make sure that the maximum deviation from the exact
    ! answer is less than "tol" times the smallest element of "S". If "max_it"
    ! is present, use this as the maximum number of terms in the power series,
    ! before termination with an error.
      real(kind=kind(1.0d0)), dimension(:,:) :: S
      real(kind=kind(1.0d0)), optional :: tol
      integer(kind=kind(1)), optional :: max_it
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: W,X
      integer(kind=kind(1)) :: max_iter,n,k
      real(kind=kind(1.0d0)) :: eps,fac

      call ensure_(tonto,has_unit_diagonal_(S),"REALMAT:to_power_series_inv_sqrt_of ... must have unit diagonal")
      call ensure_(tonto,is_square_(S),"REALMAT:to_power_series_inv_sqrt_of ... S not square")
      call ensure_(tonto,is_same_shape_as_(self,S),"REALMAT:to_power_series_inv_sqrt_of ... wrong shape")
      eps = 10.0d0**(-6)
      if (present(tol)) eps = tol
      max_iter = 100
      if (present(max_it)) max_iter = max_it
      n = size(S,1)
      call to_unit_matrix_(self)
      S = S - self
      call create_(X,n,n)
      call create_(W,n,n)
      fac = -0.50d0
      X = fac*S
      W = X
      self = self + X
      k = 1
      do
         k   = k + 1
         fac = fac - 1.0d0
         call to_scaled_product_of_(X,(fac/k),S,W)
         W = X
         self = self + X
         if (maxval(abs(X)) < eps) exit
         call ensure_(tonto,k<=max_iter,"REALMAT:to_power_series_inv_sqrt_of ... power series too long")
      end do
      call to_unit_matrix_(W)
      S = S + W
      call destroy_(W)
      call destroy_(X)

   end subroutine

   subroutine to_exponential_of(self,X,tol)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Exponentiate the matrix "X" using a power series expansion, self = exp(X),
       real(kind=kind(1.0d0)), dimension(:,:) :: X
      real(kind=kind(1.0d0)), optional :: tol

      call exponentiate_to_(X,self,tol)

   end subroutine

   subroutine exponentiate_to(self,U,tol)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Exponentiate the matrix self using a power series expansion, U = exp(self),
    ! so that the maximum deviation from the exact answer is less than "tol"
    ! if present.
      real(kind=kind(1.0d0)), dimension(:,:) :: U
      real(kind=kind(1.0d0)), optional :: tol
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: W
      integer(kind=kind(1)) :: n,k
      real(kind=kind(1.0d0)) :: eps,fac
      integer(kind=kind(1)), dimension(2) :: ind

      call ensure_(tonto,is_square_(U),"REALMAT:exponentiate_to ... U not square")
      call ensure_(tonto,is_same_shape_as_(self,U),"REALMAT:exponentiate_to ... wrong shape")
      eps = 10.0d0**(-6)
      if (present(tol)) eps = tol
      n = size(U,1)
      call to_unit_matrix_(U)
      call create_(W,n,n)
      W = self
      U = U + W
      k = 1
      do
         k   = k+1
         fac = 1.0d0/k
         W   = fac*self*W
         U   = U + W
         ind = maxloc(abs(W))
         if ( abs(W(ind(1),ind(2))) < eps ) exit
      end do
      call destroy_(W)

   end subroutine

   subroutine antisymmetric_exponential(self,U, eval,evec)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Make unitary matrix U = exp(self) where "self" must be antisymmetric.
    ! Uses the formula:  exp A = V (cos P) V^t + V (sin P)/P V^t A
    !                        P = sqrt diag(eig(A^t A))
    ! (c) dylan jayatilaka, university of western australia, 1993
    ! Untested in TONTO.
      real(kind=kind(1.0d0)), dimension(:,:) :: U
      real(kind=kind(1.0d0)), dimension(:,:), pointer, optional :: evec
      real(kind=kind(1.0d0)), dimension(:), pointer, optional :: eval
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: W
      integer(kind=kind(1)) :: dim1,dim2,dim,k
      real(kind=kind(1.0d0)) :: e,e2,cs,sn
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: v_k

      call ensure_(tonto,is_square_(self),"REALMAT:antisymmetric_exponential ... self is a non-square matrix")
      call ensure_(tonto,is_same_shape_as_(self,U),"REALMAT:antisymmetric_exponential ... incompatible shapes")
      dim = size(self,1)
      if (.not. present(eval)) then
         call create_(eval,dim)
      else
         dim2 = size(eval)
         call ensure_(tonto,dim2>=dim,"REALMAT:antisymmetric_exponential ... eval too small")
      end if
      if (.not. present(evec)) then
         call create_(evec,dim,dim)
      else
         dim1 = size(evec,1)
         dim2 = size(evec,2)
         call ensure_(tonto,dim1==dim .and. dim2==dim,"REALMAT:antisymmetric_exponential ... evec incompatible")
      end if
      call create_(W,dim,dim)

      U = matmul(self,self)    ! U = -self^t*self = self^2, makes U hermitian
      call solve_eigenproblem_(U,eval,evec)  ! diagonalise U ...

      U = 0.0d0
      do k = 1,dim             ! do the exponential ... loop over eigenvalues ...
         e2 = eval(k)
         if (e2<0)  then
            e = sqrt(-e2); cs = cos(e);  sn = sin(e)/e;
         end if
         if (e2>0)  then
            e = sqrt(e2) ; cs = cosh(e); sn = sinh(e)/e
         end if
         if (e2==0) then
            cs = 1.0d0    ; sn = 1.0d0
         end if
         v_k => evec(1:dim,k:k)
         call to_product_of_(W,v_k,v_k,transpose_b=.true.)  ! V V^\dag part
         call plus_scaled_product_of_(U,sn,W,self)        ! sin part
         call plus_scaled_mat_(U,cs,W)                 ! cos part
      end do
      if (.not. present(eval)) call destroy_(eval)
      if (.not. present(evec)) call destroy_(evec)

   end subroutine

!  ************************
!  Block returning routines
!  ************************

   function alpha_alpha(self) result(res)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! return the alpha-alpha sector of the matrix
      TARGET :: self
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: res
       integer(kind=kind(1)) :: n

   call ensure_(tonto,is_square_(self),"REALMAT:alpha_alpha ... non-square matrix")
   call ensure_(tonto,is_even_(size(self,1)),"REALMAT:alpha_alpha ... uneven dimension")
      n = size(self,1)/2
      res => self(1:n,1:n)

   end function

   function beta_alpha(self) result(res)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! return the beta-alpha sector of the matrix
      TARGET :: self
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: res
       integer(kind=kind(1)) :: n

   call ensure_(tonto,is_square_(self),"REALMAT:beta_alpha ... non-square matrix")
   call ensure_(tonto,is_even_(size(self,1)),"REALMAT:beta_alpha ... uneven dimension")
      n = size(self,1)/2
      res => self(n+1:2*n,1:n)

   end function

   function alpha_beta(self) result(res)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! return the alpha-beta sector of the matrix
      TARGET :: self
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: res
       integer(kind=kind(1)) :: n

   call ensure_(tonto,is_square_(self),"REALMAT:alpha_beta ... non-square matrix")
   call ensure_(tonto,is_even_(size(self,1)),"REALMAT:alpha_beta ... uneven dimension")
      n = size(self,1)/2
      res => self(1:n,n+1:2*n)

   end function

   function beta_beta(self) result(res)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! return the beta-beta sector of the matrix
      TARGET :: self
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: res
       integer(kind=kind(1)) :: n

   call ensure_(tonto,is_square_(self),"REALMAT:beta_beta ... non-square matrix")
   call ensure_(tonto,is_even_(size(self,1)),"REALMAT:beta_beta ... uneven dimension")
      n = size(self,1)/2
      res => self(n+1:2*n,n+1:2*n)

   end function

!  ***************
!  Set_to routines
!  ***************

   subroutine alpha_alpha_set_to(self,X,factor)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Set the alpha-alpha sector of the matrix to "X"
       real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: X
      real(kind=kind(1.0d0)), optional, intent(in) :: factor
       integer(kind=kind(1)) :: n

   call ensure_(tonto,is_square_(self),"REALMAT:alpha_alpha_set_to ... non-square matrix")
   call ensure_(tonto,is_even_(size(self,1)),"REALMAT:alpha_alpha_set_to ... uneven dimension")
      n = size(self,1)/2
      if (present(factor)) then; self(1:n,1:n) = factor*X
      else;                      self(1:n,1:n) = X
      end if

   end subroutine

   subroutine beta_alpha_set_to(self,X,factor)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Set the beta-alpha sector of the matrix to "X"
       real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: X
      real(kind=kind(1.0d0)), optional, intent(in) :: factor
       integer(kind=kind(1)) :: n

   call ensure_(tonto,is_square_(self),"REALMAT:beta_alpha_set_to ... non-square matrix")
   call ensure_(tonto,is_even_(size(self,1)),"REALMAT:beta_alpha_set_to ... uneven dimension")
      n = size(self,1)/2
      if (present(factor)) then; self(n+1:2*n,1:n) = factor*X
      else;                      self(n+1:2*n,1:n) = X
      end if

   end subroutine

   subroutine alpha_beta_set_to(self,X,factor)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Set the alpha-beta sector of the matrix to "X"
       real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: X
      real(kind=kind(1.0d0)), optional, intent(in) :: factor
       integer(kind=kind(1)) :: n

   call ensure_(tonto,is_square_(self),"REALMAT:alpha_beta_set_to ... non-square matrix")
   call ensure_(tonto,is_even_(size(self,1)),"REALMAT:alpha_beta_set_to ... uneven dimension")
      n = size(self,1)/2
      if (present(factor)) then; self(1:n,n+1:2*n) = factor*X
      else;                      self(1:n,n+1:2*n) = X
      end if

   end subroutine

   subroutine beta_beta_set_to(self,X,factor)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Set the beta-beta sector of the matrix to "X"
       real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: X
      real(kind=kind(1.0d0)), optional, intent(in) :: factor
       integer(kind=kind(1)) :: n

   call ensure_(tonto,is_square_(self),"REALMAT:beta_beta_set_to ... non-square matrix")
   call ensure_(tonto,is_even_(size(self,1)),"REALMAT:beta_beta_set_to ... uneven dimension")
      n = size(self,1)/2
      if (present(factor)) then; self(n+1:2*n,n+1:2*n) = factor*X
      else;                      self(n+1:2*n,n+1:2*n) = X
      end if

   end subroutine

!  ***************
!  Put_to routines
!  ***************

   subroutine alpha_alpha_put_to(self,X,factor)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Put the alpha-alpha sector of the matrix to "X"
       real(kind=kind(1.0d0)), dimension(:,:), intent(out) :: X
      real(kind=kind(1.0d0)), optional, intent(in) :: factor
       integer(kind=kind(1)) :: n

   call ensure_(tonto,is_square_(self),"REALMAT:alpha_alpha_put_to ... non-square matrix")
   call ensure_(tonto,is_even_(size(self,1)),"REALMAT:alpha_alpha_put_to ... uneven dimension")
      n = size(self,1)/2
      if (present(factor)) then; X = factor*self(1:n,1:n)
      else;                      X = self(1:n,1:n)
      end if

   end subroutine

   subroutine beta_alpha_put_to(self,X,factor)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Put the beta-alpha sector of the matrix to "X"
       real(kind=kind(1.0d0)), dimension(:,:), intent(out) :: X
      real(kind=kind(1.0d0)), optional, intent(in) :: factor
       integer(kind=kind(1)) :: n

   call ensure_(tonto,is_square_(self),"REALMAT:beta_alpha_put_to ... non-square matrix")
   call ensure_(tonto,is_even_(size(self,1)),"REALMAT:beta_alpha_put_to ... uneven dimension")
      n = size(self,1)/2
      if (present(factor)) then; X = factor*self(n+1:2*n,1:n)
      else;                      X = self(n+1:2*n,1:n)
      end if

   end subroutine

   subroutine alpha_beta_put_to(self,X,factor)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Put the alpha-beta sector of the matrix to "X"
       real(kind=kind(1.0d0)), dimension(:,:), intent(out) :: X
      real(kind=kind(1.0d0)), optional, intent(in) :: factor
       integer(kind=kind(1)) :: n

   call ensure_(tonto,is_square_(self),"REALMAT:alpha_beta_put_to ... non-square matrix")
   call ensure_(tonto,is_even_(size(self,1)),"REALMAT:alpha_beta_put_to ... uneven dimension")
      n = size(self,1)/2
      if (present(factor)) then; X = factor*self(1:n,n+1:2*n)
      else;                      X = self(1:n,n+1:2*n)
      end if

   end subroutine

   subroutine beta_beta_put_to(self,X,factor)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Put the beta-beta sector of the matrix to "X"
       real(kind=kind(1.0d0)), dimension(:,:), intent(out) :: X
      real(kind=kind(1.0d0)), optional, intent(in) :: factor
       integer(kind=kind(1)) :: n

   call ensure_(tonto,is_square_(self),"REALMAT:beta_beta_put_to ... non-square matrix")
   call ensure_(tonto,is_even_(size(self,1)),"REALMAT:beta_beta_put_to ... uneven dimension")
      n = size(self,1)/2
      if (present(factor)) then; X = factor*self(n+1:2*n,n+1:2*n)
      else;                      X = self(n+1:2*n,n+1:2*n)
      end if

   end subroutine

   subroutine convert_to(self,units)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Convert the number "self" in atomic units or generic units to a
    ! new number in "units".
      intent(inout) :: self
      character(*), intent(in) :: units
      real(kind=kind(1.0d0)) :: factor

   call ensure_(tonto,is_known_unit_(units),"REALMAT:convert_to ... unknown units, " // units)
      factor = conversion_factor_(units)
      self = self * factor

   end subroutine

   subroutine convert_from(self,units)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Convert the number "self" from "units" system to a new number
    ! in atomic units or generic units.  Returns "err" whether it was successful.
      intent(inout) :: self
      character(*), intent(in) :: units
      real(kind=kind(1.0d0)) :: factor

   call ensure_(tonto,is_known_unit_(units),"REALMAT:convert_from ... unknown units, " // units)
      factor = 1.0d0/(conversion_factor_(units))
      self = self * factor

   end subroutine

   subroutine to_transpose(self)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Self becomes its own transpose.
     intent(inout) :: self
     integer(kind=kind(1)) :: i,j,dim
     real(kind=kind(1.0d0)) :: tmp

   call ensure_(tonto,is_square_(self),"REALMAT:to_transpose ... non-square matrix")
     dim = size(self,1)
      ! do it element by element, otherwise the intrinsic routine can run out
      ! of stack space
      ! Also, only loop over the triangle, otherwise you end up where you started
      ! from.
     do i=1,dim
       do j=1,i
         tmp       = self(i,j)
         self(i,j) = self(j,i)
         self(j,i) = tmp
       end do
     end do

   end subroutine

   subroutine to_transpose_1(self,a)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Self becomes the transpose of "a"
     intent(out) :: self
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: a
     integer(kind=kind(1)) :: i,j,dim

   call ensure_(tonto,is_square_(self),"REALMAT:to_transpose_1 ... non-square matrix")
   call ensure_(tonto,is_same_shape_as_(self,a),"REALMAT:to_transpose_1 ... different shapes")
     dim = size(self,1)
      ! do it element by element, otherwise the intrinsic routine can run out
      ! of stack space
     do i=1,dim
       do j=1,dim
         self(i,j) = a(j,i)
       end do
     end do

   end subroutine

   subroutine make_corresponding_orbitals(self,left,right,theta,p)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! This algorithm from Camp and King, J. Chem Phys. Vol 75(1), pp 268-274.
    ! p is the dimenstion of the first block of the partitioned matrices.
    ! Works best if "left" and "right" matrices are nonzero.
     target :: self
     real(kind=kind(1.0d0)), dimension(:,:), target :: left,right
     real(kind=kind(1.0d0)), dimension(:), intent(out) :: theta
      integer(kind=kind(1)), intent(in) :: p
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: Vp,Vq,Wp,Wq,M,MWq,Hq,Up,Uq
     real(kind=kind(1.0d0)), dimension(:), pointer :: lambda
     integer(kind=kind(1)) :: minpq,q,n

   call ensure_(tonto,is_square_(self),"REALMAT:make_corresponding_orbitals ... non-square matrix")
   call ensure_(tonto,is_same_shape_as_(self,right),"REALMAT:make_corresponding_orbitals ... right is incompatible")
   call ensure_(tonto,is_same_shape_as_(self,left),"REALMAT:make_corresponding_orbitals ... left is incompatible")
   call ensure_(tonto,size(theta)==min(size(self,1),size(self,1)-p),"REALMAT:make_corresponding_orbitals ... theta has wrong &
&size")
     n = size(self,1)
     q = n - p
     minpq = min(p,q)
      ! I've only tested this for q>p.  Suspect p>q does not work.
     Vp => left(:p,:p)
     Vq => left(p+1:,p+1:)
     Wp => right(:p,:p)
     Wq => right(p+1:,p+1:)
     M  => self(:p,p+1:)
     Up => self(:p,:p)
     Uq => self(p+1:,p+1:)
     right(:p,p+1:)=0.0d0
     right(p+1:,:p)=0.0d0
     left(:p,p+1:)=0.0d0
     left(p+1:,:p)=0.0d0
     call zero_small_values_(self,10.0d0**(-10))

     call create_(lambda,q)                        ! get eigenvalues and Wq.
     call create_(Hq,q,q)
     call to_product_of_(Hq,M,M,transpose_a=.true.)
     call solve_eigenproblem_(Hq,lambda,Wq)
     call destroy_(Hq)

     call reverse_order_(lambda)                    ! get rotation angles, largest first.
     theta = lambda(:minpq)
     call destroy_(lambda)
     call zero_small_values_(theta,10.0d0**(-10))

   call ensure_(tonto,minval(theta)>=0.0d0,"REALMAT:make_corresponding_orbitals ... eigenvalues less than zero!")
   call ensure_(tonto,maxval(theta)<=1.0d0,"REALMAT:make_corresponding_orbitals ... eigenvalues more than one!")
     theta = min(theta,1.0d0)
     theta = max(theta,0.0d0)

     call zero_small_values_(Wq,10.0d0**(-10))           ! get Vp
     call reverse_column_order_(Wq)
     call create_(MWq,p,q)
     call to_product_of_(MWq,M,Wq)
     Vp = MWq(:p,:p)
     call destroy_(MWq)
     call schmidt_orthonormalise_(Vp,theta)

     call to_product_of_(Vq,Uq,Wq)                    ! get Vq
     call reverse_schmidt_orthogonalise_(Vq)

     call to_product_of_(Wp,Up,Vp,transpose_a=.true.)   ! get Wp
     call reverse_schmidt_orthogonalise_(Wp)

     theta = sqrt(theta)
     theta = asin(theta)

   end subroutine

   subroutine schmidt_orthonormalise_2(self,lambda)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Schmidt orthonormalise the column vectors in "self".
    ! If the eigenvalue (lambda) of a vector is less than a cutoff, then that
    ! vector is chosen to be an orthonormal component.
    ! Eigenvalues must be sorted largest to smallest.
     target :: self
     real(kind=kind(1.0d0)), dimension(:), intent(in) :: lambda
     real(kind=kind(1.0d0)), dimension(:), pointer :: new,old
     real(kind=kind(1.0d0)) :: fac,norm
     integer(kind=kind(1)) :: dim1,dim2,n,k,x,y,j

   call ensure_(tonto,size(lambda)>=size(self,2),"REALMAT:schmidt_orthonormalise_2 ... not enough eigenvalues")
   call ensure_(tonto,size(self,1)>=size(self,2),"REALMAT:schmidt_orthonormalise_2 ... more vectors than dimension of vector &
&space")
     dim1 = size(self,1)
     dim2 = size(self,2)

     y=dim2+1    ! y is set to the first vanishing eigenvalue.
     do x=1,dim2
       if (lambda(x)<10.0d0**(-10)) then
         y=x
         exit
       end if
     end do

     do n = 1,y-1   ! the usual Schmidt orthogonalisation.
        new => self(:,n)
        do k = 1,n-1
           old => self(:,k)
           fac = dot_product(old,new)
           new = new - fac*old
        end do
        norm = dot_product(new,new)
        call ensure_(tonto,norm>10.0d0**(-10),"REALMAT:schmidt_orthonormalise_2 ... linear dependence in vector " // to_str_(&
&n))
        new = new * (1.0d0/sqrt(norm))
     end do

     do n = y,dim2                        ! make up some orthogonal vectors for
       do j=1,dim1                        ! the vanishing eigenvalues.
         new => self(:,n)
         new = 0.0d0
         new(j) = 1.0d0
         do k = 1,n-1
            old => self(:,k)
            fac = dot_product(old,new)
            new = new - fac*old
         end do
         norm = dot_product(new,new)
         if (norm>10.0d0**(-10)) then   ! we have found an orthogonal vector
           new = new * (1.0d0/sqrt(norm))
           exit
         else                    ! try another
           call die_if_(tonto,j==dim1,"REALMAT:schmidt_orthonormalise_2 ... cannot find an orthogonal vector")
         end if
       end do
     end do

   end subroutine

!  **************************************************
!  Gaussian function rotation representation matrices
!  *************************************************

   function gaussian_d_xyz_matrix(self) result(dtr)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Return the representation matrix for a d xyz product found in gaussian shells
    ! from a p-type xyz matrix.
      real(kind=kind(1.0d0)), dimension(6,6) :: dtr
      integer(kind=kind(1)) :: j,i1,i2
      real(kind=kind(1.0d0)) :: sqrt3
      integer(kind=kind(1)), dimension(6) :: d1  = (/1,2,3,1,1,2/)
      integer(kind=kind(1)), dimension(6) :: d2  = (/1,2,3,2,3,3/)

   call ensure_(tonto,is_square_(self),"REALMAT:gaussian_d_xyz_matrix ... self not square")
   call ensure_(tonto,size(self,1)==3,"REALMAT:gaussian_d_xyz_matrix ... wrong size, self")
      sqrt3  = sqrt(3.0d0)
      do j = 1,6
         i1=d1(j)
         i2=d2(j)
         dtr(1,j)  = self(1,i1)*self(1,i2)
         dtr(2,j)  = self(2,i1)*self(2,i2)
         dtr(3,j)  = self(3,i1)*self(3,i2)
         dtr(4,j)  = self(1,i1)*self(2,i2) &
                   + self(2,i1)*self(1,i2)
         dtr(5,j)  = self(1,i1)*self(3,i2) &
                   + self(3,i1)*self(1,i2)
         dtr(6,j)  = self(2,i1)*self(3,i2) &
                   + self(3,i1)*self(2,i2)
      end do
      dtr(1:6,4:6) = dtr(1:6,4:6)*sqrt3  ! Put in correct normalization for old primitives
      dtr(4:6,1:6) = dtr(4:6,1:6)/sqrt3  ! Put in wrong   normalization for new primitives

   end function

   function gaussian_f_xyz_matrix(self) result(ftr)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Return the representation matrix for an f xyz product found in gaussian
    ! shells from a p-type xyz matrix
      real(kind=kind(1.0d0)), dimension(10,10) :: ftr
      integer(kind=kind(1)) :: j,i1,i2,i3
      real(kind=kind(1.0d0)) :: sqrt5,sqrt15
      integer(kind=kind(1)), dimension(10) :: f1 = (/1,2,3,1,1,2,2,3,3,1/)
      integer(kind=kind(1)), dimension(10) :: f2 = (/1,2,3,1,1,2,2,3,3,2/)
      integer(kind=kind(1)), dimension(10) :: f3 = (/1,2,3,2,3,1,3,1,2,3/)

   call ensure_(tonto,is_square_(self),"REALMAT:gaussian_f_xyz_matrix ... self not square")
   call ensure_(tonto,size(self,1)==3,"REALMAT:gaussian_f_xyz_matrix ... wrong size, self")
      sqrt5  = sqrt(5.0d0)
      sqrt15 = sqrt(15d0)
      do j = 1,10
         i1=f1(j)
         i2=f2(j)
         i3=f3(j)
         ftr(1,j)  = self(1,i1)*self(1,i2)*self(1,i3)
         ftr(2,j)  = self(2,i1)*self(2,i2)*self(2,i3)
         ftr(3,j)  = self(3,i1)*self(3,i2)*self(3,i3)
         ftr(4,j)  = self(1,i1)*self(1,i2)*self(2,i3) &
                   + self(1,i1)*self(2,i2)*self(1,i3) &
                   + self(2,i1)*self(1,i2)*self(1,i3)
         ftr(5,j)  = self(1,i1)*self(1,i2)*self(3,i3) &
                   + self(1,i1)*self(3,i2)*self(1,i3) &
                   + self(3,i1)*self(1,i2)*self(1,i3)
         ftr(6,j)  = self(1,i1)*self(2,i2)*self(2,i3) &
                   + self(2,i1)*self(1,i2)*self(2,i3) &
                   + self(2,i1)*self(2,i2)*self(1,i3)
         ftr(7,j)  = self(3,i1)*self(2,i2)*self(2,i3) &
                   + self(2,i1)*self(3,i2)*self(2,i3) &
                   + self(2,i1)*self(2,i2)*self(3,i3)
         ftr(8,j)  = self(1,i1)*self(3,i2)*self(3,i3) &
                   + self(3,i1)*self(1,i2)*self(3,i3) &
                   + self(3,i1)*self(3,i2)*self(1,i3)
         ftr(9,j)  = self(2,i1)*self(3,i2)*self(3,i3) &
                   + self(3,i1)*self(2,i2)*self(3,i3) &
                   + self(3,i1)*self(3,i2)*self(2,i3)
         ftr(10,j) = self(1,i1)*self(2,i2)*self(3,i3) &
                   + self(1,i1)*self(3,i2)*self(2,i3) &
                   + self(2,i1)*self(1,i2)*self(3,i3) &
                   + self(2,i1)*self(3,i2)*self(1,i3) &
                   + self(3,i1)*self(1,i2)*self(2,i3) &
                   + self(3,i1)*self(2,i2)*self(1,i3)
      end do
      ftr(1:10, 4:9 ) = ftr(1:10, 4:9 )*sqrt5
      ftr(1:10,10:10) = ftr(1:10,10:10)*sqrt15
      ftr(4:9 , 1:10) = ftr(4:9 , 1:10)/sqrt5
      ftr(10:10,1:10) = ftr(10:10,1:10)/sqrt15

   end function

   function gaussian_g_xyz_matrix(self) result(gtr)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Return the representation matrix for a g xyz product found in gaussian
    ! shells from a p-type xyz matrix
      real(kind=kind(1.0d0)), dimension(15,15) :: gtr
      integer(kind=kind(1)) :: j,i1,i2,i3,i4
      real(kind=kind(1.0d0)) :: sqrt7,sqrt35,sqrt353
      integer(kind=kind(1)), dimension(15) :: g1 = (/1,2,3,1,1,2,2,3,3,1,1,2,1,2,3/)
      integer(kind=kind(1)), dimension(15) :: g2 = (/1,2,3,1,1,2,2,3,3,1,1,2,1,2,3/)
      integer(kind=kind(1)), dimension(15) :: g3 = (/1,2,3,1,1,2,2,3,3,2,3,3,2,1,1/)
      integer(kind=kind(1)), dimension(15) :: g4 = (/1,2,3,2,3,1,3,1,2,2,3,3,3,3,2/)

   call ensure_(tonto,is_square_(self),"REALMAT:gaussian_g_xyz_matrix ... self not square")
   call ensure_(tonto,size(self,1)==3,"REALMAT:gaussian_g_xyz_matrix ... wrong size, self")
      sqrt7  = sqrt(7.0d0)
      sqrt35 = sqrt(35d0)          ! = sqrt(35)
      sqrt353= sqrt35/sqrt(3.0d0)  ! = sqrt(35)/sqrt(3)
      do j = 1,15
         i1=g1(j)
         i2=g2(j)
         i3=g3(j)
         i4=g4(j)
         gtr(1,j)  = self(1,i1)*self(1,i2)*self(1,i3)*self(1,i4)
         gtr(2,j)  = self(2,i1)*self(2,i2)*self(2,i3)*self(2,i4)
         gtr(3,j)  = self(3,i1)*self(3,i2)*self(3,i3)*self(3,i4)
         gtr(4,j)  = self(1,i1)*self(1,i2)*self(1,i3)*self(2,i4) &
                   + self(1,i1)*self(1,i2)*self(2,i3)*self(1,i4) &
                   + self(1,i1)*self(2,i2)*self(1,i3)*self(1,i4) &
                   + self(2,i1)*self(1,i2)*self(1,i3)*self(1,i4)
         gtr(5,j)  = self(1,i1)*self(1,i2)*self(1,i3)*self(3,i4) &
                   + self(1,i1)*self(1,i2)*self(3,i3)*self(1,i4) &
                   + self(1,i1)*self(3,i2)*self(1,i3)*self(1,i4) &
                   + self(3,i1)*self(1,i2)*self(1,i3)*self(1,i4)
         gtr(6,j)  = self(1,i1)*self(2,i2)*self(2,i3)*self(2,i4) &
                   + self(2,i1)*self(1,i2)*self(2,i3)*self(2,i4) &
                   + self(2,i1)*self(2,i2)*self(1,i3)*self(2,i4) &
                   + self(2,i1)*self(2,i2)*self(2,i3)*self(1,i4)
         gtr(7,j)  = self(3,i1)*self(2,i2)*self(2,i3)*self(2,i4) &
                   + self(2,i1)*self(3,i2)*self(2,i3)*self(2,i4) &
                   + self(2,i1)*self(2,i2)*self(3,i3)*self(2,i4) &
                   + self(2,i1)*self(2,i2)*self(2,i3)*self(3,i4)
         gtr(8,j)  = self(1,i1)*self(3,i2)*self(3,i3)*self(3,i4) &
                   + self(3,i1)*self(1,i2)*self(3,i3)*self(3,i4) &
                   + self(3,i1)*self(3,i2)*self(1,i3)*self(3,i4) &
                   + self(3,i1)*self(3,i2)*self(3,i3)*self(1,i4)
         gtr(9,j)  = self(2,i1)*self(3,i2)*self(3,i3)*self(3,i4) &
                   + self(3,i1)*self(2,i2)*self(3,i3)*self(3,i4) &
                   + self(3,i1)*self(3,i2)*self(2,i3)*self(3,i4) &
                   + self(3,i1)*self(3,i2)*self(3,i3)*self(2,i4)
         gtr(10,j) = self(1,i1)*self(1,i2)*self(2,i3)*self(2,i4) &
                   + self(1,i1)*self(2,i2)*self(1,i3)*self(2,i4) &
                   + self(1,i1)*self(2,i2)*self(2,i3)*self(1,i4) &
                   + self(2,i1)*self(1,i2)*self(1,i3)*self(2,i4) &
                   + self(2,i1)*self(1,i2)*self(2,i3)*self(1,i4) &
                   + self(2,i1)*self(2,i2)*self(1,i3)*self(1,i4)
         gtr(11,j) = self(1,i1)*self(1,i2)*self(3,i3)*self(3,i4) &
                   + self(1,i1)*self(3,i2)*self(1,i3)*self(3,i4) &
                   + self(1,i1)*self(3,i2)*self(3,i3)*self(1,i4) &
                   + self(3,i1)*self(1,i2)*self(1,i3)*self(3,i4) &
                   + self(3,i1)*self(1,i2)*self(3,i3)*self(1,i4) &
                   + self(3,i1)*self(3,i2)*self(1,i3)*self(1,i4)
         gtr(12,j) = self(2,i1)*self(2,i2)*self(3,i3)*self(3,i4) &
                   + self(2,i1)*self(3,i2)*self(2,i3)*self(3,i4) &
                   + self(2,i1)*self(3,i2)*self(3,i3)*self(2,i4) &
                   + self(3,i1)*self(2,i2)*self(2,i3)*self(3,i4) &
                   + self(3,i1)*self(2,i2)*self(3,i3)*self(2,i4) &
                   + self(3,i1)*self(3,i2)*self(2,i3)*self(2,i4)
         gtr(13,j) = self(1,i1)*self(1,i2)*self(2,i3)*self(3,i4) &
                   + self(1,i1)*self(1,i2)*self(3,i3)*self(2,i4) &
                   + self(1,i1)*self(2,i2)*self(1,i3)*self(3,i4) &
                   + self(1,i1)*self(2,i2)*self(3,i3)*self(1,i4) &
                   + self(1,i1)*self(3,i2)*self(1,i3)*self(2,i4) &
                   + self(1,i1)*self(3,i2)*self(2,i3)*self(1,i4) &
                   + self(2,i1)*self(1,i2)*self(1,i3)*self(3,i4) &
                   + self(2,i1)*self(1,i2)*self(3,i3)*self(1,i4) &
                   + self(2,i1)*self(3,i2)*self(1,i3)*self(1,i4) &
                   + self(3,i1)*self(1,i2)*self(1,i3)*self(2,i4) &
                   + self(3,i1)*self(1,i2)*self(2,i3)*self(1,i4) &
                   + self(3,i1)*self(2,i2)*self(1,i3)*self(1,i4)
         gtr(14,j) = self(2,i1)*self(2,i2)*self(1,i3)*self(3,i4) &
                   + self(2,i1)*self(2,i2)*self(3,i3)*self(1,i4) &
                   + self(2,i1)*self(1,i2)*self(2,i3)*self(3,i4) &
                   + self(2,i1)*self(1,i2)*self(3,i3)*self(2,i4) &
                   + self(2,i1)*self(3,i2)*self(2,i3)*self(1,i4) &
                   + self(2,i1)*self(3,i2)*self(1,i3)*self(2,i4) &
                   + self(1,i1)*self(2,i2)*self(2,i3)*self(3,i4) &
                   + self(1,i1)*self(2,i2)*self(3,i3)*self(2,i4) &
                   + self(1,i1)*self(3,i2)*self(2,i3)*self(2,i4) &
                   + self(3,i1)*self(2,i2)*self(2,i3)*self(1,i4) &
                   + self(3,i1)*self(2,i2)*self(1,i3)*self(2,i4) &
                   + self(3,i1)*self(1,i2)*self(2,i3)*self(2,i4)
         gtr(15,j) = self(3,i1)*self(3,i2)*self(1,i3)*self(2,i4) &
                   + self(3,i1)*self(3,i2)*self(2,i3)*self(1,i4) &
                   + self(3,i1)*self(1,i2)*self(3,i3)*self(2,i4) &
                   + self(3,i1)*self(1,i2)*self(2,i3)*self(3,i4) &
                   + self(3,i1)*self(2,i2)*self(3,i3)*self(1,i4) &
                   + self(3,i1)*self(2,i2)*self(1,i3)*self(3,i4) &
                   + self(1,i1)*self(3,i2)*self(3,i3)*self(2,i4) &
                   + self(1,i1)*self(3,i2)*self(2,i3)*self(3,i4) &
                   + self(1,i1)*self(2,i2)*self(3,i3)*self(3,i4) &
                   + self(2,i1)*self(3,i2)*self(3,i3)*self(1,i4) &
                   + self(2,i1)*self(3,i2)*self(1,i3)*self(3,i4) &
                   + self(2,i1)*self(1,i2)*self(3,i3)*self(3,i4)
      end do
      gtr(1:15, 4:9 ) = gtr(1:15, 4:9 )*sqrt7
      gtr(1:15,10:12) = gtr(1:15,10:12)*sqrt353
      gtr(1:15,13:15) = gtr(1:15,13:15)*sqrt35
      gtr(4:9 , 1:15) = gtr(4:9 , 1:15)/sqrt7
      gtr(10:12,1:15) = gtr(10:12,1:15)/sqrt353
      gtr(13:15,1:15) = gtr(13:15,1:15)/sqrt35

   end function

   pure subroutine make_enclosing_sphere(self,pos,radius)
    real(kind=kind(1.0d0)), dimension(:,:) :: self
    ! Determine the position and radius of a sphere that encloses all points in
    ! the grid.
     intent(in) :: self
     real(kind=kind(1.0d0)), intent(out) :: radius
     real(kind=kind(1.0d0)), dimension(3), intent(out) :: pos
     real(kind=kind(1.0d0)), dimension(3) :: diff
     real(kind=kind(1.0d0)) :: dist
     integer(kind=kind(1)) :: n,n_pts

     n_pts = size(self,2)

      ! Get the centre of the sphere.  Should use a better algorithm than just the
      ! average.
     pos = 0.0d0
     do n = 1,n_pts
       pos = pos + self(:,n)
     end do
     pos = pos / n_pts

      ! The radius is the distance to the furthest point.
     radius = 0
     do n = 1,n_pts
       diff = self(:,n) - pos
       dist = dot_product(diff,diff)
       if (dist > radius) radius = dist
     end do
     radius = sqrt(radius)

   end subroutine

end
