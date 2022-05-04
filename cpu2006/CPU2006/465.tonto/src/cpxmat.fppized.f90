!---------------------------------------------------------------------------
!
!  complex matrix operations :: CPXMAT ...
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
! $Id: cpxmat.foo,v 1.17.2.2 2003/11/13 05:34:39 reaper Exp $
!---------------------------------------------------------------------------

module CPXMAT_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

   use REALVEC_MODULE, only: create_
   use REALVEC_MODULE, only: destroy_

   use INT_MODULE, only: is_even_
   use INT_MODULE, only: to_str_

   use REALMAT_MODULE, only: dot_
   use REALMAT_MODULE, only: is_square_

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

   public    solve_eigenproblem_
   interface solve_eigenproblem_
      module procedure solve_eigenproblem
   end interface

   public    beta_beta_put_to_
   interface beta_beta_put_to_
      module procedure beta_beta_put_to
      module procedure beta_beta_put_to_1
      module procedure beta_beta_put_to_2
      module procedure beta_beta_put_to_3
   end interface

   public    compress_to_triangle_
   interface compress_to_triangle_
      module procedure compress_to_triangle
   end interface

   public    beta_beta_plus_
   interface beta_beta_plus_
      module procedure beta_beta_plus
      module procedure beta_beta_plus_1
      module procedure beta_beta_plus_2
      module procedure beta_beta_plus_3
   end interface

   public    beta_beta_
   interface beta_beta_
      module procedure beta_beta
   end interface

   public    set_to_
   interface set_to_
      module procedure set_to
   end interface

   public    plus_scaled_product_of_
   interface plus_scaled_product_of_
      module procedure plus_scaled_product_of
      module procedure plus_scaled_product_of_1
      module procedure plus_scaled_product_of_2
   end interface

   public    antihermitian_reflect_
   interface antihermitian_reflect_
      module procedure antihermitian_reflect
   end interface

   public    antihermitian_fold_
   interface antihermitian_fold_
      module procedure antihermitian_fold
   end interface

   public    is_square_
   interface is_square_
      module procedure is_square
   end interface

   public    same_as_
   interface same_as_
      module procedure same_as
   end interface

   public    plus_product_of_
   interface plus_product_of_
      module procedure plus_product_of
      module procedure plus_product_of_1
      module procedure plus_product_of_2
   end interface

   public    beta_alpha_plus_
   interface beta_alpha_plus_
      module procedure beta_alpha_plus
      module procedure beta_alpha_plus_1
      module procedure beta_alpha_plus_2
      module procedure beta_alpha_plus_3
   end interface

   public    tri_size_
   interface tri_size_
      module procedure tri_size
   end interface

   public    change_basis_
   interface change_basis_
      module procedure change_basis
      module procedure change_basis_1
      module procedure change_basis_2
      module procedure change_basis_3
   end interface

   public    is_symmetric_
   interface is_symmetric_
      module procedure is_symmetric
   end interface

   public    weight_diagonal_
   interface weight_diagonal_
      module procedure weight_diagonal
      module procedure weight_diagonal_1
   end interface

   public    to_product_of_
   interface to_product_of_
      module procedure to_product_of
      module procedure to_product_of_1
      module procedure to_product_of_2
   end interface

   public    alpha_alpha_
   interface alpha_alpha_
      module procedure alpha_alpha
   end interface

   public    is_zero_
   interface is_zero_
      module procedure is_zero
   end interface

   public    alpha_beta_
   interface alpha_beta_
      module procedure alpha_beta
   end interface

   public    hermitian_reflect_
   interface hermitian_reflect_
      module procedure hermitian_reflect
   end interface

   public    plus_
   interface plus_
      module procedure plus
   end interface

   public    swap_columns_
   interface swap_columns_
      module procedure swap_columns
   end interface

   public    is_antisymmetric_
   interface is_antisymmetric_
      module procedure is_antisymmetric
   end interface

   public    alpha_alpha_plus_
   interface alpha_alpha_plus_
      module procedure alpha_alpha_plus
      module procedure alpha_alpha_plus_1
      module procedure alpha_alpha_plus_2
      module procedure alpha_alpha_plus_3
   end interface

   private    solve_eigenproblem_LAPACK_
   interface solve_eigenproblem_LAPACK_
      module procedure solve_eigenproblem_LAPACK
   end interface

   public    alpha_beta_set_to_
   interface alpha_beta_set_to_
      module procedure alpha_beta_set_to
      module procedure alpha_beta_set_to_1
      module procedure alpha_beta_set_to_2
      module procedure alpha_beta_set_to_3
   end interface

   public    to_inverse_sqrt_
   interface to_inverse_sqrt_
      module procedure to_inverse_sqrt
   end interface

   public    beta_alpha_
   interface beta_alpha_
      module procedure beta_alpha
   end interface

   public    to_product_with_diagonal_
   interface to_product_with_diagonal_
      module procedure to_product_with_diagonal
      module procedure to_product_with_diagonal_1
   end interface

   public    alpha_alpha_set_to_
   interface alpha_alpha_set_to_
      module procedure alpha_alpha_set_to
      module procedure alpha_alpha_set_to_1
      module procedure alpha_alpha_set_to_2
      module procedure alpha_alpha_set_to_3
   end interface

   public    beta_alpha_put_to_
   interface beta_alpha_put_to_
      module procedure beta_alpha_put_to
      module procedure beta_alpha_put_to_1
      module procedure beta_alpha_put_to_2
      module procedure beta_alpha_put_to_3
   end interface

   public    minus_
   interface minus_
      module procedure minus
   end interface

   public    trace_of_product_
   interface trace_of_product_
      module procedure trace_of_product
      module procedure trace_of_product_1
   end interface

   public    is_hermitian_
   interface is_hermitian_
      module procedure is_hermitian
   end interface

   public    alpha_beta_plus_
   interface alpha_beta_plus_
      module procedure alpha_beta_plus
      module procedure alpha_beta_plus_1
      module procedure alpha_beta_plus_2
      module procedure alpha_beta_plus_3
   end interface

   public    to_scaled_product_of_
   interface to_scaled_product_of_
      module procedure to_scaled_product_of
      module procedure to_scaled_product_of_1
      module procedure to_scaled_product_of_2
   end interface

   public    create_
   interface create_
      module procedure create
      module procedure create_1
   end interface

   public    is_antihermitian_
   interface is_antihermitian_
      module procedure is_antihermitian
   end interface

   public    beta_beta_set_to_
   interface beta_beta_set_to_
      module procedure beta_beta_set_to
      module procedure beta_beta_set_to_1
      module procedure beta_beta_set_to_2
      module procedure beta_beta_set_to_3
   end interface

   public    symmetrically_orthonormalise_
   interface symmetrically_orthonormalise_
      module procedure symmetrically_orthonormalise
   end interface

   public    make_hermitian_
   interface make_hermitian_
      module procedure make_hermitian
   end interface

   public    compress_to_square_
   interface compress_to_square_
      module procedure compress_to_square
   end interface

   public    dot_
   interface dot_
      module procedure dot
   end interface

   public    equals_
   interface equals_
      module procedure equals
   end interface

   public    from_diagonal_
   interface from_diagonal_
      module procedure from_diagonal
   end interface

   public    trace_
   interface trace_
      module procedure trace
   end interface

   public    to_unit_mat_
   interface to_unit_mat_
      module procedure to_unit_mat
   end interface

   private    solve_eigenproblem_ESSL_
   interface solve_eigenproblem_ESSL_
      module procedure solve_eigenproblem_ESSL
   end interface

   public    get_diagonal_
   interface get_diagonal_
      module procedure get_diagonal
   end interface

   public    destroy_
   interface destroy_
      module procedure destroy
   end interface

   public    to_sqrt_
   interface to_sqrt_
      module procedure to_sqrt
   end interface

   public    schmidt_orthonormalise_
   interface schmidt_orthonormalise_
      module procedure schmidt_orthonormalise
   end interface

   public    is_transposed_shape_of_
   interface is_transposed_shape_of_
      module procedure is_transposed_shape_of
      module procedure is_transposed_shape_of_1
   end interface

   public    make_antihermitian_
   interface make_antihermitian_
      module procedure make_antihermitian
   end interface

   public    alpha_beta_put_to_
   interface alpha_beta_put_to_
      module procedure alpha_beta_put_to
      module procedure alpha_beta_put_to_1
      module procedure alpha_beta_put_to_2
      module procedure alpha_beta_put_to_3
   end interface

   public    hermitian_fold_
   interface hermitian_fold_
      module procedure hermitian_fold
   end interface

   public    plus_scaled_mat_
   interface plus_scaled_mat_
      module procedure plus_scaled_mat
      module procedure plus_scaled_mat_1
   end interface

   public    uncompress_from_square_
   interface uncompress_from_square_
      module procedure uncompress_from_square
   end interface

   public    to_scaled_mat_
   interface to_scaled_mat_
      module procedure to_scaled_mat
      module procedure to_scaled_mat_1
   end interface

   public    alpha_alpha_put_to_
   interface alpha_alpha_put_to_
      module procedure alpha_alpha_put_to
      module procedure alpha_alpha_put_to_1
      module procedure alpha_alpha_put_to_2
      module procedure alpha_alpha_put_to_3
   end interface

   public    uncompress_from_triangle_
   interface uncompress_from_triangle_
      module procedure uncompress_from_triangle
   end interface

   public    is_same_shape_as_
   interface is_same_shape_as_
      module procedure is_same_shape_as
      module procedure is_same_shape_as_1
   end interface

   public    beta_alpha_set_to_
   interface beta_alpha_set_to_
      module procedure beta_alpha_set_to
      module procedure beta_alpha_set_to_1
      module procedure beta_alpha_set_to_2
      module procedure beta_alpha_set_to_3
   end interface

   public    trace_of_product_with_
   interface trace_of_product_with_
      module procedure trace_of_product_with
      module procedure trace_of_product_with_1
   end interface

   public    back_transform_
   interface back_transform_
      module procedure back_transform
   end interface

   real(kind=kind(1.0d0)), private :: tol5 = 1.0d-5
!   tol10 :: real(kind=kind(1.0d0)), private = 1.0d-10

contains

   subroutine create(self,dim1,dim2)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Create a matrix with the given dimensions
      pointer :: self
      integer(kind=kind(1)), intent(in) :: dim1,dim2
    ! The following code is inherited from INTRINSICMAT

      nullify(self)
      allocate(self(dim1,dim2))

   end subroutine

   subroutine create_1(self,lb1,ub1,lb2,ub2)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Create a matrix with the given dimensions
      pointer :: self
      integer(kind=kind(1)), intent(in) :: lb1,ub1,lb2,ub2
    ! The following code is inherited from INTRINSICMAT

      nullify(self)
      allocate(self(lb1:ub1,lb2:ub2))

   end subroutine

   subroutine destroy(self)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
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
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Returns .true. if the matrix is square
      intent(in) :: self
      logical(kind=kind(.true.)) :: res
    ! The following code is inherited from INTRINSICMAT
      res = size(self,1)==size(self,2)

   end function

   function is_symmetric(self,tol) result(res)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Returns .true. if the matrix is symmetric. You can set the tolerance "tol"
    ! for comparison.
      logical(kind=kind(.true.)) :: res
      real(kind=kind(1.0d0)), optional :: tol

      res = is_square_(self) .and. same_as_(self,transpose(self),tol)

   end function

   function is_antisymmetric(self,tol) result(res)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Returns .true. if the matrix is symmetric. You can set the tolerance "tol"
    ! for comparison.
      logical(kind=kind(.true.)) :: res
      real(kind=kind(1.0d0)), optional :: tol

      res = is_square_(self) .and. same_as_(self,-transpose(self),tol)

   end function

   function is_hermitian(self,tol) result(res)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Returns .true. if the matrix is hermitian. You can set the tolerance "tol"
    ! for comparison.
      intent(in) :: self
      logical(kind=kind(.true.)) :: res
      real(kind=kind(1.0d0)), optional :: tol

      res = is_square_(self) .and. same_as_(self,transpose(conjg(self)),tol)

   end function

   function is_antihermitian(self,tol) result(res)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Returns .true. if the matrix is antihermitian. You can set the tolerance
    ! "tol" for comparison.
      intent(in) :: self
      logical(kind=kind(.true.)) :: res
      real(kind=kind(1.0d0)), optional :: tol

      res = is_square_(self) .and. same_as_(self,-transpose(conjg(self)),tol)

   end function

   pure function is_same_shape_as(self,b) result(res)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Returns .true. if the matrix "b" has the same shape as self
      intent(in) :: self
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), intent(in) :: b
      logical(kind=kind(.true.)) :: res
    ! The following code is inherited from INTRINSICMAT
      res = size(self,1)==size(b,1) .and. size(self,2)==size(b,2)

   end function

   pure function is_same_shape_as_1(self,b) result(res)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Returns .true. if the matrix "b" has the same shape as self
      intent(in) :: self
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: b
      logical(kind=kind(.true.)) :: res
    ! The following code is inherited from INTRINSICMAT
      res = size(self,1)==size(b,1) .and. size(self,2)==size(b,2)

   end function

   pure function is_transposed_shape_of(self,b) result(res)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Returns .true. if the matrix "b" is the transposed shape of self
      intent(in) :: self
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), intent(in) :: b
      logical(kind=kind(.true.)) :: res
    ! The following code is inherited from INTRINSICMAT
      res = size(self,1)==size(b,2) .and. size(self,2)==size(b,1)

   end function

   pure function is_transposed_shape_of_1(self,b) result(res)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Returns .true. if the matrix "b" is the transposed shape of self
      intent(in) :: self
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: b
      logical(kind=kind(.true.)) :: res
    ! The following code is inherited from INTRINSICMAT
      res = size(self,1)==size(b,2) .and. size(self,2)==size(b,1)

   end function

   function is_zero(self,eps) result(res)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Return .true. is "self" is the zero matrix, i.e. every element is zero.
    ! If present, "eps" is used to decide when a small number is zero.
      real(kind=kind(1.0d0)), optional, intent(in) :: eps
      logical(kind=kind(.true.)) :: res
      integer(kind=kind(1)) :: dim1,dim2,i,j
      real(kind=kind(1.0d0)) :: re,im

      dim1 = size(self,1)
      dim2 = size(self,2)
      res = .true.
      do i = 1,dim1
      do j = 1,dim2
         re = real(self(i,j))
         im = aimag(self(i,j))
         if (is_zero_(re,eps) .and. is_zero_(im,eps)) cycle
         res = .false.
         exit
      end do
      end do

   end function

   function dot(self,l,r) result(res)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Multiply the matrix self by the complex conjugate of vector "l" on the left
    ! and vector "r" on the right ie:  res = l^\dagger self r. Useful for
    ! non-unit metric dot_products.
     complex(kind=kind((1.0d0,1.0d0))), dimension(:), intent(in) :: l,r
     complex(kind=kind((1.0d0,1.0d0))) :: res
     complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: w

     call ensure_(tonto,size(self,1)==size(l),"CPXMAT:dot ... wrong size, r")
     call ensure_(tonto,size(self,2)==size(r),"CPXMAT:dot ... wrong size, r")
     call create_(w,size(l))
     call to_product_of_(w,self,r)
     res = dot_product(l,w)
     call destroy_(w)

   end function

   subroutine to_product_of(self,a,b,dagger_a,dagger_b)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Set "self" to the matrix product of "a" and "b". If present,
    ! "dagger_a" and "dagger_b" can be set to .true. if "a" and "b"
    ! needs to be daggerred.
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: a, b
      logical(kind=kind(.true.)), optional :: dagger_a, dagger_b
      logical(kind=kind(.true.)) :: dagg_a,dagg_b
      integer(kind=kind(1)) :: opt
      integer(kind=kind(1)) :: dima,dim1,dim2,i,j,k
      complex(kind=kind((1.0d0,1.0d0))) :: temp

      dagg_a = .false.;       dagg_b = .false.
      if (present(dagger_a)) dagg_a = dagger_a
      if (present(dagger_b)) dagg_b = dagger_b
      opt = 0
      if (dagg_a) opt = opt + 1
      if (dagg_b) opt = opt + 2
      select case (opt)
        case (0)  ! .to_product_an_bn
          call ensure_(tonto,size(self,1)==size(a,1),"CPXMAT:to_product_of ... incompatible shapes")
          call ensure_(tonto,size(self,2)==size(b,2),"CPXMAT:to_product_of ... incompatible shapes")
          call ensure_(tonto,size(a,2)==size(b,1),"CPXMAT:to_product_of ... incompatible shapes")
          dima = size(a,2)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (0.0d0,0.0d0)
            do k=1,dima
              temp = temp + a(i,k) * b(k,j)
            end do
            self(i,j) = temp
          end do
          end do
        case (1)  ! .to_product_ad_bn
          call ensure_(tonto,size(self,1)==size(a,2),"CPXMAT:to_product_of ... incompatible shapes")
          call ensure_(tonto,size(self,2)==size(b,2),"CPXMAT:to_product_of ... incompatible shapes")
          call ensure_(tonto,size(a,1)==size(b,1),"CPXMAT:to_product_of ... incompatible shapes")
          dima = size(a,1)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (0.0d0,0.0d0)
            do k=1,dima
              temp = temp + conjg(a(k,i)) * b(k,j)
            end do
            self(i,j) = temp
          end do
          end do
        case (2)  ! .to_product_an_bd
          call ensure_(tonto,size(self,1)==size(a,1),"CPXMAT:to_product_of ... incompatible shapes")
          call ensure_(tonto,size(self,2)==size(b,1),"CPXMAT:to_product_of ... incompatible shapes")
          call ensure_(tonto,size(a,2)==size(b,2),"CPXMAT:to_product_of ... incompatible shapes")
          dima = size(a,2)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (0.0d0,0.0d0)
            do k=1,dima
              temp = temp + a(i,k) * conjg(b(j,k))
            end do
            self(i,j) = temp
          end do
          end do
        case (3)  ! .to_product_ad_bd
          call ensure_(tonto,size(self,1)==size(a,2),"CPXMAT:to_product_of ... incompatible shapes")
          call ensure_(tonto,size(self,2)==size(b,1),"CPXMAT:to_product_of ... incompatible shapes")
          call ensure_(tonto,size(a,1)==size(b,2),"CPXMAT:to_product_of ... incompatible shapes")
          dima = size(a,1)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (0.0d0,0.0d0)
            do k=1,dima
              temp = temp + conjg(a(k,i)) * conjg(b(j,k))
            end do
            self(i,j) = temp
          end do
          end do
      end select

   end subroutine

   subroutine to_product_of_1(self,a,b,dagger_a,transpose_b)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Set "self" to the product of complex matrix "a" and real matrix "b".
    ! If present, "dagger_a" and "transpose_b" can be set to .true.
    ! if "a" and "b" needs to be daggerred or transposed.
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: a
      real(kind=kind(1.0d0)), dimension(:,:) :: b
      logical(kind=kind(.true.)), optional :: dagger_a, transpose_b
      logical(kind=kind(.true.)) :: dagg_a,trans_b
      integer(kind=kind(1)) :: opt
      integer(kind=kind(1)) :: dima,dim1,dim2,i,j,k
      complex(kind=kind((1.0d0,1.0d0))) :: temp

      dagg_a = .false.;           trans_b = .false.
      if (present(dagger_a))    dagg_a  = dagger_a
      if (present(transpose_b)) trans_b = transpose_b
      opt = 0
      if (dagg_a)  opt = opt + 1
      if (trans_b) opt = opt + 2
      select case (opt)
        case(0)  ! .to_product_an_bn
          call ensure_(tonto,size(self,1)==size(a,1),"CPXMAT:to_product_of_1 ... incompatible shapes")
          call ensure_(tonto,size(self,2)==size(b,2),"CPXMAT:to_product_of_1 ... incompatible shapes")
          call ensure_(tonto,size(a,2)==size(b,1),"CPXMAT:to_product_of_1 ... incompatible shapes")
          dima = size(a,2)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (0.0d0,0.0d0)
            do k=1,dima
              self(i,j) = self(i,j) + a(i,k) * b(k,j)
            end do
            self(i,j) = temp
          end do
          end do
        case(1)  ! .to_product_ad_bn
          call ensure_(tonto,size(self,1)==size(a,2),"CPXMAT:to_product_of_1 ... incompatible shapes")
          call ensure_(tonto,size(self,2)==size(b,2),"CPXMAT:to_product_of_1 ... incompatible shapes")
          call ensure_(tonto,size(a,1)==size(b,1),"CPXMAT:to_product_of_1 ... incompatible shapes")
          dima = size(a,1)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (0.0d0,0.0d0)
            do k=1,dima
              self(i,j) = self(i,j) + conjg(a(k,i)) * b(k,j)
            end do
            self(i,j) = temp
          end do
          end do
        case(2)  ! .to_product_an_bt
          call ensure_(tonto,size(self,2)==size(b,1),"CPXMAT:to_product_of_1 ... incompatible shapes")
          call ensure_(tonto,size(a,2)==size(b,2),"CPXMAT:to_product_of_1 ... incompatible shapes")
          dima = size(a,2)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (0.0d0,0.0d0)
            do k=1,dima
              self(i,j) = self(i,j) + a(i,k) * b(j,k)
            end do
            self(i,j) = temp
          end do
          end do
        case(3)  ! .to_product_ad_bt
          call ensure_(tonto,size(self,1)==size(a,2),"CPXMAT:to_product_of_1 ... incompatible shapes")
          call ensure_(tonto,size(self,2)==size(b,1),"CPXMAT:to_product_of_1 ... incompatible shapes")
          call ensure_(tonto,size(a,1)==size(b,2),"CPXMAT:to_product_of_1 ... incompatible shapes")
          dima = size(a,1)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (0.0d0,0.0d0)
            do k=1,dima
              self(i,j) = self(i,j) + conjg(a(k,i)) * b(j,k)
            end do
            self(i,j) = temp
          end do
          end do
      end select

   end subroutine

   subroutine to_product_of_2(self,a,b,transpose_a,dagger_b)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Set "self" to the product of real matrix "a" and complex matrix "b".
    ! If present, "transpose_a" and "dagger_b" can be set to .true.
    ! if "a" and "b" needs to be daggerred or transposed.
      real(kind=kind(1.0d0)), dimension(:,:) :: a
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: b
      logical(kind=kind(.true.)), optional :: transpose_a, dagger_b
      logical(kind=kind(.true.)) :: trans_a,dagg_b
      integer(kind=kind(1)) :: opt
      integer(kind=kind(1)) :: dima,dim1,dim2,i,j,k
      complex(kind=kind((1.0d0,1.0d0))) :: temp

      trans_a = .false.;          dagg_b  = .false.
      if (present(transpose_a)) trans_a = transpose_a
      if (present(dagger_b))    dagg_b  = dagger_b
      opt = 0
      if (trans_a) opt = opt + 1
      if (dagg_b)  opt = opt + 2
      select case (opt)
        case(0)  ! .to_product_an_bn
          call ensure_(tonto,size(self,1)==size(a,1),"CPXMAT:to_product_of_2 ... incompatible shapes")
          call ensure_(tonto,size(self,2)==size(b,2),"CPXMAT:to_product_of_2 ... incompatible shapes")
          call ensure_(tonto,size(a,2)==size(b,1),"CPXMAT:to_product_of_2 ... incompatible shapes")
          dima = size(a,2)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (0.0d0,0.0d0)
            do k=1,dima
              self(i,j) = self(i,j) + a(i,k) * b(k,j)
            end do
            self(i,j) = temp
          end do
          end do
        case(1)  ! .to_product_at_bn
          call ensure_(tonto,size(self,1)==size(a,2),"CPXMAT:to_product_of_2 ... incompatible shapes")
          call ensure_(tonto,size(self,2)==size(b,2),"CPXMAT:to_product_of_2 ... incompatible shapes")
          call ensure_(tonto,size(a,1)==size(b,1),"CPXMAT:to_product_of_2 ... incompatible shapes")
          dima = size(a,1)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (0.0d0,0.0d0)
            do k=1,dima
              self(i,j) = self(i,j) + a(k,i) * b(k,j)
            end do
            self(i,j) = temp
          end do
          end do
        case(2)  ! .to_product_an_bd
          call ensure_(tonto,size(self,1)==size(a,1),"CPXMAT:to_product_of_2 ... incompatible shapes")
          call ensure_(tonto,size(self,2)==size(b,1),"CPXMAT:to_product_of_2 ... incompatible shapes")
          call ensure_(tonto,size(a,2)==size(b,2),"CPXMAT:to_product_of_2 ... incompatible shapes")
          dima = size(a,2)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (0.0d0,0.0d0)
            do k=1,dima
              self(i,j) = self(i,j) + a(i,k) * conjg(b(j,k))
            end do
            self(i,j) = temp
          end do
          end do
        case(3)  ! .to_product_at_bd
          call ensure_(tonto,size(self,1)==size(a,2),"CPXMAT:to_product_of_2 ... incompatible shapes")
          call ensure_(tonto,size(self,2)==size(b,1),"CPXMAT:to_product_of_2 ... incompatible shapes")
          call ensure_(tonto,size(a,1)==size(b,2),"CPXMAT:to_product_of_2 ... incompatible shapes")
          dima = size(a,1)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (0.0d0,0.0d0)
            do k=1,dima
              self(i,j) = self(i,j) + a(k,i) * conjg(b(j,k))
            end do
            self(i,j) = temp
          end do
          end do
      end select

   end subroutine

   subroutine plus_product_of(self,a,b,dagger_a,dagger_b)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Add to "self" the matrix product of "a" and "b". If present, "dagger_a"
    ! and "dagger_b" can be set to .true. if "a" and "b" neeb to be daggerd.
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: a, b
      logical(kind=kind(.true.)), optional :: dagger_a, dagger_b
      logical(kind=kind(.true.)) :: dagg_a,dagg_b
      integer(kind=kind(1)) :: opt
      integer(kind=kind(1)) :: dima,dim1,dim2,i,j,k
      complex(kind=kind((1.0d0,1.0d0))) :: temp

      dagg_a = .false.;        dagg_b = .false.
      if (present(dagger_a)) dagg_a = dagger_a
      if (present(dagger_b)) dagg_b = dagger_b
      opt = 0
      if (dagg_a) opt = opt + 1
      if (dagg_b) opt = opt + 2
      select case (opt)
        case(0)  ! .plus_product_an_bn
          call ensure_(tonto,size(self,1)==size(a,1),"CPXMAT:plus_product_of ... incompatible shapes")
          call ensure_(tonto,size(self,2)==size(b,2),"CPXMAT:plus_product_of ... incompatible shapes")
          call ensure_(tonto,size(a,2)==size(b,1),"CPXMAT:plus_product_of ... incompatible shapes")
          dima = size(a,2)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (0.0d0,0.0d0)
            do k=1,dima
              temp = temp + a(i,k) * b(k,j)
            end do
            self(i,j) = self(i,j) + temp
          end do
          end do
        case(1)  ! .plus_product_ad_bn
          call ensure_(tonto,size(self,1)==size(a,2),"CPXMAT:plus_product_of ... incompatible shapes")
          call ensure_(tonto,size(self,2)==size(b,2),"CPXMAT:plus_product_of ... incompatible shapes")
          call ensure_(tonto,size(a,1)==size(b,1),"CPXMAT:plus_product_of ... incompatible shapes")
          dima = size(a,1)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (0.0d0,0.0d0)
            do k=1,dima
              temp = temp + conjg(a(k,i)) * b(k,j)
            end do
            self(i,j) = self(i,j) + temp
          end do
          end do
        case(2)  ! .plus_product_an_bd
          call ensure_(tonto,size(self,1)==size(a,1),"CPXMAT:plus_product_of ... incompatible shapes")
          call ensure_(tonto,size(self,2)==size(b,1),"CPXMAT:plus_product_of ... incompatible shapes")
          call ensure_(tonto,size(a,2)==size(b,2),"CPXMAT:plus_product_of ... incompatible shapes")
          dima = size(a,2)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (0.0d0,0.0d0)
            do k=1,dima
              temp = temp + a(i,k) * conjg(b(j,k))
            end do
            self(i,j) = self(i,j) + temp
          end do
          end do
        case(3)  ! .plus_product_ad_bd
          call ensure_(tonto,size(self,1)==size(a,2),"CPXMAT:plus_product_of ... incompatible shapes")
          call ensure_(tonto,size(self,2)==size(b,1),"CPXMAT:plus_product_of ... incompatible shapes")
          call ensure_(tonto,size(a,1)==size(b,2),"CPXMAT:plus_product_of ... incompatible shapes")
          dima = size(a,1)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (0.0d0,0.0d0)
            do k=1,dima
              temp = temp + conjg(a(k,i)) * conjg(b(j,k))
            end do
            self(i,j) = self(i,j) + temp
          end do
          end do
      end select

   end subroutine

   subroutine plus_product_of_1(self,a,b,dagger_a,transpose_b)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Add to "self" the matrix product of "a" and real matrix "b".
    ! If present, "dagger_a" and "transpose_b" can be set to .true.
    ! if "a" and "b" need to be daggered or transposed
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: a
      real(kind=kind(1.0d0)), dimension(:,:) :: b
      logical(kind=kind(.true.)), optional :: dagger_a, transpose_b
      logical(kind=kind(.true.)) :: dagg_a,trans_b
      integer(kind=kind(1)) :: opt
      integer(kind=kind(1)) :: dima,dim1,dim2,i,j,k
      complex(kind=kind((1.0d0,1.0d0))) :: temp

      dagg_a = .false.;           trans_b = .false.
      if (present(dagger_a))    dagg_a = dagger_a
      if (present(transpose_b)) trans_b = transpose_b
      opt = 0
      if (dagg_a)  opt = opt + 1
      if (trans_b) opt = opt + 2
      select case (opt)
        case(0)  ! .plus_product_an_bn
          call ensure_(tonto,size(self,1)==size(a,1),"CPXMAT:plus_product_of_1 ... incompatible shapes")
          call ensure_(tonto,size(self,2)==size(b,2),"CPXMAT:plus_product_of_1 ... incompatible shapes")
          call ensure_(tonto,size(a,2)==size(b,1),"CPXMAT:plus_product_of_1 ... incompatible shapes")
          dima = size(a,2)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (0.0d0,0.0d0)
            do k=1,dima
              temp = temp + a(i,k) * b(k,j)
            end do
            self(i,j) = self(i,j) + temp
          end do
          end do
        case(1)  ! .plus_product_ad_bn
          call ensure_(tonto,size(self,1)==size(a,2),"CPXMAT:plus_product_of_1 ... incompatible shapes")
          call ensure_(tonto,size(self,2)==size(b,2),"CPXMAT:plus_product_of_1 ... incompatible shapes")
          call ensure_(tonto,size(a,1)==size(b,1),"CPXMAT:plus_product_of_1 ... incompatible shapes")
          dima = size(a,1)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (0.0d0,0.0d0)
            do k=1,dima
              temp = temp + conjg(a(k,i)) * b(k,j)
            end do
            self(i,j) = self(i,j) + temp
          end do
          end do
        case(2)  ! .plus_product_an_bt
          call ensure_(tonto,size(self,1)==size(a,1),"CPXMAT:plus_product_of_1 ... incompatible shapes")
          call ensure_(tonto,size(self,2)==size(b,1),"CPXMAT:plus_product_of_1 ... incompatible shapes")
          call ensure_(tonto,size(a,2)==size(b,2),"CPXMAT:plus_product_of_1 ... incompatible shapes")
          dima = size(a,2)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (0.0d0,0.0d0)
            do k=1,dima
              temp = temp + a(i,k) * b(j,k)
            end do
            self(i,j) = self(i,j) + temp
          end do
          end do
        case(3)  ! .plus_product_ad_bt
          call ensure_(tonto,size(self,1)==size(a,2),"CPXMAT:plus_product_of_1 ... incompatible shapes")
          call ensure_(tonto,size(self,2)==size(b,1),"CPXMAT:plus_product_of_1 ... incompatible shapes")
          call ensure_(tonto,size(a,1)==size(b,2),"CPXMAT:plus_product_of_1 ... incompatible shapes")
          dima = size(a,1)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (0.0d0,0.0d0)
            do k=1,dima
              temp = temp + conjg(a(k,i)) * b(j,k)
            end do
            self(i,j) = self(i,j) + temp
          end do
          end do
      end select

   end subroutine

   subroutine plus_product_of_2(self,a,b,transpose_a,dagger_b)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Add to "self" the product of real matrix "a" and complex matrix "b".
    ! If present, "tranpose_a" and "dagger_b" can be set to .true. if
    ! "a" and "b" need to be transposed or daggerd.
      real(kind=kind(1.0d0)), dimension(:,:) :: a
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: b
      logical(kind=kind(.true.)), optional :: transpose_a, dagger_b
      logical(kind=kind(.true.)) :: trans_a,dagg_b
      integer(kind=kind(1)) :: opt
      integer(kind=kind(1)) :: dima,dim1,dim2,i,j,k
      complex(kind=kind((1.0d0,1.0d0))) :: temp

      trans_a = .false.;          dagg_b  = .false.
      if (present(transpose_a)) trans_a = transpose_a
      if (present(dagger_b))    dagg_b  = dagger_b
      opt = 0
      if (trans_a) opt = opt + 1
      if (dagg_b)  opt = opt + 2
      select case (opt)
        case(0)  ! .plus_product_an_bn
          call ensure_(tonto,size(self,1)==size(a,1),"CPXMAT:plus_product_of_2 ... incompatible shapes")
          call ensure_(tonto,size(self,2)==size(b,2),"CPXMAT:plus_product_of_2 ... incompatible shapes")
          call ensure_(tonto,size(a,2)==size(b,1),"CPXMAT:plus_product_of_2 ... incompatible shapes")
          dima = size(a,2)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (0.0d0,0.0d0)
            do k=1,dima
              temp = temp + a(i,k) * b(k,j)
            end do
            self(i,j) = self(i,j) + temp
          end do
          end do
        case(1)  ! .plus_product_at_bn
          call ensure_(tonto,size(self,1)==size(a,2),"CPXMAT:plus_product_of_2 ... incompatible shapes")
          call ensure_(tonto,size(self,2)==size(b,2),"CPXMAT:plus_product_of_2 ... incompatible shapes")
          call ensure_(tonto,size(a,1)==size(b,1),"CPXMAT:plus_product_of_2 ... incompatible shapes")
          dima = size(a,1)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (0.0d0,0.0d0)
            do k=1,dima
              temp = temp + a(k,i) * b(k,j)
            end do
            self(i,j) = self(i,j) + temp
          end do
          end do
        case(2)  ! .plus_product_an_bd
          call ensure_(tonto,size(self,1)==size(a,1),"CPXMAT:plus_product_of_2 ... incompatible shapes")
          call ensure_(tonto,size(self,2)==size(b,1),"CPXMAT:plus_product_of_2 ... incompatible shapes")
          call ensure_(tonto,size(a,2)==size(b,2),"CPXMAT:plus_product_of_2 ... incompatible shapes")
          dima = size(a,2)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (0.0d0,0.0d0)
            do k=1,dima
              temp = temp + a(i,k) * conjg(b(j,k))
            end do
            self(i,j) = self(i,j) + temp
          end do
          end do
        case(3)  ! .plus_product_at_bd
          call ensure_(tonto,size(self,1)==size(a,2),"CPXMAT:plus_product_of_2 ... incompatible shapes")
          call ensure_(tonto,size(self,2)==size(b,1),"CPXMAT:plus_product_of_2 ... incompatible shapes")
          call ensure_(tonto,size(a,1)==size(b,2),"CPXMAT:plus_product_of_2 ... incompatible shapes")
          dima = size(a,1)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (0.0d0,0.0d0)
            do k=1,dima
              temp = temp + a(k,i) * conjg(b(j,k))
            end do
            self(i,j) = self(i,j) + temp
          end do
          end do
      end select

   end subroutine

   subroutine to_scaled_product_of(self,fac,a,b,dagger_a,dagger_b)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Set "self" to the matrix product of "a" and "b" scaled by "fac". If
    ! present, "dagger_a" and "dagger_b" can be set to .true. if "a" and "b"
    ! need to be daggerred.
      real(kind=kind(1.0d0)) :: fac
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: a, b
      logical(kind=kind(.true.)), optional :: dagger_a, dagger_b
      logical(kind=kind(.true.)) :: dagg_a,dagg_b
      integer(kind=kind(1)) :: opt
      integer(kind=kind(1)) :: dima,dim1,dim2,i,j,k
      complex(kind=kind((1.0d0,1.0d0))) :: temp

      dagg_a = .false.;        dagg_b = .false.
      if (present(dagger_a)) dagg_a = dagger_a
      if (present(dagger_b)) dagg_b = dagger_b
      opt = 0
      if (dagg_a) opt = opt + 1
      if (dagg_b) opt = opt + 2
      select case (opt)
        case(0)  ! .to_scaled_product_an_bn
          call ensure_(tonto,size(self,1)==size(a,1),"CPXMAT:to_scaled_product_of ... incompatible shapes")
          call ensure_(tonto,size(self,2)==size(b,2),"CPXMAT:to_scaled_product_of ... incompatible shapes")
          call ensure_(tonto,size(a,2)==size(b,1),"CPXMAT:to_scaled_product_of ... incompatible shapes")
          dima = size(a,2)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (0.0d0,0.0d0)
            do k=1,dima
              self(i,j) = self(i,j) + a(i,k) * b(k,j)
            end do
            self(i,j) = fac*temp
          end do
          end do
        case(1)  ! .to_scaled_product_ad_bn
          call ensure_(tonto,size(self,1)==size(a,2),"CPXMAT:to_scaled_product_of ... incompatible shapes")
          call ensure_(tonto,size(self,2)==size(b,2),"CPXMAT:to_scaled_product_of ... incompatible shapes")
          call ensure_(tonto,size(a,1)==size(b,1),"CPXMAT:to_scaled_product_of ... incompatible shapes")
          dima = size(a,1)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (0.0d0,0.0d0)
            do k=1,dima
              self(i,j) = self(i,j) + conjg(a(k,i)) * b(k,j)
            end do
            self(i,j) = fac*temp
          end do
          end do
        case(2)  ! .to_scaled_product_an_bd
          call ensure_(tonto,size(self,1)==size(a,1),"CPXMAT:to_scaled_product_of ... incompatible shapes")
          call ensure_(tonto,size(self,2)==size(b,1),"CPXMAT:to_scaled_product_of ... incompatible shapes")
          call ensure_(tonto,size(a,2)==size(b,2),"CPXMAT:to_scaled_product_of ... incompatible shapes")
          dima = size(a,2)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (0.0d0,0.0d0)
            do k=1,dima
              self(i,j) = self(i,j) + a(i,k) * conjg(b(j,k))
            end do
            self(i,j) = fac*temp
          end do
          end do
        case(3)  ! .to_scaled_product_ad_bd
          call ensure_(tonto,size(self,1)==size(a,2),"CPXMAT:to_scaled_product_of ... incompatible shapes")
          call ensure_(tonto,size(self,2)==size(b,1),"CPXMAT:to_scaled_product_of ... incompatible shapes")
          call ensure_(tonto,size(a,1)==size(b,2),"CPXMAT:to_scaled_product_of ... incompatible shapes")
          dima = size(a,1)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (0.0d0,0.0d0)
            do k=1,dima
              self(i,j) = self(i,j) + conjg(a(k,i)) * conjg(b(j,k))
            end do
            self(i,j) = fac*temp
          end do
          end do
      end select

   end subroutine

   subroutine to_scaled_product_of_1(self,fac,a,b,dagger_a,transpose_b)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Set "self" to the matrix product of "a" and real matrix "b" scaled by "fac".
    ! If present, "dagger_a" and "transpose_b" can be set to .true. if "a" and "b"
    ! need to be daggerred or transposed.
      real(kind=kind(1.0d0)) :: fac
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: a
      real(kind=kind(1.0d0)), dimension(:,:) :: b
      logical(kind=kind(.true.)), optional :: dagger_a, transpose_b
      logical(kind=kind(.true.)) :: dagg_a,trans_b
      integer(kind=kind(1)) :: opt
      integer(kind=kind(1)) :: dima,dim1,dim2,i,j,k
      complex(kind=kind((1.0d0,1.0d0))) :: temp

      dagg_a = .false.;           trans_b = .false.
      if (present(dagger_a))    dagg_a  = dagger_a
      if (present(transpose_b)) trans_b = transpose_b
      opt = 0
      if (dagg_a)  opt = opt + 1
      if (trans_b) opt = opt + 2
      select case (opt)
        case(0)  ! .to_scaled_product_an_bn
          call ensure_(tonto,size(self,1)==size(a,1),"CPXMAT:to_scaled_product_of_1 ... incompatible shapes")
          call ensure_(tonto,size(self,2)==size(b,2),"CPXMAT:to_scaled_product_of_1 ... incompatible shapes")
          call ensure_(tonto,size(a,2)==size(b,1),"CPXMAT:to_scaled_product_of_1 ... incompatible shapes")
          dima = size(a,2)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (0.0d0,0.0d0)
            do k=1,dima
              self(i,j) = self(i,j) + a(i,k) * b(k,j)
            end do
            self(i,j) = fac*temp
          end do
          end do
        case(1)  ! .to_scaled_product_ad_bn
          call ensure_(tonto,size(self,1)==size(a,2),"CPXMAT:to_scaled_product_of_1 ... incompatible shapes")
          call ensure_(tonto,size(self,2)==size(b,2),"CPXMAT:to_scaled_product_of_1 ... incompatible shapes")
          call ensure_(tonto,size(a,1)==size(b,1),"CPXMAT:to_scaled_product_of_1 ... incompatible shapes")
          dima = size(a,1)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (0.0d0,0.0d0)
            do k=1,dima
              self(i,j) = self(i,j) + conjg(a(k,i)) * b(k,j)
            end do
            self(i,j) = fac*temp
          end do
          end do
        case(2)  ! .to_scaled_product_an_bt
          call ensure_(tonto,size(self,1)==size(a,1),"CPXMAT:to_scaled_product_of_1 ... incompatible shapes")
          call ensure_(tonto,size(self,2)==size(b,1),"CPXMAT:to_scaled_product_of_1 ... incompatible shapes")
          call ensure_(tonto,size(a,2)==size(b,2),"CPXMAT:to_scaled_product_of_1 ... incompatible shapes")
          dima = size(a,2)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (0.0d0,0.0d0)
            do k=1,dima
              self(i,j) = self(i,j) + a(i,k) * b(j,k)
            end do
            self(i,j) = fac*temp
          end do
          end do
        case(3)  ! .to_scaled_product_ad_bt
          call ensure_(tonto,size(self,1)==size(a,2),"CPXMAT:to_scaled_product_of_1 ... incompatible shapes")
          call ensure_(tonto,size(self,2)==size(b,1),"CPXMAT:to_scaled_product_of_1 ... incompatible shapes")
          call ensure_(tonto,size(a,1)==size(b,2),"CPXMAT:to_scaled_product_of_1 ... incompatible shapes")
          dima = size(a,1)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (0.0d0,0.0d0)
            do k=1,dima
              self(i,j) = self(i,j) + conjg(a(k,i)) * b(j,k)
            end do
            self(i,j) = fac*temp
          end do
          end do
      end select

   end subroutine

   subroutine to_scaled_product_of_2(self,fac,a,b,transpose_a,dagger_b)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Set "self" to the product of real matrix "a" and complex "b" scaled by "fac".
    ! If present, "transpose_a" and "dagger_b" can be set to .true. if "a" and "b"
    ! need to be transposed or daggerred.
      real(kind=kind(1.0d0)) :: fac
      real(kind=kind(1.0d0)), dimension(:,:) :: a
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: b
      logical(kind=kind(.true.)), optional :: transpose_a, dagger_b
      logical(kind=kind(.true.)) :: trans_a,dagg_b
      integer(kind=kind(1)) :: opt
      integer(kind=kind(1)) :: dima,dim1,dim2,i,j,k
      complex(kind=kind((1.0d0,1.0d0))) :: temp

      trans_a = .false.;          dagg_b  = .false.
      if (present(transpose_a)) trans_a = transpose_a
      if (present(dagger_b))    dagg_b  = dagger_b
      opt = 0
      if (trans_a) opt = opt + 1
      if (dagg_b)  opt = opt + 2
      select case (opt)
        case(0)  ! .to_scaled_product_an_bn
          call ensure_(tonto,size(self,1)==size(a,1),"CPXMAT:to_scaled_product_of_2 ... incompatible shapes")
          call ensure_(tonto,size(self,2)==size(b,2),"CPXMAT:to_scaled_product_of_2 ... incompatible shapes")
          call ensure_(tonto,size(a,2)==size(b,1),"CPXMAT:to_scaled_product_of_2 ... incompatible shapes")
          dima = size(a,2)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (0.0d0,0.0d0)
            do k=1,dima
              temp = temp + a(i,k) * b(k,j)
            end do
            self(i,j) = fac*temp
          end do
          end do
        case(1)  ! .to_scaled_product_at_bn
          call ensure_(tonto,size(self,1)==size(a,2),"CPXMAT:to_scaled_product_of_2 ... incompatible shapes")
          call ensure_(tonto,size(self,2)==size(b,2),"CPXMAT:to_scaled_product_of_2 ... incompatible shapes")
          call ensure_(tonto,size(a,1)==size(b,1),"CPXMAT:to_scaled_product_of_2 ... incompatible shapes")
          dima = size(a,1)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (0.0d0,0.0d0)
            do k=1,dima
              temp = temp + a(k,i) * b(k,j)
            end do
            self(i,j) = fac*temp
          end do
          end do
        case(2)  ! .to_scaled_product_an_bd
          call ensure_(tonto,size(self,1)==size(a,1),"CPXMAT:to_scaled_product_of_2 ... incompatible shapes")
          call ensure_(tonto,size(self,2)==size(b,1),"CPXMAT:to_scaled_product_of_2 ... incompatible shapes")
          call ensure_(tonto,size(a,2)==size(b,2),"CPXMAT:to_scaled_product_of_2 ... incompatible shapes")
          dima = size(a,2)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (0.0d0,0.0d0)
            do k=1,dima
              temp = temp + a(i,k) * conjg(b(j,k))
            end do
            self(i,j) = fac*temp
          end do
          end do
        case(3)  ! .to_scaled_product_at_bd
          call ensure_(tonto,size(self,1)==size(a,2),"CPXMAT:to_scaled_product_of_2 ... incompatible shapes")
          call ensure_(tonto,size(self,2)==size(b,1),"CPXMAT:to_scaled_product_of_2 ... incompatible shapes")
          call ensure_(tonto,size(a,1)==size(b,2),"CPXMAT:to_scaled_product_of_2 ... incompatible shapes")
          dima = size(a,1)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (0.0d0,0.0d0)
            do k=1,dima
              temp = temp + a(k,i) * conjg(b(j,k))
            end do
            self(i,j) = fac*temp
          end do
          end do
      end select

   end subroutine

   subroutine plus_scaled_product_of(self,fac,a,b,dagger_a,dagger_b)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Add to "self" the matrix product of "a" and "b" scaled by "fac". If
    ! present, "dagger_a" and "dagger_b" can be set to .true. if "a" and "b"
    ! need to be daggerred.
      real(kind=kind(1.0d0)) :: fac
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: a, b
      logical(kind=kind(.true.)), optional :: dagger_a, dagger_b
      logical(kind=kind(.true.)) :: dagg_a,dagg_b
      integer(kind=kind(1)) :: opt
      integer(kind=kind(1)) :: dima,dim1,dim2,i,j,k
      complex(kind=kind((1.0d0,1.0d0))) :: temp

      dagg_a = .false.;        dagg_b = .false.
      if (present(dagger_a)) dagg_a = dagger_a
      if (present(dagger_b)) dagg_b = dagger_b
      opt = 0
      if (dagg_a) opt = opt + 1
      if (dagg_b) opt = opt + 2
      select case (opt)
        case(0)  ! .plus_scaled_product_an_bn
          call ensure_(tonto,size(self,1)==size(a,1),"CPXMAT:plus_scaled_product_of ... incompatible shapes")
          call ensure_(tonto,size(self,2)==size(b,2),"CPXMAT:plus_scaled_product_of ... incompatible shapes")
          call ensure_(tonto,size(a,2)==size(b,1),"CPXMAT:plus_scaled_product_of ... incompatible shapes")
          dima = size(a,2)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (0.0d0,0.0d0)
            do k=1,dima
              temp = temp + fac * a(i,k) * b(k,j)
            end do
            self(i,j) = self(i,j) + fac*temp
          end do
          end do
        case(1)  ! .plus_scaled_product_ad_bn
          call ensure_(tonto,size(self,1)==size(a,2),"CPXMAT:plus_scaled_product_of ... incompatible shapes")
          call ensure_(tonto,size(self,2)==size(b,2),"CPXMAT:plus_scaled_product_of ... incompatible shapes")
          call ensure_(tonto,size(a,1)==size(b,1),"CPXMAT:plus_scaled_product_of ... incompatible shapes")
          dima = size(a,1)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (0.0d0,0.0d0)
            do k=1,dima
              temp = temp + fac * conjg(a(k,i)) * b(k,j)
            end do
            self(i,j) = self(i,j) + fac*temp
          end do
          end do
        case(2)  ! .plus_scaled_product_an_bd
          call ensure_(tonto,size(self,1)==size(a,1),"CPXMAT:plus_scaled_product_of ... incompatible shapes")
          call ensure_(tonto,size(self,2)==size(b,1),"CPXMAT:plus_scaled_product_of ... incompatible shapes")
          call ensure_(tonto,size(a,2)==size(b,2),"CPXMAT:plus_scaled_product_of ... incompatible shapes")
          dima = size(a,2)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (0.0d0,0.0d0)
            do k=1,dima
              temp = temp + fac * a(i,k) * conjg(b(j,k))
            end do
            self(i,j) = self(i,j) + fac*temp
          end do
          end do
        case(3)  ! .plus_scaled_product_ad_bd
          call ensure_(tonto,size(self,1)==size(a,2),"CPXMAT:plus_scaled_product_of ... incompatible shapes")
          call ensure_(tonto,size(self,2)==size(b,1),"CPXMAT:plus_scaled_product_of ... incompatible shapes")
          call ensure_(tonto,size(a,1)==size(b,2),"CPXMAT:plus_scaled_product_of ... incompatible shapes")
          dima = size(a,1)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (0.0d0,0.0d0)
            do k=1,dima
              temp = temp + fac * conjg(a(k,i)) * conjg(b(j,k))
            end do
            self(i,j) = self(i,j) + fac*temp
          end do
          end do
      end select

   end subroutine

   subroutine plus_scaled_product_of_1(self,fac,a,b,dagger_a,transpose_b)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Add to "self" the matrix product of "a" and real matrix "b" scaled by "fac".
    ! If present, "dagger_a" and "transpose_b" can be set to .true. if "a" and "b"
    ! need to be daggerred or transposed.
      real(kind=kind(1.0d0)) :: fac
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: a
      real(kind=kind(1.0d0)), dimension(:,:) :: b
      logical(kind=kind(.true.)), optional :: dagger_a, transpose_b
      logical(kind=kind(.true.)) :: dagg_a,trans_b
      integer(kind=kind(1)) :: opt
      integer(kind=kind(1)) :: dima,dim1,dim2,i,j,k
      complex(kind=kind((1.0d0,1.0d0))) :: temp

      dagg_a = .false.;           trans_b = .false.
      if (present(dagger_a))    dagg_a  = dagger_a
      if (present(transpose_b)) trans_b = transpose_b
      opt = 0
      if (dagg_a)  opt = opt + 1
      if (trans_b) opt = opt + 2
      select case (opt)
        case(0)  ! .plus_scaled_product_an_bn
          call ensure_(tonto,size(self,1)==size(a,1),"CPXMAT:plus_scaled_product_of_1 ... incompatible shapes")
          call ensure_(tonto,size(self,2)==size(b,2),"CPXMAT:plus_scaled_product_of_1 ... incompatible shapes")
          call ensure_(tonto,size(a,2)==size(b,1),"CPXMAT:plus_scaled_product_of_1 ... incompatible shapes")
          dima = size(a,2)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (0.0d0,0.0d0)
            do k=1,dima
              temp = temp + fac * a(i,k) * b(k,j)
            end do
            self(i,j) = self(i,j) + fac*temp
          end do
          end do
        case(1)  ! .plus_scaled_product_ad_bn
          call ensure_(tonto,size(self,1)==size(a,2),"CPXMAT:plus_scaled_product_of_1 ... incompatible shapes")
          call ensure_(tonto,size(self,2)==size(b,2),"CPXMAT:plus_scaled_product_of_1 ... incompatible shapes")
          call ensure_(tonto,size(a,1)==size(b,1),"CPXMAT:plus_scaled_product_of_1 ... incompatible shapes")
          dima = size(a,1)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (0.0d0,0.0d0)
            do k=1,dima
              temp = temp + fac * conjg(a(k,i)) * b(k,j)
            end do
            self(i,j) = self(i,j) + fac*temp
          end do
          end do
        case(2)  ! .plus_scaled_product_an_bt
          call ensure_(tonto,size(self,1)==size(a,1),"CPXMAT:plus_scaled_product_of_1 ... incompatible shapes")
          call ensure_(tonto,size(self,2)==size(b,1),"CPXMAT:plus_scaled_product_of_1 ... incompatible shapes")
          call ensure_(tonto,size(a,2)==size(b,2),"CPXMAT:plus_scaled_product_of_1 ... incompatible shapes")
          dima = size(a,2)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (0.0d0,0.0d0)
            do k=1,dima
              temp = temp + fac * a(i,k) * b(j,k)
            end do
            self(i,j) = self(i,j) + fac*temp
          end do
          end do
        case(3)  ! .plus_scaled_product_ad_bt
          call ensure_(tonto,size(self,1)==size(a,2),"CPXMAT:plus_scaled_product_of_1 ... incompatible shapes")
          call ensure_(tonto,size(self,2)==size(b,1),"CPXMAT:plus_scaled_product_of_1 ... incompatible shapes")
          call ensure_(tonto,size(a,1)==size(b,2),"CPXMAT:plus_scaled_product_of_1 ... incompatible shapes")
          dima = size(a,1)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (0.0d0,0.0d0)
            do k=1,dima
              temp = temp + fac * conjg(a(k,i)) * b(j,k)
            end do
            self(i,j) = self(i,j) + fac*temp
          end do
          end do
      end select

   end subroutine

   subroutine plus_scaled_product_of_2(self,fac,a,b,transpose_a,dagger_b)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Add to "self" the product of real matrix "a" and complex "b" scaled by "fac".
    ! If present, "transpose_a" and "dagger_b" can be set to .true. if "a" and "b"
    ! need to be transposed or daggerred.
      real(kind=kind(1.0d0)) :: fac
      real(kind=kind(1.0d0)), dimension(:,:) :: a
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: b
      logical(kind=kind(.true.)), optional :: transpose_a, dagger_b
      logical(kind=kind(.true.)) :: trans_a,dagg_b
      integer(kind=kind(1)) :: opt
      integer(kind=kind(1)) :: dima,dim1,dim2,i,j,k
      complex(kind=kind((1.0d0,1.0d0))) :: temp

      trans_a = .false.;          dagg_b  = .false.
      if (present(transpose_a)) trans_a = transpose_a
      if (present(dagger_b))    dagg_b  = dagger_b
      opt = 0
      if (trans_a) opt = opt + 1
      if (dagg_b)  opt = opt + 2
      select case (opt)
        case(0)  ! .plus_scaled_product_an_bn
          call ensure_(tonto,size(self,1)==size(a,1),"CPXMAT:plus_scaled_product_of_2 ... incompatible shapes")
          call ensure_(tonto,size(self,2)==size(b,2),"CPXMAT:plus_scaled_product_of_2 ... incompatible shapes")
          call ensure_(tonto,size(a,2)==size(b,1),"CPXMAT:plus_scaled_product_of_2 ... incompatible shapes")
          dima = size(a,2)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (0.0d0,0.0d0)
            do k=1,dima
              temp = temp + fac * a(i,k) * b(k,j)
            end do
            self(i,j) = self(i,j) + fac*temp
          end do
          end do
        case(1)  ! .plus_scaled_product_at_bn
          call ensure_(tonto,size(self,1)==size(a,2),"CPXMAT:plus_scaled_product_of_2 ... incompatible shapes")
          call ensure_(tonto,size(self,2)==size(b,2),"CPXMAT:plus_scaled_product_of_2 ... incompatible shapes")
          call ensure_(tonto,size(a,1)==size(b,1),"CPXMAT:plus_scaled_product_of_2 ... incompatible shapes")
          dima = size(a,1)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (0.0d0,0.0d0)
            do k=1,dima
              temp = temp + fac * a(k,i) * b(k,j)
            end do
            self(i,j) = self(i,j) + fac*temp
          end do
          end do
        case(2)  ! .plus_scaled_product_an_bd
          call ensure_(tonto,size(self,1)==size(a,1),"CPXMAT:plus_scaled_product_of_2 ... incompatible shapes")
          call ensure_(tonto,size(self,2)==size(b,1),"CPXMAT:plus_scaled_product_of_2 ... incompatible shapes")
          call ensure_(tonto,size(a,2)==size(b,2),"CPXMAT:plus_scaled_product_of_2 ... incompatible shapes")
          dima = size(a,2)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (0.0d0,0.0d0)
            do k=1,dima
              temp = temp + fac * a(i,k) * conjg(b(j,k))
            end do
            self(i,j) = self(i,j) + fac*temp
          end do
          end do
        case(3)  ! .plus_scaled_product_at_bd
          call ensure_(tonto,size(self,1)==size(a,2),"CPXMAT:plus_scaled_product_of_2 ... incompatible shapes")
          call ensure_(tonto,size(self,2)==size(b,1),"CPXMAT:plus_scaled_product_of_2 ... incompatible shapes")
          call ensure_(tonto,size(a,1)==size(b,2),"CPXMAT:plus_scaled_product_of_2 ... incompatible shapes")
          dima = size(a,1)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (0.0d0,0.0d0)
            do k=1,dima
              temp = temp + fac * a(k,i) * conjg(b(j,k))
            end do
            self(i,j) = self(i,j) + fac*temp
          end do
          end do
      end select

   end subroutine

   subroutine to_product_with_diagonal(self,a,diag,dagger_a)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! set self to the product of "a" and with diagonal "diag"
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: a
      real(kind=kind(1.0d0)), dimension(:) :: diag
      logical(kind=kind(.true.)), optional :: dagger_a
      integer(kind=kind(1)) :: a1,a2,s1,s2,d1,i,j
      real(kind=kind(1.0d0)) :: diag_j

      call ensure_(tonto,is_same_shape_as_(self,a),"CPXMAT:to_product_with_diagonal ... incompatible dimensions")
      s1 = size(self,1); s2 = size(self,2)
      a1 = size(a,1);    a2 = size(a,2)
      d1 = size(diag)
      if (present(dagger_a)) then
         call ensure_(tonto,a1==d1,"CPXMAT:to_product_with_diagonal ... incompatible dimensions")
         do j = 1,s2
           diag_j = diag(j)
           do i = 1,s1
             self(i,j) = conjg(a(j,i))*diag_j
           end do
         end do
      else
         call ensure_(tonto,a2==d1,"CPXMAT:to_product_with_diagonal ... incompatible dimensions")
         do j = 1,s2
           diag_j = diag(j)
           do i = 1,s1
             self(i,j) = a(i,j)*diag_j
           end do
         end do
      end if

   end subroutine

   subroutine to_product_with_diagonal_1(self,dg,a,dagger_a)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! set self to the product of diagonal "dg" with "a"
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: a
      real(kind=kind(1.0d0)), dimension(:) :: dg
      logical(kind=kind(.true.)), optional :: dagger_a
      integer(kind=kind(1)) :: a1,a2,s1,s2,d1,i,j
      real(kind=kind(1.0d0)) :: dg_i

      call ensure_(tonto,is_same_shape_as_(self,a),"CPXMAT:to_product_with_diagonal_1 ... incompatible dimensions")
      s1 = size(self,1);  s2 = size(self,2)
      a1 = size(a,1); a2 = size(a,2)
      d1 = size(dg)
      if (present(dagger_a)) then
         call ensure_(tonto,a2==d1,"CPXMAT:to_product_with_diagonal_1 ... incompatible dimensions")
         do i = 1,s1
           dg_i = dg(i)
           do j = 1,s2
             self(i,j) = dg_i*conjg(a(j,i))
           end do
         end do
      else
         call ensure_(tonto,a1==d1,"CPXMAT:to_product_with_diagonal_1 ... incompatible dimensions")
         do i = 1,s1
           dg_i = dg(i)
           do j = 1,s2
             self(i,j) = dg_i*a(i,j)
           end do
         end do
      end if

   end subroutine

   function trace(self) result(res)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Return the trace of self
      intent(in) :: self
      complex(kind=kind((1.0d0,1.0d0))) :: res
    ! The following code is inherited from INTRINSICMAT
      integer(kind=kind(1)) :: dim,i

      call ensure_(tonto,size(self,1)==size(self,2),"CPXMAT:trace ... non-square matrix")
      dim = size(self,1)
      res = 0.0d0
      do i = 1,dim
         res = res + self(i,i)
      end do

   end function

! These next few commented routines are unused ESSL routines.

!   solve_general_eigenproblem(eigenvalues,eigenvectors)
!   ! Solve the eigenproblem for "self", yeilding a vector of "eigenvalues" and
!   ! a matrix of "eigenvectors"
!      eigenvalues :: CPXVEC
!      eigenvectors :: CPXMAT
!       W :: REALVEC*
!      dim1,dim2,dime,dimv :: integer(kind=kind(1))
!      select :: logical(kind=kind(.true.))
!      dim1 = .dim1
!      dim2 = .dim2
!      dime = size(eigenvalues)
!      dimv = size(eigenvectors)
!      call ensure_(tonto,dim1==dim2,"non-square matrix")
!      call ensure_(tonto,dime>=dim1,"supplied eigenvalue array too small")
!      call ensure_(tonto,dimv>=dim1*dim1,"supplied eigenvector matrix too small")
!      W.create(3*dim1)
!      call zgeev(1,self,dim1,eigenvalues,eigenvectors,dim1,select,dim1,W,3*dim1)
!      W.destroy
!   end
!
!   solve_linear_equations(rhs,solution)
!   ! Solve the linear equations posed by "self", with "rhs" as the RHS vector,
!   ! yeilding vector "solution" as the answer
!      rhs, solution :: CPXVEC
!      err,nrhs :: integer(kind=kind(1))
!      LU :: CPXMAT*
!      pivot :: INTVEC*
!      dim,dim1,dim2 :: integer(kind=kind(1))
!      dim1 = .dim1
!      dim2 = .dim2
!      call ensure_(tonto,dim1==dim2,"non-square matrix")
!      dim = size(rhs)
!      call ensure_(tonto,dim==dim1,"incompatible rhs")
!      nrhs = 1
!      LU.create(dim,dim)
!      pivot.create(dim)
!      LU = self
!      solution = rhs
!      call zgef(LU,dim,dim,pivot)
!      call zges(LU,dim,dim,pivot,solution,0)
!      pivot.destroy
!      LU.destroy
!   end
!
!   solve_linear_equations(rhs,solution)
!   ! Solve the linear equations posed by "self", with "rhs" as a matrix of RHS vectors,
!   ! yeilding matrix "solution" as a matrix of solution vectors.
!      rhs, solution :: CPXMAT
!      err,nrhs :: integer(kind=kind(1))
!      LU :: CPXMAT*
!      pivot :: INTVEC*
!      dim1,dim2 :: integer(kind=kind(1))
!      dim1 = .dim1
!      dim2 = .dim2
!      call ensure_(tonto,dim1==dim2,"non-square matrix")
!      dim1 = size(rhs,1)
!      nrhs = size(rhs,2)
!      call ensure_(tonto,dim1==dim2,"rhs incompatible with coefficient matrix")
!      call ensure_(tonto,nrhs>0,"no rhs vectors")
!      LU.create(dim1,dim1)
!      pivot.create(dim1)
!      LU = self
!      solution = rhs
!      call zgef(LU,dim1,dim1,pivot)
!      call zgesm("N",LU,dim1,dim1,pivot,solution,dim1,nrhs)
!      pivot.destroy
!      LU.destroy
!   end

   subroutine solve_eigenproblem(self,eigenvalues,eigenvectors,routine)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Solve the hermitian eigenvalue problem for self
      real(kind=kind(1.0d0)), dimension(:) :: eigenvalues
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: eigenvectors
      character(*), optional :: routine

      call solve_eigenproblem_LAPACK_(self,eigenvalues,eigenvectors,routine)
   end subroutine

   subroutine solve_eigenproblem_ESSL(self,eigenvalues,eigenvectors,routine)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Solve the hermitian eigenvalue problem for self. ESSL version.
      real(kind=kind(1.0d0)), dimension(:) :: eigenvalues
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: eigenvectors
      character(*), optional :: routine
      complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: ap
      real(kind=kind(1.0d0)), dimension(:), pointer :: RW
      integer(kind=kind(1)) :: dim

      call ensure_(tonto,is_square_(self),"CPXMAT:solve_eigenproblem_ESSL ... non-square matrix")
      call ensure_(tonto,size(eigenvalues)>=size(self,1),"CPXMAT:solve_eigenproblem_ESSL ... supplied eigenvalue array too sm&
&all")
      call ensure_(tonto,size(eigenvectors)>=size(self),"CPXMAT:solve_eigenproblem_ESSL ... supplied eigenvector array too sm&
&all")
      call ensure_(tonto,.not. present(routine),"CPXMAT:solve_eigenproblem_ESSL ... routine specifier not allowed in ESSL ver&
&sion")
      dim = size(self,1)
      call create_(ap,dim*(dim+1)/2)
      call compress_to_triangle_(self,ap)
      call create_(RW,4*dim)
      call destroy_(RW)
      call destroy_(ap)

   end subroutine

   subroutine solve_eigenproblem_LAPACK(self,eigenvalues,eigenvectors,routine)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Solve the hermitian eigenvalue problem for self. LAPACK version.
      real(kind=kind(1.0d0)), dimension(:) :: eigenvalues
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: eigenvectors
      character(*), optional :: routine
      character(128) :: rout
      complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: ap,W
      real(kind=kind(1.0d0)), dimension(:), pointer :: RW
      integer(kind=kind(1)) :: dim,fail,info

      call ensure_(tonto,is_square_(self),"CPXMAT:solve_eigenproblem_LAPACK ... non-square matrix")
      call ensure_(tonto,size(eigenvalues)>=size(self,1),"CPXMAT:solve_eigenproblem_LAPACK ... supplied eigenvalue array too &
&small")
      call ensure_(tonto,size(eigenvectors)>=size(self),"CPXMAT:solve_eigenproblem_LAPACK ... supplied eigenvector array too &
&small")
      rout = "zheev"
      if (present(routine)) rout = routine
      dim = size(self,1)
      select case (rout)
        case ("zheev")
          call create_(W,dim*dim)
          call create_(RW,3*dim)
          call set_to_(eigenvectors,self)
          fail = 0
          call zheev("V","L",dim,eigenvectors,dim,eigenvalues,W,dim*dim,RW,fail)
          call destroy_(RW)
          call destroy_(W)
        case ("zhpev")
          call create_(ap,dim*(dim+1)/2)
          call compress_to_triangle_(self,ap)
          call create_(W,2*dim)
          call create_(RW,3*dim)
          call zhpev("V","U",dim,ap,eigenvalues,eigenvectors,dim,W,RW,info)
          call destroy_(RW)
          call destroy_(W)
          call destroy_(ap)
      end select

   end subroutine

   function trace_of_product_with(self,b) result(res)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Return the trace of the product of self with "b"
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), intent(in) :: b
      complex(kind=kind((1.0d0,1.0d0))) :: res

      res = trace_of_product_(self,b)

   end function

   function trace_of_product(self,b) result(res)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Return the trace of the product of self with "b"
      intent(in) :: self
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), intent(in) :: b
      complex(kind=kind((1.0d0,1.0d0))) :: res
      integer(kind=kind(1)) :: a1,a2,i,j

      call ensure_(tonto,is_transposed_shape_of_(self,b),"CPXMAT:trace_of_product ... incompatible shape")
      a1 = size(self,1)
      a2 = size(self,2)
      res = 0.0d0
      if (a1==1 .and. a2==1) then
        res = self(1,1)*b(1,1)
      else
        res = 0.0d0
        do i = 1,a1
          do j = 1,a2
            res = res + self(i,j)*b(j,i)
          end do
        end do
      end if

   end function

   function trace_of_product_with_1(self,b) result(res)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Return the trace of the product of self with real matrix "b"
      intent(in) :: self
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: b
      complex(kind=kind((1.0d0,1.0d0))) :: res

      res = trace_of_product_(self,b)

   end function

   function trace_of_product_1(self,b) result(res)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Return the trace of the product of self with real matrix "b"
      intent(in) :: self
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: b
      complex(kind=kind((1.0d0,1.0d0))) :: res
      integer(kind=kind(1)) :: a1,a2,i,j

      call ensure_(tonto,is_transposed_shape_of_(self,b),"CPXMAT:trace_of_product_1 ... incompatible shape")
      a1 = size(self,1)
      a2 = size(self,2)
      if (a1==1 .and. a2==1) then
        res = self(1,1)*b(1,1)
      else
        res = 0.0d0
        do i = 1,a1
          do j = 1,a2
            res = res + self(i,j)*b(j,i)
          end do
        end do
      end if

   end function

   function equals(self,b) result(res)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Check if the matrix is the same as "b".
      intent(in) :: self
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), intent(in) :: b
      logical(kind=kind(.true.)) :: res

      res = same_as_(self,b)

   end function

   function same_as(self,b, tol) result(res)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Return .true. if self is the same as "b:", within tolerance "tol", if
    ! provided, or 10^-5 if not.
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: b
      real(kind=kind(1.0d0)), optional :: tol
      logical(kind=kind(.true.)) :: res
      real(kind=kind(1.0d0)) :: diff
      integer(kind=kind(1)) :: a1,a2,i,j
      real(kind=kind(1.0d0)) :: tolerance

      call ensure_(tonto,is_same_shape_as_(self,b),"CPXMAT:same_as ... incompatible shape")
      a1 = size(self,1)
      a2 = size(self,2)
      if (present(tol))     tolerance = tol
      if (.not. present(tol)) tolerance = tol5
      diff = 0.0d0
      do i = 1,a1
      do j = 1,a2
         diff = diff + abs(self(j,i)-b(j,i))**2
      end do
      end do
      diff = sqrt(diff)
      res = .false.
      if (diff<tolerance) res=.true.

   end function

   subroutine swap_columns(self,col1,col2)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Swap columns "col1" and "col2" of self
      integer(kind=kind(1)), intent(in) :: col1,col2
    ! The following code is inherited from INTRINSICMAT
      integer(kind=kind(1)) :: a1,a2,i
      complex(kind=kind((1.0d0,1.0d0))) :: val

      call ensure_(tonto,col1<=size(self,2) .and. col2<=size(self,2),"CPXMAT:swap_columns ... columns exceed dimesions")
      a1 = size(self,1)
      a2 = size(self,2)
      do i = 1,a1
         val = self(i,col1)
         self(i,col1) = self(i,col2)
         self(i,col2) = val
      end do

   end subroutine

   subroutine set_to(self,b)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Set self to "b"
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), intent(in) :: b
    ! The following code is inherited from INTRINSICMAT

      call ensure_(tonto,is_same_shape_as_(self,b),"CPXMAT:set_to ... incompatible shape")
      self = b

   end subroutine

   subroutine plus(self,b)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Add to self the matrix "b"
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), intent(in) :: b
    ! The following code is inherited from INTRINSICMAT

      call ensure_(tonto,is_same_shape_as_(self,b),"CPXMAT:plus ... incompatible shape")
      self = self+b

   end subroutine

   subroutine minus(self,b)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Subtract from self the matrix "b"
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), intent(in) :: b
    ! The following code is inherited from INTRINSICMAT

      call ensure_(tonto,is_same_shape_as_(self,b),"CPXMAT:minus ... incompatible shape")
      self = self-b

   end subroutine

   subroutine to_scaled_mat(self,fac,b)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Set self to the scaled matrix "b"
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), intent(in) :: b
      complex(kind=kind((1.0d0,1.0d0))), intent(in) :: fac

      call ensure_(tonto,is_same_shape_as_(self,b),"CPXMAT:to_scaled_mat ... incompatible shape")
      self = fac*b

   end subroutine

   subroutine to_scaled_mat_1(self,fac,b)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Set self to the scaled matrix "b"
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), intent(in) :: b
      real(kind=kind(1.0d0)), intent(in) :: fac

      call ensure_(tonto,is_same_shape_as_(self,b),"CPXMAT:to_scaled_mat_1 ... incompatible shape")
      self = fac*b

   end subroutine

   subroutine plus_scaled_mat(self,fac,b)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Add to self the scaled matrix "b"
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), intent(in) :: b
      complex(kind=kind((1.0d0,1.0d0))), intent(in) :: fac

      call ensure_(tonto,is_same_shape_as_(self,b),"CPXMAT:plus_scaled_mat ... incompatible shape")
      self = self+fac*b

   end subroutine

   subroutine plus_scaled_mat_1(self,fac,b)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Add to self the scaled matrix "b"
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), intent(in) :: b
      real(kind=kind(1.0d0)), intent(in) :: fac

      call ensure_(tonto,is_same_shape_as_(self,b),"CPXMAT:plus_scaled_mat_1 ... incompatible shape")
      self = self+fac*b

   end subroutine

   subroutine change_basis(self,V)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Change the basis of self using matrix V, i.e. self = V^dagger self V
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: V
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: W
      integer(kind=kind(1)) :: o1,v2

      call ensure_(tonto,is_square_(self),"CPXMAT:change_basis ... non-square matrix")
      call ensure_(tonto,is_square_(V),"CPXMAT:change_basis ... new basis not square")
      call ensure_(tonto,size(self,1)==size(V,1),"CPXMAT:change_basis ... incompatible sizes")
      o1 = size(self,1)
      v2 = size(V,2)
      call create_(W,o1,v2)
      call to_product_of_(W,self,V)
      call to_product_of_(self,V,W,dagger_a=.true.)
    !  W = matmul(self,V)
    !  self = matmul(transpose(conjg(V)),W)
      call destroy_(W)

   end subroutine

   subroutine change_basis_1(self,L,R)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Change the basis of self using diagonals L and R, i.e. self = L^T self R
      real(kind=kind(1.0d0)), dimension(:) :: L,R
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: W
      integer(kind=kind(1)) :: l1,r1

      call ensure_(tonto,size(self,1)==size(L),"CPXMAT:change_basis_1 ... incompatible sizes")
      call ensure_(tonto,size(self,2)==size(R),"CPXMAT:change_basis_1 ... incompatible sizes")
      l1 = size(L)
      r1 = size(R)
      call create_(W,l1,r1)
      call to_product_with_diagonal_(W,self,R)
      call to_product_with_diagonal_(self,L,W)
      call destroy_(W)

   end subroutine

   subroutine change_basis_2(self,new,V)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Set new = V^T self V
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), intent(out) :: new
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), intent(in) :: V
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: W
      integer(kind=kind(1)) :: o1,v2

      call ensure_(tonto,is_square_(self),"CPXMAT:change_basis_2 ... non-square matrix")
      call ensure_(tonto,is_square_(new),"CPXMAT:change_basis_2 ... new basis not square")
      call ensure_(tonto,size(V,1)==size(self,2),"CPXMAT:change_basis_2 ... incompatible sizes")
      call ensure_(tonto,size(V,2)==size(new,2),"CPXMAT:change_basis_2 ... incompatible sizes")
      o1 = size(self,1)
      v2 = size(V,2)
      call create_(W,o1,v2)
      call to_product_of_(W,self,V)
      call to_product_of_(new,V,W,dagger_a=.true.)
      call destroy_(W)

   end subroutine

   subroutine change_basis_3(self,new,V)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Set new = V^T self V, V a real matrix
      intent(in) :: self
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), intent(inout) :: new
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: V
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: W
      integer(kind=kind(1)) :: o1,v2

      call ensure_(tonto,is_square_(self),"CPXMAT:change_basis_3 ... non-square matrix")
      call ensure_(tonto,is_square_(new),"CPXMAT:change_basis_3 ... new basis not square")
      call ensure_(tonto,size(V,1)==size(self,2),"CPXMAT:change_basis_3 ... incompatible sizes")
      call ensure_(tonto,size(V,2)==size(new,2),"CPXMAT:change_basis_3 ... incompatible sizes")
      o1 = size(self,1)
      v2 = size(V,2)
      call create_(W,o1,v2)
      call to_product_of_(W,self,V)
      call to_product_of_(new,V,W,transpose_a=.true.)
      call destroy_(W)

   end subroutine

   subroutine back_transform(self,new,V)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Back transform "self" using vectors "V", and place the result in "new",
    ! new = V self V^dagger
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: new,V
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: W
      integer(kind=kind(1)) :: v1,o2

      call ensure_(tonto,is_square_(self),"CPXMAT:back_transform ... non-square matrix")
      call ensure_(tonto,is_square_(new),"CPXMAT:back_transform ... new basis not square")
      call ensure_(tonto,size(V,1)==size(new,1),"CPXMAT:back_transform ... incompatible sizes")
      call ensure_(tonto,size(V,2)==size(self,1),"CPXMAT:back_transform ... incompatible sizes")
      v1 = size(V,1)
      o2 = size(self,2)
      call create_(W,v1,o2)
      call to_product_of_(W,V,self)  ! W = V self
      call to_product_of_(new,W,V,dagger_b=.true.)   ! new = V self V^dagger
      call destroy_(W)

   end subroutine

   subroutine compress_to_square(self,sq)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Compresses the hermitian matrix self to vector "sq". First comes the
    ! lower half of the real part, then lower half of the imaginary part.
      intent(in) :: self
      real(kind=kind(1.0d0)), dimension(:) :: sq
      integer(kind=kind(1)) :: dim1,i,j,ij

      call ensure_(tonto,is_square_(self),"CPXMAT:compress_to_square ... non-square matrix")
      call ensure_(tonto,size(sq)>=size(self),"CPXMAT:compress_to_square ... sq array too small")
      dim1 = size(self,1)
      ij = 0
      do i = 1,dim1
         do j = 1,i
            sq(ij+j) = real(self(i,j),kind=kind(1.0d0))
         end do
         ij = ij+i
      end do
      do i = 1,dim1
         do j = 1,i-1
            sq(ij+j) = aimag(self(i,j))
         end do
         ij = ij+i-1
      end do

   end subroutine

   subroutine uncompress_from_square(self,sq)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Uncompress the vector "sq" to a hermitian matrix assuming the lower half
    ! of the real part comes first, then the lower half of the imaginary part.
      real(kind=kind(1.0d0)), dimension(:) :: sq
      integer(kind=kind(1)) :: dim1,i,j,ij

      call ensure_(tonto,is_square_(self),"CPXMAT:uncompress_from_square ... non-square matrix")
      call ensure_(tonto,size(sq)>=size(self),"CPXMAT:uncompress_from_square ... sq array too small")
      dim1 = size(self,1)
      ij = 0
      do i = 1,dim1
         do j = 1,i
            self(i,j) = sq(ij+j)
            self(j,i) = sq(ij+j)
         end do
         ij = ij+i
      end do
      do i = 1,dim1
         do j = 1,i-1
            self(i,j) = self(i,j) + cmplx(0.0d0,sq(ij+j),kind=kind((1.0d0,1.0d0)))
            self(j,i) = self(j,i) - cmplx(0.0d0,sq(ij+j),kind=kind((1.0d0,1.0d0)))
         end do
         ij = ij+i-1
      end do

   end subroutine

   subroutine compress_to_triangle(self,tr)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Converts the upper triangle of matrix self to the triangle "tr".
      intent(in) :: self
      complex(kind=kind((1.0d0,1.0d0))), dimension(:) :: tr
      integer(kind=kind(1)) :: dim1,i,j,ij

      call ensure_(tonto,is_square_(self),"CPXMAT:compress_to_triangle ... non-square matrix")
      call ensure_(tonto,size(tr)>=size(self,1)*(size(self,1)+1)/2,"CPXMAT:compress_to_triangle ... tr array too small")
      dim1 = size(self,1)
      ij = 0
      do i = 1,dim1
         do j = 1,i
            tr(ij+j) = self(j,i)
         end do
         ij = ij+i
      end do

   end subroutine

   subroutine uncompress_from_triangle(self,tr)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Converts the triangle "tr" into the hermitian matrix "self".
    ! WARNING: won't work for symmetric matrices
      complex(kind=kind((1.0d0,1.0d0))), dimension(:) :: tr
      integer(kind=kind(1)) :: dim1,i,j,ij

      call ensure_(tonto,is_square_(self),"CPXMAT:uncompress_from_triangle ... non-square matrix")
      call ensure_(tonto,size(tr)>=tri_size_(self),"CPXMAT:uncompress_from_triangle ... tr array too small")
      dim1 = size(self,1)
      ij = 0
      do i = 1,dim1
         do j = 1,i
            self(j,i) =       tr(ij+j)
            self(i,j) = conjg(tr(ij+j))
         end do
         ij = ij+i
      end do

   end subroutine

   subroutine from_diagonal(self,d)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Converts the diagonal vector "d" to matrix "self".
      real(kind=kind(1.0d0)), dimension(:) :: d
      integer(kind=kind(1)) :: dim,i

      call ensure_(tonto,is_square_(self),"CPXMAT:from_diagonal ... non-square matrix")
      call ensure_(tonto,size(d)==size(self,1),"CPXMAT:from_diagonal ... wrong diagonal length")
      dim  = size(d)
      self = 0.0d0
      do i = 1,dim
         self(i,i) = d(i)
      end do

   end subroutine

   function tri_size(self) result(ltr)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Returns the size of the lower triangle needed to store self.
      intent(in) :: self
      integer(kind=kind(1)) :: ltr
      integer(kind=kind(1)) :: dim1

      call ensure_(tonto,is_square_(self),"CPXMAT:tri_size ... non-square matrix")
      dim1 = size(self,1)
      ltr = dim1*(dim1+1)/2

   end function

   subroutine to_unit_mat(self)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Set self to the unit matrix
      integer(kind=kind(1)) :: dim,i

      call ensure_(tonto,is_square_(self),"CPXMAT:to_unit_mat ... non-square matrix")
      dim = size(self,1)
      self = (0.0,0.0)
      do i = 1,dim
         self(i,i) = (1.0,0.0)
      end do

   end subroutine

   subroutine weight_diagonal(self,fac)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Weight the diagonals of self by "fac"
      complex(kind=kind((1.0d0,1.0d0))) :: fac
      integer(kind=kind(1)) :: dim,i

      call ensure_(tonto,is_square_(self),"CPXMAT:weight_diagonal ... non-square matrix")
      dim = size(self,1)
      do i = 1,dim
         self(i,i) = fac*self(i,i)
      end do

   end subroutine

   subroutine weight_diagonal_1(self,fac)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Weight the diagonals of self by "fac"
      real(kind=kind(1.0d0)) :: fac
      integer(kind=kind(1)) :: dim,i

      call ensure_(tonto,is_square_(self),"CPXMAT:weight_diagonal_1 ... non-square matrix")
      dim = size(self,1)
      do i = 1,dim
         self(i,i) = fac*self(i,i)
      end do

   end subroutine

   subroutine get_diagonal(self,diag)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Return the diagonals of self in "diag"
      complex(kind=kind((1.0d0,1.0d0))), dimension(:) :: diag
      integer(kind=kind(1)) :: dim,i

      call ensure_(tonto,size(diag)==min(size(self,1),size(self,2)),"CPXMAT:get_diagonal ... diag vector is incompatible")
      dim  = size(diag)
      do i = 1,dim
         diag(i) = self(i,i)
      end do

   end subroutine

   subroutine hermitian_fold(self)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Add the hermitian conjugate of the upper half of the
    ! matrix into its lower half.
      integer(kind=kind(1)) :: dim1,i,j

      call ensure_(tonto,is_square_(self),"CPXMAT:hermitian_fold ... non-square matrix")
      dim1 = size(self,1)
      do i = 1,dim1
         do j=1,i-1
            self(i,j) = self(i,j)+conjg(self(j,i))
         end do
         self(i,i) = real(self(i,i),kind=kind(1.0d0))
      end do

   end subroutine

   subroutine antihermitian_fold(self)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Subtract the hermitian conjugate of the upper half of the
    ! matrix into its lower half.
      integer(kind=kind(1)) :: dim1,i,j

      call ensure_(tonto,is_square_(self),"CPXMAT:antihermitian_fold ... non-square matrix")
      dim1 = size(self,1)
      do i = 1,dim1
         do j=1,i-1
            self(i,j) = self(i,j)-conjg(self(j,i))
         end do
         self(i,i) = 0.0d0
      end do

   end subroutine

   subroutine make_hermitian(self)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Make the upper half of self hermitian with respect to the lower half
      integer(kind=kind(1)) :: dim1,i,j

      call ensure_(tonto,is_square_(self),"CPXMAT:make_hermitian ... non-square matrix")
      dim1 = size(self,1)
      do i = 1,dim1
         do j = 1,i-1
            self(j,i) = conjg(self(i,j))
         end do
      end do
      do i = 1,dim1
         self(i,i) = real(self(i,i),kind=kind(1.0d0))
      end do

   end subroutine

   subroutine hermitian_reflect(self)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Make the upper half of self hermitian with respect
    ! to the lower half
      integer(kind=kind(1)) :: dim1,i,j

      call ensure_(tonto,is_square_(self),"CPXMAT:hermitian_reflect ... non-square matrix")
      dim1 = size(self,1)
      do i = 1,dim1
         do j = 1,i-1
            self(j,i) = conjg(self(i,j))
         end do
      end do
      do i = 1,dim1
         self(i,i) = real(self(i,i),kind=kind(1.0d0))
      end do

   end subroutine

   subroutine make_antihermitian(self)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Make the upper half of self anti-hermitian with respect
    ! to the lower half
      integer(kind=kind(1)) :: dim1,i,j

      call ensure_(tonto,is_square_(self),"CPXMAT:make_antihermitian ... non-square matrix")
      dim1 = size(self,1)
      do i = 1,dim1
         do j = 1,i-1
            self(j,i) = -conjg(self(i,j))
         end do
      end do
      do i = 1,dim1
         self(i,i) = 0.0d0
      end do

   end subroutine

   subroutine antihermitian_reflect(self)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Make the upper half of self anti-hermitian with respect
    ! to the lower half
      integer(kind=kind(1)) :: dim1,i,j

      call ensure_(tonto,is_square_(self),"CPXMAT:antihermitian_reflect ... non-square matrix")
      dim1 = size(self,1)
      do i = 1,dim1
         do j = 1,i-1
            self(j,i) = -conjg(self(i,j))
         end do
      end do
      do i = 1,dim1
         self(i,i) = 0.0d0
      end do

   end subroutine

!  ************************
!  Block returning routines
!  ************************

   function alpha_alpha(self) result(res)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! return the alpha-alpha sector of the matrix
      TARGET :: self
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: res
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:alpha_alpha ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:alpha_alpha ... not even-dimensioned")
      n = size(self,1)/2
      res => self(1:n,1:n)

   end function

   function beta_alpha(self) result(res)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! return the beta-alpha sector of the matrix
      TARGET :: self
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: res
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:beta_alpha ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:beta_alpha ... not even-dimensioned")
      n = size(self,1)/2
      res => self(n+1:2*n,1:n)

   end function

   function alpha_beta(self) result(res)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! return the alpha-beta sector of the matrix
      TARGET :: self
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: res
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:alpha_beta ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:alpha_beta ... not even-dimensioned")
      n = size(self,1)/2
      res => self(1:n,n+1:2*n)

   end function

   function beta_beta(self) result(res)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! return the beta-beta sector of the matrix
      TARGET :: self
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: res
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:beta_beta ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:beta_beta ... not even-dimensioned")
      n = size(self,1)/2
      res => self(n+1:2*n,n+1:2*n)

   end function

!  ***************
!  Set_to routines
!  ***************

   subroutine alpha_alpha_set_to(self,X,factor)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Set the alpha-alpha sector of the matrix to "X"
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), intent(in) :: X
      complex(kind=kind((1.0d0,1.0d0))), intent(in), optional :: factor
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:alpha_alpha_set_to ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:alpha_alpha_set_to ... not even-dimensioned")
      n = size(self,1)/2
      if (present(factor)) then; self(1:n,1:n) = factor*X
      else;                      self(1:n,1:n) = X
      end if

   end subroutine

   subroutine alpha_alpha_set_to_1(self,X,factor)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Set the alpha-alpha sector of the matrix to "X"
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), intent(in) :: X
      real(kind=kind(1.0d0)), intent(in) :: factor
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:alpha_alpha_set_to_1 ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:alpha_alpha_set_to_1 ... not even-dimensioned")
      n = size(self,1)/2
      self(1:n,1:n) = factor*X

   end subroutine

   subroutine alpha_alpha_set_to_2(self,X,factor)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Set the alpha-alpha sector of the matrix to "X"
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: X
      complex(kind=kind((1.0d0,1.0d0))), intent(in), optional :: factor
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:alpha_alpha_set_to_2 ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:alpha_alpha_set_to_2 ... not even-dimensioned")
      n = size(self,1)/2
      if (present(factor)) then; self(1:n,1:n) = factor*X
      else;                      self(1:n,1:n) = X
      end if

   end subroutine

   subroutine alpha_alpha_set_to_3(self,X,factor)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Set the alpha-alpha sector of the matrix to "X"
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: X
      real(kind=kind(1.0d0)), intent(in) :: factor
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:alpha_alpha_set_to_3 ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:alpha_alpha_set_to_3 ... not even-dimensioned")
      n = size(self,1)/2
      self(1:n,1:n) = factor*X

   end subroutine

   subroutine beta_alpha_set_to(self,X,factor)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Set the beta-alpha sector of the matrix to "X"
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), intent(in) :: X
      complex(kind=kind((1.0d0,1.0d0))), intent(in), optional :: factor
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:beta_alpha_set_to ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:beta_alpha_set_to ... not even-dimensioned")
      n = size(self,1)/2
      if (present(factor)) then; self(n+1:2*n,1:n) = factor*X
      else;                      self(n+1:2*n,1:n) = X
      end if

   end subroutine

   subroutine beta_alpha_set_to_1(self,X,factor)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Set the beta-alpha sector of the matrix to "X"
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), intent(in) :: X
      real(kind=kind(1.0d0)), intent(in) :: factor
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:beta_alpha_set_to_1 ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:beta_alpha_set_to_1 ... not even-dimensioned")
      n = size(self,1)/2
      self(n+1:2*n,1:n) = factor*X

   end subroutine

   subroutine beta_alpha_set_to_2(self,X,factor)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Set the beta-alpha sector of the matrix to "X"
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: X
      complex(kind=kind((1.0d0,1.0d0))), intent(in), optional :: factor
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:beta_alpha_set_to_2 ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:beta_alpha_set_to_2 ... not even-dimensioned")
      n = size(self,1)/2
      if (present(factor)) then; self(n+1:2*n,1:n) = factor*X
      else;                      self(n+1:2*n,1:n) = X
      end if

   end subroutine

   subroutine beta_alpha_set_to_3(self,X,factor)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Set the beta-alpha sector of the matrix to "X"
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: X
      real(kind=kind(1.0d0)), intent(in) :: factor
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:beta_alpha_set_to_3 ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:beta_alpha_set_to_3 ... not even-dimensioned")
      n = size(self,1)/2
      self(n+1:2*n,1:n) = factor*X

   end subroutine

   subroutine alpha_beta_set_to(self,X,factor)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Set the alpha-beta sector of the matrix to "X"
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), intent(in) :: X
      complex(kind=kind((1.0d0,1.0d0))), intent(in), optional :: factor
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:alpha_beta_set_to ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:alpha_beta_set_to ... not even-dimensioned")
      n = size(self,1)/2
      if (present(factor)) then; self(1:n,n+1:2*n) = factor*X
      else;                      self(1:n,n+1:2*n) = X
      end if

   end subroutine

   subroutine alpha_beta_set_to_1(self,X,factor)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Set the alpha-beta sector of the matrix to "X"
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), intent(in) :: X
      real(kind=kind(1.0d0)), intent(in) :: factor
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:alpha_beta_set_to_1 ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:alpha_beta_set_to_1 ... not even-dimensioned")
      n = size(self,1)/2
      self(1:n,n+1:2*n) = factor*X

   end subroutine

   subroutine alpha_beta_set_to_2(self,X,factor)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Set the alpha-beta sector of the matrix to "X"
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: X
      complex(kind=kind((1.0d0,1.0d0))), intent(in), optional :: factor
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:alpha_beta_set_to_2 ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:alpha_beta_set_to_2 ... not even-dimensioned")
      n = size(self,1)/2
      if (present(factor)) then; self(1:n,n+1:2*n) = factor*X
      else;                      self(1:n,n+1:2*n) = X
      end if

   end subroutine

   subroutine alpha_beta_set_to_3(self,X,factor)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Set the alpha-beta sector of the matrix to "X"
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: X
      real(kind=kind(1.0d0)), intent(in) :: factor
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:alpha_beta_set_to_3 ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:alpha_beta_set_to_3 ... not even-dimensioned")
      n = size(self,1)/2
      self(1:n,n+1:2*n) = factor*X

   end subroutine

   subroutine beta_beta_set_to(self,X,factor)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Set the beta-beta sector of the matrix to "X"
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), intent(in) :: X
      complex(kind=kind((1.0d0,1.0d0))), intent(in), optional :: factor
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:beta_beta_set_to ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:beta_beta_set_to ... not even-dimensioned")
      n = size(self,1)/2
      if (present(factor)) then; self(n+1:2*n,n+1:2*n) = factor*X
      else;                      self(n+1:2*n,n+1:2*n) = X
      end if

   end subroutine

   subroutine beta_beta_set_to_1(self,X,factor)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Set the beta-beta sector of the matrix to "X"
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), intent(in) :: X
      real(kind=kind(1.0d0)), intent(in) :: factor
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:beta_beta_set_to_1 ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:beta_beta_set_to_1 ... not even-dimensioned")
      n = size(self,1)/2
      self(n+1:2*n,n+1:2*n) = factor*X

   end subroutine

   subroutine beta_beta_set_to_2(self,X,factor)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Set the beta-beta sector of the matrix to "X"
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: X
      complex(kind=kind((1.0d0,1.0d0))), intent(in), optional :: factor
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:beta_beta_set_to_2 ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:beta_beta_set_to_2 ... not even-dimensioned")
      n = size(self,1)/2
      if (present(factor)) then; self(n+1:2*n,n+1:2*n) = factor*X
      else;                      self(n+1:2*n,n+1:2*n) = X
      end if

   end subroutine

   subroutine beta_beta_set_to_3(self,X,factor)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Set the beta-beta sector of the matrix to "X"
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: X
      real(kind=kind(1.0d0)), intent(in) :: factor
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:beta_beta_set_to_3 ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:beta_beta_set_to_3 ... not even-dimensioned")
      n = size(self,1)/2
      self(n+1:2*n,n+1:2*n) = factor*X

   end subroutine

!  ***************
!  Put_to routines
!  ***************

   subroutine alpha_alpha_put_to(self,X,factor)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Put the alpha-alpha sector of the matrix to "X"
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), intent(out) :: X
      complex(kind=kind((1.0d0,1.0d0))), intent(in), optional :: factor
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:alpha_alpha_put_to ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:alpha_alpha_put_to ... not even-dimensioned")
      n = size(self,1)/2
      if (present(factor)) then; X = factor*self(1:n,1:n)
      else;                      X = self(1:n,1:n)
      end if

   end subroutine

   subroutine alpha_alpha_put_to_1(self,X,factor)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Put the alpha-alpha sector of the matrix to "X"
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), intent(out) :: X
      real(kind=kind(1.0d0)) :: factor
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:alpha_alpha_put_to_1 ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:alpha_alpha_put_to_1 ... not even-dimensioned")
      n = size(self,1)/2
      X = factor*self(1:n,1:n)

   end subroutine

   subroutine alpha_alpha_put_to_2(self,X,factor)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Put the alpha-alpha sector of the matrix to "X"
      real(kind=kind(1.0d0)), dimension(:,:), intent(out) :: X
      complex(kind=kind((1.0d0,1.0d0))), intent(in), optional :: factor
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:alpha_alpha_put_to_2 ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:alpha_alpha_put_to_2 ... not even-dimensioned")
      n = size(self,1)/2
      if (present(factor)) then; X = factor*self(1:n,1:n)
      else;                      X = self(1:n,1:n)
      end if

   end subroutine

   subroutine alpha_alpha_put_to_3(self,X,factor)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Put the alpha-alpha sector of the matrix to "X"
      real(kind=kind(1.0d0)), dimension(:,:), intent(out) :: X
      real(kind=kind(1.0d0)), intent(in) :: factor
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:alpha_alpha_put_to_3 ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:alpha_alpha_put_to_3 ... not even-dimensioned")
      n = size(self,1)/2
      X = factor*self(1:n,1:n)

   end subroutine

   subroutine beta_alpha_put_to(self,X,factor)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Put the beta-alpha sector of the matrix to "X"
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), intent(out) :: X
      complex(kind=kind((1.0d0,1.0d0))), intent(in), optional :: factor
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:beta_alpha_put_to ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:beta_alpha_put_to ... not even-dimensioned")
      n = size(self,1)/2
      if (present(factor)) then; X = factor*self(n+1:2*n,1:n)
      else;                      X = self(n+1:2*n,1:n)
      end if

   end subroutine

   subroutine beta_alpha_put_to_1(self,X,factor)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Put the beta-alpha sector of the matrix to "X"
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), intent(out) :: X
      real(kind=kind(1.0d0)), intent(in) :: factor
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:beta_alpha_put_to_1 ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:beta_alpha_put_to_1 ... not even-dimensioned")
      n = size(self,1)/2
      X = factor*self(n+1:2*n,1:n)

   end subroutine

   subroutine beta_alpha_put_to_2(self,X,factor)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Put the beta-alpha sector of the matrix to "X"
      real(kind=kind(1.0d0)), dimension(:,:), intent(out) :: X
      complex(kind=kind((1.0d0,1.0d0))), intent(in), optional :: factor
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:beta_alpha_put_to_2 ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:beta_alpha_put_to_2 ... not even-dimensioned")
      n = size(self,1)/2
      if (present(factor)) then; X = factor*self(n+1:2*n,1:n)
      else;                      X = self(n+1:2*n,1:n)
      end if

   end subroutine

   subroutine beta_alpha_put_to_3(self,X,factor)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Put the beta-alpha sector of the matrix to "X"
      real(kind=kind(1.0d0)), dimension(:,:), intent(out) :: X
      real(kind=kind(1.0d0)), intent(in) :: factor
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:beta_alpha_put_to_3 ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:beta_alpha_put_to_3 ... not even-dimensioned")
      n = size(self,1)/2
      X = factor*self(n+1:2*n,1:n)

   end subroutine

   subroutine alpha_beta_put_to(self,X,factor)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Put the alpha-beta sector of the matrix to "X"
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), intent(out) :: X
      complex(kind=kind((1.0d0,1.0d0))), intent(in), optional :: factor
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:alpha_beta_put_to ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:alpha_beta_put_to ... not even-dimensioned")
      n = size(self,1)/2
      if (present(factor)) then; X = factor*self(1:n,n+1:2*n)
      else;                      X = self(1:n,n+1:2*n)
      end if

   end subroutine

   subroutine alpha_beta_put_to_1(self,X,factor)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Put the alpha-beta sector of the matrix to "X"
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), intent(out) :: X
      real(kind=kind(1.0d0)), intent(in) :: factor
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:alpha_beta_put_to_1 ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:alpha_beta_put_to_1 ... not even-dimensioned")
      n = size(self,1)/2
      X = factor*self(1:n,n+1:2*n)

   end subroutine

   subroutine alpha_beta_put_to_2(self,X,factor)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Put the alpha-beta sector of the matrix to "X"
      real(kind=kind(1.0d0)), dimension(:,:), intent(out) :: X
      complex(kind=kind((1.0d0,1.0d0))), intent(in), optional :: factor
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:alpha_beta_put_to_2 ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:alpha_beta_put_to_2 ... not even-dimensioned")
      n = size(self,1)/2
      if (present(factor)) then; X = factor*self(1:n,n+1:2*n)
      else;                      X = self(1:n,n+1:2*n)
      end if

   end subroutine

   subroutine alpha_beta_put_to_3(self,X,factor)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Put the alpha-beta sector of the matrix to "X"
      real(kind=kind(1.0d0)), dimension(:,:), intent(out) :: X
      real(kind=kind(1.0d0)), intent(in) :: factor
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:alpha_beta_put_to_3 ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:alpha_beta_put_to_3 ... not even-dimensioned")
      n = size(self,1)/2
      X = factor*self(1:n,n+1:2*n)

   end subroutine

   subroutine beta_beta_put_to(self,X,factor)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Put the beta-beta sector of the matrix to "X"
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), intent(out) :: X
      complex(kind=kind((1.0d0,1.0d0))), intent(in), optional :: factor
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:beta_beta_put_to ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:beta_beta_put_to ... not even-dimensioned")
      n = size(self,1)/2
      if (present(factor)) then; X = factor*self(n+1:2*n,n+1:2*n)
      else;                      X = self(n+1:2*n,n+1:2*n)
      end if

   end subroutine

   subroutine beta_beta_put_to_1(self,X,factor)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Put the beta-beta sector of the matrix to "X"
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), intent(out) :: X
      real(kind=kind(1.0d0)), intent(in) :: factor
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:beta_beta_put_to_1 ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:beta_beta_put_to_1 ... not even-dimensioned")
      n = size(self,1)/2
      X = factor*self(n+1:2*n,n+1:2*n)

   end subroutine

   subroutine beta_beta_put_to_2(self,X,factor)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Put the beta-beta sector of the matrix to "X"
      real(kind=kind(1.0d0)), dimension(:,:), intent(out) :: X
      complex(kind=kind((1.0d0,1.0d0))), intent(in), optional :: factor
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:beta_beta_put_to_2 ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:beta_beta_put_to_2 ... not even-dimensioned")
      n = size(self,1)/2
      if (present(factor)) then; X = factor*self(n+1:2*n,n+1:2*n)
      else;                      X = self(n+1:2*n,n+1:2*n)
      end if

   end subroutine

   subroutine beta_beta_put_to_3(self,X,factor)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Put the beta-beta sector of the matrix to "X"
      real(kind=kind(1.0d0)), dimension(:,:), intent(out) :: X
      real(kind=kind(1.0d0)), intent(in) :: factor
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:beta_beta_put_to_3 ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:beta_beta_put_to_3 ... not even-dimensioned")
      n = size(self,1)/2
      X = factor*self(n+1:2*n,n+1:2*n)

   end subroutine

!  *************
!  plus routines
!  *************

   subroutine alpha_alpha_plus(self,X,factor)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Add "X" to the alpha-alpha sector of the matrix
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), intent(in) :: X
      complex(kind=kind((1.0d0,1.0d0))), intent(in), optional :: factor
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:alpha_alpha_plus ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:alpha_alpha_plus ... not even-dimensioned")
      n = size(self,1)/2
      if (present(factor)) then; self(1:n,1:n) = self(1:n,1:n) + factor*X
      else;                      self(1:n,1:n) = self(1:n,1:n) + X
      end if

   end subroutine

   subroutine alpha_alpha_plus_1(self,X,factor)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Add "X" to the alpha-alpha sector of the matrix
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), intent(in) :: X
      real(kind=kind(1.0d0)), intent(in) :: factor
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:alpha_alpha_plus_1 ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:alpha_alpha_plus_1 ... not even-dimensioned")
      n = size(self,1)/2
      self(1:n,1:n) = self(1:n,1:n) + factor*X

   end subroutine

   subroutine alpha_alpha_plus_2(self,X,factor)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Add "X" to the alpha-alpha sector of the matrix
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: X
      complex(kind=kind((1.0d0,1.0d0))), intent(in), optional :: factor
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:alpha_alpha_plus_2 ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:alpha_alpha_plus_2 ... not even-dimensioned")
      n = size(self,1)/2
      if (present(factor)) then; self(1:n,1:n) = self(1:n,1:n) + factor*X
      else;                      self(1:n,1:n) = self(1:n,1:n) + X
      end if

   end subroutine

   subroutine alpha_alpha_plus_3(self,X,factor)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Add "X" to the alpha-alpha sector of the matrix
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: X
      real(kind=kind(1.0d0)), intent(in) :: factor
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:alpha_alpha_plus_3 ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:alpha_alpha_plus_3 ... not even-dimensioned")
      n = size(self,1)/2
      self(1:n,1:n) = self(1:n,1:n) + factor*X

   end subroutine

   subroutine beta_alpha_plus(self,X,factor)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Add "X" to the beta-alpha sector of the matrix
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), intent(in) :: X
      complex(kind=kind((1.0d0,1.0d0))), intent(in), optional :: factor
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:beta_alpha_plus ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:beta_alpha_plus ... not even-dimensioned")
      n = size(self,1)/2
      if (present(factor)) then; self(n+1:2*n,1:n) = self(n+1:2*n,1:n) + factor*X
      else;                      self(n+1:2*n,1:n) = self(n+1:2*n,1:n) + X
      end if

   end subroutine

   subroutine beta_alpha_plus_1(self,X,factor)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Add "X" to the beta-alpha sector of the matrix
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), intent(in) :: X
      real(kind=kind(1.0d0)), intent(in) :: factor
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:beta_alpha_plus_1 ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:beta_alpha_plus_1 ... not even-dimensioned")
      n = size(self,1)/2
      self(n+1:2*n,1:n) = self(n+1:2*n,1:n) + factor*X

   end subroutine

   subroutine beta_alpha_plus_2(self,X,factor)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Add "X" to the beta-alpha sector of the matrix
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: X
      complex(kind=kind((1.0d0,1.0d0))), intent(in), optional :: factor
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:beta_alpha_plus_2 ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:beta_alpha_plus_2 ... not even-dimensioned")
      n = size(self,1)/2
      if (present(factor)) then; self(n+1:2*n,1:n) = self(n+1:2*n,1:n) + factor*X
      else;                      self(n+1:2*n,1:n) = self(n+1:2*n,1:n) + X
      end if

   end subroutine

   subroutine beta_alpha_plus_3(self,X,factor)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Add "X" to the beta-alpha sector of the matrix
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: X
      real(kind=kind(1.0d0)), intent(in) :: factor
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:beta_alpha_plus_3 ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:beta_alpha_plus_3 ... not even-dimensioned")
      n = size(self,1)/2
      self(n+1:2*n,1:n) = self(n+1:2*n,1:n) + factor*X

   end subroutine

   subroutine alpha_beta_plus(self,X,factor)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Add "X" to the alpha-beta sector of the matrix
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), intent(in) :: X
      complex(kind=kind((1.0d0,1.0d0))), intent(in), optional :: factor
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:alpha_beta_plus ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:alpha_beta_plus ... not even-dimensioned")
      n = size(self,1)/2
      if (present(factor)) then; self(1:n,n+1:2*n) = self(1:n,n+1:2*n) + factor*X
      else;                      self(1:n,n+1:2*n) = self(1:n,n+1:2*n) + X
      end if

   end subroutine

   subroutine alpha_beta_plus_1(self,X,factor)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Add "X" to the alpha-beta sector of the matrix
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), intent(in) :: X
      real(kind=kind(1.0d0)), intent(in) :: factor
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:alpha_beta_plus_1 ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:alpha_beta_plus_1 ... not even-dimensioned")
      n = size(self,1)/2
      self(1:n,n+1:2*n) = self(1:n,n+1:2*n) + factor*X

   end subroutine

   subroutine alpha_beta_plus_2(self,X,factor)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Add "X" to the alpha-beta sector of the matrix
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: X
      complex(kind=kind((1.0d0,1.0d0))), intent(in), optional :: factor
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:alpha_beta_plus_2 ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:alpha_beta_plus_2 ... not even-dimensioned")
      n = size(self,1)/2
      if (present(factor)) then; self(1:n,n+1:2*n) = self(1:n,n+1:2*n) + factor*X
      else;                      self(1:n,n+1:2*n) = self(1:n,n+1:2*n) + X
      end if

   end subroutine

   subroutine alpha_beta_plus_3(self,X,factor)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Add "X" to the alpha-beta sector of the matrix
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: X
      real(kind=kind(1.0d0)), intent(in) :: factor
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:alpha_beta_plus_3 ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:alpha_beta_plus_3 ... not even-dimensioned")
      n = size(self,1)/2
      self(1:n,n+1:2*n) = self(1:n,n+1:2*n) + factor*X

   end subroutine

   subroutine beta_beta_plus(self,X, factor)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Add "X" to the beta-beta sector of the matrix
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), intent(in) :: X
      complex(kind=kind((1.0d0,1.0d0))), intent(in), optional :: factor
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:beta_beta_plus ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:beta_beta_plus ... not even-dimensioned")
      n = size(self,1)/2
      if (present(factor)) then; self(n+1:2*n,n+1:2*n) = self(n+1:2*n,n+1:2*n) + factor*X
      else;                      self(n+1:2*n,n+1:2*n) = self(n+1:2*n,n+1:2*n) + X
      end if

   end subroutine

   subroutine beta_beta_plus_1(self,X, factor)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Add "X" to the beta-beta sector of the matrix
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), intent(in) :: X
      real(kind=kind(1.0d0)), intent(in) :: factor
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:beta_beta_plus_1 ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:beta_beta_plus_1 ... not even-dimensioned")
      n = size(self,1)/2
      self(n+1:2*n,n+1:2*n) = self(n+1:2*n,n+1:2*n) + factor*X

   end subroutine

   subroutine beta_beta_plus_2(self,X,factor)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Add "X" to the beta-beta sector of the matrix
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: X
      complex(kind=kind((1.0d0,1.0d0))), intent(in), optional :: factor
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:beta_beta_plus_2 ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:beta_beta_plus_2 ... not even-dimensioned")
      n = size(self,1)/2
      if (present(factor)) then; self(n+1:2*n,n+1:2*n) = self(n+1:2*n,n+1:2*n) + factor*X
      else;                      self(n+1:2*n,n+1:2*n) = self(n+1:2*n,n+1:2*n) + X
      end if

   end subroutine

   subroutine beta_beta_plus_3(self,X,factor)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Add "X" to the beta-beta sector of the matrix
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: X
      real(kind=kind(1.0d0)), intent(in) :: factor
      integer(kind=kind(1)) :: n

      call ensure_(tonto,is_square_(self),"CPXMAT:beta_beta_plus_3 ... non-square matrix")
      call ensure_(tonto,is_even_(size(self,1)),"CPXMAT:beta_beta_plus_3 ... not even-dimensioned")
      n = size(self,1)/2
      self(n+1:2*n,n+1:2*n) = self(n+1:2*n,n+1:2*n) + factor*X

   end subroutine

   subroutine schmidt_orthonormalise(self,S,scale)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Schmidt ortyhonormalise the column vectors in "self" using "S" as the
    ! metric.  If "scale" is present, it is set to the product of the
    ! normalisation factors used to normalise each column after the Schmidt
    ! procedure.
     target :: self
     real(kind=kind(1.0d0)), optional :: scale
     real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: S
     integer(kind=kind(1)) :: dim,n,o
     complex(kind=kind((1.0d0,1.0d0))) :: fac
     real(kind=kind(1.0d0)) :: norm
     complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: new,old

     call ensure_(tonto,is_square_(self),"CPXMAT:schmidt_orthonormalise ... non-square matrix")
     call ensure_(tonto,is_square_(S),"CPXMAT:schmidt_orthonormalise ... non-square matrix")
     call ensure_(tonto,size(self,1)==size(S,1),"CPXMAT:schmidt_orthonormalise ... matrices not same size")
     call ensure_(tonto,.not. is_zero_(self),"CPXMAT:schmidt_orthonormalise ... self is zero matrix")
     if (present(scale)) scale = 1.0d0
     dim = size(self,1)
     do n = 1,dim
        new => self(:,n)
        do o = 1,n-1
           old => self(:,o)
           fac = dot_(S,old,new)
           new = new - fac*old
        end do
        norm = dot_(S,new,new)
        call ensure_(tonto,norm>10.0d0**(-10),"CPXMAT:schmidt_orthonormalise ... linear dependence in vector "//to_str_(n))
        norm = 1/sqrt(norm)
        new = new*norm
        if (present(scale)) scale = scale*norm
     end do

   end subroutine

   subroutine symmetrically_orthonormalise(self,S)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! Symmetrically orthonormalise the column vectors in "self" using "S" as the
    ! metric.
     real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: S
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: SS,SI
     integer(kind=kind(1)) :: dim

     call ensure_(tonto,is_square_(self),"CPXMAT:symmetrically_orthonormalise ... non-square matrix")
     call ensure_(tonto,is_same_shape_as_(self,S),"CPXMAT:symmetrically_orthonormalise ... non-square matrix")
     dim = size(S,1)
     call create_(SS,dim,dim)
     call create_(SI,dim,dim)
     SS = S
     call change_basis_(SS,self)
     call to_inverse_sqrt_(SI,SS)
     call to_product_of_(SS,self,SI)
     self = SS
     call destroy_(SI)
     call destroy_(SS)

   end subroutine

   subroutine to_sqrt(self,R)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! self = sqrt(R), cannot have R=self
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: R
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: evec
      real(kind=kind(1.0d0)), dimension(:), pointer :: eval
      complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: veci,vecj
      integer(kind=kind(1)) :: d,i,j
      real(kind=kind(1.0d0)) :: temp

      d = size(R,1)
      call create_(eval,d)
      call create_(evec,d,d)
      call solve_eigenproblem_(R,eval,evec)
      do i = 1,d
         temp = eval(i)
         if (temp <= 0.0d0) then
           call warn_(tonto,"CPXMAT:to_sqrt ... non-positive eigenvalue, " // trim(to_str_(temp,"e15.8")))
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
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: self
    ! self = sqrt(R)^(-1), cannot have R=self
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: R
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: evec
      real(kind=kind(1.0d0)), dimension(:), pointer :: eval
      complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: veci,vecj
      integer(kind=kind(1)) :: d,i,j
      character(128) :: val
      real(kind=kind(1.0d0)) :: temp

      d = size(R,1)
      call create_(eval,d)
      call create_(evec,d,d)
      call solve_eigenproblem_(R,eval,evec)
      do i = 1,d
         temp = eval(i)
         val = to_str_(temp,"e15.8")
         call warn_if_(tonto,temp<=0.0d0,"CPXMAT:to_inverse_sqrt ... non-positive eigenvalue, "// trim(val))
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

end
