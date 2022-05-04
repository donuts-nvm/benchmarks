!---------------------------------------------------------------------------
!
!  INTMAT: Integer matrix operations ...
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
! $Id: intmat.foo,v 1.8.2.2 2003/11/13 05:35:09 reaper Exp $
!---------------------------------------------------------------------------

module INTMAT_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

   use REALVEC_MODULE, only: in_range_
   use REALVEC_MODULE, only: create_
   use REALVEC_MODULE, only: destroy_

   use INTVEC_MODULE, only: same_as_
   use INTVEC_MODULE, only: create_
   use INTVEC_MODULE, only: destroy_

   use INT_MODULE, only: n_comp_sum_

   use BINMAT_MODULE, only: create_
   use BINMAT_MODULE, only: destroy_

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

   public    antisymmetric_fold_
   interface antisymmetric_fold_
      module procedure antisymmetric_fold
   end interface

   public    append_columns_
   interface append_columns_
      module procedure append_columns
   end interface

   public    to_gaussian_xyz_powers_
   interface to_gaussian_xyz_powers_
      module procedure to_gaussian_xyz_powers
      module procedure to_gaussian_xyz_powers_1
   end interface

   public    compress_to_triangle_
   interface compress_to_triangle_
      module procedure compress_to_triangle
   end interface

   public    set_to_
   interface set_to_
      module procedure set_to
   end interface

   public    plus_scaled_product_of_
   interface plus_scaled_product_of_
      module procedure plus_scaled_product_of
   end interface

   public    to_inverse_matrix_
   interface to_inverse_matrix_
      module procedure to_inverse_matrix
   end interface

   public    is_square_
   interface is_square_
      module procedure is_square
   end interface

   public    plus_product_of_
   interface plus_product_of_
      module procedure plus_product_of
   end interface

   public    same_as_
   interface same_as_
      module procedure same_as
   end interface

   public    tri_size_
   interface tri_size_
      module procedure tri_size
   end interface

   public    is_symmetric_
   interface is_symmetric_
      module procedure is_symmetric
   end interface

   public    weight_diagonal_
   interface weight_diagonal_
      module procedure weight_diagonal
   end interface

   public    to_product_of_
   interface to_product_of_
      module procedure to_product_of
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

   public    sum_row_vectors_
   interface sum_row_vectors_
      module procedure sum_row_vectors
   end interface

   public    antisymmetric_reflect_
   interface antisymmetric_reflect_
      module procedure antisymmetric_reflect
   end interface

   public    has_unit_diagonal_
   interface has_unit_diagonal_
      module procedure has_unit_diagonal
   end interface

   public    is_diagonal_
   interface is_diagonal_
      module procedure is_diagonal
   end interface

   public    shrink_
   interface shrink_
      module procedure shrink
   end interface

   public    shrink_columns_
   interface shrink_columns_
      module procedure shrink_columns
   end interface

   public    zero_off_diagonal_
   interface zero_off_diagonal_
      module procedure zero_off_diagonal
   end interface

   public    in_range_
   interface in_range_
      module procedure in_range
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

   public    to_scaled_product_of_
   interface to_scaled_product_of_
      module procedure to_scaled_product_of
   end interface

   public    get_column_norms_
   interface get_column_norms_
      module procedure get_column_norms
   end interface

   public    trace_product_with_
   interface trace_product_with_
      module procedure trace_product_with
   end interface

   public    create_
   interface create_
      module procedure create
      module procedure create_1
      module procedure create_2
   end interface

   public    add_to_diagonal_
   interface add_to_diagonal_
      module procedure add_to_diagonal
   end interface

   public    all_in_range_
   interface all_in_range_
      module procedure all_in_range
   end interface

   public    create_copy_
   interface create_copy_
      module procedure create_copy
   end interface

   public    append_column_
   interface append_column_
      module procedure append_column
   end interface

   public    zero_diagonal_
   interface zero_diagonal_
      module procedure zero_diagonal
   end interface

   public    plus_scaled_imat_
   interface plus_scaled_imat_
      module procedure plus_scaled_imat
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

   public    max_abs_column_difference_
   interface max_abs_column_difference_
      module procedure max_abs_column_difference
   end interface

   public    range_
   interface range_
      module procedure range
   end interface

   public    to_scaled_imat_
   interface to_scaled_imat_
      module procedure to_scaled_imat
   end interface

   public    symmetric_fold_
   interface symmetric_fold_
      module procedure symmetric_fold
   end interface

   public    get_diagonal_
   interface get_diagonal_
      module procedure get_diagonal
   end interface

   public    expand_
   interface expand_
      module procedure expand
   end interface

   public    destroy_
   interface destroy_
      module procedure destroy
   end interface

   public    column_norms_
   interface column_norms_
      module procedure column_norms
   end interface

   public    is_transposed_shape_of_
   interface is_transposed_shape_of_
      module procedure is_transposed_shape_of
   end interface

   public    mean_column_vector_
   interface mean_column_vector_
      module procedure mean_column_vector
   end interface

   public    expand_columns_
   interface expand_columns_
      module procedure expand_columns
   end interface

   public    is_unit_matrix_
   interface is_unit_matrix_
      module procedure is_unit_matrix
   end interface

   public    make_diagonally_dominant_
   interface make_diagonally_dominant_
      module procedure make_diagonally_dominant
   end interface

   public    uncompress_from_triangle_
   interface uncompress_from_triangle_
      module procedure uncompress_from_triangle
   end interface

   public    is_same_shape_as_
   interface is_same_shape_as_
      module procedure is_same_shape_as
   end interface

   public    bin_XY_data_
   interface bin_XY_data_
      module procedure bin_XY_data
   end interface

   public    set_diagonal_
   interface set_diagonal_
      module procedure set_diagonal
   end interface

   public    to_unit_matrix_
   interface to_unit_matrix_
      module procedure to_unit_matrix
   end interface

contains

   subroutine create(self,dim1,dim2)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Create a matrix with the given dimensions
      pointer :: self
      integer(kind=kind(1)), intent(in) :: dim1,dim2
    ! The following code is inherited from INTRINSICMAT

      nullify(self)
      allocate(self(dim1,dim2))

   end subroutine

   subroutine create_1(self,lb1,ub1,lb2,ub2)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Create a matrix with the given dimensions
      pointer :: self
      integer(kind=kind(1)), intent(in) :: lb1,ub1,lb2,ub2
    ! The following code is inherited from INTRINSICMAT

      nullify(self)
      allocate(self(lb1:ub1,lb2:ub2))

   end subroutine

   subroutine create_2(self,bounds1,bounds2)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Create a matrix with the specified bounds for each dimension
      pointer :: self
      integer(kind=kind(1)), dimension(:), intent(in) :: bounds1,bounds2
    ! The following code is inherited from INTRINSICMAT

      call create_(self,bounds1(1),bounds1(2),bounds2(1),bounds2(2))

   end subroutine

   subroutine create_copy(self,matrix)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Create a replica copy of matrix
      pointer :: self
      integer(kind=kind(1)), dimension(:,:), intent(in) :: matrix
    ! The following code is inherited from INTRINSICMAT

      call create_(self,lbound(matrix,1),ubound(matrix,1), &
              lbound(matrix,2),ubound(matrix,2)  )
      self = matrix

   end subroutine

   subroutine destroy(self)
    integer(kind=kind(1)), dimension(:,:) :: self
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

   function is_square(self) result(res)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Returns .true. if the matrix is square
      intent(in) :: self
      logical(kind=kind(.true.)) :: res
    ! The following code is inherited from INTRINSICMAT

      res = size(self,1)==size(self,2)

   end function

   function is_diagonal(self) result(res)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Returns .true. if the matrix "self" is a diagonal matrix
      intent(in) :: self
      logical(kind=kind(.true.)) :: res
      integer(kind=kind(1)) :: dim,i,j
      logical(kind=kind(.true.)) :: off_diagonal_is_zero

      call ensure_(tonto,is_square_(self),"INTMAT:is_diagonal ... Non-square matrix")
      dim = size(self,1)
      res = .true.
      do i = 1,dim
      do j = 1,dim
         if (i==j) cycle
         off_diagonal_is_zero = self(i,j)==0
         if (off_diagonal_is_zero) cycle
         res = .false.
           return
      end do
      end do

   end function

   function has_unit_diagonal(self) result(res)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Returns .true. if the matrix "self" has 1's as diagonal elements
      intent(in) :: self
      logical(kind=kind(.true.)) :: res
      integer(kind=kind(1)) :: i
      logical(kind=kind(.true.)) :: diagonal_is_one

      call ensure_(tonto,is_square_(self),"INTMAT:has_unit_diagonal ... Non-square matrix")
      res = .true.
      do i = 1,size(self,1)
         diagonal_is_one = (1 - self(i,i))==0
         if (diagonal_is_one) cycle
         res = .false.
         exit
      end do

   end function

   function is_unit_matrix(self) result(res)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Returns .true. if the matrix "self" is the unit matrix
      intent(in) :: self
      logical(kind=kind(.true.)) :: res

      call ensure_(tonto,is_square_(self),"INTMAT:is_unit_matrix ... Non-square matrix")
      res = is_diagonal_(self) .and. has_unit_diagonal_(self)

   end function

   function is_inversion_matrix(self) result(res)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Returns .true. if the matrix "self" is an inversion matrix
    ! i.e. minus the unit matrix
      intent(in) :: self
      logical(kind=kind(.true.)) :: res
      integer(kind=kind(1)) :: dim,i
      logical(kind=kind(.true.)) :: diagonal_is_minus_one

      call ensure_(tonto,is_square_(self),"INTMAT:is_inversion_matrix ... Non-square matrix")
      dim = size(self,1)
      res = .true.
      do i = 1,dim
         diagonal_is_minus_one = (self(i,i)+1)==0
         if (diagonal_is_minus_one) cycle
         res = .false.
         exit
      end do
      if (res) res = is_diagonal_(self)

   end function

   function is_symmetric(self) result(res)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Returns .true. if the matrix "self" is a symmetric matrix
      intent(in) :: self
      logical(kind=kind(.true.)) :: res
      integer(kind=kind(1)) :: dim,i,j
      integer(kind=kind(1)) :: diff

      call ensure_(tonto,is_square_(self),"INTMAT:is_symmetric ... Non-square matrix")
      dim = size(self,1)
      res = .true.
      do i = 1,dim
      do j = 1,i-1
         diff = abs(self(i,j)-self(j,i))
         if (diff==0) cycle
         res = .false.
           return
      end do
      end do

   end function

   pure function is_same_shape_as(self,b) result(res)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Returns .true. if the matrix "b" has the same shape as self
      intent(in) :: self
      integer(kind=kind(1)), dimension(:,:), intent(in) :: b
      logical(kind=kind(.true.)) :: res
    ! The following code is inherited from INTRINSICMAT
      res = size(self,1)==size(b,1) .and. size(self,2)==size(b,2)

   end function

   pure function is_transposed_shape_of(self,b) result(res)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Returns .true. if the matrix "b" is the transposed shape of self
      intent(in) :: self
      integer(kind=kind(1)), dimension(:,:), intent(in) :: b
      logical(kind=kind(.true.)) :: res
    ! The following code is inherited from INTRINSICMAT
      res = size(self,1)==size(b,2) .and. size(self,2)==size(b,1)

   end function

   function equals(self,b) result(res)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Check if the matrix is the same as "b".
      intent(in) :: self
      integer(kind=kind(1)), dimension(:,:), intent(in) :: b
      logical(kind=kind(.true.)) :: res

      res = same_as_(self,b)

   end function

   function same_as(self,b) result(res)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Check if the matrix is the same as "b".
      intent(in) :: self
      integer(kind=kind(1)), dimension(:,:), intent(in) :: b
      logical(kind=kind(.true.)) :: res
      integer(kind=kind(1)) :: i,del

      call ensure_(tonto,is_same_shape_as_(self,b),"INTMAT:same_as ... incompatible dimensions")
      res =  .true.
      do i = 1,size(self,2)
         del = sum( abs(self(:,i)-b(:,i)) )
         if (del==0) cycle
         res = .false.
         exit
      end do

   end function

   function has_column(self,c,col) result(res)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Returns .true. if the matrix "self" has a column "c". If present, the
    ! first matching column index "col" is also returned.
      intent(in) :: self
      integer(kind=kind(1)), dimension(:), intent(in) :: c
      integer(kind=kind(1)), optional :: col
      logical(kind=kind(.true.)) :: res
      integer(kind=kind(1)) :: n

      call ensure_(tonto,size(c)==size(self,1),"INTMAT:has_column ... incompatible column size")
      res = .false.
      do n = 1,size(self,2)
         res = same_as_(self(:,n),c)
         if (.not. res) cycle
         if (present(col)) col = n
         exit
      end do

   end function

   function all_in_range(self,range) result(res)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Return .true. if all values of self are within the specified "range".
      integer(kind=kind(1)), dimension(2) :: range
      logical(kind=kind(.true.)) :: res

      res = all(range(1) <= self .and. self <= range(2))

   end function

   function in_range(self,range) result(res)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Return element ij as .true. if self(i,j) is within the specified "range".
      integer(kind=kind(1)), dimension(2) :: range
      logical(kind=kind(.true.)), dimension(size(self,1),size(self,2)) :: res

      res = (range(1) <= self .and. self <= range(2))

   end function

   function range(self) result(res)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Return the range (smallest and largest value) of self.
    ! NOTE: Returns a real.
      real(kind=kind(1.0d0)), dimension(2) :: res

      res(1) = minval(self)
      res(2) = maxval(self)

   end function

   subroutine shrink(self,dim1,dim2)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Shrinks self to dimension dim1xdim2.  Contents are retained.
     pointer :: self
     integer(kind=kind(1)), intent(in) :: dim1,dim2
    ! The following code is inherited from INTRINSICMAT
     integer(kind=kind(1)), dimension(:,:), pointer :: old

     call ensure_(tonto,associated(self),"INTMAT:shrink ... matrix not allocated")
     call ensure_(tonto,dim1<=size(self,1),"INTMAT:shrink ... 1st dimension given is too large.")
     call ensure_(tonto,dim2<=size(self,2),"INTMAT:shrink ... 2nd dimension given is too large.")
     if (dim1==size(self,1) .and. dim2==size(self,2)) then;   return; end if
     old => self
     nullify(self)
     call create_(self,dim1,dim2)
     self=old(1:dim1,1:dim2)
     call destroy_(old)

   end subroutine

   subroutine shrink_columns(self,dim2)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Shrinks columns of self to dimension dim2. Contents are retained.
     pointer :: self
     integer(kind=kind(1)), intent(in) :: dim2
    ! The following code is inherited from INTRINSICMAT
     integer(kind=kind(1)), dimension(:,:), pointer :: old
     integer(kind=kind(1)) :: dim1

     call ensure_(tonto,associated(self),"INTMAT:shrink_columns ... matrix not allocated")
     call ensure_(tonto,dim2<=size(self,2),"INTMAT:shrink_columns ... 2nd dimension given is too large.")
     if (dim2==size(self,2)) then;   return; end if
     dim1 = size(self,1)
     old => self
     nullify(self)
     call create_(self,dim1,dim2)
     self=old(1:dim1,1:dim2)
     call destroy_(old)

   end subroutine

   subroutine expand(self,dim1,dim2)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Expands self to dimension dim1xdim2.  Contents are retained.
     pointer :: self
     integer(kind=kind(1)), intent(in) :: dim1,dim2
    ! The following code is inherited from INTRINSICMAT
     integer(kind=kind(1)), dimension(:,:), pointer :: old
     integer(kind=kind(1)) :: old_size1,old_size2

     if (.not. associated(self)) then
       call create_(self,0,0)
     else
     call ensure_(tonto,dim1>=size(self,1),"INTMAT:expand ... 1st dimension given is too small")
     call ensure_(tonto,dim2>=size(self,2),"INTMAT:expand ... 2nd dimension given is too small")
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
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Expands the columns self to dim2.  Contents are retained.
     pointer :: self
     integer(kind=kind(1)), intent(in) :: dim2
    ! The following code is inherited from INTRINSICMAT
     integer(kind=kind(1)) :: dim1,old_dim2
     integer(kind=kind(1)), dimension(:,:), pointer :: old

     call ensure_(tonto,associated(self),"INTMAT:expand_columns ... matrix not allocated")
     call ensure_(tonto,dim2>=size(self,2),"INTMAT:expand_columns ... 2nd dimension given is too small")
     dim1 = size(self,1)
     old => self
     old_dim2 = size(old,2)
     nullify(self)
     call create_(self,dim1,dim2)
     self(:,1:old_dim2) = old
     call destroy_(old)

   end subroutine

   subroutine append_column(self,col)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Append the column "col" onto the end of self.
     pointer :: self
     integer(kind=kind(1)), dimension(:) :: col
     integer(kind=kind(1)) :: old_dim2,new_dim2

     call ensure_(tonto,associated(self),"INTMAT:append_column ... self not allocated")
     call ensure_(tonto,size(self,1)==size(col),"INTMAT:append_column ... 2nd dimension given is too small")
     old_dim2 = size(self,2)
     new_dim2 = size(self,2) + 1
     call expand_columns_(self,new_dim2)
     self(:,new_dim2) = col

   end subroutine

   subroutine append_columns(self,cols)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Append the columns "cols" onto the end of self.
     pointer :: self
     integer(kind=kind(1)), dimension(:,:) :: cols
    ! The following code is inherited from INTRINSICMAT
     integer(kind=kind(1)) :: old_dim2,new_dim2

     call ensure_(tonto,associated(self),"INTMAT:append_columns ... self not allocated")
     call ensure_(tonto,size(self,1)==size(cols,1),"INTMAT:append_columns ... 1st dimension wrong, cols")
     old_dim2 = size(self,2)
     new_dim2 = size(self,2) + size(cols,2)
     call expand_columns_(self,new_dim2)
     self(:,old_dim2+1:new_dim2) = cols

   end subroutine

   subroutine to_unit_matrix(self)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Set self to the unit matrix
      integer(kind=kind(1)) :: dim,i

      call ensure_(tonto,is_square_(self),"INTMAT:to_unit_matrix ... non-square matrix")
      dim = size(self,1)
      self = 0
      do i = 1,dim
         self(i,i) = 1
      end do

   end subroutine

   subroutine to_inverse_matrix(self)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Set self to the negative unit matrix
      integer(kind=kind(1)) :: dim,i

      call ensure_(tonto,is_square_(self),"INTMAT:to_inverse_matrix ... non-square matrix")
      dim = size(self,1)
      self = 0
      do i = 1,dim
         self(i,i) = -1
      end do

   end subroutine

   subroutine set_to(self,b)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Set self to "b"
      integer(kind=kind(1)), dimension(:,:), intent(in) :: b
    ! The following code is inherited from INTRINSICMAT

      call ensure_(tonto,is_same_shape_as_(self,b),"INTMAT:set_to ... incompatible shape")
      self = b

   end subroutine

   subroutine plus(self,b)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Add to self the matrix "b"
      integer(kind=kind(1)), dimension(:,:), intent(in) :: b
    ! The following code is inherited from INTRINSICMAT

      call ensure_(tonto,is_same_shape_as_(self,b),"INTMAT:plus ... incompatible shape")
      self = self+b

   end subroutine

   subroutine minus(self,b)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Subtract from self the matrix "b"
      integer(kind=kind(1)), dimension(:,:), intent(in) :: b
    ! The following code is inherited from INTRINSICMAT

      call ensure_(tonto,is_same_shape_as_(self,b),"INTMAT:minus ... incompatible shape")
      self = self-b

   end subroutine

   subroutine to_scaled_imat(self,fac,b)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Set self to the scaled matrix "b"
      integer(kind=kind(1)), dimension(:,:), intent(in) :: b
      integer(kind=kind(1)), intent(in) :: fac

      call ensure_(tonto,is_same_shape_as_(self,b),"INTMAT:to_scaled_imat ... incompatible shapes")
      self = fac*b

   end subroutine

   subroutine plus_scaled_imat(self,fac,b)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Add to self the scaled matrix "b"
      integer(kind=kind(1)), dimension(:,:), intent(in) :: b
      integer(kind=kind(1)), intent(in) :: fac

      call ensure_(tonto,is_same_shape_as_(self,b),"INTMAT:plus_scaled_imat ... incompatible shapes")
      self = self+fac*b

   end subroutine

   subroutine to_product_of(self,a,b,transpose_a,transpose_b)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Set self to the product of "a" and "b"
      integer(kind=kind(1)), dimension(:,:), intent(in) :: a, b
      logical(kind=kind(.true.)), optional, intent(in) :: transpose_a, transpose_b

      if (present(transpose_a)) then
        if (present(transpose_b)) then
          self = matmul(transpose(a),transpose(b))
        else
          self = matmul(transpose(a),b)
        end if
      else
        if (present(transpose_b)) then
          self = matmul(a,transpose(b))
        else
          self = matmul(a,b)
        end if
      end if

   end subroutine

   subroutine to_scaled_product_of(self,fac,a,b,transpose_a,transpose_b)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Set self to the scaled product of "a" and "b"
      integer(kind=kind(1)), dimension(:,:), intent(in) :: a, b
      real(kind=kind(1.0d0)), intent(in) :: fac
      logical(kind=kind(.true.)), optional, intent(in) :: transpose_a, transpose_b

      if (present(transpose_a)) then
        if (present(transpose_b)) then
          self = fac*matmul(transpose(a),transpose(b))
        else
          self = fac*matmul(transpose(a),b)
        end if
      else
        if (present(transpose_b)) then
          self = fac*matmul(a,transpose(b))
        else
          self = fac*matmul(a,b)
        end if
      end if

   end subroutine

   subroutine plus_product_of(self,a,b,transpose_a,transpose_b)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Add to self the product of "a" and "b"
      integer(kind=kind(1)), dimension(:,:), intent(in) :: a, b
      logical(kind=kind(.true.)), optional, intent(in) :: transpose_a, transpose_b

      if (present(transpose_a)) then
        if (present(transpose_b)) then
          self = self+matmul(transpose(a),transpose(b))
        else
          self = self+matmul(transpose(a),b)
        end if
      else
        if (present(transpose_b)) then
          self = self+matmul(a,transpose(b))
        else
          self = self+matmul(a,b)
        end if
      end if

   end subroutine

   subroutine plus_scaled_product_of(self,fac,a,b,transpose_a,transpose_b)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Add to self the scaled product of "a" and "b"
      integer(kind=kind(1)), dimension(:,:), intent(in) :: a, b
      real(kind=kind(1.0d0)), intent(in) :: fac
      logical(kind=kind(.true.)), optional, intent(in) :: transpose_a, transpose_b

      if (present(transpose_a)) then
        if (present(transpose_b)) then
          self = self+fac*matmul(transpose(a),transpose(b))
        else
          self = self+fac*matmul(transpose(a),b)
        end if
      else
        if (present(transpose_b)) then
          self = self+fac*matmul(a,transpose(b))
        else
          self = self+fac*matmul(a,b)
        end if
      end if

   end subroutine

   function trace(self) result(res)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Return the trace of self
      intent(in) :: self
      integer(kind=kind(1)) :: res
    ! The following code is inherited from INTRINSICMAT
      integer(kind=kind(1)) :: dim,i

      call ensure_(tonto,size(self,1)==size(self,2),"INTMAT:trace ... non-square matrix")
      dim = size(self,1)
      res = 0.0d0
      do i = 1,dim
         res = res + self(i,i)
      end do

   end function

   function trace_product_with(self,b) result(res)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Return the trace of the product of "self" with matrix b.
      intent(in) :: self
      integer(kind=kind(1)), dimension(:,:), intent(in) :: b
      integer(kind=kind(1)) :: res
    ! The following code is inherited from INTRINSICMAT
      integer(kind=kind(1)) :: i

      call ensure_(tonto,is_transposed_shape_of_(self,b),"INTMAT:trace_product_with ... incompatible dimensions")
      res = 0.0d0
      do i = 1,size(self,1)
         res = res + sum( self(i,:)*b(:,i) )
      end do

   end function

   function sum_row_vectors(self) result(res)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Sum the row vectors (i.e. columns) in "self".
      integer(kind=kind(1)), dimension(size(self,2)) :: res
      integer(kind=kind(1)) :: j

      do j = 1,size(self,2)
         res(j) = sum(self(:,j))
      end do

   end function

   function sum_column_vectors(self) result(res)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Sum the column vectors (i.e. rows) in "self".
      integer(kind=kind(1)), dimension(size(self,1)) :: res
      integer(kind=kind(1)) :: i

      do i = 1,size(self,1)
         res(i) = sum(self(i,:))
      end do

   end function

   subroutine swap_columns(self,col1,col2)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Swap columns "col1" and "col2" in "self".
      integer(kind=kind(1)) :: col1,col2
      integer(kind=kind(1)) :: i
      real(kind=kind(1.0d0)) :: val

   call ensure_(tonto,col1<=size(self,1) .and. col2<=size(self,2),"INTMAT:swap_columns ... columns exceed dimesions")
      if (col1==col2) then;   return; end if
      do i = 1,size(self,1)
         val = self(i,col1)
         self(i,col1) = self(i,col2)
         self(i,col2) = val
      end do

   end subroutine

   subroutine swap_columns_1(self,list)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Sequentially swap all columns in a column "list",
    ! self(:,i)       = self(:,list(i))
    ! self(:,list(i)) = self(:,i)
      integer(kind=kind(1)), dimension(:), intent(in) :: list
      integer(kind=kind(1)) :: l

   call ensure_(tonto,maxval(list)<=size(self,2),"INTMAT:swap_columns_1 ... list value exceed column dimension")
      do l = 1,size(list)
         call swap_columns_(self,l,list(l))
      end do

   end subroutine

   subroutine reverse_column_order(self)
    integer(kind=kind(1)), dimension(:,:) :: self
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

   function column_norms(self) result(res)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Return the norms of every column
      real(kind=kind(1.0d0)), dimension(size(self,2)) :: res
      integer(kind=kind(1)) :: i

      do i = 1,size(self,2)
         res(i) = sqrt(real( sum(self(:,i)*self(:,i)) ,kind=kind(1.0d0)))
      end do

   end function

   subroutine get_column_norms(self,res)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Return the norms of every column
      real(kind=kind(1.0d0)), dimension(:) :: res
      integer(kind=kind(1)) :: i

   call ensure_(tonto,size(res)==size(self,2),"INTMAT:get_column_norms ... wrong size, res array")
      do i = 1,size(self,2)
         res(i) = sqrt(real( sum(self(:,i)*self(:,i)) ,kind=kind(1.0d0)))
      end do

   end subroutine

   function max_abs_column_difference(self) result(res)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Return the maximum of the absolute difference between all the column vector
    ! pairs of the matrix.
      integer(kind=kind(1)), dimension(size(self,1)) :: res
      integer(kind=kind(1)) :: i,j,dim
      integer(kind=kind(1)), dimension(size(self,1)) :: diff,col_i,col_j

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

   function mean_column_vector(self) result(res)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Return the mean of the column vectors.
      real(kind=kind(1.0d0)), dimension(size(self,1)) :: res

      res = float(sum_column_vectors_(self))/size(self,2)

   end function

   subroutine compress_to_triangle(self,tr)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Converts the lower triangle of matrix self to the triangle "tr".
    ! using row order.
      intent(in) :: self
      integer(kind=kind(1)), dimension(:) :: tr
      integer(kind=kind(1)) :: dim,i,j,ij

      call ensure_(tonto,is_square_(self),"INTMAT:compress_to_triangle ... non-square matrix")
      call ensure_(tonto,size(tr)>=tri_size_(self),"INTMAT:compress_to_triangle ... triangle array too small")
      dim = size(self,1)
      ij = 0
      do i = 1,dim
         do j = 1,i
            tr(ij+j) = self(j,i)
         end do
         ij = ij+i
      end do

   end subroutine

   subroutine uncompress_from_triangle(self,tr)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Converts the triangle "tr" into the symmetric matrix "self".
      integer(kind=kind(1)), dimension(:) :: tr
      real(kind=kind(1.0d0)) :: tmp
      integer(kind=kind(1)) :: dim,i,j,ij

      call ensure_(tonto,is_square_(self),"INTMAT:uncompress_from_triangle ... non-square matrix")
      call ensure_(tonto,size(tr)>=tri_size_(self),"INTMAT:uncompress_from_triangle ... triangle array too small")
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
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Converts the diagonal vector "d" to matrix "self".
      integer(kind=kind(1)), dimension(:) :: d
      integer(kind=kind(1)) :: dim,i

      call ensure_(tonto,is_square_(self),"INTMAT:from_diagonal ... non-square matrix")
      call ensure_(tonto,size(d)==size(self,1),"INTMAT:from_diagonal ... incompatibale diagonal length")
      dim  = size(d)
      self = 0.0d0
      do i = 1,dim
         self(i,i) = d(i)
      end do

   end subroutine

   function tri_size(self) result(ltr)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Returns the size of the lower triangle needed to store self.
      intent(in) :: self
      integer(kind=kind(1)) :: ltr
      integer(kind=kind(1)) :: dim

      call ensure_(tonto,is_square_(self),"INTMAT:tri_size ... non-square matrix")
      dim = size(self,1)
      ltr = dim*(dim+1)/2

   end function

   subroutine set_diagonal(self,val)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Set the diagonal of "self" to "val"
      integer(kind=kind(1)) :: val
      integer(kind=kind(1)) :: dim,i

      call ensure_(tonto,is_square_(self),"INTMAT:set_diagonal ... non-square matrix")
      dim = size(self,1)
      do i = 1,dim
         self(i,i) = val
      end do

   end subroutine

   subroutine add_to_diagonal(self,val)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Add "val" to the diagonal of "self"
      integer(kind=kind(1)) :: val
      integer(kind=kind(1)) :: dim,i

      call ensure_(tonto,is_square_(self),"INTMAT:add_to_diagonal ... non-square matrix")
      dim = size(self,1)
      do i = 1,dim
         self(i,i) = self(i,i) + val
      end do

   end subroutine

   subroutine zero_diagonal(self)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Zero the diagonal elements of "self"
      integer(kind=kind(1)) :: dim,i

      call ensure_(tonto,is_square_(self),"INTMAT:zero_diagonal ... non-square matrix")
      dim = size(self,1)
      do i = 1,dim
         self(i,i) = 0
      end do

   end subroutine

   subroutine zero_off_diagonal(self)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Zero the off diagonal elements of "self"
      integer(kind=kind(1)) :: dim,i,j

      call ensure_(tonto,is_square_(self),"INTMAT:zero_off_diagonal ... non-square matrix")
      dim = size(self,1)
      do i = 1,dim
      do j = 1,dim
         if (i==j) cycle
         self(i,j) = 0
      end do
      end do

   end subroutine

   subroutine weight_diagonal(self,fac)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Weight the diagonal elements of "self" by "fac"
      integer(kind=kind(1)), intent(in) :: fac
      integer(kind=kind(1)) :: dim,i

      call ensure_(tonto,is_square_(self),"INTMAT:weight_diagonal ... non-square matrix")
      dim = size(self,1)
      do i = 1,dim
         self(i,i) = fac*self(i,i)
      end do

   end subroutine

   subroutine get_diagonal(self,diag)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Get the diagonal elements of "self" in vector "diag"
      integer(kind=kind(1)), dimension(:) :: diag
      integer(kind=kind(1)) :: dim,i

      call ensure_(tonto,size(diag)==min(size(self,1),size(self,2)),"INTMAT:get_diagonal ... diag vector is incompatible")
      dim  = size(diag)
      do i = 1,dim
         diag(i) = self(i,i)
      end do

   end subroutine

   function max_diagonal_element(self) result(res)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Get the maximum element on the diagonal of the matrix
      integer(kind=kind(1)) :: res
      integer(kind=kind(1)) :: dim,i

      call ensure_(tonto,min(size(self,1),size(self,2))>0,"INTMAT:max_diagonal_element ... cannot have zero sized dimensions"&
&)
      dim = min(size(self,1),size(self,2))
      res = self(1,1)
      do i = 2,dim
         res = max(self(i,i),res)
      end do

   end function

   function max_abs_diagonal_element(self) result(res)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Get the maximum absolute value of the diagonal elements of the self matrix
      integer(kind=kind(1)) :: res
      integer(kind=kind(1)) :: dim,i

      call ensure_(tonto,min(size(self,1),size(self,2))>0,"INTMAT:max_abs_diagonal_element ... cannot have zero sized dimensi&
&ons")
      dim = min(size(self,1),size(self,2))
      res = abs(self(1,1))
      do i = 2,dim
         res = max(abs(self(i,i)),res)
      end do

   end function

   subroutine symmetric_fold(self)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Add the upper triangle of "self" into the lower triangle
      integer(kind=kind(1)) :: dim,i,j

   call ensure_(tonto,is_square_(self),"INTMAT:symmetric_fold ... non-square matrix")
      dim = size(self,1)
      do i = 1,dim
         do j=1,i-1
            self(i,j) = self(i,j)+self(j,i)
         end do
      end do

   end subroutine

   subroutine antisymmetric_fold(self)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Subtract the upper triangle of "self" into the lower triangle
      integer(kind=kind(1)) :: dim,i,j

   call ensure_(tonto,is_square_(self),"INTMAT:antisymmetric_fold ... non-square matrix")
      dim = size(self,1)
      do i = 1,dim
         do j=1,i-1
            self(i,j) = self(i,j)-self(j,i)
         end do
      end do

   end subroutine

   subroutine symmetric_reflect(self)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Set the upper half of self to the lower half
      integer(kind=kind(1)) :: dim,i,j

   call ensure_(tonto,is_square_(self),"INTMAT:symmetric_reflect ... non-square matrix")
      dim = size(self,1)
      do i = 1,dim
      do j = 1,i-1
         self(j,i) = self(i,j)
      end do
      end do

   end subroutine

   subroutine antisymmetric_reflect(self)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Set the upper half of self to the negative of the lower half.
    ! The diagonals are set to zero
      integer(kind=kind(1)) :: dim,i,j

   call ensure_(tonto,is_square_(self),"INTMAT:antisymmetric_reflect ... non-square matrix")
      dim = size(self,1)
      do i = 1,dim
         do j = 0,i-1
            self(j,i) = -self(i,j)
         end do
      end do
      do i = 1,dim
         self(i,i) = 0
      end do

   end subroutine

   subroutine make_diagonally_dominant(self,permutation)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Rearrange the order of the columns of self so that the largest magnitude
    ! elements in each column occur along the diagonal. If "permutation" is
    ! present, it is a matrix which achieves this ordering, i.e. at the
    ! conclusion of the routine, self = self(:,permutation).
      integer(kind=kind(1)), dimension(:), optional :: permutation
      integer(kind=kind(1)), dimension(:), pointer :: perm
      logical(kind=kind(.true.)), dimension(:,:), pointer :: mask
      integer(kind=kind(1)) :: i,n
      integer(kind=kind(1)), dimension(2) :: loc

   call ensure_(tonto,is_square_(self),"INTMAT:make_diagonally_dominant ... not square")
      if (present(permutation)) &
   call ensure_(tonto,size(permutation)==size(self,2),"INTMAT:make_diagonally_dominant ... wrong size, perm")
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

   subroutine to_gaussian_xyz_powers(self,l_max)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Make "self(1:3,i)", the three xyz powers of all cartesian gaussian
    ! functions "i" of angular momentum up to "l_max", where "i" is the standard
    ! lexical index of the cartesian gaussian. (This routine essentially defines
    ! the standard order).  The shape of "self" is: [3, l_max.n_comp_sum ].
     intent(out) :: self
     integer(kind=kind(1)), intent(in) :: l_max
     integer(kind=kind(1)) :: i,L,a,b,c

   call ensure_(tonto,size(self,1)==3,"INTMAT:to_gaussian_xyz_powers ... wrong 1st dimension, self")
   call ensure_(tonto,size(self,2)==n_comp_sum_(l_max),"INTMAT:to_gaussian_xyz_powers ... wrong 2nd dimension, self")
     i = 1           ! This is the total lexical index
     do L = 0,l_max  ! Loop over all shells with momentum L
                     ! Loop over powers a, b, c
       do a = L,floor((L+2)*0.33333333333333333333333d0),-1
         do b = min(L-a,a),floor((L-a+1)*0.50d0),-1
           c = L-a-b
           if (a==b .and. b==c) then
             self(:,i)   = (/a,a,a/)
             i = i+1
           else if (a>b .and. b==c) then
             self(:,i)   = (/a,b,b/)
             self(:,i+1) = (/b,a,b/)
             self(:,i+2) = (/b,b,a/)
             i = i+3
           else if (a==b .and. b>c) then
             self(:,i)   = (/a,a,c/)
             self(:,i+1) = (/a,c,a/)
             self(:,i+2) = (/c,a,a/)
             i = i+3
           else
             self(:,i)   = (/a,b,c/)
             self(:,i+1) = (/a,c,b/)
             self(:,i+2) = (/b,a,c/)
             self(:,i+3) = (/c,a,b/)
             self(:,i+4) = (/b,c,a/)
             self(:,i+5) = (/c,b,a/)
             i = i+6
           end if
         end do
       end do
     end do

   end subroutine

   subroutine to_gaussian_xyz_powers_1(self,l_max,index)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Make "self(1:3,i)", the three xyz powers of all cartesian gaussian
    ! functions "i" of angular momentum up to "l_max", where "i" is the standard
    ! lexical index of the cartesian gaussian. (This routine essentially defines
    ! the standard order).  The shape of "self" is: [3, l_max.n_comp_sum ].
    ! Array "index" maps the three xyz powers of each cartesian gaussian back to
    ! its lexical index *within a shell of the same angular momentum* i.e. not
    ! the total lexical index. NOTE: "index" has lower bounds of 0, and so is
    ! passed as a pointer.
     intent(out) :: self
     integer(kind=kind(1)), intent(in) :: l_max
     integer(kind=kind(1)), dimension(:,:,:), pointer :: index
     integer(kind=kind(1)) :: i,L,k,a,b,c

   call ensure_(tonto,size(self,1)==3,"INTMAT:to_gaussian_xyz_powers_1 ... wrong 1st dimension, self")
   call ensure_(tonto,size(self,2)==n_comp_sum_(l_max),"INTMAT:to_gaussian_xyz_powers_1 ... wrong 2nd dimension, self")
   call ensure_(tonto,lbound(index,1)==0,"INTMAT:to_gaussian_xyz_powers_1 ... wrong lower bound, index")
   call ensure_(tonto,lbound(index,2)==0,"INTMAT:to_gaussian_xyz_powers_1 ... wrong lower bound, index")
   call ensure_(tonto,lbound(index,3)==0,"INTMAT:to_gaussian_xyz_powers_1 ... wrong lower bound, index")
   call ensure_(tonto,ubound(index,1)==l_max,"INTMAT:to_gaussian_xyz_powers_1 ... wrong upper bound, index")
   call ensure_(tonto,ubound(index,2)==l_max,"INTMAT:to_gaussian_xyz_powers_1 ... wrong upper bound, index")
   call ensure_(tonto,ubound(index,3)==l_max,"INTMAT:to_gaussian_xyz_powers_1 ... wrong upper bound, index")
     i = 1               ! This is the total lexical index
     do L = 0,l_max      ! Loop over all shells with momentum L
       k = 1             ! This is the local shell lexical index
        !                  Loop over powers a, b, c
       do a = L,floor((L+2)*0.33333333333333333333333d0),-1
         do b = min(L-a,a),floor((L-a+1)*0.50d0),-1
           c = L-a-b
           if (a==b .and. b==c) then
             self(:,i)   = (/a,a,a/)
             index(a,a,a) = k
             i = i+1
             k = k+1
           else if (a>b .and. b==c) then
             self(:,i)   = (/a,b,b/)
             self(:,i+1) = (/b,a,b/)
             self(:,i+2) = (/b,b,a/)
             index(a,b,b) = k
             index(b,a,b) = k+1
             index(b,b,a) = k+2
             i = i+3
             k = k+3
           else if (a==b .and. b>c) then
             self(:,i)   = (/a,a,c/)
             self(:,i+1) = (/a,c,a/)
             self(:,i+2) = (/c,a,a/)
             index(a,a,c) = k
             index(a,c,a) = k+1
             index(c,a,a) = k+2
             i = i+3
             k = k+3
           else
             self(:,i)   = (/a,b,c/)
             self(:,i+1) = (/a,c,b/)
             self(:,i+2) = (/b,a,c/)
             self(:,i+3) = (/c,a,b/)
             self(:,i+4) = (/b,c,a/)
             self(:,i+5) = (/c,b,a/)
             index(a,b,c) = k
             index(a,c,b) = k+1
             index(b,a,c) = k+2
             index(c,a,b) = k+3
             index(b,c,a) = k+4
             index(c,b,a) = k+5
             i = i+6
             k = k+6
           end if
         end do
       end do
     end do

   end subroutine

   subroutine bin_XY_data(self,X,Y,bin_side_length,data_count)
    integer(kind=kind(1)), dimension(:,:) :: self
    ! Set self to a matrix whose ij-th element contains the number of data points
    ! [X(k),Y(k)] which lie in the ij-th bin. A bin is simply a range of values
    ! of side length "bin_side_length" covering the set of points from
    ! [X_min,Y_min] to [X_max,Y_max]. The dimension of self is calculated within
    ! this routine. If "data_count" is present, then the bin count associated
    ! with each data item [X(k),Y(k)] is returned in an array.
      pointer :: self
      real(kind=kind(1.0d0)), dimension(:), intent(in) :: X,Y
      real(kind=kind(1.0d0)), intent(in) :: bin_side_length
      integer(kind=kind(1)), dimension(:), pointer, optional :: data_count
      real(kind=kind(1.0d0)) :: X_min,X_max,X_mid,X_ran
      real(kind=kind(1.0d0)) :: Y_min,Y_max,Y_mid,Y_ran
      real(kind=kind(1.0d0)), dimension(2) :: X_range,Y_range
      integer(kind=kind(1)) :: dim,n_X,n_Y,i,j,k

   call ensure_(tonto,size(X)==size(Y),"INTMAT:bin_XY_data ... incompatible data points")
      dim = size(X)
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
      call create_(self,n_X,n_Y)
      if (present(data_count)) call create_(data_count,dim)
      do i = 1,n_X
      do j = 1,n_Y
         X_range(1) = X_min + (i-1)*bin_side_length
         X_range(2) = X_range(1)  + bin_side_length
         Y_range(1) = Y_min + (j-1)*bin_side_length
         Y_range(2) = Y_range(1)  + bin_side_length
         self(i,j) = count(in_range_(X,X_range) .and. in_range_(Y,Y_range))
         if (present(data_count)) then
         do k = 1,dim
            if (is_in_range_(X(k),X_range) .and. is_in_range_(Y(k),Y_range)) then
            data_count(k) = self(i,j)
            end if
         end do
         end if
      end do
      end do

   end subroutine

end
