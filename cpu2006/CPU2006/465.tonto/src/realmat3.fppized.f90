!---------------------------------------------------------------------------
!
!  REALMAT3: 3 dimensional matrices
!
! Copyright (C) Dylan Jayatilaka, 1998
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
! $Id: realmat3.foo,v 1.8.2.1 2003/11/13 05:36:07 reaper Exp $
!---------------------------------------------------------------------------

module REALMAT3_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

   use REALMAT_MODULE, only: gaussian_g_xyz_matrix_
   use REALMAT_MODULE, only: similarity_transform_
   use REALMAT_MODULE, only: gaussian_f_xyz_matrix_
   use REALMAT_MODULE, only: gaussian_d_xyz_matrix_
   use REALMAT_MODULE, only: is_square_
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

   public    dim1_
   interface dim1_
      module procedure dim1
   end interface

   public    dim2_
   interface dim2_
      module procedure dim2
   end interface

   public    set_to_
   interface set_to_
      module procedure set_to
      module procedure set_to_1
   end interface

   public    dim3_
   interface dim3_
      module procedure dim3
   end interface

   public    create_
   interface create_
      module procedure create
      module procedure create_1
      module procedure create_2
      module procedure create_3
   end interface

   public    create_copy_
   interface create_copy_
      module procedure create_copy
   end interface

   public    gaussian_f_xyz_matrices_
   interface gaussian_f_xyz_matrices_
      module procedure gaussian_f_xyz_matrices
   end interface

   public    gaussian_d_xyz_matrices_
   interface gaussian_d_xyz_matrices_
      module procedure gaussian_d_xyz_matrices
   end interface

   public    transpose_12_
   interface transpose_12_
      module procedure transpose_12
   end interface

   public    to_tri_23_
   interface to_tri_23_
      module procedure to_tri_23
   end interface

   public    from_tri_23_
   interface from_tri_23_
      module procedure from_tri_23
   end interface

   public    check_square_23_
   interface check_square_23_
      module procedure check_square_23
   end interface

   public    destroy_
   interface destroy_
      module procedure destroy
   end interface

   public    make_symmetric_
   interface make_symmetric_
      module procedure make_symmetric
   end interface

   public    gaussian_g_xyz_matrices_
   interface gaussian_g_xyz_matrices_
      module procedure gaussian_g_xyz_matrices
   end interface

   public    symmetric_reflect_12_
   interface symmetric_reflect_12_
      module procedure symmetric_reflect_12
   end interface

   public    similarity_transform_12_
   interface similarity_transform_12_
      module procedure similarity_transform_12
   end interface

   public    symmetric_reflect_23_
   interface symmetric_reflect_23_
      module procedure symmetric_reflect_23
   end interface

   public    is_same_shape_as_
   interface is_same_shape_as_
      module procedure is_same_shape_as
   end interface

   public    tri_size_23_
   interface tri_size_23_
      module procedure tri_size_23
   end interface

contains

   subroutine create(self,dim1,dim2,dim3)
    real(kind=kind(1.0d0)), dimension(:,:,:) :: self
    ! Create a mat3 with the given dimensions
      pointer :: self
      integer(kind=kind(1)) :: dim1,dim2,dim3

      nullify(self)
      allocate(self(dim1,dim2,dim3))

   end subroutine

   subroutine create_1(self,lb1,ub1,lb2,ub2,lb3,ub3)
    real(kind=kind(1.0d0)), dimension(:,:,:) :: self
    ! Create a mat3 with the given bounds
      pointer :: self
      integer(kind=kind(1)), intent(in) :: lb1,lb2,lb3,ub1,ub2,ub3

      nullify(self)
      allocate(self(lb1:ub1,lb2:ub2,lb3:ub3))

   end subroutine

   subroutine create_2(self,bounds1,bounds2,bounds3)
    real(kind=kind(1.0d0)), dimension(:,:,:) :: self
    ! Create a mat3 with the given bounds for each dimension
      pointer :: self
      integer(kind=kind(1)), dimension(2), intent(in) :: bounds1,bounds2,bounds3

      call create_(self,bounds1(1),bounds1(2),bounds2(1),bounds2(2),bounds3(1),bounds3(2))

   end subroutine

   subroutine create_3(self,bounds)
    real(kind=kind(1.0d0)), dimension(:,:,:) :: self
    ! Create a mat3 with the given bounds for all dimensions
      pointer :: self
      integer(kind=kind(1)), dimension(3,2), intent(in) :: bounds

      call create_(self,bounds(1,1),bounds(1,2),bounds(2,1),bounds(2,2), &
              bounds(3,1),bounds(3,2))

   end subroutine

   subroutine create_copy(self,m)
    real(kind=kind(1.0d0)), dimension(:,:,:) :: self
    ! Create a copy of matrix "m"
      pointer :: self
      real(kind=kind(1.0d0)), dimension(:,:,:), pointer :: m

      call create_(self,lbound(m,1),ubound(m,1), &
              lbound(m,2),ubound(m,2), &
              lbound(m,3),ubound(m,3)  )
      self = m

   end subroutine

   subroutine destroy(self)
    real(kind=kind(1.0d0)), dimension(:,:,:) :: self
    ! Destroy at mat3 object
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

   function dim1(self) result(res)
    real(kind=kind(1.0d0)), dimension(:,:,:) :: self
    ! Returns the first dimension of self
      integer(kind=kind(1)) :: res

      res = size(self,1)

   end function

   function dim2(self) result(res)
    real(kind=kind(1.0d0)), dimension(:,:,:) :: self
    ! Returns the second dimension of self
      integer(kind=kind(1)) :: res

      res = size(self,2)

   end function

   function dim3(self) result(res)
    real(kind=kind(1.0d0)), dimension(:,:,:) :: self
    ! Returns the thirs dimension of self
      integer(kind=kind(1)) :: res

      res = size(self,3)

   end function

   pure function is_same_shape_as(self,b) result(res)
    real(kind=kind(1.0d0)), dimension(:,:,:) :: self
    ! Returns .true. if the matrix "b" has the same shape as self
      real(kind=kind(1.0d0)), dimension(:,:,:), intent(in) :: b
      intent(in) :: self
      logical(kind=kind(.true.)) :: res
      res = size(self,1)==size(b,1) .and. size(self,2)==size(b,2) .and. size(self,3)==size(b,3)

   end function

   subroutine set_to(self,b)
    real(kind=kind(1.0d0)), dimension(:,:,:) :: self
    ! Set "self" to "b"
      real(kind=kind(1.0d0)), dimension(:,:,:) :: b

   call ensure_(tonto,is_same_shape_as_(self,b),"REALMAT3:set_to ... different shapes")
      self = b

   end subroutine

   subroutine set_to_1(self,b)
    real(kind=kind(1.0d0)), dimension(:,:,:) :: self
    ! Set "self" to vector "b" in standard order
      real(kind=kind(1.0d0)), dimension(:) :: b

   call ensure_(tonto,size(self)==size(b),"REALMAT3:set_to_1 ... different sizes")
      self = reshape(b,(/ size(self,1),size(self,2),size(self,3) /))

   end subroutine

   subroutine make_symmetric(self)
    real(kind=kind(1.0d0)), dimension(:,:,:) :: self
    ! Make the upper pyramid of "self" the same as the lower pyramid
      integer(kind=kind(1)) :: dim, i,j,k
      real(kind=kind(1.0d0)) :: val

   call ensure_(tonto,size(self,1)==size(self,2),"REALMAT3:make_symmetric ... non-cube tensor")
   call ensure_(tonto,size(self,1)==size(self,3),"REALMAT3:make_symmetric ... non-cube tensor")
      dim = size(self,1)
      do i = 1,dim
      do j = 1,i
      do k = 1,j
         val = self(i,j,k)
         self(i,k,j) = val
         self(j,i,k) = val
         self(j,k,i) = val
         self(k,i,j) = val
         self(k,j,i) = val
      end do
      end do
      end do

   end subroutine

   subroutine transpose_12(self)
    real(kind=kind(1.0d0)), dimension(:,:,:) :: self
    ! Makes the matrix formed by the 1st and 2nd columns symmetrical.
     integer(kind=kind(1)) :: k

   call ensure_(tonto,size(self,1)==size(self,2),"REALMAT3:transpose_12 ... non-square matrix")
     do k = 1,size(self,3)
       self(:,:,k) = transpose(self(:,:,k))
     end do

   end subroutine

   subroutine symmetric_reflect_12(self)
    real(kind=kind(1.0d0)), dimension(:,:,:) :: self
    ! Makes the matrix formed by the 1st and 2nd columns symmetrical.
     integer(kind=kind(1)) :: dim1,dim2,i,j

   call ensure_(tonto,size(self,1)==size(self,2),"REALMAT3:symmetric_reflect_12 ... non-square matrix")
     dim1 = size(self,1)
     dim2 = size(self,2)
     do i = 1,dim1
     do j = 1,i-1
       self(j,i,:) = self(i,j,:)
     end do
     end do

   end subroutine

   subroutine symmetric_reflect_23(self)
    real(kind=kind(1.0d0)), dimension(:,:,:) :: self
    ! Makes the matrix formed by the 2nd and 3rd columns symmetrical.
     integer(kind=kind(1)) :: dim,i,j

     call check_square_23_(self)
     dim = size(self,2)
     do i = 1,dim
     do j = 1,i-1
       self(:,j,i) = self(:,i,j)
     end do
     end do

   end subroutine

   subroutine to_tri_23(self,tr)
    real(kind=kind(1.0d0)), dimension(:,:,:) :: self
    ! Converts the matrix self to the lower triangle tr.
    ! Assumes the matrix formed by columns 2 and 3 is the symmetric one.
     intent(in) :: self
     real(kind=kind(1.0d0)), dimension(:) :: tr
     integer(kind=kind(1)) :: dim1,dim2,h,i,j,ij

   call ensure_(tonto,size(tr)>=tri_size_23_(self),"REALMAT3:to_tri_23 ... tr array too small")
     dim1 = size(self,1)
     dim2 = size(self,2)
     call check_square_23_(self)
     ij = 0
     do h = 1,dim1
       do i = 1,dim2
         do j = 1,i
           ij = ij+1
           tr(ij) = self(h,j,i)
         end do
       end do
     end do

   end subroutine

   subroutine from_tri_23(self,tr)
    real(kind=kind(1.0d0)), dimension(:,:,:) :: self
    ! Converts the matrix self to the lower triangle tr.
    ! Assumes the matrix formed by columns 2 and 3 is the symmetric one.
     real(kind=kind(1.0d0)), dimension(:), intent(in) :: tr
     integer(kind=kind(1)) :: dim1,dim2,h,i,j,ij

   call ensure_(tonto,size(tr)>=tri_size_23_(self),"REALMAT3:from_tri_23 ... tr array too small")
     dim1 = size(self,1)
     dim2 = size(self,2)
     call check_square_23_(self)
     ij = 0
     do h = 1,dim1
       do i = 1,dim2
         do j = 1,i
           ij = ij+1
           self(h,j,i) = tr(ij)
           self(h,i,j) = tr(ij)
         end do
       end do
     end do

   end subroutine

   function tri_size_23(self) result(ltr)
    real(kind=kind(1.0d0)), dimension(:,:,:) :: self
    ! Returns the size of the lower triangle needed to store the matrix self.
    ! Assumes the matrix formed by columns 2 and 3 is the symmetric one.
     intent(in) :: self
     integer(kind=kind(1)) :: ltr
     integer(kind=kind(1)) :: dim1,dim2

     dim1 = size(self,1)
     dim2 = size(self,2)
     call check_square_23_(self)
     ltr = dim1*dim2*(dim2+1)/2

   end function

   subroutine check_square_23(self)
    real(kind=kind(1.0d0)), dimension(:,:,:) :: self
    ! Checks to see that the matrix formed by the 2nd and 3rd columns is square.
     integer(kind=kind(1)) :: dim2,dim3

     dim2 = size(self,2)
     dim3 = size(self,3)
     call ensure_(tonto,dim2==dim3,"REALMAT3:check_square_23 ... non-square 2nd and 3rd dimensions")

   end subroutine

   function gaussian_d_xyz_matrices(self) result(dtr)
    real(kind=kind(1.0d0)), dimension(:,:,:) :: self
    ! Return the representation matrices for d xyz products found in
    ! gaussian shells from the p xyz matrices.
      real(kind=kind(1.0d0)), dimension(:,:,:), pointer :: dtr
      integer(kind=kind(1)) :: n,order

   call ensure_(tonto,size(self,1)==3,"REALMAT3:gaussian_d_xyz_matrices ... wrong 1st dimension, self")
   call ensure_(tonto,size(self,2)==3,"REALMAT3:gaussian_d_xyz_matrices ... wrong 1st dimension, self")
   call ensure_(tonto,size(self,3)>0,"REALMAT3:gaussian_d_xyz_matrices ... no p type matrices")
      order = size(self,3)
      call create_(dtr,6,6,order)
      do n = 1,order
         dtr(:,:,n) = gaussian_d_xyz_matrix_(self(:,:,n))
      end do

   end function

   function gaussian_f_xyz_matrices(self) result(ftr)
    real(kind=kind(1.0d0)), dimension(:,:,:) :: self
    ! Return the representation matrices for f xyz products found in
    ! gaussian shells from the p xyz matrices
      real(kind=kind(1.0d0)), dimension(:,:,:), pointer :: ftr
      integer(kind=kind(1)) :: n,order

   call ensure_(tonto,size(self,1)==3,"REALMAT3:gaussian_f_xyz_matrices ... wrong 1st dimension, self")
   call ensure_(tonto,size(self,2)==3,"REALMAT3:gaussian_f_xyz_matrices ... wrong 1st dimension, self")
   call ensure_(tonto,size(self,3)>0,"REALMAT3:gaussian_f_xyz_matrices ... no p type matrices")
      order = size(self,3)
      call create_(ftr,10,10,order)
      do n = 1,order
         ftr(:,:,n) = gaussian_f_xyz_matrix_(self(:,:,n))
      end do

   end function

   function gaussian_g_xyz_matrices(self) result(gtr)
    real(kind=kind(1.0d0)), dimension(:,:,:) :: self
    ! Return the representation matrices for g xyz products found in
    ! gaussian shells from the p xyz matrices
      real(kind=kind(1.0d0)), dimension(:,:,:), pointer :: gtr
      integer(kind=kind(1)) :: n,order

   call ensure_(tonto,size(self,1)==3,"REALMAT3:gaussian_g_xyz_matrices ... wrong 1st dimension, self")
   call ensure_(tonto,size(self,2)==3,"REALMAT3:gaussian_g_xyz_matrices ... wrong 1st dimension, self")
   call ensure_(tonto,size(self,3)>0,"REALMAT3:gaussian_g_xyz_matrices ... no p type matrices")
      order = size(self,3)
      call create_(gtr,15,15,order)
      do n = 1,order
         gtr(:,:,n) = gaussian_g_xyz_matrix_(self(:,:,n))
      end do

   end function

   subroutine similarity_transform_12(self,V)
    real(kind=kind(1.0d0)), dimension(:,:,:) :: self
    ! Do a similarity tranform on the first two indices of self, i.e.
    ! self(:,:,i) -> V self(:,:,i) V^-1
      real(kind=kind(1.0d0)), dimension(:,:) :: V
      integer(kind=kind(1)) :: n

   call ensure_(tonto,size(self,1)==size(self,2),"REALMAT3:similarity_transform_12 ... 1st two dimensions of self unequal")
   call ensure_(tonto,size(self,1)==size(V,1),"REALMAT3:similarity_transform_12 ... incompatible transform matrix, V")
   call ensure_(tonto,is_square_(V),"REALMAT3:similarity_transform_12 ... transform matrix not square")
   call ensure_(tonto,size(self,3)>0,"REALMAT3:similarity_transform_12 ... no p type matrices")
      do n = 1,size(self,3)
         call similarity_transform_(self(:,:,n),V)
      end do

   end subroutine

end
