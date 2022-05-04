!---------------------------------------------------------------------------
!
!  CPXVEC: Complex vector operations ...
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
! $Id: cpxvec.foo,v 1.7.2.2 2003/11/13 05:34:39 reaper Exp $
!---------------------------------------------------------------------------

module CPXVEC_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

   use INT_MODULE, only: is_even_

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

   public    index_of_largest_value_
   interface index_of_largest_value_
      module procedure index_of_largest_value
   end interface

   public    join_
   interface join_
      module procedure join
      module procedure join_1
   end interface

   public    chop_large_values_
   interface chop_large_values_
      module procedure chop_large_values
   end interface

   public    norm_
   interface norm_
      module procedure norm
   end interface

   public    create_
   interface create_
      module procedure create
   end interface

   public    create_copy_
   interface create_copy_
      module procedure create_copy
   end interface

   public    alpha_
   interface alpha_
      module procedure alpha
   end interface

   public    set_beta_
   interface set_beta_
      module procedure set_beta
      module procedure set_beta_1
   end interface

   public    dot_
   interface dot_
      module procedure dot
   end interface

   public    plus_product_of_
   interface plus_product_of_
      module procedure plus_product_of
      module procedure plus_product_of_1
   end interface

   public    set_alpha_
   interface set_alpha_
      module procedure set_alpha
      module procedure set_alpha_1
   end interface

   public    beta_
   interface beta_
      module procedure beta
   end interface

   public    to_product_of_
   interface to_product_of_
      module procedure to_product_of
      module procedure to_product_of_1
   end interface

   public    expand_
   interface expand_
      module procedure expand
   end interface

   public    destroy_
   interface destroy_
      module procedure destroy
   end interface

   public    largest_value_
   interface largest_value_
      module procedure largest_value
   end interface

   public    dim_
   interface dim_
      module procedure dim
   end interface

   public    index_of_smallest_value_
   interface index_of_smallest_value_
      module procedure index_of_smallest_value
   end interface

   public    shrink_
   interface shrink_
      module procedure shrink
   end interface

   public    smallest_value_
   interface smallest_value_
      module procedure smallest_value
   end interface

   public    to_cross_product_
   interface to_cross_product_
      module procedure to_cross_product
   end interface

   public    copy_
   interface copy_
      module procedure copy
   end interface

contains

   subroutine create(self,dim)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:) :: self
    ! Create space for object
      pointer :: self
    ! The following code is inherited from INTRINSICVEC
      integer(kind=kind(1)), intent(in) :: dim

      call ensure_(tonto,dim>=0,"CPXVEC:create ... dimension of array not 1 or greater")
      nullify(self)
      allocate(self(dim))

   end subroutine

   subroutine destroy(self)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:) :: self
    ! Destroy space for object
      pointer :: self
    ! The following code is inherited from INTRINSICVEC

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
    complex(kind=kind((1.0d0,1.0d0))), dimension(:) :: self
    ! Create a replica copy of vec.
      complex(kind=kind((1.0d0,1.0d0))), dimension(:), intent(in) :: vec
      pointer :: self
    ! The following code is inherited from INTRINSICVEC

      call create_(self,size(vec))
      self = vec

   end subroutine

   subroutine copy(self,vec)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:) :: self
    ! Copy "vec".
      complex(kind=kind((1.0d0,1.0d0))), dimension(:), intent(in) :: vec
    ! The following code is inherited from INTRINSICVEC

      call ensure_(tonto,size(self)==size(vec),"CPXVEC:copy ... vec size does not match")
      self = vec

   end subroutine

   function dim(self) result(res)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:) :: self
    ! Returns the size of self as a one dimensional array
      integer(kind=kind(1)) :: res

      res = size(self)

   end function

   subroutine shrink(self,dim)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:) :: self
    ! Shrink self to dimension dim.  Contents are retained.
     pointer :: self
     integer(kind=kind(1)), intent(in) :: dim
    ! The following code is inherited from INTRINSICVEC
     complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: old
     integer(kind=kind(1)) :: n

     call ensure_(tonto,associated(self),"CPXVEC:shrink ... no self array")
     call ensure_(tonto,dim<=size(self),"CPXVEC:shrink ... dim too large")
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
    complex(kind=kind((1.0d0,1.0d0))), dimension(:) :: self
    ! Expand self to dimension dim. New slots are left undefined.
     pointer :: self
     integer(kind=kind(1)), intent(in) :: dim
    ! The following code is inherited from INTRINSICVEC
     complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: old
     integer(kind=kind(1)) :: old_dim

     if (.not. associated(self)) then
        call create_(self,dim)
     else
        call ensure_(tonto,dim>=size(self),"CPXVEC:expand ... dim not large enough")
        old => self
        old_dim = size(old)
        nullify(self)
        call create_(self,dim)
        self(1:old_dim) = old
        call destroy_(old)
     end if

   end subroutine

   function join(self,v) result(res)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:) :: self
    ! Yield a vector which is the concatenation of "self" and "v"
     complex(kind=kind((1.0d0,1.0d0))), dimension(:), intent(in) :: v
     complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: res
    ! The following code is inherited from INTRINSICVEC
     integer(kind=kind(1)) :: dim, dim_v

     dim   = size(self)
     dim_v = size(v)
     call create_(res,dim+dim_v)
     res(    1:dim      ) = self
     res(dim+1:dim+dim_v) = v

   end function

   function join_1(self,v1,v2) result(res)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:) :: self
    ! Yield a vector which is the concatenation of "self" and "v1" and "v2"
     complex(kind=kind((1.0d0,1.0d0))), dimension(:), intent(in) :: v1,v2
     complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: res
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

   subroutine to_product_of(self,a,b,dagger_a)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:) :: self
    ! Set "self" to the product of matrix "a" and vector "b". If present,
    ! "dagger_a" can be set to .true. if "a" needs to be daggered.
       complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), intent(in) :: a
       complex(kind=kind((1.0d0,1.0d0))), dimension(:), intent(in) :: b
      logical(kind=kind(.true.)), optional :: dagger_a
      integer(kind=kind(1)) :: a1,a2, i,j
      logical(kind=kind(.true.)) :: ta

      a1 = size(a,1); a2 = size(a,2)
      ta = .false.
      if (present(dagger_a)) ta = dagger_a
      if (ta) then
         self = 0.0d0
         do i = 1,a2
         do j = 1,a1
            self(i) = self(i) + conjg(a(j,i))*b(j)
         end do
         end do
      else
         self = 0.0d0
         do i = 1,a1
         do j = 1,a2
            self(i) = self(i) + a(i,j)*b(j)
         end do
         end do
      end if

   end subroutine

   subroutine to_product_of_1(self,a,b,transpose_a)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:) :: self
    ! Set "self" to the product of matrix "a" and vector "b". If present,
    ! "transpose_a" can be set to .true. if "a" needs to be transposed.
       real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: a
       complex(kind=kind((1.0d0,1.0d0))), dimension(:), intent(in) :: b
      logical(kind=kind(.true.)), optional :: transpose_a
      integer(kind=kind(1)) :: a1,a2, i,j
      logical(kind=kind(.true.)) :: ta

      a1 = size(a,1); a2 = size(a,2)
      ta = .false.
      if (present(transpose_a)) ta = transpose_a
      if (ta) then
         self = 0.0d0
         do i = 1,a2
         do j = 1,a1
            self(i) = self(i) + a(j,i)*b(j)
         end do
         end do
      else
         self = 0.0d0
         do i = 1,a1
         do j = 1,a2
            self(i) = self(i) + a(i,j)*b(j)
         end do
         end do
      end if

   end subroutine

   subroutine plus_product_of(self,a,b,dagger_a)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:) :: self
    ! Set "self" to the product of matrix "a" and vector "b". If present,
    ! "dagger_a" can be set to .true. if "a" needs to be daggered.
       complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), intent(in) :: a
       complex(kind=kind((1.0d0,1.0d0))), dimension(:), intent(in) :: b
      logical(kind=kind(.true.)), optional :: dagger_a
      integer(kind=kind(1)) :: a1,a2, i,j
      logical(kind=kind(.true.)) :: ta

      a1 = size(a,1); a2 = size(a,2)
      ta = .false.
      if (present(dagger_a)) ta = dagger_a
      if (ta) then
         do i = 1,a2
         do j = 1,a1
            self(i) = self(i) + conjg(a(j,i))*b(j)
         end do
         end do
      else
         do i = 1,a1
         do j = 1,a2
            self(i) = self(i) + a(i,j)*b(j)
         end do
         end do
      end if

   end subroutine

   subroutine plus_product_of_1(self,a,b,transpose_a)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:) :: self
    ! Set "self" to the product of matrix "a" and vector "b". If present,
    ! "transpose_a" can be set to .true. if "a" needs to be transposed.
       real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: a
       complex(kind=kind((1.0d0,1.0d0))), dimension(:), intent(in) :: b
      logical(kind=kind(.true.)), optional :: transpose_a
      integer(kind=kind(1)) :: a1,a2, i,j
      logical(kind=kind(.true.)) :: ta

      a1 = size(a,1); a2 = size(a,2)
      ta = .false.
      if (present(transpose_a)) ta = transpose_a
      if (ta) then
         do i = 1,a2
         do j = 1,a1
            self(i) = self(i) + a(j,i)*b(j)
         end do
         end do
      else
         do i = 1,a1
         do j = 1,a2
            self(i) = self(i) + a(i,j)*b(j)
         end do
         end do
      end if

   end subroutine

   pure function dot(self,b) result(res)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:) :: self
    ! Return the dot product with "b"
      intent(in) :: self
      complex(kind=kind((1.0d0,1.0d0))), dimension(:), intent(in) :: b
      complex(kind=kind((1.0d0,1.0d0))) :: res
    ! The following code is inherited from INTRINSICVEC
      res = dot_product(self,b)

   end function

   pure subroutine to_cross_product(self,a,b)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:) :: self
    ! Set the vector to the cross product of "a" and "b".
      intent(inout) :: self
      complex(kind=kind((1.0d0,1.0d0))), dimension(:), intent(in) :: a,b
    ! The following code is inherited from INTRINSICVEC

      self(1) = a(2)*b(3) - b(2)*a(3)
      self(2) = a(3)*b(1) - b(3)*a(1)
      self(3) = a(1)*b(2) - b(1)*a(2)

   end subroutine

   pure function norm(self) result(val)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:) :: self
    ! Return the norm of self
      intent(in) :: self
      complex(kind=kind((1.0d0,1.0d0))) :: val
       integer(kind=kind(1)) :: i
      val = 0.0d0
      do i=1,size(self)
        val = val + self(i)*self(i)
      end do
      val = sqrt(val)

   end function

   pure subroutine chop_large_values(self,val)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:) :: self
    ! Set all values in the self whose absolute value is larger than "val" to
    ! "val" times the sign of the original value.
      intent(inout) :: self
      real(kind=kind(1.0d0)), intent(in) :: val
      integer(kind=kind(1)) :: dim,i
      real(kind=kind(1.0d0)) :: ba
      complex(kind=kind((1.0d0,1.0d0))) :: bb,phase
      dim = size(self)
      do i = 1,dim
         bb = self(i)
         ba = abs(bb)
         if (ba==0.0d0) then
            self(i) = 0.0d0
         else
            phase = bb/ba
            self(i) = phase*min(val,ba)
         end if
     end do

   end subroutine

   pure function largest_value(self) result(maxv)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:) :: self
    ! Return the largest value in self
      intent(in) :: self
      real(kind=kind(1.0d0)) :: maxv
      real(kind=kind(1.0d0)) :: bb
      integer(kind=kind(1)) :: i
      maxv = abs(self(1))
      do i = 2,size(self)
        bb = abs(self(i))
        if (bb > maxv) maxv = bb
      end do
!      res = dzamax(size(self),self,1) ! blas library

   end function

   pure function smallest_value(self) result(minv)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:) :: self
    ! Return the smallest value in self
      intent(in) :: self
      real(kind=kind(1.0d0)) :: minv
      real(kind=kind(1.0d0)) :: bb
      integer(kind=kind(1)) :: i
      minv = abs(self(1))
      do i = 2,size(self)
        bb = abs(self(i))
        if (bb < minv) minv = bb
      end do
!      res = dzamin(size(self),self,1) ! blas library

   end function

   pure function index_of_largest_value(self) result(ind)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:) :: self
    ! Return the index of the largest value in self
      intent(in) :: self
      integer(kind=kind(1)) :: ind
      integer(kind=kind(1)) :: i
      real(kind=kind(1.0d0)) :: maxv,bb
      maxv = abs(self(1))
      ind = 1
      do i = 2,size(self)
        bb = abs(self(i))
        if (bb > maxv) then
          maxv = bb
          ind = i
        end if
      end do
!      ind = izamax(size(self),self,1) ! blas library

   end function

   pure function index_of_smallest_value(self) result(ind)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:) :: self
    ! Return the index of the smallest value in self
      intent(in) :: self
      integer(kind=kind(1)) :: ind
      integer(kind=kind(1)) :: i
      real(kind=kind(1.0d0)) :: minv,bb
      minv = abs(self(1))
      ind = 1
      do i = 2,size(self)
        bb = abs(self(i))
        if (bb < minv) then
          minv = bb
          ind = i
        end if
      end do
!      ind = izamin(size(self),self,1) ! blas library

   end function

   function alpha(self) result(res)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:) :: self
    ! return the alpha sector of the vector
      TARGET :: self
      complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: res
       integer(kind=kind(1)) :: n

   call ensure_(tonto,is_even_(size(self)),"CPXVEC:alpha ... self is not even-dimensioned")
      n = size(self)/2
      res => self(1:n)

   end function

   function beta(self) result(res)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:) :: self
    ! return the beta sector of the vector
      TARGET :: self
      complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: res
       integer(kind=kind(1)) :: n

   call ensure_(tonto,is_even_(size(self)),"CPXVEC:beta ... self is not even-dimensioned")
      n = size(self)/2
      res => self(n+1:2*n)

   end function

   subroutine set_alpha(self,X)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:) :: self
    ! Set the alpha sector of the vector
       complex(kind=kind((1.0d0,1.0d0))), dimension(:) :: X
       integer(kind=kind(1)) :: n

   call ensure_(tonto,is_even_(size(self)),"CPXVEC:set_alpha ... self is not even-dimensioned")
      n = size(self)/2
      self(1:n) = X

   end subroutine

   subroutine set_beta(self,X)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:) :: self
    ! Set the beta sector of the vector
       complex(kind=kind((1.0d0,1.0d0))), dimension(:) :: X
       integer(kind=kind(1)) :: n

   call ensure_(tonto,is_even_(size(self)),"CPXVEC:set_beta ... self is not even-dimensioned")
      n = size(self)/2
      self(n+1:2*n) = X

   end subroutine

   subroutine set_alpha_1(self,X)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:) :: self
    ! Set the alpha sector of the vector
       real(kind=kind(1.0d0)), dimension(:) :: X
       integer(kind=kind(1)) :: n

   call ensure_(tonto,is_even_(size(self)),"CPXVEC:set_alpha_1 ... self is not even-dimensioned")
      n = size(self)/2
      self(1:n) = X

   end subroutine

   subroutine set_beta_1(self,X)
    complex(kind=kind((1.0d0,1.0d0))), dimension(:) :: self
    ! Set the beta sector of the vector
       real(kind=kind(1.0d0)), dimension(:) :: X
       integer(kind=kind(1)) :: n

   call ensure_(tonto,is_even_(size(self)),"CPXVEC:set_beta_1 ... self is not even-dimensioned")
      n = size(self)/2
      self(n+1:2*n) = X

   end subroutine

end
