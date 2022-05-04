!-------------------------------------------------------------------------------
!
! SHELLPAIR : pair of gaussian SHELLs, incorporating stored intermediate data
! valuable for integral evaluation.
!
! Copyright (C) Dylan Jayatilaka, 2000
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
! $Id: shellpair.foo,v 1.2 2003/02/19 07:48:58 reaper Exp $
!-------------------------------------------------------------------------------

module SHELLPAIR_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

   use REALVEC_MODULE, only: normalising_factors_
   use REALVEC_MODULE, only: create_
   use REALVEC_MODULE, only: destroy_

   use INTMAT3_MODULE, only: create_
   use INTMAT3_MODULE, only: destroy_

   use INTVEC_MODULE, only: create_
   use INTVEC_MODULE, only: destroy_

   use INT_MODULE, only: n_comp_
   use INT_MODULE, only: make_gaussian_xyz_indices_
   use INT_MODULE, only: make_gaussian_xyz_powers_
   use INT_MODULE, only: n_comp_sum_

   use SHELL_MODULE, only: nullify_ptr_part_
   use SHELL_MODULE, only: destroy_ptr_part_
   use SHELL_MODULE, only: unnormalise_
   use SHELL_MODULE, only: copy_

   use INTMAT_MODULE, only: create_
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

   public    make_precalculated_data_
   interface make_precalculated_data_
      module procedure make_precalculated_data
   end interface

   public    copy_a_
   interface copy_a_
      module procedure copy_a
   end interface

   public    nullify_ptr_part_
   interface nullify_ptr_part_
      module procedure nullify_ptr_part
   end interface

   public    copy_b_
   interface copy_b_
      module procedure copy_b
   end interface

   public    destroy_ptr_part_
   interface destroy_ptr_part_
      module procedure destroy_ptr_part
   end interface

   public    create_
   interface create_
      module procedure create
      module procedure create_1
   end interface

   public    unnormalise_
   interface unnormalise_
      module procedure unnormalise
   end interface

   public    create_copy_
   interface create_copy_
      module procedure create_copy
   end interface

   public    destroy_
   interface destroy_
      module procedure destroy
   end interface

   public    copy_
   interface copy_
      module procedure copy
      module procedure copy_1
   end interface

contains

   subroutine create(self)
    type(shellpair_type) :: self
    ! Creates a shell2 object
     pointer :: self

     nullify(self)
     allocate(self)

     call nullify_ptr_part_(self)

   end subroutine

   subroutine create_1(self,shell_a,shell_b)
    type(shellpair_type) :: self
    ! Create a copy of a shell1 objects
     pointer :: self
     type(shell_type), intent(in) :: shell_a,shell_b

     call create_(self)
     call copy_(self,shell_a,shell_b)

   end subroutine

   subroutine destroy(self)
    type(shellpair_type) :: self
    ! Destroys a shell2 object
     pointer :: self

     if (.not. associated(self)) then;   return; end if
     call destroy_ptr_part_(self)

     deallocate(self)

   end subroutine

   subroutine nullify_ptr_part(self)
    type(shellpair_type) :: self
    ! Nullify the pointer parts of self

     call nullify_ptr_part_(self%a)
     call nullify_ptr_part_(self%b)
     nullify(self%exponent_sum)
     nullify(self%exponent_inv)
     nullify(self%a_exponent_inv)
     nullify(self%b_exponent_inv)
     nullify(self%cc_prefactor)
     nullify(self%normalising_factors)
     nullify(self%hrr_comp_to_use)
     nullify(self%hrr_components)
     nullify(self%hrr_index_smaller)
     nullify(self%hrr_index_larger)
     nullify(self%form_3dints_x_indices)
     nullify(self%form_3dints_y_indices)
     nullify(self%form_3dints_z_indices)
     nullify(self%form_3dints_yz_rms_indices)

   end subroutine

   subroutine destroy_ptr_part(self)
    type(shellpair_type) :: self
    ! Destroy the pointer parts of self

     call destroy_ptr_part_(self%a)
     call destroy_ptr_part_(self%b)
     call destroy_(self%exponent_sum)
     call destroy_(self%exponent_inv)
     call destroy_(self%a_exponent_inv)
     call destroy_(self%b_exponent_inv)
     call destroy_(self%cc_prefactor)
     call destroy_(self%normalising_factors)
     call destroy_(self%hrr_comp_to_use)
     call destroy_(self%hrr_components)
     call destroy_(self%hrr_index_smaller)
     call destroy_(self%hrr_index_larger)
     call destroy_(self%form_3dints_x_indices)
     call destroy_(self%form_3dints_y_indices)
     call destroy_(self%form_3dints_z_indices)
     call destroy_(self%form_3dints_yz_rms_indices)

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

   subroutine create_copy(self,ab)
    type(shellpair_type) :: self
    ! Create a copy of the "ab" shellpair
     pointer :: self
     type(shellpair_type), intent(in) :: ab

     call create_(self)
     call copy_(self,ab)

   end subroutine

   subroutine copy(self,ab)
    type(shellpair_type) :: self
    ! Copy the using shellpair "ab" and also set precalculated data.
    ! NOTE : ensure the ptr parts have been destroyed beforehand.
     type(shellpair_type), intent(in) :: ab

     call copy_(self%a,ab%a)
     call copy_(self%b,ab%b)
     call make_precalculated_data_(self)

   end subroutine

   subroutine copy_1(self,shell_a,shell_b)
    type(shellpair_type) :: self
    ! Copy the shell2 using shell1 objects
     type(shell_type), intent(in) :: shell_a,shell_b

     call copy_(self%a,shell_a)
     call copy_(self%b,shell_b)
     call make_precalculated_data_(self)

   end subroutine

   subroutine copy_a(self,shell_a)
    type(shellpair_type) :: self
    ! Copy the "a" shell of the shell2 objects from "shell_a"
     type(shell_type), intent(in) :: shell_a

     call copy_(self%a,shell_a)

   end subroutine

   subroutine copy_b(self,shell_b)
    type(shellpair_type) :: self
    ! Copy the "b" shell of the shell2 objects from "shell_b"
     type(shell_type), intent(in) :: shell_b

     call copy_(self%b,shell_b)

   end subroutine

   subroutine unnormalise(self)
    type(shellpair_type) :: self
    ! Unnormalise each shell

     call unnormalise_(self%a)
     call unnormalise_(self%b)

   end subroutine

   subroutine make_precalculated_data(self)
    type(shellpair_type) :: self
    ! Precalculate some data for the 1/r_{12} electron repulsion integrals
     real(kind=kind(1.0d0)) :: b_cc,a,b,ab_sum,ab_inv
     integer(kind=kind(1)) :: ag,bg,i,j,ub
     real(kind=kind(1.0d0)), dimension(:), pointer :: anorm,bnorm

     self%n_gaussian_pairs = self%a%n_cc*self%b%n_cc
     self%l_max = max(self%a%l,self%b%l)
     self%l_min = min(self%a%l,self%b%l)
     self%l_sum = self%a%l + self%b%l
     call create_(self%exponent_sum,self%n_gaussian_pairs)
     call create_(self%exponent_inv,self%n_gaussian_pairs)
     call create_(self%a_exponent_inv,self%n_gaussian_pairs)
     call create_(self%b_exponent_inv,self%n_gaussian_pairs)
     call create_(self%cc_prefactor,self%n_gaussian_pairs)
     call create_(self%normalising_factors,n_comp_(self%a%l)*n_comp_(self%b%l))
     call create_(anorm,n_comp_(self%a%l))
     call create_(bnorm,n_comp_(self%b%l))
     call normalising_factors_(anorm,self%a%l)
     call normalising_factors_(bnorm,self%b%l)
     i = 0
     do bg = 1,self%b%n_cc
       b      = self%b%ex(bg)
       b_cc   = self%b%cc(bg)
       do ag = 1,self%a%n_cc
         i = i + 1
         a = self%a%ex(ag)
         ab_sum = a + b
         ab_inv = 1.0d0/ab_sum
         self%exponent_sum(i)        = ab_sum
         self%exponent_inv(i)        = ab_inv
         self%cc_prefactor(i)        = b_cc*self%a%cc(ag) *ab_inv*sqrt(ab_inv)
         self%a_exponent_inv(i)      = a*ab_inv
         self%b_exponent_inv(i)      = b*ab_inv
       end do
     end do
     i = 0
     do bg=1,n_comp_(self%b%l)
       do ag=1,n_comp_(self%a%l)
         i = i + 1
         self%normalising_factors(i) = anorm(ag)*bnorm(bg)
       end do
     end do
     call destroy_(bnorm)
     call destroy_(anorm)

     ub = n_comp_sum_(self%l_sum) - n_comp_sum_((self%l_max-1))
     call create_(self%hrr_index_larger,0,self%l_sum,0,self%l_sum,0,self%l_sum)
     call create_(self%hrr_index_smaller,0,self%l_sum,0,self%l_sum,0,self%l_sum)
     call create_(self%form_3dints_x_indices,ub)
     call create_(self%form_3dints_y_indices,ub)
     call create_(self%form_3dints_z_indices,ub)
     call create_(self%hrr_components,3,n_comp_sum_(self%l_sum))
     call create_(self%hrr_comp_to_use,n_comp_sum_(self%l_sum))
     i=0;
     call make_gaussian_xyz_powers_(i,self%hrr_components,self%l_sum,self%hrr_index_smaller,self%hrr_comp_to_use)
     call make_gaussian_xyz_indices_(self%l_max,self%form_3dints_x_indices,self%form_3dints_y_indices, &
                      self%form_3dints_z_indices,self%hrr_index_larger,self%l_sum)
     call create_(self%form_3dints_yz_rms_indices,ub)
     do i=1,ub
       j = self%form_3dints_z_indices(i)
       self%form_3dints_yz_rms_indices(i) = -self%l_sum - 2 + j*(2*self%l_sum+5-j)/2 + &
                                   self%form_3dints_y_indices(i)
     end do

   end subroutine

end
