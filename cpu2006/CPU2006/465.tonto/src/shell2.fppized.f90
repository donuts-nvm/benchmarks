!-------------------------------------------------------------------------------
!
! SHELL2 : pair of SHELLs
!
! Given two shells, can calculate
!  - Fourier transform of their product, evaluated on a grid
!  - overlap integrals
!  - kinetic energy integrals
!  - nuclear attraction integrals
!
! Based on the Rys method, as described by Lindh et al,
! J. Chem. Phys 84(7) 3963-3974
!
! - overlap (non rys version) and kinetic energy integrals work at least
!   up to (q|q).
! - methods using the rys module are limited up to about (m|m) due to
!   the general rys method algorithm failing.  (As tested with atoms on the
!   same center.  Different centers can go higher, but not recommended).
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
! $Id: shell2.foo,v 1.33.2.2.4.1 2004/06/28 10:29:37 reaper Exp $
!-------------------------------------------------------------------------------

module SHELL2_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

   use RYS_MODULE, only: get_weights_
   use RYS_MODULE, only: create_
   use RYS_MODULE, only: destroy_

   use SHELL1_MODULE, only: put_
   use SHELL1_MODULE, only: set_
   use SHELL1_MODULE, only: nullify_ptr_part_
   use SHELL1_MODULE, only: destroy_ptr_part_
   use SHELL1_MODULE, only: copy_

   use INTVEC_MODULE, only: index_of_first_nonzero_value_

   use CPXMAT3_MODULE, only: create_
   use CPXMAT3_MODULE, only: destroy_

   use REALVEC_MODULE, only: normalising_factors_
   use REALVEC_MODULE, only: create_
   use REALVEC_MODULE, only: create_copy_
   use REALVEC_MODULE, only: destroy_

   use CPXMAT4_MODULE, only: create_
   use CPXMAT4_MODULE, only: destroy_

   use INT_MODULE, only: n_comp_
   use INT_MODULE, only: make_gaussian_xyz_indices_
   use INT_MODULE, only: n_comp_sum_
   use INT_MODULE, only: make_gaussian_xyz_powers_

   use GAUSSIAN2_MODULE, only: make_nuclear_attraction_ints_
   use GAUSSIAN2_MODULE, only: set_
   use GAUSSIAN2_MODULE, only: make_ft_r_
   use GAUSSIAN2_MODULE, only: make_NA_1st_deriv_ints_
   use GAUSSIAN2_MODULE, only: make_dipole_ints_
   use GAUSSIAN2_MODULE, only: make_overlap_ints_
   use GAUSSIAN2_MODULE, only: make_T_1st_deriv_ints_
   use GAUSSIAN2_MODULE, only: make_magnetic_jp_ints_
   use GAUSSIAN2_MODULE, only: make_e_coeff_
   use GAUSSIAN2_MODULE, only: make_L_ints_
   use GAUSSIAN2_MODULE, only: make_spin_orbit_B_ints_
   use GAUSSIAN2_MODULE, only: make_magnetic_S_ints_
   use GAUSSIAN2_MODULE, only: make_ft_component_
   use GAUSSIAN2_MODULE, only: make_irrotational_jp_ints_
   use GAUSSIAN2_MODULE, only: make_spin_orbit_ints_
   use GAUSSIAN2_MODULE, only: make_octupole_ints_
   use GAUSSIAN2_MODULE, only: make_E_field_ints_
   use GAUSSIAN2_MODULE, only: make_S_1st_deriv_ints_
   use GAUSSIAN2_MODULE, only: make_solenoidal_jp_ints_
   use GAUSSIAN2_MODULE, only: make_quadrupole_ints_
   use GAUSSIAN2_MODULE, only: make_E_gradient_ints_
   use GAUSSIAN2_MODULE, only: make_ft_nabla_
   use GAUSSIAN2_MODULE, only: make_ft_

   use INTMAT_MODULE, only: create_
   use INTMAT_MODULE, only: destroy_

   use REALMAT3_MODULE, only: create_
   use REALMAT3_MODULE, only: destroy_

   use REALMAT4_MODULE, only: create_
   use REALMAT4_MODULE, only: destroy_

   use INTMAT3_MODULE, only: make_index_of_components_
   use INTMAT3_MODULE, only: create_
   use INTMAT3_MODULE, only: destroy_

   use TEXTFILE_MODULE, only: stdin
   use TEXTFILE_MODULE, only: stdout
   use TEXTFILE_MODULE, only: put_
   use TEXTFILE_MODULE, only: flush_

   use REALMAT_MODULE, only: create_
   use REALMAT_MODULE, only: destroy_

   use CPXVEC_MODULE, only: create_
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

   public    make_nuclear_attraction_ints_
   interface make_nuclear_attraction_ints_
      module procedure make_nuclear_attraction_ints
   end interface

   public    nullify_ptr_part_
   interface nullify_ptr_part_
      module procedure nullify_ptr_part
   end interface

   private    make_nuc_es_
   interface make_nuc_es_
      module procedure make_nuc_es
   end interface

   public    make_T_1st_deriv_ints_
   interface make_T_1st_deriv_ints_
      module procedure make_T_1st_deriv_ints
   end interface

   public    skip_ft_
   interface skip_ft_
      module procedure skip_ft
      module procedure skip_ft_1
   end interface

   private    transfer_l_a_highest_
   interface transfer_l_a_highest_
      module procedure transfer_l_a_highest
   end interface

   public    destroy_ptr_part_
   interface destroy_ptr_part_
      module procedure destroy_ptr_part
   end interface

   public    make_gaussian_partition_
   interface make_gaussian_partition_
      module procedure make_gaussian_partition
   end interface

   private    form_overlap_mat_a_
   interface form_overlap_mat_a_
      module procedure form_overlap_mat_a
   end interface

   private    form_overlap_mat_b_
   interface form_overlap_mat_b_
      module procedure form_overlap_mat_b
   end interface

   public    precalculate_
   interface precalculate_
      module procedure precalculate
   end interface

   private    transfer_l_b_highest_
   interface transfer_l_b_highest_
      module procedure transfer_l_b_highest
   end interface

   public    make_kei_
   interface make_kei_
      module procedure make_kei
   end interface

   public    make_E_field_ints_
   interface make_E_field_ints_
      module procedure make_E_field_ints
   end interface

   public    put_
   interface put_
      module procedure put
   end interface

   private    combine_nuc_2d_ints_
   interface combine_nuc_2d_ints_
      module procedure combine_nuc_2d_ints
   end interface

   public    normalise_
   interface normalise_
      module procedure normalise
   end interface

   public    make_solenoidal_jp_ints_
   interface make_solenoidal_jp_ints_
      module procedure make_solenoidal_jp_ints
   end interface

   public    make_E_gradient_ints_
   interface make_E_gradient_ints_
      module procedure make_E_gradient_ints
   end interface

   public    make_quadrupole_ints_
   interface make_quadrupole_ints_
      module procedure make_quadrupole_ints
   end interface

   private    make_nuc_dp_
   interface make_nuc_dp_
      module procedure make_nuc_dp
   end interface

   public    copy_
   interface copy_
      module procedure copy
      module procedure copy_1
      module procedure copy_2
      module procedure copy_3
   end interface

   public    make_ft_nabla_
   interface make_ft_nabla_
      module procedure make_ft_nabla
      module procedure make_ft_nabla_1
   end interface

   private    make_nuc_ss_
   interface make_nuc_ss_
      module procedure make_nuc_ss
   end interface

   public    normalise_ft_
   interface normalise_ft_
      module procedure normalise_ft
   end interface

   private    make_nuc_ds_
   interface make_nuc_ds_
      module procedure make_nuc_ds
   end interface

   public    set_
   interface set_
      module procedure set
      module procedure set_1
   end interface

   private    form_overlap_es_
   interface form_overlap_es_
      module procedure form_overlap_es
   end interface

   public    create_
   interface create_
      module procedure create
      module procedure create_1
      module procedure create_2
   end interface

   public    make_ft_r_
   interface make_ft_r_
      module procedure make_ft_r
   end interface

   public    create_copy_
   interface create_copy_
      module procedure create_copy
   end interface

   public    make_NA_1st_deriv_ints_
   interface make_NA_1st_deriv_ints_
      module procedure make_NA_1st_deriv_ints
   end interface

   private    s_overlap_
   interface s_overlap_
      module procedure s_overlap
   end interface

   public    make_overlap_
   interface make_overlap_
      module procedure make_overlap
   end interface

   public    make_dipole_ints_
   interface make_dipole_ints_
      module procedure make_dipole_ints
   end interface

   public    make_overlap_ints_
   interface make_overlap_ints_
      module procedure make_overlap_ints
   end interface

   public    make_magnetic_jp_ints_
   interface make_magnetic_jp_ints_
      module procedure make_magnetic_jp_ints
   end interface

   private    make_overlap_es_
   interface make_overlap_es_
      module procedure make_overlap_es
   end interface

   public    make_spin_orbit_B_ints_
   interface make_spin_orbit_B_ints_
      module procedure make_spin_orbit_B_ints
   end interface

   public    make_L_ints_
   interface make_L_ints_
      module procedure make_L_ints
   end interface

   public    make_tanaka_thermal_smearing_
   interface make_tanaka_thermal_smearing_
      module procedure make_tanaka_thermal_smearing
   end interface

   public    make_magnetic_S_ints_
   interface make_magnetic_S_ints_
      module procedure make_magnetic_S_ints
      module procedure make_magnetic_S_ints_1
   end interface

   public    destroy_
   interface destroy_
      module procedure destroy
   end interface

   public    make_irrotational_jp_ints_
   interface make_irrotational_jp_ints_
      module procedure make_irrotational_jp_ints
   end interface

   public    make_spin_orbit_ints_
   interface make_spin_orbit_ints_
      module procedure make_spin_orbit_ints
   end interface

   private    make_nuc_pp_
   interface make_nuc_pp_
      module procedure make_nuc_pp
   end interface

   public    make_octupole_ints_
   interface make_octupole_ints_
      module procedure make_octupole_ints
   end interface

   public    make_S_1st_deriv_ints_
   interface make_S_1st_deriv_ints_
      module procedure make_S_1st_deriv_ints
   end interface

   public    get_kei_
   interface get_kei_
      module procedure get_kei
   end interface

   private    make_nuc_ps_
   interface make_nuc_ps_
      module procedure make_nuc_ps
   end interface

   public    get_nuc_
   interface get_nuc_
      module procedure get_nuc
   end interface

   public    transfer_
   interface transfer_
      module procedure transfer
   end interface

   public    make_ft_
   interface make_ft_
      module procedure make_ft
      module procedure make_ft_1
   end interface

contains

   subroutine create(self)
    type(shell2_type) :: self
    ! Creates a shell2 object
     pointer :: self

     nullify(self)
     allocate(self)

     call nullify_ptr_part_(self)

   end subroutine

   subroutine create_1(self,shell_a,shell_b)
    type(shell2_type) :: self
    ! Create a copy of a shell1 objects
     pointer :: self
     type(shell1_type), intent(in) :: shell_a,shell_b

     call create_(self)
     call copy_(self,shell_a,shell_b)

   end subroutine

   subroutine create_2(self,shell_a,shell_b,pos_a,pos_b)
    type(shell2_type) :: self
    ! Create a copy of a shell1 objects with positions
     pointer :: self
     type(shell_type), intent(in) :: shell_a,shell_b
     real(kind=kind(1.0d0)), dimension(3), intent(in) :: pos_a,pos_b

     call create_(self)
     call copy_(self,shell_a,shell_b,pos_a,pos_b)

   end subroutine

   subroutine destroy(self)
    type(shell2_type) :: self
    ! Destroys a shell2 object
     pointer :: self

     if (.not. associated(self)) then;   return; end if
     call destroy_ptr_part_(self)

     deallocate(self)

   end subroutine

   subroutine nullify_ptr_part(self)
    type(shell2_type) :: self
    ! Nullify the pointer parts of self

      call nullify_ptr_part_(self%a)
      call nullify_ptr_part_(self%b)
      nullify(self%exponent_sum)
      nullify(self%exponent_inv)
      nullify(self%a_exponent_inv)
      nullify(self%b_exponent_inv)
      nullify(self%cc_prefactor)
      nullify(self%normalising_factors)
      nullify(self%pair_center)
      nullify(self%center_diff)

   end subroutine

   subroutine destroy_ptr_part(self)
    type(shell2_type) :: self
    ! Destroy the pointer parts of self

      call destroy_ptr_part_(self%a)
      call destroy_ptr_part_(self%b)
      call destroy_(self%exponent_sum)
      call destroy_(self%exponent_inv)
      call destroy_(self%a_exponent_inv)
      call destroy_(self%b_exponent_inv)
      call destroy_(self%cc_prefactor)
      call destroy_(self%normalising_factors)
      call destroy_(self%pair_center)
      call destroy_(self%center_diff)

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

   subroutine create_copy(self,shell)
    type(shell2_type) :: self
    ! Create a copy from "shell"
     pointer :: self
     type(shell2_type), intent(in) :: shell

     call create_(self)
     call copy_(self,shell)
     call precalculate_(self)

   end subroutine

   subroutine copy(self,shell)
    type(shell2_type) :: self
    ! Make a copy from "shell"
     type(shell2_type), intent(in) :: shell

     call copy_(self%a,shell%a)
     call copy_(self%b,shell%b)
     call precalculate_(self)

   end subroutine

   subroutine copy_1(self,shell_a,shell_b)
    type(shell2_type) :: self
    ! Copy the shell2 using shell1 objects
     type(shell1_type), intent(in) :: shell_a,shell_b

     call copy_(self%a,shell_a)
     call copy_(self%b,shell_b)
     call precalculate_(self)

   end subroutine

   subroutine copy_2(self,shell_a,shell_b,pos_a,pos_b)
    type(shell2_type) :: self
    ! Copy the shell2 using shell objects and positions
     type(shell_type), intent(in) :: shell_a,shell_b
     real(kind=kind(1.0d0)), dimension(:), intent(in) :: pos_a,pos_b

     call copy_(self%a,shell_a,pos_a)
     call copy_(self%b,shell_b,pos_b)
     call precalculate_(self)

   end subroutine

   subroutine set(self,shell_a,shell_b)
    type(shell2_type) :: self
    ! Set the shell2 using shell1 objects
     type(shell1_type), intent(in) :: shell_a,shell_b

     call set_(self%a,shell_a)
     call set_(self%b,shell_b)
     call precalculate_(self)

   end subroutine

   subroutine set_1(self,shell_a,shell_b,pos_a,pos_b)
    type(shell2_type) :: self
    ! Copy the shell2 using shell objects
     type(shell_type), intent(in) :: shell_a,shell_b
     real(kind=kind(1.0d0)), dimension(:), intent(in) :: pos_a,pos_b

     call set_(self%a,shell_a,pos_a)
     call set_(self%b,shell_b,pos_b)
     call precalculate_(self)

   end subroutine

   subroutine copy_3(self,shellpr,pos_a,pos_b)
    type(shell2_type) :: self
    ! Copy the parts of the shell2 from shell1 objects
     type(shellpair_type), intent(in) :: shellpr
     real(kind=kind(1.0d0)), dimension(:), intent(in) :: pos_a,pos_b
     real(kind=kind(1.0d0)), dimension(3) :: AB,At,P,b_pos_b
     real(kind=kind(1.0d0)) :: r2ab,a,b,b_r2ab,inv
     integer(kind=kind(1)) :: i,bg,ag

     call copy_(self%a,shellpr%a,pos_a)
     call copy_(self%b,shellpr%b,pos_b)
     self%n_gaussian_pairs = shellpr%n_gaussian_pairs
     self%l_max = shellpr%l_max
     self%l_min = shellpr%l_min
     self%l_sum = shellpr%l_sum
     call create_copy_(self%exponent_sum,shellpr%exponent_sum)
     call create_copy_(self%exponent_inv,shellpr%exponent_inv)
     call create_copy_(self%a_exponent_inv,shellpr%a_exponent_inv)
     call create_copy_(self%b_exponent_inv,shellpr%b_exponent_inv)
     call create_copy_(self%cc_prefactor,shellpr%cc_prefactor)
     call create_(self%pair_center,3,self%n_gaussian_pairs)
     call create_(self%center_diff,3,self%n_gaussian_pairs)
     call create_copy_(self%normalising_factors,shellpr%normalising_factors)
      AB = pos_a-pos_b
       ! Want position of shell1 with higher angular momentum.
      if (self%a%l > self%b%l) then; At = pos_a
      else;                  At = pos_b
      end if
      r2ab = dot_product(AB,AB)
      i = 0
      do bg = 1,self%b%n_cc
        b       = self%b%ex(bg)
        b_r2ab  = b*r2ab
        b_pos_b = b*pos_b
        do ag = 1,self%a%n_cc
          i = i + 1
          a = self%a%ex(ag)
          inv = self%exponent_inv(i)
          self%cc_prefactor(i)  = shellpr%cc_prefactor(i)*exp(-a*b_r2ab*inv)
          P = (b_pos_b + a*pos_a) * inv
          self%pair_center(:,i) = P
          self%center_diff(:,i) = P - At
        end do
      end do
     self%kappa_max = maxval(self%cc_prefactor/self%exponent_inv**(3/2))

   end subroutine

   subroutine precalculate(self)
    type(shell2_type) :: self
    !
     real(kind=kind(1.0d0)), dimension(:), pointer :: anorm,bnorm
     real(kind=kind(1.0d0)), dimension(3) :: AB,At,P
     real(kind=kind(1.0d0)) :: b_cc,a,b,exp_sum,exp_inv,r2ab,b_r2ab,a_exp_inv
     integer(kind=kind(1)) :: ag,bg,i

     self%n_gaussian_pairs = self%a%n_cc*self%b%n_cc
     self%l_max = max(self%a%l,self%b%l)
     self%l_min = min(self%a%l,self%b%l)
     self%l_sum = self%a%l + self%b%l
     call create_(self%exponent_sum,self%n_gaussian_pairs)
     call create_(self%exponent_inv,self%n_gaussian_pairs)
     call create_(self%a_exponent_inv,self%n_gaussian_pairs)
     call create_(self%b_exponent_inv,self%n_gaussian_pairs)
     call create_(self%cc_prefactor,self%n_gaussian_pairs)
     call create_(self%pair_center,3,self%n_gaussian_pairs)
     call create_(self%center_diff,3,self%n_gaussian_pairs)
     call create_(self%normalising_factors,n_comp_(self%a%l)*n_comp_(self%b%l))
     call create_(anorm,n_comp_(self%a%l))
     call create_(bnorm,n_comp_(self%b%l))
     AB = self%a%pos-self%b%pos
      ! Want position of shell1 with higher angular momentum.
     if (self%a%l > self%b%l) then; At = self%a%pos
     else;                  At = self%b%pos
     end if
     r2ab = dot_product(AB,AB)
     i = 0
     do bg = 1,self%b%n_cc
       b      = self%b%ex(bg)
       b_r2ab = b * r2ab
       b_cc   = self%b%cc(bg)
       do ag = 1,self%a%n_cc
         i = i + 1
         a = self%a%ex(ag)
         exp_sum = a + b
         exp_inv = 1.0d0/exp_sum
         P = (b*self%b%pos + a*self%a%pos) * exp_inv
         self%exponent_sum(i)        = exp_sum
         self%exponent_inv(i)        = exp_inv
         a_exp_inv               = a*exp_inv
         self%a_exponent_inv(i)      = a_exp_inv
         self%b_exponent_inv(i)      = b*exp_inv
         self%cc_prefactor(i)        = b_cc*self%a%cc(ag) *exp_inv*sqrt(exp_inv)* &
                                                        exp(-b_r2ab*a_exp_inv)
         self%pair_center(:,i) = P
         self%center_diff(:,i) = P - At
       end do
     end do
     self%kappa_max = maxval(self%cc_prefactor/self%exponent_inv**(3/2))
     call normalising_factors_(anorm,self%a%l)
     call normalising_factors_(bnorm,self%b%l)
     i = 0
     do bg=1,n_comp_(self%b%l)
       do ag=1,n_comp_(self%a%l)
         i = i + 1
         self%normalising_factors(i) = anorm(ag)*bnorm(bg)
       end do
     end do
     call destroy_(bnorm)
     call destroy_(anorm)

   end subroutine

!*******************************************************************************
!    Fourier transform integrals
!*******************************************************************************

   subroutine normalise_ft(self,ft)
    type(shell2_type) :: self
    ! Multiplies the ft product at a series of k points, by the normalisation
    ! factors for the two gaussian shells.
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:), target :: ft
     complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: ft_ab
     integer(kind=kind(1)) :: a,b,i

     if (self%a%l>1 .or. self%b%l>1) then
       i = 0
       do b = 1, self%b%n_comp
         do a = 1, self%a%n_comp
           i = i + 1
           ft_ab => ft(:,a,b)
           ft_ab(:) = ft_ab(:) * self%normalising_factors(i)
         end do
       end do
     end if

   end subroutine

   function skip_ft(self,cutoff) result(res)
    type(shell2_type) :: self
    ! Whether the ft for this shell pair is too small.
     intent(in) :: self
     real(kind=kind(1.0d0)), intent(in) :: cutoff
     logical(kind=kind(.true.)) :: res
     real(kind=kind(1.0d0)) :: R2,gamma,g1,e2,ex,ey,ez,fac,e000x,e000y,e000z
     real(kind=kind(1.0d0)), dimension(3) :: AB
     integer(kind=kind(1)) :: a,b,t_max
     real(kind=kind(1.0d0)), dimension(:,:,:), pointer :: e
     type(gaussian2_type) :: G

     call set_(G,self%a%l,self%a%pos,0.0d0,self%b%l,self%b%pos,0.0d0)
     t_max = self%a%l + self%b%l
     call create_(e,(/0,t_max/),(/0,self%a%l/),(/0,self%b%l/))
     e = 0.0d0
     AB = (self%a%pos-self%b%pos)
     R2 = dot_product(AB,AB)
     res = .true.
     do a = 1, self%a%n_cc
       call set_(G,ex_a=self%a%ex(a))
       do b = 1, self%b%n_cc
         call set_(G,ex_b=self%b%ex(b))
         call make_e_coeff_(G,e,1);   ex=maxval(e); e000x = e(0,0,0)
         call make_e_coeff_(G,e,2);   ey=maxval(e); e000y = e(0,0,0)
         call make_e_coeff_(G,e,3);   ez=maxval(e); e000z = e(0,0,0)
         gamma = self%a%ex(a) + self%b%ex(b)
         g1 = 1.0d0/gamma
         e2 = ex*ey*ez*g1*sqrt(g1)*e000x*e000y*e000z
         if (e2>cutoff) then
           res = .false.
           call destroy_(e)
             return
         end if
       end do
     end do
     call destroy_(e)

   end function

   function skip_ft_1(self,Pmax,cutoff) result(res)
    type(shell2_type) :: self
    ! Whether the ft for this shell pair is too small.
     intent(in) :: self
     real(kind=kind(1.0d0)), intent(in) :: Pmax,cutoff
     logical(kind=kind(.true.)) :: res
     real(kind=kind(1.0d0)) :: R2,gamma,g1,e2,ex,ey,ez,fac,e000x,e000y,e000z
     real(kind=kind(1.0d0)), dimension(3) :: AB
     integer(kind=kind(1)) :: a,b,t_max
      real(kind=kind(1.0d0)), dimension(:,:,:), pointer :: e
      type(gaussian2_type) :: G

     call set_(G,self%a%l,self%a%pos,0.0d0,self%b%l,self%b%pos,0.0d0)
     t_max = self%a%l + self%b%l
     call create_(e,(/0,t_max/),(/0,self%a%l/),(/0,self%b%l/))
     e = 0.0d0
     AB = (self%a%pos-self%b%pos)
     R2 = dot_product(AB,AB)
     res = .true.
     do a = 1, self%a%n_cc
       call set_(G,ex_a=self%a%ex(a))
       fac=self%a%cc(a)*Pmax
       do b = 1, self%b%n_cc
         call set_(G,ex_b=self%b%ex(b))
         call make_e_coeff_(G,e,1);   ex=maxval(e); e000x = e(0,0,0)
         call make_e_coeff_(G,e,2);   ey=maxval(e); e000y = e(0,0,0)
         call make_e_coeff_(G,e,3);   ez=maxval(e); e000z = e(0,0,0)
         gamma = self%a%ex(a) + self%b%ex(b)
         g1 = 1.0d0/gamma
          !e2 = ex*ey*ez*(g1)**(3.0d0/2.0d0)*exp(-.a.ex(a)*.b.ex(b)*g1*R2)
         e2 = ex*ey*ez*g1*sqrt(g1)*e000x*e000y*e000z
         if (e2*self%b%cc(b)*fac>cutoff) then
           res = .false.
           call destroy_(e)
             return
         end if
       end do
     end do
     call destroy_(e)

   end function

   subroutine make_ft(self,res,k_pts)
    type(shell2_type) :: self
    ! Calculates the Fourier transform for a product of two contracted
    ! gaussian shells, evaluated at a series of k points k_pts
    ! Dimensions of res are [k_max,.a.n_comp,.b.n_comp].
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:), target :: res
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: k_pts
      type(gaussian2_type) :: G
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:), pointer :: ft_ab
      integer(kind=kind(1)) :: a,b,k_max,i,j,n_comp_a,n_comp_b
      real(kind=kind(1.0d0)) :: ca,fac
      complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: res_ij

      n_comp_a = self%a%n_comp; n_comp_b = self%b%n_comp
      k_max  = size(k_pts,1)
      res = 0.0d0
      call set_(G,self%a%l,self%a%pos,0.0d0,self%b%l,self%b%pos,0.0d0)
      call create_(ft_ab,k_max,n_comp_a,n_comp_b)
      do a = 1, self%a%n_cc
         call set_(G,ex_a=self%a%ex(a))
         ca = self%a%cc(a)
         do b = 1, self%b%n_cc
            call set_(G,ex_b=self%b%ex(b))
            call make_ft_(G,ft_ab,k_pts)
            fac = ca * self%b%cc(b)
            do j=1,n_comp_b
              do i=1,n_comp_a
                res_ij => res(:,i,j)
                res_ij(:) = res_ij(:) + ft_ab(:,i,j) * fac
              end do
            end do
         end do
      end do
      call destroy_(ft_ab)
      call normalise_ft_(self,res)

   end subroutine

   subroutine make_ft_1(self,res,k_pts,thermal,partition)
    type(shell2_type) :: self
    ! Calculates the Fourier transform for a product of two contracted
    ! gaussian shells, evaluated at a series of k points k_pts
    ! Dimensions of res are [k_max,.a.n_comp,.b.n_comp].
    ! Also adds in thermal correction and partition factors.
    ! This version has the gaussian2 stuff inlined and special cases optimised,
    ! it's pretty messy but much faster.
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:), target :: res
      real(kind=kind(1.0d0)), dimension(:,:), target :: k_pts
      real(kind=kind(1.0d0)), dimension(:,:,:), target :: thermal
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: partition
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:), pointer :: ft_x,ft_y,ft_z
      type(gaussian2_type) :: G
      real(kind=kind(1.0d0)), dimension(:), pointer :: therm,k_x,k_y,k_z,k_x2,k_y2,k_z2
      real(kind=kind(1.0d0)), dimension(:,:,:), pointer :: e
      complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: res_ij,prefac,ft_xb,ft_yb,ft_zb
      integer(kind=kind(1)), dimension(:,:), pointer :: la,lb
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: ft_xbx,ft_yby,ft_zbz
      complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: ft_x01,ft_x10,ft_y01,ft_y10,ft_z01,ft_z10
      real(kind=kind(1.0d0)), dimension(3) :: pos
      real(kind=kind(1.0d0)) :: ca,fac
      real(kind=kind(1.0d0)) :: g1_exa,g1_exb,e00,ex_a,ex_b
      real(kind=kind(1.0d0)) :: pifac,gamma,g1,g4,dot1,dot2,k1,k2,k3,P1,P2,P3,PI_on_gamma
      integer(kind=kind(1)) :: n_a,n_b,a,b,k,ax,ay,az,bx,by,bz,bxlast,bylast,bzlast
      integer(kind=kind(1)) :: l_a,l_b,aa,bb,k_max
      complex(kind=kind((1.0d0,1.0d0))) :: fac1
      real(kind=kind(1.0d0)) :: Ex000,Ey000,Ez000,Ex001,Ey001,Ez001,Ex101,Ey101,Ez101
      real(kind=kind(1.0d0)) :: Ex010,Ey010,Ez010,Ex110,Ey110,Ez110,Ex111,Ey111,Ez111
      real(kind=kind(1.0d0)) :: Ex211,Ey211,Ez211,Ex011,Ey011,Ez011
      real(kind=kind(1.0d0)) :: PA1,PA2,PA3,PB1,PB2,PB3,pp

      n_a = self%a%n_comp
      n_b = self%b%n_comp
      l_a = self%a%l
      l_b = self%b%l
      k_max  = size(k_pts,1)
      res = 0.0d0
      call set_(G,l_a,self%a%pos,0.0d0,l_b,self%b%pos,0.0d0)

      select case (l_a)
        case (0)
          select case(l_b)
            case (0)
              k_x => k_pts(:,1)
              k_y => k_pts(:,2)
              k_z => k_pts(:,3)
              pos = self%a%pos - self%b%pos
              pp = dot_product(pos,pos)
              res_ij => res(:,1,1)
              PA1 = self%a%pos(1)
              PA2 = self%a%pos(2)
              PA3 = self%a%pos(3)
              PB1 = self%b%pos(1)
              PB2 = self%b%pos(2)
              PB3 = self%b%pos(3)
              do a = 1, self%a%n_cc
                ca = self%a%cc(a)
                ex_a = self%a%ex(a)
                do b = 1, self%b%n_cc
                  ex_b = self%b%ex(b)
                  g1 = 1.0d0/(ex_a+ex_b)
                  g4 = -0.25d0*g1
                  PI_on_gamma = 3.141592653589793d0*g1
                  pifac = sqrt(PI_on_gamma) * PI_on_gamma
                  g1_exa = g1*ex_a
                  g1_exb = g1*ex_b
                  P1 = g1_exa*PA1 + g1_exb*PB1
                  P2 = g1_exa*PA2 + g1_exb*PB2
                  P3 = g1_exa*PA3 + g1_exb*PB3
                  e00 = exp(-ex_a*ex_b*pp*g1)
                  fac1= cmplx(pifac*e00,0.0d0,kind=kind((1.0d0,1.0d0))) * ca * self%b%cc(b) * partition(a,b)
                  therm => thermal(:,a,b)
                  do k = 1, k_max
                    k1 = k_x(k);    k2 = k_y(k);    k3 = k_z(k)
                    dot1 = k1*P1+k2*P2+k3*P3
                    dot2 = g4 * (k1*k1+k2*k2+k3*k3)
                    res_ij(k) = res_ij(k) + therm(k) * (fac1 * exp(cmplx(dot2,dot1,kind=kind((1.0d0,1.0d0)))))
                  end do
                end do
              end do
            case (1)
              call create_(prefac,k_max)
              k_x => k_pts(:,1)
              k_y => k_pts(:,2)
              k_z => k_pts(:,3)
              PA1 = self%a%pos(1)
              PA2 = self%a%pos(2)
              PA3 = self%a%pos(3)
              PB1 = self%b%pos(1)
              PB2 = self%b%pos(2)
              PB3 = self%b%pos(3)
              call create_(e,(/0,1/),(/0,0/),(/0,1/))
              do aa = 1, self%a%n_cc
                ca = self%a%cc(aa)
                ex_a=self%a%ex(aa)
                call set_(G,ex_a=ex_a)
                do bb = 1, self%b%n_cc
                  ex_b=self%b%ex(bb)
                  call set_(G,ex_b=ex_b)
                  gamma = G%a%ex+G%b%ex
                  g1 = 1.0d0/gamma
                  g4 = 0.25d0*g1
                  PI_on_gamma = 3.141592653589793d0*g1
                  pifac = sqrt(PI_on_gamma) * PI_on_gamma
                  g1_exa = g1*G%a%ex
                  g1_exb = g1*G%b%ex
                  P1 = g1_exa*PA1 + g1_exb*PB1
                  P2 = g1_exa*PA2 + g1_exb*PB2
                  P3 = g1_exa*PA3 + g1_exb*PB3
                  call make_e_coeff_(G,e,1)
                  Ex000 = e(0,0,0)
                  Ex001 = e(0,0,1)
                  Ex101 = e(1,0,1)
                  call make_e_coeff_(G,e,2)
                  Ey000 = e(0,0,0)
                  Ey001 = e(0,0,1)
                  Ey101 = e(1,0,1)
                  call make_e_coeff_(G,e,3)
                  Ez000 = e(0,0,0)
                  Ez001 = e(0,0,1)
                  Ez101 = e(1,0,1)
                  fac = pifac * ca * self%b%cc(bb) * partition(aa,bb)
                  therm => thermal(:,aa,bb)
                  do k = 1,k_max
                    k1 = k_x(k);    k2 = k_y(k);    k3 = k_z(k)
                    dot1 = k1*P1+k2*P2+k3*P3
                    dot2 = -g4 * (k1*k1+k2*k2+k3*k3)
                    prefac(k) = fac*exp(cmplx(dot2,dot1,kind=kind((1.0d0,1.0d0))))*therm(k)
                  end do
                  res_ij => res(:,1,1)
                  res_ij(:) = res_ij(:) + cmplx(Ex001,Ex101*k_x,kind=kind((1.0d0,1.0d0)))*Ey000*Ez000*prefac(:)
                  res_ij => res(:,1,2)
                  res_ij(:) = res_ij(:) + Ex000*cmplx(Ey001,Ey101*k_y,kind=kind((1.0d0,1.0d0)))*Ez000*prefac(:)
                  res_ij => res(:,1,3)
                  res_ij(:) = res_ij(:) + Ex000*Ey000*cmplx(Ez001,Ez101*k_z,kind=kind((1.0d0,1.0d0)))*prefac(:)
                end do
              end do
              call destroy_(e)
              call destroy_(prefac)
            case default
              call create_(lb,3,n_b);   call make_gaussian_xyz_powers_(l_b,lb)
              call create_(ft_x,(/1,k_max/),(/0,l_a/),(/0,l_b/))
              call create_(ft_y,(/1,k_max/),(/0,l_a/),(/0,l_b/))
              call create_(ft_z,(/1,k_max/),(/0,l_a/),(/0,l_b/))
              call create_(prefac,k_max)
              PA1 = self%a%pos(1)
              PA2 = self%a%pos(2)
              PA3 = self%a%pos(3)
              PB1 = self%b%pos(1)
              PB2 = self%b%pos(2)
              PB3 = self%b%pos(3)
              k_x => k_pts(:,1)
              k_y => k_pts(:,2)
              k_z => k_pts(:,3)
              do aa = 1, self%a%n_cc
                ca = self%a%cc(aa)
                ex_a=self%a%ex(aa)
                call set_(G,ex_a=ex_a)
                do bb = 1, self%b%n_cc
                  ex_b=self%b%ex(bb)
                  call set_(G,ex_b=ex_b)
                  gamma = G%a%ex+G%b%ex
                  g1 = 1.0d0/gamma
                  g4 = 0.25d0*g1
                  PI_on_gamma = 3.141592653589793d0*g1
                  pifac = sqrt(PI_on_gamma) * PI_on_gamma
                  g1_exa = g1*G%a%ex
                  g1_exb = g1*G%b%ex
                  P1 = g1_exa*PA1 + g1_exb*PB1
                  P2 = g1_exa*PA2 + g1_exb*PB2
                  P3 = g1_exa*PA3 + g1_exb*PB3
                  call make_ft_component_(G,ft_x,1,k_x,k_max)
                  call make_ft_component_(G,ft_y,2,k_y,k_max)
                  call make_ft_component_(G,ft_z,3,k_z,k_max)
                  fac = pifac * ca * self%b%cc(bb) * partition(aa,bb)
                  therm => thermal(:,aa,bb)
                  do k = 1,k_max
                    k1 = k_x(k);    k2 = k_y(k);    k3 = k_z(k)
                    dot1 = k1*P1+k2*P2+k3*P3
                    dot2 = -g4 * (k1*k1+k2*k2+k3*k3)
                    prefac(k) = fac*exp(cmplx(dot2,dot1,kind=kind((1.0d0,1.0d0)))) * therm(k)
                  end do
                  do b=0,l_b  ! incorporate prefac into ft_z
                    ft_z(:,0,b) = ft_z(:,0,b) * prefac(:)
                  end do
                  bxlast = lb(1,1)
                  bylast = lb(2,1)
                  bzlast = lb(3,1)
                  ft_xb => ft_x(:,0,bxlast)
                  ft_yb => ft_y(:,0,bylast)
                  ft_zb => ft_z(:,0,bzlast)

                  do b = 1,n_b
                    bx = lb(1,b)
                    by = lb(2,b)
                    bz = lb(3,b)
                    if (bxlast/=bx) then
                      ft_xb => ft_x(:,0,bx)
                      bxlast = bx
                    end if
                    if (bylast/=by) then
                      ft_yb=> ft_y(:,0,by)
                      bylast = by
                    end if
                    if (bzlast/=bz) then
                      ft_zb=> ft_z(:,0,bz)
                      bzlast = bz
                    end if
                    res_ij => res(:,1,b)
                    res_ij(:) = res_ij(:) + ft_xb(:)*ft_yb(:)*ft_zb(:)
                  end do
                end do
              end do
              call destroy_(lb)
              call destroy_(prefac)
              call destroy_(ft_x)
              call destroy_(ft_y)
              call destroy_(ft_z)
              call normalise_ft_(self,res)
          end select
        case (1)
          select case (l_b)
            case (0)
              call create_(prefac,k_max)
              call create_(e,(/0,1/),(/0,1/),(/0,0/))
              PA1 = self%a%pos(1)
              PA2 = self%a%pos(2)
              PA3 = self%a%pos(3)
              PB1 = self%b%pos(1)
              PB2 = self%b%pos(2)
              PB3 = self%b%pos(3)
              k_x => k_pts(:,1)
              k_y => k_pts(:,2)
              k_z => k_pts(:,3)
              do aa = 1, self%a%n_cc
                ca = self%a%cc(aa)
                ex_a=self%a%ex(aa)
                call set_(G,ex_a=ex_a)
                do bb = 1, self%b%n_cc
                  ex_b=self%b%ex(bb)
                  call set_(G,ex_b=ex_b)
                  gamma = G%a%ex+G%b%ex
                  g1 = 1.0d0/gamma
                  g4 = 0.25d0*g1
                  PI_on_gamma = 3.141592653589793d0*g1
                  pifac = sqrt(PI_on_gamma) * PI_on_gamma
                  g1_exa = g1*G%a%ex
                  g1_exb = g1*G%b%ex
                  P1 = g1_exa*PA1 + g1_exb*PB1
                  P2 = g1_exa*PA2 + g1_exb*PB2
                  P3 = g1_exa*PA3 + g1_exb*PB3
                  call make_e_coeff_(G,e,1)
                  Ex000 = e(0,0,0)
                  Ex010 = e(0,1,0)
                  Ex110 = e(1,1,0)
                  call make_e_coeff_(G,e,2)
                  Ey000 = e(0,0,0)
                  Ey010 = e(0,1,0)
                  Ey110 = e(1,1,0)
                  call make_e_coeff_(G,e,3)
                  Ez000 = e(0,0,0)
                  Ez010 = e(0,1,0)
                  Ez110 = e(1,1,0)
                  fac = pifac * ca * self%b%cc(bb) * partition(aa,bb)
                  therm => thermal(:,aa,bb)
                  do k = 1,k_max
                    k1 = k_x(k);    k2 = k_y(k);    k3 = k_z(k)
                    dot1 = k1*P1+k2*P2+k3*P3
                    dot2 = -g4 * (k1*k1+k2*k2+k3*k3)
                    prefac(k) = fac*exp(cmplx(dot2,dot1,kind=kind((1.0d0,1.0d0)))) * therm(k)
                  end do
                  res_ij => res(:,1,1)
                  res_ij(:) = res_ij(:) + cmplx(Ex010,Ex110*k_x(:),kind=kind((1.0d0,1.0d0)))*Ey000*Ez000*prefac(:)
                  res_ij => res(:,2,1)
                  res_ij(:) = res_ij(:) + Ex000*cmplx(Ey010,Ey110*k_y(:),kind=kind((1.0d0,1.0d0)))*Ez000*prefac(:)
                  res_ij => res(:,3,1)
                  res_ij(:) = res_ij(:) + Ex000*Ey000*cmplx(Ez010,Ez110*k_z(:),kind=kind((1.0d0,1.0d0)))*prefac(:)
                end do
              end do
              call destroy_(e)
              call destroy_(prefac)
            case (1)
              call create_(ft_x,(/1,k_max/),(/0,1/),(/0,1/))
              call create_(ft_y,(/1,k_max/),(/0,1/),(/0,1/))
              call create_(ft_z,(/1,k_max/),(/0,1/),(/0,1/))
              call create_(prefac,k_max)
              k_x => k_pts(:,1)
              k_y => k_pts(:,2)
              k_z => k_pts(:,3)
              PA1 = self%a%pos(1)
              PA2 = self%a%pos(2)
              PA3 = self%a%pos(3)
              PB1 = self%b%pos(1)
              PB2 = self%b%pos(2)
              PB3 = self%b%pos(3)
              call create_(e,(/0,2/),(/0,1/),(/0,1/))
              call create_(ft_x01,k_max)
              call create_(ft_x10,k_max)
              call create_(ft_y01,k_max)
              call create_(ft_y10,k_max)
              call create_(ft_z01,k_max)
              call create_(ft_z10,k_max)
              call create_(k_x2,k_max)
              call create_(k_y2,k_max)
              call create_(k_z2,k_max)
              k_x2=k_x*k_x
              k_y2=k_y*k_y
              k_z2=k_z*k_z
              do aa = 1, self%a%n_cc
                ca = self%a%cc(aa)
                ex_a=self%a%ex(aa)
                call set_(G,ex_a=ex_a)
                do bb = 1, self%b%n_cc
                  ex_b=self%b%ex(bb)
                  call set_(G,ex_b=ex_b)
                  gamma = G%a%ex+G%b%ex
                  g1 = 1.0d0/gamma
                  g4 = 0.25d0*g1
                  PI_on_gamma = 3.141592653589793d0*g1
                  pifac = sqrt(PI_on_gamma) * PI_on_gamma
                  g1_exa = g1*G%a%ex
                  g1_exb = g1*G%b%ex
                  P1 = g1_exa*PA1 + g1_exb*PB1
                  P2 = g1_exa*PA2 + g1_exb*PB2
                  P3 = g1_exa*PA3 + g1_exb*PB3
                  fac = pifac * ca * self%b%cc(bb) * partition(aa,bb)
                  therm => thermal(:,aa,bb)
                  do k = 1,k_max
                    k1 = k_x(k);    k2 = k_y(k);    k3 = k_z(k)
                    dot1 = k1*P1+k2*P2+k3*P3
                    dot2 = -g4 * (k1*k1+k2*k2+k3*k3)
                    prefac(k) = fac*exp(cmplx(dot2,dot1,kind=kind((1.0d0,1.0d0)))) * therm(k)
                  end do

                  call make_e_coeff_(G,e,1)
                  Ex000 = e(0,0,0)
                  Ex001 = e(0,0,1)
                  Ex010 = e(0,1,0)
                  Ex011 = e(0,1,1)
                  Ex101 = e(1,0,1)
                  Ex110 = e(1,1,0)
                  Ex111 = e(1,1,1)
                  Ex211 = e(2,1,1)
                  call make_e_coeff_(G,e,2)
                  Ey000 = e(0,0,0)
                  Ey001 = e(0,0,1)
                  Ey010 = e(0,1,0)
                  Ey011 = e(0,1,1)
                  Ey101 = e(1,0,1)
                  Ey110 = e(1,1,0)
                  Ey111 = e(1,1,1)
                  Ey211 = e(2,1,1)
                  call make_e_coeff_(G,e,3)
                  Ez000 = e(0,0,0)
                  Ez001 = e(0,0,1)
                  Ez010 = e(0,1,0)
                  Ez011 = e(0,1,1)
                  Ez101 = e(1,0,1)
                  Ez110 = e(1,1,0)
                  Ez111 = e(1,1,1)
                  Ez211 = e(2,1,1)

                  ft_x01(:) = cmplx(Ex001,Ex101*k_x(:),kind=kind((1.0d0,1.0d0)))
                  ft_x10(:) = cmplx(Ex010,Ex110*k_x(:),kind=kind((1.0d0,1.0d0)))
                  ft_y01(:) = cmplx(Ey001,Ey101*k_y(:),kind=kind((1.0d0,1.0d0)))
                  ft_y10(:) = cmplx(Ey010,Ey110*k_y(:),kind=kind((1.0d0,1.0d0)))
                  ft_z01(:) = cmplx(Ez001,Ez101*k_z(:),kind=kind((1.0d0,1.0d0)))
                  ft_z10(:) = cmplx(Ez010,Ez110*k_z(:),kind=kind((1.0d0,1.0d0)))

                  res_ij => res(:,1,1)
                  res_ij(:) = res_ij(:) + cmplx(Ex011-Ex211*k_x2(:),Ex111*k_x(:),kind=kind((1.0d0,1.0d0)))*Ey000*Ez000*prefac&
&(:)
                  res_ij => res(:,2,1)
                  res_ij(:) = res_ij(:) + ft_x01(:)*ft_y10(:)*Ez000*prefac(:)
                  res_ij => res(:,3,1)
                  res_ij(:) = res_ij(:) + ft_x01(:)*Ey000*ft_z10(:)*prefac(:)
                  res_ij => res(:,1,2)
                  res_ij(:) = res_ij(:) + ft_x10(:)*ft_y01(:)*Ez000*prefac(:)
                  res_ij => res(:,2,2)
                  res_ij(:) = res_ij(:) + Ex000*cmplx(Ey011-Ey211*k_y2(:),Ey111*k_y(:),kind=kind((1.0d0,1.0d0)))*Ez000*prefac&
&(:)
                  res_ij => res(:,3,2)
                  res_ij(:) = res_ij(:) + Ex000*ft_y01(:)*ft_z10(:)*prefac(:)
                  res_ij => res(:,1,3)
                  res_ij(:) = res_ij(:) + ft_x10(:)*Ey000*ft_z01(:)*prefac(:)
                  res_ij => res(:,2,3)
                  res_ij(:) = res_ij(:) + Ex000*ft_y10(:)*ft_z01(:)*prefac(:)
                  res_ij => res(:,3,3)
                  res_ij(:) = res_ij(:) + Ex000*Ey000*cmplx(Ez011-Ez211*k_z2(:),Ez111*k_z(:),kind=kind((1.0d0,1.0d0)))*prefac&
&(:)
                end do
              end do
              call destroy_(k_z2)
              call destroy_(k_y2)
              call destroy_(k_x2)
              call destroy_(ft_z10)
              call destroy_(ft_z01)
              call destroy_(ft_y10)
              call destroy_(ft_y01)
              call destroy_(ft_x10)
              call destroy_(ft_x01)
              call destroy_(e)
              call destroy_(prefac)
              call destroy_(ft_x)
              call destroy_(ft_z)
              call destroy_(ft_y)
            case default
              call create_(ft_x,(/1,k_max/),(/0,l_a/),(/0,l_b/))
              call create_(ft_y,(/1,k_max/),(/0,l_a/),(/0,l_b/))
              call create_(ft_z,(/1,k_max/),(/0,l_a/),(/0,l_b/))
              call create_(prefac,k_max)
              k_x => k_pts(:,1)
              k_y => k_pts(:,2)
              k_z => k_pts(:,3)
              call create_(lb,3,n_b);   call make_gaussian_xyz_powers_(l_b,lb)
              do aa = 1, self%a%n_cc
                ca = self%a%cc(aa)
                ex_a=self%a%ex(aa)
                call set_(G,ex_a=ex_a)
                do bb = 1, self%b%n_cc
                  ex_b=self%b%ex(bb)
                  call set_(G,ex_b=ex_b)
                  gamma = G%a%ex+G%b%ex
                  g1 = 1.0d0/gamma
                  g4 = 0.25d0*g1
                  PI_on_gamma = 3.141592653589793d0*g1
                  pifac = sqrt(PI_on_gamma) * PI_on_gamma
                  g1_exa = g1*G%a%ex
                  g1_exb = g1*G%b%ex
                  P1 = g1_exa*self%a%pos(1) + g1_exb*self%b%pos(1)
                  P2 = g1_exa*self%a%pos(2) + g1_exb*self%b%pos(2)
                  P3 = g1_exa*self%a%pos(3) + g1_exb*self%b%pos(3)
                  call make_ft_component_(G,ft_x,1,k_x,k_max)
                  call make_ft_component_(G,ft_y,2,k_y,k_max)
                  call make_ft_component_(G,ft_z,3,k_z,k_max)
                  fac = pifac * ca * self%b%cc(bb) * partition(aa,bb)
                  therm => thermal(:,aa,bb)
                  do k = 1,k_max
                    k1 = k_x(k);    k2 = k_y(k);    k3 = k_z(k)
                    dot1 = k1*P1+k2*P2+k3*P3
                    dot2 = -g4 * (k1*k1+k2*k2+k3*k3)
                    prefac(k) = fac*exp(cmplx(dot2,dot1,kind=kind((1.0d0,1.0d0)))) * therm(k)
                  end do
                  do b=0,l_b
                    ft_z(:,0,b) = ft_z(:,0,b) * prefac(:)
                    ft_z(:,1,b) = ft_z(:,1,b) * prefac(:)
                  end do
                  do b = 1,n_b
                    bx = lb(1,b)
                    by = lb(2,b)
                    bz = lb(3,b)
                    ft_xb => ft_x(:,0,bx)
                    ft_yb => ft_y(:,0,by)
                    ft_zb => ft_z(:,0,bz)
                    res_ij => res(:,1,b)
                    res_ij(:) = res_ij(:) + ft_x(:,1,bx)*ft_yb*ft_zb
                    res_ij => res(:,2,b)
                    res_ij(:) = res_ij(:) + ft_xb*ft_y(:,1,by)*ft_zb
                    res_ij => res(:,3,b)
                    res_ij(:) = res_ij(:) + ft_xb*ft_yb*ft_z(:,1,bz)
                  end do
                end do
              end do
              call destroy_(lb)
              call destroy_(prefac)
              call destroy_(ft_x)
              call destroy_(ft_z)
              call destroy_(ft_y)
              call normalise_ft_(self,res)
          end select
        case default
          select case (l_b)
            case (0)
              call create_(ft_x,(/1,k_max/),(/0,l_a/),(/0,l_b/))
              call create_(ft_y,(/1,k_max/),(/0,l_a/),(/0,l_b/))
              call create_(ft_z,(/1,k_max/),(/0,l_a/),(/0,l_b/))
              call create_(prefac,k_max)
              k_x => k_pts(:,1)
              k_y => k_pts(:,2)
              k_z => k_pts(:,3)
              call create_(la,3,n_a);   call make_gaussian_xyz_powers_(l_a,la)
              do aa = 1, self%a%n_cc
                ca = self%a%cc(aa)
                ex_a=self%a%ex(aa)
                call set_(G,ex_a=ex_a)
                do bb = 1, self%b%n_cc
                  ex_b=self%b%ex(bb)
                  call set_(G,ex_b=ex_b)
                  gamma = G%a%ex+G%b%ex
                  g1 = 1.0d0/gamma
                  g4 = 0.25d0*g1
                  PI_on_gamma = 3.141592653589793d0*g1
                  pifac = sqrt(PI_on_gamma) * PI_on_gamma
                  g1_exa = g1*G%a%ex
                  g1_exb = g1*G%b%ex
                  P1 = g1_exa*self%a%pos(1) + g1_exb*self%b%pos(1)
                  P2 = g1_exa*self%a%pos(2) + g1_exb*self%b%pos(2)
                  P3 = g1_exa*self%a%pos(3) + g1_exb*self%b%pos(3)
                  call make_ft_component_(G,ft_x,1,k_x,k_max)
                  call make_ft_component_(G,ft_y,2,k_y,k_max)
                  call make_ft_component_(G,ft_z,3,k_z,k_max)
                  fac = pifac * ca * self%b%cc(bb) * partition(aa,bb)
                  therm => thermal(:,aa,bb)
                  do k = 1,k_max
                    k1 = k_x(k);    k2 = k_y(k);    k3 = k_z(k)
                    dot1 = k1*P1+k2*P2+k3*P3
                    dot2 = -g4 * (k1*k1+k2*k2+k3*k3)
                    prefac(k) = fac*exp(cmplx(dot2,dot1,kind=kind((1.0d0,1.0d0)))) * therm(k)
                  end do
                  do a=0,l_a
                    ft_z(:,a,0) = ft_z(:,a,0) * prefac(:)
                  end do
                  do a = 1,n_a
                    ax = la(1,a)
                    ay = la(2,a)
                    az = la(3,a)
                    res_ij => res(:,a,1)
                    res_ij(:) = res_ij(:) + ft_x(:,ax,0)*ft_y(:,ay,0)*ft_z(:,az,0)
                  end do
                end do
              end do
              call destroy_(la)
              call destroy_(prefac)
              call destroy_(ft_x)
              call destroy_(ft_z)
              call destroy_(ft_y)
              call normalise_ft_(self,res)
            case (1)
              call create_(ft_x,(/1,k_max/),(/0,l_a/),(/0,l_b/))
              call create_(ft_y,(/1,k_max/),(/0,l_a/),(/0,l_b/))
              call create_(ft_z,(/1,k_max/),(/0,l_a/),(/0,l_b/))
              call create_(prefac,k_max)
              k_x => k_pts(:,1)
              k_y => k_pts(:,2)
              k_z => k_pts(:,3)
              call create_(la,3,n_a);   call make_gaussian_xyz_powers_(l_a,la)
              do aa = 1, self%a%n_cc
                ca = self%a%cc(aa)
                ex_a=self%a%ex(aa)
                call set_(G,ex_a=ex_a)
                do bb = 1, self%b%n_cc
                  ex_b=self%b%ex(bb)
                  call set_(G,ex_b=ex_b)
                  gamma = G%a%ex+G%b%ex
                  g1 = 1.0d0/gamma
                  g4 = 0.25d0*g1
                  PI_on_gamma = 3.141592653589793d0*g1
                  pifac = sqrt(PI_on_gamma) * PI_on_gamma
                  g1_exa = g1*G%a%ex
                  g1_exb = g1*G%b%ex
                  P1 = g1_exa*self%a%pos(1) + g1_exb*self%b%pos(1)
                  P2 = g1_exa*self%a%pos(2) + g1_exb*self%b%pos(2)
                  P3 = g1_exa*self%a%pos(3) + g1_exb*self%b%pos(3)
                  call make_ft_component_(G,ft_x,1,k_x,k_max)
                  call make_ft_component_(G,ft_y,2,k_y,k_max)
                  call make_ft_component_(G,ft_z,3,k_z,k_max)
                  fac = pifac * ca * self%b%cc(bb) * partition(aa,bb)
                  therm => thermal(:,aa,bb)
                  do k = 1,k_max
                    k1 = k_x(k);    k2 = k_y(k);    k3 = k_z(k)
                    dot1 = k1*P1+k2*P2+k3*P3
                    dot2 = -g4 * (k1*k1+k2*k2+k3*k3)
                    prefac(k) = fac*exp(cmplx(dot2,dot1,kind=kind((1.0d0,1.0d0)))) * therm(k)
                  end do
                  do a=0,l_a  ! incorporate prefac into ft_z
                    ft_z(:,a,0) = ft_z(:,a,0) * prefac(:)
                    ft_z(:,a,1) = ft_z(:,a,1) * prefac(:)
                  end do
                  do a = 1,n_a
                    ax = la(1,a)
                    ay = la(2,a)
                    az = la(3,a)
                    res_ij => res(:,a,1)
                    res_ij(:) = res_ij(:) + ft_x(:,ax,1)*ft_y(:,ay,0)*ft_z(:,az,0)
                    res_ij => res(:,a,2)
                    res_ij(:) = res_ij(:) + ft_x(:,ax,0)*ft_y(:,ay,1)*ft_z(:,az,0)
                    res_ij => res(:,a,3)
                    res_ij(:) = res_ij(:) + ft_x(:,ax,0)*ft_y(:,ay,0)*ft_z(:,az,1)
                  end do
                end do
              end do
              call destroy_(la)
              call destroy_(prefac)
              call destroy_(ft_x)
              call destroy_(ft_z)
              call destroy_(ft_y)
              call normalise_ft_(self,res)
            case default
              call create_(ft_x,(/1,k_max/),(/0,l_a/),(/0,l_b/))
              call create_(ft_y,(/1,k_max/),(/0,l_a/),(/0,l_b/))
              call create_(ft_z,(/1,k_max/),(/0,l_a/),(/0,l_b/))
              call create_(prefac,k_max)
              k_x => k_pts(:,1)
              k_y => k_pts(:,2)
              k_z => k_pts(:,3)
              call create_(la,3,n_a);   call make_gaussian_xyz_powers_(l_a,la)
              call create_(lb,3,n_b);   call make_gaussian_xyz_powers_(l_b,lb)
              ! This is the general routine.  It includes reduced multiplication,
              ! and use of pointers to minimise array finding.
              do aa = 1, self%a%n_cc
                ca = self%a%cc(aa)
                ex_a=self%a%ex(aa)
                call set_(G,ex_a=ex_a)
                do bb = 1, self%b%n_cc
                  ex_b=self%b%ex(bb)
                  call set_(G,ex_b=ex_b)
                  gamma = G%a%ex+G%b%ex
                  g1 = 1.0d0/gamma
                  g4 = 0.25d0*g1
                  PI_on_gamma = 3.141592653589793d0*g1
                  pifac = sqrt(PI_on_gamma) * PI_on_gamma
                  g1_exa = g1*G%a%ex
                  g1_exb = g1*G%b%ex
                  P1 = g1_exa*self%a%pos(1) + g1_exb*self%b%pos(1)
                  P2 = g1_exa*self%a%pos(2) + g1_exb*self%b%pos(2)
                  P3 = g1_exa*self%a%pos(3) + g1_exb*self%b%pos(3)
                  call make_ft_component_(G,ft_x,1,k_x,k_max)
                  call make_ft_component_(G,ft_y,2,k_y,k_max)
                  call make_ft_component_(G,ft_z,3,k_z,k_max)
                  fac = pifac * ca * self%b%cc(bb) * partition(aa,bb)
                  therm => thermal(:,aa,bb)
                  do k = 1,k_max
                    k1 = k_x(k);    k2 = k_y(k);    k3 = k_z(k)
                    dot1 = k1*P1+k2*P2+k3*P3
                    dot2 = -g4 * (k1*k1+k2*k2+k3*k3)
                    prefac(k) = fac*exp(cmplx(dot2,dot1,kind=kind((1.0d0,1.0d0)))) * therm(k)
                  end do
                  do b=0,l_b  ! incorporate prefac into ft_z
                    do a=0,l_a
                      ft_z(:,a,b) = ft_z(:,a,b) * prefac(:)
                    end do
                  end do
                  bxlast = lb(1,1)
                  bylast = lb(2,1)
                  bzlast = lb(3,1)
                  ft_xbx => ft_x(:,:,bxlast)
                  ft_yby => ft_y(:,:,bylast)
                  ft_zbz => ft_z(:,:,bzlast)
                  do b = 1,n_b
                    bx = lb(1,b)
                    by = lb(2,b)
                    bz = lb(3,b)
                    if (bxlast/=bx) then
                      ft_xbx => ft_x(:,:,bx)
                      bxlast = bx
                    end if
                    if (bylast/=by) then
                      ft_yby=> ft_y(:,:,by)
                      bylast = by
                    end if
                    if (bzlast/=bz) then
                      ft_zbz=> ft_z(:,:,bz)
                      bzlast = bz
                    end if
                    do a = 1,n_a
                      ax = la(1,a)
                      ay = la(2,a)
                      az = la(3,a)
                      res_ij => res(:,a,b)
                      res_ij(:) = res_ij(:) + ft_xbx(:,ax+1)*ft_yby(:,ay+1)*ft_zbz(:,az+1)
                    end do
                  end do
                end do
              end do
              call destroy_(lb)
              call destroy_(la)
              call destroy_(prefac)
              call destroy_(ft_x)
              call destroy_(ft_z)
              call destroy_(ft_y)
              call normalise_ft_(self,res)
          end select
      end select

   end subroutine

   subroutine make_gaussian_partition(self,res,factor_a,factor_b)
    type(shell2_type) :: self
    ! Make the factors for partitioning the density contribution from
    ! each atom.
    ! Dimensions of res are [.a.n_cc,.b.n_cc].
     real(kind=kind(1.0d0)), dimension(:,:), intent(out) :: res
     real(kind=kind(1.0d0)), intent(in) :: factor_a,factor_b
     integer(kind=kind(1)) :: a,b
     real(kind=kind(1.0d0)) :: ex_a,ex_b,exb_fb

     do b = 1, self%b%n_cc
       ex_b = self%b%ex(b)
       exb_fb = ex_b * factor_b
       do a = 1, self%a%n_cc
         ex_a = self%a%ex(a)
         res(a,b) = (exb_fb + ex_a * factor_a) / (ex_a + ex_b)
       end do
     end do

   end subroutine

   subroutine make_tanaka_thermal_smearing(self,res,k_pts,Ua,Ub)
    type(shell2_type) :: self
    ! Make the thermal smearing correction according to Tanaka.
    ! Dimensions of "res" are [k_max,.a.n_cc,.b.n_cc].
    ! "res" contains the correction for each pair of primitives.
     real(kind=kind(1.0d0)), dimension(:,:,:), intent(out) :: res
     real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: k_pts,Ua,Ub
     real(kind=kind(1.0d0)), dimension(3,3) :: Ua_ea,Ub_eb,U
      ! S,Utemp :: real(kind=kind(1.0d0)), dimension(3)
     real(kind=kind(1.0d0)) :: ea,eb
     real(kind=kind(1.0d0)) :: U11,U22,U33,U21_12,U31_13,U32_23,S1,S2,S3
     integer(kind=kind(1)) :: a,b,k,k_max

     k_max  = size(k_pts,1)
     do a = 1, self%a%n_cc
       ea = self%a%ex(a)
       Ua_ea = ea * Ua
       do b = 1, self%b%n_cc
         eb = self%b%ex(b)
         Ub_eb = eb * Ub
         U = - 0.50d0 * (Ua_ea + Ub_eb) / (ea + eb)
         U11 = U(1,1)
         U22 = U(2,2)
         U33 = U(3,3)
         U21_12 = U(2,1) + U(1,2)
         U31_13 = U(3,1) + U(1,3)
         U32_23 = U(3,2) + U(2,3)
         do k = 1,k_max
           S1 = k_pts(k,1)
           S2 = k_pts(k,2)
           S3 = k_pts(k,3)
           res(k,a,b) = exp(S1*(S1*U11+S2*U21_12+S3*U31_13)+S2*(S2*U22+S3*U32_23)+S3*S3*U33)
            ! SUS has been expanded to save multiplies and array accesses.
  !          S = k_pts(k,:)
  !          Utemp = matmul(U,S)         ! Change coordinates of the U matrix.
  !          res(k,a,b) = exp(dot_product(S,Utemp))
         end do
       end do
     end do

   end subroutine

   subroutine make_ft_nabla(self,res,k_pts)
    type(shell2_type) :: self
    ! Calculates the Fourier transform for a product of the gradient of
    ! shell a minus shell b for two contracted gaussian shells, evaluated
    ! at a series of k points k_pts
    ! Dimensions of res are [k_max,.n_comp_a,.n_comp_b,3].
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:,:) :: res
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: k_pts
       type(gaussian2_type) :: G
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:,:), pointer :: ft_ab
      integer(kind=kind(1)) :: a,b,k_max
      real(kind=kind(1.0d0)) :: ca,cb,cacb

      k_max  = size(k_pts,1)
      res = 0.0d0
      call set_(G,self%a%l,self%a%pos,0.0d0,self%b%l,self%b%pos,0.0d0)
      do a = 1, self%a%n_cc
         call set_(G,ex_a=self%a%ex(a))
         ca = self%a%cc(a)
         do b = 1, self%b%n_cc
            call set_(G,ex_b=self%b%ex(b))
            cb = self%b%cc(b)
            cacb = ca*cb
            call create_(ft_ab,k_max,self%a%n_comp,self%b%n_comp,3)
            call make_ft_nabla_(G,ft_ab,k_pts)
            res = res + ft_ab*cacb
            call destroy_(ft_ab)
         end do
      end do
      call normalise_ft_(self,res(:,:,:,1))
      call normalise_ft_(self,res(:,:,:,2))
      call normalise_ft_(self,res(:,:,:,3))

   end subroutine

   subroutine make_ft_nabla_1(self,res,k_pts,thermal,partition)
    type(shell2_type) :: self
    ! Calculates "res" the Fourier transform for a product of the gradient of
    ! shell a minus shell b for two contracted gaussian shells, evaluated
    ! at a series of k points "k_pts", including "thermal" smearing corrections
    ! and "partition" factors.
    ! Dimensions of res are [k_max,.n_comp_a,.n_comp_b,3].
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:,:) :: res
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: k_pts,partition
      real(kind=kind(1.0d0)), dimension(:,:,:), target :: thermal
      real(kind=kind(1.0d0)), dimension(:), pointer :: therm
       type(gaussian2_type) :: G
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:,:), pointer :: ft_ab
      integer(kind=kind(1)) :: a,b,k_max,i,j
      real(kind=kind(1.0d0)) :: ca,cb,fac

      k_max  = size(k_pts,1)
      res = 0.0d0
      call set_(G,self%a%l,self%a%pos,0.0d0,self%b%l,self%b%pos,0.0d0)
      do a = 1, self%a%n_cc
         call set_(G,ex_a=self%a%ex(a))
         ca = self%a%cc(a)
         do b = 1, self%b%n_cc
            call set_(G,ex_b=self%b%ex(b))
            cb = self%b%cc(b)
            call create_(ft_ab,k_max,self%a%n_comp,self%b%n_comp,3)
            call make_ft_nabla_(G,ft_ab,k_pts)
            fac   =  ca*cb*partition(a,b)
            therm => thermal(:,a,b)
            do i = 1,self%a%n_comp
            do j = 1,self%b%n_comp
                res(:,i,j,1) = res(:,i,j,1) + ft_ab(:,i,j,1) * fac * therm(:)
                res(:,i,j,2) = res(:,i,j,2) + ft_ab(:,i,j,2) * fac * therm(:)
                res(:,i,j,3) = res(:,i,j,3) + ft_ab(:,i,j,3) * fac * therm(:)
            end do
            end do
            call destroy_(ft_ab)
         end do
      end do
      call normalise_ft_(self,res(:,:,:,1))
      call normalise_ft_(self,res(:,:,:,2))
      call normalise_ft_(self,res(:,:,:,3))

   end subroutine

   subroutine make_ft_r(self,res,k_pts)
    type(shell2_type) :: self
    ! Calculates the Fourier transform for a product of two contracted
    ! gaussian shells, evaluated at a series of k points k_pts
    ! Dimensions of res are [k_max,.n_comp_a,.n_comp_b,3].
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: k_pts
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:,:) :: res
       type(gaussian2_type) :: G
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:,:), pointer :: ft_ab
      integer(kind=kind(1)) :: a,b,k_max
      real(kind=kind(1.0d0)) :: ca,cb,cacb

      k_max  = size(k_pts,1)
      res = 0.0d0
      call set_(G,self%a%l,self%a%pos,0.0d0,self%b%l,self%b%pos,0.0d0)
      do a = 1, self%a%n_cc
         call set_(G,ex_a=self%a%ex(a))
         ca = self%a%cc(a)
         do b = 1, self%b%n_cc
            call set_(G,ex_b=self%b%ex(b))
            cb = self%b%cc(b)
            cacb = ca*cb
            call create_(ft_ab,k_max,self%a%n_comp,self%b%n_comp,3)
            call make_ft_r_(G,ft_ab,k_pts)
            res = res + ft_ab*cacb
            call destroy_(ft_ab)
         end do
      end do
      call normalise_ft_(self,res(:,:,:,1))
      call normalise_ft_(self,res(:,:,:,2))
      call normalise_ft_(self,res(:,:,:,3))

   end subroutine

!  *********************************
!  Simplistic CADPAC-style integrals
!  *********************************

   subroutine make_overlap_ints(self,S)
    type(shell2_type) :: self
    ! Calculates overlap integral matrix, using Gauss-Hermite quadrature, like in
    ! CADPAC
       real(kind=kind(1.0d0)), dimension(:,:) :: S
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: SS
       type(gaussian2_type) :: G
      integer(kind=kind(1)) :: a,b
      real(kind=kind(1.0d0)) :: cacb

      S = 0.0d0
      call create_(SS,self%a%n_comp,self%b%n_comp)
      call set_(G,self%a%l,self%a%pos,0.0d0,self%b%l,self%b%pos,0.0d0)
      do a = 1,self%a%n_cc
      do b = 1,self%b%n_cc
         call set_(G,self%a%ex(a),self%b%ex(b))
         call make_overlap_ints_(G,SS)
         cacb = self%a%cc(a)*self%b%cc(b)
         S = S + SS*cacb
      end do
      end do
      call destroy_(SS)
      call normalise_(self,S)

   end subroutine

   subroutine make_S_1st_deriv_ints(self,Ax,Ay,Az)
    type(shell2_type) :: self
    ! Calculates the derivatives of the overlap integrals with respect to
    ! position A, in "Ax", "Ay", and "Az" using Gauss-Hermite quadrature.
      real(kind=kind(1.0d0)), dimension(:,:) :: Ax,Ay,Az
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: AAx,AAy,AAz
       type(gaussian2_type) :: G
      integer(kind=kind(1)) :: a,b
      real(kind=kind(1.0d0)) :: cacb

      Ax = 0.0d0; Ay = 0.0d0; Az = 0.0d0
      call create_(AAx,self%a%n_comp,self%b%n_comp)
      call create_(AAy,self%a%n_comp,self%b%n_comp)
      call create_(AAz,self%a%n_comp,self%b%n_comp)
      call set_(G,self%a%l,self%a%pos,0.0d0,self%b%l,self%b%pos,0.0d0)
      do a = 1,self%a%n_cc
      do b = 1,self%b%n_cc
         call set_(G,self%a%ex(a),self%b%ex(b))
         call make_S_1st_deriv_ints_(G,AAx,AAy,AAz)
         cacb = self%a%cc(a)*self%b%cc(b)
         Ax = Ax + AAx*cacb
         Ay = Ay + AAy*cacb
         Az = Az + AAz*cacb
      end do
      end do
      call destroy_(AAz)
      call destroy_(AAy)
      call destroy_(AAx)
      call normalise_(self,Ax); call normalise_(self,Ay); call normalise_(self,Az)

   end subroutine

   subroutine make_T_1st_deriv_ints(self,Ax,Ay,Az)
    type(shell2_type) :: self
    ! Calculates the derivatives of the kinetic integrals with respect to
    ! position A, in "Ax", "Ay", and "Az" using Gauss-Hermite quadrature.
      real(kind=kind(1.0d0)), dimension(:,:) :: Ax,Ay,Az
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: AAx,AAy,AAz
       type(gaussian2_type) :: G
      integer(kind=kind(1)) :: a,b
      real(kind=kind(1.0d0)) :: cacb

      Ax = 0.0d0; Ay = 0.0d0; Az = 0.0d0
      call create_(AAx,self%a%n_comp,self%b%n_comp)
      call create_(AAy,self%a%n_comp,self%b%n_comp)
      call create_(AAz,self%a%n_comp,self%b%n_comp)
      call set_(G,self%a%l,self%a%pos,0.0d0,self%b%l,self%b%pos,0.0d0)
      do a = 1,self%a%n_cc
      do b = 1,self%b%n_cc
         call set_(G,self%a%ex(a),self%b%ex(b))
         call make_T_1st_deriv_ints_(G,AAx,AAy,AAz)
         cacb = self%a%cc(a)*self%b%cc(b)
         Ax = Ax + AAx*cacb
         Ay = Ay + AAy*cacb
         Az = Az + AAz*cacb
      end do
      end do
      call destroy_(AAz)
      call destroy_(AAy)
      call destroy_(AAx)
      call normalise_(self,Ax); call normalise_(self,Ay); call normalise_(self,Az)

   end subroutine

   subroutine make_NA_1st_deriv_ints(self,Ax,Ay,Az,Bx,By,Bz,c)
    type(shell2_type) :: self
    ! Calculates the derivatives of the nuclear attraction integrals with respect to
    ! positions A *and* B, in "Ax", "Ay", "Az", and "Bx", "By", "Bz" for a given
    ! nuclear position "c".
      real(kind=kind(1.0d0)), dimension(:,:) :: Ax,Ay,Az, Bx,By,Bz
       real(kind=kind(1.0d0)), dimension(3) :: c
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: AAx,AAy,AAz, BBx,BBy,BBz
       type(gaussian2_type) :: G
      integer(kind=kind(1)) :: a,b
      real(kind=kind(1.0d0)) :: cacb

      Ax = 0.0d0; Ay = 0.0d0; Az = 0.0d0
      Bx = 0.0d0; By = 0.0d0; Bz = 0.0d0
      call create_(AAx,self%a%n_comp,self%b%n_comp)
      call create_(AAy,self%a%n_comp,self%b%n_comp)
      call create_(AAz,self%a%n_comp,self%b%n_comp)
      call create_(BBx,self%a%n_comp,self%b%n_comp)
      call create_(BBy,self%a%n_comp,self%b%n_comp)
      call create_(BBz,self%a%n_comp,self%b%n_comp)
      call set_(G,self%a%l,self%a%pos,0.0d0,self%b%l,self%b%pos,0.0d0)
      do a = 1,self%a%n_cc
      do b = 1,self%b%n_cc
         call set_(G,self%a%ex(a),self%b%ex(b))
         call make_NA_1st_deriv_ints_(G,AAx,AAy,AAz,BBx,BBy,BBz,c)
         cacb = self%a%cc(a)*self%b%cc(b)
         Ax = Ax + AAx*cacb
         Ay = Ay + AAy*cacb
         Az = Az + AAz*cacb
         Bx = Bx + BBx*cacb
         By = By + BBy*cacb
         Bz = Bz + BBz*cacb
      end do
      end do
      call destroy_(BBz); call destroy_(BBy); call destroy_(BBx)
      call destroy_(AAz); call destroy_(AAy); call destroy_(AAx)
      call normalise_(self,Ax); call normalise_(self,Ay); call normalise_(self,Az)
      call normalise_(self,Bx); call normalise_(self,By); call normalise_(self,Bz)

   end subroutine

   subroutine make_dipole_ints(self,Dx,Dy,Dz,origin)
    type(shell2_type) :: self
    ! Make the dipole moment integral matrices "Di"
    ! with gauge origin "origin"
      real(kind=kind(1.0d0)), dimension(:,:) :: Dx,Dy,Dz
      real(kind=kind(1.0d0)), dimension(3) :: origin
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: DDx,DDy,DDz
       type(gaussian2_type) :: G
      integer(kind=kind(1)) :: a,b,n_a,n_b
      real(kind=kind(1.0d0)) :: cacb

      Dx = 0.0d0; Dy = 0.0d0; Dz = 0.0d0
      n_a = self%a%n_comp; n_b = self%b%n_comp
      call create_(DDx,n_a,n_b); call create_(DDy,n_a,n_b); call create_(DDz,n_a,n_b)
      call set_(G,self%a%l,self%a%pos,0.0d0,self%b%l,self%b%pos,0.0d0)
      do a = 1,self%a%n_cc
      do b = 1,self%b%n_cc
         call set_(G,self%a%ex(a),self%b%ex(b))
         call make_dipole_ints_(G,DDx,DDy,DDz,origin)
         cacb = self%a%cc(a)*self%b%cc(b)
         Dx = Dx + DDx*cacb; Dy = Dy + DDy*cacb; Dz = Dz + DDz*cacb
      end do
      end do
      call destroy_(DDz); call destroy_(DDy); call destroy_(DDx)
      call normalise_(self,Dx); call normalise_(self,Dy); call normalise_(self,Dz)

   end subroutine

   subroutine make_quadrupole_ints(self,Qxx,Qyy,Qzz,Qxy,Qxz,Qyz,origin)
    type(shell2_type) :: self
    ! Make the quadrupole moment integral matrices "Qij"
    ! with gauge origin "origin"
      real(kind=kind(1.0d0)), dimension(:,:) :: Qxx,Qyy,Qzz,Qxy,Qxz,Qyz
      real(kind=kind(1.0d0)), dimension(3) :: origin
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: QQxx,QQyy,QQzz,QQxy,QQxz,QQyz
       type(gaussian2_type) :: G
      integer(kind=kind(1)) :: a,b,n_a,n_b
      real(kind=kind(1.0d0)) :: cacb

      Qxx = 0.0d0; Qyy = 0.0d0; Qzz = 0.0d0
      Qxy = 0.0d0; Qxz = 0.0d0; Qyz = 0.0d0
      n_a = self%a%n_comp; n_b = self%b%n_comp
      call create_(QQxx,n_a,n_b); call create_(QQyy,n_a,n_b); call create_(QQzz,n_a,n_b)
      call create_(QQxy,n_a,n_b); call create_(QQxz,n_a,n_b); call create_(QQyz,n_a,n_b)
      call set_(G,self%a%l,self%a%pos,0.0d0,self%b%l,self%b%pos,0.0d0)
      do a = 1,self%a%n_cc
      do b = 1,self%b%n_cc
         call set_(G,self%a%ex(a),self%b%ex(b))
         call make_quadrupole_ints_(G,QQxx,QQyy,QQzz,QQxy,QQxz,QQyz,origin)
         cacb = self%a%cc(a)*self%b%cc(b)
         Qxx = Qxx + QQxx*cacb; Qyy = Qyy + QQyy*cacb; Qzz = Qzz + QQzz*cacb
         Qxy = Qxy + QQxy*cacb; Qxz = Qxz + QQxz*cacb; Qyz = Qyz + QQyz*cacb
      end do
      end do
      call destroy_(QQyz); call destroy_(QQxz); call destroy_(QQxy)
      call destroy_(QQzz); call destroy_(QQyy); call destroy_(QQxx)
      call normalise_(self,Qxx); call normalise_(self,Qyy); call normalise_(self,Qzz)
      call normalise_(self,Qxy); call normalise_(self,Qxz); call normalise_(self,Qyz)

   end subroutine

   subroutine make_octupole_ints(self,Oxxx,Oyyy,Ozzz,Oxxy,Oxxz,Oyyx,Oyyz,Ozzx,Ozzy,Oxyz,origin)
    type(shell2_type) :: self
    ! Make the octupole moment integral matrices "Oijk"
    ! with gauge origin "origin"
      real(kind=kind(1.0d0)), dimension(:,:) :: Oxxx,Oyyy,Ozzz,Oxxy,Oxxz,Oyyx,Oyyz,Ozzx,Ozzy,Oxyz
      real(kind=kind(1.0d0)), dimension(3) :: origin
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: OOxxx,OOyyy,OOzzz,OOxxy,OOxxz,OOyyx,OOyyz,OOzzx,OOzzy,OOxyz
      type(gaussian2_type) :: G
      integer(kind=kind(1)) :: a,b,n_a,n_b
      real(kind=kind(1.0d0)) :: cacb

      Oxxx = 0.0d0; Oyyy = 0.0d0; Ozzz = 0.0d0
      Oxxy = 0.0d0; Oxxz = 0.0d0
      Oyyx = 0.0d0; Oyyz = 0.0d0
      Ozzx = 0.0d0; Ozzy = 0.0d0
      Oxyz = 0.0d0
      n_a = self%a%n_comp; n_b = self%b%n_comp
      call create_(OOxxx,n_a,n_b); call create_(OOyyy,n_a,n_b); call create_(OOzzz,n_a,n_b)
      call create_(OOxxy,n_a,n_b); call create_(OOxxz,n_a,n_b)
      call create_(OOyyx,n_a,n_b); call create_(OOyyz,n_a,n_b)
      call create_(OOzzx,n_a,n_b); call create_(OOzzy,n_a,n_b)
      call create_(OOxyz,n_a,n_b)
      call set_(G,self%a%l,self%a%pos,0.0d0,self%b%l,self%b%pos,0.0d0)
      do a = 1,self%a%n_cc
      do b = 1,self%b%n_cc
         call set_(G,self%a%ex(a),self%b%ex(b))
         call make_octupole_ints_(G,OOxxx,OOyyy,OOzzz,OOxxy,OOxxz,OOyyx,OOyyz,OOzzx,OOzzy,OOxyz,origin)
         cacb = self%a%cc(a)*self%b%cc(b)
         Oxxx = Oxxx + OOxxx*cacb; Oyyy = Oyyy + OOyyy*cacb; Ozzz = Ozzz + OOzzz*cacb
         Oxxy = Oxxy + OOxxy*cacb; Oxxz = Oxxz + OOxxz*cacb
         Oyyx = Oyyx + OOyyx*cacb; Oyyz = Oyyz + OOyyz*cacb
         Ozzx = Ozzx + OOzzx*cacb; Ozzy = Ozzy + OOzzy*cacb
         Oxyz = Oxyz + OOxyz*cacb
      end do
      end do
      call destroy_(OOxyz)
      call destroy_(OOzzy); call destroy_(OOzzx)
      call destroy_(OOyyz); call destroy_(OOyyx)
      call destroy_(OOxxz); call destroy_(OOxxy)
      call destroy_(OOzzz); call destroy_(OOyyy); call destroy_(OOxxx)
      call normalise_(self,Oxxx); call normalise_(self,Oyyy); call normalise_(self,Ozzz)
      call normalise_(self,Oxxy); call normalise_(self,Oxxz)
      call normalise_(self,Oyyx); call normalise_(self,Oyyz)
      call normalise_(self,Ozzx); call normalise_(self,Ozzy)
      call normalise_(self,Oxyz)

   end subroutine

   subroutine make_nuclear_attraction_ints(self,N,c)
    type(shell2_type) :: self
    ! Make the nuclear attraction integral matrix "N" for nucleus at position "c"
       real(kind=kind(1.0d0)), dimension(:,:) :: N
       real(kind=kind(1.0d0)), dimension(3) :: c
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: NN
       type(gaussian2_type) :: G
      integer(kind=kind(1)) :: a,b
      real(kind=kind(1.0d0)) :: cacb

      N = 0.0d0
      call create_(NN,self%a%n_comp,self%b%n_comp)
      call set_(G,self%a%l,self%a%pos,0.0d0,self%b%l,self%b%pos,0.0d0)
      do a = 1,self%a%n_cc
      do b = 1,self%b%n_cc
         call set_(G,self%a%ex(a),self%b%ex(b))
         call make_nuclear_attraction_ints_(G,NN,c)
         cacb = self%a%cc(a)*self%b%cc(b)
         N = N + NN*cacb
      end do
      end do
      call destroy_(NN)
      call normalise_(self,N)

   end subroutine

   subroutine make_E_field_ints(self,Ex,Ey,Ez,p)
    type(shell2_type) :: self
    ! Make the electric field integral matrices "Ei" evaluated at
    ! the position "p"
      real(kind=kind(1.0d0)), dimension(:,:) :: Ex,Ey,Ez
       real(kind=kind(1.0d0)), dimension(3) :: p
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: EEx,EEy,EEz
       type(gaussian2_type) :: G
      integer(kind=kind(1)) :: a,b,n_a,n_b
      real(kind=kind(1.0d0)) :: cacb

      Ex = 0.0d0; Ey = 0.0d0; Ez = 0.0d0
      n_a = self%a%n_comp; n_b = self%b%n_comp
      call create_(EEx,n_a,n_b); call create_(EEy,n_a,n_b); call create_(EEz,n_a,n_b)
      call set_(G,self%a%l,self%a%pos,0.0d0,self%b%l,self%b%pos,0.0d0)
      do a = 1,self%a%n_cc
      do b = 1,self%b%n_cc
         call set_(G,self%a%ex(a),self%b%ex(b))
         call make_E_field_ints_(G,EEx,EEy,EEz,p)
         cacb = self%a%cc(a)*self%b%cc(b)
         Ex = Ex + EEx*cacb; Ey = Ey + EEy*cacb; Ez = Ez + EEz*cacb
      end do
      end do
      call destroy_(EEz); call destroy_(EEy); call destroy_(EEx)
      call normalise_(self,Ex); call normalise_(self,Ey); call normalise_(self,Ez)

   end subroutine

   subroutine make_E_gradient_ints(self,Exx,Eyy,Ezz,Exy,Exz,Eyz,c)
    type(shell2_type) :: self
    ! Make the electric field gradient integral matrices "Eij" evaluated
    ! at the position "c".
      real(kind=kind(1.0d0)), dimension(:,:) :: Exx,Eyy,Ezz,Exy,Exz,Eyz
       real(kind=kind(1.0d0)), dimension(:) :: c
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: EExx,EEyy,EEzz,EExy,EExz,EEyz
       type(gaussian2_type) :: G
      integer(kind=kind(1)) :: a,b,n_a,n_b
      real(kind=kind(1.0d0)) :: cacb

      Exx = 0.0d0; Eyy = 0.0d0; Ezz = 0.0d0
      Exy = 0.0d0; Exz = 0.0d0; Eyz = 0.0d0
      n_a = self%a%n_comp; n_b = self%b%n_comp
      call create_(EExx,n_a,n_b); call create_(EEyy,n_a,n_b); call create_(EEzz,n_a,n_b)
      call create_(EExy,n_a,n_b); call create_(EExz,n_a,n_b); call create_(EEyz,n_a,n_b)
      call set_(G,self%a%l,self%a%pos,0.0d0,self%b%l,self%b%pos,0.0d0)
      do a = 1,self%a%n_cc
      do b = 1,self%b%n_cc
         call set_(G,self%a%ex(a),self%b%ex(b))
         call make_E_gradient_ints_(G,EExx,EEyy,EEzz,EExy,EExz,EEyz,c)
         cacb = self%a%cc(a)*self%b%cc(b)
         Exx = Exx + EExx*cacb; Eyy = Eyy + EEyy*cacb; Ezz = Ezz + EEzz*cacb
         Exy = Exy + EExy*cacb; Exz = Exz + EExz*cacb; Eyz = Eyz + EEyz*cacb
      end do
      end do
      call destroy_(EEyz); call destroy_(EExz); call destroy_(EExy)
      call destroy_(EEzz); call destroy_(EEyy); call destroy_(EExx)
      call normalise_(self,Exx); call normalise_(self,Eyy); call normalise_(self,Ezz)
      call normalise_(self,Exy); call normalise_(self,Exz); call normalise_(self,Eyz)

   end subroutine

   subroutine make_spin_orbit_ints(self,Lx,Ly,Lz,c)
    type(shell2_type) :: self
    ! Make the spin orbit integral matrices "Lx" "Ly" and "Lz" for nucleus at
    ! position "c"
      real(kind=kind(1.0d0)), dimension(:,:) :: Lx,Ly,Lz
       real(kind=kind(1.0d0)), dimension(3) :: c
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: LLx,LLy,LLz
       type(gaussian2_type) :: G
      integer(kind=kind(1)) :: a,b
      real(kind=kind(1.0d0)) :: cacb

      Lx = 0.0d0; Ly = 0.0d0; Lz = 0.0d0
      call create_(LLx,self%a%n_comp,self%b%n_comp)
      call create_(LLy,self%a%n_comp,self%b%n_comp)
      call create_(LLz,self%a%n_comp,self%b%n_comp)
      call set_(G,self%a%l,self%a%pos,0.0d0,self%b%l,self%b%pos,0.0d0)
      do a = 1,self%a%n_cc
      do b = 1,self%b%n_cc
         call set_(G,self%a%ex(a),self%b%ex(b))
         call make_spin_orbit_ints_(G,LLx,LLy,LLz,c)
         cacb = self%a%cc(a)*self%b%cc(b)
         Lx = Lx + LLx*cacb
         Ly = Ly + LLy*cacb
         Lz = Lz + LLz*cacb
      end do
      end do
      call destroy_(LLz); call destroy_(LLy); call destroy_(LLx)
      call normalise_(self,Lx); call normalise_(self,Ly); call normalise_(self,Lz)

   end subroutine

   subroutine make_spin_orbit_B_ints(self,Qxx,Qxy,Qxz,Qyx,Qyy,Qyz,Qzx,Qzy,Qzz,c,origin)
    type(shell2_type) :: self
    ! Make the gauge modified (B field) spin orbit integral matrices "Qij"
    ! for nucleus at position "c" and gauge origin "origin"
      real(kind=kind(1.0d0)), dimension(:,:) :: Qxx,Qxy,Qxz,Qyx,Qyy,Qyz,Qzx,Qzy,Qzz
      real(kind=kind(1.0d0)), dimension(3) :: c,origin
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: QQxx,QQxy,QQxz,QQyx,QQyy,QQyz,QQzx,QQzy,QQzz
       type(gaussian2_type) :: G
      integer(kind=kind(1)) :: a,b,n_a,n_b
      real(kind=kind(1.0d0)) :: cacb

      Qxx = 0.0d0; Qyx = 0.0d0; Qzx = 0.0d0
      Qxy = 0.0d0; Qyy = 0.0d0; Qzy = 0.0d0
      Qxz = 0.0d0; Qyz = 0.0d0; Qzz = 0.0d0
      n_a = self%a%n_comp; n_b = self%b%n_comp
      call create_(QQxx,n_a,n_b); call create_(QQyx,n_a,n_b); call create_(QQzx,n_a,n_b)
      call create_(QQxy,n_a,n_b); call create_(QQyy,n_a,n_b); call create_(QQzy,n_a,n_b)
      call create_(QQxz,n_a,n_b); call create_(QQyz,n_a,n_b); call create_(QQzz,n_a,n_b)
      call set_(G,self%a%l,self%a%pos,0.0d0,self%b%l,self%b%pos,0.0d0)
      do a = 1,self%a%n_cc
      do b = 1,self%b%n_cc
         call set_(G,self%a%ex(a),self%b%ex(b))
         call make_spin_orbit_B_ints_(G,QQxx,QQxy,QQxz,QQyx,QQyy,QQyz,QQzx,QQzy,QQzz,c,origin)
         cacb = self%a%cc(a)*self%b%cc(b)
         Qxx = Qxx + QQxx*cacb; Qxy = Qxy + QQxy*cacb; Qxz = Qxz + QQxz*cacb
         Qyx = Qyx + QQyx*cacb; Qyy = Qyy + QQyy*cacb; Qyz = Qyz + QQyz*cacb
         Qzx = Qzx + QQzx*cacb; Qzy = Qzy + QQzy*cacb; Qzz = Qzz + QQzz*cacb
      end do
      end do
      call destroy_(QQzz); call destroy_(QQyz); call destroy_(QQxz)
      call destroy_(QQzy); call destroy_(QQyy); call destroy_(QQxy)
      call destroy_(QQzx); call destroy_(QQyx); call destroy_(QQxx)
      call normalise_(self,Qxx); call normalise_(self,Qxy); call normalise_(self,Qxz)
      call normalise_(self,Qyx); call normalise_(self,Qyy); call normalise_(self,Qyz)
      call normalise_(self,Qzx); call normalise_(self,Qzy); call normalise_(self,Qzz)

   end subroutine

   subroutine make_L_ints(self,Lx,Ly,Lz,origin)
    type(shell2_type) :: self
    ! Calculate the angular momentum integral matrices "Lx", "Ly", "Lz",
    ! with "origin" as gauge origin, using Gauss-Hermite quadrature
      real(kind=kind(1.0d0)), dimension(:,:) :: Lx,Ly,Lz
      real(kind=kind(1.0d0)), dimension(3) :: origin
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: LLx,LLy,LLz
       type(gaussian2_type) :: G
      integer(kind=kind(1)) :: a,b
      real(kind=kind(1.0d0)) :: cacb

      Lx = 0.0d0; Ly = 0.0d0; Lz = 0.0d0
      call create_(LLx,self%a%n_comp,self%b%n_comp)
      call create_(LLy,self%a%n_comp,self%b%n_comp)
      call create_(LLz,self%a%n_comp,self%b%n_comp)
      call set_(G,self%a%l,self%a%pos,0.0d0,self%b%l,self%b%pos,0.0d0)
      do a = 1,self%a%n_cc
      do b = 1,self%b%n_cc
         call set_(G,self%a%ex(a),self%b%ex(b))
         call make_L_ints_(G,LLx,LLy,LLz,origin)
         cacb = self%a%cc(a)*self%b%cc(b)
         Lx = Lx + LLx*cacb
         Ly = Ly + LLy*cacb
         Lz = Lz + LLz*cacb
      end do
      end do
      call destroy_(LLz); call destroy_(LLy); call destroy_(LLx)
      call normalise_(self,Lx); call normalise_(self,Ly); call normalise_(self,Lz)

   end subroutine

   subroutine make_solenoidal_jp_ints(self,Jx,Jy,Jz,c)
    type(shell2_type) :: self
    ! Make the solenoidal "Ji" integral matrices evaluated at position "c"
      real(kind=kind(1.0d0)), dimension(:,:) :: Jx,Jy,Jz
       real(kind=kind(1.0d0)), dimension(3) :: c
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: Ix,Iy,Iz
       type(gaussian2_type) :: G
      integer(kind=kind(1)) :: a,b
      real(kind=kind(1.0d0)) :: cacb

      Jx = 0.0d0; Jy = 0.0d0; Jz = 0.0d0
      call create_(Ix,self%a%n_comp,self%b%n_comp)
      call create_(Iy,self%a%n_comp,self%b%n_comp)
      call create_(Iz,self%a%n_comp,self%b%n_comp)
      call set_(G,self%a%l,self%a%pos,0.0d0,self%b%l,self%b%pos,0.0d0)
      do a = 1,self%a%n_cc
      do b = 1,self%b%n_cc
         call set_(G,self%a%ex(a),self%b%ex(b))
         call make_solenoidal_jp_ints_(G,Ix,Iy,Iz,c)
         cacb = self%a%cc(a)*self%b%cc(b)
         Jx = Jx + Ix*cacb
         Jy = Jy + Iy*cacb
         Jz = Jz + Iz*cacb
      end do
      end do
      call destroy_(Iz); call destroy_(Iy); call destroy_(Ix)
      call normalise_(self,Jx); call normalise_(self,Jy); call normalise_(self,Jz)

   end subroutine

   subroutine make_irrotational_jp_ints(self,Jx,Jy,Jz,c)
    type(shell2_type) :: self
    ! Make the irrotational "Ji" integral matrices evaluated at position "c"
      real(kind=kind(1.0d0)), dimension(:,:) :: Jx,Jy,Jz
       real(kind=kind(1.0d0)), dimension(3) :: c
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: Ix,Iy,Iz
       type(gaussian2_type) :: G
      integer(kind=kind(1)) :: a,b
      real(kind=kind(1.0d0)) :: cacb

      Jx = 0.0d0; Jy = 0.0d0; Jz = 0.0d0
      call create_(Ix,self%a%n_comp,self%b%n_comp)
      call create_(Iy,self%a%n_comp,self%b%n_comp)
      call create_(Iz,self%a%n_comp,self%b%n_comp)
      call set_(G,self%a%l,self%a%pos,0.0d0,self%b%l,self%b%pos,0.0d0)
      do a = 1,self%a%n_cc
      do b = 1,self%b%n_cc
         call set_(G,self%a%ex(a),self%b%ex(b))
         call make_irrotational_jp_ints_(G,Ix,Iy,Iz,c)
         cacb = self%a%cc(a)*self%b%cc(b)
         Jx = Jx + Ix*cacb
         Jy = Jy + Iy*cacb
         Jz = Jz + Iz*cacb
      end do
      end do
      call destroy_(Iz); call destroy_(Iy); call destroy_(Ix)
      call normalise_(self,Jx); call normalise_(self,Jy); call normalise_(self,Jz)

   end subroutine

   subroutine make_magnetic_jp_ints(self,Jx,Jy,Jz,c)
    type(shell2_type) :: self
    ! Make the magnetic "Ji" integral matrices evaluated at position "c"
      real(kind=kind(1.0d0)), dimension(:,:) :: Jx,Jy,Jz
       real(kind=kind(1.0d0)), dimension(3) :: c
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: Ix,Iy,Iz
       type(gaussian2_type) :: G
      integer(kind=kind(1)) :: a,b
      real(kind=kind(1.0d0)) :: cacb

      Jx = 0.0d0; Jy = 0.0d0; Jz = 0.0d0
      call create_(Ix,self%a%n_comp,self%b%n_comp)
      call create_(Iy,self%a%n_comp,self%b%n_comp)
      call create_(Iz,self%a%n_comp,self%b%n_comp)
      call set_(G,self%a%l,self%a%pos,0.0d0,self%b%l,self%b%pos,0.0d0)
      do a = 1,self%a%n_cc
      do b = 1,self%b%n_cc
         call set_(G,self%a%ex(a),self%b%ex(b))
         call make_magnetic_jp_ints_(G,Ix,Iy,Iz,c)
         cacb = self%a%cc(a)*self%b%cc(b)
         Jx = Jx + Ix*cacb
         Jy = Jy + Iy*cacb
         Jz = Jz + Iz*cacb
      end do
      end do
      call destroy_(Iz); call destroy_(Iy); call destroy_(Ix)
      call normalise_(self,Jx); call normalise_(self,Jy); call normalise_(self,Jz)

   end subroutine

   subroutine make_magnetic_S_ints(self,Mxx,Mxy,Mxz,Myx,Myy,Myz,Mzx,Mzy,Mzz,c)
    type(shell2_type) :: self
    ! Make the magnetic "Mij" integral matrices evaluated at position "c"
      real(kind=kind(1.0d0)), dimension(:,:) :: Mxx,Mxy,Mxz,Myx,Myy,Myz,Mzx,Mzy,Mzz
      real(kind=kind(1.0d0)), dimension(3) :: c
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: Ixx,Ixy,Ixz,Iyx,Iyy,Iyz,Izx,Izy,Izz
      type(gaussian2_type) :: G
      integer(kind=kind(1)) :: a,b
      real(kind=kind(1.0d0)) :: cacb

      Mxx = 0.0d0; Mxy = 0.0d0; Mxz = 0.0d0
      Myx = 0.0d0; Myy = 0.0d0; Myz = 0.0d0
      Mzx = 0.0d0; Mzy = 0.0d0; Mzz = 0.0d0
      call create_(Ixx,self%a%n_comp,self%b%n_comp)
      call create_(Ixy,self%a%n_comp,self%b%n_comp)
      call create_(Ixz,self%a%n_comp,self%b%n_comp)
      call create_(Iyx,self%a%n_comp,self%b%n_comp)
      call create_(Iyy,self%a%n_comp,self%b%n_comp)
      call create_(Iyz,self%a%n_comp,self%b%n_comp)
      call create_(Izx,self%a%n_comp,self%b%n_comp)
      call create_(Izy,self%a%n_comp,self%b%n_comp)
      call create_(Izz,self%a%n_comp,self%b%n_comp)
      call set_(G,self%a%l,self%a%pos,0.0d0,self%b%l,self%b%pos,0.0d0)
      do a = 1,self%a%n_cc
      do b = 1,self%b%n_cc
         call set_(G,self%a%ex(a),self%b%ex(b))
         call make_magnetic_S_ints_(G,Ixx,Ixy,Ixz,Iyx,Iyy,Iyz,Izx,Izy,Izz,c)
         cacb = self%a%cc(a)*self%b%cc(b)
         Mxx = Mxx + Ixx*cacb; Mxy = Mxy + Ixy*cacb; Mxz = Mxz + Ixz*cacb
         Myx = Myx + Iyx*cacb; Myy = Myy + Iyy*cacb; Myz = Myz + Iyz*cacb
         Mzx = Mzx + Izx*cacb; Mzy = Mzy + Izy*cacb; Mzz = Mzz + Izz*cacb
      end do
      end do
      call destroy_(Izz); call destroy_(Izy); call destroy_(Izx)
      call destroy_(Iyz); call destroy_(Iyy); call destroy_(Iyx)
      call destroy_(Ixz); call destroy_(Ixy); call destroy_(Ixx)
      call normalise_(self,Mxx); call normalise_(self,Mxy); call normalise_(self,Mxz)
      call normalise_(self,Myx); call normalise_(self,Myy); call normalise_(self,Myz)
      call normalise_(self,Mzx); call normalise_(self,Mzy); call normalise_(self,Mzz)

   end subroutine

   subroutine make_magnetic_S_ints_1(self,M,c)
    type(shell2_type) :: self
    ! Make the magnetic "M(:,:,i,j)" integral matrices evaluated at position "c"
      real(kind=kind(1.0d0)), dimension(:,:,:,:), target :: M
      real(kind=kind(1.0d0)), dimension(3) :: c
      real(kind=kind(1.0d0)), dimension(:,:,:,:), pointer :: I
      type(gaussian2_type) :: G
      integer(kind=kind(1)) :: a,b
      real(kind=kind(1.0d0)) :: cacb
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: Mxx,Mxy,Mxz,Myx,Myy,Myz,Mzx,Mzy,Mzz

  call ensure_(tonto,size(M,3)==3,"SHELL2:make_magnetic_S_ints_1 ... wrong shape for M")
  call ensure_(tonto,size(M,4)==3,"SHELL2:make_magnetic_S_ints_1 ... wrong shape for M")
      M = 0.0d0
      call create_(I,self%a%n_comp,self%b%n_comp,3,3)
      call set_(G,self%a%l,self%a%pos,0.0d0,self%b%l,self%b%pos,0.0d0)
      do a = 1,self%a%n_cc
      do b = 1,self%b%n_cc
         call set_(G,self%a%ex(a),self%b%ex(b))
         call make_magnetic_S_ints_(G,I,c)
         cacb = self%a%cc(a)*self%b%cc(b)
         M = M + I*cacb
      end do
      end do
      call destroy_(I)
      Mxx => M(:,:,1,1); Mxy => M(:,:,1,2); Mxz => M(:,:,1,3)
      Myx => M(:,:,2,1); Myy => M(:,:,2,2); Myz => M(:,:,2,3)
      Mzx => M(:,:,3,1); Mzy => M(:,:,3,2); Mzz => M(:,:,3,3)
      call normalise_(self,Mxx); call normalise_(self,Mxy); call normalise_(self,Mxz)
      call normalise_(self,Myx); call normalise_(self,Myy); call normalise_(self,Myz)
      call normalise_(self,Mzx); call normalise_(self,Mzy); call normalise_(self,Mzz)

   end subroutine

!  ******************************
!  Roland Lindh's style integrals
!  ******************************

   elemental function s_overlap(self,zeta,zz,r2) result(res)
    type(shell2_type) :: self
    ! Calculate the overlap of two s functions.
     intent(in) :: self
     real(kind=kind(1.0d0)), intent(in) :: zeta,zz,r2
     real(kind=kind(1.0d0)) :: res
     real(kind=kind(1.0d0)) :: PI_zinv
     PI_zinv = 3.141592653589793d0/zeta
     res=PI_zinv*sqrt(PI_zinv) * exp(-zz*r2)

   end function

   subroutine make_overlap(self,ab)
    type(shell2_type) :: self
    ! Calculate the overlap matrix for the two shells
     real(kind=kind(1.0d0)), dimension(:,:) :: ab
     real(kind=kind(1.0d0)), dimension(:), pointer :: es
     integer(kind=kind(1)) :: n

     n = n_comp_sum_((self%a%l+self%b%l)) - n_comp_sum_((self%l_max-1))
     call create_(es,n)
     call make_overlap_es_(self,es)
     call transfer_(self,es,ab)
     call destroy_(es)
     call normalise_(self,ab)

   end subroutine

   subroutine make_overlap_es(self,es)
    type(shell2_type) :: self
    ! Make the (es) overlap integrals, summed over primitives
     intent(in) :: self
     real(kind=kind(1.0d0)), dimension(:), intent(out) :: es
     real(kind=kind(1.0d0)), dimension(:), pointer :: temp
     real(kind=kind(1.0d0)), dimension(3) :: AB
     integer(kind=kind(1)) :: a,b,templb,tempub,i
     real(kind=kind(1.0d0)) :: r2ab,ea,eb

     tempub = n_comp_sum_(self%l_sum)
     templb = n_comp_sum_((self%l_max-1)) + 1
     call create_(temp,tempub)

     AB = self%a%pos - self%b%pos
     r2ab = dot_product(AB,AB)

     es=0.0d0
     i = 0
     do b=1, self%b%n_cc
       eb=self%b%ex(b)
       do a=1, self%a%n_cc
         i = i + 1
         ea=self%a%ex(a)
         call form_overlap_es_(self,temp,ea,eb,r2ab,i)
         es=es + self%a%cc(a) * self%b%cc(b) * temp(templb:tempub)
       end do
     end do
     call destroy_(temp)

   end subroutine

   subroutine form_overlap_es(self,es,ea,eb,r2ab,i)
    type(shell2_type) :: self
    ! Form the (es) overlap integrals for a pair of primitives
     intent(in) :: self
     real(kind=kind(1.0d0)), dimension(:), intent(inout) :: es
     real(kind=kind(1.0d0)), intent(in) :: r2ab,ea,eb
     integer(kind=kind(1)), intent(in) :: i
     real(kind=kind(1.0d0)), dimension(3) :: PE
     integer(kind=kind(1)), dimension(3) :: a_momenta
     integer(kind=kind(1)), dimension(:,:,:), pointer :: index
     integer(kind=kind(1)), dimension(:,:), pointer :: momenta
     real(kind=kind(1.0d0)) :: half_zinv,zinv,zeta,zz,PEi
     integer(kind=kind(1)) :: j,z,lz,a1,a2,tmp

     zeta = self%exponent_sum(i)
     zinv = self%exponent_inv(i)
     zz=ea*eb/zeta

     es=0.0d0
     es(1)=s_overlap_(self,zeta,zz,r2ab)

     if (self%l_sum > 0) then
       half_zinv = 0.50d0*zinv
       PE = self%center_diff(:,i)
       es(2) = PE(1) * es(1)                                 ! p||s
       es(3) = PE(2) * es(1)
       es(4) = PE(3) * es(1)

       if (self%l_sum > 1) then
         call create_(momenta,3, n_comp_sum_(self%l_sum))
         tmp=0;    call make_gaussian_xyz_powers_(tmp,momenta,self%l_sum)
         call create_(index,0,self%l_sum, 0,self%l_sum, 0,self%l_sum)
         call make_index_of_components_(index,momenta)

         do j=5, n_comp_sum_(self%l_sum)                           ! d||s to .l_sum||s
           a_momenta = momenta(:,j)
           z = index_of_first_nonzero_value_(a_momenta)
           lz = a_momenta(z)
           a_momenta(z) = a_momenta(z)-1
           a1 = index(a_momenta(1),a_momenta(2),a_momenta(3))
           PEi=PE(z)
           if (lz < 2) then
             es(j) = PEi * es(a1)
           else
             a_momenta(z) = a_momenta(z)-1
             a2 = index(a_momenta(1),a_momenta(2),a_momenta(3))
             es(j) = PEi * es(a1) + half_zinv * (lz-1) * es(a2)
           end if
         end do
         call destroy_(index)
         call destroy_(momenta)
       end if
     end if

   end subroutine

   subroutine get_kei(self,kei,overlap)
    type(shell2_type) :: self
    ! Calculate the kinetic energy and overlap (optional) integrals.
    ! (More efficient than making them separate).
     intent(in) :: self
     real(kind=kind(1.0d0)), dimension(:,:), intent(inout) :: kei
     real(kind=kind(1.0d0)), dimension(:,:), intent(inout), optional :: overlap

     call make_kei_(self,kei,overlap)
     if (present(overlap)) call normalise_(self,overlap)
     call normalise_(self,kei)

   end subroutine

   subroutine make_kei(self,kei,overlap)
    type(shell2_type) :: self
    ! Makes the kei and overlap matrics, summed over primitives
    ! Is called by kei, but does not do the orbital normalisation corrections
     intent(in) :: self
     real(kind=kind(1.0d0)), dimension(:,:), intent(inout) :: kei
     real(kind=kind(1.0d0)), dimension(:,:), intent(inout), optional :: overlap
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: temp_kei,temp_overlap
     real(kind=kind(1.0d0)), dimension(3) :: P,PA,PB,ara,brb,AB
     integer(kind=kind(1)) :: a,b,alb,aub,blb,bub
     real(kind=kind(1.0d0)) :: ca,ea,eb,zeta,zinv2,r2ab

     alb = n_comp_sum_((self%a%l-1)) + 1; aub = alb + self%a%n_comp -1
     blb = n_comp_sum_((self%b%l-1)) + 1; bub = blb + self%b%n_comp -1
     call create_(temp_kei,aub,bub)
     call create_(temp_overlap,aub,bub)
     kei=0.0d0
     if (present(overlap)) overlap=0.0d0
     AB = self%a%pos - self%b%pos
     r2ab = dot_product(AB,AB)
     do a=1, self%a%n_cc
       ca=self%a%cc(a)
       ea=self%a%ex(a)
       ara=ea*self%a%pos
       do b=1, self%b%n_cc
         eb    = self%b%ex(b)
         brb   = eb * self%b%pos
         zeta  = ea + eb
         zinv2 = 0.50d0 / zeta
         P     = (ara + brb) / zeta
         PA    = P - self%a%pos
         PB    = P - self%b%pos
         if (self%a%l > self%b%l) then
           call form_overlap_mat_a_(self,temp_kei,temp_overlap,PA,PB,zinv2,zeta,ea,eb,r2ab)
         else
           call form_overlap_mat_b_(self,temp_kei,temp_overlap,PA,PB,zinv2,zeta,ea,eb,r2ab)
         end if
         if (present(overlap)) &
           overlap = overlap + ca * self%b%cc(b) * temp_overlap(alb:aub,blb:bub)
         kei = kei + ca * self%b%cc(b) * temp_kei(alb:aub,blb:bub)
       end do
     end do
     call destroy_(temp_kei)
     call destroy_(temp_overlap)

   end subroutine

   subroutine form_overlap_mat_a(self,kei,overlap,PA,PB,zinv2,zeta,ea,eb,r2ab)
    type(shell2_type) :: self
    ! Makes the kinetic energy and overlap matrices for a primitive pair
    ! if .a.l > .b.l
     intent(in) :: self
     real(kind=kind(1.0d0)), dimension(:,:), intent(inout) :: overlap,kei
     real(kind=kind(1.0d0)), dimension(3), intent(in) :: PA,PB
     real(kind=kind(1.0d0)), intent(in) :: zinv2,zeta,ea,eb,r2ab
     integer(kind=kind(1)), dimension(3) :: a_momenta, b_momenta
     integer(kind=kind(1)), dimension(:,:,:), pointer :: index
     integer(kind=kind(1)), dimension(:,:), pointer :: momenta
     integer(kind=kind(1)) :: j,z,lza,lzb,a,a1,a2,b,b1,laub
     integer(kind=kind(1)) :: la,lb,la1_n_comp_sum,lb1_n_comp_sum,la_n_comp_sum,lb_n_comp_sum
     integer(kind=kind(1)) :: na,b_n_comp_sum,tmp
     real(kind=kind(1.0d0)) :: PAi,PBi,zz,zz2,zinv2_na,zinv2_nb,zz_ea,zz_eb,zz_ea_na

     zz       = ea * eb / zeta
     zz2      = 2 * zz
     zz_ea    = zz / ea
     zz_eb    = zz / eb
     b_n_comp_sum = n_comp_sum_(self%b%l)

     overlap(1,1) = s_overlap_(self,zeta,zz,r2ab)
     kei(1,1)     = zz * (3.0-zz2 * r2ab) * overlap(1,1)

     call create_(momenta,3, n_comp_sum_(self%a%l))
     tmp=0;    call make_gaussian_xyz_powers_(tmp,momenta,self%a%l)
     call create_(index,0,self%a%l, 0,self%a%l, 0,self%a%l)
     call make_index_of_components_(index,momenta)

     do j = 2, b_n_comp_sum                                 ! s||p to s||lb
       a_momenta    = momenta(:,j)                          ! p||s to lb||s
       z            = index_of_first_nonzero_value_(a_momenta)
       lza          = a_momenta(z)
       a_momenta(z) = a_momenta(z) - 1
       a1           = index(a_momenta(1),a_momenta(2),a_momenta(3))
       PBi          = PB(z)
       PAi          = PA(z)
       if (lza < 2) then
         overlap(1,j) = PBi * overlap(1,a1)
         kei(1,j)     = PBi * kei(1,a1) + zz2 * overlap(1,j)
         overlap(j,1) = PAi * overlap(a1,1)
         kei(j,1)     = PAi * kei(a1,1) + zz2 * overlap(j,1)
       else
         na           = lza - 1
         zinv2_na     = zinv2 * na
         a_momenta(z) = a_momenta(z) - 1
         a2           = index(a_momenta(1),a_momenta(2),a_momenta(3))
         overlap(1,j) = PBi * overlap(1,a1) + zinv2_na * overlap(1,a2)
         kei(1,j)     = PBi * kei(1,a1) + zinv2_na * kei(1,a2) + &
                        zz2 * overlap(1,j) - zz_eb * na * overlap(1,a2)
         overlap(j,1) = PAi * overlap(a1,1) + zinv2_na * overlap(a2,1)
         kei(j,1)     = PAi * kei(a1,1) + zinv2_na * kei(a2,1) + &
                        zz2 * overlap(j,1) - zz_ea * na * overlap(a2,1)
       end if
     end do

     do j = b_n_comp_sum + 1, n_comp_sum_((self%a%l-self%b%l))    ! lb||s to la-lb||s
       a_momenta    = momenta(:,j)
       z            = index_of_first_nonzero_value_(a_momenta)
       lza          = a_momenta(z)
       a_momenta(z) = a_momenta(z) - 1
       a1           = index(a_momenta(1),a_momenta(2),a_momenta(3))
       PAi          = PA(z)
       if (lza < 2) then
         overlap(j,1) = PAi * overlap(a1,1)
         kei(j,1)     = PAi * kei(a1,1) + zz2 * overlap(j,1)
       else
         na           = lza - 1
         zinv2_na     = zinv2 * na
         a_momenta(z) = a_momenta(z) - 1
         a2           = index(a_momenta(1),a_momenta(2),a_momenta(3))
         overlap(j,1) = PAi * overlap(a1,1) + zinv2_na * overlap(a2,1)
         kei(j,1)     = PAi * kei(a1,1) + zinv2_na * kei(a2,1) + &
                        zz2 * overlap(j,1) - zz_ea * na * overlap(a2,1)
       end if
     end do

     laub = self%a%l - self%b%l
     do lb = 1, self%b%l
       lb1_n_comp_sum = n_comp_sum_((lb-1)) + 1
       lb_n_comp_sum  = n_comp_sum_(lb)
       laub           = laub + 1
       do la = 1, laub
         la1_n_comp_sum = n_comp_sum_((la-1)) + 1
         la_n_comp_sum  = n_comp_sum_(la)
         do a = la1_n_comp_sum, la_n_comp_sum
           a_momenta    = momenta(:,a)
           z            = index_of_first_nonzero_value_(a_momenta)
           lza          = a_momenta(z)
           a_momenta(z) = a_momenta(z) - 1
           a1           = index(a_momenta(1),a_momenta(2),a_momenta(3))
           PAi          = PA(z)
           if (lza==1) then
             do b = lb1_n_comp_sum,lb_n_comp_sum
               lzb = momenta(z,b)
               if (lzb==0) then
                 overlap(a,b) = PAi * overlap(a1,b)
                 kei(a,b)     = PAi * kei(a1,b) + zz2 * overlap(a,b)
               else
                 b_momenta    = momenta(:,b)
                 b_momenta(z) = b_momenta(z) - 1
                 b1           = index(b_momenta(1),b_momenta(2),b_momenta(3))
                 zinv2_nb     = zinv2 * lzb
                 overlap(a,b) = PAi * overlap(a1,b) + zinv2_nb * overlap(a1,b1)
                 kei(a,b)     = PAi * kei(a1,b) + zinv2_nb * kei(a1,b1) + &
                                zz2 * overlap(a,b)
               end if
             end do
           else
             na           = lza - 1
             zinv2_na     = zinv2 * na
             zz_ea_na    = zz_ea * na
             a_momenta(z) = a_momenta(z) - 1
             a2           = index(a_momenta(1),a_momenta(2),a_momenta(3))
             do b = lb1_n_comp_sum, lb_n_comp_sum
               lzb = momenta(z,b)
               if (lzb==0) then
                 overlap(a,b) = PAi * overlap(a1,b) + zinv2_na * overlap(a2,b)
                 kei(a,b)     = PAi * kei(a1,b) + zinv2_na * kei(a2,b) + &
                                zz2 * overlap(a,b) - zz_ea_na * overlap(a2,b)
               else
                 zinv2_nb     = zinv2 * lzb
                 b_momenta    = momenta(:,b)
                 b_momenta(z) = b_momenta(z) - 1
                 b1           = index(b_momenta(1),b_momenta(2),b_momenta(3))
                 overlap(a,b) = PAi * overlap(a1,b) + zinv2_na * overlap(a2,b) +&
                                zinv2_nb * overlap(a1,b1)
                 kei(a,b)     = PAi * kei(a1,b) + zinv2_na * kei(a2,b) + &
                                zinv2_nb * kei(a1,b1) + &
                                zz2 * overlap(a,b) - zz_ea_na * overlap(a2,b)
               end if
             end do
           end if
         end do
       end do
     end do
     call destroy_(momenta)
     call destroy_(index)

   end subroutine

   subroutine form_overlap_mat_b(self,kei,overlap,PA,PB,zinv2,zeta,ea,eb,r2ab)
    type(shell2_type) :: self
    ! Makes the kinetic energy and overlap matrices for a primitive pair
    ! if .b.l > .a.l
     intent(in) :: self
     real(kind=kind(1.0d0)), dimension(:,:), intent(inout) :: overlap,kei
     real(kind=kind(1.0d0)), dimension(3), intent(in) :: PA,PB
     real(kind=kind(1.0d0)), intent(in) :: zinv2,zeta,ea,eb,r2ab
     integer(kind=kind(1)), dimension(3) :: b_momenta, a_momenta
     integer(kind=kind(1)), dimension(:,:,:), pointer :: index
     integer(kind=kind(1)), dimension(:,:), pointer :: momenta
     integer(kind=kind(1)) :: j,z,lza,lzb,a,a1,b,b1,b2,lbub,la,lb
     integer(kind=kind(1)) :: la1_n_comp_sum,lb1_n_comp_sum,la_n_comp_sum,lb_n_comp_sum
     integer(kind=kind(1)) :: nb,a_n_comp_sum,tmp
     real(kind=kind(1.0d0)) :: PAi,PBi,zz,zz2,zinv2_na,zinv2_nb,zz_ea,zz_eb,zz_eb_nb

     zz      = ea * eb / zeta
     zz2     = 2 * zz
     zz_ea    = zz / ea
     zz_eb    = zz / eb
     a_n_comp_sum  = n_comp_sum_(self%a%l)

     overlap(1,1) = s_overlap_(self,zeta,zz,r2ab)
     kei(1,1)     = zz * (3.0-zz2 * r2ab) * overlap(1,1)

     call create_(momenta,3, n_comp_sum_(self%b%l))
     tmp=0;    call make_gaussian_xyz_powers_(tmp,momenta,self%b%l)
     call create_(index,0,self%b%l, 0,self%b%l, 0,self%b%l)
     call make_index_of_components_(index,momenta)

     do j=2, a_n_comp_sum                                ! p||s to la||s
       b_momenta    = momenta(:,j)                       ! s||p to s||la
       z            = index_of_first_nonzero_value_(b_momenta)
       lzb          = b_momenta(z)
       b_momenta(z) = b_momenta(z)-1
       b1           = index(b_momenta(1),b_momenta(2),b_momenta(3))
       PBi          = PB(z)
       PAi          = PA(z)
       if (lzb < 2) then
         overlap(j,1) = PAi * overlap(b1,1)
         kei(j,1)     = PAi * kei(b1,1) + zz2 * overlap(j,1)
         overlap(1,j) = PBi * overlap(1,b1)
         kei(1,j)     = PBi * kei(1,b1) + zz2 * overlap(1,j)
       else
         nb           = lzb - 1
         zinv2_nb     = zinv2 * nb
         b_momenta(z) = b_momenta(z) - 1
         b2           = index(b_momenta(1),b_momenta(2),b_momenta(3))
         overlap(j,1) = PAi * overlap(b1,1) + zinv2_nb * overlap(b2,1)
         kei(j,1)     = PAi * kei(b1,1) + zinv2_nb * kei(b2,1) + &
                        zz2 * overlap(j,1) - zz_ea * nb * overlap(b2,1)
         overlap(1,j) = PBi * overlap(1,b1) + zinv2_nb * overlap(1,b2)
         kei(1,j)     = PBi * kei(1,b1) + zinv2_nb * kei(1,b2) + &
                        zz2 * overlap(1,j) - zz_eb * nb * overlap(1,b2)
       end if
     end do

     do j= a_n_comp_sum + 1, n_comp_sum_((self%b%l-self%a%l))       ! s||lb to s||lb-la
       b_momenta    = momenta(:,j)                        ! s||p to s||la
       z            = index_of_first_nonzero_value_(b_momenta)
       lzb          = b_momenta(z)
       b_momenta(z) = b_momenta(z) - 1
       b1           = index(b_momenta(1),b_momenta(2),b_momenta(3))
       PBi          = PB(z)
       if (lzb < 2) then
         overlap(1,j) = PBi * overlap(1,b1)
         kei(1,j)     = PBi * kei(1,b1) + zz2 * overlap(1,j)
       else
         nb           = lzb - 1
         zinv2_nb     = zinv2 * nb
         b_momenta(z) = b_momenta(z) - 1
         b2           = index(b_momenta(1),b_momenta(2),b_momenta(3))
         overlap(1,j) = PBi * overlap(1,b1) + zinv2_nb * overlap(1,b2)
         kei(1,j)     = PBi * kei(1,b1) + zinv2_nb * kei(1,b2) + &
                        zz2 * overlap(1,j) - zz_eb * nb * overlap(1,b2)
       end if
     end do

     lbub = self%b%l - self%a%l
     do la = 1, self%a%l
       la1_n_comp_sum = n_comp_sum_((la-1)) + 1
       la_n_comp_sum  = n_comp_sum_(la)
       lbub           = lbub + 1
       do lb = 1, lbub
         lb1_n_comp_sum = n_comp_sum_((lb-1))+1
         lb_n_comp_sum  = n_comp_sum_(lb)
         do b = lb1_n_comp_sum, lb_n_comp_sum
           b_momenta    = momenta(:,b)
           z            = index_of_first_nonzero_value_(b_momenta)
           lzb          = b_momenta(z)
           b_momenta(z) = b_momenta(z) - 1
           b1           = index(b_momenta(1),b_momenta(2),b_momenta(3))
           PBi          = PB(z)
           if (lzb == 1) then
             do a = la1_n_comp_sum, la_n_comp_sum
               lza = momenta(z,a)
               if (lza == 0) then
                 overlap(a,b) = PBi * overlap(a,b1)
                 kei(a,b)     = PBi * kei(a,b1) + zz2 * overlap(a,b)
               else
                 a_momenta    = momenta(:,a)
                 a_momenta(z) = a_momenta(z) - 1
                 a1           = index(a_momenta(1),a_momenta(2),a_momenta(3))
                 zinv2_na     = zinv2 * lza
                 overlap(a,b) = PBi * overlap(a,b1) + zinv2_na * overlap(a1,b1)
                 kei(a,b)     = PBi * kei(a,b1) + zinv2_na * kei(a1,b1) + &
                                zz2 * overlap(a,b)
               end if
             end do
           else
             nb           = lzb - 1
             zinv2_nb     = zinv2 * nb
             zz_eb_nb     = zz_eb * nb
             b_momenta(z) = b_momenta(z) - 1
             b2           = index(b_momenta(1),b_momenta(2),b_momenta(3))
             do a = la1_n_comp_sum, la_n_comp_sum
               lza = momenta(z,a)
               if (lza==0) then
                 overlap(a,b) = PBi * overlap(a,b1) + zinv2_nb * overlap(a,b2)
                 kei(a,b)     = PBi * kei(a,b1) + zinv2_nb * kei(a,b2)+ &
                                zz2 * overlap(a,b) - zz_eb_nb * overlap(a,b2)
               else
                 zinv2_na     = zinv2 * lza
                 a_momenta    = momenta(:,a)
                 a_momenta(z) = a_momenta(z) - 1
                 a1           = index(a_momenta(1),a_momenta(2),a_momenta(3))
                 overlap(a,b) = PBi * overlap(a,b1) + zinv2_nb * overlap(a,b2) +&
                                zinv2_na * overlap(a1,b1)
                 kei(a,b)     = PBi * kei(a,b1) + zinv2_nb * kei(a,b2) + &
                                zinv2_na * kei(a1,b1) + &
                                zz2 * overlap(a,b) - zz_eb_nb * overlap(a,b2)
               end if
             end do
           end if
         end do
       end do
     end do
     call destroy_(momenta)
     call destroy_(index)

   end subroutine

   subroutine get_nuc(self,ab,mass_c,pos_c)
    type(shell2_type) :: self
    ! Calculate the nuclear attraction matrix for the two shells with the
    ! nucleus at c.
     intent(in) :: self
     real(kind=kind(1.0d0)), dimension(:,:), intent(out) :: ab
     real(kind=kind(1.0d0)), intent(in) :: mass_c
     real(kind=kind(1.0d0)), dimension(3), intent(in) :: pos_c
     real(kind=kind(1.0d0)), dimension(:), pointer :: es
     real(kind=kind(1.0d0)) :: ss
     integer(kind=kind(1)) :: n

     if (self%l_sum==0) then   ! ss
       call make_nuc_ss_(self,ss,mass_c,pos_c)
       ab(1,1)=ss
     else if (self%l_sum==1) then  ! ps or sp
       call make_nuc_ps_(self,ab,mass_c,pos_c)
     else if (self%l_max==1) then
       call make_nuc_pp_(self,ab,mass_c,pos_c)
     else if (self%l_sum==2 .and. self%l_max==2) then  ! ds or sd
       call make_nuc_ds_(self,ab,mass_c,pos_c)
       call normalise_(self,ab)
     else if (self%l_sum==3 .and. self%l_max==1) then  ! dp or pd
       call make_nuc_dp_(self,ab,mass_c,pos_c)
       call normalise_(self,ab)
     else
       n = n_comp_sum_((self%l_sum)) - n_comp_sum_((self%l_max-1))
       call create_(es,n)
       call make_nuc_es_(self,es,mass_c,pos_c)
       call transfer_(self,es,ab)
       call destroy_(es)
       call normalise_(self,ab)
     end if

   end subroutine

   subroutine make_nuc_es(self,es,mass_c,pos_c)
    type(shell2_type) :: self
    ! Make the (es) nuclear attraction integrals, summed over primitives
    ! Numbers may be slightly different to cadpac due to a relativistic
    ! correction term.
     intent(in) :: self
     real(kind=kind(1.0d0)), dimension(:), intent(out) :: es
     real(kind=kind(1.0d0)), intent(in) :: mass_c
     real(kind=kind(1.0d0)), dimension(3), intent(in) :: pos_c
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: Ixa,Iya,Iza
     real(kind=kind(1.0d0)), dimension(:), pointer :: Ix,Iy,Iz
     type(rys_type), pointer :: rysa
     real(kind=kind(1.0d0)) :: rzt,ce,e1_ce,rho_zinv,half_zinv,Ix2,Iy2,Iz2,wt
     real(kind=kind(1.0d0)) :: QPx,QPy,QPz,PAx,PAy,PAz,zeta,zinv,rho,xx,eta_c,two_pi
     real(kind=kind(1.0d0)) :: Ixe,Iye,Ize,Ixe1,Iye1,Ize1,Ixep1,Iyep1,Izep1
     integer(kind=kind(1)) :: ag,bg,e,ep1,eub,nroots,i,j,n,n_sum

     if (mass_c < 10.0d0**(-15)) then
       eta_c=1.0d30   ! Very big.
     else
       eta_c=3880000000d0*(mass_c**(-0.66666666666666666666667d0))
     end if
     eta_c = 1.0d60  ! uncomment this to get rid of relativistic correction.

     eub  = size(es)
     nroots=(self%l_sum+2)/2
     n_sum = nroots * self%a%n_cc * self%b%n_cc
     call create_(rysa,nroots)
     call create_(Ixa,n_sum,self%l_sum+1)
     call create_(Iya,n_sum,self%l_sum+1)
     call create_(Iza,n_sum,self%l_sum+1)

     two_pi=2.0d0*3.141592653589793d0
     i = 0
     j = 0
     do bg = 1, self%b%n_cc
       do ag = 1, self%a%n_cc
         i     = i + 1
         zeta  = self%exponent_sum(i)
         zinv  = self%exponent_inv(i)
         rho   = zeta * eta_c / (zeta + eta_c)
         PAx   = self%center_diff(1,i)
         PAy   = self%center_diff(2,i)
         PAz   = self%center_diff(3,i)
         QPx   = pos_c(1) - self%pair_center(1,i)
         QPy   = pos_c(2) - self%pair_center(2,i)
         QPz   = pos_c(3) - self%pair_center(3,i)
         xx    = rho * (QPx*QPx + QPy*QPy + QPz*QPz)
         call get_weights_(rysa,xx)
         rysa%w = rysa%w * (two_pi * self%cc_prefactor(i) * sqrt(rho))
         half_zinv = 0.50d0 * zinv
         rho_zinv = rho * zinv

         do n = 1, nroots
           j = j + 1
           Ix => Ixa(j,:)
           Iy => Iya(j,:)
           Iz => Iza(j,:)
           wt = rysa%w(n)
           Ix(1) = 1.0d0
           Iy(1) = 1.0d0
           Iz(1) = wt
           if (self%l_sum>0) then
              rzt   = rysa%r(n)*rho_zinv
              Ix2 = PAx+rzt*QPx
              Iy2 = PAy+rzt*QPy
              Iz2 = PAz+rzt*QPz
              Ix(2) = Ix2;    Iy(2) = Iy2;    Iz(2) = Iz2 * wt
              if (self%l_sum>1) then
                ce    = (1.0d0-rzt)*half_zinv
                Ixep1 = Ix2*Ix2+ce
                Iyep1 = Iy2*Iy2+ce
                Izep1 = Iz2*Iz2+ce
                Ix(3) = Ixep1
                Iy(3) = Iyep1
                Iz(3) = Izep1 * wt
                if (self%l_sum>2) then
                  Ixe1 = Ix2;      Iye1 = Iy2;      Ize1 = Iz2
                  Ixe = Ixep1;     Iye = Iyep1;     Ize = Izep1
                  do e = 3,self%l_sum
                    ep1   = e+1
                    e1_ce = (e-1)*ce
                    Ixep1 = Ix2*Ixe+e1_ce*Ixe1
                    Iyep1 = Iy2*Iye+e1_ce*Iye1
                    Izep1 = Iz2*Ize+e1_ce*Ize1
                    Ix(ep1) = Ixep1; Iy(ep1) = Iyep1; Iz(ep1) = Izep1 * wt
                    Ixe1 = Ixe;      Iye1 = Iye;      Ize1 = Ize
                    Ixe = Ixep1;     Iye = Iyep1;     Ize = Izep1
                  end do
                end if
              end if
           end if
         end do
       end do
     end do

     call combine_nuc_2d_ints_(self,es,Ixa,Iya,Iza,eub)

     call destroy_(Iza)
     call destroy_(Iya)
     call destroy_(Ixa)
     call destroy_(rysa)

   end subroutine

   subroutine combine_nuc_2d_ints(self,es,Ix,Iy,Iz,eub)
    type(shell2_type) :: self
    ! ?
     real(kind=kind(1.0d0)), dimension(:) :: es
     real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: Ix,Iy,Iz
     integer(kind=kind(1)), intent(in) :: eub
     integer(kind=kind(1)), dimension(:,:), pointer :: e_powers
     integer(kind=kind(1)), dimension(:), pointer :: e_x,e_y,e_z
     integer(kind=kind(1)) :: e

     call create_(e_powers,eub,3)
     e_x => e_powers(:,1); e_y => e_powers(:,2); e_z => e_powers(:,3)
     call make_gaussian_xyz_indices_(self%l_max,e_x,e_y,e_z,self%l_sum)
     forall (e=1:eub)
       es(e) = sum( Ix(:,e_x(e)) * Iy(:,e_y(e)) * Iz(:,e_z(e)) )
     end forall
     call destroy_(e_powers)

   end subroutine

   subroutine make_nuc_dp(self,dp,mass_c,pos_c)
    type(shell2_type) :: self
    ! Make the (dp) nuclear attraction integrals, summed over primitives
    ! Numbers may be slightly different to cadpac due to a relativistic
    ! correction term.
     intent(in) :: self
     real(kind=kind(1.0d0)), dimension(:,:), intent(out) :: dp
     real(kind=kind(1.0d0)), intent(in) :: mass_c
     real(kind=kind(1.0d0)), dimension(3), intent(in) :: pos_c
     type(rys_type), pointer :: rysa
     real(kind=kind(1.0d0)) :: rzt,ce,rho_zinv,half_zinv,wt
     real(kind=kind(1.0d0)) :: QPx,QPy,QPz,PAx,PAy,PAz,zeta,zinv,rho,xx,eta_c,two_pi
     real(kind=kind(1.0d0)) :: Ix2,Iy2,Iz2,Ix3,Iy3,Iz3,two_ce,ABx,ABy,ABz
     real(kind=kind(1.0d0)) :: dxx_s,dyy_s,dzz_s,dxy_s,dxz_s,dyz_s,fxxx_s,fyyy_s,fzzz_s
     real(kind=kind(1.0d0)) :: fxxy_s,fxxz_s,fxyy_s,fyyz_s,fxzz_s,fyzz_s,fxyz_s
     integer(kind=kind(1)) :: ag,bg,i,j,n

     if (mass_c < 10.0d0**(-15)) then
       eta_c=1.0d30   ! Very big.
     else
       eta_c=3880000000d0*(mass_c**(-0.66666666666666666666667d0))
     end if
     eta_c = 1.0d60  ! uncomment this to get rid of relativistic correction.

     call create_(rysa,2)

     two_pi=2.0d0*3.141592653589793d0
     i = 0
     j = 0
     dxx_s = 0.0d0;  dyy_s = 0.0d0;  dzz_s = 0.0d0
     dxy_s = 0.0d0;  dxz_s = 0.0d0;  dyz_s = 0.0d0
     fxxx_s = 0.0d0; fyyy_s = 0.0d0; fzzz_s = 0.0d0
     fxxy_s = 0.0d0; fxxz_s = 0.0d0; fxyy_s = 0.0d0
     fyyz_s = 0.0d0; fxzz_s = 0.0d0; fyzz_s = 0.0d0
     fxyz_s = 0.0d0
     do bg = 1, self%b%n_cc
       do ag = 1, self%a%n_cc
         i     = i + 1
         zeta  = self%exponent_sum(i)
         zinv  = self%exponent_inv(i)
         rho   = zeta * eta_c / (zeta + eta_c)
         PAx   = self%center_diff(1,i)
         PAy   = self%center_diff(2,i)
         PAz   = self%center_diff(3,i)
         QPx   = pos_c(1) - self%pair_center(1,i)
         QPy   = pos_c(2) - self%pair_center(2,i)
         QPz   = pos_c(3) - self%pair_center(3,i)
         xx    = rho * (QPx*QPx + QPy*QPy + QPz*QPz)
         call get_weights_(rysa,xx)
         rysa%w = rysa%w * (two_pi * self%cc_prefactor(i) * sqrt(rho))
         half_zinv = 0.50d0 * zinv
         rho_zinv = rho * zinv

         do n = 1, 2
           j = j + 1
           wt = rysa%w(n)
           rzt   = rysa%r(n)*rho_zinv
           ce    = (1.0d0-rzt)*half_zinv
           Ix2 = PAx+rzt*QPx
           Iy2 = PAy+rzt*QPy
           Iz2 = PAz+rzt*QPz
           Ix3 = Ix2*Ix2+ce
           Iy3 = Iy2*Iy2+ce
           Iz3 = (Iz2*Iz2+ce)*wt
           Iz2 = Iz2 * wt
           two_ce = ce+ce
           dxx_s  = dxx_s  + Ix3 * wt
           dyy_s  = dyy_s  + Iy3 * wt
           dzz_s  = dzz_s  + Iz3
           dxy_s  = dxy_s  + Ix2 * Iy2 * wt
           dxz_s  = dxz_s  + Ix2 * Iz2
           dyz_s  = dyz_s  + Iy2 * Iz2
           fxxx_s = fxxx_s + Ix2*(Ix3+two_ce) * wt
           fyyy_s = fyyy_s + Iy2*(Iy3+two_ce) * wt
           fzzz_s = fzzz_s + Iz2*(Iz3+two_ce) * wt
           fxxy_s = fxxy_s + Ix3 * Iy2 * wt
           fxxz_s = fxxz_s + Ix3 * Iz2
           fxyy_s = fxyy_s + Ix2 * Iy3 * wt
           fyyz_s = fyyz_s + Iy3 * Iz2
           fxzz_s = fxzz_s + Ix3 * Iz3
           fyzz_s = fyzz_s + Iy2 * Iz3
           fxyz_s = fxyz_s + Ix2 * Iy2 * Iz2
         end do
       end do
     end do

     if (self%a%l==2) then
       ABx = self%a%pos(1) - self%b%pos(1)
       ABy = self%a%pos(2) - self%b%pos(2)
       ABz = self%a%pos(3) - self%b%pos(3)
       dp(1,1)      = fxxx_s + ABx * dxx_s
       dp(2,1)      = fxyy_s + ABx * dyy_s
       dp(3,1)      = fxzz_s + ABx * dzz_s
       dp(4,1)      = fxxy_s + ABx * dxy_s
       dp(5,1)      = fxxz_s + ABx * dxz_s
       dp(6,1)      = fxyz_s + ABx * dyz_s
       dp(1,2)      = fxxy_s + ABy * dxx_s
       dp(2,2)      = fyyy_s + ABy * dyy_s
       dp(3,2)      = fyzz_s + ABy * dzz_s
       dp(4,2)      = fxyy_s + ABy * dxy_s
       dp(5,2)      = fxyz_s + ABy * dxz_s
       dp(6,2)      = fyyz_s + ABy * dyz_s
       dp(1,3)      = fxxz_s + ABz * dxx_s
       dp(2,3)      = fyyz_s + ABz * dyy_s
       dp(3,3)      = fzzz_s + ABz * dzz_s
       dp(4,3)      = fxyz_s + ABz * dxy_s
       dp(5,3)      = fxzz_s + ABz * dxz_s
       dp(6,3)      = fyzz_s + ABz * dyz_s
     else
       ABx = self%b%pos(1) - self%a%pos(1)
       ABy = self%b%pos(2) - self%a%pos(2)
       ABz = self%b%pos(3) - self%a%pos(3)
       dp(1,1)      = fxxx_s + ABx * dxx_s
       dp(1,2)      = fxyy_s + ABx * dyy_s
       dp(1,3)      = fxzz_s + ABx * dzz_s
       dp(1,4)      = fxxy_s + ABx * dxy_s
       dp(1,5)      = fxxz_s + ABx * dxz_s
       dp(1,6)      = fxyz_s + ABx * dyz_s
       dp(2,1)      = fxxy_s + ABy * dxx_s
       dp(2,2)      = fyyy_s + ABy * dyy_s
       dp(2,3)      = fyzz_s + ABy * dzz_s
       dp(2,4)      = fxyy_s + ABy * dxy_s
       dp(2,5)      = fxyz_s + ABy * dxz_s
       dp(2,6)      = fyyz_s + ABy * dyz_s
       dp(3,1)      = fxxz_s + ABz * dxx_s
       dp(3,2)      = fyyz_s + ABz * dyy_s
       dp(3,3)      = fzzz_s + ABz * dzz_s
       dp(3,4)      = fxyz_s + ABz * dxy_s
       dp(3,5)      = fxzz_s + ABz * dxz_s
       dp(3,6)      = fyzz_s + ABz * dyz_s
     end if

     call destroy_(rysa)

   end subroutine

   subroutine make_nuc_ds(self,ds,mass_c,pos_c)
    type(shell2_type) :: self
    ! Make the (ds) nuclear attraction integrals, summed over primitives
    ! Numbers may be slightly different to cadpac due to a relativistic
    ! correction term.
     intent(in) :: self
     real(kind=kind(1.0d0)), dimension(:,:), intent(out) :: ds
     real(kind=kind(1.0d0)), intent(in) :: mass_c
     real(kind=kind(1.0d0)), dimension(3), intent(in) :: pos_c
     type(rys_type), pointer :: rysa
     real(kind=kind(1.0d0)) :: rzt,ce,rho_zinv,half_zinv,Ix2,Iy2,Iz2,wt,Iz2_wt
     real(kind=kind(1.0d0)) :: QPx,QPy,QPz,PAx,PAy,PAz,zeta,zinv,rho,xx,eta_c,two_pi
     real(kind=kind(1.0d0)) :: dxx_s,dyy_s,dzz_s,dxy_s,dxz_s,dyz_s
     integer(kind=kind(1)) :: ag,bg,i,n

     if (mass_c < 10.0d0**(-15)) then
       eta_c=1.0d30   ! Very big.
     else
       eta_c=3880000000d0*(mass_c**(-0.66666666666666666666667d0))
     end if
     eta_c = 1.0d60  ! uncomment this to get rid of relativistic correction.

     call create_(rysa,2)
     two_pi=2.0d0*3.141592653589793d0
     dxx_s = 0.0d0;    dyy_s = 0.0d0;    dzz_s = 0.0d0
     dxy_s = 0.0d0;    dxz_s = 0.0d0;    dyz_s = 0.0d0
     i = 0
     do bg = 1, self%b%n_cc
       do ag = 1, self%a%n_cc
         i     = i + 1
         zeta  = self%exponent_sum(i)
         zinv  = self%exponent_inv(i)
         rho   = zeta * eta_c / (zeta + eta_c)
         PAx   = self%center_diff(1,i)
         PAy   = self%center_diff(2,i)
         PAz   = self%center_diff(3,i)
         QPx   = pos_c(1) - self%pair_center(1,i)
         QPy   = pos_c(2) - self%pair_center(2,i)
         QPz   = pos_c(3) - self%pair_center(3,i)
         xx    = rho * (QPx*QPx + QPy*QPy + QPz*QPz)
         call get_weights_(rysa,xx)
         rysa%w = rysa%w * (two_pi * self%cc_prefactor(i) * sqrt(rho))
         half_zinv = 0.50d0 * zinv
         rho_zinv = rho * zinv
         do n = 1,2
           wt = rysa%w(n)
           rzt   = rysa%r(n)*rho_zinv
           Ix2 = PAx+rzt*QPx
           Iy2 = PAy+rzt*QPy
           Iz2 = PAz+rzt*QPz
           ce  = (1.0d0-rzt)*half_zinv
           Iz2_wt = Iz2 * wt
           dxx_s = dxx_s + (Ix2*Ix2+ce) * wt
           dyy_s = dyy_s + (Iy2*Iy2+ce) * wt
           dzz_s = dzz_s + (Iz2*Iz2+ce) * wt
           dxy_s = dxy_s + Ix2 * Iy2    * wt
           dxz_s = dxz_s + Ix2 *      Iz2_wt
           dyz_s = dyz_s +      Iy2 * Iz2_wt
         end do
       end do
     end do
     call destroy_(rysa)
     if (self%a%l==2) then
       ds(1,1) = dxx_s
       ds(2,1) = dyy_s
       ds(3,1) = dzz_s
       ds(4,1) = dxy_s
       ds(5,1) = dxz_s
       ds(6,1) = dyz_s
     else
       ds(1,1) = dxx_s
       ds(1,2) = dyy_s
       ds(1,3) = dzz_s
       ds(1,4) = dxy_s
       ds(1,5) = dxz_s
       ds(1,6) = dyz_s
     end if

   end subroutine

   subroutine make_nuc_pp(self,pp,mass_c,pos_c)
    type(shell2_type) :: self
    ! Make the (es) nuclear attraction integrals, summed over primitives
    ! Numbers may be slightly different to cadpac due to a relativistic
    ! correction term.
     intent(in) :: self
     real(kind=kind(1.0d0)), dimension(:,:), intent(out) :: pp
     real(kind=kind(1.0d0)), intent(in) :: mass_c
     real(kind=kind(1.0d0)), dimension(3), intent(in) :: pos_c
     type(rys_type), pointer :: rysa
     real(kind=kind(1.0d0)) :: rzt,ce,rho_zinv,half_zinv,Ix2,Iy2,Iz2,wt,Iy2_wt,Iz2_wt
     real(kind=kind(1.0d0)) :: QPx,QPy,QPz,PAx,PAy,PAz,zeta,zinv,rho,xx,eta_c,two_pi
     real(kind=kind(1.0d0)) :: BAx,BAy,BAz,px_s,py_s,pz_s,dxx_s,dyy_s,dzz_s,dxy_s,dxz_s,dyz_s
     integer(kind=kind(1)) :: ag,bg,i,n

     if (mass_c < 10.0d0**(-15)) then
       eta_c=1.0d30   ! Very big.
     else
       eta_c=3880000000d0*(mass_c**(-0.66666666666666666666667d0))
     end if
     eta_c = 1.0d60  ! uncomment this to get rid of relativistic correction.

     call create_(rysa,2)

     two_pi=2.0d0*3.141592653589793d0
     i = 0
     px_s = 0.0d0;   py_s = 0.0d0;   pz_s = 0.0d0
     dxx_s = 0.0d0;   dyy_s = 0.0d0;   dzz_s = 0.0d0
     dxy_s = 0.0d0;   dxz_s = 0.0d0;   dyz_s = 0.0d0
     do bg = 1, self%b%n_cc
       do ag = 1, self%a%n_cc
         i     = i + 1
         zeta  = self%exponent_sum(i)
         zinv  = self%exponent_inv(i)
         rho   = zeta * eta_c / (zeta + eta_c)
         PAx   = self%center_diff(1,i)
         PAy   = self%center_diff(2,i)
         PAz   = self%center_diff(3,i)
         QPx   = pos_c(1) - self%pair_center(1,i)
         QPy   = pos_c(2) - self%pair_center(2,i)
         QPz   = pos_c(3) - self%pair_center(3,i)
         xx    = rho * (QPx*QPx + QPy*QPy + QPz*QPz)
         call get_weights_(rysa,xx)
         rysa%w = rysa%w * (two_pi * self%cc_prefactor(i) * sqrt(rho))
         half_zinv = 0.50d0 * zinv
         rho_zinv = rho * zinv

         do n = 1,2
           wt = rysa%w(n)
           rzt = rysa%r(n)*rho_zinv
           Ix2 = PAx+rzt*QPx
           Iy2 = PAy+rzt*QPy
           Iz2 = PAz+rzt*QPz
           ce  = (1.0d0-rzt)*half_zinv
           Iz2_wt = Iz2 * wt
           Iy2_wt = Iy2 * wt
           px_s = px_s + Ix2 * wt
           py_s = py_s + Iy2_wt
           pz_s = pz_s + Iz2_wt
           dxx_s = dxx_s + (Ix2*Ix2+ce) * wt
           dyy_s = dyy_s + (Iy2*Iy2+ce) * wt
           dzz_s = dzz_s + (Iz2*Iz2+ce) * wt
           dxy_s = dxy_s + Ix2 * Iy2_wt
           dxz_s = dxz_s + Ix2 * Iz2_wt
           dyz_s = dyz_s + Iy2 * Iz2_wt
         end do
       end do
     end do
     call destroy_(rysa)

     BAx = self%b%pos(1) - self%a%pos(1)
     BAy = self%b%pos(2) - self%a%pos(2)
     BAz = self%b%pos(3) - self%a%pos(3)
     pp(1,1) = dxx_s + BAx * px_s
     pp(1,2) = dxy_s + BAx * py_s
     pp(1,3) = dxz_s + BAx * pz_s
     pp(2,1) = dxy_s + BAy * px_s
     pp(2,2) = dyy_s + BAy * py_s
     pp(2,3) = dyz_s + BAy * pz_s
     pp(3,1) = dxz_s + BAz * px_s
     pp(3,2) = dyz_s + BAz * py_s
     pp(3,3) = dzz_s + BAz * pz_s

   end subroutine

   subroutine make_nuc_ps(self,ps,mass_c,pos_c)
    type(shell2_type) :: self
    ! Make the (ps) nuclear attraction integrals, summed over primitives
    ! Numbers may be slightly different to cadpac due to a relativistic
    ! correction term.
     intent(in) :: self
     real(kind=kind(1.0d0)), dimension(:,:), intent(out) :: ps
     real(kind=kind(1.0d0)), intent(in) :: mass_c
     real(kind=kind(1.0d0)), dimension(3), intent(in) :: pos_c
     type(rys_type), pointer :: rysa
     real(kind=kind(1.0d0)) :: rzt,wt
     real(kind=kind(1.0d0)) :: QPx,QPy,QPz,PAx,PAy,PAz,zeta,rho,xx,eta_c,two_pi,ps_x,ps_y,ps_z
     integer(kind=kind(1)) :: ag,bg,i

     if (mass_c < 10.0d0**(-15)) then
       eta_c=1.0d30   ! Very big.
     else
       eta_c=3880000000d0*(mass_c**(-0.66666666666666666666667d0))
     end if
     eta_c = 1.0d60  ! uncomment this to get rid of relativistic correction.

     call create_(rysa,1)

     two_pi=2.0d0*3.141592653589793d0
     i = 0
     ps_x = 0.0d0;   ps_y = 0.0d0;   ps_z = 0.0d0
     do bg = 1, self%b%n_cc
       do ag = 1, self%a%n_cc
         i     = i + 1
         zeta  = self%exponent_sum(i)
         rho   = zeta * eta_c / (zeta + eta_c)
         PAx   = self%center_diff(1,i)
         PAy   = self%center_diff(2,i)
         PAz   = self%center_diff(3,i)
         QPx   = pos_c(1) - self%pair_center(1,i)
         QPy   = pos_c(2) - self%pair_center(2,i)
         QPz   = pos_c(3) - self%pair_center(3,i)
         xx    = rho * (QPx*QPx + QPy*QPy + QPz*QPz)
         call get_weights_(rysa,xx)
         wt = rysa%w(1) * (two_pi * self%cc_prefactor(i) * sqrt(rho))
         rzt   = rysa%r(1) * rho * self%exponent_inv(i)
         ps_x = ps_x + (PAx+rzt*QPx) * wt
         ps_y = ps_y + (PAy+rzt*QPy) * wt
         ps_z = ps_z + (PAz+rzt*QPz) * wt
       end do
     end do
     if (self%a%l==1) then
       ps(1,1) = ps_x
       ps(2,1) = ps_y
       ps(3,1) = ps_z
     else
       ps(1,1) = ps_x
       ps(1,2) = ps_y
       ps(1,3) = ps_z
     end if
     call destroy_(rysa)

   end subroutine

   subroutine make_nuc_ss(self,es,mass_c,pos_c)
    type(shell2_type) :: self
    ! Make the (ss) nuclear attraction integrals, summed over primitives
    ! Numbers may be slightly different to cadpac due to a relativistic
    ! correction term.
     intent(in) :: self
     real(kind=kind(1.0d0)), intent(out) :: es
     real(kind=kind(1.0d0)), intent(in) :: mass_c
     real(kind=kind(1.0d0)), dimension(3), intent(in) :: pos_c
     type(rys_type), pointer :: rysa
     real(kind=kind(1.0d0)) :: QPx,QPy,QPz,zeta,rho,xx,eta_c,two_pi
     integer(kind=kind(1)) :: ag,bg,i

     if (mass_c < 10.0d0**(-15)) then
       eta_c=1.0d30   ! Very big.
     else
       eta_c=3880000000d0*(mass_c**(-0.66666666666666666666667d0))
     end if
     eta_c = 1.0d60  ! uncomment this to get rid of relativistic correction.

     call create_(rysa,1)
     two_pi=2.0d0*3.141592653589793d0
     es = 0.0d0
     i = 0
     do bg = 1, self%b%n_cc
       do ag = 1, self%a%n_cc
         i     = i + 1
         zeta  = self%exponent_sum(i)
         rho   = zeta * eta_c / (zeta + eta_c)
         QPx   = pos_c(1) - self%pair_center(1,i)
         QPy   = pos_c(2) - self%pair_center(2,i)
         QPz   = pos_c(3) - self%pair_center(3,i)
         xx    = rho * (QPx*QPx + QPy*QPy + QPz*QPz)
         call get_weights_(rysa,xx)
         es    = es + (two_pi * self%cc_prefactor(i) * sqrt(rho)) * rysa%w(1)
       end do
     end do
     call destroy_(rysa)

   end subroutine

   subroutine transfer(self,es,ab)
    type(shell2_type) :: self
    ! Applies the transfer equation to (e|s) to give (a|b)
     intent(in) :: self
     real(kind=kind(1.0d0)), dimension(:), intent(in) :: es
     real(kind=kind(1.0d0)), dimension(:,:), intent(out) :: ab

     if (self%a%l > self%b%l) then
       call transfer_l_a_highest_(self,es,ab)
     else
       call transfer_l_b_highest_(self,es,ab)
     end if

   end subroutine

   subroutine transfer_l_a_highest(self,es,ab)
    type(shell2_type) :: self
    ! Applies the transfer equation to (e|s) to give (a|b)
    ! where a has the higher angular momentum.
     intent(in) :: self
     real(kind=kind(1.0d0)), dimension(:), intent(in) :: es
     real(kind=kind(1.0d0)), dimension(:,:), intent(out) :: ab
     real(kind=kind(1.0d0)), dimension(3) :: ABvec
     integer(kind=kind(1)), dimension(:,:), pointer :: momenta
     integer(kind=kind(1)), dimension(:,:,:), pointer :: index
     integer(kind=kind(1)), dimension(3) :: a_momenta,b_momenta
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: int_new,int_old
     integer(kind=kind(1)) :: a,b,lb,aub,bub,lb1_n_comp_sum,lb2_n_comp_sum
     integer(kind=kind(1)) :: a1,b1,z,eadd,tmp
     real(kind=kind(1.0d0)) :: ABi

     select case (self%b%l)
       case (0)
         ab(:,1)=es

       case (1)
         ABvec = self%a%pos - self%b%pos
         eadd  = n_comp_sum_((self%a%l-1))
         aub   = n_comp_sum_((self%l_sum-1)) - eadd

         call create_(momenta,3, n_comp_sum_(self%l_sum))
         tmp=0;    call make_gaussian_xyz_powers_(tmp,momenta,self%l_sum)
         call create_(index,0,self%l_sum, 0,self%l_sum, 0,self%l_sum)
         call make_index_of_components_(index,momenta)

         do b=1,3
           z   = index_of_first_nonzero_value_(momenta(:,b+1))
           ABi = ABvec(z)
           do a=1,aub
             a_momenta    = momenta(:, a + eadd)
             a_momenta(z) = a_momenta(z) + 1
             a1 = index(a_momenta(1), a_momenta(2), a_momenta(3)) - eadd
             ab(a,b)      = es(a1) + ABi * es(a)
           end do
         end do
         call destroy_(index)
         call destroy_(momenta)

       case default
         ABvec = self%a%pos - self%b%pos
         eadd  = n_comp_sum_((self%l_max-1))
         aub   = n_comp_sum_((self%l_sum-1)) - eadd

         call create_(momenta,3, n_comp_sum_(self%l_sum))
         tmp=0;    call make_gaussian_xyz_powers_(tmp,momenta,self%l_sum)
         call create_(index,0,self%l_sum, 0,self%l_sum, 0,self%l_sum)
         call make_index_of_components_(index,momenta)

         call create_(int_new,aub,3)
         do b=1,3
           z   = index_of_first_nonzero_value_(momenta(:,b+1))
           ABi = ABvec(z)
           do a=1,aub
             a_momenta    = momenta(:, a + eadd)
             a_momenta(z) = a_momenta(z) + 1
             a1 = index(a_momenta(1), a_momenta(2), a_momenta(3)) - eadd
             int_new(a,b) = es(a1) + ABi * es(a)
           end do
         end do

         do lb=2, self%b%l-1
           lb1_n_comp_sum = n_comp_sum_((lb-1))
           lb2_n_comp_sum = n_comp_sum_((lb-2))
           bub            = n_comp_sum_(lb) - lb1_n_comp_sum
           aub            = n_comp_sum_((self%l_sum-lb)) - eadd
           int_old        => int_new
           nullify(int_new)
           call create_(int_new,aub,bub)
           do b=1,bub
             b_momenta    = momenta(:, b + lb1_n_comp_sum)
             z            = index_of_first_nonzero_value_(b_momenta)
             b_momenta(z) = b_momenta(z) - 1
             b1 = index(b_momenta(1),b_momenta(2),b_momenta(3)) - lb2_n_comp_sum
             ABi          = ABvec(z)
             do a=1,aub
               a_momenta    = momenta(:, a + eadd)
               a_momenta(z) = a_momenta(z) + 1
               a1 = index(a_momenta(1),a_momenta(2),a_momenta(3)) - eadd
               int_new(a,b) = int_old(a1,b1) + ABi * int_old(a,b1)
             end do
           end do
           call destroy_(int_old)
         end do

         lb1_n_comp_sum = n_comp_sum_((self%b%l-1))
         lb2_n_comp_sum = n_comp_sum_((self%b%l-2))
         bub            = n_comp_sum_(self%b%l) - lb1_n_comp_sum
         aub            = n_comp_sum_((self%l_sum-self%b%l)) - eadd
         int_old        => int_new
         do b=1,bub
           b_momenta    = momenta(:, b + lb1_n_comp_sum)
           z            = index_of_first_nonzero_value_(b_momenta)
           b_momenta(z) = b_momenta(z) - 1
           b1 = index(b_momenta(1),b_momenta(2),b_momenta(3)) - lb2_n_comp_sum
           ABi          = ABvec(z)
           do a=1,aub
             a_momenta    = momenta(:, a + eadd)
             a_momenta(z) = a_momenta(z) + 1
             a1 = index(a_momenta(1),a_momenta(2),a_momenta(3)) - eadd
             ab(a,b)      = int_old(a1,b1) + ABi * int_old(a,b1)
           end do
         end do
         call destroy_(int_old)
         call destroy_(index)
         call destroy_(momenta)
     end select

   end subroutine

   subroutine transfer_l_b_highest(self,es,ab)
    type(shell2_type) :: self
    ! Applies the transfer equation to (e|s) to give (a|b)
    ! where b has the higher angular momentum.
     intent(in) :: self
     real(kind=kind(1.0d0)), dimension(:), intent(in) :: es
     real(kind=kind(1.0d0)), dimension(:,:), intent(out) :: ab
     real(kind=kind(1.0d0)), dimension(3) :: BAvec
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: int_new,int_old
     integer(kind=kind(1)), dimension(:,:), pointer :: momenta
     integer(kind=kind(1)), dimension(:,:,:), pointer :: index
     integer(kind=kind(1)), dimension(3) :: a_momenta,b_momenta
     integer(kind=kind(1)) :: a,b,la,aub,bub,la1_n_comp_sum,la2_n_comp_sum
     integer(kind=kind(1)) :: a1,b1,z,eadd,tmp
     real(kind=kind(1.0d0)) :: BAi

     select case (self%a%l)
       case (0)
         ab(1,:)=es

       case (1)
         BAvec = self%b%pos - self%a%pos
         eadd  = n_comp_sum_((self%b%l-1))
         bub   = n_comp_sum_((self%l_sum-1)) - eadd

         call create_(momenta,3, n_comp_sum_(self%l_sum))
         tmp=0;    call make_gaussian_xyz_powers_(tmp,momenta,self%l_sum)
         call create_(index,0,self%l_sum, 0,self%l_sum, 0,self%l_sum)
         call make_index_of_components_(index,momenta)

         do a=1,3
           z   = index_of_first_nonzero_value_(momenta(:,a+1))
           BAi = BAvec(z)
           do b=1, bub
             b_momenta    = momenta(:, b + eadd)
             b_momenta(z) = b_momenta(z) + 1
             b1 = index(b_momenta(1), b_momenta(2), b_momenta(3)) - eadd
             ab(a,b)      = es(b1) + BAi * es(b)
           end do
         end do
         call destroy_(index)
         call destroy_(momenta)

       case default
         BAvec = self%b%pos - self%a%pos
         eadd  = n_comp_sum_((self%b%l-1))
         bub   = n_comp_sum_((self%l_sum-1)) - eadd

         call create_(momenta,3, n_comp_sum_(self%l_sum))
         tmp=0;    call make_gaussian_xyz_powers_(tmp,momenta,self%l_sum)
         call create_(index,0,self%l_sum, 0,self%l_sum, 0,self%l_sum)
         call make_index_of_components_(index,momenta)

         call create_(int_new,3,bub)
         do a = 1, 3
           z   = index_of_first_nonzero_value_(momenta(:,a+1))
           BAi = BAvec(z)
           do b = 1, bub
             b_momenta    = momenta(:, b + eadd)
             b_momenta(z) = b_momenta(z) + 1
             b1 = index(b_momenta(1), b_momenta(2), b_momenta(3)) - eadd
             int_new(a,b) = es(b1) + BAi * es(b)
           end do
         end do

         do la=2, self%a%l - 1
           la1_n_comp_sum = n_comp_sum_((la-1))
           la2_n_comp_sum = n_comp_sum_((la-2))
           aub            = n_comp_sum_(la) - la1_n_comp_sum
           bub            = n_comp_sum_((self%l_sum-la)) - eadd
           int_old=>int_new
           nullify(int_new)
           call create_(int_new,aub,bub)
           do a=1,aub
             a_momenta    = momenta(:,a + la1_n_comp_sum)
             z            = index_of_first_nonzero_value_(a_momenta)
             a_momenta(z) = a_momenta(z) - 1
             a1 = index(a_momenta(1),a_momenta(2),a_momenta(3)) - la2_n_comp_sum
             BAi          = BAvec(z)
             do b=1,bub
               b_momenta    = momenta(:,b + eadd)
               b_momenta(z) = b_momenta(z) + 1
               b1 = index(b_momenta(1), b_momenta(2), b_momenta(3)) - eadd
               int_new(a,b) = int_old(a1,b1) + BAi * int_old(a1,b)
             end do
           end do
           call destroy_(int_old)
         end do

         la1_n_comp_sum = n_comp_sum_((self%a%l-1))
         la2_n_comp_sum = n_comp_sum_((self%a%l-2))
         aub            = n_comp_sum_(self%a%l) - la1_n_comp_sum
         bub            = n_comp_sum_((self%l_sum-self%a%l)) - eadd
         int_old=>int_new
         do a=1,aub
           a_momenta    = momenta(:,a + la1_n_comp_sum)
           z            = index_of_first_nonzero_value_(a_momenta)
           a_momenta(z) = a_momenta(z) - 1
           a1 = index(a_momenta(1),a_momenta(2),a_momenta(3)) - la2_n_comp_sum
           BAi          = BAvec(z)
           do b=1,bub
             b_momenta    = momenta(:,b + eadd)
             b_momenta(z) = b_momenta(z) + 1
             b1 = index(b_momenta(1), b_momenta(2), b_momenta(3)) - eadd
             ab(a,b)      = int_old(a1,b1) + BAi * int_old(a1,b)
           end do
         end do
         call destroy_(int_old)
         call destroy_(index)
         call destroy_(momenta)
     end select

   end subroutine

  ! Normalization routines and functions

   subroutine normalise(self,ab)
    type(shell2_type) :: self
    ! Multiply the matrix by the orbital normalisation coefficients
    ! for orbitals a and b.
     intent(in) :: self
     real(kind=kind(1.0d0)), dimension(:,:), intent(inout) :: ab
     integer(kind=kind(1)) :: a,b,i

     if (self%a%l>1 .or. self%b%l>1) then
       i = 0
       do b = 1, self%b%n_comp
         do a = 1, self%a%n_comp
           i = i + 1
           ab(a,b)=ab(a,b)*self%normalising_factors(i)
         end do
       end do
     end if

   end subroutine

   subroutine put(self)
    type(shell2_type) :: self
    ! Put the shell2 information on file "out"

     call flush_(stdout)
     call put_(stdout,"Shell a:",flush=1); call put_(self%a)
     call flush_(stdout)
     call put_(stdout,"Shell b:",flush=1); call put_(self%b)

   end subroutine

end
