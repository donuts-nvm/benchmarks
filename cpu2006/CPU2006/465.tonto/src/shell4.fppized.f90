!
! Copyright (C) Daniel Grimwood, March 1998
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
! $Id: shell4.foo,v 1.49.2.6 2003/11/13 05:33:21 reaper Exp $
!*******************************************************************************

module SHELL4_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

   use REALVEC_MODULE, only: normalising_factors_
   use REALVEC_MODULE, only: create_
   use REALVEC_MODULE, only: destroy_

   use RYS_MODULE, only: get_weights_
   use RYS_MODULE, only: create_
   use RYS_MODULE, only: destroy_

   use INT_MODULE, only: n_comp_
   use INT_MODULE, only: make_gaussian_xyz_indices_
   use INT_MODULE, only: make_gaussian_xyz_power_index_
   use INT_MODULE, only: make_gaussian_xyz_powers_
   use INT_MODULE, only: n_comp_sum_

   use GAUSSIAN4_MODULE, only: make_ERI_derivatives_
   use GAUSSIAN4_MODULE, only: make_spin_orbit_ints_
   use GAUSSIAN4_MODULE, only: make_ERI_ints_
   use GAUSSIAN4_MODULE, only: make_spin_spin_dipole_ints_

   use INTMAT_MODULE, only: create_
   use INTMAT_MODULE, only: destroy_

   use SHELL1_MODULE, only: set_
   use SHELL1_MODULE, only: nullify_ptr_part_
   use SHELL1_MODULE, only: destroy_ptr_part_
   use SHELL1_MODULE, only: unnormalise_
   use SHELL1_MODULE, only: copy_

   use REALMAT3_MODULE, only: create_
   use REALMAT3_MODULE, only: destroy_

   use INTMAT3_MODULE, only: create_
   use INTMAT3_MODULE, only: destroy_

   use REALMAT4_MODULE, only: create_
   use REALMAT4_MODULE, only: destroy_

   use INTVEC_MODULE, only: create_
   use INTVEC_MODULE, only: destroy_

   use TEXTFILE_MODULE, only: stdin
   use TEXTFILE_MODULE, only: stdout
   use TEXTFILE_MODULE, only: put_
   use TEXTFILE_MODULE, only: tab_
   use TEXTFILE_MODULE, only: flush_
   use TEXTFILE_MODULE, only: dash_
   use TEXTFILE_MODULE, only: show_

   use REALMAT5_MODULE, only: create_
   use REALMAT5_MODULE, only: destroy_

   use REALMAT_MODULE, only: create_
   use REALMAT_MODULE, only: destroy_
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

   public    copy_cd_
   interface copy_cd_
      module procedure copy_cd
   end interface

   public    make_r_JK_
   interface make_r_JK_
      module procedure make_r_JK
   end interface

   private    form_3d_ints_rm_
   interface form_3d_ints_rm_
      module procedure form_3d_ints_rm
   end interface

   public    nullify_ptr_part_
   interface nullify_ptr_part_
      module procedure nullify_ptr_part
   end interface

   private    add_to_component_
   interface add_to_component_
      module procedure add_to_component
   end interface

   public    cd_kappa_max_
   interface cd_kappa_max_
      module procedure cd_kappa_max
   end interface

   private    transfer_l_a_highest_
   interface transfer_l_a_highest_
      module procedure transfer_l_a_highest
      module procedure transfer_l_a_highest_1
   end interface

   public    destroy_ptr_part_
   interface destroy_ptr_part_
      module procedure destroy_ptr_part
   end interface

   public    to_normalise_
   interface to_normalise_
      module procedure to_normalise
      module procedure to_normalise_1
   end interface

   private    transfer_l_b_highest_
   interface transfer_l_b_highest_
      module procedure transfer_l_b_highest
      module procedure transfer_l_b_highest_1
   end interface

   private    form_3d_ints_
   interface form_3d_ints_
      module procedure form_3d_ints
   end interface

   public    unnormalise_
   interface unnormalise_
      module procedure unnormalise
   end interface

   public    destroy_ab_
   interface destroy_ab_
      module procedure destroy_ab
   end interface

   public    make_spin_spin_dipole_ints_
   interface make_spin_spin_dipole_ints_
      module procedure make_spin_spin_dipole_ints
   end interface

   public    put_
   interface put_
      module procedure put
   end interface

   public    transfer_ab_
   interface transfer_ab_
      module procedure transfer_ab
      module procedure transfer_ab_1
   end interface

   public    skip_ERI_
   interface skip_ERI_
      module procedure skip_ERI
      module procedure skip_ERI_1
   end interface

   public    ab_kappa_max_
   interface ab_kappa_max_
      module procedure ab_kappa_max
   end interface

   public    make_r_J_
   interface make_r_J_
      module procedure make_r_J
   end interface

   private    transfer_l_c_highest_
   interface transfer_l_c_highest_
      module procedure transfer_l_c_highest
   end interface

   public    destroy_cd_
   interface destroy_cd_
      module procedure destroy_cd
   end interface

   private    subtract_from_component_
   interface subtract_from_component_
      module procedure subtract_from_component
   end interface

   public    transfer_cd_
   interface transfer_cd_
      module procedure transfer_cd
   end interface

   public    get_ERI_
   interface get_ERI_
      module procedure get_ERI
   end interface

   public    copy_
   interface copy_
      module procedure copy
      module procedure copy_1
      module procedure copy_2
   end interface

   public    make_ERI_derivatives_
   interface make_ERI_derivatives_
      module procedure make_ERI_derivatives
   end interface

   public    set_
   interface set_
      module procedure set
      module procedure set_1
   end interface

   public    create_
   interface create_
      module procedure create
      module procedure create_1
      module procedure create_2
   end interface

   public    create_copy_
   interface create_copy_
      module procedure create_copy
   end interface

   private    transfer_l_d_highest_
   interface transfer_l_d_highest_
      module procedure transfer_l_d_highest
   end interface

   private    make_esfs_
   interface make_esfs_
      module procedure make_esfs
   end interface

   public    destroy_
   interface destroy_
      module procedure destroy
   end interface

   public    make_spin_orbit_ints_
   interface make_spin_orbit_ints_
      module procedure make_spin_orbit_ints
      module procedure make_spin_orbit_ints_1
   end interface

   public    copy_ab_
   interface copy_ab_
      module procedure copy_ab
   end interface

   private    form_3d_ints_no_rm_
   interface form_3d_ints_no_rm_
      module procedure form_3d_ints_no_rm
   end interface

   public    make_ERI_ints_
   interface make_ERI_ints_
      module procedure make_ERI_ints
   end interface

contains

!*******************************************************************************
!  Create/Destroy routines.
!*******************************************************************************

   subroutine create(self)
    type(shell4_type) :: self
    ! Create a shell4 object, but no its component shells.
     pointer :: self

     nullify(self)
     allocate(self)

     call nullify_ptr_part_(self)

   end subroutine

   subroutine create_1(self,shell_a,shell_b,shell_c,shell_d)
    type(shell4_type) :: self
    ! Create a shell4 object from copies of shell1s.
     pointer :: self
     type(shell1_type), intent(in) :: shell_a,shell_b,shell_c,shell_d

     call create_(self)
     call copy_(self,shell_a,shell_b,shell_c,shell_d)

   end subroutine

   subroutine create_2(self,shell_a,shell_b,shell_c,shell_d,pos_a,pos_b,pos_c,pos_d)
    type(shell4_type) :: self
    ! Create a shell4 object from copies of shells and their positions.
     pointer :: self
     type(shell_type), intent(in) :: shell_a,shell_b,shell_c,shell_d
     real(kind=kind(1.0d0)), dimension(3), intent(in) :: pos_a,pos_b,pos_c,pos_d

     call create_(self)
     call copy_(self,shell_a,shell_b,shell_c,shell_d,pos_a,pos_b,pos_c,pos_d)

   end subroutine

   subroutine destroy(self)
    type(shell4_type) :: self
    ! Destroy a shell4 object.
     pointer :: self

     if (associated(self)) then
       call destroy_ptr_part_(self)

       deallocate(self)
     end if

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

   subroutine nullify_ptr_part(self)
    type(shell4_type) :: self
    ! Nullify the pointer parts of self

     call nullify_ptr_part_(self%a)
     call nullify_ptr_part_(self%b)
     call nullify_ptr_part_(self%c)
     call nullify_ptr_part_(self%d)

   end subroutine

   subroutine destroy_ptr_part(self)
    type(shell4_type) :: self
    ! Destroy the pointer parts of self

     call destroy_ab_(self)
     call destroy_cd_(self)

   end subroutine

   subroutine destroy_ab(self)
    type(shell4_type) :: self
    ! Destroy the shella and shellb pointer parts of self

     call destroy_ptr_part_(self%a)
     call destroy_ptr_part_(self%b)

   end subroutine

   subroutine destroy_cd(self)
    type(shell4_type) :: self
    ! Destroy the shellc and shelld pointer parts of self

     call destroy_ptr_part_(self%c)
     call destroy_ptr_part_(self%d)

   end subroutine

   subroutine create_copy(self,shell)
    type(shell4_type) :: self
    ! Create a copy of "shell"
     pointer :: self
     type(shell4_type), intent(in) :: shell

     call create_(self)
     call copy_(self,shell)

   end subroutine

!*******************************************************************************
!  Setting parts of self from other shells and shell1s.
!*******************************************************************************

   subroutine copy(self,shell)
    type(shell4_type) :: self
    ! Make a copy of "shell"
     type(shell4_type), intent(in) :: shell

     call copy_(self%a,shell%a)
     call copy_(self%b,shell%b)
     call copy_(self%c,shell%c)
     call copy_(self%d,shell%d)

   end subroutine

   subroutine copy_1(self,shell_a,shell_b,shell_c,shell_d)
    type(shell4_type) :: self
    ! Copy the shell4 using from shell1 objects
     type(shell1_type), intent(in) :: shell_a,shell_b,shell_c,shell_d

     call copy_(self%a,shell_a)
     call copy_(self%b,shell_b)
     call copy_(self%c,shell_c)
     call copy_(self%d,shell_d)

   end subroutine

   subroutine copy_2(self,shell_a,shell_b,shell_c,shell_d,pos_a,pos_b,pos_c,pos_d)
    type(shell4_type) :: self
    ! Set the shell4 using shell objects and positions
     type(shell_type), intent(in) :: shell_a,shell_b,shell_c,shell_d
     real(kind=kind(1.0d0)), dimension(:), intent(in) :: pos_a,pos_b,pos_c,pos_d

     call copy_(self%a,shell_a,pos_a)
     call copy_(self%b,shell_b,pos_b)
     call copy_(self%c,shell_c,pos_c)
     call copy_(self%d,shell_d,pos_d)

   end subroutine

   subroutine set(self,shell_a,shell_b,shell_c,shell_d)
    type(shell4_type) :: self
    ! Set the shell4 using from shell1 objects
     type(shell1_type), intent(in) :: shell_a,shell_b,shell_c,shell_d

     call set_(self%a,shell_a)
     call set_(self%b,shell_b)
     call set_(self%c,shell_c)
     call set_(self%d,shell_d)

   end subroutine

   subroutine copy_ab(self,shell_a,shell_b,pos_a,pos_b)
    type(shell4_type) :: self
    ! Copy the a and b parts of the shell4 using from shell1 objects
     type(shell_type), intent(in) :: shell_a,shell_b
     real(kind=kind(1.0d0)), dimension(:), intent(in) :: pos_a,pos_b

     call copy_(self%a,shell_a,pos_a)
     call copy_(self%b,shell_b,pos_b)

   end subroutine

   subroutine copy_cd(self,shell_c,shell_d,pos_c,pos_d)
    type(shell4_type) :: self
    ! Copy the c and d parts of the shell4 using from shell1 objects
     type(shell_type), intent(in) :: shell_c,shell_d
     real(kind=kind(1.0d0)), dimension(:), intent(in) :: pos_c,pos_d

     call copy_(self%c,shell_c,pos_c)
     call copy_(self%d,shell_d,pos_d)

   end subroutine

   subroutine set_1(self,shell_a,shell_b,shell_c,shell_d,pos_a,pos_b,pos_c,pos_d)
    type(shell4_type) :: self
    ! Set the shell4 using shell objects and positions
     type(shell_type), intent(in) :: shell_a,shell_b,shell_c,shell_d
     real(kind=kind(1.0d0)), dimension(:), intent(in) :: pos_a,pos_b,pos_c,pos_d

     call set_(self%a,shell_a,pos_a)
     call set_(self%b,shell_b,pos_b)
     call set_(self%c,shell_c,pos_c)
     call set_(self%d,shell_d,pos_d)

   end subroutine

!*******************************************************************************
!  Miscellaneous Routines.
!*******************************************************************************

   subroutine unnormalise(self)
    type(shell4_type) :: self
    ! Unnormalise each shell in this shell quartet

     call unnormalise_(self%a)
     call unnormalise_(self%b)
     call unnormalise_(self%c)
     call unnormalise_(self%d)

   end subroutine

!*******************************************************************************
!  ERI cutoffs
!*******************************************************************************

  pure function ab_kappa_max(self) result(res)
    type(shell4_type) :: self
   ! Return the largest kappa_ab used in the Lindh integrals.
    intent(in) :: self
    real(kind=kind(1.0d0)) :: res
    real(kind=kind(1.0d0)), dimension(3) :: AB
    real(kind=kind(1.0d0)) :: b,b_cc,a,ab_inv,prefac,r2_ab
    integer(kind=kind(1)) :: bg,ag
    AB = self%b%pos - self%a%pos
    r2_ab = dot_product(AB,AB)
    res = 0.0d0
    do bg = 1,self%b%n_cc
      b      = self%b%ex(bg)
      b_cc   = self%b%cc(bg)
      do ag = 1,self%a%n_cc
        a = self%a%ex(ag)
        ab_inv = 1.0d0/(a+b)
        prefac = b_cc*self%a%cc(ag) * ab_inv * sqrt(ab_inv) * exp(-a*b*r2_ab*ab_inv)
        res = max(res,prefac/(sqrt(ab_inv)*ab_inv))
      end do
    end do

  end function

  pure function cd_kappa_max(self) result(res)
    type(shell4_type) :: self
   ! Return the largest kappa_cd used in the Lindh integrals.
    intent(in) :: self
    real(kind=kind(1.0d0)) :: res
    real(kind=kind(1.0d0)), dimension(3) :: CD
    real(kind=kind(1.0d0)) :: d,d_cc,c,cd_inv,prefac,r2_cd
    integer(kind=kind(1)) :: dg,cg
    CD = self%d%pos - self%c%pos
    r2_cd = dot_product(CD,CD)
    res = 0.0d0
    do dg = 1,self%d%n_cc
      d      = self%d%ex(dg)
      d_cc   = self%d%cc(dg)
      do cg = 1,self%c%n_cc
        c = self%c%ex(cg)
        cd_inv = 1.0d0/(c+d)
        prefac = d_cc*self%c%cc(cg) * cd_inv * sqrt(cd_inv) * exp(-c*d*r2_cd*cd_inv)
        res = max(res,prefac/(sqrt(cd_inv)*cd_inv))
      end do
    end do

  end function

   pure function skip_ERI(self) result(res)
    type(shell4_type) :: self
    ! Whether the ERI block will be less than a cutoff value.
     intent(in) :: self
     logical(kind=kind(.true.)) :: res
     res = (ab_kappa_max_(self)*cd_kappa_max_(self) < 1.0d-15)

   end function

   pure function skip_ERI_1(self,cutoff) result(res)
    type(shell4_type) :: self
    ! Whether the ERI block will be less than a cutoff value.
     intent(in) :: self
     real(kind=kind(1.0d0)), intent(in) :: cutoff
     logical(kind=kind(.true.)) :: res
     res = (ab_kappa_max_(self)*cd_kappa_max_(self) < cutoff)

   end function

!*******************************************************************************
!                            CADPAC-style integrals
!*******************************************************************************

  subroutine make_ERI_ints(self,I)
    type(shell4_type) :: self
   ! Make the ERI integral matrix, using Gauss-Hermite quadrature, like in
   ! CADPAC.
   ! This is not expected to be as efficient as Daniel's code, below!
   ! But probably much easier to understand.
    real(kind=kind(1.0d0)), dimension(:,:,:,:) :: I
    real(kind=kind(1.0d0)), dimension(:,:,:,:), pointer :: II
    type(gaussian4_type) :: G
    integer(kind=kind(1)) :: a,b,c,d

    I = 0.0d0
    call create_(II,self%a%n_comp,self%b%n_comp,self%c%n_comp,self%d%n_comp)
    G%a%l   = self%a%l;   G%b%l   = self%b%l;   G%c%l   = self%c%l;   G%d%l   = self%d%l
    G%a%pos = self%a%pos; G%b%pos = self%b%pos; G%c%pos = self%c%pos; G%d%pos = self%d%pos
    do a = 1,self%a%n_cc
      do b = 1,self%b%n_cc
        do c = 1,self%c%n_cc
          do d = 1,self%d%n_cc
            G%a%ex = self%a%ex(a)
            G%b%ex = self%b%ex(b)
            G%c%ex = self%c%ex(c)
            G%d%ex = self%d%ex(d)
            call make_ERI_ints_(G,II)
            I = I + II*self%a%cc(a)*self%b%cc(b)*self%c%cc(c)*self%d%cc(d)
          end do
        end do
      end do
    end do
    call destroy_(II)
    call to_normalise_(self,I)

  end subroutine

  subroutine make_spin_orbit_ints(self,S,O)
    type(shell4_type) :: self
   ! Make the spin same orbit integrals, "Sx", ... , and the
   ! spin other orbit integrals "Ox", ... , using
   ! Gauss-Hermite quadrature. Probably not the best implementation.
    real(kind=kind(1.0d0)), dimension(:,:,:,:,:) :: S,O

    call make_spin_orbit_ints_(self,S(:,:,:,:,1),S(:,:,:,:,2),S(:,:,:,:,3), &
                          O(:,:,:,:,1),O(:,:,:,:,2),O(:,:,:,:,3))

  end subroutine

  subroutine make_spin_orbit_ints_1(self,Sx,Sy,Sz,Ox,Oy,Oz)
    type(shell4_type) :: self
   ! Make the spin same orbit integrals, "Sx", ... , and the
   ! spin other orbit integrals "Ox", ... , using
   ! Gauss-Hermite quadrature. Probably not the best implementation.
    real(kind=kind(1.0d0)), dimension(:,:,:,:) :: Sx,Sy,Sz,Ox,Oy,Oz
    real(kind=kind(1.0d0)), dimension(:,:,:,:), pointer :: SSx,SSy,SSz,OOx,OOy,OOz
    type(gaussian4_type) :: G
    integer(kind=kind(1)) :: a,b,c,d,na,nb,nc,nd
    real(kind=kind(1.0d0)) :: cc

    Sx = 0.0d0; Sy = 0.0d0; Sz = 0.0d0
    Ox = 0.0d0; Oy = 0.0d0; Oz = 0.0d0
    na = self%a%n_comp; nb = self%b%n_comp; nc =self%c%n_comp; nd =self%d%n_comp
    call create_(SSx,na,nb,nc,nd); call create_(SSy,na,nb,nc,nd); call create_(SSz,na,nb,nc,nd)
    call create_(OOx,na,nb,nc,nd); call create_(OOy,na,nb,nc,nd); call create_(OOz,na,nb,nc,nd)
    G%a%l   = self%a%l;   G%b%l   = self%b%l;   G%c%l   = self%c%l;   G%d%l   = self%d%l
    G%a%pos = self%a%pos; G%b%pos = self%b%pos; G%c%pos = self%c%pos; G%d%pos = self%d%pos
    do a = 1,self%a%n_cc
      do b = 1,self%b%n_cc
        do c = 1,self%c%n_cc
          do d = 1,self%d%n_cc
            G%a%ex = self%a%ex(a)
            G%b%ex = self%b%ex(b)
            G%c%ex = self%c%ex(c)
            G%d%ex = self%d%ex(d)
            call make_spin_orbit_ints_(G,SSx,SSy,SSz,OOx,OOy,OOz)
            cc = self%a%cc(a)*self%b%cc(b)*self%c%cc(c)*self%d%cc(d)
            Sx = Sx + SSx*cc; Sy = Sy + SSy*cc; Sz = Sz + SSz*cc
            Ox = Ox + OOx*cc; Oy = Oy + OOy*cc; Oz = Oz + OOz*cc
          end do
        end do
      end do
    end do
    call destroy_(OOz); call destroy_(OOy); call destroy_(OOx)
    call destroy_(SSz); call destroy_(SSy); call destroy_(SSx)
    call to_normalise_(self,Sx); call to_normalise_(self,Sy); call to_normalise_(self,Sz)
    call to_normalise_(self,Ox); call to_normalise_(self,Oy); call to_normalise_(self,Oz)

  end subroutine

  subroutine make_spin_spin_dipole_ints(self,Dxx,Dyy,Dzz,Dxy,Dxz,Dyz)
    type(shell4_type) :: self
   ! Make the spin spin magnetic dipole integrals, "Dij"
   ! using Gauss-Hermite quadrature. For sure, not the
   ! best implementation, but where else will you get em', eh?
    real(kind=kind(1.0d0)), dimension(:,:,:,:) :: Dxx,Dyy,Dzz,Dxy,Dxz,Dyz
    real(kind=kind(1.0d0)), dimension(:,:,:,:), pointer :: Mxx,Myy,Mzz,Mxy,Mxz,Myz
    type(gaussian4_type) :: G
    integer(kind=kind(1)) :: a,b,c,d,na,nb,nc,nd
    real(kind=kind(1.0d0)) :: cc

    Dxx = 0.0d0; Dyy = 0.0d0; Dzz = 0.0d0
    Dxy = 0.0d0; Dxz = 0.0d0; Dyz = 0.0d0
    na = self%a%n_comp; nb = self%b%n_comp; nc =self%c%n_comp; nd =self%d%n_comp
    call create_(Mxx,na,nb,nc,nd); call create_(Myy,na,nb,nc,nd); call create_(Mzz,na,nb,nc,nd)
    call create_(Mxy,na,nb,nc,nd); call create_(Mxz,na,nb,nc,nd); call create_(Myz,na,nb,nc,nd)
    G%a%l   = self%a%l;   G%b%l   = self%b%l;   G%c%l   = self%c%l;   G%d%l   = self%d%l
    G%a%pos = self%a%pos; G%b%pos = self%b%pos; G%c%pos = self%c%pos; G%d%pos = self%d%pos
    do a = 1,self%a%n_cc
      do b = 1,self%b%n_cc
        do c = 1,self%c%n_cc
          do d = 1,self%d%n_cc
            G%a%ex = self%a%ex(a)
            G%b%ex = self%b%ex(b)
            G%c%ex = self%c%ex(c)
            G%d%ex = self%d%ex(d)
            call make_spin_spin_dipole_ints_(G,Mxx,Myy,Mzz,Mxy,Mxz,Myz)
            cc = self%a%cc(a)*self%b%cc(b)*self%c%cc(c)*self%d%cc(d)
            Dxx = Dxx + Mxx*cc; Dyy = Dyy + Myy*cc; Dzz = Dzz + Mzz*cc
            Dxy = Dxy + Mxy*cc; Dxz = Dxz + Mxz*cc; Dyz = Dyz + Myz*cc
          end do
        end do
      end do
    end do
    call destroy_(Myz); call destroy_(Mxz); call destroy_(Mxy)
    call destroy_(Mzz); call destroy_(Myy); call destroy_(Mxx)
    call to_normalise_(self,Dxx); call to_normalise_(self,Dyy); call to_normalise_(self,Dzz)
    call to_normalise_(self,Dxy); call to_normalise_(self,Dxz); call to_normalise_(self,Dyz)

  end subroutine

  subroutine make_ERI_derivatives(self,AA,BB,CC,DD)
    type(shell4_type) :: self
   !
    real(kind=kind(1.0d0)), dimension(:,:,:,:,:), optional :: AA,BB,CC,DD
    real(kind=kind(1.0d0)), dimension(:,:,:,:,:), pointer :: AX,BX,CX,DX
    type(gaussian4_type) :: G
    integer(kind=kind(1)) :: a,b,c,d, na,nb,nc,nd
    real(kind=kind(1.0d0)) :: fac

    G%a%l = self%a%l; G%b%l = self%b%l; G%c%l = self%c%l; G%d%l = self%d%l
    G%a%pos = self%a%pos; G%b%pos = self%b%pos; G%c%pos = self%c%pos; G%d%pos = self%d%pos
    na = self%a%n_comp; nb = self%b%n_comp; nc =self%c%n_comp; nd =self%d%n_comp
    if (present(AA)) then
      AA = 0.0d0; call create_(AX,na,nb,nc,nd,3)
    end if
    if (present(BB)) then
      BB = 0.0d0; call create_(BX,na,nb,nc,nd,3)
    end if
    if (present(CC)) then
      CC = 0.0d0; call create_(CX,na,nb,nc,nd,3)
    end if
    if (present(DD)) then
      DD = 0.0d0; call create_(DX,na,nb,nc,nd,3)
    end if
    do a = 1,self%a%n_cc
      do b = 1,self%b%n_cc
        do c = 1,self%c%n_cc
          do d = 1,self%d%n_cc
            G%a%ex = self%a%ex(a)
            G%b%ex = self%b%ex(b)
            G%c%ex = self%c%ex(c)
            G%d%ex = self%d%ex(d)
            call make_ERI_derivatives_(G,AA=AX,BB=BX,CC=CX,DD=DX)
            fac = self%a%cc(a)*self%b%cc(b)*self%c%cc(c)*self%d%cc(d)
            if (present(AA)) AA = AA + AX*fac
            if (present(BB)) BB = BB + BX*fac
            if (present(CC)) CC = CC + CX*fac
            if (present(DD)) DD = DD + DX*fac
          end do
        end do
      end do
    end do
    if (present(DD)) then
      call destroy_(DX); call to_normalise_(self,DD)
    end if
    if (present(CC)) then
      call destroy_(CX); call to_normalise_(self,CC)
    end if
    if (present(BB)) then
      call destroy_(BX); call to_normalise_(self,BB)
    end if
    if (present(AA)) then
      call destroy_(AX); call to_normalise_(self,AA)
    end if

  end subroutine

!*******************************************************************************
!                   Roland Lindh-style integrals
!
! Electron repulsion integrals from Lindh, Ryu and Liu,
! J. Chem. Phys 95(8) 1991, 5889-5897.
!
! See also:
! Obara and Saika, J. Chem. Phys. 84(7), 1986, 3963-3974.
! Head-Gordon and Pople, J. Chem. Phys. 89(9), 1988, 5777-5786.
!
!*******************************************************************************

  subroutine get_ERI(self,abcd)
    type(shell4_type) :: self
   ! Makes the (ab|cd) integrals, summed over the primitives
   ! (uses the transfer equation to make (ab|cd) from (es|fs))
    intent(in) :: self
    real(kind=kind(1.0d0)), dimension(:,:,:,:), intent(out) :: abcd
    real(kind=kind(1.0d0)), dimension(:,:,:), pointer :: escd
    real(kind=kind(1.0d0)), dimension(:,:), pointer :: esfs
    integer(kind=kind(1)) :: eub,fub,ab_l_max,cd_l_max

    nullify(esfs)
    nullify(escd)
    ab_l_max = max(self%a%l,self%b%l)-1
    cd_l_max = max(self%c%l,self%d%l)-1
    eub = n_comp_sum_((self%a%l+self%b%l)) - n_comp_sum_(ab_l_max)
    fub = n_comp_sum_((self%c%l+self%d%l)) - n_comp_sum_(cd_l_max)
    call create_(esfs,eub,fub)
    call make_esfs_(self,esfs)
    call create_(escd,eub, self%c%n_comp, self%d%n_comp)
    call transfer_cd_(self,esfs,escd)
    call transfer_ab_(self,escd,abcd)
    call destroy_(escd)
    call destroy_(esfs)
    call to_normalise_(self,abcd)

  end subroutine

  subroutine make_esfs(self,esfs)
    type(shell4_type) :: self
   ! Makes the initial (es|fs) integrals, summed over the primitives
    intent(in) :: self
    real(kind=kind(1.0d0)), dimension(:,:), intent(out) :: esfs
    real(kind=kind(1.0d0)), dimension(:,:,:), pointer :: Ixa,Iya,Iza
    type(rys_type), pointer :: rys1
    real(kind=kind(1.0d0)), dimension(:,:), pointer :: Ix,Iy,Iz
    real(kind=kind(1.0d0)), dimension(3) :: At,Ct,P,Q,PA,QC,QP,AB,CD
    real(kind=kind(1.0d0)) :: zeta,eta,xx,zinv,rho,einv,rho_zinv,rho_einv
    real(kind=kind(1.0d0)) :: ce,cf,bb,ce1,kappa_ab,kappa_cd,normab,norm,r2ab,r2cd
    real(kind=kind(1.0d0)) :: a_cc,b_cc,c_cc,d_cc,a_ex,b_ex,c_ex,d_ex
    real(kind=kind(1.0d0)) :: t2,t2_rz,t2_re,half_zinv,half_einv,f1_bb,f1_cf
    integer(kind=kind(1)) :: ag,bg,cg,dg,nroots,eub,fub,dim1,dim2
    integer(kind=kind(1)) :: e,f,e1,f1,fp1,ep1,n,i,n_sum,ab_l_sum,cd_l_sum,ab_l_max,cd_l_max

    ab_l_sum = self%a%l + self%b%l
    cd_l_sum = self%c%l + self%d%l
    ab_l_max = max(self%a%l,self%b%l)-1
    cd_l_max = max(self%c%l,self%d%l)-1
    eub = n_comp_sum_(ab_l_sum) - n_comp_sum_(ab_l_max)
    fub = n_comp_sum_(cd_l_sum) - n_comp_sum_(cd_l_max)
    dim1 = ab_l_sum + 1
    dim2 = cd_l_sum + 1
    nroots = (dim1+dim2)/2

     ! number of elements to sum over
    n_sum = nroots*self%a%n_cc*self%b%n_cc*self%c%n_cc*self%d%n_cc
    call create_(Ixa,n_sum,dim1,dim2)
    call create_(Iya,n_sum,dim1,dim2)
    call create_(Iza,n_sum,dim1,dim2)

    AB = self%a%pos-self%b%pos
    CD = self%c%pos-self%d%pos
    r2ab = dot_product(AB,AB)
    r2cd = dot_product(CD,CD)
     ! Want position of shell1 with higher angular momentum.
    if (self%a%l > self%b%l) then; At = self%a%pos
    else;                  At = self%b%pos
    end if
    if (self%c%l > self%d%l) then; Ct = self%c%pos
    else;                  Ct = self%d%pos
    end if

    call create_(rys1,nroots)

    i = 0
    do ag = 1, self%a%n_cc
      a_cc = self%a%cc(ag)
      a_ex = self%a%ex(ag)
      do bg = 1, self%b%n_cc
        b_cc = self%b%cc(bg)
        b_ex = self%b%ex(bg)
        zeta = a_ex + b_ex
        zinv = 1.0d0/zeta
        half_zinv = 0.50d0 * zinv
        kappa_ab = exp(-a_ex*b_ex*r2ab*zinv)
        normab = 34.98683665524973d0 * b_cc*a_cc * zinv * sqrt(zinv) * kappa_ab
        P = (b_ex*self%b%pos + a_ex*self%a%pos) * zinv
        PA = P - At
        do cg = 1, self%c%n_cc
          c_cc = self%c%cc(cg)
          c_ex = self%c%ex(cg)
          do dg = 1, self%d%n_cc
            d_cc = self%d%cc(dg)
            d_ex = self%d%ex(dg)
            eta = c_ex + d_ex
            einv = 1.0d0/eta
            half_einv = 0.50d0 * einv
            kappa_cd = exp(-c_ex*d_ex*r2cd*einv)
            norm = normab * d_cc*c_cc * einv * sqrt(einv) * kappa_cd
            Q = (d_ex*self%d%pos + c_ex*self%c%pos) * einv
            QC = Q - Ct
            rho  = zeta * eta / (zeta + eta)
            rho_zinv = rho * zinv
            rho_einv = rho * einv
            half_einv = 0.50d0 * einv
            QP = Q - P
            xx   = rho * dot_product(QP,QP)
            call get_weights_(rys1,xx)
            rys1%w(:) = rys1%w(:) * (norm * sqrt(rho))

             ! Now make the 2 dimensional integrals.
            do n=1,nroots
              i = i + 1
              Ix => Ixa(i,:,:)
              Iy => Iya(i,:,:)
              Iz => Iza(i,:,:)
              t2      = rys1%r(n)
              t2_rz   = t2 * rho_zinv
              t2_re   = t2 * rho_einv
              cf      = (1.0d0 - t2_re) * half_einv
              ce      = (1.0d0 - t2_rz) * half_zinv
              bb      = t2_rz * half_einv
              Ix(1,1) = 1.0d0
              Iy(1,1) = 1.0d0
              Iz(1,1) = 1.0d0
              if (ab_l_sum>0) then
                Ix(2,1) = PA(1) + t2_rz * QP(1)
                Iy(2,1) = PA(2) + t2_rz * QP(2)
                Iz(2,1) = PA(3) + t2_rz * QP(3)
                do e = 2, ab_l_sum
                  e1 = e - 1
                  ep1 = e + 1
                  ce1 = (e-1) * ce
                  Ix(ep1,1) = Ix(2,1) * Ix(e,1) + ce1 * Ix(e1,1)
                  Iy(ep1,1) = Iy(2,1) * Iy(e,1) + ce1 * Iy(e1,1)
                  Iz(ep1,1) = Iz(2,1) * Iz(e,1) + ce1 * Iz(e1,1)
                end do
              end if
              if (cd_l_sum>0) then
                Ix(1,2) = QC(1) - t2_re * QP(1)
                Iy(1,2) = QC(2) - t2_re * QP(2)
                Iz(1,2) = QC(3) - t2_re * QP(3)
                do f = 2,cd_l_sum
                  f1 = f - 1
                  fp1   = f + 1
                  f1_cf = (f-1)*cf
                  Ix(1,fp1) = Ix(1,2) * Ix(1,f) + f1_cf * Ix(1,f1)
                  Iy(1,fp1) = Iy(1,2) * Iy(1,f) + f1_cf * Iy(1,f1)
                  Iz(1,fp1) = Iz(1,2) * Iz(1,f) + f1_cf * Iz(1,f1)
                end do
                if (ab_l_sum>0) then
                  Ix(2,2) = Ix(1,2) * Ix(2,1) + bb
                  Iy(2,2) = Iy(1,2) * Iy(2,1) + bb
                  Iz(2,2) = Iz(1,2) * Iz(2,1) + bb
                  do f = 2,cd_l_sum
                    f1 = f - 1
                    fp1   = f + 1
                    f1_cf = (f-1)*cf
                    Ix(2,fp1) = Ix(1,2) * Ix(2,f) + f1_cf * Ix(2,f1) + bb * Ix(1,f)
                    Iy(2,fp1) = Iy(1,2) * Iy(2,f) + f1_cf * Iy(2,f1) + bb * Iy(1,f)
                    Iz(2,fp1) = Iz(1,2) * Iz(2,f) + f1_cf * Iz(2,f1) + bb * Iz(1,f)
                  end do
                end if
              end if
              do e = 2, ab_l_sum
                e1  = e - 1
                ep1 = e + 1
                ce1 = (e-1) * ce
                do f=2, cd_l_sum+1
                  f1 = f - 1
                  f1_bb = (f-1)*bb
                  Ix(ep1,f) = Ix(2,1)*Ix(e,f) + ce1*Ix(e1,f) + f1_bb*Ix(e,f1)
                  Iy(ep1,f) = Iy(2,1)*Iy(e,f) + ce1*Iy(e1,f) + f1_bb*Iy(e,f1)
                  Iz(ep1,f) = Iz(2,1)*Iz(e,f) + ce1*Iz(e1,f) + f1_bb*Iz(e,f1)
                end do
              end do
              Iz = Iz * rys1%w(n)
            end do
          end do
        end do
      end do
    end do

    call destroy_(rys1)

    call form_3d_ints_(self,Ixa,Iya,Iza,esfs,eub,fub,n_sum)

    call destroy_(Iza)
    call destroy_(Iya)
    call destroy_(Ixa)

  end subroutine

  subroutine form_3d_ints(self,Ix,Iy,Iz,esfs,eub,fub,n_sum)
    type(shell4_type) :: self
   ! Forms (es|fs) from the two dimensional integrals, summed over primitives.
   ! This is the main routine, all the others are specialised and may break if
   ! given the wrong shell4.
    intent(in) :: self
    real(kind=kind(1.0d0)), dimension(:,:,:), intent(in) :: Ix,Iy,Iz
    real(kind=kind(1.0d0)), dimension(:,:), intent(out) :: esfs
    integer(kind=kind(1)), intent(in) :: eub,fub,n_sum

    if (min(self%a%l,self%b%l) < 3 .and. min(self%c%l,self%d%l) < 3) then  !s or p or d
      call form_3d_ints_no_rm_(self,Ix,Iy,Iz,esfs,eub,fub)
    else
      call form_3d_ints_rm_(self,Ix,Iy,Iz,esfs,eub,fub,n_sum)
    end if

  end subroutine

  subroutine form_3d_ints_rm(self,Ix,Iy,Iz,esfs,eub,fub,n_sum)
    type(shell4_type) :: self
   ! Forms the three dimensional integrals from the two dimensional integrals,
   ! summed over primitives.
   ! This version uses the reduced multiplication scheme.
    intent(in) :: self
    real(kind=kind(1.0d0)), dimension(:,:,:), intent(in) :: Ix,Iy,Iz
    real(kind=kind(1.0d0)), dimension(:,:), intent(out) :: esfs
    integer(kind=kind(1)), intent(in) :: eub,fub,n_sum
    integer(kind=kind(1)), dimension(:), pointer :: e_x,e_y,e_z,f_x,f_y,f_z,ii_e_ivec,ii_f_ivec
    real(kind=kind(1.0d0)), dimension(:,:,:), pointer :: Ief
    integer(kind=kind(1)) :: e,f,zf,yf,ze,ye,iie,iif,m
    integer(kind=kind(1)) :: dime,dimf,dime1,dimf1,dime2,dimf2

    dime  = self%a%l+self%b%l+1
    dime1 = dime+1
    dime2 = 2*dime1+1
    dimf  = self%c%l+self%d%l+1
    dimf1 = dimf+1
    dimf2 = 2*dimf1+1

    call create_(e_x,eub);  call create_(e_y,eub);  call create_(e_z,eub)
    call create_(f_x,fub);  call create_(f_y,fub);  call create_(f_z,fub)
    m = max(self%a%l,self%b%l); call make_gaussian_xyz_indices_(m,e_x,e_y,e_z,self%a%l+self%b%l)
    m = max(self%c%l,self%d%l); call make_gaussian_xyz_indices_(m,f_x,f_y,f_z,self%c%l+self%d%l)

    call create_(ii_e_ivec,eub)
    do e=1,eub
      ze = e_z(e)
      ii_e_ivec(e) = -dime1 + ze*(dime2-ze)/2 + e_y(e)
    end do
    call create_(ii_f_ivec,fub)
    do f=1,fub
      zf = f_z(f)
      ii_f_ivec(f) = -dimf1 + zf*(dimf2-zf)/2 + f_y(f)
    end do

     ! Apply reduced multiplication scheme to Iy and Iz, store in triangle.
    call create_(Ief,n_sum,dime*dime1/2,dimf*dimf1/2)
    iif = 0
    do zf=1,dimf
      do yf=1,dimf1-zf
        iif = iif + 1
        iie = 0
        do ze=1,dime
          do ye=1,dime1-ze
            iie = iie + 1
            Ief(:,iie,iif) = Iy(:,ye,yf) * Iz(:,ze,zf)
          end do
        end do
      end do
    end do

     ! Now add in the Ix 2d integrals and sum over contractions and roots
    esfs = sum(Ix(:,e_x,f_x) * Ief(:,ii_e_ivec,ii_f_ivec),dim=1)
    call destroy_(Ief)

    call destroy_(ii_f_ivec)
    call destroy_(ii_e_ivec)
    call destroy_(f_z);  call destroy_(f_y);  call destroy_(f_x)
    call destroy_(e_z);  call destroy_(e_y);  call destroy_(e_x)

  end subroutine

  subroutine form_3d_ints_no_rm(self,Ix,Iy,Iz,esfs,eub,fub)
    type(shell4_type) :: self
   ! Forms the three dimensional integrals from the two dimensional integrals,
   ! summed over primitives.
   ! This version does not use the reduced multiplication scheme.
    intent(in) :: self
    real(kind=kind(1.0d0)), dimension(:,:,:), intent(in) :: Ix,Iy,Iz
    real(kind=kind(1.0d0)), dimension(:,:), intent(out) :: esfs
    integer(kind=kind(1)), intent(in) :: eub,fub
    integer(kind=kind(1)) :: e,f,m
    integer(kind=kind(1)), dimension(:), pointer :: e_x,e_y,e_z,f_x,f_y,f_z

    call create_(e_x,eub);  call create_(e_y,eub);  call create_(e_z,eub)
    call create_(f_x,fub);  call create_(f_y,fub);  call create_(f_z,fub)
    m = max(self%a%l,self%b%l); call make_gaussian_xyz_indices_(m,e_x,e_y,e_z,self%a%l+self%b%l)
    m = max(self%c%l,self%d%l); call make_gaussian_xyz_indices_(m,f_x,f_y,f_z,self%c%l+self%d%l)

     !esfs = sum(Ix(:,e_x,f_x) * Iy(:,e_y,f_y) * Iz(:,e_z,f_z),dim=1)
    do e=1,eub
      do f=1,fub
        esfs(e,f) = sum(Ix(:,e_x(e),f_x(f)) * Iy(:,e_y(e),f_y(f)) * Iz(:,e_z(e),f_z(f)))
      end do
    end do

    call destroy_(f_z);  call destroy_(f_y);  call destroy_(f_x)
    call destroy_(e_z);  call destroy_(e_y);  call destroy_(e_x)

  end subroutine

  subroutine transfer_cd(self,esfs,escd)
    type(shell4_type) :: self
   ! Applies the transfer equation to (es|fs) to give (es|cd)
    intent(in) :: self
    real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: esfs
    real(kind=kind(1.0d0)), dimension(:,:,:), intent(out) :: escd

    if (self%c%l > self%d%l) then
      call transfer_l_c_highest_(self,esfs,escd)
    else
      call transfer_l_d_highest_(self,esfs,escd)
    end if

  end subroutine

  subroutine transfer_ab(self,escd,abcd)
    type(shell4_type) :: self
   ! Applies the transfer equation to (es|cd) to give (ab|cd)
    intent(in) :: self
    real(kind=kind(1.0d0)), dimension(:,:,:), intent(in) :: escd
    real(kind=kind(1.0d0)), dimension(:,:,:,:), intent(out) :: abcd

    if (self%a%l > self%b%l) then
      call transfer_l_a_highest_(self,escd,abcd)
    else
      call transfer_l_b_highest_(self,escd,abcd)
    end if

  end subroutine

  subroutine transfer_ab_1(self,escd,abcd)
    type(shell4_type) :: self
   ! Applies the transfer equation to (es|cd) to give (ab|cd)
    intent(in) :: self
    real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: escd
    real(kind=kind(1.0d0)), dimension(:,:,:), intent(out) :: abcd

    if (self%a%l > self%b%l) then
      call transfer_l_a_highest_(self,escd,abcd)
    else
      call transfer_l_b_highest_(self,escd,abcd)
    end if

  end subroutine

   subroutine transfer_l_c_highest(self,esfs,escd)
    type(shell4_type) :: self
    ! Applies the transfer equation to (es|fs) to give (es|cd)
     intent(in) :: self
     real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: esfs
     real(kind=kind(1.0d0)), dimension(:,:,:), intent(out) :: escd
     real(kind=kind(1.0d0)), dimension(:,:,:), pointer :: int_new,int_old
     integer(kind=kind(1)), dimension(:,:), pointer :: components,components_c,components_d
     integer(kind=kind(1)), dimension(:,:,:), pointer :: index_c,index_d
     integer(kind=kind(1)), dimension(:), pointer :: comp_to_use,component_to_use
     real(kind=kind(1.0d0)), dimension(3) :: CD
     integer(kind=kind(1)) :: c,d,c1,c2,c3,d1,ld,cub,dub,ab_l_max
     integer(kind=kind(1)) :: cx,cy,cz,dx,dy,dz,j,clb,dlb,tmp,e,eub
     real(kind=kind(1.0d0)) :: CDi,CDx,CDy,CDz,esfs_ec

     select case (self%d%l)
       case (0)
         escd(:,:,1)=esfs

       case (1)
         ab_l_max = max(self%a%l,self%b%l)-1
         clb = n_comp_sum_((self%c%l-1))
         eub = n_comp_sum_((self%a%l+self%b%l)) - n_comp_sum_(ab_l_max)
         CD   = self%c%pos - self%d%pos
         cub  = self%c%n_comp

         call create_(components,3, n_comp_sum_((self%c%l+self%d%l)) - clb)
         call create_(index_c,0,(self%c%l+self%d%l),0,(self%c%l+self%d%l),0,(self%c%l+self%d%l))
         call make_gaussian_xyz_powers_(self%c%l,components,(self%c%l+self%d%l),index_c)

         CDx = CD(1); CDy = CD(2); CDz = CD(3)
         do c = 1, cub
           cx = components(1,c)
           cy = components(2,c)
           cz = components(3,c)
           c1 = index_c(cx+1,cy,cz)
           c2 = index_c(cx,cy+1,cz)
           c3 = index_c(cx,cy,cz+1)
           do e = 1, eub
             esfs_ec = esfs(e,c)
             escd(e,c,1) = esfs(e,c1) + CDx * esfs_ec
             escd(e,c,2) = esfs(e,c2) + CDy * esfs_ec
             escd(e,c,3) = esfs(e,c3) + CDz * esfs_ec
           end do
         end do
         call destroy_(components)
         call destroy_(index_c)

       case default
         ab_l_max = max(self%a%l,self%b%l)-1
         clb  = n_comp_sum_((self%c%l-1))
         eub  = n_comp_sum_((self%a%l+self%b%l)) - n_comp_sum_(ab_l_max)
         CD   = self%c%pos - self%d%pos
         cub  = n_comp_sum_(((self%c%l+self%d%l)-1)) - clb

         call create_(index_c,0,(self%c%l+self%d%l),0,(self%c%l+self%d%l),0,(self%c%l+self%d%l))
         call create_(index_d,0,(self%c%l+self%d%l),0,(self%c%l+self%d%l),0,(self%c%l+self%d%l))
         call create_(components,3, n_comp_sum_((self%c%l+self%d%l)))
         call create_(comp_to_use, n_comp_sum_((self%c%l+self%d%l)) )
         tmp=0; call make_gaussian_xyz_powers_(tmp,components,(self%c%l+self%d%l),index_d,comp_to_use)
         call make_gaussian_xyz_power_index_(self%c%l,index_c,(self%c%l+self%d%l))
         components_c => components(:,clb+1:)

         nullify(int_new)
         call create_(int_new,eub,cub,3)

         CDx = CD(1); CDy = CD(2); CDz = CD(3)
         do c = 1, cub
           cx = components_c(1,c)
           cy = components_c(2,c)
           cz = components_c(3,c)
           c1 = index_c(cx+1,cy,cz)
           c2 = index_c(cx,cy+1,cz)
           c3 = index_c(cx,cy,cz+1)
           do e = 1, eub
             esfs_ec = esfs(e,c)
             int_new(e,c,1) = esfs(e,c1) + CDx * esfs_ec
             int_new(e,c,2) = esfs(e,c2) + CDy * esfs_ec
             int_new(e,c,3) = esfs(e,c3) + CDz * esfs_ec
           end do
         end do

         do ld=2, self%d%l - 1
           dlb              = n_comp_sum_((ld-1))
           dub              = n_comp_(ld)
           cub              = n_comp_sum_(((self%c%l+self%d%l)-ld)) - clb
           component_to_use => comp_to_use(dlb+1:dlb+dub)
           components_d     => components(:,dlb+1:dlb+dub)
           int_old          => int_new
           nullify(int_new)
           call create_(int_new,eub,cub,dub)
           do d=1,dub
             dx = components_d(1,d)
             dy = components_d(2,d)
             dz = components_d(3,d)
             j=component_to_use(d)
             call subtract_from_component_(self,dx,dy,dz,j)
             d1 = index_d(dx,dy,dz)
             CDi=CD(j)
             do c=1,cub
               cx = components_c(1,c)
               cy = components_c(2,c)
               cz = components_c(3,c)
               call add_to_component_(self,cx,cy,cz,j)
               c1 = index_c(cx,cy,cz)
               int_new(:,c,d)=int_old(:,c1,d1)+CDi*int_old(:,c,d1)
             end do
           end do
           call destroy_(int_old)
         end do

         dlb              = n_comp_sum_((self%d%l-1))
         dub              = self%d%n_comp
         cub              = self%c%n_comp
         component_to_use => comp_to_use(dlb+1:dlb+dub)
         components_d     => components(:,dlb+1:dlb+dub)
         int_old          => int_new
         do d=1,dub
           dx = components_d(1,d)
           dy = components_d(2,d)
           dz = components_d(3,d)
           j=component_to_use(d)
           call subtract_from_component_(self,dx,dy,dz,j)
           d1 = index_d(dx,dy,dz)
           CDi=CD(j)
           do c=1,cub
             cx = components_c(1,c)
             cy = components_c(2,c)
             cz = components_c(3,c)
             call add_to_component_(self,cx,cy,cz,j)
             c1 = index_c(cx,cy,cz)
             escd(:,c,d)=int_old(:,c1,d1)+CDi*int_old(:,c,d1)
           end do
         end do
         call destroy_(int_old)
         call destroy_(index_c)
         call destroy_(index_d)
         call destroy_(components)
         call destroy_(comp_to_use)
     end select

   end subroutine

   subroutine transfer_l_d_highest(self,esfs,escd)
    type(shell4_type) :: self
    ! Applies the transfer equation to (es|fs) to give (es|cd)
     intent(in) :: self
     real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: esfs
     real(kind=kind(1.0d0)), dimension(:,:,:), intent(out) :: escd
     real(kind=kind(1.0d0)), dimension(:,:,:), pointer :: int_new,int_old
     integer(kind=kind(1)), dimension(:,:), pointer :: components,components_c,components_d
     integer(kind=kind(1)), dimension(:,:,:), pointer :: index_c,index_d
     integer(kind=kind(1)), dimension(:), pointer :: comp_to_use,component_to_use
     real(kind=kind(1.0d0)), dimension(3) :: DC
     integer(kind=kind(1)) :: c,d,c1,d1,d2,d3,lc,cub,dub,ab_l_max
     integer(kind=kind(1)) :: cx,cy,cz,dx,dy,dz,j,clb,dlb,tmp,eub
     real(kind=kind(1.0d0)) :: DCi,DCx,DCy,DCz

     select case (self%c%l)
       case (0)
         escd(:,1,:)=esfs

       case (1)
         ab_l_max = max(self%a%l,self%b%l)-1
         dlb = n_comp_sum_((self%d%l-1))
         eub = n_comp_sum_((self%a%l+self%b%l)) - n_comp_sum_(ab_l_max)
         DC   = self%d%pos - self%c%pos
         dub  = self%d%n_comp

         call create_(components,3, n_comp_sum_((self%c%l+self%d%l)) - dlb)
         call create_(index_d,0,(self%c%l+self%d%l),0,(self%c%l+self%d%l),0,(self%c%l+self%d%l))
         call make_gaussian_xyz_powers_(self%d%l,components,(self%c%l+self%d%l),index_d)

         DCx=DC(1); DCy=DC(2); DCz=DC(3)
         do d=1,dub
           dx = components(1,d)
           dy = components(2,d)
           dz = components(3,d)
           d1 = index_d(dx+1,dy,dz)
           d2 = index_d(dx,dy+1,dz)
           d3 = index_d(dx,dy,dz+1)
             escd(:,1,d)=esfs(:,d1)+DCx*esfs(:,d)
             escd(:,2,d)=esfs(:,d2)+DCy*esfs(:,d)
             escd(:,3,d)=esfs(:,d3)+DCz*esfs(:,d)
         end do
         call destroy_(components)
         call destroy_(index_d)

       case default
         ab_l_max = max(self%a%l,self%b%l)-1
         dlb = n_comp_sum_((self%d%l-1))
         eub = n_comp_sum_((self%a%l+self%b%l)) - n_comp_sum_(ab_l_max)
         DC   = self%d%pos - self%c%pos
         dub  = n_comp_sum_((self%c%l+self%d%l-1)) - dlb

         call create_(index_c,0,(self%c%l+self%d%l),0,(self%c%l+self%d%l),0,(self%c%l+self%d%l))
         call create_(index_d,0,(self%c%l+self%d%l),0,(self%c%l+self%d%l),0,(self%c%l+self%d%l))
         call create_(components,3, n_comp_sum_((self%c%l+self%d%l)))
         call create_(comp_to_use, n_comp_sum_((self%c%l+self%d%l)) )
         tmp=0; call make_gaussian_xyz_powers_(tmp,components,(self%c%l+self%d%l),index_c,comp_to_use)
         call make_gaussian_xyz_power_index_(self%d%l,index_d,(self%c%l+self%d%l))
         components_d => components(:,dlb+1:)

         nullify(int_new)
         call create_(int_new,eub,dub,3)
         DCx=DC(1); DCy=DC(2); DCz=DC(3)
         do d=1,dub
           dx = components_d(1,d)
           dy = components_d(2,d)
           dz = components_d(3,d)
           d1 = index_d(dx+1,dy,dz)
           d2 = index_d(dx,dy+1,dz)
           d3 = index_d(dx,dy,dz+1)
             int_new(:,d,1)=esfs(:,d1)+DCx*esfs(:,d)
             int_new(:,d,2)=esfs(:,d2)+DCy*esfs(:,d)
             int_new(:,d,3)=esfs(:,d3)+DCz*esfs(:,d)
         end do
         do lc=2, self%c%l - 1
           clb              = n_comp_sum_((lc-1))
           cub              = n_comp_(lc)
           dub              = n_comp_sum_(((self%c%l+self%d%l)-lc)) - dlb
           component_to_use => comp_to_use(clb+1:clb+cub)
           components_c     => components(:,clb+1:clb+cub)
           int_old          => int_new
           nullify(int_new)
           call create_(int_new,eub,dub,cub)
           do c=1,cub
             cx = components_c(1,c)
             cy = components_c(2,c)
             cz = components_c(3,c)
             j=component_to_use(c)
             call subtract_from_component_(self,cx,cy,cz,j)
             c1 = index_c(cx,cy,cz)
             DCi=DC(j)
             do d=1,dub
               dx = components_d(1,d)
               dy = components_d(2,d)
               dz = components_d(3,d)
               call add_to_component_(self,dx,dy,dz,j)
               d1 = index_d(dx,dy,dz)
               int_new(:,d,c)=int_old(:,d1,c1)+DCi*int_old(:,d,c1)
             end do
           end do
           call destroy_(int_old)
         end do
         clb              = n_comp_sum_((self%c%l-1))
         cub              = self%c%n_comp
         dub              = self%d%n_comp
         component_to_use => comp_to_use(clb+1:clb+cub)
         components_c     => components(:,clb+1:clb+cub)
         int_old          => int_new
         do c=1,cub
           cx = components_c(1,c)
           cy = components_c(2,c)
           cz = components_c(3,c)
           j=component_to_use(c)
           call subtract_from_component_(self,cx,cy,cz,j)
           c1 = index_c(cx,cy,cz)
           DCi=DC(j)
           do d=1,dub
             dx = components_d(1,d)
             dy = components_d(2,d)
             dz = components_d(3,d)
             call add_to_component_(self,dx,dy,dz,j)
             d1 = index_d(dx,dy,dz)
             escd(:,c,d)=int_old(:,d1,c1)+DCi*int_old(:,d,c1)
           end do
         end do
         call destroy_(int_old)
         call destroy_(index_c)
         call destroy_(index_d)
         call destroy_(components)
         call destroy_(comp_to_use)
     end select

   end subroutine

   subroutine transfer_l_a_highest(self,escd,abcd)
    type(shell4_type) :: self
    ! Applies the transfer equation to (es|cd) to give (ab|cd)
     intent(in) :: self
     real(kind=kind(1.0d0)), dimension(:,:,:), intent(in) :: escd
     real(kind=kind(1.0d0)), dimension(:,:,:,:), intent(out) :: abcd
     real(kind=kind(1.0d0)), dimension(:,:,:,:), pointer :: int_new,int_old
     integer(kind=kind(1)), dimension(:,:), pointer :: components,components_a,components_b
     integer(kind=kind(1)), dimension(:,:,:), pointer :: index_a,index_b
     real(kind=kind(1.0d0)), dimension(3) :: AB
     integer(kind=kind(1)) :: a,b,c,d,a1,a2,a3,b1,lb,aub,bub,cub,dub
     integer(kind=kind(1)) :: ax,ay,az,bx,by,bz,j,alb,blb,tmp
     real(kind=kind(1.0d0)) :: ABi,ABx,ABy,ABz,escd_acd
     integer(kind=kind(1)), dimension(:), pointer :: comp_to_use,component_to_use

     select case (self%b%l)
       case (0)
         abcd(:,1,:,:)=escd

       case (1)
         alb = n_comp_sum_((self%a%l-1))
         AB   = self%a%pos - self%b%pos
         aub  = self%a%n_comp
         cub  = self%c%n_comp
         dub  = self%d%n_comp

         call create_(components,3, n_comp_sum_((self%a%l+self%b%l)) - alb)
         call create_(index_a,0,(self%a%l+self%b%l),0,(self%a%l+self%b%l),0,(self%a%l+self%b%l))
         call make_gaussian_xyz_powers_(self%a%l,components,(self%a%l+self%b%l),index_a)

         ABx=AB(1); ABy=AB(2); ABz=AB(3)
         do a=1,aub
           ax = components(1,a)
           ay = components(2,a)
           az = components(3,a)
           a1 = index_a(ax+1,ay,az)
           a2 = index_a(ax,ay+1,az)
           a3 = index_a(ax,ay,az+1)
           do d=1,dub
             do c=1,cub
               escd_acd = escd(a,c,d)
               abcd(a,1,c,d)=escd(a1,c,d) + ABx * escd_acd
               abcd(a,2,c,d)=escd(a2,c,d) + ABy * escd_acd
               abcd(a,3,c,d)=escd(a3,c,d) + ABz * escd_acd
             end do
           end do
         end do
         call destroy_(index_a)
         call destroy_(components)

       case default
         alb = n_comp_sum_((self%a%l-1))
         AB   = self%a%pos - self%b%pos
         aub  = n_comp_sum_(((self%a%l+self%b%l)-1)) - alb
         bub  = self%b%n_comp
         cub  = self%c%n_comp
         dub  = self%d%n_comp

         call create_(index_b,0,(self%a%l+self%b%l),0,(self%a%l+self%b%l),0,(self%a%l+self%b%l))
         call create_(index_a,0,(self%a%l+self%b%l),0,(self%a%l+self%b%l),0,(self%a%l+self%b%l))
         call create_(components,3, n_comp_sum_((self%a%l+self%b%l)))
         call create_(comp_to_use, n_comp_sum_((self%a%l+self%b%l)) )
         tmp=0; call make_gaussian_xyz_powers_(tmp,components,(self%a%l+self%b%l),index_b,comp_to_use)
         tmp=self%a%l; call make_gaussian_xyz_power_index_(tmp,index_a,(self%a%l+self%b%l))
         components_a => components(:,alb+1:)

         nullify(int_new)
         call create_(int_new,cub,dub,bub,aub)
         ABx=AB(1); ABy=AB(2); ABz=AB(3)
         do a=1,aub
           ax = components_a(1,a)
           ay = components_a(2,a)
           az = components_a(3,a)
           a1 = index_a(ax+1,ay,az)
           a2 = index_a(ax,ay+1,az)
           a3 = index_a(ax,ay,az+1)
           do d=1,dub
             do c=1,cub
               escd_acd = escd(a,c,d)
               int_new(c,d,1,a)=escd(a1,c,d) + ABx * escd_acd
               int_new(c,d,2,a)=escd(a2,c,d) + ABy * escd_acd
               int_new(c,d,3,a)=escd(a3,c,d) + ABz * escd_acd
             end do
           end do
         end do

         do lb=2, self%b%l - 1
           blb              = n_comp_sum_((lb-1))
           bub              = n_comp_(lb)
           aub              = n_comp_sum_(((self%a%l+self%b%l)-lb)) - alb
           component_to_use => comp_to_use(blb+1:blb+bub)
           components_b     => components(:,blb+1:blb+bub)
           int_old          => int_new
           nullify(int_new)
           call create_(int_new,cub,dub,bub,aub)
           do b=1,bub
             bx = components_b(1,b)
             by = components_b(2,b)
             bz = components_b(3,b)
             j = component_to_use(b)
             call subtract_from_component_(self,bx,by,bz,j)
             b1 = index_b(bx,by,bz)
             ABi=AB(j)
             do a=1,aub
               ax = components_a(1,a)
               ay = components_a(2,a)
               az = components_a(3,a)
               call add_to_component_(self,ax,ay,az,j)
               a1 = index_a(ax,ay,az)
               int_new(:,:,b,a)=int_old(:,:,b1,a1) + ABi * int_old(:,:,b1,a)
             end do
           end do
           call destroy_(int_old)
         end do

         blb              = n_comp_sum_((self%b%l-1))
         bub              = self%b%n_comp
         aub              = self%a%n_comp
         component_to_use => comp_to_use(blb+1:blb+bub)
         components_b     => components(:,blb+1:blb+bub)
         int_old          => int_new
         do b=1,bub
           bx = components_b(1,b)
           by = components_b(2,b)
           bz = components_b(3,b)
           j = component_to_use(b)
           call subtract_from_component_(self,bx,by,bz,j)
           b1 = index_b(bx,by,bz)
           ABi=AB(j)
           do a=1,aub
             ax = components_a(1,a)
             ay = components_a(2,a)
             az = components_a(3,a)
             call add_to_component_(self,ax,ay,az,j)
             a1 = index_a(ax,ay,az)
             abcd(a,b,:,:)=int_old(:,:,b1,a1) + ABi * int_old(:,:,b1,a)
           end do
         end do
         call destroy_(int_old)
         call destroy_(comp_to_use)
         call destroy_(components)
         call destroy_(index_a)
         call destroy_(index_b)
     end select

   end subroutine

   subroutine transfer_l_b_highest(self,escd,abcd)
    type(shell4_type) :: self
    ! Applies the transfer equation to (es|cd) to give (ab|cd)
     intent(in) :: self
     real(kind=kind(1.0d0)), dimension(:,:,:), intent(in) :: escd
     real(kind=kind(1.0d0)), dimension(:,:,:,:), intent(out) :: abcd
     real(kind=kind(1.0d0)), dimension(:,:,:,:), pointer :: int_new,int_old
     integer(kind=kind(1)), dimension(:,:), pointer :: components,components_a,components_b
     integer(kind=kind(1)), dimension(:,:,:), pointer :: index_a,index_b
     real(kind=kind(1.0d0)), dimension(3) :: BA
     integer(kind=kind(1)) :: a,b,c,d,a1,b1,b2,b3,la,aub,bub,cub,dub
     integer(kind=kind(1)) :: ax,ay,az,bx,by,bz,j,alb,blb,tmp
     real(kind=kind(1.0d0)) :: BAi,BAx,BAy,BAz,escd_bcd
     integer(kind=kind(1)), dimension(:), pointer :: comp_to_use,component_to_use

     select case (self%a%l)
       case (0)
         abcd(1,:,:,:)=escd

       case (1)
         blb = n_comp_sum_((self%b%l-1))
         BA   = self%b%pos - self%a%pos
         bub  = self%b%n_comp
         cub  = self%c%n_comp
         dub  = self%d%n_comp

         call create_(components,3, n_comp_sum_((self%a%l+self%b%l)) - blb)
         call create_(index_b,0,(self%a%l+self%b%l),0,(self%a%l+self%b%l),0,(self%a%l+self%b%l))
         call make_gaussian_xyz_powers_(self%b%l,components,(self%a%l+self%b%l),index_b)

         BAx = BA(1);    BAy = BA(2);    BAz = BA(3)
         do b = 1, bub
           bx = components(1,b)
           by = components(2,b)
           bz = components(3,b)
           b1 = index_b(bx+1,by,bz)
           b2 = index_b(bx,by+1,bz)
           b3 = index_b(bx,by,bz+1)
           do d = 1, dub
             do c = 1, cub
               escd_bcd = escd(b,c,d)
               abcd(1,b,c,d) = escd(b1,c,d) + BAx * escd_bcd
               abcd(2,b,c,d) = escd(b2,c,d) + BAy * escd_bcd
               abcd(3,b,c,d) = escd(b3,c,d) + BAz * escd_bcd
             end do
           end do
         end do

         call destroy_(components)
         call destroy_(index_b)

       case default
         blb = n_comp_sum_((self%b%l-1))
         BA   = self%b%pos - self%a%pos
         bub  = n_comp_sum_(((self%a%l+self%b%l)-1)) - blb
         aub  = self%a%n_comp
         cub  = self%c%n_comp
         dub  = self%d%n_comp

         call create_(index_a,0,(self%a%l+self%b%l),0,(self%a%l+self%b%l),0,(self%a%l+self%b%l))
         call create_(index_b,0,(self%a%l+self%b%l),0,(self%a%l+self%b%l),0,(self%a%l+self%b%l))
         call create_(components,3, n_comp_sum_((self%a%l+self%b%l)))
         call create_(comp_to_use, n_comp_sum_((self%a%l+self%b%l)) )
         tmp=0; call make_gaussian_xyz_powers_(tmp,components,(self%a%l+self%b%l),index_a,comp_to_use)
         tmp=self%b%l; call make_gaussian_xyz_power_index_(tmp,index_b,(self%a%l+self%b%l))
         components_b => components(:,blb+1:)

         nullify(int_new)
         call create_(int_new,cub,dub,bub,aub)

         BAx = BA(1);    BAy = BA(2);    BAz = BA(3)
         do b = 1, bub
           bx = components_b(1,b)
           by = components_b(2,b)
           bz = components_b(3,b)
           b1 = index_b(bx+1,by,bz)
           b2 = index_b(bx,by+1,bz)
           b3 = index_b(bx,by,bz+1)
           do c = 1, cub
             do d = 1, dub
               escd_bcd = escd(b,c,d)
               int_new(c,d,b,1) = escd(b1,c,d) + BAx * escd_bcd
               int_new(c,d,b,2) = escd(b2,c,d) + BAy * escd_bcd
               int_new(c,d,b,3) = escd(b3,c,d) + BAz * escd_bcd
             end do
           end do
         end do

         do la=2, self%a%l - 1
           alb              = n_comp_sum_((la-1))
           aub              = n_comp_(la)
           bub              = n_comp_sum_(((self%a%l+self%b%l)-la)) - blb
           component_to_use => comp_to_use(alb+1:alb+aub)
           components_a     => components(:,alb+1:alb+aub)
           int_old          => int_new
           nullify(int_new)
           call create_(int_new,cub,dub,bub,aub)
           do a=1,aub
             ax = components_a(1,a)
             ay = components_a(2,a)
             az = components_a(3,a)
             j=component_to_use(a)
             call subtract_from_component_(self,ax,ay,az,j)
             a1 = index_a(ax,ay,az)
             BAi=BA(j)
             do b=1,bub
               bx = components_b(1,b)
               by = components_b(2,b)
               bz = components_b(3,b)
               call add_to_component_(self,bx,by,bz,j)
               b1 = index_b(bx,by,bz)
               int_new(:,:,b,a)=int_old(:,:,b1,a1) + BAi * int_old(:,:,b,a1)
             end do
           end do
           call destroy_(int_old)
         end do

         alb              = n_comp_sum_((self%a%l-1))
         aub              = self%a%n_comp
         bub              = self%b%n_comp
         component_to_use => comp_to_use(alb+1:alb+aub)
         components_a     => components(:,alb+1:alb+aub)
         int_old          => int_new
         do a=1,aub
           ax = components_a(1,a)
           ay = components_a(2,a)
           az = components_a(3,a)
           j = component_to_use(a)
           call subtract_from_component_(self,ax,ay,az,j)
           a1 = index_a(ax,ay,az)
           BAi=BA(j)
           do b=1,bub
             bx = components_b(1,b)
             by = components_b(2,b)
             bz = components_b(3,b)
             call add_to_component_(self,bx,by,bz,j)
             b1 = index_b(bx,by,bz)
             abcd(a,b,:,:)=int_old(:,:,b1,a1) + BAi * int_old(:,:,b,a1)
           end do
         end do
         call destroy_(int_old)
         call destroy_(comp_to_use)
         call destroy_(components)
         call destroy_(index_b)
         call destroy_(index_a)
     end select

   end subroutine

   subroutine transfer_l_a_highest_1(self,escd,abcd)
    type(shell4_type) :: self
    ! Applies the transfer equation to (es|cd) to give (ab|cd)
     intent(in) :: self
     real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: escd
     real(kind=kind(1.0d0)), dimension(:,:,:), intent(out) :: abcd
     real(kind=kind(1.0d0)), dimension(:,:,:), pointer :: int_new,int_old
     integer(kind=kind(1)), dimension(:,:), pointer :: components,components_a,components_b
     integer(kind=kind(1)), dimension(:,:,:), pointer :: index_a,index_b
     real(kind=kind(1.0d0)), dimension(3) :: AB
     integer(kind=kind(1)) :: a,b,f,a1,a2,a3,b1,lb,aub,bub,fub
     integer(kind=kind(1)) :: ax,ay,az,bx,by,bz,j,alb,blb,tmp
     real(kind=kind(1.0d0)) :: ABi,ABx,ABy,ABz,escd_acd
     integer(kind=kind(1)), dimension(:), pointer :: comp_to_use,component_to_use

     select case (self%b%l)
       case (0)
         abcd(:,1,:)=escd

       case (1)
         alb = n_comp_sum_((self%a%l-1))
         AB   = self%a%pos - self%b%pos
         aub  = self%a%n_comp
         fub  = size(escd,2)

         call create_(components,3, n_comp_sum_((self%a%l+self%b%l)) - alb)
         call create_(index_a,0,(self%a%l+self%b%l),0,(self%a%l+self%b%l),0,(self%a%l+self%b%l))
         call make_gaussian_xyz_powers_(self%a%l,components,(self%a%l+self%b%l),index_a)

         ABx=AB(1); ABy=AB(2); ABz=AB(3)
         do a=1,aub
           ax = components(1,a)
           ay = components(2,a)
           az = components(3,a)
           a1 = index_a(ax+1,ay,az)
           a2 = index_a(ax,ay+1,az)
           a3 = index_a(ax,ay,az+1)
           do f=1,fub
             escd_acd = escd(a,f)
             abcd(a,1,f)=escd(a1,f) + ABx * escd_acd
             abcd(a,2,f)=escd(a2,f) + ABy * escd_acd
             abcd(a,3,f)=escd(a3,f) + ABz * escd_acd
           end do
         end do
         call destroy_(index_a)
         call destroy_(components)

       case default
         alb = n_comp_sum_((self%a%l-1))
         AB   = self%a%pos - self%b%pos
         aub  = n_comp_sum_(((self%a%l+self%b%l)-1)) - alb
         bub  = self%b%n_comp
         fub  = size(escd,2)

         call create_(index_b,0,(self%a%l+self%b%l),0,(self%a%l+self%b%l),0,(self%a%l+self%b%l))
         call create_(index_a,0,(self%a%l+self%b%l),0,(self%a%l+self%b%l),0,(self%a%l+self%b%l))
         call create_(components,3, n_comp_sum_((self%a%l+self%b%l)))
         call create_(comp_to_use, n_comp_sum_((self%a%l+self%b%l)) )
         tmp=0; call make_gaussian_xyz_powers_(tmp,components,(self%a%l+self%b%l),index_b,comp_to_use)
         tmp=self%a%l; call make_gaussian_xyz_power_index_(tmp,index_a,(self%a%l+self%b%l))
         components_a => components(:,alb+1:)

         nullify(int_new)
         call create_(int_new,fub,bub,aub)
         ABx=AB(1); ABy=AB(2); ABz=AB(3)
         do a=1,aub
           ax = components_a(1,a)
           ay = components_a(2,a)
           az = components_a(3,a)
           a1 = index_a(ax+1,ay,az)
           a2 = index_a(ax,ay+1,az)
           a3 = index_a(ax,ay,az+1)
           do f=1,fub
             escd_acd = escd(a,f)
             int_new(f,1,a)=escd(a1,f) + ABx * escd_acd
             int_new(f,2,a)=escd(a2,f) + ABy * escd_acd
             int_new(f,3,a)=escd(a3,f) + ABz * escd_acd
           end do
         end do

         do lb=2, self%b%l - 1
           blb              = n_comp_sum_((lb-1))
           bub              = n_comp_(lb)
           aub              = n_comp_sum_(((self%a%l+self%b%l)-lb)) - alb
           component_to_use => comp_to_use(blb+1:blb+bub)
           components_b     => components(:,blb+1:blb+bub)
           int_old          => int_new
           nullify(int_new)
           call create_(int_new,fub,bub,aub)
           do b=1,bub
             bx = components_b(1,b)
             by = components_b(2,b)
             bz = components_b(3,b)
             j = component_to_use(b)
             call subtract_from_component_(self,bx,by,bz,j)
             b1 = index_b(bx,by,bz)
             ABi=AB(j)
             do a=1,aub
               ax = components_a(1,a)
               ay = components_a(2,a)
               az = components_a(3,a)
               call add_to_component_(self,ax,ay,az,j)
               a1 = index_a(ax,ay,az)
               int_new(:,b,a)=int_old(:,b1,a1) + ABi * int_old(:,b1,a)
             end do
           end do
           call destroy_(int_old)
         end do

         blb              = n_comp_sum_((self%b%l-1))
         bub              = self%b%n_comp
         aub              = self%a%n_comp
         component_to_use => comp_to_use(blb+1:blb+bub)
         components_b     => components(:,blb+1:blb+bub)
         int_old          => int_new
         do b=1,bub
           bx = components_b(1,b)
           by = components_b(2,b)
           bz = components_b(3,b)
           j = component_to_use(b)
           call subtract_from_component_(self,bx,by,bz,j)
           b1 = index_b(bx,by,bz)
           ABi=AB(j)
           do a=1,aub
             ax = components_a(1,a)
             ay = components_a(2,a)
             az = components_a(3,a)
             call add_to_component_(self,ax,ay,az,j)
             a1 = index_a(ax,ay,az)
             do f=1,fub
               abcd(a,b,f)=int_old(f,b1,a1) + ABi * int_old(f,b1,a)
             end do
           end do
         end do
         call destroy_(int_old)
         call destroy_(comp_to_use)
         call destroy_(components)
         call destroy_(index_a)
         call destroy_(index_b)
     end select

   end subroutine

   subroutine transfer_l_b_highest_1(self,escd,abcd)
    type(shell4_type) :: self
    ! Applies the transfer equation to (es|cd) to give (ab|cd)
     intent(in) :: self
     real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: escd
     real(kind=kind(1.0d0)), dimension(:,:,:), intent(out) :: abcd
     real(kind=kind(1.0d0)), dimension(:,:,:), pointer :: int_new,int_old
     integer(kind=kind(1)), dimension(:,:), pointer :: components,components_a,components_b
     integer(kind=kind(1)), dimension(:,:,:), pointer :: index_a,index_b
     real(kind=kind(1.0d0)), dimension(3) :: BA
     integer(kind=kind(1)) :: a,b,f,a1,b1,b2,b3,la,aub,bub,fub
     integer(kind=kind(1)) :: ax,ay,az,bx,by,bz,j,alb,blb,tmp
     real(kind=kind(1.0d0)) :: BAi,BAx,BAy,BAz,escd_bcd
     integer(kind=kind(1)), dimension(:), pointer :: comp_to_use,component_to_use

     select case (self%a%l)
       case (0)
         abcd(1,:,:)=escd

       case (1)
         blb = n_comp_sum_((self%b%l-1))
         BA   = self%b%pos - self%a%pos
         bub  = self%b%n_comp
         fub  = size(escd,2)

         call create_(components,3, n_comp_sum_((self%a%l+self%b%l)) - blb)
         call create_(index_b,0,(self%a%l+self%b%l),0,(self%a%l+self%b%l),0,(self%a%l+self%b%l))
         call make_gaussian_xyz_powers_(self%b%l,components,(self%a%l+self%b%l),index_b)

         BAx = BA(1);    BAy = BA(2);    BAz = BA(3)
         do b = 1, bub
           bx = components(1,b)
           by = components(2,b)
           bz = components(3,b)
           b1 = index_b(bx+1,by,bz)
           b2 = index_b(bx,by+1,bz)
           b3 = index_b(bx,by,bz+1)
           do f = 1, fub
             escd_bcd = escd(b,f)
             abcd(1,b,f) = escd(b1,f) + BAx * escd_bcd
             abcd(2,b,f) = escd(b2,f) + BAy * escd_bcd
             abcd(3,b,f) = escd(b3,f) + BAz * escd_bcd
           end do
         end do

         call destroy_(components)
         call destroy_(index_b)

       case default
         blb = n_comp_sum_((self%b%l-1))
         BA   = self%b%pos - self%a%pos
         bub  = n_comp_sum_(((self%a%l+self%b%l)-1)) - blb
         aub  = self%a%n_comp
         fub  = size(escd,2)

         call create_(index_a,0,(self%a%l+self%b%l),0,(self%a%l+self%b%l),0,(self%a%l+self%b%l))
         call create_(index_b,0,(self%a%l+self%b%l),0,(self%a%l+self%b%l),0,(self%a%l+self%b%l))
         call create_(components,3, n_comp_sum_((self%a%l+self%b%l)))
         call create_(comp_to_use, n_comp_sum_((self%a%l+self%b%l)) )
         tmp=0; call make_gaussian_xyz_powers_(tmp,components,(self%a%l+self%b%l),index_a,comp_to_use)
         tmp=self%b%l; call make_gaussian_xyz_power_index_(tmp,index_b,(self%a%l+self%b%l))
         components_b => components(:,blb+1:)

         nullify(int_new)
         call create_(int_new,fub,bub,aub)

         BAx = BA(1);    BAy = BA(2);    BAz = BA(3)
         do b = 1, bub
           bx = components_b(1,b)
           by = components_b(2,b)
           bz = components_b(3,b)
           b1 = index_b(bx+1,by,bz)
           b2 = index_b(bx,by+1,bz)
           b3 = index_b(bx,by,bz+1)
           do f = 1, fub
             escd_bcd = escd(b,f)
             int_new(f,b,1) = escd(b1,f) + BAx * escd_bcd
             int_new(f,b,2) = escd(b2,f) + BAy * escd_bcd
             int_new(f,b,3) = escd(b3,f) + BAz * escd_bcd
           end do
         end do

         do la=2, self%a%l - 1
           alb              = n_comp_sum_((la-1))
           aub              = n_comp_(la)
           bub              = n_comp_sum_(((self%a%l+self%b%l)-la)) - blb
           component_to_use => comp_to_use(alb+1:alb+aub)
           components_a     => components(:,alb+1:alb+aub)
           int_old          => int_new
           nullify(int_new)
           call create_(int_new,fub,bub,aub)
           do a=1,aub
             ax = components_a(1,a)
             ay = components_a(2,a)
             az = components_a(3,a)
             j=component_to_use(a)
             call subtract_from_component_(self,ax,ay,az,j)
             a1 = index_a(ax,ay,az)
             BAi=BA(j)
             do b=1,bub
               bx = components_b(1,b)
               by = components_b(2,b)
               bz = components_b(3,b)
               call add_to_component_(self,bx,by,bz,j)
               b1 = index_b(bx,by,bz)
               int_new(:,b,a)=int_old(:,b1,a1) + BAi * int_old(:,b,a1)
             end do
           end do
           call destroy_(int_old)
         end do

         alb              = n_comp_sum_((self%a%l-1))
         aub              = self%a%n_comp
         bub              = self%b%n_comp
         component_to_use => comp_to_use(alb+1:alb+aub)
         components_a     => components(:,alb+1:alb+aub)
         int_old          => int_new
         do a=1,aub
           ax = components_a(1,a)
           ay = components_a(2,a)
           az = components_a(3,a)
           j = component_to_use(a)
           call subtract_from_component_(self,ax,ay,az,j)
           a1 = index_a(ax,ay,az)
           BAi=BA(j)
           do b=1,bub
             bx = components_b(1,b)
             by = components_b(2,b)
             bz = components_b(3,b)
             call add_to_component_(self,bx,by,bz,j)
             b1 = index_b(bx,by,bz)
             do f=1,fub
               abcd(a,b,f)=int_old(f,b1,a1) + BAi * int_old(f,b,a1)
             end do
           end do
         end do
         call destroy_(int_old)
         call destroy_(comp_to_use)
         call destroy_(components)
         call destroy_(index_b)
         call destroy_(index_a)
     end select

   end subroutine

   pure subroutine add_to_component(self,x,y,z,j)
    type(shell4_type) :: self
    ! Adds one to the component specified by j.  Used by transfer equation.
    ! j=1 => x=x+1, j=2 => y=y+1, j=3 => z=z+1.
     intent(in) :: self
     integer(kind=kind(1)), intent(inout) :: x,y,z
     integer(kind=kind(1)), intent(in) :: j
     select case(j)
       case (1); x = x + 1
       case (2); y = y + 1
       case (3); z = z + 1
     end select

   end subroutine

   pure subroutine subtract_from_component(self,x,y,z,j)
    type(shell4_type) :: self
    ! Subtracts one from the component specified by j.  Used by transfer
    ! equation.  j=1 => x=x-1, j=2 => y=y-1, j=3 => z=z-1.
     intent(in) :: self
     integer(kind=kind(1)), intent(inout) :: x,y,z
     integer(kind=kind(1)), intent(in) :: j
     select case(j)
       case (1); x = x - 1
       case (2); y = y - 1
       case (3); z = z - 1
     end select

   end subroutine

!*******************************************************************************
!                  Normalisation routines.
!*******************************************************************************

   subroutine to_normalise(self,abcd)
    type(shell4_type) :: self
    ! Multiply the matrix by the orbital normalisation coefficients
    ! for the orbitals a, b, c and d.
     intent(in) :: self
     real(kind=kind(1.0d0)), dimension(:,:,:,:), intent(inout) :: abcd
     integer(kind=kind(1)) :: aub,bub,cub,dub,a,b,c,d
     real(kind=kind(1.0d0)) :: norm_d,norm_cd,norm_bcd
     real(kind=kind(1.0d0)), dimension(:), pointer :: anorm,bnorm,cnorm,dnorm

     call create_(anorm,n_comp_(self%a%l));    call normalising_factors_(anorm,self%a%l)
     call create_(bnorm,n_comp_(self%b%l));    call normalising_factors_(bnorm,self%b%l)
     call create_(cnorm,n_comp_(self%c%l));    call normalising_factors_(cnorm,self%c%l)
     call create_(dnorm,n_comp_(self%d%l));    call normalising_factors_(dnorm,self%d%l)
     aub=self%a%n_comp
     bub=self%b%n_comp
     cub=self%c%n_comp
     dub=self%d%n_comp
     do d=1,dub
       norm_d = dnorm(d)
       do c=1,cub
         norm_cd = norm_d*cnorm(c)
         do b=1,bub
           norm_bcd = norm_cd*bnorm(b)
           do a=1,aub
             abcd(a,b,c,d)=abcd(a,b,c,d)*norm_bcd*anorm(a)
           end do
         end do
       end do
     end do
     call destroy_(dnorm)
     call destroy_(cnorm)
     call destroy_(bnorm)
     call destroy_(anorm)

   end subroutine

   subroutine to_normalise_1(self,X)
    type(shell4_type) :: self
    ! Multiply the matrix by the orbital normalisation coefficients for the
    ! orbitals a, b, c and d.
     intent(in) :: self
     real(kind=kind(1.0d0)), dimension(:,:,:,:,:), intent(inout) :: X
     integer(kind=kind(1)) :: i,dim

     dim = size(X,5)
     do i = 1,dim
        call to_normalise_(self,X(:,:,:,:,i))
     end do

   end subroutine

!*******************************************************************************
!       make the J and K contributions from the shell4 and density matrix.
!*******************************************************************************

   subroutine make_r_JK(self,J,K,P,factor,fa,la,fb,lb,fc,lc,fd,ld)
    type(shell4_type) :: self
    ! Make the J and K contribution due to self and P and add it in.
    ! For any shell4!
     intent(in) :: self
     real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: P
     real(kind=kind(1.0d0)), dimension(:,:), target :: J,K
     integer(kind=kind(1)), intent(in) :: fa,la,fb,lb,fc,lc,fd,ld
     real(kind=kind(1.0d0)), intent(in) :: factor
     real(kind=kind(1.0d0)), dimension(:,:,:,:), pointer :: I4
     real(kind=kind(1.0d0)), dimension(:), pointer :: Kc,Kd
     real(kind=kind(1.0d0)) :: P_dc,Jcd,P_db,P_cb,Kbc,Kbd,I_abcd
     integer(kind=kind(1)) :: a,b,c,d

     call create_(I4,fa,la,fb,lb,fc,lc,fd,ld)
     call get_ERI_(self,I4)
     if (factor > 0.9) then
       do d = fd,ld
         Kd => K(:,d)
         do c = fc,lc
           P_dc = P(d,c)
           Kc => K(:,c)
           Jcd = 0.0d0
           do b = fb,lb
             P_db = P(d,b)
             P_cb = P(c,b)
             Kbc = 0.0d0
             Kbd = 0.0d0
             do a = fa,la
               I_abcd = I4(a,b,c,d)
               J(a,b) = J(a,b) + I_abcd*P_dc
               Kc(a)  = Kc(a)  + I_abcd*P_db
               Kd(a)  = Kd(a)  + I_abcd*P_cb
               Jcd    = Jcd    + I_abcd*P(b,a)
               Kbc    = Kbc    + I_abcd*P(d,a)
               Kbd    = Kbd    + I_abcd*P(c,a)
             end do
             Kc(b) = Kc(b) + Kbc
             Kd(b) = Kd(b) + Kbd
           end do
           J(c,d) = J(c,d) + Jcd
         end do
       end do
     else
       do d = fd,ld
         Kd => K(:,d)
         do c = fc,lc
           P_dc = factor*P(d,c)
           Kc => K(:,c)
           Jcd = 0.0d0
           do b = fb,lb
             P_db = factor*P(d,b)
             P_cb = factor*P(c,b)
             Kbc = 0.0d0
             Kbd = 0.0d0
             do a = fa,la
               I_abcd = I4(a,b,c,d)
               J(a,b) = J(a,b) + I_abcd*P_dc
               Kc(a)  = Kc(a)  + I_abcd*P_db
               Kd(a)  = Kd(a)  + I_abcd*P_cb
               Jcd    = Jcd    + I_abcd*P(b,a)
               Kbc    = Kbc    + I_abcd*P(d,a)
               Kbd    = Kbd    + I_abcd*P(c,a)
             end do
             Kc(b) = Kc(b) + factor*Kbc
             Kd(b) = Kd(b) + factor*Kbd
           end do
           J(c,d) = J(c,d) + factor*Jcd
         end do
       end do
     end if
     call destroy_(I4)

   end subroutine

!*******************************************************************************
!       make only the J contributions from the shell4 and density matrix.
!*******************************************************************************

   subroutine make_r_J(self,J,P,factor,fa,la,fb,lb,fc,lc,fd,ld)
    type(shell4_type) :: self
    ! Make the J contribution due to self and P and add it in.
    ! For any shell4!
     intent(in) :: self
     real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: P
     real(kind=kind(1.0d0)), dimension(:,:), target :: J
     integer(kind=kind(1)), intent(in) :: fa,la,fb,lb,fc,lc,fd,ld
     real(kind=kind(1.0d0)), intent(in) :: factor
     real(kind=kind(1.0d0)), dimension(:,:,:,:), pointer :: I4
     real(kind=kind(1.0d0)) :: P_dc,Jcd,P_db,P_cb,I_abcd
     integer(kind=kind(1)) :: a,b,c,d

     call create_(I4,fa,la,fb,lb,fc,lc,fd,ld)
     call get_ERI_(self,I4)
     if (factor > 0.9) then
       do d = fd,ld
         do c = fc,lc
           P_dc = P(d,c)
           Jcd = 0.0d0
           do b = fb,lb
             P_db = P(d,b)
             P_cb = P(c,b)
             do a = fa,la
               I_abcd = I4(a,b,c,d)
               J(a,b) = J(a,b) + I_abcd*P_dc
               Jcd    = Jcd    + I_abcd*P(b,a)
             end do
           end do
           J(c,d) = J(c,d) + Jcd
         end do
       end do
     else
       do d = fd,ld
         do c = fc,lc
           P_dc = factor*P(d,c)
           Jcd = 0.0d0
           do b = fb,lb
             P_db = factor*P(d,b)
             P_cb = factor*P(c,b)
             do a = fa,la
               I_abcd = I4(a,b,c,d)
               J(a,b) = J(a,b) + I_abcd*P_dc
               Jcd    = Jcd    + I_abcd*P(b,a)
             end do
           end do
           J(c,d) = J(c,d) + factor*Jcd
         end do
       end do
     end if
     call destroy_(I4)

   end subroutine

!*******************************************************************************
!  Output Routines.
!*******************************************************************************

   subroutine put(self)
    type(shell4_type) :: self
    ! Put the shell4 information to file "out"
     integer(kind=kind(1)) :: n_cc,i

     call flush_(stdout)
     call show_(stdout,"A shell l quantum number =",self%a%l)
     call show_(stdout,"B shell l quantum number =",self%b%l)
     call show_(stdout,"C shell l quantum number =",self%c%l)
     call show_(stdout,"D shell l quantum number =",self%d%l)
     call show_(stdout,"A position               =",self%a%pos)
     call show_(stdout,"B position               =",self%b%pos)
     call show_(stdout,"C position               =",self%c%pos)
     call show_(stdout,"D position               =",self%d%pos)
     call flush_(stdout)
     call dash_(stdout,int_fields=1,real_fields=8)
     call put_(stdout,"N", int_width=.true.)
     call put_(stdout,"ex_a")
     call put_(stdout,"cc_a")
     call put_(stdout,"ex_b")
     call put_(stdout,"cc_b")
     call put_(stdout,"ex_c")
     call put_(stdout,"cc_c")
     call put_(stdout,"ex_d")
     call put_(stdout,"cc_d")
     call flush_(stdout)
     call dash_(stdout,int_fields=1,real_fields=8)
     n_cc = max(self%a%n_cc,self%b%n_cc,self%c%n_cc,self%d%n_cc)
     do i = 1,n_cc
        call put_(stdout,i)
        if (i<=self%a%n_cc) then
        call put_(stdout, self%a%ex(i))
        call put_(stdout, self%a%cc(i))
        else
        call tab_(stdout,real_fields=2)
        end if
        if (i<=self%b%n_cc) then
        call put_(stdout, self%b%ex(i))
        call put_(stdout, self%b%cc(i))
        else
        call tab_(stdout,real_fields=2)
        end if
        if (i<=self%c%n_cc) then
        call put_(stdout, self%c%ex(i))
        call put_(stdout, self%c%cc(i))
        else
        call tab_(stdout,real_fields=2)
        end if
        if (i<=self%d%n_cc) then
        call put_(stdout, self%d%ex(i))
        call put_(stdout, self%d%cc(i))
        else
        call tab_(stdout,real_fields=2)
        end if
        call flush_(stdout)
     end do
     call dash_(stdout,int_fields=1,real_fields=8)

   end subroutine

end
