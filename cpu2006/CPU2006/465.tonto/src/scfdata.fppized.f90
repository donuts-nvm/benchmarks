!---------------------------------------------------------------------------
!
!  SCFDATA: Store SCF data and deal with iteration control ......
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
! $Id: scfdata.foo,v 1.67.2.9 2004/04/21 09:12:56 reaper Exp $
!---------------------------------------------------------------------------

module SCFDATA_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

   use DIIS_MODULE, only: set_defaults_
   use DIIS_MODULE, only: nullify_ptr_part_
   use DIIS_MODULE, only: delete_archives_
   use DIIS_MODULE, only: destroy_ptr_part_
   use DIIS_MODULE, only: set_keep_
   use DIIS_MODULE, only: cleanup_
   use DIIS_MODULE, only: set_archive_name_
   use DIIS_MODULE, only: copy_

   use INT_MODULE, only: to_str_

   use STR_MODULE, only: to_lower_case_
   use STR_MODULE, only: includes_

   use TEXTFILE_MODULE, only: stdin
   use TEXTFILE_MODULE, only: stdout
   use TEXTFILE_MODULE, only: text_
   use TEXTFILE_MODULE, only: redirect_
   use TEXTFILE_MODULE, only: put_text_
   use TEXTFILE_MODULE, only: show_
   use TEXTFILE_MODULE, only: put_
   use TEXTFILE_MODULE, only: revert_
   use TEXTFILE_MODULE, only: tab_
   use TEXTFILE_MODULE, only: read_
   use TEXTFILE_MODULE, only: flush_
   use TEXTFILE_MODULE, only: set_real_style_
   use TEXTFILE_MODULE, only: reverted_
   use TEXTFILE_MODULE, only: dash_

   use CRYSTAL_MODULE, only: F_r_factor_
   use CRYSTAL_MODULE, only: F_weighted_r_factor_
   use CRYSTAL_MODULE, only: F_goodness_of_fit_
   use CRYSTAL_MODULE, only: F_chi2_

   use REAL_MODULE, only: is_zero_
   use REAL_MODULE, only: to_str_no_zeros_
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

   public    read_rough_diis_convergence_
   interface read_rough_diis_convergence_
      module procedure read_rough_diis_convergence
   end interface

   public    read_diis_start_
   interface read_diis_start_
      module procedure read_diis_start
   end interface

   public    put_results_
   interface put_results_
      module procedure put_results
   end interface

   public    update_
   interface update_
      module procedure update
   end interface

   public    nullify_ptr_part_
   interface nullify_ptr_part_
      module procedure nullify_ptr_part
   end interface

   public    scf_done_
   interface scf_done_
      module procedure scf_done
   end interface

   public    orbital_energy_kind_
   interface orbital_energy_kind_
      module procedure orbital_energy_kind
   end interface

   public    put_constrained_scf_results_
   interface put_constrained_scf_results_
      module procedure put_constrained_scf_results
   end interface

   public    read_diis_keep_
   interface read_diis_keep_
      module procedure read_diis_keep
   end interface

   public    put_banner_
   interface put_banner_
      module procedure put_banner
   end interface

   public    read_rough_convergence_
   interface read_rough_convergence_
      module procedure read_rough_convergence
   end interface

   public    put_crystal_
   interface put_crystal_
      module procedure put_crystal
   end interface

   public    read_max_iterations_
   interface read_max_iterations_
      module procedure read_max_iterations
   end interface

   public    read_kind_
   interface read_kind_
      module procedure read_kind
   end interface

   public    read_diis_auto_start_
   interface read_diis_auto_start_
      module procedure read_diis_auto_start
   end interface

   public    destroy_ptr_part_
   interface destroy_ptr_part_
      module procedure destroy_ptr_part
   end interface

   private    put_summary_
   interface put_summary_
      module procedure put_summary
   end interface

   public    read_keywords_
   interface read_keywords_
      module procedure read_keywords
   end interface

   public    apply_diis_
   interface apply_diis_
      module procedure apply_diis
   end interface

   public    read_delta_build_
   interface read_delta_build_
      module procedure read_delta_build
   end interface

   public    roughly_converged_
   interface roughly_converged_
      module procedure roughly_converged
   end interface

   public    diff_converged_
   interface diff_converged_
      module procedure diff_converged
   end interface

   public    process_keyword_
   interface process_keyword_
      module procedure process_keyword
   end interface

   public    read_initial_density_
   interface read_initial_density_
      module procedure read_initial_density
   end interface

   public    spinorbital_kind_
   interface spinorbital_kind_
      module procedure spinorbital_kind
   end interface

   public    molecular_orbital_kind_
   interface molecular_orbital_kind_
      module procedure molecular_orbital_kind
   end interface

   public    read_MO_gradient_update_
   interface read_MO_gradient_update_
      module procedure read_MO_gradient_update
   end interface

   public    using_diis_
   interface using_diis_
      module procedure using_diis
   end interface

   public    copy_
   interface copy_
      module procedure copy
   end interface

   public    eri_cutoff_
   interface eri_cutoff_
      module procedure eri_cutoff
   end interface

   public    set_defaults_
   interface set_defaults_
      module procedure set_defaults
   end interface

   public    set_
   interface set_
      module procedure set
   end interface

   public    create_
   interface create_
      module procedure create
   end interface

   public    create_copy_
   interface create_copy_
      module procedure create_copy
   end interface

   public    set_diis_error_
   interface set_diis_error_
      module procedure set_diis_error
   end interface

   private    put_table_head_
   interface put_table_head_
      module procedure put_table_head
   end interface

   public    number_kind_
   interface number_kind_
      module procedure number_kind
   end interface

   public    finalize_
   interface finalize_
      module procedure finalize
   end interface

   public    read_dft_correlation_
   interface read_dft_correlation_
      module procedure read_dft_correlation
   end interface

   public    apply_rough_convergence_
   interface apply_rough_convergence_
      module procedure apply_rough_convergence
   end interface

   public    exceeded_lambda_max_
   interface exceeded_lambda_max_
      module procedure exceeded_lambda_max
   end interface

   public    read_fock_diis_
   interface read_fock_diis_
      module procedure read_fock_diis
   end interface

   private    put_table_foot_
   interface put_table_foot_
      module procedure put_table_foot
   end interface

   public    fitting_
   interface fitting_
      module procedure fitting
   end interface

   public    exceeded_min_it_
   interface exceeded_min_it_
      module procedure exceeded_min_it
   end interface

   public    exceeded_max_it_
   interface exceeded_max_it_
      module procedure exceeded_max_it
   end interface

   public    read_initial_mos_
   interface read_initial_mos_
      module procedure read_initial_mos
   end interface

   public    diis_converged_
   interface diis_converged_
      module procedure diis_converged
   end interface

   public    apply_camp_king_
   interface apply_camp_king_
      module procedure apply_camp_king
   end interface

   public    orbital_energies_kind_
   interface orbital_energies_kind_
      module procedure orbital_energies_kind
   end interface

   public    eri_cutoff_altered_
   interface eri_cutoff_altered_
      module procedure eri_cutoff_altered
   end interface

   public    destroy_
   interface destroy_
      module procedure destroy
   end interface

   public    read_dft_exchange_
   interface read_dft_exchange_
      module procedure read_dft_exchange
   end interface

   public    diis_used_
   interface diis_used_
      module procedure diis_used
   end interface

   public    read_direct_
   interface read_direct_
      module procedure read_direct
   end interface

   public    apply_dynamic_damping_
   interface apply_dynamic_damping_
      module procedure apply_dynamic_damping
   end interface

   public    delete_diis_archives_
   interface delete_diis_archives_
      module procedure delete_diis_archives
   end interface

   public    read_convergence_
   interface read_convergence_
      module procedure read_convergence
   end interface

   public    update_lambda_
   interface update_lambda_
      module procedure update_lambda
   end interface

   public    reset_
   interface reset_
      module procedure reset
   end interface

   public    apply_MO_diis_
   interface apply_MO_diis_
      module procedure apply_MO_diis
   end interface

   public    read_min_iterations_
   interface read_min_iterations_
      module procedure read_min_iterations
   end interface

   public    cleanup_diis_
   interface cleanup_diis_
      module procedure cleanup_diis
   end interface

   public    apply_fock_diis_
   interface apply_fock_diis_
      module procedure apply_fock_diis
   end interface

   public    read_MO_diis_
   interface read_MO_diis_
      module procedure read_MO_diis
   end interface

   public    apply_damping_
   interface apply_damping_
      module procedure apply_damping
   end interface

   public    converged_
   interface converged_
      module procedure converged
   end interface

   public    read_eri_limit_
   interface read_eri_limit_
      module procedure read_eri_limit
   end interface

   public    apply_level_shifting_
   interface apply_level_shifting_
      module procedure apply_level_shifting
   end interface

   public    do_delta_build_
   interface do_delta_build_
      module procedure do_delta_build
   end interface

   public    read_diis_convergence_
   interface read_diis_convergence_
      module procedure read_diis_convergence
   end interface

contains

!  *******************
!  Allocation routines
!  *******************

   subroutine create(self)
    type(scfdata_type) :: self
    ! Create space for an SCF type
     pointer :: self

     nullify(self)
     allocate(self)

     call nullify_ptr_part_(self)
     call set_defaults_(self)

   end subroutine

   subroutine destroy(self)
    type(scfdata_type) :: self
    ! Destroy space for an SCF type
     pointer :: self

     if (associated(self)) then
       call destroy_ptr_part_(self)

       deallocate(self)
     end if

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

   subroutine nullify_ptr_part(self)
    type(scfdata_type) :: self
    ! Nullify the pointer parts of self

      call nullify_ptr_part_(self%diis)

   end subroutine

   subroutine destroy_ptr_part(self)
    type(scfdata_type) :: self
    ! Destroy the pointer parts of self

      call destroy_ptr_part_(self%diis)

   end subroutine

   subroutine create_copy(self,scfdata)
    type(scfdata_type) :: self
    ! Create a copy of the "scfdata" object
      pointer :: self
      type(scfdata_type) :: scfdata

      call create_(self)
      call copy_(self,scfdata)

   end subroutine

   subroutine copy(self,scfdata)
    type(scfdata_type) :: self
    ! Make a copy of the "scfdata" object
      type(scfdata_type) :: scfdata

      self = scfdata
      call copy_(self%diis,scfdata%diis)

   end subroutine

   subroutine delete_diis_archives(self)
    type(scfdata_type) :: self
    ! Delete the type(diis_type) archives on disk

     call delete_archives_(self%diis)

   end subroutine

   subroutine cleanup_diis(self)
    type(scfdata_type) :: self
    ! Delete the type(diis_type) archives on disk, and restore the type(diis_type) object so it can be
    ! reused.  Does not go back to the default type(diis_type) parameters.

     call cleanup_(self%diis)

   end subroutine

!  ************
!  Set routines
!  ************

   subroutine set_defaults(self)
    type(scfdata_type) :: self
    ! Set default SCF data values

     self%scf_kind                  = " "  ! Purposely set blank so diis comes later
     self%dft_exchange              = "slater"
     self%dft_correlation           = "lyp"
     self%dft_non_local_exchange    = .false.
     self%dft_non_local_correlation = .true.
     self%initial_density         = "core"
     self%initial_mos             = " "
     self%nuclear_energy          = 0.0d0
     self%kinetic_energy          = 0.0d0
     self%energy                  = 0.0d0
     self%old_energy              = 0.0d0
     self%dft_energy_correction   = 0.0d0
     self%difference              = 0.0d0
     self%convergence             = 10.0d0**(-4)              ! 10.0d0**(-4)
     self%diis_convergence        = 10.0d0**(-4)         ! 10.0d0**(-4)
     self%diis_start_iteration    = 3     ! 3
     self%using_rough_convergence = .true.  ! .true.
     self%rough_convergence       = 10.0d0**(-2)        ! 10.0d0**(-2)
     self%rough_diis_convergence  = 10.0d0**(-2)   ! 10.0d0**(-2)
     self%iteration               = 0
     self%total_iterations        = 0
     self%lambda_iteration        = 0
     self%min_iterations          = 1
     self%max_iterations          = 100
     self%lambda                  = 0
     self%lambda_max              = 0
     self%lambda_step             = 1
     self%fit_value               = 0.0d0
     self%old_fit_value           = 0.0d0
     self%F_chi2                  = 0.0d0
     self%old_F_chi2              = 0.0d0
     self%F_gof                   = 0.0d0
     self%F_r_factor              = 0.0d0
     self%F_weighted_r_factor     = 0.0d0
     self%test                    = .false.
     self%direct                  = .false.
     self%using_delta_build       = .false.
     self%using_fock_diis         = .true.
     self%using_MO_diis           = .false.
     self%using_damping           = .true.  ! .true.
     self%using_level_shift       = .true.  ! .true.
     self%using_camp_king         = .false.
     self%camp_king_iters         = 0
     self%using_dynamic_damping   = .false.
     self%dynamic_damp_factor     = 0.0d0
     call set_defaults_(self%diis)
     self%diis_error              = 0.0d0
     self%old_diis_error          = 0.0d0
     self%using_diis_auto_start   = .false.
     self%damp_finish             = 3
     self%damp_factor             = 0.5d0
     self%level_shift             = 0.3d0
     self%level_shift_finish      = 3
     self%output                  = .true.
     self%nddo                    = .false.
     self%nudo                    = .false.
     self%pie                     = .false.
     self%using_bl_term           = .true.
     self%using_bs_term           = .true.
     self%using_bs_t_term         = .true.
     self%using_aa_term           = .true.
     self%using_1e_sl_term        = .true.
     self%using_1e_srxa_term      = .true.
     self%using_2e_sl_term        = .true.
     self%using_1e_zora_term      = .false.
     self%sl_1e_factor            = 1.0d0
     self%sl_2e_factor            = 1.0d0
     self%eri_limit               = 1.0d-12
     self%old_eri_cutoff          = 1.0d-12
     self%quantization_axis       = (/ 0.0d0, 0.0d0, 1.0d0 /)
     self%group                   = .false.
     self%using_MO_gradient_update= .false.
     self%MO_gradient_stepsize    = 0.01d0
     self%max_update_stepsize     = 0.01d0

   end subroutine

   subroutine set(self,nuclear_energy,energy,kinetic_energy,dft_energy_correction,crystal)
    type(scfdata_type) :: self
    ! Set various parts of the scfdata type.
     real(kind=kind(1.0d0)), intent(in), optional :: nuclear_energy,energy,kinetic_energy,dft_energy_correction
     type(crystal_type), pointer, optional :: crystal

     if (present(nuclear_energy))        self%nuclear_energy = nuclear_energy
     if (present(dft_energy_correction)) self%dft_energy_correction = dft_energy_correction
     if (present(crystal)) then
     if (associated(crystal)) then
       if (associated(crystal%reflections)) then
         self%old_F_chi2 = self%F_chi2
         self%F_chi2 = F_chi2_(crystal)
         self%F_gof = F_goodness_of_fit_(crystal)
         self%F_r_factor = F_r_factor_(crystal)
         self%F_weighted_r_factor = F_weighted_r_factor_(crystal)
       end if
     end if
     end if
     if (present(energy)) then
       self%energy = energy
       if (fitting_(self)) self%fit_value = energy + self%lambda * self%F_chi2
     end if
     if (present(kinetic_energy))    self%kinetic_energy = kinetic_energy

   end subroutine

   subroutine reset(self,nuclear_energy,energy,kinetic_energy,dft_energy_correction,crystal)
    type(scfdata_type) :: self
    ! Reset SCF energy and iteration data , but don't change
    ! nuclear_energy, convergence, direct, or max_it options
      real(kind=kind(1.0d0)), intent(in), optional :: nuclear_energy,energy,kinetic_energy,dft_energy_correction
      type(crystal_type), pointer, optional :: crystal

     self%energy                = 0.0d0
     self%fit_value             = 0.0d0
     self%old_fit_value         = 0.0d0
     self%kinetic_energy        = 0.0d0
     self%old_energy            = 0.0d0
     self%dft_energy_correction = 0.0d0
     self%iteration             = 0
     self%total_iterations      = 0
     self%lambda_iteration      = 0
     self%F_chi2                = 0.0d0
     self%F_gof                 = 0.0d0
     self%F_r_factor            = 0.0d0
     self%F_weighted_r_factor   = 0.0d0
     self%old_eri_cutoff        = eri_cutoff_(self)
     call set_(self,nuclear_energy,energy,kinetic_energy,dft_energy_correction,crystal)
     if (present(energy)) self%difference = energy
     if (present(crystal)) self%old_fit_value = 0.0d0
     if (present(dft_energy_correction)) self%dft_energy_correction = dft_energy_correction

   end subroutine

   subroutine update_lambda(self)
    type(scfdata_type) :: self
    ! Increments lambda by lambda_step

     self%lambda           = self%lambda + self%lambda_step
     self%lambda_iteration = self%lambda_iteration + 1
     self%iteration        = 0
     self%old_fit_value    = self%old_energy + self%lambda * self%old_F_chi2
     self%fit_value        = self%energy + self%lambda * self%F_chi2

   end subroutine

   subroutine update(self,energy,kinetic_energy,dft_energy_correction,crystal)
    type(scfdata_type) :: self
    ! Update the SCF energy and/or chi2 and increment iteration and any other
    ! data of use in the SCF calculation.
     real(kind=kind(1.0d0)), intent(in), optional :: energy,kinetic_energy,dft_energy_correction
     type(crystal_type), pointer, optional :: crystal

     self%old_fit_value = self%fit_value
     self%old_energy = self%energy
     call set_(self,energy=energy,kinetic_energy=kinetic_energy, &
                dft_energy_correction=dft_energy_correction,crystal=crystal)
     self%iteration = self%iteration + 1
     self%total_iterations = self%total_iterations + 1
     self%difference = self%energy - self%old_energy
      ! For updating the fock matrix in incremental builds
     self%old_eri_cutoff = eri_cutoff_(self)

   end subroutine

   subroutine set_diis_error(self,err)
    type(scfdata_type) :: self
    ! Set the type(diis_type) error and the starting iteration for automatic type(diis_type) start
     real(kind=kind(1.0d0)), intent(in) :: err
     logical(kind=kind(.true.)) :: set_start

     self%old_diis_error = self%diis_error
     self%diis_error = err
     set_start = self%using_diis_auto_start .and. &
                 self%diis_start_iteration == self%max_iterations .and. &
                (self%diis_error<self%rough_diis_convergence)
!     if (set_start) .diis.set_start(.iteration)

   end subroutine

   function spinorbital_kind(self,scf_kind) result(res)
    type(scfdata_type) :: self
    ! Return the kind of spinorbitals used in a particular "scf_kind"
    ! (i.e. for the fock_matrix and density_matrix, but not neccesarily the mo's)
     character(*), optional :: scf_kind
     character(128) :: res
     character(128) :: s_kind

     s_kind = self%scf_kind
     if (present(scf_kind)) s_kind = scf_kind
     select case (s_kind)
       case("rhf","rdft","restricted_hartree_fock"); res = "restricted"
       case("rohf", "restricted_open_shell_hartree_fock"); res = "unrestricted"
       case("uhf","udft","unrestricted_hartree_fock");    res = "unrestricted"
       case("ghf","general_hartree_fock");         res = "general"
       case("rchf","crhf", &
            "restricted_complex_hartree_fock", &
            "complex_restricted_hartree_fock");    res = "restricted_complex"
       case("uchf","cuhf", &
            "unrestricted_complex_hartree_fock", &
            "complex_unrestricted_hartree_fock");  res = "unrestricted_complex"
       case("gchf","cghf", &
            "general_complex_hartree_fock", &
            "complex_general_hartree_fock");       res = "general_complex"
       case("xray_rhf", &
            "xray_restricted_hartree_fock");       res = "restricted"
       case("xray_rks");                           res = "restricted"
       case("xray_rdft");                          res = "restricted"
       case("xray_udft");                          res = "unrestricted"
       case("rdvpt","restricted_dvpt");            res = "restricted"
       case("noninteracting-group-rhf");           res = "restricted"
       case default; call die_(tonto,"SCFDATA:spinorbital_kind ... unknown scf kind")
     end select

   end function

   function molecular_orbital_kind(self,scf_kind) result(res)
    type(scfdata_type) :: self
    ! Return the kind of spinorbitals used for the molecular orbitals in a
    ! particular "scf_kind"
     character(*), optional :: scf_kind
     character(128) :: res
     character(128) :: s_kind

      s_kind = self%scf_kind
     if (present(scf_kind)) s_kind = scf_kind
     select case (s_kind)
       case("rhf","rdft","restricted_hartree_fock"); res = "restricted"
       case("rohf", &
            "restricted_open_shell_hartree_fock"); res = "restricted"
       case("uhf","udft", "unrestricted_hartree_fock");    res = "unrestricted"
       case("ghf","general_hartree_fock");         res = "general"
       case("rchf","crhf", &
            "restricted_complex_hartree_fock", &
            "complex_restricted_hartree_fock");    res = "restricted_complex"
       case("uchf","cuhf", &
            "unrestricted_complex_hartree_fock", &
            "complex_unrestricted_hartree_fock");  res = "unrestricted_complex"
       case("gchf","cghf", &
            "general_complex_hartree_fock", &
            "complex_general_hartree_fock");       res = "general_complex"
       case("xray_rhf", &
            "xray_restricted_hartree_fock");       res = "restricted"
       case("xray_rks");                           res = "restricted"
       case("xray_rdft");                          res = "restricted"
       case("xray_udft");                          res = "unrestricted"
       case("rdvpt","restricted_dvpt");            res = "restricted"
       case("noninteracting-group-rhf");           res = "restricted"
       case default; call die_(tonto,"SCFDATA:molecular_orbital_kind ... unknown scf kind")
     end select

   end function

   function orbital_energy_kind(self,scf_kind) result(res)
    type(scfdata_type) :: self
    ! Return the kind of vectors used for the orbital energies in a
    ! particular "scf_kind"
      character(*), optional :: scf_kind
      character(128) :: res

      res = orbital_energies_kind_(self,scf_kind)

   end function

   function orbital_energies_kind(self,scf_kind) result(res)
    type(scfdata_type) :: self
    ! Return the kind of vectors used for the orbital energies in a
    ! particular "scf_kind"
      character(*), optional :: scf_kind
      character(128) :: res
      character(128) :: s_kind

      s_kind = self%scf_kind
      if (present(scf_kind)) s_kind = scf_kind
      select case (s_kind)
         case("rhf","rdft","restricted_hartree_fock");      res = "restricted"
         case("rohf","restricted_open_shell_hartree_fock"); res = "restricted"
         case("uhf","udft", "unrestricted_hartree_fock");   res = "unrestricted"
         case("ghf","general_hartree_fock");                res = "general"
         case("rchf","crhf", &
              "restricted_complex_hartree_fock", &
              "complex_restricted_hartree_fock");           res = "restricted"
         case("uchf","cuhf", &
              "unrestricted_complex_hartree_fock", &
              "complex_unrestricted_hartree_fock");         res = "unrestricted"
         case("gchf","cghf", &
              "general_complex_hartree_fock", &
              "complex_general_hartree_fock");              res = "general"
         case("xray_rhf","xray_restricted_hartree_fock");   res = "restricted"
         case("xray_rks");                                  res = "restricted"
         case("xray_rdft");                                 res = "restricted"
         case("xray_udft");                                 res = "unrestricted"
         case("rdvpt","restricted_dvpt");                   res = "restricted"
         case("noninteracting-group-rhf");                  res = "restricted"
         case default; call die_(tonto,"SCFDATA:orbital_energies_kind ... unknown scf kind")
      end select

   end function

   function number_kind(self) result(res)
    type(scfdata_type) :: self
    ! Return the kind of numbers used for a particular "kind" of scf calculation
      character(128) :: res

      select case (self%scf_kind)
         case("rhf","rdft","rohf","uhf","udft", "ghf");    res = "real"
         case("rchf","uchf","gchf");                       res = "complex"
         case("restricted_hartree_fock");                  res = "real"
         case("restricted_open_shell_hartree_fock");       res = "real"
         case("unrestricted_hartree_fock");                res = "real"
         case("general_hartree_fock");                     res = "real"
         case("restricted_complex_hartree_fock");          res = "complex"
         case("unrestricted_complex_hartree_fock");        res = "complex"
         case("general_complex_hartree_fock");             res = "complex"
         case("xray_rhf","xray_restricted_hartree_fock");  res = "real"
         case("xray_rks");                                 res = "real"
         case("xray_rdft","xray_udft");                    res = "real"
         case("rdvpt","restricted_dvpt");                  res = "real"
         case("noninteracting-group-rhf");                 res = "real"
         case default; call die_(tonto,"SCFDATA:number_kind ... unknown scf kind")
      end select

   end function

!  *************
!  Input methods
!  *************

   recursive subroutine read_keywords(self)
    type(scfdata_type) :: self
    ! Read data from "stdin" using keyword style input.
      character(128) :: word

      call read_(stdin,word)
   call ensure_(tonto,word=="{","SCFDATA:read_keywords ... expecting an open bracket symbol, {")
      call set_defaults_(self)
      read_loop: do              ! Loop over keywords
         call read_(stdin,word)
         if (word=="}")         exit read_loop
         if (reverted_(stdin))    exit read_loop
         call process_keyword_(self,word)
      end do read_loop
      call finalize_(self)

   end subroutine

   subroutine process_keyword(self,keyword)
    type(scfdata_type) :: self
    ! Process a command "keyword". Data is inputted from "stdin", unless
    ! "word" is a sequence of blank separated strings. In this case,
    ! the sequence is processed as if it were a separate file.
      character(*) :: keyword
      character(128) :: word

      word = keyword
      call to_lower_case_(word)
      if (includes_(word," ")) then
         call redirect_(stdin,(/word/))
         call read_keywords_(self)
         call revert_(stdin)
      else
         select case (word)
            case ("}                       ")   ! exit case
            case ("1e_sl_factor=           "); call read_(stdin,self%sl_1e_factor)
            case ("2e_sl_factor=           "); call read_(stdin,self%sl_2e_factor)
            case ("camp-king=              "); call read_(stdin,self%using_camp_king)
            case ("convergence=            "); call read_convergence_(self)
            case ("converge=               "); call read_convergence_(self)
            case ("damp_factor=            "); call read_(stdin,self%damp_factor)
            case ("damp_finish=            "); call read_(stdin,self%damp_finish)
            case ("dft_correlation=        "); call read_dft_correlation_(self)
            case ("dft_exchange=           "); call read_dft_exchange_(self)
            case ("diis=                   "); call read_fock_diis_(self)
            case ("diis_auto_start=        "); call read_diis_auto_start_(self)
            case ("diis_convergence=       "); call read_diis_convergence_(self)
            case ("diis_keep=              "); call read_diis_keep_(self)
            case ("diis_start=             "); call read_diis_start_(self)
            case ("direct=                 "); call read_direct_(self)
            case ("dynamic_damping=        "); call read_(stdin,self%using_dynamic_damping)
            case ("eri_cutoff=             "); call read_eri_limit_(self)
            case ("eri_limit=              "); call read_eri_limit_(self)
            case ("fock_diis=              "); call read_fock_diis_(self)
            case ("guess=                  "); call read_initial_density_(self)
            case ("initial_density=        "); call read_initial_density_(self)
            case ("initial_guess=          "); call read_initial_density_(self)
            case ("initial_lambda=         "); call read_(stdin,self%lambda)
            case ("initial_mos=            "); call read_initial_mos_(self)
            case ("kind=                   "); call read_kind_(self)
            case ("lambda_max=             "); call read_(stdin,self%lambda_max)
            case ("lambda_min=             "); call read_(stdin,self%lambda)
            case ("lambda_step=            "); call read_(stdin,self%lambda_step)
            case ("level_shift=            "); call read_(stdin,self%level_shift)
            case ("level_shift_finish=     "); call read_(stdin,self%level_shift_finish)
            case ("min_iterations=         "); call read_min_iterations_(self)
            case ("min_it=                 "); call read_min_iterations_(self)
            case ("max_iterations=         "); call read_max_iterations_(self)
            case ("max_it=                 "); call read_max_iterations_(self)
            case ("max_update_stepsize=    "); call read_(stdin,self%max_update_stepsize)
            case ("mo_diis=                "); call read_mo_diis_(self)
            case ("mo_gradient_stepsize=   "); call read_(stdin,self%MO_gradient_stepsize)
            case ("nddo=                   "); call read_(stdin,self%nddo)
            case ("nudo=                   "); call read_(stdin,self%nudo)
            case ("output=                 "); call read_(stdin,self%output)
            case ("pie=                    "); call read_(stdin,self%pie)
            case ("quantization_axis=      "); call read_(stdin,self%quantization_axis)
            case ("rough_convergence=      "); call read_rough_convergence_(self)
            case ("rough_diis_convergence= "); call read_rough_diis_convergence_(self)
            case ("scf_kind=               "); call read_kind_(self)
            case ("scf_type=               "); call read_kind_(self)
            case ("sl_1e_factor=           "); call read_(stdin,self%sl_1e_factor)
            case ("sl_2e_factor=           "); call read_(stdin,self%sl_2e_factor)
            case ("test=                   "); call read_(stdin,self%test)
            case ("use_1e_sl_term=         "); call read_(stdin,self%using_1e_sl_term)
            case ("use_1e_s(rxa)_term=     "); call read_(stdin,self%using_1e_srxa_term)
            case ("use_1e_zora_term=       "); call read_(stdin,self%using_1e_zora_term)
            case ("use_2e_sl_term=         "); call read_(stdin,self%using_2e_sl_term)
            case ("use_aa_term=            "); call read_(stdin,self%using_aa_term)
            case ("use_bl_term=            "); call read_(stdin,self%using_bl_term)
            case ("use_bs_term=            "); call read_(stdin,self%using_bs_term)
            case ("use_bs_t_term=          "); call read_(stdin,self%using_bs_t_term)
            case ("use_damping=            "); call read_(stdin,self%using_damping)
            case ("use_delta_build=        "); call read_delta_build_(self)
            case ("use_diis=               "); call read_fock_diis_(self)
            case ("use_fock_diis=          "); call read_fock_diis_(self)
            case ("use_mo_diis=            "); call read_MO_diis_(self)
            case ("use_mo_gradient_update= "); call read_MO_gradient_update_(self)
            case ("use_level_shifting=     "); call read_(stdin,self%using_level_shift)
            case ("use_level_shift=        "); call read_(stdin,self%using_level_shift)
            case ("use_rough_convergence=  "); call read_(stdin,self%using_rough_convergence)
            case default;           allocate(tonto%known_keywords(67))
            tonto%known_keywords(1) = "}                       "
            tonto%known_keywords(2) = "1e_sl_factor=           "
            tonto%known_keywords(3) = "2e_sl_factor=           "
            tonto%known_keywords(4) = "camp-king=              "
            tonto%known_keywords(5) = "convergence=            "
            tonto%known_keywords(6) = "converge=               "
            tonto%known_keywords(7) = "damp_factor=            "
            tonto%known_keywords(8) = "damp_finish=            "
            tonto%known_keywords(9) = "dft_correlation=        "
            tonto%known_keywords(10) = "dft_exchange=           "
            tonto%known_keywords(11) = "diis=                   "
            tonto%known_keywords(12) = "diis_auto_start=        "
            tonto%known_keywords(13) = "diis_convergence=       "
            tonto%known_keywords(14) = "diis_keep=              "
            tonto%known_keywords(15) = "diis_start=             "
            tonto%known_keywords(16) = "direct=                 "
            tonto%known_keywords(17) = "dynamic_damping=        "
            tonto%known_keywords(18) = "eri_cutoff=             "
            tonto%known_keywords(19) = "eri_limit=              "
            tonto%known_keywords(20) = "fock_diis=              "
            tonto%known_keywords(21) = "guess=                  "
            tonto%known_keywords(22) = "initial_density=        "
            tonto%known_keywords(23) = "initial_guess=          "
            tonto%known_keywords(24) = "initial_lambda=         "
            tonto%known_keywords(25) = "initial_mos=            "
            tonto%known_keywords(26) = "kind=                   "
            tonto%known_keywords(27) = "lambda_max=             "
            tonto%known_keywords(28) = "lambda_min=             "
            tonto%known_keywords(29) = "lambda_step=            "
            tonto%known_keywords(30) = "level_shift=            "
            tonto%known_keywords(31) = "level_shift_finish=     "
            tonto%known_keywords(32) = "min_iterations=         "
            tonto%known_keywords(33) = "min_it=                 "
            tonto%known_keywords(34) = "max_iterations=         "
            tonto%known_keywords(35) = "max_it=                 "
            tonto%known_keywords(36) = "max_update_stepsize=    "
            tonto%known_keywords(37) = "mo_diis=                "
            tonto%known_keywords(38) = "mo_gradient_stepsize=   "
            tonto%known_keywords(39) = "nddo=                   "
            tonto%known_keywords(40) = "nudo=                   "
            tonto%known_keywords(41) = "output=                 "
            tonto%known_keywords(42) = "pie=                    "
            tonto%known_keywords(43) = "quantization_axis=      "
            tonto%known_keywords(44) = "rough_convergence=      "
            tonto%known_keywords(45) = "rough_diis_convergence= "
            tonto%known_keywords(46) = "scf_kind=               "
            tonto%known_keywords(47) = "scf_type=               "
            tonto%known_keywords(48) = "sl_1e_factor=           "
            tonto%known_keywords(49) = "sl_2e_factor=           "
            tonto%known_keywords(50) = "test=                   "
            tonto%known_keywords(51) = "use_1e_sl_term=         "
            tonto%known_keywords(52) = "use_1e_s(rxa)_term=     "
            tonto%known_keywords(53) = "use_1e_zora_term=       "
            tonto%known_keywords(54) = "use_2e_sl_term=         "
            tonto%known_keywords(55) = "use_aa_term=            "
            tonto%known_keywords(56) = "use_bl_term=            "
            tonto%known_keywords(57) = "use_bs_term=            "
            tonto%known_keywords(58) = "use_bs_t_term=          "
            tonto%known_keywords(59) = "use_damping=            "
            tonto%known_keywords(60) = "use_delta_build=        "
            tonto%known_keywords(61) = "use_diis=               "
            tonto%known_keywords(62) = "use_fock_diis=          "
            tonto%known_keywords(63) = "use_mo_diis=            "
            tonto%known_keywords(64) = "use_mo_gradient_update= "
            tonto%known_keywords(65) = "use_level_shifting=     "
            tonto%known_keywords(66) = "use_level_shift=        "
            tonto%known_keywords(67) = "use_rough_convergence=  "
            call unknown_(tonto,word,"SCFDATA:process_keyword")
            deallocate(tonto%known_keywords)
         end select
      end if

   end subroutine

   subroutine finalize(self)
    type(scfdata_type) :: self
    ! Make sure the input satisfies sanity checks and generate
    ! any other missing data

   call ensure_(tonto,self%scf_kind/=" ","SCFDATA:finalize ... no scf kind specified")
   call ensure_(tonto,self%max_iterations>=self%min_iterations,"SCFDATA:finalize ... max_it must be greater than min_it!")
      if (self%initial_mos/=" ")   self%initial_density = "--using MO's--"
      if (self%using_1e_zora_term) self%using_1e_sl_term = .false.
      if (self%using_1e_sl_term)   self%using_1e_zora_term = .false.
      if (self%scf_kind=="noninteracting-group-rhf") self%group = .true.
      if (.not. self%direct)         self%using_rough_convergence = .false.
      if (self%using_fock_diis .or. self%using_MO_diis) then
         call ensure_(tonto,self%diis%keep>0,"SCFDATA:finalize ... DIIS_keep must be > 0 for MO_gradient_update")
      end if
      if (self%using_MO_diis) then
         call set_archive_name_(self%diis,"DIIS_molecular_orbitals")
      else if (self%using_fock_diis) then
         call set_archive_name_(self%diis,"DIIS_fock_matrix")
      end if
      if (self%using_diis_auto_start) self%diis_start_iteration = self%max_iterations

   end subroutine

   subroutine read_dft_exchange(self)
    type(scfdata_type) :: self
    ! Read the SCF type

      call read_(stdin,self%dft_exchange)
      call to_lower_case_(self%dft_exchange)
      select case (self%dft_exchange)
         case("none                              "); self%dft_non_local_exchange=.false.
         case("slater                            "); self%dft_non_local_exchange=.false.
         case("xalpha                            "); self%dft_non_local_exchange=.false.
         case("becke88                           "); self%dft_non_local_exchange=.true.
         case("gill96                            "); self%dft_non_local_exchange=.true.
         case default;    allocate(tonto%known_keywords(5))
         tonto%known_keywords(1) = "none                              "
         tonto%known_keywords(2) = "slater                            "
         tonto%known_keywords(3) = "xalpha                            "
         tonto%known_keywords(4) = "becke88                           "
         tonto%known_keywords(5) = "gill96                            "
         call unknown_(tonto,self%dft_exchange,"SCFDATA:read_dft_exchange")
         deallocate(tonto%known_keywords)
      end select

   end subroutine

   subroutine read_dft_correlation(self)
    type(scfdata_type) :: self
    ! Read the SCF type

      call read_(stdin,self%dft_correlation)
      call to_lower_case_(self%dft_correlation)
      select case (self%dft_correlation)
         case("none                              "); self%dft_non_local_correlation=.false.
         case("vwn                               "); self%dft_non_local_correlation=.false.
         case("lyp                               "); self%dft_non_local_correlation=.true.
         case default;    allocate(tonto%known_keywords(3))
         tonto%known_keywords(1) = "none                              "
         tonto%known_keywords(2) = "vwn                               "
         tonto%known_keywords(3) = "lyp                               "
         call unknown_(tonto,self%dft_correlation,"SCFDATA:read_dft_correlation")
         deallocate(tonto%known_keywords)
      end select

   end subroutine

   subroutine read_kind(self)
    type(scfdata_type) :: self
    ! Read the SCF type

      call read_(stdin,self%scf_kind)
      select case (self%scf_kind)
         case("rhf                               ")
         case("rdft                              ")
         case("udft                              ")
         case("restricted_hartree_fock           ")
         case("xray_rhf                          ")
         case("xray_rdft                         ")
         case("xray_udft                         ")
         case("rohf                              ")
         case("restricted_open_shell_hartree_fock")
         case("uhf                               ")
         case("unrestricted_hartree_fock         ")
         case("ghf                               ")
         case("general_hartree_fock              ")
         case("rchf                              ")
         case("restricted_complex_hartree_fock   ")
         case("uchf                              ")
         case("unrestricted_complex_hartree_fock ")
         case("gchf                              ")
         case("general_complex_hartree_fock      ")
         case("noninteracting-group-rhf          ")
         case default;    allocate(tonto%known_keywords(20))
         tonto%known_keywords(1) = "rhf                               "
         tonto%known_keywords(2) = "rdft                              "
         tonto%known_keywords(3) = "udft                              "
         tonto%known_keywords(4) = "restricted_hartree_fock           "
         tonto%known_keywords(5) = "xray_rhf                          "
         tonto%known_keywords(6) = "xray_rdft                         "
         tonto%known_keywords(7) = "xray_udft                         "
         tonto%known_keywords(8) = "rohf                              "
         tonto%known_keywords(9) = "restricted_open_shell_hartree_fock"
         tonto%known_keywords(10) = "uhf                               "
         tonto%known_keywords(11) = "unrestricted_hartree_fock         "
         tonto%known_keywords(12) = "ghf                               "
         tonto%known_keywords(13) = "general_hartree_fock              "
         tonto%known_keywords(14) = "rchf                              "
         tonto%known_keywords(15) = "restricted_complex_hartree_fock   "
         tonto%known_keywords(16) = "uchf                              "
         tonto%known_keywords(17) = "unrestricted_complex_hartree_fock "
         tonto%known_keywords(18) = "gchf                              "
         tonto%known_keywords(19) = "general_complex_hartree_fock      "
         tonto%known_keywords(20) = "noninteracting-group-rhf          "
         call unknown_(tonto,self%scf_kind,"SCFDATA:read_kind")
         deallocate(tonto%known_keywords)
      end select

   end subroutine

   subroutine read_initial_density(self)
    type(scfdata_type) :: self
    ! Read the initial density guess

      call read_(stdin,self%initial_density)
      select case (self%initial_density)
         case("core                ")
         case("fock                ")
         case("atom                ")
         case("group               ")
         case("restricted          ")
         case("unrestricted        ")
         case("general             ")
         case("restricted_complex  ")
         case("complex_unrestricted")
         case("unrestricted_complex")
         case("general_complex     ")
         case("complex_general     ")
         case default;  allocate(tonto%known_keywords(12))
         tonto%known_keywords(1) = "core                "
         tonto%known_keywords(2) = "fock                "
         tonto%known_keywords(3) = "atom                "
         tonto%known_keywords(4) = "group               "
         tonto%known_keywords(5) = "restricted          "
         tonto%known_keywords(6) = "unrestricted        "
         tonto%known_keywords(7) = "general             "
         tonto%known_keywords(8) = "restricted_complex  "
         tonto%known_keywords(9) = "complex_unrestricted"
         tonto%known_keywords(10) = "unrestricted_complex"
         tonto%known_keywords(11) = "general_complex     "
         tonto%known_keywords(12) = "complex_general     "
         call unknown_(tonto,self%initial_density,"SCFDATA:read_initial_density")
         deallocate(tonto%known_keywords)
      end select
       ! User inputted guesses are usually converged, so stay accurate.
     ! .using_rough_convergence = .false.
     ! call warn_(tonto,"Rough convergence switched off by initial_density= option")

   end subroutine

   subroutine read_initial_mos(self)
    type(scfdata_type) :: self
    ! Read the initial density guess

      call read_(stdin,self%initial_mos)
      select case (self%initial_mos)
         case("restricted          ")
         case("unrestricted        ")
         case("general             ")
         case("restricted_complex  ")
         case("complex_unrestricted")
         case("unrestricted_complex")
         case("general_complex     ")
         case("complex_general     ")
         case default;   allocate(tonto%known_keywords(8))
         tonto%known_keywords(1) = "restricted          "
         tonto%known_keywords(2) = "unrestricted        "
         tonto%known_keywords(3) = "general             "
         tonto%known_keywords(4) = "restricted_complex  "
         tonto%known_keywords(5) = "complex_unrestricted"
         tonto%known_keywords(6) = "unrestricted_complex"
         tonto%known_keywords(7) = "general_complex     "
         tonto%known_keywords(8) = "complex_general     "
         call unknown_(tonto,self%initial_mos,"SCFDATA:read_initial_mos")
         deallocate(tonto%known_keywords)
      end select
       ! User inputted guesses are usually converged, so stay accurate.
      if (self%using_rough_convergence) then
        self%using_rough_convergence = .false.
        call warn_(tonto,"SCFDATA:read_initial_mos ... Rough convergence switched off by initial_mos= option")
      end if

   end subroutine

   subroutine read_min_iterations(self)
    type(scfdata_type) :: self
    ! Read the minimum no. of SCF interations

      call read_(stdin,self%min_iterations)
   call ensure_(tonto,self%min_iterations>=0,"SCFDATA:read_min_iterations ... min_iteration must be non-negative")

   end subroutine

   subroutine read_max_iterations(self)
    type(scfdata_type) :: self
    ! Read the maximum no. of SCF interations

      call read_(stdin,self%max_iterations)
   call ensure_(tonto,self%max_iterations>=0,"SCFDATA:read_max_iterations ... must be non-negative")
   call ensure_(tonto,self%max_iterations>=self%min_iterations,"SCFDATA:read_max_iterations ... smaller than min_iterations!"&
&)

   end subroutine

   subroutine read_convergence(self)
    type(scfdata_type) :: self
    ! Read the SCF convergence criteria

      call read_(stdin,self%convergence)
   call ensure_(tonto,self%convergence>0,"SCFDATA:read_convergence ... convergence must be positive")
      call warn_if_(tonto,self%convergence<10.0d0**(-11),"SCFDATA:read_convergence ... convergence may be too small")

   end subroutine

   subroutine read_rough_convergence(self)
    type(scfdata_type) :: self
    ! Read the rough SCF convergence criteria

      call read_(stdin,self%rough_convergence)
   call ensure_(tonto,self%rough_convergence>0,"SCFDATA:read_rough_convergence ... must be positive")
   call ensure_(tonto,self%rough_convergence>self%convergence,"SCFDATA:read_rough_convergence ... smaller than convergence!")
      call warn_if_(tonto,self%rough_convergence<10.0d0**(-11),"SCFDATA:read_rough_convergence ... may be too small")

   end subroutine

   subroutine read_fock_diis(self)
    type(scfdata_type) :: self
    ! Read whether to use type(diis_type) for fock matrix

      call read_(stdin,self%using_fock_diis)
      if (self%using_fock_diis) self%using_MO_diis = .false.

   end subroutine

   subroutine read_MO_diis(self)
    type(scfdata_type) :: self
    ! Read whether to use type(diis_type) for molecular orbitals

      call read_(stdin,self%using_MO_diis)
      if (self%using_MO_diis) self%using_fock_diis = .false.

   end subroutine

   subroutine read_diis_convergence(self)
    type(scfdata_type) :: self
    ! Read the type(diis_type) SCF convergence criteria

      call read_(stdin,self%diis_convergence)
   call ensure_(tonto,self%diis_convergence>0,"SCFDATA:read_diis_convergence ... must be positive")
      call warn_if_(tonto,self%diis_convergence<10.0d0**(-11),"SCFDATA:read_diis_convergence ... may be too small")

   end subroutine

   subroutine read_diis_keep(self)
    type(scfdata_type) :: self
    ! Read the number of type(diis_type) vectors to keep
     integer(kind=kind(1)) :: i

     call read_(stdin,i)
     call set_keep_(self%diis,i)

   end subroutine

   subroutine read_diis_start(self)
    type(scfdata_type) :: self
    ! Read when to start type(diis_type)
     integer(kind=kind(1)) :: i

     call read_(stdin,i)
     self%diis_start_iteration = i

   end subroutine

   subroutine read_diis_auto_start(self)
    type(scfdata_type) :: self
    ! Read whether to start type(diis_type) automatically based on the diis error

      call read_(stdin,self%using_diis_auto_start)
     self%diis_start_iteration = self%max_iterations

   end subroutine

   subroutine read_rough_diis_convergence(self)
    type(scfdata_type) :: self
    ! Read the rough type(diis_type) SCF convergence criteria

      call read_(stdin,self%rough_diis_convergence)
   call ensure_(tonto,self%rough_diis_convergence>0,"SCFDATA:read_rough_diis_convergence ... must be positive")
   call ensure_(tonto,self%rough_diis_convergence>self%diis_convergence,"SCFDATA:read_rough_diis_convergence ... too small")
   call warn_if_(tonto,self%rough_diis_convergence<10.0d0**(-11),"SCFDATA:read_rough_diis_convergence ... may be too small")

   end subroutine

   subroutine read_direct(self)
    type(scfdata_type) :: self
    ! Read whether to use direct SCF or not

      call read_(stdin,self%direct)
      if (self%direct) self%using_delta_build = .true.

   end subroutine

   subroutine read_delta_build(self)
    type(scfdata_type) :: self
    ! Read whether to use incremental fock build

      call read_(stdin,self%using_delta_build)

   end subroutine

   subroutine read_MO_gradient_update(self)
    type(scfdata_type) :: self
    ! Read whether to an MO gradient update method

      call read_(stdin,self%using_MO_gradient_update)

   end subroutine

   subroutine read_eri_limit(self)
    type(scfdata_type) :: self
    ! Read the ERI cutoff limit

      call read_(stdin,self%eri_limit)

   end subroutine

!  *****
!  Tests
!  *****

   function scf_done(self) result(res)
    type(scfdata_type) :: self
    ! Return .true. if the scf procedure is done
      logical(kind=kind(.true.)) :: res

      res = (converged_(self) .or. exceeded_max_it_(self)) .and. exceeded_min_it_(self)

   end function

   function converged(self) result(res)
    type(scfdata_type) :: self
    ! Return .true. if the type(scfdata_type) appears to be converged
      logical(kind=kind(.true.)) :: res

      res = diff_converged_(self) .and. diis_converged_(self) &
          .and. .not. apply_rough_convergence_(self)  ! must use full accuracy integrals

   end function

   function roughly_converged(self) result(res)
    type(scfdata_type) :: self
    ! Return .true. if the type(scfdata_type) is roughly converged
      logical(kind=kind(.true.)) :: res

     res = abs(self%difference) < self%rough_convergence .and. &
           abs(self%diis_error) < self%rough_diis_convergence

   end function

   function apply_rough_convergence(self) result(res)
    type(scfdata_type) :: self
    ! Return .true. if applying rough integral convergence this iteration
     logical(kind=kind(.true.)) :: res

     res = self%using_rough_convergence .and. self%direct
     res = res .and. (.not. roughly_converged_(self)) .and. self%lambda_iteration==0

   end function

   function diff_converged(self) result(res)
    type(scfdata_type) :: self
    ! Return .true. if the energy difference has converged
      logical(kind=kind(.true.)) :: res

      if (fitting_(self)) then
        res = abs(self%fit_value-self%old_fit_value) < self%convergence
      else
        res = abs(self%difference) < self%convergence
      end if

   end function

   function diis_converged(self) result(res)
    type(scfdata_type) :: self
    ! Return .true. if the gradient/type(diis_type) error has converged
      logical(kind=kind(.true.)) :: res

      res = abs(self%diis_error) < self%diis_convergence

   end function

   function exceeded_max_it(self) result(res)
    type(scfdata_type) :: self
    ! Return .true. if the type(scfdata_type) has exceeded the maximum iterations
      logical(kind=kind(.true.)) :: res

      res = self%iteration >= self%max_iterations

   end function

   function exceeded_min_it(self) result(res)
    type(scfdata_type) :: self
    ! Return .true. if the type(scfdata_type) has exceeded the minimum iterations
      logical(kind=kind(.true.)) :: res

      res = self%iteration >= self%min_iterations
      if (fitting_(self)) res = res .and. (self%iteration > 1)

   end function

   function exceeded_lambda_max(self) result(res)
    type(scfdata_type) :: self
    ! Return .true. if the type(scfdata_type) has exceeded the maximum lambda
      logical(kind=kind(.true.)) :: res

      res = self%lambda > (1.0d0+10.0d0**(-10)) * self%lambda_max
                          ! 10.0d0**(-10) allows for roundoff errors
      if (self%lambda_step < 10.0d0**(-10)) res = .true.

   end function

   function eri_cutoff(self) result(res)
    type(scfdata_type) :: self
    ! Return a value to eliminate small integrals in direct SCF calculations
     real(kind=kind(1.0d0)) :: res

     if (apply_rough_convergence_(self)) then
        res = 1.0d-7
     else
        res = self%eri_limit
     end if

   end function

   function eri_cutoff_altered(self) result(res)
    type(scfdata_type) :: self
    ! Return .true. if the eri_cutoff has changed since the last .update.
    ! This function is needed for recreating the fock matrix where
    ! incremental builds are used.
      logical(kind=kind(.true.)) :: res

      res = self%old_eri_cutoff/=eri_cutoff_(self)

   end function

   function do_delta_build(self) result(res)
    type(scfdata_type) :: self
    ! Return .true. if a delta fock matrix build is allowed (assuming that the old
    ! fock matrix and old density matrix are available)
      logical(kind=kind(.true.)) :: res

      res = self%using_delta_build .and. .not. eri_cutoff_altered_(self)
      res = res .and. .not. includes_(self%scf_kind,"dft")

   end function

!  **********************
!  type(diis_type) tests. Be careful
!  **********************

   function apply_fock_diis(self) result(res)
    type(scfdata_type) :: self
    ! Return .true. if type(diis_type) extrapolation is to be used for extrapolating
    ! the fock matrix (this is the default). Currently the only alternative
    ! is gradient extrapolation of the orbitals.
      logical(kind=kind(.true.)) :: res

      res = apply_diis_(self) .and. self%using_fock_diis

   end function

   function apply_MO_diis(self) result(res)
    type(scfdata_type) :: self
    ! Return .true. if type(diis_type) extrapolation is to be used for extrapolating
    ! the fock matrix (this is the default). Currently the only alternative
    ! is gradient extrapolation of the orbitals.
      logical(kind=kind(.true.)) :: res

      res = apply_diis_(self) .and. self%using_MO_diis

   end function

   function using_diis(self) result(res)
    type(scfdata_type) :: self
    ! Return .true. if type(diis_type) extrapolation is to be used
      logical(kind=kind(.true.)) :: res

      res = self%using_fock_diis .or. self%using_MO_diis

   end function

   function diis_used(self) result(res)
    type(scfdata_type) :: self
    ! Return .true. if type(diis_type) extrapolation has *really* been used this iteration
    ! (The first time doesn't really count, see apply_diis below for that case)
      logical(kind=kind(.true.)) :: res

      if (.not. using_diis_(self)) then
         res = .false.
      else
         res = (self%total_iterations > self%diis_start_iteration) .and. self%diis%keep > 1
      end if

   end function

   function apply_diis(self) result(res)
    type(scfdata_type) :: self
    ! Return .true. if type(diis_type) extrapolation must be applied this iteration,
    ! or has been applied this iteration.
      logical(kind=kind(.true.)) :: res

      if (.not. using_diis_(self)) then
         res = .false.
      else
         res = (self%total_iterations >= self%diis_start_iteration) .and. self%diis%keep > 1
      end if
!      ! Do not have diis at lambda=0 between lambda increments.
!      if (.fitting) then
!        if (.iteration==0 .and. .lambda_iteration>0) res = .false.
!      end

   end function

   function apply_camp_king(self) result(res)
    type(scfdata_type) :: self
    ! Return .true. if Camp-King converger is to be used this iteration
     logical(kind=kind(.true.)) :: res
!    res = .using_camp_king .and. (.diis_error > .old_diis_error)

     res = self%using_camp_king .and. spinorbital_kind_(self) == "restricted"

   end function

   function apply_dynamic_damping(self) result(res)
    type(scfdata_type) :: self
    ! Return .true. if Camp-King converger is to be used this iteration
     logical(kind=kind(.true.)) :: res

     res = self%using_dynamic_damping .and. .not. apply_damping_(self)

   end function

   function apply_damping(self) result(res)
    type(scfdata_type) :: self
    ! Return .true. if density matrix damping is to be applied this iteration
      logical(kind=kind(.true.)) :: res

      res = self%using_damping .and. self%iteration < self%damp_finish

   end function

   function apply_level_shifting(self) result(res)
    type(scfdata_type) :: self
    ! Return .true. if level shifting must be applied this iteration
      logical(kind=kind(.true.)) :: res

      res = .not. is_zero_(self%level_shift) &
            .and. self%using_level_shift .and. self%iteration < self%level_shift_finish

   end function

   function fitting(self) result(res)
    type(scfdata_type) :: self
    ! Return true if we are fitting the wavefunction.
     logical(kind=kind(.true.)) :: res

     select case (self%scf_kind)
       case("xray_rhf","xray_restricted_hartree_fock");   res = .true.
       case("xray_rks");                                  res = .true.
       case("xray_rdft");                                 res = .true.
       case("xray_udft");                                 res = .true.
       case default;                                      res = .false.
     end select

   end function

  !  ***************
  !  Output routines
  !  ***************

   subroutine put_banner(self)
    type(scfdata_type) :: self
    ! Prints out the nuclear energy and initial guess energy.

     if (.not. self%output) then;   return; end if
     call flush_(stdout)
     call text_(stdout,"***************")
     call text_(stdout,"SCF calculation")
     call text_(stdout,"***************")
     call flush_(stdout)
     call put_summary_(self)

   end subroutine

   subroutine put_summary(self)
    type(scfdata_type) :: self
    ! Prints out a summary of what is stored in the scfdata object.
     real(kind=kind(1.0d0)), dimension(3) :: q
     logical(kind=kind(.true.)) :: real_width

     real_width = .true.
     call show_(stdout,"SCF kind                    = ", self%scf_kind)
     if (includes_(self%scf_kind,"dft")) then
     call show_(stdout,"DFT Exchange                = ", self%dft_exchange)
     call show_(stdout,"DFT Correlation             = ", self%dft_correlation)
     end if
     call show_(stdout,"Direct                      = ", self%direct,real_width)
     call set_real_style_(stdout,"e")
     call show_(stdout,"Integral cutoff             = ", self%eri_limit)
     call set_real_style_(stdout,"f")
     if (self%nddo) &
     call show_(stdout,"NDDO                        = ", self%nddo,real_width)
     call show_(stdout,"ZORA (1 electron) terms     = ", self%using_1e_zora_term,real_width)
     call flush_(stdout)
     call text_(stdout,"Initial guess options:")
     call flush_(stdout)
     call show_(stdout,"Initial density             = ", self%initial_density)
     call show_(stdout,"Initial MO's                = ", self%initial_mos)
     call flush_(stdout)
     call text_(stdout,"Initial guess energies:")
     call flush_(stdout)
     call show_(stdout,"Nuclear Energy              = ", self%nuclear_energy)
     call show_(stdout,"SCF Energy                  = ", self%energy)
     call show_(stdout,"Kinetic Energy              = ", self%kinetic_energy)
     if (includes_(self%scf_kind,"dft")) then
     call show_(stdout,"DFT Energy                  = ", self%energy+self%dft_energy_correction)
     end if
     call flush_(stdout)
     call text_(stdout,"SCF termination criteria:")
     call flush_(stdout)
     call show_(stdout,"Convergence                 = ", self%convergence)
     call show_(stdout,"Gradient/DIIS convergence   = ", self%diis_convergence)
     call show_(stdout,"Minimum iterations          = ", self%min_iterations,real_width)
     call show_(stdout,"Maximum iterations          = ", self%max_iterations,real_width)
     call flush_(stdout)
     call text_(stdout,"Convergence acceleration options:")
     call flush_(stdout)
     call show_(stdout,"Using Rough Convergence     = ", self%using_rough_convergence,real_width)
     if (self%using_rough_convergence) then
     call show_(stdout,"Rough Convergence           = ", self%rough_convergence)
     call show_(stdout,"Rough DIIS Convergence      = ", self%rough_diis_convergence)
     end if
     call show_(stdout,"Using level shift           = ", self%using_level_shift,real_width)
     if (self%using_level_shift) then
     call show_(stdout,"Level shift                 = ", self%level_shift)
     call show_(stdout,"Level shift  quits at       = ", self%level_shift_finish,real_width)
     end if
     call show_(stdout,"Using density damping       = ", self%using_damping,real_width)
     if (self%using_damping) then
     call show_(stdout,"Damping factor              = ", self%damp_factor)
     call show_(stdout,"Damping quits at            = ", self%damp_finish,real_width)
     end if
     call show_(stdout,"Using MO gradient update    = ", self%using_MO_gradient_update,real_width)
     if (self%using_MO_gradient_update) then
     call show_(stdout,"MO gradient stepsize        = ", self%MO_gradient_stepsize)
     call show_(stdout,"Maximum update stepsize     = ", self%max_update_stepsize)
     end if
     if (self%using_dynamic_damping) then
     call show_(stdout,"Using Dynamic Damping       = ", self%using_dynamic_damping,real_width)
     end if
     if (self%using_camp_king) then
     call show_(stdout,"Using Camp-King             = ", self%using_camp_king)
     end if
     call show_(stdout,"Using DIIS                  = ", using_diis_(self),real_width)
     call show_(stdout,"Using Fock DIIS?            = ", self%using_fock_diis,real_width)
     call show_(stdout,"Using MO DIIS?              = ", self%using_MO_diis,real_width)
     if (using_diis_(self)) then
     call show_(stdout,"DIIS archive root name      = ", self%diis%archive%root_name)
     call show_(stdout,"DIIS no. to keep            = ", self%diis%keep,real_width)
     call show_(stdout,"DIIS automatic start?       = ", self%using_diis_auto_start,real_width)
     if (.not. self%using_diis_auto_start) then
     call show_(stdout,"DIIS start iteration        = ", self%diis_start_iteration,real_width)
     end if
     end if
     if (spinorbital_kind_(self)=="general_complex") then
       call flush_(stdout)
       call text_(stdout,"Magnetic/Relativistic terms:")
       call flush_(stdout)
       q = self%quantization_axis
       call show_(stdout,"Quantization axis           = ", q(1),q(2),q(3))
       call show_(stdout,"ZORA (1 electron) terms     = ", self%using_1e_zora_term)
       call show_(stdout,"Using B:L term              = ", self%using_bl_term)
       call show_(stdout,"Using B:S term              = ", self%using_bs_term)
       call show_(stdout,"Using B:S T term            = ", self%using_bs_t_term)
       call show_(stdout,"Using A:A term              = ", self%using_aa_term)
       call show_(stdout,"Using 1e S:L term           = ", self%using_1e_sl_term)
       call show_(stdout,"Using 1e S:(rxA) term       = ", self%using_1e_srxa_term)
       call show_(stdout,"Using 2e S:L term           = ", self%using_2e_sl_term)
       call show_(stdout,"Factor for 1e S:L term      = ", self%sl_1e_factor)
       call show_(stdout,"Factor for 2e S:L term      = ", self%sl_2e_factor)
     end if
     if (fitting_(self)) then
       call flush_(stdout)
       call text_(stdout,"Experimental wavefunction parameters:")
       call flush_(stdout)
       call show_(stdout,"Lambda fitting parameter    = ", self%lambda)
       call show_(stdout,"Lambda max                  = ", self%lambda_max)
       call show_(stdout,"Lambda step                 = ", self%lambda_step)
       call put_crystal_(self)
     end if

   end subroutine

   subroutine put_table_head(self)
    type(scfdata_type) :: self
    ! Prints out the table head for an SCF calculation
     integer(kind=kind(1)) :: fields

     if (.not. self%output) then;   return; end if
     fields = 3
     if (fitting_(self))     fields = fields + 2
     call flush_(stdout)
     call dash_(stdout,real_fields=fields,int_fields=1)
     call put_(stdout,"Iter",int_width=.true.)
     if (fitting_(self)) then
       call put_(stdout,"lambda")
       call put_(stdout,"F_chi2")
     end if
     call put_(stdout,"Energy")
     call flush_(stdout)
     call dash_(stdout,real_fields=fields,int_fields=1)

   end subroutine

   subroutine put_results(self)
    type(scfdata_type) :: self
    ! Print out the results for the current iteration. This routine must be
    ! compatible with put_banner. This routine must be called at iteration 0.
     integer(kind=kind(1)) :: fields,n,i,margin_pos
     logical(kind=kind(.true.)), save :: diis_converged,diff_converged,damping_on,damping_off
     logical(kind=kind(.true.)), save :: level_on,level_off,diis_on,diis_up,rough_off
     character(128), dimension(20) :: info

     if (.not. self%output) then;   return; end if
      ! This is the table head ...
     if (self%iteration == 0) then
        if (self%lambda_iteration == 0) call put_table_head_(self)
        diff_converged = .false.
        rough_off = .false.
        diis_converged = .false.
        damping_on = .false.
        damping_off = .false.
        level_on = .false.
        level_off = .false.
        diis_on = .false.
        diis_up = .false.
  !       if (.lambda_iteration > 0) return
     end if
     call put_(stdout,self%iteration)
     fields = 3
     if (fitting_(self)) then
        fields = fields + 2
        call put_(stdout,self%lambda)
        call put_(stdout,self%F_chi2)
     end if
      ! This is the important info ...
     call put_(stdout,self%energy)
      ! Margin notes ...
     if (.not. scf_done_(self)) then
        info = " "
        n = 0
        if (.not. diff_converged .and. diff_converged_(self) .and. self%iteration>0) then
          n = n + 1
          info(n) = " *Difference has converged"
          diff_converged = .true.
        end if
        if (.not. diis_converged .and. diis_converged_(self)) then
          n = n + 1
          info(n) = " *Gradient has converged"
          diis_converged = .true.
        end if
        if (.not. rough_off .and. .not. apply_rough_convergence_(self) .and. self%using_rough_convergence) then
           n = n + 1
           info(n) = " *Increasing integral accuracy"
           rough_off = .true.
        end if
        if (apply_damping_(self) .and. .not. damping_on) then
           n = n + 1
           info(n) = " *Damping on"
           damping_on = .true.
        else if (.not. apply_damping_(self) .and. .not. damping_off) then
           n = n + 1
           info(n) = " *Damping off"
           damping_off = .true.
        end if
        if (apply_level_shifting_(self) .and. .not. level_on) then
           n = n + 1
           info(n) = " *Levelshift on"
           level_on = .true.
        else if (.not. apply_level_shifting_(self) .and. .not. level_off) then
           n = n + 1
           info(n) = " *Levelshift off"
           level_off = .true.
        end if
        if ((using_diis_(self) .and. .not. diis_on .and. apply_diis_(self))) then
           n = n + 1
           info(n) = " *DIIS on"
           diis_on = .true.
        end if
        if (.not. diis_up .and. apply_diis_(self) .and. self%diis%n_vec==self%diis%keep) then
           n = n + 1
           info(n) = " *DIIS subspace saturated"
           diis_up = .true.
        end if
        if (self%camp_king_iters>0) then
           n = n + 1
           info(n) = " *Camp-King iterations = " // trim(to_str_(self%camp_king_iters))
        end if
        if (self%using_dynamic_damping .and. self%dynamic_damp_factor > 10.0d0**(-7)) then
           n = n + 1
           info(n) = " *damp factor = " // &
                     trim(to_str_no_zeros_(self%dynamic_damp_factor,"f8.6"))
        end if
        margin_pos = stdout%buffer%item_end
        do i = 1,n
          if (i>1) then
          call flush_(stdout)
          call tab_(stdout,width=margin_pos)
          end if
          call put_text_(stdout,trim(info(i)))
        end do
        call flush_(stdout)
      ! This is the table foot ...
     else
        call put_table_foot_(self)
     end if

   end subroutine

   subroutine put_table_foot(self)
    type(scfdata_type) :: self
    ! Prints out the table foot for an SCF calculation, after convergence
    ! or not as the case may be
     integer(kind=kind(1)) :: fields

     if (.not. self%output) then;   return; end if
     fields = 3
     if (fitting_(self))     fields = fields + 2
     call flush_(stdout)
     call dash_(stdout,real_fields=fields,int_fields=1)
     if (converged_(self)) then; call text_(stdout,"* * * SCF has converged * * *",flush=1)
     else;                 call text_(stdout,"* * * SCF has not converged * * *",flush=1)
     end if
     call dash_(stdout,real_fields=fields,int_fields=1)
     call show_(stdout,"SCF Energy                  = ", self%energy)
     call show_(stdout,"Kinetic Energy              = ", self%kinetic_energy)
     if (includes_(self%scf_kind,"dft")) then
     call show_(stdout,"DFT Energy                  = ", self%energy+self%dft_energy_correction)
     end if
     if (fitting_(self)) call put_crystal_(self)
     call dash_(stdout,real_fields=fields,int_fields=1)

   end subroutine

   subroutine put_crystal(self)
    type(scfdata_type) :: self
    ! Prints out the crystal structure factor statistics.

     call show_(stdout,"Chi^2 in F                  = ", self%F_chi2)
     call show_(stdout,"Goodness of fit in F        = ", self%F_gof)
     call show_(stdout,"R factor in F               = ", self%F_r_factor)
     call show_(stdout,"Weighted R factor in F      = ", self%F_weighted_r_factor)

   end subroutine

   subroutine put_constrained_scf_results(self,out)
    type(scfdata_type) :: self
    ! Outputs SCF information to a file, which is useful for constrained
    ! Hartree-Fock methods where you want to view the effects of the constraint
    ! Lagrange multiplier.
     type(textfile_type) :: out

     call put_(out,self%lambda)
     call put_(out,self%F_chi2)
     call put_(out,self%energy)
     call put_(out,self%kinetic_energy)
     call put_(out,self%F_weighted_r_factor)

   end subroutine

end
