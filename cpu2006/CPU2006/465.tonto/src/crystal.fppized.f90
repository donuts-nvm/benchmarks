!-------------------------------------------------------------------------------
!
! CRYSTAL: Data structure for crystals
!
! $Id: crystal.foo,v 1.86.2.23 2004/04/21 09:12:54 reaper Exp $
!
! Copyright (C) Dylan Jayatilaka, Daniel Grimwood, 1999
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
!-------------------------------------------------------------------------------

module CRYSTAL_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

   use REFLECTION_MODULE, only: F_r_
   use REFLECTION_MODULE, only: F_z_
   use REFLECTION_MODULE, only: F_z2_

   use CIF_MODULE, only: open_
   use CIF_MODULE, only: read_looped_item_
   use CIF_MODULE, only: create_
   use CIF_MODULE, only: find_looped_item_
   use CIF_MODULE, only: destroy_
   use CIF_MODULE, only: find_crystal_data_block_

   use INTVEC_MODULE, only: shrink_
   use INTVEC_MODULE, only: create_
   use INTVEC_MODULE, only: expand_
   use INTVEC_MODULE, only: create_copy_
   use INTVEC_MODULE, only: destroy_

   use BINVEC_MODULE, only: create_
   use BINVEC_MODULE, only: destroy_

   use CPXMAT_MODULE, only: create_
   use CPXMAT_MODULE, only: destroy_

   use REAL_MODULE, only: is_zero_
   use REAL_MODULE, only: to_str_

   use CPXMAT3_MODULE, only: create_
   use CPXMAT3_MODULE, only: destroy_

   use REALVEC_MODULE, only: same_as_
   use REALVEC_MODULE, only: create_
   use REALVEC_MODULE, only: to_product_of_
   use REALVEC_MODULE, only: create_copy_
   use REALVEC_MODULE, only: destroy_

   use SPACEGROUP_MODULE, only: put_
   use SPACEGROUP_MODULE, only: read_CIF_
   use SPACEGROUP_MODULE, only: set_defaults_
   use SPACEGROUP_MODULE, only: nullify_ptr_part_
   use SPACEGROUP_MODULE, only: destroy_ptr_part_
   use SPACEGROUP_MODULE, only: read_keywords_
   use SPACEGROUP_MODULE, only: analyse_
   use SPACEGROUP_MODULE, only: copy_

   use INT_MODULE, only: to_str_

   use ATOM_MODULE, only: chemical_symbol_

   use UNITCELL_MODULE, only: change_from_fractional_
   use UNITCELL_MODULE, only: read_CIF_
   use UNITCELL_MODULE, only: set_defaults_
   use UNITCELL_MODULE, only: put_CX_
   use UNITCELL_MODULE, only: put_
   use UNITCELL_MODULE, only: make_info_
   use UNITCELL_MODULE, only: read_keywords_
   use UNITCELL_MODULE, only: copy_
   use UNITCELL_MODULE, only: change_into_fractional_

   use REFLECTIONVEC_MODULE, only: scale_F_exp_
   use REFLECTIONVEC_MODULE, only: scale_F_sigma_
   use REFLECTIONVEC_MODULE, only: have_indices_
   use REFLECTIONVEC_MODULE, only: set_F_pred_
   use REFLECTIONVEC_MODULE, only: have_F_sigma_
   use REFLECTIONVEC_MODULE, only: have_F_exp_
   use REFLECTIONVEC_MODULE, only: simulate_new_F_exp_
   use REFLECTIONVEC_MODULE, only: n_refl_
   use REFLECTIONVEC_MODULE, only: have_F_calc_
   use REFLECTIONVEC_MODULE, only: F_weighted_r_factor_
   use REFLECTIONVEC_MODULE, only: put_
   use REFLECTIONVEC_MODULE, only: put_I_stats_
   use REFLECTIONVEC_MODULE, only: I_r_factor_
   use REFLECTIONVEC_MODULE, only: F_r_factor_
   use REFLECTIONVEC_MODULE, only: read_list_keywords_
   use REFLECTIONVEC_MODULE, only: set_F_exp_
   use REFLECTIONVEC_MODULE, only: have_I_pred_
   use REFLECTIONVEC_MODULE, only: clear_keys_
   use REFLECTIONVEC_MODULE, only: I_goodness_of_fit_
   use REFLECTIONVEC_MODULE, only: put_F_stats_
   use REFLECTIONVEC_MODULE, only: create_
   use REFLECTIONVEC_MODULE, only: put_F_qq_plot_
   use REFLECTIONVEC_MODULE, only: create_copy_
   use REFLECTIONVEC_MODULE, only: indices_
   use REFLECTIONVEC_MODULE, only: set_keys_
   use REFLECTIONVEC_MODULE, only: set_F_sigma_
   use REFLECTIONVEC_MODULE, only: I_chi2_
   use REFLECTIONVEC_MODULE, only: set_F_calc_
   use REFLECTIONVEC_MODULE, only: I_weighted_r_factor_
   use REFLECTIONVEC_MODULE, only: destroy_
   use REFLECTIONVEC_MODULE, only: have_F_pred_
   use REFLECTIONVEC_MODULE, only: F_goodness_of_fit_
   use REFLECTIONVEC_MODULE, only: put_labelled_F_qq_plot_
   use REFLECTIONVEC_MODULE, only: F_chi2_

   use REALMAT3_MODULE, only: create_

   use STR_MODULE, only: to_lower_case_

   use TEXTFILE_MODULE, only: stdin
   use TEXTFILE_MODULE, only: stdout
   use TEXTFILE_MODULE, only: set_default_units_
   use TEXTFILE_MODULE, only: text_
   use TEXTFILE_MODULE, only: next_str_
   use TEXTFILE_MODULE, only: skip_next_item_
   use TEXTFILE_MODULE, only: next_item_
   use TEXTFILE_MODULE, only: show_
   use TEXTFILE_MODULE, only: put_
   use TEXTFILE_MODULE, only: tab_
   use TEXTFILE_MODULE, only: read_
   use TEXTFILE_MODULE, only: flush_
   use TEXTFILE_MODULE, only: reverted_
   use TEXTFILE_MODULE, only: read_ptr_
   use TEXTFILE_MODULE, only: dash_

   use STRVEC_MODULE, only: destroy_

   use ATOMVEC_MODULE, only: get_geometry_

   use ARCHIVE_MODULE, only: read_
   use ARCHIVE_MODULE, only: set_

   use REALMAT_MODULE, only: shrink_columns_
   use REALMAT_MODULE, only: equals_
   use REALMAT_MODULE, only: max_abs_column_difference_
   use REALMAT_MODULE, only: create_
   use REALMAT_MODULE, only: expand_
   use REALMAT_MODULE, only: create_copy_
   use REALMAT_MODULE, only: destroy_
   use REALMAT_MODULE, only: has_column_

   use CPXVEC_MODULE, only: create_
   use CPXVEC_MODULE, only: destroy_
   use REALVEC_MODULE, only: minimise_BFGS

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

   private    put_inv_trans_symop_data_
   interface put_inv_trans_symop_data_
      module procedure put_inv_trans_symop_data
   end interface

   private    read_thermal_smearing_model_
   interface read_thermal_smearing_model_
      module procedure read_thermal_smearing_model
   end interface

   private    get_optimum_parameters_
   interface get_optimum_parameters_
      module procedure get_optimum_parameters
   end interface

   public    lp_factor_
   interface lp_factor_
      module procedure lp_factor
   end interface

   private    put_to_unit_cell_
   interface put_to_unit_cell_
      module procedure put_to_unit_cell
      module procedure put_to_unit_cell_1
   end interface

   public    put_CX_
   interface put_CX_
      module procedure put_CX
   end interface

   public    have_F_sigma_
   interface have_F_sigma_
      module procedure have_F_sigma
   end interface

   public    read_kind_
   interface read_kind_
      module procedure read_kind
   end interface

   public    fragment_data_exists_
   interface fragment_data_exists_
      module procedure fragment_data_exists
   end interface

   public    destroy_reflection_data_
   interface destroy_reflection_data_
      module procedure destroy_reflection_data
   end interface

   private    put_reduced_symop_data_
   interface put_reduced_symop_data_
      module procedure put_reduced_symop_data
   end interface

   public    d_chi2_dU_
   interface d_chi2_dU_
      module procedure d_chi2_dU
   end interface

   public    read_keywords_
   interface read_keywords_
      module procedure read_keywords
   end interface

   public    have_F_exp_
   interface have_F_exp_
      module procedure have_F_exp
   end interface

   public    make_reduced_group_data_
   interface make_reduced_group_data_
      module procedure make_reduced_group_data
   end interface

   private    is_same_geometry_
   interface is_same_geometry_
      module procedure is_same_geometry
   end interface

   public    put_PND_sf_
   interface put_PND_sf_
      module procedure put_PND_sf
   end interface

   public    put_correction_data_
   interface put_correction_data_
      module procedure put_correction_data
   end interface

   private    read_partition_model_
   interface read_partition_model_
      module procedure read_partition_model
   end interface

   public    F_exp_
   interface F_exp_
      module procedure F_exp
   end interface

   private    extinction_angle_part_
   interface extinction_angle_part_
      module procedure extinction_angle_part
   end interface

   public    copy_
   interface copy_
      module procedure copy
   end interface

   private    make_inverted_symops_
   interface make_inverted_symops_
      module procedure make_inverted_symops
   end interface

   public    I_sigma_
   interface I_sigma_
      module procedure I_sigma
   end interface

   public    sum_ft_r_ints_
   interface sum_ft_r_ints_
      module procedure sum_ft_r_ints
   end interface

   public    put_reflection_data_
   interface put_reflection_data_
      module procedure put_reflection_data
   end interface

   public    transform_geometry_
   interface transform_geometry_
      module procedure transform_geometry
   end interface

   private    read_CIF_atoms_
   interface read_CIF_atoms_
      module procedure read_CIF_atoms
   end interface

   public    F_sigma_
   interface F_sigma_
      module procedure F_sigma
   end interface

   public    make_unique_SF_k_pts_
   interface make_unique_SF_k_pts_
      module procedure make_unique_SF_k_pts
   end interface

   public    I_weighted_r_factor_
   interface I_weighted_r_factor_
      module procedure I_weighted_r_factor
   end interface

   public    destroy_
   interface destroy_
      module procedure destroy
   end interface

   public    read_spacegroup_
   interface read_spacegroup_
      module procedure read_spacegroup
   end interface

   private    put_asymmetric_unit_geometry_
   interface put_asymmetric_unit_geometry_
      module procedure put_asymmetric_unit_geometry
   end interface

   public    chi2_
   interface chi2_
      module procedure chi2
   end interface

   private    read_exp_scale_factor_
   interface read_exp_scale_factor_
      module procedure read_exp_scale_factor
   end interface

   public    F_pred_
   interface F_pred_
      module procedure F_pred
   end interface

   private    read_optimise_extinction_
   interface read_optimise_extinction_
      module procedure read_optimise_extinction
   end interface

   public    make_F_predicted_
   interface make_F_predicted_
      module procedure make_F_predicted
   end interface

   private    make_phased_matrix_for_symop_
   interface make_phased_matrix_for_symop_
      module procedure make_phased_matrix_for_symop
   end interface

   public    read_units_
   interface read_units_
      module procedure read_units
   end interface

   public    put_F_calc_
   interface put_F_calc_
      module procedure put_F_calc
   end interface

   public    make_k_pts_
   interface make_k_pts_
      module procedure make_k_pts
   end interface

   public    read_CIF_
   interface read_CIF_
      module procedure read_CIF
      module procedure read_CIF_1
   end interface

   public    sum_unique_sf_ints_
   interface sum_unique_sf_ints_
      module procedure sum_unique_sf_ints
   end interface

   public    update_
   interface update_
      module procedure update
   end interface

   private    make_repetition_factors_
   interface make_repetition_factors_
      module procedure make_repetition_factors
   end interface

   public    destroy_ptr_part_
   interface destroy_ptr_part_
      module procedure destroy_ptr_part
   end interface

   private    put_unique_fragment_geometry_
   interface put_unique_fragment_geometry_
      module procedure put_unique_fragment_geometry
   end interface

   public    d_chi2_d_ext_
   interface d_chi2_d_ext_
      module procedure d_chi2_d_ext
   end interface

   public    sum_unique_sf_deriv_U_
   interface sum_unique_sf_deriv_U_
      module procedure sum_unique_sf_deriv_U
   end interface

   public    have_F_calc_
   interface have_F_calc_
      module procedure have_F_calc
   end interface

   private    unique_SF_symop_mat_
   interface unique_SF_symop_mat_
      module procedure unique_SF_symop_mat
   end interface

   public    sum_ft_nabla_ints_
   interface sum_ft_nabla_ints_
      module procedure sum_ft_nabla_ints
   end interface

   public    stl_
   interface stl_
      module procedure stl
   end interface

   private    put_I_stats_
   interface put_I_stats_
      module procedure put_I_stats
   end interface

   public    put_
   interface put_
      module procedure put
   end interface

   private    put_fragment_cell_geometry_
   interface put_fragment_cell_geometry_
      module procedure put_fragment_cell_geometry
   end interface

   public    sum_ft_ints_
   interface sum_ft_ints_
      module procedure sum_ft_ints
   end interface

   public    cartesian_fragment_width_
   interface cartesian_fragment_width_
      module procedure cartesian_fragment_width
   end interface

   public    destroy_fragment_data_
   interface destroy_fragment_data_
      module procedure destroy_fragment_data
   end interface

   public    optimise_scale_factor_
   interface optimise_scale_factor_
      module procedure optimise_scale_factor
   end interface

   public    put_fcalc_plots_
   interface put_fcalc_plots_
      module procedure put_fcalc_plots
   end interface

   public    optimise_extinction_factor_
   interface optimise_extinction_factor_
      module procedure optimise_extinction_factor
   end interface

   private    put_F_stats_
   interface put_F_stats_
      module procedure put_F_stats
   end interface

   public    create_
   interface create_
      module procedure create
   end interface

   private    make_unique_fragment_atoms_
   interface make_unique_fragment_atoms_
      module procedure make_unique_fragment_atoms
   end interface

   public    create_copy_
   interface create_copy_
      module procedure create_copy
   end interface

   public    sum_ft_spin_ints_
   interface sum_ft_spin_ints_
      module procedure sum_ft_spin_ints
   end interface

   public    I_chi2_
   interface I_chi2_
      module procedure I_chi2
   end interface

   public    F_goodness_of_fit_
   interface F_goodness_of_fit_
      module procedure F_goodness_of_fit
   end interface

   private    make_fragment_cell_geometry_
   interface make_fragment_cell_geometry_
      module procedure make_fragment_cell_geometry
   end interface

   private    read_optimise_scale_
   interface read_optimise_scale_
      module procedure read_optimise_scale
   end interface

   public    sum_PND_spin_ints_
   interface sum_PND_spin_ints_
      module procedure sum_PND_spin_ints
   end interface

   private    put_fragment_data_
   interface put_fragment_data_
      module procedure put_fragment_data
   end interface

   public    sum_unique_sf_
   interface sum_unique_sf_
      module procedure sum_unique_sf
   end interface

   public    fragment_width_
   interface fragment_width_
      module procedure fragment_width
   end interface

   private    make_reduced_symops_
   interface make_reduced_symops_
      module procedure make_reduced_symops
   end interface

   public    nullify_ptr_part_
   interface nullify_ptr_part_
      module procedure nullify_ptr_part
   end interface

   private    put_repetition_factors_
   interface put_repetition_factors_
      module procedure put_repetition_factors
   end interface

   public    z_factor_
   interface z_factor_
      module procedure z_factor
   end interface

   public    extinction_correction_
   interface extinction_correction_
      module procedure extinction_correction
      module procedure extinction_correction_1
   end interface

   private    read_repetition_factors_
   interface read_repetition_factors_
      module procedure read_repetition_factors
   end interface

   public    unit_cell_geometry_exists_
   interface unit_cell_geometry_exists_
      module procedure unit_cell_geometry_exists
   end interface

   private    put_stl_
   interface put_stl_
      module procedure put_stl
   end interface

   private    make_unit_cell_geometry_
   interface make_unit_cell_geometry_
      module procedure make_unit_cell_geometry
   end interface

   private    put_reflection_phases_
   interface put_reflection_phases_
      module procedure put_reflection_phases
   end interface

   private    make_cluster_symops_
   interface make_cluster_symops_
      module procedure make_cluster_symops
   end interface

   public    F_calc_
   interface F_calc_
      module procedure F_calc
   end interface

   public    I_r_factor_
   interface I_r_factor_
      module procedure I_r_factor
   end interface

   private    put_fragment_geometry_
   interface put_fragment_geometry_
      module procedure put_fragment_geometry
   end interface

   public    F_r_factor_
   interface F_r_factor_
      module procedure F_r_factor
   end interface

   private    make_phases_for_symop_
   interface make_phases_for_symop_
      module procedure make_phases_for_symop
   end interface

   public    I_goodness_of_fit_
   interface I_goodness_of_fit_
      module procedure I_goodness_of_fit
   end interface

   private    read_reflection_data_
   interface read_reflection_data_
      module procedure read_reflection_data
   end interface

   public    transposed_xyz_seitz_matrices_
   interface transposed_xyz_seitz_matrices_
      module procedure transposed_xyz_seitz_matrices
   end interface

   public    put_chi2_vs_angle_plot_
   interface put_chi2_vs_angle_plot_
      module procedure put_chi2_vs_angle_plot
   end interface

   public    d_chi2_d_scale_
   interface d_chi2_d_scale_
      module procedure d_chi2_d_scale
   end interface

   public    make_crystal_error_map_
   interface make_crystal_error_map_
      module procedure make_crystal_error_map
   end interface

   private    read_correct_dispersion_
   interface read_correct_dispersion_
      module procedure read_correct_dispersion
   end interface

   public    asymmetric_unit_exists_
   interface asymmetric_unit_exists_
      module procedure asymmetric_unit_exists
   end interface

   public    d_chi2_
   interface d_chi2_
      module procedure d_chi2
   end interface

   public    unit_cell_offset_
   interface unit_cell_offset_
      module procedure unit_cell_offset
   end interface

   public    set_F_calc_
   interface set_F_calc_
      module procedure set_F_calc
   end interface

   private    make_translated_symops_
   interface make_translated_symops_
      module procedure make_translated_symops
   end interface

   private    read_wavelength_
   interface read_wavelength_
      module procedure read_wavelength
   end interface

   public    F_chi2_
   interface F_chi2_
      module procedure F_chi2
   end interface

   private    make_unique_SF_symops_
   interface make_unique_SF_symops_
      module procedure make_unique_SF_symops
   end interface

   public    put_qq_plot_
   interface put_qq_plot_
      module procedure put_qq_plot
   end interface

   private    transform_position_
   interface transform_position_
      module procedure transform_position
   end interface

   public    reflection_data_exists_
   interface reflection_data_exists_
      module procedure reflection_data_exists
   end interface

   public    equivalence_factors_
   interface equivalence_factors_
      module procedure equivalence_factors
   end interface

   private    put_unit_cell_geometry_
   interface put_unit_cell_geometry_
      module procedure put_unit_cell_geometry
   end interface

   public    simulate_new_F_exp_
   interface simulate_new_F_exp_
      module procedure simulate_new_F_exp
   end interface

   private    read_synthesize_sigma_I_
   interface read_synthesize_sigma_I_
      module procedure read_synthesize_sigma_I
   end interface

   public    n_refl_
   interface n_refl_
      module procedure n_refl
   end interface

   public    F_weighted_r_factor_
   interface F_weighted_r_factor_
      module procedure F_weighted_r_factor
   end interface

   public    process_keyword_
   interface process_keyword_
      module procedure process_keyword
   end interface

   public    read_unitcell_
   interface read_unitcell_
      module procedure read_unitcell
   end interface

   public    set_defaults_
   interface set_defaults_
      module procedure set_defaults
   end interface

   public    put_labelled_qq_plot_
   interface put_labelled_qq_plot_
      module procedure put_labelled_qq_plot
   end interface

   public    I_pred_
   interface I_pred_
      module procedure I_pred
   end interface

   public    I_exp_
   interface I_exp_
      module procedure I_exp
   end interface

   public    make_asymmetric_geometry_
   interface make_asymmetric_geometry_
      module procedure make_asymmetric_geometry
   end interface

   public    have_F_pred_
   interface have_F_pred_
      module procedure have_F_pred
   end interface

   public    read_junk_
   interface read_junk_
      module procedure read_junk
   end interface

   public    move_to_unit_cell_
   interface move_to_unit_cell_
      module procedure move_to_unit_cell
   end interface

   public    sum_ft_j_ints_
   interface sum_ft_j_ints_
      module procedure sum_ft_j_ints
   end interface

   public    sum_PND_nabla_ints_
   interface sum_PND_nabla_ints_
      module procedure sum_PND_nabla_ints
   end interface

   private    reduced_symop_mat_
   interface reduced_symop_mat_
      module procedure reduced_symop_mat
   end interface

   public    n_unique_SF_k_pts_
   interface n_unique_SF_k_pts_
      module procedure n_unique_SF_k_pts
   end interface

   type(crystal_type), pointer :: saved_self

contains

!  **************************
!  Create and destroy methods
!  **************************

   subroutine create(self)
    type(crystal_type) :: self
    ! Create an crystal object
      pointer :: self

      nullify(self)
      allocate(self)

      call nullify_ptr_part_(self)
      call set_defaults_(self)

   end subroutine

   subroutine destroy(self)
    type(crystal_type) :: self
    ! Destroy an crystal object
      pointer :: self

      if (.not. associated(self)) then;   return; end if
      call destroy_ptr_part_(self)

      deallocate(self)

   end subroutine

   subroutine nullify_ptr_part(self)
    type(crystal_type) :: self
    ! Nullify the pointer parts of the crystal object

     call nullify_ptr_part_(self%spacegroup)
     nullify(self%fragment_geometry)
     nullify(self%fragment_cell_geometry)
     nullify(self%symop_for_fragment_cell_atom)
     nullify(self%atom_for_fragment_cell_atom)
     nullify(self%unique_fragment_atom)
     nullify(self%unique_atom_for_fragment_atom)
     nullify(self%unique_symop_for_fragment_atom)
     nullify(self%reduced_symop)
     nullify(self%inverted_symop)
     nullify(self%translated_symop)
     nullify(self%unique_SF_symop)
     nullify(self%repetition_factor)
     nullify(self%asymmetric_unit_geometry)
     nullify(self%unit_cell_geometry)
     nullify(self%symop_for_unit_cell_atom)
     nullify(self%atom_for_unit_cell_atom)
     nullify(self%reflections)

   end subroutine

   subroutine destroy_ptr_part(self)
    type(crystal_type) :: self
    ! Erase all pointer information

    call destroy_reflection_data_(self)
    call destroy_fragment_data_(self)

   end subroutine

   subroutine destroy_reflection_data(self)
    type(crystal_type) :: self
    ! Erase all reflection data

     if (reflection_data_exists_(self)) then
       self%scale_factor = 1.0d0
       self%exp_scale_factor = 1.0d0
       self%extinction_factor = 0.0d0
       call destroy_(self%reflections)
     end if

   end subroutine

   subroutine destroy_fragment_data(self)
    type(crystal_type) :: self
    ! Destroy the geometry and symmetry data for the fragment and unitcell.

     call destroy_(self%fragment_geometry)
     call destroy_(self%fragment_cell_geometry)
     call destroy_(self%symop_for_fragment_cell_atom)
     call destroy_(self%atom_for_fragment_cell_atom)
     call destroy_(self%unique_fragment_atom)
     call destroy_(self%unique_atom_for_fragment_atom)
     call destroy_(self%unique_symop_for_fragment_atom)
     call destroy_(self%reduced_symop)
     call destroy_(self%inverted_symop)
     call destroy_(self%translated_symop)
     call destroy_(self%unique_SF_symop)
     call destroy_(self%repetition_factor)
     call destroy_(self%asymmetric_unit_geometry)
     call destroy_(self%unit_cell_geometry)
     call destroy_(self%symop_for_unit_cell_atom)
     call destroy_(self%atom_for_unit_cell_atom)

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

   subroutine create_copy(self,crys)
    type(crystal_type) :: self
    ! Create a copy of "crys"
     pointer :: self
     type(crystal_type), intent(in) :: crys

     call create_(self)
     call copy_(self,crys)

   end subroutine

   subroutine copy(self,crystal)
    type(crystal_type) :: self
    ! Set self to be crystal.
     type(crystal_type), intent(in) :: crystal

     self = crystal
     call nullify_ptr_part_(self)
     call copy_(self%spacegroup,crystal%spacegroup)
     call copy_(self%unitcell,crystal%unitcell)
     if (associated(crystal%fragment_geometry)) &
        call create_copy_(self%fragment_geometry,crystal%fragment_geometry)
     if (associated(crystal%fragment_cell_geometry)) &
        call create_copy_(self%fragment_cell_geometry,crystal%fragment_cell_geometry)
     if (associated(crystal%symop_for_fragment_cell_atom)) &
        call create_copy_(self%symop_for_fragment_cell_atom,crystal%symop_for_fragment_cell_atom)
     if (associated(crystal%atom_for_fragment_cell_atom)) &
        call create_copy_(self%atom_for_fragment_cell_atom,crystal%atom_for_fragment_cell_atom)
     if (associated(crystal%unique_fragment_atom)) &
        call create_copy_(self%unique_fragment_atom,crystal%unique_fragment_atom)
     if (associated(crystal%unique_atom_for_fragment_atom)) &
        call create_copy_(self%unique_atom_for_fragment_atom,crystal%unique_atom_for_fragment_atom)
     if (associated(crystal%unique_symop_for_fragment_atom)) &
        call create_copy_(self%unique_symop_for_fragment_atom,crystal%unique_symop_for_fragment_atom)
     if (associated(crystal%reduced_symop)) &
        call create_copy_(self%reduced_symop,crystal%reduced_symop)
     if (associated(crystal%cluster_symop)) &
        call create_copy_(self%cluster_symop,crystal%cluster_symop)
     if (associated(crystal%inverted_symop)) &
        call create_copy_(self%inverted_symop,crystal%inverted_symop)
     if (associated(crystal%translated_symop)) &
        call create_copy_(self%translated_symop,crystal%translated_symop)
     if (associated(crystal%unique_SF_symop)) &
        call create_copy_(self%unique_SF_symop,crystal%unique_SF_symop)
     if (associated(crystal%repetition_factor)) &
        call create_copy_(self%repetition_factor,crystal%repetition_factor)
     if (associated(crystal%asymmetric_unit_geometry)) &
        call create_copy_(self%asymmetric_unit_geometry,crystal%asymmetric_unit_geometry)
     if (associated(crystal%unit_cell_geometry)) &
        call create_copy_(self%unit_cell_geometry,crystal%unit_cell_geometry)
     if (associated(crystal%symop_for_unit_cell_atom)) &
        call create_copy_(self%symop_for_unit_cell_atom,crystal%symop_for_unit_cell_atom)
     if (associated(crystal%atom_for_unit_cell_atom)) &
        call create_copy_(self%atom_for_unit_cell_atom,crystal%atom_for_unit_cell_atom)
     if (associated(crystal%reflections)) &
        call create_copy_(self%reflections,crystal%reflections)

   end subroutine

   subroutine set_defaults(self)
    type(crystal_type) :: self
    ! Set up a default crystal object

      call set_defaults_(self%spacegroup)
      call set_defaults_(self%unitcell)
      call destroy_reflection_data_(self)
      self%synthesize_sigma_I     = .false.
      self%optimise_scale         = .true.
      self%optimise_extinction    = .true.
      self%correct_dispersion     = .false.
      self%scale_factor           = 1.0d0
      self%exp_scale_factor       = 1.0d0
      self%extinction_factor      = 0.0d0
      self%wavelength             = 0.71069d0 * (1/0.52917724924d0)
      self%data_kind              = "x-ray"
      self%thermal_smearing_model = " "
      self%partition_model        = " "
      self%reduced_group_info_made= .false.

   end subroutine

   subroutine update(self)
    type(crystal_type) :: self
    ! Update the crystal information after setting values.

     if (associated(self%reflections)) then
       if (have_F_exp_(self%reflections)) then
         call scale_F_exp_(self%reflections,self%exp_scale_factor)
       else
         self%optimise_scale = .false.
         self%optimise_extinction = .false.
       end if
       if (have_F_sigma_(self%reflections)) then
         call scale_F_sigma_(self%reflections,self%exp_scale_factor)
       else
         self%optimise_scale = .false.
         self%optimise_extinction = .false.
       end if
       self%exp_scale_factor = 1.0d0
     end if

   end subroutine

!  ************
!  Read methods
!  ************

   recursive subroutine read_keywords(self)
    type(crystal_type) :: self
    ! Read data from "stdin" using keyword style input.
    ! The following code is inherited from OBJECT
     character(128) :: word

     call ensure_(tonto,next_item_(stdin)=="{","CRYSTAL:read_keywords ... expecting open bracket symbol, {")
     call read_(stdin,word)
     do                  ! Loop over input keywords
       call read_(stdin,word)
       call to_lower_case_(word)
       if (word=="}")      exit
       if (reverted_(stdin)) exit
       call process_keyword_(self,word)
     end do

   end subroutine

   subroutine process_keyword(self,keyword)
    type(crystal_type) :: self
    ! Process command "keyword". Any required data needed by the "keyword" is
    ! inputted from "stdin".
     character(*) :: keyword
     character(128) :: word

     word = keyword
     call to_lower_case_(word)
     select case (word)
        case ("}                          ")   ! exit read_loop
        case ("correct_dispersion=        "); call read_correct_dispersion_(self)
        case ("destroy_reflection_data    "); call destroy_reflection_data_(self)
        case ("destroy_spacegroup         "); call destroy_ptr_part_(self%spacegroup)
        case ("erase_reflection_data      "); call destroy_reflection_data_(self)
        case ("erase_spacegroup           "); call destroy_ptr_part_(self%spacegroup)
        case ("exp_scale_factor=          "); call read_exp_scale_factor_(self)
        case ("kind=                      "); call read_kind_(self)
        case ("make_f_predicted           "); call make_F_predicted_(self)
        case ("optimise_extinction=       "); call read_optimise_extinction_(self)
        case ("optimise_extinction_factor="); call read_optimise_extinction_(self)
        case ("optimize_extinction=       "); call read_optimise_extinction_(self)
        case ("optimize_extinction_factor="); call read_optimise_extinction_(self)
        case ("optimise_scale=            "); call read_optimise_scale_(self)
        case ("optimise_scale_factor=     "); call read_optimise_scale_(self)
        case ("optimize_scale=            "); call read_optimise_scale_(self)
        case ("optimize_scale_factor=     "); call read_optimise_scale_(self)
        case ("partition_model=           "); call read_partition_model_(self)
        case ("repetition_factors=        "); call read_repetition_factors_(self)
        case ("reflection_data=           "); call read_reflection_data_(self)
        case ("spacegroup=                "); call read_spacegroup_(self)
        case ("synthesize_sigma_i=        "); call read_synthesize_sigma_I_(self)
        case ("thermal_smearing_model=    "); call read_thermal_smearing_model_(self)
        case ("unitcell=                  "); call read_unitcell_(self)
        case ("wavelength=                "); call read_wavelength_(self)
        case ("put                        "); call put_(self)
        case ("put_stl                    "); call put_stl_(self)
        case ("read_cif                   "); call read_CIF_(self)
        case default;                       allocate(tonto%known_keywords(28))
        tonto%known_keywords(1) = "}                          "
        tonto%known_keywords(2) = "correct_dispersion=        "
        tonto%known_keywords(3) = "destroy_reflection_data    "
        tonto%known_keywords(4) = "destroy_spacegroup         "
        tonto%known_keywords(5) = "erase_reflection_data      "
        tonto%known_keywords(6) = "erase_spacegroup           "
        tonto%known_keywords(7) = "exp_scale_factor=          "
        tonto%known_keywords(8) = "kind=                      "
        tonto%known_keywords(9) = "make_f_predicted           "
        tonto%known_keywords(10) = "optimise_extinction=       "
        tonto%known_keywords(11) = "optimise_extinction_factor="
        tonto%known_keywords(12) = "optimize_extinction=       "
        tonto%known_keywords(13) = "optimize_extinction_factor="
        tonto%known_keywords(14) = "optimise_scale=            "
        tonto%known_keywords(15) = "optimise_scale_factor=     "
        tonto%known_keywords(16) = "optimize_scale=            "
        tonto%known_keywords(17) = "optimize_scale_factor=     "
        tonto%known_keywords(18) = "partition_model=           "
        tonto%known_keywords(19) = "repetition_factors=        "
        tonto%known_keywords(20) = "reflection_data=           "
        tonto%known_keywords(21) = "spacegroup=                "
        tonto%known_keywords(22) = "synthesize_sigma_i=        "
        tonto%known_keywords(23) = "thermal_smearing_model=    "
        tonto%known_keywords(24) = "unitcell=                  "
        tonto%known_keywords(25) = "wavelength=                "
        tonto%known_keywords(26) = "put                        "
        tonto%known_keywords(27) = "put_stl                    "
        tonto%known_keywords(28) = "read_cif                   "
        call unknown_(tonto,word,"CRYSTAL:process_keyword")
        deallocate(tonto%known_keywords)
      end select

   end subroutine

   subroutine read_units(self)
    type(crystal_type) :: self
    ! Read a string which describes the units to be used
    ! The following code is inherited from OBJECT

      call set_default_units_(stdin,next_str_(stdin))

   end subroutine

   subroutine read_junk(self)
    type(crystal_type) :: self
    ! Read in a junk string, useful for ignoring a field
    ! The following code is inherited from OBJECT

      call skip_next_item_(stdin)

   end subroutine

   subroutine read_kind(self)
    type(crystal_type) :: self
    ! Read the kind of crystal experiment

      call read_(stdin,self%data_kind)
      call to_lower_case_(self%data_kind)
      select case (self%data_kind)
         case("x-ray")
         case("pnd  ")
         case default; allocate(tonto%known_keywords(2))
         tonto%known_keywords(1) = "x-ray"
         tonto%known_keywords(2) = "pnd  "
         call unknown_(tonto,self%data_kind,"CRYSTAL:read_kind")
         deallocate(tonto%known_keywords)
      end select

   end subroutine

   subroutine read_spacegroup(self)
    type(crystal_type) :: self
    ! Read the spacegroup

      call read_keywords_(self%spacegroup)
      call analyse_(self%spacegroup)

   end subroutine

   subroutine read_unitcell(self)
    type(crystal_type) :: self
    ! Read the unit cell information

      call read_keywords_(self%unitcell)
      call make_info_(self%unitcell)

   end subroutine

   subroutine read_reflection_data(self)
    type(crystal_type) :: self
    ! Read in data in the standard order from "stdin".

      call read_list_keywords_(self%reflections)

   end subroutine

   subroutine read_repetition_factors(self)
    type(crystal_type) :: self
    ! Read in the crystal fragment repetition factors. Useful to get structure
    ! factor contributions from a small portion of the fragment.

     call destroy_(self%repetition_factor)
     call read_ptr_(stdin,self%repetition_factor)

   end subroutine

   subroutine read_exp_scale_factor(self)
    type(crystal_type) :: self
    ! Read the structure factor multiplier

      call read_(stdin,self%exp_scale_factor)

   end subroutine

   subroutine read_wavelength(self)
    type(crystal_type) :: self
    ! Read the experimental wavelength

      call read_(stdin,self%wavelength)

   end subroutine

   subroutine read_optimise_scale(self)
    type(crystal_type) :: self
    ! Read the switch whether to use and overall scale factor to minimise
    ! the chi2 statistic when calculating the structure factors.
    ! NOTE: this is not the same at the overall .scale_factor which is applied
    ! to the experimental structure factors.

      call read_(stdin,self%optimise_scale)

   end subroutine

   subroutine read_synthesize_sigma_I(self)
    type(crystal_type) :: self
    ! Read the switch whether to artificially create sigma(I) errors when
    ! evaluating the chi2 statistics based on intensities. Refer to routine
    ! .I_sigma

      call read_(stdin,self%synthesize_sigma_I)

   end subroutine

   subroutine read_optimise_extinction(self)
    type(crystal_type) :: self
    ! Read the switch whether to correct extinction or not, according to the
    ! Larson formula

      call read_(stdin,self%optimise_extinction)

   end subroutine

   subroutine read_correct_dispersion(self)
    type(crystal_type) :: self
    ! Read the switch whether to correct dispersion or not, according to the
    ! atomic dispersion factors

      call read_(stdin,self%correct_dispersion)

   end subroutine

   subroutine read_thermal_smearing_model(self)
    type(crystal_type) :: self
    ! Read the thermal smearing model to use to correct for thermal vibration
    ! in the calculated structure factors

      call read_(stdin,self%thermal_smearing_model)
      call to_lower_case_(self%thermal_smearing_model)
      select case (self%thermal_smearing_model)
         case("       ")
         case("none   ")
         case("coppens")
         case("stewart")
         case("tanaka ")
         case default;    allocate(tonto%known_keywords(5))
         tonto%known_keywords(1) = "       "
         tonto%known_keywords(2) = "none   "
         tonto%known_keywords(3) = "coppens"
         tonto%known_keywords(4) = "stewart"
         tonto%known_keywords(5) = "tanaka "
         call unknown_(tonto,self%thermal_smearing_model,"CRYSTAL:read_thermal_smearing_model")
         deallocate(tonto%known_keywords)
      end select

   end subroutine

   subroutine read_partition_model(self)
    type(crystal_type) :: self
    ! Read the partition model to used to correct for oversampled fragments
    ! of the unit cell when calculating the structure factors

      call read_(stdin,self%partition_model)
      call to_lower_case_(self%partition_model)
      select case(self%partition_model)
         case("        ")
         case("none    ")
         case("mulliken")
         case("gaussian")
         case default;    allocate(tonto%known_keywords(4))
         tonto%known_keywords(1) = "        "
         tonto%known_keywords(2) = "none    "
         tonto%known_keywords(3) = "mulliken"
         tonto%known_keywords(4) = "gaussian"
         call unknown_(tonto,self%partition_model,"CRYSTAL:read_partition_model")
         deallocate(tonto%known_keywords)
      end select

   end subroutine

   subroutine read_CIF(self)
    type(crystal_type) :: self
    ! Read information from a Crystallographic Information File object, with
    ! the "name" taken from stdin.
      character(128) :: name
      logical(kind=kind(.true.)) :: found
      type(cif_type), pointer :: cif

      call read_(stdin,name)
      call create_(cif,name)
      call open_(cif)
      call find_crystal_data_block_(cif,found)
      call ensure_(tonto,found,"CRYSTAL:read_CIF ... no crsytal data block found")
      call read_CIF_(self,cif)
      call destroy_(cif)

   end subroutine

   subroutine read_CIF_1(self,cif)
    type(crystal_type) :: self
    ! Read information from a Crystallographic Information File object, "cif"
      type(cif_type) :: cif

      call set_defaults_(self)
      call read_CIF_(self%spacegroup,cif)
      call read_CIF_(self%unitcell,cif)
      call read_CIF_atoms_(self,cif)
      call update_(self)

   end subroutine

   subroutine read_CIF_atoms(self,cif)
    type(crystal_type) :: self
    ! Read atom information from a type(cif_type) file, "cif"
      type(cif_type) :: cif
      character(128) :: ID
      logical(kind=kind(.true.)) :: fs,fx,fy,fz
      character(128), dimension(:), pointer :: labels
      real(kind=kind(1.0d0)), dimension(:), pointer :: x,y,z
      integer(kind=kind(1)) :: n

      ID = "_atom_site_type_symbol"
      call find_looped_item_(cif,trim(ID),fs)
      if (.not. fs) then
      ID = "_atom_site_label"
      call find_looped_item_(cif,trim(ID),fs)
      end if
      call find_looped_item_(cif,"_atom_site_fract_x",fx)
      call find_looped_item_(cif,"_atom_site_fract_y",fy)
      call find_looped_item_(cif,"_atom_site_fract_z",fz)
      call ensure_(tonto,fs .and. fx .and. fy .and. fz,"CRYSTAL:read_CIF_atoms ... incomplete atom information in CIF file")
      call read_looped_item_(cif,trim(ID),labels)
      call read_looped_item_(cif,"_atom_site_fract_x",x)
      call read_looped_item_(cif,"_atom_site_fract_y",y)
      call read_looped_item_(cif,"_atom_site_fract_z",z)
       ! Assign the type(cif_type) info
      n = size(labels)
      self%n_asymmetric_unit_atoms = n
      call destroy_(self%asymmetric_unit_geometry)
      call create_(self%asymmetric_unit_geometry,3,n)
      self%asymmetric_unit_geometry(1,:) = x
      self%asymmetric_unit_geometry(2,:) = y
      self%asymmetric_unit_geometry(3,:) = z
      call destroy_(z); call destroy_(y); call destroy_(x)
      call destroy_(labels)

   end subroutine

!  ***************
!  General methods
!  ***************

   function lp_factor(self) result(res)
    type(crystal_type) :: self
    ! Return the array of the Lorentz Polarization factors for all the
    ! reflections
     real(kind=kind(1.0d0)), dimension(size(self%reflections)) :: res
     integer(kind=kind(1)) :: n
     real(kind=kind(1.0d0)) :: c,s,two_theta

     call ensure_(tonto,associated(self%reflections),"CRYSTAL:lp_factor ... no reflection data")
     do n=1, n_refl_(self%reflections)
       two_theta = 2.0d0*asin( stl_(self,n) * self%wavelength )
       c = cos(two_theta)
       s = sin(two_theta)
       if (is_zero_(s,10.0d0**(-8))) then
         res(n) = 0.0d0
         call warn_(tonto,"CRYSTAL:lp_factor ... lp_factor for (000) reflection set to zero")
       else
         res(n) = (1+c*c)/(2.0d0*s)
       end if
     end do

   end function

   function I_pred(self) result(res)
    type(crystal_type) :: self
    ! Return the array of predicted Intensities. Only the Lorentz Polarization
    ! factor and the angular velocity factor for a single crystal are used.
    ! Fundamental constants appearing in front of this are not calculated
    ! NOTE: unlike I_exp, these may include extinction and dispersion effects.
     real(kind=kind(1.0d0)), dimension(size(self%reflections)) :: res
     integer(kind=kind(1)) :: n
     real(kind=kind(1.0d0)) :: c,s,two_theta,F2

     call ensure_(tonto,associated(self%reflections),"CRYSTAL:I_pred ... no reflection data")
     call ensure_(tonto,have_F_exp_(self%reflections),"CRYSTAL:I_pred ... no calculated structure factors")
     do n=1, n_refl_(self%reflections)
       two_theta = 2.0d0*asin( stl_(self,n) * self%wavelength )
       c = cos(two_theta)
       s = sin(two_theta)
       F2 = self%reflections(n)%F_pred
       F2 = F2*F2
       if (is_zero_(s,10.0d0**(-8))) then
         res(n) = F2
       else
         res(n) = (1+c*c)/(2.0d0*s)*F2
       end if
     end do

   end function

   function I_exp(self) result(res)
    type(crystal_type) :: self
    ! Return the array of experimental Intensities. Only the Lorentz Polarization
    ! factor and the angular velocity factor for a single crystal are used.
    ! Fundamental constants appearing in front of this are not calculated
    ! NOTE: extinction factors, dispersion, multiple scattering corrections
    ! are not included. These are the experimental intensities with these effects
    ! removed.
     real(kind=kind(1.0d0)), dimension(size(self%reflections)) :: res
     integer(kind=kind(1)) :: n
     real(kind=kind(1.0d0)) :: c,s,two_theta,F2

     call ensure_(tonto,associated(self%reflections),"CRYSTAL:I_exp ... no reflection data")
     call ensure_(tonto,have_F_exp_(self%reflections),"CRYSTAL:I_exp ... no calculated structure factors")
     do n=1, n_refl_(self%reflections)
       two_theta = 2.0d0*asin( stl_(self,n) * self%wavelength )
       c = cos(two_theta)
       s = sin(two_theta)
       F2 = abs(self%reflections(n)%F_exp)
       F2 = F2*F2
       if (is_zero_(s,10.0d0**(-8))) then
          res(n) = F2
       else
          res(n) = (1+c*c)/(2.0d0*s)*F2
       end if
     end do

   end function

   function I_sigma(self) result(res)
    type(crystal_type) :: self
    ! Return the array of experimental sigma's in the Intensities.
     real(kind=kind(1.0d0)), dimension(size(self%reflections)) :: res
     integer(kind=kind(1)) :: n
     real(kind=kind(1.0d0)) :: c,s,two_theta,F2

     call ensure_(tonto,associated(self%reflections),"CRYSTAL:I_sigma ... no reflection data")
   call ensure_(tonto,have_I_pred_(self%reflections) .or. have_F_exp_(self%reflections),"CRYSTAL:I_sigma ... no structure fac&
&tors")
     if (.not. have_F_exp_(self%reflections) .and. .not. self%synthesize_sigma_I) then
       call warn_(tonto,"CRYSTAL:I_sigma ... The synthesize_sigma_I flag was not set; it is now set")
       self%synthesize_sigma_I = .true.
     end if
     if (self%synthesize_sigma_I) then
       res = self%reflections%I_pred
       res = sqrt(res/equivalence_factors_(self))
     else
       do n=1, n_refl_(self%reflections)
         two_theta = 2.0d0*asin( stl_(self,n) * self%wavelength )
         c = cos(two_theta)
         s = sin(two_theta)
         F2 = abs(self%reflections(n)%F_exp)
         if (is_zero_(s,10.0d0**(-8))) then
           res(n) = F2
         else
           res(n) = 2.0d0*(1+c*c)/(2.0d0*s)*F2
         end if
       end do
     end if

   end function

   function equivalence_factors(self) result(res)
    type(crystal_type) :: self
    ! Return the equivalence factors, the number of distinct reflections
    ! which are symmetry equivalent to a particular (hkl) triple, for all
    ! the reflections.
      integer(kind=kind(1)), dimension(size(self%reflections)) :: res
      integer(kind=kind(1)), dimension(3) :: hkl,new
      integer(kind=kind(1)) :: n,s,u,n_refl

      call ensure_(tonto,associated(self%reflections),"CRYSTAL:equivalence_factors ... no reflection data")
      n_refl = n_refl_(self%reflections)
      do n = 1,n_refl
         hkl = indices_(self%reflections,n)
         u = 1
         do s = 2,self%spacegroup%n_seitz
            new = matmul(hkl,self%spacegroup%seitz(1:3,1:3,s))
            if (hkl(1)/=new(1) .or. hkl(2)/=new(2) .or. hkl(3)/=new(3)) u = u + 1
         end do
         res(n) = u
      end do

   end function

   function stl(self,n) result(res)
    type(crystal_type) :: self
    ! Return the value of sin(theta) / lambda for reflection n
     intent(in) :: self
     integer(kind=kind(1)), intent(in) :: n
     real(kind=kind(1.0d0)) :: res
     integer(kind=kind(1)), dimension(3) :: hkl
     real(kind=kind(1.0d0)) :: kx,ky,kz

     hkl = indices_(self%reflections,n)
     kx = dot_product(self%unitcell%reciprocal_matrix(1,:),hkl(:))
     ky = dot_product(self%unitcell%reciprocal_matrix(2,:),hkl(:))
     kz = dot_product(self%unitcell%reciprocal_matrix(3,:),hkl(:))
     res = 0.50d0*sqrt(kx*kx+ky*ky+kz*kz)

   end function

   subroutine make_F_predicted(self)
    type(crystal_type) :: self
    ! Make the predicted magnitude of the structure factors, including possibly
    ! an overall scale factor and extinction correction.
     real(kind=kind(1.0d0)), dimension(:), pointer :: F_pred
     intent(inout) :: self

     call die_if_(tonto,.not. associated(self%reflections),"CRYSTAL:make_F_predicted ... no reflection data")
     self%n_param = 0
     if (self%optimise_extinction .or. self%optimise_scale) call get_optimum_parameters_(self)
     call create_(F_pred,n_refl_(self%reflections))
     if (self%data_kind=="pnd") then
       F_pred = self%reflections%F_calc * extinction_correction_(self)
     else
       F_pred = abs(self%reflections%F_calc) * extinction_correction_(self)
     end if
     call set_F_pred_(self%reflections,F_pred)
     call destroy_(F_pred)

   end subroutine

   subroutine get_optimum_parameters(self)
    type(crystal_type) :: self
    ! Get the scale factors, extinction parameters, etc, which minimise the chi2.
    ! (To get the corrections cooresponding to these parameters see routine
    ! .extinction_correction)

     if (self%optimise_extinction) then
       call optimise_extinction_factor_(self)
     else if (self%optimise_scale) then
       call optimise_scale_factor_(self)
     end if

   end subroutine

   function extinction_correction(self) result(res)
    type(crystal_type) :: self
    ! Return the extinction correction  factors "res" to the calculated
    ! individual structure factors.
     real(kind=kind(1.0d0)), dimension(size(self%reflections)) :: res

     res = extinction_correction_(self,self%scale_factor,self%extinction_factor)

   end function

   function extinction_correction_1(self,scale_factor,extinction_factor) result(res)
    type(crystal_type) :: self
    ! Return the extinction correction  factors "res" to the calculated
    ! individual structure factors. NOTE: this routine also does scaling
    ! corrections without extinction.
     real(kind=kind(1.0d0)) :: scale_factor,extinction_factor
     real(kind=kind(1.0d0)), dimension(size(self%reflections)) :: res
     complex(kind=kind((1.0d0,1.0d0))) :: F_calc
     real(kind=kind(1.0d0)), dimension(:), pointer :: angle_part
     integer(kind=kind(1)) :: n,n_refl

     n_refl = n_refl_(self%reflections)
     call ensure_(tonto,associated(self%reflections),"CRYSTAL:extinction_correction_1 ... no reflection data")
     call ensure_(tonto,have_F_calc_(self%reflections),"CRYSTAL:extinction_correction_1 ... no calculated structure factors")
     if (is_zero_(extinction_factor,10.0d0**(-9))) then
        res(:) = scale_factor
     else
       call create_(angle_part,n_refl)
       angle_part = extinction_angle_part_(self)
       do n=1, n_refl
         F_calc = self%reflections(n)%F_calc
         res(n) = scale_factor / sqrt(sqrt(1.0d0 + &
             extinction_factor*F_calc*conjg(F_calc)*angle_part(n)))
       end do
       call destroy_(angle_part)
     end if

   end function

   function extinction_angle_part(self) result(res)
    type(crystal_type) :: self
    ! Return the angular part of the extinction correction.
     real(kind=kind(1.0d0)), dimension(size(self%reflections)) :: res
      integer(kind=kind(1)) :: n
     real(kind=kind(1.0d0)) :: twotheta,c,s

     do n=1, n_refl_(self%reflections)
       twotheta=2.0d0*asin( stl_(self,n) * self%wavelength )
       c = cos(twotheta)
       s = sin(twotheta)
       res(n) = (1+c*c)/(1+c*s)
     end do

   end function

   subroutine optimise_scale_factor(self)
    type(crystal_type) :: self
    ! Determine the structure factor scale factor to scale the calculated
    ! structure factors .F_calc by in order to minimise the chi2. (But it does
    ! not do any scaling; see .extinction_correction routine for that)
     real(kind=kind(1.0d0)) :: top,bot,F_pred
     integer(kind=kind(1)) :: n,n_refl
     type(reflection_type), pointer :: ref

     n_refl = n_refl_(self%reflections)
     call die_if_(tonto,.not. associated(self%reflections),"CRYSTAL:optimise_scale_factor ... no reflection data")
     call die_if_(tonto,.not. have_F_calc_(self%reflections),"CRYSTAL:optimise_scale_factor ... no calculated structure facto&
&rs")
     call die_if_(tonto,.not. have_F_exp_(self%reflections),"CRYSTAL:optimise_scale_factor ... no experimental structure fact&
&ors")
     call die_if_(tonto,.not. have_F_sigma_(self%reflections),"CRYSTAL:optimise_scale_factor ... no structure factor errors")
     top = 0.0d0
     bot = 0.0d0
     do n=1,n_refl
       ref => self%reflections(n)
       if (self%data_kind=="pnd") then; F_pred = ref%F_calc
       else;                   F_pred = abs(ref%F_calc)
       end if
       call ensure_(tonto,ref%F_sigma/=0.0d0,"CRYSTAL:optimise_scale_factor ... Structure factor has zero error!")
       top = top + F_pred * ref%F_exp  / (ref%F_sigma * ref%F_sigma)
       bot = bot + F_pred * F_pred / (ref%F_sigma * ref%F_sigma)
     end do
     self%scale_factor = top/bot
     self%n_param = 1

   end subroutine

   subroutine optimise_extinction_factor(self)
    type(crystal_type) :: self
    ! Optimize the .scale_factor and .extinction_factor parameter of Larson's
    ! method.
    ! NOTE: the corrections are not applied to .F_pred; use the routine
    ! .extinction_corrections to do that.
    ! Reference: Larson, A. C., in <I>Crystallographic Computing</I>
    !            Ed. Ahmed, F. R. (Copenhagen, Munksgaard 1970), pp. 291-294.
      target :: self
      real(kind=kind(1.0d0)), dimension(2) :: p
      real(kind=kind(1.0d0)) :: chi2_min

      call optimise_scale_factor_(self)
      p(1) = self%scale_factor
      p(2) = 0.0d0           ! This is the .extinction_factor
     ! saved_self.copy(self)
      saved_self => self
      call minimise_BFGS(chi2,d_chi2,p,chi2_min,tol=10.0d0**(-7),gtol=10.0d0**(-7),step=10.0d0**(-4))
     ! saved_self.destroy_ptr_part
      self%scale_factor = p(1)
      self%extinction_factor = p(2)
      self%n_param = 2

   end subroutine

   function chi2(p) result(res)
    ! Make the chi2 between the calculated and experimental structure factors
    ! with extinction and scale parameters stored in vector p.
     real(kind=kind(1.0d0)), dimension(:) :: p
     real(kind=kind(1.0d0)) :: res
     real(kind=kind(1.0d0)) :: tmp,tmp1
     real(kind=kind(1.0d0)), dimension(:), pointer :: ext
     type(reflection_type), pointer :: ref
     integer(kind=kind(1)) :: n,n_refl
     type(crystal_type), pointer :: self
    ! self.copy(saved_self)

     self => saved_self
     call ensure_(tonto,associated(self%reflections),"CRYSTAL:chi2 ... no reflection data")
     call ensure_(tonto,have_F_calc_(self%reflections),"CRYSTAL:chi2 ... no calculated structure factors")
     call ensure_(tonto,have_F_exp_(self%reflections),"CRYSTAL:chi2 ... no experimental structure factors")
     call ensure_(tonto,have_F_sigma_(self%reflections),"CRYSTAL:chi2 ... no structure factor errors")
     call ensure_(tonto,size(p)==2,"CRYSTAL:chi2 ... wrong size, p")
     n_refl = n_refl_(self%reflections)
     call create_(ext,n_refl)
     ext = extinction_correction_(self,p(1),p(2))
     tmp = 0.0d0
     do n=1,n_refl
       ref => self%reflections(n)
       tmp1 = (abs(ref%F_calc) * ext(n) - ref%F_exp) / ref%F_sigma
       tmp = tmp + tmp1 * tmp1
     end do
       res = tmp / max(n_refl-self%n_param,1)
     call destroy_(ext)
    ! self.destroy_ptr_part

   end function

   function d_chi2(p) result(res)
    ! Return the derivative of the chi2 with respect to the .scale_factor in
    ! p(1), and with respect to .extinction_factor in p(2). This routine is for
    ! use in the BFGS minimiser.
      real(kind=kind(1.0d0)), dimension(:) :: p
      real(kind=kind(1.0d0)), dimension(size(p)) :: res
      type(crystal_type), pointer :: self
    !  self.copy(saved_self)

      self => saved_self
      call ensure_(tonto,size(p)==2,"CRYSTAL:d_chi2 ... wrong size, p")
      res(1) = d_chi2_d_scale_(self,p)
      res(2) = d_chi2_d_ext_(self,p)
    !  self.destroy_ptr_part
!      res(1) = saved_self.d_chi2_d_scale(p)
!      res(2) = saved_self.d_chi2_d_ext(p)

   end function

   function d_chi2_d_scale(self,p) result(res)
    type(crystal_type) :: self
    ! Derivative of the chi^2 with respect to the scale factor
     real(kind=kind(1.0d0)), dimension(:) :: p
     real(kind=kind(1.0d0)) :: res
     real(kind=kind(1.0d0)) :: tmp,F_pred
     real(kind=kind(1.0d0)), dimension(:), pointer :: ext
     type(reflection_type), pointer :: ref
     integer(kind=kind(1)) :: n,n_refl

     n_refl = n_refl_(self%reflections)
     call ensure_(tonto,associated(self%reflections),"CRYSTAL:d_chi2_d_scale ... no reflection data")
     call ensure_(tonto,have_F_calc_(self%reflections),"CRYSTAL:d_chi2_d_scale ... no calculated structure factors")
     call ensure_(tonto,have_F_exp_(self%reflections),"CRYSTAL:d_chi2_d_scale ... no experimental structure factors")
     call ensure_(tonto,have_F_sigma_(self%reflections),"CRYSTAL:d_chi2_d_scale ... no structure factor errors")
     call ensure_(tonto,size(p)==2,"CRYSTAL:d_chi2_d_scale ... wrong size, p")
     call create_(ext,n_refl)
     ext = extinction_correction_(self,p(1),p(2))
     tmp = 0.0d0
     do n=1,n_refl
       ref => self%reflections(n)
       F_pred = abs(ref%F_calc) * ext(n)
       tmp = tmp + F_pred*F_pred - ref%F_exp*F_pred / (ref%F_sigma*ref%F_sigma)
     end do
     res = 2.0d0*tmp/(p(1)*max(n_refl-self%n_param,1))
     call destroy_(ext)

   end function

   function d_chi2_d_ext(self,p) result(res)
    type(crystal_type) :: self
    ! Derivative of the chi^2 with respect to the extinction parameter.
     real(kind=kind(1.0d0)), dimension(:), intent(in) :: p
     real(kind=kind(1.0d0)) :: res
     real(kind=kind(1.0d0)) :: tmp,extn,p1
     real(kind=kind(1.0d0)), dimension(:), pointer :: angle_bit,ext
     type(reflection_type), pointer :: ref
     integer(kind=kind(1)) :: n,n_refl

     n_refl = n_refl_(self%reflections)
     call ensure_(tonto,associated(self%reflections),"CRYSTAL:d_chi2_d_ext ... no reflection data")
     call ensure_(tonto,have_F_calc_(self%reflections),"CRYSTAL:d_chi2_d_ext ... no calculated structure factors")
     call ensure_(tonto,have_F_exp_(self%reflections),"CRYSTAL:d_chi2_d_ext ... no experimental structure factors")
     call ensure_(tonto,have_F_sigma_(self%reflections),"CRYSTAL:d_chi2_d_ext ... no structure factor errors")
     call ensure_(tonto,size(p)==2,"CRYSTAL:d_chi2_d_ext ... wrong size, p")
     call create_(angle_bit,n_refl)
     call create_(ext,n_refl)
     angle_bit = extinction_angle_part_(self)
     p1 = p(1)
     ext = extinction_correction_(self,1.0d0,p(2))
     tmp = 0.0d0
     do n = 1,n_refl
       ref => self%reflections(n)
       extn = ext(n)
       tmp = tmp + (p1 * abs(ref%F_calc) * extn - ref%F_exp) * &
           extn*extn*extn*extn*extn* ref%F_calc*ref%F_calc*ref%F_calc * &
           angle_bit(n) / (ref%F_sigma * ref%F_sigma)
     end do
     res = -p1*tmp/(2.0d0 * max(n_refl-self%n_param,1))
     call destroy_(ext)
     call destroy_(angle_bit)

   end function

   function d_chi2_dU(self,dF) result(res)
    type(crystal_type) :: self
    ! Evaluate the derivative of the chi^2 with respect to parameters U
    ! (e.g. thermal parameters) given the derivatives "dF" of where
    ! .F_calc with respect to these parameters U. NOTE: this routine
    ! assumes that the .scale_factor and .extinction_factor are fixed.
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: dF
     real(kind=kind(1.0d0)), dimension(size(dF,2)) :: res
     complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: Fc_conjgFcalc
     type(reflection_type), pointer :: ref
     real(kind=kind(1.0d0)), dimension(:), pointer :: ext,angle_bit
     real(kind=kind(1.0d0)) :: fac,Fc,Fc1,extn
     integer(kind=kind(1)) :: u,n_refl,n

     n_refl = n_refl_(self%reflections)
     call ensure_(tonto,associated(self%reflections),"CRYSTAL:d_chi2_dU ... no reflection data")
     call ensure_(tonto,size(dF,1)==n_refl,"CRYSTAL:d_chi2_dU ... wrong size, dF")
     call ensure_(tonto,have_F_calc_(self%reflections),"CRYSTAL:d_chi2_dU ... no calculated structure factors")
     call ensure_(tonto,have_F_exp_(self%reflections),"CRYSTAL:d_chi2_dU ... no experimental structure factors")
     call ensure_(tonto,have_F_sigma_(self%reflections),"CRYSTAL:d_chi2_dU ... no structure factor errors")
     call create_(angle_bit,n_refl)
     angle_bit = extinction_angle_part_(self)
     call create_(ext,n_refl)
     ext = extinction_correction_(self,1.0d0,self%extinction_factor)
     call create_(Fc_conjgFcalc,n_refl)
     do n = 1,n_refl
       ref => self%reflections(n)
       extn = ext(n)
       Fc1 = abs(ref%F_calc)
       Fc = ( (self%scale_factor*extn*Fc1 - ref%F_exp) / &
          (ref%F_sigma * ref%F_sigma) ) * &
          (extn / Fc1 - 0.50d0 * extn * extn * extn * extn * extn * &
          self%extinction_factor * Fc1 * angle_bit(n))
       Fc_conjgFcalc(n) = Fc * conjg(ref%F_calc)
     end do
     fac = 2.0d0*self%scale_factor/max(n_refl-self%n_param,1)
     do u = 1,size(dF,2)
        res(u) = fac * sum( Fc_conjgFcalc(:) * dF(:,u) )
     end do
     call destroy_(Fc_conjgFcalc)
     call destroy_(ext)
     call destroy_(angle_bit)

   end function

   subroutine make_k_pts(self,res)
    type(crystal_type) :: self
    ! Convert the .reflection.hkl indices to reciprocal lattice vectors
    ! Dimension of res is [.n_refl,3]
      real(kind=kind(1.0d0)), dimension(:,:) :: res
       integer(kind=kind(1)) :: n
      integer(kind=kind(1)), dimension(3) :: hkl
      real(kind=kind(1.0d0)), dimension(3,3) :: rcm

      rcm = 2.0d0*3.141592653589793d0*self%unitcell%reciprocal_matrix
      do n = 1, n_refl_(self%reflections)
         hkl = indices_(self%reflections,n)
         res(n,1) = dot_product(rcm(1,:),hkl(:))
         res(n,2) = dot_product(rcm(2,:),hkl(:))
         res(n,3) = dot_product(rcm(3,:),hkl(:))
      end do

   end subroutine

   pure function n_unique_SF_k_pts(self) result(res)
    type(crystal_type) :: self
    ! The number of unique k-points for an SF calculation
     intent(in) :: self
     integer(kind=kind(1)) :: res
     res = self%n_unique_SF_symops * n_refl_(self%reflections)

   end function

   subroutine make_unique_SF_k_pts(self,k)
    type(crystal_type) :: self
    ! Convert the hkl indices to unique reciprocal lattice vectors "k" required
    ! for structure factor calculations.
    ! (c) Dylan Jayatilaka, UWA, feb 1996
      real(kind=kind(1.0d0)), dimension(:,:) :: k
      integer(kind=kind(1)) :: p,u,n
      real(kind=kind(1.0d0)), dimension(3,3) :: b,rcm
      integer(kind=kind(1)), dimension(3) :: hkl

      call ensure_(tonto,associated(self%unique_SF_symop),"CRYSTAL:make_unique_SF_k_pts ... no unique_SF_symop array!")
      call ensure_(tonto,size(k,1)>0,"CRYSTAL:make_unique_SF_k_pts ... no unique k points")
      call ensure_(tonto,size(k,1)==n_unique_SF_k_pts_(self),"CRYSTAL:make_unique_SF_k_pts ... wrong # of k points")
      p = 0
      rcm = 2.0d0*3.141592653589793d0*self%unitcell%reciprocal_matrix
      do u = 1,self%n_unique_SF_symops
         b = matmul(rcm,transpose(unique_SF_symop_mat_(self,u)))
         do n = 1,n_refl_(self%reflections)
            p = p + 1
            hkl = indices_(self%reflections,n)
            k(p,1) = dot_product(b(1,:),hkl(:))
            k(p,2) = dot_product(b(2,:),hkl(:))
            k(p,3) = dot_product(b(3,:),hkl(:))
         end do
      end do

   end subroutine

!  *************************************
!  Service methods used by other modules
!  *************************************

   subroutine make_phases_for_symop(self,u,phase,mask)
    type(crystal_type) :: self
    ! Return the sum of the "phase" shifts for each (hkl) reflection from each
    ! glide vector for all symops which are equivalent to the "u"-th
    ! unique symmetry operation, .unique_SF_symop(u), as determined by the mask
    ! array.
     integer(kind=kind(1)), intent(in) :: u
     complex(kind=kind((1.0d0,1.0d0))), dimension(:), intent(out) :: phase
     integer(kind=kind(1)), dimension(:), intent(in) :: mask
     integer(kind=kind(1)), dimension(3) :: hkl
     real(kind=kind(1.0d0)) :: pi2,tx,ty,tz
     integer(kind=kind(1)) :: s,n,n_refl

     n_refl = n_refl_(self%reflections)
     call ensure_(tonto,associated(self%unique_SF_symop),"CRYSTAL:make_phases_for_symop ... no unique_SF_symop array!")
     call ensure_(tonto,u<=self%n_unique_SF_symops,"CRYSTAL:make_phases_for_symop ... symop index out of range")
     call ensure_(tonto,size(phase)==n_refl,"CRYSTAL:make_phases_for_symop ... wrong length for phase array")
     pi2 = 2.0d0*3.141592653589793d0
     phase = 0.0d0
     do s = 1,self%spacegroup%n_seitz
       if (mask(s)/=self%unique_SF_symop(u)) cycle
       tx = pi2*self%spacegroup%seitz(1,4,s)
       ty = pi2*self%spacegroup%seitz(2,4,s)
       tz = pi2*self%spacegroup%seitz(3,4,s)
       do n=1,n_refl
         hkl = indices_(self%reflections,n)
         phase(n) = phase(n) + exp(cmplx(0.0d0,hkl(1)*tx+hkl(2)*ty+hkl(3)*tz,kind=kind((1.0d0,1.0d0))))
       end do
     end do

   end subroutine

   subroutine sum_unique_sf(self,sf,unique_sf)
    type(crystal_type) :: self
    ! Form the structure factors "sf" from the sum of the list of
    ! unique structure factors, "unique_sf".
     complex(kind=kind((1.0d0,1.0d0))), dimension(:) :: sf
     complex(kind=kind((1.0d0,1.0d0))), dimension(:), intent(in) :: unique_sf
     complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: phase
     integer(kind=kind(1)) :: u,uf,ul,n_refl

     n_refl = n_refl_(self%reflections)
     call ensure_(tonto,size(sf)==n_refl,"CRYSTAL:sum_unique_sf ... incorrect size for array sf")
     call create_(phase,n_refl)
     sf = 0.0d0
     do u = 1,self%n_unique_SF_symops
        uf = n_refl*(u-1)+1
        ul = n_refl*u
        call make_phases_for_symop_(self,u,phase,self%translated_symop)
        sf(:) = sf(:) + phase(:)*unique_sf(uf:ul)
        if ( all(self%inverted_symop/=self%unique_SF_symop(u)) ) cycle
        call make_phases_for_symop_(self,u,phase,self%inverted_symop)
        sf(:) = sf(:) + phase(:)*conjg(unique_sf(uf:ul))
     end do
     call destroy_(phase)

   end subroutine

   subroutine sum_unique_sf_ints(self,sf_ints,unique_sf_ints)
    type(crystal_type) :: self
    ! Form the structure factor integrals "sf_ints" from a sum of the list
    ! of unique structure factors integrals  "unique_sf_ints".
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:) :: sf_ints
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:), intent(in) :: unique_sf_ints
     complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: phase
     integer(kind=kind(1)) :: u,uf,n,n_refl

     n_refl = n_refl_(self%reflections)
     call ensure_(tonto,size(sf_ints,1)==n_refl,"CRYSTAL:sum_unique_sf_ints ... wrong size for sf_ints!")
     call create_(phase,n_refl)
     sf_ints = 0.0d0
     do u = 1,self%n_unique_SF_symops
       uf = n_refl*(u-1)
       call make_phases_for_symop_(self,u,phase,self%translated_symop)
       do n = 1,n_refl
         sf_ints(n,:,:) = sf_ints(n,:,:) + phase(n)*unique_sf_ints(uf+n,:,:)
       end do
       if ( all(self%inverted_symop/=self%unique_SF_symop(u)) ) cycle
       call make_phases_for_symop_(self,u,phase,self%inverted_symop)
       do n = 1,n_refl
         sf_ints(n,:,:) = sf_ints(n,:,:) + phase(n)*conjg(unique_sf_ints(uf+n,:,:))
       end do
     end do
     call destroy_(phase)

   end subroutine

   subroutine sum_unique_sf_deriv_U(self,sf,unique_sf)
    type(crystal_type) :: self
    ! Form the structure factor derivatives "sf" (wrt the thermal paramaters,U)
    !  from a sum of the list of unique structure factor derivatives "unique_sf".
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: sf
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), intent(in) :: unique_sf
     complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: phase
     integer(kind=kind(1)) :: u,uf,n,n_refl

     n_refl = n_refl_(self%reflections)
     call ensure_(tonto,size(sf,1)==n_refl,"CRYSTAL:sum_unique_sf_deriv_U ... wrong size, matrix sf")
     call ensure_(tonto,size(unique_sf,2)==n_unique_SF_k_pts_(self),"CRYSTAL:sum_unique_sf_deriv_U ... wrong size, matrix sf"&
&)
     call create_(phase,n_refl)
     sf = 0.0d0
     do u = 1,self%n_unique_SF_symops
       uf = n_refl*(u-1)
       call make_phases_for_symop_(self,u,phase,self%translated_symop)
       do n = 1,n_refl
         sf(n,:) = sf(n,:) + phase(n)*unique_sf(:,uf+n)
       end do
       if ( all(self%inverted_symop/=self%unique_SF_symop(u)) ) cycle
       call make_phases_for_symop_(self,u,phase,self%inverted_symop)
       do n = 1,n_refl
         sf(n,:) = sf(n,:) + phase(n)*conjg(unique_sf(:,uf+n))
       end do
     end do
     call destroy_(phase)

   end subroutine

   subroutine sum_ft_ints(self,ft_ints,unique_ft_ints)
    type(crystal_type) :: self
    ! Form the Fourier transform integrals "ft_ints" from a sum of the list
    ! of unique integrals "unique_ft_ints".  Dimensions of ft_ints are
    ! [.n_refl,n_comp_a,n_comp_b].
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:) :: ft_ints
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:), intent(in) :: unique_ft_ints
     complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: phase
     integer(kind=kind(1)) :: u,uf,n,n_refl

     n_refl = n_refl_(self%reflections)
     call ensure_(tonto,size(ft_ints,1)==n_refl,"CRYSTAL:sum_ft_ints ... incorrect size for array ft_ints")
     call create_(phase,n_refl)
     ft_ints = 0.0d0
     do u = 1,self%n_unique_SF_symops
       uf = n_refl*(u-1)
       call make_phases_for_symop_(self,u,phase,self%translated_symop)
       do n = 1,n_refl
         ft_ints(n,:,:) = ft_ints(n,:,:) + phase(n)*unique_ft_ints(uf+n,:,:)
       end do
       if ( all(self%inverted_symop/=self%unique_SF_symop(u)) ) cycle
       call make_phases_for_symop_(self,u,phase,self%inverted_symop)
       do n = 1,n_refl
         ft_ints(n,:,:) = ft_ints(n,:,:) + phase(n)*conjg(unique_ft_ints(uf+n,:,:))
       end do
     end do
     call destroy_(phase)

   end subroutine

   subroutine make_phased_matrix_for_symop(self,u,phase,mask)
    type(crystal_type) :: self
    ! Return the sum of the "phase" shifts times the seitz matrices for all
    ! symops which are equivalent to the "u"-th unique symmetry operation,
    ! .unique_SF_symop(u), as determined by the "mask" array, when
    ! mask(u)==.unique_SF_symop(u).
     integer(kind=kind(1)), intent(in) :: u
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:), intent(out) :: phase
     integer(kind=kind(1)), dimension(:), intent(in) :: mask
     integer(kind=kind(1)), dimension(3) :: hkl
     real(kind=kind(1.0d0)), dimension(3) :: t
     real(kind=kind(1.0d0)) :: pi2
     integer(kind=kind(1)) :: s,n,n_refl
     real(kind=kind(1.0d0)), dimension(4,4) :: seitz

     pi2 = 2.0d0*3.141592653589793d0
     phase = 0.0d0
     n_refl = n_refl_(self%reflections)
     call ensure_(tonto,associated(self%unique_SF_symop),"CRYSTAL:make_phased_matrix_for_symop ... no unique_SF_symop array!"&
&)
     call ensure_(tonto,u<=self%n_unique_SF_symops,"CRYSTAL:make_phased_matrix_for_symop ... symop index out of range")
     call ensure_(tonto,size(phase,1)==n_refl,"CRYSTAL:make_phased_matrix_for_symop ... wrong size, dim=1, phase array")
     call ensure_(tonto,size(phase,2)==3,"CRYSTAL:make_phased_matrix_for_symop ... wrong size, dim=2, phase array")
     call ensure_(tonto,size(phase,3)==3,"CRYSTAL:make_phased_matrix_for_symop ... wrong size, dim=3, phase array")
     do s = 1,self%spacegroup%n_seitz
       if (mask(s)/=self%unique_SF_symop(u)) cycle
       seitz = transpose(self%spacegroup%seitz(:,:,s))
       t = (/ pi2*seitz(4,1), pi2*seitz(4,2), pi2*seitz(4,3) /)
       if (seitz(3,3)>0) then                   ! M points along +z always
         do n = 1,n_refl
           hkl = indices_(self%reflections,n)
           phase(n,:,:) = phase(n,:,:) + &
              seitz(:,:)*exp(cmplx(0.0d0,dot_product(t,hkl),kind=kind((1.0d0,1.0d0))))
         end do
       else                                     ! M points in -z direction
         seitz(3,:3) = -seitz(3,:3)            ! Invert
         do n = 1,n_refl
           hkl = indices_(self%reflections,n)
           phase(n,:,:) = phase(n,:,:) + &
              seitz(:,:)*exp(cmplx(0.0d0,dot_product(t,hkl),kind=kind((1.0d0,1.0d0))))
         end do
       end if
     end do

   end subroutine

   subroutine sum_PND_spin_ints(self,ft_ints,unique_ft_ints)
    type(crystal_type) :: self
    ! Form the Fourier transform integrals "ft_ints", required for the spin
    ! magnetic structure factors, from a sum of the list of unique structure
    ! factor intergals "unique_ft_ints".  Dimensions of ft_ints are
    ! [.n_refl,n_comp_a,n_comp_b,3].
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:,:) :: ft_ints
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:), intent(in) :: unique_ft_ints
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:), pointer :: phase
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: q
     integer(kind=kind(1)) :: u,uf,n,n_refl

     n_refl = n_refl_(self%reflections)
     call ensure_(tonto,size(ft_ints,1)==n_refl,"CRYSTAL:sum_PND_spin_ints ... wrong size for ft_ints!")
     call create_(q,n_refl,3)
     call create_(phase,n_refl,3,3)
     call make_k_pts_(self,q)
     ft_ints = 0.0d0
     do u = 1,self%n_unique_SF_symops
        uf = n_refl*(u-1)
        call make_phased_matrix_for_symop_(self,u,phase,self%translated_symop)
        do n = 1,n_refl
           ft_ints(n,:,:,1) = ft_ints(n,:,:,1) + &
              ( q(n,3)*q(n,1)*phase(n,1,1) + q(n,3)*q(n,2)*phase(n,2,1)     &
              - q(n,1)*q(n,1)*phase(n,3,1) - q(n,2)*q(n,2)*phase(n,3,1) ) * &
              unique_ft_ints(uf+n,:,:)
           ft_ints(n,:,:,2) = ft_ints(n,:,:,2) + &
              ( q(n,3)*q(n,1)*phase(n,1,2) + q(n,3)*q(n,2)*phase(n,2,2)     &
              - q(n,1)*q(n,1)*phase(n,3,2) - q(n,2)*q(n,2)*phase(n,3,2) ) * &
              unique_ft_ints(uf+n,:,:)
           ft_ints(n,:,:,3) = ft_ints(n,:,:,3) + &
              ( q(n,3)*q(n,1)*phase(n,1,3) + q(n,3)*q(n,2)*phase(n,2,3)     &
              - q(n,1)*q(n,1)*phase(n,3,3) - q(n,2)*q(n,2)*phase(n,3,3) ) * &
              unique_ft_ints(uf+n,:,:)
        end do
        if ( all(self%inverted_symop/=self%unique_SF_symop(u)) ) cycle  ! for inversions...
        call make_phased_matrix_for_symop_(self,u,phase,self%inverted_symop)
        do n = 1,n_refl
           ft_ints(n,:,:,1) = ft_ints(n,:,:,1) + &
              ( q(n,3)*q(n,1)*phase(n,1,1) + q(n,3)*q(n,2)*phase(n,2,1)     &
              - q(n,1)*q(n,1)*phase(n,3,1) - q(n,2)*q(n,2)*phase(n,3,1) ) * &
              conjg(unique_ft_ints(uf+n,:,:))
           ft_ints(n,:,:,2) = ft_ints(n,:,:,2) + &
              ( q(n,3)*q(n,1)*phase(n,1,2) + q(n,3)*q(n,2)*phase(n,2,2)     &
              - q(n,1)*q(n,1)*phase(n,3,2) - q(n,2)*q(n,2)*phase(n,3,2) ) * &
              conjg(unique_ft_ints(uf+n,:,:))
           ft_ints(n,:,:,3) = ft_ints(n,:,:,3) + &
              ( q(n,3)*q(n,1)*phase(n,1,3) + q(n,3)*q(n,2)*phase(n,2,3)     &
              - q(n,1)*q(n,1)*phase(n,3,3) - q(n,2)*q(n,2)*phase(n,3,3) ) * &
              conjg(unique_ft_ints(uf+n,:,:))
        end do
     end do
     call destroy_(phase)
      ! The factor of two for conversion to Bohr magnetons cancels the
      ! factor of half for the S=sigma/2 operator, and g_e x mu_B = 1.
     do n = 1,n_refl
       ft_ints(n,:,:,:) = ft_ints(n,:,:,:)/(q(n,1)*q(n,1)+q(n,2)*q(n,2))
     end do
     call destroy_(q)

   end subroutine

   subroutine sum_PND_nabla_ints(self,ft_ints,unique_ft_ints)
    type(crystal_type) :: self
    ! Form the fourier transform nabla_a_3 integrals "ft_ints", required for the
    ! PND magnetic structure factors, from a sum of "unique_ft_ints".
    ! Dimensions of ft_ints are [.n_refl,n_comp_a,n_comp_b,3].
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:) :: ft_ints
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:,:), intent(in) :: unique_ft_ints
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:), pointer :: phase
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: q
     integer(kind=kind(1)) :: u,uf,n,n_refl

     n_refl = n_refl_(self%reflections)
     call ensure_(tonto,size(ft_ints,1)==n_refl,"CRYSTAL:sum_PND_nabla_ints ... wrong size for ft_ints!")
     call create_(q,n_refl,3)
     call create_(phase,n_refl,3,3)
     call make_k_pts_(self,q)
     ft_ints = 0.0d0
     do u = 1,self%n_unique_SF_symops
       uf = n_refl*(u-1)
       call make_phased_matrix_for_symop_(self,u,phase,self%translated_symop)
       do n = 1,n_refl
         ft_ints(n,:,:) = ft_ints(n,:,:) + &
          (q(n,1)*phase(n,2,1)-q(n,2)*phase(n,1,1))*unique_ft_ints(uf+n,:,:,1) + &
          (q(n,1)*phase(n,2,2)-q(n,2)*phase(n,1,2))*unique_ft_ints(uf+n,:,:,2) + &
          (q(n,1)*phase(n,2,3)-q(n,2)*phase(n,1,3))*unique_ft_ints(uf+n,:,:,3)
       end do
       if ( all(self%inverted_symop/=self%unique_SF_symop(u)) ) cycle  ! for inversions ...
       call make_phased_matrix_for_symop_(self,u,phase,self%translated_symop)
       do n = 1,n_refl
         ft_ints(n,:,:) = ft_ints(n,:,:) + &
          (q(n,1)*phase(n,2,1)-q(n,2)*phase(n,1,1))*conjg(unique_ft_ints(uf+n,:,:,1)) + &
          (q(n,1)*phase(n,2,2)-q(n,2)*phase(n,1,2))*conjg(unique_ft_ints(uf+n,:,:,2)) + &
          (q(n,1)*phase(n,2,3)-q(n,2)*phase(n,1,3))*conjg(unique_ft_ints(uf+n,:,:,3))
       end do
     end do
     call destroy_(phase)
      ! The factor of 2 to convert to Bohr magnetons cancels the factor
      ! of 1/2 for the Bohr magneton.
     do n = 1,n_refl
       ft_ints(n,:,:) = ft_ints(n,:,:)/(q(n,1)*q(n,1)+q(n,2)*q(n,2))
     end do
     call destroy_(q)

   end subroutine

   subroutine sum_ft_spin_ints(self,ft_ints,unique_ft_ints)
    type(crystal_type) :: self
    ! Form the Fourier transform integrals "ft_ints", required for the spin
    ! magnetic structure factors, from a sum of the list of unique structure
    ! factor intergals "unique_ft_ints".  Dimensions of ft_ints are
    ! [.n_refl,n_comp_a,n_comp_b,3,3].
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:,:,:) :: ft_ints
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:), intent(in) :: unique_ft_ints
     complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: phase
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: q
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: ints
     integer(kind=kind(1)) :: u,uf,n,n_refl

     n_refl = n_refl_(self%reflections)
     call ensure_(tonto,size(ft_ints,1)==n_refl,"CRYSTAL:sum_ft_spin_ints ... wrong size for ft_ints!")
     call create_(phase,n_refl)
     call create_(q,n_refl,3)
     call create_(ints,size(ft_ints,2),size(ft_ints,3))
     call make_k_pts_(self,q)
     ft_ints = 0.0d0
     do u = 1,self%n_unique_SF_symops
       uf = n_refl*(u-1)
       call make_phases_for_symop_(self,u,phase,self%translated_symop)
       do n = 1,n_refl
         ints = phase(n)*unique_ft_ints(uf+n,:,:)
         ft_ints(n,:,:,1,1) = ft_ints(n,:,:,1,1) + q(n,1)*q(n,1)*ints(:,:)
         ft_ints(n,:,:,2,1) = ft_ints(n,:,:,2,1) + q(n,2)*q(n,1)*ints(:,:)
         ft_ints(n,:,:,2,2) = ft_ints(n,:,:,2,2) + q(n,2)*q(n,2)*ints(:,:)
         ft_ints(n,:,:,3,1) = ft_ints(n,:,:,3,1) + q(n,3)*q(n,1)*ints(:,:)
         ft_ints(n,:,:,3,2) = ft_ints(n,:,:,3,2) + q(n,3)*q(n,2)*ints(:,:)
         ft_ints(n,:,:,3,3) = ft_ints(n,:,:,3,3) + q(n,3)*q(n,3)*ints(:,:)
       end do
       if ( all(self%inverted_symop/=self%unique_SF_symop(u)) ) cycle  ! for inversions...
       call make_phases_for_symop_(self,u,phase,self%inverted_symop)
       do n = 1,n_refl
         ints = phase(n)*conjg(unique_ft_ints(uf+n,:,:))
         ft_ints(n,:,:,1,1) = ft_ints(n,:,:,1,1) + q(n,1)*q(n,1)*ints(:,:)
         ft_ints(n,:,:,2,1) = ft_ints(n,:,:,2,1) + q(n,2)*q(n,1)*ints(:,:)
         ft_ints(n,:,:,2,2) = ft_ints(n,:,:,2,2) + q(n,2)*q(n,2)*ints(:,:)
         ft_ints(n,:,:,3,1) = ft_ints(n,:,:,3,1) + q(n,3)*q(n,1)*ints(:,:)
         ft_ints(n,:,:,3,2) = ft_ints(n,:,:,3,2) + q(n,3)*q(n,2)*ints(:,:)
         ft_ints(n,:,:,3,3) = ft_ints(n,:,:,3,3) + q(n,3)*q(n,3)*ints(:,:)
       end do
     end do
     call destroy_(ints)
      ! The factor of two for conversion to Bohr magnetons cancels the
      ! factor of half for the S operator
     do n = 1,n_refl
        ft_ints(n,:,:,:,:) = -ft_ints(n,:,:,:,:)/(q(n,1)*q(n,1)+q(n,2)*q(n,2))
     end do
     call destroy_(q)
     call destroy_(phase)

   end subroutine

   subroutine sum_ft_r_ints(self,ft_ints,unique_ft_ints,B)
    type(crystal_type) :: self
    ! Form the Fourier transform dipole integrals "ft_ints", required for the PND
    ! magnetic structure factors, from a sum of the list of unique structure
    ! factor intergals "unique_ft_ints".  "B" is the external magnetic field.
    ! Note: only inversions are allowed as symmetry elements for PND simulations.
    ! Dimensions of ft are [.n_refl,n_comp_a,n_comp_b,3].
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:,:) :: ft_ints
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:,:), intent(in) :: unique_ft_ints
     real(kind=kind(1.0d0)), dimension(3) :: B
     complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: phase
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: q
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:), pointer :: ints
     integer(kind=kind(1)) :: u,uf,n,n_refl
     complex(kind=kind((1.0d0,1.0d0))) :: ci

     n_refl = n_refl_(self%reflections)
     call ensure_(tonto,size(ft_ints,1)==n_refl,"CRYSTAL:sum_ft_r_ints ... incorrect size for array ft_ints")
     call create_(phase,n_refl)
     call create_(q,n_refl,3)
     call create_(ints,size(ft_ints,2),size(ft_ints,3),3)
     call make_k_pts_(self,q)
     ft_ints = 0.0d0
     do u = 1,self%n_unique_SF_symops
       uf = n_refl*(u-1)
       call make_phases_for_symop_(self,u,phase,self%translated_symop)
       do n = 1,n_refl
         ints = phase(n)*unique_ft_ints(uf+n,:,:,:)
         ft_ints(n,:,:,1) = ft_ints(n,:,:,1) + q(n,2)*(B(1)*ints(:,:,2)-B(2)*ints(:,:,1)) &
                                             - q(n,3)*(B(3)*ints(:,:,1)-B(1)*ints(:,:,3))
         ft_ints(n,:,:,2) = ft_ints(n,:,:,2) + q(n,3)*(B(2)*ints(:,:,3)-B(3)*ints(:,:,2)) &
                                             - q(n,1)*(B(1)*ints(:,:,2)-B(2)*ints(:,:,1))
         ft_ints(n,:,:,3) = ft_ints(n,:,:,3) + q(n,1)*(B(3)*ints(:,:,1)-B(1)*ints(:,:,3)) &
                                             - q(n,2)*(B(2)*ints(:,:,3)-B(3)*ints(:,:,2))
       end do
       if ( all(self%inverted_symop/=self%unique_SF_symop(u)) ) cycle  ! for inversions ...
       call make_phases_for_symop_(self,u,phase,self%inverted_symop)
       do n = 1,n_refl
         ints = -phase(n)*conjg(unique_ft_ints(uf+n,:,:,:))  ! note minus sign
         ft_ints(n,:,:,1) = ft_ints(n,:,:,1) + q(n,2)*(B(1)*ints(:,:,2)-B(2)*ints(:,:,1)) &
                                             - q(n,3)*(B(3)*ints(:,:,1)-B(1)*ints(:,:,3))
         ft_ints(n,:,:,2) = ft_ints(n,:,:,2) + q(n,3)*(B(2)*ints(:,:,3)-B(3)*ints(:,:,2)) &
                                             - q(n,1)*(B(1)*ints(:,:,2)-B(2)*ints(:,:,1))
         ft_ints(n,:,:,3) = ft_ints(n,:,:,3) + q(n,1)*(B(3)*ints(:,:,1)-B(1)*ints(:,:,3)) &
                                             - q(n,2)*(B(2)*ints(:,:,3)-B(3)*ints(:,:,2))
       end do
     end do
     call destroy_(ints)
      ! The factor of two for conversion to Bohr magnetons cancels the
      ! factor of half for the S operator
     ci = (0.0d0,1.0d0)
     do n = 1,n_refl
       ft_ints(n,:,:,:) = -ci*ft_ints(n,:,:,:)/(q(n,1)*q(n,1)+q(n,2)*q(n,2))
     end do
     call destroy_(q)
     call destroy_(phase)

   end subroutine

   subroutine sum_ft_nabla_ints(self,ft_ints,unique_ft_ints)
    type(crystal_type) :: self
    ! Form the fourier transform nabla_a integrals "ft_ints", required for the
    ! PND magnetic striucture factors, from a sum of "unique_ft_ints".
    ! Note: only inversions are allowed as symmetry elements for PND simulations.
    ! Note: the complex conjugate nabla_b integrals are not included.
    ! Dimensions of ft_ints are [.n_refl,n_comp_a,n_comp_b,3].
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:,:) :: ft_ints
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:,:), intent(in) :: unique_ft_ints
      complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: phase
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: q
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:), pointer :: ints
      integer(kind=kind(1)) :: u,uf,n,n_refl

      n_refl = n_refl_(self%reflections)
      call ensure_(tonto,size(ft_ints,1)==n_refl,"CRYSTAL:sum_ft_nabla_ints ... wrong size for ft_ints!")
      call create_(phase,n_refl)
      call create_(q,n_refl,3)
      call create_(ints,size(ft_ints,2),size(ft_ints,3),3)
      call make_k_pts_(self,q)
      ft_ints = 0.0d0
      do u = 1,self%n_unique_SF_symops
         uf = n_refl*(u-1)
         call make_phases_for_symop_(self,u,phase,self%translated_symop)
         do n = 1,n_refl
            ints = phase(n)*unique_ft_ints(uf+n,:,:,:)
            ft_ints(n,:,:,1) = ft_ints(n,:,:,1) + q(n,2)*ints(:,:,3)-q(n,3)*ints(:,:,2)
            ft_ints(n,:,:,2) = ft_ints(n,:,:,2) + q(n,3)*ints(:,:,1)-q(n,1)*ints(:,:,3)
            ft_ints(n,:,:,3) = ft_ints(n,:,:,3) + q(n,1)*ints(:,:,2)-q(n,2)*ints(:,:,1)
         end do
         if ( all(self%inverted_symop/=self%unique_SF_symop(u)) ) cycle  ! for inversions ...
         call make_phases_for_symop_(self,u,phase,self%inverted_symop)
         do n = 1,n_refl
            ints = -phase(n)*conjg(unique_ft_ints(uf+n,:,:,:))  ! note minus sign
            ft_ints(n,:,:,1) = ft_ints(n,:,:,1) + q(n,2)*ints(:,:,3)-q(n,3)*ints(:,:,2)
            ft_ints(n,:,:,2) = ft_ints(n,:,:,2) + q(n,3)*ints(:,:,1)-q(n,1)*ints(:,:,3)
            ft_ints(n,:,:,3) = ft_ints(n,:,:,3) + q(n,1)*ints(:,:,2)-q(n,2)*ints(:,:,1)
         end do
      end do
      call destroy_(ints)
       ! The factor of 2 to convert to Bohr magnetons cancels the factor
       ! of 1/2 for the Bohr magneton
       ! Extra minus sign introduced, but not sure why ...
       ! Reversed minus sign introduced ...
      do n = 1,n_refl
          ft_ints(n,:,:,:) = ft_ints(n,:,:,:)/(q(n,1)*q(n,1)+q(n,2)*q(n,2))
      end do
      call destroy_(q)
      call destroy_(phase)

   end subroutine

   subroutine sum_ft_j_ints(self,ft_ints,unique_ft_ints)
    type(crystal_type) :: self
    ! Form the fourier transform j integrals "ft_ints", required for the PND
    ! magnetic striucture factors, from a sum of "unique_ft_ints".
    ! Note: only inversions are allowed as symmetry elements for PND simulations.
    ! Note: the complex conjugate nabla_b integrals are not included.
    ! Dimensions of ft_ints are [.n_refl,n_comp_a,n_comp_b,3].
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:,:) :: ft_ints
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:,:), intent(in) :: unique_ft_ints
     complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: phase
     integer(kind=kind(1)) :: u,uf, n,n_refl

     n_refl = n_refl_(self%reflections)
     call ensure_(tonto,size(ft_ints,1)==n_refl,"CRYSTAL:sum_ft_j_ints ... incorrect size for array ft_ints")
     call create_(phase,n_refl)
     ft_ints = 0.0d0
     do u = 1,self%n_unique_SF_symops
        uf = n_refl*(u-1)
        call make_phases_for_symop_(self,u,phase,self%translated_symop)
        do n = 1,n_refl
          ft_ints(n,:,:,:) = ft_ints(n,:,:,:) + phase(n)*unique_ft_ints(uf+n,:,:,:)
        end do
        if ( all(self%inverted_symop/=self%unique_SF_symop(u)) ) cycle  ! for inversions...
        call make_phases_for_symop_(self,u,phase,self%inverted_symop)
        do n = 1,n_refl
          ft_ints(n,:,:,:) = ft_ints(n,:,:,:) - phase(n)*conjg(unique_ft_ints(uf+n,:,:,:))
        end do
     end do
     call destroy_(phase)

   end subroutine

!  *****************************************
!  Reduced group, Unique operators, Z number
!  *****************************************

   subroutine make_reduced_group_data(self,atom,prune_asymmetric_unit)
    type(crystal_type) :: self
    ! The reduced group are those unique seitz operators which are needed to
    ! generate the complete ".fragment_cell_geometry" from the atom fragment
    ! positions in "atom".  Some of the spacegroup symmetry operations may only
    ! lead to fragment geometries which are inversions of, or translations of,
    ! other operations in the reduced group.  This information is also worked out
    ! here, and it can be used to save work in structure factor calculations.
    ! If "prune_asymmetric_unit") is present then any non-unique atoms in the
    ! ".asymmetric_unit_geometry" are removed. NOTE: ths is probably not what you
    ! want for strcuture factor calcs, where the fragment geometry becomes the
    ! asymmetric unit, but it IS what you want when processing type(cif_type) files with
    ! possibly overcomplete asymmetric units.
      type(atom_type), dimension(:), intent(in) :: atom
      logical(kind=kind(.true.)), optional, intent(in) :: prune_asymmetric_unit

      if (self%reduced_group_info_made) call destroy_fragment_data_(self)
      self%n_fragment_atoms = size(atom)
      call create_(self%fragment_geometry,3,self%n_fragment_atoms)
      call get_geometry_(atom,self%fragment_geometry)
      call change_into_fractional_(self%unitcell,self%fragment_geometry)
      call make_reduced_symops_(self)          ! These don't seem to be used
      call make_cluster_symops_(self)
      call make_inverted_symops_(self)         ! These are used to save time in SF calcs
      call make_translated_symops_(self)
      call make_unique_SF_symops_(self)
      call make_unique_fragment_atoms_(self)   ! See if we have an asymmetric fragment
      call make_fragment_cell_geometry_(self)  ! These are to get full unit cell geoms
      if (.not. associated(self%asymmetric_unit_geometry)) call make_asymmetric_geometry_(self)
      call make_unit_cell_geometry_(self,prune_asymmetric_unit)
      call make_repetition_factors_(self)      ! Rep. factors for SF calculations
      self%Z = z_factor_(self,atom)
      self%reduced_group_info_made = .true.

   end subroutine

   function z_factor(self,atom) result(res)
    type(crystal_type) :: self
    ! The Z crystallographic factor for ".fragment_geometry", defined as the
    ! ratio of the number of electrons in the unit cell on the number of
    ! electrons in the fragment. Hence, we require information for each "atom" in
    ! the fragment.
      type(atom_type), dimension(:), intent(in) :: atom
      real(kind=kind(1.0d0)) :: res
      real(kind=kind(1.0d0)) :: u,f
      integer(kind=kind(1)) :: n,a

      call ensure_(tonto,associated(self%atom_for_fragment_cell_atom),"CRYSTAL:z_factor ... no atom_for_fragment_cell_atom ar&
&ray")
      u = 0.0d0
      do n = 1,self%n_fragment_cell_atoms
         a = self%atom_for_fragment_cell_atom(n)
         u = u + atom(a)%atomic_number
      end do
      f = 0.0d0
      do n = 1,self%n_fragment_atoms
         f = f + atom(n)%atomic_number
      end do
      res = u/f

   end function

   subroutine make_reduced_symops(self)
    type(crystal_type) :: self
    ! Make a list of the indices of the Seitz matrices, ".reduced_symop", which
    ! will generate different geometries from that in ".fragment_geometry" when
    ! both the original and transformed geometries are converted to unit cell
    ! coordinates.  ".n_reduced_symops" is set to the number of these reduced
    ! symmetry operations.
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: gi,gu
      integer(kind=kind(1)) :: i,j,u, n
      logical(kind=kind(.true.)) :: identical

      call create_(gi,3,self%n_fragment_atoms)
      call create_(gu,3,self%n_fragment_atoms)
      call create_(self%reduced_symop,self%spacegroup%n_seitz)
      n = 1
      self%reduced_symop(1) = 1
      do i = 2,self%spacegroup%n_seitz
         gi = self%fragment_geometry
         call transform_geometry_(self,gi,i,to_unit_cell=.true.)
         do j = 1,n                             ! Loop over reduced symops
            gu = self%fragment_geometry
            u = self%reduced_symop(j)
            call transform_geometry_(self,gu,u,to_unit_cell=.true.)
            identical  = is_same_geometry_(self,gi,gu)
            if (identical) exit
         end do
         if (.not. identical) then
            n = n + 1
            self%reduced_symop(n) = i
         end if
      end do
      self%n_reduced_symops = n
      call shrink_(self%reduced_symop,n)
      call destroy_(gu)
      call destroy_(gi)

   end subroutine

   subroutine make_cluster_symops(self)
    type(crystal_type) :: self
    ! Make a list of the indices of the Seitz matrices, ".cluster_symop", which
    ! will generate different geometries from that in ".fragment_geometry".
    ! This routine is the same as make_reduced_symops except that the geometries
    ! are *not* converted to unit cell coordinates.  ".n_cluster_symops" is set
    ! to the number of these symmetry operations.  These operators are useful for
    ! generating clusters of this fragment. NOTE: the unit operator, element 1,
    ! is part of the set of cluster_symops.
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: gi,gu
      integer(kind=kind(1)) :: i,j,u, n
      logical(kind=kind(.true.)) :: identical

      call create_(gi,3,self%n_fragment_atoms)
      call create_(gu,3,self%n_fragment_atoms)
      call create_(self%cluster_symop,self%spacegroup%n_seitz)
      n = 1
      self%cluster_symop(1) = 1
      do i = 2,self%spacegroup%n_seitz
         gi = self%fragment_geometry
         call transform_geometry_(self,gi,i)
         do j = 1,n                        ! Loop over cluster symops
            gu = self%fragment_geometry
            u = self%cluster_symop(j)
            call transform_geometry_(self,gu,u)
            identical  = is_same_geometry_(self,gi,gu)
            if (identical) exit
         end do
         if (.not. identical) then
            n = n + 1
            self%cluster_symop(n) = i
         end if
      end do
      self%n_cluster_symops = n
      call shrink_(self%cluster_symop,n)
      call destroy_(gu)
      call destroy_(gi)

   end subroutine

   subroutine make_inverted_symops(self)
    type(crystal_type) :: self
    ! Determine which of the reduced symops can generate geometries from
    ! ".fragment cell_geometry" which are related by inversion.
      integer(kind=kind(1)) :: n,i,j
      logical(kind=kind(.true.)) :: inverted

      call ensure_(tonto,associated(self%reduced_symop),"CRYSTAL:make_inverted_symops ... no reduced_symop array")
      call ensure_(tonto,associated(self%spacegroup%seitz),"CRYSTAL:make_inverted_symops ... Seitz matrices not initialised")
      call create_(self%inverted_symop,self%spacegroup%n_seitz)
      self%inverted_symop = 0
      n = 0
      do i = 2,self%spacegroup%n_seitz
         do j = 1,i-1
            inverted = equals_(self%spacegroup%seitz(1:3,1:3,i), &
                      -self%spacegroup%seitz(1:3,1:3,j))
            if (inverted) then
               n = n + 1
               self%inverted_symop(i) = j
               exit
            end if
         end do
      end do
      self%n_inverted_symops = n

   end subroutine

   subroutine make_translated_symops(self)
    type(crystal_type) :: self
    ! Determine which of the reduced symops can generate geometries from
    ! ".fragment_cell_geometry" which are related by translation, (including
    ! translation by the zero vector), but not inversion
      integer(kind=kind(1)) :: i,j
      logical(kind=kind(.true.)) :: translated

      call ensure_(tonto,associated(self%inverted_symop),"CRYSTAL:make_translated_symops ... no inverted_symop array")
      call ensure_(tonto,associated(self%spacegroup%seitz),"CRYSTAL:make_translated_symops ... Seitz matrices not initialised&
&")
      call create_(self%translated_symop,self%spacegroup%n_seitz)
      self%translated_symop = 0
      do i = 1,self%spacegroup%n_seitz
         self%translated_symop(i) = i
         if (self%inverted_symop(i)>0) cycle
         do j = 1,i-1
            translated = equals_(self%spacegroup%seitz(1:3,1:3,i), &
                         self%spacegroup%seitz(1:3,1:3,j) )
            if (translated) then
               self%translated_symop(i) = j
               exit
            end if
         end do
      end do

   end subroutine

   subroutine make_unique_SF_symops(self)
    type(crystal_type) :: self
    ! Determine which are the structure-factor unique symops, i.e. those which
    ! generate fragment geometries which are different than a translation or
    ! inversion. This list should be a superset of the reduced symops.
      integer(kind=kind(1)) :: n,i

      call ensure_(tonto,associated(self%inverted_symop),"CRYSTAL:make_unique_SF_symops ... no inverted_symop array")
      call ensure_(tonto,associated(self%translated_symop),"CRYSTAL:make_unique_SF_symops ... no translated_symop array")
      n = 0
      do i = 1,self%spacegroup%n_seitz
         if (self%inverted_symop(i)>0) cycle
         if (self%translated_symop(i)<i) cycle  ! Only true translations count
         n = n + 1
      end do
      self%n_unique_SF_symops = n
      call create_(self%unique_SF_symop,n)
      n = 0
      do i = 1,self%spacegroup%n_seitz
         if (self%inverted_symop(i)>0) cycle
         if (self%translated_symop(i)<i) cycle
         n = n + 1
         self%unique_SF_symop(n) = i
      end do

   end subroutine

   function is_same_geometry(self,geom_i,geom_j) result(res)
    type(crystal_type) :: self
    ! Return .true. if the geometries "geom_i" and "geom_j" in fractional
    ! coordinates are the same, exactly.
      real(kind=kind(1.0d0)), dimension(:,:) :: geom_i,geom_j
      logical(kind=kind(.true.)) :: res
      integer(kind=kind(1)) :: i,j,n_atom
      logical(kind=kind(.true.)) :: same
      logical(kind=kind(.true.)), dimension(:), pointer :: skip

      call ensure_(tonto,associated(self%spacegroup%seitz),"CRYSTAL:is_same_geometry ... Seitz matrices not initialised")
      call ensure_(tonto,size(geom_i,1)==3,"CRYSTAL:is_same_geometry ... incorrect size for array geom_i")
      call ensure_(tonto,size(geom_j,1)==3,"CRYSTAL:is_same_geometry ... incorrect size for array geom_j")
      call ensure_(tonto,size(geom_i,2)==size(geom_j,2),"CRYSTAL:is_same_geometry ... incompatible sizes for geom_i, geom_j")
      n_atom = size(geom_i,2)
      call create_(skip,n_atom); skip(:) = .false.
      do i = 1,n_atom
         do j = 1,n_atom
            same = same_as_(geom_i(:,i),geom_j(:,j), 10.0d0**(-3))
            if (.not. same .or. skip(j)) cycle
            skip(j) = .true.
            exit
         end do
      end do
      res = all(skip)  ! True if all atoms in i were matched (skipped) in j
      call destroy_(skip)

   end function

   subroutine make_unique_fragment_atoms(self)
    type(crystal_type) :: self
    ! Make a list of the symmetry unique atoms in the ".fragment_geometry". The
    ! atoms in ".fragment_geometry" are first put to the unit cell; then we
    ! transform each of them in turn by all the symmetry operations (placing them
    ! back into the unit cell, if necessary) and accumulating only the unique
    ! ones.
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: geometry
      real(kind=kind(1.0d0)), dimension(3) :: pa
      integer(kind=kind(1)) :: u,a,s,col
      logical(kind=kind(.true.)) :: found

      call ensure_(tonto,associated(self%fragment_geometry),"CRYSTAL:make_unique_fragment_atoms ... no fragment_geometry")
      call create_(self%unique_atom_for_fragment_atom,self%n_fragment_atoms)
      self%unique_atom_for_fragment_atom    = 0
      self%unique_atom_for_fragment_atom(1) = 1
      call create_(self%unique_fragment_atom,1)
      self%unique_fragment_atom(1) = 1
      call create_(self%unique_symop_for_fragment_atom,self%n_fragment_atoms)
      self%unique_symop_for_fragment_atom = 1
      call create_(geometry,3,self%n_fragment_atoms)
      geometry = self%fragment_geometry
      call put_to_unit_cell_(self,geometry)
      u = 1
      do a = 2,self%n_fragment_atoms
         found = .false.
         do s = 1,self%spacegroup%n_seitz
            pa = geometry(:,a)
            call transform_position_(self,pa,s,to_unit_cell=.true.)
            found = has_column_(geometry(:,self%unique_fragment_atom),pa,10.0d0**(-3),col)
            if (self%unique_atom_for_fragment_atom(a)==0 .and. found) then
               self%unique_atom_for_fragment_atom(a) = col
               self%unique_symop_for_fragment_atom(a) = s
               exit
            end if
         end do
         if (.not. found) then
            u = u + 1
            self%unique_atom_for_fragment_atom(a) = u
            call expand_(self%unique_fragment_atom,u)
            self%unique_fragment_atom(u) = a
         end if
      end do
      self%n_unique_fragment_atoms = u
      call destroy_(geometry)

   end subroutine

   subroutine make_fragment_cell_geometry(self)
    type(crystal_type) :: self
    ! Get the all the fragment atom positions in the unit cell,
    ! ".fragment_cell_geometry", given a (possibly) partial or overcomplete set
    ! for the cell in array ".fragment_geometry".
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: geometry
      real(kind=kind(1.0d0)), dimension(3) :: pa
      integer(kind=kind(1)) :: n,a,s
      logical(kind=kind(.true.)) :: found

      call ensure_(tonto,associated(self%fragment_geometry),"CRYSTAL:make_fragment_cell_geometry ... no fragment_geometry")
      nullify(self%fragment_cell_geometry)
      call create_(geometry,3,self%n_fragment_atoms)  ! added this
      geometry = self%fragment_geometry  ! added this
      call put_to_unit_cell_(self,geometry)  ! added this
      n = 0
      do a = 1,self%n_fragment_atoms
         do s = 1,self%spacegroup%n_seitz
            pa = geometry(:,a)  ! added this
            call transform_position_(self,pa,s,to_unit_cell=.true.)
            if (.not. associated(self%fragment_cell_geometry)) then
              found = .false.
            else
              found = has_column_(self%fragment_cell_geometry,pa,10.0d0**(-3))
            end if
            if (.not. found) then
               n = n + 1
               if (n==1) then
                 call create_(self%atom_for_fragment_cell_atom,1)
                 call create_(self%symop_for_fragment_cell_atom,1)
                 call create_(self%fragment_cell_geometry,3,1)
               else
                 call expand_(self%atom_for_fragment_cell_atom,n)
                 call expand_(self%symop_for_fragment_cell_atom,n)
                 call expand_(self%fragment_cell_geometry,3,n)
               end if
               self%atom_for_fragment_cell_atom(n) = a
               self%symop_for_fragment_cell_atom(n) = s
               self%fragment_cell_geometry(:,n) = pa
            end if
         end do
      end do
      self%n_fragment_cell_atoms = n
      call destroy_(geometry)
      call ensure_(tonto,associated(self%fragment_cell_geometry),"CRYSTAL:make_fragment_cell_geometry ... could not make frag&
&ment_cell-geometry")

   end subroutine

   subroutine make_unit_cell_geometry(self,prune_asymmetric_unit)
    type(crystal_type) :: self
    ! Get the all the atom positions in the unit cell, ".unit_cell_geometry" from
    ! the ".asymmetric_unit_geometry", if it is created. Also checks if the
    ! ".asymmetric_unit_geometry" is really an asymmetric unit! If
    ! "prune_asymmetric_unit" is present and true, and non-asymmetric unit atoms
    ! are eliminated.
      logical(kind=kind(.true.)), optional :: prune_asymmetric_unit
      logical(kind=kind(.true.)) :: prune
      real(kind=kind(1.0d0)), dimension(3) :: pa
      integer(kind=kind(1)) :: n,a,m,s,col
      logical(kind=kind(.true.)) :: found,non_unique
      real(kind=kind(1.0d0)) :: tol

      call ensure_(tonto,associated(self%asymmetric_unit_geometry),"CRYSTAL:make_unit_cell_geometry ... no asymmetric unit ge&
&ometry")
      prune = .false.
      if (present(prune_asymmetric_unit)) prune = prune_asymmetric_unit
      nullify(self%unit_cell_geometry)
      tol = 10.0d0**(-3)/maxval(self%unitcell%length)
      n = 0
      a = 1
      do  ! a = 1,.n_asymmetric_unit_atoms
         do s = 1,self%spacegroup%n_seitz
            pa = self%asymmetric_unit_geometry(:,a)
            call transform_position_(self,pa,s,to_unit_cell=.true.)
            non_unique = has_column_(self%asymmetric_unit_geometry,pa,tol,col)
            non_unique = non_unique .and. col/=a
            if (non_unique) then
               call warn_(tonto,"CRYSTAL:make_unit_cell_geometry ... asymmetric unit atoms "//trim(to_str_(a))//" and "//trim&
&(to_str_(col))//" are the same!")
               call warn_(tonto,"CRYSTAL:make_unit_cell_geometry ... this may indicate a disordered aysmmetric unit")
               if (prune) then
                  m = max(a,col)
                  call warn_(tonto,"CRYSTAL:make_unit_cell_geometry ... pruning atom "//trim(to_str_(m))//" from asymmetric u&
&nit atom list")
                  self%n_asymmetric_unit_atoms = self%n_asymmetric_unit_atoms - 1
                  self%asymmetric_unit_geometry(:,m:self%n_asymmetric_unit_atoms) = self%asymmetric_unit_geometry(:,m+1:)
                  call shrink_columns_(self%asymmetric_unit_geometry,self%n_asymmetric_unit_atoms)
                  exit
               end if
            end if
            if (.not. associated(self%unit_cell_geometry)) then
              found = .false.
            else
              found = has_column_(self%unit_cell_geometry,pa,10.0d0**(-3))
            end if
            if (.not. found) then
               n = n + 1
               if (n==1) then
                 call create_(self%atom_for_unit_cell_atom,1)
                 call create_(self%symop_for_unit_cell_atom,1)
                 call create_(self%unit_cell_geometry,3,1)
               else
                 call expand_(self%atom_for_unit_cell_atom,n)
                 call expand_(self%symop_for_unit_cell_atom,n)
                 call expand_(self%unit_cell_geometry,3,n)
               end if
               self%atom_for_unit_cell_atom(n) = a
               self%symop_for_unit_cell_atom(n) = s
               self%unit_cell_geometry(:,n) = pa
            end if
         end do
         a = a + 1
         if (a>self%n_asymmetric_unit_atoms) exit
      end do
      self%n_unit_cell_atoms = n
      call ensure_(tonto,associated(self%unit_cell_geometry),"CRYSTAL:make_unit_cell_geometry ... could not make unit_cell_ge&
&ometry")

   end subroutine

   subroutine make_repetition_factors(self)
    type(crystal_type) :: self
    ! The number of times an atom with index "a" is mapped into itself under
    ! the reduced group is the ".repetition_factor(a)". It is used to correct
    ! structure factors for fragment geometries which are "oversampled" relative
    ! to the asymmetric cell geometry.
      real(kind=kind(1.0d0)), dimension(3) :: pa,pb
      integer(kind=kind(1)) :: a,b,n,n_same
      logical(kind=kind(.true.)) :: same

      call ensure_(tonto,associated(self%fragment_geometry),"CRYSTAL:make_repetition_factors ... no fragment_geometry!")
      call create_(self%repetition_factor,self%n_fragment_atoms)
      do a = 1,self%n_fragment_atoms
         pa = self%fragment_geometry(:,a)
         call put_to_unit_cell_(self,pa)
         n_same = 0
         do b = 1,self%n_fragment_atoms
         do n = 1,self%spacegroup%n_seitz
            pb = self%fragment_geometry(:,b)
            call transform_position_(self,pb,n,to_unit_cell=.true.)
            same = same_as_(pa,pb,10.0d0**(-3))
            if (same) n_same = n_same + 1
         end do
         end do
         self%repetition_factor(a) = n_same
      end do

   end subroutine

   subroutine transform_geometry(self,g,op,translate,ignore_glide,to_unit_cell)
    type(crystal_type) :: self
    ! Transform the positions "g" in fractional coordinates with the
    ! Seitz operator with index "op". If present, "translate" will be
    ! added to the transformed position. If present and .true., "ignore_glide"
    ! will not add the glide vector part of the Seitz operator.
    ! If present and .true., "to_unit_cell" will translate the fractional
    ! coordinates into the (1,1,1) unit cell.
      real(kind=kind(1.0d0)), dimension(:,:) :: g
      integer(kind=kind(1)) :: op
      integer(kind=kind(1)), dimension(3), optional :: translate
      logical(kind=kind(.true.)), optional :: ignore_glide,to_unit_cell
      integer(kind=kind(1)) :: n

      call ensure_(tonto,size(g,1)==3,"CRYSTAL:transform_geometry ... incorrect size for array g")
      do n = 1,size(g,2)
         call transform_position_(self,g(:,n),op,translate,ignore_glide,to_unit_cell)
      end do

   end subroutine

   subroutine transform_position(self,p,op,translate,ignore_glide,to_unit_cell)
    type(crystal_type) :: self
    ! Transform the position "p" in fractional coordinates with the
    ! Seitz operator with index "op". If present, "translate" will be
    ! added to the transformed position. If present and .true., "ignore_glide"
    ! will not add the glide vector part of the Seitz operator.
    ! If present and .true., "to_unit_cell" will translate the fractional
    ! coordinates into the (1,1,1) unit cell.
      real(kind=kind(1.0d0)), dimension(3) :: p
      integer(kind=kind(1)) :: op
      integer(kind=kind(1)), dimension(3), optional :: translate
      logical(kind=kind(.true.)), optional :: ignore_glide,to_unit_cell
      logical(kind=kind(.true.)) :: ignore,to_cell

      call ensure_(tonto,associated(self%spacegroup%seitz),"CRYSTAL:transform_position ... Seitz matrices not initialised")
      call ensure_(tonto,op>0,"CRYSTAL:transform_position ... operator index out of bounds")
      call ensure_(tonto,op<=self%spacegroup%n_seitz,"CRYSTAL:transform_position ... operator index out of bounds")
      ignore = .false.
      if (present(ignore_glide)) ignore = ignore_glide
      if (ignore) then
        p = matmul(self%spacegroup%seitz(1:3,1:3,op),p)
      else
        p = matmul(self%spacegroup%seitz(1:3,1:3,op),p) + self%spacegroup%seitz(1:3,4,op)
      end if
      if (present(translate)) p = p + translate
      to_cell = .false.
      if (present(to_unit_cell)) to_cell = to_unit_cell
      if (to_cell) call put_to_unit_cell_(self,p)

   end subroutine

   subroutine put_to_unit_cell(self,g)
    type(crystal_type) :: self
    ! Transform the geometry "g" in fractional coordinates into the
    ! (1,1,1) unit cell.  All atoms will be shifted into the first unit cell
    ! independently, so the resulting geometry may not reflect the shape of the
    ! original molecule.
      real(kind=kind(1.0d0)), dimension(:,:) :: g
      integer(kind=kind(1)) :: n,n_atom

      call ensure_(tonto,size(g,1)==3,"CRYSTAL:put_to_unit_cell ... incorrect size for array g")
      n_atom = size(g,2)
      do n = 1,n_atom
         call put_to_unit_cell_(self,g(:,n))
      end do

   end subroutine

   subroutine put_to_unit_cell_1(self,p)
    type(crystal_type) :: self
    ! Transform the position "p" in fractional coordinates into the
    ! (1,1,1) unit cell.
      real(kind=kind(1.0d0)), dimension(3) :: p

      call ensure_(tonto,size(p)==3,"CRYSTAL:put_to_unit_cell_1 ... p must be length 3")
      p(:) = mod(p(:)-floor(p(:))+2.0d0+0.001d0,1.0d0) - 0.001d0
       ! p(:)-floor(p(:))+2.0d0 should make it positive.

   end subroutine

   subroutine move_to_unit_cell(self,g)
    type(crystal_type) :: self
    ! Transform the geometry "g" in fractional coordinates into the first
    ! lattice cell.  The shape of the molecule remains intact, so some of it
    ! may cross into other cells.  The center of the molecule will be in the
    ! first cell.
      real(kind=kind(1.0d0)), dimension(:,:) :: g
      integer(kind=kind(1)) :: n,n_atom
      integer(kind=kind(1)), dimension(3) :: centre

      call ensure_(tonto,size(g,1)==3,"CRYSTAL:move_to_unit_cell ... incorrect size for array g")
      n_atom = size(g,2)
      centre = unit_cell_offset_(self,g)
      do n = 1, n_atom
        g(:,n) = g(:,n) - centre
      end do

   end subroutine

   function unit_cell_offset(self,g) result(res)
    type(crystal_type) :: self
    ! Which hkl indices match the centre of the fragment geometry "g" when
    ! expressed in crystal coordinates.  Usually we expect that it is 0,0,0 but
    ! not always.
      real(kind=kind(1.0d0)), dimension(:,:) :: g
      integer(kind=kind(1)), dimension(3) :: res

      call ensure_(tonto,size(g,1)==3,"CRYSTAL:unit_cell_offset ... incorrect size for array g")
      res = sum(g,dim=2)/size(g,2)

   end function

   function fragment_width(self) result(res)
    type(crystal_type) :: self
    ! Return the width "res" of the fragment in each of the 3 axis directions.
      real(kind=kind(1.0d0)), dimension(3) :: res

      call ensure_(tonto,associated(self%fragment_geometry),"CRYSTAL:fragment_width ... no fragment geometry")
      res = max_abs_column_difference_(self%fragment_geometry)

   end function

   function cartesian_fragment_width(self) result(res)
    type(crystal_type) :: self
    ! Return the cartesian width "res" of the fragment in each of the three axis
    ! directions.
      real(kind=kind(1.0d0)), dimension(3) :: res

      res = fragment_width_(self)
      call change_from_fractional_(self%unitcell,res)

   end function

   function reduced_symop_mat(self,r) result(res)
    type(crystal_type) :: self
    ! Return the "r"-th reduced symop matrix in the unique list made by routine
    ! ".make_reduced_symops".
      real(kind=kind(1.0d0)), dimension(3,3) :: res
      integer(kind=kind(1)) :: r

      call ensure_(tonto,associated(self%reduced_symop),"CRYSTAL:reduced_symop_mat ... no reduced_symops!")
      call ensure_(tonto,r<=self%n_reduced_symops,"CRYSTAL:reduced_symop_mat ... symop index out of range")
      call ensure_(tonto,r>0,"CRYSTAL:reduced_symop_mat ... symop index out of range")
      res = self%spacegroup%seitz(1:3,1:3,self%reduced_symop(r))

   end function

   function unique_SF_symop_mat(self,u) result(res)
    type(crystal_type) :: self
    ! Return the "u"-th reduced symop matrix in the unique list made by routine
    ! ".make_reduced_symops".
      real(kind=kind(1.0d0)), dimension(3,3) :: res
      integer(kind=kind(1)) :: u
      integer(kind=kind(1)) :: r

      call ensure_(tonto,associated(self%unique_SF_symop),"CRYSTAL:unique_SF_symop_mat ... no unique_SF_symops!")
      call ensure_(tonto,u<=self%n_unique_SF_symops,"CRYSTAL:unique_SF_symop_mat ... symop index out of range")
      call ensure_(tonto,u>0,"CRYSTAL:unique_SF_symop_mat ... symop index out of range")
      r = self%unique_SF_symop(u)
      res = self%spacegroup%seitz(1:3,1:3,r)
    !  res = .spacegroup.seitz(1:3,1:3,.reduced_symop(r))

   end function

   function transposed_xyz_seitz_matrices(self) result(res)
    type(crystal_type) :: self
    ! Create and return a list of the 3x3 part of the seitz matrices
    ! in the cartesian axis system. NOTE: these are actually the
    ! *transpose* of the matrices in Hall's paper.
    !               S^T_cartesian  =  B  S^T_crystal  B^-1
      real(kind=kind(1.0d0)), dimension(:,:,:), pointer :: res
      integer(kind=kind(1)) :: i

      call ensure_(tonto,associated(self%spacegroup%seitz),"CRYSTAL:transposed_xyz_seitz_matrices ... no Seitz matrices")
      call create_(res,3,3,self%spacegroup%n_seitz)
      do i = 1,self%spacegroup%n_seitz
         res(:,:,i) = matmul(self%unitcell%reciprocal_matrix, &
                      matmul(transpose(self%spacegroup%seitz(1:3,1:3,i)), &
                             transpose(self%unitcell%direct_matrix)))
      end do

   end function

!  **************
!  Output methods
!  **************

   subroutine put(self,atom)
    type(crystal_type) :: self
    ! Put out the crystal data to file "out"
      type(atom_type), dimension(:), optional :: atom

      call flush_(stdout)
      call text_(stdout,"CRYSTAL information:")
      call show_(stdout,"kind                        = ", self%data_kind)
      call put_(self%unitcell)
      call put_(self%spacegroup)
      if (associated(self%asymmetric_unit_geometry)) call put_asymmetric_unit_geometry_(self,atom)
      if (associated(self%unit_cell_geometry))       call put_unit_cell_geometry_(self,atom)
      if (associated(self%fragment_geometry))        call put_fragment_data_(self,atom)
      if (associated(self%reflections))              call put_reflection_data_(self)

   end subroutine

   subroutine put_stl(self)
    type(crystal_type) :: self
    ! Output sin(theta)/lambda for all reflections.
     integer(kind=kind(1)) :: n

     call ensure_(tonto,associated(self%reflections),"CRYSTAL:put_stl ... No list of reflections")
     call ensure_(tonto,have_indices_(self%reflections),"CRYSTAL:put_stl ... No list of reflections")
     call text_(stdout,"sin(theta)/lambda for the reflections")
     call dash_(stdout,int_fields=3,real_fields=1)
     call put_(stdout,"h",int_width=.true.)
     call put_(stdout,"k",int_width=.true.)
     call put_(stdout,"l",int_width=.true.)
     call put_(stdout,"stl")
     call flush_(stdout)
     call dash_(stdout,int_fields=3,real_fields=1)
     do n=1,size(self%reflections)
       call put_(stdout,self%reflections(n)%h)
       call put_(stdout,self%reflections(n)%k)
       call put_(stdout,self%reflections(n)%l)
       call put_(stdout,stl_(self,n))
       call flush_(stdout)
     end do
     call dash_(stdout,int_fields=3,real_fields=1)

   end subroutine

   subroutine put_fragment_data(self,atom)
    type(crystal_type) :: self
    ! Put fragment information to file "out". Optional "atom" list may
    ! be used to enhace output.
      type(atom_type), dimension(:), optional :: atom

      call flush_(stdout)
      call text_(stdout,"Crystal fragment data:")
      call flush_(stdout)
      call show_(stdout,"No. of inputted atoms        = ",self%n_fragment_atoms,real_width=.true.)
      call show_(stdout,"No. of fragment cell atoms   = ",self%n_fragment_cell_atoms,real_width=.true.)
      call show_(stdout,"No. of unique fragment atoms = ",self%n_unique_fragment_atoms,real_width=.true.)
      call show_(stdout,"Z factor                     = ",self%Z)
      call flush_(stdout)
      call show_(stdout,"Fragment partition model     = ",self%partition_model)
      call show_(stdout,"Thermal smearing model       = ",self%thermal_smearing_model)
     ! .put_reduced_symop_data
      call put_inv_trans_symop_data_(self)
      call put_fragment_geometry_(self,atom)
      call put_unique_fragment_geometry_(self,atom)
      call put_repetition_factors_(self,atom)

   end subroutine

   subroutine put_reduced_symop_data(self)
    type(crystal_type) :: self
    ! Put out the reduced symop data
      integer(kind=kind(1)) :: n,s
      logical(kind=kind(.true.)) :: inverted,translated

      call ensure_(tonto,associated(self%fragment_geometry),"CRYSTAL:put_reduced_symop_data ... fragment_geometry?")
      call ensure_(tonto,associated(self%unique_atom_for_fragment_atom),"CRYSTAL:put_reduced_symop_data ... no unique_atom_fo&
&r_fragment_atom array!")
      call flush_(stdout)
      call text_(stdout,"Crystal fragment reduced group information:")
      call flush_(stdout)
      call text_(stdout,"NOTE: this table is NOT used any more in structure factor calculations")
      call flush_(stdout)
      call dash_(stdout,int_fields=5)
      call put_(stdout,"Reduced",int_width=.true.)
      call put_(stdout,"Seitz",int_width=.true.)
      call put_(stdout,"Inv.",int_width=.true.)
      call put_(stdout,"Trans.",int_width=.true.)
      call flush_(stdout)
      call put_(stdout,"Symop",int_width=.true.)
      call put_(stdout,"Symop",int_width=.true.)
      call put_(stdout,"of?",int_width=.true.)
      call put_(stdout,"of?",int_width=.true.)
      call put_(stdout,"Unique?",int_width=.true.)
      call flush_(stdout)
      call dash_(stdout,int_fields=5)
      do n = 1,self%n_reduced_symops
         call put_(stdout,n)
         s = self%reduced_symop(n)
         call put_(stdout,s)
         inverted = self%inverted_symop(s)>0
         if (inverted) then;   call put_(stdout,self%inverted_symop(s))
         else;                 call tab_(stdout,int_fields=1)
         end if
         translated = self%translated_symop(s)<n
         call put_(stdout,self%translated_symop(s))
         if (inverted .or. translated) then; call put_(stdout,"No",int_width=.true.)
         else;                             call put_(stdout,"Yes",int_width=.true.)
         end if
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=5)

   end subroutine

   subroutine put_inv_trans_symop_data(self)
    type(crystal_type) :: self
    ! Put out the inverted translated symop data
      integer(kind=kind(1)) :: n
      logical(kind=kind(.true.)) :: inverted,translated

      call ensure_(tonto,associated(self%fragment_geometry),"CRYSTAL:put_inv_trans_symop_data ... fragment_geometry?")
      call ensure_(tonto,associated(self%unique_atom_for_fragment_atom),"CRYSTAL:put_inv_trans_symop_data ... no unique_atom_&
&for_fragment_atom array")
      call flush_(stdout)
      call text_(stdout,"Crystal Inversion/Translation related symop information:")
      call flush_(stdout)
      call text_(stdout,"NOTE: this table IS used in structure factor calculations")
      call flush_(stdout)
      call dash_(stdout,int_fields=4)
      call put_(stdout,"Seitz",int_width=.true.)
      call put_(stdout,"Inv.",int_width=.true.)
      call put_(stdout,"Trans.",int_width=.true.)
      call flush_(stdout)
      call put_(stdout,"Symop",int_width=.true.)
      call put_(stdout,"of?",int_width=.true.)
      call put_(stdout,"of?",int_width=.true.)
      call put_(stdout,"Unique?",int_width=.true.)
      call flush_(stdout)
      call dash_(stdout,int_fields=4)
      do n = 1,self%spacegroup%n_seitz
         call put_(stdout,n)
         inverted = self%inverted_symop(n)>0
         if (inverted) then;   call put_(stdout,self%inverted_symop(n))
         else;                 call tab_(stdout,int_fields=1)
         end if
         translated = self%translated_symop(n)<n
         call put_(stdout,self%translated_symop(n))
         if (inverted .or. translated) then; call put_(stdout,"No",int_width=.true.)
         else;                             call put_(stdout,"Yes",int_width=.true.)
         end if
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=4)

   end subroutine

   subroutine put_fragment_geometry(self,atom)
    type(crystal_type) :: self
    ! Put out the fragment_geometry information
      type(atom_type), dimension(:), optional :: atom
      character(128) :: symbol
      integer(kind=kind(1)) :: n

      call ensure_(tonto,associated(self%fragment_geometry),"CRYSTAL:put_fragment_geometry ... fragment_geometry?")
      call ensure_(tonto,associated(self%unique_atom_for_fragment_atom),"CRYSTAL:put_fragment_geometry ... no unique_atom_for&
&_fragment_atom array")
      call flush_(stdout)
      call text_(stdout,"Crystal fragment cell geometry:")
      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=3)
      call put_(stdout,"Fragment",int_width=.true.)
      call put_(stdout,"Unique",int_width=.true.)
      call flush_(stdout)
      call put_(stdout,"Atom",int_width=.true.)
      call put_(stdout,"Atom",int_width=.true.)
      call put_(stdout,"x")
      call put_(stdout,"y")
      call put_(stdout,"z")
      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=3)
      do n = 1,self%n_fragment_atoms
         if (present(atom)) then
            symbol = trim(chemical_symbol_(atom(n))) // " (" // trim(to_str_(n)) // ")"
            call put_(stdout,symbol,int_width=.true.)
         else
            call put_(stdout,n)
         end if
         call put_(stdout,self%unique_atom_for_fragment_atom(n))
         call put_(stdout,self%fragment_geometry(1,n))
         call put_(stdout,self%fragment_geometry(2,n))
         call put_(stdout,self%fragment_geometry(3,n))
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=2,real_fields=3)

   end subroutine

   subroutine put_unique_fragment_geometry(self,atom)
    type(crystal_type) :: self
    ! Put out the unique fragment_geometry information.  These are unique atoms
    ! in fragment_geometry, and so are not necessarily the same as the asymmetric
    ! unit atoms.
      type(atom_type), dimension(:), optional :: atom
      integer(kind=kind(1)) :: n,u
      character(128) :: symbol

      call ensure_(tonto,associated(self%unique_fragment_atom),"CRYSTAL:put_unique_fragment_geometry ... unique_fragment_atom&
&s?")
      call ensure_(tonto,associated(self%fragment_geometry),"CRYSTAL:put_unique_fragment_geometry ... fragment_geometry?")
      call flush_(stdout)
      call text_(stdout,"Crystal unique atom unit cell geometry:")
      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=3)
      call put_(stdout,"Unique",int_width=.true.)
      call put_(stdout,"Fragment",int_width=.true.)
      call flush_(stdout)
      call put_(stdout,"Atom",int_width=.true.)
      call put_(stdout,"Atom",int_width=.true.)
      call put_(stdout,"x")
      call put_(stdout,"y")
      call put_(stdout,"z")
      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=3)
      do u = 1,self%n_unique_fragment_atoms
         call put_(stdout,u)
         n = self%unique_fragment_atom(u)
         if (present(atom)) then
            symbol = trim(chemical_symbol_(atom(n))) // " (" // trim(to_str_(n)) // ")"
            call put_(stdout,symbol,int_width=.true.)
         else
            call put_(stdout,n)
         end if
         call put_(stdout,self%fragment_geometry(1,n))
         call put_(stdout,self%fragment_geometry(2,n))
         call put_(stdout,self%fragment_geometry(3,n))
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=2,real_fields=3)

   end subroutine

   subroutine put_fragment_cell_geometry(self,atom)
    type(crystal_type) :: self
    ! Put out the full fragment cell geometry information
      type(atom_type), dimension(:), optional :: atom
      integer(kind=kind(1)) :: n,f
      character(128) :: symbol

      call flush_(stdout)
      call text_(stdout,"Full fragment cell geometry:")
      call flush_(stdout)
      call dash_(stdout,int_fields=3,real_fields=3)
      call put_(stdout,"Cell ",int_width=.true.)
      call put_(stdout,"Fragment",int_width=.true.)
      call flush_(stdout)
      call put_(stdout,"Atom",int_width=.true.)
      call put_(stdout,"Atom",int_width=.true.)
      call put_(stdout,"x")
      call put_(stdout,"y")
      call put_(stdout,"z")
      call put_(stdout,"symop",int_width=.true.)
      call flush_(stdout)
      call dash_(stdout,int_fields=3,real_fields=3)
      do n = 1,self%n_fragment_cell_atoms
         call put_(stdout,n)
         if (present(atom)) then
            f = self%atom_for_fragment_cell_atom(n)
            symbol = trim(chemical_symbol_(atom(f))) // " (" // trim(to_str_(f)) // ")"
         else
            call put_(stdout,f)
         end if
         call put_(stdout,symbol,int_width=.true.)
         call put_(stdout,self%fragment_cell_geometry(1,n))
         call put_(stdout,self%fragment_cell_geometry(2,n))
         call put_(stdout,self%fragment_cell_geometry(3,n))
         call put_(stdout,self%symop_for_fragment_cell_atom(n))
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=3,real_fields=3)

   end subroutine

   subroutine put_repetition_factors(self,atom)
    type(crystal_type) :: self
    ! Put out the repetition factors
      type(atom_type), dimension(:), optional :: atom
      integer(kind=kind(1)) :: n,rf
      character(128) :: symbol

      call flush_(stdout)
      call text_(stdout,"Crystal fragment atom repetition factors:")
      call flush_(stdout)
      call dash_(stdout,int_fields=2)
      call put_(stdout,"Fragment",int_width=.true.)
      call put_(stdout,"Rep.",int_width=.true.)
      call flush_(stdout)
      call put_(stdout,"Atom",int_width=.true.)
      call put_(stdout,"Factor",int_width=.true.)
      call flush_(stdout)
      call dash_(stdout,int_fields=2)
      do n = 1,self%n_fragment_atoms
         if (present(atom)) then
            symbol = trim(chemical_symbol_(atom(n))) // " (" // trim(to_str_(n)) // ")"
            call put_(stdout,symbol,int_width=.true.)
         else
            call put_(stdout,n)
         end if
         rf = self%repetition_factor(n)
         call put_(stdout,rf)
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=2)

   end subroutine

   subroutine put_F_calc(self)
    type(crystal_type) :: self
    ! Put the hkl Miller index triple followed by the calculated
    ! structure factor to add onto existing structure factors.

      call ensure_(tonto,associated(self%reflections),"CRYSTAL:put_F_calc ... no reflection data")
      call set_keys_(self%reflections,(/"indices","F_calc "/))
      call put_(self%reflections)
      call clear_keys_(self%reflections)

   end subroutine

   subroutine put_F_stats(self)
    type(crystal_type) :: self
    ! Output the structure factor goodness of fit statistics
    ! stdout.

      call put_F_stats_(self%reflections)
      call put_correction_data_(self)

   end subroutine

   subroutine put_I_stats(self)
    type(crystal_type) :: self
    ! Output the intensity goodness of fit statistics

      call put_I_stats_(self%reflections)
      call put_correction_data_(self)

   end subroutine

   subroutine put_correction_data(self)
    type(crystal_type) :: self
    ! Output the correction data
     logical(kind=kind(.true.)) :: real_width

     real_width = .true.
     call flush_(stdout)
     call show_(stdout,"Using scale factor          = ", self%optimise_scale,real_width)
     call show_(stdout,"Using extinction            = ", self%optimise_extinction,real_width)
     call show_(stdout,"Thermal smearing model      = ", self%thermal_smearing_model)
     call show_(stdout,"Fragment partition model    = ", self%partition_model)
     call show_(stdout,"Correct dispersion?         = ", self%correct_dispersion,real_width=.true.)
     if (self%optimise_extinction) then;
     call show_(stdout,"Optimize extinction         = ", .true.,real_width)
     call show_(stdout,"Secondary extinction factor = ", self%extinction_factor)
     else
     call show_(stdout,"Optimize extinction         = ", .false.,real_width)
     end if
     if (self%optimise_scale) then
     call show_(stdout,"Optimize scale factor       = ", .true.,real_width)
     call show_(stdout,"Scale factor                = ", self%scale_factor)
     else
     call show_(stdout,"Optimize scale factor       = ", .false.,real_width)
     end if

   end subroutine

   subroutine put_reflection_data(self)
    type(crystal_type) :: self
    ! Output the reflection data t

     call ensure_(tonto,associated(self%reflections),"CRYSTAL:put_reflection_data ... no reflections")
     if (have_F_calc_(self) .or. have_F_pred_(self)) call put_correction_data_(self)
     call put_(self%reflections)

   end subroutine

   subroutine put_reflection_phases(self)
    type(crystal_type) :: self
    ! Output the reflection phases

     call ensure_(tonto,associated(self%reflections),"CRYSTAL:put_reflection_phases ... no reflection data")
     call set_keys_(self%reflections,(/"indices","F_calc ","F_phase"/))
     call put_(self%reflections)
     call clear_keys_(self%reflections)

   end subroutine

   subroutine put_qq_plot(self,name)
    type(crystal_type) :: self
    ! Output a qq plot to the text file.
    ! It is a plot of the experimental quantile vs expected quantile.
     character(128), optional :: name

     call ensure_(tonto,associated(self%reflections),"CRYSTAL:put_qq_plot ... no reflection data")
     call put_F_qq_plot_(self%reflections,name)

   end subroutine

   subroutine put_labelled_qq_plot(self,name)
    type(crystal_type) :: self
    ! Output a qq plot to the text file.
    ! It is a plot of the experimental quantile vs expected quantile.
     character(128), optional :: name

   call ensure_(tonto,associated(self%reflections),"CRYSTAL:put_labelled_qq_plot ... no reflection data")
     call put_labelled_F_qq_plot_(self%reflections,name)

   end subroutine

   subroutine put_chi2_vs_angle_plot(self)
    type(crystal_type) :: self
    ! Output a table with the chi^2 for the structure factor data set broken
    ! into sections.
    ! Reads from stdin the number of divisions in the plot.
     integer(kind=kind(1)) :: num_sections
     integer(kind=kind(1)) :: n,num,k,n_refl
     integer(kind=kind(1)), dimension(:), pointer :: section_for
     real(kind=kind(1.0d0)) :: stl,stl_min,stl_max,chi2,width,stl_mean

     call read_(stdin,num_sections)
   call ensure_(tonto,num_sections > 0,"CRYSTAL:put_chi2_vs_angle_plot ... number of sections to plot not positive")
   call ensure_(tonto,associated(self%reflections),"CRYSTAL:put_chi2_vs_angle_plot ... no structure factors")
     n_refl = n_refl_(self%reflections)

      ! Work out the limits of the sin(theta)/lambda.
     stl_min = 1000
     stl_max = 0.0d0
     do n=1, n_refl
       stl = stl_(self,n) * (1/0.52917724924d0)
       if (stl < stl_min) stl_min = stl
       if (stl > stl_max) stl_max = stl
     end do
     width = (10.0d0**(-3) + stl_max - stl_min)/num_sections

     call create_(section_for,n_refl)
      ! Determine which section each reflection belongs to.
     do n=1, n_refl
       stl = stl_(self,n) * (1/0.52917724924d0)
       section_for(n) = ceiling((10.0d0**(-3) + stl - stl_min) / width)
     end do

     call flush_(stdout)
     call text_(stdout,"Chi^2 vs angle plot")
     call flush_(stdout)
     call text_(stdout,"sin(theta)/lambda in Angstrom^(-1)")
     call show_(stdout,"Smallest sin(theta)/lambda  = ",stl_min)
     call show_(stdout,"Largest sin(theta)/lambda   = ",stl_max)
     call flush_(stdout)
     call put_(stdout,"stl")
     call put_(stdout,"chi^2",flush=1)
     call dash_(stdout,real_fields=2)
     do n = 1, num_sections
       chi2 = 0.0d0
       num = 0
       stl_mean = stl_min + (n - 0.50d0) * width
       do k = 1, n_refl
         if (n==section_for(k)) then
           chi2 = chi2 + F_z2_(self%reflections(k))
           num = num + 1
         end if
       end do
       call put_(stdout,stl_mean)
       if (num==0) then
         call flush_(stdout)
         cycle
       else
         chi2 = chi2 / max(num - self%n_param,1)
         call put_(stdout,chi2,flush=1)
       end if
     end do
     call flush_(stdout)
     call destroy_(section_for)

   end subroutine

   subroutine put_fcalc_plots(self)
    type(crystal_type) :: self
    ! Output some different plots about the calculated structure factors.
     real(kind=kind(1.0d0)), dimension(:), pointer :: ext
     type(reflection_type), dimension(:), pointer :: ext_refs
     integer(kind=kind(1)) :: n,n_refl
     real(kind=kind(1.0d0)) :: y,w

     n_refl = n_refl_(self%reflections)

     call text_(stdout,"The effects of angle.")
     call text_(stdout,"Scatter plot of (Fexp-Fpred)/F_sigma vs sin(theta)/lambda")
     do n=1,n_refl
       call put_(stdout,stl_(self,n))
       call put_(stdout,F_z_(self%reflections(n)))
       call flush_(stdout)
     end do
     call flush_(stdout)

     call text_(stdout,"The effects of intensity.")
     call text_(stdout,"Scatter plot of (Fexp-Fpred)/F_sigma vs Fexp")
     do n=1,n_refl
       call put_(stdout,self%reflections(n)%F_exp)
       call put_(stdout,F_z_(self%reflections(n)))
       call flush_(stdout)
     end do
     call flush_(stdout)

     call create_(ext_refs,n_refl)
     call create_(ext,n_refl)
     ext = extinction_correction_(self)
     call set_F_exp_(ext_refs,self%reflections%F_pred)
     call set_F_pred_(ext_refs,self%reflections%F_pred * ext)
     call set_F_sigma_(ext_refs,self%reflections%F_sigma)
     call destroy_(ext)

     call text_(stdout,"The effects of extinction.")
     w = 0.0d0
     y = 0.0d0
     do n=1,n_refl
       y = y + abs(F_z_(ext_refs(n)))
       w = w + abs(F_r_(ext_refs(n)))
     end do
     y = y / n_refl
     w = w / n_refl
     call show_(stdout,"Average value of abs(Fcalc_ext-Fcalc)/F_sigma is ",y)
     call show_(stdout,"Average value of abs(extinction correction) is ",w)
     call flush_(stdout)

     call text_(stdout,"The effects of intensity on extinction.")
     call text_(stdout,"Scatter plot of (Fcalc_ext-Fcalc)/F_sigma vs Fpred")
     do n=1,n_refl
       call put_(stdout,ext_refs(n)%F_pred)
       call put_(stdout,F_z_(ext_refs(n)))
       call flush_(stdout)
     end do
     call flush_(stdout)

     call text_(stdout,"The effects of scattering angle on extinction.")
     call text_(stdout,"Scatter plot of (Fcalc_ext-Fcalc)/F_sigma vs sin(theta)/lambda")
     do n=1,n_refl
       call put_(stdout,stl_(self,n))
       call put_(stdout,F_z_(ext_refs(n)))
       call flush_(stdout)
     end do
     call destroy_(ext_refs)

     call flush_(stdout)

   end subroutine

   subroutine make_crystal_error_map(self,map,pts)
    type(crystal_type) :: self
    ! Make the crystal error "map" for the supplied points "pts" from the crystal
    ! structure factors
     real(kind=kind(1.0d0)), dimension(:) :: map
     real(kind=kind(1.0d0)), dimension(:,:) :: pts
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: k
     real(kind=kind(1.0d0)), dimension(:), pointer :: F,phase
     integer(kind=kind(1)) :: n_pts,n_refl,n
     real(kind=kind(1.0d0)) :: fac

   call ensure_(tonto,associated(self%reflections),"CRYSTAL:make_crystal_error_map ... no structure factors")
   call ensure_(tonto,have_F_calc_(self),"CRYSTAL:make_crystal_error_map ... no calculated structure factors")
   call ensure_(tonto,have_F_exp_(self),"CRYSTAL:make_crystal_error_map ... no experimental structure factors")
   call ensure_(tonto,size(pts,2)==3,"CRYSTAL:make_crystal_error_map ... incorrect dimension for points array")
     n_pts = size(pts,1)
     n_refl = n_refl_(self%reflections)
     call create_(k,n_refl,3); call make_k_pts_(self,k)
     call create_(F,n_refl)
     F = (self%reflections%F_exp - self%reflections%F_pred) * sign(1.0d0,real(self%reflections%F_calc))
     call create_(phase,n_refl)
     do n = 1,n_pts
        call to_product_of_(phase,k,pts(n:n,:),transpose_b=.true.)
        map(n) = sum(F*cos(phase))
     end do
     fac = 2.0d0/self%unitcell%volume
     map = fac*map
     call destroy_(phase)
     call destroy_(F)
     call destroy_(k)

   end subroutine

   subroutine put_PND_sf(self,name)
    type(crystal_type) :: self
    ! Output the magnetic structure factors
       character(128) :: name
       complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: FM_s,FM_l
       type(archive_type) :: arch
        integer(kind=kind(1)) :: n

       call create_(FM_s,n_refl_(self%reflections))
       call set_(arch,name,"PND_spin_sf")
       call read_(arch,FM_s)
       call create_(FM_l,n_refl_(self%reflections))
       call set_(arch,name,"PND_nabla_sf")
       call read_(arch,FM_l)
       call text_(stdout,"PND magnetic structure factors:")
       call flush_(stdout)
       call dash_(stdout,int_fields=3,real_fields=3)
       call put_(stdout,"h", int_width=.true.)
       call put_(stdout,"k", int_width=.true.)
       call put_(stdout,"l", int_width=.true.)
       call put_(stdout,"FM_s")
       call put_(stdout,"FM_l")
       call put_(stdout,"FM")
       call flush_(stdout)
       call dash_(stdout,int_fields=3,real_fields=3)
       do n = 1, n_refl_(self%reflections)
          call put_(stdout,self%reflections(n)%h)
          call put_(stdout,self%reflections(n)%k)
          call put_(stdout,self%reflections(n)%l)
          call put_(stdout,real(FM_s(n)) )
          call put_(stdout,real(FM_l(n)) )
          call put_(stdout,real(FM_s(n)+FM_l(n)) )
          call flush_(stdout)
       end do
       call dash_(stdout,int_fields=3,real_fields=3)
       call destroy_(FM_l)
       call destroy_(FM_s)

   end subroutine

   subroutine put_asymmetric_unit_geometry(self,atom)
    type(crystal_type) :: self
    ! Put out the asymmetric unit geometry.
      type(atom_type), dimension(:), optional :: atom
      integer(kind=kind(1)) :: n
      character(128) :: symbol

      call ensure_(tonto,associated(self%asymmetric_unit_geometry),"CRYSTAL:put_asymmetric_unit_geometry ... no asymmetric un&
&it atoms")
      call flush_(stdout)
      call text_(stdout,"Crystal asymmetric unit cell geometry:")
      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=3)
      call put_(stdout,"Atom",int_width=.true.)
      call put_(stdout,"ID",int_width=.true.)
      call put_(stdout,"x")
      call put_(stdout,"y")
      call put_(stdout,"z")
      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=3)
      do n = 1,self%n_asymmetric_unit_atoms
         call put_(stdout,n)
         if (present(atom)) then
            symbol = trim(chemical_symbol_(atom(n))) // " (" // trim(to_str_(n)) // ")"
            call put_(stdout,symbol,int_width=.true.)
         else
            call put_(stdout,n)
         end if
         call put_(stdout,self%asymmetric_unit_geometry(1,n))
         call put_(stdout,self%asymmetric_unit_geometry(2,n))
         call put_(stdout,self%asymmetric_unit_geometry(3,n))
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=2,real_fields=3)

   end subroutine

   subroutine put_unit_cell_geometry(self,atom)
    type(crystal_type) :: self
    ! Put out the unit_cell_geometry.
      type(atom_type), dimension(:), optional :: atom
      integer(kind=kind(1)) :: n,u
      character(128) :: symbol

      call ensure_(tonto,associated(self%unit_cell_geometry),"CRYSTAL:put_unit_cell_geometry ... no unit_cell_geometry")
      if (present(atom)) then
      call ensure_(tonto,size(atom)>=self%n_asymmetric_unit_atoms,"CRYSTAL:put_unit_cell_geometry ... atom array too small")
      end if
      call flush_(stdout)
      call text_(stdout,"Crystal unit cell geometry:")
      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=3)
      call put_(stdout,"Atom",int_width=.true.)
      call put_(stdout,"ID",int_width=.true.)
      call put_(stdout,"x")
      call put_(stdout,"y")
      call put_(stdout,"z")
      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=3)
      do n = 1,self%n_unit_cell_atoms
         call put_(stdout,n)
         u = self%atom_for_unit_cell_atom(n)
         if (present(atom)) then
            symbol = trim(chemical_symbol_(atom(u))) // " (" // trim(to_str_(n)) // ")"
            call put_(stdout,symbol,int_width=.true.)
         else
            call put_(stdout,n)
         end if
         call put_(stdout,self%unit_cell_geometry(1,n))
         call put_(stdout,self%unit_cell_geometry(2,n))
         call put_(stdout,self%unit_cell_geometry(3,n))
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=2,real_fields=3)

   end subroutine

   subroutine put_CX(self,label)
    type(crystal_type) :: self
    ! Output some information for the Crystal Explorer program.
       character(128) :: label

       call put_CX_(self%unitcell,label)

   end subroutine

!  *******************
!  Tests for existence
!  *******************

   function asymmetric_unit_exists(self) result(res)
    type(crystal_type) :: self
    ! Return .true. if the asymmetric unit geometry information exists
     logical(kind=kind(.true.)) :: res

     res = associated(self%asymmetric_unit_geometry)

   end function

   function unit_cell_geometry_exists(self) result(res)
    type(crystal_type) :: self
    ! Return .true. if the unit_cell_geometry information exists
     logical(kind=kind(.true.)) :: res

     res = associated(self%unit_cell_geometry)

   end function

   function fragment_data_exists(self) result(res)
    type(crystal_type) :: self
    ! Return .true. if a fragment information exists
     logical(kind=kind(.true.)) :: res

     res = associated(self%fragment_geometry)

   end function

   function reflection_data_exists(self) result(res)
    type(crystal_type) :: self
    ! Return .true. if reflection data exists
     logical(kind=kind(.true.)) :: res

     res = associated(self%reflections)

   end function

   function have_F_exp(self) result(res)
    type(crystal_type) :: self
    ! Return .true. if have some experimental structure factors
      logical(kind=kind(.true.)) :: res

      res = .false.
      if (associated(self%reflections)) res = have_F_exp_(self%reflections)

   end function

   function have_F_calc(self) result(res)
    type(crystal_type) :: self
    ! Return .true. if have some calculated structure factors
      logical(kind=kind(.true.)) :: res

      res = .false.
      if (associated(self%reflections)) res = have_F_calc_(self%reflections)

   end function

   function have_F_pred(self) result(res)
    type(crystal_type) :: self
    ! Return .true. if have some predicted structure factors
      logical(kind=kind(.true.)) :: res

      res = .false.
      if (associated(self%reflections)) res = have_F_pred_(self%reflections)

   end function

   function have_F_sigma(self) result(res)
    type(crystal_type) :: self
    ! Return .true. if have some structure factor errors
      logical(kind=kind(.true.)) :: res

      res = .false.
      if (associated(self%reflections)) res = have_F_sigma_(self%reflections)

   end function

!*******************************************************************************
!                     Inherited reflection routines
!*******************************************************************************

   function n_refl(self) result(res)
    type(crystal_type) :: self
    ! The number of reflections
     integer(kind=kind(1)) :: res

     call ensure_(tonto,associated(self%reflections),"CRYSTAL:n_refl ... no reflection data")
     res = n_refl_(self%reflections)

   end function

   function F_calc(self) result(res)
    type(crystal_type) :: self
    ! The calculated structure factors
     complex(kind=kind((1.0d0,1.0d0))), dimension(size(self%reflections)) :: res

     call ensure_(tonto,reflection_data_exists_(self),"CRYSTAL:F_calc ... no reflection data")
     res = self%reflections%F_calc

   end function

   function F_pred(self) result(res)
    type(crystal_type) :: self
    ! The predicted structure factors
     real(kind=kind(1.0d0)), dimension(size(self%reflections)) :: res

     call ensure_(tonto,reflection_data_exists_(self),"CRYSTAL:F_pred ... no reflection data")
     res = self%reflections%F_pred

   end function

   function F_sigma(self) result(res)
    type(crystal_type) :: self
    ! The structure factor errors
     real(kind=kind(1.0d0)), dimension(size(self%reflections)) :: res

     call ensure_(tonto,reflection_data_exists_(self),"CRYSTAL:F_sigma ... no reflection data")
     res = self%reflections%F_sigma

   end function

   function F_exp(self) result(res)
    type(crystal_type) :: self
    ! The experimental structure factors
     real(kind=kind(1.0d0)), dimension(size(self%reflections)) :: res

     call ensure_(tonto,reflection_data_exists_(self),"CRYSTAL:F_exp ... no reflection data")
     res = self%reflections%F_exp

   end function

   subroutine set_F_calc(self,F_calc)
    type(crystal_type) :: self
    ! Set the calculated structure factors
     complex(kind=kind((1.0d0,1.0d0))), dimension(:), intent(in) :: F_calc

     call ensure_(tonto,reflection_data_exists_(self),"CRYSTAL:set_F_calc ... no reflection data")
     call set_F_calc_(self%reflections,F_calc)

   end subroutine

   function F_chi2(self) result(res)
    type(crystal_type) :: self
    ! Returns data
     intent(in) :: self
     real(kind=kind(1.0d0)) :: res

     call ensure_(tonto,reflection_data_exists_(self),"CRYSTAL:F_chi2 ... no reflection data")
     res = F_chi2_(self%reflections)

   end function

   function F_goodness_of_fit(self) result(res)
    type(crystal_type) :: self
    ! Returns data
     intent(in) :: self
     real(kind=kind(1.0d0)) :: res

     call ensure_(tonto,reflection_data_exists_(self),"CRYSTAL:F_goodness_of_fit ... no reflection data")
     res = F_goodness_of_fit_(self%reflections)

   end function

   function F_r_factor(self) result(res)
    type(crystal_type) :: self
    ! Returns data
     intent(in) :: self
     real(kind=kind(1.0d0)) :: res

     call ensure_(tonto,reflection_data_exists_(self),"CRYSTAL:F_r_factor ... no reflection data")
     res = F_r_factor_(self%reflections)

   end function

   function F_weighted_r_factor(self) result(res)
    type(crystal_type) :: self
    ! Returns data
     intent(in) :: self
     real(kind=kind(1.0d0)) :: res

     call ensure_(tonto,reflection_data_exists_(self),"CRYSTAL:F_weighted_r_factor ... no reflection data")
     res = F_weighted_r_factor_(self%reflections)

   end function

   function I_chi2(self) result(res)
    type(crystal_type) :: self
    ! Returns data
     intent(in) :: self
     real(kind=kind(1.0d0)) :: res

     call ensure_(tonto,reflection_data_exists_(self),"CRYSTAL:I_chi2 ... no reflection data")
     res = I_chi2_(self%reflections)

   end function

   function I_goodness_of_fit(self) result(res)
    type(crystal_type) :: self
    ! Returns data
     intent(in) :: self
     real(kind=kind(1.0d0)) :: res

     call ensure_(tonto,reflection_data_exists_(self),"CRYSTAL:I_goodness_of_fit ... no reflection data")
     res = I_goodness_of_fit_(self%reflections)

   end function

   function I_r_factor(self) result(res)
    type(crystal_type) :: self
    ! Returns data
     intent(in) :: self
     real(kind=kind(1.0d0)) :: res

     call ensure_(tonto,reflection_data_exists_(self),"CRYSTAL:I_r_factor ... no reflection data")
     res = I_r_factor_(self%reflections)

   end function

   function I_weighted_r_factor(self) result(res)
    type(crystal_type) :: self
    ! Returns data
     intent(in) :: self
     real(kind=kind(1.0d0)) :: res

     call ensure_(tonto,reflection_data_exists_(self),"CRYSTAL:I_weighted_r_factor ... no reflection data")
     res = I_weighted_r_factor_(self%reflections)

   end function

   subroutine simulate_new_F_exp(self)
    type(crystal_type) :: self
    ! Simulate a new experiment by adding normally distributed experimental
    ! errors the the F_exp.
     real(kind=kind(1.0d0)), dimension(:), pointer :: F_exp
     integer(kind=kind(1)) :: n
     real(kind=kind(1.0d0)) :: chi2,z

     call ensure_(tonto,reflection_data_exists_(self),"CRYSTAL:simulate_new_F_exp ... no reflection data")
     call ensure_(tonto,have_F_exp_(self),"CRYSTAL:simulate_new_F_exp ... no experimental structure factors")
     call ensure_(tonto,have_F_sigma_(self),"CRYSTAL:simulate_new_F_exp ... no experimental structure factor errors")
     call flush_(stdout)
     call text_(stdout,"Adding simulated errors to F_exp using F_sigma.")
     call create_(F_exp,n_refl_(self))
     F_exp = self%reflections%F_exp
     call simulate_new_F_exp_(self%reflections)
     chi2=0.0d0
     do n=1,n_refl_(self)
       z=(F_exp(n)-self%reflections(n)%F_exp)/self%reflections(n)%F_sigma
       chi2 = chi2 + z*z
     end do
     chi2 = chi2 / max(n_refl_(self) - 1,1)
     call text_(stdout,"chi^2 of old F_exp to new F_exp is " // trim(to_str_(chi2)))
     call flush_(stdout)
     call dash_(stdout,real_fields=4)
     call put_(stdout,"F_exp (old)")
     call put_(stdout,"F_exp (new)")
     call put_(stdout,"F_sigma")
     call put_(stdout,"dF/sigma")
     call flush_(stdout)
     call dash_(stdout,real_fields=4)
     do n=1,n_refl_(self)
       z=(F_exp(n)-self%reflections(n)%F_exp)/self%reflections(n)%F_sigma
       call put_(stdout,F_exp(n))
       call put_(stdout,self%reflections(n)%F_exp)
       call put_(stdout,self%reflections(n)%F_sigma)
       call put_(stdout,z)
       call flush_(stdout)
     end do
     call dash_(stdout,real_fields=4)
     call destroy_(F_exp)

   end subroutine

   subroutine make_asymmetric_geometry(self)
    type(crystal_type) :: self
    ! Set the asymmetric geometry array from the ".unique_fragment_atom" info.
    ! This should not be done if an ".asymmetric_unit_geometry" already exists!

     call ensure_(tonto,.not. associated(self%asymmetric_unit_geometry),"CRYSTAL:make_asymmetric_geometry ... asymmetric unit&
& already exists")
     call ensure_(tonto,associated(self%unique_fragment_atom),"CRYSTAL:make_asymmetric_geometry ... unique_fragment_atom list&
& does not exist")
     call create_(self%asymmetric_unit_geometry,3,self%n_unique_fragment_atoms)
     self%asymmetric_unit_geometry = self%fragment_geometry(:,self%unique_fragment_atom)
     self%n_asymmetric_unit_atoms = self%n_unique_fragment_atoms

   end subroutine

end
