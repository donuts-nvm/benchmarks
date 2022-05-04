!-------------------------------------------------------------------------------
!
! MOL: An object representation of a molecule.
!
! Copyright (C) Dylan Jayatilaka, Daniel Grimwood, 1996
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
! $Id: mol.foo,v 1.161.2.60 2004/04/21 09:12:54 reaper Exp $
!-------------------------------------------------------------------------------

module MOL_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

   use FILE_MODULE, only: read_
   use FILE_MODULE, only: write_

   use SCFDATA_MODULE, only: read_keywords_
   use SCFDATA_MODULE, only: eri_cutoff_
   use SCFDATA_MODULE, only: do_delta_build_
   use SCFDATA_MODULE, only: apply_damping_
   use SCFDATA_MODULE, only: apply_fock_diis_
   use SCFDATA_MODULE, only: orbital_energies_kind_
   use SCFDATA_MODULE, only: cleanup_diis_
   use SCFDATA_MODULE, only: reset_
   use SCFDATA_MODULE, only: set_defaults_
   use SCFDATA_MODULE, only: set_diis_error_
   use SCFDATA_MODULE, only: destroy_
   use SCFDATA_MODULE, only: create_
   use SCFDATA_MODULE, only: using_diis_
   use SCFDATA_MODULE, only: put_banner_
   use SCFDATA_MODULE, only: molecular_orbital_kind_
   use SCFDATA_MODULE, only: exceeded_lambda_max_
   use SCFDATA_MODULE, only: update_lambda_
   use SCFDATA_MODULE, only: apply_level_shifting_
   use SCFDATA_MODULE, only: fitting_
   use SCFDATA_MODULE, only: spinorbital_kind_
   use SCFDATA_MODULE, only: create_copy_
   use SCFDATA_MODULE, only: put_results_
   use SCFDATA_MODULE, only: update_
   use SCFDATA_MODULE, only: scf_done_

   use SHELL2_MODULE, only: make_dipole_ints_
   use SHELL2_MODULE, only: make_quadrupole_ints_
   use SHELL2_MODULE, only: copy_
   use SHELL2_MODULE, only: make_irrotational_jp_ints_
   use SHELL2_MODULE, only: make_ft_r_
   use SHELL2_MODULE, only: make_ft_
   use SHELL2_MODULE, only: make_spin_orbit_ints_
   use SHELL2_MODULE, only: make_e_gradient_ints_
   use SHELL2_MODULE, only: make_tanaka_thermal_smearing_
   use SHELL2_MODULE, only: get_nuc_
   use SHELL2_MODULE, only: destroy_
   use SHELL2_MODULE, only: create_
   use SHELL2_MODULE, only: make_overlap_
   use SHELL2_MODULE, only: make_spin_orbit_B_ints_
   use SHELL2_MODULE, only: make_nuclear_attraction_ints_
   use SHELL2_MODULE, only: get_kei_
   use SHELL2_MODULE, only: make_L_ints_
   use SHELL2_MODULE, only: make_solenoidal_jp_ints_
   use SHELL2_MODULE, only: make_ft_nabla_
   use SHELL2_MODULE, only: destroy_ptr_part_
   use SHELL2_MODULE, only: make_E_field_ints_
   use SHELL2_MODULE, only: make_octupole_ints_
   use SHELL2_MODULE, only: skip_ft_
   use SHELL2_MODULE, only: make_gaussian_partition_

   use CPXMAT5_MODULE, only: create_
   use CPXMAT5_MODULE, only: destroy_

   use REALMAT4_MODULE, only: create_
   use REALMAT4_MODULE, only: destroy_

   use BASIS_MODULE, only: copy_
   use BASIS_MODULE, only: create_
   use BASIS_MODULE, only: no_of_primitives_
   use BASIS_MODULE, only: contraction_matrix_
   use BASIS_MODULE, only: destroy_
   use BASIS_MODULE, only: same_as_
   use BASIS_MODULE, only: no_of_basis_functions_

   use SHELL1_MODULE, only: create_
   use SHELL1_MODULE, only: make_laplacian_grid_
   use SHELL1_MODULE, only: make_grid_
   use SHELL1_MODULE, only: copy_
   use SHELL1_MODULE, only: make_nabla_grid_
   use SHELL1_MODULE, only: destroy_ptr_part_
   use SHELL1_MODULE, only: nullify_ptr_part_
   use SHELL1_MODULE, only: destroy_
   use SHELL1_MODULE, only: set_

   use REALMAT3_MODULE, only: create_
   use REALMAT3_MODULE, only: destroy_

   use OPVECTOR_MODULE, only: create_
   use OPVECTOR_MODULE, only: convert_to_
   use OPVECTOR_MODULE, only: no_of_occupied_
   use OPVECTOR_MODULE, only: created_
   use OPVECTOR_MODULE, only: spinorbital_kind_
   use OPVECTOR_MODULE, only: create_copy_
   use OPVECTOR_MODULE, only: zero_
   use OPVECTOR_MODULE, only: destroy_

   use SHELLPAIR_MODULE, only: copy_a_
   use SHELLPAIR_MODULE, only: copy_b_
   use SHELLPAIR_MODULE, only: make_precalculated_data_

   use SHELLVEC_MODULE, only: create_
   use SHELLVEC_MODULE, only: nullify_ptr_part_

   use DIIS_MODULE, only: save_pair_
   use DIIS_MODULE, only: get_item_
   use DIIS_MODULE, only: dimension_
   use DIIS_MODULE, only: remake_diis_matrix_
   use DIIS_MODULE, only: get_parameter_item_
   use DIIS_MODULE, only: cleanup_
   use DIIS_MODULE, only: extrapolate_
   use DIIS_MODULE, only: save_item_
   use DIIS_MODULE, only: set_archive_root_name_

   use SLATERBASISVEC_MODULE, only: create_copy_
   use SLATERBASISVEC_MODULE, only: read_library_data_
   use SLATERBASISVEC_MODULE, only: put_
   use SLATERBASISVEC_MODULE, only: read_list_keywords_
   use SLATERBASISVEC_MODULE, only: destroy_

   use REALMAT_MODULE, only: symmetrize_
   use REALMAT_MODULE, only: back_transform_
   use REALMAT_MODULE, only: schmidt_orthonormalise_
   use REALMAT_MODULE, only: trace_
   use REALMAT_MODULE, only: plus_scaled_
   use REALMAT_MODULE, only: minus_scaled_
   use REALMAT_MODULE, only: symmetric_reflect_
   use REALMAT_MODULE, only: to_product_of_
   use REALMAT_MODULE, only: uncompress_from_triangle_
   use REALMAT_MODULE, only: solve_eigenproblem_
   use REALMAT_MODULE, only: is_square_
   use REALMAT_MODULE, only: get_diagonal_
   use REALMAT_MODULE, only: trace_of_product_
   use REALMAT_MODULE, only: plus_product_of_
   use REALMAT_MODULE, only: rotate_
   use REALMAT_MODULE, only: change_basis_
   use REALMAT_MODULE, only: to_inverse_sqrt_
   use REALMAT_MODULE, only: to_sqrt_
   use REALMAT_MODULE, only: beta_alpha_
   use REALMAT_MODULE, only: alpha_alpha_
   use REALMAT_MODULE, only: zero_small_values_
   use REALMAT_MODULE, only: compress_to_triangle_
   use REALMAT_MODULE, only: to_transpose_
   use REALMAT_MODULE, only: alpha_beta_
   use REALMAT_MODULE, only: beta_beta_set_to_
   use REALMAT_MODULE, only: to_unit_matrix_
   use REALMAT_MODULE, only: to_scaled_product_of_
   use REALMAT_MODULE, only: antisymmetric_reflect_
   use REALMAT_MODULE, only: beta_beta_
   use REALMAT_MODULE, only: antisymmetric_fold_
   use REALMAT_MODULE, only: make_enclosing_sphere_
   use REALMAT_MODULE, only: trace_product_with_
   use REALMAT_MODULE, only: tri_size_
   use REALMAT_MODULE, only: destroy_
   use REALMAT_MODULE, only: to_product_with_diagonal_
   use REALMAT_MODULE, only: create_
   use REALMAT_MODULE, only: to_inverse_of_
   use REALMAT_MODULE, only: alpha_alpha_set_to_
   use REALMAT_MODULE, only: symmetrically_orthonormalise_

   use ATOM_MODULE, only: no_of_occupied_NOs_
   use ATOM_MODULE, only: ground_state_multiplicity_
   use ATOM_MODULE, only: dispersion_correction_
   use ATOM_MODULE, only: put_natural_orbitals_
   use ATOM_MODULE, only: make_density_grid_
   use ATOM_MODULE, only: chemical_name_
   use ATOM_MODULE, only: mass_
   use ATOM_MODULE, only: n_prim_
   use ATOM_MODULE, only: n_bf_
   use ATOM_MODULE, only: chemical_symbol_

   use PLOTGRID_MODULE, only: read_keywords_
   use PLOTGRID_MODULE, only: create_
   use PLOTGRID_MODULE, only: set_defaults_
   use PLOTGRID_MODULE, only: create_copy_
   use PLOTGRID_MODULE, only: make_points_
   use PLOTGRID_MODULE, only: put_
   use PLOTGRID_MODULE, only: destroy_

   use INTVECVEC_MODULE, only: create_
   use INTVECVEC_MODULE, only: create_copy_
   use INTVECVEC_MODULE, only: destroy_

   use POINTGROUP_MODULE, only: read_keywords_
   use POINTGROUP_MODULE, only: create_
   use POINTGROUP_MODULE, only: create_copy_
   use POINTGROUP_MODULE, only: put_
   use POINTGROUP_MODULE, only: xyz_matrix_
   use POINTGROUP_MODULE, only: destroy_

   use COPPENSBASISVEC_MODULE, only: no_of_orbitals_
   use COPPENSBASISVEC_MODULE, only: renormalise_
   use COPPENSBASISVEC_MODULE, only: put_
   use COPPENSBASISVEC_MODULE, only: read_list_keywords_
   use COPPENSBASISVEC_MODULE, only: create_copy_
   use COPPENSBASISVEC_MODULE, only: read_library_data_
   use COPPENSBASISVEC_MODULE, only: no_of_primitives_
   use COPPENSBASISVEC_MODULE, only: unnormalise_
   use COPPENSBASISVEC_MODULE, only: maximum_basis_n_value_
   use COPPENSBASISVEC_MODULE, only: destroy_

   use CLUSTER_MODULE, only: read_keywords_
   use CLUSTER_MODULE, only: create_
   use CLUSTER_MODULE, only: create_copy_
   use CLUSTER_MODULE, only: create_atom_list_
   use CLUSTER_MODULE, only: make_fragment_atom_
   use CLUSTER_MODULE, only: put_
   use CLUSTER_MODULE, only: destroy_
   use CLUSTER_MODULE, only: create_from_molecule_

   use CPXMAT3_MODULE, only: create_
   use CPXMAT3_MODULE, only: destroy_

   use SHELL4_MODULE, only: create_
   use SHELL4_MODULE, only: get_ERI_
   use SHELL4_MODULE, only: copy_
   use SHELL4_MODULE, only: copy_cd_
   use SHELL4_MODULE, only: destroy_ptr_part_
   use SHELL4_MODULE, only: skip_ERI_
   use SHELL4_MODULE, only: make_spin_orbit_ints_
   use SHELL4_MODULE, only: make_r_J_
   use SHELL4_MODULE, only: destroy_cd_
   use SHELL4_MODULE, only: copy_ab_
   use SHELL4_MODULE, only: destroy_ab_
   use SHELL4_MODULE, only: destroy_
   use SHELL4_MODULE, only: make_r_JK_

   use ROBY_MODULE, only: read_keywords_
   use ROBY_MODULE, only: create_
   use ROBY_MODULE, only: atom_populations_
   use ROBY_MODULE, only: group_pair_populations_
   use ROBY_MODULE, only: group_bond_analysis_
   use ROBY_MODULE, only: group_populations_
   use ROBY_MODULE, only: group_shared_population_
   use ROBY_MODULE, only: atom_bond_analysis_
   use ROBY_MODULE, only: atom_shared_population_
   use ROBY_MODULE, only: atom_pair_populations_
   use ROBY_MODULE, only: destroy_

   use SHELLPAIRVEC_MODULE, only: create_
   use SHELLPAIRVEC_MODULE, only: destroy_

   use REALMAT5_MODULE, only: create_
   use REALMAT5_MODULE, only: destroy_

   use INTMAT_MODULE, only: create_
   use INTMAT_MODULE, only: destroy_

   use ATOMVEC_MODULE, only: centre_of_mass_
   use ATOMVEC_MODULE, only: get_shell_limits_
   use ATOMVEC_MODULE, only: nuclear_octupole_moment_
   use ATOMVEC_MODULE, only: nuclear_EFG_at_nuclei_
   use ATOMVEC_MODULE, only: resolve_axis_system_
   use ATOMVEC_MODULE, only: make_unique_atom_list_
   use ATOMVEC_MODULE, only: nuclear_quadrupole_moment_
   use ATOMVEC_MODULE, only: make_atom_kind_list_
   use ATOMVEC_MODULE, only: n_e_
   use ATOMVEC_MODULE, only: nullify_ptr_part_
   use ATOMVEC_MODULE, only: put_thermal_tensors_
   use ATOMVEC_MODULE, only: put_coord_info_
   use ATOMVEC_MODULE, only: make_shell_for_atom_limits_
   use ATOMVEC_MODULE, only: molecular_weight_
   use ATOMVEC_MODULE, only: nullify_basis_part_
   use ATOMVEC_MODULE, only: resolve_bases_
   use ATOMVEC_MODULE, only: coppensbases_are_resolved_
   use ATOMVEC_MODULE, only: library_basis_labels_
   use ATOMVEC_MODULE, only: nullify_coppensbasis_part_
   use ATOMVEC_MODULE, only: destroy_ptr_part_
   use ATOMVEC_MODULE, only: translate_
   use ATOMVEC_MODULE, only: slaterbases_are_resolved_
   use ATOMVEC_MODULE, only: nullify_slaterbasis_part_
   use ATOMVEC_MODULE, only: make_slater_interpolators_
   use ATOMVEC_MODULE, only: get_geometry_
   use ATOMVEC_MODULE, only: bounding_cube_width_
   use ATOMVEC_MODULE, only: numbered_chemical_symbols_
   use ATOMVEC_MODULE, only: nuclear_E_field_at_nuclei_
   use ATOMVEC_MODULE, only: reduced_mass_
   use ATOMVEC_MODULE, only: n_prim_
   use ATOMVEC_MODULE, only: get_mean_neutron_numbers_
   use ATOMVEC_MODULE, only: n_bf_
   use ATOMVEC_MODULE, only: atom_index_from_pos_
   use ATOMVEC_MODULE, only: destroy_
   use ATOMVEC_MODULE, only: n_shell_
   use ATOMVEC_MODULE, only: create_
   use ATOMVEC_MODULE, only: bases_are_resolved_
   use ATOMVEC_MODULE, only: make_nuclear_matrix_
   use ATOMVEC_MODULE, only: bonded_
   use ATOMVEC_MODULE, only: nuclear_energy_
   use ATOMVEC_MODULE, only: chemical_formula_
   use ATOMVEC_MODULE, only: nuclear_dipole_moment_
   use ATOMVEC_MODULE, only: put_vrml_
   use ATOMVEC_MODULE, only: read_list_keywords_
   use ATOMVEC_MODULE, only: make_coppens_interpolators_
   use ATOMVEC_MODULE, only: centre_of_atoms_
   use ATOMVEC_MODULE, only: atom_shell_for_shell_
   use ATOMVEC_MODULE, only: groups_defined_
   use ATOMVEC_MODULE, only: atom_for_shell_
   use ATOMVEC_MODULE, only: create_copy_
   use ATOMVEC_MODULE, only: n_shell_for_atom_
   use ATOMVEC_MODULE, only: get_coordinates_
   use ATOMVEC_MODULE, only: make_atom_basis_fn_limits_

   use TEXTFILE_MODULE, only: stdin
   use TEXTFILE_MODULE, only: stdout
   use TEXTFILE_MODULE, only: line_number_
   use TEXTFILE_MODULE, only: put_
   use TEXTFILE_MODULE, only: move_to_line_item_
   use TEXTFILE_MODULE, only: save_
   use TEXTFILE_MODULE, only: move_to_line_
   use TEXTFILE_MODULE, only: read_intvec_ptr_
   use TEXTFILE_MODULE, only: read_
   use TEXTFILE_MODULE, only: read_real_
   use TEXTFILE_MODULE, only: look_for_
   use TEXTFILE_MODULE, only: show_
   use TEXTFILE_MODULE, only: has_string_
   use TEXTFILE_MODULE, only: read_intvec_
   use TEXTFILE_MODULE, only: open_
   use TEXTFILE_MODULE, only: flush_
   use TEXTFILE_MODULE, only: next_line_item_
   use TEXTFILE_MODULE, only: next_str_
   use TEXTFILE_MODULE, only: dash_
   use TEXTFILE_MODULE, only: unsave_
   use TEXTFILE_MODULE, only: set_use_labels_
   use TEXTFILE_MODULE, only: next_item_
   use TEXTFILE_MODULE, only: n_line_items_
   use TEXTFILE_MODULE, only: read_line_
   use TEXTFILE_MODULE, only: set_real_style_
   use TEXTFILE_MODULE, only: close_
   use TEXTFILE_MODULE, only: put_text_
   use TEXTFILE_MODULE, only: text_
   use TEXTFILE_MODULE, only: tab_
   use TEXTFILE_MODULE, only: destroy_
   use TEXTFILE_MODULE, only: create_
   use TEXTFILE_MODULE, only: set_n_fields_
   use TEXTFILE_MODULE, only: set_int_width_
   use TEXTFILE_MODULE, only: buffer_exhausted_

   use CPXMAT_MODULE, only: alpha_alpha_
   use CPXMAT_MODULE, only: alpha_beta_
   use CPXMAT_MODULE, only: alpha_beta_put_to_
   use CPXMAT_MODULE, only: alpha_alpha_put_to_
   use CPXMAT_MODULE, only: beta_beta_set_to_
   use CPXMAT_MODULE, only: to_scaled_product_of_
   use CPXMAT_MODULE, only: beta_alpha_put_to_
   use CPXMAT_MODULE, only: alpha_beta_plus_
   use CPXMAT_MODULE, only: beta_beta_
   use CPXMAT_MODULE, only: destroy_
   use CPXMAT_MODULE, only: beta_alpha_set_to_
   use CPXMAT_MODULE, only: to_product_of_
   use CPXMAT_MODULE, only: create_
   use CPXMAT_MODULE, only: alpha_alpha_plus_
   use CPXMAT_MODULE, only: beta_beta_put_to_
   use CPXMAT_MODULE, only: make_antihermitian_
   use CPXMAT_MODULE, only: make_hermitian_
   use CPXMAT_MODULE, only: beta_alpha_plus_
   use CPXMAT_MODULE, only: solve_eigenproblem_
   use CPXMAT_MODULE, only: alpha_alpha_set_to_
   use CPXMAT_MODULE, only: is_square_
   use CPXMAT_MODULE, only: antihermitian_fold_
   use CPXMAT_MODULE, only: change_basis_
   use CPXMAT_MODULE, only: trace_of_product_
   use CPXMAT_MODULE, only: beta_beta_plus_
   use CPXMAT_MODULE, only: beta_alpha_

   use ISOSURFACE_MODULE, only: read_keywords_
   use ISOSURFACE_MODULE, only: create_
   use ISOSURFACE_MODULE, only: plot_function_
   use ISOSURFACE_MODULE, only: cubify_
   use ISOSURFACE_MODULE, only: put_vrml_
   use ISOSURFACE_MODULE, only: destroy_

   use REAL_MODULE, only: to_str_
   use REAL_MODULE, only: harmonic_vibrational_fn_
   use REAL_MODULE, only: to_units_

   use DFTGRID_MODULE, only: read_keywords_
   use DFTGRID_MODULE, only: d_u_lda_exchange_functional_
   use DFTGRID_MODULE, only: d_u_lyp_correlation_functional_
   use DFTGRID_MODULE, only: u_lda_exchange_functional_
   use DFTGRID_MODULE, only: d_r_b88_exchange_functional_
   use DFTGRID_MODULE, only: put_
   use DFTGRID_MODULE, only: u_becke88_exchange_functional_
   use DFTGRID_MODULE, only: make_grid_
   use DFTGRID_MODULE, only: r_lyp_correlation_functional_
   use DFTGRID_MODULE, only: set_defaults_
   use DFTGRID_MODULE, only: r_lda_exchange_functional_
   use DFTGRID_MODULE, only: r_gill96_exchange_functional_
   use DFTGRID_MODULE, only: d_r_lda_exchange_functional_
   use DFTGRID_MODULE, only: destroy_
   use DFTGRID_MODULE, only: create_
   use DFTGRID_MODULE, only: u_lyp_correlation_functional_
   use DFTGRID_MODULE, only: r_becke88_exchange_functional_
   use DFTGRID_MODULE, only: u_gill96_exchange_functional_
   use DFTGRID_MODULE, only: d_r_lyp_correlation_functional_
   use DFTGRID_MODULE, only: make_matrix_elements_of_
   use DFTGRID_MODULE, only: create_copy_
   use DFTGRID_MODULE, only: d_r_gill96_exchange_functional_
   use DFTGRID_MODULE, only: d_u_b88_exchange_functional_
   use DFTGRID_MODULE, only: make_SO_matrix_elements_of_
   use DFTGRID_MODULE, only: d_u_gill96_exchange_functional_

   use CIF_MODULE, only: create_copy_
   use CIF_MODULE, only: destroy_

   use CPXMAT4_MODULE, only: create_
   use CPXMAT4_MODULE, only: destroy_

   use STR_MODULE, only: to_lower_case_
   use STR_MODULE, only: from_real_
   use STR_MODULE, only: includes_
   use STR_MODULE, only: conversion_factor_
   use STR_MODULE, only: same_as_

   use REFLECTIONVEC_MODULE, only: F_chi2_

   use PARALLEL_MODULE, only: sum_vectors_
   use PARALLEL_MODULE, only: sum_symmetric_matrices_
   use PARALLEL_MODULE, only: n_proc_
   use PARALLEL_MODULE, only: this_proc_

   use CRYSTAL_MODULE, only: read_keywords_
   use CRYSTAL_MODULE, only: put_correction_data_
   use CRYSTAL_MODULE, only: put_
   use CRYSTAL_MODULE, only: put_reflection_data_
   use CRYSTAL_MODULE, only: make_crystal_error_map_
   use CRYSTAL_MODULE, only: put_qq_plot_
   use CRYSTAL_MODULE, only: put_PND_sf_
   use CRYSTAL_MODULE, only: reflection_data_exists_
   use CRYSTAL_MODULE, only: F_sigma_
   use CRYSTAL_MODULE, only: sum_unique_sf_deriv_U_
   use CRYSTAL_MODULE, only: sum_ft_r_ints_
   use CRYSTAL_MODULE, only: make_unique_SF_k_pts_
   use CRYSTAL_MODULE, only: destroy_
   use CRYSTAL_MODULE, only: create_
   use CRYSTAL_MODULE, only: make_k_pts_
   use CRYSTAL_MODULE, only: asymmetric_unit_exists_
   use CRYSTAL_MODULE, only: make_F_predicted_
   use CRYSTAL_MODULE, only: F_calc_
   use CRYSTAL_MODULE, only: n_unique_SF_k_pts_
   use CRYSTAL_MODULE, only: sum_ft_ints_
   use CRYSTAL_MODULE, only: sum_PND_nabla_ints_
   use CRYSTAL_MODULE, only: d_chi2_dU_
   use CRYSTAL_MODULE, only: extinction_correction_
   use CRYSTAL_MODULE, only: make_asymmetric_geometry_
   use CRYSTAL_MODULE, only: make_reduced_group_data_
   use CRYSTAL_MODULE, only: create_copy_
   use CRYSTAL_MODULE, only: sum_PND_spin_ints_
   use CRYSTAL_MODULE, only: n_refl_
   use CRYSTAL_MODULE, only: F_exp_
   use CRYSTAL_MODULE, only: sum_unique_sf_
   use CRYSTAL_MODULE, only: update_
   use CRYSTAL_MODULE, only: set_F_calc_

   use TIME_MODULE, only: std_time
   use TIME_MODULE, only: time_taken_
   use TIME_MODULE, only: current_time_

   use INTVEC_MODULE, only: create_
   use INTVEC_MODULE, only: create_copy_
   use INTVEC_MODULE, only: no_of_unique_elements_
   use INTVEC_MODULE, only: destroy_
   use INTVEC_MODULE, only: join_

   use INT_MODULE, only: is_even_
   use INT_MODULE, only: n_comp_
   use INT_MODULE, only: to_str_
   use INT_MODULE, only: triangle_
   use INT_MODULE, only: n_comp_sum_

   use SHELL1QUARTET_MODULE, only: set_cd_
   use SHELL1QUARTET_MODULE, only: destroy_cd_
   use SHELL1QUARTET_MODULE, only: destroy_ab_
   use SHELL1QUARTET_MODULE, only: set_ab_
   use SHELL1QUARTET_MODULE, only: make_r_JK_

   use ARCHIVE_MODULE, only: create_
   use ARCHIVE_MODULE, only: set_root_name_
   use ARCHIVE_MODULE, only: write_gnuplot_
   use ARCHIVE_MODULE, only: delete_all_genres_
   use ARCHIVE_MODULE, only: open_
   use ARCHIVE_MODULE, only: close_
   use ARCHIVE_MODULE, only: read_
   use ARCHIVE_MODULE, only: delete_
   use ARCHIVE_MODULE, only: destroy_
   use ARCHIVE_MODULE, only: exists_
   use ARCHIVE_MODULE, only: set_
   use ARCHIVE_MODULE, only: write_

   use CPXVEC_MODULE, only: create_
   use CPXVEC_MODULE, only: destroy_

   use OPMATRIX_MODULE, only: convert_to_
   use OPMATRIX_MODULE, only: guess_scf_kind_
   use OPMATRIX_MODULE, only: set_to_
   use OPMATRIX_MODULE, only: schmidt_orthonormalise_
   use OPMATRIX_MODULE, only: damp_
   use OPMATRIX_MODULE, only: destroyed_
   use OPMATRIX_MODULE, only: destroy_
   use OPMATRIX_MODULE, only: l_compress_
   use OPMATRIX_MODULE, only: create_
   use OPMATRIX_MODULE, only: any_created_
   use OPMATRIX_MODULE, only: uncompress_
   use OPMATRIX_MODULE, only: minus_
   use OPMATRIX_MODULE, only: symmetrically_orthonormalise_
   use OPMATRIX_MODULE, only: plus_
   use OPMATRIX_MODULE, only: spinorbital_kind_
   use OPMATRIX_MODULE, only: created_
   use OPMATRIX_MODULE, only: create_copy_
   use OPMATRIX_MODULE, only: destroy_ptr_part_
   use OPMATRIX_MODULE, only: compress_
   use OPMATRIX_MODULE, only: number_kind_
   use OPMATRIX_MODULE, only: all_destroyed_

   use STRVEC_MODULE, only: create_
   use STRVEC_MODULE, only: destroy_

   use REALVEC_MODULE, only: distance_to_
   use REALVEC_MODULE, only: create_
   use REALVEC_MODULE, only: index_of_first_zero_value_
   use REALVEC_MODULE, only: zero_small_values_
   use REALVEC_MODULE, only: index_of_first_nonzero_value_
   use REALVEC_MODULE, only: is_zero_
   use REALVEC_MODULE, only: norm_
   use REALVEC_MODULE, only: plus_
   use REALVEC_MODULE, only: destroy_

   use SHELL_MODULE, only: create_
   use SHELL_MODULE, only: copy_
   use SHELL_MODULE, only: destroy_ptr_part_
   use SHELL_MODULE, only: l_chr_
   use SHELL_MODULE, only: destroy_

   use BASISVEC_MODULE, only: create_
   use BASISVEC_MODULE, only: renormalise_
   use BASISVEC_MODULE, only: put_
   use BASISVEC_MODULE, only: read_list_keywords_
   use BASISVEC_MODULE, only: create_copy_
   use BASISVEC_MODULE, only: read_library_data_
   use BASISVEC_MODULE, only: library_directory_
   use BASISVEC_MODULE, only: expand_
   use BASISVEC_MODULE, only: unnormalise_
   use BASISVEC_MODULE, only: destroy_
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

   public    put_roby_smo_energy_partition_
   interface put_roby_smo_energy_partition_
      module procedure put_roby_smo_energy_partition
   end interface

   public    make_fock_matrix_
   interface make_fock_matrix_
      module procedure make_fock_matrix
      module procedure make_fock_matrix_1
   end interface

   public    add_ex_corr_matrix_
   interface add_ex_corr_matrix_
      module procedure add_ex_corr_matrix
      module procedure add_ex_corr_matrix_1
   end interface

   public    put_g_tensor_information_
   interface put_g_tensor_information_
      module procedure put_g_tensor_information
   end interface

   public    create_cluster_
   interface create_cluster_
      module procedure create_cluster
   end interface

   public    resolve_axis_system_
   interface resolve_axis_system_
      module procedure resolve_axis_system
   end interface

   public    put_
   interface put_
      module procedure put
   end interface

   public    make_sz_structure_factors_
   interface make_sz_structure_factors_
      module procedure make_sz_structure_factors
   end interface

   public    make_ELF_grid_r_
   interface make_ELF_grid_r_
      module procedure make_ELF_grid_r
   end interface

   public    MO_eigen_update_
   interface MO_eigen_update_
      module procedure MO_eigen_update
   end interface

   public    make_electron_density_grid_
   interface make_electron_density_grid_
      module procedure make_electron_density_grid
   end interface

   public    make_density_matrix_
   interface make_density_matrix_
      module procedure make_density_matrix
   end interface

   public    make_ft_
   interface make_ft_
      module procedure make_ft
      module procedure make_ft_1
   end interface

   public    make_image_of_shell_
   interface make_image_of_shell_
      module procedure make_image_of_shell
   end interface

   public    get_initial_density_
   interface get_initial_density_
      module procedure get_initial_density
   end interface

   public    set_scf_occupations_
   interface set_scf_occupations_
      module procedure set_scf_occupations
   end interface

   public    ft_thermally_smear_
   interface ft_thermally_smear_
      module procedure ft_thermally_smear
   end interface

   private    put_roby_energy_parts_
   interface put_roby_energy_parts_
      module procedure put_roby_energy_parts
   end interface

   public    make_kinetic_overlap_
   interface make_kinetic_overlap_
      module procedure make_kinetic_overlap
   end interface

   public    add_dispersion_correction_
   interface add_dispersion_correction_
      module procedure add_dispersion_correction
   end interface

   public    d_sfchi2_d_thermal_
   interface d_sfchi2_d_thermal_
      module procedure d_sfchi2_d_thermal
   end interface

   public    make_gc_fock_guess_
   interface make_gc_fock_guess_
      module procedure make_gc_fock_guess
   end interface

   public    make_spin_orbit_integrals_
   interface make_spin_orbit_integrals_
      module procedure make_spin_orbit_integrals
   end interface

   public    integrate_rho_grid_
   interface integrate_rho_grid_
      module procedure integrate_rho_grid
   end interface

   public    make_group_nuclear_matrix_
   interface make_group_nuclear_matrix_
      module procedure make_group_nuclear_matrix
      module procedure make_group_nuclear_matrix_1
   end interface

   public    make_group_overlap_matrix_
   interface make_group_overlap_matrix_
      module procedure make_group_overlap_matrix
   end interface

   public    destroy_ptr_part_
   interface destroy_ptr_part_
      module procedure destroy_ptr_part
   end interface

   public    make_constraint_
   interface make_constraint_
      module procedure make_constraint
   end interface

   public    add_non_local_ex_corr_matrix_
   interface add_non_local_ex_corr_matrix_
      module procedure add_non_local_ex_corr_matrix
      module procedure add_non_local_ex_corr_matrix_1
   end interface

   public    has_valid_no_of_beta_electrons_
   interface has_valid_no_of_beta_electrons_
      module procedure has_valid_no_of_beta_electrons
   end interface

   public    make_ANO_data_for_atom_
   interface make_ANO_data_for_atom_
      module procedure make_ANO_data_for_atom
   end interface

   public    make_rho_grid_at_
   interface make_rho_grid_at_
      module procedure make_rho_grid_at
   end interface

   public    make_kinetic_matrix_
   interface make_kinetic_matrix_
      module procedure make_kinetic_matrix
   end interface

   public    make_ao_sz_density_matrix_
   interface make_ao_sz_density_matrix_
      module procedure make_ao_sz_density_matrix
   end interface

   public    make_octupole_matrices_
   interface make_octupole_matrices_
      module procedure make_octupole_matrices
   end interface

   public    make_ft_nabla_pair_
   interface make_ft_nabla_pair_
      module procedure make_ft_nabla_pair
   end interface

   public    electronic_octupole_moment_
   interface electronic_octupole_moment_
      module procedure electronic_octupole_moment
   end interface

   public    read_CX_surface_
   interface read_CX_surface_
      module procedure read_CX_surface
   end interface

   public    nuclear_E_field_at_nuclei_
   interface nuclear_E_field_at_nuclei_
      module procedure nuclear_E_field_at_nuclei
   end interface

   public    make_integration_grid_
   interface make_integration_grid_
      module procedure make_integration_grid
   end interface

   public    electronic_E_field_at_nuclei_
   interface electronic_E_field_at_nuclei_
      module procedure electronic_E_field_at_nuclei
   end interface

   public    integrate_density_numerically_
   interface integrate_density_numerically_
      module procedure integrate_density_numerically
   end interface

   public    constrained_scf_
   interface constrained_scf_
      module procedure constrained_scf
   end interface

   public    archive_doesnt_exist_
   interface archive_doesnt_exist_
      module procedure archive_doesnt_exist
   end interface

   public    add_u_correlation_matrix_
   interface add_u_correlation_matrix_
      module procedure add_u_correlation_matrix
   end interface

   public    get_1e_ZORA_matrices_
   interface get_1e_ZORA_matrices_
      module procedure get_1e_ZORA_matrices
   end interface

   public    make_r_NOs_
   interface make_r_NOs_
      module procedure make_r_NOs
   end interface

   public    nuclear_energy_
   interface nuclear_energy_
      module procedure nuclear_energy
   end interface

   public    destroy_ANO_data_
   interface destroy_ANO_data_
      module procedure destroy_ANO_data
   end interface

   public    make_Tsirelson_ELF_grid_c_
   interface make_Tsirelson_ELF_grid_c_
      module procedure make_Tsirelson_ELF_grid_c
   end interface

   public    destroy_scf_results_
   interface destroy_scf_results_
      module procedure destroy_scf_results
   end interface

   public    unarchive_occupation_numbers_
   interface unarchive_occupation_numbers_
      module procedure unarchive_occupation_numbers
   end interface

   public    get_PND_ft_r_ints_
   interface get_PND_ft_r_ints_
      module procedure get_PND_ft_r_ints
   end interface

   public    no_of_unpaired_electrons_
   interface no_of_unpaired_electrons_
      module procedure no_of_unpaired_electrons
   end interface

   public    make_PND_r_sf_
   interface make_PND_r_sf_
      module procedure make_PND_r_sf
   end interface

   public    fermi_mobility_grid_
   interface fermi_mobility_grid_
      module procedure fermi_mobility_grid
   end interface

   public    make_r_fock_
   interface make_r_fock_
      module procedure make_r_fock
   end interface

   public    make_electric_potential_grid_r_
   interface make_electric_potential_grid_r_
      module procedure make_electric_potential_grid_r
   end interface

   public    electric_potential_grid_
   interface electric_potential_grid_
      module procedure electric_potential_grid
   end interface

   public    make_atom_groups_
   interface make_atom_groups_
      module procedure make_atom_groups
   end interface

   public    make_core_matrix_
   interface make_core_matrix_
      module procedure make_core_matrix
   end interface

   public    nuclear_EFG_at_nuclei_
   interface nuclear_EFG_at_nuclei_
      module procedure nuclear_EFG_at_nuclei
   end interface

   public    blockwise_symmetric_fold_
   interface blockwise_symmetric_fold_
      module procedure blockwise_symmetric_fold
   end interface

   public    make_true_fermi_mobil_grid_r_
   interface make_true_fermi_mobil_grid_r_
      module procedure make_true_fermi_mobil_grid_r
   end interface

   public    MO_gradient_update_
   interface MO_gradient_update_
      module procedure MO_gradient_update
   end interface

   public    copy_
   interface copy_
      module procedure copy
   end interface

   public    electronic_EFG_at_nuclei_
   interface electronic_EFG_at_nuclei_
      module procedure electronic_EFG_at_nuclei
   end interface

   public    update_scfdata_
   interface update_scfdata_
      module procedure update_scfdata
   end interface

   public    make_true_fermi_mobility_grid_
   interface make_true_fermi_mobility_grid_
      module procedure make_true_fermi_mobility_grid
      module procedure make_true_fermi_mobility_grid_1
   end interface

   public    write_archive_
   interface write_archive_
      module procedure write_archive
   end interface

   public    move_origin_to_centre_of_mass_
   interface move_origin_to_centre_of_mass_
      module procedure move_origin_to_centre_of_mass
   end interface

   public    make_PND_ft_nabla_ints_
   interface make_PND_ft_nabla_ints_
      module procedure make_PND_ft_nabla_ints
   end interface

   public    add_r_correlation_matrix_
   interface add_r_correlation_matrix_
      module procedure add_r_correlation_matrix
   end interface

   public    grad_rho_on_rho_grid_
   interface grad_rho_on_rho_grid_
      module procedure grad_rho_on_rho_grid
   end interface

   public    read_isosurface_
   interface read_isosurface_
      module procedure read_isosurface
   end interface

   public    put_plotgrid_
   interface put_plotgrid_
      module procedure put_plotgrid
   end interface

   public    read_atoms_
   interface read_atoms_
      module procedure read_atoms
   end interface

   public    make_grid_densities_
   interface make_grid_densities_
      module procedure make_grid_densities
   end interface

   public    make_scf_density_matrix_
   interface make_scf_density_matrix_
      module procedure make_scf_density_matrix
      module procedure make_scf_density_matrix_1
   end interface

   public    schmidt_orthonormalise_MOs_
   interface schmidt_orthonormalise_MOs_
      module procedure schmidt_orthonormalise_MOs
   end interface

   public    make_group_density_
   interface make_group_density_
      module procedure make_group_density
   end interface

   public    canonicalize_r_MO_
   interface canonicalize_r_MO_
      module procedure canonicalize_r_MO
   end interface

   public    put_time_taken_
   interface put_time_taken_
      module procedure put_time_taken
   end interface

   public    make_electric_field_matrices_
   interface make_electric_field_matrices_
      module procedure make_electric_field_matrices
   end interface

   public    get_crystal_geometry_
   interface get_crystal_geometry_
      module procedure get_crystal_geometry
   end interface

   public    make_weak_force_energy_shift_
   interface make_weak_force_energy_shift_
      module procedure make_weak_force_energy_shift
   end interface

   public    put_crystal_reflection_data_
   interface put_crystal_reflection_data_
      module procedure put_crystal_reflection_data
   end interface

   public    make_PND_ft_r_ints_
   interface make_PND_ft_r_ints_
      module procedure make_PND_ft_r_ints
   end interface

   public    make_laplacian_orbital_grid_r_
   interface make_laplacian_orbital_grid_r_
      module procedure make_laplacian_orbital_grid_r
   end interface

   private    get_shell_
   interface get_shell_
      module procedure get_shell
      module procedure get_shell_1
      module procedure get_shell_2
   end interface

   public    read_slaterbasis_sets_
   interface read_slaterbasis_sets_
      module procedure read_slaterbasis_sets
   end interface

   public    usual_scf_
   interface usual_scf_
      module procedure usual_scf
   end interface

   public    add_gc_so_fock_
   interface add_gc_so_fock_
      module procedure add_gc_so_fock
   end interface

   public    make_unrestricted_NOs_
   interface make_unrestricted_NOs_
      module procedure make_unrestricted_NOs
   end interface

   public    make_gc_fock_
   interface make_gc_fock_
      module procedure make_gc_fock
   end interface

   public    make_r_JK_nosym_
   interface make_r_JK_nosym_
      module procedure make_r_JK_nosym
   end interface

   public    make_u_JK_direct_
   interface make_u_JK_direct_
      module procedure make_u_JK_direct
   end interface

   public    orbital_grid_
   interface orbital_grid_
      module procedure orbital_grid
   end interface

   public    make_atom_density_
   interface make_atom_density_
      module procedure make_atom_density
   end interface

   public    make_nddo_kinetic_overlap_
   interface make_nddo_kinetic_overlap_
      module procedure make_nddo_kinetic_overlap
   end interface

   public    schwarz_inequality_test_
   interface schwarz_inequality_test_
      module procedure schwarz_inequality_test
   end interface

   public    put_molecular_orbitals_
   interface put_molecular_orbitals_
      module procedure put_molecular_orbitals
   end interface

   public    get_spin_orbit_integrals_
   interface get_spin_orbit_integrals_
      module procedure get_spin_orbit_integrals
   end interface

   public    make_MO_r_gradient_
   interface make_MO_r_gradient_
      module procedure make_MO_r_gradient
   end interface

   public    put_dftgrid_
   interface put_dftgrid_
      module procedure put_dftgrid
   end interface

   public    thermal_smearing_correction_
   interface thermal_smearing_correction_
      module procedure thermal_smearing_correction
   end interface

   public    scf_energy_
   interface scf_energy_
      module procedure scf_energy
      module procedure scf_energy_1
      module procedure scf_energy_2
   end interface

   public    make_density_grid_
   interface make_density_grid_
      module procedure make_density_grid
   end interface

   public    read_name_
   interface read_name_
      module procedure read_name
   end interface

   public    get_atom_pair_indices_
   interface get_atom_pair_indices_
      module procedure get_atom_pair_indices
      module procedure get_atom_pair_indices_1
   end interface

   public    MO_r_eigen_update_
   interface MO_r_eigen_update_
      module procedure MO_r_eigen_update
   end interface

   public    weight_diagonal_blocks_
   interface weight_diagonal_blocks_
      module procedure weight_diagonal_blocks
      module procedure weight_diagonal_blocks_1
   end interface

   public    make_shell_info_
   interface make_shell_info_
      module procedure make_shell_info
   end interface

   public    make_group_density_matrix_
   interface make_group_density_matrix_
      module procedure make_group_density_matrix
   end interface

   public    make_u_JK_disk_
   interface make_u_JK_disk_
      module procedure make_u_JK_disk
   end interface

   public    no_of_occupied_NOs_
   interface no_of_occupied_NOs_
      module procedure no_of_occupied_NOs
   end interface

   public    electronic_dipole_moment_
   interface electronic_dipole_moment_
      module procedure electronic_dipole_moment
   end interface

   public    make_nabla_orbital_grid_c_
   interface make_nabla_orbital_grid_c_
      module procedure make_nabla_orbital_grid_c
   end interface

   public    nuclear_octupole_moment_
   interface nuclear_octupole_moment_
      module procedure nuclear_octupole_moment
   end interface

   public    put_natural_orbitals_
   interface put_natural_orbitals_
      module procedure put_natural_orbitals
   end interface

   public    read_B_field_
   interface read_B_field_
      module procedure read_B_field
   end interface

   public    archive_occupation_numbers_
   interface archive_occupation_numbers_
      module procedure archive_occupation_numbers
   end interface

   public    read_pointgroup_
   interface read_pointgroup_
      module procedure read_pointgroup
   end interface

   public    set_defaults_
   interface set_defaults_
      module procedure set_defaults
   end interface

   public    make_contraction_matrix_
   interface make_contraction_matrix_
      module procedure make_contraction_matrix
   end interface

   public    put_bases_
   interface put_bases_
      module procedure put_bases
   end interface

   public    make_spin_orbit_Q_matrices_
   interface make_spin_orbit_Q_matrices_
      module procedure make_spin_orbit_Q_matrices
   end interface

   public    put_fock_matrix_
   interface put_fock_matrix_
      module procedure put_fock_matrix
   end interface

   public    u_exchange_functional_
   interface u_exchange_functional_
      module procedure u_exchange_functional
   end interface

   public    make_laplacian_grid_r_
   interface make_laplacian_grid_r_
      module procedure make_laplacian_grid_r
   end interface

   public    make_ft_ints_
   interface make_ft_ints_
      module procedure make_ft_ints
   end interface

   public    read_multiplicity_
   interface read_multiplicity_
      module procedure read_multiplicity
   end interface

   public    destroy_matrices_
   interface destroy_matrices_
      module procedure destroy_matrices
   end interface

   public    assign_MOs_to_NOs_
   interface assign_MOs_to_NOs_
      module procedure assign_MOs_to_NOs
   end interface

   public    resolve_basis_info_
   interface resolve_basis_info_
      module procedure resolve_basis_info
   end interface

   public    make_u_fock_
   interface make_u_fock_
      module procedure make_u_fock
   end interface

   public    dft_energy_correction_
   interface dft_energy_correction_
      module procedure dft_energy_correction
   end interface

   public    fit_thermal_parameters_
   interface fit_thermal_parameters_
      module procedure fit_thermal_parameters
   end interface

   public    make_r_fock_guess_
   interface make_r_fock_guess_
      module procedure make_r_fock_guess
   end interface

   public    initialise_scfdata_
   interface initialise_scfdata_
      module procedure initialise_scfdata
   end interface

   public    delete_old_scf_archives_
   interface delete_old_scf_archives_
      module procedure delete_old_scf_archives
   end interface

   public    unsave_
   interface unsave_
      module procedure unsave
   end interface

   public    make_u_nabla_density_grid_
   interface make_u_nabla_density_grid_
      module procedure make_u_nabla_density_grid
   end interface

   public    make_unrestricted_density_grid_
   interface make_unrestricted_density_grid_
      module procedure make_unrestricted_density_grid
   end interface

   public    make_electronic_pot_grid_r_
   interface make_electronic_pot_grid_r_
      module procedure make_electronic_pot_grid_r
   end interface

   public    make_PND_scalar_magnetic_sf_
   interface make_PND_scalar_magnetic_sf_
      module procedure make_PND_scalar_magnetic_sf
   end interface

   public    orbital_density_grid_
   interface orbital_density_grid_
      module procedure orbital_density_grid
   end interface

   public    make_general_complex_NOs_
   interface make_general_complex_NOs_
      module procedure make_general_complex_NOs
   end interface

   public    make_EFG_matrices_
   interface make_EFG_matrices_
      module procedure make_EFG_matrices
   end interface

   public    make_spin_density_grid_
   interface make_spin_density_grid_
      module procedure make_spin_density_grid
   end interface

   public    make_electric_potential_grid_
   interface make_electric_potential_grid_
      module procedure make_electric_potential_grid
      module procedure make_electric_potential_grid_1
   end interface

   public    make_r_dft_fock_
   interface make_r_dft_fock_
      module procedure make_r_dft_fock
   end interface

   public    read_gauge_origin_
   interface read_gauge_origin_
      module procedure read_gauge_origin
   end interface

   public    create_
   interface create_
      module procedure create
   end interface

   public    set_precomp_shell_quartet_ab_
   interface set_precomp_shell_quartet_ab_
      module procedure set_precomp_shell_quartet_ab
   end interface

   public    make_L_matrices_
   interface make_L_matrices_
      module procedure make_L_matrices
   end interface

   public    add_constraint_
   interface add_constraint_
      module procedure add_constraint
   end interface

   public    set_scf_defaults_
   interface set_scf_defaults_
      module procedure set_scf_defaults
   end interface

   public    atom_group_AO_subspace_set_
   interface atom_group_AO_subspace_set_
      module procedure atom_group_AO_subspace_set
   end interface

   public    get_core_matrix_
   interface get_core_matrix_
      module procedure get_core_matrix
   end interface

   public    put_AO_energy_partition_
   interface put_AO_energy_partition_
      module procedure put_AO_energy_partition
   end interface

   public    create_copy_
   interface create_copy_
      module procedure create_copy
   end interface

   public    make_basis_shell_for_shell_
   interface make_basis_shell_for_shell_
      module procedure make_basis_shell_for_shell
   end interface

   public    make_nabla_density_grid_r_
   interface make_nabla_density_grid_r_
      module procedure make_nabla_density_grid_r
   end interface

   public    read_group_charges_
   interface read_group_charges_
      module procedure read_group_charges
   end interface

   public    resolve_gaussianbasis_info_
   interface resolve_gaussianbasis_info_
      module procedure resolve_gaussianbasis_info
   end interface

   public    make_r_group_fock_
   interface make_r_group_fock_
      module procedure make_r_group_fock
   end interface

   public    electron_density_grid_
   interface electron_density_grid_
      module procedure electron_density_grid
   end interface

   public    set_diagonal_blocks_
   interface set_diagonal_blocks_
      module procedure set_diagonal_blocks
   end interface

   public    delete_scf_density_archive_
   interface delete_scf_density_archive_
      module procedure delete_scf_density_archive
   end interface

   public    set_shell_quartet_ab_
   interface set_shell_quartet_ab_
      module procedure set_shell_quartet_ab
   end interface

   public    get_spin_orbit_B_matrices_
   interface get_spin_orbit_B_matrices_
      module procedure get_spin_orbit_B_matrices
   end interface

   public    make_fermi_mobility_grid_r_
   interface make_fermi_mobility_grid_r_
      module procedure make_fermi_mobility_grid_r
   end interface

   public    archive_scf_results_
   interface archive_scf_results_
      module procedure archive_scf_results
   end interface

   public    put_E_field_at_nuclei_
   interface put_E_field_at_nuclei_
      module procedure put_E_field_at_nuclei
   end interface

   public    put_total_time_
   interface put_total_time_
      module procedure put_total_time
   end interface

   public    read_ascii_archive_
   interface read_ascii_archive_
      module procedure read_ascii_archive
   end interface

   public    r_ex_corr_energy_
   interface r_ex_corr_energy_
      module procedure r_ex_corr_energy
   end interface

   public    get_spin_orbit_Q_matrices_
   interface get_spin_orbit_Q_matrices_
      module procedure get_spin_orbit_Q_matrices
   end interface

   public    scf_
   interface scf_
      module procedure scf
   end interface

   public    first_basis_shell_for_atom_
   interface first_basis_shell_for_atom_
      module procedure first_basis_shell_for_atom
   end interface

   public    make_orbital_density_grid_
   interface make_orbital_density_grid_
      module procedure make_orbital_density_grid
   end interface

   public    sfchi2_
   interface sfchi2_
      module procedure sfchi2
   end interface

   public    read_old_MOs_guess_
   interface read_old_MOs_guess_
      module procedure read_old_MOs_guess
   end interface

   public    no_of_atom_pairs_
   interface no_of_atom_pairs_
      module procedure no_of_atom_pairs
   end interface

   public    put_crystal_
   interface put_crystal_
      module procedure put_crystal
   end interface

   public    make_ao_density_matrix_
   interface make_ao_density_matrix_
      module procedure make_ao_density_matrix
   end interface

   public    get_shell_quartet_indexes_
   interface get_shell_quartet_indexes_
      module procedure get_shell_quartet_indexes
   end interface

   public    get_shell_pair_indices_
   interface get_shell_pair_indices_
      module procedure get_shell_pair_indices
      module procedure get_shell_pair_indices_1
      module procedure get_shell_pair_indices_2
   end interface

   public    unarchive_fock_matrix_
   interface unarchive_fock_matrix_
      module procedure unarchive_fock_matrix
   end interface

   public    make_nabla_density_grid_fdm_r_
   interface make_nabla_density_grid_fdm_r_
      module procedure make_nabla_density_grid_fdm_r
   end interface

   public    unarchive_density_matrix_
   interface unarchive_density_matrix_
      module procedure unarchive_density_matrix
   end interface

   public    make_quadrupole_matrices_
   interface make_quadrupole_matrices_
      module procedure make_quadrupole_matrices
   end interface

   public    put_energy_partition_
   interface put_energy_partition_
      module procedure put_energy_partition
   end interface

   public    make_nddo_nuclear_matrix_
   interface make_nddo_nuclear_matrix_
      module procedure make_nddo_nuclear_matrix
      module procedure make_nddo_nuclear_matrix_1
   end interface

   public    make_difference_density_grid_
   interface make_difference_density_grid_
      module procedure make_difference_density_grid
   end interface

   public    get_shell_quartet_
   interface get_shell_quartet_
      module procedure get_shell_quartet
      module procedure get_shell_quartet_1
      module procedure get_shell_quartet_2
      module procedure get_shell_quartet_3
   end interface

   public    reset_constraint_stuff_
   interface reset_constraint_stuff_
      module procedure reset_constraint_stuff
   end interface

   public    put_current_time_
   interface put_current_time_
      module procedure put_current_time
   end interface

   public    make_mulliken_matrix_
   interface make_mulliken_matrix_
      module procedure make_mulliken_matrix
   end interface

   public    force_thermal_symmetry_
   interface force_thermal_symmetry_
      module procedure force_thermal_symmetry
   end interface

   public    archive_orbital_energies_
   interface archive_orbital_energies_
      module procedure archive_orbital_energies
   end interface

   public    make_Tsirelson_ELF_grid_r_
   interface make_Tsirelson_ELF_grid_r_
      module procedure make_Tsirelson_ELF_grid_r
   end interface

   public    archive_natural_orbitals_
   interface archive_natural_orbitals_
      module procedure archive_natural_orbitals
   end interface

   public    true_fermi_mobility_grid_
   interface true_fermi_mobility_grid_
      module procedure true_fermi_mobility_grid
   end interface

   public    put_1e_properties_
   interface put_1e_properties_
      module procedure put_1e_properties
   end interface

   public    get_PND_ft_nabla_ints_
   interface get_PND_ft_nabla_ints_
      module procedure get_PND_ft_nabla_ints
   end interface

   public    chemical_formula_
   interface chemical_formula_
      module procedure chemical_formula
   end interface

   public    add_core_hamiltonian_
   interface add_core_hamiltonian_
      module procedure add_core_hamiltonian
      module procedure add_core_hamiltonian_1
   end interface

   public    get_kinetic_matrix_
   interface get_kinetic_matrix_
      module procedure get_kinetic_matrix
   end interface

   public    read_robydata_
   interface read_robydata_
      module procedure read_robydata
   end interface

   public    symmetrically_orthonormalise_
   interface symmetrically_orthonormalise_
      module procedure symmetrically_orthonormalise
   end interface

   public    get_overlap_matrix_
   interface get_overlap_matrix_
      module procedure get_overlap_matrix
   end interface

   public    get_shell_pair_
   interface get_shell_pair_
      module procedure get_shell_pair
      module procedure get_shell_pair_1
   end interface

   public    make_max_density_elements_
   interface make_max_density_elements_
      module procedure make_max_density_elements
   end interface

   public    symmetrise_
   interface symmetrise_
      module procedure symmetrise
   end interface

   public    make_density_guess_
   interface make_density_guess_
      module procedure make_density_guess
   end interface

   public    put_roby_energy_partition_
   interface put_roby_energy_partition_
      module procedure put_roby_energy_partition
   end interface

   public    unarchive_molecular_orbitals_
   interface unarchive_molecular_orbitals_
      module procedure unarchive_molecular_orbitals
   end interface

   public    make_structure_factors_
   interface make_structure_factors_
      module procedure make_structure_factors
   end interface

   public    centre_of_mass_
   interface centre_of_mass_
      module procedure centre_of_mass
   end interface

   public    read_scfdata_
   interface read_scfdata_
      module procedure read_scfdata
   end interface

   public    put_SAO_energy_partition_
   interface put_SAO_energy_partition_
      module procedure put_SAO_energy_partition
   end interface

   public    make_group_kinetic_overlap_
   interface make_group_kinetic_overlap_
      module procedure make_group_kinetic_overlap
   end interface

   public    put_dipole_
   interface put_dipole_
      module procedure put_dipole
   end interface

   public    make_Tsirelson_ELF_grid_
   interface make_Tsirelson_ELF_grid_
      module procedure make_Tsirelson_ELF_grid
      module procedure make_Tsirelson_ELF_grid_1
   end interface

   public    make_ro_fock_
   interface make_ro_fock_
      module procedure make_ro_fock
   end interface

   public    make_gc_PND_spin_sf_
   interface make_gc_PND_spin_sf_
      module procedure make_gc_PND_spin_sf
   end interface

   public    put_roby_mmo_energy_partition_
   interface put_roby_mmo_energy_partition_
      module procedure put_roby_mmo_energy_partition
   end interface

   public    make_orbital_grid_r_
   interface make_orbital_grid_r_
      module procedure make_orbital_grid_r
   end interface

   public    save_
   interface save_
      module procedure save
   end interface

   public    no_of_beta_electrons_
   interface no_of_beta_electrons_
      module procedure no_of_beta_electrons
   end interface

   public    put_mos_and_energies_
   interface put_mos_and_energies_
      module procedure put_mos_and_energies
   end interface

   public    put_pointgroup_
   interface put_pointgroup_
      module procedure put_pointgroup
   end interface

   public    make_spin_orbit_matrices_
   interface make_spin_orbit_matrices_
      module procedure make_spin_orbit_matrices
   end interface

   public    laplacian_density_grid_
   interface laplacian_density_grid_
      module procedure laplacian_density_grid
   end interface

   public    make_r_core_guess_
   interface make_r_core_guess_
      module procedure make_r_core_guess
   end interface

   public    get_octupole_matrices_
   interface get_octupole_matrices_
      module procedure get_octupole_matrices
   end interface

   public    kinetic_energy_
   interface kinetic_energy_
      module procedure kinetic_energy
   end interface

   public    swap_g94_orbital_order_
   interface swap_g94_orbital_order_
      module procedure swap_g94_orbital_order
   end interface

   public    get_jp_density_grid_
   interface get_jp_density_grid_
      module procedure get_jp_density_grid
   end interface

   public    read_atom_groups_
   interface read_atom_groups_
      module procedure read_atom_groups
   end interface

   public    spin_density_grid_
   interface spin_density_grid_
      module procedure spin_density_grid
   end interface

   public    make_irrotational_jp_grid_
   interface make_irrotational_jp_grid_
      module procedure make_irrotational_jp_grid
      module procedure make_irrotational_jp_grid_1
   end interface

   public    make_r_group_JK_
   interface make_r_group_JK_
      module procedure make_r_group_JK
   end interface

   public    make_core_guess_
   interface make_core_guess_
      module procedure make_core_guess
   end interface

   public    expectation_
   interface expectation_
      module procedure expectation
   end interface

   public    make_j_density_grid_
   interface make_j_density_grid_
      module procedure make_j_density_grid
   end interface

   public    make_fock_guess_
   interface make_fock_guess_
      module procedure make_fock_guess
   end interface

   public    unarchive_orbital_energies_
   interface unarchive_orbital_energies_
      module procedure unarchive_orbital_energies
   end interface

   public    make_nudo_kinetic_overlap_
   interface make_nudo_kinetic_overlap_
      module procedure make_nudo_kinetic_overlap
   end interface

   public    make_r_JK_disk_
   interface make_r_JK_disk_
      module procedure make_r_JK_disk
   end interface

   public    make_vib_averaged_rho_grid_
   interface make_vib_averaged_rho_grid_
      module procedure make_vib_averaged_rho_grid
   end interface

   public    make_u_dft_fock_
   interface make_u_dft_fock_
      module procedure make_u_dft_fock
   end interface

   public    archive_exists_
   interface archive_exists_
      module procedure archive_exists
   end interface

   public    make_gc_so_JK_direct_
   interface make_gc_so_JK_direct_
      module procedure make_gc_so_JK_direct
   end interface

   public    atom_index_from_pos_
   interface atom_index_from_pos_
      module procedure atom_index_from_pos
   end interface

   public    delete_scf_integrals_
   interface delete_scf_integrals_
      module procedure delete_scf_integrals
   end interface

   public    make_restricted_complex_NOs_
   interface make_restricted_complex_NOs_
      module procedure make_restricted_complex_NOs
   end interface

   public    get_nuclear_matrix_
   interface get_nuclear_matrix_
      module procedure get_nuclear_matrix
   end interface

   public    isosurface_plot_
   interface isosurface_plot_
      module procedure isosurface_plot
   end interface

   public    nuclear_dipole_moment_
   interface nuclear_dipole_moment_
      module procedure nuclear_dipole_moment
   end interface

   public    put_vrml_
   interface put_vrml_
      module procedure put_vrml
   end interface

   public    put_coppensbases_
   interface put_coppensbases_
      module procedure put_coppensbases
   end interface

   public    get_precomp_shell_pair_
   interface get_precomp_shell_pair_
      module procedure get_precomp_shell_pair
   end interface

   public    resolve_coppensbasis_info_
   interface resolve_coppensbasis_info_
      module procedure resolve_coppensbasis_info
   end interface

   public    read_cluster_
   interface read_cluster_
      module procedure read_cluster
   end interface

   public    get_quadrupole_matrices_
   interface get_quadrupole_matrices_
      module procedure get_quadrupole_matrices
   end interface

   public    make_diis_error_
   interface make_diis_error_
      module procedure make_diis_error
   end interface

   public    make_overlap_matrix_
   interface make_overlap_matrix_
      module procedure make_overlap_matrix
   end interface

   public    make_nudo_kinetic_matrix_
   interface make_nudo_kinetic_matrix_
      module procedure make_nudo_kinetic_matrix
   end interface

   public    plot_
   interface plot_
      module procedure plot
   end interface

   public    make_molecule_from_atom_
   interface make_molecule_from_atom_
      module procedure make_molecule_from_atom
   end interface

   public    r_correlation_functional_
   interface r_correlation_functional_
      module procedure r_correlation_functional
   end interface

   public    make_natural_orbitals_
   interface make_natural_orbitals_
      module procedure make_natural_orbitals
   end interface

   public    make_electronic_pot_grid_c_
   interface make_electronic_pot_grid_c_
      module procedure make_electronic_pot_grid_c
   end interface

   public    r_dft_energy_correction_
   interface r_dft_energy_correction_
      module procedure r_dft_energy_correction
   end interface

   public    read_coppensbasis_sets_
   interface read_coppensbasis_sets_
      module procedure read_coppensbasis_sets
   end interface

   public    make_gc_JK_disk_
   interface make_gc_JK_disk_
      module procedure make_gc_JK_disk
   end interface

   public    make_jd_density_grid_
   interface make_jd_density_grid_
      module procedure make_jd_density_grid
   end interface

   public    nuclear_quadrupole_moment_
   interface nuclear_quadrupole_moment_
      module procedure nuclear_quadrupole_moment
   end interface

   public    make_div_jp_density_grid_
   interface make_div_jp_density_grid_
      module procedure make_div_jp_density_grid
      module procedure make_div_jp_density_grid_1
   end interface

   public    get_group_density_
   interface get_group_density_
      module procedure get_group_density
   end interface

   public    put_quadrupole_
   interface put_quadrupole_
      module procedure put_quadrupole
   end interface

   public    u_correlation_functional_
   interface u_correlation_functional_
      module procedure u_correlation_functional
   end interface

   public    make_nabla_density_grid_
   interface make_nabla_density_grid_
      module procedure make_nabla_density_grid
   end interface

   public    set_precomp_shell_quartet_cd_
   interface set_precomp_shell_quartet_cd_
      module procedure set_precomp_shell_quartet_cd
   end interface

   public    blockwise_hermitian_fold_
   interface blockwise_hermitian_fold_
      module procedure blockwise_hermitian_fold
   end interface

   public    put_density_matrix_
   interface put_density_matrix_
      module procedure put_density_matrix
   end interface

   public    put_EFG_at_nuclei_
   interface put_EFG_at_nuclei_
      module procedure put_EFG_at_nuclei
   end interface

   public    put_slaterbases_
   interface put_slaterbases_
      module procedure put_slaterbases
   end interface

   public    set_shell_quartet_cd_
   interface set_shell_quartet_cd_
      module procedure set_shell_quartet_cd
   end interface

   public    update_scfdata_error_
   interface update_scfdata_error_
      module procedure update_scfdata_error
   end interface

   public    molecular_weight_
   interface molecular_weight_
      module procedure molecular_weight
   end interface

   public    read_basis_set_kind_
   interface read_basis_set_kind_
      module procedure read_basis_set_kind
   end interface

   public    write_wfn_file_
   interface write_wfn_file_
      module procedure write_wfn_file
   end interface

   public    no_of_electrons_
   interface no_of_electrons_
      module procedure no_of_electrons
   end interface

   public    nuclear_potential_
   interface nuclear_potential_
      module procedure nuclear_potential
   end interface

   public    make_orbital_grid_
   interface make_orbital_grid_
      module procedure make_orbital_grid
   end interface

   public    put_cluster_
   interface put_cluster_
      module procedure put_cluster
   end interface

   public    make_r_diis_error_
   interface make_r_diis_error_
      module procedure make_r_diis_error
   end interface

   public    make_restricted_NOs_
   interface make_restricted_NOs_
      module procedure make_restricted_NOs
   end interface

   public    get_spin_orbit_matrices_
   interface get_spin_orbit_matrices_
      module procedure get_spin_orbit_matrices
   end interface

   public    get_ft_ints_
   interface get_ft_ints_
      module procedure get_ft_ints
   end interface

   public    make_r_J_direct_
   interface make_r_J_direct_
      module procedure make_r_J_direct
   end interface

   public    archive_molecular_orbitals_
   interface archive_molecular_orbitals_
      module procedure archive_molecular_orbitals
   end interface

   public    get_PND_ft_spin_ints_
   interface get_PND_ft_spin_ints_
      module procedure get_PND_ft_spin_ints
   end interface

   private    make_shellpair_vector_
   interface make_shellpair_vector_
      module procedure make_shellpair_vector
   end interface

   public    get_jd_density_grid_
   interface get_jd_density_grid_
      module procedure get_jd_density_grid
   end interface

   private    AO_subspace_set_
   interface AO_subspace_set_
      module procedure AO_subspace_set
   end interface

   private    AO_subspace_put_
   interface AO_subspace_put_
      module procedure AO_subspace_put
   end interface

   public    make_dipole_matrices_
   interface make_dipole_matrices_
      module procedure make_dipole_matrices
   end interface

   public    no_of_alpha_electrons_
   interface no_of_alpha_electrons_
      module procedure no_of_alpha_electrons
   end interface

   public    numbered_chemical_symbols_
   interface numbered_chemical_symbols_
      module procedure numbered_chemical_symbols
   end interface

   public    symmetrise_r_
   interface symmetrise_r_
      module procedure symmetrise_r
   end interface

   public    r_exchange_functional_
   interface r_exchange_functional_
      module procedure r_exchange_functional
   end interface

   public    make_max_abab_integrals_
   interface make_max_abab_integrals_
      module procedure make_max_abab_integrals
   end interface

   public    Tsirelson_ELF_grid_
   interface Tsirelson_ELF_grid_
      module procedure Tsirelson_ELF_grid
   end interface

   public    make_nudo_nuclear_matrix_
   interface make_nudo_nuclear_matrix_
      module procedure make_nudo_nuclear_matrix
      module procedure make_nudo_nuclear_matrix_1
   end interface

   public    make_laplacian_grid_
   interface make_laplacian_grid_
      module procedure make_laplacian_grid
      module procedure make_laplacian_grid_1
   end interface

   public    make_density_grid_r_
   interface make_density_grid_r_
      module procedure make_density_grid_r
   end interface

   public    canonicalize_MOs_
   interface canonicalize_MOs_
      module procedure canonicalize_MOs
   end interface

   public    scf_electronic_energy_
   interface scf_electronic_energy_
      module procedure scf_electronic_energy
      module procedure scf_electronic_energy_1
      module procedure scf_electronic_energy_2
   end interface

   private    set_thermal_parameters_
   interface set_thermal_parameters_
      module procedure set_thermal_parameters
   end interface

   public    make_group_energies_
   interface make_group_energies_
      module procedure make_group_energies
   end interface

   public    read_E_field_
   interface read_E_field_
      module procedure read_E_field
   end interface

   public    initialise_scfdata_error_
   interface initialise_scfdata_error_
      module procedure initialise_scfdata_error
   end interface

   public    make_gc_JK_direct_
   interface make_gc_JK_direct_
      module procedure make_gc_JK_direct
   end interface

   public    make_PND_spin_sf_
   interface make_PND_spin_sf_
      module procedure make_PND_spin_sf
   end interface

   public    get_ANO_data_for_atom_
   interface get_ANO_data_for_atom_
      module procedure get_ANO_data_for_atom
   end interface

   public    make_promol_density_matrix_
   interface make_promol_density_matrix_
      module procedure make_promol_density_matrix
   end interface

   public    make_r_JK_direct_
   interface make_r_JK_direct_
      module procedure make_r_JK_direct
   end interface

   public    extrapolate_fock_matrix_
   interface extrapolate_fock_matrix_
      module procedure extrapolate_fock_matrix
   end interface

   public    make_ENA_matrix_
   interface make_ENA_matrix_
      module procedure make_ENA_matrix
   end interface

   public    make_atom_MOs_guess_
   interface make_atom_MOs_guess_
      module procedure make_atom_MOs_guess
   end interface

   public    add_local_ex_corr_matrix_
   interface add_local_ex_corr_matrix_
      module procedure add_local_ex_corr_matrix
      module procedure add_local_ex_corr_matrix_1
   end interface

   public    make_solenoidal_jp_grid_
   interface make_solenoidal_jp_grid_
      module procedure make_solenoidal_jp_grid
      module procedure make_solenoidal_jp_grid_1
   end interface

   public    plot_on_isosurface_
   interface plot_on_isosurface_
      module procedure plot_on_isosurface
   end interface

   public    delete_scf_archives_
   interface delete_scf_archives_
      module procedure delete_scf_archives
   end interface

   public    u_ex_corr_energy_
   interface u_ex_corr_energy_
      module procedure u_ex_corr_energy
   end interface

   public    read_archive_
   interface read_archive_
      module procedure read_archive
   end interface

   public    schmidt_orthonormalise_
   interface schmidt_orthonormalise_
      module procedure schmidt_orthonormalise
   end interface

   public    destroy_cluster_
   interface destroy_cluster_
      module procedure destroy_cluster
   end interface

   public    symorthonormalise_occupied_MOs_
   interface symorthonormalise_occupied_MOs_
      module procedure symorthonormalise_occupied_MOs
   end interface

   public    get_ERI_integrals_
   interface get_ERI_integrals_
      module procedure get_ERI_integrals
   end interface

   public    make_jp_density_grid_
   interface make_jp_density_grid_
      module procedure make_jp_density_grid
      module procedure make_jp_density_grid_1
   end interface

   public    nullify_ptr_part_
   interface nullify_ptr_part_
      module procedure nullify_ptr_part
   end interface

   public    add_r_exchange_matrix_
   interface add_r_exchange_matrix_
      module procedure add_r_exchange_matrix
   end interface

   public    make_group_kinetic_matrix_
   interface make_group_kinetic_matrix_
      module procedure make_group_kinetic_matrix
   end interface

   public    put_mulliken_populations_
   interface put_mulliken_populations_
      module procedure put_mulliken_populations
   end interface

   public    get_shell_quartet_indices_
   interface get_shell_quartet_indices_
      module procedure get_shell_quartet_indices
      module procedure get_shell_quartet_indices_1
   end interface

   public    get_dipole_matrices_
   interface get_dipole_matrices_
      module procedure get_dipole_matrices
   end interface

   public    make_orbital_grid_c_
   interface make_orbital_grid_c_
      module procedure make_orbital_grid_c
   end interface

   public    make_group_guess_
   interface make_group_guess_
      module procedure make_group_guess
   end interface

   public    symmetrise_c_
   interface symmetrise_c_
      module procedure symmetrise_c
   end interface

   public    get_atom_density_
   interface get_atom_density_
      module procedure get_atom_density
   end interface

   public    basis_set_library_
   interface basis_set_library_
      module procedure basis_set_library
   end interface

   public    make_u_PND_spin_sf_
   interface make_u_PND_spin_sf_
      module procedure make_u_PND_spin_sf
   end interface

   public    make_laplacian_orbital_grid_c_
   interface make_laplacian_orbital_grid_c_
      module procedure make_laplacian_orbital_grid_c
   end interface

   public    get_kinetic_overlap_
   interface get_kinetic_overlap_
      module procedure get_kinetic_overlap
   end interface

   public    set_basis_info_
   interface set_basis_info_
      module procedure set_basis_info
   end interface

   public    delete_scf_MO_archive_
   interface delete_scf_MO_archive_
      module procedure delete_scf_MO_archive
   end interface

   public    make_molecule_from_atom_group_
   interface make_molecule_from_atom_group_
      module procedure make_molecule_from_atom_group
   end interface

   public    no_of_shell_pairs_
   interface no_of_shell_pairs_
      module procedure no_of_shell_pairs
   end interface

   public    bounding_cube_width_
   interface bounding_cube_width_
      module procedure bounding_cube_width
   end interface

   public    put_MO_energy_partition_
   interface put_MO_energy_partition_
      module procedure put_MO_energy_partition
   end interface

   public    read_crystal_
   interface read_crystal_
      module procedure read_crystal
   end interface

   public    make_crystal_error_map_
   interface make_crystal_error_map_
      module procedure make_crystal_error_map
      module procedure make_crystal_error_map_1
   end interface

   public    put_scf_energy_in_mo_pairs_
   interface put_scf_energy_in_mo_pairs_
      module procedure put_scf_energy_in_mo_pairs
   end interface

   public    make_pie_nuclear_matrix_
   interface make_pie_nuclear_matrix_
      module procedure make_pie_nuclear_matrix
   end interface

   public    reduced_mass_
   interface reduced_mass_
      module procedure reduced_mass
   end interface

   private    get_thermal_parameters_
   interface get_thermal_parameters_
      module procedure get_thermal_parameters
   end interface

   public    make_density_grid_c_
   interface make_density_grid_c_
      module procedure make_density_grid_c
   end interface

   public    read_charge_
   interface read_charge_
      module procedure read_charge
   end interface

   public    make_ELF_grid_
   interface make_ELF_grid_
      module procedure make_ELF_grid
      module procedure make_ELF_grid_1
   end interface

   public    ZORA_potential_
   interface ZORA_potential_
      module procedure ZORA_potential
   end interface

   public    get_fock_matrix_
   interface get_fock_matrix_
      module procedure get_fock_matrix
   end interface

   public    make_nuclear_matrix_
   interface make_nuclear_matrix_
      module procedure make_nuclear_matrix
      module procedure make_nuclear_matrix_1
   end interface

   public    make_spin_orbit_B_matrices_
   interface make_spin_orbit_B_matrices_
      module procedure make_spin_orbit_B_matrices
   end interface

   public    make_gc_PND_nabla_sf_
   interface make_gc_PND_nabla_sf_
      module procedure make_gc_PND_nabla_sf
   end interface

   public    get_ANO_data_
   interface get_ANO_data_
      module procedure get_ANO_data
   end interface

   public    centre_of_atoms_
   interface centre_of_atoms_
      module procedure centre_of_atoms
   end interface

   public    save_constraint_for_diis_
   interface save_constraint_for_diis_
      module procedure save_constraint_for_diis
   end interface

   public    put_atoms_
   interface put_atoms_
      module procedure put_atoms
   end interface

   public    make_gc_diis_error_
   interface make_gc_diis_error_
      module procedure make_gc_diis_error
   end interface

   public    get_initial_guess_
   interface get_initial_guess_
      module procedure get_initial_guess
   end interface

   public    set_atom_info_
   interface set_atom_info_
      module procedure set_atom_info
   end interface

   public    make_ELF_grid_c_
   interface make_ELF_grid_c_
      module procedure make_ELF_grid_c
   end interface

   public    make_eri_integrals_
   interface make_eri_integrals_
      module procedure make_eri_integrals
   end interface

   public    make_PND_fit_ints_
   interface make_PND_fit_ints_
      module procedure make_PND_fit_ints
   end interface

   public    get_L_matrices_
   interface get_L_matrices_
      module procedure get_L_matrices
   end interface

   public    default_multiplicity_
   interface default_multiplicity_
      module procedure default_multiplicity
   end interface

   public    make_atom_guess_
   interface make_atom_guess_
      module procedure make_atom_guess
   end interface

   public    put_PND_sf_
   interface put_PND_sf_
      module procedure put_PND_sf
   end interface

   public    put_octupole_
   interface put_octupole_
      module procedure put_octupole
   end interface

   public    make_nabla_orbital_grid_r_
   interface make_nabla_orbital_grid_r_
      module procedure make_nabla_orbital_grid_r
   end interface

   public    get_scf_results_
   interface get_scf_results_
      module procedure get_scf_results
   end interface

   public    make_r_J_disk_
   interface make_r_J_disk_
      module procedure make_r_J_disk
   end interface

   public    read_plotgrid_
   interface read_plotgrid_
      module procedure read_plotgrid
   end interface

   public    MO_gc_eigen_update_
   interface MO_gc_eigen_update_
      module procedure MO_gc_eigen_update
   end interface

   public    assign_natural_orbitals_
   interface assign_natural_orbitals_
      module procedure assign_natural_orbitals
   end interface

   public    electronic_quadrupole_moment_
   interface electronic_quadrupole_moment_
      module procedure electronic_quadrupole_moment
   end interface

   public    make_gc_so_JK_disk_
   interface make_gc_so_JK_disk_
      module procedure make_gc_so_JK_disk
   end interface

   public    make_1e_ZORA_matrices_
   interface make_1e_ZORA_matrices_
      module procedure make_1e_ZORA_matrices
   end interface

   public    add_u_exchange_matrix_
   interface add_u_exchange_matrix_
      module procedure add_u_exchange_matrix
   end interface

   public    read_dftgrid_
   interface read_dftgrid_
      module procedure read_dftgrid
   end interface

   public    cleanup_scf_
   interface cleanup_scf_
      module procedure cleanup_scf
   end interface

   public    make_ft_pair_
   interface make_ft_pair_
      module procedure make_ft_pair
   end interface

   public    read_basis_sets_
   interface read_basis_sets_
      module procedure read_basis_sets
   end interface

   public    put_all_bonds_
   interface put_all_bonds_
      module procedure put_all_bonds
   end interface

   public    delete_scf_fock_archive_
   interface delete_scf_fock_archive_
      module procedure delete_scf_fock_archive
   end interface

   public    read_g94_checkpoint_file_
   interface read_g94_checkpoint_file_
      module procedure read_g94_checkpoint_file
   end interface

   public    make_electric_potential_grid_c_
   interface make_electric_potential_grid_c_
      module procedure make_electric_potential_grid_c
   end interface

   public    unarchive_natural_orbitals_
   interface unarchive_natural_orbitals_
      module procedure unarchive_natural_orbitals
   end interface

   public    make_ft_deriv_U_
   interface make_ft_deriv_U_
      module procedure make_ft_deriv_U
      module procedure make_ft_deriv_U_1
   end interface

   public    u_dft_energy_correction_
   interface u_dft_energy_correction_
      module procedure u_dft_energy_correction
   end interface

   public    roby_analysis_
   interface roby_analysis_
      module procedure roby_analysis
   end interface

   private    AO_subspace_zero_
   interface AO_subspace_zero_
      module procedure AO_subspace_zero
   end interface

   public    make_constraint_data_
   interface make_constraint_data_
      module procedure make_constraint_data
   end interface

   public    make_stockholder_grid_
   interface make_stockholder_grid_
      module procedure make_stockholder_grid
      module procedure make_stockholder_grid_1
   end interface

   public    get_geometry_
   interface get_geometry_
      module procedure get_geometry
   end interface

   public    make_laplacian_density_grid_
   interface make_laplacian_density_grid_
      module procedure make_laplacian_density_grid
   end interface

   public    get_vib_averaged_rho_grid_
   interface get_vib_averaged_rho_grid_
      module procedure get_vib_averaged_rho_grid
   end interface

   public    make_grad_rho_on_rho_grid_
   interface make_grad_rho_on_rho_grid_
      module procedure make_grad_rho_on_rho_grid
      module procedure make_grad_rho_on_rho_grid_1
   end interface

   public    get_initial_MOs_
   interface get_initial_MOs_
      module procedure get_initial_MOs
   end interface

   public    delete_gauge_integrals_
   interface delete_gauge_integrals_
      module procedure delete_gauge_integrals
   end interface

   public    resolve_slaterbasis_info_
   interface resolve_slaterbasis_info_
      module procedure resolve_slaterbasis_info
   end interface

   public    make_partition_factors_
   interface make_partition_factors_
      module procedure make_partition_factors
   end interface

   public    stockholder_density_grid_
   interface stockholder_density_grid_
      module procedure stockholder_density_grid
   end interface

   public    make_nddo_kinetic_matrix_
   interface make_nddo_kinetic_matrix_
      module procedure make_nddo_kinetic_matrix
   end interface

   public    put_atom_groups_
   interface put_atom_groups_
      module procedure put_atom_groups
   end interface

   public    destroy_
   interface destroy_
      module procedure destroy
   end interface

   public    archive_density_matrix_
   interface archive_density_matrix_
      module procedure archive_density_matrix
   end interface

   public    make_max_abab_so_integrals_
   interface make_max_abab_so_integrals_
      module procedure make_max_abab_so_integrals
   end interface

   public    make_fermi_mobility_grid_
   interface make_fermi_mobility_grid_
      module procedure make_fermi_mobility_grid
      module procedure make_fermi_mobility_grid_1
   end interface

   public    read_optimise_thermals_
   interface read_optimise_thermals_
      module procedure read_optimise_thermals
   end interface

   public    put_atom_thermal_tensors_
   interface put_atom_thermal_tensors_
      module procedure put_atom_thermal_tensors
   end interface

   public    ELF_grid_
   interface ELF_grid_
      module procedure ELF_grid
   end interface

   public    put_scf_energy_
   interface put_scf_energy_
      module procedure put_scf_energy
   end interface

   public    n_shell_quartets_
   interface n_shell_quartets_
      module procedure n_shell_quartets
   end interface

   public    put_scf_results_
   interface put_scf_results_
      module procedure put_scf_results
   end interface

   public    make_PND_ft_spin_ints_
   interface make_PND_ft_spin_ints_
      module procedure make_PND_ft_spin_ints
   end interface

   public    update_molecular_orbitals_
   interface update_molecular_orbitals_
      module procedure update_molecular_orbitals
   end interface

   public    make_PND_nabla_sf_
   interface make_PND_nabla_sf_
      module procedure make_PND_nabla_sf
   end interface

   public    write_ascii_archive_
   interface write_ascii_archive_
      module procedure write_ascii_archive
   end interface

   public    archive_fock_matrix_
   interface archive_fock_matrix_
      module procedure archive_fock_matrix
   end interface

   public    in_same_atom_group_
   interface in_same_atom_group_
      module procedure in_same_atom_group
      module procedure in_same_atom_group_1
   end interface

   type(mol_type), pointer, private :: saved_self

contains

!  **************************
!  Create and destroy methods
!  **************************

   subroutine create(self)
    type(mol_type) :: self
    ! Create a molecule object
      pointer :: self

      nullify(self)
      allocate(self)

      call nullify_ptr_part_(self)
      call set_defaults_(self)

   end subroutine

   recursive subroutine destroy(self)
    type(mol_type) :: self
    ! Destroy a molecule object
      pointer :: self

      if (.not. associated(self)) then;   return; end if
      call destroy_ptr_part_(self)
      deallocate(self)

   end subroutine

   subroutine nullify_ptr_part(self)
    type(mol_type) :: self
    ! Nullify the pointer parts of the molecule

      nullify(self%atom)
      nullify(self%basis)
      nullify(self%slaterbasis)
      nullify(self%coppensbasis)
      nullify(self%grid)
      nullify(self%isosurface)
      nullify(self%dftgrid)
      nullify(self%crystal)
      nullify(self%cluster)
      nullify(self%cif)
      nullify(self%pointgroup)
      nullify(self%atom_for_shell)
      nullify(self%atom_shell_for_shell)
      nullify(self%first_shell_for_atom)
      nullify(self%last_shell_for_atom)
      nullify(self%basis_shell_for_shell)
      nullify(self%first_basis_fn_for_shell)
      nullify(self%last_basis_fn_for_shell)
      nullify(self%first_basis_fn_for_atom)
      nullify(self%last_basis_fn_for_atom)
      nullify(self%precomputed_basis_shellpair)
      nullify(self%atom_kind)
      nullify(self%unique_atom)
      nullify(self%scfdata)
      nullify(self%orbital_energies)
      nullify(self%molecular_orbitals)
      nullify(self%density_matrix)
      nullify(self%natural_orbitals)
      nullify(self%occupation_numbers)
      nullify(self%fock_matrix)
      nullify(self%constraint_matrix)
      nullify(self%old_molecular_orbitals)
      nullify(self%old_density_matrix)
      nullify(self%old_fock_matrix)
      nullify(self%old_constraint_matrix)
      nullify(self%atom_group)
      nullify(self%atom_group_energy)
      nullify(self%group_charges)
      nullify(self%roby)
      nullify(self%saved)

   end subroutine

   recursive subroutine destroy_ptr_part(self)
    type(mol_type) :: self
    ! Destroy the pointer parts of a molecule object

      call destroy_(self%saved)
      if (associated(self%atom)) then
         call nullify_basis_part_(self%atom)  ! nullify this !
         call nullify_slaterbasis_part_(self%atom)
         call nullify_coppensbasis_part_(self%atom)
         call destroy_ANO_data_(self)         ! in case .roby was created
      end if
      call destroy_(self%atom)
      call destroy_(self%basis)
      call destroy_(self%slaterbasis)
      call destroy_(self%coppensbasis)
      call destroy_(self%grid)
      call destroy_(self%isosurface)
      call destroy_(self%dftgrid)
      call destroy_(self%crystal)
      call destroy_(self%cluster)
      call destroy_(self%cif)
      call destroy_(self%pointgroup)
      call destroy_(self%atom_for_shell)
      call destroy_(self%atom_shell_for_shell)
      call destroy_(self%first_shell_for_atom)
      call destroy_(self%last_shell_for_atom)
      call destroy_(self%basis_shell_for_shell)
      call destroy_(self%first_basis_fn_for_shell)
      call destroy_(self%last_basis_fn_for_shell)
      call destroy_(self%first_basis_fn_for_atom)
      call destroy_(self%last_basis_fn_for_atom)
      call destroy_(self%precomputed_basis_shellpair)
      call destroy_(self%atom_kind)
      call destroy_(self%unique_atom)
      call destroy_(self%scfdata)
      call destroy_(self%orbital_energies)
      call destroy_(self%molecular_orbitals)
      call destroy_(self%density_matrix)
      call destroy_(self%natural_orbitals)
      call destroy_(self%occupation_numbers)
      call destroy_(self%fock_matrix)
      call destroy_(self%constraint_matrix)
      call destroy_(self%old_molecular_orbitals)
      call destroy_(self%old_density_matrix)
      call destroy_(self%old_fock_matrix)
      call destroy_(self%old_constraint_matrix)
      call destroy_(self%atom_group)
      call destroy_(self%atom_group_energy)
      call destroy_(self%group_charges)
      call destroy_(self%roby)

   end subroutine

   subroutine destroy_matrices(self)
    type(mol_type) :: self
    ! Destroy the matrices of a molecule object

      call destroy_(self%atom_for_shell)
      call destroy_(self%atom_shell_for_shell)
      call destroy_(self%first_shell_for_atom)
      call destroy_(self%last_shell_for_atom)
      call destroy_(self%first_basis_fn_for_shell)
      call destroy_(self%last_basis_fn_for_shell)
      call destroy_(self%first_basis_fn_for_atom)
      call destroy_(self%last_basis_fn_for_atom)
      call destroy_(self%orbital_energies)
      call destroy_(self%molecular_orbitals)
      call destroy_(self%density_matrix)
      call destroy_(self%natural_orbitals)
      call destroy_(self%occupation_numbers)
      call destroy_(self%fock_matrix)
      call destroy_(self%old_molecular_orbitals)
      call destroy_(self%old_density_matrix)
      call destroy_(self%old_fock_matrix)
      call destroy_(self%old_constraint_matrix)
      call destroy_(self%constraint_matrix)
      call destroy_(self%atom_group)

   end subroutine

!   created result(res)
!   ! Returns true if self has been created
!      self :: pointer
!      res :: logical(kind=kind(.true.))
!      res = associated(self)
!   end

!   destroyed result(res)
!   ! Returns true if self has not been created
!      self :: pointer
!      res :: logical(kind=kind(.true.))
!      res = .not. associated(self)
!   end

   subroutine save(self)
    type(mol_type) :: self
    ! Save everything about "self" in ".saved".
      pointer :: self
      type(mol_type), pointer :: saved

      saved => self
      call create_(self)
      self%saved => saved

   end subroutine

   subroutine unsave(self)
    type(mol_type) :: self
    ! Revert back to a previously saved molecule stored in .saved, and
    ! destroy everything about the current molecule in "self".
    ! WARNING: be careful when setting up self, make sure none of its pointer
    ! parts refer to .saved information.
      pointer :: self
      type(mol_type), pointer :: saved

   call ensure_(tonto,associated(self%saved),"MOL:unsave ... no previous settings")
      saved => self%saved
      nullify(self%saved)  ! don't destroy this
      call destroy_(self)         ! careful no .saved information is destroyed
      self => saved    ! restore

   end subroutine

   subroutine create_copy(self,mol)
    type(mol_type) :: self
    ! Create a copy of "mol"
      pointer :: self
      type(mol_type), intent(in) :: mol

      call create_(self)
      call copy_(self,mol)

   end subroutine

   subroutine copy(self,mol)
    type(mol_type) :: self
    ! Set self to be a copy of "mol"
      type(mol_type), intent(in) :: mol
       ! Copy all non-pointer fields

      call nullify_ptr_part_(self)
      self = mol
       ! Create a copy of all pointer fields
      if (associated(mol%atom)) &
         call create_copy_(self%atom,mol%atom)
      if (associated(mol%basis)) &
         call create_copy_(self%basis,mol%basis)
      if (associated(mol%slaterbasis)) &
         call create_copy_(self%slaterbasis,mol%slaterbasis)
      if (associated(mol%coppensbasis)) &
         call create_copy_(self%coppensbasis,mol%coppensbasis)
      if (associated(mol%grid)) &
         call create_copy_(self%grid,mol%grid)
      if (associated(mol%dftgrid)) &
         call create_copy_(self%dftgrid,mol%dftgrid)
      if (associated(mol%crystal)) &
         call create_copy_(self%crystal,mol%crystal)
      if (associated(mol%cluster)) &
         call create_copy_(self%cluster,mol%cluster)
      if (associated(mol%cif)) &
         call create_copy_(self%cif,mol%cif)
      if (associated(mol%pointgroup)) &
         call create_copy_(self%pointgroup,mol%pointgroup)
      if (associated(mol%saved)) &
         call create_copy_(self%saved,mol%saved)
      if (associated(mol%atom_for_shell)) &
         call create_copy_(self%atom_for_shell,mol%atom_for_shell)
      if (associated(mol%atom_shell_for_shell)) &
         call create_copy_(self%atom_shell_for_shell,mol%atom_shell_for_shell)
      if (associated(mol%first_shell_for_atom)) &
         call create_copy_(self%first_shell_for_atom,mol%first_shell_for_atom)
      if (associated(mol%last_shell_for_atom)) &
         call create_copy_(self%last_shell_for_atom,mol%last_shell_for_atom)
      if (associated(mol%first_basis_fn_for_shell)) &
         call create_copy_(self%first_basis_fn_for_shell,mol%first_basis_fn_for_shell)
      if (associated(mol%last_basis_fn_for_shell)) &
         call create_copy_(self%last_basis_fn_for_shell,mol%last_basis_fn_for_shell)
      if (associated(mol%atom_kind)) &
         call create_copy_(self%atom_kind,mol%atom_kind)
      if (associated(mol%unique_atom)) &
         call create_copy_(self%unique_atom,mol%unique_atom)
      if (associated(mol%first_basis_fn_for_atom)) &
         call create_copy_(self%first_basis_fn_for_atom,mol%first_basis_fn_for_atom)
      if (associated(mol%last_basis_fn_for_atom)) &
         call create_copy_(self%last_basis_fn_for_atom,mol%last_basis_fn_for_atom)
      if (associated(mol%scfdata)) &
         call create_copy_(self%scfdata,mol%scfdata)
      if (associated(mol%orbital_energies)) &
         call create_copy_(self%orbital_energies,mol%orbital_energies)
      if (associated(mol%molecular_orbitals)) &
         call create_copy_(self%molecular_orbitals,mol%molecular_orbitals)
      if (associated(mol%density_matrix)) &
         call create_copy_(self%density_matrix,mol%density_matrix)
      if (associated(mol%natural_orbitals)) &
         call create_copy_(self%natural_orbitals,mol%natural_orbitals)
      if (associated(mol%occupation_numbers)) &
         call create_copy_(self%occupation_numbers,mol%occupation_numbers)
      if (associated(mol%fock_matrix)) &
         call create_copy_(self%fock_matrix,mol%fock_matrix)
      if (associated(mol%old_molecular_orbitals)) &
         call create_copy_(self%old_molecular_orbitals,mol%old_molecular_orbitals)
      if (associated(mol%old_density_matrix)) &
         call create_copy_(self%old_density_matrix,mol%old_density_matrix)
      if (associated(mol%old_fock_matrix)) &
         call create_copy_(self%old_fock_matrix,mol%old_fock_matrix)
      if (associated(mol%constraint_matrix)) &
         call create_copy_(self%constraint_matrix,mol%constraint_matrix)
      if (associated(mol%old_constraint_matrix)) &
         call create_copy_(self%old_constraint_matrix,mol%old_constraint_matrix)
      if (associated(mol%atom_group)) &
         call create_copy_(self%atom_group,mol%atom_group)
      if (associated(mol%group_charges)) &
         call create_copy_(self%group_charges,mol%group_charges)

   end subroutine

   subroutine set_defaults(self)
    type(mol_type) :: self
    ! Set up a default molecule

      self%name = "unknown"
      self%E_field = 0.0d0
      self%B_field = 0.0d0
      self%minimal_io = .true.
      self%gauge_origin = 0.0d0
      self%charge = 0
      self%mult = 1
      self%optimise_thermals = .false.
      self%n_bf = 0
      self%basis_set_kind = " "
      self%basis_info_made = .false.
      self%CIF_file_name = " "
      self%CIF_data_block_name = " "
      self%CX_file_name = " "
      if (associated(self%atom)) then
        self%mult = default_multiplicity_(self)
        call set_atom_info_(self)
      end if

   end subroutine

   subroutine set_atom_info(self)
    type(mol_type) :: self
    ! Set the associated atom information, including the number of alpha and beta
    ! electrons. NOTE: The multiplicity must be right for this to work.  The
    ! routine tries to reassign the multiplicity so that it will be consistent
    ! with the charge.

      call ensure_(tonto,associated(self%atom),"MOL:set_atom_info ... no atom info")
      self%n_atom = size(self%atom)
      if (.not. has_valid_no_of_beta_electrons_(self)) then
         call warn_(tonto,"MOL:set_atom_info ... Incosistent charge and multiplicity")
         call warn_(tonto,"MOL:set_atom_info ... Reassigning the multiplicity")
         self%mult = default_multiplicity_(self)
      end if
      self%n_e = no_of_electrons_(self)
      self%n_a = no_of_alpha_electrons_(self)
      self%n_b = no_of_beta_electrons_(self)
      call destroy_(self%atom_kind)
      call create_(self%atom_kind,self%n_atom)
      call make_atom_kind_list_(self%atom,self%atom_kind,self%n_atom_kind)
      call destroy_(self%unique_atom)
      call create_(self%unique_atom,self%n_atom_kind)
      call make_unique_atom_list_(self%atom,self%unique_atom)
      if (groups_defined_(self%atom)) call make_atom_groups_(self)

   end subroutine

   subroutine resolve_basis_info(self)
    type(mol_type) :: self
    ! Match the basis set labels for every atom with the actual atomic basis set.
    ! Also perform any finalization procedures to fully define all objects.

      if (.not. associated(self%atom)) then;   return; end if
      if (self%n_atom <1) then;   return; end if
      if    (same_as_(self%basis_set_kind,"Thakkar",ignore_case=.true.)) then
         call resolve_slaterbasis_info_(self)
      else if (same_as_(self%basis_set_kind,"Coppens",ignore_case=.true.)) then
         call resolve_coppensbasis_info_(self)
      else
         call resolve_gaussianbasis_info_(self)
      end if

   end subroutine

!   resolve_gaussianbasis_info ::: leaky
!   ! Match the basis set labels for every atom with the actual atomic basis set.
!   ! Also perform any finalization procedures to fully define all objects.
!      labels :: STRVEC*
!      if (.atom.destroyed) return
!      .basis_info_made = .false.
!      ! Bases are fully or partially labelled ?
!      if (.not. .atom.bases_are_all_unlabeled) then
!         if (.basis.created) then ! first try to resolve using existing bases.
!            .atom.resolve_library_bases(.basis,clobber=.true.,resolve_all=.false.)
!            if (.atom.bases_are_resolved .and. .basis_set_kind/=" ") then
!               call warn_(tonto,"basis_set_kind= specified but ignored")
!            end
!         end
!         if (.not. .atom.bases_are_resolved  .and. .basis_set_kind/=" ") then
!            labels => .atom.library_basis_labels(.basis_set_kind,missing=.true.)
!            .basis.read_library_data(.basis_set_library,labels) ! stdin is redirected
!            .atom.resolve_basis_suffix(.basis,.basis_set_kind,clobber=.false.,resolve_all=.true.)
!            labels.destroy
!         end
!      ! Bases unlabelled. Generic basis_set_kind= specifier used ?
!      else if (.basis_set_kind/=" ") then
!         call ensure_(tonto,.basis.destroyed,"all bases are unlabeled, can't use basis_set_kind=")
!         labels => .atom.library_basis_labels(.basis_set_kind)
!         .basis.read_library_data(.basis_set_library,labels) ! stdin is redirected
!         .atom.resolve_basis_suffix(.basis,.basis_set_kind,clobber=.true.,resolve_all=.true.)
!         labels.destroy
!      end
!      if (.atom.bases_are_resolved) then
!         call ensure_(tonto,.basis.created,"list of bases does not exists")
!         .basis.unnormalise ! NOTE this
!         .set_basis_info
!         stdout.text(" ")
!         stdout.text("Bases all resolved")
!      end
!   end

   subroutine resolve_gaussianbasis_info(self)
    type(mol_type) :: self
    ! For a Coppens basis set, match the basis set labels for every atom with the
    ! actual fitted atomic basis set.
      character(128), dimension(:), pointer :: library_basis_labels
      integer(kind=kind(1)) :: n_unique

      if (associated(self%basis)) call renormalise_(self%basis)
       ! First, add all library bases; this may scramble atom.basis ptr's
      if (self%basis_set_kind/=" ") then
         library_basis_labels => library_basis_labels_(self%atom,self%basis_set_kind)
         call read_library_data_(self%basis,basis_set_library_(self),library_basis_labels,n_unique)
         call text_(stdout,"Added "//trim(to_str_(n_unique))//" bases from library: "//trim(self%basis_set_kind))
         call destroy_(library_basis_labels)
      end if
       ! Now try to resolve atom bases afresh
      if (associated(self%basis)) then
         call resolve_bases_(self%atom,self%basis,self%basis_set_kind)
         if (bases_are_resolved_(self%atom)) call text_(stdout,"Bases all resolved")
      end if
       ! Unnormalise if completely done
      if (associated(self%basis)) then
         call unnormalise_(self%basis)  ! NOTE this
         call set_basis_info_(self)
      end if

   end subroutine

!   resolve_slaterbasis_info ::: leaky
!   ! For a Slater basis set, match the basis set labels for every atom with the
!   ! actual fitted atomic basis set.
!      labels :: STRVEC*
!      call ensure_(tonto,.basis_set_kind.same_as("Slater",ignore_case=.true.),"not a Slater basis")
!      ! Bases are fully or partially labelled ?
!      if (.not. .atom.slaterbases_are_all_unlabeled) then
!         if (.slaterbasis.created) then ! first try to resolve using existing bases.
!            .atom.resolve_library_bases(.slaterbasis,clobber=.true.,resolve_all=.false.)
!            if (.atom.slaterbases_are_resolved .and. .basis_set_kind/=" ") then
!               call warn_(tonto,"basis_set_kind= specified but ignored")
!            end
!         end
!         if (.not. .atom.slaterbases_are_resolved  .and. .basis_set_kind/=" ") then
!            labels => .atom.library_basis_labels(.basis_set_kind,missing=.true.)
!            .slaterbasis.read_library_data(.basis_set_library,labels) ! stdin is redirected
!            .atom.resolve_basis_suffix(.slaterbasis,.basis_set_kind,clobber=.false.,resolve_all=.true.)
!            labels.destroy
!         end
!      ! Bases unlabelled. Generic basis_set_kind= specifier used ?
!      else if (.basis_set_kind/=" ") then
!      call ensure_(tonto,.slaterbasis.destroyed,"all bases are unlabeled, can't use basis_set_kind=")
!         labels => .atom.library_basis_labels(.basis_set_kind)
!         .slaterbasis.read_library_data(.basis_set_library,labels) ! stdin is redirected
!         .atom.resolve_basis_suffix(.slaterbasis,.basis_set_kind,clobber=.true.,resolve_all=.true.)
!         labels.destroy
!      end
!      if (.atom.slaterbases_are_resolved) then
!         stdout.text(" ")
!         stdout.text("Slater bases all resolved")
!      end
!   end

   subroutine resolve_slaterbasis_info(self)
    type(mol_type) :: self
    ! For a Coppens basis set, match the basis set labels for every atom with the
    ! actual fitted atomic basis set.
      character(128), dimension(:), pointer :: library_basis_labels
      integer(kind=kind(1)) :: n_unique

      call ensure_(tonto,same_as_(self%basis_set_kind,"Thakkar",ignore_case=.true.),"MOL:resolve_slaterbasis_info ... not a S&
&later basis")
       ! First, add all library bases; this may scramble atom.basis ptr's
      if (self%basis_set_kind/=" ") then
         library_basis_labels => library_basis_labels_(self%atom,self%basis_set_kind)
         call read_library_data_(self%slaterbasis,basis_set_library_(self),library_basis_labels,n_unique)
         call text_(stdout,"Added "//trim(to_str_(n_unique))//" bases from library: "//trim(self%basis_set_kind))
         call destroy_(library_basis_labels)
      end if
       ! Now try to resolve atom bases afresh
      if (associated(self%slaterbasis)) then
         call resolve_bases_(self%atom,self%slaterbasis,self%basis_set_kind)
         if (slaterbases_are_resolved_(self%atom)) call text_(stdout,"Slater bases all resolved")
      end if
       ! .set_basis_info

   end subroutine

   subroutine resolve_coppensbasis_info(self)
    type(mol_type) :: self
    ! For a Coppens basis set, match the basis set labels for every atom with the
    ! actual fitted atomic basis set.
      character(128), dimension(:), pointer :: library_basis_labels
      integer(kind=kind(1)) :: n_unique

      call ensure_(tonto,same_as_(self%basis_set_kind,"Coppens",ignore_case=.true.),"MOL:resolve_coppensbasis_info ... not a &
&Coppens basis")
      if (associated(self%coppensbasis)) call renormalise_(self%coppensbasis)
       ! First, add all library bases; this may scramble atom.basis ptr's
      if (self%basis_set_kind/=" ") then
         library_basis_labels => library_basis_labels_(self%atom,self%basis_set_kind)
         call read_library_data_(self%coppensbasis,basis_set_library_(self),library_basis_labels,n_unique)
         call text_(stdout,"Added "//trim(to_str_(n_unique))//" bases from library: "//trim(self%basis_set_kind))
         call destroy_(library_basis_labels)
      end if
       ! Now try to resolve atom bases afresh
      if (associated(self%coppensbasis)) then
         call resolve_bases_(self%atom,self%coppensbasis,self%basis_set_kind)
         if (coppensbases_are_resolved_(self%atom)) call text_(stdout,"Coppens bases all resolved")
      end if
       ! Unnormalise if completely done
      if (associated(self%coppensbasis)) call unnormalise_(self%coppensbasis)  ! NOTE this
       ! .set_basis_info

   end subroutine

   function basis_set_library(self) result(res)
    type(mol_type) :: self
    ! Return a string giving the basis set library
      character(128) :: res
      type(basis_type), dimension(:), pointer :: basis

      call ensure_(tonto,self%basis_set_kind/=" ","MOL:basis_set_library ... no basis_set_kind specified")
      res = library_directory_(basis,self%basis_set_kind)

   end function

   subroutine resolve_axis_system(self)
    type(mol_type) :: self
    ! Change the axis system to crystal coordinates, if required.

      if (.not. associated(self%crystal)) then;   return; end if
      if (.not. associated(self%atom)) then;   return; end if
      call resolve_axis_system_(self%atom,self%crystal)

   end subroutine

   subroutine set_basis_info(self)
    type(mol_type) :: self
    ! Set the associated basis set information

      self%basis_info_made = .true.
      self%n_basis = size(self%basis)
      self%n_bf = n_bf_(self%atom)
      self%n_prim = n_prim_(self%atom)
      self%n_shell = n_shell_(self%atom)
      self%n_shell_pairs = no_of_shell_pairs_(self)
      call destroy_(self%molecular_orbitals)
      call create_(self%molecular_orbitals,self%n_bf)
      call destroy_(self%orbital_energies)
      call create_(self%orbital_energies,self%n_bf)
      call destroy_(self%density_matrix)
      call create_(self%density_matrix,self%n_bf)
      call destroy_(self%natural_orbitals)
      call create_(self%natural_orbitals,self%n_bf)
      call destroy_(self%occupation_numbers)
      call create_(self%occupation_numbers,self%n_bf)
      call destroy_(self%fock_matrix)
      call create_(self%fock_matrix,self%n_bf)
      call make_shell_info_(self)

   end subroutine

   subroutine assign_natural_orbitals(self)
    type(mol_type) :: self
    ! Assign the natural orbitals to be the molecular_orbitals
      character(128) :: NO_kind

      call ensure_(tonto,associated(self%molecular_orbitals),"MOL:assign_natural_orbitals ... no molecular orbitals")
      call ensure_(tonto,any_created_(self%molecular_orbitals),"MOL:assign_natural_orbitals ... no molecular orbitals")
      call destroy_(self%natural_orbitals)
      call create_copy_(self%natural_orbitals,self%molecular_orbitals)
      NO_kind = spinorbital_kind_(self%natural_orbitals)
      call destroy_(self%occupation_numbers)
      call create_(self%occupation_numbers,self%n_bf,NO_kind)
      call zero_(self%occupation_numbers)
      call set_scf_occupations_(self,NO_kind)

   end subroutine

   subroutine assign_MOs_to_NOs(self)
    type(mol_type) :: self
    ! Assign the MOs to be the same as the natural orbitals
      character(128) :: NO_kind

      call destroy_(self%molecular_orbitals)
      call create_copy_(self%molecular_orbitals,self%natural_orbitals)
      NO_kind = spinorbital_kind_(self%natural_orbitals)
      call create_(self%occupation_numbers,NO_kind)
      call zero_(self%occupation_numbers)
      call set_scf_occupations_(self,NO_kind)

   end subroutine

   subroutine set_scf_occupations(self,NO_kind)
    type(mol_type) :: self
    ! Set the SCF occupation numbers for the natural orbitals
      character(128) :: NO_kind

      select case (NO_kind)
         case ("restricted")
            self%occupation_numbers%restricted(1:self%n_a) = 2.0d0
         case ("unrestricted")
            self%occupation_numbers%alpha(1:self%n_a) = 1.0d0
            self%occupation_numbers%beta(1:self%n_b) = 1.0d0
         case ("general")
            self%occupation_numbers%general(1:self%n_e) = 1.0d0
         case ("restricted_complex")
            self%occupation_numbers%restricted(1:self%n_a) = 2.0d0
         case ("unrestricted_complex")
            self%occupation_numbers%alpha(1:self%n_a) = 1.0d0
            self%occupation_numbers%beta(1:self%n_b) = 1.0d0
         case ("general_complex")
            self%occupation_numbers%general(1:self%n_e) = 1.0d0
      end select

   end subroutine

!  ***************************
!  ATOMVEC information methods
!  ***************************

   function default_multiplicity(self) result(res)
    type(mol_type) :: self
    ! Return the default multiplicity for a molecule.
     intent(in) :: self
     real(kind=kind(1.0d0)) :: res
     integer(kind=kind(1)) :: n_e

     call ensure_(tonto,associated(self%atom),"MOL:default_multiplicity ... no atom info")
     n_e = no_of_electrons_(self)
     if (size(self%atom)==1) then
        res = ground_state_multiplicity_(self%atom(1),n_e)
     else
        res = mod(n_e,2) + 1
     end if

   end function

   function nuclear_energy(self) result(res)
    type(mol_type) :: self
    ! Return the nuclear repulsion energy
      real(kind=kind(1.0d0)) :: res

      call ensure_(tonto,associated(self%atom),"MOL:nuclear_energy ... atom list required")
      res = nuclear_energy_(self%atom)

   end function

   function chemical_formula(self) result(res)
    type(mol_type) :: self
    ! Return the chemical formula for the molecule, as a string, in alphabetical
    ! order of elements
      character(128) :: res

      call ensure_(tonto,associated(self%atom),"MOL:chemical_formula ... atom list required")
      res = chemical_formula_(self%atom)

   end function

   function centre_of_mass(self) result(centre)
    type(mol_type) :: self
    ! Return the centre of mass
      real(kind=kind(1.0d0)), dimension(3) :: centre

      call ensure_(tonto,associated(self%atom),"MOL:centre_of_mass ... atom list required")
      centre = centre_of_mass_(self%atom)

   end function

   subroutine move_origin_to_centre_of_mass(self)
    type(mol_type) :: self
    ! Move the origin to the centre of mass
      real(kind=kind(1.0d0)), dimension(3) :: com

      call ensure_(tonto,associated(self%atom),"MOL:move_origin_to_centre_of_mass ... atom list required")
      com = centre_of_mass_(self%atom)
      call translate_(self%atom,-com)

   end subroutine

   function molecular_weight(self) result(res)
    type(mol_type) :: self
    ! Return the molceular weight
      real(kind=kind(1.0d0)) :: res

      call ensure_(tonto,associated(self%atom),"MOL:molecular_weight ... atom list required")
      res = molecular_weight_(self%atom)

   end function

   function reduced_mass(self) result(res)
    type(mol_type) :: self
    ! Return the reduced mass
      real(kind=kind(1.0d0)) :: res

      call ensure_(tonto,associated(self%atom),"MOL:reduced_mass ... atom list required")
      res = reduced_mass_(self%atom)

   end function

   function centre_of_atoms(self) result(centre)
    type(mol_type) :: self
    ! Return the centroid of the atom positions
      real(kind=kind(1.0d0)), dimension(3) :: centre

      call ensure_(tonto,associated(self%atom),"MOL:centre_of_atoms ... atom list required")
      centre = centre_of_atoms_(self%atom)

   end function

   function atom_index_from_pos(self,pos) result(res)
    type(mol_type) :: self
    ! Return the index of atom from its position "pos"
      real(kind=kind(1.0d0)), dimension(3) :: pos
      integer(kind=kind(1)) :: res

      call ensure_(tonto,associated(self%atom),"MOL:atom_index_from_pos ... atom list required")
      res = atom_index_from_pos_(self%atom,pos)

   end function

   subroutine get_geometry(self,g)
    type(mol_type) :: self
    ! Return the geometry "g" in a matrix
       real(kind=kind(1.0d0)), dimension(:,:) :: g

      call ensure_(tonto,associated(self%atom),"MOL:get_geometry ... no atom list")
      call get_geometry_(self%atom,g)

   end subroutine

   subroutine get_crystal_geometry(self,g)
    type(mol_type) :: self
    ! Return the crystal (fractional coordinate) geometry "g" in a matrix
       real(kind=kind(1.0d0)), dimension(:,:) :: g
       integer(kind=kind(1)) :: n

      call get_geometry_(self,g)
      if (size(g,1)==3) then
         do n = 1,self%n_atom
            call rotate_(self%crystal%unitcell%inverse_matrix,g(:,n))
         end do
      else if (size(g,2)==3) then
         do n = 1,self%n_atom
            call rotate_(self%crystal%unitcell%inverse_matrix,g(n,:))
         end do
      end if

   end subroutine

   subroutine get_atom_pair_indices(self,index,a,b)
    type(mol_type) :: self
    ! Return the atom indicies "a" and "b" which map to an atom-pair "index"
     integer(kind=kind(1)), intent(in) :: index
     integer(kind=kind(1)), intent(out) :: a,b

     a  = (1+int(sqrt(8.0d0*index-7.0d0)))/2
     b  = index - a*(a-1)/2

   end subroutine

   subroutine get_atom_pair_indices_1(self,index,a,b,fa,la,fb,lb)
    type(mol_type) :: self
    ! Return the atom indicies "a" and "b" which map to an atom-pair "index".
    ! Also get the first and last basis functions for the atoms.
     integer(kind=kind(1)), intent(in) :: index
     integer(kind=kind(1)), intent(out) :: a,b,fa,la,fb,lb

     a  = (1+int(sqrt(8.0d0*index-7.0d0)))/2
     b  = index - a*(a-1)/2
     fa = self%first_basis_fn_for_atom(a)
     la =  self%last_basis_fn_for_atom(a)
     fb = self%first_basis_fn_for_atom(b)
     lb =  self%last_basis_fn_for_atom(b)

   end subroutine

!  ********************************
!  ATOMVEC shell extraction methods
!  ********************************

   subroutine get_shell(self,shell,index)
    type(mol_type) :: self
    ! Get basis set "shell" corresponding to "index"
      integer(kind=kind(1)), intent(in) :: index
      type(shell_type), intent(out) :: shell
      integer(kind=kind(1)) :: aa,sa

      aa = self%atom_for_shell(index)
      sa = self%atom_shell_for_shell(index)
      call copy_(shell,self%atom(aa)%basis%shell(sa) )

   end subroutine

   subroutine get_shell_1(self,shell,index)
    type(mol_type) :: self
    ! Update the shell1 "shell".
      integer(kind=kind(1)), intent(in) :: index
      type(shell1_type), intent(out) :: shell
      integer(kind=kind(1)) :: aa,sa

      aa = self%atom_for_shell(index)
      sa = self%atom_shell_for_shell(index)
      call copy_(shell,shell=self%atom(aa)%basis%shell(sa),pos=self%atom(aa)%pos)

   end subroutine

   subroutine get_shell_2(self,shell,index,fa,la)
    type(mol_type) :: self
    ! Update the shell1 "shell".
      integer(kind=kind(1)), intent(in) :: index
      type(shell1_type), intent(out) :: shell
      integer(kind=kind(1)), intent(out) :: fa,la
      integer(kind=kind(1)) :: aa,sa

      aa = self%atom_for_shell(index)
      sa = self%atom_shell_for_shell(index)
      fa = self%first_basis_fn_for_shell(index)
      la = self%last_basis_fn_for_shell(index)
      call copy_(shell,shell=self%atom(aa)%basis%shell(sa),pos=self%atom(aa)%pos)

   end subroutine

!  ******************
!  Shell pair indices
!  ******************

   subroutine get_shell_pair_indices(self,index,a,b)
    type(mol_type) :: self
    ! Return the actual shell indicies "a" and "b" which map to "index"
     integer(kind=kind(1)), intent(in) :: index
     integer(kind=kind(1)), intent(out) :: a,b

     a  = (1+int(sqrt(8.0d0*index-7.0d0)))/2
     b  = index - a*(a-1)/2

   end subroutine

   subroutine get_shell_pair_indices_1(self,index,a,b,fa,la,fb,lb)
    type(mol_type) :: self
    ! Return the shell indicies "a" and "b" which map to "index"
    ! Also return the basis function start indices "fa", "la", etc ...
      integer(kind=kind(1)), intent(in) :: index
      integer(kind=kind(1)), intent(out) :: a,b,fa,la,fb,lb

      a  = (1+int(sqrt(8.0d0*index-7.0d0)))/2
      b  = index - a*(a-1)/2
      fa = self%first_basis_fn_for_shell(a)
      fb = self%first_basis_fn_for_shell(b)
      la = self%last_basis_fn_for_shell(a)
      lb = self%last_basis_fn_for_shell(b)

   end subroutine

   subroutine get_shell_pair_indices_2(self,index,a,b,fa,la,fb,lb,atom_a,atom_b)
    type(mol_type) :: self
    ! Return the shell indicies "a" and "b" which map to "index"
    ! Also return the basis function start indices "fa", "la", etc ...
    ! Also return the atom indices "atom_a" and "atom_b" for each shell.
      integer(kind=kind(1)), intent(in) :: index
      integer(kind=kind(1)), intent(out) :: a,b,fa,la,fb,lb,atom_a,atom_b

      a  = (1+int(sqrt(8.0d0*index-7.0d0)))/2
      b  = index - a*(a-1)/2
      fa = self%first_basis_fn_for_shell(a)
      fb = self%first_basis_fn_for_shell(b)
      la = self%last_basis_fn_for_shell(a)
      lb = self%last_basis_fn_for_shell(b)
      atom_a = self%atom_for_shell(a)
      atom_b = self%atom_for_shell(b)

   end subroutine

!  ***********
!  Shell pairs
!  ***********

   subroutine get_shell_pair(self,shell,index,fa,la,fb,lb)
    type(mol_type) :: self
    ! Get the type(shell2_type) object "shell" correponding to the pair index "index"
    ! Also return the basis function start indices "fa", "la", etc ...
      integer(kind=kind(1)), intent(in) :: index
      type(shell2_type), intent(out) :: shell
      integer(kind=kind(1)), intent(out) :: fa,la,fb,lb
      integer(kind=kind(1)) :: a,b,aa,sa,bb,sb

      a  = (1+int(sqrt(8.0d0*index-7.0d0)))/2
      b  = index - a*(a-1)/2
      fa = self%first_basis_fn_for_shell(a)
      fb = self%first_basis_fn_for_shell(b)
      la = self%last_basis_fn_for_shell(a)
      lb = self%last_basis_fn_for_shell(b)
      aa = self%atom_for_shell(a)
      bb = self%atom_for_shell(b)
      sa = self%atom_shell_for_shell(a)
      sb = self%atom_shell_for_shell(b)
      call copy_(shell,self%atom(aa)%basis%shell(sa), self%atom(bb)%basis%shell(sb), &
                 self%atom(aa)%pos, self%atom(bb)%pos )

   end subroutine

   subroutine get_shell_pair_1(self,shell,index,fa,la,fb,lb,atom_a,atom_b)
    type(mol_type) :: self
    ! Get the type(shell2_type) object "shell" correponding to the pair index "index"
    ! Also return the basis function start indices "fa", "la", etc ...
    ! Plus the atoms the shells are located on, "atom_a" and "atom_b".
     integer(kind=kind(1)), intent(in) :: index
     type(shell2_type), intent(out) :: shell
     integer(kind=kind(1)), intent(out) :: fa,la,fb,lb,atom_a,atom_b
     integer(kind=kind(1)) :: a,b,sa,sb

     a  = (1+int(sqrt(8.0d0*index-7.0d0)))/2
     b  = index - a*(a-1)/2
     fa = self%first_basis_fn_for_shell(a)
     fb = self%first_basis_fn_for_shell(b)
     la = self%last_basis_fn_for_shell(a)
     lb = self%last_basis_fn_for_shell(b)
     atom_a = self%atom_for_shell(a)
     atom_b = self%atom_for_shell(b)
     sa = self%atom_shell_for_shell(a)
     sb = self%atom_shell_for_shell(b)
     call copy_(shell,self%atom(atom_a)%basis%shell(sa), self%atom(atom_b)%basis%shell(sb), &
                self%atom(atom_a)%pos, self%atom(atom_b)%pos )

   end subroutine

   subroutine get_precomp_shell_pair(self,shell,index,fa,la,fb,lb)
    type(mol_type) :: self
    ! Get the type(shell2_type) object "shell" correponding to the pair index "index"
    ! Also return the basis function start indices "fa", "la", etc ...
      integer(kind=kind(1)), intent(in) :: index
      type(shell2_type), intent(out) :: shell
      integer(kind=kind(1)), intent(out) :: fa,la,fb,lb
      integer(kind=kind(1)) :: a,b,aa,sa,bb,sb

      a  = (1+int(sqrt(8.0d0*index-7.0d0)))/2
      b  = index - a*(a-1)/2
      fa = self%first_basis_fn_for_shell(a)
      fb = self%first_basis_fn_for_shell(b)
      la = self%last_basis_fn_for_shell(a)
      lb = self%last_basis_fn_for_shell(b)
      aa = self%atom_for_shell(a)
      bb = self%atom_for_shell(b)
      sa = self%basis_shell_for_shell(a)
      sb = self%basis_shell_for_shell(b)
      call copy_(shell,self%precomputed_basis_shellpair((sa-1)*self%n_unique_shells+sb), &
                 self%atom(aa)%pos,self%atom(bb)%pos)

   end subroutine

!  *********************
!  Shell quartet indices
!  *********************

   subroutine get_shell_quartet_indices(self,index,a,b,c,d)
    type(mol_type) :: self
    ! Get the indexes a,b,c,d of the quartet from the "index".
    ! Note that "index" will die at 361 shells for integer(4).
     integer(kind=kind(1)), intent(in) :: index
     integer(kind=kind(1)), intent(out) :: a,b,c,d
     integer(kind=kind(1)) :: ab,cd

     ab = (1+sqrt(8.0d0*index-7.0d0))/2
     cd = index - ab*(ab-1)/2
     a  = (1+sqrt(8.0d0*ab-7.0d0))/2
     b  = ab - a*(a-1)/2
     c  = (1+sqrt(8.0d0*cd-7.0d0))/2
     d  = cd - c*(c-1)/2

   end subroutine

   subroutine get_shell_quartet_indices_1(self,index,fa,la,fb,lb,fc,lc,fd,ld)
    type(mol_type) :: self
    ! For the quartet index "index" return the shell indicies "a", "b",
    ! "c" and "d" which map to "index". Also return the basis function
    ! start indices "fa", "la", etc ...
      integer(kind=kind(1)), intent(in) :: index
      integer(kind=kind(1)), intent(out) :: fa,la,fb,lb,fc,lc,fd,ld
      integer(kind=kind(1)) :: a,b,c,d
      integer(kind=kind(1)) :: ab,cd

      ab = (1+sqrt(8.0d0*index-7.0d0))/2
      cd = index - ab*(ab-1)/2
      a  = (1+sqrt(8.0d0*ab-7.0d0))/2
      b  = ab - a*(a-1)/2
      c  = (1+sqrt(8.0d0*cd-7.0d0))/2
      d  = cd - c*(c-1)/2
      fa = self%first_basis_fn_for_shell(a)
      fb = self%first_basis_fn_for_shell(b)
      fc = self%first_basis_fn_for_shell(c)
      fd = self%first_basis_fn_for_shell(d)
      la = self%last_basis_fn_for_shell(a)
      lb = self%last_basis_fn_for_shell(b)
      lc = self%last_basis_fn_for_shell(c)
      ld = self%last_basis_fn_for_shell(d)

   end subroutine

   subroutine get_shell_quartet_indexes(self,index,a,b,c,d,atom_a,atom_b,atom_c,atom_d)
    type(mol_type) :: self
    ! For the quartet index "index" return the shell indicies "a", "b", "c" and
    ! "d" which map to "index" and the corresponding atom centers. Note different
    ! spelling for this routine and the one above to avoid overload problems.
      integer(kind=kind(1)), intent(in) :: index
      integer(kind=kind(1)), intent(out) :: a,b,c,d,atom_a,atom_b,atom_c,atom_d
      integer(kind=kind(1)) :: ab,cd

      ab = (1+sqrt(8.0d0*index-7.0d0))/2
      cd = index - ab*(ab-1)/2
      a  = (1+sqrt(8.0d0*ab-7.0d0))/2
      b  = ab - a*(a-1)/2
      c  = (1+sqrt(8.0d0*cd-7.0d0))/2
      d  = cd - c*(c-1)/2
      atom_a = self%atom_for_shell(a)
      atom_b = self%atom_for_shell(b)
      atom_c = self%atom_for_shell(c)
      atom_d = self%atom_for_shell(d)

   end subroutine

!  **************
!  Shell quartets
!  **************

   subroutine get_shell_quartet(self,shell,a,b,c,d)
    type(mol_type) :: self
    ! Get the type(shell4_type) object "shell" correponding to the indicies "a", "b", "c"
    ! and "d".
      type(shell4_type), intent(out) :: shell
      integer(kind=kind(1)), intent(in) :: a,b,c,d
      integer(kind=kind(1)) :: aa,sa,bb,sb,cc,sc,dd,sd

      aa = self%atom_for_shell(a)
      sa = self%atom_shell_for_shell(a)
      bb = self%atom_for_shell(b)
      sb = self%atom_shell_for_shell(b)
      cc = self%atom_for_shell(c)
      sc = self%atom_shell_for_shell(c)
      dd = self%atom_for_shell(d)
      sd = self%atom_shell_for_shell(d)
      call copy_(shell,self%atom(aa)%basis%shell(sa), self%atom(bb)%basis%shell(sb), &
                 self%atom(cc)%basis%shell(sc), self%atom(dd)%basis%shell(sd), &
                 self%atom(aa)%pos, self%atom(bb)%pos, self%atom(cc)%pos, self%atom(dd)%pos )

   end subroutine

   subroutine get_shell_quartet_1(self,shell,index,a,b,c,d)
    type(mol_type) :: self
    ! Get the type(shell4_type) object "shell" correponding to the quartet index "index"
    ! Also return the shell indicies "a", "b", "c" and "d" which map to "index"
      integer(kind=kind(1)), intent(in) :: index
      type(shell4_type), intent(out) :: shell
      integer(kind=kind(1)), intent(out) :: a,b,c,d
      integer(kind=kind(1)) :: ab,cd,aa,sa,bb,sb,cc,sc,dd,sd

      ab = (1+sqrt(8.0d0*index-7.0d0))/2
      cd = index - ab*(ab-1)/2
      a  = (1+sqrt(8.0d0*ab-7.0d0))/2
      b  = ab - a*(a-1)/2
      c  = (1+sqrt(8.0d0*cd-7.0d0))/2
      d  = cd - c*(c-1)/2
      aa = self%atom_for_shell(a)
      sa = self%atom_shell_for_shell(a)
      bb = self%atom_for_shell(b)
      sb = self%atom_shell_for_shell(b)
      cc = self%atom_for_shell(c)
      sc = self%atom_shell_for_shell(c)
      dd = self%atom_for_shell(d)
      sd = self%atom_shell_for_shell(d)
      call copy_(shell,self%atom(aa)%basis%shell(sa), self%atom(bb)%basis%shell(sb), &
                  self%atom(cc)%basis%shell(sc), self%atom(dd)%basis%shell(sd), &
                  self%atom(aa)%pos, self%atom(bb)%pos, self%atom(cc)%pos, self%atom(dd)%pos )

   end subroutine

   subroutine get_shell_quartet_2(self,shell,index,a,b,c,d,fa,la,fb,lb,fc,lc,fd,ld)
    type(mol_type) :: self
    ! Get the type(shell4_type) object "shell" correponding to the quartet index "index"
    ! Also return the shell indicies "a", "b", "c" and "d" which map to "index"
    ! Also return the basis function start indices "fa", "la", etc ...
      integer(kind=kind(1)), intent(in) :: index
      type(shell4_type), intent(out) :: shell
      integer(kind=kind(1)), intent(out) :: a,b,c,d,fa,la,fb,lb,fc,lc,fd,ld
      integer(kind=kind(1)) :: ab,cd,aa,sa,bb,sb,cc,sc,dd,sd

      ab = (1+sqrt(8.0d0*index-7.0d0))/2
      cd = index - ab*(ab-1)/2
      a  = (1+sqrt(8.0d0*ab-7.0d0))/2
      b  = ab - a*(a-1)/2
      c  = (1+sqrt(8.0d0*cd-7.0d0))/2
      d  = cd - c*(c-1)/2
      aa = self%atom_for_shell(a)
      sa = self%atom_shell_for_shell(a)
      bb = self%atom_for_shell(b)
      sb = self%atom_shell_for_shell(b)
      cc = self%atom_for_shell(c)
      sc = self%atom_shell_for_shell(c)
      dd = self%atom_for_shell(d)
      sd = self%atom_shell_for_shell(d)
      call copy_(shell,self%atom(aa)%basis%shell(sa), self%atom(bb)%basis%shell(sb), &
                  self%atom(cc)%basis%shell(sc), self%atom(dd)%basis%shell(sd), &
                  self%atom(aa)%pos, self%atom(bb)%pos, self%atom(cc)%pos, self%atom(dd)%pos )
      fa = self%first_basis_fn_for_shell(a)
      fb = self%first_basis_fn_for_shell(b)
      fc = self%first_basis_fn_for_shell(c)
      fd = self%first_basis_fn_for_shell(d)
      la = self%last_basis_fn_for_shell(a)
      lb = self%last_basis_fn_for_shell(b)
      lc = self%last_basis_fn_for_shell(c)
      ld = self%last_basis_fn_for_shell(d)

   end subroutine

   subroutine get_shell_quartet_3(self,shell,index,a,b,c,d,atom_a,atom_b,atom_c,atom_d)
    type(mol_type) :: self
    ! Get the type(shell4_type) object "shell" correponding to the quartet index "index"
    ! Also return the shell indicies "a", "b", "c" and "d" which map to "index"
    ! Plus the atoms which the shells are on.
      integer(kind=kind(1)), intent(in) :: index
      type(shell4_type), intent(out) :: shell
      integer(kind=kind(1)), intent(out) :: a,b,c,d,atom_a,atom_b,atom_c,atom_d
      integer(kind=kind(1)) :: ab,cd,sa,sb,sc,sd

      ab = (1+sqrt(8.0d0*index-7.0d0))/2
      cd = index - ab*(ab-1)/2
      a  = (1+sqrt(8.0d0*ab-7.0d0))/2
      b  = ab - a*(a-1)/2
      c  = (1+sqrt(8.0d0*cd-7.0d0))/2
      d  = cd - c*(c-1)/2
      atom_a = self%atom_for_shell(a)
      sa = self%atom_shell_for_shell(a)
      atom_b = self%atom_for_shell(b)
      sb = self%atom_shell_for_shell(b)
      atom_c = self%atom_for_shell(c)
      sc = self%atom_shell_for_shell(c)
      atom_d = self%atom_for_shell(d)
      sd = self%atom_shell_for_shell(d)
      call copy_(shell,self%atom(atom_a)%basis%shell(sa), self%atom(atom_b)%basis%shell(sb), &
                 self%atom(atom_c)%basis%shell(sc), self%atom(atom_d)%basis%shell(sd), &
                 self%atom(atom_a)%pos, self%atom(atom_b)%pos, &
                 self%atom(atom_c)%pos, self%atom(atom_d)%pos )

   end subroutine

   subroutine set_shell_quartet_ab(self,shell,a,b)
    type(mol_type) :: self
    ! Set the a and b parts of the "shell" type(shell4_type) object.
     type(shell4_type), intent(inout) :: shell
     integer(kind=kind(1)), intent(in) :: a,b
     integer(kind=kind(1)) :: aa,sa,bb,sb

     aa = self%atom_for_shell(a)
     sa = self%atom_shell_for_shell(a)
     bb = self%atom_for_shell(b)
     sb = self%atom_shell_for_shell(b)
     call copy_ab_(shell,self%atom(aa)%basis%shell(sa), self%atom(bb)%basis%shell(sb), &
                 self%atom(aa)%pos, self%atom(bb)%pos)

   end subroutine

   subroutine set_shell_quartet_cd(self,shell,c,d)
    type(mol_type) :: self
    ! Set the c and d parts of the "shell" type(shell4_type) object.
     type(shell4_type), intent(inout) :: shell
     integer(kind=kind(1)), intent(in) :: c,d
     integer(kind=kind(1)) :: cc,sc,dd,sd

     cc = self%atom_for_shell(c)
     sc = self%atom_shell_for_shell(c)
     dd = self%atom_for_shell(d)
     sd = self%atom_shell_for_shell(d)
     call copy_cd_(shell,self%atom(cc)%basis%shell(sc), self%atom(dd)%basis%shell(sd), &
                 self%atom(cc)%pos, self%atom(dd)%pos)

   end subroutine

   subroutine set_precomp_shell_quartet_ab(self,shell,a,b)
    type(mol_type) :: self
    ! Set the a and b parts of the "shell" type(shell4_type) object.
     type(shell1quartet_type), intent(inout) :: shell
     integer(kind=kind(1)), intent(in) :: a,b
     integer(kind=kind(1)) :: aa,sa,bb,sb

     aa = self%atom_for_shell(a)          !which atom.
     sa = self%basis_shell_for_shell(a)   !which shell.
     bb = self%atom_for_shell(b)
     sb = self%basis_shell_for_shell(b)
     call set_ab_(shell,self%precomputed_basis_shellpair((sa-1)*self%n_unique_shells+sb), &
                 self%atom(aa)%pos,self%atom(bb)%pos)

   end subroutine

   subroutine set_precomp_shell_quartet_cd(self,shell,c,d)
    type(mol_type) :: self
    ! Set the c and d parts of the "shell" type(shell4_type) object.
     type(shell1quartet_type), intent(inout) :: shell
     integer(kind=kind(1)), intent(in) :: c,d
     integer(kind=kind(1)) :: cc,sc,dd,sd

     cc = self%atom_for_shell(c)          !which atom.
     sc = self%basis_shell_for_shell(c)   !which shell.
     dd = self%atom_for_shell(d)
     sd = self%basis_shell_for_shell(d)
     call set_cd_(shell,self%precomputed_basis_shellpair((sc-1)*self%n_unique_shells+sd), &
                 self%atom(cc)%pos,self%atom(dd)%pos)

   end subroutine

!  ************************
!  Contraction Coefficients
!  ************************

   subroutine make_contraction_matrix(self,cc_mat)
    type(mol_type) :: self
    ! Returns the matrix of complete contraction coefficients for each basis set
    ! Size of cc_mat is [.n_prim,.n_bf].
      real(kind=kind(1.0d0)), dimension(:,:), intent(out) :: cc_mat
      integer(kind=kind(1)) :: a, bf_count, prim_count,a_prim,a_bf

   call ensure_(tonto,size(cc_mat,1)==self%n_prim,"MOL:make_contraction_matrix ... wrong shape, cc_mat")
   call ensure_(tonto,size(cc_mat,2)==self%n_bf,"MOL:make_contraction_matrix ... wrong shape, cc_mat")
      bf_count = 1
      prim_count = 1
      cc_mat = 0.0d0
      do a = 1, self%n_atom
         a_prim = self%atom(a)%basis%n_prim
         a_bf = self%atom(a)%basis%n_bf
         cc_mat(prim_count : prim_count + a_prim - 1, bf_count : bf_count + a_bf - 1) &
                  = contraction_matrix_(self%atom(a)%basis)
         prim_count = prim_count + a_prim
         bf_count = bf_count + a_bf
      end do

   end subroutine

!  **********************
!  Make shell information
!  **********************

   subroutine make_shell_info(self)
    type(mol_type) :: self
    ! Define a vector of atom numbers corresponding to the molecule
    ! basis set shell numbers; also define a vector of atom shell numbers
    ! corresponding to the molecule basis set shell number

     call ensure_(tonto,associated(self%atom),"MOL:make_shell_info ... no atom information")
     call ensure_(tonto,self%n_atom>0,"MOL:make_shell_info ... no atoms")
     call ensure_(tonto,associated(self%basis),"MOL:make_shell_info ... no basis set")
     call destroy_(self%atom_for_shell)
     self%atom_for_shell       => atom_for_shell_(self%atom)
     call destroy_(self%atom_shell_for_shell)
     self%atom_shell_for_shell => atom_shell_for_shell_(self%atom)
     call create_(self%first_shell_for_atom,self%n_atom)
     call create_(self%last_shell_for_atom,self%n_atom)
     call make_shell_for_atom_limits_(self%atom,self%first_shell_for_atom,self%last_shell_for_atom)
     call destroy_(self%first_basis_fn_for_shell)
     call destroy_(self%last_basis_fn_for_shell)
     call get_shell_limits_(self%atom,self%first_basis_fn_for_shell, self%last_basis_fn_for_shell)
     call destroy_(self%first_basis_fn_for_atom)
     call destroy_(self%last_basis_fn_for_atom)
     call make_atom_basis_fn_limits_(self%atom,self%first_basis_fn_for_atom,self%last_basis_fn_for_atom)
     call make_basis_shell_for_shell_(self)
     call make_shellpair_vector_(self)

   end subroutine

   subroutine make_shellpair_vector(self)
    type(mol_type) :: self
    ! Make a type(shellpair_type) vector "shellpair" which includes precomputed data for the
    ! basis set.
    ! NOTE for later: n_unique_shells should be a basisvec routine.
    ! NOTE for later: precomputed_basis_shellpair should be a REALMAT type
     integer(kind=kind(1)) :: i,j,k,a,b

     call ensure_(tonto,associated(self%basis),"MOL:make_shellpair_vector ... no basis set")
     i=0
     do k=1,size(self%basis)  ! Loop over bases
       i=i+self%basis(k)%n_shell
     end do
     self%n_unique_shells = i
     call destroy_(self%precomputed_basis_shellpair)
     call create_(self%precomputed_basis_shellpair,i*i)
     i=0
     do j=1,size(self%basis)  ! Loop over bases
       do a = 1, self%basis(j)%n_shell
         do k=1,size(self%basis)  ! Loop over bases
           do b = 1, self%basis(k)%n_shell
             i=i+1
             call copy_a_(self%precomputed_basis_shellpair(i),self%basis(j)%shell(a))
             call copy_b_(self%precomputed_basis_shellpair(i),self%basis(k)%shell(b))
             call make_precalculated_data_(self%precomputed_basis_shellpair(i))
           end do
         end do
       end do
     end do

   end subroutine

   function first_basis_shell_for_atom(self,atom) result(res)
    type(mol_type) :: self
    ! Return the index of the first shell in .basis (treating .basis as a
    ! flattened list of shells) which has the same basis label as "atom".
    ! NOTE for later: this should be a basisvec routine.
     integer(kind=kind(1)), intent(in) :: atom
     integer(kind=kind(1)) :: res
     integer(kind=kind(1)) :: i,j

     i=1
     do j=1,size(self%basis)  ! Loop over bases
       if (self%basis(j)%label==self%atom(atom)%basis%label) then
         res=i
         exit
       else
         i = i + self%basis(j)%n_shell
       end if
     end do

   end function

   subroutine make_basis_shell_for_shell(self)
    type(mol_type) :: self
    ! Return the index of the first shell in .basis (treating .basis as a
    ! flattened list of shells) for a given shell index in the molecular basis set
    ! (i.e. the flattened list of shells in the .atom list).
    ! NOTE for later: this should be a basisvec routine.
     integer(kind=kind(1)) :: sh,at,atom_shell,ind

     call ensure_(tonto,associated(self%basis),"MOL:make_basis_shell_for_shell ... no basis set")
     call destroy_(self%basis_shell_for_shell)
     call create_(self%basis_shell_for_shell,self%n_shell)
     do sh=1,self%n_shell
       at = self%atom_for_shell(sh)                  ! which atom
       atom_shell = self%atom_shell_for_shell(sh)    ! which shell for this atom
       ind = first_basis_shell_for_atom_(self,at)     ! first shell for this atom in
                                                 ! the basis set
       self%basis_shell_for_shell(sh) = ind + atom_shell - 1
     end do

   end subroutine

!  **********************
!  Density matrix methods
!  **********************

   subroutine make_scf_density_matrix(self,damp,scf_kind)
    type(mol_type) :: self
    ! Make the density matrix from the molecular orbitals. If "damp" is present
    ! use it to damp the updated density matrix.
    ! NOTE: the final computed density matrix is written to an archive
    ! NOTE: if any old density matrix exists, it is saved in an old archive.
     logical(kind=kind(.true.)), optional :: damp
     character(*), optional :: scf_kind
     logical(kind=kind(.true.)) :: damping
     type(opmatrix_type), pointer :: D_old
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: MO,D,MOa,MOb,Da,Db
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: CMO,CD,CMOa,CMOb,CDa,CDb
     character(128) :: orb_kind

     call ensure_(tonto,self%basis_info_made,"MOL:make_scf_density_matrix ... no basis info")
     call ensure_(tonto,associated(self%molecular_orbitals),"MOL:make_scf_density_matrix ... no molecular orbitals")
      ! Determine if damping is to be used
     if (.not. associated(self%scfdata)) then;                 damping = .false.
     else if (all_destroyed_(self%density_matrix)) then; damping = .false.
     else
       damping = apply_damping_(self%scfdata)
       if (present(damp)) damping = damp
     end if
     if (damping) call create_copy_(D_old,self%density_matrix)
      ! Determine the orb_kind of density matrix to be made
     if (.not. associated(self%scfdata)) then; orb_kind = spinorbital_kind_(self%molecular_orbitals)
     else;                         orb_kind = spinorbital_kind_(self%scfdata)
     end if
      ! Create space for the right kind of density matrix, or save old density matrix
     if (destroyed_(self%density_matrix,orb_kind)) then; call create_(self%density_matrix,orb_kind)
     else;        call destroy_(self%old_density_matrix)
                  call create_copy_(self%old_density_matrix,self%density_matrix)
     end if
      ! Now determine the kind of SCF (if any) associated with the density matrix
     if (present(scf_kind)) then;        orb_kind = scf_kind
     else if (.not. associated(self%scfdata)) then;  orb_kind = guess_scf_kind_(self%molecular_orbitals)
     else;                               orb_kind = self%scfdata%scf_kind
     end if
     select case (orb_kind)
       case ("rhf","rdft","restricted_hartree_fock","xray_rdft", &
             "xray_rhf","noninteracting-group-rhf")
         call ensure_(tonto,created_(self%molecular_orbitals,"restricted"),"MOL:make_scf_density_matrix ... no MO's")
         call ensure_(tonto,self%mult==1,"MOL:make_scf_density_matrix ... this is not a singlet state")
         MO => self%molecular_orbitals%restricted(:,1:self%n_a)
         D  => self%density_matrix%restricted
         call to_scaled_product_of_(D,2.0d0,MO,MO,transpose_b=.true.)
       case ("rohf","restricted_open_shell_hartree_fock")
         call ensure_(tonto,created_(self%molecular_orbitals,"restricted"),"MOL:make_scf_density_matrix ... no MO's")
         MOa => self%molecular_orbitals%restricted(:,1:self%n_a)
         MOb => self%molecular_orbitals%restricted(:,1:self%n_b)
         Da => self%density_matrix%alpha
         Db => self%density_matrix%beta
         call to_product_of_(Da,MOa,MOa,transpose_b=.true.)
         call to_product_of_(Db,MOb,MOb,transpose_b=.true.)
       case ("uhf","udft","unrestricted_hartree_fock")
         call ensure_(tonto,created_(self%molecular_orbitals,"unrestricted"),"MOL:make_scf_density_matrix ... no MO's")
         MOa => self%molecular_orbitals%alpha(:,1:self%n_a)
         MOb => self%molecular_orbitals%beta(:,1:self%n_b)
         Da => self%density_matrix%alpha
         Db => self%density_matrix%beta
         call to_product_of_(Da,MOa,MOa,transpose_b=.true.)
         call to_product_of_(Db,MOb,MOb,transpose_b=.true.)
       case ("ghf","general_hartree_fock")
         call ensure_(tonto,created_(self%molecular_orbitals,"general"),"MOL:make_scf_density_matrix ... no MO's")
         call ensure_(tonto,self%mult==1,"MOL:make_scf_density_matrix ... this is not a singlet state")
         MO => self%molecular_orbitals%general(:,1:self%n_e)
         D =>  self%density_matrix%general
         call to_product_of_(D,MO,MO,transpose_b=.true.)
       case ("rchf","restricted_complex_hartree_fock")
         call ensure_(tonto,created_(self%molecular_orbitals,"restricted_complex"),"MOL:make_scf_density_matrix ... no MO's")
         CMO => self%molecular_orbitals%restricted_complex(:,1:self%n_a)
         CD  => self%density_matrix%restricted_complex
         call to_product_of_(CD,CMO,CMO,dagger_b=.true.)
         CD = 2.0d0*CD
       case ("uchf","unrestricted_complex_hartree_fock")
         call ensure_(tonto,created_(self%molecular_orbitals,"unrestricted_complex"),"MOL:make_scf_density_matrix ... no MO's&
&")
         CMOa => self%molecular_orbitals%alpha_complex(:,1:self%n_a)
         CMOb => self%molecular_orbitals%beta_complex(:,1:self%n_b)
         CDa => self%density_matrix%alpha_complex
         CDb => self%density_matrix%beta_complex
         call to_product_of_(CDa,CMOa,CMOa,dagger_b=.true.)
         call to_product_of_(CDb,CMOb,CMOb,dagger_b=.true.)
       case ("gchf","general_complex_hartree_fock")
         call ensure_(tonto,created_(self%molecular_orbitals,"general_complex"),"MOL:make_scf_density_matrix ... no MO's")
         CMO => self%molecular_orbitals%general_complex(:,1:self%n_e)
         CD =>  self%density_matrix%general_complex
         call to_product_of_(CD,CMO,CMO,dagger_b=.true.)
       case default
         call die_(tonto,"MOL:make_scf_density_matrix ... unknown SCF kind, "//trim(orb_kind))
     end select
     if (damping) then
       call damp_(self%density_matrix,D_old,self%scfdata%damp_factor)
       call destroy_(D_old)
     end if
     if (.not. self%minimal_io) call archive_density_matrix_(self)

   end subroutine

   subroutine make_ao_density_matrix(self)
    type(mol_type) :: self
    ! Make the AO (spin independent) density matrix from the existing density
    ! matrix. The result is placed in the "restricted" part of the density
    ! matrix. NOTE: The density matrix is archived.
      character(128) :: orb_kind

      call ensure_(tonto,self%basis_info_made,"MOL:make_ao_density_matrix ... no basis info")
      call ensure_(tonto,associated(self%density_matrix),"MOL:make_ao_density_matrix ... no density matrix")
      call ensure_(tonto,any_created_(self%density_matrix),"MOL:make_ao_density_matrix ... no density matrix")
      orb_kind = spinorbital_kind_(self%density_matrix)
      select case (orb_kind)
         case ("restricted")
             ! do nothing
         case ("unrestricted")
            call destroy_(self%density_matrix,"restricted")
            call create_(self%density_matrix,"restricted")
            self%density_matrix%restricted = self%density_matrix%alpha + self%density_matrix%beta
         case ("general")
            call destroy_(self%density_matrix,"restricted")
            call create_(self%density_matrix,"restricted")
            self%density_matrix%restricted = alpha_alpha_(self%density_matrix%general) &
                                       + beta_beta_(self%density_matrix%general)
         case ("restricted_complex")
             ! do nothing
         case ("unrestricted_complex")
            call destroy_(self%density_matrix,"restricted_complex")
            call create_(self%density_matrix,"restricted_complex")
            self%density_matrix%restricted_complex = self%density_matrix%alpha_complex &
                                               + self%density_matrix%beta_complex
         case ("general_complex")
            call destroy_(self%density_matrix,"restricted_complex")
            call create_(self%density_matrix,"restricted_complex")
            self%density_matrix%restricted_complex = alpha_alpha_(self%density_matrix%general_complex) &
                                               + beta_beta_(self%density_matrix%general_complex)
         case default;    call die_(tonto,"MOL:make_ao_density_matrix ... unknown kind, "//trim(orb_kind))
      end select
      if (.not. self%minimal_io) call archive_density_matrix_(self)

   end subroutine

   subroutine make_ao_sz_density_matrix(self)
    type(mol_type) :: self
    ! Make the AO (spin independent) density matrix from the existing density
    ! matrix. The result is placed in the "restricted" part of the density matrix
      character(128) :: orb_kind

      call ensure_(tonto,self%basis_info_made,"MOL:make_ao_sz_density_matrix ... no basis info")
      call ensure_(tonto,associated(self%density_matrix),"MOL:make_ao_sz_density_matrix ... no density matrix")
      orb_kind = spinorbital_kind_(self%density_matrix)
      if (orb_kind=="restricted") then;   return; end if
      if (includes_(orb_kind,"complex")) then
         call destroy_(self%density_matrix,"restricted_complex")
         call create_(self%density_matrix,"restricted_complex")
      else
         call destroy_(self%density_matrix,"restricted")
         call create_(self%density_matrix,"restricted")
      end if
      select case (orb_kind)
         case ("unrestricted")
            self%density_matrix%restricted = self%density_matrix%alpha - self%density_matrix%beta
         case ("general")
            self%density_matrix%restricted = alpha_alpha_(self%density_matrix%general) &
                                       - beta_beta_(self%density_matrix%general)
         case ("unrestricted_complex")
            self%density_matrix%restricted_complex = self%density_matrix%alpha_complex &
                                               - self%density_matrix%beta_complex
         case ("general_complex")
            self%density_matrix%restricted_complex = alpha_alpha_(self%density_matrix%general_complex) &
                                               - beta_beta_(self%density_matrix%general_complex)
         case default;   call die_(tonto,"MOL:make_ao_sz_density_matrix ... unknown kind, "//trim(orb_kind))
      end select

   end subroutine

   subroutine make_scf_density_matrix_1(self,n,nb)
    type(mol_type) :: self
    ! Make the density matrix from orbital "n" of the molecular orbitals.
    ! If present, orbital "nb" of the beta molecular orbitals is used.
    ! If either index is not an occupied MO, the density is set to zero.
      integer(kind=kind(1)), intent(in) :: n
      integer(kind=kind(1)), intent(in), optional :: nb
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: MO,D,MOa,MOb,Da,Db
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: CMO,CD,CMOa,CMOb,CDa,CDb
      character(128) :: orb_kind
      type(archive_type) :: arch
      integer(kind=kind(1)) :: m
      logical(kind=kind(.true.)) :: uhf

      call ensure_(tonto,self%basis_info_made,"MOL:make_scf_density_matrix_1 ... no basis info")
      call ensure_(tonto,associated(self%molecular_orbitals),"MOL:make_scf_density_matrix_1 ... no molecular orbitals")
      uhf = includes_(self%scfdata%scf_kind,"unrestricted")
      call warn_if_(tonto,present(nb) .and. (.not. uhf),"MOL:make_scf_density_matrix_1 ... nb ignored")
      m = n
      if (present(nb)) m = nb
      orb_kind = spinorbital_kind_(self%scfdata)
      call destroy_(self%density_matrix,orb_kind)
      call create_(self%density_matrix,orb_kind)
      call set_(arch,self%name,"density_matrix",orb_kind)
      orb_kind = self%scfdata%scf_kind
      select case (orb_kind)
         case ("rhf","rdft","restricted_hartree_fock","xray_rhf","xray_rks", &
               "xray_rdft","noninteracting-group-rhf")
            call ensure_(tonto,self%mult==1,"MOL:make_scf_density_matrix_1 ... this is not a singlet state")
            D  => self%density_matrix%restricted
            if (n>0 .and. n<=self%n_a) then
               MO => self%molecular_orbitals%restricted(:,n:n)
               call to_scaled_product_of_(D,2.0d0,MO,MO,transpose_b=.true.)
            else
               D = 0.0d0
            end if
         case ("rohf","restricted_open_shell_hartree_fock")
            Da => self%density_matrix%alpha
            Db => self%density_matrix%beta
            if (n>0 .and. n<=self%n_b) then
               MOa => self%molecular_orbitals%restricted(:,n:n)
               MOb => self%molecular_orbitals%restricted(:,n:n)
               call to_product_of_(Da,MOa,MOa,transpose_b=.true.)
               call to_product_of_(Db,MOb,MOb,transpose_b=.true.)
            else if (n>nb .and. n<=self%n_a) then
               MOa => self%molecular_orbitals%restricted(:,n:n)
               call to_product_of_(Da,MOa,MOa,transpose_b=.true.)
               Db = 0.0d0
            else
               Da = 0.0d0
               Db = 0.0d0
            end if
         case ("uhf","udft","unrestricted_hartree_fock")
            Da => self%density_matrix%alpha
            Db => self%density_matrix%beta
            if (n>0 .and. n<=self%n_a) then
               MOa => self%molecular_orbitals%alpha(:,n:n)
               call to_product_of_(Da,MOa,MOa,transpose_b=.true.)
            else
               Da = 0.0d0
            end if
            if (m>0 .and. m<=self%n_b) then
               MOb => self%molecular_orbitals%beta(:,m:m)
               call to_product_of_(Db,MOb,MOb,transpose_b=.true.)
            else
               Db = 0.0d0
            end if
         case ("ghf","general_hartree_fock")
            call ensure_(tonto,self%mult==1,"MOL:make_scf_density_matrix_1 ... this is not a singlet state")
            D =>  self%density_matrix%general
            if (n>0 .and. n<=self%n_e) then
               MO => self%molecular_orbitals%general(:,n:n)
               call to_product_of_(D,MO,MO,transpose_b=.true.)
            else
               D = 0.0d0
            end if
         case ("rchf","restricted_complex_hartree_fock")
            CD  => self%density_matrix%restricted_complex
            if (n>0 .and. n<=self%n_a) then
               CMO => self%molecular_orbitals%restricted_complex(:,n:n)
               call to_scaled_product_of_(CD,2.0d0,CMO,CMO,dagger_b=.true.)
            else
               CD = 0.0d0
            end if
         case ("uchf","unrestricted_complex_hartree_fock")
            CDa => self%density_matrix%alpha_complex
            CDb => self%density_matrix%beta_complex
            if (n>0 .and. n<=self%n_a) then
               CMOa => self%molecular_orbitals%alpha_complex(:,n:n)
               call to_product_of_(CDa,CMOa,CMOa,dagger_b=.true.)
            else
               CDa = 0.0d0
            end if
            if (m>0 .and. m<=self%n_b) then
               CMOb => self%molecular_orbitals%beta_complex(:,m:m)
               call to_product_of_(CDb,CMOb,CMOb,dagger_b=.true.)
            else
               CDb = 0.0d0
            end if
         case ("gchf","general_complex_hartree_fock")
            CD =>  self%density_matrix%general_complex
            if (n>0 .and. n<=self%n_e) then
               CMO => self%molecular_orbitals%general_complex(:,n:n)
               call to_product_of_(CD,CMO,CMO,dagger_b=.true.)
            else
               CD = 0.0d0
            end if
         case default;
            call die_(tonto,"MOL:make_scf_density_matrix_1 ... unknown kind, "//trim(orb_kind))
      end select
      call write_(arch,self%density_matrix)

   end subroutine

   subroutine make_group_density_matrix(self)
    type(mol_type) :: self
    ! Make the density matrix which is a sum of density matrices for
    ! each group specified in the molecule in the atom_group array.
    ! This will destroy any existing restricted density matrix archive.
      type(archive_type) :: archive
       type(opmatrix_type), pointer :: P

      call make_group_density_(self)
      call create_(P,self%n_bf)
      call set_(archive,self%name,"group_density_matrix", genre="restricted")
      call read_(archive,P, genre="restricted")
      call set_(archive,self%name,"density_matrix", genre="restricted")
      call write_(archive,P, genre="restricted")

   end subroutine

   subroutine make_promol_density_matrix(self)
    type(mol_type) :: self
    ! Make the promolecule density matrix from symmetrically orthonormalised
    ! group (monomer) molecular orbitals. The promolecule orbitals are also
    ! made and are ordered by group.
    ! This will destroy any existing restricted density matrix and MOs.

   call ensure_(tonto,associated(self%molecular_orbitals),"MOL:make_promol_density_matrix ... no MO's")
      call make_group_density_(self,MOs=.true.)
      call symorthonormalise_occupied_MOs_(self)
      self%natural_orbitals => self%molecular_orbitals
      call make_density_matrix_(self)
      nullify(self%natural_orbitals)

   end subroutine

   subroutine symorthonormalise_occupied_MOs(self)
    type(mol_type) :: self
    ! Symmetrically orthonormalise the occupied MO's. The occupied orbitals
    ! are defined by the .occupation_numbers vector. The virtual orbitals are
    ! schmidt orthonormalised to the occupied MOs. Only works for restricted
    ! at the moment.
      integer(kind=kind(1)), dimension(:), pointer :: occ,vir,all
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: S,MO
      integer(kind=kind(1)) :: i,no,nv

   call ensure_(tonto,associated(self%molecular_orbitals),"MOL:symorthonormalise_occupied_MOs ... no MO's")
   call ensure_(tonto,associated(self%molecular_orbitals%restricted),"MOL:symorthonormalise_occupied_MOs ... no rhf MO's")
      no = count(self%occupation_numbers%restricted >0.1); call create_(occ,no)
      nv = count(self%occupation_numbers%restricted<=0.1); call create_(vir,nv)
      call ensure_(tonto,(no+nv)==self%n_bf,"MOL:symorthonormalise_occupied_MOs ... error determining no and nv")
      call flush_(stdout)
      call show_(stdout,"No. of occupied MO's =",no)
      call show_(stdout,"No. of virtual  MO's =",nv)
      call create_(S,self%n_bf,self%n_bf); call get_overlap_matrix_(self,S)
      occ = pack((/(i,i=1,self%n_bf)/),self%occupation_numbers%restricted> 1.9)
      vir = pack((/(i,i=1,self%n_bf)/),self%occupation_numbers%restricted<=1.9)
       ! Symmetric orthonormalise the occupied
      call create_(MO,self%n_bf,no)
!      MO = .molecular_orbitals.restricted(:,occ)
      call flush_(stdout)
      call text_(stdout,"Occupied molecular orbitals before symmetrisation:")
      call put_(stdout,MO)
      call symmetrically_orthonormalise_(MO,S)
      call flush_(stdout)
      call text_(stdout,"Occupied molecular orbitals after symmetrisation:")
      call put_(stdout,MO)
!      .molecular_orbitals.restricted(:,occ) = MO
      call destroy_(MO)
       ! Schmidt orthonormalise the virtuals
      all => join_(occ,vir)
      call create_(MO,self%n_bf,self%n_bf)
!      MO = .molecular_orbitals.restricted(:,all)
      call schmidt_orthonormalise_(MO,S)
!      .molecular_orbitals.restricted(:,all) = MO
      call flush_(stdout)
      call text_(stdout,"Occupied & virtual molecular orbitals after Schmidt:")
      call put_(stdout,MO)
      call destroy_(all)
       ! Clean up
      call destroy_(MO)
      call destroy_(S)
      call destroy_(vir)
      call destroy_(occ)

   end subroutine

   subroutine make_density_matrix(self)
    type(mol_type) :: self
    ! Make the .density_matrix from the .natural_orbitals and the
    ! .occupation_numbers vector. Only restricted so far.
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: NO,D
     real(kind=kind(1.0d0)), dimension(:), pointer :: occ
     character(128) :: orb_kind
     type(archive_type) :: arch

     call ensure_(tonto,associated(self%occupation_numbers),"MOL:make_density_matrix ... no occupation numbers")
     call ensure_(tonto,associated(self%natural_orbitals),"MOL:make_density_matrix ... no natural orbitals")
     orb_kind = spinorbital_kind_(self%natural_orbitals)
     call destroy_(self%density_matrix,orb_kind)
     call create_(self%density_matrix,orb_kind)
     call set_(arch,self%name,"density_matrix",orb_kind)
     select case (orb_kind)
       case ("restricted")
         call ensure_(tonto,created_(self%natural_orbitals,"restricted"),"MOL:make_density_matrix ... no NO's")
         call create_(occ,self%n_bf)
         occ = sqrt(self%occupation_numbers%restricted)
         call create_(NO,self%n_bf,self%n_bf)
         call to_product_with_diagonal_(NO,self%natural_orbitals%restricted,occ)
         D  => self%density_matrix%restricted
         call to_product_of_(D,NO,NO,transpose_b=.true.)
         call destroy_(NO)
         call destroy_(occ)
       case default
         call die_(tonto,"MOL:make_density_matrix ... unknown SCF kind, "//trim(orb_kind))
     end select
     call write_(arch,self%density_matrix)

   end subroutine

!****************************
!* Overlap Matrix formation *
!****************************

   subroutine get_overlap_matrix(self,S)
    type(mol_type) :: self
    ! Set "S" to the overlap matrix.
    ! If the archive file exists, read it; otherwise make it.
      real(kind=kind(1.0d0)), dimension(:,:) :: S
      type(archive_type) :: arch
      logical(kind=kind(.true.)) :: write_archive,calc_it

      call set_(arch,self%name,"overlap_matrix")
      calc_it = self%scfdata%direct
      write_archive = .not. self%scfdata%direct
      if (.not. calc_it) then
        if (exists_(arch)) then
          call read_(arch,S)
          calc_it = .false.
          write_archive = .false.
        else
          calc_it = .true.
        end if
      end if
      if (calc_it) then
         if (self%scfdata%group) then
               call make_group_overlap_matrix_(self,S)
         else
               call make_overlap_matrix_(self,S)
         end if
      end if
      if (write_archive) call write_(arch,S)

   end subroutine

   subroutine make_overlap_matrix(self,S)
    type(mol_type) :: self
    ! Calculate the overlap matrix "S"
     real(kind=kind(1.0d0)), dimension(:,:), target :: S
     integer(kind=kind(1)) :: q,fa,la,fb,lb,start,step
     type(shell2_type) :: sh

     call ensure_(tonto,associated(self%atom),"MOL:make_overlap_matrix ... no atom list")
     call ensure_(tonto,self%basis_info_made,"MOL:make_overlap_matrix ... no basis info")
     start = 1 + this_proc_(tonto_parallel)
     step = n_proc_(tonto_parallel)
     if (tonto_parallel%do_parallel) S = 0.0d0
     do q=start,self%n_shell_pairs,step
       call get_shell_pair_(self,sh,q,fa,la,fb,lb)
       call make_overlap_(sh,S(fa:la,fb:lb))
       call destroy_ptr_part_(sh)
     end do
     call sum_symmetric_matrices_(tonto_parallel,S)
     call symmetric_reflect_(S)

   end subroutine

   subroutine make_group_overlap_matrix(self,S)
    type(mol_type) :: self
    ! Calculate the noninteracting group overlap matrix "S"
     real(kind=kind(1.0d0)), dimension(:,:), target :: S
     integer(kind=kind(1)) :: q,fa,la,fb,lb, atom_a,atom_b
     type(shell2_type) :: sh

     call ensure_(tonto,self%basis_info_made,"MOL:make_group_overlap_matrix ... no basis info")
     call ensure_(tonto,associated(self%atom),"MOL:make_group_overlap_matrix ... no atom list")
     call ensure_(tonto,associated(self%atom_group),"MOL:make_group_overlap_matrix ... no atom_group info")
     do q=1,self%n_shell_pairs
       call get_shell_pair_(self,sh,q,fa,la,fb,lb,atom_a,atom_b)
       if (in_same_atom_group_(self,atom_a,atom_b)) call make_overlap_(sh,S(fa:la,fb:lb))
       call destroy_ptr_part_(sh)
     end do
     call symmetric_reflect_(S)

   end subroutine

!  ***************
!  Inquiry methods
!  ***************

   function no_of_electrons(self) result(res)
    type(mol_type) :: self
    ! Work out and return the number of electrons in the molecule.
      integer(kind=kind(1)) :: res

      call ensure_(tonto,associated(self%atom),"MOL:no_of_electrons ... no atom list")
      res = n_e_(self%atom) - self%charge

   end function

   function no_of_alpha_electrons(self) result(res)
    type(mol_type) :: self
    ! Work out and return the number of alpha electrons in the molecule.
      integer(kind=kind(1)) :: res

      res = no_of_electrons_(self) - no_of_beta_electrons_(self)

   end function

   function no_of_beta_electrons(self) result(res)
    type(mol_type) :: self
    ! Work out and return the number of beta electrons in the molecule
      integer(kind=kind(1)) :: res
      integer(kind=kind(1)) :: tmp

      tmp = no_of_electrons_(self) - no_of_unpaired_electrons_(self)
      res = tmp/2
      call ensure_(tonto,is_even_(tmp),"MOL:no_of_beta_electrons ... Inconsistent multiplicity, non integer # of beta electro&
&ns")

   end function

   function has_valid_no_of_beta_electrons(self) result(res)
    type(mol_type) :: self
    ! Return .true. if the number of beta electrons is valid based on the assigned
    ! charge and multiplicity i.e. it must come out integral.
      logical(kind=kind(.true.)) :: res
      integer(kind=kind(1)) :: tmp

      tmp = no_of_electrons_(self) - no_of_unpaired_electrons_(self)
      res = is_even_(tmp)

   end function

   function no_of_unpaired_electrons(self) result(res)
    type(mol_type) :: self
    ! Work out and return the number of unpaired electrons in the molecule
      integer(kind=kind(1)) :: res

      res = self%mult - 1
      call ensure_(tonto,res<=no_of_electrons_(self),"MOL:no_of_unpaired_electrons ... Wrong multiplicity, not enough electro&
&ns")

   end function

   function no_of_atom_pairs(self) result(res)
    type(mol_type) :: self
    ! Return the number of shell pairs in the basis set for the molecule
      integer(kind=kind(1)) :: res

      call ensure_(tonto,associated(self%atom),"MOL:no_of_atom_pairs ... no atom list")
      res = self%n_atom*(self%n_atom+1)/2

   end function

   function no_of_shell_pairs(self) result(res)
    type(mol_type) :: self
    ! Return the number of shell pairs in the basis set for the molecule
      integer(kind=kind(1)) :: res
      integer(kind=kind(1)) :: n_shell

      call ensure_(tonto,self%basis_info_made,"MOL:no_of_shell_pairs ... no basis info")
      call ensure_(tonto,associated(self%atom),"MOL:no_of_shell_pairs ... no atom list")
      n_shell = n_shell_(self%atom)
      res = n_shell*(n_shell+1)/2

   end function

   function n_shell_quartets(self) result(res)
    type(mol_type) :: self
    ! Return the number of shell quartets in the basis set for the molecule
    ! Note, "res" will die at 22 shells for integer(2).
    !       "res" will die at 361 shells for integer(4).
    !       "res" will die at 92681 shells for integer(8).
      integer(kind=kind(1)) :: res
      integer(kind=kind(1)) :: n_shell_pair

      n_shell_pair = no_of_shell_pairs_(self)
      if (n_shell_pair/sqrt(2.0d0) <= (huge(n_shell_pair))**0.50d0) then
         res = n_shell_pair*(n_shell_pair+1)/2
      else
         call warn_(tonto,"MOL:n_shell_quartets ... too many shells")
         res = 0
      end if

   end function

   function no_of_occupied_NOs(self,orb_kind,tol) result(res)
    type(mol_type) :: self
    ! Returns the number of non-zero occupied natural orbitals. For this purpose,
    ! zero is defined to be "tol" if present, or 10.0d0**(-7) otherwise
      character(*), optional :: orb_kind
      real(kind=kind(1.0d0)), optional :: tol
      integer(kind=kind(1)) :: res

      call ensure_(tonto,associated(self%occupation_numbers),"MOL:no_of_occupied_NOs ... no occupation numbers")
      res = no_of_occupied_(self%occupation_numbers,orb_kind,tol)

   end function

!  ****************
!  Natural orbitals
!  ****************

   subroutine make_natural_orbitals(self)
    type(mol_type) :: self
    ! Make the natural orbitals from the density matrix
      character(128) :: orb_kind

      call ensure_(tonto,self%basis_info_made,"MOL:make_natural_orbitals ... no basis info")
      call ensure_(tonto,associated(self%density_matrix),"MOL:make_natural_orbitals ... no density matrix")
      call ensure_(tonto,any_created_(self%density_matrix),"MOL:make_natural_orbitals ... no density matrix")
      orb_kind = spinorbital_kind_(self%density_matrix)
      if (associated(self%natural_orbitals)) call destroy_ptr_part_(self%natural_orbitals)
      select case (orb_kind)
         case ("restricted        "); call make_restricted_NOs_(self)
         case ("unrestricted      "); call make_unrestricted_NOs_(self)
!        case ("general           "); .make_general_NOs
         case ("restricted_complex"); call make_restricted_complex_NOs_(self)
         case ("general_complex   "); call make_general_complex_NOs_(self)
         case default;                allocate(tonto%known_keywords(4))
         tonto%known_keywords(1) = "restricted        "
         tonto%known_keywords(2) = "unrestricted      "
         tonto%known_keywords(3) = "restricted_complex"
         tonto%known_keywords(4) = "general_complex   "
         call unknown_(tonto,orb_kind,"MOL:make_natural_orbitals")
         deallocate(tonto%known_keywords)
      end select
      call archive_natural_orbitals_(self)
      call archive_occupation_numbers_(self)

   end subroutine

   subroutine make_restricted_NOs(self)
    type(mol_type) :: self
    ! Make the restricted natural orbitals from the density matrix

      call ensure_(tonto,associated(self%density_matrix),"MOL:make_restricted_NOs ... no density matrix")
      call ensure_(tonto,associated(self%density_matrix%restricted),"MOL:make_restricted_NOs ... no density matrix")
      if (associated(self%natural_orbitals)) call destroy_(self%natural_orbitals,"restricted")
      call create_(self%natural_orbitals,self%n_bf,"restricted")
      if (associated(self%occupation_numbers)) call destroy_(self%occupation_numbers,"restricted")
      call create_(self%occupation_numbers,self%n_bf,"restricted")
      call make_r_NOs_(self,self%natural_orbitals%restricted, &
                  self%occupation_numbers%restricted, &
                  self%density_matrix%restricted)

   end subroutine

   subroutine make_unrestricted_NOs(self)
    type(mol_type) :: self
    ! Make the unrestricted natural orbitals from the density matrix

      call ensure_(tonto,associated(self%density_matrix),"MOL:make_unrestricted_NOs ... no density matrix")
      call ensure_(tonto,associated(self%density_matrix%alpha),"MOL:make_unrestricted_NOs ... no density matrix")
      call ensure_(tonto,associated(self%density_matrix%beta),"MOL:make_unrestricted_NOs ... no density matrix")
      if (associated(self%natural_orbitals)) call destroy_(self%natural_orbitals,"unrestricted")
      call create_(self%natural_orbitals,self%n_bf,"unrestricted")
      if (associated(self%occupation_numbers)) call destroy_(self%occupation_numbers,"unrestricted")
      call create_(self%occupation_numbers,self%n_bf,"unrestricted")
      call make_r_NOs_(self,self%natural_orbitals%alpha, &
                  self%occupation_numbers%alpha, &
                  self%density_matrix%alpha)
      call make_r_NOs_(self,self%natural_orbitals%beta, &
                  self%occupation_numbers%beta, &
                  self%density_matrix%beta)

   end subroutine

   subroutine make_r_NOs(self,NO,occupation,P)
    type(mol_type) :: self
    ! Make the real natural orbitals "NO" and "occupation" numbers from the
    ! density matrix "P"
      real(kind=kind(1.0d0)), dimension(:,:) :: NO,P
      real(kind=kind(1.0d0)), dimension(:) :: occupation
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: V,X,S

      call create_(V,self%n_bf,self%n_bf)
      call create_(S,self%n_bf,self%n_bf); call get_overlap_matrix_(self,S)
      call to_sqrt_(V,S)
      call destroy_(S)
      call create_(X,self%n_bf,self%n_bf)
      X = P
      call change_basis_(X,V)
      call solve_eigenproblem_(X,occupation,V)
      call create_(S,self%n_bf, self%n_bf); call get_overlap_matrix_(self,S)
      call to_inverse_sqrt_(X,S)
      call destroy_(S)
      call to_product_of_(NO,X,V)
      call destroy_(X)
      call destroy_(V)
      occupation = occupation(self%n_bf:1:-1)
      call zero_small_values_(occupation,10.0d0**(-10))
      NO = NO(:,self%n_bf:1:-1)

   end subroutine

   subroutine make_restricted_complex_NOs(self)
    type(mol_type) :: self
    ! Make the restricted complex natural orbitals from the density matrix
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: V,X
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: S,O

     call ensure_(tonto,associated(self%density_matrix),"MOL:make_restricted_complex_NOs ... no density matrix")
     call ensure_(tonto,associated(self%density_matrix%restricted_complex),"MOL:make_restricted_complex_NOs ... no density ma&
&trix")
     if (associated(self%natural_orbitals)) call destroy_(self%natural_orbitals,"restricted_complex")
     if (associated(self%occupation_numbers)) call destroy_(self%occupation_numbers,"restricted")
     call create_(self%natural_orbitals,self%n_bf,"restricted_complex")
     call create_(self%occupation_numbers,self%n_bf,"restricted")
     call create_(V,self%n_bf,self%n_bf)
     call create_(X,self%n_bf,self%n_bf)
     call create_(O,self%n_bf,self%n_bf)
     call create_(S,self%n_bf,self%n_bf); call get_overlap_matrix_(self,S)
     call to_sqrt_(O,S)
     call destroy_(S)
     V = O
     X = self%density_matrix%restricted_complex
     call change_basis_(X,V)
     call solve_eigenproblem_(X,self%occupation_numbers%restricted,V)
     call create_(S,self%n_bf, self%n_bf); call get_overlap_matrix_(self,S)
     call to_inverse_sqrt_(O,S)
     call destroy_(S)
     X = O
     call destroy_(O)
     call to_product_of_(self%natural_orbitals%restricted_complex,X,V)
     call destroy_(X)
     call destroy_(V)
     self%occupation_numbers%restricted = self%occupation_numbers%restricted(self%n_bf:1:-1)
     call zero_small_values_(self%occupation_numbers%restricted,10.0d0**(-10))
     self%natural_orbitals%restricted_complex = self%natural_orbitals%restricted_complex(:,self%n_bf:1:-1)

   end subroutine

   subroutine make_general_complex_NOs(self)
    type(mol_type) :: self
    ! Make the general natural orbitals and occupations from the complex density
    ! matrix.
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: S,R
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: V,X

     call ensure_(tonto,associated(self%density_matrix),"MOL:make_general_complex_NOs ... no density matrix")
     call ensure_(tonto,associated(self%density_matrix%general_complex),"MOL:make_general_complex_NOs ... no density matrix")
     if (associated(self%natural_orbitals)) call destroy_(self%natural_orbitals,"general_complex")
     if (associated(self%occupation_numbers)) call create_(self%occupation_numbers,self%n_bf,"general_complex")
     call create_(self%natural_orbitals,self%n_bf,"general_complex")
     call create_(self%occupation_numbers,self%n_bf,"general")
     call create_(V,2*self%n_bf,2*self%n_bf)
     call create_(R,self%n_bf,self%n_bf)
     call create_(S,self%n_bf,self%n_bf); call get_overlap_matrix_(self,S)
     call to_sqrt_(R,S)
     call destroy_(S)
     call alpha_alpha_set_to_(V,R)
     call beta_beta_set_to_(V,R)
     call destroy_(R)
     call create_(X,2*self%n_bf,2*self%n_bf)
     X = self%density_matrix%general_complex
     call change_basis_(X,V)
     call solve_eigenproblem_(X,self%occupation_numbers%general,V)
     call create_(R,self%n_bf,self%n_bf)
     call create_(S,self%n_bf,self%n_bf); call get_overlap_matrix_(self,S)
     call to_inverse_sqrt_(R,S)
     call destroy_(S)
     call alpha_alpha_set_to_(X,R)
     call beta_beta_set_to_(X,R)
     call destroy_(R)
     call to_product_of_(self%natural_orbitals%general_complex,X,V)
     self%occupation_numbers%general = self%occupation_numbers%general(2*self%n_bf:1:-1)
     call zero_small_values_(self%occupation_numbers%general,10.0d0**(-10))
     self%natural_orbitals%general = self%natural_orbitals%general(:,2*self%n_bf:1:-1)
     call destroy_(X)
     call destroy_(V)

   end subroutine

!  ***************************
!  Population Analysis Methods
!  ***************************

   subroutine make_mulliken_matrix(self)
    type(mol_type) :: self
    ! sets the mulliken population matrix and the outputs it.
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: mulliken_matrix, S
      real(kind=kind(1.0d0)), dimension(:), pointer :: diagonals

      call create_(S,self%n_bf, self%n_bf)
      call create_(mulliken_matrix,self%n_bf, self%n_bf)
      call create_(diagonals,self%n_bf)
      call get_overlap_matrix_(self,S)
      mulliken_matrix = matmul(self%density_matrix%restricted, S)
      call get_diagonal_(mulliken_matrix,diagonals)
      call put_mulliken_populations_(self,mulliken_matrix, diagonals)
      call destroy_(S)
      call destroy_(mulliken_matrix)
      call destroy_(diagonals)

   end subroutine

   subroutine put_mulliken_populations(self,mulliken_matrix, diagonals)
    type(mol_type) :: self
    ! outputs the mulliken populations to the output file
      real(kind=kind(1.0d0)), dimension(:,:) :: mulliken_matrix
      real(kind=kind(1.0d0)), dimension(:) :: diagonals
      real(kind=kind(1.0d0)) :: sum
      integer(kind=kind(1)) :: o_count, a, b, c

      call flush_(stdout)
      call text_(stdout,"Mulliken population analysis: ")
      call dash_(stdout,int_fields=2,real_fields=1)
      call put_(stdout,"Atom", int_width = .true.)
      call put_(stdout,"Type", int_width = .true.)
      call put_(stdout,"Population")
      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=1)
      o_count = 1
      do a = 1, self%n_atom
         do b = 1, n_shell_for_atom_(self%atom,a)
            do c = 1, self%atom(a)%basis%shell(b)%n_comp
               call put_(stdout,self%atom(a)%label, int_width = .true.)
               call put_(stdout,l_chr_(self%atom(a)%basis%shell(b)), int_width = .true.)
               call put_(stdout,diagonals(c + o_count - 1))
               call flush_(stdout)
            end do
            o_count = o_count + self%atom(a)%basis%shell(b)%n_comp
         end do
      end do
      call flush_(stdout)
      call text_(stdout,"Trace of the matrix: ")
      call put_(stdout,trace_(mulliken_matrix) )
      call flush_(stdout)
      call text_(stdout,"Atomic Populations: ")
      call dash_(stdout,int_fields=1, real_fields=1)
      call put_(stdout,"Atom", int_width = .true.)
      call put_(stdout,"Population")
      call flush_(stdout)
      call dash_(stdout,int_fields=1, real_fields=1)
      o_count = 1
      do a = 1, self%n_atom
         call put_(stdout,self%atom(a)%label, int_width = .true.)
         sum = 0
         do b = 1, n_shell_for_atom_(self%atom,a)
            sum = sum + diagonals(b - 1 + o_count)
         end do
         call put_(stdout,sum)
         call flush_(stdout)
         o_count = o_count + n_bf_(self%atom(a))
      end do

   end subroutine

   subroutine get_kinetic_matrix(self,T)
    type(mol_type) :: self
    ! Set "T" to the kinetic matrix
    ! If the archive file exists, read it; otherwise make it.
      real(kind=kind(1.0d0)), dimension(:,:) :: T
      type(archive_type) :: arch
      logical(kind=kind(.true.)) :: write_archive,calc_it

      call set_(arch,self%name,"kinetic_matrix")
      calc_it = self%scfdata%direct
      write_archive = .not. self%scfdata%direct
      if (.not. calc_it) then
        if (exists_(arch)) then
          call read_(arch,T)
          calc_it = .false.
          write_archive = .false.
        else
          calc_it = .true.
        end if
      end if
      if (calc_it) then
         if      (self%scfdata%nddo) then;  call make_nddo_kinetic_matrix_(self,T)
         else if (self%scfdata%nudo) then;  call make_nudo_kinetic_matrix_(self,T)
         else if (self%scfdata%group) then; call make_group_kinetic_matrix_(self,T)
         else;                          call make_kinetic_matrix_(self,T)
         end if
      end if
      if (write_archive) call write_(arch,T)

   end subroutine

   subroutine make_kinetic_matrix(self,T)
    type(mol_type) :: self
    ! Calculate the kinetic energy matrix "T".
     real(kind=kind(1.0d0)), dimension(:,:) :: T
     integer(kind=kind(1)) :: q,fa,la,fb,lb,start,step
     type(shell2_type) :: sh

     call ensure_(tonto,self%basis_info_made,"MOL:make_kinetic_matrix ... no basis info")
     call ensure_(tonto,associated(self%atom),"MOL:make_kinetic_matrix ... no atom list")
     start = 1 + this_proc_(tonto_parallel)
     step = n_proc_(tonto_parallel)
     if (tonto_parallel%do_parallel) T = 0.0d0
     do q=start,self%n_shell_pairs,step
       call get_shell_pair_(self,sh,q,fa,la,fb,lb)
       call get_kei_(sh,T(fa:la,fb:lb))
       call destroy_ptr_part_(sh)
     end do
     call sum_symmetric_matrices_(tonto_parallel,T)
     call symmetric_reflect_(T)

   end subroutine

   subroutine make_nddo_kinetic_matrix(self,T)
    type(mol_type) :: self
    ! Calculate the NDDO kinetic energy matrix "T".
     real(kind=kind(1.0d0)), dimension(:,:) :: T
     integer(kind=kind(1)) :: q,fa,la,fb,lb,atom_a,atom_b
     type(shell2_type) :: sh

     call ensure_(tonto,self%basis_info_made,"MOL:make_nddo_kinetic_matrix ... no basis info")
     call ensure_(tonto,associated(self%atom),"MOL:make_nddo_kinetic_matrix ... no atom list")
     do q=1,self%n_shell_pairs
       call get_shell_pair_(self,sh,q,fa,la,fb,lb,atom_a,atom_b)
       if (atom_a==atom_b) then; call get_kei_(sh,T(fa:la,fb:lb))
       else;                     T(fa:la,fb:lb) = 0.0d0
       end if
       call destroy_ptr_part_(sh)
     end do
     call symmetric_reflect_(T)

   end subroutine

   subroutine make_nudo_kinetic_matrix(self,T)
    type(mol_type) :: self
    ! Calculate the NUDO kinetic energy matrix "T". NUDO = neglect of unconnected
    ! differential overlap.
     real(kind=kind(1.0d0)), dimension(:,:) :: T
     integer(kind=kind(1)) :: q,fa,la,fb,lb,atom_a,atom_b
     type(shell2_type) :: sh

     call ensure_(tonto,self%basis_info_made,"MOL:make_nudo_kinetic_matrix ... no basis info")
     call ensure_(tonto,associated(self%atom),"MOL:make_nudo_kinetic_matrix ... no atom list")
     do q=1,self%n_shell_pairs
       call get_shell_pair_(self,sh,q,fa,la,fb,lb,atom_a,atom_b)
       if (bonded_(self%atom,atom_a,atom_b)) then; call get_kei_(sh,T(fa:la,fb:lb))
       else;                                  T(fa:la,fb:lb) = 0.0d0
       end if
       call destroy_ptr_part_(sh)
     end do
     call symmetric_reflect_(T)

   end subroutine

   subroutine make_group_kinetic_matrix(self,T)
    type(mol_type) :: self
    ! Calculate the noninteracting group  kinetic energy matrix "T".
     real(kind=kind(1.0d0)), dimension(:,:) :: T
     integer(kind=kind(1)) :: q,fa,la,fb,lb,atom_a,atom_b
     type(shell2_type) :: sh

     call ensure_(tonto,self%basis_info_made,"MOL:make_group_kinetic_matrix ... no basis info")
     call ensure_(tonto,associated(self%atom),"MOL:make_group_kinetic_matrix ... no atom list")
     call ensure_(tonto,associated(self%atom_group),"MOL:make_group_kinetic_matrix ... no atom_group info")
     do q=1,self%n_shell_pairs
       call get_shell_pair_(self,sh,q,fa,la,fb,lb,atom_a,atom_b)
       if (in_same_atom_group_(self,atom_a,atom_b)) then; call get_kei_(sh,T(fa:la,fb:lb))
       else;                                         T(fa:la,fb:lb) = 0.0d0
       end if
       call destroy_ptr_part_(sh)
     end do
     call symmetric_reflect_(T)

   end subroutine

   subroutine get_kinetic_overlap(self,T,S)
    type(mol_type) :: self
    ! Set "S" to the overlap matrix and "T" to the kinetic matrix
    ! If both archives exists, read them; otherwise make them both!
      real(kind=kind(1.0d0)), dimension(:,:) :: T,S
      logical(kind=kind(.true.)) :: write_archive,calc_it
      type(archive_type) :: T_archive,S_archive

      call set_(S_archive,self%name,"overlap_matrix")
      call set_(T_archive,self%name,"kinetic_matrix")
      calc_it = self%scfdata%direct
      write_archive = .not. self%scfdata%direct
      if (.not. calc_it) then
        if ((exists_(S_archive)) .and. (exists_(T_archive))) then
          call read_(S_archive,S)
          call read_(T_archive,T)
          calc_it = .false.
          write_archive = .false.
        else
          calc_it = .true.
        end if
      end if
      if (calc_it) then
         if      (self%scfdata%nddo) then;  call make_nddo_kinetic_overlap_(self,T,S)
         else if (self%scfdata%nudo) then;  call make_nudo_kinetic_overlap_(self,T,S)
         else if (self%scfdata%group) then; call make_group_kinetic_overlap_(self,T,S)
         else;                          call make_kinetic_overlap_(self,T,S)
         end if
      end if
      if (write_archive) then
        call write_(S_archive,S)
        call write_(T_archive,T)
      end if

   end subroutine

   subroutine make_kinetic_overlap(self,T,S)
    type(mol_type) :: self
    ! Calculate the kinetic energy matrix "T" and overlap integral matrix "S"
     real(kind=kind(1.0d0)), dimension(:,:) :: T,S
     integer(kind=kind(1)) :: q,fa,la,fb,lb
     type(shell2_type) :: sh

     call ensure_(tonto,self%basis_info_made,"MOL:make_kinetic_overlap ... no basis info")
     call ensure_(tonto,associated(self%atom),"MOL:make_kinetic_overlap ... no atom list")
     do q=1,self%n_shell_pairs
       call get_shell_pair_(self,sh,q,fa,la,fb,lb)
       call get_kei_(sh,T(fa:la,fb:lb), S(fa:la,fb:lb))
       call destroy_ptr_part_(sh)
     end do
     call symmetric_reflect_(S)
     call symmetric_reflect_(T)

   end subroutine

   subroutine make_nddo_kinetic_overlap(self,T,S)
    type(mol_type) :: self
    ! Calculate the NDDO kinetic energy matrix "T" and overlap integral matrix
    ! "S"
     real(kind=kind(1.0d0)), dimension(:,:) :: T,S
     integer(kind=kind(1)) :: q,fa,la,fb,lb,atom_a,atom_b
     type(shell2_type) :: sh

     call ensure_(tonto,self%basis_info_made,"MOL:make_nddo_kinetic_overlap ... no basis info")
     call ensure_(tonto,associated(self%atom),"MOL:make_nddo_kinetic_overlap ... no atom list")
     do q=1,self%n_shell_pairs
       call get_shell_pair_(self,sh,q,fa,la,fb,lb,atom_a,atom_b)
       if (atom_a==atom_b) then; call get_kei_(sh,T(fa:la,fb:lb),S(fa:la,fb:lb))
       else;                     T(fa:la,fb:lb) = 0.0d0; S(fa:la,fb:lb) = 0.0d0
       end if
       call destroy_ptr_part_(sh)
     end do
     call symmetric_reflect_(S)
     call symmetric_reflect_(T)

   end subroutine

   subroutine make_nudo_kinetic_overlap(self,T,S)
    type(mol_type) :: self
    ! Calculate the NUDO kinetic energy matrix "T" and overlap integral matrix
    ! "S"
     real(kind=kind(1.0d0)), dimension(:,:) :: T,S
     integer(kind=kind(1)) :: q,fa,la,fb,lb,atom_a,atom_b
     type(shell2_type) :: sh

     call ensure_(tonto,self%basis_info_made,"MOL:make_nudo_kinetic_overlap ... no basis info")
     call ensure_(tonto,associated(self%atom),"MOL:make_nudo_kinetic_overlap ... no atom list")
     do q=1,self%n_shell_pairs
       call get_shell_pair_(self,sh,q,fa,la,fb,lb,atom_a,atom_b)
       if (bonded_(self%atom,atom_a,atom_b)) then; call get_kei_(sh,T(fa:la,fb:lb),S(fa:la,fb:lb))
       else;                                  T(fa:la,fb:lb) = 0.0d0; S(fa:la,fb:lb) = 0.0d0
       end if
       call destroy_ptr_part_(sh)
     end do
     call symmetric_reflect_(S)
     call symmetric_reflect_(T)

   end subroutine

   subroutine make_group_kinetic_overlap(self,T,S)
    type(mol_type) :: self
    ! Calculate the noninteracting group kinetic energy matrix "T" and overlap
    ! integral matrix "S"
     real(kind=kind(1.0d0)), dimension(:,:) :: T,S
     integer(kind=kind(1)) :: q,fa,la,fb,lb,atom_a,atom_b
     type(shell2_type) :: sh
     type(archive_type) :: arch

     call ensure_(tonto,self%basis_info_made,"MOL:make_group_kinetic_overlap ... no basis info")
     call ensure_(tonto,associated(self%atom),"MOL:make_group_kinetic_overlap ... no atom list")
     call ensure_(tonto,associated(self%atom_group),"MOL:make_group_kinetic_overlap ... no atom_group info")
     do q=1,self%n_shell_pairs
       call get_shell_pair_(self,sh,q,fa,la,fb,lb,atom_a,atom_b)
       if (in_same_atom_group_(self,atom_a,atom_b)) then; call get_kei_(sh,T(fa:la,fb:lb),S(fa:la,fb:lb))
       else;                                         T(fa:la,fb:lb) = 0.0d0; S(fa:la,fb:lb) = 0.0d0
       end if
       call destroy_ptr_part_(sh)
     end do
     call symmetric_reflect_(S)
     call symmetric_reflect_(T)
     call set_(arch,self%name,"kinetic_matrix")
     call write_(arch,T)
     call set_(arch,self%name,"overlap_matrix")
     call write_(arch,S)

   end subroutine

   subroutine get_nuclear_matrix(self,Z)
    type(mol_type) :: self
    ! Set "Z" to the nuclear attraction matrix.
    ! If the archive file exists, read it; otherwise make it.
      real(kind=kind(1.0d0)), dimension(:,:) :: Z
      type(archive_type) :: arch
      logical(kind=kind(.true.)) :: write_archive,calc_it

      call set_(arch,self%name,"nuclear_matrix")
      calc_it = self%scfdata%direct
      write_archive = .not. self%scfdata%direct
      if (.not. calc_it) then
        if (exists_(arch)) then
          call read_(arch,Z)
          calc_it = .false.
          write_archive = .false.
        else
          calc_it = .true.
        end if
      end if
      if (calc_it) then
         if      (self%scfdata%nddo)  then; call make_nddo_nuclear_matrix_(self,Z)
         else if (self%scfdata%nudo)  then; call make_nudo_nuclear_matrix_(self,Z)
         else if (self%scfdata%group) then; call make_group_nuclear_matrix_(self,Z)
         else if (self%scfdata%pie)   then; call make_pie_nuclear_matrix_(self,Z)
         else;                          call make_nuclear_matrix_(self,Z)
         end if
      end if
      if (write_archive) call write_(arch,Z)

   end subroutine

   subroutine make_nuclear_matrix(self,Z)
    type(mol_type) :: self
    ! Calculate the nuclear attraction matrix "Z".
     real(kind=kind(1.0d0)), dimension(:,:) :: Z
     type(atom_type), pointer :: atom1
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: Z_c
     integer(kind=kind(1)) :: q,c,fa,la,fb,lb,start,step
     type(shell2_type) :: sh

     call ensure_(tonto,self%basis_info_made,"MOL:make_nuclear_matrix ... no basis info")
     call ensure_(tonto,associated(self%atom),"MOL:make_nuclear_matrix ... no atom list")
     Z = 0.0d0
     start = 1 + this_proc_(tonto_parallel)
     step = n_proc_(tonto_parallel)
     do q=start,self%n_shell_pairs,step
       call get_shell_pair_(self,sh,q,fa,la,fb,lb)
       call create_(Z_c,sh%a%n_comp,sh%b%n_comp)
       do c = 1,self%n_atom
         atom1 => self%atom(c)
         call get_nuc_(sh,Z_c, mass_(atom1),atom1%pos)
        ! sh.make_nuclear_attraction_ints(Z_c, atom1.pos)
         Z(fa:la,fb:lb) = Z(fa:la,fb:lb) - atom1%atomic_number * Z_c
       end do
       call destroy_(Z_c)
       call destroy_ptr_part_(sh)
     end do
     call sum_symmetric_matrices_(tonto_parallel,Z)
     call symmetric_reflect_(Z)

   end subroutine

   subroutine make_nuclear_matrix_1(self,Z,nuclei)
    type(mol_type) :: self
    ! Calculate the nuclear attraction matrix "Z" only for a specified
    ! list of "nuclei".
     real(kind=kind(1.0d0)), dimension(:,:) :: Z
     integer(kind=kind(1)), dimension(:) :: nuclei
     type(atom_type), pointer :: atom1
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: Z_c
     integer(kind=kind(1)) :: q,c,fa,la,fb,lb
     type(shell2_type) :: sh

     call ensure_(tonto,self%basis_info_made,"MOL:make_nuclear_matrix_1 ... no basis info")
     call ensure_(tonto,associated(self%atom),"MOL:make_nuclear_matrix_1 ... no atom list")
     Z = 0.0d0
     do q = 1,self%n_shell_pairs
       call get_shell_pair_(self,sh,q,fa,la,fb,lb)
       call create_(Z_c,sh%a%n_comp,sh%b%n_comp)
       do c = 1,size(nuclei)
         atom1 => self%atom(nuclei(c))
         call get_nuc_(sh,Z_c, mass_(atom1),atom1%pos)
        ! sh.make_nuclear_attraction_ints(Z_c, atom1.pos)
         Z(fa:la,fb:lb) = Z(fa:la,fb:lb) - atom1%atomic_number * Z_c
       end do
       call destroy_(Z_c)
       call destroy_ptr_part_(sh)
     end do
     call symmetric_reflect_(Z)

   end subroutine

   subroutine make_nddo_nuclear_matrix(self,Z)
    type(mol_type) :: self
    ! Calculate the NDDO nuclear attraction matrix "Z"
     real(kind=kind(1.0d0)), dimension(:,:) :: Z
     type(atom_type), pointer :: atom1
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: Z_c
     integer(kind=kind(1)) :: q,c,fa,la,fb,lb,atom_a,atom_b
     type(shell2_type) :: sh

     call ensure_(tonto,self%basis_info_made,"MOL:make_nddo_nuclear_matrix ... no basis info")
     call ensure_(tonto,associated(self%atom),"MOL:make_nddo_nuclear_matrix ... no atom list")
     Z = 0.0d0
     do q=1,self%n_shell_pairs
       call get_shell_pair_(self,sh,q,fa,la,fb,lb,atom_a,atom_b)
       if (atom_a==atom_b) then
         call create_(Z_c,sh%a%n_comp,sh%b%n_comp)
         do c=1,self%n_atom
           atom1=>self%atom(c)
           call get_nuc_(sh,Z_c, mass_(atom1),atom1%pos)
          ! sh.make_nuclear_attraction_ints(Z_c, atom1.pos)
           Z(fa:la,fb:lb) = Z(fa:la,fb:lb) - atom1%atomic_number * Z_c
         end do
         call destroy_(Z_c)
       end if
       call destroy_ptr_part_(sh)
     end do
     call symmetric_reflect_(Z)

   end subroutine

   subroutine make_nddo_nuclear_matrix_1(self,Z,nuclei)
    type(mol_type) :: self
    ! Calculate the NDDO nuclear attraction matrix "Z" only for a
    ! specified list of nuclei.
     real(kind=kind(1.0d0)), dimension(:,:) :: Z
     integer(kind=kind(1)), dimension(:) :: nuclei
     type(atom_type), pointer :: atom1
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: Z_c
     integer(kind=kind(1)) :: q,c,fa,la,fb,lb,atom_a,atom_b
     type(shell2_type) :: sh

     call ensure_(tonto,self%basis_info_made,"MOL:make_nddo_nuclear_matrix_1 ... no basis info")
     call ensure_(tonto,associated(self%atom),"MOL:make_nddo_nuclear_matrix_1 ... no atom list")
     Z = 0.0d0
     do q=1,self%n_shell_pairs
       call get_shell_pair_(self,sh,q,fa,la,fb,lb,atom_a,atom_b)
       if (atom_a==atom_b) then
         call create_(Z_c,sh%a%n_comp,sh%b%n_comp)
         do c=1,size(nuclei)
           atom1=>self%atom(nuclei(c))
           call get_nuc_(sh,Z_c, mass_(atom1),atom1%pos)
          ! sh.make_nuclear_attraction_ints(Z_c, atom1.pos)
           Z(fa:la,fb:lb) = Z(fa:la,fb:lb) - atom1%atomic_number * Z_c
         end do
         call destroy_(Z_c)
       end if
       call destroy_ptr_part_(sh)
     end do
     call symmetric_reflect_(Z)

   end subroutine

   subroutine make_nudo_nuclear_matrix(self,Z)
    type(mol_type) :: self
    ! Calculate the NUDO nuclear attraction matrix "Z"
     real(kind=kind(1.0d0)), dimension(:,:) :: Z
     type(atom_type), pointer :: atom1
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: Z_c
     integer(kind=kind(1)) :: q,c,fa,la,fb,lb,atom_a,atom_b
     type(shell2_type) :: sh

     call ensure_(tonto,self%basis_info_made,"MOL:make_nudo_nuclear_matrix ... no basis info")
     call ensure_(tonto,associated(self%atom),"MOL:make_nudo_nuclear_matrix ... no atom list")
     Z = 0.0d0
     do q=1,self%n_shell_pairs
       call get_shell_pair_(self,sh,q,fa,la,fb,lb,atom_a,atom_b)
       if (bonded_(self%atom,atom_a,atom_b)) then
         call create_(Z_c,sh%a%n_comp,sh%b%n_comp)
         do c=1,self%n_atom
           atom1=>self%atom(c)
           call get_nuc_(sh,Z_c, mass_(atom1),atom1%pos)
          ! sh.make_nuclear_attraction_ints(Z_c, atom1.pos)
           Z(fa:la,fb:lb) = Z(fa:la,fb:lb) - atom1%atomic_number * Z_c
         end do
         call destroy_(Z_c)
       end if
       call destroy_ptr_part_(sh)
     end do
     call symmetric_reflect_(Z)

   end subroutine

   subroutine make_nudo_nuclear_matrix_1(self,Z,nuclei)
    type(mol_type) :: self
    ! Calculate the NUDO nuclear attraction matrix "Z" only for a specified
    ! list of "nuclei".
     real(kind=kind(1.0d0)), dimension(:,:) :: Z
     integer(kind=kind(1)), dimension(:) :: nuclei
     type(atom_type), pointer :: atom1
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: Z_c
     integer(kind=kind(1)) :: q,c,fa,la,fb,lb,atom_a,atom_b
     type(shell2_type) :: sh

     call ensure_(tonto,self%basis_info_made,"MOL:make_nudo_nuclear_matrix_1 ... no basis info")
     call ensure_(tonto,associated(self%atom),"MOL:make_nudo_nuclear_matrix_1 ... no atom list")
     Z = 0.0d0
     do q = 1,self%n_shell_pairs
       call get_shell_pair_(self,sh,q,fa,la,fb,lb,atom_a,atom_b)
       if (bonded_(self%atom,atom_a,atom_b)) then
         call create_(Z_c,sh%a%n_comp,sh%b%n_comp)
         do c = 1,size(nuclei)
           atom1 => self%atom(nuclei(c))
           call get_nuc_(sh,Z_c, mass_(atom1),atom1%pos)
          ! sh.make_nuclear_attraction_ints(Z_c, atom1.pos)
           Z(fa:la,fb:lb) = Z(fa:la,fb:lb) - atom1%atomic_number * Z_c
         end do
         call destroy_(Z_c)
       end if
       call destroy_ptr_part_(sh)
     end do
     call symmetric_reflect_(Z)

   end subroutine

   subroutine make_group_nuclear_matrix(self,Z)
    type(mol_type) :: self
    ! Calculate the noninteracting group nuclear attraction matrix "Z"
     real(kind=kind(1.0d0)), dimension(:,:) :: Z
     type(atom_type), pointer :: atom1
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: Z_c
     integer(kind=kind(1)) :: q,c,fa,la,fb,lb, atom_a,atom_b, n,g
     type(shell2_type) :: sh

     call ensure_(tonto,self%basis_info_made,"MOL:make_group_nuclear_matrix ... no basis info")
     call ensure_(tonto,associated(self%atom),"MOL:make_group_nuclear_matrix ... no atom list")
     call ensure_(tonto,associated(self%atom_group),"MOL:make_group_nuclear_matrix ... no atom_group info")
     Z = 0.0d0
     do q=1,self%n_shell_pairs
       call get_shell_pair_(self,sh,q,fa,la,fb,lb,atom_a,atom_b)
       if (in_same_atom_group_(self,atom_a,atom_b,g)) then
         call create_(Z_c,sh%a%n_comp,sh%b%n_comp)
         do n = 1,size(self%atom_group(g)%element)
           c = self%atom_group(g)%element(n)
           atom1=>self%atom(c)
           call get_nuc_(sh,Z_c, mass_(atom1),atom1%pos)
          ! sh.make_nuclear_attraction_ints(Z_c, atom1.pos)
           Z(fa:la,fb:lb) = Z(fa:la,fb:lb) - atom1%atomic_number * Z_c
         end do
         call destroy_(Z_c)
       end if
       call destroy_ptr_part_(sh)
     end do
     call symmetric_reflect_(Z)

   end subroutine

   subroutine make_group_nuclear_matrix_1(self,Z,nuclei)
    type(mol_type) :: self
    ! Calculate the noninteracting group nuclear attraction matrix "Z"
    ! only for a specified list of "nuclei".
     real(kind=kind(1.0d0)), dimension(:,:) :: Z
     integer(kind=kind(1)), dimension(:) :: nuclei
     type(atom_type), pointer :: atom1
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: Z_c
     integer(kind=kind(1)) :: q,c,fa,la,fb,lb, atom_a,atom_b, n,g
     type(shell2_type) :: sh

     call ensure_(tonto,self%basis_info_made,"MOL:make_group_nuclear_matrix_1 ... no basis info")
     call ensure_(tonto,associated(self%atom),"MOL:make_group_nuclear_matrix_1 ... no atom list")
     call ensure_(tonto,associated(self%atom_group),"MOL:make_group_nuclear_matrix_1 ... no atom_group info")
     Z = 0.0d0
     do q=1,self%n_shell_pairs
       call get_shell_pair_(self,sh,q,fa,la,fb,lb,atom_a,atom_b)
       if (in_same_atom_group_(self,atom_a,atom_b,g)) then
         call create_(Z_c,sh%a%n_comp,sh%b%n_comp)
         do n = 1,size(self%atom_group(g)%element)
           c = self%atom_group(g)%element(n)
           if (all(nuclei/=c)) cycle
           atom1=>self%atom(c)
           call get_nuc_(sh,Z_c, mass_(atom1),atom1%pos)
          ! sh.make_nuclear_attraction_ints(Z_c, atom1.pos)
           Z(fa:la,fb:lb) = Z(fa:la,fb:lb) - atom1%atomic_number * Z_c
         end do
         call destroy_(Z_c)
       end if
       call destroy_ptr_part_(sh)
     end do
     call symmetric_reflect_(Z)

   end subroutine

   subroutine make_pie_nuclear_matrix(self,Z)
    type(mol_type) :: self
    ! Calculate the PIE nuclear attraction matrix "Z". In this scheme the three
    ! center nuclear attraction integrals are approximated by a diatom projective
    ! scheme. See Mayer, CPL 332, 381 (2000).
     real(kind=kind(1.0d0)), dimension(:,:) :: Z
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: Z_c,S,S_a_c,S_inv,S_sub
     integer(kind=kind(1)) :: a,c,na,nc,nac,fc,lc
     integer(kind=kind(1)), dimension(2) :: ac

     call ensure_(tonto,self%basis_info_made,"MOL:make_pie_nuclear_matrix ... no basis info")
     call ensure_(tonto,associated(self%atom),"MOL:make_pie_nuclear_matrix ... no atom list")
     call create_(S,self%n_bf,self%n_bf)
     call get_overlap_matrix_(self,S)
     Z = 0.0d0
     do c = 1,self%n_atom  ! loop over nuclear attraction centres
     do a = 1,self%n_atom  ! loop over bra
        na  = n_bf_(self%atom(a))
        nc  = n_bf_(self%atom(c))
        nac = na + nc
        call create_(Z_c,nac,nac)
        ac = (/a,c/)
        call make_nuclear_matrix_(self%atom(ac),Z_c,nuclei=(/2/))
        call create_(S_inv,nac,nac)
        call create_(S_a_c,nac,nac)
        call AO_subspace_set_(self,S_a_c,S,ac,ac)
        call to_inverse_of_(S_inv,S_a_c)
        call destroy_(S_a_c)
        call create_(S_sub,self%n_bf,nac)
        call AO_subspace_set_(self,S_sub,S,col_atom=ac)
        S_inv = matmul(S_inv,Z_c)
        fc = self%first_basis_fn_for_atom(c)
        lc =  self%last_basis_fn_for_atom(c)
        Z(:,fc:lc) = Z(:,fc:lc) + matmul(S_sub,S_inv)
        call destroy_(S_inv); call destroy_(S_sub);
        call destroy_(Z_c)
     end do
     end do
     call symmetrize_(Z)
     call destroy_(S)

   end subroutine

   subroutine get_core_matrix(self,H,nuclei)
    type(mol_type) :: self
    ! Get the core hamiltonian matrix. If already in memory, do nothing. Else if
    ! the archive file exists, read it. Otherwise make it.
    ! If the nuclei are given make it regardless, since the stored one is
    ! likely to be different
      real(kind=kind(1.0d0)), dimension(:,:) :: H
      integer(kind=kind(1)), dimension(:), optional :: nuclei
      type(archive_type) :: arch
      logical(kind=kind(.true.)) :: write_archive,calc_it

      call set_(arch,self%name,"core_matrix")
      calc_it = self%scfdata%direct .or. present(nuclei)
      write_archive = .not. self%scfdata%direct
      if (.not. calc_it) then
        if (exists_(arch)) then
          call read_(arch,H)
          calc_it = .false.
          write_archive = .false.
        else
          calc_it = .true.
        end if
      end if
      if (calc_it) call make_core_matrix_(self,H,nuclei)
      if (write_archive) call write_(arch,H)

   end subroutine

   subroutine make_core_matrix(self,H,nuclei)
    type(mol_type) :: self
    ! Make the core hamiltonian matrix "H".
      real(kind=kind(1.0d0)), dimension(:,:) :: H
      integer(kind=kind(1)), dimension(:), optional :: nuclei
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: Z

      call make_kinetic_matrix_(self,H)
      call create_(Z,self%n_bf, self%n_bf)
      if (present(nuclei)) then
        call make_nuclear_matrix_(self,Z,nuclei)
      else
        call make_nuclear_matrix_(self,Z)
      end if
      H = H + Z
      call destroy_(Z)

   end subroutine

   subroutine get_dipole_matrices(self,Dx,Dy,Dz)
    type(mol_type) :: self
    ! Set "Di" to the dipole moment integral matrices.
    ! If archives exist, read them; otherwise make them.
      real(kind=kind(1.0d0)), dimension(:,:) :: Dx,Dy,Dz
      type(archive_type) :: x_archive,y_archive,z_archive

      call set_(x_archive,self%name,"dipole_x_matrix")
      call set_(y_archive,self%name,"dipole_y_matrix")
      call set_(z_archive,self%name,"dipole_z_matrix")
      if (exists_(x_archive)) then
         call read_(x_archive,Dx); call read_(y_archive,Dy); call read_(z_archive,Dz)
      else
         call make_dipole_matrices_(self,Dx,Dy,Dz)
      end if

   end subroutine

   subroutine make_dipole_matrices(self,Dx,Dy,Dz)
    type(mol_type) :: self
    ! Make "Di", the dipole moment integral matrices.
      real(kind=kind(1.0d0)), dimension(:,:) :: Dx,Dy,Dz
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: DDx,DDy,DDz
      integer(kind=kind(1)) :: q,fa,la,fb,lb
      type(shell2_type) :: sh
      type(archive_type) :: arch
      integer(kind=kind(1)) :: n_a,n_b

      call ensure_(tonto,self%basis_info_made,"MOL:make_dipole_matrices ... no basis info")
      call ensure_(tonto,associated(self%atom),"MOL:make_dipole_matrices ... no atom list")
      Dx = 0.0d0; Dy = 0.0d0; Dz = 0.0d0
      do q=1,self%n_shell_pairs
         call get_shell_pair_(self,sh,q,fa,la,fb,lb)
         n_a = sh%a%n_comp; n_b = sh%b%n_comp
         call create_(DDx,n_a,n_b); call create_(DDy,n_a,n_b); call create_(DDz,n_a,n_b)
         call make_dipole_ints_(sh,DDx,DDy,DDz, self%gauge_origin)
         Dx(fa:la,fb:lb) = Dx(fa:la,fb:lb) + DDx
         Dy(fa:la,fb:lb) = Dy(fa:la,fb:lb) + DDy
         Dz(fa:la,fb:lb) = Dz(fa:la,fb:lb) + DDz
         call destroy_ptr_part_(sh)
         call destroy_(DDz); call destroy_(DDy); call destroy_(DDx)
      end do
      call symmetric_reflect_(Dx); call symmetric_reflect_(Dy); call symmetric_reflect_(Dz)
      call set_(arch,self%name,"dipole_x_matrix"); call write_(arch,Dx)
      call set_(arch,self%name,"dipole_y_matrix"); call write_(arch,Dy)
      call set_(arch,self%name,"dipole_z_matrix"); call write_(arch,Dz)

   end subroutine

   function electronic_dipole_moment(self) result(res)
    type(mol_type) :: self
    ! Return the electronic dipole moment
      real(kind=kind(1.0d0)), dimension(3) :: res
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: Dx,Dy,Dz

      call create_(Dx,self%n_bf,self%n_bf)
      call create_(Dy,self%n_bf,self%n_bf)
      call create_(Dz,self%n_bf,self%n_bf)
      call get_dipole_matrices_(self,Dx,Dy,Dz)
      res(1) = -expectation_(self,Dx)  ! Minus sign for electron charge
      res(2) = -expectation_(self,Dy)
      res(3) = -expectation_(self,Dz)
      call destroy_(Dz); call destroy_(Dy); call destroy_(Dx)

   end function

   function nuclear_dipole_moment(self) result(res)
    type(mol_type) :: self
    ! Return the nuclear dipole moment
      real(kind=kind(1.0d0)), dimension(3) :: res

      res = nuclear_dipole_moment_(self%atom)

   end function

   subroutine get_quadrupole_matrices(self,Qxx,Qyy,Qzz,Qxy,Qxz,Qyz)
    type(mol_type) :: self
    ! Set "Qij" to the quadrupole moment integral matrices.
    ! If archives exist, read them; otherwise make them.
      real(kind=kind(1.0d0)), dimension(:,:) :: Qxx,Qyy,Qzz,Qxy,Qxz,Qyz
      type(archive_type) :: xx_archive,yy_archive,zz_archive
      type(archive_type) :: xy_archive,xz_archive,yz_archive

      call set_(xx_archive,self%name,"quadrupole_xx_matrix")
      call set_(yy_archive,self%name,"quadrupole_yy_matrix")
      call set_(zz_archive,self%name,"quadrupole_zz_matrix")
      call set_(xy_archive,self%name,"quadrupole_xy_matrix")
      call set_(xz_archive,self%name,"quadrupole_xz_matrix")
      call set_(yz_archive,self%name,"quadrupole_yz_matrix")
      if (exists_(xx_archive)) then
         call read_(xx_archive,Qxx); call read_(yy_archive,Qyy); call read_(zz_archive,Qzz)
         call read_(xy_archive,Qxy); call read_(xz_archive,Qxz); call read_(yz_archive,Qyz)
      else
         call make_quadrupole_matrices_(self,Qxx,Qyy,Qzz,Qxy,Qxz,Qyz)
      end if

   end subroutine

   subroutine make_quadrupole_matrices(self,Qxx,Qyy,Qzz,Qxy,Qxz,Qyz)
    type(mol_type) :: self
    ! Make "Qij", the quadrupole moment integral matrices.
      real(kind=kind(1.0d0)), dimension(:,:) :: Qxx,Qyy,Qzz,Qxy,Qxz,Qyz
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: QQxx,QQyy,QQzz,QQxy,QQxz,QQyz
      integer(kind=kind(1)) :: q,fa,la,fb,lb
      type(shell2_type) :: sh
      type(archive_type) :: arch
      integer(kind=kind(1)) :: n_a,n_b

      call ensure_(tonto,self%basis_info_made,"MOL:make_quadrupole_matrices ... no basis info")
      call ensure_(tonto,associated(self%atom),"MOL:make_quadrupole_matrices ... no atom list")
      Qxx = 0.0d0; Qyy = 0.0d0; Qzz = 0.0d0
      Qxy = 0.0d0; Qxz = 0.0d0; Qyz = 0.0d0
      do q=1,self%n_shell_pairs
         call get_shell_pair_(self,sh,q,fa,la,fb,lb)
         n_a = sh%a%n_comp; n_b = sh%b%n_comp
         call create_(QQxx,n_a,n_b); call create_(QQyy,n_a,n_b); call create_(QQzz,n_a,n_b)
         call create_(QQxy,n_a,n_b); call create_(QQxz,n_a,n_b); call create_(QQyz,n_a,n_b)
         call make_quadrupole_ints_(sh,QQxx,QQyy,QQzz,QQxy,QQxz,QQyz, self%gauge_origin)
         Qxx(fa:la,fb:lb) = Qxx(fa:la,fb:lb) + QQxx
         Qyy(fa:la,fb:lb) = Qyy(fa:la,fb:lb) + QQyy
         Qzz(fa:la,fb:lb) = Qzz(fa:la,fb:lb) + QQzz
         Qxy(fa:la,fb:lb) = Qxy(fa:la,fb:lb) + QQxy
         Qxz(fa:la,fb:lb) = Qxz(fa:la,fb:lb) + QQxz
         Qyz(fa:la,fb:lb) = Qyz(fa:la,fb:lb) + QQyz
         call destroy_ptr_part_(sh)
         call destroy_(QQyz); call destroy_(QQxz); call destroy_(QQxy)
         call destroy_(QQzz); call destroy_(QQyy); call destroy_(QQxx)
      end do
      call symmetric_reflect_(Qxx); call symmetric_reflect_(Qyy); call symmetric_reflect_(Qzz)
      call symmetric_reflect_(Qxy); call symmetric_reflect_(Qxz); call symmetric_reflect_(Qyz)
      call set_(arch,self%name,"quadrupole_xx_matrix"); call write_(arch,Qxx)
      call set_(arch,self%name,"quadrupole_yy_matrix"); call write_(arch,Qyy)
      call set_(arch,self%name,"quadrupole_zz_matrix"); call write_(arch,Qzz)
      call set_(arch,self%name,"quadrupole_xy_matrix"); call write_(arch,Qxy)
      call set_(arch,self%name,"quadrupole_xz_matrix"); call write_(arch,Qxz)
      call set_(arch,self%name,"quadrupole_yz_matrix"); call write_(arch,Qyz)

   end subroutine

   function electronic_quadrupole_moment(self) result(res)
    type(mol_type) :: self
    ! Return the electronic quadrupole moments as a vectors in the
    ! order: xx, yy, zz, xy, xz, yz
      real(kind=kind(1.0d0)), dimension(6) :: res
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: Qxx,Qyy,Qzz,Qxy,Qxz,Qyz

      call create_(Qxx,self%n_bf,self%n_bf)
      call create_(Qyy,self%n_bf,self%n_bf)
      call create_(Qzz,self%n_bf,self%n_bf)
      call create_(Qxy,self%n_bf,self%n_bf)
      call create_(Qxz,self%n_bf,self%n_bf)
      call create_(Qyz,self%n_bf,self%n_bf)
      call get_quadrupole_matrices_(self,Qxx,Qyy,Qzz,Qxy,Qxz,Qyz)
      res(1) = -expectation_(self,Qxx)  ! Minus sign for electron charge
      res(2) = -expectation_(self,Qyy)
      res(3) = -expectation_(self,Qzz)
      res(4) = -expectation_(self,Qxy)
      res(5) = -expectation_(self,Qxz)
      res(6) = -expectation_(self,Qyz)
      call destroy_(Qyz); call destroy_(Qxz); call destroy_(Qxy)
      call destroy_(Qzz); call destroy_(Qyy); call destroy_(Qxx)

   end function

   function nuclear_quadrupole_moment(self) result(res)
    type(mol_type) :: self
    ! Return the nuclear quadrupole moment
      real(kind=kind(1.0d0)), dimension(6) :: res

      res = nuclear_quadrupole_moment_(self%atom)

   end function

   subroutine get_octupole_matrices(self,Oxxx,Oyyy,Ozzz,Oxxy,Oxxz,Oyyx,Oyyz,Ozzx,Ozzy,Oxyz)
    type(mol_type) :: self
    ! Set "Oijk" to the quadrupole moment integral matrices.
    ! If archives exist, read them; otherwise make them.
      real(kind=kind(1.0d0)), dimension(:,:) :: Oxxx,Oyyy,Ozzz,Oxxy,Oxxz,Oyyx,Oyyz,Ozzx,Ozzy,Oxyz
      type(archive_type) :: xxx,yyy,zzz,xxy,xxz,yyx,yyz,zzx,zzy,xyz

      call set_(xxx,self%name,"octupole_xxx_matrix")
      call set_(yyy,self%name,"octupole_yyy_matrix")
      call set_(zzz,self%name,"octupole_zzz_matrix")
      call set_(xxy,self%name,"octupole_xxy_matrix")
      call set_(xxz,self%name,"octupole_xxz_matrix")
      call set_(yyx,self%name,"octupole_yyx_matrix")
      call set_(yyz,self%name,"octupole_yyz_matrix")
      call set_(zzx,self%name,"octupole_zzx_matrix")
      call set_(zzy,self%name,"octupole_zzy_matrix")
      call set_(xyz,self%name,"octupole_xyz_matrix")
      if (exists_(xxx)) then
         call read_(xxx,Oxxx); call read_(yyy,Oyyy); call read_(zzz,Ozzz)
         call read_(xxy,Oxxy); call read_(xxz,Oxxz)
         call read_(yyx,Oyyx); call read_(yyz,Oyyz)
         call read_(zzx,Ozzx); call read_(zzy,Ozzy)
         call read_(xyz,Oxyz)
      else
         call make_octupole_matrices_(self,Oxxx,Oyyy,Ozzz,Oxxy,Oxxz,Oyyx,Oyyz,Ozzx,Ozzy,Oxyz)
      end if

   end subroutine

   subroutine make_octupole_matrices(self,Oxxx,Oyyy,Ozzz,Oxxy,Oxxz,Oyyx,Oyyz,Ozzx,Ozzy,Oxyz)
    type(mol_type) :: self
    ! Make "Oijk", the octupole moment integral matrices.
      real(kind=kind(1.0d0)), dimension(:,:) :: Oxxx,Oyyy,Ozzz,Oxxy,Oxxz,Oyyx,Oyyz,Ozzx,Ozzy,Oxyz
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: OOxxx,OOyyy,OOzzz,OOxxy,OOxxz,OOyyx,OOyyz,OOzzx,OOzzy,OOxyz
      integer(kind=kind(1)) :: q,fa,la,fb,lb
      type(shell2_type) :: sh
      type(archive_type) :: arch
      integer(kind=kind(1)) :: n_a,n_b

      call ensure_(tonto,self%basis_info_made,"MOL:make_octupole_matrices ... no basis info")
      call ensure_(tonto,associated(self%atom),"MOL:make_octupole_matrices ... no atom list")
      Oxxx = 0.0d0; Oyyy = 0.0d0; Ozzz = 0.0d0
      Oxxy = 0.0d0; Oxxz = 0.0d0
      Oyyx = 0.0d0; Oyyz = 0.0d0
      Ozzx = 0.0d0; Ozzy = 0.0d0
      Oxyz = 0.0d0
      do q = 1,self%n_shell_pairs
         call get_shell_pair_(self,sh,q,fa,la,fb,lb)
         n_a = sh%a%n_comp; n_b = sh%b%n_comp
         call create_(OOxxx,n_a,n_b); call create_(OOyyy,n_a,n_b); call create_(OOzzz,n_a,n_b)
         call create_(OOxxy,n_a,n_b); call create_(OOxxz,n_a,n_b)
         call create_(OOyyx,n_a,n_b); call create_(OOyyz,n_a,n_b)
         call create_(OOzzx,n_a,n_b); call create_(OOzzy,n_a,n_b)
         call create_(OOxyz,n_a,n_b)
         call make_octupole_ints_(sh,OOxxx,OOyyy,OOzzz,OOxxy,OOxxz,OOyyx,OOyyz,OOzzx,OOzzy,OOxyz,self%gauge_origin)
         Oxxx(fa:la,fb:lb) = Oxxx(fa:la,fb:lb) + OOxxx
         Oyyy(fa:la,fb:lb) = Oyyy(fa:la,fb:lb) + OOyyy
         Ozzz(fa:la,fb:lb) = Ozzz(fa:la,fb:lb) + OOzzz
         Oxxy(fa:la,fb:lb) = Oxxy(fa:la,fb:lb) + OOxxy
         Oxxz(fa:la,fb:lb) = Oxxz(fa:la,fb:lb) + OOxxz
         Oyyx(fa:la,fb:lb) = Oyyx(fa:la,fb:lb) + OOyyx
         Oyyz(fa:la,fb:lb) = Oyyz(fa:la,fb:lb) + OOyyz
         Ozzx(fa:la,fb:lb) = Ozzx(fa:la,fb:lb) + OOzzx
         Ozzy(fa:la,fb:lb) = Ozzy(fa:la,fb:lb) + OOzzy
         Oxyz(fa:la,fb:lb) = Oxyz(fa:la,fb:lb) + OOxyz
         call destroy_ptr_part_(sh)
         call destroy_(OOxyz)
         call destroy_(OOzzy); call destroy_(OOzzx)
         call destroy_(OOyyz); call destroy_(OOyyx)
         call destroy_(OOxxz); call destroy_(OOxxy)
         call destroy_(OOzzz); call destroy_(OOyyy); call destroy_(OOxxx)
      end do
      call symmetric_reflect_(Oxxx); call symmetric_reflect_(Oyyy); call symmetric_reflect_(Ozzz)
      call symmetric_reflect_(Oxxy); call symmetric_reflect_(Oxxz)
      call symmetric_reflect_(Oyyx); call symmetric_reflect_(Oyyz)
      call symmetric_reflect_(Ozzx); call symmetric_reflect_(Ozzy)
      call symmetric_reflect_(Oxyz)
      call set_(arch,self%name,"octupole_xxx_matrix"); call write_(arch,Oxxx)
      call set_(arch,self%name,"octupole_yyy_matrix"); call write_(arch,Oyyy)
      call set_(arch,self%name,"octupole_zzz_matrix"); call write_(arch,Ozzz)
      call set_(arch,self%name,"octupole_xxy_matrix"); call write_(arch,Oxxy)
      call set_(arch,self%name,"octupole_xxz_matrix"); call write_(arch,Oxxz)
      call set_(arch,self%name,"octupole_yyx_matrix"); call write_(arch,Oyyx)
      call set_(arch,self%name,"octupole_yyz_matrix"); call write_(arch,Oyyz)
      call set_(arch,self%name,"octupole_zzx_matrix"); call write_(arch,Ozzx)
      call set_(arch,self%name,"octupole_zzy_matrix"); call write_(arch,Ozzy)
      call set_(arch,self%name,"octupole_xyz_matrix"); call write_(arch,Oxyz)

   end subroutine

   function electronic_octupole_moment(self) result(res)
    type(mol_type) :: self
    ! Return the electronic octupole moments as a vectors in the
    ! order: xxx, yyy, zzz, xxy, xxz, yyx, yyz, zzx, zzy, xyz
      real(kind=kind(1.0d0)), dimension(10) :: res
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: Oxxx,Oyyy,Ozzz,Oxxy,Oxxz,Oyyx,Oyyz,Ozzx,Ozzy,Oxyz

      call create_(Oxxx,self%n_bf,self%n_bf)
      call create_(Oyyy,self%n_bf,self%n_bf)
      call create_(Ozzz,self%n_bf,self%n_bf)
      call create_(Oxxy,self%n_bf,self%n_bf)
      call create_(Oxxz,self%n_bf,self%n_bf)
      call create_(Oyyx,self%n_bf,self%n_bf)
      call create_(Oyyz,self%n_bf,self%n_bf)
      call create_(Ozzx,self%n_bf,self%n_bf)
      call create_(Ozzy,self%n_bf,self%n_bf)
      call create_(Oxyz,self%n_bf,self%n_bf)
      call get_octupole_matrices_(self,Oxxx,Oyyy,Ozzz,Oxxy,Oxxz,Oyyx,Oyyz,Ozzx,Ozzy,Oxyz)
      res(1)  = -expectation_(self,Oxxx)  ! Minus sign for electron charge
      res(2)  = -expectation_(self,Oyyy)
      res(3)  = -expectation_(self,Ozzz)
      res(4)  = -expectation_(self,Oxxy)
      res(5)  = -expectation_(self,Oxxz)
      res(6)  = -expectation_(self,Oyyx)
      res(7)  = -expectation_(self,Oyyz)
      res(8)  = -expectation_(self,Ozzx)
      res(9)  = -expectation_(self,Ozzy)
      res(10) = -expectation_(self,Oxyz)
      call destroy_(Oxyz)
      call destroy_(Ozzy); call destroy_(Ozzx)
      call destroy_(Oyyz); call destroy_(Oyyx)
      call destroy_(Oxxz); call destroy_(Oxxy)
      call destroy_(Ozzz); call destroy_(Oyyy); call destroy_(Oxxx)

   end function

   function nuclear_octupole_moment(self) result(res)
    type(mol_type) :: self
    ! Return the nuclear octupole moment
      real(kind=kind(1.0d0)), dimension(10) :: res

      res = nuclear_octupole_moment_(self%atom)

   end function

   subroutine make_electric_field_matrices(self,Ex,Ey,Ez,c)
    type(mol_type) :: self
    ! Make "Ei", the electric fields integral matrices evaluated at
    ! position "c".
      real(kind=kind(1.0d0)), dimension(:,:) :: Ex,Ey,Ez
       real(kind=kind(1.0d0)), dimension(3) :: c
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: EEx,EEy,EEz
      integer(kind=kind(1)) :: q,fa,la,fb,lb
      type(shell2_type) :: sh
      integer(kind=kind(1)) :: n_a,n_b

      call ensure_(tonto,self%basis_info_made,"MOL:make_electric_field_matrices ... no basis info")
      call ensure_(tonto,associated(self%atom),"MOL:make_electric_field_matrices ... no atom list")
      Ex = 0.0d0; Ey = 0.0d0; Ez = 0.0d0
      do q=1,self%n_shell_pairs
         call get_shell_pair_(self,sh,q,fa,la,fb,lb)
         n_a = sh%a%n_comp; n_b = sh%b%n_comp
         call create_(EEx,n_a,n_b); call create_(EEy,n_a,n_b); call create_(EEz,n_a,n_b)
         call make_E_field_ints_(sh,EEx,EEy,EEz,c)
         Ex(fa:la,fb:lb) = Ex(fa:la,fb:lb) + EEx
         Ey(fa:la,fb:lb) = Ey(fa:la,fb:lb) + EEy
         Ez(fa:la,fb:lb) = Ez(fa:la,fb:lb) + EEz
         call destroy_ptr_part_(sh)
         call destroy_(EEz); call destroy_(EEy); call destroy_(EEx)
      end do
      call symmetric_reflect_(Ex); call symmetric_reflect_(Ey); call symmetric_reflect_(Ez)

   end subroutine

   function electronic_E_field_at_nuclei(self) result(res)
    type(mol_type) :: self
    ! Return the electronic contribution to the electric fields
    ! at the nuclei as a (3 x .n_atom) array
      real(kind=kind(1.0d0)), dimension(3,self%n_atom) :: res
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: Ex,Ey,Ez
       integer(kind=kind(1)) :: a

      call create_(Ex,self%n_bf,self%n_bf)
      call create_(Ey,self%n_bf,self%n_bf)
      call create_(Ez,self%n_bf,self%n_bf)
      do a = 1,self%n_atom
         call make_electric_field_matrices_(self,Ex,Ey,Ez,self%atom(a)%pos)
         res(1,a) = -expectation_(self,Ex)
         res(2,a) = -expectation_(self,Ey)
         res(3,a) = -expectation_(self,Ez)
      end do
      call destroy_(Ez); call destroy_(Ey); call destroy_(Ex)

   end function

   function nuclear_E_field_at_nuclei(self) result(res)
    type(mol_type) :: self
    ! Return the nuclear contribution to the electric fields
    ! at the nuclei as a (3 x .n_atom) array
      real(kind=kind(1.0d0)), dimension(3,self%n_atom) :: res

      res = nuclear_E_field_at_nuclei_(self%atom)

   end function

   subroutine make_EFG_matrices(self,Exx,Eyy,Ezz,Exy,Exz,Eyz,c)
    type(mol_type) :: self
    ! Make "Eij", the electric field gradient (EFG) integral matrices evaluated
    ! at point "c".
      real(kind=kind(1.0d0)), dimension(:,:) :: Exx,Eyy,Ezz,Exy,Exz,Eyz
       real(kind=kind(1.0d0)), dimension(3) :: c
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: EExx,EEyy,EEzz,EExy,EExz,EEyz
      integer(kind=kind(1)) :: q,fa,la,fb,lb
      type(shell2_type) :: sh
      integer(kind=kind(1)) :: n_a,n_b

   call ensure_(tonto,self%basis_info_made,"MOL:make_EFG_matrices ... no basis info")
   call ensure_(tonto,associated(self%atom),"MOL:make_EFG_matrices ... no atom list")
      Exx = 0.0d0; Eyy = 0.0d0; Ezz = 0.0d0
      Exy = 0.0d0; Exz = 0.0d0; Eyz = 0.0d0
      do q=1,self%n_shell_pairs
         call get_shell_pair_(self,sh,q,fa,la,fb,lb)
         n_a = sh%a%n_comp; n_b = sh%b%n_comp
         call create_(EExx,n_a,n_b); call create_(EEyy,n_a,n_b); call create_(EEzz,n_a,n_b)
         call create_(EExy,n_a,n_b); call create_(EExz,n_a,n_b); call create_(EEyz,n_a,n_b)
         call make_e_gradient_ints_(sh,EExx,EEyy,EEzz,EExy,EExz,EEyz,c)
         Exx(fa:la,fb:lb) = Exx(fa:la,fb:lb) + EExx
         Eyy(fa:la,fb:lb) = Eyy(fa:la,fb:lb) + EEyy
         Ezz(fa:la,fb:lb) = Ezz(fa:la,fb:lb) + EEzz
         Exy(fa:la,fb:lb) = Exy(fa:la,fb:lb) + EExy
         Exz(fa:la,fb:lb) = Exz(fa:la,fb:lb) + EExz
         Eyz(fa:la,fb:lb) = Eyz(fa:la,fb:lb) + EEyz
         call destroy_ptr_part_(sh)
         call destroy_(EEyz); call destroy_(EExz); call destroy_(EExy)
         call destroy_(EEzz); call destroy_(EEyy); call destroy_(EExx)
      end do
      call symmetric_reflect_(Exx); call symmetric_reflect_(Eyy); call symmetric_reflect_(Ezz)
      call symmetric_reflect_(Exy); call symmetric_reflect_(Exz); call symmetric_reflect_(Eyz)

   end subroutine

   function electronic_EFG_at_nuclei(self) result(res)
    type(mol_type) :: self
    ! Return the electronic contribution to the electric field gradient
    ! (EFG) at the nuclei as a (6 x .n_atom) array. The order of the
    ! electric field gradient elements is: xx, yy, zz, xy, xz, yz
      real(kind=kind(1.0d0)), dimension(6,self%n_atom) :: res
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: Exx,Eyy,Ezz,Exy,Exz,Eyz
       integer(kind=kind(1)) :: a

      call create_(Exx,self%n_bf,self%n_bf)
      call create_(Eyy,self%n_bf,self%n_bf)
      call create_(Ezz,self%n_bf,self%n_bf)
      call create_(Exy,self%n_bf,self%n_bf)
      call create_(Exz,self%n_bf,self%n_bf)
      call create_(Eyz,self%n_bf,self%n_bf)
      do a = 1,self%n_atom
         call make_EFG_matrices_(self,Exx,Eyy,Ezz,Exy,Exz,Eyz,self%atom(a)%pos)
         res(1,a) = -expectation_(self,Exx)
         res(2,a) = -expectation_(self,Eyy)
         res(3,a) = -expectation_(self,Ezz)
         res(4,a) = -expectation_(self,Exy)
         res(5,a) = -expectation_(self,Exz)
         res(6,a) = -expectation_(self,Eyz)
      end do
      call destroy_(Eyz); call destroy_(Exz); call destroy_(Exy)
      call destroy_(Ezz); call destroy_(Eyy); call destroy_(Exx)

   end function

   function nuclear_EFG_at_nuclei(self) result(res)
    type(mol_type) :: self
    ! Return the nuclear contribution to the electric fields gradient (EFG)
    ! at the nuclei as a (6 x .n_atom) array
      real(kind=kind(1.0d0)), dimension(6,self%n_atom) :: res

      res = nuclear_EFG_at_nuclei_(self%atom)

   end function

   subroutine get_spin_orbit_matrices(self,SOx,SOy,SOz)
    type(mol_type) :: self
    ! Set "SOx", "SOy", and "SOz" to the one electron spin orbit matrices.
    ! If archives exist, read them; otherwise make them.
      real(kind=kind(1.0d0)), dimension(:,:) :: SOx,SOy,SOz
      type(archive_type) :: SOx_archive,SOy_archive,SOz_archive

      call set_(SOx_archive,self%name,"SOx_matrix")
      call set_(SOy_archive,self%name,"SOy_matrix")
      call set_(SOz_archive,self%name,"SOz_matrix")
      if (exists_(SOx_archive) .and. exists_(SOy_archive) .and. exists_(SOz_archive)) then
         call read_(SOx_archive,SOx)
         call read_(SOy_archive,SOy)
         call read_(SOz_archive,SOz)
      else
         call make_spin_orbit_matrices_(self,SOx,SOy,SOz)
      end if

   end subroutine

   subroutine make_spin_orbit_matrices(self,SOx,SOy,SOz)
    type(mol_type) :: self
    ! Calculate the spin orbit matrices "SOx" "SOy" and "SOz"
      real(kind=kind(1.0d0)), dimension(:,:) :: SOx,SOy,SOz
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: SOx_c,SOy_c,SOz_c
      integer(kind=kind(1)) :: q,c,fa,la,fb,lb
       real(kind=kind(1.0d0)) :: Z
      type(shell2_type) :: sh
      type(archive_type) :: arch

      call ensure_(tonto,self%basis_info_made,"MOL:make_spin_orbit_matrices ... no basis info")
      call ensure_(tonto,associated(self%atom),"MOL:make_spin_orbit_matrices ... no atom list")
      SOx = 0.0d0; SOy = 0.0d0; SOz = 0.0d0
      do q=1,self%n_shell_pairs
         call get_shell_pair_(self,sh,q,fa,la,fb,lb)
         call create_(SOx_c,sh%a%n_comp,sh%b%n_comp)
         call create_(SOy_c,sh%a%n_comp,sh%b%n_comp)
         call create_(SOz_c,sh%a%n_comp,sh%b%n_comp)
         do c = 1,self%n_atom
            Z = self%atom(c)%atomic_number
            call make_spin_orbit_ints_(sh,SOx_c,SOy_c,SOz_c, self%atom(c)%pos)
            SOx(fa:la,fb:lb) = SOx(fa:la,fb:lb) + Z*SOx_c
            SOy(fa:la,fb:lb) = SOy(fa:la,fb:lb) + Z*SOy_c
            SOz(fa:la,fb:lb) = SOz(fa:la,fb:lb) + Z*SOz_c
         end do
         call destroy_ptr_part_(sh)
         call destroy_(SOz_c); call destroy_(SOy_c); call destroy_(SOx_c)
      end do
      call antisymmetric_reflect_(SOx)
      call antisymmetric_reflect_(SOy)
      call antisymmetric_reflect_(SOz)
      call set_(arch,self%name,"SOx_matrix"); call write_(arch,SOx)
      call set_(arch,self%name,"SOy_matrix"); call write_(arch,SOy)
      call set_(arch,self%name,"SOz_matrix"); call write_(arch,SOz)

   end subroutine

   subroutine get_spin_orbit_B_matrices(self,SOBx,SOBy,SOBz)
    type(mol_type) :: self
    ! Set "SOBi" to the gauge modified (B field) one electron spin orbit
    ! matrices.  If archives exist, read them; otherwise make them.
      real(kind=kind(1.0d0)), dimension(:,:) :: SOBx,SOBy,SOBz
      type(archive_type) :: x_archive,y_archive,z_archive

      call set_(x_archive,self%name,"SOBx_matrix")
      call set_(y_archive,self%name,"SOBy_matrix")
      call set_(z_archive,self%name,"SOBz_matrix")
      if (exists_(x_archive)) then
         call read_(x_archive,SOBx); call read_(y_archive,SOBy); call read_(z_archive,SOBz)
      else
         call make_spin_orbit_B_matrices_(self,SOBx,SOBy,SOBz)
      end if

   end subroutine

   subroutine make_spin_orbit_B_matrices(self,SOBx,SOBy,SOBz)
    type(mol_type) :: self
    ! Calculate the gauge modified (B field) spin orbit matrices "SOBx" "SOBy"
    ! and "SOBz"
      real(kind=kind(1.0d0)), dimension(:,:) :: SOBx,SOBy,SOBz
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: Qxx,Qxy,Qxz,Qyx,Qyy,Qyz,Qzx,Qzy,Qzz
      integer(kind=kind(1)) :: n_bf
      type(archive_type) :: arch

   call ensure_(tonto,self%basis_info_made,"MOL:make_spin_orbit_B_matrices ... no basis info")
   call ensure_(tonto,associated(self%atom),"MOL:make_spin_orbit_B_matrices ... no atom list")
      n_bf = self%n_bf
      call create_(Qxx,n_bf,n_bf); call create_(Qxy,n_bf,n_bf); call create_(Qxz,n_bf,n_bf)
      call create_(Qyx,n_bf,n_bf); call create_(Qyy,n_bf,n_bf); call create_(Qyz,n_bf,n_bf)
      call create_(Qzx,n_bf,n_bf); call create_(Qzy,n_bf,n_bf); call create_(Qzz,n_bf,n_bf)
      call get_spin_orbit_Q_matrices_(self,Qxx,Qxy,Qxz,Qyx,Qyy,Qyz,Qzx,Qzy,Qzz)
      SOBx = (Qyy + Qzz)*self%B_field(1) - Qxy*self%B_field(2) - Qxz*self%B_field(3)
      SOBy = (Qxx + Qzz)*self%B_field(2) - Qyx*self%B_field(1) - Qyz*self%B_field(3)
      SOBz = (Qxx + Qyy)*self%B_field(3) - Qzx*self%B_field(1) - Qzy*self%B_field(2)
      call destroy_(Qzz); call destroy_(Qzy); call destroy_(Qzx)
      call destroy_(Qyz); call destroy_(Qyy); call destroy_(Qyx)
      call destroy_(Qxz); call destroy_(Qxy); call destroy_(Qxx)
      call symmetric_reflect_(SOBx)
      call symmetric_reflect_(SOBy)
      call symmetric_reflect_(SOBz)
      call set_(arch,self%name,"SOBx_matrix"); call write_(arch,SOBx)
      call set_(arch,self%name,"SOBy_matrix"); call write_(arch,SOBy)
      call set_(arch,self%name,"SOBz_matrix"); call write_(arch,SOBz)

   end subroutine

   subroutine get_spin_orbit_Q_matrices(self,Qxx,Qxy,Qxz,Qyx,Qyy,Qyz,Qzx,Qzy,Qzz)
    type(mol_type) :: self
    ! Set "Qij" the gauge modified (B field) spin orbit quadrupole matrices.
    ! If archives exist, read them; otherwise make them.
      real(kind=kind(1.0d0)), dimension(:,:) :: Qxx,Qxy,Qxz,Qyx,Qyy,Qyz,Qzx,Qzy,Qzz
      type(archive_type) :: xx_archive,xy_archive,xz_archive
      type(archive_type) :: yx_archive,yy_archive,yz_archive
      type(archive_type) :: zx_archive,zy_archive,zz_archive

      call set_(xx_archive,self%name,"SO_Qxx_matrix")
      call set_(xy_archive,self%name,"SO_Qxy_matrix")
      call set_(xz_archive,self%name,"SO_Qxz_matrix")
      call set_(yx_archive,self%name,"SO_Qyx_matrix")
      call set_(yy_archive,self%name,"SO_Qyy_matrix")
      call set_(yz_archive,self%name,"SO_Qyz_matrix")
      call set_(zx_archive,self%name,"SO_Qzx_matrix")
      call set_(zy_archive,self%name,"SO_Qzy_matrix")
      call set_(zz_archive,self%name,"SO_Qzz_matrix")
      if (exists_(xx_archive) ) then
         call read_(xx_archive,Qxx); call read_(xy_archive,Qxy); call read_(xz_archive,Qxz)
         call read_(yx_archive,Qyx); call read_(yy_archive,Qyy); call read_(yz_archive,Qyz)
         call read_(zx_archive,Qzx); call read_(zy_archive,Qzy); call read_(zz_archive,Qzz)
      else
         call make_spin_orbit_Q_matrices_(self,Qxx,Qxy,Qxz,Qyx,Qyy,Qyz,Qzx,Qzy,Qzz)
      end if

   end subroutine

   subroutine make_spin_orbit_Q_matrices(self,Qxx,Qxy,Qxz,Qyx,Qyy,Qyz,Qzx,Qzy,Qzz)
    type(mol_type) :: self
    ! Calculate the gauge modified (B field) spin orbit quadrupole matrices
    ! "Qij".
      real(kind=kind(1.0d0)), dimension(:,:) :: Qxx,Qxy,Qxz,Qyx,Qyy,Qyz,Qzx,Qzy,Qzz
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: xx,xy,xz,yx,yy,yz,zx,zy,zz
      integer(kind=kind(1)) :: q,c,fa,la,fb,lb
       real(kind=kind(1.0d0)) :: Z
      type(shell2_type) :: sh
      type(archive_type) :: arch
      integer(kind=kind(1)) :: n_a,n_b

   call ensure_(tonto,self%basis_info_made,"MOL:make_spin_orbit_Q_matrices ... no basis info")
   call ensure_(tonto,associated(self%atom),"MOL:make_spin_orbit_Q_matrices ... no atom list")
      Qxx = 0.0d0; Qxy = 0.0d0; Qxz = 0.0d0
      Qyx = 0.0d0; Qyy = 0.0d0; Qyz = 0.0d0
      Qzx = 0.0d0; Qzy = 0.0d0; Qzz = 0.0d0
      do q = 1,self%n_shell_pairs
         call get_shell_pair_(self,sh,q,fa,la,fb,lb)
         n_a = sh%a%n_comp; n_b = sh%b%n_comp
         call create_(xx,n_a,n_b); call create_(xy,n_a,n_b); call create_(xz,n_a,n_b)
         call create_(yx,n_a,n_b); call create_(yy,n_a,n_b); call create_(yz,n_a,n_b)
         call create_(zx,n_a,n_b); call create_(zy,n_a,n_b); call create_(zz,n_a,n_b)
         do c = 1,self%n_atom
            Z = self%atom(c)%atomic_number
            call make_spin_orbit_B_ints_(sh,xx,xy,xz,yx,yy,yz,zx,zy,zz, &
                                    self%atom(c)%pos, self%gauge_origin)
             ! minus due to r-c, not c-r in the electric field part
             ! this comment should go in gaussian2?
            Qxx(fa:la,fb:lb) = Qxx(fa:la,fb:lb) - Z*xx
            Qxy(fa:la,fb:lb) = Qxy(fa:la,fb:lb) - Z*xy
            Qxz(fa:la,fb:lb) = Qxz(fa:la,fb:lb) - Z*xz
            Qyx(fa:la,fb:lb) = Qyx(fa:la,fb:lb) - Z*yx
            Qyy(fa:la,fb:lb) = Qyy(fa:la,fb:lb) - Z*yy
            Qyz(fa:la,fb:lb) = Qyz(fa:la,fb:lb) - Z*yz
            Qzx(fa:la,fb:lb) = Qzx(fa:la,fb:lb) - Z*zx
            Qzy(fa:la,fb:lb) = Qzy(fa:la,fb:lb) - Z*zy
            Qzz(fa:la,fb:lb) = Qzz(fa:la,fb:lb) - Z*zz
         end do
         call destroy_ptr_part_(sh)
         call destroy_(zz); call destroy_(zy); call destroy_(zx)
         call destroy_(yz); call destroy_(yy); call destroy_(yx)
         call destroy_(xz); call destroy_(xy); call destroy_(xx)
      end do
      call symmetric_reflect_(Qxx); call symmetric_reflect_(Qxy); call symmetric_reflect_(Qxz)
      call symmetric_reflect_(Qyx); call symmetric_reflect_(Qyy); call symmetric_reflect_(Qyz)
      call symmetric_reflect_(Qzx); call symmetric_reflect_(Qzy); call symmetric_reflect_(Qzz)
      call set_(arch,self%name,"SO_Qxx_matrix"); call write_(arch,Qxx)
      call set_(arch,self%name,"SO_Qxy_matrix"); call write_(arch,Qxy)
      call set_(arch,self%name,"SO_Qxz_matrix"); call write_(arch,Qxz)
      call set_(arch,self%name,"SO_Qyx_matrix"); call write_(arch,Qyx)
      call set_(arch,self%name,"SO_Qyy_matrix"); call write_(arch,Qyy)
      call set_(arch,self%name,"SO_Qyz_matrix"); call write_(arch,Qyz)
      call set_(arch,self%name,"SO_Qzx_matrix"); call write_(arch,Qzx)
      call set_(arch,self%name,"SO_Qzy_matrix"); call write_(arch,Qzy)
      call set_(arch,self%name,"SO_Qzz_matrix"); call write_(arch,Qzz)

   end subroutine

   subroutine nuclear_potential(self,values,pts)
    type(mol_type) :: self
    ! Calculate the nuclear potential "values" of a given set of "pts".
    ! This is usefule for numerical integration of nuclear attraction integrals.
      real(kind=kind(1.0d0)), dimension(:) :: values
      real(kind=kind(1.0d0)), dimension(:,:) :: pts
      integer(kind=kind(1)) :: n_pts,n,i
      real(kind=kind(1.0d0)) :: Z_n,r
      real(kind=kind(1.0d0)), dimension(3) :: pos

      n_pts = size(pts,1)
      values = 0.0d0
      do n = 1,self%n_atom
         Z_n = self%atom(n)%atomic_number
         pos = self%atom(n)%pos
         do i = 1,n_pts
            r = distance_to_(pos,pts(i,:))
            if (r>tiny(r)) then
              values(i) = values(i) + Z_n / r
            else
              values(i) = values(i) + 10.0d0**6
            end if
         end do
      end do

   end subroutine

   subroutine ZORA_potential(self,values,pts)
    type(mol_type) :: self
    ! Calculate the one electron ZORA potential "values" of a given set of "pts".
    ! This is useful for numerical integration.
      real(kind=kind(1.0d0)), dimension(:) :: values
      real(kind=kind(1.0d0)), dimension(:,:) :: pts
      real(kind=kind(1.0d0)) :: fac

      call nuclear_potential_(self,values,pts)
      fac    = 1.0d0/(137.03599d0*137.03599d0)
      values = 1.0d0/(2.0d0 + fac*values)

   end subroutine

   subroutine get_1e_ZORA_matrices(self,T,SOx,SOy,SOz)
    type(mol_type) :: self
    ! Set "SOx", "SOy", and "SOz" to the one electron ZORA spin orbit matrices.
    ! If archives exist, read them; otherwise make them.
      real(kind=kind(1.0d0)), dimension(:,:) :: T,SOx,SOy,SOz
      type(archive_type) :: T_archive,SOx_archive,SOy_archive,SOz_archive

      call set_(T_archive,self%name,"ZORA_kinetic_matrix")
      call set_(SOx_archive,self%name,"ZORA_SOx_matrix")
      call set_(SOy_archive,self%name,"ZORA_SOy_matrix")
      call set_(SOz_archive,self%name,"ZORA_SOz_matrix")
      if (exists_(SOx_archive) .and. exists_(SOy_archive) .and. exists_(SOz_archive)) then
         call read_(T_archive,T)
         call read_(SOx_archive,SOx)
         call read_(SOy_archive,SOy)
         call read_(SOz_archive,SOz)
      else
         call make_1e_ZORA_matrices_(self,T,SOx,SOy,SOz)
      end if

   end subroutine

   subroutine make_1e_ZORA_matrices(self,T,Zx,Zy,Zz)
    type(mol_type) :: self
    ! Calculate the one-electron ZORA spin orbit matrices numerically.
    ! This includes the relativitically modified kinetic energy integrals.
     real(kind=kind(1.0d0)), dimension(:,:) :: T, Zx,Zy,Zz
     real(kind=kind(1.0d0)), dimension(:,:,:,:), pointer :: ZORA,SO
     integer(kind=kind(1)) :: q,fa,la,fb,lb,k,l,a,b
     type(shell2_type) :: sh
     type(archive_type) :: arch

   call ensure_(tonto,self%basis_info_made,"MOL:make_1e_ZORA_matrices ... no basis info")
   call ensure_(tonto,associated(self%atom),"MOL:make_1e_ZORA_matrices ... no atom list")
   call ensure_(tonto,associated(self%dftgrid),"MOL:make_1e_ZORA_matrices ... need to specify dftgrid for ZORA")
     call create_(ZORA,self%n_bf,self%n_bf,3,3)
      ! Make the lower half of the ZORA spin orbit integrals
     ZORA = 0.0d0
     do q = 1,self%n_shell_pairs
        call get_shell_pair_(self,sh,q,fa,la,fb,lb)
        call create_(SO,sh%a%n_comp,sh%b%n_comp,3,3)
        call make_SO_matrix_elements_of_(self%dftgrid,ZORA_potential,self,sh,SO)
        ZORA(fa:la,fb:lb,:,:) = ZORA(fa:la,fb:lb,:,:) + SO
        call destroy_(SO)
        call destroy_ptr_part_(sh)
     end do
      ! Make the upper half of the ZORA spin orbit integrals
     do k = 1,3
     do l = 1,3
     do a = 1,self%n_bf
     do b = 1,a-1
        ZORA(b,a,l,k) = ZORA(a,b,k,l)
     end do
     end do
     end do
     end do
      ! Assemble the ZORA contribution to the 1 electron hamiltonian
      ! Scalar kinetic energy contribution
     T = ZORA(:,:,1,1) + ZORA(:,:,2,2) + ZORA(:,:,3,3)
      ! Spin-dependent spin-orbit contribution
     Zx = ZORA(:,:,2,3) - ZORA(:,:,3,2)
     Zy = ZORA(:,:,3,1) - ZORA(:,:,1,3)
     Zz = ZORA(:,:,1,2) - ZORA(:,:,2,1)
     call destroy_(ZORA)
     call set_(arch,self%name,"ZORA_kinetic_matrix"); call write_(arch,T)
     call set_(arch,self%name,"ZORA_SOx_matrix");     call write_(arch,Zx)
     call set_(arch,self%name,"ZORA_SOy_matrix");     call write_(arch,Zy)
     call set_(arch,self%name,"ZORA_SOz_matrix");     call write_(arch,Zz)

   end subroutine

   subroutine make_ENA_matrix(self,Z)
    type(mol_type) :: self
    ! Calculate the one-electron electron nuclear attractio matrix numerically.
      real(kind=kind(1.0d0)), dimension(:,:) :: Z
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: ZZ
     integer(kind=kind(1)) :: q,fa,la,fb,lb
     type(shell2_type) :: sh
     type(archive_type) :: arch

   call ensure_(tonto,self%basis_info_made,"MOL:make_ENA_matrix ... no basis info")
   call ensure_(tonto,associated(self%atom),"MOL:make_ENA_matrix ... no atom list")
   call ensure_(tonto,associated(self%dftgrid),"MOL:make_ENA_matrix ... need to specify dftgrid for ZORA")
     Z = 0.0d0
     do q = 1,self%n_shell_pairs
        call get_shell_pair_(self,sh,q,fa,la,fb,lb)
        call create_(ZZ,sh%a%n_comp,sh%b%n_comp)
        call make_matrix_elements_of_(self%dftgrid,nuclear_potential,self,sh,ZZ)
        Z(fa:la,fb:lb) = Z(fa:la,fb:lb) - ZZ
        call destroy_(ZZ)
        call destroy_ptr_part_(sh)
     end do
     call symmetric_reflect_(Z)
     call set_(arch,self%name,"ENA_matrix"); call write_(arch,Z)

   end subroutine

   subroutine get_L_matrices(self,Lx,Ly,Lz)
    type(mol_type) :: self
    ! Set "Lx", "Ly", and "Lz" to the angular momentum matrices.
    ! If archives exist, read them; otherwise make them.
      real(kind=kind(1.0d0)), dimension(:,:) :: Lx,Ly,Lz
      type(archive_type) :: Lx_archive,Ly_archive,Lz_archive

      call set_(Lx_archive,self%name,"Lx_matrix")
      call set_(Ly_archive,self%name,"Ly_matrix")
      call set_(Lz_archive,self%name,"Lz_matrix")
      if (exists_(Lx_archive) .and. exists_(Ly_archive) .and. exists_(Lz_archive)) then
         call read_(Lx_archive,Lx)
         call read_(Ly_archive,Ly)
         call read_(Lz_archive,Lz)
      else
         call make_L_matrices_(self,Lx,Ly,Lz)
      end if

   end subroutine

   subroutine make_L_matrices(self,Lx,Ly,Lz)
    type(mol_type) :: self
    ! Make the angular momentum matrices  "Lx", "Ly", and "Lz".
      real(kind=kind(1.0d0)), dimension(:,:) :: Lx,Ly,Lz
      integer(kind=kind(1)) :: q,fa,la,fb,lb
      type(shell2_type) :: sh
      type(archive_type) :: arch

   call ensure_(tonto,self%basis_info_made,"MOL:make_L_matrices ... no basis info")
   call ensure_(tonto,associated(self%atom),"MOL:make_L_matrices ... no atom list")
      do q=1,self%n_shell_pairs
         call get_shell_pair_(self,sh,q,fa,la,fb,lb)
         call make_L_ints_(sh,Lx(fa:la,fb:lb),Ly(fa:la,fb:lb),Lz(fa:la,fb:lb), self%gauge_origin)
         call destroy_ptr_part_(sh)
      end do
      call antisymmetric_reflect_(Lx)
      call antisymmetric_reflect_(Ly)
      call antisymmetric_reflect_(Lz)
      call set_(arch,self%name,"Lx_matrix"); call write_(arch,Lx)
      call set_(arch,self%name,"Ly_matrix"); call write_(arch,Ly)
      call set_(arch,self%name,"Lz_matrix"); call write_(arch,Lz)

   end subroutine

!*******************************************************************************
!  Restricted DFT
!*******************************************************************************

   subroutine add_ex_corr_matrix(self,K)
    type(mol_type) :: self
    ! Calculate the exchange correlation matrix numerically.
      real(kind=kind(1.0d0)), dimension(:,:) :: K

      if (self%scfdata%dft_non_local_exchange .or. self%scfdata%dft_non_local_correlation) then
         call add_non_local_ex_corr_matrix_(self,K)
      else
         call add_local_ex_corr_matrix_(self,K)
      end if

   end subroutine

   subroutine add_non_local_ex_corr_matrix(self,K)
    type(mol_type) :: self
    ! Add the exchange and correlation matrices to K.
    ! This routine supports non-local functionals.
     real(kind=kind(1.0d0)), dimension(:,:), target :: K
     type(shell1_type) :: sh1
     type(atom_type), dimension(2) :: atomlist2
     type(atom_type), dimension(1) :: atomlist1
     real(kind=kind(1.0d0)), dimension(3) :: pos1,pos2
     real(kind=kind(1.0d0)), dimension(:,:,:), pointer :: nabla_a_grid,nabla_b_grid
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: pt,np,a_grid,b_grid,non_local
     real(kind=kind(1.0d0)), dimension(:), pointer :: wt,p,local,a_grid_a,b_grid_b
     integer(kind=kind(1)) :: q,fa,la,fb,lb,sa,sb,n_pt,start,step
     integer(kind=kind(1)) :: atom1,atom2,i,j,a,b,z,n_comp_a,n_comp_b
     real(kind=kind(1.0d0)) :: thresh,KK
      ! n :: integer(kind=kind(1))
      ! nabla_phiaphib :: real(kind=kind(1.0d0))

     call ensure_(tonto,self%basis_info_made,"MOL:add_non_local_ex_corr_matrix ... no basis info")
     call ensure_(tonto,associated(self%atom),"MOL:add_non_local_ex_corr_matrix ... no atom list")
     call ensure_(tonto,associated(self%dftgrid),"MOL:add_non_local_ex_corr_matrix ... need to specify dftgrid for ZORA")
     thresh = 1.0e-30

     start = 1 + this_proc_(tonto_parallel)
     step = n_proc_(tonto_parallel)
     if (tonto_parallel%do_parallel) then
       K = 0.0d0
     end if

     do q = start,no_of_atom_pairs_(self),step
       atom1 = (1+int(sqrt(8.0d0*q-7.0d0)))/2
       atom2 = q - atom1*(atom1-1)/2
       if (atom1/=atom2) then
         atomlist2(1) = self%atom(atom1)
         atomlist2(2) = self%atom(atom2)
         n_pt = self%dftgrid%n_pts*2
         call create_(wt,n_pt)
         call create_(pt,n_pt,3)
         call make_grid_(self%dftgrid,pt,wt,atomlist2)
       else
         atomlist1(1) = self%atom(atom1)
         n_pt = self%dftgrid%n_pts
         call create_(wt,n_pt)
         call create_(pt,n_pt,3)
         call make_grid_(self%dftgrid,pt,wt,atomlist1)
       end if
       pos1 = self%atom(atom1)%pos
       pos2 = self%atom(atom2)%pos

       call create_(p,n_pt)
       call create_(np,n_pt,3)
       call create_(local,n_pt)
       call create_(non_local,n_pt,3)
!       phia_phib.create(n_pt)

        ! Get the densities and their gradients.
       call make_density_grid_(self,p,pt)
       call make_nabla_density_grid_(self,np,pt)
       p = max(thresh,p)

        ! Get the local and nonlocal contributions.
       local=0.0d0
       non_local=0.0d0
       if (self%scfdata%dft_non_local_exchange) then
         call add_r_exchange_matrix_(self,p,local,np,non_local)
       else if (self%scfdata%dft_exchange /= "none") then
         call add_r_exchange_matrix_(self,p,local)
       end if
       if (self%scfdata%dft_non_local_correlation) then
         call add_r_correlation_matrix_(self,p,local,np,non_local)
       else if (self%scfdata%dft_correlation /= "none") then
         call add_r_correlation_matrix_(self,p,local)
       end if

        ! Multiply by the weights.
       local = wt*local
       forall (i=1:3) non_local(:,i) = wt*non_local(:,i)

        ! Calculate orbital grids, then multiply by the orbital grids and put in
        ! ex-corr matrix.
       do i=self%first_shell_for_atom(atom1),self%last_shell_for_atom(atom1)
         fa = self%first_basis_fn_for_shell(i)
         la = self%last_basis_fn_for_shell(i)
         sa = self%atom_shell_for_shell(i)
         call set_(sh1,self%atom(atom1)%basis%shell(sa),pos1)
         n_comp_a = sh1%n_comp
         call create_(nabla_a_grid,n_pt,n_comp_a,3)
         call create_(a_grid,n_pt,n_comp_a)
         call make_nabla_grid_(sh1,nabla_a_grid,a_grid,pt)
         call nullify_ptr_part_(sh1)

         do j=self%first_shell_for_atom(atom2),self%last_shell_for_atom(atom2)
           fb = self%first_basis_fn_for_shell(j)
           lb = self%last_basis_fn_for_shell(j)
           sb = self%atom_shell_for_shell(j)
           call set_(sh1,self%atom(atom2)%basis%shell(sb),pos2)
           n_comp_b = sh1%n_comp
           call create_(b_grid,n_pt,n_comp_b)
           call create_(nabla_b_grid,n_pt,n_comp_b,3)
           call make_nabla_grid_(sh1,nabla_b_grid,b_grid,pt)
           call nullify_ptr_part_(sh1)

            ! ------------------------------------------------------------
           do a = 1,n_comp_a
             a_grid_a => a_grid(:,a)
             do b = 1,n_comp_b
               b_grid_b => b_grid(:,b)
!               phia_phib = a_grid_a*b_grid_b
!               KK = sum(local(:)*phia_phib(:))
               KK = sum(local*a_grid_a*b_grid_b)
               do z=1,3
!                 do n=1,n_pt
!                   nabla_phiaphib = nabla_a_grid(n,a,z)*b_grid_b(n) + &
!                                    nabla_b_grid(n,b,z)*a_grid_a(n)
!                   KK = KK + non_local(n,z)*nabla_phiaphib
!                 end
                 KK = KK + sum(non_local(:,z)*(nabla_a_grid(:,a,z)*b_grid_b(:)+nabla_b_grid(:,b,z)*a_grid_a(:)))
               end do
               K(fa-1+a,fb-1+b) = K(fa-1+a,fb-1+b) + KK
             end do
           end do
            ! ------------------------------------------------------------

           call destroy_(nabla_b_grid)
           call destroy_(b_grid)
         end do
         call destroy_(nabla_a_grid)
         call destroy_(a_grid)
       end do
       call nullify_ptr_part_(atomlist1)
       call nullify_ptr_part_(atomlist2)
!       phia_phib.destroy
       call destroy_(local); call destroy_(non_local)
       call destroy_(np)
       call destroy_(p)
       call destroy_(wt)
       call destroy_(pt)
     end do

     call symmetric_reflect_(K)
     call sum_symmetric_matrices_(tonto_parallel,K)

   end subroutine

   subroutine add_local_ex_corr_matrix(self,K)
    type(mol_type) :: self
    ! Add the exchange and correlation matrices to K.
    ! This routine supports local functionals only.
     real(kind=kind(1.0d0)), dimension(:,:), target :: K
     type(shell1_type) :: sh1
     type(atom_type), dimension(2) :: atomlist2
     type(atom_type), dimension(1) :: atomlist1
     real(kind=kind(1.0d0)), dimension(3) :: pos1,pos2
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: pt,a_grid,b_grid
     real(kind=kind(1.0d0)), dimension(:), pointer :: wt,p,local,a_grid_a,b_grid_b
     integer(kind=kind(1)) :: q,fa,la,fb,lb,sa,sb,n_pt,start,step
     integer(kind=kind(1)) :: atom1,atom2,i,j,a,b,n_comp_a,n_comp_b
     real(kind=kind(1.0d0)) :: thresh

     call ensure_(tonto,self%basis_info_made,"MOL:add_local_ex_corr_matrix ... no basis info")
     call ensure_(tonto,associated(self%atom),"MOL:add_local_ex_corr_matrix ... no atom list")
     call ensure_(tonto,associated(self%dftgrid),"MOL:add_local_ex_corr_matrix ... need to specify dftgrid for ZORA")
     thresh = 1.0e-30

     start = 1 + this_proc_(tonto_parallel)
     step = n_proc_(tonto_parallel)
     if (tonto_parallel%do_parallel) then
       K = 0.0d0
     end if

     do q = start,no_of_atom_pairs_(self),step
       atom1 = (1+int(sqrt(8.0d0*q-7.0d0)))/2
       atom2 = q - atom1*(atom1-1)/2
       if (atom1/=atom2) then
         atomlist2(1) = self%atom(atom1)
         atomlist2(2) = self%atom(atom2)
         n_pt = self%dftgrid%n_pts*2
         call create_(wt,n_pt)
         call create_(pt,n_pt,3)
         call make_grid_(self%dftgrid,pt,wt,atomlist2)
       else
         atomlist1(1) = self%atom(atom1)
         n_pt = self%dftgrid%n_pts
         call create_(wt,n_pt)
         call create_(pt,n_pt,3)
         call make_grid_(self%dftgrid,pt,wt,atomlist1)
       end if
       pos1 = self%atom(atom1)%pos
       pos2 = self%atom(atom2)%pos

       call create_(p,n_pt)
       call create_(local,n_pt)

        ! Get the densities and their gradients.
       call make_density_grid_(self,p,pt)
       p = max(thresh,p)

        ! Get the local and nonlocal contributions.
       local=0.0d0
       call add_r_exchange_matrix_(self,p,local)
       call add_r_correlation_matrix_(self,p,local)

        ! Multiply by the weights.
       local = wt*local

        ! Calculate orbital grids, then multiply by the orbital grids and put in
        ! ex-corr matrix.
       do i=self%first_shell_for_atom(atom1),self%last_shell_for_atom(atom1)
         fa = self%first_basis_fn_for_shell(i)
         la = self%last_basis_fn_for_shell(i)
         sa = self%atom_shell_for_shell(i)
         call set_(sh1,self%atom(atom1)%basis%shell(sa),pos1)
         n_comp_a = sh1%n_comp
         call create_(a_grid,n_pt,n_comp_a)
         call make_grid_(sh1,a_grid,pt)
         call nullify_ptr_part_(sh1)

         do j=self%first_shell_for_atom(atom2),self%last_shell_for_atom(atom2)
           fb = self%first_basis_fn_for_shell(j)
           lb = self%last_basis_fn_for_shell(j)
           sb = self%atom_shell_for_shell(j)
           call set_(sh1,self%atom(atom2)%basis%shell(sb),pos2)
           n_comp_b = sh1%n_comp
           call create_(b_grid,n_pt,n_comp_b)
           call make_grid_(sh1,b_grid,pt)
           call nullify_ptr_part_(sh1)

            ! ------------------------------------------------------------
           do a = 1,n_comp_a
             a_grid_a => a_grid(:,a)
             do b = 1,n_comp_b
               b_grid_b => b_grid(:,b)
               K(fa-1+a,fb-1+b) = K(fa-1+a,fb-1+b) + sum(local*a_grid_a*b_grid_b)
             end do
           end do
            ! ------------------------------------------------------------

           call destroy_(b_grid)
         end do
         call destroy_(a_grid)
       end do
       call nullify_ptr_part_(atomlist1)
       call nullify_ptr_part_(atomlist2)
       call destroy_(local)
       call destroy_(p)
       call destroy_(wt)
       call destroy_(pt)
     end do

     call symmetric_reflect_(K)
     call sum_symmetric_matrices_(tonto_parallel,K)

   end subroutine

   function r_dft_energy_correction(self) result(res)
    type(mol_type) :: self
    ! The total electronic energy can be written as....
    !   2.718281828459045d0 = sum(P*(F+H)) + correction
    ! where
    !   correction = -(sum(P*K)) + Exc
    ! where K is the exchange-correlation contribution to the Fock matrix.
     real(kind=kind(1.0d0)) :: res
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: pt,np,non_local
     real(kind=kind(1.0d0)), dimension(:), pointer :: wt,p,local,f,total
     real(kind=kind(1.0d0)) :: THRESH
     integer(kind=kind(1)) :: n_pt

     call ensure_(tonto,self%basis_info_made,"MOL:r_dft_energy_correction ... no basis info")
     call ensure_(tonto,associated(self%atom),"MOL:r_dft_energy_correction ... no atom list")
     call ensure_(tonto,associated(self%dftgrid),"MOL:r_dft_energy_correction ... need to specify dftgrid for ZORA")
     THRESH = 10.0d0**(-90)
     n_pt = self%dftgrid%n_pts*self%n_atom
     call create_(wt,n_pt)
     call create_(pt,n_pt,3)
     call make_grid_(self%dftgrid,pt,wt,self%atom)

      ! Get the densities and their gradients.
     call create_(p,n_pt)
     call create_(np,n_pt,3)

     call make_density_grid_(self,p,pt)
     call make_nabla_density_grid_(self,np,pt)

     call destroy_(pt)
     p = max(thresh,p)

     call create_(local,n_pt)
     call create_(non_local,n_pt,3)
     local=0.0d0
     non_local=0.0d0
     if (self%scfdata%dft_non_local_exchange) then
       call add_r_exchange_matrix_(self,p,local,np,non_local)
     else if (self%scfdata%dft_exchange /= "none") then
       call add_r_exchange_matrix_(self,p,local)
     end if
     if (self%scfdata%dft_non_local_correlation) then
       call add_r_correlation_matrix_(self,p,local,np,non_local)
     else if (self%scfdata%dft_correlation /= "none") then
       call add_r_correlation_matrix_(self,p,local)
     end if

      ! This is -1/2 integral v_xc rho dr.
     call create_(total,n_pt)
     total = local*p
     if (self%scfdata%dft_non_local_exchange .or. self%scfdata%dft_non_local_correlation) then
       total = total + sum(non_local(:,:)*np(:,:),2)
     end if
     total = -0.50d0*total
     call destroy_(non_local)
     call destroy_(local)

      ! Now add Exc
     call create_(f,n_pt)
     call r_exchange_functional_(self,p,np,f)
     total = total + f
     call r_correlation_functional_(self,p,np,f)
     total = total + f
     call destroy_(f)
     res = sum(wt*total)
     call destroy_(wt)
     call destroy_(total)

     call destroy_(np)
     call destroy_(p)

   end function

   function r_ex_corr_energy(self) result(res)
    type(mol_type) :: self
    ! Return the exchange-correlation energy
     real(kind=kind(1.0d0)) :: res
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: pt,np
     real(kind=kind(1.0d0)), dimension(:), pointer :: wt,p,fx,fc
     integer(kind=kind(1)) :: n_pt
     real(kind=kind(1.0d0)) :: THRESH

     call ensure_(tonto,self%basis_info_made,"MOL:r_ex_corr_energy ... no basis info")
     call ensure_(tonto,associated(self%atom),"MOL:r_ex_corr_energy ... no atom list")
     call ensure_(tonto,associated(self%dftgrid),"MOL:r_ex_corr_energy ... need to specify dftgrid for ZORA")
     THRESH = 10.0d0**(-90)
     n_pt = self%dftgrid%n_pts*self%n_atom
     call create_(wt,n_pt)
     call create_(pt,n_pt,3)
     call make_grid_(self%dftgrid,pt,wt,self%atom)
     call make_restricted_NOs_(self)

      ! Get the densities and their gradients.
     call create_(p,n_pt)
     call make_density_grid_(self,p,pt)

     if (self%scfdata%dft_non_local_exchange .or. self%scfdata%dft_non_local_correlation) then
       call create_(np,n_pt,3)
       call make_nabla_density_grid_(self,np,pt)
     end if

     call destroy_(pt)
     p = max(THRESH,p)
     call create_(fx,n_pt)
     call create_(fc,n_pt)

     call r_exchange_functional_(self,p,np,fx)
     call r_correlation_functional_(self,p,np,fc)
     res = sum(wt*(fx+fc))

     call destroy_(fc)
     call destroy_(fx)
     if (self%scfdata%dft_non_local_exchange .or. self%scfdata%dft_non_local_correlation) then
       call destroy_(np)
     end if
     call destroy_(p)
     call destroy_(wt)

   end function

   subroutine r_exchange_functional(self,p,np,f)
    type(mol_type) :: self
    ! Calculate the exchange functional.
    ! Input is p,np.  Output is f.
    ! np is optional.
    ! (p means density, np means gradient of the density).
    !
    ! NOTE: When adding a new functional, alter the subroutine
    !       scfdata.foo/read_dft_exchange also.
    !
     real(kind=kind(1.0d0)), dimension(:), intent(in) :: p
     real(kind=kind(1.0d0)), dimension(:) :: f
     real(kind=kind(1.0d0)), dimension(:,:), optional, intent(in) :: np
     character(128) :: ex_kind
     real(kind=kind(1.0d0)) :: alpha

     ex_kind = self%scfdata%dft_exchange
     select case (ex_kind)
       case ("none")
         f = 0.0d0
       case ("slater")
         alpha=2*0.33333333333333333333333d0
         call r_lda_exchange_functional_(self%dftgrid,p,f,alpha)
       case ("xalpha")
         alpha=0.7
         call r_lda_exchange_functional_(self%dftgrid,p,f,alpha)
       case ("becke88")
         call ensure_(tonto,present(np),"MOL:r_exchange_functional ... program bug!")
         call r_becke88_exchange_functional_(self%dftgrid,p,np,f)
       case ("gill96")
         call ensure_(tonto,present(np),"MOL:r_exchange_functional ... program bug!")
         call r_gill96_exchange_functional_(self%dftgrid,p,np,f)
       case default
         call die_(tonto,"MOL:r_exchange_functional ... unknown exchange functional")
     end select

   end subroutine

   subroutine r_correlation_functional(self,p,np,f)
    type(mol_type) :: self
    ! Calculate the exchange correlation.
    ! Input is p,np.  Output is f.
    ! np are optional.
    ! (p means density, np means gradient of the density).
    !
    ! NOTE: When adding a new functional, alter the subroutine
    !       scfdata.foo/read_dft_correlation also.
    !
     real(kind=kind(1.0d0)), dimension(:), intent(in) :: p
     real(kind=kind(1.0d0)), dimension(:) :: f
     real(kind=kind(1.0d0)), dimension(:,:), optional, intent(in) :: np
     character(128) :: corr_kind

     corr_kind = self%scfdata%dft_correlation
     select case (corr_kind)
       case ("none")
         f = 0.0d0
       case ("lyp")
         call ensure_(tonto,present(np),"MOL:r_correlation_functional ... program bug!")
         call r_lyp_correlation_functional_(self%dftgrid,p,np,f)
       case default
         call die_(tonto,"MOL:r_correlation_functional ... unknown exchange functional")
     end select

   end subroutine

   subroutine add_r_exchange_matrix(self,p,local,np,non_local)
    type(mol_type) :: self
    ! Get terms necessary to calculate the exchange functional matrix.
    ! Input is p,np.
    ! Output is local,non_local.
    ! np and non_local are optional arguments.
    ! (p means density, np means nabla density).
    !
    ! NOTE: When adding a new functional, alter the subroutine
    !       scfdata.foo/read_dft_exchange also.
    !
     real(kind=kind(1.0d0)), dimension(:) :: p,local
     real(kind=kind(1.0d0)), dimension(:,:), optional :: np,non_local
     real(kind=kind(1.0d0)), dimension(:), pointer :: l
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: nl
     character(128) :: ex_kind
     real(kind=kind(1.0d0)) :: alpha
     integer(kind=kind(1)) :: n_pt

     ex_kind = self%scfdata%dft_exchange
     n_pt = size(p)
     select case (ex_kind)
       case ("none")
        ! Don't add anything.
       case ("slater")
         alpha=2*0.33333333333333333333333d0
         call create_(l,n_pt)
         call d_r_lda_exchange_functional_(self%dftgrid,p,l,alpha)
         local = local + l
         call destroy_(l)
       case ("xalpha")
         alpha=0.7
         call create_(l,n_pt)
         call d_r_lda_exchange_functional_(self%dftgrid,p,l,alpha)
         local = local + l
         call destroy_(l)
       case ("becke88")
         call ensure_(tonto,present(np),"MOL:add_r_exchange_matrix ... program bug!")
         call ensure_(tonto,present(non_local),"MOL:add_r_exchange_matrix ... program bug!")
         call create_(l,n_pt)
         call create_(nl,n_pt,3)
         call d_r_b88_exchange_functional_(self%dftgrid,p,np,l,nl)
         local = local + l
         non_local = non_local + nl
         call destroy_(nl)
         call destroy_(l)
       case ("gill96")
         call ensure_(tonto,present(np),"MOL:add_r_exchange_matrix ... program bug!")
         call ensure_(tonto,present(non_local),"MOL:add_r_exchange_matrix ... program bug!")
         call create_(l,n_pt)
         call create_(nl,n_pt,3)
         call d_r_gill96_exchange_functional_(self%dftgrid,p,np,l,nl)
         local = local + l
         non_local = non_local + nl
         call destroy_(nl)
         call destroy_(l)
       case default
         call die_(tonto,"MOL:add_r_exchange_matrix ... unknown exchange functional")
     end select

   end subroutine

   subroutine add_r_correlation_matrix(self,p,local,np,non_local)
    type(mol_type) :: self
    ! Get terms necessary to calculate the correlation functional matrix.
    ! Input is p,np.
    ! Output is local,non_local.
    ! np and non_local are optional arguments.
    ! (p means density, np means nabla density).
    !
    ! NOTE: When adding a new functional, alter the subroutine
    !       scfdata.foo/read_dft_correlation also.
    !
     real(kind=kind(1.0d0)), dimension(:) :: p,local
     real(kind=kind(1.0d0)), dimension(:,:), optional :: np,non_local
     real(kind=kind(1.0d0)), dimension(:), pointer :: l
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: nl
     integer(kind=kind(1)) :: n_pt
     character(128) :: corr_kind

     corr_kind = self%scfdata%dft_correlation
     n_pt = size(p)
     select case (corr_kind)
       case ("none")
        ! Don't add anything.
!       case ("vwn")
       case ("lyp")
         call ensure_(tonto,present(np),"MOL:add_r_correlation_matrix ... program bug!")
         call ensure_(tonto,present(non_local),"MOL:add_r_correlation_matrix ... program bug!")
         call create_(l,n_pt)
         call create_(nl,n_pt,3)
         call d_r_lyp_correlation_functional_(self%dftgrid,p,np,l,nl)
         local = local + l
         non_local = non_local + nl
         call destroy_(nl)
         call destroy_(l)
       case default
         call die_(tonto,"MOL:add_r_correlation_matrix ... unknown correlation functional")
     end select

   end subroutine

!*******************************************************************************
!  Unrestricted DFT
!*******************************************************************************

   subroutine add_ex_corr_matrix_1(self,Ka,Kb)
    type(mol_type) :: self
    ! Calculate the exchange correlation matrix numerically.
      real(kind=kind(1.0d0)), dimension(:,:) :: Ka,Kb

      if (self%scfdata%dft_non_local_exchange .or. self%scfdata%dft_non_local_correlation) then
         call add_non_local_ex_corr_matrix_(self,Ka,Kb)
      else
         call add_local_ex_corr_matrix_(self,Ka,Kb)
      end if

   end subroutine

   subroutine add_non_local_ex_corr_matrix_1(self,Ka,Kb)
    type(mol_type) :: self
    ! Add the exchange and correlation matrices to Ka and Kb.
    ! This routine supports non-local functionals.
     real(kind=kind(1.0d0)), dimension(:,:), target :: Ka,Kb
     type(shell1_type) :: sh1
     type(atom_type), dimension(2) :: atomlist2
     type(atom_type), dimension(1) :: atomlist1
     real(kind=kind(1.0d0)), dimension(3) :: pos1,pos2
     real(kind=kind(1.0d0)), dimension(:,:,:), pointer :: nabla_a_grid,nabla_b_grid
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: pt,np_a,np_b,a_grid,b_grid,non_local_a,non_local_b
     real(kind=kind(1.0d0)), dimension(:), pointer :: wt,p_a,p_b,local_a,local_b,a_grid_a,b_grid_b,phia_phib
     integer(kind=kind(1)) :: q,fa,la,fb,lb,sa,sb,n_pt,start,step
     integer(kind=kind(1)) :: atom1,atom2,i,j,a,b,z,n,n_comp_a,n_comp_b
     real(kind=kind(1.0d0)) :: thresh,nabla_phiaphib,KKa_ab,KKb_ab

     call ensure_(tonto,self%basis_info_made,"MOL:add_non_local_ex_corr_matrix_1 ... no basis info")
     call ensure_(tonto,associated(self%atom),"MOL:add_non_local_ex_corr_matrix_1 ... no atom list")
     call ensure_(tonto,associated(self%dftgrid),"MOL:add_non_local_ex_corr_matrix_1 ... need to specify dftgrid for ZORA")
     thresh = 1.0e-30

     start = 1 + this_proc_(tonto_parallel)
     step = n_proc_(tonto_parallel)
     if (tonto_parallel%do_parallel) then
       Ka = 0.0d0
       Kb = 0.0d0
     end if

     do q = start,no_of_atom_pairs_(self),step
       atom1 = (1+int(sqrt(8.0d0*q-7.0d0)))/2
       atom2 = q - atom1*(atom1-1)/2
       if (atom1/=atom2) then
         atomlist2(1) = self%atom(atom1)
         atomlist2(2) = self%atom(atom2)
         n_pt = self%dftgrid%n_pts*2
         call create_(wt,n_pt)
         call create_(pt,n_pt,3)
         call make_grid_(self%dftgrid,pt,wt,atomlist2)
       else
         atomlist1(1) = self%atom(atom1)
         n_pt = self%dftgrid%n_pts
         call create_(wt,n_pt)
         call create_(pt,n_pt,3)
         call make_grid_(self%dftgrid,pt,wt,atomlist1)
       end if
       pos1 = self%atom(atom1)%pos
       pos2 = self%atom(atom2)%pos

       call create_(p_a,n_pt)
       call create_(p_b,n_pt)
       call create_(np_a,n_pt,3)
       call create_(np_b,n_pt,3)
       call create_(local_a,n_pt)
       call create_(local_b,n_pt)
       call create_(non_local_a,n_pt,3)
       call create_(non_local_b,n_pt,3)
       call create_(phia_phib,n_pt)

        ! Get the densities and their gradients.
       call make_unrestricted_density_grid_(self,p_a,p_b,pt)
       call make_u_nabla_density_grid_(self,np_a,np_b,pt)
       p_a = max(thresh,p_a)
       p_b = max(thresh,p_b)

        ! Get the local and nonlocal contributions.
       local_a=0.0d0
       local_b=0.0d0
       non_local_a=0.0d0
       non_local_b=0.0d0
       if (self%scfdata%dft_non_local_exchange) then
         call add_u_exchange_matrix_(self,p_a,p_b,local_a,local_b,np_a,np_b,non_local_a,non_local_b)
       else if (self%scfdata%dft_exchange /= "none") then
         call add_u_exchange_matrix_(self,p_a,p_b,local_a,local_b)
       end if
       if (self%scfdata%dft_non_local_correlation) then
         call add_u_correlation_matrix_(self,p_a,p_b,local_a,local_b,np_a,np_b,non_local_a,non_local_b)
       else if (self%scfdata%dft_correlation /= "none") then
         call add_u_correlation_matrix_(self,p_a,p_b,local_a,local_b)
       end if

        ! Multiply by the weights.
       local_a = wt*local_a
       local_b = wt*local_b
       forall (i=1:3) non_local_a(:,i) = wt*non_local_a(:,i)
       forall (i=1:3) non_local_b(:,i) = wt*non_local_b(:,i)

        ! Calculate orbital grids, then multiply by the orbital grids and put in
        ! ex-corr matrix.
       do i=self%first_shell_for_atom(atom1),self%last_shell_for_atom(atom1)
         fa = self%first_basis_fn_for_shell(i)
         la = self%last_basis_fn_for_shell(i)
         sa = self%atom_shell_for_shell(i)
         call set_(sh1,self%atom(atom1)%basis%shell(sa),pos1)
         n_comp_a = sh1%n_comp
         call create_(nabla_a_grid,n_pt,n_comp_a,3)
         call create_(a_grid,n_pt,n_comp_a)
         call make_nabla_grid_(sh1,nabla_a_grid,a_grid,pt)
         call nullify_ptr_part_(sh1)

         do j=self%first_shell_for_atom(atom2),self%last_shell_for_atom(atom2)
           fb = self%first_basis_fn_for_shell(j)
           lb = self%last_basis_fn_for_shell(j)
           sb = self%atom_shell_for_shell(j)
           call set_(sh1,self%atom(atom2)%basis%shell(sb),pos2)
           n_comp_b = sh1%n_comp
           call create_(b_grid,n_pt,n_comp_b)
           call create_(nabla_b_grid,n_pt,n_comp_b,3)
           call make_nabla_grid_(sh1,nabla_b_grid,b_grid,pt)
           call nullify_ptr_part_(sh1)

            ! ------------------------------------------------------------
           do a = 1,n_comp_a
             a_grid_a => a_grid(:,a)
             do b = 1,n_comp_b
               b_grid_b => b_grid(:,b)
               phia_phib = a_grid_a*b_grid_b
               KKa_ab = sum(local_a(:)*phia_phib(:))
               KKb_ab = sum(local_b(:)*phia_phib(:))
               do z=1,3
                 do n=1,n_pt
                   nabla_phiaphib = nabla_a_grid(n,a,z)*b_grid_b(n) + &
                                    nabla_b_grid(n,b,z)*a_grid_a(n)
                   KKa_ab = KKa_ab + non_local_a(n,z)*nabla_phiaphib
                   KKb_ab = KKb_ab + non_local_b(n,z)*nabla_phiaphib
                 end do
               end do
               Ka(fa-1+a,fb-1+b) = Ka(fa-1+a,fb-1+b) + KKa_ab
               Kb(fa-1+a,fb-1+b) = Kb(fa-1+a,fb-1+b) + KKb_ab
             end do
           end do
            ! ------------------------------------------------------------

           call destroy_(nabla_b_grid)
           call destroy_(b_grid)
         end do
         call destroy_(nabla_a_grid)
         call destroy_(a_grid)
       end do
       call nullify_ptr_part_(atomlist1)
       call nullify_ptr_part_(atomlist2)
       call destroy_(phia_phib)
       call destroy_(local_b); call destroy_(non_local_b)
       call destroy_(local_a); call destroy_(non_local_a)
       call destroy_(np_b)
       call destroy_(np_a)
       call destroy_(p_b)
       call destroy_(p_a)
       call destroy_(wt)
       call destroy_(pt)
     end do

     call symmetric_reflect_(Ka)
     call symmetric_reflect_(Kb)
     call sum_symmetric_matrices_(tonto_parallel,Ka)
     call sum_symmetric_matrices_(tonto_parallel,Kb)

   end subroutine

   subroutine add_local_ex_corr_matrix_1(self,Ka,Kb)
    type(mol_type) :: self
    ! Add the exchange and correlation matrices to Ka and Kb.
    ! This routine supports only local functionals.
     real(kind=kind(1.0d0)), dimension(:,:), target :: Ka,Kb
     type(shell2_type) :: sh
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: pt,a_grid,b_grid,KKa,KKb
     real(kind=kind(1.0d0)), dimension(:), pointer :: wt,p_a,p_b,local_a,local_b,a_grid_a,b_grid_b,phia_phib
     integer(kind=kind(1)) :: q,fa,la,fb,lb,a,b,n_pt,start,step
     real(kind=kind(1.0d0)) :: THRESH

     call ensure_(tonto,self%basis_info_made,"MOL:add_local_ex_corr_matrix_1 ... no basis info")
     call ensure_(tonto,associated(self%atom),"MOL:add_local_ex_corr_matrix_1 ... no atom list")
     call ensure_(tonto,associated(self%dftgrid),"MOL:add_local_ex_corr_matrix_1 ... need to specify dftgrid for ZORA")
     THRESH = 10.0d0**(-90)
     n_pt = self%dftgrid%n_pts*self%n_atom
     call create_(wt,n_pt)
     call create_(pt,n_pt,3)
     call make_grid_(self%dftgrid,pt,wt,self%atom)

      ! Get the densities and their gradients.
     call create_(p_a,n_pt)
     call create_(p_b,n_pt)

     call make_unrestricted_density_grid_(self,p_a,p_b,pt)

     call create_(local_a,n_pt)
     call create_(local_b,n_pt)
     p_a = max(THRESH,p_a)
     p_b = max(THRESH,p_b)

     local_a=0.0d0
     local_b=0.0d0
     if (self%scfdata%dft_exchange /= "none") then
       call add_u_exchange_matrix_(self,p_a,p_b,local_a,local_b)
     end if
     if (self%scfdata%dft_correlation /= "none") then
       call add_u_correlation_matrix_(self,p_a,p_b,local_a,local_b)
     end if
     call destroy_(p_b)
     call destroy_(p_a)
     local_a = wt*local_a
     local_b = wt*local_b
     call destroy_(wt)

     call create_(phia_phib,n_pt)
     start = 1 + this_proc_(tonto_parallel)
     step = n_proc_(tonto_parallel)
     if (tonto_parallel%do_parallel) then
       Ka = 0.0d0
       Kb = 0.0d0
     end if
     do q = start,self%n_shell_pairs,step
        call get_shell_pair_(self,sh,q,fa,la,fb,lb)
        call create_(a_grid,n_pt,sh%a%n_comp)
        call create_(b_grid,n_pt,sh%b%n_comp)
        KKa => Ka(fa:la,fb:lb)
        KKb => Kb(fa:la,fb:lb)
        call make_grid_(sh%a,a_grid,pt)
        call make_grid_(sh%b,b_grid,pt)
         ! ------------------------------------------------------------
        do a = 1,sh%a%n_comp
          a_grid_a => a_grid(:,a)
          do b = 1,sh%b%n_comp
            b_grid_b => b_grid(:,b)
            phia_phib = a_grid_a*b_grid_b
            KKa(a,b) = KKa(a,b) + sum(local_a*phia_phib)
            KKb(a,b) = KKb(a,b) + sum(local_b*phia_phib)
          end do
        end do
         ! ------------------------------------------------------------
        call destroy_(b_grid)
        call destroy_(a_grid)
        call destroy_ptr_part_(sh)
     end do
     call sum_symmetric_matrices_(tonto_parallel,Ka)
     call sum_symmetric_matrices_(tonto_parallel,Kb)
     call symmetric_reflect_(Ka)
     call symmetric_reflect_(Kb)

     call destroy_(phia_phib)
     call destroy_(local_b)
     call destroy_(local_a)
     call destroy_(pt)

   end subroutine

   function u_dft_energy_correction(self) result(res)
    type(mol_type) :: self
    ! The total electronic energy can be written as....
    !   2.718281828459045d0 = 1/2 * sum(Palpha*(Falpha+H) + Pbeta*(Fbeta+H)) + correction
    ! where
    !   correction = -1/2 * (sum(Palpha*Kalpha+Pbeta*Kbeta)) + Exc
    ! where Kalpha and Kbeta are the exchange-correlation contributions to the
    ! Fock matrices.
     real(kind=kind(1.0d0)) :: res
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: pt,np_a,np_b,non_local_a,non_local_b
     real(kind=kind(1.0d0)), dimension(:), pointer :: wt,p_a,p_b,local_a,local_b,f,total
     real(kind=kind(1.0d0)) :: THRESH
     integer(kind=kind(1)) :: n_pt

     call ensure_(tonto,self%basis_info_made,"MOL:u_dft_energy_correction ... no basis info")
     call ensure_(tonto,associated(self%atom),"MOL:u_dft_energy_correction ... no atom list")
     call ensure_(tonto,associated(self%dftgrid),"MOL:u_dft_energy_correction ... need to specify dftgrid for ZORA")
     THRESH = 10.0d0**(-90)
     n_pt = self%dftgrid%n_pts*self%n_atom
     call create_(wt,n_pt)
     call create_(pt,n_pt,3)
     call make_grid_(self%dftgrid,pt,wt,self%atom)

      ! Get the densities and their gradients.
     call create_(p_a,n_pt)
     call create_(p_b,n_pt)
     call create_(np_a,n_pt,3)
     call create_(np_b,n_pt,3)

     call make_unrestricted_density_grid_(self,p_a,p_b,pt)
     call make_u_nabla_density_grid_(self,np_a,np_b,pt)

     call destroy_(pt)
     p_a = max(thresh,p_a)
     p_b = max(thresh,p_b)

     call create_(local_a,n_pt)
     call create_(local_b,n_pt)
     call create_(non_local_a,n_pt,3)
     call create_(non_local_b,n_pt,3)
     local_a=0.0d0
     local_b=0.0d0
     non_local_a=0.0d0
     non_local_b=0.0d0
     if (self%scfdata%dft_non_local_exchange) then
       call add_u_exchange_matrix_(self,p_a,p_b,local_a,local_b,np_a,np_b,non_local_a,non_local_b)
     else if (self%scfdata%dft_exchange /= "none") then
       call add_u_exchange_matrix_(self,p_a,p_b,local_a,local_b)
     end if
     if (self%scfdata%dft_non_local_correlation) then
       call add_u_correlation_matrix_(self,p_a,p_b,local_a,local_b,np_a,np_b,non_local_a,non_local_b)
     else if (self%scfdata%dft_correlation /= "none") then
       call add_u_correlation_matrix_(self,p_a,p_b,local_a,local_b)
     end if

      ! This is -1/2 integral v_xc rho dr.
     call create_(total,n_pt)
     total = local_a*p_a+local_b*p_b
     if (self%scfdata%dft_non_local_exchange .or. self%scfdata%dft_non_local_correlation) then
       total = total + sum(non_local_b(:,:)*np_b(:,:),2) + sum(non_local_a(:,:)*np_a(:,:),2)
     end if
     total = -0.50d0*total
     call destroy_(non_local_b)
     call destroy_(non_local_a)
     call destroy_(local_b)
     call destroy_(local_a)

      ! Now add Exc
     call create_(f,n_pt)
     call u_exchange_functional_(self,p_a,p_b,np_a,np_b,f)
     total = total + f
     call u_correlation_functional_(self,p_a,p_b,np_a,np_b,f)
     total = total + f
     call destroy_(f)
     res = sum(wt*total)
     call destroy_(wt)
     call destroy_(total)

     call destroy_(np_b)
     call destroy_(np_a)
     call destroy_(p_b)
     call destroy_(p_a)

   end function

   function u_ex_corr_energy(self) result(res)
    type(mol_type) :: self
    ! Return the exchange-correlation energy
     real(kind=kind(1.0d0)) :: res
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: pt,np_a,np_b
     real(kind=kind(1.0d0)), dimension(:), pointer :: wt,p_a,p_b,fx,fc
     integer(kind=kind(1)) :: n_pt
     real(kind=kind(1.0d0)) :: THRESH

     call ensure_(tonto,self%basis_info_made,"MOL:u_ex_corr_energy ... no basis info")
     call ensure_(tonto,associated(self%atom),"MOL:u_ex_corr_energy ... no atom list")
     call ensure_(tonto,associated(self%dftgrid),"MOL:u_ex_corr_energy ... need to specify dftgrid for ZORA")
     THRESH = 10.0d0**(-90)
     n_pt = self%dftgrid%n_pts*self%n_atom
     call create_(wt,n_pt)
     call create_(pt,n_pt,3)
     call make_grid_(self%dftgrid,pt,wt,self%atom)
     call make_unrestricted_NOs_(self)

      ! Get the densities and their gradients.
     call create_(p_a,n_pt)
     call create_(p_b,n_pt)
     call make_unrestricted_density_grid_(self,p_a,p_b,pt)

     if (self%scfdata%dft_non_local_exchange .or. self%scfdata%dft_non_local_correlation) then
       call create_(np_a,n_pt,3)
       call create_(np_b,n_pt,3)
       call make_u_nabla_density_grid_(self,np_a,np_b,pt)
     end if

     call destroy_(pt)
     p_a = max(THRESH,p_a)
     p_b = max(THRESH,p_b)
     call create_(fx,n_pt)
     call create_(fc,n_pt)

     call u_exchange_functional_(self,p_a,p_b,np_a,np_b,fx)
     call u_correlation_functional_(self,p_a,p_b,np_a,np_b,fc)
     res = sum(wt*(fx+fc))

     call destroy_(fc)
     call destroy_(fx)
     if (self%scfdata%dft_non_local_exchange .or. self%scfdata%dft_non_local_correlation) then
       call destroy_(np_b)
       call destroy_(np_a)
     end if
     call destroy_(p_b)
     call destroy_(p_a)
     call destroy_(wt)

   end function

   subroutine u_exchange_functional(self,p_a,p_b,np_a,np_b,f)
    type(mol_type) :: self
    ! Calculate the exchange functional.
    ! Input is p_a,p_b,np_a,np_b.  Output is f.
    ! np_a,np_b are optional.
    ! (p means density, np means gradient of the density).
    !
    ! NOTE: When adding a new functional, alter the subroutine
    !       scfdata.foo/read_dft_exchange also.
    !
     real(kind=kind(1.0d0)), dimension(:), intent(in) :: p_a,p_b
     real(kind=kind(1.0d0)), dimension(:) :: f
     real(kind=kind(1.0d0)), dimension(:,:), optional, intent(in) :: np_a,np_b
     character(128) :: ex_kind
     real(kind=kind(1.0d0)) :: alpha

     ex_kind = self%scfdata%dft_exchange
     select case (ex_kind)
       case ("none")
         f = 0.0d0
       case ("slater")
         alpha=2*0.33333333333333333333333d0
         call u_lda_exchange_functional_(self%dftgrid,p_a,p_b,f,alpha)
       case ("xalpha")
         alpha=0.7
         call u_lda_exchange_functional_(self%dftgrid,p_a,p_b,f,alpha)
       case ("becke88")
         call ensure_(tonto,present(np_a),"MOL:u_exchange_functional ... program bug!")
         call ensure_(tonto,present(np_b),"MOL:u_exchange_functional ... program bug!")
         call u_becke88_exchange_functional_(self%dftgrid,p_a,p_b,np_a,np_b,f)
       case ("gill96")
         call ensure_(tonto,present(np_a),"MOL:u_exchange_functional ... program bug!")
         call ensure_(tonto,present(np_b),"MOL:u_exchange_functional ... program bug!")
         call u_gill96_exchange_functional_(self%dftgrid,p_a,p_b,np_a,np_b,f)
       case default
         call die_(tonto,"MOL:u_exchange_functional ... unknown exchange functional")
     end select

   end subroutine

   subroutine u_correlation_functional(self,p_a,p_b,np_a,np_b,f)
    type(mol_type) :: self
    ! Calculate the exchange correlation.
    ! Input is p_a,p_b,np_a,np_b.  Output is f.
    ! np_a,np_b are optional.
    ! (p means density, np means gradient of the density).
    !
    ! NOTE: When adding a new functional, alter the subroutine
    !       scfdata.foo/read_dft_correlation also.
    !
     real(kind=kind(1.0d0)), dimension(:), intent(in) :: p_a,p_b
     real(kind=kind(1.0d0)), dimension(:) :: f
     real(kind=kind(1.0d0)), dimension(:,:), optional, intent(in) :: np_a,np_b
     character(128) :: corr_kind

     corr_kind = self%scfdata%dft_correlation
     select case (corr_kind)
       case ("none")
         f = 0.0d0
       case ("lyp")
         call ensure_(tonto,present(np_a),"MOL:u_correlation_functional ... program bug!")
         call ensure_(tonto,present(np_b),"MOL:u_correlation_functional ... program bug!")
         call u_lyp_correlation_functional_(self%dftgrid,p_a,p_b,np_a,np_b,f)
       case default
         call die_(tonto,"MOL:u_correlation_functional ... unknown exchange functional")
     end select

   end subroutine

   subroutine add_u_exchange_matrix(self,p_a,p_b,local_a,local_b,np_a,np_b,non_local_a,non_local_b)
    type(mol_type) :: self
    ! Get terms necessary to calculate the exchange functional matrix.
    ! Input is p_a,p_b,np_a,np_b.
    ! Output is local_a,local_b,non_local_a,non_local_b.
    ! np_a, np_b and non_local are optional arguments.
    ! (p means density, np means nabla density, a means alpha, b means beta).
    !
    ! NOTE: When adding a new functional, alter the subroutine
    !       scfdata.foo/read_dft_exchange also.
    !
     real(kind=kind(1.0d0)), dimension(:) :: p_a,p_b,local_a,local_b
     real(kind=kind(1.0d0)), dimension(:,:), optional :: np_a,np_b,non_local_a,non_local_b
     real(kind=kind(1.0d0)), dimension(:), pointer :: la,lb
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: nla,nlb
     character(128) :: ex_kind
     real(kind=kind(1.0d0)) :: alpha
     integer(kind=kind(1)) :: n_pt

     ex_kind = self%scfdata%dft_exchange
     n_pt = size(p_a)
     select case (ex_kind)
       case ("none")
        ! Don't add anything.
       case ("slater")
         alpha=2*0.33333333333333333333333d0
         call create_(la,n_pt)
         call create_(lb,n_pt)
         call d_u_lda_exchange_functional_(self%dftgrid,p_a,p_b,la,lb,alpha)
         local_a = local_a + la
         local_b = local_b + lb
         call destroy_(lb)
         call destroy_(la)
       case ("xalpha")
         alpha=0.7
         call create_(la,n_pt)
         call create_(lb,n_pt)
         call d_u_lda_exchange_functional_(self%dftgrid,p_a,p_b,la,lb,alpha)
         local_a = local_a + la
         local_b = local_b + lb
         call destroy_(lb)
         call destroy_(la)
       case ("becke88")
         call ensure_(tonto,present(np_a),"MOL:add_u_exchange_matrix ... program bug!")
         call ensure_(tonto,present(np_b),"MOL:add_u_exchange_matrix ... program bug!")
         call ensure_(tonto,present(non_local_a),"MOL:add_u_exchange_matrix ... program bug!")
         call ensure_(tonto,present(non_local_b),"MOL:add_u_exchange_matrix ... program bug!")
         call create_(la,n_pt)
         call create_(lb,n_pt)
         call create_(nla,n_pt,3)
         call create_(nlb,n_pt,3)
         call d_u_b88_exchange_functional_(self%dftgrid,p_a,p_b,np_a,np_b,la,lb,nla,nlb)
         local_a = local_a + la
         local_b = local_b + lb
         non_local_a = non_local_a + nla
         non_local_b = non_local_b + nlb
         call destroy_(nlb)
         call destroy_(nla)
         call destroy_(lb)
         call destroy_(la)
       case ("gill96")
         call ensure_(tonto,present(np_a),"MOL:add_u_exchange_matrix ... program bug!")
         call ensure_(tonto,present(np_b),"MOL:add_u_exchange_matrix ... program bug!")
         call ensure_(tonto,present(non_local_a),"MOL:add_u_exchange_matrix ... program bug!")
         call ensure_(tonto,present(non_local_b),"MOL:add_u_exchange_matrix ... program bug!")
         call create_(la,n_pt)
         call create_(lb,n_pt)
         call create_(nla,n_pt,3)
         call create_(nlb,n_pt,3)
         call d_u_gill96_exchange_functional_(self%dftgrid,p_a,p_b,np_a,np_b,la,lb,nla,nlb)
         local_a = local_a + la
         local_b = local_b + lb
         non_local_a = non_local_a + nla
         non_local_b = non_local_b + nlb
         call destroy_(nlb)
         call destroy_(nla)
         call destroy_(lb)
         call destroy_(la)
       case default
         call die_(tonto,"MOL:add_u_exchange_matrix ... unknown exchange functional")
     end select

   end subroutine

   subroutine add_u_correlation_matrix(self,p_a,p_b,local_a,local_b,np_a,np_b,non_local_a,non_local_b)
    type(mol_type) :: self
    ! Get terms necessary to calculate the correlation functional matrix.
    ! Input is p_a,p_b,np_a,np_b.
    ! Output is local_a,local_b,non_local_a,non_local_b.
    ! np_a, np_b and non_local are optional arguments.
    ! (p means density, np means nabla density, a means alpha, b means beta).
    !
    ! NOTE: When adding a new functional, alter the subroutine
    !       scfdata.foo/read_dft_correlation also.
    !
     real(kind=kind(1.0d0)), dimension(:) :: p_a,p_b,local_a,local_b
     real(kind=kind(1.0d0)), dimension(:,:), optional :: np_a,np_b,non_local_a,non_local_b
     real(kind=kind(1.0d0)), dimension(:), pointer :: la,lb
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: nla,nlb
     integer(kind=kind(1)) :: n_pt
     character(128) :: corr_kind

     corr_kind = self%scfdata%dft_correlation
     n_pt = size(p_a)
     select case (corr_kind)
       case ("none")
        ! Don't add anything.
!       case ("vwn")
       case ("lyp")
         call ensure_(tonto,present(np_a),"MOL:add_u_correlation_matrix ... program bug!")
         call ensure_(tonto,present(np_b),"MOL:add_u_correlation_matrix ... program bug!")
         call ensure_(tonto,present(non_local_a),"MOL:add_u_correlation_matrix ... program bug!")
         call ensure_(tonto,present(non_local_b),"MOL:add_u_correlation_matrix ... program bug!")
         call create_(la,n_pt)
         call create_(lb,n_pt)
         call create_(nla,n_pt,3)
         call create_(nlb,n_pt,3)
         call d_u_lyp_correlation_functional_(self%dftgrid,p_a,p_b,np_a,np_b,la,lb,nla,nlb)
         local_a = local_a + la
         local_b = local_b + lb
         non_local_a = non_local_a + nla
         non_local_b = non_local_b + nlb
         call destroy_(nlb)
         call destroy_(nla)
         call destroy_(lb)
         call destroy_(la)
       case default
         call die_(tonto,"MOL:add_u_correlation_matrix ... unknown correlation functional")
     end select

   end subroutine

!  **********************
!  Two electron integrals
!  **********************

   subroutine get_ERI_integrals(self)
    type(mol_type) :: self
    ! Get the electron repulsion integrals on disk. If the integral
    ! file is already there, do nothing.
     type(archive_type) :: eri_archive,ind_archive

     call set_(eri_archive,self%name,"eri_integrals")
     call set_(ind_archive,self%name,"eri_index")
     if ((.not. exists_(eri_archive)) .or. (.not. exists_(ind_archive))) then
        call open_(eri_archive,for="write-only",buffered=.true.,type="real")
        call open_(ind_archive,for="write-only",buffered=.true.,type="int")
        call make_eri_integrals_(self,eri_archive,ind_archive)
        call close_(ind_archive)
        call close_(eri_archive)
     end if

   end subroutine

   subroutine make_eri_integrals(self,eri_archive,eri_index)
    type(mol_type) :: self
    ! Calculate the electron repulsion integrals (ERI's) over all basis functions
    ! Outputs to archive "eri_archive".  Note that index coincidence factors are
    ! included.
     type(archive_type) :: eri_archive,eri_index
     type(shell4_type) :: sh4
     real(kind=kind(1.0d0)), dimension(:,:,:,:), pointer :: ERI
     integer(kind=kind(1)) :: q,a,b,c,d,skipped,atom_a,atom_b,atom_c,atom_d
     real(kind=kind(1.0d0)) :: factor

   call ensure_(tonto,self%basis_info_made,"MOL:make_eri_integrals ... no basis info")
   call ensure_(tonto,associated(self%atom),"MOL:make_eri_integrals ... no atom list")
     skipped = 0
     if (self%scfdata%output) then
       call set_real_style_(stdout,"e")
       call show_(stdout,"Skipping electron repulsion integrals less than ",self%scfdata%eri_limit)
       call set_real_style_(stdout,"f")
     end if
     if (self%scfdata%nddo) then
        do q = 1, n_shell_quartets_(self)
          call get_shell_quartet_indexes_(self,q,a,b,c,d,atom_a,atom_b,atom_c,atom_d)
          if (atom_a==atom_b .and. atom_c==atom_d) then
            call get_shell_quartet_(self,sh4,q,a,b,c,d)
          else
            skipped = skipped + 1
            cycle
          end if
          if (skip_ERI_(sh4)) then
            skipped = skipped + 1
            call destroy_ptr_part_(sh4)
            cycle
          end if
          call create_(ERI,sh4%a%n_comp,sh4%b%n_comp,sh4%c%n_comp,sh4%d%n_comp)
          call get_ERI_(sh4,ERI)
          factor = 1.0d0
          if (a==b) factor = 0.50d0                     ! Coincidence factors
          if (c==d) factor = 0.50d0 * factor
          if (a==c .and. b==d) factor = 0.50d0 * factor
          ERI = factor * ERI
          call write_(eri_archive%file,ERI)
          call write_(eri_index%file,q)
          call destroy_(ERI)
          call destroy_ptr_part_(sh4)
        end do
     else if (self%scfdata%nudo) then
        do q = 1, n_shell_quartets_(self)
          call get_shell_quartet_indexes_(self,q,a,b,c,d,atom_a,atom_b,atom_c,atom_d)
          if (bonded_(self%atom,atom_a,atom_b) .and. bonded_(self%atom,atom_c,atom_d)) then
            call get_shell_quartet_(self,sh4,q,a,b,c,d)
          else
            skipped = skipped + 1
            cycle
          end if
          if (skip_ERI_(sh4)) then
            skipped = skipped + 1
            call destroy_ptr_part_(sh4)
            cycle
          end if
          call create_(ERI,sh4%a%n_comp,sh4%b%n_comp,sh4%c%n_comp,sh4%d%n_comp)
          call get_ERI_(sh4,ERI)
          factor = 1.0d0
          if (a==b) factor = 0.50d0                     ! Coincidence factors
          if (c==d) factor = 0.50d0 * factor
          if (a==c .and. b==d) factor = 0.50d0 * factor
          ERI = factor * ERI
          call write_(eri_archive%file,ERI)
          call write_(eri_index%file,q)
          call destroy_(ERI)
          call destroy_ptr_part_(sh4)
        end do
     else
        do q = 1, n_shell_quartets_(self)
          call get_shell_quartet_(self,sh4,q,a,b,c,d)
          if (skip_ERI_(sh4,self%scfdata%eri_limit)) then
            skipped = skipped + 1
            call destroy_ptr_part_(sh4)
            cycle
          end if
          call create_(ERI,sh4%a%n_comp,sh4%b%n_comp,sh4%c%n_comp,sh4%d%n_comp)
          call get_ERI_(sh4,ERI)
          factor=1.0d0
          if (a==b) factor = 0.50d0                     ! Coincidence factors
          if (c==d) factor = 0.50d0 * factor
          if (a==c .and. b==d) factor = 0.50d0 * factor
          ERI = factor * ERI
          call write_(eri_archive%file,ERI)
          call write_(eri_index%file,q)
          call destroy_(ERI)
          call destroy_ptr_part_(sh4)
        end do
     end if
     if (self%scfdata%output) then
       a = n_shell_quartets_(self)
       call text_(stdout,"Skipped "//trim(to_str_(skipped))//" out of "//trim(to_str_(a))//" blocks.")
     end if
     call write_(eri_index%file,n_shell_quartets_(self) + 1 )

   end subroutine

   subroutine get_spin_orbit_integrals(self)
    type(mol_type) :: self
    ! Get the spin orbit integrals on disk. If the integral files are
    ! file is already there, do nothing.
     type(archive_type) :: SOx_archive,SOy_archive,SOz_archive,ind_archive

     call set_(SOx_archive,self%name,"SOx_integrals")
     call set_(SOy_archive,self%name,"SOy_integrals")
     call set_(SOz_archive,self%name,"SOz_integrals")
     call set_(ind_archive,self%name,"SO_indices")
     if (.not. exists_(SOx_archive)) then
        call open_(SOx_archive,for="write-only",buffered=.true.,type="real")
        call open_(SOy_archive,for="write-only",buffered=.true.,type="real")
        call open_(SOz_archive,for="write-only",buffered=.true.,type="real")
        call open_(ind_archive,for="write-only",buffered=.true.,type="int")
        call make_spin_orbit_integrals_(self,SOx_archive,SOy_archive,SOz_archive,ind_archive)
        call close_(SOx_archive)
        call close_(SOy_archive)
        call close_(SOz_archive)
        call close_(ind_archive)
     end if

   end subroutine

   subroutine make_spin_orbit_integrals(self,SOx_archive,SOy_archive,SOz_archive,ind_archive)
    type(mol_type) :: self
    ! Calculate the spin orbit integrals. Outputs the spin same-orbit integrals
    ! to for each component i to "SOi_archive". The shell quartet index for
    ! these integrals are put in "ind_archive". Note that index coincidence
    ! factors are included.
     type(archive_type) :: SOx_archive,SOy_archive,SOz_archive,ind_archive
     type(shell4_type) :: sh4
     real(kind=kind(1.0d0)), dimension(:,:,:,:), pointer :: Sx,Sy,Sz,Ox,Oy,Oz
     integer(kind=kind(1)) :: q,a,b,c,d,skipped,atom_a,atom_b,atom_c,atom_d,na,nb,nc,nd
     real(kind=kind(1.0d0)) :: factor
     character(128) :: cutoff

   call ensure_(tonto,self%basis_info_made,"MOL:make_spin_orbit_integrals ... no basis info")
   call ensure_(tonto,associated(self%atom),"MOL:make_spin_orbit_integrals ... no atom list")
     call from_real_(cutoff,1.0d-15)
     skipped = 0
     if (self%scfdata%output) then
       call text_(stdout,"Skipping electron repulsion integrals less than " // trim(cutoff) // ".")
     end if
     do q = 1, n_shell_quartets_(self)
       if (self%scfdata%nddo) then
         call get_shell_quartet_indexes_(self,q,a,b,c,d,atom_a,atom_b,atom_c,atom_d)
         if (atom_a==atom_b .and. atom_c==atom_d) then
           call get_shell_quartet_(self,sh4,q,a,b,c,d)
         else
           skipped = skipped + 1
           cycle
         end if
       else if (self%scfdata%nudo) then
         call get_shell_quartet_indexes_(self,q,a,b,c,d,atom_a,atom_b,atom_c,atom_d)
         if (bonded_(self%atom,atom_a,atom_b) .and. bonded_(self%atom,atom_c,atom_d)) then
           call get_shell_quartet_(self,sh4,q,a,b,c,d)
         else
           skipped = skipped + 1
           cycle
         end if
       else
         call get_shell_quartet_(self,sh4,q,a,b,c,d)
       end if
       if (skip_ERI_(sh4)) then
         skipped = skipped + 1
         call destroy_ptr_part_(sh4)
         cycle
       end if
       na = sh4%a%n_comp; nb = sh4%b%n_comp
       nc = sh4%c%n_comp; nd = sh4%d%n_comp
       call create_(Sx,na,nb,nc,nd); call create_(Sy,na,nb,nc,nd); call create_(Sz,na,nb,nc,nd)
       call create_(Ox,na,nb,nc,nd); call create_(Oy,na,nb,nc,nd); call create_(Oz,na,nb,nc,nd)
       call make_spin_orbit_ints_(sh4,Sx,Sy,Sz,Ox,Oy,Oz)
       factor = 1.0d0
       if (a==b) factor = 0.50d0                     ! Coincidence factors
       if (c==d) factor = 0.50d0 * factor
       if (a==c .and. b==d) factor = 0.50d0 * factor
       Sx = factor*Sx; Sy = factor*Sy; Sz = factor*Sz
       Ox = factor*Ox; Oy = factor*Oy; Oz = factor*Oz
       call write_(SOx_archive%file,Sx); call write_(SOx_archive%file,Ox)
       call write_(SOy_archive%file,Sy); call write_(SOy_archive%file,Oy)
       call write_(SOz_archive%file,Sz); call write_(SOz_archive%file,Oz)
       call write_(ind_archive%file,q)
       call destroy_(Oz); call destroy_(Oy); call destroy_(Ox)
       call destroy_(Sz); call destroy_(Sy); call destroy_(Sx)
       call destroy_ptr_part_(sh4)
     end do
     if (self%scfdata%output) then
       a = n_shell_quartets_(self)
       call text_(stdout,"Skipped "//trim(to_str_(skipped))//" out of "//trim(to_str_(a))//" blocks.")
     end if
     call write_(ind_archive%file,n_shell_quartets_(self) + 1 )

   end subroutine

!   make_r_pie_JK_direct(J,K,P)
!   ! Make the real coulomb matrix "J" and exchange matrix "K" matrix from a
!   ! symmetric density matrix "P" directly, using the projective integral
!   ! expansions (PIE) method. See Mayer, CPL 332, p. 381-388 (2000), eqn (12).
!   ! (c) Dylan Jayatilaka, Budapest September 2002.
!     J,K,P :: REALMAT
!     S,SS,S_nxn,S_inv,ERI,JJ,KK :: REALMAT*
!     XBXD :: REALMAT4*
!     D,Q :: REALMATVEC*
!     DD  :: REALMATMAT*
!     sh4 :: type(shell4_type)
!     a,b,c,d,sa,sb,sc,sd,n,n1,n2,i,j :: integer(kind=kind(1))
!     at_b,at_d,atom_a,atom_b,atom_c,atom_d :: integer(kind=kind(1))
!     fa,la,fb,lb,fc,lc,fd,ld,fsa,lsa,fsc,lsc,ffa,lfa,ffc,lfc :: integer(kind=kind(1))
!     diatom,n_diatom,fs_diatom,ls_diatom,ff_diatom,lf_diatom :: integer(kind=kind(1)), dimension(2)
!     S.create(.n_bf,.n_bf)
!     .get_overlap_matrix(S)
!     D1.create(2)
!     D2.create(2,2)
!     nullify(S_inv); nullify(S_nxn); nullify(SS)
!     atom_b = 0; atom_d = 0
!     J = 0.0d0
!     K = 0.0d0
!     do sb = 1,.n_shell
!     do sd = 1,.n_shell
!        if (sb==sd) cycle ! diagonal case below ?
!        fb  = .first_basis_fn_for_shell(sb); lb = .last_basis_fn_for_shell(sb)
!        fd  = .first_basis_fn_for_shell(sd); ld = .last_basis_fn_for_shell(sd)
!        at_b = .atom_for_shell(sb)
!        at_d = .atom_for_shell(sd)
!        ! Make the projectors Q(*), only if needed
!        if (at_b/=atom_b .or. at_d/=atom_d) then
!           atom_b = at_b; n1 = .atom(atom_b).n_bf
!           atom_d = at_d; n2 = .atom(atom_d).n_bf
!           diatom = [atom_b,atom_d]
!           n_diatom = [n1,n2]
!           n = n1 + n2
!           DD.destroy_ptr_part; D.destroy_ptr_part; Q.destroy_ptr_part
!           Q(1).element.create(.n_bf,n1); Q(2).element.create(.n_bf,n2)
!           S_inv.create(n,n)
!           S_nxn.create(n,n)
!           .AO_subspace_set(S_nxn,S,diatom,diatom)
!           S_inv.to_inverse_of(S_nxn)
!           S_nxn.destroy
!           SS.create(.n_bf,n)
!           .AO_subspace_set(SS,S,col_atom=diatom)
!           Q(1).element = matmul(SS,S_inv(:,   1:n1))
!           Q(2).element = matmul(SS,S_inv(:,n2+1:  ))
!           SS.destroy
!           S_inv.destroy
!           do i = 1,2
!              D(i).element.create(.n_bf,n_diatom(i))
!              D(i).element = matmul(P,Q(i).element)
!           end
!           do i = 1,2
!           do j = 1,2
!              DD(i,j).element.create(n_diatom(i),n_diatom(j))
!              DD(i,j).element = matmul(transpose(Q(i).element),D(j).element)
!           end
!           end
!           do i = 1,2
!              fs_diatom(i) = .first_shell_for_atom(diatom(i))
!              ls_diatom(i) =  .last_shell_for_atom(diatom(i))
!              ff_diatom(i) = .first_basis_fn_for_atom(diatom(i))
!              lf_diatom(i) =  .last_basis_fn_for_atom(diatom(i))
!           end
!        end
!        ! Loop over diatom subspaces on centers A & C
!        do atom_a = 1,2
!        do atom_c = 1,2
!           fsa = fs_diatom(atom_a); lsa = ls_diatom(atom_a)
!           fsc = fs_diatom(atom_c); lsc = ls_diatom(atom_c)
!           ffa = ff_diatom(atom_a); lfa = lf_diatom(atom_a)
!           ffc = ff_diatom(atom_c); lfc = lf_diatom(atom_c)
!           JJ.create(ffa:lfa,fb:lb);   JJ = 0.0d0
!           KK.create(ffa:lfa,ffc:lfc); KK = 0.0d0
!           do sa = fsa,lsa
!           do sc = fsc,lsc
!              .get_shell_quartet(sh4,sa,sb,sc,sd) ! B B B D
!              XBXD.create(sh4.a.n_comp,sh4.b.n_comp,sh4.c.n_comp,sh4.d.n_comp)
!              sh4.get_ERI(XBXD)
!              sh4.destroy_ptr_part
!              fa = .first_basis_fn_for_shell(sa); la = .last_basis_fn_for_shell(sa)
!              fc = .first_basis_fn_for_shell(sc); lc = .last_basis_fn_for_shell(sc)
!              JJ = 0.0d0
!              do a = fa,la
!              do b = fb,lb
!                 ERI => XBXD(a,b,:,:)
!                 JJ(a,b) = JJ(a,b) + ERI.trace_product_with(D1(atom_c).element(fd:ld,ffc:lfc))
!              end
!              end
!              J = J + matmul(Q(atom_a).element,JJ)
!              KK = 0.0d0
!              do a = fa,la
!              do c = fc,lc
!                 ERI => XBXD(a,:,c,:)
!                 KK(a,c) = KK(a,c) + ERI.trace_product_with(P(fd:ld,fb:lb))
!              end
!              end
!              K = K + matmul(Q(atom_a).element,matmul(KK,transpose(Q(atom_c).element)))
!              do b = fb,lb
!              do d = fd,ld
!                 ERI => XBXD(:,b,:,d)
!                 K(b,d) = K(b,d) + &
!                    ERI.trace_product_with(DD(atom_c,atom_a).element(ffc:lfc,ffa:lfa))
!              end
!              end
!              XBXD.destroy
!           end
!           end
!           KK.destroy
!           JJ.destroy
!        end
!        end
!     end
!     end
!     D2.destroy; D1.destroy
!     S.destroy
!     J.symmetrize
!     K = 0.50d0*K
!   end

!   make_2_center_diatom_ERI_array(v,shell_b,shell_d)
!   ! Calculate the 4 dimensional diatom electron repulsion integral array "v".
!   ! v(a,b,c,d) = [ab|cd] is a restricted set of the two electron integrals,
!   ! with "b" and "d" being the indices of basis functions belonging to shells
!   ! "shell_b" and "shell_d" respectively, while "a" and "c" the indices of
!   ! basis functions belonging to *all* shells on either atom B or D, which are
!   ! the atoms on which shells "shell_b" and "shell_d" are found.
!     v :: REALMAT4
!     shell_b,shell_d :: integer(kind=kind(1))
!     a,aa,c,cc,fa,la,fb,lb,fc,lc,fd,ld :: integer(kind=kind(1))
!     sh :: type(shell2_type)
!     shell_atom :: integer(kind=kind(1)), dimension(2)
!     sh4 :: type(shell4_type)
!     ERI :: REALMAT4*
!   call ensure_(tonto,.bases_are_all_resolved,"no basis set")
!   call ensure_(tonto,v.dim2==.atom(.atom_for_shell(shell_b)).n_bf,"wrong dim 3 for v")
!   call ensure_(tonto,v.dim4==.atom(.atom_for_shell(shell_d)).n_bf,"wrong dim 4 for v")
!     sb = shell_b; b = .atom_for_shell(sb)
!     sd = shell_d; d = .atom_for_shell(sd)
!     shell_atom(1:2) = [b,d]
!     fb = .first_shell_for_atom(b)
!     lb =  .last_shell_for_atom(b)
!     fd = .first_shell_for_atom(d)
!     ld =  .last_shell_for_atom(d)
!     v = 0.0d0
!     do aa = 1,2
!        a = shell_atom(aa)
!        fa = .first_shell_for_atom(a)
!        la =  .last_shell_for_atom(a)
!        do sa = fa,la
!           do cc = 1,2
!              c = shell_atom(cc)
!              fc = .first_shell_for_atom(c)
!              lc =  .last_shell_for_atom(c)
!              do sc = fb,lb
!                 .get_shell_quartet(sh4,sa,sb,sc,sd)
!                 if (sh4.skip_ERI(.scfdata.eri_limit)) then
!                    sh4.destroy_ptr_part
!                    cycle
!                 end
!                 ERI.create(sh4.a.n_comp,sh4.b.n_comp,sh4.c.n_comp,sh4.d.n_comp)
!                 sh4.get_ERI(ERI)
!                 v(fa:la,fb:lb,fc:lc,fd:ld) = ERI
!              end
!           end
!        end
!     end
!   end

!  **************************
!  Crystal structure routines
!  **************************

   subroutine make_ft(self,res,dens,k_pts)
    type(mol_type) :: self
    ! Fourier transform of a density described by AO density matrix dens
    ! evaluated at a series of reciprocal space points k_pts
    ! Size of res is [size(k_pts,1)]
      real(kind=kind(1.0d0)), dimension(:,:), target :: dens
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: k_pts
      complex(kind=kind((1.0d0,1.0d0))), dimension(:), intent(out) :: res
      type(shell2_type) :: sh
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:), pointer :: ft_ab
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: dens_ba
      real(kind=kind(1.0d0)), dimension(:), pointer :: max_P
      real(kind=kind(1.0d0)) :: cutoff
      integer(kind=kind(1)) :: k_max,k,fa,fb,la,lb,q,atom_a,atom_b,start,step

      call ensure_(tonto,self%basis_info_made,"MOL:make_ft ... no basis info")
      call ensure_(tonto,associated(self%atom),"MOL:make_ft ... no atom list")
      k_max = size(k_pts,1)
      res = 0.0d0
      cutoff = 10.0d0**(-10) / self%n_shell_pairs
      start = 1 + this_proc_(tonto_parallel)
      step = n_proc_(tonto_parallel)
      call create_(max_P,self%n_shell_pairs)
      call make_max_density_elements_(self,max_P,dens)
      do q=start,self%n_shell_pairs,step
         call get_shell_pair_(self,sh,q,fa,la,fb,lb,atom_a,atom_b)
         if (skip_ft_(sh,max_P(q)*sh%a%n_comp*sh%b%n_comp,cutoff)) then
           call destroy_ptr_part_(sh)
           cycle
         end if
         call create_(ft_ab,k_max,sh%a%n_comp,sh%b%n_comp)
         call make_ft_pair_(self,ft_ab,k_pts,sh,atom_a,atom_b)
!         dens_ba => dens(fb:lb,fa:la) ! P^dagger
         dens_ba => dens(fa:la,fb:lb)
         if (fa/=fb) then  ! count off-diagonals twice.
           do k = 1,k_max
!              res(k) = res(k) + 2.0d0*ft_ab(k,:,:).trace_of_product(dens_ba)
              res(k) = res(k) + 2.0d0*sum(ft_ab(k,:,:)*dens_ba(:,:))
           end do
         else
           do k = 1,k_max
!              res(k) = res(k) + ft_ab(k,:,:).trace_of_product(dens_ba)
              res(k) = res(k) + sum(ft_ab(k,:,:)*dens_ba(:,:))
           end do
         end if
         call destroy_(ft_ab)
         call destroy_ptr_part_(sh)
      end do
      call destroy_(max_P)
      call sum_vectors_(tonto_parallel,res)

   end subroutine

   subroutine make_ft_1(self,res,dens,k_pts)
    type(mol_type) :: self
    ! Fourier transform of a density described by AO density matrix dens
    ! evaluated at a series of reciprocal space points k_pts
    ! Size of res is [size(k_pts,1)].
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), target :: dens
      real(kind=kind(1.0d0)), dimension(:,:) :: k_pts
      complex(kind=kind((1.0d0,1.0d0))), dimension(:) :: res
      integer(kind=kind(1)) :: k_max,q,k,fa,la,fb,lb,atom_a,atom_b
      type(shell2_type) :: sh
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:), pointer :: ft_ab
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: dens_ba
      real(kind=kind(1.0d0)) :: cutoff

      call ensure_(tonto,self%basis_info_made,"MOL:make_ft_1 ... no basis info")
      call ensure_(tonto,associated(self%atom),"MOL:make_ft_1 ... no atom list")
      k_max = size(k_pts,1)
      res = 0.0d0
      cutoff = 10.0d0**(-10) / self%n_shell_pairs
      do q = 1,self%n_shell_pairs
         call get_shell_pair_(self,sh,q,fa,la,fb,lb,atom_a,atom_b)
         if (skip_ft_(sh,cutoff)) then
           call destroy_ptr_part_(sh)
           cycle
         end if
         call create_(ft_ab,k_max,sh%a%n_comp,sh%b%n_comp)
         call make_ft_pair_(self,ft_ab,k_pts,sh,atom_a,atom_b)
         dens_ba => dens(fb:lb,fa:la)
         if (fa/=fb) then  ! count off-diagonals twice.
           do k = 1,k_max
              res(k) = res(k) + 2.0d0*trace_of_product_(ft_ab(k,:,:),dens_ba)
           end do
         else
           do k = 1,k_max
              res(k) = res(k) + trace_of_product_(ft_ab(k,:,:),dens_ba)
           end do
         end if
         call destroy_(ft_ab)
         call destroy_ptr_part_(sh)
      end do

   end subroutine

   subroutine make_ft_deriv_U(self,res,dens,k_pts)
    type(mol_type) :: self
    ! Fourier transform of a density described by AO density matrix dens
    ! evaluated at a series of reciprocal space points k_pts
    ! Size of res is [k_max,n_atom*6]
     real(kind=kind(1.0d0)), dimension(:,:), target :: dens
     real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: k_pts
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), target :: res
     complex(kind=kind((1.0d0,1.0d0))) :: ft_ab2,tmp1,tmp2,tmpa1,tmpa2,tmpa3,tmpa4,tmpa5,tmpa6
     integer(kind=kind(1)) :: k_max,k,fa,fb,la,lb,q,atom_a,atom_b,basea,baseb
     real(kind=kind(1.0d0)) :: delta,g,separation,x,y,z,z2
     real(kind=kind(1.0d0)), dimension(3) :: diff
     type(shell2_type) :: sh
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:), pointer :: ft_ab
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: dens_ba
     real(kind=kind(1.0d0)) :: cutoff

   call ensure_(tonto,self%basis_info_made,"MOL:make_ft_deriv_U ... no basis info")
   call ensure_(tonto,associated(self%atom),"MOL:make_ft_deriv_U ... no atom list")
     k_max = size(k_pts,1)
     res = 0.0d0
     cutoff = 10.0d0**(-10) / self%n_shell_pairs
     do q = 1, self%n_shell_pairs
       call get_shell_pair_(self,sh,q,fa,la,fb,lb,atom_a,atom_b)
       if (skip_ft_(sh,cutoff)) then
         call destroy_ptr_part_(sh)
         cycle
       end if
       call create_(ft_ab,k_max,sh%a%n_comp,sh%b%n_comp)
       call make_ft_pair_(self,ft_ab,k_pts,sh,atom_a,atom_b)
       dens_ba => dens(fb:lb,fa:la)
       if (fa/=fb) ft_ab = 2.0d0 * ft_ab  ! count off-diagonals twice.
       diff = self%atom(atom_a)%pos-self%atom(atom_b)%pos
       separation = norm_(diff)
       g=0.50d0
       if (separation > 2.5d0) g=0.25d0
       g = g * (-0.50d0)  ! k_pts is 2pi * Bh so formula was divided by 4pi^2.

       basea = (atom_a-1) * 6
       if (atom_a == atom_b) then
         delta = 2.0d0 * g
         do k = 1,k_max
           ft_ab2 = trace_of_product_(ft_ab(k,:,:),dens_ba) * delta
           x = k_pts(k,1)            ! SBh_x
           y = k_pts(k,2)            ! SBh_y
           z = k_pts(k,3)            ! SBh_z
           tmp1 = ft_ab2 * x
           tmp2 = ft_ab2 * y
           z2 = 2.0d0 * z
           res(basea+1,k) = res(basea+1,k) + tmp1 * x
           res(basea+2,k) = res(basea+2,k) + tmp1 * y * 2.0d0
           res(basea+3,k) = res(basea+3,k) + tmp1 * z2
           res(basea+4,k) = res(basea+4,k) + tmp2 * y
           res(basea+5,k) = res(basea+5,k) + tmp2 * z2
           res(basea+6,k) = res(basea+6,k) + ft_ab2 * z * z
         end do
       else
         baseb = (atom_b-1) * 6
         do k = 1,k_max
           ft_ab2 = trace_of_product_(ft_ab(k,:,:),dens_ba) * g
           x = k_pts(k,1)            ! SBh_x
           y = k_pts(k,2)            ! SBh_y
           z = k_pts(k,3)            ! SBh_z
           tmp1 = ft_ab2 * x
           tmp2 = ft_ab2 * y
           z2 = 2.0d0 * z
           tmpa1 = tmp1 * x
           tmpa2 = tmp1 * y * 2.0d0
           tmpa3 = tmp1 * z2
           tmpa4 = tmp2 * y
           tmpa5 = tmp2 * z2
           tmpa6 = ft_ab2 * z * z
           res(basea+1,k) = res(basea+1,k) + tmpa1
           res(basea+2,k) = res(basea+2,k) + tmpa2
           res(basea+3,k) = res(basea+3,k) + tmpa3
           res(basea+4,k) = res(basea+4,k) + tmpa4
           res(basea+5,k) = res(basea+5,k) + tmpa5
           res(basea+6,k) = res(basea+6,k) + tmpa6
           res(baseb+1,k) = res(baseb+1,k) + tmpa1
           res(baseb+2,k) = res(baseb+2,k) + tmpa2
           res(baseb+3,k) = res(baseb+3,k) + tmpa3
           res(baseb+4,k) = res(baseb+4,k) + tmpa4
           res(baseb+5,k) = res(baseb+5,k) + tmpa5
           res(baseb+6,k) = res(baseb+6,k) + tmpa6
         end do
       end if
       call destroy_(ft_ab)
       call destroy_ptr_part_(sh)
     end do

   end subroutine

   subroutine make_ft_deriv_U_1(self,res,dens,k_pts)
    type(mol_type) :: self
    ! Fourier transform of a density described by AO density matrix dens
    ! evaluated at a series of reciprocal space points k_pts
    ! Size of res is [k_max,n_atom*6]
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), target :: dens
     real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: k_pts
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), target :: res
     complex(kind=kind((1.0d0,1.0d0))) :: ft_ab2,tmp1,tmp2
     integer(kind=kind(1)) :: k_max,k,fa,fb,la,lb,q,atom_a,atom_b,basea,baseb
     real(kind=kind(1.0d0)) :: delta,g,separation,x,y,z
     real(kind=kind(1.0d0)), dimension(3) :: diff
     type(shell2_type) :: sh
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:), pointer :: ft_ab
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: dens_ba
     real(kind=kind(1.0d0)) :: cutoff

   call ensure_(tonto,self%basis_info_made,"MOL:make_ft_deriv_U_1 ... no basis info")
   call ensure_(tonto,associated(self%atom),"MOL:make_ft_deriv_U_1 ... no atom list")
     k_max = size(k_pts,1)
     res = 0.0d0
     cutoff = 10.0d0**(-10) / self%n_shell_pairs
     do q = 1, self%n_shell_pairs
       call get_shell_pair_(self,sh,q,fa,la,fb,lb,atom_a,atom_b)
       if (skip_ft_(sh,cutoff)) then
         call destroy_ptr_part_(sh)
         cycle
       end if
       call create_(ft_ab,k_max,sh%a%n_comp,sh%b%n_comp)
       call make_ft_pair_(self,ft_ab,k_pts,sh,atom_a,atom_b)
       dens_ba => dens(fb:lb,fa:la)
       if (fa/=fb) ft_ab = 2.0d0 * ft_ab  ! count off-diagonals twice.

       diff = self%atom(atom_a)%pos-self%atom(atom_b)%pos
       separation = norm_(diff)
       g=0.50d0
       if (separation > 2.5d0) g=0.25d0
       g = g * (-0.50d0)  ! k_pts is 2pi * Bh so formula was divided by 4pi^2.

       basea = (atom_a-1) * 6
       if (atom_a == atom_b) then
         delta = 2.0d0 * g
         do k = 1,k_max
           ft_ab2 = trace_of_product_(ft_ab(k,:,:),dens_ba) * delta
           x = k_pts(k,1)            ! SBh_x
           y = k_pts(k,2)            ! SBh_y
           z = k_pts(k,3)            ! SBh_z
           tmp1 = ft_ab2 * x
           tmp2 = ft_ab2 * y
           res(basea+1,k) = res(basea+1,k) + tmp1 * x
           res(basea+2,k) = res(basea+2,k) + tmp1 * y
           res(basea+3,k) = res(basea+3,k) + tmp1 * z
           res(basea+4,k) = res(basea+4,k) + tmp2 * y
           res(basea+5,k) = res(basea+5,k) + tmp2 * z
           res(basea+6,k) = res(basea+6,k) + ft_ab2 * z * z
         end do
       else
         baseb = (atom_b-1) * 6
         do k = 1,k_max
           ft_ab2 = trace_of_product_(ft_ab(k,:,:),dens_ba) * g
           x = k_pts(k,1)            ! SBh_x
           y = k_pts(k,2)            ! SBh_y
           z = k_pts(k,3)            ! SBh_z
           tmp1 = ft_ab2 * x
           tmp2 = ft_ab2 * y
           res(basea+1,k) = res(basea+1,k) + tmp1 * x
           res(basea+2,k) = res(basea+2,k) + tmp1 * y
           res(basea+3,k) = res(basea+3,k) + tmp1 * z
           res(basea+4,k) = res(basea+4,k) + tmp2 * y
           res(basea+5,k) = res(basea+5,k) + tmp2 * z
           res(basea+6,k) = res(basea+6,k) + ft_ab2 * z * z
           res(baseb+1,k) = res(baseb+1,k) + tmp1 * x
           res(baseb+2,k) = res(baseb+2,k) + tmp1 * y
           res(baseb+3,k) = res(baseb+3,k) + tmp1 * z
           res(baseb+4,k) = res(baseb+4,k) + tmp2 * y
           res(baseb+5,k) = res(baseb+5,k) + tmp2 * z
           res(baseb+6,k) = res(baseb+6,k) + ft_ab2 * z * z
         end do
       end if
       call destroy_(ft_ab)
       call destroy_ptr_part_(sh)
     end do

   end subroutine

   subroutine make_ft_pair(self,ft_ab,k_pts,sh,atom_a,atom_b)
    type(mol_type) :: self
    ! Make_ft for one pair of gaussians.
    ! Includes partitioning and thermal smearing.
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:) :: ft_ab
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: k_pts
      type(shell2_type), intent(in) :: sh
      integer(kind=kind(1)), intent(in) :: atom_a,atom_b
      logical(kind=kind(.true.)) :: is_zero
       real(kind=kind(1.0d0)), dimension(:,:,:), pointer :: t
       real(kind=kind(1.0d0)), dimension(:,:), pointer :: p
      integer(kind=kind(1)) :: k_max

   call ensure_(tonto,associated(self%crystal),"MOL:make_ft_pair ... no crystal data")
      k_max = size(k_pts,1)
      call create_(p,sh%a%n_cc,sh%b%n_cc)
      call make_partition_factors_(self,p,sh,atom_a,atom_b,is_zero)
      if (.not. is_zero) then
        call create_(t,k_max,sh%a%n_cc,sh%b%n_cc)
        call thermal_smearing_correction_(self,t,k_pts,atom_a,atom_b,sh)
        call make_ft_(sh,ft_ab,k_pts,t,p)
        call destroy_(t)
      else
        ft_ab = 0.0d0
      end if
      call destroy_(p)

   end subroutine

   subroutine make_partition_factors(self,p,sh,atom_a,atom_b,is_zero)
    type(mol_type) :: self
    ! Make the partitition factors "p" for a particular thermal smearing "model",
    ! for a given "sh" pair with atom centres "atom_a" and "atom_b".
       real(kind=kind(1.0d0)), dimension(:,:) :: p
      type(shell2_type), intent(in) :: sh
      integer(kind=kind(1)), intent(in) :: atom_a,atom_b
      logical(kind=kind(.true.)), intent(out), optional :: is_zero
      real(kind=kind(1.0d0)) :: ra,rb,fa,fb

   call ensure_(tonto,associated(self%crystal),"MOL:make_partition_factors ... no crystal data")
      ra = self%crystal%repetition_factor(atom_a)
      rb = self%crystal%repetition_factor(atom_b)
      if (present(is_zero)) then
        if (ra<1 .and. rb<1) then
          is_zero = .true.
            return
        else
          is_zero = .false.
        end if
      end if
      if (ra<1) then; fa = 0.0d0
      else;           fa = 1.0d0/ra
      end if
      if (rb<1) then; fb = 0.0d0
      else;           fb = 1.0d0/rb
      end if
      select case (trim(self%crystal%partition_model))
         case (" ","none")
            p = 1.0d0
         case ("mulliken")
            p = 0.50d0 * (fa + fb)
         case ("gaussian")
            call make_gaussian_partition_(sh,p,fa,fb)
         case default
            call die_(tonto,"MOL:make_partition_factors ... unknown model, "//trim(self%crystal%partition_model))
      end select

   end subroutine

   subroutine thermal_smearing_correction(self,res,k_pts,a,b,sh)
    type(mol_type) :: self
    ! Makes the thermal smearing correction term.  Note that res
    ! is done over the contraction coefficients.
    ! Dimensions of "res" are [k_max,.a.n_cc,.b.n_cc].
     real(kind=kind(1.0d0)), dimension(:,:,:), intent(out) :: res
     real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: k_pts
     integer(kind=kind(1)), intent(in) :: a,b
     type(shell2_type), intent(in) :: sh
     real(kind=kind(1.0d0)), dimension(3,3) :: Ua,Ub,Uab
     real(kind=kind(1.0d0)), dimension(3) :: S,US
     integer(kind=kind(1)) :: k_max,k
     real(kind=kind(1.0d0)) :: separation,bondlength,Ta,Tb,g
     real(kind=kind(1.0d0)), dimension(3) :: diff

     k_max = size(k_pts,1)
     select case (trim(self%crystal%thermal_smearing_model))
       case (" ","none")
         res = 1.0d0
       case ("coppens")  ! tf=0.50d0[exp(-(1/2)S(Ua)S)+exp(-(1/2)S(Ub)S)]
         Ua = -0.50d0 * self%atom(a)%thermal_tensor
         Ub = -0.50d0 * self%atom(b)%thermal_tensor
         do k = 1,k_max
            S = k_pts(k,:)
            US = matmul(Ua,S)
            Ta = dot_product(S,US)
            US = matmul(Ub,S)
            Tb = dot_product(S,US)
            res(k,:,:) = 0.50d0*(exp(Ta)+exp(Tb))
         end do
       case ("stewart")  ! tf=exp[-0.50d0 Sg(Ua+Ub)S], g=1/2,1/4]
         bondlength=2.5
         Ua = self%atom(a)%thermal_tensor
         Ub = self%atom(b)%thermal_tensor
         Uab = Ua + Ub
         diff = self%atom(a)%pos-self%atom(b)%pos
         separation = norm_(diff)
         g=0.50d0
         if (separation > bondlength) g=0.25d0
         Uab = -0.50d0 * g * Uab      ! -0.50d0 g (Ua+Ub)
         do k = 1,k_max
            S = k_pts(k,:)
            US = matmul(Uab,S)
            Ta = dot_product(S,US)
            res(k,:,:) = exp(Ta)
         end do
       case ("tanaka")  ! tf=exp[-0.50d0 S (Ua*alpha + Ub*beta)/gamma S]
         Ua = self%atom(a)%thermal_tensor
         Ub = self%atom(b)%thermal_tensor
         call make_tanaka_thermal_smearing_(sh,res,k_pts,Ua,Ub)
       case default
         call die_(tonto,"MOL:thermal_smearing_correction ... unknown model, "//trim(self%crystal%thermal_smearing_model))
     end select

   end subroutine

   subroutine ft_thermally_smear(self,ft_ab,k_pts,a,b)
    type(mol_type) :: self
    ! Thermally smear the fourier transform integrals "ft_ab" evaluated
    ! on "k_pts" for shell pair (ab) centered on atoms "a" and "b" .
     integer(kind=kind(1)) :: a,b
     real(kind=kind(1.0d0)), dimension(:,:) :: k_pts
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:) :: ft_ab
     real(kind=kind(1.0d0)), dimension(3,3) :: Ua,Ub,Uab
     real(kind=kind(1.0d0)), dimension(3) :: S,US,diff
     real(kind=kind(1.0d0)) :: separation,bondlength,Ta,Tb,g
     integer(kind=kind(1)) :: k,k_max

     k_max = size(k_pts,1)
     select case (trim(self%crystal%thermal_smearing_model))
       case (" ","none")
       case ("coppens")  ! tf=0.50d0[exp(-(1/2)S(Ua)S)+exp(-(1/2)S(Ub)S)]
         Ua = -0.50d0 * self%atom(a)%thermal_tensor
         Ub = -0.50d0 * self%atom(b)%thermal_tensor
         do k = 1,k_max
            S = k_pts(k,:)
            US = matmul(Ua,S)
            Ta = dot_product(S,US)
            US = matmul(Ub,S)
            Tb = dot_product(S,US)
            ft_ab(k,:,:) = ft_ab(k,:,:) * 0.50d0*(exp(Ta)+exp(Tb))
         end do
       case ("stewart")  ! tf=exp[-0.50d0 Sg(Ua+Ub)S], g=1/2,1/4
         bondlength=2.5
         Ua = self%atom(a)%thermal_tensor
         Ub = self%atom(b)%thermal_tensor
         Uab = Ua + Ub
         diff = self%atom(a)%pos-self%atom(b)%pos
         separation = norm_(diff)
         g=0.50d0
         if (separation > bondlength) g=0.25d0
         Uab = -0.50d0 * g * Uab      ! -0.50d0 g (Ua+Ub)
         do k = 1,k_max
            S = k_pts(k,:)
            US = matmul(Uab,S)
            Ta = dot_product(S,US)
            ft_ab(k,:,:) = exp(Ta)*ft_ab(k,:,:)
         end do
       case ("tanaka")  ! tf=exp[-0.50d0 S (Ua*alpha + Ub*beta)/gamma S]
         call die_(tonto,"MOL:ft_thermally_smear ... cannot thermally smear at contracted level.")
       case default
         call die_(tonto,"MOL:ft_thermally_smear ... unknown thermal smearing model, "//trim(self%crystal%thermal_smearing_mo&
&del))
     end select

   end subroutine

   subroutine add_dispersion_correction(self,ft,k_pts)
    type(mol_type) :: self
    ! Adds the dispersion correction to "ft".
     real(kind=kind(1.0d0)), dimension(:,:) :: k_pts
     complex(kind=kind((1.0d0,1.0d0))), dimension(:) :: ft
     complex(kind=kind((1.0d0,1.0d0))) :: disp,phase
     real(kind=kind(1.0d0)), dimension(3) :: pos
     real(kind=kind(1.0d0)), dimension(3,3) :: U,seitz
     real(kind=kind(1.0d0)), dimension(3) :: Bh,UBh
     complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: phases
     real(kind=kind(1.0d0)) :: x,y,z,T
     integer(kind=kind(1)) :: a,symop,n,k,k_max

     call ensure_(tonto,associated(self%crystal),"MOL:add_dispersion_correction ... no crystal")

     if (self%crystal%correct_dispersion) then
       select case (trim(self%crystal%thermal_smearing_model))
         case (" ")
           call create_(phases,size(k_pts))
           do n=1,self%crystal%n_fragment_cell_atoms
             a = self%crystal%atom_for_fragment_cell_atom(n)
             disp = dispersion_correction_(self%atom(a),self%crystal%wavelength)
             pos = self%crystal%unit_cell_geometry(:,n)
             x = pos(1); y = pos(2); z = pos(3)
             ft(:) = ft(:) + disp * exp(cmplx(0.0d0,k_pts(:,1)*x + k_pts(:,2)*y + k_pts(:,3)*z,kind=kind((1.0d0,1.0d0))))
           end do
           call destroy_(phases)

         case default  ! thermal smearing methods collapse to be the same
                       ! when dealing with one atom.

           k_max = size(k_pts,1)
           do n=1,self%crystal%n_fragment_cell_atoms
             a = self%crystal%atom_for_fragment_cell_atom(n)
             symop = self%crystal%symop_for_unit_cell_atom(n)
             seitz = self%crystal%spacegroup%seitz(1:3,1:3,symop)

             U = self%atom(a)%thermal_tensor
              ! to crystal coordinates
             call change_basis_(U,self%crystal%unitcell%reciprocal_U_matrix)
              ! apply seitz operation
             call change_basis_(U,transpose(seitz))
              ! to cartesian coordinates
             call change_basis_(U,self%crystal%unitcell%direct_U_matrix)

             disp = dispersion_correction_(self%atom(a),self%crystal%wavelength)
             U = -0.50d0 * U
             pos = self%crystal%unit_cell_geometry(:,n)
             x = pos(1); y = pos(2); z = pos(3)

             do k = 1,k_max
               Bh = k_pts(k,:)
               UBh = matmul(U,Bh)
               T = (dot_product(Bh,UBh))
               phase = exp(cmplx(T,Bh(1)*x + Bh(2)*y + Bh(3)*z,kind=kind((1.0d0,1.0d0))))
               ft(k) = ft(k) + disp * phase
             end do
           end do
       end select
     end if

   end subroutine

   subroutine get_ft_ints(self)
    type(mol_type) :: self
    ! Get the fourier transform of the overlap integrals.
    ! If the archive file exists, read it; otherwise make it.
      type(archive_type) :: arch

      call set_(arch,self%name,"ft_ints")
      if (.not. exists_(arch)) call make_ft_ints_(self)

   end subroutine

   subroutine make_ft_ints(self)
    type(mol_type) :: self
    ! Make the fourier transform of the overlap integrals on an archive file
      type(archive_type) :: arch
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: k_pts
      integer(kind=kind(1)) :: q,fa,la,fb,lb,atom_a,atom_b
      type(shell2_type), pointer :: sh
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:), pointer :: ft_ab_eq,ft_ab

   call ensure_(tonto,associated(self%crystal),"MOL:make_ft_ints ... no crystal")
      call set_(arch,self%name,"ft_ints")
      call open_(arch,for="write-only",buffered=.true.,type="cpx")
      call create_(k_pts,n_unique_SF_k_pts_(self%crystal),3)
      call make_unique_SF_k_pts_(self%crystal,k_pts)
      call create_(sh)
      do q = 1,self%n_shell_pairs
         call get_shell_pair_(self,sh,q,fa,la,fb,lb,atom_a,atom_b)
         call create_(ft_ab_eq,size(k_pts,1),sh%a%n_comp,sh%b%n_comp)
         call create_(ft_ab,n_refl_(self%crystal),sh%a%n_comp,sh%b%n_comp)
         call make_ft_pair_(self,ft_ab_eq,k_pts,sh,atom_a,atom_b)
         call sum_ft_ints_(self%crystal,ft_ab,ft_ab_eq)
         call write_(arch%file,ft_ab)
         call destroy_(ft_ab)
         call destroy_(ft_ab_eq)
         call destroy_ptr_part_(sh)
      end do
      call destroy_(sh)
      call destroy_(k_pts)
      call close_(arch)

   end subroutine

   subroutine make_structure_factors(self)
    type(mol_type) :: self
    ! Make the structure factors for this molecule.
      complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: sf_eq,Fc
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: k_pts
      character(128) :: orb_kind
      logical(kind=kind(.true.)) :: complex

      call ensure_(tonto,associated(self%crystal),"MOL:make_structure_factors ... no crystal")
      call ensure_(tonto,reflection_data_exists_(self%crystal),"MOL:make_structure_factors ... no reflection data")
      call ensure_(tonto,associated(self%density_matrix),"MOL:make_structure_factors ... no density matrix")
      call create_(Fc,n_refl_(self%crystal))
      call create_(k_pts,n_unique_SF_k_pts_(self%crystal),3)
      call make_unique_SF_k_pts_(self%crystal,k_pts)
      call create_(sf_eq,size(k_pts,1))
      call make_ao_density_matrix_(self)
      orb_kind = spinorbital_kind_(self%density_matrix)
      complex = includes_(orb_kind,"complex")
      if (complex) then
        call make_ft_(self,sf_eq,self%density_matrix%restricted_complex,k_pts)
      else
        call make_ft_(self,sf_eq,self%density_matrix%restricted,k_pts)
      end if
      call destroy_(k_pts)
      call sum_unique_sf_(self%crystal,Fc,sf_eq)
      if (self%crystal%correct_dispersion) then
        call create_(k_pts,n_refl_(self%crystal),3)
        call make_k_pts_(self%crystal,k_pts)
        call add_dispersion_correction_(self,Fc,k_pts)
        call destroy_(k_pts)
      end if
      call destroy_(sf_eq)
      call set_F_calc_(self%crystal,Fc)
      call destroy_(Fc)
      call make_F_predicted_(self%crystal)

   end subroutine

   subroutine make_sz_structure_factors(self)
    type(mol_type) :: self
    ! Make the structure factors for this molecule.
      complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: sf_eq,Fc
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: k_pts
      type(archive_type) :: arch
      character(128) :: orb_kind
      logical(kind=kind(.true.)) :: complex

      call ensure_(tonto,associated(self%crystal),"MOL:make_sz_structure_factors ... no crystal")
      call ensure_(tonto,reflection_data_exists_(self%crystal),"MOL:make_sz_structure_factors ... no reflection data")
      call create_(Fc,n_refl_(self%crystal))
      call create_(k_pts,n_unique_SF_k_pts_(self%crystal),3)
      call make_unique_SF_k_pts_(self%crystal,k_pts)
      call create_(sf_eq,size(k_pts,1))
      call make_ao_sz_density_matrix_(self)  ! make S_z density matrix
      orb_kind = spinorbital_kind_(self%density_matrix)
      complex = includes_(orb_kind,"complex")
      if (complex) then; call make_ft_(self,sf_eq,self%density_matrix%restricted_complex,k_pts)
      else;              call make_ft_(self,sf_eq,self%density_matrix%restricted,k_pts)
      end if
      call sum_unique_sf_(self%crystal,Fc,sf_eq)
      call set_F_calc_(self%crystal,Fc)
       ! Now destroy the S_z density matrix
      if (complex) then; call destroy_(self%density_matrix,"restricted_complex")
      else;              call destroy_(self%density_matrix,"restricted")
      end if
      call destroy_(sf_eq)
      call destroy_(k_pts)
      call make_F_predicted_(self%crystal)
      call set_(arch,self%name,"sz_structure_factors")
      call write_(arch,Fc)
      call destroy_(Fc)

   end subroutine

   subroutine make_PND_scalar_magnetic_sf(self)
    type(mol_type) :: self
    ! Make the PND scalar magnetic structure factors and assign them
    ! To the crystal objects structure factors for analysis
      complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: FM_s, FM_l, FM_r, Fc
      integer(kind=kind(1)) :: n_refl
      type(archive_type) :: arch

   call ensure_(tonto,associated(self%crystal),"MOL:make_PND_scalar_magnetic_sf ... no crystal")
   call ensure_(tonto,reflection_data_exists_(self%crystal),"MOL:make_PND_scalar_magnetic_sf ... no reflection data")
      n_refl = n_refl_(self%crystal)
      call create_(Fc,n_refl)
       ! Spin contribution
      call make_PND_spin_sf_(self)
      call create_(FM_s,n_refl)
      call set_(arch,self%name,"PND_spin_sf")
      call read_(arch,FM_s)
       ! Orbital contribution
      call make_PND_nabla_sf_(self)
      call create_(FM_l,n_refl)
      call set_(arch,self%name,"PND_nabla_sf")
      call read_(arch,FM_l)
       ! Diamagnetic contribution
     ! .make_PND_r_sf
     ! FM_r.create(n_refl)
     ! arch.set(.name,"PND_r_sf")
     ! arch.read(FM_r)
       ! Make F_calc
      Fc = FM_s + FM_l  ! + FM_r
      call set_(arch,self%name,"PND_scalar_magnetic_sf")
      call write_(arch,Fc)
      call set_F_calc_(self%crystal,Fc)
      call make_F_predicted_(self%crystal)
      call destroy_(FM_r)
      call destroy_(FM_l)
      call destroy_(FM_s)
      call destroy_(Fc)

   end subroutine

   subroutine make_PND_spin_sf(self)
    type(mol_type) :: self
    ! Make the PND structure factors
      character(128) :: orb_kind

   call ensure_(tonto,associated(self%density_matrix),"MOL:make_PND_spin_sf ... no density")
      orb_kind = spinorbital_kind_(self%density_matrix)
      select case (orb_kind)
         case ("unrestricted");    call make_u_PND_spin_sf_(self)
         case ("general_complex"); call make_gc_PND_spin_sf_(self)
         case default
            call die_(tonto,"MOL:make_PND_spin_sf ... not implemented for "//trim(orb_kind))
      end select

   end subroutine

   subroutine make_u_PND_spin_sf(self)
    type(mol_type) :: self
    ! Make the scalar spin magnetic structure factors assuming an applied field
    ! in the z direction, and assuming an unrestricted density matrix exists.
      type(archive_type) :: arch
      real(kind=kind(1.0d0)) :: fac,ans
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: d_aa,d_bb
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:,:), pointer :: ft_ab
      complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: FM
      type(shell2_type), pointer :: sh
      integer(kind=kind(1)) :: k_max,i,q,fa,la,fb,lb

   call ensure_(tonto,associated(self%crystal),"MOL:make_u_PND_spin_sf ... no crystal")
   call ensure_(tonto,associated(self%density_matrix),"MOL:make_u_PND_spin_sf ... no density")
   call ensure_(tonto,associated(self%density_matrix%alpha),"MOL:make_u_PND_spin_sf ... no density")
      call set_(arch,self%name,"PND_ft_spin_ints")
      if (.not. exists_(arch)) call make_PND_ft_spin_ints_(self)
      call open_(arch,for="read-only",type="real")
      k_max = n_refl_(self%crystal)
      call create_(FM,k_max)
      FM = 0.0d0
      call create_(sh)
      do q = 1,self%n_shell_pairs
         call get_shell_pair_(self,sh,q,fa,la,fb,lb)
         call create_(ft_ab,k_max,sh%a%n_comp,sh%b%n_comp,3)
         call read_(arch%file,ft_ab)
         d_aa => self%density_matrix%alpha(fb:lb,fa:la)
         d_bb => self%density_matrix%beta( fb:lb,fa:la)
         fac = 2.0d0
         if (fa==fb) fac=1.0d0
         do i = 1,k_max
            ans = ans +    trace_of_product_(ft_ab(i,:,:,3),d_aa)
            ans = ans -    trace_of_product_(ft_ab(i,:,:,3),d_bb)
            FM(i) = FM(i) + fac*ans
         end do
         call destroy_(ft_ab)
         call destroy_ptr_part_(sh)
      end do
      call destroy_(sh)
      call close_(arch)
      call set_(arch,self%name,"PND_spin_sf")
      call write_(arch,FM)
      call destroy_(FM)

   end subroutine

   subroutine make_gc_PND_spin_sf(self)
    type(mol_type) :: self
    ! Make the scalar spin magnetic structure factors assuming an applied field
    ! in the z direction, and assuming a general complex denisty matrix exists
      type(archive_type) :: arch
      real(kind=kind(1.0d0)) :: fac,ans
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: d_aa,d_bb,d_ba
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:,:), pointer :: ft_ab
      complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: FM
      complex(kind=kind((1.0d0,1.0d0))) :: ci
      type(shell2_type), pointer :: sh
      integer(kind=kind(1)) :: k_max,i,q,fa,la,fb,lb

   call ensure_(tonto,associated(self%crystal),"MOL:make_gc_PND_spin_sf ... no crystal")
   call ensure_(tonto,associated(self%density_matrix),"MOL:make_gc_PND_spin_sf ... no density")
   call ensure_(tonto,associated(self%density_matrix%general_complex),"MOL:make_gc_PND_spin_sf ... no density")
      call set_(arch,self%name,"PND_ft_spin_ints")
      if (.not. exists_(arch)) call make_PND_ft_spin_ints_(self)
      call open_(arch,for="read-only",type="real")
      ci = (0.0d0,1.0d0)
      k_max = n_refl_(self%crystal)
      call create_(FM,k_max)
      FM = 0.0d0
      call create_(sh)
      do q = 1,self%n_shell_pairs
         call get_shell_pair_(self,sh,q,fa,la,fb,lb)
         call create_(ft_ab,k_max,sh%a%n_comp,sh%b%n_comp,3)
         call read_(arch%file,ft_ab)
         d_aa => self%density_matrix%general_complex(      fb:      lb,      fa:      la)
         d_bb => self%density_matrix%general_complex(self%n_bf+fb:self%n_bf+lb,self%n_bf+fa:self%n_bf+la)
         d_ba => self%density_matrix%general_complex(self%n_bf+fb:self%n_bf+lb,      fa:      la)
         fac = 2.0d0
         if (fa==fb) fac=1.0d0
         do i = 1,k_max
            ans =          trace_of_product_(ft_ab(i,:,:,1),d_ba)
            ans = ans - ci*trace_of_product_(ft_ab(i,:,:,2),d_ba)
            ans = ans +    trace_of_product_(ft_ab(i,:,:,3),d_aa)
            ans = ans -    trace_of_product_(ft_ab(i,:,:,3),d_bb)
            FM(i) = FM(i) + fac*ans
         end do
         call destroy_(ft_ab)
         call destroy_ptr_part_(sh)
      end do
      call destroy_(sh)
      call close_(arch)
      call set_(arch,self%name,"PND_spin_sf")
      call write_(arch,FM)
      call destroy_(FM)

   end subroutine

   subroutine get_PND_ft_spin_ints(self)
    type(mol_type) :: self
    ! Get the PND ft spin integrals on disk if they don't already exist
      type(archive_type) :: arch

      call set_(arch,self%name,"PND_ft_spin_ints")
      if (.not. exists_(arch)) call make_PND_ft_spin_ints_(self)

   end subroutine

   subroutine make_PND_ft_spin_ints(self)
    type(mol_type) :: self
    ! Make the fourier transform of the PND spin integrals on the archive
      type(archive_type) :: arch
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: k_pts
      type(shell2_type), pointer :: sh
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:), pointer :: ft_ab_eq
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:,:), pointer :: ft_ab
      integer(kind=kind(1)) :: q,fa,la,fb,lb,atom_a,atom_b

   call ensure_(tonto,associated(self%crystal),"MOL:make_PND_ft_spin_ints ... no crystal")
      call set_(arch,self%name,"PND_ft_spin_ints")
      call open_(arch,for="write-only",type="cpx")
      call create_(k_pts,n_unique_SF_k_pts_(self%crystal),3)
      call make_unique_SF_k_pts_(self%crystal,k_pts)
      call create_(sh)
      do q = 1,self%n_shell_pairs
         call get_shell_pair_(self,sh,q,fa,la,fb,lb,atom_a,atom_b)
         call create_(ft_ab_eq,size(k_pts,1),sh%a%n_comp,sh%b%n_comp)
         call make_ft_pair_(self,ft_ab_eq,k_pts,sh,atom_a,atom_b)
         call create_(ft_ab,n_refl_(self%crystal),sh%a%n_comp,sh%b%n_comp,3)
         call sum_PND_spin_ints_(self%crystal,ft_ab,ft_ab_eq)
         call write_(arch%file,ft_ab)
         call destroy_(ft_ab)
         call destroy_(ft_ab_eq)
         call destroy_ptr_part_(sh)
      end do
      call destroy_(sh)
      call destroy_(k_pts)
      call close_(arch)

   end subroutine

   subroutine make_PND_nabla_sf(self)
    type(mol_type) :: self
    ! Make PND nabla structure factors
      character(128) :: orb_kind

   call ensure_(tonto,associated(self%density_matrix),"MOL:make_PND_nabla_sf ... no density")
      orb_kind = spinorbital_kind_(self%density_matrix)
      select case (orb_kind)
         case ("general_complex"); call make_gc_PND_nabla_sf_(self)
         case default
            call die_(tonto,"MOL:make_PND_nabla_sf ... not implemented for "//trim(orb_kind))
      end select

   end subroutine

   subroutine make_gc_PND_nabla_sf(self)
    type(mol_type) :: self
    ! Make the scalar nabla magnetic structure factors assuming an applied field
    ! in the z direction, and assuming a general complex density exists
      type(archive_type) :: arch
      real(kind=kind(1.0d0)) :: fac
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: dens
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:), pointer :: ft_ab_z
      complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: FM
      type(shell2_type), pointer :: sh
      integer(kind=kind(1)) :: k_max,i,q,fa,la,fb,lb

   call ensure_(tonto,associated(self%crystal),"MOL:make_gc_PND_nabla_sf ... no crystal")
   call ensure_(tonto,associated(self%density_matrix),"MOL:make_gc_PND_nabla_sf ... no density")
   call ensure_(tonto,associated(self%density_matrix%general_complex),"MOL:make_gc_PND_nabla_sf ... no density")
      call set_(arch,self%name,"PND_ft_nabla_ints")
      if (.not. exists_(arch)) call make_PND_ft_nabla_ints_(self)
      call open_(arch,for="read-only",type="real")
      k_max = n_refl_(self%crystal)
      call create_(FM,k_max)
      FM = 0.0d0
      call create_(sh)
      do q = 1,self%n_shell_pairs
         call get_shell_pair_(self,sh,q,fa,la,fb,lb)
         call create_(ft_ab_z,k_max,sh%a%n_comp,sh%b%n_comp)
         call read_(arch%file,ft_ab_z)
         call create_(dens,sh%b%n_comp,sh%a%n_comp)
         dens =        self%density_matrix%general_complex(      fb:      lb,      fa:      la)
         dens = dens + self%density_matrix%general_complex(self%n_bf+fb:self%n_bf+lb,self%n_bf+fa:self%n_bf+la)
         fac = 2.0d0
         if (fa==fb) fac=1.0d0
         do i = 1,k_max
            FM(i) = FM(i) + fac*trace_of_product_(ft_ab_z(i,:,:),dens)
         end do
         call destroy_(dens)
         call destroy_(ft_ab_z)
         call destroy_ptr_part_(sh)
      end do
      call destroy_(sh)
      call close_(arch)
      call set_(arch,self%name,"PND_nabla_sf")
      call write_(arch,FM)
      call destroy_(FM)

   end subroutine

   subroutine get_PND_ft_nabla_ints(self)
    type(mol_type) :: self
    ! Get the PND ft nabla integrals on disk if they don't already exist
      type(archive_type) :: arch

      call set_(arch,self%name,"PND_ft_nabla_ints")
      if (.not. exists_(arch)) call make_PND_ft_nabla_ints_(self)

   end subroutine

   subroutine make_PND_ft_nabla_ints(self)
    type(mol_type) :: self
    ! Make the fourier transform of the nabla integrals on the archive
      type(archive_type) :: arch
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: k_pts
      type(shell2_type), pointer :: sh
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:,:), pointer :: ft_ab_eq
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:), pointer :: ft_ab_z
      integer(kind=kind(1)) :: q,fa,la,fb,lb,atom_a,atom_b

   call ensure_(tonto,associated(self%crystal),"MOL:make_PND_ft_nabla_ints ... no crystal")
      call set_(arch,self%name,"PND_ft_nabla_ints")
      call open_(arch,for="write-only",type="cpx")
      call create_(k_pts,n_unique_SF_k_pts_(self%crystal),3)
      call make_unique_SF_k_pts_(self%crystal,k_pts)
      call create_(sh)
      do q = 1,self%n_shell_pairs
         call get_shell_pair_(self,sh,q,fa,la,fb,lb,atom_a,atom_b)
         call create_(ft_ab_eq,size(k_pts,1),sh%a%n_comp,sh%b%n_comp,3)
         call make_ft_nabla_pair_(self,ft_ab_eq,k_pts,sh,atom_a,atom_b)
        ! sh.make_ft_nabla(ft_ab_eq,k_pts)
        ! .ft_thermally_smear(ft_ab_eq(:,:,:,1),k_pts,atom_a,atom_b)
        ! .ft_thermally_smear(ft_ab_eq(:,:,:,2),k_pts,atom_a,atom_b)
        ! .ft_thermally_smear(ft_ab_eq(:,:,:,3),k_pts,atom_a,atom_b)
         call create_(ft_ab_z,n_refl_(self%crystal),sh%a%n_comp,sh%b%n_comp)
         call sum_PND_nabla_ints_(self%crystal,ft_ab_z,ft_ab_eq)
         call write_(arch%file,ft_ab_z)
         call destroy_(ft_ab_z)
         call destroy_(ft_ab_eq)
         call destroy_ptr_part_(sh)
      end do
      call destroy_(sh)
      call destroy_(k_pts)
      call close_(arch)

   end subroutine

   subroutine make_ft_nabla_pair(self,ft_ab,k_pts,sh,atom_a,atom_b)
    type(mol_type) :: self
    ! Make_ft for one pair of gaussians.
    ! Includes partitioning, dispersion and thermal smearing.
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:,:) :: ft_ab
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: k_pts
      type(shell2_type), intent(in) :: sh
      integer(kind=kind(1)), intent(in) :: atom_a,atom_b
       real(kind=kind(1.0d0)), dimension(:,:,:), pointer :: t
       real(kind=kind(1.0d0)), dimension(:,:), pointer :: p
      integer(kind=kind(1)) :: k_max

   call ensure_(tonto,associated(self%crystal),"MOL:make_ft_nabla_pair ... no crystal data")
      k_max = size(k_pts,1)
      call create_(t,k_max,sh%a%n_cc,sh%b%n_cc)
      call thermal_smearing_correction_(self,t,k_pts,atom_a,atom_b,sh)
      call create_(p,sh%a%n_cc,sh%b%n_cc)
      call make_partition_factors_(self,p,sh,atom_a,atom_b)
      call make_ft_nabla_(sh,ft_ab,k_pts,t,p)
      call destroy_(p)
      call destroy_(t)

   end subroutine

   subroutine make_PND_r_sf(self)
    type(mol_type) :: self
    ! Make the scalar dipole magnetic structure factors assuming an applied field
    ! in the z direction
      type(archive_type) :: arch
      real(kind=kind(1.0d0)) :: fac,ans
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: d_aa,d_bb
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:,:), pointer :: ft_ab
      complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: FM
      type(shell2_type), pointer :: sh
      integer(kind=kind(1)) :: k_max,i,q,fa,la,fb,lb

   call ensure_(tonto,associated(self%crystal),"MOL:make_PND_r_sf ... no crystal")
      call set_(arch,self%name,"PND_ft_r_ints")
      if (.not. exists_(arch)) call make_PND_ft_r_ints_(self)
      call open_(arch,for="read-only",type="real")
      k_max = n_refl_(self%crystal)
      call create_(FM,k_max)
      FM = 0.0d0
      call create_(sh)
      do q = 1,self%n_shell_pairs
         call get_shell_pair_(self,sh,q,fa,la,fb,lb)
         call create_(ft_ab,k_max,sh%a%n_comp,sh%b%n_comp,3)
         call read_(arch%file,ft_ab)
         d_aa => self%density_matrix%general_complex(      fb:      lb,      fa:      la)
         d_bb => self%density_matrix%general_complex(self%n_bf+fb:self%n_bf+lb,self%n_bf+fa:self%n_bf+la)
         fac = 2.0d0
         if (fa==fb) fac=1.0d0
         do i = 1,k_max
            ans =       fac*trace_of_product_(ft_ab(i,:,:,3),d_aa)
            ans = ans + fac*trace_of_product_(ft_ab(i,:,:,3),d_bb)
            FM(i) = FM(i) + ans
         end do
         call destroy_(ft_ab)
         call destroy_ptr_part_(sh)
      end do
      call destroy_(sh)
      call close_(arch)
      call set_(arch,self%name,"PND_r_sf")
      call write_(arch,FM)
      call destroy_(FM)

   end subroutine

   subroutine get_PND_ft_r_ints(self)
    type(mol_type) :: self
    ! Get the PND ft r integrals on disk if they don't already exist
      type(archive_type) :: arch

      call set_(arch,self%name,"PND_ft_r_ints")
      if (.not. exists_(arch)) call make_PND_ft_r_ints_(self)

   end subroutine

   subroutine make_PND_ft_r_ints(self)
    type(mol_type) :: self
    ! Make the fourier transform of the dipole "r" integrals on the archive
      type(archive_type) :: arch
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: k_pts
      type(shell2_type), pointer :: sh
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:,:), pointer :: ft_ab_eq, ft_ab
      integer(kind=kind(1)) :: q,fa,la,fb,lb,atom_a,atom_b,n_refl

   call ensure_(tonto,associated(self%crystal),"MOL:make_PND_ft_r_ints ... no crystal")
   call ensure_(tonto,reflection_data_exists_(self%crystal),"MOL:make_PND_ft_r_ints ... no structure factor data")
      n_refl = n_refl_(self%crystal)
      call set_(arch,self%name,"PND_ft_r_ints")
      call open_(arch,for="write-only",type="cpx")
      call create_(k_pts,n_unique_SF_k_pts_(self%crystal),3)
      call make_unique_SF_k_pts_(self%crystal,k_pts)
      call create_(sh)
      do q = 1,self%n_shell_pairs
         call get_shell_pair_(self,sh,q,fa,la,fb,lb,atom_a,atom_b)
         call create_(ft_ab_eq,size(k_pts,1),sh%a%n_comp,sh%b%n_comp,3)
         call make_ft_r_(sh,ft_ab_eq,k_pts)
         call ft_thermally_smear_(self,ft_ab_eq(:,:,:,1),k_pts,atom_a,atom_b)
         call ft_thermally_smear_(self,ft_ab_eq(:,:,:,2),k_pts,atom_a,atom_b)
         call ft_thermally_smear_(self,ft_ab_eq(:,:,:,3),k_pts,atom_a,atom_b)
         call create_(ft_ab,n_refl,sh%a%n_comp,sh%b%n_comp,3)
         call sum_ft_r_ints_(self%crystal,ft_ab,ft_ab_eq,self%B_field)
         call write_(arch%file,ft_ab)
         call destroy_(ft_ab)
         call destroy_(ft_ab_eq)
         call destroy_ptr_part_(sh)
      end do
      call destroy_(sh)
      call destroy_(k_pts)
      call close_(arch)

   end subroutine

   subroutine make_PND_fit_ints(self,ints,lambda)
    type(mol_type) :: self
    ! Make the PND constraint integrals on the archive
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), target :: ints
      real(kind=kind(1.0d0)) :: lambda
      type(archive_type) :: spin_archive,nabla_archive
      real(kind=kind(1.0d0)), dimension(:), pointer :: Fc,F_exp,F_sigma
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: ints_aa,ints_bb,ints_ba
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:), pointer :: nabla_z
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:,:), pointer :: spin
      type(shell2_type), pointer :: sh
      complex(kind=kind((1.0d0,1.0d0))) :: ci
      real(kind=kind(1.0d0)) :: fac,fac1
      integer(kind=kind(1)) :: i,q,fa,la,fb,lb,n_bf,n_refl

      call get_PND_ft_spin_ints_(self)
      call get_PND_ft_nabla_ints_(self)
      call get_PND_ft_r_ints_(self)
      call set_(spin_archive,self%name,"PND_ft_spin_ints")
      call set_(nabla_archive,self%name,"PND_ft_nabla_ints")
     ! r_archive.set(.name,"PND_ft_r_ints")
      call open_(spin_archive,for="read-only",type="real")
      call open_(nabla_archive,for="read-only",type="real")
     ! r_archive.open(for="read-only",type="real")
      ci = (0.0d0,1.0d0)
      n_bf   =  self%n_bf
      n_refl =  n_refl_(self%crystal)
      call create_(Fc,n_refl)
      call create_(F_exp,n_refl)
      call create_(F_sigma,n_refl)
      Fc = abs(F_calc_(self%crystal))
      F_exp  = F_exp_(self%crystal)
      F_sigma  = F_sigma_(self%crystal)
      ints = 0.0d0
      call create_(sh)
      fac1 = 2.0d0*lambda/n_refl
      do q = 1,self%n_shell_pairs
         call get_shell_pair_(self,sh,q,fa,la,fb,lb)
         call create_(spin,n_refl,sh%a%n_comp,sh%b%n_comp,3)
         call read_(spin_archive%file,spin)
         ints_aa => ints(     fa:     la,     fb:     lb)
         ints_bb => ints(n_bf+fa:n_bf+la,n_bf+fb:n_bf+lb)
         ints_ba => ints(n_bf+fa:n_bf+la,     fb:     lb)
         do i = 1,n_refl
            fac = fac1*(Fc(i) - F_exp(i))/(F_sigma(i)*F_sigma(i))
            ints_ba = ints_ba +    fac*spin(i,:,:,1)
            ints_ba = ints_ba + ci*fac*spin(i,:,:,2)
            ints_aa = ints_aa +    fac*spin(i,:,:,3)
            ints_bb = ints_bb -    fac*spin(i,:,:,3)
         end do
         call destroy_(spin)
         call create_(nabla_z,n_refl,sh%a%n_comp,sh%b%n_comp)
         call read_(nabla_archive%file,nabla_z)
         do i = 1,n_refl
            fac = fac1*(Fc(i) - F_exp(i))/(F_sigma(i)*F_sigma(i))
            ints_aa = ints_aa + fac*nabla_z(i,:,:)
            ints_bb = ints_bb + fac*nabla_z(i,:,:)
         end do
         call destroy_(nabla_z)
        ! r.create(n_refl,sh.a.n_comp,sh.b.n_comp,3)
        ! r_archive.file.read(r)
        ! do i = 1,n_refl
        !    fac = fac1*(Fc(i) - F_exp(i))/(F_sigma(i)*F_sigma(i)*)
        !    ints_aa = ints_aa + fac*r(i,:,:,3)
        !    ints_bb = ints_bb + fac*r(i,:,:,3)
        ! end
        ! r.destroy
         call destroy_ptr_part_(sh)
      end do
      call destroy_(sh)
      call make_hermitian_(ints)
     ! r_archive.close
      call close_(nabla_archive)
      call close_(spin_archive)
      call destroy_(F_sigma)
      call destroy_(F_exp)
      call destroy_(Fc)

   end subroutine

!  ****************
!  Symmetry methods
!  ****************

   subroutine symmetrise(self,mat,orb_kind)
    type(mol_type) :: self
    ! Symmetrise an opmatrix matrix "mat" of spinorbital_kind "orb_kind" (if
    ! specified)
      type(opmatrix_type) :: mat
      character(128), optional :: orb_kind
      character(128) :: itemkind

      itemkind = spinorbital_kind_(mat)
      if (present(orb_kind)) itemkind = orb_kind
      select case (itemkind)
         case ("restricted");           call symmetrise_r_(self,mat%restricted)
         case ("unrestricted");         call symmetrise_r_(self,mat%alpha)
                                        call symmetrise_r_(self,mat%beta)
         case ("general");              call symmetrise_r_(self,alpha_alpha_(mat%general))
                                        call symmetrise_r_(self,beta_beta_(mat%general))
                                        call symmetrise_r_(self,alpha_beta_(mat%general))
                                        call symmetrise_r_(self,beta_alpha_(mat%general))
         case ("restricted_complex");   call symmetrise_c_(self,mat%restricted_complex)
         case ("unrestricted_complex"); call symmetrise_c_(self,mat%alpha_complex)
                                        call symmetrise_c_(self,mat%beta_complex)
         case ("general_complex");      call symmetrise_c_(self,alpha_alpha_(mat%general_complex))
                                        call symmetrise_c_(self,beta_beta_(mat%general_complex))
                                        call symmetrise_c_(self,alpha_beta_(mat%general_complex))
                                        call symmetrise_c_(self,beta_alpha_(mat%general_complex))
         case default;   call die_(tonto,"MOL:symmetrise ... unknown kind, "//trim(orb_kind))
      end select

   end subroutine

   subroutine symmetrise_r(self,mat)
    type(mol_type) :: self
    ! Symmetrise a real restricted basis kind matrix
      real(kind=kind(1.0d0)), dimension(:,:) :: mat
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: sym,w,tra,trb
      type(shell_type), pointer :: sh
      integer(kind=kind(1)) :: n,a,b,i,j,na,nb
      integer(kind=kind(1)) :: fa,fb,fi,fj,la,lb,li,lj
      integer(kind=kind(1)), dimension(:), pointer :: first,last
      integer(kind=kind(1)), dimension(:,:), pointer :: image

   call ensure_(tonto,self%basis_info_made,"MOL:symmetrise_r ... no basis info")
   call ensure_(tonto,associated(self%atom),"MOL:symmetrise_r ... no atom list")
   call ensure_(tonto,is_square_(mat),"MOL:symmetrise_r ... non-square matrix")
   call ensure_(tonto,size(mat,1)==self%n_bf,"MOL:symmetrise_r ... incorrectly dimensioned matrix")
      first => self%first_basis_fn_for_shell
      last  => self%last_basis_fn_for_shell
      call create_(image,self%n_shell, self%pointgroup%order)
      call make_image_of_shell_(self,image)
      call create_(sym,self%n_bf,self%n_bf)
      sym = 0.0d0
      call create_(sh)
      do n = 1,self%pointgroup%order
         do a = 1,self%n_shell
            call get_shell_(self,sh,a); na = sh%n_comp; la = sh%l; call destroy_ptr_part_(sh)
            tra => xyz_matrix_(self%pointgroup,n,la)
            i = image(a,n)
            fa = first(a);  la = last(a); fi = first(i);  li = last(i)
            do b = 1,self%n_shell
               call get_shell_(self,sh,b); nb = sh%n_comp; lb = sh%l; call destroy_ptr_part_(sh)
               trb => xyz_matrix_(self%pointgroup,n,lb)
               j = image(b,n)
               fb = first(b);  lb = last(b); fj = first(j);  lj = last(j)
               call create_(w,na,nb)
               w = matmul(tra, mat(fa:la,fb:lb) )
               sym(fi:li,fj:lj) = sym(fi:li,fj:lj) + matmul(w,transpose(trb))
               call destroy_(w)
               call destroy_(trb)
            end do
            call destroy_(tra)
         end do
      end do
      call destroy_(sh)
      mat = sym/self%pointgroup%order
      call destroy_(sym)
      call destroy_(image)

   end subroutine

   subroutine symmetrise_c(self,mat)
    type(mol_type) :: self
    ! Symmetrise a complex restricted basis kind matrix
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: mat
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: sym,w
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: tra,trb
      type(shell_type), pointer :: sh
      integer(kind=kind(1)) :: n,a,b,i,j,na,nb
      integer(kind=kind(1)) :: fa,fb,fi,fj,la,lb,li,lj
      integer(kind=kind(1)), dimension(:), pointer :: first,last
      integer(kind=kind(1)), dimension(:,:), pointer :: image

   call ensure_(tonto,self%basis_info_made,"MOL:symmetrise_c ... no basis info")
   call ensure_(tonto,associated(self%atom),"MOL:symmetrise_c ... no atom list")
   call ensure_(tonto,is_square_(mat),"MOL:symmetrise_c ... non-square matrix")
   call ensure_(tonto,size(mat,1)==self%n_bf,"MOL:symmetrise_c ... incorrectly dimensioned matrix")
      first => self%first_basis_fn_for_shell
      last  => self%last_basis_fn_for_shell
      call create_(image,self%n_shell, self%pointgroup%order)
      call make_image_of_shell_(self,image)
      call create_(sym,self%n_bf,self%n_bf)
      sym = 0.0d0
      call create_(sh)
      do n = 1,self%pointgroup%order
         do a = 1,self%n_shell
            call get_shell_(self,sh,a); na = sh%n_comp; la = sh%l; call destroy_ptr_part_(sh)
            tra => xyz_matrix_(self%pointgroup,n,la)
            i = image(a,n)
            fa = first(a);  la = last(a); fi = first(i);  li = last(i)
            do b = 1,self%n_shell
               call get_shell_(self,sh,b); nb = sh%n_comp; lb = sh%l; call destroy_ptr_part_(sh)
               trb => xyz_matrix_(self%pointgroup,n,lb)
               j = image(b,n)
               fb = first(b);  lb = last(b); fj = first(j);  lj = last(j)
               call create_(w,na,nb)
               w = matmul(tra,mat(fa:la,fb:lb))
               sym(fi:li,fj:lj) = sym(fi:li,fj:lj) + matmul(w,transpose(trb))
               call destroy_(w)
               call destroy_(trb)
            end do
            call destroy_(tra)
         end do
      end do
      call destroy_(sh)
      mat = sym/self%pointgroup%order
      call destroy_(sym)
      call destroy_(image)

   end subroutine

   subroutine make_image_of_shell(self,res)
    type(mol_type) :: self
    ! Return an array "res(a,n)" which is the image of shell "a" under pointgroup
    ! operation "n"
    ! Size of "res" is [.n_shell,.pointgroup.order]
      integer(kind=kind(1)), dimension(:,:), intent(out) :: res
      real(kind=kind(1.0d0)), dimension(3) :: new_pos
      integer(kind=kind(1)) :: n,a,aa,as,new_atom,fs

   call ensure_(tonto,size(res,1)==self%n_shell,"MOL:make_image_of_shell ... dimension of matrix incorrect")
   call ensure_(tonto,size(res,2)==self%pointgroup%order,"MOL:make_image_of_shell ... dimension of matrix incorrect")
      do n = 1,self%pointgroup%order
         do a = 1,self%n_shell
            aa = self%atom_for_shell(a)
            as = self%atom_shell_for_shell(a)
            new_pos = matmul(self%pointgroup%mat(:,:,n), self%atom(aa)%pos)
            new_atom = atom_index_from_pos_(self,new_pos)
            fs = self%first_shell_for_atom(new_atom)
            res(a,n) = fs+as-1
         end do
      end do

   end subroutine

!  ************
!  I/O routines
!  ************

   subroutine read_name(self)
    type(mol_type) :: self
    ! Read in the name of the molecule. This must always be
    ! the first keyword inputted.

      call read_(stdin,self%name)

   end subroutine

   subroutine read_multiplicity(self)
    type(mol_type) :: self
    ! Read in the spin multiplicity

      call read_(stdin,self%mult)
   call ensure_(tonto,self%mult/=0,"MOL:read_multiplicity ... cannot have zero multiplicity")

   end subroutine

   subroutine read_charge(self)
    type(mol_type) :: self
    ! Read in the total molecular charge

      call read_(stdin,self%charge)

   end subroutine

   subroutine read_E_field(self)
    type(mol_type) :: self
    ! Read in the applied electric 2.718281828459045d0 field

      call read_(stdin,self%E_field)

   end subroutine

   subroutine read_gauge_origin(self)
    type(mol_type) :: self
    ! Read in the gauge origin vector for an applied external magnetic field

      call read_(stdin,self%gauge_origin)
      call delete_gauge_integrals_(self)

   end subroutine

   subroutine read_B_field(self)
    type(mol_type) :: self
    ! Read in the applied magnetic B field

      call read_(stdin,self%B_field)

   end subroutine

   subroutine read_optimise_thermals(self)
    type(mol_type) :: self
    ! Read in whether to optimise thermal tensor parameters

      call read_(stdin,self%optimise_thermals)

   end subroutine

   subroutine delete_gauge_integrals(self)
    type(mol_type) :: self
    ! Delete all integral files which depend on the gauge origin.
    ! This is required whenever the gauge origin is changed.
      type(archive_type) :: arch

      call set_(arch,self%name,"Lx_matrix"); call delete_(arch)
      call set_(arch,self%name,"Ly_matrix"); call delete_(arch)
      call set_(arch,self%name,"Lz_matrix"); call delete_(arch)

   end subroutine

   subroutine read_plotgrid(self)
    type(mol_type) :: self
    ! Read in the plot grid data

      call destroy_(self%grid)
      call create_(self%grid,self%atom)
      call read_keywords_(self%grid)

   end subroutine

   subroutine read_isosurface(self)
    type(mol_type) :: self
    ! Read in the isosurface data. NOTE: the isosurface has its own plotgrid,
    ! different from that used in normal density plots.

      call destroy_(self%isosurface)
      call create_(self%isosurface,self%atom)
       !!!!!!!!!!!!!!!!!!!!!!!!!
      call read_keywords_(self%isosurface)
       !!!!!!!!!!!!!!!!!!!!!!!!!

   end subroutine

   subroutine read_CX_surface(self)
    type(mol_type) :: self
    ! Read in special crystalexplorer isosurface data. NOTE: the isosurface has
    ! its own plotgrid, different from that used in normal density plots.
      type(atom_type), dimension(:), pointer :: fragment_atom
      call ensure_(tonto,associated(self%cluster),"MOL:read_CX_surface ... no cluster data, use cluster= keyword")

      call ensure_(tonto,associated(self%cluster%asymmetric_cell_atom),"MOL:read_CX_surface ... no cluster asymmetric cell at&
&oms")
      call ensure_(tonto,associated(self%cluster%crystal),"MOL:read_CX_surface ... no cluster crystal")
      call ensure_(tonto,associated(self%atom),"MOL:read_CX_surface ... no atom list")
      call destroy_(self%isosurface)
      call create_(self%isosurface,self%atom)
      call create_(fragment_atom,self%cluster%n_fragment_atoms)
      call make_fragment_atom_(self%cluster,fragment_atom)
      call set_defaults_(self%isosurface%grid,fragment_atom)  ! Make Hirshfeld for fragment
      self%isosurface%iso_kind = "stockholder_density"
       !!!!!!!!!!!!!!!!!!!!!!!!!
      call read_keywords_(self%isosurface)
       !!!!!!!!!!!!!!!!!!!!!!!!!
      if (associated(self%coppensbasis) .and. self%isosurface%use_interpolator) &
         call make_coppens_interpolators_(self%atom)
      if (associated(self%slaterbasis) .and. self%isosurface%use_interpolator) &
         call make_slater_interpolators_(self%atom)

   end subroutine

   subroutine read_dftgrid(self)
    type(mol_type) :: self
    ! Read in the DFT grid data

      if (.not. associated(self%dftgrid)) call create_(self%dftgrid)
      call read_keywords_(self%dftgrid)
      call set_root_name_(self%dftgrid%archive,self%name)

   end subroutine

   subroutine read_crystal(self)
    type(mol_type) :: self
    ! Read in the crystal data

      call warn_if_(tonto,associated(self%crystal),"MOL:read_crystal ... crystal data already defined!")
      if (.not. associated(self%crystal)) call create_(self%crystal)
      call read_keywords_(self%crystal)
      call update_(self%crystal)
      if (associated(self%atom)) then
         call resolve_axis_system_(self)
         call make_reduced_group_data_(self%crystal,self%atom)
      end if

   end subroutine

   subroutine read_pointgroup(self)
    type(mol_type) :: self
    ! Read in the pointgroup data

      if (.not. associated(self%pointgroup)) call create_(self%pointgroup)
      call read_keywords_(self%pointgroup)

   end subroutine

   subroutine read_scfdata(self)
    type(mol_type) :: self
    ! Read in the scf data

      if (.not. associated(self%scfdata)) call create_(self%scfdata)
      call read_keywords_(self%scfdata)
      if (using_diis_(self%scfdata)) then
      call set_archive_root_name_(self%scfdata%diis,self%name)
      end if

   end subroutine

   subroutine read_robydata(self)
    type(mol_type) :: self
    ! Read in the Roby data. NOTE: this keyword must appear after a basis set
    ! has been defined
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: S

      call ensure_(tonto,self%basis_info_made,"MOL:read_robydata ... no basis info")
      call ensure_(tonto,associated(self%atom),"MOL:read_robydata ... no atom list")
      if (associated(self%roby)) then
         call destroy_(self%roby)
      end if
      call create_(S,self%n_bf,self%n_bf)
      call get_overlap_matrix_(self,S)
      call create_(self%roby,self,S)
      call read_keywords_(self%roby)

   end subroutine

   subroutine read_atoms(self)
    type(mol_type) :: self
    ! Read in the atom list information

      call warn_if_(tonto,associated(self%atom),"MOL:read_atoms ... atom list already defined!")
     ! if (.atom.created) then
     !    .atom.nullify_bases
     !    .atom.destroy
     ! end
      call read_list_keywords_(self%atom)
      call set_atom_info_(self)
      call resolve_basis_info_(self)
      if (associated(self%crystal)) then
         call resolve_axis_system_(self)
         call make_reduced_group_data_(self%crystal,self%atom)
      end if

   end subroutine

   subroutine read_basis_set_kind(self)
    type(mol_type) :: self
    ! Read in a suffix string representing the basis type to be used
    ! for the atoms.

      call read_(stdin,self%basis_set_kind)
      call resolve_basis_info_(self)

   end subroutine

   subroutine read_basis_sets(self)
    type(mol_type) :: self
    ! Read in a list of atomic basis sets for the molecule

      call warn_if_(tonto,associated(self%basis),"MOL:read_basis_sets ... basis sets already defined!")
      call read_list_keywords_(self%basis)
      if (associated(self%basis)) call unnormalise_(self%basis)  ! NOTE this
      call resolve_basis_info_(self)

   end subroutine

   subroutine read_slaterbasis_sets(self)
    type(mol_type) :: self
    ! Read in a list of atomic Coppens basis sets for the molecule

      call warn_if_(tonto,associated(self%slaterbasis),"MOL:read_slaterbasis_sets ... slaterbasis sets already defined!")
      call read_list_keywords_(self%slaterbasis)
      call resolve_basis_info_(self)

   end subroutine

   subroutine read_coppensbasis_sets(self)
    type(mol_type) :: self
    ! Read in a list of atomic Coppens basis sets for the molecule

      call warn_if_(tonto,associated(self%coppensbasis),"MOL:read_coppensbasis_sets ... coppensbasis sets already defined!")
      call read_list_keywords_(self%coppensbasis)
      call resolve_basis_info_(self)

   end subroutine

   subroutine read_atom_groups(self)
    type(mol_type) :: self
    ! Read in the .atom_group array
      integer(kind=kind(1)) :: n,n_group,i,length,line,item
      integer(kind=kind(1)), dimension(:), pointer :: atoms,chg
      integer(kind=kind(1)), dimension(2) :: range
      character(128) :: word

      call ensure_(tonto,associated(self%atom),"MOL:read_atom_groups ... no atom list")
      call ensure_(tonto,next_item_(stdin)=="{","MOL:read_atom_groups ... Expecting open bracket symbol: {")
      call ensure_(tonto,.not. associated(self%atom_group),"MOL:read_atom_groups ... groups already defined")
      call ensure_(tonto,.not. groups_defined_(self%atom),"MOL:read_atom_groups ... groups already defined")
      call read_(stdin,word)
      line = line_number_(stdin)
      item = next_line_item_(stdin)
      n = 0
      do                              ! Loop over keywords
         nullify(atoms)
         call read_(stdin,word)
         call to_lower_case_(word)
         select case (word)
            case("}             "); exit
            case("atom_group=   "); call read_intvec_ptr_(stdin,atoms); n = n+1
            case("atom_range   ="); call read_intvec_(stdin,range);     n = n+1
            case("group_charges="); call read_intvec_ptr_(stdin,chg);   n = n+1
            case default;           allocate(tonto%known_keywords(4))
            tonto%known_keywords(1) = "}             "
            tonto%known_keywords(2) = "atom_group=   "
            tonto%known_keywords(3) = "atom_range   ="
            tonto%known_keywords(4) = "group_charges="
            call unknown_(tonto,word,"MOL:read_atom_groups")
            deallocate(tonto%known_keywords)
         end select
         if (associated(atoms)) call destroy_(atoms)
         if (associated(chg))   call destroy_(chg)
      end do
       !
      call move_to_line_(stdin,line)
      call move_to_line_item_(stdin,item)
      n_group = n
      call create_(self%atom_group,n)
      n = 0
      do                              ! Loop over keywords
         call read_(stdin,word)
         call to_lower_case_(word)
         select case (word)
            case("}");               exit
            case("atom_group=")
               n = n+1
               call read_intvec_ptr_(stdin,self%atom_group(n)%element)
               call ensure_(tonto,all(self%atom_group(n)%element>0),"MOL:read_atom_groups ... non-positive atom indices")
               call ensure_(tonto,all(self%atom_group(n)%element<=self%n_atom),"MOL:read_atom_groups ... index too large")
            case("atom_range=")
               n = n+1
               call read_intvec_(stdin,range)
               length = range(2) - range(1) + 1
               call ensure_(tonto,length>=1,"MOL:read_atom_groups ... non-positive atom group range!")
               call create_(self%atom_group(n)%element,length)
               self%atom_group(n)%element = (/ (i, i=range(1),range(2)) /)
            case("group_charges=")
               call read_intvec_ptr_(stdin,self%group_charges)
               call ensure_(tonto,size(self%group_charges)==n_group,"MOL:read_atom_groups ... wrong size, group_charges")
         end select
      end do
      call put_atom_groups_(self)

   end subroutine

   subroutine read_group_charges(self)
    type(mol_type) :: self
    ! Read in the .group_charges array

      call ensure_(tonto,.not. associated(self%group_charges),"MOL:read_group_charges ... charges already exist")
      call read_intvec_ptr_(stdin,self%group_charges)
      if (associated(self%atom_group)) then
        call ensure_(tonto,size(self%group_charges)==size(self%atom_group),"MOL:read_group_charges ... wrong size, group_char&
&ges")
      end if

   end subroutine

   subroutine make_atom_groups(self)
    type(mol_type) :: self
    ! Make the .atom_group array from the .atom(:).group information
      integer(kind=kind(1)) :: n_group,m_group,a,g,n,i
      integer(kind=kind(1)), dimension(:), pointer :: group_indices

      call ensure_(tonto,groups_defined_(self%atom),"MOL:make_atom_groups ... no group info supplied in atoms=")
      call ensure_(tonto,.not. associated(self%atom_group),"MOL:make_atom_groups ... atom_group already defined")
      call create_(group_indices,self%n_atom)
      group_indices = self%atom(:)%group
      n_group = no_of_unique_elements_(group_indices)
      if (.not. n_group == 1) then
        m_group = maxval(group_indices)
        call create_(self%atom_group,n_group)
        g = 0
        do a = 1,m_group
           n = count(group_indices==a)
           if (n==0) cycle
           g = g + 1
           call create_(self%atom_group(g)%element,n)
           self%atom_group(g)%element = pack( (/ (i, i=1,self%n_atom) /), group_indices==a)
        end do
        if (associated(self%group_charges)) &
        call ensure_(tonto,size(self%group_charges)==size(self%atom_group),"MOL:make_atom_groups ... wrong size, atom_groups"&
&)
        call put_atom_groups_(self)
      end if
      call destroy_(group_indices)

   end subroutine

   subroutine read_archive(self)
    type(mol_type) :: self
    ! Read the archive called "name". Must also specify a basis "orb_kind", e.g.
    ! "restricted".  For use with TONTO generated archives.
      character(128) :: name,orb_kind
      type(archive_type) :: arch

   call ensure_(tonto,n_line_items_(stdin)==3,"MOL:read_archive ... must specify an archive and a kind")
   call ensure_(tonto,self%n_bf>0,"MOL:read_archive ... need to already know the number of basis functions")
      call read_(stdin,name)
      call read_(stdin,orb_kind)
      call text_(stdout,"reading archive : " // trim(name))
      call set_(arch,self%name,name,genre=orb_kind)
      select case (name)
         case ("molecular_orbitals")
           if (.not. associated(self%molecular_orbitals)) call create_(self%molecular_orbitals,self%n_bf)
           call read_(arch,self%molecular_orbitals)
         case ("density_matrix    ")
           if (.not. associated(self%density_matrix)) call create_(self%density_matrix,self%n_bf)
           call read_(arch,self%density_matrix)
         case ("MP2_density_matrix")
           if (.not. associated(self%density_matrix)) call create_(self%density_matrix,self%n_bf)
           call read_(arch,self%density_matrix)
         case ("MP3_density_matrix")
           if (.not. associated(self%density_matrix)) call create_(self%density_matrix,self%n_bf)
           call read_(arch,self%density_matrix)
         case ("fock_matrix       ")
           if (.not. associated(self%fock_matrix)) call create_(self%fock_matrix,self%n_bf)
           call read_(arch,self%fock_matrix)
         case ("orbital_energies  ")
           if (.not. associated(self%orbital_energies)) call create_(self%orbital_energies,self%n_bf)
           call read_(arch,self%orbital_energies)
         case ("natural_orbitals  ")
           if (.not. associated(self%natural_orbitals)) call create_(self%natural_orbitals,self%n_bf)
           call read_(arch,self%natural_orbitals)
         case ("occupation_numbers")
           if (.not. associated(self%occupation_numbers)) call create_(self%occupation_numbers,self%n_bf)
           call read_(arch,self%occupation_numbers)
         case ("covalent_orbitals ")
           if (.not. associated(self%natural_orbitals)) call create_(self%natural_orbitals,self%n_bf)
           call read_(arch,self%natural_orbitals)
         case ("ionic_orbitals    ")
           if (.not. associated(self%natural_orbitals)) call create_(self%natural_orbitals,self%n_bf)
           call read_(arch,self%natural_orbitals)
         case default;                allocate(tonto%known_keywords(10))
         tonto%known_keywords(1) = "molecular_orbitals"
         tonto%known_keywords(2) = "density_matrix    "
         tonto%known_keywords(3) = "MP2_density_matrix"
         tonto%known_keywords(4) = "MP3_density_matrix"
         tonto%known_keywords(5) = "fock_matrix       "
         tonto%known_keywords(6) = "orbital_energies  "
         tonto%known_keywords(7) = "natural_orbitals  "
         tonto%known_keywords(8) = "occupation_numbers"
         tonto%known_keywords(9) = "covalent_orbitals "
         tonto%known_keywords(10) = "ionic_orbitals    "
         call unknown_(tonto,name,"MOL:read_archive")
         deallocate(tonto%known_keywords)
      end select
      call close_(arch)

   end subroutine

   subroutine read_ascii_archive(self)
    type(mol_type) :: self
    ! Read the archive called "name". Must also specify a basis "orb_kind", e.g.
    ! "restricted".
    ! If required, the input order may be specified as "by_row" (default) or
    ! "by_column".
      character(128) :: name,orb_kind,order
      type(archive_type) :: arch

   call ensure_(tonto,stdin%buffer%n_items==3,"MOL:read_ascii_archive ... must specify an archive and a kind")
   call ensure_(tonto,self%n_bf>0,"MOL:read_ascii_archive ... need to already know the number of basis functions")
      call read_(stdin,name)
      call read_(stdin,orb_kind)
      order = "by_row"
      if (.not. buffer_exhausted_(stdin)) call read_(stdin,order)
      call set_(arch,self%name,name,genre=orb_kind,format="ascii")
      select case (name)
         case ("molecular_orbitals")
           if (.not. associated(self%molecular_orbitals)) call create_(self%molecular_orbitals,self%n_bf)
           call read_(arch,self%molecular_orbitals,order=order)
         case ("density_matrix    ")
           if (.not. associated(self%density_matrix)) call create_(self%density_matrix,self%n_bf)
           call read_(arch,self%density_matrix,order=order)
         case ("MP2_density_matrix")
           if (.not. associated(self%density_matrix)) call create_(self%density_matrix,self%n_bf)
           call read_(arch,self%density_matrix,order=order)
         case ("MP3_density_matrix")
           if (.not. associated(self%density_matrix)) call create_(self%density_matrix,self%n_bf)
           call read_(arch,self%density_matrix,order=order)
         case ("orbital_energies  ")
           if (.not. associated(self%orbital_energies)) call create_(self%orbital_energies,self%n_bf)
           call read_(arch,self%orbital_energies)
         case ("natural_orbitals  ")
           if (.not. associated(self%natural_orbitals)) call create_(self%natural_orbitals,self%n_bf)
           call read_(arch,self%natural_orbitals,order=order)
         case ("occupation_numbers")
           if (.not. associated(self%occupation_numbers)) call create_(self%occupation_numbers,self%n_bf)
           call read_(arch,self%occupation_numbers)
         case ("covalent_orbitals ")
           if (.not. associated(self%natural_orbitals)) call create_(self%natural_orbitals,self%n_bf)
           call read_(arch,self%natural_orbitals,order=order)
         case ("ionic_orbitals    ")
           if (.not. associated(self%natural_orbitals)) call create_(self%natural_orbitals,self%n_bf)
           call read_(arch,self%natural_orbitals,order=order)
         case ("fock_matrix       ")
           if (.not. associated(self%fock_matrix)) call create_(self%fock_matrix,self%n_bf)
           call read_(arch,self%fock_matrix,order=order)
         case default;                allocate(tonto%known_keywords(10))
         tonto%known_keywords(1) = "molecular_orbitals"
         tonto%known_keywords(2) = "density_matrix    "
         tonto%known_keywords(3) = "MP2_density_matrix"
         tonto%known_keywords(4) = "MP3_density_matrix"
         tonto%known_keywords(5) = "orbital_energies  "
         tonto%known_keywords(6) = "natural_orbitals  "
         tonto%known_keywords(7) = "occupation_numbers"
         tonto%known_keywords(8) = "covalent_orbitals "
         tonto%known_keywords(9) = "ionic_orbitals    "
         tonto%known_keywords(10) = "fock_matrix       "
         call unknown_(tonto,name,"MOL:read_ascii_archive")
         deallocate(tonto%known_keywords)
      end select
      call close_(arch)

   end subroutine

   subroutine read_g94_checkpoint_file(self)
    type(mol_type) :: self
    ! Read a g94 checkpoint file (after fchk conversion to ASCII) into TONTO
      character(128) :: name
      type(textfile_type), pointer :: chkfile
      integer(kind=kind(1)), dimension(:), pointer :: shell_l, nps, shell_to_atom, e_atom
      integer(kind=kind(1)), dimension(:), pointer :: tmp_nps, tmp_sta, tmp_shells
      real(kind=kind(1.0d0)), dimension(:), pointer :: tmp_pe, tmp_cc
      real(kind=kind(1.0d0)), dimension(:), pointer :: pe, cc, extra_cc, temp_vec
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: temp_mat
      type(opmatrix_type), pointer :: temp_spin, temp_total, D
      type(basis_type), pointer :: basis1
      integer(kind=kind(1)) :: c, t, a, s, u, m1, m2, i,n_basis
      integer(kind=kind(1)) :: n_shell, n_comp, n_cc, nps_c
      logical(kind=kind(.true.)) :: same
      type(archive_type) :: arch

      if (associated(self%basis)) call destroy_(self%basis)
      if (associated(self%atom))  call destroy_(self%atom)
      name = trim(self%name) // achar(46) // "FChk"         ! name.FChk or
      if (.not. buffer_exhausted_(stdin)) call read_(stdin,name)  ! specified on stdin
      call create_(self%scfdata)

       ! Read in basic stuff.
      call create_(chkfile,name)
      call open_(chkfile,for="read")
      call read_line_(chkfile)
      call look_for_(chkfile,"Number of atoms")
      call move_to_line_item_(chkfile,5); call read_(chkfile,self%n_atom)
      call look_for_(chkfile,"Charge")
      call move_to_line_item_(chkfile,3); call read_(chkfile,self%charge)
      call look_for_(chkfile,"Multiplicity")
      call move_to_line_item_(chkfile,3); call read_(chkfile,self%mult)
      call look_for_(chkfile,"Number of electrons")
      call move_to_line_item_(chkfile,5); call read_(chkfile,self%n_e)
      call look_for_(chkfile,"Number of alpha electrons")
      call move_to_line_item_(chkfile,6); call read_(chkfile,self%n_a)
      call look_for_(chkfile,"Number of beta electrons")
      call move_to_line_item_(chkfile,6); call read_(chkfile,self%n_b)
      call look_for_(chkfile,"Number of basis functions")
      call move_to_line_item_(chkfile,6); call read_(chkfile,self%n_bf)
      call look_for_(chkfile,"Number of contracted shells")
      call move_to_line_item_(chkfile,6); call read_(chkfile,self%n_shell)
      call look_for_(chkfile,"Number of primitive shells")
      call move_to_line_item_(chkfile,6); call read_(chkfile,self%n_prim)
      call look_for_(chkfile,"Total Energy")
      call move_to_line_item_(chkfile,4)
      call read_real_(chkfile,self%scfdata%energy )
      call look_for_(chkfile,"Atomic numbers"); call read_line_(chkfile)
      call create_(self%atom,self%n_atom)
      do a = 1, self%n_atom
         call read_(chkfile,self%atom(a)%atomic_number )
         self%atom(a)%label = trim(chemical_symbol_(self%atom(a))) // trim(to_str_(a))
      end do
      call look_for_(chkfile,"Current cartesian coordinates")
      call read_line_(chkfile)
      do a = 1, self%n_atom
         call read_(chkfile,self%atom(a)%pos)
      end do

       ! Shell info.
      call look_for_(chkfile,"Shell types"); call read_line_(chkfile)
      call create_(tmp_shells,self%n_shell); call create_(tmp_nps,self%n_shell)
      call create_(tmp_sta,self%n_shell); call create_(tmp_pe,self%n_prim)
      call create_(tmp_cc,self%n_prim)
      call read_(chkfile,tmp_shells)
      call look_for_(chkfile,"Number of primitives per shell")
      call read_line_(chkfile); call read_(chkfile,tmp_nps)
      call look_for_(chkfile,"Shell to atom map"); call read_line_(chkfile)
      call read_(chkfile,tmp_sta)
      call look_for_(chkfile,"Primitive exponents"); call read_line_(chkfile)
      call read_(chkfile,tmp_pe)
      call look_for_(chkfile,"Contraction coefficients"); call read_line_(chkfile)
      call read_(chkfile,tmp_cc)
      call create_(e_atom,self%n_atom); e_atom = 0.0d0
      call create_(extra_cc,self%n_prim)
      if (any(tmp_shells == (-1) )) then
        do a = 1, self%n_shell
           if (tmp_shells(a) == (-1)) then
              e_atom(tmp_sta(a)) = e_atom(tmp_sta(a)) + 1
              self%n_prim = self%n_prim + tmp_nps(a)
           end if
        end do
        call look_for_(chkfile,"P(S=P)"); call read_line_(chkfile)
        call read_(chkfile,extra_cc)
      end if
      s = self%n_shell + sum(e_atom)
      call create_(shell_l,s); call create_(nps,s)
      call create_(pe,self%n_prim); call create_(cc,self%n_prim)
      call create_(shell_to_atom,s)
      a = 0
      m1 = 1; m2 = 1
      do c = 1, self%n_shell
        a = a + 1
        nps_c = tmp_nps(c)
        if (tmp_shells(c) == (-1)) then
           shell_l(a) = 0
           nps(a) = nps_c
           shell_to_atom(a) = tmp_sta(c)
           pe(m1:m1+nps_c-1) = tmp_pe(m2:m2+nps_c-1)
           cc(m1:m1+nps_c-1) = tmp_cc(m2:m2+nps_c-1)
           a = a+1; m1=m1+nps_c
           shell_l(a) = 1
           nps(a) = nps_c
           shell_to_atom(a) = tmp_sta(c)
           pe(m1:m1+nps_c-1) = tmp_pe(m2:m2+nps_c-1)
           cc(m1:m1+nps_c-1) = extra_cc(m2:m2+nps_c-1)
        else
           pe(m1:m1+nps_c-1) = tmp_pe(m2:m2+nps_c-1)
           cc(m1:m1+nps_c-1) = tmp_cc(m2:m2+nps_c-1)
          ! shell_l(a) = tmp_shells(c)
           shell_l(a) = abs(tmp_shells(c))  ! WARNING: sometimes this can be -2
           nps(a) = nps_c
           shell_to_atom(a) = tmp_sta(c)
        end if
        m1 = m1 + nps_c; m2 = m2 + nps_c
      end do
      call destroy_(extra_cc)
      call destroy_(tmp_shells); call destroy_(tmp_sta)
      call destroy_(tmp_nps); call destroy_(tmp_pe); call destroy_(tmp_cc)
      self%n_shell = s
      c = 0       ! for recording number of primitives read
      t = 0       ! for recording number of shells read
      n_basis = 0
      call create_(self%basis,n_basis)
      do a = 1, self%n_atom
         call create_(basis1)
         basis1%label = trim(self%atom(a)%label) // "_g94_basis"
         n_shell = count(shell_to_atom==a)
         basis1%n_shell = n_shell
         call create_(basis1%shell,n_shell)
         call nullify_ptr_part_(basis1%shell)
         do s = 1, n_shell
            n_comp = n_comp_(shell_l(t+s))
            n_cc = nps(t+s)
            basis1%shell(s)%l = shell_l(t+s)
            basis1%shell(s)%n_comp = n_comp
            basis1%shell(s)%n_cc = n_cc
            call create_(basis1%shell(s)%ex,n_cc )
            call create_(basis1%shell(s)%cc,n_cc )
            basis1%shell(s)%ex = pe(c+1 : c+n_cc )
            basis1%shell(s)%cc = cc(c+1 : c+n_cc )
            c = c + n_cc
         end do
         basis1%n_bf = no_of_basis_functions_(basis1)
         basis1%n_prim = no_of_primitives_(basis1)
        ! basis1.unnormalise
         t = t + n_shell
         same = .false.
         do i = 1,n_basis
            if (same_as_(self%basis(i),basis1)) then
               same = .true.
               exit
            end if
         end do
         call create_(self%atom(a)%basis)
         if (same) then
            self%atom(a)%basis%label = self%basis(i)%label
         else
            self%atom(a)%basis%label =  basis1%label
            n_basis = n_basis + 1
            call expand_(self%basis,n_basis)
            call copy_(self%basis(n_basis),basis1)
         end if
         call destroy_(basis1)
      end do
      call destroy_(pe); call destroy_(cc)
      call destroy_(shell_to_atom); call destroy_(nps); call destroy_(e_atom)
      call destroy_(shell_l)
      m1 = self%n_bf
      call set_atom_info_(self)
      call resolve_basis_info_(self)
      call ensure_(tonto,m1==self%n_bf,"MOL:read_g94_checkpoint_file ... No. of basis functions inconsistent with basis set, &
&did you use 6d/10f?")

       ! Read in orbital energies.
      call create_(temp_vec,self%n_bf)
      call look_for_(chkfile,"Alpha Orbital Energies")
      call read_line_(chkfile); call read_(chkfile,temp_vec)
      if (next_str_(chkfile) == "Beta") then
         u = 1
         self%scfdata%scf_kind = "uhf"
         call create_(self%orbital_energies,self%n_bf, "beta")
         call read_line_(chkfile); call read_(chkfile,self%orbital_energies%beta)
         self%orbital_energies%alpha => temp_vec
         nullify(temp_vec)
      else
         u = 0
         self%scfdata%scf_kind = "rhf"
         call create_(self%orbital_energies,self%n_bf)
         self%orbital_energies%restricted => temp_vec
         nullify(temp_vec)
      end if

       ! Read in molecular orbitals.
      call create_(temp_mat,self%n_bf, self%n_bf)
      call look_for_(chkfile,"Alpha MO"); call read_line_(chkfile)
      call read_(chkfile,temp_mat)
      call to_transpose_(temp_mat)
      if (next_str_(chkfile) == "Beta") then
         call create_(self%molecular_orbitals,self%n_bf, "beta")
         call read_line_(chkfile); call read_(chkfile,self%molecular_orbitals%beta)
         call to_transpose_(self%molecular_orbitals%beta)
         self%molecular_orbitals%alpha => temp_mat
         nullify(temp_vec)
         call swap_g94_orbital_order_(self,self%molecular_orbitals%alpha,"row")
         call swap_g94_orbital_order_(self,self%molecular_orbitals%beta, "row")
      else
         call create_(self%molecular_orbitals,self%n_bf)
         self%molecular_orbitals%restricted => temp_mat
         nullify(temp_vec)
         call swap_g94_orbital_order_(self,self%molecular_orbitals%restricted,"row")
      end if

       ! Read in density matrix.
      if (u==0) then
         call create_(self%density_matrix,self%n_bf, "restricted")
         call create_(self%density_matrix%triangle,l_compress_(self%density_matrix,"restricted"))
         call look_for_(chkfile,"Total SCF Density"); call read_line_(chkfile)
         call read_(chkfile,self%density_matrix%triangle)
         call uncompress_(self%density_matrix)
         call swap_g94_orbital_order_(self,self%density_matrix%restricted,"row")
         call swap_g94_orbital_order_(self,self%density_matrix%restricted,"column")
      else
         call create_(temp_total,self%n_bf, "restricted")
         call create_(temp_spin,self%n_bf, "restricted")
         call create_(temp_total%triangle,l_compress_(temp_total,"restricted"))
         call look_for_(chkfile,"Total SCF Density"); call read_line_(chkfile)
         call read_(chkfile,temp_total%triangle)
         call uncompress_(temp_total)
         call create_(temp_spin%triangle,l_compress_(temp_spin,"restricted"))
         call look_for_(chkfile,"Spin SCF Density"); call read_line_(chkfile)
         call read_(chkfile,temp_spin%triangle)
         call uncompress_(temp_spin)
         call create_(self%density_matrix,self%n_bf, "alpha")
         call create_(self%density_matrix,"beta")
         self%density_matrix%beta = (temp_total%restricted - temp_spin%restricted)/2
         self%density_matrix%alpha = (temp_spin%restricted + temp_total%restricted)/2
         call destroy_(temp_spin)
         call destroy_(temp_total)
         call swap_g94_orbital_order_(self,self%density_matrix%alpha,"row")
         call swap_g94_orbital_order_(self,self%density_matrix%alpha,"column")
         call swap_g94_orbital_order_(self,self%density_matrix%beta, "row")
         call swap_g94_orbital_order_(self,self%density_matrix%beta, "column")
      end if

       ! Extract MP2 density matrix to file.  UHF untested
      if (has_string_(chkfile,"MP2 Density")) then
        if (u==0) then
           call create_(D,self%n_bf, "restricted")
           call create_(D%triangle,l_compress_(D,"restricted"))
           call look_for_(chkfile,"Total MP2 Density"); call read_line_(chkfile)
           call read_(chkfile,D%triangle)
           call uncompress_(D)
           call swap_g94_orbital_order_(self,D%restricted,"row")
           call swap_g94_orbital_order_(self,D%restricted,"column")
           call set_(arch,self%name,"MP2_density_matrix")
           call write_(arch,D)
           call destroy_(D)
        else
           call create_(temp_total,self%n_bf, "restricted")
           call create_(temp_spin,self%n_bf, "restricted")
           call create_(temp_total%triangle,l_compress_(temp_total,"restricted"))
           call look_for_(chkfile,"Total MP2 Density"); call read_line_(chkfile)
           call read_(chkfile,temp_total%triangle)
           call uncompress_(temp_total)
           call create_(temp_spin%triangle,l_compress_(temp_spin,"restricted"))
           call look_for_(chkfile,"Spin MP2 Density"); call read_line_(chkfile)
           call read_(chkfile,temp_spin%triangle)
           call uncompress_(temp_spin)
           call create_(D,self%n_bf, "alpha")
           call create_(D,"beta")
           D%beta = (temp_total%restricted - temp_spin%restricted)/2
           D%alpha = (temp_spin%restricted + temp_total%restricted)/2
           call destroy_(temp_spin)
           call destroy_(temp_total)
           call swap_g94_orbital_order_(self,D%alpha,"row")
           call swap_g94_orbital_order_(self,D%alpha,"column")
           call swap_g94_orbital_order_(self,D%beta, "row")
           call swap_g94_orbital_order_(self,D%beta, "column")
           call set_(arch,self%name,"MP2_density_matrix")
           call write_(arch,D)
           call destroy_(D)
        end if
      end if

       ! Extract MP3 density matrix to file.  UHF untested
      if (has_string_(chkfile,"MP3 Density")) then
        if (u==0) then
           call create_(D,self%n_bf, "restricted")
           call create_(D%triangle,l_compress_(D,"restricted"))
           call look_for_(chkfile,"Total MP3 Density"); call read_line_(chkfile)
           call read_(chkfile,D%triangle)
           call uncompress_(D)
           call swap_g94_orbital_order_(self,D%restricted,"row")
           call swap_g94_orbital_order_(self,D%restricted,"column")
           call set_(arch,self%name,"MP3_density_matrix")
           call write_(arch,D)
           call destroy_(D)
        else
           call create_(temp_total,self%n_bf, "restricted")
           call create_(temp_spin,self%n_bf, "restricted")
           call create_(temp_total%triangle,l_compress_(temp_total,"restricted"))
           call look_for_(chkfile,"Total MP3 Density"); call read_line_(chkfile)
           call read_(chkfile,temp_total%triangle)
           call uncompress_(temp_total)
           call create_(temp_spin%triangle,l_compress_(temp_spin,"restricted"))
           call look_for_(chkfile,"Spin MP3 Density"); call read_line_(chkfile)
           call read_(chkfile,temp_spin%triangle)
           call uncompress_(temp_spin)
           call create_(D,self%n_bf, "alpha")
           call create_(D,"beta")
           D%beta = (temp_total%restricted - temp_spin%restricted)/2
           D%alpha = (temp_spin%restricted + temp_total%restricted)/2
           call destroy_(temp_spin)
           call destroy_(temp_total)
           call swap_g94_orbital_order_(self,D%alpha,"row")
           call swap_g94_orbital_order_(self,D%alpha,"column")
           call swap_g94_orbital_order_(self,D%beta, "row")
           call swap_g94_orbital_order_(self,D%beta, "column")
           call set_(arch,self%name,"MP3_density_matrix")
           call write_(arch,D)
           call destroy_(D)
        end if
      end if

      call close_(chkfile); call destroy_(chkfile)

       ! Save data in archive files
      call set_(arch,self%name,"density_matrix")
      call write_(arch,self%density_matrix)
      call set_(arch,self%name,"molecular_orbitals")
      call write_(arch,self%molecular_orbitals)
      call set_(arch,self%name,"orbital_energies")
      call write_(arch,self%orbital_energies)

      if (associated(self%crystal)) then
         call resolve_axis_system_(self)
         call make_reduced_group_data_(self%crystal,self%atom)
      end if

   end subroutine

   subroutine swap_g94_orbital_order(self,X,swap)
    type(mol_type) :: self
    ! Swap the order of f orbitals on matrix "X" after reading a g94 checkpoint
    ! file, for "swap" equal to "row" or "1", or "coloumn" or "2".
       real(kind=kind(1.0d0)), dimension(:,:) :: X
      character(*) :: swap
      integer(kind=kind(1)) :: n,f,l
      integer(kind=kind(1)), dimension(10) :: ff = (/ 1, 2, 3, 5, 6, 4, 9, 7, 8,10 /)

      select case (swap)
         case("row","1")
            do n = 1,self%n_shell
               f = self%first_basis_fn_for_shell(n)
               l = self%last_basis_fn_for_shell(n)
   call ensure_(tonto,(l-f)<=9,"MOL:swap_g94_orbital_order ... cannot yet convert order for g shells")
               if ((l-f)==9) then  ! f functions
                  X(f:l,:) = X(f-1+ff,:)
               end if
            end do
         case("column","2")
            do n = 1,self%n_shell
               f = self%first_basis_fn_for_shell(n)
               l = self%last_basis_fn_for_shell(n)
   call ensure_(tonto,(l-f)<=9,"MOL:swap_g94_orbital_order ... cannot yet convert order for g shells")
               if ((l-f)==9) then  ! f functions
                  X(:,f:l) = X(:,f-1+ff)
               end if
            end do
         case default
            call die_(tonto,"MOL:swap_g94_orbital_order ... unknown swap kind, "//trim(swap))
      end select

   end subroutine

   subroutine write_wfn_file(self)
    type(mol_type) :: self
    ! writes a .wfn file for input to morphy98
      character(128) :: name
      type(textfile_type), pointer :: wfnfile
      integer(kind=kind(1)), dimension(:), pointer :: lvec
      real(kind=kind(1.0d0)), dimension(:), pointer :: evec
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: dmatrix, cc
      type(shell_type), pointer :: sh
      integer(kind=kind(1)) :: i, j, pcount, n_orbitals, a, l, atomn, shelln
!      title_format :: STR, parameter = "(A80)"
      character(128), parameter :: n_vars_format = "(A8, 10X, I5, 15X, I5, 15X, I5, 17X)"
      character(128), parameter :: atom_format = "(A5, I3, '    (CENTRE', 1I3, ') ', 3F12.8, '          ', F5.1)"
      character(128), parameter :: c_assignment_format = "('CENTRE ASSIGNMENTS  ', 20I3)"
      character(128), parameter :: t_assignment_format = "('TYPE ASSIGNMENTS    ', 20I3)"
      character(128), parameter :: exponent_format = "('EXPONENTS ', 5D14.7)"
      character(128), parameter :: mol_title_format = "(1A4, I3, 1A30, F12.8, 1A15, F12.8)"
      character(128), parameter :: coefficient_format = "(5D16.8)"
      character(128), parameter :: e_v_format = "(' THE HF ENERGY = ', F20.12, ' THE VIRIAL(-V/T)=', F13.8)"

   call ensure_(tonto,created_(self%occupation_numbers,"restricted"),"MOL:write_wfn_file ... No occupation numbers")
   call ensure_(tonto,created_(self%orbital_energies,"restricted"),"MOL:write_wfn_file ... No orbital energies")
   call ensure_(tonto,created_(self%molecular_orbitals,"restricted"),"MOL:write_wfn_file ... No orbitals")
      name = self%name
      if (.not. buffer_exhausted_(stdin)) call read_(stdin,name)
      call create_(wfnfile,name)
      call open_(wfnfile,for="write")
      if (mod(self%n_e, 2) == 0) then
         n_orbitals = self%n_e / 2
      else
         n_orbitals = (self%n_e + 1) / 2
      end if
      write(unit = wfnfile%unit, fmt = '(a)') trim(name) // " computed by TONTO"
      write(unit = wfnfile%unit, fmt = n_vars_format) "GAUSSIAN", n_orbitals, self%n_prim, self%n_atom
      write(unit = wfnfile%unit, fmt = atom_format) (self%atom(i)%label, i, i, &
         self%atom(i)%pos, real(self%atom(i)%atomic_number,kind=kind(1.0d0)) , i = 1, self%n_atom)
      write(unit = wfnfile%unit, fmt = c_assignment_format) ((j, i = 1, n_prim_(self%atom(j)) ), j = 1, self%n_atom )
      pcount = 1
      call create_(lvec,self%n_prim)
      call create_(evec,self%n_prim)
      do a = 1, self%n_shell
        atomn  = self%atom_for_shell(a)
        shelln = self%atom_shell_for_shell(a)
        sh => self%atom(atomn)%basis%shell(shelln)
        l = n_comp_sum_(sh%l) - sh%n_comp
        do j = 1, sh%n_cc
          if (sh%l == 3) then
            lvec(pcount  ) = l + 1
            lvec(pcount+1) = l + 2
            lvec(pcount+2) = l + 3
            lvec(pcount+3) = l + 4
            lvec(pcount+4) = l + 5
            lvec(pcount+5) = l + 7
            lvec(pcount+6) = l + 6
            lvec(pcount+7) = l + 8
            lvec(pcount+8) = l + 9
            lvec(pcount+9) = l + 10
            do i = 1, sh%n_comp
              evec(pcount) = sh%ex(j)
              pcount = pcount + 1
            end do
          else
            do i = 1, sh%n_comp
              evec(pcount) = sh%ex(j)
              lvec(pcount) = l + i
              pcount = pcount + 1
            end do
          end if
        end do
      end do
      write(unit = wfnfile%unit, fmt = t_assignment_format) lvec
      write(unit = wfnfile%unit, fmt = exponent_format) evec
      call destroy_(lvec)
      call destroy_(evec)
      call create_(dmatrix,self%n_prim, self%n_bf)
      dmatrix = 0.0d0
      call create_(cc,self%n_prim, self%n_bf);    call make_contraction_matrix_(self,cc)

      call to_product_of_(dmatrix,cc,self%molecular_orbitals%restricted)
      do i = 1, n_orbitals
         write(unit = wfnfile%unit, fmt = mol_title_format) &
            "MO  ", i, &
            "OCC NO = ", self%occupation_numbers%restricted(i), &
            " ORB. ENERGY = ", self%orbital_energies%restricted(i)
         write(unit = wfnfile%unit, fmt = coefficient_format) dmatrix(:, i)
      end do
      write(unit = wfnfile%unit, fmt = "(1A8)") "END DATA"
      write(unit = wfnfile%unit, fmt = e_v_format) self%scfdata%energy, 2.0D00
      call destroy_(cc)
      call destroy_(dmatrix)
      call close_(wfnfile)
      call destroy_(wfnfile)

   end subroutine

   subroutine write_archive(self)
    type(mol_type) :: self
    ! Write the archive called "name". The kind is defined by the object to be
    ! written.
      character(128) :: name
      type(archive_type) :: arch

      call read_(stdin,name)
      call set_(arch,self%name,name)
      select case (name)
         case ("molecular_orbitals"); call write_(arch,self%molecular_orbitals)
         case ("density_matrix    "); call write_(arch,self%density_matrix)
         case ("natural_orbitals  "); call write_(arch,self%natural_orbitals)
         case ("occupation_numbers"); call write_(arch,self%occupation_numbers)
         case ("fock_matrix       "); call write_(arch,self%fock_matrix)
         case ("orbital_energies  "); call write_(arch,self%orbital_energies)
         case default;    allocate(tonto%known_keywords(6))
         tonto%known_keywords(1) = "molecular_orbitals"
         tonto%known_keywords(2) = "density_matrix    "
         tonto%known_keywords(3) = "natural_orbitals  "
         tonto%known_keywords(4) = "occupation_numbers"
         tonto%known_keywords(5) = "fock_matrix       "
         tonto%known_keywords(6) = "orbital_energies  "
         call unknown_(tonto,name,"MOL:write_archive")
         deallocate(tonto%known_keywords)
      end select
      call close_(arch)

   end subroutine

   subroutine write_ascii_archive(self)
    type(mol_type) :: self
    ! Write the archive called "name". The kind is defined by the object to be
    ! written. If required, the output order may be specified as
    ! "by_row" (default) or "by_column".
      character(128) :: name,order
      type(archive_type) :: arch

      call read_(stdin,name)
      order = "by_row"
      if (.not. buffer_exhausted_(stdin)) call read_(stdin,order)
      call set_(arch,self%name,name,format="ascii")
      select case (name)
         case ("molecular_orbitals"); call write_(arch,self%molecular_orbitals,order=order)
         case ("density_matrix    "); call write_(arch,self%density_matrix,order=order)
         case ("natural_orbitals  "); call write_(arch,self%natural_orbitals,order=order)
         case ("occupation_numbers"); call write_(arch,self%occupation_numbers,order=order)
         case ("fock_matrix       "); call write_(arch,self%fock_matrix,order=order)
         case ("orbital_energies  "); call write_(arch,self%orbital_energies,order=order)
         case default;     allocate(tonto%known_keywords(6))
         tonto%known_keywords(1) = "molecular_orbitals"
         tonto%known_keywords(2) = "density_matrix    "
         tonto%known_keywords(3) = "natural_orbitals  "
         tonto%known_keywords(4) = "occupation_numbers"
         tonto%known_keywords(5) = "fock_matrix       "
         tonto%known_keywords(6) = "orbital_energies  "
         call unknown_(tonto,name,"MOL:write_ascii_archive")
         deallocate(tonto%known_keywords)
      end select
      call close_(arch)

   end subroutine

!  **************
!  Output methods
!  **************

   subroutine put(self)
    type(mol_type) :: self
    ! Put all the available molecule information on file "out"

     call flush_(stdout)
     call text_(stdout,"Molecule information:")
     call flush_(stdout)
     call show_(stdout,"Name                   =",self%name)
     call show_(stdout,"Charge                 =",self%charge)
     call show_(stdout,"Multiplicity           =",self%mult)
     call show_(stdout,"Applied E Field        =",self%E_field(1),self%E_field(2),self%E_field(3))
     call show_(stdout,"Applied B Field        =",self%B_field(1),self%B_field(2),self%B_field(3))
     call show_(stdout,"B field Gauge origin   =",self%gauge_origin)
     if (associated(self%atom))        call put_atoms_(self)
     if (associated(self%atom_group))  call put_atom_groups_(self)
     if (associated(self%pointgroup))  call put_pointgroup_(self)
     if (associated(self%crystal))     call put_crystal_(self)
     if (associated(self%cluster))     call put_cluster_(self)
!    if (.grid.created)        .put_grid
     if (associated(self%dftgrid))     call put_dftgrid_(self)

   end subroutine

   subroutine put_atoms(self)
    type(mol_type) :: self
    ! Output the atom coordinate and basis set information

   call ensure_(tonto,associated(self%atom),"MOL:put_atoms ... no atom list")
      call flush_(stdout)
      call text_(stdout,"Molecule atom information:")
      call flush_(stdout)
      call show_(stdout,"Chemical Formula       =",trim(chemical_formula_(self)))
      call show_(stdout,"No of atoms            =",self%n_atom)
      call show_(stdout,"No of electrons        =",self%n_e)
      call show_(stdout,"No of alpha electrons  =",self%n_a)
      call show_(stdout,"No of beta  electrons  =",self%n_b)
      call put_coord_info_(self%atom)
      if (associated(self%basis))        call put_bases_(self)
      if (associated(self%coppensbasis)) call put_coppensbases_(self)
      if (associated(self%slaterbasis))  call put_slaterbases_(self)
      if (associated(self%crystal))      call put_atom_thermal_tensors_(self)

   end subroutine

   subroutine put_all_bonds(self)
    type(mol_type) :: self
    ! Put all the bond length info out

      call put_coord_info_(self%atom,all=.true.)

   end subroutine

   subroutine put_bases(self)
    type(mol_type) :: self
    ! Output the molecule basis set information, if the bases are all resolved.

   call ensure_(tonto,associated(self%atom),"MOL:put_bases ... no atom list")
   call ensure_(tonto,associated(self%basis),"MOL:put_bases ... no basis set list")
      if (.not. bases_are_resolved_(self%atom)) then;   return; end if
      call flush_(stdout)
      call text_(stdout,"Molecule atom basis set information:")
      call flush_(stdout)
      call show_(stdout,"No. of basis sets      =",self%n_basis)
      call show_(stdout,"No. of shells          =",self%n_shell)
      call show_(stdout,"No. of shell pairs     =",self%n_shell_pairs)
      call show_(stdout,"No. of basis functions =",self%n_bf)
      call show_(stdout,"No. of primitives      =",self%n_prim)
      call renormalise_(self%basis)
      call put_(self%basis)
      call unnormalise_(self%basis)

   end subroutine

   subroutine put_slaterbases(self)
    type(mol_type) :: self
    ! Output the molecule slaterbasis set information, if the bases are all
    ! resolved.

   call ensure_(tonto,associated(self%atom),"MOL:put_slaterbases ... no atom list")
   call ensure_(tonto,associated(self%slaterbasis),"MOL:put_slaterbases ... no basis set list")
      if (.not. slaterbases_are_resolved_(self%atom)) then;   return; end if
      call flush_(stdout)
      call text_(stdout,"Molecule atom slaterbasis set information:")
      call flush_(stdout)
      call show_(stdout,"No. of basis sets =",size(self%slaterbasis))
      call put_(self%slaterbasis)

   end subroutine

   subroutine put_coppensbases(self)
    type(mol_type) :: self
    ! Output the molecule coppensbasis set information, if the bases are all
    ! resolved.

   call ensure_(tonto,associated(self%atom),"MOL:put_coppensbases ... no atom list")
   call ensure_(tonto,associated(self%coppensbasis),"MOL:put_coppensbases ... no basis set list")
      if (.not. coppensbases_are_resolved_(self%atom)) then;   return; end if
      call flush_(stdout)
      call text_(stdout,"Molecule atom coppensbasis set information:")
      call flush_(stdout)
      call show_(stdout,"No. of basis sets =",size(self%coppensbasis))
      call show_(stdout,"Maximum N value   =",maximum_basis_n_value_(self%coppensbasis))
      call show_(stdout,"No. of orbitals   =",no_of_orbitals_(self%coppensbasis))
      call show_(stdout,"No. of primitives =",no_of_primitives_(self%coppensbasis))
      call put_(self%coppensbasis)

   end subroutine

   subroutine put_atom_thermal_tensors(self)
    type(mol_type) :: self
    ! Output the thermal tensors if they exist.
     character(128) :: thermal_smearing_model

   call ensure_(tonto,associated(self%crystal),"MOL:put_atom_thermal_tensors ... no crystal info")
   call ensure_(tonto,associated(self%atom),"MOL:put_atom_thermal_tensors ... no atom info")
     thermal_smearing_model = self%crystal%thermal_smearing_model
     if (thermal_smearing_model==" ") then
       call flush_(stdout)
       call put_(stdout,"No thermal smearing.")
       call flush_(stdout)
     else
       call flush_(stdout)
       call put_(stdout,"Thermal tensor information:")
       call flush_(stdout)
       call show_(stdout,"Thermal smearing model = ", trim(thermal_smearing_model))
       call flush_(stdout)
   call ensure_(tonto,associated(self%atom),"MOL:put_atom_thermal_tensors ... no atom list data")
       call put_thermal_tensors_(self%atom)
     end if

   end subroutine

   subroutine put_atom_groups(self)
    type(mol_type) :: self
    ! Out out the .atom_group info array
      integer(kind=kind(1)) :: n,i

   call ensure_(tonto,associated(self%atom_group),"MOL:put_atom_groups ... no atom group information")
      call save_(stdout)
      call set_int_width_(stdout,3)
      call set_n_fields_(stdout,25)
      call set_use_labels_(stdout,.false.)
      call text_(stdout," ")
      call text_(stdout,"Atom group information")
      call text_(stdout," ")
      call show_(stdout,"n_groups =",size(self%atom_group))
      do n = 1,size(self%atom_group)
         call put_text_(stdout,"group "// trim(to_str_(n)) //"  =")
         do i = 1,size(self%atom_group(n)%element)
            call put_(stdout,self%atom_group(n)%element(i))
         end do
         call flush_(stdout)
      end do
      if (associated(self%group_charges)) then
         call put_text_(stdout,"group charges =")
         do i = 1,size(self%group_charges)
            call put_(stdout,self%group_charges(i))
         end do
      end if
      call unsave_(stdout)

   end subroutine

   subroutine put_pointgroup(self)
    type(mol_type) :: self
    ! Output the current pointgroup

      call put_(self%pointgroup)

   end subroutine

   subroutine put_plotgrid(self)
    type(mol_type) :: self
    ! Output the current grid.

   call ensure_(tonto,associated(self%grid),"MOL:put_plotgrid ... no plot grid to output")
     call put_(self%grid)

   end subroutine

   subroutine put_dftgrid(self)
    type(mol_type) :: self
    ! Output the current DFT grid.

   call ensure_(tonto,associated(self%dftgrid),"MOL:put_dftgrid ... no DFT grid to output")
     call put_(self%dftgrid)

   end subroutine

   subroutine put_crystal(self)
    type(mol_type) :: self
    ! Output the current crystal

      if (associated(self%atom)) then; call put_(self%crystal,self%atom)
      else;                    call put_(self%crystal)
      end if

   end subroutine

   subroutine put_cluster(self)
    type(mol_type) :: self
    ! Put out the cluster information

   call ensure_(tonto,associated(self%cluster),"MOL:put_cluster ... no cluster data")
      call put_(self%cluster)

   end subroutine

   subroutine put_crystal_reflection_data(self)
    type(mol_type) :: self
    ! Output the current crystal

      call put_reflection_data_(self%crystal)

   end subroutine

   subroutine put_molecular_orbitals(self)
    type(mol_type) :: self
    ! Output the current associated molecular orbitals

      call flush_(stdout)
      call text_(stdout,"Molecular orbitals:")
      call put_(stdout,self%molecular_orbitals)

   end subroutine

   subroutine put_mos_and_energies(self)
    type(mol_type) :: self
    ! Output the current associated molecular orbitals and their energies

      call flush_(stdout)
      call text_(stdout,"Molecular orbital energies:")
      call put_(stdout,self%orbital_energies, format="column")
      call flush_(stdout)
      call text_(stdout,"Molecular orbitals:")
      call put_(stdout,self%molecular_orbitals)

   end subroutine

   subroutine put_natural_orbitals(self)
    type(mol_type) :: self
    ! Output the current associated molecular orbitals

      call flush_(stdout)
      call text_(stdout,"Natural orbital occupations:")
      call put_(stdout,self%occupation_numbers, format="column")
      call flush_(stdout)
      call text_(stdout,"Natural orbitals:")
      call put_(stdout,self%natural_orbitals)

   end subroutine

   subroutine put_density_matrix(self)
    type(mol_type) :: self
    ! Output the current associated density matrix

   call ensure_(tonto,associated(self%density_matrix),"MOL:put_density_matrix ... no density matrix")
      call flush_(stdout)
      call text_(stdout,"Density matrix:")
      call put_(stdout,self%density_matrix)

   end subroutine

   subroutine put_fock_matrix(self)
    type(mol_type) :: self
    ! Output the current associated fock matrix

   call ensure_(tonto,associated(self%fock_matrix),"MOL:put_fock_matrix ... no fock matrix")
      call flush_(stdout)
      call text_(stdout,"Fock matrix:")
      call put_(stdout,self%fock_matrix)

   end subroutine

   subroutine put_PND_sf(self)
    type(mol_type) :: self
    ! Output the magnetic structure factors

      call put_PND_sf_(self%crystal,self%name)

   end subroutine

   subroutine put_current_time(self,timer)
    type(mol_type) :: self
    ! Output the current time
      type(time_type) :: timer

      call text_(stdout,current_time_(timer))

   end subroutine

   subroutine put_time_taken(self,timer)
    type(mol_type) :: self
    ! Output the time taken as given by the "timer" object
      type(time_type) :: timer

      call text_(stdout,time_taken_(timer))

   end subroutine

   subroutine put_total_time(self)
    type(mol_type) :: self
    ! Output the total time

      call text_(stdout,time_taken_(std_time))

   end subroutine

   subroutine put_vrml(self)
    type(mol_type) :: self
    ! Output a VRML file for the coordinate geometry
     type(textfile_type), pointer :: out

     call ensure_(tonto,associated(self%atom),"MOL:put_vrml ... no atom list!")
     call create_(out,trim(self%name)//achar(46)//"wrl")
     call open_(out,for="write")
     call text_(stdout,"Generating VRML molecule")
     call text_(out,"#VRML V2.0 utf8")
     call text_(out,"NavigationInfo { type " // achar(34) // "EXAMINE" // achar(34) // " }")
     call text_(out,"Viewpoint { ")
     call text_(out,"position 0 0 10")
     call text_(out,"fieldOfView 1")
     call text_(out,"orientation 0 0 1 0")
     call text_(out,'description "camera z"')
     call text_(out,"}")
     call text_(out,"DirectionalLight {")
     call text_(out,"  color 1 1 1")
     call text_(out,"  direction 1 0 0")
     call text_(out,"  intensity 0.4")
     call text_(out,"}")
     call put_vrml_(self%atom,out)
     if (associated(self%isosurface)) call put_vrml_(self%isosurface,out)
     call text_(stdout,"done VRML molecule")

   end subroutine

   subroutine put_1e_properties(self)
    type(mol_type) :: self
    ! Put all the available one elctron properties

   call ensure_(tonto,associated(self%density_matrix),"MOL:put_1e_properties ... no density")
   call ensure_(tonto,associated(self%atom),"MOL:put_1e_properties ... no atoms")
   call ensure_(tonto,associated(self%basis),"MOL:put_1e_properties ... no basis sets specified")
      call put_dipole_(self)
      call put_quadrupole_(self)
      call put_octupole_(self)
      call put_E_field_at_nuclei_(self)
      call put_EFG_at_nuclei_(self)

   end subroutine

   subroutine put_dipole(self)
    type(mol_type) :: self
    ! Put out the dipole to stdout
      real(kind=kind(1.0d0)), dimension(3) :: electronic,nuclear,total
      character(128), dimension(3) :: axis
      integer(kind=kind(1)) :: i

      axis = (/ "x", "y", "z" /)
      electronic = electronic_dipole_moment_(self)
      nuclear    = nuclear_dipole_moment_(self)
      total      = electronic + nuclear
      call flush_(stdout)
      call text_(stdout,"Dipole moments:")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=6)
      call tab_(stdout,int_fields=1)
      call put_(stdout,"Electronic")
      call put_(stdout,"Nuclear")
      call put_(stdout,"Total")
      call put_(stdout,"Electronic")
      call put_(stdout,"Nuclear")
      call put_(stdout,"Total")
      call flush_(stdout)
      call put_(stdout,"Component",int_width=.true.)
      call put_(stdout,"/au")
      call put_(stdout,"/au")
      call put_(stdout,"/au")
      call put_(stdout,"/Debye")
      call put_(stdout,"/Debye")
      call put_(stdout,"/Debye")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=6)
      do i = 1,3
         call put_(stdout,axis(i),int_width=.true.)
         call put_(stdout,electronic(i))
         call put_(stdout,nuclear(i))
         call put_(stdout,total(i))
         call put_(stdout,to_units_(electronic(i),"debye"))
         call put_(stdout,to_units_(nuclear(i),"debye"))
         call put_(stdout,to_units_(total(i),"debye"))
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=1,real_fields=6)

   end subroutine

   subroutine put_quadrupole(self)
    type(mol_type) :: self
    ! Put out the quadrupole to stdout
      real(kind=kind(1.0d0)), dimension(6) :: electronic,nuclear,total
      character(128), dimension(6) :: axis
      integer(kind=kind(1)) :: i

      axis = (/ "xx","yy","zz","xy","xz","yz" /)
      electronic = electronic_quadrupole_moment_(self)
      nuclear    = nuclear_quadrupole_moment_(self)
      total      = electronic + nuclear
      call flush_(stdout)
      call text_(stdout,"Quadrupole moments:")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=6)
      call tab_(stdout,int_fields=1)
      call put_(stdout,"Electronic")
      call put_(stdout,"Nuclear")
      call put_(stdout,"Total")
      call put_(stdout,"Electronic")
      call put_(stdout,"Nuclear")
      call put_(stdout,"Total")
      call flush_(stdout)
      call put_(stdout,"Component",int_width=.true.)
      call put_(stdout,"/au")
      call put_(stdout,"/au")
      call put_(stdout,"/au")
      call put_(stdout,"/Debye-A")
      call put_(stdout,"/Debye-A")
      call put_(stdout,"/Debye-A")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=6)
      do i = 1,6
         call put_(stdout,axis(i),int_width=.true.)
         call put_(stdout,electronic(i))
         call put_(stdout,nuclear(i))
         call put_(stdout,total(i))
         call put_(stdout,to_units_(electronic(i),"debye-angstrom"))
         call put_(stdout,to_units_(nuclear(i),"debye-angstrom"))
         call put_(stdout,to_units_(total(i),"debye-angstrom"))
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=1,real_fields=6)

   end subroutine

   subroutine put_octupole(self)
    type(mol_type) :: self
    ! Put out the octupole to stdout
      real(kind=kind(1.0d0)), dimension(10) :: electronic,nuclear,total
      character(128), dimension(10) :: axis
      integer(kind=kind(1)) :: i

      axis = (/ "xxx","yyy","zzz","xxy","xxz","yyx","yyz","zzx","zzy","xyz" /)
      electronic = electronic_octupole_moment_(self)
      nuclear    = nuclear_octupole_moment_(self)
      total      = electronic + nuclear
      call flush_(stdout)
      call text_(stdout,"Octupole moments:")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=6)
      call tab_(stdout,int_fields=1)
      call put_(stdout,"Electronic")
      call put_(stdout,"Nuclear")
      call put_(stdout,"Total")
      call put_(stdout,"Electronic")
      call put_(stdout,"Nuclear")
      call put_(stdout,"Total")
      call flush_(stdout)
      call put_(stdout,"Component",int_width=.true.)
      call put_(stdout,"/au")
      call put_(stdout,"/au")
      call put_(stdout,"/au")
      call put_(stdout,"/Debye-A2")
      call put_(stdout,"/Debye-A2")
      call put_(stdout,"/Debye-A2")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=6)
      do i = 1,10
         call put_(stdout,axis(i),int_width=.true.)
         call put_(stdout,electronic(i))
         call put_(stdout,nuclear(i))
         call put_(stdout,total(i))
         call put_(stdout,to_units_(electronic(i),"debye-angstrom^2"))
         call put_(stdout,to_units_(nuclear(i),"debye-angstrom^2"))
         call put_(stdout,to_units_(total(i),"debye-angstrom^2"))
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=1,real_fields=6)

   end subroutine

   subroutine put_E_field_at_nuclei(self)
    type(mol_type) :: self
    ! Put out the electric field at every nuclear position
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: electronic,nuclear,total
      character(128), dimension(3) :: axis
      character(128), dimension(:), pointer :: symbol
      integer(kind=kind(1)) :: a,i

      axis = (/ "x", "y", "z" /)
      symbol => numbered_chemical_symbols_(self)
      call create_(electronic,3,self%n_atom)
      call create_(nuclear,3,self%n_atom)
      call create_(total,3,self%n_atom)
      electronic = electronic_E_field_at_nuclei_(self)
      nuclear    = nuclear_E_field_at_nuclei_(self)
      total      = electronic + nuclear
      call flush_(stdout)
      call text_(stdout,"Electric fields at nuclei:")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=4)
      call tab_(stdout,real_fields=1,int_fields=1)
      call put_(stdout,"Electronic")
      call put_(stdout,"Nuclear")
      call put_(stdout,"Total")
      call flush_(stdout)
      call put_(stdout,"Atom")
      call put_(stdout,"E_i",int_width=.true.)
      call put_(stdout,"/au")
      call put_(stdout,"/au")
      call put_(stdout,"/au")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=4)
      do a = 1,self%n_atom
      do i = 1,3
         if (i==1) then; call put_(stdout,symbol(a))
         else;           call tab_(stdout,real_fields=1)
         end if
         call put_(stdout,axis(i),int_width=.true.)
         call put_(stdout,electronic(i,a))
         call put_(stdout,nuclear(i,a))
         call put_(stdout,total(i,a))
         call flush_(stdout)
      end do
      end do
      call dash_(stdout,int_fields=1,real_fields=4)
      call destroy_(total)
      call destroy_(nuclear)
      call destroy_(electronic)
      call destroy_(symbol)

   end subroutine

   subroutine put_EFG_at_nuclei(self)
    type(mol_type) :: self
    ! Put out the electric field gradient at every nuclear position
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: electronic,nuclear,total
      character(128), dimension(6) :: axis
      character(128), dimension(:), pointer :: symbol
      integer(kind=kind(1)) :: a,i

      axis = (/ "xx","yy","zz","xy","xz","yz" /)
      symbol => numbered_chemical_symbols_(self)
      call create_(electronic,6,self%n_atom)
      call create_(nuclear,6,self%n_atom)
      call create_(total,6,self%n_atom)
      electronic = electronic_EFG_at_nuclei_(self)
      nuclear    = nuclear_EFG_at_nuclei_(self)
      total      = electronic + nuclear
      call flush_(stdout)
      call text_(stdout,"Electric field gradient at nuclei:")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=4)
      call tab_(stdout,real_fields=1,int_fields=1)
      call put_(stdout,"Electronic")
      call put_(stdout,"Nuclear")
      call put_(stdout,"Total")
      call flush_(stdout)
      call put_(stdout,"Atom")
      call put_(stdout,"E_ij",int_width=.true.)
      call put_(stdout,"/au")
      call put_(stdout,"/au")
      call put_(stdout,"/au")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=4)
      do a = 1,self%n_atom
      do i = 1,6
         if (i==1) then; call put_(stdout,symbol(a))
         else;           call tab_(stdout,real_fields=1)
         end if
         call put_(stdout,axis(i),int_width=.true.)
         call put_(stdout,electronic(i,a))
         call put_(stdout,nuclear(i,a))
         call put_(stdout,total(i,a))
         call flush_(stdout)
      end do
      end do
      call dash_(stdout,int_fields=1,real_fields=4)
      call destroy_(total)
      call destroy_(nuclear)
      call destroy_(electronic)
      call destroy_(symbol)

   end subroutine

   function numbered_chemical_symbols(self) result(res)
    type(mol_type) :: self
    ! Return a list of numbered chemical symbols
      character(128), dimension(:), pointer :: res

   call ensure_(tonto,associated(self%atom),"MOL:numbered_chemical_symbols ... no atom list")
      call create_(res,size(self%atom))
      res = numbered_chemical_symbols_(self%atom(:))

   end function

!  ***********
!  SCF methods
!  ***********

   subroutine set_scf_defaults(self)
    type(mol_type) :: self
    ! Set up scf defaults for molecule

      if (.not. associated(self%scfdata)) then
        call create_(self%scfdata)
      else
        call set_defaults_(self%scfdata)
      end if
      if (self%mult==1) self%scfdata%scf_kind = "rhf"
      if (self%mult/=1) self%scfdata%scf_kind = "uhf"
      self%scfdata%using_fock_diis = .true.
      self%scfdata%using_MO_diis = .true.
      self%scfdata%direct = .false.
      call set_archive_root_name_(self%scfdata%diis,self%name)

   end subroutine

   subroutine make_molecule_from_atom(self,n,mol)
    type(mol_type) :: self
    ! Make a fully fledged molecule "mol" from a single atom "n" in self
    ! The new molecule is ready for an SCF calculation.
       integer(kind=kind(1)) :: n
      type(mol_type) :: mol

   call ensure_(tonto,n<=self%n_atom,"MOL:make_molecule_from_atom ... atom number n too big")
      call nullify_ptr_part_(mol)
      call create_(mol%atom,1)
      mol%atom = self%atom(n)    ! WARNING: dont destroy ptr parts of mol.atom !
      mol%atom(1)%group = 0  ! does not belong to a group
      mol%atom(1)%pos = (/0.0d0,0.0d0,0.0d0/)
      call set_defaults_(mol)
      mol%name = trim(chemical_name_(self%atom(n)))
      mol%basis => self%basis  ! WARNING: dont destroy basis of mol !
      mol%slaterbasis => self%slaterbasis
      mol%coppensbasis => self%coppensbasis
      call set_basis_info_(mol)
      call set_scf_defaults_(mol)

   end subroutine

   subroutine make_molecule_from_atom_group(self,n,mol)
    type(mol_type) :: self
    ! Make a fully fledged molecule "mol" from group "n" of atoms
    ! specified in .atom_group(n).element(:), ready fro an SCF calculation.
      integer(kind=kind(1)) :: n
      type(mol_type) :: mol

      call ensure_(tonto,associated(self%atom_group),"MOL:make_molecule_from_atom_group ... no atom_group info")
      call ensure_(tonto,n<=size(self%atom_group),"MOL:make_molecule_from_atom_group ... group number n too big")
      call nullify_ptr_part_(mol)
      call create_(mol%atom,size(self%atom_group(n)%element))
      mol%atom = self%atom(self%atom_group(n)%element)  ! WARNING: dont destroy ptr parts of mol.atom !
      call set_defaults_(mol)
      if (associated(self%group_charges)) mol%charge = self%group_charges(n)
      mol%mult = default_multiplicity_(mol)
      call set_atom_info_(mol)
      mol%name  =  trim(self%name) // "_group_" // trim(to_str_(n))
      mol%basis => self%basis  ! WARNING: dont destroy basis of mol !
      mol%slaterbasis => self%slaterbasis
      mol%coppensbasis => self%coppensbasis
      call set_basis_info_(mol)
      call set_scf_defaults_(mol)

   end subroutine

!  ************
!  SCF routines
!  ************

   recursive subroutine scf(self)
    type(mol_type) :: self
    ! Do an SCF calculation. The following :
    !   .molecular_orbitals, .orbital_energies, .density_matrix
    ! are produced as results.

   call ensure_(tonto,associated(self%scfdata),"MOL:scf ... no scfdata provided")
     select case (self%scfdata%scf_kind)
       case ("xray_rhf","xray_rks","xray_rdft","xray_udft")
         call constrained_scf_(self)
       case default;
         call usual_scf_(self)
     end select

   end subroutine

   recursive subroutine usual_scf(self)
    type(mol_type) :: self
    ! Do an SCF calculation. The following :
    !   .molecular_orbitals, .orbital_energies, .density_matrix
    ! are produced as results.
     character(128) :: scf_kind

     call ensure_(tonto,self%basis_info_made,"MOL:usual_scf ... no basis info")
     call ensure_(tonto,associated(self%atom),"MOL:usual_scf ... no atom list")
     scf_kind = self%scfdata%scf_kind
     if (scf_kind == "dft" .and. .not. associated(self%dftgrid)) then
       call create_(self%dftgrid)
       call set_defaults_(self%dftgrid)
     end if
     call get_initial_guess_(self)
     call make_fock_matrix_(self)
     call initialise_scfdata_(self)
     call initialise_scfdata_error_(self)
!     .set_scfdata_dft_energy_correction
     call put_scf_results_(self)
     do
       if (scf_done_(self%scfdata)) exit
       call extrapolate_fock_matrix_(self)
       call update_molecular_orbitals_(self)
       call schmidt_orthonormalise_MOs_(self)
       call make_scf_density_matrix_(self)
       call make_fock_matrix_(self)
       call update_scfdata_(self)
       call update_scfdata_error_(self)
!       .set_scfdata_dft_energy_correction
       call put_scf_results_(self)
     end do
     call archive_scf_results_(self)
     call cleanup_scf_(self)

   end subroutine

   subroutine make_integration_grid(self)
    type(mol_type) :: self
    ! Makes an integration grid for numerical integration.
    ! Note that the routine "dftgrid.make_grid" archives the grid.
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: pt
     real(kind=kind(1.0d0)), dimension(:), pointer :: wt
     integer(kind=kind(1)) :: n_pts

     n_pts = self%dftgrid%n_pts*self%n_atom
     call create_(pt,n_pts,3)
     call create_(wt,n_pts)
     call make_grid_(self%dftgrid,pt,wt,self%atom)
     call destroy_(pt)
     call destroy_(wt)

   end subroutine

   subroutine make_grid_densities(self)
    type(mol_type) :: self
    ! Make various "time-consuming" grid densities and archive them.
    ! Densities made in this routine:
    ! 1. density  (= rho)
    ! 2. grad of density (= nabla_rho)
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: pt, nabla_rho
     real(kind=kind(1.0d0)), dimension(:), pointer :: wt, rho
     integer(kind=kind(1)) :: n_pts
     logical(kind=kind(.true.)) :: old_NOs
     type(archive_type) :: archive

     old_NOs = .false.
     if (associated(self%natural_orbitals)) old_NOs = any_created_(self%natural_orbitals)
     if (.not. old_NOs) call make_natural_orbitals_(self)
     n_pts = self%dftgrid%n_pts*self%n_atom
     call create_(pt,n_pts,3)
     call create_(wt,n_pts)
     call create_(rho,n_pts)
     call create_(nabla_rho,n_pts,3)
     call make_grid_(self%dftgrid,pt,wt,self%atom)
     call make_density_grid_(self,rho,pt)
     call make_nabla_density_grid_(self,nabla_rho,pt)
     call set_(archive,self%name, "DENSITY_grid")
     call write_(archive,rho)
     call set_(archive,self%name, "NABLA_DENSITY_grid")
     call write_(archive,nabla_rho)
     call destroy_(pt)
     call destroy_(wt)
     call destroy_(rho)
     call destroy_(nabla_rho)
      if (.not. old_NOs) call destroy_ptr_part_(self%natural_orbitals)

   end subroutine

   subroutine put_scf_results(self)
    type(mol_type) :: self
    ! Put out the SCF results

     if (self%scfdata%iteration==0 .and. self%scfdata%lambda_iteration==0) then
        call put_banner_(self%scfdata)
        if (fitting_(self%scfdata)) call put_correction_data_(self%crystal)
     end if
     call put_results_(self%scfdata)

   end subroutine

   subroutine initialise_scfdata(self)
    type(mol_type) :: self
    ! Initialise the scf data, including iteration counters
     real(kind=kind(1.0d0)) :: E_nuc,E_HF,E_K,dft_energy_correction

     E_nuc = nuclear_energy_(self)
     E_HF  = scf_energy_(self)
     E_K   = kinetic_energy_(self)
     if (includes_(self%scfdata%scf_kind,"dft")) then
       dft_energy_correction = dft_energy_correction_(self)
       call reset_(self%scfdata,nuclear_energy=E_nuc,energy=E_HF,kinetic_energy=E_K, &
                dft_energy_correction=dft_energy_correction,crystal=self%crystal)
     else
       call reset_(self%scfdata,nuclear_energy=E_nuc,energy=E_HF,kinetic_energy=E_K, &
                dft_energy_correction=0.0d0,crystal=self%crystal)
     end if

   end subroutine

   subroutine update_scfdata(self)
    type(mol_type) :: self
    ! Update the scf iteration count, scf energy and property data. The energy
    ! differences are used to test for convergence in the SCF procedure.
     real(kind=kind(1.0d0)) :: E_HF,E_K,dft_energy_correction

     E_HF = scf_energy_(self)
     E_K  = kinetic_energy_(self)
     if (includes_(self%scfdata%scf_kind,"dft")) then
       dft_energy_correction = dft_energy_correction_(self)
       call update_(self%scfdata,energy=E_HF,kinetic_energy=E_K, &
                dft_energy_correction=dft_energy_correction,crystal=self%crystal)
     else
       call update_(self%scfdata,energy=E_HF,kinetic_energy=E_K, &
                dft_energy_correction=0.0d0,crystal=self%crystal)
     end if

   end subroutine

!   set_scfdata_dft_energy_correction
!   ! Set the scfdata dft energy correction to the SCF energy.  It is not an
!   ! error to call this routine without the scf being for dft.
!     dft_energy_correction :: real(kind=kind(1.0d0))
!     if (.scfdata.scf_kind.includes("dft")) then
!       dft_energy_correction = .dft_energy_correction
!       .scfdata.set(dft_energy_correction=dft_energy_correction)
!     end
!   end

   subroutine initialise_scfdata_error(self)
    type(mol_type) :: self
    ! Reset the scfdata gradient of the SCF energy with respect to orbital
    ! rotations, otherwise known as the "diis error". This is used to check for
    ! SCF convergence.
     real(kind=kind(1.0d0)) :: diis_error

     call make_diis_error_(self,diis_error)
     call set_diis_error_(self%scfdata,diis_error)

   end subroutine

   subroutine update_scfdata_error(self)
    type(mol_type) :: self
    ! Update the scfdata gradient of the SCF energy with respect to orbital
    ! rotations, otherwise known as the "diis error". This is used to check for
    ! SCF convergence.
     real(kind=kind(1.0d0)) :: diis_error

     call make_diis_error_(self,diis_error)
     call set_diis_error_(self%scfdata,diis_error)

   end subroutine

!  ********************************
!  Orbital update/cleaning routines
!  ********************************

   subroutine schmidt_orthonormalise_MOs(self)
    type(mol_type) :: self
    ! Schmidt orthonormalise the ".molecular_orbitals" and archive

      call schmidt_orthonormalise_(self,self%molecular_orbitals)
      if (.not. self%minimal_io) call archive_molecular_orbitals_(self)

   end subroutine

   subroutine schmidt_orthonormalise(self,MO,scale)
    type(mol_type) :: self
    ! Schmidt orthonormalise the molecular orbitals, "MO"
      type(opmatrix_type) :: MO
      real(kind=kind(1.0d0)), optional :: scale
       real(kind=kind(1.0d0)), dimension(:,:), pointer :: S

      call create_(S,self%n_bf,self%n_bf)
      call get_overlap_matrix_(self,S)
      call schmidt_orthonormalise_(MO,S,scale)
      call destroy_(S)

   end subroutine

   subroutine symmetrically_orthonormalise(self,MO)
    type(mol_type) :: self
    ! Symmetrically orthonormalise the molecular orbitals, "MO"
      type(opmatrix_type) :: MO
       real(kind=kind(1.0d0)), dimension(:,:), pointer :: S

      call create_(S,self%n_bf,self%n_bf)
      call get_overlap_matrix_(self,S)
      call symmetrically_orthonormalise_(MO,S)
      call destroy_(S)

   end subroutine

   subroutine update_molecular_orbitals(self)
    type(mol_type) :: self
    ! Solve for the molecular orbitals.  Requires a set of old molecular
    ! orbitals.

     if (self%scfdata%using_MO_gradient_update) then; call MO_gradient_update_(self)
     else;                                        call MO_eigen_update_(self)
     end if

   end subroutine

   subroutine MO_eigen_update(self)
    type(mol_type) :: self
    ! Solve for the molecular orbitals.  Requires a set of old molecular
    ! orbitals.
    ! The new molecular orbitals "cU" are found from the old MOs "c" by solving :
    ! (c^T F c)U = c^T S c U 2.718281828459045d0 ... where U is an orthogonal matrix, F fock
    ! matrix.
     character(128) :: orb_kind
     real(kind=kind(1.0d0)), dimension(:), pointer :: e
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: MO,F
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: MOc,Fc

     call ensure_(tonto,associated(self%molecular_orbitals),"MOL:MO_eigen_update ... no old MO's")
     call ensure_(tonto,any_created_(self%molecular_orbitals),"MOL:MO_eigen_update ... no old MO's")
!     .archive_molecular_orbitals(archive_name="old_molecular_orbitals")
     orb_kind = self%scfdata%scf_kind
     select case (orb_kind)
        case ("rhf","rdft","restricted_hartree_fock", &
              "rohf","restricted_open_shell_hartree_fock", &
              "xray_rhf","xray_rks","xray_rdft", &
              "noninteracting-group-rhf")
           e   => self%orbital_energies%restricted
           MO  => self%molecular_orbitals%restricted
           F   => self%fock_matrix%restricted
           call MO_r_eigen_update_(self,e,MO,F)
        case ("uhf","udft","xray_udft","unrestricted_hartree_fock")
           e   => self%orbital_energies%alpha
           MO  => self%molecular_orbitals%alpha
           F   => self%fock_matrix%alpha
           call MO_r_eigen_update_(self,e,MO,F)
           e   => self%orbital_energies%beta
           MO  => self%molecular_orbitals%beta
           F   => self%fock_matrix%beta
           call MO_r_eigen_update_(self,e,MO,F)
        case ("gchf","cghf","general_complex_hartree_fock")
           e   => self%orbital_energies%general
           MOc => self%molecular_orbitals%general_complex
           Fc  => self%fock_matrix%general_complex
           call MO_gc_eigen_update_(self,e,MOc,Fc)
     end select
     if (.not. self%minimal_io) call archive_molecular_orbitals_(self)

   end subroutine

   subroutine MO_r_eigen_update(self,MO_energies,MO,F)
    type(mol_type) :: self
    ! Solve for the new molecular orbital energies "MO_energies" and the new
    ! molecular orbitals "MO", given an initial old set (in "MO") and a fock
    ! matrix "F".  The new molecular orbitals "MO*U" are found from the old MOs
    ! "MO" by solving : (c^T F c)U = c^T S c U 2.718281828459045d0 ...... where U is an orthogonal
    ! matrix.
     real(kind=kind(1.0d0)), dimension(:) :: MO_energies
     real(kind=kind(1.0d0)), dimension(:,:) :: MO,F
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: G,U,X,S
      integer(kind=kind(1)) :: i

     call create_(G,self%n_bf,self%n_bf)
     G = F
  !    G.change_basis(MO)

     call create_(X,self%n_bf,self%n_bf)
     call create_(S,self%n_bf,self%n_bf)
     call get_overlap_matrix_(self,S)
     call to_inverse_sqrt_(X,S)
     call destroy_(S)

     call change_basis_(G,X)

      ! Level shifting
     if (apply_level_shifting_(self%scfdata)) then
       do i = self%n_a + 1, self%n_bf
         G(i,i) = self%scfdata%level_shift + G(i,i)
       end do
     end if

     call create_(U,self%n_bf,self%n_bf)
     call solve_eigenproblem_(G,MO_energies,U)
  !    G.to_product_of(MO,U)
     call to_product_of_(G,X,U)
     MO = G
     call destroy_(U)
     call destroy_(X)
     call destroy_(G)

   end subroutine

   subroutine MO_gc_eigen_update(self,MO_energies,MO,F)
    type(mol_type) :: self
    ! Solve for the new molecular orbital energies "MO_energies" and the new
    ! molecular orbitals "MO", given an initial old set (in "MO") and a fock
    ! matrix "F".
    ! The new molecular orbitals "MO*U" are found from the old MOs "MO" by
    ! solving (c^T F c)U = c^T S c U 2.718281828459045d0 ...... where U is an orthogonal matrix.
     real(kind=kind(1.0d0)), dimension(:) :: MO_energies
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: MO,F
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: G,U
      integer(kind=kind(1)) :: i

     call create_(G,2*self%n_bf,2*self%n_bf)
     call create_(U,2*self%n_bf,2*self%n_bf)
     G = F
     call change_basis_(G,MO)
      ! Level shifting
     if (apply_level_shifting_(self%scfdata)) then
       do i = self%n_e + 1, 2*self%n_bf
         G(i,i) = self%scfdata%level_shift + G(i,i)
       end do
     end if
     call solve_eigenproblem_(G,MO_energies,U)
     call to_product_of_(G,MO,U)
     MO = G
     call destroy_(U)
     call destroy_(G)

   end subroutine

   subroutine MO_gradient_update(self)
    type(mol_type) :: self
    ! Update the molecular orbitals using the gradient. Requires a set of old
    ! molecular orbitals. The new molecular orbitals "c" are found by
    ! incrementing a little along the gradient: (FDS - SDFDS)c and then
    ! reorthogonalising c.  NOTE: the normal Fock matrix type(diis_type) update is turned
    ! off if this routine executes.
     character(128) :: orb_kind
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: g,h,MO,F,P
     real(kind=kind(1.0d0)) :: f1,f2,scale
     real(kind=kind(1.0d0)) :: step = 1.0d0

   call ensure_(tonto,self%scfdata%using_MO_gradient_update,"MOL:MO_gradient_update ... not allowed")
   call ensure_(tonto,associated(self%fock_matrix),"MOL:MO_gradient_update ... no fock_matrix")
   call ensure_(tonto,any_created_(self%fock_matrix),"MOL:MO_gradient_update ... no fock_matrix")
   call ensure_(tonto,associated(self%density_matrix),"MOL:MO_gradient_update ... no density_matrix")
   call ensure_(tonto,any_created_(self%density_matrix),"MOL:MO_gradient_update ... no density_matrix")
   call ensure_(tonto,associated(self%molecular_orbitals),"MOL:MO_gradient_update ... no MO's")
   call ensure_(tonto,any_created_(self%molecular_orbitals),"MOL:MO_gradient_update ... no MO's")
!     .archive_molecular_orbitals(archive_name="old_molecular_orbitals")
     orb_kind = self%scfdata%scf_kind
     select case (orb_kind)
        case ("rhf","restricted_hartree_fock", &
              "rohf","restricted_open_shell_hartree_fock", &
              "xray_rhf","xray_rks","xray_rdft", &
              "noninteracting-group-rhf")
           call create_(g,self%n_bf,self%n_bf)
           F  => self%fock_matrix%restricted
           P  => self%density_matrix%restricted
           MO => self%molecular_orbitals%restricted
           call make_MO_r_gradient_(self,g,F,P,MO)
           f1 = trace_product_with_(g,transpose(g))
           MO = MO - (10.0d0**(-2)/sqrt(f1))*g
           call schmidt_orthonormalise_(self,self%molecular_orbitals,scale)
           scale = 1.0d0/scale
           call make_scf_density_matrix_(self)
           call make_fock_matrix_(self)
           call create_(h,self%n_bf,self%n_bf)
           call make_MO_r_gradient_(self,h,F,P,MO)
           h = (scale*h-g)/10.0d0**(-2)
           f2 = trace_product_with_(h,transpose(h))
           call destroy_(h)
           step = f1/f2
           step = min(self%scfdata%max_update_stepsize,step)
           write(*,*) "f1 = ",f1
           write(*,*) "f2 = ",f2
           write(*,*) "s  = ",step
           MO = MO - (step + 10.0d0**(-2)/sqrt(f1))*g
        !   if (.scfdata.apply_MO_diis) .scfdata.diis.extrapolate(MO(:,1:.n_a),g(:,1:.n_a))
        !   if (.not. .scfdata.diis_used) then ! only do this the first time
        !      if (.scfdata.difference>0.0d0) step = 0.70d0*step
        !      if (.scfdata.difference<0.0d0) step = 1.10d0*step
        !      MO = MO - step*(.scfdata.MO_gradient_stepsize)*g
        !   end
           call destroy_(g)
        case default
           call die_(tonto,"MOL:MO_gradient_update ... SCF kind "//trim(orb_kind)//" not implemented")
     end select
     call set_diis_error_(self%scfdata,f1)
     if (.not. self%minimal_io) call archive_molecular_orbitals_(self)

   end subroutine

   subroutine make_MO_r_gradient(self,g,F,P,c)
    type(mol_type) :: self
    ! Make the real gradient "g" of the moleculard orbitals,
    ! g = (FPS - (1/2)SPFPS)c
      real(kind=kind(1.0d0)), dimension(:,:) :: g,F,P,c
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: S,W

      call create_(S,self%n_bf,self%n_bf); call get_overlap_matrix_(self,S)
      call create_(W,self%n_bf,self%n_bf)
      call to_product_of_(W,P,S)
      call to_product_of_(S,F,W)
      g =       S  ! = FPS
      W = -0.50d0*W  ! = -PS ... 1/2 is for double occupancy in P
      call plus_product_of_(S,W,g,transpose_a=.true.)
      call destroy_(W)
      call to_product_of_(g,S,c)
      call destroy_(S)

   end subroutine

! **************************
! Energy evaluation routines
! **************************

   subroutine put_scf_energy(self)
    type(mol_type) :: self
    ! Output the scf energy.

     call show_(stdout,"The SCF energy is ", scf_energy_(self))
     call show_(stdout,"The kinetic energy is ", kinetic_energy_(self))
     if (includes_(self%scfdata%scf_kind,"dft")) then
       call show_(stdout,"The DFT energy is ", scf_energy_(self)+dft_energy_correction_(self))
     end if

   end subroutine

   function kinetic_energy(self) result(res)
    type(mol_type) :: self
    ! Evaluates the SCF kinetic energy as a trace of ".density_matrix" with the
    ! ".kinetic matrix".
     real(kind=kind(1.0d0)) :: res
     character(128) :: scf_kind
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: H
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: HH

   call ensure_(tonto,associated(self%density_matrix),"MOL:kinetic_energy ... no density matrix")
   call ensure_(tonto,any_created_(self%density_matrix),"MOL:kinetic_energy ... no density matrix")
     scf_kind = self%scfdata%scf_kind
     select case (scf_kind)
        case ("rhf","rdft","restricted_hartree_fock","xray_rhf","xray_rks", &
              "xray_rdft","noninteracting-group-rhf")
           call create_(H,self%n_bf,self%n_bf); call get_kinetic_matrix_(self,H)
           res = trace_of_product_(self%density_matrix%restricted,H)
           call destroy_(H)
        case ("uhf","udft","xray_udft","unrestricted_hartree_fock","rohf", &
              "restricted_open_shell_hartree_fock")
           call create_(H,self%n_bf,self%n_bf); call get_kinetic_matrix_(self,H)
           res = trace_of_product_(self%density_matrix%alpha,H)
           res = trace_of_product_(self%density_matrix%beta,H) + res
           call destroy_(H)
        case ("cghf","gchf","general_complex_hartree_fock")
           call create_(HH,2*self%n_bf,2*self%n_bf); HH=0.0d0
           call create_(H,self%n_bf,self%n_bf)
           call get_kinetic_matrix_(self,H)
           call alpha_alpha_set_to_(HH,H)
           call beta_beta_set_to_(HH,H)
           call destroy_(H)
           res = trace_of_product_(self%density_matrix%general_complex,HH)
           call destroy_(HH)
        case default; call die_(tonto,"MOL:kinetic_energy ... unknown scf kind, "//trim(scf_kind))
     end select

   end function

   function dft_energy_correction(self) result(res)
    type(mol_type) :: self
    ! Evaluates the total Kohn-Sham energy (including nuclear).
     real(kind=kind(1.0d0)) :: res
     character(128) :: orb_kind

     orb_kind = spinorbital_kind_(self%density_matrix)
     select case (orb_kind)
       case ("unrestricted")
         res = u_dft_energy_correction_(self)
       case ("restricted")
          !res = .ex_corr_energy_term
         res = r_dft_energy_correction_(self)
     end select

   end function

   function scf_energy(self) result(res)
    type(mol_type) :: self
    ! Evaluates the total SCF energy (including nuclear) as a trace of
    ! ".density_matrix" with the ".fock matrix".
     real(kind=kind(1.0d0)) :: res

   call ensure_(tonto,associated(self%density_matrix),"MOL:scf_energy ... no density matrix")
   call ensure_(tonto,any_created_(self%density_matrix),"MOL:scf_energy ... no density matrix")
   call ensure_(tonto,associated(self%fock_matrix),"MOL:scf_energy ... no fock matrix")
   call ensure_(tonto,any_created_(self%fock_matrix),"MOL:scf_energy ... no fock matrix")
     res = scf_energy_(self,self%density_matrix,self%fock_matrix)

   end function

   function scf_energy_1(self,P) result(res)
    type(mol_type) :: self
    ! Evaluates the total SCF energy (including nuclear) as a trace of
    ! "P" with the fock matrix ".fock_matrix".
     type(opmatrix_type) :: P
     real(kind=kind(1.0d0)) :: res

   call ensure_(tonto,associated(self%fock_matrix),"MOL:scf_energy_1 ... no fock matrix")
   call ensure_(tonto,any_created_(self%fock_matrix),"MOL:scf_energy_1 ... no fock matrix")
     res = scf_energy_(self,P,self%fock_matrix)

   end function

   function scf_energy_2(self,P,F) result(res)
    type(mol_type) :: self
    ! Evaluates the total SCF energy (including nuclear) as a trace of
    ! "P" with the fock matrix "F".
     type(opmatrix_type) :: P,F
     real(kind=kind(1.0d0)) :: res
     integer(kind=kind(1)) :: g
     character(128) :: scf_kind

     call ensure_(tonto,associated(self%scfdata),"MOL:scf_energy_2 ... no scf data")
     scf_kind = self%scfdata%scf_kind
     select case (scf_kind)
        case ("noninteracting-group-rhf")
           res = scf_electronic_energy_(self,P,F)
           do g = 1,size(self%atom_group)
              res = res + nuclear_energy_(self%atom(self%atom_group(g)%element))
           end do
        case default
           res = scf_electronic_energy_(self,P,F) + nuclear_energy_(self)
     end select

   end function

   function scf_electronic_energy(self,P,core) result(res)
    type(mol_type) :: self
    ! Evaluates the SCF electronic energy as a trace of density "P" with
    ! ".fock_matrix". If "core" is present and .false., the core contribution
    ! is not added.
     type(opmatrix_type) :: P
     logical(kind=kind(.true.)), optional :: core
     real(kind=kind(1.0d0)) :: res

     call ensure_(tonto,associated(self%fock_matrix),"MOL:scf_electronic_energy ... no fock matrix")
     call ensure_(tonto,any_created_(self%fock_matrix),"MOL:scf_electronic_energy ... no fock matrix")
     res = scf_electronic_energy_(self,P,self%fock_matrix,core)

   end function

   function scf_electronic_energy_1(self,P,F,core) result(res)
    type(mol_type) :: self
    ! Evaluates the SCF electronic energy as a trace of density "P" with
    ! fock matrix "F". If "core" is present and .false., the core contribution
    ! is not added.
     type(opmatrix_type) :: P,F
     logical(kind=kind(.true.)), optional :: core
     real(kind=kind(1.0d0)) :: res
     logical(kind=kind(.true.)) :: add_core
     character(128) :: scf_kind
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: Fr
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: Fc

     scf_kind = self%scfdata%scf_kind
     add_core = .true.
     if (present(core)) add_core = core
     select case (scf_kind)
        case ("rhf","restricted_hartree_fock", &
              "xray_rdft","rdft", &
              "xray_rhf","xray_rks", &
              "noninteracting-group-rhf")
           call create_(Fr,self%n_bf,self%n_bf)
           Fr = F%restricted
           if (add_core) call add_core_hamiltonian_(self,Fr)
           res = 0.50d0 * trace_of_product_(P%restricted,Fr)
           call destroy_(Fr)
!        case ("xray_rdft","rdft")
!           Fr.create(.n_bf,.n_bf)
!           Fr = F.restricted
!           if (add_core) .add_core_hamiltonian(Fr)
!           res = 0.50d0 * P.restricted.trace_of_product(Fr)
!           res = res + .ex_corr_energy_term
!           Fr.destroy
        case ("uhf","unrestricted_hartree_fock", &
              "xray_udft","udft", &
              "rohf","restricted_open_shell_hartree_fock", &
              "pnd_uhf")
           call create_(Fr,self%n_bf,self%n_bf);
           Fr = F%alpha
           if (add_core) call add_core_hamiltonian_(self,Fr)
           res = 0.50d0 * trace_of_product_(P%alpha,Fr)
           Fr = F%beta
           if (add_core) call add_core_hamiltonian_(self,Fr)
           res = 0.50d0 * trace_of_product_(P%beta,Fr) + res
           call destroy_(Fr)
!        case ("udft","xray_udft")
!           Fr.create(.n_bf,.n_bf);
!           Fr = F.alpha
!           if (add_core) .add_core_hamiltonian(Fr)
!           res = 0.50d0 * P.alpha.trace_of_product(Fr)
!           Fr = F.beta
!           if (add_core) .add_core_hamiltonian(Fr)
!           res = 0.50d0 * P.beta.trace_of_product(Fr) + res
!           Fr.destroy
!           res = res + .u_dft_energy_correction(P)
        case ("cghf","gchf","general_complex_hartree_fock","complex_general_hartree_fock")
           call create_(Fc,2*self%n_bf,2*self%n_bf);
           Fc = F%general_complex
           if (add_core) call add_core_hamiltonian_(self,Fc)
           res = 0.50d0 * trace_of_product_(P%general_complex,Fc)
           call destroy_(Fc)
        case default; call die_(tonto,"MOL:scf_electronic_energy_1 ... unknown scf kind, "//trim(scf_kind))
     end select

   end function

   function scf_electronic_energy_2(self,P,nuclei) result(res)
    type(mol_type) :: self
    ! Evaluates the SCF electronic energy as a trace of "D" with the
    ! ".fock_matrix".  "nuclei" lists the nuclei to be used for obtaining n->e
    ! attractions.
     type(opmatrix_type), target :: P
     integer(kind=kind(1)), dimension(:) :: nuclei
     real(kind=kind(1.0d0)) :: res
     logical(kind=kind(.true.)) :: add_core
     character(128) :: scf_kind
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: F
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: FF

     scf_kind = self%scfdata%scf_kind
     call ensure_(tonto,associated(self%fock_matrix),"MOL:scf_electronic_energy_2 ... no fock matrix")
     call ensure_(tonto,any_created_(self%fock_matrix),"MOL:scf_electronic_energy_2 ... no fock matrix")
     call ensure_(tonto,associated(self%density_matrix),"MOL:scf_electronic_energy_2 ... no density matrix")
     call ensure_(tonto,any_created_(self%density_matrix),"MOL:scf_electronic_energy_2 ... no density matrix")
     add_core = .true.
     select case (scf_kind)
        case ("rhf","restricted_hartree_fock", &
              "xray_rhf","xray_rks", &
              "xray_rdft","rdft", &
              "noninteracting-group-rhf")
           call create_(F,self%n_bf,self%n_bf)
           F = self%fock_matrix%restricted
           call add_core_hamiltonian_(self,F,nuclei)
           res = 0.50d0 * trace_of_product_(P%restricted,F)
           call destroy_(F)
!        case ("xray_rdft","rdft")
!           F.create(.n_bf,.n_bf)
!           F = .fock_matrix.restricted
!           .add_core_hamiltonian(F,nuclei)
!           res = 0.50d0 * P.restricted.trace_of_product(F)
!           res = res + .ex_corr_energy_term
!           F.destroy
        case ("uhf","udft","xray_udft","unrestricted_hartree_fock", &
              "rohf","restricted_open_shell_hartree_fock", &
              "pnd_uhf")
           call create_(F,self%n_bf,self%n_bf);
           F = self%fock_matrix%alpha
           call add_core_hamiltonian_(self,F,nuclei)
           res = 0.50d0 * trace_of_product_(P%alpha,F)
           F = self%fock_matrix%beta
           call add_core_hamiltonian_(self,F,nuclei)
           res = 0.50d0 * trace_of_product_(P%beta,F) + res
           call destroy_(F)
        case ("cghf","gchf","general_complex_hartree_fock","complex_general_hartree_fock")
           call create_(FF,2*self%n_bf,2*self%n_bf);
           FF = self%fock_matrix%general_complex
           call add_core_hamiltonian_(self,FF,nuclei)
           res = 0.50d0 * trace_of_product_(P%general_complex,FF)
           call destroy_(FF)
        case default; call die_(tonto,"MOL:scf_electronic_energy_2 ... unknown scf kind, "//trim(scf_kind))
     end select

   end function

   function expectation(self,X) result(res)
    type(mol_type) :: self
    ! Evaluates the expectation value of the matrix of the operator X
    ! with the current density matrix
     real(kind=kind(1.0d0)), dimension(:,:) :: X
     real(kind=kind(1.0d0)) :: res
     character(128) :: orb_kind

   call ensure_(tonto,is_square_(X),"MOL:expectation ... X operator matrix is not square")
     orb_kind = spinorbital_kind_(self%density_matrix)
     select case (orb_kind)
        case ("restricted")
           call ensure_(tonto,size(X,1)==self%n_bf,"MOL:expectation ... wrong size, X")
           res = trace_of_product_(self%density_matrix%restricted,X)
        case ("unrestricted")
           call ensure_(tonto,size(X,1)==self%n_bf,"MOL:expectation ... wrong size, X")
           res = trace_of_product_(self%density_matrix%alpha,X)
           res = trace_of_product_(self%density_matrix%beta,X) + res
        case ("general_complex")
           call ensure_(tonto,size(X,1)==2*self%n_bf,"MOL:expectation ... wrong size, X")
           res = trace_of_product_(self%density_matrix%general_complex,X)
        case default
           call die_(tonto,"MOL:expectation ... unimplemented kind, "//trim(orb_kind))
     end select

   end function

! ******************
! Archiving routines
! ******************

   subroutine archive_scf_results(self)
    type(mol_type) :: self
    ! Save the SCF results in various archives on disk

      if (.not. self%minimal_io) then
        call archive_density_matrix_(self)
        call archive_molecular_orbitals_(self)
        call archive_orbital_energies_(self)
        call archive_fock_matrix_(self)
      end if

   end subroutine

   subroutine archive_density_matrix(self,archive_name)
    type(mol_type) :: self
    ! Save the density matrix in an archive on disk
     character(*), optional :: archive_name
     character(128) :: name
     type(archive_type) :: archive

     name = "density_matrix"
     if (present(archive_name)) name = archive_name
     call set_(archive,self%name,name)
     call write_(archive,self%density_matrix)
     if (associated(self%scfdata)) then
     if (includes_(self%scfdata%scf_kind,"xray_")) then
        call set_(archive,self%name,trim(name)//",lambda="//trim(to_str_(self%scfdata%lambda,"f5.3")))
        call write_(archive,self%density_matrix)
     end if
     end if

   end subroutine

   subroutine archive_molecular_orbitals(self,archive_name)
    type(mol_type) :: self
    ! Save the molecular orbitals and energies in an archive on disk
     character(*), optional :: archive_name
     character(128) :: name
     type(archive_type) :: archive

     name = "molecular_orbitals"
     if (present(archive_name)) name = archive_name
     call set_(archive,self%name,name)
     call write_(archive,self%molecular_orbitals)
     if (associated(self%scfdata)) then
     if (includes_(self%scfdata%scf_kind,"xray_")) then
        call set_(archive,self%name,trim(name)//",lambda="//trim(to_str_(self%scfdata%lambda,"f5.3")))
        call write_(archive,self%molecular_orbitals)
     end if
     end if

   end subroutine

   subroutine archive_orbital_energies(self,archive_name)
    type(mol_type) :: self
    ! Save the orbitals energies in an archive on disk
     character(*), optional :: archive_name
     character(128) :: name
     type(archive_type) :: archive

     name = "orbital_energies"
     if (present(archive_name)) name = archive_name
     call set_(archive,self%name,name)
     call write_(archive,self%orbital_energies)

   end subroutine

   subroutine archive_fock_matrix(self,archive_name)
    type(mol_type) :: self
    ! Save the fock matrix in an archive on disk
     character(*), optional :: archive_name
     character(128) :: name
     type(archive_type) :: archive

     name = "fock_matrix"
     if (present(archive_name)) name = archive_name
     call set_(archive,self%name,name)
     call write_(archive,self%fock_matrix)

   end subroutine

   subroutine archive_natural_orbitals(self,archive_name)
    type(mol_type) :: self
    ! Save the natural_orbitals in an archive on disk
     character(*), optional :: archive_name
     character(128) :: name
     type(archive_type) :: archive

   call ensure_(tonto,associated(self%natural_orbitals),"MOL:archive_natural_orbitals ... no natural orbitals")
     name = "natural_orbitals"
     if (present(archive_name)) name = archive_name
     call set_(archive,self%name,name)
     call write_(archive,self%natural_orbitals)

   end subroutine

   subroutine archive_occupation_numbers(self,archive_name)
    type(mol_type) :: self
    ! Save the natural occupation numbers in an archive on disk
     character(*), optional :: archive_name
     character(128) :: name
     type(archive_type) :: archive

   call ensure_(tonto,associated(self%occupation_numbers),"MOL:archive_occupation_numbers ... no natural occupation numbers")
     name = "occupation_numbers"
     if (present(archive_name)) name = archive_name
     call set_(archive,self%name,name)
     call write_(archive,self%occupation_numbers)

   end subroutine

   subroutine get_scf_results(self)
    type(mol_type) :: self
    ! Get the SCF results in from archives. Do not get the density matrix,
    ! because it can/should be generated from the orbitals, depending on the kind
    ! of SCF.
     character(128) :: a_kind

     a_kind = molecular_orbital_kind_(self%scfdata)
     call unarchive_molecular_orbitals_(self,genre=a_kind)
     a_kind = orbital_energies_kind_(self%scfdata)
     call unarchive_orbital_energies_(self,genre=a_kind)

   end subroutine

   subroutine unarchive_density_matrix(self,archive_name,genre)
    type(mol_type) :: self
    ! Recover the density matrix from an archive on disk
     character(*), optional :: archive_name,genre
     character(128) :: name,a_kind
     type(archive_type) :: archive

     name = "density_matrix"
     if (present(archive_name)) name = archive_name
     if (present(genre)) then; a_kind = genre
     else;                            a_kind = spinorbital_kind_(self%density_matrix)
     end if
     call set_(archive,self%name,name,genre=a_kind)
     call read_(archive,self%density_matrix)

   end subroutine

   subroutine unarchive_molecular_orbitals(self,archive_name,genre)
    type(mol_type) :: self
    ! Save the molecular orbitals and energies in an archive on disk
     character(*), optional :: archive_name,genre
     character(128) :: name,a_kind
     type(archive_type) :: archive

     name = "molecular_orbitals"
     if (present(archive_name)) name = archive_name
     if (present(genre)) then; a_kind = genre
     else;                            a_kind = spinorbital_kind_(self%molecular_orbitals)
     end if
     call set_(archive,self%name,name,a_kind)
     call read_(archive,self%molecular_orbitals)

   end subroutine

   subroutine unarchive_orbital_energies(self,archive_name,genre)
    type(mol_type) :: self
    ! Save the orbitals energies in an archive on disk
     character(*), optional :: archive_name,genre
     character(128) :: name,a_kind
     type(archive_type) :: archive

     name = "orbital_energies"
     if (present(archive_name)) name = archive_name
     if (present(genre)) then; a_kind = genre
     else;                            a_kind = spinorbital_kind_(self%orbital_energies)
     end if
     call set_(archive,self%name,name,a_kind)
     call read_(archive,self%orbital_energies)

   end subroutine

   subroutine unarchive_fock_matrix(self,archive_name,genre)
    type(mol_type) :: self
    ! Save the fock matrix in an archive on disk
     character(*), optional :: archive_name,genre
     character(128) :: name,a_kind
     type(archive_type) :: archive

     name = "fock_matrix"
     if (present(archive_name)) name = archive_name
     if (present(genre)) then; a_kind = genre
     else;                            a_kind = spinorbital_kind_(self%fock_matrix)
     end if
     call set_(archive,self%name,name,a_kind)
     call read_(archive,self%fock_matrix)

   end subroutine

   subroutine unarchive_natural_orbitals(self,archive_name,genre)
    type(mol_type) :: self
    ! Recover the natural orbitals from an archive on disk
     character(*), optional :: archive_name,genre
     character(128) :: name,a_kind
     type(archive_type) :: archive

     name = "natural_orbitals"
     if (present(archive_name)) name = archive_name
     if (present(genre))          then; a_kind = genre
     else if (associated(self%natural_orbitals)) then; a_kind = spinorbital_kind_(self%natural_orbitals)
     else; call die_(tonto,"MOL:unarchive_natural_orbitals ... unknown archive kind")
     end if
     call set_(archive,self%name,name,a_kind)
     call read_(archive,self%natural_orbitals)

   end subroutine

   subroutine unarchive_occupation_numbers(self,archive_name,genre)
    type(mol_type) :: self
    ! Recover the natural orbital occupation numbers from an archive on disk
     character(*), optional :: archive_name,genre
     character(128) :: name,a_kind
     type(archive_type) :: archive

     name = "occupation_numbers"
     if (present(archive_name)) name = archive_name
     if (present(genre)) then; a_kind = genre
     else;                            a_kind = spinorbital_kind_(self%occupation_numbers)
     end if
     call set_(archive,self%name,name,a_kind)
     call read_(archive,self%occupation_numbers)

   end subroutine

   function archive_exists(self,archive_name,genre) result(res)
    type(mol_type) :: self
    ! Return .true. if the fock matrix exists
     character(*) :: archive_name,genre
     logical(kind=kind(.true.)) :: res
     type(archive_type) :: archive

     call set_(archive,self%name,archive_name,genre)
     res = exists_(archive)

   end function

   function archive_doesnt_exist(self,archive_name,genre) result(res)
    type(mol_type) :: self
    ! Return .true. if the fock matrix exists
     character(*) :: archive_name,genre
     logical(kind=kind(.true.)) :: res
     type(archive_type) :: archive

     call set_(archive,self%name,archive_name,genre)
     res = .not. exists_(archive)

   end function

! ********************
! SCF cleanup routines
! ********************

   subroutine cleanup_scf(self)
    type(mol_type) :: self
    ! Clean up the SCF files produced. NOTE: Does not delete integral files

     call cleanup_diis_(self%scfdata)
     call delete_old_scf_archives_(self)

   end subroutine

   subroutine destroy_scf_results(self)
    type(mol_type) :: self
    ! Destroy the SCF results in memory

     call destroy_(self%density_matrix)
     call create_(self%density_matrix,self%n_bf)
     call destroy_(self%molecular_orbitals)
     call create_(self%molecular_orbitals,self%n_bf)
     call destroy_(self%orbital_energies)
     call create_(self%orbital_energies,self%n_bf)

   end subroutine

   subroutine delete_scf_archives(self)
    type(mol_type) :: self
    ! Delete any matrices stored on disk.
      type(archive_type) :: archive

      call set_(archive,self%name,"density_matrix");     call delete_all_genres_(archive)
      call set_(archive,self%name,"molecular_orbitals"); call delete_all_genres_(archive)
      call set_(archive,self%name,"orbital_energies");   call delete_all_genres_(archive)
      call set_(archive,self%name,"fock_matrix");        call delete_all_genres_(archive)
      call set_(archive,self%name,"constraint_matrix");  call delete_all_genres_(archive)
      call delete_old_scf_archives_(self)

   end subroutine

   subroutine delete_scf_density_archive(self)
    type(mol_type) :: self
    ! Delete the SCF density matrix archives
      type(archive_type) :: archive

      call set_(archive,self%name,"molecular_orbitals"); call delete_all_genres_(archive)
      call set_(archive,self%name,"orbital_energies");   call delete_all_genres_(archive)

   end subroutine

   subroutine delete_scf_MO_archive(self)
    type(mol_type) :: self
    ! Delete the SCF MO archives
      type(archive_type) :: archive

      call set_(archive,self%name,"molecular_orbitals"); call delete_all_genres_(archive)
      call set_(archive,self%name,"orbital_energies");   call delete_all_genres_(archive)

   end subroutine

   subroutine delete_scf_fock_archive(self)
    type(mol_type) :: self
    ! Delete the SCF fock matrix archives
      type(archive_type) :: archive

      call set_(archive,self%name,"fock_matrix");        call delete_all_genres_(archive)

   end subroutine

   subroutine delete_old_scf_archives(self)
    type(mol_type) :: self
    ! Delete any matrices stored on disk.
      type(archive_type) :: archive

      call set_(archive,self%name,"old_density_matrix");     call delete_all_genres_(archive)
      call set_(archive,self%name,"old_molecular_orbitals"); call delete_all_genres_(archive)
      call set_(archive,self%name,"old_orbital_energies");   call delete_all_genres_(archive)

   end subroutine

   subroutine delete_scf_integrals(self)
    type(mol_type) :: self
    ! Delete all integral files. This is required whenever the geometry is
    ! changed.
      type(archive_type) :: archive

      call set_(archive,self%name,"overlap_matrix"); call delete_all_genres_(archive)
      call set_(archive,self%name,"kinetic_matrix"); call delete_all_genres_(archive)
      call set_(archive,self%name,"nuclear_matrix"); call delete_all_genres_(archive)
      call set_(archive,self%name,"core_matrix");    call delete_all_genres_(archive)
      call set_(archive,self%name,"fock_matrix");    call delete_all_genres_(archive)
      call set_(archive,self%name,"eri_integrals");  call delete_all_genres_(archive)
      call set_(archive,self%name,"eri_index");      call delete_all_genres_(archive)
      call set_(archive,self%name,"ft_ints");        call delete_all_genres_(archive)

   end subroutine

! *************************
! Canonicalisation routines
! *************************

   subroutine canonicalize_MOs(self)
    type(mol_type) :: self
    ! Generate the Fock matrix from the molecular orbitals and diagonalise it
    ! on the occupied-occupied block to get the canonical molecular orbitals.
     character(128) :: scf_kind

     call ensure_(tonto,associated(self%molecular_orbitals),"MOL:canonicalize_MOs ... need old MOs!")
     call ensure_(tonto,associated(self%orbital_energies),"MOL:canonicalize_MOs ... need old energies")
     call make_scf_density_matrix_(self)
     call make_fock_matrix_(self)
     scf_kind = self%scfdata%scf_kind
     select case (scf_kind)
        case ("rhf","dft","restricted_hartree_fock", &
              "rohf","restricted_open_shell_hartree_fock", &
              "xray_rhf","xray_rks","xray_rdft", &
              "noninteracting-group-rhf")
           call destroy_(self%orbital_energies)
           call create_(self%orbital_energies,self%n_bf,"restricted")
           call canonicalize_r_MO_(self,self%orbital_energies%restricted, &
                              self%molecular_orbitals%restricted, &
                              self%fock_matrix%restricted)
           call flush_(stdout)
           call text_(stdout,"Canonicalized molecular orbital energies:")
           call put_(stdout,self%orbital_energies%restricted(1:self%n_e/2), format="column")
        case default; call die_(tonto,"MOL:canonicalize_MOs ... unknown scf kind, "//trim(scf_kind))
     end select

   end subroutine

   subroutine canonicalize_r_MO(self,MO_energies,MO,F)
    type(mol_type) :: self
    ! Digaonalise the Fock matrix "F" in the occupied-occupied block of the
    ! molecular orbitals "MO", and reset the occupied "MO_energies".
     real(kind=kind(1.0d0)), dimension(:) :: MO_energies
     real(kind=kind(1.0d0)), dimension(:,:) :: MO,F
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: G,U,MO_old
     integer(kind=kind(1)) :: n

     n = self%n_e/2
     call create_(G,n,n)
     call create_(U,n,n)
     call create_(MO_old,self%n_bf,n)
     MO_old = MO(:,1:n)
     call change_basis_(F,G,MO_old)
     MO_energies = 0.0d0
     call solve_eigenproblem_(G,MO_energies,U)
     call to_product_of_(MO(:,1:n),MO_old,U)
     call destroy_(MO_old)
     call destroy_(U)
     call destroy_(G)

   end subroutine

! ******************************
! Initial orbital guess routines
! ******************************

   subroutine get_initial_guess(self)
    type(mol_type) :: self
    ! Get the initial guess for the ".density_matrix" and ".molecular_orbitals"
    ! If .scfdata.initial_mos is set, it overides .initial_density.
      character(128) :: initial_mos

      initial_mos = self%scfdata%initial_mos
      if (initial_mos/=" ") then; call get_initial_MOs_(self)
      else;                       call get_initial_density_(self)
      end if

   end subroutine

   subroutine get_initial_MOs(self)
    type(mol_type) :: self
    ! Get the initial guess for the .molecular_orbitals
    ! Convert the .molecular_orbitals, if neccesary.
      character(128) :: initial_mos

      initial_mos = self%scfdata%initial_mos
      select case (initial_mos)
         case("restricted          ") ; call read_old_MOs_guess_(self)
         case("unrestricted        ") ; call read_old_MOs_guess_(self)
         case("general             ") ; call read_old_MOs_guess_(self)
         case("restricted_complex  ") ; call read_old_MOs_guess_(self)
         case("complex_unrestricted") ; call read_old_MOs_guess_(self)
         case("unrestricted_complex") ; call read_old_MOs_guess_(self)
         case("general_complex     ") ; call read_old_MOs_guess_(self)
         case("complex_general     ") ; call read_old_MOs_guess_(self)
         case default;   allocate(tonto%known_keywords(8))
         tonto%known_keywords(1) = "restricted          "
         tonto%known_keywords(2) = "unrestricted        "
         tonto%known_keywords(3) = "general             "
         tonto%known_keywords(4) = "restricted_complex  "
         tonto%known_keywords(5) = "complex_unrestricted"
         tonto%known_keywords(6) = "unrestricted_complex"
         tonto%known_keywords(7) = "general_complex     "
         tonto%known_keywords(8) = "complex_general     "
         call unknown_(tonto,initial_mos,"MOL:get_initial_MOs")
         deallocate(tonto%known_keywords)
      end select

   end subroutine

   subroutine read_old_MOs_guess(self)
    type(mol_type) :: self
    ! Get the initial guess for the .molecular_orbitals by reading an old archive
    ! of .molecular_orbitals, and then make the SCF density matrix as well.
    ! Convert the .molecular_orbitals, if neccesary.
      type(archive_type) :: arch

      call set_(arch,self%name,"molecular_orbitals",genre=self%scfdata%initial_mos)
      call read_(arch,self%molecular_orbitals)
      call convert_to_(self%molecular_orbitals,molecular_orbital_kind_(self%scfdata),self%n_a,self%n_b, &
                                     self%scfdata%quantization_axis)
      call make_scf_density_matrix_(self)
      call create_(self%orbital_energies,opveckind=orbital_energies_kind_(self%scfdata))

   end subroutine

   subroutine make_atom_MOs_guess(self)
    type(mol_type) :: self
    ! Get the initial guess for the .molecular_orbitals by getting the highest
    ! occupied restricted natural orbitals from the sum of atomic densities guess
    ! for the density matrix. Convert these .molecular_orbitals, if neccesary.
    ! Also make the SCF .density_matrix from these .molecular_orbitals.
      character(128) :: orb_kind
      type(archive_type) :: arch

      call get_atom_density_(self)
      if (spinorbital_kind_(self%density_matrix)/="restricted") then
         call set_(arch,self%name,"atom_density_matrix", genre="restricted")
         call destroy_(self%density_matrix,"restricted")
         call read_(arch,self%density_matrix)
      end if
      call make_natural_orbitals_(self)
      call put_natural_orbitals_(self)
      call assign_MOs_to_NOs_(self)
      orb_kind = molecular_orbital_kind_(self%scfdata)
      call convert_to_(self%molecular_orbitals,orb_kind,self%n_a,self%n_b,self%scfdata%quantization_axis)
      call make_scf_density_matrix_(self)
      call create_(self%orbital_energies,opveckind=orbital_energies_kind_(self%scfdata))

   end subroutine

   subroutine get_initial_density(self)
    type(mol_type) :: self
    ! Get the initial guess for the ".density_matrix"
      character(128) :: initial_density

      initial_density = self%scfdata%initial_density
      select case (initial_density)
         case("core                ") ; call make_core_guess_(self)
         case("fock                ") ; call make_fock_guess_(self)
         case("atom                ") ; call make_atom_guess_(self)
         case("group               ") ; call make_group_guess_(self)
         case("restricted          ") ; call make_density_guess_(self)
         case("unrestricted        ") ; call make_density_guess_(self)
         case("general             ") ; call make_density_guess_(self)
         case("restricted_complex  ") ; call make_density_guess_(self)
         case("complex_unrestricted") ; call make_density_guess_(self)
         case("unrestricted_complex") ; call make_density_guess_(self)
         case("general_complex     ") ; call make_density_guess_(self)
         case("complex_general     ") ; call make_density_guess_(self)
         case default;  call die_(tonto,"MOL:get_initial_density ... unknown density, "//trim(initial_density))
      end select

   end subroutine

   subroutine make_density_guess(self)
    type(mol_type) :: self
    ! Read an old .density_matrix of type "initial_guess" and use it to
    ! make initial .molecular_orbitals to start an scf calculation.
    ! Convert the .density_matrix, if neccesary.
      character(128) :: initial_density
      type(archive_type) :: arch

      initial_density = self%scfdata%initial_density
      call set_(arch,self%name,"density_matrix")
      call read_(arch,self%density_matrix,genre=initial_density)
      call convert_to_(self%density_matrix,newkind = spinorbital_kind_(self%scfdata))
      call make_fock_matrix_(self)   ! Make a fock matrix
      call make_fock_guess_(self)    ! Get a reasonable set of starting MO's

   end subroutine

   subroutine make_core_guess(self)
    type(mol_type) :: self
    ! Do a core scf for the ".density_matrix", ".molecular_orbitals" and
    ! ".orbital_energies"
     character(128) :: orb_kind

     call destroy_scf_results_(self)
     call destroy_(self%orbital_energies,"restricted")
     call create_(self%orbital_energies,"restricted")
     call destroy_(self%molecular_orbitals,"restricted")
     call create_(self%molecular_orbitals,"restricted")
     call make_r_core_guess_(self,self%orbital_energies%restricted,self%molecular_orbitals%restricted)
     orb_kind = orbital_energies_kind_(self%scfdata)
     call convert_to_(self%orbital_energies,orb_kind)
     orb_kind = molecular_orbital_kind_(self%scfdata)
     call convert_to_(self%molecular_orbitals,orb_kind,self%n_a,self%n_b,self%scfdata%quantization_axis)
     call make_scf_density_matrix_(self)
     call make_fock_matrix_(self)   ! Make a fock matrix (only to get MO's)
     call make_fock_guess_(self)    ! Get a reasonable set of starting MO's

   end subroutine

   subroutine make_r_core_guess(self,MO_energies,MO)
    type(mol_type) :: self
    ! Do a real core scf for the molecular orbital energies  "MO_energies"
    ! and the molecular_orbitals "MO"
     real(kind=kind(1.0d0)), dimension(:) :: MO_energies
     real(kind=kind(1.0d0)), dimension(:,:) :: MO
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: S,R,H => NULL()
      !  H~ = S^-1/2 H S^-1/2
      !  c  = S^-1/2 c~

     call create_(S,self%n_bf,self%n_bf); call get_overlap_matrix_(self,S)
     call create_(R,self%n_bf,self%n_bf); call to_inverse_sqrt_(R,S)
     call create_(H,self%n_bf,self%n_bf); call get_core_matrix_(self,H)
     call to_product_of_(S,R,H)
     call to_product_of_(H,S,R)
     call solve_eigenproblem_(H,MO_energies,MO)
     call to_product_of_(H,R,MO)
     MO = H
     call destroy_(H)
     call destroy_(R)
     call destroy_(S)

   end subroutine

   subroutine make_atom_guess(self)
    type(mol_type) :: self
    ! Make a ".density matrix" and fock matrix from the sum of atomic densities.
    ! Then get initial orbitals from this fock matrix, and make the density
    ! corresponding to these initial orbitals.

      call ensure_(tonto,self%scfdata%scf_kind/="rohf","MOL:make_atom_guess ... atom guess not available for ROHF")
      call get_atom_density_(self)
      call make_fock_matrix_(self)        ! Make a fock matrix (only to get MO's)
      call make_fock_guess_(self)         ! Get a reasonable set of starting MO's

   end subroutine

   subroutine get_atom_density(self)
    type(mol_type) :: self
    ! Reads ".density_matrix" from the archive, otherwise makes it and
    ! writes to the arch.
     type(archive_type) :: arch
     logical(kind=kind(.true.)) :: write_archive,calc_it

     calc_it = self%scfdata%direct
     write_archive = .not. self%scfdata%direct
     if (.not. calc_it) then
       call set_(arch,self%name,"atom_density_matrix", genre="restricted")
       if (exists_(arch)) then
         call destroy_(self%density_matrix)
         call create_(self%density_matrix,self%n_bf,"restricted")
         call read_(arch,self%density_matrix)
         calc_it = .false.
         write_archive = .false.
       else
         calc_it = .true.
       end if
     end if
     if (calc_it) then
       call make_atom_density_(self)
     end if
     call convert_to_(self%density_matrix,spinorbital_kind_(self%scfdata),factor=0.50d0)
     if (write_archive) call write_(arch,self%density_matrix, genre="restricted")

   end subroutine

   subroutine make_atom_density(self)
    type(mol_type) :: self
    ! Make a ".density matrix" from the sum of atomic densities
    ! if output is present and .false. the density matrix is not archived
     integer(kind=kind(1)) :: k,l,kl,a,b,start,step
     type(mol_type), pointer :: mol
     logical(kind=kind(.true.)) :: old_do_parallel
     integer(kind=kind(1)), dimension(:), pointer :: first,last
     type(intvec__type), dimension(:), pointer :: atom_kind

     call destroy_(self%density_matrix)
     call create_(self%density_matrix,self%n_bf,"restricted")
     self%density_matrix%restricted = 0.0d0
     call make_atom_basis_fn_limits_(self%atom,first,last)
     call make_atom_kind_list_(self%atom,atom_kind)

     start = 1 + this_proc_(tonto_parallel)
     step = n_proc_(tonto_parallel)
     old_do_parallel = tonto_parallel%do_parallel
     tonto_parallel%do_parallel = .false.

     do k=start,size(atom_kind),step  ! Do an SCF for every different kind of atom
       call create_(mol)
       l = atom_kind(k)%element(1)
       call make_molecule_from_atom_(self,l,mol)
       if (no_of_electrons_(mol) >= 1) then
         mol%scfdata%convergence = 0.0001
         mol%scfdata%convergence = 0.0000001
         mol%scfdata%output = .false.
         mol%scfdata%using_fock_diis = .false.
         mol%scfdata%using_MO_diis = .false.
         mol%scfdata%using_rough_convergence = .false.
         mol%scfdata%direct = .true.
         call scf_(mol)
         call make_ao_density_matrix_(mol)
         call create_(mol%pointgroup,"oh")
         call symmetrise_(mol,mol%density_matrix)
         do l = 1,size(atom_kind(k)%element)  ! Copy this atom density to every
           kl = atom_kind(k)%element(l)       ! atom of this kind
           a = first(kl); b = last(kl)
          self%density_matrix%restricted(a:b,a:b) = &
          self%density_matrix%restricted(a:b,a:b) &
          + mol%density_matrix%restricted
         end do
       end if
       call nullify_ptr_part_(mol%atom)  ! do not destroy these
       nullify(mol%basis)         ! do not destroy this
       nullify(mol%slaterbasis)   ! do not destroy this
       nullify(mol%coppensbasis)  ! do not destroy this
       call delete_scf_integrals_(mol)
       call delete_scf_archives_(mol)
       call destroy_(mol)
     end do

     tonto_parallel%do_parallel = old_do_parallel
     call sum_symmetric_matrices_(tonto_parallel,self%density_matrix%restricted)

     call destroy_(atom_kind)
     call destroy_(last)
     call destroy_(first)

   end subroutine

   subroutine make_group_guess(self)
    type(mol_type) :: self
    ! Make a ".density matrix" and fock matrix from the sum of noninteracting
    ! atom_group densities. Then get initial orbitals from this fock matrix,
    ! and make the density corresponding to these initial orbitals.

      call ensure_(tonto,self%scfdata%scf_kind/="rohf","MOL:make_group_guess ... atom guess not available for ROHF")
      call get_group_density_(self)
      call make_fock_matrix_(self)        ! Make a fock matrix (only to get MO's)
      call make_fock_guess_(self)         ! Get a reasonable set of starting MO's

   end subroutine

   subroutine get_group_density(self)
    type(mol_type) :: self
    ! Reads ".density_matrix" from the archive, otherwise makes it and
    ! writes to the arch.
     type(archive_type) :: arch

     call set_(arch,self%name,"group_density_matrix", genre="restricted")
     if (exists_(arch)) then
        call destroy_(self%density_matrix)
        call create_(self%density_matrix,self%n_bf,"restricted")
        call read_(arch,self%density_matrix)
     else
        call make_group_density_(self)
     end if
     call convert_to_(self%density_matrix,spinorbital_kind_(self%scfdata),factor=0.50d0)

   end subroutine

   subroutine make_group_density(self,MOs,output)
    type(mol_type) :: self
    ! Make a ".density matrix" from the sum of atom_group densities.
    ! If "MOs" is present and true, make ".molecular_orbitals" which
    ! are the columns of orbitals from each group and also make the
    ! ".occupation_numbers" vector set to 2 for the occupied group MO's.
    ! If output is present and .false. the density matrix is not archived
     logical(kind=kind(.true.)), OPTIONAL :: MOs
     logical(kind=kind(.true.)), OPTIONAL :: output
     logical(kind=kind(.true.)) :: do_output,do_MOs
     integer(kind=kind(1)) :: g,n_g,a,b,aa,bb,faa,laa,fbb,lbb,fa,la,fb,lb, ms,mf, no
     type(mol_type), pointer :: mol
     type(archive_type) :: arch

   call ensure_(tonto,associated(self%atom_group),"MOL:make_group_density ... no atom_group info")
     do_output=.true.
     if (present(output)) do_output=output
     do_MOs=.false.
     if (present(MOs)) do_MOs=MOs
     call destroy_(self%density_matrix)
     call create_(self%density_matrix,self%n_bf,"restricted")
     self%density_matrix%restricted = 0.0d0
     if (do_MOs) then
     call destroy_(self%molecular_orbitals)
     call create_(self%molecular_orbitals,self%n_bf,"restricted")
     self%molecular_orbitals%restricted = 0.0d0
     call destroy_(self%occupation_numbers)
     call create_(self%occupation_numbers,self%n_bf,"restricted")
     self%occupation_numbers%restricted = 0.0d0
     end if
     n_g = size(self%atom_group)
     call create_(self%atom_group_energy,n_g)
     self%atom_group_energy = 0.0d0
     mf = 0
     do g = 1,n_g  ! Do an SCF for every atom group
       call create_(mol)
       call make_molecule_from_atom_group_(self,g,mol)
       if (no_of_electrons_(mol) >= 1) then
         mol%scfdata%convergence = 0.0001
         mol%scfdata%output = .true.
         mol%scfdata%direct = .true.
         if (size(self%atom_group(g)%element)>1) &
         mol%scfdata%initial_density = "atom"
         call scf_(mol)
         self%atom_group_energy(g) = mol%scfdata%energy
         call make_ao_density_matrix_(mol)
         do a = 1,size(self%atom_group(g)%element)  ! Copy this atom density
         do b = 1,size(self%atom_group(g)%element)
            aa  = self%atom_group(g)%element(a)
            bb  = self%atom_group(g)%element(b)
            faa = self%first_basis_fn_for_atom(aa)
            fbb = self%first_basis_fn_for_atom(bb)
            laa = self%last_basis_fn_for_atom(aa)
            lbb = self%last_basis_fn_for_atom(bb)
            fa  = mol%first_basis_fn_for_atom(a)
            fb  = mol%first_basis_fn_for_atom(b)
            la  = mol%last_basis_fn_for_atom(a)
            lb  = mol%last_basis_fn_for_atom(b)
            self%density_matrix%restricted(faa:laa,fbb:lbb) = &
            self%density_matrix%restricted(faa:laa,fbb:lbb) &
            + mol%density_matrix%restricted(fa:la,fb:lb)
         end do
         end do
         if (do_MOs) then
         ms = mf + 1
         mf = mf + mol%n_bf
         do a = 1,size(self%atom_group(g)%element)
            aa  = self%atom_group(g)%element(a)
            faa = self%first_basis_fn_for_atom(aa)
            laa = self%last_basis_fn_for_atom(aa)
            fa  = mol%first_basis_fn_for_atom(a)
            la  = mol%last_basis_fn_for_atom(a)
            self%molecular_orbitals%restricted(faa:laa,ms:mf) = &
            self%molecular_orbitals%restricted(faa:laa,ms:mf) &
            + mol%molecular_orbitals%restricted(fa:la,:)
         end do
         no = mol%n_e/2
         self%occupation_numbers%restricted(ms:ms+no-1) = 2.0d0
         end if
       end if
       call nullify_ptr_part_(mol%atom)  ! do not destroy these
       nullify(mol%basis)         ! do not destroy this
       nullify(mol%slaterbasis)   ! do not destroy this
       nullify(mol%coppensbasis)  ! do not destroy this
       call delete_scf_integrals_(mol)
      ! mol.delete_scf_archives   ! keep group density matrices, etc ...
       call delete_old_scf_archives_(mol)
       call destroy_(mol)
     end do
     if (do_output) then
       call set_(arch,self%name,"group_density_matrix", genre="restricted")
       call write_(arch,self%density_matrix, genre="restricted")
     end if
     if (do_output .and. do_MOs) then
       call set_(arch,self%name,"group_molecular_orbitals", genre="restricted")
       call write_(arch,self%molecular_orbitals, genre="restricted")
     end if

   end subroutine

   subroutine make_group_energies(self,g,E_SCF,E_T,E_Z,E_C,E_K,fac)
    type(mol_type) :: self
    ! Assuming group densities exist (see make_group_density), then for group
    ! number "g" make the group SCF energy "E_SCF", the group kinetic energy
    ! "E_T", the group nuclear attraction energy "E_Z", the group coulomb energy
    ! "E_C", and the group exchange energy "E_K". If "fac" is present, multiply
    ! all the energy values by this factor.
     integer(kind=kind(1)) :: g
     real(kind=kind(1.0d0)) :: E_SCF,E_T,E_Z,E_C,E_K
     real(kind=kind(1.0d0)), optional :: fac
     type(mol_type), pointer :: mol
     type(archive_type) :: archive
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: P,T,Z,C,K

   call ensure_(tonto,associated(self%atom_group),"MOL:make_group_energies ... no atom_group info")
     call create_(mol)
     call make_molecule_from_atom_group_(self,g,mol)
     E_SCF = 0.0d0
     E_T = 0.0d0
     E_Z = 0.0d0
     E_C = 0.0d0
     E_K = 0.0d0
     if (no_of_electrons_(mol) >= 1) then
        call set_(archive,self%name,"density_matrix","restricted")
        call create_(P,mol%n_bf,mol%n_bf)
        call read_(archive,P)
        E_SCF = self%atom_group_energy(g)
        call create_(T,mol%n_bf,mol%n_bf)
        call get_kinetic_matrix_(mol,T)
        E_T = trace_product_with_(T,P)
        call destroy_(T)
        call create_(Z,mol%n_bf,mol%n_bf)
        call get_nuclear_matrix_(mol,Z)
        E_Z = trace_product_with_(Z,P)
        call make_nuclear_matrix_(mol,Z)
        call destroy_(Z)
         !
        call create_(C,mol%n_bf,mol%n_bf)
        call create_(K,mol%n_bf,mol%n_bf)
        call make_r_JK_nosym_(mol,C,K,P)
        E_C = 0.50d0*trace_product_with_(C,P)
        E_K = -0.25d0*trace_product_with_(K,P)
        call destroy_(K)
        call destroy_(C)
        call destroy_(P)
     end if
     call nullify_ptr_part_(mol%atom)  ! do not destroy these
     nullify(mol%basis)         ! do not destroy this
     nullify(mol%slaterbasis)   ! do not destroy this
     nullify(mol%coppensbasis)  ! do not destroy this
     call delete_scf_integrals_(mol)
    ! mol.delete_scf_archives   ! keep group density matrices, etc ...
     call delete_old_scf_archives_(mol)
     call destroy_(mol)
     if (present(fac)) then
        E_SCF = fac*E_SCF
        E_T   = fac*E_T
        E_Z   = fac*E_Z
        E_C   = fac*E_C
        E_K   = fac*E_K
     end if

   end subroutine

   subroutine make_fock_guess(self)
    type(mol_type) :: self
    ! From an initial .fock_matrix make the .molecular_orbitals, and
    ! .orbital_energies.  Then make the .density_matrix from these orbitals.
    ! Destroy .fock_matrix afterwards
     character(128) :: orb_kind
     real(kind=kind(1.0d0)), dimension(:), pointer :: e
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: MO,F
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: MOc,Fc

     orb_kind = spinorbital_kind_(self%scfdata)
   call ensure_(tonto,created_(self%fock_matrix,orb_kind),"MOL:make_fock_guess ... need an initial fock matrix")
     call destroy_scf_results_(self)
     orb_kind = self%scfdata%scf_kind
     select case (orb_kind)
      !  F~ = S^-1/2 F S^-1/2
      !  c  = S^-1/2 c~
        case ("rhf","rdft","restricted_hartree_fock", &
              "rohf","restricted_open_shell_hartree_fock", &
              "xray_rhf","xray_rks","xray_rdft", &
              "noninteracting-group-rhf")
           call destroy_(self%orbital_energies,"restricted")
           call create_(self%orbital_energies,"restricted")
           call destroy_(self%molecular_orbitals,"restricted")
           call create_(self%molecular_orbitals,"restricted")
           e  => self%orbital_energies%restricted
           MO => self%molecular_orbitals%restricted
           F  => self%fock_matrix%restricted
           call make_r_fock_guess_(self,e,MO,F)
        case ("uhf","udft","xray_udft","unrestricted_hartree_fock")
           call destroy_(self%orbital_energies,"unrestricted")
           call create_(self%orbital_energies,"unrestricted")
           call destroy_(self%molecular_orbitals,"unrestricted")
           call create_(self%molecular_orbitals,"unrestricted")
           e  => self%orbital_energies%alpha
           MO => self%molecular_orbitals%alpha
           F  => self%fock_matrix%alpha
           call make_r_fock_guess_(self,e,MO,F)
           e  => self%orbital_energies%beta
           MO => self%molecular_orbitals%beta
           F  => self%fock_matrix%beta
           call make_r_fock_guess_(self,e,MO,F)
        case ("cghf","gchf","general_complex_hartree_fock","complex_general_hartree_fock")
           call destroy_(self%orbital_energies,"general")
           call create_(self%orbital_energies,"general")
           call destroy_(self%molecular_orbitals,"general_complex")
           call create_(self%molecular_orbitals,"general_complex")
           e   => self%orbital_energies%general
           MOc => self%molecular_orbitals%general_complex
           Fc  => self%fock_matrix%general_complex
           call make_gc_fock_guess_(self,e,MOc,Fc)
        case default;  call die_(tonto,"MOL:make_fock_guess ... unknown kind, "//trim(orb_kind))
     end select
     orb_kind = spinorbital_kind_(self%scfdata)
     call destroy_(self%fock_matrix,orb_kind)
     call make_scf_density_matrix_(self)

   end subroutine

   subroutine make_r_fock_guess(self,MO_energies,MO,fock_matrix)
    type(mol_type) :: self
    ! Make/guess the orbital energies "MO_energies" and orbitals "MO" from
    ! a given real restricted "fock_matrix".
     real(kind=kind(1.0d0)), dimension(:) :: MO_energies
     real(kind=kind(1.0d0)), dimension(:,:) :: MO, fock_matrix
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: S,R,H
      !  F~ = S^-1/2 F S^-1/2
      !  c  = S^-1/2 c~

     call create_(S,self%n_bf,self%n_bf); call get_overlap_matrix_(self,S)
     call create_(R,self%n_bf,self%n_bf); call to_inverse_sqrt_(R,S)
     call create_(H,self%n_bf,self%n_bf); H = fock_matrix
     call to_product_of_(S,R,H)
     call to_product_of_(H,S,R)
     call solve_eigenproblem_(H,MO_energies,MO)
     call to_product_of_(H,R,MO)
     MO = H
     call destroy_(H)
     call destroy_(R)
     call destroy_(S)

   end subroutine

   subroutine make_gc_fock_guess(self,MO_energies,MO,fock_matrix)
    type(mol_type) :: self
    ! Make/guess the orbital energies "MO_energies" and orbitals "MO" from
    ! a given complex general "fock_matrix".
     real(kind=kind(1.0d0)), dimension(:) :: MO_energies
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: MO, fock_matrix
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: S,R
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: H,W
      !  F~ = S^-1/2 F S^-1/2
      !  c  = S^-1/2 c~

     call create_(S,self%n_bf,self%n_bf)
     call create_(R,self%n_bf,self%n_bf)
     call get_overlap_matrix_(self,R)
     call to_inverse_sqrt_(S,R)
     call destroy_(R)
     call create_(R,2*self%n_bf,2*self%n_bf); R=0.0d0
     call alpha_alpha_set_to_(R,S)
     call beta_beta_set_to_(R,S)
     call destroy_(S)
     call create_(H,2*self%n_bf,2*self%n_bf); H = fock_matrix
     call create_(W,2*self%n_bf,2*self%n_bf)
     W = matmul(R,H)
     H = matmul(W,R)
     call destroy_(W)
     call solve_eigenproblem_(H,MO_energies,MO)
     H = matmul(R,MO)
     MO = H
     call destroy_(H)
     call destroy_(R)

   end subroutine

! **************************
! type(diis_type)/Orbital extrapolation
! **************************

!   extrapolate_molecular_orbitals
!!  Extrapolate the moleculeat orbitals
!!   Extrapolate the molecular orbitals using Camp-King or other technique
!!    if (.scfdata.apply_camp_king) .camp_king(E_HF,E_K)
!!   if (.scfdata.apply_dynamic_damping) .dynamic_damp(E_HF,E_K)
!!  end

   subroutine make_diis_error(self,length)
    type(mol_type) :: self
    ! Make the SCF "error" vector from the current fock_matrix and
    ! density_matrix, and return the error length.  Useful for reporting the
    ! error length.
      real(kind=kind(1.0d0)) :: length
      type(opmatrix_type), pointer :: error
      character(128) :: scf_kind
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: err,F,P
      integer(kind=kind(1)) :: tri

      scf_kind = self%scfdata%scf_kind
      tri = triangle_(self%n_bf)
      select case (scf_kind)
         case ("rhf","rdft","restricted_hartree_fock", &
               "xray_rhf","xray_rks","xray_rdft", &
               "noninteracting-group-rhf")
            call create_(error,self%n_bf,"restricted")
            call make_r_diis_error_(self,error%restricted,self%fock_matrix%restricted,self%density_matrix%restricted)
            call compress_(error)
            length = sqrt(dot_product(error%triangle,error%triangle)) / tri
            call destroy_(error)
         case ("rohf","restricted_open_shell_hartree_fock")
            call create_(error,self%n_bf,"restricted")
            call make_r_diis_error_(self,error%restricted,self%fock_matrix%restricted,self%density_matrix%alpha)
            call compress_(error)
            length = sqrt(dot_product(error%triangle,error%triangle)) / tri
            call destroy_(error)
         case ("uhf","udft","xray_udft","unrestricted_hartree_fock", &
               "pnd_uhf")
            call create_(error,self%n_bf,"unrestricted")
            call make_r_diis_error_(self,error%alpha,self%fock_matrix%alpha,self%density_matrix%alpha)
           call make_r_diis_error_(self,error%beta, self%fock_matrix%beta, self%density_matrix%beta)
            call compress_(error)
            length = sqrt(dot_product(error%triangle,error%triangle)) / tri
            call destroy_(error)
         case ("gchf","cghf","complex_general_hartree_fock","general_complex_hartree_fock")
            call create_(error,self%n_bf,"general_complex")
            err => error%general_complex
            F   => self%fock_matrix%general_complex
            P   => self%density_matrix%general_complex
            call make_gc_diis_error_(self,err,F,P)
            call compress_(error)
            length = sqrt(dot_product(error%square,error%square)) / tri
            call destroy_(error)
         case default;  call die_(tonto,"MOL:make_diis_error ... unknown kind, "//trim(scf_kind))
      end select

   end subroutine

   subroutine extrapolate_fock_matrix(self)
    type(mol_type) :: self
    ! Extrapolate the fock matrix, currently only using type(diis_type).
     type(opmatrix_type), pointer :: error
     character(128) :: scf_kind
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: Fr,Pr
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: Fc,Pc
     real(kind=kind(1.0d0)) :: err_len
     integer(kind=kind(1)) :: tri

     if (.not. apply_fock_diis_(self%scfdata)) then;   return; end if
     scf_kind = self%scfdata%scf_kind
     tri = triangle_(self%n_bf)
     select case (scf_kind)
       case ("rhf","rdft","restricted_hartree_fock", &
             "xray_rhf","xray_rks","xray_rdft", &
             "noninteracting-group-rhf")
          call create_(error,self%n_bf,"restricted")
          Fr  => self%fock_matrix%restricted
          Pr  => self%density_matrix%restricted
          call make_r_diis_error_(self,error%restricted,Fr,Pr)
          call compress_(error)
          err_len = norm_(error%triangle)/tri
          call compress_(self%fock_matrix)
          call extrapolate_(self%scfdata%diis,self%fock_matrix%triangle,error%triangle)
          call destroy_(error)
       case ("rohf","restricted_open_shell_hartree_fock")
          call create_(error,self%n_bf,"restricted")
          Fr  => self%fock_matrix%restricted
          Pr  => self%density_matrix%alpha  ! ??? really ???
          call make_r_diis_error_(self,error%restricted,Fr,Pr)
          call compress_(error)
          err_len = norm_(error%triangle)/tri
          call compress_(self%fock_matrix,"restricted")
          call extrapolate_(self%scfdata%diis,self%fock_matrix%triangle,error%triangle)
          call destroy_(error)
       case ("uhf","udft","xray_udft","unrestricted_hartree_fock", &
             "pnd_uhf")
          call create_(error,self%n_bf,"unrestricted")
          Fr  => self%fock_matrix%alpha
          Pr  => self%density_matrix%alpha
          call make_r_diis_error_(self,error%alpha,Fr,Pr)
          Fr  => self%fock_matrix%beta
          Pr  => self%density_matrix%beta
          call make_r_diis_error_(self,error%beta, Fr,Pr)
          call compress_(error)
          err_len = norm_(error%triangle)/tri
          call compress_(self%fock_matrix)
          call extrapolate_(self%scfdata%diis,self%fock_matrix%triangle,error%triangle)
          call destroy_(error)
       case ("gchf","cghf","complex_general_hartree_fock","general_complex_hartree_fock")
          call create_(error,self%n_bf,"general_complex")
          Fc  => self%fock_matrix%general_complex
          Pc  => self%density_matrix%general_complex
          call make_gc_diis_error_(self,error%general_complex,Fc,Pc)
          call compress_(error)
          err_len = norm_(error%square)/tri
          call compress_(self%fock_matrix)
          call extrapolate_(self%scfdata%diis,self%fock_matrix%square,error%square)
          call destroy_(error)
       case default;          call die_(tonto,"MOL:extrapolate_fock_matrix ... unknown SCF kind, "//trim(scf_kind))
     end select
     call set_diis_error_(self%scfdata,err_len)
     call uncompress_(self%fock_matrix)
     call destroy_(error)

   end subroutine

   subroutine make_r_diis_error(self,error,F,P)
    type(mol_type) :: self
    ! Make the real type(diis_type) error matrix, error = FPS - SPF
      real(kind=kind(1.0d0)), dimension(:,:) :: error,F,P
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: S,W

      call create_(W,self%n_bf, self%n_bf)
      call create_(S,self%n_bf, self%n_bf); call get_overlap_matrix_(self,S)
      call to_product_of_(W,P,S)
      call destroy_(S)
      call to_product_of_(error,F,W)
      call destroy_(W)
      call antisymmetric_fold_(error)
      call antisymmetric_reflect_(error)

   end subroutine

   subroutine make_gc_diis_error(self,error,F,P)
    type(mol_type) :: self
    ! Make the complex general type(diis_type) error matrix, error = FPS - SPF
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: error,F,P
       real(kind=kind(1.0d0)), dimension(:,:), pointer :: R
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: S,W

      call create_(W,2*self%n_bf, 2*self%n_bf)
      call create_(S,2*self%n_bf,2*self%n_bf); S=0.0d0
      call create_(R,self%n_bf,self%n_bf)
      call get_overlap_matrix_(self,R)
      call alpha_alpha_set_to_(S,R)
      call beta_beta_set_to_(S,R)
      call destroy_(R)
      call to_product_of_(W,P,S)
      call destroy_(S)
      call to_product_of_(error,F,W)
      call destroy_(W)
      call antihermitian_fold_(error)
      call make_antihermitian_(error)

   end subroutine

!*********************
! Fock matrix routines
!*********************

   subroutine get_fock_matrix(self)
    type(mol_type) :: self
    ! Get the Fock matrix.
    ! If the archive file exists, read it. Otherwise make it.
      type(archive_type) :: archive
      character(128) :: orb_kind

      call ensure_(tonto,associated(self%scfdata),"MOL:get_fock_matrix ... no scfdata")
      orb_kind = spinorbital_kind_(self%scfdata)
      call set_(archive,self%name,"fock_matrix",orb_kind)
      if (exists_(archive)) then; call read_(archive,self%fock_matrix)
      else;                     call make_fock_matrix_(self)
      end if

   end subroutine

   subroutine make_fock_matrix(self,core,r12)
    type(mol_type) :: self
    ! If r12  is present and .false., the r12 part is not computed.
    ! NOTE: The computed fock matrix is immediately archived to disk.
     logical(kind=kind(.true.)), optional :: core,r12
     type(opmatrix_type), pointer :: dP,dF
     character(128) :: orb_kind
     logical(kind=kind(.true.)) :: do_delta_build,kind_ok

     call ensure_(tonto,associated(self%scfdata),"MOL:make_fock_matrix ... no scfdata")
     call ensure_(tonto,associated(self%density_matrix),"MOL:make_fock_matrix ... no density matrix")
     call ensure_(tonto,any_created_(self%density_matrix),"MOL:make_fock_matrix ... no density matrix")
     kind_ok = spinorbital_kind_(self%density_matrix)==spinorbital_kind_(self%scfdata)
     call ensure_(tonto,kind_ok,"MOL:make_fock_matrix ... incompatible density")
      ! Determine whether to use a delta fock matrix build
     orb_kind = spinorbital_kind_(self%density_matrix)
     do_delta_build = do_delta_build_(self%scfdata) &
                      .and. created_(self%old_fock_matrix,orb_kind) &
                      .and. created_(self%old_density_matrix,orb_kind)
     if (do_delta_build) then
        call create_copy_(dP,self%density_matrix)
        call minus_(dP,self%old_density_matrix)
        call create_(dF,self%n_bf)
        call make_fock_matrix_(self,dP,dF,core=.false.,r12=r12)
        call destroy_(self%fock_matrix)
        call create_copy_(self%fock_matrix,self%old_fock_matrix)
        call plus_(self%fock_matrix,dF)
        call destroy_(dF)
        call destroy_(dP)
        call destroy_(self%old_fock_matrix)
        call create_copy_(self%old_fock_matrix,self%fock_matrix)
     else
        call make_fock_matrix_(self,self%density_matrix,self%fock_matrix,core,r12)
     end if

   end subroutine

   subroutine make_fock_matrix_1(self,P,F,core,r12)
    type(mol_type) :: self
    ! For the quicker ERI method.
    ! If r12  is present and .false., the r12 part is not computed
     type(opmatrix_type), pointer :: P,F
     logical(kind=kind(.true.)), optional :: core,r12
     character(128) :: scf_kind,scf_sokind
     logical(kind=kind(.true.)) :: direct

     call ensure_(tonto,associated(self%scfdata),"MOL:make_fock_matrix_1 ... no scfdata")
     call ensure_(tonto,associated(P),"MOL:make_fock_matrix_1 ... no density matrix")
     call ensure_(tonto,any_created_(P),"MOL:make_fock_matrix_1 ... no density matrix")
     scf_sokind = spinorbital_kind_(self%scfdata)  ! otherwise ENSURE line too long.
     call ensure_(tonto,spinorbital_kind_(P)==scf_sokind,"MOL:make_fock_matrix_1 ... incompatible density")
     scf_kind = self%scfdata%scf_kind
     direct = self%scfdata%direct
     if (.not. direct) call get_ERI_integrals_(self)
     select case (scf_kind)
        case ("rhf","restricted_hartree_fock","xray_rhf")
           call make_r_fock_(self,P,F,direct,core,r12)
       case ("rdft","xray_rdft")
          call make_r_dft_fock_(self,P,F,direct,core,r12)
       case ("udft","xray_udft")
          call make_u_dft_fock_(self,P,F,direct,core,r12)
        case ("noninteracting-group-rhf")
           call make_r_group_fock_(self,P,F,direct,core,r12)
        case ("xray_rks")
  !          F.destroy("restricted")
  !          F.create("restricted")
           if (present(core)) then
              if (core) then; call get_kinetic_matrix_(self,F%restricted)
              else;           F%restricted = 0.0d0
              end if
           else
                              call get_kinetic_matrix_(self,F%restricted)
           end if
        case ("rohf","restricted_open_shell_hartree_fock")
           call make_ro_fock_(self,P,F,direct,core,r12)
        case ("uhf","unrestricted_hartree_fock")
           call make_u_fock_(self,P,F,direct,core,r12)
        case ("gchf","cghf", &
              "general_complex_hartree_fock","complex_general_hartree_fock")
           call make_gc_fock_(self,P,F,direct,core,r12)
           call add_gc_so_fock_(self,P,F)
        case default;  call die_(tonto,"MOL:make_fock_matrix_1 ... unknown SCF kind, "//trim(scf_kind))
     end select

   end subroutine

   subroutine add_core_hamiltonian(self,F,nuclei)
    type(mol_type) :: self
    ! Add the core hamiltonain to "F"
    ! if "nuclei" is present then the core_matrix is calculated using
    ! only the nuclei specified
      real(kind=kind(1.0d0)), dimension(:,:) :: F
      integer(kind=kind(1)), dimension(:), optional :: nuclei
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: H

      call create_(H,self%n_bf, self%n_bf)
      call get_core_matrix_(self,H,nuclei)
      F = F + H
      call destroy_(H)

   end subroutine

   subroutine add_core_hamiltonian_1(self,F,nuclei)
    type(mol_type) :: self
    ! Add the core hamiltonain to a general complex "F"
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: F
      integer(kind=kind(1)), dimension(:), optional :: nuclei
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: HH
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: H,S,T,xx,yy,zz,xy,xz,yz,Lx,Ly,Lz
      real(kind=kind(1.0d0)), dimension(3,3) :: BB
      integer(kind=kind(1)) :: k,l
      real(kind=kind(1.0d0)) :: fac
      complex(kind=kind((1.0d0,1.0d0))) :: I

      call create_(HH,2*self%n_bf, 2*self%n_bf); HH = 0.0d0
      call create_(H,self%n_bf, self%n_bf)
      call get_core_matrix_(self,H,nuclei)
      call alpha_alpha_plus_(HH,H)
      call beta_beta_plus_(HH,H)
      call destroy_(H)
      I = (0.0d0,1.0d0)
      if (self%scfdata%using_1e_sl_term) then
            call create_(Lx,self%n_bf,self%n_bf)
            call create_(Ly,self%n_bf,self%n_bf)
            call create_(Lz,self%n_bf,self%n_bf)
            call get_spin_orbit_matrices_(self,Lx,Ly,Lz)
            fac = 2.002319304386/(8.0d0*137.03599d0**2)
            fac = fac * self%scfdata%sl_1e_factor
            call beta_alpha_plus_(HH,Lx,factor=-fac*I)
            call beta_alpha_plus_(HH,Ly,factor=fac)
            call alpha_alpha_plus_(HH,Lz,factor=-fac*I)
            call beta_beta_plus_(HH,Lz,factor=fac*I)
            call destroy_(Lz); call destroy_(Ly); call destroy_(Lx)
      else if (self%scfdata%using_1e_zora_term) then
            call create_(T,self%n_bf,self%n_bf)
            call create_(Lx,self%n_bf,self%n_bf)
            call create_(Ly,self%n_bf,self%n_bf)
            call create_(Lz,self%n_bf,self%n_bf)
            call get_1e_ZORA_matrices_(self,T,Lx,Ly,Lz)
            call alpha_alpha_plus_(HH,T)
            call beta_beta_plus_(HH,T)
            call get_kinetic_matrix_(self,T)  ! remove normal kinetic energy
            call alpha_alpha_plus_(HH,T,factor=-1.0d0)
            call beta_beta_plus_(HH,T,factor=-1.0d0)
            fac = 2.002319304386/2.0d0
            fac = fac * self%scfdata%sl_1e_factor
            call beta_alpha_plus_(HH,Lx,factor=-fac*I)
            call beta_alpha_plus_(HH,Ly,factor=fac)
            call alpha_alpha_plus_(HH,Lz,factor=-fac*I)
            call beta_beta_plus_(HH,Lz,factor=fac*I)
            call destroy_(T)
            call destroy_(Lz); call destroy_(Ly); call destroy_(Lx)
      end if
      if (.not. is_zero_(self%B_field)) then
         if (self%scfdata%using_bs_term) then
            call create_(S,self%n_bf,self%n_bf)
            call get_overlap_matrix_(self,S)
            fac = 2.002319304386/4.0d0
            call alpha_alpha_plus_(HH,S,factor=fac*self%B_field(3))
            call beta_beta_plus_(HH,S,factor=-fac*self%B_field(3))
            call beta_alpha_plus_(HH,S,factor=fac*self%B_field(1))
            call beta_alpha_plus_(HH,S,factor=fac*self%B_field(2)*I)
            call destroy_(S)
         end if
         if (self%scfdata%using_bs_t_term) then
            call create_(T,self%n_bf,self%n_bf)
            call get_kinetic_matrix_(self,T)
            fac = -2.002319304386/(4.0d0*137.03599d0**2)
            call alpha_alpha_plus_(HH,T,factor=fac*self%B_field(3))
            call beta_beta_plus_(HH,T,factor=-fac*self%B_field(3))
            call beta_alpha_plus_(HH,T,factor=fac*self%B_field(1))
            call beta_alpha_plus_(HH,T,factor=fac*self%B_field(2)*I)
            call destroy_(T)
         end if
         if (self%scfdata%using_bl_term) then
            call create_(Lx,self%n_bf,self%n_bf)
            call create_(Ly,self%n_bf,self%n_bf)
            call create_(Lz,self%n_bf,self%n_bf)
            call get_L_matrices_(self,Lx,Ly,Lz)
            Lx = Lx*self%B_field(1) + Ly*self%B_field(2) + Lz*self%B_field(3)
            fac = 0.50d0
            call alpha_alpha_plus_(HH,Lx,factor=fac*I)
            call beta_beta_plus_(HH,Lx,factor=fac*I)
            call destroy_(Lz); call destroy_(Ly); call destroy_(Lx)
         end if
         if (self%scfdata%using_aa_term) then
           call create_(xx,self%n_bf,self%n_bf); call create_(yy,self%n_bf,self%n_bf); call create_(zz,self%n_bf,self%n_bf)
           call create_(xy,self%n_bf,self%n_bf); call create_(xz,self%n_bf,self%n_bf); call create_(yz,self%n_bf,self%n_bf)
           call get_quadrupole_matrices_(self,xx,yy,zz,xy,xz,yz)
           fac = 1.0d0/8.0d0
           do k = 1,3
              BB(k,k) = fac*(sum(self%B_field*self%B_field) - self%B_field(k)*self%B_field(k))
           end do
           do k = 1,3
           do l = k+1,3
              BB(k,l) = -2.0d0*fac*self%B_field(k)*self%B_field(l)
           end do
           end do
           xx =      BB(1,1)*xx; xx = xx + BB(2,2)*yy; xx = xx + BB(3,3)*zz
           xx = xx + BB(1,2)*xy; xx = xx + BB(1,3)*xz; xx = xx + BB(2,3)*yz
           call alpha_alpha_plus_(HH,xx)
           call beta_beta_plus_(HH,xx)
           call destroy_(yz); call destroy_(xz); call destroy_(xy)
           call destroy_(zz); call destroy_(yy); call destroy_(xx)
         end if
         if (self%scfdata%using_1e_srxa_term) then
            call create_(Lx,self%n_bf,self%n_bf)
            call create_(Ly,self%n_bf,self%n_bf)
            call create_(Lz,self%n_bf,self%n_bf)
            call get_spin_orbit_B_matrices_(self,Lx,Ly,Lz)
            fac = 2.002319304386/(2.0d0*8.0d0*(137.03599d0)**2)
            call beta_alpha_plus_(HH,Lx,factor=fac)
            call beta_alpha_plus_(HH,Ly,factor=fac*I)
            call alpha_alpha_plus_(HH,Lz,factor=fac)
            call beta_beta_plus_(HH,Lz,factor=-fac)
            call destroy_(Lz); call destroy_(Ly); call destroy_(Lx)
         end if
      end if
      call make_hermitian_(HH)
      F = HH + F
      call destroy_(HH)

   end subroutine

   subroutine make_r_fock(self,P,F,direct,core,r12)
    type(mol_type) :: self
    ! Make a new restricted Fock matrix "F" from the density matrix "P".
    ! If present and .true. , "direct" means calculate integrals on the fly
    ! If present and .false., "core" removes the core matrix contribution
    ! If present and .false., "r12"  removes the two electron contribution
     logical(kind=kind(.true.)), optional :: direct,core,r12
     type(opmatrix_type), pointer :: P,F
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: J,K
     logical(kind=kind(.true.)) :: do_direct,add_core,add_r12

     call ensure_(tonto,associated(P),"MOL:make_r_fock ... no density matrix")
     call ensure_(tonto,associated(F),"MOL:make_r_fock ... no fock matrix")
     call ensure_(tonto,F%n_bf==self%n_bf,"MOL:make_r_fock ... Fock matrix dimensions wrong")
     call ensure_(tonto,P%n_bf==self%n_bf,"MOL:make_r_fock ... Density matrix dimensions wrong")
     call ensure_(tonto,associated(P%restricted),"MOL:make_r_fock ... no density matrix")
     call destroy_(F,"restricted")
     call create_(F,"restricted")
     do_direct= .false.
     add_core = .true.
     add_r12  = .true.
     if (present(direct)) do_direct = direct
     if (present(core))   add_core = core
     if (present(r12))    add_r12  = r12
     if (add_r12) then
        call create_(J,self%n_bf,self%n_bf)
        call create_(K,self%n_bf,self%n_bf)
        if (do_direct) then; call make_r_JK_direct_(self,J,K,P%restricted)
        else;                call make_r_JK_disk_(self,J,K,P%restricted)
        end if
        F%restricted = J - 0.50d0*K
        call destroy_(K)
        call destroy_(J)
     else
        F%restricted = 0.0d0
     end if
     if (add_core) call add_core_hamiltonian_(self,F%restricted)

   end subroutine

   subroutine make_r_dft_fock(self,P,F,direct,core,r12)
    type(mol_type) :: self
    ! Make a new restricted Fock matrix "F" from the density matrix "P".
    ! If present and .true. , "direct" means calculate integrals on the fly
    ! If present and .false., "core" removes the core matrix contribution
    ! If present and .false., "r12"  removes the two electron contribution
     logical(kind=kind(.true.)), optional :: direct,core,r12
     type(opmatrix_type), pointer :: P,F
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: J,K
     logical(kind=kind(.true.)) :: do_direct,add_core,add_r12

     call ensure_(tonto,associated(P),"MOL:make_r_dft_fock ... no density matrix")
     call ensure_(tonto,associated(F),"MOL:make_r_dft_fock ... no fock matrix")
     call ensure_(tonto,F%n_bf==self%n_bf,"MOL:make_r_dft_fock ... Fock matrix dimensions wrong")
     call ensure_(tonto,P%n_bf==self%n_bf,"MOL:make_r_dft_fock ... Density matrix dimensions wrong")
     call ensure_(tonto,associated(P%restricted),"MOL:make_r_dft_fock ... no density matrix")
     call destroy_(F,"restricted")
     call create_(F,"restricted")
     do_direct= .false.
     add_core = .true.
     add_r12  = .true.
     if (present(direct)) do_direct = direct
     if (present(core))   add_core = core
     if (present(r12))    add_r12  = r12
     if (add_r12) then
        call create_(J,self%n_bf,self%n_bf)
        call create_(K,self%n_bf,self%n_bf)
        if (do_direct) then; call make_r_J_direct_(self,J,P%restricted)
        else;                call make_r_J_disk_(self,J,P%restricted)
        end if
        K=0.0d0
        if (.not. associated(self%molecular_orbitals) .or. all_destroyed_(self%molecular_orbitals)) then
          call make_natural_orbitals_(self)
        else
          call assign_natural_orbitals_(self)
        end if
        call add_ex_corr_matrix_(self,K)
        call symmetric_reflect_(K)
        F%restricted = J + K
        call destroy_(K)
        call destroy_(J)
     else
        F%restricted = 0.0d0
     end if
     if (add_core) call add_core_hamiltonian_(self,F%restricted)

   end subroutine

   subroutine make_u_dft_fock(self,P,F,direct,core,r12)
    type(mol_type) :: self
    ! Make a new restricted Fock matrix "F" from the density matrix "P".
    ! If present and .true. , "direct" means calculate integrals on the fly
    ! If present and .false., "core" removes the core matrix contribution
    ! If present and .false., "r12"  removes the two electron contribution
     logical(kind=kind(.true.)), optional :: direct,core,r12
     type(opmatrix_type), pointer :: P,F
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: J,Ka,Kb,Pab
     logical(kind=kind(.true.)) :: do_direct,add_core,add_r12

     call ensure_(tonto,associated(P),"MOL:make_u_dft_fock ... no density matrix")
     call ensure_(tonto,associated(F),"MOL:make_u_dft_fock ... no fock matrix")
     call ensure_(tonto,F%n_bf==self%n_bf,"MOL:make_u_dft_fock ... Fock matrix dimensions wrong")
     call ensure_(tonto,P%n_bf==self%n_bf,"MOL:make_u_dft_fock ... Density matrix dimensions wrong")
     call ensure_(tonto,associated(P%alpha),"MOL:make_u_dft_fock ... no density matrix")
     call ensure_(tonto,associated(P%beta),"MOL:make_u_dft_fock ... no density matrix")
     call destroy_(F,"alpha")
     call create_(F,"alpha")
     call destroy_(F,"beta")
     call create_(F,"beta")
     do_direct= .false.
     add_core = .true.
     add_r12  = .true.
     if (present(direct)) do_direct = direct
     if (present(core))   add_core = core
     if (present(r12))    add_r12  = r12
     if (add_r12) then
        call create_(J,self%n_bf,self%n_bf)
        call create_(Ka,self%n_bf,self%n_bf)
        call create_(Kb,self%n_bf,self%n_bf)
        call create_(Pab,self%n_bf,self%n_bf)
        Pab  = P%alpha + P%beta
        call make_r_J_direct_(self,J,Pab)
        call destroy_(Pab)
        Ka=0.0d0
        Kb=0.0d0
        if (.not. associated(self%molecular_orbitals) .or. all_destroyed_(self%molecular_orbitals)) then
          call make_natural_orbitals_(self)
        else
          call assign_natural_orbitals_(self)
        end if
        call add_ex_corr_matrix_(self,Ka,Kb)
        F%alpha = J + Ka
        F%beta = J + Kb
        call destroy_(Kb)
        call destroy_(Ka)
        call destroy_(J)
     else
        F%alpha = 0.0d0
        F%beta  = 0.0d0
     end if
     if (add_core) call add_core_hamiltonian_(self,F%alpha)
     if (add_core) call add_core_hamiltonian_(self,F%beta)

   end subroutine

   subroutine make_r_group_fock(self,P,F,direct,core,r12)
    type(mol_type) :: self
    ! Make a new restricted Fock matrix "F" from the density matrix "P".
    ! If present and .true. , "direct" means calculate integrals on the fly
    ! If present and .false., "core" removes the core matrix contribution
    ! If present and .false., "r12"  removes the two electron contribution
     logical(kind=kind(.true.)), optional :: direct,core,r12
     type(opmatrix_type), pointer :: P,F
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: J,K
     logical(kind=kind(.true.)) :: do_direct,add_core,add_r12

   call ensure_(tonto,associated(self%atom_group),"MOL:make_r_group_fock ... no atom_group info")
   call ensure_(tonto,associated(P),"MOL:make_r_group_fock ... no density matrix")
   call ensure_(tonto,associated(F),"MOL:make_r_group_fock ... no fock matrix")
   call ensure_(tonto,F%n_bf == self%n_bf,"MOL:make_r_group_fock ... Fock matrix dimensions wrong")
   call ensure_(tonto,P%n_bf == self%n_bf,"MOL:make_r_group_fock ... Density matrix dimensions wrong")
   call ensure_(tonto,associated(P%restricted),"MOL:make_r_group_fock ... no density matrix")
     call destroy_(F,"restricted")
     call create_(F,"restricted")
     do_direct= .false.
     add_core = .true.
     add_r12  = .true.
     if (present(direct)) do_direct = direct
     if (present(core))   add_core = core
     if (present(r12))    add_r12  = r12
     do_direct = .true.
     if (add_r12) then
        call create_(J,self%n_bf,self%n_bf)
        call create_(K,self%n_bf,self%n_bf)
        if (do_direct) call make_r_group_JK_(self,J,K,P%restricted)
        F%restricted = J - 0.50d0*K
        call destroy_(K)
        call destroy_(J)
     else
        F%restricted = 0.0d0
     end if
     if (add_core) call add_core_hamiltonian_(self,F%restricted)

   end subroutine

   subroutine make_u_fock(self,P,F,direct,core,r12)
    type(mol_type) :: self
    ! Make a new unrestricted Fock matrix.
    ! If present and .true. , "direct" means calculate integrals on the fly
    ! If present and .false., "core" removes the core matrix contribution
    ! If present and .false., "r12"  removes the two electron contribution
     type(opmatrix_type), pointer :: P,F
     logical(kind=kind(.true.)), optional :: direct,core,r12
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: J,Ka,Kb,Pab
     logical(kind=kind(.true.)) :: do_direct,add_core,add_r12

   call ensure_(tonto,associated(P),"MOL:make_u_fock ... no density matrix")
   call ensure_(tonto,associated(F),"MOL:make_u_fock ... no fock matrix")
   call ensure_(tonto,associated(P%alpha),"MOL:make_u_fock ... no density matrix")
   call ensure_(tonto,associated(P%beta),"MOL:make_u_fock ... no density matrix")
   call ensure_(tonto,F%n_bf==self%n_bf,"MOL:make_u_fock ... Fock matrix dimensions wrong")
   call ensure_(tonto,P%n_bf==self%n_bf,"MOL:make_u_fock ... Density matrix dimensions wrong")
     call destroy_(F,"unrestricted")
     call create_(F,"unrestricted")
     do_direct= .false.
     add_core = .true.
     add_r12  = .true.
     if (present(direct)) do_direct = direct
     if (present(core))   add_core = core
     if (present(r12))    add_r12  = r12
     if (add_r12) then
        call create_(J,self%n_bf,self%n_bf)
        call create_(Ka,self%n_bf,self%n_bf)
        call create_(Kb,self%n_bf,self%n_bf)
        call create_(Pab,self%n_bf,self%n_bf)
        Pab  = P%alpha + P%beta
        if (do_direct) then; call make_u_JK_direct_(self,J,Ka,Kb,Pab,P%alpha,P%beta)
        else;                call make_u_JK_disk_(self,J,Ka,Kb,Pab,P%alpha,P%beta)
        end if
        call destroy_(Pab)
        F%alpha = J - Ka
        F%beta = J - Kb
        call destroy_(Kb)
        call destroy_(Ka)
        call destroy_(J)
     else
        F%alpha = 0.0d0
        F%beta  = 0.0d0
     end if
     if (add_core) call add_core_hamiltonian_(self,F%alpha)
     if (add_core) call add_core_hamiltonian_(self,F%beta)

   end subroutine

   subroutine make_ro_fock(self,P,F,direct,core,r12)
    type(mol_type) :: self
    ! Make a new restricted open shell Fock matrix.
    ! If present and .true. , "direct" means calculate integrals on the fly
    ! If present and .false., "core" removes the core matrix contribution
    ! If present and .false., "r12"  removes the two electron contribution
     type(opmatrix_type), pointer :: F,P
     logical(kind=kind(.true.)), optional :: direct,core,r12
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: Fr,Fa,Fb,S,W

   call ensure_(tonto,associated(self%molecular_orbitals%restricted),"MOL:make_ro_fock ... no orbitals")
     call destroy_(F,"restricted")
     call create_(F,"restricted")
     call make_u_fock_(self,P,F,direct,core,r12)
     Fr => F%restricted
     call create_(Fa,self%n_bf,self%n_bf); Fa = F%alpha
     call create_(Fb,self%n_bf,self%n_bf); Fb = F%beta
     call change_basis_(Fa,self%molecular_orbitals%restricted)
     call change_basis_(Fb,self%molecular_orbitals%restricted)
     Fr = 0.50d0*(Fa+Fb)
     Fr(self%n_b+1:self%n_a,     1:self%n_b) = Fb(self%n_b+1:self%n_a,     1:self%n_b)
     Fr(     1:self%n_b,self%n_b+1:self%n_a) = Fb(     1:self%n_b,self%n_b+1:self%n_a)
     Fr(self%n_a+1:    ,self%n_b+1:self%n_a) = Fa(self%n_a+1:    ,self%n_b+1:self%n_a)
     Fr(self%n_b+1:self%n_a,self%n_a+1:    ) = Fa(self%n_b+1:self%n_a,self%n_a+1:    )
     S => Fb; call get_overlap_matrix_(self,S)
     W => Fa; call to_product_of_(W,self%molecular_orbitals%restricted,S,transpose_a=.true.)
     call change_basis_(Fr,W)  ! change to AO basis
     call destroy_(Fb)
     call destroy_(Fa)

   end subroutine

   subroutine make_gc_fock(self,P,F,direct,core,r12)
    type(mol_type) :: self
    ! Make a new complex general Fock matrix.
    ! If present and .true. , "direct" means calculate integrals on the fly
    ! If present and .false., "core" removes the core matrix contribution
    ! If present and .false., "r12"  removes the two electron contribution
     logical(kind=kind(.true.)), optional :: direct,core,r12
     type(opmatrix_type), pointer :: P,F
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: Ka,Kb,Kba,Pa,Pb,Pba,Fgc
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: J, Pab
     logical(kind=kind(.true.)) :: do_direct,add_core,add_r12

   call ensure_(tonto,associated(P),"MOL:make_gc_fock ... no density matrix")
   call ensure_(tonto,associated(F),"MOL:make_gc_fock ... no fock matrix")
   call ensure_(tonto,associated(P%general_complex),"MOL:make_gc_fock ... no density matrix")
   call ensure_(tonto,F%n_bf==self%n_bf,"MOL:make_gc_fock ... Fock matrix dimensions wrong")
   call ensure_(tonto,P%n_bf==self%n_bf,"MOL:make_gc_fock ... Density matrix dimensions wrong")
     call destroy_(F,"general_complex")
     call create_(F,"general_complex")
     do_direct= .false.
     add_core = .true.
     add_r12  = .true.
     if (present(direct)) do_direct = direct
     if (present(core)) add_core = core
     if (present(r12))  add_r12  = r12
     Fgc => F%general_complex
     if (add_r12) then
        call create_(J,self%n_bf,self%n_bf)
        call create_(Ka,self%n_bf,self%n_bf)
        call create_(Kb,self%n_bf,self%n_bf)
        call create_(Kba,self%n_bf,self%n_bf)
        call create_(Pa,self%n_bf,self%n_bf);  call alpha_alpha_put_to_(P%general_complex,Pa)
        call create_(Pb,self%n_bf,self%n_bf);  call beta_beta_put_to_(P%general_complex,Pb)
        call create_(Pba,self%n_bf,self%n_bf); call beta_alpha_put_to_(P%general_complex,Pba)
        call create_(Pab,self%n_bf,self%n_bf)
        Pab  = Pa + Pb
        if (do_direct) then; call make_gc_JK_direct_(self,J,Ka,Kb,Kba,Pab,Pa,Pb,Pba)
        else;                call make_gc_JK_disk_(self,J,Ka,Kb,Kba,Pab,Pa,Pb,Pba)
        end if
        call destroy_(Pab)
        call destroy_(Pba)
        call destroy_(Pb)
        call destroy_(Pa)
        Ka = J - Ka; call alpha_alpha_set_to_(Fgc,Ka)
        Kb = J - Kb; call beta_beta_set_to_(Fgc,Kb)
        Kba = -Kba;  call beta_alpha_set_to_(Fgc,Kba)
        call destroy_(Kba)
        call destroy_(Kb)
        call destroy_(Ka)
        call destroy_(J)
     else
        Fgc = 0.0d0
     end if
     call make_hermitian_(Fgc)
     if (add_core) call add_core_hamiltonian_(self,Fgc)

   end subroutine

   subroutine add_gc_so_fock(self,P,F,direct)
    type(mol_type) :: self
    ! Make the general complex two electron spin orbit contribution to the Fock
    ! matrix.
     type(opmatrix_type), pointer :: P,F
     logical(kind=kind(.true.)), optional :: direct
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:,:,:), pointer :: JS,KS,JO,KO
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:,:), pointer :: Pgc
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: Faa,Fbb,Fba
     complex(kind=kind((1.0d0,1.0d0))) :: I
     real(kind=kind(1.0d0)) :: fac
     integer(kind=kind(1)) :: a,b,x,y,z
     logical(kind=kind(.true.)) :: do_direct

   call ensure_(tonto,associated(P),"MOL:add_gc_so_fock ... no density matrix")
   call ensure_(tonto,associated(F),"MOL:add_gc_so_fock ... no fock matrix")
   call ensure_(tonto,associated(P%general_complex),"MOL:add_gc_so_fock ... no density matrix")
   call ensure_(tonto,associated(F%general_complex),"MOL:add_gc_so_fock ... no fock matrix")
   call ensure_(tonto,F%n_bf==self%n_bf,"MOL:add_gc_so_fock ... Fock matrix dimensions wrong")
   call ensure_(tonto,P%n_bf==self%n_bf,"MOL:add_gc_so_fock ... Density matrix dimensions wrong")
     do_direct= .false.
     if (present(direct)) do_direct = direct
     if (self%scfdata%using_2e_sl_term) then
       call get_spin_orbit_integrals_(self)
       call create_(Faa,self%n_bf,self%n_bf)
       call create_(Fbb,self%n_bf,self%n_bf)
       call create_(Fba,self%n_bf,self%n_bf)
       call create_(JS,self%n_bf,self%n_bf,2,2,3); call create_(KS,self%n_bf,self%n_bf,2,2,3)
       call create_(JO,self%n_bf,self%n_bf,2,2,3); call create_(KO,self%n_bf,self%n_bf,2,2,3)
       call create_(Pgc,self%n_bf,self%n_bf,2,2)
       call alpha_alpha_put_to_(P%general_complex,Pgc(:,:,1,1))
       call beta_alpha_put_to_(P%general_complex,Pgc(:,:,2,1))
       call alpha_beta_put_to_(P%general_complex,Pgc(:,:,1,2))
       call beta_beta_put_to_(P%general_complex,Pgc(:,:,2,2))
       if (do_direct) then
       call make_gc_so_JK_direct_(self,JS,KS,JO,KO,Pgc)
       else
       call make_gc_so_JK_disk_(self,JS(:,:,:,:,1),KS(:,:,:,:,1),JO(:,:,:,:,1),KO(:,:,:,:,1),Pgc,"x")
       call make_gc_so_JK_disk_(self,JS(:,:,:,:,2),KS(:,:,:,:,2),JO(:,:,:,:,2),KO(:,:,:,:,2),Pgc,"y")
       call make_gc_so_JK_disk_(self,JS(:,:,:,:,3),KS(:,:,:,:,3),JO(:,:,:,:,3),KO(:,:,:,:,3),Pgc,"z")
       end if
       call destroy_(Pgc)
       I = (0.0d0,1.0d0)
       a = 1; b = 2; x = 1; y = 2; z = 3
       Faa =  3.0d0*JO(:,:,a,a,z) +       JO(:,:,b,b,z) + 3.0d0*JS(:,:,a,a,z) -   JS(:,:,b,b,z) &
           +    2.0d0*JS(:,:,a,b,x) +   2.0d0*JS(:,:,b,a,x) +       JO(:,:,a,b,x) +   JO(:,:,b,a,x) &
           +  I*2.0d0*JS(:,:,a,b,y) - I*2.0d0*JS(:,:,b,a,y) +     I*JO(:,:,a,b,y) - I*JO(:,:,b,a,y) &
           -    2.0d0*KO(:,:,b,a,x) -       KO(:,:,a,b,x) -   2.0d0*KS(:,:,a,b,x) -   KS(:,:,b,a,x) &
           +  I*2.0d0*KO(:,:,b,a,y) -     I*KO(:,:,a,b,y) - I*2.0d0*KS(:,:,a,b,y) + I*KS(:,:,b,a,y) &
           -  3.0d0*KO(:,:,a,a,z) - 3.0d0*KS(:,:,a,a,z)
       Fbb = -3.0d0*JO(:,:,b,b,z) -       JO(:,:,a,a,z) - 3.0d0*JS(:,:,b,b,z) +   JS(:,:,a,a,z) &
           +    2.0d0*JS(:,:,a,b,x) +   2.0d0*JS(:,:,b,a,x) +       JO(:,:,a,b,x) +   JO(:,:,b,a,x) &
           +  I*2.0d0*JS(:,:,a,b,y) - I*2.0d0*JS(:,:,b,a,y) +     I*JO(:,:,a,b,y) - I*JO(:,:,b,a,y) &
           -    2.0d0*KO(:,:,a,b,x) -       KO(:,:,b,a,x) -   2.0d0*KS(:,:,b,a,x) -   KS(:,:,a,b,x) &
           -  I*2.0d0*KO(:,:,a,b,y) + I*KO(:,:,b,a,y)     + I*2.0d0*KS(:,:,b,a,y) - I*KS(:,:,a,b,y) &
           +  3.0d0*KO(:,:,b,b,z) + 3.0d0*KS(:,:,b,b,z)
       Fba =    2.0d0*JO(:,:,a,a,x) +   2.0d0*JO(:,:,b,b,x) +       JS(:,:,a,a,x) +   JS(:,:,b,b,x) &
           +  I*2.0d0*JO(:,:,a,a,y) + I*2.0d0*JO(:,:,b,b,y) +     I*JS(:,:,a,a,y) + I*JS(:,:,b,b,y) &
           -    2.0d0*KO(:,:,a,a,x) -       KO(:,:,b,b,x) -   2.0d0*KS(:,:,b,b,x) -   KS(:,:,a,a,x) &
           -  I*2.0d0*KO(:,:,a,a,y) -     I*KO(:,:,b,b,y) - I*2.0d0*KS(:,:,b,b,y) - I*KS(:,:,a,a,y) &
           +        KO(:,:,b,a,z) -       KS(:,:,b,a,z)
       call destroy_(KO); call destroy_(JO); call destroy_(KS); call destroy_(JS)

       fac = 2.002319304386/(8.0d0*137.03599d0**2)
       fac = fac * self%scfdata%sl_2e_factor
       call alpha_alpha_plus_(F%general_complex,Faa,factor=I*fac)
       call beta_beta_plus_(F%general_complex,Fbb,factor=I*fac)
       call beta_alpha_plus_(F%general_complex,Fba,factor=I*fac)
       Fbb = conjg(transpose(Fba))
       call alpha_beta_plus_(F%general_complex,Fbb,factor=-I*fac)
       call destroy_(Fba); call destroy_(Fbb); call destroy_(Faa)
     end if

   end subroutine

!**********************
! J & K matrix routines
!**********************

   function in_same_atom_group(self,atom_a,atom_b,group) result(res)
    type(mol_type) :: self
    ! Return .true. if the atoms are all in the same atom_group
    ! If present, "group" is set to the common group index.
     integer(kind=kind(1)) :: atom_a,atom_b
     integer(kind=kind(1)), optional :: group
     logical(kind=kind(.true.)) :: res
     integer(kind=kind(1)) :: n,n_group

   call ensure_(tonto,associated(self%atom_group),"MOL:in_same_atom_group ... no atom group information")
     n_group = size(self%atom_group)
     res = .true.
     do n = 1,n_group
        if (any(atom_a==self%atom_group(n)%element)) then
          if (.not. any(atom_b==self%atom_group(n)%element)) res = .false.
          exit
        end if
     end do
     if (present(group)) then
        if (res) then; group = n
        else;          group = 0
        end if
     end if

   end function

   function in_same_atom_group_1(self,atom_a,atom_b,atom_c,atom_d) result(res)
    type(mol_type) :: self
    ! Return .true. if the atoms are all in the same atom_group
     integer(kind=kind(1)) :: atom_a,atom_b,atom_c,atom_d
     logical(kind=kind(.true.)) :: res
     integer(kind=kind(1)) :: n,n_group

   call ensure_(tonto,associated(self%atom_group),"MOL:in_same_atom_group_1 ... no atom group information")
     n_group = size(self%atom_group)
     res = .true.
     do n = 1,n_group
        if (any(atom_a==self%atom_group(n)%element)) then
          if (.not. any(atom_b==self%atom_group(n)%element)) res = .false.
          if (.not. any(atom_c==self%atom_group(n)%element)) res = .false.
          if (.not. any(atom_d==self%atom_group(n)%element)) res = .false.
          exit
        end if
     end do

   end function

   function schwarz_inequality_test(self,cutoff,ab,cd,a,b,c,d,P_max,I_max) result(skip)
    type(mol_type) :: self
    ! Return "skip", a logical variable which is .true. if the maximum contribution
    ! to the restricted Fock matrix (based on the schwarz inequality test) from a
    ! shell of ERI integrals (ab|cd) with shell pair indices "ab" and "cd" is
    ! smaller than a preset "cutoff". "P_max" is the maximum density matrix
    ! elements for each shell pair "ab", and "I_max(ab)" is maximum value of the
    ! square root of the integral (ab|ab) for the shell pair with index "ab".
     logical(kind=kind(.true.)) :: skip
     real(kind=kind(1.0d0)) :: cutoff
     integer(kind=kind(1)) :: ab,cd, a,b,c,d
     real(kind=kind(1.0d0)), dimension(:) :: P_max,I_max
     real(kind=kind(1.0d0)) :: Pmax
     integer(kind=kind(1)) :: ac,ad,bc,bd,k,l,m,n

     k = max(b,c); l = min(b,c)
     m = max(b,d); n = min(b,d)
     ac = ((a-1)*a)/2 + c           ! ab|cd < sqrt(ab|ab) * sqrt(cd|cd) test.
     ad = ac - c + d
     bc = ((k-1)*k)/2 + l
     bd = ((m-1)*m)/2 + n
     Pmax = max(P_max(ab),P_max(cd),P_max(ac),P_max(ad),P_max(bc),P_max(bd))
     if (Pmax < 10.0d0**(-20)) then
       skip = .true.
     else
        if (Pmax*I_max(ab)*I_max(cd) > cutoff) then; skip = .false.
        else;                                        skip = .true.
        end if
     end if

   end function

   subroutine make_max_density_elements(self,P_max,P)
    type(mol_type) :: self
    ! Make "P_max", the maximum of the (a,b) density elements of "P" over each
    ! shell pair, for use in the schwarz test.
     real(kind=kind(1.0d0)), dimension(:) :: P_max
     real(kind=kind(1.0d0)), dimension(:,:) :: P
     integer(kind=kind(1)) :: ab,fa,fb,la,lb,aa,bb, a,b
     real(kind=kind(1.0d0)) :: Pmax

   call ensure_(tonto,size(P_max)==self%n_shell_pairs,"MOL:make_max_density_elements ... wrong size, P_max")
   call ensure_(tonto,is_square_(P),"MOL:make_max_density_elements ... wrong shape, P")
   call ensure_(tonto,size(P,1)==self%n_bf,"MOL:make_max_density_elements ... wrong size, P")
      ! Store the largest integral for each shell pair ab|ab
     do ab = 1, self%n_shell_pairs
       call get_shell_pair_indices_(self,ab,aa,bb,fa,la,fb,lb)
       Pmax = 0.0d0
       do a = fa,la
       do b = fb,lb
          Pmax = max(abs(P(a,b)),Pmax)
       end do
       end do
       P_max(ab) = Pmax
     end do

   end subroutine

   subroutine make_max_abab_integrals(self,I_max)
    type(mol_type) :: self
    ! Make "I_max", the square root of the maximum of the (ab|ab) ERI integrals.
     real(kind=kind(1.0d0)), dimension(:) :: I_max
     integer(kind=kind(1)) :: ab,fa,fb,la,lb,sa,sb,na,nb, a,b
     type(shell4_type) :: sh4
     real(kind=kind(1.0d0)), dimension(:,:,:,:), pointer :: Iabab
     real(kind=kind(1.0d0)) :: Imax

   call ensure_(tonto,size(I_max)==self%n_shell_pairs,"MOL:make_max_abab_integrals ... wrong size, I_max")
      ! Store the largest integral for each shell pair ab|ab
     do ab = 1, self%n_shell_pairs
       call get_shell_pair_indices_(self,ab,sa,sb,fa,la,fb,lb)
       na = la-fa+1
       nb = lb-fb+1
       call set_shell_quartet_ab_(self,sh4,sa,sb)   ! Set (ab|ab) shell
       call set_shell_quartet_cd_(self,sh4,sa,sb)
       call create_(Iabab,na,nb,na,nb)
       call get_ERI_(sh4,Iabab)
       call destroy_ptr_part_(sh4)
       Imax = 0.0d0
       do a=1,na
       do b=1,nb
          Imax = max(abs(Iabab(a,b,a,b)),Imax)
       end do
       end do
       I_max(ab) = sqrt(Imax)
       call destroy_(Iabab)
     end do

   end subroutine

   subroutine make_max_abab_so_integrals(self,I_max)
    type(mol_type) :: self
    ! Make "I_max", the square root of the maximum of the all (ab|ab) spin orbit
    ! integrals.
     real(kind=kind(1.0d0)), dimension(:) :: I_max
     integer(kind=kind(1)) :: ab,fa,fb,la,lb,sa,sb,na,nb, a,b
     type(shell4_type) :: sh4
     real(kind=kind(1.0d0)), dimension(:,:,:,:,:), pointer :: S,O
     real(kind=kind(1.0d0)) :: Imax

   call ensure_(tonto,size(I_max)==self%n_shell_pairs,"MOL:make_max_abab_so_integrals ... wrong size, I_max")
      ! Store the largest integral for each shell pair ab|ab
     do ab = 1, self%n_shell_pairs            ! Store the largest integral for each
       call get_shell_pair_indices_(self,ab,sa,sb,fa,la,fb,lb)
       na = la-fa+1
       nb = lb-fb+1
       call set_shell_quartet_ab_(self,sh4,sa,sb)
       call set_shell_quartet_cd_(self,sh4,sa,sb)
       call create_(S,na,nb,na,nb,3)
       call create_(O,na,nb,na,nb,3)
       call make_spin_orbit_ints_(sh4,S,O)
       call destroy_ptr_part_(sh4)
       Imax = 0.0d0
       do a=1,na
       do b=1,nb
          Imax = max(abs(S(a,b,a,b,1)),abs(S(a,b,a,b,2)),abs(S(a,b,a,b,3)), &
                     abs(O(a,b,a,b,1)),abs(O(a,b,a,b,2)),abs(O(a,b,a,b,3)),Imax)
       end do
       end do
       I_max(ab) = sqrt(Imax)
       call destroy_(O)
       call destroy_(S)
     end do

   end subroutine

   subroutine make_r_JK_direct(self,J,K,P)
    type(mol_type) :: self
    ! Make the real coulomb matrix "J" and exchange matrix "K" matrix from a
    ! symmetric density matrix "P" directly.
     real(kind=kind(1.0d0)), dimension(:,:) :: J,K,P
     real(kind=kind(1.0d0)), dimension(:), pointer :: max_I,max_P
      !sh4 :: type(shell4_type)
     type(shell1quartet_type) :: sh4q
     integer(kind=kind(1)) :: ab,cd,aa,bb,cc,dd
     integer(kind=kind(1)) :: fa,fb,fc,fd,la,lb,lc,ld,start,step
     real(kind=kind(1.0d0)) :: factor,cutoff,P_max,IP_max
     logical(kind=kind(.true.)) :: skip

     J = 0.0d0
     K = 0.0d0
     cutoff = eri_cutoff_(self%scfdata)
     call create_(max_I,self%n_shell_pairs)
     call create_(max_P,self%n_shell_pairs)
     call make_max_abab_integrals_(self,max_I)
     call make_max_density_elements_(self,max_P,P)
     P_max = maxval(max_P)
     IP_max = maxval(max_I) * P_max
     start = 1 + this_proc_(tonto_parallel)
     step = n_proc_(tonto_parallel)
     do ab=start,self%n_shell_pairs,step
       if (max_I(ab)*IP_max < cutoff)  cycle    ! Rough version of Schwarz test, but quick.
       call get_shell_pair_indices_(self,ab,aa,bb,fa,la,fb,lb)  ! a & b shell indices.
       call set_precomp_shell_quartet_ab_(self,sh4q,aa,bb)
        !.set_shell_quartet_ab(sh4,aa,bb)
       do cd = 1,ab
         if (max_I(ab)*max_I(cd)*P_max < cutoff)  cycle  ! Rough version of Schwarz test, but quick.
         call get_shell_pair_indices_(self,cd,cc,dd,fc,lc,fd,ld)   ! c & d shell indices.
                                         ! ab|cd < sqrt(ab|ab) * sqrt(cd|cd) test.
         skip = schwarz_inequality_test_(self,cutoff,ab,cd,aa,bb,cc,dd,max_P,max_I)
         if (skip) cycle

         call set_precomp_shell_quartet_cd_(self,sh4q,cc,dd)
          !.set_shell_quartet_cd(sh4,cc,dd)
         if (aa==bb) then                    ! Evaluate the integrals'
           factor = 0.50d0                     ! coincidence factors
         else
           factor = 1.0d0
         end if
         if (cc==dd) factor = 0.50d0 * factor
         if (aa==cc .and. bb==dd) factor = 0.50d0 * factor
         call make_r_JK_(sh4q,J,K,P,factor,fa,la,fb,lb,fc,lc,fd,ld)
         call destroy_cd_(sh4q)
          !sh4.make_r_JK(J,K,P,factor,fa,la,fb,lb,fc,lc,fd,ld)
          !sh4.destroy_cd
       end do
        !sh4.destroy_ab
       call destroy_ab_(sh4q)
     end do
     call destroy_(max_P)
     call destroy_(max_I)
     call weight_diagonal_blocks_(self,J,2.0d0)
     call blockwise_symmetric_fold_(self,K)
     J = 2.0d0*J
     call sum_symmetric_matrices_(tonto_parallel,J)
     call sum_symmetric_matrices_(tonto_parallel,K)
     call symmetric_reflect_(J)
     call symmetric_reflect_(K)

   end subroutine

   subroutine make_r_JK_disk(self,J,K,P)
    type(mol_type) :: self
    ! Make the real coulomb matrix "J" and exchange matrix "K" matrix from a
    ! symmetric density matrix "P" from disk integral archive "eri_integrals"
     real(kind=kind(1.0d0)), dimension(:,:) :: J,K,P
     type(archive_type) :: eri_archive,ind_archive
     real(kind=kind(1.0d0)), dimension(:,:,:,:), pointer :: I
     integer(kind=kind(1)) :: q,a,b,c,d,fa,la,fb,lb,fc,lc,fd,ld,n_quartets
     real(kind=kind(1.0d0)) :: I_abcd,P_dc,P_db,P_cb

     call set_(eri_archive,self%name,"eri_integrals")
     call set_(ind_archive,self%name,"eri_index")
     call ensure_(tonto,self%basis_info_made,"MOL:make_r_JK_disk ... no basis info")
     call ensure_(tonto,associated(self%atom),"MOL:make_r_JK_disk ... no atom list")
     call ensure_(tonto,exists_(eri_archive),"MOL:make_r_JK_disk ... no integral file")
     call ensure_(tonto,exists_(ind_archive),"MOL:make_r_JK_disk ... no integral index file")
     call open_(eri_archive,for="read-only",buffered=.true.,type="real")
     call open_(ind_archive,for="read-only",buffered=.true.,type="int")
     J = 0.0d0
     K = 0.0d0
     n_quartets = n_shell_quartets_(self)
     do
       call read_(ind_archive%file,q)
       if (q > n_quartets) exit
       call get_shell_quartet_indices_(self,q,fa,la,fb,lb,fc,lc,fd,ld)
       call create_(I,fa,la,fb,lb,fc,lc,fd,ld)
       call read_(eri_archive%file,I)
       do d = fd,ld
         do c = fc,lc
           P_dc = P(d,c)
           do b = fb,lb
             P_db = P(d,b)
             P_cb = P(c,b)
             do a = fa,la
                I_abcd = I(a,b,c,d)
                J(a,b) = J(a,b) + I_abcd*P_dc
                J(c,d) = J(c,d) + I_abcd*P(b,a)
                K(a,c) = K(a,c) + I_abcd*P_db
                K(a,d) = K(a,d) + I_abcd*P_cb
                K(b,c) = K(b,c) + I_abcd*P(d,a)
                K(b,d) = K(b,d) + I_abcd*P(c,a)
             end do
           end do
         end do
       end do
       call destroy_(I)
     end do
     call close_(ind_archive)
     call close_(eri_archive)
     call weight_diagonal_blocks_(self,J,2.0d0)
     call blockwise_symmetric_fold_(self,K)
     call symmetric_reflect_(J)
     call symmetric_reflect_(K)
     J = 2.0d0*J

   end subroutine

   subroutine make_r_J_disk(self,J,P)
    type(mol_type) :: self
    ! Make the real coulomb matrix "J" from a
    ! symmetric density matrix "P" from disk integral archive "eri_integrals"
     real(kind=kind(1.0d0)), dimension(:,:) :: J,P
     type(archive_type) :: eri_archive,ind_archive
     real(kind=kind(1.0d0)), dimension(:,:,:,:), pointer :: I
     integer(kind=kind(1)) :: q,a,b,c,d,fa,la,fb,lb,fc,lc,fd,ld,n_quartets
     real(kind=kind(1.0d0)) :: I_abcd,P_dc,P_db,P_cb

   call ensure_(tonto,self%basis_info_made,"MOL:make_r_J_disk ... no basis info")
   call ensure_(tonto,associated(self%atom),"MOL:make_r_J_disk ... no atom list")
     call set_(eri_archive,self%name,"eri_integrals")
     call set_(ind_archive,self%name,"eri_index")
   call ensure_(tonto,exists_(eri_archive),"MOL:make_r_J_disk ... no integral file")
   call ensure_(tonto,exists_(ind_archive),"MOL:make_r_J_disk ... no integral index file")
     call open_(eri_archive,for="read-only",buffered=.true.,type="real")
     call open_(ind_archive,for="read-only",buffered=.true.,type="int")
     J = 0.0d0
     n_quartets = n_shell_quartets_(self)
     do
       call read_(ind_archive%file,q)
       if (q > n_quartets) exit
       call get_shell_quartet_indices_(self,q,fa,la,fb,lb,fc,lc,fd,ld)
       call create_(I,fa,la,fb,lb,fc,lc,fd,ld)
       call read_(eri_archive%file,I)
       do d = fd,ld
         do c = fc,lc
           P_dc = P(d,c)
           do b = fb,lb
             P_db = P(d,b)
             P_cb = P(c,b)
             do a = fa,la
                I_abcd = I(a,b,c,d)
                J(a,b) = J(a,b) + I_abcd*P_dc
                J(c,d) = J(c,d) + I_abcd*P(b,a)
             end do
           end do
         end do
       end do
       call destroy_(I)
     end do
     call close_(ind_archive)
     call close_(eri_archive)
     call weight_diagonal_blocks_(self,J,2.0d0)
     call symmetric_reflect_(J)
     J = 2.0d0*J

   end subroutine

   subroutine make_r_group_JK(self,J,K,P)
    type(mol_type) :: self
    ! Make the real coulomb matrix "J" and exchange matrix "K" matrix from the
    ! density matrix "P" only for basis functions which occur in the same
    ! atom_group.
     real(kind=kind(1.0d0)), dimension(:,:) :: J,K,P
     real(kind=kind(1.0d0)), dimension(:), pointer :: max_I,max_P
     type(shell4_type) :: sh4
     integer(kind=kind(1)) :: ab,cd, aa,bb,cc,dd
     integer(kind=kind(1)) :: fa,fb,fc,fd,la,lb,lc,ld
     integer(kind=kind(1)) :: atom_a,atom_b,atom_c,atom_d
     real(kind=kind(1.0d0)) :: factor,cutoff,P_max,IP_max
     logical(kind=kind(.true.)) :: skip

     J = 0.0d0
     K = 0.0d0
     cutoff = eri_cutoff_(self%scfdata)
     call create_(max_I,self%n_shell_pairs)
     call create_(max_P,self%n_shell_pairs)
     call make_max_abab_integrals_(self,max_I)
     call make_max_density_elements_(self,max_P,P)
     P_max = maxval(max_P)
     IP_max = maxval(max_I) * P_max
     do ab = 1, self%n_shell_pairs
       if (max_I(ab)*IP_max < cutoff)  cycle    ! Rough version of Schwarz test, but quick.
       call get_shell_pair_indices_(self,ab,aa,bb,fa,la,fb,lb,atom_a,atom_b)     ! a & b shell indices.
       call set_shell_quartet_ab_(self,sh4,aa,bb)
       do cd = 1,ab
         if (max_I(ab)*max_I(cd)*P_max < cutoff)  cycle  ! Rough version of Schwarz test, but quick.
         call get_shell_pair_indices_(self,cd,cc,dd,fc,lc,fd,ld,atom_c,atom_d)   ! c & d shell indices.
         if (.not. in_same_atom_group_(self,atom_a,atom_b,atom_c,atom_d)) cycle
                                         ! ab|cd < sqrt(ab|ab) * sqrt(cd|cd) test.
         skip = schwarz_inequality_test_(self,cutoff,ab,cd,aa,bb,cc,dd,max_P,max_I)
         if (skip) cycle
                                               ! calculate ab|cd
         call set_shell_quartet_cd_(self,sh4,cc,dd)
         factor = 1.0d0                          ! Evaluate the integrals'
         if (aa==bb) factor = 0.50d0           ! coincidence factors
         if (cc==dd) factor = 0.50d0 * factor
         if (aa==cc .and. bb==dd) factor = 0.50d0 * factor
         call make_r_JK_(sh4,J,K,P,factor,fa,la,fb,lb,fc,lc,fd,ld)
         call destroy_cd_(sh4)
       end do
       call destroy_ab_(sh4)
     end do
     call destroy_(max_P)
     call destroy_(max_I)
     call weight_diagonal_blocks_(self,J,2.0d0)
     call blockwise_symmetric_fold_(self,K)
     call symmetric_reflect_(J)
     call symmetric_reflect_(K)
     J = 2.0d0*J

   end subroutine

   subroutine make_r_JK_nosym(self,J,K,P)
    type(mol_type) :: self
    ! Make the real coulomb matrix "J" and exchange matrix "K" matrix from the
    ! density matrix "P" which has no permutational symmetry.
     real(kind=kind(1.0d0)), dimension(:,:) :: J,K,P
     real(kind=kind(1.0d0)), dimension(:,:,:,:), pointer :: I
     real(kind=kind(1.0d0)), dimension(:), pointer :: max_I,max_P
     type(shell4_type) :: sh4
     integer(kind=kind(1)) :: ab,cd,i_a,i_b,i_c,i_d
     integer(kind=kind(1)) :: a,b,c,d,fa,fb,fc,fd,la,lb,lc,ld
     real(kind=kind(1.0d0)) :: I_abcd,factor,cutoff
     logical(kind=kind(.true.)) :: skip

     J = 0.0d0
     K = 0.0d0
     cutoff = 1.0d-12
     call create_(max_I,self%n_shell_pairs)
     call create_(max_P,self%n_shell_pairs)
     call make_max_abab_integrals_(self,max_I)
     call make_max_density_elements_(self,max_P,P)
     do ab = 1, self%n_shell_pairs
       call get_shell_pair_indices_(self,ab,i_a,i_b)     ! a & b shell indices.
       fa = self%first_basis_fn_for_shell(i_a)
       fb = self%first_basis_fn_for_shell(i_b)
       la = self%last_basis_fn_for_shell(i_a)
       lb = self%last_basis_fn_for_shell(i_b)
       call set_shell_quartet_ab_(self,sh4,i_a,i_b)
       do cd = 1,ab
         call get_shell_pair_indices_(self,cd,i_c,i_d)   ! c & d shell indices.
         fc = self%first_basis_fn_for_shell(i_c)
         fd = self%first_basis_fn_for_shell(i_d)
         lc = self%last_basis_fn_for_shell(i_c)
         ld = self%last_basis_fn_for_shell(i_d)
                                         ! ab|cd < sqrt(ab|ab) * sqrt(cd|cd) test.
         skip = schwarz_inequality_test_(self,cutoff,ab,cd,i_a,i_b,i_c,i_d,max_P,max_I)
         if (skip) cycle
                                               ! calculate ab|cd
         call set_shell_quartet_cd_(self,sh4,i_c,i_d)
         factor = 1.0d0                          ! Evaluate the integrals'
         if (i_a==i_b) factor = 0.50d0           ! coincidence factors
         if (i_c==i_d) factor = 0.50d0 * factor
         if (i_a==i_c .and. i_b==i_d) factor = 0.50d0 * factor
         call create_(I,fa,la,fb,lb,fc,lc,fd,ld)
         call get_ERI_(sh4,I)
           do d = fd,ld
           do c = fc,lc
           do b = fb,lb
           do a = fa,la
            I_abcd = factor * I(a,b,c,d)
            J(a,b) = J(a,b) + I_abcd*P(d,c)  ! These are symmetric
            J(a,b) = J(a,b) + I_abcd*P(c,d)
            J(b,a) = J(b,a) + I_abcd*P(d,c)
            J(b,a) = J(b,a) + I_abcd*P(c,d)
            J(c,d) = J(c,d) + I_abcd*P(b,a)
            J(c,d) = J(c,d) + I_abcd*P(a,b)
            J(d,c) = J(d,c) + I_abcd*P(b,a)
            J(d,c) = J(d,c) + I_abcd*P(a,b)
            K(a,d) = K(a,d) + I_abcd*P(b,c)  ! These are asymmetric
            K(a,c) = K(a,c) + I_abcd*P(b,d)
            K(b,c) = K(b,c) + I_abcd*P(a,d)
            K(b,d) = K(b,d) + I_abcd*P(a,c)
            K(d,a) = K(d,a) + I_abcd*P(c,b)
            K(c,a) = K(c,a) + I_abcd*P(d,b)
            K(c,b) = K(c,b) + I_abcd*P(d,a)
            K(d,b) = K(d,b) + I_abcd*P(c,a)
           end do
           end do
           end do
           end do
         call destroy_(I)
         call destroy_cd_(sh4)
       end do
       call destroy_ab_(sh4)
     end do
     call destroy_(max_P)
     call destroy_(max_I)

   end subroutine

   subroutine make_r_J_direct(self,J,P)
    type(mol_type) :: self
    ! Make the real coulomb matrix "J" from a symmetric density matrix "P"
    ! directly.
     real(kind=kind(1.0d0)), dimension(:,:) :: J,P
     real(kind=kind(1.0d0)), dimension(:), pointer :: max_I,max_P
     type(shell4_type) :: sh4
     integer(kind=kind(1)) :: ab,cd,aa,bb,cc,dd
     integer(kind=kind(1)) :: fa,fb,fc,fd,la,lb,lc,ld,start,step
     real(kind=kind(1.0d0)) :: factor,cutoff,P_max,IP_max
     logical(kind=kind(.true.)) :: skip

     J = 0.0d0
     cutoff = eri_cutoff_(self%scfdata)
     call create_(max_I,self%n_shell_pairs)
     call create_(max_P,self%n_shell_pairs)
     call make_max_abab_integrals_(self,max_I)
     call make_max_density_elements_(self,max_P,P)
     P_max = maxval(max_P)
     IP_max = maxval(max_I) * P_max
     start = 1 + this_proc_(tonto_parallel)
     step = n_proc_(tonto_parallel)
     do ab = start, self%n_shell_pairs, step
       if (max_I(ab)*IP_max < cutoff)  cycle    ! Rough version of Schwarz test, but quick.
       call get_shell_pair_indices_(self,ab,aa,bb,fa,la,fb,lb)  ! a & b shell indices.
       call set_shell_quartet_ab_(self,sh4,aa,bb)
       do cd = 1,ab
         if (max_I(ab)*max_I(cd)*P_max < cutoff)  cycle  ! Rough version of Schwarz test, but quick.
         call get_shell_pair_indices_(self,cd,cc,dd,fc,lc,fd,ld)   ! c & d shell indices.
                                         ! ab|cd < sqrt(ab|ab) * sqrt(cd|cd) test.
         skip = schwarz_inequality_test_(self,cutoff,ab,cd,aa,bb,cc,dd,max_P,max_I)
         if (skip) cycle

         call set_shell_quartet_cd_(self,sh4,cc,dd)
         if (aa==bb) then                    ! Evaluate the integrals'
           factor = 0.50d0                     ! coincidence factors
         else
           factor = 1.0d0
         end if
         if (cc==dd) factor = 0.50d0 * factor
         if (aa==cc .and. bb==dd) factor = 0.50d0 * factor
         call make_r_J_(sh4,J,P,factor,fa,la,fb,lb,fc,lc,fd,ld)
         call destroy_cd_(sh4)
       end do
       call destroy_ab_(sh4)
     end do
     call destroy_(max_P)
     call destroy_(max_I)
     call weight_diagonal_blocks_(self,J,2.0d0)
     call sum_symmetric_matrices_(tonto_parallel,J)
     call symmetric_reflect_(J)
     J = 2.0d0*J

   end subroutine

   subroutine make_u_JK_direct(self,J,Ka,Kb,P,Pa,Pb)
    type(mol_type) :: self
    ! Make the real coulomb matrices "J" and exchange matrices "Ka" and "Kb"
    ! matrix from a symmetric density matrices "P", "Pa", and "Pb" directly.
     real(kind=kind(1.0d0)), dimension(:,:),target :: J,Ka,Kb,P,Pa,Pb
     real(kind=kind(1.0d0)), dimension(:,:,:,:), pointer :: I
     real(kind=kind(1.0d0)), dimension(:), pointer :: max_P,max_I
     type(shell4_type) :: sh4
!     sh4q :: type(shell1quartet_type)
     integer(kind=kind(1)) :: ab,cd, aa,bb,cc,dd
     integer(kind=kind(1)) :: a,b,c,d,fa,fb,fc,fd,la,lb,lc,ld
     real(kind=kind(1.0d0)) :: I_abcd,factor,Pa_db,Pa_cb,Pb_db,Pb_cb,cutoff
     real(kind=kind(1.0d0)) :: P_dc,Jcd,Ka_bc,Ka_bd,Kb_bc,Kb_bd
     logical(kind=kind(.true.)) :: skip

     J  = 0.0d0
     Ka = 0.0d0
     Kb = 0.0d0
     cutoff = eri_cutoff_(self%scfdata)
     call create_(max_P,self%n_shell_pairs)
     call make_max_density_elements_(self,max_P,P)
     call create_(max_I,self%n_shell_pairs )             ! Store the largest integral for each
     call make_max_abab_integrals_(self,max_I)           ! shell pair ab|ab
     do ab = 1, self%n_shell_pairs
       call get_shell_pair_indices_(self,ab,aa,bb,fa,la,fb,lb)    ! a & b shell indices.
!       .set_precomp_shell_quartet_ab(sh4q,aa,bb)
       call set_shell_quartet_ab_(self,sh4,aa,bb)
       do cd = 1,ab
         call get_shell_pair_indices_(self,cd,cc,dd,fc,lc,fd,ld)  ! c & d shell indices.
                                               ! ab|cd < sqrt(ab|ab) * sqrt(cd|cd) test.
         skip = schwarz_inequality_test_(self,cutoff,ab,cd,aa,bb,cc,dd,max_P,max_I)
         if (skip) cycle
                                               ! calculate ab|cd
         call set_shell_quartet_cd_(self,sh4,cc,dd)
!         .set_precomp_shell_quartet_cd(sh4q,cc,dd)
         factor = 1.0d0                          ! Evaluate the integrals'
         if (aa==bb) factor = 0.50d0             ! coincidence factors
         if (cc==dd) factor = 0.50d0 * factor
         if (aa==cc .and. bb==dd) factor = 0.50d0 * factor
         call create_(I,fa,la,fb,lb,fc,lc,fd,ld)
         call get_ERI_(sh4,I)
!         sh4q.get_ERI(I)
         do d = fd,ld
           do c = fc,lc
             P_dc = P(d,c)
             Jcd = 0.0d0
             do b = fb,lb
               Pa_db = Pa(d,b)
               Pb_db = Pb(d,b)
               Pa_cb = Pa(c,b)
               Pb_cb = Pb(c,b)
               Ka_bc = 0.0d0
               Kb_bc = 0.0d0
               Ka_bd = 0.0d0
               Kb_bd = 0.0d0
               do a = fa,la
                 I_abcd  = factor  * I(a,b,c,d)
                 J(a,b)  = J(a,b)  + I_abcd*P_dc
                 Jcd     = Jcd     + I_abcd*P(b,a)
                 Ka(a,c) = Ka(a,c) + I_abcd*Pa_db
                 Ka(a,d) = Ka(a,d) + I_abcd*Pa_cb
                 Ka_bc   = Ka_bc   + I_abcd*Pa(d,a)
                 Ka_bd   = Ka_bd   + I_abcd*Pa(c,a)
                 Kb(a,c) = Kb(a,c) + I_abcd*Pb_db
                 Kb(a,d) = Kb(a,d) + I_abcd*Pb_cb
                 Kb_bc   = Kb_bc   + I_abcd*Pb(d,a)
                 Kb_bd   = Kb_bd   + I_abcd*Pb(c,a)
               end do
               Ka(b,c) = Ka(b,c) + Ka_bc
               Ka(b,d) = Ka(b,d) + Ka_bd
               Kb(b,c) = Kb(b,c) + Kb_bc
               Kb(b,d) = Kb(b,d) + Kb_bd
             end do
             J(c,d) = J(c,d) + Jcd
           end do
         end do
         call destroy_(I)
         call destroy_cd_(sh4)
!         sh4q.destroy_cd
       end do
       call destroy_ab_(sh4)
!       sh4q.destroy_ab
     end do
     call destroy_(max_I)
     call destroy_(max_P)
     call weight_diagonal_blocks_(self,J,2.0d0)
     call blockwise_symmetric_fold_(self,Ka)
     call blockwise_symmetric_fold_(self,Kb)
     call symmetric_reflect_(J)
     call symmetric_reflect_(Ka)
     call symmetric_reflect_(Kb)
     J = 2.0d0*J

   end subroutine

   subroutine make_u_JK_disk(self,J,Ka,Kb,P,Pa,Pb)
    type(mol_type) :: self
    ! Make the real coulomb matrices "J" and exchange matrices "Ka" and "Kb"
    ! matrix from a symmetric density matrices "P", "Pa", and "Pb" directly.
     real(kind=kind(1.0d0)), dimension(:,:) :: J,Ka,Kb,P,Pa,Pb
     type(archive_type), pointer :: eri_archive,ind_archive
     real(kind=kind(1.0d0)), dimension(:,:,:,:), pointer :: I
     integer(kind=kind(1)) :: q,n_quartets,a,b,c,d,fa,fb,fc,fd,la,lb,lc,ld
     real(kind=kind(1.0d0)) :: I_abcd,P_dc,Pa_db,Pa_cb,Pb_db,Pb_cb

     call create_(eri_archive,self%name,"eri_integrals")
     call create_(ind_archive,self%name,"eri_index")
   call ensure_(tonto,self%basis_info_made,"MOL:make_u_JK_disk ... no basis info")
   call ensure_(tonto,associated(self%atom),"MOL:make_u_JK_disk ... no atom list")
   call ensure_(tonto,exists_(eri_archive),"MOL:make_u_JK_disk ... no integral file")
   call ensure_(tonto,exists_(ind_archive),"MOL:make_u_JK_disk ... no integral index file")
     call open_(eri_archive,for="read-only",buffered=.true.,type="real")
     call open_(ind_archive,for="read-only",buffered=.true.,type="int")
     J  = 0.0d0
     Ka = 0.0d0
     Kb = 0.0d0
     n_quartets = n_shell_quartets_(self)
     do
       call read_(ind_archive%file,q)
       if (q > n_quartets) exit
       call get_shell_quartet_indices_(self,q,fa,la,fb,lb,fc,lc,fd,ld)
       call create_(I,fa,la,fb,lb,fc,lc,fd,ld)
       call read_(eri_archive%file,I)
       do d = fd,ld
         do c = fc,lc
           P_dc = P(d,c)
           do b = fb,lb
             Pa_db = Pa(d,b)
             Pb_db = Pb(d,b)
             Pa_cb = Pa(c,b)
             Pb_cb = Pb(c,b)
             do a = fa,la
                I_abcd = I(a,b,c,d)
                J(a,b) = J(a,b) + I_abcd*P_dc
                J(c,d) = J(c,d) + I_abcd*P(b,a)
                Ka(a,c) = Ka(a,c) + I_abcd*Pa_db
                Ka(a,d) = Ka(a,d) + I_abcd*Pa_cb
                Ka(b,c) = Ka(b,c) + I_abcd*Pa(d,a)
                Ka(b,d) = Ka(b,d) + I_abcd*Pa(c,a)
                Kb(a,c) = Kb(a,c) + I_abcd*Pb_db
                Kb(a,d) = Kb(a,d) + I_abcd*Pb_cb
                Kb(b,c) = Kb(b,c) + I_abcd*Pb(d,a)
                Kb(b,d) = Kb(b,d) + I_abcd*Pb(c,a)
             end do
           end do
         end do
       end do
       call destroy_(I)
     end do
     call destroy_(ind_archive)
     call destroy_(eri_archive)
     call weight_diagonal_blocks_(self,J,2.0d0)
     call blockwise_symmetric_fold_(self,Ka)
     call blockwise_symmetric_fold_(self,Kb)
     call symmetric_reflect_(J)
     call symmetric_reflect_(Ka)
     call symmetric_reflect_(Kb)
     J = 2.0d0*J

   end subroutine

   subroutine make_gc_JK_direct(self,J,Ka,Kb,Kba,P,Pa,Pb,Pba)
    type(mol_type) :: self
    ! Make the general complex matrices "J" and exchange matrices
    ! "Ka", "Kb" and "Kba" from a density matrices "P", "Pa", "Pb" and "Pba"
    ! directly from the integrals.
     real(kind=kind(1.0d0)), dimension(:,:) :: J,P
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: Ka,Kb,Kba,Pa,Pb,Pba
      real(kind=kind(1.0d0)), dimension(:,:,:,:), pointer :: I
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: abs_P
     real(kind=kind(1.0d0)), dimension(:), pointer :: max_I,max_P
     type(shell4_type) :: sh4
     integer(kind=kind(1)) :: ab,cd,aa,bb,cc,dd
     integer(kind=kind(1)) :: a,b,c,d,fa,fb,fc,fd,la,lb,lc,ld
     real(kind=kind(1.0d0)) :: I_abcd,factor,cutoff
     logical(kind=kind(.true.)) :: skip

     J   = 0.0d0
     Ka  = 0.0d0
     Kb  = 0.0d0
     Kba = 0.0d0
     cutoff = eri_cutoff_(self%scfdata)
     call create_(abs_P,self%n_bf,self%n_bf)
     abs_P = max(abs(P),abs(Pa),abs(Pb),abs(Pb))
     call create_(max_P,self%n_shell_pairs)
     call make_max_density_elements_(self,max_P,abs_P)
     call create_(max_I,self%n_shell_pairs)              ! Store the largest integral for each
     call make_max_abab_integrals_(self,max_I)           ! shell pair ab|ab
     do ab = 1,self%n_shell_pairs
       call get_shell_pair_indices_(self,ab,aa,bb,fa,la,fb,lb)
       call set_shell_quartet_ab_(self,sh4,aa,bb)
       do cd = 1,ab
         call get_shell_pair_indices_(self,cd,cc,dd,fc,lc,fd,ld)
                                               ! ab|cd < sqrt(ab|ab) * sqrt(cd|cd) test.
         skip = schwarz_inequality_test_(self,cutoff,ab,cd,aa,bb,cc,dd,max_P,max_I)
         if (skip) cycle
                                               ! calculate ab|cd
         call set_shell_quartet_cd_(self,sh4,cc,dd)
         call create_(I,fa,la,fb,lb,fc,lc,fd,ld)
         call get_ERI_(sh4,I)
         factor = 1.0d0                              ! Evaluate the integrals
         if (aa==bb) factor = 0.50d0                 ! Coincidence factors
         if (cc==dd) factor = 0.50d0 * factor
         if (aa==cc .and. bb==dd) factor = 0.50d0 * factor
         I = factor * I
         do d = fd,ld
         do c = fc,lc
         do b = fb,lb
         do a = fa,la
            I_abcd = I(a,b,c,d)
            J(a,b)   = J(a,b)   + I_abcd*P(d,c)    ! These are symmetric
            J(c,d)   = J(c,d)   + I_abcd*P(b,a)
            Ka(a,d)  = Ka(a,d)  + I_abcd*Pa(b,c)   ! These are symmetric
            Ka(a,c)  = Ka(a,c)  + I_abcd*Pa(b,d)   ! but must be folded
            Ka(b,c)  = Ka(b,c)  + I_abcd*Pa(a,d)
            Ka(b,d)  = Ka(b,d)  + I_abcd*Pa(a,c)
            Kb(a,d)  = Kb(a,d)  + I_abcd*Pb(b,c)   ! These are symmetric
            Kb(a,c)  = Kb(a,c)  + I_abcd*Pb(b,d)   ! but must be folded
            Kb(b,c)  = Kb(b,c)  + I_abcd*Pb(a,d)
            Kb(b,d)  = Kb(b,d)  + I_abcd*Pb(a,c)
            Kba(a,d) = Kba(a,d) + I_abcd*Pba(b,c)  ! These are asymmetric
            Kba(a,c) = Kba(a,c) + I_abcd*Pba(b,d)
            Kba(b,c) = Kba(b,c) + I_abcd*Pba(a,d)
            Kba(b,d) = Kba(b,d) + I_abcd*Pba(a,c)
            Kba(d,a) = Kba(d,a) + I_abcd*Pba(c,b)
            Kba(c,a) = Kba(c,a) + I_abcd*Pba(d,b)
            Kba(c,b) = Kba(c,b) + I_abcd*Pba(d,a)
            Kba(d,b) = Kba(d,b) + I_abcd*Pba(c,a)
         end do
         end do
         end do
         end do
         call destroy_(I)
         call destroy_cd_(sh4)
       end do
       call destroy_ab_(sh4)
     end do
     call destroy_(max_I)
     call destroy_(max_P)
     call destroy_(abs_P)
     call weight_diagonal_blocks_(self,J,2.0d0)
     call blockwise_hermitian_fold_(self,Ka)
     call blockwise_hermitian_fold_(self,Kb)
     call symmetric_reflect_(J)
     call make_hermitian_(Ka)
     call make_hermitian_(Kb)
     J = 2.0d0*J

   end subroutine

   subroutine make_gc_JK_disk(self,J,Ka,Kb,Kba,P,Pa,Pb,Pba)
    type(mol_type) :: self
    ! Make the real coulomb matrices "J" and exchange matrices "Ka" and "Kb"
    ! matrix from a symmetric density matrices "P", "Pa", and "Pb" directly.
     real(kind=kind(1.0d0)), dimension(:,:) :: J,P
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: Ka,Kb,Kba,Pa,Pb,Pba
     type(archive_type), pointer :: eri_archive,ind_archive
     real(kind=kind(1.0d0)), dimension(:,:,:,:), pointer :: I
     integer(kind=kind(1)) :: q,n_quartets,a,b,c,d,fa,fb,fc,fd,la,lb,lc,ld
     real(kind=kind(1.0d0)) :: I_abcd

     call create_(eri_archive,self%name,"eri_integrals")
     call create_(ind_archive,self%name,"eri_index")
   call ensure_(tonto,self%basis_info_made,"MOL:make_gc_JK_disk ... no basis info")
   call ensure_(tonto,associated(self%atom),"MOL:make_gc_JK_disk ... no atom list")
   call ensure_(tonto,exists_(eri_archive),"MOL:make_gc_JK_disk ... no integral file")
   call ensure_(tonto,exists_(ind_archive),"MOL:make_gc_JK_disk ... no integral index file")
     call open_(eri_archive,for="read-only",buffered=.true.,type="real")
     call open_(ind_archive,for="read-only",buffered=.true.,type="int")
     J   = 0.0d0
     Ka  = 0.0d0
     Kb  = 0.0d0
     Kba = 0.0d0
     n_quartets = n_shell_quartets_(self)
     do
       call read_(ind_archive%file,q)
       if (q > n_quartets) exit
       call get_shell_quartet_indices_(self,q,fa,la,fb,lb,fc,lc,fd,ld)
      ! I.create(fa,la,fb,lb,fc,lc,fd,ld)
       allocate(I(fa:la,fb:lb,fc:lc,fd:ld))
       call read_(eri_archive%file,I)
       do d = fd,ld
       do c = fc,lc
       do b = fb,lb
       do a = fa,la
           I_abcd = I(a,b,c,d)
           J(a,b)   = J(a,b)   + I_abcd*P(d,c)    ! These are symmetric
           J(c,d)   = J(c,d)   + I_abcd*P(b,a)
           Ka(a,d)  = Ka(a,d)  + I_abcd*Pa(b,c)   ! These are symmetric but must be folded
           Ka(a,c)  = Ka(a,c)  + I_abcd*Pa(b,d)
           Ka(b,c)  = Ka(b,c)  + I_abcd*Pa(a,d)
           Ka(b,d)  = Ka(b,d)  + I_abcd*Pa(a,c)
           Kb(a,d)  = Kb(a,d)  + I_abcd*Pb(b,c)   ! These are symmetric but must be folded
           Kb(a,c)  = Kb(a,c)  + I_abcd*Pb(b,d)
           Kb(b,c)  = Kb(b,c)  + I_abcd*Pb(a,d)
           Kb(b,d)  = Kb(b,d)  + I_abcd*Pb(a,c)
           Kba(a,d) = Kba(a,d) + I_abcd*Pba(b,c)  ! These are asymmetric
           Kba(a,c) = Kba(a,c) + I_abcd*Pba(b,d)
           Kba(b,c) = Kba(b,c) + I_abcd*Pba(a,d)
           Kba(b,d) = Kba(b,d) + I_abcd*Pba(a,c)
           Kba(d,a) = Kba(d,a) + I_abcd*Pba(c,b)
           Kba(c,a) = Kba(c,a) + I_abcd*Pba(d,b)
           Kba(c,b) = Kba(c,b) + I_abcd*Pba(d,a)
           Kba(d,b) = Kba(d,b) + I_abcd*Pba(c,a)
       end do
       end do
       end do
       end do
      ! I.destroy
       deallocate(I)
     end do
     call destroy_(ind_archive)
     call destroy_(eri_archive)
     call weight_diagonal_blocks_(self,J,2.0d0)
     call blockwise_hermitian_fold_(self,Ka)
     call blockwise_hermitian_fold_(self,Kb)
     call symmetric_reflect_(J)
     call make_hermitian_(Ka)
     call make_hermitian_(Kb)
     J = 2.0d0*J

   end subroutine

   subroutine make_gc_so_JK_direct(self,JS,KS,JO,KO,P)
    type(mol_type) :: self
    ! Make the general complex spin orbit Classical and Exchange matrices.
    ! Make them directly from integrals on the fly.
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:,:,:) :: JS,KS,JO,KO
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:,:) :: P
     real(kind=kind(1.0d0)), dimension(:,:,:,:,:), pointer :: S,O
     integer(kind=kind(1)) :: ab,cd,aa,bb,cc,dd
     integer(kind=kind(1)) :: a,b,c,d,k,fa,fb,fc,fd,la,lb,lc,ld,m,n
     real(kind=kind(1.0d0)) :: S_abcd,O_abcd,factor
     type(shell4_type), pointer :: sh4

   call ensure_(tonto,self%basis_info_made,"MOL:make_gc_so_JK_direct ... no basis info")
   call ensure_(tonto,associated(self%atom),"MOL:make_gc_so_JK_direct ... no atom list")
     JS = 0.0d0; KS = 0.0d0; JO = 0.0d0; KO = 0.0d0
     call create_(sh4)
     do ab = 1,self%n_shell_pairs
       call get_shell_pair_indices_(self,ab,aa,bb,fa,la,fb,lb)
       do cd = 1,ab
         call get_shell_pair_indices_(self,cd,cc,dd,fc,lc,fd,ld)
         call get_shell_quartet_(self,sh4,aa,bb,cc,dd)
         if (skip_ERI_(sh4)) then  ! ??????
           call destroy_ptr_part_(sh4)
           cycle
         end if
         allocate(S(fa:la,fb:lb,fc:lc,fd:ld,3))
         allocate(O(fa:la,fb:lb,fc:lc,fd:ld,3))
         call make_spin_orbit_ints_(sh4,S,O)
         factor = 1.0d0                                    ! Evaluate the integrals
         if (aa==bb) factor = 0.50d0                       ! Coincidence factors
         if (cc==dd) factor = 0.50d0 * factor
         if (aa==cc .and. bb==dd) factor = 0.50d0 * factor
         S = factor * S
         O = factor * O
         do k = 1,3
         do a = fa,la
         do b = fb,lb
         do c = fc,lc
         do d = fd,ld
            S_abcd = S(a,b,c,d,k)
            O_abcd = O(a,b,c,d,k)
            do m = 1,2
            do n = 1,2
                !
               JS(a,b, m,n,k) = JS(a,b, m,n,k) + S_abcd*P(d,c, m,n)
               JS(a,b, m,n,k) = JS(a,b, m,n,k) + S_abcd*P(c,d, m,n)
               JS(b,a, m,n,k) = JS(b,a, m,n,k) - S_abcd*P(d,c, m,n)
               JS(b,a, m,n,k) = JS(b,a, m,n,k) - S_abcd*P(c,d, m,n)
               JS(c,d, m,n,k) = JS(c,d, m,n,k) + O_abcd*P(b,a, m,n)
               JS(c,d, m,n,k) = JS(c,d, m,n,k) + O_abcd*P(a,b, m,n)
               JS(d,c, m,n,k) = JS(d,c, m,n,k) - O_abcd*P(b,a, m,n)
               JS(d,c, m,n,k) = JS(d,c, m,n,k) - O_abcd*P(a,b, m,n)
                !
               JO(a,b, m,n,k) = JO(a,b, m,n,k) + O_abcd*P(d,c, m,n)
               JO(a,b, m,n,k) = JO(a,b, m,n,k) - O_abcd*P(c,d, m,n)
               JO(b,a, m,n,k) = JO(b,a, m,n,k) + O_abcd*P(d,c, m,n)
               JO(b,a, m,n,k) = JO(b,a, m,n,k) - O_abcd*P(c,d, m,n)
               JO(c,d, m,n,k) = JO(c,d, m,n,k) + S_abcd*P(b,a, m,n)
               JO(c,d, m,n,k) = JO(c,d, m,n,k) - S_abcd*P(a,b, m,n)
               JO(d,c, m,n,k) = JO(d,c, m,n,k) + S_abcd*P(b,a, m,n)
               JO(d,c, m,n,k) = JO(d,c, m,n,k) - S_abcd*P(a,b, m,n)
                !
               KS(a,d, m,n,k) = KS(a,d, m,n,k) + S_abcd*P(b,c, m,n)
               KS(a,c, m,n,k) = KS(a,c, m,n,k) + S_abcd*P(b,d, m,n)
               KS(b,d, m,n,k) = KS(b,d, m,n,k) - S_abcd*P(a,c, m,n)
               KS(b,c, m,n,k) = KS(b,c, m,n,k) - S_abcd*P(a,d, m,n)
               KS(c,b, m,n,k) = KS(c,b, m,n,k) + O_abcd*P(d,a, m,n)
               KS(c,a, m,n,k) = KS(c,a, m,n,k) + O_abcd*P(d,b, m,n)
               KS(d,b, m,n,k) = KS(d,b, m,n,k) - O_abcd*P(c,a, m,n)
               KS(d,a, m,n,k) = KS(d,a, m,n,k) - O_abcd*P(c,b, m,n)
                !
               KO(a,d, m,n,k) = KO(a,d, m,n,k) + O_abcd*P(b,c, m,n)
               KO(a,c, m,n,k) = KO(a,c, m,n,k) - O_abcd*P(b,d, m,n)
               KO(b,d, m,n,k) = KO(b,d, m,n,k) + O_abcd*P(a,c, m,n)
               KO(b,c, m,n,k) = KO(b,c, m,n,k) - O_abcd*P(a,d, m,n)
               KO(c,b, m,n,k) = KO(c,b, m,n,k) + S_abcd*P(d,a, m,n)
               KO(c,a, m,n,k) = KO(c,a, m,n,k) - S_abcd*P(d,b, m,n)
               KO(d,b, m,n,k) = KO(d,b, m,n,k) + S_abcd*P(c,a, m,n)
               KO(d,a, m,n,k) = KO(d,a, m,n,k) - S_abcd*P(c,b, m,n)
            end do
            end do
         end do
         end do
         end do
         end do
         end do
         deallocate(O)
         deallocate(S)
         call destroy_ptr_part_(sh4)
       end do
     end do
     call destroy_(sh4)

   end subroutine

   subroutine make_gc_so_JK_disk(self,JS,KS,JO,KO,P,component)
    type(mol_type) :: self
    ! Make the general complex spin orbit Classical and Exchange matrices
    ! for a particular "component", either "x", "y" or "z".
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:,:) :: JS,KS,JO,KO,P
     character(1) :: component
     type(archive_type) :: SO_archive,ind_archive
     real(kind=kind(1.0d0)), dimension(:,:,:,:), pointer :: S,O
     integer(kind=kind(1)) :: q,n_quartets
     integer(kind=kind(1)) :: a,b,c,d,fa,fb,fc,fd,la,lb,lc,ld,m,n
     real(kind=kind(1.0d0)) :: S_abcd,O_abcd

   call ensure_(tonto,self%basis_info_made,"MOL:make_gc_so_JK_disk ... no basis info")
   call ensure_(tonto,associated(self%atom),"MOL:make_gc_so_JK_disk ... no atom list")
     call set_(SO_archive,self%name,"SO"//component//"_integrals")
     call set_(ind_archive,self%name,"SO_indices")
   call ensure_(tonto,exists_(SO_archive),"MOL:make_gc_so_JK_disk ... no spin orbit integral file!")
   call ensure_(tonto,exists_(ind_archive),"MOL:make_gc_so_JK_disk ... no spin orbit integral index file")
     call open_(SO_archive,for="read-only",buffered=.true.,type="real")
     call open_(ind_archive,for="read-only",buffered=.true.,type="int")
     JS = 0.0d0; KS = 0.0d0; JO = 0.0d0; KO = 0.0d0
     n_quartets = n_shell_quartets_(self)
     do
        call read_(ind_archive%file,q)
        if (q > n_quartets) exit
        call get_shell_quartet_indices_(self,q,fa,la,fb,lb,fc,lc,fd,ld)
        call create_(S,fa,la,fb,lb,fc,lc,fd,ld)
        call create_(O,fa,la,fb,lb,fc,lc,fd,ld)
        call read_(SO_archive%file,S)
        call read_(SO_archive%file,O)
        do a = fa,la
        do b = fb,lb
        do c = fc,lc
        do d = fd,ld
           S_abcd = S(a,b,c,d)
           O_abcd = O(a,b,c,d)
           do m = 1,2
           do n = 1,2
               !
              JS(a,b, m,n) = JS(a,b, m,n) + S_abcd*P(d,c, m,n)
              JS(a,b, m,n) = JS(a,b, m,n) + S_abcd*P(c,d, m,n)
              JS(b,a, m,n) = JS(b,a, m,n) - S_abcd*P(d,c, m,n)
              JS(b,a, m,n) = JS(b,a, m,n) - S_abcd*P(c,d, m,n)
              JS(c,d, m,n) = JS(c,d, m,n) + O_abcd*P(b,a, m,n)
              JS(c,d, m,n) = JS(c,d, m,n) + O_abcd*P(a,b, m,n)
              JS(d,c, m,n) = JS(d,c, m,n) - O_abcd*P(b,a, m,n)
              JS(d,c, m,n) = JS(d,c, m,n) - O_abcd*P(a,b, m,n)
               !
              JO(a,b, m,n) = JO(a,b, m,n) + O_abcd*P(d,c, m,n)
              JO(a,b, m,n) = JO(a,b, m,n) - O_abcd*P(c,d, m,n)
              JO(b,a, m,n) = JO(b,a, m,n) + O_abcd*P(d,c, m,n)
              JO(b,a, m,n) = JO(b,a, m,n) - O_abcd*P(c,d, m,n)
              JO(c,d, m,n) = JO(c,d, m,n) + S_abcd*P(b,a, m,n)
              JO(c,d, m,n) = JO(c,d, m,n) - S_abcd*P(a,b, m,n)
              JO(d,c, m,n) = JO(d,c, m,n) + S_abcd*P(b,a, m,n)
              JO(d,c, m,n) = JO(d,c, m,n) - S_abcd*P(a,b, m,n)
               !
              KS(a,d, m,n) = KS(a,d, m,n) + S_abcd*P(b,c, m,n)
              KS(a,c, m,n) = KS(a,c, m,n) + S_abcd*P(b,d, m,n)
              KS(b,d, m,n) = KS(b,d, m,n) - S_abcd*P(a,c, m,n)
              KS(b,c, m,n) = KS(b,c, m,n) - S_abcd*P(a,d, m,n)
              KS(c,b, m,n) = KS(c,b, m,n) + O_abcd*P(d,a, m,n)
              KS(c,a, m,n) = KS(c,a, m,n) + O_abcd*P(d,b, m,n)
              KS(d,b, m,n) = KS(d,b, m,n) - O_abcd*P(c,a, m,n)
              KS(d,a, m,n) = KS(d,a, m,n) - O_abcd*P(c,b, m,n)
               !
              KO(a,d, m,n) = KO(a,d, m,n) + O_abcd*P(b,c, m,n)
              KO(a,c, m,n) = KO(a,c, m,n) - O_abcd*P(b,d, m,n)
              KO(b,d, m,n) = KO(b,d, m,n) + O_abcd*P(a,c, m,n)
              KO(b,c, m,n) = KO(b,c, m,n) - O_abcd*P(a,d, m,n)
              KO(c,b, m,n) = KO(c,b, m,n) + S_abcd*P(d,a, m,n)
              KO(c,a, m,n) = KO(c,a, m,n) - S_abcd*P(d,b, m,n)
              KO(d,b, m,n) = KO(d,b, m,n) + S_abcd*P(c,a, m,n)
              KO(d,a, m,n) = KO(d,a, m,n) - S_abcd*P(c,b, m,n)
           end do
           end do
        end do
        end do
        end do
        end do
        call destroy_(O); call destroy_(S)
     end do
     call close_(SO_archive)
     call close_(ind_archive)

   end subroutine

   pure subroutine weight_diagonal_blocks(self,X,fac)
    type(mol_type) :: self
    ! Weight the diagonal blocks of matrix "X" by "fac"
      intent(in) :: self
      real(kind=kind(1.0d0)), dimension(:,:), intent(inout) :: X
      real(kind=kind(1.0d0)), intent(in) :: fac
      integer(kind=kind(1)) :: n,f,l
      do n = 1,self%n_shell
         f = self%first_basis_fn_for_shell(n)
         l = self%last_basis_fn_for_shell(n)
         X(f:l,f:l) = fac*X(f:l,f:l)
      end do

   end subroutine

   pure subroutine weight_diagonal_blocks_1(self,X,fac)
    type(mol_type) :: self
    ! Weight the diagonal blocks of matrix "X" by "fac"
      intent(in) :: self
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), intent(inout) :: X
      real(kind=kind(1.0d0)), intent(in) :: fac
      integer(kind=kind(1)) :: n,f,l
      do n = 1,self%n_shell
         f = self%first_basis_fn_for_shell(n)
         l = self%last_basis_fn_for_shell(n)
         X(f:l,f:l) = fac*X(f:l,f:l)
      end do

   end subroutine

   pure subroutine set_diagonal_blocks(self,X,fac)
    type(mol_type) :: self
    ! Set the diagonal blocks of matrix "X" to "fac"
      intent(in) :: self
      real(kind=kind(1.0d0)), dimension(:,:), intent(inout) :: X
      real(kind=kind(1.0d0)), intent(in) :: fac
      integer(kind=kind(1)) :: n,f,l
      do n = 1,self%n_shell
         f = self%first_basis_fn_for_shell(n)
         l = self%last_basis_fn_for_shell(n)
         X(f:l,f:l) = fac
      end do

   end subroutine

   subroutine blockwise_symmetric_fold(self,X)
    type(mol_type) :: self
    ! Symmetrically fold the matrix "X" blockwise
      real(kind=kind(1.0d0)), dimension(:,:) :: X
      integer(kind=kind(1)) :: a,fa,la,b,fb,lb

      do a = 1,self%n_shell
      do b = 1,a
         fa = self%first_basis_fn_for_shell(a)
         la = self%last_basis_fn_for_shell(a)
         fb = self%first_basis_fn_for_shell(b)
         lb = self%last_basis_fn_for_shell(b)
         X(fa:la,fb:lb) = X(fa:la,fb:lb) + transpose(X(fb:lb,fa:la))
      end do
      end do

   end subroutine

   subroutine blockwise_hermitian_fold(self,X)
    type(mol_type) :: self
    ! Hermitian fold the matrix "X" blockwise
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: X
      integer(kind=kind(1)) :: a,fa,la,b,fb,lb

      do a = 1,self%n_shell
      do b = 1,a
         fa = self%first_basis_fn_for_shell(a)
         la = self%last_basis_fn_for_shell(a)
         fb = self%first_basis_fn_for_shell(b)
         lb = self%last_basis_fn_for_shell(b)
         X(fa:la,fb:lb) = X(fa:la,fb:lb) + transpose(conjg(X(fb:lb,fa:la)))
      end do
      end do

   end subroutine

!  *******************
!  Pairwise SCF energy
!  *******************

   subroutine put_scf_energy_in_mo_pairs(self)
    type(mol_type) :: self
    ! Analyse the SCF energy into MO pairs
      type(opmatrix_type), pointer :: Di
      real(kind=kind(1.0d0)), dimension(:), pointer :: core_energy
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: pair_energy
      integer(kind=kind(1)) :: i,j
      real(kind=kind(1.0d0)) :: energy
      logical(kind=kind(.true.)) :: direct

      call create_(core_energy,self%n_a)
      call create_(pair_energy,self%n_a,self%n_a)
      core_energy = 0.0d0
      pair_energy = 0.0d0
      call create_(Di,self%n_bf)
      direct = self%scfdata%direct
      self%scfdata%direct = .true.
      do i = 1,self%n_a
         call make_scf_density_matrix_(self,i)
         call set_to_(Di,self%density_matrix)
         call make_fock_matrix_(self,self%density_matrix,self%fock_matrix,core=.true.,r12=.false.)
         core_energy(i) = scf_electronic_energy_(self,Di,core=.true.)
         do j = 1,i
            call make_scf_density_matrix_(self,j)
            call make_fock_matrix_(self,self%density_matrix,self%fock_matrix,core=.false.,r12=.true.)
            if (i==j) then  ! fac = 1.0d0
              pair_energy(i,j) = scf_electronic_energy_(self,Di,core=.false.)
            else            ! fac = 2.0d0
              pair_energy(i,j) = 2.0d0*scf_electronic_energy_(self,Di,core=.false.)
            end if
         end do
      end do
      call destroy_(Di)
      self%scfdata%direct = direct
      call flush_(stdout)
      call text_(stdout,"SCF MO pair energy decomposition:",flush=1)
      energy = sum(core_energy)+sum(transfer(pair_energy,(/1.0d0/)))+nuclear_energy_(self)
      call show_(stdout,"SCF energy =",energy)
      call text_(stdout,"Core energies:",flush=1)
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=1)
      call put_(stdout,"MO_i",int_width=.true.)
      call put_(stdout,"Core energy")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=1)
      do i = 1,self%n_a
         call put_(stdout,i)
         call put_(stdout,core_energy(i),flush=1)
      end do
      call dash_(stdout,int_fields=1,real_fields=1)
      call flush_(stdout)
      call text_(stdout,"Orbital pair energies:",flush=1)
      call dash_(stdout,int_fields=2,real_fields=1)
      call put_(stdout,"MO_i",int_width=.true.)
      call put_(stdout,"MO_j",int_width=.true.)
      call put_(stdout,"Pair energy",flush=1)
      call dash_(stdout,int_fields=2,real_fields=1)
      do i = 1,self%n_a
      do j = 1,i
         call put_(stdout,i)
         call put_(stdout,j)
         call put_(stdout,pair_energy(i,j),flush=1)
      end do
      end do
      call dash_(stdout,int_fields=2,real_fields=1)
      call flush_(stdout)

   end subroutine

!  ************************
!  SCF Energy decomposition
!  ************************

   subroutine put_AO_energy_partition(self)
    type(mol_type) :: self
    ! Put out the AO energy partition
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: Y

      call create_(Y,self%n_bf,self%n_bf)
      call to_unit_matrix_(Y)
      call put_energy_partition_(self,Y)
      call destroy_(Y)

   end subroutine

   subroutine put_SAO_energy_partition(self)
    type(mol_type) :: self
    ! Put out the symmetric AO energy partition
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: S,Smh

      call create_(S,self%n_bf,self%n_bf); call get_overlap_matrix_(self,S)
      call create_(Smh,self%n_bf,self%n_bf); call to_inverse_sqrt_(Smh,S)
      call put_energy_partition_(self,Smh)
      call destroy_(Smh)
      call destroy_(S)

   end subroutine

   subroutine put_MO_energy_partition(self)
    type(mol_type) :: self
    ! Put out the MO energy partition. NOTE: the MO's must be ordered
    ! contiguously, first group 1 then group 2 MO's, AND the atoms in each group
    ! must be contiguous in the .atom list, first group 1 then group 2. This is
    ! because the atom_group_AO_subspace_set routine is used, and by abuse of its
    ! function with the above cobstraints, it will do the right thing and copy
    ! quadrants.

   call ensure_(tonto,associated(self%molecular_orbitals),"MOL:put_MO_energy_partition ... no MO's")
   call ensure_(tonto,associated(self%molecular_orbitals%restricted),"MOL:put_MO_energy_partition ... no restricted MO's")
      call put_energy_partition_(self,self%molecular_orbitals%restricted)

   end subroutine

   subroutine put_energy_partition(self,Y)
    type(mol_type) :: self
    ! Put out the energy decomposition specified in the .atom_group array.
    ! The inverse of matrix "Y" is used to back transform the density matrix
    ! (which is taken from disk). The back transformed density matrix is then
    ! partitioned, and transformed back in order to evaluate the interaction
    ! energies between the partitioned systems.
      real(kind=kind(1.0d0)), dimension(:,:) :: Y
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: X, T,Z,ZZ,C,K,P,PP,W,E_T,E_Z, EE
      real(kind=kind(1.0d0)), dimension(:,:,:), pointer :: E_ZZ
      real(kind=kind(1.0d0)), dimension(:,:,:,:), pointer :: E_C,E_K
      integer(kind=kind(1)) :: n_group,g,h,i,j
      real(kind=kind(1.0d0)) :: fac
      character(128) :: unit
      type(archive_type) :: arch
      real(kind=kind(1.0d0)) :: kinetic_energy,nuclear_attraction,nuclear_repulsion
      real(kind=kind(1.0d0)) :: coulomb_repulsion,net_coulomb,bicentric_exchange
      real(kind=kind(1.0d0)) :: exchange_attraction,total_interaction

   call ensure_(tonto,associated(self%atom_group),"MOL:put_energy_partition ... no atom group information")
   call ensure_(tonto,size(self%atom_group)==2,"MOL:put_energy_partition ... must have only 2 groups")
   call ensure_(tonto,size(Y,1)==self%n_bf,"MOL:put_energy_partition ... incorrect size for Y array")
      call set_(arch,self%name,"density_matrix,restricted")
   call ensure_(tonto,exists_(arch),"MOL:put_energy_partition ... no density matrix")
      call create_(P,self%n_bf,self%n_bf)
      call create_(PP,self%n_bf,self%n_bf)
      call read_(arch,P)
      call create_(W,self%n_bf,self%n_bf)
      call create_(T,self%n_bf,self%n_bf); call get_kinetic_matrix_(self,T)
      call create_(Z,self%n_bf,self%n_bf); call get_nuclear_matrix_(self,Z)
      call create_(X,self%n_bf,self%n_bf); call to_inverse_of_(X,Y)
      n_group = size(self%atom_group)
      call create_(E_T,n_group,n_group); E_T = 0.0d0
      call create_(E_Z,n_group,n_group); E_Z = 0.0d0
      call create_(E_C,n_group,n_group,n_group,n_group); E_C = 0.0d0
      call create_(E_K,n_group,n_group,n_group,n_group); E_K = 0.0d0
      call create_(E_ZZ,n_group,n_group,n_group); E_ZZ = 0.0d0
      call create_(ZZ,self%n_bf,self%n_bf)
      PP = P
      call back_transform_(PP,X)
      call destroy_(X)
         do i = 1,n_group
         do j = 1,i
            W = 0.0d0
            call atom_group_AO_subspace_set_(self,W,PP,i,j)
            if (i/=j) &
            call atom_group_AO_subspace_set_(self,W,PP,j,i)
            call back_transform_(W,Y)
            E_T(i,j) = trace_product_with_(T,W)
            E_Z(i,j) = trace_product_with_(Z,W)
            do g = 1,n_group
               call make_nuclear_matrix_(self,ZZ,self%atom_group(g)%element)
               E_ZZ(i,j,g) = trace_product_with_(ZZ,W)
            end do
         end do
         end do
      call destroy_(ZZ)
      call create_(C,self%n_bf,self%n_bf)
      call create_(K,self%n_bf,self%n_bf)
      do g = 1,n_group
      do h = 1,g
         W = 0.0d0
         call atom_group_AO_subspace_set_(self,W,PP,g,h)
         if (g/=h) &
         call atom_group_AO_subspace_set_(self,W,PP,h,g)
         call back_transform_(W,Y)
         call make_r_JK_nosym_(self,C,K,W)
         do i = 1,n_group
         do j = 1,i
            W = 0.0d0
            call atom_group_AO_subspace_set_(self,W,PP,i,j)
            if (i/=j) &
            call atom_group_AO_subspace_set_(self,W,PP,j,i)
            call back_transform_(W,Y)
            E_C(i,j,g,h) =  trace_product_with_(C,W)
            E_K(i,j,g,h) = -trace_product_with_(K,W)
         end do
         end do
      end do
      end do
      call destroy_(K)
      call destroy_(C)
      E_C = 0.50d0*E_C
      E_K = 0.50d0*0.50d0*E_K
      call create_(EE,n_group,n_group)
       !
      call text_(stdout," ")
      fac = sum(E_T+E_Z) + sum(E_C+E_K)
      call show_(stdout,"SCF electronic energy =",fac)
      call show_(stdout,"SCF energy            =",fac+nuclear_energy_(self))
      call show_(stdout,"Kinetic energy        =",sum(E_T))
       !
      unit = "kcal/mol"
      fac = conversion_factor_(unit)
      E_T = fac*E_T
      E_Z = fac*E_Z
      E_C = fac*E_C
      E_K = fac*E_K
      E_ZZ = fac*E_ZZ
      call text_(stdout," ")
      call text_(stdout,"Energy decomposition in kcal/mol ...")
      call text_(stdout," ")
      call text_(stdout,"Kinetic interaction terms")
      call put_(stdout,E_T)
      call text_(stdout," ")
      call text_(stdout,"Nuclear attraction interaction terms")
      call put_(stdout,E_Z)
      call text_(stdout," ")
      call text_(stdout,"Nuclear attraction interaction terms ONLY for group 1 nuclei")
      call put_(stdout,E_ZZ(:,:,1))
      call text_(stdout," ")
      call text_(stdout,"Nuclear attraction interaction terms ONLY for group 2 nuclei")
      call put_(stdout,E_ZZ(:,:,2))
      EE(1,1) = fac*nuclear_energy_(self%atom(self%atom_group(1)%element))
      EE(2,2) = fac*nuclear_energy_(self%atom(self%atom_group(2)%element))
      nuclear_repulsion = fac*nuclear_energy_(self) - EE(1,1) - EE(2,2)
      EE(2,1) = nuclear_repulsion
      EE(1,2) = 0.0d0
      call text_(stdout," ")
      call text_(stdout,"Nuclear nuclear repulsion")
      call put_(stdout,EE)
      EE(1,1) = E_C(1,1,1,1)
      EE(2,1) = E_C(2,2,1,1)
      EE(1,2) = E_C(1,1,2,2)
      EE(2,2) = E_C(2,2,2,2)
      call text_(stdout," ")
      call text_(stdout,"Diagonal Coulomb repulsion interaction terms")
      call put_(stdout,EE)
      EE(1,1) = E_C(1,2,1,1)
      EE(2,1) = E_C(2,1,1,1)
      EE(1,2) = E_C(1,2,2,2)
      EE(2,2) = E_C(2,1,2,2)
      call text_(stdout," ")
      call text_(stdout,"Semi diagonal Coulomb repulsion interaction terms")
      call put_(stdout,EE)
      EE(1,1) = E_C(1,2,1,2)
      EE(2,1) = E_C(2,1,1,2)
      EE(1,2) = E_C(1,2,2,1)
      EE(2,2) = E_C(2,1,2,1)
      call text_(stdout," ")
      call text_(stdout,"Off diagonal Coulomb repulsion interaction terms")
      call put_(stdout,EE)
      call text_(stdout," ")
      call text_(stdout,"All Coulomb repulsion interaction terms")
      call put_(stdout,E_C)
      EE(1,1) = E_K(1,1,1,1)
      EE(2,1) = E_K(2,2,1,1)
      EE(1,2) = E_K(1,1,2,2)
      EE(2,2) = E_K(2,2,2,2)
      call text_(stdout," ")
      call text_(stdout,"Diagonal exchange interaction terms")
      call put_(stdout,EE)
      EE(1,1) = E_K(1,2,1,1)
      EE(2,1) = E_K(2,1,1,1)
      EE(1,2) = E_K(1,2,2,2)
      EE(2,2) = E_K(2,1,2,2)
      call text_(stdout," ")
      call text_(stdout,"Semi diagonal exchange interaction terms")
      call put_(stdout,EE)
      EE(1,1) = E_K(1,2,1,2)
      EE(2,1) = E_K(2,1,1,2)
      EE(1,2) = E_K(1,2,2,1)
      EE(2,2) = E_K(2,1,2,1)
      call text_(stdout," ")
      call text_(stdout,"Off diagonal exchange interaction terms")
      call put_(stdout,EE)
      call text_(stdout," ")
      call text_(stdout,"All exchange interaction terms")
      call put_(stdout,E_K)

      kinetic_energy = E_T(2,1)
      nuclear_attraction = sum(E_Z) - E_ZZ(1,1,1) - E_ZZ(2,2,2)
      coulomb_repulsion = sum(E_C) - E_C(1,1,1,1) - E_C(2,2,2,2)
      net_coulomb = nuclear_attraction + nuclear_repulsion + coulomb_repulsion
      bicentric_exchange = E_K(1,1,2,2) + E_K(2,2,1,1)
      exchange_attraction = sum(E_K) - E_K(1,1,1,1) - E_K(2,2,2,2)
      total_interaction = kinetic_energy + net_coulomb + exchange_attraction
      call text_(stdout," ")
      call text_(stdout,"Interaction energies for "//trim(self%name)//":")
      call text_(stdout," ")
      call show_(stdout,"Kinetic energy     =",kinetic_energy)
      call show_(stdout,"Exchange energy    =",exchange_attraction)
      call show_(stdout,"Kinetic + Exhange  =",kinetic_energy+exchange_attraction)
      call show_(stdout,"Bicentric Exchange =",bicentric_exchange)
      call show_(stdout,"Net Coulomb energy =",net_coulomb)
      call show_(stdout,"Total int. energy  =",total_interaction)

      kinetic_energy = E_T(1,1)
      exchange_attraction = E_K(1,1,1,1)
      net_coulomb = E_ZZ(1,1,1) + fac*nuclear_energy_(self%atom(self%atom_group(1)%element)) + E_C(1,1,1,1)
      total_interaction = kinetic_energy + net_coulomb + exchange_attraction
      call text_(stdout," ")
      call text_(stdout,"Monomer 1 energies:")
      call text_(stdout," ")
      call show_(stdout,"Kinetic energy 1   =",kinetic_energy)
      call show_(stdout,"Exchange energy 1  =",exchange_attraction)
      call show_(stdout,"Kinetic+Exhange 1  =",kinetic_energy+exchange_attraction)
      call show_(stdout,"Net Coulomb 1      =",net_coulomb)
      call show_(stdout,"Total 1            =",total_interaction)

      kinetic_energy = E_T(2,2)
      exchange_attraction = E_K(2,2,2,2)
      net_coulomb = E_ZZ(2,2,2) + fac*nuclear_energy_(self%atom(self%atom_group(2)%element)) + E_C(2,2,2,2)
      total_interaction = kinetic_energy + net_coulomb + exchange_attraction
      call text_(stdout," ")
      call text_(stdout,"Monomer 2 energies:")
      call text_(stdout," ")
      call show_(stdout,"Kinetic energy 2   =",kinetic_energy)
      call show_(stdout,"Exchange energy 2  =",exchange_attraction)
      call show_(stdout,"Kinetic+Exhange 2  =",kinetic_energy+exchange_attraction)
      call show_(stdout,"Net Coulomb 2      =",net_coulomb)
      call show_(stdout,"Total 2            =",total_interaction)

      call destroy_(EE)
      call destroy_(E_ZZ)
      call destroy_(E_K)
      call destroy_(E_C)
      call destroy_(E_Z)
      call destroy_(E_T)
      call destroy_(Z)
      call destroy_(T)
      call destroy_(W)
      call destroy_(PP)
      call destroy_(P)

   end subroutine

!   put_roby_energy_partition
!   ! Put out the energy decomposition specified in the .atom_group array.
!   ! The inverse of matrix "Y" is used to back transform the density matrix
!   ! (which is taken from disk). The back transformed density matrix is then
!   ! partitioned, and transformed back in order to evaluate the interaction
!   ! energies between the partitioned systems.
!      unit :: STR
!      arch :: type(archive_type)
!      E_T,E_T_A,E_T_B, E_Z,E_Z_A,E_Z_B :: real(kind=kind(1.0d0))
!      E_C,E_C_A,E_C_B, E_K,E_K_A,E_K_B, E_N,E_N_A,E_N_B :: real(kind=kind(1.0d0))
!      kinetic_energy,nuclear_attraction,nuclear_repulsion :: real(kind=kind(1.0d0))
!      coulomb_repulsion,net_coulomb :: real(kind=kind(1.0d0))
!      exchange_attraction,total_interaction :: real(kind=kind(1.0d0))
!      energy, promotion_energy, fac :: real(kind=kind(1.0d0))
!      P,P_A,P_B,MO,OM,Q,T,Z,C,K :: REALMAT*
!      n,f,l :: integer(kind=kind(1))
!      call ensure_(tonto,.atom_group.created,"no atom group information")
!      call ensure_(tonto,size(.atom_group)==2,"must have only 2 groups")
!      arch.set(.name,"density_matrix,restricted")
!      call ensure_(tonto,arch.exists,"no density matrix")
!      P.create(.n_bf,.n_bf)
!      arch.read(P) ! Read the density from disk
!      !
!      .make_group_density(MOs=.true.)
!  ! stdout.text(" ")
!  ! stdout.text("WARING, no symorthonormilsation")
!      .symorthonormalise_occupied_MOs
!      !
!      P_A.create(.n_bf,.n_bf)
!      P_B.create(.n_bf,.n_bf)
!      MO.create(.n_bf,.n_bf)
!      OM.create(.n_bf,.n_bf)
!      Q.create(.n_bf,.n_bf)
!      MO = .molecular_orbitals.restricted
!      OM.to_inverse_of(MO)
!      P.back_transform(Q,OM)
!      n = .occupation_numbers.restricted(1:).index_of_first_zero_value - 1
!    stdout.text(" ")
!    stdout.show("n = ",n)
!      P_A = 0.0d0
!      P_A(1:n,1:n) = Q(1:n,1:n)
!    stdout.text("P_A :")
!    stdout.put(P_A)
!      P_A.back_transform(MO)
!      f = .occupation_numbers.restricted(n+1:).index_of_first_nonzero_value
!      f = n + f
!      l = .occupation_numbers.restricted(f:).index_of_first_zero_value - 1
!      l = f + l - 1
!    stdout.show("f = ",f)
!    stdout.show("l = ",l)
!      P_B = 0.0d0
!      P_B(f:l,f:l) = Q(f:l,f:l)
!    stdout.text("P_B :")
!    stdout.put(P_B)
!      P_B.back_transform(MO)
!    stdout.text("Q :")
!    stdout.put(Q)
!      Q.destroy
!      OM.destroy
!      MO.destroy
!      !
!      unit = "kcal/mol"
!      fac = unit.conversion_factor
!      !
!      T.create(.n_bf,.n_bf)
!      .get_kinetic_matrix(T)
!      E_T   = T.trace_product_with(P)*fac
!      E_T_A = T.trace_product_with(P_A)*fac
!      E_T_B = T.trace_product_with(P_B)*fac
!      T.destroy
!      !
!      Z.create(.n_bf,.n_bf)
!      .get_nuclear_matrix(Z)
!      E_Z   = Z.trace_product_with(P)*fac
!      .make_nuclear_matrix(Z,.atom_group(1).element)
!      E_Z_A = Z.trace_product_with(P_A)*fac
!      .make_nuclear_matrix(Z,.atom_group(2).element)
!      E_Z_B = Z.trace_product_with(P_B)*fac
!      Z.destroy
!      !
!      C.create(.n_bf,.n_bf)
!      K.create(.n_bf,.n_bf)
!      .make_r_JK_nosym(C,K,P)
!      E_C   = 0.50d0*C.trace_product_with(P)*fac
!      E_K   = -0.25d0*K.trace_product_with(P)*fac
!      .make_r_JK_nosym(C,K,P_A)
!      E_C_A = 0.50d0*C.trace_product_with(P_A)*fac
!      E_K_A = -0.25d0*K.trace_product_with(P_A)*fac
!      .make_r_JK_nosym(C,K,P_B)
!      E_C_B = 0.50d0*C.trace_product_with(P_B)*fac
!      E_K_B = -0.25d0*K.trace_product_with(P_B)*fac
!      K.destroy
!      C.destroy
!      P_B.destroy
!      P_A.destroy
!      P.destroy
!      !
!      E_N   = .nuclear_energy*fac
!      E_N_A = .atom(.atom_group(1).element).nuclear_energy*fac
!      E_N_B = .atom(.atom_group(2).element).nuclear_energy*fac
!      stdout.text(" ")
!      !
!      energy              = E_T + E_Z + E_C + E_K
!      kinetic_energy      = E_T - E_T_A - E_T_B
!      nuclear_attraction  = E_Z - E_Z_A - E_Z_B
!      nuclear_repulsion   = E_N - E_N_A - E_N_B
!      coulomb_repulsion   = E_C - E_C_A - E_C_B
!      net_coulomb         = nuclear_attraction + nuclear_repulsion + coulomb_repulsion
!      exchange_attraction = E_K - E_K_A - E_K_B
!      total_interaction   = kinetic_energy + net_coulomb + exchange_attraction
!      !
!      stdout.text(" ")
!      stdout.show("SCF electronic energy =",(energy/fac))
!      stdout.show("SCF energy            =",(energy/fac)+.nuclear_energy)
!      stdout.show("Kinetic energy        =",(E_T/fac))
!      stdout.text(" ")
!      stdout.text("Roby energy decomposition in kcal/mol ...")
!      stdout.text(" ")
!      stdout.text("Interaction energies for "//trim(.name)//":")
!      stdout.text(" ")
!      stdout.show("Energy             =",energy)
!      stdout.show("Kinetic energy     =",kinetic_energy)
!      stdout.show("Exchange energy    =",exchange_attraction)
!      stdout.show("Kinetic + Exhange  =",kinetic_energy+exchange_attraction)
!      stdout.show("Net Coulomb energy =",net_coulomb)
!      stdout.show("Total int. energy  =",total_interaction)
!      !
!      kinetic_energy      = E_T_A
!      exchange_attraction = E_K_A
!      net_coulomb         = E_Z_A + E_N_A + E_C_A
!      total_interaction   = kinetic_energy + net_coulomb + exchange_attraction
!      promotion_energy    = total_interaction - .atom_group_energy(1)*fac
!      stdout.text(" ")
!      stdout.text("Monomer 1 energies:")
!      stdout.text(" ")
!      stdout.show("Kinetic energy 1   =",kinetic_energy)
!      stdout.show("Exchange energy 1  =",exchange_attraction)
!      stdout.show("Kinetic+Exhange 1  =",kinetic_energy+exchange_attraction)
!      stdout.show("Net Coulomb 1      =",net_coulomb)
!      stdout.show("Total 1            =",total_interaction)
!      stdout.show("Promotion 1        =",promotion_energy)
!      !
!      kinetic_energy      = E_T_B
!      exchange_attraction = E_K_B
!      net_coulomb         = E_Z_B + E_N_B + E_C_B
!      total_interaction   = kinetic_energy + net_coulomb + exchange_attraction
!      promotion_energy    = total_interaction - .atom_group_energy(2)*fac
!      stdout.text(" ")
!      stdout.text("Monomer 2 energies:")
!      stdout.text(" ")
!      stdout.show("Kinetic energy 2   =",kinetic_energy)
!      stdout.show("Exchange energy 2  =",exchange_attraction)
!      stdout.show("Kinetic+Exhange 2  =",kinetic_energy+exchange_attraction)
!      stdout.show("Net Coulomb 2      =",net_coulomb)
!      stdout.show("Total 2            =",total_interaction)
!      stdout.show("Promotion 2        =",promotion_energy)
!   end

   subroutine put_roby_smo_energy_partition(self)
    type(mol_type) :: self
    ! Put out the roby energy decomposition specified in the .atom_group array
    ! based on symmetrically orthonormalised monomer molecular (SOMMO) fragments.
    ! A supermolecue density matrix must exist on disk.

      call put_roby_energy_partition_(self,symorthonormalise=.true.)

   end subroutine

   subroutine put_roby_mmo_energy_partition(self)
    type(mol_type) :: self
    ! Put out the roby energy decomposition specified in the .atom_group array
    ! based on monomer molecular (MMO) fragments.  A supermolecue density matrix
    ! must exist on disk.

      call put_roby_energy_partition_(self,symorthonormalise=.false.)

   end subroutine

   subroutine put_roby_energy_partition(self,symorthonormalise)
    type(mol_type) :: self
    ! Calculate and put out the roby energy decomposition specified in the
    ! .atom_group array.  If "symorthonormalise" is .true., then the occupied
    ! monomer molecular orbitals are symmetrically orthonormalised in morder to
    ! define the fragments. The density matrix defining the supermolecule is read
    ! in from the disk, so an SCF calculation or a "make_promol_density_matrix"
    ! calculation must be done before this routine is called.
      logical(kind=kind(.true.)) :: symorthonormalise
      character(128) :: unit
      type(archive_type) :: arch
      real(kind=kind(1.0d0)) :: E_T,E_T_A,E_T_B, E_Z,E_Z_A,E_Z_B
      real(kind=kind(1.0d0)) :: E0_A,E0_B, E0_T_A,E0_T_B, E0_Z_A,E0_Z_B
      real(kind=kind(1.0d0)) :: E_C,E_C_A,E_C_B, E_K,E_K_A,E_K_B, E_N,E_N_A,E_N_B
      real(kind=kind(1.0d0)) :: E0_C_A,E0_C_B, E0_K_A,E0_K_B
      real(kind=kind(1.0d0)) :: E_kin,E_att,E_rep,E_coul,E_net_coul,E_ex,E_int,E_prom,energy, fac
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: P,P_A,P_B,MO,OM,Q,T,Z,C,K
      integer(kind=kind(1)) :: n,f,l

   call ensure_(tonto,associated(self%atom_group),"MOL:put_roby_energy_partition ... no atom group information")
   call ensure_(tonto,size(self%atom_group)==2,"MOL:put_roby_energy_partition ... must have only 2 groups")
      call set_(arch,self%name,"density_matrix,restricted")
   call ensure_(tonto,exists_(arch),"MOL:put_roby_energy_partition ... no density matrix")
      unit = "kcal/mol"
      fac = conversion_factor_(unit)
       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      call create_(P,self%n_bf,self%n_bf)
      call read_(arch,P)                    ! *** Read the density from disk
      call make_group_density_(self,MOs=.true.)   ! *** Make the monomer molecular orbitals
      call make_group_energies_(self,1,E0_A,E0_T_A,E0_Z_A,E0_C_A,E0_K_A,fac)
      call make_group_energies_(self,2,E0_B,E0_T_B,E0_Z_B,E0_C_B,E0_K_B,fac)
       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if (symorthonormalise) then
         call text_(stdout," ")
         call text_(stdout,"performing symorthonormalisation")
         call symorthonormalise_occupied_MOs_(self)
      else
         call text_(stdout," ")
         call text_(stdout,"WARNING: no symorthonormalisation")
      end if
      call create_(P_A,self%n_bf,self%n_bf)
      call create_(P_B,self%n_bf,self%n_bf)
      call create_(MO,self%n_bf,self%n_bf)
      call create_(OM,self%n_bf,self%n_bf)
      call create_(Q,self%n_bf,self%n_bf)
      MO = self%molecular_orbitals%restricted
      call to_inverse_of_(OM,MO)
      call back_transform_(P,Q,OM)
      n = index_of_first_zero_value_(self%occupation_numbers%restricted(1:)) - 1
    call text_(stdout," ")
    call show_(stdout,"n = ",n)
      P_A = 0.0d0
      P_A(1:n,1:n) = Q(1:n,1:n)
    call text_(stdout,"P_A :")
    call put_(stdout,P_A)
      call back_transform_(P_A,MO)
      f = index_of_first_nonzero_value_(self%occupation_numbers%restricted(n+1:))
      f = n + f
      l = index_of_first_zero_value_(self%occupation_numbers%restricted(f:)) - 1
      l = f + l - 1
    call show_(stdout,"f = ",f)
    call show_(stdout,"l = ",l)
      P_B = 0.0d0
      P_B(f:l,f:l) = Q(f:l,f:l)
    call text_(stdout,"P_B :")
    call put_(stdout,P_B)
      call back_transform_(P_B,MO)
    call text_(stdout,"Q :")
    call put_(stdout,Q)
      call destroy_(Q)
      call destroy_(OM)
      call destroy_(MO)
       !
      call create_(T,self%n_bf,self%n_bf)
      call get_kinetic_matrix_(self,T)
      E_T   = trace_product_with_(T,P)*fac
      E_T_A = trace_product_with_(T,P_A)*fac
      E_T_B = trace_product_with_(T,P_B)*fac
      call destroy_(T)
       !
      call create_(Z,self%n_bf,self%n_bf)
      call get_nuclear_matrix_(self,Z)
      E_Z   = trace_product_with_(Z,P)*fac
      call make_nuclear_matrix_(self,Z,self%atom_group(1)%element)
      E_Z_A = trace_product_with_(Z,P_A)*fac
      call make_nuclear_matrix_(self,Z,self%atom_group(2)%element)
      E_Z_B = trace_product_with_(Z,P_B)*fac
      call destroy_(Z)
       !
      call create_(C,self%n_bf,self%n_bf)
      call create_(K,self%n_bf,self%n_bf)
      call make_r_JK_nosym_(self,C,K,P)
      E_C   = 0.50d0*trace_product_with_(C,P)*fac
      E_K   = -0.25d0*trace_product_with_(K,P)*fac
      call make_r_JK_nosym_(self,C,K,P_A)
      E_C_A = 0.50d0*trace_product_with_(C,P_A)*fac
      E_K_A = -0.25d0*trace_product_with_(K,P_A)*fac
      call make_r_JK_nosym_(self,C,K,P_B)
      E_C_B = 0.50d0*trace_product_with_(C,P_B)*fac
      E_K_B = -0.25d0*trace_product_with_(K,P_B)*fac
      call destroy_(K)
      call destroy_(C)
      call destroy_(P_B)
      call destroy_(P_A)
      call destroy_(P)
       !
      E_N   = nuclear_energy_(self)*fac
      E_N_A = nuclear_energy_(self%atom(self%atom_group(1)%element))*fac
      E_N_B = nuclear_energy_(self%atom(self%atom_group(2)%element))*fac
      call text_(stdout," ")
       !
      call text_(stdout," ")
      call text_(stdout,"Supermolecule energies:")
      call text_(stdout," ")
      energy     = E_T + E_Z + E_C + E_K
      call show_(stdout,"SCF electronic energy =",(energy/fac))
      call show_(stdout,"... in kcal/mol       =",energy)
      call show_(stdout,"SCF energy            =",(energy/fac)+nuclear_energy_(self))
      call show_(stdout,"... in kcal/mol       =",energy+fac*nuclear_energy_(self))
      call show_(stdout,"Kinetic energy        =",(E_T/fac))
      call show_(stdout,"... in kcal/mol       =",(E_T/fac))
      call text_(stdout," ")
      call text_(stdout,"Roby interaction energy decomposition (kcal/mol):")
      E_kin      = E_T - E_T_A - E_T_B
      E_ex       = E_K - E_K_A - E_K_B
      E_att      = E_Z - E_Z_A - E_Z_B
      E_rep      = E_N - E_N_A - E_N_B
      E_coul     = E_C - E_C_A - E_C_B
      E_net_coul = E_att + E_rep + E_coul
      E_int      = E_kin + E_net_coul + E_ex
      call put_roby_energy_parts_(self,E_kin,E_ex,E_coul,E_att,E_rep,E_int)
       !
      call text_(stdout," ")
      call text_(stdout,"Interacting Monomer 1 energies (kcal/mol):")
      call text_(stdout," ")
      E_int      = E_T_A + (E_Z_A + E_N_A + E_C_A) + E_K_A
      E_prom     = E_T_A + (E_Z_A + E_N_A + E_C_A) - self%atom_group_energy(1)*fac
      call put_roby_energy_parts_(self,E_T_A,E_K_A,E_C_A,E_Z_A,E_N_A,E_int,E_prom)
       !
      call text_(stdout," ")
      call text_(stdout,"Isolated Monomer 1 energies (kcal/mol):")
      call text_(stdout," ")
      call show_(stdout,"Monomer 1 energy   =",self%atom_group_energy(1))
      call show_(stdout,"... in k/cal mol   =",self%atom_group_energy(1)*fac)
      call text_(stdout," ")
      call put_roby_energy_parts_(self,E0_T_A,E0_K_A,E0_C_A,E0_Z_A,E_N_A)
       !
      call text_(stdout," ")
      call text_(stdout,"Changes in monomer 1 energy contributions (kcal/mol):")
      E_kin      = E_T_A - E0_T_A
      E_ex       = E_K_A - E0_K_A
      E_coul     = E_C_A - E0_C_A
      E_att      = E_Z_A - E0_Z_A             ! nuclear attraction
      E_rep      = 0.0d0                       ! change in nuclear repulsion is zero
      E_net_coul = E_att + E_rep + E_coul
      E_prom     = E_kin + E_net_coul + E_ex
      call put_roby_energy_parts_(self,E_kin,E_ex,E_coul,E_att,E_rep,E_prom=E_prom)
       !
      call text_(stdout," ")
      call text_(stdout,"Interacting Monomer 2 energies (kcal/mol):")
      call text_(stdout," ")
      E_int      = E_T_B + (E_Z_B + E_N_B + E_C_B) + E_K_B
      E_prom     = E_T_B + (E_Z_B + E_N_B + E_C_B) - self%atom_group_energy(2)*fac
      call put_roby_energy_parts_(self,E_T_B,E_K_B,E_C_B,E_Z_B,E_N_B,E_int,E_prom)
       !
      call text_(stdout," ")
      call text_(stdout,"Isolated Monomer 2 energies (kcal/mol):")
      call text_(stdout," ")
      call show_(stdout,"Monomer 2 energy   =",self%atom_group_energy(2))
      call show_(stdout,"... in k/cal mol   =",self%atom_group_energy(2)*fac)
      call text_(stdout," ")
      call put_roby_energy_parts_(self,E0_T_B,E0_K_B,E0_C_B,E0_Z_B,E_N_B)
       !
      call text_(stdout," ")
      call text_(stdout,"Changes in monomer 2 energy contributions (kcal/mol):")
      E_kin      = E_T_B - E0_T_B
      E_ex       = E_K_B - E0_K_B
      E_coul     = E_C_B - E0_C_B
      E_att      = E_Z_B - E0_Z_B             ! nuclear attraction
      E_rep      = 0.0d0                       ! change in nuclear repulsion is zero
      E_net_coul = E_att + E_rep + E_coul
      E_prom     = E_kin + E_net_coul + E_ex
      call put_roby_energy_parts_(self,E_kin,E_ex,E_coul,E_att,E_rep,E_prom=E_prom)

   end subroutine

   subroutine put_roby_energy_parts(self,E_T,E_K,E_C,E_Z,E_N,E_int,E_prom)
    type(mol_type) :: self
    ! Put of the Roby energy parts
      real(kind=kind(1.0d0)) :: E_T,E_K,E_C,E_Z,E_N
      real(kind=kind(1.0d0)), optional :: E_int,E_prom

      call text_(stdout," ")
      call show_(stdout,"Kinetic energy       =",E_T)
      call show_(stdout,"Exchange energy      =",E_K)
      call show_(stdout,"Kinetic + Exhange    =",E_T+E_K)
      call show_(stdout,"Nuclear attraction   =",E_Z)
      call show_(stdout,"Nuclear repulsion    =",E_N)
      call show_(stdout,"Electronic repulsion =",E_C)
      call show_(stdout,"Net coulomb energy   =",E_Z+E_N+E_C)
      if (present(E_int)) &
      call show_(stdout,"Total int. energy    =",E_int)
      if (present(E_prom)) &
      call show_(stdout,"Promotion energy     =",E_prom)

   end subroutine

   subroutine atom_group_AO_subspace_set(self,P,Q,row_group,col_group)
    type(mol_type) :: self
    ! Set P=Q only for the block whose rows correspond to basis functions
    ! on the atoms in the atom group "row_group" and whose columns are
    ! on the atoms in the atom group "col_group", as given in the
    ! .atom_group array vector.
     real(kind=kind(1.0d0)), dimension(:,:) :: P,Q
     integer(kind=kind(1)) :: row_group,col_group
     integer(kind=kind(1)) :: i_a,i_b,a,b,fa,fb,la,lb

   call ensure_(tonto,size(P,1)==self%n_bf,"MOL:atom_group_AO_subspace_set ... wrong size for P")
   call ensure_(tonto,size(P,2)==self%n_bf,"MOL:atom_group_AO_subspace_set ... wrong size for P")
   call ensure_(tonto,size(Q,1)==self%n_bf,"MOL:atom_group_AO_subspace_set ... wrong size for Q")
   call ensure_(tonto,size(Q,2)==self%n_bf,"MOL:atom_group_AO_subspace_set ... wrong size for Q")
   call ensure_(tonto,associated(self%atom_group),"MOL:atom_group_AO_subspace_set ... no atom group information")
   call ensure_(tonto,row_group<=size(self%atom_group),"MOL:atom_group_AO_subspace_set ... no such row group")
   call ensure_(tonto,col_group<=size(self%atom_group),"MOL:atom_group_AO_subspace_set ... no such column group")
   call ensure_(tonto,row_group>0,"MOL:atom_group_AO_subspace_set ... row group index must be positive")
   call ensure_(tonto,col_group>0,"MOL:atom_group_AO_subspace_set ... column group index must be positive")
     do i_a = 1,size(self%atom_group(row_group)%element)
     do i_b = 1,size(self%atom_group(col_group)%element)
       a = self%atom_group(row_group)%element(i_a)
       b = self%atom_group(col_group)%element(i_b)
       fa = self%first_basis_fn_for_atom(a)
       fb = self%first_basis_fn_for_atom(b)
       la = self%last_basis_fn_for_atom(a)
       lb = self%last_basis_fn_for_atom(b)
       P(fa:la,fb:lb) = Q(fa:la,fb:lb)
     end do
     end do

   end subroutine

   subroutine AO_subspace_set(self,A,B,row_atom,col_atom)
    type(mol_type) :: self
    ! Set "A" equal to the AO subspace blocks of "B" specified by the atom
    ! indices in "row_atom" and "col_atom". If either is missing, then copy
    ! the entire row or col, i.e.  A(small) = B(row_atom,col_atom)
      real(kind=kind(1.0d0)), dimension(:,:) :: A,B
      integer(kind=kind(1)), dimension(:), optional :: row_atom,col_atom
      integer(kind=kind(1)) :: n_row_atoms,n_col_atoms, a1,a2
      integer(kind=kind(1)) :: i,b_i,f_i,l_i,n_i, j,b_j,f_j,l_j,n_j

   call ensure_(tonto,size(B,1)==self%n_bf,"MOL:AO_subspace_set ... B has wrong shape")
   call ensure_(tonto,size(B,2)==self%n_bf,"MOL:AO_subspace_set ... B has wrong shape")
      if (present(row_atom) .and. present(col_atom)) then
         n_row_atoms = size(row_atom)
         n_col_atoms = size(col_atom)
         a1 = n_bf_(self%atom(row_atom))
         a2 = n_bf_(self%atom(col_atom))
         call ensure_(tonto,size(A,1)==a1,"MOL:AO_subspace_set ... A has wrong shape")
         call ensure_(tonto,size(A,2)==a2,"MOL:AO_subspace_set ... A has wrong shape")
         b_i = 0
         do i = 1,n_row_atoms
            f_i = self%first_basis_fn_for_atom(row_atom(i))
            l_i = self%last_basis_fn_for_atom( row_atom(i))
            n_i = n_bf_(self%atom(row_atom(i)))
            b_j = 0
            do j = 1,n_col_atoms
               f_j = self%first_basis_fn_for_atom(col_atom(j))
               l_j = self%last_basis_fn_for_atom( col_atom(j))
               n_j = n_bf_(self%atom(col_atom(j)))
               A(b_i+1:b_i+n_i,b_j+1:b_j+n_j) = B(f_i:l_i,f_j:l_j)
               b_j = b_j + n_j
            end do
            b_i = b_i + n_i
         end do
      else if (present(row_atom)) then
         n_row_atoms = size(row_atom)
         a1 = n_bf_(self%atom(row_atom))
         a2 = self%n_bf
         call ensure_(tonto,size(A,1)==a1,"MOL:AO_subspace_set ... A has wrong shape")
         call ensure_(tonto,size(A,2)==a2,"MOL:AO_subspace_set ... A has wrong shape")
         b_i = 0
         do i = 1,n_row_atoms
            f_i = self%first_basis_fn_for_atom(row_atom(i));
            l_i = self%last_basis_fn_for_atom( row_atom(i))
            n_i = n_bf_(self%atom(row_atom(i)))
            A(b_i+1:b_i+n_i,:) = B(f_i:l_i,:)
            b_i = b_i + n_i
         end do
      else if (present(col_atom)) then
         n_col_atoms = size(col_atom)
         a1 = self%n_bf
         a2 = n_bf_(self%atom(col_atom))
         call ensure_(tonto,size(A,1)==a1,"MOL:AO_subspace_set ... A has wrong shape")
         call ensure_(tonto,size(A,2)==a2,"MOL:AO_subspace_set ... A has wrong shape")
         b_j = 0
         do j = 1,n_col_atoms
            f_j = self%first_basis_fn_for_atom(col_atom(j));
            l_j = self%last_basis_fn_for_atom( col_atom(j))
            n_j = n_bf_(self%atom(col_atom(j)))
            A(:,b_j+1:b_j+n_j) = B(:,f_j:l_j)
            b_j = b_j + n_j
         end do
      end if

   end subroutine

   subroutine AO_subspace_zero(self,B,row_atom,col_atom)
    type(mol_type) :: self
    ! Zero out the AO subspace blocks of "B" specified by the atom
    ! indices in "row_atom" and "col_atom". If either is missing, then zero
    ! the entire row or col, i.e.  B(row_atom,col_atom) = 0
      real(kind=kind(1.0d0)), dimension(:,:) :: B
      integer(kind=kind(1)), dimension(:), optional :: row_atom,col_atom
      integer(kind=kind(1)) :: n_row_atoms,n_col_atoms,a1,a2
      integer(kind=kind(1)) :: i,b_i,f_i,l_i,n_i, j,b_j,f_j,l_j,n_j

   call ensure_(tonto,size(B,1)==self%n_bf,"MOL:AO_subspace_zero ... B has wrong shape")
   call ensure_(tonto,size(B,2)==self%n_bf,"MOL:AO_subspace_zero ... B has wrong shape")
      if (present(row_atom) .and. present(col_atom)) then
         n_row_atoms = size(row_atom)
         n_col_atoms = size(col_atom)
         a1 = n_bf_(self%atom(row_atom))
         a2 = n_bf_(self%atom(col_atom))
         b_i = 0
         do i = 1,n_row_atoms
            f_i = self%first_basis_fn_for_atom(row_atom(i))
            l_i = self%last_basis_fn_for_atom( row_atom(i))
            n_i = n_bf_(self%atom(row_atom(i)))
            b_j = 0
            do j = 1,n_col_atoms
               f_j = self%first_basis_fn_for_atom(col_atom(j))
               l_j = self%last_basis_fn_for_atom( col_atom(j))
               n_j = n_bf_(self%atom(col_atom(j)))
               B(f_i:l_i,f_j:l_j) = 0.0d0
               b_j = b_j + n_j
            end do
            b_i = b_i + n_i
         end do
      else if (present(row_atom)) then
         n_row_atoms = size(row_atom)
         a1 = n_bf_(self%atom(row_atom))
         a2 = self%n_bf
         b_i = 0
         do i = 1,n_row_atoms
            f_i = self%first_basis_fn_for_atom(row_atom(i));
            l_i = self%last_basis_fn_for_atom( row_atom(i))
            n_i = n_bf_(self%atom(row_atom(i)))
            B(f_i:l_i,:) = 0.0d0
            b_i = b_i + n_i
         end do
      else if (present(col_atom)) then
         n_col_atoms = size(col_atom)
         a1 = self%n_bf
         a2 = n_bf_(self%atom(col_atom))
         b_j = 0
         do j = 1,n_col_atoms
            f_j = self%first_basis_fn_for_atom(col_atom(j));
            l_j = self%last_basis_fn_for_atom( col_atom(j))
            n_j = n_bf_(self%atom(col_atom(j)))
            B(:,f_j:l_j) = 0.0d0
            b_j = b_j + n_j
         end do
      end if

   end subroutine

   subroutine AO_subspace_put(self,B,A,row_atom,col_atom)
    type(mol_type) :: self
    ! Set the AO subspace blocks of "A" specified by the atom indices in
    ! "row_atom" and "col_atom" equal to "B". If either is missing then copy
    ! the entire row or column, i.e.  A(row_atom,col_atom) = A() + B(small)
    ! WARNING **** This adds into A, and uncopied blocks are .not. set to zero.
      real(kind=kind(1.0d0)), dimension(:,:) :: A,B
      integer(kind=kind(1)), dimension(:), optional :: row_atom,col_atom
      integer(kind=kind(1)) :: n_row_atoms,n_col_atoms,b1,b2
      integer(kind=kind(1)) :: i,b_i,f_i,l_i,n_i, j,b_j,f_j,l_j,n_j

   call ensure_(tonto,size(A,1)==self%n_bf,"MOL:AO_subspace_put ... A has wrong shape")
   call ensure_(tonto,size(A,2)==self%n_bf,"MOL:AO_subspace_put ... A has wrong shape")
      if (present(row_atom) .and. present(col_atom)) then
         n_row_atoms = size(row_atom)
         n_col_atoms = size(col_atom)
         b1 = n_bf_(self%atom(row_atom))
         b2 = n_bf_(self%atom(col_atom))
         call ensure_(tonto,size(B,1)==b1,"MOL:AO_subspace_put ... B has wrong shape")
         call ensure_(tonto,size(B,2)==b2,"MOL:AO_subspace_put ... B has wrong shape")
         b_i = 0
         do i = 1,n_row_atoms
            f_i = self%first_basis_fn_for_atom(row_atom(i))
            l_i = self%last_basis_fn_for_atom( row_atom(i))
            n_i = n_bf_(self%atom(row_atom(i)))
            b_j = 0
            do j = 1,n_col_atoms
               f_j = self%first_basis_fn_for_atom(col_atom(j))
               l_j = self%last_basis_fn_for_atom( col_atom(j))
               n_j = n_bf_(self%atom(col_atom(j)))
               A(f_i:l_i,f_j:l_j) = A(f_i:l_i,f_j:l_j) + B(b_i+1:b_i+n_i,b_j+1:b_j+n_j)
               b_j = b_j + n_j
            end do
            b_i = b_i + n_i
         end do
      else if (present(row_atom)) then
         n_row_atoms = size(row_atom)
         b1 = n_bf_(self%atom(row_atom))
         b2 = self%n_bf
         call ensure_(tonto,size(B,1)==b1,"MOL:AO_subspace_put ... B has wrong shape")
         call ensure_(tonto,size(B,2)==b2,"MOL:AO_subspace_put ... B has wrong shape")
         b_i = 0
         do i = 1,n_row_atoms
            f_i = self%first_basis_fn_for_atom(row_atom(i));
            l_i = self%last_basis_fn_for_atom( row_atom(i))
            n_i = n_bf_(self%atom(row_atom(i)))
            A(f_i:l_i,:) = A(f_i:l_i,:) + B(b_i+1:b_i+n_i,:)
            b_i = b_i + n_i
         end do
      else if (present(col_atom)) then
         n_col_atoms = size(col_atom)
         b1 = n_bf_(self%atom(col_atom))
         b2 = self%n_bf
         call ensure_(tonto,size(B,1)==b1,"MOL:AO_subspace_put ... B has wrong shape")
         call ensure_(tonto,size(B,2)==b2,"MOL:AO_subspace_put ... B has wrong shape")
         b_j = 0
         do j = 1,n_col_atoms
            f_j = self%first_basis_fn_for_atom(col_atom(j));
            l_j = self%last_basis_fn_for_atom( col_atom(j))
            n_j = n_bf_(self%atom(col_atom(j)))
            A(:,f_j:l_j) = A(:,f_j:l_j) + B(:,b_j+1:b_j+n_j)
            b_j = b_j + n_j
         end do
      end if

   end subroutine

!  ******************
!  Constraint methods
!  ******************

   subroutine constrained_scf(self)
    type(mol_type) :: self
    ! Do a constrained SCF calculation.
    ! The following objects are produced as results:
    !   .molecular_orbitals, .orbital_energies, .density_matrix
     character(128) :: scf_kind
     type(archive_type) :: archive

     call ensure_(tonto,self%basis_info_made,"MOL:constrained_scf ... no basis info")
     call ensure_(tonto,associated(self%atom),"MOL:constrained_scf ... no atom list")
     scf_kind = self%scfdata%scf_kind
     if (scf_kind == "dft" .and. .not. associated(self%dftgrid)) then
       call create_(self%dftgrid)
       call set_defaults_(self%dftgrid)
     end if
     self%scfdata%using_rough_convergence = .false.
     call get_initial_guess_(self)
     call make_fock_matrix_(self)
     call make_constraint_data_(self)
     call initialise_scfdata_(self)
     call add_constraint_(self)
     call initialise_scfdata_error_(self)
     call cleanup_(self%scfdata%diis)
     do
       call put_scf_results_(self)         ! Banner and 0th iteration results
       do
         call extrapolate_fock_matrix_(self)
         call update_molecular_orbitals_(self)
         call schmidt_orthonormalise_MOs_(self)
         call make_scf_density_matrix_(self)
         call make_fock_matrix_(self)
         call make_constraint_data_(self)
         call save_constraint_for_diis_(self)
         call update_scfdata_(self)
         call add_constraint_(self)
         call update_scfdata_error_(self)
         call put_scf_results_(self)
         if (scf_done_(self%scfdata)) exit
       end do
       call update_lambda_(self%scfdata)
       if (exceeded_lambda_max_(self%scfdata)) exit

       call reset_constraint_stuff_(self)
       call update_scfdata_error_(self)
     end do
     call cleanup_(self%scfdata%diis)
     call cleanup_scf_(self)
     call put_reflection_data_(self%crystal)
     call set_(archive,self%name,"constraint_matrix"); call delete_all_genres_(archive)

   end subroutine

   subroutine make_constraint_data(self)
    type(mol_type) :: self
    ! Make the predicted constraint data, e.g. X-ray structure factors or
    ! PND structure factors, or whatever data are to be used in the constrained
    ! wavefunction procedure
      character(128) :: scf_kind

      call ensure_(tonto,associated(self%scfdata),"MOL:make_constraint_data ... no scfdata")
      scf_kind = self%scfdata%scf_kind
      select case (scf_kind)
         case ("xray_rdft"); call make_structure_factors_(self)
         case ("xray_udft"); call make_structure_factors_(self)
         case ("xray_rhf");  call make_structure_factors_(self)
         case ("xray_uhf");  call make_structure_factors_(self)
         case ("pnd_uhf");   call make_sz_structure_factors_(self)
         case default
            call die_(tonto,"MOL:make_constraint_data ... unknown wavefunction fitting kind, "//trim(scf_kind))
      end select

   end subroutine

   subroutine add_constraint(self)
    type(mol_type) :: self
    ! Make the constraint matrix for wavefunction fitting and add it to the fock
    ! matrix to obtain the effective fock matrix.
     character(128) :: scf_kind
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: C
     real(kind=kind(1.0d0)), dimension(:), pointer :: C_tri
     type(archive_type) :: arch

     call ensure_(tonto,associated(self%scfdata),"MOL:add_constraint ... no scf data")
     call ensure_(tonto,associated(self%fock_matrix),"MOL:add_constraint ... no fock matrix")
     call ensure_(tonto,any_created_(self%fock_matrix),"MOL:add_constraint ... no fock matrix")
     call create_(C,self%n_bf,self%n_bf)
     call make_constraint_(self,C)
      ! Now add in the contribution
     scf_kind = self%scfdata%scf_kind
     select case (scf_kind)
        case("xray_rdft","xray_rhf")
           call plus_scaled_(self%fock_matrix%restricted,C,self%scfdata%lambda)
        case("xray_uhf","xray_udft")
           call plus_scaled_(self%fock_matrix%alpha,C,self%scfdata%lambda)
           call plus_scaled_(self%fock_matrix%beta,C,self%scfdata%lambda)
        case("pnd_uhf")
           call plus_scaled_(self%fock_matrix%alpha,C,self%scfdata%lambda)
           call minus_scaled_(self%fock_matrix%alpha,C,self%scfdata%lambda)
        case default
           call die_(tonto,"MOL:add_constraint ... Not a contrained wavefunction type, "//trim(scf_kind))
     end select
     call create_(C_tri,tri_size_(C))
     call compress_to_triangle_(C,C_tri)
     call set_(arch,self%name,"constraint_matrix")
     call write_(arch,C_tri)
     call destroy_(C_tri)
     call destroy_(C)

   end subroutine

   subroutine make_constraint(self,C)
    type(mol_type) :: self
    ! Make the constraint matrix "C" for wavefunction fitting.
     real(kind=kind(1.0d0)), dimension(:,:), intent(inout) :: C
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: k_pts
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:), pointer :: ft_ab_eq,ft_ab
     complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: Fc,fac_times_alpha,Fconjg
     real(kind=kind(1.0d0)), dimension(:), pointer :: Fm,Fexp,Fsig,alpha
     type(shell2_type) :: sh
     real(kind=kind(1.0d0)) :: fac,cutoff
     integer(kind=kind(1)) :: q,fa,la,fb,lb,i,j,n_refl,n_unique,atom_a,atom_b

     call ensure_(tonto,spinorbital_kind_(self%scfdata)=="restricted","MOL:make_constraint ... can only do restricted calcula&
&tions")
     call ensure_(tonto,associated(self%crystal),"MOL:make_constraint ... no crystal info")
     call ensure_(tonto,reflection_data_exists_(self%crystal),"MOL:make_constraint ... no reflection data")
     call ensure_(tonto,self%basis_info_made,"MOL:make_constraint ... no basis info")
     call ensure_(tonto,associated(self%atom),"MOL:make_constraint ... no atom list")
     n_unique = n_unique_SF_k_pts_(self%crystal)
     n_refl = n_refl_(self%crystal)
     fac = 2.0d0/max(n_refl - self%crystal%n_param,1)
     call create_(fac_times_alpha,n_refl)
     call create_(Fc,n_refl);    Fc = F_calc_(self%crystal)
     call create_(alpha,n_refl)
     alpha = extinction_correction_(self%crystal)
     call create_(Fexp,n_refl);  Fexp = F_exp_(self%crystal)
     call create_(Fsig,n_refl);  Fsig = F_sigma_(self%crystal)
     call create_(Fm,n_refl);    Fm = max(abs(Fc),10.0d0**(-10))
     fac_times_alpha = fac * alpha * (alpha * Fm - Fexp) / (Fsig * Fsig * Fm)
  !   fac_times_alpha = fac * alpha * (alpha - Fexp/Fm) / (Fsig * Fsig)
     call destroy_(Fm)
     call destroy_(Fsig)
     call destroy_(Fexp)
     call destroy_(alpha)
     call create_(Fconjg,n_refl)
     Fconjg=conjg(Fc(:))
     call create_(k_pts,n_unique,3)
     call make_unique_SF_k_pts_(self%crystal,k_pts)
     cutoff = 10.0d0**(-10) / self%n_shell_pairs
     do q=1, self%n_shell_pairs
       call get_shell_pair_(self,sh,q,fa,la,fb,lb,atom_a,atom_b)
       if (skip_ft_(sh,cutoff)) then
         C(fa:la,fb:lb) = 0.0d0
         call destroy_ptr_part_(sh)
         cycle
       end if
       call create_(ft_ab,(/1,n_refl/),(/fa,la/),(/fb,lb/))
       call create_(ft_ab_eq,n_unique,sh%a%n_comp,sh%b%n_comp)
       call make_ft_pair_(self,ft_ab_eq,k_pts,sh,atom_a,atom_b)
       call sum_ft_ints_(self%crystal,ft_ab,ft_ab_eq)
       call destroy_(ft_ab_eq)
       do i = fa,la
         do j = fb,min(lb,i)
  !         C(i,j) = sum(fac_times_alpha(:) * real(ft_ab(:,i,j) * Fconjg(:)))
           C(i,j) = sum(fac_times_alpha(:) &
                            * (real(ft_ab(:,i,j)) * real(Fconjg(:)) &
                            -  aimag(ft_ab(:,i,j)) * aimag(Fconjg(:))))
            ! real(a*b) = real(a)*real(b)-aimag(a)*aimag(b)

         end do
       end do
       call destroy_(ft_ab)
       call destroy_ptr_part_(sh)
     end do
     call symmetric_reflect_(C)
     call destroy_(Fconjg)
     call destroy_(k_pts)
     call destroy_(Fc)
     call destroy_(fac_times_alpha)

   end subroutine

   subroutine save_constraint_for_diis(self)
    type(mol_type) :: self
    ! ?
     type(archive_type) :: arch
     real(kind=kind(1.0d0)), dimension(:), pointer :: C_tri

     if (.not. apply_fock_diis_(self%scfdata)) then;   return; end if

      ! Read in the matrix from disk
     call set_(arch,self%name,"constraint_matrix")
     call create_(C_tri,triangle_(self%n_bf))
     call read_(arch,C_tri)
     call close_(arch)
     call delete_(arch)

      ! Save the constraint matrix to disk with a different name
     call save_item_(self%scfdata%diis,C_tri,"constraint",self%scfdata%diis%new)
     call destroy_(C_tri)

      ! Save the density matrix to disk with a different name
     call save_item_(self%scfdata%diis,self%density_matrix,"density",self%scfdata%diis%new)

   end subroutine

   subroutine reset_constraint_stuff(self)
    type(mol_type) :: self
    ! ?
     type(archive_type) :: arch
     integer(kind=kind(1)) :: i
     type(opmatrix_type), pointer :: error,density
     real(kind=kind(1.0d0)), dimension(:), pointer :: F_tri,C_tri
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: F,C
     character(128) :: scf_kind

     if (.not. apply_fock_diis_(self%scfdata)) then;   return; end if

     call create_(C_tri,triangle_(self%n_bf))
     call create_(F,self%n_bf,self%n_bf)

     call create_(F_tri,triangle_(self%n_bf))
     call create_(density,self%n_bf,"restricted")
     call create_(error,self%n_bf,"restricted")

     do i=1,dimension_(self%scfdata%diis)-1
        ! Read in the constraint
       call get_item_(self%scfdata%diis,C_tri,"constraint",i)
        ! Read in the Fock
       call get_parameter_item_(self%scfdata%diis,i,F_tri)
        ! Read in the density matrix
       call get_item_(self%scfdata%diis,density,"density",i)

        ! Scale the Fock matrix to the new lambda
       F_tri = F_tri + self%scfdata%lambda_step*C_tri

       call uncompress_from_triangle_(F,F_tri)

       call make_r_diis_error_(self,error%restricted,F,density%restricted)
       call compress_(error)
       call save_pair_(self%scfdata%diis,F_tri,error%triangle,i)
       call uncompress_(error)
     end do
     call remake_diis_matrix_(self%scfdata%diis,triangle_(self%n_bf))
     call destroy_(error)
     call destroy_(density)
     call destroy_(F_tri)
     call unarchive_density_matrix_(self)

      ! Just to save effort on memory reallocation.
     C => F; nullify(F)

      ! The current Fock matrix is for the old lambda, so let's make it for the
      ! new lambda.
     call set_(arch,self%name,"constraint_matrix")
     call read_(arch,C_tri)
     call uncompress_from_triangle_(C,C_tri)
     scf_kind = self%scfdata%scf_kind
     select case (scf_kind)
        case("xray_rdft","xray_rhf")
           call plus_scaled_(self%fock_matrix%restricted,C,self%scfdata%lambda_step)
        case("xray_uhf","xray_udft")
           call plus_scaled_(self%fock_matrix%alpha,C,self%scfdata%lambda_step)
           call plus_scaled_(self%fock_matrix%beta,C,self%scfdata%lambda_step)
        case("pnd_uhf")
           call plus_scaled_(self%fock_matrix%alpha,C,self%scfdata%lambda_step)
           call minus_scaled_(self%fock_matrix%alpha,C,self%scfdata%lambda_step)
        case default
           call die_(tonto,"MOL:reset_constraint_stuff ... Not a contrained wavefunction type, "//trim(scf_kind))
     end select

     call destroy_(C)
     call destroy_(C_tri)

   end subroutine

!*******************************************************************************
!                               Plotting methods
!*******************************************************************************

   subroutine plot(self)
    type(mol_type) :: self
    ! Do one of the many kinds of plot calculations
     character(128) :: word

     call ensure_(tonto,associated(self%grid),"MOL:plot ... no grid")
     call put_(self%grid)
     word = self%grid%plot_kind
     call to_lower_case_(word)
     select case (word)
        case("crystal_error_map  "); call make_crystal_error_map_(self)
        case("current_density    "); call make_j_density_grid_(self)
        case("difference_density "); call make_difference_density_grid_(self)
        case("electric_potential "); call make_electric_potential_grid_(self)
        case("electron_density   "); call make_electron_density_grid_(self)
        case("elf                "); call make_ELF_grid_(self)
        case("fermi_mobility     "); call make_fermi_mobility_grid_(self)
        case("hirshfeld_density  "); call make_stockholder_grid_(self)
        case("j_density          "); call make_j_density_grid_(self)
        case("jd_density         "); call make_jd_density_grid_(self)
        case("jp_density         "); call make_jp_density_grid_(self)
        case("laplacian_density  "); call make_laplacian_density_grid_(self)
        case("div_jp_density     "); call make_div_jp_density_grid_(self)
        case("orbital_density    "); call make_orbital_density_grid_(self)
        case("orbital            "); call make_orbital_grid_(self)
        case("qq_plot            "); call put_qq_plot_(self%crystal,self%name)
        case("solenoidal_jp      "); call make_solenoidal_jp_grid_(self)
        case("spin_density       "); call make_spin_density_grid_(self)
        case("stockholder_density"); call make_stockholder_grid_(self)
        case("true_fermi_mobility"); call make_true_fermi_mobility_grid_(self)
        case("tsirelson_elf      "); call make_Tsirelson_ELF_grid_(self)
        case("grad_rho_on_rho    "); call make_grad_rho_on_rho_grid_(self)
        case default;                allocate(tonto%known_keywords(22))
        tonto%known_keywords(1) = "crystal_error_map  "
        tonto%known_keywords(2) = "current_density    "
        tonto%known_keywords(3) = "difference_density "
        tonto%known_keywords(4) = "electric_potential "
        tonto%known_keywords(5) = "electron_density   "
        tonto%known_keywords(6) = "elf                "
        tonto%known_keywords(7) = "fermi_mobility     "
        tonto%known_keywords(8) = "hirshfeld_density  "
        tonto%known_keywords(9) = "j_density          "
        tonto%known_keywords(10) = "jd_density         "
        tonto%known_keywords(11) = "jp_density         "
        tonto%known_keywords(12) = "laplacian_density  "
        tonto%known_keywords(13) = "div_jp_density     "
        tonto%known_keywords(14) = "orbital_density    "
        tonto%known_keywords(15) = "orbital            "
        tonto%known_keywords(16) = "qq_plot            "
        tonto%known_keywords(17) = "solenoidal_jp      "
        tonto%known_keywords(18) = "spin_density       "
        tonto%known_keywords(19) = "stockholder_density"
        tonto%known_keywords(20) = "true_fermi_mobility"
        tonto%known_keywords(21) = "tsirelson_elf      "
        tonto%known_keywords(22) = "grad_rho_on_rho    "
        call unknown_(tonto,word,"MOL:plot")
        deallocate(tonto%known_keywords)
     end select

   end subroutine

   function bounding_cube_width(self) result(width)
    type(mol_type) :: self
    ! Return "width", the width of a cube in which the molecule nicely sits.
    ! Suitable for generating plot widths.
      real(kind=kind(1.0d0)) :: width

      width = bounding_cube_width_(self%atom)

   end function

   subroutine make_electron_density_grid(self)
    type(mol_type) :: self
    ! Work out the density on ".grid" using ".natural orbitals" and the
    ! ".occupation_numbers" vector. A Gnuplot ascii file is generated.
      type(archive_type) :: arch
      real(kind=kind(1.0d0)), dimension(:), pointer :: density_grid
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: pt

      call ensure_(tonto,associated(self%grid),"MOL:make_electron_density_grid ... no grid")
      call create_(density_grid,self%grid%n_pt)
      call create_(pt,self%grid%n_pt,3); call make_points_(self%grid,pt)
      call make_density_grid_(self,density_grid,pt)
      call set_(arch,self%name,"electron_density_grid",format="ascii")
      call write_gnuplot_(arch,density_grid, self%grid%n_x, self%grid%n_y, self%grid%n_z)
      call destroy_(pt)
      call destroy_(density_grid)

   end subroutine

   subroutine make_difference_density_grid(self)
    type(mol_type) :: self
    ! Make a Gnuplot ascii file containing the difference density grid.
     type(archive_type) :: arch
     type(opmatrix_type), pointer :: old_dens,old_nos
     type(opvector_type), pointer :: old_occ
     real(kind=kind(1.0d0)), dimension(:), pointer :: density_grid,atom_density_grid
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: pt
     character(128) :: orb_kind

     call ensure_(tonto,associated(self%grid),"MOL:make_difference_density_grid ... no grid")
     call ensure_(tonto,associated(self%natural_orbitals),"MOL:make_difference_density_grid ... no natural orbitals")
     call ensure_(tonto,associated(self%occupation_numbers),"MOL:make_difference_density_grid ... no natural orbitals")

     call create_(density_grid,self%grid%n_pt)
     call create_(pt,self%grid%n_pt,3); call make_points_(self%grid,pt)
     orb_kind = molecular_orbital_kind_(self%scfdata)

      ! Get density grid for the molecule
     call make_density_grid_(self,density_grid,pt)

      ! Backup the data for the molecule
     old_dens => self%density_matrix
     old_nos => self%natural_orbitals
     old_occ => self%occupation_numbers
     nullify(self%density_matrix)
     nullify(self%natural_orbitals)
     nullify(self%occupation_numbers)

      ! Get density grid for the promolecule
     call make_atom_density_(self)
     call make_natural_orbitals_(self)
     call create_(atom_density_grid,self%grid%n_pt)
     call make_density_grid_(self,atom_density_grid,pt)
     density_grid = density_grid - atom_density_grid
     call destroy_(atom_density_grid)

      ! Restore the data for the molecule
     call destroy_(self%density_matrix)
     call destroy_(self%natural_orbitals)
     call destroy_(self%occupation_numbers)
     self%density_matrix => old_dens
     self%natural_orbitals => old_nos
     self%occupation_numbers => old_occ

     call set_(arch,self%name,"difference_density_grid",format="ascii")
     call write_gnuplot_(arch,density_grid, self%grid%n_x, self%grid%n_y, self%grid%n_z)
     call destroy_(pt)
     call destroy_(density_grid)

   end subroutine

   subroutine make_spin_density_grid(self)
    type(mol_type) :: self
    ! Work out the spin density on ".grid" using ".natural orbitals" and the
    ! ".occupation_numbers" vector. A Gnuplot ascii file is generated.
      type(archive_type) :: arch
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: pt
      real(kind=kind(1.0d0)), dimension(:), pointer :: density_grid

   call ensure_(tonto,associated(self%grid),"MOL:make_spin_density_grid ... no grid")
      call make_ao_sz_density_matrix_(self)
      call create_(density_grid,self%grid%n_pt)
      call create_(pt,self%grid%n_pt,3); call make_points_(self%grid,pt)
      call make_density_grid_(self,density_grid,pt)
      call set_(arch,self%name,"spin_density_grid",format="ascii")
      call write_gnuplot_(arch,density_grid, self%grid%n_x, self%grid%n_y, self%grid%n_z)
      call destroy_(pt)
      call destroy_(density_grid)

   end subroutine

   subroutine make_density_grid(self,density_grid,pt)
    type(mol_type) :: self
    ! Work out the electron "density_grid" on "pt" using ".natural orbitals" and
    ! the ".occupation_numbers" vector.
      real(kind=kind(1.0d0)), dimension(:) :: density_grid
      real(kind=kind(1.0d0)), dimension(:,:) :: pt

   call ensure_(tonto,associated(self%natural_orbitals),"MOL:make_density_grid ... no natural orbitals")
   call ensure_(tonto,any_created_(self%natural_orbitals),"MOL:make_density_grid ... no natural orbitals")
      if (number_kind_(self%natural_orbitals) == "real") then
         call make_density_grid_r_(self,density_grid,pt)
      else
         call make_density_grid_c_(self,density_grid,pt)
      end if

   end subroutine

   subroutine make_density_grid_r(self,density_grid,pt)
    type(mol_type) :: self
    ! Make the "density_grid" for the supplied points "pt" from restricted real
    ! natural orbitals
     real(kind=kind(1.0d0)), dimension(:) :: density_grid
     real(kind=kind(1.0d0)), dimension(:,:) :: pt
     real(kind=kind(1.0d0)), dimension(:), pointer :: NO
     integer(kind=kind(1)) :: n_occ,n

   call ensure_(tonto,size(pt,2)==3,"MOL:make_density_grid_r ... wrong dimension for points array")
   call ensure_(tonto,created_(self%natural_orbitals,"restricted"),"MOL:make_density_grid_r ... no restricted NO's")
   call ensure_(tonto,associated(self%occupation_numbers%restricted),"MOL:make_density_grid_r ... no occupation numbers")
     density_grid = 0.0d0
     call create_(NO,size(pt,1))
     n_occ = no_of_occupied_NOs_(self)
     do n = 1,n_occ
       call make_orbital_grid_r_(self,NO,self%natural_orbitals%restricted(:,n), pt)
       density_grid(:) = density_grid(:) + self%occupation_numbers%restricted(n)*NO(:)*NO(:)
     end do
     call destroy_(NO)

   end subroutine

   subroutine make_density_grid_c(self,density_grid,pt)
    type(mol_type) :: self
    ! Make the "density_grid" for the supplied points "pt" from restricted
    ! complex natural orbitals.
      real(kind=kind(1.0d0)), dimension(:) :: density_grid
      real(kind=kind(1.0d0)), dimension(:,:) :: pt
      complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: NO
      integer(kind=kind(1)) :: n_occ,n

   call ensure_(tonto,size(pt,2)==3,"MOL:make_density_grid_c ... incorrect dimension for points array")
   call ensure_(tonto,created_(self%natural_orbitals,"restricted_complex"),"MOL:make_density_grid_c ... no natural orbitals")
   call ensure_(tonto,associated(self%occupation_numbers%restricted),"MOL:make_density_grid_c ... no occupation numbers")
      density_grid = 0.0d0
      call create_(NO,size(pt,1))
      n_occ = no_of_occupied_NOs_(self)
      do n = 1,n_occ
         call make_orbital_grid_c_(self,NO,self%natural_orbitals%restricted_complex(:,n), pt)
         density_grid(:) = density_grid(:) + self%occupation_numbers%restricted(n)*conjg(NO(:))*NO(:)
      end do
      call destroy_(NO)

   end subroutine

   subroutine make_nabla_density_grid_fdm_r(self,nabla_grid,pts)
    type(mol_type) :: self
    ! Work out the nabla density on ".grid" using ".natural orbitals" and the
    ! ".occupation" number vector.
    ! This method uses the finite difference method to calculate the gradient
    ! from the density.  It is much slower, but useful for checking.
     real(kind=kind(1.0d0)), dimension(:,:) :: nabla_grid, pts
     real(kind=kind(1.0d0)), dimension(:), pointer :: orb
     real(kind=kind(1.0d0)), dimension(:), pointer :: dens_1,dens_2,nabla_orb,comp
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: pt1
     real(kind=kind(1.0d0)) :: alpha
     integer(kind=kind(1)) :: n_occ,n,n_pts,i

   call ensure_(tonto,size(pts,2)==3,"MOL:make_nabla_density_grid_fdm_r ... wrong dimension for points array")
   call ensure_(tonto,size(nabla_grid,2)==3,"MOL:make_nabla_density_grid_fdm_r ... wrong dimension for nabla_grid array")
   call ensure_(tonto,created_(self%natural_orbitals,"restricted"),"MOL:make_nabla_density_grid_fdm_r ... no restricted NO's"&
&)
   call ensure_(tonto,associated(self%occupation_numbers%restricted),"MOL:make_nabla_density_grid_fdm_r ... no occupation #'s&
&")
     alpha = 10.0d0**(-6)
     n_pts = size(pts,1)
     call create_(pt1,n_pts,3)
     nabla_grid = 0.0d0
     call create_(orb,n_pts)
     call create_(dens_1,n_pts)
     call create_(dens_2,n_pts)
     call create_(nabla_orb,n_pts)
     n_occ = no_of_occupied_NOs_(self)
     do n = 1,n_occ
        do i = 1,3
          comp => pt1(:,i)
          pt1 = pts;    comp = comp + alpha
          call make_orbital_grid_r_(self,dens_1,self%natural_orbitals%restricted(:,n),pt1,square=.true.)
          pt1 = pts;    comp = comp - alpha
          call make_orbital_grid_r_(self,dens_2,self%natural_orbitals%restricted(:,n),pt1,square=.true.)
          nabla_orb = 0.50d0/alpha * (dens_1 - dens_2)
          call make_orbital_grid_r_(self,orb,self%natural_orbitals%restricted(:,n), pts)
          nabla_grid(:,i) = nabla_grid(:,i) + self%occupation_numbers%restricted(n)*orb(:)*nabla_orb
        end do
     end do
     nabla_grid = 2.0d0*nabla_grid
     call destroy_(nabla_orb)
     call destroy_(dens_2)
     call destroy_(dens_1)
     call destroy_(orb)
     call destroy_(pt1)

   end subroutine

   subroutine make_nabla_density_grid(self,nabla_grid,pts)
    type(mol_type) :: self
    ! Work out the nabla density on ".grid" using ".natural orbitals" and the
    ! ".occupation" number vector.
      real(kind=kind(1.0d0)), dimension(:,:) :: nabla_grid, pts

      if (number_kind_(self%natural_orbitals) == "real") then
         call make_nabla_density_grid_r_(self,nabla_grid,pts)
       !else
       !   .make_nabla_density_grid_c(nabla_grid,pts)
      end if

   end subroutine

   subroutine make_nabla_density_grid_r(self,nabla_grid,pts)
    type(mol_type) :: self
    ! Work out the nabla density on ".grid" using ".natural orbitals" and the
    ! ".occupation" number vector.
     real(kind=kind(1.0d0)), dimension(:,:) :: nabla_grid, pts
     real(kind=kind(1.0d0)), dimension(:), pointer :: orb
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: nabla_orb
     real(kind=kind(1.0d0)) :: occ
     integer(kind=kind(1)) :: n_occ,n,n_pts

     call ensure_(tonto,size(pts,2)==3,"MOL:make_nabla_density_grid_r ... wrong dimension for points array")
     call ensure_(tonto,size(nabla_grid,2)==3,"MOL:make_nabla_density_grid_r ... wrong dimension for nabla_grid array")
     call ensure_(tonto,associated(self%natural_orbitals),"MOL:make_nabla_density_grid_r ... no natural orbitals")
     call ensure_(tonto,created_(self%natural_orbitals,"restricted"),"MOL:make_nabla_density_grid_r ... no restricted NO's")
     call ensure_(tonto,associated(self%occupation_numbers),"MOL:make_nabla_density_grid_r ... no occupation numbers")
     call ensure_(tonto,created_(self%occupation_numbers,"restricted"),"MOL:make_nabla_density_grid_r ... no occupation numbe&
&rs")
     n_pts = size(pts,1)
     nabla_grid = 0.0d0
     call create_(orb,n_pts)
     call create_(nabla_orb,n_pts,3)
     n_occ = no_of_occupied_NOs_(self)
     do n = 1,n_occ
       call make_nabla_orbital_grid_r_(self,nabla_orb,orb,self%natural_orbitals%restricted(:,n), pts)
       occ = self%occupation_numbers%restricted(n)
       nabla_grid(:,1) = nabla_grid(:,1) + occ*orb(:)*nabla_orb(:,1)
       nabla_grid(:,2) = nabla_grid(:,2) + occ*orb(:)*nabla_orb(:,2)
       nabla_grid(:,3) = nabla_grid(:,3) + occ*orb(:)*nabla_orb(:,3)
     end do
     nabla_grid = 2.0d0*nabla_grid
     call destroy_(orb)
     call destroy_(nabla_orb)

   end subroutine

   subroutine make_unrestricted_density_grid(self,p_a,p_b,pt)
    type(mol_type) :: self
    ! Make the alpha and beta density grids separately.
     real(kind=kind(1.0d0)), dimension(:) :: p_a,p_b
     real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: pt
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: nat
     real(kind=kind(1.0d0)), dimension(:), pointer :: occ

     call ensure_(tonto,associated(self%natural_orbitals),"MOL:make_unrestricted_density_grid ... no natural orbitals")
     call ensure_(tonto,created_(self%natural_orbitals,"unrestricted"),"MOL:make_unrestricted_density_grid ... no natural orb&
&itals")
     call ensure_(tonto,associated(self%occupation_numbers),"MOL:make_unrestricted_density_grid ... no occupation numbers")
     call ensure_(tonto,created_(self%occupation_numbers,"unrestricted"),"MOL:make_unrestricted_density_grid ... no occupatio&
&n numbers")
     nat => self%natural_orbitals%restricted  ! backup pointers
     occ => self%occupation_numbers%restricted
     self%natural_orbitals%restricted => self%natural_orbitals%alpha
     self%occupation_numbers%restricted => self%occupation_numbers%alpha
     call make_density_grid_(self,p_a,pt)
     self%natural_orbitals%restricted => self%natural_orbitals%beta
     self%occupation_numbers%restricted => self%occupation_numbers%beta
     call make_density_grid_(self,p_b,pt)
     self%natural_orbitals%restricted => nat  ! restore backups
     self%occupation_numbers%restricted => occ

   end subroutine

   subroutine make_u_nabla_density_grid(self,np_a,np_b,pt)
    type(mol_type) :: self
    ! Make the alpha and beta nabla density grids separately.
     real(kind=kind(1.0d0)), dimension(:,:) :: np_a,np_b
     real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: pt
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: nat
     real(kind=kind(1.0d0)), dimension(:), pointer :: occ

     call ensure_(tonto,created_(self%natural_orbitals,"unrestricted"),"MOL:make_u_nabla_density_grid ... no natural orbitals&
&")
     call ensure_(tonto,created_(self%occupation_numbers,"unrestricted"),"MOL:make_u_nabla_density_grid ... no occupation num&
&bers")
     nat => self%natural_orbitals%restricted  ! backup pointers
     occ => self%occupation_numbers%restricted
     self%natural_orbitals%restricted => self%natural_orbitals%alpha
     self%occupation_numbers%restricted => self%occupation_numbers%alpha
     call make_nabla_density_grid_(self,np_a,pt)
     self%natural_orbitals%restricted => self%natural_orbitals%beta
     self%occupation_numbers%restricted => self%occupation_numbers%beta
     call make_nabla_density_grid_(self,np_b,pt)
     self%natural_orbitals%restricted => nat  ! restore backups
     self%occupation_numbers%restricted => occ

   end subroutine

   subroutine make_orbital_density_grid(self)
    type(mol_type) :: self
    ! Work out the orbital on ".grid" using ".natural orbitals" for orbital "orb"
    ! A Gnuplot ascii file is generated.
      type(archive_type) :: arch
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: pt
      real(kind=kind(1.0d0)), dimension(:), pointer :: gr
      complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: gc
      integer(kind=kind(1)) :: orb

      call ensure_(tonto,associated(self%grid),"MOL:make_orbital_density_grid ... no grid")
      call ensure_(tonto,self%grid%orbital>=0,"MOL:make_orbital_density_grid ... non-positive grid orbital")
      call ensure_(tonto,associated(self%natural_orbitals),"MOL:make_orbital_density_grid ... no natural orbitals")
      call ensure_(tonto,any_created_(self%natural_orbitals),"MOL:make_orbital_density_grid ... no natural orbitals")
      orb = self%grid%orbital
      call create_(pt,self%grid%n_pt,3); call make_points_(self%grid,pt)
      call create_(gr,self%grid%n_pt)
      if (number_kind_(self%natural_orbitals) == "real") then
        call make_orbital_grid_r_(self,gr,self%natural_orbitals%restricted(:,orb),pt,square=.true.)
      else
        call create_(gc,self%grid%n_pt)
        call make_orbital_grid_c_(self,gc,self%natural_orbitals%restricted_complex(:,orb),pt,square=.true.)
        gr = gc
        call destroy_(gc)
      end if
      call destroy_(gr)
      call destroy_(pt)
      call set_(arch,self%name,"orbital_"//trim(to_str_(orb))//"_density_grid")
      call write_(arch,gr)
      call set_(arch,self%name,"orbital_"//trim(to_str_(orb))//"_density_grid",format="ascii")
      call write_gnuplot_(arch,gr, self%grid%n_x, self%grid%n_y, self%grid%n_z)

   end subroutine

   subroutine make_orbital_grid(self)
    type(mol_type) :: self
    ! Work out the orbital on ".grid" using ".natural orbitals" for orbital "orb"
    ! A Gnuplot ascii file is generated.
      type(archive_type) :: arch
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: pt
      real(kind=kind(1.0d0)), dimension(:), pointer :: gr
      complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: gc
      integer(kind=kind(1)) :: orb

   call ensure_(tonto,associated(self%grid),"MOL:make_orbital_grid ... no grid")
   call ensure_(tonto,self%grid%orbital>=0,"MOL:make_orbital_grid ... non-positive grid orbital")
   call ensure_(tonto,associated(self%natural_orbitals),"MOL:make_orbital_grid ... no natural orbitals")
   call ensure_(tonto,any_created_(self%natural_orbitals),"MOL:make_orbital_grid ... no natural orbitals")
      orb = self%grid%orbital
      call create_(pt,self%grid%n_pt,3); call make_points_(self%grid,pt)
      if (number_kind_(self%natural_orbitals) == "real") then
         call create_(gr,self%grid%n_pt)
         call make_orbital_grid_r_(self,gr,self%natural_orbitals%restricted(:,orb),pt)
         call set_(arch,self%name,"orbital_"//trim(to_str_(orb))//"_grid")
         call write_(arch,gr)
         call set_(arch,self%name,"orbital_"//trim(to_str_(orb))//"_grid",format="ascii")
         call write_gnuplot_(arch,gr, self%grid%n_x, self%grid%n_y, self%grid%n_z)
         call destroy_(gr)
      else
         call create_(gc,self%grid%n_pt)
         call make_orbital_grid_c_(self,gc,self%natural_orbitals%restricted_complex(:,orb),pt)
         call set_(arch,self%name,"orbital_"//trim(to_str_(orb))//"_grid")
         call write_(arch,gc)
         call set_(arch,self%name,"orbital_"//trim(to_str_(orb))//"_grid",format="ascii")
          ! arch.write_gnuplot(gc, .grid.n_x, .grid.n_y, .grid.n_z)
         call destroy_(gc)
      end if
      call destroy_(pt)

   end subroutine

   subroutine make_orbital_grid_r(self,g,orb,pt,square)
    type(mol_type) :: self
    ! Evaluate the orbital density grid "g" for *one* AO-basis coefficient
    ! orbital vector "orb" on a set of grid points "pt"
      real(kind=kind(1.0d0)), dimension(:), intent(out) :: g
      real(kind=kind(1.0d0)), dimension(:), intent(in) :: orb
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: pt
      logical(kind=kind(.true.)), optional :: square
      logical(kind=kind(.true.)) :: sq
      type(shell1_type) :: sh
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: sh_grid
      integer(kind=kind(1)) :: n_pt,fa,la,a

      call ensure_(tonto,self%basis_info_made,"MOL:make_orbital_grid_r ... no basis info")
      call ensure_(tonto,associated(self%atom),"MOL:make_orbital_grid_r ... no atom list")
      sq = .false.
      if (present(square)) sq = square
      n_pt = size(pt,1)
      g = 0.0d0
      do a = 1,self%n_shell
         call get_shell_(self,sh,a,fa,la)
         call create_(sh_grid,n_pt,sh%n_comp)
         call make_grid_(sh,sh_grid,pt)
         g = g + matmul(sh_grid,orb(fa:la))
         call destroy_(sh_grid)
         call destroy_ptr_part_(sh)
      end do
      if (sq) g = g*g

   end subroutine

   subroutine make_orbital_grid_c(self,g,orb,pt,square)
    type(mol_type) :: self
    ! Evaluate the orbital density for *one* AO-basis orbital coefficient vector
    ! "orb" on a set of grid points "pt"
      complex(kind=kind((1.0d0,1.0d0))), dimension(:), intent(out) :: g
      complex(kind=kind((1.0d0,1.0d0))), dimension(:), intent(in), target :: orb
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: pt
      logical(kind=kind(.true.)), optional :: square
      logical(kind=kind(.true.)) :: sq
      type(shell1_type), pointer :: sh
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: sh_grid
      integer(kind=kind(1)) :: n_pt,fa,la,a

      call ensure_(tonto,self%basis_info_made,"MOL:make_orbital_grid_c ... no basis info")
      call ensure_(tonto,associated(self%atom),"MOL:make_orbital_grid_c ... no atom list")
      sq = .false.
      if (present(square)) sq = square
      n_pt = size(pt,1)
      g = 0.0d0
      call create_(sh)
      do a = 1,self%n_shell
         call get_shell_(self,sh,a,fa,la)
         call create_(sh_grid,n_pt,sh%n_comp)
         call make_grid_(sh,sh_grid,pt)
         g = g + matmul(sh_grid,orb(fa:la))
         call destroy_(sh_grid)
         call destroy_ptr_part_(sh)
      end do
      call destroy_(sh)
      if (sq) g = conjg(g)*g

   end subroutine

   subroutine make_laplacian_density_grid(self)
    type(mol_type) :: self
    ! Work out the laplacian density on ".grid" using ".natural orbitals" and the
    ! ".occupation_numbers" vector. A Gnuplot ascii file is generated.
      type(archive_type) :: arch
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: pt
      real(kind=kind(1.0d0)), dimension(:), pointer :: laplacian_grid

      call ensure_(tonto,associated(self%grid),"MOL:make_laplacian_density_grid ... no grid")
      call create_(laplacian_grid,self%grid%n_pt)
      call create_(pt,self%grid%n_pt,3); call make_points_(self%grid,pt)
      call make_laplacian_grid_(self,laplacian_grid,pt)
      call set_(arch,self%name,"laplacian_density_grid",format="ascii")
      call write_gnuplot_(arch,laplacian_grid, self%grid%n_x, self%grid%n_y, self%grid%n_z)
      call destroy_(pt)
      call destroy_(laplacian_grid)

   end subroutine

   subroutine make_laplacian_grid(self,laplacian_grid)
    type(mol_type) :: self
    ! Work out the laplacian density on ".grid" using ".natural orbitals" and the
    ! ".occupation" number vector.
      real(kind=kind(1.0d0)), dimension(:) :: laplacian_grid
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: pt
      logical(kind=kind(.true.)) :: old_NOs

      old_NOs = .false.
      call create_(pt,self%grid%n_pt,3); call make_points_(self%grid,pt)
      if (associated(self%natural_orbitals)) old_NOs = created_(self%natural_orbitals,"restricted")
      if (.not. old_NOs) call make_natural_orbitals_(self)
      call make_laplacian_grid_(self,laplacian_grid,pt)
      call destroy_(pt)

   end subroutine

   subroutine make_laplacian_grid_1(self,laplacian_grid,pt)
    type(mol_type) :: self
    ! Work out the electron "laplacian_grid" on "pt" using ".natural orbitals"
    ! and  the ".occupation_numbers" vector.
      real(kind=kind(1.0d0)), dimension(:) :: laplacian_grid
      real(kind=kind(1.0d0)), dimension(:,:) :: pt

      call ensure_(tonto,associated(self%natural_orbitals),"MOL:make_laplacian_grid_1 ... no natural orbitals")
      call ensure_(tonto,any_created_(self%natural_orbitals),"MOL:make_laplacian_grid_1 ... no natural orbitals")
      if (number_kind_(self%natural_orbitals) == "real") then
        call make_laplacian_grid_r_(self,laplacian_grid,pt)
      else
        call die_(tonto,"MOL:make_laplacian_grid_1 ... code not written yet")
      end if

   end subroutine

   subroutine make_laplacian_grid_r(self,laplacian_grid,pt)
    type(mol_type) :: self
    ! Make the "laplacian_grid" for the supplied points "pt" from restricted
    ! real natural orbitals
     real(kind=kind(1.0d0)), dimension(:) :: laplacian_grid
     real(kind=kind(1.0d0)), dimension(:,:) :: pt
     real(kind=kind(1.0d0)), dimension(:), pointer :: NO
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: P,D
     integer(kind=kind(1)) :: n_occ,n,n_pt

     call ensure_(tonto,size(pt,2)==3,"MOL:make_laplacian_grid_r ... wrong dimension for pt array")
     call ensure_(tonto,created_(self%natural_orbitals,"restricted"),"MOL:make_laplacian_grid_r ... no restricted NO's")
     call ensure_(tonto,associated(self%occupation_numbers%restricted),"MOL:make_laplacian_grid_r ... no occupation #'s")
     n_pt = size(pt,1)
     laplacian_grid = 0.0d0
     call create_(NO,n_pt)
     call create_(P,n_pt,3)
     call create_(D,n_pt,3)
     n_occ = no_of_occupied_NOs_(self)
     do n = 1,n_occ
       call make_laplacian_orbital_grid_r_(self,D,P,NO,self%natural_orbitals%restricted(:,n), pt)
       laplacian_grid(:) = laplacian_grid(:) +  &
          self%occupation_numbers%restricted(n) * ( &
          NO * (D(:,1)+D(:,2)+D(:,3)) + (P(:,1)*P(:,1) + P(:,2)*P(:,2) + P(:,3)*P(:,3)))
     end do
     laplacian_grid(:) = 2.0d0*laplacian_grid(:)
     call destroy_(D)
     call destroy_(P)
     call destroy_(NO)

   end subroutine

   subroutine make_nabla_orbital_grid_r(self,g,h,orb,pt)
    type(mol_type) :: self
    ! Evaluate the nabla orbital density grid "g" as well as the orbital grid "h"
    ! for *one* AO-basis orbital vector "orb" on a set of grid points "pt"
      real(kind=kind(1.0d0)), dimension(:,:) :: g
      real(kind=kind(1.0d0)), dimension(:) :: h
      real(kind=kind(1.0d0)), dimension(:), intent(in), target :: orb
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: pt
      real(kind=kind(1.0d0)), dimension(:), pointer :: orb_a
      type(shell1_type) :: sh
      real(kind=kind(1.0d0)), dimension(:,:,:), pointer :: sh_grid
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: sh_grid0
      integer(kind=kind(1)) :: n_pt,fa,la,a

      call ensure_(tonto,self%basis_info_made,"MOL:make_nabla_orbital_grid_r ... no basis info")
      call ensure_(tonto,associated(self%atom),"MOL:make_nabla_orbital_grid_r ... no atom list")
      call ensure_(tonto,size(g,1)==size(pt,1),"MOL:make_nabla_orbital_grid_r ... grid array has wrong 1st dimension")
      call ensure_(tonto,size(g,2)==3,"MOL:make_nabla_orbital_grid_r ... grid array has wrong 2nd dimension")
      call ensure_(tonto,size(h)==size(pt,1),"MOL:make_nabla_orbital_grid_r ... grid array has wrong 1st dimension")
      n_pt = size(pt,1)
      g = 0.0d0
      h = 0.0d0
      do a = 1,self%n_shell
         call get_shell_(self,sh,a,fa,la)
         call create_(sh_grid,n_pt,sh%n_comp,3)
         call create_(sh_grid0,n_pt,sh%n_comp)
         call make_nabla_grid_(sh,sh_grid,sh_grid0,pt)
         orb_a => orb(fa:la)
         g(:,1) = g(:,1) + matmul(sh_grid(:,:,1),orb_a)
         g(:,2) = g(:,2) + matmul(sh_grid(:,:,2),orb_a)
         g(:,3) = g(:,3) + matmul(sh_grid(:,:,3),orb_a)
         h = h + matmul(sh_grid0,orb_a)
         call destroy_(sh_grid0)
         call destroy_(sh_grid)
         call destroy_ptr_part_(sh)
      end do

   end subroutine

   subroutine make_nabla_orbital_grid_c(self,g,h,orb,pt)
    type(mol_type) :: self
    ! Evaluate the nabla orbital density grid "g" as well as the orbital grid "h"
    ! for *one* AO-basis orbital vector "orb" on a set of grid points "pt"
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: g
      complex(kind=kind((1.0d0,1.0d0))), dimension(:) :: h
      complex(kind=kind((1.0d0,1.0d0))), dimension(:), intent(in), target :: orb
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: pt
      complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: orb_a
      type(shell1_type) :: sh
      real(kind=kind(1.0d0)), dimension(:,:,:), pointer :: sh_grid
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: sh_grid0
      integer(kind=kind(1)) :: n_pt,fa,la,a

      call ensure_(tonto,self%basis_info_made,"MOL:make_nabla_orbital_grid_c ... no basis info")
      call ensure_(tonto,associated(self%atom),"MOL:make_nabla_orbital_grid_c ... no atom list")
      call ensure_(tonto,size(g,1)==size(pt,1),"MOL:make_nabla_orbital_grid_c ... grid array has wrong 1st dimension")
      call ensure_(tonto,size(g,2)==3,"MOL:make_nabla_orbital_grid_c ... grid array has wrong 2nd dimension")
      call ensure_(tonto,size(h)==size(pt,1),"MOL:make_nabla_orbital_grid_c ... grid array has wrong 1st dimension")
      n_pt = size(pt,1)
      g = 0.0d0
      h = 0.0d0
      do a = 1,self%n_shell
         call get_shell_(self,sh,a,fa,la)
         call create_(sh_grid,n_pt,sh%n_comp,3)
         call create_(sh_grid0,n_pt,sh%n_comp)
         call make_nabla_grid_(sh,sh_grid,sh_grid0,pt)
         orb_a => orb(fa:la)
         g(:,1) = g(:,1) + matmul(sh_grid(:,:,1),orb_a)
         g(:,2) = g(:,2) + matmul(sh_grid(:,:,2),orb_a)
         g(:,3) = g(:,3) + matmul(sh_grid(:,:,3),orb_a)
         h = h + matmul(sh_grid0,orb_a)
         call destroy_(sh_grid0)
         call destroy_(sh_grid)
         call destroy_ptr_part_(sh)
      end do

   end subroutine

   subroutine make_laplacian_orbital_grid_r(self,g,h,i,orb,pt)
    type(mol_type) :: self
    ! Evaluate the laplacian orbital density grid "g", as well as the nabla
    ! orbital grid "h" and the orbital grid "i" for *one* AO-basis orbital
    ! vector "orb" on a set of grid points "pt"
      real(kind=kind(1.0d0)), dimension(:,:) :: g,h
      real(kind=kind(1.0d0)), dimension(:) :: i
      real(kind=kind(1.0d0)), dimension(:), intent(in), target :: orb
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: pt
      type(shell1_type) :: sh
      real(kind=kind(1.0d0)), dimension(:,:,:), pointer :: sh_grid,sh_grid1
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: sh_grid0
      real(kind=kind(1.0d0)), dimension(:), pointer :: orb_a
      integer(kind=kind(1)) :: n_pt,fa,la,a

      call ensure_(tonto,self%basis_info_made,"MOL:make_laplacian_orbital_grid_r ... no basis info")
      call ensure_(tonto,associated(self%atom),"MOL:make_laplacian_orbital_grid_r ... no atom list")
      call ensure_(tonto,size(g,1)==size(pt,1),"MOL:make_laplacian_orbital_grid_r ... grid array has wrong 1st dimension")
      call ensure_(tonto,size(g,2)==3,"MOL:make_laplacian_orbital_grid_r ... grid array has wrong 2nd dimension")
      call ensure_(tonto,size(h,1)==size(pt,1),"MOL:make_laplacian_orbital_grid_r ... grid array has wrong 1st dimension")
      call ensure_(tonto,size(h,2)==3,"MOL:make_laplacian_orbital_grid_r ... grid array has wrong 2nd dimension")
      call ensure_(tonto,size(i)==size(pt,1),"MOL:make_laplacian_orbital_grid_r ... grid array has wrong 1st dimension")
      n_pt = size(pt,1)
      g = 0.0d0
      h = 0.0d0
      i = 0.0d0
      do a = 1,self%n_shell
         call get_shell_(self,sh,a,fa,la)
         call create_(sh_grid,n_pt,sh%n_comp,3)
         call create_(sh_grid1,n_pt,sh%n_comp,3)
         call create_(sh_grid0,n_pt,sh%n_comp)
         call make_laplacian_grid_(sh,sh_grid,sh_grid1,sh_grid0,pt)
         orb_a => orb(fa:la)
         g(:,1) = g(:,1) + matmul(sh_grid(:,:,1),orb_a)
         g(:,2) = g(:,2) + matmul(sh_grid(:,:,2),orb_a)
         g(:,3) = g(:,3) + matmul(sh_grid(:,:,3),orb_a)
         h(:,1) = h(:,1) + matmul(sh_grid1(:,:,1),orb_a)
         h(:,2) = h(:,2) + matmul(sh_grid1(:,:,2),orb_a)
         h(:,3) = h(:,3) + matmul(sh_grid1(:,:,3),orb_a)
         i = i + matmul(sh_grid0,orb_a)
         call destroy_(sh_grid1)
         call destroy_(sh_grid0)
         call destroy_(sh_grid)
         call destroy_ptr_part_(sh)
      end do

   end subroutine

   subroutine make_laplacian_orbital_grid_c(self,g,h,i,orb,pt)
    type(mol_type) :: self
    ! Evaluate the laplacian orbital density grid "g", as well as the nabla
    ! orbital grid "h" and the orbital grid "i" for *one* AO-basis orbital
    ! vector "orb" on a set of grid points "pt"
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: g,h
      complex(kind=kind((1.0d0,1.0d0))), dimension(:) :: i
      complex(kind=kind((1.0d0,1.0d0))), dimension(:), intent(in), target :: orb
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: pt
      type(shell1_type) :: sh
      real(kind=kind(1.0d0)), dimension(:,:,:), pointer :: sh_grid,sh_grid1
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: sh_grid0
      complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: orb_a
      integer(kind=kind(1)) :: n_pt,fa,la,a

   call ensure_(tonto,self%basis_info_made,"MOL:make_laplacian_orbital_grid_c ... no basis info")
   call ensure_(tonto,associated(self%atom),"MOL:make_laplacian_orbital_grid_c ... no atom list")
   call ensure_(tonto,size(g,1)==size(pt,1),"MOL:make_laplacian_orbital_grid_c ... grid array has wrong 1st dimension")
   call ensure_(tonto,size(g,2)==3,"MOL:make_laplacian_orbital_grid_c ... grid array has wrong 2nd dimension")
   call ensure_(tonto,size(h,1)==size(pt,1),"MOL:make_laplacian_orbital_grid_c ... grid array has wrong 1st dimension")
   call ensure_(tonto,size(h,2)==3,"MOL:make_laplacian_orbital_grid_c ... grid array has wrong 2nd dimension")
   call ensure_(tonto,size(i)==size(pt,1),"MOL:make_laplacian_orbital_grid_c ... grid array has wrong 1st dimension")
      n_pt = size(pt,1)
      g = 0.0d0
      h = 0.0d0
      i = 0.0d0
      do a = 1,self%n_shell
         call get_shell_(self,sh,a,fa,la)
         call create_(sh_grid,n_pt,sh%n_comp,3)
         call create_(sh_grid1,n_pt,sh%n_comp,3)
         call create_(sh_grid0,n_pt,sh%n_comp)
         call make_laplacian_grid_(sh,sh_grid,sh_grid1,sh_grid0,pt)
         orb_a => orb(fa:la)
         g(:,1) = g(:,1) + matmul(sh_grid(:,:,1),orb_a)
         g(:,2) = g(:,2) + matmul(sh_grid(:,:,2),orb_a)
         g(:,3) = g(:,3) + matmul(sh_grid(:,:,3),orb_a)
         h(:,1) = h(:,1) + matmul(sh_grid1(:,:,1),orb_a)
         h(:,2) = h(:,2) + matmul(sh_grid1(:,:,2),orb_a)
         h(:,3) = h(:,3) + matmul(sh_grid1(:,:,3),orb_a)
         i = i + matmul(sh_grid0,orb_a)
         call destroy_(sh_grid1)
         call destroy_(sh_grid0)
         call destroy_(sh_grid)
         call destroy_ptr_part_(sh)
      end do

   end subroutine

   subroutine make_grad_rho_on_rho_grid(self)
    type(mol_type) :: self
    ! Work out abs(grad(rho))/rho on a set of grid points "pt".
    ! A Gnuplot ascii file is also generated.
      type(archive_type) :: arch
      real(kind=kind(1.0d0)), dimension(:), pointer :: grid
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: pt

      call ensure_(tonto,associated(self%grid),"MOL:make_grad_rho_on_rho_grid ... no grid")
      call create_(grid,self%grid%n_pt)
      call create_(pt,self%grid%n_pt,3); call make_points_(self%grid,pt)
      call make_grad_rho_on_rho_grid_(self,grid,pt)
      call set_(arch,self%name,"grad_rho_on_rho_grid",format="ascii")
      call write_gnuplot_(arch,grid, self%grid%n_x, self%grid%n_y, self%grid%n_z)
      call destroy_(pt)
      call destroy_(grid)

   end subroutine

   subroutine make_grad_rho_on_rho_grid_1(self,grid,pt)
    type(mol_type) :: self
    ! Work out abs(grad(rho))/rho on a set of grid points "pt".
      real(kind=kind(1.0d0)), dimension(:) :: grid
      real(kind=kind(1.0d0)), dimension(:,:) :: pt
      real(kind=kind(1.0d0)), dimension(:), pointer :: rho
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: grad_rho
      integer(kind=kind(1)) :: n_pt,i

      call ensure_(tonto,size(pt,1)==size(grid),"MOL:make_grad_rho_on_rho_grid_1 ... array dimensions do not match")
      grid = 0.0d0
      n_pt = size(pt,1)
      call create_(rho,n_pt)
      call make_density_grid_(self,rho,pt)
      grid=0.0d0
      call create_(grad_rho,n_pt,3)
      call make_nabla_density_grid_(self,grad_rho,pt)
      do i=1,n_pt
        if (rho(i)>0.0d0) then
          grid(i)=sqrt(dot_product(grad_rho(i,:),grad_rho(i,:)))/rho(i)
        else
          grid(i)=0.0d0
        end if
      end do
      call destroy_(grad_rho)
      call destroy_(rho)

   end subroutine

   subroutine make_ELF_grid(self)
    type(mol_type) :: self
    ! Work out the Electron Localisation Function (ELF) density on ".grid"
    ! using ".natural orbitals" and the ".occupation_numbers" vector.
    ! Grid, basis set, and natural orbitals are required to exist.
    ! A Gnuplot ascii file is also generated.
      type(archive_type) :: arch
      real(kind=kind(1.0d0)), dimension(:), pointer :: ELF_grid
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: pt

   call ensure_(tonto,associated(self%grid),"MOL:make_ELF_grid ... no grid")
      call create_(ELF_grid,self%grid%n_pt)
      call create_(pt,self%grid%n_pt,3); call make_points_(self%grid,pt)
      call make_ELF_grid_(self,ELF_grid,pt)
      call set_(arch,self%name,"ELF_grid",format="ascii")
      call write_gnuplot_(arch,ELF_grid, self%grid%n_x, self%grid%n_y, self%grid%n_z)
      call destroy_(pt)
      call destroy_(ELF_grid)

   end subroutine

   subroutine make_ELF_grid_1(self,ELF_grid,pt)
    type(mol_type) :: self
    ! Work out the Electron Localisation Function (ELF) density "g" on a set of
    ! grid points "pt", using ".natural orbitals" and the ".occupation" number
    ! vector.
     real(kind=kind(1.0d0)), dimension(:) :: ELF_grid
     real(kind=kind(1.0d0)), dimension(:,:) :: pt

   call ensure_(tonto,associated(self%natural_orbitals),"MOL:make_ELF_grid_1 ... no natural orbitals")
   call ensure_(tonto,any_created_(self%natural_orbitals),"MOL:make_ELF_grid_1 ... no natural orbitals")
     if (number_kind_(self%natural_orbitals) == "real") then
       call make_ELF_grid_r_(self,ELF_grid,pt)
     else
       call make_ELF_grid_c_(self,ELF_grid,pt)
     end if

   end subroutine

   subroutine make_ELF_grid_r(self,ELF_grid,pt)
    type(mol_type) :: self
    ! Work out the Electron Localisation Function (ELF) density on a set of grid
    ! points "pt", using ".natural orbitals" and the ".occupation_numbers"
    ! vector.  Grid, basis set, and natural orbitals are required to exist.
      real(kind=kind(1.0d0)), dimension(:) :: ELF_grid
      real(kind=kind(1.0d0)), dimension(:,:) :: pt
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: DD
      real(kind=kind(1.0d0)), dimension(:), pointer :: mo_val,gx,gy,gz,rho_s,rho_sx,rho_sy,rho_sz,tau,d_s,d0_s,NO
      real(kind=kind(1.0d0)) :: occu,factor,mo_val_n,gx_n,gy_n,gz_n,occ_mo
      integer(kind=kind(1)) :: i,n_occ,n,n_pt

   call ensure_(tonto,self%basis_info_made,"MOL:make_ELF_grid_r ... no basis info")
   call ensure_(tonto,associated(self%natural_orbitals),"MOL:make_ELF_grid_r ... no natural orbitals")
      ELF_grid = 0.0d0
      n_pt = size(pt,1)
      call create_(rho_s,n_pt);  rho_s  = 0.0d0
      call create_(tau,n_pt);    tau    = 0.0d0
      call create_(rho_sx,n_pt); rho_sx = 0.0d0
      call create_(rho_sy,n_pt); rho_sy = 0.0d0
      call create_(rho_sz,n_pt); rho_sz = 0.0d0
      call create_(NO,n_pt)
      call create_(DD,n_pt,3)
      n_occ = no_of_occupied_NOs_(self)
      do i = 1,n_occ
         call make_nabla_orbital_grid_r_(self,DD,NO,self%natural_orbitals%restricted(:,i), pt)
         mo_val => NO(:)
         gx     => DD(:,1)
         gy     => DD(:,2)
         gz     => DD(:,3)
         occu   = 0.50d0*self%occupation_numbers%restricted(i)
         do n = 1,n_pt
           mo_val_n  = mo_val(n)
           gx_n      = gx(n)
           gy_n      = gy(n)
           gz_n      = gz(n)
           occ_mo    = occu*mo_val_n
           rho_s(n)  = rho_s(n)  + occ_mo*mo_val_n
           tau(n)    = tau(n)    + occu*(gx_n*gx_n+gy_n*gy_n+gz_n*gz_n)
           rho_sx(n) = rho_sx(n) + occ_mo*gx_n  ! this is half grad_x rho_s
           rho_sy(n) = rho_sy(n) + occ_mo*gy_n
           rho_sz(n) = rho_sz(n) + occ_mo*gz_n
         end do
      end do
      call destroy_(DD)
      call destroy_(NO)
      call create_(d_s,n_pt)
      d_s = tau - (rho_sx*rho_sx + rho_sy*rho_sy + rho_sz*rho_sz)/rho_s
      call destroy_(rho_sz); call destroy_(rho_sy); call destroy_(rho_sx); call destroy_(tau)
      call create_(d0_s,n_pt)
      factor = 0.6d0*(6.0d0*3.141592653589793d0*3.141592653589793d0)**(2.0d0/3.0d0)
      d0_s = factor*rho_s**(5.0d0/3.0d0)
      call destroy_(rho_s)
      ELF_grid = 1.0d0/(1.0d0+(d_s*d_s)/(d0_s*d0_s))
       ! this could be more efficient if the d0_s*d0_s is written
       ! factor2*rho_s**(2.0d0+5.0d0/3.0d0)
      call destroy_(d0_s); call destroy_(d_s)

   end subroutine

   subroutine make_ELF_grid_c(self,ELF_grid,pt)
    type(mol_type) :: self
    ! Work out the Electron Localisation Function (ELF) density on a set of grid
    ! points "pt", using ".natural orbitals" and the ".occupation_numbers"
    ! vector.  Grid, basis set, and natural orbitals are required to exist.
     real(kind=kind(1.0d0)), dimension(:) :: ELF_grid
     real(kind=kind(1.0d0)), dimension(:,:) :: pt
     complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: mo_val,gx,gy,gz,NO
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: DD
     real(kind=kind(1.0d0)), dimension(:), pointer :: rho_s,rho_sx,rho_sy,rho_sz,tau,d_s,d0_s
     real(kind=kind(1.0d0)) :: occu,factor
     integer(kind=kind(1)) :: n_occ,n,n_pt

   call ensure_(tonto,self%basis_info_made,"MOL:make_ELF_grid_c ... no basis set")
   call ensure_(tonto,associated(self%natural_orbitals),"MOL:make_ELF_grid_c ... no natural orbitals")
     ELF_grid = 0.0d0
     n_pt = size(pt,1)
     call create_(rho_s,n_pt);  rho_s  = 0.0d0
     call create_(tau,n_pt);    tau    = 0.0d0
     call create_(rho_sx,n_pt); rho_sx = 0.0d0
     call create_(rho_sy,n_pt); rho_sy = 0.0d0
     call create_(rho_sz,n_pt); rho_sz = 0.0d0
     call create_(NO,n_pt)
     call create_(DD,n_pt,3)
     n_occ = no_of_occupied_NOs_(self)
     do n = 1,n_occ
       call make_nabla_orbital_grid_c_(self,DD,NO,self%natural_orbitals%restricted_complex(:,n), pt)
       mo_val => NO(:)
       gx     => DD(:,1)
       gy     => DD(:,2)
       gz     => DD(:,3)
       occu   = 0.50d0*self%occupation_numbers%restricted(n)
       rho_s  = rho_s  + occu*conjg(mo_val)*mo_val
       tau    = tau    + occu*(conjg(gx)*gx+conjg(gy)*gy+conjg(gz)*gz)
       rho_sx = rho_sx + occu*conjg(gx)*mo_val  ! this is half grad_x rho_s
       rho_sy = rho_sy + occu*conjg(gy)*mo_val
       rho_sz = rho_sz + occu*conjg(gz)*mo_val
     end do
     call destroy_(DD)
     call destroy_(NO)
     call create_(d_s,n_pt)
     d_s = tau - (rho_sx*rho_sx + rho_sy*rho_sy + rho_sz*rho_sz)/rho_s
     call destroy_(rho_sz); call destroy_(rho_sy); call destroy_(rho_sx); call destroy_(tau)
     call create_(d0_s,n_pt)
     factor = 0.6d0*(6.0d0*3.141592653589793d0*3.141592653589793d0)**(2.0d0/3.0d0)
     d0_s = factor*rho_s**(5.0d0/3.0d0)
     call destroy_(rho_s)
     ELF_grid = 1.0d0/(1.0d0+(d_s*d_s)/(d0_s*d0_s))
     call destroy_(d0_s); call destroy_(d_s)

   end subroutine

   subroutine make_Tsirelson_ELF_grid(self)
    type(mol_type) :: self
    ! Work out the Tsirelson-Stash Electron Localisation Function (ELF) density
    ! on ".grid" using ".natural orbitals" and the ".occupation_numbers" vector.
    ! Grid, basis set, and natural orbitals are required to exist.  A Gnuplot
    ! ascii file is also generated.
      type(archive_type) :: arch
      real(kind=kind(1.0d0)), dimension(:), pointer :: ELF_grid
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: pt

   call ensure_(tonto,associated(self%grid),"MOL:make_Tsirelson_ELF_grid ... no grid")
      call create_(ELF_grid,self%grid%n_pt)
      call create_(pt,self%grid%n_pt,3); call make_points_(self%grid,pt)
      call make_Tsirelson_ELF_grid_(self,ELF_grid,pt)
      call set_(arch,self%name,"Tsirelson-Stash_ELF_grid",format="ascii")
      call write_gnuplot_(arch,ELF_grid, self%grid%n_x, self%grid%n_y, self%grid%n_z)
      call destroy_(pt)
      call destroy_(ELF_grid)

   end subroutine

   subroutine make_Tsirelson_ELF_grid_1(self,ELF_grid,pt)
    type(mol_type) :: self
    ! Work out the Tsirelson-Stash Electron Localisation Function (ELF) density
    ! on a series of grid points "pt" ".natural orbitals" and the
    ! ".occupation_numbers" vector.
     real(kind=kind(1.0d0)), dimension(:) :: ELF_grid
     real(kind=kind(1.0d0)), dimension(:,:) :: pt

   call ensure_(tonto,associated(self%natural_orbitals),"MOL:make_Tsirelson_ELF_grid_1 ... no natural orbitals")
   call ensure_(tonto,any_created_(self%natural_orbitals),"MOL:make_Tsirelson_ELF_grid_1 ... no natural orbitals")
     if (number_kind_(self%natural_orbitals) == "real") then
       call make_Tsirelson_ELF_grid_r_(self,ELF_grid,pt)
     else
       call make_Tsirelson_ELF_grid_c_(self,ELF_grid,pt)
     end if

   end subroutine

   subroutine make_Tsirelson_ELF_grid_r(self,ELF_grid,pt)
    type(mol_type) :: self
    ! Work out the Tsirelson-Stash Electron Localisation Function (ELF) density
    ! on a series of grid points "pt" ".natural orbitals" and the
    ! ".occupation_numbers" vector.
      real(kind=kind(1.0d0)), dimension(:) :: ELF_grid
      real(kind=kind(1.0d0)), dimension(:,:) :: pt
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: DD,LL
      real(kind=kind(1.0d0)), dimension(:), pointer :: mo_val,gx,gy,gz,lx,ly,lz,rho,rho_x,rho_y,rho_z,lap,d_p,d_0,NO
      real(kind=kind(1.0d0)) :: occu,factor,mo_val_n,gx_n,gy_n,gz_n,lx_n,ly_n,lz_n,occ_mo
      integer(kind=kind(1)) :: i,n_occ,n,n_pt

   call ensure_(tonto,self%basis_info_made,"MOL:make_Tsirelson_ELF_grid_r ... no basis info")
   call ensure_(tonto,associated(self%natural_orbitals),"MOL:make_Tsirelson_ELF_grid_r ... no natural orbitals")
      ELF_grid = 0.0d0
      n_pt = size(pt,1)
      call create_(rho,n_pt);   rho   = 0.0d0
      call create_(rho_x,n_pt); rho_x = 0.0d0
      call create_(rho_y,n_pt); rho_y = 0.0d0
      call create_(rho_z,n_pt); rho_z = 0.0d0
      call create_(lap,n_pt);   lap   = 0.0d0
      call create_(NO,n_pt)
      call create_(DD,n_pt,3)
      call create_(LL,n_pt,3)
      n_occ = no_of_occupied_NOs_(self)
      do i = 1,n_occ
         call make_laplacian_orbital_grid_r_(self,LL,DD,NO,self%natural_orbitals%restricted(:,i), pt)
         mo_val => NO(:)
         gx => DD(:,1); gy => DD(:,2); gz => DD(:,3)
         lx => LL(:,1); ly => LL(:,2); lz => LL(:,3)
         occu = self%occupation_numbers%restricted(i)
         do n = 1,n_pt
           mo_val_n = mo_val(n)
           gx_n = gx(n); gy_n = gy(n); gz_n = gz(n)
           lx_n = lx(n); ly_n = ly(n); lz_n = lz(n)
           occ_mo   = occu*mo_val_n                         ! this is n_i phi_i
           rho(n)   = rho(n)   + occ_mo*mo_val_n            ! this is rho
           rho_x(n) = rho_x(n) + occ_mo*gx_n                ! this is half grad_x rho
           rho_y(n) = rho_y(n) + occ_mo*gy_n
           rho_z(n) = rho_z(n) + occ_mo*gz_n
           lap(n)   = lap(n)   + occ_mo*(lx_n+ly_n+lz_n) &
 ! this is half nabla^2 rho
                               + occu*(gx_n*gx_n+gy_n*gy_n+gz_n*gz_n)
         end do
      end do
      call destroy_(LL)
      call destroy_(DD)
      call destroy_(NO)
      call create_(d_0,n_pt)
      factor = 0.3d0*(3.0d0*3.141592653589793d0*3.141592653589793d0)**(2.0d0/3.0d0)
      d_0 = factor*rho**(5.0d0/3.0d0)
      call create_(d_p,n_pt)
      factor = 4.0d0/9.0d0
      d_p = d_0 - factor*(rho_x*rho_x + rho_y*rho_y + rho_z*rho_z)/rho &
                + lap/3.0d0
      call destroy_(lap)
      call destroy_(rho_z); call destroy_(rho_y); call destroy_(rho_x)
      call destroy_(rho)
      ELF_grid = 1.0d0/(1.0d0+(d_p*d_p)/(d_0*d_0))
      call destroy_(d_p); call destroy_(d_0)

   end subroutine

   subroutine make_Tsirelson_ELF_grid_c(self,ELF_grid,pt)
    type(mol_type) :: self
    ! Work out the Tsirelson-Stash Electron Localisation Function (ELF) density
    ! on a series of grid points "pt" ".natural orbitals" and the
    ! ".occupation_numbers" vector.
      real(kind=kind(1.0d0)), dimension(:) :: ELF_grid
      real(kind=kind(1.0d0)), dimension(:,:) :: pt
      complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: mo_val,gx,gy,gz,lx,ly,lz,NO
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: DD,LL
      real(kind=kind(1.0d0)), dimension(:), pointer :: rho,rho_x,rho_y,rho_z,lap,d_0,d_p
      real(kind=kind(1.0d0)) :: occu,factor
      complex(kind=kind((1.0d0,1.0d0))) :: mo_val_n,gx_n,gy_n,gz_n,lx_n,ly_n,lz_n,occ_mo
      integer(kind=kind(1)) :: n_occ,n_pt,n,i

   call ensure_(tonto,self%basis_info_made,"MOL:make_Tsirelson_ELF_grid_c ... no basis info")
   call ensure_(tonto,associated(self%natural_orbitals),"MOL:make_Tsirelson_ELF_grid_c ... no natural orbitals")
      ELF_grid = 0.0d0
      n_pt = size(pt,1)
      call create_(rho,n_pt);   rho   = 0.0d0
      call create_(rho_x,n_pt); rho_x = 0.0d0
      call create_(rho_y,n_pt); rho_y = 0.0d0
      call create_(rho_z,n_pt); rho_z = 0.0d0
      call create_(lap,n_pt);   lap   = 0.0d0
      call create_(NO,n_pt)
      call create_(DD,n_pt,3)
      call create_(LL,n_pt,3)
      n_occ = no_of_occupied_NOs_(self)
      do i = 1,n_occ
         call make_laplacian_orbital_grid_c_(self,LL,DD,NO,self%natural_orbitals%restricted_complex(:,i), pt)
         mo_val => NO(:)
         gx => DD(:,1); gy => DD(:,2); gz => DD(:,3)
         lx => LL(:,1); ly => LL(:,2); lz => LL(:,3)
         occu = self%occupation_numbers%restricted(i)
         do n = 1,n_pt
           mo_val_n = mo_val(n)
           gx_n = gx(n); gy_n = gy(n); gz_n = gz(n)
           lx_n = lx(n); ly_n = ly(n); lz_n = lz(n)
           occ_mo   = occu*mo_val_n                      ! this is n_i phi_i
           rho(n)   = rho(n)   + occ_mo*conjg(mo_val_n)  ! this is rho
           rho_x(n) = rho_x(n) + occ_mo*conjg(gx_n)      ! this is half grad_x rho
           rho_y(n) = rho_y(n) + occ_mo*conjg(gy_n)
           rho_z(n) = rho_z(n) + occ_mo*conjg(gz_n)
                                                         ! this is half nabla^2 rho
           lap(n)   = lap(n)   + occ_mo*(conjg(lx_n)+conjg(ly_n)+conjg(lz_n)) &
                               + occu*(conjg(gx_n)*gx_n+conjg(gy_n)*gy_n+conjg(gz_n)*gz_n)
         end do
      end do
      call destroy_(LL)
      call destroy_(DD)
      call destroy_(NO)
      call create_(d_0,n_pt)
      factor = 0.3d0*(3.0d0*3.141592653589793d0*3.141592653589793d0)**(2.0d0/3.0d0)
      d_0 = factor*rho**(5.0d0/3.0d0)
      call create_(d_p,n_pt)
      factor = 4.0d0/9.0d0
      d_p = d_0 - factor*(rho_x*rho_x + rho_y*rho_y + rho_z*rho_z)/rho &
                + lap/3.0d0
      call destroy_(lap)
      call destroy_(rho_z); call destroy_(rho_y); call destroy_(rho_x)
      call destroy_(rho)
      ELF_grid = 1.0d0/(1.0d0+(d_p*d_p)/(d_0*d_0))
      call destroy_(d_p); call destroy_(d_0)

   end subroutine

   subroutine make_fermi_mobility_grid(self)
    type(mol_type) :: self
    ! Work out the Luken's fermi mobility density on ".grid" using ".natural
    ! orbitals" and the ".occupation_numbers" vector. A Gnuplot ascii file is
    ! generated.
      type(archive_type) :: arch
      real(kind=kind(1.0d0)), dimension(:), pointer :: fermi_grid
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: pt

   call ensure_(tonto,associated(self%grid),"MOL:make_fermi_mobility_grid ... no grid")
      call create_(fermi_grid,self%grid%n_pt)
      call create_(pt,self%grid%n_pt, 3 ); call make_points_(self%grid,pt)
      call make_fermi_mobility_grid_r_(self,fermi_grid,pt)
      call set_(arch,self%name,"fermi_mobility_grid",format="ascii")
      call write_gnuplot_(arch,fermi_grid, self%grid%n_x, self%grid%n_y, self%grid%n_z)
      call destroy_(pt)
      call destroy_(fermi_grid)

   end subroutine

   subroutine make_fermi_mobility_grid_1(self,f,pt)
    type(mol_type) :: self
    ! Evaluate the Luken's Fermi hole mobility grid "f" on a set of grid points "pt".
     real(kind=kind(1.0d0)), dimension(:) :: f
     real(kind=kind(1.0d0)), dimension(:,:) :: pt

   call ensure_(tonto,associated(self%natural_orbitals),"MOL:make_fermi_mobility_grid_1 ... no natural orbitals")
   call ensure_(tonto,any_created_(self%natural_orbitals),"MOL:make_fermi_mobility_grid_1 ... no natural orbitals")
     if (number_kind_(self%natural_orbitals) == "real") then
       call make_fermi_mobility_grid_r_(self,f,pt)
     else
       call die_(tonto,"MOL:make_fermi_mobility_grid_1 ... code not written yet")
     end if

   end subroutine

   subroutine make_fermi_mobility_grid_r(self,f,pt)
    type(mol_type) :: self
    ! Evaluate the Luken's Fermi hole mobility grid "f" on a set of grid points "pt".
     real(kind=kind(1.0d0)), dimension(:), intent(out) :: f
     real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: pt
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: del_gi
     real(kind=kind(1.0d0)), dimension(:), pointer :: dg,gi,rho,dgx,dgy,dgz,orb
     integer(kind=kind(1)) :: n,n_pt,i,n_occ
     real(kind=kind(1.0d0)) :: delx,dely,delz,fac

   call ensure_(tonto,self%basis_info_made,"MOL:make_fermi_mobility_grid_r ... no basis info")
   call ensure_(tonto,associated(self%natural_orbitals),"MOL:make_fermi_mobility_grid_r ... no natural orbitals")
     n_pt = size(pt,1)
     n_occ = no_of_occupied_NOs_(self)
     call create_(dg,n_pt);    dg  = 0.0d0
     call create_(dgx,n_pt);   dgx = 0.0d0
     call create_(dgy,n_pt);   dgy = 0.0d0
     call create_(dgz,n_pt);   dgz = 0.0d0
     call create_(gi,n_pt)
     call create_(del_gi,n_pt,3)
     do i=1, n_occ
       orb => self%natural_orbitals%restricted(:,i)
       call make_nabla_orbital_grid_r_(self,del_gi,gi,orb,pt)
       do n=1,n_pt
         delx = del_gi(n,1)
         dely = del_gi(n,2)
         delz = del_gi(n,3)
         dg(n)  = dg(n)  + delx*delx+dely*dely+delz*delz
         dgx(n) = dgx(n) + delx*gi(n)
         dgy(n) = dgy(n) + dely*gi(n)
         dgz(n) = dgz(n) + delz*gi(n)
       end do
     end do
     call destroy_(del_gi)
     call destroy_(gi)
     call create_(rho,n_pt)
     call make_density_grid_(self,rho,pt)
     fac = (3.0d0*3.141592653589793d0/4.0d0) * 0.50d0**0.66666666666666666666667d0
     f = 1.0d0/rho * (dg - 2.0d0*(dgx*dgx + dgy*dgy + dgz*dgz)/rho) - fac * rho**0.66666666666666666666667d0
     call destroy_(rho)
     call destroy_(dgz)
     call destroy_(dgy)
     call destroy_(dgx)
     call destroy_(dg)

   end subroutine

   subroutine make_true_fermi_mobility_grid(self)
    type(mol_type) :: self
    ! Work out the the true fermi mobility density "f" on a series of grid points
    ! "pt" using ".natural orbitals" and the ".occupation_numbers" vector. The
    ! true function differs from that above in that the kinetic energy density is
    ! evaluated exactly.
      type(archive_type) :: arch
      real(kind=kind(1.0d0)), dimension(:), pointer :: fermi_grid
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: pt

   call ensure_(tonto,associated(self%grid),"MOL:make_true_fermi_mobility_grid ... no grid")
      call create_(fermi_grid,self%grid%n_pt)
      call create_(pt,self%grid%n_pt, 3 ); call make_points_(self%grid,pt)
      call make_true_fermi_mobil_grid_r_(self,fermi_grid,pt)
      call set_(arch,self%name,"true_fermi_mobility_grid",format="ascii")
      call write_gnuplot_(arch,fermi_grid, self%grid%n_x, self%grid%n_y, self%grid%n_z)
      call destroy_(pt)
      call destroy_(fermi_grid)

   end subroutine

   subroutine make_true_fermi_mobility_grid_1(self,f,pt)
    type(mol_type) :: self
    ! Work out the the true fermi mobility density "f" on a series of grid points
    ! "pt" using ".natural orbitals" and the ".occupation_numbers" vector. The
    ! true function differs from that above in that the kinetic energy density is
    ! evaluated exactly.
     real(kind=kind(1.0d0)), dimension(:) :: f
     real(kind=kind(1.0d0)), dimension(:,:) :: pt

   call ensure_(tonto,associated(self%natural_orbitals),"MOL:make_true_fermi_mobility_grid_1 ... no natural orbitals")
   call ensure_(tonto,any_created_(self%natural_orbitals),"MOL:make_true_fermi_mobility_grid_1 ... no natural orbitals")
     if (number_kind_(self%natural_orbitals) == "real") then
       call make_true_fermi_mobil_grid_r_(self,f,pt)
     else
       call die_(tonto,"MOL:make_true_fermi_mobility_grid_1 ... code not written yet")
     end if

   end subroutine

   subroutine make_true_fermi_mobil_grid_r(self,f,pt)
    type(mol_type) :: self
    ! Evaluate the true Fermi hole mobility grid "f" on a set of grid points
    ! "pt".  The true function differs from that above in that the kinetic energy
    ! density is evaluated exactly.
    ! NOTE: this routine is only valid for single determinant wavefunctions.
     real(kind=kind(1.0d0)), dimension(:), intent(out) :: f
     real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: pt
     real(kind=kind(1.0d0)), dimension(:), pointer :: dg,rho,NO,K,gj,Tj
     real(kind=kind(1.0d0)), dimension(:), pointer :: dgx,dgy,dgz,gi
     integer(kind=kind(1)) :: n,n_pt,i,j,n_occ
     real(kind=kind(1.0d0)) :: delx,dely,delz
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: del_gi,g,T

   call ensure_(tonto,self%basis_info_made,"MOL:make_true_fermi_mobil_grid_r ... no basis info")
   call ensure_(tonto,associated(self%natural_orbitals),"MOL:make_true_fermi_mobil_grid_r ... no natural orbitals")
     n_pt = size(pt,1)
     n_occ = no_of_occupied_NOs_(self)
     call create_(dg,n_pt);    dg  = 0.0d0
     call create_(dgx,n_pt);   dgx = 0.0d0
     call create_(dgy,n_pt);   dgy = 0.0d0
     call create_(dgz,n_pt);   dgz = 0.0d0
     call create_(g,n_pt,n_occ)
     call create_(del_gi,n_pt,3)
      ! Assemble intermediate terms
     do i=1, n_occ
       NO => self%natural_orbitals%restricted(:,i)
       gi => g(:,i)
       call make_nabla_orbital_grid_r_(self,del_gi,gi,NO,pt)
       do n=1,n_pt
         delx = del_gi(n,1)
         dely = del_gi(n,2)
         delz = del_gi(n,3)
         dg(n)  = dg(n)  + delx*delx+dely*dely+delz*delz
         dgx(n) = dgx(n) + delx*gi(n)
         dgy(n) = dgy(n) + dely*gi(n)
         dgz(n) = dgz(n) + delz*gi(n)
       end do
     end do
     call destroy_(del_gi)
      ! Make kinetic energy matrix part
     call create_(K,n_pt);    K = 0.0d0
     call create_(T,self%n_bf,self%n_bf)
     call get_kinetic_matrix_(self,T)
     call change_basis_(T,self%natural_orbitals%restricted)
     do j=1,n_occ
       gj => g(:,j)
       Tj => T(:,j)
       do i=1,n_occ
         K(:) = K(:) + g(:,i)*gj(:)*Tj(i)
       end do
     end do
     call destroy_(T)
      ! Now do the final assembly
     call create_(rho,n_pt)
     call make_density_grid_(self,rho,pt)
     f = 1.0d0/rho * (dg - 2.0d0*(dgx*dgx+dgy*dgy+dgz*dgz)/rho - 2.0d0*K)
     call destroy_(rho)
     call destroy_(K)
     call destroy_(g)
     call destroy_(dgz)
     call destroy_(dgy)
     call destroy_(dgx)
     call destroy_(dg)

   end subroutine

   subroutine make_electric_potential_grid(self)
    type(mol_type) :: self
    ! Make the electric potential "pot_grid" on ".grid"
     type(archive_type) :: arch
     real(kind=kind(1.0d0)), dimension(:), pointer :: pot_grid
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: pt

   call ensure_(tonto,associated(self%grid),"MOL:make_electric_potential_grid ... no grid")
   call ensure_(tonto,self%basis_info_made,"MOL:make_electric_potential_grid ... no basis info")
   call ensure_(tonto,associated(self%density_matrix),"MOL:make_electric_potential_grid ... no density matrix")
   call ensure_(tonto,any_created_(self%density_matrix),"MOL:make_electric_potential_grid ... no density matrix")
     call create_(pot_grid,self%grid%n_pt)
     call create_(pt,self%grid%n_pt,3); call make_points_(self%grid,pt)
     call make_electric_potential_grid_(self,pot_grid,pt)
     call destroy_(pt)
     call set_(arch,self%name,"electric_potential_grid",format="ascii")
     call write_gnuplot_(arch,pot_grid, self%grid%n_x, self%grid%n_y, self%grid%n_z)
     call destroy_(pot_grid)

   end subroutine

   subroutine make_electric_potential_grid_1(self,pot_grid,pt)
    type(mol_type) :: self
    ! Make the electric potential "pot_grid" on a series of points "pt"
     real(kind=kind(1.0d0)), dimension(:) :: pot_grid
     real(kind=kind(1.0d0)), dimension(:,:) :: pt

   call ensure_(tonto,self%basis_info_made,"MOL:make_electric_potential_grid_1 ... no basis info")
   call ensure_(tonto,associated(self%density_matrix),"MOL:make_electric_potential_grid_1 ... no density matrix")
   call ensure_(tonto,any_created_(self%density_matrix),"MOL:make_electric_potential_grid_1 ... no density matrix")
     call make_ao_density_matrix_(self)
     if (number_kind_(self%density_matrix) == "real") then
       call make_electric_potential_grid_r_(self,pot_grid,pt,self%density_matrix%restricted)
     else
       call make_electric_potential_grid_c_(self,pot_grid,pt,self%density_matrix%restricted_complex)
     end if

   end subroutine

   subroutine make_electric_potential_grid_r(self,pot_grid,pt,dens)
    type(mol_type) :: self
    ! Make the total electric potential "pot_grid" on a series of grid points
    ! "pt" using AO density matrix "dens"
     real(kind=kind(1.0d0)), dimension(:) :: pot_grid
     real(kind=kind(1.0d0)), dimension(:,:) :: pt
     real(kind=kind(1.0d0)), dimension(:,:), target :: dens
     real(kind=kind(1.0d0)), dimension(:), pointer :: elec_grid

     call nuclear_potential_(self,pot_grid,pt)
     call create_(elec_grid,size(pot_grid))
     call make_electronic_pot_grid_r_(self,elec_grid,pt,dens)
     call plus_(pot_grid,elec_grid)
     call destroy_(elec_grid)

   end subroutine

   subroutine make_electric_potential_grid_c(self,pot_grid,pt,dens)
    type(mol_type) :: self
    ! Make the total electric potential "pot_grid" on a series of grid points
    ! "pt" using AO density matrix "dens"
     real(kind=kind(1.0d0)), dimension(:) :: pot_grid
     real(kind=kind(1.0d0)), dimension(:,:) :: pt
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), target :: dens
     real(kind=kind(1.0d0)), dimension(:), pointer :: elec_grid

     call nuclear_potential_(self,pot_grid,pt)
     call create_(elec_grid,size(pot_grid))
     call make_electronic_pot_grid_c_(self,elec_grid,pt,dens)
     call plus_(pot_grid,elec_grid)
     call destroy_(elec_grid)

   end subroutine

   subroutine make_electronic_pot_grid_r(self,pot_grid,pt,dens)
    type(mol_type) :: self
    ! Make the electronic potential "pot_grid" on a series of grid points "pt"
    ! using AO density matrix "dens"
      real(kind=kind(1.0d0)), dimension(:) :: pot_grid
      real(kind=kind(1.0d0)), dimension(:,:) :: pt
      real(kind=kind(1.0d0)), dimension(:,:), target :: dens
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: Vab,Dba
      integer(kind=kind(1)) :: n_pt,q,fa,fb,la,lb,k
      type(shell2_type) :: sh

   call ensure_(tonto,self%basis_info_made,"MOL:make_electronic_pot_grid_r ... no basis info")
   call ensure_(tonto,associated(self%atom),"MOL:make_electronic_pot_grid_r ... no atom list")
      n_pt = size(pt,1)
      pot_grid = 0.0d0
      do q = 1,self%n_shell_pairs
         call get_precomp_shell_pair_(self,sh,q,fa,la,fb,lb)
         Dba => dens(fa:la,fb:lb)
         call create_(Vab,sh%a%n_comp,sh%b%n_comp)
         if (fa/=fb) then
           do k = 1,n_pt
             call get_nuc_(sh,Vab,1.0d0,pt(k,:))
             pot_grid(k) = pot_grid(k) - 2.0d0*sum(Vab*Dba)
           end do
         else
           do k = 1,n_pt
             call get_nuc_(sh,Vab,1.0d0,pt(k,:))
             pot_grid(k) = pot_grid(k) - sum(Vab*Dba)
           end do
         end if
         call destroy_(Vab)
         call destroy_ptr_part_(sh)
      end do

   end subroutine

   subroutine make_electronic_pot_grid_c(self,pot_grid,pt,dens)
    type(mol_type) :: self
    ! Make the electronic potential "pot_grid" on a series of grid points "pt"
    ! using AO density matrix "dens"
      real(kind=kind(1.0d0)), dimension(:) :: pot_grid
      real(kind=kind(1.0d0)), dimension(:,:) :: pt
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), target :: dens
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: Vab
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: Dba
      integer(kind=kind(1)) :: n_pt,q,fa,fb,la,lb,k
      type(shell2_type) :: sh

   call ensure_(tonto,self%basis_info_made,"MOL:make_electronic_pot_grid_c ... no basis info")
   call ensure_(tonto,associated(self%atom),"MOL:make_electronic_pot_grid_c ... no atom list")
      n_pt = size(pt,1)
      pot_grid = 0.0d0
      do q = 1,self%n_shell_pairs
         call get_shell_pair_(self,sh,q,fa,la,fb,lb)
         Dba => dens(fa:la,fb:lb)
         call create_(Vab,sh%a%n_comp,sh%b%n_comp)
         if (fa/=fb) then
           do k = 1,n_pt
             call make_nuclear_attraction_ints_(sh,Vab,pt(k,:))
             pot_grid(k) = pot_grid(k) - 2.0d0*sum(Vab*real(Dba,kind=kind(1.0d0)))
           end do
         else
           do k = 1,n_pt
             call make_nuclear_attraction_ints_(sh,Vab,pt(k,:))
             pot_grid(k) = pot_grid(k) - 2.0d0*sum(Vab*real(Dba,kind=kind(1.0d0)))
           end do
         end if
         call destroy_(Vab)
         call destroy_ptr_part_(sh)
      end do

   end subroutine

   subroutine make_j_density_grid(self)
    type(mol_type) :: self
    ! Make the current density on ".grid" using ".natural orbitals"
    ! and the ".occupation_numbers" vector.
    !    J(r) = J_p(r) + J_d(r)
      type(archive_type) :: arch
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: j,jp

   call ensure_(tonto,associated(self%grid),"MOL:make_j_density_grid ... no grid")
      call create_(j,self%grid%n_pt,3);  call get_jd_density_grid_(self,j)
      call create_(jp,self%grid%n_pt,3); call get_jp_density_grid_(self,jp)
      j = j + jp
      call destroy_(jp)
      call set_(arch,self%name,"current_density")
      call write_(arch,j)
      call set_(arch,self%name,"current_density",format="ascii")
      call write_gnuplot_(arch,j, self%grid%n_x, self%grid%n_y, self%grid%n_z)
      call set_(arch,self%name,"current_norm_density",format="ascii")
      call write_gnuplot_(arch,j, self%grid%n_x, self%grid%n_y, self%grid%n_z, norm=.true.)
      call set_(arch,self%name,"current_density,normalized",format="ascii")
      call write_gnuplot_(arch,j, self%grid%n_x, self%grid%n_y, self%grid%n_z, normalise=.true.)
      call destroy_(j)

   end subroutine

   subroutine get_jd_density_grid(self,jd)
    type(mol_type) :: self
    ! Set "jd" to the diamagnetic current density grid.
    ! If the archive file exists, read it; otherwise make it.
      real(kind=kind(1.0d0)), dimension(:,:) :: jd
      type(archive_type) :: arch

      call set_(arch,self%name,"jd_density_grid")
      if (.not. exists_(arch)) call make_jd_density_grid_(self)
      call read_(arch,jd)

   end subroutine

   subroutine make_jd_density_grid(self)
    type(mol_type) :: self
    ! Work out the diamagnetic current density on ".grid" using ".natural
    ! orbitals" and the ".occupation_numbers" number vector.
    !    J_d(r) = - (e^2/2m) (B x r) rho(r)
      type(archive_type) :: arch
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: jd,r
      real(kind=kind(1.0d0)), dimension(:), pointer :: rho

      call ensure_(tonto,associated(self%grid),"MOL:make_jd_density_grid ... no grid")
      nullify(r)
      if (norm_(self%B_field)<10.0d0**(-10)) then;   return; end if
      call create_(jd,self%grid%n_pt,3)
      call create_(rho,self%grid%n_pt)
      call create_(r,self%grid%n_pt,3)
      call make_points_(self%grid,r)
      jd(:,1) = self%B_field(2)*r(:,3) - self%B_field(3)*r(:,2)
      jd(:,2) = self%B_field(3)*r(:,1) - self%B_field(1)*r(:,3)
      jd(:,3) = self%B_field(1)*r(:,2) - self%B_field(2)*r(:,1)
      call make_density_grid_(self,rho,r)
      call destroy_(r)
      jd(:,1) = -0.50d0*jd(:,1)*rho
      jd(:,2) = -0.50d0*jd(:,2)*rho
      jd(:,3) = -0.50d0*jd(:,3)*rho
      call destroy_(rho)
      call set_(arch,self%name,"jd_density")
      call write_(arch,jd)
      call set_(arch,self%name,"jd_density",format="ascii")
      call write_gnuplot_(arch,jd, self%grid%n_x, self%grid%n_y, self%grid%n_z)
      call set_(arch,self%name,"jd_norm_density",format="ascii")
      call write_gnuplot_(arch,jd, self%grid%n_x, self%grid%n_y, self%grid%n_z, norm=.true.)
      call set_(arch,self%name,"jd_density,normalized",format="ascii")
      call write_gnuplot_(arch,jd, self%grid%n_x, self%grid%n_y, self%grid%n_z, normalise=.true.)
      call destroy_(jd)

   end subroutine

   subroutine get_jp_density_grid(self,jp)
    type(mol_type) :: self
    ! Set "jp" to the paramagnetic current density grid.
    ! If the archive file exists, read it; otherwise make it.
      real(kind=kind(1.0d0)), dimension(:,:) :: jp
      type(archive_type) :: arch

      call set_(arch,self%name,"jp_density_grid")
      if (.not. exists_(arch)) call make_jp_density_grid_(self)
      call read_(arch,jp)

   end subroutine

   subroutine make_jp_density_grid(self)
    type(mol_type) :: self
    ! Work out the paramagnetic current density on ".grid" using ".natural
    ! orbitals".  A Gnuplot ascii file is generated.
      type(archive_type) :: arch
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: jp
      logical(kind=kind(.true.)) :: complex

      call ensure_(tonto,associated(self%grid),"MOL:make_jp_density_grid ... no grid")
      call ensure_(tonto,self%basis_info_made,"MOL:make_jp_density_grid ... no basis info")
      call ensure_(tonto,associated(self%natural_orbitals),"MOL:make_jp_density_grid ... no natural orbitals")
      call ensure_(tonto,associated(self%natural_orbitals),"MOL:make_jp_density_grid ... no natural orbitals")
      complex = number_kind_(self%natural_orbitals) == "complex"
      call ensure_(tonto,complex,"MOL:make_jp_density_grid ... natural orbitals not complex")
      call make_ao_density_matrix_(self)
      call make_restricted_complex_NOs_(self)
      call create_(jp,self%grid%n_pt,3)
      call make_jp_density_grid_(self,jp)
      call set_(arch,self%name,"jp_density_grid")
      call write_(arch,jp)
      call set_(arch,self%name,"jp_density_grid",format="ascii")
      call write_gnuplot_(arch,jp, self%grid%n_x, self%grid%n_y, self%grid%n_z)
      call set_(arch,self%name,"jp_norm_density_grid",format="ascii")
      call write_gnuplot_(arch,jp, self%grid%n_x, self%grid%n_y, self%grid%n_z, norm=.true.)
      call set_(arch,self%name,"jp_density_grid,normalized",format="ascii")
      call write_gnuplot_(arch,jp, self%grid%n_x, self%grid%n_y, self%grid%n_z, normalise=.true.)
      call destroy_(jp)

   end subroutine

   subroutine make_jp_density_grid_1(self,jp)
    type(mol_type) :: self
    ! Work out the paramagnetic current density "jp" on ".grid" using ".natural
    ! orbitals" .
    !    J_p(r) = - (eh/m) Re [ \sum_i n_i \phi^*_i(r) \nabla \phi^*_i(r)
     real(kind=kind(1.0d0)), dimension(:,:) :: jp
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: pt
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: P
     complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: D
     integer(kind=kind(1)) :: n_occ,n,n_pt
     real(kind=kind(1.0d0)) :: occ

     call ensure_(tonto,associated(self%grid),"MOL:make_jp_density_grid_1 ... no grid")
     call ensure_(tonto,self%basis_info_made,"MOL:make_jp_density_grid_1 ... no basis info")
     call ensure_(tonto,associated(self%natural_orbitals),"MOL:make_jp_density_grid_1 ... no density matrix")
     jp = 0.0d0
     n_pt = self%grid%n_pt
     call create_(pt,n_pt,3); call make_points_(self%grid,pt)
     call create_(D,n_pt)
     call create_(P,n_pt,3)
     n_occ = no_of_occupied_NOs_(self)
     do n = 1,n_occ
       call make_nabla_orbital_grid_c_(self,P,D,self%natural_orbitals%restricted_complex(:,n), pt)
       occ = self%occupation_numbers%restricted(n)
       jp(:,1) = jp(:,1) - occ*aimag(conjg(D(:))*P(:,1))
       jp(:,2) = jp(:,2) - occ*aimag(conjg(D(:))*P(:,2))
       jp(:,3) = jp(:,3) - occ*aimag(conjg(D(:))*P(:,3))
     end do
     call destroy_(P)
     call destroy_(D)
     call destroy_(pt)

   end subroutine

   subroutine make_solenoidal_jp_grid(self)
    type(mol_type) :: self
    ! Make the solenoidal paramagnetic current density grid "jp" on ".grid"
     type(archive_type) :: arch
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: jp

      call ensure_(tonto,associated(self%grid),"MOL:make_solenoidal_jp_grid ... no grid")
      call ensure_(tonto,self%basis_info_made,"MOL:make_solenoidal_jp_grid ... no basis info")
     call make_ao_density_matrix_(self)
     call ensure_(tonto,created_(self%density_matrix,"restricted_complex"),"MOL:make_solenoidal_jp_grid ... no density matrix&
&")
     call create_(jp,self%grid%n_pt,3)
     call make_solenoidal_jp_grid_(self,jp,self%density_matrix%restricted_complex)
     call set_(arch,self%name,"solenoidal_jp_density_grid")
     call write_(arch,jp)
     call set_(arch,self%name,"solenoidal_jp_density_grid",format="ascii")
     call write_gnuplot_(arch,jp, self%grid%n_x, self%grid%n_y, self%grid%n_z)
     call set_(arch,self%name,"solenoidal_jp_norm_density_grid",format="ascii")
     call write_gnuplot_(arch,jp, self%grid%n_x, self%grid%n_y, self%grid%n_z, norm=.true.)
     call set_(arch,self%name,"solenoidal_jp_density_grid,normalized",format="ascii")
     call write_gnuplot_(arch,jp, self%grid%n_x, self%grid%n_y, self%grid%n_z, normalise=.true.)
     call destroy_(jp)

   end subroutine

   subroutine make_solenoidal_jp_grid_1(self,jp,dens)
    type(mol_type) :: self
    ! Make the solenoidal paramagnetic current density grid "jp" on ".grid"
    ! using AO density matrix "dens"
      real(kind=kind(1.0d0)), dimension(:,:) :: jp
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), target :: dens
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: Dba
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: pt,Jxab,Jyab,Jzab
      integer(kind=kind(1)) :: q,fa,fb,la,lb,k
      real(kind=kind(1.0d0)) :: fac
      type(shell2_type) :: sh

      call ensure_(tonto,associated(self%grid),"MOL:make_solenoidal_jp_grid_1 ... no grid")
      call ensure_(tonto,self%basis_info_made,"MOL:make_solenoidal_jp_grid_1 ... no basis info")
      call ensure_(tonto,associated(self%atom),"MOL:make_solenoidal_jp_grid_1 ... no atom list")
      call create_(pt,self%grid%n_pt,3); call make_points_(self%grid,pt)
      jp = 0.0d0
      do q = 1,self%n_shell_pairs
        call get_shell_pair_(self,sh,q,fa,la,fb,lb)
        Dba => dens(fb:lb,fa:la)
        fac = 2.0d0; if (fa==fb) fac=1.0d0
        call create_(Jxab,sh%a%n_comp,sh%b%n_comp)
        call create_(Jyab,sh%a%n_comp,sh%b%n_comp)
        call create_(Jzab,sh%a%n_comp,sh%b%n_comp)
        do k = 1,self%grid%n_pt
          call make_solenoidal_jp_ints_(sh,Jxab,Jyab,Jzab,pt(k,:))
          jp(k,1) = jp(k,1) - fac*trace_of_product_(Jxab,real(aimag(Dba),kind=kind(1.0d0)))
          jp(k,2) = jp(k,2) - fac*trace_of_product_(Jyab,real(aimag(Dba),kind=kind(1.0d0)))
          jp(k,3) = jp(k,3) - fac*trace_of_product_(Jzab,real(aimag(Dba),kind=kind(1.0d0)))
        end do
        call destroy_(Jxab)
        call destroy_(Jyab)
        call destroy_(Jzab)
        call destroy_ptr_part_(sh)
      end do
      call destroy_(pt)
      call zero_small_values_(jp,10.0d0**(-10))

   end subroutine

   subroutine make_irrotational_jp_grid(self)
    type(mol_type) :: self
    ! Make the irrotational paramagnetic current density grid "jp" on ".grid"
      type(archive_type) :: arch
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: jp

      call ensure_(tonto,.not. associated(self%grid),"MOL:make_irrotational_jp_grid ... no grid")
      call ensure_(tonto,self%basis_info_made,"MOL:make_irrotational_jp_grid ... no basis info")
      call make_ao_density_matrix_(self)
      call ensure_(tonto,created_(self%density_matrix,"restricted_complex"),"MOL:make_irrotational_jp_grid ... no density mat&
&rix")
      call create_(jp,self%grid%n_pt,3)
      call make_irrotational_jp_grid_(self,jp,self%density_matrix%restricted_complex)
      call set_(arch,self%name,"irrotational_jp_density_grid")
      call write_(arch,jp)
      call set_(arch,self%name,"irrotational_jp_density_grid",format="ascii")
      call write_gnuplot_(arch,jp, self%grid%n_x, self%grid%n_y, self%grid%n_z)
      call set_(arch,self%name,"irrotational_jp_norm_density_grid",format="ascii")
      call write_gnuplot_(arch,jp, self%grid%n_x, self%grid%n_y, self%grid%n_z, norm=.true.)
      call set_(arch,self%name,"irrotational_jp_density_grid,normalized",format="ascii")
      call write_gnuplot_(arch,jp, self%grid%n_x, self%grid%n_y, self%grid%n_z, normalise=.true.)
      call destroy_(jp)

   end subroutine

   subroutine make_irrotational_jp_grid_1(self,jp,dens)
    type(mol_type) :: self
    ! Make the irrotational paramagnetic current density grid "jp" on ".grid"
    ! using AO density matrix "dens"
      real(kind=kind(1.0d0)), dimension(:,:) :: jp
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), target :: dens
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: Dba
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: pt,Jxab,Jyab,Jzab
      integer(kind=kind(1)) :: q,fa,fb,la,lb,k
      real(kind=kind(1.0d0)) :: fac
      type(shell2_type) :: sh

      call ensure_(tonto,associated(self%grid),"MOL:make_irrotational_jp_grid_1 ... no grid")
      call ensure_(tonto,self%basis_info_made,"MOL:make_irrotational_jp_grid_1 ... no basis info")
      call ensure_(tonto,associated(self%atom),"MOL:make_irrotational_jp_grid_1 ... no atom list")
      call create_(pt,self%grid%n_pt,3); call make_points_(self%grid,pt)
      jp = 0.0d0
      do q = 1,self%n_shell_pairs
        call get_shell_pair_(self,sh,q,fa,la,fb,lb)
        Dba => dens(fb:lb,fa:la)
        fac = 2.0d0; if (fa==fb) fac=1.0d0
        call create_(Jxab,sh%a%n_comp,sh%b%n_comp)
        call create_(Jyab,sh%a%n_comp,sh%b%n_comp)
        call create_(Jzab,sh%a%n_comp,sh%b%n_comp)
        do k = 1,self%grid%n_pt
          call make_irrotational_jp_ints_(sh,Jxab,Jyab,Jzab,pt(k,:))
          jp(k,1) = jp(k,1) - fac*trace_of_product_(Jxab,real(aimag(Dba),kind=kind(1.0d0)))
          jp(k,2) = jp(k,2) - fac*trace_of_product_(Jyab,real(aimag(Dba),kind=kind(1.0d0)))
          jp(k,3) = jp(k,3) - fac*trace_of_product_(Jzab,real(aimag(Dba),kind=kind(1.0d0)))
        end do
        call destroy_(Jxab)
        call destroy_(Jyab)
        call destroy_(Jzab)
        call destroy_ptr_part_(sh)
      end do
      call destroy_(pt)
      call zero_small_values_(jp,10.0d0**(-10))

   end subroutine

   subroutine make_crystal_error_map(self)
    type(mol_type) :: self
    ! Work out the crystal_error map on ".grid". A Gnuplot ascii file is
    ! generated.
      type(archive_type) :: arch
      real(kind=kind(1.0d0)), dimension(:), pointer :: map

      call ensure_(tonto,associated(self%grid),"MOL:make_crystal_error_map ... no grid")
      call create_(map,self%grid%n_pt)
      call make_crystal_error_map_(self,map)
      call set_(arch,self%name,"crystal_error_map",format="ascii")
      call write_gnuplot_(arch,map, self%grid%n_x, self%grid%n_y, self%grid%n_z)
      call destroy_(map)

   end subroutine

   subroutine make_crystal_error_map_1(self,map)
    type(mol_type) :: self
    ! Make the crystal error "map".
      real(kind=kind(1.0d0)), dimension(:) :: map
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: pt
      integer(kind=kind(1)) :: n_pt

      call ensure_(tonto,associated(self%grid),"MOL:make_crystal_error_map_1 ... no grid")
      map = 0.0d0
      n_pt = self%grid%n_pt
      call create_(pt,n_pt,3); call make_points_(self%grid,pt)
      call make_crystal_error_map_(self%crystal,map,pt)
      call destroy_(pt)

   end subroutine

   subroutine make_stockholder_grid(self)
    type(mol_type) :: self
    ! Work out the hirschfeld stockholder function on ".grid" using
    ! ".natural_orbitals" and the ".occupation" number vector. A Gnuplot ascii
    ! file is generated.
      type(archive_type) :: arch
      real(kind=kind(1.0d0)), dimension(:), pointer :: density_grid
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: pt

      call ensure_(tonto,associated(self%grid),"MOL:make_stockholder_grid ... no grid")
      call ensure_(tonto,associated(self%cluster),"MOL:make_stockholder_grid ... no cluster data")
      call ensure_(tonto,self%cluster%info_made,"MOL:make_stockholder_grid ... no cluster info")
      call create_(density_grid,self%grid%n_pt)
      call create_(pt,self%grid%n_pt,3); call make_points_(self%grid,pt)
      call make_stockholder_grid_(self,density_grid,pt)
      call set_(arch,self%name,"stockholder_grid",format="ascii")
      call write_gnuplot_(arch,density_grid, self%grid%n_x, self%grid%n_y, self%grid%n_z)
      call destroy_(pt)
      call destroy_(density_grid)

   end subroutine

   subroutine make_stockholder_grid_1(self,density_grid,pt)
    type(mol_type) :: self
    ! Make the Hirschfeld stockholder "density_grid" for the supplied points "pt"
    ! from restricted real natural orbitals
     intent(in) :: self
     real(kind=kind(1.0d0)), dimension(:), intent(out) :: density_grid
     real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: pt
     real(kind=kind(1.0d0)), dimension(3) :: sphere_pos
      !diff :: real(kind=kind(1.0d0)), dimension(3)
     real(kind=kind(1.0d0)), dimension(:), pointer :: grid,rho_fragment
     real(kind=kind(1.0d0)) :: sphere_radius
      !min_exp,dist,cutoff :: real(kind=kind(1.0d0))
     integer(kind=kind(1)) :: n_pt, a

     call ensure_(tonto,associated(self%cluster),"MOL:make_stockholder_grid_1 ... no cluster data")
     call ensure_(tonto,self%cluster%info_made,"MOL:make_stockholder_grid_1 ... no cluster info")
     call ensure_(tonto,associated(self%coppensbasis) .or. associated(self%slaterbasis),"MOL:make_stockholder_grid_1 ... no a&
&tomic orbitals")
     n_pt = size(density_grid)
     call create_(grid,n_pt)
      ! Make the central position and radius of the cluster
     call make_enclosing_sphere_(pt,sphere_pos,sphere_radius)
      !cutoff = 10.0d0**(-30)
     call create_(rho_fragment,n_pt)
     rho_fragment = 0.0d0
     do a = 1,self%cluster%n_atoms
        if (.not. self%cluster%is_fragment_atom(a)) cycle
         !min_exp = .atom(a).min_basis_exponent
         !diff = .atom(a).pos - sphere_pos
         !dist = sqrt(dot_product(diff,diff)) - sphere_radius
         !if (.atom(a).atomic_number*exp(-min_exp*dist)> cutoff) then
         !if (.atom(a).density_at_radius(dist)> cutoff) then
         !if (dist>.atom(a).coppensbasis.interpolator.last_data_point) cycle
        call make_density_grid_(self%atom(a),grid,pt)
        rho_fragment = rho_fragment + grid
     end do
     density_grid = rho_fragment
     do a = 1,self%cluster%n_atoms
        if (self%cluster%is_fragment_atom(a)) cycle
         !min_exp = .atom(a).min_basis_exponent
         !diff = .atom(a).pos - sphere_pos
         !dist = sqrt(dot_product(diff,diff)) - sphere_radius
         !if (.atom(a).atomic_number*exp(-min_exp*dist)> cutoff) then
         !if (.atom(a).density_at_radius(dist)> cutoff) then
         !if (dist>.atom(a).coppensbasis.interpolator.last_data_point) cycle
        call make_density_grid_(self%atom(a),grid,pt)
        density_grid = density_grid + grid
     end do
      ! density_grid = rho_fragment/density_grid
     do a = 1,size(density_grid)
       if (density_grid(a)>epsilon(1.0d0)) then
         density_grid(a) = rho_fragment(a)/density_grid(a)
       else
         density_grid(a) = 0.0d0
       end if
     end do
     call destroy_(rho_fragment)
     call destroy_(grid)

   end subroutine

   subroutine make_div_jp_density_grid(self)
    type(mol_type) :: self
    ! Work out the divergence of the paramagnetic current density on ".grid"
    ! using ".natural orbitals".  A Gnuplot ascii file is generated.
      type(archive_type) :: arch
      real(kind=kind(1.0d0)), dimension(:), pointer :: div_jp

      call ensure_(tonto,associated(self%grid),"MOL:make_div_jp_density_grid ... no grid")
      call ensure_(tonto,self%basis_info_made,"MOL:make_div_jp_density_grid ... no basis info")
      call ensure_(tonto,associated(self%natural_orbitals),"MOL:make_div_jp_density_grid ... no natural orbitals")
      call ensure_(tonto,associated(self%natural_orbitals),"MOL:make_div_jp_density_grid ... no natural orbitals")
      call ensure_(tonto,number_kind_(self%natural_orbitals) == "complex","MOL:make_div_jp_density_grid ... natural orbitals &
&not complex")
      call make_ao_density_matrix_(self)
      call make_restricted_complex_NOs_(self)
      call create_(div_jp,self%grid%n_pt)
      call make_div_jp_density_grid_(self,div_jp)
      call set_(arch,self%name,"div_jp_density_grid")
      call write_(arch,div_jp)
      call set_(arch,self%name,"div_jp_density_grid",format="ascii")
      call write_gnuplot_(arch,div_jp, self%grid%n_x, self%grid%n_y, self%grid%n_z)
      call destroy_(div_jp)

   end subroutine

   subroutine make_div_jp_density_grid_1(self,div_jp)
    type(mol_type) :: self
    ! Work out the divergence of the paramagnetic current density "jp" on ".grid"
    ! using ".natural orbitals" .
    !  \nabla .  J_p(r) = - (eh/m) \sum_i n_i Im[ \phi^*_i(r) \nabla^2 \phi^_i(r) ]
     real(kind=kind(1.0d0)), dimension(:) :: div_jp
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: pt
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: L
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: P
     complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: D
     integer(kind=kind(1)) :: n_occ,n,n_pt
     real(kind=kind(1.0d0)) :: occ

     call ensure_(tonto,associated(self%grid),"MOL:make_div_jp_density_grid_1 ... no grid")
     call ensure_(tonto,self%basis_info_made,"MOL:make_div_jp_density_grid_1 ... no basis info")
     call ensure_(tonto,associated(self%natural_orbitals),"MOL:make_div_jp_density_grid_1 ... no density matrix")
     div_jp = 0.0d0
     n_pt = self%grid%n_pt
     call create_(pt,n_pt,3); call make_points_(self%grid,pt)
     call create_(D,n_pt)
     call create_(P,n_pt,3)
     call create_(L,n_pt,3)
     n_occ = no_of_occupied_NOs_(self)
     do n = 1,n_occ
       call make_laplacian_orbital_grid_c_(self,L,P,D,self%natural_orbitals%restricted_complex(:,n), pt)
       occ = self%occupation_numbers%restricted(n)
       div_jp(:) = div_jp(:) - occ*aimag(conjg(D(:))*(L(:,1)+L(:,2)+L(:,3)))
     end do
     call destroy_(L)
     call destroy_(P)
     call destroy_(D)
     call destroy_(pt)

   end subroutine

! ***************************
! Isosurface plotting methods
! ***************************

   subroutine isosurface_plot(self)
    type(mol_type) :: self
    ! Do one of the many kinds of isosurface plot calculations
     character(128) :: word
     target :: self

     call ensure_(tonto,associated(self%isosurface),"MOL:isosurface_plot ... no isosurface")
     saved_self => self  ! Note this !
     word = self%isosurface%iso_kind
     call to_lower_case_(word)
     select case (word)
       ! case("delta_density      "); .isosurface.cubify(delta_density_grid)
        case("elf                "); call cubify_(self%isosurface,ELF_grid)
        case("electric_potential "); call cubify_(self%isosurface,electric_potential_grid)
        case("electron_density   "); call cubify_(self%isosurface,electron_density_grid)
        case("fermi_mobility     "); call cubify_(self%isosurface,fermi_mobility_grid)
        case("laplacian_density  "); call cubify_(self%isosurface,laplacian_density_grid)
        case("orbital            "); call cubify_(self%isosurface,orbital_grid)
        case("orbital_density    "); call cubify_(self%isosurface,orbital_density_grid)
        case("spin_density       "); call cubify_(self%isosurface,spin_density_grid)
        case("stockholder_density"); call cubify_(self%isosurface,stockholder_density_grid)
        case("true_fermi_mobility"); call cubify_(self%isosurface,true_fermi_mobility_grid)
        case("tsirelson_elf      "); call cubify_(self%isosurface,Tsirelson_ELF_grid)
        case("grad_rho_on_rho    "); call cubify_(self%isosurface,grad_rho_on_rho_grid)
        case default;                allocate(tonto%known_keywords(12))
        tonto%known_keywords(1) = "elf                "
        tonto%known_keywords(2) = "electric_potential "
        tonto%known_keywords(3) = "electron_density   "
        tonto%known_keywords(4) = "fermi_mobility     "
        tonto%known_keywords(5) = "laplacian_density  "
        tonto%known_keywords(6) = "orbital            "
        tonto%known_keywords(7) = "orbital_density    "
        tonto%known_keywords(8) = "spin_density       "
        tonto%known_keywords(9) = "stockholder_density"
        tonto%known_keywords(10) = "true_fermi_mobility"
        tonto%known_keywords(11) = "tsirelson_elf      "
        tonto%known_keywords(12) = "grad_rho_on_rho    "
        call unknown_(tonto,word,"MOL:isosurface_plot")
        deallocate(tonto%known_keywords)
     end select
   end subroutine

   subroutine plot_on_isosurface(self)
    type(mol_type) :: self
    ! Do one of the many kinds of isosurface plot calculations
     character(128) :: word
     target :: self

     call ensure_(tonto,associated(self%isosurface),"MOL:plot_on_isosurface ... no isosurface")
     saved_self => self  ! Note this !
     word = self%isosurface%surface_property
     call to_lower_case_(word)
     select case (word)
       ! case("delta_density      "); .isosurface.plot_function(delta_density_grid)
        case("elf                "); call plot_function_(self%isosurface,ELF_grid)
        case("electric_potential "); call plot_function_(self%isosurface,electric_potential_grid)
        case("electron_density   "); call plot_function_(self%isosurface,electron_density_grid)
        case("fermi_mobility     "); call plot_function_(self%isosurface,fermi_mobility_grid)
        case("laplacian_density  "); call plot_function_(self%isosurface,laplacian_density_grid)
        case("orbital            "); call plot_function_(self%isosurface,orbital_grid)
        case("orbital_density    "); call plot_function_(self%isosurface,orbital_density_grid)
        case("spin_density       "); call plot_function_(self%isosurface,spin_density_grid)
        case("stockholder_density"); call plot_function_(self%isosurface,stockholder_density_grid)
        case("true_fermi_mobility"); call plot_function_(self%isosurface,true_fermi_mobility_grid)
        case("tsirelson_elf      "); call plot_function_(self%isosurface,Tsirelson_ELF_grid)
        case("grad_rho_on_rho    "); call plot_function_(self%isosurface,grad_rho_on_rho_grid)
        case default;                allocate(tonto%known_keywords(12))
        tonto%known_keywords(1) = "elf                "
        tonto%known_keywords(2) = "electric_potential "
        tonto%known_keywords(3) = "electron_density   "
        tonto%known_keywords(4) = "fermi_mobility     "
        tonto%known_keywords(5) = "laplacian_density  "
        tonto%known_keywords(6) = "orbital            "
        tonto%known_keywords(7) = "orbital_density    "
        tonto%known_keywords(8) = "spin_density       "
        tonto%known_keywords(9) = "stockholder_density"
        tonto%known_keywords(10) = "true_fermi_mobility"
        tonto%known_keywords(11) = "tsirelson_elf      "
        tonto%known_keywords(12) = "grad_rho_on_rho    "
        call unknown_(tonto,word,"MOL:plot_on_isosurface")
        deallocate(tonto%known_keywords)
     end select
   end subroutine

   subroutine electron_density_grid(g,pt)
    ! Work out the electron density grid "g" for a series of points "pt" for
    ! using the ".natural orbitals".
      real(kind=kind(1.0d0)), dimension(:), intent(out) :: g
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: pt
      type(mol_type), pointer :: self; self => saved_self

      call make_density_grid_(self,g,pt)

   end subroutine

   subroutine spin_density_grid(g,pt)
    ! Work out the electron density grid "g" for a series of points "pt" for
    ! using the ".natural orbitals" and ".occupation_numbers" vector.
      real(kind=kind(1.0d0)), dimension(:), intent(out) :: g
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: pt
      type(mol_type), pointer :: self; self => saved_self

      call make_ao_sz_density_matrix_(self)
      call make_density_grid_(self,g,pt)

   end subroutine

   subroutine laplacian_density_grid(g,pt)
    ! Work out the laplacian density grid "g" for a series of points "pt" for
    ! using the ".natural orbitals".
      real(kind=kind(1.0d0)), dimension(:), intent(out) :: g
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: pt
      type(mol_type), pointer :: self; self => saved_self

      call make_laplacian_grid_(self,g,pt)

   end subroutine

   subroutine orbital_density_grid(g,pt)
    ! Work out the orbital density grid "g" for a series of points "pt" for using
    ! the ".natural orbitals", for a partcular orbital .grid.orbital.
      real(kind=kind(1.0d0)), dimension(:), intent(out) :: g
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: pt
      complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: gc
      integer(kind=kind(1)) :: n_pt
      type(plotgrid_type), pointer :: grid
      type(mol_type), pointer :: self; self => saved_self

      call ensure_(tonto,associated(self%isosurface),"MOL:orbital_density_grid ... no isosurface")
      call ensure_(tonto,self%isosurface%grid%orbital>=0,"MOL:orbital_density_grid ... non-positive grid orbital")
      call ensure_(tonto,size(pt,1)==size(g,1),"MOL:orbital_density_grid ... incompatible # of points, g and pt arrays")
      call ensure_(tonto,size(pt,2)==3,"MOL:orbital_density_grid ... wrong shape, pt array")
      call ensure_(tonto,associated(self%natural_orbitals),"MOL:orbital_density_grid ... no natural orbitals")
      n_pt = size(pt,1)
      grid => self%isosurface%grid
      if (number_kind_(self%natural_orbitals) == "real") then
        call make_orbital_grid_r_(self,g,self%natural_orbitals%restricted(:,grid%orbital),pt,square=.true.)
      else
        call create_(gc,n_pt)
        call make_orbital_grid_c_(self,gc,self%natural_orbitals%restricted_complex(:,grid%orbital),pt,square=.true.)
        g = gc
        call destroy_(gc)
      end if

   end subroutine

   subroutine orbital_grid(g,pt)
    ! Work out the orbital grid "g" for a series of points "pt" for using
    ! the ".natural orbitals", for a partcular orbital .grid.orbital.
    ! NOTE: for complex orbitals, the absolute value times the sign of
    ! the complex part
      real(kind=kind(1.0d0)), dimension(:), intent(out) :: g
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: pt
      complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: gc
      integer(kind=kind(1)) :: n_pt
      type(plotgrid_type), pointer :: grid
      type(mol_type), pointer :: self; self => saved_self

      call ensure_(tonto,associated(self%isosurface),"MOL:orbital_grid ... no isosurface")
      call ensure_(tonto,self%isosurface%grid%orbital>=0,"MOL:orbital_grid ... non-positive grid orbital")
      call ensure_(tonto,size(pt,1)==size(g,1),"MOL:orbital_grid ... incompatible # of points, g and pt arrays")
      call ensure_(tonto,size(pt,2)==3,"MOL:orbital_grid ... wrong shape, pt array")
      call ensure_(tonto,associated(self%natural_orbitals),"MOL:orbital_grid ... no natural orbitals")
      n_pt = size(pt,1)
      grid => self%isosurface%grid
      if (number_kind_(self%natural_orbitals) == "real") then
        call make_orbital_grid_r_(self,g,self%natural_orbitals%restricted(:,grid%orbital),pt)
      else
        call create_(gc,n_pt)
        call make_orbital_grid_c_(self,gc,self%natural_orbitals%restricted_complex(:,grid%orbital),pt)
        g = sign(real(gc),aimag(gc))
        call destroy_(gc)
      end if

   end subroutine

   subroutine ELF_grid(g,pt)
    ! Work out the Electron Localisation Function (ELF) density "g" for a series
    ! of points "pt" for using the ".natural orbitals".
      real(kind=kind(1.0d0)), dimension(:), intent(out) :: g
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: pt
      type(mol_type), pointer :: self; self => saved_self

      call make_ELF_grid_(self,g,pt)

   end subroutine

   subroutine grad_rho_on_rho_grid(g,pt)
    ! Work out abs(grad(rho))/rho on a set of grid points "pt".
      real(kind=kind(1.0d0)), dimension(:), intent(out) :: g
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: pt
      type(mol_type), pointer :: self; self => saved_self

      call make_grad_rho_on_rho_grid_(self,g,pt)

   end subroutine

   subroutine Tsirelson_ELF_grid(g,pt)
    ! Work out the Tsirelson-Stash version of the Electron Localisation Function
    ! (ELF) density "g" for a series of points "pt" for using the ".natural
    ! orbitals".
      real(kind=kind(1.0d0)), dimension(:), intent(out) :: g
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: pt
      type(mol_type), pointer :: self; self => saved_self

      call make_Tsirelson_ELF_grid_(self,g,pt)

   end subroutine

   subroutine fermi_mobility_grid(g,pt)
    ! Work out the Luken's fermi mobility density on a series of grid points "g"
    ! using ".natural orbitals" and the ".occupation_numbers" vector. A Gnuplot
    ! ascii file is generated.
      real(kind=kind(1.0d0)), dimension(:), intent(out) :: g
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: pt
      type(mol_type), pointer :: self; self => saved_self

      call make_fermi_mobility_grid_(self,g,pt)

   end subroutine

   subroutine true_fermi_mobility_grid(g,pt)
    ! Work out the Luken's true fermi mobility density (the one using kinetic
    ! energy matrix elements) on a series of grid points "g" using ".natural
    ! orbitals" and the ".occupation_numbers" vector. A Gnuplot ascii file is
    ! generated.
      real(kind=kind(1.0d0)), dimension(:), intent(out) :: g
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: pt
      type(mol_type), pointer :: self; self => saved_self

      call make_true_fermi_mobility_grid_(self,g,pt)

   end subroutine

   subroutine electric_potential_grid(g,pt)
    ! Make the electric potential "pot_grid" on a series of points "pt"
      real(kind=kind(1.0d0)), dimension(:), intent(out) :: g
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: pt
      type(mol_type), pointer :: self; self => saved_self

     call make_electric_potential_grid_(self,g,pt)

   end subroutine

   subroutine stockholder_density_grid(g,pt)
    ! Work out the Hirshfeld stockholder density grid "g" for a series of points.
      real(kind=kind(1.0d0)), dimension(:), intent(out) :: g
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: pt
      type(mol_type), pointer :: self; self => saved_self

      call make_stockholder_grid_(self,g,pt)

   end subroutine

! ****************************
! Vibrationally averaged grids
! ****************************

   subroutine get_vib_averaged_rho_grid(self,density_grid)
    type(mol_type) :: self
    ! Set "density_grid" to the vibrationally averaged electron density grid.
    ! If the archive file exists, read it; otherwise make it.
      real(kind=kind(1.0d0)), dimension(:) :: density_grid
      type(archive_type) :: arch

      call ensure_(tonto,associated(self%grid),"MOL:get_vib_averaged_rho_grid ... no grid")
      call ensure_(tonto,associated(self%atom),"MOL:get_vib_averaged_rho_grid ... no atom list")
      call ensure_(tonto,self%n_atom==2,"MOL:get_vib_averaged_rho_grid ... can only do diatomics")
      call set_(arch,self%name,"vib_averaged_rho_grid")
      if (.not. exists_(arch)) call make_vib_averaged_rho_grid_(self)
      call read_(arch,density_grid)

   end subroutine

   subroutine make_vib_averaged_rho_grid(self)
    type(mol_type) :: self
    ! Work out the averaged density on ".grid" using ".natural orbitals" and the
    ! ".occupation_numbers" vector. A Gnuplot ascii file is generated.
      type(archive_type) :: arch
      real(kind=kind(1.0d0)), dimension(:), pointer :: density_grid

      call ensure_(tonto,associated(self%grid),"MOL:make_vib_averaged_rho_grid ... no grid")
      call ensure_(tonto,associated(self%atom),"MOL:make_vib_averaged_rho_grid ... no atom list")
      call ensure_(tonto,self%basis_info_made,"MOL:make_vib_averaged_rho_grid ... no basis info")
      call ensure_(tonto,self%n_atom==2,"MOL:make_vib_averaged_rho_grid ... can only do diatomics")
      call move_origin_to_centre_of_mass_(self)
      call create_(density_grid,self%grid%n_pt)
      call integrate_rho_grid_(self,density_grid,-4.0d0,4.0d0,10.0d0**(-4))
      call set_(arch,self%name,"vib_averaged_rho_grid")
      call write_(arch,density_grid)
      call set_(arch,self%name,"vib_averaged_rho_grid",format="ascii")
      call write_gnuplot_(arch,density_grid, self%grid%n_x, self%grid%n_y, self%grid%n_z)
      call destroy_(density_grid)

   end subroutine

   recursive subroutine integrate_rho_grid(self,res,a,b,accuracy,fa0,fb0)
    type(mol_type) :: self
    ! Integrate the rho grid between the limits "a" and "b" using adaptive
    ! trapezoidal rule with Simpsons approximation.  If present, "accuracy"
    ! is the required accuracy of the integral. If present, "fa0" and
    ! "fb0" are the value of the rho_grid at "a" and "b" respectively.
    ! size of "res" is .grid.n_pt.
      real(kind=kind(1.0d0)) :: a,b
      real(kind=kind(1.0d0)), optional :: accuracy
      real(kind=kind(1.0d0)), dimension(:), pointer, optional :: fa0,fb0
      real(kind=kind(1.0d0)), dimension(:), pointer :: res
      real(kind=kind(1.0d0)), dimension(:), pointer :: fa,fb,fm,one_trap,two_trap,left,right
      integer(kind=kind(1)) :: n_pt
      logical(kind=kind(.true.)) :: same
      real(kind=kind(1.0d0)) :: tol,h,m
      integer(kind=kind(1)) :: depth = 0

      call ensure_(tonto,associated(self%grid),"MOL:integrate_rho_grid ... no grid")
      n_pt = self%grid%n_pt
      depth = depth+1
      call show_(stdout,"depth =",depth)
      tol = 10.0d0**(-6)
      if (present(accuracy)) tol = accuracy
      h  = b-a
      m  = (a+b)/2.0d0
      if (present(fb0)) then; fb => fb0
      else;                   call create_(fb,n_pt); call make_rho_grid_at_(self,b,fb)
      end if
      if (present(fa0)) then; fa => fa0
      else;                   call create_(fa,n_pt); call make_rho_grid_at_(self,a,fa)
      end if
      call create_(fm,n_pt);    call make_rho_grid_at_(self,m,fm)
      call create_(one_trap,n_pt)
      call create_(two_trap,n_pt)
      one_trap = h*(fa+fb)/2.0d0
      two_trap = h*(fa+2.0d0*fm+fb)/4.0d0
      res = abs(one_trap-two_trap)
      same = maxval(res) < 3.0d0*tol
      if (same .and. depth>1) then
        res = (4.0d0*two_trap - one_trap)/3.0d0
        call destroy_(two_trap)
        call destroy_(one_trap)
        call destroy_(fm)
        call destroy_(fa)
      else
        call destroy_(two_trap)
        call destroy_(one_trap)
        call create_(left,n_pt);  call integrate_rho_grid_(self,left ,a,m,tol/2.0d0,fa0=fa,fb0=fm)
        call create_(right,n_pt); call integrate_rho_grid_(self,right,m,b,tol/2.0d0,fa0=fm,fb0=fb)
        res = left + right
        call destroy_(right)
        call destroy_(left)
      end if
      if (depth==1) call destroy_(fb)
      depth = depth-1

   end subroutine

   subroutine make_rho_grid_at(self,q,rho)
    type(mol_type) :: self
    ! Work out the electron density on ".grid" using ".natural orbitals" and the
    ! ".occupation_numbers" vector at nuclear separation "q", where "q" is a
    ! dimensionless normal coordinate
    ! size or "rho" is .grid.n_pt.
      real(kind=kind(1.0d0)) :: q
      real(kind=kind(1.0d0)), dimension(:) :: rho
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: pt
      real(kind=kind(1.0d0)) :: re,w,mu, gamma,d,r, wt
      integer(kind=kind(1)) :: n

      re = 2.0460259  ! N2
      w  = 2708       ! N2
      re = 1.3848617  ! H2
      w  = 4658       ! H2
      mu = reduced_mass_(self)
      n = 1
      gamma = sqrt((w/2.1947463067d+5) &
                  * mu * (1.6605402d-27/9.10939d-31) )  ! This is gamma^(1/2)
      d = q/gamma  ! d is the cartesian displacement in Bohr
      r = re + d   ! r is the actual separation of atoms in Bohr
      call show_(stdout,"q =",q)
      call show_(stdout,"d =",d)
      call show_(stdout,"r =",r)
      call show_(stdout,"g =",gamma)
      self%atom(1)%pos = 0.0d0
      self%atom(2)%pos = 0.0d0
      self%atom(1)%pos(3) = -r/2.0d0
      self%atom(2)%pos(3) = +r/2.0d0
      call move_origin_to_centre_of_mass_(self)
      call delete_scf_integrals_(self)
      call scf_(self)
      call create_(pt,self%grid%n_pt,3); call make_points_(self%grid,pt)
      call make_density_grid_(self,rho,pt)
      call destroy_(pt)
      wt = harmonic_vibrational_fn_(q,n)
      wt = wt*wt
      rho = wt * rho

   end subroutine

   subroutine integrate_density_numerically(self)
    type(mol_type) :: self
    ! Integrate the density numerically
      integer(kind=kind(1)) :: n_pts
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: pt
      real(kind=kind(1.0d0)), dimension(:), pointer :: wt,rho
      real(kind=kind(1.0d0)) :: n_e

      call ensure_(tonto,associated(self%dftgrid),"MOL:integrate_density_numerically ... no dftgrid info!")
      n_pts = self%dftgrid%n_pts * self%n_atom
      call show_(stdout,"number of grid points = ", n_pts)
      call create_(pt,n_pts,3)
      call create_(wt,n_pts)
      call create_(rho,n_pts)
      call make_grid_(self%dftgrid,pt,wt,self%atom)
      call make_density_grid_(self,rho,pt)
      n_e = sum(rho*wt)
      call show_(stdout,"numerically integrated charge =",n_e)
      call destroy_(rho)
      call destroy_(wt)
      call destroy_(pt)

   end subroutine

!   integrate_density_functional
!   ! Integrate some functional of the density and nabla density numerically
!   ! skw
!      n_pts :: integer(kind=kind(1))
!      wt,rho,x,y,z :: REALVEC*
!      pt,nabla_rho :: REALMAT*
!      n_e,value :: real(kind=kind(1.0d0))
!      call ensure_(tonto,.dftgrid.created,"no dftgrid info!")
!      n_pts = .dftgrid.n_pts * .n_atom
!      stdout.show("number of grid points = ", n_pts)
!      pt.create(n_pts,3)
!      wt.create(n_pts)
!      rho.create(n_pts)
!      nabla_rho.create(n_pts,3)
!      .dftgrid.make_grid(pt,wt,.atom)
!      .make_density_grid(rho,pt)
!      .make_nabla_density_grid(nabla_rho,pt)
!      x => pt(:,1); y => pt(:,2); z => pt(:,3)
!      n_e = sum(rho*wt)
!      value = sum(x*nabla_rho(:,1)*wt)
!      stdout.show("integrate(rho) =",n_e)
!      stdout.show("integrate(x*nabla_rho) =",value)
!      rho.destroy
!      nabla_rho.destroy
!      wt.destroy
!      pt.destroy
!   end

!*******************************************************************************
!                        Camp-King routines
!*******************************************************************************

!   camp_king(E_HF,E_K)
!   ! WARNING: the old matrices were removed, this routine needs to be examined
!   ! again ...
!   ! Uses .old_molecular_orbitals (lambda=0) and .molecular_orbitals (lambda=1)
!   ! to generate molecular orbitals at optimal lambda.  These overwrite
!   ! .molecular_orbitals.
!   ! E_HF and E_K are only changed if the orbitals are changed.
!   ! Can do restricted case only.
!     E_HF,E_K :: real(kind=kind(1.0d0)), intent(inout)
!     F_backup,MO_backup,P_backup :: type(opmatrix_type)*
!     old_MO,new_MO,old_F,old_P :: REALMAT*
!     Uh,V,W,U,CV,CW,S,Q,Q1,F,Cj,Cj1,Cl :: REALMAT*
!     theta :: REALVEC*
!     minpq,n,n_p,n_q,i,k,j :: integer(kind=kind(1))
!     ded0,ded1,ded1a,lambda,El :: real(kind=kind(1.0d0))
!     tol,diis0,diis1,diisl :: real(kind=kind(1.0d0))
!     call ensure_(tonto,.fock_matrix.spinorbital_kind=="restricted","only for restricted for now")
!
!     .scfdata.camp_king_iters=0
!     F_backup.create_copy(.fock_matrix)
!     MO_backup.create_copy(.molecular_orbitals)
!     P_backup.create_copy(.density_matrix)
!     .make_diis_error(diis1)
!
!     .unarchive_fock_matrix(archive_name="old_fock_matrix")
!     old_F  => .fock_matrix.restricted
!     .unarchive_molecular_orbitals(archive_name="old_molecular_orbitals")
!     old_MO => .molecular_orbitals.restricted
!     .unarchive_density_matrix(archive_name="old_density_matrix")
!     old_P  => .density_matrix.restricted
!     .make_diis_error(diis0)
!
!     .fock_matrix        => F_backup
!     .molecular_orbitals => MO_backup
!     new_MO              => .molecular_orbitals.restricted
!     .density_matrix     => P_backup
!
!     if (diis1 < 0.95*diis0) then
!       old_F.destroy
!       old_P.destroy
!       old_MO.destroy
!       return ! the new orbitals are better than the old.
!     end
!
!     n_p = .n_e/2
!     n_q = .n_bf - n_p
!     minpq = min(n_p,n_q)
!
!     Q.create(.n_bf,.n_bf)
!     Q = old_MO                                  ! Q is an orthogonal basis.
!
!     Q1.create(.n_bf,.n_bf)                      ! Get inverse of Q, store in Q1.
!     S.create(.n_bf,.n_bf)
!     .get_overlap_matrix(S)
!     Q1.to_product_of(Q,S,transpose_a=.true.)
!     S.destroy
!
!     Cj.create(.n_bf,.n_bf)                      ! Convert the MOs into Q basis.
!     Cj1.create(.n_bf,.n_bf)
!     Cj.to_product_of(Q1,old_MO)
!     Cj1.to_product_of(Q1,new_MO)
!     Q1.destroy
!
!     V.create(.n_bf,.n_bf)
!     W.create(.n_bf,.n_bf)
!     theta.create(minpq)
!     U.create(.n_bf,.n_bf)
!     U.to_product_of(Cj,Cj1)                        ! eqn 8
!     V=Cj1
!     W=Cj1
!     U.make_corresponding_orbitals(V,W,theta,n_p)
!     U.destroy
!
!     CV.create(.n_bf,.n_bf)
!     CW.create(.n_bf,.n_bf)
!     CV.to_product_of(Cj,V)                         ! eqn 11
!     CW.to_product_of(Cj1,W)                        ! eqn 12
!     W.destroy
!     V.destroy
!     Cj1.destroy
!     Cj.destroy
!
!     F.create(.n_bf,.n_bf)
!     !get de/dl at l=1
!     F=F_backup.restricted                          ! Fbar at lambda=1
!     F.change_basis(Q)                              ! F = Q^t Fbar Q
!     F.change_basis(CW)                             ! eqn 28.
!     CW.destroy
!     ded1 = 0.0d0
!     do n=1,n_p
!       ded1 = ded1 + theta(n)*F(n+n_p,n)
!     end
!     ded1 = -4.0d0 * ded1
!
!     !get de/dl at l=0
!     F=old_F                                        ! Fbar at lambda=0
!     F.change_basis(Q)                              ! F = Q^t Fbar Q
!     F.change_basis(CV)                             ! eqn 27.
!     ded0 = 0.0d0
!     do n=1,n_p
!       ded0 = ded0 + theta(n)*F(n+n_p,n)
!     end
!     ded0 = -4.0d0 * ded0
!     F.destroy
!
!     .scfdata.camp_king_iters=0
!     if (abs(ded1) > 10.0d0*abs(ded0)) then
!       lambda = 0.1
!     else
!       lambda = 0.50d0
!     end
!     .old_fock_matrix.minus(.old_constraint_matrix)
!     .minimise_Camp_King_lambda(lambda,ded0,ded1a,Q,CV,theta,n_p,diis0)
!     .make_diis_error(diisl)
!     if (diisl < 0.95*diis1 .or. abs(ded1a) < abs(ded1)) then
!       Uh.create(.n_bf,.n_bf)
!       .make_Camp_King_U_hat(Uh,lambda,theta)
!       Cl.create(.n_bf,.n_bf)
!       Cl.to_product_of(CV,Uh)
!       Uh.destroy
!       .molecular_orbitals.restricted.to_product_of(Q,Cl)
!       Cl.destroy
!       .schmidt_orthonormalise(.molecular_orbitals)
!       .make_scf_density_matrix(damp=.false.)
!       .make_structure_factors
!       .scfdata.set(crystal=.crystal)
!
!       .make_fock_matrix
!       if (.scfdata.scf_kind == "xray_rhf") then
!         E_HF = .scf_energy
!         E_K = .kinetic_energy
!         .add_constraint
!       else
!         E_HF = .scf_energy
!         E_K = .kinetic_energy
!       end
!     else
!       .fock_matrix.set_to(F_backup)
!       .molecular_orbitals.set_to(MO_backup)
!       .density_matrix.set_to(P_backup)
!     end
!     .old_fock_matrix.plus(.old_constraint_matrix)
!
!     CV.destroy
!     theta.destroy
!     Q.destroy
!     F_backup.destroy
!     P_backup.destroy
!     MO_backup.destroy
!   end
!
!   minimise_Camp_King_lambda(lambda,ded0,dedx,Ql,CV,theta,n_p,diis0) ::: private
!   !
!     ded0,diis0 :: real(kind=kind(1.0d0)), intent(in)
!     lambda,tol,dedx :: real(kind=kind(1.0d0))
!     n_p :: integer(kind=kind(1)), intent(in)
!     Ql,CV :: REALMAT
!     theta :: REALVEC
!      i :: integer(kind=kind(1))
!     a,b,d,d1,d2,du,dv,dw,dx,e,fu,fv,fw,fx,olde,tol1,tol2,u,u1,u2,v,w,x,xm :: real(kind=kind(1.0d0))
!     diisl :: real(kind=kind(1.0d0))
!     ok1,ok2 :: logical(kind=kind(.true.))
!
!     tol=0.1
!     a=0.0d0
!     if (ded0>0) a=-0.2
!     b=1.0d0
!     v=lambda
!     w=lambda
!     x=lambda
!     e=0.0d0
!     d=0.0d0
!     .get_Camp_King_energy(x,fx,dx,Ql,CV,theta,n_p,diisl)
!     fv=fx
!     fw=fx
!     dv=dx
!     dw=dx
!     .scfdata.camp_king_iters=0
!     do i=1,100
!       .scfdata.camp_king_iters=.scfdata.camp_king_iters+1
!       xm=0.50d0*(a+b)
!       tol1=tol*abs(x)+10.0d0**(-10)
!       tol2=2.0d0*tol1
!!       if (diisl < diis0) exit
!       if (abs(x-xm)<=(tol2-0.50d0*(b-a))) exit
!       if (abs(e)>tol1) then
!         d1=2.0d0*(b-a)
!         d2=d1
!         if (dw /= dx) d1=(w-x)*dx/(dx-dw)
!         if (dv /= dx) d2=(v-x)*dx/(dx-dv)
!         u1=x+d1
!         u2=x+d2
!         ok1=((a-u1)*(u1-b)>0.0d0) .and. (dx*d1 <=0.0d0)
!         ok2=((a-u2)*(u2-b)>0.0d0) .and. (dx*d2 <=0.0d0)
!         olde=e
!         e=d
!
!         if (ok1 .or. ok2) then
!           if (ok1 .and. ok2) then
!             if (abs(d1)<abs(d2)) then; d=d1
!             else;                      d=d2
!             end
!           else
!             if (ok1) then; d=d1
!             else;          d=d2
!             end
!           end if
!           if (abs(d)<=abs(0.50d0*olde)) then
!             u=x+d
!             if (u-a <tol2 .or. b-u <tol2) d=sign(tol1,xm-x)
!           else
!             if (dx<=0.0d0) then; e=a-x
!             else;               e=b-x
!             end
!             d=0.50d0*e
!           end if
!         else
!           if (dx>=0.0d0) then; e=a-x
!           else;               e=b-x
!           end
!           d=0.50d0*e
!         end if
!       else
!         if (dx>=0.0d0) then; e=a-x
!         else;               e=b-x
!         end
!         d=0.50d0*e
!       end if
!       if (abs(d)>=tol1) then
!         u=x+d
!         .get_Camp_King_energy(u,fu,du,Ql,CV,theta,n_p,diisl)
!       else
!         u=x+sign(tol1,d)
!         .get_Camp_King_energy(u,fu,du,Ql,CV,theta,n_p,diisl)
!         if (fu > fx) exit
!       end if
!       if (fu <= fx) then
!         if (u >= x) then
!           a=x
!         else
!           b=x
!         end if
!         v=w; fv=fw; dv=dw
!         w=x; fw=fx; dw=dx
!         x=u; fx=fu; dx=du
!       else
!         if (u < x) then
!           a=u
!         else
!           b=u
!         end if
!         if (fu <=fw .or. w ==x) then
!           v=w; fv=fw; dv=dw
!           w=u; fw=fu; dw=du
!         else if (fu <=fv .or. v ==x .or. v ==w) then
!           v=u; fv=fu; dv=du
!         end if
!       end if
!       if (abs(fv-fw) < .scfdata.convergence .and. abs(fv-fx) < .scfdata.convergence) then
!         exit
!       end
!     end do
!     lambda = x
!     dedx=dx
!   end
!
!   get_Camp_King_energy(lambda,El,dedl,Q,CV,theta,n_p,diisl) ::: private
!   !
!     Q,CV :: REALMAT, intent(in)
!     theta :: REALVEC, intent(in)
!     n_p :: integer(kind=kind(1)), intent(in)
!     constraint :: type(opmatrix_type)*
!     lambda,El,dedl,diisl :: real(kind=kind(1.0d0))
!     Uh,Cl,F :: REALMAT*
!      n :: integer(kind=kind(1))
!     Uh.create(.n_bf,.n_bf)
!     .make_Camp_King_U_hat(Uh,lambda,theta)
!     Cl.create(.n_bf,.n_bf)
!     Cl.to_product_of(CV,Uh)                          ! C(lambda) = C^j V Uh(lambda)
!     Uh.destroy
!     .molecular_orbitals.restricted.to_product_of(Q,Cl) ! Cbar(lambda) = Q C(lambda)
!     .schmidt_orthonormalise(.molecular_orbitals)
!     .make_scf_density_matrix(damp=.false.)
!     .make_structure_factors
!     .make_fock_matrix
!     if (.scfdata.scf_kind == "xray_rhf") then
!       El = .scf_energy + .scfdata.lambda * .crystal.F_chi2
!       .add_constraint
!     else
!       El = .scf_energy
!     end
!     .make_diis_error(diisl)
!     F.create(.n_bf,.n_bf)
!     F=.fock_matrix.restricted                      ! Fbar at lambda in AO basis.
!     F.change_basis(Q)
!     F.change_basis(Cl)
!     Cl.destroy
!     dedl = 0.0d0
!     do n=1,n_p
!       dedl = dedl + theta(n)*F(n+n_p,n)
!     end
!     dedl = -4.0d0 * dedl
!     F.destroy
!   end
!
!   make_Camp_King_U_hat(Uh,lambda,theta) ::: private
!   !
!     self :: intent(in)
!     Uh :: REALMAT, target
!     lambda :: real(kind=kind(1.0d0)), intent(in)
!     theta :: REALVEC, intent(in)
!     sin_theta,cos_theta :: REALVEC*
!     Uh_pp,Uh_qq,Uh_qp,Uh_pq :: REALMAT*
!     minpq,maxpq,n_p,n_q,m,n :: integer(kind=kind(1))
!     ok :: logical(kind=kind(.true.))
!
!     ok = size(Uh,1)==.n_bf .and. size(Uh,2)==.n_bf
!     n_p = ceiling(.n_e/2.0d0)
!     n_q = .n_bf - n_p
!     minpq = min(n_p,n_q)
!     maxpq = max(n_p,n_q)
!     m=n_p-minpq
!     n=n_q-minpq
!     call ensure_(tonto,ok,"incorrect dimensions for Uh matrix")
!     call die_if_(tonto,size(theta)/=minpq,"incorrect dimensions for theta array")
!
!     sin_theta.create(minpq)
!     cos_theta.create(minpq)
!     sin_theta = sin(theta*lambda)
!     cos_theta = cos(theta*lambda)
!
!     Uh=0.0d0
!     Uh_pp => Uh(:n_p,:n_p)
!     Uh_qq => Uh(n_p+1:,n_p+1:)
!     Uh_qp => Uh(n_p+1:,:n_p)
!     Uh_pq => Uh(:n_p,n_p+1:)
!
!     Uh_pp(:minpq,:minpq).from_diagonal(cos_theta)
!     Uh_qq(:minpq,:minpq).from_diagonal(cos_theta)
!
!     Uh_qp(:minpq,:minpq).from_diagonal(-sin_theta)
!     Uh_pq(:minpq,:minpq).from_diagonal(sin_theta)
!
!     if (m>0) then ! do p-m:p block
!       Uh_pp(minpq+1:,minpq+1:).to_unit_mat
!     else if (n>0) then ! do p+q-n:p+q block
!       Uh_qq(minpq+1:,minpq+1:).to_unit_mat
!     end
!
!     cos_theta.destroy
!     sin_theta.destroy
!   end
!
!   dynamic_damp(E_HF,E_K)
!   ! Increase the damp factor until the new density matrix gives better results
!   ! than the previous one.  Good for difficult convergence.
!   ! E_HF and E_K are only changed if the orbitals are changed.
!     E_HF,E_K :: real(kind=kind(1.0d0)), intent(inout)
!     F_backup,MO_backup,P_backup :: type(opmatrix_type)*
!     theta :: REALVEC*
!      n :: integer(kind=kind(1))
!     diis0,diis1,diisl,damp,delta_damp,E0,E1,El,mix :: real(kind=kind(1.0d0))
!     chi2 :: real(kind=kind(1.0d0))
!
!     chi2=0.0d0
!
!     F_backup.create_copy(.fock_matrix)
!     MO_backup.create_copy(.molecular_orbitals)
!     P_backup.create_copy(.density_matrix)
!     ! .fock_matrix contains .constraint_matrix
!     ! .old_fock_matrix contains .old_constraint_matrix
!
!     ! Get type(diis_type) error and energy for new density
!     .make_diis_error(diis1)
!     if (.scfdata.scf_kind == "xray_rhf") then
!       ! Remove the constraint matrix from the new fock matrix.
!       F_backup.minus(.constraint_matrix)
!       .fock_matrix.set_to(F_backup)
!       .make_structure_factors
!       chi2 = .crystal.F_chi2
!     end
!     E1 = .scf_energy + .scfdata.lambda * chi2
!
!     ! Get type(diis_type) error and energy for old density
!     .fock_matrix.set_to(.old_fock_matrix)
!     .density_matrix.set_to(.old_density_matrix)
!     .make_diis_error(diis0)
!     if (.scfdata.scf_kind == "xray_rhf") then
!       ! Remove the constraint matrix from the old fock matrix.
!       .old_fock_matrix.minus(.old_constraint_matrix)
!       .fock_matrix.set_to(.old_fock_matrix)
!       .make_structure_factors
!       chi2 = .crystal.F_chi2
!     end
!     E0 = .scf_energy + .scfdata.lambda * chi2
!
!     ! .fock_matrix does not contain .constraint_matrix
!     ! .old_fock_matrix does not contain .old_constraint_matrix
!
!     if (diis1<10.0d0*diis0) then
!       .fock_matrix.set_to(F_backup)
!       .molecular_orbitals.set_to(MO_backup)
!       .density_matrix.set_to(P_backup)
!       if (.scfdata.scf_kind == "xray_rhf") then
!         ! reinsert contraint matrices
!         F_backup.plus(.constraint_matrix)
!         .old_fock_matrix.plus(.old_constraint_matrix)
!         .make_structure_factors
!       end
!       P_backup.destroy
!       MO_backup.destroy
!       F_backup.destroy
!       return ! the new orbitals are better than the old.
!     end
!
!     mix = 0.9 ! How much of the new, not the old.
!     do n=1,15
!       .density_matrix.set_to(P_backup)
!       .fock_matrix.set_to(F_backup)
!       .density_matrix.damp(.old_density_matrix,1.0d0-mix)
!       .fock_matrix.damp(.old_fock_matrix,1.0d0-mix)
!       E_HF = .scf_energy
!       if (.scfdata.scf_kind == "xray_rhf") then
!         .make_structure_factors
!         .add_constraint
!         El = E_HF + .scfdata.lambda * .crystal.F_chi2
!       else
!         El = E_HF
!       end
!       .make_diis_error(diisl)
!       .scfdata.dynamic_damp_factor = 1.0d0-mix
!       if (diisl<10.0d0*diis0) exit
!       mix = 0.50d0 * mix
!     end
!     E_K = .kinetic_energy ! move this outside of loop
!     .make_structure_factors
!     F_backup.destroy
!     P_backup.destroy
!     MO_backup.destroy
!   end

! *********************
! Roby analysis methods
! *********************

   subroutine roby_analysis(self)
    type(mol_type) :: self
    ! Do one of the many kind of Roby population analysis
     logical(kind=kind(.true.)) :: allowed_kind

   call ensure_(tonto,associated(self%roby),"MOL:roby_analysis ... no robydata= supplied")
   call ensure_(tonto,associated(self%density_matrix),"MOL:roby_analysis ... no density")
     allowed_kind =  spinorbital_kind_(self%density_matrix)=="restricted" &
                  .or. spinorbital_kind_(self%density_matrix)=="unrestricted"
   call ensure_(tonto,allowed_kind,"MOL:roby_analysis ... only restricted or unrestricted densities are allowed")
     call get_ANO_data_(self)
     select case (self%roby%roby_kind)
         case("atom_bond_analysis     "); call atom_bond_analysis_(self%roby)
         case("atom_shared_population "); call atom_shared_population_(self%roby)
         case("atom_pair_populations  "); call atom_pair_populations_(self%roby)
         case("atom_populations       "); call atom_populations_(self%roby)
         case("group_bond_analysis    "); call group_bond_analysis_(self%roby)
         case("group_shared_population"); call group_shared_population_(self%roby)
         case("group_pair_populations "); call group_pair_populations_(self%roby)
         case("group_populations      "); call group_populations_(self%roby)
        case default;                allocate(tonto%known_keywords(8))
        tonto%known_keywords(1) = "atom_bond_analysis     "
        tonto%known_keywords(2) = "atom_shared_population "
        tonto%known_keywords(3) = "atom_pair_populations  "
        tonto%known_keywords(4) = "atom_populations       "
        tonto%known_keywords(5) = "group_bond_analysis    "
        tonto%known_keywords(6) = "group_shared_population"
        tonto%known_keywords(7) = "group_pair_populations "
        tonto%known_keywords(8) = "group_populations      "
        call unknown_(tonto,self%roby%roby_kind,"MOL:roby_analysis")
        deallocate(tonto%known_keywords)
     end select

   end subroutine

!  ********************
!  ANO control routines
!  ********************

   subroutine get_ANO_data(self)
    type(mol_type) :: self
    ! Get the restricted atomic natural orbitals (ANO) data for all atoms in the
    ! molecule, from an archive. If it is not there, then make it and archive it.
      integer(kind=kind(1)) :: k,u,j

   call ensure_(tonto,associated(self%unique_atom),"MOL:get_ANO_data ... no unique atom list")
      call destroy_ANO_data_(self)
      do k = 1,self%n_atom_kind  ! Get ANO data only for unique atoms
        u = self%unique_atom(k)
        call get_ANO_data_for_atom_(self,u)
        do j = u+1,self%n_atom
           if (self%atom_kind(j)/=k) cycle
           self%atom(j)%density_matrix     => self%atom(u)%density_matrix
           self%atom(j)%natural_orbitals   => self%atom(u)%natural_orbitals
           self%atom(j)%occupation_numbers => self%atom(u)%occupation_numbers
        end do
      end do

   end subroutine

   subroutine get_ANO_data_for_atom(self,a)
    type(mol_type) :: self
    ! Get the restricted atomic natural orbitals (ANO) data for atom "a" from an
    ! archive. If the archive is not there then make it.
     integer(kind=kind(1)) :: a
     type(mol_type), pointer :: mol
     integer(kind=kind(1)) :: n_no
     logical(kind=kind(.true.)) :: output

     call ensure_(tonto,.not. associated(self%atom(a)%natural_orbitals),"MOL:get_ANO_data_for_atom ... NO's exist, atom "//tr&
&im(to_str_(a)))
     call ensure_(tonto,.not. associated(self%atom(a)%occupation_numbers),"MOL:get_ANO_data_for_atom ... occupations exist, a&
&tom "//trim(to_str_(a)))
     call ensure_(tonto,.not. associated(self%atom(a)%density_matrix),"MOL:get_ANO_data_for_atom ... density exists, atom "//&
&trim(to_str_(a)))
     call create_(mol)
     call make_molecule_from_atom_(self,a,mol)
     if (archive_doesnt_exist_(mol,"natural_orbitals","restricted")) &
       call make_ANO_data_for_atom_(self,a)
     call unarchive_natural_orbitals_(mol,genre="restricted")
     call unarchive_occupation_numbers_(mol,genre="restricted")
     call unarchive_density_matrix_(mol,genre="restricted")
     self%atom(a)%natural_orbitals   => mol%natural_orbitals
     self%atom(a)%occupation_numbers => mol%occupation_numbers
     self%atom(a)%density_matrix     => mol%density_matrix
     call nullify_ptr_part_(mol%atom)        ! do not destroy basis part
     nullify(mol%natural_orbitals)    ! do not destroy this
     nullify(mol%occupation_numbers)  ! do not destroy this
     nullify(mol%density_matrix)      ! do not destroy this
     nullify(mol%basis)               ! do not destroy this
     nullify(mol%slaterbasis)         ! do not destroy this
     nullify(mol%coppensbasis)        ! do not destroy this
     call destroy_(mol)
     output = .false.
     if (.not. output) then;   return; end if
     n_no = no_of_occupied_NOs_(self%atom(a),tol=0.05d0)
     call show_(stdout,"n_no=",n_no)
     call put_natural_orbitals_(self%atom(a))

   end subroutine

   subroutine make_ANO_data_for_atom(self,a)
    type(mol_type) :: self
    ! Make and archive the spherically averaged restricted atomic natural
    ! orbitals (ANO) and density matrix data for atom "a".
     integer(kind=kind(1)) :: a
     type(mol_type), pointer :: mol

     call create_(mol)
     call make_molecule_from_atom_(self,a,mol)
     mol%scfdata%output = .false.
     mol%scfdata%direct = .true.
     call scf_(mol)
     call delete_scf_MO_archive_(mol)
     call delete_scf_fock_archive_(mol)
     call delete_old_scf_archives_(mol)
     call delete_scf_integrals_(mol)
     call make_ao_density_matrix_(mol)
     call create_(mol%pointgroup,"oh")
     call symmetrise_(mol,mol%density_matrix)
     call archive_density_matrix_(mol)
     call make_natural_orbitals_(mol)        ! ... and archive them
     call nullify_basis_part_(mol%atom)      ! do not destroy basis part
     call destroy_ptr_part_(mol%atom)        ! do not destroy basis part
     nullify(mol%basis)               ! do not destroy basis part
     nullify(mol%slaterbasis)         ! do not destroy this
     nullify(mol%coppensbasis)        ! do not destroy this
     call destroy_(mol)

   end subroutine

   subroutine destroy_ANO_data(self)
    type(mol_type) :: self
    ! Destroy the restricted atomic natural orbitals (ANO) data for
    ! all atoms in the molecule.
      integer(kind=kind(1)) :: k,u,j

      if (.not. associated(self%unique_atom)) then;   return; end if
      do k = 1,self%n_atom_kind  ! Do an SCF for every different kind of atom
        u = self%unique_atom(k)
        call destroy_(self%atom(u)%natural_orbitals)
        call destroy_(self%atom(u)%occupation_numbers)
        call destroy_(self%atom(u)%density_matrix)
        nullify(self%atom(u)%natural_orbitals)
        nullify(self%atom(u)%occupation_numbers)
        nullify(self%atom(u)%density_matrix)
        self%atom(u)%energy = 0.0d0
        do j = u+1,self%n_atom
           if (self%atom_kind(j)/=k) cycle
           nullify(self%atom(j)%natural_orbitals)
           nullify(self%atom(j)%occupation_numbers)
           nullify(self%atom(j)%density_matrix)
           self%atom(j)%energy = 0.0d0
        end do
      end do

   end subroutine

!---------------------------------------------!
! Roby Energy and Shared Energy calculations  !
!---------------------------------------------!

!   roby_energy_analysis
!   ! directs the energy analysis of a diatomic
!    roby_atom :: INTVEC*
!    Ep_A, Ep_B, Ep_AB, E_A, E_B, E_AB :: real(kind=kind(1.0d0))
!    a,b :: integer(kind=kind(1))
!    .read_roby_atom(roby_atom)
!    call ensure_(tonto,size(roby_atom)==2,"Must supply a diatomic...")
!    a = roby_atom(1); b=roby_atom(2)
!    .put_roby_atom_energy( (/ a /) )
!    .put_roby_atom_energy( (/ b /) )
!    .put_roby_atom_energy( roby_atom )
!    Ep_A = .roby_atom_energy( (/a/))
!    Ep_B = .roby_atom_energy( (/b/))
!    Ep_AB = .roby_atom_energy( roby_atom)
!    E_A = .roby_total_atom_energy((/ a /), nuclei=roby_atom,output=.true.)
!    E_B = .roby_total_atom_energy((/ b /), nuclei=roby_atom,output=.true.)
!    E_AB = .roby_total_atom_energy(roby_atom, nuclei=roby_atom,output=.true.)
!!    E_AB = .roby_atom_energy(roby_atom)
!    stdout.show( "Binding Energy: ", 0.50d0 * (E_A + E_B - E_AB + Ep_A + Ep_B - Ep_AB) )
!    .put_roby_shared_energy(roby_atom)
!    .put_roby_ionic_energy(roby_atom)
!    stdout.show("Nuclear potential for A:", .atom.nuclear_energy( (/ a /) ))
!    stdout.show("Nuclear potential for B:", .atom.nuclear_energy( (/ b /) ))
!    roby_atom.destroy
!   end
!
!   get_roby_shared_energy(roby_atom) result(se) ::: leaky
!   ! returns the shared energy of the atoms given in "roby_atom"
!   ! as well as the energy of all sub-groups of atoms "en_groups"
!     roby_atom :: INTVEC
!     se, energy_subgroup, en_groups :: real(kind=kind(1.0d0))
!     m,k, n_k, n_roby_atom :: integer(kind=kind(1))
!     comb_mat :: INTMAT*
!     n_roby_atom = size(roby_atom)
!     se = 0
!     do k = 1,n_roby_atom
!       n_k = n_roby_atom.choose(k)
!       comb_mat.create(k,n_k)
!       comb_mat = roby_atom.get_combination_matrix_from(k)
!       en_groups = 0
!       do m = 1,n_k
!         energy_subgroup = .roby_total_atom_energy(comb_mat(:,m),output=.false.)
!         en_groups = en_groups + energy_subgroup
!       end
!       comb_mat.destroy
!       se = se - ((-1)**k)*en_groups
!     end
!   end
!
!   get_roby_ionic_energy(roby_atom, roby_atom2) result(ionic_energy) ::: leaky
!   ! returns the shared energy of the atoms given in "roby_atom"
!   ! as well as the energy of all sub-groups of atoms "en_groups"
!   roby_atom, roby_atom2 :: INTVEC
!   ionic_energy, energy_group1, energy_group2 :: real(kind=kind(1.0d0))
!   ground1, ground2 :: real(kind=kind(1.0d0))
!       energy_group1 = .roby_total_atom_energy(roby_atom,output=.false.)
!       energy_group2 = .roby_total_atom_energy(roby_atom2,output=.false.)
!       ground1 = sum(.atom(roby_atom).energy)
!       ground2 = sum(.atom(roby_atom2).energy)
!       stdout.flush
!       stdout.dash(real_fields=3)
!       stdout.put("Ground State Energy of Atom 1"); stdout.put(ground1)
!       stdout.flush
!       stdout.put("Ground State Energy of Atom 2"); stdout.put(ground2)
!       stdout.flush
!       stdout.dash(real_fields=3)
!       stdout.flush
!       ionic_energy = energy_group1 - energy_group2
!       ionic_energy = ionic_energy - ground1 + ground2
!   end
!
!   put_roby_shared_energy(atoms) ::: leaky
!   ! Evaluate and put the Roby shared energy for a group of atoms
!      atoms :: INTVEC, OPTIONAL
!      shared_energy :: real(kind=kind(1.0d0))
!      roby_atom :: INTVEC*
!      if (PRESENT(atoms)) then
!          roby_atom.create(size(atoms))
!          roby_atom = atoms
!      else
!          .read_roby_atom(roby_atom)
!      end
!      shared_energy = .get_roby_shared_energy(roby_atom)
!      stdout.flush
!      stdout.dash(real_fields=3)
!      stdout.show("Shared energy =",shared_energy)
!      roby_atom.destroy
!   end
!
!   put_roby_ionic_energy(atoms) ::: leaky
!   ! Evaluate and put the Roby ionic energy for a group of atoms
!      atoms :: INTVEC, OPTIONAL
!      n_roby_atom :: integer(kind=kind(1))
!      roby_atom :: INTVEC*
!      if (PRESENT(atoms)) then
!          roby_atom.create(size(atoms))
!          roby_atom = atoms
!      else
!          .read_roby_atom(roby_atom)
!      end
!      n_roby_atom = size(roby_atom)
!      call ensure_(tonto,n_roby_atom == 2,"current routine only for two atoms")
!      stdout.dash(real_fields=3)
!      stdout.show("Ionic Energy =", .get_roby_ionic_energy( roby_atom(1:1), roby_atom(2:2)))
!      stdout.flush
!      roby_atom.destroy
!   end
!
!   put_roby_atom_energy(atoms) ::: leaky
!   ! Evaluate and put the Roby ionic energy for a group of atoms
!      atoms :: INTVEC, OPTIONAL
!      n_roby_atom, i, a :: integer(kind=kind(1))
!      roby_atom :: INTVEC*
!      E_T, E_P, E_A :: real(kind=kind(1.0d0))
!      if (PRESENT(atoms)) then
!          roby_atom.create(size(atoms))
!          roby_atom = atoms
!      else
!          .read_roby_atom(roby_atom)
!      end
!      n_roby_atom = size(roby_atom)
!      stdout.dash(real_fields=3)
!      stdout.flush
!      stdout.text("Roby atom energy analysis:")
!      stdout.flush
!      stdout.dash(real_fields=3)
!      stdout.flush
!      stdout.text("Atoms =")
!      do i=1,n_roby_atom
!        a = roby_atom(i)
!        stdout.text(trim(.atom(a).label))
!      end
!      E_T = .roby_total_atom_energy(roby_atom)
!      E_P = .roby_atom_energy(roby_atom)
!      E_A = E_T/2 + E_P/2
!      stdout.flush
!      stdout.show("Atom Total Energy =", E_T)
!      stdout.flush
!      stdout.show("Promoted Atom Energy =", E_P)
!      stdout.flush
!      stdout.show("Partitioned Atom Energy =", E_A)
!      stdout.flush
!      stdout.dash(real_fields=3)
!      stdout.flush
!      roby_atom.destroy
!   end
!
!   roby_atom_energy(roby_atom) result(energy) ::: leaky
!   ! Return the Roby promoted atom energy for the group of atoms whose
!   ! indices are given in "roby_atom".
!      roby_atom :: INTVEC
!      energy :: real(kind=kind(1.0d0))
!      D,D_save :: type(opmatrix_type)*
!      P, Q :: REALMAT*
!      D_save => .density_matrix
!      P.create(.n_bf, .n_bf)
!      if (.mult/=1) then
!        Q.create(.n_bf, .n_bf)
!        .make_roby_projected_density(P,roby_atom,.density_matrix.alpha)
!        .make_roby_projected_density(Q,roby_atom,.density_matrix.beta)
!        D.create(.n_bf,"unrestricted")
!        D.alpha = P        ! P is projected density
!        D.beta = Q         ! P is projected density
!        Q.destroy
!      else
!        D.create(.n_bf,"restricted")
!        .make_roby_projected_density(P,roby_atom)
!        D.restricted = P        ! P is projected density
!      end
!      .density_matrix => D
!      .make_fock_matrix(core=.false.)
!      if (.mult/=1) then
!          .add_core_hamiltonian(.fock_matrix.beta, roby_atom)
!          .add_core_hamiltonian(.fock_matrix.alpha, roby_atom)
!      else
!          .add_core_hamiltonian(.fock_matrix.restricted, roby_atom)
!      end
!      energy = .scf_electronic_energy(D,roby_atom) + .atom(roby_atom).nuclear_energy
!      .density_matrix => D_save
!      D.destroy
!      P.destroy
!   end
!
!   roby_total_atom_energy(roby_atom, nuclei, output) result(energy) ::: leaky
!   ! Return the total Roby energy for the group of atoms whose
!   ! indices are given in "roby_atom" If output is present and false energy breakdown
!   ! output is suppressed. If nuclei is given then only the nuclei specified
!   ! are used for E_N and N_N and N_E interactions, and the projected atoms specified by nuclei for the E_E
!   ! repulsions
!      roby_atom :: INTVEC
!      nuclei :: INTVEC, optional
!      output :: logical(kind=kind(.true.)), optional
!      do_output :: logical(kind=kind(.true.))
!      energy, E_k, Na_Em, Na_Ea, Ea_Nm, Ea_Em, Ea_Ea :: real(kind=kind(1.0d0))
!      Pa, Pb, Za, Zm, T :: REALMAT*
!      D, D_save :: REALMAT*
!      do_output=.true.
!      if (present(output)) do_output=output              ! puts out the energies and sub-energies and all that
!      T.create(.n_bf, .n_bf); Pa.create(.n_bf, .n_bf)
!      Za.create(.n_bf, .n_bf); Zm.create(.n_bf, .n_bf)
!      if (present(nuclei)) then
!         .make_nuclear_matrix(Zm,nuclei)   ! The e->n attractions of the atoms electrons for the "nuclei"
!      else
!         .make_nuclear_matrix(Zm)          ! The e->n attractions of the atoms electrons for all the nuclei
!      end
!      .make_nuclear_matrix(Za,roby_atom)   ! The n->e attractions felt by the nuclei in the roby atom
!      .make_kinetic_matrix(T)              ! The electronic kinetic energy matrix
!      if (.mult/=1) then
!        Pb.create(.n_bf, .n_bf)
!        if (present(nuclei)) then          ! if we only want to look at interactions with subgroups of the molecule
!          D_save => .density_matrix.alpha  ! store the "real" density matrix of the molecule
!          D => .density_matrix.beta
!          .make_roby_projected_density(Pa,nuclei,.density_matrix.alpha)
!          .make_roby_projected_density(Pb,nuclei,.density_matrix.beta)
!          .density_matrix.alpha => Pa      ! replace the "real" density matrix with the projected "nuclei"
!          .density_matrix.beta => Pb
!          .make_fock_matrix(core=.false.)    ! calculate e-e repulsions for all electrons in projected "nuclei"
!          !Na_Em = Za.trace_of_product(.density_matrix.alpha) + Za.trace_of_product(.density_matrix.beta)
!          Na_Em = .density_matrix.alpha.trace_of_product(Za) + .density_matrix.beta.trace_of_product(Za)
!                                           ! calculate n-e repulsions for all electrons in projected "nuclei"
!                                           ! and nuclei in "roby_atom"
!          .density_matrix.alpha => D_save  ! restore "real" density matrix
!          .density_matrix.beta => D
!        else
!          .make_fock_matrix(core=.false.)    ! calculate e-e repulsions for all electrons in molecule
!        ! Na_Em = Za.trace_of_product(.density_matrix.alpha) + Za.trace_of_product(.density_matrix.beta)
!          Na_Em = .density_matrix.alpha.trace_of_product(Za) + .density_matrix.beta.trace_of_product(Za)
!                                           ! calculate e-e repulsions for all electrons in molecule with roby_atom
!        end
!        D_save => .density_matrix.alpha    ! save "real" density matrix
!        D => .density_matrix.beta
!        .make_roby_projected_density(Pa,roby_atom,.density_matrix.alpha)
!        .make_roby_projected_density(Pb,roby_atom,.density_matrix.beta )
!        Ea_Em = .fock_matrix.alpha.trace_of_product(Pa) + .fock_matrix.beta.trace_of_product(Pb)
!        E_k = T.trace_of_product(Pa) + T.trace_of_product(Pb)     ! calculate Kinetic Energy for "roby_atom" electrons
!        Na_Ea = Za.trace_of_product(Pa) + Za.trace_of_product(Pb) ! calculate Nuclear -> electron energy  (A->A)
!        Ea_Nm = Zm.trace_of_product(Pa) + Zm.trace_of_product(Pb) ! calculate Nuclear -> electron energy  (M->A)
!        .density_matrix.alpha => Pa        ! set "real" density matrix to be the projected atoms "roby_atom"
!        .density_matrix.beta => Pb
!        .make_fock_matrix(core=.false.)      ! calculate repulsions within the "roby_atom"
!        Ea_Ea = .fock_matrix.alpha.trace_of_product(Pa) + .fock_matrix.beta.trace_of_product(Pb)
!        Ea_Em = Ea_Em - Ea_Ea/2            ! subtract 1/2 of the internal atom repulsions for double up
!        .density_matrix.alpha => D_save
!        .density_matrix.beta => D          ! reset "real" density matrix
!        Pb.destroy
!      else
!        if (present(nuclei)) then             ! to look at the energy of the atom in the field of only certain atoms "nuclei"
!          D_save => .density_matrix.restricted          ! save the "real" density matrix
!          .make_roby_projected_density(Pa,nuclei)   ! reset the "real" density matrix to a projected density
!          .density_matrix.restricted => Pa
!          Na_Em = Za.trace_of_product(.density_matrix.restricted)
!          .make_fock_matrix(core=.false.)      ! no core and for the whole molecule
!          .density_matrix.restricted => D_save
!        else
!          Na_Em = Za.trace_of_product(.density_matrix.restricted)
!          .make_fock_matrix(core=.false.)
!        end
!        D_save => .density_matrix.restricted
!        .make_roby_projected_density(Pa,roby_atom)
!        Ea_Em = .fock_matrix.restricted.trace_of_product(Pa)
!        Na_Ea = Za.trace_of_product(Pa)
!        Ea_Nm = Zm.trace_of_product(Pa)
!        E_k = T.trace_of_product(Pa)
!        .density_matrix.restricted => Pa
!        .make_fock_matrix(core=.false.)      ! no core and for the whole molecule
!        Ea_Ea = .fock_matrix.restricted.trace_of_product(Pa)
!        Ea_Em = Ea_Em - Ea_Ea/2
!        .density_matrix.restricted => D_save
!      end
!      energy = Na_Em + E_k + Ea_Em + Ea_Nm - Na_Ea
!      if (present(nuclei)) then
!        energy = energy + .atom.nuclear_energy(roby_atom,nuclei)
!      else
!        energy = energy + .atom.nuclear_energy(roby_atom)
!      end
!      if (do_output) then
!       stdout.dash(real_fields=3)
!       stdout.show("N_a to E_m:",Na_Em)
!       stdout.show("N_a to E_a:",Na_Ea)
!       stdout.show("E_a to N_m:",Ea_Nm)
!       stdout.show("E_a to E_m:",Ea_Em)
!       stdout.show("N_a to N_m:", .atom.nuclear_energy(roby_atom))
!       stdout.show("Kinetic Energy:", E_k)
!       stdout.show("Total Energy:", energy)
!       stdout.dash(real_fields=3)
!      end
!      Pa.destroy
!   end

!-----------------------------------------!
! Plot covalent and ionic theta orbitals  !
!-----------------------------------------!

!   plot_roby_orbitals
!   ! Plots the covalent and ionic orbitals to a grid
!   roby_atom, roby_atom1, roby_atom2 :: INTVEC*
!     .read_roby_groups(roby_atom,roby_atom1,roby_atom2)
!     .plot_cos_sin_orbitals(roby_atom, roby_atom1, roby_atom2)
!     roby_atom.destroy
!     roby_atom1.destroy
!     roby_atom2.destroy
!   end
!
!   plot_cos_sin_orbitals(roby_atom, roby_atom1, roby_atom2)
!   ! Plots the cos and sin Roby-Gould orbitals
!   ! -----------------------------------------
!      roby_atom, roby_atom1, roby_atom2 :: INTVEC
!      C, theta_c, S, theta_s :: REALMAT*
!      cval, sval :: REALVEC*
!      c_pair, s_pair, cs_pair :: INTVEC*
!      n_a, n_b, n_ab :: integer(kind=kind(1))
!      i, j, k, kk :: integer(kind=kind(1))
!      proj_COs, proj_SOs, saved_NOs :: type(opmatrix_type)*
!      arch :: type(archive_type)
!      gr :: REALVEC*
!      pi_space, blurb :: logical(kind=kind(.true.))
!   ! -----------------------------------------
!      blurb = .true.
!      call die_if_(tonto,.grid.destroyed,"no grid exists")
!      call die_if_(tonto,.mult/=1,"multiplicity /= 1")
!      call die_if_(tonto,.natural_orbitals.number_kind /= "real","NOs not real")
!      n_a = .atom(roby_atom1).n_bf
!      n_b = .atom(roby_atom2).n_bf
!      n_ab = .atom(roby_atom).n_bf
!      call die_if_(tonto,n_ab /= n_a + n_b,"n-ab /= n_a + n_b")
!      C.create(n_ab,n_ab); theta_c.create(n_ab, n_ab); cval.create(n_ab)
!      S.create(n_ab,n_ab); theta_s.create(n_ab, n_ab); sval.create(n_ab)
!      .make_shared_operator(C)
!      .make_ionic_operator(S)
!      .diagonalise_V_AB_operator(C,roby_atom,theta_c,cval)
!      .diagonalise_V_AB_operator(S,roby_atom,theta_s,sval)
!      c_pair.create(n_ab); s_pair.create(n_ab); cs_pair.create(n_ab)
!      .find_pairs(cval, sval, c_pair,s_pair,cs_pair)
!      .put_roby_eigenvalues(roby_atom1,roby_atom2,sval,cval,c_pair,s_pair,cs_pair,blurb)
!      saved_NOs => .natural_orbitals
!      gr.create(.grid.n_pt)
!      proj_COs.create( .n_bf, "restricted")
!      proj_SOs.create( .n_bf, "restricted")
!      .AO_subspace_set(proj_COs.restricted, theta_c, roby_atom)
!      .AO_subspace_set(proj_SOs.restricted, theta_s, roby_atom)
!      pi_space = .true.
!      do i = 1, n_ab
!        stdout.flush; stdout.put("i,c_pair(i),s_pair(i),cs_pair(i) = ")
!        stdout.put(i); stdout.put(c_pair(i)); stdout.put(s_pair(i)); stdout.put(cs_pair(i))
!        j = cs_pair(i)
!        if (j < 0) cycle
!        stdout.put("cs_pair(i) >= 0")
!        if (pi_space .and. s_pair(j) == -1) then
!           ! sin: pi/2
!           stdout.flush; stdout.put("pi/2")
!           k = j
!           kk = i
!           .natural_orbitals => proj_SOs
!           .make_orbital_grid(gr,k)
!           arch.set(.name,"sin_pi_grid")
!           arch.write(gr)
!           arch.set(.name,"sin_pi_grid"//trim(kk.to_str)//"",format="ascii")
!           arch.write_gnuplot(gr, .grid.n_x, .grid.n_y, .grid.n_z)
!        end
!        if (s_pair(j) > -1) then
!           stdout.flush; stdout.put("other")
!           ! cos: +
!           k = i
!           kk = k
!           .natural_orbitals => proj_COs
!           .make_orbital_grid(gr,k)
!           arch.set(.name,"cos_orbital_grid")
!           arch.write(gr)
!           arch.set(.name,"cos_orbital_grid"//trim(kk.to_str)//"",format="ascii")
!           arch.write_gnuplot(gr, .grid.n_x, .grid.n_y, .grid.n_z)
!           ! cos: -
!           k = c_pair(i)
!           kk = k
!           .make_orbital_grid(gr,k)
!           arch.set(.name,"cos_orbital_grid")
!           arch.write(gr)
!           arch.set(.name,"cos_orbital_grid"//trim(kk.to_str)//"",format="ascii")
!           arch.write_gnuplot(gr, .grid.n_x, .grid.n_y, .grid.n_z)
!           ! sin: +
!           k = j
!           kk = i
!           .natural_orbitals => proj_SOs
!           .make_orbital_grid(gr,k)
!           arch.set(.name,"sin_orbital_grid")
!           arch.write(gr)
!           arch.set(.name,"sin_orbital_grid"//trim(kk.to_str)//"",format="ascii")
!           arch.write_gnuplot(gr, .grid.n_x, .grid.n_y, .grid.n_z)
!           ! sin: -
!           k = s_pair(j)
!           kk = c_pair(i)
!           .make_orbital_grid(gr,k)
!           arch.set(.name,"sin_orbital_grid")
!           arch.write(gr)
!           arch.set(.name,"sin_orbital_grid"//trim(kk.to_str)//"",format="ascii")
!           arch.write_gnuplot(gr, .grid.n_x, .grid.n_y, .grid.n_z)
!        end
!      end
!      .natural_orbitals => saved_NOs
!      gr.destroy
!      proj_COs.destroy
!      proj_SOs.destroy
!      C.destroy; theta_c.destroy; cval.destroy
!      S.destroy; theta_s.destroy; sval.destroy
!      c_pair.destroy; s_pair.destroy; cs_pair.destroy
!   end

!  ***************************
!  Cluster generation routines
!  ***************************

   subroutine read_cluster(self)
    type(mol_type) :: self
    ! Read in the crystal cluster data. NOTE: the .atom list must correspond to
    ! the asymmetric unit cell geometry of the .crystal.

      call ensure_(tonto,associated(self%atom),"MOL:read_cluster ... no atom data")
      call ensure_(tonto,associated(self%crystal),"MOL:read_cluster ... no crystal data")
      call destroy_(self%cluster)
      if (asymmetric_unit_exists_(self%crystal)) then
         call create_(self%cluster,self%crystal,self%atom)
      else
         call make_asymmetric_geometry_(self%crystal)
         call create_from_molecule_(self%cluster,self%crystal,self%atom)
      end if
      call read_keywords_(self%cluster)

   end subroutine

   subroutine create_cluster(self)
    type(mol_type) :: self
    ! Create a new "self" by generating a cluster from information stored in
    ! .cluster.  The original molecule is stored in .saved and can be recovered
    ! by the "destroy_cluster" routine.
     pointer :: self

     call ensure_(tonto,associated(self%crystal),"MOL:create_cluster ... no crystal data")
     call ensure_(tonto,associated(self%cluster),"MOL:create_cluster ... no cluster data")
     call ensure_(tonto,self%cluster%info_made,"MOL:create_cluster ... no cluster data")
     call save_(self)  ! Save the original molecule
      ! Making a new cluster from .saved
     self%name  =  trim(self%saved%name) // "_cluster"
      ! Get type(cif_type) information -- a cluster usually comes from type(cif_type)
     self%CIF_file_name = self%saved%CIF_file_name
     self%CIF_data_block_name = self%saved%CIF_data_block_name
     self%CX_file_name = self%saved%CX_file_name
      ! Create copies so that they can easily be destroyed without leaks
     call create_copy_(self%crystal,self%saved%crystal)
     call create_copy_(self%cluster,self%saved%cluster)
     call create_atom_list_(self%cluster,self%atom)                  ! Make new .atom list
      ! Make sure basis is OK.
     self%basis_set_kind = self%saved%basis_set_kind
     if (associated(self%saved%basis)) call create_copy_(self%basis,self%saved%basis)
     if (associated(self%saved%slaterbasis)) call create_copy_(self%slaterbasis,self%saved%slaterbasis)
     if (associated(self%saved%coppensbasis)) call create_copy_(self%coppensbasis,self%saved%coppensbasis)
     call set_atom_info_(self)
     call resolve_basis_info_(self)
    ! *** not tested yet
    ! if (.saved.density_matrix.created) then
    ! if (.saved.density_matrix.restricted.created) then
    !    .density_matrix.create(.n_bf,"restricted")
    !    .cluster.make_density_matrix(.density_matrix.restricted,.saved.density_matrix.restricted,.atom)
    ! end
    ! end
     call flush_(stdout)
     call text_(stdout,"New molecule created with name: " // trim(self%name))
     call flush_(stdout)
     call text_(stdout,'To recover the original molecule use the "unsave" or "destroy_cluster" keyword')

   end subroutine

   subroutine destroy_cluster(self)
    type(mol_type) :: self
    ! Destroy a molecule created by the "create_cluster" routine, and recover
    ! the original molecule from .saved
     pointer :: self

     call ensure_(tonto,associated(self%saved),"MOL:destroy_cluster ... no crystal data")
     call unsave_(self)

   end subroutine

!  *************
!  Miscellaneous
!  *************

!   put_sylvian_csizmadia_tensors
!   ! Put out the Sylvian-Csizmadia polarisability tensors.
!   ! This routine will read the value of the Unsold denominator.
!      Dx,Dy,Dz, P,Pi,Pj,MOi,MOj :: REALMAT*
!      Qxx,Qyy,Qzz,Qxy,Qxz,Qyz :: REALMAT*
!      Oxxx,Oyyy,Ozzz,Oxxy,Oxxz,Oyyx,Oyyz,Ozzx,Ozzy,Oxyz :: REALMAT*
!      x,y,z, i,j :: integer(kind=kind(1))
!      delta,fac :: real(kind=kind(1.0d0))
!      alpha :: real(kind=kind(1.0d0)), dimension(3,3)
!      beta :: real(kind=kind(1.0d0)), dimension(3,3,3)
!      alpha1 :: REALMAT3*
!      alpha2 :: REALMAT4*
!   call ensure_(tonto,.density_matrix.created,"no density matrix")
!   call ensure_(tonto,.basis_info_made,"no basis info")
!   call ensure_(tonto,.atom.created,"no atom info")
!      stdout.flush
!      stdout.text("Sylvian-Csizmadia polarisability tensors")
!      stdout.flush
!      stdin.read(delta)
!      stdout.show("Unsold denominator/a.u. =",delta)
!      Dx.create(.n_bf,.n_bf); Dy.create(.n_bf,.n_bf); Dz.create(.n_bf,.n_bf)
!      Qxx.create(.n_bf,.n_bf); Qyy.create(.n_bf,.n_bf); Qzz.create(.n_bf,.n_bf)
!      Qxy.create(.n_bf,.n_bf); Qxz.create(.n_bf,.n_bf); Qyz.create(.n_bf,.n_bf)
!      Oxxx.create(.n_bf,.n_bf); Oyyy.create(.n_bf,.n_bf); Ozzz.create(.n_bf,.n_bf)
!      Oxxy.create(.n_bf,.n_bf); Oxxz.create(.n_bf,.n_bf)
!      Oyyx.create(.n_bf,.n_bf); Oyyz.create(.n_bf,.n_bf)
!      Ozzx.create(.n_bf,.n_bf); Ozzy.create(.n_bf,.n_bf)
!      Oxyz.create(.n_bf,.n_bf)
!      .get_dipole_matrices(Dx,Dy,Dz)
!      .get_quadrupole_matrices(Qxx,Qyy,Qzz,Qxy,Qxz,Qyz)
!      .get_octupole_matrices(Oxxx,Oyyy,Ozzz,Oxxy,Oxxz,Oyyx,Oyyz,Ozzx,Ozzy,Oxyz)
!      .make_ao_density_matrix
!      x = 1; y = 2; z = 3
!      P.create(.n_bf,.n_bf)
!      P = 0.50d0*.density_matrix.restricted
!      alpha(x,x) = P.trace_product_with(Qxx) - P.trace_product_with(Dx,P,Dx)
!      alpha(y,y) = P.trace_product_with(Qyy) - P.trace_product_with(Dy,P,Dy)
!      alpha(z,z) = P.trace_product_with(Qzz) - P.trace_product_with(Dz,P,Dz)
!      alpha(y,x) = P.trace_product_with(Qxy) - P.trace_product_with(Dy,P,Dx)
!      alpha(z,x) = P.trace_product_with(Qxz) - P.trace_product_with(Dz,P,Dx)
!      alpha(z,y) = P.trace_product_with(Qyz) - P.trace_product_with(Dz,P,Dy)
!      alpha.symmetric_reflect
!      fac = 4.0d0/delta
!      alpha = fac*alpha
!      stdout.flush
!      stdout.show("No. of occupiedf orbitals = ",.n_a)
!      stdout.flush
!      stdout.text("Orbital eigenvalues:")
!      stdout.flush
!      stdout.put(.orbital_energies,"column")
!      stdout.flush
!      stdout.text("First polarisability:")
!      stdout.flush
!      stdout.put(alpha)
!      ! Evaluate the orbital contributions
!      alpha1.create(3,3,.n_a); alpha1 = 0.0d0
!      alpha2.create(3,3,.n_a,.n_a); alpha2 = 0.0d0
!      Pi.create(.n_bf,.n_bf)
!      Pj.create(.n_bf,.n_bf)
!      do i = 1,.n_a
!         MOi => .molecular_orbitals.restricted(:,i:i)
!         Pi.to_product_of(MOi,MOi,transpose_b=.true.)
!         alpha1(x,x,i) = Pi.trace_product_with(Qxx) - Pi.trace_product_with(Dx,Pi,Dx)
!         alpha1(y,y,i) = Pi.trace_product_with(Qyy) - Pi.trace_product_with(Dy,Pi,Dy)
!         alpha1(z,z,i) = Pi.trace_product_with(Qzz) - Pi.trace_product_with(Dz,Pi,Dz)
!         alpha1(y,x,i) = Pi.trace_product_with(Qxy) - Pi.trace_product_with(Dy,Pi,Dx)
!         alpha1(z,x,i) = Pi.trace_product_with(Qxz) - Pi.trace_product_with(Dz,Pi,Dx)
!         alpha1(z,y,i) = Pi.trace_product_with(Qyz) - Pi.trace_product_with(Dz,Pi,Dy)
!         alpha1(:,:,i).symmetric_reflect
!         do j = 1,(i-1)
!            MOj => .molecular_orbitals.restricted(:,j:j)
!            Pj.to_product_of(MOj,MOj,transpose_b=.true.)
!            alpha2(x,x,i,j) = -Pi.trace_product_with(Dx,Pj,Dx)-Pj.trace_product_with(Dx,Pi,Dx)
!            alpha2(y,y,i,j) = -Pi.trace_product_with(Dy,Pj,Dy)-Pj.trace_product_with(Dy,Pi,Dy)
!            alpha2(z,z,i,j) = -Pi.trace_product_with(Dz,Pj,Dz)-Pj.trace_product_with(Dz,Pi,Dz)
!            alpha2(y,x,i,j) = -Pi.trace_product_with(Dy,Pj,Dx)-Pj.trace_product_with(Dy,Pi,Dx)
!            alpha2(z,x,i,j) = -Pi.trace_product_with(Dz,Pj,Dx)-Pj.trace_product_with(Dz,Pi,Dx)
!            alpha2(z,y,i,j) = -Pi.trace_product_with(Dz,Pj,Dy)-Pj.trace_product_with(Dz,Pi,Dy)
!            alpha2(:,:,i,j).symmetric_reflect
!         end
!      end
!      Pj.destroy
!      Pi.destroy
!      alpha1 = fac*alpha1
!      alpha2 = fac*alpha2
!      stdout.flush
!      stdout.text("First polarisability, orbital contributions:")
!      stdout.flush
!      do i = 1,.n_a
!         stdout.text("... for orbital "//i.to_str.trim)
!         stdout.put(alpha1(:,:,i))
!      end
!      stdout.flush
!      stdout.text("First polarisability, orbital pair contributions:")
!      stdout.flush
!      do i = 1,.n_a
!      do j = 1,(i-1)
!         stdout.text("... for orbitals "//i.to_str.trim//" and "//j.to_str.trim)
!         stdout.put(alpha2(:,:,i,j))
!      end
!      end
!      stdout.flush
!      stdout.text("Sum of all orbital contributions:")
!      stdout.flush
!      alpha = 0.0d0
!      do i = 1,.n_a
!         alpha = alpha + alpha1(:,:,i)
!         do j = 1,(i-1)
!            alpha = alpha + alpha2(:,:,i,j)
!         end
!      end
!      stdout.put(alpha)
!      alpha2.destroy
!      alpha1.destroy
!      beta(x,x,x) = P.trace_product_with(Oxxx) - 3.0d0*P.trace_product_with(Dx,P,Qxx) &
!                  + P.trace_product_with(Dx,P,Dx,P,Dx)
!      beta(y,y,y) = P.trace_product_with(Oyyy) - 3.0d0*P.trace_product_with(Dy,P,Qyy) &
!                  + P.trace_product_with(Dy,P,Dy,P,Dy)
!      beta(z,z,z) = P.trace_product_with(Ozzz) - 3.0d0*P.trace_product_with(Dz,P,Qzz) &
!                  + P.trace_product_with(Dz,P,Dz,P,Dz)
!      beta(y,x,x) = P.trace_product_with(Oxxy) - 2.0d0*P.trace_product_with(Dx,P,Qxy) &
!                  - P.trace_product_with(Dy,P,Qxx) + P.trace_product_with(Dy,P,Dx,P,Dx)
!      beta(z,x,x) = P.trace_product_with(Oxxz) - 2.0d0*P.trace_product_with(Dx,P,Qxz) &
!                  - P.trace_product_with(Dz,P,Qxx) + P.trace_product_with(Dz,P,Dx,P,Dx)
!      beta(y,y,x) = P.trace_product_with(Oyyx) - 2.0d0*P.trace_product_with(Dy,P,Qxy) &
!                  - P.trace_product_with(Dx,P,Qyy) + P.trace_product_with(Dy,P,Dy,P,Dx)
!      beta(z,y,x) = P.trace_product_with(Oxyz) - P.trace_product_with(Dz,P,Qxy) &
!                  - P.trace_product_with(Dy,P,Qxz) - P.trace_product_with(Dx,P,Qyz) &
!                  + P.trace_product_with(Dz,P,Dy,P,Dx)
!      beta(z,z,x) = P.trace_product_with(Ozzx) - 2.0d0*P.trace_product_with(Dz,P,Qxz) &
!                  - P.trace_product_with(Dx,P,Qzz) + P.trace_product_with(Dz,P,Dz,P,Dx)
!      beta(z,y,y) = P.trace_product_with(Oyyz) - 2.0d0*P.trace_product_with(Dy,P,Qyz) &
!                  - P.trace_product_with(Dz,P,Qyy) + P.trace_product_with(Dz,P,Dy,P,Dy)
!      beta(z,z,y) = P.trace_product_with(Ozzy) - 2.0d0*P.trace_product_with(Dz,P,Qyz) &
!                  - P.trace_product_with(Dy,P,Qzz) + P.trace_product_with(Dz,P,Dz,P,Dy)
!      beta.make_symmetric
!      fac = 12.0d0/delta
!      beta = fac*beta
!      stdout.flush
!      stdout.text("Second polarisability:")
!      stdout.flush
!      stdout.put(beta)
!      P.destroy
!      Oxyz.destroy
!      Ozzy.destroy; Ozzx.destroy
!      Oyyz.destroy; Oyyx.destroy
!      Oxxz.destroy; Oxxy.destroy
!      Ozzz.destroy; Oyyy.destroy; Oxxx.destroy
!      Qyz.destroy; Qxz.destroy; Qxy.destroy
!      Qzz.destroy; Qyy.destroy; Qxx.destroy
!      Dz.destroy; Dy.destroy; Dx.destroy
!   end

   subroutine make_weak_force_energy_shift(self)
    type(mol_type) :: self
    ! Make the expectation value of the parity-violating energy shift.
    ! Requires some archived molecular orbitals, general_complex kind.
    ! Reference: R. Zanasi and P. Lazzeretti, CPL 286, 240 (1998)
       real(kind=kind(1.0d0)), dimension(:,:), pointer :: pt
       complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: Pa,Pb
       complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: Na,Nb
       real(kind=kind(1.0d0)), dimension(:), pointer :: PV,NN
       integer(kind=kind(1)) :: n, x,y,z
       real(kind=kind(1.0d0)) :: Gamma
        complex(kind=kind((1.0d0,1.0d0))) :: I
       type(archive_type) :: arch

   call ensure_(tonto,self%basis_info_made,"MOL:make_weak_force_energy_shift ... no basis info")
       call destroy_ptr_part_(self%molecular_orbitals)
       call set_(arch,self%name,"molecular_orbitals")
       call read_(arch,self%molecular_orbitals, genre="general_complex")
       Gamma = 5.73416d-17
       call create_(PV,self%n_atom)
       call create_(pt,self%n_atom,3)
       call get_coordinates_(self%atom,pt)
       call create_(Pa,self%n_atom);   call create_(Pb,self%n_atom)
       call create_(Na,self%n_atom,3); call create_(Nb,self%n_atom,3)
       I = (0.0d0,1.0d0)
       call set_real_style_(stdout,"d")
       x = 1; y = 2; z = 3
       PV = 0.0d0
       do n = 1,self%n_e
          call make_nabla_orbital_grid_c_(self,Na,Pa,self%molecular_orbitals%general_complex(      1:  self%n_bf,n), pt)
          call make_nabla_orbital_grid_c_(self,Nb,Pb,self%molecular_orbitals%general_complex(self%n_bf+1:2*self%n_bf,n), pt)
          PV(:) = PV(:) &
                + I*conjg(Nb(:,x))*Pa(:) + I*conjg(Na(:,x))*Pb(:) &
                - I*conjg(Pb(:))*Na(:,x) - I*conjg(Pa(:))*Nb(:,x) &
                +   conjg(Nb(:,y))*Pa(:) -   conjg(Na(:,y))*Pb(:) &
                +   conjg(Pb(:))*Na(:,y) -   conjg(Pa(:))*Nb(:,y) &
                + I*conjg(Na(:,z))*Pa(:) - I*conjg(Nb(:,z))*Pb(:) &
                - I*conjg(Pa(:))*Na(:,z) + I*conjg(Pb(:))*Nb(:,z)
       end do
       PV = -0.50d0*Gamma*PV
       call destroy_(Nb); call destroy_(Na)
       call destroy_(Pb); call destroy_(Pa)
       call destroy_(pt)
       call create_(NN,self%n_atom)
       call get_mean_neutron_numbers_(self%atom,NN)
       PV = PV*NN

       call set_real_style_(stdout,"d")
       call flush_(stdout)
       call text_(stdout,"Contributions (by nucleus) to the parity-violating weak force energy shift term:")
       call flush_(stdout)
       call put_(stdout,PV, format="column")
       call flush_(stdout)
       call show_(stdout,"Total contribution =", sum(PV) )
       call flush_(stdout)
       call text_(stdout,"Neutron numbers:")
       call flush_(stdout)
       call put_(stdout,NN, format="column")
       call set_real_style_(stdout,"f")
       call destroy_(NN)
       call destroy_(PV)
       call destroy_ptr_part_(self%molecular_orbitals)

   end subroutine

   subroutine put_g_tensor_information(self)
    type(mol_type) :: self
    ! Put the g-tensor shift information to the output.
    ! Reference: Jayatilaka, JCP 108, 7587 (1998)
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: S, Lx,Ly,Lz, Qxx,Qxy,Qxz,Qyx,Qyy,Qyz,Qzx,Qzy,Qzz, T
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: HH,P
      real(kind=kind(1.0d0)) :: Sx,Sy,Sz,SS_net, Mx,My,Mz,MM_net
      real(kind=kind(1.0d0)) :: Qx,Qy,Qz,QQ_net, Tx,Ty,Tz,TT_net
      real(kind=kind(1.0d0)) :: Sx_ppm,Sy_ppm,Sz_ppm, Mx_ppm,My_ppm,Mz_ppm
      real(kind=kind(1.0d0)) :: Qx_ppm,Qy_ppm,Qz_ppm, Tx_ppm,Ty_ppm,Tz_ppm
      real(kind=kind(1.0d0)) :: fac, fx,fy,fz, total
      real(kind=kind(1.0d0)), dimension(3) :: quantization_axis
       complex(kind=kind((1.0d0,1.0d0))) :: I
       integer(kind=kind(1)) :: m
      logical(kind=kind(.true.)) :: int_width
      type(archive_type) :: arch

      call destroy_ptr_part_(self%density_matrix)
      call set_(arch,self%name,"density_matrix")
      call read_(arch,self%density_matrix, genre="general_complex")
      P => self%density_matrix%general_complex
      call create_(HH,2*self%n_bf,2*self%n_bf)
      I = (0.0d0,1.0d0)
      quantization_axis = self%scfdata%quantization_axis
       ! Spin contribution
      call create_(S,self%n_bf,self%n_bf)
      call get_overlap_matrix_(self,S)
      fac = 2.002319304386/4.0d0
      HH = 0.0d0
      call beta_alpha_plus_(HH,S,factor=fac)
      call alpha_beta_plus_(HH,S,factor=fac)
      Sx = trace_of_product_(HH,P)
      HH = 0.0d0
      call beta_alpha_plus_(HH,S,factor=fac*I)
      call alpha_beta_plus_(HH,S,factor=-fac*I)
      Sy = trace_of_product_(HH,P)
      HH = 0.0d0
      call alpha_alpha_plus_(HH,S,factor=fac)
      call beta_beta_plus_(HH,S,factor=-fac)
      Sz = trace_of_product_(HH,P)
      call destroy_(S)
      m = self%n_a - self%n_b
      fx = m*fac*quantization_axis(1)
      fy = m*fac*quantization_axis(2)
      fz = m*fac*quantization_axis(3)
      Sx_ppm = (Sx-fx)*4000000/m
      Sy_ppm = (Sy-fy)*4000000/m
      Sz_ppm = (Sz-fz)*4000000/m
      SS_net = Sx_ppm*quantization_axis(1) + Sy_ppm*quantization_axis(2) + Sz_ppm*quantization_axis(3)
       ! L contribution
      call create_(Lx,self%n_bf,self%n_bf)
      call create_(Ly,self%n_bf,self%n_bf)
      call create_(Lz,self%n_bf,self%n_bf)
      call get_L_matrices_(self,Lx,Ly,Lz)
      fac = 0.50d0
      HH = 0.0d0
      call alpha_alpha_plus_(HH,Lx,factor=-fac*I)   ! -I factor for the nabla part
      call beta_beta_plus_(HH,Lx,factor=-fac*I)
      Mx = trace_of_product_(HH,P)
      HH = 0.0d0
      call alpha_alpha_plus_(HH,Ly,factor=-fac*I)
      call beta_beta_plus_(HH,Ly,factor=-fac*I)
      My = trace_of_product_(HH,P)
      HH = 0.0d0
      call alpha_alpha_plus_(HH,Lz,factor=-fac*I)
      call beta_beta_plus_(HH,Lz,factor=-fac*I)
      Mz = trace_of_product_(HH,P)
      call destroy_(Lz); call destroy_(Ly); call destroy_(Lx)
      Mx_ppm = Mx*4000000/m
      My_ppm = My*4000000/m
      Mz_ppm = Mz*4000000/m
      MM_net = Mx_ppm*quantization_axis(1) + My_ppm*quantization_axis(2) + Mz_ppm*quantization_axis(3)
       ! 1 electron LS gauge contribution
      call create_(Qxx,self%n_bf,self%n_bf); call create_(Qxy,self%n_bf,self%n_bf); call create_(Qxz,self%n_bf,self%n_bf)
      call create_(Qyx,self%n_bf,self%n_bf); call create_(Qyy,self%n_bf,self%n_bf); call create_(Qyz,self%n_bf,self%n_bf)
      call create_(Qzx,self%n_bf,self%n_bf); call create_(Qzy,self%n_bf,self%n_bf); call create_(Qzz,self%n_bf,self%n_bf)
      call get_spin_orbit_Q_matrices_(self,Qxx,Qxy,Qxz,Qyx,Qyy,Qyz,Qzx,Qzy,Qzz)
      fac = 2.002319304386/(2.0d0*8.0d0*137.03599d0*137.03599d0)
      HH = 0.0d0
      call beta_alpha_plus_(HH,Qyy,factor=fac)
      call beta_alpha_plus_(HH,Qzz,factor=fac)
      call beta_alpha_plus_(HH,Qyx,factor=-I*fac)
      call alpha_alpha_plus_(HH,Qzx,factor=-fac)
      call beta_beta_plus_(HH,Qzx,factor=+fac)
      call make_hermitian_(HH)
      Qx = trace_of_product_(HH,P)
      HH = 0.0d0
      call beta_alpha_plus_(HH,Qxx,factor=I*fac)
      call beta_alpha_plus_(HH,Qzz,factor=I*fac)
      call beta_alpha_plus_(HH,Qxy,factor=-fac)
      call alpha_alpha_plus_(HH,Qzy,factor=-fac)
      call beta_beta_plus_(HH,Qzy,factor=+fac)
      call make_hermitian_(HH)
      Qy = trace_of_product_(HH,P)
      HH = 0.0d0
      call alpha_alpha_plus_(HH,Qxx,factor=fac)
      call alpha_alpha_plus_(HH,Qyy,factor=fac)
      call beta_beta_plus_(HH,Qxx,factor=-fac)
      call beta_beta_plus_(HH,Qyy,factor=-fac)
      call beta_alpha_plus_(HH,Qxz,factor=-fac)
      call beta_alpha_plus_(HH,Qyz,factor=-I*fac)
      call make_hermitian_(HH)
      Qz = trace_of_product_(HH,P)
      call destroy_(Qzz); call destroy_(Qzy); call destroy_(Qzx)
      call destroy_(Qyz); call destroy_(Qyy); call destroy_(Qyx)
      call destroy_(Qxz); call destroy_(Qxy); call destroy_(Qxx)
      Qx_ppm = Qx*4000000/m
      Qy_ppm = Qy*4000000/m
      Qz_ppm = Qz*4000000/m
      QQ_net = Qx_ppm*quantization_axis(1) + Qy_ppm*quantization_axis(2) + Qz_ppm*quantization_axis(3)
       ! Relativistic kinetic energy contribution
      call create_(T,self%n_bf,self%n_bf)
      call get_kinetic_matrix_(self,T)
      fac = -2.002319304386/(4.0d0*137.03599d0*137.03599d0)
      HH = 0.0d0
      call beta_alpha_plus_(HH,T,factor=fac)
      call alpha_beta_plus_(HH,T,factor=fac)
      Tx = trace_of_product_(HH,P)
      HH = 0.0d0
      call beta_alpha_plus_(HH,T,factor=I*fac)
      call alpha_beta_plus_(HH,T,factor=-I*fac)
      Ty = trace_of_product_(HH,P)
      HH = 0.0d0
      call alpha_alpha_plus_(HH,T,factor=fac)
      call beta_beta_plus_(HH,T,factor=-fac)
      Tz = trace_of_product_(HH,P)
      call destroy_(T)
      call destroy_(HH)
      call destroy_ptr_part_(self%density_matrix)
      Tx_ppm = Tx*4000000/m
      Ty_ppm = Ty*4000000/m
      Tz_ppm = Tz*4000000/m
      TT_net = Tx_ppm*quantization_axis(1) + Ty_ppm*quantization_axis(2) + Tz_ppm*quantization_axis(3)

      call set_real_style_(stdout,"d")
      int_width = .true.
      call flush_(stdout)
      call text_(stdout,"Contribution to g-tensor shift:")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=2)
      call put_(stdout,"Term",int_width); call put_(stdout,"<value>");
      call put_(stdout,"shift/ppm"); call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=2)
      call text_(stdout,"Spin term:")
      call put_(stdout,"S_x",int_width)
      call set_real_style_(stdout,"d"); call put_(stdout,Sx)
      call set_real_style_(stdout,"f"); call put_(stdout,Sx_ppm); call flush_(stdout)
      call put_(stdout,"S_y",int_width)
      call set_real_style_(stdout,"d"); call put_(stdout,Sy)
      call set_real_style_(stdout,"f"); call put_(stdout,Sy_ppm); call flush_(stdout)
      call put_(stdout,"S_z",int_width)
      call set_real_style_(stdout,"d"); call put_(stdout,Sz);
      call set_real_style_(stdout,"f"); call put_(stdout,Sz_ppm); call flush_(stdout)
      call put_(stdout,"Net",int_width); call tab_(stdout,real_fields=1);
      call put_(stdout,SS_net); call flush_(stdout)
      call text_(stdout,"Angular momentum term:")
      call put_(stdout,"L_x",int_width)
      call set_real_style_(stdout,"d"); call put_(stdout,Mx);
      call set_real_style_(stdout,"f"); call put_(stdout,Mx_ppm); call flush_(stdout)
      call put_(stdout,"L_y",int_width)
      call set_real_style_(stdout,"d"); call put_(stdout,My);
      call set_real_style_(stdout,"f"); call put_(stdout,My_ppm); call flush_(stdout)
      call put_(stdout,"L_z",int_width)
      call set_real_style_(stdout,"d"); call put_(stdout,Mz);
      call set_real_style_(stdout,"f"); call put_(stdout,Mz_ppm); call flush_(stdout)
      call put_(stdout,"Net",int_width); call tab_(stdout,real_fields=1);
      call put_(stdout,MM_net); call flush_(stdout)
      call text_(stdout,"1-electron L:S gauge term:")
      call put_(stdout,"Q_x",int_width)
      call set_real_style_(stdout,"d"); call put_(stdout,Qx);
      call set_real_style_(stdout,"f"); call put_(stdout,Qx_ppm); call flush_(stdout)
      call put_(stdout,"Q_y",int_width)
      call set_real_style_(stdout,"d"); call put_(stdout,Qy);
      call set_real_style_(stdout,"f"); call put_(stdout,Qy_ppm); call flush_(stdout)
      call put_(stdout,"Q_z",int_width)
      call set_real_style_(stdout,"d"); call put_(stdout,Qz);
      call set_real_style_(stdout,"f"); call put_(stdout,Qz_ppm); call flush_(stdout)
      call put_(stdout,"Net",int_width); call tab_(stdout,real_fields=1);
      call put_(stdout,QQ_net); call flush_(stdout)
      call text_(stdout,"Relativistic B:S kinetic term:")
      call put_(stdout,"T_x",int_width)
      call set_real_style_(stdout,"d"); call put_(stdout,Tx)
      call set_real_style_(stdout,"f"); call put_(stdout,Tx_ppm); call flush_(stdout)
      call put_(stdout,"T_y",int_width)
      call set_real_style_(stdout,"d"); call put_(stdout,Ty);
      call set_real_style_(stdout,"f"); call put_(stdout,Ty_ppm); call flush_(stdout)
      call put_(stdout,"T_z",int_width)
      call set_real_style_(stdout,"d"); call put_(stdout,Tz);
      call set_real_style_(stdout,"f"); call put_(stdout,Tz_ppm); call flush_(stdout)
      call put_(stdout,"Net",int_width); call tab_(stdout,real_fields=1);
      call put_(stdout,TT_net); call flush_(stdout)
      total = SS_net + MM_net + QQ_net + TT_net
      call flush_(stdout)
      call put_(stdout,"Total:",int_width); call tab_(stdout,real_fields=1); call put_(stdout,total)
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=2)
      call set_real_style_(stdout,"f")

   end subroutine

!*******************************************************************************
!                      Thermal parameter fitting
!*******************************************************************************

   subroutine force_thermal_symmetry(self)
    type(mol_type) :: self
    ! Impose crystal symmetry on the thermal tensors.
     intent(inout) :: self
     integer(kind=kind(1)) :: n,u,s,n_unique
     real(kind=kind(1.0d0)), dimension(:,:,:), pointer :: unique_thermals,seitz
     real(kind=kind(1.0d0)), dimension(3,3) :: thermal,seitz_inv
     integer(kind=kind(1)), dimension(:), pointer :: n_equiv

   call ensure_(tonto,associated(self%crystal),"MOL:force_thermal_symmetry ... crystal not created")
   call ensure_(tonto,self%n_atom==self%crystal%n_fragment_atoms,"MOL:force_thermal_symmetry ... incosistent crystal fragment&
&")
     n_unique = self%crystal%n_unique_fragment_atoms
     seitz => self%crystal%spacegroup%seitz
     call create_(unique_thermals,3,3,n_unique)
     unique_thermals = 0.0d0
     call create_(n_equiv,n_unique)
     n_equiv = 0
      ! change to crystal coordinates.
     do n=1,self%n_atom
       u = self%crystal%unique_atom_for_fragment_atom(n)
       s = self%crystal%unique_symop_for_fragment_atom(n)
       call to_inverse_of_(seitz_inv,transpose(seitz(1:3,1:3,s)))
       thermal = self%atom(n)%thermal_tensor
       call change_basis_(thermal,self%crystal%unitcell%reciprocal_U_matrix)
       call change_basis_(thermal,seitz_inv)
       unique_thermals(:,:,u) = unique_thermals(:,:,u) + thermal
       n_equiv(u) = n_equiv(u) + 1
     end do
      ! Average them.
     do u=1,n_unique
       unique_thermals(:,:,u) = unique_thermals(:,:,u) / n_equiv(u)
     end do
      ! change back to cartesians.
     do n=1,self%n_atom
       u = self%crystal%unique_atom_for_fragment_atom(n)
       s = self%crystal%unique_symop_for_fragment_atom(n)
       thermal = unique_thermals(:,:,u)
       call change_basis_(thermal,transpose(seitz(1:3,1:3,s)))
       call change_basis_(thermal,self%crystal%unitcell%direct_U_matrix)
       self%atom(n)%thermal_tensor = thermal
     end do
     call destroy_(n_equiv)
     call destroy_(unique_thermals)

   end subroutine

   subroutine fit_thermal_parameters(self)
    type(mol_type) :: self
    ! Fit the thermal parameters to the calculated structure factors.
    ! Uses a line search method.
     integer(kind=kind(1)) :: dim
     real(kind=kind(1.0d0)) :: chi2_min
     real(kind=kind(1.0d0)), dimension(:), pointer :: U
     pointer :: self

     call ensure_(tonto,associated(self%crystal),"MOL:fit_thermal_parameters ... no crystal")
     call ensure_(tonto,associated(self%atom),"MOL:fit_thermal_parameters ... no atoms to fit")
     dim = self%n_atom * 6
     call create_(U,dim)
     call get_thermal_parameters_(self,U)
  !    .crystal.optimise_extinction = .false.
  !    .crystal.optimise_scale = .false.
     call put_thermal_tensors_(self%atom)
     saved_self => self
     call minimise_BFGS(sfchi2,d_sfchi2_d_thermal,U,chi2_min,tol=10.0d0**(-7),gtol=10.0d0**(-7),step=10.0d0**(-4))
     call destroy_(U)
     call put_atom_thermal_tensors_(self)

   end subroutine

   subroutine get_thermal_parameters(self,U)
    type(mol_type) :: self
    ! Set a new set of thermal parameters for all the atoms, assuming
    ! "U" is a vector of the independent thermal parameters.
      real(kind=kind(1.0d0)), dimension(:) :: U
      integer(kind=kind(1)) :: n,base

      call ensure_(tonto,size(U)==6*self%n_atom,"MOL:get_thermal_parameters ... wrong size, U")
      do n = 1,self%n_atom       ! get thermal parameters into array
        base = (n-1) * 6
        U(base+1) = self%atom(n)%thermal_tensor(1,1)
        U(base+2) = self%atom(n)%thermal_tensor(1,2)
        U(base+3) = self%atom(n)%thermal_tensor(1,3)
        U(base+4) = self%atom(n)%thermal_tensor(2,2)
        U(base+5) = self%atom(n)%thermal_tensor(2,3)
        U(base+6) = self%atom(n)%thermal_tensor(3,3)
      end do

   end subroutine

   subroutine set_thermal_parameters(self,U)
    type(mol_type) :: self
    ! Set a new set of thermal parameters for all the atoms, assuming
    ! "U" is a vector of the independent thermal parameters.
      real(kind=kind(1.0d0)), dimension(:) :: U
      integer(kind=kind(1)) :: n,base

      call ensure_(tonto,size(U)==6*self%n_atom,"MOL:set_thermal_parameters ... wrong size, U")
      do n = 1,self%n_atom       ! get thermal parameters into array
        base = (n-1) * 6
        self%atom(n)%thermal_tensor(1,1) = U(base+1)
        self%atom(n)%thermal_tensor(1,2) = U(base+2)
        self%atom(n)%thermal_tensor(1,3) = U(base+3)
        self%atom(n)%thermal_tensor(2,2) = U(base+4)
        self%atom(n)%thermal_tensor(2,3) = U(base+5)
        self%atom(n)%thermal_tensor(3,3) = U(base+6)
        self%atom(n)%thermal_tensor(2,1) = self%atom(n)%thermal_tensor(1,2)
        self%atom(n)%thermal_tensor(3,1) = self%atom(n)%thermal_tensor(1,3)
        self%atom(n)%thermal_tensor(3,2) = self%atom(n)%thermal_tensor(2,3)
      end do
      call force_thermal_symmetry_(self)

   end subroutine

   function sfchi2(U) result(res)
    ! Evaluate the structure factor chi2.
     real(kind=kind(1.0d0)), dimension(:) :: U
     real(kind=kind(1.0d0)) :: res
     type(mol_type), pointer :: self

     self => saved_self
     call ensure_(tonto,associated(self%crystal),"MOL:sfchi2 ... no crystal")
     call set_thermal_parameters_(self,U)
     call make_structure_factors_(self)
     res = F_chi2_(self%crystal%reflections)
     call show_(stdout,"new F_chi2 =",res)

   end function

   function d_sfchi2_d_thermal(U) result(res)
    ! Evaluate the gradient of the structure factor chi2 with respect to the
    ! unique thermal parameters.
    ! Size of res is [n_atom * 6].
     real(kind=kind(1.0d0)), dimension(:) :: U
     real(kind=kind(1.0d0)), dimension(size(U)) :: res
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: sf_deriv,sf_deriv_eq
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: k_pts
     character(128) :: orb_kind
     logical(kind=kind(.true.)) :: complex
     type(mol_type), pointer :: self

     self => saved_self
     call ensure_(tonto,associated(self%crystal),"MOL:d_sfchi2_d_thermal ... no crystal")
     call ensure_(tonto,associated(self%density_matrix),"MOL:d_sfchi2_d_thermal ... no density matrix")
     call set_thermal_parameters_(self,U)
     call create_(k_pts,n_unique_SF_k_pts_(self%crystal),3)
     call make_unique_SF_k_pts_(self%crystal,k_pts)
     call create_(sf_deriv, n_refl_(self%crystal), self%n_atom * 6)  ! d_Fc/d_U
     call create_(sf_deriv_eq,self%n_atom * 6,n_unique_SF_k_pts_(self%crystal))
     orb_kind = spinorbital_kind_(self%density_matrix)
     complex = includes_(orb_kind,"complex")
     if (complex) then
       call make_ft_deriv_U_(self,sf_deriv_eq,self%density_matrix%restricted_complex,k_pts)
     else
       call make_ft_deriv_U_(self,sf_deriv_eq,self%density_matrix%restricted,k_pts)
     end if
     call sum_unique_sf_deriv_U_(self%crystal,sf_deriv,sf_deriv_eq)
     call destroy_(sf_deriv_eq)
      ! Do the derivative with respect to chi2
     res = d_chi2_dU_(self%crystal,sf_deriv)
     call destroy_(sf_deriv)
     call destroy_(k_pts)

   end function

end
