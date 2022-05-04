!---------------------------------------------------------------------------
!
! ATOMVEC: ATOM vectors
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
! $Id: atomvec.foo,v 1.63.2.21 2003/11/13 05:34:14 reaper Exp $
!---------------------------------------------------------------------------

module ATOMVEC_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

   use SLATERBASIS_MODULE, only: make_interpolator_

   use CIF_MODULE, only: open_
   use CIF_MODULE, only: read_looped_item_
   use CIF_MODULE, only: create_
   use CIF_MODULE, only: read_looped_items_
   use CIF_MODULE, only: find_looped_item_
   use CIF_MODULE, only: destroy_
   use CIF_MODULE, only: find_crystal_data_block_

   use SHELL2_MODULE, only: destroy_ptr_part_
   use SHELL2_MODULE, only: get_nuc_
   use SHELL2_MODULE, only: copy_

   use INTVEC_MODULE, only: index_of_value_
   use INTVEC_MODULE, only: create_
   use INTVEC_MODULE, only: destroy_

   use REAL_MODULE, only: arccos_
   use REAL_MODULE, only: is_zero_
   use REAL_MODULE, only: to_str_

   use REALVEC_MODULE, only: same_as_
   use REALVEC_MODULE, only: dot_
   use REALVEC_MODULE, only: norm_
   use REALVEC_MODULE, only: normalise_
   use REALVEC_MODULE, only: swap_elements_
   use REALVEC_MODULE, only: to_cross_product_
   use REALVEC_MODULE, only: to_str_

   use INT_MODULE, only: to_str_

   use ATOM_MODULE, only: read_keys_
   use ATOM_MODULE, only: nullify_ptr_part_
   use ATOM_MODULE, only: no_of_occupied_NOs_
   use ATOM_MODULE, only: put_thermal_tensor_
   use ATOM_MODULE, only: resolve_axis_system_
   use ATOM_MODULE, only: put_table_header_
   use ATOM_MODULE, only: destroy_ptr_part_
   use ATOM_MODULE, only: change_axis_system_to_
   use ATOM_MODULE, only: no_of_shells_
   use ATOM_MODULE, only: process_keys_
   use ATOM_MODULE, only: set_label_and_atomic_number_
   use ATOM_MODULE, only: thermal_tensor_to_
   use ATOM_MODULE, only: put_table_footer_
   use ATOM_MODULE, only: has_ANO_data_
   use ATOM_MODULE, only: change_thermal_axis_system_to_
   use ATOM_MODULE, only: copy_
   use ATOM_MODULE, only: ground_state_multiplicity_
   use ATOM_MODULE, only: set_defaults_
   use ATOM_MODULE, only: clear_keys_
   use ATOM_MODULE, only: chemical_symbol_
   use ATOM_MODULE, only: set_coppensbasis_label_
   use ATOM_MODULE, only: create_
   use ATOM_MODULE, only: n_shell_
   use ATOM_MODULE, only: mean_neutron_number_
   use ATOM_MODULE, only: keys_created_
   use ATOM_MODULE, only: same_kind_as_
   use ATOM_MODULE, only: set_keys_
   use ATOM_MODULE, only: no_of_primitives_
   use ATOM_MODULE, only: n_bf_
   use ATOM_MODULE, only: bragg_slater_radius_
   use ATOM_MODULE, only: thermal_tensor_from_
   use ATOM_MODULE, only: destroy_
   use ATOM_MODULE, only: no_of_basis_functions_
   use ATOM_MODULE, only: library_basis_label_
   use ATOM_MODULE, only: n_prim_
   use ATOM_MODULE, only: resolve_basis_
   use ATOM_MODULE, only: mass_

   use STR_MODULE, only: is_int_
   use STR_MODULE, only: to_lower_case_
   use STR_MODULE, only: to_int_
   use STR_MODULE, only: includes_

   use TEXTFILE_MODULE, only: stdin
   use TEXTFILE_MODULE, only: stdout
   use TEXTFILE_MODULE, only: redirect_
   use TEXTFILE_MODULE, only: show_
   use TEXTFILE_MODULE, only: set_real_width_
   use TEXTFILE_MODULE, only: read_
   use TEXTFILE_MODULE, only: at_end_of_file_
   use TEXTFILE_MODULE, only: set_int_width_
   use TEXTFILE_MODULE, only: flush_
   use TEXTFILE_MODULE, only: save_
   use TEXTFILE_MODULE, only: dash_
   use TEXTFILE_MODULE, only: set_real_precision_
   use TEXTFILE_MODULE, only: next_str_
   use TEXTFILE_MODULE, only: text_
   use TEXTFILE_MODULE, only: previous_line_item_
   use TEXTFILE_MODULE, only: line_number_
   use TEXTFILE_MODULE, only: next_item_
   use TEXTFILE_MODULE, only: move_to_line_
   use TEXTFILE_MODULE, only: unsave_
   use TEXTFILE_MODULE, only: put_
   use TEXTFILE_MODULE, only: revert_
   use TEXTFILE_MODULE, only: move_to_line_item_
   use TEXTFILE_MODULE, only: move_to_previous_item_
   use TEXTFILE_MODULE, only: reverted_

   use INTVECVEC_MODULE, only: create_
   use INTVECVEC_MODULE, only: destroy_

   use STRVEC_MODULE, only: create_
   use STRVEC_MODULE, only: quick_sort_
   use STRVEC_MODULE, only: remove_repetitions_
   use STRVEC_MODULE, only: index_of_
   use STRVEC_MODULE, only: destroy_

   use COPPENSBASIS_MODULE, only: make_interpolator_

   use REALMAT_MODULE, only: swap_columns_
   use REALMAT_MODULE, only: solve_eigenproblem_
   use REALMAT_MODULE, only: determinant_
   use REALMAT_MODULE, only: create_
   use REALMAT_MODULE, only: add_to_diagonal_
   use REALMAT_MODULE, only: is_square_
   use REALMAT_MODULE, only: trace_
   use REALMAT_MODULE, only: change_basis_
   use REALMAT_MODULE, only: symmetric_reflect_
   use REALMAT_MODULE, only: destroy_
   use REALMAT_MODULE, only: to_unit_matrix_

   use INTERPOLATOR_MODULE, only: destroy_
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

   public    read_keys_
   interface read_keys_
      module procedure read_keys
   end interface

   public    convert_from_angstrom_
   interface convert_from_angstrom_
      module procedure convert_from_angstrom
   end interface

   public    same_as_
   interface same_as_
      module procedure same_as
   end interface

   public    seitz_multiply_
   interface seitz_multiply_
      module procedure seitz_multiply
   end interface

   public    make_unique_atom_list_
   interface make_unique_atom_list_
      module procedure make_unique_atom_list
      module procedure make_unique_atom_list_1
   end interface

   public    resolve_axis_system_
   interface resolve_axis_system_
      module procedure resolve_axis_system
   end interface

   public    put_table_header_
   interface put_table_header_
      module procedure put_table_header
   end interface

   public    read_keywords_
   interface read_keywords_
      module procedure read_keywords
   end interface

   public    data_length_
   interface data_length_
      module procedure data_length
   end interface

   public    slater_interpolators_exist_
   interface slater_interpolators_exist_
      module procedure slater_interpolators_exist
   end interface

   public    same_kind_of_atoms_
   interface same_kind_of_atoms_
      module procedure same_kind_of_atoms
   end interface

   public    set_coppensbasis_labels_
   interface set_coppensbasis_labels_
      module procedure set_coppensbasis_labels
   end interface

   public    slaterbases_are_all_unlabeled_
   interface slaterbases_are_all_unlabeled_
      module procedure slaterbases_are_all_unlabeled
   end interface

   private    put_coords_without_basis_label_
   interface put_coords_without_basis_label_
      module procedure put_coords_without_basis_label
   end interface

   public    are_nearby_
   interface are_nearby_
      module procedure are_nearby
   end interface

   public    destroy_coppens_interpolators_
   interface destroy_coppens_interpolators_
      module procedure destroy_coppens_interpolators
   end interface

   public    put_vrml_
   interface put_vrml_
      module procedure put_vrml
   end interface

   public    bases_all_exist_
   interface bases_all_exist_
      module procedure bases_all_exist
   end interface

   public    copy_
   interface copy_
      module procedure copy
   end interface

   public    put_thermal_tensors_
   interface put_thermal_tensors_
      module procedure put_thermal_tensors
   end interface

   public    clear_keys_
   interface clear_keys_
      module procedure clear_keys
   end interface

   public    atom_index_from_pos_
   interface atom_index_from_pos_
      module procedure atom_index_from_pos
   end interface

   public    nuclear_energy_
   interface nuclear_energy_
      module procedure nuclear_energy
      module procedure nuclear_energy_1
      module procedure nuclear_energy_2
   end interface

   public    nuclear_E_field_at_nuclei_
   interface nuclear_E_field_at_nuclei_
      module procedure nuclear_E_field_at_nuclei
   end interface

   public    rotate_thermal_
   interface rotate_thermal_
      module procedure rotate_thermal
   end interface

   public    keys_created_
   interface keys_created_
      module procedure keys_created
   end interface

   public    get_shell_limits_
   interface get_shell_limits_
      module procedure get_shell_limits
      module procedure get_shell_limits_1
   end interface

   public    thermal_tensor_from_
   interface thermal_tensor_from_
      module procedure thermal_tensor_from
   end interface

   public    destroy_
   interface destroy_
      module procedure destroy
   end interface

   public    no_of_bonds_
   interface no_of_bonds_
      module procedure no_of_bonds
   end interface

   public    nullify_slaterbasis_part_
   interface nullify_slaterbasis_part_
      module procedure nullify_slaterbasis_part
   end interface

   public    bases_are_resolved_
   interface bases_are_resolved_
      module procedure bases_are_resolved
   end interface

   public    torsion_angle_
   interface torsion_angle_
      module procedure torsion_angle
   end interface

   public    atom_shell_for_shell_
   interface atom_shell_for_shell_
      module procedure atom_shell_for_shell
      module procedure atom_shell_for_shell_1
   end interface

   public    destroy_slater_interpolators_
   interface destroy_slater_interpolators_
      module procedure destroy_slater_interpolators
   end interface

   public    read_CIF_
   interface read_CIF_
      module procedure read_CIF
      module procedure read_CIF_1
   end interface

   public    make_atom_basis_fn_limits_
   interface make_atom_basis_fn_limits_
      module procedure make_atom_basis_fn_limits
   end interface

   public    molecular_weight_
   interface molecular_weight_
      module procedure molecular_weight
   end interface

   private    put_vrml_header_
   interface put_vrml_header_
      module procedure put_vrml_header
   end interface

   public    destroy_ptr_part_
   interface destroy_ptr_part_
      module procedure destroy_ptr_part
   end interface

   public    centre_of_mass_
   interface centre_of_mass_
      module procedure centre_of_mass
   end interface

   public    nuclear_EFG_at_nuclei_
   interface nuclear_EFG_at_nuclei_
      module procedure nuclear_EFG_at_nuclei
   end interface

   private    is_linear_
   interface is_linear_
      module procedure is_linear
   end interface

   public    chemical_formula_
   interface chemical_formula_
      module procedure chemical_formula
   end interface

   public    make_slater_interpolators_
   interface make_slater_interpolators_
      module procedure make_slater_interpolators
   end interface

   public    numbered_chemical_symbols_
   interface numbered_chemical_symbols_
      module procedure numbered_chemical_symbols
   end interface

   public    connected_
   interface connected_
      module procedure connected
   end interface

   public    has_all_ANO_data_
   interface has_all_ANO_data_
      module procedure has_all_ANO_data
   end interface

   public    slaterbases_are_resolved_
   interface slaterbases_are_resolved_
      module procedure slaterbases_are_resolved
   end interface

   public    coppensbases_are_resolved_
   interface coppensbases_are_resolved_
      module procedure coppensbases_are_resolved
   end interface

   public    shrink_
   interface shrink_
      module procedure shrink
   end interface

   public    put_
   interface put_
      module procedure put
   end interface

   public    reduced_mass_
   interface reduced_mass_
      module procedure reduced_mass
   end interface

   public    nullify_basis_part_
   interface nullify_basis_part_
      module procedure nullify_basis_part
   end interface

   public    set_labels_and_atomic_numbers_
   interface set_labels_and_atomic_numbers_
      module procedure set_labels_and_atomic_numbers
   end interface

   public    get_coordinates_
   interface get_coordinates_
      module procedure get_coordinates
   end interface

   public    read_data_
   interface read_data_
      module procedure read_data
   end interface

   public    change_thermal_axis_system_to_
   interface change_thermal_axis_system_to_
      module procedure change_thermal_axis_system_to
   end interface

   public    bond_distance_
   interface bond_distance_
      module procedure bond_distance
   end interface

   private    is_prolate_top_
   interface is_prolate_top_
      module procedure is_prolate_top
   end interface

   private    get_shell_pair_
   interface get_shell_pair_
      module procedure get_shell_pair
      module procedure get_shell_pair_1
   end interface

   public    create_
   interface create_
      module procedure create
   end interface

   public    nuclear_dipole_moment_
   interface nuclear_dipole_moment_
      module procedure nuclear_dipole_moment
   end interface

   private    put_vrml_bonds_
   interface put_vrml_bonds_
      module procedure put_vrml_bonds
   end interface

   public    create_copy_
   interface create_copy_
      module procedure create_copy
   end interface

   public    make_nuclear_matrix_
   interface make_nuclear_matrix_
      module procedure make_nuclear_matrix
      module procedure make_nuclear_matrix_1
   end interface

   public    ensure_in_unitcell_
   interface ensure_in_unitcell_
      module procedure ensure_in_unitcell
   end interface

   public    nuclear_octupole_moment_
   interface nuclear_octupole_moment_
      module procedure nuclear_octupole_moment
   end interface

   public    set_keys_
   interface set_keys_
      module procedure set_keys
   end interface

   public    no_of_primitives_
   interface no_of_primitives_
      module procedure no_of_primitives
   end interface

   public    n_bf_
   interface n_bf_
      module procedure n_bf
   end interface

   public    nullify_coppensbasis_part_
   interface nullify_coppensbasis_part_
      module procedure nullify_coppensbasis_part
   end interface

   private    put_restraint_atoms_
   interface put_restraint_atoms_
      module procedure put_restraint_atoms
   end interface

   public    no_of_same_principal_moments_
   interface no_of_same_principal_moments_
      module procedure no_of_same_principal_moments
   end interface

   public    library_basis_labels_
   interface library_basis_labels_
      module procedure library_basis_labels
   end interface

   public    bases_are_all_unlabeled_
   interface bases_are_all_unlabeled_
      module procedure bases_are_all_unlabeled
   end interface

   public    no_of_basis_functions_
   interface no_of_basis_functions_
      module procedure no_of_basis_functions
   end interface

   public    resolve_bases_
   interface resolve_bases_
      module procedure resolve_bases
      module procedure resolve_bases_1
      module procedure resolve_bases_2
   end interface

   public    coppensbases_are_part_labeled_
   interface coppensbases_are_part_labeled_
      module procedure coppensbases_are_part_labeled
   end interface

   public    coppensbases_all_exist_
   interface coppensbases_all_exist_
      module procedure coppensbases_all_exist
   end interface

   public    process_list_keyword_
   interface process_list_keyword_
      module procedure process_list_keyword
   end interface

   public    put_coord_info_
   interface put_coord_info_
      module procedure put_coord_info
   end interface

   public    nullify_ptr_part_
   interface nullify_ptr_part_
      module procedure nullify_ptr_part
   end interface

   private    put_coords_with_basis_label_
   interface put_coords_with_basis_label_
      module procedure put_coords_with_basis_label
   end interface

   public    slaterbases_are_part_labeled_
   interface slaterbases_are_part_labeled_
      module procedure slaterbases_are_part_labeled
   end interface

   public    centre_of_atoms_
   interface centre_of_atoms_
      module procedure centre_of_atoms
   end interface

   public    has_residue_names_
   interface has_residue_names_
      module procedure has_residue_names
   end interface

   public    nullify_bases_
   interface nullify_bases_
      module procedure nullify_bases
   end interface

   public    get_geometry_vector_
   interface get_geometry_vector_
      module procedure get_geometry_vector
   end interface

   private    put_vrml_atoms_
   interface put_vrml_atoms_
      module procedure put_vrml_atoms
   end interface

   public    rotate_
   interface rotate_
      module procedure rotate
   end interface

   public    bounding_cube_width_
   interface bounding_cube_width_
      module procedure bounding_cube_width
   end interface

   public    convert_to_crystal_
   interface convert_to_crystal_
      module procedure convert_to_crystal
   end interface

   public    make_shape_tensor_
   interface make_shape_tensor_
      module procedure make_shape_tensor
   end interface

   public    bond_angle_
   interface bond_angle_
      module procedure bond_angle
   end interface

   public    coppensbases_are_all_unlabeled_
   interface coppensbases_are_all_unlabeled_
      module procedure coppensbases_are_all_unlabeled
   end interface

   public    put_keys_table_
   interface put_keys_table_
      module procedure put_keys_table
   end interface

   public    make_atom_kind_map_
   interface make_atom_kind_map_
      module procedure make_atom_kind_map
   end interface

   public    atom_for_shell_
   interface atom_for_shell_
      module procedure atom_for_shell
      module procedure atom_for_shell_1
   end interface

   private    put_bond_angle_table_
   interface put_bond_angle_table_
      module procedure put_bond_angle_table
   end interface

   public    n_e_
   interface n_e_
      module procedure n_e
   end interface

   public    no_of_shells_
   interface no_of_shells_
      module procedure no_of_shells
   end interface

   private    list_symbol_
   interface list_symbol_
      module procedure list_symbol
   end interface

   public    geometry_
   interface geometry_
      module procedure geometry
   end interface

   private    is_oblate_top_
   interface is_oblate_top_
      module procedure is_oblate_top
   end interface

   private    put_bond_length_table_
   interface put_bond_length_table_
      module procedure put_bond_length_table
   end interface

   public    convert_to_angstrom_
   interface convert_to_angstrom_
      module procedure convert_to_angstrom
   end interface

   public    redirect_
   interface redirect_
      module procedure redirect
   end interface

   public    no_of_angles_
   interface no_of_angles_
      module procedure no_of_angles
   end interface

   public    n_shell_
   interface n_shell_
      module procedure n_shell
   end interface

   public    groups_defined_
   interface groups_defined_
      module procedure groups_defined
   end interface

   public    make_index_info_
   interface make_index_info_
      module procedure make_index_info
   end interface

   public    nuclear_quadrupole_moment_
   interface nuclear_quadrupole_moment_
      module procedure nuclear_quadrupole_moment
   end interface

   public    n_prim_
   interface n_prim_
      module procedure n_prim
   end interface

   public    revert_
   interface revert_
      module procedure revert
   end interface

   public    read_altered_data_
   interface read_altered_data_
      module procedure read_altered_data
   end interface

   public    bonded_
   interface bonded_
      module procedure bonded
   end interface

   public    make_coppens_interpolators_
   interface make_coppens_interpolators_
      module procedure make_coppens_interpolators
   end interface

   private    put_mm_info_
   interface put_mm_info_
      module procedure put_mm_info
   end interface

   public    sum_of_atomic_numbers_
   interface sum_of_atomic_numbers_
      module procedure sum_of_atomic_numbers
   end interface

   public    make_principal_moments_
   interface make_principal_moments_
      module procedure make_principal_moments
   end interface

   public    bounding_box_
   interface bounding_box_
      module procedure bounding_box
   end interface

   public    chemical_symbols_
   interface chemical_symbols_
      module procedure chemical_symbols
   end interface

   public    has_restraints_
   interface has_restraints_
      module procedure has_restraints
   end interface

   public    coppens_interpolators_exist_
   interface coppens_interpolators_exist_
      module procedure coppens_interpolators_exist
   end interface

   private    put_torsion_angle_table_
   interface put_torsion_angle_table_
      module procedure put_torsion_angle_table
   end interface

   public    set_geometry_from_vector_
   interface set_geometry_from_vector_
      module procedure set_geometry_from_vector
   end interface

   public    first_shell_for_atom_
   interface first_shell_for_atom_
      module procedure first_shell_for_atom
      module procedure first_shell_for_atom_1
   end interface

   private    is_asymmetric_top_
   interface is_asymmetric_top_
      module procedure is_asymmetric_top
   end interface

   public    slaterbases_are_all_labeled_
   interface slaterbases_are_all_labeled_
      module procedure slaterbases_are_all_labeled
   end interface

   public    n_atom_
   interface n_atom_
      module procedure n_atom
   end interface

   public    n_shell_for_atom_
   interface n_shell_for_atom_
      module procedure n_shell_for_atom
   end interface

   public    coppensbases_are_all_labeled_
   interface coppensbases_are_all_labeled_
      module procedure coppensbases_are_all_labeled
   end interface

   public    get_geometry_
   interface get_geometry_
      module procedure get_geometry
   end interface

   public    change_axis_system_to_
   interface change_axis_system_to_
      module procedure change_axis_system_to
   end interface

   public    process_keyword_
   interface process_keyword_
      module procedure process_keyword
   end interface

   public    geometry_vector_
   interface geometry_vector_
      module procedure geometry_vector
   end interface

   public    move_origin_to_centre_of_mass_
   interface move_origin_to_centre_of_mass_
      module procedure move_origin_to_centre_of_mass
   end interface

   public    process_keys_
   interface process_keys_
      module procedure process_keys
   end interface

   public    make_shape_moments_
   interface make_shape_moments_
      module procedure make_shape_moments
   end interface

   public    thermal_tensor_to_
   interface thermal_tensor_to_
      module procedure thermal_tensor_to
   end interface

   public    put_table_footer_
   interface put_table_footer_
      module procedure put_table_footer
   end interface

   public    get_mean_neutron_numbers_
   interface get_mean_neutron_numbers_
      module procedure get_mean_neutron_numbers
   end interface

   public    translate_
   interface translate_
      module procedure translate
   end interface

   public    convert_from_crystal_
   interface convert_from_crystal_
      module procedure convert_from_crystal
   end interface

   public    read_list_keywords_
   interface read_list_keywords_
      module procedure read_list_keywords
   end interface

   public    has_sequence_numbers_
   interface has_sequence_numbers_
      module procedure has_sequence_numbers
   end interface

   public    slaterbases_all_exist_
   interface slaterbases_all_exist_
      module procedure slaterbases_all_exist
   end interface

   public    atomic_numbers_
   interface atomic_numbers_
      module procedure atomic_numbers
   end interface

   public    set_defaults_
   interface set_defaults_
      module procedure set_defaults
   end interface

   public    default_multiplicity_
   interface default_multiplicity_
      module procedure default_multiplicity
   end interface

   private    put_coordinates_
   interface put_coordinates_
      module procedure put_coordinates
   end interface

   public    destroy_index_info_
   interface destroy_index_info_
      module procedure destroy_index_info
   end interface

   public    no_of_occupied_ANOs_
   interface no_of_occupied_ANOs_
      module procedure no_of_occupied_ANOs
   end interface

   public    make_inertia_tensor_
   interface make_inertia_tensor_
      module procedure make_inertia_tensor
   end interface

   public    bases_are_all_labeled_
   interface bases_are_all_labeled_
      module procedure bases_are_all_labeled
   end interface

   public    make_shell_for_atom_limits_
   interface make_shell_for_atom_limits_
      module procedure make_shell_for_atom_limits
   end interface

   public    no_of_torsion_angles_
   interface no_of_torsion_angles_
      module procedure no_of_torsion_angles
   end interface

   public    make_atom_kind_list_
   interface make_atom_kind_list_
      module procedure make_atom_kind_list
      module procedure make_atom_kind_list_1
   end interface

   public    bases_are_part_labeled_
   interface bases_are_part_labeled_
      module procedure bases_are_part_labeled
   end interface

   public    get_distance_from_
   interface get_distance_from_
      module procedure get_distance_from
      module procedure get_distance_from_1
   end interface

   public    n_shell_pairs_
   interface n_shell_pairs_
      module procedure n_shell_pairs
   end interface

   public    basis_labels_
   interface basis_labels_
      module procedure basis_labels
   end interface

   private    is_spherical_top_
   interface is_spherical_top_
      module procedure is_spherical_top
   end interface

   public    make_atom_kind_count_
   interface make_atom_kind_count_
      module procedure make_atom_kind_count
   end interface

   private    is_symmetric_top_
   interface is_symmetric_top_
      module procedure is_symmetric_top
   end interface

    ! Index information arrays; the strange "4" stands for "for" and prevents a
    ! name clash with a procedure of the same name.

   logical(kind=kind(.true.)), private :: index_info_created = .false.
   integer(kind=kind(1)), dimension(:), pointer, private :: atom_4_shell => NULL()
   integer(kind=kind(1)), dimension(:), pointer, private :: atom_shell_4_shell => NULL()
   integer(kind=kind(1)), dimension(:), pointer, private :: first_shell_4_atom => NULL()
   integer(kind=kind(1)), dimension(:), pointer, private :: first_basis_fn_4_shell => NULL()
    integer(kind=kind(1)), dimension(:), pointer, private :: last_basis_fn_4_shell => NULL()
   integer(kind=kind(1)), dimension(:), pointer, private :: first_basis_fn_4_atom => NULL()
    integer(kind=kind(1)), dimension(:), pointer, private :: last_basis_fn_4_atom => NULL()

contains

!  ******************
!  Allocation methods
!  ******************

   subroutine create(self,dim)
    type(atom_type), dimension(:) :: self
    ! Create space for object
      pointer :: self
      integer(kind=kind(1)) :: dim

      nullify(self)
      allocate(self(dim))

      call nullify_ptr_part_(self)
      call set_defaults_(self)

   end subroutine

   subroutine destroy(self)
    type(atom_type), dimension(:) :: self
    ! Destroy space for object
      pointer :: self

      if (.not. associated(self)) then;   return; end if
      call destroy_ptr_part_(self)

      deallocate(self)

   end subroutine

   subroutine create_copy(self,vec)
    type(atom_type), dimension(:) :: self
    ! Create a replica copy of "vec".
      type(atom_type), dimension(:), intent(in) :: vec
      pointer :: self

      call create_(self,size(vec))
      call copy_(self,vec)

   end subroutine

   subroutine copy(self,vec)
    type(atom_type), dimension(:) :: self
    ! Copy "vec". Pointer parts are replicated.
      type(atom_type), dimension(:), intent(in) :: vec
      integer(kind=kind(1)) :: a

      call ensure_(tonto,size(self)==size(vec),"ATOMVEC:copy ... vec size does not match")
      do a = 1,size(vec)
        call copy_(self(a),vec(a))
      end do

   end subroutine

   subroutine nullify_ptr_part(self)
    type(atom_type), dimension(:) :: self
    ! Nullify the pointer parts of self
      integer(kind=kind(1)) :: a

      do a = 1,size(self)
         call nullify_ptr_part_(self(a))
      end do

   end subroutine

   subroutine nullify_bases(self)
    type(atom_type), dimension(:) :: self
    ! Nullify the bases

      call nullify_basis_part_(self)
      call nullify_slaterbasis_part_(self)
      call nullify_coppensbasis_part_(self)

   end subroutine

   subroutine nullify_basis_part(self)
    type(atom_type), dimension(:) :: self
    ! Nullify the basis parts of self
      integer(kind=kind(1)) :: a

      do a = 1,size(self)
         nullify(self(a)%basis)
      end do

   end subroutine

   subroutine nullify_slaterbasis_part(self)
    type(atom_type), dimension(:) :: self
    ! Nullify the slaterbasis parts of self
      integer(kind=kind(1)) :: a

      do a = 1,size(self)
         nullify(self(a)%slaterbasis)
      end do

   end subroutine

   subroutine nullify_coppensbasis_part(self)
    type(atom_type), dimension(:) :: self
    ! Nullify the coppensbasis parts of self
      integer(kind=kind(1)) :: a

      do a = 1,size(self)
         nullify(self(a)%coppensbasis)
      end do

   end subroutine

   subroutine destroy_ptr_part(self)
    type(atom_type), dimension(:) :: self
    ! Destroy the pointer parts of self
      integer(kind=kind(1)) :: a
       ! avoid double destroying

      if (coppens_interpolators_exist_(self)) call destroy_coppens_interpolators_(self)
      if (slater_interpolators_exist_(self))  call destroy_slater_interpolators_(self)
      call nullify_bases_(self)
      do a = 1,size(self)  ! Now we can safely destroy everything .....
         call destroy_ptr_part_(self(a))
      end do

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

   subroutine set_defaults(self)
    type(atom_type), dimension(:) :: self
    ! Set default values
       integer(kind=kind(1)) :: n

      do n = 1,size(self)
        call set_defaults_(self(n))
      end do

   end subroutine

   subroutine set_labels_and_atomic_numbers(self,labels)
    type(atom_type), dimension(:) :: self
    ! Set "labels" for the atoms and also their atomic numbers.
      character(len=*), dimension(:) :: labels
      integer(kind=kind(1)) :: n

   call ensure_(tonto,size(labels)==size(self),"ATOMVEC:set_labels_and_atomic_numbers ... wrong length for labels")
      do n = 1,n_atom_(self)
        call set_label_and_atomic_number_(self(n),labels(n))
      end do

   end subroutine

   subroutine set_coppensbasis_labels(self,labels)
    type(atom_type), dimension(:) :: self
    ! Set the .coppensbasis "labels".
      character(len=*), dimension(:) :: labels
      integer(kind=kind(1)) :: n

   call ensure_(tonto,size(labels)==size(self),"ATOMVEC:set_coppensbasis_labels ... wrong length for labels")
      do n = 1,n_atom_(self)
        call set_coppensbasis_label_(self(n),labels(n))
      end do

   end subroutine

   subroutine shrink(self,dim)
    type(atom_type), dimension(:) :: self
    ! Shrink the atomvec to dimension "dim", retaining contents.
     pointer :: self
     integer(kind=kind(1)), intent(in) :: dim
     type(atom_type), dimension(:), pointer :: old
     integer(kind=kind(1)) :: n

     call ensure_(tonto,associated(self),"ATOMVEC:shrink ... no self array")
     call ensure_(tonto,dim<=size(self),"ATOMVEC:shrink ... dim too large")
     if (dim==size(self)) then;   return; end if
     old => self
     nullify(self)
     call create_(self,dim)
     do n=1,dim
       call copy_(self(n),old(n))
     end do
     call nullify_basis_part_(old)
     call destroy_(old)

   end subroutine

! ***********************
! List-based I/O Routines
! ***********************

   recursive subroutine read_list_keywords(self)
    type(atom_type), dimension(:) :: self
    ! Read in and process list-based keywords from "stdin". List-based keywords
    ! are those that are intended to apply to each individual element of the list
    ! through a list of "keys" stored in the associated list-element type module.
    ! NOTE: this routine will create the list, if required.
     pointer :: self
     character(128) :: word

     call ensure_(tonto,next_item_(stdin)=="{","ATOMVEC:read_list_keywords ... expecting open bracket symbol, {")
     call read_(stdin,word)
     do                   ! Loop over input list-type keywords
       call read_(stdin,word)
       if (word=="}")      exit
       if (reverted_(stdin)) exit
       call process_list_keyword_(self,word)
     end do

   end subroutine

   subroutine process_list_keyword(self,keyword)
    type(atom_type), dimension(:) :: self
    ! Process a list-type "keyword", common to all list-type objects.
     pointer :: self
     character(*), intent(in) :: keyword
     character(128) :: word
     logical(kind=kind(.true.)) :: ignore_braces

     word = keyword
     call to_lower_case_(word)
     select case (word)
       case("altered_data= "); call read_altered_data_(self)
       case("data=         "); call read_data_(self)
       case("do            "); call read_keywords_(self)
       case("keys=         "); call read_keys_(self)
       case("new_data=     "); call destroy_(self); call read_data_(self)
       case("process_keys  "); call process_keys_(self)
       case("put_keys_table"); call put_keys_table_(self)
       case("redirect      "); call redirect_(self)
       case("revert        "); call revert_(self)
       case default;           call move_to_previous_item_(stdin)
                               call read_data_(self,ignore_braces)
     end select

   end subroutine

   subroutine read_data(self,ignore_braces)
    type(atom_type), dimension(:) :: self
    ! Process the keywords list to read data or commands. If "ignore_braces" is
    ! present then the opening and closing braces, which are normally required,
    ! are ignored.
     pointer :: self
     logical(kind=kind(.true.)), optional :: ignore_braces
     character(128) :: word,message
     integer(kind=kind(1)) :: length

     if (.not. present(ignore_braces)) then
        call ensure_(tonto,next_item_(stdin)=="{","ATOMVEC:read_data ... expecting open bracket symbol, {")
        call read_(stdin,word)  ! move past open brace
     end if
     length = data_length_(self)
     if (associated(self)) then
        message = "No. of data items in new and old data lists do not match: "// &
                  "new = "//trim(to_str_(length))//", old = "//trim(to_str_(size(self)))
        call ensure_(tonto,length==size(self),message)
     else
        call create_(self,length)
     end if
     call process_keys_(self)
     if (.not. present(ignore_braces)) then
        call read_(stdin,word)  ! read last brace
        call ensure_(tonto,word=="}","ATOMVEC:read_data ... expecting close bracket symbol, }")
     end if

   end subroutine

   function data_length(self) result(length)
    type(atom_type), dimension(:) :: self
    ! Read ahead in stdin to get the "length" of the data list, i.e. the number
    ! of data items in the list. The data must begin with the first data item,
    ! *not* a "{" symbol.  The order of data items comprising the list is given
    ! by keys defined in the associated list-element type module. The data list
    ! must be terminated by a "}" symbol.
     pointer :: self
     integer(kind=kind(1)) :: length
     type(atom_type), pointer :: tmp
     character(128) :: word
     integer(kind=kind(1)) :: line,item

     call ensure_(tonto,next_item_(stdin)/="}","ATOMVEC:data_length ... empty data list!")
     call read_(stdin,word)
     length = 0
     line = line_number_(stdin)
     item = previous_line_item_(stdin)
     do
       call move_to_previous_item_(stdin)
       call create_(tmp)
       call process_keys_(tmp)
       call destroy_(tmp)
       length = length + 1
       call read_(stdin,word)
       call to_lower_case_(word)
       if (word=="}") exit
       if (at_end_of_file_(stdin)) exit
     end do
     call move_to_line_(stdin,line)
     call move_to_line_item_(stdin,item)

   end function

   subroutine read_altered_data(self)
    type(atom_type), dimension(:) :: self
    ! Read in a sublist of the complete list, and alter the data for that
    ! sublist.  The order of the data items in the sublist is given by the "keys"
    ! defined in the associated list-element type module.
     pointer :: self
     character(128) :: word
     integer(kind=kind(1)) :: s

     call ensure_(tonto,associated(self),"ATOMVEC:read_altered_data ... list does not exist yet")
     call ensure_(tonto,next_item_(stdin)=="{","ATOMVEC:read_altered_data ... expecting open bracket symbol: {")
     call read_(stdin,word)
     read_loop: do
        call read_(stdin,word)
        if (word=="}") exit read_loop
        call ensure_(tonto,is_int_(word),"ATOMVEC:read_altered_data ... expecting integer list-element index")
        s = to_int_(word)
        call ensure_(tonto,s<size(self),"ATOMVEC:read_altered_data ... list-element too large")
        call ensure_(tonto,s>0,"ATOMVEC:read_altered_data ... list-element must be positive")
        call process_keys_(self(s))
     end do read_loop

   end subroutine

   subroutine process_keys(self)
    type(atom_type), dimension(:) :: self
    ! Process the "keys" on each element of the list.
     pointer :: self
     type(atom_type) :: tmp
     integer(kind=kind(1)) :: s

     if (associated(self)) then
        do s = 1,size(self)
           call process_keys_(self(s))
        end do
     else  ! for embedded keywords
        call process_keys_(tmp)
     end if

   end subroutine

   function keys_created(self) result(res)
    type(atom_type), dimension(:) :: self
    ! Return .true. if the list-element keys are created.
      pointer :: self
      logical(kind=kind(.true.)) :: res
      type(atom_type) :: tmp

      res = keys_created_(tmp)

   end function

   subroutine set_keys(self,the_keys)
    type(atom_type), dimension(:) :: self
    ! This is for setting the "keys" externally.
     pointer :: self
     character(len=*), dimension(:) :: the_keys
     type(atom_type) :: tmp

     call set_keys_(tmp,the_keys)

   end subroutine

   subroutine clear_keys(self)
    type(atom_type), dimension(:) :: self
    ! This is for destroying the "keys" externally.
     pointer :: self
     type(atom_type) :: tmp

     call clear_keys_(tmp)

   end subroutine

   subroutine read_keys(self)
    type(atom_type), dimension(:) :: self
    ! Read a new set of keys
      pointer :: self
      type(atom_type) :: tmp

      call read_keys_(tmp)

   end subroutine

   subroutine put_keys_table(self)
    type(atom_type), dimension(:) :: self
    ! Output a generic table based on the "keys"
     pointer :: self

     call ensure_(tonto,keys_created_(self),"ATOMVEC:put_keys_table ... no keys")
     call put_table_header_(self)
     call process_keys_(self)
     call put_table_footer_(self)

   end subroutine

   subroutine put_table_header(self)
    type(atom_type), dimension(:) :: self
    ! Put out a table header based on "keys"
      pointer :: self
      type(atom_type) :: tmp

      call put_table_header_(tmp)

   end subroutine

   subroutine put_table_footer(self)
    type(atom_type), dimension(:) :: self
    ! Put out a table footer based on "keys"
      pointer :: self
      type(atom_type) :: tmp

      call put_table_footer_(tmp)

   end subroutine

   subroutine redirect(self)
    type(atom_type), dimension(:) :: self
    ! Redirect input
     pointer :: self

     call redirect_(stdin,next_str_(stdin))

   end subroutine

   subroutine revert(self)
    type(atom_type), dimension(:) :: self
    ! Revert back to previous stdin file
     pointer :: self

     call revert_(stdin)

   end subroutine

! ***************************
! Non-list based I/O routines
! ***************************

   subroutine read_keywords(self)
    type(atom_type), dimension(:) :: self
    ! Read in and process normal (non list-type) keywords from "stdin".
     pointer :: self
     character(128) :: word

     call ensure_(tonto,next_item_(stdin)=="{","ATOMVEC:read_keywords ... expecting open bracket symbol, {")
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
    type(atom_type), dimension(:) :: self
    ! Process a normal (non list-type) "keyword".
     pointer :: self
     character(128) :: keyword
     character(128) :: word

     word = keyword
     call to_lower_case_(word)
     select case (word)
       case("}")  ! do nothing.
       case("put                    "); call put_(self)
       case("put_bond_angle_table   "); call put_bond_angle_table_(self)
       case("put_bond_length_table  "); call put_bond_length_table_(self)
       case("put_coord_info         "); call put_coord_info_(self)
       case("put_coordinates        "); call put_coordinates_(self)
       case("put_mm_info            "); call put_mm_info_(self)
       case("put_thermal_tensors    "); call put_thermal_tensors_(self)
       case("put_torsion_angle_table"); call put_torsion_angle_table_(self)
       case("put_restraint_atoms    "); call put_restraint_atoms_(self)
       case("read_cif               "); call read_CIF_(self)
       case("redirect               "); call redirect_(self)
       case("revert                 "); call revert_(self)
       case default;               allocate(tonto%known_keywords(13))
       tonto%known_keywords(1) = "}"
       tonto%known_keywords(2) = "put                    "
       tonto%known_keywords(3) = "put_bond_angle_table   "
       tonto%known_keywords(4) = "put_bond_length_table  "
       tonto%known_keywords(5) = "put_coord_info         "
       tonto%known_keywords(6) = "put_coordinates        "
       tonto%known_keywords(7) = "put_mm_info            "
       tonto%known_keywords(8) = "put_thermal_tensors    "
       tonto%known_keywords(9) = "put_torsion_angle_table"
       tonto%known_keywords(10) = "put_restraint_atoms    "
       tonto%known_keywords(11) = "read_cif               "
       tonto%known_keywords(12) = "redirect               "
       tonto%known_keywords(13) = "revert                 "
       call unknown_(tonto,word,"ATOMVEC:process_keyword")
       deallocate(tonto%known_keywords)
     end select

   end subroutine

   subroutine read_CIF(self)
    type(atom_type), dimension(:) :: self
    ! Read information from a Crystallographic Information File whose name is
    ! read from "stdin".
      pointer :: self
      type(cif_type), pointer :: cif
      logical(kind=kind(.true.)) :: found
      character(128) :: name

      call read_(stdin,name)
      call create_(cif,name)
      call open_(cif)
      call find_crystal_data_block_(cif,found)
      call ensure_(tonto,found,"ATOMVEC:read_CIF ... no crystal data block found")
      call read_CIF_(self,cif)
      call destroy_(cif)

   end subroutine

   subroutine read_CIF_1(self,cif)
    type(atom_type), dimension(:) :: self
    ! Read information from a Crystallographic Information File, "cif".
    ! NOTE: self is destroyed and created from this file!
      pointer :: self
      type(cif_type) :: cif
      logical(kind=kind(.true.)) :: fs,fc,fo,fu,fl
      character(128), dimension(:), pointer :: IDs,labels
      integer(kind=kind(1)) :: i,ind
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: pos,U
       ! Read the site labels

      call read_looped_item_(cif,"_atom_site_label",labels,fs)
      if (.not. fs) &
      call read_looped_item_(cif,"_atom_site_type_symbol",labels,fs)
      call ensure_(tonto,fs,"ATOMVEC:read_CIF_1 ... no atom site label information in CIF file")
       ! Read the site coordinates, and possibly occupancies
      call find_looped_item_(cif,"_atom_site_occupancy",fo)
      if (fo) then
         call create_(IDs,4)
         IDs = (/"_atom_site_fract_x  ", &
                "_atom_site_fract_y  ", &
                "_atom_site_fract_z  ", &
                "_atom_site_occupancy"/)
         call read_looped_items_(cif,IDs,pos,found=fc)
      else
         call create_(IDs,3)
         IDs = (/"_atom_site_fract_x  ", &
                "_atom_site_fract_y  ", &
                "_atom_site_fract_z  "/)
         call read_looped_items_(cif,IDs,pos,found=fc)
      end if
      call ensure_(tonto,fc,"ATOMVEC:read_CIF_1 ... no atom coordinate information in CIF file")
       ! Assign the type(cif_type) info to the atom list
      call destroy_(self)
      call create_(self,size(labels))
      call set_labels_and_atomic_numbers_(self,labels)
      self(:)%pos(1) = pos(1,:)
      self(:)%pos(2) = pos(2,:)
      self(:)%pos(3) = pos(3,:)
      self(:)%axis_system = "crystal"
      if (fo) self(:)%site_occupancy = pos(4,:)
      call destroy_(pos)
      call destroy_(IDs)
      call destroy_(labels)
       ! Now read U tensor if it is there ...
      call create_(IDs,6)
      IDs = (/"_atom_site_aniso_U_11", &
             "_atom_site_aniso_U_22", &
             "_atom_site_aniso_U_33", &
             "_atom_site_aniso_U_12", &
             "_atom_site_aniso_U_13", &
             "_atom_site_aniso_U_23"/)
      call read_looped_item_(cif,"_atom_site_aniso_label",labels,fl)
      call read_looped_items_(cif,IDs,U,found=fu)
      if (.not. fl .or. .not. fu) then;   return; end if
       ! Match the labels and assign the U tensors
      do i = 1,size(labels)
         ind = index_of_(self(:)%label,labels(i))
         call ensure_(tonto,ind>0,"ATOMVEC:read_CIF_1 ... label "//trim(labels(i))//" cant be found")
         self(ind)%thermal_tensor(1,1) = U(1,i)
         self(ind)%thermal_tensor(2,2) = U(2,i)
         self(ind)%thermal_tensor(3,3) = U(3,i)
         self(ind)%thermal_tensor(1,2) = U(4,i)
         self(ind)%thermal_tensor(2,1) = U(4,i)
         self(ind)%thermal_tensor(1,3) = U(5,i)
         self(ind)%thermal_tensor(3,1) = U(5,i)
         self(ind)%thermal_tensor(2,3) = U(6,i)
         self(ind)%thermal_tensor(3,2) = U(6,i)
      end do
      self(:)%thermal_axis_system = "crystal"
      call destroy_(U)
      call destroy_(labels)
      call destroy_(IDs)

   end subroutine

!*******************************************************************************

!   resolve_bases(basis,clobber,resolve_all) ::: leaky
!   ! Resolve the basis sets for each atom by matching the atom basis set label
!   ! with the labels from the basis set vector "basis". If "clobber" is present
!   ! and .true. (the default situation), then any matched basis is pointer
!   ! assigned to the matching element in "basis" irrespective of whether it is
!   ! already associated; otherwise if the matching basis set is already
!   ! associated, it is not pointer assigned. If "resolve_all" is present and .true.
!   ! (the default) an error is generated if all the basis sets are not resolved;
!   ! the default is that "resolve_all is FALSE.
!      basis :: BASISVEC*
!      clobber,resolve_all :: logical(kind=kind(.true.)), optional
!      a :: integer(kind=kind(1))
!      found,find_all :: logical(kind=kind(.true.))
!      call ensure_(tonto,basis.created,"no basis set")
!      find_all = .true.
!      if (present(resolve_all)) find_all = resolve_all
!      do a = 1,.n_atom
!         self(a).resolve_basis(basis,clobber,found)
!         if (find_all) then
!           call ensure_(tonto,found,"unknown basis, atom "// trim(a.to_str))
!         end
!      end
!   end
!
!   resolve_library_bases(basis,clobber,resolve_all) ::: leaky
!   ! Resolve the basis sets for each atom by first looking in the "basis" list,
!   ! and then (if needed) looking in a basis set library file. The appropriate
!   ! basis set library files are obtained from the basis set qualifier -- the
!   ! part after the colon in the atom basis set label. For example, if the atom
!   ! basis set label is "H:DZP", then the qualifier is "DZP" and the routine
!   ! looks in library file basis_sets/"DZP" for a matching basis set. If found,
!   ! the basis set is appended to "basis". If "clobber" is present and .true. (the
!   ! default situation), then any matched basis is pointer assigned to the
!   ! matching element in "basis" irrespective of whether it is already
!   ! associated; otherwise if the matching basis set is already associated, it
!   ! is not pointer assigned. If "resolve_all" is present and .true. (the default)
!   ! an error is generated if all the basis sets are not resolved; the default
!   ! is that "resolve_all is FALSE.
!      basis :: BASISVEC*
!      clobber,resolve_all :: logical(kind=kind(.true.)), optional
!      a :: integer(kind=kind(1))
!      found,find_all :: logical(kind=kind(.true.))
!      find_all = .true.
!      if (present(resolve_all)) find_all = resolve_all
!      do a = 1,.n_atom
!         self(a).resolve_library_basis(basis,clobber,found)
!         if (find_all) then
!           call ensure_(tonto,found,"unknown basis, atom "// trim(a.to_str))
!         end
!      end
!   end
!
!   resolve_basis_suffix(basis,suffix,clobber,resolve_all) ::: leaky
!   ! Match the basis set labels from the given basis set vector "basis"
!   ! with atom basis set labels contructed in a standard way by joining
!   ! the (lower case) atom chemical symbol with the -"suffix" string.
!   ! If "clobber" is present and .false., then any basis which is already associated
!   ! is not resolved even though there may be a matching entry. If "resolve_all"
!   ! is present and .false., then it is not an error if all the basis sets are
!   ! nopt resolved.
!      basis :: BASISVEC*
!      suffix :: character(*)
!      clobber,resolve_all :: logical(kind=kind(.true.)), optional
!      a :: integer(kind=kind(1))
!      found,find_all :: logical(kind=kind(.true.))
!      find_all = .true.
!      if (present(resolve_all)) find_all = resolve_all
!      do a = 1,.n_atom
!         self(a).resolve_basis_suffix(basis,suffix,clobber,found)
!         if (find_all) then
!           call ensure_(tonto,found,"unknown basis, atom "// trim(a.to_str))
!         end
!      end
!   end
!
!   resolve_bases(basis,clobber,resolve_all) ::: leaky
!   ! Resolve the basis sets for each atom by matching the atom basis set label
!   ! with the labels from the basis set vector "basis". If "clobber" is present
!   ! and .true. (the default situation), then any matched basis is pointer
!   ! assigned to the matching element in "basis" irrespective of whether it is
!   ! already associated; otherwise if the matching basis set is already
!   ! associated, it is not pointer assigned. If "resolve_all" is present and .true.
!   ! (the default) an error is generated if all the basis sets are not resolved;
!   ! the default is that "resolve_all is FALSE.
!      basis :: SLATERBASISVEC*
!      clobber,resolve_all :: logical(kind=kind(.true.)), optional
!      a :: integer(kind=kind(1))
!      found,find_all :: logical(kind=kind(.true.))
!      call ensure_(tonto,basis.created,"no basis set")
!      find_all = .true.
!      if (present(resolve_all)) find_all = resolve_all
!      do a = 1,.n_atom
!         self(a).resolve_basis(basis,clobber,found)
!         if (find_all) then
!           call ensure_(tonto,found,"unknown basis, atom "// trim(a.to_str))
!         end
!      end
!   end
!
!   resolve_library_bases(basis,clobber,resolve_all) ::: leaky
!   ! Resolve the basis sets for each atom by first looking in the "basis" list,
!   ! and then (if needed) looking in a basis set library file. The appropriate
!   ! basis set library files are obtained from the basis set qualifier -- the
!   ! part after the colon in the atom basis set label. For example, if the atom
!   ! basis set label is "H:DZP", then the qualifier is "DZP" and the routine
!   ! looks in library file basis_sets/"DZP" for a matching basis set. If found,
!   ! the basis set is appended to "basis". If "clobber" is present and .true. (the
!   ! default situation), then any matched basis is pointer assigned to the
!   ! matching element in "basis" irrespective of whether it is already
!   ! associated; otherwise if the matching basis set is already associated, it
!   ! is not pointer assigned. If "resolve_all" is present and .true. (the default)
!   ! an error is generated if all the basis sets are not resolved; the default
!   ! is that "resolve_all is FALSE.
!      basis :: SLATERBASISVEC*
!      clobber,resolve_all :: logical(kind=kind(.true.)), optional
!      a :: integer(kind=kind(1))
!      found,find_all :: logical(kind=kind(.true.))
!      find_all = .true.
!      if (present(resolve_all)) find_all = resolve_all
!      do a = 1,.n_atom
!         self(a).resolve_library_basis(basis,clobber,found)
!         if (find_all) then
!           call ensure_(tonto,found,"unknown basis, atom "// trim(a.to_str))
!         end
!      end
!   end
!
!   resolve_basis_suffix(basis,suffix,clobber,resolve_all) ::: leaky
!   ! Match the basis set labels from the given basis set vector "basis"
!   ! with atom basis set labels contructed in a standard way by joining
!   ! the (lower case) atom chemical symbol with the -"suffix" string.
!   ! If "clobber" is present and .false., then any basis which is already associated
!   ! is not resolved even though there may be a matching entry. If "resolve_all"
!   ! is present and .false., then it is not an error if all the basis sets are
!   ! nopt resolved.
!      basis :: SLATERBASISVEC*
!      suffix :: character(*)
!      clobber,resolve_all :: logical(kind=kind(.true.)), optional
!      a :: integer(kind=kind(1))
!      found,find_all :: logical(kind=kind(.true.))
!      find_all = .true.
!      if (present(resolve_all)) find_all = resolve_all
!      do a = 1,.n_atom
!         self(a).resolve_basis_suffix(basis,suffix,clobber,found)
!         if (find_all) then
!           call ensure_(tonto,found,"unknown basis, atom "// trim(a.to_str))
!         end
!      end
!   end

!   resolve_library_bases(basis,clobber,resolve_all) ::: leaky
!   ! Resolve the basis sets for each atom by first looking in the "basis" list,
!   ! and then (if needed) looking in a basis set library file. The appropriate
!   ! basis set library files are obtained from the basis set qualifier -- the
!   ! part after the colon in the atom basis set label. For example, if the atom
!   ! basis set label is "H:DZP", then the qualifier is "DZP" and the routine
!   ! looks in library file basis_sets/"DZP" for a matching basis set. If found,
!   ! the basis set is appended to "basis". If "clobber" is present and .true. (the
!   ! default situation), then any matched basis is pointer assigned to the
!   ! matching element in "basis" irrespective of whether it is already
!   ! associated; otherwise if the matching basis set is already associated, it
!   ! is not pointer assigned. If "resolve_all" is present and .true. (the default)
!   ! an error is generated if all the basis sets are not resolved; the default
!   ! is that "resolve_all is FALSE.
!      basis :: COPPENSBASISVEC*
!      clobber,resolve_all :: logical(kind=kind(.true.)), optional
!      a :: integer(kind=kind(1))
!      found,find_all :: logical(kind=kind(.true.))
!      find_all = .true.
!      if (present(resolve_all)) find_all = resolve_all
!      do a = 1,.n_atom
!         self(a).resolve_library_basis(basis,clobber,found)
!         if (find_all) then
!           call ensure_(tonto,found,"unknown basis, atom "// trim(a.to_str))
!         end
!      end
!   end

!   resolve_bases_by_labels(labels,basis,clobber,resolve_all)
!   ! Resolve the basis sets for each atom self(a), by pointer assigning to the
!   ! element in "basis" which has the same label as "labels(a)". If "clobber" is
!   ! present and .true. (the default situation), then any matched basis is pointer
!   ! assigned to the matching element in "basis" irrespective of whether it is
!   ! already associated; otherwise if the matching basis set is already
!   ! associated, it is not pointer assigned. If "resolve_all" is present and
!   ! .true. (the default) an error is generated if all the basis sets are not
!   ! resolved; the default is that "resolve_all is FALSE.
!      labels :: STRVEC
!      basis :: COPPENSBASISVEC*
!      clobber,resolve_all :: logical(kind=kind(.true.)), optional
!      a :: integer(kind=kind(1))
!      found,find_all :: logical(kind=kind(.true.))
!   call ensure_(tonto,basis.created,"no basis set")
!   call ensure_(tonto,labels.dim==.n_atom,"wrong number of labels")
!      find_all = .true.
!      if (present(resolve_all)) find_all = resolve_all
!      do a = 1,.n_atom
!         self(a).resolve_by_basis_label(labels(a),basis,clobber,found)
!         if (find_all) then
!           call ensure_(tonto,found,"unknown basis, atom "// trim(a.to_str))
!         end
!      end
!   end

!   resolve_bases_by_suffix(suffix,basis,clobber,resolve_all)
!   ! Resolve the basis sets for each atom self(a), by pointer assigning to the
!   ! element in "basis" which has a label constructed in a standard way, by
!   ! joining the atom chemical symbol with the -"suffix" string.  If "clobber"
!   ! is present and .false., then any basis which is already associated is not
!   ! resolved even though there may be a matching entry. If "resolve_all" is
!   ! present and .false., then it is not an error if all the basis sets are nopt
!   ! resolved.
!      suffix :: character(*)
!      basis :: COPPENSBASISVEC*
!      clobber,resolve_all :: logical(kind=kind(.true.)), optional
!      a :: integer(kind=kind(1))
!      found,find_all :: logical(kind=kind(.true.))
!      find_all = .true.
!      if (present(resolve_all)) find_all = resolve_all
!      do a = 1,.n_atom
!         self(a).resolve_by_basis_suffix(suffix,basis,clobber,found)
!         if (find_all) then
!           call ensure_(tonto,found,"unknown basis, atom "// trim(a.to_str))
!         end
!      end
!   end

   subroutine resolve_bases(self,basis,suffix)
    type(atom_type), dimension(:) :: self
    ! Resolve the basis sets for each atom -- by pointer assigning its basis to
    ! the element in "basis" which matches either the atoms .basis_label, or else
    ! matches a label constructed in a standard way, by joining the atom chemical
    ! symbol with the ":suffix" string.
      type(basis_type), dimension(:), pointer :: basis
      character(*), optional :: suffix
      integer(kind=kind(1)) :: a

      do a = 1,n_atom_(self)
         call resolve_basis_(self(a),basis,suffix)
      end do

   end subroutine

   subroutine resolve_bases_1(self,basis,suffix)
    type(atom_type), dimension(:) :: self
    ! Resolve the basis sets for each atom -- by pointer assigning its basis to
    ! the element in "basis" which matches either the atoms .basis_label, or else
    ! matches a label constructed in a standard way, by joining the atom chemical
    ! symbol with the ":suffix" string.
      type(slaterbasis_type), dimension(:), pointer :: basis
      character(*), optional :: suffix
      integer(kind=kind(1)) :: a

      do a = 1,n_atom_(self)
         call resolve_basis_(self(a),basis,suffix)
      end do

   end subroutine

   subroutine resolve_bases_2(self,basis,suffix)
    type(atom_type), dimension(:) :: self
    ! Resolve the basis sets for each atom -- by pointer assigning its basis to
    ! the element in "basis" which matches either the atoms .basis_label, or else
    ! matches a label constructed in a standard way, by joining the atom chemical
    ! symbol with the ":suffix" string.
      type(coppensbasis_type), dimension(:), pointer :: basis
      character(*), optional :: suffix
      integer(kind=kind(1)) :: a

      do a = 1,n_atom_(self)
         call resolve_basis_(self(a),basis,suffix)
      end do

   end subroutine

   subroutine resolve_axis_system(self,crystal)
    type(atom_type), dimension(:) :: self
    ! Change the atom axis systems to cartesian, from crystal, if required.
      type(crystal_type), intent(in) :: crystal
       integer(kind=kind(1)) :: a

      do a = 1,n_atom_(self)
         call resolve_axis_system_(self(a),crystal)
      end do

   end subroutine

   subroutine change_axis_system_to(self,axiskind,crystal)
    type(atom_type), dimension(:) :: self
    ! Change the axis system "axiskind" for all atoms to or from "cartesian" and
    ! "crystal".
      character(*) :: axiskind
      type(crystal_type), intent(in) :: crystal
       integer(kind=kind(1)) :: a

      do a = 1,n_atom_(self)
         call change_axis_system_to_(self(a),axiskind,crystal)
      end do

   end subroutine

   subroutine change_thermal_axis_system_to(self,axiskind,crystal)
    type(atom_type), dimension(:) :: self
    ! Change the thermal tensor axis system "axiskind" for all atoms to or from
    ! "cartesian" and "crystal".
      character(*) :: axiskind
      type(crystal_type), intent(in) :: crystal
       integer(kind=kind(1)) :: a

      do a = 1,n_atom_(self)
         call change_thermal_axis_system_to_(self(a),axiskind,crystal)
      end do

   end subroutine

!  **************
!  Output methods
!  **************

   subroutine put(self)
    type(atom_type), dimension(:) :: self
    ! Output atom information, without full basis set info

      call flush_(stdout)
      call show_(stdout,"Chemical Formula       =",trim(chemical_formula_(self)))
      call show_(stdout,"No of atoms            =",size(self))
      call show_(stdout,"No of electrons        =",n_e_(self))
      if (has_residue_names_(self)) then; call put_mm_info_(self)
      else;                         call put_coord_info_(self)
      end if
      if (has_restraints_(self))          call put_restraint_atoms_(self)

   end subroutine

   subroutine put_coord_info(self,all)
    type(atom_type), dimension(:) :: self
    ! Output atom coordinate information, including bond lengths, angles,
    ! without full basis set info
     logical(kind=kind(.true.)), intent(in), optional :: all

      call put_coordinates_(self)
      call put_bond_length_table_(self,all)
      call put_bond_angle_table_(self,all)
      call put_torsion_angle_table_(self,all)

   end subroutine

   subroutine put_coordinates(self)
    type(atom_type), dimension(:) :: self
    ! Output the atom coordinate information

      if (bases_are_all_labeled_(self) .or. coppensbases_are_all_labeled_(self)) then
        call put_coords_with_basis_label_(self)
      else
        call put_coords_without_basis_label_(self)
      end if

   end subroutine

   subroutine put_coords_with_basis_label(self)
    type(atom_type), dimension(:) :: self
    ! Output the atom coordinates information, including the basis label,
    ! but not the entire basis set.
      target :: self
      integer(kind=kind(1)) :: i
      character(128) :: label
      logical(kind=kind(.true.)) :: coppens

      call ensure_(tonto,bases_are_all_labeled_(self) .or. coppensbases_are_all_labeled_(self),"ATOMVEC:put_coords_with_basis&
&_label ... no bases")
      coppens = coppensbases_are_all_labeled_(self)
      call flush_(stdout)
      call text_(stdout,"Atom list information:")
      call dash_(stdout,int_fields=3,real_fields=4)
      call put_(stdout,"#",int_width=.true.)
      call put_(stdout,"ID",int_width=.true.)
      call put_(stdout,"Z",int_width=.true.)
      call put_(stdout,"x")
      call put_(stdout,"y")
      call put_(stdout,"z")
      call put_(stdout,"Basis")
      call flush_(stdout)
      call dash_(stdout,int_fields=3,real_fields=4)
      do i = 1,n_atom_(self)
         call put_(stdout,i)
         call put_(stdout,self(i)%label,int_width=.true.)
         call put_(stdout,self(i)%atomic_number)
         call put_(stdout,self(i)%pos(1))
         call put_(stdout,self(i)%pos(2))
         call put_(stdout,self(i)%pos(3))
         if (coppens) then; label = self(i)%coppensbasis%label
         else;              label = self(i)%basis%label
         end if
         call put_(stdout,label)
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=3,real_fields=4)

   end subroutine

   subroutine put_coords_without_basis_label(self)
    type(atom_type), dimension(:) :: self
    ! Output the atom coordinates information, without basis set label
       integer(kind=kind(1)) :: i

      call flush_(stdout)
      call text_(stdout,"Atom list information:")
      call dash_(stdout,int_fields=3,real_fields=3)
      call put_(stdout,"#",int_width=.true.)
      call put_(stdout,"ID",int_width=.true.)
      call put_(stdout,"Z",int_width=.true.)
      call put_(stdout,"x")
      call put_(stdout,"y")
      call put_(stdout,"z")
      call flush_(stdout)
      call dash_(stdout,int_fields=3,real_fields=3)
      do i = 1,n_atom_(self)
         call put_(stdout,i)
         call put_(stdout,self(i)%label,int_width=.true.)
         call put_(stdout,self(i)%atomic_number)
         call put_(stdout,self(i)%pos(1))
         call put_(stdout,self(i)%pos(2))
         call put_(stdout,self(i)%pos(3))
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=3,real_fields=3)

   end subroutine

   function list_symbol(self,a) result(res)
    type(atom_type), dimension(:) :: self
    ! Return the chemical symbol and atomvec number of atom "a".
    ! e.g., "Na (3)"
     integer(kind=kind(1)) :: a
     character(128) :: res

     res =  trim(chemical_symbol_(self(a))) // " (" // trim(to_str_(a)) // ")"

   end function

   subroutine put_bond_length_table(self,all)
    type(atom_type), dimension(:) :: self
    ! Output the bond length table. If "all" is present and true, put out all
    ! possible bond lengths
      logical(kind=kind(.true.)), intent(in), optional :: all
      logical(kind=kind(.true.)) :: print_all_bonds
      type(intvec__type), dimension(:), pointer :: atom_kind
      integer(kind=kind(1)) :: n_k,k,l,kk,ll,a,b
      character(128) :: symbol_a,symbol_b
      real(kind=kind(1.0d0)) :: r_ab

      print_all_bonds = .false.
      if (present(all)) print_all_bonds = all
      if (n_atom_(self)<2) then;   return; end if
      if (no_of_bonds_(self)<1 .and. .not. print_all_bonds) then;   return; end if
      call make_atom_kind_list_(self,atom_kind)
      n_k = size(atom_kind)
      call flush_(stdout)
      call text_(stdout,"Bond lengths:",flush=2)
      call show_(stdout,"No. of independent bonds  =",no_of_bonds_(self))
      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=2)
      call put_(stdout,"Atom_a",int_width=.true.)
      call put_(stdout,"Atom_b",int_width=.true.)
      call put_(stdout,"r_ab/au")
      call put_(stdout,"r_ab/Angstrom",flush=1)
      call dash_(stdout,int_fields=2,real_fields=2)
      do k = 1,n_k
      do kk = 1,size(atom_kind(k)%element)
         a = atom_kind(k)%element(kk)
         symbol_a = list_symbol_(self,a)
         do l = 1,n_k
         do ll = 1,size(atom_kind(l)%element)
            b = atom_kind(l)%element(ll)
            symbol_b = list_symbol_(self,b)
            if (a>=b) cycle
            if (.not. print_all_bonds .and. .not. bonded_(self,a,b)) cycle
            r_ab = bond_distance_(self,a,b)
            call put_(stdout,symbol_a,int_width=.true.)
            call put_(stdout,symbol_b,int_width=.true.)
            call put_(stdout,r_ab)
            call put_(stdout,r_ab*0.52917724924d0)
            call flush_(stdout)
         end do
         end do
      end do
      end do
      call destroy_(atom_kind)
      call dash_(stdout,int_fields=2,real_fields=2)

   end subroutine

   subroutine put_bond_angle_table(self,all)
    type(atom_type), dimension(:) :: self
    ! Output the bond length table. If "all" is present and true, put out all
    ! possible angles
      logical(kind=kind(.true.)), intent(in), optional :: all
      logical(kind=kind(.true.)) :: print_all_angles
      type(intvec__type), dimension(:), pointer :: atom_kind
      integer(kind=kind(1)) :: n_k,k,l,m,kk,ll,mm,a,b,c
      character(128) :: symbol_a,symbol_b,symbol_c
      real(kind=kind(1.0d0)) :: theta_abc

      print_all_angles = .false.
      if (present(all)) print_all_angles = all
      if (n_atom_(self)<3 ) then;   return; end if
      if (no_of_angles_(self)<1 .and. .not. print_all_angles) then;   return; end if
      if (no_of_angles_(self)>100 .and. .not. print_all_angles) then;   return; end if
      call make_atom_kind_list_(self,atom_kind)
      n_k = size(atom_kind)
      call flush_(stdout)
      call text_(stdout,"Bond angles (b the central atom):",flush=2)
      call show_(stdout,"No. of independent angles =",no_of_angles_(self))
      call flush_(stdout)
      call dash_(stdout,int_fields=3,real_fields=1)
      call put_(stdout,"Atom_a",int_width=.true.)
      call put_(stdout,"Atom_b",int_width=.true.)
      call put_(stdout,"Atom_c",int_width=.true.)
      call put_(stdout,"Angle_abc/degrees",flush=1)
      call dash_(stdout,int_fields=3,real_fields=1)
      do k = 1,n_k
      do kk = 1,size(atom_kind(k)%element)
         a = atom_kind(k)%element(kk)
         symbol_a = list_symbol_(self,a)
         do l = 1,n_k
         do ll = 1,size(atom_kind(l)%element)
            b = atom_kind(l)%element(ll)
            if (a==b) cycle
            if (.not. print_all_angles .and. .not. bonded_(self,a,b)) cycle
            symbol_b = list_symbol_(self,b)
            do m = 1,n_k
            do mm = 1,size(atom_kind(m)%element)
               c = atom_kind(m)%element(mm)
               if (b==c .or. c==a) cycle
               if (.not. print_all_angles .and. .not. bonded_(self,b,c)) cycle
               symbol_c = list_symbol_(self,c)
               theta_abc = bond_angle_(self,a,b,c,degrees=.true.)
               call put_(stdout,symbol_a,int_width=.true.)
               call put_(stdout,symbol_b,int_width=.true.)
               call put_(stdout,symbol_c,int_width=.true.)
               call put_(stdout,theta_abc)
               call flush_(stdout)
            end do
            end do
         end do
         end do
      end do
      end do
      call destroy_(atom_kind)
      call dash_(stdout,int_fields=3,real_fields=1)

   end subroutine

   subroutine put_torsion_angle_table(self,all)
    type(atom_type), dimension(:) :: self
    ! Output the torsion angle table. If "all" is present and true, put out all
    ! possible angles
      logical(kind=kind(.true.)), intent(in), optional :: all
      logical(kind=kind(.true.)) :: print_all_angles,abc_colinear,bcd_colinear
      type(intvec__type), dimension(:), pointer :: atom_kind
      integer(kind=kind(1)) :: n_k,k,l,m,n,kk,ll,mm,nn,a,b,c,d
      character(128) :: symbol_a,symbol_b,symbol_c,symbol_d
      real(kind=kind(1.0d0)) :: theta_abcd

      print_all_angles = .false.
      if (present(all)) print_all_angles = all
      if (n_atom_(self)<4) then;   return; end if
      if (no_of_torsion_angles_(self)<1 .and. .not. print_all_angles) then;   return; end if
      if (no_of_angles_(self)>100 .and. .not. print_all_angles) then;   return; end if
      call make_atom_kind_list_(self,atom_kind)
      n_k = size(atom_kind)
      call flush_(stdout)
      call text_(stdout,"Torsion angles (looking down b->c):",flush=2)
      call show_(stdout,"No. of independent angles =",no_of_torsion_angles_(self))
      call flush_(stdout)
      call dash_(stdout,int_fields=4,real_fields=1)
      call put_(stdout,"Atom_a",int_width=.true.)
      call put_(stdout,"Atom_b",int_width=.true.)
      call put_(stdout,"Atom_c",int_width=.true.)
      call put_(stdout,"Atom_d",int_width=.true.)
      call put_(stdout,"Angle_abcd/degrees",flush=1)
      call dash_(stdout,int_fields=4,real_fields=1)
      do k = 1,n_k
      do kk = 1,size(atom_kind(k)%element)
         a = atom_kind(k)%element(kk)
         symbol_a = list_symbol_(self,a)
         do l = 1,n_k
         do ll = 1,size(atom_kind(l)%element)
            b = atom_kind(l)%element(ll)
            if (a==b) cycle
            if (.not. print_all_angles .and. .not. bonded_(self,a,b)) cycle
            symbol_b = list_symbol_(self,b)
            do m = 1,n_k
            do mm = 1,size(atom_kind(m)%element)
               c = atom_kind(m)%element(mm)
               if (a==c .or. b==c) cycle
               if (.not. print_all_angles .and. .not. bonded_(self,b,c)) cycle
               symbol_c = list_symbol_(self,c)
               do n = 1,n_k
               do nn = 1,size(atom_kind(n)%element)
                  d = atom_kind(n)%element(nn)
                  if (.not. print_all_angles .and. .not. bonded_(self,c,d)) cycle
                  if (a==d .or. b==d .or. c==d) cycle
                  symbol_d = list_symbol_(self,d)
                  theta_abcd = torsion_angle_(self,a,b,c,d,abc_colinear, &
                                            bcd_colinear,degrees=.true.)
                  call put_(stdout,symbol_a,int_width=.true.)
                  call put_(stdout,symbol_b,int_width=.true.)
                  call put_(stdout,symbol_c,int_width=.true.)
                  call put_(stdout,symbol_d,int_width=.true.)
                  if (abc_colinear .and. bcd_colinear) then
                     call put_(stdout,"a-b-c-d colinear")
                  else if (abc_colinear) then
                     call put_(stdout,"a-b-c colinear")
                  else if (bcd_colinear) then
                     call put_(stdout,"b-c-d colinear")
                  else
                     call put_(stdout,theta_abcd)
                  end if
                  call flush_(stdout)
               end do
               end do
            end do
            end do
         end do
         end do
      end do
      end do
      call destroy_(atom_kind)
      call dash_(stdout,int_fields=4,real_fields=1)

   end subroutine

   subroutine put_thermal_tensors(self)
    type(atom_type), dimension(:) :: self
    ! Output the thermal tensors.
      integer(kind=kind(1)) :: i

      call flush_(stdout)
      call text_(stdout,"Thermal tensors in cartesian coordinates/(bohr^2):")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=6)
      call put_(stdout,"Atom",int_width=.true.)
      call put_(stdout,"U11")
      call put_(stdout,"U22")
      call put_(stdout,"U33")
      call put_(stdout,"U12")
      call put_(stdout,"U13")
      call put_(stdout,"U23")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=6)
      do i = 1,size(self)
        call put_(stdout,i)
        call put_thermal_tensor_(self(i))
        call flush_(stdout)
      end do
      call dash_(stdout,int_fields=1,real_fields=6)
      call flush_(stdout)

   end subroutine

   subroutine put_mm_info(self)
    type(atom_type), dimension(:) :: self
    ! Output a table of the residue names, sequence numbers, charges, but
    ! no basis sets. This is in PDB input format.
      integer(kind=kind(1)) :: i

      call flush_(stdout)
      call text_(stdout,"Residue information:",flush=1)
      call dash_(stdout,width=42)
      call put_(stdout," ",width=4)
      call put_(stdout,"#",width=7)
      call put_(stdout,"Name",width=4)
      call put_(stdout,"Residue",width=5)
      call put_(stdout,"Sequence",width=6)
      call put_(stdout,"x",width=3)
      call put_(stdout,"y",width=3)
      call put_(stdout,"z",width=3)
      call put_(stdout,"Charge",width=3)
      call put_(stdout,"Element",width=4)
      call flush_(stdout)
      call dash_(stdout,width=42)
      call dash_(stdout,int_fields=6,real_fields=5)
      do i = 1,size(self)
         call put_(stdout,"ATOM",width=4)
         call put_(stdout,i,width=7)
         call put_(stdout,self(i)%residue_atom_name,width=4)
         call put_(stdout,self(i)%residue_name,width=5)
         call put_(stdout,self(i)%sequence_number,width=6)
         call put_(stdout,self(i)%pos(1),width=8,precision=3)
         call put_(stdout,self(i)%pos(2),width=8,precision=3)
         call put_(stdout,self(i)%pos(3),width=8,precision=3)
         call put_(stdout,self(i)%mm_charge,width=7,precision=3)
         call put_(stdout,self(i)%label,width=4)
         call flush_(stdout)
      end do
      call dash_(stdout,width=42)

   end subroutine

   subroutine put_restraint_atoms(self)
    type(atom_type), dimension(:) :: self
    ! Output a table of the atom names, residue names and restraint atom
    ! information
      integer(kind=kind(1)) :: i

      call text_(stdout,"Restraint atoms:",flush=1)
      call save_(stdout)
      call set_int_width_(stdout,9)
      call set_real_width_(stdout,9)
      call set_real_precision_(stdout,3)
      call dash_(stdout,int_fields=5,real_fields=3)
      call put_(stdout," ",int_width=.true.)
      call put_(stdout,"Atom",int_width=.true.)
      call put_(stdout,"Residue",int_width=.true.)
      call put_(stdout,"Sequence")
      call put_(stdout,"x")
      call put_(stdout,"y")
      call put_(stdout,"z")
      call put_(stdout,"Force",int_width=.true.)
      call flush_(stdout)
      call dash_(stdout,int_fields=5,real_fields=3)
      do i = 1,size(self)
         if (is_zero_(self(i)%restraining_force_constant)) cycle
         call put_(stdout," ",int_width=.true.)
         call put_(stdout,self(i)%residue_atom_name,int_width=.true.)
         call put_(stdout,self(i)%residue_name,int_width=.true.)
         call put_(stdout,self(i)%sequence_number)
         call put_(stdout,self(i)%restraining_position(1))
         call put_(stdout,self(i)%restraining_position(2))
         call put_(stdout,self(i)%restraining_position(3))
         call put_(stdout,self(i)%restraining_force_constant)
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=5,real_fields=3)
      call unsave_(stdout)

   end subroutine

   subroutine put_vrml(self,out)
    type(atom_type), dimension(:) :: self
    ! Put vrml version of the atomvec to the file in object "out".
     type(textfile_type) :: out

     call set_real_precision_(out,5)
     call set_real_width_(out,12)

     call text_(stdout,"Generating VRML atoms...")
     call put_vrml_header_(self,out)
     call put_vrml_atoms_(self,out)
     call put_vrml_bonds_(self,out)
     call text_(stdout,"done VRML atoms")
     call flush_(stdout)

   end subroutine

   subroutine put_vrml_header(self,out)
    type(atom_type), dimension(:) :: self
    ! Put vrml header, and prototype bond and spheres, to the file.
     type(textfile_type) :: out

     call text_(out,"PROTO Atom " // achar(91))
     call text_(out,"   field SFColor col 1 0 0")
     call text_(out,"   field SFFloat rad 1")
     call text_(out,"   field SFVec3f pos 0 0 0")
     call text_(out,achar(93))
     call text_(out,"{")
     call text_(out,"   Transform {")
     call text_(out,"      translation IS pos")
     call text_(out,"      children " // achar(91))
     call text_(out,"         Shape {")
     call text_(out,"            appearance Appearance {")
     call text_(out,"               material Material {")
     call text_(out,"                  diffuseColor IS col")
     call text_(out,"               }")
     call text_(out,"            }")
     call text_(out,"            geometry Sphere {")
     call text_(out,"              radius IS rad")
     call text_(out,"            }")
     call text_(out,"         }")
     call text_(out,"      " // achar(93))
     call text_(out,"   }")
     call text_(out,"}")
     call flush_(out)
     call text_(out,"PROTO Bond " // achar(91))
     call text_(out,"   field SFColor col 1 0 0")
     call text_(out,"   field SFFloat hgt 1")
     call text_(out,"   field SFVec3f pos 0 0 0")
     call text_(out,"   field SFRotation rot 1 0 0 0")
     call text_(out,achar(93))
     call text_(out,"{")
     call text_(out,"   Transform {")
     call text_(out,"      translation IS pos")
     call text_(out,"      rotation IS rot")
     call text_(out,"      children " // achar(91))
     call text_(out,"         Shape {")
     call text_(out,"            appearance Appearance {")
     call text_(out,"               material Material {")
     call text_(out,"                 diffuseColor IS col")
     call text_(out,"               }")
     call text_(out,"            }")
     call text_(out,"            geometry Cylinder {")
     call text_(out,"               radius 0.304245979")
     call text_(out,"               height IS hgt")
     call text_(out,"               top    FALSE")
     call text_(out,"               bottom FALSE")
     call text_(out,"            }")
     call text_(out,"         }")
     call text_(out,"      " // achar(93))
     call text_(out,"   }")
     call text_(out,"}")
     call flush_(out)

   end subroutine

   subroutine put_vrml_atoms(self,out)
    type(atom_type), dimension(:) :: self
    ! Put spheres for the atoms to view as vrml.
     type(textfile_type) :: out
     real(kind=kind(1.0d0)), dimension(3) :: colour
     character(2) :: label
     real(kind=kind(1.0d0)) :: radius
     integer(kind=kind(1)) :: n,Z

     do n=1,n_atom_(self)
       Z = self(n)%atomic_number
       label = chemical_symbol_(self(n))
       select case (label)
         case ("H ");                     colour = (/191,196,192/)
         case ("He","Rn");                colour = (/88,196,160/)
         case ("Li","Be","Na","Mg");      colour = (/144,149,145/)
         case ("B ");                     colour = (/187,4,187/)
         case ("C ");                     colour = (/160,80,17/)
         case ("N ","Al");                colour = (/126,169,176/)
         case ("Si");                     colour = (/192,172,137/)
         case ("S ");                     colour = (/192,165,0/)
         case ("Cl");                     colour = (/0,165,0/)
         case ("O ","Ca","Ge","As");      colour = (/192,12,8/)
         case ("Se","Br","Sr","I ");      colour = (/192,12,8/)
         case default;                    colour = (/192,148,25/)
       end select
       select case (Z)
         case (1:54);     radius = bragg_slater_radius_(self(n))
         case default;    radius = 1.30d0
       end select
       colour = colour / 256
       call text_(out,"Atom {")
       call text_(out," col " // trim(to_str_(colour,"f15.8",separator=", ")))
       call text_(out," pos " // trim(to_str_(self(n)%pos,"f15.8",separator=", ")))
       call text_(out," rad " // trim(to_str_(radius,"f15.8")))
       call text_(out,"}")
     end do

   end subroutine

   subroutine put_vrml_bonds(self,out)
    type(atom_type), dimension(:) :: self
    ! Put bonds for the atoms to view as vrml.
     type(textfile_type) :: out
     real(kind=kind(1.0d0)), dimension(3) :: col_a,col_b,posa,posb,pos1,pos2,AB,centre
     real(kind=kind(1.0d0)), dimension(4) :: rot
     real(kind=kind(1.0d0)) :: hgta,hgtb,rada,radb
     integer(kind=kind(1)) :: a,b

     do a = 1,n_atom_(self)
       do b = 1,a-1
         if (bonded_(self,a,b)) then
           posa = self(a)%pos
           posb = self(b)%pos
           AB = posb-posa
           call normalise_(AB)
           rada = bragg_slater_radius_(self(a))
           radb = bragg_slater_radius_(self(b))
           centre = 0.50d0*(posa+rada*AB + posb-radb*AB)
           hgta = norm_((centre - posa))
           hgtb = norm_((posb - centre))
           pos1 = 0.50d0*(centre + posa)
           pos2 = 0.50d0*(centre + posb)
           rot(4) = 3.141592653589793d0
           rot(1:3) = AB + (/0.0d0,1.0d0,0.0d0/)
           call normalise_(rot(1:3))
           select case (chemical_symbol_(self(a)))
             case ("H ");                     col_a = (/191,196,192/)
             case ("He","Rn");                col_a = (/88,196,160/)
             case ("Li","Be","Na","Mg");      col_a = (/144,149,145/)
             case ("B ");                     col_a = (/187,4,187/)
             case ("C ");                     col_a = (/160,80,17/)
             case ("N ","Al");                col_a = (/126,169,176/)
             case ("Si");                     col_a = (/192,172,137/)
             case ("S ");                     col_a = (/192,165,0/)
             case ("Cl");                     col_a = (/0,165,0/)
             case ("O ","Ca","Ge","As");      col_a = (/192,12,8/)
             case ("Se","Br","Sr","I ");      col_a = (/192,12,8/)
             case default;                    col_a = (/192,148,25/)
           end select
           select case (chemical_symbol_(self(b)))
             case ("H ");                     col_b = (/191,196,192/)
             case ("He","Rn");                col_b = (/88,196,160/)
             case ("Li","Be","Na","Mg");      col_b = (/144,149,145/)
             case ("B ");                     col_b = (/187,4,187/)
             case ("C ");                     col_b = (/160,80,17/)
             case ("N ","Al");                col_b = (/126,169,176/)
             case ("Si");                     col_b = (/192,172,137/)
             case ("S ");                     col_b = (/192,165,0/)
             case ("Cl");                     col_b = (/0,165,0/)
             case ("O ","Ca","Ge","As");      col_b = (/192,12,8/)
             case ("Se","Br","Sr","I ");      col_b = (/192,12,8/)
             case default;                    col_b = (/192,148,25/)
           end select
           col_a = col_a / 256
           col_b = col_b / 256
           call text_(out,"Bond {")
           call text_(out," col " //  trim(to_str_(col_a,"f15.8",separator=", ")))
           call text_(out," pos " //  trim(to_str_(pos1,"f15.8",separator=", ")))
           call text_(out," rot " //  trim(to_str_(rot,"f15.8",separator=", ")))
           call text_(out," hgt " //  trim(to_str_(hgta,"f15.8")))
           call text_(out,"}")
           call text_(out,"Bond {")
           call text_(out," col " //  trim(to_str_(col_b,"f15.8",separator=", ")))
           call text_(out," pos " //  trim(to_str_(pos2,"f15.8",separator=", ")))
           call text_(out," rot " //  trim(to_str_(rot,"f15.8",separator=", ")))
           call text_(out," hgt " //  trim(to_str_(hgtb,"f15.8")))
           call text_(out,"}")
         end if
       end do
     end do

   end subroutine

!  *************************
!  Geometry unit conversions
!  *************************

   subroutine convert_from_angstrom(self)
    type(atom_type), dimension(:) :: self
    ! Convert atom positions to A.U. from Angstroms
       integer(kind=kind(1)) :: a

      do a = 1,n_atom_(self)
         self(a)%pos = self(a)%pos*(1/0.52917724924d0)
      end do

   end subroutine

   subroutine convert_to_angstrom(self)
    type(atom_type), dimension(:) :: self
    ! Convert atom positions to Angstroms from A.U.
       integer(kind=kind(1)) :: a

      do a = 1,n_atom_(self)
         self(a)%pos = self(a)%pos*0.52917724924d0
      end do

   end subroutine

   subroutine convert_from_crystal(self,crystal)
    type(atom_type), dimension(:) :: self
    ! Convert atom positions to A.U. from fractional crystal coordinates
      type(crystal_type) :: crystal

      call rotate_(self,crystal%unitcell%direct_matrix)

   end subroutine

   subroutine convert_to_crystal(self,crystal)
    type(atom_type), dimension(:) :: self
    ! Convert atom positions to fractional crystal coordinates from A.U.
      type(crystal_type) :: crystal

      call rotate_(self,crystal%unitcell%inverse_matrix)

   end subroutine

   subroutine get_coordinates(self,coord)
    type(atom_type), dimension(:) :: self
    ! Get the atom coordinates in a matrix object "coord"
      real(kind=kind(1.0d0)), dimension(:,:) :: coord
       integer(kind=kind(1)) :: a

      if (size(coord,1)==3) then
         do a = 1,n_atom_(self)
            coord(:,a) = self(a)%pos
         end do
      else if (size(coord,2)==3) then
         do a = 1,n_atom_(self)
            coord(a,:) = self(a)%pos
         end do
      else
         call die_(tonto,"ATOMVEC:get_coordinates ... wrong shape for coordinate matrix")
      end if

   end subroutine

   subroutine get_mean_neutron_numbers(self,NN)
    type(atom_type), dimension(:) :: self
    ! Get the atom coordinates in a matrix object "coord"
      real(kind=kind(1.0d0)), dimension(:) :: NN
       integer(kind=kind(1)) :: a

      do a = 1,n_atom_(self)
         NN(a) = mean_neutron_number_(self(a))
      end do

   end subroutine

!  *******************
!  Information methods
!  *******************

   function sum_of_atomic_numbers(self) result(res)
    type(atom_type), dimension(:) :: self
    ! Return the sum of the atomic numbers
      real(kind=kind(1.0d0)) :: res

      res = sum(self(:)%atomic_number)

   end function

   function atomic_numbers(self) result(res)
    type(atom_type), dimension(:) :: self
    ! Return the atomic numbers as a vector
      real(kind=kind(1.0d0)), dimension(size(self)) :: res

      res = self(:)%atomic_number

   end function

   function nuclear_energy(self) result(res)
    type(atom_type), dimension(:) :: self
    ! Return the nuclear repulsion energy
      real(kind=kind(1.0d0)) :: res
      integer(kind=kind(1)) :: i,j,qi,qj
      real(kind=kind(1.0d0)), dimension(3) :: radius

      res = 0.0d0
      do i=1,n_atom_(self)
         qi = self(i)%atomic_number
         do j=1,i-1
            qj = self(j)%atomic_number
            radius =  self(j)%pos - self(i)%pos
            res = res + qi * qj / sqrt( dot_(radius,radius) )
         end do
      end do

   end function

   function nuclear_energy_1(self,atoms) result(res)
    type(atom_type), dimension(:) :: self
    ! Return the nuclear repulsion energy felt by the group of atoms "a" in
    ! the field of all the nuclei in "self"
      integer(kind=kind(1)), dimension(:) :: atoms
      real(kind=kind(1.0d0)) :: res
      integer(kind=kind(1)) :: a, i, j,qi,qj, n_atoms
      real(kind=kind(1.0d0)), dimension(3) :: radius

      res = 0.0d0
      n_atoms = size(atoms)
      do i=1,n_atoms
       a = atoms(i)
       qi = self(a)%atomic_number
       do j=1,n_atom_(self)
        if (any(atoms==j)) cycle
        qj = self(j)%atomic_number
        radius =  self(j)%pos - self(a)%pos
        res = res + qi * qj / sqrt( dot_(radius,radius) )
       end do
      end do
      res = res + nuclear_energy_(self(atoms))

   end function

   function nuclear_energy_2(self,atoms,nuclei) result(res)
    type(atom_type), dimension(:) :: self
    ! Return the nuclear repulsion energy felt by the group of atoms "a" in
    ! the field of all the nuclei in "nuclei"
      integer(kind=kind(1)), dimension(:) :: atoms, nuclei
      real(kind=kind(1.0d0)) :: res
      integer(kind=kind(1)) :: a, i, j,k,qi,qj, n_atoms, n_field
      real(kind=kind(1.0d0)), dimension(3) :: radius

      res = 0.0d0
      n_atoms = size(atoms)
      n_field = size(nuclei)
      do i=1,n_atoms
       a = atoms(i)
       qi = self(a)%atomic_number
       do k=1,n_field
        j = nuclei(k)
        if (any(atoms==j)) cycle
        qj = self(j)%atomic_number
        radius =  self(j)%pos - self(a)%pos
        res = res + qi * qj / sqrt( dot_(radius,radius) )
       end do
      end do
      res = res + nuclear_energy_(self(atoms))

   end function

   function chemical_formula(self) result(res)
    type(atom_type), dimension(:) :: self
    ! Return the chemical formula for the molecule, as a string, in alphabetical
    ! order of elements
      character(128) :: res
      character(128), dimension(:), pointer :: symbol
      integer(kind=kind(1)) :: a,na

      call create_(symbol,size(self))
      do a = 1,n_atom_(self)
         symbol(a) = chemical_symbol_(self(a))
      end do
      call quick_sort_(symbol)
      res = " "
      a = 1
      do
         na = count(symbol==symbol(a))
         res = trim(res) // trim(symbol(a))
         if (na>1) &
         res = trim(res) // trim(to_str_(na))
         a = a + na
         if (a>n_atom_(self)) exit
      end do
      call destroy_(symbol)

   end function

   function centre_of_mass(self) result(centre)
    type(atom_type), dimension(:) :: self
    ! Return the centre of mass
      real(kind=kind(1.0d0)), dimension(3) :: centre
      integer(kind=kind(1)) :: a
      real(kind=kind(1.0d0)) :: mw

      mw = 1.0d0/molecular_weight_(self)
      centre = 0.0d0
      do a = 1,n_atom_(self)
         centre = centre + self(a)%pos*mass_(self(a))*mw
      end do

   end function

   subroutine move_origin_to_centre_of_mass(self)
    type(atom_type), dimension(:) :: self
    ! Move the origin to the centre of mass
      real(kind=kind(1.0d0)), dimension(3) :: com

      com = centre_of_mass_(self)
      call translate_(self,-com)

   end subroutine

   function reduced_mass(self) result(mu)
    type(atom_type), dimension(:) :: self
    ! Return the centre of mass
      real(kind=kind(1.0d0)) :: mu
      integer(kind=kind(1)) :: a

      mu = 0.0d0
      do a = 1,n_atom_(self)
         mu = mu + 1.0d0 / mass_(self(a))
      end do
      mu = 1.0d0/mu

   end function

   subroutine make_inertia_tensor(self,it)
    type(atom_type), dimension(:) :: self
    ! Make the moment of inertia tensor wrt the centre of mass
      real(kind=kind(1.0d0)), dimension(3,3) :: it
      real(kind=kind(1.0d0)), dimension(3,3) :: m
      real(kind=kind(1.0d0)), dimension(3) :: com,r
      real(kind=kind(1.0d0)) :: trace
      integer(kind=kind(1)) :: a

      com = centre_of_mass_(self)
      it = 0.0d0
      do a = 1, n_atom_(self)
         r = self(a)%pos - com
         m = spread(r,dim=1,ncopies=3)*spread(r,dim=2,ncopies=3)
         m = mass_(self(a)) * m
         trace = trace_(m)
         m = -m
         call add_to_diagonal_(m,trace)
         it = it + m
      end do

   end subroutine

   subroutine make_principal_moments(self,pm,pa)
    type(atom_type), dimension(:) :: self
    ! Make the principal moments "pm" and principal axes "pm" wrt the centre of
    ! mass.  The principal axes are made to be right handed.
    ! - For sperical tops, the local x,y,z axes are the pricipal axes
    ! - For symmetric tops, the C axis is unique
    ! - For asymmetric tops, the pricipal axes are aligned close to the local
    ! x,y,z axes
      real(kind=kind(1.0d0)), dimension(3) :: pm
      real(kind=kind(1.0d0)), dimension(3,3) :: pa
      real(kind=kind(1.0d0)), dimension(3,3) :: it

      call make_inertia_tensor_(self,it)
      call solve_eigenproblem_(it,pm,pa)
      where (pm<10.0d0**(-6))
        pm = 0.0d0                                ! Small moments set to zero
      end where
      if (is_spherical_top_(self,pm)) then            ! For spherical tops, principal
            call to_unit_matrix_(pa)                    ! axes are x,y,z
      else if (is_symmetric_top_(self,pm)) then       ! For symmetric tops C axis is
         if (abs(pm(1)-pm(3)) < 10.0d0**(-6)) then     ! unique
            call swap_columns_(pa,2,3)
            call swap_elements_(pm,2,3)
         else if (abs(pm(2)-pm(3)) < 10.0d0**(-6)) then
            call swap_columns_(pa,1,3)
            call swap_elements_(pm,1,3)
         end if
      else                                       ! For asymmetric tops, principal
         if (abs(pa(1,2)) > abs(pa(1,1))) then   ! axes are close to x,y,z
            call swap_columns_(pa,1,2)
            call swap_elements_(pm,1,2)
         end if
         if (abs(pa(1,3)) > abs(pa(1,1))) then
            call swap_columns_(pa,1,3)
            call swap_elements_(pm,1,3)
         end if
         if (abs(pa(2,3)) > abs(pa(2,2))) then
            call swap_columns_(pa,2,3)
            call swap_elements_(pm,2,3)
         end if
      end if
      if (determinant_(pa)>0.0d0) then;   return; end if ! Ensure principal axes are right handed
      if (abs(pm(1)-pm(2)) < 10.0d0**(-6)) then
         call swap_elements_(pm,1,2)
         call swap_columns_(pa,1,2)
      else if (abs (pm(2)-pm(3)) < 10.0d0**(-6)) then
         call swap_elements_(pm,2,3)
         call swap_columns_(pa,2,3)
      else
         pa(1:3,3) = -pa(1:3,3)
      end if

   end subroutine

   function is_linear(self,pm) result(res)
    type(atom_type), dimension(:) :: self
    ! Return .true. if the geometry is linear.
    ! Needs principal moments of inertia "pm"
      logical(kind=kind(.true.)) :: res
      real(kind=kind(1.0d0)), dimension(3) :: pm

      res = any(pm==0.0d0)

   end function

   function is_spherical_top(self,pm) result(res)
    type(atom_type), dimension(:) :: self
    ! Return .true. if the geometry is a spherical top
    ! Needs principal moments of inertia "pm"
      logical(kind=kind(.true.)) :: res
      real(kind=kind(1.0d0)), dimension(3) :: pm

      res = no_of_same_principal_moments_(self,pm)==3

   end function

   function is_symmetric_top(self,pm) result(res)
    type(atom_type), dimension(:) :: self
    ! Return .true. if the geometry is a symmetric top
    ! Needs principal moments of inertia "pm"
      logical(kind=kind(.true.)) :: res
      real(kind=kind(1.0d0)), dimension(3) :: pm

      res = no_of_same_principal_moments_(self,pm)==1

   end function

   function is_prolate_top(self,pm) result(res)
    type(atom_type), dimension(:) :: self
    ! Return .true. if the geometry is a prolate top
    ! Needs principal moments of inertia "pm" after alignment
      logical(kind=kind(.true.)) :: res
      real(kind=kind(1.0d0)), dimension(3) :: pm

      res = is_symmetric_top_(self,pm) .and. (pm(3)<pm(1))

   end function

   function is_oblate_top(self,pm) result(res)
    type(atom_type), dimension(:) :: self
    ! Return .true. if the geometry is a oblate top
    ! Needs principal moments of inertia "pm" after alignment
      logical(kind=kind(.true.)) :: res
      real(kind=kind(1.0d0)), dimension(3) :: pm

      res = is_symmetric_top_(self,pm) .and. (pm(3)>pm(1))

   end function

   function is_asymmetric_top(self,pm) result(res)
    type(atom_type), dimension(:) :: self
    ! Return .true. if the geometry is a asymmetric top
    ! Needs principal moments of inertia "pm"
      logical(kind=kind(.true.)) :: res
      real(kind=kind(1.0d0)), dimension(3) :: pm

      res = no_of_same_principal_moments_(self,pm)==0

   end function

   function no_of_same_principal_moments(self,pm) result(same)
    type(atom_type), dimension(:) :: self
    ! Return the number of "same" pairs of principal moments of inertia.
    ! Needs principal moments of inertia "pm"
      integer(kind=kind(1)) :: same
      real(kind=kind(1.0d0)), dimension(3) :: pm
      integer(kind=kind(1)) :: i,j

      same = 0
      do i = 1,3
      do j = 1,i-1
         if (abs(pm(i)-pm(j))<=10.0d0**(-6)) then
            same = same + 1
         end if
      end do
      end do

   end function

   function has_sequence_numbers(self) result(res)
    type(atom_type), dimension(:) :: self
    ! Return .true. if any atom in the list has a non zero sequence number
      logical(kind=kind(.true.)) :: res

      res = any(self(:)%sequence_number > 1)

   end function

   function has_residue_names(self) result(res)
    type(atom_type), dimension(:) :: self
    ! Return .true. if any atom in the list has a residue name different from "UNK"
      logical(kind=kind(.true.)) :: res

      res = any(self(:)%residue_name /= "UNK" )

   end function

   function has_restraints(self) result(res)
    type(atom_type), dimension(:) :: self
    ! Return .true. if any atom in the list has a restrained position
    ! or restrained force constants
      logical(kind=kind(.true.)) :: res

      res = any(self(:)%restraining_force_constant/=0.0d0)

   end function

   function molecular_weight(self) result(res)
    type(atom_type), dimension(:) :: self
    ! Return the molceular weight for this atomvec
      real(kind=kind(1.0d0)) :: res
      integer(kind=kind(1)) :: a

      res = 0.0d0
      do a = 1,n_atom_(self)
         res = res + mass_(self(a))
      end do

   end function

   function centre_of_atoms(self,axes) result(centre)
    type(atom_type), dimension(:) :: self
    ! Return the centroid of the atom positions in "centre". If "axes" is present
    ! then the "centre" is expressed with respect to the new "axes", where the
    ! columns of "axes" are the coordinates of the new axes in terms of the old.
      real(kind=kind(1.0d0)), dimension(3) :: centre
      real(kind=kind(1.0d0)), dimension(3,3), optional :: axes
      integer(kind=kind(1)) :: a

      centre = 0.0d0
      do a = 1,n_atom_(self)
         centre = centre + self(a)%pos
      end do
      centre = centre/n_atom_(self)
      if (present(axes)) centre = matmul(transpose(axes),centre)

   end function

   function bounding_cube_width(self) result(width)
    type(atom_type), dimension(:) :: self
    ! Return "width" which is a width of a side of a cube in which the molecule
    ! nicely sits.
    ! Suitable for generating plot widths.
      real(kind=kind(1.0d0)) :: width
      real(kind=kind(1.0d0)), dimension(3) :: centre,dist
      real(kind=kind(1.0d0)) :: length
      integer(kind=kind(1)) :: a

      width = 0.0d0
      centre = centre_of_atoms_(self)
      do a = 1,n_atom_(self)
         dist = self(a)%pos - centre
         length = norm_(dist) + bragg_slater_radius_(self(a))*(1/0.52917724924d0)*2.0d0
         length = max(length, norm_(dist)*2.0d0)
         width = max(width,length)
      end do
      width = 2.0d0*width

   end function

   function bounding_box(self,axes) result(box)
    type(atom_type), dimension(:) :: self
    ! Return "box" which are three widths of a side of a box in which the molecule
    ! nicely sits. If "axes" is present, the "box" coordinates are expressed in
    ! terms of the new "axes", where the columns of "axes" are the coordinates of
    ! the new axes in terms of the old. These "axes" may be (typically) the
    ! principal moment axes. This routine is suitable for generating plot widths.
      real(kind=kind(1.0d0)), dimension(3,3), optional :: axes
      real(kind=kind(1.0d0)), dimension(3) :: box
      real(kind=kind(1.0d0)), dimension(3) :: centre,dist
      integer(kind=kind(1)) :: a

      box = 0.0d0
      centre = centre_of_atoms_(self)
      do a = 1,n_atom_(self)
         dist = self(a)%pos - centre
         dist = abs(dist)
         if (present(axes)) then
         dist = matmul(transpose(axes),dist)  ! coordinates in new axis system
         dist = abs(dist)
         end if
         dist = dist + bragg_slater_radius_(self(a))*(1/0.52917724924d0)*2.0d0
         box  = max(box,dist)
      end do
      box = 4.0d0*box

   end function

   subroutine make_shape_tensor(self,st)
    type(atom_type), dimension(:) :: self
    ! Make the shape tensor "st" wrt the centre of atoms. This is the same as the
    ! moment of inertia tensor except that each atom is assumed to have unit mass.
      real(kind=kind(1.0d0)), dimension(3,3) :: st
      real(kind=kind(1.0d0)), dimension(3,3) :: m
      real(kind=kind(1.0d0)), dimension(3) :: c,r
      integer(kind=kind(1)) :: a

      c = centre_of_atoms_(self)
      st = 0.0d0
      do a = 1, n_atom_(self)
         r = self(a)%pos - c
         m = spread(r,dim=1,ncopies=3)*spread(r,dim=2,ncopies=3)
         call add_to_diagonal_(m,-trace_(m))
         st = st - m
      end do

   end subroutine

   subroutine make_shape_moments(self,sm,sa)
    type(atom_type), dimension(:) :: self
    ! Make the shape moments "sm" and principal shape axes "sa" wrt the centre of
    ! atoms. The shape axes are the same as the principal moment of inertia axes
    ! except that each atom is assumed to have unit mass. The principal shape
    ! axes are made right handed.
      real(kind=kind(1.0d0)), dimension(3) :: sm
      real(kind=kind(1.0d0)), dimension(3,3) :: sa
      real(kind=kind(1.0d0)), dimension(3,3) :: st

      call make_shape_tensor_(self,st)
      call solve_eigenproblem_(st,sm,sa)
      if (abs(sa(1,2)) > abs(sa(1,1))) then  ! make axes are close to x,y,z
         call swap_columns_(sa,1,2)
         call swap_elements_(sm,1,2)
      end if
      if (abs(sa(1,3)) > abs(sa(1,1))) then
         call swap_columns_(sa,1,3)
         call swap_elements_(sm,1,3)
      end if
      if (abs(sa(2,3)) > abs(sa(2,2))) then
         call swap_columns_(sa,2,3)
         call swap_elements_(sm,2,3)
      end if
      if (sa(1,1)<0.0d0)        sa(:,1) = -sa(:,1)   ! make axes +ve
      if (sa(2,2)<0.0d0)        sa(:,2) = -sa(:,2)
      if (determinant_(sa)<0.0d0) sa(:,3) = -sa(:,3)

   end subroutine

   subroutine get_geometry(self,g)
    type(atom_type), dimension(:) :: self
    ! Return the geometry "g" in a matrix
      intent(in) :: self
      real(kind=kind(1.0d0)), dimension(:,:), intent(out) :: g
      integer(kind=kind(1)) :: n_atom,n

      if (size(g,1)==3 .and. size(g,2)==n_atom_(self)) then
         n_atom = size(g,2)
         do n = 1,n_atom
            g(:,n) = self(n)%pos
         end do
      else if (size(g,1)==n_atom_(self) .and. size(g,2)==3) then
         n_atom = size(g,1)
         do n = 1,n_atom
            g(n,:) = self(n)%pos
         end do
      else
         call die_(tonto,"ATOMVEC:get_geometry ... incorrectly sized geometry array")
      end if

   end subroutine

   subroutine get_geometry_vector(self,g)
    type(atom_type), dimension(:) :: self
    ! Return the geometry "g" as a flat vector where the xyz positions increment
    ! fastest, useful for optimisations
      real(kind=kind(1.0d0)), dimension(:) :: g
      integer(kind=kind(1)) :: k,i

      call ensure_(tonto,size(g)==3*n_atom_(self),"ATOMVEC:get_geometry_vector ... wrong size, g")
      k = 0
      do i = 1,n_atom_(self)
        g(k+1) = self(i)%pos(1)
        g(k+2) = self(i)%pos(2)
        g(k+3) = self(i)%pos(3)
        k = k + 3
      end do

   end subroutine

   subroutine set_geometry_from_vector(self,g)
    type(atom_type), dimension(:) :: self
    ! Set the geometry from "g", a flat vector, where the xyz positions
    ! increment fastest
      real(kind=kind(1.0d0)), dimension(:) :: g
      integer(kind=kind(1)) :: k,i

      call ensure_(tonto,size(g)==3*n_atom_(self),"ATOMVEC:set_geometry_from_vector ... wrong size, g")
      k = 0
      do i = 1,n_atom_(self)
        self(i)%pos(1) = g(k+1)
        self(i)%pos(2) = g(k+2)
        self(i)%pos(3) = g(k+3)
        k = k + 3
      end do

   end subroutine

   function geometry(self) result(g)
    type(atom_type), dimension(:) :: self
    ! Return the geometry "g" in a (3 x .n_atom) matrix
       real(kind=kind(1.0d0)), dimension(3,size(self)) :: g
      integer(kind=kind(1)) :: n_atom,n

      n_atom = size(self)
      do n = 1,n_atom
         g(:,n) = self(n)%pos
      end do

   end function

   function geometry_vector(self) result(g)
    type(atom_type), dimension(:) :: self
    ! Return the geometry "g" as a flat vector where the xyz positions increment
    ! fastest, useful for optimisations
      real(kind=kind(1.0d0)), dimension(3*size(self)) :: g
      integer(kind=kind(1)) :: k,i

      k = 0
      do i = 1,n_atom_(self)
        g(k+1) = self(i)%pos(1)
        g(k+2) = self(i)%pos(2)
        g(k+3) = self(i)%pos(3)
        k = k + 3
      end do

   end function

   function nuclear_dipole_moment(self) result(res)
    type(atom_type), dimension(:) :: self
    ! Return the dipole moment obtained from the nuclear charges
      real(kind=kind(1.0d0)), dimension(3) :: res
       integer(kind=kind(1)) :: a

      res = 0.0d0
      do a = 1,n_atom_(self)
         res = res + self(a)%atomic_number*self(a)%pos(:)
      end do

   end function

   function nuclear_quadrupole_moment(self) result(res)
    type(atom_type), dimension(:) :: self
    ! Return the quadrupole moment obtained from the nuclear charges
    ! as a vector, in the order: xx, yy, zz, xy, xz, yz
      real(kind=kind(1.0d0)), dimension(6) :: res
       real(kind=kind(1.0d0)) :: Z
       integer(kind=kind(1)) :: a

      res = 0.0d0
      do a = 1,n_atom_(self)
         Z = self(a)%atomic_number
         res(1) = res(1) + Z*self(a)%pos(1)*self(a)%pos(1)
         res(2) = res(2) + Z*self(a)%pos(2)*self(a)%pos(2)
         res(3) = res(3) + Z*self(a)%pos(3)*self(a)%pos(3)
         res(4) = res(4) + Z*self(a)%pos(1)*self(a)%pos(2)
         res(5) = res(5) + Z*self(a)%pos(1)*self(a)%pos(3)
         res(6) = res(6) + Z*self(a)%pos(2)*self(a)%pos(3)
      end do

   end function

   function nuclear_octupole_moment(self) result(res)
    type(atom_type), dimension(:) :: self
    ! Return the octupole moment obtained from the nuclear charges as a vector,
    ! in the order: xxx, yyy, zzz, xxy, xxz, yyx, yyz, zzx, zzy, xyz
      real(kind=kind(1.0d0)), dimension(10) :: res
      real(kind=kind(1.0d0)) :: Z
      integer(kind=kind(1)) :: a

      res = 0.0d0
      do a = 1,n_atom_(self)
         Z = self(a)%atomic_number
         res(1)  = res(1)  + Z*self(a)%pos(1)*self(a)%pos(1)*self(a)%pos(1)
         res(2)  = res(2)  + Z*self(a)%pos(2)*self(a)%pos(2)*self(a)%pos(2)
         res(3)  = res(3)  + Z*self(a)%pos(3)*self(a)%pos(3)*self(a)%pos(3)
         res(4)  = res(4)  + Z*self(a)%pos(1)*self(a)%pos(1)*self(a)%pos(2)
         res(5)  = res(5)  + Z*self(a)%pos(1)*self(a)%pos(1)*self(a)%pos(3)
         res(6)  = res(6)  + Z*self(a)%pos(2)*self(a)%pos(2)*self(a)%pos(1)
         res(7)  = res(7)  + Z*self(a)%pos(2)*self(a)%pos(2)*self(a)%pos(3)
         res(8)  = res(8)  + Z*self(a)%pos(3)*self(a)%pos(3)*self(a)%pos(1)
         res(9)  = res(9)  + Z*self(a)%pos(3)*self(a)%pos(3)*self(a)%pos(2)
         res(10) = res(10) + Z*self(a)%pos(1)*self(a)%pos(2)*self(a)%pos(3)
      end do

   end function

   function nuclear_E_field_at_nuclei(self) result(res)
    type(atom_type), dimension(:) :: self
    ! Return the nuclear contribution to the electric fields
    ! at the nuclei as a (3 x .n_atom) array
      real(kind=kind(1.0d0)), dimension(3,size(self)) :: res
      real(kind=kind(1.0d0)) :: Z,r
      integer(kind=kind(1)) :: a,b
      real(kind=kind(1.0d0)), dimension(3) :: ab

      res = 0.0d0
      do a = 1,n_atom_(self)
         do b = 1,n_atom_(self)
            if (b==a) cycle
            Z  = self(b)%atomic_number
            ab = self(a)%pos - self(b)%pos
            r  = norm_(ab)
            res(:,a) = res(:,a) - Z*ab/(r*r*r)
         end do
      end do

   end function

   function nuclear_EFG_at_nuclei(self) result(res)
    type(atom_type), dimension(:) :: self
    ! Return the nuclear contribution to the electric fields gradient (EFG)
    ! at the nuclei as a (6 x .n_atom) array
      real(kind=kind(1.0d0)), dimension(6,size(self)) :: res
      real(kind=kind(1.0d0)) :: Z,r,r3,r5
      integer(kind=kind(1)) :: a,b
      real(kind=kind(1.0d0)), dimension(3) :: ab

      res = 0.0d0
      do a = 1,n_atom_(self)
         do b = 1,n_atom_(self)
            if (b==a) cycle
            Z  = self(b)%atomic_number
            ab = self(a)%pos - self(b)%pos
            r  = norm_(ab)
            r3 = r*r*r
            r5 = r3*r*r
            res(1,a) = res(1,a) + Z * (3.0d0*ab(1)*ab(1)/r5 - 1.0d0/r3)
            res(2,a) = res(2,a) + Z * (3.0d0*ab(2)*ab(2)/r5 - 1.0d0/r3)
            res(3,a) = res(3,a) + Z * (3.0d0*ab(3)*ab(3)/r5 - 1.0d0/r3)
            res(4,a) = res(4,a) + Z * (3.0d0*ab(1)*ab(2)/r5)
            res(5,a) = res(5,a) + Z * (3.0d0*ab(1)*ab(3)/r5)
            res(6,a) = res(6,a) + Z * (3.0d0*ab(2)*ab(3)/r5)
         end do
      end do

   end function

   function has_all_ANO_data(self) result(has)
    type(atom_type), dimension(:) :: self
    ! Return .true. if all atom ANO data exists
      intent(in) :: self
      logical(kind=kind(.true.)) :: has
      integer(kind=kind(1)) :: a

      has = .true.
      do a = 1,n_atom_(self)
         has = has .and. has_ANO_data_(self(a))
         if (.not. has) exit
      end do

   end function

!  ************************
!  Atom information methods
!  ************************

   function chemical_symbols(self) result(res)
    type(atom_type), dimension(:) :: self
    ! Return an array of the chemical symbols for each atom
      character(128), dimension(size(self)) :: res
       integer(kind=kind(1)) :: a

      do a = 1,n_atom_(self)
         res(a) = chemical_symbol_(self(a))
      end do

   end function

   function numbered_chemical_symbols(self) result(res)
    type(atom_type), dimension(:) :: self
    ! Return an array of the chemical symbols for each atom with a number
    ! at the end in brackets
      character(128), dimension(size(self)) :: res
       integer(kind=kind(1)) :: a

      do a = 1,n_atom_(self)
         res(a) = chemical_symbol_(self(a))
         res(a) = trim(res(a))//"("//trim(to_str_(a))//")"
      end do

   end function

   function basis_labels(self) result(labels)
    type(atom_type), dimension(:) :: self
    ! Return a list of basis set "labels". Missing labels are returned blank.
      character(128), dimension(:), pointer :: labels
      integer(kind=kind(1)) :: i

      call create_(labels,size(self))
      do i = 1,size(self)
         if (self(i)%basis_label/=" ") then; labels(i) = self(i)%basis_label
         else;                               labels(i) = " "
         end if
      end do

   end function

   function library_basis_labels(self,suffix) result(labels)
    type(atom_type), dimension(:) :: self
    ! Return a list of library basis set labels. The label is either the atoms
    ! own .basis_label (if it contains the colon character, the indicator of a
    ! library basis set), or else it is the atoms element name with ":suffix"
    ! appended to it. Only a unique list of basis labels is returned.
      character(128) :: suffix
      character(128), dimension(:), pointer :: labels
      integer(kind=kind(1)) :: i

      call create_(labels,size(self))
      do i = 1,size(self)
         if (includes_(self(i)%basis_label,":")) then
            labels(i) = self(i)%basis_label
         else
            labels(i) = library_basis_label_(self(i),suffix)
         end if
      end do
      call remove_repetitions_(labels)

   end function

   function groups_defined(self) result(res)
    type(atom_type), dimension(:) :: self
    ! Return .true. if atom groups have been defined
      logical(kind=kind(.true.)) :: res

      if (any(self(:)%group>0)) then; res = .true.
      else;                           res = .false.
      end if

   end function

   function atom_index_from_pos(self,pos) result(res)
    type(atom_type), dimension(:) :: self
    ! Return the index of atom from its position "pos"
      real(kind=kind(1.0d0)), dimension(3) :: pos
      integer(kind=kind(1)) :: res
      integer(kind=kind(1)) :: a
      logical(kind=kind(.true.)) :: found

      found = .false.
      do a = 1,n_atom_(self)
         found = same_as_(pos,self(a)%pos)
         if (found) exit
      end do
      res = a
      call ensure_(tonto,found,"ATOMVEC:atom_index_from_pos ... no atom for this position")

   end function

   subroutine make_atom_kind_count(self,cnt,n_kind)
    type(atom_type), dimension(:) :: self
    ! Return an array "cnt" in which each element represents an atom, and the
    ! value of that array element is a count of the number of atoms of that kind.
    ! Later atoms (i.e. array elements) which are of the same kind as earlier
    ! atoms have a count of zero. The number of different kinds is returned in
    ! "n_kind".
      integer(kind=kind(1)), dimension(:), pointer :: cnt
      integer(kind=kind(1)) :: n_kind
      integer(kind=kind(1)) :: n,l,n_atom

      n_atom = n_atom_(self)
      nullify(cnt); call create_(cnt,n_atom)
      cnt = 1
      do n = 1,n_atom
         if (cnt(n)==0) cycle
         do l = n+1,n_atom
            if (cnt(l)==0) cycle
            if ( same_kind_of_atoms_(self,l,n) ) then
               cnt(n) = cnt(n)+1
               cnt(l) = 0
            end if
         end do
      end do
      n_kind = n_atom_(self) - count(cnt==0)

   end subroutine

   subroutine make_atom_kind_list(self,atom_kind)
    type(atom_type), dimension(:) :: self
    ! Make the atom kind list ... atom_kind(k).element(c) is the c-th atom of the
    ! same kind as atom k, which is a unique kind.
      type(intvec__type), dimension(:), pointer :: atom_kind
      integer(kind=kind(1)), dimension(:), pointer :: cnt
      integer(kind=kind(1)) :: n,l,k,c,n_atom

      n_atom = n_atom_(self)
      call make_atom_kind_count_(self,cnt,n)
      nullify(atom_kind)
      call create_(atom_kind,n)
      k = 0
      do n = 1,n_atom
         if (cnt(n)==0) cycle
         k = k+1
         c = 1
         call create_(atom_kind(k)%element,cnt(n))
         atom_kind(k)%element(c) = n
         do l = n+1,n_atom
            if ( same_kind_of_atoms_(self,l,n) ) then
               c = c+1
               atom_kind(k)%element(c) = l
            end if
         end do
      end do
      call destroy_(cnt)

   end subroutine

   subroutine make_atom_kind_list_1(self,atom_kind,n_kind)
    type(atom_type), dimension(:) :: self
    ! Make a different atom kind list ... atom_kind(k) is the kind index of the
    ! k-th atom. (Same effect as the make_atom_kind_map routine).
      integer(kind=kind(1)), dimension(:) :: atom_kind
      integer(kind=kind(1)) :: n_kind
      integer(kind=kind(1)) :: n_atom,n,l

   call ensure_(tonto,size(atom_kind)==size(self),"ATOMVEC:make_atom_kind_list_1 ... atom_kind is incorrectly dimensioned")
      n_atom = size(self)
      atom_kind = (/ (n, n=1,n_atom) /)
      n_kind = 0
      do n = 1,n_atom
         if (atom_kind(n)<=n_kind) cycle
         n_kind = n_kind + 1
         atom_kind(n) = n_kind
         do l = n+1,n_atom
            if (atom_kind(l)<=n_kind) cycle
            if ( .not. same_kind_of_atoms_(self,l,n) ) cycle
            atom_kind(l) = n_kind
         end do
      end do

   end subroutine

   subroutine make_unique_atom_list(self,unique_atom)
    type(atom_type), dimension(:) :: self
    ! "unique_atom(k)" is the index of the first atom which represents all the
    ! the atoms which are of the same kind, k.
      integer(kind=kind(1)), dimension(:) :: unique_atom
      integer(kind=kind(1)), dimension(:), pointer :: atom_kind
      integer(kind=kind(1)) :: n_kind,k,pos

      call create_(atom_kind,size(self))
      call make_atom_kind_list_(self,atom_kind,n_kind)
      call ensure_(tonto,size(unique_atom)==n_kind,"ATOMVEC:make_unique_atom_list ... unique atom incorrectly dimensioned")
      do k = 1,n_kind
         pos = index_of_value_(atom_kind,k)
         call ensure_(tonto,pos/=0,"ATOMVEC:make_unique_atom_list ... atom kind "// trim(to_str_(k)) //" does not exist!")
         unique_atom(k) = pos
      end do
      call destroy_(atom_kind)

   end subroutine

   subroutine make_unique_atom_list_1(self,unique_atom,atom_kind,n_kind)
    type(atom_type), dimension(:) :: self
    ! "unique_atom(k)" is the index of the first atom which represents all the
    ! the atoms which are of the same kind, k. The atom kinds for each atom "a"
    ! are given by "atom_kind(a)" ...
      integer(kind=kind(1)), dimension(:), pointer :: unique_atom,atom_kind
      integer(kind=kind(1)) :: n_kind
      integer(kind=kind(1)) :: k,pos

      call create_(atom_kind,size(self))
      call make_atom_kind_list_(self,atom_kind,n_kind)
      call create_(unique_atom,n_kind)
      do k = 1,n_kind
         pos = index_of_value_(atom_kind,k)
         call ensure_(tonto,pos/=0,"ATOMVEC:make_unique_atom_list_1 ... atom kind "// trim(to_str_(k)) //" does not exist!")
         unique_atom(k) = pos
      end do

   end subroutine

   subroutine make_atom_kind_map(self,map)
    type(atom_type), dimension(:) :: self
    ! Make an atom kind array "map", where map(a) is the unique atom kind
    ! corresponding to atom index a.
      integer(kind=kind(1)), dimension(:), pointer :: map
      type(intvec__type), dimension(:), pointer :: atom_kind
      integer(kind=kind(1)) :: n_k,k,kk

      call create_(map,size(self))
      call make_atom_kind_list_(self,atom_kind)
      n_k = size(atom_kind)
      do k  = 1,n_k
      do kk = 1,size(atom_kind(k)%element)
         map(atom_kind(k)%element(kk)) = k
      end do
      end do
      call destroy_(atom_kind)

   end subroutine

!  *************************
!  Shell information methods
!  *************************

   subroutine get_shell_limits(self,s,first,last)
    type(atom_type), dimension(:) :: self
    ! Get the shell function limits "first" and "last" for atomvec shell
    ! number "s"
      integer(kind=kind(1)) :: s,first,last
      integer(kind=kind(1)) :: a,as,n,ss

      ss = 0; last = 0
      atom_loop: do a = 1,n_atom_(self)
         n = self(a)%basis%n_shell
         do as = 1,n
            ss = ss + 1
            first = last+1
            last  = first + self(a)%basis%shell(as)%n_comp - 1
            if (s==ss) exit atom_loop
         end do
      end do atom_loop

   end subroutine

   subroutine get_shell_limits_1(self,first,last)
    type(atom_type), dimension(:) :: self
    ! Get the shell function limit vectors "first" and "last" for corresponding
    ! to the vector of atomvec shell number
      integer(kind=kind(1)), dimension(:), pointer :: first,last
      integer(kind=kind(1)) :: a,as,n,ss,f,l

      nullify(first); call create_(first,n_shell_(self))
      nullify(last);  call create_(last,n_shell_(self))
      ss = 0; l = 0
      do a = 1,n_atom_(self)
         n = self(a)%basis%n_shell
         do as = 1,n
            ss = ss + 1
            f = l + 1
            l = f + self(a)%basis%shell(as)%n_comp - 1
            first(ss) = f
            last(ss)  = l
         end do
      end do

   end subroutine

   subroutine make_atom_basis_fn_limits(self,first,last)
    type(atom_type), dimension(:) :: self
    ! Get the first and last basis functions for the atoms
      integer(kind=kind(1)), dimension(:), pointer :: first,last
      integer(kind=kind(1)) :: a,as,n,l

      nullify(first); call create_(first,size(self))
      nullify(last);  call create_(last,size(self))
      l = 0
      do a = 1,n_atom_(self)
         n = self(a)%basis%n_shell
         first(a) = l + 1
         do as = 1,n
            l = l + self(a)%basis%shell(as)%n_comp
         end do
         last(a) = l
      end do

   end subroutine

!  ******************************
!  Atom-shell information methods
!  ******************************

   function atom_for_shell(self,s) result(a)
    type(atom_type), dimension(:) :: self
    ! Return the *atom* number "a" corresponding to the
    ! *atomvec* shell number "s"
      integer(kind=kind(1)) :: a,s
      integer(kind=kind(1)) :: ss,n

      ss = 0
      do a = 1,n_atom_(self)
         n = self(a)%basis%n_shell
         ss = ss + n
         if (s<=ss) exit
      end do

   end function

   function atom_for_shell_1(self) result(res)
    type(atom_type), dimension(:) :: self
    ! Return a vector of *atom* numbers corresponding to the
    ! vector of *atomvec* shell numbers
      integer(kind=kind(1)), dimension(:), pointer :: res
      integer(kind=kind(1)) :: a,ss,n

      nullify(res); call create_(res,n_shell_(self))
      ss = 0
      do a = 1,n_atom_(self)
         n = self(a)%basis%n_shell
         res(ss+1:ss+n) = a
         ss = ss + n
      end do

   end function

   function atom_shell_for_shell(self,s) result(as)
    type(atom_type), dimension(:) :: self
    ! Return the *atom* shell number "as" corresponding to the
    ! *atomvec* shell number "s"
      integer(kind=kind(1)) :: as,s
      integer(kind=kind(1)) :: a,ss,n

      ss = 0
      do a = 1,n_atom_(self)
         n = self(a)%basis%n_shell
         ss = ss + n
         if (s<=ss) exit
      end do
      as = s - ss + n

   end function

   function atom_shell_for_shell_1(self) result(res)
    type(atom_type), dimension(:) :: self
    ! Return a vector of *atom* shell numbers corresponding to the
    ! *atomvec* shell number vector
      integer(kind=kind(1)), dimension(:), pointer :: res
      integer(kind=kind(1)) :: a,ss,n,as

      nullify(res); call create_(res,n_shell_(self))
      ss = 0
      do a = 1,n_atom_(self)
         n = self(a)%basis%n_shell
         do as = 1,n
            res(ss+as) = as
         end do
         ss = ss + n
      end do

   end function

   function first_shell_for_atom(self,a) result(res)
    type(atom_type), dimension(:) :: self
    ! Return the index of the first shell for atom "a" in the atomvec
      integer(kind=kind(1)) :: a,res
      integer(kind=kind(1)) :: at,n

      res = 1
      do at = 1,(a-1)
         n = self(a)%basis%n_shell
         res = res + n
      end do

   end function

   subroutine make_shell_for_atom_limits(self,first,last)
    type(atom_type), dimension(:) :: self
    ! Return the indices of the first and last shell for each atom in the atomvec
      integer(kind=kind(1)), dimension(:) :: first,last
      integer(kind=kind(1)) :: ss,a,n

      ss = 0
      do a = 1,n_atom_(self)
         first(a) = ss + 1
         n = self(a)%basis%n_shell
         ss = ss + n
         last(a) = ss
      end do

   end subroutine

   function first_shell_for_atom_1(self) result(res)
    type(atom_type), dimension(:) :: self
    ! Return the indices of the first shell for an atom in the atomvec
      integer(kind=kind(1)), dimension(:), pointer :: res
      integer(kind=kind(1)) :: ss,a,n

      nullify(res); call create_(res,size(self))
      ss = 0
      do a = 1,n_atom_(self)
         n = self(a)%basis%n_shell
         res(a) = ss + 1
         ss = ss + n
      end do

   end function

   function same_kind_of_atoms(self,a,b) result(res)
    type(atom_type), dimension(:) :: self
    ! Return true if atoms "a" and "b" are the same kind
      integer(kind=kind(1)) :: a,b
      logical(kind=kind(.true.)) :: res

      res = same_kind_as_(self(a),self(b))

   end function

   function bonded(self,a,b,scale_factor) result(res)
    type(atom_type), dimension(:) :: self
    ! Return true if atoms "a" and "b" are bonded atoms.
    ! If present, "scale_factor" is used to determine a multiple
    ! of the sum of the Bragg-Slater radii within which the atoms
    ! are regarded to be bonded.
      integer(kind=kind(1)) :: a,b
      real(kind=kind(1.0d0)), optional :: scale_factor
      logical(kind=kind(.true.)) :: res
      real(kind=kind(1.0d0)) :: bond_max,fac

      fac = 1.15
      if (present(scale_factor)) fac = scale_factor
      bond_max = fac*(bragg_slater_radius_(self(a)) + bragg_slater_radius_(self(b)))
      bond_max = bond_max*(1/0.52917724924d0)
      res = are_nearby_(self,a,b,bond_max)

   end function

   function are_nearby(self,a,b,dist) result(res)
    type(atom_type), dimension(:) :: self
    ! Return whether atoms "a" and "b" are nearby, i.e. within length "dist".
      integer(kind=kind(1)) :: a,b
      real(kind=kind(1.0d0)) :: dist
      logical(kind=kind(.true.)) :: res
      real(kind=kind(1.0d0)), dimension(3) :: tmp
      real(kind=kind(1.0d0)) :: r2

      tmp = self(a)%pos - self(b)%pos
      tmp = abs(tmp)
      if      (tmp(1)>dist) then
         res = .false.
      else if (tmp(2)>dist) then
         res = .false.
      else if (tmp(3)>dist) then
         res = .false.
      else
         r2 = dot_product(tmp,tmp)
         res = (r2 < dist*dist)
      end if

   end function

   function connected(self,a,b,c,d) result(res)
    type(atom_type), dimension(:) :: self
    ! Return true if atoms "a" "b" "c" and "d" are connected together
      integer(kind=kind(1)) :: a,b,c,d
      logical(kind=kind(.true.)) :: res

      res = bonded_(self,a,b) .or. bonded_(self,a,c) .or. bonded_(self,a,d)
      res = res .and. ( bonded_(self,b,a) .or. bonded_(self,b,c) .or. bonded_(self,b,d) )
      res = res .and. ( bonded_(self,c,a) .or. bonded_(self,c,b) .or. bonded_(self,c,d) )
      res = res .and. ( bonded_(self,d,a) .or. bonded_(self,d,b) .or. bonded_(self,d,c) )

   end function

   function no_of_bonds(self) result(res)
    type(atom_type), dimension(:) :: self
    ! Return the number of bonded atoms
      integer(kind=kind(1)) :: res
      integer(kind=kind(1)) :: a,b

      res = 0
      do a = 1,n_atom_(self)
      do b = 1,a-1
         if (bonded_(self,a,b)) res = res + 1
      end do
      end do

   end function

!   bond_distance(a,b) result(res)
!   ! Return the bond distance between atoms "a" and "b"
!      a,b :: integer(kind=kind(1))
!      res :: real(kind=kind(1.0d0))
!      res = self(a).pos.distance_to(self(b).pos)
!   end

   function bond_distance(self,a,b) result(res)
    type(atom_type), dimension(:) :: self
    ! Return the bond distance between atoms "a" and "b"
      integer(kind=kind(1)) :: a,b
      real(kind=kind(1.0d0)) :: res
      real(kind=kind(1.0d0)), dimension(3) :: tmp

      tmp = self(a)%pos - self(b)%pos
      res = sqrt(dot_product(tmp,tmp))
!      res = self(a).pos.distance_to(self(b).pos)

   end function

   function no_of_angles(self) result(res)
    type(atom_type), dimension(:) :: self
    ! Return the number of angles within bond contact distance
      integer(kind=kind(1)) :: res
      integer(kind=kind(1)) :: n,a,b,c

      n = n_atom_(self)
      res = 0
      do a = 1,n
      do b = 1,n
         if (a==b) cycle
         if (.not. bonded_(self,a,b)) cycle
         do c = 1,n
            if (a==c .or. b==c) cycle
            if (.not. bonded_(self,b,c)) cycle
            res = res + 1
         end do
      end do
      end do
      res = res/2

   end function

   function bond_angle(self,a,b,c,degrees) result(res)
    type(atom_type), dimension(:) :: self
    ! Return the bond angle between atoms "a" , "b" and "c".
    ! The central atom is "b".
    ! If "degrees" is present and .true., result is in degrees.
      integer(kind=kind(1)) :: a,b,c
      logical(kind=kind(.true.)), optional :: degrees
      real(kind=kind(1.0d0)) :: res
      real(kind=kind(1.0d0)), dimension(3) :: rba,rbc
      logical(kind=kind(.true.)) :: change

      rba = self(a)%pos - self(b)%pos
      rbc = self(c)%pos - self(b)%pos
      call normalise_(rba)
      call normalise_(rbc)
      res = dot_(rba,rbc)
      res = arccos_(res)
      change = .false.
      if (present (degrees)) change = degrees
      if (change) res = (180/3.141592653589793d0)*res

   end function

   function no_of_torsion_angles(self) result(res)
    type(atom_type), dimension(:) :: self
    ! Return the number of torsion angles within bond contact distance
      integer(kind=kind(1)) :: res
      integer(kind=kind(1)) :: n,a,b,c,d

      n = n_atom_(self)
      res = 0
      do a = 1,n
      do b = 1,n
         if (a==b) cycle
         if (.not. bonded_(self,a,b)) cycle
         do c = 1,n
            if (a==c .or. b==c) cycle
            if (.not. bonded_(self,b,c)) cycle
            do d = 1,n
               if (a==d .or. b==d .or. c==d) cycle
               if (.not. bonded_(self,c,d)) cycle
               res = res + 1
            end do
         end do
      end do
      end do
      res = res/2

   end function

   function torsion_angle(self,a,b,c,d,abc_colinear,bcd_colinear,degrees) result(res)
    type(atom_type), dimension(:) :: self
    ! Return the torsion angle between atoms "a", "b", "c" and "d". The atoms are
    ! assumed connected like a--b--c--d and the angle returned is that between
    ! vectors (a-b) and (d-c) i.e. the torsion angle looking down the b--c bond.
    ! If "degrees" is present and .true., result is in degrees.
    ! NOTE *** If the result is -1.0d0, either a--b--c or b--c--d are colinear, and
    ! the variables "abc_colinear" and "bcd_colinear" are set.
      integer(kind=kind(1)) :: a,b,c,d
      logical(kind=kind(.true.)), optional :: abc_colinear,bcd_colinear
      logical(kind=kind(.true.)), optional :: degrees
      real(kind=kind(1.0d0)) :: res
      logical(kind=kind(.true.)) :: change
      real(kind=kind(1.0d0)), dimension(3) :: tcd,tba,rba,rcd,rbc

      if (present(abc_colinear)) abc_colinear = .false.
      if (present(bcd_colinear)) bcd_colinear = .false.
      rba = self(a)%pos - self(b)%pos
      rcd = self(d)%pos - self(c)%pos
      rbc = self(c)%pos - self(b)%pos
      call to_cross_product_(tcd,rcd,rbc)
      call to_cross_product_(tba,rba,rbc)
      res = 0.0d0
      if (abs( norm_(tba))<10.0d0**(-5)) then
         res = -1.0d0
         if (present(abc_colinear)) abc_colinear = .true.
      end if
      if (abs( norm_(tcd))<10.0d0**(-5)) then
         res = -1.0d0
         if (present(bcd_colinear)) bcd_colinear = .true.
      end if
      if (res<0) then;   return; end if
      call normalise_(tba)
      call normalise_(tcd)
      res = dot_(tba,tcd)
      res = arccos_(res)
      change = .false.
      if (present (degrees)) change = degrees
      if (change) res = (180/3.141592653589793d0)*res

   end function

!  ************************
!  Size information methods
!  ************************

   pure function n_atom(self) result(res)
    type(atom_type), dimension(:) :: self
    ! Return the number of atoms in the atom vector
      intent(in) :: self
      integer(kind=kind(1)) :: res
      res = size(self)

   end function

   pure function n_e(self) result(res)
    type(atom_type), dimension(:) :: self
    ! Work out and return the number of electrons in the atomvec assuming
    ! that it is neutrally charged.
      intent(in) :: self
      integer(kind=kind(1)) :: res
       integer(kind=kind(1)) :: a
      res = 0
      do a = 1,n_atom_(self)
         res = res + self(a)%atomic_number
      end do

   end function

   pure function no_of_shells(self) result(res)
    type(atom_type), dimension(:) :: self
    ! Work out and return the number of gaussian shells in the basis set for the
    ! molecule
      intent(in) :: self
      integer(kind=kind(1)) :: res
      integer(kind=kind(1)) :: a
      res = 0
      do a = 1,n_atom_(self)
         res = res + no_of_shells_(self(a))
      end do

   end function

   pure function n_shell(self) result(res)
    type(atom_type), dimension(:) :: self
    ! Work out and return the number of gaussian shells in the basis set for the
    ! molecule
      intent(in) :: self
      integer(kind=kind(1)) :: res
      integer(kind=kind(1)) :: a
      res = 0
      do a = 1,n_atom_(self)
         res = res + n_shell_(self(a))
      end do

   end function

   pure function n_shell_pairs(self) result(res)
    type(atom_type), dimension(:) :: self
    ! Return the number of shell pairs in the basis set for the molecule
      intent(in) :: self
      integer(kind=kind(1)) :: res
      integer(kind=kind(1)) :: n_shell

      n_shell = n_shell_(self)
      res = n_shell*(n_shell+1)/2

   end function

   pure function n_shell_for_atom(self,i) result(res)
    type(atom_type), dimension(:) :: self
    ! Work out and return the number of gaussian shells in the basis set for the
    ! molecule
      intent(in) :: self
       integer(kind=kind(1)), intent(in) :: i
      integer(kind=kind(1)) :: res
      res = self(i)%basis%n_shell

   end function

   pure function no_of_basis_functions(self) result(res)
    type(atom_type), dimension(:) :: self
    ! Work out and return the number of basis functions in the concatenated
    ! basis set for the atom list.
      intent(in) :: self
      integer(kind=kind(1)) :: res
      integer(kind=kind(1)) :: a
      res = 0
      do a = 1,n_atom_(self)
         res = res + no_of_basis_functions_(self(a))
      end do

   end function

   pure function n_bf(self) result(res)
    type(atom_type), dimension(:) :: self
    ! Work out and return the number of basis functions in the concatenated
    ! basis set for the atom list.
      intent(in) :: self
      integer(kind=kind(1)) :: res
       integer(kind=kind(1)) :: a
      res = 0
      do a = 1,n_atom_(self)
         res = res + n_bf_(self(a))
      end do

   end function

   pure function no_of_primitives(self) result(res)
    type(atom_type), dimension(:) :: self
    ! Work out and return the number of primitives in the basis set for the
    ! molecule
      intent(in) :: self
      integer(kind=kind(1)) :: res
       integer(kind=kind(1)) :: a
      res = 0
      do a = 1,n_atom_(self)
         res = res + no_of_primitives_(self(a))
      end do

   end function

   pure function n_prim(self) result(res)
    type(atom_type), dimension(:) :: self
    ! Work out and return the number of primitives in the basis set for the
    ! molecule
      intent(in) :: self
      integer(kind=kind(1)) :: res
       integer(kind=kind(1)) :: a
      res = 0
      do a = 1,n_atom_(self)
         res = res + n_prim_(self(a))
      end do

   end function

   function no_of_occupied_ANOs(self,ANOkind,tol) result(res)
    type(atom_type), dimension(:) :: self
    ! Returns the number of non-zero occupied atomic natural orbitals. For this
    ! purpose, zero is defined to be "tol" if present, or 10.0d0**(-7) otherwise
      character(128), optional :: ANOkind
      real(kind=kind(1.0d0)), optional :: tol
      integer(kind=kind(1)) :: res
      integer(kind=kind(1)) :: a

      call ensure_(tonto,associated(self(1)%occupation_numbers),"ATOMVEC:no_of_occupied_ANOs ... no occupation numbers")
      res = 0
      do a = 1,n_atom_(self)
         res = res + no_of_occupied_NOs_(self(a),ANOkind,tol)
      end do

   end function

!  *********************
!  Basis set information
!  *********************

   function bases_all_exist(self) result(res)
    type(atom_type), dimension(:) :: self
    ! Return .true. if *all* basis sets are associated. NOTE: do not confuse this
    ! with the routine .basis_are_resolved, which is probably what you want.
      logical(kind=kind(.true.)) :: res
      integer(kind=kind(1)) :: a

      res = .true.
      do a = 1,n_atom_(self)
         if (.not. associated(self(a)%basis)) then
            res = .false.
            exit
         end if
      end do

   end function

   function bases_are_all_unlabeled(self) result(res)
    type(atom_type), dimension(:) :: self
    ! Return .true. if all basis set labels do not exist (or are blank)
      logical(kind=kind(.true.)) :: res
      integer(kind=kind(1)) :: a

      if (.not. bases_all_exist_(self)) then
         res = .true.
      else
         res = .true.
         do a = 1,n_atom_(self)
            if (self(a)%basis%label/=" ") then
               res = .false.
               exit
            end if
         end do
      end if

   end function

   function bases_are_all_labeled(self) result(res)
    type(atom_type), dimension(:) :: self
    ! Return .true. if all basis set labels exist (i.e. are not blank)
      logical(kind=kind(.true.)) :: res
      integer(kind=kind(1)) :: a

      if (.not. bases_all_exist_(self)) then
         res = .false.
      else
         res = .true.
         do a = 1,n_atom_(self)
            if (self(a)%basis%label==" ") then
               res = .false.
               exit
            end if
         end do
      end if

   end function

   function bases_are_part_labeled(self) result(res)
    type(atom_type), dimension(:) :: self
    ! Return .true. if only some basis set labels exist (i.e. are not blank)
      logical(kind=kind(.true.)) :: res

      if (.not. bases_all_exist_(self)) then
         res = .false.
      else
         res = .not. bases_are_all_labeled_(self)   &
           .and. .not. bases_are_all_unlabeled_(self)
      end if

   end function

   function bases_are_resolved(self) result(res)
    type(atom_type), dimension(:) :: self
    ! Return .true. if all basis sets are associated .and. their shell list parts are
    ! also associated.
      logical(kind=kind(.true.)) :: res
      integer(kind=kind(1)) :: a

      res = .true.
      do a = 1,n_atom_(self)
         if (.not. associated(self(a)%basis)) then
            res = .false.
            exit
         else if (.not. associated(self(a)%basis%shell)) then
            res = .false.
            exit
         end if
      end do

   end function

   function slaterbases_all_exist(self) result(res)
    type(atom_type), dimension(:) :: self
    ! Return .true. if *all* slaterbasis sets are associated. NOTE: do not confuse
    ! this with the routine .slaterbasis_are_resolved, which is probably what you
    ! want.
      logical(kind=kind(.true.)) :: res
      integer(kind=kind(1)) :: a

      res = .true.
      do a = 1,n_atom_(self)
         if (.not. associated(self(a)%slaterbasis)) then
            res = .false.
            exit
         end if
      end do

   end function

   function slaterbases_are_all_unlabeled(self) result(res)
    type(atom_type), dimension(:) :: self
    ! Return .true. if all slaterbasis set labels do not exist (or are blank)
      logical(kind=kind(.true.)) :: res
      integer(kind=kind(1)) :: a

      if (.not. slaterbases_all_exist_(self)) then
         res = .true.
      else
         res = .true.
         do a = 1,n_atom_(self)
            if (self(a)%slaterbasis%label/=" ") then
               res = .false.
               exit
            end if
         end do
      end if

   end function

   function slaterbases_are_all_labeled(self) result(res)
    type(atom_type), dimension(:) :: self
    ! Return .true. if all slaterbasis set labels exist (i.e. are not blank)
      logical(kind=kind(.true.)) :: res
      integer(kind=kind(1)) :: a

      if (.not. slaterbases_all_exist_(self)) then
         res = .false.
      else
         res = .true.
         do a = 1,n_atom_(self)
            if (self(a)%slaterbasis%label==" ") then
               res = .false.
               exit
            end if
         end do
      end if

   end function

   function slaterbases_are_part_labeled(self) result(res)
    type(atom_type), dimension(:) :: self
    ! Return .true. if only some slaterbasis set labels exist (i.e. are not blank)
      logical(kind=kind(.true.)) :: res

      if (.not. slaterbases_all_exist_(self)) then
         res = .false.
      else
         res = .not. slaterbases_are_all_labeled_(self)   &
           .and. .not. slaterbases_are_all_unlabeled_(self)
      end if

   end function

   function slaterbases_are_resolved(self) result(res)
    type(atom_type), dimension(:) :: self
    ! Return .true. if all slaterbasis sets are associated .and. their shell list parts are
    ! also associated.
      logical(kind=kind(.true.)) :: res
      integer(kind=kind(1)) :: a

      res = .true.
      do a = 1,n_atom_(self)
         if (.not. associated(self(a)%slaterbasis)) then
            res = .false.
            exit
         else if (.not. associated(self(a)%slaterbasis%shell)) then
            res = .false.
            exit
         end if
      end do

   end function

   function coppensbases_all_exist(self) result(res)
    type(atom_type), dimension(:) :: self
    ! Return .true. if *all* coppens basis sets are associated. NOTE: do not
    ! confuse this with the routine .basis_are_resolved, which is probably what
    ! you want.
      logical(kind=kind(.true.)) :: res
      integer(kind=kind(1)) :: a

      res = .true.
      do a = 1,n_atom_(self)
         if (.not. associated(self(a)%coppensbasis)) then
            res = .false.
            exit
         end if
      end do

   end function

   function coppensbases_are_all_unlabeled(self) result(res)
    type(atom_type), dimension(:) :: self
    ! Return .true. if all coppens basis set labels do not exist (or are blank)
      logical(kind=kind(.true.)) :: res
      integer(kind=kind(1)) :: a

      if (.not. coppensbases_all_exist_(self)) then
         res = .true.
      else
         res = .true.
         do a = 1,n_atom_(self)
            if (self(a)%coppensbasis%label/=" ") then
               res = .false.
               exit
            end if
         end do
      end if

   end function

   function coppensbases_are_all_labeled(self) result(res)
    type(atom_type), dimension(:) :: self
    ! Return .true. if all coppens basis set labels exist (i.e. are not blank)
      logical(kind=kind(.true.)) :: res
      integer(kind=kind(1)) :: a

      if (.not. coppensbases_all_exist_(self)) then
         res = .false.
      else
         res = .true.
         do a = 1,n_atom_(self)
            if (self(a)%coppensbasis%label==" ") then
               res = .false.
               exit
            end if
         end do
      end if

   end function

   function coppensbases_are_part_labeled(self) result(res)
    type(atom_type), dimension(:) :: self
    ! Return .true. if only some coppens basis set labels exist (i.e. are not blank)
      logical(kind=kind(.true.)) :: res

      if (.not. coppensbases_all_exist_(self)) then
         res = .false.
      else
         res = .not. coppensbases_are_all_labeled_(self)   &
           .and. .not. coppensbases_are_all_unlabeled_(self)
      end if

   end function

   function coppensbases_are_resolved(self) result(res)
    type(atom_type), dimension(:) :: self
    ! Return .true. if all coppens basis sets are associated .and. their shell list
    ! parts are also associated.
      logical(kind=kind(.true.)) :: res
      integer(kind=kind(1)) :: a

      res = .true.
      do a = 1,n_atom_(self)
         if (.not. associated(self(a)%coppensbasis)) then
            res = .false.
            exit
         else if (.not. associated(self(a)%coppensbasis%orbital)) then
            res = .false.
            exit
         end if
      end do

   end function

   subroutine get_distance_from(self,atomvec,distance,t1,t2)
    type(atom_type), dimension(:) :: self
    ! Calculates the shortest distance between an atom in self and one in
    ! atomvec.  Will set the distance to zero if calculated to less than 10^-6.
    ! If present, t1 and t2 are the indices of the two closest atoms.
     type(atom_type), dimension(:), intent(in) :: atomvec
     intent(in) :: self
     real(kind=kind(1.0d0)) :: distance
     integer(kind=kind(1)), optional :: t1,t2
     real(kind=kind(1.0d0)) :: dist
     integer(kind=kind(1)) :: i,j,dim1,dim2
     real(kind=kind(1.0d0)), dimension(3) :: difference

     dim1 = n_atom_(self)
     dim2 = size(atomvec)
     call ensure_(tonto,present(t1) .eqv. present(t2),"ATOMVEC:get_distance_from ... need 0 or 2 optional arguments")

      ! Do the first pair explicitly to set a starting distance.
      ! We also work with distance^2 until the end - saves computation.
     difference = self(1)%pos(:) - atomvec(1)%pos(:)
     distance = dot_product(difference,difference)

     do i=1,dim1
       do j=1,dim2
         difference = self(i)%pos(:) - atomvec(j)%pos(:)
         dist = dot_product(difference,difference)
         if (dist < 10.0d0**(-6)) dist = 0.0d0
         if (dist < distance) then
           distance = dist
           if (present(t1)) then
             t1=i; t2=j
           end if
         end if
       end do
     end do

     distance = sqrt(distance)

   end subroutine

   subroutine get_distance_from_1(self,pos,distance,t1)
    type(atom_type), dimension(:) :: self
    ! Calculates the shortest distance of "pos" to an atom in self.
    ! If present, t1 is the index of the closest atom.
     real(kind=kind(1.0d0)), dimension(3), intent(in) :: pos
     intent(in) :: self
     real(kind=kind(1.0d0)) :: distance
     integer(kind=kind(1)), optional :: t1
     real(kind=kind(1.0d0)) :: dist
     real(kind=kind(1.0d0)), dimension(3) :: difference
      integer(kind=kind(1)) :: i

      ! Do the first pair explicitly to set a starting distance.
      ! We also work with distance^2 until the end - saves computation.

     difference = self(1)%pos(:) - pos(:)
     distance = dot_product(difference,difference)

     do i = 1, n_atom_(self)
       difference = self(i)%pos(:) - pos(:)
       dist = dot_product(difference,difference)
       if (dist < 10.0d0**(-6)) dist = 0.0d0
       if (dist < distance) then
         distance = dist
         if (present(t1)) t1=i
       end if
     end do

     distance = sqrt(distance)

   end subroutine

   function same_as(self,atomvec) result(res)
    type(atom_type), dimension(:) :: self
    ! Returns true if the two atomvecs contain the same atoms, though maybe in a
    ! different order.  Checks atomic number and position of each atom, but not
    ! the basis sets.
     type(atom_type), dimension(:), intent(in) :: atomvec
     logical(kind=kind(.true.)) :: res
     logical(kind=kind(.true.)), dimension(size(self)) :: matched
     logical(kind=kind(.true.)) :: match_pos,match_kind,match
     integer(kind=kind(1)) :: n,q,dim

     res = .false.
     dim = n_atom_(self)
     if (dim/=size(atomvec)) then;   return; end if ! different number of atoms in each.
     matched = .false.
     do n = 1, dim
       match=.false.
       do q = 1, dim
         match_kind =  (self(n)%atomic_number == atomvec(q)%atomic_number)
         match_pos  =  same_as_(self(n)%pos, atomvec(q)%pos, 10.0d0**(-3) )
         if (match_pos .and. match_kind .and. (.not. matched(q))) then
           matched(q) = .true.
           match = .true.
           exit
         end if
       end do
       if (.not. match) then;   return; end if ! atom n doesn't have a match.
     end do
     do q = 1, dim            ! If not all of q are matched then atomvecs not same.
       if (.not. matched(q)) then;   return; end if
     end do
     res = .true.

   end function

!  *************
!  Crystal stuff
!  *************

   subroutine seitz_multiply(self,seitz)
    type(atom_type), dimension(:) :: self
    ! Self is operated on by the seitz matrix.
    ! Self must be in fractional coordinates.
     intent(inout) :: self
     real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: seitz

     call rotate_(self,seitz(1:3,1:3))
     call rotate_thermal_(self,seitz(1:3,1:3))
     call translate_(self,seitz(1:3,4))

   end subroutine

   subroutine translate(self,vector)
    type(atom_type), dimension(:) :: self
    ! Translate self by vector.
     intent(inout) :: self
     real(kind=kind(1.0d0)), dimension(3), intent(in) :: vector
      integer(kind=kind(1)) :: n

     do n=1,n_atom_(self)
       self(n)%pos = self(n)%pos + vector
     end do

   end subroutine

   subroutine rotate(self,matrix)
    type(atom_type), dimension(:) :: self
    ! Rotate self by the rotation matrix
     intent(inout) :: self
     real(kind=kind(1.0d0)), dimension(3,3), intent(in) :: matrix
      integer(kind=kind(1)) :: n

     do n=1,n_atom_(self)
       self(n)%pos            = matmul(matrix,self(n)%pos)
     end do

   end subroutine

   subroutine rotate_thermal(self,matrix)
    type(atom_type), dimension(:) :: self
    ! Rotate self by the rotation matrix
     intent(inout) :: self
     real(kind=kind(1.0d0)), dimension(3,3), intent(in) :: matrix
      integer(kind=kind(1)) :: n

     do n=1,n_atom_(self)
       call change_basis_(self(n)%thermal_tensor,matrix)
     end do

   end subroutine

   subroutine thermal_tensor_to(self,crystal)
    type(atom_type), dimension(:) :: self
    ! Convert all thermal tensors from cartesians to crystal coordinates.
     type(crystal_type), intent(in) :: crystal
      integer(kind=kind(1)) :: n

     do n=1,n_atom_(self)
       call thermal_tensor_to_(self(n),crystal)
     end do

   end subroutine

   subroutine thermal_tensor_from(self,crystal)
    type(atom_type), dimension(:) :: self
    ! Convert all thermal tensors from crystal coordinates to cartesians.
     type(crystal_type), intent(in) :: crystal
      integer(kind=kind(1)) :: n

     do n=1,n_atom_(self)
       call thermal_tensor_from_(self(n),crystal)
     end do

   end subroutine

   subroutine ensure_in_unitcell(self,crystal)
    type(atom_type), dimension(:) :: self
    ! Translate the position of self to be in the unitcell.
    ! WARNING: Does this work -- dylan ?
     intent(inout) :: self
     type(crystal_type), intent(in) :: crystal
     real(kind=kind(1.0d0)), dimension(3) :: translation,coa_cart,coa_frac,trans_int

     coa_cart = centre_of_atoms_(self)
     coa_frac = matmul(crystal%unitcell%inverse_matrix,coa_cart)    ! fractionals
     trans_int = floor(coa_frac + 10.0d0**(-6))                 ! round to lattice vector
     translation = matmul(crystal%unitcell%inverse_matrix,translation)  !cartesians
     call translate_(self, - translation )

   end subroutine

   function default_multiplicity(self) result(res)
    type(atom_type), dimension(:) :: self
    ! Return the default multiplicity for an atomvec/molecule.
     intent(in) :: self
     real(kind=kind(1.0d0)) :: res

     if (n_atom_(self)==1) then
        res = ground_state_multiplicity_(self(1))
     else
        res = mod(n_e_(self),2) + 1
     end if

   end function

!  *********
!  Integrals
!  *********

   subroutine make_nuclear_matrix(self,Z)
    type(atom_type), dimension(:) :: self
    ! Calculate the nuclear attraction matrix "Z" for the atoms in the list.
     target :: self
     real(kind=kind(1.0d0)), dimension(:,:) :: Z
     type(atom_type), pointer :: atom
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: Z_c
     integer(kind=kind(1)) :: q,c,fa,la,fb,lb
     type(shell2_type) :: sh

     call ensure_(tonto,bases_are_resolved_(self),"ATOMVEC:make_nuclear_matrix ... no basis set")
     call ensure_(tonto,is_square_(Z),"ATOMVEC:make_nuclear_matrix ... Z is not square")
     call ensure_(tonto,size(Z,1)==n_bf_(self),"ATOMVEC:make_nuclear_matrix ... wrong size, Z")
     call make_index_info_(self)
     Z = 0.0d0
     do q = 1,n_shell_pairs_(self)
        call get_shell_pair_(self,sh,q,fa,la,fb,lb)
        call create_(Z_c,sh%a%n_comp,sh%b%n_comp)
        do c = 1,n_atom_(self)
           atom => self(c)
           call get_nuc_(sh,Z_c,mass_(atom),atom%pos)
           Z(fa:la,fb:lb) = Z(fa:la,fb:lb) - atom%atomic_number * Z_c
        end do
        call destroy_(Z_c)
        call destroy_ptr_part_(sh)
     end do
     call symmetric_reflect_(Z)
     call destroy_index_info_(self)

   end subroutine

   subroutine make_nuclear_matrix_1(self,Z,nuclei)
    type(atom_type), dimension(:) :: self
    ! Calculate the nuclear attraction matrix "Z" for the basis functions on all
    ! atoms in the list, but only for the positive nuclei specified in the
    ! "nuclei" list.
     target :: self
     real(kind=kind(1.0d0)), dimension(:,:) :: Z
     integer(kind=kind(1)), dimension(:) :: nuclei
     type(atom_type), pointer :: atom
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: Z_c
     integer(kind=kind(1)) :: q,c,fa,la,fb,lb
     type(shell2_type) :: sh

     call ensure_(tonto,bases_are_resolved_(self),"ATOMVEC:make_nuclear_matrix_1 ... no basis set")
     call ensure_(tonto,is_square_(Z),"ATOMVEC:make_nuclear_matrix_1 ... Z is not square")
     call ensure_(tonto,size(Z,1)==n_bf_(self),"ATOMVEC:make_nuclear_matrix_1 ... wrong size, Z")
     call make_index_info_(self)
     Z = 0.0d0
     do q = 1,n_shell_pairs_(self)
        call get_shell_pair_(self,sh,q,fa,la,fb,lb)
        call create_(Z_c,sh%a%n_comp,sh%b%n_comp)
        do c = 1,size(nuclei)
           atom => self(nuclei(c))
           call get_nuc_(sh,Z_c,mass_(atom),atom%pos)
           Z(fa:la,fb:lb) = Z(fa:la,fb:lb) - atom%atomic_number * Z_c
        end do
        call destroy_(Z_c)
        call destroy_ptr_part_(sh)
     end do
     call symmetric_reflect_(Z)
     call destroy_index_info_(self)

   end subroutine

!  ***********
!  Shell pairs
!  ***********

   subroutine get_shell_pair(self,shell,index,fa,la,fb,lb)
    type(atom_type), dimension(:) :: self
    ! Get the type(shell2_type) object "shell" correponding to the pair index "index"
    ! Also return the basis function start indices "fa", "la", etc ...
      integer(kind=kind(1)), intent(in) :: index
      type(shell2_type), intent(out) :: shell
      integer(kind=kind(1)), intent(out) :: fa,la,fb,lb
      integer(kind=kind(1)) :: a,b,aa,sa,bb,sb

      call ensure_(tonto,index_info_created,"ATOMVEC:get_shell_pair ... no index information")
      a  = (1+int(sqrt(8.0d0*index-7.0d0)))/2
      b  = index - a*(a-1)/2
      fa = first_basis_fn_4_shell(a)  ! These are module variables
      fb = first_basis_fn_4_shell(b)
      la = last_basis_fn_4_shell(a)
      lb = last_basis_fn_4_shell(b)
      aa = atom_4_shell(a)
      bb = atom_4_shell(b)
      sa = atom_shell_4_shell(a)
      sb = atom_shell_4_shell(b)
      call copy_(shell,self(aa)%basis%shell(sa), self(bb)%basis%shell(sb), &
                 self(aa)%pos, self(bb)%pos )

   end subroutine

   subroutine get_shell_pair_1(self,shell,index,fa,la,fb,lb,atom_a,atom_b)
    type(atom_type), dimension(:) :: self
    ! Get the type(shell2_type) object "shell" correponding to the pair index "index"
    ! Also return the basis function start indices "fa", "la", etc ...
    ! Plus the atoms the shells are located on, "atom_a" and "atom_b".
     integer(kind=kind(1)), intent(in) :: index
     type(shell2_type), intent(out) :: shell
     integer(kind=kind(1)), intent(out) :: fa,la,fb,lb,atom_a,atom_b
     integer(kind=kind(1)) :: a,b,sa,sb

     call ensure_(tonto,index_info_created,"ATOMVEC:get_shell_pair_1 ... no index information")
     a  = (1+int(sqrt(8.0d0*index-7.0d0)))/2
     b  = index - a*(a-1)/2
     fa = first_basis_fn_4_shell(a)
     fb = first_basis_fn_4_shell(b)
     la = last_basis_fn_4_shell(a)
     lb = last_basis_fn_4_shell(b)
     atom_a = atom_4_shell(a)
     atom_b = atom_4_shell(b)
     sa = atom_shell_4_shell(a)
     sb = atom_shell_4_shell(b)
     call copy_(shell,self(atom_a)%basis%shell(sa), self(atom_b)%basis%shell(sb), &
                self(atom_a)%pos, self(atom_b)%pos )

   end subroutine

   subroutine make_index_info(self)
    type(atom_type), dimension(:) :: self
    ! Define a vector of atom numbers corresponding to the molecule
    ! basis set shell numbers; also define a vector of atom shell numbers
    ! corresponding to the molecule basis set shell number

     atom_4_shell       => atom_for_shell_(self)
     atom_shell_4_shell => atom_shell_for_shell_(self)
     first_shell_4_atom => first_shell_for_atom_(self)
     call get_shell_limits_(self,first_basis_fn_4_shell, last_basis_fn_4_shell)
     call make_atom_basis_fn_limits_(self,first_basis_fn_4_atom,last_basis_fn_4_atom)
     index_info_created = .true.

   end subroutine

   subroutine destroy_index_info(self)
    type(atom_type), dimension(:) :: self
    ! Destroythe private index information. There may be problems with this for
    ! compilers without default initialisation ...

     call destroy_(atom_4_shell)
     call destroy_(atom_shell_4_shell)
     call destroy_(first_shell_4_atom)
     call destroy_(first_basis_fn_4_shell)
     call destroy_(last_basis_fn_4_shell)
     call destroy_(first_basis_fn_4_atom)
     call destroy_(last_basis_fn_4_atom)
     index_info_created = .false.

   end subroutine

   subroutine make_coppens_interpolators(self)
    type(atom_type), dimension(:) :: self
    ! Make a unique set of coppensbasis orbital density interpolators.
    ! NOTE: This requires careful destroying.
      integer(kind=kind(1)), dimension(:), pointer :: unique_atom,atom_kind
      integer(kind=kind(1)) :: n_kind,k,u,j

      call make_unique_atom_list_(self,unique_atom,atom_kind,n_kind)
      do k = 1,n_kind    ! Get interpolators only for unique atoms
        u = unique_atom(k)
        call make_interpolator_(self(u)%coppensbasis)
       ! self(u).coppensbasis.interpolator.put
        do j = u+1,size(self)  ! ... pointer assign the rest
           if (atom_kind(j)/=k) cycle
           self(j)%coppensbasis%interpolator => self(u)%coppensbasis%interpolator
        end do
      end do
      call destroy_(unique_atom)
      call destroy_(atom_kind)

   end subroutine

   subroutine destroy_coppens_interpolators(self)
    type(atom_type), dimension(:) :: self
    ! Destroy the coppens interpolators for each atom. NOTE: This assumes that
    ! they were created only by the routine "make_coppens_interpolators".
      integer(kind=kind(1)), dimension(:), pointer :: unique_atom,atom_kind
      integer(kind=kind(1)) :: n_kind,k,u,j

      if (.not. coppens_interpolators_exist_(self)) then;   return; end if
      call make_unique_atom_list_(self,unique_atom,atom_kind,n_kind)
      do k = 1,n_kind    ! Destroy interpolators only for unique atoms
        u = unique_atom(k)
        call destroy_(self(u)%coppensbasis%interpolator)
        do j = u+1,size(self)  ! ... nullify the rest
           if (atom_kind(j)/=k) cycle
           nullify(self(j)%coppensbasis%interpolator)
        end do
      end do
      call destroy_(unique_atom)
      call destroy_(atom_kind)

   end subroutine

   function coppens_interpolators_exist(self) result(res)
    type(atom_type), dimension(:) :: self
    ! Returns .true. if all the interpolators exist. If so, it is assumed they were
    ! created by make_coppens_interpolators.
      logical(kind=kind(.true.)) :: res
      integer(kind=kind(1)) :: i

      res = .true.
      do i = 1,size(self)
         if (associated(self(i)%coppensbasis)) then
            if (associated(self(i)%coppensbasis%interpolator)) then
               cycle
            end if
         end if
         res = .false.
         exit
      end do

   end function

   subroutine make_slater_interpolators(self)
    type(atom_type), dimension(:) :: self
    ! Make a unique set of slaterbasis orbital density interpolators.
    ! NOTE: This requires careful destroying.
      integer(kind=kind(1)), dimension(:), pointer :: unique_atom,atom_kind
      integer(kind=kind(1)) :: n_kind,k,u,j

      call make_unique_atom_list_(self,unique_atom,atom_kind,n_kind)
      do k = 1,n_kind    ! Get interpolators only for unique atoms
        u = unique_atom(k)
        call make_interpolator_(self(u)%slaterbasis)
       ! self(u).slaterbasis.interpolator.put
        do j = u+1,size(self)  ! ... pointer assign the rest
           if (atom_kind(j)/=k) cycle
           self(j)%slaterbasis%interpolator => self(u)%slaterbasis%interpolator
        end do
      end do
      call destroy_(unique_atom)
      call destroy_(atom_kind)

   end subroutine

   subroutine destroy_slater_interpolators(self)
    type(atom_type), dimension(:) :: self
    ! Destroy the slater interpolators for each atom. NOTE: This assumes that
    ! they were created only by the routine "make_slater_interpolators".
      integer(kind=kind(1)), dimension(:), pointer :: unique_atom,atom_kind
      integer(kind=kind(1)) :: n_kind,k,u,j

      if (.not. slater_interpolators_exist_(self)) then;   return; end if
      call make_unique_atom_list_(self,unique_atom,atom_kind,n_kind)
      do k = 1,n_kind    ! Destroy interpolators only for unique atoms
        u = unique_atom(k)
        call destroy_(self(u)%slaterbasis%interpolator)
        do j = u+1,size(self)  ! ... nullify the rest
           if (atom_kind(j)/=k) cycle
           nullify(self(j)%slaterbasis%interpolator)
        end do
      end do
      call destroy_(unique_atom)
      call destroy_(atom_kind)

   end subroutine

   function slater_interpolators_exist(self) result(res)
    type(atom_type), dimension(:) :: self
    ! Returns .true. if all the interpolators exist. If so, it is assumed they were
    ! created by make_slater_interpolators.
      logical(kind=kind(.true.)) :: res
      integer(kind=kind(1)) :: i

      res = .true.
      do i = 1,size(self)
         if (associated(self(i)%slaterbasis)) then
            if (associated(self(i)%slaterbasis%interpolator)) then
               cycle
            end if
         end if
         res = .false.
         exit
      end do

   end function

end
