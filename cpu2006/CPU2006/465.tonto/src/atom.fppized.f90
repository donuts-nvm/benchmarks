!-------------------------------------------------------------------------------
!
! ATOM: can be a quantum mechanical atom, with a basis set
!       or a molecular mechanical atom with a force field potential
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
! $Id: atom.foo,v 1.33.2.17 2003/11/13 05:34:14 reaper Exp $
!-------------------------------------------------------------------------------

module ATOM_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

   use SLATERBASIS_MODULE, only: create_
   use SLATERBASIS_MODULE, only: resolve_by_label_
   use SLATERBASIS_MODULE, only: read_keywords_
   use SLATERBASIS_MODULE, only: make_density_grid_
   use SLATERBASIS_MODULE, only: destroy_

   use SHELL1_MODULE, only: make_grid_
   use SHELL1_MODULE, only: set_

   use INTVEC_MODULE, only: create_

   use REAL_MODULE, only: same_as_

   use OPVECTOR_MODULE, only: no_of_occupied_
   use OPVECTOR_MODULE, only: create_copy_
   use OPVECTOR_MODULE, only: created_
   use OPVECTOR_MODULE, only: destroy_

   use REALVEC_MODULE, only: create_
   use REALVEC_MODULE, only: destroy_

   use INT_MODULE, only: to_str_

   use UNITCELL_MODULE, only: beta_star_
   use UNITCELL_MODULE, only: alpha_star_
   use UNITCELL_MODULE, only: gamma_star_

   use BASIS_MODULE, only: create_
   use BASIS_MODULE, only: no_of_basis_functions_
   use BASIS_MODULE, only: no_of_shells_
   use BASIS_MODULE, only: put_
   use BASIS_MODULE, only: no_of_primitives_
   use BASIS_MODULE, only: read_keywords_
   use BASIS_MODULE, only: resolve_by_label_
   use BASIS_MODULE, only: min_exponent_
   use BASIS_MODULE, only: destroy_

   use TEXTFILE_MODULE, only: stdin
   use TEXTFILE_MODULE, only: stdout
   use TEXTFILE_MODULE, only: set_default_units_
   use TEXTFILE_MODULE, only: text_
   use TEXTFILE_MODULE, only: next_str_
   use TEXTFILE_MODULE, only: skip_next_item_
   use TEXTFILE_MODULE, only: redirect_
   use TEXTFILE_MODULE, only: next_item_
   use TEXTFILE_MODULE, only: show_
   use TEXTFILE_MODULE, only: revert_
   use TEXTFILE_MODULE, only: put_
   use TEXTFILE_MODULE, only: read_
   use TEXTFILE_MODULE, only: flush_
   use TEXTFILE_MODULE, only: read_ptr_
   use TEXTFILE_MODULE, only: reverted_
   use TEXTFILE_MODULE, only: dash_

   use STR_MODULE, only: is_int_
   use STR_MODULE, only: split_
   use STR_MODULE, only: to_lower_case_
   use STR_MODULE, only: to_upper_case_
   use STR_MODULE, only: to_int_
   use STR_MODULE, only: includes_

   use STRVEC_MODULE, only: create_copy_
   use STRVEC_MODULE, only: destroy_
   use STRVEC_MODULE, only: index_of_matching_bracket_

   use COPPENSBASIS_MODULE, only: density_at_radius_
   use COPPENSBASIS_MODULE, only: set_label_
   use COPPENSBASIS_MODULE, only: create_
   use COPPENSBASIS_MODULE, only: put_
   use COPPENSBASIS_MODULE, only: read_keywords_
   use COPPENSBASIS_MODULE, only: resolve_by_label_
   use COPPENSBASIS_MODULE, only: min_exponent_
   use COPPENSBASIS_MODULE, only: destroy_
   use COPPENSBASIS_MODULE, only: make_density_grid_

   use OPMATRIX_MODULE, only: create_copy_
   use OPMATRIX_MODULE, only: created_
   use OPMATRIX_MODULE, only: number_kind_
   use OPMATRIX_MODULE, only: destroy_

   use REALMAT_MODULE, only: create_
   use REALMAT_MODULE, only: change_basis_
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

   public    put_natural_orbitals_
   interface put_natural_orbitals_
      module procedure put_natural_orbitals
   end interface

   public    read_keys_
   interface read_keys_
      module procedure read_keys
   end interface

   public    read_axis_system_
   interface read_axis_system_
      module procedure read_axis_system
   end interface

   public    read_slaterbasis_
   interface read_slaterbasis_
      module procedure read_slaterbasis
   end interface

   public    read_basis_label_
   interface read_basis_label_
      module procedure read_basis_label
   end interface

   public    nullify_ptr_part_
   interface nullify_ptr_part_
      module procedure nullify_ptr_part
   end interface

   public    no_of_occupied_NOs_
   interface no_of_occupied_NOs_
      module procedure no_of_occupied_NOs
   end interface

   public    make_density_grid_r_
   interface make_density_grid_r_
      module procedure make_density_grid_r
   end interface

   public    read_thermal_tensor_
   interface read_thermal_tensor_
      module procedure read_thermal_tensor
   end interface

   public    resolve_axis_system_
   interface resolve_axis_system_
      module procedure resolve_axis_system
   end interface

   public    put_table_header_
   interface put_table_header_
      module procedure put_table_header
   end interface

   public    read_basis_
   interface read_basis_
      module procedure read_basis
   end interface

   public    table_width_
   interface table_width_
      module procedure table_width
   end interface

   public    read_keywords_
   interface read_keywords_
      module procedure read_keywords
   end interface

   public    column_number_
   interface column_number_
      module procedure column_number
   end interface

   public    set_thermal_axis_system_
   interface set_thermal_axis_system_
      module procedure set_thermal_axis_system
   end interface

   public    read_thermal_axis_system_
   interface read_thermal_axis_system_
      module procedure read_thermal_axis_system
   end interface

   public    no_of_shells_
   interface no_of_shells_
      module procedure no_of_shells
   end interface

   public    make_orbital_grid_c_
   interface make_orbital_grid_c_
      module procedure make_orbital_grid_c
   end interface

   public    set_label_and_atomic_number_
   interface set_label_and_atomic_number_
      module procedure set_label_and_atomic_number
   end interface

   public    read_label_
   interface read_label_
      module procedure read_label
   end interface

   public    has_ANO_data_
   interface has_ANO_data_
      module procedure has_ANO_data
   end interface

   public    copy_
   interface copy_
      module procedure copy
   end interface

   public    chemical_symbol_
   interface chemical_symbol_
      module procedure chemical_symbol
   end interface

   public    clear_keys_
   interface clear_keys_
      module procedure clear_keys
   end interface

   public    set_coppensbasis_label_
   interface set_coppensbasis_label_
      module procedure set_coppensbasis_label
   end interface

   public    n_shell_
   interface n_shell_
      module procedure n_shell
   end interface

   public    mean_neutron_number_
   interface mean_neutron_number_
      module procedure mean_neutron_number
   end interface

   public    min_basis_exponent_
   interface min_basis_exponent_
      module procedure min_basis_exponent
   end interface

   public    same_kind_as_
   interface same_kind_as_
      module procedure same_kind_as
   end interface

   public    keys_created_
   interface keys_created_
      module procedure keys_created
   end interface

   public    make_orbital_grid_r_
   interface make_orbital_grid_r_
      module procedure make_orbital_grid_r
   end interface

   public    position_from_
   interface position_from_
      module procedure position_from
   end interface

   public    period_block_
   interface period_block_
      module procedure period_block
   end interface

   public    get_shell_limits_
   interface get_shell_limits_
      module procedure get_shell_limits
   end interface

   public    bragg_slater_radius_
   interface bragg_slater_radius_
      module procedure bragg_slater_radius
   end interface

   public    make_density_grid_
   interface make_density_grid_
      module procedure make_density_grid
   end interface

   public    thermal_tensor_from_
   interface thermal_tensor_from_
      module procedure thermal_tensor_from
   end interface

   public    destroy_
   interface destroy_
      module procedure destroy
   end interface

   public    density_at_radius_
   interface density_at_radius_
      module procedure density_at_radius
   end interface

   public    chemical_name_
   interface chemical_name_
      module procedure chemical_name
   end interface

   public    position_to_
   interface position_to_
      module procedure position_to
   end interface

   public    read_units_
   interface read_units_
      module procedure read_units
   end interface

   public    has_basis_
   interface has_basis_
      module procedure has_basis
   end interface

   public    n_prim_
   interface n_prim_
      module procedure n_prim
   end interface

   public    read_sequence_number_
   interface read_sequence_number_
      module procedure read_sequence_number
   end interface

   public    mass_
   interface mass_
      module procedure mass
   end interface

   public    put_mm_info_
   interface put_mm_info_
      module procedure put_mm_info
   end interface

   public    neutron_scattering_length_
   interface neutron_scattering_length_
      module procedure neutron_scattering_length
   end interface

   public    put_thermal_tensor_
   interface put_thermal_tensor_
      module procedure put_thermal_tensor
   end interface

   public    period_number_
   interface period_number_
      module procedure period_number
   end interface

   public    destroy_ptr_part_
   interface destroy_ptr_part_
      module procedure destroy_ptr_part
   end interface

   public    read_restraining_force_
   interface read_restraining_force_
      module procedure read_restraining_force
   end interface

   public    read_mm_atom_type_
   interface read_mm_atom_type_
      module procedure read_mm_atom_type
   end interface

   public    read_mm_charge_
   interface read_mm_charge_
      module procedure read_mm_charge
   end interface

   public    read_coppensbasis_
   interface read_coppensbasis_
      module procedure read_coppensbasis
   end interface

   public    change_axis_system_to_
   interface change_axis_system_to_
      module procedure change_axis_system_to
   end interface

   public    process_keyword_
   interface process_keyword_
      module procedure process_keyword
   end interface

   public    put_
   interface put_
      module procedure put
   end interface

   public    process_keys_
   interface process_keys_
      module procedure process_keys
   end interface

   public    put_table_footer_
   interface put_table_footer_
      module procedure put_table_footer
   end interface

   public    thermal_tensor_to_
   interface thermal_tensor_to_
      module procedure thermal_tensor_to
   end interface

   public    read_pdb_input_line_
   interface read_pdb_input_line_
      module procedure read_pdb_input_line
   end interface

   public    read_site_occupancy_
   interface read_site_occupancy_
      module procedure read_site_occupancy
   end interface

   public    dispersion_correction_
   interface dispersion_correction_
      module procedure dispersion_correction
   end interface

   public    change_thermal_axis_system_to_
   interface change_thermal_axis_system_to_
      module procedure change_thermal_axis_system_to
   end interface

   public    ground_state_multiplicity_
   interface ground_state_multiplicity_
      module procedure ground_state_multiplicity
   end interface

   public    set_defaults_
   interface set_defaults_
      module procedure set_defaults
   end interface

   public    create_
   interface create_
      module procedure create
   end interface

   public    read_pos_
   interface read_pos_
      module procedure read_pos
   end interface

   public    create_copy_
   interface create_copy_
      module procedure create_copy
   end interface

   public    read_mm_forcefield_name_
   interface read_mm_forcefield_name_
      module procedure read_mm_forcefield_name
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

   public    has_basis_label_
   interface has_basis_label_
      module procedure has_basis_label
   end interface

   public    read_junk_
   interface read_junk_
      module procedure read_junk
   end interface

   public    read_group_
   interface read_group_
      module procedure read_group
   end interface

   public    read_U_iso_
   interface read_U_iso_
      module procedure read_U_iso
   end interface

   public    no_of_basis_functions_
   interface no_of_basis_functions_
      module procedure no_of_basis_functions
   end interface

   public    read_residue_name_
   interface read_residue_name_
      module procedure read_residue_name
   end interface

   public    library_basis_label_
   interface library_basis_label_
      module procedure library_basis_label
   end interface

   public    make_density_grid_c_
   interface make_density_grid_c_
      module procedure make_density_grid_c
   end interface

   public    set_axis_system_
   interface set_axis_system_
      module procedure set_axis_system
   end interface

   public    read_residue_atom_name_
   interface read_residue_atom_name_
      module procedure read_residue_atom_name
   end interface

   public    read_restraining_position_
   interface read_restraining_position_
      module procedure read_restraining_position
   end interface

   public    resolve_basis_
   interface resolve_basis_
      module procedure resolve_basis
      module procedure resolve_basis_1
      module procedure resolve_basis_2
   end interface

   character(128), dimension(:), pointer, private :: keys => NULL()

!  ***************
!  Data statements
!  ***************

    !  The Periodic Table

   character(2), dimension(103) :: element_symbols

   data element_symbols/ &
    "H ",                                                                                "He", &
    "Li","Be",                                                  "B ","C ","N ","O ","F ","Ne", &
    "Na","Mg",                                                  "Al","Si","P ","S ","Cl","Ar", &
    "K ","Ca","Sc","Ti","V ","Cr","Mn","Fe","Co","Ni","Cu","Zn","Ga","Ge","As","Se","Br","Kr", &
    "Rb","Sr","Y ","Zr","Nb","Mo","Tc","Ru","Rh","Pd","Ag","Cd","In","Sn","Sb","Te","I ","Xe", &
    "Cs","Ba","La",     "Ce","Pr","Nd","Pm","Sm","Eu","Gd","Tb","Dy","Ho","Er","Tm","Yb","Lu", &
                   "Hf","Ta","W ","Re","Os","Ir","Pt","Au","Hg","Tl","Pb","Bi","Po","At","Rn", &
    "Fr","Ra","Ac",     "Th","Pa","U ","Np","Pu","Am","Cm","Bk","Cf","Es","Fm","Md","No","Lr"/

   character(12), dimension(103) :: element_names

   data element_names/ &
    "Hydrogen    ","Helium      ", &
    "Lithium     ","Beryllium   ", &
    "Boron       ","Carbon      ","Nitrogen    ","Oxygen      ","Fluorine    ","Neon        ", &
    "Sodium      ","Magnesium   ", &
    "Aluminium   ","Silicon     ","Phosphorous ","Sulfur      ","Chlorine    ","Argon       ", &
    "Potassium   ","Calcium     ", &
    "Scandium    ","Titanium    ","Vanadium    ","Chromium    ","Manganese   ", &
    "Iron        ","Cobalt      ","Nickel      ","Copper      ","Zinc        ", &
    "Gallium     ","Germanium   ","Arsenic     ","Selinium    ","Bromine     ","Krypton     ", &
    "Rubidium    ","Strontium   ", &
    "Yttrium     ","Zirconium   ","Niobium     ","Molybdenum  ","Technitium  ", &
    "Ruthenium   ","Rhodium     ","Palladium   ","Silver      ","Cadmium     ", &
    "Indium      ","Tin         ","Antimony    ","Tellurium   ","Iodine      ","Xenon       ", &
    "Cesium      ","Barium      ", &
    "Lanthanum   ", &
    "Cerium      ","Praseodymium","Neodymium   ","Promethium  ","Samarium    ","Europium    ","Gadolinium  ", &
    "Terbium     ","Dysprosium  ","Holmium     ","Erbium      ","Thulium     ","Ytterbium   ","Lutetium    ", &
                   "Haffnium    ","Tantalum    ","Tangsten    ","Rhenium     ", &
    "Osmium      ","Iridium     ","Platinum    ","Gold        ","Mercury     ", &
    "Thallium    ","Lead        ","Bismuth     ","Polonium    ","Astatine    ","Radon       ", &
    "Francium    ","Radium      ", &
    "Actinium    ", &
    "Thorium     ","Protactinium","Uranium     ","Neptunium   ","Plutonium   ","Americium   ","Curium      ", &
    "Berkellium  ","Californium ","Einsteinium ","Fermium     ","Mendelevium ","Nobelium    ","Lawrencium  "/

    !  Bragg-Slater radii taken from Aaron Lee's code

   real(kind=kind(1.0d0)), dimension(54) :: bragg_slater_radii

   data bragg_slater_radii/ &
      0.35d0,0.35d0,                                           &
      1.45d0,1.05d0,0.85d0,0.70d0,0.65d0,0.60d0,0.50d0,0.45d0, &
      1.80d0,1.50d0,1.25d0,1.10d0,1.00d0,1.00d0,1.00d0,1.00d0, &
      2.20d0,1.80d0,                                           &
      1.60d0,1.40d0,1.35d0,1.40d0,1.40d0,                      &
      1.40d0,1.35d0,1.35d0,1.35d0,1.35d0,                      &
                    1.30d0,1.25d0,1.15d0,1.15d0,1.15d0,1.15d0, &
      1.30d0,1.30d0,                                           &
      1.30d0,1.30d0,1.30d0,1.30d0,1.30d0,                      &
      1.30d0,1.30d0,1.30d0,1.30d0,1.30d0,                      &
                    1.30d0,1.30d0,1.30d0,1.30d0,1.30d0,1.30d0  /

    !  Abundance weighted atomic masses taken from the WWW to be the same as Turbomol

   real(kind=kind(1.0d0)), dimension(92) :: atomic_masses

   data atomic_masses/ &
       1.007970d0,   4.002600d0, &
       6.939000d0,   9.012200d0,  10.811000d0,  12.011150d0,  14.006700d0,  15.999400d0,  18.998400d0,  20.183000d0, &
      22.989800d0,  24.312000d0,  26.981500d0,  28.086000d0,  30.973800d0,  32.064000d0,  35.453000d0,  39.948000d0, &
      39.102000d0,  40.080000d0, &
                    44.956000d0,  47.900000d0,  50.942000d0,  51.996000d0,  54.938000d0, &
                    55.850000d0,  58.933200d0,  58.710000d0,  63.540000d0,  65.370000d0, &
                                  69.720000d0,  72.590000d0,  74.921600d0,  78.960000d0,  79.909000d0,  83.800000d0, &
      85.470000d0,  87.620000d0,  &
                    88.905000d0,  91.220000d0,  92.906000d0,  95.940000d0,  99.000000d0, &
                   101.070000d0, 102.905000d0, 106.400000d0, 107.870000d0, 112.400000d0, &
                                 114.820000d0, 118.690000d0, 121.750000d0, 127.600000d0, 126.904000d0, 131.300000d0, &
     132.905000d0, 137.330000d0, &
                   138.910000d0, 140.115000d0, 140.908000d0, 144.240000d0, 146.920000d0, 150.360000d0, 151.965000d0, &
                   157.250000d0, 158.925000d0, 162.500000d0, 164.930000d0, 167.260000d0, 168.930000d0, 173.040000d0, &
                   174.970000d0, 178.490000d0, 180.950000d0, 183.850000d0, 186.210000d0, &
                   190.200000d0, 192.220000d0, 195.080000d0, 196.070000d0, 200.590000d0, &
                                 204.380000d0, 207.200000d0, 208.980000d0, 208.980000d0, 209.990000d0, 222.020000d0, &
     223.020000d0, 226.030000d0, &
                   227.030000d0, 232.040000d0, 231.040000d0, 238.030000d0/

    !  Abundance-weighted coherent neutron scattering lengths in fm taken from:
    !  International Tables for Crystallography, Vol. C, 1992, pp 384-391

   real(kind=kind(1.0d0)), dimension(95) :: neutron_scattering_lengths

   data neutron_scattering_lengths/ &
      -3.7390,  3.2600, &
      -1.9000,  7.7900,  5.3000,  6.6460,  9.3600,  5.8030,  5.6540,  4.5470, &
       3.6300,  5.3750,  3.4490,  4.1490,  5.1300,  2.8470,  9.5770,  1.9090, &
       3.7100,  4.9000, &
               12.2900, -3.4380,  -.3824,  3.6350, -3.7300, &
                9.5400,  2.5000, 10.3000,  7.7180,  5.6890, &
                         7.2879,  8.1929,  6.5800,  7.9700,  6.7950,  7.8000, &
       7.0800,  7.0200, &
                7.7500,  7.1600,  7.0540,  6.9500,  6.8000, &
                7.2100,  5.8800,  5.9100,  5.9220,  5.1000, &
                         4.0650,  6.2257,  5.5700,  5.8000,  5.2800,  4.8500, &
       5.4200,  5.0600, &
                8.2400,  4.8400,  4.4500,  7.6900, 12.6000,  4.2000,  6.7300, &
                9.5000,  7.3800, 16.9000,  8.0800,  8.0300,  7.0700, 12.4100, &
                7.2100,  7.7700,  6.9100,  4.7700,  9.2000, &
               11.0000, 10.6000,  9.6000,  7.6300, 12.6920, &
                         8.7760,  9.4017,  8.5307,  0.0000,  0.0000,  0.0000, &
       0.0000, 10.0000, &
                0.0000, 10.6300,  9.1000,  8.4170, 10.5500, 14.1000,  8.3000/

    ! X-ray dispersion correction factors.
    ! First element is f', second is f".  Cr wavelength = 2.291A.

   complex(kind=kind((1.0d0,1.0d0))), dimension(92) :: dispersion_correction_Cr

   data dispersion_correction_Cr/ &
    (  0.0,  0.0), (  0.0,  0.0), (  0.0,  0.0), (  0.0,  0.0), (  0.0,  0.0), &
    (  0.0,  0.1), (  0.0,  0.1), (  0.1,  0.1), (  0.1,  0.2), (  0.1,  0.3), &
    (  0.2,  0.4), (  0.2,  0.5), (  0.2,  0.6), (  0.3,  0.8), (  0.3,  0.9), &
    (  0.3,  1.2), (  0.3,  1.4), (  0.2,  1.7), (  0.0,  2.1), ( -0.2,  2.6), &
    ( -0.7,  3.1), ( -1.7,  3.7), ( -4.4,  0.5), ( -2.2,  0.6), ( -1.8,  0.7), &
    ( -1.6,  0.8), ( -1.4,  0.9), ( -1.2,  1.1), ( -1.1,  1.2), ( -1.0,  1.4), &
    ( -0.9,  1.6), ( -0.8,  1.8), ( -0.7,  2.1), ( -0.7,  2.3), ( -0.7,  2.5), &
    ( -0.7,  2.8), ( -0.7,  3.2), ( -0.7,  3.6), ( -0.7,  3.9), ( -0.8,  4.3), &
    ( -0.8,  4.8), ( -0.9,  5.3), ( -1.1,  5.9), ( -1.2,  6.4), ( -1.4,  6.9), &
    ( -1.7,  7.5), ( -2.0,  8.2), ( -2.3,  8.8), ( -2.8,  9.5), ( -3.3, 10.3), &
    ( -4.0, 11.1), ( -5.0, 11.9), ( -7.1, 13.1), ( -9.0, 10.0), (-12.0, 11.0), &
    (-11.0,  8.0), (-14.0,  3.0), (-10.0,  3.0), ( -9.0,  3.0), ( -8.0,  4.0), &
    ( -7.0,  4.0), ( -7.0,  4.0), ( -6.0,  5.0), ( -6.0,  5.0), ( -6.0,  6.0), &
    ( -6.0,  6.0), ( -5.0,  7.0), ( -5.0,  7.0), ( -5.0,  8.0), ( -5.0,  8.0), &
    ( -5.0,  9.0), ( -5.0,  9.0), ( -5.0, 10.0), ( -5.0, 10.0), ( -5.0, 11.0), &
    ( -5.0, 12.0), ( -5.0, 13.0), ( -5.0, 14.0), ( -5.0, 14.0), ( -5.0, 15.0), &
    ( -5.0, 16.0), ( -6.0, 17.0), ( -6.0, 18.0), ( -7.0, 19.0), ( -8.0, 20.0), &
    ( -9.0, 22.0), (-10.0, 23.0), (-11.0, 24.0), (-12.0, 26.0), (-13.0, 27.0), &
    (-15.0, 28.0), (-17.0, 27.0)/

    ! X-ray dispersion correction factors.
    ! First element is f', second is f".  Cu wavelength = 1.542A.

   complex(kind=kind((1.0d0,1.0d0))), dimension(92) :: dispersion_correction_Cu

   data dispersion_correction_Cu/ &
    (  0.0,  0.0), (  0.0,  0.0), (  0.0,  0.0), (  0.0,  0.0), (  0.0,  0.0), &
    (  0.0,  0.0), (  0.0,  0.0), (  0.0,  0.1), (  0.0,  0.1), (  0.1,  0.1), &
    (  0.1,  0.2), (  0.1,  0.2), (  0.1,  0.3), (  0.2,  0.4), (  0.2,  0.5), &
    (  0.3,  0.6), (  0.3,  0.7), (  0.3,  0.8), (  0.3,  1.0), (  0.3,  1.3), &
    (  0.3,  1.5), (  0.2,  1.8), (  0.1,  2.2), ( -0.1,  2.5), ( -0.5,  2.9), &
    ( -1.1,  3.3), ( -2.2,  3.8), ( -3.1,  0.5), ( -2.1,  0.6), ( -1.7,  0.7), &
    ( -1.5,  0.8), ( -1.3,  0.9), ( -1.2,  1.0), ( -1.1,  1.1), ( -1.0,  1.3), &
    ( -1.0,  1.5), ( -0.9,  1.7), ( -0.8,  1.8), ( -0.8,  2.0), ( -0.7,  2.2), &
    ( -0.7,  2.5), ( -0.6,  2.7), ( -0.6,  3.0), ( -0.6,  3.3), ( -0.6,  3.6), &
    ( -0.6,  3.9), ( -0.6,  4.3), ( -0.7,  4.6), ( -0.8,  5.0), ( -0.9,  5.4), &
    ( -1.0,  5.8), ( -1.1,  6.2), ( -1.3,  6.7), ( -1.6,  7.2), ( -1.9,  7.7), &
    ( -2.3,  8.3), ( -2.7,  8.9), ( -3.1,  9.6), ( -3.6, 10.2), ( -4.4, 10.9), &
    ( -5.3, 11.5), ( -6.7, 12.4), ( -9.0, 10.2), (-12.0, 11.2), (-11.0,  7.0), &
    (-10.0,  8.0), (-13.0,  3.0), ( -9.0,  3.0), ( -8.0,  4.0), ( -8.0,  4.0), &
    ( -7.0,  4.0), ( -7.0,  4.0), ( -6.0,  5.0), ( -6.0,  5.0), ( -6.0,  5.0), &
    ( -6.0,  6.0), ( -6.0,  6.0), ( -5.0,  7.0), ( -5.0,  7.0), ( -5.0,  8.0), &
    ( -5.0,  8.0), ( -5.0,  9.0), ( -5.0,  9.0), ( -5.0, 10.0), ( -5.0, 10.0), &
    ( -5.0, 11.0), ( -5.0, 11.0), ( -5.0, 12.0), ( -5.0, 12.0), ( -5.0, 13.0), &
    ( -5.0, 14.0), ( -5.0, 15.0)/

    ! X-ray dispersion correction factors.
    ! First element is f', second is f".  Mo wavelength = 0.7107A.

   complex(kind=kind((1.0d0,1.0d0))), dimension(92) :: dispersion_correction_Mo

   data dispersion_correction_Mo/ &
    (  0.0,  0.0), (  0.0,  0.0), (  0.0,  0.0), (  0.0,  0.0), (  0.0,  0.0),  &
    (  0.0,  0.0), (  0.0,  0.0), (  0.0,  0.0), (  0.0,  0.0), (  0.0,  0.0),  &
    (  0.0,  0.1), (  0.0,  0.1), (  0.1,  0.1), (  0.1,  0.1), (  0.1,  0.1),  &
    (  0.1,  0.2), (  0.1,  0.2), (  0.1,  0.2), (  0.2,  0.3), (  0.2,  0.3),  &
    (  0.2,  0.4), (  0.2,  0.5), (  0.3,  0.6), (  0.3,  0.7), (  0.3,  0.8),  &
    (  0.3,  0.9), (  0.3,  1.0), (  0.3,  1.1), (  0.3,  1.3), (  0.3,  1.5),  &
    (  0.2,  1.6), (  0.2,  1.8), (  0.1,  2.0), ( -0.1,  2.2), ( -0.3,  2.4),  &
    ( -0.6,  2.7), ( -0.9,  3.0), ( -1.4,  3.4), ( -2.3,  3.7), ( -2.8,  0.6),  &
    ( -2.1,  0.6), ( -1.7,  0.7), ( -1.5,  0.8), ( -1.3,  0.8), ( -1.2,  0.9),  &
    ( -1.1,  1.0), ( -1.0,  1.1), ( -0.9,  1.3), ( -0.8,  1.4), ( -0.8,  1.5),  &
    ( -0.8,  1.6), ( -0.7,  1.8), ( -0.7,  1.9), ( -0.6,  2.1), ( -0.6,  2.3),  &
    ( -0.6,  2.5), ( -0.5,  2.7), ( -0.5,  2.9), ( -0.5,  3.1), ( -0.5,  3.3),  &
    ( -0.5,  3.5), ( -0.5,  3.7), ( -0.5,  3.9), ( -0.6,  4.1), ( -0.6,  4.3),  &
    ( -0.7,  4.7), ( -0.7,  5.0), ( -0.7,  5.3), ( -0.8,  5.6), ( -0.8,  5.9),  &
    ( -0.9,  6.1), ( -1.0,  6.4), ( -1.1,  6.7), ( -1.3,  7.1), ( -1.5,  7.5),  &
    ( -1.7,  7.9), ( -2.0,  8.3), ( -2.2,  8.7), ( -2.5,  9.2), ( -2.9,  9.7),  &
    ( -3.5, 10.2), ( -4.1, 10.7), ( -4.8, 11.1), ( -5.5, 11.7), ( -7.0,  9.0),  &
    ( -8.0, 10.0), ( -8.0,  7.0), ( -7.0,  7.0), ( -7.0,  8.0), ( -7.0,  7.0),  &
    ( -7.0,  8.0), ( -8.0,  8.0)/

contains

!  ******************
!  Allocation methods
!  ******************

   subroutine create(self)
    type(atom_type) :: self
    ! Create an atom
      pointer :: self

      nullify(self)
      allocate(self)

      call nullify_ptr_part_(self)
      call set_defaults_(self)

   end subroutine

   subroutine create_copy(self,atom)
    type(atom_type) :: self
    ! Create a copy of atom.
     type(atom_type) :: atom
     pointer :: self

     call create_(self)
     call copy_(self,atom)

   end subroutine

   subroutine copy(self,atom)
    type(atom_type) :: self
    ! Make self a copy of atom.  WARNING: the basis part is not explicitly
    ! copied, so be careful with destroy operations.
     type(atom_type) :: atom

     self = atom
      ! if (atom.basis.created) .basis.create_copy(atom.basis)
      ! if (atom.slaterbasis.created) .coppensbasis.create_copy(atom.coppensbasis)
      ! if (atom.coppensbasis.created) .coppensbasis.create_copy(atom.coppensbasis)
     if (associated(atom%density_matrix)) call create_copy_(self%density_matrix,atom%density_matrix)
     if (associated(atom%natural_orbitals)) call create_copy_(self%natural_orbitals,atom%natural_orbitals)
     if (associated(atom%occupation_numbers)) call create_copy_(self%occupation_numbers,atom%occupation_numbers)

   end subroutine

   subroutine destroy(self)
    type(atom_type) :: self
    ! Destroy an atom
      pointer :: self

      if (.not. associated(self)) then;   return; end if
      call destroy_ptr_part_(self)
      deallocate(self)

   end subroutine

   subroutine nullify_ptr_part(self)
    type(atom_type) :: self
    ! Nullify the pointer parts of the atom

      nullify(self%basis)
      nullify(self%slaterbasis)
      nullify(self%coppensbasis)
      nullify(self%density_matrix)
      nullify(self%natural_orbitals)
      nullify(self%occupation_numbers)

   end subroutine

   subroutine destroy_ptr_part(self)
    type(atom_type) :: self
    ! Destroy the pointer parts of an atom

      call destroy_(self%basis)
      call destroy_(self%slaterbasis)
      call destroy_(self%coppensbasis)
      call destroy_(self%density_matrix)
      call destroy_(self%natural_orbitals)
      call destroy_(self%occupation_numbers)

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
    type(atom_type) :: self
    ! Set a default atom

      self%label = "?"
      self%atomic_number = 0
      self%pos = (/ 0.0d0,0.0d0,0.0d0 /)
      self%U_iso = 0.0d0
      self%thermal_tensor = 0.0d0
      self%basis_label = " "
      self%axis_system = "cartesian"
      self%thermal_axis_system = "cartesian"
      self%energy = 0.0d0
      self%group  = 0
       ! MM/protien defaults
      self%sequence_number = 0
      self%residue_atom_name = "?"
      self%residue_name = "UNK"
      self%mm_forcefield_name = "?"
      self%mm_atom_type = "?"
      self%mm_charge = 0.0d0
      self%restraining_position = (/ 0.0d0,0.0d0,0.0d0 /)
      self%restraining_force_constant = 0.0d0
      self%site_occupancy = 1.0d0

   end subroutine

   subroutine set_label_and_atomic_number(self,label)
    type(atom_type) :: self
    ! Set an type(atom_type) "label" and extract the atomic number from it.
      character(*) :: label
      integer(kind=kind(1)) :: lensym,z
      character(128) :: symbol
      logical(kind=kind(.true.)) :: error

      self%label = label
      if (is_int_(label)) then     ! The label must be the atomic number
         z = to_int_(label)
         self%atomic_number = z
      else                       ! First part of the label is the element symbol
         lensym = scan(label,"0123456789 ")-1
         error = lensym>2 .or. lensym==0
         call ensure_(tonto,.not. error,"ATOM:set_label_and_atomic_number ... unacceptable atom symbol, "// trim(label))
         symbol = label(1:lensym)
         call to_upper_case_(symbol(1:1))
         call to_lower_case_(symbol(2:2))
         if (any(element_symbols==symbol(1:2))) then
            do z = 1,size(element_symbols)
               if (element_symbols(z)==symbol(1:2)) exit
            end do
           self%atomic_number = z
         else
            call die_(tonto,"ATOM:set_label_and_atomic_number ... unknown element symbol: "//trim(symbol))
         end if
      end if

   end subroutine

   subroutine set_coppensbasis_label(self,label)
    type(atom_type) :: self
    ! Set the coppensbasis "label".
      character(*) :: label

      if (.not. associated(self%coppensbasis)) call create_(self%coppensbasis)
      call set_label_(self%coppensbasis,label)

   end subroutine

!  ************
!  I/O Routines
!  ************

   subroutine read_keywords(self)
    type(atom_type) :: self
    ! Read data from "stdin" using keyword style input.
     character(128) :: keyword

   call ensure_(tonto,next_item_(stdin)=="{","ATOM:read_keywords ... expecting an open bracket symbol, {")
     call read_(stdin,keyword)          ! Read opening brace
     do                           ! Loop over keywords
       call read_(stdin,keyword)
       if (keyword=="}")   exit   ! Exit on closing brace
       if (reverted_(stdin)) exit   ! Exit if internal file reverted
       call process_keyword_(self,keyword)
     end do

   end subroutine

   subroutine process_keyword(self,keyword)
    type(atom_type) :: self
    ! Process command "keyword". Any required data needed by the "keyword" is
    ! inputted from "stdin".
     character(*), intent(in) :: keyword
     character(128) :: word

     word = keyword
     call to_lower_case_(word)
     select case (word)
       case ("}                            ")   ! exit case
       case ("axis_system=                 "); call read_axis_system_(self)
       case ("basis=                       "); call read_basis_(self)
       case ("basis_label=                 "); call read_basis_label_(self)
       case ("coppensbasis=                "); call read_coppensbasis_(self)
       case ("group=                       "); call read_group_(self)
       case ("junk=                        "); call read_junk_(self)
       case ("label=                       "); call read_label_(self)
       case ("mm_atom_type=                "); call read_mm_atom_type_(self)
       case ("mm_charge=                   "); call read_mm_charge_(self)
       case ("mm_forcefield_name=          "); call read_mm_forcefield_name_(self)
       case ("pdb_input_line=              "); call read_pdb_input_line_(self)
       case ("pos=                         "); call read_pos_(self)
       case ("position=                    "); call read_pos_(self)
       case ("put                          "); call put_(self)
       case ("residue_atom_name=           "); call read_residue_atom_name_(self)
       case ("residue_name=                "); call read_residue_name_(self)
       case ("restraining_position=        "); call read_restraining_position_(self)
       case ("restraining_force_constant=  "); call read_restraining_force_(self)
       case ("sequence_number=             "); call read_sequence_number_(self)
       case ("site_occupancy=              "); call read_site_occupancy_(self)
       case ("slaterbasis=                 "); call read_slaterbasis_(self)
       case ("thermal_axis_system=         "); call read_thermal_axis_system_(self)
       case ("thermal_tensor=              "); call read_thermal_tensor_(self)
       case ("u_iso=                       "); call read_U_iso_(self)
       case ("u_tensor=                    "); call read_thermal_tensor_(self)
       case ("units=                       "); call read_units_(self)
        ! These are only for making custom tables for the list type
       case ("---For custom tables---      ");
       case ("flush                        "); call flush_(stdout)
       case ("put_atomic_number            "); call put_(stdout,self%atomic_number)
       case ("put_bragg_slater_radius      "); call put_(stdout,bragg_slater_radius_(self))
       case ("put_chemical_symbol          "); call put_(stdout,chemical_symbol_(self),int_width=.true.)
       case ("put_column_number            "); call put_(stdout,column_number_(self))
       case ("put_ground_state_multiplicity"); call put_(stdout,ground_state_multiplicity_(self))
       case ("put_label                    "); call put_(stdout,self%label,int_width=.true.)
       case ("put_mass                     "); call put_(stdout,mass_(self))
       case ("put_mean_neutron_number      "); call put_(stdout,mean_neutron_number_(self))
       case ("put_neutron_scattering_length"); call put_(stdout,neutron_scattering_length_(self))
       case ("put_period_block             "); call put_(stdout,period_block_(self))
       case ("put_period_number            "); call put_(stdout,period_number_(self))
       case ("put_pos                      "); call put_(stdout,self%pos(1))
                                               call put_(stdout,self%pos(2))
                                               call put_(stdout,self%pos(3))
       case ("put_position                 "); call put_(stdout,self%pos(1))
                                               call put_(stdout,self%pos(2))
                                               call put_(stdout,self%pos(3))
       case ("put_thermal_tensor           "); call put_thermal_tensor_(self)
       case  default;                        allocate(tonto%known_keywords(43))
       tonto%known_keywords(1) = "}                            "
       tonto%known_keywords(2) = "axis_system=                 "
       tonto%known_keywords(3) = "basis=                       "
       tonto%known_keywords(4) = "basis_label=                 "
       tonto%known_keywords(5) = "coppensbasis=                "
       tonto%known_keywords(6) = "group=                       "
       tonto%known_keywords(7) = "junk=                        "
       tonto%known_keywords(8) = "label=                       "
       tonto%known_keywords(9) = "mm_atom_type=                "
       tonto%known_keywords(10) = "mm_charge=                   "
       tonto%known_keywords(11) = "mm_forcefield_name=          "
       tonto%known_keywords(12) = "pdb_input_line=              "
       tonto%known_keywords(13) = "pos=                         "
       tonto%known_keywords(14) = "position=                    "
       tonto%known_keywords(15) = "put                          "
       tonto%known_keywords(16) = "residue_atom_name=           "
       tonto%known_keywords(17) = "residue_name=                "
       tonto%known_keywords(18) = "restraining_position=        "
       tonto%known_keywords(19) = "restraining_force_constant=  "
       tonto%known_keywords(20) = "sequence_number=             "
       tonto%known_keywords(21) = "site_occupancy=              "
       tonto%known_keywords(22) = "slaterbasis=                 "
       tonto%known_keywords(23) = "thermal_axis_system=         "
       tonto%known_keywords(24) = "thermal_tensor=              "
       tonto%known_keywords(25) = "u_iso=                       "
       tonto%known_keywords(26) = "u_tensor=                    "
       tonto%known_keywords(27) = "units=                       "
       tonto%known_keywords(28) = "---For custom tables---      "
       tonto%known_keywords(29) = "flush                        "
       tonto%known_keywords(30) = "put_atomic_number            "
       tonto%known_keywords(31) = "put_bragg_slater_radius      "
       tonto%known_keywords(32) = "put_chemical_symbol          "
       tonto%known_keywords(33) = "put_column_number            "
       tonto%known_keywords(34) = "put_ground_state_multiplicity"
       tonto%known_keywords(35) = "put_label                    "
       tonto%known_keywords(36) = "put_mass                     "
       tonto%known_keywords(37) = "put_mean_neutron_number      "
       tonto%known_keywords(38) = "put_neutron_scattering_length"
       tonto%known_keywords(39) = "put_period_block             "
       tonto%known_keywords(40) = "put_period_number            "
       tonto%known_keywords(41) = "put_pos                      "
       tonto%known_keywords(42) = "put_position                 "
       tonto%known_keywords(43) = "put_thermal_tensor           "
       call unknown_(tonto,word,"ATOM:process_keyword")
       deallocate(tonto%known_keywords)
     end select

   end subroutine

   subroutine read_units(self)
    type(atom_type) :: self
    ! Read a string which describes the units to be used

      call set_default_units_(stdin,next_str_(stdin))

   end subroutine

   subroutine read_junk(self)
    type(atom_type) :: self
    ! Read in a junk string, useful for ignoring a field

      call skip_next_item_(stdin)

   end subroutine

   subroutine read_group(self)
    type(atom_type) :: self
    ! Read the index which describes the group the atom belongs to.

      call read_(stdin,self%group)

   end subroutine

   subroutine read_pos(self)
    type(atom_type) :: self
    ! Read in the position

      call read_(stdin,self%pos)

   end subroutine

   subroutine read_label(self)
    type(atom_type) :: self
    ! Read an type(atom_type) label.

      call set_label_and_atomic_number_(self,next_str_(stdin))   ! Input atom label

   end subroutine

   subroutine read_residue_atom_name(self)
    type(atom_type) :: self
    ! Read the atom name that this atom belongs to a residue (case sensitive).

      call read_(stdin,self%residue_atom_name)

   end subroutine

   subroutine read_residue_name(self)
    type(atom_type) :: self
    ! Read the residue name that this atom belongs to.

      call read_(stdin,self%residue_name)

   end subroutine

   subroutine read_pdb_input_line(self)
    type(atom_type) :: self
    ! Read a line in the PDB input style
      character(128) :: word

      call read_(stdin,word)
      call to_lower_case_(word)
      call ensure_(tonto,word=="atom","ATOM:read_pdb_input_line ... PDB line does not begin with 'atom'")
      call read_residue_atom_name_(self)
      call read_residue_name_(self)
      call read_sequence_number_(self)
      call read_pos_(self)
      call read_junk_(self)
      call read_junk_(self)
      call read_label_(self)

   end subroutine

   subroutine read_sequence_number(self)
    type(atom_type) :: self
    ! Read the sequence number of the residue in the molecule

      call read_(stdin,self%sequence_number)

   end subroutine

   subroutine read_mm_atom_type(self)
    type(atom_type) :: self
    ! Read the atom type potential, used to define the force field potential

      call read_(stdin,self%mm_atom_type)

   end subroutine

   subroutine read_mm_charge(self)
    type(atom_type) :: self
    ! Read the MM charge on this atom

      call read_(stdin,self%mm_charge)

   end subroutine

   subroutine read_mm_forcefield_name(self)
    type(atom_type) :: self
    ! Read the MM forcefield name for this atom

      call read_(stdin,self%mm_forcefield_name)

   end subroutine

   subroutine read_restraining_position(self)
    type(atom_type) :: self
    ! Read the restraining position

      call read_(stdin,self%restraining_position)

   end subroutine

   subroutine read_restraining_force(self)
    type(atom_type) :: self
    ! Read the restrain force constant for this atom

      call read_(stdin,self%restraining_force_constant)

   end subroutine

   subroutine read_site_occupancy(self)
    type(atom_type) :: self
    ! Read the site occupancy, used mainly for crystallographic applications.

      call read_(stdin,self%site_occupancy)

   end subroutine

   subroutine read_axis_system(self)
    type(atom_type) :: self
    ! Read a string which describes the axis system. Currently allowed is
    ! "cartesian" or "crystal". NOTE: .thermal_axis_system is changed too.

      call read_(stdin,self%axis_system)
      self%thermal_axis_system = self%axis_system
      select case (self%axis_system)
        case ("cartesian")
        case ("crystal  ")
        case default
          allocate(tonto%known_keywords(2))
          tonto%known_keywords(1) = "cartesian"
          tonto%known_keywords(2) = "crystal  "
          call unknown_(tonto,self%axis_system,"ATOM:read_axis_system")
          deallocate(tonto%known_keywords)
      end select

   end subroutine

   subroutine set_axis_system(self,word)
    type(atom_type) :: self
    ! Read a string which describes the axis system. Currently allowed is
    ! "cartesian" or "crystal". NOTE: .thermal_axis_system is changed too.
     character(*), intent(in) :: word

     self%axis_system = word
     self%thermal_axis_system = self%axis_system
     select case (self%axis_system)
       case ("cartesian")
       case ("crystal  ")
       case default
         allocate(tonto%known_keywords(2))
         tonto%known_keywords(1) = "cartesian"
         tonto%known_keywords(2) = "crystal  "
         call unknown_(tonto,self%axis_system,"ATOM:set_axis_system")
         deallocate(tonto%known_keywords)
     end select

   end subroutine

   subroutine read_thermal_axis_system(self)
    type(atom_type) :: self
    ! Read a string which describes the thermal tensor axis system.
    ! Currently allowed, is "cartesian" or "crystal".

      call read_(stdin,self%thermal_axis_system)
      select case (self%thermal_axis_system)
        case ("cartesian")
        case ("crystal  ")
        case default
          allocate(tonto%known_keywords(2))
          tonto%known_keywords(1) = "cartesian"
          tonto%known_keywords(2) = "crystal  "
          call unknown_(tonto,self%thermal_axis_system,"ATOM:read_thermal_axis_system")
          deallocate(tonto%known_keywords)
      end select

   end subroutine

   subroutine set_thermal_axis_system(self,word)
    type(atom_type) :: self
    ! Read a string which describes the thermal tensor axis system.
    ! Currently allowed, is "cartesian" or "crystal".
     character(*), intent(in) :: word

     self%thermal_axis_system = word
     select case (self%thermal_axis_system)
       case ("cartesian")
       case ("crystal  ")
       case default
         allocate(tonto%known_keywords(2))
         tonto%known_keywords(1) = "cartesian"
         tonto%known_keywords(2) = "crystal  "
         call unknown_(tonto,self%thermal_axis_system,"ATOM:set_thermal_axis_system")
         deallocate(tonto%known_keywords)
     end select

   end subroutine

   subroutine read_basis(self)
    type(atom_type) :: self
    ! Read the basis set from "stdin"

      call create_(self%basis)
      call read_keywords_(self%basis)

   end subroutine

   subroutine read_basis_label(self)
    type(atom_type) :: self
    ! Read the basis set label from "stdin" which will be used to match a basis
    ! set to ...

      call read_(stdin,self%basis_label)

   end subroutine

   subroutine read_slaterbasis(self)
    type(atom_type) :: self
    ! Read the slaterbasis set from "stdin"

      call create_(self%slaterbasis)
      call read_keywords_(self%slaterbasis)

   end subroutine

   subroutine read_coppensbasis(self)
    type(atom_type) :: self
    ! Read the coppensbasis set from "stdin"

      call create_(self%coppensbasis)
      call read_keywords_(self%coppensbasis)

   end subroutine

   subroutine read_U_iso(self)
    type(atom_type) :: self
    ! Read the isotropic thermal parameters from "stdin".  NOTE: units are
    ! Bohr^2, not Angstrom^2.

      call read_(stdin,self%U_iso)

   end subroutine

   subroutine read_thermal_tensor(self)
    type(atom_type) :: self
    ! Read thermal parameters from "stdin". These are assumed to be in the
    ! cartesian axis system, and in bohr^2.  The expansion of the thermal
    ! smearing temperature factor term is:
    !              TF = exp ( -2\pi^2 U_{ij} h_i h_j a^*_i a^*_j )
    ! where (h) are the miller indices and (a^*) are the reciprocal lattice
    ! constants in angstrom^{-2}.
      real(kind=kind(1.0d0)), dimension(6) :: tensor

      call read_(stdin,tensor)
      self%thermal_tensor(1,1) = tensor(1)  ! Units must be bohr^2
      self%thermal_tensor(2,2) = tensor(2)  ! unless over-ridden by read_units=
      self%thermal_tensor(3,3) = tensor(3)
      self%thermal_tensor(1,2) = tensor(4)
      self%thermal_tensor(1,3) = tensor(5)
      self%thermal_tensor(2,3) = tensor(6)
      self%thermal_tensor(2,1) = self%thermal_tensor(1,2)
      self%thermal_tensor(3,1) = self%thermal_tensor(1,3)
      self%thermal_tensor(3,2) = self%thermal_tensor(2,3)

   end subroutine

!  ********************
!  Key related routines
!  ********************

   subroutine read_keys(self)
    type(atom_type) :: self
    ! Read the "keys".

     call clear_keys_(self)
     call read_ptr_(stdin,keys)
     call ignore_memory_leak_(tonto,memory_blocks_gained=1)

   end subroutine

   subroutine process_keys(self)
    type(atom_type) :: self
    ! Process each of the words in the "keys" list.
      integer(kind=kind(1)) :: k,l,n_key
      character(128) :: keyword
      character(128), dimension(:), pointer :: internal

   call ensure_(tonto,associated(keys),"ATOM:process_keys ... no keys")
      n_key = size(keys)
      k = 0
      do
         k = k + 1
         keyword = keys(k)
         if (keyword=="}") exit
         if (keyword=="{") then
            l = index_of_matching_bracket_(keys(k:),"{")
            call ensure_(tonto,l>0,"ATOM:process_keys ... no matching closing brace, }")
            internal => keys(k:k+l-1)
            call redirect_(stdin,internal)
            call read_keywords_(self)
            call revert_(stdin)
            k = k+l-1
         else if (includes_(keyword," ")) then
            internal => split_(keyword)
            call redirect_(stdin,internal)
            call read_keywords_(self)
            call destroy_(internal)
            call revert_(stdin)
         else
            call process_keyword_(self,keyword)
         end if
         if (k==n_key) exit
      end do

   end subroutine

   function keys_created(self) result(res)
    type(atom_type) :: self
    ! Return .true. if the list-element keys are created.
      logical(kind=kind(.true.)) :: res

      res = associated(keys)

   end function

   subroutine set_keys(self,the_keys)
    type(atom_type) :: self
    ! This is for setting the "keys" externally.
     character(len=*), dimension(:) :: the_keys

     call clear_keys_(self)
     call create_copy_(keys,the_keys)
     call ignore_memory_leak_(tonto,memory_blocks_gained=1)

   end subroutine

   subroutine clear_keys(self)
    type(atom_type) :: self
    ! This is for destroying the "keys" externally.

     if (.not. associated(keys)) then;   return; end if
     call destroy_(keys)
     call ignore_memory_leak_(tonto,memory_blocks_gained=-1)

   end subroutine

   subroutine put_table_footer(self)
    type(atom_type) :: self
    ! Output a table footer from the list of "keys". NOTE: not all keywords need
    ! contribute to the banner - any unrecognised keyword is skipped.

     call dash_(stdout,width=table_width_(self))

   end subroutine

   subroutine put_table_header(self)
    type(atom_type) :: self
    ! Output a table header from the list of "keys". NOTE: not all keywords need
    ! contribute to the banner - any unrecognised keyword is skipped.
     character(128) :: word
     integer(kind=kind(1)) :: width,k

   call ensure_(tonto,associated(keys),"ATOM:put_table_header ... no keys")
     width = table_width_(self)
     if (width > 0) then
       call dash_(stdout,width=width)
       do k = 1,size(keys)
         word = keys(k)
         call to_lower_case_(word)
         select case (word)
           case ("put_label   "); call put_(stdout,"label",int_width=.true.)
           case ("put_n_shells"); call put_(stdout,"n_shells",int_width=.true.)
           case ("put_n_bf    "); call put_(stdout,"n_bf",int_width=.true.)
           case ("put_n_prim  "); call put_(stdout,"n_prim",int_width=.true.)
           case ("flush       "); call flush_(stdout); exit
         end select
         if (k==size(keys)) then
           call flush_(stdout)  ! In case they didn't write one.
           call warn_(tonto,"ATOM:put_table_header ... no flush keyword - you may later overrun the output buffer")
         end if
       end do
       call dash_(stdout,width=width)
     end if

   end subroutine

   function table_width(self) result(res)
    type(atom_type) :: self
    ! Return the table width in characters, based on "keys".  Note that not all
    ! keywords need to contribute to the banner - if a keyword is not recognised,
    ! then it is skipped.
     integer(kind=kind(1)) :: res
     character(128) :: word
     integer(kind=kind(1)) :: int_dash,real_dash,k

     call ensure_(tonto,associated(keys),"ATOM:table_width ... no keys")
     int_dash = 0
     real_dash = 0
     do k = 1,size(keys)
       word = keys(k)
       call to_lower_case_(word)
       select case (word)
         case ("}           ");  ! exit surrounding loop
         case ("put_label   "); int_dash = int_dash + 1
         case ("put_n_shells"); int_dash = int_dash + 1
         case ("put_n_bf    "); int_dash = int_dash + 1
         case ("put_n_prim  "); int_dash = int_dash + 1
         case ("flush       "); exit
         case default
       end select
     end do
     res = int_dash * stdout%int_width + real_dash * stdout%real_width

   end function

!  *******************
!  Axis change methods
!  *******************

   subroutine resolve_axis_system(self,crystal)
    type(atom_type) :: self
    ! Change the axis system for the position and thermal tensors to
    ! "cartesian". "crystal" holds the axis system information.
     type(crystal_type), intent(in) :: crystal

     if (self%axis_system=="crystal")         call position_from_(self,crystal)
     if (self%thermal_axis_system=="crystal") call thermal_tensor_from_(self,crystal)

   end subroutine

   subroutine change_axis_system_to(self,axiskind,crystal)
    type(atom_type) :: self
    ! Change the axis system for the position and thermal tensors
    ! to "axiskind", either crystal or cartesian.
     character(*), intent(in) :: axiskind
     type(crystal_type), intent(in) :: crystal

     select case (axiskind)
       case ("cartesian"); call position_from_(self,crystal); call thermal_tensor_from_(self,crystal)
       case ("crystal  "); call position_to_(self,crystal);   call thermal_tensor_to_(self,crystal)
       case default
         allocate(tonto%known_keywords(2))
         tonto%known_keywords(1) = "cartesian"
         tonto%known_keywords(2) = "crystal  "
         call unknown_(tonto,axiskind,"ATOM:change_axis_system_to")
         deallocate(tonto%known_keywords)
     end select

   end subroutine

   subroutine change_thermal_axis_system_to(self,axiskind,crystal)
    type(atom_type) :: self
    ! Change the axis system for the thermal tensors to "axiskind",
    ! either crystal or cartesian.
     character(*), intent(in) :: axiskind
     type(crystal_type), intent(in) :: crystal

     select case (axiskind)
       case ("cartesian"); call thermal_tensor_from_(self,crystal)
       case ("crystal  "); call thermal_tensor_to_(self,crystal)
       case default
         allocate(tonto%known_keywords(2))
         tonto%known_keywords(1) = "cartesian"
         tonto%known_keywords(2) = "crystal  "
         call unknown_(tonto,axiskind,"ATOM:change_thermal_axis_system_to")
         deallocate(tonto%known_keywords)
     end select

   end subroutine

   subroutine thermal_tensor_from(self,crystal)
    type(atom_type) :: self
    ! Change thermal parameters in the crystal axis system to the
    ! cartesian axis system
     type(crystal_type), intent(in) :: crystal

     select case (self%thermal_axis_system)
       case ("crystal  ")
         if (self%U_iso>0.0d0) then  ! convert U_iso first
            self%thermal_tensor(1,1) = self%U_iso
            self%thermal_tensor(2,2) = self%U_iso
            self%thermal_tensor(3,3) = self%U_iso
            self%thermal_tensor(1,2) = self%U_iso * cos(gamma_star_(crystal%unitcell))
            self%thermal_tensor(1,3) = self%U_iso * cos(beta_star_(crystal%unitcell))
            self%thermal_tensor(2,3) = self%U_iso * cos(alpha_star_(crystal%unitcell))
         end if
         call change_basis_(self%thermal_tensor,crystal%unitcell%direct_U_matrix)
         self%thermal_axis_system = "cartesian"
       case ("cartesian")
          ! do nothing
       case default
         allocate(tonto%known_keywords(2))
         tonto%known_keywords(1) = "crystal  "
         tonto%known_keywords(2) = "cartesian"
         call unknown_(tonto,self%thermal_axis_system,"ATOM:thermal_tensor_from")
         deallocate(tonto%known_keywords)
     end select

   end subroutine

   subroutine thermal_tensor_to(self,crystal)
    type(atom_type) :: self
    ! Change thermal parameters in the cartesian axis system to the
    ! crystal axis system
     type(crystal_type), intent(in) :: crystal

     select case (self%thermal_axis_system)
       case ("crystal  ")
          ! do nothing
       case ("cartesian")
         call change_basis_(self%thermal_tensor,crystal%unitcell%reciprocal_U_matrix)
         self%thermal_axis_system = "crystal"
       case default
         allocate(tonto%known_keywords(2))
         tonto%known_keywords(1) = "crystal  "
         tonto%known_keywords(2) = "cartesian"
         call unknown_(tonto,self%thermal_axis_system,"ATOM:thermal_tensor_to")
         deallocate(tonto%known_keywords)
     end select

   end subroutine

   subroutine position_from(self,crystal)
    type(atom_type) :: self
    ! Change atom positions in the crystal axis system to the
    ! cartesian axis system
     type(crystal_type), intent(in) :: crystal

     select case (self%axis_system)
       case ("crystal  ")
         self%pos = matmul(crystal%unitcell%direct_matrix,self%pos)
         self%axis_system = "cartesian"
       case ("cartesian")
          ! do nothing
       case default
         allocate(tonto%known_keywords(2))
         tonto%known_keywords(1) = "crystal  "
         tonto%known_keywords(2) = "cartesian"
         call unknown_(tonto,self%axis_system,"ATOM:position_from")
         deallocate(tonto%known_keywords)
     end select

   end subroutine

   subroutine position_to(self,crystal)
    type(atom_type) :: self
    ! Change atom positions in the cartesian axis system to the
    ! the crystal axis system
     type(crystal_type), intent(in) :: crystal

     select case (self%axis_system)
       case ("crystal  ")
          ! do nothing
       case ("cartesian")
         self%pos = matmul(crystal%unitcell%inverse_matrix,self%pos)
         self%axis_system = "crystal"
       case default
         allocate(tonto%known_keywords(2))
         tonto%known_keywords(1) = "crystal  "
         tonto%known_keywords(2) = "cartesian"
         call unknown_(tonto,self%axis_system,"ATOM:position_to")
         deallocate(tonto%known_keywords)
     end select

   end subroutine

!  ************************
!  Basis resolution methods
!  ************************

!   resolve_basis(basis,clobber,found) ::: leaky
!   ! Resolve the basis set by matching the basis set label with one of the
!   ! labels from the basis set vector "basis". If "clobber" is present and .true.
!   ! (the default situation), then the matched basis is pointer assigned to the
!   ! matching element in "basis" irrespective of whether it is already
!   ! associated; otherwise if the matching basis set is already assigned, it is
!   ! not pointer assigned. If present, "found" is set .true. if the basis set has
!   ! been resolved (or was already resolved if clobber was not set), or false
!   ! otherwise. If "found" is not present, and a match has not been found, an
!   ! error is generated
!      basis :: BASISVEC*
!      clobber,found :: logical(kind=kind(.true.)), optional
!      b :: integer(kind=kind(1))
!      check :: BINVEC*
!      force,fnd :: logical(kind=kind(.true.))
!   call ensure_(tonto,basis.created,"no basis set")
!      force = .true.
!      if (present(clobber)) force = clobber
!      if (.basis.destroyed) then
!         if (present(found)) found = .false.
!         return
!      end
!      if (.not. force) then
!         if (present(found)) found = .true.
!         return
!      end
!      check.create(basis.dim)
!      check = basis.label==.basis.label
!      b = check.index_of_first_true_element
!      check.destroy
!      fnd = b>0
!      if (fnd) then
!      !  .basis.destroy     ! don't destroy
!         .basis => basis(b) ! NOTE : this is a pointer assign .not. COPY
!      end
!      if (present(found)) then; found = fnd
!      else; call ensure_(tonto,fnd,"unknown basis, "// trim(.basis.label))
!      end
!   end
!
!   resolve_library_basis(basis,clobber,found) ::: leaky
!   ! Resolve the basis set by first looking in the "basis" list, and then (if
!   ! needed) looking in a basis set library file. The appropriate basis set
!   ! library files are obtained from the basis set qualifier -- the part after
!   ! the colon in the atom basis set label. For example, if the atom basis set
!   ! label is "H:DZP", then the qualifier is "DZP" and the routine looks in
!   ! library file basis_sets/"DZP" for a matching basis set. If found, the basis
!   ! set is appended to "basis". If "clobber" is present and .true. (the default
!   ! situation), then the matched basis is pointer assigned to the matching
!   ! element in "basis" irrespective of whether it is already associated;
!   ! otherwise if the matching basis set is already associated, it is not
!   ! pointer assigned.  If present, "found" is set .true. if the basis set has
!   ! been resolved, or false otherwise. If "found" is not present, and a match
!   ! has not been found, an error is generated
!   ! NOTE: this should probably not be used ... .basis should be filled first
!   ! with the right bases and then they should be resolved in one hit.
!      basis :: BASISVEC*
!      clobber,found :: logical(kind=kind(.true.)), optional
!      i :: integer(kind=kind(1))
!      basis_label,basis_kind,library :: STR
!      force,fnd :: logical(kind=kind(.true.))
!   call ensure_(tonto,.basis.created,"no basis set")
!      force = .true.
!      if (present(clobber)) force = clobber
!      if (.basis.created .and. .not. force) then
!         if (present(found)) found = .true.
!         return
!      end
!      if (basis.created) .resolve_basis(basis,clobber=.true.,found=fnd)
!      if (fnd) return
!      basis_label = .basis.label                 ! Look for this <<<<<<<
!      if (basis_label.includes(":")) then        ! look in library directory
!         i = basis_label.index_of_substring(":")
!         if (i>0) then
!            basis_kind = basis_label(i+1:)
!            if (basis_kind/=" ") then
!               library = basis.library_directory(basis_kind)
!               basis.read_library_data(library,[basis_label])
!               ! .basis.destroy  ! don't destroy ????
!               .basis => basis(basis.dim)  ! NOTE : this is a pointer assign .not. COPY
!               fnd = .true.
!            end
!         end
!      end
!      if (present(found)) then; found = fnd
!      else; call ensure_(tonto,fnd,"unknown basis, "// trim(.basis.label))
!      end
!   end
!
!   resolve_basis_suffix(basis,suffix,clobber,found) ::: leaky
!   ! Resolve the basis set by first making a standard basis label, by joining
!   ! the atom chemical symbol with the :"suffix" string, and then trying to find
!   ! a match with one of the "basis" set vector labels.  If "clobber" is present
!   ! and .false., then only an unassociated .basis is resolved.  If "found" is
!   ! present and it is set .true. if there was a match, otherwise .false.; and if it
!   ! is not present an error is generated if no match is found.
!      basis :: BASISVEC*
!      suffix :: character(*)
!      clobber,found :: logical(kind=kind(.true.)), optional
!      label :: STR
!      b :: integer(kind=kind(1))
!      force,fnd :: logical(kind=kind(.true.))
!   call ensure_(tonto,basis.created,"no basis")
!      force = .true.
!      if (present(clobber)) force = clobber
!      if (.basis.created .and. .not. force) then
!         if (present(found)) found = .true.
!         return
!      end
!      label = .library_basis_label(suffix)
!      fnd = .false.
!      do b = 1,basis.dim
!         if (basis(b).label.same_as(label,ignore_case=.true.)) then
!            fnd = .true.
!            exit
!         end
!      end
!      if (fnd) then
!      !  .basis.destroy     ! don't destroy
!         .basis => basis(b) ! NOTE : this is a pointer assign .not. COPY
!      end
!      if (present(found)) then; found = fnd
!      else; call ensure_(tonto,fnd,"unknown basis, "// trim(.basis.label))
!      end
!   end
!
!   resolve_basis(basis,clobber,found) ::: leaky
!   ! Resolve the basis set by matching the basis set label with one of the
!   ! labels from the basis set vector "basis". If "clobber" is present and .true.
!   ! (the default situation), then the matched basis is pointer assigned to the
!   ! matching element in "basis" irrespective of whether it is already
!   ! associated; otherwise if the matching basis set is already assigned, it is
!   ! not pointer assigned. If present, "found" is set .true. if the basis set has
!   ! been resolved (or was already resolved if clobber was not set), or false
!   ! otherwise. If "found" is not present, and a match has not been found, an
!   ! error is generated
!      basis :: SLATERBASISVEC*
!      clobber,found :: logical(kind=kind(.true.)), optional
!      b :: integer(kind=kind(1))
!      check :: BINVEC*
!      force,fnd :: logical(kind=kind(.true.))
!   call ensure_(tonto,basis.created,"no Coppens basis set")
!      force = .true.
!      if (present(clobber)) force = clobber
!      if (.slaterbasis.destroyed) then
!         if (present(found)) found = .false.
!         return
!      end
!      if (.slaterbasis.created .and. .not. force) then
!         if (present(found)) found = .true.
!         return
!      end
!      check.create(basis.dim)
!      check = basis.label==.slaterbasis.label
!      b = check.index_of_first_true_element
!      check.destroy
!      fnd = b>0
!      if (fnd) then
!      !  .slaterbasis.destroy     ! don't destroy
!         .slaterbasis => basis(b) ! NOTE : this is a pointer assign .not. COPY
!      end
!      if (present(found)) then; found = fnd
!      else; call ensure_(tonto,fnd,"unknown basis, "// trim(.slaterbasis.label))
!      end
!   end
!
!   resolve_library_basis(basis,clobber,found) ::: leaky
!   ! Resolve the basis set by first looking in the "basis" list, and then (if
!   ! needed) looking in a basis set library file. The appropriate basis set
!   ! library files are obtained from the basis set qualifier -- the part after
!   ! the colon in the atom basis set label. For example, if the atom basis set
!   ! label is "H:DZP", then the qualifier is "DZP" and the routine looks in
!   ! library file basis_sets/"DZP" for a matching basis set. If found, the basis
!   ! set is appended to "basis". If "clobber" is present and .true. (the default
!   ! situation), then the matched basis is pointer assigned to the matching
!   ! element in "basis" irrespective of whether it is already associated;
!   ! otherwise if the matching basis set is already associated, it is not
!   ! pointer assigned.  If present, "found" is set .true. if the basis set has
!   ! been resolved, or false otherwise. If "found" is not present, and a match
!   ! has not been found, an error is generated
!      basis :: SLATERBASISVEC*
!      clobber,found :: logical(kind=kind(.true.)), optional
!      i :: integer(kind=kind(1))
!      basis_label,basis_kind,library :: STR
!      force,fnd :: logical(kind=kind(.true.))
!   call ensure_(tonto,.slaterbasis.created,"no Slater basis set")
!      force = .true.
!      if (present(clobber)) force = clobber
!      if (.slaterbasis.created .and. .not. force) then
!         if (present(found)) found = .true.
!         return
!      end
!      if (basis.created) .resolve_basis(basis,clobber=.true.,found=fnd)
!      if (fnd) return
!      basis_label = .slaterbasis.label                 ! Look for this <<<<<<<
!      if (basis_label.includes(":")) then        ! look in library directory
!         i = basis_label.index_of_substring(":")
!         if (i>0) then
!            basis_kind = basis_label(i+1:)
!            if (basis_kind/=" ") then
!               library = basis.library_directory(basis_kind)
!               basis.read_library_data(library,[basis_label])
!               ! .slaterbasis.destroy  ! don't destroy ????
!               .slaterbasis => basis(basis.dim)  ! NOTE : this is a pointer assign .not. COPY
!               fnd = .true.
!            end
!         end
!      end
!      if (present(found)) then; found = fnd
!      else; call ensure_(tonto,fnd,"unknown basis, "// trim(.slaterbasis.label))
!      end
!   end
!
!   resolve_basis_suffix(basis,suffix,clobber,found) ::: leaky
!   ! Resolve the basis set by first making a standard basis label, by joining
!   ! the atom chemical symbol with the :"suffix" string, and then trying to find
!   ! a match with one of the "basis" set vector labels.  If "clobber" is present
!   ! and .false., then only an unassociated .slaterbasis is resolved.  If "found" is
!   ! present and it is set .true. if there was a match, otherwise .false.; and if it
!   ! is not present an error is generated if no match is found.
!      basis :: SLATERBASISVEC*
!      suffix :: character(*)
!      clobber,found :: logical(kind=kind(.true.)), optional
!      label :: STR
!      b :: integer(kind=kind(1))
!      force,fnd :: logical(kind=kind(.true.))
!   call ensure_(tonto,basis.created,"no Coppens basis")
!      force = .true.
!      if (present(clobber)) force = clobber
!      if (.slaterbasis.created .and. .not. force) then
!         if (present(found)) found = .true.
!         return
!      end
!      label = .library_basis_label(suffix)
!      fnd = .false.
!      do b = 1,basis.dim
!         if (basis(b).label.same_as(label,ignore_case=.true.)) then
!            fnd = .true.
!            exit
!         end
!      end
!      if (fnd) then
!      !  .slaterbasis.destroy     ! don't destroy
!         .slaterbasis => basis(b) ! NOTE : this is a pointer assign .not. COPY
!      end
!      if (present(found)) then; found = fnd
!      else; call ensure_(tonto,fnd,"unknown basis, "// trim(.slaterbasis.label))
!      end
!   end

!   resolve_basis(basis,clobber,found)
!   ! Resolve the basis set by matching the basis set label with one of the
!   ! labels from the basis set vector "basis". If "clobber" is present and .true.
!   ! (the default situation), then the matched basis is pointer assigned to the
!   ! matching element in "basis" irrespective of whether it is already
!   ! associated; otherwise if the matching basis set is already assigned, it is
!   ! not pointer assigned. If present, "found" is set .true. if the basis set has
!   ! been resolved (or was already resolved if clobber was not set), or false
!   ! otherwise. If "found" is not present, and a match has not been found, an
!   ! error is generated
!      basis :: COPPENSBASISVEC*
!      clobber,found :: logical(kind=kind(.true.)), optional
!      b :: integer(kind=kind(1))
!      check :: BINVEC*
!      force,fnd :: logical(kind=kind(.true.))
!   call ensure_(tonto,basis.created,"no Coppens basis set")
!      force = .true.
!      if (present(clobber)) force = clobber
!      stdout.text("1==")
!      if (.coppensbasis.destroyed) then
!         if (present(found)) found = .false.
!         return
!      end
!      stdout.text("2==")
!      if (.coppensbasis.created .and. .not. force) then
!      if (.coppensbasis.label/=" ") then
!         if (present(found)) found = .true.
!         return
!      end
!      end
!      stdout.text("2==")
!      check.create(basis.dim)
!      check = basis.label==.coppensbasis.label
!      b = check.index_of_first_true_element
!      check.destroy
!      fnd = b>0
!      stdout.show("coppensbasis label =",.coppensbasis.label)
!      stdout.show("found              =",fnd)
!      if (fnd) then
!      !  .coppensbasis.destroy     ! don't destroy
!         .coppensbasis => basis(b) ! NOTE : this is a pointer assign .not. COPY
!      end
!      if (present(found)) then; found = fnd
!      else; call ensure_(tonto,fnd,"unknown basis, "// trim(.coppensbasis.label))
!      end
!   end

!   set_basis(basis,clobber)
!   ! Set the .coppensbasis to be "basis". If "clobber" is present and .true. (the
!   ! default situation), then .coppensbasis is pointer assigned to the matching
!   ! element in "basis" irrespective of whether it is already associated;
!   ! otherwise it is not pointer assigned.
!      basis :: type(coppensbasis_type)
!      clobber :: logical(kind=kind(.true.)), optional
!      force :: logical(kind=kind(.true.))
!      force = .true.
!      if (present(clobber)) force = clobber
!      if (.coppensbasis.created .and. .not. force) then
!      if (.coppensbasis.label/=" ") then
!         return
!      end
!      end
!      .coppensbasis => basis ! NOTE : this is a pointer assign .not. COPY
!   end

!   resolve_library_basis(basis,clobber,found) ::: leaky
!   ! Resolve the basis set by first looking in the "basis" list, and then (if
!   ! needed) looking in a basis set library file. The appropriate basis set
!   ! library files are obtained from the basis set qualifier -- the part after
!   ! the colon in the atom basis set label. For example, if the atom basis set
!   ! label is "H:DZP", then the qualifier is "DZP" and the routine looks in
!   ! library file basis_sets/"DZP" for a matching basis set. If found, the basis
!   ! set is appended to "basis". If "clobber" is present and .true. (the default
!   ! situation), then the matched basis is pointer assigned to the matching
!   ! element in "basis" irrespective of whether it is already associated;
!   ! otherwise if the matching basis set is already associated, it is not
!   ! pointer assigned.  If present, "found" is set .true. if the basis set has
!   ! been resolved, or false otherwise. If "found" is not present, and a match
!   ! has not been found, an error is generated
!      basis :: COPPENSBASISVEC*
!      clobber,found :: logical(kind=kind(.true.)), optional
!      i :: integer(kind=kind(1))
!      basis_label,basis_kind,library :: STR
!      force,fnd :: logical(kind=kind(.true.))
!   call ensure_(tonto,.coppensbasis.created,"no Coppens basis set")
!      force = .true.
!      if (present(clobber)) force = clobber
!      if (.coppensbasis.created .and. .not. force) then
!         if (present(found)) found = .true.
!         return
!      end
!      if (basis.created) .resolve_basis(basis,clobber=.true.,found=fnd)
!      if (fnd) return
!      basis_label = .coppensbasis.label                 ! Look for this <<<<<<<
!      if (basis_label.includes(":")) then        ! look in library directory
!         i = basis_label.index_of_substring(":")
!         if (i>0) then
!            basis_kind = basis_label(i+1:)
!            if (basis_kind/=" ") then
!               library = basis.library_directory(basis_kind)
!               basis.read_library_data(library,[basis_label])
!               ! .coppensbasis.destroy  ! don't destroy ?
!               .coppensbasis => basis(basis.dim)  ! NOTE : this is a pointer assign .not. COPY
!               fnd = .true.
!            end
!         end
!      end
!      if (present(found)) then; found = fnd
!      else; call ensure_(tonto,fnd,"unknown basis, "// trim(.coppensbasis.label))
!      end
!   end

   subroutine resolve_basis(self,basis,suffix,found)
    type(atom_type) :: self
    ! Resolve the .basis by firstly trying to matching the ".basis_label"
    ! with one of the labels from "basis". If that fails, and "suffix" is present
    ! and not blank, then a library basis label is generated from the "suffix"
    ! and we try and match again to one of the labels in "basis". If present,
    ! "found" is set .true. if the basis set has been resolved, or .false. otherwise.
      type(basis_type), dimension(:), pointer :: basis
      character(*), optional :: suffix
      logical(kind=kind(.true.)), optional :: found
      logical(kind=kind(.true.)) :: fnd

   call ensure_(tonto,associated(basis),"ATOM:resolve_basis ... no basis")
      fnd = .false.
      if (self%basis_label/=" ") then
         call resolve_by_label_(self%basis,self%basis_label,basis,clobber=.true.,found=fnd)
      end if
      if (.not. fnd .and. present(suffix)) then
      if (suffix/=" ") then
         call resolve_by_label_(self%basis,library_basis_label_(self,suffix),basis,clobber=.true.,found=fnd)
      end if
      end if
      if (present(found)) found = fnd

   end subroutine

   subroutine resolve_basis_1(self,basis,suffix,found)
    type(atom_type) :: self
    ! Resolve the .slaterbasis by firstly trying to matching the ".basis_label"
    ! with one of the labels from "basis". If that fails, and "suffix" is present
    ! and not blank, then a library basis label is generated from the "suffix"
    ! and we try and match again to one of the labels in "basis". If present,
    ! "found" is set .true. if the basis set has been resolved, or .false. otherwise.
      type(slaterbasis_type), dimension(:), pointer :: basis
      character(*), optional :: suffix
      logical(kind=kind(.true.)), optional :: found
      logical(kind=kind(.true.)) :: fnd

   call ensure_(tonto,associated(basis),"ATOM:resolve_basis_1 ... no basis")
      fnd = .false.
      if (self%basis_label/=" ") then
         call resolve_by_label_(self%slaterbasis,self%basis_label,basis,clobber=.true.,found=fnd)
      end if
      if (.not. fnd .and. present(suffix)) then
      if (suffix/=" ") then
         call resolve_by_label_(self%slaterbasis,library_basis_label_(self,suffix),basis,clobber=.true.,found=fnd)
      end if
      end if
      if (present(found)) found = fnd

   end subroutine

   subroutine resolve_basis_2(self,basis,suffix,found)
    type(atom_type) :: self
    ! Resolve the .coppensbasis by firstly trying to matching the ".basis_label"
    ! with one of the labels from "basis". If that fails, and "suffix" is present
    ! and not blank, then a library basis label is generated from the "suffix"
    ! and we try and match again to one of the labels in "basis". If present,
    ! "found" is set .true. if the basis set has been resolved, or .false. otherwise.
      type(coppensbasis_type), dimension(:), pointer :: basis
      character(*), optional :: suffix
      logical(kind=kind(.true.)), optional :: found
      logical(kind=kind(.true.)) :: fnd

   call ensure_(tonto,associated(basis),"ATOM:resolve_basis_2 ... no basis")
      fnd = .false.
      if (self%basis_label/=" ") then
         call resolve_by_label_(self%coppensbasis,self%basis_label,basis,clobber=.true.,found=fnd)
      end if
      if (.not. fnd .and. present(suffix)) then
      if (suffix/=" ") then
         call resolve_by_label_(self%coppensbasis,library_basis_label_(self,suffix),basis,clobber=.true.,found=fnd)
      end if
      end if
      if (present(found)) found = fnd

   end subroutine

!   resolve_by_basis_label(basis,label,clobber,found)
!   ! Resolve the .coppensbasis set by matching the ".basis_label" with one of
!   ! the labels from "basis". IF "label" is present it is used instead of
!   ! ".basis_label". If "clobber" is present and .true. (the default situation),
!   ! then .coppensbasis is pointer assigned to the matching element in "basis"
!   ! irrespective of whether it is already associated; otherwise it is not
!   ! pointer assigned. If present, "found" is set .true. if the basis set has been
!   ! resolved (or was already resolved if clobber was not set), or false
!   ! otherwise. If "found" is not present, and a match has not been found, an
!   ! error is generated
!      basis :: COPPENSBASISVEC*
!      label :: STR
!      clobber,found :: logical(kind=kind(.true.)), optional
!   call ensure_(tonto,basis.created,"no Coppens basis")
!      .coppensbasis.resolve_by_label(label,basis,clobber,found)
!   end
!
!   resolve_by_basis_suffix(basis,suffix,clobber,found)
!   ! Resolve the basis set by first making a standard basis label, by joining
!   ! the atom chemical symbol with the ":suffix" string, and then trying to find
!   ! a match with one of the "basis" set vector labels.  If "clobber" is present
!   ! and .false., then only an unassociated .coppensbasis is resolved.  If "found" is
!   ! present and it is set .true. if there was a match, otherwise .false.; and if it
!   ! is not present an error is generated if no match is found.
!      basis :: COPPENSBASISVEC*
!      suffix :: character(*)
!      clobber,found :: logical(kind=kind(.true.)), optional
!      label :: STR
!   call ensure_(tonto,basis.created,"no Coppens basis")
!      label = .library_basis_label(suffix)
!      .coppensbasis.resolve_by_label(label,basis,clobber,found)
!   end

!  ***************
!  Inquiry methods
!  ***************

   function same_kind_as(self,atom) result(res)
    type(atom_type) :: self
    ! Return .true. if self is the same kind of atom as "atom". The position and
    ! label are not compared since the same kind of atom can have a different
    ! position and label.  To check if the basis set is the same, only the
    ! label is used.
      type(atom_type) :: atom
      logical(kind=kind(.true.)) :: res
      logical(kind=kind(.true.)) :: bases_created,coppens_created,bases_destroyed,coppens_destroyed
      logical(kind=kind(.true.)) :: mixed

      bases_created = associated(self%basis) .and. associated(atom%basis)
      coppens_created = associated(self%coppensbasis) .and. associated(atom%coppensbasis)
      bases_destroyed = .not. associated(self%basis) .and. .not. associated(atom%basis)
      coppens_destroyed = .not. associated(self%coppensbasis) .and. .not. associated(atom%coppensbasis)
      mixed = .not. (bases_created .or. bases_destroyed) .or. &
              .not. (coppens_created .or. coppens_destroyed)
      if (mixed) then
         res = .false.
      else
         res = self%atomic_number==atom%atomic_number
         if (bases_created)   res = res .and. self%basis%label==atom%basis%label
         if (coppens_created) res = res .and. self%coppensbasis%label==atom%coppensbasis%label
      end if

   end function

   pure function no_of_shells(self) result(res)
    type(atom_type) :: self
    ! Return the no of shells
      intent(in) :: self
      integer(kind=kind(1)) :: res

      res = no_of_shells_(self%basis)

   end function

   pure function n_shell(self) result(res)
    type(atom_type) :: self
    ! Return the no of shells
      intent(in) :: self
      integer(kind=kind(1)) :: res

      res = self%basis%n_shell

   end function

   pure function no_of_basis_functions(self) result(res)
    type(atom_type) :: self
    ! Evaluate and return the no. of basis functions
      intent(in) :: self
      integer(kind=kind(1)) :: res

      res = no_of_basis_functions_(self%basis)

   end function

   pure function n_bf(self) result(res)
    type(atom_type) :: self
    ! Return the no. of basis functions
      intent(in) :: self
      integer(kind=kind(1)) :: res

      res = self%basis%n_bf

   end function

   pure function no_of_primitives(self) result(res)
    type(atom_type) :: self
    ! Return the no of primitives for this atom
      intent(in) :: self
      integer(kind=kind(1)) :: res

      res = no_of_primitives_(self%basis)

   end function

   pure function n_prim(self) result(res)
    type(atom_type) :: self
    ! Return the no of primitives for this atom
      intent(in) :: self
      integer(kind=kind(1)) :: res

      res = self%basis%n_prim

   end function

   function no_of_occupied_NOs(self,axiskind,tol) result(res)
    type(atom_type) :: self
    ! Returns the number of non-zero occupied natural orbitals. For this purpose,
    ! zero is defined to be "tol" if present, or 10.0d0**(-7) otherwise
      intent(in) :: self
      character(*), optional, intent(in) :: axiskind
      real(kind=kind(1.0d0)), optional, intent(in) :: tol
      integer(kind=kind(1)) :: res

      call ensure_(tonto,associated(self%occupation_numbers),"ATOM:no_of_occupied_NOs ... no occupation numbers")
      res = no_of_occupied_(self%occupation_numbers,axiskind,tol)

   end function

   function chemical_symbol(self,Z) result(res)
    type(atom_type) :: self
    ! Return the chemical symbol for this atom. If "Z" is present then the symbol
    ! returned is the one for the atom with atomic number "Z".
      integer(kind=kind(1)), intent(in), optional :: Z
      character(2) :: res
      integer(kind=kind(1)) :: atomic_number

      atomic_number = self%atomic_number
      if (present(Z)) atomic_number = Z
      if (atomic_number<1 .or. atomic_number>103) then; res = "??"
      else;                  res = element_symbols(atomic_number)
      end if

   end function

   function chemical_name(self) result(res)
    type(atom_type) :: self
    ! Return the chemical name for this atom
      character(12) :: res

      if (self%atomic_number<1 .or. self%atomic_number>103) then; res = "??"
      else;                  res = element_names(self%atomic_number)
      end if

   end function

   function mass(self) result(res)
    type(atom_type) :: self
    ! Return the atomic mass for this atom
      real(kind=kind(1.0d0)) :: res

      if (self%atomic_number<1 .or. self%atomic_number>92) then; res = 0.0d0
      else;                  res = atomic_masses(self%atomic_number)
      end if

   end function

   function mean_neutron_number(self) result(res)
    type(atom_type) :: self
    ! Return the average (abundance weighted) number of neutrons for this atom,
    ! calculated roughly by subtracting the number of protons from the atomic mass.
      real(kind=kind(1.0d0)) :: res

      if (self%atomic_number<1 .or. self%atomic_number>92) then; res = 0.0d0
      else;                  res = mass_(self) - self%atomic_number
      end if

   end function

   function bragg_slater_radius(self) result(res)
    type(atom_type) :: self
    ! Return the Bragg-Slater radius for this atom
      real(kind=kind(1.0d0)) :: res

      call ensure_(tonto,self%atomic_number > 0,"ATOM:bragg_slater_radius ... atomic number less than 1")
      if (self%atomic_number>54) then; res = 0.0d0
      else;                  res = bragg_slater_radii(self%atomic_number)
      end if

   end function

   function neutron_scattering_length(self) result(res)
    type(atom_type) :: self
    ! Return the neutron scattering length for this atom
      real(kind=kind(1.0d0)) :: res

      if (self%atomic_number<1 .or. self%atomic_number>95) then; res = 0.0d0
      else;            res = neutron_scattering_lengths(self%atomic_number)
      end if

   end function

   function period_number(self,Z) result(p)
    type(atom_type) :: self
    ! Return the period (i.e. row) on which the atom lies.
    ! If "Z" is present it is used as the atomic number.
       integer(kind=kind(1)), optional :: Z
       integer(kind=kind(1)) :: p
      integer(kind=kind(1)) :: atomic_number,noble,n

      atomic_number = self%atomic_number
      if (present(Z)) atomic_number = Z
      p = 1
      if (atomic_number<1) then;   return; end if
      noble = 0
      do
         n = (p+2)/2
         noble = noble + 2*n**2
         if (atomic_number <= noble) exit
         p = p + 1
      end do

   end function

   function column_number(self,Z) result(col)
    type(atom_type) :: self
    ! Return the period column (i.e. row) on which the atom lies.
    ! If "Z" is present it is used as the atomic number.
       integer(kind=kind(1)), optional :: Z
      integer(kind=kind(1)) :: col
      integer(kind=kind(1)) :: atomic_number,p,noble,n

      atomic_number = self%atomic_number
      if (present(Z)) atomic_number = Z
      p = 1
      if (atomic_number<1) then;   return; end if
      noble = 0
      do
         n = (p+2)/2
         noble = noble + 2*n**2
         if (atomic_number <= noble) exit
         p = p + 1
      end do
      noble = noble - 2*n**2
      col = atomic_number - noble

   end function

   function period_block(self,Z) result(b)
    type(atom_type) :: self
    ! Return the period block character in which the atom lies.
    ! If "Z" is present it is used as the atomic number.
       integer(kind=kind(1)), optional :: Z
       character(1) :: b
      integer(kind=kind(1)) :: p,col

      p   = period_number_(self,Z)
      col = column_number_(self,Z)
      if (p<4) then
         select case (col)
            case (1:2);   b = "s"
            case (3:8);   b = "p"
         end select
      else if (p<6) then
         select case (col)
            case (1:2);   b = "s"
            case (3:12);  b = "d"
            case (13:18); b = "p"
         end select
      else if (p<8) then
         select case (col)
            case (1:2);   b = "s"
            case (3:16);  b = "f"
            case (17:26); b = "d"
            case (27:32); b = "p"
         end select
      else
         call die_(tonto,"ATOM:period_block ... cannot assign for period "// trim(to_str_(p)))
      end if

   end function

   function ground_state_multiplicity(self,Z) result(mult)
    type(atom_type) :: self
    ! Return the ground state multiplicity for this atom according to Hunds rule
    ! (Note this is not neccesarily the real ground state, esp. for Cu)
    ! If "Z" is present it is used as the atomic number.
       integer(kind=kind(1)), optional :: Z
      integer(kind=kind(1)) :: mult
      integer(kind=kind(1)) :: p,col

      p   = period_number_(self,Z)
      col = column_number_(self,Z)
      if (p<4) then
         select case (col)
            case (2,8);   mult = 1
            case (1,3,7); mult = 2
            case (4,6);   mult = 3
            case (5);     mult = 4
         end select
      else if (p<6) then
         select case (col)
            case (2,12,18);      mult = 1
            case (1,3,11,13,17); mult = 2
            case (4,10,14,16);   mult = 3
            case (5,9,15);       mult = 4
            case (6,8);          mult = 5
            case (7);            mult = 6
         end select
      else if (p<8) then
         select case (col)
            case (2,16,26,32);         mult = 1
            case (1,3,15,17,25,27,31); mult = 2
            case (4,14,18,24,28,30);   mult = 3
            case (5,13,19,23,29);      mult = 4
            case (6,12,20,22);         mult = 5
            case (7,11,21);            mult = 6
            case (8,10);               mult = 7
            case (9);                  mult = 8
         end select
      else
         call die_(tonto,"ATOM:ground_state_multiplicity ... cannot assign for period "// trim(to_str_(p)))
      end if

   end function

   function dispersion_correction(self,wavelength) result(res)
    type(atom_type) :: self
    ! The dispersion correction which best matches the wavelength.
     intent(in) :: self
     real(kind=kind(1.0d0)) :: wavelength
     complex(kind=kind((1.0d0,1.0d0))) :: res

     if (self%atomic_number<1 .or. self%atomic_number>92) then
       res = 0.0d0
     else
       if (wavelength < 2) then
         res = dispersion_correction_Mo( self%atomic_number )  ! 0.71A/1.34au
       else if (wavelength < 3.5) then
         res = dispersion_correction_Cu( self%atomic_number )  ! 1.542A/2.91au
       else
         res = dispersion_correction_Cr( self%atomic_number )  ! 2.29A/4.33au
       end if
     end if

   end function

   function library_basis_label(self,suffix) result(label)
    type(atom_type) :: self
    ! Return a library basis set label by appending "suffix" to the
    ! chemical symbol.
      character(*) :: suffix
      character(128) :: label
      character(128) :: symbol

      symbol = chemical_symbol_(self)
      select case (suffix)
         case ("Coppens")
            if (self%atomic_number>55) then
               select case (self%atomic_number)
                  case (56);     symbol = "Sr"
                  case (57:71);  symbol = "Y"
                  case (72:87);  symbol = chemical_symbol_(self,self%atomic_number-32)
                  case (88);     symbol = "Sr"
                  case (89:103); symbol = "Y"
               end select
               call warn_(tonto,"ATOM:library_basis_label ... Replaced basis for atom "//trim(chemical_symbol_(self))//" with&
& that for atom "//trim(symbol))
            end if
         case default
      end select
      label = trim(symbol)//":"//trim(suffix)

   end function

   function has_ANO_data(self) result(res)
    type(atom_type) :: self
    ! Return .true. if the ANO data exists for the atom.
      logical(kind=kind(.true.)) :: res

      res = associated(self%natural_orbitals) .and. associated(self%occupation_numbers)

   end function

   function has_basis(self) result(res)
    type(atom_type) :: self
    ! Return .true. if the basis exists
      logical(kind=kind(.true.)) :: res

      res = associated(self%basis)

   end function

   function has_basis_label(self) result(res)
    type(atom_type) :: self
    ! Return .true. if the basis label exists and is not blank
      logical(kind=kind(.true.)) :: res

      if (.not. associated(self%basis)) then;       res = .false.
      else if (self%basis%label==" ") then; res = .false.
      else;                             res = .true.
      end if

   end function

   function min_basis_exponent(self) result(res)
    type(atom_type) :: self
    ! Return the minimum exponent in the basis.
     real(kind=kind(1.0d0)) :: res

     if (associated(self%coppensbasis)) then
       res = min_exponent_(self%coppensbasis)
     else if (associated(self%basis)) then
       res = min_exponent_(self%basis)
     else
       call die_(tonto,"ATOM:min_basis_exponent ... no basis")
     end if

   end function

!  *************************
!  Density plotting routines
!  *************************

   subroutine make_density_grid(self,density_grid,pt)
    type(atom_type) :: self
    ! Work out the electron "density_grid" on "pt" using ".natural orbitals" and
    ! the ".occupation_numbers" vector.
      intent(in) :: self
      real(kind=kind(1.0d0)), dimension(:), intent(out) :: density_grid
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: pt

   call ensure_(tonto,size(density_grid)==size(pt,1),"ATOM:make_density_grid ... inconsistent number of points")
      if      (associated(self%coppensbasis)) then
         call make_density_grid_(self%coppensbasis,density_grid,pt,self%pos)
      else if (associated(self%slaterbasis)) then
         call make_density_grid_(self%slaterbasis,density_grid,pt,self%pos)
      else if (associated(self%basis) .and. associated(self%natural_orbitals)) then
         if (number_kind_(self%natural_orbitals) == "real") then
            call make_density_grid_r_(self,density_grid,pt)
         else
            call make_density_grid_c_(self,density_grid,pt)
         end if
      else
         call die_(tonto,"ATOM:make_density_grid ... Can't made density grid")
      end if
      if (.not. same_as_(self%site_occupancy,1.0d0)) &
         density_grid = self%site_occupancy*density_grid

   end subroutine

   subroutine make_density_grid_r(self,density_grid,pt)
    type(atom_type) :: self
    ! Make the "density_grid" for the supplied points "pt" from restricted real
    ! natural orbitals
     intent(in) :: self
     real(kind=kind(1.0d0)), dimension(:), intent(out) :: density_grid
     real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: pt
     real(kind=kind(1.0d0)), dimension(:), pointer :: NO
     integer(kind=kind(1)) :: n_occ,n

   call ensure_(tonto,size(pt,2)==3,"ATOM:make_density_grid_r ... wrong dimension for points array")
   call ensure_(tonto,size(density_grid)==size(pt,1),"ATOM:make_density_grid_r ... inconsistent number of points")
   call ensure_(tonto,created_(self%natural_orbitals,"restricted"),"ATOM:make_density_grid_r ... no restricted NO's")
   call ensure_(tonto,created_(self%occupation_numbers,"restricted"),"ATOM:make_density_grid_r ... no occupation numbers")
     density_grid = 0.0d0
     call create_(NO,size(pt,1))
     n_occ = no_of_occupied_NOs_(self)
     do n = 1,n_occ
       call make_orbital_grid_r_(self,NO,self%natural_orbitals%restricted(:,n),pt,self%pos)
       density_grid = density_grid &
                    + self%occupation_numbers%restricted(n)*NO*NO
     end do
     call destroy_(NO)

   end subroutine

   subroutine make_density_grid_c(self,density_grid,pt)
    type(atom_type) :: self
    ! Make the "density_grid" for the supplied points "pt" from restricted
    ! complex natural orbitals.
      real(kind=kind(1.0d0)), dimension(:) :: density_grid
      real(kind=kind(1.0d0)), dimension(:,:) :: pt
      complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: NO
      integer(kind=kind(1)) :: n_occ,n

   call ensure_(tonto,size(pt,2)==3,"ATOM:make_density_grid_c ... wrong dimension for points array")
   call ensure_(tonto,size(density_grid)==size(pt,1),"ATOM:make_density_grid_c ... inconsistent number of points")
   call ensure_(tonto,created_(self%natural_orbitals,"restricted_complex"),"ATOM:make_density_grid_c ... no restricted NO's")
   call ensure_(tonto,created_(self%occupation_numbers,"restricted"),"ATOM:make_density_grid_c ... no occupation numbers")
      density_grid = 0.0d0
      call create_(NO,size(pt,1))
      n_occ = no_of_occupied_NOs_(self)
      do n = 1,n_occ
         call make_orbital_grid_c_(self,NO,self%natural_orbitals%restricted_complex(:,n),pt,self%pos)
         density_grid = density_grid &
                      + self%occupation_numbers%restricted(n)*conjg(NO)*NO
      end do
      call destroy_(NO)

   end subroutine

   subroutine make_orbital_grid_r(self,g,orb,pt,pos,square)
    type(atom_type) :: self
    ! Evaluate the orbital density grid "g" for *one* AO-basis coefficient
    ! orbital vector "orb" on a set of grid points "pt" for an atom at position
    ! "pos". If "square" is present and .true., the square of the orbital density
    ! is returned.
      real(kind=kind(1.0d0)), dimension(:), intent(out) :: g
      real(kind=kind(1.0d0)), dimension(:), intent(in) :: orb
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: pt
      real(kind=kind(1.0d0)), dimension(3), intent(in) :: pos
      logical(kind=kind(.true.)), optional, intent(in) :: square
      logical(kind=kind(.true.)) :: sq
      type(shell1_type) :: sh
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: sh_grid
      integer(kind=kind(1)) :: n_pt,f,l,s

   call ensure_(tonto,associated(self%basis),"ATOM:make_orbital_grid_r ... no basis set")
   call ensure_(tonto,no_of_basis_functions_(self%basis)==size(orb),"ATOM:make_orbital_grid_r ... incorrect suize, orb")
      sq = .false.
      if (present(square)) sq = square
      n_pt = size(pt,1)
      g = 0.0d0
      l = 0
      do s = 1,n_shell_(self)
         call set_(sh,self%basis%shell(s),pos)
         f = l + 1
         l = f + sh%n_comp - 1
         call create_(sh_grid,n_pt,sh%n_comp)
         call make_grid_(sh,sh_grid,pt)
         g = g + matmul(sh_grid,orb(f:l))
         call destroy_(sh_grid)
      end do
      if (sq) g = g*g

   end subroutine

   subroutine make_orbital_grid_c(self,g,orb,pt,pos,square)
    type(atom_type) :: self
    ! Evaluate the orbital density grid "g" for *one* AO-basis coefficient
    ! orbital vector "orb" on a set of grid points "pt" for an atom at position
    ! "pos". If "square" is present and .true., the square of the orbital density
    ! is returned.
      intent(in) :: self
      complex(kind=kind((1.0d0,1.0d0))), dimension(:), intent(out) :: g
      complex(kind=kind((1.0d0,1.0d0))), dimension(:), intent(in) :: orb
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: pt
      real(kind=kind(1.0d0)), dimension(3), intent(in) :: pos
      logical(kind=kind(.true.)), optional, intent(in) :: square
      logical(kind=kind(.true.)) :: sq
      type(shell1_type) :: sh
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: sh_grid
      integer(kind=kind(1)) :: n_pt,f,l,s

   call ensure_(tonto,associated(self%basis),"ATOM:make_orbital_grid_c ... no basis set")
   call ensure_(tonto,no_of_basis_functions_(self%basis)==size(orb),"ATOM:make_orbital_grid_c ... incorrect suize, orb")
      sq = .false.
      if (present(square)) sq = square
      n_pt = size(pt,1)
      g = 0.0d0
      l = 0
      do s = 1,n_shell_(self)
         call set_(sh,self%basis%shell(s),pos)
         f = l + 1
         l = f + sh%n_comp - 1
         call create_(sh_grid,n_pt,sh%n_comp)
         call make_grid_(sh,sh_grid,pt)
         g = g + matmul(sh_grid,orb(f:l))
         call destroy_(sh_grid)
      end do
      if (sq) g = conjg(g)*g

   end subroutine

   function density_at_radius(self,R) result(res)
    type(atom_type) :: self
    ! Work out the electron at radius "R".
      real(kind=kind(1.0d0)) :: R,res
      real(kind=kind(1.0d0)), dimension(1) :: density_grid
      real(kind=kind(1.0d0)), dimension(3,1) :: pt

      if (associated(self%coppensbasis) .and. associated(self%coppensbasis%orbital)) then
         res = density_at_radius_(self%coppensbasis,R)
      else if (associated(self%basis)) then
         pt(:,1) = self%pos + (/R,0.0d0,0.0d0/)
         if (number_kind_(self%natural_orbitals) == "real") then
            call make_density_grid_r_(self,density_grid,pt)
         else
            call make_density_grid_c_(self,density_grid,pt)
         end if
         res = density_grid(1)
      end if

   end function

!  ***************************
!  Basis shell access routines
!  ***************************

   subroutine get_shell_limits(self,first,last)
    type(atom_type) :: self
    ! Get the indices of first and last basis functions in a particular shell
    ! "s", first(s) and last(s), respectively.
      integer(kind=kind(1)), dimension(:), pointer :: first,last
      integer(kind=kind(1)) :: n_shell,f,l,s

      n_shell = self%basis%n_shell
      nullify(first); call create_(first,n_shell)
      nullify(last);  call create_(last,n_shell)
      l = 0
      do s = 1,n_shell
         f = l + 1
         l = f + self%basis%shell(s)%n_comp - 1
         first(s) = f
         last(s)  = l
      end do

   end subroutine

!  **************
!  Output methods
!  **************

   subroutine put(self)
    type(atom_type) :: self
    ! Put out the atom information to file "stdout"

     call flush_(stdout)
     call show_(stdout,"Label                  =",trim(self%label))
     call show_(stdout,"Atomic No.             =",self%atomic_number)
     call show_(stdout,"Chemical symbol        =",trim(chemical_symbol_(self)))
     call show_(stdout,"Atom coordinates       =",self%pos(1),self%pos(2),self%pos(3))
     if (self%sequence_number/=0) call put_mm_info_(self)
     if (associated(self%basis)) call put_(self%basis)
     if (associated(self%coppensbasis)) call put_(self%coppensbasis)
    ! if (.interpolator.created) .interpolator.put

   end subroutine

   subroutine put_mm_info(self)
    type(atom_type) :: self
    ! Put out the MM/protien part of the atom information to file "stdout"

     call flush_(stdout)
     call show_(stdout,"Residue atom name      =",trim(self%residue_atom_name))
     call show_(stdout,"Residue name           =",trim(self%residue_name))
     call show_(stdout,"Sequence number        =",self%sequence_number)
     call show_(stdout,"MM forcefield name     =",self%mm_forcefield_name)
     call show_(stdout,"MM atom type           =",self%mm_atom_type)
     call show_(stdout,"MM charge              =",self%mm_charge)
     call show_(stdout,"Restraining position   =",self%restraining_position)
     call show_(stdout,"Restraining force      =",self%restraining_force_constant)

   end subroutine

   subroutine put_thermal_tensor(self)
    type(atom_type) :: self
    ! Output the thermal tensor.  Does not put a header or carriage return.
    ! Only outputs the 6 independent ones, not all 9.

     call put_(stdout,self%thermal_tensor(1,1))
     call put_(stdout,self%thermal_tensor(2,2))
     call put_(stdout,self%thermal_tensor(3,3))
     call put_(stdout,self%thermal_tensor(1,2))
     call put_(stdout,self%thermal_tensor(1,3))
     call put_(stdout,self%thermal_tensor(2,3))

   end subroutine

   subroutine put_natural_orbitals(self)
    type(atom_type) :: self
    ! Put out the current associated molecular orbitals to file "out"

      call flush_(stdout)
      call text_(stdout,"Natural orbital occupations:")
      call put_(stdout, self%occupation_numbers, format="column")
      call flush_(stdout)
      call text_(stdout,"Natural orbitals:")
      call put_(stdout, self%natural_orbitals)

   end subroutine

end
