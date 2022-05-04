!-------------------------------------------------------------------------------
!
! CLUSTER:
!
! An object to store information pertaining to a cluster of atoms or molecules
! formed from an underlying crystal structure.
!
! Note: an associated crystal and atom list should be supplied. It is intended
! that these will come from the molecule which generates the cluster (perhaps a
! cluster should contain a molecule?). These entities are not destroyed when the
! cluster is destroyed.
!
! Copyright (C) Dylan Jayatilaka, 2002
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
! $Id: cluster.foo,v 1.2.2.27 2003/11/13 05:34:39 reaper Exp $
!-------------------------------------------------------------------------------

module CLUSTER_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

   use CRYSTAL_MODULE, only: transposed_xyz_seitz_matrices_
   use CRYSTAL_MODULE, only: create_copy_
   use CRYSTAL_MODULE, only: cartesian_fragment_width_
   use CRYSTAL_MODULE, only: destroy_

   use INTVEC_MODULE, only: shrink_
   use INTVEC_MODULE, only: append_
   use INTVEC_MODULE, only: create_
   use INTVEC_MODULE, only: swap_elements_
   use INTVEC_MODULE, only: expand_
   use INTVEC_MODULE, only: create_copy_
   use INTVEC_MODULE, only: destroy_

   use BINVEC_MODULE, only: create_
   use BINVEC_MODULE, only: create_copy_
   use BINVEC_MODULE, only: destroy_

   use REAL_MODULE, only: same_as_
   use REAL_MODULE, only: to_str_

   use REALVEC_MODULE, only: same_as_
   use REALVEC_MODULE, only: create_
   use REALVEC_MODULE, only: create_copy_
   use REALVEC_MODULE, only: destroy_

   use INT_MODULE, only: inverse_triangle_number_
   use INT_MODULE, only: to_str_

   use ATOM_MODULE, only: chemical_symbol_
   use ATOM_MODULE, only: copy_

   use UNITCELL_MODULE, only: change_from_fractional_
   use UNITCELL_MODULE, only: change_into_fractional_

   use INTMAT_MODULE, only: shrink_columns_
   use INTMAT_MODULE, only: create_
   use INTMAT_MODULE, only: expand_columns_
   use INTMAT_MODULE, only: create_copy_
   use INTMAT_MODULE, only: destroy_
   use INTMAT_MODULE, only: has_column_

   use REALMAT3_MODULE, only: transpose_12_
   use REALMAT3_MODULE, only: destroy_

   use REALMAT3VEC_MODULE, only: make_gaussian_xyz_matrices_
   use REALMAT3VEC_MODULE, only: destroy_

   use STR_MODULE, only: to_lower_case_

   use TEXTFILE_MODULE, only: stdin
   use TEXTFILE_MODULE, only: stdout
   use TEXTFILE_MODULE, only: set_default_units_
   use TEXTFILE_MODULE, only: text_
   use TEXTFILE_MODULE, only: next_str_
   use TEXTFILE_MODULE, only: skip_next_item_
   use TEXTFILE_MODULE, only: put_text_
   use TEXTFILE_MODULE, only: next_item_
   use TEXTFILE_MODULE, only: show_
   use TEXTFILE_MODULE, only: put_
   use TEXTFILE_MODULE, only: tab_
   use TEXTFILE_MODULE, only: read_
   use TEXTFILE_MODULE, only: flush_
   use TEXTFILE_MODULE, only: reverted_
   use TEXTFILE_MODULE, only: read_ptr_
   use TEXTFILE_MODULE, only: dash_

   use INTVECVEC_MODULE, only: shrink_
   use INTVECVEC_MODULE, only: create_

   use ATOMVEC_MODULE, only: atom_for_shell_
   use ATOMVEC_MODULE, only: create_
   use ATOMVEC_MODULE, only: make_atom_basis_fn_limits_
   use ATOMVEC_MODULE, only: n_shell_
   use ATOMVEC_MODULE, only: create_copy_
   use ATOMVEC_MODULE, only: bases_are_all_labeled_
   use ATOMVEC_MODULE, only: nullify_basis_part_
   use ATOMVEC_MODULE, only: n_bf_
   use ATOMVEC_MODULE, only: resolve_axis_system_
   use ATOMVEC_MODULE, only: bonded_
   use ATOMVEC_MODULE, only: convert_from_crystal_
   use ATOMVEC_MODULE, only: nullify_coppensbasis_part_
   use ATOMVEC_MODULE, only: n_atom_
   use ATOMVEC_MODULE, only: destroy_

   use REALMAT_MODULE, only: get_column_dot_products_
   use REALMAT_MODULE, only: get_column_norms_
   use REALMAT_MODULE, only: create_
   use REALMAT_MODULE, only: mean_column_vector_
   use REALMAT_MODULE, only: expand_columns_
   use REALMAT_MODULE, only: create_copy_
   use REALMAT_MODULE, only: is_square_
   use REALMAT_MODULE, only: shrink_columns_
   use REALMAT_MODULE, only: change_basis_
   use REALMAT_MODULE, only: max_abs_column_difference_
   use REALMAT_MODULE, only: convert_to_
   use REALMAT_MODULE, only: column_index_
   use REALMAT_MODULE, only: destroy_
   use REALMAT_MODULE, only: has_column_
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

   public    fragment_atom_indices_
   interface fragment_atom_indices_
      module procedure fragment_atom_indices
   end interface

   public    nullify_ptr_part_
   interface nullify_ptr_part_
      module procedure nullify_ptr_part
   end interface

   public    read_fragment_geometry_
   interface read_fragment_geometry_
      module procedure read_fragment_geometry
   end interface

   public    put_CX_
   interface put_CX_
      module procedure put_CX
   end interface

   public    atom_pair_parent_count_
   interface atom_pair_parent_count_
      module procedure atom_pair_parent_count
   end interface

   public    make_density_matrix_
   interface make_density_matrix_
      module procedure make_density_matrix
   end interface

   public    cartesian_geometry_
   interface cartesian_geometry_
      module procedure cartesian_geometry
   end interface

   public    make_info_
   interface make_info_
      module procedure make_info
   end interface

   public    destroy_ptr_part_
   interface destroy_ptr_part_
      module procedure destroy_ptr_part
   end interface

   public    read_keywords_
   interface read_keywords_
      module procedure read_keywords
   end interface

   private    shrink_info_arrays_
   interface shrink_info_arrays_
      module procedure shrink_info_arrays
   end interface

   public    set_crystal_defaults_
   interface set_crystal_defaults_
      module procedure set_crystal_defaults
   end interface

   public    put_cluster_table_
   interface put_cluster_table_
      module procedure put_cluster_table
   end interface

   public    make_big_cluster_
   interface make_big_cluster_
      module procedure make_big_cluster
   end interface

   public    minimum_distance2_to_xyz_
   interface minimum_distance2_to_xyz_
      module procedure minimum_distance2_to_xyz
   end interface

   public    read_defragment_
   interface read_defragment_
      module procedure read_defragment
   end interface

   public    read_add_criteria_
   interface read_add_criteria_
      module procedure read_add_criteria
   end interface

   public    make_fragment_atom_
   interface make_fragment_atom_
      module procedure make_fragment_atom
   end interface

   public    read_radius_
   interface read_radius_
      module procedure read_radius
   end interface

   public    process_keyword_
   interface process_keyword_
      module procedure process_keyword
   end interface

   public    put_
   interface put_
      module procedure put
   end interface

   public    read_partition_factors_
   interface read_partition_factors_
      module procedure read_partition_factors
   end interface

   public    partition_density_
   interface partition_density_
      module procedure partition_density
   end interface

   private    is_bonded_to_
   interface is_bonded_to_
      module procedure is_bonded_to
   end interface

   public    is_new_atom_
   interface is_new_atom_
      module procedure is_new_atom
   end interface

   public    copy_
   interface copy_
      module procedure copy
   end interface

   public    set_defaults_
   interface set_defaults_
      module procedure set_defaults
   end interface

   private    add_new_atom_
   interface add_new_atom_
      module procedure add_new_atom
   end interface

   public    create_atom_list_
   interface create_atom_list_
      module procedure create_atom_list
   end interface

   private    is_near_origin_
   interface is_near_origin_
      module procedure is_near_origin
   end interface

   public    create_
   interface create_
      module procedure create
      module procedure create_1
   end interface

   public    make_connection_table_
   interface make_connection_table_
      module procedure make_connection_table
   end interface

   public    is_new_xyz_atom_
   interface is_new_xyz_atom_
      module procedure is_new_xyz_atom
   end interface

   private    initialise_info_arrays_
   interface initialise_info_arrays_
      module procedure initialise_info_arrays
   end interface

   public    create_copy_
   interface create_copy_
      module procedure create_copy
   end interface

   public    do_defragment_
   interface do_defragment_
      module procedure do_defragment
   end interface

   public    put_tonto_input_
   interface put_tonto_input_
      module procedure put_tonto_input
   end interface

   public    nonfragment_atom_indices_
   interface nonfragment_atom_indices_
      module procedure nonfragment_atom_indices
   end interface

   private    make_symop_list_and_geometry_
   interface make_symop_list_and_geometry_
      module procedure make_symop_list_and_geometry
   end interface

   public    destroy_cell_geom_ptr_part_
   interface destroy_cell_geom_ptr_part_
      module procedure destroy_cell_geom_ptr_part
   end interface

   public    destroy_
   interface destroy_
      module procedure destroy
   end interface

   public    cluster_width_
   interface cluster_width_
      module procedure cluster_width
   end interface

   public    read_junk_
   interface read_junk_
      module procedure read_junk
   end interface

   private    is_in_unit_cell_
   interface is_in_unit_cell_
      module procedure is_in_unit_cell
   end interface

   public    destroy_cluster_info_ptr_part_
   interface destroy_cluster_info_ptr_part_
      module procedure destroy_cluster_info_ptr_part
   end interface

   public    put_spartan_
   interface put_spartan_
      module procedure put_spartan
   end interface

   public    make_partition_factors_
   interface make_partition_factors_
      module procedure make_partition_factors
   end interface

   public    read_crystal_fragment_
   interface read_crystal_fragment_
      module procedure read_crystal_fragment
   end interface

   public    read_units_
   interface read_units_
      module procedure read_units
   end interface

   public    create_from_molecule_
   interface create_from_molecule_
      module procedure create_from_molecule
   end interface

   public    maximum_cell_axis_distance_
   interface maximum_cell_axis_distance_
      module procedure maximum_cell_axis_distance
   end interface

   private    find_fragment_atoms_
   interface find_fragment_atoms_
      module procedure find_fragment_atoms
   end interface

   public    set_add_criteria_
   interface set_add_criteria_
      module procedure set_add_criteria
   end interface

   public    minimum_distance_to_
   interface minimum_distance_to_
      module procedure minimum_distance_to
      module procedure minimum_distance_to_1
   end interface

contains

   subroutine create(self)
    type(cluster_type) :: self
    ! Create the object
     pointer :: self

     nullify(self)
     allocate(self)

     call nullify_ptr_part_(self)

   end subroutine

   subroutine create_1(self,crystal,asymmetric_cell_atom)
    type(cluster_type) :: self
    ! Create the object IMPORTANT NOTE: the number of "asymmetric_cell_atoms"
    ! must match the second dimension of "crystal.asymmetric_unit_geometry" if
    ! both are present.  Furthermore, it is assumed that the kinds of
    ! "asymmetric_cell_atoms" match those positions in the asymmetric unit.
    ! Usually both of these entities will be obtained from a single read from a
    ! type(cif_type) file. If not, you must use the "create_from_molecule" method, below.
     pointer :: self
     type(crystal_type), pointer :: crystal
     type(atom_type), dimension(:), pointer :: asymmetric_cell_atom

     call create_(self)
     call set_defaults_(self,crystal,asymmetric_cell_atom)

   end subroutine

   subroutine create_from_molecule(self,crystal,cell_atom)
    type(cluster_type) :: self
    ! Create the object from a "crystal" and a "cell_atom" list. NOTE: The
    ! fragment geometry in the crystal must be consistent with the atom list
    ! positions, even though they are in different axis frames.
     pointer :: self
     type(crystal_type), pointer :: crystal
     type(atom_type), dimension(:), pointer :: cell_atom
     type(atom_type), dimension(:), pointer :: asymmetric_atom_list

     call ensure_(tonto,associated(crystal),"CLUSTER:create_from_molecule ... no crystal information")
     call ensure_(tonto,crystal%n_fragment_atoms==size(cell_atom),"CLUSTER:create_from_molecule ... number of atoms inconsist&
&ent")
     call create_(self)  ! <<<<
     call create_copy_(asymmetric_atom_list,cell_atom(crystal%unique_fragment_atom))
     call set_defaults_(self,crystal,asymmetric_atom_list)  ! <<<<
     call nullify_basis_part_(asymmetric_atom_list)
     call nullify_coppensbasis_part_(asymmetric_atom_list)
     call destroy_(asymmetric_atom_list)

   end subroutine

   subroutine create_copy(self,object)
    type(cluster_type) :: self
    ! Create a copy of object
     type(cluster_type) :: object
     pointer :: self
    ! The following code is inherited from OBJECT

     call create_(self)
     call copy_(self,object)

   end subroutine

   subroutine copy(self,cluster)
    type(cluster_type) :: self
    ! Copy the contents of "cluster" to self. NOTE: ensure you destroy all the
    ! parts you need to before calling this.
      type(cluster_type), intent(in) :: cluster

      self = cluster
      if (associated(cluster%geometry)) &
         call create_copy_(self%geometry,cluster%geometry)
      if (associated(cluster%crystal)) &
         call create_copy_(self%crystal,cluster%crystal)
      if (associated(cluster%asymmetric_cell_atom)) &
         call create_copy_(self%asymmetric_cell_atom,cluster%asymmetric_cell_atom)
      if (associated(cluster%fragment_geometry)) &
         call create_copy_(self%fragment_geometry,cluster%fragment_geometry)
      if (associated(cluster%symop)) &
         call create_copy_(self%symop,cluster%symop)
      if (associated(cluster%symop_for_atom)) &
         call create_copy_(self%symop_for_atom,cluster%symop_for_atom)
      if (associated(cluster%parent_for_atom)) &
         call create_copy_(self%parent_for_atom,cluster%parent_for_atom)
      if (associated(cluster%atom_for_cell_atom)) &
         call create_copy_(self%atom_for_cell_atom,cluster%atom_for_cell_atom)
      if (associated(cluster%minimum_distance_to_atom)) &
         call create_copy_(self%minimum_distance_to_atom,cluster%minimum_distance_to_atom)
      if (associated(cluster%closest_fragment_atom_to_atom)) &
         call create_copy_(self%closest_fragment_atom_to_atom,cluster%closest_fragment_atom_to_atom)
      if (associated(cluster%is_fragment_atom)) &
         call create_copy_(self%is_fragment_atom,cluster%is_fragment_atom)
      if (associated(cluster%partition_factor)) &
         call create_copy_(self%partition_factor,cluster%partition_factor)

   end subroutine

   subroutine destroy(self)
    type(cluster_type) :: self
    ! Destroy the object
      pointer :: self

      if (.not. associated(self)) then;   return; end if
      call destroy_ptr_part_(self)
      deallocate(self)

   end subroutine

   subroutine nullify_ptr_part(self)
    type(cluster_type) :: self
    ! Nullify the pointer parts

      nullify(self%crystal)
      nullify(self%asymmetric_cell_atom)
      nullify(self%symop)
      nullify(self%fragment_geometry)
      nullify(self%geometry)
      nullify(self%symop_for_atom)
      nullify(self%parent_for_atom)
      nullify(self%atom_for_cell_atom)
      nullify(self%minimum_distance_to_atom)
      nullify(self%closest_fragment_atom_to_atom)
      nullify(self%is_fragment_atom)
      nullify(self%partition_factor)

   end subroutine

   subroutine destroy_ptr_part(self)
    type(cluster_type) :: self
    ! Destroy the pointer parts

      call destroy_(self%crystal)
      if (associated(self%asymmetric_cell_atom)) then
         call nullify_basis_part_(self%asymmetric_cell_atom)
         call nullify_coppensbasis_part_(self%asymmetric_cell_atom)
         call destroy_(self%asymmetric_cell_atom)
      end if
      call destroy_cell_geom_ptr_part_(self)
      call destroy_cluster_info_ptr_part_(self)
      call destroy_(self%partition_factor)

   end subroutine

   subroutine destroy_cluster_info_ptr_part(self)
    type(cluster_type) :: self
    ! Destroy the non-symop informational ptr parts. These incclude the actual
    ! .geometry of the cluster as wellas information relating to the .symop's
    ! used to generate the cluster.

      call destroy_(self%symop)
      call destroy_(self%geometry)
      call destroy_(self%symop_for_atom)
      call destroy_(self%parent_for_atom)
      call destroy_(self%atom_for_cell_atom)
      call destroy_(self%minimum_distance_to_atom)
      call destroy_(self%closest_fragment_atom_to_atom)
      call destroy_(self%is_fragment_atom)

   end subroutine

   subroutine destroy_cell_geom_ptr_part(self)
    type(cluster_type) :: self
    ! Destroy fragment geometry pointer parts. These are the geometrical
    ! coordinates used to generate the cluster.

      call destroy_(self%fragment_geometry)

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

   subroutine set_defaults(self,crystal,asymmetric_cell_atom)
    type(cluster_type) :: self
    ! Set up defaults. IMPORTANT NOTE: the number of "asymmetric_cell_atoms" must
    ! match the second dimension of "crystal.asymmetric_unit_geometry" if both
    ! are present.  Furthermore, it is assumed that the kinds of
    ! "asymmetric_cell_atoms" match those positions in the asymmetric unit.
    ! Usually both of these entities will be obtained from a single read from a
    ! type(cif_type) file.
     type(crystal_type), pointer :: crystal
     type(atom_type), dimension(:), pointer :: asymmetric_cell_atom

     call ensure_(tonto,associated(asymmetric_cell_atom),"CLUSTER:set_defaults ... no asymmetric_cell_atom data")
     call ensure_(tonto,associated(crystal),"CLUSTER:set_defaults ... no crystal created!")
     call ensure_(tonto,associated(crystal%fragment_geometry),"CLUSTER:set_defaults ... no crystal fragment_geometry created"&
&)
     call ensure_(tonto,associated(crystal%asymmetric_unit_geometry),"CLUSTER:set_defaults ... no crystal asymmetric_unit_geo&
&metry created")
     call ensure_(tonto,size(crystal%asymmetric_unit_geometry,2)==size(asymmetric_cell_atom),"CLUSTER:set_defaults ... incons&
&istent # of atoms in asymmetric_cell_atom")
     nullify(self%asymmetric_cell_atom)
     nullify(self%crystal)
     call destroy_ptr_part_(self)
     self%add_criteria = "fragment"
     self%start_with_fragment = .false.
     self%radius       = 0.0d0
     self%defragment   = .true.
     self%info_made    = .false.
     call create_copy_(self%asymmetric_cell_atom,asymmetric_cell_atom)
     call create_copy_(self%crystal,crystal)
     call set_crystal_defaults_(self,crystal)

   end subroutine

   subroutine set_crystal_defaults(self,crystal)
    type(cluster_type) :: self
    ! Set up the "crystal" defaults. The .fragment_geometry come from "crystal".
      type(crystal_type), pointer :: crystal

      call ensure_(tonto,associated(crystal),"CLUSTER:set_crystal_defaults ... no crystal created!")
      call ensure_(tonto,associated(crystal%fragment_geometry),"CLUSTER:set_crystal_defaults ... no crystal fragment_geometry&
& created")
      call ensure_(tonto,associated(crystal%asymmetric_unit_geometry),"CLUSTER:set_crystal_defaults ... no crystal asymmetric&
&_unit_geometry created")
      call destroy_cluster_info_ptr_part_(self)
      self%n_symop = 0
      self%n_atoms = 0
      call destroy_(self%fragment_geometry)
      call create_copy_(self%fragment_geometry,crystal%fragment_geometry)
      self%n_fragment_atoms = size(crystal%fragment_geometry,2)
      self%fragment_width   = max_abs_column_difference_(crystal%fragment_geometry)
      self%fragment_offset  = mean_column_vector_(crystal%fragment_geometry)

   end subroutine

   subroutine set_add_criteria(self,criteria)
    type(cluster_type) :: self
    ! Set the add criteria, whether to add atoms by whole clusters within a
    ! certain radius of the starting fragment, or by individual atoms within a
    ! certain distance of the starting fragment.
      character(128) :: criteria

      self%add_criteria = criteria
      call to_lower_case_(self%add_criteria)
      select case (self%add_criteria)
         case("within_radius         ")
         case("unit_cell             ")
         case("fragment              "); self%start_with_fragment = .true.
         case("unit_cell_and_fragment"); self%start_with_fragment = .true.
         case default;   allocate(tonto%known_keywords(4))
         tonto%known_keywords(1) = "within_radius         "
         tonto%known_keywords(2) = "unit_cell             "
         tonto%known_keywords(3) = "fragment              "
         tonto%known_keywords(4) = "unit_cell_and_fragment"
         call unknown_(tonto,self%add_criteria,"CLUSTER:set_add_criteria")
         deallocate(tonto%known_keywords)
      end select

   end subroutine

!  *************
!  Input methods
!  *************

   recursive subroutine read_keywords(self)
    type(cluster_type) :: self
    ! Read data from "stdin" using keyword style input.
    ! The following code is inherited from OBJECT
     character(128) :: word

     call ensure_(tonto,next_item_(stdin)=="{","CLUSTER:read_keywords ... expecting open bracket symbol, {")
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
    type(cluster_type) :: self
    ! Process command "keyword". Any required data needed by the "keyword" is
    ! inputted from "stdin".
      character(*) :: keyword
      character(128) :: word

      word = keyword
      call to_lower_case_(word)
      select case (word)
         case ("}                     ")   ! exit case
         case ("add_criteria=         "); call read_add_criteria_(self)
         case ("defragment=           "); call read_defragment_(self)
         case ("fragment_geometry=    "); call read_fragment_geometry_(self)
         case ("crystal_fragment=     "); call read_crystal_fragment_(self)
         case ("make_info             "); call make_info_(self)
         case ("partition_factors=    "); call read_partition_factors_(self)
         case ("put                   "); call put_(self)
         case ("put_tonto_input       "); call put_tonto_input_(self)
         case ("radius=               "); call read_radius_(self)
         case ("units=                "); call read_units_(self)
         case default;     allocate(tonto%known_keywords(11))
         tonto%known_keywords(1) = "}                     "
         tonto%known_keywords(2) = "add_criteria=         "
         tonto%known_keywords(3) = "defragment=           "
         tonto%known_keywords(4) = "fragment_geometry=    "
         tonto%known_keywords(5) = "crystal_fragment=     "
         tonto%known_keywords(6) = "make_info             "
         tonto%known_keywords(7) = "partition_factors=    "
         tonto%known_keywords(8) = "put                   "
         tonto%known_keywords(9) = "put_tonto_input       "
         tonto%known_keywords(10) = "radius=               "
         tonto%known_keywords(11) = "units=                "
         call unknown_(tonto,word,"CLUSTER:process_keyword")
         deallocate(tonto%known_keywords)
      end select

   end subroutine

   subroutine read_units(self)
    type(cluster_type) :: self
    ! Read a string which describes the units to be used
    ! The following code is inherited from OBJECT

      call set_default_units_(stdin,next_str_(stdin))

   end subroutine

   subroutine read_junk(self)
    type(cluster_type) :: self
    ! Read in a junk string, useful for ignoring a field
    ! The following code is inherited from OBJECT

      call skip_next_item_(stdin)

   end subroutine

   subroutine read_add_criteria(self)
    type(cluster_type) :: self
    ! Read the add criteria, whether to add atoms by whole clusters within a
    ! certain radius of the starting fragment, or by individual atoms within a
    ! certain distance of the starting fragment.

      call read_(stdin,self%add_criteria)
      call to_lower_case_(self%add_criteria)
      select case (self%add_criteria)
         case("within_radius         ")
         case("unit_cell             ")
         case("fragment              "); self%start_with_fragment = .true.
         case("unit_cell_and_fragment"); self%start_with_fragment = .true.
         case default;   allocate(tonto%known_keywords(4))
         tonto%known_keywords(1) = "within_radius         "
         tonto%known_keywords(2) = "unit_cell             "
         tonto%known_keywords(3) = "fragment              "
         tonto%known_keywords(4) = "unit_cell_and_fragment"
         call unknown_(tonto,self%add_criteria,"CLUSTER:read_add_criteria")
         deallocate(tonto%known_keywords)
      end select

   end subroutine

   subroutine read_defragment(self)
    type(cluster_type) :: self
    ! Read whether to defragment the cluster at the boundaries.

      call read_(stdin,self%defragment)
     ! if (.not. .defragment .and. .add_criteria=="unit_cell") then
     !    call warn_(tonto,"defragment= must be set TRUE when add_criteria= unit_cell")
     ! end

   end subroutine

   subroutine read_radius(self)
    type(cluster_type) :: self
    ! Read the radius of the cluster

      call read_(stdin,self%radius)

   end subroutine

   subroutine read_partition_factors(self)
    type(cluster_type) :: self
    ! Read the partition factors to be used. Note that the length of this array
    ! must correspond to the number of atoms in the generated cluster in order to
    ! be used. This cannot be checked at this point in the code.

      call read_ptr_(stdin,self%partition_factor)

   end subroutine

   subroutine read_fragment_geometry(self)
    type(cluster_type) :: self
    ! Read in the fragment geometry, in cartesian atomic units, and convert to
    ! fractional coordinates.
    ! NOTE: to define the Hirshfeld surface, all fragment atom positions must
    ! correspond to the positions of actual atoms in the crystal lattice.
      real(kind=kind(1.0d0)), dimension(:), pointer :: tmp

      call ensure_(tonto,associated(self%crystal),"CLUSTER:read_fragment_geometry ... no crystal defined")
      call read_ptr_(stdin,tmp)
      call ensure_(tonto,mod(size(tmp),3)==0,"CLUSTER:read_fragment_geometry ... # of elements not divisible by 3")
      self%n_fragment_atoms = size(tmp)/3
      call destroy_(self%fragment_geometry)
      call create_(self%fragment_geometry,3,self%n_fragment_atoms)
      self%fragment_geometry = reshape(tmp,(/3,self%n_fragment_atoms/))
      call change_into_fractional_(self%crystal%unitcell,self%fragment_geometry)
      call destroy_(tmp)

   end subroutine

   subroutine read_crystal_fragment(self)
    type(cluster_type) :: self
    ! Read in the crystal fragment geometry, in fractional coordinate units.
    ! NOTE: to define the Hirshfeld surface, all fragment atom positions must
    ! correspond to the positions of actual atoms in the crystal lattice.
      real(kind=kind(1.0d0)), dimension(:), pointer :: tmp

      call read_ptr_(stdin,tmp)
      call ensure_(tonto,mod(size(tmp),3)==0,"CLUSTER:read_crystal_fragment ... # of elements not divisible by 3")
      self%n_fragment_atoms = size(tmp)/3
      call destroy_(self%fragment_geometry)
      call create_(self%fragment_geometry,3,self%n_fragment_atoms)
      self%fragment_geometry = reshape(tmp,(/3,self%n_fragment_atoms/))
      call destroy_(tmp)

   end subroutine

!  ***************************
!  Cluster generation routines
!  ***************************

   subroutine make_info(self)
    type(cluster_type) :: self
    ! Make all the cluster information from an arbitrary .fragment_geometry.
    ! NOTE: .set_defaults must be called before this.

      call ensure_(tonto,associated(self%crystal),"CLUSTER:make_info ... no crystal")
      call ensure_(tonto,associated(self%crystal%cluster_symop),"CLUSTER:make_info ... no crystal cluster symops")
      call ensure_(tonto,associated(self%fragment_geometry),"CLUSTER:make_info ... no fragment geometry")
      if (.not. self%info_made) then
        call make_symop_list_and_geometry_(self)
        call find_fragment_atoms_(self)
        self%info_made = .true.
      end if

   end subroutine

   subroutine make_symop_list_and_geometry(self)
    type(cluster_type) :: self
    ! Make the list of symops which transform a .crystal.unit_cell_geometry
    ! within a certain .radius of .fragment_geometry. Also make the geometry of
    ! the cluster and other informational arrays relating to the cluster. NOTE:
    ! the .fragment_geometry is shifted to the origin by an offset before any
    ! cluster generating operations are done, but it is put back afterwards.
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: big_cluster
     integer(kind=kind(1)), dimension(:,:), pointer :: symop_list
     logical(kind=kind(.true.)), dimension(:), pointer :: atom_added
     integer(kind=kind(1)), dimension(4) :: symop
     real(kind=kind(1.0d0)), dimension(3) :: pos
     integer(kind=kind(1)) :: n_trial_atoms,n_cell_atoms,n_asym_atoms,i,n,c,a

     call ensure_(tonto,associated(self%crystal),"CLUSTER:make_symop_list_and_geometry ... no crystal")
     call ensure_(tonto,associated(self%crystal%atom_for_unit_cell_atom),"CLUSTER:make_symop_list_and_geometry ... no crystal&
& atom_for_unit_cell_atom info")
     call ensure_(tonto,associated(self%crystal%unit_cell_geometry),"CLUSTER:make_symop_list_and_geometry ... need the unit c&
&ell in the crystal")
     call ensure_(tonto,associated(self%fragment_geometry),"CLUSTER:make_symop_list_and_geometry ... no fragment geometry")
     call ensure_(tonto,associated(self%asymmetric_cell_atom),"CLUSTER:make_symop_list_and_geometry ... no asymmetric_cell_at&
&om info")
      ! The fragment is shifted by "offset" to the origin
     if (any(self%fragment_offset/=0)) then
     self%fragment_geometry = self%fragment_geometry &
                        - spread(self%fragment_offset,2,self%n_fragment_atoms)
     end if
      ! The algorithm creates "big_cluster", a cube of unit cells around the
      ! fragment.  We could use a sphere instead of a cube, but we were lazy.
     call make_big_cluster_(self,big_cluster,symop_list)
     n_trial_atoms = size(big_cluster,2)
      ! For defragmenting, we must store which atoms in big_cluster added
     call create_(atom_added,n_trial_atoms)
     atom_added = .false.
      ! Initialise the arrays we really want ...
     call initialise_info_arrays_(self,n_trial_atoms)
      ! We may want to force the fragment to be part of the outputted cluster.
     n_cell_atoms = size(self%crystal%unit_cell_geometry,2)
     n_asym_atoms = size(self%asymmetric_cell_atom)
     if (self%start_with_fragment) then
       do i = 1, self%n_fragment_atoms
         pos = self%fragment_geometry(:,i)
         n = column_index_(big_cluster,pos)
         call ensure_(tonto,n>0,"CLUSTER:make_symop_list_and_geometry ... position of fragment atom not found in big_cluster"&
&)
         call ensure_(tonto,n<=n_trial_atoms,"CLUSTER:make_symop_list_and_geometry ... incorrect position of fragment atom")
         symop = symop_list(:,n)
         c = mod((n-1),n_cell_atoms) + 1          ! unit cell atom,
         a = self%crystal%atom_for_unit_cell_atom(c)  ! asymmetric cell atom
         call ensure_(tonto,a>0,"CLUSTER:make_symop_list_and_geometry ... no unique atom for unit cell atom "//trim(to_str_(c&
&)))
!        call ensure_(tonto,a<=n_asym_atoms,"asymmetric atom too large, cell atom "//c.to_str.trim)
         call add_new_atom_(self,pos,symop,a)
         atom_added(n) = .true.
       end do
     end if
      ! Finally: extract the appropriate cluster from the big_cluster
     if (self%add_criteria/="fragment") then  ! already done this above
       do i = 1,n_trial_atoms
         if (atom_added(i)) cycle
         pos = big_cluster(:,i)
         symop = symop_list(:,i)
         c = mod((i-1),n_cell_atoms) + 1          ! unit cell atom,
         a = self%crystal%atom_for_unit_cell_atom(c)  ! asymmetric cell atom
         call ensure_(tonto,a>0,"CLUSTER:make_symop_list_and_geometry ... no unique atom for unit cell atom "//trim(to_str_(c&
&)))
!        call ensure_(tonto,a<=n_asym_atoms,"asymmetric atom too large, cell atom "//c.to_str.trim)
         if (is_new_atom_(self,pos)) then
           call add_new_atom_(self,pos,symop,a)
           atom_added(i) = .true.
         end if
       end do
     end if
      ! Add in bonded atoms if appropriate.
     if (self%defragment) call do_defragment_(self,big_cluster,symop_list,atom_added)
      ! Shift fragment and cluster back by "offset" to the origin
     if (any(self%fragment_offset/=0)) then
     self%fragment_geometry = self%fragment_geometry &
                           + spread(self%fragment_offset,2,self%n_fragment_atoms)
     self%geometry = self%geometry + spread(self%fragment_offset,2,self%n_atoms)
     end if
      ! Clean up a bit
     call shrink_info_arrays_(self)
     call destroy_(atom_added)
     call destroy_(big_cluster)
     call destroy_(symop_list)

   end subroutine

   subroutine make_big_cluster(self,big_cluster,symop_list)
    type(cluster_type) :: self
    ! Make "big_cluster": the set of atom positions in unit cells around the
    ! central unit cell. Return also "symop_list", the list of seitz symmetry
    ! operations used to generate every atom in the "big_cluster".
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: big_cluster
     integer(kind=kind(1)), dimension(:,:), pointer :: symop_list
     real(kind=kind(1.0d0)), dimension(3) :: max_dist,hkl
     real(kind=kind(1.0d0)) :: cell_a,cell_b,cell_c
     integer(kind=kind(1)) :: h_max,k_max,l_max,n_cells,n_cell_atoms,max_n_atoms
     integer(kind=kind(1)) :: first,last,i,h,h1,k,k1,l,l1,cell_atom,n_big_cluster

     call ensure_(tonto,associated(self%crystal),"CLUSTER:make_big_cluster ... no crystal")
     call ensure_(tonto,associated(self%crystal%unit_cell_geometry),"CLUSTER:make_big_cluster ... need the unit cell in the c&
&rystal")
     call ensure_(tonto,associated(self%crystal%symop_for_unit_cell_atom),"CLUSTER:make_big_cluster ... no crystal symop_for_&
&unit_cell_atom info")
      ! The maximum cell distances to search for cluster atoms
     max_dist = maximum_cell_axis_distance_(self)
     cell_a = self%crystal%unitcell%length(1)
     cell_b = self%crystal%unitcell%length(2)
     cell_c = self%crystal%unitcell%length(3)
     if (self%add_criteria=="unit_cell" .and. .not. self%start_with_fragment .and. .not. self%defragment) then
        h_max = 0
        k_max = 0
        l_max = 0
     else
        h_max = ceiling(max_dist(1)/cell_a) + 1
        k_max = ceiling(max_dist(2)/cell_b) + 1
        l_max = ceiling(max_dist(3)/cell_c) + 1
     end if
     n_cells = (2*h_max+1) * (2*k_max+1) * (2*l_max+1)
     n_cell_atoms = size(self%crystal%unit_cell_geometry,2)
     max_n_atoms = n_cell_atoms*n_cells
      ! The algorithm creates "big_cluster", a cube of unit cells around the
      ! origin.  We could use a sphere instead of a cube, but we were lazy.
     call create_(symop_list,4,max_n_atoms)
     call create_(big_cluster,3,max_n_atoms)
     big_cluster = 0.0d0
      ! Now loop over all lattice vectors consistent with max_dist to make
      ! "big_cluster".  Note the strange loop order is [0,0,0], [0,0,-1],
      ! [0,0,1], [0,0,-2], [0,0,2], [0,-1,0] etc.
     first = 1  ! Index of current atom.
     last  = n_cell_atoms
     do h1 = 0,2*h_max
       if (mod(h1,2)==0) then; h = h1 / 2
       else;                   h = - (h1 + 1) / 2
       end if
      ! k_max = ceiling((max_dist-abs(h)*cell_a) / cell_b)+1
       do k1 = 0,2*k_max
         if (mod(k1,2)==0) then; k = k1 / 2
         else;                   k = - (k1 + 1) / 2
         end if
        ! l_max = ceiling((max_dist-abs(h)*cell_a-abs(k)*cell_b) / cell_c)+1
         do l1 = 0,2*l_max
           if (mod(l1,2)==0) then; l = l1 / 2
           else;                   l = - (l1 + 1) / 2
           end if
            ! hkl is the displacement to shift the atoms.
           hkl = real((/h,k,l/),kind=kind(1.0d0))
            ! The unit cell atom corresponding to this atom.
           cell_atom = 1
           do i = first,last
             big_cluster(:,i) = self%crystal%unit_cell_geometry(:,cell_atom) + hkl
             symop_list(:,i) = (/self%crystal%symop_for_unit_cell_atom(cell_atom),h,k,l/)
             cell_atom = cell_atom + 1
           end do
           first = first + n_cell_atoms
           last  = last  + n_cell_atoms
         end do
       end do
     end do
     n_big_cluster = first - 1
     call shrink_columns_(big_cluster,n_big_cluster)
     call shrink_columns_(symop_list,n_big_cluster)
  ! stdout.text("symop_list:")
  ! stdout.put(transpose(symop_list))

   end subroutine

   function maximum_cell_axis_distance(self) result(max_dist)
    type(cluster_type) :: self
    ! Return the maximum distance that can be travelled along any single crystal
    ! axis direction in order that the minimum distance from the transformed
    ! fragment and the original fragment is less than the cluster radius.
     real(kind=kind(1.0d0)), dimension(3) :: max_dist
     real(kind=kind(1.0d0)), dimension(3) :: fragment_width
    ! fragment_length :: real(kind=kind(1.0d0))

     call ensure_(tonto,associated(self%crystal),"CLUSTER:maximum_cell_axis_distance ... no crystal")
     fragment_width = cartesian_fragment_width_(self%crystal)
    ! fragment_length = sqrt(dot_product(fragment_width,fragment_width))
     select case (self%add_criteria)
       case("within_radius         "); max_dist = self%radius + fragment_width
       case("unit_cell             "); max_dist = 0.0d0
       case("fragment              "); max_dist = fragment_width
       case("unit_cell_and_fragment"); max_dist = fragment_width
       case default;                   allocate(tonto%known_keywords(4))
       tonto%known_keywords(1) = "within_radius         "
       tonto%known_keywords(2) = "unit_cell             "
       tonto%known_keywords(3) = "fragment              "
       tonto%known_keywords(4) = "unit_cell_and_fragment"
       call unknown_(tonto,self%add_criteria,"CLUSTER:maximum_cell_axis_distance")
       deallocate(tonto%known_keywords)
     end select

   end function

   subroutine initialise_info_arrays(self,max_n_atoms)
    type(cluster_type) :: self
    ! Initialise all informational arrays ready for a cluster .geometry
    ! generation from a general .fragment_geometry.
     integer(kind=kind(1)) :: max_n_atoms

     call destroy_cluster_info_ptr_part_(self)
      ! Initial maximum size for symop info arrays
     self%n_atoms = 0
     self%n_symop = 0
     call create_(self%symop,4,max_n_atoms)
    ! .atom_for_cell_atom.create(.crystal.n_asymmetric_unit_atoms,max_n_max)
    ! .atom_for_cell_atom = 0
      ! Initial maximum size for geometry info arrays
     call create_(self%geometry,3,max_n_atoms);      self%geometry = 0.0d0
     call create_(self%symop_for_atom,max_n_atoms);  self%symop_for_atom = 0
     call create_(self%parent_for_atom,max_n_atoms); self%parent_for_atom = 0
    ! .minimum_distance_to_atom.create(max_n_atoms)
    ! .closest_fragment_atom_to_atom.create(max_n_atoms)

   end subroutine

   subroutine do_defragment(self,big_cluster,symop_list,atom_added)
    type(cluster_type) :: self
    ! Defragment the generated cluster. This requires a surrounding "big_cluster"
    ! of atom positions, a "symop_list", the symmetry operation used to generate
    ! each atom in "big_cluster", and "atom_added", the list of atoms added to
    ! the cluster .geometry from "big_cluster", so far.
     real(kind=kind(1.0d0)), dimension(:,:) :: big_cluster
     integer(kind=kind(1)), dimension(:,:) :: symop_list
     logical(kind=kind(.true.)), dimension(:) :: atom_added
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: big_cluster_xyz
     integer(kind=kind(1)), dimension(:), pointer :: out_atom
     logical(kind=kind(.true.)) :: nearby
     type(atom_type), dimension(2) :: atom_pair
     real(kind=kind(1.0d0)), dimension(3) :: in_pos,out_pos
     integer(kind=kind(1)) :: i,in_atom,in_parent,j,out,c,a
     integer(kind=kind(1)) :: n_cell_atoms,n_asym_atoms,n_big_atoms,n_out_atoms

     call ensure_(tonto,associated(self%crystal),"CLUSTER:do_defragment ... no crystal")
     call ensure_(tonto,associated(self%crystal%atom_for_unit_cell_atom),"CLUSTER:do_defragment ... no crystal atom_for_unit_&
&cell_atom info")
     call ensure_(tonto,associated(self%asymmetric_cell_atom),"CLUSTER:do_defragment ... no asymmetric_cell_atom info")
     call ensure_(tonto,size(symop_list,2)==size(big_cluster,2),"CLUSTER:do_defragment ... inconsistent symop_list array")
     call ensure_(tonto,size(atom_added) ==size(big_cluster,2),"CLUSTER:do_defragment ... inconsistent atom_added array")
     call ensure_(tonto,count(atom_added)==self%n_atoms,"CLUSTER:do_defragment ... wrong # of TRUE elements in atom_added arr&
&ay")
      ! Store the xyz positions to save time
     n_big_atoms = size(big_cluster,2)
     call create_(big_cluster_xyz,3,n_big_atoms)
     big_cluster_xyz = matmul(self%crystal%unitcell%direct_matrix,big_cluster)
     n_cell_atoms = size(self%crystal%unit_cell_geometry,2)
     n_asym_atoms = size(self%asymmetric_cell_atom)
     n_out_atoms = count(.not. atom_added)
     call create_(out_atom,n_out_atoms)
     out_atom = pack((/(i,i=1,n_big_atoms)/),mask=(.not. atom_added))
      ! Loop over atoms *inside* the cluster .....
     do in_atom = 1,n_out_atoms
        ! Worst case is to add one atom at a time, resulting in
        ! n_big_atoms as the upper limit.
       if (in_atom > self%n_atoms) exit  ! No more in_atoms were added
       in_parent = self%parent_for_atom(in_atom)  ! Now we have the latest in_atom
       in_pos = matmul(self%crystal%unitcell%direct_matrix,self%geometry(:,in_atom))
       atom_pair(1)%atomic_number = self%asymmetric_cell_atom(in_parent)%atomic_number
       atom_pair(1)%pos = in_pos
        ! Which are connected to in_atom? Loop over atoms (j) *outside* the inner
        ! cluster, that are in the big_cluster. Loop over the indices of the
        ! false parts of atom_added.
       j = 1
       do  ! j = 1,n_out_atoms
         out = out_atom(j)
         out_pos = big_cluster_xyz(:,out)
         nearby = abs(out_pos(1)-in_pos(1))<6.0d0 &
              .and. abs(out_pos(2)-in_pos(2))<6.0d0 &
              .and. abs(out_pos(3)-in_pos(3))<6.0d0
         if (.not. nearby) then
            j = j + 1
            if (j>n_out_atoms) exit
         else
            c = mod((out-1),n_cell_atoms) + 1        ! unit cell atom,
            a = self%crystal%atom_for_unit_cell_atom(c)  ! asymmetric cell atom
            call ensure_(tonto,a>0,"CLUSTER:do_defragment ... no unique atom for unit cell atom "//trim(to_str_(c)))
            call ensure_(tonto,a<=n_asym_atoms,"CLUSTER:do_defragment ... asymmetric atom too large, cell atom "//trim(to_str&
&_(c)))
            atom_pair(2)%atomic_number = self%asymmetric_cell_atom(a)%atomic_number
            atom_pair(2)%pos = out_pos
            if (bonded_(atom_pair,1,2)) then
              atom_added(out) = .true.  ! Add the new symop and atom
              call swap_elements_(out_atom,j,n_out_atoms)
              n_out_atoms = n_out_atoms - 1
              call add_new_atom_(self,big_cluster(:,out),symop_list(:,out),a)
            else
               j = j + 1
            end if
            if (j>n_out_atoms) exit
         end if
       end do
     end do
     call destroy_(out_atom)
     call destroy_(big_cluster_xyz)

   end subroutine

   subroutine make_connection_table(self,table)
    type(cluster_type) :: self
    ! Make the connection "table" for the crystal unit cell geometry.
     type(intvec__type), dimension(:), pointer :: table
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: geometry,geometry_xyz
     type(atom_type), dimension(2) :: atom_pair
     logical(kind=kind(.true.)), dimension(:), pointer :: atom_added
     real(kind=kind(1.0d0)), dimension(3) :: pos_i,pos_j
     integer(kind=kind(1)) :: n_atoms,i,j,parent_i,parent_j,last_i
     logical(kind=kind(.true.)) :: nearby

     call ensure_(tonto,associated(self%crystal),"CLUSTER:make_connection_table ... no crystal")
     call ensure_(tonto,associated(self%crystal%unit_cell_geometry),"CLUSTER:make_connection_table ... no unit_cell in the cr&
&ystal")
     call ensure_(tonto,associated(self%asymmetric_cell_atom),"CLUSTER:make_connection_table ... no asymmetric_cell_atom info&
&")
     geometry => self%crystal%unit_cell_geometry
     n_atoms = size(geometry,2)
     call create_(table,n_atoms)  ! worst case is no atoms are connected
     call create_(geometry_xyz,3,n_atoms)
     geometry_xyz = matmul(self%crystal%unitcell%direct_matrix,geometry)
     call create_(atom_added,n_atoms)
     atom_added = .false.
     do i = 1,n_atoms
        if (atom_added(i)) cycle
        pos_i = geometry_xyz(:,i)
        parent_i = self%crystal%atom_for_unit_cell_atom(i)
        atom_pair(1)%atomic_number = self%asymmetric_cell_atom(parent_i)%atomic_number
        atom_pair(1)%pos = pos_i
        do j = 2,n_atoms
           pos_j = geometry_xyz(:,j)
           nearby = abs(pos_j(1)-pos_i(1))<6.0d0 &
                .and. abs(pos_j(2)-pos_i(2))<6.0d0 &
                .and. abs(pos_j(3)-pos_i(3))<6.0d0
           if (.not. nearby) cycle
           parent_j = self%crystal%atom_for_unit_cell_atom(j)
           atom_pair(2)%atomic_number = self%asymmetric_cell_atom(parent_j)%atomic_number
           atom_pair(2)%pos = pos_j
           if (bonded_(atom_pair,1,2)) then
              atom_added(j) = .true.
              call append_(table(i)%element,j)
              last_i = i
           end if
        end do
     end do
     call destroy_(geometry_xyz)
     call destroy_(atom_added)
     call shrink_(table,last_i)
  ! stdout.text("symop_list:")
  ! stdout.put(transpose(symop_list))

   end subroutine

   subroutine shrink_info_arrays(self)
    type(cluster_type) :: self
    ! Shrink the informational arrays to save space.

     call shrink_columns_(self%symop,self%n_symop)
    ! .atom_for_cell_atom.shrink_columns(.n_symop)
     call shrink_columns_(self%geometry,self%n_atoms)
     call shrink_(self%symop_for_atom,self%n_atoms)
     call shrink_(self%parent_for_atom,self%n_atoms)
    ! .minimum_distance_to_atom.shrink(.n_atoms)
    ! .closest_fragment_atom_to_atom.shrink(.n_atoms)

   end subroutine

   function is_new_xyz_atom(self,pos,xyz) result(res)
    type(cluster_type) :: self
    ! Return .true. only if "pos" (in crystal coordinates) is to be added to
    ! .geometry.  IMPORTANT NOTE: "xyz" is "pos" in *cartesian* coordiantes!!!
    ! This is to save some computation.
     real(kind=kind(1.0d0)), dimension(3), intent(in) :: pos,xyz
     logical(kind=kind(.true.)) :: res

     res = .false.
     if (.not. has_column_(self%geometry(:,1:self%n_atoms),pos)) then
       select case (self%add_criteria)
          ! Add only those atoms proximate to the fragment
         case("within_radius         ")
                 res = minimum_distance2_to_xyz_(self,xyz)<=self%radius*self%radius
          ! Add only those atoms in the first unit cell
         case("unit_cell             ")
                 res = is_in_unit_cell_(self,pos)
          ! Add only those atoms in the initial fragment
         case("fragment              ")
                 res = minimum_distance2_to_xyz_(self,xyz)<= 10.0d0**(-10)
          ! Add only those atoms in the first unit cell or initial fragment
         case("unit_cell_and_fragment")
                 res = minimum_distance2_to_xyz_(self,xyz)<= 10.0d0**(-10) .or. is_in_unit_cell_(self,pos)
         case default;    allocate(tonto%known_keywords(4))
         tonto%known_keywords(1) = "within_radius         "
         tonto%known_keywords(2) = "unit_cell             "
         tonto%known_keywords(3) = "fragment              "
         tonto%known_keywords(4) = "unit_cell_and_fragment"
         call unknown_(tonto,self%add_criteria,"CLUSTER:is_new_xyz_atom")
         deallocate(tonto%known_keywords)
       end select
     end if

   end function

   function is_new_atom(self,pos) result(res)
    type(cluster_type) :: self
    ! Return .true. only if "pos" position (in crystal coordinates) is to be added
    ! to .geometry.
     real(kind=kind(1.0d0)), dimension(3), intent(in) :: pos
     logical(kind=kind(.true.)) :: res

     res = .false.
     if (.not. has_column_(self%geometry(:,1:self%n_atoms),pos)) then
       select case (self%add_criteria)
          ! Add only those atoms proximate to the fragment
         case("within_radius         ")
                 res = minimum_distance_to_(self,pos)<=self%radius
          ! Add only those atoms in the first unit cell
         case("unit_cell             ")
                 res = is_in_unit_cell_(self,pos)
          ! Add only those atoms in the initial fragment
         case("fragment              ")
                 res = minimum_distance_to_(self,pos)<= 10.0d0**(-10)
          ! Add only those atoms in the first unit cell or initial fragment
         case("unit_cell_and_fragment")
                 res = minimum_distance_to_(self,pos)<= 10.0d0**(-10) .or. is_in_unit_cell_(self,pos)
         case default;    allocate(tonto%known_keywords(4))
         tonto%known_keywords(1) = "within_radius         "
         tonto%known_keywords(2) = "unit_cell             "
         tonto%known_keywords(3) = "fragment              "
         tonto%known_keywords(4) = "unit_cell_and_fragment"
         call unknown_(tonto,self%add_criteria,"CLUSTER:is_new_atom")
         deallocate(tonto%known_keywords)
       end select
     end if

   end function

   subroutine add_new_atom(self,pos,symop,cell_atom)
    type(cluster_type) :: self
    ! Add a new atom with position "pos" into the cluster ".geometry" array, and
    ! update all the related information. "symop" is the symmetry operation that
    ! generated "pos". "cell_atom" is the index of the atom in the asymmetric
    ! unit cell fragment which is symmetrically equivalent to this atom with
    ! position "pos".
    ! NOTE: this routine should only be called if .any_new_atoms_in(pos) is .true..
     real(kind=kind(1.0d0)), dimension(3) :: pos
     integer(kind=kind(1)), dimension(4) :: symop
     integer(kind=kind(1)) :: cell_atom
     integer(kind=kind(1)) :: q,n,n_col
     logical(kind=kind(.true.)) :: disordered,symop_added
     real(kind=kind(1.0d0)) :: occ

     if (has_column_(self%geometry(:,1:self%n_atoms),pos,eps=10.0d0**(-2))) then;   return; end if
     occ = self%asymmetric_cell_atom(cell_atom)%site_occupancy
     disordered = .not. same_as_(occ,1.0d0)
     call warn_if_(tonto,disordered,"CLUSTER:add_new_atom ... disordered atom "//trim(to_str_((self%n_atoms+1)))//", occ = "/&
&/trim(to_str_(occ)))
     symop_added = has_column_(self%symop,symop,col=q)  ! get symop index "q" if there
     if (.not. symop_added) then     ! Add symop to .symop table if reqd.
        n_col = size(self%symop,2)
        if ((self%n_symop+1)>n_col) then  ! Expand .symop table if reqd.
           call expand_columns_(self%symop,2*n_col)
          ! .atom_for_cell_atom.expand_columns(2*n_col)
        end if
        q = self%n_symop + 1
        self%n_symop = q
        self%symop(:,q) = symop
     end if
     n_col = size(self%geometry,2)
     if ((self%n_atoms+1)>n_col) then  ! Expand info arrays if reqd.
        call expand_columns_(self%geometry,2*n_col)
        call expand_(self%symop_for_atom,2*n_col)
        call expand_(self%parent_for_atom,2*n_col)
       ! .minimum_distance_to_atom.expand(2*n_col)
       ! .closest_fragment_atom_to_atom.expand(2*n_col)
     end if
     n = self%n_atoms + 1
     self%geometry(:,n) = pos
     self%symop_for_atom(n) = q
     self%parent_for_atom(n) = cell_atom
    ! .atom_for_cell_atom(cell_atom,q) = n
    ! These may take up too much time ...
    ! dist = .minimum_distance_to(pos,closest)
    ! .minimum_distance_to_atom(n) = dist
    ! .closest_fragment_atom_to_atom(n) = closest
     self%n_atoms = n

   end subroutine

   function is_bonded_to(self,pos,p) result(res)
    type(cluster_type) :: self
    ! Return .true. only if the atom with position "pos" and parent atom index "p"
    ! is bonded to one of the cluster atoms with positions in .geometry,
    ! according to a Bragg-Slater bond-distance criteria.
     real(kind=kind(1.0d0)), dimension(3), intent(in) :: pos
     integer(kind=kind(1)), intent(in) :: p
     logical(kind=kind(.true.)) :: res
     integer(kind=kind(1)) :: a,pa
     type(atom_type), dimension(2) :: atom_pair

     call ensure_(tonto,associated(self%geometry),"CLUSTER:is_bonded_to ... no .geometry")
     call ensure_(tonto,self%n_atoms>0,"CLUSTER:is_bonded_to ... no atoms in .geometry")
     call ensure_(tonto,associated(self%asymmetric_cell_atom),"CLUSTER:is_bonded_to ... no .cell_atom")
     call ensure_(tonto,associated(self%crystal),"CLUSTER:is_bonded_to ... no .asymmetric_cell_atom")
     res = .false.
     do a = 1,self%n_atoms
        pa = self%parent_for_atom(a)
        atom_pair(1) = self%asymmetric_cell_atom(pa)
        atom_pair(2) = self%asymmetric_cell_atom(p)
        atom_pair(1)%pos = self%geometry(:,a)
        atom_pair(2)%pos = pos
        atom_pair%axis_system = "crystal"
        call convert_from_crystal_(atom_pair,self%crystal)
        res = bonded_(atom_pair,1,2)
        if (res) exit
     end do

   end function

   function is_in_unit_cell(self,pos) result(res)
    type(cluster_type) :: self
    ! Return .true. only if the atom with position "pos" is in the first unit cell.
     real(kind=kind(1.0d0)), dimension(3) :: pos
     logical(kind=kind(.true.)) :: res
     real(kind=kind(1.0d0)), parameter :: error = 10.0d0**(-8)

     res = all(pos(:)>=-error .and. pos(:)<1.0d0-error)

   end function

   function is_near_origin(self,pos) result(res)
    type(cluster_type) :: self
    ! Return .true. only if the atom with position "pos" is near the origin i.e. if
    ! corrdinates all have absolute value less than one.
     real(kind=kind(1.0d0)), dimension(3) :: pos
     logical(kind=kind(.true.)) :: res

     res = &
        abs(pos(1))<=1.0d0 .and. &
        abs(pos(2))<=1.0d0 .and. &
        abs(pos(3))<=1.0d0

   end function

   subroutine find_fragment_atoms(self)
    type(cluster_type) :: self
    ! Find the fragment atoms in the .geometry of the cluster.
     real(kind=kind(1.0d0)), dimension(3) :: new,frag
     integer(kind=kind(1)) :: i,j
     logical(kind=kind(.true.)) :: found
     real(kind=kind(1.0d0)) :: tol

     call ensure_(tonto,self%n_atoms>0,"CLUSTER:find_fragment_atoms ... there are no atoms in the cluster!")
     call ensure_(tonto,associated(self%geometry),"CLUSTER:find_fragment_atoms ... no geometry")
     call ensure_(tonto,associated(self%fragment_geometry),"CLUSTER:find_fragment_atoms ... no fragment geometry")
     call ensure_(tonto,associated(self%crystal),"CLUSTER:find_fragment_atoms ... no crystal")
     tol = 10.0d0**(-3)/maxval(self%crystal%unitcell%length)
     call create_(self%is_fragment_atom,self%n_atoms)
     self%is_fragment_atom = .false.
     do j = 1,self%n_fragment_atoms  ! loop over fragment atoms
        frag = self%fragment_geometry(:,j)
        found = .false.
        do i = 1,self%n_atoms
           new = self%geometry(:,i)  ! cluster atom position
           if (same_as_(new,frag,tol)) then
              found = .true.
              self%is_fragment_atom(i) = .true.
              exit
           end if
        end do
        if (.not. found) then
          ! if (.add_criteria=="unit_cell") then
              call warn_(tonto,"CLUSTER:find_fragment_atoms ... fragment atom "//trim(to_str_(j))//" not found")
          ! else
          !    call die_(tonto,"fragment atom "//trim(j.to_str)//" not found")
          ! end
        end if
     end do

   end subroutine

!  *******************
!  Cluster information
!  *******************

   function minimum_distance2_to_xyz(self,pos,closest_atom) result(res)
    type(cluster_type) :: self
    ! Return the minimum atom separation squared between the .fragment_geometry
    ! and a position "pos" given in cartesian coordinates. If present,
    ! "closest_atom" is set to the index of the atom (i.e. column) of
    ! .fragment_geometry whose position is closest to "pos".
     real(kind=kind(1.0d0)), dimension(3) :: pos
     integer(kind=kind(1)), optional :: closest_atom
     real(kind=kind(1.0d0)) :: res
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: diff
     real(kind=kind(1.0d0)), dimension(:), pointer :: r2

     call ensure_(tonto,associated(self%fragment_geometry),"CLUSTER:minimum_distance2_to_xyz ... no fragment geometry")
     call ensure_(tonto,associated(self%crystal),"CLUSTER:minimum_distance2_to_xyz ... no crystal data")
     call create_(diff,3,self%n_fragment_atoms)
     call create_(r2,self%n_fragment_atoms)
     diff = self%fragment_geometry - spread(pos,2,self%n_fragment_atoms)
     call get_column_dot_products_(diff,r2)
     res = minval(r2)
     if (present(closest_atom)) closest_atom = minval(minloc(r2))
     call destroy_(r2)
     call destroy_(diff)

   end function

   function minimum_distance_to(self,pos,closest_atom) result(res)
    type(cluster_type) :: self
    ! Return the minimum atom separation between the .fragment_geometry
    ! and a position "pos" given in crystal axis coordinates. If present,
    ! "closest_atom" is set to the index of the atom (i.e. column) of
    ! .fragment_geometry whose position is closest to "pos".
     real(kind=kind(1.0d0)), dimension(3) :: pos
     integer(kind=kind(1)), optional :: closest_atom
     real(kind=kind(1.0d0)) :: res
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: diff
     real(kind=kind(1.0d0)), dimension(:), pointer :: norms

     call ensure_(tonto,associated(self%fragment_geometry),"CLUSTER:minimum_distance_to ... no fragment geometry")
     call ensure_(tonto,associated(self%crystal),"CLUSTER:minimum_distance_to ... no crystal data")
     call create_(diff,3,self%n_fragment_atoms)
     call create_(norms,self%n_fragment_atoms)
     diff = self%fragment_geometry - spread(pos,2,self%n_fragment_atoms)
     call change_from_fractional_(self%crystal%unitcell,diff)   ! Put in cartesians.
     call get_column_norms_(diff,norms)
     res = minval(norms)
     if (present(closest_atom)) closest_atom = minval(minloc(norms))
     call destroy_(norms)
     call destroy_(diff)

   end function

   function minimum_distance_to_1(self,pos) result(res)
    type(cluster_type) :: self
    ! Return the minimum atom separation between the .fragment_geometry
    ! and another geometry "pos" given in crystal axis coordinates.
     real(kind=kind(1.0d0)), dimension(:,:) :: pos
     real(kind=kind(1.0d0)) :: res
     integer(kind=kind(1)) :: i

     call ensure_(tonto,size(pos,2)>0,"CLUSTER:minimum_distance_to_1 ... no atom positions")
     res = minimum_distance_to_(self,pos(:,1))
     do i = 2,size(pos,2)
        res = min(minimum_distance_to_(self,pos(:,i)),res)
     end do

   end function

   function cartesian_geometry(self) result(res)
    type(cluster_type) :: self
    ! Return the cartesian geometry for the cluster
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: res

     call ensure_(tonto,associated(self%fragment_geometry),"CLUSTER:cartesian_geometry ... no fragment geometry")
     call ensure_(tonto,associated(self%crystal),"CLUSTER:cartesian_geometry ... no crystal data")
     call create_(res,3,self%n_atoms)
     call change_from_fractional_(self%crystal%unitcell,self%geometry)

   end function

   function cluster_width(self) result(res)
    type(cluster_type) :: self
    ! Return the width "res" of the cluster in each of the 3 axis directions.
    ! NOTE: using crystal axis system.
      real(kind=kind(1.0d0)), dimension(3) :: res

      call ensure_(tonto,associated(self%geometry),"CLUSTER:cluster_width ... no fragment geometry")
      res = max_abs_column_difference_(self%geometry)

   end function

   subroutine make_fragment_atom(self,fragment_atom)
    type(cluster_type) :: self
    ! Make the list of fragment atoms, an ATOMVEC
     type(atom_type), dimension(:) :: fragment_atom
     integer(kind=kind(1)) :: a,n,p

     call ensure_(tonto,self%n_fragment_atoms>0,"CLUSTER:make_fragment_atom ... no fragment atoms")
     call ensure_(tonto,associated(self%is_fragment_atom),"CLUSTER:make_fragment_atom ... no fragment atoms")
     call ensure_(tonto,associated(self%crystal),"CLUSTER:make_fragment_atom ... no crystal info")
     n = 0
     do a = 1,self%n_atoms
         if (.not. self%is_fragment_atom(a)) cycle
         n = n + 1
         p = self%parent_for_atom(a)
         fragment_atom(n) = self%asymmetric_cell_atom(p)
         fragment_atom(n)%pos = self%geometry(:,a)
         fragment_atom(n)%axis_system = "crystal"
     end do
     call convert_from_crystal_(fragment_atom,self%crystal)

   end subroutine

   function fragment_atom_indices(self) result(res)
    type(cluster_type) :: self
    ! Return the indices of the fragment atoms in the cluster.
     integer(kind=kind(1)), dimension(self%n_fragment_atoms) :: res
     integer(kind=kind(1)) :: a,n

     call ensure_(tonto,self%n_fragment_atoms>0,"CLUSTER:fragment_atom_indices ... no fragment atoms")
     call ensure_(tonto,associated(self%is_fragment_atom),"CLUSTER:fragment_atom_indices ... no fragment atoms")
     n = 0
     do a = 1,self%n_atoms
         if (.not. self%is_fragment_atom(a)) cycle
         n = n + 1
         res(n) = a
     end do

   end function

   function nonfragment_atom_indices(self) result(res)
    type(cluster_type) :: self
    ! Return the indices of the nonfragment atoms in the cluster.
     integer(kind=kind(1)), dimension(self%n_atoms-self%n_fragment_atoms) :: res
     integer(kind=kind(1)) :: a,n

     call ensure_(tonto,self%n_atoms>self%n_fragment_atoms,"CLUSTER:nonfragment_atom_indices ... no nonfragment atoms")
     call ensure_(tonto,associated(self%is_fragment_atom),"CLUSTER:nonfragment_atom_indices ... no fragment atoms")
     n = 0
     do a = 1,self%n_atoms
         if (self%is_fragment_atom(a)) cycle
         n = n + 1
         res(n) = a
     end do

   end function

!  ****************************************
!  Cluster transformations on matrices, etc
!  ****************************************

   subroutine make_partition_factors(self,matrix)
    type(cluster_type) :: self
    ! Make the partition factors from the cluster-fragment mapping information.
     real(kind=kind(1.0d0)), dimension(:,:) :: matrix
     integer(kind=kind(1)) :: n_atom,a1,a2,f1,l1,f2,l2
     integer(kind=kind(1)), dimension(:), pointer :: first_basis_fn_for_atom,last_basis_fn_for_atom
     real(kind=kind(1.0d0)) :: factor

     call ensure_(tonto,associated(self%asymmetric_cell_atom),"CLUSTER:make_partition_factors ... no atom data")
     call make_atom_basis_fn_limits_(self%asymmetric_cell_atom,first_basis_fn_for_atom,last_basis_fn_for_atom)
     n_atom = n_atom_(self%asymmetric_cell_atom)
     do a1 = 1,n_atom
       f1 = first_basis_fn_for_atom(a1)
       l1 = last_basis_fn_for_atom(a1)
       do a2 = 1,n_atom
         f2 = first_basis_fn_for_atom(a2)
         l2 = last_basis_fn_for_atom(a2)
          ! Mulliken partitioning
         factor = (self%partition_factor(a1)+self%partition_factor(a2))/2.0d0
         matrix(f1:l1,f2:l2) = factor * matrix(f1:l1,f2:l2)
       end do
     end do
     call destroy_(last_basis_fn_for_atom)
     call destroy_(first_basis_fn_for_atom)

   end subroutine

   subroutine partition_density(self,matrix)
    type(cluster_type) :: self
    ! Applies atomic partition factors to the density matrix, useful for zeroing
    ! out certain atoms.
     real(kind=kind(1.0d0)), dimension(:,:) :: matrix
     integer(kind=kind(1)) :: n_atom,a1,a2,f1,l1,f2,l2
     integer(kind=kind(1)), dimension(:), pointer :: first_basis_fn_for_atom,last_basis_fn_for_atom
     real(kind=kind(1.0d0)) :: factor

     call ensure_(tonto,associated(self%asymmetric_cell_atom),"CLUSTER:partition_density ... no atom data")
     call make_atom_basis_fn_limits_(self%asymmetric_cell_atom,first_basis_fn_for_atom,last_basis_fn_for_atom)
     n_atom = n_atom_(self%asymmetric_cell_atom)
     do a1 = 1,n_atom
       f1 = first_basis_fn_for_atom(a1)
       l1 = last_basis_fn_for_atom(a1)
       do a2 = 1,n_atom
         f2 = first_basis_fn_for_atom(a2)
         l2 = last_basis_fn_for_atom(a2)
          ! Mulliken partitioning
         factor = (self%partition_factor(a1)+self%partition_factor(a2))/2.0d0
         matrix(f1:l1,f2:l2) = factor * matrix(f1:l1,f2:l2)
       end do
     end do
     call destroy_(last_basis_fn_for_atom)
     call destroy_(first_basis_fn_for_atom)

   end subroutine

   subroutine create_atom_list(self,atom)
    type(cluster_type) :: self
    ! Make a new atom list for the cluster
    ! NOTE: basis sets are pointer copied!
     type(atom_type), dimension(:), pointer :: atom
     real(kind=kind(1.0d0)), dimension(:,:,:), pointer :: seitz
     real(kind=kind(1.0d0)), dimension(3,3) :: therm
     integer(kind=kind(1)) :: a,p,s

     call ensure_(tonto,associated(self%fragment_geometry),"CLUSTER:create_atom_list ... no crystal fragment geometry")
     call ensure_(tonto,associated(self%crystal),"CLUSTER:create_atom_list ... no crystal data")
     call ensure_(tonto,associated(self%asymmetric_cell_atom),"CLUSTER:create_atom_list ... no atom data")
     call ensure_(tonto,self%n_atoms>0,"CLUSTER:create_atom_list ... no atoms in cluster")
     call create_(atom,self%n_atoms)
     seitz => transposed_xyz_seitz_matrices_(self%crystal)  ! transposed !
     do a = 1,self%n_atoms
        p = self%parent_for_atom(a)             ! only one parent will do.
        s = self%symop_for_atom(a)              ! the symop from the parent.
        call copy_(atom(a),self%asymmetric_cell_atom(p))         ! make copy, but .basis is a ptr copy
        atom(a)%pos = self%geometry(:,a)        ! crystal axis system
        therm = self%asymmetric_cell_atom(p)%thermal_tensor
        call change_basis_(therm,self%crystal%unitcell%reciprocal_U_matrix)
        call change_basis_(therm,seitz(:,:,self%symop(1,s)))
        call change_basis_(therm,self%crystal%unitcell%direct_U_matrix)
        atom(a)%thermal_tensor = therm
        atom(a)%axis_system = "crystal"
     end do
     call destroy_(seitz)
     call resolve_axis_system_(atom,self%crystal)     ! change pos to cartesian

   end subroutine

   subroutine make_density_matrix(self,P,D,atom)
    type(cluster_type) :: self
    ! Make a cluster density matrix "P" from a fragment density matrix "D", given
    ! a new cluster "atom" list (see routine create_atom_list).
     real(kind=kind(1.0d0)), dimension(:,:) :: P,D
     type(atom_type), dimension(:) :: atom
     real(kind=kind(1.0d0)), dimension(:,:,:), pointer :: ptr
     type(realmat3__type), dimension(:), pointer :: tr
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: tr1,tr2,pc, W
     integer(kind=kind(1)) :: f1,l1,s1,n1,m1,a1,c1,f2,l2,s2,n2,m2,a2,c2
     integer(kind=kind(1)) :: n_shell, q,s,f3,l3,f4,l4
     integer(kind=kind(1)), dimension(:), pointer :: atom_for_shell, first,last, first_fn,last_fn

     call ensure_(tonto,associated(self%crystal),"CLUSTER:make_density_matrix ... no crystal data")
     call ensure_(tonto,associated(self%asymmetric_cell_atom),"CLUSTER:make_density_matrix ... no atom data")
     call ensure_(tonto,bases_are_all_labeled_(self%asymmetric_cell_atom),"CLUSTER:make_density_matrix ... unlabelled bases!"&
&)
     call ensure_(tonto,is_square_(D) .and. size(D,1)==n_bf_(self%asymmetric_cell_atom),"CLUSTER:make_density_matrix ... D wr&
&ong shape")
     call ensure_(tonto,is_square_(P) .and. size(P,1)==n_bf_(atom),"CLUSTER:make_density_matrix ... P wrong shape")
     call ensure_(tonto,associated(self%partition_factor),"CLUSTER:make_density_matrix ... no partition factors")
     n_shell = n_shell_(self%asymmetric_cell_atom)
     atom_for_shell => atom_for_shell_(self%asymmetric_cell_atom)
     call make_atom_basis_fn_limits_(self%asymmetric_cell_atom,first,last)
      call make_atom_basis_fn_limits_(atom,first_fn,last_fn)
     ptr => transposed_xyz_seitz_matrices_(self%crystal)  ! transposes here
     call transpose_12_(ptr)
     call make_gaussian_xyz_matrices_(tr,ptr)
     call destroy_(ptr)
     pc => atom_pair_parent_count_(self)
     do q = 1,self%n_symop
       s = self%symop(1,q)
        ! Transform each shell pair by symop "s"
       do s1 = 1,n_shell
         f1 = first(s1); l1 = last(s1); n1 = (l1-f1+1)
         a1 = atom_for_shell(s1)
         c1 = self%atom_for_cell_atom(a1,q)  ! cluster atom index
         f3 = first_fn(c1); l3 = last_fn(c1)
         m1 = inverse_triangle_number_(n1) - 1
         tr1  => tr(m1)%element(:,:,s)
          ! Transform 1st index of density matrix
         call create_(W,n1,n2)
         W = matmul(tr1,D(f1:l1,f2:l2))
         do s2 = 1, n_shell
           f2 = first(s2); l2 = last(s2); n2 = (l2-f2+2)
           a2 = atom_for_shell(s2)
           c2 = self%atom_for_cell_atom(a2,q)  ! cluster atom index
           f4 = first_fn(c2); l4 = last_fn(c2)
           m2 = inverse_triangle_number_(n2) - 1
            ! Transform 2nd index of density matrix
           tr2  => tr(m2)%element(:,:,s)
           P(f3:l3,f4:l4) = P(f3:l3,f4:l4) &
                          + matmul(W,transpose(tr2))/pc(c1,c2)
         end do
         call destroy_(W)
       end do
     end do
     call destroy_(pc)
     call destroy_(last_fn); call destroy_(first_fn)
     call destroy_(last); call destroy_(first)
     call destroy_(tr)
     call destroy_(atom_for_shell)

   end subroutine

   function atom_pair_parent_count(self) result(n2)
    type(cluster_type) :: self
    ! Make the atom pair parent count, i.e. the number of times n2(i,j) a
    ! particular atom pair (i,j) is *generated from* a fragment atom pair (i',j')
    ! by the symmerty operations whose indices are stored in .symop. We divide
    ! by this factor to ensure that the pair effectively appears as being
    ! generated once, as an average of all the symmetry operations.  This is
    ! quite similar to the n2 factor in the Dacre-Elder-Dupuis-King symmetry
    ! method. For insight see:
    ! P.D. Dacre, CPL (1970) 7, 47
    ! M. Elder, IJQC (1973) 7, 75
    ! M. Dupuis and H.F> King, IJQC (1977) 11, 613
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: n2
     integer(kind=kind(1)) :: s1,a1,c1,s2,a2,c2
     integer(kind=kind(1)) :: n_shell,q
     integer(kind=kind(1)), dimension(:), pointer :: atom_for_shell

     call ensure_(tonto,associated(self%asymmetric_cell_atom),"CLUSTER:atom_pair_parent_count ... no atom data")
     call ensure_(tonto,associated(self%atom_for_cell_atom),"CLUSTER:atom_pair_parent_count ... no atom_for_fragment_atom dat&
&a")
     call create_(n2,self%n_atoms,self%n_atoms)
     n_shell = n_shell_(self%asymmetric_cell_atom)
     atom_for_shell => atom_for_shell_(self%asymmetric_cell_atom)
     do q = 1,self%n_symop
        ! Transform each shell pair by symop "s"
       do s1 = 1,n_shell
         a1 = atom_for_shell(s1)
         c1 = self%atom_for_cell_atom(a1,q)    ! cluster atom index
         do s2 = 1, n_shell
           a2 = atom_for_shell(s2)
           c2 = self%atom_for_cell_atom(a2,q)  ! cluster atom index
           n2(c1,c2) = n2(c1,c2) + 1
         end do
       end do
     end do
     call destroy_(atom_for_shell)

   end function

!  **************
!  Output methods
!  **************

   subroutine put(self)
    type(cluster_type) :: self
    ! Put the list of vertices for the object

      call ensure_(tonto,self%info_made,"CLUSTER:put ... call make_info first")
      call flush_(stdout)
      call text_(stdout,"Cluster information:")
      call flush_(stdout)
      call show_(stdout,"Radius                  =",self%radius)
      call show_(stdout,"Add criteria            =",self%add_criteria)
      call show_(stdout,"No. of atoms            =",self%n_atoms)
      call show_(stdout,"No. of fragment atoms   =",self%n_fragment_atoms)
      call show_(stdout,"No. of symops           =",self%n_symop)
      call show_(stdout,"Fragment width          =",self%fragment_width)
      call show_(stdout,"Cartesian width         =",cartesian_fragment_width_(self%crystal))
      call show_(stdout,"Fragment offset         =",self%fragment_offset)
      call put_cluster_table_(self,order_atoms_by="symop")
     ! .put_cluster_table(order_atoms_by="atom_distance")

   end subroutine

   subroutine put_cluster_table(self,order_atoms_by)
    type(cluster_type) :: self
    ! Put the cluster information table
      character(*), optional :: order_atoms_by
      character(128) :: order
      integer(kind=kind(1)) :: b,a,q
     ! i :: integer(kind=kind(1))
      real(kind=kind(1.0d0)) :: dist
      integer(kind=kind(1)), dimension(:), pointer :: list

      call ensure_(tonto,self%info_made,"CLUSTER:put_cluster_table ... call make_info first")
      order = "symop"
      if (present(order_atoms_by)) order = order_atoms_by
      call create_(list,self%n_atoms)
      if (order=="symop") then
         list = (/(a,a=1,self%n_atoms)/)
      else if (order=="atom_distance") then
         call die_(tonto,"CLUSTER:put_cluster_table ... ordering not allowed any more, "//trim(order))
        ! .minimum_distance_to_atom.quick_sort(list)
      else
         call die_(tonto,"CLUSTER:put_cluster_table ... unknown ordering, "//trim(order))
      end if
      call flush_(stdout)
      call text_(stdout,"Cluster geometry (crystal axis system):")
      call flush_(stdout)
      call text_(stdout,"Cluster atoms are ordered by "//trim(order))
      call flush_(stdout)
      call dash_(stdout,int_fields=4,real_fields=4,width=12)
      call put_(stdout,"Atom",int_width=.true.)
      call tab_(stdout,real_fields=3)
      call put_(stdout,"Closest",int_width=.true.)
      call put_(stdout,"Minimum")
      call put_(stdout,"Parent",int_width=.true.)
      call put_(stdout,"Parent",int_width=.true.)
      call put_(stdout,"Symop",width=12)
      call flush_(stdout)
      call put_(stdout,"#",int_width=.true.)
      call put_(stdout,"x")
      call put_(stdout,"y")
      call put_(stdout,"z")
      call put_(stdout,"atom",int_width=.true.)
      call put_(stdout,"distance")
      call put_(stdout,"atom",int_width=.true.)
      call put_(stdout,"symop",int_width=.true.)
      call put_(stdout,"s",width=3)
      call put_(stdout,"h",width=3)
      call put_(stdout,"k",width=3)
      call put_(stdout,"l",width=3)
      call flush_(stdout)
      call dash_(stdout,int_fields=4,real_fields=4,width=12)
      call flush_(stdout)
      do b = 1,self%n_atoms
         a = list(b)
         call put_(stdout,a)
         call put_(stdout,self%geometry(1,a))
         call put_(stdout,self%geometry(2,a))
         call put_(stdout,self%geometry(3,a))
        ! stdout.put(.closest_fragment_atom_to_atom(a))
         call put_(stdout,"n/a",int_width=.true.)
        ! dist = .minimum_distance_to_atom(a)
         dist = -1.0d0
         if (dist<0) then; call put_(stdout,"n/a")
         else;             call put_(stdout,dist)
         end if
        ! do i = 1,size(.parent_for_atom(a).element)
        !    if (i>1) &
        !    stdout.tab(int_fields=4,real_fields=4)
            call put_(stdout,self%parent_for_atom(a))
            q = self%symop_for_atom(a)
            call put_(stdout,q)
            call put_(stdout,self%symop(1,q),width=3)
            call put_(stdout,self%symop(2,q),width=3)
            call put_(stdout,self%symop(3,q),width=3)
            call put_(stdout,self%symop(4,q),width=3)
            call flush_(stdout)
        ! end
      end do
      call dash_(stdout,int_fields=4,real_fields=4,width=12)
      call destroy_(list)

   end subroutine

   subroutine put_tonto_input(self)
    type(cluster_type) :: self
    ! Outputs the tonto input file for the cluster, given additionally the list
    ! of atoms which was used to generate the fragment_geometry in crystal.
    ! (See routine make_reduced_group_data).
      real(kind=kind(1.0d0)), dimension(:,:,:), pointer :: seitz
      real(kind=kind(1.0d0)), dimension(3,3) :: therm
      integer(kind=kind(1)) :: a,p,s

      call ensure_(tonto,self%info_made,"CLUSTER:put_tonto_input ... call make_info first")
      call ensure_(tonto,associated(self%asymmetric_cell_atom),"CLUSTER:put_tonto_input ... no atom data")
      call ensure_(tonto,associated(self%crystal),"CLUSTER:put_tonto_input ... no crystal data")
      call ensure_(tonto,associated(self%parent_for_atom),"CLUSTER:put_tonto_input ... no parent atoms")
      call ensure_(tonto,associated(self%symop_for_atom),"CLUSTER:put_tonto_input ... no symops for atoms")
      call text_(stdout,"   atoms= {")
      call flush_(stdout)
      call text_(stdout,"      keys= { label= ")
      call text_(stdout,'              "{ axis_system= crystal }" pos=')
!      stdout.text("              basis_label=")
      call text_(stdout,'              "{ units= angstrom^2 }" thermal_tensor= }')
      call flush_(stdout)
      call text_(stdout,"      data= {")
      seitz => transposed_xyz_seitz_matrices_(self%crystal)  ! transposed !
      do a = 1,self%n_atoms
         p = self%parent_for_atom(a)
         s = self%symop_for_atom(a)
         call put_(stdout,trim(self%asymmetric_cell_atom(p)%label),int_width=.true.)
         call put_(stdout,self%geometry(1,a))
         call put_(stdout,self%geometry(2,a))
         call put_(stdout,self%geometry(3,a))
!         stdout.put(.asymmetric_cell_atom(p).basis.label.trim)
         therm = self%asymmetric_cell_atom(p)%thermal_tensor
         call change_basis_(therm,self%crystal%unitcell%reciprocal_U_matrix)
         call change_basis_(therm,seitz(:,:,self%symop(1,s)))
         call change_basis_(therm,self%crystal%unitcell%direct_U_matrix)
         call convert_to_(therm,"angstrom^2")
         call put_(stdout,therm(1,1))
         call put_(stdout,therm(2,2))
         call put_(stdout,therm(3,3))
         call put_(stdout,therm(1,2))
         call put_(stdout,therm(1,3))
         call put_(stdout,therm(2,3))
         call flush_(stdout)
      end do
      call destroy_(seitz)
      call text_(stdout,"      }")
      call text_(stdout,"   }")
      call flush_(stdout)

   end subroutine

   subroutine put_CX(self,label)
    type(cluster_type) :: self
    ! Outputs some information for the Crystal Explorer program: the list of atoms in
    ! the cluster, their positions, and whether they are part of the generating
    ! fragment or not.
      character(128) :: label
      integer(kind=kind(1)) :: n,p
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: geometry

      call ensure_(tonto,self%info_made,"CLUSTER:put_CX ... call make_info first")
      call ensure_(tonto,associated(self%geometry),"CLUSTER:put_CX ... no cluster geometry")
      call ensure_(tonto,associated(self%crystal),"CLUSTER:put_CX ... no crystal data")
      call ensure_(tonto,associated(self%asymmetric_cell_atom),"CLUSTER:put_CX ... no atom data")
      call ensure_(tonto,associated(self%parent_for_atom),"CLUSTER:put_CX ... no parent atoms")
      call ensure_(tonto,associated(self%is_fragment_atom),"CLUSTER:put_CX ... no is_fragment_atom array")
      call create_copy_(geometry,self%geometry)
      call change_from_fractional_(self%crystal%unitcell,geometry)
      call flush_(stdout)
      call text_(stdout,"begin atoms " // trim(label))
      do n = 1,self%n_atoms
         p = self%parent_for_atom(n)
         call put_(stdout,chemical_symbol_(self%asymmetric_cell_atom(p)))
         call put_(stdout,geometry(1,n))
         call put_(stdout,geometry(2,n))
         call put_(stdout,geometry(3,n))
         call put_(stdout,self%asymmetric_cell_atom(p)%thermal_tensor(1,1))
         call put_(stdout,self%asymmetric_cell_atom(p)%thermal_tensor(1,2))
         call put_(stdout,self%asymmetric_cell_atom(p)%thermal_tensor(1,3))
         call put_(stdout,self%asymmetric_cell_atom(p)%thermal_tensor(2,2))
         call put_(stdout,self%asymmetric_cell_atom(p)%thermal_tensor(2,3))
         call put_(stdout,self%asymmetric_cell_atom(p)%thermal_tensor(3,3))
         if (self%is_fragment_atom(n)) call put_text_(stdout," IN")
         call flush_(stdout)
      end do
      call text_(stdout,"end atoms")
      call destroy_(geometry)

   end subroutine

   subroutine put_spartan(self,label)
    type(cluster_type) :: self
    ! Outputs some information for the Spartan program: the list of atoms in
    ! the cluster, their positions, and whether they are part of the generating
    ! fragment or not.
      character(128) :: label
      character(128) :: word
      integer(kind=kind(1)) :: n,p
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: geometry

   call ensure_(tonto,self%info_made,"CLUSTER:put_spartan ... call make_info first")
   call ensure_(tonto,associated(self%geometry),"CLUSTER:put_spartan ... no cluster geometry")
   call ensure_(tonto,associated(self%crystal),"CLUSTER:put_spartan ... no crystal data")
   call ensure_(tonto,associated(self%asymmetric_cell_atom),"CLUSTER:put_spartan ... no atom data")
   call ensure_(tonto,associated(self%parent_for_atom),"CLUSTER:put_spartan ... no parent atoms")
   call ensure_(tonto,associated(self%is_fragment_atom),"CLUSTER:put_spartan ... no is_fragment_atom array")
      call create_copy_(geometry,self%geometry)
      call change_from_fractional_(self%crystal%unitcell,geometry)
      geometry = 0.52917724924d0*geometry
      call flush_(stdout)
      call text_(stdout,"=== SPARTAN DATA ===")
      call text_(stdout,trim(label))
      call text_(stdout,"M001")
      call text_(stdout,"0 1")
      do n = 1,self%n_atoms
         p = self%parent_for_atom(n)
         call put_(stdout,self%asymmetric_cell_atom(p)%atomic_number)
         call put_(stdout,geometry(1,n))
         call put_(stdout,geometry(2,n))
         call put_(stdout,geometry(3,n))
         call flush_(stdout)
      end do
      call text_(stdout,"ENDCART")
      call text_(stdout,"ATOMLABELS")
      do n = 1,self%n_atoms
         p = self%parent_for_atom(n)
         word = self%asymmetric_cell_atom(p)%label
         word = '"'//trim(word)//'"'
         call put_(stdout,trim(word))
         call flush_(stdout)
      end do
      call text_(stdout,"ENDATOMLABELS")
      call destroy_(geometry)

   end subroutine

end
