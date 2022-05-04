!---------------------------------------------------------------------
!
! ISOSURFACE:
!
! For generating triangulated iso-surfaces for display with the OPENGL
! graphics language. An isosurface is just a list of point, and a list
! of integer triples describing each triangular face of the object.
!
! You can use a homegrown "tesselate" method, or you can use the
! marching cubes "cubify" algorithm to get the isosurface.
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
! $Id: isosurface.foo,v 1.2.2.23 2004/04/21 09:12:54 reaper Exp $
!
!---------------------------------------------------------------------

module ISOSURFACE_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

   use REALVEC_MODULE, only: shrink_
   use REALVEC_MODULE, only: range_
   use REALVEC_MODULE, only: norm_
   use REALVEC_MODULE, only: normalise_
   use REALVEC_MODULE, only: create_
   use REALVEC_MODULE, only: is_zero_
   use REALVEC_MODULE, only: expand_
   use REALVEC_MODULE, only: are_all_equal_
   use REALVEC_MODULE, only: create_copy_
   use REALVEC_MODULE, only: cross_
   use REALVEC_MODULE, only: destroy_

   use INT_MODULE, only: to_str_

   use COLOURFUNCTION_MODULE, only: read_data_and_colour_
   use COLOURFUNCTION_MODULE, only: create_
   use COLOURFUNCTION_MODULE, only: set_reverse_defaults_
   use COLOURFUNCTION_MODULE, only: create_copy_
   use COLOURFUNCTION_MODULE, only: put_
   use COLOURFUNCTION_MODULE, only: get_RGB_for_
   use COLOURFUNCTION_MODULE, only: RGB255_for_
   use COLOURFUNCTION_MODULE, only: destroy_
   use COLOURFUNCTION_MODULE, only: rescale_data_

   use INTVECMAT3_MODULE, only: zero_
   use INTVECMAT3_MODULE, only: set_to_
   use INTVECMAT3_MODULE, only: create_
   use INTVECMAT3_MODULE, only: destroy_

   use INTMAT_MODULE, only: shrink_columns_
   use INTMAT_MODULE, only: range_
   use INTMAT_MODULE, only: create_
   use INTMAT_MODULE, only: bin_XY_data_
   use INTMAT_MODULE, only: expand_columns_
   use INTMAT_MODULE, only: create_copy_
   use INTMAT_MODULE, only: destroy_

   use REALMAT3_MODULE, only: set_to_
   use REALMAT3_MODULE, only: create_
   use REALMAT3_MODULE, only: destroy_

   use INTVEC_MODULE, only: append_only_if_unique_
   use INTVEC_MODULE, only: append_
   use INTVEC_MODULE, only: range_
   use INTVEC_MODULE, only: create_
   use INTVEC_MODULE, only: set_to_
   use INTVEC_MODULE, only: bin_XY_data_
   use INTVEC_MODULE, only: destroy_

   use REALMAT4_MODULE, only: create_

   use INTVECINTVECHASH_MODULE, only: append_pair_
   use INTVECINTVECHASH_MODULE, only: create_
   use INTVECINTVECHASH_MODULE, only: has_key_
   use INTVECINTVECHASH_MODULE, only: create_copy_
   use INTVECINTVECHASH_MODULE, only: value_for_item_
   use INTVECINTVECHASH_MODULE, only: set_reverse_search_
   use INTVECINTVECHASH_MODULE, only: destroy_

   use TEXTFILE_MODULE, only: stdin
   use TEXTFILE_MODULE, only: stdout
   use TEXTFILE_MODULE, only: set_default_units_
   use TEXTFILE_MODULE, only: text_
   use TEXTFILE_MODULE, only: next_str_
   use TEXTFILE_MODULE, only: skip_next_item_
   use TEXTFILE_MODULE, only: next_item_
   use TEXTFILE_MODULE, only: show_
   use TEXTFILE_MODULE, only: put_
   use TEXTFILE_MODULE, only: read_
   use TEXTFILE_MODULE, only: set_use_labels_
   use TEXTFILE_MODULE, only: flush_
   use TEXTFILE_MODULE, only: reverted_

   use STR_MODULE, only: to_lower_case_

   use MARCHINGCUBE_MODULE, only: get_edge_gaussian_curvatures_
   use MARCHINGCUBE_MODULE, only: set_big_interior_
   use MARCHINGCUBE_MODULE, only: set_iso_value_
   use MARCHINGCUBE_MODULE, only: set_defaults_
   use MARCHINGCUBE_MODULE, only: has_front_face_on_surface_
   use MARCHINGCUBE_MODULE, only: is_nearly_on_surface_
   use MARCHINGCUBE_MODULE, only: set_hessian_info_
   use MARCHINGCUBE_MODULE, only: get_edge_vertex_positions_
   use MARCHINGCUBE_MODULE, only: set_left_skip_bit_string_
   use MARCHINGCUBE_MODULE, only: is_on_surface_
   use MARCHINGCUBE_MODULE, only: get_edge_mean_curvatures_
   use MARCHINGCUBE_MODULE, only: set_vertex_info_
   use MARCHINGCUBE_MODULE, only: set_left_info_
   use MARCHINGCUBE_MODULE, only: set_below_skip_bit_string_
   use MARCHINGCUBE_MODULE, only: get_triangle_vertex_indices_
   use MARCHINGCUBE_MODULE, only: get_edge_vertex_gradients_
   use MARCHINGCUBE_MODULE, only: set_triangulation_info_
   use MARCHINGCUBE_MODULE, only: set_n_pt_
   use MARCHINGCUBE_MODULE, only: set_front_info_
   use MARCHINGCUBE_MODULE, only: is_outside_surface_
   use MARCHINGCUBE_MODULE, only: set_hessian_eval_array_
   use MARCHINGCUBE_MODULE, only: set_below_info_
   use MARCHINGCUBE_MODULE, only: reset_
   use MARCHINGCUBE_MODULE, only: has_lower_face_on_surface_
   use MARCHINGCUBE_MODULE, only: interpolate_faces_
   use MARCHINGCUBE_MODULE, only: set_case_info_
   use MARCHINGCUBE_MODULE, only: set_front_skip_bit_string_
   use MARCHINGCUBE_MODULE, only: set_side_length_
   use MARCHINGCUBE_MODULE, only: no_of_active_edges_
   use MARCHINGCUBE_MODULE, only: is_inside_surface_
   use MARCHINGCUBE_MODULE, only: set_cube_bit_string_
   use MARCHINGCUBE_MODULE, only: has_left_face_on_surface_

   use ATOMVEC_MODULE, only: n_atom_

   use PLOTGRID_MODULE, only: put_
   use PLOTGRID_MODULE, only: update_for_marching_cubes_
   use PLOTGRID_MODULE, only: set_defaults_
   use PLOTGRID_MODULE, only: set_points_widths_origin_
   use PLOTGRID_MODULE, only: read_keywords_
   use PLOTGRID_MODULE, only: reset_defaults_
   use PLOTGRID_MODULE, only: make_points_
   use PLOTGRID_MODULE, only: make_cube_of_points_

   use REALMAT_MODULE, only: shrink_columns_
   use REALMAT_MODULE, only: create_
   use REALMAT_MODULE, only: index_of_minimum_column_norm_
   use REALMAT_MODULE, only: expand_columns_
   use REALMAT_MODULE, only: create_copy_
   use REALMAT_MODULE, only: destroy_
   use REALMAT_MODULE, only: column_norms_

   use REAL_MODULE, only: same_as_
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

   public    get_vertex_curvedness_
   interface get_vertex_curvedness_
      module procedure get_vertex_curvedness
   end interface

   public    nullify_ptr_part_
   interface nullify_ptr_part_
      module procedure nullify_ptr_part
   end interface

   public    read_colourfunction_
   interface read_colourfunction_
      module procedure read_colourfunction
   end interface

   public    put_CX_
   interface put_CX_
      module procedure put_CX
      module procedure put_CX_1
   end interface

   public    test_func_
   interface test_func_
      module procedure test_func
   end interface

   public    put_grid_
   interface put_grid_
      module procedure put_grid
   end interface

   public    average_face_gradient_
   interface average_face_gradient_
      module procedure average_face_gradient
   end interface

   public    read_kind_
   interface read_kind_
      module procedure read_kind
   end interface

   public    read_iso_value_
   interface read_iso_value_
      module procedure read_iso_value
   end interface

   public    put_points_
   interface put_points_
      module procedure put_points
   end interface

   public    read_keywords_
   interface read_keywords_
      module procedure read_keywords
   end interface

   public    property_bounds_set_
   interface property_bounds_set_
      module procedure property_bounds_set
   end interface

   private    set_isosurface_info_arrays_
   interface set_isosurface_info_arrays_
      module procedure set_isosurface_info_arrays
   end interface

   public    put_vrml_
   interface put_vrml_
      module procedure put_vrml
   end interface

   public    read_use_interpolator_
   interface read_use_interpolator_
      module procedure read_use_interpolator
   end interface

   public    prepare_grid_
   interface prepare_grid_
      module procedure prepare_grid
   end interface

   private    make_2_cube_of_values_
   interface make_2_cube_of_values_
      module procedure make_2_cube_of_values
   end interface

   private    nonrecursively_cubify_
   interface nonrecursively_cubify_
      module procedure nonrecursively_cubify
   end interface

   public    put_binned_d_i_d_e_RGBs_
   interface put_binned_d_i_d_e_RGBs_
      module procedure put_binned_d_i_d_e_RGBs
   end interface

   public    copy_
   interface copy_
      module procedure copy
   end interface

   public    put_faces_
   interface put_faces_
      module procedure put_faces
   end interface

   public    default_big_interior_
   interface default_big_interior_
      module procedure default_big_interior
   end interface

   public    test_
   interface test_
      module procedure test
   end interface

   private    make_3_cube_of_values_
   interface make_3_cube_of_values_
      module procedure make_3_cube_of_values
   end interface

   public    put_face_normals_
   interface put_face_normals_
      module procedure put_face_normals
   end interface

   public    read_surface_property_
   interface read_surface_property_
      module procedure read_surface_property
   end interface

   public    destroy_
   interface destroy_
      module procedure destroy
   end interface

   private    recursively_cubify_
   interface recursively_cubify_
      module procedure recursively_cubify
   end interface

   public    put_nearest_external_atom_RGBs_
   interface put_nearest_external_atom_RGBs_
      module procedure put_nearest_external_atom_RGBs
   end interface

   public    put_vertex_normals_
   interface put_vertex_normals_
      module procedure put_vertex_normals
   end interface

   public    read_units_
   interface read_units_
      module procedure read_units
   end interface

   public    cubify_
   interface cubify_
      module procedure cubify
   end interface

   public    put_vertex_shape_index_
   interface put_vertex_shape_index_
      module procedure put_vertex_shape_index
   end interface

   public    read_big_interior_
   interface read_big_interior_
      module procedure read_big_interior
   end interface

   public    read_surface_point_
   interface read_surface_point_
      module procedure read_surface_point
   end interface

   public    put_vertex_RMS_curvature_
   interface put_vertex_RMS_curvature_
      module procedure put_vertex_RMS_curvature
   end interface

   public    put_vertex_curvedness_
   interface put_vertex_curvedness_
      module procedure put_vertex_curvedness
   end interface

   public    total_area_
   interface total_area_
      module procedure total_area
   end interface

   public    get_vertex_shape_index_
   interface get_vertex_shape_index_
      module procedure get_vertex_shape_index
   end interface

   public    read_surface_property_lb_
   interface read_surface_property_lb_
      module procedure read_surface_property_lb
   end interface

   private    make_5_cube_of_values_
   interface make_5_cube_of_values_
      module procedure make_5_cube_of_values
   end interface

   public    put_connected_area_
   interface put_connected_area_
      module procedure put_connected_area
   end interface

   private    set_default_cube_
   interface set_default_cube_
      module procedure set_default_cube
   end interface

   public    put_vertex_mean_curvatures_
   interface put_vertex_mean_curvatures_
      module procedure put_vertex_mean_curvatures
   end interface

   public    destroy_ptr_part_
   interface destroy_ptr_part_
      module procedure destroy_ptr_part
   end interface

   private    rotate_gradients_
   interface rotate_gradients_
      module procedure rotate_gradients
   end interface

   public    connected_property_area_
   interface connected_property_area_
      module procedure connected_property_area
      module procedure connected_property_area_1
      module procedure connected_property_area_2
   end interface

   public    read_triangulation_method_
   interface read_triangulation_method_
      module procedure read_triangulation_method
   end interface

   public    plot_function_
   interface plot_function_
      module procedure plot_function
   end interface

   public    face_area_
   interface face_area_
      module procedure face_area
   end interface

   private    no_of_divisions_
   interface no_of_divisions_
      module procedure no_of_divisions
   end interface

   public    put_nearest_internal_atom_RGBs_
   interface put_nearest_internal_atom_RGBs_
      module procedure put_nearest_internal_atom_RGBs
   end interface

   public    get_principal_curvatures_
   interface get_principal_curvatures_
      module procedure get_principal_curvatures
   end interface

   public    nearest_atom_distances_
   interface nearest_atom_distances_
      module procedure nearest_atom_distances
   end interface

   public    process_keyword_
   interface process_keyword_
      module procedure process_keyword
   end interface

   private    divide_
   interface divide_
      module procedure divide
   end interface

   public    put_
   interface put_
      module procedure put
   end interface

   public    put_normals_as_vertex_RGBs_
   interface put_normals_as_vertex_RGBs_
      module procedure put_normals_as_vertex_RGBs
   end interface

   public    surface_point_set_
   interface surface_point_set_
      module procedure surface_point_set
   end interface

   private    append_new_face_info_
   interface append_new_face_info_
      module procedure append_new_face_info
   end interface

   public    set_defaults_
   interface set_defaults_
      module procedure set_defaults
   end interface

   public    put_vertex_gradients_
   interface put_vertex_gradients_
      module procedure put_vertex_gradients
   end interface

   public    create_
   interface create_
      module procedure create
   end interface

   public    create_copy_
   interface create_copy_
      module procedure create_copy
   end interface

   public    put_vertex_gaussian_curvatures_
   interface put_vertex_gaussian_curvatures_
      module procedure put_vertex_gaussian_curvatures
   end interface

   private    update_4_slab_
   interface update_4_slab_
      module procedure update_4_slab
   end interface

   public    put_face_colours_
   interface put_face_colours_
      module procedure put_face_colours
   end interface

   public    read_junk_
   interface read_junk_
      module procedure read_junk
   end interface

   public    put_d_i_d_e_RGBs_
   interface put_d_i_d_e_RGBs_
      module procedure put_d_i_d_e_RGBs
   end interface

   public    index_of_nearest_point_
   interface index_of_nearest_point_
      module procedure index_of_nearest_point
   end interface

   public    get_vertex_RMS_curvature_
   interface get_vertex_RMS_curvature_
      module procedure get_vertex_RMS_curvature
   end interface

   public    read_plotgrid_
   interface read_plotgrid_
      module procedure read_plotgrid
   end interface

   public    read_surface_property_ub_
   interface read_surface_property_ub_
      module procedure read_surface_property_ub
   end interface

   public    average_face_normal_
   interface average_face_normal_
      module procedure average_face_normal
   end interface

   public    put_vertex_curvatures_
   interface put_vertex_curvatures_
      module procedure put_vertex_curvatures
   end interface

    !iter :: integer(kind=kind(1)) = 0

contains

   subroutine create(self,atom)
    type(isosurface_type) :: self
    ! Create a grid object
     pointer :: self
     type(atom_type), dimension(:), pointer, optional :: atom

     nullify(self)
     allocate(self)

     call nullify_ptr_part_(self)
     call set_defaults_(self,atom)

   end subroutine

   subroutine create_copy(self,s)
    type(isosurface_type) :: self
    ! Create self as a duplicate of "s".
     pointer :: self
     type(isosurface_type), intent(in) :: s

     call create_(self)
     call copy_(self,s)

   end subroutine

   subroutine copy(self,s)
    type(isosurface_type) :: self
    ! Copy self.
     type(isosurface_type), intent(in) :: s

     self = s
     if (associated(s%point)) call create_copy_(self%point,s%point)
     if (associated(s%face)) call create_copy_(self%face,s%face)
     if (associated(s%point_gradient)) call create_copy_(self%point_gradient,s%point_gradient)
     if (associated(s%point_mean_curvature)) call create_copy_(self%point_mean_curvature,s%point_mean_curvature)
     if (associated(s%point_gaussian_curvature)) call create_copy_(self%point_gaussian_curvature,s%point_gaussian_curvature)
     if (associated(s%hash)) call create_copy_(self%hash,s%hash)
     if (associated(s%surface_property_values)) call create_copy_(self%surface_property_values,s%surface_property_values)
     if (associated(s%colour)) call create_copy_(self%colour,s%colour)

   end subroutine

   subroutine destroy(self)
    type(isosurface_type) :: self
    ! Destroy object
      pointer :: self

      if (.not. associated(self)) then;   return; end if
      call destroy_ptr_part_(self)
      deallocate(self)

   end subroutine

   subroutine nullify_ptr_part(self)
    type(isosurface_type) :: self
    ! Nullify the pointer parts

      nullify(self%point)
      nullify(self%face)
      nullify(self%point_gradient)
      nullify(self%point_mean_curvature)
      nullify(self%point_gaussian_curvature)
      nullify(self%hash)
      nullify(self%surface_property_values)
      nullify(self%colour)
      nullify(self%atom)
       ! These are for tesselate method (not fully operational)
     ! nullify(.shift)
     ! nullify(.adjoining_face)
     ! nullify(.adjoining_edge)
     ! nullify(.ok)
     ! nullify(.ok_neighbours)

   end subroutine

   subroutine destroy_ptr_part(self)
    type(isosurface_type) :: self
    ! Destroy the pointer parts

      call destroy_(self%point)
      call destroy_(self%face)
      call destroy_(self%point_gradient)
      call destroy_(self%point_mean_curvature)
      call destroy_(self%point_gaussian_curvature)
      call destroy_(self%hash)
      call destroy_(self%surface_property_values)
      call destroy_(self%colour)
      nullify(self%atom)  ! never destroy this
       ! These are for tesselate method (not fully operational)
     ! .shift.destroy
     ! .adjoining_face.destroy
     ! .adjoining_edge.destroy
     ! .ok.destroy
     ! .ok_neighbours.destroy

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

   subroutine set_defaults(self,atom)
    type(isosurface_type) :: self
    ! Set up a defaults for marching cubes
      type(atom_type), dimension(:), pointer, optional :: atom

      self%iso_kind = " "
      self%triangulation_method = "unknown"
      self%iso_value = 1.0d0
      self%n_pt   = 0
      self%n_face = 0
      self%volume = 0.0d0
      self%volume_min = 0.0d0
      self%volume_max = 0.0d0
      self%n_skip = 0
      self%level = 0
      self%final_level = 0
      self%del = 1.0d0
      self%surface_property = " "
      self%chop_surface_property_range = .false.
      self%surface_property_cutoff_range = (/-huge(1.0d0),huge(1.0d0)/)
      self%x = 0
      self%y = 0
      self%z = 0
      self%big_interior = .false.
      self%use_interpolator = .false.
     ! .accuracy = 10.0d0**(-3)
     ! .smallness = 0.8
     ! .flatness = 0.5
     ! .n_skip = 0
      self%surface_point = (/10.0d0**(-6),10.0d0**(-6),10.0d0**(-6)/)  ! [10.0d0**(-6),10.0d0**(-6),10.0d0**(-6)]
      self%surface_property_lower_bound = 179.55d0
      self%surface_property_upper_bound = 179.55d0
      if (associated(self%colour)) call destroy_(self%colour)
      call create_(self%colour)
      call set_reverse_defaults_(self%colour)
      if (present(atom)) then
      if (associated(atom)) then
         self%atom => atom
      end if
      end if
      call set_defaults_(self%grid,atom)

   end subroutine

!   set_default_tesselation ::: leaky
!   ! Set up a defaults.
!      .iso_value = 0.5d0
!      .n_pt   = 4
!      .n_face = 4
!      .accuracy = 10.0d0**(-2)
!      .smallness = 0.8
!      .flatness = 0.5
!      .n_skip = 0
!      ! Set up a default tesselate surface, a tetrahedron.
!      .point.destroy
!      .point.create(3,4)
!      .point(:,1) = [ 1, 1, 1 ]
!      .point(:,2) = [-1,-1, 1 ]
!      .point(:,3) = [ 1,-1,-1 ]
!      .point(:,4) = [-1, 1,-1 ]
!      .shift.destroy
!      .shift.create(4)
!      .shift = sqrt(3.0d0)
!      .face.destroy
!      .face.create(3,4)
!      .face(:,1)  = [ 1, 2, 3 ]
!      .face(:,2)  = [ 1, 3, 4 ]
!      .face(:,3)  = [ 1, 4, 2 ]
!      .face(:,4)  = [ 2, 4, 3 ]
!      .face.destroy
!      .face.create(3,4)
!   end

   function default_big_interior(self) result(res)
    type(isosurface_type) :: self
    ! Return .true. if the interior of the isosurface is bigger than the exterior,
    ! i.e. gradients for the isosurface plot are to be reversed by default, so that
    ! they point outside the isosurface. NOTE: gradients should be reversed if the
    ! function has bigger values *inside* the isosurface.
     logical(kind=kind(.true.)) :: res

     res = .true.
     select case (self%iso_kind)
        case("electron_density   "); res = .true.
        case("laplacian_density  "); res = .true.
        case("orbital_density    "); res = .true.
        case("orbital            "); res = .true.
        case("delta_density      "); res = .true.  ! only for +ve densities ...
        case("true_fermi_mobility"); res = .true.
        case("fermi_mobility     "); res = .true.
        case("spin_density       "); res = .true.  ! only for +ve densities
        case("hirshfeld_density  "); res = .true.
        case("stockholder_density"); res = .true.
        case("elf                "); res = .false.
        case("tsirelson_elf      "); res = .false.
        case("electric_potential "); res = .true.
        case default;       ! don't know what it should be ...
     end select

   end function

   function property_bounds_set(self) result(res)
    type(isosurface_type) :: self
    ! Return .true. if the property bounds have been set
      logical(kind=kind(.true.)) :: res

      res = .not. same_as_(self%surface_property_lower_bound,self%surface_property_upper_bound)

   end function

   function surface_point_set(self) result(res)
    type(isosurface_type) :: self
    ! Return .true. if the surface point has been set
      logical(kind=kind(.true.)) :: res

      res = any(self%surface_point/=(/10.0d0**(-6),10.0d0**(-6),10.0d0**(-6)/))

   end function

!  *************
!  Input methods
!  *************

   subroutine read_keywords(self)
    type(isosurface_type) :: self
    ! Read data from "stdin" using keyword style input.
    ! The following code is inherited from OBJECT
     character(128) :: word

     call ensure_(tonto,next_item_(stdin)=="{","ISOSURFACE:read_keywords ... expecting open bracket symbol, {")
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
    type(isosurface_type) :: self
    ! Process a comand "keyword". Data is inputted from "stdin", unless
    ! "word" is a sequence of blank separated strings. In this case,
    ! the sequence is processed as if it were a separate file.
      character(*) :: keyword
      character(128) :: word

      word = keyword
      call to_lower_case_(word)
      select case (word)
         case ("}                            ")   ! exit case
         case ("colourfunction=              "); call read_colourfunction_(self)
         case ("put_colourfunction           "); call put_(self%colour)
         case ("big_interior=                "); call read_big_interior_(self)
         case ("iso_value=                   "); call read_iso_value_(self)
         case ("kind=                        "); call read_kind_(self)
         case ("plotgrid=                    "); call read_plotgrid_(self)
         case ("surface_property_lower_bound=        "); call read_surface_property_lb_(self)
         case ("surface_property_upper_bound=        "); call read_surface_property_ub_(self)
         case ("surface_property_cutoff_range=       "); call read_(stdin,self%surface_property_cutoff_range)
                                                         self%chop_surface_property_range = .true.
         case ("chop_surface_property_range=   "); call read_(stdin,self%chop_surface_property_range)
         case ("put                          "); call put_(self)
         case ("put_connected_area           "); call put_connected_area_(self)
         case ("put_cx                       "); call put_CX_(self,"CX")
         case ("put_faces                    "); call put_faces_(self)
         case ("put_face_colours             "); call put_face_colours_(self)
         case ("put_face_normals             "); call put_face_normals_(self)
         case ("put_normals_as_vertex_rgbs   "); call put_normals_as_vertex_RGBs_(self)
         case ("put_points                   "); call put_points_(self)
         case ("put_vertex_gradients         "); call put_vertex_gradients_(self)
         case ("put_grid                     "); call put_grid_(self)
         case ("surface_point=               "); call read_surface_point_(self)
         case ("surface_property=            "); call read_surface_property_(self)
         case ("test                         "); call test_(self)
         case ("triangulation_method=        "); call read_triangulation_method_(self)
         case ("units=                       "); call read_units_(self)
         case ("use_interpolator=            "); call read_use_interpolator_(self)
         case default;           allocate(tonto%known_keywords(27))
         tonto%known_keywords(1) = "}                            "
         tonto%known_keywords(2) = "colourfunction=              "
         tonto%known_keywords(3) = "put_colourfunction           "
         tonto%known_keywords(4) = "big_interior=                "
         tonto%known_keywords(5) = "iso_value=                   "
         tonto%known_keywords(6) = "kind=                        "
         tonto%known_keywords(7) = "plotgrid=                    "
         tonto%known_keywords(8) = "surface_property_lower_bound=        "
         tonto%known_keywords(9) = "surface_property_upper_bound=        "
         tonto%known_keywords(10) = "surface_property_cutoff_range=       "
         tonto%known_keywords(11) = "chop_surface_property_range=   "
         tonto%known_keywords(12) = "put                          "
         tonto%known_keywords(13) = "put_connected_area           "
         tonto%known_keywords(14) = "put_cx                       "
         tonto%known_keywords(15) = "put_faces                    "
         tonto%known_keywords(16) = "put_face_colours             "
         tonto%known_keywords(17) = "put_face_normals             "
         tonto%known_keywords(18) = "put_normals_as_vertex_rgbs   "
         tonto%known_keywords(19) = "put_points                   "
         tonto%known_keywords(20) = "put_vertex_gradients         "
         tonto%known_keywords(21) = "put_grid                     "
         tonto%known_keywords(22) = "surface_point=               "
         tonto%known_keywords(23) = "surface_property=            "
         tonto%known_keywords(24) = "test                         "
         tonto%known_keywords(25) = "triangulation_method=        "
         tonto%known_keywords(26) = "units=                       "
         tonto%known_keywords(27) = "use_interpolator=            "
         call unknown_(tonto,word,"ISOSURFACE:process_keyword")
         deallocate(tonto%known_keywords)
      end select

   end subroutine

   subroutine read_junk(self)
    type(isosurface_type) :: self
    ! Read in a junk string, useful for ignoring a field
    ! The following code is inherited from OBJECT

      call skip_next_item_(stdin)

   end subroutine

   subroutine read_big_interior(self)
    type(isosurface_type) :: self
    ! Read a switch which tells if the interior of the isosurface is bigger than
    ! the exterior i.e. whether to reverse the surface gradients or not.

      call read_(stdin,self%big_interior)

   end subroutine

   subroutine read_colourfunction(self)
    type(isosurface_type) :: self
    ! Read the details of the colourfunction to be used

      call read_data_and_colour_(self%colour)

   end subroutine

   subroutine read_iso_value(self)
    type(isosurface_type) :: self
    ! Read the defining iso_value for the isosurface

      call read_(stdin,self%iso_value)

   end subroutine

   subroutine read_kind(self)
    type(isosurface_type) :: self
    ! Read the kind of isosurface to plot.

      call read_(stdin,self%iso_kind)
      select case (self%iso_kind)
        case("elf                ")
        case("electric_potential ")
        case("electron_density   ")
        case("fermi_mobility     ")
        case("hirshfeld_density  ")
        case("laplacian_density  ")
        case("orbital            ")
        case("orbital_density    ")
        case("spin_density       ")
        case("stockholder_density")
        case("true_fermi_mobility")
        case("tsirelson_elf      ")
        case default; allocate(tonto%known_keywords(12))
        tonto%known_keywords(1) = "elf                "
        tonto%known_keywords(2) = "electric_potential "
        tonto%known_keywords(3) = "electron_density   "
        tonto%known_keywords(4) = "fermi_mobility     "
        tonto%known_keywords(5) = "hirshfeld_density  "
        tonto%known_keywords(6) = "laplacian_density  "
        tonto%known_keywords(7) = "orbital            "
        tonto%known_keywords(8) = "orbital_density    "
        tonto%known_keywords(9) = "spin_density       "
        tonto%known_keywords(10) = "stockholder_density"
        tonto%known_keywords(11) = "true_fermi_mobility"
        tonto%known_keywords(12) = "tsirelson_elf      "
        call unknown_(tonto,self%iso_kind,"ISOSURFACE:read_kind")
        deallocate(tonto%known_keywords)
     end select
     self%big_interior = default_big_interior_(self)

   end subroutine

   subroutine read_triangulation_method(self)
    type(isosurface_type) :: self
    ! Read the triangulation method a number whose smallness correlates with how
    ! small each face of the generated isosurface is.

      call read_(stdin,self%triangulation_method)
      select case (self%triangulation_method)
         case("marching_cube          ")
         case("recursive_marching_cube")
         case default; allocate(tonto%known_keywords(2))
         tonto%known_keywords(1) = "marching_cube          "
         tonto%known_keywords(2) = "recursive_marching_cube"
         call unknown_(tonto,self%triangulation_method,"ISOSURFACE:read_triangulation_method")
         deallocate(tonto%known_keywords)
      end select

   end subroutine

   subroutine read_use_interpolator(self)
    type(isosurface_type) :: self
    ! Read whether to use an interpolator to evaluate the function values on the
    ! surface. This may or may not be used by the object calling the isosurface
    ! module.

      call read_(stdin,self%use_interpolator)

   end subroutine

   subroutine read_plotgrid(self)
    type(isosurface_type) :: self
    ! Read in the plot grid data

      call reset_defaults_(self%grid)  ! don't reset bounding box or axes
      call read_keywords_(self%grid)
  ! call ensure_(tonto,.grid.width.are_all_equal,"grid is not a cubical volume")

   end subroutine

   subroutine read_surface_point(self)
    type(isosurface_type) :: self
    ! Read a surface point which lies on, or near the surface.

      call read_(stdin,self%surface_point)

   end subroutine

   subroutine read_surface_property(self)
    type(isosurface_type) :: self
    ! Read surface property
      character(128) :: property

      call read_(stdin,self%surface_property)
      property = self%surface_property
      call to_lower_case_(property)
!      select case (property)
!         case ("mean_curvature")
!         case ("gaussian_curvature")
!         case ("rms_curvature")
!         case ("curvedness")
!         case ("shape_index")
!         case default; call die_(tonto,"unknown case option: " // trim(property))
!      end

   end subroutine

   subroutine read_surface_property_lb(self)
    type(isosurface_type) :: self
    ! Read the value of a property lower bound.

      call read_(stdin,self%surface_property_lower_bound)

   end subroutine

   subroutine read_surface_property_ub(self)
    type(isosurface_type) :: self
    ! Read the value of a property upper bound.

      call read_(stdin,self%surface_property_upper_bound)

   end subroutine

   subroutine read_units(self)
    type(isosurface_type) :: self
    ! Read a string which describes the units to be used
    ! The following code is inherited from OBJECT

      call set_default_units_(stdin,next_str_(stdin))

   end subroutine

!   read_accuracy
!   ! Read a number which tells to what accuracy each generated isosurface
!   ! is determined
!      unit :: STR
!      stdin.read(.accuracy)
!   end
!
!   read_flatness
!   ! Read a number whose smallness correlates with the flatness of the
!   ! generated isosurface
!      unit :: STR
!      stdin.read(.flatness)
!   end
!
!   read_smallness
!   ! Read a number whose smallness correlates with how small each face of the
!   ! generated isosurface is.
!      unit :: STR
!      stdin.read(.smallness)
!   end

!  *********************
!  Marching cube methods
!  *********************

   subroutine cubify(self,func)
    type(isosurface_type) :: self
    ! Generate the isosurface using the marching cube algorithm.
      interface
         subroutine func(values,pt)
            real(kind=kind(1.0d0)), dimension(:), intent(out) :: values
            real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: pt
         end subroutine
      end interface

      call prepare_grid_(self)
      call put_(self)
      select case (self%triangulation_method)
         case("marching_cube          "); call nonrecursively_cubify_(self,func)
         case("recursive_marching_cube"); call recursively_cubify_(self,func)
         case default;        allocate(tonto%known_keywords(2))
         tonto%known_keywords(1) = "marching_cube          "
         tonto%known_keywords(2) = "recursive_marching_cube"
         call unknown_(tonto,self%triangulation_method,"ISOSURFACE:cubify")
         deallocate(tonto%known_keywords)
      end select
      call put_(self)

   end subroutine

   subroutine prepare_grid(self)
    type(isosurface_type) :: self
    ! Prepare the grid for the different kinds of isosurface alogorithms.
    ! Particularly, for the recursive method, set the .final_level and the
    ! .scan_level. Then output the grid information before doing anything, so
    ! the user can check everything is OK.
      integer(kind=kind(1)) :: n_div,n_pt
      real(kind=kind(1.0d0)) :: scale

      select case (self%triangulation_method)
         case("marching_cube          ")
            call update_for_marching_cubes_(self%grid)
            self%del = self%grid%del
         case("recursive_marching_cube")
            call ensure_(tonto,are_all_equal_(self%grid%width),"ISOSURFACE:prepare_grid ... grid widths must be all equal")
            n_div = no_of_divisions_(self,1.0d0)   ! Should be an adjustable parameter?
            n_div = max(n_div,4)            ! At least 4 divisions ...
            n_pt = 2**n_div
            if (n_pt>(self%grid%n_x-1)) then
               call warn_(tonto,"ISOSURFACE:prepare_grid ... No. of points not sufficient, adjusting to be larger")
               self%grid%n_x = n_pt + 1
            end if
            self%final_level = ceiling(log(real(self%grid%n_x-2))/log(2.0d0))
           ! .scan_level  = ceiling(log(real(n_pt-2))/log(2.0d0))
            self%scan_level  = 4
            call ensure_(tonto,self%final_level<=32,"ISOSURFACE:prepare_grid ... too many grid points: # of divisions = "//tr&
&im(to_str_(self%final_level)))
            call ensure_(tonto,self%final_level>  3,"ISOSURFACE:prepare_grid ... too few grid points: # of divisions = "//tri&
&m(to_str_(self%final_level)))
            call warn_(tonto,"ISOSURFACE:prepare_grid ... Adjusting grid points higher to the nearest power of 2")
            self%grid%n_x = 2**self%final_level + 1
            call set_points_widths_origin_(self%grid)
            if (self%grid%desired_separation>0.0d0) then
               scale = self%grid%desired_separation/self%grid%del
               self%grid%width = self%grid%width*scale
               call set_points_widths_origin_(self%grid)
            end if
            self%del = self%grid%width(1)
         case default;        allocate(tonto%known_keywords(2))
         tonto%known_keywords(1) = "marching_cube          "
         tonto%known_keywords(2) = "recursive_marching_cube"
         call unknown_(tonto,self%triangulation_method,"ISOSURFACE:prepare_grid")
         deallocate(tonto%known_keywords)
      end select

   end subroutine

   subroutine nonrecursively_cubify(self,func)
    type(isosurface_type) :: self
    ! Generate the isosurface using the standard marching cube algorithm
    ! *without* any recursion.
      interface
         subroutine func(values,pt)
            real(kind=kind(1.0d0)), dimension(:), intent(out) :: values
            real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: pt
         end subroutine
      end interface
      real(kind=kind(1.0d0)) :: vol
      integer(kind=kind(1)) :: x,y,z
      type(marchingcube_type) :: cube
      real(kind=kind(1.0d0)), dimension(:,:,:), pointer :: f
      real(kind=kind(1.0d0)), dimension(:,:,:,:), pointer :: p
      type(intvec__type), dimension(:,:,:), pointer :: info

      call set_isosurface_info_arrays_(self)                ! Set isosurface info defaults
      call set_default_cube_(self,cube)
      self%volume_min = 0.0d0
      self%volume_max = 0.0d0
      vol = self%grid%del**3
       ! Make the 4-slab of data
      call create_(p,3,self%grid%n_x,self%grid%n_y,4)          ! slab of points
      call create_(f,self%grid%n_x,self%grid%n_y,4)            ! function values
      call create_(info,self%grid%n_x,self%grid%n_y,2,(/0,11/))  ! unique vertex index information
      call zero_(info)
       ! Find all the marching cubes. The grid points on the end
       ! are not looped over, they are only there to evaluate normals
      do z = 2,self%grid%n_z-2                       ! Loop bottom to top
         call update_4_slab_(self,p,f,z,func)              ! Get a new z-slab of function values
         call set_to_(info(:,:,1:1),info(:,:,2:2))     ! Set previously saved edge info
         call zero_(info(:,:,2:2))                      ! zero current edge info
         do y = 2,self%grid%n_y-2                    ! Loop over the slab
         do x = 2,self%grid%n_x-2                    ! ... left, right; then front, back
            call set_vertex_info_(cube,p(:,x:x+1,y:y+1,2:3),f(x:x+1,y:y+1,2:3))
            call set_case_info_(cube)
            if (is_inside_surface_(cube))      self%volume_min = self%volume_min + vol
            if (.not. is_outside_surface_(cube)) self%volume_max = self%volume_max + vol
            if (.not. is_on_surface_(cube)) cycle  ! <<<<<<<<<<<<<<<<<<<<<<<<<<<<
            call set_triangulation_info_(cube)
             ! Use previous edge info, if available.
            if (x>2) call set_left_info_(cube,left=info(x-1,y,2)%element)
            if (y>2) call set_front_info_(cube,front=info(x,y-1,2)%element)
            if (z>2) call set_below_info_(cube,below=info(x,y,1)%element)
            call set_hessian_info_(cube,f(x-1:x+2,y-1:y+2,1:4))
            call interpolate_faces_(cube)
            call append_new_face_info_(self,cube)
            call set_to_(info(x,y,2)%element,cube%edge_vertex_index)  ! current slice edge info
            call reset_(cube)
            call set_n_pt_(cube,self%n_pt)
         end do
         end do
      end do
      self%volume = 0.50d0*(self%volume_min+self%volume_max)
      call destroy_(info)
      call destroy_(f)
      call shrink_columns_(self%point,self%n_pt)
      call shrink_columns_(self%point_gradient,self%n_pt)
      call shrink_(self%point_mean_curvature,self%n_pt)
      call shrink_(self%point_gaussian_curvature,self%n_pt)
      call shrink_columns_(self%face,self%n_face)
      call rotate_gradients_(self)

   end subroutine

   subroutine set_isosurface_info_arrays(self)
    type(isosurface_type) :: self
    ! Destroy the isosurface information and set the informations arrays to nice
    ! big sizes to begin with.

      self%n_pt   = 0
      self%n_face = 0
      call destroy_(self%point)
      call create_(self%point,3,10000)
      call destroy_(self%point_gradient)
      call create_(self%point_gradient,3,10000)
      call destroy_(self%point_mean_curvature)
      call create_(self%point_mean_curvature,10000)
      call destroy_(self%point_gaussian_curvature)
      call create_(self%point_gaussian_curvature,10000)
      call destroy_(self%face)
      call create_(self%face,3,20000)

   end subroutine

   subroutine set_default_cube(self,cube)
    type(isosurface_type) :: self
    ! Initialise marching "cube" to the settings required for the isosurface.
    ! NOTE: .iso_value, .del, .big_interior must all be set.
      type(marchingcube_type), intent(in) :: cube

      call set_defaults_(cube)
      call set_iso_value_(cube,self%iso_value)
      call set_side_length_(cube,self%del)
    !  cube.set_accuracy(.accuracy)
      call set_big_interior_(cube,self%big_interior)

   end subroutine

   subroutine update_4_slab(self,p,f,slice,func)
    type(isosurface_type) :: self
    ! Make or update a "slice" of an array "f", a 4-slab of "func" function
    ! values which are evaluated at the slab points "p".  The slab involves .grid
    ! points whose z values are in the range slice-1:slice+2.  The "p" points and
    ! the function values f(:,:,2:3) correspond to the bottom and top of the
    ! slice.  The other slabs f(:,:,1) and f(:,:,4) are used for finite
    ! differences to get vertex gradients.
      real(kind=kind(1.0d0)), dimension(:,:,:,:), intent(inout) :: p
      real(kind=kind(1.0d0)), dimension(:,:,:), intent(inout) :: f
      integer(kind=kind(1)) :: slice
      interface
         subroutine func(values,pt)
            real(kind=kind(1.0d0)), dimension(:), intent(out) :: values
            real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: pt
         end subroutine
      end interface
      integer(kind=kind(1)) :: fz,lz,n_pt, i,x,y,z
      real(kind=kind(1.0d0)), dimension(:), pointer :: values
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: pt

      call ensure_(tonto,slice>=2 .and. slice<=self%grid%n_z-2,"ISOSURFACE:update_4_slab ... slice out f range")
      call ensure_(tonto,size(p,1)==3,"ISOSURFACE:update_4_slab ... p must have dim1=3")
      call ensure_(tonto,size(p,2)==self%grid%n_x,"ISOSURFACE:update_4_slab ... wrong size, p")
      call ensure_(tonto,size(p,3)==self%grid%n_y,"ISOSURFACE:update_4_slab ... wrong size, p")
      call ensure_(tonto,size(p,4)==4,"ISOSURFACE:update_4_slab ... p must have 4 slabs")
      call ensure_(tonto,size(f,1)==self%grid%n_x,"ISOSURFACE:update_4_slab ... wrong size, f")
      call ensure_(tonto,size(f,2)==self%grid%n_y,"ISOSURFACE:update_4_slab ... wrong size, f")
      call ensure_(tonto,size(f,3)==4,"ISOSURFACE:update_4_slab ... f must have 4 slabs")
      if (slice == 2) then
          ! These are the first 4 slices
         fz = 1; lz = 4
      else
          ! In between: only calculate one slice
         p(:,:,:,1:3) = p(:,:,:,2:4)
         f(:,:,1:3) = f(:,:,2:4)
         fz = 4; lz = 4
      end if
       ! Now calculate the slab between the right limits
      n_pt = self%grid%n_x*self%grid%n_y*(lz-fz+1)
      call create_(values,n_pt)
      call create_(pt,n_pt,3)
      call make_points_(self%grid,pt,1,self%grid%n_x,1,self%grid%n_y,slice-2+fz,slice-2+lz)
      call func(values,pt)
      call set_to_(f(:,:,fz:lz),values)
      i = 0
      do z = fz,lz
      do y = 1,self%grid%n_y
      do x = 1,self%grid%n_x
         i = i + 1
         p(:,x,y,z) = pt(i,:)
        ! write(*,*) "i =",i,"pt =",pt(i,:),"val=",values(i)
      end do
      end do
      end do
      call destroy_(pt)
      call destroy_(values)

   end subroutine

   subroutine append_new_face_info(self,cube)
    type(isosurface_type) :: self
    ! Generate the isosurface using the marching cube algorithm.
      type(marchingcube_type) :: cube
      integer(kind=kind(1)) :: n_pt,n_face,n_col

      n_pt   = no_of_active_edges_(cube)
      n_face = cube%n_triangle
       ! Expand info arrays if required
      n_col  = size(self%point,2)
      if ((self%n_pt+n_pt)>n_col) then
         call expand_columns_(self%point,2*n_col)
         call expand_columns_(self%point_gradient,2*n_col)
         call expand_(self%point_mean_curvature,2*n_col)
         call expand_(self%point_gaussian_curvature,2*n_col)
      end if
      n_col  = size(self%face,2)
      if ((self%n_face+n_face)>n_col) then
         call expand_columns_(self%face,2*n_col)
      end if
       ! Store isosurface info.
      call get_edge_vertex_positions_(cube,self%point(:,self%n_pt+1:self%n_pt+n_pt))
      call get_edge_vertex_gradients_(cube,self%point_gradient(:,self%n_pt+1:self%n_pt+n_pt))
      call get_edge_mean_curvatures_(cube,self%point_mean_curvature(self%n_pt+1:self%n_pt+n_pt))
      call get_edge_gaussian_curvatures_(cube,self%point_gaussian_curvature(self%n_pt+1:self%n_pt+n_pt))
      call get_triangle_vertex_indices_(cube,self%face(:,self%n_face+1:self%n_face+n_face))
      self%n_pt   = self%n_pt + n_pt
      self%n_face = self%n_face + n_face

   end subroutine

   subroutine rotate_gradients(self)
    type(isosurface_type) :: self
    ! Rotate the gradients by the box axes. This needs to be done because the box
    ! axes may not be the same as the natural x-y-z axes.
      real(kind=kind(1.0d0)), dimension(3,3) :: axes
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: old

      axes(:,1) = self%grid%x_axis
      axes(:,2) = self%grid%y_axis
      axes(:,3) = self%grid%z_axis
      call create_(old,3,self%n_pt)
      old = self%point_gradient
      self%point_gradient = matmul(axes,old)
      call destroy_(old)

   end subroutine

   subroutine recursively_cubify(self,func)
    type(isosurface_type) :: self
    ! Generate the isosurface using the recursive marching cube algorithm.
      interface
         subroutine func(values,pt)
            real(kind=kind(1.0d0)), dimension(:), intent(out) :: values
            real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: pt
         end subroutine
      end interface
      type(marchingcube_type) :: parent_cube
      real(kind=kind(1.0d0)), dimension(3,0:1,0:1,0:1) :: p2
      real(kind=kind(1.0d0)), dimension(0:1,0:1,0:1) :: f2

   call ensure_(tonto,are_all_equal_(self%grid%width),"ISOSURFACE:recursively_cubify ... grid widths must be all equal")
      call set_isosurface_info_arrays_(self)
      call set_default_cube_(self,parent_cube)
      self%volume_min = 0.0d0
      self%volume_max = 0.0d0
      self%n_skip = 0
      self%level = 0
      call create_(self%hash,256,3,12)
      call set_reverse_search_(self%hash,.true.)
      self%x = 0; self%y = 0; self%z = 0
      call make_cube_of_points_(self%grid,p2,0,1,self%del,0,0,0)
      call make_2_cube_of_values_(self,f2,func,p2)
      call set_vertex_info_(parent_cube,p2,f2)
       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      call divide_(self,func,parent_cube)  ! <<< do the work here
       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      call shrink_columns_(self%point,self%n_pt)
      call shrink_columns_(self%point_gradient,self%n_pt)
      call shrink_(self%point_mean_curvature,self%n_pt)
      call shrink_(self%point_gaussian_curvature,self%n_pt)
      call shrink_columns_(self%face,self%n_face)
      call rotate_gradients_(self)
      call destroy_(self%hash)
      self%volume = 0.50d0*(self%volume_min+self%volume_max)

   end subroutine

   function no_of_divisions(self,side_length) result(res)
    type(isosurface_type) :: self
    ! Return the number of binary divisions before the .grid side length
    ! becomes less than a given "side_length".
      real(kind=kind(1.0d0)), intent(in) :: side_length
      integer(kind=kind(1)) :: res
      real(kind=kind(1.0d0)) :: width
      integer(kind=kind(1)) :: i

   call ensure_(tonto,are_all_equal_(self%grid%width),"ISOSURFACE:no_of_divisions ... grid is not a cubical volume")
      width = self%grid%width(1)
      i = 0
      do
         i = i + 1
         width = width/2.0d0
         if (width>side_length) cycle
         res = i
         exit
      end do

   end function

   recursive subroutine divide(self,func,parent_cube)
    type(isosurface_type) :: self
    ! Generate the isosurface for "func" using a recursive marching cube
    ! algorithm. "parent_cube" is the enclosing parent marching cube from which
    ! the current cube was generated by a bisection method. We keep the
    ! "parent_cube" to save on function evaluations on the corners.
      interface
         subroutine func(values,pt) ! The function whose isosurface we make
            real(kind=kind(1.0d0)), dimension(:), intent(out) :: values
            real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: pt
         end subroutine
      end interface
      type(marchingcube_type), intent(in) :: parent_cube ! Parent cube; importantly, has corner values
      type(marchingcube_type) :: cube ! One of the 8 child cube; becomes next parent
      type(marchingcube_type), dimension(8) :: kube ! The group of final 8 child "cubes"
      real(kind=kind(1.0d0)), dimension(3,0:2,0:2,0:2) :: p3 ! The points of all child "cubes"
      real(kind=kind(1.0d0)), dimension(3,0:4,0:4,0:4) :: p5 ! The interior and surrounds of the final 8-"kube"
      real(kind=kind(1.0d0)), dimension(0:2,0:2,0:2) :: f3 ! The interior values used to make all child "cubes"
      real(kind=kind(1.0d0)), dimension(0:4,0:4,0:4) :: f5 ! The interior and surrounds of the final 8-"kube"
      logical(kind=kind(.true.)), dimension(8) :: skip ! Tells if we can skip any of the final 8-"kubes"
      logical(kind=kind(.true.)), dimension(5,5,5) :: eval ! Tells which of the "p5" points need to be
                                              ! evaluated using "func".
      integer(kind=kind(1)), dimension(8) :: left = (/0,1,0,3,0,5,0,7/) ! The lexical index of the left-cube
      integer(kind=kind(1)), dimension(8) :: front = (/0,0,1,2,0,0,5,6/) ! The lexical index of the front-cube
      integer(kind=kind(1)), dimension(8) :: below = (/0,0,0,0,1,2,3,4/) ! The lexical index of the bottom cube
      integer(kind=kind(1)), dimension(8) :: ix,iy,iz
      integer(kind=kind(1)) :: bit,x,y,z,k, l,f,b
      real(kind=kind(1.0d0)) :: vol
     ! del_iso :: real(kind=kind(1.0d0))
      logical(kind=kind(.true.)) :: do_divide

      self%level = self%level + 1
      bit = self%final_level - self%level
      self%del = self%del/2.0d0
      vol = self%del**3
       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if (self%level<self%final_level) then  ! Divide into 8 cubes and do again
         call make_cube_of_points_(self%grid,p3,0,2,self%del,self%x,self%y,self%z)
         call make_3_cube_of_values_(self,f3,func,p3,parent_cube)
         call set_default_cube_(self,cube)
         do z = 0,1
         do y = 0,1
         do x = 0,1
            call set_vertex_info_(cube,p3(:,x:x+1,y:y+1,z:z+1), &
                                   f3(x:x+1,y:y+1,z:z+1))
            call set_case_info_(cube)
            if (is_inside_surface_(cube)) then
               self%volume_min = self%volume_min + vol
               self%volume_max = self%volume_max + vol
            end if
           ! del_iso = (0.50d0/.final_level)*(bit+1)
           ! del_iso = max(0.10d0,del_iso) ! should be adjustable ?
           ! do_divide = &
           !    .level < .scan_level .or. &
           !   (.level== .scan_level    .and. cube.is_nearly_on_surface(0.10d0)) .or. &
           !   (.level==(.scan_level+1) .and. cube.is_nearly_on_surface(0.05d0)) .or. &
           !   (.level >(.scan_level+1) .and. cube.is_on_surface)
           ! do_divide = (.level<.scan_level) .or. cube.is_nearly_on_surface_old(0.10d0)
            do_divide = (self%level<self%scan_level) .or. is_nearly_on_surface_(cube,1.50d0)
            if (do_divide) then
               self%x = ibclr(self%x,bit); if (x==1) self%x = ibset(self%x,bit)  ! Set cube coords
               self%y = ibclr(self%y,bit); if (y==1) self%y = ibset(self%y,bit)
               self%z = ibclr(self%z,bit); if (z==1) self%z = ibset(self%z,bit)
               call divide_(self,func,cube)                                ! <<<<<< recursive here
               self%x = ibclr(self%x,bit)                                ! Clear the cube coords
               self%y = ibclr(self%y,bit)
               self%z = ibclr(self%z,bit)
            else
               l = 2**bit - 1
               self%n_skip = self%n_skip + l*l*l + 3*l*l
            end if
         end do
         end do
         end do
       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      else                      ! Divide by 8 the last time, and triangulate
         call make_cube_of_points_(self%grid,p5,-1,3,self%del,self%x,self%y,self%z)
         f5 = 0.0d0
         call make_3_cube_of_values_(self,f5(1:3,1:3,1:3),func,p5(:,1:3,1:3,1:3),parent_cube)
         call set_default_cube_(self,cube)
         eval = .false.           ! First evaluate only the needed function points
         skip = .true.
         ix = 0; iy = 0; iz = 0
         k = 0
         do z = 0,1
         do y = 0,1
         do x = 0,1
            k = k + 1
            call set_vertex_info_(cube,p5(:,x+1:x+2,y+1:y+2,z+1:z+2), &
                                   f5(x+1:x+2,y+1:y+2,z+1:z+2))
            call set_case_info_(cube)
            if (is_inside_surface_(cube))      self%volume_min = self%volume_min + vol
            if (.not. is_outside_surface_(cube)) self%volume_max = self%volume_max + vol
            if (.not. is_on_surface_(cube)) cycle
            self%x = self%x + x                                       ! Set cube coords
            self%y = self%y + y
            self%z = self%z + z
            skip(k) = .false.
            cube%skip_bit_string = 0
            call set_triangulation_info_(cube)
           ! if (x>0 .or. .x>0) cube.set_left_skip_bit_string
            if (has_left_face_on_surface_(cube)) then
               if ( x>0 .or. &
                  (self%x>0 .and. has_key_(self%hash,(/self%x-1,self%y,self%z/),index=ix(k)))) &
                  call set_left_skip_bit_string_(cube)               ! Previous left cube exists
            end if
           ! if (y>0 .or. .y>0) cube.set_front_skip_bit_string
            if (has_front_face_on_surface_(cube)) then
               if ( y>0 .or. &
                  (self%y>0 .and. has_key_(self%hash,(/self%x,self%y-1,self%z/),index=iy(k)))) &
                  call set_front_skip_bit_string_(cube)              ! Previous front cube exists
            end if
           ! if (z>0 .or. .z>0) cube.set_below_skip_bit_string
            if (has_lower_face_on_surface_(cube)) then
               if ( z>0 .or. &
                  (self%z>0 .and. has_key_(self%hash,(/self%x,self%y,self%z-1/),index=iz(k)))) &
                  call set_below_skip_bit_string_(cube)              ! Previous lower cube exists
            end if
            call set_cube_bit_string_(cube)
            call set_hessian_eval_array_(cube,eval,x,y,z)           ! "eval" has the needed points
            kube(k) = cube                                    ! store for later ...
            self%x = self%x - x                                       ! Clear the cube coords
            self%y = self%y - y
            self%z = self%z - z
         end do
         end do
         end do
         eval(2:4,2:4,2:4) = .false.                            ! <<< Evaluate only the
         call make_5_cube_of_values_(self,f5,func,p5,eval)              ! <<< required points .....
         k = 0
         do z = 0,1             ! Now to the actual marching cube triangle interpolation
         do y = 0,1
         do x = 0,1
            k = k + 1
            if (skip(k)) cycle
            cube = kube(k)
            call set_hessian_info_(cube,f5(x:x+3,y:y+3,z:z+3))
            self%x = self%x + x                                       ! Set cube coords
            self%y = self%y + y
            self%z = self%z + z
             ! Use previously triangulated cubes if possible
            l = left(k); f = front(k); b = below(k)
            if (has_left_face_on_surface_(cube)) then
               if      ( x>0)    then; call set_left_info_(cube,kube(l)%edge_vertex_index)
               else if (ix(k)>0) then; call set_left_info_(cube,value_for_item_(self%hash,ix(k)))
           !    else if (.x>0)    then; cube.set_left_info(.hash.value_for_key([.x-1,.y,.z]))
               end if
            end if
            if (has_front_face_on_surface_(cube)) then
               if      ( y>0)    then; call set_front_info_(cube,kube(f)%edge_vertex_index)
               else if (iy(k)>0) then; call set_front_info_(cube,value_for_item_(self%hash,iy(k)))
           !    else if (.y>0)    then; cube.set_front_info(.hash.value_for_key([.x,.y-1,.z]))
               end if
            end if
            if (has_lower_face_on_surface_(cube)) then
               if      ( z>0)    then; call set_below_info_(cube,kube(b)%edge_vertex_index)
               else if (iz(k)>0) then; call set_below_info_(cube,value_for_item_(self%hash,iz(k)))
           !    else if (.z>0)    then; cube.set_below_info(.hash.value_for_key([.x,.y,.z-1]))
               end if
            end if
            call set_n_pt_(cube,self%n_pt)                              ! All known edge vertices resolved
            call interpolate_faces_(cube)                            ! Interpolate the rest ...
            call append_new_face_info_(self,cube)
            call append_pair_(self%hash,(/self%x,self%y,self%z/),cube%edge_vertex_index)
            kube(k) = cube
            self%x = self%x - x                                       ! Clear the cube coords
            self%y = self%y - y
            self%z = self%z - z
         end do
         end do
         end do
      end if
      self%del = 2.0d0*self%del
      self%level = self%level - 1

   end subroutine

   subroutine make_2_cube_of_values(self,f,func,p)
    type(isosurface_type) :: self
    ! Evaluate a cube of values "f", with two points per side (starting at point
    ! 0). The values in "f" are those of the "func" function evaluated at
    ! positions "p" which are supposed to be the cube corners.
      real(kind=kind(1.0d0)), dimension(0:1,0:1,0:1) :: f
      real(kind=kind(1.0d0)), dimension(3,0:1,0:1,0:1) :: p
      interface
         subroutine func(values,pt)
            real(kind=kind(1.0d0)), dimension(:), intent(out) :: values
            real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: pt
         end subroutine
      end interface
      real(kind=kind(1.0d0)), dimension(8) :: values

      call func(values,transpose(reshape(p,(/3,8/))))  ! Evaluate the function at pt
      f = reshape(values,(/2,2,2/))                    ! Put the values into a cube

   end subroutine

   subroutine make_3_cube_of_values(self,f,func,p,parent_cube)
    type(isosurface_type) :: self
    ! Evaluate a cube of values "f", with three points per side (starting at
    ! point 0). The values in "f" are those of the "func" function evaluated at
    ! cube point positions "p". The cube points are assumed to be ordered along
    ! firstly the x-grid axis, then y-axis, then z-axis. The corner positions are
    ! not evaluated but taken from the "parent_cube" corner vertex values.
      real(kind=kind(1.0d0)), dimension(0:2,0:2,0:2), intent(out) :: f
      real(kind=kind(1.0d0)), dimension(3,0:2,0:2,0:2), intent(in) :: p
      type(marchingcube_type), intent(in) :: parent_cube
      interface
         subroutine func(values,pt)
            real(kind=kind(1.0d0)), dimension(:), intent(out) :: values
            real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: pt
         end subroutine
      end interface
      real(kind=kind(1.0d0)), dimension(27) :: values
      real(kind=kind(1.0d0)), dimension(19) :: vals
      real(kind=kind(1.0d0)), dimension(3,27) :: q
      real(kind=kind(1.0d0)), dimension(19,3) :: pt
      integer(kind=kind(1)), dimension(19) :: edge = (/2,4,5,6,8,10,11,12,13,14,15,16,17,18,20,22,23,24,26/)

      q = reshape(p,(/3,27/))
      pt = transpose(q(:,edge))                  ! Skip edge points
      call func(vals,pt)                         ! Evaluate the function at pt
      values(edge) = vals                        ! Store values in full 3^3 array
      f = reshape(values,(/3,3,3/))                ! Put the values into a cube
      f(0,0,0) = parent_cube%value_at_vertex(0)  ! Get the parent cube values
      f(2,0,0) = parent_cube%value_at_vertex(1)
      f(2,2,0) = parent_cube%value_at_vertex(2)
      f(0,2,0) = parent_cube%value_at_vertex(3)
      f(0,0,2) = parent_cube%value_at_vertex(4)
      f(2,0,2) = parent_cube%value_at_vertex(5)
      f(2,2,2) = parent_cube%value_at_vertex(6)
      f(0,2,2) = parent_cube%value_at_vertex(7)

   end subroutine

   subroutine make_5_cube_of_values(self,f,func,p,eval)
    type(isosurface_type) :: self
    ! Evaluate a 5 cube of values "f", with five points per side (starting at
    ! point 0). The values in "f" are those of the "func" function evaluated at
    ! cube point positions "p", except that only the points corresponding to the
    ! mask "eval" being .true. are evaluated.
      real(kind=kind(1.0d0)), dimension(0:4,0:4,0:4) :: f
      real(kind=kind(1.0d0)), dimension(3,0:4,0:4,0:4) :: p
      logical(kind=kind(.true.)), dimension(5,5,5) :: eval
      interface
         subroutine func(values,pt)
            real(kind=kind(1.0d0)), dimension(:), intent(out) :: values
            real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: pt
         end subroutine
      end interface
      real(kind=kind(1.0d0)), dimension(125) :: values
      real(kind=kind(1.0d0)), dimension(125,3) :: pt
      integer(kind=kind(1)) :: n_pt

      n_pt = count(eval)
      pt(1:n_pt,:) = &
         reshape( &
            pack( &
               transpose(reshape(p,(/3,125/))), &
               spread(reshape(eval,(/125/)),dim=2,ncopies=3)), &
            (/n_pt,3/))
       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      call func(values(1:n_pt),pt(1:n_pt,:))
       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      f = unpack(values,eval,f)

   end subroutine

!  *****************
!  Tesselate methods
!  *****************

!   tesselate(func) ::: leaky
!   ! Obtain the tesselated isosurface for the function "func".
!   ! This is the main routine of the module.
!      interface
!         func(p) result (res)
!            p :: real(kind=kind(1.0d0)), dimension(3)
!            res :: real(kind=kind(1.0d0))
!         end
!      end
!   call ensure_(tonto,.point.created,"no points array")
!   call ensure_(tonto,.shift.created,"no points array")
!   call ensure_(tonto,.face.created,"no face array")
!      .initialise(func)
!      iter = 0
!      do
!         iter = iter + 1
!         stdout.show("-------->iter =",iter)
!         if (.smooth) exit
!         .add_vertices(func)
!      end
!   end
!
!   initialise(func)
!   ! Find the initial isovalue points for the isosurface
!      interface
!         func(p) result (res)
!            p :: real(kind=kind(1.0d0)), dimension(3)
!            res :: real(kind=kind(1.0d0))
!         end
!      end
!      n :: integer(kind=kind(1))
!      p :: real(kind=kind(1.0d0)), dimension(3)
!      do n = 1,.n_pt
!         p = .point(:,n)
!         func.find_isopoint(p,p,.iso_value,.accuracy)
!         .point(:,n) = p
!      end
!      .n_skip = 0
!      .make_adjoining_data
!   end
!
!   make_adjoining_data ::: leaky
!   ! Make the initial (?) list of adjoining faces and adjoining edges
!      n,m,e,d :: integer(kind=kind(1))
!      w,v :: integer(kind=kind(1)), dimension(3)
!      matched :: INTVEC*
!   call ensure_(tonto,.face.created,"no face array")
!      .adjoining_face.destroy
!      .adjoining_face.create(3,.n_face)
!      .adjoining_face = 0
!      .adjoining_edge.destroy
!      .adjoining_edge.create(3,.n_face)
!      .adjoining_edge = 0
!      matched.create(.n_face)
!      matched = 0
!      do n = .n_skip+1,.n_face                     ! Loop over faces
!      w = .face(:,n)
!       ! if (iter==6 .and. n<=48) then
!       !    stdout.show("--->w =",w)
!       ! end
!      do m = .n_skip+1,.n_face
!         if (matched(m)==3 .or. m==n) cycle
!         v = .face(:,m)
!       ! if (iter==6 .and. n<=48) then
!       !    stdout.show("--->v =",v)
!       ! end
!         e = 0
!         if     ((any(v==w(1)) .and. any(v==w(2)))) then; e = 1
!         else if((any(v==w(2)) .and. any(v==w(3)))) then; e = 2
!         else if((any(v==w(3)) .and. any(v==w(1)))) then; e = 3
!         end
!         if (e/=0) then
!            matched(m) = matched(m) + 1
!            .adjoining_face(e,n) = m
!            if     (all(v(3)/=w)) then; d = 1
!            else if(all(v(1)/=w)) then; d = 2
!            else if(all(v(2)/=w)) then; d = 3
!            end
!            .adjoining_edge(e,n) = d
!       ! if (iter==6) then
!       !    stdout.text("---")
!       !    stdout.show("n =",n)
!       !    stdout.show("m =",m)
!       !    stdout.show("e =",e)
!       !    stdout.show("d =",d)
!       ! end
!         end
!      end
!      end
!      matched.destroy
!   end
!
!   add_vertices(func) ::: leaky
!   ! Add vertices to the edges of every face which needs it.
!      interface
!         func(p) result (res)
!            p :: real(kind=kind(1.0d0)), dimension(3)
!            res :: real(kind=kind(1.0d0))
!         end
!      end
!      n_ok,n_new,n_pt,n_face, f,n,e,e1,e2,e3,m,d,d1,d2,c1,c2,t,ip :: integer(kind=kind(1))
!      ic,in :: integer(kind=kind(1)), dimension(3)
!      p,p0,normal :: real(kind=kind(1.0d0)), dimension(3)
!      face,adjoining_face,adjoining_edge,child,child_face :: INTMAT*
!      new_ok_face :: INTVEC*
!      edge :: integer(kind=kind(1)), dimension(3,3) = reshape([1,2,3,2,3,1,3,1,2],[3,3])
!      x1,x2 :: real(kind=kind(1.0d0))
!      no :: integer(kind=kind(1)) = 0
!   call ensure_(tonto,.point.created,"no points array")
!   call ensure_(tonto,.shift.created,"no points array")
!   call ensure_(tonto,.face.created,"no face array")
!      n_ok = count(.ok)
!   stdout.show("n_ok =",n_ok)
!      n_new = .n_face - n_ok
!      n_pt  = .n_pt + 3*n_new
!      n_face = n_ok + 7*n_new
!      .point.expand_columns(n_pt)         ! May not be this many
!      .shift.expand(n_pt)
!      ! The objective is to create new versions of these from the old
!      face.create(3,n_face)
!      adjoining_face.create(3,n_face); adjoining_face = 0
!      adjoining_edge.create(3,n_face); adjoining_edge = 0
!      ! Copy all the ok faces with ok neighbours to start
!      new_ok_face.create(.n_face)
!      f = 0
!      do n = 1,.n_face                     ! Loop over good old faces
!         !!!!!!!!!!!!!!!!!!!!!
!         if (.not. .ok(n)) cycle
!         !!!!!!!!!!!!!!!!!!!!!
!         face(:,f+1) = .face(:,n)
!         f = f + 1
!         new_ok_face(n) = f
!      end
!      ! Make 3 new points on the all the faces which are not OK.
!      ! Define the child array which stores indices of new points
!      ! as a function of the old face and old edge indices.
!      ! Define the child_face array which stores the indices of
!      ! the new corner faces in terms of the old face index.
!      child.create(3,.n_face)
!      child = 0
!      child_face.create(3,.n_face)
!      child = 0
!      ip = .n_pt
!      do n = 1,.n_face                     ! Loop over bad old faces
!         !!!!!!!!!!!
!         if (.ok(n)) cycle
!         !!!!!!!!!!!
!         in = .face(:,n)                   ! Parent face indices
!         do e = 1,3                        ! Loop over three old edges
!            m = .adjoining_face(e,n)       ! faces which are not OK have m
!            d = .adjoining_edge(e,n)       ! edge for the adjoining face
!            e1 = edge(1,e)
!            e2 = edge(2,e)
!            e3 = edge(3,e)
!            if (.ok(m) .or. child(d,m)==0) then ! Add new child for edge
!               ip = ip + 1
!               ic(e) = ip                  ! the index of the child edge pt.
!               p = 0.50d0*(.point(:,.face(e1,n)) + .point(:,.face(e2,n)))
!               normal = .normal_for_face(n) + .normal_for_face(m)
!               normal.normalise
!               p0 = p                      ! This is the new parent
!               x1 = 0.0d0
!               x2 = min(.shift(e1),.shift(e2))
!                                           ! Find the isosurface point
!               func.find_isopoint(p,normal,.iso_value,.accuracy,x1,x2)
!               .point(:,ip) = p            ! and add it to the expanded list
!               .shift(ip) = (p-p0).norm    ! for deciding convergence
!               child(e,n) = ip
!            else                           ! Use existing child
!               ic(e) = child(d,m)
!               d1 = edge(1,d)              ! Fix adjoining faces/edges
!               d2 = edge(2,d)
!                                           ! Connect new to old
!               c2 = child_face(d2,m)
!               c1 = child_face(d1,m)
!               adjoining_face(1,f+e1) = c2
!               adjoining_edge(1,f+e1) = 3
!               adjoining_face(3,f+e2) = c1
!               adjoining_edge(3,f+e2) = 1
!                                           ! Connect old to new
!               adjoining_face(3,c2) = f+e1
!               adjoining_edge(3,c2) = 1
!               adjoining_face(1,c1) = f+e2
!               adjoining_edge(1,c1) = 3
!            end
!         end
!                                           ! Assign 4 new faces
!         do e = 1,3                        ! Loop over three old edges
!            e1 = edge(1,e)
!            e3 = edge(3,e)
!            face(:,f+e) = [in(e1),ic(e1),ic(e3)]
!            adjoining_face(2 ,f+e) = f + 4 ! Connect to central face
!            adjoining_edge(2 ,f+e) = e3
!            adjoining_face(e3,f+4) = f + e ! Connect central face
!            adjoining_edge(e3,f+4) = 2
!            child_face(e,n) = f+e
!         end
!         face(:,f+4) = [ic(1),ic(2),ic(3)] ! The central face
!         t = 0
!         do e = 1,3                        ! See if we need to tie-off
!            m = .adjoining_face(e,n)
!            d = .adjoining_edge(e,n)       ! edge for the adjoining face
!            if (.not. .ok(m)) cycle          ! Tie-off if get past this
!            t = t + 1
!            e1 = edge(1,e)
!            e2 = edge(2,e)
!            face(:,f+4+t) = [in(e1),in(e2),ic(e1)]
!            adjoining_face(1,f+4+t) = new_ok_face(m)
!            adjoining_edge(1,f+4+t) = d
!            adjoining_face(2,f+4+t) = f + e2
!            adjoining_edge(2,f+4+t) = 3
!            adjoining_face(3,f+4+t) = f + e1
!            adjoining_edge(3,f+4+t) = 1
!         end
! ! if (any(ic<1) .or. any(ic>n_pt) .or. (any(in<1) .or. any(in>n_pt)) then
! !   stdout.show("ic =",ic)
! !   stdout.show("in =",in)
! !   stdout.show("ip =",ip)
! !   stdout.show("n  =",n)
! !   stdout.show("m  =",m)
! !   stdout.show("t  =",t)
! !   stdout.show("f  =",f+4+t)
! !   do e = f+1,f+4+t
! !   stdout.show("face(:,"//e.to_str.trim//")  =",face(:,e))
! !   end
! !   stdout.show("n_pt =",n_pt)
! !   stdout.show("adjoining_face(n) =",.adjoining_face(:,n))
! !   stdout.show("adjoining_edge(n) =",.adjoining_edge(:,n))
! ! end
!         f = f + 4 + t
!      end
!      child_face.destroy
!      child.destroy
!      new_ok_face.destroy
!      ! Shrink storage
!      .point.shrink_columns(ip)
!      .shift.shrink(ip)
!      adjoining_edge.shrink_columns(f)
!      adjoining_face.shrink_columns(f)
!      face.shrink_columns(f)
!      ! Destroy old faces and replace with new
!      .adjoining_edge.destroy; .adjoining_edge => adjoining_edge
!      .adjoining_face.destroy; .adjoining_face => adjoining_face
!      .face.destroy;           .face => face
!      .n_pt = .point.dim2
!      .n_face = face.dim2
!      no = no + 1
!    ! file.create("data"//no.to_str.trim)
!    ! file.open(for="write")
!    ! .put(output=file)
!    ! file.close
!    ! file.destroy
!   end
!
!   sort_faces
!   ! This routine sorts through the list of .faces from position .n_skip and
!   ! places those which acceptably smooth at the start of the faces array.
!   ! The number of skipped faces .n_skip is incremented.
!      n,s :: integer(kind=kind(1))
!   call ensure_(tonto,.n_face>=.n_skip,"inconsistent sizes")
!      s = 0
!      do n = .n_skip+1,.n_face
!         if (.has_smooth_face(n)) then
!            s = s + 1
!            .face.swap_columns(.n_skip+s,n)
!         end
!      end
!      .n_skip = .n_skip + s
!   end
!
!   normal_for_face(n) result (res)
!   ! Returns the normal for face "n"
!      n :: integer(kind=kind(1))
!      res :: real(kind=kind(1.0d0)), dimension(3)
!      a,b,c :: real(kind=kind(1.0d0)), dimension(3)
!   call ensure_(tonto,.point.created,"no points array")
!   call ensure_(tonto,.face.created,"no face array")
!   call ensure_(tonto,n<=.face.dim2,"n too large")
!      a = .point(:,.face(1,n))
!      b = .point(:,.face(2,n))
!      c = .point(:,.face(3,n))
!      res = (b-a).cross(c-b)
!      res.normalise
!   end

!   smooth result (res) ::: leaky
!   ! Returns .true. if the isosurface is evenly covered by enough triangles.
!      res :: logical(kind=kind(.true.))
!      n :: integer(kind=kind(1))
!      f :: integer(kind=kind(1)), dimension(4)
!   call ensure_(tonto,.n_face>0,"n_face must be non-zero")
!      .make_adjoining_data
!      .ok.destroy
!      .ok.create(.n_face)
!      do n = 1,.n_face
!         f(1)   = n
!         f(2:4) = .adjoining_face(:,n)
!         if (.has_smooth_faces(f)) then; .ok(n) = .true.
!         else;                           .ok(n) = .false.
!         end
!      end
!      res = all(.ok)
!   end
!
!   has_smooth_faces(n) result (res)
!   ! Returns .true. if the isosurface has all smooth faces "n"
!      n :: INTVEC
!      res :: logical(kind=kind(.true.))
!      i :: integer(kind=kind(1))
!      res = .true.
!      do i = 1,n.dim
!         res = res .and. .has_smooth_face(n(i))
!         if (.not. res) exit
!      end
!   end
!
!   has_smooth_face(n) result (res)
!   ! Returns .true. if the isosurface has a smooth face "n"
!      n :: integer(kind=kind(1))
!      res :: logical(kind=kind(.true.))
!      if (n==0) then ! assume a face with zero index is smooth
!         res = .true.
!      else
!         res = .has_small_face(n) .and. .has_flat_face(n)
!      end
!   end
!
!   has_small_face(n) result (res)
!   ! Returns .true. if the isosurface has a small face "n"
!      n :: integer(kind=kind(1))
!      res :: logical(kind=kind(.true.))
!      a,b,c :: real(kind=kind(1.0d0)), dimension(3)
!   call ensure_(tonto,.point.created,"no points array")
!   call ensure_(tonto,.face.created,"no face array")
!   call ensure_(tonto,n<=.face.dim2,"n too large")
! ! call ensure_(tonto,all(.face(:,n)<.point.dim2),"face indices too large")
!      if (any(.face(:,n)>.point.dim2)) then
!         stdout.show("n_pt  =",.point.dim2)
!         stdout.show("n_pt  =",.n_pt)
!         stdout.show("faces =",.face(:,n))
!      end
!      if (n==0) then
!         res = .true.
!      else
!      a = .point(:,.face(1,n))
!      b = .point(:,.face(2,n))
!      c = .point(:,.face(3,n))
!      res =  (b-a).norm < .smallness &
!         .and. (c-b).norm < .smallness &
!         .and. (a-c).norm < .smallness
!      end
!   end
!
!   has_flat_face(n) result (res)
!   ! Returns .true. if the isosurface has a flat face "n".
!      n :: integer(kind=kind(1))
!      res :: logical(kind=kind(.true.))
!      a,b,c :: real(kind=kind(1.0d0)), dimension(3)
!   call ensure_(tonto,.shift.created,"no shift array")
!   call ensure_(tonto,.face.created,"no face array")
!   call ensure_(tonto,n<=.face.dim2,"n too large")
!      if (n==0) then
!         res = .true.
!      else
!      res = min(.shift(.face(1,n)), &
!                .shift(.face(2,n)), &
!                .shift(.face(3,n))) < .flatness
!      end
!   end
!
!   isointerval_for_face(n) result (res)
!   ! Returns an estimate of the isosurface interval for face "n" as the
!   ! minimum of the deviations of the points from their parents.
!      n :: integer(kind=kind(1))
!      res :: real(kind=kind(1.0d0))
!   call ensure_(tonto,.shift.created,"no shift array")
!   call ensure_(tonto,.face.created,"no face array")
!   call ensure_(tonto,n<=.face.dim2,"n too large")
!      res = min(.shift(.face(1,n)), &
!                .shift(.face(2,n)), &
!                .shift(.face(3,n)))
!   end

   function average_face_normal(self,n) result(res)
    type(isosurface_type) :: self
    ! Returns the average normal for face "n"
      integer(kind=kind(1)) :: n
      real(kind=kind(1.0d0)), dimension(3) :: res
      real(kind=kind(1.0d0)), dimension(3) :: a,b,c

      call ensure_(tonto,associated(self%point_gradient),"ISOSURFACE:average_face_normal ... no point_gradient array")
      call ensure_(tonto,associated(self%face),"ISOSURFACE:average_face_normal ... no face array")
      call ensure_(tonto,n<=size(self%face,2),"ISOSURFACE:average_face_normal ... n too large")
      a = self%point_gradient(:,self%face(1,n)); call normalise_(a)
      b = self%point_gradient(:,self%face(2,n)); call normalise_(b)
      c = self%point_gradient(:,self%face(3,n)); call normalise_(c)
      res = (a+b+c)/3.0d0
      call normalise_(res)

   end function

   function average_face_gradient(self,n) result(res)
    type(isosurface_type) :: self
    ! Returns the average gradient for face "n"
      integer(kind=kind(1)) :: n
      real(kind=kind(1.0d0)), dimension(3) :: res
      real(kind=kind(1.0d0)), dimension(3) :: a,b,c

      call ensure_(tonto,associated(self%point_gradient),"ISOSURFACE:average_face_gradient ... no point_gradient array")
      call ensure_(tonto,associated(self%face),"ISOSURFACE:average_face_gradient ... no face array")
      call ensure_(tonto,n<=size(self%face,2),"ISOSURFACE:average_face_gradient ... n too large")
      a = self%point_gradient(:,self%face(1,n))
      b = self%point_gradient(:,self%face(2,n))
      c = self%point_gradient(:,self%face(3,n))
      res = (a+b+c)/3.0d0

   end function

!*******************************************************************************
!
! Plotting Methods
!
!*******************************************************************************

   subroutine plot_function(self,func)
    type(isosurface_type) :: self
    ! Generate the isosurface using the marching cube algorithm.
      interface
         subroutine func(values,pt)
            real(kind=kind(1.0d0)), dimension(:), intent(out) :: values
            real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: pt
         end subroutine
      end interface

      call create_(self%surface_property_values,size(self%point,2))
      call func(self%surface_property_values,transpose(self%point))

   end subroutine

!  *****
!  Areas
!  *****

   subroutine put_connected_area(self)
    type(isosurface_type) :: self
    ! Put the connected area for a given ".surface_property" out.
      real(kind=kind(1.0d0)) :: area

      call ensure_(tonto,property_bounds_set_(self),"ISOSURFACE:put_connected_area ... unacceptable property bounds")
      call ensure_(tonto,self%surface_property_lower_bound<self%surface_property_upper_bound,"ISOSURFACE:put_connected_area .&
&.. unacceptable property bounds")
      call ensure_(tonto,surface_point_set_(self),"ISOSURFACE:put_connected_area ... surface_point not set")
      if      (self%surface_property_lower_bound/=179.55d0 .and. self%surface_property_upper_bound/=179.55d0) then
         area = connected_property_area_(self,self%surface_property,self%surface_property_lower_bound,self%surface_property_u&
&pper_bound)
      else if (self%surface_property_lower_bound/=179.55d0) then
         area = connected_property_area_(self,self%surface_property,lower=self%surface_property_lower_bound)
      else if (self%surface_property_upper_bound/=179.55d0) then
         area = connected_property_area_(self,self%surface_property,upper=self%surface_property_lower_bound)
      end if
      call flush_(stdout)
      call text_(stdout,"ISOSURFACE Property area")
      call flush_(stdout)
      call show_(stdout,"Surface property               =",self%surface_property)
      call show_(stdout,"Surface point                  =",self%surface_point)
      call show_(stdout,"Index of nearest surface point =",index_of_nearest_point_(self))
      if (self%surface_property_lower_bound/=179.55d0) then
      call show_(stdout,"Property lower bound           =",self%surface_property_lower_bound)
      end if
      if (self%surface_property_upper_bound/=179.55d0) then
      call show_(stdout,"Property upper bound           =",self%surface_property_upper_bound)
      end if
      call show_(stdout,"Connected area                 =",area)

   end subroutine

   function index_of_nearest_point(self) result(res)
    type(isosurface_type) :: self
    ! Returns the index of the nearest isosurface point to .surface_point.
      integer(kind=kind(1)) :: res

   call ensure_(tonto,self%n_pt>0,"ISOSURFACE:index_of_nearest_point ... there are no isosurface points")
      res = index_of_minimum_column_norm_(self%point,offset=self%surface_point)

   end function

   function connected_property_area(self,property,lower,upper) result(res)
    type(isosurface_type) :: self
    ! Returns the *connected* area of isosurface triangles where the "property"
    ! values of all vertices of a triangles connected to a "point" which lies on
    ! or near the surface is between "lower" and "upper", if present.
      character(128) :: property
      real(kind=kind(1.0d0)), optional :: lower,upper
      real(kind=kind(1.0d0)) :: res
      character(128) :: prop
      real(kind=kind(1.0d0)), dimension(:), pointer :: W

      call ensure_(tonto,self%n_pt>0,"ISOSURFACE:connected_property_area ... there are no isosurface points")
      call ensure_(tonto,present(lower) .or. present(upper),"ISOSURFACE:connected_property_area ... no bounds specified")
      prop = property
      call to_lower_case_(prop)
      select case (prop)
         case ("mean_curvature")
            res = connected_property_area_(self,self%point_mean_curvature,lower,upper)
         case ("gaussian_curvature")
            res = connected_property_area_(self,self%point_gaussian_curvature,lower,upper)
         case ("rms_curvature")
            call create_(W,self%n_pt)
            call get_vertex_RMS_curvature_(self,W)
            res = connected_property_area_(self,W,lower,upper)
            call destroy_(W)
         case ("curvedness")
            call create_(W,self%n_pt)
            call get_vertex_curvedness_(self,W)
            res = connected_property_area_(self,W,lower,upper)
            call destroy_(W)
         case ("shape_index")
            call create_(W,self%n_pt)
            call get_vertex_shape_index_(self,W)
            res = connected_property_area_(self,W,lower,upper)
            call destroy_(W)
      end select

   end function

   function connected_property_area_1(self,property,lower,upper) result(res)
    type(isosurface_type) :: self
    ! Returns the *connected* area of isosurface triangles where the "property"
    ! values of all vertices of a triangles connected to a "point" which lies on
    ! or near the surface is between "lower" and "upper", if present.
      real(kind=kind(1.0d0)), dimension(:) :: property
      real(kind=kind(1.0d0)), optional :: lower,upper
      real(kind=kind(1.0d0)) :: res
      integer(kind=kind(1)) :: ind
      integer(kind=kind(1)), dimension(2) :: loc

      call ensure_(tonto,self%n_pt>0,"ISOSURFACE:connected_property_area_1 ... there are no isosurface points")
      call ensure_(tonto,size(property)==self%n_pt,"ISOSURFACE:connected_property_area_1 ... wrong size, property array")
      call ensure_(tonto,present(lower) .or. present(upper),"ISOSURFACE:connected_property_area_1 ... no bounds specified")
      ind = index_of_nearest_point_(self)
      loc = minloc(self%face - ind)
      res = connected_property_area_(self,property,loc(2),lower,upper)

   end function

   function connected_property_area_2(self,property,ind,lower,upper) result(res)
    type(isosurface_type) :: self
    ! Returns the *connected* area of isosurface triangles where the "property"
    ! values of all vertices of triangles connected to a face with index "ind" on
    ! the surface is between "lower" and "upper", if present.
      real(kind=kind(1.0d0)), dimension(:) :: property
      integer(kind=kind(1)) :: ind
      real(kind=kind(1.0d0)), optional :: lower,upper
      real(kind=kind(1.0d0)) :: res
      integer(kind=kind(1)) :: n_vertex,i,v,p
      logical(kind=kind(.true.)), dimension(3) :: add_vertex
      integer(kind=kind(1)), dimension(:), pointer :: face,vertex

      call ensure_(tonto,self%n_pt>0,"ISOSURFACE:connected_property_area_2 ... there are no isosurface points")
      call ensure_(tonto,size(property)==self%n_pt,"ISOSURFACE:connected_property_area_2 ... wrong size, property array")
      call ensure_(tonto,present(lower) .or. present(upper),"ISOSURFACE:connected_property_area_2 ... no bounds specified")
      nullify(face)
      nullify(vertex)
      call append_(face,ind)
      call append_(vertex,self%face(:,ind))
      do           ! Loop over new vertices, get all triangle faces
         n_vertex = size(vertex)
         do i = 1,self%n_face
            if (any(i==face)) cycle      ! Ignore all faces already in "face" list
            if (all(vertex/=self%face(1,i)) .and. all(vertex/=self%face(1,i)) .and. all(vertex/=self%face(1,i))) cycle
            add_vertex = .false.           ! At least one vertex is connected to face "i"
            do v = 1,3  ! Are all the vertices of this face between property limits?
               p = self%face(v,i)
               if (present(lower)) then
               if (property(p)<lower) cycle
               end if
               if (present(upper)) then
               if (property(p)>upper) cycle
               end if
               add_vertex(v) = .true.
            end do
            if (all(add_vertex)) then    ! All vertices pass limits
               call append_(face,i)            ! Add the face and any new vertices
               call append_only_if_unique_(vertex,self%face(1,i))
               call append_only_if_unique_(vertex,self%face(2,i))
               call append_only_if_unique_(vertex,self%face(3,i))
            end if
         end do
         if (size(vertex)==n_vertex) exit  ! Exit if no more new vertices added
      end do
      res = total_area_(self,face)
      call destroy_(vertex)
      call destroy_(face)

   end function

   function total_area(self,faces) result(res)
    type(isosurface_type) :: self
    ! Returns the *total* area of the list of "faces".
      integer(kind=kind(1)), dimension(:) :: faces
      real(kind=kind(1.0d0)) :: res
      integer(kind=kind(1)) :: i

      call ensure_(tonto,size(faces)>0,"ISOSURFACE:total_area ... faces array, zero size")
      call ensure_(tonto,maxval(faces)<=self%n_face,"ISOSURFACE:total_area ... faces array, value too large")
      call ensure_(tonto,minval(faces)>0,"ISOSURFACE:total_area ... faces array, nonpositive value")
      call ensure_(tonto,self%n_face>0,"ISOSURFACE:total_area ... there are no isosurface points")
      res = 0.0d0
      do i = 1,size(faces)
         res = res + face_area_(self,faces(i))
      end do

   end function

   function face_area(self,face) result(res)
    type(isosurface_type) :: self
    ! Returns the face area for the face with index "face".
      integer(kind=kind(1)) :: face
      real(kind=kind(1.0d0)) :: res
      real(kind=kind(1.0d0)), dimension(3) :: a,b,c

      call ensure_(tonto,face>0,"ISOSURFACE:face_area ... face, nonpositive value")
      call ensure_(tonto,face<=self%n_face,"ISOSURFACE:face_area ... face, value too large")
      call ensure_(tonto,self%n_face>0,"ISOSURFACE:face_area ... there are no isosurface points")
      a = self%point(:,self%face(2,face)) - self%point(:,self%face(1,face))
      b = self%point(:,self%face(3,face)) - self%point(:,self%face(1,face))
      c = cross_(a,b)
      res = norm_(c)

   end function

!  ******************
!  Surface properties
!  ******************

   subroutine get_vertex_RMS_curvature(self,RMS)
    type(isosurface_type) :: self
    ! Get the "RMS" curvature values for each canonical point
      real(kind=kind(1.0d0)), dimension(:) :: RMS
      real(kind=kind(1.0d0)), dimension(:), pointer :: k1,k2

      call ensure_(tonto,self%n_pt>0,"ISOSURFACE:get_vertex_RMS_curvature ... no isosurface points")
      call ensure_(tonto,size(RMS)==self%n_pt,"ISOSURFACE:get_vertex_RMS_curvature ... wrong size, RMS")
      call create_(k1,self%n_pt)
      call create_(k2,self%n_pt)
      call get_principal_curvatures_(self,k1,k2)
      RMS = sqrt((k1*k1+k2*k2)/2.0d0)
      call destroy_(k2)
      call destroy_(k1)

   end subroutine

   subroutine get_vertex_curvedness(self,C)
    type(isosurface_type) :: self
    ! Get the list of Koenderinks curvedness values "C" for each canonical point
      real(kind=kind(1.0d0)), dimension(:) :: C
      real(kind=kind(1.0d0)), dimension(:), pointer :: k1,k2
      real(kind=kind(1.0d0)) :: fac

      call ensure_(tonto,self%n_pt>0,"ISOSURFACE:get_vertex_curvedness ... no isosurface points")
      call ensure_(tonto,size(C)==self%n_pt,"ISOSURFACE:get_vertex_curvedness ... wrong size, C")
      call create_(k1,self%n_pt)
      call create_(k2,self%n_pt)
      call get_principal_curvatures_(self,k1,k2)
      fac = 2.0d0/3.141592653589793d0
      C = fac*log(sqrt((k1*k1+k2*k2)/2.0d0))
      call destroy_(k2)
      call destroy_(k1)

   end subroutine

   subroutine get_vertex_shape_index(self,SI)
    type(isosurface_type) :: self
    ! Get the list of Koenderinks shape index values "SI" for each canonical point.
      real(kind=kind(1.0d0)), dimension(:) :: SI
      real(kind=kind(1.0d0)), dimension(:), pointer :: k1,k2
      real(kind=kind(1.0d0)) :: fac
      integer(kind=kind(1)) :: i

      call ensure_(tonto,self%n_pt>0,"ISOSURFACE:get_vertex_shape_index ... no isosurface points")
      call ensure_(tonto,size(SI)==self%n_pt,"ISOSURFACE:get_vertex_shape_index ... wrong size, SI")
      call create_(k1,self%n_pt)
      call create_(k2,self%n_pt)
      call get_principal_curvatures_(self,k1,k2)
      fac = -2.0d0/3.141592653589793d0
      do i = 1,self%n_pt
         if (k1(i)/=k2(i)) then
            SI(i) = fac*atan( (k1(i)+k2(i)) / (max(k1(i),k2(i))-min(k1(i),k2(i))) )
         else
            SI(i) = -sign(1.0d0,k1(i))
         end if
      end do
      call destroy_(k2)
      call destroy_(k1)

   end subroutine

   subroutine get_principal_curvatures(self,k1,k2)
    type(isosurface_type) :: self
    ! Get the principal curvatures "k1" and "k2" for each canonical point.
      real(kind=kind(1.0d0)), dimension(:), pointer :: k1,k2
      real(kind=kind(1.0d0)), dimension(:), pointer :: m,g

      call ensure_(tonto,self%n_pt>0,"ISOSURFACE:get_principal_curvatures ... no isosurface points")
      call ensure_(tonto,size(k1)==self%n_pt,"ISOSURFACE:get_principal_curvatures ... wrong size, k1")
      call ensure_(tonto,size(k2)==self%n_pt,"ISOSURFACE:get_principal_curvatures ... wrong size, k1")
      m => self%point_mean_curvature
      g => self%point_gaussian_curvature
      k1 = sqrt(m*m-g)
      k2 = -k1
      k1 = m + k1
      k2 = m + k2

   end subroutine

!  **************
!  Output methods
!  **************

   subroutine put(self)
    type(isosurface_type) :: self
    ! Put the isosurface data

      call flush_(stdout)
      call text_(stdout,"ISOSURFACE data:")
      call flush_(stdout)
      call show_(stdout,"Kind of surface          =",self%iso_kind)
      call show_(stdout,"Triangulation method     =",self%triangulation_method)
      call show_(stdout,"Iso value                =",self%iso_value)
      call show_(stdout,"No. of points            =",self%n_pt)
      call show_(stdout,"No. of faces             =",self%n_face)
      call show_(stdout,"Use interpolator?        =",self%use_interpolator)
      call show_(stdout,"Interior volume          =",self%volume)
      call show_(stdout,"Volume lower bound       =",self%volume_min)
      call show_(stdout,"Volume upper bound       =",self%volume_max)
      call show_(stdout,"Big interior?            =",self%big_interior)
      call show_(stdout,"Reverse surface normals? =",self%big_interior)
      call show_(stdout,"# of divisions           =",self%final_level)
      call show_(stdout,"# of scan divisions      =",self%scan_level)
      call show_(stdout,"# of func. evals skipped =",self%n_skip)
      call show_(stdout,"% skipped                =",(1.0d2*self%n_skip)/self%grid%n_pt)
      call put_grid_(self)

   end subroutine

   subroutine put_points(self)
    type(isosurface_type) :: self
    ! Put the canonically indexed list of vertices for the object

      call flush_(stdout)
      call show_(stdout,"begin vertices ",self%n_pt)
      call put_(stdout,self%point,order="column")
      call text_(stdout,"end vertices")

   end subroutine

   subroutine put_faces(self)
    type(isosurface_type) :: self
    ! Put the list of canonical indices for each triangular face
      integer(kind=kind(1)) :: f
      integer(kind=kind(1)), dimension(:,:), pointer :: face

      call create_(face,3,self%n_face)
      do f = 1,self%n_face
         face(:,f) = self%face(:,f) - 1
      end do
      call flush_(stdout)
      call show_(stdout,"begin indices ",self%n_face)
      call put_(stdout,face,order="column")
      call text_(stdout,"end indices")
      call destroy_(face)

   end subroutine

   subroutine put_vertex_gradients(self)
    type(isosurface_type) :: self
    ! Put the list of gradients for each canonical point

      call flush_(stdout)
      call show_(stdout,"begin vertex_normals ",self%n_pt)
      call put_(stdout,self%point_gradient,order="column")
      call text_(stdout,"end vertex_normals")

   end subroutine

   subroutine put_vertex_normals(self)
    type(isosurface_type) :: self
    ! Put the list of normals for each canonical point
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: normal
      real(kind=kind(1.0d0)), dimension(3) :: n
      integer(kind=kind(1)) :: i

      call ensure_(tonto,self%n_pt>0,"ISOSURFACE:put_vertex_normals ... no isosurface points")
      call create_(normal,3,self%n_pt)
      do i = 1,self%n_pt
         n = self%point_gradient(:,i)
         call normalise_(n)
         normal(:,i) = n
      end do
      call flush_(stdout)
      call show_(stdout,"begin vertex_normals ",self%n_pt)
      call put_(stdout,normal,order="column")
      call text_(stdout,"end vertex_normals")
      call destroy_(normal)

   end subroutine

   subroutine put_vertex_curvatures(self)
    type(isosurface_type) :: self
    ! Put out lists of vertex surface curvature properties.
      logical(kind=kind(.true.)) :: use_labels

      use_labels = stdout%use_labels
      call set_use_labels_(stdout,.false.)
      call flush_(stdout)
      call show_(stdout,"begin vertex_properties 5 ",self%n_pt)
      call put_vertex_mean_curvatures_(self)
      call put_vertex_gaussian_curvatures_(self)
      call put_vertex_RMS_curvature_(self)
      call put_vertex_curvedness_(self)
      call put_vertex_shape_index_(self)
      call text_(stdout,"end vertex_properties")
      call set_use_labels_(stdout,use_labels)

   end subroutine

   subroutine put_vertex_mean_curvatures(self)
    type(isosurface_type) :: self
    ! Put the list of mean curvatures for each canonical point

      call flush_(stdout)
      call text_(stdout,"begin mean_curvature Mean_Curvature")
      call put_(stdout,self%point_mean_curvature,"column")
      call text_(stdout,"end mean_curvature")

   end subroutine

   subroutine put_vertex_gaussian_curvatures(self)
    type(isosurface_type) :: self
    ! Put the list of gaussian curvatures for each canonical point

      call flush_(stdout)
      call text_(stdout,"begin gaussian_curvature Gaussian_Curvature")
      call put_(stdout,self%point_gaussian_curvature,"column")
      call text_(stdout,"end gaussian_curvature")

   end subroutine

   subroutine put_vertex_RMS_curvature(self)
    type(isosurface_type) :: self
    ! Put the list of RMS curvature values for each canonical point
      real(kind=kind(1.0d0)), dimension(:), pointer :: RMS

      call ensure_(tonto,self%n_pt>0,"ISOSURFACE:put_vertex_RMS_curvature ... no isosurface points")
      call create_(RMS,self%n_pt)
      call get_vertex_RMS_curvature_(self,RMS)
      call flush_(stdout)
      call text_(stdout,"begin RMS_curvature RMS_Curvature")
      call put_(stdout,RMS,"column")
      call text_(stdout,"end RMS_curvature")
      call destroy_(RMS)

   end subroutine

   subroutine put_vertex_curvedness(self)
    type(isosurface_type) :: self
    ! Get the list of Koenderink curvedness values for each canonical point
      real(kind=kind(1.0d0)), dimension(:), pointer :: C

      call ensure_(tonto,self%n_pt>0,"ISOSURFACE:put_vertex_curvedness ... no isosurface points")
      call create_(C,self%n_pt)
      call get_vertex_curvedness_(self,C)
      call flush_(stdout)
      call text_(stdout,"begin curvedness Curvedness")
      call put_(stdout,C,"column")
      call text_(stdout,"end curvedness")
      call destroy_(C)

   end subroutine

   subroutine put_vertex_shape_index(self)
    type(isosurface_type) :: self
    ! Put the list of Koenderink shape index values for each canonical point.
      real(kind=kind(1.0d0)), dimension(:), pointer :: SI

      call ensure_(tonto,self%n_pt>0,"ISOSURFACE:put_vertex_shape_index ... no isosurface points")
      call create_(SI,self%n_pt)
      call get_vertex_shape_index_(self,SI)
      call flush_(stdout)
      call text_(stdout,"begin shape_index Shape_Index")
      call put_(stdout,SI,"column")
      call text_(stdout,"end shape_index")
      call destroy_(SI)

   end subroutine

   subroutine put_grid(self)
    type(isosurface_type) :: self
    ! Put the list of vertices for the object

      call put_(self%grid)

   end subroutine

   subroutine put_face_colours(self)
    type(isosurface_type) :: self
    ! Put the colours out, at the moment just normals
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: n
      real(kind=kind(1.0d0)), dimension(3) :: v
      integer(kind=kind(1)) :: i

      call create_(n,3,self%n_face)
      do i = 1,self%n_face
         v = average_face_gradient_(self,i)
         if (is_zero_(v)) v = (/1.0d0,0.0d0,0.0d0/)
         v = abs(v)
         call normalise_(v)
         n(:,i) = v
      end do
      call flush_(stdout)
      call show_(stdout,"begin face_colors ",self%n_face)
      call put_(stdout,n,order="column")
      call text_(stdout,"end face_colors")
      call destroy_(n)

   end subroutine

   subroutine put_normals_as_vertex_RGBs(self)
    type(isosurface_type) :: self
    ! Put the colours out for each vertex, at the moment just normals
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: n
      real(kind=kind(1.0d0)), dimension(3) :: v
      integer(kind=kind(1)) :: i

      call create_(n,3,self%n_pt)
      do i = 1,self%n_pt
         v = self%point_gradient(:,i)
         if (is_zero_(v)) v = (/1.0d0,0.0d0,0.0d0/)
         v = abs(v)
         call normalise_(v)
         n(:,i) = v
      end do
      call flush_(stdout)
      call show_(stdout,"begin vertex_colors ",self%n_pt)
      call put_(stdout,n,order="column")
      call text_(stdout,"end vertex_colors")
      call destroy_(n)

   end subroutine

   subroutine put_face_normals(self)
    type(isosurface_type) :: self
    ! Put the list of normals for each face
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: n
      real(kind=kind(1.0d0)), dimension(3) :: v
      integer(kind=kind(1)) :: i

      call create_(n,3,self%n_face)
      do i = 1,self%n_face
         v = average_face_gradient_(self,i)
         if (is_zero_(v)) v = (/1.0d0,0.0d0,0.0d0/)
         call normalise_(v)
         n(:,i) = v
      end do
      call flush_(stdout)
      call show_(stdout,"begin face_normals ",self%n_face)
      call put_(stdout,n,order="column")
      call text_(stdout,"end face_normals")
      call destroy_(n)

   end subroutine

   subroutine put_nearest_external_atom_RGBs(self,out)
    type(isosurface_type) :: self
    ! Put the nearest external atom isosurface distances, as RGB colours. "out"
    ! are the indices of the atoms outside the surface.
      integer(kind=kind(1)), dimension(:) :: out
      real(kind=kind(1.0d0)), dimension(:), pointer :: dist
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: RGB
      real(kind=kind(1.0d0)), dimension(2) :: val

   call ensure_(tonto,associated(self%atom),"ISOSURFACE:put_nearest_external_atom_RGBs ... no atom data")
   call ensure_(tonto,associated(self%colour),"ISOSURFACE:put_nearest_external_atom_RGBs ... no colour function")
      call create_(RGB,3,self%n_pt)
      dist => nearest_atom_distances_(self,self%atom(out))
      call rescale_data_(self%colour,range_(dist))
      val = range_(dist)
     ! stdout.show("dist range low  =",val(1))
     ! stdout.show("dist range high =",val(2))
     ! .colour.put
      call get_RGB_for_(self%colour,dist,RGB)
      call destroy_(dist)
      call flush_(stdout)
      call show_(stdout,"begin vertex_colors ",self%n_pt)
      call put_(stdout,RGB,order="column")
      call text_(stdout,"end vertex_colors")
      call destroy_(RGB)

   end subroutine

   subroutine put_nearest_internal_atom_RGBs(self,in)
    type(isosurface_type) :: self
    ! Put the nearest internal atom isosurface distances, as RGB colours. "in"
    ! are the indices of the atoms inside the surface.
      integer(kind=kind(1)), dimension(:) :: in
      real(kind=kind(1.0d0)), dimension(:), pointer :: dist
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: RGB

      call ensure_(tonto,associated(self%atom),"ISOSURFACE:put_nearest_internal_atom_RGBs ... no atom data")
      call ensure_(tonto,associated(self%colour),"ISOSURFACE:put_nearest_internal_atom_RGBs ... no colour function")
      call create_(RGB,3,self%n_pt)
      dist => nearest_atom_distances_(self,self%atom(in))
      call rescale_data_(self%colour,range_(dist))
      call get_RGB_for_(self%colour,dist,RGB)
      call destroy_(dist)
      call flush_(stdout)
      call show_(stdout,"begin vertex_colors ",self%n_pt)
      call put_(stdout,RGB,order="column")
      call text_(stdout,"end vertex_colors")
      call destroy_(RGB)

   end subroutine

   subroutine put_binned_d_i_d_e_RGBs(self,in,out)
    type(isosurface_type) :: self
    ! Put a binned representation of the surface, d_e vs. d_i, as RGBs. "in" and
    ! "out" are (respectively) the indices of the atoms inside and outside the
    ! surface.
      integer(kind=kind(1)), dimension(:) :: in,out
      real(kind=kind(1.0d0)), dimension(:), pointer :: d_e,d_i
      integer(kind=kind(1)), dimension(:,:), pointer :: bin_count
      integer(kind=kind(1)), dimension(3) :: RGB255
      integer(kind=kind(1)) :: n_e,n_i,e,i

   call ensure_(tonto,associated(self%atom),"ISOSURFACE:put_binned_d_i_d_e_RGBs ... no atom data")
   call ensure_(tonto,associated(self%colour),"ISOSURFACE:put_binned_d_i_d_e_RGBs ... no colour function")
      d_i => nearest_atom_distances_(self,self%atom(in))
      d_e => nearest_atom_distances_(self,self%atom(out))
      call bin_XY_data_(bin_count,d_i,d_e,0.2d0)
      call rescale_data_(self%colour,range_(bin_count))
      call destroy_(d_e)
      call destroy_(d_i)
      call flush_(stdout)
      n_i = size(bin_count,1)
      n_e = size(bin_count,2)
      call show_(stdout,"begin binned_d_i_d_e_colors ",trim(to_str_(n_i))//" "//trim(to_str_(n_e)))
      do i = 1,n_i
      do e = 1,n_e
         call put_(stdout,i)
         call put_(stdout,e)
         RGB255 = RGB255_for_(self%colour,real(bin_count(i,e),kind=kind(1.0d0)))
         call put_(stdout,RGB255(1))
         call put_(stdout,RGB255(2))
         call put_(stdout,RGB255(3))
         call flush_(stdout)
      end do
      end do
      call text_(stdout,"end binned_d_i_d_e_colors")
      call destroy_(bin_count)

   end subroutine

   subroutine put_d_i_d_e_RGBs(self,in,out)
    type(isosurface_type) :: self
    ! Put a binned representation of the surface, d_e vs. d_i, as RGBs.  "in" and
    ! "out" are (respectively) the indices of the atoms inside and outside the
    ! surface.  This differs from the put_binned_d_i_d_e_RGBs routine above in
    ! that the actual data values are outputted with the count next to them
    ! (converted to a RGB colour).
      integer(kind=kind(1)), dimension(:) :: in,out
      real(kind=kind(1.0d0)), dimension(:), pointer :: d_e,d_i
      integer(kind=kind(1)), dimension(:), pointer :: bin_count
      integer(kind=kind(1)), dimension(3) :: RGB255
      real(kind=kind(1.0d0)), dimension(2) :: range
      integer(kind=kind(1)) :: k

   call ensure_(tonto,associated(self%atom),"ISOSURFACE:put_d_i_d_e_RGBs ... no atom data")
   call ensure_(tonto,associated(self%colour),"ISOSURFACE:put_d_i_d_e_RGBs ... no colour function")
      d_i => nearest_atom_distances_(self,self%atom(in))
      d_e => nearest_atom_distances_(self,self%atom(out))
      call create_(bin_count,self%n_pt)
      call bin_XY_data_(bin_count,d_i,d_e,0.2d0)
      range = range_(bin_count)
      call rescale_data_(self%colour,range)
      call flush_(stdout)
      call show_(stdout,"begin d_i_d_e_colors ",trim(to_str_(self%n_pt)))
      do k = 1,self%n_pt
         call put_(stdout,d_i(k))
         call put_(stdout,d_e(k))
         RGB255 = RGB255_for_(self%colour,real(bin_count(k),kind=kind(1.0d0)))
         call put_(stdout,RGB255(1))
         call put_(stdout,RGB255(2))
         call put_(stdout,RGB255(3))
         call flush_(stdout)
      end do
      call text_(stdout,"end d_i_d_e_colors")
      call destroy_(bin_count)
      call destroy_(d_e)
      call destroy_(d_i)

   end subroutine

   function nearest_atom_distances(self,atom) result(res)
    type(isosurface_type) :: self
    ! Put out the list of distances from each point on the isosurface to the
    ! nearest atom.
      type(atom_type), dimension(:) :: atom
      real(kind=kind(1.0d0)), dimension(:), pointer :: res
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: dist
      integer(kind=kind(1)) :: n_atom,i,a

      call ensure_(tonto,associated(self%point),"ISOSURFACE:nearest_atom_distances ... no points")
      n_atom = n_atom_(atom)
      call create_(res,self%n_pt)
      call create_(dist,3,n_atom)
      do i = 1,self%n_pt
         do a = 1,n_atom
            dist(:,a) = atom(a)%pos - self%point(:,i)
         end do
         res(i) = minval(column_norms_(dist))
      end do
      call destroy_(dist)

   end function

   subroutine put_vrml(self,out)
    type(isosurface_type) :: self
    ! Put the isosurface data into VRML format to the file "out".
     type(textfile_type) :: out
     real(kind=kind(1.0d0)), dimension(3) :: n
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: RGB
     integer(kind=kind(1)) :: i

     call ensure_(tonto,self%n_pt>0,"ISOSURFACE:put_vrml ... no isosurface points")
     call text_(stdout,"Generating VRML isosurface")
     call text_(out,"Shape {")
     call text_(out,"  appearance Appearance {")
     call text_(out,"    material Material {")
     call text_(out,"      diffuseColor 0.5 0.5 0.5")
     call text_(out,"      ambientIntensity 0.5")
     call text_(out,"      emissiveColor 0.1 0.1 0.1")
     call text_(out,"    }")
     call text_(out,"  }")

     call text_(out,"  geometry IndexedFaceSet {")

      ! Output the list of vertices.
     call text_(out,"    coord Coordinate {")
     call text_(out,"      point "//achar(91))
     do i=1,self%n_pt
       call put_(out,"      ")
       call put_(out,self%point(1,i))
       call put_(out,self%point(2,i))
       call put_(out,self%point(3,i))
       if (i==self%n_pt) then
         call put_(out," "//achar(93),flush=1)
       else
         call put_(out,",",flush=1)
       end if
     end do
     call text_(out,"    }")

      ! Output the list of vertices for each face.  Each face is ended with index
      ! -1, since in VRML we are not restricted to triangles.
     call text_(out,"    coordIndex "//achar(91))
     do i = 1,self%n_face
       call put_(out,"      ")
       call put_(out,self%face(1,i)-1)
       call put_(out,self%face(2,i)-1)
       call put_(out,self%face(3,i)-1)
       call put_(out,-1,flush=1)
     end do
     call text_(out,"    "//achar(93))
     call text_(out,"    solid FALSE")
     call text_(out,"    creaseAngle 2")

      ! Output the list of normals corresponding to the vertices.
     call text_(out,"    normal Normal {")
     call text_(out,"      vector "//achar(91))
     do i = 1,self%n_pt
       n = self%point_gradient(:,i)
       call normalise_(n)
       call put_(out,n(1))
       call put_(out,n(2))
       call put_(out,n(3))
       if (i==self%n_pt) then
         call put_(out," "//achar(93),flush=1)
       else
         call put_(out,",",flush=1)
       end if
     end do
     call text_(out,"    }")

      ! Output the colour of each vertex, if applicable.
     if (associated(self%surface_property_values)) then
       call create_(RGB,3,self%n_pt)
       call text_(stdout,"Scaling isosurface property values for colouring...")
       if (self%chop_surface_property_range) then
         call show_(stdout,"Min value used is ",self%surface_property_cutoff_range(1))
         call show_(stdout,"Max value used is ",self%surface_property_cutoff_range(2))
         call rescale_data_(self%colour,self%surface_property_cutoff_range)
       else
         call show_(stdout,"Min value used is ",minval(self%surface_property_values))
         call show_(stdout,"Max value used is ",maxval(self%surface_property_values))
         call rescale_data_(self%colour,range_(self%surface_property_values))
       end if
       call get_RGB_for_(self%colour,self%surface_property_values,RGB)
        ! We should not rescale colours for many properties!!!
       call text_(out,"    colorPerVertex TRUE")
       call text_(out,"    color Color {")
       call text_(out,"      color "//achar(91))
       do i = 1,self%n_pt
         call put_(out,RGB(1,i))
         call put_(out,RGB(2,i))
         call put_(out,RGB(3,i))
         if (i==self%n_pt) then
           call put_(out," "//achar(93),flush=1)
         else
           call put_(out,",",flush=1)
         end if
       end do
       call text_(out,"    }")
       call destroy_(RGB)
     end if

     call text_(out,"  }")
     call text_(out,"}")
     call text_(stdout,"done VRML isosurface")

   end subroutine

   subroutine put_CX(self,label)
    type(isosurface_type) :: self
    ! Put the isosurface data in a form that the Crystal Explorer program can
    ! read it.
      character(*) :: label

      call flush_(stdout)
      call text_(stdout,"begin surface " // trim(label))
      call put_points_(self)
      call put_faces_(self)
      call put_vertex_normals_(self)
      call put_vertex_curvatures_(self)
      call text_(stdout,"end surface")

   end subroutine

   subroutine put_CX_1(self,label,in,out)
    type(isosurface_type) :: self
    ! Put the isosurface data in a form that the Crystal Explorer program can
    ! read it.  "in" and "out" are the indices of the atoms inside and outside
    ! the surface.
      character(*) :: label
      integer(kind=kind(1)), dimension(:) :: in,out

   call ensure_(tonto,associated(self%atom),"ISOSURFACE:put_CX_1 ... no atom list")
   call ensure_(tonto,maxval(in) <=size(self%atom),"ISOSURFACE:put_CX_1 ... in atoms out of range")
   call ensure_(tonto,maxval(out)<=size(self%atom),"ISOSURFACE:put_CX_1 ... out atoms out of range")
   call ensure_(tonto,minval(in) >0,"ISOSURFACE:put_CX_1 ... in atoms out of range")
   call ensure_(tonto,minval(out)>0,"ISOSURFACE:put_CX_1 ... out atoms out of range")
      call flush_(stdout)
      call text_(stdout,"begin surface " // trim(label))
      call put_points_(self)
      call put_faces_(self)
      call put_vertex_normals_(self)
      call put_vertex_curvatures_(self)
      call put_nearest_internal_atom_RGBs_(self,in)
      call put_nearest_external_atom_RGBs_(self,out)
      call put_binned_d_i_d_e_RGBs_(self,in,out)
      call put_d_i_d_e_RGBs_(self,in,out)
      call text_(stdout,"end surface")

   end subroutine

!  **************
!  Test functions
!  **************

   subroutine test(self)
    type(isosurface_type) :: self
    ! test the tesselate routine

      call cubify_(self,test_func)
   end subroutine

   subroutine test_func(res,pt)
    ! this is a test isosurface
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: pt
!      res :: real(kind=kind(1.0d0)), dimension(pt.dim1) ! you can't mix assumed size with assumed shape
!                                in interface statements
      real(kind=kind(1.0d0)), dimension(:), intent(out) :: res
      real(kind=kind(1.0d0)), dimension(3) :: r
      integer(kind=kind(1)) :: n,i

      n = size(pt,1)
      do i = 1,n
         r = pt(i,:)
         res(i) = r(1)**2 + r(2)**2 + r(3)**2
         res(i) = 1.0d0/(max(res(i),10.0d0**(-4)))
      end do

   end subroutine

end
