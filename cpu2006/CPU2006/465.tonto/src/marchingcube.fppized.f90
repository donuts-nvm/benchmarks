!---------------------------------------------------------------------
!
! MARCHINGCUBE:
!
! For generating triangulated iso-surfaces using the "marching cubes"
! algorithm. This represents a single marching cube. The data
! statements below contain details of the algorithm.
!
! CREDIT: This is a modified version of "conscript.f" written by:
!
! (C) Mike Lawrence February 2000
!     Biomolecular Research Institute
!     343 Royal Parade
!     Parkville 3052
!     Victoria
!     Australia
!
! (C) Stephen K Wolff, Dylan Jayatilaka, 2002
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
! $Id: marchingcube.foo,v 1.2.2.6 2003/10/13 06:22:45 reaper Exp $
!---------------------------------------------------------------------

module MARCHINGCUBE_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

   use REALVEC_MODULE, only: normalise_
   use REALVEC_MODULE, only: norm_
   use REALVEC_MODULE, only: is_zero_
   use REALVEC_MODULE, only: cross_

   use INT_MODULE, only: bit_test_
   use INT_MODULE, only: to_str_

   use STR_MODULE, only: to_lower_case_

   use TEXTFILE_MODULE, only: stdin
   use TEXTFILE_MODULE, only: stdout
   use TEXTFILE_MODULE, only: set_default_units_
   use TEXTFILE_MODULE, only: text_
   use TEXTFILE_MODULE, only: next_str_
   use TEXTFILE_MODULE, only: skip_next_item_
   use TEXTFILE_MODULE, only: show_bit_string_
   use TEXTFILE_MODULE, only: next_item_
   use TEXTFILE_MODULE, only: show_
   use TEXTFILE_MODULE, only: put_
   use TEXTFILE_MODULE, only: read_
   use TEXTFILE_MODULE, only: flush_
   use TEXTFILE_MODULE, only: reverted_

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

   public    no_of_edges_
   interface no_of_edges_
      module procedure no_of_edges
   end interface

   public    is_nearly_on_surface_
   interface is_nearly_on_surface_
      module procedure is_nearly_on_surface
   end interface

   public    has_right_face_on_surface_
   interface has_right_face_on_surface_
      module procedure has_right_face_on_surface
   end interface

   private    set_triangle_vertex_info_
   interface set_triangle_vertex_info_
      module procedure set_triangle_vertex_info
   end interface

   public    set_left_info_
   interface set_left_info_
      module procedure set_left_info
   end interface

   public    set_vertex_info_
   interface set_vertex_info_
      module procedure set_vertex_info
      module procedure set_vertex_info_1
   end interface

   public    read_iso_value_
   interface read_iso_value_
      module procedure read_iso_value
   end interface

   public    read_keywords_
   interface read_keywords_
      module procedure read_keywords
   end interface

   public    get_triangle_vertex_indices_
   interface get_triangle_vertex_indices_
      module procedure get_triangle_vertex_indices
   end interface

   public    get_edge_vertex_gradients_
   interface get_edge_vertex_gradients_
      module procedure get_edge_vertex_gradients
   end interface

   public    set_edge_vertex_info_
   interface set_edge_vertex_info_
      module procedure set_edge_vertex_info
   end interface

   public    put_vertex_index_info_
   interface put_vertex_index_info_
      module procedure put_vertex_index_info
   end interface

   public    interpolate_faces_
   interface interpolate_faces_
      module procedure interpolate_faces
   end interface

   public    set_case_info_
   interface set_case_info_
      module procedure set_case_info
   end interface

   public    read_interior_is_big_
   interface read_interior_is_big_
      module procedure read_interior_is_big
   end interface

   public    no_of_active_edges_
   interface no_of_active_edges_
      module procedure no_of_active_edges
   end interface

   public    is_nearly_on_surface_old_
   interface is_nearly_on_surface_old_
      module procedure is_nearly_on_surface_old
   end interface

   public    copy_
   interface copy_
      module procedure copy
   end interface

   public    set_iso_value_
   interface set_iso_value_
      module procedure set_iso_value
   end interface

   public    set_big_interior_
   interface set_big_interior_
      module procedure set_big_interior
   end interface

   public    get_edge_vertex_positions_
   interface get_edge_vertex_positions_
      module procedure get_edge_vertex_positions
   end interface

   public    has_upper_face_on_surface_
   interface has_upper_face_on_surface_
      module procedure has_upper_face_on_surface
   end interface

   public    put_bitmask_info_
   interface put_bitmask_info_
      module procedure put_bitmask_info
   end interface

   public    no_back_face_indices_in_
   interface no_back_face_indices_in_
      module procedure no_back_face_indices_in
   end interface

   public    is_on_surface_
   interface is_on_surface_
      module procedure is_on_surface
   end interface

   public    set_left_skip_bit_string_
   interface set_left_skip_bit_string_
      module procedure set_left_skip_bit_string
   end interface

   public    case_number_
   interface case_number_
      module procedure case_number
      module procedure case_number_1
   end interface

   public    destroy_
   interface destroy_
      module procedure destroy
   end interface

   public    set_n_pt_
   interface set_n_pt_
      module procedure set_n_pt
   end interface

   public    read_units_
   interface read_units_
      module procedure read_units
   end interface

   public    read_accuracy_
   interface read_accuracy_
      module procedure read_accuracy
   end interface

   public    set_cube_bit_string_
   interface set_cube_bit_string_
      module procedure set_cube_bit_string
   end interface

   public    has_left_face_on_surface_
   interface has_left_face_on_surface_
      module procedure has_left_face_on_surface
   end interface

   public    no_of_triangles_
   interface no_of_triangles_
      module procedure no_of_triangles
   end interface

   public    set_skip_bit_string_
   interface set_skip_bit_string_
      module procedure set_skip_bit_string
   end interface

   public    set_hessian_info_
   interface set_hessian_info_
      module procedure set_hessian_info
   end interface

   public    has_front_face_on_surface_
   interface has_front_face_on_surface_
      module procedure has_front_face_on_surface
   end interface

   public    get_edge_mean_curvatures_
   interface get_edge_mean_curvatures_
      module procedure get_edge_mean_curvatures
   end interface

   public    set_edge_bit_string_
   interface set_edge_bit_string_
      module procedure set_edge_bit_string
   end interface

   public    set_vertex_and_gradient_info_
   interface set_vertex_and_gradient_info_
      module procedure set_vertex_and_gradient_info
   end interface

   public    set_triangulation_info_
   interface set_triangulation_info_
      module procedure set_triangulation_info
   end interface

   public    no_upper_face_indices_in_
   interface no_upper_face_indices_in_
      module procedure no_upper_face_indices_in
   end interface

   public    set_front_info_
   interface set_front_info_
      module procedure set_front_info
   end interface

   public    is_outside_surface_
   interface is_outside_surface_
      module procedure is_outside_surface
   end interface

   public    process_keyword_
   interface process_keyword_
      module procedure process_keyword
   end interface

   public    put_
   interface put_
      module procedure put
   end interface

   public    set_side_length_
   interface set_side_length_
      module procedure set_side_length
   end interface

   public    set_defaults_
   interface set_defaults_
      module procedure set_defaults
   end interface

   public    get_edge_gaussian_curvatures_
   interface get_edge_gaussian_curvatures_
      module procedure get_edge_gaussian_curvatures
   end interface

   public    set_accuracy_
   interface set_accuracy_
      module procedure set_accuracy
   end interface

   public    read_side_length_
   interface read_side_length_
      module procedure read_side_length
   end interface

   public    create_
   interface create_
      module procedure create
   end interface

   public    triangulate_
   interface triangulate_
      module procedure triangulate
   end interface

   public    create_copy_
   interface create_copy_
      module procedure create_copy
   end interface

   public    set_gradient_info_
   interface set_gradient_info_
      module procedure set_gradient_info
   end interface

   public    set_below_skip_bit_string_
   interface set_below_skip_bit_string_
      module procedure set_below_skip_bit_string
   end interface

   public    read_junk_
   interface read_junk_
      module procedure read_junk
   end interface

   public    put_positional_info_
   interface put_positional_info_
      module procedure put_positional_info
   end interface

   private    interpolate_edge_info_
   interface interpolate_edge_info_
      module procedure interpolate_edge_info
   end interface

   public    set_below_info_
   interface set_below_info_
      module procedure set_below_info
   end interface

   public    set_hessian_eval_array_
   interface set_hessian_eval_array_
      module procedure set_hessian_eval_array
   end interface

   public    reset_
   interface reset_
      module procedure reset
   end interface

   public    has_lower_face_on_surface_
   interface has_lower_face_on_surface_
      module procedure has_lower_face_on_surface
   end interface

   public    set_front_skip_bit_string_
   interface set_front_skip_bit_string_
      module procedure set_front_skip_bit_string
   end interface

   public    has_back_face_on_surface_
   interface has_back_face_on_surface_
      module procedure has_back_face_on_surface
   end interface

   public    is_inside_surface_
   interface is_inside_surface_
      module procedure is_inside_surface
   end interface

!  ***************
!  Data Statements
!  ***************

!  Every cube has 8 vertices. "cube" lists the eight vertices of a cube in the
!  marching cubes standard order. The vertex indices are in the range 0:7

   integer(kind=kind(1)), dimension(1:3,0:7), private :: cube
!                        v0     v1     v2     v3     v4     v5     v6     v7
   data cube(1:3,0:7)/0,0,0, 1,0,0, 1,1,0, 0,1,0, 0,0,1, 1,0,1, 1,1,1, 0,1,1/

!  There are 12 cube edges which can be defined in a standard order.
!  "vertex_1_of_edge(e)" gives the 1-st vertex index of the e-th cube edge.
!  "vertex_2_of_edge(e)" gives the 2-nd vertex index of the e-th cube edge.
!  The edge indices are in the range 0:11.

   integer(kind=kind(1)), dimension(0:11), private :: vertex_1_of_edge
   integer(kind=kind(1)), dimension(0:11), private :: vertex_2_of_edge
!                                   e0  e1  e2  e3  e4  e5  e6  e7  e8  e9 e10 e11
   data vertex_1_of_edge(0:11)/      0,  1,  2,  3,  4,  5,  6,  7,  0,  1,  2,  3/
   data vertex_2_of_edge(0:11)/      1,  2,  3,  0,  5,  6,  7,  4,  4,  5,  6,  7/

!  The following data is the heart of the module. In the marching cubes
!  algorithm, every cube vertex is classified either less than or greater than
!  the iso value for the isosurface. Since there are 8 cube vertices, there are
!  only 256 possibile cases that can occur. Each case can be encoded as a bit
!  string. For example, if vertex 0 and 7 are ON (i.e. LESS than the iso value
!  of the isosurface) and all other vertex are OFF (i.e. GREATER than the iso
!  value for the isosurface), then the case # is: 010000001 = 129.
!  For each case, a number of triangles may be drawn which approximates the
!  isosurface through the cube. The "edge_table" maps each case # to another bit
!  string which describes the *edges* to be used to construct each triangle.
!  The midpoints along the edges (or some other interpolated value along the
!  edge) are used to make each vertex of the triangle. For example, for case
!  #129, only two triangle are drawn, involving edges 3, 0, 8, 11, 7 and 6 (as
!  described below for traingle_table(:,129)). The bits string encoding these
!  edges is: 000100111001001 = 9(12)9 = 9c9 base 16.  i.e. edge_table(129) =
!  "9c9", as shown below.
!  Note that the opposite case # 101111110 also involves the same triangles,
!  thus the edge_table is symmetric in two halves. However, the edge bit string
!  should not be reversed, so that ON stands for GREATER than the iso value,
!  because otherwise the the arrays vertex_1_of_edge and vertex_2_of_edge would
!  have to be swapped to be consistent with edge_table, in order that the sign
!  of the function gradients are correctly calculated.

   integer(kind=kind(1)), dimension(0:255), private :: edge_table

   data edge_table(  0:127)/ &
      Z'0'  , Z'109', Z'203', Z'30a', Z'406', Z'50f', Z'605', Z'70c', &
      Z'80c', Z'905', Z'a0f', Z'b06', Z'c0a', Z'd03', Z'e09', Z'f00', &
      Z'190', Z'99' , Z'393', Z'29a', Z'596', Z'49f', Z'795', Z'69c', &
      Z'99c', Z'895', Z'b9f', Z'a96', Z'd9a', Z'c93', Z'f99', Z'e90', &
      Z'230', Z'339', Z'33' , Z'13a', Z'636', Z'73f', Z'435', Z'53c', &
      Z'a3c', Z'b35', Z'83f', Z'936', Z'e3a', Z'f33', Z'c39', Z'd30', &
      Z'3a0', Z'2a9', Z'1a3', Z'aa' , Z'7a6', Z'6af', Z'5a5', Z'4ac', &
      Z'bac', Z'aa5', Z'9af', Z'8a6', Z'faa', Z'ea3', Z'da9', Z'ca0', &
      Z'460', Z'569', Z'663', Z'76a', Z'66' , Z'16f', Z'265', Z'36c', &
      Z'c6c', Z'd65', Z'e6f', Z'f66', Z'86a', Z'963', Z'a69', Z'b60', &
      Z'5f0', Z'4f9', Z'7f3', Z'6fa', Z'1f6', Z'ff' , Z'3f5', Z'2fc', &
      Z'dfc', Z'cf5', Z'fff', Z'ef6', Z'9fa', Z'8f3', Z'bf9', Z'af0', &
      Z'650', Z'759', Z'453', Z'55a', Z'256', Z'35f', Z'55' , Z'15c', &
      Z'e5c', Z'f55', Z'c5f', Z'd56', Z'a5a', Z'b53', Z'859', Z'950', &
      Z'7c0', Z'6c9', Z'5c3', Z'4ca', Z'3c6', Z'2cf', Z'1c5', Z'cc' , &
      Z'fcc', Z'ec5', Z'dcf', Z'cc6', Z'bca', Z'ac3', Z'9c9', Z'8c0'/

!  The bottom half is a reflection of the top half

   data edge_table(128:255)/ &
      Z'8c0', Z'9c9', Z'ac3', Z'bca', Z'cc6', Z'dcf', Z'ec5', Z'fcc', &
      Z'cc' , Z'1c5', Z'2cf', Z'3c6', Z'4ca', Z'5c3', Z'6c9', Z'7c0', &
      Z'950', Z'859', Z'b53', Z'a5a', Z'd56', Z'c5f', Z'f55', Z'e5c', &
      Z'15c', Z'55' , Z'35f', Z'256', Z'55a', Z'453', Z'759', Z'650', &
      Z'af0', Z'bf9', Z'8f3', Z'9fa', Z'ef6', Z'fff', Z'cf5', Z'dfc', &
      Z'2fc', Z'3f5', Z'ff' , Z'1f6', Z'6fa', Z'7f3', Z'4f9', Z'5f0', &
      Z'b60', Z'a69', Z'963', Z'86a', Z'f66', Z'e6f', Z'd65', Z'c6c', &
      Z'36c', Z'265', Z'16f', Z'66' , Z'76a', Z'663', Z'569', Z'460', &
      Z'ca0', Z'da9', Z'ea3', Z'faa', Z'8a6', Z'9af', Z'aa5', Z'bac', &
      Z'4ac', Z'5a5', Z'6af', Z'7a6', Z'aa' , Z'1a3', Z'2a9', Z'3a0', &
      Z'd30', Z'c39', Z'f33', Z'e3a', Z'936', Z'83f', Z'b35', Z'a3c', &
      Z'53c', Z'435', Z'73f', Z'636', Z'13a', Z'33' , Z'339', Z'230', &
      Z'e90', Z'f99', Z'c93', Z'd9a', Z'a96', Z'b9f', Z'895', Z'99c', &
      Z'69c', Z'795', Z'49f', Z'596', Z'29a', Z'393', Z'99' , Z'190', &
      Z'f00', Z'e09', Z'd03', Z'c0a', Z'b06', Z'a0f', Z'905', Z'80c', &
      Z'70c', Z'605', Z'50f', Z'406', Z'30a', Z'203', Z'109', Z'0'/

!  The "triangle_table(n)" lists the edges used to form each triangle,
!  for the n-th possible case number. They come in groups of three,
!  and -1 is the null value indicating no edge, e.g. the fourth data
!  item below is:
!      1, 8, 3, 9, 8, 1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
!  This contains two triangles.  The first is formed from points on
!  edges 1, 8, 3, while the second is formed from points on edges 9, 8, 1.

   integer(kind=kind(1)), dimension(0:15,0:255), private :: triangle_table

   data triangle_table(0:15,  0: 24)/                  &
      -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       0, 8, 3,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       0, 1, 9,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       1, 8, 3, 9, 8, 1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       1, 2,10,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       0, 8, 3, 1, 2,10,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       9, 2,10, 0, 2, 9,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       2, 8, 3, 2,10, 8,10, 9, 8,-1,-1,-1,-1,-1,-1,-1, &
       3,11, 2,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       0,11, 2, 8,11, 0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       1, 9, 0, 2, 3,11,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       1,11, 2, 1, 9,11, 9, 8,11,-1,-1,-1,-1,-1,-1,-1, &
       3,10, 1,11,10, 3,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       0,10, 1, 0, 8,10, 8,11,10,-1,-1,-1,-1,-1,-1,-1, &
       3, 9, 0, 3,11, 9,11,10, 9,-1,-1,-1,-1,-1,-1,-1, &
       9, 8,10,10, 8,11,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       4, 7, 8,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       4, 3, 0, 7, 3, 4,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       0, 1, 9, 8, 4, 7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       4, 1, 9, 4, 7, 1, 7, 3, 1,-1,-1,-1,-1,-1,-1,-1, &
       1, 2,10, 8, 4, 7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       3, 4, 7, 3, 0, 4, 1, 2,10,-1,-1,-1,-1,-1,-1,-1, &
       9, 2,10, 9, 0, 2, 8, 4, 7,-1,-1,-1,-1,-1,-1,-1, &
       2,10, 9, 2, 9, 7, 2, 7, 3, 7, 9, 4,-1,-1,-1,-1, &
       8, 4, 7, 3,11, 2,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1/
   data triangle_table(0:15, 25: 49)/                  &
      11, 4, 7,11, 2, 4, 2, 0, 4,-1,-1,-1,-1,-1,-1,-1, &
       9, 0, 1, 8, 4, 7, 2, 3,11,-1,-1,-1,-1,-1,-1,-1, &
       4, 7,11, 9, 4,11, 9,11, 2, 9, 2, 1,-1,-1,-1,-1, &
       3,10, 1, 3,11,10, 7, 8, 4,-1,-1,-1,-1,-1,-1,-1, &
       1,11,10, 1, 4,11, 1, 0, 4, 7,11, 4,-1,-1,-1,-1, &
       4, 7, 8, 9, 0,11, 9,11,10,11, 0, 3,-1,-1,-1,-1, &
       4, 7,11, 4,11, 9, 9,11,10,-1,-1,-1,-1,-1,-1,-1, &
       9, 5, 4,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       9, 5, 4, 0, 8, 3,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       0, 5, 4, 1, 5, 0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       8, 5, 4, 8, 3, 5, 3, 1, 5,-1,-1,-1,-1,-1,-1,-1, &
       1, 2,10, 9, 5, 4,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       3, 0, 8, 1, 2,10, 4, 9, 5,-1,-1,-1,-1,-1,-1,-1, &
       5, 2,10, 5, 4, 2, 4, 0, 2,-1,-1,-1,-1,-1,-1,-1, &
       2,10, 5, 3, 2, 5, 3, 5, 4, 3, 4, 8,-1,-1,-1,-1, &
       9, 5, 4, 2, 3,11,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       0,11, 2, 0, 8,11, 4, 9, 5,-1,-1,-1,-1,-1,-1,-1, &
       0, 5, 4, 0, 1, 5, 2, 3,11,-1,-1,-1,-1,-1,-1,-1, &
       2, 1, 5, 2, 5, 8, 2, 8,11, 4, 8, 5,-1,-1,-1,-1, &
      10, 3,11,10, 1, 3, 9, 5, 4,-1,-1,-1,-1,-1,-1,-1, &
       4, 9, 5, 0, 8, 1, 8,10, 1, 8,11,10,-1,-1,-1,-1, &
       5, 4, 0, 5, 0,11, 5,11,10,11, 0, 3,-1,-1,-1,-1, &
       5, 4, 8, 5, 8,10,10, 8,11,-1,-1,-1,-1,-1,-1,-1, &
       9, 7, 8, 5, 7, 9,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       9, 3, 0, 9, 5, 3, 5, 7, 3,-1,-1,-1,-1,-1,-1,-1/
   data triangle_table(0:15, 50: 74)/                  &
       0, 7, 8, 0, 1, 7, 1, 5, 7,-1,-1,-1,-1,-1,-1,-1, &
       1, 5, 3, 3, 5, 7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       9, 7, 8, 9, 5, 7,10, 1, 2,-1,-1,-1,-1,-1,-1,-1, &
      10, 1, 2, 9, 5, 0, 5, 3, 0, 5, 7, 3,-1,-1,-1,-1, &
       8, 0, 2, 8, 2, 5, 8, 5, 7,10, 5, 2,-1,-1,-1,-1, &
       2,10, 5, 2, 5, 3, 3, 5, 7,-1,-1,-1,-1,-1,-1,-1, &
       7, 9, 5, 7, 8, 9, 3,11, 2,-1,-1,-1,-1,-1,-1,-1, &
       9, 5, 7, 9, 7, 2, 9, 2, 0, 2, 7,11,-1,-1,-1,-1, &
       2, 3,11, 0, 1, 8, 1, 7, 8, 1, 5, 7,-1,-1,-1,-1, &
      11, 2, 1,11, 1, 7, 7, 1, 5,-1,-1,-1,-1,-1,-1,-1, &
       9, 5, 8, 8, 5, 7,10, 1, 3,10, 3,11,-1,-1,-1,-1, &
       5, 7, 0, 5, 0, 9, 7,11, 0, 1, 0,10,11,10, 0,-1, &
      11,10, 0,11, 0, 3,10, 5, 0, 8, 0, 7, 5, 7, 0,-1, &
      11,10, 5, 7,11, 5,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
      10, 6, 5,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       0, 8, 3, 5,10, 6,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       9, 0, 1, 5,10, 6,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       1, 8, 3, 1, 9, 8, 5,10, 6,-1,-1,-1,-1,-1,-1,-1, &
       1, 6, 5, 2, 6, 1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       1, 6, 5, 1, 2, 6, 3, 0, 8,-1,-1,-1,-1,-1,-1,-1, &
       9, 6, 5, 9, 0, 6, 0, 2, 6,-1,-1,-1,-1,-1,-1,-1, &
       5, 9, 8, 5, 8, 2, 5, 2, 6, 3, 2, 8,-1,-1,-1,-1, &
       2, 3,11,10, 6, 5,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
      11, 0, 8,11, 2, 0,10, 6, 5,-1,-1,-1,-1,-1,-1,-1, &
       0, 1, 9, 2, 3,11, 5,10, 6,-1,-1,-1,-1,-1,-1,-1/
   data triangle_table(0:15, 75: 99)/                  &
       5,10, 6, 1, 9, 2, 9,11, 2, 9, 8,11,-1,-1,-1,-1, &
       6, 3,11, 6, 5, 3, 5, 1, 3,-1,-1,-1,-1,-1,-1,-1, &
       0, 8,11, 0,11, 5, 0, 5, 1, 5,11, 6,-1,-1,-1,-1, &
       3,11, 6, 0, 3, 6, 0, 6, 5, 0, 5, 9,-1,-1,-1,-1, &
       6, 5, 9, 6, 9,11,11, 9, 8,-1,-1,-1,-1,-1,-1,-1, &
       5,10, 6, 4, 7, 8,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       4, 3, 0, 4, 7, 3, 6, 5,10,-1,-1,-1,-1,-1,-1,-1, &
       1, 9, 0, 5,10, 6, 8, 4, 7,-1,-1,-1,-1,-1,-1,-1, &
      10, 6, 5, 1, 9, 7, 1, 7, 3, 7, 9, 4,-1,-1,-1,-1, &
       6, 1, 2, 6, 5, 1, 4, 7, 8,-1,-1,-1,-1,-1,-1,-1, &
       1, 2, 5, 5, 2, 6, 3, 0, 4, 3, 4, 7,-1,-1,-1,-1, &
       8, 4, 7, 9, 0, 5, 0, 6, 5, 0, 2, 6,-1,-1,-1,-1, &
       7, 3, 9, 7, 9, 4, 3, 2, 9, 5, 9, 6, 2, 6, 9,-1, &
       3,11, 2, 7, 8, 4,10, 6, 5,-1,-1,-1,-1,-1,-1,-1, &
       5,10, 6, 4, 7, 2, 4, 2, 0, 2, 7,11,-1,-1,-1,-1, &
       0, 1, 9, 4, 7, 8, 2, 3,11, 5,10, 6,-1,-1,-1,-1, &
       9, 2, 1, 9,11, 2, 9, 4,11, 7,11, 4, 5,10, 6,-1, &
       8, 4, 7, 3,11, 5, 3, 5, 1, 5,11, 6,-1,-1,-1,-1, &
       5, 1,11, 5,11, 6, 1, 0,11, 7,11, 4, 0, 4,11,-1, &
       0, 5, 9, 0, 6, 5, 0, 3, 6,11, 6, 3, 8, 4, 7,-1, &
       6, 5, 9, 6, 9,11, 4, 7, 9, 7,11, 9,-1,-1,-1,-1, &
      10, 4, 9, 6, 4,10,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       4,10, 6, 4, 9,10, 0, 8, 3,-1,-1,-1,-1,-1,-1,-1, &
      10, 0, 1,10, 6, 0, 6, 4, 0,-1,-1,-1,-1,-1,-1,-1, &
       8, 3, 1, 8, 1, 6, 8, 6, 4, 6, 1,10,-1,-1,-1,-1/
   data triangle_table(0:15,100:124)/                  &
       1, 4, 9, 1, 2, 4, 2, 6, 4,-1,-1,-1,-1,-1,-1,-1, &
       3, 0, 8, 1, 2, 9, 2, 4, 9, 2, 6, 4,-1,-1,-1,-1, &
       0, 2, 4, 4, 2, 6,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       8, 3, 2, 8, 2, 4, 4, 2, 6,-1,-1,-1,-1,-1,-1,-1, &
      10, 4, 9,10, 6, 4,11, 2, 3,-1,-1,-1,-1,-1,-1,-1, &
       0, 8, 2, 2, 8,11, 4, 9,10, 4,10, 6,-1,-1,-1,-1, &
       3,11, 2, 0, 1, 6, 0, 6, 4, 6, 1,10,-1,-1,-1,-1, &
       6, 4, 1, 6, 1,10, 4, 8, 1, 2, 1,11, 8,11, 1,-1, &
       9, 6, 4, 9, 3, 6, 9, 1, 3,11, 6, 3,-1,-1,-1,-1, &
       8,11, 1, 8, 1, 0,11, 6, 1, 9, 1, 4, 6, 4, 1,-1, &
       3,11, 6, 3, 6, 0, 0, 6, 4,-1,-1,-1,-1,-1,-1,-1, &
       6, 4, 8,11, 6, 8,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       7,10, 6, 7, 8,10, 8, 9,10,-1,-1,-1,-1,-1,-1,-1, &
       0, 7, 3, 0,10, 7, 0, 9,10, 6, 7,10,-1,-1,-1,-1, &
      10, 6, 7, 1,10, 7, 1, 7, 8, 1, 8, 0,-1,-1,-1,-1, &
      10, 6, 7,10, 7, 1, 1, 7, 3,-1,-1,-1,-1,-1,-1,-1, &
       1, 2, 6, 1, 6, 8, 1, 8, 9, 8, 6, 7,-1,-1,-1,-1, &
       2, 6, 9, 2, 9, 1, 6, 7, 9, 0, 9, 3, 7, 3, 9,-1, &
       7, 8, 0, 7, 0, 6, 6, 0, 2,-1,-1,-1,-1,-1,-1,-1, &
       7, 3, 2, 6, 7, 2,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       2, 3,11,10, 6, 8,10, 8, 9, 8, 6, 7,-1,-1,-1,-1, &
       2, 0, 7, 2, 7,11, 0, 9, 7, 6, 7,10, 9,10, 7,-1, &
       1, 8, 0, 1, 7, 8, 1,10, 7, 6, 7,10, 2, 3,11,-1, &
      11, 2, 1,11, 1, 7,10, 6, 1, 6, 7, 1,-1,-1,-1,-1, &
       8, 9, 6, 8, 6, 7, 9, 1, 6,11, 6, 3, 1, 3, 6,-1/
   data triangle_table(0:15,125:149)/                  &
       0, 9, 1,11, 6, 7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       7, 8, 0, 7, 0, 6, 3,11, 0,11, 6, 0,-1,-1,-1,-1, &
       7,11, 6,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       7, 6,11,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       3, 0, 8,11, 7, 6,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       0, 1, 9,11, 7, 6,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       8, 1, 9, 8, 3, 1,11, 7, 6,-1,-1,-1,-1,-1,-1,-1, &
      10, 1, 2, 6,11, 7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       1, 2,10, 3, 0, 8, 6,11, 7,-1,-1,-1,-1,-1,-1,-1, &
       2, 9, 0, 2,10, 9, 6,11, 7,-1,-1,-1,-1,-1,-1,-1, &
       6,11, 7, 2,10, 3,10, 8, 3,10, 9, 8,-1,-1,-1,-1, &
       7, 2, 3, 6, 2, 7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       7, 0, 8, 7, 6, 0, 6, 2, 0,-1,-1,-1,-1,-1,-1,-1, &
       2, 7, 6, 2, 3, 7, 0, 1, 9,-1,-1,-1,-1,-1,-1,-1, &
       1, 6, 2, 1, 8, 6, 1, 9, 8, 8, 7, 6,-1,-1,-1,-1, &
      10, 7, 6,10, 1, 7, 1, 3, 7,-1,-1,-1,-1,-1,-1,-1, &
      10, 7, 6, 1, 7,10, 1, 8, 7, 1, 0, 8,-1,-1,-1,-1, &
       0, 3, 7, 0, 7,10, 0,10, 9, 6,10, 7,-1,-1,-1,-1, &
       7, 6,10, 7,10, 8, 8,10, 9,-1,-1,-1,-1,-1,-1,-1, &
       6, 8, 4,11, 8, 6,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       3, 6,11, 3, 0, 6, 0, 4, 6,-1,-1,-1,-1,-1,-1,-1, &
       8, 6,11, 8, 4, 6, 9, 0, 1,-1,-1,-1,-1,-1,-1,-1, &
       9, 4, 6, 9, 6, 3, 9, 3, 1,11, 3, 6,-1,-1,-1,-1, &
       6, 8, 4, 6,11, 8, 2,10, 1,-1,-1,-1,-1,-1,-1,-1, &
       1, 2,10, 3, 0,11, 0, 6,11, 0, 4, 6,-1,-1,-1,-1/
   data triangle_table(0:15,150:174)/                  &
       4,11, 8, 4, 6,11, 0, 2, 9, 2,10, 9,-1,-1,-1,-1, &
      10, 9, 3,10, 3, 2, 9, 4, 3,11, 3, 6, 4, 6, 3,-1, &
       8, 2, 3, 8, 4, 2, 4, 6, 2,-1,-1,-1,-1,-1,-1,-1, &
       0, 4, 2, 4, 6, 2,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       1, 9, 0, 2, 3, 4, 2, 4, 6, 4, 3, 8,-1,-1,-1,-1, &
       1, 9, 4, 1, 4, 2, 2, 4, 6,-1,-1,-1,-1,-1,-1,-1, &
       8, 1, 3, 8, 6, 1, 8, 4, 6, 6,10, 1,-1,-1,-1,-1, &
      10, 1, 0,10, 0, 6, 6, 0, 4,-1,-1,-1,-1,-1,-1,-1, &
       4, 6, 3, 4, 3, 8, 6,10, 3, 0, 3, 9,10, 9, 3,-1, &
      10, 9, 4, 6,10, 4,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       4, 9, 5, 7, 6,11,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       0, 8, 3, 4, 9, 5,11, 7, 6,-1,-1,-1,-1,-1,-1,-1, &
       5, 0, 1, 5, 4, 0, 7, 6,11,-1,-1,-1,-1,-1,-1,-1, &
      11, 7, 6, 8, 3, 4, 3, 5, 4, 3, 1, 5,-1,-1,-1,-1, &
       9, 5, 4,10, 1, 2, 7, 6,11,-1,-1,-1,-1,-1,-1,-1, &
       6,11, 7, 1, 2,10, 0, 8, 3, 4, 9, 5,-1,-1,-1,-1, &
       7, 6,11, 5, 4,10, 4, 2,10, 4, 0, 2,-1,-1,-1,-1, &
       3, 4, 8, 3, 5, 4, 3, 2, 5,10, 5, 2,11, 7, 6,-1, &
       7, 2, 3, 7, 6, 2, 5, 4, 9,-1,-1,-1,-1,-1,-1,-1, &
       9, 5, 4, 0, 8, 6, 0, 6, 2, 6, 8, 7,-1,-1,-1,-1, &
       3, 6, 2, 3, 7, 6, 1, 5, 0, 5, 4, 0,-1,-1,-1,-1, &
       6, 2, 8, 6, 8, 7, 2, 1, 8, 4, 8, 5, 1, 5, 8,-1, &
       9, 5, 4,10, 1, 6, 1, 7, 6, 1, 3, 7,-1,-1,-1,-1, &
       1, 6,10, 1, 7, 6, 1, 0, 7, 8, 7, 0, 9, 5, 4,-1, &
       4, 0,10, 4,10, 5, 0, 3,10, 6,10, 7, 3, 7,10,-1/
   data triangle_table(0:15,175:199)/                  &
       7, 6,10, 7,10, 8, 5, 4,10, 4, 8,10,-1,-1,-1,-1, &
       6, 9, 5, 6,11, 9,11, 8, 9,-1,-1,-1,-1,-1,-1,-1, &
       3, 6,11, 0, 6, 3, 0, 5, 6, 0, 9, 5,-1,-1,-1,-1, &
       0,11, 8, 0, 5,11, 0, 1, 5, 5, 6,11,-1,-1,-1,-1, &
       6,11, 3, 6, 3, 5, 5, 3, 1,-1,-1,-1,-1,-1,-1,-1, &
       1, 2,10, 9, 5,11, 9,11, 8,11, 5, 6,-1,-1,-1,-1, &
       0,11, 3, 0, 6,11, 0, 9, 6, 5, 6, 9, 1, 2,10,-1, &
      11, 8, 5,11, 5, 6, 8, 0, 5,10, 5, 2, 0, 2, 5,-1, &
       6,11, 3, 6, 3, 5, 2,10, 3,10, 5, 3,-1,-1,-1,-1, &
       5, 8, 9, 5, 2, 8, 5, 6, 2, 3, 8, 2,-1,-1,-1,-1, &
       9, 5, 6, 9, 6, 0, 0, 6, 2,-1,-1,-1,-1,-1,-1,-1, &
       1, 5, 8, 1, 8, 0, 5, 6, 8, 3, 8, 2, 6, 2, 8,-1, &
       1, 5, 6, 2, 1, 6,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       1, 3, 6, 1, 6,10, 3, 8, 6, 5, 6, 9, 8, 9, 6,-1, &
      10, 1, 0,10, 0, 6, 9, 5, 0, 5, 6, 0,-1,-1,-1,-1, &
       0, 3, 8, 5, 6,10,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
      10, 5, 6,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
      11, 5,10, 7, 5,11,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
      11, 5,10,11, 7, 5, 8, 3, 0,-1,-1,-1,-1,-1,-1,-1, &
       5,11, 7, 5,10,11, 1, 9, 0,-1,-1,-1,-1,-1,-1,-1, &
      10, 7, 5,10,11, 7, 9, 8, 1, 8, 3, 1,-1,-1,-1,-1, &
      11, 1, 2,11, 7, 1, 7, 5, 1,-1,-1,-1,-1,-1,-1,-1, &
       0, 8, 3, 1, 2, 7, 1, 7, 5, 7, 2,11,-1,-1,-1,-1, &
       9, 7, 5, 9, 2, 7, 9, 0, 2, 2,11, 7,-1,-1,-1,-1, &
       7, 5, 2, 7, 2,11, 5, 9, 2, 3, 2, 8, 9, 8, 2,-1/
   data triangle_table(0:15,200:224)/                  &
       2, 5,10, 2, 3, 5, 3, 7, 5,-1,-1,-1,-1,-1,-1,-1, &
       8, 2, 0, 8, 5, 2, 8, 7, 5,10, 2, 5,-1,-1,-1,-1, &
       9, 0, 1, 5,10, 3, 5, 3, 7, 3,10, 2,-1,-1,-1,-1, &
       9, 8, 2, 9, 2, 1, 8, 7, 2,10, 2, 5, 7, 5, 2,-1, &
       1, 3, 5, 3, 7, 5,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       0, 8, 7, 0, 7, 1, 1, 7, 5,-1,-1,-1,-1,-1,-1,-1, &
       9, 0, 3, 9, 3, 5, 5, 3, 7,-1,-1,-1,-1,-1,-1,-1, &
       9, 8, 7, 5, 9, 7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       5, 8, 4, 5,10, 8,10,11, 8,-1,-1,-1,-1,-1,-1,-1, &
       5, 0, 4, 5,11, 0, 5,10,11,11, 3, 0,-1,-1,-1,-1, &
       0, 1, 9, 8, 4,10, 8,10,11,10, 4, 5,-1,-1,-1,-1, &
      10,11, 4,10, 4, 5,11, 3, 4, 9, 4, 1, 3, 1, 4,-1, &
       2, 5, 1, 2, 8, 5, 2,11, 8, 4, 5, 8,-1,-1,-1,-1, &
       0, 4,11, 0,11, 3, 4, 5,11, 2,11, 1, 5, 1,11,-1, &
       0, 2, 5, 0, 5, 9, 2,11, 5, 4, 5, 8,11, 8, 5,-1, &
       9, 4, 5, 2,11, 3,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       2, 5,10, 3, 5, 2, 3, 4, 5, 3, 8, 4,-1,-1,-1,-1, &
       5,10, 2, 5, 2, 4, 4, 2, 0,-1,-1,-1,-1,-1,-1,-1, &
       3,10, 2, 3, 5,10, 3, 8, 5, 4, 5, 8, 0, 1, 9,-1, &
       5,10, 2, 5, 2, 4, 1, 9, 2, 9, 4, 2,-1,-1,-1,-1, &
       8, 4, 5, 8, 5, 3, 3, 5, 1,-1,-1,-1,-1,-1,-1,-1, &
       0, 4, 5, 1, 0, 5,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       8, 4, 5, 8, 5, 3, 9, 0, 5, 0, 3, 5,-1,-1,-1,-1, &
       9, 4, 5,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       4,11, 7, 4, 9,11, 9,10,11,-1,-1,-1,-1,-1,-1,-1/
   data triangle_table(0:15,225:255)/                  &
       0, 8, 3, 4, 9, 7, 9,11, 7, 9,10,11,-1,-1,-1,-1, &
       1,10,11, 1,11, 4, 1, 4, 0, 7, 4,11,-1,-1,-1,-1, &
       3, 1, 4, 3, 4, 8, 1,10, 4, 7, 4,11,10,11, 4,-1, &
       4,11, 7, 9,11, 4, 9, 2,11, 9, 1, 2,-1,-1,-1,-1, &
       9, 7, 4, 9,11, 7, 9, 1,11, 2,11, 1, 0, 8, 3,-1, &
      11, 7, 4,11, 4, 2, 2, 4, 0,-1,-1,-1,-1,-1,-1,-1, &
      11, 7, 4,11, 4, 2, 8, 3, 4, 3, 2, 4,-1,-1,-1,-1, &
       2, 9,10, 2, 7, 9, 2, 3, 7, 7, 4, 9,-1,-1,-1,-1, &
       9,10, 7, 9, 7, 4,10, 2, 7, 8, 7, 0, 2, 0, 7,-1, &
       3, 7,10, 3,10, 2, 7, 4,10, 1,10, 0, 4, 0,10,-1, &
       1,10, 2, 8, 7, 4,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       4, 9, 1, 4, 1, 7, 7, 1, 3,-1,-1,-1,-1,-1,-1,-1, &
       4, 9, 1, 4, 1, 7, 0, 8, 1, 8, 7, 1,-1,-1,-1,-1, &
       4, 0, 3, 7, 4, 3,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       4, 8, 7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       9,10, 8,10,11, 8,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       3, 0, 9, 3, 9,11,11, 9,10,-1,-1,-1,-1,-1,-1,-1, &
       0, 1,10, 0,10, 8, 8,10,11,-1,-1,-1,-1,-1,-1,-1, &
       3, 1,10,11, 3,10,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       1, 2,11, 1,11, 9, 9,11, 8,-1,-1,-1,-1,-1,-1,-1, &
       3, 0, 9, 3, 9,11, 1, 2, 9, 2,11, 9,-1,-1,-1,-1, &
       0, 2,11, 8, 0,11,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       3, 2,11,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       2, 3, 8, 2, 8,10,10, 8, 9,-1,-1,-1,-1,-1,-1,-1, &
       9,10, 2, 0, 9, 2,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       2, 3, 8, 2, 8,10, 0, 1, 8, 1,10, 8,-1,-1,-1,-1, &
       1,10, 2,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       1, 3, 8, 9, 1, 8,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       0, 9, 1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       0, 3, 8,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
      -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1/

contains

   subroutine create(self)
    type(marchingcube_type) :: self
    ! Create a object
     pointer :: self

     allocate(self)

     call set_defaults_(self)

   end subroutine

   subroutine create_copy(self,s)
    type(marchingcube_type) :: self
    ! Create self as a duplicate of "s".
     pointer :: self
     type(marchingcube_type), intent(in) :: s

     call create_(self)
     call copy_(self,s)

   end subroutine

   subroutine copy(self,s)
    type(marchingcube_type) :: self
    ! Copy the contents of "s" to self.
     type(marchingcube_type), intent(in) :: s

     self = s

   end subroutine

   subroutine destroy(self)
    type(marchingcube_type) :: self
    ! Destroy the object
      pointer :: self

      if (.not. associated(self)) then;   return; end if
      deallocate(self)

   end subroutine

!   nullify_ptr_part
!   ! Nullify the pointer parts
!   end

!   destroy_ptr_part
!   ! Destroy the pointer parts
!   end

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
    type(marchingcube_type) :: self
    ! Set up default isosurface information.

     self%vertex_pos            = 0.0d0
     self%vertex_gradient       = 0.0d0
     self%value_at_vertex       = 0.0d0
     self%iso_value             = 1.0d0
     self%accuracy              = 10.0d0**(-3)
     self%case                  = -1   ! This is not a real case
     self%interior_case         = 0    ! Interior function values assumed larger than exterior
     self%exterior_case         = 255  ! 0 means all the vertices are larger than iso_value
     self%edge_bit_string       = 0
     self%cube_bit_string       = 0
     self%skip_bit_string       = 0
     self%n_edge                = 0
     self%n_triangle            = 0
     self%n_pt                  = 0
     self%edge_vertex_pos       = 0.0d0
     self%edge_vertex_gradient  = 0.0d0
     self%edge_vertex_index     = 0.0d0
     self%vertex_edge_index     = 0.0d0
    ! .destroy_ptr_part
    ! .nullify_ptr_part
     self%triangle_edge_index   = 0
     self%triangle_vertex_index = 0

   end subroutine

   subroutine reset(self)
    type(marchingcube_type) :: self
    ! Reset defaults. Compared to set_defaults, .iso_value is unchanged,
    ! and .accuracy is unchanged.

     self%vertex_pos             = 0.0d0
     self%vertex_gradient        = 0.0d0
     self%value_at_vertex        = 0.0d0
     self%case                   = -1   ! This is not a real case
     self%edge_bit_string        = 0
     self%skip_bit_string        = 0
     self%n_edge                 = 0
     self%n_triangle             = 0
     self%n_pt                   = 0
     self%edge_vertex_pos        = 0.0d0
     self%edge_vertex_gradient   = 0.0d0
     self%edge_vertex_index      = 0.0d0
     self%vertex_edge_index      = 0.0d0
    ! .destroy_ptr_part
    ! .nullify_ptr_part
     self%triangle_edge_index    = 0
     self%triangle_vertex_index  = 0

   end subroutine

!  *************
!  Input methods
!  *************

   recursive subroutine read_keywords(self)
    type(marchingcube_type) :: self
    ! Read data from "stdin" using keyword style input.
    ! The following code is inherited from OBJECT
     character(128) :: word

     call ensure_(tonto,next_item_(stdin)=="{","MARCHINGCUBE:read_keywords ... expecting open bracket symbol, {")
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
    type(marchingcube_type) :: self
    ! Process command "keyword". Any required data needed by the "keyword" is
    ! inputted from "stdin".
      character(*) :: keyword
      character(128) :: word

      word = keyword
      call to_lower_case_(word)
      select case (word)
         case ("}                ")   ! exit case
         case ("accuracy=        "); call read_accuracy_(self)
         case ("interior_is_big= "); call read_interior_is_big_(self)
         case ("iso_value=       "); call read_iso_value_(self)
         case ("put              "); call put_(self)
         case ("side_length=     "); call read_side_length_(self)
         case ("units=           "); call read_units_(self)
         case default;           allocate(tonto%known_keywords(7))
         tonto%known_keywords(1) = "}                "
         tonto%known_keywords(2) = "accuracy=        "
         tonto%known_keywords(3) = "interior_is_big= "
         tonto%known_keywords(4) = "iso_value=       "
         tonto%known_keywords(5) = "put              "
         tonto%known_keywords(6) = "side_length=     "
         tonto%known_keywords(7) = "units=           "
         call unknown_(tonto,word,"MARCHINGCUBE:process_keyword")
         deallocate(tonto%known_keywords)
      end select

   end subroutine

   subroutine read_units(self)
    type(marchingcube_type) :: self
    ! Read a string which describes the units to be used
    ! The following code is inherited from OBJECT

      call set_default_units_(stdin,next_str_(stdin))

   end subroutine

   subroutine read_junk(self)
    type(marchingcube_type) :: self
    ! Read in a junk string, useful for ignoring a field
    ! The following code is inherited from OBJECT

      call skip_next_item_(stdin)

   end subroutine

   subroutine read_accuracy(self)
    type(marchingcube_type) :: self
    ! Read a number which tells to what accuracy each generated isosurface
    ! is determined

      call read_(stdin,self%accuracy)

   end subroutine

   subroutine read_interior_is_big(self)
    type(marchingcube_type) :: self
    ! Read a switch which tells whether the interior of the isosurface which the
    ! cube is mapping is larger than the exterior. This sets the interior and
    ! exterior case numbers, and would be determined by, for example, whether the
    ! surface gradients are required to be reversed (since in this case the
    ! gradients would be pointing inwards from the surface toward increasing
    ! values).  i.e. interior_is_big = reverse_normals
      logical(kind=kind(.true.)) :: big_interior

      call read_(stdin,big_interior)
      call set_big_interior_(self,big_interior)

   end subroutine

   subroutine read_iso_value(self)
    type(marchingcube_type) :: self
    ! Read the defining iso_value for the isosurface

      call read_(stdin,self%iso_value)

   end subroutine

   subroutine read_side_length(self)
    type(marchingcube_type) :: self
    ! Read the side length of the cube

      call read_(stdin,self%side_length)

   end subroutine

!  ***************
!  Set/get methods
!  ***************

   subroutine set_vertex_info(self,px,py,pz,f)
    type(marchingcube_type) :: self
    ! Set the marching cube vertex info, i.e. x, y z, coordinates of each vertex
    ! "px", "py", "pz", and the function values "f" at each vertex. Vertex
    ! gradient information is *not" set.
      real(kind=kind(1.0d0)), dimension(0:1,0:1,0:1), intent(in) :: px,py,pz,f
      integer(kind=kind(1)) :: v,x,y,z

      do v = 0, 7  ! loop over vertices
          x = cube(1,v)
          y = cube(2,v)
          z = cube(3,v)
          self%vertex_pos(1,v)    = px(x,y,z)
          self%vertex_pos(2,v)    = py(x,y,z)
          self%vertex_pos(3,v)    = pz(x,y,z)
          self%value_at_vertex(v) = f(x,y,z)
      end do

   end subroutine

   subroutine set_vertex_info_1(self,p,f)
    type(marchingcube_type) :: self
    ! Set the marching cube vertex info, i.e. x, y z, coordinates of each vertex
    ! "p" and the function values "f" at each vertex. Vertex gradient information
    ! is *not* set.
      real(kind=kind(1.0d0)), dimension(3,0:1,0:1,0:1), intent(in) :: p
      real(kind=kind(1.0d0)), dimension(0:1,0:1,0:1), intent(in) :: f
      integer(kind=kind(1)) :: v,x,y,z

      do v = 0, 7  ! loop over vertices
          x = cube(1,v)
          y = cube(2,v)
          z = cube(3,v)
          self%vertex_pos(:,v)     =  p(:,x,y,z)
          self%value_at_vertex(v)  =  f(x,y,z)
      end do

   end subroutine

   subroutine set_gradient_info(self,f)
    type(marchingcube_type) :: self
    ! Set the marching cube vertex gradient information, calculated by finite
    ! differences, from a cube of values "f". The "f" array indices 2 & 3
    ! correspond to the actual cube, indices 1 & 4 are the surrounding cube
    ! edges, required to evaluate the gradients. NOTE: only those corners for
    ! which .cube_bit_string is set have the gradients computed.
      real(kind=kind(1.0d0)), dimension(4,4,4), intent(in) :: f
      integer(kind=kind(1)) :: v,x,y,z
      real(kind=kind(1.0d0)) :: fac

      fac = 1.0d0/(2.0d0*self%side_length)
      if (self%interior_case==0) fac = -fac
      do v = 0, 7  ! loop over vertices
          if (.not. btest(self%cube_bit_string,v)) cycle
          x = cube(1,v) + 2
          y = cube(2,v) + 2
          z = cube(3,v) + 2
          self%vertex_gradient(1,v)  = (f(x+1,y,z)-f(x-1,y,z)) * fac
          self%vertex_gradient(2,v)  = (f(x,y+1,z)-f(x,y-1,z)) * fac
          self%vertex_gradient(3,v)  = (f(x,y,z+1)-f(x,y,z-1)) * fac
      end do

   end subroutine

   subroutine set_hessian_info(self,f)
    type(marchingcube_type) :: self
    ! Set the marching cube vertex gradient and hessian information, calculated by
    ! finite differences, from a cube of values "f". The "f" array indices 2 & 3
    ! correspond to the actual cube, indices 1 & 4 are the surrounding cube
    ! edges, required to evaluate the gradients. NOTE: only those corners for
    ! which .cube_bit_string is set have the gradients computed.
      real(kind=kind(1.0d0)), dimension(4,4,4), intent(in) :: f
      integer(kind=kind(1)) :: v,x,y,z
      real(kind=kind(1.0d0)) :: fac,fac2,fac4

      fac = 1.0d0/(2.0d0*self%side_length)
      if (self%interior_case==0) fac = -fac
      fac2 = fac*fac
      fac4 = fac2*4.0d0
      do v = 0, 7  ! loop over vertices
          if (.not. btest(self%cube_bit_string,v)) cycle
          x = cube(1,v) + 2
          y = cube(2,v) + 2
          z = cube(3,v) + 2
          self%vertex_gradient(1,v) = (f(x+1,y,z)-f(x-1,y,z)) * fac
          self%vertex_gradient(2,v) = (f(x,y+1,z)-f(x,y-1,z)) * fac
          self%vertex_gradient(3,v) = (f(x,y,z+1)-f(x,y,z-1)) * fac
          self%vertex_hessian(1,1,v) = (f(x+1,y,z)+f(x-1,y,z)-2.0d0*f(x,y,z)) * fac4
          self%vertex_hessian(2,2,v) = (f(x,y+1,z)+f(x,y-1,z)-2.0d0*f(x,y,z)) * fac4
          self%vertex_hessian(3,3,v) = (f(x,y,z+1)+f(x,y,z-1)-2.0d0*f(x,y,z)) * fac4
          self%vertex_hessian(2,1,v) = (f(x+1,y+1,z)-f(x+1,y-1,z)-f(x-1,y+1,z)+f(x-1,y-1,z)) * fac2
          self%vertex_hessian(1,2,v) = self%vertex_hessian(2,1,v)
          self%vertex_hessian(3,1,v) = (f(x+1,y,z+1)-f(x+1,y,z-1)-f(x-1,y,z+1)+f(x-1,y,z-1)) * fac2
          self%vertex_hessian(1,3,v) = self%vertex_hessian(3,1,v)
          self%vertex_hessian(3,2,v) = (f(x,y+1,z+1)-f(x,y+1,z-1)-f(x,y-1,z+1)+f(x,y-1,z-1)) * fac2
          self%vertex_hessian(2,3,v) = self%vertex_hessian(3,2,v)
      end do

   end subroutine

   subroutine set_hessian_eval_array(self,eval,ox,oy,oz)
    type(marchingcube_type) :: self
    ! This routine is used only in conjunction with the recursive marching cube
    ! algorithm. It sets an element of array "eval" .true. if the corresponding
    ! cube point is required for the evaluation of the gradient or hessian by
    ! finite differences. Armed with this information, the number of function
    ! calls to the isosurface routine can be reduced. The numbers "ox", "oy" and
    ! "oz" are offsets telling how much to offset the x, y, and z indices of the
    ! 4x4x4 cube. As before, indices 2 & 3 correspond to the actual cube, indices
    ! 1 & 4 are the surrounding cube edges, required to evaluate the gradients.
    ! NOTE: only those corners for which .cube_bit_string is set have the
    ! gradients computed.
      logical(kind=kind(.true.)), dimension(5,5,5) :: eval
      integer(kind=kind(1)), intent(in) :: ox,oy,oz
      integer(kind=kind(1)) :: v,x,y,z

      do v = 0, 7  ! loop over vertices
          if (.not. btest(self%cube_bit_string,v)) cycle
          x = cube(1,v) + 2 + ox
          y = cube(2,v) + 2 + oy
          z = cube(3,v) + 2 + oz
          eval(x,y,z) = .true.
          eval(x+1,y,z) = .true.; eval(x-1,y,z) = .true.
          eval(x,y+1,z) = .true.; eval(x,y-1,z) = .true.
          eval(x,y,z+1) = .true.; eval(x,y,z-1) = .true.
          eval(x+1,y+1,z) = .true.; eval(x+1,y-1,z) = .true.; eval(x-1,y+1,z) = .true.; eval(x-1,y-1,z) = .true.
          eval(x+1,y,z+1) = .true.; eval(x+1,y,z-1) = .true.; eval(x-1,y,z+1) = .true.; eval(x-1,y,z-1) = .true.
          eval(x,y+1,z+1) = .true.; eval(x,y+1,z-1) = .true.; eval(x,y-1,z+1) = .true.; eval(x,y-1,z-1) = .true.;
      end do

   end subroutine

   subroutine set_vertex_and_gradient_info(self,p,f)
    type(marchingcube_type) :: self
    ! Set the marching cube vertex info, i.e. x, y z, coordinates of each vertex
    ! "p" and the function values "f" at each vertex, as well as the vertex
    ! gradient information calculated by finite differences. The "f" array
    ! indices 2 & 3 correspond to the current cube, indices 1 & 4 are the
    ! surrounding cube edges, required to evaluate the gradients.
      real(kind=kind(1.0d0)), dimension(3,0:1,0:1,0:1), intent(in) :: p
      real(kind=kind(1.0d0)), dimension(-1:2,-1:2,-1:2), intent(in) :: f
      integer(kind=kind(1)) :: v,x,y,z
      real(kind=kind(1.0d0)) :: fac

      fac = 1.0d0/(2.0d0*self%side_length)
      if (self%interior_case==0) fac = -fac
      do v = 0, 7  ! loop over vertices
          x = cube(1,v)
          y = cube(2,v)
          z = cube(3,v)
          self%vertex_pos(:,v)     =  p(:,x,y,z)
          self%value_at_vertex(v)  =  f(x,y,z)
          self%vertex_gradient(1,v)  = (f(x+1,y,z)-f(x-1,y,z)) * fac
          self%vertex_gradient(2,v)  = (f(x,y+1,z)-f(x,y-1,z)) * fac
          self%vertex_gradient(3,v)  = (f(x,y,z+1)-f(x,y,z-1)) * fac
      end do

   end subroutine

   subroutine set_n_pt(self,n)
    type(marchingcube_type) :: self
    ! Set the number of unique points found for all cubes up till this one.
      integer(kind=kind(1)) :: n

      call ensure_(tonto,n>=0,"MARCHINGCUBE:set_n_pt ... must have non-negative number of points")
      self%n_pt = n

   end subroutine

   subroutine set_iso_value(self,iso_value)
    type(marchingcube_type) :: self
    ! Set the defining "iso_value"
      real(kind=kind(1.0d0)) :: iso_value

      self%iso_value = iso_value

   end subroutine

   subroutine set_side_length(self,length)
    type(marchingcube_type) :: self
    ! Set the cube side length to "length"
      real(kind=kind(1.0d0)) :: length

      self%side_length = length

   end subroutine

   subroutine set_accuracy(self,accuracy)
    type(marchingcube_type) :: self
    ! Set a number "accuracy" tells to what accuracy each generated isosurface
    ! is determined
      real(kind=kind(1.0d0)) :: accuracy

      self%accuracy = accuracy

   end subroutine

   subroutine set_big_interior(self,big_interior)
    type(marchingcube_type) :: self
    ! Set a switch which tells whether the interior of the isosurface which the
    ! cube is mapping is larger than the exterior. This sets the interior and
    ! exterior case numbers, and would be determined by, for example, whether the
    ! surface gradients are required to be reversed (since in this case the
    ! gradients would be pointing inwards from the surface toward increasing
    ! values).  i.e. interior_is_big = reverse_normals
    ! NOTE: 0 means all the vertices are larger than iso_value
    !       1 means all the vertices are less than iso_value
      logical(kind=kind(.true.)) :: big_interior

      select case (big_interior)
        case(.true.)
           self%interior_case = 0
           self%exterior_case = 255
        case(.false.)
           self%interior_case = 255
           self%exterior_case = 0
      end select

   end subroutine

   subroutine set_case_info(self)
    type(marchingcube_type) :: self
    ! Set the marching cube case information.

      self%case = case_number_(self)

   end subroutine

   subroutine set_triangulation_info(self)
    type(marchingcube_type) :: self
    ! Set the marching cube triangulation information i.e. whoich edges of the
    ! cube are supposed to be interpolated, how many triangle faces are needed.
    ! NOTE: the case number is assumed to be correctly set by a call to
    ! .set_case_info

      call set_edge_bit_string_(self)
      call set_cube_bit_string_(self)  ! WARNING: this routine notices .skip_bit_string
      self%n_triangle = no_of_triangles_(self,self%case)
      self%triangle_edge_index = reshape(triangle_table(0:14,self%case), (/3,5/))
      self%triangle_vertex_index  = 0

   end subroutine

   subroutine set_left_info(self,left)
    type(marchingcube_type) :: self
    ! Set the .skip_bit_string based on when there is information from the
    ! adjoing cube to the left this cube. The actual .edge_vertex_index values
    ! for the left cube are given in the array "left".
      integer(kind=kind(1)), dimension(0:11) :: left
       ! this skips the left face edges: 3,7,8,11

      if (btest(self%edge_bit_string,3)) then
         self%edge_vertex_index(3)  = left(1)
         self%skip_bit_string = ibset(self%skip_bit_string,3)
      end if
      if (btest(self%edge_bit_string,7)) then
         self%edge_vertex_index(7)  = left(5)
         self%skip_bit_string = ibset(self%skip_bit_string,7)
      end if
      if (btest(self%edge_bit_string,8)) then
         self%edge_vertex_index(8)  = left(9)
         self%skip_bit_string = ibset(self%skip_bit_string,8)
      end if
      if (btest(self%edge_bit_string,11)) then
         self%edge_vertex_index(11) = left(10)
         self%skip_bit_string = ibset(self%skip_bit_string,11)
      end if

   end subroutine

   subroutine set_front_info(self,front)
    type(marchingcube_type) :: self
    ! Set the .skip_bit_string based on when there is information from the
    ! adjoing cube to the front this cube. The actual .edge_vertex_index values
    ! for the front cube are given in the array "front".
      integer(kind=kind(1)), dimension(0:11) :: front
       ! this skips the front face edges: 0,4,8,9

      if (btest(self%edge_bit_string,0)) then
         self%edge_vertex_index(0)  = front(2)
         self%skip_bit_string = ibset(self%skip_bit_string,0)
      end if
      if (btest(self%edge_bit_string,4)) then
         self%edge_vertex_index(4)  = front(6)
         self%skip_bit_string = ibset(self%skip_bit_string,4)
      end if
      if (btest(self%edge_bit_string,8)) then
         self%edge_vertex_index(8)  = front(11)
         self%skip_bit_string = ibset(self%skip_bit_string,8)
      end if
      if (btest(self%edge_bit_string,9)) then
         self%edge_vertex_index(9)  = front(10)
         self%skip_bit_string = ibset(self%skip_bit_string,9)
      end if

   end subroutine

   subroutine set_below_info(self,below)
    type(marchingcube_type) :: self
    ! Set the .skip_bit_string based on when there is information from the
    ! adjoing cube below this cube. The actual .edge_vertex_index values for the
    ! cube below are given in the array "below".
      integer(kind=kind(1)), dimension(0:11) :: below
       ! this skips the bottom face edges: 0,1,2,3

      if (btest(self%edge_bit_string,0)) then
         self%edge_vertex_index(0)  = below(4)
         self%skip_bit_string = ibset(self%skip_bit_string,0)
      end if
      if (btest(self%edge_bit_string,1)) then
         self%edge_vertex_index(1)  = below(5)
         self%skip_bit_string = ibset(self%skip_bit_string,1)
      end if
      if (btest(self%edge_bit_string,2)) then
         self%edge_vertex_index(2)  = below(6)
         self%skip_bit_string = ibset(self%skip_bit_string,2)
      end if
      if (btest(self%edge_bit_string,3)) then
         self%edge_vertex_index(3)  = below(7)
         self%skip_bit_string = ibset(self%skip_bit_string,3)
      end if

   end subroutine

   subroutine set_left_skip_bit_string(self)
    type(marchingcube_type) :: self
    ! Set the .skip_bit_string based on when there is information from the
    ! adjoing cube to the left this cube.
       ! this skips the left face edges: 3,7,8,11

      if (btest(self%edge_bit_string,3))  self%skip_bit_string = ibset(self%skip_bit_string,3)
      if (btest(self%edge_bit_string,7))  self%skip_bit_string = ibset(self%skip_bit_string,7)
      if (btest(self%edge_bit_string,8))  self%skip_bit_string = ibset(self%skip_bit_string,8)
      if (btest(self%edge_bit_string,11)) self%skip_bit_string = ibset(self%skip_bit_string,11)

   end subroutine

   subroutine set_front_skip_bit_string(self)
    type(marchingcube_type) :: self
    ! Set the .skip_bit_string based on when there is information from the
    ! adjoing cube to the front this cube.
       ! this skips the front face edges: 0,4,8,9

      if (btest(self%edge_bit_string,0)) self%skip_bit_string = ibset(self%skip_bit_string,0)
      if (btest(self%edge_bit_string,4)) self%skip_bit_string = ibset(self%skip_bit_string,4)
      if (btest(self%edge_bit_string,8)) self%skip_bit_string = ibset(self%skip_bit_string,8)
      if (btest(self%edge_bit_string,9)) self%skip_bit_string = ibset(self%skip_bit_string,9)

   end subroutine

   subroutine set_below_skip_bit_string(self)
    type(marchingcube_type) :: self
    ! Set the .skip_bit_string based on when there is information from the
    ! adjoing cube below this cube.
       ! this skips the bottom face edges: 0,1,2,3

      if (btest(self%edge_bit_string,0)) self%skip_bit_string = ibset(self%skip_bit_string,0)
      if (btest(self%edge_bit_string,1)) self%skip_bit_string = ibset(self%skip_bit_string,1)
      if (btest(self%edge_bit_string,2)) self%skip_bit_string = ibset(self%skip_bit_string,2)
      if (btest(self%edge_bit_string,3)) self%skip_bit_string = ibset(self%skip_bit_string,3)

   end subroutine

   subroutine set_edge_bit_string(self)
    type(marchingcube_type) :: self
    ! Set the marching cube edge_bit_string which tells which edges cross the
    ! isosurface and therefore are to be interpolated. NOTE: the case number is
    ! assumed to be correctly set e.g. by a call to .set_case_info.

      self%edge_bit_string = edge_table(self%case)

   end subroutine

   subroutine set_cube_bit_string(self)
    type(marchingcube_type) :: self
    ! Set the .cube_bit_string which tells which cube corners lie on either side
    ! of the isosurface. The .cube_bit_string is used to save work when
    ! calculating the gradients and hessians on the cube corners. NOTE: the
    ! edge_bit_string is assumed to be correctly set e.g. by a call to
    ! .set_edge_bit_string. NOTE: if some edges are to be explicitly skipped then
    ! those vertices do not appear in .cube_bit_string.
      integer(kind=kind(1)) :: e

      self%cube_bit_string = 0
      do e = 0,11
         if (.not. btest(self%edge_bit_string,e)) cycle
         if (    btest(self%skip_bit_string,e)) cycle  ! this edge is explicitly skipped
         self%cube_bit_string = ibset(self%cube_bit_string,vertex_1_of_edge(e))
         self%cube_bit_string = ibset(self%cube_bit_string,vertex_2_of_edge(e))
      end do

   end subroutine

   function no_of_triangles(self,case) result(res)
    type(marchingcube_type) :: self
    ! Return the number of triangles for a marching cube with a
    ! particular "case" number.
      integer(kind=kind(1)), optional :: case
      integer(kind=kind(1)) :: res
      integer(kind=kind(1)) :: e,c
      integer(kind=kind(1)), dimension(0:15) :: triangle_edge_index

      c = self%case
      if (present(case)) c = case
   call ensure_(tonto,c>=0 .and. c<=255,"MARCHINGCUBE:no_of_triangles ... case is out of range")
      triangle_edge_index = triangle_table(:,c)
      res = 0
      do e = 0, 15, 3  ! loop over triangle edge triples
        if (triangle_edge_index(e) == -1) exit
        res = res + 1
      end do

   end function

   function no_of_edges(self) result(res)
    type(marchingcube_type) :: self
    ! Return the number of edges to be interpolated on this cube
      integer(kind=kind(1)) :: res
      integer(kind=kind(1)) :: e

   call ensure_(tonto,self%case>=0 .and. self%case<=255,"MARCHINGCUBE:no_of_edges ... case is out of range")
      res = 0
      do e = 0, 11
        if (.not. btest(self%edge_bit_string,e)) cycle
        res = res + 1
      end do

   end function

   function no_of_active_edges(self) result(res)
    type(marchingcube_type) :: self
    ! Return the number of edges to be interpolated on this cube not including
    ! those that are skipped.
      integer(kind=kind(1)) :: res
      integer(kind=kind(1)) :: e

   call ensure_(tonto,self%case>=0 .and. self%case<=255,"MARCHINGCUBE:no_of_active_edges ... case is out of range")
      res = 0
      do e = 0, 11
        if (.not. btest(self%edge_bit_string,e)) cycle
        if (    btest(self%skip_bit_string,e)) cycle
        res = res + 1
      end do

   end function

   subroutine get_edge_vertex_positions(self,pos)
    type(marchingcube_type) :: self
    ! Set the new edge vertex positions found on this marching cube
      real(kind=kind(1.0d0)), dimension(:,:) :: pos
      integer(kind=kind(1)) :: i,n

   call ensure_(tonto,size(pos,1)==3,"MARCHINGCUBE:get_edge_vertex_positions ... wrong 1st dimension, pos")
   call ensure_(tonto,size(pos,2)>=no_of_active_edges_(self),"MARCHINGCUBE:get_edge_vertex_positions ... pos too small")
      n = no_of_active_edges_(self)
      do i = 1,n
        pos(:,i) = self%edge_vertex_pos(:,self%vertex_edge_index(i))
      end do

   end subroutine

   subroutine get_edge_vertex_gradients(self,g)
    type(marchingcube_type) :: self
    ! Get the new edge vertex gradient vectors "g" on this marching cube
      real(kind=kind(1.0d0)), dimension(:,:) :: g
      integer(kind=kind(1)) :: i,n

   call ensure_(tonto,size(g,1)==3,"MARCHINGCUBE:get_edge_vertex_gradients ... wrong 1st dimension, g")
   call ensure_(tonto,size(g,2)>=no_of_active_edges_(self),"MARCHINGCUBE:get_edge_vertex_gradients ... g array too small")
      n = no_of_active_edges_(self)
      do i = 1,n
        g(:,i) = self%edge_vertex_gradient(:,self%vertex_edge_index(i))
      end do

   end subroutine

   subroutine get_edge_mean_curvatures(self,c)
    type(marchingcube_type) :: self
    ! Get the new edge vertex mean curvatures "c" on this marching cube
      real(kind=kind(1.0d0)), dimension(:) :: c
      integer(kind=kind(1)) :: i,n

   call ensure_(tonto,size(c)>=no_of_active_edges_(self),"MARCHINGCUBE:get_edge_mean_curvatures ... c array too small")
      n = no_of_active_edges_(self)
      do i = 1,n
        c(i) = self%edge_mean_curvature(self%vertex_edge_index(i))
      end do

   end subroutine

   subroutine get_edge_gaussian_curvatures(self,c)
    type(marchingcube_type) :: self
    ! Get the new edge vertex gaussian curvatures "c" on this marching cube
      real(kind=kind(1.0d0)), dimension(:) :: c
      integer(kind=kind(1)) :: i,n

   call ensure_(tonto,size(c)>=no_of_active_edges_(self),"MARCHINGCUBE:get_edge_gaussian_curvatures ... c array too small")
      n = no_of_active_edges_(self)
      do i = 1,n
        c(i) = self%edge_gaussian_curvature(self%vertex_edge_index(i))
      end do

   end subroutine

   subroutine get_triangle_vertex_indices(self,ind)
    type(marchingcube_type) :: self
    ! Set the new edge vertex positions found on this marching cube
      integer(kind=kind(1)), dimension(:,:) :: ind

   call ensure_(tonto,size(ind,1)==3,"MARCHINGCUBE:get_triangle_vertex_indices ... wrong 1st dimension, ind")
   call ensure_(tonto,size(ind,2)==self%n_triangle,"MARCHINGCUBE:get_triangle_vertex_indices ... wrong 2nd dimension, ind")
      ind = self%triangle_vertex_index(:,1:self%n_triangle)

   end subroutine

!  *************************************
!  Triangulation: this is the main thing
!  *************************************

   subroutine interpolate_faces(self)
    type(marchingcube_type) :: self
    ! This routine interpolates the triangle coordinates that result from the
    ! intersection of this cube with the isosurface. Any previously obtained
    ! information abount adjoining cubes to the left, infront, or below this cube
    ! should have already been set, e.g. using .set_left_info. This routine
    ! assumes that cubes are generated in an order from left to right (along the
    ! x-axis), front-to back (along the y-axis), and bottom to top (along the
    ! z-axis). NOTE: the .case number must already have been set.

      call interpolate_edge_info_(self)                    ! Skip some edges
      call set_triangle_vertex_info_(self)

   end subroutine

   subroutine interpolate_edge_info(self)
    type(marchingcube_type) :: self
    ! This routine interpolates the required edge_vetrices, which are later
    ! to become the triangle vertices.
      integer(kind=kind(1)) :: e,v1,v2
      real(kind=kind(1.0d0)) :: f1,f2,del, L
      real(kind=kind(1.0d0)), dimension(3) :: p1,p2,P,g1,g2,G,n,u,v
       !GG :: real(kind=kind(1.0d0)), dimension(3)
      real(kind=kind(1.0d0)), dimension(3,3) :: h1,h2,H
      real(kind=kind(1.0d0)), dimension(2,2) :: S
      real(kind=kind(1.0d0)), dimension(3,2) :: UV

      do e = 0,11
        if (.not. btest(self%edge_bit_string,e)) cycle  ! this edge does not intersect
        if (    btest(self%skip_bit_string,e)) cycle  ! this edge is explicitly skipped
        v1 = vertex_1_of_edge(e)
        v2 = vertex_2_of_edge(e)
        f1 = self%value_at_vertex(v1)
        f2 = self%value_at_vertex(v2)
        call ensure_(tonto,self%iso_value>=min(f1,f2),"MARCHINGCUBE:interpolate_edge_info ... iso_value is smaller than endpo&
&int values")
        call ensure_(tonto,self%iso_value<=max(f1,f2),"MARCHINGCUBE:interpolate_edge_info ... iso_value is larger than endpoi&
&nt values")
        p1 = self%vertex_pos(:,v1)
        p2 = self%vertex_pos(:,v2)
        g1 = self%vertex_gradient(:,v1)    ! The gradient and hessian are calculated
        g2 = self%vertex_gradient(:,v2)    ! w.r.t. the canonical box axes, .not. the
        h1 = self%vertex_hessian(:,:,v1)   ! true xyz axes.
        h2 = self%vertex_hessian(:,:,v2)
        del = (self%iso_value-f1)/(f2-f1)  ! f2 cannot equal f1
        P = p1 + del*(p2-p1)           ! Interpolate true P
        H = h1 + del*(h2-h1)           ! Interpolate hessian values
        G = g1 + del*(g2-g1)
       ! p1 = cube(:,v1)*.side_length  ! Reset end points to canonical axes
       ! p2 = cube(:,v2)*.side_length
       ! GG = 0.50d0*(g1 + del*matmul(h1,(p2-p1)) + g2 + (1.0d0-del)*matmul(h2,(p1-p2)))
        self%edge_vertex_pos(:,e)       = P
        self%edge_vertex_gradient(:,e)  = G
        self%edge_vertex_hessian(:,:,e) = H
        call die_if_(tonto,is_zero_(G),"MARCHINGCUBE:interpolate_edge_info ... zero gradient, edge: "//trim(to_str_(e)))
        L = norm_(G)
        n = G/L
        u = (/ n(2),-n(1), 0.0d0/)        ! Evaluate surface tangents u,v
        if (is_zero_(u)) u = (/-n(3), 0.0d0, n(1)/)
        call normalise_(u)
        v = cross_(n,u)
        UV(:,1) = u
        UV(:,2) = v
        S = -matmul(transpose(UV),matmul(H,UV))/L
        self%edge_mean_curvature(e) = 0.50d0*(S(1,1)+S(2,2))
        self%edge_gaussian_curvature(e) = S(1,1)*S(2,2)-S(1,2)*S(1,2)
      end do

   end subroutine

   subroutine set_triangle_vertex_info(self)
    type(marchingcube_type) :: self
    ! This routine sets the triangle vertex coordinates and indices.
      integer(kind=kind(1)) :: n,e,v, skip, i

      skip = self%skip_bit_string
      i = 0
      do n = 1,self%n_triangle
        do v = 1,3
          e = self%triangle_edge_index(v,n)
          if (btest(skip,e)) then  ! edge vertex info exists
             self%triangle_vertex_index(v,n) = self%edge_vertex_index(e)
             call die_if_(tonto,self%edge_vertex_index(e)==0,"MARCHINGCUBE:set_triangle_vertex_info ... zero edge vertex inde&
&x, edge: "//trim(to_str_(e)))
          else                     ! no edge vertex info, new point
             i = i + 1
             self%n_pt = self%n_pt + 1
             self%triangle_vertex_index(v,n) = self%n_pt
             self%edge_vertex_index(e) = self%n_pt
             self%vertex_edge_index(i) = e
             skip = ibset(skip,e)  ! skip this edge next time
          end if
        end do
      end do

   end subroutine

   subroutine triangulate(self,below,left,front)
    type(marchingcube_type) :: self
    ! This routine generates the triangle coordinates that result from the
    ! intersection of this cube with the isosurface, using information from
    ! the adjoining cube "below", to the "left" of, and "front" of this cube,
    ! where that information is supplied. This assumes that cubes are generated
    ! in an order from left to right (along the x-axis), then from front-to back
    ! (along the y-axis), and finally from bottom to top (along the z-axis).
    ! NOTE: the .case number must have already been set by .set_case_info
      type(intvec__type), optional :: below,left,front

      if (.not. is_on_surface_(self)) then;   return; end if ! Cube is entirely in/out of the surface
      call set_triangulation_info_(self)
      call set_skip_bit_string_(self,below,left,front)
      call set_edge_vertex_info_(self,below,left,front)   ! Fill in details for skipped edges
      call interpolate_faces_(self)

   end subroutine

   subroutine set_skip_bit_string(self,below,left,front)
    type(marchingcube_type) :: self
    ! Set the skip bit string based on whether there is any information from
    ! the adjoining cube "below", to the "left", or in "front" of this cube.
      type(intvec__type), optional :: below,left,front
      integer(kind=kind(1)) :: skip

      skip = 0
       ! this skips the bottom face edges: 0,1,2,3
      if (present(below))  skip = 15
       ! this skips the left face edges: 3,7,8,11
      if (present(left))   skip = ior(skip,2440)
       ! this skips the front face edges: 0,4,8,9
      if (present(front))  skip = ior(skip,785)
      self%skip_bit_string = skip

   end subroutine

   subroutine set_edge_vertex_info(self,below,left,front)
    type(marchingcube_type) :: self
    ! This routine gets the triangle vertex positions using information from
    ! the adjoining cube "below", to the "left", or in "front" of this cube, if
    ! that information is available.
    ! NOTE: this only sets the edge_vertex index information.
      type(intvec__type), optional :: below,left,front
      integer(kind=kind(1)) :: edge

      edge = self%edge_bit_string
      if (any(bit_test_(edge,(/0,1,2,3/))) .and. present(below)) then
         self%edge_vertex_index(0)  = below%element(4)
         self%edge_vertex_index(1)  = below%element(5)
         self%edge_vertex_index(2)  = below%element(6)
         self%edge_vertex_index(3)  = below%element(7)
      end if
      if (any(bit_test_(edge,(/3,7,8,11/))) .and. present(left)) then
         self%edge_vertex_index(3)  = left%element(1)
         self%edge_vertex_index(7)  = left%element(5)
         self%edge_vertex_index(8)  = left%element(9)
         self%edge_vertex_index(11) = left%element(10)
      end if
      if (any(bit_test_(edge,(/0,4,8,9/))) .and. present(front)) then
         self%edge_vertex_index(0)  = front%element(2)
         self%edge_vertex_index(4)  = front%element(6)
         self%edge_vertex_index(8)  = front%element(11)
         self%edge_vertex_index(9)  = front%element(10)
      end if

   end subroutine

!  ****************
!  Inquiry routines
!  ****************

   function case_number(self) result(res)
    type(marchingcube_type) :: self
    ! From the vertex values on the cube evaluate and return the case number
    ! telling us which vertices are larger than the .iso_value.
      integer(kind=kind(1)) :: res

      res = 0
      if (self%value_at_vertex(0) < self%iso_value) res = ibset(res,0)
      if (self%value_at_vertex(1) < self%iso_value) res = ibset(res,1)
      if (self%value_at_vertex(2) < self%iso_value) res = ibset(res,2)
      if (self%value_at_vertex(3) < self%iso_value) res = ibset(res,3)
      if (self%value_at_vertex(4) < self%iso_value) res = ibset(res,4)
      if (self%value_at_vertex(5) < self%iso_value) res = ibset(res,5)
      if (self%value_at_vertex(6) < self%iso_value) res = ibset(res,6)
      if (self%value_at_vertex(7) < self%iso_value) res = ibset(res,7)

   end function

   function case_number_1(self,iso_value) result(res)
    type(marchingcube_type) :: self
    ! From the vertex values on the cube evaluate and return the case number
    ! telling us which vertices are larger than the "iso_value".
      real(kind=kind(1.0d0)) :: iso_value
      integer(kind=kind(1)) :: res

      res = 0
      if (self%value_at_vertex(0) < iso_value) res = ibset(res,0)
      if (self%value_at_vertex(1) < iso_value) res = ibset(res,1)
      if (self%value_at_vertex(2) < iso_value) res = ibset(res,2)
      if (self%value_at_vertex(3) < iso_value) res = ibset(res,3)
      if (self%value_at_vertex(4) < iso_value) res = ibset(res,4)
      if (self%value_at_vertex(5) < iso_value) res = ibset(res,5)
      if (self%value_at_vertex(6) < iso_value) res = ibset(res,6)
      if (self%value_at_vertex(7) < iso_value) res = ibset(res,7)

   end function

   function is_on_surface(self) result(res)
    type(marchingcube_type) :: self
    ! Return .true. if the cube is on the surface. NOTE: .case number must be
    ! properly set beforehand with .set_case_info
      logical(kind=kind(.true.)) :: res

      res = self%case /= 0 .and. self%case /= 255

   end function

   function is_outside_surface(self) result(res)
    type(marchingcube_type) :: self
    ! Return .true. if the cube is wholly outside the surface. Usually the interior
    ! of the isosurface has function values *smaller* than the .iso_value.
    ! NOTE: .case number must be properly set beforehand with .set_case_info
      logical(kind=kind(.true.)) :: res

      res = self%case == self%exterior_case

   end function

   function is_inside_surface(self) result(res)
    type(marchingcube_type) :: self
    ! Return .true. if the cube is wholly inside the surface. Usually the interior
    ! of the isosurface has function values *bigger* than the .iso_value.
    ! NOTE: .case number must be properly set beforehand with .set_case_info
      logical(kind=kind(.true.)) :: res

      res = self%case == self%interior_case

   end function

   function is_nearly_on_surface(self,tol) result(res)
    type(marchingcube_type) :: self
    ! Return .true. if the cube is nearly on the surface. This is worked out by
    ! taking the maximum difference between any two vertices of the cube, and
    ! then seeing if the .iso_value is within any one vertex value, plus or minus
    ! this maximum difference, times a factor "tol" for good measure.
      real(kind=kind(1.0d0)) :: tol
      logical(kind=kind(.true.)) :: res
      real(kind=kind(1.0d0)) :: diff

   call ensure_(tonto,tol>0.0d0,"MARCHINGCUBE:is_nearly_on_surface ... tol must be positive")
      res = is_on_surface_(self)
      if (res) then;   return; end if
      diff = maxval(self%value_at_vertex) - minval(self%value_at_vertex)
      diff = diff*tol
      res = any( abs(self%value_at_vertex - self%iso_value) < diff )

   end function

   function is_nearly_on_surface_old(self,tol) result(res)
    type(marchingcube_type) :: self
    ! Return .true. if the cube is nearly on the surface, to within a factor "tol"
    ! of the .iso_value.
      real(kind=kind(1.0d0)) :: tol
      logical(kind=kind(.true.)) :: res

   call ensure_(tonto,tol>0.0d0,"MARCHINGCUBE:is_nearly_on_surface_old ... tol must be positive")
      res = is_on_surface_(self)
      if (res) then;   return; end if
      res = any( abs(self%value_at_vertex - self%iso_value) < tol )

   end function

   function has_left_face_on_surface(self) result(res)
    type(marchingcube_type) :: self
    ! Returns .true. if the left face is on the surface.
      logical(kind=kind(.true.)) :: res
       ! this tests left face edges: 3,7,8,11

      res = any(bit_test_(self%edge_bit_string,(/3,7,8,11/)))

   end function

   function has_front_face_on_surface(self) result(res)
    type(marchingcube_type) :: self
    ! Returns .true. if the front face is on the surface.
      logical(kind=kind(.true.)) :: res
       ! this tests front face edges: 0,4,8,9

      res = any(bit_test_(self%edge_bit_string,(/0,4,8,9/)))

   end function

   function has_lower_face_on_surface(self) result(res)
    type(marchingcube_type) :: self
    ! Returns .true. if the bottom face is on the surface.
      logical(kind=kind(.true.)) :: res
       ! this tests bottom face edges: 0,1,2,3

      res = any(bit_test_(self%edge_bit_string,(/0,1,2,3/)))

   end function

   function has_right_face_on_surface(self) result(res)
    type(marchingcube_type) :: self
    ! Returns .true. if the right face is on the surface.
      logical(kind=kind(.true.)) :: res
       ! this tests right face edges: 1,5,9,10

      res = any(bit_test_(self%edge_bit_string,(/1,5,9,10/)))

   end function

   function has_back_face_on_surface(self) result(res)
    type(marchingcube_type) :: self
    ! Returns .true. if the back face is on the surface.
      logical(kind=kind(.true.)) :: res
       ! this tests back face edges: 2,6,10,11

      res = any(bit_test_(self%edge_bit_string,(/2,6,10,11/)))

   end function

   function has_upper_face_on_surface(self) result(res)
    type(marchingcube_type) :: self
    ! Returns .true. if the upper face is on the surface.
      logical(kind=kind(.true.)) :: res
       ! this tests upper face edges: 4,5,6,7

      res = any(bit_test_(self%edge_bit_string,(/4,5,6,7/)))

   end function

   function no_back_face_indices_in(self,edge_indices) result(res)
    type(marchingcube_type) :: self
    ! Returns .true. if the "edge_indices" array has zero values in positions
    ! corresponding to back face edges of the cube.
      integer(kind=kind(1)), dimension(0:11) :: edge_indices
      logical(kind=kind(.true.)) :: res
       ! this tests back face edges: 2,6,10,11

      res = edge_indices(2)  == 0 .and. &
            edge_indices(6)  == 0 .and. &
            edge_indices(10) == 0 .and. &
            edge_indices(11) == 0

   end function

   function no_upper_face_indices_in(self,edge_indices) result(res)
    type(marchingcube_type) :: self
    ! Returns .true. if the "edge_indices" array has zero values in positions
    ! corresponding to upper face edges of the cube.
      integer(kind=kind(1)), dimension(0:11) :: edge_indices
      logical(kind=kind(.true.)) :: res
       ! this tests upper face edges: 4,5,6,7

      res = edge_indices(4) == 0 .and. &
            edge_indices(5) == 0 .and. &
            edge_indices(6) == 0 .and. &
            edge_indices(7) == 0

   end function

!  **************
!  Output methods
!  **************

   subroutine put(self,output)
    type(marchingcube_type) :: self
    ! Put the list of vertices for the object
      type(textfile_type), target, optional :: output
!      out :: type(textfile_type)*
!      if (present(output)) then; out => output
!      else;                      out => stdout
!      end

      call put_bitmask_info_(self,output)
      call put_vertex_index_info_(self,output)
      call put_positional_info_(self,output)

   end subroutine

   subroutine put_bitmask_info(self,output)
    type(marchingcube_type) :: self
    ! Put the bit mask related info for the object.
      type(textfile_type), target, optional :: output
      type(textfile_type), pointer :: out

      if (present(output)) then; out => output
      else;                      out => stdout
      end if
      call flush_(out)
      call show_(out,"iso_value       =",self%iso_value)
      call show_(out,"accuracy        =",self%accuracy)
      call show_(out,"case #          =",self%case)
      call show_bit_string_(out,"case bit_string =",self%case)
      call show_bit_string_(out,"edge_bit_string =",self%edge_bit_string)
      call show_bit_string_(out,"cube_bit_string =",self%cube_bit_string)
      call show_bit_string_(out,"skip_bit_string =",self%skip_bit_string)
      call show_(out,"left  face on?  =",has_left_face_on_surface_(self))
      call show_(out,"right face on?  =",has_right_face_on_surface_(self))
      call show_(out,"front face on?  =",has_front_face_on_surface_(self))
      call show_(out,"back  face on?  =",has_back_face_on_surface_(self))
      call show_(out,"lower face on?  =",has_lower_face_on_surface_(self))
      call show_(out,"upper face on?  =",has_upper_face_on_surface_(self))
      call show_(out,"n_edge          =",no_of_edges_(self))
      call show_(out,"n_active_edges  =",no_of_active_edges_(self))
      call show_(out,"n_triangle      =",self%n_triangle)
      call show_(out,"no_of_triangles =",no_of_triangles_(self))
      call show_(out,"n_pt            =",self%n_pt)

   end subroutine

   subroutine put_vertex_index_info(self,output)
    type(marchingcube_type) :: self
    ! Put the list of indexc related entites.
      type(textfile_type), target, optional :: output
      type(textfile_type), pointer :: out

      if (present(output)) then; out => output
      else;                      out => stdout
      end if
      call flush_(out)
      if (self%n_triangle>0) then
      call text_(out,"Triangle edge index:")
      call put_(out,self%triangle_edge_index(:,1:self%n_triangle),"column")
      call text_(out,"Triangle vertex index:")
      call put_(out,self%triangle_vertex_index(:,1:self%n_triangle),"column")
      call text_(out,"Edge vertex indices:")
      call put_(out,self%edge_vertex_index,"column")
      end if

   end subroutine

   subroutine put_positional_info(self,output)
    type(marchingcube_type) :: self
    ! Put the list of position and function related entities.
      type(textfile_type), target, optional :: output
      type(textfile_type), pointer :: out

      if (present(output)) then; out => output
      else;                      out => stdout
      end if
      call flush_(out)
      call text_(out,"Vertex positons:")
      call put_(out,self%vertex_pos,"column")
      call text_(out,"Values at each vertex:")
      call put_(out,self%value_at_vertex,"column")
      call text_(out,"Vertex gradients:")
      call put_(out,self%vertex_gradient,"column")
      if (self%n_triangle>0) then
      call text_(out,"Edge vertex positions:")
      call put_(out,self%edge_vertex_pos,"column")
      call text_(out,"Edge vertex gradients:")
      call put_(out,self%edge_vertex_gradient,"column")
      end if

   end subroutine

end
