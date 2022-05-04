!-------------------------------------------------------------------------------
!
! SPACEGROUP: Crystal space group object
!
! Synopsis
!
! Read in the international symmetry symbol (an integer or qualified integer)
! or Hermann-Mauguin symbol, or Hall symbol and make the Seitz matrices for
! the specified spacegroup. This modules provide other crystal symmetry
! information as well. Use the method in: S. R. Hall, Acta Cryst A37, 517 (1981)
!
! The code has been updated to the latest version of the Hall notation described
! in the international tables, including explicit origin notation.
!
!
! Copyright (C) S K Wolff, 1995
! Copyright (C) D Jayatilaka, 1998
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
! $Id: spacegroup.foo,v 1.15.2.12 2003/11/13 05:33:55 reaper Exp $
!-------------------------------------------------------------------------------

module SPACEGROUP_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

   use REALVEC_MODULE, only: same_as_

   use INT_MODULE, only: is_in_range_
   use INT_MODULE, only: to_str_

   use CIF_MODULE, only: read_item_
   use CIF_MODULE, only: read_looped_item_
   use CIF_MODULE, only: find_looped_item_
   use CIF_MODULE, only: find_item_

   use REALMAT3_MODULE, only: similarity_transform_12_
   use REALMAT3_MODULE, only: create_
   use REALMAT3_MODULE, only: create_copy_
   use REALMAT3_MODULE, only: destroy_

   use BUFFER_MODULE, only: exhausted_
   use BUFFER_MODULE, only: get_int_
   use BUFFER_MODULE, only: set_
   use BUFFER_MODULE, only: get_
   use BUFFER_MODULE, only: get_item_

   use INTVEC_MODULE, only: same_as_
   use INTVEC_MODULE, only: create_
   use INTVEC_MODULE, only: all_in_range_
   use INTVEC_MODULE, only: create_copy_
   use INTVEC_MODULE, only: destroy_

   use STR_MODULE, only: replace_
   use STR_MODULE, only: to_lower_case_
   use STR_MODULE, only: separate_all_characters_
   use STR_MODULE, only: trim_blanks_from_start_
   use STR_MODULE, only: frac_to_real_
   use STR_MODULE, only: separate_before_
   use STR_MODULE, only: remove_
   use STR_MODULE, only: remove_blanks_
   use STR_MODULE, only: to_int_
   use STR_MODULE, only: to_upper_case_
   use STR_MODULE, only: includes_

   use TEXTFILE_MODULE, only: stdin
   use TEXTFILE_MODULE, only: stdout
   use TEXTFILE_MODULE, only: text_
   use TEXTFILE_MODULE, only: redirect_
   use TEXTFILE_MODULE, only: put_text_
   use TEXTFILE_MODULE, only: next_item_
   use TEXTFILE_MODULE, only: show_
   use TEXTFILE_MODULE, only: put_
   use TEXTFILE_MODULE, only: revert_
   use TEXTFILE_MODULE, only: read_
   use TEXTFILE_MODULE, only: flush_
   use TEXTFILE_MODULE, only: reverted_
   use TEXTFILE_MODULE, only: dash_

   use STRVEC_MODULE, only: create_
   use STRVEC_MODULE, only: index_of_first_that_includes_
   use STRVEC_MODULE, only: destroy_
   use STRVEC_MODULE, only: index_of_
   use STRVEC_MODULE, only: includes_

   use BINVEC_MODULE, only: create_
   use BINVEC_MODULE, only: destroy_

   use REALMAT_MODULE, only: same_as_
   use REALMAT_MODULE, only: equals_
   use REALMAT_MODULE, only: create_
   use REALMAT_MODULE, only: destroy_
   use REALMAT_MODULE, only: to_unit_matrix_
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

   public    read_CIF_
   interface read_CIF_
      module procedure read_CIF
   end interface

   public    nullify_ptr_part_
   interface nullify_ptr_part_
      module procedure nullify_ptr_part
   end interface

   public    put_spacegroup_name_info_
   interface put_spacegroup_name_info_
      module procedure put_spacegroup_name_info
   end interface

   public    transform_position_
   interface transform_position_
      module procedure transform_position
   end interface

   public    destroy_ptr_part_
   interface destroy_ptr_part_
      module procedure destroy_ptr_part
   end interface

   public    read_keywords_
   interface read_keywords_
      module procedure read_keywords
   end interface

   public    put_seitz_
   interface put_seitz_
      module procedure put_seitz
   end interface

   public    put_geometry_to_unit_cell_
   interface put_geometry_to_unit_cell_
      module procedure put_geometry_to_unit_cell
   end interface

   public    decode_Jones_Faithful_symbols_
   interface decode_Jones_Faithful_symbols_
      module procedure decode_Jones_Faithful_symbols
   end interface

   public    is_same_geometry_
   interface is_same_geometry_
      module procedure is_same_geometry
   end interface

   public    decode_Jones_Faithful_symbol_
   interface decode_Jones_Faithful_symbol_
      module procedure decode_Jones_Faithful_symbol
   end interface

   public    form_L_seitz_
   interface form_L_seitz_
      module procedure form_L_seitz
   end interface

   public    read_HM_symbol_
   interface read_HM_symbol_
      module procedure read_HM_symbol
   end interface

   public    read_IT_symbol_
   interface read_IT_symbol_
      module procedure read_IT_symbol
   end interface

   public    process_keyword_
   interface process_keyword_
      module procedure process_keyword
   end interface

   public    put_
   interface put_
      module procedure put
   end interface

   public    put_matching_IT_symbols_
   interface put_matching_IT_symbols_
      module procedure put_matching_IT_symbols
   end interface

   public    get_full_geometry_
   interface get_full_geometry_
      module procedure get_full_geometry
   end interface

   public    seitz_same_as_
   interface seitz_same_as_
      module procedure seitz_same_as
   end interface

   public    put_position_to_unit_cell_
   interface put_position_to_unit_cell_
      module procedure put_position_to_unit_cell
   end interface

   public    copy_
   interface copy_
      module procedure copy
   end interface

   public    set_defaults_
   interface set_defaults_
      module procedure set_defaults
   end interface

   public    put_unique_symop_
   interface put_unique_symop_
      module procedure put_unique_symop
   end interface

   public    create_
   interface create_
      module procedure create
   end interface

   public    form_S_seitz_
   interface form_S_seitz_
      module procedure form_S_seitz
   end interface

   public    create_copy_
   interface create_copy_
      module procedure create_copy
   end interface

   public    set_Hall_symbol_
   interface set_Hall_symbol_
      module procedure set_Hall_symbol
   end interface

   public    transform_geometry_
   interface transform_geometry_
      module procedure transform_geometry
   end interface

   public    set_lattice_type_
   interface set_lattice_type_
      module procedure set_lattice_type
   end interface

   public    set_HM_symbol_
   interface set_HM_symbol_
      module procedure set_HM_symbol
   end interface

   public    make_unique_symops_
   interface make_unique_symops_
      module procedure make_unique_symops
   end interface

   public    set_IT_symbol_
   interface set_IT_symbol_
      module procedure set_IT_symbol
   end interface

   public    unique_symop_mat_
   interface unique_symop_mat_
      module procedure unique_symop_mat
   end interface

   public    destroy_
   interface destroy_
      module procedure destroy
   end interface

   public    read_Hall_symbol_
   interface read_Hall_symbol_
      module procedure read_Hall_symbol
   end interface

   public    decode_Hall_symbol_
   interface decode_Hall_symbol_
      module procedure decode_Hall_symbol
   end interface

   public    put_matching_HM_symbols_
   interface put_matching_HM_symbols_
      module procedure put_matching_HM_symbols
   end interface

   public    get_partition_factors_
   interface get_partition_factors_
      module procedure get_partition_factors
   end interface

   public    set_default_axis_symbol_info_
   interface set_default_axis_symbol_info_
      module procedure set_default_axis_symbol_info
   end interface

   public    set_spacegroup_order_
   interface set_spacegroup_order_
      module procedure set_spacegroup_order
   end interface

   public    generate_seitz_
   interface generate_seitz_
      module procedure generate_seitz
   end interface

   public    shift_origin_
   interface shift_origin_
      module procedure shift_origin
   end interface

   public    analyse_
   interface analyse_
      module procedure analyse
   end interface

!  ***************
!  Data statements
!  ***************

    ! Symbols used in the Hall space group notation

   character(128), private :: bar_symbol                 = "-" ! Bar symbol, precedes axis orders
   character(128), private :: axis_order_symbols         = "1234#6" ! Symmetry axis orders
   character(128), private :: dash_symbol                = "'" !
   character(128), private :: double_dash_symbol         = '"' !
   character(128), private :: star_symbol                = "*" !
   character(128), private :: axis_symbols               = "xyz""'*" ! Axis setting symbols
   character(128), private :: lattice_symbols            = "PABCIRHF" ! Allowed lattice symbols
   character(128), private :: alpha_translation_symbols = "abcnuvwd" ! Allowed alphabetical translation subscripts
   character(128), private :: number_translation_symbols = "12345" ! Allowed numerical translation subscripts

    ! Principal rotation matrices NN(:,:,axis,i). The "axis" symbol is either
    ! 1, 2, or 3 and stands for x, y, or z respectively. The "i" symbol is the
    ! axis order, except when i=5 or i=7 or i=8. If i=5, then the matrices are for a
    ! twofold rotation around a dash axis, and "axis" now represents the *preceding*
    ! axis symbol. Likewise, if i=7 then the matrices are for a twofold rotation around
    ! a double dash axes, with "axis" again representing the preceding axis symbol.
    ! If i=8 the rotation is in the * direction, and "axis" has no meaning, and
    ! should be set to 1.  See tables A1.4.2.4, A1.4.2.5, and A1.4.2.6 in the
    ! International Tables of Crystallography.

   real(kind=kind(1.0d0)), dimension(3,3,3,8), private :: NN

   data NN/ &
    1, 0, 0, 0, 1, 0, 0, 0, 1, &
 ! N1(:,:,x,1)
    1, 0, 0, 0, 1, 0, 0, 0, 1, &
 ! N1(:,:,y,1)
    1, 0, 0, 0, 1, 0, 0, 0, 1, &
 ! N1(:,:,z,1)
    1, 0, 0, 0,-1, 0, 0, 0,-1, &
 ! N2(:,:,x,2)
   -1, 0, 0, 0, 1, 0, 0, 0,-1, &
 ! N2(:,:,y,2)
   -1, 0, 0, 0,-1, 0, 0, 0, 1, &
 ! N2(:,:,z,2)
   -1, 0, 0, 0, 0, 1, 0,-1,-1, &
 ! N3(:,:,x,3)
   -1, 0,-1, 0, 1, 0, 1, 0, 0, &
 ! N3(:,:,y,3)
    0, 1, 0,-1,-1, 0, 0, 0, 1, &
 ! N3(:,:,z,3)
    1, 0, 0, 0, 0, 1, 0,-1, 0, &
 ! N4(:,:,x,4)
    0, 0,-1, 0, 1, 0, 1, 0, 0, &
 ! N4(:,:,y,4)
    0, 1, 0,-1, 0, 0, 0, 0, 1, &
 ! N4(:,:,z,4)
   -1, 0, 0, 0, 0,-1, 0,-1, 0, &
 ! Nd(:,:,x,5) -- dash rotations
    0, 0,-1, 0,-1, 0,-1, 0, 0, &
 ! Nd(:,:,y,5)
    0,-1, 0,-1, 0, 0, 0, 0,-1, &
 ! Nd(:,:,z,5)
    1, 0, 0, 0, 1, 1, 0,-1, 0, &
 ! N6(:,:,x,6)
    0, 0,-1, 0, 1, 0, 1, 0, 1, &
 ! N6(:,:,y,6)
    1, 1, 0,-1, 0, 0, 0, 0, 1, &
 ! N6(:,:,z,6)
   -1, 0, 0, 0, 0, 1, 0, 1, 0, &
 ! Nd(:,:,x,7) -- double dash rotation
    0, 0, 1, 0,-1, 0, 1, 0, 0, &
 ! Nd(:,:,y,7)
    0, 1, 0, 1, 0, 0, 0, 0,-1, &
 ! Nd(:,:,z,7) ... WARNING, changed last one to -1
    0, 1, 0, 0, 0, 1, 1, 0, 0, &
 ! N*(:,:,1,8) -- star rotation
    0, 0, 0, 0, 0, 0, 0, 0, 0, &
 ! N*(:,:,y,8)
    0, 0, 0, 0, 0, 0, 0, 0, 0/    ! N*(:,:,z,8)

    ! Translation vectors. See table A1.4.2.3 in the international tables of
    ! crystallography. The first index gives the actual vector. The second index
    ! gives the index of the alphabetical translation symbol.  NOTE: The
    ! numerical symbols are not stored, they can be calculated from the subscript
    ! index and the order. (See second column of the table mentioned).

   real(kind=kind(1.0d0)), dimension(3,8), private :: T_alpha

   data T_alpha/ &
      0.50d0,    0.0d0,    0.0d0, &
 ! "a" translation
      0.0d0,    0.50d0,    0.0d0, &
 ! "b" translation
      0.0d0,    0.0d0,    0.50d0, &
 ! "c" translation
      0.50d0,    0.50d0,    0.50d0, &
 ! "n" translation
   0.25d0,    0.0d0,    0.0d0, &
 ! "u" translation
      0.0d0, 0.25d0,    0.0d0, &
 ! "v" translation
      0.0d0,    0.0d0, 0.25d0, &
 ! "w" translation
   0.25d0, 0.25d0, 0.25d0/     ! "d" translation

    ! Implied translation vectors. See table A1.4.2.2 in the international tables
    ! of crystallography. The first index gives the actual translation vector.
    ! The second index runs over the number of implied vectors (the actual number
    ! of these is stored in "n_implied_translations" below). The third index is
    ! the index of the lattice type.

   integer(kind=kind(1)), dimension(8), private :: n_implied_translations

   data n_implied_translations/ 1, 2, 2, 2, 2, 3, 3, 4/

   real(kind=kind(1.0d0)), dimension(3,4,8), private :: T_implied

   data T_implied/ &
       0.0d0,     0.0d0,     0.0d0, &
 ! "P" lattice
       0.0d0,     0.0d0,     0.0d0, &
 !
       0.0d0,     0.0d0,     0.0d0, &
 !
       0.0d0,     0.0d0,     0.0d0, &
 !
       0.0d0,     0.0d0,     0.0d0, &
 ! "A" lattice
       0.0d0,     0.50d0,     0.50d0, &
 !
       0.0d0,     0.0d0,     0.0d0, &
 !
       0.0d0,     0.0d0,     0.0d0, &
 !
       0.0d0,     0.0d0,     0.0d0, &
 ! "B" lattice
       0.50d0,     0.0d0,     0.50d0, &
 !
       0.0d0,     0.0d0,     0.0d0, &
 !
       0.0d0,     0.0d0,     0.0d0, &
 !
       0.0d0,     0.0d0,     0.0d0, &
 ! "C" lattice
       0.50d0,     0.50d0,     0.0d0, &
 !
       0.0d0,     0.0d0,     0.0d0, &
 !
       0.0d0,     0.0d0,     0.0d0, &
 !
       0.0d0,     0.0d0,     0.0d0, &
 ! "I" lattice
       0.50d0,     0.50d0,     0.50d0, &
 !
       0.0d0,     0.0d0,     0.0d0, &
 !
       0.0d0,     0.0d0,     0.0d0, &
 !
       0.0d0,     0.0d0,     0.0d0, &
 ! "R" lattice
   0.66666666666666666666667d0, 0.33333333333333333333333d0, 0.33333333333333333333333d0, &
 !
   0.33333333333333333333333d0, 0.66666666666666666666667d0, 0.66666666666666666666667d0, &
 !
       0.0d0,     0.0d0,     0.0d0, &
 !
       0.0d0,     0.0d0,     0.0d0, &
 ! "H" lattice
   0.66666666666666666666667d0, 0.33333333333333333333333d0,     0.0d0, &
 !
   0.33333333333333333333333d0, 0.66666666666666666666667d0,     0.0d0, &
 !
       0.0d0,     0.0d0,     0.0d0, &
 !
       0.0d0,     0.0d0,     0.0d0, &
 ! "F" lattice
       0.0d0,     0.50d0,     0.50d0, &
 !
       0.50d0,     0.0d0,     0.50d0, &
 !
       0.50d0,     0.50d0,     0.0d0/     !

    ! Spacegroup conversion table. The first element is the International Table
    ! spacegroup number, followed by an optional colon and specific setting
    ! index. The second column is the Schoenflies pointgroup symbol. The third
    ! column is the Hermann-Mauguin spacegroup symbol. The fourth and final
    ! column is the Hall notation for the spacegroup. That is the only notation
    ! which provides a unique spacegroup specification that distinuishes all
    ! possible settings.

   character(len=14), dimension(4,593), private :: spacegroup_conversion_table

   data spacegroup_conversion_table(1:4,  1: 30)/ &
     "1             ", "c1^1          ", "p 1           ", "p 1           ", &
 ! deprecated Hall symbols
     "2             ", "ci^1          ", "p -1          ", "-p 1          ", &
     "3:b           ", "c2^1          ", "p 1 2 1       ", "p 2y          ", &
     "3:b           ", "c2^1          ", "p 2           ", "p 2y          ", &
     "3:c           ", "c2^1          ", "p 1 1 2       ", "p 2           ", &
     "3:a           ", "c2^1          ", "p 2 1 1       ", "p 2x          ", &
     "4:b           ", "c2^2          ", "p 1 21 1      ", "p 2yb         ", &
     "4:b           ", "c2^2          ", "p 1 21 1      ", "p 2y1         ", &
     "4:b           ", "c2^2          ", "p 21          ", "p 2yb         ", &
     "4:c           ", "c2^2          ", "p 1 1 21      ", "p 2c          ", &
     "4:c           ", "c2^2          ", "p 1 1 21      ", "p 21          ", &
     "4:a           ", "c2^2          ", "p 21 1 1      ", "p 2xa         ", &
     "4:a           ", "c2^2          ", "p 21 1 1      ", "p 2x1         ", &
     "5:b1          ", "c2^3          ", "c 1 2 1       ", "c 2y          ", &
     "5:b1          ", "c2^3          ", "c 2           ", "c 2y          ", &
     "5:b2          ", "c2^3          ", "a 1 2 1       ", "a 2y          ", &
     "5:b3          ", "c2^3          ", "i 1 2 1       ", "i 2y          ", &
     "5:c1          ", "c2^3          ", "a 1 1 2       ", "a 2           ", &
     "5:c2          ", "c2^3          ", "b 1 1 2       ", "b 2           ", &
     "5:c3          ", "c2^3          ", "i 1 1 2       ", "i 2           ", &
     "5:a1          ", "c2^3          ", "b 2 1 1       ", "b 2x          ", &
     "5:a2          ", "c2^3          ", "c 2 1 1       ", "c 2x          ", &
     "5:a3          ", "c2^3          ", "i 2 1 1       ", "i 2x          ", &
     "6:b           ", "cs^1          ", "p 1 m 1       ", "p -2y         ", &
     "6:b           ", "cs^1          ", "p m           ", "p -2y         ", &
     "6:c           ", "cs^1          ", "p 1 1 m       ", "p -2          ", &
     "6:a           ", "cs^1          ", "p m 1 1       ", "p -2x         ", &
     "7:b1          ", "cs^2          ", "p 1 c 1       ", "p -2yc        ", &
     "7:b1          ", "cs^2          ", "p c           ", "p -2yc        ", &
     "7:b2          ", "cs^2          ", "p 1 n 1       ", "p -2yac       "/
   data spacegroup_conversion_table(1:4, 31: 60)/ &
     "7:b2          ", "cs^2          ", "p n           ", "p -2yac       ", &
     "7:b3          ", "cs^2          ", "p 1 a 1       ", "p -2ya        ", &
     "7:b3          ", "cs^2          ", "p a           ", "p -2ya        ", &
     "7:c1          ", "cs^2          ", "p 1 1 a       ", "p -2a         ", &
     "7:c2          ", "cs^2          ", "p 1 1 n       ", "p -2ab        ", &
     "7:c3          ", "cs^2          ", "p 1 1 b       ", "p -2b         ", &
     "7:a1          ", "cs^2          ", "p b 1 1       ", "p -2xb        ", &
     "7:a2          ", "cs^2          ", "p n 1 1       ", "p -2xbc       ", &
     "7:a3          ", "cs^2          ", "p c 1 1       ", "p -2xc        ", &
     "8:b1          ", "cs^3          ", "c 1 m 1       ", "c -2y         ", &
     "8:b1          ", "cs^3          ", "c m           ", "c -2y         ", &
     "8:b2          ", "cs^3          ", "a 1 m 1       ", "a -2y         ", &
     "8:b3          ", "cs^3          ", "i 1 m 1       ", "i -2y         ", &
     "8:b3          ", "cs^3          ", "i m           ", "i -2y         ", &
     "8:c1          ", "cs^3          ", "a 1 1 m       ", "a -2          ", &
     "8:c2          ", "cs^3          ", "b 1 1 m       ", "b -2          ", &
     "8:c3          ", "cs^3          ", "i 1 1 m       ", "i -2          ", &
     "8:a1          ", "cs^3          ", "b m 1 1       ", "b -2x         ", &
     "8:a2          ", "cs^3          ", "c m 1 1       ", "c -2x         ", &
     "8:a3          ", "cs^3          ", "i m 1 1       ", "i -2x         ", &
     "9:b1          ", "cs^4          ", "c 1 c 1       ", "c -2yc        ", &
     "9:b1          ", "cs^4          ", "c c           ", "c -2yc        ", &
     "9:b2          ", "cs^4          ", "a 1 n 1       ", "a -2yab       ", &
 ! a -2yac
     "9:b3          ", "cs^4          ", "i 1 a 1       ", "i -2ya        ", &
     "9:-b1         ", "cs^4          ", "a 1 a 1       ", "a -2ya        ", &
     "9:-b2         ", "cs^4          ", "c 1 n 1       ", "c -2yac       ", &
 ! c -2ybc
     "9:-b3         ", "cs^4          ", "i 1 c 1       ", "i -2yc        ", &
     "9:c1          ", "cs^4          ", "a 1 1 a       ", "a -2a         ", &
     "9:c2          ", "cs^4          ", "b 1 1 n       ", "b -2ab        ", &
 ! b -2bc
     "9:c3          ", "cs^4          ", "i 1 1 b       ", "i -2b         "/
   data spacegroup_conversion_table(1:4, 61: 90)/ &
     "9:-c1         ", "cs^4          ", "b 1 1 b       ", "b -2b         ", &
     "9:-c2         ", "cs^4          ", "a 1 1 n       ", "a -2ab        ", &
 ! a -2ac
     "9:-c3         ", "cs^4          ", "i 1 1 a       ", "i -2a         ", &
     "9:a1          ", "cs^4          ", "b b 1 1       ", "b -2xb        ", &
     "9:a2          ", "cs^4          ", "c n 1 1       ", "c -2xac       ", &
 ! c -2xbc
     "9:a3          ", "cs^4          ", "i c 1 1       ", "i -2xc        ", &
     "9:-a1         ", "cs^4          ", "c c 1 1       ", "c -2xc        ", &
     "9:-a2         ", "cs^4          ", "b n 1 1       ", "b -2xab       ", &
 ! b -2xbc
     "9:-a3         ", "cs^4          ", "i b 1 1       ", "i -2xb        ", &
     "10:b          ", "c2h^1         ", "p 1 2/m 1     ", "-p 2y         ", &
     "10:b          ", "c2h^1         ", "p 2/m         ", "-p 2y         ", &
     "10:c          ", "c2h^1         ", "p 1 1 2/m     ", "-p 2          ", &
     "10:a          ", "c2h^1         ", "p 2/m 1 1     ", "-p 2x         ", &
     "11:b          ", "c2h^2         ", "p 1 21/m 1    ", "-p 2yb        ", &
     "11:b          ", "c2h^2         ", "p 1 21/m 1    ", "-p 2y1        ", &
     "11:b          ", "c2h^2         ", "p 21/m        ", "-p 2yb        ", &
     "11:c          ", "c2h^2         ", "p 1 1 21/m    ", "-p 2c         ", &
     "11:c          ", "c2h^2         ", "p 1 1 21/m    ", "-p 21         ", &
     "11:a          ", "c2h^2         ", "p 21/m 1 1    ", "-p 2xa        ", &
     "11:a          ", "c2h^2         ", "p 21/m 1 1    ", "-p 2x1        ", &
     "12:b1         ", "c2h^3         ", "c 1 2/m 1     ", "-c 2y         ", &
     "12:b1         ", "c2h^3         ", "c 2/m         ", "-c 2y         ", &
     "12:b2         ", "c2h^3         ", "a 1 2/m 1     ", "-a 2y         ", &
     "12:b3         ", "c2h^3         ", "i 1 2/m 1     ", "-i 2y         ", &
     "12:b3         ", "c2h^3         ", "i 2/m         ", "-i 2y         ", &
     "12:c1         ", "c2h^3         ", "a 1 1 2/m     ", "-a 2          ", &
     "12:c2         ", "c2h^3         ", "b 1 1 2/m     ", "-b 2          ", &
     "12:c3         ", "c2h^3         ", "i 1 1 2/m     ", "-i 2          ", &
     "12:a1         ", "c2h^3         ", "b 2/m 1 1     ", "-b 2x         ", &
     "12:a2         ", "c2h^3         ", "c 2/m 1 1     ", "-c 2x         "/
   data spacegroup_conversion_table(1:4, 91:120)/ &
     "12:a3         ", "c2h^3         ", "i 2/m 1 1     ", "-i 2x         ", &
     "13:b1         ", "c2h^4         ", "p 1 2/c 1     ", "-p 2yc        ", &
     "13:b1         ", "c2h^4         ", "p 2/c         ", "-p 2yc        ", &
     "13:b2         ", "c2h^4         ", "p 1 2/n 1     ", "-p 2yac       ", &
     "13:b2         ", "c2h^4         ", "p 2/n         ", "-p 2yac       ", &
     "13:b3         ", "c2h^4         ", "p 1 2/a 1     ", "-p 2ya        ", &
     "13:b3         ", "c2h^4         ", "p 2/a         ", "-p 2ya        ", &
     "13:c1         ", "c2h^4         ", "p 1 1 2/a     ", "-p 2a         ", &
     "13:c2         ", "c2h^4         ", "p 1 1 2/n     ", "-p 2ab        ", &
     "13:c3         ", "c2h^4         ", "p 1 1 2/b     ", "-p 2b         ", &
     "13:a1         ", "c2h^4         ", "p 2/b 1 1     ", "-p 2xb        ", &
     "13:a2         ", "c2h^4         ", "p 2/n 1 1     ", "-p 2xbc       ", &
     "13:a3         ", "c2h^4         ", "p 2/c 1 1     ", "-p 2xc        ", &
     "14:b1         ", "c2h^5         ", "p 1 21/c 1    ", "-p 2ybc       ", &
     "14:b1         ", "c2h^5         ", "p 21/c        ", "-p 2ybc       ", &
     "14:b2         ", "c2h^5         ", "p 1 21/n 1    ", "-p 2yn        ", &
     "14:b2         ", "c2h^5         ", "p 21/n        ", "-p 2yn        ", &
     "14:b3         ", "c2h^5         ", "p 1 21/a 1    ", "-p 2yab       ", &
     "14:b3         ", "c2h^5         ", "p 21/a        ", "-p 2yab       ", &
     "14:c1         ", "c2h^5         ", "p 1 1 21/a    ", "-p 2ac        ", &
     "14:c2         ", "c2h^5         ", "p 1 1 21/n    ", "-p 2n         ", &
     "14:c3         ", "c2h^5         ", "p 1 1 21/b    ", "-p 2bc        ", &
     "14:a1         ", "c2h^5         ", "p 21/b 1 1    ", "-p 2xab       ", &
     "14:a2         ", "c2h^5         ", "p 21/n 1 1    ", "-p 2xn        ", &
     "14:a3         ", "c2h^5         ", "p 21/c 1 1    ", "-p 2xac       ", &
     "15:b1         ", "c2h^6         ", "c 1 2/c 1     ", "-c 2yc        ", &
     "15:b1         ", "c2h^6         ", "c 2/c         ", "-c 2yc        ", &
     "15:b2         ", "c2h^6         ", "a 1 2/n 1     ", "-a 2yab       ", &
 ! -a 2yac
     "15:b3         ", "c2h^6         ", "i 1 2/a 1     ", "-i 2ya        ", &
     "15:b3         ", "c2h^6         ", "i 2/a         ", "-i 2ya        "/
   data spacegroup_conversion_table(1:4,121:150)/ &
     "15:-b1        ", "c2h^6         ", "a 1 2/a 1     ", "-a 2ya        ", &
     "15:-b2        ", "c2h^6         ", "c 1 2/n 1     ", "-c 2yac       ", &
 ! -c 2ybc
     "15:-b2        ", "c2h^6         ", "c 2/n         ", "-c 2yac       ", &
 ! -c 2ybc
     "15:-b3        ", "c2h^6         ", "i 1 2/c 1     ", "-i 2yc        ", &
     "15:-b3        ", "c2h^6         ", "i 2/c         ", "-i 2yc        ", &
     "15:c1         ", "c2h^6         ", "a 1 1 2/a     ", "-a 2a         ", &
     "15:c2         ", "c2h^6         ", "b 1 1 2/n     ", "-b 2ab        ", &
 ! -b 2bc
     "15:c3         ", "c2h^6         ", "i 1 1 2/b     ", "-i 2b         ", &
     "15:-c1        ", "c2h^6         ", "b 1 1 2/b     ", "-b 2b         ", &
     "15:-c2        ", "c2h^6         ", "a 1 1 2/n     ", "-a 2ab        ", &
 ! -a 2ac
     "15:-c3        ", "c2h^6         ", "i 1 1 2/a     ", "-i 2a         ", &
     "15:a1         ", "c2h^6         ", "b 2/b 1 1     ", "-b 2xb        ", &
     "15:a2         ", "c2h^6         ", "c 2/n 1 1     ", "-c 2xac       ", &
 ! -c 2xbc
     "15:a3         ", "c2h^6         ", "i 2/c 1 1     ", "-i 2xc        ", &
     "15:-a1        ", "c2h^6         ", "c 2/c 1 1     ", "-c 2xc        ", &
     "15:-a2        ", "c2h^6         ", "b 2/n 1 1     ", "-b 2xab       ", &
 ! -b 2xbc
     "15:-a3        ", "c2h^6         ", "i 2/b 1 1     ", "-i 2xb        ", &
     "16            ", "d2^1          ", "p 2 2 2       ", "p 2 2         ", &
     "17:           ", "d2^2          ", "p 2 2 21      ", "p 2c 2        ", &
     "17:           ", "d2^2          ", "p 2 2 21      ", "p 21 2        ", &
     "17:cab        ", "d2^2          ", "p 21 2 2      ", "p 2a 2a       ", &
     "17:bca        ", "d2^2          ", "p 2 21 2      ", "p 2 2b        ", &
     "18:           ", "d2^3          ", "p 21 21 2     ", "p 2 2ab       ", &
     "18:cab        ", "d2^3          ", "p 2 21 21     ", "p 2bc 2       ", &
     "18:bca        ", "d2^3          ", "p 21 2 21     ", "p 2ac 2ac     ", &
     "19            ", "d2^4          ", "p 21 21 21    ", "p 2ac 2ab     ", &
     "20:           ", "d2^5          ", "c 2 2 21      ", "c 2c 2        ", &
     "20:           ", "d2^5          ", "c 2 2 21      ", "c 21 2        ", &
     "20:cab        ", "d2^5          ", "a 21 2 2      ", "a 2a 2a       ", &
     "20:cab        ", "d2^5          ", "a 21 2 2      ", "a 2a 21       "/
   data spacegroup_conversion_table(1:4,151:180)/ &
     "20:bca        ", "d2^5          ", "b 2 21 2      ", "b 2 2b        ", &
     "21:           ", "d2^6          ", "c 2 2 2       ", "c 2 2         ", &
     "21:cab        ", "d2^6          ", "a 2 2 2       ", "a 2 2         ", &
     "21:bca        ", "d2^6          ", "b 2 2 2       ", "b 2 2         ", &
     "22            ", "d2^7          ", "f 2 2 2       ", "f 2 2         ", &
     "23            ", "d2^8          ", "i 2 2 2       ", "i 2 2         ", &
     "24            ", "d2^9          ", "i 21 21 21    ", "i 2b 2c       ", &
     "25:           ", "c2v^1         ", "p m m 2       ", "p 2 -2        ", &
     "25:cab        ", "c2v^1         ", "p 2 m m       ", "p -2 2        ", &
     "25:bca        ", "c2v^1         ", "p m 2 m       ", "p -2 -2       ", &
     "26:           ", "c2v^2         ", "p m c 21      ", "p 2c -2       ", &
     "26:           ", "c2v^2         ", "p m c 21      ", "p 21 -2       ", &
     "26:ba-c       ", "c2v^2         ", "p c m 21      ", "p 2c -2c      ", &
     "26:ba-c       ", "c2v^2         ", "p c m 21      ", "p 21 -2c      ", &
     "26:cab        ", "c2v^2         ", "p 21 m a      ", "p -2a 2a      ", &
     "26:-cba       ", "c2v^2         ", "p 21 a m      ", "p -2 2a       ", &
     "26:bca        ", "c2v^2         ", "p b 21 m      ", "p -2 -2b      ", &
     "26:a-cb       ", "c2v^2         ", "p m 21 b      ", "p -2b -2      ", &
     "27:           ", "c2v^3         ", "p c c 2       ", "p 2 -2c       ", &
     "27:cab        ", "c2v^3         ", "p 2 a a       ", "p -2a 2       ", &
     "27:bca        ", "c2v^3         ", "p b 2 b       ", "p -2b -2b     ", &
     "28:           ", "c2v^4         ", "p m a 2       ", "p 2 -2a       ", &
     "28:           ", "c2v^4         ", "p m a 2       ", "p 2 -21       ", &
     "28:ba-c       ", "c2v^4         ", "p b m 2       ", "p 2 -2b       ", &
     "28:cab        ", "c2v^4         ", "p 2 m b       ", "p -2b 2       ", &
     "28:-cba       ", "c2v^4         ", "p 2 c m       ", "p -2c 2       ", &
     "28:-cba       ", "c2v^4         ", "p 2 c m       ", "p -21 2       ", &
     "28:bca        ", "c2v^4         ", "p c 2 m       ", "p -2c -2c     ", &
     "28:a-cb       ", "c2v^4         ", "p m 2 a       ", "p -2a -2a     ", &
     "29:           ", "c2v^5         ", "p c a 21      ", "p 2c -2ac     "/
   data spacegroup_conversion_table(1:4,181:210)/ &
     "29:ba-c       ", "c2v^5         ", "p b c 21      ", "p 2c -2b      ", &
     "29:cab        ", "c2v^5         ", "p 21 a b      ", "p -2b 2a      ", &
     "29:-cba       ", "c2v^5         ", "p 21 c a      ", "p -2ac 2a     ", &
     "29:bca        ", "c2v^5         ", "p c 21 b      ", "p -2bc -2c    ", &
     "29:a-cb       ", "c2v^5         ", "p b 21 a      ", "p -2a -2ab    ", &
     "30:           ", "c2v^6         ", "p n c 2       ", "p 2 -2bc      ", &
     "30:ba-c       ", "c2v^6         ", "p c n 2       ", "p 2 -2ac      ", &
     "30:cab        ", "c2v^6         ", "p 2 n a       ", "p -2ac 2      ", &
     "30:-cba       ", "c2v^6         ", "p 2 a n       ", "p -2ab 2      ", &
     "30:bca        ", "c2v^6         ", "p b 2 n       ", "p -2ab -2ab   ", &
     "30:a-cb       ", "c2v^6         ", "p n 2 b       ", "p -2bc -2bc   ", &
     "31:           ", "c2v^7         ", "p m n 21      ", "p 2ac -2      ", &
     "31:ba-c       ", "c2v^7         ", "p n m 21      ", "p 2bc -2bc    ", &
     "31:cab        ", "c2v^7         ", "p 21 m n      ", "p -2ab 2ab    ", &
     "31:-cba       ", "c2v^7         ", "p 21 n m      ", "p -2 2ac      ", &
     "31:bca        ", "c2v^7         ", "p n 21 m      ", "p -2 -2bc     ", &
     "31:a-cb       ", "c2v^7         ", "p m 21 n      ", "p -2ab -2     ", &
     "32:           ", "c2v^8         ", "p b a 2       ", "p 2 -2ab      ", &
     "32:cab        ", "c2v^8         ", "p 2 c b       ", "p -2bc 2      ", &
     "32:bca        ", "c2v^8         ", "p c 2 a       ", "p -2ac -2ac   ", &
     "33:           ", "c2v^9         ", "p n a 21      ", "p 2c -2n      ", &
     "33:           ", "c2v^9         ", "p n a 21      ", "p 21 -2n      ", &
     "33:ba-c       ", "c2v^9         ", "p b n 21      ", "p 2c -2ab     ", &
     "33:ba-c       ", "c2v^9         ", "p b n 21      ", "p 21 -2ab     ", &
     "33:cab        ", "c2v^9         ", "p 21 n b      ", "p -2bc 2a     ", &
     "33:cab        ", "c2v^9         ", "p 21 n b      ", "p -2bc 21     ", &
     "33:-cba       ", "c2v^9         ", "p 21 c n      ", "p -2n 2a      ", &
     "33:-cba       ", "c2v^9         ", "p 21 c n      ", "p -2n 21      ", &
     "33:bca        ", "c2v^9         ", "p c 21 n      ", "p -2n -2ac    ", &
     "33:a-cb       ", "c2v^9         ", "p n 21 a      ", "p -2ac -2n    "/
   data spacegroup_conversion_table(1:4,211:240)/ &
     "34:           ", "c2v^10        ", "p n n 2       ", "p 2 -2n       ", &
     "34:cab        ", "c2v^10        ", "p 2 n n       ", "p -2n 2       ", &
     "34:bca        ", "c2v^10        ", "p n 2 n       ", "p -2n -2n     ", &
     "35:           ", "c2v^11        ", "c m m 2       ", "c 2 -2        ", &
     "35:cab        ", "c2v^11        ", "a 2 m m       ", "a -2 2        ", &
     "35:bca        ", "c2v^11        ", "b m 2 m       ", "b -2 -2       ", &
     "36:           ", "c2v^12        ", "c m c 21      ", "c 2c -2       ", &
     "36:           ", "c2v^12        ", "c m c 21      ", "c 21 -2       ", &
     "36:ba-c       ", "c2v^12        ", "c c m 21      ", "c 2c -2c      ", &
     "36:ba-c       ", "c2v^12        ", "c c m 21      ", "c 21 -2c      ", &
     "36:cab        ", "c2v^12        ", "a 21 m a      ", "a -2a 2a      ", &
     "36:cab        ", "c2v^12        ", "a 21 m a      ", "a -2a 21      ", &
     "36:-cba       ", "c2v^12        ", "a 21 a m      ", "a -2 2a       ", &
     "36:-cba       ", "c2v^12        ", "a 21 a m      ", "a -2 21       ", &
     "36:bca        ", "c2v^12        ", "b b 21 m      ", "b -2 -2b      ", &
     "36:a-cb       ", "c2v^12        ", "b m 21 b      ", "b -2b -2      ", &
     "37:           ", "c2v^13        ", "c c c 2       ", "c 2 -2c       ", &
     "37:cab        ", "c2v^13        ", "a 2 a a       ", "a -2a 2       ", &
     "37:bca        ", "c2v^13        ", "b b 2 b       ", "b -2b -2b     ", &
     "38:           ", "c2v^14        ", "a m m 2       ", "a 2 -2        ", &
     "38:ba-c       ", "c2v^14        ", "b m m 2       ", "b 2 -2        ", &
     "38:cab        ", "c2v^14        ", "b 2 m m       ", "b -2 2        ", &
     "38:-cba       ", "c2v^14        ", "c 2 m m       ", "c -2 2        ", &
     "38:bca        ", "c2v^14        ", "c m 2 m       ", "c -2 -2       ", &
     "38:a-cb       ", "c2v^14        ", "a m 2 m       ", "a -2 -2       ", &
     "39:           ", "c2v^15        ", "a b m 2       ", "a 2 -2c       ", &
     "39:ba-c       ", "c2v^15        ", "b m a 2       ", "b 2 -2a       ", &
 ! b 2 -2c
     "39:cab        ", "c2v^15        ", "b 2 c m       ", "b -2a 2       ", &
 ! b -2c 2
     "39:-cba       ", "c2v^15        ", "c 2 m b       ", "c -2a 2       ", &
 ! c -2b 2
     "39:bca        ", "c2v^15        ", "c m 2 a       ", "c -2a -2a     "/    ! c -2b -2b
   data spacegroup_conversion_table(1:4,241:270)/ &
     "39:a-cb       ", "c2v^15        ", "a c 2 m       ", "a -2c -2c     ", &
     "40:           ", "c2v^16        ", "a m a 2       ", "a 2 -2a       ", &
     "40:ba-c       ", "c2v^16        ", "b b m 2       ", "b 2 -2b       ", &
     "40:cab        ", "c2v^16        ", "b 2 m b       ", "b -2b 2       ", &
     "40:-cba       ", "c2v^16        ", "c 2 c m       ", "c -2c 2       ", &
     "40:bca        ", "c2v^16        ", "c c 2 m       ", "c -2c -2c     ", &
     "40:a-cb       ", "c2v^16        ", "a m 2 a       ", "a -2a -2a     ", &
     "41:           ", "c2v^17        ", "a b a 2       ", "a 2 -2ab      ", &
 ! a 2 -2ac
     "41:ba-c       ", "c2v^17        ", "b b a 2       ", "b 2 -2ab      ", &
 ! b 2 -2bc
     "41:cab        ", "c2v^17        ", "b 2 c b       ", "b -2ab 2      ", &
 ! b -2bc 2
     "41:-cba       ", "c2v^17        ", "c 2 c b       ", "c -2ac 2      ", &
 ! c -2bc 2
     "41:bca        ", "c2v^17        ", "c c 2 a       ", "c -2ac -2ac   ", &
 ! c -2bc -2bc
     "41:a-cb       ", "c2v^17        ", "a c 2 a       ", "a -2ab -2ab   ", &
 ! a -2ac -2ac
     "42:           ", "c2v^18        ", "f m m 2       ", "f 2 -2        ", &
     "42:cab        ", "c2v^18        ", "f 2 m m       ", "f -2 2        ", &
     "42:bca        ", "c2v^18        ", "f m 2 m       ", "f -2 -2       ", &
     "43:           ", "c2v^19        ", "f d d 2       ", "f 2 -2d       ", &
     "43:cab        ", "c2v^19        ", "f 2 d d       ", "f -2d 2       ", &
     "43:bca        ", "c2v^19        ", "f d 2 d       ", "f -2d -2d     ", &
     "44:           ", "c2v^20        ", "i m m 2       ", "i 2 -2        ", &
     "44:cab        ", "c2v^20        ", "i 2 m m       ", "i -2 2        ", &
     "44:bca        ", "c2v^20        ", "i m 2 m       ", "i -2 -2       ", &
     "45:           ", "c2v^21        ", "i b a 2       ", "i 2 -2c       ", &
     "45:cab        ", "c2v^21        ", "i 2 c b       ", "i -2a 2       ", &
     "45:bca        ", "c2v^21        ", "i c 2 a       ", "i -2b -2b     ", &
     "46:           ", "c2v^22        ", "i m a 2       ", "i 2 -2a       ", &
     "46:ba-c       ", "c2v^22        ", "i b m 2       ", "i 2 -2b       ", &
     "46:cab        ", "c2v^22        ", "i 2 m b       ", "i -2b 2       ", &
     "46:-cba       ", "c2v^22        ", "i 2 c m       ", "i -2c 2       ", &
     "46:bca        ", "c2v^22        ", "i c 2 m       ", "i -2c -2c     "/
   data spacegroup_conversion_table(1:4,271:300)/ &
     "46:a-cb       ", "c2v^22        ", "i m 2 a       ", "i -2a -2a     ", &
     "47            ", "d2h^1         ", "p m m m       ", "-p 2 2        ", &
     "48:1          ", "d2h^2         ", "p n n n:1     ", "p 2 2 -1n     ", &
     "48:2          ", "d2h^2         ", "p n n n:2     ", "-p 2ab 2bc    ", &
     "49:           ", "d2h^3         ", "p c c m       ", "-p 2 2c       ", &
     "49:cab        ", "d2h^3         ", "p m a a       ", "-p 2a 2       ", &
     "49:bca        ", "d2h^3         ", "p b m b       ", "-p 2b 2b      ", &
     "50:1          ", "d2h^4         ", "p b a n:1     ", "p 2 2 -1ab    ", &
     "50:2          ", "d2h^4         ", "p b a n:2     ", "-p 2ab 2b     ", &
     "50:1cab       ", "d2h^4         ", "p n c b:1     ", "p 2 2 -1bc    ", &
     "50:2cab       ", "d2h^4         ", "p n c b:2     ", "-p 2b 2bc     ", &
     "50:1bca       ", "d2h^4         ", "p c n a:1     ", "p 2 2 -1ac    ", &
     "50:2bca       ", "d2h^4         ", "p c n a:2     ", "-p 2a 2c      ", &
     "51:           ", "d2h^5         ", "p m m a       ", "-p 2a 2a      ", &
     "51:ba-c       ", "d2h^5         ", "p m m b       ", "-p 2b 2       ", &
     "51:cab        ", "d2h^5         ", "p b m m       ", "-p 2 2b       ", &
     "51:-cba       ", "d2h^5         ", "p c m m       ", "-p 2c 2c      ", &
     "51:bca        ", "d2h^5         ", "p m c m       ", "-p 2c 2       ", &
     "51:a-cb       ", "d2h^5         ", "p m a m       ", "-p 2 2a       ", &
     "52:           ", "d2h^6         ", "p n n a       ", "-p 2a 2bc     ", &
     "52:ba-c       ", "d2h^6         ", "p n n b       ", "-p 2b 2n      ", &
     "52:cab        ", "d2h^6         ", "p b n n       ", "-p 2n 2b      ", &
     "52:-cba       ", "d2h^6         ", "p c n n       ", "-p 2ab 2c     ", &
     "52:bca        ", "d2h^6         ", "p n c n       ", "-p 2ab 2n     ", &
     "52:a-cb       ", "d2h^6         ", "p n a n       ", "-p 2n 2bc     ", &
     "53:           ", "d2h^7         ", "p m n a       ", "-p 2ac 2      ", &
     "53:ba-c       ", "d2h^7         ", "p n m b       ", "-p 2bc 2bc    ", &
     "53:cab        ", "d2h^7         ", "p b m n       ", "-p 2ab 2ab    ", &
     "53:-cba       ", "d2h^7         ", "p c n m       ", "-p 2 2ac      ", &
     "53:bca        ", "d2h^7         ", "p n c m       ", "-p 2 2bc      "/
   data spacegroup_conversion_table(1:4,301:330)/ &
     "53:a-cb       ", "d2h^7         ", "p m a n       ", "-p 2ab 2      ", &
     "54:           ", "d2h^8         ", "p c c a       ", "-p 2a 2ac     ", &
     "54:ba-c       ", "d2h^8         ", "p c c b       ", "-p 2b 2c      ", &
     "54:cab        ", "d2h^8         ", "p b a a       ", "-p 2a 2b      ", &
     "54:-cba       ", "d2h^8         ", "p c a a       ", "-p 2ac 2c     ", &
     "54:bca        ", "d2h^8         ", "p b c b       ", "-p 2bc 2b     ", &
     "54:a-cb       ", "d2h^8         ", "p b a b       ", "-p 2b 2ab     ", &
     "55:           ", "d2h^9         ", "p b a m       ", "-p 2 2ab      ", &
     "55:cab        ", "d2h^9         ", "p m c b       ", "-p 2bc 2      ", &
     "55:bca        ", "d2h^9         ", "p c m a       ", "-p 2ac 2ac    ", &
     "56:           ", "d2h^10        ", "p c c n       ", "-p 2ab 2ac    ", &
     "56:cab        ", "d2h^10        ", "p n a a       ", "-p 2ac 2bc    ", &
     "56:bca        ", "d2h^10        ", "p b n b       ", "-p 2bc 2ab    ", &
     "57:           ", "d2h^11        ", "p b c m       ", "-p 2c 2b      ", &
     "57:ba-c       ", "d2h^11        ", "p c a m       ", "-p 2c 2ac     ", &
     "57:cab        ", "d2h^11        ", "p m c a       ", "-p 2ac 2a     ", &
     "57:-cba       ", "d2h^11        ", "p m a b       ", "-p 2b 2a      ", &
     "57:bca        ", "d2h^11        ", "p b m a       ", "-p 2a 2ab     ", &
     "57:a-cb       ", "d2h^11        ", "p c m b       ", "-p 2bc 2c     ", &
     "58:           ", "d2h^12        ", "p n n m       ", "-p 2 2n       ", &
     "58:cab        ", "d2h^12        ", "p m n n       ", "-p 2n 2       ", &
     "58:bca        ", "d2h^12        ", "p n m n       ", "-p 2n 2n      ", &
     "59:1          ", "d2h^13        ", "p m m n:1     ", "p 2 2ab -1ab  ", &
     "59:2          ", "d2h^13        ", "p m m n:2     ", "-p 2ab 2a     ", &
     "59:1cab       ", "d2h^13        ", "p n m m:1     ", "p 2bc 2 -1bc  ", &
     "59:2cab       ", "d2h^13        ", "p n m m:2     ", "-p 2c 2bc     ", &
     "59:1bca       ", "d2h^13        ", "p m n m:1     ", "p 2ac 2ac -1ac", &
     "59:2bca       ", "d2h^13        ", "p m n m:2     ", "-p 2c 2a      ", &
     "60:           ", "d2h^14        ", "p b c n       ", "-p 2n 2ab     ", &
     "60:ba-c       ", "d2h^14        ", "p c a n       ", "-p 2n 2c      "/
   data spacegroup_conversion_table(1:4,331:360)/ &
     "60:cab        ", "d2h^14        ", "p n c a       ", "-p 2a 2n      ", &
     "60:-cba       ", "d2h^14        ", "p n a b       ", "-p 2bc 2n     ", &
     "60:bca        ", "d2h^14        ", "p b n a       ", "-p 2ac 2b     ", &
     "60:a-cb       ", "d2h^14        ", "p c n b       ", "-p 2b 2ac     ", &
     "61:           ", "d2h^15        ", "p b c a       ", "-p 2ac 2ab    ", &
     "61:ba-c       ", "d2h^15        ", "p c a b       ", "-p 2bc 2ac    ", &
     "62:           ", "d2h^16        ", "p n m a       ", "-p 2ac 2n     ", &
     "62:ba-c       ", "d2h^16        ", "p m n b       ", "-p 2bc 2a     ", &
     "62:cab        ", "d2h^16        ", "p b n m       ", "-p 2c 2ab     ", &
     "62:-cba       ", "d2h^16        ", "p c m n       ", "-p 2n 2ac     ", &
     "62:bca        ", "d2h^16        ", "p m c n       ", "-p 2n 2a      ", &
     "62:a-cb       ", "d2h^16        ", "p n a m       ", "-p 2c 2n      ", &
     "63:           ", "d2h^17        ", "c m c m       ", "-c 2c 2       ", &
     "63:ba-c       ", "d2h^17        ", "c c m m       ", "-c 2c 2c      ", &
     "63:cab        ", "d2h^17        ", "a m m a       ", "-a 2a 2a      ", &
     "63:-cba       ", "d2h^17        ", "a m a m       ", "-a 2 2a       ", &
     "63:bca        ", "d2h^17        ", "b b m m       ", "-b 2 2b       ", &
     "63:a-cb       ", "d2h^17        ", "b m m b       ", "-b 2b 2       ", &
     "64:           ", "d2h^18        ", "c m c a       ", "-c 2ac 2      ", &
 ! -c 2bc 2
     "64:ba-c       ", "d2h^18        ", "c c m b       ", "-c 2ac 2ac    ", &
 ! -c 2bc 2bc
     "64:cab        ", "d2h^18        ", "a b m a       ", "-a 2ab 2ab    ", &
 ! -a 2ac 2ac
     "64:-cba       ", "d2h^18        ", "a c a m       ", "-a 2 2ab      ", &
 ! -a 2 2ac
     "64:bca        ", "d2h^18        ", "b b c m       ", "-b 2 2ab      ", &
 ! -b 2 2bc
     "64:a-cb       ", "d2h^18        ", "b m a b       ", "-b 2ab 2      ", &
 ! -b 2bc 2
     "65:           ", "d2h^19        ", "c m m m       ", "-c 2 2        ", &
     "65:cab        ", "d2h^19        ", "a m m m       ", "-a 2 2        ", &
     "65:bca        ", "d2h^19        ", "b m m m       ", "-b 2 2        ", &
     "66:           ", "d2h^20        ", "c c c m       ", "-c 2 2c       ", &
     "66:cab        ", "d2h^20        ", "a m a a       ", "-a 2a 2       ", &
     "66:bca        ", "d2h^20        ", "b b m b       ", "-b 2b 2b      "/
   data spacegroup_conversion_table(1:4,361:390)/ &
     "67:           ", "d2h^21        ", "c m m a       ", "-c 2a 2       ", &
 ! -c 2b 2
     "67:ba-c       ", "d2h^21        ", "c m m b       ", "-c 2a 2a      ", &
 ! -c 2b 2b
     "67:cab        ", "d2h^21        ", "a b m m       ", "-a 2b 2b      ", &
 ! -a 2c 2c
     "67:-cba       ", "d2h^21        ", "a c m m       ", "-a 2 2c       ", &
     "67:bca        ", "d2h^21        ", "b m c m       ", "-b 2 2a       ", &
 ! -b 2 2c
     "67:a-cb       ", "d2h^21        ", "b m a m       ", "-b 2a 2       ", &
 ! -b 2c 2
     "68:1          ", "d2h^22        ", "c c c a:1     ", "c 2 2 -1ac    ", &
 ! c 2 2 -1bc
     "68:2          ", "d2h^22        ", "c c c a:2     ", "-c 2a 2ac     ", &
 ! -c 2b 2bc
     "68:1ba-c      ", "d2h^22        ", "c c c b:1     ", "c 2 2 -1ac    ", &
 ! c 2 2 -1bc
     "68:2ba-c      ", "d2h^22        ", "c c c b:2     ", "-c 2a 2c      ", &
 ! -c 2b 2c
     "68:1cab       ", "d2h^22        ", "a b a a:1     ", "a 2 2 -1ab    ", &
 ! a 2 2 -1ac
     "68:2cab       ", "d2h^22        ", "a b a a:2     ", "-a 2a 2c      ", &
     "68:1-cba      ", "d2h^22        ", "a c a a:1     ", "a 2 2 -1ab    ", &
 ! a 2 2 -1ac
     "68:2-cba      ", "d2h^22        ", "a c a a:2     ", "-a 2ab 2b     ", &
 ! -a 2ac 2c
     "68:1bca       ", "d2h^22        ", "b b c b:1     ", "b 2 2 -1ab    ", &
 ! b 2 2 -1bc
     "68:2bca       ", "d2h^22        ", "b b c b:2     ", "-b 2ab 2b     ", &
 ! -b 2bc 2b
     "68:1a-cb      ", "d2h^22        ", "b b a b:1     ", "b 2 2 -1ab    ", &
 ! b 2 2 -1bc
     "68:2a-cb      ", "d2h^22        ", "b b a b:2     ", "-b 2b 2ab     ", &
 ! -b 2b 2bc
     "69            ", "d2h^23        ", "f m m m       ", "-f 2 2        ", &
     "70:1          ", "d2h^24        ", "f d d d:1     ", "f 2 2 -1d     ", &
     "70:2          ", "d2h^24        ", "f d d d:2     ", "-f 2uv 2vw    ", &
     "71            ", "d2h^25        ", "i m m m       ", "-i 2 2        ", &
     "72:           ", "d2h^26        ", "i b a m       ", "-i 2 2c       ", &
     "72:cab        ", "d2h^26        ", "i m c b       ", "-i 2a 2       ", &
     "72:bca        ", "d2h^26        ", "i c m a       ", "-i 2b 2b      ", &
     "73:           ", "d2h^27        ", "i b c a       ", "-i 2b 2c      ", &
     "73:ba-c       ", "d2h^27        ", "i c a b       ", "-i 2a 2b      ", &
     "74:           ", "d2h^28        ", "i m m a       ", "-i 2b 2       ", &
     "74:ba-c       ", "d2h^28        ", "i m m b       ", "-i 2a 2a      ", &
     "74:cab        ", "d2h^28        ", "i b m m       ", "-i 2c 2c      "/
   data spacegroup_conversion_table(1:4,391:420)/ &
     "74:-cba       ", "d2h^28        ", "i c m m       ", "-i 2 2b       ", &
     "74:bca        ", "d2h^28        ", "i m c m       ", "-i 2 2a       ", &
     "74:a-cb       ", "d2h^28        ", "i m a m       ", "-i 2c 2       ", &
     "75            ", "c4^1          ", "p 4           ", "p 4           ", &
     "76:           ", "c4^2          ", "p 41          ", "p 4w          ", &
     "76:           ", "c4^2          ", "p 41          ", "p 41          ", &
     "77:           ", "c4^3          ", "p 42          ", "p 4c          ", &
     "77:           ", "c4^3          ", "p 42          ", "p 42          ", &
     "78:           ", "c4^4          ", "p 43          ", "p 4cw         ", &
     "78:           ", "c4^4          ", "p 43          ", "p 43          ", &
     "79            ", "c4^5          ", "i 4           ", "i 4           ", &
     "80            ", "c4^6          ", "i 41          ", "i 4bw         ", &
     "81            ", "s4^1          ", "p -4          ", "p -4          ", &
     "82            ", "s4^2          ", "i -4          ", "i -4          ", &
     "83            ", "c4h^1         ", "p 4/m         ", "-p 4          ", &
     "84:           ", "c4h^2         ", "p 42/m        ", "-p 4c         ", &
     "84:           ", "c4h^2         ", "p 42/m        ", "-p 42         ", &
     "85:1          ", "c4h^3         ", "p 4/n:1       ", "p 4ab -1ab    ", &
     "85:2          ", "c4h^3         ", "p 4/n:2       ", "-p 4a         ", &
     "86:1          ", "c4h^4         ", "p 42/n:1      ", "p 4n -1n      ", &
     "86:2          ", "c4h^4         ", "p 42/n:2      ", "-p 4bc        ", &
     "87            ", "c4h^5         ", "i 4/m         ", "-i 4          ", &
     "88:1          ", "c4h^6         ", "i 41/a:1      ", "i 4bw -1bw    ", &
     "88:2          ", "c4h^6         ", "i 41/a:2      ", "-i 4ad        ", &
     "89            ", "d4^1          ", "p 4 2 2       ", "p 4 2         ", &
     "90            ", "d4^2          ", "p 4 21 2      ", "p 4ab 2ab     ", &
     "91:           ", "d4^3          ", "p 41 2 2      ", "p 4w 2c       ", &
     "91:           ", "d4^3          ", "p 41 2 2      ", "p 41 2c       ", &
     "92            ", "d4^4          ", "p 41 21 2     ", "p 4abw 2nw    ", &
     "93:           ", "d4^5          ", "p 42 2 2      ", "p 4c 2        "/
   data spacegroup_conversion_table(1:4,421:450)/ &
     "93:           ", "d4^5          ", "p 42 2 2      ", "p 42 2        ", &
     "94            ", "d4^6          ", "p 42 21 2     ", "p 4n 2n       ", &
     "95:           ", "d4^7          ", "p 43 2 2      ", "p 4cw 2c      ", &
     "95:           ", "d4^7          ", "p 43 2 2      ", "p 43 2c       ", &
     "96            ", "d4^8          ", "p 43 21 2     ", "p 4nw 2abw    ", &
     "97            ", "d4^9          ", "i 4 2 2       ", "i 4 2         ", &
     "98            ", "d4^10         ", "i 41 2 2      ", "i 4bw 2bw     ", &
     "99            ", "c4v^1         ", "p 4 m m       ", "p 4 -2        ", &
     "100           ", "c4v^2         ", "p 4 b m       ", "p 4 -2ab      ", &
     "101:          ", "c4v^3         ", "p 42 c m      ", "p 4c -2c      ", &
     "101:          ", "c4v^3         ", "p 42 c m      ", "p 42 -2c      ", &
     "102           ", "c4v^4         ", "p 42 n m      ", "p 4n -2n      ", &
     "103           ", "c4v^5         ", "p 4 c c       ", "p 4 -2c       ", &
     "104           ", "c4v^6         ", "p 4 n c       ", "p 4 -2n       ", &
     "105:          ", "c4v^7         ", "p 42 m c      ", "p 4c -2       ", &
     "105:          ", "c4v^7         ", "p 42 m c      ", "p 42 -2       ", &
     "106:          ", "c4v^8         ", "p 42 b c      ", "p 4c -2ab     ", &
     "106:          ", "c4v^8         ", "p 42 b c      ", "p 42 -2ab     ", &
     "107           ", "c4v^9         ", "i 4 m m       ", "i 4 -2        ", &
     "108           ", "c4v^10        ", "i 4 c m       ", "i 4 -2c       ", &
     "109           ", "c4v^11        ", "i 41 m d      ", "i 4bw -2      ", &
     "110           ", "c4v^12        ", "i 41 c d      ", "i 4bw -2c     ", &
     "111           ", "d2d^1         ", "p -4 2 m      ", "p -4 2        ", &
     "112           ", "d2d^2         ", "p -4 2 c      ", "p -4 2c       ", &
     "113           ", "d2d^3         ", "p -4 21 m     ", "p -4 2ab      ", &
     "114           ", "d2d^4         ", "p -4 21 c     ", "p -4 2n       ", &
     "115           ", "d2d^5         ", "p -4 m 2      ", "p -4 -2       ", &
     "116           ", "d2d^6         ", "p -4 c 2      ", "p -4 -2c      ", &
     "117           ", "d2d^7         ", "p -4 b 2      ", "p -4 -2ab     ", &
     "118           ", "d2d^8         ", "p -4 n 2      ", "p -4 -2n      "/
   data spacegroup_conversion_table(1:4,451:480)/ &
     "119           ", "d2d^9         ", "i -4 m 2      ", "i -4 -2       ", &
     "120           ", "d2d^10        ", "i -4 c 2      ", "i -4 -2c      ", &
     "121           ", "d2d^11        ", "i -4 2 m      ", "i -4 2        ", &
     "122           ", "d2d^12        ", "i -4 2 d      ", "i -4 2bw      ", &
     "123           ", "d4h^1         ", "p 4/m m m     ", "-p 4 2        ", &
     "124           ", "d4h^2         ", "p 4/m c c     ", "-p 4 2c       ", &
     "125:1         ", "d4h^3         ", "p 4/n b m:1   ", "p 4 2 -1ab    ", &
     "125:2         ", "d4h^3         ", "p 4/n b m:2   ", "-p 4a 2b      ", &
     "126:1         ", "d4h^4         ", "p 4/n n c:1   ", "p 4 2 -1n     ", &
     "126:2         ", "d4h^4         ", "p 4/n n c:2   ", "-p 4a 2bc     ", &
     "127           ", "d4h^5         ", "p 4/m b m     ", "-p 4 2ab      ", &
     "128           ", "d4h^6         ", "p 4/m n c     ", "-p 4 2n       ", &
     "129:1         ", "d4h^7         ", "p 4/n m m:1   ", "p 4ab 2ab -1ab", &
     "129:2         ", "d4h^7         ", "p 4/n m m:2   ", "-p 4a 2a      ", &
     "130:1         ", "d4h^8         ", "p 4/n c c:1   ", "p 4ab 2n -1ab ", &
     "130:2         ", "d4h^8         ", "p 4/n c c:2   ", "-p 4a 2ac     ", &
     "131           ", "d4h^9         ", "p 42/m m c    ", "-p 4c 2       ", &
     "132           ", "d4h^10        ", "p 42/m c m    ", "-p 4c 2c      ", &
     "133:1         ", "d4h^11        ", "p 42/n b c:1  ", "p 4n 2c -1n   ", &
     "133:2         ", "d4h^11        ", "p 42/n b c:2  ", "-p 4ac 2b     ", &
     "134:1         ", "d4h^12        ", "p 42/n n m:1  ", "p 4n 2 -1n    ", &
     "134:2         ", "d4h^12        ", "p 42/n n m:2  ", "-p 4ac 2bc    ", &
     "135:          ", "d4h^13        ", "p 42/m b c    ", "-p 4c 2ab     ", &
     "135:          ", "d4h^13        ", "p 42/m b c    ", "-p 42 2ab     ", &
     "136           ", "d4h^14        ", "p 42/m n m    ", "-p 4n 2n      ", &
     "137:1         ", "d4h^15        ", "p 42/n m c:1  ", "p 4n 2n -1n   ", &
     "137:2         ", "d4h^15        ", "p 42/n m c:2  ", "-p 4ac 2a     ", &
     "138:1         ", "d4h^16        ", "p 42/n c m:1  ", "p 4n 2ab -1n  ", &
     "138:2         ", "d4h^16        ", "p 42/n c m:2  ", "-p 4ac 2ac    ", &
     "139           ", "d4h^17        ", "i 4/m m m     ", "-i 4 2        "/
   data spacegroup_conversion_table(1:4,481:510)/ &
     "140           ", "d4h^18        ", "i 4/m c m     ", "-i 4 2c       ", &
     "141:1         ", "d4h^19        ", "i 41/a m d:1  ", "i 4bw 2bw -1bw", &
     "141:2         ", "d4h^19        ", "i 41/a m d:2  ", "-i 4bd 2      ", &
     "142:1         ", "d4h^20        ", "i 41/a c d:1  ", "i 4bw 2aw -1bw", &
     "142:2         ", "d4h^20        ", "i 41/a c d:2  ", "-i 4bd 2c     ", &
     "143           ", "c3^1          ", "p 3           ", "p 3           ", &
     "144           ", "c3^2          ", "p 31          ", "p 31          ", &
     "145           ", "c3^3          ", "p 32          ", "p 32          ", &
     "146:h         ", "c3^4          ", "r 3:h         ", "r 3           ", &
     "146:r         ", "c3^4          ", "r 3:r         ", "p 3*          ", &
     "147           ", "c3i^1         ", "p -3          ", "-p 3          ", &
     "148:h         ", "c3i^2         ", "r -3:h        ", "-r 3          ", &
     "148:r         ", "c3i^2         ", "r -3:r        ", "-p 3*         ", &
     "149           ", "d3^1          ", "p 3 1 2       ", "p 3 2         ", &
     "150           ", "d3^2          ", "p 3 2 1       ", "p 3 2""       ", &
     "151           ", "d3^3          ", "p 31 1 2      ", "p 31 2 (0 0 4)", &
 ! p 31 2c (0 0 1)
     "152           ", "d3^4          ", "p 31 2 1      ", "p 31 2""      ", &
     "153           ", "d3^5          ", "p 32 1 2      ", "p 32 2 (0 0 2)", &
 ! p 32 2c (0 0 -1)
     "154           ", "d3^6          ", "p 32 2 1      ", "p 32 2""      ", &
     "155:h         ", "d3^7          ", "r 3 2:h       ", "r 3 2""       ", &
     "155:r         ", "d3^7          ", "r 3 2:r       ", "p 3* 2        ", &
     "156           ", "c3v^1         ", "p 3 m 1       ", "p 3 -2""      ", &
     "157           ", "c3v^2         ", "p 3 1 m       ", "p 3 -2        ", &
     "158           ", "c3v^3         ", "p 3 c 1       ", "p 3 -2""c     ", &
     "159           ", "c3v^4         ", "p 3 1 c       ", "p 3 -2c       ", &
     "160:h         ", "c3v^5         ", "r 3 m:h       ", "r 3 -2""      ", &
     "160:r         ", "c3v^5         ", "r 3 m:r       ", "p 3* -2       ", &
     "161:h         ", "c3v^6         ", "r 3 c:h       ", "r 3 -2""c     ", &
     "161:r         ", "c3v^6         ", "r 3 c:r       ", "p 3* -2n      ", &
     "162           ", "d3d^1         ", "p -3 1 m      ", "-p 3 2        "/
   data spacegroup_conversion_table(1:4,511:540)/ &
     "163           ", "d3d^2         ", "p -3 1 c      ", "-p 3 2c       ", &
     "164           ", "d3d^3         ", "p -3 m 1      ", "-p 3 2""      ", &
     "165           ", "d3d^4         ", "p -3 c 1      ", "-p 3 2""c     ", &
     "166:h         ", "d3d^5         ", "r -3 m:h      ", "-r 3 2""      ", &
     "166:r         ", "d3d^5         ", "r -3 m:r      ", "-p 3* 2       ", &
     "167:h         ", "d3d^6         ", "r -3 c:h      ", "-r 3 2""c     ", &
     "167:r         ", "d3d^6         ", "r -3 c:r      ", "-p 3* 2n      ", &
     "168           ", "c6^1          ", "p 6           ", "p 6           ", &
     "169           ", "c6^2          ", "p 61          ", "p 61          ", &
     "170           ", "c6^3          ", "p 65          ", "p 65          ", &
     "171           ", "c6^4          ", "p 62          ", "p 62          ", &
     "172           ", "c6^5          ", "p 64          ", "p 64          ", &
     "173:          ", "c6^6          ", "p 63          ", "p 6c          ", &
     "173:          ", "c6^6          ", "p 63          ", "p 63          ", &
     "174           ", "c3h^1         ", "p -6          ", "p -6          ", &
     "175           ", "c6h^1         ", "p 6/m         ", "-p 6          ", &
     "176:          ", "c6h^2         ", "p 63/m        ", "-p 6c         ", &
     "176:          ", "c6h^2         ", "p 63/m        ", "-p 63         ", &
     "177           ", "d6^1          ", "p 6 2 2       ", "p 6 2         ", &
     "178           ", "d6^2          ", "p 61 2 2      ", "p 61 2 (0 0 5)", &
 ! p 61 2 (0 0 -1)
     "179           ", "d6^3          ", "p 65 2 2      ", "p 65 2 (0 0 1)", &
     "180           ", "d6^4          ", "p 62 2 2      ", "p 62 2 (0 0 4)", &
 ! p 62 2c (0 0 1)
     "181           ", "d6^5          ", "p 64 2 2      ", "p 64 2 (0 0 2)", &
 ! p 64 2c (0 0 -1)
     "182:          ", "d6^6          ", "p 63 2 2      ", "p 6c 2c       ", &
     "182:          ", "d6^6          ", "p 63 2 2      ", "p 63 2c       ", &
     "183           ", "c6v^1         ", "p 6 m m       ", "p 6 -2        ", &
     "184           ", "c6v^2         ", "p 6 c c       ", "p 6 -2c       ", &
     "185:          ", "c6v^3         ", "p 63 c m      ", "p 6c -2       ", &
     "185:          ", "c6v^3         ", "p 63 c m      ", "p 63 -2       ", &
     "186:          ", "c6v^4         ", "p 63 m c      ", "p 6c -2c      "/
   data spacegroup_conversion_table(1:4,541:570)/ &
     "186:          ", "c6v^4         ", "p 63 m c      ", "p 63 -2c      ", &
     "187           ", "d3h^1         ", "p -6 m 2      ", "p -6 2        ", &
     "188           ", "d3h^2         ", "p -6 c 2      ", "p -6c 2       ", &
     "189           ", "d3h^3         ", "p -6 2 m      ", "p -6 -2       ", &
     "190           ", "d3h^4         ", "p -6 2 c      ", "p -6c -2c     ", &
     "191           ", "d6h^1         ", "p 6/m m m     ", "-p 6 2        ", &
     "192           ", "d6h^2         ", "p 6/m c c     ", "-p 6 2c       ", &
     "193:          ", "d6h^3         ", "p 63/m c m    ", "-p 6c 2       ", &
     "193:          ", "d6h^3         ", "p 63/m c m    ", "-p 63 2       ", &
     "194:          ", "d6h^4         ", "p 63/m m c    ", "-p 6c 2c      ", &
     "194:          ", "d6h^4         ", "p 63/m m c    ", "-p 63 2c      ", &
     "195           ", "t^1           ", "p 2 3         ", "p 2 2 3       ", &
     "196           ", "t^2           ", "f 2 3         ", "f 2 2 3       ", &
     "197           ", "t^3           ", "i 2 3         ", "i 2 2 3       ", &
     "198           ", "t^4           ", "p 21 3        ", "p 2ac 2ab 3   ", &
     "199           ", "t^5           ", "i 21 3        ", "i 2b 2c 3     ", &
     "200           ", "th^1          ", "p m -3        ", "-p 2 2 3      ", &
     "201:1         ", "th^2          ", "p n -3:1      ", "p 2 2 3 -1n   ", &
     "201:2         ", "th^2          ", "p n -3:2      ", "-p 2ab 2bc 3  ", &
     "202           ", "th^3          ", "f m -3        ", "-f 2 2 3      ", &
     "203:1         ", "th^4          ", "f d -3:1      ", "f 2 2 3 -1d   ", &
     "203:2         ", "th^4          ", "f d -3:2      ", "-f 2uv 2vw 3  ", &
     "204           ", "th^5          ", "i m -3        ", "-i 2 2 3      ", &
     "205           ", "th^6          ", "p a -3        ", "-p 2ac 2ab 3  ", &
     "206           ", "th^7          ", "i a -3        ", "-i 2b 2c 3    ", &
     "207           ", "o^1           ", "p 4 3 2       ", "p 4 2 3       ", &
     "208           ", "o^2           ", "p 42 3 2      ", "p 4n 2 3      ", &
     "209           ", "o^3           ", "f 4 3 2       ", "f 4 2 3       ", &
     "210           ", "o^4           ", "f 41 3 2      ", "f 4d 2 3      ", &
     "211           ", "o^5           ", "i 4 3 2       ", "i 4 2 3       "/
   data spacegroup_conversion_table(1:4,571:593)/ &
     "212           ", "o^6           ", "p 43 3 2      ", "p 4acd 2ab 3  ", &
     "213           ", "o^7           ", "p 41 3 2      ", "p 4bd 2ab 3   ", &
     "214           ", "o^8           ", "i 41 3 2      ", "i 4bd 2c 3    ", &
     "215           ", "td^1          ", "p -4 3 m      ", "p -4 2 3      ", &
     "216           ", "td^2          ", "f -4 3 m      ", "f -4 2 3      ", &
     "217           ", "td^3          ", "i -4 3 m      ", "i -4 2 3      ", &
     "218           ", "td^4          ", "p -4 3 n      ", "p -4n 2 3     ", &
     "219           ", "td^5          ", "f -4 3 c      ", "f -4a 2 3     ", &
 ! f -4c 2 3
     "220           ", "td^6          ", "i -4 3 d      ", "i -4bd 2c 3   ", &
     "221           ", "oh^1          ", "p m -3 m      ", "-p 4 2 3      ", &
     "222:1         ", "oh^2          ", "p n -3 n:1    ", "p 4 2 3 -1n   ", &
     "222:2         ", "oh^2          ", "p n -3 n:2    ", "-p 4a 2bc 3   ", &
     "223           ", "oh^3          ", "p m -3 n      ", "-p 4n 2 3     ", &
     "224:1         ", "oh^4          ", "p n -3 m:1    ", "p 4n 2 3 -1n  ", &
     "224:2         ", "oh^4          ", "p n -3 m:2    ", "-p 4bc 2bc 3  ", &
     "225           ", "oh^5          ", "f m -3 m      ", "-f 4 2 3      ", &
     "226           ", "oh^6          ", "f m -3 c      ", "-f 4a 2 3     ", &
 ! -f 4c 2 3
     "227:1         ", "oh^7          ", "f d -3 m:1    ", "f 4d 2 3 -1d  ", &
     "227:2         ", "oh^7          ", "f d -3 m:2    ", "-f 4vw 2vw 3  ", &
     "228:1         ", "oh^8          ", "f d -3 c:1    ", "f 4d 2 3 -1ad ", &
 ! f 4d 2 3 -1cd
     "228:2         ", "oh^8          ", "f d -3 c:2    ", "-f 4ud 2vw 3  ", &
 ! -f 4cvw 2vw 3
     "229           ", "oh^9          ", "i m -3 m      ", "-i 4 2 3      ", &
     "230           ", "oh^10         ", "i a -3 d      ", "-i 4bd 2c 3   "/

contains

   subroutine create(self)
    type(spacegroup_type) :: self
    ! Create an object
      pointer :: self
    ! The following code is inherited from OBJECT

      nullify(self)
      allocate(self)

      call nullify_ptr_part_(self)
      call set_defaults_(self)

   end subroutine

   subroutine destroy(self)
    type(spacegroup_type) :: self
    ! Destroy an object
      pointer :: self
    ! The following code is inherited from OBJECT

      if (associated(self)) then
        call destroy_ptr_part_(self)
        deallocate(self)

      end if

   end subroutine

   subroutine nullify_ptr_part(self)
    type(spacegroup_type) :: self
    ! Nullify the pointer parts of object

      nullify(self%seitz)
      nullify(self%unique_symop)
      nullify(self%map_to_unique)

   end subroutine

   subroutine destroy_ptr_part(self)
    type(spacegroup_type) :: self
    ! Destroy the pointer parts of object

      call destroy_(self%seitz)
      call destroy_(self%unique_symop)
      call destroy_(self%map_to_unique)

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

   subroutine create_copy(self,object)
    type(spacegroup_type) :: self
    ! Create a copy of object
     type(spacegroup_type) :: object
     pointer :: self
    ! The following code is inherited from OBJECT

     call create_(self)
     call copy_(self,object)

   end subroutine

   subroutine copy(self,sg)
    type(spacegroup_type) :: self
    ! Make a copy of "sg"
     type(spacegroup_type) :: sg

     self = sg
     if (associated(sg%seitz)) &
        call create_copy_(self%seitz,sg%seitz)
     if (associated(sg%unique_symop)) &
        call create_copy_(self%unique_symop,sg%unique_symop)
     if (associated(sg%map_to_unique)) &
        call create_copy_(self%map_to_unique,sg%map_to_unique)

   end subroutine

   subroutine set_defaults(self)
    type(spacegroup_type) :: self
    ! Set default spacegroup

      self%IT_symbol          = "?"
      self%IT_group_number    = 0
      self%Hall_symbol        = "?"
      self%HM_symbol          = "?"
      self%Schoenflies_symbol = "?"
      self%lattice_symbol     = "?"
      self%lattice_type       = "?"
      self%analysed = .false.

   end subroutine

   subroutine set_IT_symbol(self,IT_symbol)
    type(spacegroup_type) :: self
    ! Set the international table symbol to be "IT_symbol"
      character(*) :: IT_symbol
      character(128) :: symbol
      integer(kind=kind(1)) :: ind,pos

      symbol = IT_symbol
      call to_lower_case_(symbol)
      if (any(symbol==spacegroup_conversion_table(1,:))) then
         ind = index_of_(spacegroup_conversion_table(1,:),symbol)
         self%IT_symbol = trim_blanks_from_start_(symbol)
      else
         ind = index_of_first_that_includes_(spacegroup_conversion_table(1,:),trim(symbol))
         call ensure_(tonto,ind>0,"SPACEGROUP:set_IT_symbol ... Symbol "//trim(symbol)//" does not match any generic IT symbo&
&l")
         self%IT_symbol = spacegroup_conversion_table(1,ind)
         call warn_(tonto,"SPACEGROUP:set_IT_symbol ... Symbol "//trim(symbol)//" does not match any specific IT symbol")
         call warn_(tonto,"SPACEGROUP:set_IT_symbol ... Using symbol "//trim(self%IT_symbol))
         call warn_(tonto,"SPACEGROUP:set_IT_symbol ... Other matching symbols are:")
         call put_matching_IT_symbols_(self,symbol)
      end if
      pos = index(self%IT_symbol,":")
      if (pos==0) then; self%IT_group_number = to_int_(self%IT_symbol)
      else;             self%IT_group_number = to_int_(self%IT_symbol(1:pos-1))
      end if
      self%Schoenflies_symbol = spacegroup_conversion_table(2,ind)
      self%HM_symbol          = spacegroup_conversion_table(3,ind)
      self%Hall_symbol        = spacegroup_conversion_table(4,ind)
      self%analysed = .false.

   end subroutine

   subroutine set_HM_symbol(self,HM_symbol)
    type(spacegroup_type) :: self
    ! Set the Herman-Mauguin symbol to be "HM_symbol"
      character(*) :: HM_symbol
      character(128) :: symbol
      integer(kind=kind(1)) :: ind,pos

      self%HM_symbol = trim_blanks_from_start_(HM_symbol)
      if (index(trim(self%HM_symbol),"_")/=0) &
         call replace_(self%HM_symbol,"_"," ")  ! Replace underscores with spaces
      symbol = self%HM_symbol
      call to_lower_case_(symbol)
      if (index(trim(symbol)," ")==0) then  ! Translate concatenated HM symbols
         call separate_all_characters_(symbol)
         call replace_(symbol,"- ","-")
         call remove_(symbol," ( ")
         call remove_(symbol," )")
         call replace_(symbol," / ","/")
      end if
      if (any(symbol==spacegroup_conversion_table(3,:))) then
         ind = index_of_(spacegroup_conversion_table(3,:),symbol)
         self%HM_symbol = trim_blanks_from_start_(symbol)
      else
         ind = index_of_first_that_includes_(spacegroup_conversion_table(3,:),trim(symbol))
         call ensure_(tonto,ind>0,"SPACEGROUP:set_HM_symbol ... Symbol "//trim(symbol)//" does not match any HM symbol")
         self%HM_symbol = spacegroup_conversion_table(3,ind)
         call warn_(tonto,"SPACEGROUP:set_HM_symbol ... Symbol "//trim(symbol)//" does not match any specific HM symbol")
         call warn_(tonto,"SPACEGROUP:set_HM_symbol ... Using symbol "//trim(self%HM_symbol))
         call warn_(tonto,"SPACEGROUP:set_HM_symbol ... Other specific symbols are:")
         call put_matching_HM_symbols_(self,symbol)
      end if
      self%IT_symbol          = spacegroup_conversion_table(1,ind)
      pos = index(self%IT_symbol,":")
      if (pos==0) then; self%IT_group_number = to_int_(self%IT_symbol)
      else;             self%IT_group_number = to_int_(self%IT_symbol(1:pos-1))
      end if
      self%Schoenflies_symbol = spacegroup_conversion_table(2,ind)
      self%Hall_symbol        = spacegroup_conversion_table(4,ind)
      self%analysed = .false.

   end subroutine

   subroutine set_Hall_symbol(self,Hall_symbol)
    type(spacegroup_type) :: self
    ! Set the Hall symbol to be "Hall_symbol"
      character(*) :: Hall_symbol
      character(128) :: symbol
      integer(kind=kind(1)) :: ind,pos

      self%Hall_symbol = trim_blanks_from_start_(Hall_symbol)
      if (index(trim(self%Hall_symbol),"_")/=0) &
         call replace_(self%Hall_symbol,"_"," ")  ! Replace underscores with spaces
      symbol = self%Hall_symbol
      call to_lower_case_(symbol)
      if (any(symbol==spacegroup_conversion_table(4,:))) then
         ind = index_of_(spacegroup_conversion_table(4,:),symbol)
         self%IT_symbol          = spacegroup_conversion_table(1,ind)
         pos = index(self%IT_symbol,":")
         if (pos==0) then; self%IT_group_number = to_int_(self%IT_symbol)
         else;             self%IT_group_number = to_int_(self%IT_symbol(1:pos-1))
         end if
         self%Schoenflies_symbol = spacegroup_conversion_table(2,ind)
         self%HM_symbol          = spacegroup_conversion_table(3,ind)
      else
         call warn_(tonto,"SPACEGROUP:set_Hall_symbol ... Non-standard Hall symbol, "//trim(symbol))
         self%IT_symbol          = "?"
         self%IT_group_number    = 0
         self%Schoenflies_symbol = "?"
         self%HM_symbol          = "?"
      end if
      self%analysed = .false.

   end subroutine

!  *************
!  Input methods
!  *************

   recursive subroutine read_keywords(self)
    type(spacegroup_type) :: self
    ! Read data from "stdin" using keyword style input.
      character(128) :: word

      call ensure_(tonto,next_item_(stdin)=="{","SPACEGROUP:read_keywords ... expecting an open bracket symbol, {")
      call read_(stdin,word)
      read_loop: do              ! Loop over keywords
         call read_(stdin,word)
         if (word=="}")         exit read_loop
         if (reverted_(stdin))    exit read_loop
         call process_keyword_(self,word)
      end do read_loop
      call analyse_(self)

   end subroutine

   subroutine process_keyword(self,keyword)
    type(spacegroup_type) :: self
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
            case ("}                      ")   ! exit case
            case ("analyse                "); call analyse_(self)
            case ("hall_symbol=           "); call read_Hall_symbol_(self)
            case ("hermann_mauguin_symbol="); call read_HM_symbol_(self)
            case ("hm_symbol=             "); call read_HM_symbol_(self)
            case ("it_symbol=             "); call read_IT_symbol_(self)
            case ("put                    "); call put_(self)
            case  default ;        allocate(tonto%known_keywords(7))
            tonto%known_keywords(1) = "}                      "
            tonto%known_keywords(2) = "analyse                "
            tonto%known_keywords(3) = "hall_symbol=           "
            tonto%known_keywords(4) = "hermann_mauguin_symbol="
            tonto%known_keywords(5) = "hm_symbol=             "
            tonto%known_keywords(6) = "it_symbol=             "
            tonto%known_keywords(7) = "put                    "
            call unknown_(tonto,word,"SPACEGROUP:process_keyword")
            deallocate(tonto%known_keywords)
         end select
      end if

   end subroutine

   subroutine read_IT_symbol(self)
    type(spacegroup_type) :: self
    ! Read the internation table symbol
      character(128) :: symbol

      call read_(stdin,symbol)
      call set_IT_symbol_(self,symbol)

   end subroutine

   subroutine read_Hall_symbol(self)
    type(spacegroup_type) :: self
    ! Read the Hall symbol
      character(128) :: symbol

      call read_(stdin,symbol)
      call set_Hall_symbol_(self,symbol)

   end subroutine

   subroutine read_HM_symbol(self)
    type(spacegroup_type) :: self
    ! Read the Hermann-Mauguin symbol
      character(128) :: symbol

      call read_(stdin,symbol)
      call set_HM_symbol_(self,symbol)

   end subroutine

   subroutine read_CIF(self,cif)
    type(spacegroup_type) :: self
    ! Read information from a type(cif_type) file, "cif"
      type(cif_type) :: cif
      logical(kind=kind(.true.)) :: found
      character(128) :: symbol
      character(128), dimension(:), pointer :: jf_symbols,itemvec
      real(kind=kind(1.0d0)), dimension(:,:,:), pointer :: mat

      self%Hall_symbol = " "
      self%HM_symbol = " "
      call find_item_(cif,"_symmetry_space_group_name_Hall",found)
      if (found) then      ! Read the Hall symbol
         call read_item_(cif,"_symmetry_space_group_name_Hall",symbol,itemvec)
         if (symbol(1:1)/="?") then
            call set_Hall_symbol_(self,symbol)
            call ensure_(tonto,.not. associated(itemvec),"SPACEGROUP:read_CIF ... multi-line Hall symbol")
         else              ! "?" is not a valid symbol
            found = .false.
         end if
      end if
      if (.not. found) then  ! OK, look for a HM symbol only if Hall not there ...
         call find_item_(cif,"_symmetry_space_group_name_H-M",found)
         if (found) then   ! Read the Hermann-Mauguin symbol
            call read_item_(cif,"_symmetry_space_group_name_H-M",symbol,itemvec)
            if (symbol(1:1)/="?") then
               call set_HM_symbol_(self,symbol)
               call ensure_(tonto,.not. associated(itemvec),"SPACEGROUP:read_CIF ... multi-line HM symbol")
            else
               found = .false.
            end if
         end if
      end if
      nullify(jf_symbols)
      call find_looped_item_(cif,"_symmetry_equiv_pos_as_xyz",found)
      if (found) then      ! Always try and read the Jones-Faithful symbol
         call read_looped_item_(cif,"_symmetry_equiv_pos_as_xyz",jf_symbols)
         call create_(mat,4,4,size(jf_symbols))
         call decode_Jones_Faithful_symbols_(self,jf_symbols,mat)
         call destroy_(jf_symbols)
      end if
       ! Analyse
      if (self%Hall_symbol/=" " .or. self%HM_symbol/=" ") then
         call analyse_(self)
         if (associated(mat)) then
           call ensure_(tonto,seitz_same_as_(self,mat),"SPACEGROUP:read_CIF ... inconsistent seitz matrices")
         end if
         call destroy_(mat)
      else if (associated(mat)) then
         self%seitz => mat
         self%n_seitz = size(mat,3)
      else
         call die_(tonto,"SPACEGROUP:read_CIF ... Can't find spacegroup information")
      end if

   end subroutine

!  ***********************
!  Spacegroup construction
!  ***********************

   subroutine analyse(self)
    type(spacegroup_type) :: self
    ! Analyse the spacegroup symbol

      call destroy_ptr_part_(self)
      call decode_Hall_symbol_(self)
      call set_lattice_type_(self)
      call set_default_axis_symbol_info_(self)
      call set_spacegroup_order_(self)
      call create_(self%seitz,4,4,self%n_seitz)
      self%seitz = 0.0d0
      call form_L_seitz_(self)
      call form_S_seitz_(self)
      call generate_seitz_(self)
      call shift_origin_(self)
      self%analysed = .true.

   end subroutine

   subroutine decode_Hall_symbol(self)
    type(spacegroup_type) :: self
    ! Decode the Hall notation symbol for the spacegroup
      type(buffer_type) :: buffer
      integer(kind=kind(1)) :: axis,i
      character(1) :: gen,latt
      character(128) :: symbol,lattice,generator,as
      logical(kind=kind(.true.)) :: impossible,alpha_used

      symbol = self%Hall_symbol
      call to_lower_case_(symbol)
      call set_(buffer,symbol)
      call to_lower_case_(buffer%string)
       ! Look for lattice symbol, and (possible) inversion
      call get_item_(buffer,lattice)
      if (lattice(1:1)==bar_symbol) then; self%centrosymmetric = .true.
      else;                               self%centrosymmetric = .false.
      end if
      if (self%centrosymmetric) lattice = lattice(2:)
      call to_upper_case_(lattice)
      latt = lattice(1:1)
      call ensure_(tonto,len_trim(lattice)==1,"SPACEGROUP:decode_Hall_symbol ... unknown lattice, "//lattice)
      call ensure_(tonto,includes_(lattice_symbols,latt),"SPACEGROUP:decode_Hall_symbol ... unknown lattice, "//latt)
      self%lattice_symbol = lattice
      self%lattice_symbol_index = index(lattice_symbols,latt)
       ! Analyse the crystal axis "N" symbols
      self%has_axis_bar       = .false.
      self%axis_order         = 1
      self%axis_symbol        = "_"                 ! *** this means a blank
      self%axis_symbol_index  = 0
      self%has_translation    = .false.
      self%translation_symbol = "_"                 ! *** this means a blank
      self%origin_shift       = 0
      do axis = 1,3                             ! Loop over crystal axis generators
         if (exhausted_(buffer)) then;   return; end if ! No more
         call get_item_(buffer,generator)
         gen = generator(1:1)
         if (gen=="(") exit                     ! explicit origin present
          ! Check for bar symbol
         if (gen==bar_symbol) then
            self%has_axis_bar(axis) = .true.
            generator = generator(2:); gen = generator(1:1)
         end if
         call ensure_(tonto,generator/=" ","SPACEGROUP:decode_Hall_symbol ... missing axis order symbol")
         call ensure_(tonto,includes_(axis_order_symbols,gen),"SPACEGROUP:decode_Hall_symbol ... unknown axis order, "//gen)
          ! Get axis order
         self%axis_order(axis) = index(axis_order_symbols,gen)
         generator = generator(2:); gen = generator(1:1)
         if (generator==" ") cycle              ! Get another axis generator
          ! Check for axis symbols: x,y,z,',"
         if (includes_(axis_symbols,gen)) then
            as = gen
            self%axis_symbol(axis) = as
            self%axis_symbol_index(axis) = index(axis_symbols,gen)
            impossible = (as==dash_symbol .or. as==double_dash_symbol) .and. &
                         (self%axis_order(axis)/=2 .or. axis==1)
            call die_if_(tonto,impossible,"SPACEGROUP:decode_Hall_symbol ... impossible axis setting")  ! see set_axis_symbol_info
            impossible = as==star_symbol .and. self%axis_order(axis)/=3
            call die_if_(tonto,impossible,"SPACEGROUP:decode_Hall_symbol ... impossible axis setting")
            generator = generator(2:); gen = generator(1:1)
            if (generator==" ") cycle           ! Get another axis generator
         end if
          ! Check for translation symbols: a,b,c,u,v,w,n and 1 .. 6
         self%has_translation(axis) = .true.
         do i = 1,3                             ! At most 3 translation symbols ...
            if (gen==" ") then
               exit
            else if (includes_(alpha_translation_symbols,gen)) then
               self%translation_symbol(i,axis) = gen
               generator = generator(2:); gen = generator(1:1)
               alpha_used = .true.
            else if (includes_(number_translation_symbols,gen)) then
               call warn_if_(tonto,alpha_used,"SPACEGROUP:decode_Hall_symbol ... mixed alphabetical/numeric translation symbo&
&ls")
               self%translation_symbol(i,axis) = gen
               generator = generator(2:); gen = generator(1:1)
            else if (gen/=" ") then
               call die_(tonto,"SPACEGROUP:decode_Hall_symbol ... Unknown translation symbol, "//gen)
            end if
         end do
      end do
      if (exhausted_(buffer)) then;   return; end if ! No more
       ! Extract change of origin vector
      call get_int_(buffer,self%origin_shift(1))
      call get_int_(buffer,self%origin_shift(2))
      call get_int_(buffer,self%origin_shift(3))
      call get_item_(buffer,generator)
      call ensure_(tonto,generator==")","SPACEGROUP:decode_Hall_symbol ... missing origin-shift closing parenthesis")
      call ensure_(tonto,exhausted_(buffer),"SPACEGROUP:decode_Hall_symbol ... extra items at end of Hall symbol")

   end subroutine

   subroutine set_lattice_type(self)
    type(spacegroup_type) :: self
    ! Set the lattice type name

   call ensure_(tonto,all_in_range_(self%axis_order,(/1,6/)),"SPACEGROUP:set_lattice_type ... wrong axis orders")
      if (self%axis_order(3)==3) then;                            self%lattice_type = "Cubic"
      else if (self%axis_order(1)==4 .and. self%axis_order(3)/=3) then; self%lattice_type = "Tetragonal"
      else if (self%axis_order(1)==2 .and. self%axis_order(2)==2) then; self%lattice_type = "Orthorhombic"
      else if (self%axis_order(1)==2 .and. self%axis_order(2)==1) then; self%lattice_type = "Monoclinic"
      else if (self%axis_order(1)==6) then;                       self%lattice_type = "Hexagonal"
      else if (self%axis_order(1)==3) then;                       self%lattice_type = "Trigonal"
      else if (self%axis_order(1)==1) then;                       self%lattice_type = "Triclinic"
      end if

   end subroutine

   subroutine set_default_axis_symbol_info(self)
    type(spacegroup_type) :: self
    ! Set the default axis symbols and their associated indices (if needed).
    ! .axis_symbol_index(i) = 1,2,3  means a,b,c or x,y,z
    ! .axis_symbol_index(i) = 4      means a+b or "
    ! .axis_symbol_index(i) = 5      means a-b or '
    ! .axis_symbol_index(i) = 6      means a+b+c or *
      logical(kind=kind(.true.)) :: set

      set = .false.
      if (self%axis_symbol(1)=="_") then
         self%axis_symbol(1) = "z"
         self%axis_symbol_index(1)  = 3
         set = .true.
      end if
      if (self%axis_symbol(2)=="_" .and. self%axis_order(2)==2) then
         call warn_if_(tonto,.not. set,"SPACEGROUP:set_default_axis_symbol_info ... default axis 2, but axis 1 is non-default&
&")
         if (self%axis_order(1)==2 .or. self%axis_order(1)==4) then
            self%axis_symbol(2) = "x"
            self%axis_symbol_index(2)  = 1
         else if (self%axis_order(1)==3 .or. self%axis_order(1)==6) then
            self%axis_symbol(2) = dash_symbol
            self%axis_symbol_index(2)  = 5
         end if
      end if
      if (self%axis_symbol(3)=="_" .and. self%axis_order(3)==3) then
         self%axis_symbol(3) = star_symbol
         self%axis_symbol_index(3)  = 6
      end if

   end subroutine

   subroutine set_spacegroup_order(self)
    type(spacegroup_type) :: self
    ! Work out the spacegroup orders.
      integer(kind=kind(1)) :: axis

   call ensure_(tonto,is_in_range_(self%lattice_symbol_index,(/1,8/)),"SPACEGROUP:set_spacegroup_order ... wrong lattice inde&
&x")
      self%nL = n_implied_translations(self%lattice_symbol_index)
      if (self%centrosymmetric) self%nL = 2*self%nL    ! No. of translations
      self%n_seitz = self%nL*product(self%axis_order)  ! No. of seitz matrices
      self%nG = self%nL                            ! No. of generators
      do axis = 1,3
         if (self%axis_order(axis)>1) self%nG = self%nG+1
      end do

   end subroutine

   subroutine form_L_seitz(self)
    type(spacegroup_type) :: self
    ! Form the seitz generators corresponding to the lattice symbol L
      integer(kind=kind(1)) :: n,v

      n = self%nL
      if (self%centrosymmetric) n = self%nL/2
      do v = 1,n
         self%seitz(1:3,1:3,v)   = NN(:,:,1,1)   ! unit matrix
         self%seitz(1:3,  4,v)   = T_implied(1:3,v,self%lattice_symbol_index)
         self%seitz(  4,1:3,v)   = 0.0d0
         self%seitz(4  ,  4,v)   = 1.0d0
      end do
      if (.not. self%centrosymmetric) then;   return; end if
      do v = 1,n
         self%seitz(1:3,1:3,n+v) = -NN(:,:,1,1)  ! inversion matrix
         self%seitz(1:3,  4,n+v) = T_implied(1:3,v,self%lattice_symbol_index)
         self%seitz(  4,1:3,n+v) = 0.0d0
         self%seitz(4  ,  4,n+v) = 1.0d0
      end do

   end subroutine

   subroutine form_S_seitz(self)
    type(spacegroup_type) :: self
    ! Add the rotational seitz matrices to the generator list
      real(kind=kind(1.0d0)), dimension(3) :: t
      integer(kind=kind(1)) :: axis,N,i,v,ax,isign
      logical(kind=kind(.true.)) :: alpha_used

      self%nR = 0
      do axis = 1,3
         N  = self%axis_order(axis)
         ax = self%axis_symbol_index(axis)
         if (N<=1) cycle
          ! Make the translation vector
         t(:) = 0.0d0
         if (self%has_translation(axis)) then
            alpha_used = .false.
            do i = 1,3  ! loop over translation symbols
                ! are the following two mutually exclusive ?
               v = index(alpha_translation_symbols,self%translation_symbol(i,axis))
               if (v/=0) then
                  t(:)  = t(:) + T_alpha(:,v)
                  alpha_used = .true.
               end if
               v = index(number_translation_symbols,self%translation_symbol(i,axis))
               if (v/=0) then
                  call ensure_(tonto,is_in_range_(ax,(/1,3/)),"SPACEGROUP:form_S_seitz ... wrong axis index")
                  call warn_if_(tonto,alpha_used,"SPACEGROUP:form_S_seitz ... mixed alphabetical/numeric translation symbols"&
&)
                  t(ax) = t(ax) + v/N  ! what if ax = ',", or * ?
               end if
            end do
         end if
         if (self%axis_symbol(axis)=='"' .or. self%axis_symbol(axis)=='"') then
            call ensure_(tonto,axis>1,"SPACEGROUP:form_S_seitz ... cannot have dash symbols on 1st axis")
            if (self%axis_symbol(axis)=="'") N = 5  ! see comments for NN matrices above
            if (self%axis_symbol(axis)=='"') N = 7  ! see comments for NN matrices above
            ax = self%axis_symbol_index(axis-1)     ! NOTE: axis of previous N symbol
            call ensure_(tonto,is_in_range_(ax,(/1,3/)),"SPACEGROUP:form_S_seitz ... wrong previous axis index")
         end if
         if (self%axis_symbol(axis)=="*") then
            call ensure_(tonto,N==3,"SPACEGROUP:form_S_seitz ... star operation is only for order 3 axis")
            N  = 8
            ax = 1
         end if
         call ensure_(tonto,is_in_range_(N,(/1,8/)),"SPACEGROUP:form_S_seitz ... Wrong axis order/type index")
         call ensure_(tonto,is_in_range_(ax,(/1,3/)),"SPACEGROUP:form_S_seitz ... Wrong axis symbol index")
         isign = 1
         if (self%has_axis_bar(axis)) isign = -1
          ! Add the generators to the list here
         self%nR = self%nR + 1
         self%seitz(1:3,1:3,self%nL+self%nR) = isign*NN(:,:,ax,N)
         self%seitz(1:3,  4,self%nL+self%nR) = t(:)
         self%seitz(  4,1:3,self%nL+self%nR) = 0.0d0
         self%seitz(4  ,  4,self%nL+self%nR) = 1.0d0
      end do

   end subroutine

   subroutine generate_seitz(self)
    type(spacegroup_type) :: self
    ! Generate all the Seitz matrices from the minimal generating set.
      integer(kind=kind(1)) :: m,i,j,k
      logical(kind=kind(.true.)) :: equal

      m = self%nG
      if (m==self%n_seitz) then;   return; end if
      i = 1
      main: do
         i = i+1
         j = 1
         pair_products: do
            j = j+1
            self%seitz(:,:,m+1) = matmul(self%seitz(:,:,j),self%seitz(:,:,i))
            self%seitz(1,4,m+1) = mod(self%seitz(1,4,m+1)+2.0d0+10.0d0**(-8),1.0d0)-10.0d0**(-8)
            self%seitz(2,4,m+1) = mod(self%seitz(2,4,m+1)+2.0d0+10.0d0**(-8),1.0d0)-10.0d0**(-8)
            self%seitz(3,4,m+1) = mod(self%seitz(3,4,m+1)+2.0d0+10.0d0**(-8),1.0d0)-10.0d0**(-8)
            do k = 1,m
               equal = same_as_(self%seitz(:,:,m+1),self%seitz(:,:,k))
               if (equal) exit
            end do
            if (.not. equal) m = m+1
            if (m==self%n_seitz) exit main
            if (j==m)        exit pair_products
         end do pair_products
         if (i==m) exit main
      end do main

   end subroutine

   subroutine shift_origin(self)
    type(spacegroup_type) :: self
    ! Shift the origin of the Seitz matrices, by doing a similarity transform
      real(kind=kind(1.0d0)), dimension(4,4) :: V

      if (same_as_(self%origin_shift,(/0,0,0/))) then;   return; end if
      call to_unit_matrix_(V)
      V(1:3,4) = self%origin_shift
      call similarity_transform_12_(self%seitz,V)

   end subroutine

!  ****************************
!  Jones-Faithful decomposition
!  ****************************

   subroutine decode_Jones_Faithful_symbols(self,symbols,mat)
    type(spacegroup_type) :: self
    ! Decode a series of Jones-Faithful symbols and place in the Seitz matrices "mat".
      character(len=*), dimension(:) :: symbols
      real(kind=kind(1.0d0)), dimension(4,4,size(symbols)) :: mat
      integer(kind=kind(1)) :: i

      do i = 1,size(symbols)
         call decode_Jones_Faithful_symbol_(self,symbols(i),mat(:,:,i))
      end do

   end subroutine

   subroutine decode_Jones_Faithful_symbol(self,symbol,mat)
    type(spacegroup_type) :: self
    ! Decode a single Jones-Faithful symbol, e.g. "x-y+z+1/2,y,z-x",
    ! and place in the Seitz matrix in "mat".
      character(*) :: symbol
      real(kind=kind(1.0d0)), dimension(4,4) :: mat
      type(buffer_type) :: buffer
      character(128) :: word,item
      integer(kind=kind(1)) :: pos,comma,row
      real(kind=kind(1.0d0)) :: t

      mat      = 0.0d0
      mat(4,4) = 1.0d0
      pos = 0
      do row = 1,3
         word = symbol(pos+1:)
         if (row<3) then  ! Extract a comma delimited row of the JF symbol
            comma = index(word,",")
            call ensure_(tonto,comma>1,"SPACEGROUP:decode_Jones_Faithful_symbol ... missing comma")
            word = word(1:comma-1)
            call ensure_(tonto,word/=" ","SPACEGROUP:decode_Jones_Faithful_symbol ... empty row, "//trim(to_str_(row)))
            pos = pos + comma
         end if
         call to_lower_case_(word)

         call remove_blanks_(word)
         call separate_before_(word,"+-")  ! Separate out the entities in the string.

         call set_(buffer,word)
         do               ! Extract all the symbols
            if (exhausted_(buffer)) exit
            call get_(buffer,item)
            select case (item)
               case ("x","+x") ;  mat(row,1) = 1
               case ("y","+y") ;  mat(row,2) = 1
               case ("z","+z") ;  mat(row,3) = 1
               case ("-x")     ;  mat(row,1) = -1
               case ("-y")     ;  mat(row,2) = -1
               case ("-z")     ;  mat(row,3) = -1
               case default
                 if (includes_(item,"/")) then  ! should be a fraction
                   t = frac_to_real_(item)
                   t = modulo(t,1.0d0)  ! translate to first unit cell.
                   mat(row,4) = mat(row,4) + t
                 else
                   call die_(tonto,"SPACEGROUP:decode_Jones_Faithful_symbol ... unknown item, "//trim(item))
                 end if
            end select
         end do
      end do

   end subroutine

   function seitz_same_as(self,mat) result(res)
    type(spacegroup_type) :: self
    ! Determine if the Seitz matrices are the same as those in "mat", except
    ! for a rearrangement of order.
      real(kind=kind(1.0d0)), dimension(:,:,:) :: mat
      logical(kind=kind(.true.)) :: res
      logical(kind=kind(.true.)), dimension(self%n_seitz) :: found
      real(kind=kind(1.0d0)), dimension(4,4) :: tmp
      integer(kind=kind(1)) :: i,j

   call ensure_(tonto,size(mat,1)==4,"SPACEGROUP:seitz_same_as ... wrong 1st dimension, mat")
   call ensure_(tonto,size(mat,2)==4,"SPACEGROUP:seitz_same_as ... wrong 1st dimension, mat")
      if (size(mat,3)/=self%n_seitz) then
         res = .false.
           return
      else
         found = .false.
         do i = 1,self%n_seitz
            do j = 1,self%n_seitz
               if (found(j)) cycle
               if (same_as_(self%seitz(:,:,i),mat(:,:,j))) then
                  found(j) = .true.
                  exit
               end if
            end do
         end do
         res = all(found)
         if (res) then;   return; end if
         if (.not. self%centrosymmetric) then;   return; end if
         do i = 1,self%n_seitz
            do j = 1,self%n_seitz
               if (found(j)) cycle
               tmp = -mat(:,:,j)   ! try inverting and rescaling
               tmp(1,4) = mod(tmp(1,4)+2.0d0+10.0d0**(-8),1.0d0)-10.0d0**(-8)
               tmp(2,4) = mod(tmp(2,4)+2.0d0+10.0d0**(-8),1.0d0)-10.0d0**(-8)
               tmp(3,4) = mod(tmp(3,4)+2.0d0+10.0d0**(-8),1.0d0)-10.0d0**(-8)
               tmp(4,4) = 1
               if (same_as_(self%seitz(:,:,i),tmp)) then
                  found(j) = .true.  ! found inverse ... we hope
                  exit             ! its inverse pair also matches
               end if
            end do
         end do
         res = all(found)
      end if

   end function

!  ***********************************
!  Spacegroup geometry transformations
!  ***********************************

   subroutine put_geometry_to_unit_cell(self,g)
    type(spacegroup_type) :: self
    ! Transform the geometry "g" in fractional coordinates into the
    ! (1,1,1) unit cell
       real(kind=kind(1.0d0)), dimension(:,:) :: g
      integer(kind=kind(1)) :: n,n_atom

   call ensure_(tonto,associated(self%seitz),"SPACEGROUP:put_geometry_to_unit_cell ... No Seitz matrices!")
   call ensure_(tonto,size(g,1)==3,"SPACEGROUP:put_geometry_to_unit_cell ... incorrect size for array g")
      n_atom = size(g,2)
      do n = 1,n_atom
         call put_position_to_unit_cell_(self,g(:,n))
      end do

   end subroutine

   subroutine put_position_to_unit_cell(self,p)
    type(spacegroup_type) :: self
    ! Transform the position "p" in fractional coordinates into the
    ! (1,1,1) unit cell
       real(kind=kind(1.0d0)), dimension(3) :: p

   call ensure_(tonto,size(p)==3,"SPACEGROUP:put_position_to_unit_cell ... position p must have size 3")
      p(:) = mod(p(:)+2.0d0,1.0d0)

   end subroutine

   subroutine get_partition_factors(self,pfac,geometry,full)
    type(spacegroup_type) :: self
    ! Given a "geometry" array in fractional coordinates for a molecule,
    ! generate the partition factors or repetition factors "pfac" to be used for
    ! each atom in a structure factor calculation.  If present, the "full"
    ! geometry in fractional coordinates will be returned, where all possible
    ! symmetry distinct atom positions in the unit cell have been generated.
      real(kind=kind(1.0d0)), dimension(:,:) :: geometry
      real(kind=kind(1.0d0)), dimension(:) :: pfac
      real(kind=kind(1.0d0)), dimension(:,:), pointer, optional :: full
      real(kind=kind(1.0d0)), dimension(3) :: pa,pb
      integer(kind=kind(1)) :: a,b,n,n_atom,n_same
      logical(kind=kind(.true.)) :: same

      n_atom = size(geometry,2)
   call ensure_(tonto,size(geometry,1)==3,"SPACEGROUP:get_partition_factors ... Wrong shape for geometry array")
   call ensure_(tonto,size(pfac)==n_atom,"SPACEGROUP:get_partition_factors ... Incompatible shape for p array")
   call ensure_(tonto,associated(self%seitz),"SPACEGROUP:get_partition_factors ... Seitz matrices not initialised")
      do a = 1,n_atom
         pa = geometry(:,a)
         call put_position_to_unit_cell_(self,pa)
         n_same = 0
         do n = 1,self%n_seitz
         do b = 1,n_atom
            pb = geometry(:,b)
            call transform_position_(self,pb,n)
            call put_position_to_unit_cell_(self,pb)
            same = same_as_(pa,pb)
            if (same) n_same = n_same + 1
         end do
         end do
         pfac(a) = n_same
      end do
      if (present(full)) call get_full_geometry_(self,full,geometry,pfac)

   end subroutine

   subroutine get_full_geometry(self,full,geometry,pfac)
    type(spacegroup_type) :: self
    ! Get the "full" set of distinct atom positions in the unit cell, given a
    ! (possibly) partial "geometry", and a set of partition factors "pfac".
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: full
      real(kind=kind(1.0d0)), dimension(:,:) :: geometry
      real(kind=kind(1.0d0)), dimension(:) :: pfac
      real(kind=kind(1.0d0)), dimension(3) :: pa,pb
      integer(kind=kind(1)) :: a,b,n,n_atom,n_full
      logical(kind=kind(.true.)) :: same

      n_atom = size(geometry,2)
   call ensure_(tonto,size(geometry,1)==3,"SPACEGROUP:get_full_geometry ... Wrong shape for geometry array")
   call ensure_(tonto,size(pfac)==n_atom,"SPACEGROUP:get_full_geometry ... Incompatible shape for p array")
   call ensure_(tonto,associated(self%seitz),"SPACEGROUP:get_full_geometry ... Seitz matrices not initialised")
      n_full = 0
      do a = 1,n_atom
         n_full = n_full + self%n_seitz/pfac(a)
      end do
      call create_(full,3,n_full)
      n_full = 0
      do a = 1,n_atom
      do n = 1,self%n_seitz
         pa = geometry(:,a)
         call put_position_to_unit_cell_(self,pa)
         call transform_position_(self,pa,n)
         call put_position_to_unit_cell_(self,pa)
         same = .false.
         do b = 1,n_full
            pb = full(:,b)
            same = same_as_(pa,pb)
            if (same) exit
         end do
         if (.not. same) then        ! New symmetry generated position
            n_full = n_full+1
            full(:,n_full) = pa
         end if
      end do
      end do

   end subroutine

   subroutine transform_geometry(self,g,op)
    type(spacegroup_type) :: self
    ! Transform the geometry "g" in fractional coordinates with the
    ! Seitz operator with index "op"
       real(kind=kind(1.0d0)), dimension(:,:) :: g
      integer(kind=kind(1)) :: op
      integer(kind=kind(1)) :: n,n_atom

   call ensure_(tonto,associated(self%seitz),"SPACEGROUP:transform_geometry ... Seitz matrices not initialised")
   call ensure_(tonto,size(g,1)==3,"SPACEGROUP:transform_geometry ... incorrect size for array g")
   call ensure_(tonto,op>0,"SPACEGROUP:transform_geometry ... operator index out of bounds")
   call ensure_(tonto,op<=self%n_seitz,"SPACEGROUP:transform_geometry ... operator index out of bounds")
      n_atom = size(g,2)
      do n = 1,n_atom
         call transform_position_(self,g(:,n),op)
      end do

   end subroutine

   subroutine transform_position(self,p,op)
    type(spacegroup_type) :: self
    ! Transform the position "p" in fractional coordinates with the
    ! Seitz operator with index "op"
       real(kind=kind(1.0d0)), dimension(3) :: p
      integer(kind=kind(1)) :: op

   call ensure_(tonto,associated(self%seitz),"SPACEGROUP:transform_position ... Seitz matrices not initialised")
   call ensure_(tonto,op>0,"SPACEGROUP:transform_position ... operator index out of bounds")
   call ensure_(tonto,op<=self%n_seitz,"SPACEGROUP:transform_position ... operator index out of bounds")
      p = matmul(self%seitz(1:3,1:3,op),p) + self%seitz(1:3,4,op)

   end subroutine

   function is_same_geometry(self,geom_i,geom_j) result(res)
    type(spacegroup_type) :: self
    ! Return .true. if the geometries "geom_i" and "geom_j" in fractional
    ! coordinates are the same, to within *traslational* symmetry
      real(kind=kind(1.0d0)), dimension(:,:) :: geom_i,geom_j
      logical(kind=kind(.true.)) :: res
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: gi,gj
      integer(kind=kind(1)) :: i,j,n_atom
      logical(kind=kind(.true.)) :: same
      logical(kind=kind(.true.)), dimension(:), pointer :: skip

      n_atom = size(geom_i,2)
   call ensure_(tonto,associated(self%seitz),"SPACEGROUP:is_same_geometry ... Seitz matrices not initialised")
   call ensure_(tonto,size(geom_i,1)==3,"SPACEGROUP:is_same_geometry ... incorrect size for array geom_i")
   call ensure_(tonto,size(geom_j,1)==3,"SPACEGROUP:is_same_geometry ... incorrect size for array geom_j")
   call ensure_(tonto,n_atom==size(geom_j,2),"SPACEGROUP:is_same_geometry ... incompatible sizes for geom_i, geom_j")
      call create_(gi,3,n_atom); gi = geom_i; call put_geometry_to_unit_cell_(self,gi)
      call create_(gj,3,n_atom); gj = geom_j; call put_geometry_to_unit_cell_(self,gj)
      call create_(skip,n_atom); skip(:) = .false.
      do i = 1,n_atom
         do j = 1,n_atom
            same = same_as_(gi(:,i),gj(:,j))
            if (.not. same .or. skip(j)) cycle
            skip(j) = .true.
            exit
         end do
      end do
      res = all(skip)  ! True if all atoms in i were matched (skipped) in j
      call destroy_(skip)
      call destroy_(gj)
      call destroy_(gi)

   end function

!  ************************
!  Spacegroup unique symops
!  ************************

   subroutine make_unique_symops(self,geometry)
    type(spacegroup_type) :: self
    ! Check to see if any of the Seitz matrices ".seitz" will change a
    ! molecular "geometry" by more than just a translation or an inversion
    ! operation: such a list is called a reduced symop list, or unique symop list
    ! Array ".map_to_unique(i)" maps the i-th symmetry operator onto the
    ! corresponding reduced operator "j" which is equivalent to it by
    ! translation, or to "-j" if equivalent by inversion. ".unique_symop(j)"
    ! is the first unique Seitz operator in the list. ".n_unique" is set
    ! to the number of unique symmetry operations.
      real(kind=kind(1.0d0)), dimension(:,:) :: geometry
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: gi,gu
      integer(kind=kind(1)), dimension(:), pointer :: unique_symop
      integer(kind=kind(1)) :: n_atom,i,j,u, n
      logical(kind=kind(.true.)) :: same,inverted

   call ensure_(tonto,associated(self%seitz),"SPACEGROUP:make_unique_symops ... Seitz matrices not initialised")
      n_atom = size(geometry,2)
      call create_(gi,3,n_atom)
      call create_(gu,3,n_atom)
      call create_(self%map_to_unique,self%n_seitz)
      call create_(unique_symop,self%n_seitz)
      n = 0
      do i = 1,self%n_seitz
         same     = .false.
         inverted = .false.
         gi = geometry; call transform_geometry_(self,gi,i)
         do j = 1,n                             ! Loop over unique symops
            u = unique_symop(j)
            gu = geometry; call transform_geometry_(self,gu,u)
            same = is_same_geometry_(self,gi,gu)
            inverted = equals_(self%seitz(1:3,1:3,i),-self%seitz(1:3,1:3,u))
            if (same .or. inverted) exit
         end do
         if (inverted) then
            self%map_to_unique(i) = -j              ! negative for inversion
         else if (same) then
            self%map_to_unique(i) =  j              ! positive for translation
         else
            n = n + 1
            unique_symop(n) = i
            self%map_to_unique(i) = n               ! map to new unique symop
         end if
      end do
      self%n_unique = n
      call create_(self%unique_symop,n)
      self%unique_symop = unique_symop
      call destroy_(unique_symop)
      call destroy_(gu)
      call destroy_(gi)

   end subroutine

   function unique_symop_mat(self,u) result(res)
    type(spacegroup_type) :: self
    ! Return the "u"-th unique symop matrix in the unique list made by routine
    ! ".make_unique_symops".
      real(kind=kind(1.0d0)), dimension(3,3) :: res
       integer(kind=kind(1)) :: u

   call ensure_(tonto,associated(self%unique_symop),"SPACEGROUP:unique_symop_mat ... No unique symops")
   call ensure_(tonto,u<=self%n_unique,"SPACEGROUP:unique_symop_mat ... symop index out of range")
   call ensure_(tonto,u>0,"SPACEGROUP:unique_symop_mat ... symop index out of range")
      res = self%seitz(1:3,1:3,self%unique_symop(u))

   end function

!  **************
!  Output methods
!  **************

   subroutine put(self)
    type(spacegroup_type) :: self
    ! Put out the spacegroup information

   call ensure_(tonto,self%analysed .or. associated(self%seitz) .or. associated(self%unique_symop),"SPACEGROUP:put ... no inf&
&o")
      call flush_(stdout)
      call put_text_(stdout,"SPACEGROUP output:",flush=2)
      if (self%analysed) call put_spacegroup_name_info_(self)
      if (associated(self%seitz)) call put_seitz_(self)
      if (associated(self%unique_symop)) call put_unique_symop_(self)

   end subroutine

   subroutine put_spacegroup_name_info(self)
    type(spacegroup_type) :: self
    ! Put out the spacegroup name information (not the seitz matrices)
      integer(kind=kind(1)) :: order,axis_symbol_indices
      character(128) :: axis_symbol,subscripts,HM_symbol,Hall_symbol
      character(len=1), dimension(3,3) :: sub

   call ensure_(tonto,self%analysed,"SPACEGROUP:put_spacegroup_name_info ... spacegroup not analysed")
      HM_symbol   = self%HM_symbol;    call to_upper_case_(HM_symbol(1:1))
      Hall_symbol = self%Hall_symbol;  call to_upper_case_(Hall_symbol(1:1))
      call put_text_(stdout,"Information about crystal cell symmetry",flush=2)
      call show_(stdout,"International Table no. = ",self%IT_group_number,real_width=.true.)
      call show_(stdout,"IT symbol               = ",trim(self%IT_symbol))
      call show_(stdout,"Hermann-Mauguin symbol  = ",trim(HM_symbol))
      call show_(stdout,"Hall symbol             = ",trim(Hall_symbol))
      call show_(stdout,"Schoenflies symbol      = ",trim(self%Schoenflies_symbol))
      call show_(stdout,"Lattice symbol          = ",trim(self%lattice_symbol))
      call show_(stdout,"Lattice type            = ",trim(self%lattice_type))
      call show_(stdout,"Spacegroup order        = ",self%n_seitz,real_width=.true.)
      call show_(stdout,"Centro-symmetric?       = ",self%centrosymmetric,real_width=.true.)
      order = 100*self%axis_order(1) + 10*self%axis_order(2) + self%axis_order(3)
      axis_symbol = self%axis_symbol(1) // &
                    self%axis_symbol(2) // &
                    self%axis_symbol(3)
      axis_symbol_indices = 100*self%axis_symbol_index(1) + &
                             10*self%axis_symbol_index(2) + &
                                self%axis_symbol_index(3)
      sub = self%translation_symbol
      subscripts = &
         sub(1,1) // sub(2,1) // sub(3,1) // " " // &
         sub(1,2) // sub(2,2) // sub(3,2) // " " // &
         sub(1,3) // sub(2,3) // sub(3,3)
      call show_(stdout,"Axis orders             = ",order,real_width=.true.)
      call show_(stdout,"Axis symbols            = ",axis_symbol)
      call show_(stdout,"Axis symbol indices     = ",axis_symbol_indices,real_width=.true.)
      call show_(stdout,"Translation subscripts  = ",subscripts)
      call show_(stdout,"No of T generators      = ",self%nL,real_width=.true.)
      call show_(stdout,"No of R generators      = ",self%nR,real_width=.true.)
      call show_(stdout,"No of Generators        = ",self%nG,real_width=.true.)
      call show_(stdout,"Origin shift            = ",self%origin_shift(1),self%origin_shift(2),self%origin_shift(3))

   end subroutine

   subroutine put_seitz(self,mat)
    type(spacegroup_type) :: self
    ! Put out the spacegroup seitz matrices.
      real(kind=kind(1.0d0)), dimension(:,:,:), target, optional :: mat
      integer(kind=kind(1)) :: n
      real(kind=kind(1.0d0)), dimension(:,:,:), pointer :: seitz

      seitz => self%seitz
      if (present(mat)) seitz => mat
      call ensure_(tonto,associated(seitz),"SPACEGROUP:put_seitz ... no seitz matrices")
      call flush_(stdout)
      call text_(stdout,"Seitz matrices :")
      do n = 1,self%n_seitz
        call flush_(stdout)
        call show_(stdout,"n = ",n)
        call put_(stdout,seitz(:,:,n))
      end do

   end subroutine

   subroutine put_unique_symop(self)
    type(spacegroup_type) :: self
    ! Put out information about unique symops (those not related by translation
    ! or inversion to other symops).
     integer(kind=kind(1)) :: n,nu,k

     call flush_(stdout)
     call put_text_(stdout,"SPACEGROUP unique symop list output:",flush=2)
     call dash_(stdout,int_fields=2,real_fields=1)
     call put_(stdout,"Symop", int_width=.true.)
     call put_(stdout,"Symop Type")
     call put_(stdout,"Unique #", int_width=.true.)
     call flush_(stdout)
     call dash_(stdout,int_fields=2,real_fields=1)
     k = 1
     do n = 1,self%n_seitz
       call put_(stdout,n)
       nu = self%map_to_unique(n)
       if (nu==k) then
         call put_(stdout,"Unique symop")
         k = k+1
       else if (nu<0) then
         call put_(stdout,"Inversion of")
       else
         call put_(stdout,"Translation of")
       end if
       call put_(stdout,nu,flush=1)
     end do
     call dash_(stdout,int_fields=2,real_fields=1)

   end subroutine

   subroutine put_matching_IT_symbols(self,symbol)
    type(spacegroup_type) :: self
    ! Put out all the IT symbols which match "symbol".
      character(*) :: symbol
      logical(kind=kind(.true.)), dimension(:), pointer :: mask
      character(128), dimension(:), pointer :: IT,HM,Hall
      integer(kind=kind(1)) :: n,i

      call create_(mask,593)
      mask = includes_(spacegroup_conversion_table(1,:),trim(symbol),at_start=.true.)
      n = count(mask)
      call create_(IT,n);   IT   = pack(spacegroup_conversion_table(1,:),mask)
      call create_(HM,n);   HM   = pack(spacegroup_conversion_table(3,:),mask)
      call create_(Hall,n); Hall = pack(spacegroup_conversion_table(4,:),mask)
      call flush_(stdout)
      call dash_(stdout,real_fields=3)
      call put_(stdout,"IT symbol")
      call put_(stdout,"HM symbol")
      call put_(stdout,"Hall symbol")
      call flush_(stdout)
      call dash_(stdout,real_fields=3)
      do i = 1,n
         call put_(stdout,IT(i))
         call put_(stdout,HM(i))
         call put_(stdout,Hall(i))
         call flush_(stdout)
      end do
      call dash_(stdout,real_fields=3)
      call destroy_(Hall)
      call destroy_(HM)
      call destroy_(IT)
      call destroy_(mask)

   end subroutine

   subroutine put_matching_HM_symbols(self,symbol)
    type(spacegroup_type) :: self
    ! Put out all the HM symbols which match "symbol".
      character(*) :: symbol
      logical(kind=kind(.true.)), dimension(:), pointer :: mask
      character(128), dimension(:), pointer :: IT,HM,Hall
      integer(kind=kind(1)) :: n,i

      call create_(mask,593)
      mask = includes_(spacegroup_conversion_table(3,:),trim(symbol),at_start=.true.)
      n = count(mask)
      call create_(IT,n);   IT   = pack(spacegroup_conversion_table(1,:),mask)
      call create_(HM,n);   HM   = pack(spacegroup_conversion_table(3,:),mask)
      call create_(Hall,n); Hall = pack(spacegroup_conversion_table(4,:),mask)
      call flush_(stdout)
      call dash_(stdout,real_fields=3)
      call put_(stdout,"IT symbol")
      call put_(stdout,"HM symbol")
      call put_(stdout,"Hall symbol")
      call flush_(stdout)
      call dash_(stdout,real_fields=3)
      do i = 1,n
         call put_(stdout,IT(i))
         call put_(stdout,HM(i))
         call put_(stdout,Hall(i))
         call flush_(stdout)
      end do
      call dash_(stdout,real_fields=3)
      call destroy_(Hall)
      call destroy_(HM)
      call destroy_(IT)
      call destroy_(mask)

   end subroutine

end
