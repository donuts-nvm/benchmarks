!-----------------------------------------------------------------------
!
! TEXTFILE : Line-oriented formatted sequential advancing file
! input/output, including input from standard input, and output to
! standard output.
!
! The TEXTFILE object is a FILE with a BUFFER and a REALFMT formatting
! object. The input/output proceeds via a line buffer. It is forbidden
! to open a TEXTFILE simultaneously for input and output.
!
! There is a default internal standard input object, "stdin",
! and a default standard output object, "stdout".
!
! A line count is maintained to allow back-tracking to previous lines,
! and particular items on each line. This is useful for input.
!
! For output, items can be put in columns of a specified width, with
! double precision numbers having a specified precision and "style"
! (set using fortran conventions). This is useful for dynamic tables,
! which the user can change at run time. Rewind and backtracking
! are not allowed for output files.
!
! It is possible to redirect the input or output to a new file
! using the "redirect" command. The previous file can be recovered
! using "unsave", or it will revert back automatically to the previous
! file if the redirected file ends. It is also possibile to redirect
! input to an internal file. This is useful for processing a list of
! text as if it were a file. Output to an internal file is not allowed.
!
! Note that the input buffer is limited to size BSTR_SIZE, set in the
! "macros" file.
!
! The system information in "tonto" is updated whenever a I/O operation
! or a buffer operation is performed.
!
! Copyright (C) Dylan Jayatilaka, 1999
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
! $Id: textfile.foo,v 1.40.2.19 2004/04/21 09:47:21 reaper Exp $
!---------------------------------------------------------------------------

module TEXTFILE_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

   use REALVEC_MODULE, only: create_
   use REALVEC_MODULE, only: convert_from_

   use INT_MODULE, only: to_str_

   use PARALLEL_MODULE, only: broadcast_
   use PARALLEL_MODULE, only: do_io_

   use INTMAT_MODULE, only: create_

   use BUFFER_MODULE, only: buffer_string_
   use BUFFER_MODULE, only: not_exhausted_
   use BUFFER_MODULE, only: set_
   use BUFFER_MODULE, only: move_to_item_
   use BUFFER_MODULE, only: skip_item_
   use BUFFER_MODULE, only: clear_
   use BUFFER_MODULE, only: next_item_number_
   use BUFFER_MODULE, only: exhausted_
   use BUFFER_MODULE, only: put_
   use BUFFER_MODULE, only: cursor_pointer_
   use BUFFER_MODULE, only: get_
   use BUFFER_MODULE, only: copy_
   use BUFFER_MODULE, only: empty_

   use INTVEC_MODULE, only: create_

   use INTVECVEC_MODULE, only: create_

   use STR_MODULE, only: is_int_
   use STR_MODULE, only: is_included_in_
   use STR_MODULE, only: is_known_unit_
   use STR_MODULE, only: to_lower_case_
   use STR_MODULE, only: trim_blanks_from_start_
   use STR_MODULE, only: includes_any_in_
   use STR_MODULE, only: get_next_item_position_
   use STR_MODULE, only: to_int_
   use STR_MODULE, only: includes_

   use STRVEC_MODULE, only: has_any_included_in_
   use STRVEC_MODULE, only: create_
   use STRVEC_MODULE, only: create_copy_
   use STRVEC_MODULE, only: destroy_

   use BINVEC_MODULE, only: create_

   use REALMAT_MODULE, only: create_
   use REALMAT_MODULE, only: convert_from_

   use REAL_MODULE, only: convert_from_

   use CPXVEC_MODULE, only: create_

   use UNITNUMBER_MODULE, only: free_
   use UNITNUMBER_MODULE, only: flush_buffer_
   use UNITNUMBER_MODULE, only: get_
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

   public    read_realmat4_
   interface read_realmat4_
      module procedure read_realmat4
   end interface

   public    put_intmat_
   interface put_intmat_
      module procedure put_intmat
   end interface

   public    show_realvec_
   interface show_realvec_
      module procedure show_realvec
   end interface

   public    put_text_
   interface put_text_
      module procedure put_text
   end interface

   public    read_cpx_
   interface read_cpx_
      module procedure read_cpx
   end interface

   public    put_strvec_
   interface put_strvec_
      module procedure put_strvec
   end interface

   public    read_formatted_real_
   interface read_formatted_real_
      module procedure read_formatted_real
   end interface

   public    close_
   interface close_
      module procedure close
   end interface

   private    put_realvec_by_row_
   interface put_realvec_by_row_
      module procedure put_realvec_by_row
   end interface

   public    set_use_labels_
   interface set_use_labels_
      module procedure set_use_labels
   end interface

   private    put_binvec_by_column_
   interface put_binvec_by_column_
      module procedure put_binvec_by_column
   end interface

   public    read_keywords_
   interface read_keywords_
      module procedure read_keywords
   end interface

   public    set_n_fields_
   interface set_n_fields_
      module procedure set_n_fields
   end interface

   public    read_int_
   interface read_int_
      module procedure read_int
   end interface

   public    skip_next_item_
   interface skip_next_item_
      module procedure skip_next_item
   end interface

   public    buffer_exhausted_
   interface buffer_exhausted_
      module procedure buffer_exhausted
   end interface

   public    line_number_
   interface line_number_
      module procedure line_number
   end interface

   public    look_backwards_for_item_
   interface look_backwards_for_item_
      module procedure look_backwards_for_item
   end interface

   private    read_line_internal_
   interface read_line_internal_
      module procedure read_line_internal
   end interface

   public    look_backwards_for_
   interface look_backwards_for_
      module procedure look_backwards_for
   end interface

   public    read_intvec_
   interface read_intvec_
      module procedure read_intvec
   end interface

   public    read_binvec_ptr_
   interface read_binvec_ptr_
      module procedure read_binvec_ptr
   end interface

   public    put_opmatrix_
   interface put_opmatrix_
      module procedure put_opmatrix
   end interface

   public    look_for_item_
   interface look_for_item_
      module procedure look_for_item
   end interface

   public    copy_
   interface copy_
      module procedure copy
   end interface

   public    read_realvec_ptr_
   interface read_realvec_ptr_
      module procedure read_realvec_ptr
   end interface

   private    open_for_read_
   interface open_for_read_
      module procedure open_for_read
   end interface

   public    read_real_
   interface read_real_
      module procedure read_real
   end interface

   private    format_for_int_
   interface format_for_int_
      module procedure format_for_int
   end interface

   public    read_intmat_
   interface read_intmat_
      module procedure read_intmat
   end interface

   private    revert_line_
   interface revert_line_
      module procedure revert_line
   end interface

   public    set_real_width_
   interface set_real_width_
      module procedure set_real_width
   end interface

   public    put_cpxvec_
   interface put_cpxvec_
      module procedure put_cpxvec
   end interface

   public    tab_
   interface tab_
      module procedure tab
   end interface

   private    put_realmat_by_column_
   interface put_realmat_by_column_
      module procedure put_realmat_by_column
   end interface

   public    read_strvec_
   interface read_strvec_
      module procedure read_strvec
   end interface

   public    flush_
   interface flush_
      module procedure flush
      module procedure flush_1
   end interface

   public    look_for_any_item_
   interface look_for_any_item_
      module procedure look_for_any_item
   end interface

   public    destroy_
   interface destroy_
      module procedure destroy
   end interface

   public    close_and_delete_
   interface close_and_delete_
      module procedure close_and_delete
   end interface

   private    put_intmat_by_column_
   interface put_intmat_by_column_
      module procedure put_intmat_by_column
   end interface

   public    open_
   interface open_
      module procedure open
      module procedure open_1
   end interface

   public    create_stdout_
   interface create_stdout_
      module procedure create_stdout
   end interface

   public    put_cpxmat_
   interface put_cpxmat_
      module procedure put_cpxmat
   end interface

   public    read_cpxmat3_
   interface read_cpxmat3_
      module procedure read_cpxmat3
   end interface

   public    read_cpxmat4_
   interface read_cpxmat4_
      module procedure read_cpxmat4
   end interface

   public    read_cpxmat5_
   interface read_cpxmat5_
      module procedure read_cpxmat5
   end interface

   public    read_formatted_mat_
   interface read_formatted_mat_
      module procedure read_formatted_mat
   end interface

   public    show_int_
   interface show_int_
      module procedure show_int
   end interface

   public    read_realmat_ptr_
   interface read_realmat_ptr_
      module procedure read_realmat_ptr
   end interface

   public    list_length_
   interface list_length_
      module procedure list_length
   end interface

   public    set_precision_
   interface set_precision_
      module procedure set_precision
   end interface

   public    put_unit_
   interface put_unit_
      module procedure put_unit
   end interface

   public    put_margin_
   interface put_margin_
      module procedure put_margin
   end interface

   private    update_line_
   interface update_line_
      module procedure update_line
   end interface

   public    show_int_3_
   interface show_int_3_
      module procedure show_int_3
   end interface

   public    next_line_item_
   interface next_line_item_
      module procedure next_line_item
   end interface

   public    destroy_ptr_part_
   interface destroy_ptr_part_
      module procedure destroy_ptr_part
   end interface

   public    move_to_previous_item_on_line_
   interface move_to_previous_item_on_line_
      module procedure move_to_previous_item_on_line
   end interface

   private    move_to_record_external_
   interface move_to_record_external_
      module procedure move_to_record_external
   end interface

   private    put_labelled_intmat_
   interface put_labelled_intmat_
      module procedure put_labelled_intmat
   end interface

   public    read_cpxvec_
   interface read_cpxvec_
      module procedure read_cpxvec
   end interface

   public    put_cpxmat3_
   interface put_cpxmat3_
      module procedure put_cpxmat3
   end interface

   public    put_cpxmat4_
   interface put_cpxmat4_
      module procedure put_cpxmat4
   end interface

   public    text_
   interface text_
      module procedure text
   end interface

   private    rewind_internal_
   interface rewind_internal_
      module procedure rewind_internal
   end interface

   public    put_cpxmat5_
   interface put_cpxmat5_
      module procedure put_cpxmat5
   end interface

   public    unit_used_
   interface unit_used_
      module procedure unit_used
   end interface

   public    show_binvec_
   interface show_binvec_
      module procedure show_binvec
   end interface

   public    read_cpxmat_
   interface read_cpxmat_
      module procedure read_cpxmat
   end interface

   private    put_binvec_by_row_
   interface put_binvec_by_row_
      module procedure put_binvec_by_row
   end interface

   public    create_
   interface create_
      module procedure create
      module procedure create_1
   end interface

   public    put_real_
   interface put_real_
      module procedure put_real
   end interface

   public    create_copy_
   interface create_copy_
      module procedure create_copy
   end interface

   public    read_strvec_ptr_
   interface read_strvec_ptr_
      module procedure read_strvec_ptr
   end interface

   private    use_style_
   interface use_style_
      module procedure use_style
   end interface

   public    set_int_width_
   interface set_int_width_
      module procedure set_int_width
   end interface

   public    set_real_precision_
   interface set_real_precision_
      module procedure set_real_precision
   end interface

   private    put_cpxvec_by_row_
   interface put_cpxvec_by_row_
      module procedure put_cpxvec_by_row
   end interface

   public    rewind_
   interface rewind_
      module procedure rewind
   end interface

   private    format_for_real_
   interface format_for_real_
      module procedure format_for_real
   end interface

   private    open_new_file_for_write_
   interface open_new_file_for_write_
      module procedure open_new_file_for_write
   end interface

   public    read_imprecise_real_
   interface read_imprecise_real_
      module procedure read_imprecise_real
   end interface

   private    move_to_end_internal_
   interface move_to_end_internal_
      module procedure move_to_end_internal
   end interface

   public    read_intvecpxvec_ptr_
   interface read_intvecpxvec_ptr_
      module procedure read_intvecpxvec_ptr
   end interface

   public    unsave_
   interface unsave_
      module procedure unsave
   end interface

   private    nice_field_width_for_
   interface nice_field_width_for_
      module procedure nice_field_width_for
   end interface

   public    read_realvec_
   interface read_realvec_
      module procedure read_realvec
   end interface

   public    show_real_
   interface show_real_
      module procedure show_real
   end interface

   public    read_intvecvec_
   interface read_intvecvec_
      module procedure read_intvecvec
   end interface

   private    read_line_external_
   interface read_line_external_
      module procedure read_line_external
   end interface

   public    next_formatted_REAL_
   interface next_formatted_REAL_
      module procedure next_formatted_REAL
   end interface

   public    put_dash_
   interface put_dash_
      module procedure put_dash
   end interface

   public    nullify_ptr_part_
   interface nullify_ptr_part_
      module procedure nullify_ptr_part
   end interface

   public    skip_line_
   interface skip_line_
      module procedure skip_line
   end interface

   public    read_realmat_
   interface read_realmat_
      module procedure read_realmat
   end interface

   public    put_bin_
   interface put_bin_
      module procedure put_bin
   end interface

   public    set_real_style_
   interface set_real_style_
      module procedure set_real_style
   end interface

   public    read_realvec_pair_quantities_
   interface read_realvec_pair_quantities_
      module procedure read_realvec_pair_quantities
   end interface

   public    next_str_
   interface next_str_
      module procedure next_str
   end interface

   public    put_realvec_
   interface put_realvec_
      module procedure put_realvec
   end interface

   public    rest_of_line_
   interface rest_of_line_
      module procedure rest_of_line
   end interface

   private    put_labelled_mat_
   interface put_labelled_mat_
      module procedure put_labelled_mat
   end interface

   public    move_to_line_
   interface move_to_line_
      module procedure move_to_line
   end interface

   public    show_intvec_
   interface show_intvec_
      module procedure show_intvec
   end interface

   public    put_str_
   interface put_str_
      module procedure put_str
   end interface

   public    read_intvec_ptr_
   interface read_intvec_ptr_
      module procedure read_intvec_ptr
   end interface

   public    move_to_end_
   interface move_to_end_
      module procedure move_to_end
   end interface

   public    set_default_units_
   interface set_default_units_
      module procedure set_default_units
   end interface

   public    put_realmat_
   interface put_realmat_
      module procedure put_realmat
   end interface

   public    redirect_
   interface redirect_
      module procedure redirect
      module procedure redirect_1
   end interface

   public    backspace_line_
   interface backspace_line_
      module procedure backspace_line
   end interface

   public    at_end_of_line_
   interface at_end_of_line_
      module procedure at_end_of_line
   end interface

   private    open_for_write_
   interface open_for_write_
      module procedure open_for_write
   end interface

   public    is_open_
   interface is_open_
      module procedure is_open
   end interface

   public    has_string_
   interface has_string_
      module procedure has_string
   end interface

   public    move_to_record_
   interface move_to_record_
      module procedure move_to_record
   end interface

   public    read_bin_
   interface read_bin_
      module procedure read_bin
   end interface

   public    put_info_
   interface put_info_
      module procedure put_info
   end interface

   public    show_strvec_
   interface show_strvec_
      module procedure show_strvec
   end interface

   public    save_
   interface save_
      module procedure save
   end interface

   public    set_default_format_
   interface set_default_format_
      module procedure set_default_format
   end interface

   public    delete_
   interface delete_
      module procedure delete
   end interface

   private    put_cpxvec_by_column_
   interface put_cpxvec_by_column_
      module procedure put_cpxvec_by_column
   end interface

   public    n_line_items_
   interface n_line_items_
      module procedure n_line_items
   end interface

   public    set_margin_
   interface set_margin_
      module procedure set_margin
   end interface

   public    read_str_
   interface read_str_
      module procedure read_str
   end interface

   private    rewind_external_
   interface rewind_external_
      module procedure rewind_external
   end interface

   public    put_binvec_
   interface put_binvec_
      module procedure put_binvec
   end interface

   public    revert_
   interface revert_
      module procedure revert
   end interface

   public    move_to_line_item_
   interface move_to_line_item_
      module procedure move_to_line_item
   end interface

   public    read_intmat_ptr_
   interface read_intmat_ptr_
      module procedure read_intmat_ptr
   end interface

   public    read_realmat_quantity_
   interface read_realmat_quantity_
      module procedure read_realmat_quantity
   end interface

   public    move_to_last_item_on_line_
   interface move_to_last_item_on_line_
      module procedure move_to_last_item_on_line
   end interface

   public    reverted_
   interface reverted_
      module procedure reverted
   end interface

   public    last_line_item_
   interface last_line_item_
      module procedure last_line_item
   end interface

   private    format_for_bin_
   interface format_for_bin_
      module procedure format_for_bin
   end interface

   public    read_real_quantity_
   interface read_real_quantity_
      module procedure read_real_quantity
   end interface

   public    show_bit_string_
   interface show_bit_string_
      module procedure show_bit_string
   end interface

   public    at_end_of_file_
   interface at_end_of_file_
      module procedure at_end_of_file
   end interface

   public    read_realvec_quantity_
   interface read_realvec_quantity_
      module procedure read_realvec_quantity
   end interface

   public    put_opvector_
   interface put_opvector_
      module procedure put_opvector
   end interface

   public    read_line_
   interface read_line_
      module procedure read_line
   end interface

   public    dash_
   interface dash_
      module procedure dash
   end interface

   private    format_for_bit_string_
   interface format_for_bit_string_
      module procedure format_for_bit_string
   end interface

   public    buffer_string_
   interface buffer_string_
      module procedure buffer_string
   end interface

   public    previous_line_item_
   interface previous_line_item_
      module procedure previous_line_item
   end interface

   public    look_for_
   interface look_for_
      module procedure look_for
   end interface

   public    read_cpxvec_ptr_
   interface read_cpxvec_ptr_
      module procedure read_cpxvec_ptr
   end interface

   public    next_item_
   interface next_item_
      module procedure next_item
   end interface

   public    process_keyword_
   interface process_keyword_
      module procedure process_keyword
   end interface

   public    show_bin_
   interface show_bin_
      module procedure show_bin
   end interface

   public    show_real_3_
   interface show_real_3_
      module procedure show_real_3
   end interface

   private    put_strvec_by_column_
   interface put_strvec_by_column_
      module procedure put_strvec_by_column
   end interface

   private    number_lines_col_
   interface number_lines_col_
      module procedure number_lines_col
   end interface

   private    move_to_end_external_
   interface move_to_end_external_
      module procedure move_to_end_external
   end interface

   public    read_binvec_
   interface read_binvec_
      module procedure read_binvec
   end interface

   public    list_list_length_
   interface list_list_length_
      module procedure list_list_length
   end interface

   public    put_formatted_mat_
   interface put_formatted_mat_
      module procedure put_formatted_mat
   end interface

   public    set_comment_chars_
   interface set_comment_chars_
      module procedure set_comment_chars
   end interface

   public    exists_
   interface exists_
      module procedure exists
   end interface

   private    put_intmat_by_row_
   interface put_intmat_by_row_
      module procedure put_intmat_by_row
   end interface

   public    create_stdin_
   interface create_stdin_
      module procedure create_stdin
   end interface

   private    move_to_record_internal_
   interface move_to_record_internal_
      module procedure move_to_record_internal
   end interface

   public    set_quote_chars_
   interface set_quote_chars_
      module procedure set_quote_chars
   end interface

   public    show_str_
   interface show_str_
      module procedure show_str
   end interface

   public    put_cpx_
   interface put_cpx_
      module procedure put_cpx
   end interface

   public    read_realvec_quantity_ptr_
   interface read_realvec_quantity_ptr_
      module procedure read_realvec_quantity_ptr
   end interface

   public    get_next_item_
   interface get_next_item_
      module procedure get_next_item
   end interface

   private    open_old_file_for_write_
   interface open_old_file_for_write_
      module procedure open_old_file_for_write
   end interface

   private    update_system_info_
   interface update_system_info_
      module procedure update_system_info
   end interface

   public    put_intvecvec_
   interface put_intvecvec_
      module procedure put_intvecvec
   end interface

   private    put_strvec_by_row_
   interface put_strvec_by_row_
      module procedure put_strvec_by_row
   end interface

   public    put_int_
   interface put_int_
      module procedure put_int
   end interface

   public    put_realmat3_
   interface put_realmat3_
      module procedure put_realmat3
   end interface

   private    put_realmat_by_row_
   interface put_realmat_by_row_
      module procedure put_realmat_by_row
   end interface

   public    put_intvec_
   interface put_intvec_
      module procedure put_intvec
   end interface

   public    put_realmat4_
   interface put_realmat4_
      module procedure put_realmat4
   end interface

   private    put_realvec_by_column_
   interface put_realvec_by_column_
      module procedure put_realvec_by_column
   end interface

   public    end_of_file_
   interface end_of_file_
      module procedure end_of_file
   end interface

   public    read_cadpac_mat_
   interface read_cadpac_mat_
      module procedure read_cadpac_mat
   end interface

   private    put_intvec_by_row_
   interface put_intvec_by_row_
      module procedure put_intvec_by_row
   end interface

   private    put_intvec_by_column_
   interface put_intvec_by_column_
      module procedure put_intvec_by_column
   end interface

   public    move_to_previous_item_
   interface move_to_previous_item_
      module procedure move_to_previous_item
   end interface

   public    read_realmat3_
   interface read_realmat3_
      module procedure read_realmat3
   end interface

   public    put_bit_string_
   interface put_bit_string_
      module procedure put_bit_string
   end interface

   public read_; interface read_
      module procedure read_str
      module procedure read_real_quantity
      module procedure read_formatted_real
      module procedure read_imprecise_real
      module procedure read_cpx
      module procedure read_int
      module procedure read_bin
      module procedure read_intvec
      module procedure read_binvec
      module procedure read_strvec
      module procedure read_realvec_quantity
      module procedure read_realvec_pair_quantities
      module procedure read_cpxvec
      module procedure read_intmat
      module procedure read_realmat_quantity
      module procedure read_cpxmat
      module procedure read_realmat3
      module procedure read_realmat4
      module procedure read_cpxmat3
      module procedure read_cpxmat4
      module procedure read_cpxmat5
      module procedure read_intvecvec
   end interface

   public read_ptr_; interface read_ptr_
      module procedure read_strvec_ptr
      module procedure read_binvec_ptr
      module procedure read_intvec_ptr
      module procedure read_realvec_quantity_ptr
      module procedure read_cpxvec_ptr
      module procedure read_realmat_ptr
      module procedure read_intmat_ptr
      module procedure read_intvecpxvec_ptr
   end interface

   public put_; interface put_
      module procedure put_info
      module procedure put_str
      module procedure put_int
      module procedure put_bin
      module procedure put_real
      module procedure put_cpx
      module procedure put_intvec
      module procedure put_strvec
      module procedure put_realvec
      module procedure put_cpxvec
      module procedure put_binvec
      module procedure put_intmat
      module procedure put_realmat
      module procedure put_realmat3
      module procedure put_realmat4
      module procedure put_cpxmat
      module procedure put_cpxmat3
      module procedure put_cpxmat4
      module procedure put_cpxmat5
      module procedure put_intvecvec
      module procedure put_opvector
      module procedure put_opmatrix
   end interface

   public show_; interface show_
      module procedure show_str
      module procedure show_int
      module procedure show_int_3
      module procedure show_bin
      module procedure show_real
      module procedure show_real_3
      module procedure show_binvec
      module procedure show_intvec
      module procedure show_realvec
      module procedure show_strvec
   end interface

   type(textfile_type), pointer, public :: stdin => NULL()

   type(textfile_type), pointer, public :: stdout => NULL()

contains

!  *****************************
!  File creation type operations
!  *****************************

   subroutine create_stdin(self)
    type(textfile_type) :: self
    ! Create a the standard input file object, if needed.
    ! Return a pointer to it if already created
      pointer :: self

      if (.not. associated(stdin)) then
         allocate(stdin)

         call nullify_ptr_part_(stdin)
         stdin%name = "stdin"
         stdin%action = "read"
         stdin%record = 0
         stdin%unit = 5
         stdin%io_status = 0
         stdin%ignore_end_of_file = .false.
         stdin%no_of_lines = -1
         stdin%default_units = " "
         stdin%comment_chars = "!#"
         stdin%quote_chars   = "'"""
      end if
      call clear_(stdin%buffer)
      self => stdin

   end subroutine

   subroutine create_stdout(self)
    type(textfile_type) :: self
    ! Create a standard output file object.
    ! Return a pointer to it if already created
      pointer :: self

      if (.not. associated(stdout)) then
         allocate(stdout)

         call nullify_ptr_part_(stdout)
         stdout%name = "stdout"
         stdout%action = "write"
         stdout%record = 0
         stdout%unit = 6
         stdout%io_status = 0
         stdout%ignore_end_of_file = .false.
         stdout%no_of_lines = -1
         stdout%default_units = " "
         call set_default_format_(stdout)
      end if
      call clear_(stdout%buffer)
      call put_margin_(stdout)
      self => stdout

   end subroutine

   subroutine create(self,name)
    type(textfile_type) :: self
    ! Create a textfile, and optionally set the name. Does not open the file.
      pointer :: self
      character(*), optional :: name
      type(unitnumber_type) :: unit

      nullify(self)
      allocate(self)

      call nullify_ptr_part_(self)
      if (present(name)) then
        self%name = name
      else
        self%name = "unknown"
      end if
      self%action = "unknown"
      self%record = 0
      self%io_status = 0
      self%ignore_end_of_file = .false.
      self%no_of_lines = -1
      self%default_units = " "
      self%comment_chars = "!#"
      self%quote_chars   = "'"""
      call get_(unit,self%unit)  ! get a unique unit number
      call clear_(self%buffer)

   end subroutine

   subroutine create_1(self,internal)
    type(textfile_type) :: self
    ! Create an internal textfile
      pointer :: self
      character(len=*), dimension(:), intent(in) :: internal

      call ensure_(tonto,.not. associated(self%internal),"TEXTFILE:create_1 ... internal file already exists")
      nullify(self)
      allocate(self)

      call nullify_ptr_part_(self)
      self%name = "internal"
      self%record = 0
      self%io_status = 0
      self%ignore_end_of_file = .false.
      self%no_of_lines = size(internal)
      self%default_units = " "
      self%comment_chars = "!#"
      self%quote_chars   = "'"""
      self%unit = 0
      call create_(self%internal,size(internal))
      self%internal = internal
      self%action = "read"     ! only read action allowed
      call read_line_(self)

   end subroutine

   subroutine destroy(self)
    type(textfile_type) :: self
    ! Destroy a textfile
      pointer :: self
      type(unitnumber_type) :: unit

      if (.not. associated(self)) then;   return; end if
      call destroy_ptr_part_(self)
      if (is_open_(self)) call close_(self)
      if (do_io_(tonto_parallel)) then
        call free_(unit,self%unit)
      end if

      deallocate(self)

   end subroutine

   subroutine nullify_ptr_part(self)
    type(textfile_type) :: self
    ! Nullify the pointer parts of "self".

      nullify(self%internal)
      nullify(self%saved)

   end subroutine

   subroutine destroy_ptr_part(self)
    type(textfile_type) :: self
    ! Destroy the pointer parts of "self", including any saved parts,
    ! which are destroyed recursively.

      call destroy_(self%internal)
      call destroy_(self%saved)

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

   recursive subroutine create_copy(self,file)
    type(textfile_type) :: self
    ! Create a copy of this textfile
      pointer :: self
      type(textfile_type) :: file
      type(unitnumber_type) :: unit

      call create_(self," ")
      if (do_io_(tonto_parallel)) then
        call free_(unit,self%unit)
      end if
      call copy_(self,file)

   end subroutine

   recursive subroutine copy(self,file)
    type(textfile_type) :: self
    ! Make a copy of this textfile
      type(textfile_type) :: file

      self = file
      call copy_(self%buffer,file%buffer)
      if (associated(file%internal)) &
         call create_copy_(self%internal,file%internal)
      if (associated(file%saved)) &
         call create_copy_(self%saved,file%saved)

   end subroutine

!  **********************
!  Saving and redirection
!  **********************

   subroutine save(self)
    type(textfile_type) :: self
    ! Saved everything about the current textfile "self" in ".saved"
    ! Do not open a new textfile. Can be used to save style settings.
      pointer :: self
      type(textfile_type), pointer :: saved

      saved => self
      nullify(self)
      allocate(self)

      self = saved             ! Keep all settings, even line number
      self%saved => saved

   end subroutine

   subroutine unsave(self)
    type(textfile_type) :: self
    ! Revert to previously saved textfile settings. Note that this
    ! is not the same as reverting to a previously saved textfile
      type(textfile_type), pointer :: saved

      call ensure_(tonto,associated(self%saved),"TEXTFILE:unsave ... no previous settings")
      saved => self%saved
      self  = saved

      deallocate(saved)

   end subroutine

   subroutine redirect(self,name)
    type(textfile_type) :: self
    ! Save all the info for the current file in ".saved", and open a new
    ! textfile.  This is used for input, or output redirection. The new file
    ! retains the style settings of the saved file
      pointer :: self
      character(*) :: name
      type(textfile_type), pointer :: saved

      saved => self
      call create_(self,name)
      self%saved => saved
      call use_style_(self,saved)
      call open_(self,for=self%saved%action)

   end subroutine

   subroutine redirect_1(self,internal)
    type(textfile_type) :: self
    ! Save all the info for the current file in ".saved", and open a new
    ! internal textfile. This is used for *only* input redirection.
    ! The new file retains the style settings of the saved file
      pointer :: self
      character(len=*), dimension(:), intent(in) :: internal
      type(textfile_type), pointer :: saved

      saved => self
      nullify(self%internal)
      call create_(self,internal)
      self%saved => saved
      call use_style_(self,saved)

   end subroutine

   subroutine use_style(self,saved)
    type(textfile_type) :: self
    ! Revert to the previously saved style settings, including
    ! any default units.
      type(textfile_type) :: saved

      self%default_units = saved%default_units
      self%use_labels    = saved%use_labels
      self%margin_width  = saved%margin_width
      self%n_fields      = saved%n_fields
      self%int_width     = saved%int_width
      self%real_width     = saved%real_width
      self%real_precision = saved%real_precision
      self%real_style     = saved%real_style
      self%default_units = saved%default_units

   end subroutine

   subroutine revert(self)
    type(textfile_type) :: self
    ! Revert to the previously redirected textfile, but keep current style
    ! settings, including (for example) any default_units settings.
      type(textfile_type), pointer :: saved
      type(unitnumber_type) :: unit

      call ensure_(tonto,associated(self%saved),"TEXTFILE:revert ... no previous settings")
      saved => self%saved
      if (associated(self%internal)) then
         call destroy_(self%internal)       ! Do not destroy .saved recursively ...
      else
         call close_(self)
         call free_(unit,self%unit)
      end if
      call use_style_(saved,self)
      self  = saved
      self%io_status = -1            ! Soft-ending

      deallocate(saved)          ! instead, just deallocate it

   end subroutine

!  ******************************
!  Opening, closing, and deletion
!  ******************************

   subroutine open(self)
    type(textfile_type) :: self
    ! Open the textfile based on its action attribute

     call ensure_(tonto,self%action/="unknown","TEXTFILE:open ... file has unknown action")
     call open_(self,for=self%action)

   end subroutine

   subroutine open_1(self,for)
    type(textfile_type) :: self
    ! Open the textfile "for" either "read" or "write".
     character(*) :: for

     select case (for)
        case("read      ","reading   ","read-only ") ; call open_for_read_(self)
        case("write     ","writing   ","write-only") ; call open_for_write_(self)
        case default; allocate(tonto%known_keywords(6))
        tonto%known_keywords(1) = "read      "
        tonto%known_keywords(2) = "reading   "
        tonto%known_keywords(3) = "read-only "
        tonto%known_keywords(4) = "write     "
        tonto%known_keywords(5) = "writing   "
        tonto%known_keywords(6) = "write-only"
        call unknown_(tonto,for,"TEXTFILE:open_1")
        deallocate(tonto%known_keywords)
     end select

   end subroutine

   subroutine open_for_read(self)
    type(textfile_type) :: self
    ! Open the input file. The input file object must already be created
    ! The file is positioned at the first line.

      call ensure_(tonto,.not. associated(self%internal),"TEXTFILE:open_for_read ... no need to open an internal file")
      call ensure_(tonto,exists_(self),"TEXTFILE:open_for_read ... opening new file "//trim(self%name)//" for read!")
      call ensure_(tonto,self%action=="unknown" .or. self%action=="read","TEXTFILE:open_for_read ... not a readable file")
      self%action = "read"
      if (do_io_(tonto_parallel)) then
        open(unit=self%unit,        &
           file=trim(self%name),     &
           status="old",        &
           access="sequential", &
           form="formatted",    &
           iostat=self%io_status)
      end if
      call broadcast_(tonto_parallel,self%io_status,0)
      call ensure_(tonto,self%io_status==0,"TEXTFILE:open_for_read ... error opening old file "//trim(self%name))
      call rewind_(self)

   end subroutine

   subroutine open_for_write(self)
    type(textfile_type) :: self
    ! Open the output file associated with the output object

      call ensure_(tonto,.not. associated(self%internal),"TEXTFILE:open_for_write ... no need to open an internal file")
      call ensure_(tonto,self%action=="unknown" .or. self%action=="write","TEXTFILE:open_for_write ... not a writable file")
      self%action = "write"
      if (exists_(self)) then
                   call open_old_file_for_write_(self)
      else
                   call open_new_file_for_write_(self)
      end if

   end subroutine

   subroutine open_old_file_for_write(self)
    type(textfile_type) :: self
    ! Open an old output file for writing

      call ensure_(tonto,.not. associated(self%internal),"TEXTFILE:open_old_file_for_write ... no need to open an internal fi&
&le")
      call ensure_(tonto,exists_(self),"TEXTFILE:open_old_file_for_write ... not an existing file!")
      self%action = "write"
      if (do_io_(tonto_parallel)) then
        open(unit=self%unit,        &
           file=trim(self%name),     &
           status="old",        &
           access="sequential", &
           form="formatted",    &
           iostat=self%io_status)
      end if
      call broadcast_(tonto_parallel,self%io_status,0)
      call ensure_(tonto,self%io_status==0,"TEXTFILE:open_old_file_for_write ... error opening old file "//trim(self%name))
      call rewind_(self)
      call set_default_format_(self)
      call put_margin_(self)

   end subroutine

   subroutine open_new_file_for_write(self)
    type(textfile_type) :: self
    ! Open an new output file for writing

      call ensure_(tonto,.not. associated(self%internal),"TEXTFILE:open_new_file_for_write ... no need to open an internal fi&
&le")
      call ensure_(tonto,.not. exists_(self),"TEXTFILE:open_new_file_for_write ... output file exists!")
      self%action = "write"
      if (do_io_(tonto_parallel)) then
        open(unit=self%unit,        &
           file=trim(self%name),     &
           status="new",        &
           access="sequential", &
           form="formatted",    &
           iostat=self%io_status)
      end if
      call broadcast_(tonto_parallel,self%io_status,0)
      call ensure_(tonto,self%io_status==0,"TEXTFILE:open_new_file_for_write ... error opening new file "//trim(self%name))
      call rewind_(self)
      call set_default_format_(self)
      call put_margin_(self)

   end subroutine

   subroutine close(self)
    type(textfile_type) :: self
    ! Close the input file

      close(unit=self%unit)

   end subroutine

   subroutine close_and_delete(self)
    type(textfile_type) :: self
    ! Close the input file and delete it from the file system

      if (do_io_(tonto_parallel)) then
        close(unit=self%unit,status="delete")
      end if

   end subroutine

   subroutine delete(self)
    type(textfile_type) :: self
    ! Delete the input file from the file system

      if (.not. is_open_(self)) call open_(self,for="read")
      call close_and_delete_(self)

   end subroutine

!  ******************
!  Read style options
!  ******************

   subroutine read_keywords(self,in)
    type(textfile_type) :: self
    ! Read the default output style parameters from another textfile, "in"
      type(textfile_type) :: in
      character(128) :: word

      call ensure_(tonto,in%action=="read",'file "in" is not an input file')
      call read_(in,word)
      call ensure_(tonto,word=="{","TEXTFILE:read_keywords ... expecting a {")
      read_loop: do           ! Loop over keywords
         call read_(in,word)
         call to_lower_case_(word)
         if (word=="}")      exit read_loop
         call process_keyword_(self,word,in)
      end do read_loop

   end subroutine

   subroutine process_keyword(self,keyword,in)
    type(textfile_type) :: self
    ! Process a command "keyword". Data is inputted from "in", unless
    ! "word" is a sequence of blank separated strings. In this case,
    ! the sequence is processed as if it were a separate file.
      character(*) :: keyword
      type(textfile_type), target, optional :: in
      type(textfile_type), pointer :: input
      target :: self
      character(128) :: word,style
      logical(kind=kind(.true.)) :: bin
      integer(kind=kind(1)) :: val

      word = keyword
      call to_lower_case_(word)
      if (present(in)) then; input => in
      else;                  input => self
      end if
      select case (word)
        case ("real_precision="); call read_(input,val);   call set_real_precision_(self,val)
        case ("real_style=    "); call read_(input,style); call set_real_style_(self,style)
        case ("real_width=    "); call read_(input,val);   call set_real_width_(self,val)
        case ("fields=        "); call read_(input,val);   call set_n_fields_(self,val)
        case ("field_width=   "); call read_(input,val);   call set_real_width_(self,val)
        case ("int_width=     "); call read_(input,val);   call set_int_width_(self,val)
        case ("labels=        "); call read_(input,bin);   call set_use_labels_(self,bin)
        case ("margin=        "); call read_(input,val);   call set_margin_(self,val)
        case ("n_fields=      "); call read_(input,val);   call set_n_fields_(self,val)
        case ("precision=     "); call read_(input,val);   call set_real_precision_(self,val)
        case ("style=         "); call read_(input,style); call set_real_style_(self,style)
        case ("use_labels=    "); call read_(input,bin);   call set_use_labels_(self,bin)
        case default;          allocate(tonto%known_keywords(12))
        tonto%known_keywords(1) = "real_precision="
        tonto%known_keywords(2) = "real_style=    "
        tonto%known_keywords(3) = "real_width=    "
        tonto%known_keywords(4) = "fields=        "
        tonto%known_keywords(5) = "field_width=   "
        tonto%known_keywords(6) = "int_width=     "
        tonto%known_keywords(7) = "labels=        "
        tonto%known_keywords(8) = "margin=        "
        tonto%known_keywords(9) = "n_fields=      "
        tonto%known_keywords(10) = "precision=     "
        tonto%known_keywords(11) = "style=         "
        tonto%known_keywords(12) = "use_labels=    "
        call unknown_(tonto,word,"TEXTFILE:process_keyword")
        deallocate(tonto%known_keywords)
      end select

   end subroutine

!  **********************************
!  Line repositioning type operations
!  **********************************

   recursive subroutine read_line(self)
    type(textfile_type) :: self
    ! Read a line into the buffer. (The routine is recursive because there may
    ! need to be a reversion to a previously redirected file).

      if (associated(self%internal)) then; call read_line_internal_(self)
      else;                        call read_line_external_(self)
      end if

   end subroutine

   recursive subroutine read_line_internal(self)
    type(textfile_type) :: self
    ! Read a line into the buffer from the internal file. If the file end, then
    ! this routine dies, UNLESS (1) .ignore_end_of_file is set, in which case the
    ! another succesive failure to read a line will generate an error (this
    ! prevents infinite read loops, and is also useful for testing whether at the
    ! end of a file), .or. (2) there is a .saved file, in which case the current
    ! file reverts to the saved file, and an attempt is made to read from that
    ! saved file.
      character(256) :: string
      logical(kind=kind(.true.)) :: eliminate_specials

      call ensure_(tonto,associated(self%internal),"TEXTFILE:read_line_internal ... no internal file")
      call update_system_info_(self)
      if (self%record<size(self%internal)) then       ! No errors.
         self%io_status = 0
         self%record = self%record+1
         string = self%internal(self%record)
         call set_(self%buffer,string,self%comment_chars,self%quote_chars,eliminate_specials)
      else                                    ! At end of file.
         self%io_status = 1
         if (self%ignore_end_of_file) then        ! Ignore end of file ...
            self%ignore_end_of_file = .false.       ! Next time don't ignore
         else
            if(associated(self%saved)) then           ! It's OK, go back to saved file
               call revert_(self)
               call read_line_(self)
            else
               call die_(tonto,"TEXTFILE:read_line_internal ... unexpected end of file")  ! Die if nothing saved
            end if
         end if
      end if
      call update_system_info_(self)

   end subroutine

   recursive subroutine read_line_external(self)
    type(textfile_type) :: self
    ! Read a line into the buffer from the input file. If the file ends, this
    ! routine dies, UNLESS (1) .ignore_end_of_file is set, in which case another
    ! succesive failure to read a line will generate an error (this prevents
    ! infinite read loops, and is also useful for testing whether at the end of a
    ! file), .or. (2) there is a .saved file, in which case the current file
    ! reverts to the saved file, and an attempt is made to read that saved file.
      integer(kind=kind(1)) :: fail
      character(256) :: string
      logical(kind=kind(.true.)) :: eliminate_specials

      call ensure_(tonto,is_open_(self),"TEXTFILE:read_line_external ... file is not open")
      call ensure_(tonto,self%action=="read","TEXTFILE:read_line_external ... file does not have read action!")
      call update_system_info_(self)
      string = " "
      if (do_io_(tonto_parallel)) then
        fail = 2
        read(unit=self%unit,fmt="(a)",end=20,err=10) string
        fail = fail-1
20      fail = fail-1
10      continue
      end if
      call broadcast_(tonto_parallel,fail,0)
      call broadcast_(tonto_parallel,string,0)
      select case (fail)
         case (0)                                ! No errors.
            self%io_status = 0                       ! Read a line into the buffer.
            call set_(self%buffer,string,self%comment_chars,self%quote_chars,eliminate_specials)
            self%record = self%record + 1
         case (1)                                ! At end of file.
            self%io_status = 1
            if (self%ignore_end_of_file) then        ! Ignore end of file ...
               self%ignore_end_of_file = .false.       ! Next time don't ignore
               if (self%no_of_lines<0) then
                  self%no_of_lines = self%record
                  self%record = self%record + 1
               else
                  self%record = self%no_of_lines + 1
               end if
            else
               if(associated(self%saved)) then           ! It's OK, go back to saved file
                  call revert_(self)
                  call read_line_(self)                     ! Try again ...
               else
                  call die_(tonto,"TEXTFILE:read_line_external ... unexpected end of file")  ! Die if nothing saved
               end if
            end if
         case (2)                                ! Some error, but not end of file.
            call die_(tonto,"TEXTFILE:read_line_external ... read error")
      end select
      call update_system_info_(self)

   end subroutine

   subroutine update_line(self)
    type(textfile_type) :: self
    ! Get the next non-exhausted line if the current one is exhausted---and in
    ! this case, the buffer pointer is positioned before the first line item.
    ! This routine performs a reversion if there is a .saved file.

      do
         if (not_exhausted_(self%buffer)) exit
         call read_line_(self)
         if (end_of_file_(self)) exit
      end do

   end subroutine

   subroutine revert_line(self)
    type(textfile_type) :: self
    ! Revert back to the previous non-exhausted line in the input file if the
    ! current buffer pointer is before the first item; and in this case,
    ! the buffer pointer is repositioned *after* the last line item.

      do
         if (next_line_item_(self)>1) exit
         call backspace_line_(self)
         call move_to_line_item_(self,last_line_item_(self)+1)
      end do

   end subroutine

   subroutine skip_next_item(self)
    type(textfile_type) :: self
    ! Move to the next item in the input file.

      call update_line_(self)
      call skip_item_(self%buffer)

   end subroutine

   subroutine move_to_previous_item(self)
    type(textfile_type) :: self
    ! Move to the previous item in the input file. Backspace a line if required.

      call revert_line_(self)
      call move_to_previous_item_on_line_(self)      ! move to the last read item

   end subroutine

   subroutine move_to_last_item_on_line(self)
    type(textfile_type) :: self
    ! Move the cursor over to the beginning of the last item on the line
      integer(kind=kind(1)) :: item

      item = last_line_item_(self)
      call move_to_line_item_(self,item)

   end subroutine

   subroutine move_to_previous_item_on_line(self)
    type(textfile_type) :: self
    ! Move the cursor over to the beginning of the previous item on the
      integer(kind=kind(1)) :: item

      item = previous_line_item_(self)
      call move_to_line_item_(self,item)

   end subroutine

   subroutine move_to_line_item(self,number)
    type(textfile_type) :: self
    ! Move the cursor over to the beginning of the item with index "number"
    ! on the current line. (More accurately: after the end of the previous item
    ! on the current line. So if there are "nitems" on the line you can move to
    ! "nitem+1" if you want to, and that will be the end of the line).
      integer(kind=kind(1)) :: number
       integer(kind=kind(1)) :: n

      call update_system_info_(self)
      n = number
      call move_to_item_(self%buffer,n)

   end subroutine

   subroutine look_for_item(self,item,from,until,end_tokens,exact_match,head_match,return_cursor,found)
    type(textfile_type) :: self
    ! Scans through the file for a token which includes the given "item" (the
    ! token in the file must be a separate thing surrounded by whitespace, begin
    ! or end of line, or comment characters; whereas "item" may may match only a
    ! part of a given token). If a token matching "item" exists, the file cursor
    ! is left just after this token. If no match is found, the file is rewound to
    ! the initial line of the search.  If "from" is present then it is used as
    ! the start line for the search. If "until" is present, it is used as the
    ! last line of the search. If "end_tokens" is present then an exact match to
    ! any of these tokens indicates the end of search for "item".  If
    ! "exact_match" is present and .true., then "item" must match exactly the token
    ! in the file, instead of just being included in it. If "head_match" is
    ! present and .true., then occurs only if the characters at the head of the
    ! "item" string match the token in the file. only If "return_cursor" is
    ! present and .false., the cursor is not returned to the starting line of the
    ! search in the case where no match is found. If "found" is present, it is
    ! set .true. when the item is found, else .false..
      character(*), intent(in) :: item
      integer(kind=kind(1)), intent(in), optional :: from,until
      character(len=*), dimension(:), intent(in), optional :: end_tokens
      logical(kind=kind(.true.)), intent(in), optional :: exact_match,head_match,return_cursor
      logical(kind=kind(.true.)), intent(out), optional :: found
      integer(kind=kind(1)) :: start_record
      character(128) :: word
      logical(kind=kind(.true.)) :: fnd,exact,head,put_back,has_item,has_end,match
      integer(kind=kind(1)) :: i

      self%ignore_end_of_file = .true.
      fnd = .false.
      start_record = self%record                 ! Initialise switches and counters
      if (present(from)) then
         start_record = from
         if (from==1) then; call rewind_(self)          ! The following may yeild an EOF
         else;              call move_to_record_(self,from)
         end if
      end if
      put_back = .true.; if (present(return_cursor)) put_back = return_cursor
      if (.not. end_of_file_(self)) then
        if (present(until)) then
        call ensure_(tonto,until>=start_record,"TEXTFILE:look_for_item ... ending line must be greater than starting line")
        end if
        exact = .false.; if (present(exact_match)) exact = exact_match
        head = .false.;  if (present(head_match))  head = head_match
        call ensure_(tonto,.not. (head .and. exact),"TEXTFILE:look_for_item ... don't specify head *and* exact match")
        has_end = .false.
        line_search: do                        ! Now do the search here ...
           has_item = includes_(self%buffer%string,item)
           if (present(end_tokens) .and. self%record>start_record) &
           has_end = includes_any_in_(self%buffer%string,end_tokens)
           if (has_item .or. has_end) then
              do i = 1,self%buffer%n_items
                 call get_next_item_(self,word)
             ! stdout.flush
             ! stdout.show("line    =",.record)
             ! stdout.show("   i    =",i)
             ! stdout.show("   word =",word)
             ! stdout.show("   item =",item)
                 if (has_item) then
                    if (head) then;           match = includes_(word,item,at_start=.true.)
                    else if (exact) then;     match = word==item
                    else if (.not. exact) then; match = includes_(word,item)
                    end if
                    if (match) then            ! Found a match
                       fnd = .true.; exit line_search
                    end if
                 end if
                 if (has_end) then
                    if (has_any_included_in_(end_tokens,word,at_start=.true.)) exit line_search
                 end if
              end do
           end if
           if (present(until)) then
              if (self%record==until) exit line_search
           end if
           call read_line_(self)
           if (end_of_file_(self)) exit line_search
        end do line_search
      end if
      if (present(found)) found = fnd
      if (.not. fnd .and. put_back) call move_to_record_(self,start_record)
      self%ignore_end_of_file = .false.

   end subroutine

   subroutine look_backwards_for_item(self,item,from,until,end_tokens,exact_match,head_match,return_cursor,found)
    type(textfile_type) :: self
    ! Scans *backwards* through the file for a token which includes the given
    ! "item" (the token in the file must be a separate thing surrounded by
    ! whitespace, begin or end of line, or comment characters; whereas "item" may
    ! may match only a part of a given token). If a token matching "item" exists,
    ! the file cursor is left just after this token. If no match is found, the
    ! file is rewound to the initial line of the search.  If "from" is present
    ! then it is used as the start line for the search. If "until" is present, it
    ! is used as the last line of the search. If "end_tokens" is present then an
    ! exact match to any of these tokens indicates the end of search for "item".
    ! If "exact_match" is present and .true., then "item" must match exactly the
    ! token in the file, instead of just being included in it. If "head_match" is
    ! present and .true., then occurs only if the characters at the head of the
    ! "item" string match the token in the file. only If "return_cursor" is
    ! present and .false., the cursor is not returned to the starting line of the
    ! search in the case where no match is found. If "found" is present, it is
    ! set .true. when the item is found, else .false..
      character(*), intent(in) :: item
      integer(kind=kind(1)), intent(in), optional :: from,until
      character(len=*), dimension(:), intent(in), optional :: end_tokens
      logical(kind=kind(.true.)), intent(in), optional :: exact_match,head_match,return_cursor
      logical(kind=kind(.true.)), intent(out), optional :: found
      integer(kind=kind(1)) :: start_record,end_record
      character(128) :: word
      logical(kind=kind(.true.)) :: fnd,exact,head,put_back,has_item,has_end,match
      integer(kind=kind(1)) :: i

      self%ignore_end_of_file = .true.
      fnd = .false.
      start_record = self%record                 ! Initialise switches and counters
      if (present(from)) start_record = from
      call move_to_record_(self,start_record)          ! Possible EOF here ...
      put_back = .true.; if (present(return_cursor)) put_back = return_cursor
      if (.not. end_of_file_(self)) then
        call move_to_last_item_on_line_(self)
        end_record = 1; if (present(until)) end_record = until
        call ensure_(tonto,end_record<=start_record,"TEXTFILE:look_backwards_for_item ... ending line must be smaller than st&
&arting line")
        exact = .false.; if (present(exact_match)) exact = exact_match
        head = .false.;  if (present(head_match))  head = head_match
        call ensure_(tonto,.not. (head .and. exact),"TEXTFILE:look_backwards_for_item ... don't specify head *and* exact matc&
&h")
        has_end = .false.
        line_search: do                        ! Now do the search here ...
           has_item = includes_(self%buffer%string,item)
           if (present(end_tokens) .and. self%record>start_record) &
           has_end = includes_any_in_(self%buffer%string,end_tokens)
           if (has_item .or. has_end) then
              do i = self%buffer%n_items,1,-1      ! Look backwards ...
                 call move_to_line_item_(self,i)
                 call get_next_item_(self,word)
                 if (has_item) then
                    if (head) then;           match = includes_(word,item,at_start=.true.)
                    else if (exact) then;     match = word==item
                    else if (.not. exact) then; match = includes_(word,item)
                    end if
                    if (match) then            ! Found a match
                       fnd = .true.; exit line_search
                    end if
                 end if
                 if (has_end) then
                    if (has_any_included_in_(end_tokens,word,at_start=.true.)) exit line_search
                 end if
              end do
           end if
           if (self%record==end_record) exit line_search
           call backspace_line_(self)                     ! Move to previous line
        end do line_search
      end if
      if (present(found)) found = fnd
      if (.not. fnd .and. put_back) call move_to_record_(self,start_record)
      self%ignore_end_of_file = .false.

   end subroutine

   subroutine look_for_any_item(self,item,from,until,end_tokens,exact_match,head_match,return_cursor,found)
    type(textfile_type) :: self
    ! Scans through the file for a token which includes the any of the given
    ! elements in the "item" list (the token in the file must be a separate thing
    ! surrounded by whitespace, begin or end of line, or comment characters;
    ! whereas "item" may may match only a part of a given token). If a token
    ! matching "item" exists, the file cursor is left just after this token. If
    ! no match is found, the file is rewound to the initial line of the search.
    ! If "from" is present then it is used as the start line for the search. If
    ! "until" is present, it is used as the last line of the search. If
    ! "end_tokens" is present then an exact match to any of these tokens
    ! indicates the end of search for "item".  If "exact_match" is present and
    ! .true., then "item" must match exactly the token in the file, instead of just
    ! being included in it. If "head_match" is present and .true., then occurs only
    ! if the characters at the head of the "item" string match the token in the
    ! file. only If "return_cursor" is present and .false., the cursor is not
    ! returned to the starting line of the search in the case where no match is
    ! found. If "found" is present, it is set .true. when the item is found, else
    ! .false..
      character(len=*), dimension(:), intent(in) :: item
      integer(kind=kind(1)), intent(in), optional :: from,until
      character(len=*), dimension(:), intent(in), optional :: end_tokens
      logical(kind=kind(.true.)), intent(in), optional :: exact_match,head_match,return_cursor
      logical(kind=kind(.true.)), intent(out), optional :: found
      integer(kind=kind(1)) :: start_record
      character(128) :: word
      logical(kind=kind(.true.)) :: fnd,exact,head,put_back,has_item,has_end,match
      integer(kind=kind(1)) :: i,j

      self%ignore_end_of_file = .true.
      fnd = .false.
      start_record = self%record                 ! Initialise switches and counters
      if (present(from)) then
         start_record = from
         if (from==1) then; call rewind_(self)          ! The following may yeild an EOF
         else;              call move_to_record_(self,from)
         end if
      end if
      put_back = .true.; if (present(return_cursor)) put_back = return_cursor
      if (.not. end_of_file_(self)) then
        if (present(until)) then
        call ensure_(tonto,until>=start_record,"TEXTFILE:look_for_any_item ... ending line must be greater than starting line&
&")
        end if
        exact = .false.; if (present(exact_match)) exact = exact_match
        head = .false.;  if (present(head_match))  head = head_match
        call ensure_(tonto,.not. (head .and. exact),"TEXTFILE:look_for_any_item ... don't specify head *and* exact match")
        has_end = .false.
        line_search: do                        ! Now do the search here ...
           has_item = includes_any_in_(self%buffer%string,item)
           if (present(end_tokens) .and. self%record>start_record) &
           has_end = includes_any_in_(self%buffer%string,end_tokens)
           if (has_item .or. has_end) then
              do i = 1,self%buffer%n_items
                 call get_next_item_(self,word)
                 if (has_item) then
                    if (head) then
                       do j = 1,size(item)
                          match = includes_(word,item(j),at_start=.true.)
                          if (match) exit
                       end do
                    else if (exact) then
                       do j = 1,size(item)
                          match = word==item(j)
                          if (match) exit
                       end do
                    else if (.not. exact) then
                       do j = 1,size(item)
                          match = includes_(word,item(j))
                          if (match) exit
                       end do
                    end if
                    if (match) then            ! Found a match
                       fnd = .true.; exit line_search
                    end if
                 end if
                 if (has_end) then
                    if (has_any_included_in_(end_tokens,word,at_start=.true.)) exit line_search
                 end if
              end do
           end if
           if (present(until)) then
              if (self%record==until) exit line_search
           end if
           call read_line_(self)
           if (end_of_file_(self)) exit line_search
        end do line_search
      end if
      if (present(found)) found = fnd
      if (.not. fnd .and. put_back) call move_to_record_(self,start_record)
      self%ignore_end_of_file = .false.

   end subroutine

   subroutine look_for(self,item,from,until,first,found)
    type(textfile_type) :: self
    ! Scans through the file for a line which includes string "item".  If there,
    ! the file record is left at the first line at which the match occured.  If
    ! no match is found, the file is rewound to the initial line before the
    ! search.  If "from" is present then it is used as the start line for the
    ! search, and if "item" is not found the file record is returned to that
    ! line.  If "until" is present then matches to these tokens are used to
    ! indicate the end of search condition for "item". If "first" is present and
    ! .true., then the item is matched only if it is the first non-blank token in
    ! the input, and likewise the search is terminated only when the "until"
    ! tokens are the first characters in the input. If "found" is present, it is
    ! set .true. when the item is found, else .false..
      character(*), intent(in) :: item
      integer(kind=kind(1)), intent(in), optional :: from
      character(len=*), dimension(:), intent(in), optional :: until
      logical(kind=kind(.true.)), intent(in), optional :: first
      logical(kind=kind(.true.)), intent(out), optional :: found
      integer(kind=kind(1)) :: start_record
      character(128) :: word

      start_record = self%record
      if (present(from)) then
         if (from==1) then
            call rewind_(self)
            start_record = 1
         else
            call move_to_record_(self,from)
            start_record = from
         end if
      end if
      if (present(found)) found = .false.
      self%ignore_end_of_file = .true.
      line_search: do
         word = trim_blanks_from_start_(self%buffer%string)
         if (is_included_in_(item,word,first)) then
            if (present(found)) found = .true.
            exit line_search
         end if
         if (present(until) .and. self%record>start_record) then
         if (has_any_included_in_(until,word,first)) then
            call move_to_record_(self,start_record)
            if (present(found)) found = .false.
            exit line_search
         end if
         end if
         call read_line_(self)
         if (end_of_file_(self)) then
            call move_to_record_(self,start_record)
            if (present(found)) found = .false.
            exit line_search
         end if
      end do line_search
      self%ignore_end_of_file = .false.

   end subroutine

   subroutine look_backwards_for(self,item,from,until,first,found)
    type(textfile_type) :: self
    ! Scans backward through the file for a line which includes string "item".
    ! If there, the file record is left at the first line at which the match
    ! occured.  If no match is found, the file is rewound to the initial line
    ! before the search.  If "from" is present then it is used as the start line
    ! for the search, and if "item" is not found the file record is returned to
    ! that line.  If "until" is present then matches to these tokens are used to
    ! indicate the end of search condition for "item". If "first" is present and
    ! .true., then the item is matched only if it is the first non-blank token in
    ! the input, and likewise the search is terminated only when the "until"
    ! tokens are the first characters in the input. If "found" is present, it is
    ! set .true. when the item is found, else .false..
      character(*), intent(in) :: item
      integer(kind=kind(1)), intent(in), optional :: from
      character(len=*), dimension(:), intent(in), optional :: until
      logical(kind=kind(.true.)), intent(in), optional :: first
      logical(kind=kind(.true.)), intent(out), optional :: found
      integer(kind=kind(1)) :: start_record
      character(128) :: word

      start_record = self%record
      if (present(from)) then
         call move_to_record_(self,from)
         start_record = from
      end if
      if (present(found)) found = .false.
      self%ignore_end_of_file = .true.
      do
         word = trim_blanks_from_start_(self%buffer%string)
         if (is_included_in_(item,word,first)) then
            if (present(found)) found = .true.
            exit
         end if
         if (self%record==1) then  ! This is the start of the file
            call move_to_record_(self,start_record)
            if (present(found)) found = .false.
            exit
         end if
         call backspace_line_(self)
         if (present(until) .and. self%record<start_record) then
         if (has_any_included_in_(until,word,first)) then
            call move_to_record_(self,start_record)
            if (present(found)) found = .false.
            exit
         end if
         end if
      end do
      self%ignore_end_of_file = .false.

   end subroutine

   function has_string(self,search) result(res)
    type(textfile_type) :: self
    ! Returns .true. if the file contains string "search". The file is returned
    ! to its original line number after this routine.
      character(*) :: search
      logical(kind=kind(.true.)) :: res
      integer(kind=kind(1)) :: update_record
      logical(kind=kind(.true.)) :: found

      update_record = self%record
      call rewind_(self)
      self%ignore_end_of_file = .true.
      found = .false.
      do
         if (is_included_in_(search,self%buffer%string)) then
           found = .true.
           exit
         end if
         call read_line_(self)
         if (end_of_file_(self)) exit
      end do
      call move_to_record_(self,update_record)
      self%ignore_end_of_file = .false.
      res = found

   end function

   function rest_of_line(self) result(res)
    type(textfile_type) :: self
    ! If there is anything left on the input line, then it is returned, else
    ! nothing is returned.  Record moves to the next line.
     intent(inout) :: self
     character(128) :: res

     call ensure_(tonto,self%action=="read","TEXTFILE:rest_of_line ... file does not have read action!")
     if (.not. empty_(self%buffer)) then
       res = self%buffer%string(self%buffer%item_end+1: )
       call read_line_(self)
     else
       res = " "
       call read_line_(self)
     end if

   end function

   subroutine rewind(self)
    type(textfile_type) :: self
    ! Rewind the input file, reading the first line for read-only files.

      if (associated(self%internal)) then; call rewind_internal_(self)
      else;                        call rewind_external_(self)
      end if
      select case (self%action)
         case("read      ","reading   ","read-only "); call read_line_(self)
         case("write     ","writing   ","write-only")
         case default; allocate(tonto%known_keywords(6))
         tonto%known_keywords(1) = "read      "
         tonto%known_keywords(2) = "reading   "
         tonto%known_keywords(3) = "read-only "
         tonto%known_keywords(4) = "write     "
         tonto%known_keywords(5) = "writing   "
         tonto%known_keywords(6) = "write-only"
         call unknown_(tonto,self%action,"TEXTFILE:rewind")
         deallocate(tonto%known_keywords)
      end select

   end subroutine

   subroutine rewind_internal(self)
    type(textfile_type) :: self
    ! Rewind the input file, but do not read a line.

      self%record = 0
      self%io_status = 0

   end subroutine

   subroutine rewind_external(self)
    type(textfile_type) :: self
    ! Rewind the input file, but do not read a line.

      call update_system_info_(self)
      if (do_io_(tonto_parallel)) then
        rewind(unit=self%unit,iostat=self%io_status)
      end if
      call broadcast_(tonto_parallel,self%io_status,0)
      call die_if_(tonto,self%io_status>0,"TEXTFILE:rewind_external ... rewind error")
      self%record = 0
      self%io_status = 0

   end subroutine

   subroutine move_to_end(self)
    type(textfile_type) :: self
    ! Move to the end of the input file

      if (associated(self%internal)) then; call move_to_end_internal_(self)
      else;                        call move_to_end_external_(self)
      end if

   end subroutine

   subroutine move_to_end_internal(self)
    type(textfile_type) :: self
    ! Move to the end of the input file

      self%record = size(self%internal)-1
      call read_line_(self)

   end subroutine

   subroutine move_to_end_external(self)
    type(textfile_type) :: self
    ! Move to the end of the input file

      call update_system_info_(self)
      do
        if (do_io_(tonto_parallel)) then
          read(unit=self%unit, fmt="()", iostat=self%io_status)
        end if
        call broadcast_(tonto_parallel,self%io_status,0)
        if (self%io_status/=0) exit
        self%record = self%record + 1
      end do
      if (do_io_(tonto_parallel)) then
        backspace(unit=self%unit,iostat=self%io_status)
        backspace(unit=self%unit,iostat=self%io_status)
      end if
      call broadcast_(tonto_parallel,self%io_status,0)
      self%record = max(0,self%record-1)
      call read_line_(self)

   end subroutine

   subroutine backspace_line(self)
    type(textfile_type) :: self
    ! Reprocess previously input line

      call move_to_record_(self,self%record-1)

   end subroutine

   subroutine skip_line(self)
    type(textfile_type) :: self
    ! Skip the next line in the input file

      call move_to_record_(self,self%record+1)

   end subroutine

   subroutine move_to_line(self,line)
    type(textfile_type) :: self
    ! Make sure that "line" was the last one processed
    ! in the input file
      integer(kind=kind(1)) :: line

      call ensure_(tonto,line>=0,"TEXTFILE:move_to_line ... line number must be non-negative")
      call move_to_record_(self,line)

   end subroutine

   subroutine move_to_record(self,rec)
    type(textfile_type) :: self
    ! Move to the requested record "rec" in the input file.
    ! Remember: .record is the last processed record.
      integer(kind=kind(1)) :: rec

      if (associated(self%internal)) then; call move_to_record_internal_(self,rec)
      else;                        call move_to_record_external_(self,rec)
      end if

   end subroutine

   subroutine move_to_record_internal(self,rec)
    type(textfile_type) :: self
    ! Move to the requested record "rec" in the internal file.
    ! Remember: .record is the last processed record.
      integer(kind=kind(1)) :: rec

      call ensure_(tonto,rec>=0,"TEXTFILE:move_to_record_internal ... record number must be non-negative")
      call ensure_(tonto,rec<=size(self%internal),"TEXTFILE:move_to_record_internal ... record outside file range")
      self%record = rec-1
      call read_line_(self)

   end subroutine

   subroutine move_to_record_external(self,rec)
    type(textfile_type) :: self
    ! Move to the requested record "rec" in the input file.
    ! Remember: .record is the last processed record.
      integer(kind=kind(1)) :: rec

      call ensure_(tonto,rec>=0,"TEXTFILE:move_to_record_external ... record number must be non-negative")
      call update_system_info_(self)
      if (rec<(self%record+1)) then
         do
           if (do_io_(tonto_parallel)) then
             backspace(unit=self%unit,iostat=self%io_status)
           end if
           call broadcast_(tonto_parallel,self%io_status,0)
           call die_if_(tonto,self%io_status>0,"TEXTFILE:move_to_record_external ... backspace error")
           self%record = self%record-1
           self%io_status = 0
           if (rec==(self%record+1)) exit
         end do
      else if (rec>(self%record+1)) then
         do
           if (do_io_(tonto_parallel)) then
             read(unit=self%unit,fmt="()",iostat=self%io_status)
           end if
           call broadcast_(tonto_parallel,self%io_status,0)
           call die_if_(tonto,self%io_status>0,"TEXTFILE:move_to_record_external ... read error")
           self%record = self%record + 1
           if (rec==(self%record+1)) exit
         end do
      end if
      call read_line_(self)

   end subroutine

!  **************************
!  Data input type operations
!  **************************

   subroutine get_next_item(self,word)
    type(textfile_type) :: self
    ! Get the next item in the file
      character(128) :: word

      call update_line_(self)
      call get_(self%buffer,word)

   end subroutine

   recursive subroutine read_str(self,word)
    type(textfile_type) :: self
    ! Read a str into "word".
     character(*) :: word
     character(128) :: the_word

     call ensure_(tonto,self%action=="read","TEXTFILE:read_str ... file does not have read action!")
     call update_line_(self)
     call get_(self%buffer,the_word)
     if (len_trim(the_word)>len(word)) then
        call die_(tonto,"TEXTFILE:read_str ... Length of word is too long: " // trim(word))
     end if
     word = the_word

   end subroutine

   subroutine read_real_quantity(self,value)
    type(textfile_type) :: self
    ! Read a real(kind=kind(1.0d0)) quantity into "value". Will convert the value of the quantity
    ! from atomic units, if followed by a unit specifier.
    ! WARNING: You must *not* be at the end of file to use this routine, since
    ! it will try to read the next token after the real(kind=kind(1.0d0)) as a units specifier.
    ! (There will usually be at least a close brackets symbol "}" after this
    ! data in TONTO input files).
      real(kind=kind(1.0d0)) :: value
      character(128) :: units
      logical(kind=kind(.true.)) :: known_unit

      call ensure_(tonto,self%action=="read","TEXTFILE:read_real_quantity ... file does not have read action!")
      call read_real_(self,value)
      if (.not. at_end_of_file_(self)) then
        call read_str_(self,units)                       ! Read possible unit string
        known_unit = is_known_unit_(units)    ! If unknown unit, move back
        if (.not. known_unit) call move_to_previous_item_(self)
        if (known_unit) then
           call convert_from_(value,units)               ! Convert from known units
        else if (self%default_units/=" ") then
           call convert_from_(value,self%default_units)      ! Convert from assumed default units
           self%default_units = " "
        end if
      else if (self%default_units/=" ") then
        call convert_from_(value,self%default_units)      ! Convert from assumed default units
        self%default_units = " "
      end if

   end subroutine

   subroutine read_real(self,value)
    type(textfile_type) :: self
    ! Read a real(kind=kind(1.0d0)) into "value"
     real(kind=kind(1.0d0)) :: value

      call ensure_(tonto,self%action=="read","TEXTFILE:read_real ... file does not have read action!")
     call update_line_(self)
     call get_(self%buffer,value)

   end subroutine

   subroutine read_formatted_real(self,value,format)
    type(textfile_type) :: self
    ! Read a formatted real(kind=kind(1.0d0)) into "value". Does not check the end of line!
     real(kind=kind(1.0d0)) :: value
     character(*), intent(in) :: format

      call ensure_(tonto,self%action=="read","TEXTFILE:read_formatted_real ... file does not have read action!")
     call update_line_(self)
     call get_(self%buffer,value,format)

   end subroutine

   subroutine read_imprecise_real(self,value,error)
    type(textfile_type) :: self
    ! Read a real(kind=kind(1.0d0)) into "value" and also its associated "error", which appears in
    ! parentheses after it.
     real(kind=kind(1.0d0)) :: value,error

      call ensure_(tonto,self%action=="read","TEXTFILE:read_imprecise_real ... file does not have read action!")
     call update_line_(self)
     call get_(self%buffer,value,error)

   end subroutine

   subroutine read_cpx(self,value)
    type(textfile_type) :: self
    ! Read a cpx into "value"
      complex(kind=kind((1.0d0,1.0d0))) :: value
     ! real,imag :: real(kind=kind(1.0d0))

      call ensure_(tonto,self%action=="read","TEXTFILE:read_cpx ... file does not have read action!")
      call update_line_(self)
     ! .buffer.get(real)
     ! .update_line
     ! .buffer.get(imag)
     ! value = cmplx(real,imag,kind=kind((1.0d0,1.0d0)))
      call get_(self%buffer,value)

   end subroutine

   subroutine read_int(self,value)
    type(textfile_type) :: self
    ! Read an integer into "value"
      integer(kind=kind(1)) :: value

      call ensure_(tonto,self%action=="read","TEXTFILE:read_int ... file does not have read action!")
      call warn_if_(tonto,self%default_units/=" ","TEXTFILE:read_int ... default units ignored for integers")
      call update_line_(self)
      call get_(self%buffer,value)

   end subroutine

   subroutine read_bin(self,value)
    type(textfile_type) :: self
    ! Read a logical into "value"
      logical(kind=kind(.true.)) :: value

      call ensure_(tonto,self%action=="read","TEXTFILE:read_bin ... file does not have read action!")
      call warn_if_(tonto,self%default_units/=" ","TEXTFILE:read_bin ... default units ignored for logicals")
      call update_line_(self)
      call get_(self%buffer,value)

   end subroutine

   function list_length(self,ignore_opening_brace) result(res)
    type(textfile_type) :: self
    ! Return the size of the list, by reading the input after an initial opening
    ! bracket "{" until a *matching* end bracket "}" token is found. If
    ! "ignore_opening_brace" is present, the initial curly bracket is not
    ! required. Line breaks are not significant.
      logical(kind=kind(.true.)), optional :: ignore_opening_brace
      integer(kind=kind(1)) :: res
      integer(kind=kind(1)) :: line,item,n
      character(128) :: word

      line = line_number_(self)
      item = next_line_item_(self)  ! next one to be read
      if (.not. present(ignore_opening_brace)) then
      call read_(self,word)
      call ensure_(tonto,word=="{","TEXTFILE:list_length ... list does not begin with {")
      end if
      n = 0
      res = 0
      do
         call read_(self,word)
         if (word=="{") n = n + 1
         if (word=="}" .and. n==0) exit
         if (word=="}" .and. n>0) n = n - 1
         res = res+1
      end do
      call move_to_line_(self,line)
      call move_to_line_item_(self,item)

   end function

   function list_list_length(self) result(res)
    type(textfile_type) :: self
    ! Return the size of an INTVECVEC list, by reading the input until an end
    ! bracket "}" token is found. Line breaks are not significant.
      integer(kind=kind(1)) :: res
      integer(kind=kind(1)) :: line,item
      character(128) :: word
      character(128), dimension(:), pointer :: v

      line = line_number_(self)
      item = next_line_item_(self)
      call read_(self,word)
      call ensure_(tonto,word=="{","TEXTFILE:list_list_length ... list does not begin with {")
      res = 0
      do
         call read_str_(self,word)
         if (word=="}") exit
         call move_to_previous_item_(self)
         call read_strvec_ptr_(self,v)
         res = res+1
      end do
      call move_to_line_(self,line)
      call move_to_line_item_(self,item)

   end function

   subroutine read_binvec(self,v)
    type(textfile_type) :: self
    ! Read in a logical(kind=kind(.true.)) vector sequentially. Line breaks are not significant.
      logical(kind=kind(.true.)), dimension(:) :: v
      integer(kind=kind(1)) :: dim,i

      dim = size(v)
      do i = 1,dim
         call read_bin_(self,v(i))
      end do

   end subroutine

   subroutine read_binvec_ptr(self,v,ignore_opening_brace)
    type(textfile_type) :: self
    ! Read in a logical(kind=kind(.true.)) vector pointer "v" sequentially. Line breaks are not
    ! significant. If "ignore_opening_brace" is present, the opening curly brace
    ! "{" is not required; however, the closing curly brace "}" is always
    ! required. The "v" vector pointer is created to be the right size.
      logical(kind=kind(.true.)), optional :: ignore_opening_brace
      logical(kind=kind(.true.)), dimension(:), pointer :: v
      integer(kind=kind(1)) :: dim
      character(128) :: word

      dim = list_length_(self,ignore_opening_brace)
      call create_(v,dim)
      if (.not. present(ignore_opening_brace)) then
      call read_(self,word)
      call ensure_(tonto,word=="{","TEXTFILE:read_binvec_ptr ... list does not begin with a {")
      end if
      call read_binvec_(self,v)
      call read_(self,word)
      call ensure_(tonto,word=="}","TEXTFILE:read_binvec_ptr ... list does not end with a {")

   end subroutine

   subroutine read_strvec(self,v)
    type(textfile_type) :: self
    ! Read in an string vector sequentially. Line breaks are not significant.
      character(128), dimension(:) :: v
      integer(kind=kind(1)) :: dim,i

      dim = size(v)
      do i = 1,dim
         call read_str_(self,v(i))
      end do

   end subroutine

   subroutine read_strvec_ptr(self,v,ignore_opening_brace)
    type(textfile_type) :: self
    ! Read in a string vector pointer "v" sequentially. Line breaks are not
    ! significant. If "ignore_opening_brace" is present, the opening curly
    ! brace "{" is not required; however, the closing curly brace "}" is always
    ! required. The "v" vector pointer is created to be the right size.
      character(128), dimension(:), pointer :: v
      logical(kind=kind(.true.)), optional :: ignore_opening_brace
      integer(kind=kind(1)) :: dim
      character(128) :: word

      dim = list_length_(self,ignore_opening_brace)
      call create_(v,dim)
      if (.not. present(ignore_opening_brace)) then
      call read_(self,word)
      call ensure_(tonto,word=="{","TEXTFILE:read_strvec_ptr ... list does not begin with a {")
      end if
      call read_strvec_(self,v)
      call read_(self,word)
      call ensure_(tonto,word=="}","TEXTFILE:read_strvec_ptr ... list does not end with a {")

   end subroutine

   subroutine read_intvec(self,v)
    type(textfile_type) :: self
    ! Read in an integer vector sequentially. Line breaks are not significant.
      integer(kind=kind(1)), dimension(:) :: v
      integer(kind=kind(1)) :: dim,i

      dim = size(v)
      do i = 1,dim
         call read_int_(self,v(i))
      end do

   end subroutine

   subroutine read_intvec_ptr(self,v,ignore_opening_brace)
    type(textfile_type) :: self
    ! Read in an integer vector pointer "v" sequentially. Line breaks are not
    ! significant.  If "ignore_opening_brace" is present, the opening curly brace
    ! "{" is not required; however, the closing curly brace "}" is always
    ! required. The "v" vector pointer is created to be the right size.
    ! NOTE: this will not handle zero length INTVEC* arrays.
      integer(kind=kind(1)), dimension(:), pointer :: v
      logical(kind=kind(.true.)), optional :: ignore_opening_brace
      integer(kind=kind(1)) :: dim,f,l,i
      character(128) :: first,word,last

      if (.not. present(ignore_opening_brace)) then
      call read_(self,word)
      call ensure_(tonto,word=="{","TEXTFILE:read_intvec_ptr ... list does not begin with a {")
      end if
      call read_str_(self,first)       ! Read the first item
      call read_str_(self,word)
      if (word=="...") then  ! this is a looping INTVEC ptr
         call read_str_(self,last)
         l = to_int_(last)
         f = to_int_(first)
         dim = l - f + 1
         call create_(v,abs(dim))
         if (dim>0) then; v = (/ (i, i=f,l)    /)
         else           ; v = (/ (i, i=f,l,-1) /)
         end if
      else
         call move_to_previous_item_(self)
         call move_to_previous_item_(self)
         if (.not. present(ignore_opening_brace)) &
         call move_to_previous_item_(self)
         dim = list_length_(self,ignore_opening_brace)
         call create_(v,dim)
         if (.not. present(ignore_opening_brace)) then
         call read_(self,word)
         call ensure_(tonto,word=="{","TEXTFILE:read_intvec_ptr ... list does not begin with a {")
         end if
         call read_intvec_(self,v)
      end if
      call read_(self,word)
      call ensure_(tonto,word=="}","TEXTFILE:read_intvec_ptr ... list does not end with a {")

   end subroutine

   subroutine read_intvecvec(self,v)
    type(textfile_type) :: self
    ! Read in an integer vector vector "v" sequentially. Line breaks are not
    ! significant. The "v" INTVECVEC is created to be the right size.
      type(intvec__type), dimension(:) :: v
      integer(kind=kind(1)) :: dim,i

      dim = size(v)
      do i = 1,dim
         call read_intvec_ptr_(self,v(i)%element)
      end do

   end subroutine

   subroutine read_intvecpxvec_ptr(self,v)
    type(textfile_type) :: self
    ! Read in an integer vector vector pointer "v" sequentially. Line breaks
    ! are not significant. The "v" INTVECVEC* is created to be the right size.
      type(intvec__type), dimension(:), pointer :: v
      character(128) :: word
      integer(kind=kind(1)) :: dim

      dim = list_list_length_(self)
      call create_(v,dim)
      call read_(self,word)
      call ensure_(tonto,word=="{","TEXTFILE:read_intvecpxvec_ptr ... list does not begin with a {")
      call read_intvecvec_(self,v)
      call read_(self,word)
      call ensure_(tonto,word=="}","TEXTFILE:read_intvecpxvec_ptr ... list does not end with a {")

   end subroutine

   subroutine read_realvec_quantity(self,v)
    type(textfile_type) :: self
    ! Read in a vector sequentially. Line breaks are not significant.
    ! WARNING: You must *not* be at the end of file to use this routine, since
    ! it will try to read the next token after the vector as a units specifier.
    ! (There will usually be at least a close brackets symbol "}" after this
    ! data in TONTO input files).
      real(kind=kind(1.0d0)), dimension(:) :: v
      character(128) :: units
      logical(kind=kind(.true.)) :: known_unit

      call read_realvec_(self,v)
      if (.not. at_end_of_file_(self)) then
        call read_str_(self,units)                       ! Read possible unit string
        known_unit = is_known_unit_(units)    ! If unknown unit, move back
        if (.not. known_unit) call move_to_previous_item_(self)
        if (known_unit) then
           call convert_from_(v,units)               ! Convert from known units
        else if (self%default_units/=" ") then
           call convert_from_(v,self%default_units)      ! Convert from assumed default units
           self%default_units = " "
        end if
      else if (self%default_units/=" ") then
        call convert_from_(v,self%default_units)      ! Convert from assumed default units
        self%default_units = " "
      end if

   end subroutine

   subroutine read_realvec(self,v)
    type(textfile_type) :: self
    ! Read in a vector sequentially. Line breaks are not significant.
      real(kind=kind(1.0d0)), dimension(:) :: v
      integer(kind=kind(1)) :: dim,i

      dim = size(v)
      do i = 1,dim
         call read_real_(self,v(i))
      end do

   end subroutine

   subroutine read_realvec_ptr(self,v,ignore_opening_brace)
    type(textfile_type) :: self
    ! Read in a vector pointer "v" sequentially. Line breaks are not significant.
    ! If "ignore_opening_brace" is present, the opening curly brace "{" is not
    ! required; however, the closing curly brace "}" is always required. The "v"
    ! vector pointer is created to be the right size.
      logical(kind=kind(.true.)), optional :: ignore_opening_brace
      real(kind=kind(1.0d0)), dimension(:), pointer :: v
      integer(kind=kind(1)) :: dim
      character(128) :: word

      dim = list_length_(self,ignore_opening_brace)
      call create_(v,dim)
      if (.not. present(ignore_opening_brace)) then
      call read_(self,word)
      call ensure_(tonto,word=="{","TEXTFILE:read_realvec_ptr ... list does not begin with a {")
      end if
      call read_realvec_(self,v)
      call read_(self,word)
      call ensure_(tonto,word=="}","TEXTFILE:read_realvec_ptr ... list does not end with a {")

   end subroutine

   subroutine read_realvec_quantity_ptr(self,v)
    type(textfile_type) :: self
    ! Read in a vector quantity pointer "v" sequentially. The units specifier, if
    ! present, must follow the final curly bracket. Line breaks are not
    ! significant. The "v" vector pointer is created to be the right size.
      real(kind=kind(1.0d0)), dimension(:), pointer :: v
      integer(kind=kind(1)) :: dim
      character(128) :: units
      character(128) :: word
      logical(kind=kind(.true.)) :: known_unit

      dim = list_length_(self)
      call create_(v,dim)
      call read_(self,word)
      call ensure_(tonto,word=="{","TEXTFILE:read_realvec_quantity_ptr ... list does not begin with a {")
      call read_realvec_(self,v)
      call read_(self,word)
      call ensure_(tonto,word=="}","TEXTFILE:read_realvec_quantity_ptr ... list does not end with a {")
       ! Read units part
      if (.not. at_end_of_file_(self)) then
        call read_str_(self,units)                       ! Read possible unit string
        known_unit = is_known_unit_(units)    ! If unknown unit, move back
        if (.not. known_unit) call move_to_previous_item_(self)
        if (known_unit) then
           call convert_from_(v,units)               ! Convert from known units
        else if (self%default_units/=" ") then
           call convert_from_(v,self%default_units)      ! Convert from assumed default units
           self%default_units = " "
        end if
      else if (self%default_units/=" ") then
        call convert_from_(v,self%default_units)      ! Convert from assumed default units
        self%default_units = " "
      end if

   end subroutine

   subroutine read_realvec_pair_quantities(self,v1,v2)
    type(textfile_type) :: self
    ! Read in a pair of vectors sequentially, alternating. Line breaks are
    ! not significant.
      real(kind=kind(1.0d0)), dimension(:) :: v1,v2
      integer(kind=kind(1)) :: dim,i

      call ensure_(tonto,size(v1)==size(v2),"TEXTFILE:read_realvec_pair_quantities ... incompatible vectors")
      dim = size(v1)
      do i = 1,dim
         call read_(self,v1(i))
         call read_(self,v2(i))
      end do

   end subroutine

   subroutine read_cpxvec(self,v)
    type(textfile_type) :: self
    ! Read in a complex vector sequentially. Line breaks are not significant.
      complex(kind=kind((1.0d0,1.0d0))), dimension(:) :: v
      integer(kind=kind(1)) :: dim,i

      dim = size(v)
      do i = 1,dim
         call read_cpx_(self,v(i))
      end do

   end subroutine

   subroutine read_cpxvec_ptr(self,v,ignore_opening_brace)
    type(textfile_type) :: self
    ! Read in a complex vector pointer "v" sequentially. Line breaks are not
    ! significant. If "ignore_opening_brace" is present, the opening curly
    ! brace "{" is not required; however, the closing curly brace "}" is always
    ! required. The "v" vector pointer is created to be the right size.
      complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: v
      logical(kind=kind(.true.)), optional :: ignore_opening_brace
      integer(kind=kind(1)) :: dim
      character(128) :: word

      dim = list_length_(self,ignore_opening_brace)
      call create_(v,dim)
      if (.not. present(ignore_opening_brace)) then
      call read_(self,word)
      call ensure_(tonto,word=="{","TEXTFILE:read_cpxvec_ptr ... list does not begin with a {")
      end if
      call read_cpxvec_(self,v)
      call read_(self,word)
      call ensure_(tonto,word=="}","TEXTFILE:read_cpxvec_ptr ... list does not end with a {")

   end subroutine

   subroutine read_realmat_quantity(self,m,order)
    type(textfile_type) :: self
    ! Read in a matrix sequentially by row (default) or by column. If a units
    ! string appears after the matrix, the elements are converted into atomic
    ! units or generic units. Line breaks are not significant.
    ! WARNING: You must *not* be at the end of file to use this routine, since
    ! it will try to read the next token after the matrix as a units specifier.
    ! (There will usually be at least a close brackets symbol "}" after this
    ! data in TONTO input files).
      real(kind=kind(1.0d0)), dimension(:,:) :: m
      character(*), optional :: order
      character(128) :: units
      logical(kind=kind(.true.)) :: known_unit

      call read_realmat_(self,m,order)
      if (.not. at_end_of_file_(self)) then
        call read_str_(self,units)                       ! Read possible unit string
        known_unit = is_known_unit_(units)    ! If unknown unit, move back
        if (.not. known_unit) call move_to_previous_item_(self)
        if (known_unit) then
           call convert_from_(m,units)               ! Convert from known units
        else if (self%default_units/=" ") then
           call convert_from_(m,self%default_units)      ! Convert from assumed default units
           self%default_units = " "
        end if
      else if (self%default_units/=" ") then
        call convert_from_(m,self%default_units)      ! Convert from assumed default units
        self%default_units = " "
      end if

   end subroutine

   subroutine read_intmat(self,m,order)
    type(textfile_type) :: self
    ! Read in an integer complex matrix sequentially by row (default) or by
    ! column.  Line breaks are not significant.
      integer(kind=kind(1)), dimension(:,:) :: m
      character(*), optional :: order
      integer(kind=kind(1)) :: dim1,dim2,i,j
      character(128) :: print_order

      dim1 = size(m,1)
      dim2 = size(m,2)
      print_order = "by_row"
      if (present(order)) print_order = order
      select case (print_order)
         case ("by_column   ","column_major")
            do j = 1,dim2
            do i = 1,dim1
              call read_int_(self,m(i,j))
            end do
            end do
         case ("by_row      ","row_major   ")
            do i = 1,dim1
            do j = 1,dim2
              call read_int_(self,m(i,j))
            end do
            end do
         case default
            allocate(tonto%known_keywords(4))
            tonto%known_keywords(1) = "by_column   "
            tonto%known_keywords(2) = "column_major"
            tonto%known_keywords(3) = "by_row      "
            tonto%known_keywords(4) = "row_major   "
            call unknown_(tonto,print_order,"TEXTFILE:read_intmat")
            deallocate(tonto%known_keywords)
      end select

   end subroutine

   subroutine read_intmat_ptr(self,v,n_rows,n_cols,order)
    type(textfile_type) :: self
    ! Read in an integer matrix pointer "v" sequentially. Line breaks are not
    ! significant.  The "m" matrix pointer is created to be the right size.
    ! You must specify n_rows or n_cols in the calling routine, but not both.
      integer(kind=kind(1)), dimension(:,:), pointer :: v
      integer(kind=kind(1)), intent(in), optional :: n_rows,n_cols
      character(*), optional :: order
      integer(kind=kind(1)) :: dim,len
      character(128) :: word

      len = list_length_(self)
      call read_(self,word)
      call ensure_(tonto,word=="{","TEXTFILE:read_intmat_ptr ... list does not begin with a {")
      if (present(n_rows)) then
        call ensure_(tonto,.not. present(n_cols),"TEXTFILE:read_intmat_ptr ... subroutine incorrectly called.")
        call ensure_(tonto,mod(dim,n_rows)==0,"TEXTFILE:read_intmat_ptr ... number of matrix elements entered is incorrect")
        dim = len/n_rows
        call create_(v,n_rows,dim)
        call read_intmat_(self,v,order)
      else if (present(n_cols)) then
        call ensure_(tonto,mod(dim,n_cols)==0,"TEXTFILE:read_intmat_ptr ... number of matrix elements entered is incorrect")
        dim = len/n_cols
        call create_(v,dim,n_cols)
        call read_intmat_(self,v,order)
      else
        call die_(tonto,"TEXTFILE:read_intmat_ptr ... subroutine incorrectly called.")
        dim = len  ! if DIE macro not set, fall back to n_cols=1
        call create_(v,dim,1)
        call read_intmat_(self,v,order)
      end if
      call read_(self,word)
      call ensure_(tonto,word=="}","TEXTFILE:read_intmat_ptr ... list does not end with a {")

   end subroutine

   subroutine read_realmat(self,m,order)
    type(textfile_type) :: self
    ! Read in a complex matrix sequentially by row (default) or by column.
    ! Line breaks are not significant.
      real(kind=kind(1.0d0)), dimension(:,:) :: m
      character(*), optional :: order
      integer(kind=kind(1)) :: dim1,dim2,i,j
      character(128) :: print_order

      dim1 = size(m,1)
      dim2 = size(m,2)
      print_order = "by_row"
      if (present(order)) print_order = order
      select case (print_order)
         case ("by_column   ","column_major")
            do j = 1,dim2
            do i = 1,dim1
              call read_real_(self,m(i,j))
            end do
            end do
         case ("by_row      ","row_major   ")
            do i = 1,dim1
            do j = 1,dim2
              call read_real_(self,m(i,j))
            end do
            end do
         case default
            allocate(tonto%known_keywords(4))
            tonto%known_keywords(1) = "by_column   "
            tonto%known_keywords(2) = "column_major"
            tonto%known_keywords(3) = "by_row      "
            tonto%known_keywords(4) = "row_major   "
            call unknown_(tonto,print_order,"TEXTFILE:read_realmat")
            deallocate(tonto%known_keywords)
      end select

   end subroutine

   subroutine read_realmat_ptr(self,mat)
    type(textfile_type) :: self
    ! Read in a matrix pointer "mat" sequentially. Line breaks are significant.
       real(kind=kind(1.0d0)), dimension(:,:), pointer :: mat
       character(128) :: word
       integer(kind=kind(1)), dimension(2) :: dim

       call number_lines_col_(self,dim)
       call read_(self,word)
       call ensure_(tonto,word=="{","TEXTFILE:read_realmat_ptr ... matrix does not begin with a {")
       call create_(mat,dim(1),dim(2))
       call read_realmat_(self,mat)
       call read_(self,word)
       call ensure_(tonto,word=="}","TEXTFILE:read_realmat_ptr ... expecting close bracket symbol: }")

   end subroutine

   subroutine number_lines_col(self,dim)
    type(textfile_type) :: self
    ! Returns the number of lines which are not blank, and the number of items
    ! per line between an opening and closing brace. It is an error if there are
    ! a different number of items per line on every non-blank line.
      integer(kind=kind(1)), dimension(2) :: dim
      integer(kind=kind(1)) :: line,item,first_line
      character(128) :: word
      integer(kind=kind(1)) :: dim1,dim2

      item = next_line_item_(self)
      line = line_number_(self)
      call read_(self,word)
      call ensure_(tonto,word=="{","TEXTFILE:number_lines_col ... list does not begin with a {")
      dim1 = 0
      dim2 = 0
      call read_(self,word)                            ! first item of 1st line
      if (word == "}") then;   return; end if
      first_line = line_number_(self)
      dim1 = 1                               ! There is at least one row
      dim2 = n_line_items_(self)                   ! The number of columns
      dim(2) = dim2                          ! This (no. of items per line) shouln't change
      if (first_line==line) dim2 = dim2 - 1  ! Opening { is on ths same line as first item
      do
         call skip_line_(self)
         if (n_line_items_(self)==0) cycle
         call read_(self,word)                         ! first item of next non-blank line
         if (word == "}") exit               ! End of matrix
         dim1 = dim1 + 1                     ! Add another row
         dim2 = n_line_items_(self)
         if (dim2>dim(2)) then
            dim2 = dim(2)
            call move_to_line_item_(self,dim2+1)       ! This item must be a closing }
            call ensure_(tonto,next_item_(self)=="}","TEXTFILE:number_lines_col ... different number of line items on succesi&
&ve lines")
            exit
         else
            call ensure_(tonto,dim2==dim(2),"TEXTFILE:number_lines_col ... different number of line items on succesive lines"&
&)
         end if
      end do
      call move_to_line_(self,line)
      call move_to_line_item_(self,item)
      dim = (/dim1,dim2/)

   end subroutine

   subroutine read_realmat3(self,mx)
    type(textfile_type) :: self
    ! Read a REALMAT3 from the input buffer flat style
     real(kind=kind(1.0d0)), dimension(:,:,:) :: mx
     integer(kind=kind(1)) :: a,b,c,aub,bub,cub,tmp

     aub=size(mx,1)
     bub=size(mx,2)
     cub=size(mx,3)
     call warn_if_(tonto,aub==0,"TEXTFILE:read_realmat3 ... zero sized 1st dimension!")
     call warn_if_(tonto,bub==0,"TEXTFILE:read_realmat3 ... zero sized 2nd dimension!")
     call warn_if_(tonto,cub==0,"TEXTFILE:read_realmat3 ... zero sized 3rd dimension!")
     do a=1,aub
       do b=1,bub
         do c=1,cub
           call read_int_(self,tmp)
           call read_int_(self,tmp)
           call read_int_(self,tmp)
           call read_real_(self,mx(a,b,c))
         end do
       end do
     end do

   end subroutine

   subroutine read_realmat4(self,mx)
    type(textfile_type) :: self
    ! Read a REALMAT4 from the input buffer flat style
     real(kind=kind(1.0d0)), dimension(:,:,:,:) :: mx
     integer(kind=kind(1)) :: a,b,c,d,aub,bub,cub,dub,tmp

     aub=size(mx,1)
     bub=size(mx,2)
     cub=size(mx,3)
     dub=size(mx,3)
     call warn_if_(tonto,aub==0,"TEXTFILE:read_realmat4 ... zero sized 1st dimension!")
     call warn_if_(tonto,bub==0,"TEXTFILE:read_realmat4 ... zero sized 2nd dimension!")
     call warn_if_(tonto,cub==0,"TEXTFILE:read_realmat4 ... zero sized 3rd dimension!")
     call warn_if_(tonto,dub==0,"TEXTFILE:read_realmat4 ... zero sized 4th dimension!")
     do a=1,aub
       do b=1,bub
         do c=1,cub
           do d=1,dub
             call read_int_(self,tmp)
             call read_int_(self,tmp)
             call read_int_(self,tmp)
             call read_int_(self,tmp)
             call read_real_(self,mx(a,b,c,d))
           end do
         end do
       end do
     end do

   end subroutine

   subroutine read_formatted_mat(self,m,format,items_per_line)
    type(textfile_type) :: self
    ! Read a formatted matrix "m" with fortran "format" and a given
    ! number of "items_per_line".
      real(kind=kind(1.0d0)), dimension(:,:), intent(out) :: m
     character(*), intent(in) :: format
     integer(kind=kind(1)), intent(in) :: items_per_line
     integer(kind=kind(1)) :: i,j,d,count

     call move_to_item_(self%buffer,1)
     d = size(m,1)
     count=0
     do i=1,d
       do j=1,d
         call read_formatted_real_(self,m(i,j),format)
         count = count + 1
          if (count==items_per_line) then
            do
               call read_line_(self)
               if (.not. empty_(self%buffer)) exit
            end do
            count=0
          end if
       end do
     end do

   end subroutine

   subroutine read_cadpac_mat(self,m)
    type(textfile_type) :: self
    ! Read in a matrix produced by the constraint part of cadpac.
     intent(in) :: self
     real(kind=kind(1.0d0)), dimension(:,:), intent(out) :: m

     call read_formatted_mat_(self,m,"5D16.8",5)

   end subroutine

   subroutine read_cpxmat(self,m,order)
    type(textfile_type) :: self
    ! Read in a complex matrix sequentially by row (default) or by column.
    ! Line breaks are not significant.
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: m
      character(*), optional :: order
      integer(kind=kind(1)) :: dim1,dim2,i,j
      character(128) :: print_order

      dim1 = size(m,1)
      dim2 = size(m,2)
      print_order = "by_row"
      if (present(order)) print_order = order
      select case (print_order)
         case ("by_column   ","column_major")
            do j = 1,dim2
            do i = 1,dim1
              call read_cpx_(self,m(i,j))
            end do
            end do
         case ("by_row      ","row_major   ")
            do i = 1,dim1
            do j = 1,dim2
              call read_cpx_(self,m(i,j))
            end do
            end do
         case default
            allocate(tonto%known_keywords(4))
            tonto%known_keywords(1) = "by_column   "
            tonto%known_keywords(2) = "column_major"
            tonto%known_keywords(3) = "by_row      "
            tonto%known_keywords(4) = "row_major   "
            call unknown_(tonto,print_order,"TEXTFILE:read_cpxmat")
            deallocate(tonto%known_keywords)
      end select

   end subroutine

   subroutine read_cpxmat3(self,mx)
    type(textfile_type) :: self
    ! Read a CPXMAT3 from the input buffer flat style
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:) :: mx
     integer(kind=kind(1)) :: a,b,c,aub,bub,cub,tmp

     aub=size(mx,1)
     bub=size(mx,2)
     cub=size(mx,3)
     call warn_if_(tonto,aub==0,"TEXTFILE:read_cpxmat3 ... zero sized 1st dimension!")
     call warn_if_(tonto,bub==0,"TEXTFILE:read_cpxmat3 ... zero sized 2nd dimension!")
     call warn_if_(tonto,cub==0,"TEXTFILE:read_cpxmat3 ... zero sized 3rd dimension!")
     do a=1,aub
       do b=1,bub
         do c=1,cub
           call read_int_(self,tmp)
           call read_int_(self,tmp)
           call read_int_(self,tmp)
           call read_cpx_(self,mx(a,b,c))
         end do
       end do
     end do

   end subroutine

   subroutine read_cpxmat4(self,mx)
    type(textfile_type) :: self
    ! Read a CPXMAT4 from the input buffer flat style
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:,:) :: mx
     integer(kind=kind(1)) :: a,b,c,d,aub,bub,cub,dub,tmp

     aub=size(mx,1)
     bub=size(mx,2)
     cub=size(mx,3)
     dub=size(mx,4)
     call warn_if_(tonto,aub==0,"TEXTFILE:read_cpxmat4 ... zero sized 1st dimension!")
     call warn_if_(tonto,bub==0,"TEXTFILE:read_cpxmat4 ... zero sized 2nd dimension!")
     call warn_if_(tonto,cub==0,"TEXTFILE:read_cpxmat4 ... zero sized 3rd dimension!")
     call warn_if_(tonto,dub==0,"TEXTFILE:read_cpxmat4 ... zero sized 4th dimension!")
     do a=1,aub
       do b=1,bub
         do c=1,cub
           do d=1,dub
             call read_int_(self,tmp)
             call read_int_(self,tmp)
             call read_int_(self,tmp)
             call read_int_(self,tmp)
             call read_cpx_(self,mx(a,b,c,d))
           end do
         end do
       end do
     end do

   end subroutine

   subroutine read_cpxmat5(self,mx)
    type(textfile_type) :: self
    ! Read a CPXMAT5 from the input buffer flat style
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:,:,:) :: mx
     integer(kind=kind(1)) :: a,b,c,d,e,aub,bub,cub,dub,eub,tmp

     aub=size(mx,1)
     bub=size(mx,2)
     cub=size(mx,3)
     dub=size(mx,4)
     eub=size(mx,5)
     call warn_if_(tonto,aub==0,"TEXTFILE:read_cpxmat5 ... zero sized 1st dimension!")
     call warn_if_(tonto,bub==0,"TEXTFILE:read_cpxmat5 ... zero sized 2nd dimension!")
     call warn_if_(tonto,cub==0,"TEXTFILE:read_cpxmat5 ... zero sized 3rd dimension!")
     call warn_if_(tonto,dub==0,"TEXTFILE:read_cpxmat5 ... zero sized 4th dimension!")
     call warn_if_(tonto,eub==0,"TEXTFILE:read_cpxmat5 ... zero sized 5th dimension!")
     do a=1,aub
       do b=1,bub
         do c=1,cub
           do d=1,dub
             do e=1,eub
               call read_int_(self,tmp)
               call read_int_(self,tmp)
               call read_int_(self,tmp)
               call read_int_(self,tmp)
               call read_int_(self,tmp)
               call read_cpx_(self,mx(a,b,c,d,e))
             end do
           end do
         end do
       end do
     end do

   end subroutine

   function next_item(self) result(word)
    type(textfile_type) :: self
    ! Read a str from the input file and return it but
    ! *do not advance the cursor*. Use next_str for that.
     character(128) :: word

     call read_str_(self,word)
     call move_to_previous_item_(self)

   end function

   function next_str(self) result(word)
    type(textfile_type) :: self
    ! Read a str from the input file and return it and advance the cursor
     character(128) :: word

     call read_str_(self,word)

   end function

   function next_formatted_REAL(self,format) result(value)
    type(textfile_type) :: self
    ! Read a formatted real(kind=kind(1.0d0)) from the input file and return it.
    ! Does not check the end of line!
     real(kind=kind(1.0d0)) :: value
     character(*), intent(in) :: format

     call read_formatted_real_(self,value,format)

   end function

!  ********************
!  Data output routines
!  ********************

   subroutine put_margin(self)
    type(textfile_type) :: self
    ! Put a margin into the buffer of the output object

      call put_(self%buffer,repeat(" ",self%margin_width))

   end subroutine

   subroutine flush(self)
    type(textfile_type) :: self
    ! Flush the buffer to the output file
      type(unitnumber_type) :: unit

      call ensure_(tonto,is_open_(self),"TEXTFILE:flush ... file is not open!")
      call ensure_(tonto,self%action=="write","TEXTFILE:flush ... file does not have write action!")
      if (do_io_(tonto_parallel)) then
        write(unit=self%unit,iostat=self%io_status,fmt='(a)') trim(self%buffer%string)
      end if
      call broadcast_(tonto_parallel,self%io_status,0)
      unit%unit = self%unit
      call flush_buffer_(unit)
      call clear_(self%buffer)
      call put_margin_(self)
      self%record = self%record + 1

   end subroutine

   subroutine flush_1(self,times)
    type(textfile_type) :: self
    ! Flush the buffer multiple times to get extra carriage returns
     integer(kind=kind(1)) :: times
     integer(kind=kind(1)) :: i

     do i=1,times
        call flush_(self)
     end do

   end subroutine

   subroutine tab(self,real_fields,int_fields,bin_fields,width)
    type(textfile_type) :: self
    ! Tab across the specified number of fields in the output buffer
    ! "real_fields", "int_fields" and "bin_fields" refer to the number of
    ! real, integer and logical fields to tab; "width" is the width of spaces
    ! to tab.
      integer(kind=kind(1)), optional :: real_fields,int_fields,bin_fields,width

      if (present(real_fields)) &
         call put_(self%buffer,repeat(repeat(" ",self%real_width),real_fields))
      if (present(int_fields)) &
         call put_(self%buffer,repeat(repeat(" ",self%int_width),int_fields))
      if (present(bin_fields)) &
         call put_(self%buffer,repeat(repeat(" ",self%int_width),bin_fields))
      if (present(width)) &
         call put_(self%buffer,repeat(" ",width))

   end subroutine

   subroutine dash(self,real_fields,int_fields,bin_fields,width)
    type(textfile_type) :: self
    ! Put a dashed line into the buffer. "real_fields", "int_fields" and
    ! "bin_fields" refer to the number of real, integer and logical fields
    ! to draw the line over; "width" is the width of characters to draw the
    ! dashed line.
      integer(kind=kind(1)), optional :: real_fields,int_fields,bin_fields,width

      call put_dash_(self,real_fields,int_fields,bin_fields,width,flush=1)

   end subroutine

   subroutine put_dash(self,real_fields,int_fields,bin_fields,width,flush)
    type(textfile_type) :: self
    ! Put a dashed line into the buffer. "real_fields", "int_fields" and
    ! "bin_fields" refer to the number of real, integer and logical fields
    ! to draw the line over; "width" is the width of characters to draw the
    ! dashed line.
      integer(kind=kind(1)), optional :: real_fields,int_fields,bin_fields,width,flush

      if (present(real_fields)) &
         call put_(self%buffer,repeat(repeat("-",self%real_width),real_fields))
      if (present(int_fields)) &
         call put_(self%buffer,repeat(repeat("-",self%int_width),int_fields))
      if (present(bin_fields)) &
         call put_(self%buffer,repeat(repeat("-",self%int_width),bin_fields))
      if (present(width)) &
         call put_(self%buffer,repeat("-",width))
      if (present(flush)) call flush_(self,flush)

   end subroutine

   subroutine show_str(self,pretext,string,int_width)
    type(textfile_type) :: self
    ! Put a formatted STR into the output buffer with descriptive "pretext".
      character(*) :: pretext
      character(*) :: string
      logical(kind=kind(.true.)), optional :: int_width

      call put_text_(self,pretext)
      if (includes_(string," ")) then
        call put_str_(self,'"'//trim(string)//'"',int_width,flush=1)
      else
        call put_str_(self,string,int_width,flush=1)
      end if

   end subroutine

   subroutine show_int(self,pretext,value,real_width)
    type(textfile_type) :: self
    ! Put a formatted integer(kind=kind(1)) into the output buffer with descriptive "pretext".  If
    ! present, use "real_width" for the width of the field, instead of int_wdith.
      character(*) :: pretext
      logical(kind=kind(.true.)), optional :: real_width
      integer(kind=kind(1)) :: value

      call put_text_(self,pretext)
      call put_int_(self,value,real_width,flush=1)

   end subroutine

   subroutine show_int_3(self,pretext,val1,val2,val3,real_width)
    type(textfile_type) :: self
    ! Put three integer(kind=kind(1))'s "val1", "val2" and "val3" into the output buffer, with
    ! descriptive "pretext" before. If present, use "real_width" for all of them.
      character(*) :: pretext
      integer(kind=kind(1)) :: val1,val2,val3
      logical(kind=kind(.true.)), optional :: real_width

      call put_text_(self,pretext)
      call put_int_(self,val1,real_width)
      call put_int_(self,val2,real_width)
      call put_int_(self,val3,real_width,flush=1)

   end subroutine

   subroutine show_bit_string(self,pretext,value,int_width,width)
    type(textfile_type) :: self
    ! Put a formatted integer(kind=kind(1)) as a binary number into the output buffer,
    ! with descriptive "pretext".
      character(*) :: pretext
      logical(kind=kind(.true.)), optional :: int_width
      integer(kind=kind(1)), optional :: width
      integer(kind=kind(1)) :: value

      call put_text_(self,pretext)
      call put_bit_string_(self,value,int_width,width,flush=1)

   end subroutine

   subroutine show_bin(self,pretext,value,real_width)
    type(textfile_type) :: self
    ! Put a formatted logical(kind=kind(.true.)) into the output buffer with descriptive "pretext".
      character(*) :: pretext
      logical(kind=kind(.true.)), optional :: real_width
      logical(kind=kind(.true.)) :: value

      call put_text_(self,pretext)
      call put_bin_(self,value,real_width,flush=1)

   end subroutine

   subroutine show_real(self,pretext,value)
    type(textfile_type) :: self
    ! Put a formatted real(kind=kind(1.0d0)) into the output buffer with descriptive "pretext".
      character(*) :: pretext
      real(kind=kind(1.0d0)) :: value

      call put_text_(self,pretext)
      call put_real_(self,value,flush=1)

   end subroutine

   subroutine show_real_3(self,pretext,val1,val2,val3)
    type(textfile_type) :: self
    ! Put three real(kind=kind(1.0d0))'s "val1", "val2" and "val3" into the output buffer, with
    ! descriptive "pretext" before.
      character(*) :: pretext
      real(kind=kind(1.0d0)) :: val1,val2,val3

      call put_text_(self,pretext)
      call put_real_(self,val1)
      call put_real_(self,val2)
      call put_real_(self,val3,flush=1)

   end subroutine

   subroutine show_binvec(self,pretext,value)
    type(textfile_type) :: self
    ! Put a formatted BINVEC "value" into the output buffer with descriptive
    ! "pretext".
      character(*) :: pretext
      logical(kind=kind(.true.)), dimension(:) :: value
      logical(kind=kind(.true.)) :: use_labels

      call put_text_(self,pretext)
      use_labels = self%use_labels
      call set_use_labels_(self,.false.)
      call put_binvec_(self,value)
      call set_use_labels_(self,use_labels)
    !  .flush

   end subroutine

   subroutine show_intvec(self,pretext,value)
    type(textfile_type) :: self
    ! Put a formatted INTVEC "value" into the output buffer with descriptive
    ! "pretext".
      character(*) :: pretext
      integer(kind=kind(1)), dimension(:) :: value
      logical(kind=kind(.true.)) :: use_labels

      call put_text_(self,pretext)
      use_labels = self%use_labels
      call set_use_labels_(self,.false.)
      call put_intvec_(self,value)
      call set_use_labels_(self,use_labels)
    !  .flush

   end subroutine

   subroutine show_realvec(self,pretext,value)
    type(textfile_type) :: self
    ! Put a formatted real(kind=kind(1.0d0)) "value" into the output buffer with descriptive
    ! "pretext".
      character(*) :: pretext
      real(kind=kind(1.0d0)), dimension(:) :: value
      logical(kind=kind(.true.)) :: use_labels

      call put_text_(self,pretext)
      use_labels = self%use_labels
      call set_use_labels_(self,.false.)
      call put_realvec_(self,value)
      call set_use_labels_(self,use_labels)
    !  .flush

   end subroutine

   subroutine show_strvec(self,pretext,value)
    type(textfile_type) :: self
    ! Put a formatted real(kind=kind(1.0d0)) "value" into the output buffer with descriptive
    ! "pretext".
      character(*) :: pretext
      character(128), dimension(:) :: value
      logical(kind=kind(.true.)) :: use_labels

      call put_text_(self,pretext)
      use_labels = self%use_labels
      call set_use_labels_(self,.false.)
      call put_strvec_(self,value)
      call set_use_labels_(self,use_labels)

   end subroutine

   subroutine text(self,string,real_width,int_width,flush)
    type(textfile_type) :: self
    ! Put text into the output buffer as is and flush,
    ! unless an explicit flush is present
      character(*) :: string
      logical(kind=kind(.true.)), optional :: real_width,int_width
      integer(kind=kind(1)), optional :: flush

      if (present(flush)) then
         call put_text_(self,string,real_width,int_width,flush)
      else
         call put_text_(self,string,real_width,int_width,flush=1)
      end if

   end subroutine

   subroutine put_text(self,string,real_width,int_width,flush)
    type(textfile_type) :: self
    ! Put text into the output buffer as is.
      character(*) :: string
      logical(kind=kind(.true.)), optional :: real_width,int_width
      integer(kind=kind(1)), optional :: flush
      character(self%int_width) :: int_string
      character(self%real_width) :: real_string

      if (present(real_width) .and. (len(string) <= self%real_width )) then
        real_string = string
        call put_(self%buffer, real_string )
      else if (present(int_width) .and. (len(string) <= self%int_width )) then
        int_string = string
        call put_(self%buffer, int_string )
      else
        call put_(self%buffer, string )
      end if
      if (present(flush)) call flush_(self,flush)

   end subroutine

   subroutine put_str(self,string,int_width,width,flush)
    type(textfile_type) :: self
    ! Put a formatted STR into the output buffer; if too big, put as is.
    ! if present and .true., "int_width" says to use field width of int_width.
    ! if present, "width" is how wide the field should be, instead of real_width.
    ! if present, flush is how many times to flush the buffer.
      character(*) :: string
      integer(kind=kind(1)), optional :: flush
      logical(kind=kind(.true.)), optional :: int_width
      integer(kind=kind(1)), optional :: width
      character(128) :: form
      integer(kind=kind(1)) :: wid

      call ensure_(tonto,.not. (present(int_width) .and. present(width)),"TEXTFILE:put_str ... too many widths")
      wid = self%real_width
      if (present(int_width)) wid = self%int_width
      if (present(width))     wid = width
      if (len_trim(string)>wid) then
         call put_(self%buffer,trim(string))
      else if (len_trim(string)/=0) then
         form = "a" // trim( to_str_(wid))
         call put_(self%buffer,trim(string),form)
      end if
      if (present(flush)) call flush_(self,flush)

   end subroutine

   subroutine put_unit(self,value,units)
    type(textfile_type) :: self
    ! Put a formatted value and its units into the output buffer.
     integer(kind=kind(1)), intent(in) :: value
     character(128), intent(in) :: units
     character(128) :: format

     format = format_for_int_(self, nice_field_width_for_(self,value) )
     call put_(self%buffer,value,format)
     call put_text_(self, " " //  trim(units))
     if (value /= 1) call put_text_(self,"s")

   end subroutine

   subroutine put_int(self,value,real_width,width,flush)
    type(textfile_type) :: self
    ! Put a formatted integer into the output buffer; the field width used is
    ! int_width, usually the length of the nondecimal part of a formatted real(kind=kind(1.0d0))
    ! number. Can override the width and use real_width if "real_width" is set to
    ! .true..  Can override real_width by explicitly setting "width".
      integer(kind=kind(1)) :: value
      logical(kind=kind(.true.)), optional :: real_width
      integer(kind=kind(1)), optional :: width
      integer(kind=kind(1)), optional :: flush
      character(128) :: format
      integer(kind=kind(1)) :: wide

      call ensure_(tonto,.not. (present(real_width) .and. present(width)),"TEXTFILE:put_int ... too many widths")
      wide = self%int_width
      if (present(real_width)) wide = self%real_width
      if (present(width))     wide = width
      format = format_for_int_(self,wide)
      call put_(self%buffer,value,format)
      if (present(flush)) call flush_(self,flush)

   end subroutine

!   put_int_with_zeros(int,width,flush)
!   !
!     int, width :: integer(kind=kind(1)), intent(in)
!     flush :: integer(kind=kind(1)), optional
!     int_width :: integer(kind=kind(1))
!     format :: STR
!     int_width = .nice_field_width_for(int)
!     if (int<0) then
!       .buffer.put("-")
!       format = .format_for_int(int_width-1)
!     else
!       format = .format_for_int(int_width)
!     end
!     .buffer.put(repeat("0",width-int_width))
!     .buffer.put(abs(int),format)
!     if (present(flush)) .flush(flush)
!   end

   subroutine put_bit_string(self,value,int_width,width,flush)
    type(textfile_type) :: self
    ! Put a formatted integer as a bit string into the output buffer; the
    ! field width used is real_width. Can override real_width by explicitly
    ! setting "width".
      integer(kind=kind(1)) :: value
      logical(kind=kind(.true.)), optional :: int_width
      integer(kind=kind(1)), optional :: width
      integer(kind=kind(1)), optional :: flush
      character(128) :: format
      integer(kind=kind(1)) :: wide

      call ensure_(tonto,.not. (present(int_width) .and. present(width)),"TEXTFILE:put_bit_string ... too many widths")
      wide = self%real_width
      if (present(int_width)) wide = self%int_width
      if (present(width))     wide = width
      format = format_for_bit_string_(self,wide)
      call put_(self%buffer,value,format)
      if (present(flush)) call flush_(self,flush)

   end subroutine

   subroutine put_bin(self,value,real_width,flush)
    type(textfile_type) :: self
    ! Put a formatted logical into the output buffer; can specify the width of
    ! the field if desired. Otherwise, the field with used is int_width, or
    ! the length of the nondecimal part of a formatted real(kind=kind(1.0d0)) number
      logical(kind=kind(.true.)) :: value
      integer(kind=kind(1)), optional :: flush
      logical(kind=kind(.true.)), optional :: real_width
      character(128) :: format

      format = format_for_bin_(self, self%int_width)
      if (present(real_width)) then
        if (real_width) format = format_for_bin_(self, self%real_width)
      end if
      call put_(self%buffer,value,format)
      if (present(flush)) call flush_(self,flush)

   end subroutine

   subroutine put_real(self,value,int_width,width,precision,flush)
    type(textfile_type) :: self
    ! Put a formatted real(kind=kind(1.0d0)) into the output buffer. The field with used is
    ! .real_width unless "width" is given which explicitly overrides the default.
    ! Likewise, "precision" can be used to overide the default number of decimal
    ! places given in .real_precision.
      real(kind=kind(1.0d0)) :: value
      logical(kind=kind(.true.)), optional :: int_width
      integer(kind=kind(1)), optional :: width, precision, flush
      character(128) :: format
      integer(kind=kind(1)) :: wide,precise

      call ensure_(tonto,.not. (present(int_width) .and. present(width)),"TEXTFILE:put_real ... too many widths")
      call ensure_(tonto,.not. (present(int_width) .and. present(precision)),"TEXTFILE:put_real ... int_width overrides preci&
&sion")
      precise = self%real_precision
      if (present(precision)) precise = precision
      wide = self%real_width
      if (present(int_width)) then
         wide = self%int_width
         precise = 3
      end if
      if (present(width))     wide = width
      format = format_for_real_(self,self%real_style,wide,precise)
      call put_(self%buffer,value,format)
      if (present(flush)) call flush_(self,flush)

   end subroutine

   subroutine put_cpx(self,value,flush)
    type(textfile_type) :: self
    ! Put a formatted complex(kind=kind((1.0d0,1.0d0))) into the output buffer
    ! The field with used is int_width, or
    ! the length of the nondecimal part of a formatted real(kind=kind(1.0d0)) number
      complex(kind=kind((1.0d0,1.0d0))) :: value
      integer(kind=kind(1)), optional :: flush
      character(128) :: format

      format = format_for_real_(self, self%real_style, self%real_width, self%real_precision)
      call put_(self%buffer,value,format)
      if (present(flush)) call flush_(self,flush)

   end subroutine

   subroutine put_binvec(self,vec,format)
    type(textfile_type) :: self
    ! Put a formatted logical vector into the output buffer
      logical(kind=kind(.true.)), dimension(:) :: vec
      character(*), optional :: format
      character(128) :: fmt

      fmt = "row_wise"
      if (present(format)) fmt = format
      select case (fmt)
         case("by_row     ","row_wise   ","row        ")
            call put_binvec_by_row_(self,vec)
         case("by_column  ","column_wise","column     ")
            call put_binvec_by_column_(self,vec)
         case default
            allocate(tonto%known_keywords(6))
            tonto%known_keywords(1) = "by_row     "
            tonto%known_keywords(2) = "row_wise   "
            tonto%known_keywords(3) = "row        "
            tonto%known_keywords(4) = "by_column  "
            tonto%known_keywords(5) = "column_wise"
            tonto%known_keywords(6) = "column     "
            call unknown_(tonto,fmt,"TEXTFILE:put_binvec")
            deallocate(tonto%known_keywords)
      end select

   end subroutine

   subroutine put_binvec_by_column(self,vec)
    type(textfile_type) :: self
    ! Put a formatted logical vector into the output buffer
      logical(kind=kind(.true.)), dimension(:) :: vec
      integer(kind=kind(1)) :: dim,i

      dim = size(vec)
      do i = 1,dim
         if (self%use_labels) then
            call put_int_(self,i)
         end if
         call put_bin_(self,vec(i),flush=1)
      end do

   end subroutine

   subroutine put_binvec_by_row(self,vec)
    type(textfile_type) :: self
    ! Put a formatted logical vector into the output buffer
      logical(kind=kind(.true.)), dimension(:) :: vec
      integer(kind=kind(1)) :: dim,block,nb,f,l,i,fields

      dim = size(vec)
      fields = self%n_fields
      nb = (dim-0.1)/fields+1
      do block = 1,nb
         f = 1+fields*(block-1)
         l = min(f+fields-1,dim)
         if (block>1) call flush_(self)
         if (self%use_labels) then
!           .tab(int_fields=1)
            do i = f,l
               call put_int_(self,i)
            end do
            call flush_(self,2)
!           .tab(int_fields=1)
         end if
         do i = f,l
            call put_bin_(self,vec(i))
         end do
         call flush_(self)
      end do

   end subroutine

   subroutine put_intvec(self,vec,format)
    type(textfile_type) :: self
    ! Put a formatted integer vector into the output buffer
      integer(kind=kind(1)), dimension(:) :: vec
      character(*), optional :: format
      character(128) :: fmt

      fmt = "row_wise"
      if (present(format)) fmt = format
      select case (fmt)
         case("by_row     ","row_wise   ","row        ")
            call put_intvec_by_row_(self,vec)
         case("by_column  ","column_wise","column     ")
            call put_intvec_by_column_(self,vec)
         case default
            allocate(tonto%known_keywords(6))
            tonto%known_keywords(1) = "by_row     "
            tonto%known_keywords(2) = "row_wise   "
            tonto%known_keywords(3) = "row        "
            tonto%known_keywords(4) = "by_column  "
            tonto%known_keywords(5) = "column_wise"
            tonto%known_keywords(6) = "column     "
            call unknown_(tonto,fmt,"TEXTFILE:put_intvec")
            deallocate(tonto%known_keywords)
      end select

   end subroutine

   subroutine put_intvec_by_column(self,vec)
    type(textfile_type) :: self
    ! Put a formatted integer vector into the output buffer
      integer(kind=kind(1)), dimension(:) :: vec
      integer(kind=kind(1)) :: dim,i

      dim = size(vec)
      do i = 1,dim
         if (self%use_labels) then
            call put_int_(self,i)
         end if
         call put_int_(self,vec(i),flush=1)
      end do

   end subroutine

   subroutine put_intvec_by_row(self,vec)
    type(textfile_type) :: self
    ! Put a formatted integer vector into the output buffer
      integer(kind=kind(1)), dimension(:) :: vec
      integer(kind=kind(1)) :: dim,block,nb,f,l,i,fields

      dim = size(vec)
      fields = self%n_fields
      nb = (dim-0.1)/fields+1
      do block = 1,nb
         f = 1+fields*(block-1)
         l = min(f+fields-1,dim)
         if (block>1) call flush_(self)
         if (self%use_labels) then
!           .tab(int_fields=1)
            do i = f,l
               call put_int_(self,i)
            end do
            call flush_(self,2)
!           .tab(int_fields=1)
         end if
         do i = f,l
            call put_int_(self,vec(i))
         end do
         call flush_(self)
      end do

   end subroutine

   subroutine put_strvec(self,vec,format)
    type(textfile_type) :: self
    ! Put a formatted real(kind=kind(1.0d0)) vector into the output buffer
      character(128), dimension(:) :: vec
      character(*), optional :: format
      character(128) :: fmt

      fmt = "row_wise"
      if (present(format)) fmt = format
      select case (fmt)
         case("by_row     ","row_wise   ","row        ")
            call put_strvec_by_row_(self,vec)
         case("by_column  ","column_wise","column     ")
            call put_strvec_by_column_(self,vec)
         case default
            allocate(tonto%known_keywords(6))
            tonto%known_keywords(1) = "by_row     "
            tonto%known_keywords(2) = "row_wise   "
            tonto%known_keywords(3) = "row        "
            tonto%known_keywords(4) = "by_column  "
            tonto%known_keywords(5) = "column_wise"
            tonto%known_keywords(6) = "column     "
            call unknown_(tonto,fmt,"TEXTFILE:put_strvec")
            deallocate(tonto%known_keywords)
      end select

   end subroutine

   subroutine put_strvec_by_column(self,vec)
    type(textfile_type) :: self
    ! Put a formatted integer vector into the output buffer
      character(128), dimension(:) :: vec
      integer(kind=kind(1)) :: dim,i

      dim = size(vec)
      do i = 1,dim
         if (self%use_labels) then
            call put_int_(self,i)
         end if
         call put_str_(self,vec(i))
         call flush_(self)
      end do

   end subroutine

   subroutine put_strvec_by_row(self,vec)
    type(textfile_type) :: self
    ! Put a formatted string vector into the output buffer
      character(128), dimension(:) :: vec
      integer(kind=kind(1)) :: dim,block,nb,f,l,i,fields

      dim = size(vec)
      fields = self%n_fields
      nb = (dim-0.1)/fields+1
      do block = 1,nb
         f = 1+fields*(block-1)
         l = min(f+fields-1,dim)
         if (block>1) call flush_(self)
         if (self%use_labels) then
!           .tab(real_fields=1)
            do i = f,l
               call put_int_(self,i,real_width=.true.)
            end do
            call flush_(self,2)
!           .tab(real_fields=1)
         end if
         do i = f,l
            call put_str_(self,vec(i))
         end do
         call flush_(self)
      end do

   end subroutine

   subroutine put_realvec(self,vec,format)
    type(textfile_type) :: self
    ! Put a formatted real(kind=kind(1.0d0)) vector into the output buffer
      real(kind=kind(1.0d0)), dimension(:) :: vec
      character(*), optional :: format
      character(128) :: fmt

      fmt = "row_wise"
      if (present(format)) fmt = format
      select case (fmt)
         case("by_row     ","row_wise   ","row        ")
            call put_realvec_by_row_(self,vec)
         case("by_column  ","column_wise","column     ")
            call put_realvec_by_column_(self,vec)
         case default
            allocate(tonto%known_keywords(6))
            tonto%known_keywords(1) = "by_row     "
            tonto%known_keywords(2) = "row_wise   "
            tonto%known_keywords(3) = "row        "
            tonto%known_keywords(4) = "by_column  "
            tonto%known_keywords(5) = "column_wise"
            tonto%known_keywords(6) = "column     "
            call unknown_(tonto,fmt,"TEXTFILE:put_realvec")
            deallocate(tonto%known_keywords)
      end select

   end subroutine

   subroutine put_realvec_by_column(self,vec)
    type(textfile_type) :: self
    ! Put a formatted integer vector into the output buffer
      real(kind=kind(1.0d0)), dimension(:) :: vec
      integer(kind=kind(1)) :: dim,i

      dim = size(vec)
      call warn_if_(tonto,dim==0,"TEXTFILE:put_realvec_by_column ... zero length vector!")
      do i = 1,dim
         if (self%use_labels) then
!           .put_int(i,real_width=.true.)
            call put_int_(self,i)
         end if
         call put_real_(self,vec(i),flush=1)
      end do

   end subroutine

   subroutine put_realvec_by_row(self,vec)
    type(textfile_type) :: self
    ! Put a formatted real(kind=kind(1.0d0)) vector into the output buffer
      real(kind=kind(1.0d0)), dimension(:) :: vec
      integer(kind=kind(1)) :: dim,block,nb,f,l,i,fields

      dim = size(vec)
      call warn_if_(tonto,dim==0,"TEXTFILE:put_realvec_by_row ... zero length vector!")
      fields = self%n_fields
      nb = (dim-0.1)/fields+1
      do block = 1,nb
         f = 1+fields*(block-1)
         l = min(f+fields-1,dim)
!         if (block>1) .flush
         if (self%use_labels) then
!           .tab(int_fields=1)
            do i = f,l
               call put_int_(self,i,real_width=.true.)
            end do
            call flush_(self,2)
!           .tab(int_fields=1)
         end if
         do i = f,l
            call put_real_(self,vec(i))
         end do
         call flush_(self)
      end do

   end subroutine

   subroutine put_cpxvec(self,vec,format)
    type(textfile_type) :: self
    ! Put a formatted real(kind=kind(1.0d0)) vector into the output buffer
      complex(kind=kind((1.0d0,1.0d0))), dimension(:) :: vec
      character(*), optional :: format
      character(128) :: fmt

      fmt = "row_wise"
      if (present(format)) fmt = format
      select case (fmt)
         case("by_row     ","row_wise   ","row        ")
            call put_cpxvec_by_row_(self,vec)
         case("by_column  ","column_wise","column     ")
            call put_cpxvec_by_column_(self,vec)
         case default
            allocate(tonto%known_keywords(6))
            tonto%known_keywords(1) = "by_row     "
            tonto%known_keywords(2) = "row_wise   "
            tonto%known_keywords(3) = "row        "
            tonto%known_keywords(4) = "by_column  "
            tonto%known_keywords(5) = "column_wise"
            tonto%known_keywords(6) = "column     "
            call unknown_(tonto,fmt,"TEXTFILE:put_cpxvec")
            deallocate(tonto%known_keywords)
      end select

   end subroutine

   subroutine put_cpxvec_by_column(self,vec)
    type(textfile_type) :: self
    ! Put a formatted complex vector into the output buffer
      complex(kind=kind((1.0d0,1.0d0))), dimension(:) :: vec
      integer(kind=kind(1)) :: dim,i

      dim = size(vec)
      call warn_if_(tonto,dim==0,"TEXTFILE:put_cpxvec_by_column ... zero length vector!")
      do i = 1,dim
         if (self%use_labels) then
            call put_int_(self,i,real_width=.true.)
         end if
         call put_cpx_(self,vec(i),flush=1)
      end do

   end subroutine

   subroutine put_cpxvec_by_row(self,vec)
    type(textfile_type) :: self
    ! Put a formatted real(kind=kind(1.0d0)) vector into the output buffer
      complex(kind=kind((1.0d0,1.0d0))), dimension(:) :: vec
      integer(kind=kind(1)) :: dim,block,nb,f,l,i,fields

      dim = size(vec)
      call warn_if_(tonto,dim==0,"TEXTFILE:put_cpxvec_by_row ... zero length vector!")
      fields = self%n_fields/2
      nb = (dim-0.1)/fields+1
      do block = 1,nb
         f = 1+fields*(block-1)
         l = min(f+fields-1,dim)
         if (block>1) call flush_(self)
         if (self%use_labels) then
!           .tab(int_fields=1)
            do i = f,l
               call tab_(self,real_fields=1)
               call put_int_(self,i,real_width=.true.)
            end do
            call flush_(self,2)
!           .tab(int_fields=1)
         end if
         do i = f,l
            call put_cpx_(self,vec(i))
         end do
         call flush_(self)
      end do

   end subroutine

   subroutine put_intmat(self,mx,order)
    type(textfile_type) :: self
    ! Put a formatted integer matrix into the output buffer
    ! If "order" is present use unformatted output suitable for ASCII archive
      integer(kind=kind(1)), dimension(:,:) :: mx
      character(*), optional :: order

      if (.not. present(order)) then      ! formatted output
         call put_labelled_intmat_(self,mx)
      else                              ! unformatted output
         select case (order)
            case ("by_column  ","column_wise","column     ","transpose  ")
               call put_intmat_by_column_(self,mx)
            case ("by_row     ","row_wise   ","row        ","normal     ")
               call put_intmat_by_row_(self,mx)
            case default
               allocate(tonto%known_keywords(8))
               tonto%known_keywords(1) = "by_column  "
               tonto%known_keywords(2) = "column_wise"
               tonto%known_keywords(3) = "column     "
               tonto%known_keywords(4) = "transpose  "
               tonto%known_keywords(5) = "by_row     "
               tonto%known_keywords(6) = "row_wise   "
               tonto%known_keywords(7) = "row        "
               tonto%known_keywords(8) = "normal     "
               call unknown_(tonto,order,"TEXTFILE:put_intmat")
               deallocate(tonto%known_keywords)
         end select
      end if

   end subroutine

   subroutine put_labelled_intmat(self,mx)
    type(textfile_type) :: self
    ! Put a labeled real(kind=kind(1.0d0)) matrix into the output buffer by column
      integer(kind=kind(1)), dimension(:,:) :: mx
      integer(kind=kind(1)) :: i,j,block,nb,f,l,dim1,dim2,fields

      call warn_if_(tonto,size(mx,1)==0,"TEXTFILE:put_labelled_intmat ... zero sized 1st dimension!")
      call warn_if_(tonto,size(mx,2)==0,"TEXTFILE:put_labelled_intmat ... zero sized 2nd dimension!")
      dim1 = size(mx,1)
      dim2 = size(mx,2)
      fields = self%n_fields
      nb = (dim2-0.1)/fields+1
      do block = 1,nb
         f = 1+fields*(block-1)
         l = min(f+fields-1,dim2)
         if (block>1) call flush_(self)
         if (self%use_labels) then
            call tab_(self,int_fields=1)
            do j = f,l
               call put_int_(self,j)
            end do
            call flush_(self,2)
         end if
         do i = 1,dim1
            if (self%use_labels)     call put_int_(self,i)
            if (.not. self%use_labels) call tab_(self,int_fields=1)
            do j = f,l
               call put_int_(self,mx(i,j))
            end do
            call flush_(self)
         end do
      end do

   end subroutine

   subroutine put_intmat_by_column(self,mx)
    type(textfile_type) :: self
    ! Put a real(kind=kind(1.0d0)) matrix into the output buffer by column, without labels
      integer(kind=kind(1)), dimension(:,:) :: mx
      integer(kind=kind(1)) :: i,j,dim1,dim2

      dim1 = size(mx,1)
      dim2 = size(mx,2)
      call warn_if_(tonto,dim1==0,"TEXTFILE:put_intmat_by_column ... zero sized 1st dimension!")
      call warn_if_(tonto,dim2==0,"TEXTFILE:put_intmat_by_column ... zero sized 2nd dimension!")
      do j = 1,dim2
      do i = 1,dim1
         call put_int_(self,mx(i,j))
         if(mod(i,self%n_fields)==0) call flush_(self)
      end do
      call flush_(self)
      end do

   end subroutine

   subroutine put_intmat_by_row(self,mx)
    type(textfile_type) :: self
    ! Put a real(kind=kind(1.0d0)) matrix into the output buffer by row, without labels
      integer(kind=kind(1)), dimension(:,:) :: mx
      integer(kind=kind(1)) :: i,j,dim1,dim2

      dim1 = size(mx,1)
      dim2 = size(mx,2)
      call warn_if_(tonto,dim1==0,"TEXTFILE:put_intmat_by_row ... zero sized 1st dimension!")
      call warn_if_(tonto,dim2==0,"TEXTFILE:put_intmat_by_row ... zero sized 2nd dimension!")
      do i = 1,dim1
      do j = 1,dim2
         call put_int_(self,mx(i,j))
         if(mod(j,self%n_fields)==0) call flush_(self)
      end do
      call flush_(self)
      end do

   end subroutine

   subroutine put_realmat(self,mx,order)
    type(textfile_type) :: self
    ! Put a formatted real(kind=kind(1.0d0)) matrix into the output buffer
    ! If "order" is present use unformatted output suitable for ASCII archive
      real(kind=kind(1.0d0)), dimension(:,:) :: mx
      character(*), optional :: order

      if (.not. present(order)) then
         call put_labelled_mat_(self,mx)
      else
         select case (order)
            case("by_column  ","column_wise","column     ","transpose  ")
               call put_realmat_by_column_(self,mx)
            case("by_row     ","row_wise   ","row        ","normal     ")
               call put_realmat_by_row_(self,mx)
            case default
               allocate(tonto%known_keywords(8))
               tonto%known_keywords(1) = "by_column  "
               tonto%known_keywords(2) = "column_wise"
               tonto%known_keywords(3) = "column     "
               tonto%known_keywords(4) = "transpose  "
               tonto%known_keywords(5) = "by_row     "
               tonto%known_keywords(6) = "row_wise   "
               tonto%known_keywords(7) = "row        "
               tonto%known_keywords(8) = "normal     "
               call unknown_(tonto,order,"TEXTFILE:put_realmat")
               deallocate(tonto%known_keywords)
         end select
      end if

   end subroutine

   subroutine put_labelled_mat(self,mx)
    type(textfile_type) :: self
    ! Put a labeled real(kind=kind(1.0d0)) matrix into the output buffer by column
      real(kind=kind(1.0d0)), dimension(:,:) :: mx
      integer(kind=kind(1)) :: i,j,block,nb,f,l,dim1,dim2,fields

      call warn_if_(tonto,size(mx,1)==0,"TEXTFILE:put_labelled_mat ... zero sized 1st dimension!")
      call warn_if_(tonto,size(mx,2)==0,"TEXTFILE:put_labelled_mat ... zero sized 2nd dimension!")
      dim1 = size(mx,1)
      dim2 = size(mx,2)
      fields = self%n_fields
      nb = (dim2-0.1)/fields+1
      do block = 1,nb
         f = 1+fields*(block-1)
         l = min(f+fields-1,dim2)
         if (block>1) call flush_(self)
         if (self%use_labels) then
            call tab_(self,int_fields=1)
            do j = f,l
               call put_int_(self,j,real_width=.true.)
            end do
            call flush_(self,2)
         end if
         do i = 1,dim1
            if (self%use_labels)     call put_int_(self,i)
            if (.not. self%use_labels) call tab_(self,int_fields=1)
            do j = f,l
               call put_real_(self,mx(i,j))
            end do
            call flush_(self)
         end do
      end do

   end subroutine

   subroutine put_realmat_by_column(self,mx)
    type(textfile_type) :: self
    ! Put a real(kind=kind(1.0d0)) matrix into the output buffer by column, without labels
      real(kind=kind(1.0d0)), dimension(:,:) :: mx
      integer(kind=kind(1)) :: i,j,dim1,dim2

      dim1 = size(mx,1)
      dim2 = size(mx,2)
      call warn_if_(tonto,dim1==0,"TEXTFILE:put_realmat_by_column ... zero sized 1st dimension!")
      call warn_if_(tonto,dim2==0,"TEXTFILE:put_realmat_by_column ... zero sized 2nd dimension!")
      do j = 1,dim2
      do i = 1,dim1
         call put_real_(self,mx(i,j))
         if(mod(i,self%n_fields)==0) call flush_(self)
      end do
      call flush_(self)
      end do

   end subroutine

   subroutine put_realmat_by_row(self,mx)
    type(textfile_type) :: self
    ! Put a real(kind=kind(1.0d0)) matrix into the output buffer by row, without labels
      real(kind=kind(1.0d0)), dimension(:,:) :: mx
      integer(kind=kind(1)) :: i,j,dim1,dim2

      dim1 = size(mx,1)
      dim2 = size(mx,2)
      call warn_if_(tonto,dim1==0,"TEXTFILE:put_realmat_by_row ... zero sized 1st dimension!")
      call warn_if_(tonto,dim2==0,"TEXTFILE:put_realmat_by_row ... zero sized 2nd dimension!")
      do i = 1,dim1
      do j = 1,dim2
         call put_real_(self,mx(i,j))
         if(mod(j,self%n_fields)==0) call flush_(self)
      end do
      call flush_(self)
      end do

   end subroutine

   subroutine put_formatted_mat(self,mx,form)
    type(textfile_type) :: self
    ! Put out a matrix suitable to be read in by other programs.
    ! Will overwrite the current buffer.
     real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: mx
     character(*), intent(in) :: form
     integer(kind=kind(1)) :: columns,first,last,i,j,nbasis,count,old_margin
     character(128) :: forma,formb

     i = scan(form,"FfEe")
     call get_next_item_position_(form,first,last)
     call die_if_(tonto,i==0,"TEXTFILE:put_formatted_mat ... incorrect format specification")
     forma=" "
     forma(1:i-first)=form(first:i-1)
     call die_if_(tonto,.not. is_int_(forma),"TEXTFILE:put_formatted_mat ... incorrect format specification")
     columns = to_int_(forma)
     formb=" "
     formb(1:last-i+1) = form(i:last)
     call clear_(self%buffer)
     old_margin = self%margin_width
     self%margin_width = 0
     nbasis=size(mx,1)
     count=0
     do i=1,nbasis
       do j=1,nbasis
         call put_(self%buffer,mx(i,j), trim(formb))
         count=count+1
         if (count==columns) then
           call flush_(self)
           count=0
         end if
       end do
     end do
     if (.not. count == 0) call flush_(self)
     self%margin_width = old_margin

   end subroutine

   subroutine put_cpxmat(self,mx,order)
    type(textfile_type) :: self
    ! Put a formatted complex(kind=kind((1.0d0,1.0d0))) matrix into the output buffer
    ! If "order" is present use unformatted output suitable for ASCII archive
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: mx
      character(*), optional :: order
      integer(kind=kind(1)) :: i,j,block,nb,f,l,dim1,dim2,fields

      dim1 = size(mx,1)
      dim2 = size(mx,2)
      call warn_if_(tonto,dim1==0,"TEXTFILE:put_cpxmat ... zero sized 1st dimension!")
      call warn_if_(tonto,dim2==0,"TEXTFILE:put_cpxmat ... zero sized 2nd dimension!")
      fields = self%n_fields/2
      if (.not. present(order)) then
         nb = (dim2-0.1)/fields+1
         do block = 1,nb
            f = 1+fields*(block-1)
            l = min(f+fields-1,dim2)
            if (block>1) call flush_(self)
            if (self%use_labels) then
               call tab_(self,int_fields=1)
               do j = f,l
                  call tab_(self,real_fields=1)
                  call put_int_(self,j,real_width=.true.)
               end do
               call flush_(self,2)
            end if
            do i = 1,dim1
               if (self%use_labels)     call put_int_(self,i)
               if (.not. self%use_labels) call tab_(self,int_fields=1)
               do j = f,l
                  call put_cpx_(self,mx(i,j))
               end do
               call flush_(self)
            end do
         end do
      else
         select case (order)
            case ("by_column  ","column_wise","transpose  ")
               do j = 1,dim2
               do i = 1,dim1
                  call put_cpx_(self,mx(i,j))
                  if(mod(i,fields)==0) call flush_(self)
               end do
               call flush_(self)
               end do
            case ("by_row     ","row_wise   ","normal     ")
               do i = 1,dim1
               do j = 1,dim2
                  call put_cpx_(self,mx(i,j))
                 if(mod(j,fields)==0) call flush_(self)
               end do
               call flush_(self)
               end do
            case default
               allocate(tonto%known_keywords(6))
               tonto%known_keywords(1) = "by_column  "
               tonto%known_keywords(2) = "column_wise"
               tonto%known_keywords(3) = "transpose  "
               tonto%known_keywords(4) = "by_row     "
               tonto%known_keywords(5) = "row_wise   "
               tonto%known_keywords(6) = "normal     "
               call unknown_(tonto,order,"TEXTFILE:put_cpxmat")
               deallocate(tonto%known_keywords)
         end select
      end if

   end subroutine

   subroutine put_realmat3(self,mx)
    type(textfile_type) :: self
    ! Put a REALMAT3 into the output buffer flat style
     real(kind=kind(1.0d0)), dimension(:,:,:) :: mx
     integer(kind=kind(1)) :: a,b,c,aub,bub,cub

     aub=size(mx,1)
     bub=size(mx,2)
     cub=size(mx,3)
     call warn_if_(tonto,aub==0,"TEXTFILE:put_realmat3 ... zero sized 1st dimension!")
     call warn_if_(tonto,bub==0,"TEXTFILE:put_realmat3 ... zero sized 2nd dimension!")
     call warn_if_(tonto,cub==0,"TEXTFILE:put_realmat3 ... zero sized 3rd dimension!")
     do a=1,aub
       do b=1,bub
         do c=1,cub
           call put_int_(self,a)
           call put_int_(self,b)
           call put_int_(self,c)
           call put_real_(self,mx(a,b,c),flush=1)
         end do
       end do
     end do

   end subroutine

   subroutine put_realmat4(self,mx)
    type(textfile_type) :: self
    ! Put a REALMAT4 into the output buffer flat style
     real(kind=kind(1.0d0)), dimension(:,:,:,:) :: mx
     integer(kind=kind(1)) :: a,b,c,d,aub,bub,cub,dub

     aub=size(mx,1)
     bub=size(mx,2)
     cub=size(mx,3)
     dub=size(mx,4)
     call warn_if_(tonto,aub==0,"TEXTFILE:put_realmat4 ... zero sized 1st dimension!")
     call warn_if_(tonto,bub==0,"TEXTFILE:put_realmat4 ... zero sized 2nd dimension!")
     call warn_if_(tonto,cub==0,"TEXTFILE:put_realmat4 ... zero sized 3rd dimension!")
     call warn_if_(tonto,dub==0,"TEXTFILE:put_realmat4 ... zero sized 4th dimension!")
     do a=1,aub
       do b=1,bub
         do c=1,cub
           do d=1,dub
             call put_int_(self,a)
             call put_int_(self,b)
             call put_int_(self,c)
             call put_int_(self,d)
             call put_real_(self,mx(a,b,c,d),flush=1)
           end do
         end do
       end do
     end do

   end subroutine

   subroutine put_cpxmat3(self,mx)
    type(textfile_type) :: self
    ! Put a REALMAT3 into the output buffer flat style
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:) :: mx
     integer(kind=kind(1)) :: a,b,c,aub,bub,cub

     aub=size(mx,1)
     bub=size(mx,2)
     cub=size(mx,3)
     call warn_if_(tonto,aub==0,"TEXTFILE:put_cpxmat3 ... zero sized 1st dimension!")
     call warn_if_(tonto,bub==0,"TEXTFILE:put_cpxmat3 ... zero sized 2nd dimension!")
     call warn_if_(tonto,cub==0,"TEXTFILE:put_cpxmat3 ... zero sized 3rd dimension!")
     do a=1,aub
       do b=1,bub
         do c=1,cub
           call put_int_(self,a)
           call put_int_(self,b)
           call put_int_(self,c)
           call put_cpx_(self,mx(a,b,c),flush=1)
         end do
       end do
     end do

   end subroutine

   subroutine put_cpxmat4(self,mx)
    type(textfile_type) :: self
    ! Put a CPXMAT4 into the output buffer flat style
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:,:) :: mx
     integer(kind=kind(1)) :: a,b,c,d,aub,bub,cub,dub

     aub=size(mx,1)
     bub=size(mx,2)
     cub=size(mx,3)
     dub=size(mx,4)
     call warn_if_(tonto,aub==0,"TEXTFILE:put_cpxmat4 ... zero sized 1st dimension!")
     call warn_if_(tonto,bub==0,"TEXTFILE:put_cpxmat4 ... zero sized 2nd dimension!")
     call warn_if_(tonto,cub==0,"TEXTFILE:put_cpxmat4 ... zero sized 3rd dimension!")
     call warn_if_(tonto,dub==0,"TEXTFILE:put_cpxmat4 ... zero sized 4th dimension!")
     do a=1,aub
       do b=1,bub
         do c=1,cub
           do d=1,dub
             call put_int_(self,a)
             call put_int_(self,b)
             call put_int_(self,c)
             call put_int_(self,d)
             call put_cpx_(self,mx(a,b,c,d),flush=1)
           end do
         end do
       end do
     end do

   end subroutine

   subroutine put_cpxmat5(self,mx)
    type(textfile_type) :: self
    ! Put a CPXMAT5 into the output buffer flat style
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:,:,:) :: mx
     integer(kind=kind(1)) :: a,b,c,d,e,aub,bub,cub,dub,eub

     aub=size(mx,1)
     bub=size(mx,2)
     cub=size(mx,3)
     dub=size(mx,4)
     eub=size(mx,5)
     do a=1,aub
     do b=1,bub
     do c=1,cub
     do d=1,dub
     do e=1,eub
        call put_int_(self,a)
        call put_int_(self,b)
        call put_int_(self,c)
        call put_int_(self,d)
        call put_int_(self,e)
        call put_cpx_(self,mx(a,b,c,d,e),flush=1)
     end do
     end do
     end do
     end do
     end do

   end subroutine

!   put_gaussian(g)
!   ! Output gaussian information
!       g :: type(gaussian_type)
!      .flush
!      .show("L quantum number = ", g.l)
!      .show("Position         = ", g.pos(1), g.pos(2), g.pos(3))
!      .show("Exponent         = ", g.ex)
!   end

!   put_gaussian2(g)
!   ! Output gaussian information
!       g :: type(gaussian2_type)
!      .flush
!      .text("Shell a:")
!      .show("L quantum number = ", g.a.l)
!      .show("Position         = ", g.a.pos(1), g.a.pos(2), g.a.pos(3))
!      .show("Exponent         = ", g.a.ex)
!      .text("Shell b:")
!      .show("L quantum number = ", g.b.l)
!      .show("Position         = ", g.b.pos(1), g.b.pos(2), g.b.pos(3))
!      .show("Exponent         = ", g.b.ex)
!   end

   subroutine put_intvecvec(self,vec)
    type(textfile_type) :: self
    ! Put a integer vector list into the output buffer
      type(intvec__type), dimension(:) :: vec
      integer(kind=kind(1)) :: i
      logical(kind=kind(.true.)) :: use_labels

      use_labels = self%use_labels
      call set_use_labels_(self,.false.)
      do i = 1,size(vec)
         call put_int_(self,i)
         call put_text_(self,":")
         call put_intvec_by_row_(self,vec(i)%element)
      end do
      call set_use_labels_(self,use_labels)

   end subroutine

   subroutine put_opvector(self,vec,format)
    type(textfile_type) :: self
    ! Outputs the opvector
     type(opvector_type), intent(in) :: vec
     character(*), optional :: format

     if (associated( vec%restricted)) then
       call flush_(self)
       call put_text_(self,"restricted part:",flush=2)
       call put_realvec_(self, vec%restricted,format)
     end if
     if (associated( vec%alpha)) then
       call flush_(self)
       call put_text_(self,"alpha part:",flush=2)
       call put_realvec_(self, vec%alpha,format)
     end if
     if (associated( vec%beta)) then
       call flush_(self)
       call put_text_(self,"beta part:",flush=2)
       call put_realvec_(self, vec%beta,format)
     end if
     if (associated( vec%general)) then
       call flush_(self)
       call put_text_(self,"general part:",flush=2)
       call put_realvec_(self, vec%general,format)
     end if

   end subroutine

   subroutine put_opmatrix(self,mat,order)
    type(textfile_type) :: self
    ! Outputs the opmatrix
     type(opmatrix_type), intent(in) :: mat
     character(*), optional :: order

     if (associated( mat%restricted)) then
       call flush_(self)
       call put_text_(self,"restricted part:",flush=2)
       call put_realmat_(self, mat%restricted,order)
     end if
     if (associated( mat%alpha)) then
       call flush_(self)
       call put_text_(self,"alpha part:",flush=2)
       call put_realmat_(self, mat%alpha,order)
     end if
     if (associated( mat%beta)) then
       call flush_(self)
       call put_text_(self,"beta part:",flush=2)
       call put_realmat_(self, mat%beta,order)
     end if
     if (associated( mat%general)) then
       call flush_(self)
       call put_text_(self,"general part:",flush=2)
       call put_realmat_(self, mat%general,order)
     end if
     if (associated( mat%restricted_complex)) then
       call flush_(self)
       call put_text_(self,"complex restricted part:",flush=2)
       call put_cpxmat_(self, mat%restricted_complex,order)
     end if
     if (associated( mat%alpha_complex)) then
       call flush_(self)
       call put_text_(self,"complex alpha part:",flush=2)
       call put_cpxmat_(self, mat%alpha_complex,order)
     end if
     if (associated( mat%beta_complex)) then
       call flush_(self)
       call put_text_(self,"beta part:",flush=2)
       call put_cpxmat_(self, mat%beta_complex,order)
     end if
     if (associated( mat%general_complex)) then
       call flush_(self)
       call put_text_(self,"complex general part:",flush=2)
       call put_cpxmat_(self, mat%general_complex,order)
     end if

   end subroutine

!  ****************
!  Output self info
!  ****************

   subroutine put_info(self)
    type(textfile_type) :: self
    ! Put all the available molecule information on file "out"

      call show_(stdout,"Name       =",self%name)
      call show_(stdout,"I/O action =",self%action)
      call show_(stdout,"Unit       =",self%unit)
      call show_(stdout,"Line       =",self%record)
      call show_(stdout,"Buffer     =",trim(self%buffer%string))
      call show_(stdout,"Cursor pos =",trim(cursor_pointer_(self%buffer)))

   end subroutine

!  ***************
!  Set fmt methods
!  ***************

   subroutine set_default_units(self,units)
    type(textfile_type) :: self
    ! Set the .default_units to "units". This is reset back to 1
    ! after a particular number has been read and converted.
      character(*) :: units

      call ensure_(tonto,units==" " .or. is_known_unit_(units),"TEXTFILE:set_default_units ... Unknown units!")
      self%default_units = units

   end subroutine

   subroutine set_comment_chars(self,comment_chars)
    type(textfile_type) :: self
    ! Set .comment_chars to "comment_chars".
      character(*) :: comment_chars

      self%comment_chars = comment_chars
     ! .buffer.set(comment_chars=comment_chars)
      self%buffer%comment_chars = comment_chars

   end subroutine

   subroutine set_quote_chars(self,quote_chars)
    type(textfile_type) :: self
    ! Set .quote_chars to "quote_chars".
      character(*) :: quote_chars

      self%quote_chars = quote_chars
     ! .buffer.set(quote_chars=quote_chars)
      self%buffer%quote_chars = quote_chars

   end subroutine

   subroutine set_use_labels(self,use_labels)
    type(textfile_type) :: self
    ! Set whether to use numbered column or row labels on matrix or vector output
      logical(kind=kind(.true.)) :: use_labels

      self%use_labels = use_labels

   end subroutine

   subroutine set_margin(self,margin_width)
    type(textfile_type) :: self
    ! Set the width of the margin in the buffer. Takes effect at the next
    ! output line
      integer(kind=kind(1)) :: margin_width
      integer(kind=kind(1)) :: item_end

      call ensure_(tonto,margin_width>=0,"TEXTFILE:set_margin ... margin width less than zero")
      call ensure_(tonto,margin_width<=128,"TEXTFILE:set_margin ... margin width too large")
      item_end=self%buffer%item_end  ! to shorten the ENSURE line below.
      call ensure_(tonto,item_end<=self%margin_width,"TEXTFILE:set_margin ... set margin width only on empty buffers")
      self%margin_width = margin_width
      call clear_(self%buffer)
      call put_margin_(self)

   end subroutine

!  ************************
!  Inherited REALfmt methods
!  ************************

   subroutine set_default_format(self)
    type(textfile_type) :: self
    ! Set the default settings for the real(kind=kind(1.0d0)) formatting object
    ! Extra functions added compared to inherited code

      self%use_labels    = .true.
      self%margin_width  = 0
      self%n_fields      = 5
      self%int_width     = 9
      self%real_width     = 16
      self%real_precision = 6
      self%real_style     = " f"

   end subroutine

   subroutine set_int_width(self,width)
    type(textfile_type) :: self
    ! Set the width of an integer in the format object
      integer(kind=kind(1)) :: width

      call ensure_(tonto,width>=0,"TEXTFILE:set_int_width ... width less than zero")
      self%int_width = width

   end subroutine

   subroutine set_real_width(self,width)
    type(textfile_type) :: self
    ! Set the width in the realfmt format object
      integer(kind=kind(1)) :: width

      call ensure_(tonto,width>=0,"TEXTFILE:set_real_width ... width less than zero")
      call ensure_(tonto,width>=self%real_precision,"TEXTFILE:set_real_width ... width smaller than precision")
      self%real_width = width

   end subroutine

   subroutine set_n_fields(self,n_fields)
    type(textfile_type) :: self
    ! Set the number of fields in the realfmt format object
      integer(kind=kind(1)) :: n_fields

      call ensure_(tonto,n_fields>=1,"TEXTFILE:set_n_fields ... not enough fields")
      self%n_fields = n_fields

   end subroutine

   subroutine set_precision(self,precision)
    type(textfile_type) :: self
    ! Set the precision required in the realfmt format object
      integer(kind=kind(1)) :: precision

      call set_real_precision_(self,precision)

   end subroutine

   subroutine set_real_precision(self,precision)
    type(textfile_type) :: self
    ! Set the precision required in the realfmt format object
      integer(kind=kind(1)) :: precision
      integer(kind=kind(1)) :: w

      call ensure_(tonto,precision>=0,"TEXTFILE:set_real_precision ... precision less than zero")
      call ensure_(tonto,precision<=self%real_width,"TEXTFILE:set_real_precision ... precision greater than field width")
      w = self%int_width - (self%real_width-self%real_precision-1)
      call warn_if_(tonto,w>0,"TEXTFILE:set_real_precision ... width may be too small")
      self%real_precision = precision

   end subroutine

   subroutine set_real_style(self,real_style)
    type(textfile_type) :: self
    ! Set the fortran format style string in the realfmt format object
      character(*) :: real_style
      character(2) :: style

      style = adjustl(real_style)
      select case (style)
        case ("f","d","e","en","es")
        case default; allocate(tonto%known_keywords(5))
        tonto%known_keywords(1) = "f"
        tonto%known_keywords(2) = "d"
        tonto%known_keywords(3) = "e"
        tonto%known_keywords(4) = "en"
        tonto%known_keywords(5) = "es"
        call unknown_(tonto,style,"TEXTFILE:set_real_style")
        deallocate(tonto%known_keywords)
      end select
      self%real_style = style

   end subroutine

   function nice_field_width_for(self,num) result(res)
    type(textfile_type) :: self
    ! Return the field width that would look nice
     integer(kind=kind(1)) :: res
     integer(kind=kind(1)), intent(in) :: num
     real(kind=kind(1.0d0)) :: temp

     res=0
     if (num<0) res=res+1
     temp=abs(num)
     do
       res=res+1
       temp=temp/10
       if (temp < 1.0d0) exit
     end do

   end function

   function format_for_real(self,style,width,precision) result(res)
    type(textfile_type) :: self
    ! Format for a real(kind=kind(1.0d0)) string
      character(128) :: res
      character(128), intent(in) :: style
      integer(kind=kind(1)), intent(in) :: width,precision
      integer(kind=kind(1)) :: n

      res = trim(style) // trim( to_str_(width)) // "." // trim( to_str_(precision))
      if (trim(style)=="e" .or. trim(style)=="E") then
         ! Max number of digits permitted for the exponent.
        n = max(ceiling(log10(huge(1.0d0))),width-precision-5)
        if (width-precision-n>=5) then
          res = trim(res) // "E" // trim(to_str_(n))
        end if
      end if

   end function

   function format_for_int(self,width) result(res)
    type(textfile_type) :: self
    ! Format for a integer(kind=kind(1)) string
      character(128) :: res
      integer(kind=kind(1)), intent(in) :: width

      res = "i" // trim( to_str_(width))

   end function

   function format_for_bit_string(self,width) result(res)
    type(textfile_type) :: self
    ! Format for a integer(kind=kind(1)) string
      character(128) :: res
      integer(kind=kind(1)), intent(in) :: width

      res = "b" // trim( to_str_(width))

   end function

   function format_for_bin(self,width) result(res)
    type(textfile_type) :: self
    ! Format for a integer(kind=kind(1)) string
      character(128) :: res
      integer(kind=kind(1)), intent(in) :: width

      res = "l" // trim( to_str_(width))

   end function

!  ***************
!  Inquiry methods
!  ***************

   function exists(self,name) result(res)
    type(textfile_type) :: self
    ! Returns true if the file exists on the file system.
    ! Uses "name" if present, otherwise ".name".
      character(*), optional :: name
      logical(kind=kind(.true.)) :: res

      if (present(name)) then
        if (do_io_(tonto_parallel)) then
          inquire(file=name,exist=res)
        end if
      else
        if (do_io_(tonto_parallel)) then
          inquire(file=self%name,exist=res)
        end if
      end if
      call broadcast_(tonto_parallel,res,0)

   end function

   function is_open(self) result(res)
    type(textfile_type) :: self
    ! Returns true if the file has been opened
      logical(kind=kind(.true.)) :: res
       ! inquire(unit=.unit,opened=res)

      if (self%unit==5 .or. self%unit==6) then
        res = .true.  ! not needed for stdin, stdout
      else
        if (do_io_(tonto_parallel)) then
          inquire(file=self%name,opened=res)
        end if
      end if
      call broadcast_(tonto_parallel,res,0)

   end function

   function unit_used(self) result(res)
    type(textfile_type) :: self
    ! Returns true if the file unit is in use
      logical(kind=kind(.true.)) :: res

      if (do_io_(tonto_parallel)) then
        inquire(unit=self%unit,opened=res)
      end if
      call broadcast_(tonto_parallel,res,0)

   end function

   function next_line_item(self) result(res)
    type(textfile_type) :: self
    ! Return the index of the next item to be processed on the line
      integer(kind=kind(1)) :: res

      res = next_item_number_(self%buffer)

   end function

   function previous_line_item(self) result(res)
    type(textfile_type) :: self
    ! Return the index of the previous item to be processed on the line
      integer(kind=kind(1)) :: res

      res = self%buffer%item_index

   end function

   function last_line_item(self) result(res)
    type(textfile_type) :: self
    ! Return the index of the final item on the line
      integer(kind=kind(1)) :: res

      res = self%buffer%n_items

   end function

   function n_line_items(self) result(res)
    type(textfile_type) :: self
    ! Return the number of items on the line
      integer(kind=kind(1)) :: res

      res = self%buffer%n_items

   end function

   function at_end_of_line(self) result(res)
    type(textfile_type) :: self
    ! Return .true. if at the end of the line
      logical(kind=kind(.true.)) :: res

      res = self%buffer%item_index==self%buffer%n_items

   end function

   function line_number(self) result(res)
    type(textfile_type) :: self
    ! Return the input file line number which is being processed
      integer(kind=kind(1)) :: res

      res = self%record

   end function

   function buffer_string(self) result(res)
    type(textfile_type) :: self
    ! Put a string into the buffer
      character(256) :: res

      res = buffer_string_(self%buffer)

   end function

   function end_of_file(self) result(res)
    type(textfile_type) :: self
    ! See if .io_status>1, indicating the end of file has been found.
      logical(kind=kind(.true.)) :: res

      res = self%io_status>0

   end function

    !reverted(reset) result(res)
   function reverted(self) result(res)
    type(textfile_type) :: self
    ! See if .io_status==-1, indicating an internal file has ended (i.e. a soft
    ! ending).  NOTE: Unless the "reset" variable is present and .false., the
    ! .io_status variable is reset by default. So if you want the soft ending to
    ! be detectable by later routines, you must call this routine with
    ! "reset=FALSE".
     !  reset :: logical(kind=kind(.true.)), optional
      logical(kind=kind(.true.)) :: res
     ! reset_io_status :: logical(kind=kind(.true.))

      res = self%io_status==-1
     ! reset_io_status = .true.
     ! if (present(reset)) reset_io_status = reset
     ! if (reset_io_status) .io_status = 0

   end function

   function at_end_of_file(self) result(res)
    type(textfile_type) :: self
    ! Read a line into the buffer from the input file and see if it is at the end
    ! of file. This is an explicit test, not just a viewing of ".io_status".
    ! NOTE: If all lines to the end of file are empty, then the result is also
    ! true.
      logical(kind=kind(.true.)) :: res
      character(128) :: word
      logical(kind=kind(.true.)) :: ignore

      call ensure_(tonto,self%action=="read","TEXTFILE:at_end_of_file ... file does not have read status")
      ignore = self%ignore_end_of_file  ! Save this
      self%ignore_end_of_file = .true.
      call read_str_(self,word)
      res = end_of_file_(self)
      self%io_status = 0
      call move_to_previous_item_(self)
      self%ignore_end_of_file = ignore  ! Put back ignore state

   end function

   function buffer_exhausted(self) result(res)
    type(textfile_type) :: self
    ! Return whether the buffer is exhausted
     logical(kind=kind(.true.)) :: res

       res = exhausted_(self%buffer)

   end function

! ***************
! System routines
! ***************

   subroutine update_system_info(self)
    type(textfile_type) :: self
    ! Lets the system know info about the file being read, in case of error.
     target :: self

     tonto%io_file => self

   end subroutine

end
