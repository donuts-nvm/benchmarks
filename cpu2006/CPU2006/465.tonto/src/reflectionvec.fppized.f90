!---------------------------------------------------------------------------
!
!  REFLECTIONVEC: a vector of crystal reflection data
!
! Copyright (C) Daniel Grimwood, 2000
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
! $Id: reflectionvec.foo,v 1.11.2.3 2003/11/13 05:36:07 reaper Exp $
!---------------------------------------------------------------------------

module REFLECTIONVEC_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

   use REALVEC_MODULE, only: swap_elements_
   use REALVEC_MODULE, only: sort_

   use REFLECTION_MODULE, only: read_keys_
   use REFLECTION_MODULE, only: clear_keys_
   use REFLECTION_MODULE, only: set_keys_
   use REFLECTION_MODULE, only: process_keys_
   use REFLECTION_MODULE, only: set_defaults_
   use REFLECTION_MODULE, only: put_table_footer_
   use REFLECTION_MODULE, only: F_z_
   use REFLECTION_MODULE, only: put_table_header_
   use REFLECTION_MODULE, only: create_
   use REFLECTION_MODULE, only: I_z_
   use REFLECTION_MODULE, only: destroy_
   use REFLECTION_MODULE, only: keys_created_
   use REFLECTION_MODULE, only: copy_

   use INT_MODULE, only: to_str_

   use STR_MODULE, only: is_int_
   use STR_MODULE, only: to_lower_case_
   use STR_MODULE, only: to_int_

   use TEXTFILE_MODULE, only: stdin
   use TEXTFILE_MODULE, only: stdout
   use TEXTFILE_MODULE, only: create_
   use TEXTFILE_MODULE, only: redirect_
   use TEXTFILE_MODULE, only: close_
   use TEXTFILE_MODULE, only: show_
   use TEXTFILE_MODULE, only: read_
   use TEXTFILE_MODULE, only: at_end_of_file_
   use TEXTFILE_MODULE, only: set_use_labels_
   use TEXTFILE_MODULE, only: flush_
   use TEXTFILE_MODULE, only: destroy_
   use TEXTFILE_MODULE, only: next_str_
   use TEXTFILE_MODULE, only: text_
   use TEXTFILE_MODULE, only: open_
   use TEXTFILE_MODULE, only: previous_line_item_
   use TEXTFILE_MODULE, only: line_number_
   use TEXTFILE_MODULE, only: next_item_
   use TEXTFILE_MODULE, only: move_to_line_
   use TEXTFILE_MODULE, only: put_
   use TEXTFILE_MODULE, only: revert_
   use TEXTFILE_MODULE, only: move_to_line_item_
   use TEXTFILE_MODULE, only: move_to_previous_item_
   use TEXTFILE_MODULE, only: reverted_

   use REAL_MODULE, only: z_from_p_
   use REAL_MODULE, only: to_random_normal_

   use ARCHIVE_MODULE, only: set_
   use ARCHIVE_MODULE, only: write_
   use ARCHIVE_MODULE, only: close_

   use REALMAT_MODULE, only: create_
   use REALMAT_MODULE, only: destroy_

   use INTMAT_MODULE, only: swap_columns_
   use INTMAT_MODULE, only: create_
   use INTMAT_MODULE, only: destroy_

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

   public    scale_F_exp_
   interface scale_F_exp_
      module procedure scale_F_exp
   end interface

   public    read_keys_
   interface read_keys_
      module procedure read_keys
   end interface

   public    scale_F_sigma_
   interface scale_F_sigma_
      module procedure scale_F_sigma
   end interface

   public    have_F_sigma_
   interface have_F_sigma_
      module procedure have_F_sigma
   end interface

   public    scale_F_calc_
   interface scale_F_calc_
      module procedure scale_F_calc
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

   public    have_F_exp_
   interface have_F_exp_
      module procedure have_F_exp
   end interface

   public    put_keys_table_
   interface put_keys_table_
      module procedure put_keys_table
   end interface

   public    put_structure_factor_data_
   interface put_structure_factor_data_
      module procedure put_structure_factor_data
   end interface

   public    F_exp_
   interface F_exp_
      module procedure F_exp
   end interface

   public    F_calc_
   interface F_calc_
      module procedure F_calc
   end interface

   public    I_r_factor_
   interface I_r_factor_
      module procedure I_r_factor
   end interface

   public    F_r_factor_
   interface F_r_factor_
      module procedure F_r_factor
   end interface

   public    copy_
   interface copy_
      module procedure copy
   end interface

   public    have_I_pred_
   interface have_I_pred_
      module procedure have_I_pred
   end interface

   public    clear_keys_
   interface clear_keys_
      module procedure clear_keys
   end interface

   public    I_goodness_of_fit_
   interface I_goodness_of_fit_
      module procedure I_goodness_of_fit
   end interface

   public    redirect_
   interface redirect_
      module procedure redirect
   end interface

   public    put_F_qq_plot_
   interface put_F_qq_plot_
      module procedure put_F_qq_plot
   end interface

   public    set_indices_
   interface set_indices_
      module procedure set_indices
   end interface

   public    scale_F_pred_
   interface scale_F_pred_
      module procedure scale_F_pred
   end interface

   public    put_intensity_data_
   interface put_intensity_data_
      module procedure put_intensity_data
   end interface

   public    keys_created_
   interface keys_created_
      module procedure keys_created
   end interface

   public    put_F_pred_data_
   interface put_F_pred_data_
      module procedure put_F_pred_data
   end interface

   public    F_sigma_
   interface F_sigma_
      module procedure F_sigma
   end interface

   public    set_F_calc_
   interface set_F_calc_
      module procedure set_F_calc
   end interface

   public    I_weighted_r_factor_
   interface I_weighted_r_factor_
      module procedure I_weighted_r_factor
   end interface

   public    destroy_
   interface destroy_
      module procedure destroy
   end interface

   public    set_I_exp_
   interface set_I_exp_
      module procedure set_I_exp
   end interface

   public    F_pred_
   interface F_pred_
      module procedure F_pred
   end interface

   public    F_chi2_
   interface F_chi2_
      module procedure F_chi2
   end interface

   public    append_
   interface append_
      module procedure append
      module procedure append_1
   end interface

   public    revert_
   interface revert_
      module procedure revert
   end interface

   public    read_altered_data_
   interface read_altered_data_
      module procedure read_altered_data
   end interface

   public    have_indices_
   interface have_indices_
      module procedure have_indices
   end interface

   public    set_F_pred_
   interface set_F_pred_
      module procedure set_F_pred
   end interface

   public    simulate_new_F_exp_
   interface simulate_new_F_exp_
      module procedure simulate_new_F_exp
   end interface

   public    n_refl_
   interface n_refl_
      module procedure n_refl
   end interface

   public    add_random_error_
   interface add_random_error_
      module procedure add_random_error
   end interface

   public    have_F_calc_
   interface have_F_calc_
      module procedure have_F_calc
   end interface

   public    F_weighted_r_factor_
   interface F_weighted_r_factor_
      module procedure F_weighted_r_factor
   end interface

   public    process_keyword_
   interface process_keyword_
      module procedure process_keyword
   end interface

   public    put_I_stats_
   interface put_I_stats_
      module procedure put_I_stats
   end interface

   public    shrink_
   interface shrink_
      module procedure shrink
   end interface

   public    put_
   interface put_
      module procedure put
   end interface

   public    process_keys_
   interface process_keys_
      module procedure process_keys
   end interface

   public    make_F_qq_plot_grid_
   interface make_F_qq_plot_grid_
      module procedure make_F_qq_plot_grid
   end interface

   public    put_table_footer_
   interface put_table_footer_
      module procedure put_table_footer
   end interface

   public    read_list_keywords_
   interface read_list_keywords_
      module procedure read_list_keywords
   end interface

   public    read_data_
   interface read_data_
      module procedure read_data
   end interface

   public    have_I_exp_
   interface have_I_exp_
      module procedure have_I_exp
   end interface

   public    set_I_sigma_
   interface set_I_sigma_
      module procedure set_I_sigma
   end interface

   public    set_F_exp_
   interface set_F_exp_
      module procedure set_F_exp
   end interface

   public    set_
   interface set_
      module procedure set
   end interface

   public    set_defaults_
   interface set_defaults_
      module procedure set_defaults
   end interface

   public    put_F_stats_
   interface put_F_stats_
      module procedure put_F_stats
   end interface

   public    create_
   interface create_
      module procedure create
   end interface

   public    create_copy_
   interface create_copy_
      module procedure create_copy
   end interface

   public    indices_
   interface indices_
      module procedure indices
   end interface

   public    I_pred_
   interface I_pred_
      module procedure I_pred
   end interface

   public    read_append_data_
   interface read_append_data_
      module procedure read_append_data
   end interface

   public    I_chi2_
   interface I_chi2_
      module procedure I_chi2
   end interface

   public    set_F_sigma_
   interface set_F_sigma_
      module procedure set_F_sigma
   end interface

   public    set_keys_
   interface set_keys_
      module procedure set_keys
   end interface

   public    put_I_pred_data_
   interface put_I_pred_data_
      module procedure put_I_pred_data
   end interface

   public    expand_
   interface expand_
      module procedure expand
   end interface

   public    F_goodness_of_fit_
   interface F_goodness_of_fit_
      module procedure F_goodness_of_fit
   end interface

   public    have_F_pred_
   interface have_F_pred_
      module procedure have_F_pred
   end interface

   public    put_labelled_F_qq_plot_
   interface put_labelled_F_qq_plot_
      module procedure put_labelled_F_qq_plot
   end interface

   public    set_I_pred_
   interface set_I_pred_
      module procedure set_I_pred
   end interface

   public    process_list_keyword_
   interface process_list_keyword_
      module procedure process_list_keyword
   end interface

   public    put_I_exp_data_
   interface put_I_exp_data_
      module procedure put_I_exp_data
   end interface

   public    have_I_sigma_
   interface have_I_sigma_
      module procedure have_I_sigma
   end interface

   public    put_F_exp_data_
   interface put_F_exp_data_
      module procedure put_F_exp_data
   end interface

   type(reflection_type), dimension(:), pointer, private :: saved_self => NULL()

contains

!*******************************************************************************
!                           Create and Destroy Routines
!*******************************************************************************

   subroutine create(self,dim)
    type(reflection_type), dimension(:) :: self
    ! Create space for a reflection vector
     pointer :: self
     integer(kind=kind(1)), intent(in) :: dim

     nullify(self)
     allocate(self(dim))

     call set_defaults_(self)

   end subroutine

   subroutine create_copy(self,vec)
    type(reflection_type), dimension(:) :: self
    ! Create a replica copy of "vec".
      type(reflection_type), dimension(:), intent(in) :: vec
      pointer :: self
    ! The following code is inherited from OBJECTVEC

      call create_(self,size(vec))
      call copy_(self,vec)

   end subroutine

   subroutine copy(self,vec)
    type(reflection_type), dimension(:) :: self
    ! Copy "vec".
      type(reflection_type), dimension(:), intent(in) :: vec
    ! The following code is inherited from OBJECTVEC
      integer(kind=kind(1)) :: a

      call ensure_(tonto,size(self)==size(vec),"REFLECTIONVEC:copy ... vec size does not match")
      do a = 1,size(vec)
        call copy_(self(a),vec(a))
      end do

   end subroutine

   subroutine destroy(self)
    type(reflection_type), dimension(:) :: self
    ! Destroy space for a reflection vector
     pointer :: self
     integer(kind=kind(1)) :: dim

     if (.not. associated(self)) then;   return; end if
     dim = size(self)

     deallocate(self)

   end subroutine

   subroutine set_defaults(self)
    type(reflection_type), dimension(:) :: self
    ! Set defaults
      integer(kind=kind(1)) :: n

     do n = 1,size(self)
       call set_defaults_(self(n))
     end do

   end subroutine

!*******************************************************************************
!                           Data changing routines
!*******************************************************************************

   subroutine set_indices(self,h,k,l)
    type(reflection_type), dimension(:) :: self
    ! Set the reflection data from the vector.
     target :: self
     integer(kind=kind(1)), dimension(:), intent(in) :: h,k,l
     type(reflection_type), pointer :: ref
     integer(kind=kind(1)) :: n

     do n=1,n_refl_(self)
       ref => self(n)
       ref%h       = h(n)
       ref%k       = k(n)
       ref%l       = l(n)
     end do

   end subroutine

   subroutine set_F_calc(self,F_calc)
    type(reflection_type), dimension(:) :: self
    ! Set the reflection data from the vector.
     target :: self
     complex(kind=kind((1.0d0,1.0d0))), dimension(:), intent(in) :: F_calc
      integer(kind=kind(1)) :: n

     do n=1,n_refl_(self)
       self(n)%F_calc = F_calc(n)
     end do

   end subroutine

   subroutine set_F_pred(self,F_pred)
    type(reflection_type), dimension(:) :: self
    ! Set the reflection data from the vector.
     target :: self
     real(kind=kind(1.0d0)), dimension(:), intent(in) :: F_pred
      integer(kind=kind(1)) :: n

     do n=1,n_refl_(self)
       self(n)%F_pred = F_pred(n)
     end do

   end subroutine

   subroutine set_F_exp(self,F_exp)
    type(reflection_type), dimension(:) :: self
    ! Set the reflection data from the vector.
     target :: self
     real(kind=kind(1.0d0)), dimension(:), intent(in) :: F_exp
      integer(kind=kind(1)) :: n

     do n=1,n_refl_(self)
       self(n)%F_exp = F_exp(n)
     end do

   end subroutine

   subroutine set_F_sigma(self,F_sigma)
    type(reflection_type), dimension(:) :: self
    ! Set the reflection data from the vector.
     target :: self
     real(kind=kind(1.0d0)), dimension(:), intent(in) :: F_sigma
      integer(kind=kind(1)) :: n

     do n=1,n_refl_(self)
       self(n)%F_sigma = F_sigma(n)
     end do

   end subroutine

   subroutine set_I_pred(self,I_pred)
    type(reflection_type), dimension(:) :: self
    ! Set the reflection data from the vector.
     target :: self
     real(kind=kind(1.0d0)), dimension(:), intent(in) :: I_pred
      integer(kind=kind(1)) :: n

     do n=1,n_refl_(self)
       self(n)%I_pred = I_pred(n)
     end do

   end subroutine

   subroutine set_I_exp(self,I_exp)
    type(reflection_type), dimension(:) :: self
    ! Set the reflection data from the vector.
     target :: self
     real(kind=kind(1.0d0)), dimension(:), intent(in) :: I_exp
      integer(kind=kind(1)) :: n

     do n=1,n_refl_(self)
       self(n)%I_exp = I_exp(n)
     end do

   end subroutine

   subroutine set_I_sigma(self,I_sigma)
    type(reflection_type), dimension(:) :: self
    ! Set the reflection data from the vector.
     target :: self
     real(kind=kind(1.0d0)), dimension(:), intent(in) :: I_sigma
      integer(kind=kind(1)) :: n

     do n=1,n_refl_(self)
       self(n)%I_sigma = I_sigma(n)
     end do

   end subroutine

   subroutine set(self,ref)
    type(reflection_type), dimension(:) :: self
    ! Set the reflection data from.
     target :: self
     type(reflection_type), dimension(:), target :: ref
     type(reflection_type), pointer :: ref1,self1
      integer(kind=kind(1)) :: n

     do n=1,n_refl_(self)
       self1 => self(n)
       ref1  => ref(n)
       self1%h       = ref1%h
       self1%k       = ref1%k
       self1%l       = ref1%l
       self1%F_exp   = ref1%F_exp
       self1%F_pred  = ref1%F_pred
       self1%F_calc  = ref1%F_calc
       self1%F_sigma = ref1%F_sigma
       self1%I_exp   = ref1%I_exp
       self1%I_pred  = ref1%I_pred
       self1%I_sigma = ref1%I_sigma
     end do

   end subroutine

   subroutine scale_F_pred(self,fac)
    type(reflection_type), dimension(:) :: self
    ! Scale the predicted structure factors
     intent(inout) :: self
     real(kind=kind(1.0d0)), intent(in) :: fac
      integer(kind=kind(1)) :: n

     do n=1,size(self)
       self(n)%F_pred = self(n)%F_pred * fac
     end do

   end subroutine

   subroutine scale_F_calc(self,fac)
    type(reflection_type), dimension(:) :: self
    ! Scale the calculated structure factors
     intent(inout) :: self
     real(kind=kind(1.0d0)), intent(in) :: fac
      integer(kind=kind(1)) :: n

     do n=1,size(self)
       self(n)%F_calc = self(n)%F_calc * fac
     end do

   end subroutine

   subroutine scale_F_exp(self,fac)
    type(reflection_type), dimension(:) :: self
    ! Scale the experimental structure factors
     intent(inout) :: self
     real(kind=kind(1.0d0)), intent(in) :: fac
      integer(kind=kind(1)) :: n

     do n=1,size(self)
       self(n)%F_exp = self(n)%F_exp * fac
     end do

   end subroutine

   subroutine scale_F_sigma(self,fac)
    type(reflection_type), dimension(:) :: self
    ! Scale the structure factor errors
     intent(inout) :: self
     real(kind=kind(1.0d0)), intent(in) :: fac
      integer(kind=kind(1)) :: n

     do n=1,size(self)
       self(n)%F_sigma = self(n)%F_sigma * fac
     end do

   end subroutine

!*******************************************************************************
!                             Enquiry Routines
!*******************************************************************************

!   created result(res)
!   ! Returns true if self has been created
!     self :: pointer
!     res :: logical(kind=kind(.true.))
!     res = associated(self)
!   end

!   destroyed result(res)
!   ! Returns true if self has *not* been created
!     self :: pointer
!     res :: logical(kind=kind(.true.))
!     res = .not. associated(self)
!   end

   pure function n_refl(self) result(res)
    type(reflection_type), dimension(:) :: self
    ! The number of reflections
     intent(in) :: self
     integer(kind=kind(1)) :: res
     res = size(self)

   end function

   pure function have_F_calc(self) result(res)
    type(reflection_type), dimension(:) :: self
    ! Whether we have any calculated structure factors
     intent(in) :: self
     logical(kind=kind(.true.)) :: res
      integer(kind=kind(1)) :: n
     res = .false.
     do n=1,size(self)
       if (abs(self(n)%F_calc) > 10.0d0**(-10)) then
         res = .true.
         exit
       end if
     end do

   end function

   pure function have_F_pred(self) result(res)
    type(reflection_type), dimension(:) :: self
    ! Whether we have any predicted structure factors
     intent(in) :: self
     logical(kind=kind(.true.)) :: res
      integer(kind=kind(1)) :: n
     res = .false.
     do n=1,size(self)
       if (abs(self(n)%F_pred) > 10.0d0**(-10)) then
         res = .true.
         exit
       end if
     end do

   end function

   pure function have_F_exp(self) result(res)
    type(reflection_type), dimension(:) :: self
    ! Whether we have any experimental structure factors
     intent(in) :: self
     logical(kind=kind(.true.)) :: res
      integer(kind=kind(1)) :: n
     res = .false.
     do n=1,size(self)
       if (abs(self(n)%F_exp) > 10.0d0**(-10)) then
         res = .true.
         exit
       end if
     end do

   end function

   pure function have_F_sigma(self) result(res)
    type(reflection_type), dimension(:) :: self
    ! Whether we have any errors in the structure factors
     intent(in) :: self
     logical(kind=kind(.true.)) :: res
      integer(kind=kind(1)) :: n
     res = .false.
     do n=1,size(self)
       if (abs(self(n)%F_sigma) > 10.0d0**(-10)) then
         res = .true.
         exit
       end if
     end do

   end function

   pure function have_I_pred(self) result(res)
    type(reflection_type), dimension(:) :: self
    ! Whether we have any predicted intensities
     intent(in) :: self
     logical(kind=kind(.true.)) :: res
      integer(kind=kind(1)) :: n
     res = .false.
     do n=1,size(self)
       if (abs(self(n)%I_pred) > 10.0d0**(-10)) then
         res = .true.
         exit
       end if
     end do

   end function

   pure function have_I_exp(self) result(res)
    type(reflection_type), dimension(:) :: self
    ! Whether we have any experimental intensities
     intent(in) :: self
     logical(kind=kind(.true.)) :: res
      integer(kind=kind(1)) :: n
     res = .false.
     do n=1,size(self)
       if (abs(self(n)%I_exp) > 10.0d0**(-10)) then
         res = .true.
         exit
       end if
     end do

   end function

   pure function have_I_sigma(self) result(res)
    type(reflection_type), dimension(:) :: self
    ! Whether we have any errors in the intensities
     intent(in) :: self
     logical(kind=kind(.true.)) :: res
      integer(kind=kind(1)) :: n
     res = .false.
     do n=1,size(self)
       if (abs(self(n)%I_sigma) > 10.0d0**(-10)) then
         res = .true.
         exit
       end if
     end do

   end function

   pure function have_indices(self) result(res)
    type(reflection_type), dimension(:) :: self
    ! Whether we have the Miller indices
     intent(in) :: self
     logical(kind=kind(.true.)) :: res
      integer(kind=kind(1)) :: n
     res = .false.
     do n=1,size(self)
       if (self(n)%h /= 0 .or. self(n)%k /= 0 .or. self(n)%l /= 0) then
         res = .true.
         exit
       end if
     end do

   end function

   pure function indices(self,n) result(res)
    type(reflection_type), dimension(:) :: self
    ! Return the miller indices of reflection n.
     intent(in) :: self
      integer(kind=kind(1)), intent(in) :: n
     integer(kind=kind(1)), dimension(3) :: res
     res = (/ self(n)%h, self(n)%k, self(n)%l /)

   end function

!*******************************************************************************
!                              Statistical Routines
!*******************************************************************************

   pure function F_chi2(self) result(res)
    type(reflection_type), dimension(:) :: self
    ! chi2 for the structure factors
     intent(in) :: self
     real(kind=kind(1.0d0)) :: res
     real(kind=kind(1.0d0)) :: z
     integer(kind=kind(1)) :: n

     res = 0.0d0
     do n=1,size(self)
       z = F_z_(self(n))
       res = res + z*z
     end do
     res = res / max(n_refl_(self) - 1,1)

   end function

   pure function I_chi2(self) result(res)
    type(reflection_type), dimension(:) :: self
    ! chi2 for the intensities
     intent(in) :: self
     real(kind=kind(1.0d0)) :: res
     real(kind=kind(1.0d0)) :: z
     integer(kind=kind(1)) :: n
     res = 0.0d0

     do n=1,size(self)
       z = I_z_(self(n))
       res = res + z*z
     end do
     res = res / max(n_refl_(self) - 1,1)

   end function

   pure function F_goodness_of_fit(self) result(res)
    type(reflection_type), dimension(:) :: self
    ! goodness_of_fit for the structure factors
     intent(in) :: self
     real(kind=kind(1.0d0)) :: res
     res = sqrt(F_chi2_(self))

   end function

   pure function I_goodness_of_fit(self) result(res)
    type(reflection_type), dimension(:) :: self
    ! goodness_of_fit for the intensities
     intent(in) :: self
     real(kind=kind(1.0d0)) :: res
     res = sqrt(I_chi2_(self))

   end function

   pure function F_r_factor(self) result(res)
    type(reflection_type), dimension(:) :: self
    ! r factor for the structure factors
     intent(in) :: self
     real(kind=kind(1.0d0)) :: res
     real(kind=kind(1.0d0)) :: top,bot

     integer(kind=kind(1)) :: n
     top = 0.0d0
     bot = 0.0d0
     do n=1,size(self)
       top = top + abs(self(n)%F_pred - self(n)%F_exp)
       bot = bot + abs(self(n)%F_exp)
     end do
     res = top / bot

   end function

   pure function I_r_factor(self) result(res)
    type(reflection_type), dimension(:) :: self
    ! r factor for the intensities
     intent(in) :: self
     real(kind=kind(1.0d0)) :: res
     real(kind=kind(1.0d0)) :: top,bot
     integer(kind=kind(1)) :: n

     top = 0.0d0
     bot = 0.0d0
     do n=1,size(self)
       top = top + abs(self(n)%I_pred - self(n)%I_exp)
       bot = bot + abs(self(n)%I_exp)
     end do
     res = top / bot

   end function

   pure function F_weighted_r_factor(self) result(res)
    type(reflection_type), dimension(:) :: self
    ! weighted r factor for the structure factors
     intent(in) :: self
     real(kind=kind(1.0d0)) :: res
     real(kind=kind(1.0d0)) :: top,bot,z,b
     integer(kind=kind(1)) :: n

     top = 0.0d0
     bot = 0.0d0
     do n=1,size(self)
       z = F_z_(self(n))
       b = self(n)%F_exp / self(n)%F_sigma
       top = top + z*z
       bot = bot + b*b
     end do
     res = sqrt(top / bot)

   end function

   pure function I_weighted_r_factor(self) result(res)
    type(reflection_type), dimension(:) :: self
    ! weighted r factor for the intensities
     intent(in) :: self
     real(kind=kind(1.0d0)) :: res
     real(kind=kind(1.0d0)) :: top,bot,z,b
     integer(kind=kind(1)) :: n

     top = 0.0d0
     bot = 0.0d0
     do n=1,size(self)
       z = I_z_(self(n))
       b = self(n)%I_exp / self(n)%I_sigma
       top = top + z*z
       bot = bot + b*b
     end do
     res = sqrt(top / bot)

   end function

   pure function F_calc(self) result(res)
    type(reflection_type), dimension(:) :: self
    ! return the calculated structure factors
     intent(in) :: self
     complex(kind=kind((1.0d0,1.0d0))), dimension(size(self)) :: res
     res(:) = self(:)%F_calc

   end function

   pure function F_pred(self) result(res)
    type(reflection_type), dimension(:) :: self
    ! return the predicted structure factors
     intent(in) :: self
     real(kind=kind(1.0d0)), dimension(size(self)) :: res
     res(:) = self(:)%F_pred

   end function

   pure function F_exp(self) result(res)
    type(reflection_type), dimension(:) :: self
    ! return the experimental structure factors
     intent(in) :: self
     real(kind=kind(1.0d0)), dimension(size(self)) :: res
     res(:) = self(:)%F_exp

   end function

   pure function I_pred(self) result(res)
    type(reflection_type), dimension(:) :: self
    ! return the predicted intensities
     intent(in) :: self
     real(kind=kind(1.0d0)), dimension(size(self)) :: res
     res(:) = self(:)%I_pred

   end function

   pure function F_sigma(self) result(res)
    type(reflection_type), dimension(:) :: self
    ! return the errors in the experimental structure factors
     intent(in) :: self
     real(kind=kind(1.0d0)), dimension(size(self)) :: res
     res(:) = self(:)%F_sigma

   end function

! ***********************
! List-based I/O Routines
! ***********************

   recursive subroutine read_list_keywords(self)
    type(reflection_type), dimension(:) :: self
    ! Read in and process list-based keywords from "stdin". List-based keywords
    ! are those that are intended to apply to each individual element of the list
    ! through a list of "keys" stored in the associated list-element type module.
    ! NOTE: this routine will create the list, if required.
     pointer :: self
    ! The following code is inherited from OBJECTVEC
     character(128) :: word

     call ensure_(tonto,next_item_(stdin)=="{","REFLECTIONVEC:read_list_keywords ... expecting open bracket symbol, {")
     call read_(stdin,word)
     do                   ! Loop over input list-type keywords
       call read_(stdin,word)
       if (word=="}")      exit
       if (reverted_(stdin)) exit
       call process_list_keyword_(self,word)
     end do

   end subroutine

   subroutine process_list_keyword(self,keyword)
    type(reflection_type), dimension(:) :: self
    ! Process a list-type "keyword", common to all list-type objects.
     pointer :: self
     character(*), intent(in) :: keyword
    ! The following code is inherited from OBJECTVEC
     character(128) :: word
     logical(kind=kind(.true.)) :: ignore_braces

     word = keyword
     call to_lower_case_(word)
     select case (word)
       case("altered_data= "); call read_altered_data_(self)
       case("append_data=  "); call read_append_data_(self)
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
    type(reflection_type), dimension(:) :: self
    ! Process the keywords list to read data or commands. If "ignore_braces" is
    ! present then the opening and closing braces, which are normally required,
    ! are ignored.
     pointer :: self
     logical(kind=kind(.true.)), optional :: ignore_braces
    ! The following code is inherited from OBJECTVEC
     character(128) :: word,message
     integer(kind=kind(1)) :: length

     if (.not. present(ignore_braces)) then
        call ensure_(tonto,next_item_(stdin)=="{","REFLECTIONVEC:read_data ... expecting open bracket symbol, {")
        call read_(stdin,word)  ! move past open brace
     end if
     length = data_length_(self)
     if (associated(self)) then
        message = "No. of data items in new and old data lists do not match: " &
                  // "new = "//trim(to_str_(length))//", old = "//trim(to_str_(size(self)))
        call ensure_(tonto,length==size(self),message)
     else
        call create_(self,length)
     end if
     call process_keys_(self)
     if (.not. present(ignore_braces)) then
        call read_(stdin,word)  ! read last brace
        call ensure_(tonto,word=="}","REFLECTIONVEC:read_data ... expecting close bracket symbol, }")
     end if

   end subroutine

   function data_length(self) result(length)
    type(reflection_type), dimension(:) :: self
    ! Read ahead in stdin to get the "length" of the data list, i.e. the number
    ! of data items in the list. The data must begin with the first data item,
    ! *not* a "{" symbol.  The order of data items comprising the list is given
    ! by keys defined in the associated list-element type module. The data list
    ! must be terminated by a "}" symbol.
     pointer :: self
     integer(kind=kind(1)) :: length
    ! The following code is inherited from OBJECTVEC
     type(reflection_type), pointer :: tmp
     character(128) :: word
     integer(kind=kind(1)) :: line,item

     call ensure_(tonto,next_item_(stdin)/="}","REFLECTIONVEC:data_length ... empty data list!")
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
    type(reflection_type), dimension(:) :: self
    ! Read in a sublist of the complete list, and alter the data for that
    ! sublist.  The order of the data items in the sublist is given by the "keys"
    ! defined in the associated list-element type module.
     pointer :: self
    ! The following code is inherited from OBJECTVEC
     character(128) :: word
     integer(kind=kind(1)) :: s

     call ensure_(tonto,associated(self),"REFLECTIONVEC:read_altered_data ... list does not exist yet")
     call ensure_(tonto,next_item_(stdin)=="{","REFLECTIONVEC:read_altered_data ... expecting open bracket symbol: {")
     call read_(stdin,word)
     read_loop: do
        call read_(stdin,word)
        if (word=="}") exit read_loop
        call ensure_(tonto,is_int_(word),"REFLECTIONVEC:read_altered_data ... expecting integer list-element index")
        s = to_int_(word)
        call ensure_(tonto,s<=size(self),"REFLECTIONVEC:read_altered_data ... list-element too large")
        call ensure_(tonto,s>0,"REFLECTIONVEC:read_altered_data ... list-element must be positive")
        call process_keys_(self(s))
     end do read_loop

   end subroutine

   subroutine read_append_data(self)
    type(reflection_type), dimension(:) :: self
    ! Read in a set of data to append to an existing set.
     pointer :: self
    ! The following code is inherited from OBJECTVEC

   call ensure_(tonto,associated(self),"REFLECTIONVEC:read_append_data ... list does not exist yet")
   call ensure_(tonto,next_item_(stdin)=="{","REFLECTIONVEC:read_append_data ... expecting open bracket symbol: {")
     nullify(saved_self)
     call read_data_(saved_self)
     call append_(self,saved_self)
     call destroy_(saved_self)

   end subroutine

   subroutine process_keys(self)
    type(reflection_type), dimension(:) :: self
    ! Process the "keys" on each element of the list.
     pointer :: self
    ! The following code is inherited from OBJECTVEC
     type(reflection_type) :: tmp
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
    type(reflection_type), dimension(:) :: self
    ! Return .true. if the list-element keys are created.
      pointer :: self
      logical(kind=kind(.true.)) :: res
    ! The following code is inherited from OBJECTVEC
      type(reflection_type) :: tmp

      res = keys_created_(tmp)

   end function

   subroutine set_keys(self,the_keys)
    type(reflection_type), dimension(:) :: self
    ! This is for setting the "keys" externally.
     pointer :: self
     character(len=*), dimension(:) :: the_keys
    ! The following code is inherited from OBJECTVEC
     type(reflection_type) :: tmp

     call set_keys_(tmp,the_keys)

   end subroutine

   subroutine clear_keys(self)
    type(reflection_type), dimension(:) :: self
    ! This is for destroying the "keys" externally.
     pointer :: self
    ! The following code is inherited from OBJECTVEC
     type(reflection_type) :: tmp

     call clear_keys_(tmp)

   end subroutine

   subroutine read_keys(self)
    type(reflection_type), dimension(:) :: self
    ! Read a new set of keys
      pointer :: self
    ! The following code is inherited from OBJECTVEC
      type(reflection_type) :: tmp

      call read_keys_(tmp)

   end subroutine

   subroutine put_keys_table(self)
    type(reflection_type), dimension(:) :: self
    ! Output a generic table based on the "keys"
     pointer :: self
    ! The following code is inherited from OBJECTVEC

     call ensure_(tonto,keys_created_(self),"REFLECTIONVEC:put_keys_table ... no keys")
     call put_table_header_(self)
     call process_keys_(self)
     call put_table_footer_(self)

   end subroutine

   subroutine put_table_header(self)
    type(reflection_type), dimension(:) :: self
    ! Put out a table header based on "keys"
      pointer :: self
    ! The following code is inherited from OBJECTVEC
      type(reflection_type) :: tmp

      call put_table_header_(tmp)

   end subroutine

   subroutine put_table_footer(self)
    type(reflection_type), dimension(:) :: self
    ! Put out a table footer based on "keys"
      pointer :: self
    ! The following code is inherited from OBJECTVEC
      type(reflection_type) :: tmp

      call put_table_footer_(tmp)

   end subroutine

   subroutine redirect(self)
    type(reflection_type), dimension(:) :: self
    ! Redirect input
     pointer :: self
    ! The following code is inherited from OBJECT

     call redirect_(stdin,next_str_(stdin))

   end subroutine

   subroutine revert(self)
    type(reflection_type), dimension(:) :: self
    ! Revert back to previous stdin file
     pointer :: self
    ! The following code is inherited from OBJECT

     call revert_(stdin)

   end subroutine

! ***************************
! Non-list based I/O routines
! ***************************

   subroutine read_keywords(self)
    type(reflection_type), dimension(:) :: self
    ! Read in and process normal (non list-type) keywords from "stdin".
     pointer :: self
     character(128) :: word

     call ensure_(tonto,next_item_(stdin)=="{","REFLECTIONVEC:read_keywords ... expecting open bracket symbol, {")
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
    type(reflection_type), dimension(:) :: self
    ! Process a normal (non list-type) "keyword".
     pointer :: self
     character(128) :: keyword
     character(128) :: word

     word = keyword
     call to_lower_case_(word)
     select case (word)
       case("}")  ! do nothing.
       case("add_random_error         "); call add_random_error_(self)
       case("put                      "); call put_(self)
       case("put_f_exp_data           "); call put_F_exp_data_(self)
       case("put_f_pred_data          "); call put_F_pred_data_(self)
       case("put_f_stats              "); call put_F_stats_(self)
       case("put_i_exp_data           "); call put_I_exp_data_(self)
       case("put_i_stats              "); call put_I_stats_(self)
       case("put_intensity_data       "); call put_intensity_data_(self)
       case("put_structure_factor_data"); call put_structure_factor_data_(self)
       case("redirect                 "); call redirect_(self)
       case("revert                   "); call revert_(self)
       case("simulate_new_f_exp       "); call simulate_new_F_exp_(self)
       case default;     allocate(tonto%known_keywords(13))
       tonto%known_keywords(1) = "}"
       tonto%known_keywords(2) = "add_random_error         "
       tonto%known_keywords(3) = "put                      "
       tonto%known_keywords(4) = "put_f_exp_data           "
       tonto%known_keywords(5) = "put_f_pred_data          "
       tonto%known_keywords(6) = "put_f_stats              "
       tonto%known_keywords(7) = "put_i_exp_data           "
       tonto%known_keywords(8) = "put_i_stats              "
       tonto%known_keywords(9) = "put_intensity_data       "
       tonto%known_keywords(10) = "put_structure_factor_data"
       tonto%known_keywords(11) = "redirect                 "
       tonto%known_keywords(12) = "revert                   "
       tonto%known_keywords(13) = "simulate_new_f_exp       "
       call unknown_(tonto,word,"REFLECTIONVEC:process_keyword")
       deallocate(tonto%known_keywords)
     end select

   end subroutine

   subroutine shrink(self,dim)
    type(reflection_type), dimension(:) :: self
    ! Shrink self to dimension dim.  Contents are retained.
     pointer :: self
     integer(kind=kind(1)), intent(in) :: dim
    ! The following code is inherited from OBJECTVEC
     type(reflection_type), dimension(:), pointer :: old
     integer(kind=kind(1)) :: n

     call ensure_(tonto,associated(self),"REFLECTIONVEC:shrink ... no self array")
     call ensure_(tonto,dim<=size(self),"REFLECTIONVEC:shrink ... dim too large")
     call ensure_(tonto,dim>=0,"REFLECTIONVEC:shrink ... dim must be non-negative")
     if (dim==size(self)) then;   return; end if
     old => self
     nullify(self)
     call create_(self,dim)
     do n=1,dim
       call copy_(self(n),old(n))
     end do
     call destroy_(old)

   end subroutine

   subroutine expand(self,dim)
    type(reflection_type), dimension(:) :: self
    ! Expand the vector "self" to "dim". New slots are left undefined.
     pointer :: self
     integer(kind=kind(1)), intent(in) :: dim
    ! The following code is inherited from OBJECTVEC
     type(reflection_type), dimension(:), pointer :: old
     integer(kind=kind(1)) :: old_dim

     if (.not. associated(self)) then
        call create_(self,dim)
     else
        call ensure_(tonto,dim>=size(self),"REFLECTIONVEC:expand ... dim not large enough")
        call ensure_(tonto,dim>=0,"REFLECTIONVEC:expand ... dim must be non-negative")
        old => self
        old_dim = size(old)
        nullify(self)
        call create_(self,dim)
        call copy_(self(1:old_dim),old)
        call destroy_(old)
     end if

   end subroutine

   subroutine append(self,v)
    type(reflection_type), dimension(:) :: self
    ! Expands self and appends the contents of vector "v".
     pointer :: self
     type(reflection_type), dimension(:), intent(in) :: v
    ! The following code is inherited from OBJECTVEC
     integer(kind=kind(1)) :: dim

     if (.not. associated(self)) then; dim = 0
     else;                 dim = size(self)
     end if
     call expand_(self,dim+size(v))
     call copy_(self(dim+1:),v)

   end subroutine

   subroutine append_1(self,value)
    type(reflection_type), dimension(:) :: self
    ! Expands self by 1, and appends the single scalar "value" onto the end.
     pointer :: self
     type(reflection_type), intent(in) :: value
    ! The following code is inherited from OBJECTVEC
     integer(kind=kind(1)) :: dim

     if (.not. associated(self)) then; dim = 0
     else;                 dim = size(self)
     end if
     call expand_(self,dim+1)
     call copy_(self(dim+1),value)

   end subroutine

!*******************************************************************************
!                                  Output routines
!*******************************************************************************

   subroutine put(self)
    type(reflection_type), dimension(:) :: self
    ! Output the reflection data.
     pointer :: self

     call flush_(stdout)
     call text_(stdout,"Reflection data:")
     call flush_(stdout)
     call show_(stdout,"Number of reflections is ",size(self))
     call flush_(stdout)
     if (have_F_pred_(self)) then
       if (have_F_exp_(self)) then
         call put_structure_factor_data_(self)
       else
         call put_F_pred_data_(self)
       end if
     else
       if (have_F_exp_(self)) call put_F_exp_data_(self)
     end if
     if (have_I_pred_(self)) then
       if (have_I_exp_(self)) then
         call put_intensity_data_(self)
       else
         call put_I_pred_data_(self)
       end if
     else
       if (have_I_exp_(self)) call put_I_exp_data_(self)
     end if

   end subroutine

   subroutine put_structure_factor_data(self)
    type(reflection_type), dimension(:) :: self
    ! Output the structure factor data
     pointer :: self

     call put_F_stats_(self)
     call set_keys_(self,(/"put_indices", &
                "put_F_calc ", &
                "put_F_pred ", &
                "put_F_exp  ", &
                "put_F_sigma", &
                "flush      "/) )
     call put_keys_table_(self)

   end subroutine

   subroutine put_F_pred_data(self)
    type(reflection_type), dimension(:) :: self
    ! Output only the predicted structure factor data
     pointer :: self

     call set_keys_(self,(/"put_indices", &
                "put_F_calc ", &
                "put_F_pred ", &
                "flush      "/) )
     call put_keys_table_(self)

   end subroutine

   subroutine put_F_exp_data(self)
    type(reflection_type), dimension(:) :: self
    ! Output only the experimental structure factor data
     pointer :: self

     call set_keys_(self,(/"put_indices", &
                "put_F_exp  ", &
                "put_F_sigma", &
                "flush      "/) )
     call put_keys_table_(self)

   end subroutine

   subroutine put_intensity_data(self)
    type(reflection_type), dimension(:) :: self
    ! Output the structure factor data
     pointer :: self

     call put_I_stats_(self)
     call set_keys_(self,(/"put_indices", &
                "put_I_pred ", &
                "put_I_exp  ", &
                "put_I_sigma", &
                "flush      "/) )
     call put_keys_table_(self)

   end subroutine

   subroutine put_I_pred_data(self)
    type(reflection_type), dimension(:) :: self
    ! Output only the predicted structure factor data
     pointer :: self

     call set_keys_(self,(/"put_indices", &
                "put_I_calc ", &
                "put_I_pred ", &
                "flush      "/) )
     call put_keys_table_(self)

   end subroutine

   subroutine put_I_exp_data(self)
    type(reflection_type), dimension(:) :: self
    ! Output only the experimental intensity data
     pointer :: self

     call set_keys_(self,(/"put_indices", &
                "put_I_exp  ", &
                "put_I_sigma", &
                "flush      "/) )
     call put_keys_table_(self)

   end subroutine

   subroutine put_F_stats(self)
    type(reflection_type), dimension(:) :: self
    ! Output the structure factor goodness of fit statistics
     real(kind=kind(1.0d0)) :: chi2

     call flush_(stdout)
     if (.not. have_F_pred_(self)) &
     call text_(stdout,"Note that the F_pred are all set to zero!")
     chi2 = F_chi2_(self)
     call text_(stdout,"Goodness of fit parameters based on Structure Factors:",flush=2)
     call show_(stdout,"R factor                    =", F_r_factor_(self))
     call show_(stdout,"Weighted R factor           =", F_weighted_r_factor_(self))
     call show_(stdout,"chi**2                      =", chi2)
     call show_(stdout,"Goodness of fit             =", sqrt(chi2))

   end subroutine

   subroutine put_I_stats(self)
    type(reflection_type), dimension(:) :: self
    ! Output the intensity goodness of fit statistics
     real(kind=kind(1.0d0)) :: chi2

     chi2 = I_chi2_(self)
     call flush_(stdout)
     call text_(stdout,"Goodness of fit parameters based on Intensities:",flush=2)
     call show_(stdout,"R factor                    =", I_r_factor_(self))
     call show_(stdout,"Weighted R factor           =", I_weighted_r_factor_(self))
     call show_(stdout,"chi**2                      =", chi2)
     call show_(stdout,"Goodness of fit             =", sqrt(chi2))

   end subroutine

!*******************************************************************************
!                                    QQ plots
!*******************************************************************************

   subroutine put_F_qq_plot(self,name)
    type(reflection_type), dimension(:) :: self
    ! Output a qq plot to the text file.
    ! It is a plot of the experimental quantile vs expected quantile.
     character(128), optional :: name
     type(archive_type) :: arch
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: grid

     call create_(grid,n_refl_(self),2)
     call make_F_qq_plot_grid_(self,grid)
     call set_(arch,root_name=name,name="qq_plot",format="ascii")
     call write_(arch,grid,order="by_row")
     call close_(arch)
     call destroy_(grid)

   end subroutine

   subroutine put_labelled_F_qq_plot(self,name)
    type(reflection_type), dimension(:) :: self
    ! Output a qq plot to the text file.
    ! It is a plot of the experimental quantile vs expected quantile.
     character(128), optional :: name
     type(textfile_type), pointer :: tf
     real(kind=kind(1.0d0)), dimension(:,:), pointer :: grid
     integer(kind=kind(1)), dimension(:,:), pointer :: hkl
      integer(kind=kind(1)) :: n

     call create_(hkl,n_refl_(self),3)
     do n=1,n_refl_(self)
       hkl(n,:) = indices_(self,n)
     end do
     call create_(grid,n_refl_(self),2)
     call make_F_qq_plot_grid_(self,grid,hkl)
     call create_(tf,trim(name) // ":qq_plot_labelled")
     call open_(tf,for="write")
     call set_use_labels_(tf,.false.)
     do n=1,n_refl_(self)
       call put_(tf,grid(n,1))
       call put_(tf,grid(n,2))
       call put_(tf,hkl(n,1))
       call put_(tf,hkl(n,2))
       call put_(tf,hkl(n,3))
       call flush_(tf)
     end do
     call close_(tf)
     call destroy_(tf)
     call destroy_(grid)
     call destroy_(hkl)

   end subroutine

   subroutine make_F_qq_plot_grid(self,grid,hkl)
    type(reflection_type), dimension(:) :: self
    ! Make the grid for the Q-Q plot, which is a plot of the deviations X-Y
    ! versus the expected deviations, assuming that the expected devaitions
    ! are normally distributed. grid(1,:) contains the expected deviation d0j,
    ! grid(2,:) contains actual deviation dj.
     target :: self
     real(kind=kind(1.0d0)), dimension(:,:), target :: grid
     integer(kind=kind(1)), dimension(:,:), optional :: hkl
     real(kind=kind(1.0d0)), dimension(:), pointer :: d,e
     type(reflection_type), pointer :: ref
     real(kind=kind(1.0d0)) :: p
     integer(kind=kind(1)) :: n_refl,i,j

     n_refl = size(self)
     call ensure_(tonto,size(grid,1)==n_refl,"REFLECTIONVEC:make_F_qq_plot_grid ... grid wrong size")
     call ensure_(tonto,size(grid,2)==2,"REFLECTIONVEC:make_F_qq_plot_grid ... grid wrong size")

     e => grid(:,1)  ! theoretical z's
     d => grid(:,2)  ! calculated z's

     do i=1,n_refl
       ref => self(i)
       d(i)  = F_z_(ref) * sign(1.0d0,real(ref%F_calc))
     end do

     ! sort array from lowest z to highest
     if (present(hkl)) then
       call ensure_(tonto,size(hkl,1)==n_refl,"REFLECTIONVEC:make_F_qq_plot_grid ... size of index array incorrect")
       call ensure_(tonto,size(hkl,2)==3,"REFLECTIONVEC:make_F_qq_plot_grid ... size of index array incorrect")
       do i=1,n_refl-1
         do j=i+1,n_refl
           if (d(j) < d(i)) then
             call swap_elements_(d,i,j)
             call swap_columns_(hkl,i,j)
           end if
         end do
       end do
     else
       call sort_(d)
     end if

     do j=1,n_refl                     ! expected quantile, d0j
       p = (2.0d0*(n_refl-j)+1.0d0)/(2.0d0*n_refl)  ! cumulative probability
       e(n_refl-j+1) = z_from_p_(p)
     end do

   end subroutine

   subroutine simulate_new_F_exp(self)
    type(reflection_type), dimension(:) :: self
    ! Set the reflection data to be the experimental reflection data plus an
    ! error.  The errors are normally distributed, with the experimental errors
    ! being their standard deviations.
     integer(kind=kind(1)) :: n
     real(kind=kind(1.0d0)) :: dF

     call flush_(stdout)
     call text_(stdout,"*************************************************")
     call text_(stdout,"adding normally distributed random error to F_exp")
     call text_(stdout,"*************************************************")
     do n=1,n_refl_(self)
       call to_random_normal_(dF)  ! dF has mean zero, standard deviation one
       self(n)%F_exp = self(n)%F_exp + dF*self(n)%F_sigma
     end do

   end subroutine

   subroutine add_random_error(self)
    type(reflection_type), dimension(:) :: self
    ! Set the reflection data to be the experimental reflection data plus an
    ! error.  The errors are normally distributed, with the experimental errors
    ! being their standard deviations multiplied by a scale factor which is read
    ! from stdin.
     integer(kind=kind(1)) :: n
     real(kind=kind(1.0d0)) :: dF,scale

     call flush_(stdout)
     call text_(stdout,"*************************************************")
     call text_(stdout,"adding normally distributed random error to F_exp")
     call text_(stdout,"*************************************************")
     call read_(stdin,scale)
     call show_(stdout,"normal distribution scaled by ",scale)
     do n=1,n_refl_(self)
       call to_random_normal_(dF)  ! dF has mean zero, standard deviation one
       dF=dF*scale
       self(n)%F_exp = self(n)%F_exp + dF*self(n)%F_sigma
     end do

   end subroutine

end
