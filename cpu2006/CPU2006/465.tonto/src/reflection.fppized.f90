!-------------------------------------------------------------------------------
!
! REFLECTION: Reflection data for crystals
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
! $Id: reflection.foo,v 1.3.2.2 2003/11/13 05:36:07 reaper Exp $
!-------------------------------------------------------------------------------

module REFLECTION_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

   use STR_MODULE, only: split_
   use STR_MODULE, only: to_lower_case_
   use STR_MODULE, only: includes_

   use TEXTFILE_MODULE, only: stdin
   use TEXTFILE_MODULE, only: stdout
   use TEXTFILE_MODULE, only: set_default_units_
   use TEXTFILE_MODULE, only: next_str_
   use TEXTFILE_MODULE, only: skip_next_item_
   use TEXTFILE_MODULE, only: redirect_
   use TEXTFILE_MODULE, only: next_item_
   use TEXTFILE_MODULE, only: revert_
   use TEXTFILE_MODULE, only: put_
   use TEXTFILE_MODULE, only: read_
   use TEXTFILE_MODULE, only: flush_
   use TEXTFILE_MODULE, only: read_ptr_
   use TEXTFILE_MODULE, only: reverted_
   use TEXTFILE_MODULE, only: dash_
   use TEXTFILE_MODULE, only: read_int_

   use STRVEC_MODULE, only: create_copy_
   use STRVEC_MODULE, only: destroy_
   use STRVEC_MODULE, only: index_of_matching_bracket_

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

   public    add_F_exp_
   interface add_F_exp_
      module procedure add_F_exp
   end interface

   public    F_z2_
   interface F_z2_
      module procedure F_z2
   end interface

   public    read_F_exp_
   interface read_F_exp_
      module procedure read_F_exp
   end interface

   public    add_F_pred_
   interface add_F_pred_
      module procedure add_F_pred
   end interface

   public    put_table_header_
   interface put_table_header_
      module procedure put_table_header
   end interface

   public    table_width_
   interface table_width_
      module procedure table_width
   end interface

   public    read_keywords_
   interface read_keywords_
      module procedure read_keywords
   end interface

   public    add_I_exp_
   interface add_I_exp_
      module procedure add_I_exp
   end interface

   public    read_I_exp_
   interface read_I_exp_
      module procedure read_I_exp
   end interface

   public    F_phase_
   interface F_phase_
      module procedure F_phase
   end interface

   public    process_keyword_
   interface process_keyword_
      module procedure process_keyword
   end interface

   public    process_keys_
   interface process_keys_
      module procedure process_keys
   end interface

   public    put_table_footer_
   interface put_table_footer_
      module procedure put_table_footer
   end interface

   public    I_z_
   interface I_z_
      module procedure I_z
   end interface

   public    add_I_sigma_
   interface add_I_sigma_
      module procedure add_I_sigma
   end interface

   public    copy_
   interface copy_
      module procedure copy
   end interface

   public    set_defaults_
   interface set_defaults_
      module procedure set_defaults
   end interface

   public    clear_keys_
   interface clear_keys_
      module procedure clear_keys
   end interface

   public    create_
   interface create_
      module procedure create
   end interface

   public    read_F_calc_
   interface read_F_calc_
      module procedure read_F_calc
   end interface

   public    create_copy_
   interface create_copy_
      module procedure create_copy
   end interface

   public    read_I_sigma_
   interface read_I_sigma_
      module procedure read_I_sigma
   end interface

   public    add_F_sigma_
   interface add_F_sigma_
      module procedure add_F_sigma
   end interface

   public    keys_created_
   interface keys_created_
      module procedure keys_created
   end interface

   public    set_keys_
   interface set_keys_
      module procedure set_keys
   end interface

   public    F_r_
   interface F_r_
      module procedure F_r
   end interface

   public    read_indices_
   interface read_indices_
      module procedure read_indices
   end interface

   public    read_h_
   interface read_h_
      module procedure read_h
   end interface

   public    destroy_
   interface destroy_
      module procedure destroy
   end interface

   public    add_F_calc_
   interface add_F_calc_
      module procedure add_F_calc
   end interface

   public    read_junk_
   interface read_junk_
      module procedure read_junk
   end interface

   public    read_k_
   interface read_k_
      module procedure read_k
   end interface

   public    read_F_sigma_
   interface read_F_sigma_
      module procedure read_F_sigma
   end interface

   public    read_I_pred_
   interface read_I_pred_
      module procedure read_I_pred
   end interface

   public    F_z_
   interface F_z_
      module procedure F_z
   end interface

   public    read_l_
   interface read_l_
      module procedure read_l
   end interface

   public    read_units_
   interface read_units_
      module procedure read_units
   end interface

   public    add_I_pred_
   interface add_I_pred_
      module procedure add_I_pred
   end interface

   public    read_F_pred_
   interface read_F_pred_
      module procedure read_F_pred
   end interface

   character(128), dimension(:), pointer, private :: keys => NULL()

contains

!*******************************************************************************
!                        Create and Destroy Routines
!*******************************************************************************

   subroutine create(self)
    type(reflection_type) :: self
    ! Create the object
      pointer :: self

      nullify(self)
      allocate(self)

      call set_defaults_(self)

   end subroutine

   subroutine destroy(self)
    type(reflection_type) :: self
    ! Destroy the object
      pointer :: self

      if (.not. associated(self)) then;   return; end if

      deallocate(self)

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

   subroutine create_copy(self,r)
    type(reflection_type) :: self
    ! Create a copy of the reflection "r"
      pointer :: self
      type(reflection_type) :: r

      call create_(self)
      call copy_(self,r)

   end subroutine

   subroutine copy(self,r)
    type(reflection_type) :: self
    ! Make a copy of the reflection "r"
      type(reflection_type) :: r

      self = r

   end subroutine

   subroutine set_defaults(self)
    type(reflection_type) :: self
    ! Set up a default object

     self%h = 0
     self%k = 0
     self%l = 0
     self%F_exp = 0.0d0
     self%F_calc = 0.0d0
     self%F_pred = 0.0d0
     self%F_sigma = 0.0d0
     self%I_exp = 0.0d0
     self%I_pred = 0.0d0
     self%I_sigma = 0.0d0

   end subroutine

!  ************
!  I/O Routines
!  ************

   recursive subroutine read_keywords(self)
    type(reflection_type) :: self
    ! Read data from "stdin" using keyword style input.
    ! The following code is inherited from OBJECT
     character(128) :: word

     call ensure_(tonto,next_item_(stdin)=="{","REFLECTION:read_keywords ... expecting open bracket symbol, {")
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
    type(reflection_type) :: self
    ! Process command "keyword". Any required data needed by the "keyword" is
    ! inputted from "stdin".
     character(*), intent(in) :: keyword
     character(128) :: word

     word = keyword
     call to_lower_case_(word)
     select case (word)
       case ("}           ");  ! exit surrounding loop
       case ("add_f_calc= "); call add_F_calc_(self)
       case ("add_f_exp=  "); call add_F_exp_(self)
       case ("add_f_pred= "); call add_F_pred_(self)
       case ("add_f_sigma="); call add_F_sigma_(self)
       case ("add_i_exp=  "); call add_I_exp_(self)
       case ("add_i_pred= "); call add_I_pred_(self)
       case ("add_i_sigma="); call add_I_sigma_(self)
       case ("f_calc=     "); call read_F_calc_(self)
       case ("f_exp=      "); call read_F_exp_(self)
       case ("f_pred=     "); call read_F_pred_(self)
       case ("f_sigma=    "); call read_F_sigma_(self)
       case ("h=          "); call read_h_(self)
       case ("indices=    "); call read_indices_(self)
       case ("i_exp=      "); call read_I_exp_(self)
       case ("i_pred=     "); call read_I_pred_(self)
       case ("i_sigma=    "); call read_I_sigma_(self)
       case ("junk=       "); call read_junk_(self)
       case ("k=          "); call read_k_(self)
       case ("l=          "); call read_l_(self)
       case ("units=      "); call read_units_(self)
        ! These are only for making custom tables for the list type
       case ("flush       "); call flush_(stdout)
       case ("put_f_calc  "); call put_(stdout,self%F_calc)
       case ("put_f_exp   "); call put_(stdout,self%F_exp)
       case ("put_f_phase "); call put_(stdout,F_phase_(self))
       case ("put_f_pred  "); call put_(stdout,self%F_pred)
       case ("put_f_sigma "); call put_(stdout,self%F_sigma)
       case ("put_f_z     "); call put_(stdout,F_z_(self))
       case ("put_f_z2    "); call put_(stdout,F_z2_(self))
       case ("put_h       "); call put_(stdout,self%h)
       case ("put_i_exp   "); call put_(stdout,self%I_exp)
       case ("put_i_pred  "); call put_(stdout,self%I_pred)
       case ("put_i_sigma "); call put_(stdout,self%I_sigma)
       case ("put_i_z     "); call put_(stdout,I_z_(self))
       case ("put_indices "); call put_(stdout,self%h); call put_(stdout,self%k); call put_(stdout,self%l)
       case ("put_k       "); call put_(stdout,self%k)
       case ("put_l       "); call put_(stdout,self%l)
       case default;         allocate(tonto%known_keywords(37))
       tonto%known_keywords(1) = "}           "
       tonto%known_keywords(2) = "add_f_calc= "
       tonto%known_keywords(3) = "add_f_exp=  "
       tonto%known_keywords(4) = "add_f_pred= "
       tonto%known_keywords(5) = "add_f_sigma="
       tonto%known_keywords(6) = "add_i_exp=  "
       tonto%known_keywords(7) = "add_i_pred= "
       tonto%known_keywords(8) = "add_i_sigma="
       tonto%known_keywords(9) = "f_calc=     "
       tonto%known_keywords(10) = "f_exp=      "
       tonto%known_keywords(11) = "f_pred=     "
       tonto%known_keywords(12) = "f_sigma=    "
       tonto%known_keywords(13) = "h=          "
       tonto%known_keywords(14) = "indices=    "
       tonto%known_keywords(15) = "i_exp=      "
       tonto%known_keywords(16) = "i_pred=     "
       tonto%known_keywords(17) = "i_sigma=    "
       tonto%known_keywords(18) = "junk=       "
       tonto%known_keywords(19) = "k=          "
       tonto%known_keywords(20) = "l=          "
       tonto%known_keywords(21) = "units=      "
       tonto%known_keywords(22) = "flush       "
       tonto%known_keywords(23) = "put_f_calc  "
       tonto%known_keywords(24) = "put_f_exp   "
       tonto%known_keywords(25) = "put_f_phase "
       tonto%known_keywords(26) = "put_f_pred  "
       tonto%known_keywords(27) = "put_f_sigma "
       tonto%known_keywords(28) = "put_f_z     "
       tonto%known_keywords(29) = "put_f_z2    "
       tonto%known_keywords(30) = "put_h       "
       tonto%known_keywords(31) = "put_i_exp   "
       tonto%known_keywords(32) = "put_i_pred  "
       tonto%known_keywords(33) = "put_i_sigma "
       tonto%known_keywords(34) = "put_i_z     "
       tonto%known_keywords(35) = "put_indices "
       tonto%known_keywords(36) = "put_k       "
       tonto%known_keywords(37) = "put_l       "
       call unknown_(tonto,word,"REFLECTION:process_keyword")
       deallocate(tonto%known_keywords)
     end select

   end subroutine

   subroutine read_units(self)
    type(reflection_type) :: self
    ! Read a string which describes the units to be used
    ! The following code is inherited from OBJECT

      call set_default_units_(stdin,next_str_(stdin))

   end subroutine

   subroutine read_junk(self)
    type(reflection_type) :: self
    ! Read in a junk string, useful for ignoring a field
    ! The following code is inherited from OBJECT

      call skip_next_item_(stdin)

   end subroutine

   subroutine read_h(self)
    type(reflection_type) :: self
    ! Read in the "h" Miller index

      call read_int_(stdin,self%h)

   end subroutine

   subroutine read_k(self)
    type(reflection_type) :: self
    ! Read in the "k" Miller index

      call read_int_(stdin,self%k)

   end subroutine

   subroutine read_l(self)
    type(reflection_type) :: self
    ! Read in the "l" Miller index

      call read_int_(stdin,self%l)

   end subroutine

   subroutine read_indices(self)
    type(reflection_type) :: self
    ! Read in the h k l Miller indices as a triple

      call read_int_(stdin,self%h)
      call read_int_(stdin,self%k)
      call read_int_(stdin,self%l)

   end subroutine

   subroutine read_F_exp(self)
    type(reflection_type) :: self
    ! Read in the experimentally determined structure factor

      call read_(stdin,self%F_exp)

   end subroutine

   subroutine add_F_exp(self)
    type(reflection_type) :: self
    ! Read in an increment to add to .F_exp
      real(kind=kind(1.0d0)) :: tmp

      call read_(stdin,tmp)
      self%F_exp = self%F_exp + tmp

   end subroutine

   subroutine read_F_calc(self)
    type(reflection_type) :: self
    ! Read in a calculated (complex) structure factor

      call read_(stdin,self%F_calc)

   end subroutine

   subroutine add_F_calc(self)
    type(reflection_type) :: self
    ! Read in a *complex* increment to add to .F_calc
      complex(kind=kind((1.0d0,1.0d0))) :: tmp

      call read_(stdin,tmp)
      self%F_calc = self%F_calc + tmp

   end subroutine

   subroutine read_F_pred(self)
    type(reflection_type) :: self
    ! Read in a predicted (real) structure factor

      call read_(stdin,self%F_pred)

   end subroutine

   subroutine add_F_pred(self)
    type(reflection_type) :: self
    ! Read in an increment to add to .F_pred
      real(kind=kind(1.0d0)) :: tmp

      call read_(stdin,tmp)
      self%F_pred = self%F_pred + tmp

   end subroutine

   subroutine read_F_sigma(self)
    type(reflection_type) :: self
    ! Read in the experimentally determined standard devaiation in
    ! the structure factor

      call read_(stdin,self%F_sigma)

   end subroutine

   subroutine add_F_sigma(self)
    type(reflection_type) :: self
    ! Read in an increment to add to .F_sigma
      real(kind=kind(1.0d0)) :: tmp

      call read_(stdin,tmp)
      self%F_sigma = self%F_sigma + tmp

   end subroutine

   subroutine read_I_exp(self)
    type(reflection_type) :: self
    ! Read in the experimentally determined intensity

      call read_(stdin,self%I_exp)

   end subroutine

   subroutine add_I_exp(self)
    type(reflection_type) :: self
    ! Read in an increment to add to .I_exp
      real(kind=kind(1.0d0)) :: tmp

      call read_(stdin,tmp)
      self%I_exp = self%I_exp + tmp

   end subroutine

   subroutine read_I_pred(self)
    type(reflection_type) :: self
    ! Read in a predicted intensity

      call read_(stdin,self%I_pred)

   end subroutine

   subroutine add_I_pred(self)
    type(reflection_type) :: self
    ! Read in an increment to add to .I_pred
      real(kind=kind(1.0d0)) :: tmp

      call read_(stdin,tmp)
      self%I_pred = self%I_pred + tmp

   end subroutine

   subroutine read_I_sigma(self)
    type(reflection_type) :: self
    ! Read in an experimentally determined standard deviation in the
    ! observed intensity

      call read_(stdin,self%I_sigma)

   end subroutine

   subroutine add_I_sigma(self)
    type(reflection_type) :: self
    ! Read in an increment to add to .I_sigma
      real(kind=kind(1.0d0)) :: tmp

      call read_(stdin,tmp)
      self%I_sigma = self%I_sigma + tmp

   end subroutine

!  ********************
!  Key related routines
!  ********************

   subroutine read_keys(self)
    type(reflection_type) :: self
    ! Read the "keys".
    ! The following code is inherited from OBJECT

     call clear_keys_(self)
     call read_ptr_(stdin,keys)
     call ignore_memory_leak_(tonto,memory_blocks_gained=1)

   end subroutine

   subroutine process_keys(self)
    type(reflection_type) :: self
    ! Process each of the words in the "keys" list.
    ! The following code is inherited from OBJECT
      integer(kind=kind(1)) :: k,l,n_key
      character(128) :: keyword
      character(128), dimension(:), pointer :: internal

      call ensure_(tonto,associated(keys),"REFLECTION:process_keys ... no keys")
      n_key = size(keys)
      k = 0
      do
         k = k + 1
         keyword = keys(k)
         if (keyword=="}") exit
         if (keyword=="{") then
            l = index_of_matching_bracket_(keys(k:),"{")
            call ensure_(tonto,l>0,"REFLECTION:process_keys ... no matching closing brace, }")
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
    type(reflection_type) :: self
    ! Return .true. if the list-element keys are created.
      logical(kind=kind(.true.)) :: res
    ! The following code is inherited from OBJECT

      res = associated(keys)

   end function

   subroutine set_keys(self,the_keys)
    type(reflection_type) :: self
    ! This is for setting the "keys" externally.
     character(len=*), dimension(:) :: the_keys
    ! The following code is inherited from OBJECT

     call clear_keys_(self)
     call create_copy_(keys,the_keys)
     call ignore_memory_leak_(tonto,memory_blocks_gained=1)

   end subroutine

   subroutine clear_keys(self)
    type(reflection_type) :: self
    ! This is for destroying the "keys" externally.
    ! The following code is inherited from OBJECT

     if (associated(keys)) then
       call destroy_(keys)
       call ignore_memory_leak_(tonto,memory_blocks_gained=-1)
     end if

   end subroutine

   subroutine put_table_footer(self)
    type(reflection_type) :: self
    ! Output a table footer from the list of "keys". NOTE: not all keywords need
    ! contribute to the banner - any unrecognised keyword is skipped.
    ! The following code is inherited from OBJECT

     call dash_(stdout,width=table_width_(self))

   end subroutine

   subroutine put_table_header(self)
    type(reflection_type) :: self
    ! Output a table header from the list of "keys". NOTE: not all keywords need
    ! contribute to the banner - any unrecognised keyword is skipped.
     character(128) :: word
     integer(kind=kind(1)) :: width,k

     width = table_width_(self)
     if (width > 0) then
       call dash_(stdout,width=width)
       do k = 1,size(keys)
         word = keys(k)
         call to_lower_case_(word)
         select case (word)
           case ("flush       "); call flush_(stdout); exit
           case ("put_indices "); call put_(stdout,"h",int_width=.true.)
                                  call put_(stdout,"k",int_width=.true.)
                                  call put_(stdout,"l",int_width=.true.)
           case ("put_h       "); call put_(stdout,"h",int_width=.true.)
           case ("put_k       "); call put_(stdout,"k",int_width=.true.)
           case ("put_l       "); call put_(stdout,"l",int_width=.true.)
           case ("put_f_calc  "); call put_(stdout,"F_calc",width=2*stdout%real_width)
           case ("put_f_exp   "); call put_(stdout,"F_exp")
           case ("put_f_pred  "); call put_(stdout,"F_pred")
           case ("put_f_sigma "); call put_(stdout,"F_sigma")
           case ("put_f_phase "); call put_(stdout,"F_phase")
           case ("put_f_z     "); call put_(stdout,"F_z")
           case ("put_f_z2    "); call put_(stdout,"F_z2")
           case ("put_i_exp   "); call put_(stdout,"I_exp")
           case ("put_i_pred  "); call put_(stdout,"I_pred")
           case ("put_i_sigma "); call put_(stdout,"I_sigma")
           case ("put_i_z     "); call put_(stdout,"I_z")
           case default
         end select
         if (k==size(keys)) then
           call flush_(stdout)  ! In case they didn't write one.
           call warn_(tonto,"REFLECTION:put_table_header ... no flush keyword - you may later overrun the output buffer")
         end if
       end do
       call dash_(stdout,width=width)
     end if

   end subroutine

   function table_width(self) result(res)
    type(reflection_type) :: self
    ! Return the table width in characters, based on "keys".  Note that not all
    ! keywords need to contribute to the banner - if a keyword is not recognised,
    ! then it is skipped.
     integer(kind=kind(1)) :: res
     character(128) :: word
     integer(kind=kind(1)) :: int_dash,real_dash,k

     call ensure_(tonto,associated(keys),"REFLECTION:table_width ... no keywords")
     int_dash = 0
     real_dash = 0
     do k = 1,size(keys)
       word = keys(k)
       call to_lower_case_(word)
       select case (word)
         case ("flush       "); exit
         case ("put_indices "); int_dash = int_dash + 3
         case ("put_h       "); int_dash = int_dash + 1
         case ("put_k       "); int_dash = int_dash + 1
         case ("put_l       "); int_dash = int_dash + 1
         case ("put_f_calc  "); real_dash = real_dash + 2
         case ("put_f_exp   "); real_dash = real_dash + 1
         case ("put_f_pred  "); real_dash = real_dash + 1
         case ("put_f_sigma "); real_dash = real_dash + 1
         case ("put_f_phase "); real_dash = real_dash + 1
         case ("put_f_z     "); real_dash = real_dash + 1
         case ("put_f_z2    "); real_dash = real_dash + 1
         case ("put_i_exp   "); real_dash = real_dash + 1
         case ("put_i_pred  "); real_dash = real_dash + 1
         case ("put_i_sigma "); real_dash = real_dash + 1
         case ("put_i_z     "); real_dash = real_dash + 1
         case default
       end select
     end do
     res = int_dash * stdout%int_width + real_dash * stdout%real_width

   end function

!*******************************************************************************
!                           Enquiry Routines
!*******************************************************************************

   pure function F_z(self) result(res)
    type(reflection_type) :: self
    ! Return the z statistic for the predicted structure factor.
     intent(in) :: self
     real(kind=kind(1.0d0)) :: res
     res = sign(1.0d0,real(self%F_calc)) * (self%F_pred - self%F_exp) / self%F_sigma

   end function

   pure function F_z2(self) result(res)
    type(reflection_type) :: self
    ! Return the z**2 statistic for the predicted structure factor.
     intent(in) :: self
     real(kind=kind(1.0d0)) :: res
     real(kind=kind(1.0d0)) :: z
     z = (self%F_pred - self%F_exp) / self%F_sigma
     res = z*z

   end function

   pure function F_r(self) result(res)
    type(reflection_type) :: self
    ! Return the r factor for the predicted structure factor.
     intent(in) :: self
     real(kind=kind(1.0d0)) :: res
     res = (self%F_pred - self%F_exp) / self%F_exp

   end function

   pure function I_z(self) result(res)
    type(reflection_type) :: self
    ! Return the z statistic for the predicted intensity.
     intent(in) :: self
     real(kind=kind(1.0d0)) :: res
     res = (self%I_pred - self%I_exp) / self%I_sigma

   end function

   pure function F_phase(self) result(res)
    type(reflection_type) :: self
    ! Return the phase angle of the complex structure factor.
     intent(in) :: self
     real(kind=kind(1.0d0)) :: res
     res = atan2(aimag(self%F_calc),real(self%F_calc))

   end function
end
