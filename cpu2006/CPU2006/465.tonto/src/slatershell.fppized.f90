!-------------------------------------------------------------------------------
!
! SLATERSHELL: used to describe contracted slater shells.
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
! $Id: slatershell.foo,v 1.1.2.5 2004/04/21 09:12:56 reaper Exp $
!-------------------------------------------------------------------------------

module SLATERSHELL_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

   use REALVEC_MODULE, only: same_as_
   use REALVEC_MODULE, only: create_
   use REALVEC_MODULE, only: create_copy_
   use REALVEC_MODULE, only: destroy_

   use INTVEC_MODULE, only: same_as_
   use INTVEC_MODULE, only: create_
   use INTVEC_MODULE, only: create_copy_
   use INTVEC_MODULE, only: destroy_

   use INT_MODULE, only: to_str_

   use STR_MODULE, only: is_int_
   use STR_MODULE, only: split_
   use STR_MODULE, only: to_lower_case_
   use STR_MODULE, only: to_int_
   use STR_MODULE, only: includes_

   use TEXTFILE_MODULE, only: stdin
   use TEXTFILE_MODULE, only: stdout
   use TEXTFILE_MODULE, only: set_default_units_
   use TEXTFILE_MODULE, only: next_str_
   use TEXTFILE_MODULE, only: skip_next_item_
   use TEXTFILE_MODULE, only: redirect_
   use TEXTFILE_MODULE, only: next_item_
   use TEXTFILE_MODULE, only: show_
   use TEXTFILE_MODULE, only: put_
   use TEXTFILE_MODULE, only: revert_
   use TEXTFILE_MODULE, only: read_
   use TEXTFILE_MODULE, only: flush_
   use TEXTFILE_MODULE, only: reverted_
   use TEXTFILE_MODULE, only: move_to_previous_item_
   use TEXTFILE_MODULE, only: read_ptr_
   use TEXTFILE_MODULE, only: dash_

   use STRVEC_MODULE, only: create_
   use STRVEC_MODULE, only: create_copy_
   use STRVEC_MODULE, only: index_of_matching_bracket_
   use STRVEC_MODULE, only: destroy_

   use REAL_MODULE, only: is_int_

   use REALMAT_MODULE, only: same_as_
   use REALMAT_MODULE, only: create_
   use REALMAT_MODULE, only: create_copy_
   use REALMAT_MODULE, only: destroy_
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

   public    read_z_
   interface read_z_
      module procedure read_z
   end interface

   public    read_keys_
   interface read_keys_
      module procedure read_keys
   end interface

   public    read_n_prim_
   interface read_n_prim_
      module procedure read_n_prim
   end interface

   private    read_l_int_
   interface read_l_int_
      module procedure read_l_int
   end interface

   public    nullify_ptr_part_
   interface nullify_ptr_part_
      module procedure nullify_ptr_part
   end interface

   public    density_values_at_points_
   interface density_values_at_points_
      module procedure density_values_at_points
   end interface

   public    same_as_
   interface same_as_
      module procedure same_as
   end interface

   public    read_occupancy_
   interface read_occupancy_
      module procedure read_occupancy
   end interface

   public    read_kind_
   interface read_kind_
      module procedure read_kind
   end interface

   public    put_table_header_
   interface put_table_header_
      module procedure put_table_header
   end interface

   public    destroy_ptr_part_
   interface destroy_ptr_part_
      module procedure destroy_ptr_part
   end interface

   public    table_width_
   interface table_width_
      module procedure table_width
   end interface

   public    read_keywords_
   interface read_keywords_
      module procedure read_keywords
   end interface

   public    read_l_kind_n_z_c_ptr_
   interface read_l_kind_n_z_c_ptr_
      module procedure read_l_kind_n_z_c_ptr
   end interface

   public    read_n_z_c_ptr_
   interface read_n_z_c_ptr_
      module procedure read_n_z_c_ptr
   end interface

   public    read_n_orb_
   interface read_n_orb_
      module procedure read_n_orb
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

   private    read_l_chr_
   interface read_l_chr_
      module procedure read_l_chr
   end interface

   public    create_
   interface create_
      module procedure create
   end interface

   public    put_table_
   interface put_table_
      module procedure put_table
   end interface

   public    keys_created_
   interface keys_created_
      module procedure keys_created
   end interface

   public    read_c_
   interface read_c_
      module procedure read_c
   end interface

   public    set_keys_
   interface set_keys_
      module procedure set_keys
   end interface

   public    no_of_primitives_
   interface no_of_primitives_
      module procedure no_of_primitives
   end interface

   public    destroy_
   interface destroy_
      module procedure destroy
   end interface

   public    read_junk_
   interface read_junk_
      module procedure read_junk
   end interface

   public    read_l_
   interface read_l_
      module procedure read_l
   end interface

   public    density_values_at_radii_
   interface density_values_at_radii_
      module procedure density_values_at_radii
   end interface

   public    l_chr_
   interface l_chr_
      module procedure l_chr
   end interface

   public    read_n_
   interface read_n_
      module procedure read_n
   end interface

   public    no_of_basis_functions_
   interface no_of_basis_functions_
      module procedure no_of_basis_functions
   end interface

   public    read_units_
   interface read_units_
      module procedure read_units
   end interface

   public    set_n_comp_
   interface set_n_comp_
      module procedure set_n_comp
   end interface

   public    density_value_at_radius_
   interface density_value_at_radius_
      module procedure density_value_at_radius
   end interface

   character(128), dimension(:), pointer, private :: keys => NULL()

contains

! ***************************
! Create and destroy routines
! ***************************

   subroutine create(self)
    type(slatershell_type) :: self
    ! Create an object
      pointer :: self
    ! The following code is inherited from OBJECT

      nullify(self)
      allocate(self)

      call nullify_ptr_part_(self)
      call set_defaults_(self)

   end subroutine

   subroutine destroy(self)
    type(slatershell_type) :: self
    ! Destroy an object
      pointer :: self
    ! The following code is inherited from OBJECT

      if (associated(self)) then
        call destroy_ptr_part_(self)
        deallocate(self)

      end if

   end subroutine

   subroutine nullify_ptr_part(self)
    type(slatershell_type) :: self
    ! Nullify the pointer parts of the atomvec

      nullify(self%n)
      nullify(self%z)
      nullify(self%c)
      nullify(self%orb_kind)
      nullify(self%occupancy)

   end subroutine

   subroutine destroy_ptr_part(self)
    type(slatershell_type) :: self
    ! Destroy the pointer parts

      call destroy_(self%n)
      call destroy_(self%z)
      call destroy_(self%c)
      call destroy_(self%orb_kind)
      call destroy_(self%occupancy)

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
    type(slatershell_type) :: self
    ! Set default values

      self%l = 0
      self%n_orb = 0
      self%n_prim = 0

   end subroutine

   subroutine copy(self,c)
    type(slatershell_type) :: self
    ! Create a copy of c
       type(slatershell_type) :: c

       self = c
       if (associated(c%n)) call create_copy_(self%n,c%n)
       if (associated(c%z)) call create_copy_(self%z,c%z)
       if (associated(c%c)) call create_copy_(self%c,c%c)
       if (associated(c%orb_kind)) call create_copy_(self%orb_kind,c%orb_kind)
       if (associated(c%occupancy)) call create_copy_(self%occupancy,c%occupancy)

   end subroutine

   subroutine set_n_comp(self)
    type(slatershell_type) :: self
    ! Set the number of components

      self%n_comp = 2*self%l+1

   end subroutine

! ***********
! I/O methods
! ***********

   recursive subroutine read_keywords(self)
    type(slatershell_type) :: self
    ! Read data from "stdin" using keyword style input.
    ! The following code is inherited from OBJECT
     character(128) :: word

     call ensure_(tonto,next_item_(stdin)=="{","SLATERSHELL:read_keywords ... expecting open bracket symbol, {")
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
    type(slatershell_type) :: self
    ! Process command "keyword". Data is inputted from "stdin", unless "keyword"
    ! is a sequence of blank separated strings.  In this case, the sequence is
    ! processed as if it were a separate file.
     character(*), intent(in) :: keyword
     character(128) :: word

     word = keyword
     call to_lower_case_(word)
     select case (word)
        case ("}                          ")   ! exit case
        case ("c=                         "); call read_c_(self)
        case ("junk=                      "); call read_junk_(self)
        case ("kind=                      "); call read_kind_(self)
        case ("l=                         "); call read_l_(self)
        case ("l_int=                     "); call read_l_int_(self)
        case ("l_chr=                     "); call read_l_chr_(self)
        case ("l,kind,n,z,c*=             "); call read_l_kind_n_z_c_ptr_(self)
        case ("n=                         "); call read_n_(self)
        case ("n_prim=                    "); call read_n_prim_(self)
        case ("n_orb=                     "); call read_n_orb_(self)
        case ("n,z,c*=                    "); call read_n_z_c_ptr_(self)
        case ("occupancy=                 "); call read_occupancy_(self)
        case ("units=                     "); call read_units_(self)
        case ("z=                         "); call read_z_(self)
        case  default;                        allocate(tonto%known_keywords(15))
        tonto%known_keywords(1) = "}                          "
        tonto%known_keywords(2) = "c=                         "
        tonto%known_keywords(3) = "junk=                      "
        tonto%known_keywords(4) = "kind=                      "
        tonto%known_keywords(5) = "l=                         "
        tonto%known_keywords(6) = "l_int=                     "
        tonto%known_keywords(7) = "l_chr=                     "
        tonto%known_keywords(8) = "l,kind,n,z,c*=             "
        tonto%known_keywords(9) = "n=                         "
        tonto%known_keywords(10) = "n_prim=                    "
        tonto%known_keywords(11) = "n_orb=                     "
        tonto%known_keywords(12) = "n,z,c*=                    "
        tonto%known_keywords(13) = "occupancy=                 "
        tonto%known_keywords(14) = "units=                     "
        tonto%known_keywords(15) = "z=                         "
        call unknown_(tonto,word,"SLATERSHELL:process_keyword")
        deallocate(tonto%known_keywords)
     end select

   end subroutine

   subroutine read_units(self)
    type(slatershell_type) :: self
    ! Read a string which describes the units to be used
    ! The following code is inherited from OBJECT

      call set_default_units_(stdin,next_str_(stdin))

   end subroutine

   subroutine read_junk(self)
    type(slatershell_type) :: self
    ! Read in a junk string, useful for ignoring a field
    ! The following code is inherited from OBJECT

      call skip_next_item_(stdin)

   end subroutine

   subroutine read_l(self)
    type(slatershell_type) :: self
    ! Read in the l symbol
    ! The following code is inherited from type(shell_type)
      character(128) :: word

      call read_(stdin,word)
      call move_to_previous_item_(stdin)
      if (is_int_(word)) then; call read_l_int_(self)
      else;                  call read_l_chr_(self)
      end if

   end subroutine

   subroutine read_l_int(self)
    type(slatershell_type) :: self
    ! Read in the l integer
    ! The following code is inherited from type(shell_type)
      character(128) :: word

      call read_(stdin,word)
      call ensure_(tonto,is_int_(word),"SLATERSHELL:read_l_int ... expecting an integer for L")
      self%l = to_int_(word)
      call set_n_comp_(self)

   end subroutine

   subroutine read_l_chr(self)
    type(slatershell_type) :: self
    ! Read in the l symbol
    ! The following code is inherited from type(shell_type)
      character(128) :: word
      character(1) :: l_c
      integer(kind=kind(1)) :: l

      call read_(stdin,word)
      call ensure_(tonto,len_trim(word)==1,"SLATERSHELL:read_l_chr ... unknown L symbol")
      l_c = word
      call to_lower_case_(l_c)
      select case (l_c)
         case ("s");   l = 0
         case ("p");   l = 1
         case ("d");   l = 2
         case ("f");   l = 3
         case ("g");   l = 4
         case default; l = 4 + iachar(l_c)-iachar("g")
      end select
      self%l = l
      call set_n_comp_(self)

   end subroutine

   subroutine read_n_prim(self)
    type(slatershell_type) :: self
    ! Read in the number of contraction coefficients

      call read_(stdin,self%n_prim)
      call ensure_(tonto,self%n_prim>0,"SLATERSHELL:read_n_prim ... n_prim must be positive")

   end subroutine

   subroutine read_n_orb(self)
    type(slatershell_type) :: self
    ! Read in the number of generally contracted orbitals

      call read_(stdin,self%n_orb)
      call ensure_(tonto,self%n_orb>0,"SLATERSHELL:read_n_orb ... n_orb must be positive")

   end subroutine

   subroutine read_n(self)
    type(slatershell_type) :: self
    ! Read in the "n" quantum numbers. NOTE: n_prim must already have been input.
   call ensure_(tonto,self%n_prim>0,"SLATERSHELL:read_n ... n_prim not set; use n_prim= before this command")

      call destroy_(self%n)
      call create_(self%n,self%n_prim)
      call read_(stdin,self%n)

   end subroutine

   subroutine read_c(self)
    type(slatershell_type) :: self
    ! Read in the "c" contraction coefficients. NOTE: n_orb must already have
    ! been input.
   call ensure_(tonto,self%n_prim>0,"SLATERSHELL:read_c ... n_prim not set; use n_prim= before this command")
   call ensure_(tonto,self%n_orb>0,"SLATERSHELL:read_c ... n_orb not set; use n_orb= before this command")

      call destroy_(self%c)
      call create_(self%c,self%n_prim,self%n_orb)
      call read_(stdin,self%c)

   end subroutine

   subroutine read_z(self)
    type(slatershell_type) :: self
    ! Read in the "z" slater function exponents. NOTE: n_prim must already have
    ! been input.
   call ensure_(tonto,self%n_prim>0,"SLATERSHELL:read_z ... n_prim not set; use n_prim= before this command")

      call destroy_(self%z)
      call create_(self%z,self%n_prim)
      call read_(stdin,self%z)

   end subroutine

   subroutine read_kind(self)
    type(slatershell_type) :: self
    ! Read in the orbital kind ("1s", "2s", "2p" ....); also set zero
    ! occupancies, if kinds are not set

      if (associated(self%orb_kind)) call destroy_(self%orb_kind)
      call read_ptr_(stdin,self%orb_kind)
      if (self%n_orb>0) then; call ensure_(tonto,self%n_orb==size(self%orb_kind),"SLATERSHELL:read_kind ... n_orb and kind le&
&ngth inconsistent")
      else;               self%n_orb = size(self%orb_kind)
      end if
      if (associated(self%occupancy)) then
         call warn_if_(tonto,self%n_orb/=size(self%occupancy),"SLATERSHELL:read_kind ... n_orb and occupancy length inconsist&
&ent")
      else
         call create_(self%occupancy,self%n_orb)
         self%occupancy = 0
      end if

   end subroutine

   subroutine read_occupancy(self)
    type(slatershell_type) :: self
    ! Read in the occupancies; also set blank kinds, if kinds are not set.

      if (associated(self%occupancy)) call destroy_(self%occupancy)
      call read_ptr_(stdin,self%occupancy)
      if (self%n_orb>0) then; call ensure_(tonto,self%n_orb==size(self%orb_kind),"SLATERSHELL:read_occupancy ... n_orb and ki&
&nd length inconsistent")
      else;               self%n_orb = size(self%orb_kind)
      end if
      if (associated(self%orb_kind)) then
         call warn_if_(tonto,self%n_orb/=size(self%orb_kind),"SLATERSHELL:read_occupancy ... n_orb and kind length inconsiste&
&nt")
      else
         call create_(self%orb_kind,self%n_orb)
         self%orb_kind = " "
      end if

   end subroutine

   subroutine read_l_kind_n_z_c_ptr(self)
    type(slatershell_type) :: self
    ! Read in everything: l, kind, and then n,z,c in a table, as in an Ajit
    ! Thakkar table

      call read_l_chr_(self)
      call read_kind_(self)
      call read_n_z_c_ptr_(self)

   end subroutine

   subroutine read_n_z_c_ptr(self)
    type(slatershell_type) :: self
    ! Read in the "n", "z" and then "c" coefficients, across a line.
      real(kind=kind(1.0d0)), dimension(:), pointer :: tmp
      integer(kind=kind(1)) :: i,k
   call ensure_(tonto,self%n_orb>0,"SLATERSHELL:read_n_z_c_ptr ... n_orb not set; use n_orb= before this command")

   call ensure_(tonto,self%n_prim==0,"SLATERSHELL:read_n_z_c_ptr ... n_prim already defined!")
      call read_ptr_(stdin,tmp)
      call ensure_(tonto,mod(size(tmp),(self%n_orb+2))==0,"SLATERSHELL:read_n_z_c_ptr ... number of data not divisible by n_o&
&rb")
      self%n_prim =  size(tmp)/(self%n_orb+2)
      call destroy_(self%n); call destroy_(self%c); call destroy_(self%z)
      call create_(self%n,self%n_prim)
      call create_(self%z,self%n_prim)
      call create_(self%c,self%n_prim,self%n_orb)
      k = 0
      do i = 1,self%n_prim
         call ensure_(tonto,is_int_(tmp(k+1)),"SLATERSHELL:read_n_z_c_ptr ... n number is not integer")
         self%n(i)   = tmp(k+1)
         self%z(i)   = tmp(k+2)
         self%c(i,:) = tmp(k+2+1:k+2+self%n_orb)
         k = k + 2 + self%n_orb
      end do
      call destroy_(tmp)

   end subroutine

!  ********************
!  Key related routines
!  ********************

   subroutine read_keys(self)
    type(slatershell_type) :: self
    ! Read the "keys".
    ! The following code is inherited from OBJECT

     call clear_keys_(self)
     call read_ptr_(stdin,keys)
     call ignore_memory_leak_(tonto,memory_blocks_gained=1)

   end subroutine

   subroutine process_keys(self)
    type(slatershell_type) :: self
    ! Process each of the words in the "keys" list.
    ! The following code is inherited from OBJECT
      integer(kind=kind(1)) :: k,l,n_key
      character(128) :: keyword
      character(128), dimension(:), pointer :: internal

      call ensure_(tonto,associated(keys),"SLATERSHELL:process_keys ... no keys")
      n_key = size(keys)
      k = 0
      do
         k = k + 1
         keyword = keys(k)
         if (keyword=="}") exit
         if (keyword=="{") then
            l = index_of_matching_bracket_(keys(k:),"{")
            call ensure_(tonto,l>0,"SLATERSHELL:process_keys ... no matching closing brace, }")
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
    type(slatershell_type) :: self
    ! Return .true. if the list-element keys are created.
      logical(kind=kind(.true.)) :: res
    ! The following code is inherited from OBJECT

      res = associated(keys)

   end function

   subroutine set_keys(self,the_keys)
    type(slatershell_type) :: self
    ! This is for setting the "keys" externally.
     character(len=*), dimension(:) :: the_keys
    ! The following code is inherited from OBJECT

     call clear_keys_(self)
     call create_copy_(keys,the_keys)
     call ignore_memory_leak_(tonto,memory_blocks_gained=1)

   end subroutine

   subroutine clear_keys(self)
    type(slatershell_type) :: self
    ! This is for destroying the "keys" externally.
    ! The following code is inherited from OBJECT

     if (associated(keys)) then
       call destroy_(keys)
       call ignore_memory_leak_(tonto,memory_blocks_gained=-1)
     end if

   end subroutine

   subroutine put_table_footer(self)
    type(slatershell_type) :: self
    ! Output a table footer from the list of "keys". NOTE: not all keywords need
    ! contribute to the banner - any unrecognised keyword is skipped.
    ! The following code is inherited from OBJECT

     call dash_(stdout,width=table_width_(self))

   end subroutine

   subroutine put_table_header(self)
    type(slatershell_type) :: self
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
           case ("flush            "); call flush_(stdout); exit
           case ("put_l            "); call put_(stdout,"l",int_width=.true.)
           case ("put_n_prim       "); call put_(stdout,"n_prim",int_width=.true.)
           case ("put_n_orb        "); call put_(stdout,"n_orb",int_width=.true.)
           case ("put_configuration"); call put_(stdout,"Config.")
           case default
         end select
         if (k==size(keys)) then
           call flush_(stdout)  ! In case they didn't write one.
           call warn_(tonto,"SLATERSHELL:put_table_header ... no flush keyword - you may later overrun the output buffer")
         end if
       end do
       call dash_(stdout,width=width)
     end if

   end subroutine

   function table_width(self) result(res)
    type(slatershell_type) :: self
    ! Return how wide a table is, based on "keys".  Note that not all keywords
    ! need to contribute to the banner - any unrecognised keyword is skipped.
     integer(kind=kind(1)) :: res
     character(128) :: word
     integer(kind=kind(1)) :: int_dash,real_dash,k

     int_dash = 0
     real_dash = 0
     call ensure_(tonto,associated(keys),"SLATERSHELL:table_width ... no keywords")
     do k = 1,size(keys)
       word = keys(k)
       call to_lower_case_(word)
       select case (word)
         case ("put_l            "); int_dash = int_dash + 1
         case ("put_n_prim       "); int_dash = int_dash + 1
         case ("put_n_orb        "); int_dash = int_dash + 1
         case ("put_configuration"); real_dash = real_dash + 1
         case ("flush            "); exit
         case default
       end select
     end do
     res = int_dash * stdout%int_width + real_dash * stdout%real_width

   end function

! *******
! Methods
! *******

   function same_as(self,b) result(res)
    type(slatershell_type) :: self
    ! Return .true. if "self" is the same as "b".
      intent(in) :: self
      type(slatershell_type), intent(in) :: b
      logical(kind=kind(.true.)) :: res

   call ensure_(tonto,associated(self%n),"SLATERSHELL:same_as ... no n quantum numbers")
   call ensure_(tonto,associated(self%c),"SLATERSHELL:same_as ... no contraction coefficients")
   call ensure_(tonto,associated(self%z),"SLATERSHELL:same_as ... no exponents")
   call ensure_(tonto,associated(b%n),"SLATERSHELL:same_as ... no n quantum numbers to compare to")
   call ensure_(tonto,associated(b%c),"SLATERSHELL:same_as ... no contraction coefficients to compare to")
   call ensure_(tonto,associated(b%z),"SLATERSHELL:same_as ... no exponents to compare to")
      res = self%l == b%l &
            .and. same_as_(self%n,b%n) &
            .and. same_as_(self%c,b%c) &
            .and. same_as_(self%z,b%z)
      if (res .and. associated(self%orb_kind))  res = all(self%orb_kind==b%orb_kind)
      if (res .and. associated(self%occupancy)) res = all(self%occupancy==b%occupancy)

   end function

   function l_chr(self) result(res)
    type(slatershell_type) :: self
    ! Return a character representation for the angular mtm
      character(1) :: res
    ! The following code is inherited from type(shell_type)
      integer(kind=kind(1)) :: l

      l = self%l
      select case (l)
         case (0); res="s"
         case (1); res="p"
         case (2); res="d"
         case (3); res="f"
         case (4); res="g"
         case default;
            call die_if_(tonto,l>23,"SLATERSHELL:l_chr ... angular momentum too large:"// trim(to_str_(l)))
            res = achar(l-4+iachar("g"))
      end select

   end function

   pure function no_of_basis_functions(self) result(res)
    type(slatershell_type) :: self
    ! Work out and return the TOTAL number of generally contracted basis
    ! functions , counting the agular part also.
      intent(in) :: self
      integer(kind=kind(1)) :: res
      res = self%n_comp*self%n_orb

   end function

   pure function no_of_primitives(self) result(res)
    type(slatershell_type) :: self
    ! Work out and return the TOTAL number of primitives, counting the angular
    ! part also.
      intent(in) :: self
      integer(kind=kind(1)) :: res
      res = self%n_comp*self%n_prim

   end function

   function density_value_at_radius(self,R) result(res)
    type(slatershell_type) :: self
    ! Returns the value of the coppens orbital at radius "R".
    ! SOme work can be save if the prefactor array is made.
      real(kind=kind(1.0d0)), intent(in) :: R
      real(kind=kind(1.0d0)) :: res
      integer(kind=kind(1)) :: i
      real(kind=kind(1.0d0)) :: orb
      real(kind=kind(1.0d0)), dimension(:), pointer :: val

      call create_(val,self%n_prim)
      val = R**(self%n-1) * exp(-self%z*R)  ! save this
      res = 0.0d0
      do i = 1,self%n_orb
         orb = sum(self%c(:,i) * val)
         res = res + self%occupancy(i)*orb*orb
      end do
      call destroy_(val)

   end function

   function density_values_at_radii(self,R) result(res)
    type(slatershell_type) :: self
    ! Returns the values of the coppens density at all the radii "R".
      real(kind=kind(1.0d0)), dimension(:), intent(in) :: R
      real(kind=kind(1.0d0)), dimension(size(R)) :: res
      integer(kind=kind(1)) :: i,p
      real(kind=kind(1.0d0)) :: orb
      real(kind=kind(1.0d0)), dimension(:), pointer :: val

      call create_(val,self%n_prim)
      do p = 1,size(R)
         val = R(p)**(self%n-1) * exp(-self%z*R(p))  ! save this
         res(p) = 0.0d0
         do i = 1,self%n_orb
            orb = sum(self%c(:,i) * val)
            res(p) = res(p) + self%occupancy(i)*orb*orb
         end do
      end do
      call destroy_(val)

   end function

   function density_values_at_points(self,pt,pos) result(res)
    type(slatershell_type) :: self
    ! Make the orbital density values on the series of points specified in "pt",
    ! assuming the orbital is at point "pos"; and put the results in "res".
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: pt
      real(kind=kind(1.0d0)), dimension(3), intent(in) :: pos
      real(kind=kind(1.0d0)), dimension(size(pt,1)) :: res
      real(kind=kind(1.0d0)), dimension(:), pointer :: R
      real(kind=kind(1.0d0)) :: x,y,z
      integer(kind=kind(1)) :: n,n_pt

   call ensure_(tonto,size(pt,2)==3,"SLATERSHELL:density_values_at_points ... incorrect second dimension, pt array")
      n_pt = size(pt,1)
      call create_(R,n_pt)
      do n = 1,n_pt
         x = pt(n,1) - pos(1)
         y = pt(n,2) - pos(2)
         z = pt(n,3) - pos(3)
         R(n) = sqrt(x*x + y*y + z*z)
      end do
      res = density_values_at_radii_(self,R)
      call destroy_(R)

   end function

   subroutine put(self)
    type(slatershell_type) :: self
    ! Put the orbital information to "stdout"
      integer(kind=kind(1)) :: i

      call flush_(stdout)
      call show_(stdout,"L quantum no.     = ",self%l)
      call show_(stdout,"No. of orbitals   = ",self%n_orb)
      call show_(stdout,"No. of primitives = ",self%n_prim)
      call put_table_(self)

   end subroutine

   subroutine put_table(self)
    type(slatershell_type) :: self
    ! Put the orbital information to "stdout" in table format
      integer(kind=kind(1)) :: i,j

      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=self%n_orb+1)
      call put_(stdout,"#",int_width=.true.)
      call put_(stdout,"N",int_width=.true.)
      call put_(stdout,"Exponent")
      if (associated(self%orb_kind) .and. associated(self%occupancy)) then
         do j = 1,self%n_orb
            call put_(stdout,trim(self%orb_kind(j))//"("//trim(to_str_(self%occupancy(j)))//")")
         end do
      else if (associated(self%orb_kind)) then
         do j = 1,self%n_orb
            call put_(stdout,trim(self%orb_kind(j)))
         end do
      else
         do j = 1,self%n_orb
            call put_(stdout,"Orb. "//trim(to_str_(j)))
         end do
      end if
      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=self%n_orb+1)
      do i = 1,self%n_prim
         call put_(stdout,i)
         call put_(stdout,self%n(i))
         call put_(stdout,self%z(i))
         do j = 1,self%n_orb
            call put_(stdout,self%c(i,j))
         end do
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=2,real_fields=self%n_orb+1)

   end subroutine

end
