!-------------------------------------------------------------------------------
!
! SLATERBASIS: For Slater basis sets
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
! $Id: slaterbasis.foo,v 1.1.2.5 2004/04/21 09:12:56 reaper Exp $
!
!-------------------------------------------------------------------------------

module SLATERBASIS_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

   use REALVEC_MODULE, only: create_
   use REALVEC_MODULE, only: destroy_

   use TEXTFILE_MODULE, only: stdin
   use TEXTFILE_MODULE, only: stdout
   use TEXTFILE_MODULE, only: set_default_units_
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

   use STR_MODULE, only: replace_
   use STR_MODULE, only: split_
   use STR_MODULE, only: to_lower_case_
   use STR_MODULE, only: to_int_
   use STR_MODULE, only: includes_

   use STRVEC_MODULE, only: create_copy_
   use STRVEC_MODULE, only: index_of_matching_bracket_
   use STRVEC_MODULE, only: destroy_

   use SLATERSHELLVEC_MODULE, only: densities_at_radii_
   use SLATERSHELLVEC_MODULE, only: set_saved_self_
   use SLATERSHELLVEC_MODULE, only: no_of_basis_functions_
   use SLATERSHELLVEC_MODULE, only: create_copy_
   use SLATERSHELLVEC_MODULE, only: no_of_shells_
   use SLATERSHELLVEC_MODULE, only: put_
   use SLATERSHELLVEC_MODULE, only: same_as_
   use SLATERSHELLVEC_MODULE, only: set_keys_
   use SLATERSHELLVEC_MODULE, only: no_of_primitives_
   use SLATERSHELLVEC_MODULE, only: min_exponent_
   use SLATERSHELLVEC_MODULE, only: read_list_keywords_
   use SLATERSHELLVEC_MODULE, only: read_data_
   use SLATERSHELLVEC_MODULE, only: destroy_

   use BINVEC_MODULE, only: create_
   use BINVEC_MODULE, only: destroy_
   use BINVEC_MODULE, only: index_of_first_true_element_

   use INTERPOLATOR_MODULE, only: set_even_spaced_data_
   use INTERPOLATOR_MODULE, only: create_
   use INTERPOLATOR_MODULE, only: values_for_
   use INTERPOLATOR_MODULE, only: create_copy_
   use INTERPOLATOR_MODULE, only: destroy_
   use SLATERSHELLVEC_MODULE, only: density_value_at_radius

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

   public    update_
   interface update_
      module procedure update
   end interface

   public    nullify_ptr_part_
   interface nullify_ptr_part_
      module procedure nullify_ptr_part
   end interface

   public    read_configuration_
   interface read_configuration_
      module procedure read_configuration
   end interface

   public    same_as_
   interface same_as_
      module procedure same_as
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

   public    no_of_shells_
   interface no_of_shells_
      module procedure no_of_shells
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

   public    read_label_
   interface read_label_
      module procedure read_label
   end interface

   public    min_exponent_
   interface min_exponent_
      module procedure min_exponent
   end interface

   public    copy_
   interface copy_
      module procedure copy
   end interface

   public    read_tonto_style_
   interface read_tonto_style_
      module procedure read_tonto_style
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

   public    make_interpolator_
   interface make_interpolator_
      module procedure make_interpolator
   end interface

   public    create_copy_
   interface create_copy_
      module procedure create_copy
   end interface

   public    keys_created_
   interface keys_created_
      module procedure keys_created
   end interface

   public    analyse_configuration_
   interface analyse_configuration_
      module procedure analyse_configuration
   end interface

   public    set_keys_
   interface set_keys_
      module procedure set_keys
   end interface

   public    no_of_primitives_
   interface no_of_primitives_
      module procedure no_of_primitives
   end interface

   public    resolve_by_label_
   interface resolve_by_label_
      module procedure resolve_by_label
   end interface

   public    make_density_grid_
   interface make_density_grid_
      module procedure make_density_grid
   end interface

   public    destroy_
   interface destroy_
      module procedure destroy
   end interface

   public    read_junk_
   interface read_junk_
      module procedure read_junk
   end interface

   public    read_shell_
   interface read_shell_
      module procedure read_shell
   end interface

   public    max_n_orb_
   interface max_n_orb_
      module procedure max_n_orb
   end interface

   public    set_label_
   interface set_label_
      module procedure set_label
   end interface

   public    make_interpolated_density_grid_
   interface make_interpolated_density_grid_
      module procedure make_interpolated_density_grid
   end interface

   public    make_normal_density_grid_
   interface make_normal_density_grid_
      module procedure make_normal_density_grid
   end interface

   public    no_of_basis_functions_
   interface no_of_basis_functions_
      module procedure no_of_basis_functions
   end interface

   public    read_units_
   interface read_units_
      module procedure read_units
   end interface

   public    shell_kinds_created_
   interface shell_kinds_created_
      module procedure shell_kinds_created
   end interface

   character(128), dimension(:), pointer, private :: keys => NULL()

contains

!*******************************************************************************
!                             Create and Destroy Routines
!*******************************************************************************

   subroutine create(self)
    type(slaterbasis_type) :: self
    ! Create an object
      pointer :: self
    ! The following code is inherited from OBJECT

      nullify(self)
      allocate(self)

      call nullify_ptr_part_(self)
      call set_defaults_(self)

   end subroutine

   subroutine destroy(self)
    type(slaterbasis_type) :: self
    ! Destroy an object
      pointer :: self
    ! The following code is inherited from OBJECT

      if (associated(self)) then
        call destroy_ptr_part_(self)
        deallocate(self)

      end if

   end subroutine

   subroutine nullify_ptr_part(self)
    type(slaterbasis_type) :: self
    ! Nullify the shell parts of self

      nullify(self%shell)
      nullify(self%interpolator)

   end subroutine

   subroutine destroy_ptr_part(self)
    type(slaterbasis_type) :: self
    ! Destroy the shell parts of self

      call destroy_(self%shell)
      call destroy_(self%interpolator)

   end subroutine

!   created result(res) ::: pure
!   ! Returns true if self has been created
!      self :: pointer
!      res :: logical(kind=kind(.true.))
!      res = associated(self)
!   end

!   destroyed result(res) ::: pure
!   ! Returns true if self has *not* been created
!      self :: pointer
!      res :: logical(kind=kind(.true.))
!      res = .not. associated(self)
!   end

   subroutine create_copy(self,b)
    type(slaterbasis_type) :: self
    ! Create a copy of the basis "b".
     type(slaterbasis_type), intent(in) :: b
     pointer :: self

     call create_(self)
     call copy_(self,b)

   end subroutine

   subroutine copy(self,b)
    type(slaterbasis_type) :: self
    ! Copy a basis "b" to "self". Make sure pointer parts are first
    ! destroyed or nullified, as you want.
      type(slaterbasis_type), intent(in) :: b

      self = b
      if (associated(b%shell)) call create_copy_(self%shell,b%shell)
      if (associated(b%interpolator)) call create_copy_(self%interpolator,b%interpolator)

   end subroutine

   subroutine set_defaults(self)
    type(slaterbasis_type) :: self
    ! Create and set up a default basis set
    ! The following code is inherited from type(basis_type)

      self%label   = " "
      self%n_shell = 0
      self%n_bf    = 0
      self%n_prim  = 0

   end subroutine

   subroutine update(self)
    type(slaterbasis_type) :: self
    ! Update the shell data, if it exists
    ! The following code is inherited from type(basis_type)

      if (.not. associated(self%shell)) then;   return; end if
      self%n_shell = no_of_shells_(self)
      self%n_bf    = no_of_basis_functions_(self)
      self%n_prim  = no_of_primitives_(self)

   end subroutine

   subroutine set_label(self,label)
    type(slaterbasis_type) :: self
    ! Set the basis label
      character(128) :: label

      self%label = label

   end subroutine

   subroutine resolve_by_label(self,label,basis,clobber,found)
    type(slaterbasis_type) :: self
    ! Resolve "self" by pointer assigning it to the element in "basis" which has
    ! a label which matches "label". If "clobber" is present and .true. (the
    ! default situation), then "self" is pointer assigned to the matching element
    ! in "basis" irrespective of whether it is already associated; otherwise it
    ! is not pointer assigned. If present, "found" is set .true. if "self" is
    ! resolved (or was already resolved if clobber was not set), or false
    ! otherwise. If "found" is not present, and a match has not been found, an
    ! error is generated.
      pointer :: self
      character(128) :: label
      type(slaterbasis_type), dimension(:), pointer :: basis
      logical(kind=kind(.true.)), optional :: clobber,found
      integer(kind=kind(1)) :: b
      logical(kind=kind(.true.)), dimension(:), pointer :: check
      logical(kind=kind(.true.)) :: fnd

   call ensure_(tonto,associated(basis),"SLATERBASIS:resolve_by_label ... no basis set")
      if (present(clobber)) then
      if (.not. clobber) then
      if (associated(self)) then
      if (self%label/=" ") then
         if (present(found)) found = .true.
           return
      end if
      end if
      end if
      end if
      call create_(check,size(basis))
      check = basis%label==label
      b = index_of_first_true_element_(check)
      call destroy_(check)
      fnd = b>0
      if (fnd) self => basis(b)  ! NOTE : this is a pointer assign .not. COPY
      if (present(found)) then; found = fnd
      else; call ensure_(tonto,fnd,"SLATERBASIS:resolve_by_label ... unknown basis label, "// trim(label))
      end if

   end subroutine

!  ************
!  I/O Routines
!  ************

   recursive subroutine read_keywords(self)
    type(slaterbasis_type) :: self
    ! Read data from "stdin" using keyword style input.
    ! The following code is inherited from OBJECT
     character(128) :: word

     call ensure_(tonto,next_item_(stdin)=="{","SLATERBASIS:read_keywords ... expecting open bracket symbol, {")
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
    type(slaterbasis_type) :: self
    ! Process command "keyword". Any required data needed by the "keyword" is
    ! inputted from "stdin".
     character(*), intent(in) :: keyword
     character(128) :: word

     word = keyword
     call to_lower_case_(word)
     select case (word)
       case ("-- Regular options --   ")
       case ("}                       ");  ! exit surrounding loop
       case ("analyse_configuration   "); call analyse_configuration_(self)
       case ("configuration=          "); call read_configuration_(self)
       case ("label=                  "); call read_label_(self)
       case ("put                     "); call put_(self)
       case ("shells=                 "); call read_shell_(self)
       case ("tonto-style=            "); call read_tonto_style_(self)
       case ("units=                  "); call read_units_(self)
        ! These are only for making custom tables for the list type
       case ("-- Options for tables --")
       case ("flush                   "); call flush_(stdout)
       case ("put_label               "); call put_(stdout,self%label,int_width=.true.)
       case ("put_n_shells            "); call put_(stdout,self%n_shell)
       case ("put_n_bf                "); call put_(stdout,self%n_bf)
       case ("put_n_prim              "); call put_(stdout,self%n_prim)
       case  default ;      allocate(tonto%known_keywords(15))
       tonto%known_keywords(1) = "-- Regular options --   "
       tonto%known_keywords(2) = "}                       "
       tonto%known_keywords(3) = "analyse_configuration   "
       tonto%known_keywords(4) = "configuration=          "
       tonto%known_keywords(5) = "label=                  "
       tonto%known_keywords(6) = "put                     "
       tonto%known_keywords(7) = "shells=                 "
       tonto%known_keywords(8) = "tonto-style=            "
       tonto%known_keywords(9) = "units=                  "
       tonto%known_keywords(10) = "-- Options for tables --"
       tonto%known_keywords(11) = "flush                   "
       tonto%known_keywords(12) = "put_label               "
       tonto%known_keywords(13) = "put_n_shells            "
       tonto%known_keywords(14) = "put_n_bf                "
       tonto%known_keywords(15) = "put_n_prim              "
       call unknown_(tonto,word,"SLATERBASIS:process_keyword")
       deallocate(tonto%known_keywords)
     end select

   end subroutine

   subroutine read_units(self)
    type(slaterbasis_type) :: self
    ! Read a string which describes the units to be used
    ! The following code is inherited from OBJECT

      call set_default_units_(stdin,next_str_(stdin))

   end subroutine

   subroutine read_junk(self)
    type(slaterbasis_type) :: self
    ! Read in a junk string, useful for ignoring a field
    ! The following code is inherited from OBJECT

      call skip_next_item_(stdin)

   end subroutine

   subroutine read_label(self)
    type(slaterbasis_type) :: self
    ! Read only the basis label
    ! The following code is inherited from type(basis_type)

      call read_(stdin,self%label)

   end subroutine

   subroutine read_shell(self)
    type(slaterbasis_type) :: self
    ! Read a shell

      call read_list_keywords_(self%shell)
      call update_(self)
    ! The following code is inherited from type(basis_type)

   end subroutine

   subroutine read_configuration(self)
    type(slaterbasis_type) :: self
    ! Read in the configuration string

      call read_(stdin,self%configuration)

   end subroutine

   subroutine read_tonto_style(self)
    type(slaterbasis_type) :: self
    ! Create and read a tonto style basis set
      character(128), dimension(:), pointer :: the_keys

      call read_label_(self)
      call read_configuration_(self)
      the_keys => split_(("l_chr= kind= n,z,c*="))
      call set_keys_(self%shell,the_keys)
      call destroy_(the_keys)
      call read_data_(self%shell)
      call update_(self)

   end subroutine

   subroutine analyse_configuration(self)
    type(slaterbasis_type) :: self
    ! Analyse the orbital configuration and set the shell occupancies
      character(128) :: configuration,conf_kind
      character(128), dimension(:), pointer :: split,conf
      logical(kind=kind(.true.)), dimension(:), pointer :: match
      logical(kind=kind(.true.)) :: found,keep
      integer(kind=kind(1)) :: i,j,s,occ,ind

   call ensure_(tonto,self%configuration/=" ","SLATERBASIS:analyse_configuration ... no configuration")
   call ensure_(tonto,associated(self%shell),"SLATERBASIS:analyse_configuration ... no shells")
   call ensure_(tonto,shell_kinds_created_(self),"SLATERBASIS:analyse_configuration ... not all orbital kinds are there")
       ! Split configuration into separate fields
      configuration = self%configuration
      call replace_(configuration,"("," ")
      call replace_(configuration,")"," ")
      split => split_(configuration)
      i = 1
      do  ! loop over shell/orbital configurations
         if (i>size(split)) exit
          ! Now get orbital configurations only
         keep = .false.
         select case (split(i))
            case ("K");   conf => split_(("1S 2"))
            case ("L");   conf => split_(("2S 2 2P 6"))
            case ("M");   conf => split_(("3S 2 3P 6 3D 10"))
            case default; conf => split(i:i+1); keep = .true.
         end select
         j = 1
         do  ! Loop over orbital configurations
            if (j>size(conf)) exit
            conf_kind = conf(j)
            occ  = to_int_(conf(j+1))
             ! Now find the shell with the correct orbital kind
            if (occ>0) then
            found = .false.
            do s = 1,size(self%shell)
               if (all(self%shell(s)%orb_kind/=conf_kind)) cycle
               call create_(match,self%shell(s)%n_orb)
               match = self%shell(s)%orb_kind==conf_kind
               ind = index_of_first_true_element_(match)
               call destroy_(match)
               call ensure_(tonto,associated(self%shell(s)%occupancy),"SLATERBASIS:analyse_configuration ... occupancies have&
& not been created")
               self%shell(s)%occupancy(ind) = occ
               found = .true.
               exit
            end do
            call ensure_(tonto,found,"SLATERBASIS:analyse_configuration ... orbital kind "//trim(conf_kind)//" not found")
            end if
            j = j + 2
         end do
         if (.not. keep) call destroy_(conf)
         i = i + 2
      end do
      call destroy_(split)

   end subroutine

!  ********************
!  Key related routines
!  ********************

   subroutine read_keys(self)
    type(slaterbasis_type) :: self
    ! Read the "keys".
    ! The following code is inherited from OBJECT

     call clear_keys_(self)
     call read_ptr_(stdin,keys)
     call ignore_memory_leak_(tonto,memory_blocks_gained=1)

   end subroutine

   subroutine process_keys(self)
    type(slaterbasis_type) :: self
    ! Process each of the words in the "keys" list.
    ! The following code is inherited from OBJECT
      integer(kind=kind(1)) :: k,l,n_key
      character(128) :: keyword
      character(128), dimension(:), pointer :: internal

      call ensure_(tonto,associated(keys),"SLATERBASIS:process_keys ... no keys")
      n_key = size(keys)
      k = 0
      do
         k = k + 1
         keyword = keys(k)
         if (keyword=="}") exit
         if (keyword=="{") then
            l = index_of_matching_bracket_(keys(k:),"{")
            call ensure_(tonto,l>0,"SLATERBASIS:process_keys ... no matching closing brace, }")
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
    type(slaterbasis_type) :: self
    ! Return .true. if the list-element keys are created.
      logical(kind=kind(.true.)) :: res
    ! The following code is inherited from OBJECT

      res = associated(keys)

   end function

   subroutine set_keys(self,the_keys)
    type(slaterbasis_type) :: self
    ! This is for setting the "keys" externally.
     character(len=*), dimension(:) :: the_keys
    ! The following code is inherited from OBJECT

     call clear_keys_(self)
     call create_copy_(keys,the_keys)
     call ignore_memory_leak_(tonto,memory_blocks_gained=1)

   end subroutine

   subroutine clear_keys(self)
    type(slaterbasis_type) :: self
    ! This is for destroying the "keys" externally.
    ! The following code is inherited from OBJECT

     if (associated(keys)) then
       call destroy_(keys)
       call ignore_memory_leak_(tonto,memory_blocks_gained=-1)
     end if

   end subroutine

   subroutine put_table_footer(self)
    type(slaterbasis_type) :: self
    ! Output a table footer from the list of "keys". NOTE: not all keywords need
    ! contribute to the banner - any unrecognised keyword is skipped.
    ! The following code is inherited from OBJECT

     call dash_(stdout,width=table_width_(self))

   end subroutine

   subroutine put_table_header(self)
    type(slaterbasis_type) :: self
    ! Output a table header from the list of "keys". NOTE: not all keywords need
    ! contribute to the banner - any unrecognised keyword is skipped.
     character(128) :: word
     integer(kind=kind(1)) :: width,k

     call ensure_(tonto,associated(keys),"SLATERBASIS:put_table_header ... no keys")
     width = table_width_(self)
     if (width > 0) then
       call dash_(stdout,width=width)
       do k = 1,size(keys)
         word = keys(k)
         call to_lower_case_(word)
         select case (word)
           case ("put_configuration"); call put_(stdout,"Config.")
           case ("put_label        "); call put_(stdout,"label",int_width=.true.)
           case ("put_n_shells     "); call put_(stdout,"n_shells",int_width=.true.)
           case ("put_n_bf         "); call put_(stdout,"n_bf",int_width=.true.)
           case ("put_n_prim       "); call put_(stdout,"n_prim",int_width=.true.)
           case ("flush            "); call flush_(stdout); exit
         end select
         if (k==size(keys)) then
           call flush_(stdout)  ! In case they didn't write one.
           call warn_(tonto,"SLATERBASIS:put_table_header ... no flush keyword - you may later overrun the output buffer")
         end if
       end do
       call dash_(stdout,width=width)
     end if

   end subroutine

   function table_width(self) result(res)
    type(slaterbasis_type) :: self
    ! Return the table width in characters, based on "keys".  Note that not all
    ! keywords need to contribute to the banner - if a keyword is not recognised,
    ! then it is skipped.
     integer(kind=kind(1)) :: res
     integer(kind=kind(1)) :: int_dash,real_dash,k
     character(128) :: word

     call ensure_(tonto,associated(keys),"SLATERBASIS:table_width ... no keys")
     int_dash = 0
     real_dash = 0
     do k = 1,size(keys)
       word = keys(k)
       call to_lower_case_(word)
       select case (word)
         case ("}                ");  ! exit surrounding loop
         case ("put_configuration"); real_dash = real_dash + 1
         case ("put_label        "); int_dash = int_dash + 1
         case ("put_n_shells     "); int_dash = int_dash + 1
         case ("put_n_bf         "); int_dash = int_dash + 1
         case ("put_n_prim       "); int_dash = int_dash + 1
         case ("flush            "); exit
         case default
       end select
     end do
     res = int_dash * stdout%int_width + real_dash * stdout%real_width

   end function

!  ***************
!  Inquiry methods
!  ***************

   function same_as(self,b) result(res)
    type(slaterbasis_type) :: self
    ! Return .true. if the basis set "self" is the same as "b". Only the
    ! shell vector is compared to see if they are "really" the same.
      intent(in) :: self
      type(slaterbasis_type), intent(in) :: b
      logical(kind=kind(.true.)) :: res
    ! The following code is inherited from type(basis_type)

      res = same_as_(self%shell,b%shell)

   end function

   function max_n_orb(self) result(res)
    type(slaterbasis_type) :: self
    ! Return the maximum number of contracted orbitals in any one shell.
      intent(in) :: self
      integer(kind=kind(1)) :: res

      res = maxval(self%shell%n_orb)

   end function

   pure function no_of_shells(self) result(res)
    type(slaterbasis_type) :: self
    ! Work out and return the number of shells in the basis set
      intent(in) :: self
      integer(kind=kind(1)) :: res
    ! The following code is inherited from type(basis_type)

      res = no_of_shells_(self%shell)

   end function

   pure function no_of_basis_functions(self) result(res)
    type(slaterbasis_type) :: self
    ! Work out and return the number of basis functions in the basis set
      intent(in) :: self
      integer(kind=kind(1)) :: res
    ! The following code is inherited from type(basis_type)

      res = no_of_basis_functions_(self%shell)

   end function

   pure function no_of_primitives(self) result(res)
    type(slaterbasis_type) :: self
    ! Work out and return the number of primitives in the basis set
      intent(in) :: self
      integer(kind=kind(1)) :: res
    ! The following code is inherited from type(basis_type)

      res = no_of_primitives_(self%shell)

   end function

   pure function min_exponent(self) result(res)
    type(slaterbasis_type) :: self
    ! Return the minimum exponent in the basis.
     intent(in) :: self
     real(kind=kind(1.0d0)) :: res
    ! The following code is inherited from type(basis_type)

     res = min_exponent_(self%shell)

   end function

   function shell_kinds_created(self) result(res)
    type(slaterbasis_type) :: self
    ! Return .true. if all the .shell.orb_kind vectors are there.
      intent(in) :: self
      logical(kind=kind(.true.)) :: res
      integer(kind=kind(1)) :: i

      if (.not. associated(self%shell)) then
         res = .false.
      else
         res = .true.
         do i = 1,size(self%shell)
            if (associated(self%shell(i)%orb_kind)) cycle
            res = .false.
            exit
         end do
      end if

   end function

!  **************
!  Output methods
!  **************

   subroutine put(self)
    type(slaterbasis_type) :: self
    ! Put out the basis information to file "stdout"

      call flush_(stdout)
      call show_(stdout,"Slater basis set : ",trim(self%label))
      call flush_(stdout)
      call show_(stdout,"Configuration          =",self%configuration)
      call show_(stdout,"No. of shells          =",self%n_shell)
      call show_(stdout,"No. of basis functions =",self%n_bf)
      call show_(stdout,"No. of primitives      =",self%n_prim)
      if (associated(self%shell)) call put_(self%shell)

   end subroutine

!  ******************
!  Density evaluation
!  ******************

   subroutine make_density_grid(self,density_grid,pt,pos)
    type(slaterbasis_type) :: self
    ! Work out the electron "density_grid" on a set of points "pt", assuming the
    ! orbitals are at position "pos".
      intent(in) :: self
      real(kind=kind(1.0d0)), dimension(:), intent(out) :: density_grid
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: pt
      real(kind=kind(1.0d0)), dimension(3), intent(in) :: pos

   call ensure_(tonto,size(density_grid)==size(pt,1),"SLATERBASIS:make_density_grid ... inconsistent number of points")
      if (associated(self%interpolator)) then
         call make_interpolated_density_grid_(self,density_grid,pt,pos)
      else
         call make_normal_density_grid_(self,density_grid,pt,pos)
      end if

   end subroutine

   subroutine make_normal_density_grid(self,density_grid,pt,pos)
    type(slaterbasis_type) :: self
    ! Make the normal (uninterpolated) "density_grid" for the supplied points
    ! "pt" from the real slater atomic orbitals, as fitted by coppens, assuming
    ! the orbital is at position "pos".
     intent(in) :: self
     real(kind=kind(1.0d0)), dimension(:), intent(out) :: density_grid
     real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: pt
     real(kind=kind(1.0d0)), dimension(3), intent(in) :: pos
     real(kind=kind(1.0d0)), dimension(:), pointer :: R
     integer(kind=kind(1)) :: n_pt,n
     real(kind=kind(1.0d0)) :: x, y, z

   call ensure_(tonto,size(pt,2)==3,"SLATERBASIS:make_normal_density_grid ... wrong dimension for points array")
   call ensure_(tonto,size(density_grid)==size(pt,1),"SLATERBASIS:make_normal_density_grid ... inconsistent number of points"&
&)
   call ensure_(tonto,associated(self%shell),"SLATERBASIS:make_normal_density_grid ... no shell vector")
     n_pt = size(pt,1)
     call create_(R,n_pt)
     do n = 1,n_pt
        x = pt(n,1) - pos(1)
        y = pt(n,2) - pos(2)
        z = pt(n,3) - pos(3)
        R(n) = sqrt(x*x + y*y + z*z)
     end do
      ! Now get the density values
     density_grid = densities_at_radii_(self%shell,R)
     call destroy_(R)

   end subroutine

   subroutine make_interpolated_density_grid(self,density_grid,pt,pos)
    type(slaterbasis_type) :: self
    ! Make the interpolated "density_grid" for the supplied points "pt" from the real
    ! slater atomic orbitals, as fitted by coppens, assuming the orbitals are at
    ! position "pos".
     intent(in) :: self
     real(kind=kind(1.0d0)), dimension(:), intent(out) :: density_grid
     real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: pt
     real(kind=kind(1.0d0)), dimension(3), intent(in) :: pos
     real(kind=kind(1.0d0)), dimension(:), pointer :: R
     integer(kind=kind(1)) :: n_pt,n
     real(kind=kind(1.0d0)) :: x, y, z

   call ensure_(tonto,size(pt,2)==3,"SLATERBASIS:make_interpolated_density_grid ... wrong dimension for points array")
   call ensure_(tonto,size(density_grid)==size(pt,1),"SLATERBASIS:make_interpolated_density_grid ... inconsistent number of p&
&oints")
   call ensure_(tonto,associated(self%interpolator),"SLATERBASIS:make_interpolated_density_grid ... no interpolator defined!"&
&)
     n_pt = size(pt,1)
     call create_(R,n_pt)
     do n = 1,n_pt
        x = pt(n,1) - pos(1)
        y = pt(n,2) - pos(2)
        z = pt(n,3) - pos(3)
        R(n) = sqrt(x*x + y*y + z*z)
     end do
      ! Now get the interpolated density values
     density_grid = values_for_(self%interpolator,R)
     call destroy_(R)

   end subroutine

   subroutine make_interpolator(self)
    type(slaterbasis_type) :: self
    ! Make the interpolator for the coppens atom density

   call ensure_(tonto,associated(self%shell),"SLATERBASIS:make_interpolator ... no coppens orbitals defined!")
     call destroy_(self%interpolator)
     call create_(self%interpolator)
     self%interpolator%interp_kind = "logarithmic"
     call set_saved_self_(self%shell)  ! Used by function below
     call set_even_spaced_data_(self%interpolator,first=0.0d0,spacing=0.05d0,length=20.0d0, &
        func=density_value_at_radius,tol=10.0d0**(-9))
   end subroutine

end
