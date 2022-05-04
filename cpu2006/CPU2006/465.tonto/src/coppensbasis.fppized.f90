!-------------------------------------------------------------------------------
!
! COPPENSBASIS: For a Coppens style fitted relativistic atomic orbital basis.
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
! $Id: coppensbasis.foo,v 1.2.2.13 2004/04/21 09:12:54 reaper Exp $
!
!-------------------------------------------------------------------------------

module COPPENSBASIS_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

   use REALVEC_MODULE, only: create_
   use REALVEC_MODULE, only: destroy_

   use COPPENSORBITAL_MODULE, only: make_values_at_opt_

   use COPPENSORBITALVEC_MODULE, only: density_at_radius_
   use COPPENSORBITALVEC_MODULE, only: set_saved_self_
   use COPPENSORBITALVEC_MODULE, only: unnormalise_
   use COPPENSORBITALVEC_MODULE, only: maximum_orbital_n_value_
   use COPPENSORBITALVEC_MODULE, only: create_copy_
   use COPPENSORBITALVEC_MODULE, only: same_as_
   use COPPENSORBITALVEC_MODULE, only: set_keys_
   use COPPENSORBITALVEC_MODULE, only: renormalise_
   use COPPENSORBITALVEC_MODULE, only: read_list_keywords_
   use COPPENSORBITALVEC_MODULE, only: read_data_
   use COPPENSORBITALVEC_MODULE, only: destroy_

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
   use TEXTFILE_MODULE, only: tab_
   use TEXTFILE_MODULE, only: read_
   use TEXTFILE_MODULE, only: flush_
   use TEXTFILE_MODULE, only: read_ptr_
   use TEXTFILE_MODULE, only: reverted_
   use TEXTFILE_MODULE, only: dash_

   use STR_MODULE, only: split_
   use STR_MODULE, only: to_lower_case_
   use STR_MODULE, only: includes_

   use STRVEC_MODULE, only: create_copy_
   use STRVEC_MODULE, only: destroy_
   use STRVEC_MODULE, only: index_of_matching_bracket_

   use BINVEC_MODULE, only: create_
   use BINVEC_MODULE, only: destroy_
   use BINVEC_MODULE, only: index_of_first_true_element_

   use INTERPOLATOR_MODULE, only: set_even_spaced_data_
   use INTERPOLATOR_MODULE, only: create_
   use INTERPOLATOR_MODULE, only: values_for_
   use INTERPOLATOR_MODULE, only: value_for_
   use INTERPOLATOR_MODULE, only: create_copy_
   use INTERPOLATOR_MODULE, only: destroy_
   use COPPENSORBITALVEC_MODULE, only: density_value_at_radius

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

   public    read_orbitals_
   interface read_orbitals_
      module procedure read_orbitals
   end interface

   public    same_as_
   interface same_as_
      module procedure same_as
   end interface

   public    maximum_basis_n_value_
   interface maximum_basis_n_value_
      module procedure maximum_basis_n_value
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

   public    no_of_orbitals_
   interface no_of_orbitals_
      module procedure no_of_orbitals
   end interface

   public    unnormalise_
   interface unnormalise_
      module procedure unnormalise
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

   public    renormalise_
   interface renormalise_
      module procedure renormalise
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

   public    put_table_
   interface put_table_
      module procedure put_table
   end interface

   public    keys_created_
   interface keys_created_
      module procedure keys_created
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

   public    density_at_radius_
   interface density_at_radius_
      module procedure density_at_radius
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

   public    read_units_
   interface read_units_
      module procedure read_units
   end interface

   character(128), dimension(:), pointer, private :: keys => NULL()

contains

!****************************
! Create and Destroy Routines
!****************************

   subroutine create(self)
    type(coppensbasis_type) :: self
    ! Create an object
      pointer :: self
    ! The following code is inherited from OBJECT

      nullify(self)
      allocate(self)

      call nullify_ptr_part_(self)
      call set_defaults_(self)

   end subroutine

   subroutine destroy(self)
    type(coppensbasis_type) :: self
    ! Destroy an object
      pointer :: self
    ! The following code is inherited from OBJECT

      if (associated(self)) then
        call destroy_ptr_part_(self)
        deallocate(self)

      end if

   end subroutine

   subroutine nullify_ptr_part(self)
    type(coppensbasis_type) :: self
    ! Nullify the pointer parts of self

      nullify(self%orbital)
      nullify(self%interpolator)

   end subroutine

   subroutine destroy_ptr_part(self)
    type(coppensbasis_type) :: self
    ! Destroy the pointer parts of self

      call destroy_(self%orbital)
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
    type(coppensbasis_type) :: self
    ! Create a copy of the basis "b".
     type(coppensbasis_type), intent(in) :: b
     pointer :: self

     call create_(self)
     call copy_(self,b)

   end subroutine

   subroutine copy(self,b)
    type(coppensbasis_type) :: self
    ! Copy a basis "b" to "self". Make sure pointer parts of self are first
    ! destroyed or nullified, as you want.
      type(coppensbasis_type), intent(in) :: b

      self = b
      if (associated(b%orbital)) call create_copy_(self%orbital,b%orbital)
      if (associated(b%interpolator)) call create_copy_(self%interpolator,b%interpolator)

   end subroutine

   subroutine set_defaults(self)
    type(coppensbasis_type) :: self
    ! Create and set up a default basis set

      self%label   = "?"
      self%n_orb   = 0
      self%n_prim  = 0

   end subroutine

   subroutine update(self)
    type(coppensbasis_type) :: self
    ! Update the shell data, if it exists

      if (.not. associated(self%orbital)) then;   return; end if
      self%n_orb   = no_of_orbitals_(self)
      self%n_prim  = no_of_primitives_(self)

   end subroutine

   subroutine set_label(self,label)
    type(coppensbasis_type) :: self
    ! Set the basis label
      character(128) :: label

      self%label = label

   end subroutine

   subroutine resolve_by_label(self,label,basis,clobber,found)
    type(coppensbasis_type) :: self
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
      type(coppensbasis_type), dimension(:), pointer :: basis
      logical(kind=kind(.true.)), optional :: clobber,found
      integer(kind=kind(1)) :: b
      logical(kind=kind(.true.)), dimension(:), pointer :: check
      logical(kind=kind(.true.)) :: fnd

   call ensure_(tonto,associated(basis),"COPPENSBASIS:resolve_by_label ... no basis set")
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
      else; call ensure_(tonto,fnd,"COPPENSBASIS:resolve_by_label ... unknown basis label, "// trim(label))
      end if

   end subroutine

!  ************
!  I/O Routines
!  ************

   recursive subroutine read_keywords(self)
    type(coppensbasis_type) :: self
    ! Read data from "stdin" using keyword style input.
    ! The following code is inherited from OBJECT
     character(128) :: word

     call ensure_(tonto,next_item_(stdin)=="{","COPPENSBASIS:read_keywords ... expecting open bracket symbol, {")
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
    type(coppensbasis_type) :: self
    ! Process command "keyword". Any required data needed by the "keyword" is
    ! inputted from "stdin".
     character(*), intent(in) :: keyword
     character(128) :: word

     word = keyword
     call to_lower_case_(word)
     select case (word)
       case ("}            ");  ! exit surrounding loop
       case ("label=       "); call read_label_(self)
       case ("orbitals=    "); call read_orbitals_(self)
       case ("put          "); call put_(self)
       case ("put_table    "); call put_table_(self)
       case ("tonto-style= "); call read_tonto_style_(self)
       case ("units=       "); call read_units_(self)
        ! These are only for making custom tables for the list type
       case ("put_label    "); call put_(stdout,self%label,int_width=.true.)
       case ("put_n_orb    "); call put_(stdout,self%n_orb)
       case ("put_n_prim   "); call put_(stdout,self%n_prim)
       case ("flush        "); call flush_(stdout)
       case  default ;      allocate(tonto%known_keywords(11))
       tonto%known_keywords(1) = "}            "
       tonto%known_keywords(2) = "label=       "
       tonto%known_keywords(3) = "orbitals=    "
       tonto%known_keywords(4) = "put          "
       tonto%known_keywords(5) = "put_table    "
       tonto%known_keywords(6) = "tonto-style= "
       tonto%known_keywords(7) = "units=       "
       tonto%known_keywords(8) = "put_label    "
       tonto%known_keywords(9) = "put_n_orb    "
       tonto%known_keywords(10) = "put_n_prim   "
       tonto%known_keywords(11) = "flush        "
       call unknown_(tonto,word,"COPPENSBASIS:process_keyword")
       deallocate(tonto%known_keywords)
     end select

   end subroutine

   subroutine read_units(self)
    type(coppensbasis_type) :: self
    ! Read a string which describes the units to be used
    ! The following code is inherited from OBJECT

      call set_default_units_(stdin,next_str_(stdin))

   end subroutine

   subroutine read_junk(self)
    type(coppensbasis_type) :: self
    ! Read in a junk string, useful for ignoring a field
    ! The following code is inherited from OBJECT

      call skip_next_item_(stdin)

   end subroutine

   subroutine read_label(self)
    type(coppensbasis_type) :: self
    ! Read only the basis label

      call read_(stdin,self%label)

   end subroutine

   subroutine read_orbitals(self)
    type(coppensbasis_type) :: self
    ! Read a list of orbitals

      call read_list_keywords_(self%orbital)
      call update_(self)

   end subroutine

   subroutine read_tonto_style(self)
    type(coppensbasis_type) :: self
    ! Create and read a tonto style basis set

      call read_label_(self)
      call set_keys_(self%orbital,(/"kind= ","occ=  ","n_fun=","n,c,z="/))
      call read_data_(self%orbital)
      call update_(self)

   end subroutine

   subroutine put(self)
    type(coppensbasis_type) :: self
    ! Put out the basis information to file "stdout"

      call flush_(stdout)
      call show_(stdout,"Coppens basis set : ",trim(self%label))
      call flush_(stdout)
      call show_(stdout,"No. of orbitals        =",self%n_orb)
      call show_(stdout,"No. of primitives      =",self%n_prim)
      call put_table_(self)

   end subroutine

   subroutine put_table(self)
    type(coppensbasis_type) :: self
    ! Put out the basis information to file "stdout"
      integer(kind=kind(1)) :: i,j

      call flush_(stdout)
      call dash_(stdout,int_fields=3,real_fields=3)
      call put_(stdout,"Kind",int_width=.true.)
      call put_(stdout,"Orb #",int_width=.true.)
      call put_(stdout,"Prim",int_width=.true.)
      call put_(stdout,"N",int_width=.true.)
      call put_(stdout,"Exponent")
      call put_(stdout,"Coeff")
      call flush_(stdout)
      call dash_(stdout,int_fields=3,real_fields=3)
      do i = 1,self%n_orb
         do j = 1,self%orbital(i)%n_fun
            if (j==1) then
               call put_(stdout,self%orbital(i)%orb_kind,int_width=.true.)
               call put_(stdout,i)
            else
               call tab_(stdout,int_fields=2)
            end if
            call put_(stdout,j)
            call put_(stdout,self%orbital(i)%n(j))
            call put_(stdout,self%orbital(i)%z(j))
            call put_(stdout,self%orbital(i)%c(j))
            call flush_(stdout)
         end do
      end do
      call dash_(stdout,int_fields=3,real_fields=3)

   end subroutine

!  ********************
!  Key related routines
!  ********************

   subroutine read_keys(self)
    type(coppensbasis_type) :: self
    ! Read the "keys".
    ! The following code is inherited from OBJECT

     call clear_keys_(self)
     call read_ptr_(stdin,keys)
     call ignore_memory_leak_(tonto,memory_blocks_gained=1)

   end subroutine

   subroutine process_keys(self)
    type(coppensbasis_type) :: self
    ! Process each of the words in the "keys" list.
    ! The following code is inherited from OBJECT
      integer(kind=kind(1)) :: k,l,n_key
      character(128) :: keyword
      character(128), dimension(:), pointer :: internal

      call ensure_(tonto,associated(keys),"COPPENSBASIS:process_keys ... no keys")
      n_key = size(keys)
      k = 0
      do
         k = k + 1
         keyword = keys(k)
         if (keyword=="}") exit
         if (keyword=="{") then
            l = index_of_matching_bracket_(keys(k:),"{")
            call ensure_(tonto,l>0,"COPPENSBASIS:process_keys ... no matching closing brace, }")
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
    type(coppensbasis_type) :: self
    ! Return .true. if the list-element keys are created.
      logical(kind=kind(.true.)) :: res
    ! The following code is inherited from OBJECT

      res = associated(keys)

   end function

   subroutine set_keys(self,the_keys)
    type(coppensbasis_type) :: self
    ! This is for setting the "keys" externally.
     character(len=*), dimension(:) :: the_keys
    ! The following code is inherited from OBJECT

     call clear_keys_(self)
     call create_copy_(keys,the_keys)
     call ignore_memory_leak_(tonto,memory_blocks_gained=1)

   end subroutine

   subroutine clear_keys(self)
    type(coppensbasis_type) :: self
    ! This is for destroying the "keys" externally.
    ! The following code is inherited from OBJECT

     if (associated(keys)) then
       call destroy_(keys)
       call ignore_memory_leak_(tonto,memory_blocks_gained=-1)
     end if

   end subroutine

   subroutine put_table_footer(self)
    type(coppensbasis_type) :: self
    ! Output a table footer from the list of "keys". NOTE: not all keywords need
    ! contribute to the banner - any unrecognised keyword is skipped.
    ! The following code is inherited from OBJECT

     call dash_(stdout,width=table_width_(self))

   end subroutine

   subroutine put_table_header(self)
    type(coppensbasis_type) :: self
    ! Output a table header from the list of "keys". NOTE: not all keywords need
    ! contribute to the banner - any unrecognised keyword is skipped.
     character(128) :: word
     integer(kind=kind(1)) :: width,k

   call ensure_(tonto,associated(keys),"COPPENSBASIS:put_table_header ... no keys")
     width = table_width_(self)
     if (width > 0) then
       call dash_(stdout,width=width)
       do k = 1,size(keys)
         word = keys(k)
         call to_lower_case_(word)
         select case (word)
           case ("put_label   "); call put_(stdout,"label",int_width=.true.)
           case ("put_n_orb   "); call put_(stdout,"n_orb",int_width=.true.)
           case ("put_n_prim  "); call put_(stdout,"n_prim",int_width=.true.)
           case ("flush       "); call flush_(stdout); exit
         end select
         if (k==size(keys)) then
           call flush_(stdout)  ! In case they didn't write one.
           call warn_(tonto,"COPPENSBASIS:put_table_header ... no flush keyword - you may later overrun the output buffer")
         end if
       end do
       call dash_(stdout,width=width)
     end if

   end subroutine

   function table_width(self) result(res)
    type(coppensbasis_type) :: self
    ! Return the table width in characters, based on "keys".  Note that not all
    ! keywords need to contribute to the banner - if a keyword is not recognised,
    ! then it is skipped.
     integer(kind=kind(1)) :: res
     character(128) :: word
     integer(kind=kind(1)) :: int_dash,real_dash,k

     call ensure_(tonto,associated(keys),"COPPENSBASIS:table_width ... no keys")
     int_dash = 0
     real_dash = 0
     do k = 1,size(keys)
       word = keys(k)
       call to_lower_case_(word)
       select case (word)
         case ("}           ");  ! exit surrounding loop
         case ("put_label   "); int_dash = int_dash + 1
         case ("put_n_orb   "); int_dash = int_dash + 1
         case ("put_n_prim  "); int_dash = int_dash + 1
         case ("flush       "); exit
         case default
       end select
     end do
     res = int_dash * stdout%int_width + real_dash * stdout%real_width

   end function

!  *******
!  Methods
!  *******

   function same_as(self,b) result(res)
    type(coppensbasis_type) :: self
    ! Return .true. if the basis set "self" is the same as "b". Only the
    ! orbital vector is compared to see if they are "really" the same.
      intent(in) :: self
      type(coppensbasis_type), intent(in) :: b
      logical(kind=kind(.true.)) :: res

      res = same_as_(self%orbital,b%orbital)

   end function

   pure function no_of_orbitals(self) result(res)
    type(coppensbasis_type) :: self
    ! Work out and return the number of orbitals in the basis set
      intent(in) :: self
      integer(kind=kind(1)) :: res
      if (associated(self%orbital)) then; res = size(self%orbital)
      else;                       res = 0
      end if

   end function

   pure function no_of_primitives(self) result(res)
    type(coppensbasis_type) :: self
    ! Work out and return the number of primitive fitting functions in the basis ! set
      intent(in) :: self
      integer(kind=kind(1)) :: res
      integer(kind=kind(1)) :: i
      res = 0
      if (.not. associated(self%orbital)) then;  return; end if
      do i = 1,no_of_orbitals_(self)
         res = res + self%orbital(i)%n_fun
      end do

   end function

   function maximum_basis_n_value(self) result(res)
    type(coppensbasis_type) :: self
    ! Returns the maximum n value over all orbitals in every basis set of the
    ! vector
     intent(in) :: self
     integer(kind=kind(1)) :: res

     if (associated(self%orbital)) then; res = maximum_orbital_n_value_(self%orbital)
     else;                       res = 0
     end if

   end function

   pure function min_exponent(self) result(res)
    type(coppensbasis_type) :: self
    ! Return the minimum exponent in the basis.
     intent(in) :: self
     real(kind=kind(1.0d0)) :: res
     real(kind=kind(1.0d0)) :: tmp
     integer(kind=kind(1)) :: i

     res = 0.0d0
     do i= 1,self%n_orb
       tmp = minval(self%orbital(i)%z)
       if (tmp < res) res = tmp
     end do

   end function

!  ******************
!  Density evaluation
!  ******************

   subroutine make_density_grid(self,density_grid,pt,pos)
    type(coppensbasis_type) :: self
    ! Work out the electron "density_grid" on a set of points "pt", assuming the
    ! orbitals are at position "pos".
      intent(in) :: self
      real(kind=kind(1.0d0)), dimension(:), intent(out) :: density_grid
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: pt
      real(kind=kind(1.0d0)), dimension(3), intent(in) :: pos

   call ensure_(tonto,size(density_grid)==size(pt,1),"COPPENSBASIS:make_density_grid ... inconsistent number of points")
      if (associated(self%interpolator)) then
         call make_interpolated_density_grid_(self,density_grid,pt,pos)
      else
         call make_normal_density_grid_(self,density_grid,pt,pos)
      end if

   end subroutine

   subroutine make_normal_density_grid(self,density_grid,pt,pos)
    type(coppensbasis_type) :: self
    ! Make the normal (uninterpolated) "density_grid" for the supplied points
    ! "pt" from the real slater atomic orbitals, as fitted by coppens, assuming
    ! the orbital is at position "pos".
     intent(in) :: self
     real(kind=kind(1.0d0)), dimension(:), intent(out) :: density_grid
     real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: pt
     real(kind=kind(1.0d0)), dimension(3), intent(in) :: pos
     real(kind=kind(1.0d0)), dimension(:), pointer :: ORB
     integer(kind=kind(1)) :: n

   call ensure_(tonto,size(pt,2)==3,"COPPENSBASIS:make_normal_density_grid ... wrong dimension for points array")
   call ensure_(tonto,size(density_grid)==size(pt,1),"COPPENSBASIS:make_normal_density_grid ... inconsistent number of points&
&")
   call ensure_(tonto,associated(self%orbital),"COPPENSBASIS:make_normal_density_grid ... no orbital vector")
     call create_(ORB,size(pt,1))
     density_grid = 0.0d0
     do n = 1,size(self%orbital)
        call make_values_at_opt_(self%orbital(n),pt,pos,ORB)
        density_grid = density_grid  + self%orbital(n)%occupancy*ORB*ORB
     end do
     call destroy_(ORB)

   end subroutine

   subroutine make_interpolated_density_grid(self,density_grid,pt,pos)
    type(coppensbasis_type) :: self
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

   call ensure_(tonto,size(pt,2)==3,"COPPENSBASIS:make_interpolated_density_grid ... wrong dimension for points array")
   call ensure_(tonto,size(density_grid)==size(pt,1),"COPPENSBASIS:make_interpolated_density_grid ... inconsistent number of &
&points")
   call ensure_(tonto,associated(self%interpolator),"COPPENSBASIS:make_interpolated_density_grid ... no coppens interpolator &
&defined!")
     n_pt = size(pt,1)
     call create_(R,n_pt)
     do n = 1,n_pt
        x = pt(n,1) - pos(1)
        y = pt(n,2) - pos(2)
        z = pt(n,3) - pos(3)
        R(n) = sqrt(x*x + y*y + z*z)
     end do
      ! Now get the interpolated values
     density_grid = values_for_(self%interpolator,R)
     call destroy_(R)

   end subroutine

   function density_at_radius(self,R) result(res)
    type(coppensbasis_type) :: self
    ! Work out the electron density at radius "R" from the orbitals.
      real(kind=kind(1.0d0)) :: R,res

      if (associated(self%interpolator)) then
         res = value_for_(self%interpolator,R)
      else
         res = density_at_radius_(self%orbital,R)
      end if

   end function

   subroutine make_interpolator(self)
    type(coppensbasis_type) :: self
    ! Make the interpolator for the coppens atom density

   call ensure_(tonto,associated(self%orbital),"COPPENSBASIS:make_interpolator ... no coppens orbitals defined!")
     call destroy_(self%interpolator)
     call create_(self%interpolator)
     self%interpolator%interp_kind = "logarithmic"
     call set_saved_self_(self%orbital)  ! Used by function below
     call set_even_spaced_data_(self%interpolator,first=0.0d0,spacing=0.05d0,length=20.0d0, &
        func=density_value_at_radius,tol=10.0d0**(-9))
   end subroutine

   subroutine unnormalise(self)
    type(coppensbasis_type) :: self
    ! Set the value of the orbital coefficients to correspond to un-normalised
    ! Slater functions -- assuming they are normalised. This saves computation.

      if (associated(self%orbital)) call unnormalise_(self%orbital)

   end subroutine

   subroutine renormalise(self)
    type(coppensbasis_type) :: self
    ! Set the value of the orbitals coefficients to correspond to normalised
    ! Slater functions --- assuming they are w.r.t. unnormalised functions.

      if (associated(self%orbital)) call renormalise_(self%orbital)

   end subroutine

end
