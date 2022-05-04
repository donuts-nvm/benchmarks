!-------------------------------------------------------------------------------
!
! BASIS: For gaussian basis sets
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
! $Id: basis.foo,v 1.17.2.8 2003/11/13 05:34:14 reaper Exp $
!
!-------------------------------------------------------------------------------

module BASIS_MODULE

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
   use TEXTFILE_MODULE, only: show_
   use TEXTFILE_MODULE, only: revert_
   use TEXTFILE_MODULE, only: put_
   use TEXTFILE_MODULE, only: tab_
   use TEXTFILE_MODULE, only: read_
   use TEXTFILE_MODULE, only: flush_
   use TEXTFILE_MODULE, only: read_ptr_
   use TEXTFILE_MODULE, only: reverted_
   use TEXTFILE_MODULE, only: dash_

   use STRVEC_MODULE, only: create_copy_
   use STRVEC_MODULE, only: index_of_matching_bracket_
   use STRVEC_MODULE, only: destroy_

   use SHELLVEC_MODULE, only: no_of_basis_functions_
   use SHELLVEC_MODULE, only: unnormalise_
   use SHELLVEC_MODULE, only: create_copy_
   use SHELLVEC_MODULE, only: no_of_shells_
   use SHELLVEC_MODULE, only: same_as_
   use SHELLVEC_MODULE, only: set_keys_
   use SHELLVEC_MODULE, only: no_of_primitives_
   use SHELLVEC_MODULE, only: renormalise_
   use SHELLVEC_MODULE, only: min_exponent_
   use SHELLVEC_MODULE, only: read_list_keywords_
   use SHELLVEC_MODULE, only: read_data_
   use SHELLVEC_MODULE, only: destroy_

   use SHELL_MODULE, only: n_prim_
   use SHELL_MODULE, only: l_chr_
   use SHELL_MODULE, only: make_contraction_matrix_

   use BINVEC_MODULE, only: create_
   use BINVEC_MODULE, only: destroy_
   use BINVEC_MODULE, only: index_of_first_true_element_

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

   public    contraction_matrix_
   interface contraction_matrix_
      module procedure contraction_matrix
   end interface

   public    unnormalise_
   interface unnormalise_
      module procedure unnormalise
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

   public    create_copy_
   interface create_copy_
      module procedure create_copy
   end interface

   public    keys_created_
   interface keys_created_
      module procedure keys_created
   end interface

   public    put_table_
   interface put_table_
      module procedure put_table
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

   public    set_label_
   interface set_label_
      module procedure set_label
   end interface

   public    no_of_basis_functions_
   interface no_of_basis_functions_
      module procedure no_of_basis_functions
   end interface

   public    read_units_
   interface read_units_
      module procedure read_units
   end interface

   public    read_gamess_us_
   interface read_gamess_us_
      module procedure read_gamess_us
   end interface

   character(128), dimension(:), pointer, private :: keys => NULL()

contains

!*******************************************************************************
!                             Create and Destroy Routines
!*******************************************************************************

   subroutine create(self)
    type(basis_type) :: self
    ! Create a basis object
      pointer :: self

      nullify(self)
      allocate(self)

      call nullify_ptr_part_(self)
      call set_defaults_(self)

   end subroutine

   subroutine destroy(self)
    type(basis_type) :: self
    ! Destroy a basis object
      pointer :: self

      if (.not. associated(self)) then;   return; end if
      call destroy_ptr_part_(self)

      deallocate(self)

   end subroutine

   subroutine nullify_ptr_part(self)
    type(basis_type) :: self
    ! Nullify the shell parts of self

      nullify(self%shell)

   end subroutine

   subroutine destroy_ptr_part(self)
    type(basis_type) :: self
    ! Destroy the shell parts of self

      call destroy_(self%shell)

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
    type(basis_type) :: self
    ! Create a copy of the basis "b".
     type(basis_type), intent(in) :: b
     pointer :: self

     call create_(self)
     call copy_(self,b)

   end subroutine

   subroutine copy(self,b)
    type(basis_type) :: self
    ! Copy a basis "b" to "self". Make sure pointer parts are first
    ! destroyed or nullified, as you want.
      type(basis_type), intent(in) :: b

      self = b
      if (associated(b%shell)) call create_copy_(self%shell,b%shell)

   end subroutine

   subroutine set_defaults(self)
    type(basis_type) :: self
    ! Create and set up a default basis set

      self%label   = " "
      self%n_shell = 0
      self%n_bf    = 0
      self%n_prim  = 0

   end subroutine

   subroutine update(self)
    type(basis_type) :: self
    ! Update the shell data, if it exists

      if (.not. associated(self%shell)) then;   return; end if
      self%n_shell = no_of_shells_(self)
      self%n_bf    = no_of_basis_functions_(self)
      self%n_prim  = no_of_primitives_(self)

   end subroutine

   subroutine set_label(self,label)
    type(basis_type) :: self
    ! Set the basis label
      character(128) :: label

      self%label = label

   end subroutine

   subroutine resolve_by_label(self,label,basis,clobber,found)
    type(basis_type) :: self
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
      type(basis_type), dimension(:), pointer :: basis
      logical(kind=kind(.true.)), optional :: clobber,found
      integer(kind=kind(1)) :: b
      logical(kind=kind(.true.)), dimension(:), pointer :: check
      logical(kind=kind(.true.)) :: fnd

   call ensure_(tonto,associated(basis),"BASIS:resolve_by_label ... no basis set")
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
      else; call ensure_(tonto,fnd,"BASIS:resolve_by_label ... unknown basis label, "// trim(label))
      end if

   end subroutine

!  ************
!  I/O Routines
!  ************

   recursive subroutine read_keywords(self)
    type(basis_type) :: self
    ! Read data from "stdin" using keyword style input.
    ! The following code is inherited from OBJECT
     character(128) :: word

     call ensure_(tonto,next_item_(stdin)=="{","BASIS:read_keywords ... expecting open bracket symbol, {")
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
    type(basis_type) :: self
    ! Process command "keyword". Any required data needed by the "keyword" is
    ! inputted from "stdin".
     character(*), intent(in) :: keyword
     character(128) :: word

     word = keyword
     call to_lower_case_(word)
     select case (word)
       case ("}            ");  ! exit surrounding loop
       case ("gamess-us=   "); call read_gamess_us_(self)
       case ("label=       "); call read_label_(self)
       case ("shells=      "); call read_shell_(self)
       case ("units=       "); call read_units_(self)
       case ("put          "); call put_(self)
       case ("put_table    "); call put_table_(self)
        ! These are only for making custom tables for the list type
       case ("put_label    "); call put_(stdout,self%label,int_width=.true.)
       case ("put_n_shells "); call put_(stdout,self%n_shell)
       case ("put_n_bf     "); call put_(stdout,self%n_bf)
       case ("put_n_prim   "); call put_(stdout,self%n_prim)
       case ("tonto-style= "); call read_tonto_style_(self)
       case ("flush        "); call flush_(stdout)
       case  default ;      allocate(tonto%known_keywords(13))
       tonto%known_keywords(1) = "}            "
       tonto%known_keywords(2) = "gamess-us=   "
       tonto%known_keywords(3) = "label=       "
       tonto%known_keywords(4) = "shells=      "
       tonto%known_keywords(5) = "units=       "
       tonto%known_keywords(6) = "put          "
       tonto%known_keywords(7) = "put_table    "
       tonto%known_keywords(8) = "put_label    "
       tonto%known_keywords(9) = "put_n_shells "
       tonto%known_keywords(10) = "put_n_bf     "
       tonto%known_keywords(11) = "put_n_prim   "
       tonto%known_keywords(12) = "tonto-style= "
       tonto%known_keywords(13) = "flush        "
       call unknown_(tonto,word,"BASIS:process_keyword")
       deallocate(tonto%known_keywords)
     end select

   end subroutine

   subroutine read_units(self)
    type(basis_type) :: self
    ! Read a string which describes the units to be used
    ! The following code is inherited from OBJECT

      call set_default_units_(stdin,next_str_(stdin))

   end subroutine

   subroutine read_junk(self)
    type(basis_type) :: self
    ! Read in a junk string, useful for ignoring a field
    ! The following code is inherited from OBJECT

      call skip_next_item_(stdin)

   end subroutine

   subroutine read_label(self)
    type(basis_type) :: self
    ! Read only the basis label

      call read_(stdin,self%label)

   end subroutine

   subroutine read_shell(self)
    type(basis_type) :: self
    ! Read a shell

      call read_list_keywords_(self%shell)
      call update_(self)

   end subroutine

   subroutine read_tonto_style(self)
    type(basis_type) :: self
    ! Create and read a tonto style basis set

      call read_label_(self)
      call set_keys_(self%shell,(/"l_chr=","n_cc= ","ex,cc="/))
      call read_data_(self%shell)
      call update_(self)

   end subroutine

   subroutine read_gamess_us(self)
    type(basis_type) :: self
    ! Create and read a GAMESS-US style basis set
      character(128), dimension(:), pointer :: the_keys

      call read_label_(self)
      the_keys => split_(("l_chr= n_cc= junk,ex,cc="))
      call set_keys_(self%shell,the_keys)
      call destroy_(the_keys)
      call read_data_(self%shell)
      call update_(self)

   end subroutine

!  ********************
!  Key related routines
!  ********************

   subroutine read_keys(self)
    type(basis_type) :: self
    ! Read the "keys".
    ! The following code is inherited from OBJECT

     call clear_keys_(self)
     call read_ptr_(stdin,keys)
     call ignore_memory_leak_(tonto,memory_blocks_gained=1)

   end subroutine

   subroutine process_keys(self)
    type(basis_type) :: self
    ! Process each of the words in the "keys" list.
    ! The following code is inherited from OBJECT
      integer(kind=kind(1)) :: k,l,n_key
      character(128) :: keyword
      character(128), dimension(:), pointer :: internal

      call ensure_(tonto,associated(keys),"BASIS:process_keys ... no keys")
      n_key = size(keys)
      k = 0
      do
         k = k + 1
         keyword = keys(k)
         if (keyword=="}") exit
         if (keyword=="{") then
            l = index_of_matching_bracket_(keys(k:),"{")
            call ensure_(tonto,l>0,"BASIS:process_keys ... no matching closing brace, }")
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
    type(basis_type) :: self
    ! Return .true. if the list-element keys are created.
      logical(kind=kind(.true.)) :: res
    ! The following code is inherited from OBJECT

      res = associated(keys)

   end function

   subroutine set_keys(self,the_keys)
    type(basis_type) :: self
    ! This is for setting the "keys" externally.
     character(len=*), dimension(:) :: the_keys
    ! The following code is inherited from OBJECT

     call clear_keys_(self)
     call create_copy_(keys,the_keys)
     call ignore_memory_leak_(tonto,memory_blocks_gained=1)

   end subroutine

   subroutine clear_keys(self)
    type(basis_type) :: self
    ! This is for destroying the "keys" externally.
    ! The following code is inherited from OBJECT

     if (associated(keys)) then
       call destroy_(keys)
       call ignore_memory_leak_(tonto,memory_blocks_gained=-1)
     end if

   end subroutine

   subroutine put_table_footer(self)
    type(basis_type) :: self
    ! Output a table footer from the list of "keys". NOTE: not all keywords need
    ! contribute to the banner - any unrecognised keyword is skipped.
    ! The following code is inherited from OBJECT

     call dash_(stdout,width=table_width_(self))

   end subroutine

   subroutine put_table_header(self)
    type(basis_type) :: self
    ! Output a table header from the list of "keys". NOTE: not all keywords need
    ! contribute to the banner - any unrecognised keyword is skipped.
     character(128) :: word
     integer(kind=kind(1)) :: width,k

     call ensure_(tonto,associated(keys),"BASIS:put_table_header ... no keys")
     width = table_width_(self)
     if (width > 0) then
       call dash_(stdout,width=width)
       do k = 1,size(keys)
         word = keys(k)
         call to_lower_case_(word)
         select case (word)
           case ("put_label   "); call put_(stdout,"label",int_width=.true.)
           case ("put_n_shells"); call put_(stdout,"n_shells",int_width=.true.)
           case ("put_n_bf    "); call put_(stdout,"n_bf",int_width=.true.)
           case ("put_n_prim  "); call put_(stdout,"n_prim",int_width=.true.)
           case ("flush       "); call flush_(stdout); exit
         end select
         if (k==size(keys)) then
           call flush_(stdout)  ! In case they didn't write one.
           call warn_(tonto,"BASIS:put_table_header ... no flush keyword - you may later overrun the output buffer")
         end if
       end do
       call dash_(stdout,width=width)
     end if

   end subroutine

   function table_width(self) result(res)
    type(basis_type) :: self
    ! Return the table width in characters, based on "keys".  Note that not all
    ! keywords need to contribute to the banner - if a keyword is not recognised,
    ! then it is skipped.
     integer(kind=kind(1)) :: res
     character(128) :: word
     integer(kind=kind(1)) :: int_dash,real_dash,k

     call ensure_(tonto,associated(keys),"BASIS:table_width ... no keys")
     int_dash = 0
     real_dash = 0
     do k = 1,size(keys)
       word = keys(k)
       call to_lower_case_(word)
       select case (word)
         case ("}           ");  ! exit surrounding loop
         case ("put_label   "); int_dash = int_dash + 1
         case ("put_n_shells"); int_dash = int_dash + 1
         case ("put_n_bf    "); int_dash = int_dash + 1
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
    type(basis_type) :: self
    ! Return .true. if the basis set "self" is the same as "b". Only the
    ! shell vector is compared to see if they are "really" the same.
      intent(in) :: self
      type(basis_type), intent(in) :: b
      logical(kind=kind(.true.)) :: res

      res = same_as_(self%shell,b%shell)

   end function

   function contraction_matrix(self) result(ccm)
    type(basis_type) :: self
    ! Set the contraction coefficient matrix. Its dimensions are (.n_prim,.n_bf).
      intent(in) :: self
      real(kind=kind(1.0d0)), dimension(self%n_prim,self%n_bf) :: ccm
      integer(kind=kind(1)) :: b,p,n_p,n_b,i
      type(shell_type), pointer :: sh

      p = 0                        ! no of primitives in all previous shells
      b = 0                        ! basis number
      ccm = 0.0d0
      do i = 1,self%n_shell            ! For segmented basis sets !
         sh => self%shell(i)
         n_b = sh%n_comp
         n_p = n_prim_(sh)
         call make_contraction_matrix_(sh,ccm(p+1:p+n_p,b+1:b+n_b))
         b = b + n_b
         p = p + n_p
      end do

   end function

   subroutine unnormalise(self)
    type(basis_type) :: self
    ! Take the normalisation factors out of the primitives, assuming
    ! that the contraction coeff's refer to normalised basis functions

      if (associated(self%shell)) call unnormalise_(self%shell)

   end subroutine

   subroutine renormalise(self)
    type(basis_type) :: self
    ! Put back in the normalisation factors of the primitives, assuming
    ! that the contraction coeff's refer to unnormalised basis functions

      if (associated(self%shell)) call renormalise_(self%shell)

   end subroutine

!  ************
!  Put routines
!  ************

   subroutine put(self)
    type(basis_type) :: self
    ! Put out the basis information to file "stdout"

      call flush_(stdout)
      call show_(stdout,"Basis set : ",trim(self%label))
      call flush_(stdout)
      call show_(stdout,"No. of shells          =",self%n_shell)
      call show_(stdout,"No. of basis functions =",self%n_bf)
      call show_(stdout,"No. of primitives      =",self%n_prim)
      call put_table_(self)

   end subroutine

   subroutine put_table(self)
    type(basis_type) :: self
    ! Put out the basis information to file "stdout"
      type(shell_type), pointer :: sh
      integer(kind=kind(1)) :: i,j,b,p

      call flush_(stdout)
      call dash_(stdout,int_fields=3,real_fields=2)
      call put_(stdout,"Type",int_width=.true.)
      call put_(stdout,"Fn",int_width=.true.)
      call put_(stdout,"Prim",int_width=.true.)
      call put_(stdout,"Exponent")
      call put_(stdout,"Contraction")
      call flush_(stdout)
      call dash_(stdout,int_fields=3,real_fields=2)
      b = 1
      p = 1
      do i = 1,self%n_shell
         sh => self%shell(i)
         do j = 1,sh%n_cc
            if (j==1) then
               call put_(stdout,l_chr_(sh),int_width=.true.)
               call put_(stdout,b)
            else
               call tab_(stdout,int_fields=2)
            end if
            call put_(stdout,p)
            call put_(stdout,sh%ex(j))
            call put_(stdout,sh%cc(j))
            call flush_(stdout)
            p = p + sh%n_comp
         end do
         b = b + sh%n_comp
      end do
      call dash_(stdout,int_fields=3,real_fields=2)

   end subroutine

!  ***************
!  Inquiry methods
!  ***************

   pure function no_of_shells(self) result(res)
    type(basis_type) :: self
    ! Work out and return the number of shells in the basis set
      intent(in) :: self
      integer(kind=kind(1)) :: res

      res = no_of_shells_(self%shell)

   end function

   pure function no_of_basis_functions(self) result(res)
    type(basis_type) :: self
    ! Work out and return the number of basis functions in the basis set
      intent(in) :: self
      integer(kind=kind(1)) :: res

      res = no_of_basis_functions_(self%shell)

   end function

   pure function no_of_primitives(self) result(res)
    type(basis_type) :: self
    ! Work out and return the number of primitives in the basis set
      intent(in) :: self
      integer(kind=kind(1)) :: res

      res = no_of_primitives_(self%shell)

   end function

   pure function min_exponent(self) result(res)
    type(basis_type) :: self
    ! Return the minimum exponent in the basis.
     intent(in) :: self
     real(kind=kind(1.0d0)) :: res

     res = min_exponent_(self%shell)

   end function

end
