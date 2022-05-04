!---------------------------------------------------------------------------
!
! COPPENSBASISVEC: COPPENSBASIS vectors
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
! $Id: coppensbasisvec.foo,v 1.2.2.6 2003/10/01 15:35:48 dylan Exp $
!---------------------------------------------------------------------------

module COPPENSBASISVEC_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

   use INT_MODULE, only: to_str_

   use STR_MODULE, only: same_as_
   use STR_MODULE, only: is_int_
   use STR_MODULE, only: to_lower_case_
   use STR_MODULE, only: to_int_

   use TEXTFILE_MODULE, only: stdin
   use TEXTFILE_MODULE, only: stdout
   use TEXTFILE_MODULE, only: next_str_
   use TEXTFILE_MODULE, only: previous_line_item_
   use TEXTFILE_MODULE, only: redirect_
   use TEXTFILE_MODULE, only: line_number_
   use TEXTFILE_MODULE, only: next_item_
   use TEXTFILE_MODULE, only: move_to_line_
   use TEXTFILE_MODULE, only: revert_
   use TEXTFILE_MODULE, only: move_to_line_item_
   use TEXTFILE_MODULE, only: read_
   use TEXTFILE_MODULE, only: at_end_of_file_
   use TEXTFILE_MODULE, only: flush_
   use TEXTFILE_MODULE, only: reverted_
   use TEXTFILE_MODULE, only: move_to_previous_item_

   use STRVEC_MODULE, only: shrink_
   use STRVEC_MODULE, only: create_
   use STRVEC_MODULE, only: create_copy_
   use STRVEC_MODULE, only: destroy_

   use COPPENSBASIS_MODULE, only: read_keys_
   use COPPENSBASIS_MODULE, only: clear_keys_
   use COPPENSBASIS_MODULE, only: set_defaults_
   use COPPENSBASIS_MODULE, only: nullify_ptr_part_
   use COPPENSBASIS_MODULE, only: create_
   use COPPENSBASIS_MODULE, only: put_table_
   use COPPENSBASIS_MODULE, only: keys_created_
   use COPPENSBASIS_MODULE, only: same_as_
   use COPPENSBASIS_MODULE, only: set_keys_
   use COPPENSBASIS_MODULE, only: no_of_primitives_
   use COPPENSBASIS_MODULE, only: maximum_basis_n_value_
   use COPPENSBASIS_MODULE, only: destroy_ptr_part_
   use COPPENSBASIS_MODULE, only: put_table_header_
   use COPPENSBASIS_MODULE, only: no_of_orbitals_
   use COPPENSBASIS_MODULE, only: destroy_
   use COPPENSBASIS_MODULE, only: unnormalise_
   use COPPENSBASIS_MODULE, only: put_
   use COPPENSBASIS_MODULE, only: process_keys_
   use COPPENSBASIS_MODULE, only: renormalise_
   use COPPENSBASIS_MODULE, only: put_table_footer_
   use COPPENSBASIS_MODULE, only: copy_
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

   public    read_library_directory_
   interface read_library_directory_
      module procedure read_library_directory
   end interface

   public    read_keys_
   interface read_keys_
      module procedure read_keys
   end interface

   public    nullify_ptr_part_
   interface nullify_ptr_part_
      module procedure nullify_ptr_part
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

   public    read_keywords_
   interface read_keywords_
      module procedure read_keywords
   end interface

   public    data_length_
   interface data_length_
      module procedure data_length
   end interface

   public    no_of_orbitals_
   interface no_of_orbitals_
      module procedure no_of_orbitals
   end interface

   public    library_directory_
   interface library_directory_
      module procedure library_directory
   end interface

   public    read_library_data_
   interface read_library_data_
      module procedure read_library_data
   end interface

   public    put_keys_table_
   interface put_keys_table_
      module procedure put_keys_table
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

   public    shrink_
   interface shrink_
      module procedure shrink
   end interface

   public    set_library_directory_
   interface set_library_directory_
      module procedure set_library_directory
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

   public    read_list_keywords_
   interface read_list_keywords_
      module procedure read_list_keywords
   end interface

   public    read_data_
   interface read_data_
      module procedure read_data
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

   public    redirect_
   interface redirect_
      module procedure redirect
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

   public    read_append_data_
   interface read_append_data_
      module procedure read_append_data
   end interface

   public    set_keys_
   interface set_keys_
      module procedure set_keys
   end interface

   public    no_of_primitives_
   interface no_of_primitives_
      module procedure no_of_primitives
   end interface

   public    expand_
   interface expand_
      module procedure expand
   end interface

   public    destroy_
   interface destroy_
      module procedure destroy
   end interface

   public    revert_
   interface revert_
      module procedure revert
   end interface

   public    append_
   interface append_
      module procedure append
      module procedure append_1
   end interface

   public    read_altered_data_
   interface read_altered_data_
      module procedure read_altered_data
   end interface

   public    process_list_keyword_
   interface process_list_keyword_
      module procedure process_list_keyword
   end interface

   public    n_basis_
   interface n_basis_
      module procedure n_basis
   end interface

   type(coppensbasis_type), dimension(:), pointer, private :: saved_self => NULL()
   character(128), private :: basis_set_library_directory = "./basis_sets"

contains

!  ******************
!  Allocation methods
!  ******************

   subroutine create(self,dim)
    type(coppensbasis_type), dimension(:) :: self
    ! Create space for object
      pointer :: self
      integer(kind=kind(1)) :: dim
    ! The following code is inherited from OBJECTVEC

      nullify(self)
      allocate(self(dim))

      call nullify_ptr_part_(self)
      call set_defaults_(self)

   end subroutine

   subroutine destroy(self)
    type(coppensbasis_type), dimension(:) :: self
    ! Destroy space for object
      pointer :: self
    ! The following code is inherited from OBJECTVEC

      if (.not. associated(self)) then;   return; end if
      call destroy_ptr_part_(self)

      deallocate(self)

   end subroutine

   subroutine create_copy(self,vec)
    type(coppensbasis_type), dimension(:) :: self
    ! Create a replica copy of "vec".
      type(coppensbasis_type), dimension(:), intent(in) :: vec
      pointer :: self
    ! The following code is inherited from OBJECTVEC

      call create_(self,size(vec))
      call copy_(self,vec)

   end subroutine

   subroutine copy(self,vec)
    type(coppensbasis_type), dimension(:) :: self
    ! Copy "vec".
      type(coppensbasis_type), dimension(:), intent(in) :: vec
    ! The following code is inherited from OBJECTVEC
      integer(kind=kind(1)) :: a

      call ensure_(tonto,size(self)==size(vec),"COPPENSBASISVEC:copy ... vec size does not match")
      do a = 1,size(vec)
        call copy_(self(a),vec(a))
      end do

   end subroutine

   subroutine nullify_ptr_part(self)
    type(coppensbasis_type), dimension(:) :: self
    ! Nullify the pointer parts of self
    ! The following code is inherited from OBJECTVEC
      integer(kind=kind(1)) :: a

      do a = 1,size(self)
         call nullify_ptr_part_(self(a))
      end do

   end subroutine

   subroutine destroy_ptr_part(self)
    type(coppensbasis_type), dimension(:) :: self
    ! Destroy the pointer parts of self
    ! The following code is inherited from OBJECTVEC
      integer(kind=kind(1)) :: a

      do a = 1,size(self)
        call destroy_ptr_part_(self(a))
      end do

   end subroutine

!   created result(res) ::: get_from(OBJECTVEC)
!   ! Returns true if self has been created
!      self :: pointer
!      res :: logical(kind=kind(.true.))
!   end

!   destroyed result(res) ::: get_from(OBJECTVEC)
!   ! Returns true if self has *not* been created
!      self :: pointer
!      res :: logical(kind=kind(.true.))
!   end

   subroutine set_defaults(self)
    type(coppensbasis_type), dimension(:) :: self
    ! Set default values
    ! The following code is inherited from OBJECTVEC
       integer(kind=kind(1)) :: n

      do n = 1,size(self)
        call set_defaults_(self(n))
      end do

   end subroutine

   function library_directory(self,basis_set_kind) result(res)
    type(coppensbasis_type), dimension(:) :: self
    ! Return the basis set library directory for a particular "basis_set_kind".
     pointer :: self
     character(*) :: basis_set_kind
     character(128) :: res

     res = trim(basis_set_library_directory)//"/"//trim(basis_set_kind)

   end function

   subroutine set_library_directory(self)
    type(coppensbasis_type), dimension(:) :: self
    ! Read the directory name for where the basis set libraries are stored.

     call read_(stdin,basis_set_library_directory)

   end subroutine

! ***********************
! List-based I/O Routines
! ***********************

   subroutine read_list_keywords(self)
    type(coppensbasis_type), dimension(:) :: self
    ! Read in and process list-based keywords from "stdin". List-based keywords
    ! are those that are intended to apply to each individual element of the list
    ! through a list of "keys" stored in the associated list-element type module.
    ! NOTE: this routine will create the list, if required.
     pointer :: self
    ! The following code is inherited from OBJECTVEC
     character(128) :: word

     call ensure_(tonto,next_item_(stdin)=="{","COPPENSBASISVEC:read_list_keywords ... expecting open bracket symbol, {")
     call read_(stdin,word)
     do                   ! Loop over input list-type keywords
       call read_(stdin,word)
       if (word=="}")      exit
       if (reverted_(stdin)) exit
       call process_list_keyword_(self,word)
     end do

   end subroutine

   subroutine process_list_keyword(self,keyword)
    type(coppensbasis_type), dimension(:) :: self
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
    type(coppensbasis_type), dimension(:) :: self
    ! Process the keywords list to read data or commands. If "ignore_braces" is
    ! present then the opening and closing braces, which are normally required,
    ! are ignored.
     pointer :: self
     logical(kind=kind(.true.)), optional :: ignore_braces
    ! The following code is inherited from OBJECTVEC
     character(128) :: word,message
     integer(kind=kind(1)) :: length

     if (.not. present(ignore_braces)) then
        call ensure_(tonto,next_item_(stdin)=="{","COPPENSBASISVEC:read_data ... expecting open bracket symbol, {")
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
        call ensure_(tonto,word=="}","COPPENSBASISVEC:read_data ... expecting close bracket symbol, }")
     end if

   end subroutine

   function data_length(self) result(length)
    type(coppensbasis_type), dimension(:) :: self
    ! Read ahead in stdin to get the "length" of the data list, i.e. the number
    ! of data items in the list. The data must begin with the first data item,
    ! *not* a "{" symbol.  The order of data items comprising the list is given
    ! by keys defined in the associated list-element type module. The data list
    ! must be terminated by a "}" symbol.
     pointer :: self
     integer(kind=kind(1)) :: length
    ! The following code is inherited from OBJECTVEC
     type(coppensbasis_type), pointer :: tmp
     character(128) :: word
     integer(kind=kind(1)) :: line,item

     call ensure_(tonto,next_item_(stdin)/="}","COPPENSBASISVEC:data_length ... empty data list!")
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
    type(coppensbasis_type), dimension(:) :: self
    ! Read in a sublist of the complete list, and alter the data for that
    ! sublist.  The order of the data items in the sublist is given by the "keys"
    ! defined in the associated list-element type module.
     pointer :: self
    ! The following code is inherited from OBJECTVEC
     character(128) :: word
     integer(kind=kind(1)) :: s

     call ensure_(tonto,associated(self),"COPPENSBASISVEC:read_altered_data ... list does not exist yet")
     call ensure_(tonto,next_item_(stdin)=="{","COPPENSBASISVEC:read_altered_data ... expecting open bracket symbol: {")
     call read_(stdin,word)
     read_loop: do
        call read_(stdin,word)
        if (word=="}") exit read_loop
        call ensure_(tonto,is_int_(word),"COPPENSBASISVEC:read_altered_data ... expecting integer list-element index")
        s = to_int_(word)
        call ensure_(tonto,s<=size(self),"COPPENSBASISVEC:read_altered_data ... list-element too large")
        call ensure_(tonto,s>0,"COPPENSBASISVEC:read_altered_data ... list-element must be positive")
        call process_keys_(self(s))
     end do read_loop

   end subroutine

   subroutine read_append_data(self)
    type(coppensbasis_type), dimension(:) :: self
    ! Read in a set of data to append to an existing set.
     pointer :: self
    ! The following code is inherited from OBJECTVEC

   call ensure_(tonto,associated(self),"COPPENSBASISVEC:read_append_data ... list does not exist yet")
   call ensure_(tonto,next_item_(stdin)=="{","COPPENSBASISVEC:read_append_data ... expecting open bracket symbol: {")
     nullify(saved_self)
     call read_data_(saved_self)
     call append_(self,saved_self)
     call destroy_(saved_self)

   end subroutine

   subroutine process_keys(self)
    type(coppensbasis_type), dimension(:) :: self
    ! Process the "keys" on each element of the list.
     pointer :: self
    ! The following code is inherited from OBJECTVEC
     type(coppensbasis_type) :: tmp
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
    type(coppensbasis_type), dimension(:) :: self
    ! Return .true. if the list-element keys are created.
      pointer :: self
      logical(kind=kind(.true.)) :: res
    ! The following code is inherited from OBJECTVEC
      type(coppensbasis_type) :: tmp

      res = keys_created_(tmp)

   end function

   subroutine set_keys(self,the_keys)
    type(coppensbasis_type), dimension(:) :: self
    ! This is for setting the "keys" externally.
     pointer :: self
     character(len=*), dimension(:) :: the_keys
    ! The following code is inherited from OBJECTVEC
     type(coppensbasis_type) :: tmp

     call set_keys_(tmp,the_keys)

   end subroutine

   subroutine clear_keys(self)
    type(coppensbasis_type), dimension(:) :: self
    ! This is for destroying the "keys" externally.
     pointer :: self
    ! The following code is inherited from OBJECTVEC
     type(coppensbasis_type) :: tmp

     call clear_keys_(tmp)

   end subroutine

   subroutine read_keys(self)
    type(coppensbasis_type), dimension(:) :: self
    ! Read a new set of keys
      pointer :: self
    ! The following code is inherited from OBJECTVEC
      type(coppensbasis_type) :: tmp

      call read_keys_(tmp)

   end subroutine

   subroutine put_keys_table(self)
    type(coppensbasis_type), dimension(:) :: self
    ! Output a generic table based on the "keys"
     pointer :: self
    ! The following code is inherited from OBJECTVEC

     call ensure_(tonto,keys_created_(self),"COPPENSBASISVEC:put_keys_table ... no keys")
     call put_table_header_(self)
     call process_keys_(self)
     call put_table_footer_(self)

   end subroutine

   subroutine put_table_header(self)
    type(coppensbasis_type), dimension(:) :: self
    ! Put out a table header based on "keys"
      pointer :: self
    ! The following code is inherited from OBJECTVEC
      type(coppensbasis_type) :: tmp

      call put_table_header_(tmp)

   end subroutine

   subroutine put_table_footer(self)
    type(coppensbasis_type), dimension(:) :: self
    ! Put out a table footer based on "keys"
      pointer :: self
    ! The following code is inherited from OBJECTVEC
      type(coppensbasis_type) :: tmp

      call put_table_footer_(tmp)

   end subroutine

   subroutine redirect(self)
    type(coppensbasis_type), dimension(:) :: self
    ! Redirect input
     pointer :: self
    ! The following code is inherited from OBJECT

     call redirect_(stdin,next_str_(stdin))

   end subroutine

   subroutine revert(self)
    type(coppensbasis_type), dimension(:) :: self
    ! Revert back to previous stdin file
     pointer :: self
    ! The following code is inherited from OBJECT

     call revert_(stdin)

   end subroutine

! ***************************
! Non-list based I/O routines
! ***************************

   subroutine read_keywords(self)
    type(coppensbasis_type), dimension(:) :: self
    ! Read in and process normal (non list-type) keywords from "stdin".
     pointer :: self
     character(128) :: word

     call ensure_(tonto,next_item_(stdin)=="{","COPPENSBASISVEC:read_keywords ... expecting open bracket symbol, {")
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
    type(coppensbasis_type), dimension(:) :: self
    ! Process a normal (non list-type) "keyword".
     pointer :: self
     character(128) :: keyword
     character(128) :: word

     word = keyword
     call to_lower_case_(word)
     select case (word)
       case("}")  ! do nothing.
       case("library_directory="); call read_library_directory_(self)
       case("put               "); call put_(self)
       case("redirect          "); call redirect_(self)
       case("revert            "); call revert_(self)
       case default;               allocate(tonto%known_keywords(5))
       tonto%known_keywords(1) = "}"
       tonto%known_keywords(2) = "library_directory="
       tonto%known_keywords(3) = "put               "
       tonto%known_keywords(4) = "redirect          "
       tonto%known_keywords(5) = "revert            "
       call unknown_(tonto,word,"COPPENSBASISVEC:process_keyword")
       deallocate(tonto%known_keywords)
     end select

   end subroutine

   subroutine read_library_directory(self)
    type(coppensbasis_type), dimension(:) :: self
    ! Read the directory name for where the basis set libraries are stored.
     pointer :: self

     call read_(stdin,basis_set_library_directory)

   end subroutine

   subroutine read_library_data(self,library,labels,n_unique_labels)
    type(coppensbasis_type), dimension(:) :: self
    ! Open the file named "library", read the basis sets which have the right
    ! "labels", and assign them to the basis vector.  The file must begin with a
    ! "{" symbol and must have at least one "data=" list-type keyword following
    ! the open bracket. NOTE: Self is created or expanded by the number of unique
    ! labels in "labels" -- those which cannot be found already in "self".
     pointer :: self
     character(128), intent(in) :: library
     character(128), dimension(:), intent(in) :: labels
     integer(kind=kind(1)), intent(out), optional :: n_unique_labels
    ! The following code is inherited from BASISVEC
     character(128), dimension(:), pointer :: unique_labels
     character(128) :: word
     type(coppensbasis_type), pointer :: basis
     integer(kind=kind(1)) :: found,i,u,n_labels,N

     if (.not. associated(self)) then
        N = 0
        n_labels = size(labels)     ! All labels are unique
        call create_copy_(unique_labels,labels)
        if (n_labels>0) call create_(self,n_labels)
     else
        N = n_basis_(self)
        n_labels = size(labels)     ! Find and add only unique labels
        call create_(unique_labels,n_labels)
        u = 0
        do i = 1,n_labels
           if (any(self(:)%label==labels(i))) cycle
           u = u + 1
           unique_labels(u) = labels(i)
        end do
        n_labels = u
        call shrink_(unique_labels,n_labels)
        if (n_labels>0) call expand_(self,N+n_labels)
     end if
     if (present(n_unique_labels)) n_unique_labels = n_labels
     if (n_labels==0) then
        call destroy_(unique_labels)
          return
     end if
     call redirect_(stdin,library)
     call ensure_(tonto,next_item_(stdin)=="{","COPPENSBASISVEC:read_library_data ... expecting open bracket symbol, {")
     call read_(stdin,word)             ! Read the open bracket
     do                           ! Loop over input list-type keywords
       call read_(stdin,word)
       if (word=="}")      exit
       if (word=="data=")  exit
       call process_list_keyword_(self,word)
     end do
     call ensure_(tonto,keys_created_(self),"COPPENSBASISVEC:read_library_data ... no keys= supplied")
     call ensure_(tonto,word=="data=","COPPENSBASISVEC:read_library_data ... no data= list keyword")
     call ensure_(tonto,next_item_(stdin)=="{","COPPENSBASISVEC:read_library_data ... expecting open bracket symbol, {")
     call read_(stdin,word)             ! move past open brace
     found = 0                    ! Explicitly process data= keyword
     do                           ! Try to match all basis labels
       call create_(basis)
       call process_keys_(basis)
       do i = 1,n_labels
          if (same_as_(basis%label,unique_labels(i),ignore_case=.true.)) then
             found = found + 1
             call copy_(self(N+i),basis)
          end if
       end do
       call destroy_(basis)
       if (found==n_labels) exit  ! All bases found
       call ensure_(tonto,next_item_(stdin)/="}","COPPENSBASISVEC:read_library_data ... missing basis sets in library")
     end do
     call revert_(stdin)
     call destroy_(unique_labels)

   end subroutine

!*******************************************************************************

   subroutine shrink(self,dim)
    type(coppensbasis_type), dimension(:) :: self
    ! Shrink self to dimension dim.  Contents are retained.
     pointer :: self
     integer(kind=kind(1)), intent(in) :: dim
    ! The following code is inherited from OBJECTVEC
     type(coppensbasis_type), dimension(:), pointer :: old
     integer(kind=kind(1)) :: n

     call ensure_(tonto,associated(self),"COPPENSBASISVEC:shrink ... no self array")
     call ensure_(tonto,dim<=size(self),"COPPENSBASISVEC:shrink ... dim too large")
     call ensure_(tonto,dim>=0,"COPPENSBASISVEC:shrink ... dim must be non-negative")
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
    type(coppensbasis_type), dimension(:) :: self
    ! Expand the vector "self" to "dim". New slots are left undefined.
     pointer :: self
     integer(kind=kind(1)), intent(in) :: dim
    ! The following code is inherited from OBJECTVEC
     type(coppensbasis_type), dimension(:), pointer :: old
     integer(kind=kind(1)) :: old_dim

     if (.not. associated(self)) then
        call create_(self,dim)
     else
        call ensure_(tonto,dim>=size(self),"COPPENSBASISVEC:expand ... dim not large enough")
        call ensure_(tonto,dim>=0,"COPPENSBASISVEC:expand ... dim must be non-negative")
        old => self
        old_dim = size(old)
        nullify(self)
        call create_(self,dim)
        call copy_(self(1:old_dim),old)
        call destroy_(old)
     end if

   end subroutine

   subroutine append(self,v)
    type(coppensbasis_type), dimension(:) :: self
    ! Expands self and appends the contents of vector "v".
     pointer :: self
     type(coppensbasis_type), dimension(:), intent(in) :: v
    ! The following code is inherited from OBJECTVEC
     integer(kind=kind(1)) :: dim

     if (.not. associated(self)) then; dim = 0
     else;                 dim = size(self)
     end if
     call expand_(self,dim+size(v))
     call copy_(self(dim+1:),v)

   end subroutine

   subroutine append_1(self,value)
    type(coppensbasis_type), dimension(:) :: self
    ! Expands self by 1, and appends the single scalar "value" onto the end.
     pointer :: self
     type(coppensbasis_type), intent(in) :: value
    ! The following code is inherited from OBJECTVEC
     integer(kind=kind(1)) :: dim

     if (.not. associated(self)) then; dim = 0
     else;                 dim = size(self)
     end if
     call expand_(self,dim+1)
     call copy_(self(dim+1),value)

   end subroutine

   subroutine put(self)
    type(coppensbasis_type), dimension(:) :: self
    ! Output the list information
    ! The following code is inherited from OBJECTVEC
      integer(kind=kind(1)) :: b

      do b = 1,size(self)
         call put_(self(b))
         call flush_(stdout)
      end do

   end subroutine

   subroutine put_table(self)
    type(coppensbasis_type), dimension(:) :: self
    ! Output table information
    ! The following code is inherited from OBJECTVEC
      integer(kind=kind(1)) :: b

      do b = 1,size(self)
         call put_table_(self(b))
         call flush_(stdout)
      end do

   end subroutine

   function n_basis(self) result(res)
    type(coppensbasis_type), dimension(:) :: self
    ! Return the number of basis sets
      integer(kind=kind(1)) :: res

      res = size(self)

   end function

   pure function no_of_orbitals(self) result(res)
    type(coppensbasis_type), dimension(:) :: self
    ! Work out and return the number of orbitals in the basis set
      intent(in) :: self
      integer(kind=kind(1)) :: res
      integer(kind=kind(1)) :: i
      res = 0
      do i = 1,size(self)
         res = res + no_of_orbitals_(self(i))
      end do

   end function

   pure function no_of_primitives(self) result(res)
    type(coppensbasis_type), dimension(:) :: self
    ! Work out and return the number of primitive fitting functions in the basis
      intent(in) :: self
      integer(kind=kind(1)) :: res
      integer(kind=kind(1)) :: i
      res = 0
      do i = 1,size(self)
         res = res + no_of_primitives_(self(i))
      end do

   end function

   function maximum_basis_n_value(self) result(res)
    type(coppensbasis_type), dimension(:) :: self
    ! Returns the maximum n value over all orbitals in every basis set of the
    ! vector
     intent(in) :: self
     integer(kind=kind(1)) :: res
     integer(kind=kind(1)) :: i

     res = 0
     do i = 1,size(self)
        res = max(maximum_basis_n_value_(self(i)),res)
     end do

   end function

   function same_as(self,vec) result(same)
    type(coppensbasis_type), dimension(:) :: self
    ! Return .true. if the orbital vector "self" is the same as "vec".
      type(coppensbasis_type), dimension(:) :: vec
      logical(kind=kind(.true.)) :: same
      integer(kind=kind(1)) :: i

      if (size(self) /= size(vec)) then
         same = .false.
           return
      else
         same = .true.
         do i = 1,size(self)
            same = same .and. same_as_(self(i),vec(i))
            if (.not. same) exit
         end do
      end if

   end function

   subroutine unnormalise(self)
    type(coppensbasis_type), dimension(:) :: self
    ! Un-normalise each basis set.
      integer(kind=kind(1)) :: i

      do i = 1,size(self)
         call unnormalise_(self(i))
      end do

   end subroutine

   subroutine renormalise(self)
    type(coppensbasis_type), dimension(:) :: self
    ! Renormalise each basis set.
      integer(kind=kind(1)) :: i

      do i = 1,size(self)
         call renormalise_(self(i))
      end do

   end subroutine

end
