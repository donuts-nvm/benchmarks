!-------------------------------------------------------------------------------
!
! CIF: An object for processing Crystallographic Information Files (CIF).
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
! $Id: cif.foo,v 1.2.2.8 2003/11/11 02:41:45 dylan Exp $
!-------------------------------------------------------------------------------

module CIF_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

   use REALVEC_MODULE, only: append_
   use REALVEC_MODULE, only: create_

   use INTVEC_MODULE, only: append_
   use INTVEC_MODULE, only: has_repetitions_
   use INTVEC_MODULE, only: create_

   use TEXTFILE_MODULE, only: stdin
   use TEXTFILE_MODULE, only: stdout
   use TEXTFILE_MODULE, only: open_
   use TEXTFILE_MODULE, only: skip_next_item_
   use TEXTFILE_MODULE, only: create_
   use TEXTFILE_MODULE, only: rest_of_line_
   use TEXTFILE_MODULE, only: line_number_
   use TEXTFILE_MODULE, only: create_copy_
   use TEXTFILE_MODULE, only: next_item_
   use TEXTFILE_MODULE, only: look_backwards_for_item_
   use TEXTFILE_MODULE, only: is_open_
   use TEXTFILE_MODULE, only: close_
   use TEXTFILE_MODULE, only: at_end_of_file_
   use TEXTFILE_MODULE, only: read_
   use TEXTFILE_MODULE, only: look_for_any_item_
   use TEXTFILE_MODULE, only: look_for_item_
   use TEXTFILE_MODULE, only: move_to_previous_item_
   use TEXTFILE_MODULE, only: read_line_
   use TEXTFILE_MODULE, only: destroy_

   use STRVEC_MODULE, only: append_
   use STRVEC_MODULE, only: has_repetitions_
   use STRVEC_MODULE, only: create_
   use STRVEC_MODULE, only: index_of_
   use STRVEC_MODULE, only: destroy_

   use REALMAT_MODULE, only: create_
   use REALMAT_MODULE, only: expand_columns_

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

   public    open_
   interface open_
      module procedure open
   end interface

   public    set_defaults_
   interface set_defaults_
      module procedure set_defaults
   end interface

   public    nullify_ptr_part_
   interface nullify_ptr_part_
      module procedure nullify_ptr_part
   end interface

   public    read_looped_item_
   interface read_looped_item_
      module procedure read_looped_item
      module procedure read_looped_item_1
      module procedure read_looped_item_2
   end interface

   public    find_next_data_block_
   interface find_next_data_block_
      module procedure find_next_data_block
   end interface

   public    create_
   interface create_
      module procedure create
   end interface

   public    read_looped_items_
   interface read_looped_items_
      module procedure read_looped_items
   end interface

   public    find_looped_item_
   interface find_looped_item_
      module procedure find_looped_item
   end interface

   public    create_copy_
   interface create_copy_
      module procedure create_copy
   end interface

   private    find_end_of_data_block_
   interface find_end_of_data_block_
      module procedure find_end_of_data_block
   end interface

   public    find_item_
   interface find_item_
      module procedure find_item
   end interface

   public    find_crystal_data_block_
   interface find_crystal_data_block_
      module procedure find_crystal_data_block
   end interface

   public    close_
   interface close_
      module procedure close
   end interface

   public    read_item_
   interface read_item_
      module procedure read_item
      module procedure read_item_1
      module procedure read_item_2
   end interface

   public    destroy_ptr_part_
   interface destroy_ptr_part_
      module procedure destroy_ptr_part
   end interface

   private    find_start_of_data_block_
   interface find_start_of_data_block_
      module procedure find_start_of_data_block
   end interface

   public    find_looped_items_
   interface find_looped_items_
      module procedure find_looped_items
   end interface

   public    find_data_block_
   interface find_data_block_
      module procedure find_data_block
   end interface

   public    destroy_
   interface destroy_
      module procedure destroy
   end interface

   public    copy_
   interface copy_
      module procedure copy
   end interface

contains

   subroutine create(self,name)
    type(cif_type) :: self
    ! Create the object
     pointer :: self
     character(*), optional :: name

     nullify(self)
     allocate(self)

     call nullify_ptr_part_(self)
     call set_defaults_(self,name)

   end subroutine

   subroutine destroy(self)
    type(cif_type) :: self
    ! Destroy the object
      pointer :: self

      if (.not. associated(self)) then;   return; end if
      call destroy_ptr_part_(self)
      deallocate(self)

   end subroutine

   subroutine nullify_ptr_part(self)
    type(cif_type) :: self
    ! Nullify the pointer parts

      nullify(self%file)

   end subroutine

   subroutine destroy_ptr_part(self)
    type(cif_type) :: self
    ! Destroy the pointer parts

      call destroy_(self%file)

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

   subroutine create_copy(self,cif)
    type(cif_type) :: self
    ! Create a copy.
      pointer :: self
      type(cif_type), intent(in) :: cif

      call create_(self)
      call copy_(self,cif)

   end subroutine

   subroutine copy(self,cif)
    type(cif_type) :: self
    ! Set self to be a copy of "cif"
      type(cif_type), intent(in) :: cif

      self = cif
      if (associated(cif%file)) call create_copy_(self%file,cif%file)

   end subroutine

   subroutine set_defaults(self,name)
    type(cif_type) :: self
    ! Set up defaults, especially the type(cif_type) file "name" if it is present.
      character(*), optional :: name

      self%start_of_data = 1
      self%end_of_data = 0
      self%data_block_found = .false.
      if (present(name)) call create_(self%file,name)

   end subroutine

   subroutine open(self)
    type(cif_type) :: self
    ! Create and open the type(cif_type)

      call ensure_(tonto,associated(self%file),"CIF:open ... file not created")
     ! .file.quote_chars = "'" ! disallow ", it is used as an axis symbol !
      call open_(self%file,for="read")

   end subroutine

   subroutine close(self)
    type(cif_type) :: self
    ! Close the type(cif_type), and destroy it

      call ensure_(tonto,associated(self%file),"CIF:close ... file not created")
      call close_(self%file)

   end subroutine

!  *****************
!  Find/read methods
!  *****************

   subroutine find_data_block(self,block_name,found)
    type(cif_type) :: self
    ! Read and find the start of the data block with name "block_name", starting
    ! from ".start_of_data", and store its ".data_block_name". If present "found"
    ! is set .true. if the bblock is found, else .false.; if it is not present and
    ! the data block is not found then it is an error.
      character(*), intent(in) :: block_name
      logical(kind=kind(.true.)), optional, intent(out) :: found

      call ensure_(tonto,associated(self%file),"CIF:find_data_block ... CIF file has not been created")
      call ensure_(tonto,is_open_(self%file),"CIF:find_data_block ... CIF file has not been opened")
      do
         call find_next_data_block_(self)
         if (present(found)) then
            found = self%data_block_found
            if (.not. found) then;   return; end if
         else
            call ensure_(tonto,self%data_block_found,"CIF:find_data_block ... cant find data block with name " // trim(block_&
&name))
         end if
         if (self%data_block_name==block_name) exit
         self%start_of_data = self%end_of_data
      end do

   end subroutine

   subroutine find_next_data_block(self)
    type(cif_type) :: self
    ! Read through and find the next data block, and store its ".data_block_name",
    ! ".start_of_data", and ".end_of_data" line numbers. The search begins from
    ! line ".start_of_data". The routine returns if no data block can be found
    ! and in that case ".data_block_found" is set to .false..

      call ensure_(tonto,associated(self%file),"CIF:find_next_data_block ... CIF file has not been created")
      call ensure_(tonto,is_open_(self%file),"CIF:find_next_data_block ... CIF file has not been opened")
      call find_start_of_data_block_(self)
      if (.not. self%data_block_found) then;   return; end if
      call find_end_of_data_block_(self)

   end subroutine

   subroutine find_start_of_data_block(self)
    type(cif_type) :: self
    ! Read through and find a data block, and store its ".data_block_name",
    ! ".start_of_data". The search begins from line ".start_of_data". The
    ! ".data_block_found" is set to .true. if the block is found, otherwise it is
    ! set .false. and the routine returns. The cursor is left after the starting
    ! data block token, or at the starting position if nothing is found.

      call ensure_(tonto,associated(self%file),"CIF:find_start_of_data_block ... CIF file has not been created")
      call ensure_(tonto,is_open_(self%file),"CIF:find_start_of_data_block ... CIF file has not been opened")
      call look_for_item_(self%file,"data_", &
                          from=self%start_of_data, &
                          head_match=.true., &
                          found=self%data_block_found)
      if (.not. self%data_block_found) then;   return; end if
      call move_to_previous_item_(self%file)
      call read_(self%file,self%data_block_name)
      self%data_block_name = self%data_block_name(6:)  ! without data_ at start
      self%start_of_data = line_number_(self%file)

   end subroutine

   subroutine find_end_of_data_block(self)
    type(cif_type) :: self
    ! Read through and find the end of a data block, starting from the first line
    ! after ".start_of_data", and store this end in ".end_of_data". The cursor is
    ! left at the end of the data block (possibly at the end of the file if there
    ! was no explicit block terminator found). NOTE: There is always an ending to
    ! a data block.

      call ensure_(tonto,associated(self%file),"CIF:find_end_of_data_block ... CIF file has not been created")
      call ensure_(tonto,is_open_(self%file),"CIF:find_end_of_data_block ... CIF file has not been opened")
      call look_for_item_(self%file,"_eof", return_cursor=.false., &
                                  from=self%start_of_data+1, &
                                  end_tokens=(/"data_","_eof "/))
      if (self%file%no_of_lines>0) then; self%end_of_data = self%file%no_of_lines
      else;                          self%end_of_data = line_number_(self%file)
      end if

   end subroutine

   subroutine find_crystal_data_block(self,found)
    type(cif_type) :: self
    ! Read through and find the first acceptable crystal data block, starting
    ! from ".start_of_data", and store its ".data_block_name". It is an error if
    ! a data block is not found; except in the case when "found" is present, in
    ! which case it is set .true. if the data block exists, otherwise .false..
      logical(kind=kind(.true.)), intent(out), optional :: found
      logical(kind=kind(.true.)), dimension(4) :: there
      integer(kind=kind(1)) :: first,last

      call ensure_(tonto,associated(self%file),"CIF:find_crystal_data_block ... CIF file has not been created")
      call ensure_(tonto,is_open_(self%file),"CIF:find_crystal_data_block ... CIF file has not been opened")
      do
         call find_next_data_block_(self)
         if (present(found)) then
            found = self%data_block_found
            if (.not. found) then;   return; end if
         else
            call ensure_(tonto,self%data_block_found,"CIF:find_crystal_data_block ... cant find valid data block in file: "//&
&trim(self%file%name))
         end if
         first = self%start_of_data
         last  = self%end_of_data
         call look_for_item_(self%file,"_symmetry_space_group_name",from=first,until=last,found=there(1))
         call look_for_item_(self%file,"_cell_length",from=first,until=last,found=there(2))
         call look_for_item_(self%file,"_atom_site_label",from=first,until=last,found=there(3))
         if (.not. there(3)) &
         call look_for_item_(self%file,"_atom_site_type_symbol",from=first,until=last,found=there(3))
         call look_for_item_(self%file,"_atom_site_fract",from=first,until=last,found=there(4))
         if (all(there)) exit  ! This is really a crystal info data block
         self%start_of_data = self%end_of_data
      end do
      if (present(found)) found = .true.

   end subroutine

   subroutine find_item(self,ID,found)
    type(cif_type) :: self
    ! Find a (non-looped) data item with identifier "ID" in the current data
    ! block, and leave the file cursor positioned just after it. If present,
    ! "found" is set .true. if "ID" is found, else it is set .false. and the routine
    ! returns without an error. NOTE: it is *not* checked that "ID" is a true
    ! non-looped item.
      character(*), intent(in) :: ID
      logical(kind=kind(.true.)), intent(out), optional :: found
      logical(kind=kind(.true.)) :: fnd

      call ensure_(tonto,associated(self%file),"CIF:find_item ... CIF file has not been created")
      call ensure_(tonto,is_open_(self%file),"CIF:find_item ... CIF file has not been opened")
      call ensure_(tonto,self%start_of_data>0,"CIF:find_item ... no data block found")
      call ensure_(tonto,self%end_of_data>self%start_of_data,"CIF:find_item ... no data block found")
      call look_for_item_(self%file,ID,from=self%start_of_data,until=self%end_of_data,found=fnd,exact_match=.true.)
      if (present(found)) then
         found = fnd
         if (.not. found) then;   return; end if
      else if (.not. fnd) then
         call die_(tonto,"CIF:find_item ... data item "//trim(ID)//" not found")
      end if

   end subroutine

   subroutine read_item(self,ID,item,itemvec,found)
    type(cif_type) :: self
    ! Read a non-looped STR data item with string identifier "ID" from the data
    ! block.  The result is put in "item" if it is a single string, or "itemvec"
    ! if it is a multi-line semicolon delimited paragraph. If present, "found" is
    ! set .true. if "ID" is found.
      character(*), intent(in) :: ID
      character(128), intent(out) :: item
      character(128), dimension(:), pointer :: itemvec
      logical(kind=kind(.true.)), intent(out), optional :: found
      character(128) :: semicolon

      call ensure_(tonto,associated(self%file),"CIF:read_item ... CIF file has not been created")
      call ensure_(tonto,is_open_(self%file),"CIF:read_item ... CIF file has not been opened")
      call ensure_(tonto,self%start_of_data>0,"CIF:read_item ... no data block found")
      call find_item_(self,ID,found)
      if (present(found)) then
        if (.not. found) then;   return; end if
      end if
      call read_(self%file,item)  ! read the item
      semicolon = ";"
      if (item==semicolon .and. self%file%buffer%item_start==1) then
        ! quote_chars = .file.quote_chars     ! switch off quotes
        ! .file.set_quote_chars(" ")
         call create_(itemvec,0)
         item = rest_of_line_(self%file)
         if (item/=" ") call append_(itemvec,item)
         do
           call read_(self%file,item)
           if (item==semicolon .and. self%file%buffer%item_start==1) exit
           call append_(itemvec,self%file%buffer%string)
           call read_line_(self%file)
         end do
         call ensure_(tonto,size(itemvec)>0,"CIF:read_item ... zero length string")
         if (size(itemvec)==1) then
            item = itemvec(1)
            call destroy_(itemvec)
         else
            item = " "
         end if
        ! .file.set_quote_chars(quote_chars)  ! put back quotes
      end if

   end subroutine

   subroutine read_item_1(self,ID,real,error,found)
    type(cif_type) :: self
    ! Read a non-looped real(kind=kind(1.0d0)) data item with string identifier "ID" from the data
    ! block.  The result is put in "real". If present, andy "error" associated
    ! with "real" is also read. If present, "found" is set .true. if "ID" is found.
      character(*) :: ID
      real(kind=kind(1.0d0)) :: real
      real(kind=kind(1.0d0)), optional :: error
      logical(kind=kind(.true.)), intent(out), optional :: found

      call ensure_(tonto,associated(self%file),"CIF:read_item_1 ... CIF file has not been created")
      call ensure_(tonto,is_open_(self%file),"CIF:read_item_1 ... CIF file has not been opened")
      call ensure_(tonto,self%start_of_data>0,"CIF:read_item_1 ... no data block found")
      call find_item_(self,ID,found)
      if (present(found)) then
        if (.not. found) then;   return; end if
      end if
      if (present(error)) then; call read_(self%file,real,error)
      else;                     call read_(self%file,real)
      end if

   end subroutine

   subroutine read_item_2(self,ID,int,found)
    type(cif_type) :: self
    ! Read a non-looped integer(kind=kind(1)) data item with string identifier "ID" from the data block.
    ! The result is put in "int". If present, "found" is set .true. if "ID" is found.
      character(*) :: ID
      integer(kind=kind(1)) :: int
      logical(kind=kind(.true.)), intent(out), optional :: found

      call ensure_(tonto,associated(self%file),"CIF:read_item_2 ... CIF file has not been created")
      call ensure_(tonto,is_open_(self%file),"CIF:read_item_2 ... CIF file has not been opened")
      call ensure_(tonto,self%start_of_data>0,"CIF:read_item_2 ... no data block found")
      call find_item_(self,ID,found)
      if (present(found)) then
        if (.not. found) then;   return; end if
      end if
      call read_(self%file,int)

   end subroutine

   subroutine find_looped_item(self,ID,found,n_item,n_data)
    type(cif_type) :: self
    ! Find a looped data item with identifier "ID" in the current data block, and
    ! leave the file cursor just before the actual looped data.  If present,
    ! "found" is set .true. if "ID" is found, else it is set .false. and the routine
    ! returns without an error. If present, "n_item" is set to the position
    ! number of the item in the looped list, and "n_data" is set to the number of
    ! data elements in the looped list.
      character(len=*) :: ID
      logical(kind=kind(.true.)), intent(out), optional :: found
      integer(kind=kind(1)), intent(out), optional :: n_item,n_data
      logical(kind=kind(.true.)) :: fnd
      character(128) :: word
      integer(kind=kind(1)) :: n_dat,n_itm

      call ensure_(tonto,associated(self%file),"CIF:find_looped_item ... CIF file has not been created")
      call ensure_(tonto,is_open_(self%file),"CIF:find_looped_item ... CIF file has not been opened")
      call ensure_(tonto,self%start_of_data>0,"CIF:find_looped_item ... no data block found")
      call ensure_(tonto,ID(1:1)=="_","CIF:find_looped_item ... ID list is not a looped datum")
      call look_for_item_(self%file,ID,from=self%start_of_data, &
                             until=self%end_of_data, &
                             found=fnd, &
                             exact_match=.true.)
      if (present(found)) then
         found = fnd
         if (.not. found) then;   return; end if
      else if (.not. fnd) then
         call die_(tonto,"CIF:find_looped_item ... data item "//trim(ID)//" not found")
      end if
      call look_backwards_for_item_(self%file,"loop_",until=self%start_of_data,found=fnd)
      if (present(found)) then
         found = fnd
         if (.not. found) then;   return; end if
      else if (.not. fnd) then
         call die_(tonto,"CIF:find_looped_item ... data item "//trim(ID)//" is not looped")
      end if
      n_dat = 0
      do
         call read_(self%file,word)  ! read first looped data item
         if (ID==word) n_itm = n_dat + 1
         if (word(1:1)/="_") exit  ! this is an actual datum
         n_dat = n_dat + 1
      end do
      call move_to_previous_item_(self%file)
      if (present(n_data)) n_data = n_dat
      if (present(n_item)) n_item = n_itm

   end subroutine

   subroutine read_looped_item(self,ID,ivec,found)
    type(cif_type) :: self
    ! Read a looped REALVEC data item with string identifier "ID" from the data block.
    ! The result is put in "ivec". NOTE: "ivec" is created. If present,
    ! "found" is set .true. if "ID" is found, else it is set .false. and the routine
    ! returns without an error.
      character(len=*) :: ID
      integer(kind=kind(1)), dimension(:), pointer :: ivec
      logical(kind=kind(.true.)), intent(out), optional :: found
      integer(kind=kind(1)) :: n_data,n_item,i
      character(128) :: word
      integer(kind=kind(1)) :: val

      call ensure_(tonto,associated(self%file),"CIF:read_looped_item ... CIF file has not been created")
      call ensure_(tonto,is_open_(self%file),"CIF:read_looped_item ... CIF file has not been opened")
      call ensure_(tonto,self%start_of_data>0,"CIF:read_looped_item ... no data block found")
      call ensure_(tonto,ID(1:1)=="_","CIF:read_looped_item ... ID list is not a looped datum")
      call find_looped_item_(self,ID,found,n_item,n_data)
      if (present(found)) then
        if (.not. found) then;   return; end if
      end if
      call create_(ivec,0)
      do
         do i = 1,n_item-1
            call skip_next_item_(self%file)
         end do
         call read_(self%file,val)
         call append_(ivec,val)
         do i = n_item+1,n_data
            call skip_next_item_(self%file)
         end do
         if (at_end_of_file_(self%file)) exit
         word = next_item_(self%file)
         if (word(1:1)=="_") exit
         if (word(1:5)=="loop_") exit
         if (word(1:5)=="data_") exit
         if (word(1:5)=="save_") exit
         if (word(1:7)=="global_") exit
      end do

   end subroutine

   subroutine read_looped_item_1(self,ID,vec,error,found)
    type(cif_type) :: self
    ! Read a looped REALVEC data item with string identifier "ID" from the data block.
    ! The result is put in "vec" and the associated errors are placed in "error".
    ! NOTE: "vec" and "err" are created. If present, "found" is set .true. if "ID"
    ! is found, else it is set .false. and the routine returns without an error.
      character(len=*) :: ID
      real(kind=kind(1.0d0)), dimension(:), pointer :: vec
      real(kind=kind(1.0d0)), dimension(:), pointer, optional :: error
      logical(kind=kind(.true.)), intent(out), optional :: found
      integer(kind=kind(1)) :: n_data,n_item,i
      character(128) :: word
      real(kind=kind(1.0d0)) :: val,err

      call ensure_(tonto,associated(self%file),"CIF:read_looped_item_1 ... CIF file has not been created")
      call ensure_(tonto,is_open_(self%file),"CIF:read_looped_item_1 ... CIF file has not been opened")
      call ensure_(tonto,self%start_of_data>0,"CIF:read_looped_item_1 ... no data block found")
      call ensure_(tonto,ID(1:1)=="_","CIF:read_looped_item_1 ... ID list is not have a looped datum")
      call find_looped_item_(self,ID,found,n_item,n_data)
      if (present(found)) then
        if (.not. found) then;   return; end if
      end if
      call create_(vec,0)
      if (present(error)) call create_(error,0)
      do
         do i = 1,n_item-1
            call skip_next_item_(self%file)
         end do
         if (present(error)) then  ! read both value and error
            call read_(self%file,val,err)
            call append_(vec,val)
            call append_(error,err)
         else                      ! read only value
            call read_(self%file,val,err)
            call append_(vec,val)
         end if
         do i = n_item+1,n_data
            call skip_next_item_(self%file)
         end do
         if (at_end_of_file_(self%file)) exit
         word = next_item_(self%file)
         if (word(1:1)=="_") exit
         if (word(1:5)=="loop_") exit
         if (word(1:5)=="data_") exit
         if (word(1:5)=="save_") exit
         if (word(1:7)=="global_") exit
      end do

   end subroutine

   subroutine read_looped_item_2(self,ID,strvec,found)
    type(cif_type) :: self
    ! Read a looped REALVEC data item with string identifier "ID" from the data block.
    ! The result is put in "strvec". NOTE: "strvec" is created. If present,
    ! "found" is set .true. if "ID" is found, else it is set .false. and the routine
    ! returns without an error.
      character(len=*) :: ID
      character(128), dimension(:), pointer :: strvec
      logical(kind=kind(.true.)), intent(out), optional :: found
      integer(kind=kind(1)) :: n_data,n_item,i
      character(128) :: word

      call ensure_(tonto,associated(self%file),"CIF:read_looped_item_2 ... CIF file has not been created")
      call ensure_(tonto,is_open_(self%file),"CIF:read_looped_item_2 ... CIF file has not been opened")
      call ensure_(tonto,self%start_of_data>0,"CIF:read_looped_item_2 ... no data block found")
      call ensure_(tonto,ID(1:1)=="_","CIF:read_looped_item_2 ... ID list is not have a looped datum")
      call find_looped_item_(self,ID,found,n_item,n_data)
      if (present(found)) then
        if (.not. found) then;   return; end if
      end if
      call create_(strvec,0)
      do
         do i = 1,n_item-1
            call skip_next_item_(self%file)
         end do
         call read_(self%file,word)
         call append_(strvec,word)
         do i = n_item+1,n_data
            call skip_next_item_(self%file)
         end do
         if (at_end_of_file_(self%file)) exit
         word = next_item_(self%file)
         if (word(1:1)=="_") exit
         if (word(1:5)=="loop_") exit
         if (word(1:5)=="data_") exit
         if (word(1:5)=="save_") exit
         if (word(1:7)=="global_") exit
      end do

   end subroutine

   subroutine find_looped_items(self,ID,found,ID_pos,ID_ind,n_labels)
    type(cif_type) :: self
    ! Find a set of looped data items all in the same loop, with identifiers
    ! "ID", in the current data block, and leave the file cursor just before the
    ! actual looped data.  If present, "found" is set .true. if "ID" is found, else
    ! it is set .false. and the routine returns without an error.
      character(len=*), dimension(:) :: ID
      logical(kind=kind(.true.)), intent(out), optional :: found
      integer(kind=kind(1)), dimension(:), intent(out), optional :: ID_pos,ID_ind
      integer(kind=kind(1)), intent(out), optional :: n_labels
      logical(kind=kind(.true.)) :: fnd
      character(128) :: word
      integer(kind=kind(1)) :: label,item

   call ensure_(tonto,associated(self%file),"CIF:find_looped_items ... CIF file has not been created")
   call ensure_(tonto,is_open_(self%file),"CIF:find_looped_items ... CIF file has not been opened")
   call ensure_(tonto,self%start_of_data>0,"CIF:find_looped_items ... no data block found")
   call ensure_(tonto,size(ID)>0,"CIF:find_looped_items ... no items in ID list")
   call ensure_(tonto,all(ID(:)(1:1)=="_"),"CIF:find_looped_items ... ID list does not have a looped datum")
   call ensure_(tonto,.not. has_repetitions_(ID),"CIF:find_looped_items ... ID list has repetitions")
      if (present(ID_pos)) then
      call ensure_(tonto,size(ID)==size(ID_pos),"CIF:find_looped_items ... ID and ID_pos are inconsistent")
      end if
      if (present(ID_ind)) then
      call ensure_(tonto,size(ID)==size(ID_ind),"CIF:find_looped_items ... ID and ID_ind are inconsistent")
      end if
      call look_for_any_item_(self%file,ID,from=self%start_of_data, &
                              until=self%end_of_data, &
                              found=fnd, &
                              exact_match=.true.)
      if (present(found)) then
         found = fnd
         if (.not. found) then;   return; end if
      else if (.not. fnd) then
         call die_(tonto,"CIF:find_looped_items ... no data items found")
      end if
      call look_backwards_for_item_(self%file,"loop_",until=self%start_of_data,found=fnd)
      if (present(found)) then
         found = fnd
         if (.not. found) then;   return; end if
      else if (.not. fnd) then
         call die_(tonto,"CIF:find_looped_items ... at least one data item is not looped")
      end if
      item = 0
      label = 0
      do
         call read_(self%file,word)  ! read looped data item
         if (any(ID==word)) then        ! This is an ID
            item = item + 1             ! The item number; this is not the ID index
            label = label + 1           ! The label number in the looped list
            if (present(ID_pos)) ID_pos(item) = label
            if (present(ID_ind)) ID_ind(item) = index_of_(ID,word)
         else if (word(1:1)=="_") then  ! This is a looped data descriptor
            label = label + 1
         else                           ! This is not a looped descriptor
            exit
         end if
      end do
      fnd = item==size(ID) .and. .not. has_repetitions_(ID_ind)
      if (present(found)) then; found = fnd
      else if (.not. fnd) then; call die_(tonto,"CIF:find_looped_items ... not all data items found")
      end if
      call move_to_previous_item_(self%file)
      if (present(n_labels)) n_labels = label

   end subroutine

   subroutine read_looped_items(self,ID,mat,error,found)
    type(cif_type) :: self
    ! Read a looped set of data items "mat" with string identifiers "ID" from the
    ! data block.  The the associated errors are placed in "error".  NOTE: "mat"
    ! and "err" are created. If present, "found" is set .true. if all the "ID" are
    ! found, else it is set .false. and the routine returns without an error.
      character(len=*), dimension(:) :: ID
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: mat
      real(kind=kind(1.0d0)), dimension(:,:), pointer, optional :: error
      logical(kind=kind(.true.)), intent(out), optional :: found
      integer(kind=kind(1)), dimension(size(ID)) :: ID_pos,ID_ind
      integer(kind=kind(1)) :: n_labels,n,item,i,ind
      character(128) :: word
      real(kind=kind(1.0d0)) :: val,err

   call ensure_(tonto,associated(self%file),"CIF:read_looped_items ... CIF file has not been created")
   call ensure_(tonto,is_open_(self%file),"CIF:read_looped_items ... CIF file has not been opened")
   call ensure_(tonto,self%start_of_data>0,"CIF:read_looped_items ... no data block found")
      call find_looped_items_(self,ID,found,ID_pos,ID_ind,n_labels)
      if (present(found)) then
         if (.not. found) then;   return; end if
      end if
      call create_(mat,size(ID),1)
      if (present(error)) call create_(error,size(ID),1)
      n = 1
      do  ! loop over all the n "looped" data
         item = 1
         do i = 1,n_labels
            if (item>size(ID)) then
               call skip_next_item_(self%file)
            else if (i/=ID_pos(item)) then
               call skip_next_item_(self%file)
            else
               ind = ID_ind(item)
               call read_(self%file,val,err)
                ! stdout.text(.file.buffer.string.trim)
                ! stdout.text(.file.buffer.cursor_pointer)
               mat(ind,n) = val
               if (present(error)) error(ind,n) = err
               item = item + 1  ! next ID label
            end if
         end do
         if (at_end_of_file_(self%file)) exit
         word = next_item_(self%file)
         if (word(1:1)=="_") exit
         if (word(1:5)=="loop_") exit
         if (word(1:5)=="data_") exit
         if (word(1:5)=="save_") exit
         if (word(1:7)=="global_") exit
         n = n + 1
         call expand_columns_(mat,n)
         if (present(error)) call expand_columns_(error,n)
      end do

   end subroutine

end
