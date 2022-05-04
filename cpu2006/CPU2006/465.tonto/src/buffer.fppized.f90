!-----------------------------------------------------------------------------
!
!  BUFFER : operations on a string buffer ...
!
!  Synopsis
!
!  A BUFFER consists of a string (string), a pair of cursor positions
!  (item_start, item_end), an item counter (item_index), the number of items
!  in the buffer (n_items), a logical switch which indicates whether the
!  buffer has been analysed (analysed), and a variable indicating which
!  characters are to be regarded as initiating a comment (comment_chars).
!
!  Methods are divided into two types -- "put" operations and "get" operations.
!  The former involving placing value type variables, strings, integers, real's,
!  into the buffer, with or without formatting. The "get" operations involving
!  extracting value type variables from the string buffer.
!
!  There are also methods for moving around the buffer, including skipping
!  forwards or backwards, either by item or by character.
!
!  Notes
!
!  The buffer string is of length BSTR_SIZE. Problems will occur if you want
!  to analyse strings larger than this. The maximum size of any item is
!  STR_SIZE.
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
! $Id: buffer.foo,v 1.12.2.3 2003/05/29 03:37:15 reaper Exp $
!-----------------------------------------------------------------------------

module BUFFER_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

   use STR_MODULE, only: is_int_
   use STR_MODULE, only: to_bin_
   use STR_MODULE, only: to_imprecise_real_
   use STR_MODULE, only: is_bin_
   use STR_MODULE, only: to_cpx_
   use STR_MODULE, only: get_next_item_
   use STR_MODULE, only: to_real_
   use STR_MODULE, only: is_imprecise_real_
   use STR_MODULE, only: is_cpx_
   use STR_MODULE, only: to_int_
   use STR_MODULE, only: is_real_
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

   public    put_formatted_cpx_
   interface put_formatted_cpx_
      module procedure put_formatted_cpx
   end interface

   public    previous_item_
   interface previous_item_
      module procedure previous_item
   end interface

   public    put_formatted_real_
   interface put_formatted_real_
      module procedure put_formatted_real
   end interface

   public    cursor_pointer_
   interface cursor_pointer_
      module procedure cursor_pointer
   end interface

   public    get_bin_
   interface get_bin_
      module procedure get_bin
   end interface

   public    n_items_
   interface n_items_
      module procedure n_items
   end interface

   public    all_items_
   interface all_items_
      module procedure all_items
   end interface

   public    includes_
   interface includes_
      module procedure includes
   end interface

   public    put_formatted_int_
   interface put_formatted_int_
      module procedure put_formatted_int
   end interface

   public    not_exhausted_
   interface not_exhausted_
      module procedure not_exhausted
   end interface

   public    analysed_
   interface analysed_
      module procedure analysed
   end interface

   public    buffer_string_
   interface buffer_string_
      module procedure buffer_string
   end interface

   public    clear_
   interface clear_
      module procedure clear
   end interface

   public    skip_item_
   interface skip_item_
      module procedure skip_item
   end interface

   public    next_item_
   interface next_item_
      module procedure next_item
   end interface

   public    eliminate_special_chars_
   interface eliminate_special_chars_
      module procedure eliminate_special_chars
   end interface

   public    put_str_
   interface put_str_
      module procedure put_str
   end interface

   public    get_str_
   interface get_str_
      module procedure get_str
   end interface

   public    move_cursor_
   interface move_cursor_
      module procedure move_cursor
   end interface

   public    copy_
   interface copy_
      module procedure copy
   end interface

   public    get_item_
   interface get_item_
      module procedure get_item
   end interface

   public    set_
   interface set_
      module procedure set
   end interface

   public    set_defaults_
   interface set_defaults_
      module procedure set_defaults
   end interface

   public    move_to_item_
   interface move_to_item_
      module procedure move_to_item
   end interface

   public    all_remaining_items_
   interface all_remaining_items_
      module procedure all_remaining_items
   end interface

   public    create_
   interface create_
      module procedure create
   end interface

   public    get_imprecise_real_
   interface get_imprecise_real_
      module procedure get_imprecise_real
   end interface

   public    create_copy_
   interface create_copy_
      module procedure create_copy
   end interface

   public    get_cpx_
   interface get_cpx_
      module procedure get_cpx
   end interface

   public    exhausted_
   interface exhausted_
      module procedure exhausted
   end interface

   public    put_formatted_bin_
   interface put_formatted_bin_
      module procedure put_formatted_bin
   end interface

   public    get_real_
   interface get_real_
      module procedure get_real
   end interface

   public    destroy_
   interface destroy_
      module procedure destroy
   end interface

   public    empty_
   interface empty_
      module procedure empty
   end interface

   public    get_int_
   interface get_int_
      module procedure get_int
   end interface

   public    next_item_number_
   interface next_item_number_
      module procedure next_item_number
   end interface

   public    get_formatted_real_
   interface get_formatted_real_
      module procedure get_formatted_real
   end interface

   public    put_formatted_str_
   interface put_formatted_str_
      module procedure put_formatted_str
   end interface

   public    analyse_
   interface analyse_
      module procedure analyse
   end interface

   public get_; interface get_
      module procedure get_item
      module procedure get_bin
      module procedure get_int
      module procedure get_real
      module procedure get_formatted_real
      module procedure get_imprecise_real
      module procedure get_cpx
   end interface

   public put_; interface put_
      module procedure put_str
      module procedure put_formatted_str
      module procedure put_formatted_bin
      module procedure put_formatted_int
      module procedure put_formatted_real
      module procedure put_formatted_cpx
   end interface

contains

!  *************************
!  Initialisation operations
!  *************************

   subroutine create(self,string,comment_chars,quote_chars)
    type(buffer_type) :: self
    ! Create a buffer and initialize it
      pointer :: self
      character(*), optional :: string,comment_chars,quote_chars

      nullify(self)
      allocate(self)

      call set_(self,string,comment_chars,quote_chars)

   end subroutine

   subroutine destroy(self)
    type(buffer_type) :: self
    ! Destroy a buffer
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

   subroutine create_copy(self,b)
    type(buffer_type) :: self
    ! Copy a buffer "b"
      pointer :: self
       type(buffer_type) :: b

      call create_(self)
      call copy_(self,b)

   end subroutine

   subroutine copy(self,b)
    type(buffer_type) :: self
    ! Copy a buffer "b"
      type(buffer_type) :: b

      self = b

   end subroutine

   subroutine set(self,string,comment_chars,quote_chars,eliminate_specials)
    type(buffer_type) :: self
    ! Set the buffer .string to "string", analyse it, and initialize the
    ! counters. You can set the "comment_chars" used to define the end of a
    ! parsable line, the allowed "quote_chars" used to defined a quoted item, and
    ! you can define whether to "eliminate_special" unreadable characters.
      character(*), intent(in), optional :: string,comment_chars,quote_chars
      logical(kind=kind(.true.)), intent(in), optional :: eliminate_specials

      call set_defaults_(self)
      if (present(string))        self%string = string
      if (present(comment_chars)) self%comment_chars = comment_chars
      if (present(quote_chars))   self%quote_chars   = quote_chars
      if (present(eliminate_specials)) call eliminate_special_chars_(self)
      call analyse_(self)

   end subroutine

   subroutine set_defaults(self)
    type(buffer_type) :: self
    ! Set default values

      self%string = " "
      self%item_start = 0
      self%item_end   = 0
      self%item_index = 0
      self%n_items = 0
      self%analysed = .false.
      self%comment_chars = "!#"
      self%quote_chars   = "'"""

   end subroutine

   subroutine clear(self)
    type(buffer_type) :: self
    ! Clear the buffer string and reset the counters

      call set_(self)

   end subroutine

   subroutine analyse(self)
    type(buffer_type) :: self
    ! Analyse the buffer string and process it into items
      integer(kind=kind(1)) :: end,f,l
      character(256) :: item

      end = 0
      self%n_items = 0
      do
         item = " "
         call get_next_item_(self%string(end+1:),item,f,l,self%comment_chars,self%quote_chars)
         if (item==" ") exit
         end = end+l+1
         self%n_items = self%n_items+1
         if (end>256) exit
      end do
      self%analysed = .true.

   end subroutine

   subroutine eliminate_special_chars(self)
    type(buffer_type) :: self
    ! Remove any special characters from the buffer .string, by setting them to
    ! the blank character. Useful for getting rid of control characters in funny
    ! files from other crazy operating systems (read DOS).
      character(52) :: letters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
      character(41) :: symbols = "1234567890-=;',./`~!@#$%^&*()_+{}|:""<>?"
      character(97) :: allowed
      integer(kind=kind(1)) :: i,k

      allowed = letters//symbols//achar(91)//achar(92)//achar(93)
      do i = 1,len_trim(self%string)
         k = verify(self%string(i:i),set=allowed)
         if (k==0) cycle
         self%string(i:i) = " "
      end do

   end subroutine

!  ************************
!  Get operations on buffer
!  ************************

   subroutine get_item(self,item)
    type(buffer_type) :: self
    ! Get the next item in the buffer string as a str and increment
    ! the cursor
      character(128) :: item
      character(256) :: bitem
      integer(kind=kind(1)) :: f,l

   call ensure_(tonto,self%analysed,"BUFFER:get_item ... buffer not analysed")
      item = " "
      if (.not. exhausted_(self)) then
         bitem = " "
         call get_next_item_(self%string(self%item_end+1:),bitem,f,l,self%comment_chars,self%quote_chars)
         item        = adjustl(bitem)
         self%item_start = self%item_end + f
         self%item_end   = self%item_end + l
         self%item_index = self%item_index + 1
      end if

   end subroutine

   subroutine skip_item(self)
    type(buffer_type) :: self
    ! Skip the next item in the buffer string, if there is one to skip.
      character(256) :: bitem
      integer(kind=kind(1)) :: f,l

   call ensure_(tonto,self%analysed,"BUFFER:skip_item ... buffer not analysed")
      bitem = " "
      if (.not. exhausted_(self)) then
         call get_next_item_(self%string(self%item_end+1:),bitem,f,l,self%comment_chars,self%quote_chars)
         self%item_start = self%item_end + f
         self%item_end   = self%item_end + l
         self%item_index = self%item_index + 1
      end if

   end subroutine

   subroutine move_to_item(self,number)
    type(buffer_type) :: self
    ! Move the cursor over to the *start* of item "number" (actually, it is moved
    ! to after the end of the previous item).
      integer(kind=kind(1)) :: number
      integer(kind=kind(1)) :: item

   call ensure_(tonto,self%analysed,"BUFFER:move_to_item ... buffer not analysed")
   call ensure_(tonto,number<=self%n_items+1,"BUFFER:move_to_item ... not enough items")
   call ensure_(tonto,number>=1,"BUFFER:move_to_item ... cannot move to items less than 1")
      self%item_start = 0
      self%item_end   = 0
      self%item_index = 0
      do item=1,(number-1)
         call skip_item_(self)
      end do

   end subroutine

   subroutine get_str(self,value)
    type(buffer_type) :: self
    ! Get the next item in the buffer string as a str
      character(128) :: value

      call get_item_(self,value)

   end subroutine

   subroutine get_bin(self,value)
    type(buffer_type) :: self
    ! Get the next item in the buffer string as a logical
      logical(kind=kind(.true.)) :: value
      character(128) :: item

      call get_item_(self,item)
   call ensure_(tonto,is_bin_(item),"BUFFER:get_bin ... expected logical in input")
      value = to_bin_(item)

   end subroutine

   subroutine get_int(self,value)
    type(buffer_type) :: self
    ! Get the next item in the buffer string as an integer number
      integer(kind=kind(1)) :: value
      character(128) :: item

      call get_item_(self,item)
   call ensure_(tonto,is_int_(item),"BUFFER:get_int ... expected integer in input")
      value = to_int_(item)

   end subroutine

   subroutine get_real(self,value)
    type(buffer_type) :: self
    ! Get the next item in the buffer string as a real number
      real(kind=kind(1.0d0)) :: value
      character(128) :: item

      call get_item_(self,item)
      call ensure_(tonto,is_real_(item),"BUFFER:get_real ... expected real number in input")
      value = to_real_(item)

   end subroutine

   subroutine get_cpx(self,value)
    type(buffer_type) :: self
    ! Get the next item in the buffer string as a cpx number
      complex(kind=kind((1.0d0,1.0d0))) :: value
      character(128) :: item,item2

      call get_item_(self,item)
      item2 = " "
      if (.not. is_cpx_(item)) call get_item_(self,item2)  ! Try two real numbers
      item = trim(item)//" "//trim(item2)
   call ensure_(tonto,is_cpx_(item),"BUFFER:get_cpx ... expected complex number in input")
      value = to_cpx_(item)

   end subroutine

   subroutine get_formatted_real(self,value,format)
    type(buffer_type) :: self
    ! Get a real "value" into the buffer string using fortran format string
    ! "format", and increment the cursor.
      real(kind=kind(1.0d0)) :: value
      character(*), intent(in) :: format
      integer(kind=kind(1)) :: first,last,width

      first = scan(format,"FfEeDd") + 1
      last = scan(format,".") - 1
      read(format(first:last),*) width
      read( self%string(self%item_end+1:self%item_end+1+width), format) value
      self%item_end = self%item_end + width
      self%item_index = self%item_index + 1

   end subroutine

   subroutine get_imprecise_real(self,value,error)
    type(buffer_type) :: self
    ! Get the next item in the buffer string as a real number "value" with a
    ! quoted "error" in parentheses immediately afterwards. If the error is not
    ! present in the string it is assumed to be zero. This only works for "f"
    ! numbers.
      real(kind=kind(1.0d0)) :: value,error
      character(128) :: item

      call get_item_(self,item)
      call ensure_(tonto,is_real_(item) .or. is_imprecise_real_(item),"BUFFER:get_imprecise_real ... expected imprecise REAL &
&in input")
      call to_imprecise_real_(item,value,error)

   end subroutine

!  ************************
!  Put operations on buffer
!  ************************

   subroutine move_cursor(self,skip)
    type(buffer_type) :: self
    ! Move the cursor "skip" characters in the buffer string
      integer(kind=kind(1)) :: skip

      self%item_end = self%item_end + skip
   call ensure_(tonto,self%item_end<=256,"BUFFER:move_cursor ... cursor beyond buffer end")

   end subroutine

   subroutine put_str(self,string)
    type(buffer_type) :: self
    ! Put "string" into the buffer string *after* the current position and
    ! increment the cursor
      character(*) :: string
       integer(kind=kind(1)) :: l

   call ensure_(tonto,self%item_end+len(string)<=256,"BUFFER:put_str ... cursor beyond buffer end")
      l = len(string)
      self%string(self%item_end+1:self%item_end+l) = string
      call move_cursor_(self,l)
      self%analysed = .false.

   end subroutine

   subroutine put_formatted_str(self,value,format)
    type(buffer_type) :: self
    ! Put a string "value" into the buffer string using fortran format string
    ! "format", and increment the cursor.
      character(*) :: value
      character(*) :: format
      character(128) :: string

      string = " "
      write(string,"("//trim(format)//")") value
      call put_str_(self,trim(string))
      self%analysed = .false.

   end subroutine

   subroutine put_formatted_bin(self,value,format)
    type(buffer_type) :: self
    ! Put a logical "value" into the buffer string using fortran format string
    ! "format", and increment the cursor.
      logical(kind=kind(.true.)) :: value
      character(*) :: format
      character(128) :: string

      write(string,"("//trim(format)//")") value
      call put_str_(self,trim(string))
      self%analysed = .false.

   end subroutine

   subroutine put_formatted_int(self,value,format)
    type(buffer_type) :: self
    ! Put an integer "value" into the buffer string using fortran format string
    ! "format", and increment the cursor.
      integer(kind=kind(1)) :: value
      character(*) :: format
      character(128) :: string

      write(string,"("//trim(format)//")") value
      call put_str_(self,trim(string))
      self%analysed = .false.

   end subroutine

   subroutine put_formatted_real(self,value,format)
    type(buffer_type) :: self
    ! Put a real "value" into the buffer string using fortran format string
    ! "format", and increment the cursor.
      real(kind=kind(1.0d0)) :: value
      character(*) :: format
      character(128) :: string

      write(string,"("//trim(format)//")") value
      call put_str_(self,trim(string))
      self%analysed = .false.

   end subroutine

   subroutine put_formatted_cpx(self,value,format)
    type(buffer_type) :: self
    ! Put a cpx "value" into the buffer string using fortran format string
    ! "format", and increment the cursor.
      complex(kind=kind((1.0d0,1.0d0))) :: value
     !  r,i :: real(kind=kind(1.0d0))
      character(*) :: format
      character(128) :: string
     ! rstr,istr,sn :: STR
     ! r = real(value)
     ! i = aimag(value)
     ! write(rstr,"("//trim(format)//")") real(value)
     ! write(istr,"("//trim(format)//")") aimag(value)

      write(string,"("//"2"//trim(format)//")") value
     ! call die_if_(tonto,sign(r)<1 .and. rstr(2:2)/=" ","cannot convert "//r.to_str.trim//" to cmplx")
     ! call die_if_(tonto,rstr(1:1)/=" ","cannot convert "//r.to_str.trim//" to cmplx")
     ! call die_if_(tonto,sign(i)<1 .and. istr(2:2)/=" ","cannot convert "//i.to_str.trim//" to cmplx")
     ! call die_if_(tonto,istr(1:1)/=" ","cannot convert "//i.to_str.trim//" to cmplx")
     ! if (sign(r)<1) sn = "-"
     ! string = adjustr("("//trim(sn)//
      call put_str_(self,trim(string))
      self%analysed = .false.

   end subroutine

!  ***************
!  Inquiry methods
!  ***************

!   next_item_index result(res)
!   ! Return the number of the item which the cursor lies *before*,
!   ! i.e. the next item to be processed.
!      res :: integer(kind=kind(1))
!      res = .item_index+1 !
!   end

   function next_item_number(self) result(res)
    type(buffer_type) :: self
    ! Return the number of the item which the cursor lies *before*,
    ! i.e. the next item to be processed.
      integer(kind=kind(1)) :: res

      res = self%item_index+1  !

   end function

   function next_item(self) result(res)
    type(buffer_type) :: self
    ! Return the actual item which the cursor lies *before*, i.e. the next
    ! item to be processed. The cursor is positioned after the end of this
    ! item when the routine concludes.
      character(128) :: res

      call get_item_(self,res)

   end function

   function previous_item(self) result(res)
    type(buffer_type) :: self
    ! Return the item which the cursor lies *after*, i.e. the previous item
    ! to be processed. The cursor remains unchanged after this routine.
      character(128) :: res

      if (self%item_start>0) then
         res = self%string(self%item_start:self%item_end)
      else
         res = " "
      end if

   end function

   function all_items(self) result(res)
    type(buffer_type) :: self
    ! Return all items as a vector
      character(128), dimension(self%n_items) :: res
      integer(kind=kind(1)) :: n

   call ensure_(tonto,self%analysed,"BUFFER:all_items ... must be analysed first")
   call ensure_(tonto,self%n_items>=1,"BUFFER:all_items ... must have at least one item")
      call move_to_item_(self,1)
      do n = 1,self%n_items
         call get_item_(self,res(n))
      end do

   end function

   function all_remaining_items(self) result(res)
    type(buffer_type) :: self
    ! Return all remaining items in the buffer as a vector
      character(128), dimension(self%n_items-self%item_index) :: res
      integer(kind=kind(1)) :: n,i

   call ensure_(tonto,self%analysed,"BUFFER:all_remaining_items ... must be analysed first")
   call ensure_(tonto,self%n_items-self%item_index>=0,"BUFFER:all_remaining_items ... must have some remaining items")
      call skip_item_(self)
      i = 0
      do n = 1,self%n_items-self%item_index
         i = i + 1
         call get_item_(self,res(i))
      end do

   end function

   function n_items(self) result(res)
    type(buffer_type) :: self
    ! Return the number of items in the buffer string
      integer(kind=kind(1)) :: res

      if (.not. self%analysed) call analyse_(self)
      res = self%n_items

   end function

   function buffer_string(self) result(res)
    type(buffer_type) :: self
    ! Return the buffer string, less any blank spaces at the end
      character(256) :: res

      res = " "
      res = self%string(1:max(len_trim(self%string),1))

   end function

   function cursor_pointer(self) result(res)
    type(buffer_type) :: self
    ! Return a string of the form "----^" which is a pictorial representation
    ! of where the cursor position lies
      character(256) :: res

      res = " "
      res = repeat("-",self%item_end-1)//"^"

   end function

   function analysed(self) result(res)
    type(buffer_type) :: self
    ! Return true if the buffer string has been analysed
      logical(kind=kind(.true.)) :: res

      res = self%analysed

   end function

   function exhausted(self) result(res)
    type(buffer_type) :: self
    ! Return true if there are no more items in the buffer string that could be
    ! extracted
      logical(kind=kind(.true.)) :: res

      res = (self%n_items == 0) .or. (self%item_index >= self%n_items)

   end function

   function not_exhausted(self) result(res)
    type(buffer_type) :: self
    ! Return true if there are more items in the buffer string that could be
    ! extracted
      logical(kind=kind(.true.)) :: res

      res = .not. exhausted_(self)

   end function

   function empty(self) result(res)
    type(buffer_type) :: self
    ! Return true if the buffer string contains no items
      logical(kind=kind(.true.)) :: res

      res = self%n_items==0

   end function

   function includes(self,item) result(res)
    type(buffer_type) :: self
    ! Return .true. if the buffer contains "item" as a separate entity
     character(*) :: item
     logical(kind=kind(.true.)) :: res
     character(128) :: word

     call move_to_item_(self,1)
     res = .false.
     item_search: do
        call get_str_(self,word)
        if (word==item) then
           res = .true.
           exit item_search
        end if
        if (exhausted_(self)) exit
     end do item_search

   end function

end
