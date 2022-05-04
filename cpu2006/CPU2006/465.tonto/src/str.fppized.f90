!---------------------------------------------------------------------------
!
!  STR: Methods of dealing with arbitrary length character strings
!
!  Notes
!
!  Normally, a STR variable means a character string of length STR_SIZE.
!  However, in this module we use arbitrary length character strings.
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
! $Id: str.foo,v 1.16.2.11 2003/11/13 05:33:02 reaper Exp $
!
!---------------------------------------------------------------------------

module STR_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

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

   public    trim_blanks_from_start_
   interface trim_blanks_from_start_
      module procedure trim_blanks_from_start
   end interface

   public    filename_head_
   interface filename_head_
      module procedure filename_head
   end interface

   public    same_as_
   interface same_as_
      module procedure same_as
   end interface

   public    is_a_true_cpx_
   interface is_a_true_cpx_
      module procedure is_a_true_cpx
   end interface

   public    includes_any_in_
   interface includes_any_in_
      module procedure includes_any_in
   end interface

   public    is_known_unit_
   interface is_known_unit_
      module procedure is_known_unit
   end interface

   public    to_cpx_
   interface to_cpx_
      module procedure to_cpx
   end interface

   public    is_a_true_int_
   interface is_a_true_int_
      module procedure is_a_true_int
   end interface

   public    conversion_factor_
   interface conversion_factor_
      module procedure conversion_factor
   end interface

   public    frac_to_real_
   interface frac_to_real_
      module procedure frac_to_real
   end interface

   public    remove_blanks_
   interface remove_blanks_
      module procedure remove_blanks
   end interface

   public    to_int_
   interface to_int_
      module procedure to_int
   end interface

   public    copy_
   interface copy_
      module procedure copy
   end interface

   public    get_item_
   interface get_item_
      module procedure get_item
   end interface

   public    from_cpx_
   interface from_cpx_
      module procedure from_cpx
   end interface

   public    replace_
   interface replace_
      module procedure replace
   end interface

   public    align_right_
   interface align_right_
      module procedure align_right
   end interface

   public    item_
   interface item_
      module procedure item
   end interface

   public    is_real_
   interface is_real_
      module procedure is_real
   end interface

   public    from_int_
   interface from_int_
      module procedure from_int
   end interface

   public    remove_
   interface remove_
      module procedure remove
   end interface

   public    destroy_
   interface destroy_
      module procedure destroy
   end interface

   public    separate_after_
   interface separate_after_
      module procedure separate_after
   end interface

   public    separate_all_characters_
   interface separate_all_characters_
      module procedure separate_all_characters
   end interface

   public    to_bin_
   interface to_bin_
      module procedure to_bin
   end interface

   public    to_real_
   interface to_real_
      module procedure to_real
   end interface

   public    to_upper_case_
   interface to_upper_case_
      module procedure to_upper_case
   end interface

   public    index_of_character_not_in_
   interface index_of_character_not_in_
      module procedure index_of_character_not_in
   end interface

   public    insert_
   interface insert_
      module procedure insert
   end interface

   public    index_of_substring_
   interface index_of_substring_
      module procedure index_of_substring
   end interface

   public    filename_tail_
   interface filename_tail_
      module procedure filename_tail
   end interface

   public    from_bin_
   interface from_bin_
      module procedure from_bin
   end interface

   public    is_cpx_
   interface is_cpx_
      module procedure is_cpx
   end interface

   public    separate_before_
   interface separate_before_
      module procedure separate_before
   end interface

   public    index_of_character_in_
   interface index_of_character_in_
      module procedure index_of_character_in
   end interface

   public    n_items_
   interface n_items_
      module procedure n_items
   end interface

   public    includes_
   interface includes_
      module procedure includes
   end interface

   public    is_int_
   interface is_int_
      module procedure is_int
   end interface

   public    trim_blanks_from_end_
   interface trim_blanks_from_end_
      module procedure trim_blanks_from_end
   end interface

   public    from_real_
   interface from_real_
      module procedure from_real
   end interface

   public    is_numeric_
   interface is_numeric_
      module procedure is_numeric
   end interface

   public    is_alphabetical_
   interface is_alphabetical_
      module procedure is_alphabetical
   end interface

   public    does_not_include_
   interface does_not_include_
      module procedure does_not_include
   end interface

   public    has_all_characters_in_
   interface has_all_characters_in_
      module procedure has_all_characters_in
   end interface

   public    create_
   interface create_
      module procedure create
   end interface

   public    has_any_characters_in_
   interface has_any_characters_in_
      module procedure has_any_characters_in
   end interface

   public    create_copy_
   interface create_copy_
      module procedure create_copy
   end interface

   private    quote_position_
   interface quote_position_
      module procedure quote_position
   end interface

   public    left_justify_
   interface left_justify_
      module procedure left_justify
   end interface

   public    index_of_matching_
   interface index_of_matching_
      module procedure index_of_matching
   end interface

   public    is_bin_
   interface is_bin_
      module procedure is_bin
   end interface

   public    get_next_item_
   interface get_next_item_
      module procedure get_next_item
   end interface

   public    get_next_item_position_
   interface get_next_item_position_
      module procedure get_next_item_position
   end interface

   public    get_next_items_
   interface get_next_items_
      module procedure get_next_items
   end interface

   public    is_included_in_
   interface is_included_in_
      module procedure is_included_in
   end interface

   public    is_a_real_pair_
   interface is_a_real_pair_
      module procedure is_a_real_pair
   end interface

   public    to_imprecise_real_
   interface to_imprecise_real_
      module procedure to_imprecise_real
   end interface

   public    to_lower_case_
   interface to_lower_case_
      module procedure to_lower_case
   end interface

   public    is_one_of_
   interface is_one_of_
      module procedure is_one_of
   end interface

   public    is_alphanumeric_
   interface is_alphanumeric_
      module procedure is_alphanumeric
   end interface

   public    filename_directory_
   interface filename_directory_
      module procedure filename_directory
   end interface

   public    is_imprecise_real_
   interface is_imprecise_real_
      module procedure is_imprecise_real
   end interface

   public    is_included_in_any_
   interface is_included_in_any_
      module procedure is_included_in_any
   end interface

   public    split_
   interface split_
      module procedure split
   end interface

   public    align_left_
   interface align_left_
      module procedure align_left
   end interface

   public    right_justify_
   interface right_justify_
      module procedure right_justify
   end interface

!  Make strings arguments arbitrary length by default

   public trim_; interface trim_
      module procedure trim_blanks_from_end
   end interface

   public scan_; interface scan_
      module procedure index_of_character_in
   end interface

   public verify_; interface verify_
      module procedure index_of_character_not_in
   end interface

   public adjustl_; interface adjustl_
      module procedure align_left
   end interface

   public adjustr_; interface adjustr_
      module procedure align_right
   end interface

contains

   subroutine create(self)
    character(*) :: self
    ! Create space for a string variable
      pointer :: self

      nullify(self)
      allocate(self)

      self = " "

   end subroutine

   subroutine destroy(self)
    character(*) :: self
    ! Destroy space for a string variable
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

   subroutine create_copy(self,s)
    character(*) :: self
    ! Create a copy of "s"
      pointer :: self
      character(*) :: s

      call create_(self)
      call copy_(self,s)

   end subroutine

   subroutine copy(self,s)
    character(*) :: self
    ! Make a copy of the string "s"
      character(*) :: s

      self = s

   end subroutine

   function trim_blanks_from_end(self) result(res)
    character(*) :: self
    ! Return the trimmed version of "self"
      character(len_trim(self)) :: res

      res = trim(self)

   end function

   function trim_blanks_from_start(self) result(res)
    character(*) :: self
    ! Return the trimmed version of "self"
      character(len(self)) :: res
!      res = self(.index_of_character_not_in(" "):)

      res = self(verify(self," "):)

   end function

   function same_as(self,string,ignore_case) result(same)
    character(*) :: self
    ! Test to see if the string is the same as another string
    ! If "ignore_case" is present and .true. then case is ignored.
      character(*), intent(in) :: string
      logical(kind=kind(.true.)), optional :: ignore_case
      logical(kind=kind(.true.)) :: same
      logical(kind=kind(.true.)) :: ignore
      character(len(self)) :: s1
      character(len(string)) :: s2

      ignore = .false.
      if (present(ignore_case)) ignore = ignore_case
      if (ignore) then
         s1 = self;   call to_lower_case_(s1)
         s2 = string; call to_lower_case_(s2)
         same = s1==s2
      else
         same = self==string
      end if

   end function

   function n_items(self) result(res)
    character(*) :: self
    ! Return the number of items in the string
      integer(kind=kind(1)) :: res
      integer(kind=kind(1)) :: end,f,l
      character(256) :: item

      end = 0
      res = 0
      do
         item = " "
         call get_next_item_(self(end+1:),item,f,l)
         if (item==" ") exit
         end = end + l + 1
         res = res + 1
         if (end>len(self)) exit
      end do

   end function

   function item(self,n) result(res)
    character(*) :: self
    ! Return the item no. "n" in the string.
      intent(in) :: self
      integer(kind=kind(1)), intent(in) :: n
      character(len(self)) :: res

      call get_item_(self,n,res)

   end function

   subroutine get_item(self,n,item,position)
    character(*) :: self
    ! Get the item no. "n" in the self string and put the result in "item". If
    ! "position" is present it is set to the index of the character after the end
    ! of item "n".
      intent(in) :: self
      integer(kind=kind(1)), intent(in) :: n
      character(*), intent(out) :: item
      integer(kind=kind(1)), intent(out), optional :: position
      character(len(self)) :: word
      integer(kind=kind(1)) :: i,f,l

      f = 1
      do i = 1,n
         call get_next_item_(self(f:),word,last=l)
         if (word==" ") exit
         f = f + l
      end do
      item = word
      if (present(position)) position=f

   end subroutine

   subroutine get_next_item(self,item,first,last,comment_chars,quote_chars)
    character(*) :: self
    ! Get the first sequence of non-blank characters in the string (i.e. an
    ! "item") and (if present) the "first" and "last" character positions of the
    ! item in the self string.  If the first character of the word is a double
    ! quote then all text between it and the next double quote is treated as one
    ! item. If "quote_chars" is present and not blank, then any of these
    ! characters is regarded as a double quote; but if "quote_chars" is blank
    ! then no quotation characters are recognised. If "comment_chars" is present,
    ! the rest of the string following these comment characters is ignored.
      intent(in) :: self
      character(*), intent(out) :: item
      integer(kind=kind(1)), intent(out), optional :: first,last
      character(*), optional :: comment_chars,quote_chars
      character(16) :: quotes
      character(1) :: quote
      character(len(self)) :: word
      integer(kind=kind(1)) :: f,l

      quotes = '"'
      if (present(quote_chars)) quotes = quote_chars
      call get_next_item_position_(self,f,l)
      if (f==0) then                           ! all characters are blanks
         word = " "
      else if (quotes/=" " .and. scan(self(f:f),quotes)/=0) then  ! quotes
         quote = self(f:f)
         l = quote_position_(self(f+1:),quote)
         call die_if_(tonto,l==0,"STR:get_next_item ... unclosed quotes")
         l = f+l
         word = self(f+1:l-1)
      else if (present(comment_chars)) then    ! comments
         if (has_any_characters_in_(self(f:f),comment_chars)) then
            l = f-1
            word = " "
         else
            word = self(f:l)
         end if
      else
         word = self(f:l)
      end if
      item = word
      if(present(first)) first = f
      if(present(last))  last  = l

   end subroutine

   subroutine get_next_item_position(self,first,last)
    character(*) :: self
    ! Get the first and last character positions of the first sequence of
    ! non-blank characters in the string (i.e. a "word")
      integer(kind=kind(1)), intent(out) :: first,last

      first = verify(self," ")
      last = scan(self(max(first,1):)//" "," ") - 1
      last = last + max(first,1) - 1
      last = max(last,0)

   end subroutine

   function split(self) result(res)
    character(*) :: self
    ! Split the string into a vector of separate items.
      character(128), dimension(:), pointer :: res
      integer(kind=kind(1)) :: i,n

      n = n_items_(self)
      allocate(res(n))

      do i = 1,n
         res(i) = item_(self,i)
      end do

   end function

   subroutine get_next_items(self,word)
    character(*) :: self
    ! Get items 2 and after from self, i.e. all except the first item.
      character(len(self)), intent(out) :: word
      integer(kind=kind(1)) :: l

      call get_next_item_(self,word,last=l)
      word = adjustl(self(2+l:))

   end subroutine

   function quote_position(self,quote_chars) result(pos)
    character(*) :: self
    ! Find the position of the first double quote character.
     integer(kind=kind(1)) :: pos
     character(*), optional :: quote_chars

     if (present(quote_chars)) then
       pos = scan(self,quote_chars)
     else
       pos = index(self,'"')
     end if

   end function

   function index_of_matching(self,symbol) result(res)
    character(*) :: self
    ! Return the first index of the matching "symbol" in self.  The first element
    ! of self need not be an opening bracket symbol. Returns zero if no match
    ! could be found, and also a warning.
      character(*), intent(in) :: symbol
      integer(kind=kind(1)) :: res
      character(len=1), dimension(6) :: opening = (/"'",'"',"{","(","[","<"/)
      character(len=1), dimension(6) :: closing = (/"'",'"',"}",")","]",">"/)
      character(1) :: op,cl,c
      integer(kind=kind(1)) :: i,s,n

      call ensure_(tonto,any(symbol==opening),"STR:index_of_matching ... unrecognised opening symbol")
      call ensure_(tonto,includes_(self,symbol),"STR:index_of_matching ... opening symbol cannot be found in self")
      op = symbol
      do i = 1,size(opening)
         if (op/=opening(i)) cycle
         exit
      end do
      cl = closing(i)
      s = scan(self,op)
      n = 0
      res = 0
      do i = s+1,len_trim(self)
         c = self(i:i)
         if      (c==op) then;           n = n + 1
         else if (c==cl .and. n==0) then; res = i; exit
         else if (c==cl .and. n >0) then;  n = n - 1
         end if
      end do
      call warn_if_(tonto,res==0,"STR:index_of_matching ... unmatching number of closing bracket symbols")

   end function

   subroutine insert(self,string,position)
    character(*) :: self
    ! Insert "string" at "position" into the self string.
      character(*), intent(in) :: string
      integer(kind=kind(1)), intent(in) :: position
      character(len(self)) :: rest

      rest = self(position:)
      self(position:) = string
      self(position+len(string):) = rest

   end subroutine

   function align_left(self) result(res)
    character(*) :: self
    ! Return a string the same as "self" except with the first nonblank character
    ! aligned flush to the LHS of the string
      character(len(self)) :: res

      res = adjustl(self)

   end function

   pure subroutine left_justify(self)
    character(*) :: self
    ! Remove leftmost blank characters by shifting all characters to the left
      intent(inout) :: self
      self = adjustl(self)

   end subroutine

   function align_right(self) result(res)
    character(*) :: self
    ! Return a string the same as self except with the last nonblank character
    ! aligned flush to the RHS of the string
      character(len(self)) :: res

      res = adjustr(self)

   end function

   subroutine right_justify(self)
    character(*) :: self
    ! Remove rightmost blank characters by shifting all characters to the right

      self = adjustr(self)

   end subroutine

   function index_of_substring(self,substring,backwards) result(ind)
    character(*) :: self
    ! Return the starting index of a substring in the original string
      character(*), intent(in) :: substring
      logical(kind=kind(.true.)), optional :: backwards
      integer(kind=kind(1)) :: ind
!      ind = index(self,substring,backwards)

      if (present(backwards)) then
        ind = index(self,substring,backwards)
      else
        ind = index(self,substring)
      end if

   end function

   function is_included_in(self,string,at_start) result(res)
    character(*) :: self
    ! Return .true. if self is included in "string". Trailing blanks in self are
    ! ignored. If "at_start" is present and .true. then the result is .true. only
    ! if self is included at the start of the string.
      character(*), intent(in) :: string
      logical(kind=kind(.true.)), intent(in), optional :: at_start
      logical(kind=kind(.true.)) :: res
      integer(kind=kind(1)) :: ind

      ind = index(string,trim(self))
      res = ind /= 0
      if (present(at_start)) then
      if (at_start) then
         res = ind == 1
      end if
      end if

   end function

   function is_included_in_any(self,strvec) result(res)
    character(*) :: self
    ! Return .true. if self is included in any element of the string vector
    ! "strvec". Trailing blanks in self are *not* ignored.
      character(*), dimension(:), intent(in) :: strvec
      logical(kind=kind(.true.)) :: res

      res = any(index(strvec,spread(self,1,size(strvec))) /= 0)

   end function

   function includes(self,string,at_start) result(res)
    character(*) :: self
    ! Return true if self includes "string". Trailing blanks in self are ignored
    ! If "at_start" is present, the result is true only if "string" is the first
    ! part of self.
      character(*), intent(in) :: string
      logical(kind=kind(.true.)), intent(in), optional :: at_start
      logical(kind=kind(.true.)) :: res
      integer(kind=kind(1)) :: ind

      ind = index(trim(self),string)
      res = ind /= 0
      if (present(at_start)) then
      if (at_start) then
         res = ind == 1
      end if
      end if

   end function

   function includes_any_in(self,strvec) result(res)
    character(*) :: self
    ! Return .true. if self includes any element of the string vector "strvec".
    ! Trailing blanks in self are ignored.
      character(*), dimension(:), intent(in) :: strvec
      logical(kind=kind(.true.)) :: res
      integer(kind=kind(1)) :: i

      res = .false.
      do i = 1,size(strvec)
         res = index(trim(self),trim(strvec(i))) /= 0
         if (res) exit
      end do

   end function

   function does_not_include(self,string) result(res)
    character(*) :: self
    ! Return true if self does not include string. Traling blanks in self are ignored
      character(*), intent(in) :: string
      logical(kind=kind(.true.)) :: res
      integer(kind=kind(1)) :: ind

      ind = index(self(1:len_trim(self)),string)
      res = (ind==0)

   end function

   function has_any_characters_in(self,set) result(res)
    character(*) :: self
    ! Return .true. if self has any of the characters in "set".
      character(*), intent(in) :: set
      logical(kind=kind(.true.)) :: res
!     res = .index_of_character_in(set) /= 0

      res = scan(self,set) /= 0

   end function

   function has_all_characters_in(self,set) result(res)
    character(*) :: self
    ! Return .true. if self has all of its characters in "set".
      character(*), intent(in) :: set
      logical(kind=kind(.true.)) :: res

      res = index_of_character_not_in_(self,set) == 0

   end function

   function index_of_character_in(self,set,backwards) result(ind)
    character(*) :: self
    ! In self, scan from left to right and return the index of the first
    ! character in "set". If backwards is present and true, scan from
    ! right to left
      character(*), intent(in) :: set
      logical(kind=kind(.true.)), optional :: backwards
      integer(kind=kind(1)) :: ind
!     ind = scan(self,set,backwards)

      if (present(backwards)) then
        ind = scan(self,set,backwards)
      else
        ind = scan(self,set)
      end if

   end function

!   verify(set,backwards) result(ind)
!   ! In self, scan from left to right and return the index of the first
!   ! character *not* in "set". If backwards is present and true, scan from
!   ! right to left
!      set :: STR, intent(in)
!      backwards :: logical(kind=kind(.true.)), optional
!      ind :: integer(kind=kind(1))
!!      ind = verify(self,set,backwards)
!      if (present(backwards)) then
!        ind = verify(self,set,backwards)
!      else
!        ind = verify(self,set)
!      end
!   end

   function index_of_character_not_in(self,set,backwards) result(ind)
    character(*) :: self
    ! In self, scan from left to right and return the index of the first
    ! character *not* in "set". If backwards is present and true, scan from
    ! right to left
      character(*), intent(in) :: set
      logical(kind=kind(.true.)), optional :: backwards
      integer(kind=kind(1)) :: ind
!      ind = verify(self,set,backwards)

      if (present(backwards)) then
        ind = verify(self,set,backwards)
      else
        ind = verify(self,set)
      end if

   end function

   subroutine to_lower_case(self)
    character(*) :: self
    ! Change upper case charaters to lower case in the original string
       integer(kind=kind(1)) :: i

      do i = 1,len(self)
         if("A"<=self(i:i) .and. self(i:i)<="Z") then
            self(i:i) = achar(iachar(self(i:i))+32)
         end if
      end do

   end subroutine

   subroutine to_upper_case(self)
    character(*) :: self
    ! Change lower case charaters to upper case in the original string
       integer(kind=kind(1)) :: i

      do i = 1,len(self)
         if("a"<=self(i:i) .and. self(i:i)<="z") then
            self(i:i) = achar(iachar(self(i:i))-32)
         end if
      end do

   end subroutine

   subroutine replace(self,a,b)
    character(*) :: self
    ! Replace all occurences of string "a" by "b". String "b" can be zero
    ! length, however, replacements only occur up to the last nonblank
    ! character in "self" i.e. up to len_trim(self).
      character(*) :: a,b
      character(len(self)) :: post
      integer(kind=kind(1)) :: len_a,len_b,i

      call ensure_(tonto,len(a)>0,"STR:replace ... len(a) must be non-zero")
      len_a = len(a)
      len_b = len(b)
      i = 0
      do
         i = i + 1
         if (i+len_a-1>len_trim(self)) exit
         if (self(i:i+len_a-1)/=a)     cycle
         post = self(i+len_a:)
         if (len_b>0) self(i:i+len_b-1) = b
         call ensure_(tonto,i+len_b<len(self),"STR:replace ... replacement exceeds string length")
         self(i+len_b:) = post
         i = i + len_b - 1
      end do

   end subroutine

   subroutine remove(self,a)
    character(*) :: self
    ! Remove all occurences of "a" from "self".
      character(*) :: a

      call replace_(self,a,"")

   end subroutine

   subroutine remove_blanks(self)
    character(*) :: self
    ! Replace all blanks by moving all non-blank characters leftwards

      call remove_(self," ")

   end subroutine

   subroutine separate_all_characters(self)
    character(*) :: self
    ! Separate all nonblank characters by one space
      integer(kind=kind(1)) :: i

      i = 1
      do
         if (i>=len_trim(self)) exit
         if (self(i:i)==" ") then
            self(i:) = self(i+1:)
         else
            self(i+2:)    = self(i+1:)
            self(i+1:i+1) = " "
            i = i + 2
         end if
      end do

   end subroutine

   subroutine separate_before(self,characters)
    character(*) :: self
    ! Separate the string by placing a space before each character that occurs in
    ! "characters".
      intent(inout) :: self
      character(*) :: characters
      character(1) :: thischar
      integer(kind=kind(1)) :: i,last

      i = 1
      last = len(self)
      do
        if (i>len_trim(self)) exit
        thischar = self(i:i)
        if (includes_(characters,thischar)) then
            self(i+1:last) = self(i:last-1)  ! move all along by one.
            self(i:i) = " "
            i = i + 1
        end if
        i = i + 1
      end do

   end subroutine

   subroutine separate_after(self,characters)
    character(*) :: self
    ! Separate the string by placing a space after each character that occurs in
    ! "characters".
      intent(inout) :: self
      character(*) :: characters
      character(1) :: thischar
      integer(kind=kind(1)) :: i,last

      i = 1
      last = len(self)
      do
        if (i>=len_trim(self)-1) exit
        thischar = self(i:i)
        if (includes_(characters,thischar)) then
            self(i+2:last) = self(i+1:last-1)  ! move all along by one.
            self(i+1:i+1) = " "
            i = i + 1
        end if
        i = i + 1
      end do

   end subroutine

!  **********************
!  File name manipulation
!  **********************

   function filename_head(self) result(res)
    character(*) :: self
    ! Return the head part of a file name, e.g. if self is "/home/file.c" it
    ! returnd the string "file".
      character(len(self)) :: res
      integer(kind=kind(1)) :: f,l

      call ensure_(tonto,self/=" ","STR:filename_head ... string is blank!")
      l = index_of_character_in_(self,".",backwards=.true.)
      if (l/=0) then
         l = l - 1
      else
         l = index_of_character_not_in_(self," ",backwards=.true.)
      end if
      f = index_of_character_in_(self(:l),"/",backwards=.true.)
      if (f==0) then
         f = 1
      else
         f = f + 1
      end if
      res = self(f:l)

   end function

   function filename_tail(self) result(res)
    character(*) :: self
    ! Return the tail part of a file name, e.g. if self is "/home/file.c" it
    ! returnd the string "c".
      character(len(self)) :: res
      integer(kind=kind(1)) :: f,l

      call ensure_(tonto,self/=" ","STR:filename_tail ... string is blank!")
      l = index_of_character_not_in_(self," ",backwards=.true.)
      f = index_of_character_in_(self(:l),".",backwards=.true.)
      call ensure_(tonto,f>0,"STR:filename_tail ... no dot in file name")
      f = f + 1
      res = self(f:l)

   end function

   function filename_directory(self) result(res)
    character(*) :: self
    ! Return the directory part of a file name, e.g. if self is "/home/file.c" it
    ! returnd the string "/home".
      character(len(self)) :: res
      integer(kind=kind(1)) :: l

      call ensure_(tonto,self/=" ","STR:filename_directory ... string is blank!")
      l = index_of_character_in_(self,"/",backwards=.true.)
      if (l == 0) then
         res = "."
      else if (l == 1) then
         res = "/"
      else
         l = l - 1
         res = self(:l)
      end if

   end function

!  *****************
!  Inquiry functions
!  *****************

   function is_real(self) result(res)
    character(*) :: self
    ! Returns true if the string can be interpreted as a real number
      logical(kind=kind(.true.)) :: res
      real(kind=kind(1.0d0)) :: value
      integer(kind=kind(1)) :: i,ios

      i = index_of_character_in_(self,"0123456789")
      if (i==0) then
        res = .false.
      else
        read(unit=self,fmt=*,iostat=ios) value
        res = ios==0
      end if

   end function

   function is_cpx(self) result(res)
    character(*) :: self
    ! Returns true if the string can be interpreted as a (fortran) complex number
      logical(kind=kind(.true.)) :: res

      res = is_a_true_cpx_(self) .or. is_a_real_pair_(self)

   end function

   function is_a_true_cpx(self) result(res)
    character(*) :: self
    ! Returns true if the string can be interpreted as a true (fortran) complex
    ! number
      logical(kind=kind(.true.)) :: res
      complex(kind=kind((1.0d0,1.0d0))) :: value
      integer(kind=kind(1)) :: i,ios

      i = index_of_character_in_(self,"0123456789")
      if (i==0) then
        res = .false.
      else if ((self(1:1)=="(" .or. self(1:2)=="-(") .and. scan(self,",")>1) then
        read(unit=self,fmt=*,iostat=ios) value
        res = ios==0
      end if

   end function

   function is_a_real_pair(self) result(res)
    character(*) :: self
    ! Returns true if the string can be interpreted as a pair of
    ! double precision numbers comprising a complex number
      logical(kind=kind(.true.)) :: res
      integer(kind=kind(1)) :: i,ios
      real(kind=kind(1.0d0)) :: r,c

      i = index_of_character_in_(self,"0123456789")
      if (i==0) then
        res = .false.
      else
        read(unit=self,fmt=*,iostat=ios) r,c
        res = ios==0
      end if

   end function

   function is_int(self) result(res)
    character(*) :: self
    ! Returns true if the string can be interpreted as an integer number
      logical(kind=kind(.true.)) :: res
      integer(kind=kind(1)) :: value,ios
      logical(kind=kind(.true.)) :: char

      char = .not. has_any_characters_in_(self(1:1),"0123456789-")
      if (char) then
         res = .false.
      else
         read(unit=self,fmt=*,iostat=ios) value
         res = ios==0
      end if

   end function

   function is_a_true_int(self) result(res)
    character(*) :: self
    ! Returns true if the string can be interpreted as an integer number
      logical(kind=kind(.true.)) :: res
      integer(kind=kind(1)) :: value,ios

      if (has_all_characters_in_(self,"0123456789- ")) then
         read(unit=self,fmt=*,iostat=ios) value
         res = ios==0
      else
         res = .false.
      end if

   end function

   function is_bin(self) result(res)
    character(*) :: self
    ! Returns true if the string can be interpreted as a logical
      logical(kind=kind(.true.)) :: res
      character(len(self)) :: word

      read(unit=self,fmt=*) word
      call to_lower_case_(word)
      select case (word)
         case("true", "t","on", "yes","y"); res = .true.
         case("false","f","off","no", "n"); res = .true.
         case default;                      res = .false.
      end select

   end function

   function is_imprecise_real(self) result(res)
    character(*) :: self
    ! Returns .true. if the string can be interpreted as an imprecise double, i.e.
    ! a real fortran F "number" followed immediately (without intervening spaces)
    ! by a quoted "error" in parentheses.
      logical(kind=kind(.true.)) :: res
      integer(kind=kind(1)) :: f,l,p

      f = index_of_substring_(self,"(")
      l = index_of_substring_(self,")")
      p = index_of_substring_(self,".")
      if (f==0 .or. (l-f)<=1) then  ! there is no error
         res = .false.
      else
         res = is_real_(self(1:f-1)) .and. is_real_(self(f+1:l-1))
      end if

   end function

   function is_alphabetical(self) result(res)
    character(*) :: self
    ! Returns true if the string contains only alphabetical characters.
      logical(kind=kind(.true.)) :: res
      character(52) :: letters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

      res = index_of_character_not_in_(self,letters) == 0

   end function

   function is_numeric(self) result(res)
    character(*) :: self
    ! Returns true if the string contains only digit characters.
      logical(kind=kind(.true.)) :: res
      character(10) :: digits = "0123456789"

      res = index_of_character_not_in_(self,digits) == 0

   end function

   function is_alphanumeric(self) result(res)
    character(*) :: self
    ! Returns true if the string contains only alphanumeric characters.
      logical(kind=kind(.true.)) :: res
      character(62) :: alphanumeric = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

      res = index_of_character_not_in_(self,alphanumeric) == 0

   end function

!  **********************************
!  Conversion to value type variables
!  **********************************

   function to_real(self) result(value)
    character(*) :: self
    ! Returns the real number corresponding to the first token string
      real(kind=kind(1.0d0)) :: value
      integer(kind=kind(1)) :: ios

      read(unit=self,fmt=*,iostat=ios) value
      call ensure_(tonto,ios==0,"STR:to_real ... not a real number")

   end function

   function frac_to_real(self) result(value)
    character(*) :: self
    ! Returns the real number corresponding to the first token string,
    ! represented as a fraction.
      intent(in) :: self
      real(kind=kind(1.0d0)) :: value
      character(len(self)) :: numerator,denominator,word1
      real(kind=kind(1.0d0)) :: num,denom
      integer(kind=kind(1)) :: ind,ios

      word1 = self
      call remove_blanks_(word1)
      call separate_before_(word1,"/")
      call separate_after_(word1,"/")
      ind = index_of_character_in_(word1,"/")
      numerator = word1(1:ind-1)
      denominator = word1(ind+1:)
      read(unit=numerator,fmt=*,iostat=ios) num
      call ensure_(tonto,ios==0,trim(numerator) // " is not a real number")
      read(unit=denominator,fmt=*,iostat=ios) denom
      call ensure_(tonto,ios==0,trim(denominator) // " is not a real number")
      value = num/denom

   end function

   function to_cpx(self) result(value)
    character(*) :: self
    ! Returns the cpx number corresponding to the token string
    ! If a single token won't do, two tokens are inputted to see
    ! if it could be two real numbers in a row representing a cpx.
      complex(kind=kind((1.0d0,1.0d0))) :: value
      real(kind=kind(1.0d0)) :: r,c

      if (is_a_true_cpx_(self)) then
         read(unit=self,fmt=*) value
      else if (is_a_real_pair_(self)) then
         read(unit=self,fmt=*) r,c
         value = cmplx(r,c,kind=kind((1.0d0,1.0d0)))
      else
         call die_(tonto,"STR:to_cpx ... Could not read complex number")
      end if

   end function

   function to_int(self) result(value)
    character(*) :: self
    ! Returns the integer number corresponding to the first token string
      integer(kind=kind(1)) :: value
      integer(kind=kind(1)) :: ios

      read(unit=self,fmt=*,iostat=ios) value
      call ensure_(tonto,ios==0,"STR:to_int ... not a real number")

   end function

   function to_bin(self) result(value)
    character(*) :: self
    ! Returns the logical corresponding to the first token string
      logical(kind=kind(.true.)) :: value
      character(len(self)) :: word

      word = self
      call to_lower_case_(word)
      select case (word)
         case("true", "t","on", "yes","y"); value=.true.
         case("false","f","off","no", "n"); value=.false.
         case default; call die_(tonto,"STR:to_bin ... cant change to logical type, "//self)
      end select

   end function

   subroutine to_imprecise_real(self,value,error)
    character(*) :: self
    ! Returns the imprecise number corresponding to the first token, i.e. the
    ! real number "value" with a quoted "error" in parentheses immediately
    ! afterwards.  If the error is not present in the string it is assumed to be
    ! zero. This only works for "f" numbers.
      real(kind=kind(1.0d0)) :: value,error
      character(len(self)) :: item,real_str,err_str
      integer(kind=kind(1)) :: f,l,p

      call get_next_item_(self,item)
      f = index_of_substring_(item,"(")
      l = index_of_substring_(item,")")
      p = index_of_substring_(item,".")
      if (f==0 .or. (l-f)<=1) then  ! there is no error
         real_str = item
         err_str = "0"
      else
         real_str = item(1:f-1)
         err_str = item(f+1:l-1)
      end if
      call ensure_(tonto,is_real_(real_str),"STR:to_imprecise_real ... expected real number in input")
      call ensure_(tonto,is_real_(err_str),"STR:to_imprecise_real ... expected real number error in input")
      value = to_real_(real_str)
      error = to_real_(err_str)
      if (p>0) error = error * 10.0d0**(-f+p+1)

   end subroutine

!  ******************************
!  Conversion to self variables
!  ******************************

   subroutine from_int(self,value)
    character(*) :: self
    ! Set the original string to the result from changing integer "value" to a
    ! string
      integer(kind=kind(1)) :: value

      self = " "
      write(self,fmt=*) value
      call left_justify_(self)

   end subroutine

   subroutine from_bin(self,value)
    character(*) :: self
    ! Set the original string to the result from changing logical "value" to a
    ! string
      logical(kind=kind(.true.)) :: value

      self = " "
      write(self,*) value
      call left_justify_(self)

   end subroutine

   subroutine from_real(self,value)
    character(*) :: self
    ! Set the original string to the result from changing real "value" to a
    ! string
      real(kind=kind(1.0d0)) :: value

      self = " "
      write(self,*) value
      call left_justify_(self)

   end subroutine

   subroutine from_cpx(self,value)
    character(*) :: self
    ! Set the original string to the result from changing cpx "value" to a string
      complex(kind=kind((1.0d0,1.0d0))) :: value

      self = " "
      write(self,*) value
      call left_justify_(self)

   end subroutine

!  ****************
!  Units conversion
!  ****************

   function is_known_unit(self) result(res)
    character(*) :: self
    ! Return .true. if the string represents a known unit string
      logical(kind=kind(.true.)) :: res
      character(len(self)) :: word
      integer(kind=kind(1)) :: l

      word = self
      call to_lower_case_(word)
      l = len_trim(word)
      if (word(l:l)=="s" .and. l>1) word = word(1:l-1)
      res = is_one_of_(word,(/ &
              "debye           ", &
              "debye-angstrom  ", &
              "debye-angstrom^2", &
              "degree          ", &
              "bohr            ", &
              "angstrom        ", &
              "angstrom^2      ", &
              "meter           ", &
              "amu             ", &
              "wavenumber      ", &
              "ev              ", &
              "kelvin          ", &
              "joule           ", &
              "kilojoule       ", &
              "kjoule          ", &
              "kj              ", &
              "kcal/mol        " /))

   end function

   function conversion_factor(self) result(res)
    character(*) :: self
    ! Return the conversion factor which converts a value into the specified
    ! unit "self", assuming that the value has default units. In most cases
    ! the default units are atomic units, or radians for angles.
      real(kind=kind(1.0d0)) :: res
      character(len(self)) :: word
      integer(kind=kind(1)) :: l

      word = self
      call to_lower_case_(word)
      l = len_trim(word)
      if (word(l:l)=="s" .and. l>1) word = word(1:l-1)
      select case (word)
        case ("debye           "); res = 2.5418d0
        case ("debye-angstrom  "); res = 2.5418d0*(0.52917724924d0)
        case ("debye-angstrom^2"); res = 2.5418d0*(0.52917724924d0)**2
        case ("degree          "); res = (180d0/3.141592653589793d0)
        case ("bohr            "); res = 1.0d0
        case ("angstrom        "); res = 0.52917724924d0
        case ("angstrom^2      "); res = 0.52917724924d0**2
        case ("meter           "); res = 0.52917724924d0*1.0d-10
        case ("amu             "); res = (9.10939d-31/1.6605402d-27)
        case ("wavenumber      "); res = 2.1947463067d+5
        case ("ev              "); res = 27.21
        case ("kelvin          "); res = 3.158d+5
        case ("joule           "); res = 4.3597482d-18
        case ("kilojoule       "); res = 4.3597482d-21
        case ("kjoule          "); res = 4.3597482d-21
        case ("kj              "); res = 4.3597482d-21
        case ("kcal/mol        "); res = 627.5
        case default;        allocate(tonto%known_keywords(17))
        tonto%known_keywords(1) = "debye           "
        tonto%known_keywords(2) = "debye-angstrom  "
        tonto%known_keywords(3) = "debye-angstrom^2"
        tonto%known_keywords(4) = "degree          "
        tonto%known_keywords(5) = "bohr            "
        tonto%known_keywords(6) = "angstrom        "
        tonto%known_keywords(7) = "angstrom^2      "
        tonto%known_keywords(8) = "meter           "
        tonto%known_keywords(9) = "amu             "
        tonto%known_keywords(10) = "wavenumber      "
        tonto%known_keywords(11) = "ev              "
        tonto%known_keywords(12) = "kelvin          "
        tonto%known_keywords(13) = "joule           "
        tonto%known_keywords(14) = "kilojoule       "
        tonto%known_keywords(15) = "kjoule          "
        tonto%known_keywords(16) = "kj              "
        tonto%known_keywords(17) = "kcal/mol        "
        call unknown_(tonto,word,"STR:conversion_factor")
        deallocate(tonto%known_keywords)
      end select

   end function

   function is_one_of(self,allowed) result(res)
    character(*) :: self
    ! Return .true. if "self" is one of the strings in "allowed".
      character(*), dimension(:) :: allowed
      logical(kind=kind(.true.)) :: res

      res = any(self==allowed)

   end function

end
