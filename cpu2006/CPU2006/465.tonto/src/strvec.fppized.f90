!---------------------------------------------------------------------------
!
!  STRVEC: String vectors
!
!  Notes
!
!  Normally, a STR variable means a character string of length STR_SIZE.
!  However, in this module we use arbitrary length character strings.
!  (See also the STR module where the same tyhing is done).
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
! $Id: strvec.foo,v 1.11.2.3 2003/11/13 05:33:55 reaper Exp $
!---------------------------------------------------------------------------

module STRVEC_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

   use STR_MODULE, only: to_lower_case_
   use STR_MODULE, only: to_upper_case_
   use STR_MODULE, only: copy_

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

   public    has_any_included_in_
   interface has_any_included_in_
      module procedure has_any_included_in
   end interface

   public    create_
   interface create_
      module procedure create
      module procedure create_1
   end interface

   public    create_copy_
   interface create_copy_
      module procedure create_copy
   end interface

   public    remove_repetitions_
   interface remove_repetitions_
      module procedure remove_repetitions
   end interface

   public    append_only_if_unique_
   interface append_only_if_unique_
      module procedure append_only_if_unique
   end interface

   public    index_of_first_that_includes_
   interface index_of_first_that_includes_
      module procedure index_of_first_that_includes
   end interface

   public    expand_
   interface expand_
      module procedure expand
   end interface

   public    destroy_
   interface destroy_
      module procedure destroy
   end interface

   public    includes_
   interface includes_
      module procedure includes
   end interface

   public    has_any_including_
   interface has_any_including_
      module procedure has_any_including
   end interface

   public    to_lower_case_
   interface to_lower_case_
      module procedure to_lower_case
   end interface

   public    quick_sort_
   interface quick_sort_
      module procedure quick_sort
   end interface

   public    index_of_matching_bracket_
   interface index_of_matching_bracket_
      module procedure index_of_matching_bracket
   end interface

   public    index_of_
   interface index_of_
      module procedure index_of
   end interface

   public    shrink_
   interface shrink_
      module procedure shrink
   end interface

   public    append_
   interface append_
      module procedure append
      module procedure append_1
   end interface

   public    index_of_first_included_in_
   interface index_of_first_included_in_
      module procedure index_of_first_included_in
   end interface

   public    has_repetitions_
   interface has_repetitions_
      module procedure has_repetitions
   end interface

   public    prepend_
   interface prepend_
      module procedure prepend
      module procedure prepend_1
   end interface

   public    to_upper_case_
   interface to_upper_case_
      module procedure to_upper_case
   end interface

   public    copy_
   interface copy_
      module procedure copy
   end interface

!  Make strings arguments arbitrary length by default

contains

   subroutine create(self,dim)
    character(*), dimension(:) :: self
    ! Create space for a string vector
      pointer :: self
      integer(kind=kind(1)) :: dim

      nullify(self)
      allocate(self(dim))

   end subroutine

   subroutine create_1(self,v)
    character(*), dimension(:) :: self
    ! Create space for a string vector as assign "v"
      pointer :: self
      character(*), dimension(:) :: v
      integer(kind=kind(1)) :: dim

      nullify(self)
      dim = size(v)
      allocate(self(dim))

      self = v
!     do i = 1,dim            ! self = v is buggy on DEC
!        s = scan(v(i)," ")   ! when v is a constructor
!        self(i) = v(i)(1:s)
!     end

   end subroutine

   subroutine destroy(self)
    character(*), dimension(:) :: self
    ! Destroy space for a string vector
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

   subroutine create_copy(self,vec)
    character(*), dimension(:) :: self
    ! Make a copy of "vec"
      pointer :: self
      character(*), dimension(:) :: vec

      call create_(self,size(vec))
      call copy_(self,vec)

   end subroutine

   subroutine copy(self,vec)
    character(*), dimension(:) :: self
    ! Make a copy of "vec"
      character(*), dimension(:) :: vec
      integer(kind=kind(1)) :: i

      do i = 1,size(vec)
         call copy_(self(i),vec(i))
      end do

   end subroutine

   subroutine to_lower_case(self)
    character(*), dimension(:) :: self
    ! Change upper case charaters to lower case in all elements
       integer(kind=kind(1)) :: i

      do i = 1,size(self)
         call to_lower_case_(self(i))
      end do

   end subroutine

   subroutine to_upper_case(self)
    character(*), dimension(:) :: self
    ! Change lower case charaters to upper case in all elements
       integer(kind=kind(1)) :: i

      do i = 1,size(self)
         call to_upper_case_(self(i))
      end do

   end subroutine

   recursive subroutine quick_sort(self)
    character(*), dimension(:) :: self
    ! Sort the vector into fortran dictionary order
      character(len=len(self(1))), dimension(:), pointer :: smaller,larger
      integer(kind=kind(1)) :: n, ns, ne, nl
      character(len(self(1))) :: chosen

      n = size(self)
      if (n>=2) then
         chosen = self(1)
         ns = count(self<chosen)
         nl = count(self>chosen)
         ne = n - ns - nl
         call create_(smaller,ns)
         call create_(larger,nl)
         smaller = pack(self,self<chosen)
         larger  = pack(self,self>chosen)
         call quick_sort_(smaller)
         call quick_sort_(larger)
         self(1:ns)       = smaller
         self(ns+1:ns+ne) = chosen
         self(ns+ne+1:)   = larger
         call destroy_(larger)
         call destroy_(smaller)
      end if

   end subroutine

   subroutine shrink(self,dim)
    character(*), dimension(:) :: self
    ! Shrinks self to dimension dim.  Contents are retained.
     pointer :: self
     integer(kind=kind(1)), intent(in) :: dim
     character(len=len(self(1))), dimension(:), pointer :: old

   call ensure_(tonto,associated(self),"STRVEC:shrink ... no self array")
   call ensure_(tonto,dim<=size(self),"STRVEC:shrink ... dim too large")
     if (dim==size(self)) then;   return; end if
     old => self
     nullify(self)
     call create_(self,dim)
     self=old(1:dim)
     call destroy_(old)

   end subroutine

   subroutine expand(self,dim)
    character(*), dimension(:) :: self
    ! Expands self to dimension dim.  Contents are retained.
    ! Elements added are set to zero.
     pointer :: self
     integer(kind=kind(1)), intent(in) :: dim
     character(len=len(self(1))), dimension(:), pointer :: old
     integer(kind=kind(1)) :: old_dim

     if (.not. associated(self)) then
        call create_(self,dim)
        self = " "
     else
        call ensure_(tonto,dim>=size(self),"STRVEC:expand ... dim not large enough")
        old => self
        old_dim = size(old)
        nullify(self)
        call create_(self,dim)
        self(        1:old_dim) = old
        self(old_dim+1:dim    ) = " "
        call destroy_(old)
     end if

   end subroutine

   subroutine append(self,v)
    character(*), dimension(:) :: self
    ! Expands self and appends the contents of vector "v".
     pointer :: self
     character(*), dimension(:), intent(in) :: v
    ! The following code is inherited from INTRINSICVEC
     integer(kind=kind(1)) :: dim

     if (.not. associated(self)) then; dim = 0
     else;                 dim = size(self)
     end if
     call expand_(self,dim+size(v))
     self(dim+1:) = v

   end subroutine

   subroutine append_1(self,value)
    character(*), dimension(:) :: self
    ! Expands self by 1, and appends the single scalar "value" onto the end.
     pointer :: self
     character(*), intent(in) :: value
    ! The following code is inherited from INTRINSICVEC
     integer(kind=kind(1)) :: dim

     if (.not. associated(self)) then; dim = 0
     else;                 dim = size(self)
     end if
     call expand_(self,dim+1)
     self(dim+1) = value

   end subroutine

   subroutine append_only_if_unique(self,value)
    character(*), dimension(:) :: self
    ! Expands self by 1, and appends the single scalar "value" onto the end, but
    ! only if the "value" is unique
     pointer :: self
     character(*), intent(in) :: value
    ! The following code is inherited from INTRINSICVEC

     if (any(self==value)) then;   return; end if
     call append_(self,value)

   end subroutine

   subroutine remove_repetitions(self)
    character(*), dimension(:) :: self
    ! Sort through the vector and remove repeated elements which come later in
    ! the list.  NOTE: the vector may shrink
      pointer :: self
      character(len=len(self(1))), dimension(:), pointer :: unique
      integer(kind=kind(1)) :: i,n

   call ensure_(tonto,associated(self),"STRVEC:remove_repetitions ... no vector")
      if (size(self)==1) then;   return; end if
      call create_(unique,size(self))
      n = 1
      unique(1) = self(1)
      do i = 2,size(self)
         if (any(self(i)==unique(1:n))) cycle
         n = n + 1
         unique(n) = self(i)
      end do
      call destroy_(self)
      call create_(self,n)
      self = unique(1:n)
      call destroy_(unique)

   end subroutine

   function has_repetitions(self) result(res)
    character(*), dimension(:) :: self
    ! Return .true. if self has at least one repeated element.
      logical(kind=kind(.true.)) :: res
      character(len=len(self(1))), dimension(:), pointer :: unique
      integer(kind=kind(1)) :: i,n

      res = .false.
      if (size(self)==1) then;   return; end if
      call create_(unique,size(self))
      n = 1
      unique(1) = self(1)
      do i = 2,size(self)
         if (any(self(i)==unique(1:n))) then
            res = .true.
         else
            n = n + 1
            unique(n) = self(i)
         end if
      end do
      call destroy_(unique)

   end function

   subroutine prepend(self,v)
    character(*), dimension(:) :: self
    ! Prepend the vector "v" to "self". "self" is expanded.
     pointer :: self
     character(*), dimension(:), intent(in) :: v
    ! The following code is inherited from INTRINSICVEC
     integer(kind=kind(1)) :: dim,dimv

     dim  = size(self)
     dimv = size(v)
     call expand_(self,dim+dimv)
     self(dimv+1:    ) = self(1:dim)
     self(     1:dimv) = v

   end subroutine

   subroutine prepend_1(self,value)
    character(*), dimension(:) :: self
    ! Prepend an single "value" to "self". "self" is expanded.
     pointer :: self
     character(*), intent(in) :: value
    ! The following code is inherited from INTRINSICVEC
     integer(kind=kind(1)) :: dim

     dim = size(self)
     call expand_(self,dim+1)
     self(2:) = self(1:dim)
     self(1 ) = value

   end subroutine

   function has_any_included_in(self,string,at_start) result(res)
    character(*), dimension(:) :: self
    ! Return .true. if self has any element included in "string" which starts at
    ! the start of the "string", provided "at_start" is .true.; otherwise
    ! returns .true. even if the match was not at the start.
      character(*), intent(in) :: string
      logical(kind=kind(.true.)), optional :: at_start
      logical(kind=kind(.true.)) :: res
      logical(kind=kind(.true.)) :: first

      first = .false.
      if (present(at_start)) first = at_start
      if (first) then
         res = any(index(spread(string,1,size(self)),self) == 1)
      else
         res = any(index(spread(string,1,size(self)),self) /= 0)
      end if

   end function

   function index_of_first_included_in(self,string) result(res)
    character(*), dimension(:) :: self
    ! Return the index of the first element in self which is included in
    ! "string", or zero otherwise.
      character(*), intent(in) :: string
      integer(kind=kind(1)) :: res
      integer(kind=kind(1)) :: i

      res = 0
      do i = 1,size(self)
         res = index(string,self(i))
         if (res>0) exit
      end do

   end function

   function has_any_including(self,string) result(res)
    character(*), dimension(:) :: self
    ! Return .true. if self has any element which includes "string".
      character(*), intent(in) :: string
      logical(kind=kind(.true.)) :: res

      res = any(index(self,spread(string,1,size(self))) /= 0)

   end function

   function includes(self,string,at_start) result(res)
    character(*), dimension(:) :: self
    ! Return .true. for a particular element, if that element of self includes "string".
    ! Returns .false. if no element matches. If "at_start" is present and .true., then the
    ! result is .true. only if the item matches at the start of the string.
      character(*), intent(in) :: string
      logical(kind=kind(.true.)), optional :: at_start
      logical(kind=kind(.true.)), dimension(size(self)) :: res
      logical(kind=kind(.true.)) :: first

      first = .false.
      if (present(at_start)) first = at_start
      if (first) then
         res = index(self,spread(string,1,size(self))) == 1
      else
         res = index(self,spread(string,1,size(self))) /= 0
      end if

   end function

   function index_of_first_that_includes(self,string) result(res)
    character(*), dimension(:) :: self
    ! Return the index of the first element of self that includes "string".
    ! Returns 0 if no element matches.
      character(*), intent(in) :: string
      integer(kind=kind(1)) :: res
      integer(kind=kind(1)) :: i

      do i = 1,size(self)
         res = index(self(i),string)
         if (res==0) cycle
         res = i
         exit
      end do

   end function

   function index_of(self,string) result(res)
    character(*), dimension(:) :: self
    ! Return the first index of the "string" in self.
    ! Returns 0 if no element matches.
      character(*), intent(in) :: string
      integer(kind=kind(1)) :: res
      integer(kind=kind(1)) :: i

      res = 0
      do i = 1,size(self)
         if (self(i)/=string) cycle
         res = i
         exit
      end do

   end function

   function index_of_matching_bracket(self,symbol) result(res)
    character(*), dimension(:) :: self
    ! Return the first index of the matching bracket "symbol" in self.
    ! The first element of self need not be an opening bracket symbol.
      character(*), intent(in) :: symbol
      integer(kind=kind(1)) :: res
      character(len=1), dimension(4) :: opening = (/"{","(","[","<"/)
      character(len=1), dimension(4) :: closing = (/"}",")","]",">"/)
      character(1) :: op,cl
      integer(kind=kind(1)) :: i,s,n

      call ensure_(tonto,any(symbol==opening),"STRVEC:index_of_matching_bracket ... unrecognised open bracket symbol")
      call ensure_(tonto,index_of_(self,symbol)>0,"STRVEC:index_of_matching_bracket ... no open bracket symbol in self")
      call ensure_(tonto,index_of_(self,symbol)<size(self),"STRVEC:index_of_matching_bracket ... open bracket is at end of se&
&lf")
      op = symbol
      cl = closing(index_of_(opening,symbol))
      s = index_of_(self,op)
      n = 0
      do i = s+1,size(self)
         if (self(i)==op) n = n + 1
         if (self(i)==cl .and. n==0) exit
         if (self(i)==cl .and. n>0) n = n - 1
      end do
      call ensure_(tonto,n==0,"STRVEC:index_of_matching_bracket ... unmatching number of closing bracket symbols")
      res = i

   end function

end
