!---------------------------------------------------------------------------
!
! UNITNUMBER:  Keeps a list of the unit numbers used for I/O.
!
! Whenever you use a new unit, use "get" to get a unique number for it.
! When finished with the unit, use "free" so that the number becomes
! available for later use.
!
! The numbers start at UNITNUMBER_STARTING_UNIT, not one.
!
! Copyright (C) Daniel Grimwood, 1998
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
! $Id: unitnumber.foo,v 1.10.2.1 2003/11/13 05:33:02 reaper Exp $
!
!---------------------------------------------------------------------------

module UNITNUMBER_MODULE

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

   public    free_
   interface free_
      module procedure free
   end interface

   public    flush_buffer_
   interface flush_buffer_
      module procedure flush_buffer
   end interface

   public    get_
   interface get_
      module procedure get
   end interface

   integer(kind=kind(1)), dimension(:), pointer :: unitlist

contains

   subroutine get(self,unit)
    type(unitnumber_type) :: self
    ! Returns the next unused unit number, and updates the unitlist.
     integer(kind=kind(1)), intent(out) :: unit
     integer(kind=kind(1)), dimension(:), pointer :: old
     integer(kind=kind(1)) :: oldlength

     if (associated(unitlist)) then
       oldlength=size(unitlist)
       unit=10-1
       do
         unit=unit+1
         if (any(unitlist == unit)) then
           cycle
         else
           exit
         end if
       end do
        ! expand the array by one.
       old => unitlist
       nullify(unitlist)
       allocate(unitlist(oldlength+1))
       unitlist(1:oldlength) = old
       unitlist(oldlength+1) = unit
     else
       allocate(unitlist(1))
       unit=10
       unitlist(1)=unit
     end if

   end subroutine

   subroutine free(self,unit)
    type(unitnumber_type) :: self
    ! Removes the unit number from the unitlist.
     integer(kind=kind(1)), intent(in) :: unit
     integer(kind=kind(1)), dimension(:), pointer :: old
     integer(kind=kind(1)) :: oldlength,position,i

     if (unit<10) then;   return; end if
     oldlength=size(unitlist)
     call ensure_(tonto,oldlength>0,"UNITNUMBER:free ... no unitnumber array")
     if (oldlength==1) then
       deallocate(unitlist)
     else
       do i=1,oldlength
         if (unitlist(i)==unit) then
           position=i
           exit
         else
           cycle
         end if
       end do
       do i=position,oldlength-1
         unitlist(i)=unitlist(i+1)
       end do
        ! shrink the array by 1.
       old => unitlist
       nullify(unitlist)
       allocate(unitlist(oldlength-1))
       unitlist = old(1:oldlength-1)
     end if

   end subroutine

   subroutine flush_buffer(self)
    type(unitnumber_type) :: self
    ! Flush the buffer if need be.

     call flush_buffer_(tonto,self%unit)

   end subroutine

end
