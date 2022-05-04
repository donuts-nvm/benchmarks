!---------------------------------------------------------------------------
!
!  Time: timing routines, such as output date, time execution, etc.
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
! $Id: time.foo,v 1.16.2.5 2003/11/13 05:33:55 reaper Exp $
!---------------------------------------------------------------------------

module TIME_MODULE

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

   public    time_taken_
   interface time_taken_
      module procedure time_taken
   end interface

   private    from_julian_
   interface from_julian_
      module procedure from_julian
   end interface

   private    to_julian_
   interface to_julian_
      module procedure to_julian
   end interface

   public    time_to_str_
   interface time_to_str_
      module procedure time_to_str
   end interface

   private    number_with_units_
   interface number_with_units_
      module procedure number_with_units
   end interface

   public    stop_
   interface stop_
      module procedure stop
   end interface

   private    subtract_times_
   interface subtract_times_
      module procedure subtract_times
   end interface

   public    current_cpu_time_
   interface current_cpu_time_
      module procedure current_cpu_time
   end interface

   public    current_
   interface current_
      module procedure current
   end interface

   public    elapsed_
   interface elapsed_
      module procedure elapsed
   end interface

   private    time_to_seconds_
   interface time_to_seconds_
      module procedure time_to_seconds
   end interface

   public    current_time_
   interface current_time_
      module procedure current_time
   end interface

   public    elapsed_time_to_str_
   interface elapsed_time_to_str_
      module procedure elapsed_time_to_str
   end interface

   public    start_time_
   interface start_time_
      module procedure start_time
   end interface

   public    stop_time_
   interface stop_time_
      module procedure stop_time
   end interface

   public    cpu_time_taken_
   interface cpu_time_taken_
      module procedure cpu_time_taken
   end interface

   public    elapsed_time_in_seconds_
   interface elapsed_time_in_seconds_
      module procedure elapsed_time_in_seconds
   end interface

   public    start_
   interface start_
      module procedure start
   end interface

   private    seconds_to_time_
   interface seconds_to_time_
      module procedure seconds_to_time
   end interface

   type(time_type), public :: std_time

   public reset_; interface reset_
    module procedure start
  end interface

contains

   subroutine start(self)
    type(time_type) :: self
    ! starts the counter

     self%started   = current_(self)
     self%start_cpu = current_cpu_time_(self)
     self%stopped = 0

   end subroutine

   subroutine stop(self)
    type(time_type) :: self
    ! Stop the counter.

     self%stopped  = current_(self)
     self%stop_cpu = current_cpu_time_(self)

   end subroutine

   function current(self) result(res)
    type(time_type) :: self
    ! returns current time
     integer(kind=kind(1)), dimension(5) :: res
     integer(kind=kind(1)), dimension(8) :: time
     integer(kind=kind(1)) :: y,m,d,j

     call date_and_time(values=time)
     y = time(1)
     m = time(2)
     d = time(3)
     call to_julian_(self,d,m,y,j)
     res(1) = j        ! Julian day
     res(2) = time(5)  ! hour
     res(3) = time(6)  ! minute
     res(4) = time(7)  ! second
     res(5) = time(8)  ! millisecond

   end function

   function current_cpu_time(self) result(res)
    type(time_type) :: self
    ! What the current CPU_TIME is.
     real(kind=kind(1.0d0)) :: res
     real(kind=kind(1.0d0)) :: time

     time = 0.0d0
     call cpu_time(time)
     res = time

   end function

   function elapsed(self) result(time)
    type(time_type) :: self
    ! returns elapsed time
     integer(kind=kind(1)), dimension(5) :: time

     time = subtract_times_(self, self%stopped , self%started)

   end function

   function elapsed_time_in_seconds(self) result(secs)
    type(time_type) :: self
    ! returns elapsed time
     real(kind=kind(1.0d0)) :: secs
     integer(kind=kind(1)), dimension(5) :: time

     time = subtract_times_(self, self%stopped , self%started)
     secs = time_to_seconds_(self,time)

   end function

! *********************************************************************
! return time strings.
! *********************************************************************

   function current_time(self) result(res)
    type(time_type) :: self
    !
     character(40) :: res

     res = "The current time is " // time_to_str_(self, current_(self) ) // "."

   end function

   function start_time(self) result(res)
    type(time_type) :: self
    !
     character(37) :: res

      !SPEC res = "Timer started at " // time_to_str_(self, self%started ) // "."
     res = "Timer started at XX" // "."

   end function

   function stop_time(self) result(res)
    type(time_type) :: self
    !
     character(37) :: res

     res = "Timer stopped at " // time_to_str_(self, self%stopped ) // "."

   end function

   function time_taken(self,task) result(res)
    type(time_type) :: self
    ! Returns time taken. "Time taken is ..."
    ! If task given, "Time taken for "task" is ..."
     character(132) :: res
     character(*), optional :: task

     call stop_(self)
     if (present(task)) then
        !SPEC res = "Wall-clock time taken for " // trim(task) // " is " // &
        !SPEC       trim( elapsed_time_to_str_(self, elapsed_(self) )) // "."
       res = "Wall-clock time taken for " // trim(task) // " is " // &
              "XX."
     else
     res = "Wall-clock time taken is " // trim( elapsed_time_to_str_(self, elapsed_(self) )) // "."
     end if

   end function

   function cpu_time_taken(self,task) result(res)
    type(time_type) :: self
    ! Returns cpu time taken. "CPU time taken is ... CPU seconds."
    ! If task given, "CPU time taken for "task" is ... CPU seconds."
     character(*), optional :: task
     character(128) :: res
     real(kind=kind(1.0d0)) :: time,time1
     character(132) :: time_str

     call stop_(self)
     time = self%stop_cpu - self%start_cpu
     write(time_str,'(f15.3)') time
     if (present(task)) then
        !SPEC res = "CPU time taken for " // trim(task) // " is " // &
        !SPEC       trim(adjustl(time_str)) // " CPU seconds."
       res = "CPU time taken for " // trim(task) // " is " // &
               "XX CPU seconds."
     else
       res = "CPU time taken is " // trim(adjustl(time_str)) // " CPU seconds."
     end if

   end function

   function elapsed_time_to_str(self,time) result(res)
    type(time_type) :: self
    ! Returns the elapsed time as a string.
    ! Formatted as years, months, days, hours, min, sec, msec.
    ! If time is greater than a day, does not output min or smaller.
     integer(kind=kind(1)), dimension(5), intent(in) :: time
     character(128) :: res
     logical(kind=kind(.true.)) :: comma

     comma = .false.
     res = " "
     res =              number_with_units_(self,time(1),"day",comma)
     res = trim(res) // number_with_units_(self,time(2),"hour",comma)
     if (time(1) == 0) then
       res = trim(res) // number_with_units_(self,time(3),"minute",comma)
       if (time(2) == 0) then
         res = trim(res) // number_with_units_(self,time(4),"second",comma)
         res = trim(res) // number_with_units_(self,time(5),"millisecond",comma)
       end if
     end if

   end function

   function time_to_str(self,time) result(res)
    type(time_type) :: self
    ! Return a string containing the time.
    ! Formatted as DD/MM/YYYY HH:MM:SS
     integer(kind=kind(1)), dimension(5), intent(in) :: time
     character(19) :: res
     character(2) :: day,month,hour,min,sec
     character(4) :: year
     integer(kind=kind(1)) :: i,y,m,d

     call from_julian_(self,d,m,y,time(1))
     res = " "
     write(day,fmt='(i2)') d
     write(month,fmt='(i2)') m
     write(year,fmt='(i4)') y
     write(hour,fmt='(i2)') time(2)
     write(min,fmt='(i2)') time(3)
     write(sec,fmt='(i2)') time(4)
     res = day //"/"// month //"/"// year //" "// hour //":"// min //":"// sec
     do i=1,19
       if (res(i:i) == " ") res(i:i) = "0"
     end do
     res(11:11) = " "

   end function

! *********************************************************************
! private routines
! *********************************************************************

   function number_with_units(self,number,unit,comma) result(res)
    type(time_type) :: self
    ! Returns the number with its units, if the number is non-zero.
    ! Inserts a comma if comma is true.  If the number is non-zero,
    ! then comma is set to true.
     intent(in) :: self
     integer(kind=kind(1)), intent(in) :: number
     character(*), intent(in) :: unit
     logical(kind=kind(.true.)) :: comma
     character(128) :: res
     integer(kind=kind(1)) :: i
     character(128) :: res1

     res = " "
     if (number /= 0) then
       res1 = " "
       write(res1,'(i 20)') number
       res1 = adjustl(res1)
       i = 1
       if (comma) then            ! insert comma at start if needed.
         res(1:2) = ", "
         i = 3
       end if
       res(i:) = trim(res1) // " " // unit
       if (number /= 1) then      ! add "s" to "unit"s if number not 1.
         i=len_trim(res)+1
         res(i:i) = "s"
       end if
       comma = .true.
     end if

   end function

   function subtract_times(self,time1,time2) result(res)
    type(time_type) :: self
    ! result = time1 - time2
     intent(in) :: self
     integer(kind=kind(1)), dimension(5), intent(in) :: time1,time2
     integer(kind=kind(1)), dimension(5) :: res
     real(kind=kind(1.0d0)) :: secs,secs1,secs2

     secs1 = time_to_seconds_(self,time1)
     secs2 = time_to_seconds_(self,time2)
     secs = secs1 - secs2
     res = seconds_to_time_(self,secs)

   end function

   function seconds_to_time(self,secs) result(res)
    type(time_type) :: self
    ! Returns the number of days, hours, minutes, seconds, milliseconds from the
    ! number of seconds.
     intent(in) :: self
     integer(kind=kind(1)), dimension(5) :: res
     real(kind=kind(1.0d0)), intent(in) :: secs
     real(kind=kind(1.0d0)) :: sex

     sex = abs(secs)
     res(1) = floor( sex / 86400 )  ! days
     sex = sex - res(1) * 86400
     res(2) = floor( sex / 3600 )   ! hours
     sex = sex - res(2) * 3600
     res(3) = floor( sex / 60 )     ! minutes
     sex = sex - res(3) * 60
     res(4) = floor(sex)            ! seconds
     sex = sex - res(4)
     res(5) = 1000*sex              ! milliseconds
     if (secs < 0) res = - res

   end function

   function time_to_seconds(self,time) result(res)
    type(time_type) :: self
    ! Returns the number of seconds from the days, hours, minutes, seconds, and
    ! milliseconds.
     intent(in) :: self
     integer(kind=kind(1)), dimension(5), intent(in) :: time
     real(kind=kind(1.0d0)) :: res

     res = 86400d0*time(1)+3600d0*time(2)+60d0*time(3)+time(4)+time(5)/1000d0

   end function

   subroutine to_julian(self,d,m,y,j)
    type(time_type) :: self
    ! Converts the days, months, years, into the Julian date.
     intent(in) :: self
     integer(kind=kind(1)), intent(in) :: d,m,y
      integer(kind=kind(1)), intent(out) :: j
     integer(kind=kind(1)) :: mm,yy,c

     mm = m
     yy = y
     if (mm>2) then
       mm = mm - 3
     else
       mm = mm + 9
       yy = yy - 1
     end if
     c = yy / 100
     yy = yy - 100 * c
     j = (146097*c)/4 + (1461*yy)/4 + (153*mm+2)/5 + d + 1721119

   end subroutine

   subroutine from_julian(self,d,m,y,j)
    type(time_type) :: self
    ! Converts the days, months, years, from the Julian date.
     intent(in) :: self
      integer(kind=kind(1)), intent(in) :: j
     integer(kind=kind(1)), intent(out) :: d,m,y
     integer(kind=kind(1)) :: jj

     jj=j - 1721119
     y = (4*jj-1)/146097
     jj = 4*jj - 1 - 146097 * y
     d = jj/4
     jj = (4*d+3)/1461
     d = 4*d + 3 - 1461*jj
     d = (d+4)/4
     m = (5*d-3)/153
     d = 5*d - 3 - 153*m
     d = (d+5)/5
     y = 100*y + jj
     if (m<10) then
       m = m + 3
     else
       m = m - 9
       y = y + 1
     end if

   end subroutine

end
