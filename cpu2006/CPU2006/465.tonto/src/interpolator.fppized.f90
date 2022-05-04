!------------------------------------------------------------------------------
!
! INTERPOLATOR:
!
! For interpolating values from a 1-D table.
!
! The module takes as input some "data_point"'s with their assigned "values".
! Any data value is then assigned a value by interpolating between the two
! nearest data points.
!
! The module allows for even-spaced or uneven-spaced data. In the former case
! all the "data_point"'s are not required: only the first data point and the
! "spacing".
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
! $Id: interpolator.foo,v 1.1.2.8 2004/04/21 09:12:54 reaper Exp $
!
!------------------------------------------------------------------------------

module INTERPOLATOR_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

   use REALVEC_MODULE, only: shrink_
   use REALVEC_MODULE, only: range_
   use REALVEC_MODULE, only: create_
   use REALVEC_MODULE, only: quick_sort_
   use REALVEC_MODULE, only: create_copy_
   use REALVEC_MODULE, only: destroy_

   use INTVEC_MODULE, only: create_
   use INTVEC_MODULE, only: destroy_

   use STR_MODULE, only: to_lower_case_

   use TEXTFILE_MODULE, only: stdin
   use TEXTFILE_MODULE, only: stdout
   use TEXTFILE_MODULE, only: set_default_units_
   use TEXTFILE_MODULE, only: text_
   use TEXTFILE_MODULE, only: next_str_
   use TEXTFILE_MODULE, only: skip_next_item_
   use TEXTFILE_MODULE, only: next_item_
   use TEXTFILE_MODULE, only: show_
   use TEXTFILE_MODULE, only: put_
   use TEXTFILE_MODULE, only: read_
   use TEXTFILE_MODULE, only: flush_
   use TEXTFILE_MODULE, only: reverted_
   use TEXTFILE_MODULE, only: read_ptr_
   use TEXTFILE_MODULE, only: dash_

   use REAL_MODULE, only: is_in_range_

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

   public    set_even_spaced_data_
   interface set_even_spaced_data_
      module procedure set_even_spaced_data
   end interface

   public    set_defaults_
   interface set_defaults_
      module procedure set_defaults
   end interface

   public    nullify_ptr_part_
   interface nullify_ptr_part_
      module procedure nullify_ptr_part
   end interface

   public    create_
   interface create_
      module procedure create
   end interface

   public    create_copy_
   interface create_copy_
      module procedure create_copy
   end interface

   public    set_spacing_
   interface set_spacing_
      module procedure set_spacing
   end interface

   public    read_kind_
   interface read_kind_
      module procedure read_kind
   end interface

   public    destroy_ptr_part_
   interface destroy_ptr_part_
      module procedure destroy_ptr_part
   end interface

   public    set_data_values_
   interface set_data_values_
      module procedure set_data_values
      module procedure set_data_values_1
      module procedure set_data_values_2
   end interface

   public    set_data_points_
   interface set_data_points_
      module procedure set_data_points
   end interface

   public    set_even_spaced_data_points_
   interface set_even_spaced_data_points_
      module procedure set_even_spaced_data_points
      module procedure set_even_spaced_data_points_1
   end interface

   public    read_keywords_
   interface read_keywords_
      module procedure read_keywords
   end interface

   public    destroy_
   interface destroy_
      module procedure destroy
   end interface

   public    read_junk_
   interface read_junk_
      module procedure read_junk
   end interface

   public    read_spacing_
   interface read_spacing_
      module procedure read_spacing
   end interface

   public    values_for_
   interface values_for_
      module procedure values_for
   end interface

   public    read_data_points_
   interface read_data_points_
      module procedure read_data_points
   end interface

   public    value_for_
   interface value_for_
      module procedure value_for
   end interface

   public    read_data_values_
   interface read_data_values_
      module procedure read_data_values
   end interface

   public    process_keyword_
   interface process_keyword_
      module procedure process_keyword
   end interface

   public    read_units_
   interface read_units_
      module procedure read_units
   end interface

   public    put_
   interface put_
      module procedure put
   end interface

   public    finalise_
   interface finalise_
      module procedure finalise
   end interface

   public    last_data_point_
   interface last_data_point_
      module procedure last_data_point
   end interface

   public    set_first_data_point_
   interface set_first_data_point_
      module procedure set_first_data_point
   end interface

   public    is_even_spaced_
   interface is_even_spaced_
      module procedure is_even_spaced
   end interface

   public    first_data_point_
   interface first_data_point_
      module procedure first_data_point
   end interface

   public    copy_
   interface copy_
      module procedure copy
   end interface

contains

   subroutine create(self,spacing,first)
    type(interpolator_type) :: self
    ! Create the object
     pointer :: self
     real(kind=kind(1.0d0)), optional :: spacing,first

     nullify(self)
     allocate(self)

     call nullify_ptr_part_(self)
     call set_defaults_(self,spacing,first)

   end subroutine

   subroutine create_copy(self,object)
    type(interpolator_type) :: self
    ! Create a copy of object
     type(interpolator_type) :: object
     pointer :: self
    ! The following code is inherited from OBJECT

     call create_(self)
     call copy_(self,object)

   end subroutine

   subroutine copy(self,c)
    type(interpolator_type) :: self
    ! Copy the contents of "c" to self.
     type(interpolator_type), intent(in) :: c

     self = c
     if (associated(c%data_point)) &
        call create_copy_(self%data_point,c%data_point)
     if (associated(c%data_value)) &
        call create_copy_(self%data_value,c%data_value)

   end subroutine

   subroutine destroy(self)
    type(interpolator_type) :: self
    ! Destroy an object
      pointer :: self
    ! The following code is inherited from OBJECT

      if (associated(self)) then
        call destroy_ptr_part_(self)
        deallocate(self)

      end if

   end subroutine

   subroutine nullify_ptr_part(self)
    type(interpolator_type) :: self
    ! Nullify the pointer parts

      nullify(self%data_point)
      nullify(self%data_value)

   end subroutine

   subroutine destroy_ptr_part(self)
    type(interpolator_type) :: self
    ! Destroy the pointer parts

      call destroy_(self%data_point)
      call destroy_(self%data_value)

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

   subroutine set_defaults(self,spacing,first)
    type(interpolator_type) :: self
    ! Set up a defaults
      real(kind=kind(1.0d0)), optional :: spacing,first

      self%n_data = 0
      call destroy_(self%data_point)
      call destroy_(self%data_value)
      self%spacing = 0.0d0
      self%finalised = .false.
      if (present(spacing)) call set_spacing_(self,spacing)
      if (present(first))   call set_first_data_point_(self,first)

   end subroutine

   subroutine set_spacing(self,spacing)
    type(interpolator_type) :: self
    ! Set the "spacing" of an even-spaced interpolator
      real(kind=kind(1.0d0)) :: spacing

   call ensure_(tonto,spacing>0.0d0,"INTERPOLATOR:set_spacing ... spacing must be +ve")
      self%spacing = spacing

   end subroutine

   subroutine set_first_data_point(self,first)
    type(interpolator_type) :: self
    ! Set the "first" datum of an even-spaced interpolator
      real(kind=kind(1.0d0)) :: first

   call ensure_(tonto,self%spacing>0.0d0,"INTERPOLATOR:set_first_data_point ... spacing must be +ve")
      call destroy_(self%data_point)
      call create_(self%data_point,1)
      self%data_point(1) = first

   end subroutine

   function is_even_spaced(self) result(res)
    type(interpolator_type) :: self
    ! Returns .true. if the interpolator uses even spaced data points.
      logical(kind=kind(.true.)) :: res

      res = self%spacing > 0.0d0

   end function

   function first_data_point(self) result(res)
    type(interpolator_type) :: self
    ! Returns the first data point
      real(kind=kind(1.0d0)) :: res

   call ensure_(tonto,self%finalised,"INTERPOLATOR:first_data_point ... Not finalised")
   call ensure_(tonto,self%n_data>0,"INTERPOLATOR:first_data_point ... No data")
   call ensure_(tonto,associated(self%data_point),"INTERPOLATOR:first_data_point ... No data")
      res = self%data_point(1)

   end function

   function last_data_point(self) result(res)
    type(interpolator_type) :: self
    ! Returns the last data point
      real(kind=kind(1.0d0)) :: res

   call ensure_(tonto,self%finalised,"INTERPOLATOR:last_data_point ... Not finalised")
   call ensure_(tonto,self%n_data>0,"INTERPOLATOR:last_data_point ... No data")
   call ensure_(tonto,associated(self%data_point),"INTERPOLATOR:last_data_point ... No data")
      res = self%data_point(self%n_data)

   end function

!  *************
!  Input methods
!  *************

   recursive subroutine read_keywords(self)
    type(interpolator_type) :: self
    ! Read data from "stdin" using keyword style input.
    ! The following code is inherited from OBJECT
     character(128) :: word

     call ensure_(tonto,next_item_(stdin)=="{","INTERPOLATOR:read_keywords ... expecting open bracket symbol, {")
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
    type(interpolator_type) :: self
    ! Process command "keyword". Any required data needed by the "keyword" is
    ! inputted from "stdin".
      character(*) :: keyword
      character(128) :: word

      word = keyword
      call to_lower_case_(word)
      select case (word)
         case ("}               ")   ! exit case
         case ("data_points=    "); call read_data_points_(self)
         case ("data_values=    "); call read_data_values_(self)
         case ("kind=           "); call read_kind_(self)
         case ("put             "); call put_(self)
         case ("spacing=        "); call read_spacing_(self)
         case ("units=          "); call read_units_(self)
         case default;         allocate(tonto%known_keywords(7))
         tonto%known_keywords(1) = "}               "
         tonto%known_keywords(2) = "data_points=    "
         tonto%known_keywords(3) = "data_values=    "
         tonto%known_keywords(4) = "kind=           "
         tonto%known_keywords(5) = "put             "
         tonto%known_keywords(6) = "spacing=        "
         tonto%known_keywords(7) = "units=          "
         call unknown_(tonto,word,"INTERPOLATOR:process_keyword")
         deallocate(tonto%known_keywords)
      end select

   end subroutine

   subroutine read_units(self)
    type(interpolator_type) :: self
    ! Read a string which describes the units to be used
    ! The following code is inherited from OBJECT

      call set_default_units_(stdin,next_str_(stdin))

   end subroutine

   subroutine read_junk(self)
    type(interpolator_type) :: self
    ! Read in a junk string, useful for ignoring a field
    ! The following code is inherited from OBJECT

      call skip_next_item_(stdin)

   end subroutine

   subroutine read_kind(self)
    type(interpolator_type) :: self
    ! Read the kind of interpolation to use.

      call read_(stdin,self%interp_kind)
      call to_lower_case_(self%interp_kind)
      select case (self%interp_kind)
         case ("linear")
         case ("logarithmic")
         case default; allocate(tonto%known_keywords(2))
         tonto%known_keywords(1) = "linear"
         tonto%known_keywords(2) = "logarithmic"
         call unknown_(tonto,self%interp_kind,"INTERPOLATOR:read_kind")
         deallocate(tonto%known_keywords)
      end select

   end subroutine

   subroutine read_data_points(self)
    type(interpolator_type) :: self
    ! Read in the data points.

      call read_ptr_(stdin,self%data_point)
      if (is_even_spaced_(self)) then
         call ensure_(tonto,size(self%data_point)==1,"INTERPOLATOR:read_data_points ... specify only first data point for eve&
&n spaced interpolation")
      end if

   end subroutine

   subroutine read_data_values(self)
    type(interpolator_type) :: self
    ! Read in the data values.

      call read_ptr_(stdin,self%data_point)

   end subroutine

   subroutine read_spacing(self)
    type(interpolator_type) :: self
    ! Read in the spacing
      real(kind=kind(1.0d0)) :: spacing

   call ensure_(tonto,.not. associated(self%data_point) .or. size(self%data_point)==1,"INTERPOLATOR:read_spacing ... can't ha&
&ve more than one data point")
      call read_(stdin,spacing)
      call set_spacing_(self,spacing)

   end subroutine

   subroutine finalise(self)
    type(interpolator_type) :: self
    ! Check everything is OK after reading data. Saturation must exist but it is
    ! applied into the RGB triples and then destroyed.
      integer(kind=kind(1)), dimension(:), pointer :: order

      call ensure_(tonto,associated(self%data_point),"INTERPOLATOR:finalise ...  no data_point's")
      call ensure_(tonto,associated(self%data_value),"INTERPOLATOR:finalise ...  no data_value's")
      if (is_even_spaced_(self)) then
         self%n_data = size(self%data_value)
         call set_even_spaced_data_points_(self)
      else
         call ensure_(tonto,size(self%data_point)==size(self%data_value),"INTERPOLATOR:finalise ... inconsistent data/value l&
&engths")
         call ensure_(tonto,size(self%data_point)>2,"INTERPOLATOR:finalise ... not enough data_point's")
         self%n_data = size(self%data_point)
         call create_(order,self%n_data)
          ! Re-order from smallest to largest
         call quick_sort_(self%data_point,order)
         self%data_point = self%data_point(order)
         self%data_value = self%data_value(order)
         call destroy_(order)
      end if
      self%finalised = .true.

   end subroutine

   subroutine set_even_spaced_data_points(self)
    type(interpolator_type) :: self
    ! Set the ".data_point" to be even-spaced, assuming .n_data has been set:
    ! this can only be done when the ".data_point" array contains one element.
      integer(kind=kind(1)) :: i
      real(kind=kind(1.0d0)) :: p

   call ensure_(tonto,is_even_spaced_(self),"INTERPOLATOR:set_even_spaced_data_points ... not even spaced")
   call ensure_(tonto,size(self%data_point)==1,"INTERPOLATOR:set_even_spaced_data_points ... data_point.dim/=1")
   call ensure_(tonto,self%n_data>0,"INTERPOLATOR:set_even_spaced_data_points ... there is no data")
      p = self%data_point(1)
      call destroy_(self%data_point)
      call create_(self%data_point,self%n_data)
      do i = 1,self%n_data
         self%data_point(i) = p
         p = p + self%spacing
      end do

   end subroutine

   subroutine set_even_spaced_data_points_1(self,spacing,first,length)
    type(interpolator_type) :: self
    ! Set the ".data_point" to be even-spaced. In this routine .n_data must not
    ! have been set, nor ".data_values". Instead .n_data is worked out from the
    ! "first" data value, the "spacing", and the "length" of the interpolation
    ! region.
      real(kind=kind(1.0d0)) :: spacing,first,length

   call ensure_(tonto,self%n_data==0,"INTERPOLATOR:set_even_spaced_data_points_1 ... there is already data")
   call ensure_(tonto,.not. associated(self%data_value),"INTERPOLATOR:set_even_spaced_data_points_1 ... there are already dat&
&a_values")
      call set_spacing_(self,spacing)
      call set_first_data_point_(self,first)
      self%n_data = floor(length/spacing) + 1
      call set_even_spaced_data_points_(self)

   end subroutine

   subroutine set_even_spaced_data(self,first,spacing,length,func,tol)
    type(interpolator_type) :: self
    ! Set ".data_point" to be even-spaced, starting from the "first" value, with
    ! a given "spacing", extending for a total "length", and with ".data_value"
    ! to be the corresponding values evaluated using monotonically decreasing
    ! function "func". The number of data values is worked out from where
    ! function "func" is greater than "tol".
      real(kind=kind(1.0d0)) :: spacing,first,length,tol
      interface
         function func(point) result(value)
            real(kind=kind(1.0d0)) :: point
            real(kind=kind(1.0d0)) :: value
         end function
      end interface
      integer(kind=kind(1)) :: max_data
      integer(kind=kind(1)) :: i,s,n
      real(kind=kind(1.0d0)) :: p,val

   call ensure_(tonto,self%n_data==0,"INTERPOLATOR:set_even_spaced_data ... there is already data")
   call ensure_(tonto,.not. associated(self%data_value),"INTERPOLATOR:set_even_spaced_data ... there are already data_values"&
&)
   call ensure_(tonto,.not. associated(self%data_point),"INTERPOLATOR:set_even_spaced_data ... there are already data_points"&
&)
   call ensure_(tonto,spacing>0.0d0,"INTERPOLATOR:set_even_spaced_data ... spacing must be +ve")
      self%spacing = spacing
      max_data = floor((length-first)/spacing) + 1
      call create_(self%data_point,max_data)
      call create_(self%data_value,max_data)
     ! call ensure_(tonto,func(first)>tol_max,"tol_max initially too small")
     ! ! Find first data_point
     ! p = first
     ! do i = 1,max_data
     !    val = func(p)
     !    if (val<tol_max) exit
     !    p = p + spacing
     ! end
     ! call ensure_(tonto,val<tol_max,"tol_max finally too small")
     ! ! Find last data_point
     ! .data_point(1) = p - spacing
     ! .data_value(1) = val
     ! s = i
     ! n = 2
      p = first
      s = 0
      n = 1
      do i = s+1,max_data
         val = func(p)
         self%data_point(n) = p
         self%data_value(n) = val
         if (val<tol) exit
         p = p + spacing
         n = n + 1
      end do
      call ensure_(tonto,val<tol,"INTERPOLATOR:set_even_spaced_data ... data table not large enough")
      self%n_data = n-1
      call shrink_(self%data_value,self%n_data)
      call shrink_(self%data_point,self%n_data)
      if (self%interp_kind=="logarithmic") then
         call ensure_(tonto,all(self%data_value>0.0d0),"INTERPOLATOR:set_even_spaced_data ... -ve data_values cant be used wi&
&th log interpolation")
         self%data_value = log(self%data_value)
      end if
      self%finalised = .true.

   end subroutine

!  ************
!  Set routines
!  ************

   subroutine set_data_points(self,points)
    type(interpolator_type) :: self
    ! Set the data "points". NOTE: Make sure that .finalise is called after all
    ! set routines.
      real(kind=kind(1.0d0)), dimension(:) :: points

      call destroy_(self%data_point)
      call create_copy_(self%data_point,points)

   end subroutine

   subroutine set_data_values(self,values)
    type(interpolator_type) :: self
    ! Set the data "values". NOTE: Make sure that .finalise is called after all
    ! set routines.
      real(kind=kind(1.0d0)), dimension(:) :: values

      call destroy_(self%data_value)
      call create_copy_(self%data_value,values)

   end subroutine

!  *****************
!  Values for points
!  *****************

   function value_for(self,point) result(res)
    type(interpolator_type) :: self
    ! Return the interpolated value for "point".
      intent(in) :: self
      real(kind=kind(1.0d0)), intent(in) :: point
      real(kind=kind(1.0d0)) :: res
      real(kind=kind(1.0d0)) :: del,frac
      integer(kind=kind(1)) :: i1,i2

   call ensure_(tonto,self%finalised,"INTERPOLATOR:value_for ... not finalised")
      if (is_even_spaced_(self)) then  ! This is more efficient than below
         frac = (point - self%data_point(1))/self%spacing
         i1 = floor(frac)
         if (frac>i1) then
           i2 = i1+1
           if (-1<i1 .and. i2<self%n_data) then
              del  = self%data_value(i1+2) - self%data_value(i2)
              res  = self%data_value(i2) + (frac-i1)*del
              if (self%interp_kind=="logarithmic") res = exp(res)
           else
              res = 0.0d0
           end if
         else
           if (-1<i1 .and. i1<self%n_data) then
              res  = self%data_value(i1+1)
              if (self%interp_kind=="logarithmic") res = exp(res)
           else
              res = 0.0d0
           end if
         end if
      else
         if (is_in_range_(point,range_(self%data_point))) then
            i1 = count(self%data_point<=point)  ! assuming .data is ordered !
            i1 = min(i1,self%n_data-1)
            i2 = i1 + 1
            frac = (point - self%data_point(i1))/(self%data_point(i2)-self%data_point(i1))
            del  = self%data_value(i2) - self%data_value(i1)
            res  = self%data_value(i1) + frac*del
            if (self%interp_kind=="logarithmic") res = exp(res)
         else
            res = 0.0d0
         end if
      end if

   end function

   function values_for(self,points) result(values)
    type(interpolator_type) :: self
    ! Return the interpolated "values" for a series of "points".
      intent(in) :: self
      real(kind=kind(1.0d0)), dimension(:), intent(in) :: points
      real(kind=kind(1.0d0)), dimension(size(points)) :: values
      real(kind=kind(1.0d0)) :: point
      real(kind=kind(1.0d0)) :: res
      real(kind=kind(1.0d0)) :: del,frac
      integer(kind=kind(1)) :: i1,i2
      integer(kind=kind(1)) :: i

      call ensure_(tonto,self%finalised,"INTERPOLATOR:values_for ... not finalised")
      if (is_even_spaced_(self)) then  ! This is more efficient than below
        do i = 1,size(points)
          point = points(i)

          frac = (point - self%data_point(1))/self%spacing
          i1 = floor(frac)
          if (frac>i1) then
            i2 = i1+1
            if (-1<i1 .and. i2<self%n_data) then
              del  = self%data_value(i1+2) - self%data_value(i2)
              res  = self%data_value(i2) + (frac-i1)*del
              if (self%interp_kind=="logarithmic") res = exp(res)
              values(i) = res
            else
              values(i) = 0.0d0
            end if
          else
            if (-1<i1 .and. i1<self%n_data) then
              res  = self%data_value(i1+1)
              if (self%interp_kind=="logarithmic") res = exp(res)
              values(i) = res
            else
              values(i) = 0.0d0
            end if
          end if
        end do
      else
        do i = 1,size(points)
          point = points(i)
          if (is_in_range_(point,range_(self%data_point))) then
            i1 = count(self%data_point<=point)  ! assuming .data is ordered !
            i1 = min(i1,self%n_data-1)
            i2 = i1 + 1
            frac = (point - self%data_point(i1))/(self%data_point(i2)-self%data_point(i1))
            del  = self%data_value(i2) - self%data_value(i1)
            res  = self%data_value(i1) + frac*del
            if (self%interp_kind=="logarithmic") res = exp(res)
            values(i) = res
          else
            values(i) = 0.0d0
          end if
        end do
      end if

   end function

   subroutine set_data_values_1(self,func)
    type(interpolator_type) :: self
    ! Set the ".data_values" from the function "func", which returns "values"
    ! from a set of given "points".
      interface
         subroutine func(points,values)
            real(kind=kind(1.0d0)), dimension(:) :: points
            real(kind=kind(1.0d0)), dimension(:) :: values
         end subroutine
      end interface

   call ensure_(tonto,self%finalised,"INTERPOLATOR:set_data_values_1 ... not finalised")
   call ensure_(tonto,associated(self%data_point),"INTERPOLATOR:set_data_values_1 ... no data_points")
   call ensure_(tonto,size(self%data_point)==self%n_data,"INTERPOLATOR:set_data_values_1 ... wrong number of data_points")
   call warn_if_(tonto,associated(self%data_value),"INTERPOLATOR:set_data_values_1 ... data_values will be lost")
      call destroy_(self%data_value)
      call create_(self%data_value,self%n_data)
      call func(self%data_point,self%data_value)

   end subroutine

   subroutine set_data_values_2(self,func_at,pos)
    type(interpolator_type) :: self
    ! Set the ".data_values" from the function "func_at", which returns "values"
    ! from a set of given "points", and an additional single "pos" as parameter.
      interface
         subroutine func_at(points,pos,values)
            real(kind=kind(1.0d0)), dimension(:) :: points,values
            real(kind=kind(1.0d0)), dimension(3) :: pos
         end subroutine
      end interface
      real(kind=kind(1.0d0)), dimension(3) :: pos

   call ensure_(tonto,self%finalised,"INTERPOLATOR:set_data_values_2 ... not finalised")
   call ensure_(tonto,associated(self%data_point),"INTERPOLATOR:set_data_values_2 ... no data_points")
   call ensure_(tonto,size(self%data_point)==self%n_data,"INTERPOLATOR:set_data_values_2 ... wrong number of data_points")
   call warn_if_(tonto,associated(self%data_value),"INTERPOLATOR:set_data_values_2 ... data_values will be lost")
      call destroy_(self%data_value)
      call create_(self%data_value,self%n_data)
      call func_at(self%data_point,pos,self%data_value)

   end subroutine

!  **************
!  Output methods
!  **************

   subroutine put(self)
    type(interpolator_type) :: self
    ! Put the list of interpolating colours.
      integer(kind=kind(1)) :: i

   call ensure_(tonto,self%finalised,"INTERPOLATOR:put ... not finalised")
      call flush_(stdout)
      call text_(stdout,"INTERPOLATOR info")
      call flush_(stdout)
      call show_(stdout,"No. of interpolating data points =",self%n_data)
      call show_(stdout,"Using even spaced data points?   =",is_even_spaced_(self))
      call flush_(stdout)
      if (is_even_spaced_(self)) then
         call show_(stdout,"Initial data value               =",self%data_point(1))
         call show_(stdout,"Last data value                  =",self%data_point(self%n_data))
         call show_(stdout,"Data point spacing               =",self%spacing)
         call flush_(stdout)
         call dash_(stdout,int_fields=1,real_fields=1)
         call put_(stdout,"#",int_width=.true.)
         call put_(stdout,"Value")
         call flush_(stdout)
         call dash_(stdout,int_fields=1,real_fields=1)
         do i = 1,self%n_data
            call put_(stdout,i)
            call put_(stdout,self%data_value(i))
            call flush_(stdout)
         end do
         call dash_(stdout,int_fields=1,real_fields=1)
      else
         call dash_(stdout,int_fields=1,real_fields=1)
         call put_(stdout,"#",int_width=.true.)
         call put_(stdout,"Point")
         call put_(stdout,"Value")
         call flush_(stdout)
         call dash_(stdout,int_fields=1,real_fields=2)
         do i = 1,self%n_data
            call put_(stdout,i)
            call put_(stdout,self%data_point(i))
            call put_(stdout,self%data_value(i))
            call flush_(stdout)
         end do
         call dash_(stdout,int_fields=1,real_fields=2)
      end if

   end subroutine

end
