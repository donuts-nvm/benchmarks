!------------------------------------------------------------------------------
!
! COLOURFUNCTION:
!
! For generating an RGB colour triple from a given data value.
!
! The module takes as input a series of data values with their assigned RGB
! colours. Any data value is then assigned an RGB colour by interpolation from
! the colour table.
!
! The default data range is [0...1]. The colours assigned are:
! Blue-Green for the range [0...0.5
! Green-Red  for the range [0.5...1].
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
! $Id: colourfunction.foo,v 1.2.2.4 2003/09/18 05:28:23 reaper Exp $
!
!------------------------------------------------------------------------------

module COLOURFUNCTION_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

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
   use TEXTFILE_MODULE, only: show_
   use TEXTFILE_MODULE, only: read_
   use TEXTFILE_MODULE, only: flush_
   use TEXTFILE_MODULE, only: dash_
   use TEXTFILE_MODULE, only: next_str_
   use TEXTFILE_MODULE, only: text_
   use TEXTFILE_MODULE, only: skip_next_item_
   use TEXTFILE_MODULE, only: previous_line_item_
   use TEXTFILE_MODULE, only: line_number_
   use TEXTFILE_MODULE, only: next_item_
   use TEXTFILE_MODULE, only: move_to_line_
   use TEXTFILE_MODULE, only: put_
   use TEXTFILE_MODULE, only: move_to_line_item_
   use TEXTFILE_MODULE, only: move_to_previous_item_
   use TEXTFILE_MODULE, only: reverted_

   use COLOUR_MODULE, only: read_
   use COLOUR_MODULE, only: RGB_

   use REAL_MODULE, only: is_in_range_

   use REALMAT_MODULE, only: create_
   use REALMAT_MODULE, only: create_copy_
   use REALMAT_MODULE, only: destroy_
   use REALMAT_MODULE, only: column_norms_
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

   public    RGB_for_
   interface RGB_for_
      module procedure RGB_for
   end interface

   public    set_data_
   interface set_data_
      module procedure set_data
   end interface

   public    set_reverse_defaults_
   interface set_reverse_defaults_
      module procedure set_reverse_defaults
   end interface

   public    create_copy_
   interface create_copy_
      module procedure create_copy
   end interface

   public    set_default_colours_
   interface set_default_colours_
      module procedure set_default_colours
   end interface

   public    destroy_ptr_part_
   interface destroy_ptr_part_
      module procedure destroy_ptr_part
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

   public    get_RGB255_for_
   interface get_RGB255_for_
      module procedure get_RGB255_for
   end interface

   public    read_data_and_colour_
   interface read_data_and_colour_
      module procedure read_data_and_colour
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

   public    get_RGB_for_
   interface get_RGB_for_
      module procedure get_RGB_for
   end interface

   public    set_RGB_
   interface set_RGB_
      module procedure set_RGB
   end interface

   public    RGB255_for_
   interface RGB255_for_
      module procedure RGB255_for
   end interface

   public    rescale_data_
   interface rescale_data_
      module procedure rescale_data
   end interface

   public    copy_
   interface copy_
      module procedure copy
   end interface

contains

   subroutine create(self,range)
    type(colourfunction_type) :: self
    ! Create the object
     pointer :: self
     real(kind=kind(1.0d0)), dimension(2), optional :: range

     nullify(self)
     allocate(self)

     call nullify_ptr_part_(self)
     call set_defaults_(self,range)

   end subroutine

   subroutine create_copy(self,object)
    type(colourfunction_type) :: self
    ! Create a copy of object
     pointer :: self
     type(colourfunction_type), intent(in) :: object

     call create_(self)
     call copy_(self,object)

   end subroutine

   subroutine copy(self,c)
    type(colourfunction_type) :: self
    ! Copy the contents of "c" to self.
     type(colourfunction_type), intent(in) :: c

     self%n_data = c%n_data
     call create_copy_(self%data,c%data)
     call create_copy_(self%RGB,c%RGB)

   end subroutine

   subroutine destroy(self)
    type(colourfunction_type) :: self
    ! Destroy an object
      pointer :: self
    ! The following code is inherited from OBJECT

      if (associated(self)) then
        call destroy_ptr_part_(self)
        deallocate(self)

      end if

   end subroutine

   subroutine nullify_ptr_part(self)
    type(colourfunction_type) :: self
    ! Nullify the pointer parts

      nullify(self%data)
      nullify(self%RGB)

   end subroutine

   subroutine destroy_ptr_part(self)
    type(colourfunction_type) :: self
    ! Destroy the pointer parts

      call destroy_(self%data)
      call destroy_(self%RGB)

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

   subroutine set_defaults(self,range)
    type(colourfunction_type) :: self
    ! Set up a defaults
      real(kind=kind(1.0d0)), dimension(2), optional :: range

      self%n_data = 3
      call destroy_(self%data)
      call create_(self%data,3)
      self%data     = (/0.0d0,0.50d0, 1.0d0/)
      call destroy_(self%RGB)
      call create_(self%RGB,3,3)
      self%RGB(:,1) = (/0.0d0,0.0d0, 1.0d0/)
      self%RGB(:,2) = (/0.0d0, 1.0d0,0.0d0/)
      self%RGB(:,3) = (/ 1.0d0,0.0d0,0.0d0/)
      if (present(range)) call rescale_data_(self,range)
      self%finalised = .true.

   end subroutine

   subroutine set_reverse_defaults(self,range)
    type(colourfunction_type) :: self
    ! Set up reversed defaults
      real(kind=kind(1.0d0)), dimension(2), optional :: range

      self%n_data = 3
      call destroy_(self%data)
      call create_(self%data,3)
      self%data     = (/0.0d0,0.50d0, 1.0d0/)
      call destroy_(self%RGB)
      call create_(self%RGB,3,3)
      self%RGB(:,1) = (/ 1.0d0,0.0d0,0.0d0/)
      self%RGB(:,2) = (/0.0d0, 1.0d0,0.0d0/)
      self%RGB(:,3) = (/0.0d0,0.0d0, 1.0d0/)
      if (present(range)) call rescale_data_(self,range)
      self%finalised = .true.

   end subroutine

!  *************
!  Input methods
!  *************

   recursive subroutine read_keywords(self)
    type(colourfunction_type) :: self
    ! Read data from "stdin" using keyword style input.
    ! The following code is inherited from OBJECT
     character(128) :: word

     call ensure_(tonto,next_item_(stdin)=="{","COLOURFUNCTION:read_keywords ... expecting open bracket symbol, {")
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
    type(colourfunction_type) :: self
    ! Process command "keyword". Any required data needed by the "keyword" is
    ! inputted from "stdin".
      character(*) :: keyword
      character(128) :: word

      word = keyword
      call to_lower_case_(word)
      select case (word)
         case ("}               ")   ! exit case
         case ("data_and_colour="); call read_data_and_colour_(self)
         case ("put             "); call put_(self)
         case ("units=          "); call read_units_(self)
         case default;         allocate(tonto%known_keywords(4))
         tonto%known_keywords(1) = "}               "
         tonto%known_keywords(2) = "data_and_colour="
         tonto%known_keywords(3) = "put             "
         tonto%known_keywords(4) = "units=          "
         call unknown_(tonto,word,"COLOURFUNCTION:process_keyword")
         deallocate(tonto%known_keywords)
      end select

   end subroutine

   subroutine read_units(self)
    type(colourfunction_type) :: self
    ! Read a string which describes the units to be used
    ! The following code is inherited from OBJECT

      call set_default_units_(stdin,next_str_(stdin))

   end subroutine

   subroutine read_junk(self)
    type(colourfunction_type) :: self
    ! Read in a junk string, useful for ignoring a field
    ! The following code is inherited from OBJECT

      call skip_next_item_(stdin)

   end subroutine

   subroutine read_data_and_colour(self)
    type(colourfunction_type) :: self
    ! Read in the data-colour pair. The colour can be a text string of a known
    ! colour, an RGB 255 integer triple, or an RGB real triple. Only the latter
    ! is used and stored.
      character(128) :: word
      type(colour_type) :: colour
      integer(kind=kind(1)) :: length,line,item,i
      real(kind=kind(1.0d0)) :: data

      call ensure_(tonto,next_item_(stdin)=="{","COLOURFUNCTION:read_data_and_colour ... open bracket '{' expected")
      call read_(stdin,word)
      call read_(stdin,word)
      length = 0
      line = line_number_(stdin)
      item = previous_line_item_(stdin)
      do
         call move_to_previous_item_(stdin)
         call read_(stdin,data)
         call read_(colour)
         length = length + 1
         call read_(stdin,word)
         call to_lower_case_(word)
         if (word=="}") exit
      end do
      call move_to_line_(stdin,line)
      call move_to_line_item_(stdin,item)
      call destroy_(self%data)
      call create_(self%data,length)
      call destroy_(self%RGB)
      call create_(self%RGB,3,length)
      do i = 1,length
         call read_(stdin,data)
         call read_(colour)
         self%data(i)  = data
         self%RGB(:,i) = RGB_(colour)
      end do
      call read_(stdin,word)
      call to_lower_case_(word)
      call ensure_(tonto,word=="}","COLOURFUNCTION:read_data_and_colour ... expecting a }")

   end subroutine

   subroutine finalise(self)
    type(colourfunction_type) :: self
    ! Check everything is OK after reading data. Saturation must exist but it is
    ! applied into the RGB triples and then destroyed.
      integer(kind=kind(1)), dimension(:), pointer :: order

   call ensure_(tonto,associated(self%data),"COLOURFUNCTION:finalise ... no data values")
   call ensure_(tonto,associated(self%RGB),"COLOURFUNCTION:finalise ... no RGB values")
   call ensure_(tonto,size(self%data)==size(self%RGB,2),"COLOURFUNCTION:finalise ... incompatible #: data and RGB")
   call ensure_(tonto,all(self%RGB>=0.0d0),"COLOURFUNCTION:finalise ... negative RGB values exist")
   call ensure_(tonto,all(self%RGB<=1.0d0),"COLOURFUNCTION:finalise ... some RGB values greater than 1")
   call ensure_(tonto,all(column_norms_(self%RGB)<=3.0d0),"COLOURFUNCTION:finalise ... some RGB norms larger than 3")
      self%n_data = size(self%data)
      call create_(order,self%n_data)
       ! Re-order from smallest to largest
      call quick_sort_(self%data,order)
      self%data       = self%data(order)
      self%RGB        = self%RGB(:,order)
      call destroy_(order)
      self%finalised = .true.

   end subroutine

!  ************
!  Set routines
!  ************

   subroutine set_data(self,data)
    type(colourfunction_type) :: self
    ! Set the data. Make sure that .finalise is called after all set routines.
      real(kind=kind(1.0d0)), dimension(:) :: data

      call destroy_(self%data)
      call create_copy_(self%data,data)

   end subroutine

   subroutine set_RGB(self,RGB)
    type(colourfunction_type) :: self
    ! Set the RGB's. Make sure that .finalise is called after all set routines.
      real(kind=kind(1.0d0)), dimension(:,:) :: RGB

      call destroy_(self%RGB)
      call create_copy_(self%RGB,RGB)

   end subroutine

!  ****************
!  Colours for data
!  ****************

   function RGB_for(self,value,truncate_to_range) result(res)
    type(colourfunction_type) :: self
    ! Return the RGB triple corresponding to a certain "value".
      intent(in) :: self
      real(kind=kind(1.0d0)), intent(in) :: value
      logical(kind=kind(.true.)), optional, intent(in) :: truncate_to_range
      real(kind=kind(1.0d0)), dimension(3) :: res
      real(kind=kind(1.0d0)), dimension(3) :: colour
      real(kind=kind(1.0d0)) :: frac,thevalue
      integer(kind=kind(1)) :: i1,i2
      logical(kind=kind(.true.)) :: truncate

      call ensure_(tonto,self%finalised,"COLOURFUNCTION:RGB_for ... not finalised")
      thevalue = value
      truncate = .true.
      if (present(truncate_to_range)) truncate = truncate_to_range
      if (.not. truncate) then
        call ensure_(tonto,is_in_range_(thevalue,range_(self%data)),"COLOURFUNCTION:RGB_for ... value out of range")
      else
        thevalue = min(thevalue,maxval(self%data))
        thevalue = max(thevalue,minval(self%data))
      end if
      i1 = count(self%data<=thevalue)  ! assuming .data is ordered !
      i1 = min(i1,self%n_data-1)
      i2 = i1 + 1
      frac   = (thevalue - self%data(i1))/(self%data(i2)-self%data(i1))
      colour = self%RGB(:,i2) - self%RGB(:,i1)
      res    = self%RGB(:,i1) + frac*colour

   end function

   function RGB255_for(self,value) result(res)
    type(colourfunction_type) :: self
    ! Return the RGB255 triple corresponding to a certain "value".
      real(kind=kind(1.0d0)) :: value
      integer(kind=kind(1)), dimension(3) :: res
      real(kind=kind(1.0d0)), dimension(3) :: colour
      real(kind=kind(1.0d0)) :: frac
      integer(kind=kind(1)) :: i1,i2

   call ensure_(tonto,self%finalised,"COLOURFUNCTION:RGB255_for ... not finalised")
   call ensure_(tonto,is_in_range_(value,range_(self%data)),"COLOURFUNCTION:RGB255_for ... value out of range")
      i1 = count(self%data<=value)  ! assuming .data is ordered !
      i1 = min(i1,self%n_data-1)
      i2 = i1 + 1
      frac   = (value - self%data(i1))/(self%data(i2)-self%data(i1))
      colour = self%RGB(:,i2) - self%RGB(:,i1)
      res    = nint(255*(self%RGB(:,i1) + frac*colour))

   end function

   subroutine get_RGB_for(self,values,RGB)
    type(colourfunction_type) :: self
    ! Return the "RGB" triples corresponding to a set of "values".
      real(kind=kind(1.0d0)), dimension(:) :: values
      real(kind=kind(1.0d0)), dimension(:,:) :: RGB
      integer(kind=kind(1)) :: i,n

   call ensure_(tonto,self%finalised,"COLOURFUNCTION:get_RGB_for ... not finalised")
   call ensure_(tonto,size(RGB,2)==size(values),"COLOURFUNCTION:get_RGB_for ... values and RGB are incompatible")
      n = size(values)
      do i = 1,n
         RGB(:,i) = RGB_for_(self,values(i))
      end do

   end subroutine

   subroutine get_RGB255_for(self,values,RGB255)
    type(colourfunction_type) :: self
    ! Return the "RGB255" triples corresponding to a set of "values".
      real(kind=kind(1.0d0)), dimension(:) :: values
      integer(kind=kind(1)), dimension(:,:) :: RGB255
      integer(kind=kind(1)) :: i,n

   call ensure_(tonto,self%finalised,"COLOURFUNCTION:get_RGB255_for ... not finalised")
   call ensure_(tonto,size(RGB255,2)==size(values),"COLOURFUNCTION:get_RGB255_for ... values and RGB255 are incompatible")
      n = size(values)
      do i = 1,n
         RGB255(:,i) = RGB255_for_(self,values(i))
      end do

   end subroutine

   subroutine rescale_data(self,range)
    type(colourfunction_type) :: self
    ! Rescale the .data so that the lowest value corresponds to
    ! range(1), and the largest value corresponds to range(2).
      real(kind=kind(1.0d0)), dimension(2) :: range
      real(kind=kind(1.0d0)) :: data1,del,frac
      integer(kind=kind(1)) :: i

   call ensure_(tonto,associated(self%data),"COLOURFUNCTION:rescale_data ... no data")
   call ensure_(tonto,self%n_data>1,"COLOURFUNCTION:rescale_data ... not enough data")
      data1 = self%data(1)
      del   = self%data(self%n_data) - data1
      do i = 1,self%n_data
         frac = (self%data(i) - data1)/del
         self%data(i) = range(1) + frac*(range(2)-range(1))
      end do

   end subroutine

   subroutine set_default_colours(self,values)
    type(colourfunction_type) :: self
    ! Set up default colours for a set of "values".
      real(kind=kind(1.0d0)), dimension(:) :: values

      call rescale_data_(self,range_(values))

   end subroutine

!  **************
!  Output methods
!  **************

   subroutine put(self)
    type(colourfunction_type) :: self
    ! Put the list of interpolating colours.
      integer(kind=kind(1)) :: i

      call flush_(stdout)
      call text_(stdout,"Colourfunction data")
      call flush_(stdout)
      call show_(stdout,"No. of interpolating data points =",self%n_data)
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=4)
      call put_(stdout,"#",int_width=.true.)
      call put_(stdout,"Data")
      call put_(stdout,"Red")
      call put_(stdout,"Green")
      call put_(stdout,"Blue")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=4)
      do i = 1,self%n_data
         call put_(stdout,i)
         call put_(stdout,self%data(i))
         call put_(stdout,self%RGB(1,i))
         call put_(stdout,self%RGB(2,i))
         call put_(stdout,self%RGB(3,i))
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=1,real_fields=4)

   end subroutine

end
